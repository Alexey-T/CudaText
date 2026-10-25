'''
Plugin for CudaText: "cuda_testing_lexers".
Testing of lexers before/after patches of the lexer engine (EControl),
see github.com/Alexey-T/CudaText issue #6565.

It parses a corpus of test files (subfolder "lexers" of this plugin),
and saves lexer data (tokens, folding) as JSON files into snapshot folders.
Then it compares snapshots, to find changes in lexer parsing.

Folder layout (all inside this plugin's folder):

  lexers/                          test files to parse (flat list, no subfolders)
  results_base/                    first snapshot (baseline)
  results_<name>/                  other snapshots
  results_base/markdown.md_tokens.json   tokens JSON for markdown.md
  results_base/markdown.md_fold.json     folding JSON for markdown.md

JSON contents (ALL fields which CudaText API returns, with their values
and their order unchanged; nothing is sorted or normalized):

  tokens: {"lexer", "line_count", "token_count",
           "tokens": [ {"str","style","x1","y1","x2","y2","ki","ks"}, ... ]}
  folds:  {"lexer", "fold_count",
           "folds": [ {"x","y","x2","y2","staple","folded","hint","tag"}, ... ]}

How the full parse is forced and VERIFIED (important):

  For each opened file:
  1. ed.action(EDACTION_LEXER_SCAN, "0") makes EControl parse the whole
     file, from line 0 to the end, synchronously. Right after it the
     plugin pumps the message loop, so the engine surely publishes the
     finished data before anything is read.
  2. Data is verified CHEAPLY, so it also works for big files (v4 read
     the full token list twice per attempt and demanded two identical
     full parses: a 9 MB file made ~2 millions of token dicts per read,
     froze CudaText and looked like an infinite wait):
     - a small window of lines near the file end must contain tokens
       which reach the last non-blank line, and must not change on
       a re-read;
     - only then the FULL token list is read, and its last token must
       reach the last non-blank line;
     - the full token list is read in line chunks, with UI message
       pumps between chunks (one giant API call freezes the UI for a
       long time), and JSONs are written right after each file, so
       big token lists never pile up in memory.
  If verification still fails (aborted parse, missing analyzer), the
  plugin pumps the message loop and forces a new full parse, but only
  while the data really does not reach the file end. From the 3rd
  attempt the lexer is also re-asserted (ed.set_prop(PROP_LEXER_FILE,
  ...) re-creates the internal EControl analyzer if it was missing).
  A file which cannot be parsed to the end after all attempts is
  reported as an error - wrong data is never silently written.

  Why verification is needed (found from CudaText/EControl sources and
  from 3 broken snapshots of same files): the engine publishes parse
  results asynchronously, and ed.action() can return before the data
  is really published:
  - TATAdapterEControl.ParseFromLine() silently does NOTHING when the
    EControl analyzer object ("AnClient") does not exist yet (lexer
    was only "stashed" for the tab, not applied);
  - its wait-loop exits when analyzer flag IsFinished is set - but that
    flag is also set by analyzer Stop() on any text change, i.e. an
    ABORTED parse looks "finished", with empty/stale token data;
  - the wait-loop pumps UI messages (Application.ProcessMessages), so
    the background parser thread publishes its results to the token
    data only on some LATER pump - right when get_token() may read.
  folding() reads editor's own fold list, which is filled at parse-done,
  so broken snapshots had FULL folds but EMPTY tokens. Without
  verification, such a run silently wrote "token_count: 0".

Note about BIG test files: CudaText itself disables the lexer for
files over option "ui_max_size_lexer" (2 MB by default) - the editor
shows a "lexer disabled by size" panel for them. The plugin pins that
option (to 500 MB) for its session before parsing, see the next note,
so big corpus files parse without any manual setup. Only on a very old
CudaText without the PROC_CONFIG_READ API the pinning is skipped; then
such files are reported as skipped and the option must be increased in
Options Editor before running "make_snapshot" again.

Pinned session settings (constant results):

  Before a snapshot parses the corpus, the plugin applies this JSON
  via app_proc(PROC_CONFIG_READ, ...):

    {"auto_fold_comments": 2, "lexer_folding_max_lines": 1500000,
     "ui_max_size_lexer": 500, "fold_style": 4,
     "max_line_len_parsed": 40000000,
     "wrap_enabled_max_lines": 2100000,
     "max_line_len_for_editing_keeping_lexer": 0}

  All these options change what the lexer engine does with the same
  file (lexer enabled/disabled by file size, folding computed or not,
  fold "folded" flags, word wrap on/off for huge files, dropped lexer
  on long edited lines...). Their defaults differ between machines and
  sessions, so without pinning, two snapshots of the same files could
  differ because of OPTIONS, not because of the tested lexer patch.
  The pinning is TEMPORARY: CudaText loads these values into memory
  only (DoOps_LoadOptionsFromString), user.json is never touched, and
  after a CudaText restart the user's own option values are back.

Commands (menu items are registered by the plugin .inf file,
or use Command Palette):

- make_snapshot
  Parses all files from "lexers". For every file with a detected lexer,
  saves two JSONs into "results_base" (created if missing), otherwise
  asks for a name and saves into "results_<name>".
  Files without lexer (and files of lite-lexers, they have no EControl
  tokens) are skipped and reported at the end.
  Files which could not be parsed to the end are reported as errors,
  nothing wrong is silently written.
  Enter name "base" to overwrite results_base.

- compare_results
  Offers a menu of found "results_*" folders (results_base excluded),
  compares the chosen folder against results_base, JSON-by-JSON.

- compare_two_folders
  Asks for ANY two folders (with 2 directory-picker dialogs) and
  compares their JSONs pair-by-name. Reports identical/DIFFERENT
  per file, and files present only in one folder.

  Both compare commands show a summary dialog + report in a new
  editor tab. Only "identical"/"DIFFERENT" is reported per file,
  contents of differences is NOT shown.

- open_dir
  Opens the plugin folder, "lexers" or a "results_*" folder
  in the OS file manager.

Typical usage for testing a lexer-engine patch:

  1) Run CudaText built without the lexer patch,
     call "make_snapshot" -> results_base is created.
  2) Rebuild CudaText with the lexer patch, restart it.
  3) Call "make_snapshot" with name e.g. "patch1",
     then "compare_results" to check that nothing changed.
'''

import os
import sys
import json
import time
import subprocess

import cudatext
import cudatext_cmd as cmds
from cudatext import *

TAG = 'cuda_testing_lexers'
SUF_TOKENS = '_tokens.json'
SUF_FOLDS = '_fold.json'

# verified options of file_open():
# /nonear - do not add file to "recent files"
# /nohistory - do not load/save file history: caret pos, bookmarks,
#              folded ranges (we need the SAME fold "folded" flags
#              on every run, not state from previous sessions)
# /noevent, /noopenedevent, /nononeevent - do not call events for other plugins
# ("/passive" must NOT be used: passive tabs are never painted,
#  and EControl parses the file only when the tab is painted)
# ("/nolexerdetect" must NOT be used: lexer detection is required)
OPEN_OPTIONS = '/nonear /nohistory /noevent /noopenedevent /nononeevent'

# ed.action(EDACTION_LEXER_SCAN, "0") makes EControl parse the whole
# file, from line 0 to the end (CudaText source: Ada.ParseFromLine(N, true),
# N = StrToInt64Def(param, 0), so "0" and "" both mean line 0).
SCAN_FROM_LINE = '0'

# the full token list is read in chunks of CHUNK_LINES lines, with a
# message-loop pump between chunks: one giant get_token() call for a
# big file builds millions of Python dicts in a single C call and
# freezes CudaText for a long time (seen on the user's 9 MB corpus file)
CHUNK_LINES = 2000

# verification window: TAIL_CONTEXT_LINES lines before the last
# non-blank line are checked cheaply (a few tokens) before the full
# token list is read at all
TAIL_CONTEXT_LINES = 2

# fallback scan depth for _last_content_line() when get_text_all()
# cannot be split to the same number of lines (unusual line ends)
MAX_BACK_LINES = 20000

# how many attempts to get complete data for one file.
# Not a time-based timeout: each attempt does a forced full parse only
# while the data really does not reach the file end; the cheap
# EOF-window check decides when a new parse is needed at all.
MAX_PARSE_TRIES = 12

# from this attempt on, the lexer is re-asserted with
# set_prop(PROP_LEXER_FILE): that re-creates the internal EControl
# analyzer when it is missing at all (lexer was only "stashed").
# Not done on attempts 1-2: re-applying a lexer also drops the already
# parsed data and forces a new full parse, so it must be a last resort.
REASSERT_FROM = 3

# streamed JSON writing: flush the output buffer every N list items
WRITE_FLUSH_ITEMS = 2000

# lite-lexers have this suffix in lexer name, they have no EControl
# tokens, so they are skipped like "no lexer" files
LITE_SUFFIX = ' ^'

# plain-text lexer names (case-insensitive), file is skipped
PLAIN_LEXERS = ('-', 'plain text', 'plaintext')

# Option values pinned for the CudaText session before every snapshot,
# via app_proc(PROC_CONFIG_READ, json_with_these_values): CudaText
# loads the JSON into its option structures IN MEMORY and re-applies
# options to all editors (verified in CudaText source:
# DoOps_LoadOptionsFromString -> DoOps_LoadOptions -> DoApplyAllOps;
# keys absent from the JSON keep their current values, so ONLY these
# options are touched). Nothing is written to user.json - the pinning
# is temporary, a CudaText restart brings the user's own values back.
# Why pinning: all these options change what the engine produces for
# the same file, and their defaults differ between machines/sessions;
# snapshots must run under identical options ("constant results"),
# or compares report phantom differences:
#   ui_max_size_lexer - lexer is disabled for files >= this many MB
#     (default 2; the 9 MB corpus file is "no lexer" without pinning)
#   lexer_folding_max_lines - parser features (folding) are not
#     enabled for files with more lines -> fold JSONs would be empty
#   auto_fold_comments, fold_style - EControl folding behavior/style
#   max_line_len_parsed - lines longer than this are not parsed
#   wrap_enabled_max_lines - word wrap is disabled above this many
#     lines (wrapping changes what gets painted, and EControl parses
#     painted tabs)
#   max_line_len_for_editing_keeping_lexer - lexer is dropped while a
#     longer line is edited; 0 = keep the lexer always
SESSION_SETTINGS = {
    'auto_fold_comments': 2,
    'lexer_folding_max_lines': 1500000,
    'ui_max_size_lexer': 500,
    'fold_style': 4,
    'max_line_len_parsed': 40000000,
    'wrap_enabled_max_lines': 2100000,
    'max_line_len_for_editing_keeping_lexer': 0,
}

MAX_DIALOG_LIST = 20
BAD_NAME_CHARS = '\\/:*?"<>|'

STATUS_LABELS = {
    'ok': 'identical',
    'diff': 'DIFFERENT',
    'only_base': 'only-in-first',
    'only_other': 'only-in-second',
    'error': 'error',
    }


def _app_ver():
    try:
        v = app_exe_version()
        return v if v else '?'
    except Exception:
        return '?'


def _os_open_dir(path):
    try:
        if sys.platform.startswith('win'):
            os.startfile(path)
        elif sys.platform == 'darwin':
            subprocess.Popen(['open', path])
        else:
            subprocess.Popen(['xdg-open', path])
        msg_status('%s: opened in file manager: %s' % (TAG, path))
    except Exception as ex:
        msg_box('%s: cannot open folder:\n%s\n\n%s' % (TAG, path, ex),
                MB_OK + MB_ICONERROR)


def _log(text):
    try:
        app_log(LOG_CONSOLE_ADD, text)
    except Exception:
        pass


def _tokens_dict(lexer, line_count, tokens):
    return {
        'lexer': lexer,
        'line_count': line_count,
        'token_count': len(tokens),
        'tokens': tokens,
    }


def _folds_dict(lexer, folds):
    return {
        'lexer': lexer,
        'fold_count': len(folds),
        'folds': folds,
    }


def _pretty_text(data, list_key):
    '''
    Makes readable JSON text: scalar fields first, then the big list
    with one item per line, so the JSON is readable in any diff tool.
    Data itself (values, item order) is written exactly as API returned.
    '''
    lines = ['{']
    for k in data:
        if k == list_key:
            continue
        lines.append('  %s: %s,' % (json.dumps(k), json.dumps(data[k], ensure_ascii=False)))
    lines.append('  %s: [' % json.dumps(list_key))
    items = data[list_key]
    n = len(items)
    for i, it in enumerate(items):
        comma = ',' if i < n - 1 else ''
        lines.append('    %s%s' % (json.dumps(it, ensure_ascii=False), comma))
    lines.append('  ]')
    lines.append('}')
    return '\n'.join(lines) + '\n'


def _write_pretty_json(path, data, list_key):
    '''
    Writes the same text format as _pretty_text(), but streams the big
    list item-by-item: a multi-MB test file makes a JSON of hundreds
    of MB, which must not be built as one giant string in memory.
    Message loop is pumped while writing, to keep CudaText alive.
    '''
    with open(path, 'w', encoding='utf-8', newline='\n') as f:
        f.write('{\n')
        for k in data:
            if k == list_key:
                continue
            f.write('  %s: %s,\n' % (json.dumps(k),
                                    json.dumps(data[k], ensure_ascii=False)))
        f.write('  %s: [\n' % json.dumps(list_key))
        items = data[list_key]
        n = len(items)
        buf = []
        for i, it in enumerate(items):
            comma = ',' if i < n - 1 else ''
            buf.append('    %s%s\n' %
                       (json.dumps(it, ensure_ascii=False), comma))
            if len(buf) >= WRITE_FLUSH_ITEMS:
                f.write(''.join(buf))
                buf = []
                msg_status('%s: writing %s: %d/%d' %
                           (TAG, os.path.basename(path), i + 1, n), True)
        if buf:
            f.write(''.join(buf))
        f.write('  ]\n}\n')


def _item_stat(da, db):
    '''short numeric statistic for a JSON pair: "tokens 123 vs 125"'''
    parts = []
    for key, label in (('token_count', 'tokens'), ('fold_count', 'folds')):
        if key in da or key in db:
            va = da.get(key, '?')
            vb = db.get(key, '?')
            parts.append('%s %s vs %s' % (label, va, vb))
    return '; '.join(parts)


def _is_plain_lexer(name):
    s = str(name).strip().lower()
    return (not s) or (s in PLAIN_LEXERS)


def _is_lite_lexer(name):
    return str(name).strip().endswith(LITE_SUFFIX)


class Command:

    def __init__(self):
        self.dir_plugin = os.path.dirname(__file__)
        self.dir_lex = os.path.join(self.dir_plugin, 'lexers')
        self.dir_base = os.path.join(self.dir_plugin, 'results_base')
        # True while this CudaText session runs with pinned
        # SESSION_SETTINGS (set by _apply_session_settings)
        self.settings_applied = False


    #--------- engine ---------

    def _apply_session_settings(self):
        '''
        Pins SESSION_SETTINGS for this CudaText session, before any
        corpus file is opened: every snapshot then parses under the
        SAME options on any machine and in any session (see the file
        docstring, "Pinned session settings").
        Temporary by design: values are loaded into memory only
        (app_proc(PROC_CONFIG_READ, json) -> DoOps_LoadOptions-
        FromString), user.json is not touched, a CudaText restart
        brings the user's own values back.
        Returns True when the settings were applied.
        '''
        self.settings_applied = False
        if not hasattr(cudatext, 'PROC_CONFIG_READ'):
            # very old CudaText: snapshot still works, but options are
            # not pinned (big files may be skipped by size, folds may
            # be missing for them) - this is reported in _info.txt
            _log('%s: no PROC_CONFIG_READ API, session settings not '
                 'pinned' % TAG)
            return False
        try:
            app_proc(PROC_CONFIG_READ, json.dumps(SESSION_SETTINGS))
        except Exception as ex:
            _log('%s: cannot pin session settings: %s' % (TAG, ex))
            msg_status('%s: cannot pin session settings: %s' % (TAG, ex))
            return False
        self.settings_applied = True
        _log('%s: session settings pinned (temporary): %s'
             % (TAG, json.dumps(SESSION_SETTINGS)))
        msg_status('%s: session settings pinned: %d options'
                   % (TAG, len(SESSION_SETTINGS)))
        return True

    def _corpus_files(self):
        '''flat list of files in "lexers", dot-files and subfolders skipped'''
        try:
            names = os.listdir(self.dir_lex)
        except OSError:
            return []
        res = []
        for n in sorted(names):
            if n.startswith('.'):
                continue
            if os.path.isfile(os.path.join(self.dir_lex, n)):
                res.append(n)
        return res


    def _find_editor(self, filename):
        '''finds already opened tab with this filename'''
        target = os.path.normcase(os.path.abspath(filename))
        for h in ed_handles():
            e = Editor(h)
            fn = e.get_filename()
            if fn and os.path.normcase(os.path.abspath(fn)) == target:
                return e
        return None


    def _open_doc(self, filename):
        '''
        Opens file in an ACTIVE tab, returns (Editor, opened_here).
        Tab must be active: EControl parses only painted (visible) tabs.
        If file is already opened, reuses that tab (opened_here=False).
        If opening failed, returns (None, False).
        '''
        e = self._find_editor(filename)
        if e is not None:
            try:
                e.focus()
            except Exception:
                pass
            return e, False
        try:
            ok = file_open(filename, options=OPEN_OPTIONS)
        except Exception as ex:
            _log('%s: file_open failed for %s: %s' % (TAG, filename, ex))
            ok = False
        if not ok:
            return None, False
        e = self._find_editor(filename)
        if e is None:
            return None, False
        return e, True


    def _close_doc(self, e):
        try:
            e.cmd(cmds.cmd_FileClose)
        except Exception as ex:
            _log('%s: cannot close tab: %s' % (TAG, ex))


    def _read_tokens(self, e):
        '''
        Reads tokens of the whole file in LINE CHUNKS.
        One giant get_token() call for a multi-MB file builds millions
        of Python dicts inside one C call and freezes the UI for a long
        time; chunked reading keeps the message loop alive between
        chunks and does not change the resulting list.
        A token which starts before the chunk and ends inside it is
        returned by BOTH chunks (the API finds tokens touching the
        range), so items with y1 < chunk start are skipped - such token
        was already added with the chunk of its own first line.
        '''
        line_count = e.get_line_count()
        if line_count <= 0:
            return []
        res = []
        for y in range(0, line_count, CHUNK_LINES):
            y2 = min(y + CHUNK_LINES - 1, line_count - 1)
            part = e.get_token(TOKEN_LIST_SUB, y, y2)
            if part:
                for t in part:
                    try:
                        if int(t.get('y1', 0)) < y:
                            continue
                    except Exception:
                        pass
                    res.append(t)
            if y2 >= line_count - 1:
                break
            # keep UI alive between chunks; also lets the engine
            # publish finished parse data, if any is still pending
            msg_status('%s: reading tokens: lines %d-%d of %d' %
                       (TAG, y + 1, y2, line_count), True)
        return res


    def _read_tokens_range(self, e, y_from, y_to):
        '''tokens which the API returns for a line range'''
        if y_to < 0 or y_to < y_from:
            return []
        res = e.get_token(TOKEN_LIST_SUB, y_from, y_to)
        return list(res) if res else []


    def _read_folds(self, e):
        res = e.folding(FOLDING_ENUM)
        return list(res) if res else []


    def _line_has_content(self, s):
        for ch in s:
            if not ch.isspace() and ch != '\ufeff':
                return True
        return False


    def _last_content_line(self, e, line_count):
        '''
        Index of the last line which has any non-blank char, -1 if none.
        Uses ONE get_text_all() call (fast for any file size, unlike a
        per-line backward scan through thousands of trailing blank
        lines of a big log). Falls back to a bounded per-line scan when
        the text cannot be split to the same number of lines (unusual
        line ends in the editor settings).
        '''
        if line_count <= 0:
            return -1
        try:
            lines = e.get_text_all().split('\n')
            if lines and lines[-1] == '':
                lines.pop()
            if len(lines) == line_count:
                for i in range(len(lines) - 1, -1, -1):
                    if self._line_has_content(lines[i]):
                        return i
                return -1
        except Exception:
            pass
        # fallback: bounded backward scan with per-line API calls
        stop = line_count - MAX_BACK_LINES
        if stop < 0:
            stop = 0
        for i in range(line_count - 1, stop - 1, -1):
            try:
                s = e.get_text_line(i)
            except Exception:
                s = ''
            if s and self._line_has_content(s):
                return i
        return -1


    def _tokens_cover_file(self, tokens, last):
        '''
        True if the token list is the result of a parse to the file end:
        the last token must reach the last non-blank line ("last",
        precomputed by _last_content_line).
        For a file without any non-blank line, any data is accepted:
        some lexers tokenize whitespace too, so "empty" cannot be
        required here.
        '''
        if last < 0:
            return True
        if not tokens:
            return False
        try:
            return int(tokens[-1].get('y2', -1)) >= last
        except Exception:
            return False


    def _window_ok(self, e, win_from, win_to, last):
        '''
        CHEAP check (reads a few tokens near the file end only, works
        the same fast for any file size): do tokens exist in the window
        of the last content lines, reach the last non-blank line, and
        stay the same on a re-read? Used to decide whether a new forced
        full parse is needed, before the (for big files, slow) full
        token read. A false alarm here only costs one extra parse.
        For a file without content nothing can be checked: True.
        '''
        if last < 0:
            return True
        t1 = self._read_tokens_range(e, win_from, win_to)
        if not t1:
            return False
        for t in t1:
            try:
                if int(t.get('y2', -1)) >= last:
                    break
            except Exception:
                pass
        else:
            return False
        return self._read_tokens_range(e, win_from, win_to) == t1


    def _parse_file_verified(self, e, fn, lexer):
        '''
        Forces the full parse and reads VERIFIED tokens+folds.
        Returns dict {name, lexer, line_count, tokens, folds}.

        Deals with the engine races (see file docstring), but CHEAPLY,
        so it scales to big files (v4 read the full token list twice
        per attempt and demanded two identical full parses - on a 9 MB
        file that froze CudaText and looped forever):
        - one forced parse, then a message pump right away: the engine
          surely publishes the finished data before it is read;
        - the full token list is read only after the cheap EOF-window
          check passes; new full parses happen only while the window
          still shows no stable complete data (aborted parse, missing
          analyzer); with the stash state the window stays empty until
          set_prop creates the analyzer (from REASSERT_FROM-th
          attempt), like in the older versions;
        - on every attempt the (cheap, when data is not ready) full
          read plus the exact "last token reaches file end" check
          gives the final verdict - wrong data is never accepted;
        - for a file WITHOUT content lines tokens cannot prove
          anything (empty list is also the correct result), so there
          the data is accepted only after it repeats after a NEW full
          parse, with the lexer re-asserted from attempt 2 (blank
          files are tiny, these extra parses are free).
        Raises if complete data cannot be obtained after MAX_PARSE_TRIES.
        '''
        try:
            e.focus()
        except Exception:
            pass

        line_count = e.get_line_count()
        last = self._last_content_line(e, line_count)
        if last >= 0:
            win_from = max(0, last - TAIL_CONTEXT_LINES)
            win_to = last
        else:
            win_from = 0
            win_to = 0

        prev_blank = None
        for attempt in range(1, MAX_PARSE_TRIES + 1):
            # full parse is needed on attempt 1, and on later attempts:
            # - when the cheap window check still sees no stable
            #   complete data (i.e. the failure was not just the engine
            #   publishing its results one message-pump later);
            # - ALWAYS for a file without content: its tokens carry no
            #   signal, and the scan of a "stashed" lexer silently
            #   no-ops, so data must be confirmed after the lexer is
            #   re-asserted and a new parse really ran.
            # ("or" short-circuits, so the window check is not even
            # called when a full parse is already decided)
            if (attempt == 1 or last < 0 or
                    not self._window_ok(e, win_from, win_to, last)):
                if (attempt >= REASSERT_FROM or
                        (last < 0 and attempt >= 2)):
                    # re-assert the lexer. If the EControl analyzer
                    # object is missing (lexer was only "stashed"),
                    # this creates it and starts parsing; if it
                    # exists, CudaText sees the same lexer and skips.
                    # For files WITH content this is not done early:
                    # a really re-applied lexer also drops the parsed
                    # data and forces a new full parse. For a file
                    # WITHOUT content there is nothing to drop.
                    try:
                        e.set_prop(PROP_LEXER_FILE, lexer)
                    except Exception as ex:
                        _log('%s: cannot re-assert lexer "%s": %s' %
                             (TAG, lexer, ex))
                msg_status('%s: %s: parsing, attempt %d/%d' %
                           (TAG, fn, attempt, MAX_PARSE_TRIES), True)
                e.action(EDACTION_LEXER_SCAN, SCAN_FROM_LINE)
                # pump: the background parser thread publishes results
                # exactly on such message-loop pumps
                msg_status('%s: %s: parse done, collecting data' %
                           (TAG, fn), True)

            # full read is cheap while data is not ready (chunks return
            # nothing), and it is the needed work when data is ready
            tokens = self._read_tokens(e)
            if self._tokens_cover_file(tokens, last):
                folds = self._read_folds(e)
                if last >= 0:
                    # tokens reach the file end: the parse surely ran
                    # to the end, data is complete
                    return {
                        'name': fn,
                        'lexer': lexer,
                        'line_count': line_count,
                        'tokens': tokens,
                        'folds': folds,
                        }
                # file without content: accept only data which repeats
                # after a new full parse (catches the stashed lexer,
                # whose first scans silently no-op with empty data)
                data = (tokens, folds)
                if prev_blank is not None and data == prev_blank:
                    return {
                        'name': fn,
                        'lexer': lexer,
                        'line_count': line_count,
                        'tokens': tokens,
                        'folds': folds,
                        }
                prev_blank = data

            # incomplete: pump, then the next attempt decides whether
            # a new forced parse is really needed
            msg_status('%s: %s: lexer data does not reach file end, '
                       'attempt %d/%d' %
                       (TAG, fn, attempt, MAX_PARSE_TRIES), True)

        raise RuntimeError(
            'cannot get complete lexer data after %d tries' %
            MAX_PARSE_TRIES)


    def _clean_old_jsons(self, folder):
        '''removes old JSONs of this plugin from folder, to avoid stale files'''
        n = 0
        for fn in os.listdir(folder):
            if fn.endswith((SUF_TOKENS, SUF_FOLDS)):
                try:
                    os.remove(os.path.join(folder, fn))
                    n += 1
                except Exception:
                    pass
        return n


    def _write_info_file(self, folder):
        try:
            with open(os.path.join(folder, '_info.txt'), 'w',
                      encoding='utf-8', newline='\n') as f:
                f.write('Snapshot created by plugin "%s"\n' % TAG)
                f.write('date: %s\n' % time.strftime('%Y-%m-%d %H:%M:%S'))
                f.write('cudatext version: %s\n' % _app_ver())
                f.write('parse method: ed.action(EDACTION_LEXER_SCAN, "0")\n')
                f.write('verification: token window at the file end is complete\n'
                        'and stable, and the full token list reaches the\n'
                        'last non-blank line of the file\n')
                if self.settings_applied:
                    f.write('session settings: pinned temporarily via\n'
                            '  app_proc(PROC_CONFIG_READ), user.json untouched\n')
                    f.write('session settings json: %s\n'
                            % json.dumps(SESSION_SETTINGS))
                else:
                    f.write('session settings: NOT pinned (no PROC_CONFIG_READ\n'
                            '  API, or it failed), option values of this session\n'
                            '  were used as-is\n')
        except Exception as ex:
            _log('%s: cannot write _info.txt: %s' % (TAG, ex))


    def _process_corpus(self, save_dir):
        '''
        Parses all corpus files; returns (items, no_lex, lite_lex, errors).
        If save_dir is given, it is created at once and each parsed file
        is written to JSON right away: big token lists are never kept
        in memory until the end of the run (a multi-MB corpus file makes
        millions of token dicts, and the whole corpus must not pile
        them up).
        '''
        files = self._corpus_files()
        items, no_lex, lite_lex, errors = [], [], [], []
        total = len(files)

        if save_dir:
            try:
                os.makedirs(save_dir, exist_ok=True)
            except Exception as ex:
                msg_box('%s: cannot create folder:\n%s\n\n%s' %
                        (TAG, save_dir, ex), MB_OK + MB_ICONERROR)
                return [], [], [], [str(ex)]

        for i, fn in enumerate(files):
            msg_status('%s: parsing %d/%d: %s' % (TAG, i + 1, total, fn),
                       True)
            path = os.path.join(self.dir_lex, fn)

            e, opened_here = self._open_doc(path)
            if e is None:
                errors.append('%s: cannot open' % fn)
                continue

            try:
                try:
                    lexer = e.get_prop(PROP_LEXER_FILE)
                except Exception:
                    lexer = ''
                lexer = str(lexer or '')

                if _is_plain_lexer(lexer):
                    no_lex.append(fn)
                    continue
                if _is_lite_lexer(lexer):
                    # lite-lexers have no EControl tokens at all
                    lite_lex.append(fn)
                    continue

                data = self._parse_file_verified(e, fn, lexer)
                if save_dir:
                    _write_pretty_json(
                        os.path.join(save_dir, data['name'] + SUF_TOKENS),
                        _tokens_dict(data['lexer'], data['line_count'],
                                     data['tokens']),
                        'tokens')
                    _write_pretty_json(
                        os.path.join(save_dir, data['name'] + SUF_FOLDS),
                        _folds_dict(data['lexer'], data['folds']),
                        'folds')
                    # big lists are not needed anymore: keep only counts
                    items.append({
                        'name': data['name'],
                        'lexer': data['lexer'],
                        'line_count': data['line_count'],
                        'token_count': len(data['tokens']),
                        'fold_count': len(data['folds']),
                        })
                    del data
                else:
                    items.append(data)
            except Exception as ex:
                errors.append('%s: %s' % (fn, ex))
            finally:
                if opened_here:
                    self._close_doc(e)

        if save_dir:
            self._write_info_file(save_dir)

        return items, no_lex, lite_lex, errors


    #--------- helpers for commands ---------

    def _dirs_results(self):
        '''names of "results_*" folders, except results_base'''
        try:
            names = os.listdir(self.dir_plugin)
        except OSError:
            return []
        return sorted(
            n for n in names
            if n.startswith('results_')
            and n != 'results_base'
            and os.path.isdir(os.path.join(self.dir_plugin, n))
            )


    def _json_names(self, folder):
        '''names of JSON files this plugin created in the folder'''
        try:
            names = os.listdir(folder)
        except OSError:
            return set()
        return set(
            n for n in names
            if n.endswith((SUF_TOKENS, SUF_FOLDS))
            and os.path.isfile(os.path.join(folder, n))
            )


    def _load_json(self, path):
        with open(path, 'r', encoding='utf-8') as f:
            return json.load(f)


    #--------- reporting ---------

    def _report_text(self, report):
        items = report['items']
        counts = {}
        for it in items:
            counts[it['status']] = counts.get(it['status'], 0) + 1

        L = []
        L.append('=' * 60)
        L.append('Lexer testing report')
        L.append('=' * 60)
        L.append('First:  %s' % report['base'])
        L.append('Second: %s' % report['other'])
        L.append('Date:   %s' % time.strftime('%Y-%m-%d %H:%M:%S'))
        v = _app_ver()
        if v != '?':
            L.append('CudaText version: %s' % v)
        L.append('-' * 60)
        L.append('JSON files compared: %d' % len(items))
        for st in ('ok', 'diff', 'only_base', 'only_other', 'error'):
            if st in counts:
                L.append('  %-16s %d' % (STATUS_LABELS[st], counts[st]))
        L.append('-' * 60)
        L.append('Comparison is exact data equality: it detects any change')
        L.append('of values, and any change of token/fold order.')
        L.append('Contents of differences is not shown here.')
        L.append('-' * 60)
        L.append('Results:')
        for it in items:
            label = '[%s]' % STATUS_LABELS[it['status']]
            extra = ('   (%s)' % it['stat']) if it.get('stat') else ''
            L.append('  %-16s %s%s' % (label, it['name'], extra))
        return '\n'.join(L) + '\n'


    def _show_report(self, report):
        '''
        Shows summary dialog; opens report in a new tab.
        '''
        items = report['items']
        problems = [it for it in items if it['status'] != 'ok']

        L = ['Compared: %s vs %s' % (report['base'], report['other']),
             'JSON files compared: %d' % len(items)]
        for st, label in (
            ('diff', 'different'),
            ('only_base', 'only in first folder'),
            ('only_other', 'only in second folder'),
            ('error', 'errors'),
            ):
            cnt = sum(1 for it in items if it['status'] == st)
            if cnt:
                L.append('  %s: %d' % (label, cnt))

        if problems:
            L.append('')
            L.append('Not identical (%d):' % len(problems))
            for it in problems[:MAX_DIALOG_LIST]:
                extra = ('   (%s)' % it['stat']) if it.get('stat') else ''
                L.append('  %s%s' % (it['name'], extra))
            if len(problems) > MAX_DIALOG_LIST:
                L.append('  ... and %d more' % (len(problems) - MAX_DIALOG_LIST))
        else:
            L.append('')
            L.append('All files are identical.')

        L.append('')
        L.append('Report is opened in a new tab.')
        msg_box('\n'.join(L), MB_OK + (MB_ICONWARNING if problems else MB_ICONINFO))

        file_open('')
        ed.set_text_all(self._report_text(report))
        for prop, val in (
            (PROP_TAB_TITLE, 'Lexer testing report'),
            (PROP_LEXER_FILE, 'Diff'),
            ):
            try:
                ed.set_prop(prop, val)
            except Exception:
                pass

        _log('%s: compare finished, %d JSONs, %d not identical' %
             (TAG, len(items), len(problems)))
        msg_status('%s: compare: %d JSONs, %d not identical' %
                   (TAG, len(items), len(problems)))


    #--------- comparison engine ---------

    def _compare_folder_pair(self, dir_a, dir_b, label_a, label_b):
        '''
        Compares JSONs of two folders pair-by-name.
        Returns the list of per-file result items.
        '''
        names_a = self._json_names(dir_a)
        names_b = self._json_names(dir_b)
        items = []

        for fn in sorted(names_a | names_b):
            if fn not in names_b:
                items.append({'name': fn, 'status': 'only_base',
                              'stat': None})
                continue
            if fn not in names_a:
                items.append({'name': fn, 'status': 'only_other',
                              'stat': None})
                continue
            try:
                da = self._load_json(os.path.join(dir_a, fn))
                db = self._load_json(os.path.join(dir_b, fn))
            except Exception as ex:
                items.append({'name': fn, 'status': 'error',
                              'stat': str(ex)})
                continue
            # exact data equality: detects any change of values
            # and any change of token/fold order
            if da == db:
                items.append({'name': fn, 'status': 'ok',
                              'stat': None})
            else:
                items.append({'name': fn, 'status': 'diff',
                              'stat': _item_stat(da, db)})

        report = {
            'base': label_a,
            'other': label_b,
            'items': items,
            'errors': [],
        }
        self._show_report(report)
        return items


    #--------- commands ---------

    def make_snapshot(self):
        if not hasattr(cudatext, 'EDACTION_LEXER_SCAN'):
            msg_box('%s: this CudaText version has no API\n'
                    'ed.action(EDACTION_LEXER_SCAN), which is needed to parse\n'
                    'files from start to end.\n'
                    'Update CudaText to the latest version.' % TAG,
                    MB_OK + MB_ICONERROR)
            return
        if not os.path.isdir(self.dir_lex):
            try:
                os.makedirs(self.dir_lex, exist_ok=True)
            except Exception as ex:
                msg_box('%s: cannot create folder:\n%s\n\n%s' % (TAG, self.dir_lex, ex),
                        MB_OK + MB_ICONERROR)
                return
            msg_box('%s: folder "lexers" was not found and was created empty:\n'
                    '%s\n\nPut test files there and call this command again.'
                    % (TAG, self.dir_lex),
                    MB_OK + MB_ICONINFO)
            return

        files = self._corpus_files()
        if not files:
            msg_box('%s: no test files found in:\n%s' % (TAG, self.dir_lex),
                    MB_OK + MB_ICONWARNING)
            return

        if os.path.isdir(self.dir_base):
            s = dlg_input(
                'Name for the new snapshot folder "results_<name>"\n'
                '(enter "base" to overwrite results_base):',
                '')
            if not s:
                return
            s = s.strip()
            if s.lower() == 'base':
                if msg_box('Overwrite existing results_base?',
                           MB_YESNO + MB_ICONWARNING) != ID_YES:
                    return
                target = self.dir_base
                tname = 'results_base'
            else:
                for c in BAD_NAME_CHARS:
                    s = s.replace(c, '_')
                if not s:
                    msg_box('%s: incorrect folder name.' % TAG,
                            MB_OK + MB_ICONWARNING)
                    return
                target = os.path.join(self.dir_plugin, 'results_' + s)
                tname = 'results_' + s
                if os.path.isdir(target):
                    if msg_box('Folder already exists:\n%s\n\n'
                               'Overwrite JSONs in it?' % target,
                               MB_YESNO + MB_ICONWARNING) != ID_YES:
                        return
            if os.path.isdir(target):
                self._clean_old_jsons(target)
        else:
            target = self.dir_base
            tname = 'results_base'

        # pin option values for this session BEFORE any corpus file is
        # opened: ui_max_size_lexer is checked at file open, folding
        # and parse limits at parse time - all files of every snapshot
        # must be parsed under the same pinned options
        self._apply_session_settings()

        items, no_lex, lite_lex, errors = self._process_corpus(save_dir=target)

        msg_status('%s: snapshot saved: %s (%d files)' % (TAG, tname, len(items)))
        L = ['Snapshot saved to: %s' % tname,
             'Parsed files: %d' % len(items)]
        if no_lex:
            L.append('Skipped, no lexer (%d): %s' % (len(no_lex), ', '.join(no_lex)))
            # lexer-by-size limit which was active for THIS snapshot:
            # the pinned ui_max_size_lexer, or CudaText's 2 MB default
            # when pinning was not possible (very old CudaText)
            limit_mb = (SESSION_SETTINGS['ui_max_size_lexer']
                        if self.settings_applied else 2)
            big = []
            for f in no_lex:
                try:
                    if os.path.getsize(os.path.join(self.dir_lex, f)) >= \
                       limit_mb * 1024 * 1024:
                        big.append(f)
                except OSError:
                    pass
            if big:
                if self.settings_applied:
                    L.append('Note: these files are over the pinned limit of\n'
                             '%d MB (option "ui_max_size_lexer"), for which\n'
                             'CudaText disables the lexer: %s'
                             % (limit_mb, ', '.join(big)))
                else:
                    L.append('Note: for files over ~2 MB, CudaText itself can disable\n'
                             'the lexer by file size (option "ui_max_size_lexer",\n'
                             '2 MB by default). Increase it in Options Editor\n'
                             'to parse big files: %s' % ', '.join(big))
        if lite_lex:
            L.append('Skipped, lite lexer - no EControl tokens (%d): %s'
                     % (len(lite_lex), ', '.join(lite_lex)))
        if errors:
            L.append('Errors:')
            for s in errors:
                L.append('  ' + s)
        L.append('Folder: %s' % target)
        msg_box('\n'.join(L), MB_OK +
                (MB_ICONWARNING if errors else MB_ICONINFO))

        if tname != 'results_base' and items:
            if msg_box('Compare new snapshot with results_base now?',
                       MB_YESNO + MB_ICONINFO) == ID_YES:
                self._run_compare(tname)


    def compare_results(self):
        if not os.path.isdir(self.dir_base):
            msg_box('%s: no "results_base" folder yet,\n'
                    'call "make_snapshot" first.' % TAG,
                    MB_OK + MB_ICONWARNING)
            return
        others = self._dirs_results()
        if not others:
            msg_box('%s: no other "results_*" folders found,\n'
                    'call "make_snapshot" to create one.' % TAG,
                    MB_OK + MB_ICONWARNING)
            return
        if len(others) == 1:
            name = others[0]
        else:
            n = dlg_menu(DMENU_LIST, others, caption='Compare results_base with')
            if n is None:
                return
            name = others[n]
        self._run_compare(name)


    def _run_compare(self, other_name):
        if not self._json_names(self.dir_base):
            msg_box('%s: folder results_base has no JSON files,\n'
                    'call "make_snapshot" first.' % TAG,
                    MB_OK + MB_ICONWARNING)
            return
        other_dir = os.path.join(self.dir_plugin, other_name)
        self._compare_folder_pair(self.dir_base, other_dir,
                                  'results_base', other_name)


    def compare_two_folders(self):
        dir_a = dlg_dir(self.dir_plugin, 'Select FIRST folder to compare')
        if not dir_a:
            return
        dir_b = dlg_dir(self.dir_plugin, 'Select SECOND folder to compare')
        if not dir_b:
            return
        if os.path.normcase(os.path.abspath(dir_a)) == \
           os.path.normcase(os.path.abspath(dir_b)):
            msg_box('%s: the same folder was selected twice.' % TAG,
                    MB_OK + MB_ICONWARNING)
            return
        if not self._json_names(dir_a) and not self._json_names(dir_b):
            msg_box('%s: no JSON files (*%s, *%s) found in either folder:\n'
                    '%s\n%s' % (TAG, SUF_TOKENS, SUF_FOLDS, dir_a, dir_b),
                    MB_OK + MB_ICONWARNING)
            return
        self._compare_folder_pair(dir_a, dir_b, dir_a, dir_b)


    def open_dir(self):
        pairs = [
            ('plugin folder', self.dir_plugin),
            ('lexers (test files)', self.dir_lex),
        ]
        for d in ['results_base'] + self._dirs_results():
            path = os.path.join(self.dir_plugin, d)
            if os.path.isdir(path):
                pairs.append((d, path))
        n = dlg_menu(DMENU_LIST, [p[0] for p in pairs],
                     caption='Open folder in file manager')
        if n is None:
            return
        label, path = pairs[n]
        if not os.path.isdir(path):
            msg_box('%s: folder does not exist:\n%s' % (TAG, path),
                    MB_OK + MB_ICONWARNING)
            return
        _os_open_dir(path)
