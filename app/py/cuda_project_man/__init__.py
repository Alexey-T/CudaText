import os
import sys
import re
import collections
import json
import stat
import copy
import functools
import string
import time
import threading
from collections import deque
from fnmatch import fnmatch
from pathlib import Path, PurePosixPath
from .projman_glob import *
from .projman_backup import ProjectFileBackups, DEFAULT_BACKUP_COUNT, DEFAULT_BACKUP_FOLDER

from cudatext import *
from cudatext_keys import *
import cudatext_cmd

from cudax_lib import get_translation
_   = get_translation(__file__)  # i18n

from . import favorites as _favorites
try:
    _favorites.init_on_start()
except Exception as _favorites_startup_error:
    print('Project Manager: favorites startup failed:',
          _favorites_startup_error)

IS_WIN = os.name == 'nt'
IS_MAC = sys.platform == 'darwin'

S_CTRL_API = 'm' if IS_MAC else 'c'
S_CTRL_NAME = 'Meta' if IS_MAC else 'Ctrl'

# on_state code of the "UI theme changed" app event; the constant is
# missing in older cudatext.py builds, so a literal fallback is kept
try:
    APPSTATE_THEME_UI
except NameError:
    APPSTATE_THEME_UI = 21

ctypes = None
if IS_WIN:
    try:
        import ctypes
    except (ImportError, ModuleNotFoundError):
        pass

PROJECT_EXTENSION = ".cuda-proj"
PROJECT_DIALOG_FILTER = _("CudaText projects") + "|*" + PROJECT_EXTENSION
PROJECT_UNSAVED_NAME = _("(No project)")
NODE_PROJECT, NODE_DIR, NODE_FILE, NODE_BAD = range(4)
OS_SUFFIX = app_proc(PROC_GET_OS_SUFFIX, '')

#: delay (ms) after the last keystroke in the tree filter field before
#: the (possibly slow) filtered scan starts: it walks the whole project
#: tree recursively, and running it per keystroke hammered the disk
FILTER_APPLY_DELAY_MS = 400

#: cache of tree node icons: (name, is_dir, locked) -> icon index.
#: The lexer detection per file name is not free, and the filtered
#: fill asks for thousands of icons
_icon_cache = {}
DEF_SES = 'default'+OS_SUFFIX+'.cuda-session'
# Suffix of the internal session created by the automatic import of the
# current (external) session when an old-format project file is opened:
# appended to the external session's name, e.g. 'default' ->
# 'default auto-imported' (a number is appended on collisions).
# Deliberately NOT translated: a session name is DATA stored inside the
# project file, it must stay identical for every UI language (same rule
# as the 'imported' fallback name in action_import_external_session).
AUTO_IMPORT_TAG = ' auto-imported'

# Dedicated migration guide for the old project file format, which the
# v2-upgrade dialog offers to open: readme/upgrade_v2.txt. The whole
# file is about the v2 upgrade, so _open_plugin_doc_v2() simply opens
# it at the top -- no scrolling to a section inside the long readme is
# needed. readme.txt keeps a short "UPGRADE TO PROJECT FILE FORMAT V2"
# section that mentions this file; keep the file name in sync with that
# section (tests assert the file exists and that the readme mentions it).
DOC_FILE_V2 = 'upgrade_v2.txt'

global_project_info = {}
sort_order = 'ext'

#==========================================================
# background filtering of the project tree
#
# The filtered scan walks the WHOLE project recursively; on a big
# folder tree that takes seconds, so it runs in a background THREAD
# (pure os calls, no CudaText API - the API is not thread-safe). The
# worker makes exactly ONE pass over the disk (every folder is listed
# at most once, no re-scans, no cached listings - the old listing
# cache kept millions of os.DirEntry objects in RAM while scanning a
# drive-sized project and CudaText's memory grew by gigabytes) and
# builds the result as a LAZY TREE INDEX instead of a flat row list:
# parent folder path -> [(name, path, is_dir), ...].
#
# What the filtered tree shows (the matching rules):
#   - a FILE whose name matches the filter;
#   - a FOLDER whose own name matches, or which contains (at any
#     depth) a matching node - so the path down to every match shows.
#     A name-matching folder does NOT bring its whole content: its
#     children follow the same two rules.
#
# The live behavior the user sees:
#   - typing hides every non-matching node AT ONCE (the instant view
#     of _filter_start shows only the name-matching root nodes);
#   - while the scan runs, the status line counts the matches so far,
#     and every progress tick REBUILDS the visible tree from the
#     worker's partial index: a folder appears the moment a match is
#     found inside it, and a folder the user expanded gets every new
#     finding as it comes (nothing shows only when nothing was found
#     yet - the rebuild is skipped while the worker has no news);
#   - cancelling the search KEEPS the findings so far (the tree is
#     rebuilt from a cleaned snapshot of the partial index);
#   - when the scan finishes, the tree is rebuilt from the final
#     index; deeper levels load LAZILY when their folder is expanded
#     (instant, no disk access) - a huge match list never floods the
#     tree at once, and expanding a folder while the filter is active
#     costs exactly as little as expanding it without a filter.
#==========================================================

#: interval (ms) of the UI timer which polls the running filter worker
FILTER_PROGRESS_MS = 150


def _direntry_ordering(path, order):
    """Sort key of one os.DirEntry / pathlib.Path for an explicit
    sort order (the same rules Command.node_ordering_direntry applies
    to the visible tree). Module-level so the background worker can
    use it without touching the plugin state."""
    isfile = path.is_file()
    if order == 'ext':
        key = _file_ext(path.name).upper() if isfile else ''
    elif order == 'name':
        key = path.name.upper()
    elif order == 'size':
        key = -path.stat().st_size if isfile else ''
    elif order == 'size-':
        key = path.stat().st_size if isfile else ''
    elif order == 'mtime':
        key = -path.stat().st_mtime if isfile else ''
    elif order == 'mtime-':
        key = path.stat().st_mtime if isfile else ''
    elif order == 'ctime':
        key = -path.stat().st_ctime if isfile else ''
    elif order == 'ctime-':
        key = path.stat().st_ctime if isfile else ''
    else:
        key = ''
    return isfile, key, path.name.upper()


def scan_dir_entries(fn, opts):
    """Children of a folder as os.DirEntry objects, with the ignore
    rules from the given options dict applied and sorted like the
    tree; None when the folder cannot be read. Symlinked folders are
    skipped (no cycles). NOT cached: the filtered scan walks each
    folder once anyway, and a cache of the whole walk lived in RAM
    for gigabytes on drive-sized projects (see the "background
    filtering" note above). One uncached scandir of a single folder
    is what the normal (unfiltered) tree expand does too - it is
    fast.

    Called on the UI thread AND from the background filter worker
    (os.scandir is safe to call from several threads)."""
    try:
        entries = list(os.scandir(fn))
    except OSError:
        return None
    res = []
    no_hidden = opts.get("no_hidden", True)
    no_files = str(opts.get("no_files", "") or "")
    no_dirs = str(opts.get("no_dirs", "") or "")
    for e in entries:
        try:
            is_dir = e.is_dir()
        except OSError:
            continue
        if is_dir and e.is_symlink():
            continue
        if no_hidden and is_hidden(e.path):
            continue
        msk = no_dirs if is_dir else no_files
        if msk and is_mask_listed(e.name, msk):
            continue
        res.append(e)

    order = opts.get('sort_order', 'ext')
    if order != '':
        try:
            res = sorted(res, key=lambda p: _direntry_ordering(p, order))
        except OSError:
            pass
    else:
        res.sort(key=lambda e: (not e.is_dir(), e.name.upper()))
    return res


class _FilterWorker(threading.Thread):
    """Background scan for the tree filter. Walks the project's root
    nodes ONCE, BREADTH-FIRST, and builds the LAZY TREE INDEX of the
    result:
        {parent folder path: [(name, path, is_dir), ...]}
    (the project root's children live under the key ''). The UI thread
    rebuilds the visible part of the tree from this index on every
    progress tick, so the index must be readable WHILE the scan runs:
      - every scanned folder gets an OPTIMISTIC row in its parent's
        bucket at once (the final cleanup drops the rows of the folders
        in which no match was ever found);
      - `live` collects the folders allowed to show: folders whose own
        name matches, and every ancestor folder of a match - so a
        folder row shows from the moment the scan finds a match inside
        it, and the whole path down to a match is visible at once.

    The matching rules (what the filtered tree shows):
      - a FILE whose name matches the filter;
      - a FOLDER whose own name matches, or which contains (at any
        depth) a matching node. A name-matching folder does NOT bring
        its whole content: its children follow the same two rules.

    The worker NEVER calls the CudaText API (not thread-safe) - it only
    touches the disk. The UI thread polls .done/.count/.version with a
    timer and reads .res/.live while the scan runs; a new filter text
    cancels the previous worker co-operatively (the .cancel flag is
    checked around every folder). The walk is ITERATIVE (an explicit
    FIFO queue), not recursive: a drive-deep folder chain cannot hit
    Python's recursion limit, and the breadth-first order makes the
    live tree grow where the user looks at it (shallow levels first)."""

    def __init__(self, text, roots):
        threading.Thread.__init__(self)
        self.daemon = True
        self.text = text
        #: [(spath, sname, is_dir)] of the project's root nodes,
        #: pre-filtered and sorted by the UI thread
        self.roots = roots
        #: snapshot of the options which rule the scan
        self.opts = {}
        self.cancel = False
        #: matches found so far - files whose name matches plus folders
        #: whose name matches (a folder which only CONTAINS matches is
        #: not itself a match): the live counter of the status line
        self.count = 0
        #: the PARTIAL lazy index, readable by the UI thread WHILE the
        #: scan runs: {parent path: [(name, path, is_dir)]}. During the
        #: walk buckets are only ever APPENDED to; the final cleanup
        #: REPLACES bucket lists instead of mutating them, so a
        #: concurrent UI read is always a consistent snapshot (CPython
        #: list/dict reads are safe against that)
        self.res = {}
        #: folder paths whose row is allowed to SHOW in the live tree: a
        #: folder whose own name does not match the filter stays hidden
        #: until the scan finds a match INSIDE it - the exact rule the
        #: live filtered tree follows. A marked folder always implies
        #: its ancestors are marked too
        self.live = set()
        #: bumped on every index/live change, so the UI timer can skip
        #: idle refreshes (e.g. the worker stuck on one slow network
        #: folder)
        self.version = 0
        #: the finished result: {parent path: [(name, path, is_dir)]}
        #: (cleaned of the optimistic rows; None when cancelled)
        self.tree = None
        self.done = False

    def _mark_chain(self, path):
        """Mark the folder `path` and every ancestor folder up to the
        top as live-visible. Early-exits on an already-marked folder
        (chains are always marked whole, so a marked folder implies
        that all its ancestors are marked too). Harmlessly also marks
        paths above the project roots: no tree row exists for them."""
        live = self.live
        p = path
        while p and p not in live:
            live.add(p)
            nxt = os.path.dirname(p)
            if nxt == p:
                break
            p = nxt

    def run(self):
        try:
            res = self.res
            flt = self.text.lower()
            queue = deque()
            # roots: every root FOLDER gets its (optimistic) top-level
            # row immediately, so the top level of the live tree is
            # complete from the very start (a root folder shows as soon
            # as a match is found inside it, a name-matching root at
            # once); a root FILE only shows when its name matches
            for spath, sname, is_dir in self.roots:
                if is_dir:
                    res.setdefault('', []).append((sname, spath, True))
                    if flt in sname.lower():
                        self.count += 1
                        self._mark_chain(spath)
                    queue.append(spath)
                elif flt in sname.lower():
                    res.setdefault('', []).append((sname, spath, False))
                    self.count += 1
            self.version += 1
            while queue:
                if self.cancel:
                    break
                path = queue.popleft()
                for e in scan_dir_entries(path, self.opts) or []:
                    if self.cancel:
                        break
                    if e.is_dir():
                        # optimistic row: hidden (not live) until a
                        # match is found inside, or the name matches
                        res.setdefault(path, []).append(
                            (e.name, e.path, True))
                        if flt in e.name.lower():
                            self.count += 1
                            self._mark_chain(e.path)
                        queue.append(e.path)
                    elif flt in e.name.lower():
                        res.setdefault(path, []).append(
                            (e.name, e.path, False))
                        self.count += 1
                        # a match was found: the folder chain above
                        # the file becomes visible at once
                        self._mark_chain(path)
                self.version += 1
            if not self.cancel:
                # final cleanup: drop the optimistic rows of the folders
                # in which no match was ever found (the live tree never
                # showed them - the UI filters folder rows by `live`);
                # buckets which end up empty are removed completely, so
                # the finished index holds only real findings
                cleaned = {}
                for key, bucket in res.items():
                    rows = [r for r in bucket
                            if not r[2] or r[1] in self.live]
                    if rows:
                        cleaned[key] = rows
                self.tree = cleaned
                self.version += 1
        except Exception:
            self.tree = {}
        finally:
            self.done = True


# Project file format version written by this plugin. Files without a
# "version" key are the old format (version 1) and are upgraded on open.
PROJECT_FORMAT_VERSION = 2

DRAG_HOVER_TIMER_CALLBACK = 'module=cuda_project_man;cmd=_drag_hover_tick;'


def load_project_file(path):
    """Load and validate a .cuda-proj file.
    Returns (data, error): exactly one of them is non-None.
    `data` is the parsed JSON dict; `error` is a string 'kind[:detail]'
    with kind one of: io, empty, json, type, nodes, sessions."""
    try:
        with open(path, encoding='utf8') as fin:
            text = fin.read()
    except OSError as ex:
        return None, 'io: %s' % ex

    if not text.strip():
        return None, 'empty'

    try:
        data = json.loads(text)
    except ValueError as ex:
        return None, 'json: %s' % ex

    if not isinstance(data, dict):
        return None, 'type'

    nodes = data.get('nodes')
    if nodes is not None:
        if not isinstance(nodes, list) or not all(isinstance(n, str) for n in nodes):
            return None, 'nodes'

    sessions = data.get('sessions')
    if sessions is not None and not isinstance(sessions, dict):
        return None, 'sessions'

    return data, None


def normalize_project_data(data):
    """Ensure a project dict has all standard keys with correct types.
    Only adds missing keys / fixes types; never removes user keys.
    - last_session: name of the session that was used last; filled when a
      session is switched to, and loaded when the project is opened.
    Does not touch the 'version' key (managed by the save/open code)."""
    if not isinstance(data, dict):
        data = {}
    if not isinstance(data.get('nodes'), list):
        data['nodes'] = []
    else:
        data['nodes'] = [n for n in data['nodes'] if isinstance(n, str)]
    if not isinstance(data.get('vars'), list):
        data['vars'] = []
    if not isinstance(data.get('mainfile'), str):
        data['mainfile'] = ''
    if not isinstance(data.get('unfold'), list):
        data['unfold'] = []
    if not isinstance(data.get('sessions'), dict):
        data['sessions'] = {}
    if not isinstance(data.get('last_session'), str):
        data['last_session'] = ''
    return data


def make_project_backup(path):
    """Copy a project file to a backup next to it, before an automatic
    upgrade rewrites the file. Backup name: '<file>.bak'; if that name is
    already taken (an earlier upgrade, or the user's own file), a
    timestamped name '<file>.<yymmdd_hhmmss>.bak' is used — an existing
    backup is never overwritten.
    Returns the backup path, or None when the copy failed (read-only
    folder, locked file, ...)."""
    from datetime import datetime
    import shutil
    p = Path(path)
    bak = p.with_name(p.name + '.bak')
    if bak.exists():
        stamp = datetime.now().strftime('%y%m%d_%H%M%S')
        bak = p.with_name('%s.%s.bak' % (p.name, stamp))
        if bak.exists():
            return None
    try:
        shutil.copy2(str(p), str(bak))
    except OSError:
        return None
    return bak


def is_session_name(s):
    allowed = string.ascii_letters+string.digits+'., ()-+_$%='
    for ch in s:
        if not ch in allowed:
            return False
    return True


def write_json_file(path, data):
    """Write `data` as pretty JSON to `path`, atomically: the JSON goes to a
    temporary file next to the target first, which then replaces the target
    in one step. A crash / power loss / full disk in the middle of the write
    can never leave a truncated file behind — for a .cuda-proj file that
    would lose ALL project data (nodes + every embedded session). Either
    the file is replaced completely, or the previous complete file stays
    intact and the error propagates to the caller (callers report OSError
    to the user)."""
    _write_text_file_atomic(path, json.dumps(data, indent=2))

def _write_text_file_atomic(path, text):
    """Write already-serialized `text` to `path` atomically (see
    write_json_file). Shared by the JSON writer and the surgical single-key
    editor of _set_last_session_in_file."""
    tmp = str(path) + '.tmp' + str(os.getpid())
    try:
        with open(tmp, 'w', encoding='utf8') as fout:
            fout.write(text)
        os.replace(tmp, str(path))
    finally:
        # the replace didn't happen -> remove the temp file
        if os.path.exists(tmp):
            try:
                os.unlink(tmp)
            except OSError:
                pass

# Helpers for the surgical single-key editor of _set_last_session_in_file:
# locating a JSON key in the raw text of a .cuda-proj without parsing it.
def _quote_is_structural(text, pos):
    """True when the '"' at text[pos] is a structural JSON quote (opening/
    closing a key or value), not an escaped one inside a string value: an
    escaped quote is preceded by an odd number of backslashes."""
    i = pos - 1
    bs = 0
    while i >= 0 and text[i] == '\\':
        bs += 1
        i -= 1
    return bs % 2 == 0

def _find_structural_keys(text, key):
    """Positions of the (quoted) JSON key `key` (e.g. '"last_session"')
    in `text`, counting only occurrences whose opening quote is structural.
    Uses str.find (C-speed scanning): a regex finditer over the tens of MB
    of a .cuda-proj with embedded session texts costs hundreds of ms, this
    costs one linear memmem pass. A string VALUE which happens to equal the
    key text can still show up here (its quotes are structural too) — the
    caller tolerates that by requiring exactly one hit and verifying the
    colon, and falls back to the safe full rewrite otherwise."""
    res = []
    pos = text.find(key)
    while pos != -1:
        if _quote_is_structural(text, pos):
            res.append(pos)
        pos = text.find(key, pos + 1)
    return res

def _file_open(fn, options=''):
    gr = ed.get_prop(PROP_INDEX_GROUP)
    #print('Opening file in group %d'%gr)
    file_open(fn, group=gr, options=options)

# don't allow spaces and punctuation in file extension
EXT_CH = string.ascii_letters + string.digits + '.-_'

def _file_ext(fn):
    _, s = os.path.splitext(fn)
    for ch in s:
        if not ch in EXT_CH:
            return ''
    return s

def _file_editor(fn):
    for h in ed_handles():
        e = Editor(h)
        if e.get_prop(PROP_FN, '*')==fn:
            return e
    return None

# https://stackoverflow.com/questions/377017/test-if-executable-exists-in-python
def which(program):
    def is_exe(fpath):
        return os.path.isfile(fpath) and os.access(fpath, os.X_OK)

    fpath, fname = os.path.split(program)
    if fpath:
        if is_exe(program):
            return program
    else:
        for path in os.environ["PATH"].split(os.pathsep):
            exe_file = os.path.join(path, program)
            if is_exe(exe_file):
                return exe_file

    return None

def project_variables():
    """
    gives dict with "project variables", which is ok for using from other plugins,
    e.g. ExtTools.
    add to names {} or $() if you want.
    1) predefined var ProjMainFile (defined by right-click menu in ProjMan)
    2) predefined var ProjDir (dir of .cuda-proj file)
    3) other vars are defined by user in Proj Properties dialog.
    """
    res = collections.OrderedDict()
    data = global_project_info
    res['ProjDir'] = os.path.dirname(data.get('filename', ''))

    fn = data.get('mainfile', '')
    res['ProjMainFile'] = fn
    res['ProjMainFileNameOnly'] = os.path.basename(fn)
    res['ProjMainFileNameNoExt'] = '.'.join(os.path.basename(fn).split('.')[0:-1])

    data = global_project_info.get('vars', [])
    for item in data:
        s1, s2 = item.split('=', maxsplit=1)
        res[s1] = s2
    return res

NodeInfo = collections.namedtuple("NodeInfo", "caption image")

_homedir = os.path.expanduser('~')
_appdir = os.path.dirname(app_path(APP_DIR_SETTINGS))

def collapse_macros(proj_dir, fn):
    if proj_dir and (fn+os.sep).startswith(proj_dir+os.sep):
        fn = fn.replace(proj_dir, '{ProjDir}', 1)
    if (fn+os.sep).startswith(_appdir+os.sep):
        fn = fn.replace(_appdir, '{AppDir}', 1)
    fn = fn.replace(os.sep, '/') if IS_WIN else fn
    return fn

def expand_macros(proj_dir, s):
    if proj_dir:
        s = s.replace('{ProjDir}', proj_dir, 1)
    s = s.replace('{AppDir}', _appdir, 1)
    return os.path.normpath(s)

def collapse_filename(fn):
    if (fn+'/').startswith(_homedir+'/'):
        fn = fn.replace(_homedir, '~', 1)
    return fn

def resolve_session_pointer(ptr):
    """Expand the file part of a session pointer the same way the app does
    (AppFile_Session): a pointer whose file part carries no directory names
    a file in the settings dir. The app stores such pointers with the
    settings dir stripped (AppSessionName_ForHistoryFile writes this into
    history.json), so history.json can contain e.g. 'sw.cuda-proj|/sessions/sw'
    for the project '<settings>/sw.cuda-proj'. Comparing that raw value with
    absolute project paths never matches and made on_start detach the
    project on every restart (issue #6591): resolve the pointer first."""
    if '|' not in ptr:
        return ptr
    fn, sep, rest = ptr.partition('|')
    if fn and not os.path.dirname(fn):
        fn = os.path.join(app_path(APP_DIR_SETTINGS), fn)
    return fn + sep + rest

def same_path(a, b):
    """Filename comparison tolerant to separators and (on Windows only)
    letter case, mirroring the app's SameFileName."""
    if not a or not b:
        return False
    return os.path.normcase(os.path.normpath(a)) == os.path.normcase(os.path.normpath(b))

def nice_filename(path):
    return os.path.basename(path) + ' ('+ collapse_filename(os.path.dirname(path)) + ')'


def is_simple_listed(name, masks):
    s = name.lower()
    for mask in masks.split(' '):
        if s.endswith(mask):
            return True
    return False

def is_mask_listed(s, masks):
    for mask in masks.split(';'):
        r = fnmatch(s, mask)
        #print("fnmatch('%s', '%s') = %d"%(s, mask, int(r)))
        if r:
            return True
    return False

# only Py 3.5 supports os.stat(s).st_file_attributes
# so this is to support Py 3.4
def is_hidden_win32(s):
    if not ctypes:
        return False
    try:
        attrs = ctypes.windll.kernel32.GetFileAttributesW(s)
        assert attrs != -1
        res = bool(attrs & 2)
    except (AttributeError, AssertionError):
        res = False
    return res

def is_hidden(s):
    if IS_WIN:
        if s=='':
            return False
        if s.endswith(':\\'):
            return False

        return is_hidden_win32(s)
        #try:
        #    return bool(os.stat(s).st_file_attributes & stat.FILE_ATTRIBUTE_HIDDEN)
        #except Exception:
        #    return True

    else:
        return os.path.basename(s).startswith('.')

def is_win_root(s):
    return IS_WIN and s.endswith(':\\')

def _rtrim_sep(s):
    """Path without its trailing separator(s): 'D:\\' -> 'D:',
    '\\\\server\\share\\' -> '\\\\server\\share'. Needed to compare
    paths that keep/drop the trailing separator in different places
    (the tree, the saved nodes list, the OS dialogs)."""
    if not s:
        return s
    return s.rstrip('\\/' if IS_WIN else '/')

def path_equals(a, b):
    """True if the two paths are equal, IGNORING the trailing separator.
    The tree stores a root node's path as pathlib renders it, which KEEPS
    the backslash of a drive root ('D:\\') but DROPS it for a UNC root
    ('\\\\server\\share'), while the saved nodes list keeps what the
    folder dialog returned ('D:\\', '\\\\server\\share\\') - so an exact
    'x in nodes' compare failed for BOTH root kinds and 'Stop tracking'
    reported 'cannot find path' for a whole drive."""
    if not a or not b:
        return False
    if a == b:
        return True
    return _rtrim_sep(a) == _rtrim_sep(b)

def path_contains(parent, child):
    """True if 'child' is 'parent' itself or lies under it (the trailing
    separator is ignored on both sides; the boundary accepts both '\\'
    and '/', so mixed-separator paths compare correctly too)."""
    p = _rtrim_sep(parent)
    c = _rtrim_sep(child)
    if not p or not c:
        return False
    if c == p:
        return True
    # the char right after the common prefix must be a separator:
    # 'D:\\data' is under 'D:' but 'D:\\database' is NOT under 'D:\\data'
    return c.startswith(p) and len(c) > len(p) and c[len(p)] in '\\/'

def is_locked(s):
    if IS_WIN:
        if s.endswith(':\\'):
            return False
        return is_hidden_win32(s)
    else:
        return not os.access(s, os.R_OK)


def _toolbar_add_btn(h_bar, hint, icon=-1, command=''):
    toolbar_proc(h_bar, TOOLBAR_ADD_ITEM)
    cnt = toolbar_proc(h_bar, TOOLBAR_GET_COUNT)
    h_btn = toolbar_proc(h_bar, TOOLBAR_GET_BUTTON_HANDLE, index=cnt-1)
    if hint=='-':
        button_proc(h_btn, BTN_SET_KIND, BTNKIND_SEP_HORZ)
    else:
        button_proc(h_btn, BTN_SET_KIND, BTNKIND_ICON_ONLY)
        button_proc(h_btn, BTN_SET_HINT, hint)
        button_proc(h_btn, BTN_SET_IMAGEINDEX, icon)
        button_proc(h_btn, BTN_SET_DATA1, command)


def _time_am_pm():
    """Current local time as e.g. '4:23:07 AM' (12-hour clock, no
    leading zero, like the OS clock shows it) -- the timestamp of the
    console report of an automatic project backup."""
    from datetime import datetime
    return datetime.now().strftime('%I:%M:%S %p').lstrip('0')


def _with_backup_cause(cause):
    """Decorator for the Command methods which trigger session saves
    themselves. While a decorated method runs:

    - the automatic project backup made by its saves is labeled with
      `cause` in the console report ('Project Manager: project backup
      <cause> at <time>: ...');
    - its saves are NOT backed up one by one. One command can save the
      session several times in a row: 'Save session as...' saves the
      currently active slot AND the new one (two rewrites of the whole
      .cuda-proj), 'Save project as...' into a new path saves the old
      project's slot and the new file's slot. Backing every save up
      would rotate the backup set on half-done intermediate states --
      after 'Save session as...', the newest backup lacked the very
      session the command was creating, while another slot held the
      same state minus that session. So every APPSTATE_SESSION_SAVE_AFTER
      made inside the flow only REMEMBERS the project file it may have
      changed (on_state appends it to _backup_pending); ONE backup per
      remembered file is made when the OUTERMOST decorated method of
      the flow ends -- the final on-disk state, one rotation step.

    Only the OUTERMOST decorated method of a flow sets the label --
    action_save_project -> action_save_project_as -> session_save_as
    reports the action the user actually invoked ('on project save'),
    not the internal helper. Saves made while NO decorated method runs
    (CudaText's ~40 s session autosave, or its session manager writing
    the current session) keep the default label 'by timer' and are
    backed up immediately, one event at a time (single saves: nothing
    to coalesce)."""
    def deco(fn):
        @functools.wraps(fn)
        def wrapper(self, *args, **kwargs):
            outermost = not self._backup_flow
            prev = self._backup_cause
            if outermost:
                if not prev:
                    self._backup_cause = cause
                self._backup_pending = []
            self._backup_flow += 1
            try:
                return fn(self, *args, **kwargs)
            finally:
                self._backup_flow -= 1
                if outermost:
                    pending = self._backup_pending
                    label = self._backup_cause
                    self._backup_pending = []
                    self._backup_cause = prev
                    for fn_proj in pending:
                        self._backup_file_reported(fn_proj, label)
        return wrapper
    return deco


class Command:
    goto_history = []
    cur_dir = ''
    project_file_path = None
    # guard for on_state(APPSTATE_SESSION_SET): True while the plugin
    # itself changes the app session (see Command._set_session)
    _session_set_by_plugin = False

    # mouse movement (in px) after mouse-down, which starts a drag gesture
    # in the project tree (same value as LCL's default drag threshold)
    TREE_DRAG_THRESHOLD = 5
    # hovering a FOLDED folder node for this many seconds during a drag&drop
    # (inner drag&drop, or files dragged in from the OS file manager) unfolds
    # it, like OS file managers do
    TREE_UNFOLD_HOVER_SEC = 0.7
    # interval of the timer which runs the unfold-on-hover state machine:
    # events alone (mouse moves, on_drag_over) are not enough, the cursor is
    # often held STILL over a folder - exactly the case which must unfold
    DRAG_HOVER_TIMER_MS = 100

    title ="Project" # No _() here, the translation is offered in "translation template.ini".
    menuitems = (
        # item_caption, item_parent, item_types, item_action, item_hotkey
        (_("Create new empty project..."), "proj", [None, NODE_PROJECT, NODE_DIR, NODE_FILE, NODE_BAD], "cuda_project_man.action_new_project", ""),
        (_("Create new project from open tabs..."), "proj", [None, NODE_PROJECT, NODE_DIR, NODE_FILE, NODE_BAD], "cuda_project_man.action_save_session_as_project", ""),
        (_("Open project..."), "proj", [None, NODE_PROJECT, NODE_DIR, NODE_FILE, NODE_BAD], "cuda_project_man.action_open_project", ""),
        (_("Recent projects"), "proj", [None, NODE_PROJECT, NODE_DIR, NODE_FILE, NODE_BAD], "collect_recent_projects", ""),
        ("-", "proj", [None, NODE_PROJECT, NODE_DIR, NODE_FILE, NODE_BAD], "", ""),
        (_("Save project"), "proj", [None, NODE_PROJECT, NODE_DIR, NODE_FILE, NODE_BAD], "cuda_project_man.action_save_project", ""),
        (_("Save project as..."), "proj", [None, NODE_PROJECT, NODE_DIR, NODE_FILE, NODE_BAD], "cuda_project_man.action_save_project_as", ""),
        (_("Backup project file now"), "proj", [None, NODE_PROJECT, NODE_DIR, NODE_FILE, NODE_BAD], "cuda_project_man.action_backup_project_now", ""),
        (_("Close project"), "proj", [None, NODE_PROJECT, NODE_DIR, NODE_FILE, NODE_BAD], "cuda_project_man.action_close_project", ""),
        (_("Open project folder"), "proj", [None, NODE_PROJECT, NODE_DIR, NODE_FILE, NODE_BAD], "cuda_project_man.action_open_project_folder", ""),
        ("-", "proj", [None, NODE_PROJECT, NODE_DIR, NODE_FILE, NODE_BAD], "", ""),
        (_("Project properties..."), "proj", [None, NODE_PROJECT, NODE_DIR, NODE_FILE, NODE_BAD], "cuda_project_man.action_project_properties", ""),

        (_("Add folder..."), "nodes", [None, NODE_PROJECT, NODE_DIR, NODE_FILE, NODE_BAD], "cuda_project_man.action_add_folder", ""),
        (_("Add file..."), "nodes", [None, NODE_PROJECT, NODE_DIR, NODE_FILE, NODE_BAD], "cuda_project_man.action_add_file", ""),
        (_("Add current file"), "nodes", [None, NODE_PROJECT, NODE_DIR, NODE_FILE, NODE_BAD], "cuda_project_man.add_current_file", ""),
        (_("Add all opened files"), "nodes", [None, NODE_PROJECT, NODE_DIR, NODE_FILE, NODE_BAD], "cuda_project_man.add_opened_files", ""),
        (_("Add folder of current file"), "nodes", [None, NODE_PROJECT, NODE_DIR, NODE_FILE, NODE_BAD], "cuda_project_man.add_current_dir", ""),
        ("-", "nodes", [None, NODE_PROJECT, NODE_DIR, NODE_FILE, NODE_BAD], "", ""),
        (_("Stop tracking this folder/file"), "nodes", [None, NODE_PROJECT, NODE_DIR, NODE_FILE, NODE_BAD], "cuda_project_man.action_remove_node", ""),
        (_("Clean up missing paths..."), "nodes", [None, NODE_PROJECT, NODE_DIR, NODE_FILE, NODE_BAD], "cuda_project_man.action_remove_deleted_nodes", ""),

        (_("New file..."), "dir", [NODE_DIR], "cuda_project_man.action_new_file", S_CTRL_NAME + "+N"),
        (_("New folder..."), "dir", [NODE_DIR], "cuda_project_man.action_new_directory", "F7"),
        ("-", "dir", [NODE_DIR], "", ""),
        (_("Cut"), "dir", [NODE_DIR], "cuda_project_man.action_cut", S_CTRL_NAME + "+X"),
        (_("Copy"), "dir", [NODE_DIR], "cuda_project_man.action_copy", S_CTRL_NAME + "+C"),
        (_("Paste"), "dir", [NODE_DIR], "cuda_project_man.action_paste", S_CTRL_NAME + "+V"),
        ("-", "dir", [NODE_DIR], "", ""),
        (_("Copy path"), "dir", [NODE_DIR], "cuda_project_man.action_copy_path", ""),
        (_("Copy relative path") , "dir", [NODE_DIR], "cuda_project_man.action_copy_relative_path", ""),
        ("-", "dir", [NODE_DIR], "", ""),
        (_("Rename..."), "dir", [NODE_DIR], "cuda_project_man.action_rename", "F2"),
        (_("Duplicate..."), "dir", [NODE_DIR], "cuda_project_man.action_duplicate", ""),
        (_("Delete"), "dir", [NODE_DIR], "cuda_project_man.action_delete_directory", "Del"),
        ("-", "dir", [NODE_DIR], "", ""),
        (_("Find in folder..."), "dir", [NODE_DIR], "cuda_project_man.action_find_in_directory", ""),
        ("-", "dir", [NODE_DIR], "", ""),
        (_("Reveal in file manager"), "dir", [NODE_DIR], "cuda_project_man.action_focus_in_fileman", ""),
        (_("Properties..."), "dir", [NODE_DIR], "cuda_project_man.action_get_properties", "Alt+Enter"),

        (_("Open in default application"), "file", [NODE_FILE], "cuda_project_man.action_open_def", ""),
        (_("Set as main file"), "file", [NODE_FILE], "cuda_project_man.action_set_as_main_file", ""),
        ("-", "file", [NODE_FILE], "", ""),
        (_("Cut"), "file", [NODE_FILE], "cuda_project_man.action_cut", S_CTRL_NAME + "+X"),
        (_("Copy"), "file", [NODE_FILE], "cuda_project_man.action_copy", S_CTRL_NAME + "+C"),
        (_("Paste"), "file", [NODE_FILE], "cuda_project_man.action_paste", S_CTRL_NAME + "+V"),
        ("-", "file", [NODE_FILE], "", ""),
        (_("Copy path"), "file", [NODE_FILE], "cuda_project_man.action_copy_path", ""),
        (_("Copy relative path"), "file", [NODE_FILE], "cuda_project_man.action_copy_relative_path", ""),
        ("-", "file", [NODE_FILE], "", ""),
        (_("Rename..."), "file", [NODE_FILE], "cuda_project_man.action_rename", "F2"),
        (_("Duplicate..."), "file", [NODE_FILE], "cuda_project_man.action_duplicate", ""),
        (_("Delete"), "file", [NODE_FILE], "cuda_project_man.action_delete_file", "Del"),
        ("-", "file", [NODE_FILE], "", ""),
        (_("Reveal in file manager"), "file", [NODE_FILE], "cuda_project_man.action_focus_in_fileman", ""),
        (_("Properties..."), "file", [NODE_FILE], "cuda_project_man.action_get_properties", "Alt+Enter"),

        ("-"   , "", [None, NODE_PROJECT, NODE_DIR, NODE_FILE, NODE_BAD], "", ""),
        (_("Refresh"), "", [None, NODE_PROJECT, NODE_DIR, NODE_FILE, NODE_BAD], "cuda_project_man.action_refresh", "F5"),
        (_("Show current file in project tree"), "", [None, NODE_PROJECT, NODE_DIR, NODE_FILE, NODE_BAD], "cuda_project_man.sync_to_ed", ""),
        ("-"   , "", [None, NODE_PROJECT, NODE_DIR, NODE_FILE, NODE_BAD], "", ""),
        (_("Go to file...")  , "", [None, NODE_PROJECT, NODE_DIR, NODE_FILE, NODE_BAD], "cuda_project_man.action_go_to_file", ""),
        (_("Go to main file"), "", [None, NODE_PROJECT, NODE_DIR, NODE_FILE, NODE_BAD], "cuda_project_man.goto_main", ""),
        (_("Open main file"), "", [None, NODE_PROJECT, NODE_DIR, NODE_FILE, NODE_BAD], "cuda_project_man.open_main", ""),
        (_("Favorites"), "", [None, NODE_PROJECT, NODE_DIR, NODE_FILE, NODE_BAD], "favorites_submenu", ""),
        ("-"   , "", [None, NODE_PROJECT, NODE_DIR, NODE_FILE, NODE_BAD], "", ""),
        (_("Project sessions"), "", [None, NODE_PROJECT, NODE_DIR, NODE_FILE, NODE_BAD], "sessions_submenu", ""),
        ("-"   , "", [None, NODE_PROJECT, NODE_DIR, NODE_FILE, NODE_BAD], "", ""),
        (_("Project Manager options..."), "", [None, NODE_PROJECT, NODE_DIR, NODE_FILE, NODE_BAD], "cuda_project_man.action_config", ""),
    )

    options = {
        "recent_projects": [],
        "no_files": "",
        "no_dirs": ".git;.svn",
        "no_hidden": True,
        "toolbar": True,
        "preview": True,
        "d_click": False,
        "goto_open": False,
        "sort_order": "ext",
        "always_sync": False,
        "on_start": False,
        "on_start_activate": False,
        "icon_theme": "vscode_16x16",
        "toolbar_theme": "default_16x16",
        "backup_enabled": True,
        "backup_folder": DEFAULT_BACKUP_FOLDER,
        "backup_count": DEFAULT_BACKUP_COUNT,
        "projects_dir": "",
    }
    # Default options used as the base when loading from disk: any key
    # missing from the loaded file is filled in from here, so settings
    # stay fresh after adding/removing options in code.

    tree = None
    tree_index = None
    h_dlg = None
    h_menu = None
    h_menu_cfg = None

    #--- tree filter (the field above the tree) and type-ahead state
    #: current filter text ('' = no filtering)
    tree_filter = ''
    #: control index of the filter edit (set in init_form_main)
    filter_ctl_index = -1
    #: full (shown) width of the filter's (x) button, set in
    #: init_form_main; the hidden state collapses the button to w=0
    #: (see _filter_cancel_btn_update)
    _filter_cancel_w = 0
    #: folds/selection captured when the filtering started, restored
    #: when it is cleared
    _filter_saved_folds = None
    _filter_saved_sel = ''
    #: filter text the debounce timer is waiting for (None = no timer
    #: armed); see panel_filter_on_change
    _filter_pending = None
    #: the background scan of the current filter (a _FilterWorker
    #: thread, or None); polled by _filter_progress_tick
    _filter_worker = None
    #: True while the repeating progress timer is armed
    _filter_progress_on = False
    #: match count of the last FINISHED filtered scan (used to restore
    #: the "finished, N match(es)" line when the debounce fires for an
    #: unchanged filter text)
    _filter_rows_count = 0
    #: the LAZY TREE INDEX of the last finished/cancelled filtered scan:
    #: {parent folder path: [(name, path, is_dir), ...]}, the project
    #: root's children under the key ''. The tree is filled from it
    #: level by level as the user expands the nodes - see
    #: _filter_rebuild / _filter_refill_children
    _filter_index = None
    #: options snapshot of the running/last scan (for the lazy
    #: single-level fallback refill of folders the index does not know)
    _filter_opts = None
    #: the live set of the scan whose (finished or cancelled) result is
    #: in _filter_index: the uniform live filter of _filter_rows_for
    #: keeps needing it after the worker object is dropped
    _filter_live_done = None
    #: {folder path: [(name, path, is_dir)]} cached single-level
    #: fallback scans of folders no scan index knew when the user
    #: expanded them; per-search cache, so the repeating live rebuilds
    #: never re-read the disk
    _filter_fallback = None
    #: folder paths the user unfolded during the current search: a
    #: level which is (still) empty shows no expander, so the rebuild
    #: must remember its unfolded state for the rows which come later
    _filter_expanded = None
    #: worker .version last shown by a live rebuild (lets the progress
    #: tick skip a refresh when the worker found nothing new)
    _filter_live_version = -1
    #: True while _filter_rebuild recreates the filtered tree: blocks
    #: tree_on_unfold, whose refill would fight the rebuild's own
    #: programmatic unfolds
    _filter_rebuilding = False
    #: type-ahead buffer and the time of its last keystroke
    _ta_buf = ''
    _ta_time = 0.0
    #: gap (seconds) after the last typed char which resets the buffer
    TYPEAHEAD_TIMEOUT = 1.2

    action_cut_activate = False

    def __init__(self):
        # state of the drag&drop in the project tree (tree_mouse_down/move/up):
        # _tree_drag_press: (x, y, item_handle) of the mouse-down, or None
        # _tree_drag_mode: None | 'inner' ('inner' = inner drag&drop tracked by
        #   mouse events; only used on OS-es without the OLE drag-out)
        # _tree_drop_target: item handle currently highlighted as drop target
        # _tree_drag_sel: selection to restore when a drag is cancelled
        # _tree_drag_bounds: (w, h) of the tree control, cached at drag start
        # _tree_hover_item/_tree_hover_time: unfold-on-hover state: the node
        #   currently hovered during a drag&drop, and when the hover started
        # _drag_hover_kind: None | 'inner' | 'ole': the tracked drag-hover
        #   session ('inner' = mouse-tracked inner drag&drop; 'ole' = OLE
        #   drag&drop hovering over the panel: CudaText's own drag-out
        #   round-trip, or files dragged in from the OS file manager -
        #   positions come from the form's on_drag_over event)
        # _drag_hover_pos: last known cursor pos of the drag, in tree client
        #   coords (None = not known yet, drag is starting)
        # _drag_hover_self: the tracked OLE drag-hover is CudaText's own
        #   drag-out (the inner drag&drop round-trip)
        # _tree_suppress_click: a drag gesture happened, don't open the file
        #   on the following OnClick (LCL fires OnClick even after a mouse
        #   drag inside a control, and it fires BEFORE OnMouseUp)
        # _tree_press_folded: (item_handle, folded) snapshot of the node hit
        #   by the current left-button press, taken at mouse-down; used by
        #   tree_on_click to detect a click on a node's fold +/- sign: such
        #   a click toggles the node right on mouse-down (LCL does it,
        #   without changing the selection), so the folded state changes
        #   between the press and the OnClick which follows
        # _tree_drag_out_item/_tree_drag_out_path: the node dragged out of the
        #   tree with CudaText's OLE drag ('drag_files'); on Windows the drop
        #   back onto this panel is reported to form_drop_files with
        #   self_drag=True (fired after the OLE drag returned; when the drop
        #   was not accepted by any OLE target, the release position over the
        #   panel is reported the same way), which completes the inner
        #   drag&drop
        self._tree_drag_press = None
        self._tree_drag_mode = None
        self._tree_drop_target = None
        self._tree_drag_sel = None
        self._tree_drag_bounds = None
        self._tree_hover_item = None
        self._tree_hover_time = 0.0
        self._drag_hover_kind = None
        self._drag_hover_pos = None
        self._drag_hover_self = False
        self._tree_suppress_click = False
        self._tree_press_folded = None
        self._tree_drag_out_item = None
        self._tree_drag_out_path = ''
        # _session_names_cache/_session_names_key: (project-file stat,
        #   session-name list) of the last file-read fallback in
        #   session_get_names(); the fast path reads the in-memory mirror
        #   and never touches the file — see the docstring there
        self._session_names_cache = []
        self._session_names_key = None

        settings_dir = Path(app_path(APP_DIR_SETTINGS))
        self.options_filename = settings_dir / "cuda_project_man.json"
        # Rotating backups of the open project file; see projman_backup.
        # Warnings go to the console via print.
        self.project_backups = ProjectFileBackups(log=print)
        # Cause label for the automatic backup report of the session save
        # which is running now ('' = the plugin's commands did NOT
        # initiate it: CudaText's session autosave / session manager --
        # reported as 'by timer'). Set while a method decorated with
        # _with_backup_cause runs; on_exit pins it to 'on exit' so the
        # app's own exit save (which runs right after on_exit) is
        # labelled too.
        self._backup_cause = ''
        # Depth of the _with_backup_cause-decorated methods currently on
        # the stack (0 = no plugin command which saves sessions is
        # running). While a command runs, APPSTATE_SESSION_SAVE_AFTER
        # only records the project files it may have changed
        # (_backup_pending); the backups are made once, when the
        # outermost command ends -- see _with_backup_cause.
        self._backup_flow = 0
        # Project files (os.path.normpath'd) whose session was saved
        # inside the running decorated command; each gets ONE backup
        # (of the final state) when the outermost command ends. A list,
        # not a set: 'Save project as...' into a new path touches the
        # old file first and the new file second, and the backups follow
        # that order.
        self._backup_pending = []
        # Start from a fresh copy of the defaults, then merge the loaded
        # file on top. This ensures newly added options appear even in old
        # config files, while preserving the user's existing values.
        self.options = copy.deepcopy(self.options)
        if self.options_filename.exists():
            try:
                with self.options_filename.open(encoding='utf8') as fin:
                    loaded = json.load(fin)
                self.options.update(loaded)
                if "recent_projects" in self.options:
                    # heal entries merged into one tab-joined line by the
                    # buggy options dialog of older builds (the dialog-API
                    # memo control separates its lines with TAB, and the
                    # old code read the value with splitlines())
                    healed = []
                    for fn in self.options["recent_projects"]:
                        healed.extend(p for p in str(fn).split("\t") if p.strip())
                    self.options["recent_projects"] = [expand_macros('', fn) for fn in healed]
            except Exception:
                pass

        self.new_project() # reset to the "no project" state


    def init_form_main(self):

        show_toolbar = self.options.get("toolbar", True)
        toolbar_theme = self.options.get("toolbar_theme", "default_16x16")

        self.h_dlg = dlg_proc(0, DLG_CREATE)

        dlg_proc(self.h_dlg, DLG_PROP_SET, {
            'keypreview': True,
            'on_key_down': self.form_key_down,
            'on_drop_files': self.form_drop_files,
            'on_drag_over': self.form_drag_over,
            } )

        n = dlg_proc(self.h_dlg, DLG_CTL_ADD, prop='toolbar')
        dlg_proc(self.h_dlg, DLG_CTL_PROP_SET, index=n, prop={
            'name':'bar',
            'a_r':('',']'), #anchor to top: l,r,t
            'vis': show_toolbar,
            'h': 24,
            'autosize': True,
            } )

        self.h_bar = dlg_proc(self.h_dlg, DLG_CTL_HANDLE, index=n)
        self.toolbar_imglist = toolbar_proc(self.h_bar, TOOLBAR_GET_IMAGELIST)
        self.set_imagelist_size(toolbar_theme, self.toolbar_imglist)

        dirname = os.path.join(app_path(APP_DIR_DATA), 'projtoolbaricons', toolbar_theme)
        icon_recents = imagelist_proc(self.toolbar_imglist, IMAGELIST_ADD, value = os.path.join(dirname, 'recents.png'))
        icon_open = imagelist_proc(self.toolbar_imglist, IMAGELIST_ADD, value = os.path.join(dirname, 'open.png'))
        icon_save = imagelist_proc(self.toolbar_imglist, IMAGELIST_ADD, value = os.path.join(dirname, 'save.png'))
        icon_add_file = imagelist_proc(self.toolbar_imglist, IMAGELIST_ADD, value = os.path.join(dirname, 'add-file.png'))
        icon_add_dir = imagelist_proc(self.toolbar_imglist, IMAGELIST_ADD, value = os.path.join(dirname, 'add-dir.png'))
        # icon_del = imagelist_proc(self.toolbar_imglist, IMAGELIST_ADD, value = os.path.join(dirname, 'del.png'))
        icon_cfg = imagelist_proc(self.toolbar_imglist, IMAGELIST_ADD, value = os.path.join(dirname, 'cfg.png'))
        icon_sync = imagelist_proc(self.toolbar_imglist, IMAGELIST_ADD, value = os.path.join(dirname, 'sync.png'))

        toolbar_proc(self.h_bar, TOOLBAR_THEME)
        _toolbar_add_btn(self.h_bar, hint=_('Recent projects'), icon=icon_recents, command='cuda_project_man.menu_recents' )
        _toolbar_add_btn(self.h_bar, hint=_('Open project'), icon=icon_open, command='cuda_project_man.action_open_project' )
        _toolbar_add_btn(self.h_bar, hint=_('Save project'), icon=icon_save, command='cuda_project_man.action_save_project' )
        _toolbar_add_btn(self.h_bar, hint='-' )
        _toolbar_add_btn(self.h_bar, hint=_('Add folder'), icon=icon_add_dir, command='cuda_project_man.action_add_folder' )
        _toolbar_add_btn(self.h_bar, hint=_('Add file'), icon=icon_add_file, command='cuda_project_man.action_add_file' )
        # _toolbar_add_btn(self.h_bar, hint=_('Remove node'), icon=icon_del, command='cuda_project_man.action_remove_node' )
        _toolbar_add_btn(self.h_bar, hint='-' )
        _toolbar_add_btn(self.h_bar, hint=_('Project sessions'), icon=icon_cfg, command='cuda_project_man.menu_cfg')
        _toolbar_add_btn(self.h_bar, hint=_('Show current file in project tree'), icon=icon_sync, command='cuda_project_man.sync_to_ed' )

        btn_count = toolbar_proc(self.h_bar, TOOLBAR_GET_COUNT)
        self.h_btn_sync = toolbar_proc(self.h_bar, TOOLBAR_GET_BUTTON_HANDLE, index=btn_count-1)
        button_proc(self.h_btn_sync, BTN_SET_VISIBLE, not self.options.get('always_sync', False))

        # Favorites (bookmarks) button - a TEXT button showing the ☆
        # UTF-8 font icon itself (no image files); opens the Favorites
        # window. Added after h_btn_sync is captured so its handle stays
        # valid (it assumes it is the last button)
        try:
            _toolbar_add_btn(self.h_bar, hint='-')
            toolbar_proc(self.h_bar, TOOLBAR_ADD_ITEM)
            cnt = toolbar_proc(self.h_bar, TOOLBAR_GET_COUNT)
            h_btn_fav = toolbar_proc(self.h_bar, TOOLBAR_GET_BUTTON_HANDLE, index=cnt-1)
            button_proc(h_btn_fav, BTN_SET_KIND, BTNKIND_TEXT_ONLY)
            button_proc(h_btn_fav, BTN_SET_TEXT, '☆')
            button_proc(h_btn_fav, BTN_SET_HINT, _('Favorites (bookmarks)'))
            button_proc(h_btn_fav, BTN_SET_DATA1, 'cuda_project_man.favorites_show')
        except Exception as e:
            print('Project Manager: cannot add Favorites toolbar button:', e)

        toolbar_proc(self.h_bar, TOOLBAR_SET_WRAP, index=True)
        toolbar_proc(self.h_bar, TOOLBAR_UPDATE)

        #---- filter field above the tree ----
        # typing here shows only the nodes/files/folders whose name
        # matches (folders keep the path to the matches); Esc clears it
        # NOTE 'act': True is REQUIRED: the dialog engine fires the
        # on_change event of an edit only for controls marked 'act'
        # (DoOnChange -> "if not Props.FActive then exit"), and 'edit'
        # controls don't get it by default (buttons do) - without it
        # typing in the field did nothing at all
        EH = app_proc(PROC_GET_GUI_HEIGHT, 'edit') or 28
        #---- cancel (x) button at the right of the filter field ----
        # Created BEFORE the edit: the edit's right edge is anchored to
        # this button's left edge. Clicking it cancels a RUNNING search
        # (the tree keeps its current content, the status line reports
        # what was found so far); when no search is running it clears
        # the filter, like the x of a usual search box. button_ex = the
        # flat themed button, so it follows the UI theme.
        # VISIBILITY: the button shows ONLY while it can do something -
        # the filter field has text, or a background search is running
        # (_filter_cancel_btn_update, called on every filter change).
        # Hidden, it is also COLLAPSED to zero width, so the filter
        # field stretches over the full row: a hidden control keeps
        # its bounds, so only hiding it would leave an empty gap at
        # the right of the field.
        # ANCHORS: 'a_l': None is REQUIRED. The dialog engine maps a_l/
        # a_r onto the LCL anchors, and the LCL default is akLeft=on:
        # with the left anchor kept, the left edge stayed pinned to the
        # initial x=0 while the right edge followed the panel edge - the
        # button stretched across the WHOLE filter row and covered the
        # edit (nothing could be typed). With akLeft removed, the button
        # keeps its width (EH) and only MOVES with the panel's right
        # edge - and setting 'w': 0 (the hidden state) moves its left
        # edge to the panel's right edge, which is what lets the edit
        # grow.
        # The button's TOP+BOTTOM are re-anchored to the edit AFTER the
        # edit is created (below): the engine resolves the control name
        # of an anchor only if that control already exists, so the
        # vertical glue cannot be set here yet. With both vertical edges
        # glued to the edit (0 spacing), the button is EXACTLY as tall
        # as the filter box at every DPI / GUI height - the fixed h=EH
        # below is only the initial pre-anchor size (without the glue
        # the button rendered SMALLER than the filter field).
        n = dlg_proc(self.h_dlg, DLG_CTL_ADD, prop='button_ex')
        dlg_proc(self.h_dlg, DLG_CTL_PROP_SET, index=n, prop={
            'name': 'btn_fcancel',
            'x': 300, 'y': 2,      # sane pre-anchor position; the right
                                   # anchor places the button finally
            'a_t': ('bar', ']'),
            'sp_t': 2,
            'a_l': None,
            'a_r': ('', ']'),
            'sp_r': 2,
            'w': EH,
            'h': EH,
            'cap': '×',
            'hint': _('Cancel the running search\r(or clear the filter when no search is running)'),
            'on_change': self.panel_filter_cancel_click,
        })
        n = dlg_proc(self.h_dlg, DLG_CTL_ADD, prop='edit')
        self.filter_ctl_index = n
        dlg_proc(self.h_dlg, DLG_CTL_PROP_SET, index=n, prop={
            'name': 'f_filter',
            'a_t': ('bar', ']'),
            'sp_t': 2,
            'a_l': ('', '['),
            'sp_l': 2,
            'a_r': ('btn_fcancel', '['),
            'sp_r': 2,
            'h': EH,
            'act': True,
            'texthint': _('Filter the project tree...'),
            'hint': _('Show only the files/folders whose name matches.\rEsc clears the filter'),
            'on_change': self.panel_filter_on_change,
        })
        # 'f_filter' exists now: glue the cancel button's top and bottom
        # edges to the edit's ones (0 spacing) - the button always keeps
        # the same height as the filter field, at any DPI / GUI height
        dlg_proc(self.h_dlg, DLG_CTL_PROP_SET, name='btn_fcancel', prop={
            'a_t': ('f_filter', '['),
            'sp_t': 0,
            'a_b': ('f_filter', ']'),
            'sp_b': 0,
        })
        # the (x) button starts HIDDEN and collapsed: with no filter
        # text and no running search there is nothing it could do
        # (every later filter change calls _filter_cancel_btn_update)
        self._filter_cancel_w = EH
        self._filter_cancel_btn_update()

        #---- filter status line (between the filter and the tree) ----
        # While the filter field is not empty, it ALWAYS shows what the
        # background search is doing: "searching... N match(es) so far"
        # while the scan runs, "finished, N match(es) found" when it
        # ended. Empty caption = no filtering.
        n = dlg_proc(self.h_dlg, DLG_CTL_ADD, prop='label')
        dlg_proc(self.h_dlg, DLG_CTL_PROP_SET, index=n, prop={
            'name': 'lbl_fstat',
            'a_t': ('f_filter', ']'),
            'sp_t': 2,
            'a_l': ('', '['),
            'sp_l': 4,
            'a_r': ('', ']'),
            'sp_r': 4,
            'x': 4,
            'y': EH + 4 + 2,
            'w': 400,
            'h': 17,
            'cap': '',
        })

        n = dlg_proc(self.h_dlg, DLG_CTL_ADD, prop='treeview')
        self.tree_index = n
        dlg_proc(self.h_dlg, DLG_CTL_PROP_SET, index=n, prop={
            'name':'tree',
            'a_t':('lbl_fstat', ']'),
            'sp_t': 2,
            'a_r':('',']'), #anchor to entire form
            'a_b':('',']'),
            'on_menu': 'cuda_project_man.tree_on_menu',
            'on_unfold': 'cuda_project_man.tree_on_unfold',
            'on_click': 'cuda_project_man.tree_on_click',
            'on_click_dbl': 'cuda_project_man.tree_on_click_dbl',
            'on_mouse_down': self.tree_mouse_down,
            'on_mouse_move': self.tree_mouse_move,
            'on_mouse_up': self.tree_mouse_up,
            } )

        self.tree = dlg_proc(self.h_dlg, DLG_CTL_HANDLE, index=n)
        self.tree_imglist = tree_proc(self.tree, TREE_GET_IMAGELIST)
        tree_proc(self.tree, TREE_PROP_SHOW_ROOT, text='0')
        tree_proc(self.tree, TREE_ITEM_DELETE, 0)

        self.icon_init()
        self.ICON_ALL = self.icon_get('_')
        self.ICON_DIR = self.icon_get('_dir')
        self.ICON_PROJ = self.icon_get('_proj')
        self.ICON_BAD = self.icon_get('_bad')
        self.ICON_ZIP = self.icon_get('_zip')
        self.ICON_BIN = self.icon_get('_bin')
        self.ICON_IMG = self.icon_get('_img')
        self.ICON_TEXT = self.icon_get('_text')
        self.ICON_AUDIO = self.icon_get('_audio')
        self.ICON_VIDEO = self.icon_get('_video')

        # color the form and the filter field with the current UI theme
        # (the tree and the toolbar are themed by the engine itself)
        self.apply_panel_theme()

    def apply_panel_theme(self):
        """Re-color the panel background, the filter field and the
        filter status line to the current UI theme (the treeview and
        the toolbar are re-themed by CudaText itself - TREE_THEME /
        TOOLBAR_THEME and the app's theme-change pass over the docked
        plugin dialogs; the form, the edit and the label are ours to
        color). Also called when the UI theme is switched while
        CudaText runs (on_state, APPSTATE_THEME_UI)."""
        if not self.h_dlg:
            return
        colors = _favorites.get_theme_ui_colors()
        try:
            if colors['form_bg'] is not None:
                dlg_proc(self.h_dlg, DLG_PROP_SET, prop={
                    'color': colors['form_bg']})
            if colors['edit_bg'] is not None:
                dlg_proc(self.h_dlg, DLG_CTL_PROP_SET, name='f_filter',
                         prop={'color': colors['edit_bg']})
            if colors['edit_font'] is not None:
                dlg_proc(self.h_dlg, DLG_CTL_PROP_SET, name='f_filter',
                         prop={'font_color': colors['edit_font']})
            lbl = {}
            if colors['form_bg'] is not None:
                lbl['color'] = colors['form_bg']
            if colors['label_font'] is not None:
                lbl['font_color'] = colors['label_font']
            if lbl:
                dlg_proc(self.h_dlg, DLG_CTL_PROP_SET, name='lbl_fstat',
                         prop=lbl)
        except Exception:
            pass


    def init_panel(self, and_activate=True):
        # already inited?
        if self.tree:
            return

        self.init_form_main()

        #dlg_proc(self.h_dlg, DLG_SCALE)
        tree_proc(self.tree, TREE_THEME) #TREE_THEME only after DLG_SCALE

        app_proc(PROC_SIDEPANEL_ADD_DIALOG, (self.title, self.h_dlg, 'project.png'))

        if and_activate:
            self.do_show()

        self.action_refresh()
        self.generate_context_menu()


    def show_panel(self):
        self.do_show()

    def focus_panel(self):
        self.do_show()
        app_proc(PROC_SIDEPANEL_ACTIVATE, (self.title, True))

    def do_show(self):
        if not self.tree:
            self.init_panel(True)
        else:
            ed.cmd(cudatext_cmd.cmd_ShowSidePanelAsIs)
            app_proc(PROC_SIDEPANEL_ACTIVATE, self.title)

    def tree_mouse_down(self, id_dlg, id_ctl, data='', info=''):
        # any stale suppress-flag is for a click which already happened
        self._tree_suppress_click = False
        # a new mouse gesture cancels any stale drag state (eg an inner drag
        # of the previous gesture which never saw its mouse-up)
        self._tree_drag_mode = None
        self._tree_drop_target = None
        self._drag_hover_end()
        self._tree_drag_out_item = None
        self._tree_drag_out_path = ''
        self._tree_press_folded = None
        if isinstance(data, dict) and data.get('btn', -1) == 0:
            x = int(data.get('x', 0))
            y = int(data.get('y', 0))
            item = tree_proc(self.tree, TREE_FIND_BY_COORD, 0, -1,
                             '{},{}'.format(x, y))
            self._tree_drag_press = (x, y, item)
            # remember the hit node's folded state: when it changes before
            # OnClick fires, the press landed on the node's fold +/- sign
            # (LCL toggles the node on mouse-down, without selecting it) and
            # tree_on_click must not toggle that node a second time
            if item:
                props = tree_proc(self.tree, TREE_ITEM_GET_PROPS, item)
                if props:
                    self._tree_press_folded = (item, props.get('folded'))
        else:
            self._tree_drag_press = None

    def tree_mouse_move(self, id_dlg, id_ctl, data='', info=''):
        press = self._tree_drag_press
        if press is None or not isinstance(data, dict):
            return
        x = int(data.get('x', 0))
        y = int(data.get('y', 0))

        if self._tree_drag_mode is None:
            if max(abs(x-press[0]), abs(y-press[1])) <= self.TREE_DRAG_THRESHOLD:
                return
            item = press[2]
            if not item or not self._is_draggable(item):
                return
            if IS_WIN:
                # the drag starts immediately, exactly like the drag of files
                # out of CudaText always worked: CudaText runs an OLE
                # CF_HDROP drag, so the node can be dropped into Explorer
                # and other apps. Dropping it back onto this panel is
                # reported to form_drop_files (self_drag=True) and is
                # handled as the inner drag&drop (see _do_panel_self_drop).
                self._begin_drag_out(item)
            else:
                # OS-es without the OLE drag-out: track the inner drag&drop
                # by mouse events while the cursor stays over the tree
                self._tree_drag_mode = 'inner'
                self._tree_drop_target = None
                self._tree_drag_sel = self.selected
                self._tree_suppress_click = True
                self._drag_hover_begin('inner')
                self._tree_drag_update(x, y)
            return

        if self._tree_drag_mode == 'inner':
            if not self._point_in_tree(x, y):
                # cursor left the tree: cancel the inner drag&drop
                self._restore_drag_selection()
                self._tree_drag_press = None
                self._tree_drag_mode = None
                self._tree_drop_target = None
                self._drag_hover_end()
                return
            self._tree_drag_update(x, y)

    def tree_mouse_up(self, id_dlg, id_ctl, data='', info=''):
        press = self._tree_drag_press
        mode = self._tree_drag_mode
        target = self._tree_drop_target
        saved_sel = self._tree_drag_sel
        self._tree_drag_press = None
        self._tree_drag_mode = None
        self._tree_drop_target = None
        self._tree_drag_bounds = None
        self._drag_hover_end()
        self._tree_drag_sel = None
        self._tree_press_folded = None

        if mode != 'inner':
            return

        # a drag gesture ended with the button still held by our code:
        # the OnClick which LCL fires for this mouse-up must not open files
        self._tree_suppress_click = True

        if press and target:
            self._do_tree_drop(press[2], target)
        else:
            # dropped on nothing allowed: when the drop was on some node,
            # explain why it is not allowed; restore the selection
            if press and isinstance(data, dict):
                x = int(data.get('x', 0))
                y = int(data.get('y', 0))
                hit = tree_proc(self.tree, TREE_FIND_BY_COORD, 0, -1,
                                '{},{}'.format(x, y))
                if hit and hit != press[2]:
                    self._msg_drop_not_allowed(press[2], hit)
            self._select_item_if_alive(saved_sel)

    #---------------------------------------------------------------------
    # drag&drop helpers (all run in the context of the tree's mouse events)

    def _point_in_tree(self, x, y):
        """Is the point (tree control client coords) inside the tree?"""
        if self._tree_drag_bounds is None:
            w, h = 0, 0
            props = dlg_proc(self.h_dlg, DLG_CTL_PROP_GET, name='tree')
            if props:
                w = int(props.get('w', 0))
                h = int(props.get('h', 0))
            self._tree_drag_bounds = (w, h)
        w, h = self._tree_drag_bounds
        return 0 <= x < w and 0 <= y < h

    def _begin_drag_out(self, item):
        """Start dragging the item's file/folder out to other apps (Windows:
        OLE CF_HDROP drag, implemented by CudaText's 'drag_files'). The OLE
        drag owns the mouse from now on. When the node is dropped back onto
        this panel, CudaText reports it to form_drop_files with
        self_drag=True (after the OLE drag returned; when no OLE target
        accepted the drop, the release position over the panel is reported
        instead), and _do_panel_self_drop() completes the inner drag&drop
        (reorder / move, with confirmation dialogs)."""
        self._tree_drag_press = None
        self._tree_drag_mode = None
        self._tree_suppress_click = True
        self._tree_drag_out_item = item
        path = str(self.get_location_by_index(item)) if item else ''
        self._tree_drag_out_path = path
        if not item or self.tree_index is None:
            return
        if path and os.path.exists(path):
            dlg_proc(self.h_dlg, DLG_CTL_PROP_SET, index=self.tree_index,
                     prop={'drag_files': path})
            # While the OLE drag runs, the tree gets no mouse events: the
            # hover positions for the unfold-on-hover feature come from the
            # form's on_drag_over event (fired on DragEnter/DragOver of the
            # panel's OLE drop target - for this own drag-out, and for files
            # dragged in from the OS file manager alike)
            self._drag_hover_begin('ole')

    def _restore_drag_selection(self):
        """Restore the tree selection saved at drag start (drag cancelled)."""
        sel = self._tree_drag_sel
        self._tree_drag_sel = None
        self._select_item_if_alive(sel)

    def _select_item_if_alive(self, item):
        """Select the item when its handle is still valid (the tree may
        have been rebuilt in the meantime)."""
        if item and tree_proc(self.tree, TREE_ITEM_GET_PROPS, item):
            tree_proc(self.tree, TREE_ITEM_SELECT, item)

    def _is_draggable(self, item):
        """Not the project root node, not a 'dummy' node: has a data-path."""
        props = tree_proc(self.tree, TREE_ITEM_GET_PROPS, item)
        return bool(props and props.get('data', ''))

    def _tree_drag_update(self, x, y):
        """Inner drag: highlight the drop target under the cursor."""
        # feed the unfold-on-hover state machine; its timer checks the
        # hover timeout, so the cursor may also be held still over a folder
        self._drag_hover_move(x, y)

        item = tree_proc(self.tree, TREE_FIND_BY_COORD, 0, -1,
                         '{},{}'.format(x, y))
        press = self._tree_drag_press
        src_item = press[2] if press else None
        target = item if item and src_item and \
            self._drop_kind(src_item, item) else None
        if target == self._tree_drop_target:
            return

        self._tree_drop_target = target
        if target:
            tree_proc(self.tree, TREE_ITEM_SELECT, target)
        else:
            # keep the dragged node selected, like OS file managers do
            sel = src_item if src_item else self._tree_drag_sel
            if sel:
                tree_proc(self.tree, TREE_ITEM_SELECT, sel)

    #---------------------------------------------------------------------
    # unfold-on-hover during a drag&drop (any drag which hovers the tree:
    # the mouse-tracked inner drag&drop, the Windows OLE round-trip of our
    # own drag-out, and files dragged in from the OS file manager)

    def _drag_hover_begin(self, kind):
        """Begin tracking a drag-hover session. kind: 'inner' (the
        mouse-tracked inner drag&drop) or 'ole' (an OLE drag&drop hovering
        over the panel; positions come from the form's on_drag_over event).
        Starts the timer which runs the hover timeout checks: events alone
        never fire while the cursor is held still over a folder."""
        self._drag_hover_kind = kind
        self._drag_hover_pos = None
        self._drag_hover_self = False
        self._tree_hover_item = None
        self._tree_hover_time = time.time()
        timer_proc(TIMER_START, DRAG_HOVER_TIMER_CALLBACK,
                   self.DRAG_HOVER_TIMER_MS, tag='projman_drag_hover')

    def _drag_hover_end(self):
        """End the tracked drag-hover session (drag ended, was dropped,
        cancelled or left the panel)."""
        if self._drag_hover_kind is None:
            return
        self._drag_hover_kind = None
        self._drag_hover_pos = None
        self._drag_hover_self = False
        self._tree_hover_item = None
        self._tree_hover_time = 0.0
        timer_proc(TIMER_STOP, DRAG_HOVER_TIMER_CALLBACK, 0,
                   tag='projman_drag_hover')

    def _drag_hover_move(self, x, y):
        """The tracked drag is at (x, y), in tree client coords (from
        tree_mouse_move, or from the form's on_drag_over event)."""
        if self._drag_hover_kind is None:
            # positions without a session we started: an OLE drag&drop from
            # the OS file manager hovering over the panel - begin one
            self._drag_hover_begin('ole')
        self._drag_hover_pos = (x, y)

    def _drag_hover_tick(self, tag='', info=''):
        """Timer tick of a drag-hover session: runs the unfold-on-hover
        state machine, and for the Windows inner drag&drop (OLE round-trip
        of our own drag-out) highlights the drop target under the cursor,
        like the mouse-tracked inner drag&drop does on other OS-es."""
        kind = self._drag_hover_kind
        if kind is None:
            return
        if not self.tree:
            self._drag_hover_end()
            return

        if kind == 'ole':
            # OLE drags deliver no mouse events, so a missed leave/drop
            # notification cannot be detected by events: no pressed mouse
            # button anymore = the drag is over (belt and braces, DragLeave
            # and on_drop_files normally end the session; this also ends
            # sessions of drags which never hovered the panel at all)
            state = app_proc(PROC_GET_KEYSTATE, '')
            if 'L' not in state and 'R' not in state:
                self._drag_hover_end()
                return

        pos = self._drag_hover_pos
        if pos is None:
            return # drag not started yet (the OLE drag is still queued)

        now = time.time()
        item = tree_proc(self.tree, TREE_FIND_BY_COORD, 0, -1,
                         '{},{}'.format(pos[0], pos[1]))
        item = item if self._hover_item_allowed(item) else None

        # Windows inner drag&drop: highlight the drop target, like
        # _tree_drag_update does for the mouse-tracked inner drag&drop
        if kind == 'ole' and self._drag_hover_self and \
          self._tree_drag_out_item:
            sel = item if item else self._tree_drag_out_item
            if tree_proc(self.tree, TREE_ITEM_GET_PROPS, sel) and \
              tree_proc(self.tree, TREE_ITEM_GET_SELECTED) != sel:
                tree_proc(self.tree, TREE_ITEM_SELECT, sel)

        if item != self._tree_hover_item:
            self._tree_hover_item = item
            self._tree_hover_time = now
            return
        if item is None:
            return
        if now - self._tree_hover_time < self.TREE_UNFOLD_HOVER_SEC:
            return

        props = tree_proc(self.tree, TREE_ITEM_GET_PROPS, item)
        if props and props.get('folded') and props.get('sub_items'):
            tree_proc(self.tree, TREE_ITEM_UNFOLD, item)
            # don't unfold it again while still hovering it
            self._tree_hover_time = now + 3600.0

    def _hover_item_allowed(self, item):
        """Is the hovered node a candidate for the unfold-on-hover?
        - inner drag&drop (mouse-tracked, or the OLE round-trip of our own
          drag-out): only nodes which are a valid drop target for the
          dragged node (same rule as the drop-target highlighting);
        - files dragged in from the OS file manager: any node (such a drop
          adds them to the project; the unfold is a navigation preview,
          like in OS file managers)."""
        if not item:
            return False
        src_item = None
        if self._drag_hover_kind == 'inner':
            press = self._tree_drag_press
            src_item = press[2] if press else None
        elif self._drag_hover_self:
            src_item = self._tree_drag_out_item
        if src_item is None:
            return True
        if not tree_proc(self.tree, TREE_ITEM_GET_PROPS, src_item):
            return True # stale handle: the dragged node left the tree
        return bool(self._drop_kind(src_item, item))

    def _drop_kind(self, src_item, tgt_item):
        """What does dropping the node src_item onto the node tgt_item mean?
        Returns 'reorder', 'reorder_end' or 'move', or None (not allowed).
        Rules:
        - a project's root node (level 1) can only change its order among the
          other root nodes; it is never moved inside another node;
        - a file/folder inside a root folder node (level 2+) can be moved
          into another folder node (moved on disk, like Cut+Paste)."""
        src = tree_proc(self.tree, TREE_ITEM_GET_PROPS, src_item) \
            if src_item else None
        dst = tree_proc(self.tree, TREE_ITEM_GET_PROPS, tgt_item)
        if not src or not dst:
            return None
        src_lvl = src.get('level', 0)
        dst_lvl = dst.get('level', 0)
        src_path = src.get('data', '')
        dst_path = dst.get('data', '')

        if not src_path or src_lvl < 1:
            return None

        if src_lvl == 1:
            # a project's root node: reorder only, never inside another node;
            # with a single root node the order cannot change at all, so no
            # drag&drop of it (no confirmation, only a statusbar message)
            if len(self.project.get('nodes', [])) < 2:
                return None
            if dst_lvl == 1:
                if self._same_path(dst_path, src_path):
                    return None
                return 'reorder'
            if dst_lvl == 0:
                # onto the project header: reorder to the end of the list
                return 'reorder_end'
            return None

        # a file/folder inside some root folder node: can be moved
        # into another folder node (never onto the project header)
        if dst_lvl == 0:
            return None
        if dst_path and os.path.isdir(dst_path) and not is_locked(dst_path):
            if self._same_path(dst_path, src_path):
                return None
            # not into a subfolder of itself (only matters for folders)
            if os.path.isdir(src_path) and \
              self._path_is_inside(dst_path, src_path):
                return None
            # not onto its own parent folder: nothing would change
            if self._same_path(os.path.dirname(src_path), dst_path):
                return None
            return 'move'
        return None

    @staticmethod
    def _same_path(p1, p2):
        if not p1 or not p2:
            return False
        return os.path.normcase(os.path.normpath(p1)) == \
               os.path.normcase(os.path.normpath(p2))

    @staticmethod
    def _path_is_inside(path, folder):
        return os.path.normcase(os.path.normpath(path)).startswith(
            os.path.normcase(os.path.normpath(folder)) + os.sep)

    def _do_tree_drop(self, src_item, tgt_item):
        """Perform the drop of src_item onto tgt_item (inner drag&drop).
        Asks the user for confirmation:
        - changing the order of a node (a root node dropped onto another
          root node / onto the project header);
        - moving a file/folder from its folder node into another folder node.
        A reorder also needs sorting to be off, else the manual order is
        overwritten by sorting: the user is asked to disable sorting."""
        kind = self._drop_kind(src_item, tgt_item)
        if kind is None:
            self._msg_drop_not_allowed(src_item, tgt_item)
            self._restore_drag_selection()
            return

        src_props = tree_proc(self.tree, TREE_ITEM_GET_PROPS, src_item)
        tgt_props = tree_proc(self.tree, TREE_ITEM_GET_PROPS, tgt_item)
        src_path = src_props.get('data', '')
        tgt_path = tgt_props.get('data', '')

        if kind in ('reorder', 'reorder_end'):
            if not self._confirm_disable_sorting_for_dnd():
                # user declined to disable sorting: cancel the drag&drop,
                # with sorting on the manual order is overwritten anyway
                self._restore_drag_selection()
                return
            name = os.path.basename(src_path.rstrip('/\\'))
            res = msg_box_ex(
                _('Project Manager'),
                _("Change the order of the node '{}' in the project?").format(name),
                [_('OK'), _('Cancel')],
                MB_ICONQUESTION)
            if res != 0:
                self._restore_drag_selection()
                return
            if kind == 'reorder':
                self._reorder_root_node(src_path, tgt_path)
            else:
                self._reorder_root_node(src_path, None)

        elif kind == 'move':
            src_parent = os.path.dirname(src_path.rstrip('/\\'))
            res = msg_box_ex(
                _('Project Manager'),
                _("Move '{}' from folder '{}' to folder '{}'?").format(
                    os.path.basename(src_path.rstrip('/\\')),
                    os.path.basename(src_parent.rstrip('/\\')) or src_parent,
                    os.path.basename(tgt_path.rstrip('/\\')) or tgt_path),
                [_('OK'), _('Cancel')],
                MB_ICONQUESTION)
            if res != 0:
                self._restore_drag_selection()
                return
            self._move_node_on_disk(src_path, tgt_path)

    def _confirm_disable_sorting_for_dnd(self):
        """A drag&drop which changes the order of nodes needs the option
        "Sorting order" to be empty ('none'), else the manual order is
        overwritten by sorting on the next refresh. Ask the user to disable
        sorting. Returns True when the reorder may continue (sorting was
        already off, or the user disabled it now)."""
        if not self.options.get('sort_order', 'ext'):
            return True
        res = msg_box_ex(
            _('Project Manager'),
            _('Drag & drop changes the order of project nodes, but the tree is sorted now: the new order would be overwritten by sorting.') + '\n\n' +
            _('Disable sorting (set "Sorting order" to "none")?'),
            [_('OK'), _('Cancel')],
            MB_ICONQUESTION)
        if res != 0:
            return False
        self.options['sort_order'] = ''
        self.save_options()
        self.action_refresh()
        msg_status(_('Sorting disabled (drag & drop can change the order of nodes now)'))
        return True

    def _msg_drop_not_allowed(self, src_item, tgt_item):
        """Show a statusbar message explaining why the drop of src_item onto
        tgt_item is not allowed."""
        src = tree_proc(self.tree, TREE_ITEM_GET_PROPS, src_item) \
            if src_item else None
        dst = tree_proc(self.tree, TREE_ITEM_GET_PROPS, tgt_item) \
            if tgt_item else None
        if not src or not dst:
            return
        src_lvl = src.get('level', 0)
        dst_lvl = dst.get('level', 0)
        src_path = src.get('data', '')
        dst_path = dst.get('data', '')

        if src_lvl == 1 and len(self.project.get('nodes', [])) < 2:
            # a single root node has no order to change ,so no drag&drop for it, no confirmation dialog
            msg_status(_("Cannot drag&drop single root node"))
        elif src_lvl == 1 and dst_lvl >= 2:
            msg_status(_("Project nodes can only change their order, they cannot be moved inside other nodes"))
        elif src_lvl >= 2 and dst_lvl == 0:
            msg_status(_("Cannot drop onto the project header: drop onto a folder node to move the file/folder"))
        elif src_lvl >= 2 and dst_path and os.path.isdir(dst_path) and \
                self._same_path(os.path.dirname(src_path), dst_path):
            msg_status(_("File/folder is already in this folder"))
        elif src_lvl >= 2 and dst_path and not os.path.isdir(dst_path):
            msg_status(_("Cannot drop onto a file: drop onto a folder node to move the file/folder"))
        else:
            msg_status(_("Drop not allowed here"))

    @_with_backup_cause('on project save')
    def _reorder_root_node(self, src_path, before_path):
        """Move the root project entry src_path before the root entry
        before_path (or to the end of the list when before_path is None)."""
        nodes = self.project.get('nodes', [])
        if src_path not in nodes:
            return
        nodes.remove(src_path)
        if before_path and before_path in nodes:
            nodes.insert(nodes.index(before_path), src_path)
        else:
            nodes.append(src_path)
        # no APPSTATE_PROJECT_NODE_ADD/DEL here: the same nodes, only
        # their order changed -- 'added/removed' would be a lie; the
        # save below still reports the change through the backup cause
        self.action_refresh()
        if self.project_file_path:
            self.action_save_project_as(self.project_file_path)
        msg_status(_('Project node moved'))

    @_with_backup_cause('on project save')
    def _move_node_on_disk(self, src_path, tgt_folder):
        """Move the file/folder src_path into tgt_folder, like the plugin's
        Cut+Paste does. Updates the project when the moved path was a root
        node or the main file."""
        import shutil
        dst_path = os.path.join(tgt_folder, os.path.basename(src_path))
        if os.path.exists(dst_path):
            msg_status(_('Cannot move, already exists: ') + collapse_filename(dst_path))
            return
        try:
            shutil.move(src_path, dst_path)
        except OSError as ex:
            msg_status(_('Cannot move: ') + str(ex))
            return

        # a root entry which was moved (a root folder dragged deeper is not
        # allowed, but be safe): point the project to the new location
        if src_path in self.project['nodes']:
            self.project['nodes'][self.project['nodes'].index(src_path)] = dst_path
            # the node list changed: old path removed, new path added
            self._fire_project_nodes_removed()
            self._fire_project_nodes_added()
        if self.project.get('mainfile', '') == src_path:
            self.project['mainfile'] = dst_path
            self.update_global_data()

        if self.project_file_path:
            self.action_save_project_as(self.project_file_path)
        self.action_refresh()
        msg_status(_('Moved to: ') + collapse_filename(dst_path))

    #---------------------------------------------------------------------
    # drop of files onto the panel

    def form_drag_over(self, id_dlg, id_ctl, data='', info=''):
        """A file drag&drop hovers over the Project panel (Windows, OLE
        drag&drop): either files dragged in from the OS file manager, or the
        plugin's own drag-out (the inner drag&drop round-trip) hovering back
        over the panel. Drives the unfold-on-hover feature (see
        _drag_hover_tick): the position is only stored here - this handler
        runs inside the OLE drag loop (IDropTarget::DragEnter/DragOver/
        DragLeave), so it must be fast and must not show any UI.

        `data` is a dict: { 'x': int, 'y': int, 'self_drag': bool }
        ('x'/'y' are the cursor position in the panel form's client coords,
        both -1 when the drag left the panel; 'self_drag' is True for
        CudaText's own file drag-out)."""
        if not self.tree:
            return
        if not isinstance(data, dict):
            return
        try:
            x = int(data.get('x', -1))
            y = int(data.get('y', -1))
        except (TypeError, ValueError):
            return
        self._drag_hover_self = bool(data.get('self_drag', False))

        if x < 0 or y < 0:
            # the drag left the panel form (OLE DragLeave)
            self._drag_hover_end()
            return
        pos = self._form_to_tree_coord(x, y)
        if pos is None:
            return
        self._drag_hover_move(pos[0], pos[1])

    def _form_to_tree_coord(self, x, y):
        """Panel form client coords -> tree control client coords (live
        position of the tree control inside the panel form), or None when
        the tree control cannot be found."""
        props = dlg_proc(self.h_dlg, DLG_CTL_PROP_GET, name='tree')
        if not props:
            return None
        return (x - int(props.get('x', 0)), y - int(props.get('y', 0)))

    def form_drop_files(self, id_dlg, id_ctl, data='', info=''):
        """Files/folders dropped onto the Project panel.

        - Files/folders dropped from the OS file manager are added to the
          current project (CudaText opens files dropped anywhere else in
          the editor, as before).
        - On Windows, a node dragged out of the tree (OLE 'drag_files')
          and dropped back onto the panel is the inner drag&drop: reorder
          the node / move a file/folder into a folder node, with
          confirmation dialogs. CudaText reports it with self_drag=True;
          the event is fired after the OLE drag returned (deferred by
          proc_customdialog_dummy: never from inside IDropTarget::Drop),
          and when the drop was not accepted by any OLE drop target, the
          release position over the panel is reported the same way
          (fallback), so the inner drag&drop cannot get lost. The drop
          position is in the panel form's client coords.

        `data` is a dict: { 'files': str (paths separated by chr(1)),
        'x': int, 'y': int, 'self_drag': bool } ('x'/'y' are -1 when the
        position is unknown, eg drops on non-Windows OS-es)."""
        if not self.tree:
            return
        # any drop onto the panel ends the tracked drag-hover session
        self._drag_hover_end()

        if isinstance(data, dict):
            files_str = str(data.get('files', ''))
            self_drag = bool(data.get('self_drag', False))
            try:
                x = int(data.get('x', -1))
                y = int(data.get('y', -1))
            except (TypeError, ValueError):
                x, y = -1, -1
        else:
            files_str = str(data or '')
            self_drag = False
            x, y = -1, -1

        if not files_str:
            # loud: a drop event without files is a bug, don't ignore it
            msg_status(_('Project Manager: drop event without files'))
            return

        if self_drag:
            # round-trip of our own drag-out: inner drag&drop
            # (safety: the dropped path must be the dragged node's path)
            path = self._tree_drag_out_path
            dropped = files_str.split(chr(1))
            if path and not any(self._same_path(path, fn)
                                for fn in dropped):
                # must not happen (the event carries the 'drag_files' list
                # of our own drag); loud, so it can never be silent
                self._tree_drag_out_item = None
                self._tree_drag_out_path = ''
                msg_status(_('Project Manager: inner drop ignored, dropped paths do not match the dragged node: ') +
                           files_str.replace(chr(1), ', '))
                return
            self._do_panel_self_drop(x, y)
            return

        added = []
        skipped = []
        for fn in files_str.split(chr(1)):
            fn = fn.strip().strip('"').strip("'")
            if not fn:
                continue
            if not (os.path.isfile(fn) or os.path.isdir(fn)):
                skipped.append(fn + ' (' + _('not found') + ')')
                continue
            if is_locked(fn):
                skipped.append(fn + ' (' + _('hidden/locked') + ')')
                continue
            self.add_node(fn)
            added.append(fn)

        if added:
            msg_status(_('Added to project') + ': ' + str(len(added)))
        else:
            # loud and diagnostic: show what was received, so a broken
            # drop can never be silent again
            received = files_str.replace(chr(1), ', ')
            detail = ('; ' + '; '.join(skipped)) if skipped else ''
            msg_status(_('Cannot add dropped paths to the project') + ': ' +
                       received + detail)

    def _do_panel_self_drop(self, x, y):
        """A node dragged out of the tree was dropped back onto this panel
        (Windows: the OLE drag of 'drag_files' ended on the panel; it is
        reported by the OLE drop target, or by the fallback in
        ExecuteAppFileDrag when the drop was not accepted by any OLE
        target). (x, y) are the drop position in the panel form's client
        coords: convert them to the tree's coords and perform the inner
        drop on the node under the cursor."""
        src_item = self._tree_drag_out_item
        self._tree_drag_out_item = None
        self._tree_drag_out_path = ''

        if not src_item or not tree_proc(self.tree, TREE_ITEM_GET_PROPS,
                                         src_item):
            msg_status(_('The dragged node is not in the tree anymore'))
            return
        if x < 0 or y < 0:
            msg_status(_('Drop position unknown'))
            return

        # panel form client coords -> tree client coords (live position
        # of the tree control inside the panel form)
        pos = self._form_to_tree_coord(x, y)
        if pos is None:
            msg_status(_('Cannot find the tree control'))
            return

        tgt_item = tree_proc(self.tree, TREE_FIND_BY_COORD, 0, -1,
                             '{},{}'.format(pos[0], pos[1]))
        if not tgt_item:
            msg_status(_('Drop the node onto another node in the project tree'))
            return
        if tgt_item == src_item:
            return # dropped on itself: nothing to do
        self._do_tree_drop(src_item, tgt_item)

    @property
    def selected(self):
        # panel may be not created yet (plugin was lazy-loaded by a menu
        # command, side panel never shown): behave as "nothing selected"
        if not self.tree:
            return None
        return tree_proc(self.tree, TREE_ITEM_GET_SELECTED)

    def add_context_menu_node(self, parent, action, name):
        return menu_proc(parent, MENU_ADD, command=action, caption=name)


    def generate_context_menu(self):
        node_type = None
        if self.selected is not None:
            n = self.get_info(self.selected).image
            if n == self.ICON_PROJ: node_type = NODE_PROJECT
            elif n == self.ICON_DIR: node_type = NODE_DIR
            elif n == self.ICON_BAD: node_type = NODE_BAD
            else: node_type = NODE_FILE

        if not self.h_menu:
            self.h_menu = menu_proc(0, MENU_CREATE)

        menu_all = self.h_menu
        menu_proc(menu_all, MENU_CLEAR)
        menu_proj = self.add_context_menu_node(menu_all, "0", _("Project file"))
        menu_nodes = self.add_context_menu_node(menu_all, "0", _("Root nodes"))
        if node_type == NODE_FILE:
            menu_file = self.add_context_menu_node(menu_all, "0", _("Selected file"))
        if node_type == NODE_DIR:
            menu_dir = self.add_context_menu_node(menu_all, "0", _("Selected folder"))

        for item in self.menuitems:
            item_caption = item[0]
            item_parent = item[1]
            item_types = item[2]
            item_action = item[3]
            item_hotkey = item[4]
            if node_type not in item_types:
                continue

            if item_parent == "proj":
                menu_use = menu_proj
            elif item_parent == "nodes":
                menu_use = menu_nodes
            elif item_parent == "file":
                menu_use = menu_file
            elif item_parent == "dir":
                menu_use = menu_dir
            else:
                menu_use = menu_all

            if item_action in ("collect_recent_projects", "sessions_submenu", "favorites_submenu"):
                action = ""
            else:
                action = item_action

            menu_added = self.add_context_menu_node(menu_use, action, item_caption)

            if item_hotkey:
                menu_proc(menu_added, MENU_SET_HOTKEY, item_hotkey)

            if item_action == "collect_recent_projects":
                for path in self.options["recent_projects"]:
                    if os.sep in path:
                        action = str.format("module=cuda_project_man;cmd=action_open_project;info={};", path)
                        self.add_context_menu_node(menu_added, action, nice_filename(path))

            elif item_action == "sessions_submenu":
                self.fill_sessions_menu(menu_added)
            elif item_action == "favorites_submenu":
                self.fill_favorites_menu(menu_added)

    @staticmethod
    def node_ordering(node):
        # sort folders first, then by extension
        path = Path(node)
        return path.is_file(), path.suffix.upper(), path.name.upper()

    @staticmethod
    def node_ordering_direntry(path):
        ''' node_ordering() for os.DirEntry and pathlib.Path '''
        global sort_order
        isfile = path.is_file()
        if sort_order=='ext':
            key = _file_ext(path.name).upper() if isfile else ''
        elif sort_order=='name':
            key = path.name.upper()
        elif sort_order=='size':
            key = -path.stat().st_size if isfile else ''
        elif sort_order=='size-':
            key = path.stat().st_size if isfile else ''
        elif sort_order=='mtime':
            key = -path.stat().st_mtime if isfile else ''
        elif sort_order=='mtime-':
            key = path.stat().st_mtime if isfile else ''
        elif sort_order=='ctime':
            key = -path.stat().st_ctime if isfile else ''
        elif sort_order=='ctime-':
            key = path.stat().st_ctime if isfile else ''
        elif sort_order=='':
            key = ''
        else:
            raise ValueError('Unknown sort_order: '+sort_order)

        return isfile, key, path.name.upper()

    @_with_backup_cause('on project save')
    def add_node(self, path):
        if path:
            # tolerant compare (path_equals): the dialog may return the
            # path with or without the trailing separator ('D:\\' vs
            # 'D:', UNC roots) - an exact 'in' check could let a root
            # node in twice
            if any(path_equals(path, n) for n in self.project["nodes"]):
                return

            msg_status(_("Adding to project: ") + '"' + collapse_filename(path) + '"', True)
            self.project["nodes"].append(path)
            self.project["nodes"].sort(key=Command.node_ordering)
            self._fire_project_nodes_added()
            self.action_refresh()
            if self.project_file_path:
                self.action_save_project_as(self.project_file_path)

    def new_project(self):
        """Reset the plugin state to "no project". Only used at plugin
        startup, when nothing is open yet — it never touches sessions,
        tabs or files."""
        self.project = normalize_project_data(dict(nodes=[]))
        self.project_file_path = None
        self.update_global_data()
        self.goto_history = []
        self.cur_dir = ''

        app_proc(PROC_SET_FOLDER, '')
        app_proc(PROC_SET_PROJECT, '')

    def add_recent(self, path):
        recent = self.options["recent_projects"]
        if path in recent:
            recent.pop(recent.index(path))

        self.options["recent_projects"] = ([path] + recent)[:10]
        self.generate_context_menu()

    def action_new_file(self):
        location = Path(self.get_location_by_index(self.selected))
        if location.is_file():
            location = location.parent

        result = dlg_input(_("New file:"), "")
        if not result:
            return

        if os.sep in result:
            msg_status(_("Incorrect file name"))
            return

        path = location / result
        path.touch()
        self.action_refresh()

        #open new file
        self.jump_to_filename(str(path))
        if os.path.isfile(str(path)):
            _file_open(str(path))

    def action_open_def(self):
        fn = self.get_location_by_index(self.selected)
        sfn = str(fn)
        if not os.path.isfile(sfn):
            return
        suffix = OS_SUFFIX
        if suffix=='':
            #Windows
            #os.startfile(sfn) crashes with LSP plugin
            import subprocess
            subprocess.Popen('start "" "{}"'.format(sfn), shell=True, cwd=os.path.dirname(sfn))
        elif suffix=='__mac':
            #macOS
            os.system('open "%s"'%sfn)
        elif suffix=='__haiku':
            #Haiku
            msg_status('"Open in default application" is not implemented for this OS')
        else:
            #other Unixes
            os.spawnvp(os.P_WAIT, 'xdg-open', ('xdg-open', fn.as_uri()))

    def action_focus_in_fileman(self):
        fn = self.get_location_by_index(self.selected)
        sfn = str(fn)
        #if not os.path.isfile(sfn):
            #return
        suffix = OS_SUFFIX

        if suffix=='':
            #Windows
            #os.system('explorer.exe /select,'+sfn)
            import subprocess
            subprocess.Popen(('explorer.exe', '/select,', sfn), shell=True) # works better
        elif suffix=='__mac':
            #macOS
            #macOS cannot handle quoted filename
            os.system('open --new --reveal '+sfn.replace(' ', '\\ '))
        elif suffix=='__haiku':
            #Haiku
            msg_status('"Reveal in file manager" is not implemented for this OS')
        else:
            #Linux and others
            if os.spawnvp(os.P_WAIT, 'dbus-send', ('dbus-send', '--session', '--dest=org.freedesktop.FileManager1', '--type=method_call', '--print-reply', '/org/freedesktop/FileManager1', 'org.freedesktop.FileManager1.ShowItems', 'array:string:'+fn.as_uri(), 'string:')):
                os.spawnvp(os.P_WAIT, 'xdg-open', ('xdg-open', fn.parent.as_uri()))

    def action_rename(self):
        location = Path(self.get_location_by_index(self.selected))
        e = _file_editor(str(location))

        # TODO: Support unsaved changes when API available
        if e is not None \
        and e.get_prop(PROP_MODIFIED, '') == True \
        and msg_box(_('The file you are renaming has unsaved changes. Would you like to save the changes first?'), MB_OKCANCEL+MB_ICONWARNING) != ID_OK:
            return

        result = dlg_input(_("Rename to:"), str(location.name))
        if not result:
            return

        new_location = location.parent / result
        if location == new_location:
            return

        if e is not None:
            e.save(str(new_location))
            location.unlink(True)
        else:
            location.replace(new_location)

        if self.is_path_in_root(location):
            self.action_remove_node()
            self.add_node(str(new_location))

        # fix filename of existing ui-tab
        for h in ed_handles():
            e = Editor(h)
            if e.get_prop(PROP_FN, '*')==str(location):
                e.save(str(new_location))

        self.action_refresh()
        self.jump_to_filename(str(new_location))
        msg_status(_("Renamed to: ") + str(collapse_filename(str(new_location.name))))

    def action_duplicate(self):
        location = Path(self.get_location_by_index(self.selected))
        e = _file_editor(str(location))

        fn_parts = location.name.split('.')
        from datetime import datetime
        fn_1 = location.name if not fn_parts[0] else fn_parts[0]
        fn_2 = '' if not fn_parts[0] else ('.' + '.'.join(fn_parts[i+1] for i in range(len(fn_parts) - 1)))
        backup_name = fn_1 + '_' + datetime.now().strftime("%y%m%d_%H%M%S") + fn_2 if not os.path.isdir(str(location)) else location.name+' copy'
        result = dlg_input(_("Duplicate to:"), str(backup_name))
        if not result:
            return

        new_location = location.parent / result
        if location == new_location:
            return

        if e is not None:
            e.save(str(new_location), True)
        else:
            import shutil
            if os.path.isdir(location):
                shutil.copytree(location, new_location, symlinks=False)
            else:
                shutil.copy2(location, new_location)

        if self.is_path_in_root(location):
            self.add_node(str(new_location))

        self.action_refresh()
        msg_status(_("Copied to: ") + str(collapse_filename(str(new_location.name))))

    def action_delete_file(self):
        location = Path(self.get_location_by_index(self.selected))
        if msg_box(_("Delete file from disk:\n") + str(location), MB_OKCANCEL + MB_ICONWARNING) != ID_OK:
            return

        location.unlink()
        if self.is_path_in_root(location):
            self.action_remove_node()
        else:
            h_parent = tree_proc(self.tree, TREE_ITEM_GET_PROPS, self.selected)['parent']
            tree_proc(self.tree, TREE_ITEM_SELECT, h_parent)
            self.action_refresh(h_parent)
        msg_status(_("Deleted file: ") + str(location.name))

    def action_copy_relative_path(self):
        file_path = str(self.get_location_by_index(self.selected))
        project_node = ''
        for node in self.project["nodes"]:
            if (file_path+os.sep).startswith(node+os.sep):
                project_node = node
                break
        if project_node:
            file_path = file_path[len(os.path.dirname(project_node))+1:]
        app_proc(PROC_SET_CLIP, file_path)

    def action_copy_path(self):
        file_path = str(self.get_location_by_index(self.selected))
        proj_path = os.path.dirname(self.project_file_path or '')
        if not proj_path:
            return
        if file_path.startswith(proj_path+os.sep):
            file_path = file_path[len(proj_path)+1:]
        app_proc(PROC_SET_CLIP, file_path)

    def action_cut(self):
        selected = self.get_location_by_index(self.selected)
        if (selected.is_file() or selected.is_dir()):
            app_proc(PROC_SET_CLIP, str(selected))
        self.action_cut_activate = True

    def action_copy(self):
        selected = self.get_location_by_index(self.selected)
        if (selected.is_file() or selected.is_dir()):
            app_proc(PROC_SET_CLIP, str(selected))

    def action_paste(self):
        location = app_proc(PROC_GET_CLIP, '')
        new_location = Path(self.get_location_by_index(self.selected))
        if (Path(location).is_file() or Path(location).is_dir()):
            if new_location.is_file():
                new_location = new_location.parent
            if Path(location).is_file():
                new_location_path = str(new_location) + os.sep + str(Path(location).name)
                if location == new_location_path:
                    fn_parts = Path(location).name.split('.')
                    from datetime import datetime
                    fn_1 = Path(location).name if not fn_parts[0] else fn_parts[0]
                    fn_2 = '' if not fn_parts[0] else ('.' + '.'.join(fn_parts[i+1] for i in range(len(fn_parts) - 1)))
                    new_name = fn_1 + '_' + datetime.now().strftime("%y%m%d_%H%M%S") + fn_2
                    new_location = str(new_location) + os.sep + new_name
                import shutil
                if self.action_cut_activate:
                    shutil.move(location, new_location)
                else:
                    shutil.copy2(location, new_location)
            else:
                new_location = str(new_location) + os.sep + Path(location).name
                import shutil
                if self.action_cut_activate:
                    shutil.move(str(location), new_location)
                else:
                    # shutil, not the removed distutils (gone in Python 3.12);
                    # copytree refuses to overwrite an existing folder, while
                    # distutils copy_tree silently merged into it
                    try:
                        shutil.copytree(str(location), new_location)
                    except OSError as ex:
                        msg_status(_('Cannot copy folder: ') + str(ex))
                        return
            self.action_refresh()

    def get_w_h(self):
        w_ = 600
        h_ = 600
        r = app_proc(PROC_COORD_MONITOR, 0)
        if r:
            w_ = (r[2]-r[0]) // 3
            h_ = (r[3]-r[1]) // 4

        return w_, h_

    def callback_button_ok(self, id_dlg, id_ctl, data='', info=''):
        dlg_proc(id_dlg, DLG_HIDE)

    def show_memo(self, text, caption):
        w_, h_ = self.get_w_h()
        colors = _favorites.get_theme_ui_colors()

        def on_form_key(id_dlg, id_ctl, data='', info=''):
            # button_ex has no "default button" support, so the form
            # handles Enter itself (Esc closes the modal form - engine)
            if id_ctl == VK_ENTER and not (data or ''):
                dlg_proc(id_dlg, DLG_HIDE)
                return False
            return None

        fprop = {
            'w': w_,
            'h': h_,
            'w_min': 300,
            'h_min': 150,
            'cap': caption,
            'border': DBORDER_SIZE,
            'keypreview': True,     # needed for on_key_down (Enter = OK)
            'on_key_down': on_form_key,
        }
        if colors['form_bg'] is not None:
            fprop['color'] = colors['form_bg']
        h = dlg_proc(0, DLG_CREATE)
        dlg_proc(h, DLG_PROP_SET, prop=fprop)

        # button_ex = the flat themed CudaText button (follows the UI
        # theme); the OS 'button' cannot be themed via the dialog API.
        # It draws its caption itself, so no '&' accelerator
        n = dlg_proc(h, DLG_CTL_ADD, prop='button_ex')
        dlg_proc(h, DLG_CTL_PROP_SET, index=n, prop={
            'name': 'btn_ok',
            'a_l': None,
            'a_t': None,
            'a_r': ('', ']'),
            'a_b': ('', ']'),
            'sp_a': 6,
            'x': w_-100,
            'y': h_-6-25,
            'w': 100-6,
            'cap': _('OK'),
            'on_change': 'module=cuda_project_man;cmd=callback_button_ok;',
        })

        mp = {
            'name': 'memo_log',
            'val': text,
            'a_r': ('btn_ok', ']'),
            'a_b': ('btn_ok', '['),
            'x': 6,
            'y': 6,
            'w': w_-6*2,
            'h': h_-6*3-25,
            'ex0': True,           # read-only (dialog API prop of edit/memo)
            'ex1': True,           # monospaced font
        }
        if colors['edit_bg'] is not None:
            mp['color'] = colors['edit_bg']
        if colors['edit_font'] is not None:
            mp['font_color'] = colors['edit_font']
        n = dlg_proc(h, DLG_CTL_ADD, prop='memo')
        dlg_proc(h, DLG_CTL_PROP_SET, index=n, prop=mp)

        dlg_proc(h, DLG_CTL_FOCUS, name='btn_ok')
        dlg_proc(h, DLG_SCALE)
        dlg_proc(h, DLG_SHOW_MODAL)
        dlg_proc(h, DLG_FREE)

    def convert_size(self, size_bytes):
        size_bytes = int(size_bytes)
        if size_bytes == 0:
            return '0 b'
        size_name = ('b', 'kB', 'mB', 'gB')
        import math
        i = int(math.floor(math.log(size_bytes, 1024)))
        p = math.pow(1024, i)
        s = round(size_bytes / p, 2)
        return str("%s %s" % (s, size_name[i]))

    def action_get_properties(self):
        import os
        import datetime
        selected = self.get_location_by_index(self.selected)
        if os.path.exists(selected):
            if os.path.isfile(selected):
                text = _('File: ') + str(selected) + "\n"
                text += _('Size: ') + self.convert_size(os.path.getsize(selected)) + "\n"
                text += _('Date of creation: ') + str(datetime.datetime.fromtimestamp(int(os.path.getctime(selected)))) + "\n"
                text += _('Date of opening: ') + str(datetime.datetime.fromtimestamp(int(os.path.getatime(selected)))) + "\n"
                text += _('Date of modification: ') + str(datetime.datetime.fromtimestamp(int(os.path.getmtime(selected)))) + "\n"
            elif os.path.isdir(selected):
                text = _('Folder: ') + str(selected) + "\n"

                def folder_stats(path):
                    # single recursive walk returns (total_size,
                    # file_count, dir_count, symlink_count): counting
                    # files, sub-directories and symlinks here avoids
                    # walking the tree a second time just for the
                    # counts. Symlinks are NOT followed (same rule as
                    # the old folder_size) so cyclic links cannot loop
                    # forever -- they are only counted.
                    total = 0
                    files = 0
                    dirs = 0
                    symlinks = 0
                    try:
                        it = os.scandir(path)
                    except OSError:
                        return total, files, dirs, symlinks
                    with it:
                        for entry in it:
                            try:
                                if entry.is_symlink():
                                    symlinks += 1
                                    continue
                                if entry.is_dir(follow_symlinks=False):
                                    dirs += 1
                                    sub_size, sub_files, sub_dirs, sub_syml = folder_stats(entry.path)
                                    total += sub_size
                                    files += sub_files
                                    dirs += sub_dirs
                                    symlinks += sub_syml
                                elif entry.is_file(follow_symlinks=False):
                                    total += entry.stat(follow_symlinks=False).st_size
                                    files += 1
                            except OSError:
                                continue
                    return total, files, dirs, symlinks

                total_size, file_count, dir_count, symlink_count = folder_stats(selected)
                text += _('Size: ') + str(self.convert_size(total_size)) + "\n"
                text += _('Files: ') + str(file_count) + "\n"
                text += _('Folders: ') + str(dir_count) + "\n"
                if symlink_count:
                    text += _('Symlinks: ') + str(symlink_count) + "\n"
        else:
            text = _('Object not found')

        if text:
            self.show_memo(text, _('Properties'))

    def do_delete_dir(self, location):
        for path in location.glob("*"):
            if path.is_file():
                path.unlink()
            elif path.is_dir():
                self.do_delete_dir(path)
        location.rmdir()

    def action_delete_directory(self):
        location = Path(self.get_location_by_index(self.selected))
        if msg_box(_("Delete folder from disk:\n") + str(location), MB_OKCANCEL + MB_ICONWARNING) != ID_OK:
            return

        self.do_delete_dir(location)
        if self.is_path_in_root(location):
            self.action_remove_node()
        else:
            h_parent = tree_proc(self.tree, TREE_ITEM_GET_PROPS, self.selected)['parent']
            tree_proc(self.tree, TREE_ITEM_SELECT, h_parent)
            self.action_refresh(h_parent)

            path = self.get_location_by_index(self.selected)
            app_proc(PROC_SET_FOLDER, path)
            self.cur_dir = path

        msg_status(_("Deleted folder: ") + str(location.name))

    def action_new_directory(self):
        location = Path(self.get_location_by_index(self.selected))
        if location.is_file():
            location = location.parent
        result = dlg_input(_("New folder:"), "")
        if not result:
            return

        location = location / result
        location.mkdir()
        self.action_refresh()
        self.jump_to_filename(str(location))

    def action_find_in_directory(self):
        location = str(self.get_location_by_index(self.selected))
        try:
            import cuda_find_in_files as fif
            msg_status(_('Called "Find in Files" for "%s"') % location)
            fif.show_dlg(what="", opts={"fold": location})
        except ImportError:
            try:
                from cuda_find_in_files4 import show_fif4
                msg_status(_('Called "Find in Files 4" for "%s"') % location)
                show_fif4({'with': {
                    'in_what': '',
                    'wk_fold': location,
                    'wk_incl': '*'
                    }})
            except ImportError:
                msg_box(_('Plugin "Find in Files" or "Find in Files 4" is not installed; install it first'), MB_OK + MB_ICONERROR)


    def action_refresh(self, parent=None):
        # panel may be not created yet (plugin was lazy-loaded by a menu
        # command, side panel never shown): nothing to refresh, the tree
        # is filled by init_panel() when the panel is shown
        if not self.tree:
            return

        sel_fn = ''
        id = self.selected
        if id:
            prop = tree_proc(self.tree, TREE_ITEM_GET_PROPS, id)
            if prop:
                sel_fn = prop.get('data', '')

        # it was hard to add TREE_LOCK/UNLOCK directly into action_refresh_int
        tree_proc(self.tree, TREE_LOCK)
        try:
            unfolds = []
            self.enum_all_getfolds(unfolds)
            #msg_box('unfolds:\n'+', '.join(unfolds), MB_OK)

            if self.tree_filter:
                if parent is None:
                    # the filter field is active: re-run the BACKGROUND
                    # filtered scan (action_refresh_int would show
                    # everything); the tree is refilled when it finishes
                    self._filter_start(self.tree_filter)
                else:
                    # refresh ONE node, filtered: the same cheap lazy
                    # refill an expand does (from the index / one-level
                    # name filter) - see _filter_refill_children
                    fn = str(self.get_location_by_index(parent) or '')
                    if fn:
                        items = tree_proc(self.tree, TREE_ITEM_ENUM_EX,
                                          parent) or []
                        for item in items:
                            tree_proc(self.tree, TREE_ITEM_DELETE,
                                      item['id'])
                        self._filter_refill_children(parent, fn)
            else:
                self.action_refresh_int(parent)

            self.enum_all_setfolds(unfolds)
        finally:
            tree_proc(self.tree, TREE_UNLOCK)

        if sel_fn:
            #print('sel to:', sel_fn)
            self.enum_all_sel(sel_fn)


    def get_project_name(self):

        if self.project_file_path is None:
            return PROJECT_UNSAVED_NAME
        else:
            return self.project_file_path.stem

    def action_refresh_int(self, parent=None):

        global sort_order
        sort_order = self.options.get('sort_order', 'ext')

        unfold = parent is None
        if parent is None:
            # clear tree
            tree_proc(self.tree, TREE_ITEM_DELETE, 0)
            project_name = self.get_project_name()

            parent = tree_proc(
                self.tree,
                TREE_ITEM_ADD,
                0,
                -1,
                project_name,
                self.ICON_PROJ,
            )

            #select 1st node
            items_root = tree_proc(self.tree, TREE_ITEM_ENUM_EX, 0)
            tree_proc(self.tree, TREE_ITEM_SELECT, items_root[0]['id'])

            nodes = map(Path, self.project.get("nodes", []))
            if sort_order != '':
                nodes = sorted(nodes, key=Command.node_ordering_direntry)
        else:
            fn = str(self.get_location_by_index(parent)) # str() is required for old Python 3.5 for os.scandir()
            if not fn: return
            #print('Reading dir:', fn)
            try:
                if hasattr(os, "scandir") and callable(os.scandir):
                    if sort_order == '':
                        scandir_ = list(os.scandir(fn))
                        dirs_ = [i for i in scandir_ if i.is_dir()]
                        files_ = [i for i in scandir_ if not i.is_dir()]
                        nodes = dirs_ + files_
                    else:
                        nodes = sorted(os.scandir(fn), key=Command.node_ordering_direntry)
                else:
                    # support for old Python 3.4
                    if sort_order == '':
                        nodes = Path(fn).iterdir()
                    else:
                        nodes = sorted(Path(fn).iterdir(), key=Command.node_ordering)
            except Exception:
                tree_proc(self.tree, TREE_ITEM_SET_ICON, parent, image_index=self.ICON_BAD)
                raise # good to see the error
                return

        for path in nodes:
            # DirEntry or Path?
            if isinstance(path, Path):
                spath = str(path)
            else:
                spath = path.path
            is_dir = path.is_dir()
            sname = path.name
            if is_win_root(spath) or (not sname):
                sname = spath
            elif self.options.get("no_hidden", True) and is_hidden(spath):
                continue
            elif self.is_filename_ignored(spath, is_dir):
                continue

            imageindex = self._node_icon(spath, sname, is_dir)

            index = tree_proc(
                self.tree,
                TREE_ITEM_ADD,
                parent,
                -1,
                sname,
                imageindex,
                data=spath
                )

            # dummy nested node for folders
            if imageindex == self.ICON_DIR:
                tree_proc(
                    self.tree,
                    TREE_ITEM_ADD,
                    index,
                    -1,
                    'dummy',
                    -1
                    )

        if unfold:
            tree_proc(self.tree, TREE_ITEM_UNFOLD, parent)

    #------------- tree filter (the field above the tree)

    def panel_filter_on_change(self, id_dlg, id_ctl, data='', info=''):
        """The filter field above the tree changed. The (slow) filtered
        fill does NOT run per keystroke: a one-shot timer applies the
        text only after the typing pauses, then the scan runs in a
        BACKGROUND thread - a big tree never freezes CudaText anymore.
        Clearing the field is applied at once (it is cheap). While the
        field is not empty, the status line under it always shows what
        the search is doing."""
        if not self.tree:
            return
        try:
            props = dlg_proc(self.h_dlg, DLG_CTL_PROP_GET, name='f_filter') or {}
            text = str(props.get('val', '') or '')
        except Exception:
            text = ''
        # the (x) button shows only with text / a running search
        self._filter_cancel_btn_update()
        if not text:
            timer_proc(TIMER_STOP, self._filter_timer_fire, 0)
            self._filter_pending = None
            self.apply_tree_filter('')
            return
        # the filter is active from the FIRST keystroke: the status
        # line shows "searching" right away (the debounce only delays
        # the scan start)
        self._filter_label_running(0)
        self._filter_pending = text
        # re-arm the debounce: stop the pending one-shot timer, then
        # start a fresh one, so it fires FILTER_APPLY_DELAY_MS after
        # the LAST keystroke, not after the first
        timer_proc(TIMER_STOP, self._filter_timer_fire, 0)
        timer_proc(TIMER_START_ONE, self._filter_timer_fire,
                   FILTER_APPLY_DELAY_MS)

    def _filter_timer_fire(self, tag=None):
        """The debounce delay passed: apply the live filter text."""
        self._filter_pending = None
        if not self.tree:
            return
        try:
            props = dlg_proc(self.h_dlg, DLG_CTL_PROP_GET, name='f_filter') or {}
            text = str(props.get('val', '') or '')
        except Exception:
            return
        if text == self.tree_filter:
            # the field was edited back to the same text: nothing to
            # re-scan, just restore the "finished" line which the
            # keystroke had replaced with "searching"
            if text:
                self._filter_label_done(self._filter_rows_count)
            return
        self.apply_tree_filter(text)

    def _filter_cancel_btn_update(self):
        """Show the filter's (x) button only while it can do something:
        while the filter field has text, or a background search is
        running. With no text and no search the button is hidden AND
        collapsed to zero width, so the filter field stretches over
        the full row (the field's right edge is anchored to the
        button's LEFT edge, and a hidden control keeps its bounds - so
        only hiding the button would leave an empty gap at the right
        of the field). Called at panel build, on every filter-field
        change and whenever a scan starts or ends."""
        if not self.h_dlg:
            return
        try:
            props = dlg_proc(self.h_dlg, DLG_CTL_PROP_GET, name='f_filter') or {}
            text = str(props.get('val', '') or '')
        except Exception:
            text = ''
        w = self._filter_worker
        searching = (w is not None) and (not w.done)
        try:
            if text or searching:
                dlg_proc(self.h_dlg, DLG_CTL_PROP_SET, name='btn_fcancel',
                         prop={'w': self._filter_cancel_w, 'vis': True})
            else:
                dlg_proc(self.h_dlg, DLG_CTL_PROP_SET, name='btn_fcancel',
                         prop={'w': 0, 'vis': False})
        except Exception:
            pass

    def apply_tree_filter(self, text):
        """Show only the matching nodes in the tree: starts the
        BACKGROUND scan and shows its INSTANT first view (only the
        root nodes whose own name matches - see _filter_start); while
        the scan runs, the progress timer grows the tree with the
        findings so far, and when it finishes the tree is refilled
        with the TOP level of the scan's lazy index (deeper levels
        load as the user expands them). Empty text cancels the scan
        and restores the normal tree, with the folds/selection
        captured before the filtering started."""
        text = (text or '').strip()
        if not self.tree:
            return
        if text == self.tree_filter:
            return

        if not self.tree_filter and text:
            # filtering starts: remember what to restore later
            folds = []
            self.enum_all_getfolds(folds)
            self._filter_saved_folds = folds
            self._filter_saved_sel = ''
            node = self.selected
            if node:
                prop = tree_proc(self.tree, TREE_ITEM_GET_PROPS, node)
                if prop:
                    self._filter_saved_sel = prop.get('data', '')

        self.tree_filter = text
        if text:
            self._filter_start(text)
        else:
            self._filter_cancel_scan()
            self._filter_label_clear()
            # the (x) button hides with the scan stopped and (normally)
            # the field emptied - AFTER the cancel, so the update sees
            # the worker already gone (an update before the cancel
            # would still see it running and keep the button visible)
            self._filter_cancel_btn_update()
            # drop the lazy index of the finished scan and the fallback
            # cache: the next filtering builds fresh ones
            self._filter_index = None
            self._filter_live_done = None
            self._filter_fallback = None
            self._filter_expanded = None
            self._filter_rows_count = 0
            self.action_refresh()
            # restore the state from before the filtering
            if self._filter_saved_folds:
                self.enum_all_setfolds(list(self._filter_saved_folds))
            if self._filter_saved_sel:
                self._filter_select_by_path(self._filter_saved_sel)

    def panel_filter_cancel_click(self, id_dlg, id_ctl, data='', info=''):
        """The x button at the right of the filter field (it is only
        visible while the field has text or a search is running - see
        _filter_cancel_btn_update). While a search is RUNNING (the
        background worker) it CANCELS the search: the tree is rebuilt
        from the findings SO FAR, so they stay visible and browsable
        (expanding their folders keeps refilling exactly those rows),
        the status line reports what was found, and the filter text
        stays - the user can keep browsing what is shown (so the
        button stays visible). With no search running, it clears the
        filter, like the x button of a usual search box - and the
        button hides with the text gone."""
        if not self.tree:
            return
        w = self._filter_worker
        if w is None or w.done:
            # nothing to cancel - clear the filter
            self.clear_panel_filter()
            return
        self._filter_cancel_scan()
        # promote the PARTIAL result to a finished-style lazy index,
        # cleaned of the optimistic rows (the folders in which no match
        # was found so far): the tree keeps showing the findings so far,
        # and expanding nodes after the cancel must refill exactly those
        # rows - not the single-level fallback, which knows nothing
        # about the deeper matches. The worker thread may still be
        # running (it dies on the cancel flag a moment later), so the
        # dict snapshot retries while it keeps adding buckets - a plain
        # .items() loop could raise "dictionary changed size during
        # iteration"
        while True:
            try:
                items = list((w.res or {}).items())
                break
            except RuntimeError:
                continue
        cleaned = {}
        for key, bucket in items:
            rows = [r for r in bucket if not r[2] or r[1] in w.live]
            if rows:
                cleaned[key] = rows
        self._filter_index = cleaned
        self._filter_live_done = w.live
        self._filter_rows_count = w.count
        self._filter_label_cancelled(w.count)
        # rebuild the visible tree from the snapshot: the findings stay
        self._filter_rebuild(cleaned, w.live)

    def clear_panel_filter(self):
        """Esc / the (x) button: clear the filter field and show the
        full tree again. Setting the edit's value programmatically does
        not fire its on_change event, so apply_tree_filter() must be
        called here. The field is also cleared while the typed text is
        still inside the debounce delay (not applied yet): the pending
        scan is dropped, the tree was never filtered, so there is
        nothing to restore - and the (x) button, which shows for ANY
        text, must always have an effect."""
        try:
            props = dlg_proc(self.h_dlg, DLG_CTL_PROP_GET, name='f_filter') or {}
            text = str(props.get('val', '') or '')
        except Exception:
            text = ''
        # stop the debounce of a not-yet-applied filter text (harmless
        # when no timer is armed)
        timer_proc(TIMER_STOP, self._filter_timer_fire, 0)
        self._filter_pending = None
        if not self.tree_filter:
            # nothing applied: just empty the field (if it holds the
            # not-yet-applied text) and hide the (x) button
            if text:
                try:
                    dlg_proc(self.h_dlg, DLG_CTL_PROP_SET, name='f_filter',
                             prop={'val': ''})
                except Exception:
                    pass
                self._filter_cancel_btn_update()
            return
        self._filter_cancel_scan()
        self._filter_label_clear()
        try:
            dlg_proc(self.h_dlg, DLG_CTL_PROP_SET, name='f_filter',
                     prop={'val': ''})
        except Exception:
            pass
        self.apply_tree_filter('')

    #------------- background scan (the _FilterWorker thread)

    def _filter_start(self, text):
        """(Re)start the background scan for the given filter text: a
        fresh _FilterWorker thread (the previous one is cancelled
        co-operatively) + the repeating progress timer which polls it.
        The root nodes and the scan-ruling options are snapshotted
        HERE, on the UI thread - the worker never touches the plugin
        state (the CudaText API is not thread-safe).

        The tree also gets the INSTANT first view of the filter right
        here, before any scanning: only the root nodes whose own name
        matches stay visible, everything else hides at once - a node
        can only come back when the scan finds a match inside it (the
        progress timer's live rebuilds show those findings as they
        come). The previous search's lazy index and fallback cache are
        dropped as well: their rows belong to the OLD filter text and
        must never leak into a rebuild or an expand of the new search."""
        if not self.tree or not text:
            return
        self._filter_cancel_worker()
        global sort_order
        sort_order = self.options.get('sort_order', 'ext')

        roots = []
        nodes = map(Path, self.project.get("nodes", []))
        if sort_order != '':
            nodes = sorted(nodes, key=Command.node_ordering_direntry)
        for path in nodes:
            spath = str(path)
            sname = path.name
            is_dir = path.is_dir()
            if is_win_root(spath) or (not sname):
                sname = spath
            elif self.options.get("no_hidden", True) and is_hidden(spath):
                continue
            elif self.is_filename_ignored(spath, is_dir):
                continue
            roots.append((spath, sname, bool(is_dir)))

        w = _FilterWorker(text, roots)
        w.opts = {
            'no_hidden': bool(self.options.get('no_hidden', True)),
            'no_files': str(self.options.get('no_files', '') or ''),
            'no_dirs': str(self.options.get('no_dirs', '') or ''),
            'sort_order': sort_order,
        }
        # remember the snapshot for the lazy single-level fallback
        # refill too (folders the scan index does not know)
        self._filter_opts = dict(w.opts)
        self._filter_worker = w
        # drop the previous search's lazy index and fallback cache (its
        # rows are for the OLD filter text - a rebuild or an expand of
        # the new search must never show them) and reset the live-rebuild
        # bookkeeping
        self._filter_index = None
        self._filter_live_done = None
        self._filter_fallback = {}
        self._filter_expanded = set()
        self._filter_rows_count = 0
        self._filter_live_version = -1
        # the instant view: the root nodes whose own name matches
        inst_rows = [(sname, spath, is_dir)
                     for spath, sname, is_dir in roots
                     if text.lower() in sname.lower()]
        try:
            w.start()
        except Exception as ex:
            # cannot start a thread (very exotic): fall back to the
            # name-only view rather than break the panel (expands then
            # use the single-level fallback filter)
            print('Project Manager: cannot start the filter thread: ' + str(ex))
            self._filter_worker = None
            self._filter_index = {}
            self._fill_tree_filtered_top(inst_rows)
            self._filter_label_done(0)
            return
        self._fill_tree_filtered_top(inst_rows)
        self._filter_label_running(0)
        self._filter_timer_start()
        # a search is running now: the (x) button must show even before
        # the next keystroke reports the text
        self._filter_cancel_btn_update()

    def _filter_cancel_worker(self):
        """Cancel the running background scan (if any): it notices the
        flag around every directory and dies quietly; its result is
        dropped by _filter_progress_tick."""
        w = self._filter_worker
        self._filter_worker = None
        if w is not None:
            w.cancel = True

    def _filter_cancel_scan(self):
        """Full stop of the filter machinery: worker + progress timer
        (there is no chunked fill anymore - the tree is filled lazily
        from the index). The status line is NOT touched here (the
        callers decide what it shows next)."""
        self._filter_cancel_worker()
        self._filter_timer_stop()

    def _filter_timer_start(self):
        if not self._filter_progress_on:
            timer_proc(TIMER_START, self._filter_progress_tick,
                       FILTER_PROGRESS_MS)
            self._filter_progress_on = True

    def _filter_timer_stop(self):
        if self._filter_progress_on:
            timer_proc(TIMER_STOP, self._filter_progress_tick, 0)
            self._filter_progress_on = False

    def _filter_progress_tick(self, tag=''):
        """Repeating UI timer which polls the running filter worker.
        While it runs, the status line shows the live match count AND
        the visible tree is REBUILT from the worker's partial index
        whenever it changed (see _filter_rebuild): the findings so far
        are always shown, at the top level and inside every folder the
        user expanded - rows only ever appear, nothing else moves.
        When the scan finishes (and its text is still the current
        filter), its cleaned index becomes the search result, the tree
        is rebuilt from it once more (deeper levels still load lazily
        on expand - see _filter_refill_children), and the line reports
        the final count. Then the timer stops: nothing is left to do.
        The result of a cancelled/stale worker is dropped."""
        w = self._filter_worker
        if w is None:
            self._filter_timer_stop()
            return
        if not w.done:
            self._filter_label_running(w.count)
            if w.version != self._filter_live_version:
                self._filter_live_version = w.version
                self._filter_rebuild(w.res, w.live)
            return

        # finished
        self._filter_worker = None
        # the search ended: the (x) button now depends on the text alone
        self._filter_cancel_btn_update()
        if not self.tree or not self.tree_filter:
            self._filter_timer_stop()
            return
        if w.text == self.tree_filter and w.tree is not None:
            self._filter_rows_count = w.count
            self._filter_label_done(w.count)
            self._filter_index = w.tree
            self._filter_live_done = w.live
            self._filter_live_version = w.version
            self._filter_rebuild(w.tree, w.live)
        # else: stale (the filter text changed / the scan was
        # cancelled) - the newer worker reports itself
        self._filter_timer_stop()

    def _filter_scan_index(self):
        """The lazy index the filtered rows must be read from RIGHT
        NOW: the running worker's partial index, or the snapshot of the
        finished/cancelled scan."""
        w = self._filter_worker
        if w is not None and not w.done:
            return w.res
        return self._filter_index

    def _filter_scan_live(self):
        """The live set belonging to _filter_scan_index()."""
        w = self._filter_worker
        if w is not None and not w.done:
            return w.live
        return self._filter_live_done or set()

    def _filter_rows_for(self, key, index, live):
        """Rows to show under the folder `key`, read from the given
        index: its bucket (files always - a file row only enters a
        bucket when its name matched - and folders only when live: a
        match was found inside, or the folder's own name matches). A
        folder the index does not know yet (the scan has not reached
        it) shows the cached single-level fallback instead (see
        _filter_fallback_rows); the TOP level ('' key) is always read
        from the index alone - its bucket exists from the very first
        worker step."""
        bucket = (index or {}).get(key)
        if bucket is None:
            if key:
                return self._filter_fallback_rows(key)
            return []
        return [r for r in bucket if not r[2] or r[1] in live]

    def _filter_fallback_rows(self, path):
        """Cached single-level fallback for a folder no scan index
        knows: the names matching the filter in ONE listing of that
        folder - one scandir, the same cost as the normal unfiltered
        expand, never a recursive walk. Folders whose name matches show
        in it too (they ARE matches), with their +/- expander; what is
        inside them is found by the scan (or by expanding them - the
        same fallback again, one level deeper). Cached per search: the
        repeating live rebuilds must not re-read the disk, and the
        cache is dropped when the filter text changes or is cleared."""
        if self._filter_fallback is None:
            self._filter_fallback = {}
        cached = self._filter_fallback.get(path)
        if cached is not None:
            return cached
        flt = (self.tree_filter or '').lower()
        opts = self._filter_opts or self.options
        rows = []
        for e in scan_dir_entries(path, opts) or []:
            try:
                if flt in e.name.lower():
                    rows.append((e.name, e.path, e.is_dir()))
            except OSError:
                pass
        self._filter_fallback[path] = rows
        return rows

    def _filter_capture_expanded(self):
        """Folder paths whose nodes are unfolded RIGHT NOW, plus the
        folders the user unfolded earlier in this search whose level is
        (still) empty: such a level shows no expander, so its state can
        only be remembered in _filter_expanded - without that, rows
        which appear later would stay hidden behind a collapsed node.
        Folders the user folded again are NOT captured (they must stay
        folded); expanded folders hidden below a folded parent need no
        capture either - their level is refilled when the user expands
        the parent again."""
        res = set()
        if not self.tree:
            return res
        ever = self._filter_expanded or set()

        def walk(node):
            for it in tree_proc(self.tree, TREE_ITEM_ENUM_EX, node) or []:
                d = it.get('data', '')
                if not d:
                    continue    # dummy / project node
                if it.get('sub_items'):
                    prop = tree_proc(self.tree, TREE_ITEM_GET_PROPS,
                                     it['id'])
                    if prop and not prop.get('folded', True):
                        res.add(d)
                        walk(it['id'])
                elif d in ever:
                    # leaf node of a folder the user unfolded when the
                    # level was still empty: keep it expanded
                    res.add(d)

        items = tree_proc(self.tree, TREE_ITEM_ENUM_EX, 0) or []
        if items:
            walk(items[0]['id'])
        return res

    def _filter_select_by_path(self, path):
        """Select (and show) the tree node whose data is `path` - file OR
        folder node (the shared enum_all_sel can only select file nodes:
        its walk treats every node with sub-items as a folder to recurse
        into). Used to carry the selection over the filtered-tree
        rebuilds and back to the normal tree when the filter is
        cleared."""
        if not path or not self.tree:
            return
        found = []

        def cb(fn, node):
            if fn == path:
                found.append(node)
                return False
            return True

        self.enum_all(cb)
        if found:
            tree_proc(self.tree, TREE_ITEM_SELECT, found[0])
            tree_proc(self.tree, TREE_ITEM_SHOW, found[0])

    def _filter_rebuild(self, index, live):
        """Rebuild the visible part of the filtered tree from a lazy
        index - the worker's PARTIAL index while the scan runs (called
        from _filter_progress_tick), its cleaned result when it finished
        or was cancelled. The user's unfolded folders (see
        _filter_capture_expanded) and the selection are carried over,
        so a rebuild is visually calm: rows only ever appear, the folds
        and the selected node stay where they were.

        The tree stays LAZY: every folder which is not unfolded gets a
        'dummy' child, and its real rows are added on the expand
        (tree_on_unfold -> _filter_refill_children) - so even a match
        list of many thousands of rows (a single common letter can
        match that many files) never floods the tree at once, and
        expanding a folder while the filter is active costs exactly as
        little as expanding it with no filter at all."""
        if not self.tree or not self.tree_filter:
            return
        expanded = self._filter_capture_expanded()
        sel_path = ''
        sel = self.selected
        if sel:
            prop = tree_proc(self.tree, TREE_ITEM_GET_PROPS, sel)
            if prop:
                sel_path = str(prop.get('data', '') or '')
        # block tree_on_unfold: the rebuild's own programmatic unfolds
        # must not delete/refill the levels it is filling right now
        self._filter_rebuilding = True
        tree_proc(self.tree, TREE_LOCK)
        try:
            tree_proc(self.tree, TREE_ITEM_DELETE, 0)
            parent = tree_proc(
                self.tree,
                TREE_ITEM_ADD,
                0,
                -1,
                self.get_project_name(),
                self.ICON_PROJ,
            )
            self._filter_fill_level(parent, '', index, live, expanded)
            tree_proc(self.tree, TREE_ITEM_UNFOLD, parent)
        finally:
            tree_proc(self.tree, TREE_UNLOCK)
            self._filter_rebuilding = False
        # keep the selection where it was; select the project node when
        # the old selection does not exist anymore
        if sel_path:
            self._filter_select_by_path(sel_path)
        if not self.selected:
            items_root = tree_proc(self.tree, TREE_ITEM_ENUM_EX, 0)
            if items_root:
                tree_proc(self.tree, TREE_ITEM_SELECT, items_root[0]['id'])

    def _filter_fill_level(self, node, key, index, live, expanded):
        """Add the rows of ONE level of the filtered tree (the children
        of `node`, addressed by the index bucket key `key`): files
        always, folders only when live (see _filter_rows_for). A folder
        the user unfolded (its path is in `expanded`) gets its level
        filled recursively and STAYS unfolded - the on_unfold event of
        that programmatic unfold is blocked by _filter_rebuilding, so
        it cannot re-refill the level; every other folder gets a
        'dummy' child, so expanding it later fires tree_on_unfold,
        which lazily refills the real (filtered) children."""
        for sname, spath, is_dir in self._filter_rows_for(key, index,
                                                          live):
            child = self._add_tree_node(node, sname, spath, is_dir)
            if not is_dir:
                continue
            if spath in expanded:
                self._filter_fill_level(child, spath, index, live,
                                        expanded)
                tree_proc(self.tree, TREE_ITEM_UNFOLD, child)
            else:
                tree_proc(self.tree, TREE_ITEM_ADD, child, -1,
                          'dummy', -1)

    def _filter_label_running(self, count):
        self._filter_label(
            _('Filter: searching... {} match(es) so far').format(count))

    def _filter_label_done(self, count):
        self._filter_label(
            _('Filter: finished, {} match(es) found').format(count))

    def _filter_label_cancelled(self, count):
        self._filter_label(
            _('Filter: cancelled, {} match(es) so far').format(count))

    def _filter_label_clear(self):
        self._filter_label('')

    def _filter_label(self, cap):
        """Set the caption of the status line under the filter field
        (empty = no filtering: no text is shown, as promised)."""
        if not self.h_dlg:
            return
        try:
            dlg_proc(self.h_dlg, DLG_CTL_PROP_SET, name='lbl_fstat',
                     prop={'cap': cap})
        except Exception:
            pass

    def _node_icon(self, spath, sname, is_dir):
        """Icon index of one tree node (shared by the normal and the
        filtered tree fill). Cached by (name, is_dir, locked): the
        lexer detection per file name is not free, and the filtered
        fill asks for thousands of icons."""
        locked = is_locked(spath)
        key = (sname, is_dir, locked)
        res = _icon_cache.get(key)
        if res is not None:
            return res

        if locked:
            res = self.ICON_BAD
        elif is_dir:
            res = self.ICON_DIR
        elif is_simple_listed(sname, MASKS_IMAGES):
            res = self.ICON_IMG
        elif is_simple_listed(sname, MASKS_ZIP):
            res = self.ICON_ZIP
        elif is_simple_listed(sname, MASKS_BINARY):
            res = self.ICON_BIN
        elif is_simple_listed(sname, MASKS_AUDIO):
            res = self.ICON_AUDIO
        elif is_simple_listed(sname, MASKS_VIDEO):
            res = self.ICON_VIDEO
        else:
            lexname = lexer_proc(LEXER_DETECT, sname)
            if lexname:
                res = self.icon_get(lexname)
            elif is_simple_listed(sname, MASKS_TEXT):
                res = self.ICON_TEXT
            else:
                res = self.ICON_ALL
        _icon_cache[key] = res
        return res

    def _add_tree_node(self, parent, sname, spath, is_dir, index=-1):
        """Add one tree node with the standard icon; returns its
        handle. index=-1 appends the node, otherwise it is inserted at
        that 0-based position among the parent's children (the live
        filter sync uses it to drop new findings into their place)."""
        return tree_proc(
            self.tree,
            TREE_ITEM_ADD,
            parent,
            index,
            sname,
            self._node_icon(spath, sname, is_dir),
            data=spath
            )

    def _fill_tree_filtered_top(self, rows=None):
        """Rebuild the tree showing its TOP level only: the direct
        children of the project root taken from `rows` - the explicit
        rows of the INSTANT name-only view a scan starts with (only the
        root nodes whose own name matches: everything else hides until
        the scan finds a match inside it). Each folder gets a 'dummy'
        child so it can be expanded. Deeper levels are added LAZILY
        when their folder is expanded, and the whole visible tree is
        refreshed with the findings so far by _filter_rebuild while the
        scan runs. The project root is selected."""
        if not self.tree:
            return
        if rows is None:
            rows = (self._filter_index or {}).get('') or []
        tree_proc(self.tree, TREE_LOCK)
        try:
            tree_proc(self.tree, TREE_ITEM_DELETE, 0)
            parent = tree_proc(
                self.tree,
                TREE_ITEM_ADD,
                0,
                -1,
                self.get_project_name(),
                self.ICON_PROJ,
            )
            for sname, spath, is_dir in rows:
                node = self._add_tree_node(parent, sname, spath, is_dir)
                if is_dir:
                    # dummy child: gives the folder its +/- expander;
                    # expanding it fires tree_on_unfold, which refills
                    # the real (filtered) children lazily
                    tree_proc(self.tree, TREE_ITEM_ADD, node, -1,
                              'dummy', -1)
            tree_proc(self.tree, TREE_ITEM_UNFOLD, parent)
            items_root = tree_proc(self.tree, TREE_ITEM_ENUM_EX, 0)
            if items_root:
                tree_proc(self.tree, TREE_ITEM_SELECT, items_root[0]['id'])
        finally:
            tree_proc(self.tree, TREE_UNLOCK)

    def _filter_refill_children(self, node, path):
        """Refill the children of ONE folder of the filtered tree,
        LAZILY and CHEAPLY - this is what runs when the user expands a
        node while the filter is active (see _filter_rows_for):
        the rows the current scan state knows - the running worker's
        PARTIAL index (files always, folders only when a match was
        already found inside them), or the finished/cancelled snapshot
        (pure memory reads, no disk access at all) - and, for a folder
        no index knows yet, the cached SINGLE-LEVEL name filter over
        one directory listing (one scandir, the same cost as the normal
        unfiltered expand). The level keeps growing while the scan
        continues: every progress tick rebuilds it with the findings
        so far (see _filter_rebuild).

        It NEVER walks a folder recursively on the UI thread (the old
        implementation re-scanned whole subtrees here: expanding a
        drive-sized folder while a search was running hung CudaText
        and ate RAM). Every folder child gets a 'dummy' node, so the
        deeper levels keep loading the same lazy way."""
        if not self.tree or not self.tree_filter:
            return
        children = self._filter_rows_for(path,
                                         self._filter_scan_index(),
                                         self._filter_scan_live())
        tree_proc(self.tree, TREE_LOCK)
        try:
            for sname, spath, is_dir in children:
                child = self._add_tree_node(node, sname, spath, is_dir)
                if is_dir:
                    tree_proc(self.tree, TREE_ITEM_ADD, child, -1,
                              'dummy', -1)
        finally:
            tree_proc(self.tree, TREE_UNLOCK)

    #------------- type-ahead (incremental search in the tree)

    def _filter_edit_focused(self):
        """True while the user types in the panel's filter field (those
        keystrokes belong to the filter, not to the type-ahead)."""
        if self.filter_ctl_index < 0 or not self.h_dlg:
            return False
        try:
            props = dlg_proc(self.h_dlg, DLG_PROP_GET) or {}
            return int(props.get('focused', -1)) == self.filter_ctl_index
        except Exception:
            return False

    def _typeahead_scope(self):
        """(node handles, position of the selection) of the level the
        type-ahead searches in: the children of the selected node's
        parent. Being 'inside' a folder means the selection is one of
        its children, so typing jumps only inside that folder."""
        if not self.tree:
            return [], -1
        items_root = tree_proc(self.tree, TREE_ITEM_ENUM_EX, 0) or []
        if not items_root:
            return [], -1
        root = items_root[0]['id']

        parent = root
        sel = self.selected
        cur = -1
        if sel:
            prop = tree_proc(self.tree, TREE_ITEM_GET_PROPS, sel)
            if prop:
                parent = prop.get('parent') or root

        items = tree_proc(self.tree, TREE_ITEM_ENUM_EX, parent) or []
        handles = []
        for i, it in enumerate(items):
            handles.append(it['id'])
            if it['id'] == sel:
                cur = i
        return handles, cur

    def _typeahead_match(self, text, select=False):
        """First node of the current level whose caption starts with
        text (case-insensitive), searched cyclically AFTER the selected
        node - so pressing the same letter again jumps to the next
        match. Selects the found node when select=True. Returns the node
        handle, or None when nothing matches."""
        text = (text or '').lower()
        if not text:
            return None
        items, cur = self._typeahead_scope()
        count = len(items)
        if not count:
            return None
        for i in range(1, count + 1):
            index = (cur + i) % count
            node = items[index]
            prop = tree_proc(self.tree, TREE_ITEM_GET_PROPS, node)
            if not prop:
                continue
            caption = prop.get('text', '')
            if not caption or not prop.get('data', ''):
                continue  # skip the 'dummy' placeholders of folded dirs
            if caption.lower().startswith(text):
                if select:
                    tree_proc(self.tree, TREE_ITEM_SELECT, node)
                    tree_proc(self.tree, TREE_ITEM_SHOW, node)
                return node
        return None

    def _typeahead_key(self, key):
        """Windows-Explorer-like incremental search: plain letters and
        digits jump to the first node of the current level starting with
        the typed text ("myf" finds "myfile"); pressing the same letter
        again cycles to the next match; the buffer resets after
        TYPEAHEAD_TIMEOUT seconds of pause. Returns True when the key
        was consumed."""
        # on_key_down reports raw VK codes: letters are ALWAYS 65..90
        # ('A'..'Z' - lowercase never appears), digits are 48..57 (top
        # row). 96..111 are the NUMPAD keys - they must not be taken
        # for letters (VK_NUMPAD1=97 would collide with ord('a'))
        is_letter = ord('A') <= key <= ord('Z')
        is_digit = ord('0') <= key <= ord('9')
        if not (is_letter or is_digit):
            self._ta_buf = ''  # any other key restarts the search
            return False
        if self._filter_edit_focused() or not self.tree:
            return False

        ch = chr(key).lower()
        now = time.time()
        if now - self._ta_time > self.TYPEAHEAD_TIMEOUT:
            self._ta_buf = ''
        self._ta_time = now

        if self._ta_buf != ch:
            # extend the buffer when the longer text still matches
            # something ("m" + "y" -> "my"); when it doesn't, the buffer
            # stays as it was and the same letter keeps cycling
            if self._typeahead_match(self._ta_buf + ch) is not None:
                self._ta_buf += ch

        if self._typeahead_match(self._ta_buf, select=True) is None:
            msg_status(_('No match in this folder: ') + self._ta_buf)
        return True

    def get_projects_dir(self):
        """The default folder for new .cuda-proj files (the option
        "projects_dir", empty by default). Supports the $settings
        prefix - the CudaText settings folder - e.g. "$settings" or
        "$settings/projects"; '~' is expanded too. A missing folder is
        created. Returns '' when the option is empty or cannot be
        resolved (the dialogs then use the last used folder as before)."""
        s = (self.options.get('projects_dir') or '').strip()
        if not s:
            return ''
        if '$settings' in s:
            s = s.replace('$settings', app_path(APP_DIR_SETTINGS))
        s = os.path.expanduser(s)
        s = os.path.normpath(s)
        try:
            if not os.path.isdir(s):
                os.makedirs(s, exist_ok=True)
        except OSError as ex:
            msg_status(_('Project Manager: cannot create projects folder: ')
                       + s + ' (' + str(ex) + ')')
            return ''
        return s

    def action_new_project(self, info=None):
        """Create a new project. Prompts for save location, creates an empty
        session called 'session1' inside the project, and loads it."""

        path = info
        if path is None:
            # the configured default projects folder, when set: the save
            # dialog opens directly in it (the user just types a name)
            path = dlg_file(False, "", self.get_projects_dir(),
                            PROJECT_DIALOG_FILTER)
        if not path:
            return

        path = Path(path)
        if path.suffix != PROJECT_EXTENSION:
            path = path.parent / (path.name + PROJECT_EXTENSION)

        if (self.project_file_path
        and os.path.normpath(str(self.project_file_path)) == os.path.normpath(str(path))):
            msg_status(_('Project is already open'))
            return

        self._create_project_file(path, close_tabs=True)
        msg_status(_("Created project: ") + '"' + os.path.basename(str(path)) + '"')

    @_with_backup_cause('on project switch')
    def _create_project_file(self, path, close_tabs=True):
        """Create a new project file at `path` and make it the current
        project. Saves the current project (and its session slot) first.
        With close_tabs=True all tabs are closed and session 'session1'
        starts empty (used by New project); with close_tabs=False the
        current tabs are embedded as session 'session1' (used by
        'New project from open tabs...')."""

        # The command can run with the side panel not created yet: the
        # plugin was lazy-loaded by the menu command itself and the panel
        # was never shown (issue #6582). The new project must be visible
        # in the panel, and the code below reads the panel tree (saving
        # the previous project, refresh) — create the panel first.
        if not self.tree:
            self.init_panel()

        path_str = str(path)

        # Save the current project and its session slot before switching
        had_project = bool(self.project_file_path)
        # Abort-revert target for the load below: the session owning the
        # current tabs (the old project's slot / the app session file),
        # captured before the flow re-points it
        old_ptr = self._current_state_session_ptr()
        if had_project:
            self.action_save_project_as(self.project_file_path)
            # (action_save_project_as -> session_save_as saves the CURRENT
            # session into its slot synchronously; the explicit extra
            # PROC_SAVE_SESSION which was here rewrote the whole big project
            # file a second time for the same state)

        if close_tabs:
            # Save the current state into the session it belongs to (the
            # old project's slot / the app session file), then only clear
            # the modified flags: the closing of the old tabs is left to
            # the PROC_LOAD_SESSION(sess1) below (session1 does not exist
            # in the new project file yet, so the load closes the tabs and
            # restores nothing). The load's closes run under
            # APPSTATE_SESSION_LOAD_BEGIN_PRE, so plugins like cuda_differ
            # keep the closing tabs' per-session registrations.
            saved = self._save_session_before_close_tabs(already_saved=had_project)
            if saved:
                self._clear_modified_all_tabs()

        # Reset project state
        self.project = normalize_project_data(dict(nodes=[]))
        self.project_file_path = path
        self.update_global_data()
        self.goto_history = []
        self.cur_dir = ''

        app_proc(PROC_SET_FOLDER, '')
        app_proc(PROC_SET_PROJECT, path_str)

        # Save the initial project file (current format version)
        d = copy.deepcopy(self.project)
        d['version'] = PROJECT_FORMAT_VERSION
        d['last_session'] = 'session1'
        write_json_file(path, d)

        # Create session1 inside the project and set as active
        sess1 = path_str + '|/sessions/session1'
        if close_tabs:
            # Point the app session at session1 BEFORE the load (the
            # APPSTATE_SESSION_LOAD the load fires must find the pointer
            # inside the project, else on_state closes the project). The
            # load of the not-yet-existing session1 closes the old tabs
            # and restores none of them; the save below then stores the
            # now-empty state as session1. (An empty target session also
            # resets the tab-group layout to the default single group —
            # acceptable for a brand-new empty project.)
            self._set_session(sess1)
            # On an ABORTED load (Cancel in 'Save modified tab?' -- only
            # reachable when the state save failed, so modified flags
            # stayed) skip the save too: it would bake the still-open OLD
            # tabs into the new project's session1. The repair re-points
            # the app session to old_ptr, so the ~40 s session autosave
            # writes the tabs into their owner's slot instead.
            if self._load_session_repairing_abort(sess1, old_ptr,
                    _('Tab closing cancelled; old tabs kept')):
                app_proc(PROC_SAVE_SESSION, sess1)
        else:
            # 'Create project from open tabs': the current tabs BECOME
            # session1 — save them into it, then point the app session.
            app_proc(PROC_SAVE_SESSION, sess1)
            self._set_session(sess1)

        # refresh the in-memory project from the file on disk (it now
        # contains session1 and last_session)
        try:
            with open(path_str, 'r', encoding='utf8') as f:
                data = json.load(f)
            self.project = normalize_project_data(data)
        except (OSError, ValueError):
            pass
        self.project['last_session'] = 'session1'

        self.update_global_data()
        self.add_recent(path_str)
        self.options['on_start'] = True
        self.save_events()
        self.save_options()

        self.action_refresh()

    @_with_backup_cause('on project save')
    def action_save_session_as_project(self, info=None):
        """Save the current editor state as a NEW project: prompts for the
        .cuda-proj location, embeds the currently open tabs as session
        'session1', makes it the default session and switches to the new
        project. Open tabs are NOT closed — the current state becomes the
        new project's session. (Migration helper for the old workflow which
        kept sessions in external .cuda-session files.)"""

        path = info
        if path is None:
            # the configured default projects folder, when set: the save
            # dialog opens directly in it (the user just types a name)
            path = dlg_file(False, "", self.get_projects_dir(),
                            PROJECT_DIALOG_FILTER)
        if not path:
            return

        path = Path(path)
        if path.suffix != PROJECT_EXTENSION:
            path = path.parent / (path.name + PROJECT_EXTENSION)

        if (self.project_file_path
        and os.path.normpath(str(self.project_file_path)) == os.path.normpath(str(path))):
            msg_status(_('This file is already the current project'))
            return

        self._create_project_file(path, close_tabs=False)
        msg_status(_("Created project from current session: ") + os.path.basename(str(path)))

    @_with_backup_cause('on project save')
    def action_save_project(self):
        """Save project in place. Falls back to Save as... if no path yet."""
        if self.project_file_path:
            self.action_save_project_as(self.project_file_path)
        else:
            self.action_save_project_as()

    @_with_backup_cause('on project close')
    def action_close_project(self):
        """Close the current project: save session, close all tabs, load
        default.cuda-session (restoring, not overwriting, its tabs)."""

        # Saving the project also saves the current session into its slot
        # inside the project file
        had_project = bool(self.project_file_path)
        # Abort-revert target, captured before the project is forgotten:
        # with a project open, its session slot owns the current tabs
        # (without a project the app session already IS the DEF_SES the
        # flow switches to -- nothing to protect, no revert)
        old_ptr = self._current_state_session_ptr() if had_project else ''
        if had_project:
            self.action_save_project_as(self.project_file_path)

        # Save the current state into the session it belongs to (the
        # project's session slot / the app's session file), so unsaved text
        # is not lost. skip_session_save: the save just happened inside
        # action_save_project_as, no tab could change since (synchronous
        # code) — a second PROC_SAVE_SESSION would rewrite the whole big
        # project file again for the same state. The CLOSING of the tabs is
        # left to the default-session load below.
        saved = self._save_session_before_close_tabs(already_saved=had_project)

        # Forget the project BEFORE touching the app session: the
        # _set_session/PROC_LOAD_SESSION calls below fire app state events,
        # and the on_state handler must not see a foreign session with a
        # still-open project.
        self._close_project_silent()

        # No project is loaded anymore: the plugin must not load at the
        # next CudaText start. Remove the on_start subscription from
        # plugins.ini [events] (a later open/create/save of a project
        # subscribes it again).
        self._update_on_start_subscription(False)

        # Load default session via PROC_LOAD_SESSION (reads, doesn't
        # overwrite). The load itself closes the old tabs (all groups,
        # pinned included) under APPSTATE_SESSION_LOAD_BEGIN_PRE, so
        # plugins like cuda_differ keep the closing tabs' per-session
        # registrations -- the project's tabs, compare tabs included, come
        # fully back when the project is opened again. Clear the modified
        # flags first (the state is in its session now): a tab still marked
        # modified would make the load ask 'Save modified tab?', and a
        # Cancel there aborts the load -- _load_session_repairing_abort
        # then re-points the app session back to the project's slot, so
        # the ~40 s session autosave cannot write the still-open project
        # tabs into default.cuda-session.
        self._set_session(DEF_SES)
        default_sess_path = os.path.join(app_path(APP_DIR_SETTINGS), DEF_SES)
        aborted = False
        if os.path.isfile(default_sess_path):
            if saved:
                self._clear_modified_all_tabs()
            ok = self._load_session_repairing_abort(default_sess_path,
                old_ptr, _('Project close cancelled; tabs kept'))
            aborted = (not ok) and bool(ed_handles())
        else:
            # No default session file: PROC_LOAD_SESSION exits BEFORE
            # closing any tab (the file must exist), so close here.
            self.close_all_tabs(silent=saved)
            if ed_handles():
                # The close was cancelled in its own confirm: same repair
                # as a load abort -- the still-open tabs belong to old_ptr,
                # and the autosave must not write them into DEF_SES.
                if old_ptr:
                    self._set_session(old_ptr)
                msg_status(_('Project close cancelled; tabs kept'))
                aborted = True

        if not aborted:
            msg_status(_("Project closed"))

    def _set_session(self, ptr):
        """app_proc(PROC_SET_SESSION, ...) wrapper for session changes made
        by the plugin itself. Sets a guard flag so the on_state handler
        doesn't treat our own change as an external session switch (which
        would close the project); see APPSTATE_SESSION_SET, issue #6366.
        Also records project sessions as the project's "last_session" —
        the session loaded the next time the project is opened."""
        self._session_set_by_plugin = True
        try:
            app_proc(PROC_SET_SESSION, ptr)
        finally:
            self._session_set_by_plugin = False
        self._remember_last_session(ptr)

    def _remember_last_session(self, ptr):
        """Store the session named by `ptr` as the project's "last_session"
        (in memory and in the project file on disk), so the project reopens
        with the session that was used last.
        Only INTERNAL session pointers (own .cuda-proj path +
        "|/sessions/" + name) are recorded — external session pointers
        (e.g. the default cuda-session) never change, and never overwrite,
        the project's last used session. Also drops the removed
        "def_session" key still present in older project files.

        The file update is SURGICAL when possible (_set_last_session_in_file):
        a .cuda-proj with big unsaved/untitled tabs embeds their full texts
        (tens of MB), and this runs after every session switch — the old
        json.load+json.dump of the whole file cost seconds per switch just
        to change one string value."""
        fn_proj = str(self.project_file_path) if self.project_file_path else ''
        if not fn_proj or '|/sessions/' not in ptr:
            return
        # tolerate pointers whose file part carries no directory (history.json
        # stores settings-dir files that way) and compare paths like
        # SameFileName does, instead of raw startswith (issue #6591)
        ptr = resolve_session_pointer(ptr)
        ptr_file, _, name = ptr.partition('|/sessions/')
        if not name:
            return
        if not same_path(ptr_file, fn_proj):
            return
        if not self._set_last_session_in_file(fn_proj, name):
            # surgical edit not possible: full parse + rewrite (also handles
            # the 'def_session' migration of old files and inserts the key
            # into files which don't have it yet)
            try:
                with open(fn_proj, 'r', encoding='utf8') as f:
                    data = json.load(f)
                if data.get('last_session') != name or 'def_session' in data:
                    data['last_session'] = name
                    data.pop('def_session', None)
                    write_json_file(fn_proj, data)
            except (OSError, ValueError):
                pass
        self.project['last_session'] = name

    def _set_last_session_in_file(self, fn, name):
        """Replace the value of the top-level 'last_session' key in the JSON
        file `fn` with `name`, WITHOUT parsing and re-serializing the whole
        file: the new text is built from the old text by swapping the string
        literal after the key. On a .cuda-proj which embeds session texts
        (big unsaved tabs) this is much faster than json.load+json.dump, and
        it runs after every session switch.

        Returns True when the file is (or already was) in the wanted state;
        False when the caller must fall back to the full parse+rewrite:
        unreadable file, name needing JSON escaping, 'last_session' key
        absent (nothing to replace), key not exactly once / not followed
        by a string value (corrupt, hand-written or exotic JSON: a string
        value equal to the key text also matches the scan), legacy
        'def_session' key present, write error.
        The write is atomic (tmp+replace), like write_json_file."""
        try:
            with open(fn, 'r', encoding='utf8') as f:
                text = f.read()
        except OSError:
            return False

        # only fast-path names which need no JSON escaping (session names
        # are restricted to safe ASCII by is_session_name, but a hand-edited
        # project file can contain anything)
        if json.dumps(name)[1:-1] != name:
            return False

        occ = _find_structural_keys(text, '"last_session"')
        if len(occ) != 1:
            # zero: no key yet (insert needs a structural edit, the caller's
            # full rewrite does it); 2+: duplicated key or exotic content
            return False
        if _find_structural_keys(text, '"def_session"'):
            # legacy key to remove as well: full rewrite path handles both
            return False

        n = len(text)
        i = occ[0] + len('"last_session"')
        while i < n and text[i] in ' \t\r\n':
            i += 1
        if i >= n or text[i] != ':':
            return False
        i += 1
        while i < n and text[i] in ' \t\r\n':
            i += 1
        if i >= n or text[i] != '"':
            return False # not a string value

        # end of the value string: next structural quote
        j = text.find('"', i + 1)
        while j != -1 and not _quote_is_structural(text, j):
            j = text.find('"', j + 1)
        if j == -1:
            return False

        if text[i + 1:j] == name:
            return True # already the wanted value: no write at all

        new_text = text[:i + 1] + name + text[j:]
        try:
            _write_text_file_atomic(fn, new_text)
        except OSError:
            return False
        return True

    @staticmethod
    def _read_session_keys(path):
        """Read the 'sessions' and 'last_session' keys from a .cuda-proj
        file on disk. Returns a dict holding only the keys that were found
        with valid types; {} when the file cannot be read or parsed.
        CudaText writes session sub-keys straight into the project file
        (PROC_SAVE_SESSION on session switches, the ~40 s session
        autosave, the save on exit), bypassing every in-memory copy —
        the file on disk is the only up-to-date source for them."""
        try:
            with open(path, 'r', encoding='utf8') as f:
                data = json.load(f)
        except (OSError, ValueError):
            return {}
        res = {}
        if isinstance(data, dict):
            if isinstance(data.get('sessions'), dict):
                res['sessions'] = data['sessions']
            if isinstance(data.get('last_session'), str):
                res['last_session'] = data['last_session']
        return res

    def _sync_sessions_from_disk(self):
        """Refresh the in-memory project's 'sessions' and 'last_session'
        from the project file on disk. Must be called after anything
        saves a session into the file (PROC_SAVE_SESSION), so that a
        later project-file save can never write stale session data back
        over the file — that reverted every non-current session to an
        old state and silently lost typed text (found via on_exit)."""
        if not self.project_file_path:
            return
        fresh = self._read_session_keys(str(self.project_file_path))
        if 'sessions' in fresh:
            self.project['sessions'] = fresh['sessions']
        if 'last_session' in fresh:
            self.project['last_session'] = fresh['last_session']

    def _close_project_silent(self):
        """Reset project state without touching tabs or sessions. Used when
        an external session switch is detected — the foreign session is now
        active and we must detach from the project without disrupting it."""
        self.project = normalize_project_data(dict(nodes=[]))
        self.project_file_path = None
        self.update_global_data()
        self.goto_history = []
        self.cur_dir = ''

        app_proc(PROC_SET_FOLDER, '')
        app_proc(PROC_SET_PROJECT, '')

        self.action_refresh()

    @_with_backup_cause('on project switch')
    def action_open_project(self, info=None, load_session=True):
        had_project = bool(self.project_file_path)
        # Abort-revert target for the load at the bottom: the session
        # owning the current tabs (the old project's slot / the app
        # session file), captured before the flow re-points it
        old_ptr = self._current_state_session_ptr()
        # Name of the session auto-imported into an old-format project
        # file in the load_session block below ('' = no import); see
        # _auto_import_current_session (issue #6600)
        imported_name = ''
        # Outcome of the on-disk upgrade of an old-format file:
        # (rewritten_on_disk, backup_path_or_None); reported to the user
        # by _show_upgrade_v2_dialog after the open finishes
        upgrade_info = None
        if had_project:
            self.action_save_project_as(self.project_file_path)

        path = info
        if path is None:
            path = dlg_file(True, "", "", PROJECT_DIALOG_FILTER)
        if not path:
            return

        # Make the path absolute (callers like on_open_pre or the command
        # line can pass a relative one). project_file_path, the recent list,
        # PROC_SET_PROJECT and every session pointer built below must be
        # absolute: the app's session-pointer comparisons
        # (session_cur_name, _remember_last_session, on_start vs history.json)
        # fail on relative paths (issue #6591).
        path = os.path.abspath(path)

        if not Path(path).exists():
            msg_status(_("Project file not found: ") + collapse_filename(str(path)))
            return

        proj_dir = os.path.dirname(path)

        # Load and validate the project file BEFORE closing any tab, so a
        # broken file leaves the editor state untouched
        print(_('Loading project: ') + '"' + collapse_filename(path) + '"')
        data, err = load_project_file(path)
        if err:
            self._report_project_load_error(path, err)
            return

        ver = data.get('version', 1)
        if not isinstance(ver, int):
            ver = 1
        if ver > PROJECT_FORMAT_VERSION:
            msg_box(
                _('Cannot open project:\n%s\n\nThe file uses project format version %d,\nbut this Project Manager supports only version %d.\nPlease update CudaText and its plugins.') %
                    (collapse_filename(str(path)), ver, PROJECT_FORMAT_VERSION),
                MB_OK + MB_ICONERROR)
            return

        old_format = ver < PROJECT_FORMAT_VERSION
        if old_format:
            # Old format: normalize the structure in memory, then upgrade the
            # file on disk automatically. The upgrade only adds missing
            # standard keys and a backup of the original is kept next to the
            # file, so it is lossless and reversible — the open is never
            # blocked or confirmed (a blocking prompt would only prevent
            # users from opening their own projects; the file is rewritten
            # on exit anyway). An INFORMATIONAL dialog about the upgrade is
            # shown after the project is fully open, see
            # _show_upgrade_v2_dialog below.
            data = normalize_project_data(data)
            self._migrate_last_session(data)
            upgrade_info = self.upgrade_project_file_on_disk(path, data)
        else:
            data = normalize_project_data(data)
            self._migrate_last_session(data)

        # "Open project" can run with the side panel not created yet: the
        # command was called from the "Plugins" menu while the panel was
        # never shown (issue #6582). The opened project must appear in the
        # panel, and the code below reads the panel tree (unfolds,
        # refresh, context menu) — create the panel first.
        if not self.tree:
            self.init_panel()

        # Save the current session to its slot, then prepare the tabs for
        # the closing load below. Skipped when load_session=False (on_start
        # when CudaText already loaded the project session from
        # history.json).
        # skip_session_save: with a project open, action_save_project_as
        # above saved the current session into its slot already (same
        # synchronous call, no tab could change since); without a project
        # the state goes to the app session file as before.
        if load_session:
            saved = self._save_session_before_close_tabs(already_saved=had_project)
            # Old-workflow migration (issue #6600): an old-format project
            # file with NO internal sessions, opened with NO project open
            # before it — the current tabs belong to the app's external
            # session (default.cuda-session or another .cuda-session),
            # exactly the state the old (pre-versioning) workflow kept
            # next to the project. Embed it as the project's first
            # internal session instead of dropping the user into an
            # empty 'session1' — the tabs then simply stay open, and no
            # manual 'Import external session...' is needed.
            # NOT when another project was open before (had_project):
            # those tabs belong to THAT project's session slot, importing
            # them here would bake the wrong tab set into this project
            # (the app session is also not an external file then, so
            # 'save the loaded session to its external file' would
            # violate the plugin's rules). NOT when the editor is empty
            # (nothing to import; a plain 'session1' below keeps its
            # standard name instead of a confusing empty '...
            # auto-imported' one).
            if old_format and not data.get('sessions') and not had_project:
                imported_name = self._auto_import_current_session(path, data, saved)
            if imported_name:
                # The current tabs ARE the imported session now: nothing
                # is closed here, and the modified flags stay as they
                # are — no load below will close these tabs either (the
                # flow only re-points the app session at the imported
                # session), so the flags keep matching the live tabs.
                pass
            elif data.get('sessions'):
                # The project has sessions: DON'T close the old tabs here.
                # The PROC_LOAD_SESSION at the bottom of this method closes
                # them itself (its DoFileCloseAll runs under
                # APPSTATE_SESSION_LOAD_BEGIN_PRE, issue #6496), so plugins
                # like cuda_differ can tell these closes from user closes
                # and keep the closing tabs' per-session registrations -- a
                # compare tab created in the old project then comes fully
                # back (toolbar, overview, title color) when that project's
                # session is loaded again. Only clear the modified flags
                # here: the state is stored in its session now, and a tab
                # still marked modified would make the load ask
                # 'Save modified tab?' -- where Cancel aborts the whole
                # switch with the session pointer already re-pointed.
                if saved:
                    self._clear_modified_all_tabs()
            else:
                # Legacy session-less project file: no session exists to
                # load, so no load would close the old tabs -- close them
                # here (the current state was saved into its session first,
                # so unsaved text is not lost).
                self.close_all_tabs(silent=saved)

        # Adopt the validated project
        self.project = data
        for i in range(len(self.project['nodes'])):
            node = self.project['nodes'][i]
            # normalize os separators
            node = os.path.normpath(node)
            self.project['nodes'][i] = expand_macros(proj_dir, node)

        # detect orphan items (only reported, not removed)
        bads = [fn for fn in self.project["nodes"] if not os.path.exists(fn)]

        self.project_file_path = Path(path)
        self.add_recent(path)
        self.action_refresh()
        self.options['on_start'] = True
        self.save_events()
        self.save_options()

        self.update_global_data()
        self.goto_history = []

        for fn in self.project["nodes"]:
            if os.path.isdir(fn):
                app_proc(PROC_SET_FOLDER, fn)
                self.cur_dir = fn
                break

        app_proc(PROC_SET_PROJECT, path)

        s = _("Opened project: ") + '"' + os.path.basename(path) + '"'
        if bads:
            s += ', ' + _('%d missing item(s)')%len(bads)
        msg_status(s)

        if load_session:
            names = list(self.project.get('sessions', {}).keys())
            if imported_name:
                # The current state was embedded as the imported session
                # moments ago: re-point the app session WITHOUT a load —
                # the state on screen already IS that session's state
                # (PROC_SAVE_SESSION captured it from the live tabs), so a
                # load would only close and reopen the identical tabs,
                # and its 'Save modified tab?' confirm could abort the
                # switch for no benefit. Same pattern as 'Create new
                # project from open tabs...': the open tabs become the
                # project's session, nothing flickers, and the ~40 s
                # session autosave keeps writing them into the project
                # file from now on.
                self._set_session(str(path) + '|/sessions/' + imported_name)
            elif names:
                # load the session that was used last (the first one when
                # the project has no "last_session" yet or it names a
                # session that no longer exists)
                sess = self.project.get('last_session', '')
                if sess not in names:
                    sess = names[0]
                sess_path = str(path) + '|/sessions/' + sess
                self._set_session(sess_path)
                # A cancelled load (Cancel in 'Save modified tab?', only
                # reachable when the slot save failed) is repaired by
                # re-pointing the app session to old_ptr: the still-open
                # old tabs then stay in their owner's slot, and this
                # project's session cannot be clobbered by the ~40 s
                # session autosave.
                self._load_session_repairing_abort(sess_path, old_ptr,
                    _('Project switch cancelled; old tabs kept'))
            else:
                # A project always keeps at least one session (a project is
                # itself a session): create 'session1' from the current
                # (empty) state, so the app session stays inside the project
                # and can never leak onto an external session file.
                sess_path = str(path) + '|/sessions/session1'
                app_proc(PROC_SAVE_SESSION, sess_path)
                self._set_session(sess_path)
                try:
                    with open(path, 'r', encoding='utf8') as f:
                        self.project['sessions'] = json.load(f).get('sessions', {})
                except (OSError, ValueError):
                    pass
        elif not self.project.get('sessions'):
            # Startup path (load_session=False): CudaText is restoring the
            # project's session from history.json itself. If the file has
            # no sessions at all (left over by old builds), create
            # 'session1' from the restored state now — a project is itself
            # a session, it always keeps one (otherwise the ~40 s session
            # autosave would write the app session pointer's sub-key into
            # the file only much later, and the sessions menu would be
            # empty meanwhile).
            sess_path = str(path) + '|/sessions/session1'
            if app_proc(PROC_SAVE_SESSION, sess_path):
                self._set_session(sess_path)
                self._sync_sessions_from_disk()
        if 'unfold' in self.project:
            unfolds = self.project['unfold']
            for i in range(len(unfolds)):
                unfolds[i] = expand_macros(proj_dir, unfolds[i])
            self.enum_all_setfolds(unfolds)

        if old_format:
            # The file was just upgraded from the old format, and the project
            # is open with its session loaded now — the user can SEE that
            # everything works. Tell them what was done, and about the one
            # breaking change of v2, with the migration guide one click
            # away. Issue #6600 taught that users don't re-read a plugin's
            # documentation after an update: the information must come to
            # them, or the v2 session rule looks like 'tabs lost' /
            # 'the project closed itself' and lands in the issue tracker.
            self._show_upgrade_v2_dialog(path, upgrade_info, imported_name)

    @staticmethod
    def _migrate_last_session(data):
        """Migrate the removed 'def_session' key (written by earlier
        builds) to 'last_session'. Files with neither key get their
        last_session filled when a session is switched to / the project
        is opened."""
        if 'def_session' in data:
            # 'not data.get(...)' also covers the '' normalize may have
            # just inserted for a missing key
            if not data.get('last_session'):
                data['last_session'] = data['def_session']
            del data['def_session']
        if not isinstance(data.get('last_session'), str):
            data['last_session'] = ''

    def upgrade_project_file_on_disk(self, path, data):
        """Rewrite an old-format (version-less) project file in the current
        format, after normalize_project_data() was applied to `data`.

        A backup of the original file is saved next to it first (see
        make_project_backup). When the backup or the write fails (read-only
        folder, locked file, ...), the project is still opened with the
        upgraded data in memory and the file on disk is upgraded on the
        first successful save — opening a project must never be blocked.
        The user is informed via the status bar and the console panel.

        Returns (rewritten_on_disk, backup_path): rewritten_on_disk tells
        whether the file on disk carries the new format now (False = it
        keeps the old format until the first successful save),
        backup_path is the .bak copy just written or None. Consumed by
        _show_upgrade_v2_dialog to report the exact outcome.
        """
        data['version'] = PROJECT_FORMAT_VERSION

        bak = make_project_backup(path)
        if bak is None:
            msg_status(_('Project opened; its file could not be upgraded on disk'))
            print(_('Project Manager: cannot back up old project file, '
                'the file on disk was not upgraded: ') + collapse_filename(path))
            print(_('Project Manager: it will get the current format on the first successful save'))
            return False, None

        try:
            write_json_file(path, data)
        except OSError as ex:
            msg_status(_('Project opened; its file could not be upgraded on disk'))
            print(_('Project Manager: cannot upgrade project file on disk: %s') % ex)
            print(_('Project Manager: the original file is kept in the backup: ') +
                collapse_filename(str(bak)))
            print(_('Project Manager: it will get the current format on the first successful save'))
            return False, bak

        msg_status(_('Project file upgraded to v%d (backup: %s)') %
            (PROJECT_FORMAT_VERSION, os.path.basename(bak)))
        print(_('Project Manager: old project file was upgraded to format v%d: ') %
            PROJECT_FORMAT_VERSION + collapse_filename(path))
        print(_('Project Manager: backup of the original saved as: ') +
            collapse_filename(str(bak)))
        print(_('Project Manager: you can delete the backup file if everything works fine'))

        # tip for the old (pre-versioning) workflow: a sibling external
        # session file can be embedded into the project
        ses = Path(path).with_suffix('.cuda-session')
        if ses.exists():
            print(_('Project Manager: found an external session file: ') +
                collapse_filename(str(ses)))
            print(_('Project Manager: to embed it into the project, use '
                '"Import external session..." in the sessions menu'))

        return True, bak

    def _auto_import_current_session(self, path, data, ext_saved):
        """Import the CURRENT editor state into the session-less project
        being opened, as its first internal session. Migration helper for
        the old (pre-versioning) workflow, issue #6600: the user worked
        with an external session file (default.cuda-session or another
        .cuda-session) next to the project; that state now becomes a
        session of the project itself, so it loads with the project from
        now on and no manual "Import external session..." is needed.

        Called from action_open_project ONLY when the project file has no
        internal sessions, no project was open before this open (the
        current tabs belong to the app's external session), and the
        editor has tabs. The caller saved the state into its external
        session file first (_save_session_before_close_tabs, right
        before this call): `ext_saved` reports whether THAT succeeded,
        so the external .cuda-session file ends up holding the final
        state too and nothing is lost on either side.

        The new session is named after its source: the external session
        file's base name ('default.cuda-session' -> 'default', the OS
        suffix stripped) plus AUTO_IMPORT_TAG -> 'default auto-imported';
        a name that already exists gets a number appended
        ('... auto-imported 2'). Returns the created session's name, or
        '' when there was nothing to import (no tabs open) or the save
        failed — the caller then falls back to the legacy empty-
        'session1' flow.

        The capture is done by the app itself (PROC_SAVE_SESSION into
        the project's session pointer), not by embedding the external
        file's JSON: the app's save stores the LIVE state — including
        the text of modified and untitled tabs — so the import is
        lossless even when the external save (ext_saved) failed, and
        writing straight into the project file also re-reads whatever
        is on disk, keeping the just-upgraded file's other keys intact.
        """
        if not ed_handles():
            # nothing to import: keep the plain 'session1' name for the
            # empty session created by the legacy flow (an empty
            # '... auto-imported' session would be a confusing name)
            return ''

        # session name from the app's CURRENT session pointer (an
        # external session file: its base name; a pointer into some
        # project file -- a foreign session left active by the session
        # manager while no project is open: that session's name)
        ptr = app_path(APP_FILE_SESSION)
        base = ''
        ext_fn = ''    # external session file to report, '' when none
        if ptr:
            if '|/sessions/' in ptr:
                base = ptr.split('|/sessions/', 1)[1]
            else:
                ext_fn = ptr
                base = os.path.splitext(os.path.basename(ext_fn))[0]
                # 'default' + OS_SUFFIX + '.cuda-session' would make an
                # ugly session name ('default_linux_'): strip the suffix
                if OS_SUFFIX and base.endswith(OS_SUFFIX):
                    base = base[:-len(OS_SUFFIX)]
                # a pointer without a directory part names a file in the
                # settings dir (how the app itself stores it) -- show it
                # with its folder in the report below
                if not os.path.dirname(ext_fn):
                    ext_fn = os.path.join(app_path(APP_DIR_SETTINGS), ext_fn)
        allowed = string.ascii_letters + string.digits + '., ()-+_$%='
        base = ''.join(ch if ch in allowed else '_' for ch in base).strip()
        if not base:
            # no session pointer at all (session saving switched off):
            # still import the open tabs, under a generic name
            base = 'session'

        wanted = base + AUTO_IMPORT_TAG
        names = list(data.get('sessions', {}).keys())
        name = wanted
        n = 2
        while name in names:
            name = '%s %d' % (wanted, n)
            n += 1

        # save the CURRENT state into the project file as that session;
        # the file was just upgraded on disk (or the project still opens
        # with in-memory upgraded data when that failed — the app's
        # read-modify-write works on whatever JSON is there, and the
        # same failure would make this save fail too, falling back to
        # the legacy flow)
        sess = str(path) + '|/sessions/' + name
        if not app_proc(PROC_SAVE_SESSION, sess):
            print(_('Project Manager: could not import the current session '
                'into the project file; an empty session will be created instead'))
            return ''

        # refresh the project data being adopted: session names come
        # from the file on disk (written by the app a moment ago; the
        # session DATA in the mirror is never trusted anywhere, but the
        # names must be exact), and the imported session becomes the
        # one the project loads
        fresh = self._read_session_keys(str(path))
        if 'sessions' in fresh:
            data['sessions'] = fresh['sessions']
        else:
            data.setdefault('sessions', {})[name] = {}
        data['last_session'] = name

        # rotating backup: APPSTATE_SESSION_SAVE_AFTER (fired by the
        # save above) records only the OPEN project's file, and this
        # project is not adopted yet -- record it manually, so the
        # imported file gets its backup when the open command ends.
        # Parity with the legacy empty-session1 flow, whose
        # PROC_SAVE_SESSION runs after the adoption and is recorded by
        # the event itself.
        if self._backup_flow:
            p = os.path.normpath(str(path))
            if p not in self._backup_pending:
                self._backup_pending.append(p)

        msg_status(_('Imported session into the project: ') + name)
        print(_('Project Manager: old project file had no sessions, '
            'the current session was imported automatically as "%s"') % name)
        if ext_fn:
            if ext_saved:
                print(_('Project Manager: the external session file is kept '
                    '(updated with the final state, not deleted): ') +
                    collapse_filename(ext_fn))
            else:
                print(_('Project Manager: could not update the external '
                    'session file; the current tabs are stored in the '
                    'imported project session: ') + collapse_filename(ext_fn))
        return name

    def _show_upgrade_v2_dialog(self, path, upgrade_info, imported_name):
        """Show the one-time information dialog after an old-format project
        file was upgraded to v2 and the project finished opening (issue
        #6600: users don't re-read a plugin's documentation after an
        update, so the information must come to them — without it, the v2
        session rule looks like 'tabs lost' / 'the project closed itself'
        and lands in the issue tracker).

        Called at the END of action_open_project, after the project is open
        and its session is loaded, so the user first SEES the working end
        state. Shown once per project file: after a successful on-disk
        upgrade the file carries "version": 2 and is never old-format
        again; a file whose on-disk rewrite failed keeps the old format,
        so the dialog honestly reappears on the next open (the upgrade is
        still pending then).

        The message reports only what actually happened: the automatic
        upgrade, the backup file name (or why the file was not rewritten),
        the auto-imported session when one was created, and the ONE
        breaking change users notice (a project and an external session
        can no longer be open at the same time). 'Open documentation'
        opens the dedicated migration guide (readme/upgrade_v2.txt,
        DOC_FILE_V2) in a new tab -- the whole file is about the upgrade,
        so it opens at the top, no scrolling; 'OK' just closes the dialog.
        """
        written, bak = upgrade_info
        fn = os.path.basename(path)

        parts = [
            _('Project "%s" was automatically upgraded to the new file format v2.') % fn,
            _('Nothing is required from you: the project is open and your tabs are restored.'),
            '',
        ]
        if written:
            parts.append(_('A backup of the original file was saved next to it: "%s".') %
                os.path.basename(str(bak)))
            parts.append(_('You can delete the backup when everything works fine.'))
        elif bak is not None:
            parts.append(_('The original file was backed up as "%s", but the project file could not be rewritten (locked file? read-only folder?).') %
                os.path.basename(str(bak)))
            parts.append(_('The original is kept; it will get the new format on the first successful save.'))
        else:
            parts.append(_('The project file could not be backed up or rewritten (read-only folder? locked file?).'))
            parts.append(_('The original is untouched; it will get the new format on the first successful save.'))
        if imported_name:
            parts.append(_('Your open tabs were saved as the project session "%s", inside the project file: from now on they load with the project.') %
                imported_name)

        parts.append('')
        parts.append(_('Attention — one breaking change in v2: the session is now always stored INSIDE the project file.'))
        parts.append(_('A project and an external session (e.g. default.cuda-session) can no longer be open at the same time.'))
        parts.append(_('This rule fixed real bugs of lost and overwritten sessions. External .cuda-session files are never deleted: "Import external session..." embeds one into a project.'))
        parts.append('')
        parts.append(_('The details — why the change, which bugs it fixes, and how to get an external session back into a project — are in the migration guide "%s" (in the plugin readme folder).') %
            DOC_FILE_V2)

        res = msg_box_ex(
            _('Project Manager'),
            '\n'.join(parts),
            [_('Open documentation'), _('OK')],
            MB_ICONINFO)
        if res == 0:
            self._open_plugin_doc_v2()

    def _open_plugin_doc_v2(self):
        """Open the plugin's v2 migration guide in a new editor tab — the
        'Open documentation' button of the v2-upgrade dialog calls this.

        The guide is a SEPARATE file, readme/upgrade_v2.txt (DOC_FILE_V2):
        its whole content is the v2 story, so it simply opens at the top —
        no section lookup or caret scrolling inside the long readme.txt
        (which keeps only a short summary section pointing here). When the
        file is missing (an exotic install?), a status-bar message names
        the missing path instead of failing silently.
        """
        fn = os.path.join(os.path.dirname(__file__), 'readme', DOC_FILE_V2)
        if not os.path.isfile(fn):
            msg_status(_('Documentation file not found: ') + str(fn))
            return
        file_open(fn)

    def _report_project_load_error(self, path, err):
        """Show a dialog for a project file that failed validation.
        An empty (0-byte) file is special-cased: it holds no data, so the
        user is offered to create a new empty project in it."""

        err_kind = err.split(':', 1)[0]
        err_detail = err.split(':', 1)[1] if ':' in err else ''

        if err_kind == 'empty':
            if msg_box(
                _('The project file is empty:\n%s\n\nCreate a new empty project in this file?') %
                    collapse_filename(str(path)),
                MB_OKCANCEL + MB_ICONQUESTION) == ID_OK:
                self._create_project_file(Path(path), close_tabs=True)
            else:
                msg_status(_('No project open'))
            return

        reasons = {
            'io': _('Cannot read the file.'),
            'json': _('The file does not contain valid JSON.'),
            'type': _('The file is not a CudaText project (a JSON object is expected).'),
            'nodes': _('The file is not a valid CudaText project ("nodes" must be a list of paths).'),
            'sessions': _('The file is not a valid CudaText project ("sessions" must be an object).'),
        }
        reason = reasons.get(err_kind, err)
        if err_detail:
            reason += '\n' + err_detail

        msg_box(_('Cannot open project file:\n%s\n\n%s') % (collapse_filename(str(path)), reason),
            MB_OK + MB_ICONERROR)

    def action_add_folder(self):
        fn = dlg_dir("")
        self.add_node(fn)

    def action_add_file(self):
        fn = dlg_file(True, "", "", "")
        self.add_node(fn)

    @_with_backup_cause('on project save')
    def action_remove_node(self):
        index = self.selected
        while True:
            prop = tree_proc(self.tree, TREE_ITEM_GET_PROPS, index)
            if prop["level"] == 0:
                return
            if prop["level"] == 1:
                path = prop["data"]
                break
            index = prop["parent"]

        tree_proc(self.tree, TREE_ITEM_DELETE, index)

        # find the SAVED node that matches the tree item's path: the tree
        # renders a root path as pathlib does ('D:\\' for a drive root,
        # '\\server\share' for a UNC root) while the saved nodes list
        # keeps the folder dialog's form ('D:\\', '\\server\share\') -
        # an exact compare failed for both root kinds (the old code
        # blindly stripped the trailing backslash for UNC and then broke
        # the drive roots: 'Stop tracking' on 'D:\' errored with
        # 'cannot find path "D:"' and the node returned on refresh).
        # Remove the entry that is ACTUALLY saved, so the removal sticks.
        node = None
        for n in self.project["nodes"]:
            if path_equals(n, path):
                node = n
                break

        if node is not None:
            self.project["nodes"].remove(node)
            self._fire_project_nodes_removed()
            # reset the 'current folder' if it was under the removed
            # node (trailing-separator-tolerant: 'D:\foo' IS under 'D:\')
            if path_contains(node, self.cur_dir):
                app_proc(PROC_SET_FOLDER, '')
                self.cur_dir = ''
        else:
            msg_box(_('Cannot find path "%s" in root nodes list') % path, MB_OK + MB_ICONERROR)

        if self.project_file_path:
            self.action_save_project_as(self.project_file_path)

    @_with_backup_cause('on project save')
    def action_remove_deleted_nodes(self):

        if msg_box(_("Remove missing paths from the project?"), MB_OKCANCEL + MB_ICONWARNING) != ID_OK:
            return

        nodes_deleted = []
        # iterate over a COPY: removing from the same list that is being
        # iterated skips every second matching entry (two adjacent missing
        # paths -> only the first one was removed)
        for node in list(self.project["nodes"]):
            path_ = Path(node)
            if not path_.exists():
                self.project["nodes"].remove(str(path_))
                nodes_deleted.append(str(path_))

        if len(nodes_deleted) > 0:
            self._fire_project_nodes_removed()
            self.action_refresh()

            if self.project_file_path:
                self.action_save_project_as(self.project_file_path)
            msg_status(_('Removed missing paths: ') + ', '.join(nodes_deleted))
        else:
            msg_status(_('No missing paths'))

    @_with_backup_cause('on project save')
    def action_set_as_main_file(self):

        path = self.get_location_by_index(self.selected)
        self.project["mainfile"] = str(path)
        self.update_global_data()

        if self.project_file_path:
            self.action_save_project_as(self.project_file_path)

    @_with_backup_cause('on project save')
    def action_save_project_as(self, path=None):

        need_refresh = path is None
        if path is None:
            # the configured default projects folder wins; without it the
            # dialog starts in the folder of the current project file
            proj_dir = self.get_projects_dir()
            if not proj_dir:
                if self.project_file_path:
                    proj_dir = str(self.project_file_path.parent)
                else:
                    proj_dir = ""
            path = dlg_file(False, "", proj_dir, PROJECT_DIALOG_FILTER)

        if path:
            proj_dir = os.path.dirname(str(path))

            path = Path(path)
            if path.suffix != PROJECT_EXTENSION:
                path = path.parent / (path.name + PROJECT_EXTENSION)

            # pre-processing of dict before saving
            # 'sessions' can hold tens of MB of embedded session texts (the
            # in-memory mirror); it is REPLACED below with data read fresh
            # from disk whenever a source file exists, and json.dump only
            # reads it — deep-copying that subtree was pure waste, seconds
            # on a project with big unsaved tabs (attach it by reference)
            sess_mirror = self.project.get('sessions')
            if not isinstance(sess_mirror, dict):
                sess_mirror = {}
            d = {k: copy.deepcopy(v) for k, v in self.project.items()
                 if k != 'sessions'}
            d['sessions'] = sess_mirror
            if 'nodes' in d:
                for i in range(len(d['nodes'])):
                    d['nodes'][i] = collapse_macros(proj_dir, d['nodes'][i])
            d.setdefault('nodes', [])

            unfolds = []
            self.enum_all_getfolds(unfolds)
            unfolds = list(map(lambda x: collapse_macros(proj_dir, x), unfolds))
            d['unfold'] = unfolds
            d['version'] = PROJECT_FORMAT_VERSION

            # 'sessions' and 'last_session' in the in-memory project can be
            # STALE: CudaText writes session sub-keys straight into the
            # project file on disk (PROC_SAVE_SESSION on session switches,
            # the ~40 s session autosave, the save on exit), and those
            # writes don't pass through this dict. Dumping the dict here
            # would revert every session except the current one to an old
            # state — silent data loss (text typed in a session before
            # switching away from it reappeared as the old text after a
            # CudaText restart). Always take the fresh values from the
            # file on disk; the in-memory copy is only the fallback for a
            # first-ever save. For Save-As to a new path, the sessions of
            # the OLD project file are carried over to the new file.
            old_path = str(self.project_file_path) if self.project_file_path else ''
            target = str(path)
            carry_name = ''
            if old_path and os.path.normpath(old_path) != os.path.normpath(target):
                src = old_path      # Save As: carry the open project's sessions
                # Save the active session into the OLD project first, so it
                # doesn't lag one state behind, and remember its name — the
                # current state must land in the new file under the SAME
                # session name, not a generic 'session1'
                cur_sess = self.session_cur_name()
                if cur_sess:
                    app_proc(PROC_SAVE_SESSION, old_path + '|/sessions/' + cur_sess)
                    carry_name = cur_sess
            elif os.path.isfile(target):
                src = target        # normal save: the file being rewritten
            else:
                src = ''
            if src:
                fresh = self._read_session_keys(src)
                if 'sessions' in fresh:
                    d['sessions'] = fresh['sessions']
                if 'last_session' in fresh:
                    d['last_session'] = fresh['last_session']

            # Point the plugin to the new path BEFORE saving the session,
            # so the session is embedded into the new file, not the old one
            self.project_file_path = path
            write_json_file(path, d)

            # Save the current session into the file. This round-trips the
            # JSON file on disk, so it runs AFTER the initial write — no
            # final rewrite with stale data (previously the last write
            # clobbered the just-saved session). _set_session inside also
            # persists "last_session".
            self.session_save_as(False, sess_name=carry_name)

            # any saving of project file makes on_start On: a project is
            # in use, so the plugin must load at the next CudaText start
            # (save_options persists the on_start flag for consistency;
            # plugins.ini is what CudaText actually reads on start)
            self.options['on_start'] = True
            self.save_events()
            self.save_options()

            self.update_global_data()
            print(_('Saving project: ') + '"' + collapse_filename(str(path)) + '"')
            msg_status(_("Project saved"))

            if need_refresh:
                self.add_recent(str(path))
                self.action_refresh()
                self.save_options()

    def _backup_file_reported(self, fn_proj, cause):
        """One AUTOMATIC backup of the project file fn_proj, with the
        console report line. The two callers: the on_state handler for
        session saves the plugin did NOT initiate (no decorated command
        running -- 'by timer' / the pinned 'on exit'), and the
        _with_backup_cause flush for the saves made inside a plugin
        command (one backup per touched project file, of the FINAL
        state -- the intermediate half-done states are skipped, see
        _with_backup_cause).

        `cause` labels the save which changed the file ('' = the app
        saved on its own). Returns the new backup path, or None when no
        backup was made (unchanged / disabled / failed -- failures are
        reported through the ProjectFileBackups log callback)."""
        if not fn_proj:
            return None
        if not self.options.get('backup_enabled', True):
            return None
        res = self.project_backups.run(
            fn_proj,
            folder=self.options.get('backup_folder', ''),
            count=self.options.get('backup_count', DEFAULT_BACKUP_COUNT),
            )
        if res:
            # console, NOT the status bar: the automatic backup can fire
            # every ~40 s while typing, a status bar line per backup
            # would be spam; the status bar is reserved for the manual
            # 'Backup project file now'
            print(_('Project Manager: project backup %s at %s: %s')
                  % (_(cause or 'by timer'), _time_am_pm(), res))
        return res

    def _backup_project_file(self, force=False):
        """Backup of the OPEN project file -- the MANUAL path (the menu
        command 'Backup project file now', force=True): no console
        report here, the command itself reports to the status bar. The
        automatic path is _backup_file_reported.

        force=True ignores the 'backup_enabled' option and a
        'backup_count' below 1 -- a manual snapshot is always allowed
        and always keeps the newest copy."""
        if not self.project_file_path:
            return None
        if not force and not self.options.get('backup_enabled', True):
            return None
        return self.project_backups.run(
            str(self.project_file_path),
            folder=self.options.get('backup_folder', ''),
            count=self.options.get('backup_count', DEFAULT_BACKUP_COUNT),
            force=force,
            )

    def action_backup_project_now(self):
        """Menu command 'Backup project file now': make one rotating
        backup of the open project file even when it did not change --
        a manual snapshot point (e.g. before risky edits).

        Works regardless of the backup options: with 'Backup project
        file when its sessions change' unchecked, and with 'Backups to
        keep' at 0 (a manual snapshot always keeps the single newest
        copy). Reports to the status bar -- the automatic backups
        report to the console only (they are too frequent for the
        status bar)."""
        if not self.project_file_path:
            msg_status(_('No project open'))
            return
        res = self._backup_project_file(force=True)
        if res:
            msg_status(_('Project backup: ') + res)
        else:
            msg_status(_('Project backup was not created (see console messages)'))

    def action_go_to_file(self):
        self.menu_goto()

    #----- Favorites (bookmarks) - see favorites.py -----

    def favorites_show(self):
        """Open the Favorites (bookmarks) window: projects, files and
        folders in one resizable manager dialog."""
        _favorites.show_favorites()

    def favorites_add_current_file(self):
        """Bookmark the file open in the editor."""
        _favorites.add_current_file()

    def favorites_add_current_dir(self):
        """Bookmark the folder of the file open in the editor."""
        _favorites.add_current_dir()

    def favorites_add_current_project(self):
        """Bookmark the currently opened project."""
        _favorites.add_current_project()

    def favorites_add_opened(self):
        """'Add opened...' - a dialog to choose between the opened
        project, the opened editor file, and the folder of the opened
        file (same chooser as the Add opened... button)."""
        _favorites.add_opened()

    def action_add_to_favorites(self):
        """Context-menu command: add the selected tree node (project,
        file or folder node) to favorites."""
        node = self.selected
        if node is None:
            return
        node_type = self.get_node_type()
        if node_type == NODE_PROJECT:
            path = str(self.project_file_path) if self.project_file_path else ''
            cat = 'projects'
        else:
            path = str(self.get_location_by_index(node))
            cat = 'folders' if node_type == NODE_DIR else 'files'
        if not path or path == '.':
            msg_status(_('Project is not saved yet, cannot add it to favorites'))
            return
        _favorites.add_favorite(cat, path)

    def _favorites_ctx(self, info=None):
        """Internal: callback of the favorites list context menu."""
        _favorites.context_action(info)

    def action_project_properties(self):
        self.config_proj()

    def action_config(self):
        self.config()

    def menu_cfg(self):
        """Toolbar button "Project sessions": show the session menu."""
        if self.h_menu_cfg is None:
            self.h_menu_cfg = menu_proc(0, MENU_CREATE)
        menu_proc(self.h_menu_cfg, MENU_CLEAR)

        self.fill_sessions_menu(self.h_menu_cfg)

        menu_proc(self.h_menu_cfg, MENU_SHOW)

    def fill_sessions_menu(self, menu):
        """Fill the given menu with the project-session commands: the list
        of the project's sessions plus save/delete/default/import items.
        Used both for the toolbar button menu (menu_cfg) and for the
        "Project sessions" submenu of the panel context menu."""
        cur_name = self.session_cur_name()

        names = self.session_get_names()
        if names:
            for (index, name) in enumerate(names):
                id = menu_proc(menu, MENU_ADD,
                    command="module=cuda_project_man;cmd=session_load;info=%s;"%name,
                    caption=_('Project session:')+' '+name
                    )
                if name==cur_name:
                    menu_proc(id, MENU_SET_ENABLED, command=False)
        else:
            id = menu_proc(menu, MENU_ADD, caption=_('Project session:'))
            menu_proc(id, MENU_SET_ENABLED, command=False)

        menu_proc(menu, MENU_ADD, caption='-')

        id = menu_proc(menu, MENU_ADD, command='cuda_project_man.session_save_inplace', caption=_('Save session'))
        if (not self.project_file_path) or (not cur_name):
            menu_proc(id, MENU_SET_ENABLED, command=False)

        id = menu_proc(menu, MENU_ADD, command='cuda_project_man.session_save_as', caption=_('Save session as...'))
        if not self.project_file_path:
            menu_proc(id, MENU_SET_ENABLED, command=False)

        id = menu_proc(menu, MENU_ADD, command='cuda_project_man.session_delete', caption=_('Delete session...'))
        # a project must always keep at least one session
        if len(names) < 2:
            menu_proc(id, MENU_SET_ENABLED, command=False)

        id = menu_proc(menu, MENU_ADD, command='cuda_project_man.action_import_external_session', caption=_('Import external session...'))
        if not self.project_file_path:
            menu_proc(id, MENU_SET_ENABLED, command=False)

    def fill_favorites_menu(self, menu):
        """Fill the context-menu "Favorites" submenu: add the clicked
        node, then the same items as the Plugins-menu Favorites submenu
        (see the Favorites > items in install.inf)."""
        id = menu_proc(menu, MENU_ADD,
                       command='cuda_project_man.action_add_to_favorites',
                       caption=_('Add to favorites'))
        if self.selected is None:
            menu_proc(id, MENU_SET_ENABLED, command=False)

        menu_proc(menu, MENU_ADD, caption='-')
        menu_proc(menu, MENU_ADD, command='cuda_project_man.favorites_show',
                  caption=_('Show Favorites window...'))
        menu_proc(menu, MENU_ADD, caption='-')
        menu_proc(menu, MENU_ADD, command='cuda_project_man.favorites_add_opened',
                  caption=_('Add opened...'))

    def update_global_data(self):
        global global_project_info
        global_project_info['filename'] = str(self.project_file_path) if self.project_file_path else ''
        global_project_info['nodes'] = self.project.get('nodes', [])
        global_project_info['vars'] = self.project.setdefault('vars', [])
        global_project_info['mainfile'] = self.project.setdefault('mainfile', '')

    def _fire_project_nodes_added(self):
        """Fire APPSTATE_PROJECT_NODE_ADD: node(s) were added to the open
        project's tree. For OTHER plugins: until now only project
        open/close had an app event (APPSTATE_PROJECT, fired by the core
        when this plugin calls PROC_SET_PROJECT), tree node changes were
        invisible and listeners had to poll global_project_info. The
        dict is refreshed FIRST, so an on_state listener already sees
        the new node list. Fired once per finished operation, not per
        node. Additions funnel through add_node ('Add folder/file',
        drag&drop from the OS file manager, file creation and rename
        flows); _move_node_on_disk fires it directly for the new path
        of a moved node."""
        self.update_global_data()
        app_proc(PROC_PROJECT_NODE_ADD, '')

    def _fire_project_nodes_removed(self):
        """Fire APPSTATE_PROJECT_NODE_DEL: node(s) were removed from the
        open project's tree; see _fire_project_nodes_added. Fired once
        per finished operation ('Remove missing paths' removes a whole
        batch with one event)."""
        self.update_global_data()
        app_proc(PROC_PROJECT_NODE_DEL, '')

    def get_info(self, index):
        if index is None:
            return
        info = tree_proc(self.tree, TREE_ITEM_GET_PROPS, index)
        if info:
            return NodeInfo(info['text'], info['icon'])

    def get_location_by_index(self, index):
        '''
        path = []
        while index and index not in self.top_nodes:
            path.append(self.get_info(index).caption)
            index = tree_proc(self.tree, TREE_ITEM_GET_PROPS, index)['parent']

        path.reverse()
        node = self.top_nodes.get(index, None)
        full_path = Path(node / str.join(os.sep, path)) if node else Path('')

        return full_path
        '''
        # same guard as get_info(): a None index (nothing selected) or an
        # unknown item must not reach tree_proc/props parsing (issue #6582
        # error class: tree_proc with a None param)
        if index is None:
            return Path('')
        p = tree_proc(self.tree, TREE_ITEM_GET_PROPS, index)
        return Path(p.get('data', '')) if p else Path('')

    def save_options(self):
        # Only save keys that exist in the current defaults dict. This
        # automatically drops stale keys that were removed from the code,
        # keeping the config file clean and in sync with the code.
        defaults = type(self).options
        d = {k: copy.deepcopy(v) for k, v in self.options.items() if k in defaults}
        d["recent_projects"] = [collapse_macros('', fn) for fn in d.get("recent_projects", [])]
        write_json_file(self.options_filename, d)

    def menu_recents(self):
        items = list(self.options["recent_projects"])

        if '' in items:
            items.remove('')
        if str(self.project_file_path) in items:
            items.remove(str(self.project_file_path))
        items = [item for item in items if os.path.isfile(item)]
        if not items:
            return

        items_nice = [os.path.basename(fn)+'\t'+collapse_filename(str(os.path.dirname(fn))) for fn in items]
        res = dlg_menu(DMENU_LIST, items_nice, caption=_('Recent projects'))
        if res is None:
            return

        self.init_panel()
        self.action_open_project(items[res])

    def do_unfold_first(self):
        """unfold 1st item under root"""
        items = tree_proc(self.tree, TREE_ITEM_ENUM_EX, 0)
        if not items:
            return
        h_first = items[0]['id']
        items = tree_proc(self.tree, TREE_ITEM_ENUM_EX, h_first)
        if not items:
            return
        tree_proc(self.tree, TREE_ITEM_UNFOLD, h_first)
        tree_proc(self.tree, TREE_ITEM_SELECT, h_first)

        path = self.get_location_by_index(self.selected)
        app_proc(PROC_SET_FOLDER, path)
        self.cur_dir = path

    def _new_project_from_dir(self, fn):
        """Create a project from folder `fn` in one step: the project file
        is created inside the folder, named after the folder (e.g. "myapp"
        -> "myapp/myapp.cuda-proj") — no second "save as" dialog. If the
        folder already contains that project file, it is simply opened
        instead."""

        if not os.path.isdir(fn):
            print(_('Project Manager: folder not found: ') + fn)
            return

        #expand "." to fully qualified name
        fn = os.path.abspath(fn)

        if is_locked(fn):
            print(_('Project Manager: folder is locked: ') + fn)
            return

        base = os.path.basename(fn.rstrip('\\/')) or 'project'
        proj = os.path.join(fn, base + PROJECT_EXTENSION)

        if os.path.isfile(proj):
            # the folder already has a project: open it instead
            self.action_open_project(proj)
            return

        if not os.access(fn, os.W_OK):
            msg_status(_('Cannot create project file in a read-only folder'))
            return

        try:
            self._create_project_file(Path(proj))
            self.add_node(fn)
            self.do_unfold_first()
        except OSError as ex:
            msg_status(_('Cannot create project file: %s') % ex)
            return

        app_proc(PROC_SIDEPANEL_ACTIVATE, self.title)
        msg_status(_('Created project: ') + '"' + os.path.basename(proj) + '"')

    def new_project_open_dir(self):
        """CudaText's "File - Open folder..." command (the core calls this
        method by name, it is not in this plugin's own menus): pick a
        folder and make a project from it in one step, see
        _new_project_from_dir."""

        fn = dlg_dir("")
        if fn is None: return

        self.init_panel()
        self._new_project_from_dir(fn)

    def open_dir(self, dirname, new_proj=False):
        """Called by CudaText itself when a folder is dragged from the OS
        file manager and dropped onto the editor (the core calls this
        method by name, it is not in this plugin's own menus). The folder
        is added as a root node of the current project; with no project
        open, of a new untitled project (it is saved to disk when the
        user saves the project). new_proj=True (not used by the current
        core) makes a new project from the folder in one step, like
        "File - Open folder..."."""

        if not os.path.isdir(dirname):
            print(_('Project Manager: folder not found: ') + dirname)
            return

        #expand "." to fully qualified name
        dirname = os.path.abspath(dirname)

        if is_locked(dirname):
            print(_('Project Manager: folder is locked: ') + dirname)
            return

        self.init_panel()
        if new_proj:
            self._new_project_from_dir(dirname)
            return

        self.add_node(dirname)
        app_proc(PROC_SIDEPANEL_ACTIVATE, self.title)

    def on_open_pre(self, ed_self, filename):
        if filename.endswith(PROJECT_EXTENSION):
            self.init_panel()
            self.action_open_project(filename)
            return False #block opening

    def on_exit(self, ed_self):
        # label the backups made by the exit saves -- this one, and the
        # app's own session save which runs right after on_exit (the core
        # fires OnExit first, then DoOps_SaveHistory saves the session) --
        # instead of the default 'by timer'
        self._backup_cause = 'on exit'
        # stop the background filter scan and its progress timer: no
        # dialog/tree updates may run after this point
        self._filter_cancel_scan()
        if self.project_file_path:
            self.action_save_project_as(self.project_file_path)
        else:
            # No project is loaded at exit: the plugin must not load at
            # the next CudaText start. It would only find a non-project
            # session in history.json and detach again. Remove the on_start
            # subscription from plugins.ini [events].
            self._update_on_start_subscription(False)

    def config(self):

        from .projman_dlg import dialog_config
        if dialog_config(self.options):
            print(_('Project Manager: saving options'))
            self.save_options()

            if self.h_dlg:
                dlg_proc(self.h_dlg, DLG_CTL_PROP_SET, name='bar', prop={
                    'vis': self.options.get('toolbar', True)
                    })
                button_proc(self.h_btn_sync, BTN_SET_VISIBLE, not self.options.get('always_sync', False))

            # keep the on_start subscription in sync with the project
            # state (the hidden on_start option is not in the dialog; a
            # stale True in a hand-edited options file must not
            # re-subscribe the plugin with no project open)
            self._update_on_start_subscription(bool(self.project_file_path))

    def _update_on_start_subscription(self, loaded):
        """Keep the on_start subscription in plugins.ini [events] in sync
        with whether a project is loaded. CudaText loads this plugin at
        start ONLY to restore an open project's session (the lazy '~'
        events of install.inf still work and load it on demand), so
        plugins.ini must not keep a stale on_start key.

        The key is subscribed by every project open/create/save (they set
        options['on_start']=True and call save_events). This method is the
        single unsubscribe point, called on EVERY path that ends with no
        project: "Close project", the on_start detach branches (foreign
        or plain session restored at start), the external-session-switch
        close in on_state, and on_exit with no project. With no project
        loaded the plugin then never loads at the next CudaText start.

        The hidden on_start option is set off, which makes save_events()
        delete the plugins.ini key, and the option file records it too."""
        self.options['on_start'] = bool(loaded)
        self.save_events()
        self.save_options()

    def save_events(self):
        # The on_start event is subscribed via plugins.ini (in addition
        # to the lazy '~' events of install.inf). CudaText loads this
        # plugin on start only when on_start is subscribed here, so
        # users who don't use Project Manager get a fast start: the
        # plugin is not loaded at all.
        ev = []

        if self.options.get('on_start', False):
            ev.append('on_start')

        # save events to plugins.ini [events], they work additionally to install.inf events
        if ev:
            ini_write('plugins.ini', 'events', 'cuda_project_man', ','.join(ev))
        else:
            ini_proc(INI_DELETE_KEY, 'plugins.ini', 'events', 'cuda_project_man')

    @_with_backup_cause('on project save')
    def config_proj(self):
        if not self.tree:
            msg_status(_('No project open'))
            return

        from .projman_dlg import dialog_proj_prop
        if dialog_proj_prop(self.project):
            self.update_global_data()
            if self.project_file_path:
                self.action_save_project_as(self.project_file_path)

    def is_filename_ignored(self, fn, is_dir):
        if is_dir:
            msk = self.options.get("no_dirs", "")
        else:
            msk = self.options.get("no_files", "")
        if msk:
            return is_mask_listed(os.path.basename(fn), msk)
        else:
            return False

    def on_start(self, ed_self):
        # on_start is subscribed via plugins.ini, so it only runs when the
        # plugin was loaded on start, i.e. when a project was used on the
        # previous run. Auto-open the most recent project, but only if
        # CudaText is restoring that project's session (detected via
        # history.json). If history.json points at a different session,
        # detach silently to avoid overwriting it.
        and_activate = self.options.get("on_start_activate", False)
        self.init_panel(and_activate)

        items = self.options.get("recent_projects", [])
        if items:
            # Read history.json to see which session CudaText is loading.
            # The "session" key is "project_path|/sessions/name" for a
            # project session, or a plain filename (e.g. default.cuda-session)
            # for a non-project session.
            history_path = os.path.join(app_path(APP_DIR_SETTINGS), 'history.json')
            sess_ptr = ''
            try:
                with open(history_path, encoding='utf8') as f:
                    hist = json.load(f)
                sess_ptr = hist.get('session' + OS_SUFFIX, '')
            except Exception:
                pass

            if sess_ptr and '|' in sess_ptr:
                # history.json stores the pointer of a session file located
                # in the settings dir WITHOUT its directory part (the app's
                # AppSessionName_ForHistoryFile strips the settings dir), so
                # 'sw.cuda-proj|/sessions/sw' means the project
                # '<settings>/sw.cuda-proj'. Resolve it before comparing,
                # or the comparison below never matches such projects and
                # the project is detached on every restart (issue #6591).
                sess_ptr = resolve_session_pointer(sess_ptr)
                proj_in_hist = sess_ptr.split('|', 1)[0]
                if same_path(proj_in_hist, items[0]):
                    # CudaText is loading this project's session — let it
                    # proceed, just load the project metadata (no session
                    # save/close/load, which would prompt for modified tabs).
                    # CudaText restores the last used session itself; only
                    # record it in the project file as "last_session".
                    self.action_open_project(items[0], load_session=False)
                    self._remember_last_session(sess_ptr)
                else:
                    # CudaText is loading a session that belongs to a
                    # DIFFERENT project. This happens if history.json was
                    # changed manually after CudaText was closed. Close the
                    # project silently (no tab closing, no DEF_SES load)
                    # so the foreign session's tabs stay open and the
                    # project's session isn't overwritten.
                    self._close_project_silent()
                    # No project is in use now — also drop the on_start
                    # subscription, so the next start doesn't load the
                    # plugin just to detach again.
                    self._update_on_start_subscription(False)
            else:
                # No project session in history.json (either empty or a
                # plain session like default.cuda-session). Don't auto-open
                # the project — let the foreign/default session stand.
                self._close_project_silent()
                # No project is in use now — also drop the on_start
                # subscription, so the next start doesn't load the
                # plugin just to detach again.
                self._update_on_start_subscription(False)

    def enum_all(self, callback):
        """
        Enum for all items.
        Until callback gets false.
        """
        if not self.tree:
            return
        items = tree_proc(self.tree, TREE_ITEM_ENUM_EX, 0)
        if items:
            return self.enum_subitems(items[0]['id'], callback)

    def enum_subitems(self, item, callback):
        """
        Callback for all subitems of given item.
        Until callback gets false.
        """
        items = tree_proc(self.tree, TREE_ITEM_ENUM_EX, item)
        if items:
            for i in items:
                subitem = i['id']
                fn = i.get('data', '')
                if not callback(fn, subitem):
                    return False
                if not self.enum_subitems(subitem, callback):
                    return False
        return True


    def enum_all_getfolds(self, unfolds):
        # no panel yet: no unfolded items to collect
        if not self.tree:
            return
        items = tree_proc(self.tree, TREE_ITEM_ENUM_EX, 0)
        if items:
            return self.enum_subitems_getfolds(items[0]['id'], unfolds)

    def enum_subitems_getfolds(self, item, unfolds):
        items = tree_proc(self.tree, TREE_ITEM_ENUM_EX, item)
        if items:
            for i in items:
                if i['sub_items']:
                    id = i['id']
                    prop = tree_proc(self.tree, TREE_ITEM_GET_PROPS, id)
                    if not prop['folded']:
                        fn = i.get('data', '')
                        if fn:
                            unfolds.append(fn)
                    self.enum_subitems_getfolds(id, unfolds)


    def enum_all_setfolds(self, unfolds):
        if not unfolds:
            return
        if not self.tree:
            return
        items = tree_proc(self.tree, TREE_ITEM_ENUM_EX, 0)
        if items:
            return self.enum_subitems_setfolds(items[0]['id'], unfolds)

    def enum_subitems_setfolds(self, item, unfolds):
        items = tree_proc(self.tree, TREE_ITEM_ENUM_EX, item)
        if items:
            for i in items:
                if i['sub_items']:
                    id = i['id']
                    fn = i.get('data', '')
                    if fn in unfolds:
                        tree_proc(self.tree, TREE_ITEM_UNFOLD, id)
                        unfolds.remove(fn)
                    #else:
                    #    tree_proc(self.tree, TREE_ITEM_FOLD, id)
                    self.enum_subitems_setfolds(id, unfolds)


    def enum_all_fn(self, filename, and_open):
        """
        Enum for all items.
        Find 'filename', and focus its node.
        """
        if not self.tree:
            return
        items = tree_proc(self.tree, TREE_ITEM_ENUM_EX, 0)
        if items:
            return self.enum_subitems_fn(items[0]['id'], filename, and_open)

    def enum_subitems_fn(self, item_src, filename, and_open):
        """
        Callback for all subitems of given item_src.
        When found 'filename', focus it and return False
        """
        def _need(dirpath):
            return filename.startswith(dirpath+os.sep)

        prop_list = tree_proc(self.tree, TREE_ITEM_ENUM_EX, item_src) or []
        for prop in prop_list:
            fn = prop['data']
            is_dir = prop['sub_items']

            if is_dir:
                if _need(fn):
                    node = prop['id']
                    tree_proc(self.tree, TREE_ITEM_UNFOLD, node)
                    if not self.enum_subitems_fn(node, filename, and_open):
                        return False

            elif fn==filename:
                node = prop['id']
                tree_proc(self.tree, TREE_ITEM_SELECT, node)
                tree_proc(self.tree, TREE_ITEM_SHOW, node)
                if is_dir:
                    tree_proc(self.tree, TREE_ITEM_UNFOLD, node)
                if and_open:
                    _file_open(fn)
                return False

        return True


    def enum_all_sel(self, filename):
        """
        Enum for all items.
        Find 'filename', and select/show its node.
        """
        if not self.tree:
            return
        items = tree_proc(self.tree, TREE_ITEM_ENUM_EX, 0)
        if items:
            return self.enum_subitems_sel(items[0]['id'], filename)

    def enum_subitems_sel(self, item_src, filename):
        """
        Callback for all subitems of given item_src.
        When found 'filename', focus it and return False
        """

        prop_list = tree_proc(self.tree, TREE_ITEM_ENUM_EX, item_src) or []
        for prop in prop_list:
            fn = prop['data']
            is_dir = prop['sub_items']

            if is_dir:
                node = prop['id']
                if not self.enum_subitems_sel(node, filename):
                    return False

            elif fn==filename:
                node = prop['id']
                tree_proc(self.tree, TREE_ITEM_SELECT, node)
                tree_proc(self.tree, TREE_ITEM_SHOW, node)
                #print('enum_subitems_sel found node!')
                return False

        return True


    def menu_goto(self):
        """ Show menu-dialog with all files in project, and jump to chosen file """
        if not self.tree:
            msg_status(_('No project open'))
            return

        files = self.enum_all_files()
        if self.is_project_empty():
            msg_status(_('Project is empty'))
            return

        files.sort()
        for item in self.goto_history:
            if item in files:
                files.remove(item)
        files = self.goto_history + files
        files_nice = [os.path.basename(fn)+'\t'+collapse_filename(os.path.dirname(fn)) for fn in files]

        res = dlg_menu(DMENU_LIST_ALT+DMENU_NO_FULLFILTER, #fuzzy search is needed for users
                       files_nice,
                       caption=_('Go to file')
                       )
        if res is None:
            return
        fn = files[res]

        if fn in self.goto_history:
            self.goto_history.remove(fn)
        self.goto_history.insert(0, fn)

        and_open = self.options.get('goto_open', False) \
            or not app_proc(PROC_SHOW_SIDEPANEL_GET, '') \
            or app_proc(PROC_SIDEPANEL_GET, '')!='Project'
        self.jump_to_filename(fn, and_open)

    def jump_to_filename(self, filename, and_open=False):
        """ Find filename in entire project and focus its tree node """
        msg_status(_('Located in project: ') + '"' + collapse_filename(filename) + '"')
        return self.enum_all_fn(filename, and_open)

    def sync_to_ed(self):
        """ Jump to active editor file, if it's in project """
        if not self.tree:
            msg_status(_('No project open'))
            return

        fn = ed.get_prop(PROP_FN, '*')
        if fn:
            if self.jump_to_filename(fn): #gets False if found
                msg_status(_('Not found in project: ') + '"' + collapse_filename(fn) + '"')


    def tree_on_unfold(self, id_dlg, id_ctl, data='', info=''):
        # a filtered-tree rebuild is filling the levels itself: its own
        # programmatic unfolds must not delete/refill anything (the
        # rebuild already placed the correct rows)
        if self._filter_rebuilding:
            return
        info = self.get_info(data)
        path = self.get_location_by_index(data)

        if not path.is_dir():
            tree_proc(self.tree, TREE_ITEM_DELETE, data)
            if str(path) in self.project["nodes"]:
                self.project["nodes"].remove(str(path))
                self._fire_project_nodes_removed()
            return

        if info.image != self.ICON_DIR:
            return
        items = tree_proc(self.tree, TREE_ITEM_ENUM_EX, data)
        if items:
            for item in items:
                tree_proc(self.tree, TREE_ITEM_DELETE, item['id'])

        if self.tree_filter:
            # the filter is active: remember the unfold (the repeating
            # live rebuilds of this search must keep the level visible
            # and growing), then refill the children LAZILY and cheaply
            # (from the scan's partial/finished index, or a single-level
            # name filter) - see _filter_refill_children. NEVER the
            # normal unfiltered refill (it would show everything), and
            # never a recursive re-scan of the subtree (that hung
            # CudaText on drive-sized folders while a search ran)
            if self._filter_expanded is None:
                self._filter_expanded = set()
            self._filter_expanded.add(str(path))
            self._filter_refill_children(data, str(path))
            return

        self.action_refresh_int(data) # call _int version, to avoid recursion

    def tree_on_menu(self, id_dlg, id_ctl, data='', info=''):

        self.generate_context_menu()
        menu_proc(self.h_menu, MENU_SHOW, command='')


    def do_open_current_file(self, options):

        info = self.get_info(self.selected)
        if not info:
            return
        path = self.get_location_by_index(self.selected)
        if not path:
            return

        if info.image == self.ICON_DIR:
            tree_proc(self.tree, TREE_ITEM_UNFOLD if tree_proc(self.tree, TREE_ITEM_GET_PROPS, self.selected)['folded'] else TREE_ITEM_FOLD, self.selected)
            return
        elif info.image in [self.ICON_BAD, self.ICON_PROJ]:
            return

        if not os.path.isfile(str(path)):
            tree_proc(self.tree, TREE_ITEM_SET_ICON, self.selected, image_index=self.ICON_BAD)
            return

        open_def_ext = tuple(MASKS_ZIP.split(' ') + MASKS_BINARY.split(' ') + MASKS_AUDIO.split(' ') + MASKS_VIDEO.split(' '))
        if info.caption.endswith(open_def_ext):
            self.action_open_def()
            return

        _file_open(str(path), options=options)

        # avoid stealing focus by editor
        dlg_proc(self.h_dlg, DLG_FOCUS)


    def get_open_options(self):

        s = '/preview' if self.options.get('preview', True) else ''
        s += ' /nozip /nontext-view-text'
        return s

    def _tree_click_item(self, data):
        """Node handle under the click position given in the event data
        (on_click/on_click_dbl pass a tuple (x, y) with tree client
        coordinates), or None when the click hit no node at all — LCL fires
        OnClick/OnDblClick for ANY left press+release inside the tree
        control, also for clicks on the empty area (below the items, the
        panel's margin), and such a click does NOT change the tree
        selection: acting on self.selected there would fold/unfold (or
        open, or re-base the 'current folder' of) a node which was not
        clicked.
        Returns False when the click position cannot be read from the
        event data (unexpected data): callers then fall back to the legacy
        behavior of acting on the selected node."""
        if not isinstance(data, (tuple, list)) or len(data) != 2:
            return False
        try:
            x = int(data[0])
            y = int(data[1])
        except (TypeError, ValueError):
            return False
        return tree_proc(self.tree, TREE_FIND_BY_COORD, 0, -1,
                         '{},{}'.format(x, y))

    def _tree_click_skips_node(self, item):
        """True when a click which hit the node `item` must NOT act on the
        selected node (used by tree_on_click/tree_on_click_dbl):

        - the hit node is not the selected one: this happens when the
          click landed on a node's fold +/- sign — LCL toggles that node
          right on mouse-down and does NOT select it, so the still-selected
          node was not clicked at all;

        - the hit node IS the selected one, but its folded state changed
          since the mouse-down (see _tree_press_folded): the click landed
          on that node's fold +/- sign and LCL already toggled it — acting
          here would toggle it a second time (the fold would visibly stay
          unchanged)."""
        if item != self.selected:
            return True
        press = self._tree_press_folded
        if press and press[0] == item:
            props = tree_proc(self.tree, TREE_ITEM_GET_PROPS, item)
            if props and props.get('folded') != press[1]:
                return True
        return False

    def tree_on_click(self, id_dlg, id_ctl, data='', info=''):

        # a mouse click moves the selection: restart the type-ahead
        self._ta_buf = ''

        # a drag gesture in the tree just ended on this mouse-up: LCL fires
        # OnClick even after a drag inside a control (and before OnMouseUp),
        # don't let it open the dropped-on node
        if self._tree_suppress_click:
            self._tree_suppress_click = False
            return

        # LCL fires OnClick for every left press+release inside the tree,
        # even when the click hit no node (the empty area below the items,
        # a fold +/- sign): such a click doesn't change the selection, so
        # don't act on the still-selected node — folding/unfolding (and
        # file opening) must only happen for a click on the node itself
        item = self._tree_click_item(data)
        if item is None:
            return

        # set folder in project as current folder for Open/Save-as dialogs
        node = self.selected
        if not node: # may be from some OnClick events
            return
        s = str(self.get_location_by_index(node))
        if s and not s.startswith('.'): # skip parasitic '.' for project root node
            if os.path.isdir(s):
                app_proc(PROC_SET_FOLDER, s)
                self.cur_dir = s
            elif os.path.isfile(s):
                app_proc(PROC_SET_FOLDER, os.path.dirname(s))
                self.cur_dir = os.path.dirname(s)

        if self.options.get('d_click', False):
            return
        if item and self._tree_click_skips_node(item):
            # click was on a fold +/- sign (the node was already toggled by
            # LCL on mouse-down): don't toggle/open a node which was not
            # clicked with the intent of activating it
            return
        self.do_open_current_file(self.get_open_options())

    def tree_on_click_dbl(self, id_dlg, id_ctl, data='', info=''):

        # like in tree_on_click: a double-click which hit no node (the
        # empty area of the tree) must not act on the selected node
        item = self._tree_click_item(data)
        if item is None:
            return

        if not self.options.get('d_click', False):
            #turn off 'preview' tab kind on dbl-click
            ed.set_prop(PROP_PREVIEW, False)
            return
        if item and self._tree_click_skips_node(item):
            return
        self.do_open_current_file(self.get_open_options())


    def set_imagelist_size(self, theme_name, imglist):

        res = re.match(r'^\S+x(\d+)$', theme_name)
        if not res:
            return msg_box(_('Project Manager: bad icons folder name: "%s"') % theme_name, MB_OK+MB_ICONERROR)
        n = int(res.group(1))
        if not 8<=n<=64:
            return msg_box(_('Project Manager: bad icons size: "%s"') % theme_name, MB_OK+MB_ICONERROR)

        imagelist_proc(imglist, IMAGELIST_SET_SIZE, (n, n))

    def icon_init(self):

        self.icon_theme = self.options.get('icon_theme', 'vscode_16x16')
        self.set_imagelist_size(self.icon_theme, self.tree_imglist)

        self.icon_dir = os.path.join(app_path(APP_DIR_DATA), 'filetypeicons', self.icon_theme)
        if not os.path.isdir(self.icon_dir):
            self.icon_dir = os.path.join(app_path(APP_DIR_DATA), 'filetypeicons', 'vscode_16x16')

        self.icon_json = os.path.join(self.icon_dir, 'icons.json')
        self.icon_json_dict = json.loads(open(self.icon_json, encoding='utf8').read())
        self.icon_indexes = {}


    def icon_get(self, key):

        s = self.icon_indexes.get(key, None)
        if s:
            return s

        fn = self.icon_json_dict.get(key, None)
        if fn is None:
            n = self.ICON_ALL
            self.icon_indexes[key] = n
            return n

        fn = os.path.join(self.icon_dir, fn)
        n = imagelist_proc(self.tree_imglist, IMAGELIST_ADD, value=fn)
        if n is None:
            print(_('Project Manager: incorrect filetype icon:'), fn)
            n = self.ICON_ALL
        self.icon_indexes[key] = n
        return n

    def get_node_type(self):
        node_type = None
        if self.selected is not None:
            n = self.get_info(self.selected).image
            if n == self.ICON_PROJ: node_type = NODE_PROJECT
            elif n == self.ICON_DIR: node_type = NODE_DIR
            elif n == self.ICON_BAD: node_type = NODE_BAD
            else: node_type = NODE_FILE
        return node_type

    def form_key_down(self, id_dlg, id_ctl, data):
        reg_key = ('a' in data) or ('c' in data) or ('s' in data) or ('m' in data)

        if self._filter_edit_focused():
            # the user types in the tree filter field: Esc clears it,
            # everything else belongs to the edit (the form has
            # keypreview=True, so every keystroke passes here first)
            if id_ctl == VK_ESCAPE and not reg_key and self.tree_filter:
                self.clear_panel_filter()
                return False  # block key
            return None

        # Esc anywhere else in the panel: clear the active tree filter
        if id_ctl == VK_ESCAPE and not reg_key:
            if self.tree_filter:
                self.clear_panel_filter()
                return False  # block key

        # type-ahead: plain letters/digits jump to the node starting
        # with the typed text (Explorer-like)
        if not reg_key and self._typeahead_key(id_ctl):
            return False  # block key

        if id_ctl in [VK_SPACE, VK_ENTER, VK_F4] and not reg_key:
            self.do_open_current_file(self.get_open_options())
            return False #block key
        elif id_ctl == VK_DELETE and not reg_key:
            node_type = self.get_node_type()
            if node_type == NODE_FILE:
                self.action_delete_file()
            elif node_type == NODE_DIR:
                self.action_delete_directory()
            return False #block key
        elif (id_ctl == VK_F5) or (data == S_CTRL_API and (id_ctl in (ord('r'), ord('R')))):
            self.action_refresh()
            return False #block key
        elif id_ctl == VK_F2 and not reg_key:
            self.action_rename()
            return False #block key
        elif id_ctl == VK_F7 and not reg_key:
            self.action_new_directory()
            return False #block key
        elif (data == S_CTRL_API and (id_ctl in (ord('n'), ord('N')))):
            self.action_new_file()
            return False #block key
        elif (data == S_CTRL_API and (id_ctl in (ord('x'), ord('X')))):
            self.action_cut()
            return False #block key
        elif (data == S_CTRL_API and (id_ctl in (ord('c'), ord('C')))):
            self.action_copy()
            return False #block key
        elif (data == S_CTRL_API and (id_ctl in (ord('v'), ord('V')))):
            self.action_paste()
            return False #block key
        elif (data == 'a' and id_ctl == VK_ENTER):
            self.action_get_properties()
            return False #block key

    def add_current_file(self):

        if not self.tree:
            self.init_panel(False)

        fn = ed.get_prop(PROP_FN, '*')
        self.add_node(fn)

    def add_current_dir(self):

        if not self.tree:
            self.init_panel(False)

        fn = ed.get_prop(PROP_FN, '*')
        if fn:
            d = os.path.dirname(fn)
            if not (IS_WIN and d.endswith(':\\')):
                self.add_node(d)

    def add_opened_files(self):

        if not self.tree:
            self.init_panel(False)

        for h in ed_handles():
            e = Editor(h)
            fn = e.get_prop(PROP_FN, '*')
            self.add_node(fn)

    def add_folder(self):
        """Command "Add to project: folder...": pick a folder and add it
        to the project."""
        self.init_panel()
        self.action_add_folder()

    def add_file(self):
        """Command "Add to project: file...": pick a file and add it
        to the project."""
        self.init_panel()
        self.action_add_file()

    def action_open_project_folder(self):
        """Open the folder that contains the project's .cuda-proj file in
        the OS file manager (Windows Explorer, Finder, ...). Added folders
        are not used here — they already have "Reveal in file manager"
        in their own context menu."""
        if not self.project_file_path:
            msg_status(_('No project open'))
            return

        path = str(self.project_file_path.parent)
        if not path or not os.path.isdir(path):
            msg_status(_('Project folder not found: ') + path)
            return

        suffix = OS_SUFFIX
        if suffix=='':
            #Windows
            import subprocess
            subprocess.Popen(('explorer.exe', path), shell=True)
        elif suffix=='__mac':
            #macOS
            os.system('open "%s"'%path)
        elif suffix=='__haiku':
            #Haiku
            msg_status('"Open project folder" is not implemented for this OS')
        else:
            #Linux and other Unixes
            os.spawnvp(os.P_WAIT, 'xdg-open', ('xdg-open', Path(path).as_uri()))


    def goto_main(self):
        if not self.tree:
            msg_status(_('No project open'))
            return

        fn = self.project.get('mainfile', '')
        if not fn:
            msg_status(_('Project main file is not set'))
            return
        self.jump_to_filename(fn)

    def open_main(self):
        fn = self.project.get('mainfile', '')
        if fn:
            _file_open(fn)
        else:
            msg_status(_('Project main file is not set'))

    def is_project_empty(self):
        return not bool(self.project.get('nodes'))

    def enum_all_files(self):
        files, dirs = [], []
        for root in self.project['nodes']:
            if os.path.isdir(root):
                dirs.append(root)
            elif os.path.isfile(root):
                files.append(root)

        while dirs:
            try:
                next_dir = dirs.pop(0)
                for found in os.scandir(next_dir):
                    # Ignoring symlinks prevents infinite loops with cyclic directory layouts
                    if found.is_dir() and not found.is_symlink() and not self.is_filename_ignored(found.path, True):
                        dirs.append(found.path)
                    elif found.is_file() and not self.is_filename_ignored(found.path, False):
                        files.append(found.path)
            except (OSError, FileNotFoundError):
                pass # Permissions issue. Not much we can do
        return files

    def open_all(self):
        if not self.tree:
            msg_status(_('No project open'))
            return

        files = self.enum_all_files()
        if self.is_project_empty():
            msg_status(_('Project is empty'))
            return

        if msg_box(_('Open all %d file(s) in editor?') % len(files), MB_OKCANCEL+MB_ICONQUESTION)!=ID_OK:
            return

        for (i, fn) in enumerate(files):
            _file_open(fn, options="/nontext-cancel")
            if i%10==0:
                app_proc(PROC_IDLE, False)

    def session_cur_name(self):

        fn_proj = str(self.project_file_path) if self.project_file_path else ''
        if not fn_proj:
            return ''

        s = ''
        sess = resolve_session_pointer(app_path(APP_FILE_SESSION))
        if '|' in sess:
            l = sess.split('|')
            if same_path(l[0], fn_proj):
                s = l[1]
                k = '/sessions/'
                if s.startswith(k):
                    s = s[len(k):]
        return s

    def session_get_names(self):
        """Names of the project's sessions, for the sessions menus.

        Fast path: the in-memory mirror self.project['sessions']. Every
        flow which can change the set of session names updates the mirror:
        action_open_project loads it from the project file,
        _sync_sessions_from_disk refreshes it after every plugin
        PROC_SAVE_SESSION (session_save, session_save_as, session_load,
        _save_session_before_close_tabs), session_delete pops the name
        from it, action_import_external_session stores into it.

        The old code json.load()-ed the ENTIRE project file here on every
        call. A .cuda-proj file embeds the full session data — the complete
        text of every unsaved and untitled tab, and of the secondary
        editor of split tabs ('pair') — so with a few big unsaved tabs open
        (cuda_differ comparing 3MB files, split tabs with big files) the
        file grows to tens of MB, and ONE right-click in the project tree
        (context-menu rebuild -> fill_sessions_menu -> here) froze the
        panel for 1-2 seconds at 100% of one core: the whole file was
        parsed just to read the session NAMES. The app's ~40 s session
        autosave rewrites the file (new mtime) but never adds/removes a
        session name, so the mirror cannot go stale for these menus.

        Fallback (mirror empty but a project file exists — a file written
        by an external tool or an older build): read the names from the
        file, cached on (path, mtime, size) so the big parse runs only
        when the file really changed, not on every autosave tick."""
        sess = self.project.get('sessions')
        if isinstance(sess, dict) and sess:
            return list(sess.keys())

        res = []
        fn = str(self.project_file_path) if self.project_file_path else ''
        if not (fn and os.path.isfile(fn)):
            return res
        try:
            st = os.stat(fn)
            key = (fn, st.st_mtime_ns, st.st_size)
        except OSError:
            key = None
        if key is not None and key == self._session_names_key:
            return list(self._session_names_cache)
        try:
            with open(fn, 'r', encoding='utf8') as f:
                data = json.load(f)
                k = data.get('sessions')
                if type(k)==dict:
                    res = list(k.keys())
        except (OSError, ValueError):
            pass
        if key is not None:
            self._session_names_key = key
            self._session_names_cache = list(res)
        return res

    def session_delete(self):
        """Delete a session from the project.

        A project must always keep at least one session (a project is
        itself a session), so the last remaining session cannot be
        deleted — the menu item is disabled and the command explains why.

        When the deleted session is the active one, the user picks another
        session OF THE PROJECT to load instead. The app session never
        leaves the project for an external one (switching to e.g.
        default.cuda-session while the project stays open would let the
        session autosave overwrite that external file — data loss)."""

        names = self.session_get_names()
        if not names:
            return msg_status(_('No project sessions'))

        if len(names) == 1:
            msg_box(
                _('The project has only one session, "%s".\n\nA project always keeps at least one session, so it cannot be deleted.') % names[0],
                MB_OK + MB_ICONINFO)
            return

        res = dlg_menu(DMENU_LIST, names, caption=_('Delete project session'))
        if res is None:
            return
        name = names[res]

        fn = self.project_file_path
        if not (fn and os.path.isfile(fn)):
            return

        # deleting the ACTIVE session: let the user pick another session
        # of this project to load instead (never an external session)
        if name == self.session_cur_name():
            rest = [n for n in names if n != name]
            res2 = dlg_menu(DMENU_LIST, rest,
                caption=_('Session to load instead of "%s"') % name)
            if res2 is None:
                return
            repl = rest[res2]
            # no need to save the doomed session's state — it is deleted.
            # Tabs keep their 'modified' flag, so CudaText asks "save
            # changes?" for unsaved text — the session is deleted on
            # purpose, but files can still be saved to disk.
            self.close_all_tabs(silent=False)
            ptr = str(fn) + '|/sessions/' + repl
            self._set_session(ptr)
            # A cancelled load MUST stop the deletion: the doomed session's
            # tabs are still open, and deleting the session (file +
            # in-memory) with the pointer at the replacement would let the
            # ~40 s session autosave clobber the REPLACEMENT session with
            # them. The repair re-points to the doomed session -- it stays
            # fully alive.
            ok = self._load_session_repairing_abort(ptr,
                str(fn) + '|/sessions/' + name,
                _('Session delete cancelled; session kept'))
            if not ok and ed_handles():
                return

        # 1. delete the session from the .cuda-proj file
        with open(fn, 'r', encoding='utf8') as f:
            data = json.load(f)
        if isinstance(data.get('sessions'), dict):
            data['sessions'].pop(name, None)
        if data.get('last_session') == name:
            rest = list((data.get('sessions') or {}).keys())
            data['last_session'] = rest[0] if rest else ''
        data.pop('def_session', None)
        write_json_file(fn, data)

        # 2. delete from the in-memory project dict too (pop, not del: the
        # in-memory copy may lag the file — a KeyError here would crash
        # the command right after the file was already updated)
        if isinstance(self.project.get('sessions'), dict):
            self.project['sessions'].pop(name, None)
        if self.project.get('last_session') == name:
            self.project['last_session'] = data.get('last_session', '')
        elif not self.project.get('last_session'):
            self.project['last_session'] = data.get('last_session', '')

        msg_status(_('Deleted session: ') + name)

    @_with_backup_cause('on session save')
    def session_save_as(self, is_request=True, sess_name=None):
        """Save the current editor state as a session of the project.

        Interactive (menu command "Save session as..."): asks for the new
        session's name. The state is written BOTH to the new session and
        to the currently active session — the changes were made while it
        was active, so they belong to it (switching sessions always saves
        the active one too). No tabs are closed: the full current state
        (including tabs not belonging to the project) is saved as is.

        Non-interactive (called after saving the project file): saves the
        current state into the current session slot, or 'session1' if
        none is active. `sess_name` (used by "Save project as...") keeps
        the active session's NAME in the new file instead of the generic
        'session1' — the session keeps its identity across the save-as."""

        fn = str(self.project_file_path) if self.project_file_path else ''
        if not fn:
            msg_status(_('Untitled project'))
            return

        names = self.session_get_names()
        if is_request:
            s = 'new'
            while True:
                s = dlg_input(_('Save session with name:'), s)
                if s is None:
                    return
                s = s.strip()
                if not s:
                    msg_status(_('Empty session name'))
                    continue
                if not is_session_name(s):
                    msg_status(_('Not allowed char(s) in the session name'))
                    continue
                if s in names:
                    msg_status(_('Session "%s" already exists')%s)
                    continue
                break
        else:
            s = sess_name or self.session_cur_name() or 'session1'

        # the changes were made while the current session was active:
        # save them into it as well, before switching to the new one
        cur_sess = self.session_cur_name()
        if is_request and cur_sess and cur_sess != s:
            app_proc(PROC_SAVE_SESSION, fn + '|/sessions/' + cur_sess)

        sess = fn+'|/sessions/'+s
        app_proc(PROC_SAVE_SESSION, sess)
        self._set_session(sess)

        # Refresh the in-memory mirror ONLY when the save added a NEW
        # session name (interactive 'Save session as...'), so the sessions
        # menus list it: re-reading the whole big project file after every
        # in-place save (action_save_project_as) was the main cost of
        # project switches. The session DATA in the mirror can stay stale:
        # it is never trusted — action_save_project_as re-reads it from
        # disk before writing the file, and only names are read elsewhere
        # (session_get_names). When the name-set changes without the
        # plugin's involvement (external edit of the file), the next
        # save-as falls back to the stat-cached file read.
        if s not in self.project.get('sessions', {}):
            self._sync_sessions_from_disk()

    def session_load_menu(self):

        names = self.session_get_names()
        res = dlg_menu(DMENU_LIST, names, caption=_('Open project session'))
        if res is None:
            return
        self.session_load(names[res])

    @_with_backup_cause('on session switch')
    def session_load(self, name=''):
        """Load another session of the current project.

        The active session is ALWAYS saved into its slot first — changes
        are never lost on switching, and no prompt is shown: CudaText's
        session autosave writes the state out every ~40 seconds anyway,
        so asking adds nothing (the only thing the "No" answer did was
        create a state the autosave would overwrite moments later)."""

        if not name:
            return

        if not name in self.session_get_names():
            msg_status(_('Project session "%s" not found')%name)
            return

        fn = str(self.project_file_path) if self.project_file_path else ''
        if not fn:
            msg_status(_('Untitled project'))
            return

        # always save the current session before switching (or RE-loading
        # the same one: its unsaved text goes into the slot and is restored
        # from there — skipping the save discarded it silently)
        cur_sess = self.session_cur_name()
        saved = False
        if cur_sess:
            # no _sync_sessions_from_disk here: saving an EXISTING session
            # cannot change the set of session names, and the session data
            # mirror is never trusted (re-read from disk before any write,
            # see session_save_as) — the full-file parse was pure cost on
            # every session switch
            saved = bool(app_proc(PROC_SAVE_SESSION, fn + '|/sessions/' + cur_sess))

        # Clear the modified flags (the state was just saved into its slot;
        # sessions embed the text of modified and untitled tabs) and leave
        # the CLOSING of the tabs to the PROC_LOAD_SESSION below: its
        # DoFileCloseAll runs under APPSTATE_SESSION_LOAD_BEGIN_PRE, so
        # plugins like cuda_differ keep the closing tabs' per-session
        # registrations and re-attach the tabs when this session is loaded
        # again. A tab still marked modified would make the load ask
        # 'Save modified tab?', where a Cancel aborts the whole switch —
        # so when the slot save above failed, the flags stay and the load
        # asks: unsaved text is never discarded silently. A Cancel is then
        # repaired by the load call below (the app session goes back to
        # this slot, so the autosave cannot clobber the target session).
        if saved:
            self._clear_modified_all_tabs()

        ptr_new = fn + '|/sessions/' + name
        # Abort-revert target: the session owning the still-open tabs
        # (the active slot -- the app pointer is not re-pointed yet)
        old_ptr = self._current_state_session_ptr()
        self._set_session(ptr_new)
        self._load_session_repairing_abort(ptr_new, old_ptr,
            _('Session switch cancelled; tabs kept'))

    def action_import_external_session(self):
        """Import an external .cuda-session file into the current project
        as an internal session. Migration helper for the old (pre-
        versioning) workflow which allowed external session files to be
        used with projects."""

        if not self.project_file_path:
            msg_status(_('No project open'))
            return

        fn = dlg_file(True, "", "", _("CudaText sessions") + "|*.cuda-session")
        if not fn:
            return

        try:
            with open(fn, encoding='utf8') as f:
                ses_data = json.load(f)
        except (OSError, ValueError) as ex:
            msg_box(_('Cannot read session file:\n%s\n\n%s') % (collapse_filename(fn), ex),
                MB_OK + MB_ICONERROR)
            return

        if not isinstance(ses_data, dict):
            msg_box(_('File is not a valid CudaText session:\n%s') % collapse_filename(fn),
                MB_OK + MB_ICONERROR)
            return

        # session name from the file name, sanitized and made unique
        base = os.path.splitext(os.path.basename(fn))[0]
        allowed = string.ascii_letters + string.digits + '., ()-+_$%='
        base = ''.join(ch if ch in allowed else '_' for ch in base) or 'imported'
        names = self.session_get_names()
        name = base
        n = 2
        while name in names:
            name = '%s_%d' % (base, n)
            n += 1

        # store the session inside the project file
        fn_proj = str(self.project_file_path)
        try:
            with open(fn_proj, 'r', encoding='utf8') as f:
                data = json.load(f)
        except (OSError, ValueError) as ex:
            msg_box(_('Cannot read project file:\n%s\n\n%s') % (collapse_filename(fn_proj), ex),
                MB_OK + MB_ICONERROR)
            return

        data.setdefault('sessions', {})
        data['sessions'][name] = ses_data
        data.pop('def_session', None)
        if data.get('last_session', '') not in data['sessions']:
            data['last_session'] = name

        try:
            write_json_file(fn_proj, data)
        except OSError as ex:
            msg_box(_('Cannot write project file:\n%s\n\n%s') % (collapse_filename(fn_proj), ex),
                MB_OK + MB_ICONERROR)
            return

        # update the in-memory project
        self.project['sessions'] = data['sessions']
        self.project['last_session'] = data['last_session']

        msg_box(
            _('Imported session "%s" into the project.\n\nLoad it when needed via the "Project sessions" menu.') % name,
            MB_OK + MB_ICONINFO)

    def close_foreign_tabs(self, confirm=True):
        """Close editor tabs that don't belong to the current project.

        confirm=True (menu command "Close tabs not belonging to project"):
        asks once whether to close the foreign tabs, then closes them.
        PROP_MODIFIED is not cleared, so tabs with unsaved changes get
        CudaText's own "save changes?" prompt — nothing is lost silently.

        confirm=False (menu command "... (no prompt)" and internal callers
        like session saving): all foreign tabs are closed at once, without
        any dialogs — unsaved changes in them are discarded (in both
        editors of split tabs, see _clear_modified_all_editors)."""

        # don't detect if project is empty
        items = self.project['nodes']
        if not items:
            return

        files, dirs = [], []
        for root in items:
            if os.path.isdir(root):
                dirs.append(root)
            else:
                files.append(root)

        def is_from_proj(fn):
            if not fn: # untitled tabs
                return False
            for d in dirs:
                if fn.startswith(d+os.sep):
                    return True
            for f in files:
                if f==fn:
                    return True
            return False

        import cudatext_cmd as cmds

        to_close = []
        for h in ed_handles():
            e = Editor(h)
            if not is_from_proj(e.get_prop(PROP_FN, '*')):
                to_close.append(e)

        if to_close:
            msg = _('CudaText has opened %d tab(s) not belonging to the project "%s". Close them?') \
                % (len(to_close), self.get_project_name())

            if not confirm or msg_box(msg, MB_OKCANCEL+MB_ICONQUESTION)==ID_OK:
                for e in reversed(to_close):
                    if not confirm:
                        self._clear_modified_all_editors(e)
                    e.cmd(cmds.cmd_FileClose)
                    time.sleep(0.2)

    def close_foreign_tabs_force(self):
        """Menu command "Close tabs not belonging to project (no prompt)":
        closes all foreign tabs without any dialogs, discarding unsaved
        changes (the old behavior of the main command)."""
        self.close_foreign_tabs(confirm=False)

    def _clear_modified_all_editors(self, e):
        """Clear PROP_MODIFIED on the given editor's ui-tab, in BOTH its
        editors. ed_handles() enumerates only the primary editor of each
        ui-tab, but a tab split into two unlinked editors
        (PROP_EDITORS_LINKED=False + PROP_SPLIT) has a secondary editor
        with its own text buffer and its own modified flag: the app counts
        a tab as modified when EITHER editor is modified
        (TEditorFrame.Modified = EdFirst.Modified or EdSecond.Modified).
        Clearing only the primary one made the app show the "save
        changes?" dialog on tab closing (cmd_FileCloseAll), even though
        the state had just been saved into a session — the session keeps
        the unsaved text of the secondary editor too (sub-key 'pair')."""
        e.set_prop(PROP_MODIFIED, False)
        h2 = e.get_prop(PROP_HANDLE_SECONDARY)
        if h2:
            # handle of the secondary editor object (PROP_HANDLE_SECONDARY
            # is a memory-address int, accepted by Editor()): for editors
            # not living in a ui-tab it is None
            Editor(h2).set_prop(PROP_MODIFIED, False)

    def _clear_modified_all_tabs(self):
        """Clear PROP_MODIFIED on every ui-tab (both editors of each, see
        _clear_modified_all_editors) WITHOUT closing anything.

        Used right before a PROC_LOAD_SESSION whose own DoFileCloseAll will
        close the tabs: after the state was saved into the session it
        belongs to, clearing the flags loses nothing (CudaText sessions
        embed the text of modified and untitled tabs) and keeps the switch
        promptless -- the app's load path first runs
        ConfirmAllFramesAreSaved, which asks 'Save modified tab?' for every
        tab still marked modified, and a CANCEL there aborts the WHOLE
        session load with the session pointer already switched by the
        caller (repaired by _load_session_repairing_abort: the app session
        goes back to the session owning the still-open tabs). The same
        contract as close_all_tabs(silent=True) minus the
        unpin and the close: the load's DoCloseAllTabs(AClosePinned=true)
        closes pinned tabs itself (AppSessionIsClosing suppresses its
        'Close pinned tab?' confirm)."""
        for h in ed_handles():
            self._clear_modified_all_editors(Editor(h))

    def _current_state_session_ptr(self):
        """The session pointer the CURRENT editor state belongs to: the
        open project's active session slot, or (no project open / the app
        session outside the project) the app's current session file.
        The abort-revert target of _load_session_repairing_abort: after a
        cancelled load the still-open tabs belong to THAT session, and
        the ~40 s session autosave must write them there, not into the
        not-loaded target."""
        fn_proj = str(self.project_file_path) if self.project_file_path else ''
        if fn_proj:
            cur = self.session_cur_name()
            if cur:
                return fn_proj + '|/sessions/' + cur
        return app_path(APP_FILE_SESSION)

    def _load_session_repairing_abort(self, ptr_new, ptr_old, cancelled_msg):
        """app_proc(PROC_LOAD_SESSION, ptr_new) with repair of the aborted
        state. Returns the load's bool result.

        The load returns False in two shapes (verified in the core,
        formmain_loadsave.inc / formmain.pas):
        - ABORT: its ConfirmAllFramesAreSaved -- which runs BEFORE any
          tab is closed -- was cancelled (reachable only when the state
          save failed earlier, so modified tabs kept their flag). Every
          tab is still open, but the app session pointer was already
          switched to ptr_new by the caller, so the ~40 s session
          autosave (SessionSaveInterval) and the exit save would write
          the still-open tabs into ptr_new's slot, CLOBBERING that
          session's saved tab set. Repair: re-point the app session to
          ptr_old -- the session owning the still-open tabs -- so the
          autosave writes them where they belong. The switch itself is
          not unwound (the project bookkeeping already ran); the on_state
          guard closes the project at the next session event when its
          session no longer belongs to it.
        - FAIL: the session file could not be parsed -- the tabs ARE
          closed already and nothing was restored; re-pointing would
          only aim the autosave at ptr_old's slot and wipe it with the
          now-empty state. Reported only."""
        ok = bool(app_proc(PROC_LOAD_SESSION, ptr_new))
        if ok:
            return True
        if ed_handles():
            # abort: the confirm was cancelled before any close
            if ptr_old and ptr_old != ptr_new:
                self._set_session(ptr_old)
                msg_status(cancelled_msg)
                print(_('Project Manager: session load cancelled; tabs kept,'
                        ' app session re-pointed to the tabs\' owner'))
            else:
                msg_status(cancelled_msg)
                print(_('Project Manager: session load cancelled; tabs kept'))
        else:
            # fail: the tabs are closed, nothing was restored
            msg_status(_('Session file could not be loaded; tabs were closed'))
            print(_('Project Manager: session file could not be parsed;'
                    ' tabs were closed, nothing restored'))
        return False

    def close_all_tabs(self, silent=True):
        """Close all tabs in all groups without changing the session pointer.

        Only used where NO session load follows the close (legacy
        session-less project files, a missing default session file on
        'Close project', deleting the active project session) -- every
        session/project SWITCH flow leaves the closing to the session load
        itself (PROC_LOAD_SESSION closes all ui-tabs under
        APPSTATE_SESSION_LOAD_BEGIN_PRE, so plugins can tell these closes
        from user closes; see _clear_modified_all_tabs).

        silent=True: clears PROP_MODIFIED (in the primary AND secondary
        editor of every tab, see _clear_modified_all_editors) and unpins
        pinned tabs first, so no "save changes?" prompt appears — ONLY for
        callers that have just saved the state into a session
        (PROC_SAVE_SESSION also stores the text of modified and untitled
        tabs, of both editors of split tabs, so a saved state loses
        nothing), or that discard the state on purpose.

        silent=False: keeps PROP_MODIFIED, so CudaText itself asks "save
        changes?" for every modified tab — used when the state could NOT be
        saved into any session; unsaved text is never discarded silently.
        Does NOT switch to DEF_SES — that's the caller's responsibility."""
        import cudatext_cmd as cmds
        for h in ed_handles():
            e = Editor(h)
            if silent:
                self._clear_modified_all_editors(e)
            if e.get_prop(PROP_TAB_PINNED):
                e.set_prop(PROP_TAB_PINNED, False)
        ed.cmd(cmds.cmd_FileCloseAll)

    def _save_session_before_close_tabs(self, already_saved=False):
        """Best-effort save of the current editor state into the session it
        belongs to, right before the tabs are closed (by close_all_tabs, or
        by the session load's own DoFileCloseAll) — the
        state (INCLUDING the text of modified and untitled tabs, which the
        app embeds into the session) must be stored somewhere before the
        buffers are gone.

        - While a project session is active: the project's session slot.
        - Otherwise (no project open, e.g. about to open/create a project):
          the app's current session file (typically default.cuda-session) —
          the very file CudaText's ~40 s session autosave and its exit save
          write anyway, so this cannot harm an external session; with no
          project open it IS the user's working session.

        Returns True when the state was stored. Callers use this to decide
        whether the closing may be silent: clear the modified flags
        (_clear_modified_all_tabs before a load, close_all_tabs(silent=True)
        otherwise) only when True; when False, CudaText's own "save
        changes?" prompt appears for modified tabs instead."""
        fn_proj = str(self.project_file_path) if self.project_file_path else ''
        cur = self.session_cur_name()
        if fn_proj and cur:
            if already_saved:
                # action_save_project_as -> session_save_as saved this very
                # session into its slot moments ago, in the same synchronous
                # call (no tab could change since): a second
                # PROC_SAVE_SESSION would just re-read and re-rewrite the
                # whole big project file for the identical state
                return True
            if bool(app_proc(PROC_SAVE_SESSION, fn_proj + '|/sessions/' + cur)):
                return True
            return False
        ptr = app_path(APP_FILE_SESSION)
        if not ptr:
            return False
        return bool(app_proc(PROC_SAVE_SESSION, ptr))

    @_with_backup_cause('on project save')
    def on_delete_file(self, ed_self, fn):

        #print('on_delete_file', fn)
        if fn in self.project["nodes"]:
            self.project["nodes"].remove(fn)
            self._fire_project_nodes_removed()
            self.action_refresh()
            if self.project_file_path:
                self.action_save_project_as(self.project_file_path)

    @_with_backup_cause('on session save')
    def session_save_inplace(self):
        """Save current state into the current session slot (no prompt).
        Menu entry point for 'Save session'."""
        if not self.project_file_path:
            msg_status(_('No project open'))
            return
        cur_sess = self.session_cur_name()
        if not cur_sess:
            msg_status(_('No active session'))
            return
        self.session_save()
        msg_status(_('Session saved: ') + cur_sess)

    def session_save(self):
        """Save the current state into the current session slot (no prompt,
        no session switch)."""
        cur_fn = str(self.project_file_path)
        cur_sess = self.session_cur_name()
        if cur_fn and cur_sess:
            sess = cur_fn+'|/sessions/'+cur_sess
            app_proc(PROC_SAVE_SESSION, sess)
            # no _sync_sessions_from_disk: the session name exists already
            # and the data mirror is never trusted (see session_save_as)

    def is_path_in_root(self, path):

        return str(path) in self.project['nodes']

    def on_tab_change(self, ed_self):

        if self.options.get('always_sync', False):
            if self.tree:
                self.sync_to_ed()

    def on_state(self, ed_self, state):
        """App state events.

        - APPSTATE_SESSION_SAVE_AFTER: a session save just finished (the
          ~40 s autosave with real tab changes, a session switch, a
          project save/switch, the save on exit -- see DoOps_SaveSession
          in formmain_loadsave.inc, which fires it for project-internal
          sessions too). While a project is open, the saved session is
          one of the project's slots inside the .cuda-proj file, so the
          file may have changed. When NO plugin command is running
          (_backup_flow == 0: the app saved on its own -- the autosave /
          the session manager / the exit save), the rotating backup is
          refreshed now (_backup_file_reported; it skips the copy when
          the file did not change on disk, so saves which rewrote
          nothing cost nothing). While a command decorated with
          _with_backup_cause IS running, the save is only recorded in
          _backup_pending: one command can save several times ('Save
          session as...' saves the current slot AND the new one), and
          one backup of the final state is made when the command ends.
          A made backup is reported to the CONSOLE (with the cause and
          the time), never to the status bar: the status bar is
          reserved for the manual 'Backup project file now'.
        - APPSTATE_SESSION_SET / APPSTATE_SESSION_LOAD: detect external
          session switching (issue #6366). If a project is open and the
          app's session no longer belongs to it, close the project
          silently (no tab closing, no DEF_SES load), so the project's
          session and the foreign session don't fight over the app's
          session slot.

        Session changes made by the plugin itself are guarded with
        _session_set_by_plugin and ignored here."""
        if state == APPSTATE_THEME_UI:
            # the UI theme was switched while CudaText runs: the app
            # re-themes the panel tree and toolbar itself (its pass over
            # the docked plugin dialogs), our own parts (the panel form
            # background, the filter field) and the favorites windows
            # are re-colored here
            if self.tree:
                self.apply_panel_theme()
            try:
                _favorites.apply_theme_all()
            except Exception:
                pass
            return

        if state == APPSTATE_SESSION_SAVE_AFTER:
            if self._backup_flow:
                # a plugin command is running which saves the session:
                # remember the project file it may have changed -- the
                # backup is made once, of the final state, when the
                # outermost command ends (_with_backup_cause)
                if self.project_file_path:
                    p = os.path.normpath(str(self.project_file_path))
                    if p not in self._backup_pending:
                        self._backup_pending.append(p)
            else:
                # not initiated by this plugin: back up now (the copy
                # itself is skipped when the file did not change)
                fn = str(self.project_file_path) if self.project_file_path else ''
                self._backup_file_reported(fn, self._backup_cause)
            return

        if state in (APPSTATE_SESSION_SET, APPSTATE_SESSION_LOAD):
            if self._session_set_by_plugin:
                return
            if self.project_file_path:
                if not self.session_cur_name():
                    print(_('Project Manager: app session was changed outside the project, closing the project'))
                    self._close_project_silent()
                    # No project is in use anymore — drop the on_start
                    # subscription too, so the next CudaText start doesn't
                    # load the plugin for a project that is no longer open.
                    self._update_on_start_subscription(False)
                    msg_status(_('Project closed: another session is now active'))
