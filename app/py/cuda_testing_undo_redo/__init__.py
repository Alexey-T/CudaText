"""
cuda_undo_redo_tests - extensive undo/redo regression test suite for CudaText.

PURPOSE
  Guards the undo/redo machinery against behavior changes and hidden bugs
  while the engine internals are being modified. Everything is tested
  through the public plugin API (black-box), with expected results computed
  by an independent pure-Python text model.

DESIGN: every test is standalone
  Each test (test_T01..test_T35, test_P1..test_P5) is a single
  self-contained function: document setup, the operation, every check,
  the exact-count undo/redo steps, and (for perf tests) the threshold
  judging are ALL inside the test function itself. Nothing test-specific is
  shared or factored out, so a test can be read and debugged top to
  bottom without following call chains. Only non-test infrastructure is
  shared: the text-model oracle, the document factories, the
  check/report channel, and the tab lifecycle.

UNDO/REDO MODEL (verified in the CudaText console, 2026-09-05)
  set_text_all() does NOT clear the undo stack. It keeps ONE entry:
  undoing it yields the EMPTY document - "it keeps one empty" - which
  is why the undo button stays enabled for exactly one extra click
  after set_text_all and greys out only after that click. Encoded in
  the suite:
  * after set_text_all(T) + N edits there are exactly N+1 undo steps;
  * every test undoes / redoes EXACTLY N steps - its own edits - and
    checks the text after every step. The old blind "drain until the
    text stops changing" loops are gone on purpose: they ran into
    the kept entry and mis-reported the document;
  * T23 pins the kept entry itself with the exact console trace
    (set_text_all(''), insert, undo, undo, redo, redo, no-ops beyond).

UNDO GROUPING (PROP_UNDO_GROUPED)
  By default CudaText (ATSynEdit) merges consecutive edits that occur
  within a short time window (~0.6-0.7 s) into a single undo step.
  This is the same mechanism that turns typing a word into one Undo
  instead of one Undo per character.  For a regression suite that
  asserts "exactly N API calls => exactly N undo entries" the
  grouping is fatal: an insert immediately followed by a delete. all collapse into fewer stack
  entries, so intermediate states never match and step counts are wrong.

  The suite therefore forces:
      ed.set_prop(PROP_UNDO_GROUPED, False)
  on some tests, and the
  exact-count undo/redo walks become deterministic.


  Console repro of the original problem (grouping on):
      ed.set_text_all('unicode ...'); ed.insert(2,0,'中Äßé');
      ed.delete(1,0,5,0); ed.cmd(Undo)   # fails - chars not restored
  With a 0.7 s sleep (or PROP_UNDO_GROUPED=False) the same sequence
  restores correctly.
  example: in cudatext console do:
    import cudatext_cmd as cmds; import time; ed.set_text_all('unicode ünïcödé 中文'); ed.insert(2,0,'中Äßé'); time.sleep(0.6); ed.delete(1,0,5,0); ed.cmd(cmds.cCommand_Undo); print('UNDO FAILED' if '中Äßé' not in ed.get_text_all() else 'undo ok')
    ===> UNDO FAILED
    
    import cudatext_cmd as cmds; import time; ed.set_text_all('unicode ünïcödé 中文'); ed.insert(2,0,'中Äßé'); time.sleep(0.7); ed.delete(1,0,5,0); ed.cmd(cmds.cCommand_Undo); print('UNDO FAILED' if '中Äßé' not in ed.get_text_all() else 'undo ok')
    ===> undo ok

  so this is not a bug:
    this is undo grouping (coalescing), not a Unicode bug.
    CudaText (via its editor component ATSynEdit) automatically merges consecutive edits that happen close together in time into one undo step. This is the same mechanism that makes typing a whole word produce a single Undo instead of one Undo per character.
    What happens in your test
    	1. ed.insert(2, 0, '中Äßé')
    → creates an undo record for the insertion.
    	2. Immediately afterwards ed.delete(1, 0, 5, 0)
    → if this occurs before the grouping timeout expires, the editor treats the two actions as part of the same logical edit.
    The delete is either merged with the previous insert or cancels/replaces the previous undo record.
    	3. Result:
    		○ The document ends up in the correct final state (net effect of insert-then-delete).
    		○ But there is only one (or a combined) undo entry.
    		○ Therefore a single cCommand_Undo cannot restore the intermediate state that still contained 中Äßé.
    	4. When you insert a pause of ~0.7 s the grouping timer expires.
    The insert is “closed”, the following delete starts a new undo group, and now you have two independent entries. Undo of the delete correctly brings the four characters back.
    That is why:
    	• time.sleep(0.6) → still grouped → UNDO FAILED
    	• time.sleep(0.7) → group closed → undo ok



  IMPORTANT - do NOT force PROP_UNDO_GROUPED=False for the whole suite.
  With grouping off, the full performance suite (300k-line docs, P1-P5)
  can consume >6 GB RAM.  Grouping must stay ON (True) globally; only
  individual tests that need exact per-op undo entries may temporarily
  set it to False and must restore True afterwards.

  Tests that need exact "1 API call = 1 undo entry" behaviour (e.g.
  T21 insert+delete, sequential storms) should disable grouping only
  for their own body:
      ed.set_prop(PROP_UNDO_GROUPED, False)
      try:
          ... ops and exact-count undo/redo ...
      finally:
          ed.set_prop(PROP_UNDO_GROUPED, True)

  Note: a prior bug in cCommand_TextDeleteSelection made redo of a
  selection-to-EOF incorrect when PROP_UNDO_GROUPED was False; that
  was fixed upstream (CudaText issue #6446).

_________________________________
WHAT IT COVERS
  - inserts: ed.insert(), typing via cmd(cCommand_TextInsert) (without/
    with a selection), typing simulation (adjacent chars), insert at doc
    start/end/into empty doc, multi-line insert, 500-line single op,
    100k-char line edit
  - deletes: key Backspace (mid-line / line start join), key Delete
    (mid-line / line end join), cCommand_TextDeleteSelection (forward,
    backward, multi-line, to-EOF, select-all), ed.delete(x1,y1,x2,y2)
    crossing newlines, 30 sequential line deletions, 150-op random storm
  - behavior contracts: text after op / after undo / after redo
    equals the model at every exact step; caret + selection restored
    by undo; redo stack invalidated by a new edit; extra undos/redos
    on empty stacks are no-ops; every undo step lands on a valid
    intermediate state (state walk); modified-flag / save-marker
    cycle; set_text_all keeps ONE undo entry (T23 traces it exactly)
  - environment interactions: word wrap on/off (whole suite runs twice),
    wrap toggled between undo/redo, tab switching away and back, unicode
    and tab chars, EOL toggle fidelity
  - multi-caret editing: Enter and Backspace with 3 carets, undo/redo
  - performance & mass-event regression on 50k/300k-line docs, including
    your exact scenario: delete first 200k lines of a 300k-line doc with
    wrap on, then undo/redo; repeated cycles; interleaved edit after the
    big undo. Times and per-op on_change/on_caret event counts are
    printed (the "mass events" metric).

OUTPUT
  All results go to the Console panel. Per test: check lines
  (ok / FAIL with got/expected / ERR), info lines (undo step
  counts, event counts, timings). A SUMMARY is printed at the end:
  totals, list of failed tests, performance table, event totals,
  overall verdict.

NOTES
  * Do not touch the editor while the suite runs.
  * Full mode needs ~2 GB RAM (300k-line docs) and can take several
    minutes (much longer if undo is still slow/not patched - that is the point).
  * The plugin registers on_change/on_caret counters to measure how many
    events fire per operation (mass-events regression). They stay active
    after the run (cheap counters); restart CudaText to unload. Counters
    are event handlers Command.on_change / Command.on_caret, subscribed
    statically via the [events] section of install.inf.
  * A fresh untitled tab (tag URTEST_TAB) is created for the tests and
    closed automatically at the end; your tabs are not modified.
  * PROP_UNDO_GROUPED stays True (the CudaText default) for the suite.
    Forcing it False globally exhausts RAM on the 300k-line perf tests
    (>6 GB).  Only tests that need exact per-op undo entries may
    temporarily set it False and must restore True.  See UNDO GROUPING.
"""

import os
import sys
import time
import random
import traceback
import tempfile

import cudatext
import cudatext_cmd as cmds

SEED = 20260904

# ----------------------------------------------------------------------------
# event counters (observability of mass events)
# ----------------------------------------------------------------------------
# NOTE: the real event handlers are the methods Command.on_change and
# Command.on_caret at the bottom of this file. CudaText calls event
# handlers only as methods of class Command, subscribed via the
# [events] section of install.inf ("events=on_change,on_caret");
# module-level functions would never be called.
#
# Tests reset EV['change']/EV['caret'] before an operation and read them
# after it, to count the events fired by that single operation.

EV = {'change': 0, 'caret': 0, 'change_total': 0, 'caret_total': 0}

# ----------------------------------------------------------------------------
# independent text model (the "oracle"; pure python, no editor involved)
# ----------------------------------------------------------------------------
# The ONLY shared test logic: deterministic reference implementations the
# tests call with explicit literal arguments to compute the expected
# document. Keeping the oracle in one place is what makes the checks
# trustworthy; duplicating it per test would invite drift.

def N(t):
    """Normalize newlines for comparison (CRLF docs must compare equal)."""
    if isinstance(t, str) and '\r' in t:
        return t.replace('\r\n', '\n').replace('\r', '\n')
    return t

def m_join(lines):
    return '\n'.join(lines)

def m_insert(lines, x, y, s):
    """Insert string s at (x, y); returns new list of lines."""
    parts = s.split('\n')
    line = lines[y]
    if len(parts) == 1:
        return lines[:y] + [line[:x] + s + line[x:]] + lines[y + 1:]
    seg = [line[:x] + parts[0]] + parts[1:-1] + [parts[-1] + line[x:]]
    return lines[:y] + seg + lines[y + 1:]

def m_delete(lines, x1, y1, x2, y2):
    """Delete chars between (x1,y1) and (x2,y2) (endpoints auto-ordered)."""
    if (y1, x1) > (y2, x2):
        x1, y1, x2, y2 = x2, y2, x1, y1
    if y1 == y2:
        line = lines[y1]
        return lines[:y1] + [line[:x1] + line[x2:]] + lines[y1 + 1:]
    merged = lines[y1][:x1] + lines[y2][x2:]
    return lines[:y1] + [merged] + lines[y2 + 1:]

# ----------------------------------------------------------------------------
# deterministic test documents
# ----------------------------------------------------------------------------

def make_small_lines():
    """80 lines: empty, short, long, unicode(BMP), tabs, trailing spaces."""
    rng = random.Random(SEED)
    lines = []
    for i in range(80):
        k = i % 8
        if k == 0:
            lines.append('')
        elif k == 1:
            lines.append('x' * rng.randint(10, 60))
        elif k == 2:
            lines.append('line %d %s' % (i, 'y' * rng.randint(60, 300)))
        elif k == 3:
            lines.append('unicode ünïcödé 中文 Тест line %d' % i)
        elif k == 4:
            lines.append('tab\there\tand\tthere %d' % i)
        elif k == 5:
            lines.append('trailing spaces %d   ' % i)
        elif k == 6:
            lines.append('short %d' % i)
        else:
            lines.append('z' * rng.randint(5, 40))
    return lines

def make_big_lines(n):
    """The benchmark corpus: n lines of 'x' * rand(100..1000)."""
    rng = random.Random(SEED)
    return ['x' * rng.randint(100, 1000) for _i in range(n)]

_BIG_CACHE = {}

def big_lines(n):
    if n not in _BIG_CACHE:
        _BIG_CACHE.clear()
        _BIG_CACHE[n] = make_big_lines(n)
    return _BIG_CACHE[n]

# ----------------------------------------------------------------------------
# misc helpers
# ----------------------------------------------------------------------------

def _pv(v):
    """Short preview of a value for FAIL messages."""
    if isinstance(v, bool):
        return repr(v)
    if isinstance(v, str):
        s = v.replace('\n', '\\n').replace('\r', '\\r').replace('\t', '\\t')
        if len(s) > 96:
            s = s[:96] + '...'
        return 'str(%d) %r' % (len(v), s)
    return '%r' % (v,)

# perf thresholds for line counts not in Runner.TH (fallback:
# the 300k-line thresholds)
TH_DEFAULT = (2.0, 12.0, 5.0, 25.0, 5.0, 25.0)


# ----------------------------------------------------------------------------
# runner
# ----------------------------------------------------------------------------

class Runner:

    # perf thresholds: nlines -> (warn/fail delete, warn/fail undo, warn/fail redo)
    TH = {
        50000:  (1.0, 5.0, 2.0, 8.0, 2.0, 8.0),
        300000: (2.0, 12.0, 5.0, 25.0, 5.0, 25.0),
    }

    def __init__(self, full):
        self.full = full
        self.TE = None          # test editor (own tab, tag URTEST_TAB)
        self.orig = None        # user's originally active editor
        self.wrap = 0
        self.results = []       # one dict per test
        self.perf = []          # one dict per perf run
        self.cur = None         # current test record
        self.fatal = None
        self._undo_grouped_orig = None  # saved PROP_UNDO_GROUPED
        self.log = []           # full console log (also opened in a tab)

    # ---- console ----

    def out(self, s=''):
        print(s)
        try:
            self.log.append(s)
        except Exception:
            pass
        try:
            sys.stdout.flush()
        except Exception:
            pass

    # ---- bookkeeping: the check/report channel every test writes to ----

    def begin(self, tid, name):
        self.cur = {
            'id': tid, 'name': name, 'wrap': self.wrap,
            'ok': 0, 'bad': 0, 'status': 'PASS', 'note': '',
        }
        self.results.append(self.cur)
        self.out()
        self.out('[%s] %s (wrap=%s)' % (tid, name, 'on' if self.wrap else 'off'))

    def done(self):
        c = self.cur
        extra = ''
        if c['note']:
            extra = '   note: %s' % c['note'][:150]
        self.out('  => %s   (checks: %d ok, %d failed)%s' % (
            c['status'], c['ok'], c['bad'], extra))
        self.cur = None

    def t(self, tid, name, fn):
        """Record one test: run fn() with exception bookkeeping."""
        self.begin(tid, name)
        try:
            fn()
        except Exception:
            self.cur['status'] = 'ERR'
            tb = traceback.format_exc()
            self.cur['note'] = tb.strip().splitlines()[-1][:200]
            self.out('    ERR   exception raised:')
            for ln in tb.strip().splitlines()[-5:]:
                self.out('            ' + ln)
        self.done()

    def check(self, label, actual, expected):
        a, e = N(actual), N(expected)
        if a == e:
            self.cur['ok'] += 1
            self.out('    ok    %s' % label)
            return True
        self.cur['bad'] += 1
        if self.cur['status'] != 'ERR':
            self.cur['status'] = 'FAIL'
        note = '%s: got %s, want %s' % (label, _pv(actual), _pv(expected))
        if self.cur['note']:
            self.cur['note'] += '; '
        self.cur['note'] += note[:200]
        self.out('    FAIL  %s' % label)
        self.out('            got      %s' % _pv(actual))
        self.out('            expected %s' % _pv(expected))
        return False

    def info(self, label, val=''):
        if val == '' and not isinstance(val, (int, float)):
            self.out('    info  %s' % label)
        else:
            self.out('    info  %s: %s' % (label, val))

    # ---- editor api helpers ----

    def _ed_focused(self):
        """Standalone Editor object for the currently focused editor.
        cudatext.ed is the virtual Editor(0) whose handle 0 always refers
        to the focused editor, so storing it would not pin a tab; instead
        grab the editor's unique handle (PROP_HANDLE_SELF) and build an
        independent object with cudatext.Editor(handle)."""
        try:
            h = cudatext.ed.get_prop(cudatext.PROP_HANDLE_SELF)
            if h:
                return cudatext.Editor(h)
        except Exception:
            pass
        return cudatext.ed

    # ---- lifecycle ----

    def run(self):
        self.out('=' * 66)
        self.out(' CudaText Undo/Redo Regression Suite  (cuda_undo_redo_tests)')
        self.out(' mode=%s   seed=%d   %s' % (
            'full: incl. 300k-line perf tests' if self.full
            else 'quick: 50k-line perf test',
            SEED, time.strftime('%Y-%m-%d %H:%M:%S')))
        self.out(' NOTE: do not touch the editor while the suite is running.')
        if self.full:
            self.out(' NOTE: full mode needs ~1 GB RAM and several minutes;')
            self.out('       it takes much longer if undo/redo is still slow.')
        self.out('=' * 66)
        self._run_body(lambda: (self._core_suite(0), self._core_suite(1),
                                self._perf_suite()))

    def run_single(self, tid):
        """Run only one test, by id from test_catalog() ('T07', 'P2', ...).
        Core tests run with word wrap off and on, like in the full suite;
        perf tests handle their wrap modes themselves."""
        self.out('=' * 66)
        self.out(' CudaText Undo/Redo Regression Suite  (cuda_undo_redo_tests)')
        self.out(' mode=single test %s   seed=%d   %s' % (
            tid, SEED, time.strftime('%Y-%m-%d %H:%M:%S')))
        self.out(' NOTE: do not touch the editor while the test is running.')
        if tid.startswith('P'):
            self.out(' NOTE: perf tests build big docs (up to 300k lines);')
            self.out('       they need RAM and can take a while.')
        self.out('=' * 66)
        self._run_body(lambda: self._single_test(tid))

    def _run_body(self, body):
        """Common lifecycle around a run: setup, body, cleanup, summary.
        Exceptions are reported as FATAL with full traceback - nothing
        is silently swallowed."""
        try:
            self._setup()
            if self.fatal:
                self.out()
                self.out('FATAL: %s' % self.fatal)
                return
            body()
        except Exception:
            self.out()
            self.out('FATAL ERROR inside suite:')
            self.out(traceback.format_exc())
        finally:
            self._cleanup()
            self._summary()

    def _setup(self):
        # capture the user's active editor first: we need an independent
        # Editor object (see _ed_focused), before the test tab is created
        self.orig = self._ed_focused()
        te = None
        try:
            # file_open('') creates and activates a fresh untitled tab;
            # it returns bool (True on success), not an editor object.
            # After it returns, cudatext.ed refers to the new tab.
            if cudatext.file_open(''):
                te = self._ed_focused()
        except Exception:
            te = None
        if te is None:
            cudatext.ed.cmd(cmds.cmd_FileNew)
            te = self._ed_focused()
        if te is None:
            self.fatal = ('cannot create a new editor tab '
                          '(cudatext.file_open("") / cmd_FileNew both failed)')
            return
        self.TE = te
        try:
            self.TE.set_prop(cudatext.PROP_TAG, 'URTEST_TAB')
        except Exception:
            pass
        # Keep undo grouping ON (CudaText default).  Forcing it False
        # for the whole suite makes the 300k-line perf tests use >6 GB
        # RAM.  Individual tests that need exact per-op undo entries
        # disable it only for their own body and restore True after.
        # Saved value is restored in _cleanup.
        try:
            self._undo_grouped_orig = self.TE.get_prop(
                cudatext.PROP_UNDO_GROUPED)
            self.TE.set_prop(cudatext.PROP_UNDO_GROUPED, True)
        except Exception:
            self._undo_grouped_orig = None
        try:
            # let the app process pending messages so the new editor is
            # fully inited before caret commands are run on it
            cudatext.app_proc(cudatext.PROC_IDLE, True)
        except Exception:
            pass

    def _cleanup(self):
        if self.TE is None:
            if self.orig is not None:
                try:
                    self.orig.focus()
                except Exception:
                    pass
            return
        try:
            self.TE.set_prop(cudatext.PROP_MODIFIED, False)
        except Exception:
            pass
        try:
            self.TE.set_prop(cudatext.PROP_WRAP, 0)
        except Exception:
            pass
        # restore the user's original undo-grouping preference
        if self._undo_grouped_orig is not None:
            try:
                self.TE.set_prop(cudatext.PROP_UNDO_GROUPED,
                                 self._undo_grouped_orig)
            except Exception:
                pass
        closed = False
        # focus the test tab first, so cmd_FileClose (run on the
        # focused editor) closes the test tab, not another one
        self.TE.focus()
        active_tag = str(cudatext.ed.get_prop(cudatext.PROP_TAG))
        if active_tag == 'URTEST_TAB':
            cudatext.ed.cmd(cmds.cmd_FileClose)
            closed = True
        if self.orig is not None:
            try:
                self.orig.focus()
            except Exception:
                pass
        if not closed:
            self.out('note: could not close the test tab automatically - '
                     'close it manually (tab tag URTEST_TAB).')

    def _summary(self):
        n = len(self.results)
        st = {'PASS': 0, 'FAIL': 0, 'ERR': 0}
        for r in self.results:
            if r['status'] in st:
                st[r['status']] += 1
        self.out()
        self.out('=' * 66)
        self.out(' SUMMARY   (%s)' % time.strftime('%Y-%m-%d %H:%M:%S'))
        self.out('=' * 66)
        self.out(' tests run: %d    PASS: %d    FAIL: %d    ERR: %d' % (
            n, st['PASS'], st['FAIL'], st['ERR']))
        bad = [r for r in self.results if r['status'] in ('FAIL', 'ERR')]
        if bad:
            self.out(' failed tests:')
            for r in bad:
                self.out('   [%s] %s (wrap=%s)' % (r['id'], r['name'],
                         'on' if r.get('wrap') else 'off'))
                if r['note']:
                    self.out('        %s' % r['note'][:220])
        if self.perf:
            self.out(' performance results:')
            self.out('   %-6s %-4s %-8s %9s %9s %9s  %-6s' % (
                'test', 'wrap', 'lines', 'delete', 'undo', 'redo', 'status'))
            for p in self.perf:
                self.out('   %-6s %-4s %-8d %8.2fs %8.2fs %8.2fs  %-6s' % (
                    p['id'], 'on' if p['wrap'] else 'off', p['lines'],
                    p['del'], p['undo'], p['redo'], p['status']))
                if p['note']:
                    self.out('        %s' % p['note'][:200])
        self.out(' event totals (whole run): on_change=%d, on_caret=%d' % (
            EV['change_total'], EV['caret_total']))
        perf_fail = any(p['status'] == 'FAIL' for p in self.perf)
        if st['FAIL'] == 0 and st['ERR'] == 0 and not perf_fail:
            overall = 'ALL TESTS PASSED'
        else:
            overall = 'FAILURES DETECTED - see details above'
        self.out(' overall: %s' % overall)
        self.out('=' * 66)
        try:
            cudatext.msg_status('Undo/Redo tests: %s (see console)' % overall)
        except Exception:
            pass

        # ---- end-of-run UI: summary dialog + log tab ----
        lines = [
            'CudaText Undo/Redo tests',
            '',
            'tests run:  %d' % n,
            'PASS:       %d' % st['PASS'],
            'FAIL:       %d' % st['FAIL'],
            'ERR:        %d' % st['ERR'],
        ]
        if self.perf:
            pf = sum(1 for p in self.perf if p['status'] == 'FAIL')
            pw = sum(1 for p in self.perf if p['status'] == 'WARN')
            lines.append('perf FAIL:  %d' % pf)
            if pw:
                lines.append('perf WARN:  %d' % pw)
        lines.append('')
        lines.append(overall)
        if bad:
            lines.append('')
            lines.append('Failed:')
            for r in bad[:12]:
                lines.append('  [%s] %s (wrap=%s)' % (
                    r['id'], r['name'], 'on' if r.get('wrap') else 'off'))
            if len(bad) > 12:
                lines.append('  ... and %d more' % (len(bad) - 12))
        summary_text = '\n'.join(lines)
        try:
            # MB_OK + icon: INFO if all passed, else WARNING
            flags = getattr(cudatext, 'MB_OK', 0)
            if st['FAIL'] or st['ERR'] or perf_fail:
                flags |= getattr(cudatext, 'MB_ICONWARNING', 0)
            else:
                flags |= getattr(cudatext, 'MB_ICONINFO', 0)
            cudatext.msg_box(summary_text, flags)
        except Exception:
            pass

        # open a new untitled tab with the full console log
        try:
            log_text = '\n'.join(self.log)
            opened = False
            try:
                opened = bool(cudatext.file_open(''))
            except Exception:
                opened = False
            if not opened:
                try:
                    cudatext.ed.cmd(cmds.cmd_FileNew)
                    opened = True
                except Exception:
                    opened = False
            if opened:
                log_ed = self._ed_focused()
                log_ed.set_text_all(log_text)
                try:
                    log_ed.set_prop(cudatext.PROP_TAB_TITLE,
                                    'Undo/Redo test log')
                except Exception:
                    pass
                try:
                    log_ed.set_prop(cudatext.PROP_MODIFIED, False)
                except Exception:
                    pass
                try:
                    # put caret at top so the user sees the start of the log
                    log_ed.set_caret(0, 0)
                except Exception:
                    pass
        except Exception:
            pass

    # ---- suite driving (uses the TESTS registry at module level) ----

    def _set_wrap(self, w):
        """Set word wrap for the following tests."""
        self.wrap = w
        self.TE.set_prop(cudatext.PROP_WRAP, w)

    def _core_suite(self, w):
        self._set_wrap(w)
        self.out()
        self.out('------ core tests, word wrap = %s ------' % ('on' if w else 'off'))
        for tid, name, fn in TESTS:
            if tid.startswith('P'):
                continue
            self.t(tid, name, lambda f=fn: f(self))

    def _perf_suite(self):
        self.out()
        self.out('------ performance & mass-op tests ------')
        # P1 (50k lines) always runs; the 300k-line tests only in full mode
        for tid, _name, fn in TESTS:
            if not tid.startswith('P'):
                continue
            if self.full or tid == 'P1':
                fn(self)

    def _single_test(self, tid):
        """Run exactly one test from the catalog. A core test runs with
        word wrap off and then on (same contract as in the whole suite);
        a perf test manages its wrap modes itself."""
        for t_id, name, fn in TESTS:
            if t_id != tid:
                continue
            if tid.startswith('T'):
                for w in (0, 1):
                    self._set_wrap(w)
                    self.out()
                    self.out('------ single test %s, word wrap = %s ------' % (
                        tid, 'on' if w else 'off'))
                    self.t(tid, name, lambda f=fn: f(self))
                return
            # perf test: makes its own records and wrap modes
            self.out()
            self.out('------ single perf test %s ------' % tid)
            fn(self)
            return
        self.out()
        self.out('ERROR: no test with id %r in the catalog' % tid)

    def test_catalog(self):
        """All tests selectable via the "run single test" command:
        (id, label) pairs; ids are unique across core and perf tests."""
        return [(tid, name) for tid, name, _fn in TESTS]


    # ========================================================================
    # STANDALONE CORE TESTS T01..T35
    # Every test contains its complete code: document setup, expected
    # result from the model, the operation, all checks, and the
    # exact-count undo/redo steps. Nothing is shared with other tests.
    # ========================================================================

    def test_T01(self):
        """ed.insert('Q') at (3,1). ed.insert returns the end position and
        does NOT move the caret; undo restores base text + pre-op caret;
        redo restores the op result."""
        L = make_small_lines()
        self.TE.set_text_all(m_join(L))
        self.TE.set_caret(0, 0)
        base = m_join(L)
        x, y, s = 3, 1, 'Q'
        exp = m_insert(L, x, y, s)
        pre = (x, y)
        self.TE.set_caret(x, y)
        self.info('op', 'insert(%d, %d, %r)' % (x, y, s))
        EV['change'] = EV['caret'] = 0
        ret = self.TE.insert(x, y, s)
        ec, ek = EV['change'], EV['caret']
        self.check('text after op', self.TE.get_text_all(), m_join(exp))
        self.check('line_count after op', self.TE.get_line_count(), len(exp))
        # ed.insert() does NOT move the caret - it returns the end
        # position instead (both are asserted; catches regressions in
        # either direction)
        self.check('insert() return value', ret, (x + len(s), y))
        self.check('caret unchanged after ed.insert',
                   self.TE.get_carets()[0][:2], pre)
        # exactly ONE edit was made: undo it with exactly one step and
        # check the exact base state - never a blind drain (set_text_all
        # keeps one more undo entry below, see UNDO/REDO MODEL at top)
        self.TE.cmd(cmds.cCommand_Undo)
        self.check('text after 1 undo', self.TE.get_text_all(), base)
        self.check('line_count after undo', self.TE.get_line_count(), len(L))
        self.check('caret restored after undo', self.TE.get_carets()[0][:2], pre)
        # redo the single edit step
        self.TE.cmd(cmds.cCommand_Redo)
        self.check('text after 1 redo', self.TE.get_text_all(), m_join(exp))
        self.info('events during op (change/caret)', '%d / %d' % (ec, ek))

    def test_T02(self):
        """ed.insert of multi-line text 'AB\\ndef\\nghi' at (4,6). Return
        value of a multi-line insert is informational; caret must stay
        put; undo/redo round-trip the whole block."""
        L = make_small_lines()
        self.TE.set_text_all(m_join(L))
        self.TE.set_caret(0, 0)
        base = m_join(L)
        x, y, s = 4, 6, 'AB\ndef\nghi'
        exp = m_insert(L, x, y, s)
        pre = (x, y)
        self.TE.set_caret(x, y)
        self.info('op', 'insert(%d, %d, %r)' % (x, y, s))
        EV['change'] = EV['caret'] = 0
        ret = self.TE.insert(x, y, s)
        ec, ek = EV['change'], EV['caret']
        self.check('text after op', self.TE.get_text_all(), m_join(exp))
        self.check('line_count after op', self.TE.get_line_count(), len(exp))
        self.info('insert() return value (not asserted, multiline)', ret)
        self.check('caret unchanged after ed.insert',
                   self.TE.get_carets()[0][:2], pre)
        # exactly ONE edit was made: undo it with exactly one step and
        # check the exact base state - never a blind drain (set_text_all
        # keeps one more undo entry below, see UNDO/REDO MODEL at top)
        self.TE.cmd(cmds.cCommand_Undo)
        self.check('text after 1 undo', self.TE.get_text_all(), base)
        self.check('line_count after undo', self.TE.get_line_count(), len(L))
        self.check('caret restored after undo', self.TE.get_carets()[0][:2], pre)
        # redo the single edit step
        self.TE.cmd(cmds.cCommand_Redo)
        self.check('text after 1 redo', self.TE.get_text_all(), m_join(exp))
        self.info('events during op (change/caret)', '%d / %d' % (ec, ek))

    def test_T03(self):
        """ed.insert('START') at the very beginning (0,0)."""
        L = make_small_lines()
        self.TE.set_text_all(m_join(L))
        self.TE.set_caret(0, 0)
        base = m_join(L)
        x, y, s = 0, 0, 'START'
        exp = m_insert(L, x, y, s)
        pre = (x, y)
        self.TE.set_caret(x, y)
        self.info('op', 'insert(%d, %d, %r)' % (x, y, s))
        EV['change'] = EV['caret'] = 0
        ret = self.TE.insert(x, y, s)
        ec, ek = EV['change'], EV['caret']
        self.check('text after op', self.TE.get_text_all(), m_join(exp))
        self.check('line_count after op', self.TE.get_line_count(), len(exp))
        self.check('insert() return value', ret, (x + len(s), y))
        self.check('caret unchanged after ed.insert',
                   self.TE.get_carets()[0][:2], pre)
        # exactly ONE edit was made: undo it with exactly one step and
        # check the exact base state - never a blind drain (set_text_all
        # keeps one more undo entry below, see UNDO/REDO MODEL at top)
        self.TE.cmd(cmds.cCommand_Undo)
        self.check('text after 1 undo', self.TE.get_text_all(), base)
        self.check('line_count after undo', self.TE.get_line_count(), len(L))
        self.check('caret restored after undo', self.TE.get_carets()[0][:2], pre)
        # redo the single edit step
        self.TE.cmd(cmds.cCommand_Redo)
        self.check('text after 1 redo', self.TE.get_text_all(), m_join(exp))
        self.info('events during op (change/caret)', '%d / %d' % (ec, ek))

    def test_T04(self):
        """ed.insert('END') at the very end of the document."""
        L = make_small_lines()
        self.TE.set_text_all(m_join(L))
        self.TE.set_caret(0, 0)
        base = m_join(L)
        x, y, s = len(L[79]), 79, 'END'
        exp = m_insert(L, x, y, s)
        pre = (x, y)
        self.TE.set_caret(x, y)
        self.info('op', 'insert(%d, %d, %r)' % (x, y, s))
        EV['change'] = EV['caret'] = 0
        ret = self.TE.insert(x, y, s)
        ec, ek = EV['change'], EV['caret']
        self.check('text after op', self.TE.get_text_all(), m_join(exp))
        self.check('line_count after op', self.TE.get_line_count(), len(exp))
        self.check('insert() return value', ret, (x + len(s), y))
        self.check('caret unchanged after ed.insert',
                   self.TE.get_carets()[0][:2], pre)
        # exactly ONE edit was made: undo it with exactly one step and
        # check the exact base state - never a blind drain (set_text_all
        # keeps one more undo entry below, see UNDO/REDO MODEL at top)
        self.TE.cmd(cmds.cCommand_Undo)
        self.check('text after 1 undo', self.TE.get_text_all(), base)
        self.check('line_count after undo', self.TE.get_line_count(), len(L))
        self.check('caret restored after undo', self.TE.get_carets()[0][:2], pre)
        # redo the single edit step
        self.TE.cmd(cmds.cCommand_Redo)
        self.check('text after 1 redo', self.TE.get_text_all(), m_join(exp))
        self.info('events during op (change/caret)', '%d / %d' % (ec, ek))

    def test_T05(self):
        """ed.insert('abc\\ndef') into an empty document."""
        L = ['']
        self.TE.set_text_all(m_join(L))
        self.TE.set_caret(0, 0)
        base = m_join(L)
        x, y, s = 0, 0, 'abc\ndef'
        exp = m_insert(L, x, y, s)
        pre = (x, y)
        self.TE.set_caret(x, y)
        self.info('op', 'insert(%d, %d, %r)' % (x, y, s))
        EV['change'] = EV['caret'] = 0
        ret = self.TE.insert(x, y, s)
        ec, ek = EV['change'], EV['caret']
        self.check('text after op', self.TE.get_text_all(), m_join(exp))
        self.check('line_count after op', self.TE.get_line_count(), len(exp))
        self.info('insert() return value (not asserted, multiline)', ret)
        self.check('caret unchanged after ed.insert',
                   self.TE.get_carets()[0][:2], pre)
        # exactly ONE edit was made: undo it with exactly one step and
        # check the exact base state - never a blind drain (set_text_all
        # keeps one more undo entry below, see UNDO/REDO MODEL at top)
        self.TE.cmd(cmds.cCommand_Undo)
        self.check('text after 1 undo', self.TE.get_text_all(), base)
        self.check('line_count after undo', self.TE.get_line_count(), len(L))
        self.check('caret restored after undo', self.TE.get_carets()[0][:2], pre)
        # redo the single edit step
        self.TE.cmd(cmds.cCommand_Redo)
        self.check('text after 1 redo', self.TE.get_text_all(), m_join(exp))
        self.info('events during op (change/caret)', '%d / %d' % (ec, ek))

    def test_T06(self):
        """Typing simulation: 12 adjacent single-char inserts.
        PROP_UNDO_GROUPED is forced False so each char is its own undo
        entry (un == 12).  Grouping is restored in finally."""
        try:
            self.TE.set_prop(cudatext.PROP_UNDO_GROUPED, False)
        except Exception:
            pass
        try:
            L = make_small_lines()
            self.TE.set_text_all(m_join(L))
            self.TE.set_caret(0, 0)
            base = m_join(L)
            x, y = 2, 1
            exp = list(L)
            self.TE.set_caret(x, y)
            self.info('op', '12 adjacent single-char inserts (typing simulation)')
            EV['change'] = EV['caret'] = 0
            for i in range(12):
                ch = chr(ord('a') + i)
                self.TE.insert(x + i, y, ch)
                exp = m_insert(exp, x + i, y, ch)
            ec, ek = EV['change'], EV['caret']
            self.check('text after 12 inserts', self.TE.get_text_all(),
                       m_join(exp))
            # at most 12 steps (one per insert); stop at base
            un = 0
            while N(self.TE.get_text_all()) != N(base) and un < 12:
                self.TE.cmd(cmds.cCommand_Undo)
                un += 1
            self.check('text after undoing the typing (base reached)',
                       self.TE.get_text_all(), base)
            self.check('undo steps with grouping off == 12', un, 12)
            self.info('undo steps to reach base', un)
            rn = 0
            while N(self.TE.get_text_all()) != N(m_join(exp)) and rn < un:
                self.TE.cmd(cmds.cCommand_Redo)
                rn += 1
            self.check('text after redoing the typing', self.TE.get_text_all(),
                       m_join(exp))
            self.info('redo steps', rn)
            self.info('events during 12 inserts (change/caret)',
                      '%d / %d' % (ec, ek))
        finally:
            try:
                self.TE.set_prop(cudatext.PROP_UNDO_GROUPED, True)
            except Exception:
                pass

    def test_T07(self):
        """Typing via cmd(cCommand_TextInsert, 'hello') with no selection:
        goes through the command processor, so the caret moves to the
        end of the typed text."""
        L = make_small_lines()
        self.TE.set_text_all(m_join(L))
        self.TE.set_caret(0, 0)
        base = m_join(L)
        x, y, s = 4, 2, 'hello'
        exp = m_insert(L, x, y, s)
        pre = (x, y)
        self.TE.set_caret(x, y)
        self.info('op', 'textinsert(%d, %d, %r)' % (x, y, s))
        EV['change'] = EV['caret'] = 0
        # typing simulation: goes through the command processor,
        # deletes the selection if any, groups undo like typing
        self.TE.cmd(cmds.cCommand_TextInsert, s)
        ec, ek = EV['change'], EV['caret']
        self.check('text after op', self.TE.get_text_all(), m_join(exp))
        self.check('line_count after op', self.TE.get_line_count(), len(exp))
        self.check('caret after op', self.TE.get_carets()[0][:2],
                   (x + len(s), y))
        # exactly ONE edit was made: undo it with exactly one step and
        # check the exact base state - never a blind drain (set_text_all
        # keeps one more undo entry below, see UNDO/REDO MODEL at top)
        self.TE.cmd(cmds.cCommand_Undo)
        self.check('text after 1 undo', self.TE.get_text_all(), base)
        self.check('line_count after undo', self.TE.get_line_count(), len(L))
        self.check('caret restored after undo', self.TE.get_carets()[0][:2], pre)
        # redo the single edit step
        self.TE.cmd(cmds.cCommand_Redo)
        self.check('text after 1 redo', self.TE.get_text_all(), m_join(exp))
        self.info('events during op (change/caret)', '%d / %d' % (ec, ek))

    def test_T08(self):
        """Typing via cmd(cCommand_TextInsert, 'XYZ') over an existing
        selection: the selection is replaced; undo restores both text
        and selection."""
        L = make_small_lines()
        self.TE.set_text_all(m_join(L))
        self.TE.set_caret(0, 0)
        base = m_join(L)
        x1, y1, x2, y2 = 2, 2, 6, 2
        self.TE.set_caret(x1, y1, x2, y2)
        pre = self.TE.get_carets()
        exp = m_insert(m_delete(L, x1, y1, x2, y2), x1, y1, 'XYZ')
        self.info('op', "cmd(TextInsert, 'XYZ') over selection (%d,%d)-(%d,%d)" % (
            x1, y1, x2, y2))
        EV['change'] = EV['caret'] = 0
        self.TE.cmd(cmds.cCommand_TextInsert, 'XYZ')
        ec, ek = EV['change'], EV['caret']
        self.check('text after op', self.TE.get_text_all(), m_join(exp))
        self.check('line_count after op', self.TE.get_line_count(), len(exp))
        # exactly ONE edit was made: undo it with exactly one step and
        # check the exact base state - never a blind drain (set_text_all
        # keeps one more undo entry below, see UNDO/REDO MODEL at top)
        self.TE.cmd(cmds.cCommand_Undo)
        self.check('text after 1 undo', self.TE.get_text_all(), base)
        self.check('selection restored after undo', self.TE.get_carets(), pre)
        # redo the single edit step
        self.TE.cmd(cmds.cCommand_Redo)
        self.check('text after 1 redo', self.TE.get_text_all(), m_join(exp))
        self.info('events during op (change/caret)', '%d / %d' % (ec, ek))

    def test_T09(self):
        """Backspace key in the middle of a line: deletes the char before
        the caret."""
        L = make_small_lines()
        self.TE.set_text_all(m_join(L))
        self.TE.set_caret(0, 0)
        base = m_join(L)
        x, y = 5, 1
        exp = m_delete(L, x - 1, y, x, y)
        pre = (x, y)
        self.TE.set_caret(x, y)
        self.info('op', 'key cCommand_KeyBackspace at (%d, %d)' % (x, y))
        EV['change'] = EV['caret'] = 0
        self.TE.cmd(cmds.cCommand_KeyBackspace)
        ec, ek = EV['change'], EV['caret']
        self.check('text after op', self.TE.get_text_all(), m_join(exp))
        self.check('line_count after op', self.TE.get_line_count(), len(exp))
        # exactly ONE edit was made: undo it with exactly one step and
        # check the exact base state - never a blind drain (set_text_all
        # keeps one more undo entry below, see UNDO/REDO MODEL at top)
        self.TE.cmd(cmds.cCommand_Undo)
        self.check('text after 1 undo', self.TE.get_text_all(), base)
        self.check('line_count after undo', self.TE.get_line_count(), len(L))
        self.check('caret restored after undo', self.TE.get_carets()[0][:2], pre)
        # redo the single edit step
        self.TE.cmd(cmds.cCommand_Redo)
        self.check('text after 1 redo', self.TE.get_text_all(), m_join(exp))
        self.info('events during op (change/caret)', '%d / %d' % (ec, ek))

    def test_T10(self):
        """Backspace key at line start: joins the line with the previous
        one."""
        L = make_small_lines()
        self.TE.set_text_all(m_join(L))
        self.TE.set_caret(0, 0)
        base = m_join(L)
        x, y = 0, 4
        exp = m_delete(L, len(L[y - 1]), y - 1, 0, y)
        pre = (x, y)
        self.TE.set_caret(x, y)
        self.info('op', 'key cCommand_KeyBackspace at (%d, %d)' % (x, y))
        EV['change'] = EV['caret'] = 0
        self.TE.cmd(cmds.cCommand_KeyBackspace)
        ec, ek = EV['change'], EV['caret']
        self.check('text after op', self.TE.get_text_all(), m_join(exp))
        self.check('line_count after op', self.TE.get_line_count(), len(exp))
        # exactly ONE edit was made: undo it with exactly one step and
        # check the exact base state - never a blind drain (set_text_all
        # keeps one more undo entry below, see UNDO/REDO MODEL at top)
        self.TE.cmd(cmds.cCommand_Undo)
        self.check('text after 1 undo', self.TE.get_text_all(), base)
        self.check('line_count after undo', self.TE.get_line_count(), len(L))
        self.check('caret restored after undo', self.TE.get_carets()[0][:2], pre)
        # redo the single edit step
        self.TE.cmd(cmds.cCommand_Redo)
        self.check('text after 1 redo', self.TE.get_text_all(), m_join(exp))
        self.info('events during op (change/caret)', '%d / %d' % (ec, ek))

    def test_T11(self):
        """Delete key in the middle of a line: deletes the char after the
        caret."""
        L = make_small_lines()
        self.TE.set_text_all(m_join(L))
        self.TE.set_caret(0, 0)
        base = m_join(L)
        x, y = 3, 6
        exp = m_delete(L, x, y, x + 1, y)
        pre = (x, y)
        self.TE.set_caret(x, y)
        self.info('op', 'key cCommand_KeyDelete at (%d, %d)' % (x, y))
        EV['change'] = EV['caret'] = 0
        self.TE.cmd(cmds.cCommand_KeyDelete)
        ec, ek = EV['change'], EV['caret']
        self.check('text after op', self.TE.get_text_all(), m_join(exp))
        self.check('line_count after op', self.TE.get_line_count(), len(exp))
        # exactly ONE edit was made: undo it with exactly one step and
        # check the exact base state - never a blind drain (set_text_all
        # keeps one more undo entry below, see UNDO/REDO MODEL at top)
        self.TE.cmd(cmds.cCommand_Undo)
        self.check('text after 1 undo', self.TE.get_text_all(), base)
        self.check('line_count after undo', self.TE.get_line_count(), len(L))
        self.check('caret restored after undo', self.TE.get_carets()[0][:2], pre)
        # redo the single edit step
        self.TE.cmd(cmds.cCommand_Redo)
        self.check('text after 1 redo', self.TE.get_text_all(), m_join(exp))
        self.info('events during op (change/caret)', '%d / %d' % (ec, ek))

    def test_T12(self):
        """Delete key at line end: joins the line with the next one."""
        L = make_small_lines()
        self.TE.set_text_all(m_join(L))
        self.TE.set_caret(0, 0)
        base = m_join(L)
        x, y = len(L[5]), 5
        exp = m_delete(L, len(L[y]), y, 0, y + 1)
        pre = (x, y)
        self.TE.set_caret(x, y)
        self.info('op', 'key cCommand_KeyDelete at (%d, %d)' % (x, y))
        EV['change'] = EV['caret'] = 0
        self.TE.cmd(cmds.cCommand_KeyDelete)
        ec, ek = EV['change'], EV['caret']
        self.check('text after op', self.TE.get_text_all(), m_join(exp))
        self.check('line_count after op', self.TE.get_line_count(), len(exp))
        # exactly ONE edit was made: undo it with exactly one step and
        # check the exact base state - never a blind drain (set_text_all
        # keeps one more undo entry below, see UNDO/REDO MODEL at top)
        self.TE.cmd(cmds.cCommand_Undo)
        self.check('text after 1 undo', self.TE.get_text_all(), base)
        self.check('line_count after undo', self.TE.get_line_count(), len(L))
        self.check('caret restored after undo', self.TE.get_carets()[0][:2], pre)
        # redo the single edit step
        self.TE.cmd(cmds.cCommand_Redo)
        self.check('text after 1 redo', self.TE.get_text_all(), m_join(exp))
        self.info('events during op (change/caret)', '%d / %d' % (ec, ek))

    def test_T13(self):
        """cCommand_TextDeleteSelection of a forward selection on one
        line, (2,2)-(6,2)."""
        L = make_small_lines()
        self.TE.set_text_all(m_join(L))
        self.TE.set_caret(0, 0)
        base = m_join(L)
        x1, y1, x2, y2 = 2, 2, 6, 2
        if (y1, x1) > (y2, x2):
            mx1, my1, mx2, my2 = x2, y2, x1, y1
        else:
            mx1, my1, mx2, my2 = x1, y1, x2, y2
        exp = m_delete(L, mx1, my1, mx2, my2)
        self.TE.set_caret(x1, y1, x2, y2)
        pre = self.TE.get_carets()
        self.info('op', 'TextDeleteSelection of (%d,%d)-(%d,%d)' % (x1, y1, x2, y2))
        EV['change'] = EV['caret'] = 0
        self.TE.cmd(cmds.cCommand_TextDeleteSelection)
        ec, ek = EV['change'], EV['caret']
        self.check('text after op', self.TE.get_text_all(), m_join(exp))
        self.check('line_count after op', self.TE.get_line_count(), len(exp))
        # exactly ONE edit was made: undo it with exactly one step and
        # check the exact base state - never a blind drain (set_text_all
        # keeps one more undo entry below, see UNDO/REDO MODEL at top)
        self.TE.cmd(cmds.cCommand_Undo)
        self.check('text after 1 undo', self.TE.get_text_all(), base)
        self.check('line_count after undo', self.TE.get_line_count(), len(L))
        self.check('selection restored after undo', self.TE.get_carets(), pre)
        # redo the single edit step
        self.TE.cmd(cmds.cCommand_Redo)
        self.check('text after 1 redo', self.TE.get_text_all(), m_join(exp))
        self.info('events during op (change/caret)', '%d / %d' % (ec, ek))

    def test_T14(self):
        """cCommand_TextDeleteSelection of a BACKWARD selection (made
        from right to left), (6,2)-(2,2): the delete must be of the same
        range as the forward one."""
        L = make_small_lines()
        self.TE.set_text_all(m_join(L))
        self.TE.set_caret(0, 0)
        base = m_join(L)
        x1, y1, x2, y2 = 6, 2, 2, 2
        if (y1, x1) > (y2, x2):
            mx1, my1, mx2, my2 = x2, y2, x1, y1
        else:
            mx1, my1, mx2, my2 = x1, y1, x2, y2
        exp = m_delete(L, mx1, my1, mx2, my2)
        self.TE.set_caret(x1, y1, x2, y2)
        pre = self.TE.get_carets()
        self.info('op', 'TextDeleteSelection of (%d,%d)-(%d,%d) (backward)' % (
            x1, y1, x2, y2))
        EV['change'] = EV['caret'] = 0
        self.TE.cmd(cmds.cCommand_TextDeleteSelection)
        ec, ek = EV['change'], EV['caret']
        self.check('text after op', self.TE.get_text_all(), m_join(exp))
        self.check('line_count after op', self.TE.get_line_count(), len(exp))
        # exactly ONE edit was made: undo it with exactly one step and
        # check the exact base state - never a blind drain (set_text_all
        # keeps one more undo entry below, see UNDO/REDO MODEL at top)
        self.TE.cmd(cmds.cCommand_Undo)
        self.check('text after 1 undo', self.TE.get_text_all(), base)
        self.check('line_count after undo', self.TE.get_line_count(), len(L))
        self.check('selection restored after undo', self.TE.get_carets(), pre)
        # redo the single edit step
        self.TE.cmd(cmds.cCommand_Redo)
        self.check('text after 1 redo', self.TE.get_text_all(), m_join(exp))
        self.info('events during op (change/caret)', '%d / %d' % (ec, ek))

    def test_T15(self):
        """cCommand_TextDeleteSelection of a multi-line selection,
        (3,10)-(2,40)."""
        L = make_small_lines()
        self.TE.set_text_all(m_join(L))
        self.TE.set_caret(0, 0)
        base = m_join(L)
        x1, y1, x2, y2 = 3, 10, 2, 41
        if (y1, x1) > (y2, x2):
            mx1, my1, mx2, my2 = x2, y2, x1, y1
        else:
            mx1, my1, mx2, my2 = x1, y1, x2, y2
        exp = m_delete(L, mx1, my1, mx2, my2)
        self.TE.set_caret(x1, y1, x2, y2)
        pre = self.TE.get_carets()
        self.info('op', 'TextDeleteSelection of (%d,%d)-(%d,%d)' % (x1, y1, x2, y2))
        EV['change'] = EV['caret'] = 0
        self.TE.cmd(cmds.cCommand_TextDeleteSelection)
        ec, ek = EV['change'], EV['caret']
        self.check('text after op', self.TE.get_text_all(), m_join(exp))
        self.check('line_count after op', self.TE.get_line_count(), len(exp))
        # exactly ONE edit was made: undo it with exactly one step and
        # check the exact base state - never a blind drain (set_text_all
        # keeps one more undo entry below, see UNDO/REDO MODEL at top)
        self.TE.cmd(cmds.cCommand_Undo)
        self.check('text after 1 undo', self.TE.get_text_all(), base)
        self.check('line_count after undo', self.TE.get_line_count(), len(L))
        self.check('selection restored after undo', self.TE.get_carets(), pre)
        # redo the single edit step
        self.TE.cmd(cmds.cCommand_Redo)
        self.check('text after 1 redo', self.TE.get_text_all(), m_join(exp))
        self.info('events during op (change/caret)', '%d / %d' % (ec, ek))

    def test_T16(self):
        """cCommand_TextDeleteSelection of a selection reaching to EOF,
        (5,70)-(end,79)."""
        L = make_small_lines()
        self.TE.set_text_all(m_join(L))
        self.TE.set_caret(0, 0)
        base = m_join(L)
        x1, y1, x2, y2 = 5, 70, len(L[79]), 79
        if (y1, x1) > (y2, x2):
            mx1, my1, mx2, my2 = x2, y2, x1, y1
        else:
            mx1, my1, mx2, my2 = x1, y1, x2, y2
        exp = m_delete(L, mx1, my1, mx2, my2)
        self.TE.set_caret(x1, y1, x2, y2)
        pre = self.TE.get_carets()
        self.info('op', 'TextDeleteSelection of (%d,%d)-(%d,%d) (to EOF)' % (
            x1, y1, x2, y2))
        EV['change'] = EV['caret'] = 0
        self.TE.cmd(cmds.cCommand_TextDeleteSelection)
        ec, ek = EV['change'], EV['caret']
        self.check('text after op', self.TE.get_text_all(), m_join(exp))
        self.check('line_count after op', self.TE.get_line_count(), len(exp))
        # exactly ONE edit was made: undo it with exactly one step and
        # check the exact base state - never a blind drain (set_text_all
        # keeps one more undo entry below, see UNDO/REDO MODEL at top)
        self.TE.cmd(cmds.cCommand_Undo)
        self.check('text after 1 undo', self.TE.get_text_all(), base)
        self.check('line_count after undo', self.TE.get_line_count(), len(L))
        self.check('selection restored after undo', self.TE.get_carets(), pre)
        # redo the single edit step
        self.TE.cmd(cmds.cCommand_Redo)
        self.check('text after 1 redo', self.TE.get_text_all(), m_join(exp))
        self.info('events during op (change/caret)', '%d / %d' % (ec, ek))

    def test_T17(self):
        """Select the whole document and delete it with
        cCommand_TextDeleteSelection."""
        L = make_small_lines()
        self.TE.set_text_all(m_join(L))
        self.TE.set_caret(0, 0)
        base = m_join(L)
        x1, y1, x2, y2 = 0, 0, len(L[79]), 79
        if (y1, x1) > (y2, x2):
            mx1, my1, mx2, my2 = x2, y2, x1, y1
        else:
            mx1, my1, mx2, my2 = x1, y1, x2, y2
        exp = m_delete(L, mx1, my1, mx2, my2)
        self.TE.set_caret(x1, y1, x2, y2)
        pre = self.TE.get_carets()
        self.info('op', 'TextDeleteSelection of (%d,%d)-(%d,%d) (all)' % (
            x1, y1, x2, y2))
        EV['change'] = EV['caret'] = 0
        self.TE.cmd(cmds.cCommand_TextDeleteSelection)
        ec, ek = EV['change'], EV['caret']
        self.check('text after op', self.TE.get_text_all(), m_join(exp))
        self.check('line_count after op', self.TE.get_line_count(), len(exp))
        # exactly ONE edit was made: undo it with exactly one step and
        # check the exact base state - never a blind drain (set_text_all
        # keeps one more undo entry below, see UNDO/REDO MODEL at top)
        self.TE.cmd(cmds.cCommand_Undo)
        self.check('text after 1 undo', self.TE.get_text_all(), base)
        self.check('line_count after undo', self.TE.get_line_count(), len(L))
        self.check('selection restored after undo', self.TE.get_carets(), pre)
        # redo the single edit step
        self.TE.cmd(cmds.cCommand_Redo)
        self.check('text after 1 redo', self.TE.get_text_all(), m_join(exp))
        self.info('events during op (change/caret)', '%d / %d' % (ec, ek))

    def test_T18(self):
        """ed.delete(x1,y1,x2,y2) crossing a newline: tail of one line +
        head of the next disappear, lines join."""
        L = make_small_lines()
        self.TE.set_text_all(m_join(L))
        self.TE.set_caret(0, 0)
        base = m_join(L)
        x, y = len(L[3]), 3
        x2, y2 = 4, 4
        exp = m_delete(L, x, y, x2, y2)      # crosses one newline
        pre = (x, y)
        self.TE.set_caret(x, y)
        self.info('op', 'ed.delete(%d, %d, %d, %d) - crosses a newline' % (
            x, y, x2, y2))
        EV['change'] = EV['caret'] = 0
        self.TE.delete(x, y, x2, y2)
        ec, ek = EV['change'], EV['caret']
        self.check('text after op', self.TE.get_text_all(), m_join(exp))
        self.check('line_count after op', self.TE.get_line_count(), len(exp))
        # exactly ONE edit was made: undo it with exactly one step and
        # check the exact base state - never a blind drain (set_text_all
        # keeps one more undo entry below, see UNDO/REDO MODEL at top)
        self.TE.cmd(cmds.cCommand_Undo)
        self.check('text after 1 undo', self.TE.get_text_all(), base)
        self.check('line_count after undo', self.TE.get_line_count(), len(L))
        self.check('caret restored after undo', self.TE.get_carets()[0][:2], pre)
        # redo the single edit step
        self.TE.cmd(cmds.cCommand_Redo)
        self.check('text after 1 redo', self.TE.get_text_all(), m_join(exp))
        self.info('events during op (change/caret)', '%d / %d' % (ec, ek))


    def test_T19(self):
        """30 sequential line deletions (select line + TextDeleteSelection
        each time), then the full stack must undo back to the base."""
        L = make_small_lines()
        self.TE.set_text_all(m_join(L))
        self.TE.set_caret(0, 0)
        base = m_join(L)
        cur = list(L)
        snaps = [m_join(L)]          # one snapshot per applied op
        y = 10
        self.info('op', '30x: select line %d and TextDeleteSelection' % y)
        for _i in range(30):
            self.TE.set_caret(0, y, 0, y + 1)
            self.TE.cmd(cmds.cCommand_TextDeleteSelection)
            cur = m_delete(cur, 0, y, 0, y + 1)
            snaps.append(m_join(cur))
        self.check('text after 30 line deletes', self.TE.get_text_all(), m_join(cur))
        self.check('line_count after deletes', self.TE.get_line_count(), len(cur))
        # exactly 30 undo steps, one per deletion; every step must land
        # on the exact reverse snapshot (no blind drain - set_text_all
        # keeps one more entry below, see UNDO/REDO MODEL at top)
        bad = []
        for i in range(30):
            self.TE.cmd(cmds.cCommand_Undo)
            if N(self.TE.get_text_all()) != N(snaps[29 - i]):
                bad.append(i + 1)
        self.check('every undo step lands on the exact reverse state', bad, [])
        self.check('text after 30 undos == base', self.TE.get_text_all(), base)
        self.check('line_count after undo', self.TE.get_line_count(), len(L))
        # exactly 30 redo steps back through the forward snapshots
        bad2 = []
        for i in range(30):
            self.TE.cmd(cmds.cCommand_Redo)
            if N(self.TE.get_text_all()) != N(snaps[i + 1]):
                bad2.append(i + 1)
        self.check('every redo step lands on the exact forward state', bad2, [])
        self.check('text after 30 redos == final', self.TE.get_text_all(),
                   m_join(cur))
        self.info('undo/redo steps: 30 / 30 (exactly one entry per deletion)')

    def test_T20(self):
        """Random storm: 150 seeded mixed inserts and deletes.
        Verifies: final text equals pure model, exactly N undo entries,
        full undo reaches base, full redo reaches the storm result.
        (Intermediate bit-identical restoration is not required; the
        editor may normalise trailing newlines / empty lines on undo.)"""
        rng = random.Random(SEED + 1234)
        L = make_small_lines()
        self.TE.set_text_all(m_join(L))
        self.TE.set_caret(0, 0)
        base = m_join(L)
        cur = list(L)
        nops = 0
        self.info('op', '150 random inserts / deletes (seeded)')
        for i in range(150):
            if rng.random() < 0.55 or len(cur) < 4:
                y = rng.randrange(len(cur))
                x = rng.randrange(len(cur[y]) + 1)
                s = ('a%d\nb%d\nc' % (i, i) if rng.random() < 0.15
                     else 'w%d%s' % (i, 'q' * rng.randint(1, 8)))
                self.TE.insert(x, y, s)
                cur = m_insert(cur, x, y, s)
                nops += 1
            else:
                y1 = rng.randrange(len(cur) - 1)
                y2 = rng.randrange(y1 + 1, min(y1 + 6, len(cur)))
                x1 = rng.randrange(len(cur[y1]) + 1)
                x2 = rng.randrange(len(cur[y2]) + 1)
                if (y1, x1) == (y2, x2):
                    x2 = (x2 + 1) % (len(cur[y2]) + 1)
                    if x2 == x1:
                        continue
                if (y1, x1) > (y2, x2):
                    y1, x1, y2, x2 = y2, x2, y1, x1
                self.TE.set_caret(x1, y1)
                self.TE.delete(x1, y1, x2, y2)
                cur = m_delete(cur, x1, y1, x2, y2)
                nops += 1

        self.check('text after storm', self.TE.get_text_all(), m_join(cur))
        self.check('line_count after storm', self.TE.get_line_count(), len(cur))
        self.info('ops actually applied', nops)

        # exactly nops undos must reach base
        for _ in range(nops):
            self.TE.cmd(cmds.cCommand_Undo)
        self.check('text after %d undos == base' % nops,
                   self.TE.get_text_all(), base)
        self.check('line_count after undo', self.TE.get_line_count(), len(L))

        # exactly nops redos must reach the storm result
        for _ in range(nops):
            self.TE.cmd(cmds.cCommand_Redo)
        self.check('text after full redo', self.TE.get_text_all(), m_join(cur))
        self.info('undo/redo steps: %d / %d (exactly one entry per op)'
                  % (nops, nops))
                   
    def test_T21(self):
        """Unicode roundtrip: insert 4 non-ASCII chars, then delete them;
        undo/redo must restore exact unicode text (UTF-16 coords).
        PROP_UNDO_GROUPED is False so the insert and the delete stay as
        two separate undo entries (with grouping on they coalesce).
        Grouping is restored in finally."""
        try:
            self.TE.set_prop(cudatext.PROP_UNDO_GROUPED, False)
        except Exception:
            pass
        try:
            L = make_small_lines()
            self.TE.set_text_all(m_join(L))
            self.TE.set_caret(0, 0)
            base = m_join(L)
            s = '中Äßé'
            x, y = 2, 3
            exp1 = m_insert(L, x, y, s)
            self.TE.set_caret(x, y)
            self.TE.insert(x, y, s)
            self.check('text after unicode insert', self.TE.get_text_all(),
                       m_join(exp1))
            # delete 4 unicode chars starting at (1, 3)
            x2, y2 = 1, 3
            exp2 = m_delete(exp1, x2, y2, x2 + 4, y2)
            self.TE.set_caret(x2, y2)
            self.TE.delete(x2, y2, x2 + 4, y2)
            self.check('text after unicode delete', self.TE.get_text_all(),
                       m_join(exp2))
            # exactly 2 edits: undo one by one
            self.TE.cmd(cmds.cCommand_Undo)     # undo #1: the delete
            self.check('text after undo #1 (delete undone)',
                       self.TE.get_text_all(), m_join(exp1))
            self.TE.cmd(cmds.cCommand_Undo)     # undo #2: the insert
            self.check('text after undo #2 (insert undone) == base',
                       self.TE.get_text_all(), base)
            self.TE.cmd(cmds.cCommand_Redo)     # redo #1: the insert
            self.check('text after redo #1 (insert redone)',
                       self.TE.get_text_all(), m_join(exp1))
            self.TE.cmd(cmds.cCommand_Redo)     # redo #2: the delete
            self.check('text after redo #2 (delete redone)',
                       self.TE.get_text_all(), m_join(exp2))
            self.info('undo/redo steps: 2 / 2 (exactly one entry per edit)')
        finally:
            try:
                self.TE.set_prop(cudatext.PROP_UNDO_GROUPED, True)
            except Exception:
                pass
        
    def test_T22(self):
        """Tab char + EOL fidelity (raw snapshot compare): undo of a tab
        insert must restore EXACT raw text; then the same while the doc's
        EOL kind is toggled (PROP_NEWLINE)."""
        L = make_small_lines()
        self.TE.set_text_all(m_join(L))
        self.TE.set_caret(0, 0)
        base_raw = self.TE.get_text_all()
        # 1) tab char insert: undo must restore EXACT raw text
        # (exactly 1 edit: 1 undo step, 1 redo step)
        x, y = 3, 6
        self.TE.set_caret(x, y)
        self.TE.insert(x, y, '\t')
        snap = self.TE.get_text_all()
        self.info('line after tab insert', repr(self.TE.get_text_line(y))[:80])
        self.TE.cmd(cmds.cCommand_Undo)
        self.check('raw text after undo of tab insert', self.TE.get_text_all(),
                   base_raw)
        self.TE.cmd(cmds.cCommand_Redo)
        self.check('raw text after redo of tab insert', self.TE.get_text_all(),
                   snap)
        # 2) EOL toggle: fidelity only (snapshot-based); the document's
        # line-ending kind is the str property PROP_NEWLINE ("lf"/"crlf"/"cr")
        e0 = self.TE.get_prop(cudatext.PROP_NEWLINE)
        try:
            self.TE.set_prop(cudatext.PROP_NEWLINE, 'crlf' if e0 != 'crlf' else 'lf')
            e1 = self.TE.get_prop(cudatext.PROP_NEWLINE)
            self.info('PROP_NEWLINE toggled', '%r -> %r' % (e0, e1))
            self.TE.set_caret(2, 2)
            self.TE.insert(2, 2, 'e')
            snap2 = self.TE.get_text_all()
            # exactly 1 edit under the toggled EOL: 1 undo (back to the
            # pre-edit state), 1 redo
            self.TE.cmd(cmds.cCommand_Undo)
            self.check('raw text after undo with toggled EOL',
                       self.TE.get_text_all(), snap)
            self.TE.cmd(cmds.cCommand_Redo)
            self.check('raw text after redo with toggled EOL',
                       self.TE.get_text_all(), snap2)
        finally:
            # restore the original EOL kind for the following tests
            self.TE.set_prop(cudatext.PROP_NEWLINE, e0)

    def test_T23(self):
        """The set_text_all undo entry (user-verified in the CudaText
        console): set_text_all does NOT clear the undo stack - it keeps
        ONE entry, and undoing it yields the EMPTY document. With
        set_text_all('') + one insert there are exactly 2 undo steps.
        This test traces them exactly: insert, undo, undo (the kept
        entry - this is the click after which the undo/redo buttons
        grey out), an extra undo that must be a no-op, redo (the kept
        entry re-applied), redo (the insert), and an extra redo that
        must be a no-op."""
        self.TE.set_text_all('')
        self.TE.set_caret(0, 0)
        self.TE.insert(0, 0, 'abc\ndef')
        self.check('text after insert', self.TE.get_text_all(), 'abc\ndef')
        # undo #1: removes the insert
        self.TE.cmd(cmds.cCommand_Undo)
        self.check('text after 1st undo', self.TE.get_text_all(), '')
        # undo #2: removes the kept set_text_all entry - still the empty
        # document; this is the click after which the buttons grey out
        self.TE.cmd(cmds.cCommand_Undo)
        self.check('text after 2nd undo (the kept set_text_all entry)',
                   self.TE.get_text_all(), '')
        # extra undo on the exhausted stack: no-op, text stable
        self.TE.cmd(cmds.cCommand_Undo)
        self.check('extra undo on empty stack is a no-op',
                   self.TE.get_text_all(), '')
        # redo #1: re-applies the kept set_text_all entry (empty again)
        self.TE.cmd(cmds.cCommand_Redo)
        self.check('text after 1st redo (the kept entry re-applied)',
                   self.TE.get_text_all(), '')
        # redo #2: re-applies the insert
        self.TE.cmd(cmds.cCommand_Redo)
        self.check('text after 2nd redo', self.TE.get_text_all(), 'abc\ndef')
        # extra redo on the exhausted stack: no-op, text stable
        self.TE.cmd(cmds.cCommand_Redo)
        self.check('extra redo on empty stack is a no-op',
                   self.TE.get_text_all(), 'abc\ndef')
        self.info('traced: insert, 2 undos (+1 no-op), 2 redos (+1 no-op)')
        self.info('no exception from undo/redo on empty stacks')

    def test_T24(self):
        """Redo idempotence: after an edit is undone and redone again the
        redo stack is exhausted; extra redo calls must keep the text
        stable and not raise. Undo/redo use the exact step count (1
        edit = 1 step; the extra set_text_all entry is never popped
        here)."""
        L = make_small_lines()
        self.TE.set_text_all(m_join(L))
        self.TE.set_caret(0, 0)
        exp = m_insert(L, 3, 1, 'Z')
        self.TE.set_caret(3, 1)
        self.TE.insert(3, 1, 'Z')
        # exactly 1 edit: undo once, redo once
        self.TE.cmd(cmds.cCommand_Undo)
        self.check('undo returns to base', self.TE.get_text_all(), m_join(L))
        self.TE.cmd(cmds.cCommand_Redo)
        self.check('redo returns to op state', self.TE.get_text_all(),
                   m_join(exp))
        stable = True
        for i in range(5):
            self.TE.cmd(cmds.cCommand_Redo)
            t = self.TE.get_text_all()
            if t != m_join(exp):
                stable = False
                self.check('extra redo #%d keeps text' % (i + 1), t, m_join(exp))
        if stable:
            self.check('5 extra redos on empty stack keep text stable',
                       True, True)
        self.info('no exception from redo on empty stack')

    def test_T25(self):
        """Redo invalidation: after undo, a NEW edit must clear the redo
        stack - the old undone future must not resurrect.
        Temporarily sets PROP_UNDO_GROUPED=False so the two inserts are
        separate undo entries (with grouping on, one Undo would remove
        both A1 and B2).  Grouping is restored afterwards."""
        # need exact 1-entry-per-insert so "undo once" lands on st1
        try:
            self.TE.set_prop(cudatext.PROP_UNDO_GROUPED, False)
        except Exception:
            pass
        try:
            L = make_small_lines()
            self.TE.set_text_all(m_join(L))
            self.TE.set_caret(0, 0)
            base = m_join(L)
            # two far-apart inserts (A1, B2), then undo EXACTLY one step
            # (the B2 insert) - specific step count, no grouping-tolerant
            # walk, and the set_text_all entry below is never popped
            p1 = (5, 2)
            p2 = (5, 41)
            p3 = (5, 61)
            st1 = m_insert(L, p1[0], p1[1], 'A1 ')
            self.TE.set_caret(p1[0], p1[1])
            self.TE.insert(p1[0], p1[1], 'A1 ')
            st2 = m_insert(st1, p2[0], p2[1], 'B2 ')
            self.TE.set_caret(p2[0], p2[1])
            self.TE.insert(p2[0], p2[1], 'B2 ')
            self.check('text after 2 inserts', self.TE.get_text_all(), m_join(st2))
            # undo exactly 1 step: the B2 insert
            self.TE.cmd(cmds.cCommand_Undo)
            self.check('undo lands on the intermediate state',
                       self.TE.get_text_all(), m_join(st1))
            # new edit must clear the redo stack: the undone B2 must NOT
            # resurrect on redo
            exp3 = m_insert(st1, p3[0], p3[1], 'C3 ')
            self.TE.set_caret(p3[0], p3[1])
            self.TE.insert(p3[0], p3[1], 'C3 ')
            self.check('text after new edit C3', self.TE.get_text_all(),
                       m_join(exp3))
            self.TE.cmd(cmds.cCommand_Redo)
            self.check('redo does not resurrect the undone B2',
                       self.TE.get_text_all(), m_join(exp3))
            self.TE.cmd(cmds.cCommand_Redo)
            self.check('2nd redo is still a no-op', self.TE.get_text_all(),
                       m_join(exp3))
        finally:
            try:
                self.TE.set_prop(cudatext.PROP_UNDO_GROUPED, True)
            except Exception:
                pass

    def test_T26(self):
        """Modified flag / save marker: save, edit, undo to the save
        point, redo - PROP_MODIFIED must track the save marker through
        the undo/redo stack."""
        L = make_small_lines()
        self.TE.set_text_all(m_join(L))
        self.TE.set_caret(0, 0)
        base = m_join(L)
        fd, path = tempfile.mkstemp(prefix='urtest_', suffix='.txt')
        os.close(fd)
        try:
            self.TE.save(path)
            m0 = self.TE.get_prop(cudatext.PROP_MODIFIED)
            self.info('modified flag right after save', repr(m0))
            self.check('modified==False after save', bool(m0), False)
            self.TE.set_caret(4, 2)
            self.TE.insert(4, 2, 'CHANGED')
            self.check('modified==True after edit',
                       bool(self.TE.get_prop(cudatext.PROP_MODIFIED)), True)
            # exactly 1 edit after the save: undo it with 1 step
            self.TE.cmd(cmds.cCommand_Undo)
            self.check('text after 1 undo == saved state',
                       self.TE.get_text_all(), base)
            self.check('modified==False after undo to save point',
                       bool(self.TE.get_prop(cudatext.PROP_MODIFIED)), False)
            self.TE.cmd(cmds.cCommand_Redo)
            self.check('text after redo', self.TE.get_text_all(),
                       m_join(m_insert(L, 4, 2, 'CHANGED')))
            self.check('modified==True after redo',
                       bool(self.TE.get_prop(cudatext.PROP_MODIFIED)), True)
            self.info('save-marker tracked through undo/redo correctly')
        finally:
            try:
                self.TE.set_prop(cudatext.PROP_MODIFIED, False)
            except Exception:
                pass
            try:
                os.remove(path)
            except OSError:
                pass

    def test_T27(self):
        """Undo/redo with a live selection somewhere else in the doc: the
        selection must not disturb the undo or its result."""
        L = make_small_lines()
        self.TE.set_text_all(m_join(L))
        self.TE.set_caret(0, 0)
        base = m_join(L)
        self.TE.set_caret(3, 1)
        self.TE.insert(3, 1, 'PRE ')
        after = m_join(m_insert(L, 3, 1, 'PRE '))
        # live selection elsewhere, then undo
        self.TE.set_caret(2, 30, 9, 30)
        EV['change'] = EV['caret'] = 0
        self.TE.cmd(cmds.cCommand_Undo)
        ec, ek = EV['change'], EV['caret']
        self.check('undo with live selection: text==base',
                   self.TE.get_text_all(), base)
        # live selection again, then redo
        self.TE.set_caret(2, 50, 9, 50)
        self.TE.cmd(cmds.cCommand_Redo)
        self.check('redo with live selection: text==base+PRE',
                   self.TE.get_text_all(), after)
        self.info('events during undo (change/caret)', '%d / %d' % (ec, ek))

    def test_T28(self):
        """Tab switch away and back around an undo: switching focus must
        not corrupt the undo stack."""
        L = make_small_lines()
        self.TE.set_text_all(m_join(L))
        self.TE.set_caret(0, 0)
        base = m_join(L)
        exp = m_insert(L, 3, 1, 'SWITCHED')
        self.TE.set_caret(3, 1)
        self.TE.insert(3, 1, 'SWITCHED')
        after = self.TE.get_text_all()
        self.orig.focus()
        try:
            self.check('get_text_all on inactive test tab',
                       self.TE.get_text_all(), after)
        finally:
            self.TE.focus()
        # exactly 1 edit around the tab round-trip: 1 undo, 1 redo
        self.TE.cmd(cmds.cCommand_Undo)
        self.check('undo after tab round-trip', self.TE.get_text_all(), base)
        self.TE.cmd(cmds.cCommand_Redo)
        self.check('redo after tab round-trip', self.TE.get_text_all(), after)

    def test_T29(self):
        """Word wrap toggled between undo and redo: wrap changes must not
        affect the undo stack contents."""
        L = make_small_lines()
        self.TE.set_text_all(m_join(L))
        self.TE.set_caret(0, 0)
        base = m_join(L)
        exp = m_insert(L, 3, 1, 'WRAP')
        try:
            self.TE.set_prop(cudatext.PROP_WRAP, 0)
            self.TE.set_caret(3, 1)
            self.TE.insert(3, 1, 'WRAP')
            self.check('text after insert (wrap off)',
                       self.TE.get_text_all(), m_join(exp))
            self.TE.set_prop(cudatext.PROP_WRAP, 1)
            # exactly 1 edit between the wrap toggles: 1 undo, 1 redo
            self.TE.cmd(cmds.cCommand_Undo)
            self.check('undo with wrap on', self.TE.get_text_all(), base)
            self.TE.set_prop(cudatext.PROP_WRAP, 0)
            self.TE.cmd(cmds.cCommand_Redo)
            self.check('redo with wrap off', self.TE.get_text_all(), m_join(exp))
        finally:
            try:
                self.TE.set_prop(cudatext.PROP_WRAP, self.wrap)
            except Exception:
                pass

    def test_T30(self):
        """Multi-caret Enter with 3 carets: three line breaks at once;
        undo restores the text and leaves at least one caret."""
        L = make_small_lines()
        self.TE.set_text_all(m_join(L))
        self.TE.set_caret(0, 0)
        base = m_join(L)
        pts = [(4, 10), (4, 30), (4, 50)]
        # first caret replaces all existing ones (CARET_SET_ONE is the
        # default), each next caret is added with id=CARET_ADD
        self.TE.set_caret(pts[0][0], pts[0][1])
        for x, y in pts[1:]:
            self.TE.set_caret(x, y, -1, -1, cudatext.CARET_ADD)
        # API doc: carets changed via API don't repaint automatically
        self.TE.action(cudatext.EDACTION_UPDATE)
        carets_before = self.TE.get_carets()
        self.check('3 carets set via set_caret(CARET_ADD)',
                   len(carets_before), len(pts))
        self.info('carets set', carets_before)
        EV['change'] = EV['caret'] = 0
        self.TE.cmd(cmds.cCommand_KeyEnter)
        ec, ek = EV['change'], EV['caret']
        exp = list(L)
        for (x, y) in sorted(pts, key=lambda p: p[1], reverse=True):
            exp = m_insert(exp, x, y, '\n')
        self.check('text after multi-caret enter', self.TE.get_text_all(), m_join(exp))
        self.check('line_count after op', self.TE.get_line_count(), len(exp))
        # exactly ONE edit was made: undo it with exactly one step and
        # check the exact base state - never a blind drain (set_text_all
        # keeps one more undo entry below, see UNDO/REDO MODEL at top)
        self.TE.cmd(cmds.cCommand_Undo)
        self.check('text after 1 undo', self.TE.get_text_all(), base)
        self.check('line_count after undo', self.TE.get_line_count(), len(L))
        self.check('at least one caret after undo',
                   len(self.TE.get_carets()) >= 1, True)
        # redo the single command step
        self.TE.cmd(cmds.cCommand_Redo)
        self.check('text after 1 redo', self.TE.get_text_all(), m_join(exp))
        self.info('carets after undo (not asserted)', self.TE.get_carets())
        self.info('events during op (change/caret)', '%d / %d' % (ec, ek))

    def test_T31(self):
        """Multi-caret Backspace with 3 carets at line starts: three
        line-joins at once; undo restores the text."""
        L = make_small_lines()
        self.TE.set_text_all(m_join(L))
        self.TE.set_caret(0, 0)
        base = m_join(L)
        pts = [(0, 10), (0, 30), (0, 50)]
        # first caret replaces all existing ones (CARET_SET_ONE is the
        # default), each next caret is added with id=CARET_ADD
        self.TE.set_caret(pts[0][0], pts[0][1])
        for x, y in pts[1:]:
            self.TE.set_caret(x, y, -1, -1, cudatext.CARET_ADD)
        # API doc: carets changed via API don't repaint automatically
        self.TE.action(cudatext.EDACTION_UPDATE)
        carets_before = self.TE.get_carets()
        self.check('3 carets set via set_caret(CARET_ADD)',
                   len(carets_before), len(pts))
        self.info('carets set', carets_before)
        EV['change'] = EV['caret'] = 0
        self.TE.cmd(cmds.cCommand_KeyBackspace)
        ec, ek = EV['change'], EV['caret']
        exp = list(L)
        for (x, y) in sorted(pts, key=lambda p: p[1], reverse=True):
            exp = m_delete(exp, len(exp[y - 1]), y - 1, 0, y)
        self.check('text after multi-caret bs', self.TE.get_text_all(), m_join(exp))
        self.check('line_count after op', self.TE.get_line_count(), len(exp))
        # exactly ONE edit was made: undo it with exactly one step and
        # check the exact base state - never a blind drain (set_text_all
        # keeps one more undo entry below, see UNDO/REDO MODEL at top)
        self.TE.cmd(cmds.cCommand_Undo)
        self.check('text after 1 undo', self.TE.get_text_all(), base)
        self.check('line_count after undo', self.TE.get_line_count(), len(L))
        self.check('at least one caret after undo',
                   len(self.TE.get_carets()) >= 1, True)
        # redo the single command step
        self.TE.cmd(cmds.cCommand_Redo)
        self.check('text after 1 redo', self.TE.get_text_all(), m_join(exp))
        self.info('carets after undo (not asserted)', self.TE.get_carets())
        self.info('events during op (change/caret)', '%d / %d' % (ec, ek))

    def test_T32(self):
        """100k-char line: 60k-char selection delete (wrap stress); the
        whole line content must survive undo/redo exactly."""
        L = make_small_lines()
        big = 'L' * 100000
        L2 = L[:5] + [big] + L[6:]
        self.TE.set_text_all(m_join(L2))
        self.TE.set_caret(0, 0)
        y = 5
        x1, x2 = 20000, 80000
        exp = m_delete(L2, x1, y, x2, y)
        self.TE.set_caret(x1, y, x2, y)
        self.info('op', 'TextDeleteSelection of %d chars on a %d-char line' % (
            x2 - x1, len(big)))
        EV['change'] = EV['caret'] = 0
        self.TE.cmd(cmds.cCommand_TextDeleteSelection)
        ec, ek = EV['change'], EV['caret']
        self.check('text after 60k-char delete', self.TE.get_text_all(), m_join(exp))
        # exactly 1 delete command: 1 undo, 1 redo
        self.TE.cmd(cmds.cCommand_Undo)
        self.check('text after 1 undo', self.TE.get_text_all(), m_join(L2))
        self.TE.cmd(cmds.cCommand_Redo)
        self.check('text after 1 redo', self.TE.get_text_all(), m_join(exp))
        self.info('events during op (change/caret)', '%d / %d' % (ec, ek))

    def test_T33(self):
        """Single ed.insert of a 501-line block: one undo step must remove
        the whole block."""
        L = make_small_lines()
        self.TE.set_text_all(m_join(L))
        self.TE.set_caret(0, 0)
        base = m_join(L)
        s = 'AB\n' * 500            # 500 newlines -> 501 lines
        x, y = 3, 4
        exp = m_insert(L, x, y, s)
        self.TE.set_caret(x, y)
        self.info('op', 'single ed.insert of %d chars with %d newlines' % (
            len(s), s.count('\n')))
        EV['change'] = EV['caret'] = 0
        self.TE.insert(x, y, s)
        ec, ek = EV['change'], EV['caret']
        self.check('text after 501-line insert', self.TE.get_text_all(), m_join(exp))
        self.check('line_count after op', self.TE.get_line_count(), len(exp))
        # exactly ONE edit was made: undo it with exactly one step and
        # check the exact base state - never a blind drain (set_text_all
        # keeps one more undo entry below, see UNDO/REDO MODEL at top)
        self.TE.cmd(cmds.cCommand_Undo)
        self.check('text after 1 undo', self.TE.get_text_all(), base)
        self.check('line_count after undo', self.TE.get_line_count(), len(L))
        # redo the single edit step
        self.TE.cmd(cmds.cCommand_Redo)
        self.check('text after 1 redo', self.TE.get_text_all(), m_join(exp))
        self.info('events during op (change/caret)', '%d / %d' % (ec, ek))

    def test_T34(self):
        """Undo/redo state walk: 10 single-line inserts, then undo step
        by step - every intermediate state must be one of the known
        snapshots (no corrupted intermediate states), all the way to
        base; then the mirror check for redo."""
        L = make_small_lines()
        self.TE.set_text_all(m_join(L))
        self.TE.set_caret(0, 0)
        base = m_join(L)
        snaps = [m_join(L)]
        cur = list(L)
        ys = [2, 5, 11, 13, 17, 21, 25, 27, 29, 30]
        for i, y in enumerate(ys):
            s = 's%d' % i
            self.TE.set_caret(2, y)
            self.TE.insert(2, y, s)
            cur = m_insert(cur, 2, y, s)
            snaps.append(m_join(cur))
        # undo walk: each step must land on a known state, going
        # backwards; bound = 10 ops + 1 - never drains into the
        # set_text_all entry kept below (see UNDO/REDO MODEL)
        idx = len(snaps) - 1
        steps = 0
        exhausted = False
        while steps < 11:
            self.TE.cmd(cmds.cCommand_Undo)
            steps += 1
            t = self.TE.get_text_all()
            if t == base:
                break
            j = None
            for k in range(idx, -1, -1):
                if N(snaps[k]) == N(t):
                    j = k
                    break
            if j is None:
                self.check('undo step %d lands on a known state' % steps,
                           _pv(t), '(no match among snapshots)')
                return
            if j == idx:
                exhausted = True
                break
            idx = j
        if exhausted:
            self.check('undo stack not exhausted before base',
                       'exhausted at snapshot %d' % idx, 'should reach base')
        else:
            self.check('undo walk: reached base via valid states',
                       self.TE.get_text_all(), base)
        # redo walk: mirror check, same bound (10 ops + 1)
        idx2 = 0
        steps2 = 0
        exhausted2 = False
        while steps2 < 11:
            self.TE.cmd(cmds.cCommand_Redo)
            steps2 += 1
            t = self.TE.get_text_all()
            if N(t) == N(snaps[-1]):
                break
            j = None
            for k in range(idx2, len(snaps)):
                if N(snaps[k]) == N(t):
                    j = k
                    break
            if j is None:
                self.check('redo step %d lands on a known state' % steps2,
                           _pv(t), '(no match among snapshots)')
                return
            if j == idx2:
                exhausted2 = True
                break
            idx2 = j
        if exhausted2:
            self.check('redo stack not exhausted before final state',
                       'exhausted at snapshot %d' % idx2,
                       'should reach final')
        else:
            self.check('redo walk: reached final via valid states',
                       self.TE.get_text_all(), snaps[-1])
        self.info('undo/redo steps', '%d / %d' % (steps, steps2))

    def test_T35(self):
        """Caret/selection moves must not change text nor consume undo:
        20 moves/sels over a one-char document; undo must then remove
        exactly the marker insert (proving the moves added no undo
        entries above it), and the next undo lands on the kept
        set_text_all entry (see UNDO/REDO MODEL at top).
        PROP_UNDO_GROUPED is forced False so the kept set_text_all('')
        entry and the insert stay as two distinct undo/redo steps;
        grouping is restored in finally."""
        try:
            self.TE.set_prop(cudatext.PROP_UNDO_GROUPED, False)
        except Exception:
            pass
        try:
            self.TE.set_text_all('')
            self.TE.set_caret(0, 0)
            self.TE.insert(0, 0, 'Q')
            self.check('text after marker insert', self.TE.get_text_all(), 'Q')
            rng = random.Random(SEED + 42)
            self.info('op', '20 caret moves / selections, then undo')
            for i in range(20):
                y = rng.randrange(1)          # the single line
                x = rng.randrange(2)          # 0..1
                if i % 3 == 2:
                    self.TE.set_caret(x, y, min(x + 1, 1), y)
                else:
                    self.TE.set_caret(x, y)
            self.check('text unchanged after 20 caret moves',
                       self.TE.get_text_all(), 'Q')
            # undo #1 must remove the insert itself: if any caret move had
            # created an undo entry, this step would land on 'Q' instead
            self.TE.cmd(cmds.cCommand_Undo)
            self.check('undo #1 removes the insert (moves added no entries)',
                       self.TE.get_text_all(), '')
            # undo #2: the kept set_text_all entry (empty document here too)
            self.TE.cmd(cmds.cCommand_Undo)
            self.check('undo #2 removes the kept set_text_all entry',
                       self.TE.get_text_all(), '')
            # redo both steps back to the marker state
            self.TE.cmd(cmds.cCommand_Redo)
            self.check('redo #1 re-applies the kept entry',
                       self.TE.get_text_all(), '')
            self.TE.cmd(cmds.cCommand_Redo)
            self.check('redo #2 re-applies the insert',
                       self.TE.get_text_all(), 'Q')
            self.info('undo steps: exactly 2 (insert + kept set_text_all '
                      'entry); caret moves must add none')
        finally:
            try:
                self.TE.set_prop(cudatext.PROP_UNDO_GROUPED, True)
            except Exception:
                pass


    # ========================================================================
    # STANDALONE PERF TESTS P1..P5
    # Same rule as the core tests: the full scenario - doc build, timed
    # operations, checks, and the threshold/event judging - is inline in
    # each test function. Each perf test makes its own test records
    # (usually one per word-wrap mode) and manages its own wrap setting.
    # ========================================================================

    def test_P1(self, nlines=50000, ndel=30000):
        """Perf: delete the first ndel lines of a nlines-line doc (select
        + TextDeleteSelection), undo, redo - with word wrap off and on.
        Times and per-op event counts are judged against thresholds."""
        for w in (0, 1):
            self.wrap = w
            self.TE.set_prop(cudatext.PROP_WRAP, w)
            self.begin('P1', 'P1: delete first %d of %d lines, undo, redo '
                              '(wrap=%s)' % (ndel, nlines,
                                             'on' if w else 'off'))
            try:
                L = big_lines(nlines)
                t0 = time.time()
                self.TE.set_text_all(m_join(L))
                self.TE.set_caret(0, 0)
                t_load = time.time() - t0
                base_join = m_join(L)
                # (0,0)-(0,ndel) = the first ndel lines incl. their
                # newlines, leaving exactly L[ndel:] (the user's benchmark)
                x2, y2 = 0, ndel
                exp_lines = L[ndel:]
                self.TE.set_caret(0, 0, x2, y2)
                pre = self.TE.get_carets()
                self.info('doc', '%d lines, %d chars; set_text_all took %.2fs' % (
                    nlines, len(base_join), t_load))
                self.info('op', 'TextDeleteSelection of the first %d lines' % ndel)
                EV['change'] = EV['caret'] = 0
                t0 = time.time()
                self.TE.cmd(cmds.cCommand_TextDeleteSelection)
                t_del = time.time() - t0
                ec, ek = EV['change'], EV['caret']
                self.check('text after delete', self.TE.get_text_all(),
                           m_join(exp_lines))
                self.check('line_count after delete', self.TE.get_line_count(),
                           len(exp_lines))
                # exactly ONE delete command was made: undo once, redo
                # once - never drain blindly (set_text_all keeps one
                # more entry below, see UNDO/REDO MODEL at top)
                t0 = time.time()
                self.TE.cmd(cmds.cCommand_Undo)
                t_undo = time.time() - t0
                self.check('text after 1 undo (the single delete)',
                           self.TE.get_text_all(), base_join)
                self.check('line_count after undo', self.TE.get_line_count(),
                           nlines)
                self.check('selection/carets restored after undo',
                           self.TE.get_carets(), pre)
                t0 = time.time()
                self.TE.cmd(cmds.cCommand_Redo)
                t_redo = time.time() - t0
                self.check('text after 1 redo', self.TE.get_text_all(),
                           m_join(exp_lines))
                self.info('times: load %.2fs | delete %.3fs | undo %.3fs | '
                          'redo %.3fs' % (t_load, t_del, t_undo, t_redo))
                self.info('events during delete (change/caret)',
                          '%d / %d' % (ec, ek))
                # ---- perf judging (inline) ----
                th = self.TH.get(nlines, TH_DEFAULT)
                perf_fails = []
                perf_warns = []
                for label, tv, w_, f_ in (
                        ('delete', t_del, th[0], th[1]),
                        ('undo', t_undo, th[2], th[3]),
                        ('redo', t_redo, th[4], th[5])):
                    if tv > f_:
                        perf_fails.append('%s %.2fs exceeds FAIL threshold %.1fs'
                                          % (label, tv, f_))
                    elif tv > w_:
                        perf_warns.append('%s %.2fs exceeds warn threshold %.1fs'
                                          % (label, tv, w_))
                if ec == 0:
                    perf_warns.append('no on_change fired during the op '
                                      '(events deferred to idle or suppressed?)')
                elif ec > 200:
                    perf_warns.append('mass on_change events still firing '
                                      'during one op: %d' % ec)
                text_bad = self.cur['bad'] > 0
                status = ('FAIL' if (perf_fails or text_bad)
                          else ('WARN' if perf_warns else 'PASS'))
                self.perf.append({
                    'id': 'P1', 'wrap': self.wrap, 'lines': nlines,
                    'del': t_del, 'undo': t_undo, 'redo': t_redo,
                    'status': status, 'note': '; '.join(perf_fails + perf_warns),
                })
                if perf_fails:
                    self.cur['bad'] += 1
                    if self.cur['status'] != 'ERR':
                        self.cur['status'] = 'FAIL'
                    self.out('    FAIL  perf: %s' % '; '.join(perf_fails))
                elif perf_warns:
                    if self.cur['note']:
                        self.cur['note'] += '; '
                    self.cur['note'] = (self.cur['note'] + '; '.join(perf_warns))[:200]
                    self.out('    WARN  perf: %s' % '; '.join(perf_warns))
                else:
                    self.cur['ok'] += 1
                    self.out('    ok    perf thresholds')
            except Exception:
                self.cur['status'] = 'ERR'
                tb = traceback.format_exc()
                self.cur['note'] = tb.strip().splitlines()[-1][:200]
                self.out('    ERR   exception raised:')
                for ln in tb.strip().splitlines()[-5:]:
                    self.out('            ' + ln)
            self.done()

    def test_P2(self, nlines=300000, ndel=200000):
        """Perf: the user's exact scenario - delete the first 200k lines
        of a 300k-line doc, undo, redo - with word wrap off and on."""
        for w in (0, 1):
            self.wrap = w
            self.TE.set_prop(cudatext.PROP_WRAP, w)
            self.begin('P2', 'P2: delete first %d of %d lines, undo, redo '
                              '(wrap=%s)' % (ndel, nlines,
                                             'on' if w else 'off'))
            try:
                L = big_lines(nlines)
                t0 = time.time()
                self.TE.set_text_all(m_join(L))
                self.TE.set_caret(0, 0)
                t_load = time.time() - t0
                base_join = m_join(L)
                # (0,0)-(0,ndel) = the first ndel lines incl. their
                # newlines, leaving exactly L[ndel:] (the user's benchmark)
                x2, y2 = 0, ndel
                exp_lines = L[ndel:]
                self.TE.set_caret(0, 0, x2, y2)
                pre = self.TE.get_carets()
                self.info('doc', '%d lines, %d chars; set_text_all took %.2fs' % (
                    nlines, len(base_join), t_load))
                self.info('op', 'TextDeleteSelection of the first %d lines' % ndel)
                EV['change'] = EV['caret'] = 0
                t0 = time.time()
                self.TE.cmd(cmds.cCommand_TextDeleteSelection)
                t_del = time.time() - t0
                ec, ek = EV['change'], EV['caret']
                self.check('text after delete', self.TE.get_text_all(),
                           m_join(exp_lines))
                self.check('line_count after delete', self.TE.get_line_count(),
                           len(exp_lines))
                # exactly ONE delete command was made: undo once, redo
                # once - never drain blindly (set_text_all keeps one
                # more entry below, see UNDO/REDO MODEL at top)
                t0 = time.time()
                self.TE.cmd(cmds.cCommand_Undo)
                t_undo = time.time() - t0
                self.check('text after 1 undo (the single delete)',
                           self.TE.get_text_all(), base_join)
                self.check('line_count after undo', self.TE.get_line_count(),
                           nlines)
                self.check('selection/carets restored after undo',
                           self.TE.get_carets(), pre)
                t0 = time.time()
                self.TE.cmd(cmds.cCommand_Redo)
                t_redo = time.time() - t0
                self.check('text after 1 redo', self.TE.get_text_all(),
                           m_join(exp_lines))
                self.info('times: load %.2fs | delete %.3fs | undo %.3fs | '
                          'redo %.3fs' % (t_load, t_del, t_undo, t_redo))
                self.info('events during delete (change/caret)',
                          '%d / %d' % (ec, ek))
                # ---- perf judging (inline) ----
                th = self.TH.get(nlines, TH_DEFAULT)
                perf_fails = []
                perf_warns = []
                for label, tv, w_, f_ in (
                        ('delete', t_del, th[0], th[1]),
                        ('undo', t_undo, th[2], th[3]),
                        ('redo', t_redo, th[4], th[5])):
                    if tv > f_:
                        perf_fails.append('%s %.2fs exceeds FAIL threshold %.1fs'
                                          % (label, tv, f_))
                    elif tv > w_:
                        perf_warns.append('%s %.2fs exceeds warn threshold %.1fs'
                                          % (label, tv, w_))
                if ec == 0:
                    perf_warns.append('no on_change fired during the op '
                                      '(events deferred to idle or suppressed?)')
                elif ec > 200:
                    perf_warns.append('mass on_change events still firing '
                                      'during one op: %d' % ec)
                text_bad = self.cur['bad'] > 0
                status = ('FAIL' if (perf_fails or text_bad)
                          else ('WARN' if perf_warns else 'PASS'))
                self.perf.append({
                    'id': 'P2', 'wrap': self.wrap, 'lines': nlines,
                    'del': t_del, 'undo': t_undo, 'redo': t_redo,
                    'status': status, 'note': '; '.join(perf_fails + perf_warns),
                })
                if perf_fails:
                    self.cur['bad'] += 1
                    if self.cur['status'] != 'ERR':
                        self.cur['status'] = 'FAIL'
                    self.out('    FAIL  perf: %s' % '; '.join(perf_fails))
                elif perf_warns:
                    if self.cur['note']:
                        self.cur['note'] += '; '
                    self.cur['note'] = (self.cur['note'] + '; '.join(perf_warns))[:200]
                    self.out('    WARN  perf: %s' % '; '.join(perf_warns))
                else:
                    self.cur['ok'] += 1
                    self.out('    ok    perf thresholds')
            except Exception:
                self.cur['status'] = 'ERR'
                tb = traceback.format_exc()
                self.cur['note'] = tb.strip().splitlines()[-1][:200]
                self.out('    ERR   exception raised:')
                for ln in tb.strip().splitlines()[-5:]:
                    self.out('            ' + ln)
            self.done()

    def test_P3(self, nlines=300000, ndel=300000):
        """Perf: select ALL of a 300k-line doc and delete it, then undo
        and redo - wrap on only (the heaviest wrap case)."""
        w = 1
        self.wrap = w
        self.TE.set_prop(cudatext.PROP_WRAP, w)
        self.begin('P3', 'P3: delete ALL of %d lines, undo, redo (wrap=on)'
                          % nlines)
        try:
            L = big_lines(nlines)
            t0 = time.time()
            self.TE.set_text_all(m_join(L))
            self.TE.set_caret(0, 0)
            t_load = time.time() - t0
            base_join = m_join(L)
            # select the entire document
            x2, y2 = len(L[-1]), nlines - 1
            exp_lines = ['']
            self.TE.set_caret(0, 0, x2, y2)
            pre = self.TE.get_carets()
            self.info('doc', '%d lines, %d chars; set_text_all took %.2fs' % (
                nlines, len(base_join), t_load))
            self.info('op', 'TextDeleteSelection of the entire document')
            EV['change'] = EV['caret'] = 0
            t0 = time.time()
            self.TE.cmd(cmds.cCommand_TextDeleteSelection)
            t_del = time.time() - t0
            ec, ek = EV['change'], EV['caret']
            self.check('text after delete', self.TE.get_text_all(),
                       m_join(exp_lines))
            self.check('line_count after delete', self.TE.get_line_count(),
                       len(exp_lines))
            # exactly ONE delete command was made: undo once, redo once
            # - never drain blindly (set_text_all keeps one more entry
            # below, see UNDO/REDO MODEL at top)
            t0 = time.time()
            self.TE.cmd(cmds.cCommand_Undo)
            t_undo = time.time() - t0
            self.check('text after 1 undo (the single delete)',
                       self.TE.get_text_all(), base_join)
            self.check('line_count after undo', self.TE.get_line_count(),
                       nlines)
            self.check('selection/carets restored after undo',
                       self.TE.get_carets(), pre)
            t0 = time.time()
            self.TE.cmd(cmds.cCommand_Redo)
            t_redo = time.time() - t0
            self.check('text after 1 redo', self.TE.get_text_all(),
                       m_join(exp_lines))
            self.info('times: load %.2fs | delete %.3fs | undo %.3fs | '
                      'redo %.3fs' % (t_load, t_del, t_undo, t_redo))
            self.info('events during delete (change/caret)',
                      '%d / %d' % (ec, ek))
            # ---- perf judging (inline) ----
            th = self.TH.get(nlines, TH_DEFAULT)
            perf_fails = []
            perf_warns = []
            for label, tv, w_, f_ in (
                    ('delete', t_del, th[0], th[1]),
                    ('undo', t_undo, th[2], th[3]),
                    ('redo', t_redo, th[4], th[5])):
                if tv > f_:
                    perf_fails.append('%s %.2fs exceeds FAIL threshold %.1fs'
                                      % (label, tv, f_))
                elif tv > w_:
                    perf_warns.append('%s %.2fs exceeds warn threshold %.1fs'
                                      % (label, tv, w_))
            if ec == 0:
                perf_warns.append('no on_change fired during the op '
                                  '(events deferred to idle or suppressed?)')
            elif ec > 200:
                perf_warns.append('mass on_change events still firing '
                                  'during one op: %d' % ec)
            text_bad = self.cur['bad'] > 0
            status = ('FAIL' if (perf_fails or text_bad)
                      else ('WARN' if perf_warns else 'PASS'))
            self.perf.append({
                'id': 'P3', 'wrap': self.wrap, 'lines': nlines,
                'del': t_del, 'undo': t_undo, 'redo': t_redo,
                'status': status, 'note': '; '.join(perf_fails + perf_warns),
            })
            if perf_fails:
                self.cur['bad'] += 1
                if self.cur['status'] != 'ERR':
                    self.cur['status'] = 'FAIL'
                self.out('    FAIL  perf: %s' % '; '.join(perf_fails))
            elif perf_warns:
                if self.cur['note']:
                    self.cur['note'] += '; '
                self.cur['note'] = (self.cur['note'] + '; '.join(perf_warns))[:200]
                self.out('    WARN  perf: %s' % '; '.join(perf_warns))
            else:
                self.cur['ok'] += 1
                self.out('    ok    perf thresholds')
        except Exception:
            self.cur['status'] = 'ERR'
            tb = traceback.format_exc()
            self.cur['note'] = tb.strip().splitlines()[-1][:200]
            self.out('    ERR   exception raised:')
            for ln in tb.strip().splitlines()[-5:]:
                self.out('            ' + ln)
        self.done()

    def test_P4(self, nlines=300000, ndel=200000):
        """Perf: repeated delete/undo/redo cycles on a 300k-line doc
        (x1 with wrap off, x3 with wrap on): every cycle must return to
        the exact base state; per-cycle times are judged."""
        for w in (0, 1):
            self.wrap = w
            self.TE.set_prop(cudatext.PROP_WRAP, w)
            cycles = 1 if w == 0 else 3
            self.begin('P4', 'P4: delete/undo/redo cycles x%d, %d lines '
                              '(wrap=%s)' % (cycles, nlines,
                                             'on' if w else 'off'))
            try:
                L = big_lines(nlines)
                t0 = time.time()
                self.TE.set_text_all(m_join(L))
                self.TE.set_caret(0, 0)
                t_load = time.time() - t0
                base_join = m_join(L)
                exp_join = m_join(L[ndel:])
                self.info('doc', '%d lines; set_text_all took %.2fs' % (
                    nlines, t_load))
                t_del = t_undo = t_redo = 0.0
                ev = {'d': 0, 'u': 0, 'r': 0}
                for c in range(cycles):
                    # selection does not survive the previous cycle's
                    # undo/redo and the exp state has too few lines for
                    # it, so it must be re-set (each cycle starts and
                    # ends at the base state)
                    self.TE.set_caret(0, 0, 0, ndel)
                    EV['change'] = EV['caret'] = 0
                    t0 = time.time()
                    self.TE.cmd(cmds.cCommand_TextDeleteSelection)
                    t_del += time.time() - t0
                    ec_d, ek_d = EV['change'], EV['caret']
                    self.check('cycle %d: text after delete' % (c + 1),
                               self.TE.get_text_all(), exp_join)
                    EV['change'] = EV['caret'] = 0
                    t0 = time.time()
                    self.TE.cmd(cmds.cCommand_Undo)
                    t_undo += time.time() - t0
                    ec_u, ek_u = EV['change'], EV['caret']
                    self.check('cycle %d: text after undo' % (c + 1),
                               self.TE.get_text_all(), base_join)
                    EV['change'] = EV['caret'] = 0
                    t0 = time.time()
                    self.TE.cmd(cmds.cCommand_Redo)
                    t_redo += time.time() - t0
                    ec_r, ek_r = EV['change'], EV['caret']
                    self.check('cycle %d: text after redo' % (c + 1),
                               self.TE.get_text_all(), exp_join)
                    # second undo returns to base so the next cycle can
                    # re-select
                    t0 = time.time()
                    self.TE.cmd(cmds.cCommand_Undo)
                    t_undo += time.time() - t0
                    self.check('cycle %d: back to base' % (c + 1),
                               self.TE.get_text_all(), base_join)
                    ev['d'] += ec_d
                    ev['u'] += ec_u
                    ev['r'] += ec_r
                    self.info('cycle %d events (change/caret): delete %d/%d, '
                              'undo %d/%d, redo %d/%d' % (
                                  c + 1, ec_d, ek_d, ec_u, ek_u, ec_r, ek_r))
                self.check('final state after cycles == base',
                           self.TE.get_text_all(), base_join)
                n = max(1, cycles)
                self.info('times: delete %.2fs | undo %.2fs (2 undos per '
                          'cycle) | redo %.2fs (%d cycles; per-cycle values '
                          'judged)' % (t_del, t_undo, t_redo, cycles))
                # ---- perf judging (inline, per-cycle averages) ----
                th = self.TH.get(nlines, TH_DEFAULT)
                perf_fails = []
                perf_warns = []
                for label, tv, w_, f_ in (
                        ('delete', t_del / n, th[0], th[1]),
                        ('undo', t_undo / n, th[2], th[3]),
                        ('redo', t_redo / n, th[4], th[5])):
                    if tv > f_:
                        perf_fails.append('%s %.2fs exceeds FAIL threshold %.1fs'
                                          % (label, tv, f_))
                    elif tv > w_:
                        perf_warns.append('%s %.2fs exceeds warn threshold %.1fs'
                                          % (label, tv, w_))
                ec = ev['d'] / n
                if ec == 0:
                    perf_warns.append('no on_change fired during the op '
                                      '(events deferred to idle or suppressed?)')
                elif ec > 200:
                    perf_warns.append('mass on_change events still firing '
                                      'during one op: %d' % ec)
                text_bad = self.cur['bad'] > 0
                status = ('FAIL' if (perf_fails or text_bad)
                          else ('WARN' if perf_warns else 'PASS'))
                self.perf.append({
                    'id': 'P4', 'wrap': self.wrap, 'lines': nlines,
                    'del': t_del / n, 'undo': t_undo / n, 'redo': t_redo / n,
                    'status': status, 'note': '; '.join(perf_fails + perf_warns),
                })
                if perf_fails:
                    self.cur['bad'] += 1
                    if self.cur['status'] != 'ERR':
                        self.cur['status'] = 'FAIL'
                    self.out('    FAIL  perf: %s' % '; '.join(perf_fails))
                elif perf_warns:
                    if self.cur['note']:
                        self.cur['note'] += '; '
                    self.cur['note'] = (self.cur['note'] + '; '.join(perf_warns))[:200]
                    self.out('    WARN  perf: %s' % '; '.join(perf_warns))
                else:
                    self.cur['ok'] += 1
                    self.out('    ok    perf thresholds')
            except Exception:
                self.cur['status'] = 'ERR'
                tb = traceback.format_exc()
                self.cur['note'] = tb.strip().splitlines()[-1][:200]
                self.out('    ERR   exception raised:')
                for ln in tb.strip().splitlines()[-5:]:
                    self.out('            ' + ln)
            self.done()

    def test_P5(self, nlines=300000, ndel=200000):
        """Perf: big delete, undo, then a small 4-char edit on the
        restored 300k-line doc (wrap on) - undo of the small edit must
        stay quick: catches undo-stack damage after mass operations."""
        w = 1
        self.wrap = w
        self.TE.set_prop(cudatext.PROP_WRAP, w)
        self.begin('P5', 'P5: big delete, undo, small edit, undo, redo '
                         '(wrap=on)')
        try:
            L = big_lines(nlines)
            t0 = time.time()
            self.TE.set_text_all(m_join(L))
            self.TE.set_caret(0, 0)
            t_load = time.time() - t0
            base_join = m_join(L)
            exp_join = m_join(L[ndel:])
            self.TE.set_caret(0, 0, 0, ndel)
            self.info('doc', '%d lines; set_text_all took %.2fs' % (
                nlines, t_load))
            EV['change'] = EV['caret'] = 0
            self.TE.cmd(cmds.cCommand_TextDeleteSelection)
            ec, ek = EV['change'], EV['caret']
            self.check('text after delete', self.TE.get_text_all(), exp_join)
            EV['change'] = EV['caret'] = 0
            self.TE.cmd(cmds.cCommand_Undo)
            ec2, ek2 = EV['change'], EV['caret']
            self.check('text after big undo', self.TE.get_text_all(), base_join)
            # small edit on the restored 300k doc
            x, y = 10, 5
            exp2 = m_insert(L, x, y, 'MARK')
            self.TE.set_caret(x, y)
            EV['change'] = EV['caret'] = 0
            self.TE.insert(x, y, 'MARK')
            ec3, ek3 = EV['change'], EV['caret']
            self.check('text after small insert on restored doc',
                       self.TE.get_text_all(), m_join(exp2))
            # undo of just that insert must be quick and exact:
            # exactly 1 edit since the big undo - 1 undo step, 1 redo
            t0 = time.time()
            self.TE.cmd(cmds.cCommand_Undo)
            t_u2 = time.time() - t0
            self.check('text after undo of small insert == base',
                       self.TE.get_text_all(), base_join)
            self.TE.cmd(cmds.cCommand_Redo)
            self.check('text after redo == base+MARK', self.TE.get_text_all(),
                       m_join(exp2))
            self.info('times: load %.2fs; undo of 4-char insert on the 300k '
                      'doc: %.3fs' % (t_load, t_u2))
            self.info('events (change): delete %d, undo %d, insert %d'
                      % (ec, ec2, ec3))
            # ---- perf judging (inline; delete/redo not timed here) ----
            th = self.TH.get(nlines, TH_DEFAULT)
            perf_fails = []
            perf_warns = []
            for label, tv, w_, f_ in (
                    ('delete', 0.0, th[0], th[1]),
                    ('undo', t_u2, th[2], th[3]),
                    ('redo', 0.0, th[4], th[5])):
                if tv > f_:
                    perf_fails.append('%s %.2fs exceeds FAIL threshold %.1fs'
                                      % (label, tv, f_))
                elif tv > w_:
                    perf_warns.append('%s %.2fs exceeds warn threshold %.1fs'
                                      % (label, tv, w_))
            if ec == 0:
                perf_warns.append('no on_change fired during the op '
                                  '(events deferred to idle or suppressed?)')
            elif ec > 200:
                perf_warns.append('mass on_change events still firing '
                                  'during one op: %d' % ec)
            text_bad = self.cur['bad'] > 0
            status = ('FAIL' if (perf_fails or text_bad)
                      else ('WARN' if perf_warns else 'PASS'))
            self.perf.append({
                'id': 'P5', 'wrap': self.wrap, 'lines': nlines,
                'del': 0.0, 'undo': t_u2, 'redo': 0.0,
                'status': status, 'note': '; '.join(perf_fails + perf_warns),
            })
            if perf_fails:
                self.cur['bad'] += 1
                if self.cur['status'] != 'ERR':
                    self.cur['status'] = 'FAIL'
                self.out('    FAIL  perf: %s' % '; '.join(perf_fails))
            elif perf_warns:
                if self.cur['note']:
                    self.cur['note'] += '; '
                self.cur['note'] = (self.cur['note'] + '; '.join(perf_warns))[:200]
                self.out('    WARN  perf: %s' % '; '.join(perf_warns))
            else:
                self.cur['ok'] += 1
                self.out('    ok    perf thresholds')
        except Exception:
            self.cur['status'] = 'ERR'
            tb = traceback.format_exc()
            self.cur['note'] = tb.strip().splitlines()[-1][:200]
            self.out('    ERR   exception raised:')
            for ln in tb.strip().splitlines()[-5:]:
                self.out('            ' + ln)
        self.done()


# ----------------------------------------------------------------------------
# test registry: every test standalone, referenced directly
# ----------------------------------------------------------------------------

TESTS = [
    # core tests (each runs with word wrap off and on)
    ('T01', 'insert single char via ed.insert', Runner.test_T01),
    ('T02', 'insert multi-line text', Runner.test_T02),
    ('T03', 'insert at doc start', Runner.test_T03),
    ('T04', 'insert at doc end', Runner.test_T04),
    ('T05', 'insert into empty document', Runner.test_T05),
    ('T06', 'typing simulation: 12 adjacent chars', Runner.test_T06),
    ('T07', 'typing via TextInsert, no selection', Runner.test_T07),
    ('T08', 'typing via TextInsert over a selection', Runner.test_T08),
    ('T09', 'Backspace mid-line', Runner.test_T09),
    ('T10', 'Backspace at line start (join lines)', Runner.test_T10),
    ('T11', 'Delete key mid-line', Runner.test_T11),
    ('T12', 'Delete key at line end (join lines)', Runner.test_T12),
    ('T13', 'delete selection, forward', Runner.test_T13),
    ('T14', 'delete selection, backward', Runner.test_T14),
    ('T15', 'delete multi-line selection', Runner.test_T15),
    ('T16', 'delete selection to EOF', Runner.test_T16),
    ('T17', 'select all + delete', Runner.test_T17),
    ('T18', 'ed.delete crossing a newline', Runner.test_T18),
    ('T19', '30 sequential line deletions', Runner.test_T19),
    ('T20', 'random storm: 150 mixed ops', Runner.test_T20),
    ('T21', 'unicode insert + delete roundtrip', Runner.test_T21),
    ('T22', 'tab char + EOL fidelity (snapshot)', Runner.test_T22),
    ('T23', 'set_text_all keeps one undo entry (exact trace)', Runner.test_T23),
    ('T24', 'redo idempotence (empty redo stack)', Runner.test_T24),
    ('T25', 'redo invalidation by new edit', Runner.test_T25),
    ('T26', 'modified flag + save marker', Runner.test_T26),
    ('T27', 'undo/redo with live selection', Runner.test_T27),
    ('T28', 'tab switch away and back', Runner.test_T28),
    ('T29', 'wrap toggled between undo/redo', Runner.test_T29),
    ('T30', 'multi-caret Enter', Runner.test_T30),
    ('T31', 'multi-caret Backspace', Runner.test_T31),
    ('T32', '100k-char line: 60k-char delete', Runner.test_T32),
    ('T33', 'single insert of 501 lines', Runner.test_T33),
    ('T34', 'undo/redo state walk (10 ops)', Runner.test_T34),
    ('T35', 'caret/selection moves keep text stable', Runner.test_T35),
    # perf tests (manage their own wrap modes; P1 also in quick mode)
    ('P1', 'perf: delete first 30k of 50k lines, undo, redo (wrap off+on)',
     Runner.test_P1),
    ('P2', 'perf: delete first 200k of 300k lines, undo, redo (wrap off+on)',
     Runner.test_P2),
    ('P3', 'perf: select all + delete of 300k lines, undo, redo (wrap on)',
     Runner.test_P3),
    ('P4', 'perf: delete/undo/redo cycles x1/x3, 300k lines',
     Runner.test_P4),
    ('P5', 'perf: big delete, undo, small edit, undo, redo '
           '(300k lines, wrap on)', Runner.test_P5),
]

# ----------------------------------------------------------------------------
# plugin entry points
# ----------------------------------------------------------------------------

class Command:

    # ---- event handlers (subscribed via install.inf [events]:
    #      "events=on_change,on_caret"); CudaText calls event handlers
    #      only as methods of class Command -------------------------------

    def on_change(self, ed_self):
        """Called after editor text is changed; counts mass events."""
        try:
            EV['change'] += 1
            EV['change_total'] += 1
        except Exception:
            pass

    def on_caret(self, ed_self):
        """Called after caret position/selection is changed; counts events."""
        try:
            EV['caret'] += 1
            EV['caret_total'] += 1
        except Exception:
            pass

    # ---- menu commands ----------------------------------------------------

    def run_full(self):
        """Run the full suite, including 300k-line perf tests."""
        Runner(full=True).run()

    def run_quick(self):
        """Run the quick suite (50k-line perf test only)."""
        Runner(full=False).run()

    def run_single(self):
        """Show the list of all tests (core T01..T35, perf P1..P5) and run
        only the one chosen in the dialog. dlg_menu(DMENU_LIST_ALT, ...)
        is the documented list-with-filter dialog; the ALT flavor shows
        each item with double height, the description below the id (good
        for these long labels). It returns the 0-based index of the
        chosen item, or None when cancelled."""
        r = Runner(full=True)
        cat = r.test_catalog()
        # 'id\tdescription': the part after the tab shows below the id
        items = ['%s\t%s' % (tid, label) for tid, label in cat]
        res = cudatext.dlg_menu(cudatext.DMENU_LIST_ALT, items, focused=0,
                                caption='Undo/Redo tests: select a test '
                                        'to run it alone')
        if res is None:
            return
        r.run_single(cat[res][0])

    def about(self):
        print(__doc__)
        try:
            sys.stdout.flush()
        except Exception:
            pass
