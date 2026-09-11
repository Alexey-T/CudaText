"""
cuda_testing_perf - PERFORMANCE regression test suite for CudaText.

QUICK GUIDE:
  In the results table, Compare each row’s Total to base.
  Total should be close to base (the reference measurement with wrap on).
_____________________________________________________________________

PURPOSE
  Catch performance REGRESSIONS of the undo/redo machinery,
  replace_lines, set_text_all and of file_open / wrap on big
  documents: every timed command is judged against its own warn/fail
  thresholds, so a slowdown trips a WARN/FAIL in the summary, not
  just a number in a table.

DESIGN: every test is standalone
  Each test is a single self-contained function: corpus setup, the timed commands, the
  Hang1/Hang2 measurement, every check and the threshold judging are
  ALL inside the test function itself. Nothing test-specific is
  shared or factored out, so a test can be read and debugged top to
  bottom without following call chains. Only non-test infrastructure
  is shared: the corpus writers, the check/report channel, the hang
  measurement and the tab lifecycle.

HANG TIME MEASUREMENT (2026-09-08, v2 - the benchmark's method)
  The manual benchmarks show the API time is only PART of the cost:
  after e.g. replace_lines returns in ~3s, the app is still "hung"
  ~8s more, and the forced repaint costs ~8s again. The suite
  measures it EXACTLY like the manual benchmark, right after each
  timed op - no polling, no CPU sampling, just two timed calls:
    Hang1:  t1 = time.time()
            app_proc(PROC_IDLE, True)
            t2 = time.time()
    Hang2:  t1 = time.time()
            ed.action(EDACTION_UPDATE, 1)
            t2 = time.time()
  * Hang1 (app_proc(PROC_IDLE, True)) is Application.ProcessMessages +
    Application.Idle (verified in app/formmain_py_api.inc): one
    message-loop pump + idle pass. The deferred work queued by the op
    runs inside this call, so its duration IS the first part of the
    hang.
  * Hang2 (ed.action(EDACTION_UPDATE, 1)) is Ed.Update(True, true):
    forces the WrapInfo update + repaint - the deferred word-wrap
    recalculation (API wiki: param1="1" updates WrapInfo). Its
    duration is the second part of the hang.
  Every timed op (replace_lines, set_text_all, TextDeleteSelection,
  Undo, Redo, file_open) is followed by exactly these
  two calls; the console shows "op X (+Hang1 Y +Hang2 Z)", and the
  summary table's "hang" column is the sum of all Hang1+Hang2 of
  that perf record. Judged against each test's local hang thresholds (warn/fail), so a
  regression like the removed wrap-items cache (commit3 vs commit4:
  +12s hang on set_text_all with wrap on) trips a WARN/FAIL, not
  just a note.

UNDO GROUPING (PROP_UNDO_GROUPED)
  IMPORTANT - do NOT force PROP_UNDO_GROUPED=False for the whole
  suite. With grouping off, the full performance suite (300k-line
  docs) can consume >6 GB RAM. Grouping must stay ON
  (True) globally for the suite.

COMMANDS (menu: Testing / Testing of Performance)
  Run all tests (300k lines)     run_all_300k:    MP1..MP4 on the
    300k corpus, plus MP5
  Run all tests (1M lines)       run_all_1M:      MP1..MP4 on the
    1M corpus, plus MP5
  Run single test (300k lines)   run_single_300k: one test of the
    300k catalog, chosen in a dialog
  Run single test (1M lines)     run_single_1M:   one test of the
    1M catalog, chosen in a dialog
  Help                           about
  Each test opens its own temp tab (tag URTEST_LOAD) and closes it
  when done; your tabs are not modified. Do not touch the editor
  while the suite runs. When finished, a summary dialog is shown
  and a new tab opens with the full console log.

THE TWO CORPORA (2026-09-11)
  300k corpus: 300,000 random lines of 490..510 chars (~150 MB),
    file cuda_undo_test_rand_300K.txt; MP4 deletes the first
    200,000 lines (200k-of-300k).
  1M corpus: 1,000,000 random lines of 490..510 chars (~500 MB),
    file cuda_undo_test_rand_1M.txt; MP4 deletes the first
    600,000 lines (600k-of-1M).
  MP1..MP4 exist once per corpus: the TESTS_300K / TESTS_1M
  registries bind each test to its corpus's size, and the four
  commands above select the registry. MP5 (file_open) is the SAME
  test in both suites: it always opens the 1M-line corpus file,
  exactly like the manual benchmark. Console use stays possible:
  Runner().run('1M'), Runner().run_single('MP3', '1M'),
  Runner().test_MP1(1000000) etc.

WHAT IT COVERS
  - MP1..MP5: the 5 manual console benchmarks replicated
    EXACTLY, command for command, on both corpora. The document
    is the corpus FILE opened with file_open into its own tab -
    exactly the manual session's state; no set_text_all, no
    suite-tab commands anywhere in the setup. The timed part is
    ONLY the manual test's own commands with nothing run in
    between, Hang1/Hang2 are the manual test's two timed calls,
    and every check is read-only and runs after the timing. The
    corpus file is written once per run with the manual
    benchmark's exact seeded generator (SEED 20260904, one
    bytes(rng.getrandbits(8) for _ in range(rng.randint(245,
    255))).hex() line + newline per line), so its content is
    byte-identical to the benchmark's own corpus file. The
    replicas:
    MP1 replace_lines(0, get_line_count()-1, open().readlines()),
    MP2 set_text_all(open().read()), MP3 replace_lines load then
    set_caret(0, get_line_count(), 0, 0) + TextDeleteSelection +
    Undo + Redo,
    MP4 replace_lines load then set_caret(0, ndel, 0, 0) +
    TextDeleteSelection + Undo + Redo (200k-of-300k on the 300k
    corpus, 600k-of-1M on the 1M corpus),
    MP5 file_open (wrap off then wrap on via global opts)

THRESHOLDS (2026-09-11)
  Every timed quantity - each command's own time and its own
  Hang1+Hang2, judged separately - is compared to thresholds
  derived from the reference machine's MEASURED healthy baselines
  (the manual benchmark timings, both corpora):
      warn = 2 * baseline + 0.25 s
      fail = 3 * baseline + 1.0 s
  That is the "WARN above 2x, FAIL above 3x" rule plus a small
  absolute jitter slack: sub-second baselines (Hang2 ~0.01 s,
  small redo hangs ~0.1 s) cannot trip on timer noise, while
  every baseline above ~0.25 s is judged by the pure 2x / 3x
  rule. Set TH_SLACK_WARN / TH_SLACK_FAIL to 0 for the strict
  multiplier-only rule. The baselines live in each test, right
  next to the thresholds they produce; corpus sizes without a
  measurement scale linearly from the 300k value (base_for).
  All timed ops have measured Hang1+Hang2 baselines from the manual benchmark (2026-09-11) which was done on an Intel Core i7 CPU M 640 @ 2.80GHz.

OUTPUT
  All results go to the Console panel. Per test: check lines
  (ok / FAIL with got/expected / ERR), info lines (timings,
  Hang1/Hang2 per op) and profile lines (per timed command:
  cmd / Hang1 / Hang2 / Total). A SUMMARY is printed at the end:
  totals, list of failed tests, performance table (with a "hang"
  column), overall verdict.

HOW TO READ THE OUTPUT
  [MPx] test name (wrap=off/on)
    ok    <check>          check passed
    FAIL  <check>          mismatch; got/expected previews follow
    ERR   exception        the test crashed the API (bug or API change)
    info  ...              timings, Hang1/Hang2 per op
    profile  <name>: <cmd>s  Hang1: <h1>s  Hang2: <h2>s  Total: <sum>s
    WARN  perf: ...        a timing exceeded the warn threshold
  => PASS/FAIL/ERR/SKIP  (n ok, m failed)
  SUMMARY: totals, failed list, performance table (with hang column),
  overall verdict. A dialog repeats the short summary; a new tab holds
  the full log.

NOTES
  * Do not touch the editor while the suite runs. Any click or keypress
    you make during a perf test is processed by the PROC_IDLE pass and
    pollutes the hang numbers.
  * The 300k suite needs ~2 GB RAM; the 1M suite needs considerably
    more (every test builds a ~500 MB document) - the free-RAM gate
    asks before each test when it gets low. Runs can take several
    minutes (much longer if undo is still slow/not patched - that
    is the point). Each timed op additionally runs one
    app_proc(PROC_IDLE) pass and one ed.action(EDACTION_UPDATE)
    pass (the hang measurement): on a healthy editor both return
    in milliseconds, on a regressed one they take seconds - that
    IS the hang being measured.
  * MP tests write the corpus file(s) into the system temp dir
    under <tempdir>/cuda_testing_undo_redo (once per run, removed
    on cleanup): the 300k suite writes the ~150 MB 300k file
    (MP1..MP4) AND the ~500 MB 1M file (MP5); the 1M suite writes
    only the ~500 MB 1M file. Delete them by hand to reclaim
    space if a run was killed mid-way.
  * While the suite runs, user.json's "wrap_enabled_max_lines" is
    temporarily set to 1100000 (CudaText refuses to enable word wrap
    above that line count, and the suite wraps 300k/1M-line docs) and
    "wrap_mode" is temporarily set (MP5 toggles it off/on per row).
    Three helpers: Runner._enable_wrap_opts (high max + wrap on),
    Runner._disable_wrap_opts (high max + wrap off; both from MP5)
    and Runner._restore_wrap_opts (user's original values back; from
    _cleanup, also on FATAL/error paths). All writes go through
    cudax_lib's get_opt/set_opt, which only change the file on disk:
    right after each write the helpers open user.json, save it with
    the editor's save command and close it again, because the running
    CudaText re-reads its options only when user.json is saved in the
    editor (or on restart). If the process is killed mid-run, restore
    the values by hand; the old ones are printed to the console when
    enable first runs.
  * PROP_UNDO_GROUPED stays True (the CudaText default) for the suite.
    Forcing it False globally exhausts RAM on the 300k-line perf
    tests (>6 GB).  See UNDO GROUPING.
  * Test data is seeded (SEED 20260904): identical documents on
    every run.

"""

import os
import sys
import time
import random
import traceback
import tempfile
from functools import partial

import cudatext
import cudatext_cmd as cmds
import cudax_lib

SEED = 20260904

# temp dir where the MP corpus files are written
LOAD_DIR = os.path.join(tempfile.gettempdir(), 'cuda_testing_undo_redo')

# user.json options patched for the duration of a run: CudaText
# refuses to enable word wrap on documents longer than
# "wrap_enabled_max_lines" lines. The suite enables wrap on 300k
# (MP1..MP4) and 1M-line (MP5) documents, so
# Runner._enable_wrap_opts bumps this limit to 1.1M lines and forces
# "wrap_mode" to 1 (word wrap on) so tabs the suite opens inherit
# wrap as the global setting. Runner._disable_wrap_opts keeps the
# high max but sets wrap_mode to 0 (MP5 wrap-off row).
# Runner._restore_wrap_opts writes the user's originals back at
# the end of the run.
WRAP_MAX_KEY = 'wrap_enabled_max_lines'
WRAP_MAX_RUN_VALUE = 1100000
WRAP_MODE_KEY = 'wrap_mode'
WRAP_MODE_RUN_VALUE = 1
# the user's original values: saved by Runner._enable_wrap_opts
# (get_opt) the first time it runs, set back by
# Runner._restore_wrap_opts (set_opt)
WRAP_MAX_OLD = None
WRAP_MODE_OLD = None


# ----------------------------------------------------------------------------
# free-RAM check (before each test)
# ----------------------------------------------------------------------------

def free_ram_percent():
    """Return free/available RAM as percent of total, or None if unknown.
    Prefers 'available' (Linux MemAvailable / Windows ullAvailPhys) over
    free-only, so reclaimable cache is counted as usable."""
    try:
        import platform
        system = platform.system()
        if system == 'Linux':
            mem = {}
            with open('/proc/meminfo') as f:
                for line in f:
                    parts = line.split()
                    if len(parts) >= 2 and parts[0].endswith(':'):
                        key = parts[0][:-1]
                        mem[key] = int(parts[1])  # kB
            total = mem.get('MemTotal')
            # MemAvailable is the realistic "free for new allocs" figure
            avail = mem.get('MemAvailable')
            if avail is None:
                avail = mem.get('MemFree', 0) + mem.get('Buffers', 0) + mem.get('Cached', 0)
            if total and total > 0:
                return 100.0 * avail / total
        elif system == 'Windows':
            import ctypes
            class MEMORYSTATUSEX(ctypes.Structure):
                _fields_ = [
                    ('dwLength', ctypes.c_ulong),
                    ('dwMemoryLoad', ctypes.c_ulong),
                    ('ullTotalPhys', ctypes.c_ulonglong),
                    ('ullAvailPhys', ctypes.c_ulonglong),
                    ('ullTotalPageFile', ctypes.c_ulonglong),
                    ('ullAvailPageFile', ctypes.c_ulonglong),
                    ('ullTotalVirtual', ctypes.c_ulonglong),
                    ('ullAvailVirtual', ctypes.c_ulonglong),
                    ('ullAvailExtendedVirtual', ctypes.c_ulonglong),
                ]
            stat = MEMORYSTATUSEX()
            stat.dwLength = ctypes.sizeof(MEMORYSTATUSEX)
            if ctypes.windll.kernel32.GlobalMemoryStatusEx(ctypes.byref(stat)):
                if stat.ullTotalPhys > 0:
                    return 100.0 * stat.ullAvailPhys / stat.ullTotalPhys
        elif system == 'Darwin':
            import subprocess
            # total pages
            out = subprocess.check_output(['sysctl', '-n', 'hw.memsize'],
                                          universal_newlines=True).strip()
            total = int(out)
            # free + inactive (approx available)
            vm = subprocess.check_output(['vm_stat'], universal_newlines=True)
            page_size = 4096
            free = inactive = 0
            for line in vm.splitlines():
                if line.startswith('Pages free:'):
                    free = int(line.split(':')[1].strip().rstrip('.'))
                elif line.startswith('Pages inactive:'):
                    inactive = int(line.split(':')[1].strip().rstrip('.'))
                elif 'page size of' in line:
                    # "Mach Virtual Memory Statistics: (page size of 16384 bytes)"
                    try:
                        page_size = int(line.split('page size of')[1].split()[0])
                    except Exception:
                        pass
            if total > 0:
                avail = (free + inactive) * page_size
                return 100.0 * avail / total
    except Exception:
        pass
    return None


# ----------------------------------------------------------------------------
# shared helpers: newline normalization (used by Runner.check) and the
# FAIL-message preview
# ----------------------------------------------------------------------------

def N(t):
    """Normalize newlines for comparison (CRLF docs must compare equal)."""
    if isinstance(t, str) and '\r' in t:
        return t.replace('\r\n', '\n').replace('\r', '\n')
    return t

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

def make_big_lines(n):
    """The benchmark corpus: create random text with random lines lenght 490 to 510 char (hex() double the chars). 1M line=500mb, 300k line= 147mb (takes 16s to create 300k line)
    n lines of random hex from urandom-like
    bytes of length 245..255 (seeded for reproducibility). Matches the
    style of: os.urandom(random.randint(245, 255)).hex() per line."""
    # old: it uses no seed so everytime we test a diferent corpus
    # import os, tempfile, random, time; fpath = os.path.join(tempfile.gettempdir(), "cuda_undo_test_rand_1M.txt"); t1 = time.time(); open(fpath, "w").writelines(os.urandom(random.randint(245, 255)).hex() + "\n" for _ in range(1000000)); print(f"saved to {fpath} in {time.time()-t1:.4f}s")

    # new and better  using seed for reproducibility:
    # import os, tempfile, random, time; SEED = 20260904; rng = random.Random(SEED); fpath = os.path.join(tempfile.gettempdir(), "cuda_undo_test_rand_1M.txt"); t1 = time.time(); open(fpath, "w").writelines(bytes(rng.getrandbits(8) for _ in range(rng.randint(245, 255))).hex() + "\n" for _ in range(1000000)); print(f"saved to {fpath} in {time.time()-t1:.4f}s")
    
    rng = random.Random(SEED)
    lines = []
    for _i in range(n):
        length = rng.randint(245, 255)
        data = bytes(rng.getrandbits(8) for _ in range(length))
        lines.append(data.hex())
    return lines

_BIG_CACHE = {}

def big_lines(n):
    if n not in _BIG_CACHE:
        _BIG_CACHE.clear()
        _BIG_CACHE[n] = make_big_lines(n)
    return _BIG_CACHE[n]

# ----------------------------------------------------------------------------
# performance thresholds: measured healthy baseline -> (warn, fail).
# Shared judging infrastructure (like Runner._judge_cmd / Runner._hang):
# every test keeps its own measured baselines locally and only calls
# these two helpers - see the THRESHOLDS section in the module docstring.
# ----------------------------------------------------------------------------

# absolute jitter slack (seconds) added to the 2x / 3x rule so
# sub-second measurements cannot trip on timer noise; set both to
# 0.0 for the strict pure-multiplier rule
TH_SLACK_WARN = 0.25
TH_SLACK_FAIL = 1.0


def th(base):
    """(warn, fail) thresholds for a measured healthy baseline.

    The judging rule is "WARN above 2x, FAIL above 3x the healthy
    baseline", plus a small absolute slack:
        warn = 2 * base + TH_SLACK_WARN
        fail = 3 * base + TH_SLACK_FAIL
    For baselines above ~0.25 s the slack is negligible and this is
    the pure 2x / 3x rule; for tiny baselines (Hang2 ~0.01 s, small
    redo hangs ~0.1 s) the slack dominates and ordinary timer
    jitter cannot trip a WARN/FAIL."""
    return (2.0 * base + TH_SLACK_WARN,
            3.0 * base + TH_SLACK_FAIL)


def base_for(table, nlines):
    """Healthy baseline of one measured quantity for a corpus of
    nlines lines. table holds the reference machine's measured
    values keyed by the known corpora (300000 / 1000000); corpus
    sizes without a measurement are linearly scaled from the
    closest known one."""
    if nlines in table:
        return table[nlines]
    if 300000 in table:
        return table[300000] * (nlines / 300000.0)
    if 1000000 in table:
        return table[1000000] * (nlines / 1000000.0)
    return 0.0

# ----------------------------------------------------------------------------
# deferred-work ("hang") measurement - see the HANG TIME section in the
# module docstring. Each perf test defines its own hang thresholds
# (warn, fail) locally at the top of the test function.
# ----------------------------------------------------------------------------

_MP_CORPUS = {}


def mp_corpus_file(nlines):
    '''The manual benchmarks' corpus FILE, written once per run with
    their EXACT generator command (every MP test file_opens it, like
    the manual session had the file open):
        rng = random.Random(SEED)          # 20260904
        fpath = <LOAD_DIR>/cuda_undo_test_rand_300K.txt   (300k corpus)
                <LOAD_DIR>/cuda_undo_test_rand_1M.txt    (1M corpus)
                <LOAD_DIR>/cuda_undo_test_rand_<N>.txt   (other sizes)
        open(fpath, "w").writelines(
            bytes(rng.getrandbits(8) for _ in range(
                rng.randint(245, 255))).hex() + "\n"
            for _ in range(nlines))
    The content equals the first nlines lines of the benchmark's own
    1M-line file (same seed, same random stream) and big_lines(nlines)
    joined with EOLs. Returns (fpath, write_seconds); write_seconds
    is None when the file was already written earlier in this run.'''
    if nlines in _MP_CORPUS:
        return _MP_CORPUS[nlines], None
    # the manual benchmarks' own file names (other sizes keep the
    # plain-number form)
    if nlines == 1000000:
        fname = 'cuda_undo_test_rand_1M.txt'
    elif nlines == 300000:
        fname = 'cuda_undo_test_rand_300K.txt'
    else:
        fname = 'cuda_undo_test_rand_%d.txt' % nlines
    fpath = os.path.join(LOAD_DIR, fname)
    os.makedirs(LOAD_DIR, exist_ok=True)
    rng = random.Random(SEED)
    t1 = time.time()
    with open(fpath, 'w') as f:
        f.writelines(bytes(rng.getrandbits(8)
                           for _ in range(rng.randint(245, 255))).hex()
                     + '\n'
                     for _ in range(nlines))
    t_write = time.time() - t1
    _MP_CORPUS[nlines] = fpath
    return fpath, t_write


# ----------------------------------------------------------------------------
# runner
# ----------------------------------------------------------------------------

class Runner:

    def __init__(self):
        self.TE = None          # current test editor (set per-test, not shared)
        self.orig = None        # user's originally active editor
        self.wrap = 0
        self.results = []       # one dict per test
        self.perf = []          # one dict per perf run
        self.cur = None         # current test record
        self.fatal = None
        self._undo_grouped_orig = None  # saved PROP_UNDO_GROUPED
        self.log = []           # full console log (also opened in a tab)
        self.corpus = '300k'    # active corpus: '300k' or '1M'
        self.tests = None       # active catalog: TESTS_300K / TESTS_1M
        # (set by run / run_single; None until then)

    # ---- console ----

    def out(self, s=''):
        print(s)
        self.log.append(s)
        sys.stdout.flush()

    # ---- bookkeeping: the check/report channel every test writes to ----

    def begin(self, tid, name):
        self.cur = {
            'id': tid, 'name': name, 'wrap': self.wrap,
            'ok': 0, 'bad': 0, 'status': 'PASS', 'note': '',
        }
        self.results.append(self.cur)
        self.out()
        self.out('[%s] %s (wrap=%s)' % (tid, name, 'on' if self.wrap else 'off'))
        # free-RAM gate: under 30% ask Continue / Cancel
        if not self._memory_gate(tid):
            self.cur['status'] = 'SKIP'
            self.cur['note'] = 'cancelled: free RAM under 30%'
            self.out('    SKIP  free RAM under 30% (user cancelled)')
            return False
        return True

    def done(self):
        c = self.cur
        extra = ''
        if c['note']:
            extra = '   note: %s' % c['note'][:150]
        self.out('  => %s   (checks: %d ok, %d failed)%s' % (
            c['status'], c['ok'], c['bad'], extra))
        self.cur = None

    def t(self, tid, name, fn, own_tab=True):
        """Record one test: run fn() with exception bookkeeping.
        When own_tab is True (default), open a fresh suite tab for the
        duration of the test and close it afterwards. Load tests that
        open their own file tabs pass own_tab=False."""
        if not self.begin(tid, name):
            self.done()
            return
        ed = None
        try:
            if own_tab:
                ed, _ = self._open_tab(
                    '', tag='URTEST_TAB', wrap=self.wrap,
                    title=self._tab_title(tid))
                self.TE = ed
            fn()
        except Exception:
            self.cur['status'] = 'ERR'
            tb = traceback.format_exc()
            self.cur['note'] = tb.strip().splitlines()[-1][:200]
            self.out('    ERR   exception raised:')
            for ln in tb.strip().splitlines()[-5:]:
                self.out('            ' + ln)
        finally:
            if own_tab:
                self.TE = None
                self._close_tab(ed)
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

    def _memory_gate(self, tid):
        """Before each test: if free RAM < 30%, ask user to continue or cancel.
        Returns True to run the test, False to skip it."""
        pct = free_ram_percent()
        if pct is None:
            self.info('free RAM', 'unknown (skipping gate)')
            return True
        self.info('free RAM', '%.1f%%' % pct)
        if pct >= 30.0:
            return True
        msg = (
            'Free RAM is only %.1f%% (under 30%%).\n'
            'Test [%s] may use a lot of memory.\n\n'
            'OK = continue anyway\n'
            'Cancel = skip this test'
        ) % (pct, tid)
        res = cudatext.msg_box(
            msg, cudatext.MB_OKCANCEL | cudatext.MB_ICONWARNING)
        return res == cudatext.ID_OK

    # ---- deferred-work ("hang") measurement: the manual benchmark's
    #      exact two timed calls, used inline by every perf test ----

    def _hang(self, ed, tag=''):
        """Measure the deferred "hang" of the just-finished operation
        EXACTLY like the manual benchmark - nothing more, no polling,
        no CPU sampling:
          Hang1: app_proc(PROC_IDLE, True)
                 Application.ProcessMessages + Application.Idle, i.e.
                 one message-loop pump + idle pass; the deferred work
                 queued by the op runs inside this call
          Hang2: ed.action(EDACTION_UPDATE, 1)
                 Ed.Update(True, true): forces the WrapInfo update +
                 repaint (the deferred word-wrap recalculation)
        Returns (hang1, hang2)."""
        t1 = time.time()
        cudatext.app_proc(cudatext.PROC_IDLE, True)
        t2 = time.time()
        hang1 = t2 - t1
        t1 = time.time()
        ed.action(cudatext.EDACTION_UPDATE, 1)
        t2 = time.time()
        hang2 = t2 - t1
        if tag:
            self.info('hang(%s): Hang1 %.4fs (PROC_IDLE) + Hang2 %.4fs '
                      '(EDACTION_UPDATE) = %.4fs' % (
                          tag, hang1, hang2, hang1 + hang2))
        return hang1, hang2

    def _profile_line(self, name, t_cmd, h1, h2, base_total=None):
        """Print one clear profile line for a timed command + its hangs.
        Format:  profile  <name>: <cmd>s  Hang1: <h1>s  Hang2: <h2>s  Total: <sum>s
        Returns a profile dict {name, cmd, h1, h2, total, base}."""
        total = t_cmd + h1 + h2
        self.out('    profile  %s: %.4fs  Hang1: %.4fs  Hang2: %.4fs  '
                 'Total: %.4fs' % (name, t_cmd, h1, h2, total))
        return {
            'name': name, 'cmd': t_cmd, 'h1': h1, 'h2': h2, 'total': total,
            'base': base_total,
        }

    def _judge_cmd(self, name, t_cmd, h1, h2, th_cmd, th_hang,
                   fails, warns, base_total=None):
        """Judge one profiled command against its own thresholds.
        th_cmd / th_hang are (warn, fail) tuples for the pure command
        time and for (Hang1+Hang2) respectively. Appends to fails/warns
        lists. base_total is the reference-machine measured total
        (cmd + Hang1 + Hang2) used to derive the thresholds.
        Returns a profile dict {name, cmd, h1, h2, total, base}."""
        prof = self._profile_line(name, t_cmd, h1, h2, base_total=base_total)
        t_hang = h1 + h2
        for label, tv, th_ in (
                (name, t_cmd, th_cmd),
                (name + ' hang', t_hang, th_hang)):
            w_, f_ = th_
            if tv > f_:
                fails.append('%s %.2fs exceeds FAIL threshold %.2fs'
                             % (label, tv, f_))
            elif tv > w_:
                warns.append('%s %.2fs exceeds warn threshold %.2fs'
                             % (label, tv, w_))
        return prof

    # ---- editor api helpers ----

    def _ed_focused(self):
        """Standalone Editor object for the currently focused editor.
        cudatext.ed is the virtual Editor(0) whose handle 0 always refers
        to the focused editor, so storing it would not pin a tab; instead
        grab the editor's unique handle (PROP_HANDLE_SELF) and build an
        independent object with cudatext.Editor(handle)."""
        h = cudatext.ed.get_prop(cudatext.PROP_HANDLE_SELF)
        if h:
            return cudatext.Editor(h)
        return cudatext.ed

    # ---- lifecycle ----

    def run(self, corpus='300k'):
        """Run the whole performance suite on one corpus: '300k' or
        '1M'. MP1..MP4 run at the corpus's size (MP4 deletes 200k
        of 300k / 600k of 1M lines); MP5 is the same 1M-line
        file_open benchmark in both suites."""
        if corpus not in ('300k', '1M'):
            self.out('ERROR: unknown corpus %r (use "300k" or "1M")'
                     % corpus)
            return
        self.corpus = corpus
        self.tests = self._tests_for(corpus)
        nmain = 300000 if corpus == '300k' else 1000000
        ndel = 200000 if corpus == '300k' else 600000
        if corpus == '300k':
            ram_note = ('~2 GB RAM (MP5 adds the 1M-line / ~500 MB '
                        'corpus file)')
        else:
            ram_note = ('several GB RAM (every test builds a '
                        '~500 MB document)')
        self.out('=' * 66)
        self.out(' CudaText Performance Suite')
        self.out(' mode=all_%s: MP1..MP4 on the %d-line corpus '
                 '(MP4 deletes %dk lines), MP5: file_open of' % (
                     corpus, nmain, ndel // 1000))
        self.out('       the 1M-line corpus (same in both suites)   '
                 'seed=%d   %s' % (
                     SEED, time.strftime('%Y-%m-%d %H:%M:%S')))
        self.out(' NOTE: do not touch the editor while the suite is running.')
        self.out(' NOTE: the %s suite needs %s and several minutes;' % (
            corpus, ram_note))
        self.out('       it takes much longer if undo/redo is still slow.')
        self.out(' NOTE: corpus files are written once (~16s for the 300k')
        self.out('       file, ~1 min for the 1M file).')
        self.out('=' * 66)
        self._run_body(lambda: self._perf_suite())

    def run_single(self, tid, corpus='300k'):
        """Run only one perf test of the given corpus's catalog
        ('300k' or '1M'), by id from test_catalog(corpus). MP tests
        handle their wrap modes themselves."""
        if corpus not in ('300k', '1M'):
            self.out('ERROR: unknown corpus %r (use "300k" or "1M")'
                     % corpus)
            return
        self.corpus = corpus
        self.tests = self._tests_for(corpus)
        self.out('=' * 66)
        self.out(' CudaText Performance Suite')
        self.out(' mode=single test %s (%s corpus)   seed=%d   %s' % (
            tid, corpus, SEED, time.strftime('%Y-%m-%d %H:%M:%S')))
        self.out(' NOTE: do not touch the editor while the test is running.')
        self.out(' NOTE: perf tests build big docs (300k or 1M lines); they')
        self.out('       need RAM and can take a while.')
        self.out(' NOTE: MP tests write the corpus file once per run')
        self.out('       (~16s for 300k lines, ~1 min for 1M) if needed.')
        self.out('=' * 66)
        self._run_body(lambda: self._single_test(tid))

    def _tests_for(self, corpus):
        """The test catalog of a corpus: TESTS_1M for '1M', else
        TESTS_300K."""
        return TESTS_1M if corpus == '1M' else TESTS_300K

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

    # ---- unified tab open / close (for suite, load, bulk, MP) ----

    def _tab_title(self, tid, wrap=None, extra=''):
        """Tab title shown while a test runs, e.g. 'T01 wrap on'."""
        w = self.wrap if wrap is None else wrap
        s = '%s wrap %s' % (tid, 'on' if w else 'off')
        if extra:
            s = '%s %s' % (s, extra)
        return s

    def _configure_suite_tab(self, ed, tag='URTEST_TAB', wrap=None):
        """Apply the standard suite properties to an already-opened editor.
        Used by every path that creates a suite-owned tab so tagging,
        undo-grouping, saving flags and wrap stay consistent."""
        ed.set_prop(cudatext.PROP_TAG, tag)
        # Keep undo grouping ON (CudaText default).  Forcing it False
        # for the whole suite makes the 300k-line perf tests use >6 GB
        # RAM.  Individual tests that need exact per-op undo entries
        # disable it only for their own body and restore True after.
        ed.set_prop(cudatext.PROP_UNDO_GROUPED, True)
        ed.set_prop(cudatext.PROP_SAVING_FORCE_FINAL_EOL, False)
        ed.set_prop(cudatext.PROP_SAVING_TRIM_FINAL_EMPTY_LINES, False)
        ed.set_prop(cudatext.PROP_SAVING_TRIM_SPACES, False)
        if wrap is not None:
            ed.set_prop(cudatext.PROP_WRAP, wrap)
        elif tag in ('URTEST_TAB', 'URTEST_TAB2'):
            ed.set_prop(cudatext.PROP_WRAP, self.wrap)
        cudatext.app_proc(cudatext.PROC_IDLE, True)

    def _open_tab(self, path='', tag='URTEST_TAB', wrap=None, title=None):
        """Open path (empty string = fresh untitled) as a suite-owned tab.
        Returns (ed, file_open_result). file_open returns bool.
        title: optional PROP_TAB_TITLE (e.g. 'T01 wrap on')."""
        res = cudatext.file_open(path if path else '')
        ed = self._ed_focused()
        self._configure_suite_tab(ed, tag=tag, wrap=wrap)
        if title:
            ed.set_prop(cudatext.PROP_TAB_TITLE, title)
        return ed, res

    def _close_tab(self, ed):
        """Close a suite-owned tab (no save prompt)."""
        if ed is None:
            return
        try:
            ed.set_prop(cudatext.PROP_MODIFIED, False)
            ed.focus()
            ed.cmd(cmds.cmd_FileClose)
        except Exception:
            pass
        cudatext.app_proc(cudatext.PROC_IDLE, True)

    def _resave_user_json(self):
        """Make the running CudaText notice the user.json changes.

        cudax_lib's set_opt writes user.json directly on disk, which
        the running CudaText does NOT notice: option changes take
        effect only after a restart, or after user.json is saved
        through the editor (CudaText re-reads its options when
        user.json is saved in the editor). So this opens user.json in
        a tab, runs the save command, then closes the tab again.
        Called only from _enable_wrap_opts / _disable_wrap_opts /
        _restore_wrap_opts.

        If user.json is already open in any tab, that tab is closed
        first (no save prompt), then the file is opened fresh, marked
        modified, saved, and closed again."""
        path = os.path.join(cudatext.app_path(cudatext.APP_DIR_SETTINGS),
                            'user.json')
        if not os.path.isfile(path):
            return
        # Close any already-open user.json tab(s) first, so we always
        # start from a clean open (avoids focusing / saving an old
        # buffer that may not match the on-disk content set_opt wrote).
        try:
            for h in list(cudatext.ed_handles()):
                e = cudatext.Editor(h)
                if e.get_prop(cudatext.PROP_FN) == path:
                    e.set_prop(cudatext.PROP_MODIFIED, False)
                    e.focus()
                    e.cmd(cmds.cmd_FileClose)
            cudatext.app_proc(cudatext.PROC_IDLE, True)
        except Exception:
            pass
        ed = None
        try:
            if not cudatext.file_open(path):
                self.out('NOTE: cannot open user.json, changed options '
                         'not applied (restart CudaText to apply them)')
                return
            opened = self._ed_focused()
            if opened.get_prop(cudatext.PROP_FN) != path:
                # the open did not focus user.json: never touch - and
                # never close below - a tab that is not user.json
                self.out('NOTE: user.json tab not focused, changed '
                         'options not applied (restart to apply them)')
                return
            ed = opened
            # tag it suite-owned: _close_all_suite_tabs also closes
            # URTEST_USERJSON if this method dies before its own close
            ed.set_prop(cudatext.PROP_TAG, 'URTEST_USERJSON')
            ed.cmd(cmds.cmd_FileSave)
            
            # force wrap setting to take effect, enabling wrap for test MP5 does not work without this!
            cudatext.app_proc(cudatext.PROC_IDLE, True)
            ed.action(cudatext.EDACTION_UPDATE, 1)
        
        except Exception:
            self.out('NOTE: saving user.json failed, changed options '
                     'not applied (restart CudaText to apply them)')
        finally:
            # saved or failed: close the tab (no save prompt - either
            # it was just saved, or _close_tab clears the flag)
            if ed is not None:
                self._close_tab(ed)

    def _enable_wrap_opts(self):
        """Enable suite wrap options in user.json and apply them.

        Saves the user's original wrap_enabled_max_lines /
        wrap_mode the first time it is called, then sets
        wrap_enabled_max_lines to WRAP_MAX_RUN_VALUE (so 300k/1M-line
        docs can enable wrap) and wrap_mode to WRAP_MODE_RUN_VALUE
        (word wrap on; new tabs inherit it). set_opt only writes the
        file on disk: _resave_user_json re-saves user.json in the
        editor so the running CudaText re-reads the options.
        Called from _setup and from MP5 when wrap is on."""
        global WRAP_MAX_OLD
        global WRAP_MODE_OLD
        # save the user's originals only once (first enable)
        if WRAP_MAX_OLD is None:
            WRAP_MAX_OLD = cudax_lib.get_opt(WRAP_MAX_KEY)
        if WRAP_MODE_OLD is None:
            # wrap_mode lives in cudax_lib's OPT2PROP map, so the
            # default CONFIG_LEV_ALL get would read the current tab's
            # PROP_WRAP, not user.json: read CONFIG_LEV_USER
            # (user.json), then CONFIG_LEV_DEF (default.json) when
            # the user never set the option.
            WRAP_MODE_OLD = cudax_lib.get_opt(
                WRAP_MODE_KEY, lev=cudax_lib.CONFIG_LEV_USER)
            if WRAP_MODE_OLD is None:
                WRAP_MODE_OLD = cudax_lib.get_opt(
                    WRAP_MODE_KEY, lev=cudax_lib.CONFIG_LEV_DEF)
        cudax_lib.set_opt(WRAP_MAX_KEY, WRAP_MAX_RUN_VALUE)
        cudax_lib.set_opt(WRAP_MODE_KEY, WRAP_MODE_RUN_VALUE)
        self.out('info: user.json: %s: %s -> %s, %s: %s -> %s '
                 '(enable wrap opts)' % (
                     WRAP_MAX_KEY, WRAP_MAX_OLD, WRAP_MAX_RUN_VALUE,
                     WRAP_MODE_KEY, WRAP_MODE_OLD, WRAP_MODE_RUN_VALUE))
        self._resave_user_json()

    def _disable_wrap_opts(self):
        """Turn global wrap off for the suite (keep the high max).

        Sets wrap_mode to 0 so new tabs inherit wrap off, while
        leaving wrap_enabled_max_lines at WRAP_MAX_RUN_VALUE so a
        later enable can still wrap 300k/1M-line docs. Does NOT
        restore the user's originals - that is _restore_wrap_opts.
        Called from MP5 when wrap is off."""
        cudax_lib.set_opt(WRAP_MAX_KEY, WRAP_MAX_RUN_VALUE)
        cudax_lib.set_opt(WRAP_MODE_KEY, 0)
        self.out('info: user.json: %s: %s, %s: 0 '
                 '(disable wrap opts)' % (
                     WRAP_MAX_KEY, WRAP_MAX_RUN_VALUE, WRAP_MODE_KEY))
        self._resave_user_json()

    def _restore_wrap_opts(self):
        """Restore the user's original wrap options in user.json.

        Writes WRAP_MAX_OLD / WRAP_MODE_OLD back (if they were saved)
        and re-applies them via _resave_user_json. Called from
        _cleanup at the end of the run."""
        if WRAP_MAX_OLD is not None:
            cudax_lib.set_opt(WRAP_MAX_KEY, WRAP_MAX_OLD)
        if WRAP_MODE_OLD is not None:
            cudax_lib.set_opt(WRAP_MODE_KEY, WRAP_MODE_OLD)
        self.out('info: user.json: %s: %s, %s: %s '
                 '(restore wrap opts)' % (
                     WRAP_MAX_KEY, WRAP_MAX_OLD,
                     WRAP_MODE_KEY, WRAP_MODE_OLD))
        self._resave_user_json()

    def _setup(self):
        # Enable suite wrap options (high wrap_enabled_max_lines +
        # wrap_mode on) so 300k/1M-line docs can wrap and new tabs
        # inherit wrap as the global setting. See _enable_wrap_opts.
        self._enable_wrap_opts()
        
        # Capture the user's active editor (independent Editor object -
        # see _ed_focused) and its raw handle separately, so _cleanup
        # can check liveness via cudatext.ed_handles() without calling
        # any method on a possibly-dead Editor object (which logs a
        # native "bad handle" error even when the Python exception is
        # caught).
        self.orig = self._ed_focused()
        self.orig_handle = (self.orig.get_prop(cudatext.PROP_HANDLE_SELF)
                            if self.orig is not None else None)
        self.TE = None
        try:
            self._undo_grouped_orig = self.orig.get_prop(
                cudatext.PROP_UNDO_GROUPED, '')
        except Exception:
            self._undo_grouped_orig = True

    def _cleanup(self):
        # Script end: restore the user's original wrap options FIRST
        # (before the tab closing below), then close every tab this
        # suite opened, free caches, remove LOAD_DIR. Failures are
        # printed (not swallowed) so leaks/close bugs are visible.
        self._close_all_suite_tabs()
        self._restore_wrap_opts()
        if self.orig is not None:
            # self.orig was captured at _setup() time as an
            # independent Editor(handle). If that tab got closed
            # during the run, the handle is stale: calling ANY method
            # on the Editor object (repr(), get_prop(), .focus()) is
            # a use-after-free that logs a native "bad handle" error
            # (or worse, crashes) even when the Python exception is
            # caught. So check liveness via the raw handle captured
            # in _setup(), against cudatext.ed_handles(), without
            # touching self.orig at all until we know it's safe.
            if (self.orig_handle is not None
                    and self.orig_handle in cudatext.ed_handles()):
                self.orig.focus()
            else:
                self.out('info: original tab no longer exists, '
                         'skipping focus restore')
        _BIG_CACHE.clear()
        _MP_CORPUS.clear()
        if os.path.isdir(LOAD_DIR):
            import shutil
            shutil.rmtree(LOAD_DIR, ignore_errors=True)
        self.TE = None

    def _close_all_suite_tabs(self):
        """Close tabs tagged URTEST_TAB / URTEST_TAB2 / URTEST_LOAD /
        URTEST_USERJSON (the last one: a user.json tab left over from
        _resave_user_json, e.g. if it failed mid-way)."""
        tags = {'URTEST_TAB', 'URTEST_TAB2', 'URTEST_LOAD',
                'URTEST_USERJSON'}
        closed = 0
        handles = list(cudatext.ed_handles())
        for h in handles:
            e = cudatext.Editor(h)
            tag = str(e.get_prop(cudatext.PROP_TAG) or '')
            if tag not in tags:
                continue
            e.set_prop(cudatext.PROP_MODIFIED, False)
            e.focus()
            e.cmd(cmds.cmd_FileClose)
            closed += 1
        if self._undo_grouped_orig is not None:
            cudatext.ed.set_prop(cudatext.PROP_UNDO_GROUPED,
                                 self._undo_grouped_orig)
        if closed:
            cudatext.app_proc(cudatext.PROC_IDLE, True)
            self.out('info: closed %d suite tab(s)' % closed)

    def _summary(self):
        n = len(self.results)
        st = {'PASS': 0, 'FAIL': 0, 'ERR': 0, 'SKIP': 0}
        for r in self.results:
            if r['status'] in st:
                st[r['status']] += 1
        self.out()
        self.out('=' * 66)
        self.out(' SUMMARY   (%s)' % time.strftime('%Y-%m-%d %H:%M:%S'))
        self.out('=' * 66)
        self.out(' tests run: %d    PASS: %d    FAIL: %d    ERR: %d    SKIP: %d' % (
            n, st['PASS'], st['FAIL'], st['ERR'], st['SKIP']))
        bad = [r for r in self.results if r['status'] in ('FAIL', 'ERR')]
        if bad:
            self.out(' failed tests:')
            for r in bad:
                self.out('   [%s] %s (wrap=%s)' % (r['id'], r['name'],
                         'on' if r.get('wrap') else 'off'))
                if r['note']:
                    self.out('        %s' % r['note'][:220])
        if self.perf:
            self.out('\n\nperformance results (one row per timed command):')
            self.out('   Thresholds judge each command and its own hang')
            self.out('   separately (not a summed hang).')
            self.out('   base = measured total (cmd+Hang1+Hang2) on ref machine')
            self.out('          (Intel Core i7 2.80 GHz, wrap ON); shown only for wrap=on rows.')
            # header
            self.out('\n   %-5s %-4s %-7s %-14s %9s %9s %9s %9s %9s  %-6s' % (
                'test', 'wrap', 'lines', 'command',
                'cmd', 'Hang1', 'Hang2', 'Total', 'base', 'status'))
            self.out('   ' + '-' * 88)

            for p in self.perf:
                wrap_s = ('on' if p.get('wrap') else 'off') \
                    if not str(p['id']).startswith('L') else '-'
                profiles = p.get('profiles') or []
                if profiles and isinstance(profiles[0], dict):
                    # one table row per profiled command
                    # base only when wrap=on (baselines measured with wrap)
                    for i, pl in enumerate(profiles):
                        tid = p['id'] if i == 0 else ''
                        w = wrap_s if i == 0 else ''
                        lines_s = ('%7d' % p['lines']) if i == 0 else ' ' * 7
                        pstat = p['status'] if i == 0 else ''
                        base = pl.get('base')
                        if p.get('wrap') and base is not None:
                            base_s = '%8.4fs' % base
                        else:
                            base_s = '        -'
                        self.out(
                            '   %-5s %-4s %s %-14s %8.4fs %8.4fs %8.4fs '
                            '%8.4fs %s  %-6s' % (
                                tid, w, lines_s, pl['name'],
                                pl['cmd'], pl['h1'], pl['h2'], pl['total'],
                                base_s, pstat))
                elif profiles:
                    # legacy string profiles (should not happen after update)
                    for i, pl in enumerate(profiles):
                        tid = p['id'] if i == 0 else ''
                        w = wrap_s if i == 0 else ''
                        lines_s = ('%7d' % p['lines']) if i == 0 else ' ' * 7
                        self.out('   %-5s %-4s %s  %s' % (tid, w, lines_s, pl))
                else:
                    # no profiles list — fall back to command/undo/redo columns
                    hang = p.get('hang')
                    hang_s = ('%8.4fs' % hang) if hang is not None \
                        else '        -'
                    self.out(
                        '   %-5s %-4s %7d %-14s %8.4fs %9s %9s %s  %-6s' % (
                            p['id'], wrap_s, p['lines'],
                            'command',
                            p['del'], '-', '-', hang_s, p['status']))
                    if p.get('undo'):
                        self.out(
                            '   %-5s %-4s %7s %-14s %8.4fs %9s %9s %9s  %-6s'
                            % ('', '', '', 'undo',
                               p['undo'], '-', '-', '-', ''))
                    if p.get('redo'):
                        self.out(
                            '   %-5s %-4s %7s %-14s %8.4fs %9s %9s %9s  %-6s'
                            % ('', '', '', 'redo',
                               p['redo'], '-', '-', '-', ''))
                if p.get('note'):
                    self.out('        note: %s' % p['note'][:200])
        perf_fail = any(p['status'] == 'FAIL' for p in self.perf)
        if st['FAIL'] == 0 and st['ERR'] == 0 and not perf_fail:
            overall = 'ALL TESTS PASSED'
        else:
            overall = 'FAILURES DETECTED - see details above'
        self.out(' overall: %s' % overall)
        self.out('=' * 66)
        cudatext.msg_status('Perf tests: %s (see console)' % overall)

        # ---- end-of-run UI: summary dialog + log tab ----
        lines = [
            'CudaText perf tests',
            '',
            'tests run:  %d' % n,
            'PASS:       %d' % st['PASS'],
            'FAIL:       %d' % st['FAIL'],
            'ERR:        %d' % st['ERR'],
            'SKIP:       %d' % st['SKIP'],
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
        flags = cudatext.MB_OK
        if st['FAIL'] or st['ERR'] or perf_fail:
            flags |= cudatext.MB_ICONWARNING
        else:
            flags |= cudatext.MB_ICONINFO
        cudatext.msg_box(summary_text, flags)

        # open a new untitled tab with the full console log
        log_text = '\n'.join(self.log)
        if not cudatext.file_open(''):
            cudatext.ed.cmd(cmds.cmd_FileNew)
        log_ed = self._ed_focused()
        log_ed.set_text_all(log_text)
        log_ed.set_prop(cudatext.PROP_TAB_TITLE, 'Perf test log')
        log_ed.set_prop(cudatext.PROP_MODIFIED, False)
        log_ed.set_caret(0, 0)

    # ---- suite driving (uses the TESTS_300K / TESTS_1M registries) ----

    def _perf_suite(self):
        self.out()
        self.out('------ performance & mass-op tests (MP*) ------')
        # MP* are the exact manual-benchmark replicas of the active
        # corpus (self.tests). They are heavy (300k / 1M lines) and
        # run their wrap-off + wrap-on rows themselves.
        for tid, _name, fn in self.tests:
            if tid.startswith('M'):
                fn(self)

    def _single_test(self, tid):
        """Run exactly one perf test from the active corpus catalog
        (self.tests). MP tests manage their wrap modes themselves."""
        for t_id, name, fn in self.tests:
            if t_id != tid:
                continue
            # perf test: makes its own records and wrap modes
            self.out()
            self.out('------ single perf test %s (%s corpus) ------'
                     % (tid, self.corpus))
            fn(self)
            return
        self.out()
        self.out('ERROR: no test with id %r in the %s corpus catalog'
                 % (tid, self.corpus))

    def test_catalog(self, corpus='300k'):
        """All tests selectable via the "run single test" commands:
        (id, label) pairs of one corpus's catalog ('300k' or '1M');
        ids are unique inside a catalog."""
        return [(tid, name) for tid, name, _fn in self._tests_for(corpus)]


    # ========================================================================
    # STANDALONE MANUAL-BENCHMARK REPLICAS MP1..MP5 (2026-09-08)
    # The 5 manual console performance tests, replicated command for
    # command. The ONLY document setup is file_open of the corpus file
    # (mp_corpus_file() writes it once per run with the benchmark's
    # exact seeded generator) - the manual session's state: no
    # set_text_all, no suite-tab commands, no caret resets. Inside the
    # timed part NOTHING runs between the manual test's own commands;
    # Hang1/Hang2 are the manual test's two timed calls (Runner._hang).
    # Checks are read-only and run after each op's timing so they
    # cannot pollute it. Every wrap variant re-opens the file in a
    # fresh tab (the previous variant's tab is closed), so each row is
    # a clean, repeatable measurement of the manual test's own command
    # sequence. Every test takes the corpus size as its nlines
    # parameter (MP4 also the delete count, auto-picked per corpus:
    # 200k-of-300k / 600k-of-1M); the TESTS_300K / TESTS_1M registries
    # bind the two suites, so the menu commands run either corpus, and
    # console calls like Runner().test_MP1(1000000) or
    # Runner().run('1M') reach the exact 1M-line scale of the manual
    # benchmarks. MP5 is the same 1M-line file_open benchmark in both
    # suites.
    # ========================================================================

    def test_MP1(self, nlines=300000):
        '''Manual test1: replace_lines of ALL lines with the corpus
        file's readlines(), word wrap off and on. The manual test, run
        in the console against the opened corpus file:
            ed.set_prop(PROP_WRAP, 1)                  # untimed setup
            lines = open(fpath, "r").readlines()       # untimed input
            t1 = time.time()
            ed.replace_lines(0, ed.get_line_count()-1, lines)
            t2 = time.time()                           # op time
            t1 = time.time(); app_proc(PROC_IDLE, True);     t2  # Hang1
            t1 = time.time(); ed.action(EDACTION_UPDATE, 1); t2  # Hang2
        The document is the corpus FILE opened with file_open (the
        manual session's tab state), NOT a set_text_all-built doc.
        The replaced content equals the file's own text, so the text
        stays the same - the manual test measures the replace cost
        itself, and so does this replica. No undo/redo here: the
        manual test1 had none (its speed is covered by MP3/MP4).'''

        # ---- thresholds (warn, fail) per profiled quantity ----
        # Command time and its own Hang1+Hang2 are judged separately,
        # each against th(baseline) = (2*base + 0.25, 3*base + 1.0):
        # the "WARN above 2x / FAIL above 3x" rule with a jitter slack
        # (see the THRESHOLDS section in the module docstring).
        # Measured healthy baselines of this op (manual benchmark,
        # reference machine):
        #   corpus   replace_lines   Hang1 + Hang2
        #   300k     0.9491s         2.3201 + 0.0090 = 2.3291s
        #   1M       3.0732s         7.8765 + 0.0100 = 7.8865s
        BASE_CMD  = {300000: 0.9491, 1000000: 3.0732}
        BASE_HANG = {300000: 2.3291, 1000000: 7.8865}
        TH_OP   = th(base_for(BASE_CMD, nlines))   # 300k (2.15, 3.85), 1M (6.40, 10.22)
        TH_HANG = th(base_for(BASE_HANG, nlines))  # 300k (4.91, 7.99), 1M (16.02, 24.66)

        # note about real consumed time:after replace_lines finishes in 3.0762s (for 1M lines) it takes 8s to show text and for cpu to return to 0%, and another 8s when i do the first click on text or first scroll, it eats 25% cpu for 8s while app hangs,so real total time is 19s
        # to automate the time spent calculation of hang1 and hang2 we can use app_proc(PROC_IDLE, True) to calculate hang1 and ed.action(EDACTION_UPDATE,1) to calculate hang2 as used bellow
        '''
        import os, tempfile, random, time; fpath = os.path.join(tempfile.gettempdir(), "cuda_undo_test_rand_300K.txt"); t1 = time.time(); open(fpath, "w").writelines(os.urandom(random.randint(245, 255)).hex() + "\n" for _ in range(300000)); print(f"saved to {fpath} in {time.time()-t1:.4f}s");

        import os, tempfile, time; file_open(""); app_proc(PROC_IDLE, True); ed.set_prop(PROP_WRAP,1); fpath = os.path.join(tempfile.gettempdir(), "cuda_undo_test_rand_300K.txt"); lines = open(fpath, "r").readlines(); t1 = time.time(); ed.replace_lines(0, ed.get_line_count()-1, lines); t2 = time.time(); print(f"replace_lines: {t2-t1:.4f}s"); 
        t1 = time.time(); app_proc(PROC_IDLE, True); t2 = time.time(); print(f"Hang1: {t2-t1:.4f}s");
        t1 = time.time(); ed.action(EDACTION_UPDATE,1); t2 = time.time(); print(f"Hang2: {t2-t1:.4f}s"); del lines

        replace_lines: 0.9011s
        Hang1: 2.6972s
        Hang2: 2.0361s
        '''

        # to test it from this script use this, uncomment bellow code and see console to see the diference of both methods
        '''
        import os, tempfile, random, time; fpath = os.path.join(tempfile.gettempdir(), "cuda_undo_test_rand_300K.txt"); t1 = time.time(); open(fpath, "w").writelines(os.urandom(random.randint(245, 255)).hex() + "\n" for _ in range(300000)); print(f"saved to {fpath} in {time.time()-t1:.4f}s")

        # met1: this method is more correct than met2 because it reproduce exactly the test i run manually in cuda console, because i first start a tab, then i click in console then i run the one line command, when i open the tab cuda had the time to idle, but in met2 i don t use PROC_IDLE so the hang1 and hang2 are both mixed and calculated in the first PROC_IDLE hang1, while met1 show them clearly in diferent time so i can calculate the timinig in better granularity, in MP functions i will use met1

        cudatext.file_open("")
        ed1 = self._ed_focused()
        ed1.set_prop(cudatext.PROP_TAG, 'URTEST_LOAD')
        cudatext.app_proc(cudatext.PROC_IDLE, True);
        import os, tempfile, time; ed1.set_prop(cudatext.PROP_WRAP,1); fpath = os.path.join(tempfile.gettempdir(), "cuda_undo_test_rand_300K.txt"); lines = open(fpath, "r").readlines(); t1 = time.time(); ed1.replace_lines(0, ed1.get_line_count()-1, lines); t2 = time.time(); print(f"replace_lines: {t2-t1:.4f}s"); 
        t1 = time.time(); cudatext.app_proc(cudatext.PROC_IDLE, True); t2 = time.time(); print(f"Hang1: {t2-t1:.4f}s");
        t1 = time.time(); ed1.action(cudatext.EDACTION_UPDATE,1); t2 = time.time(); print(f"Hang2: {t2-t1:.4f}s"); del lines
        self._close_tab(ed1)
        
        # return:
          # replace_lines: 0.9011s
          # Hang1: 2.3531s
          # Hang2: 1.9571s
          
          
        # _______
        # met2:

        cudatext.file_open("")
        ed2 = self._ed_focused()
        ed2.set_prop(cudatext.PROP_TAG, 'URTEST_LOAD')
        import os, tempfile, time; ed2.set_prop(cudatext.PROP_WRAP,1); fpath = os.path.join(tempfile.gettempdir(), "cuda_undo_test_rand_300K.txt"); lines = open(fpath, "r").readlines(); t1 = time.time(); ed2.replace_lines(0, ed2.get_line_count()-1, lines); t2 = time.time(); print(f"replace_lines: {t2-t1:.4f}s"); 
        t1 = time.time(); cudatext.app_proc(cudatext.PROC_IDLE, True); t2 = time.time(); print(f"Hang1: {t2-t1:.4f}s");
        t1 = time.time(); ed2.action(cudatext.EDACTION_UPDATE,1); t2 = time.time(); print(f"Hang2: {t2-t1:.4f}s"); del lines
        self._close_tab(ed2)
        
        # return:
          # replace_lines: 0.9031s
          # Hang1: 4.4093s
          # Hang2: 0.0210s
        '''
        
        fpath, t_write = mp_corpus_file(nlines)
        for w in (0, 1):
            self.wrap = w
            if not self.begin('MP1', 'MP1 (manual test1): replace_lines of all '
                              '%d corpus-file lines' % nlines):
                self.done()
                continue
            ed = None
            try:
                # ---- setup: the manual session's state, untimed ----
                ed, res = self._open_tab(
                    "", tag='URTEST_LOAD', wrap=w,
                    title=self._tab_title('MP1', w))
                self.info('doc', '%s: %d lines (+fake), %d bytes%s' % (
                    fpath, nlines, os.path.getsize(fpath),
                    (', written in %.1fs' % t_write) if t_write is not None
                    else ' (already written this run)'))
                lines = open(fpath, 'r').readlines()
                self.info('op', 'replace_lines(0, get_line_count()-1, %d '
                          'readlines() items)' % len(lines))
                # as i explained above, calling PROC_IDLE here after open() is important to get a correct hang1 and hang2 separated timing
                cudatext.app_proc(cudatext.PROC_IDLE, True)
                t1 = time.time()
                ok = ed.replace_lines(0, ed.get_line_count() - 1, lines)
                t2 = time.time()
                t_op = t2 - t1
                # Hang1 (PROC_IDLE) + Hang2 (EDACTION_UPDATE), right after the op
                h1, h2 = self._hang(ed)   # no tag: detail comes from _profile_line

                # ---- read-only verification, after the timing ----
                # items end with their EOLs: concatenation = file text
                corpus = ''.join(lines)
                del lines
                self.check('text after replace (final EOL + fake line)',
                           ed.get_text_all(), corpus)
                self.check('line_count after replace (fake line incl.)',
                           ed.get_line_count(), nlines + 1)
                del corpus

                # ---- per-command profile + thresholds (command only) ----
                perf_fails = []
                perf_warns = []
                profiles = [
                    self._judge_cmd(
                        'replace_lines', t_op, h1, h2, TH_OP, TH_HANG,
                        perf_fails, perf_warns,
                        base_total=base_for(BASE_CMD, nlines) +
                                   base_for(BASE_HANG, nlines))
                ]

                text_bad = self.cur['bad'] > 0
                status = ('FAIL' if (perf_fails or text_bad)
                          else ('WARN' if perf_warns else 'PASS'))
                self.perf.append({
                    'id': 'MP1', 'wrap': self.wrap, 'lines': nlines,
                    'del': t_op, 'undo': 0.0, 'redo': 0.0,
                    'hang': h1 + h2,   # this command's hang only
                    'profiles': profiles,
                    'status': status, 'note': '; '.join(perf_fails +
                                                        perf_warns),
                })
                if perf_fails:
                    self.cur['bad'] += 1
                    if self.cur['status'] != 'ERR':
                        self.cur['status'] = 'FAIL'
                    self.out('    FAIL  perf: %s' % '; '.join(perf_fails))
                elif perf_warns:
                    if self.cur['note']:
                        self.cur['note'] += '; '
                    self.cur['note'] = (self.cur['note'] + '; '.join(
                        perf_warns))[:200]
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
            finally:
                self._close_tab(ed)
            self.done()

    def test_MP2(self, nlines=300000):
        '''Manual test2: set_text_all of the whole corpus file text,
        word wrap off and on. The manual test, run in the console
        against the opened corpus file:
            text = open(fpath, "r").read()             # untimed input
            t1 = time.time(); ed.set_text_all(text); t2 = time.time()
            t1 = time.time(); app_proc(PROC_IDLE, True);     t2  # Hang1
            t1 = time.time(); ed.action(EDACTION_UPDATE, 1); t2  # Hang2
        The start document is the corpus FILE opened with file_open
        (empty undo history - exactly the manual session's tab), not a
        set_text_all-built doc on the suite tab: the doc's undo/wrap
        state is part of what makes the timing comparable to the
        manual numbers. No undo/redo: the manual test2 had none (the
        set_text_all undo contract itself is pinned by T23).'''
        # ---- thresholds (warn, fail) per profiled quantity ----
        # th(baseline) = (2*base + 0.25, 3*base + 1.0): WARN above 2x /
        # FAIL above 3x with jitter slack (module docstring, THRESHOLDS).
        # Measured healthy baselines of this op (manual benchmark):
        #   corpus   set_text_all    Hang1 + Hang2
        #   300k     3.1442s         2.3361 + 0.0180 = 2.3541s
        #   1M       9.4745s         8.1685 + 0.0090 = 8.1775s
        BASE_CMD  = {300000: 3.1442, 1000000: 9.4745}
        BASE_HANG = {300000: 2.3541, 1000000: 8.1775}
        TH_OP   = th(base_for(BASE_CMD, nlines))   # 300k (6.54, 10.43), 1M (19.20, 29.42)
        TH_HANG = th(base_for(BASE_HANG, nlines))  # 300k (4.96, 8.06), 1M (16.61, 25.53)

        fpath, t_write = mp_corpus_file(nlines)
        for w in (0, 1):
            self.wrap = w
            if not self.begin('MP2', 'MP2 (manual test2): set_text_all of the '
                              '%d-line corpus file text' % nlines):
                self.done()
                continue
            ed = None
            try:
                # ---- setup: the manual session's state, untimed ----
                ed, res = self._open_tab(
                    "", tag='URTEST_LOAD', wrap=w,
                    title=self._tab_title('MP2', w))
                self.info('doc', '%s: %d lines (+fake), %d bytes%s' % (
                    fpath, nlines, os.path.getsize(fpath),
                    (', written in %.1fs' % t_write) if t_write is not None
                    else ' (already written this run)'))
                text = open(fpath, 'r').read()
                self.info('op', 'set_text_all(%d chars)' % len(text))

                # see MP1 for why this is necesary
                cudatext.app_proc(cudatext.PROC_IDLE, True)
                t1 = time.time()
                ed.set_text_all(text)
                t2 = time.time()
                t_op = t2 - t1
                # Hang1 + Hang2, right after the op
                h1, h2 = self._hang(ed)
                # ---- read-only verification, after the timing ----
                self.check('file_open returns True', res, True)
                self.check('text after set_text_all', ed.get_text_all(),
                           text)
                self.check('line_count after set_text_all (fake line '
                           'incl.)', ed.get_line_count(), nlines + 1)
                del text

                # ---- per-command profile + thresholds (command only) ----
                perf_fails = []
                perf_warns = []
                profiles = [
                    self._judge_cmd(
                        'set_text_all', t_op, h1, h2, TH_OP, TH_HANG,
                        perf_fails, perf_warns,
                        base_total=base_for(BASE_CMD, nlines) +
                                   base_for(BASE_HANG, nlines))
                ]

                text_bad = self.cur['bad'] > 0
                status = ('FAIL' if (perf_fails or text_bad)
                          else ('WARN' if perf_warns else 'PASS'))
                self.perf.append({
                    'id': 'MP2', 'wrap': self.wrap, 'lines': nlines,
                    'del': t_op, 'undo': 0.0, 'redo': 0.0,
                    'hang': h1 + h2,
                    'profiles': profiles,
                    'status': status, 'note': '; '.join(perf_fails +
                                                        perf_warns),
                })
                if perf_fails:
                    self.cur['bad'] += 1
                    if self.cur['status'] != 'ERR':
                        self.cur['status'] = 'FAIL'
                    self.out('    FAIL  perf: %s' % '; '.join(perf_fails))
                elif perf_warns:
                    if self.cur['note']:
                        self.cur['note'] += '; '
                    self.cur['note'] = (self.cur['note'] + '; '.join(
                        perf_warns))[:200]
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
            finally:
                self._close_tab(ed)
            self.done()

    def test_MP3(self, nlines=300000):
        '''Manual test3: load corpus via replace_lines (same as the
        manual console setup), then select ALL, delete, undo, redo -
        word wrap off and on. The manual test, run in the console:
            ed.set_prop(PROP_WRAP, 1)                      # untimed
            lines = open(fpath, "r").readlines()           # untimed
            ed.replace_lines(0, ed.get_line_count()-1, lines)  # untimed setup
            del lines
            ed.set_caret(0, ed.get_line_count(), 0, 0)
            ed.cmd(cCommand_TextDeleteSelection)
            t1; ed.cmd(cCommand_Undo); t2; Hang1; Hang2
            t1; ed.cmd(cCommand_Redo); t2; Hang1; Hang2
        Document setup matches MP1: empty tab + replace_lines of the
        corpus file's readlines(). The select-all caret
        y=get_line_count() overshoots by one (fake last line): the
        delete clamps to the true document end, and undo cannot
        restore the overshooting caret as-is - the check accepts the
        exact pre state and the clamped form.'''
        # ---- thresholds (warn, fail) per profiled quantity ----
        # th(baseline) = (2*base + 0.25, 3*base + 1.0): WARN above 2x /
        # FAIL above 3x with jitter slack (module docstring, THRESHOLDS).
        # Measured healthy baselines (manual benchmark):
        #   corpus   Delete   Del hang   Undo     Undo hang   Redo     Redo hang
        #   300k     0.8140   0.1190     2.6962   2.2231      0.8801   0.1130
        #   1M       2.8732   0.4850     9.3815   7.7514      2.9832   0.1730
        # Delete hang is now measured (Hang1+Hang2 after TextDeleteSelection).
        BASE_DEL       = {300000: 0.8140, 1000000: 2.8732}
        BASE_DEL_HANG  = {300000: 0.1190, 1000000: 0.4850}
        BASE_UNDO      = {300000: 2.6962, 1000000: 9.3815}
        BASE_UNDO_HANG = {300000: 2.2231, 1000000: 7.7514}
        BASE_REDO      = {300000: 0.8801, 1000000: 2.9832}
        BASE_REDO_HANG = {300000: 0.1130, 1000000: 0.1730}
        TH_DEL       = th(base_for(BASE_DEL, nlines))
        TH_DEL_HANG  = th(base_for(BASE_DEL_HANG, nlines))
        TH_UNDO      = th(base_for(BASE_UNDO, nlines))
        TH_UNDO_HANG = th(base_for(BASE_UNDO_HANG, nlines))
        TH_REDO      = th(base_for(BASE_REDO, nlines))
        TH_REDO_HANG = th(base_for(BASE_REDO_HANG, nlines))

        fpath, t_write = mp_corpus_file(nlines)
        for w in (0, 1):
            self.wrap = w
            if not self.begin('MP3', 'MP3 (manual test3): replace_lines load + '
                              'select all + delete, undo, redo of %d lines'
                              % nlines):
                self.done()
                continue
            ed = None
            try:
                # ---- setup: empty tab + corpus via replace_lines (untimed)
                ed, res = self._open_tab(
                    "", tag='URTEST_LOAD', wrap=w,
                    title=self._tab_title('MP3', w))
                self.info('doc', '%s: %d lines (+fake), %d bytes%s' % (
                    fpath, nlines, os.path.getsize(fpath),
                    (', written in %.1fs' % t_write) if t_write is not None
                    else ' (already written this run)'))
                lines = open(fpath, 'r').readlines()
                corpus = ''.join(lines)
                self.info('setup', 'replace_lines(0, get_line_count()-1, %d '
                          'readlines() items) [untimed]' % len(lines))
                ok = ed.replace_lines(0, ed.get_line_count() - 1, lines)
                del lines
                self.check('replace_lines returns True (setup)', ok, True)
                self.check('text after replace_lines setup',
                           ed.get_text_all(), corpus)
                self.check('line_count after replace_lines setup '
                           '(fake line incl.)',
                           ed.get_line_count(), nlines + 1)
                # ---- the manual test's timed commands ----
                ed.set_caret(0, ed.get_line_count(), 0, 0)
                self.info('op', 'set_caret(0, get_line_count()=%d, 0, 0) '
                          '+ TextDeleteSelection + Undo + Redo' % (
                              ed.get_line_count()))
                # see MP1 for why this is necesary
                cudatext.app_proc(cudatext.PROC_IDLE, True)
                t1 = time.time()
                ed.cmd(cmds.cCommand_TextDeleteSelection)
                t2 = time.time()
                t_del = t2 - t1
                # Hang1 + Hang2, right after the delete
                hd1, hd2 = self._hang(ed)
                # ---- read-only checks (after that op's timing) ----
                self.check('text after delete (empty doc)',
                           ed.get_text_all(), '')
                self.check('line_count after delete', ed.get_line_count(),
                           1)
                cudatext.app_proc(cudatext.PROC_IDLE, True)
                t1 = time.time()
                ed.cmd(cmds.cCommand_Undo)
                t2 = time.time()
                t_undo = t2 - t1
                hu1, hu2 = self._hang(ed)
                self.check('text after undo (full doc back)',
                           ed.get_text_all(), corpus)
                self.check('line_count after undo (fake line incl.)',
                           ed.get_line_count(), nlines + 1)
                self.check('carets after undo valid (select-all overshoot)',
                           ed.get_carets() in (
                               [(0, nlines + 1, 0, 0)],
                               [(0, nlines, -1, -1)]),
                           True)
                cudatext.app_proc(cudatext.PROC_IDLE, True)
                t1 = time.time()
                ed.cmd(cmds.cCommand_Redo)
                t2 = time.time()
                t_redo = t2 - t1
                hr1, hr2 = self._hang(ed)
                self.check('text after redo (delete result back)',
                           ed.get_text_all(), '')
                self.check('line_count after redo', ed.get_line_count(), 1)
                del corpus

                # ---- per-command profiles + thresholds (each command only)
                perf_fails = []
                perf_warns = []
                profiles = []
                profiles.append(self._judge_cmd(
                    'Delete', t_del, hd1, hd2, TH_DEL, TH_DEL_HANG,
                    perf_fails, perf_warns,
                    base_total=base_for(BASE_DEL, nlines) +
                               base_for(BASE_DEL_HANG, nlines)))
                profiles.append(self._judge_cmd(
                    'Undo', t_undo, hu1, hu2, TH_UNDO, TH_UNDO_HANG,
                    perf_fails, perf_warns,
                    base_total=base_for(BASE_UNDO, nlines) +
                               base_for(BASE_UNDO_HANG, nlines)))
                profiles.append(self._judge_cmd(
                    'Redo', t_redo, hr1, hr2, TH_REDO, TH_REDO_HANG,
                    perf_fails, perf_warns,
                    base_total=base_for(BASE_REDO, nlines) +
                               base_for(BASE_REDO_HANG, nlines)))
                t_hang = (hd1 + hd2) + (hu1 + hu2) + (hr1 + hr2)

                text_bad = self.cur['bad'] > 0
                status = ('FAIL' if (perf_fails or text_bad)
                          else ('WARN' if perf_warns else 'PASS'))
                self.perf.append({
                    'id': 'MP3', 'wrap': self.wrap, 'lines': nlines,
                    'del': t_del, 'undo': t_undo, 'redo': t_redo,
                    'hang': t_hang,
                    'profiles': profiles,
                    'status': status, 'note': '; '.join(perf_fails +
                                                        perf_warns),
                })
                if perf_fails:
                    self.cur['bad'] += 1
                    if self.cur['status'] != 'ERR':
                        self.cur['status'] = 'FAIL'
                    self.out('    FAIL  perf: %s' % '; '.join(perf_fails))
                elif perf_warns:
                    if self.cur['note']:
                        self.cur['note'] += '; '
                    self.cur['note'] = (self.cur['note'] + '; '.join(
                        perf_warns))[:200]
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
            finally:
                self._close_tab(ed)
            self.done()

    def test_MP4(self, nlines=300000, ndel=None):
        '''Manual test4: load corpus via replace_lines (same setup as
        the manual console and as MP1/MP3), then delete the FIRST ndel
        lines, undo, redo - word wrap off and on. ndel=None auto-picks
        the corpus's own delete count - the two manual benchmarks'
        exact values: 200,000 for the 300k corpus, 600,000 for the 1M
        corpus (other sizes: 2/3 of the lines). The manual test:
            file_open(""); app_proc(PROC_IDLE, True)
            ed.set_prop(PROP_WRAP, 1)                       # untimed
            lines = open(fpath, "r").readlines()            # untimed
            ed.replace_lines(0, ed.get_line_count()-1, lines)  # untimed
            del lines
            ed.set_caret(0, ndel, 0, 0)
            t1; ed.cmd(cCommand_TextDeleteSelection); t2; Hang1; Hang2
            t1; ed.cmd(cCommand_Undo);               t2; Hang1; Hang2
            t1; ed.cmd(cCommand_Redo);               t2; Hang1; Hang2
        (0,0)-(0,ndel) selects exactly the first ndel lines with their
        newlines; the document keeps lines ndel.. plus the fake last
        line. Document setup matches MP1/MP3: empty tab + replace_lines
        of the corpus file's readlines() (NOT file_open of the corpus).'''
        # ---- thresholds (warn, fail) per profiled quantity ----
        # th(baseline) = (2*base + 0.25, 3*base + 1.0): WARN above 2x /
        # FAIL above 3x with jitter slack (module docstring, THRESHOLDS).
        # Measured healthy baselines (manual benchmark, corrected 2026-09-11;
        # partial deletes are 200k-of-300k and 600k-of-1M):
        #   corpus   Delete   Del hang   Undo     Undo hang   Redo     Redo hang
        #   300k     0.6060   0.1040     0.5390   0.1070      0.6260   0.1030
        #   1M       1.7321   3.0762     5.5503   7.7294      1.6831   3.1252
        # Delete hang is now measured (Hang1+Hang2 after TextDeleteSelection).
        BASE_DEL       = {300000: 0.6060, 1000000: 1.7321}
        BASE_DEL_HANG  = {300000: 0.1040, 1000000: 3.0762}
        BASE_UNDO      = {300000: 0.5390, 1000000: 5.5503}
        BASE_UNDO_HANG = {300000: 0.1070, 1000000: 7.7294}
        BASE_REDO      = {300000: 0.6260, 1000000: 1.6831}
        BASE_REDO_HANG = {300000: 0.1030, 1000000: 3.1252}
        TH_DEL       = th(base_for(BASE_DEL, nlines))
        TH_DEL_HANG  = th(base_for(BASE_DEL_HANG, nlines))
        TH_UNDO      = th(base_for(BASE_UNDO, nlines))
        TH_UNDO_HANG = th(base_for(BASE_UNDO_HANG, nlines))
        TH_REDO      = th(base_for(BASE_REDO, nlines))
        TH_REDO_HANG = th(base_for(BASE_REDO_HANG, nlines))

        # the corpus's own delete count (the manual benchmarks' exact
        # values): 200k-of-300k, 600k-of-1M; other sizes: 2/3 of lines
        if ndel is None:
            ndel = {300000: 200000, 1000000: 600000}.get(
                nlines, nlines * 2 // 3)

        fpath, t_write = mp_corpus_file(nlines)
        for w in (0, 1):
            self.wrap = w
            if not self.begin('MP4', 'MP4 (manual test4): replace_lines load + '
                              'delete first %d of %d lines, undo, redo' % (
                                  ndel, nlines)):
                self.done()
                continue
            ed = None
            try:
                # ---- setup: empty tab + corpus via replace_lines (untimed)
                ed, res = self._open_tab(
                    "", tag='URTEST_LOAD', wrap=w,
                    title=self._tab_title('MP4', w))
                self.info('doc', '%s: %d lines (+fake), %d bytes%s' % (
                    fpath, nlines, os.path.getsize(fpath),
                    (', written in %.1fs' % t_write) if t_write is not None
                    else ' (already written this run)'))
                lines = open(fpath, 'r').readlines()
                corpus = ''.join(lines)
                after_del = ''.join(lines[ndel:])
                self.info('setup', 'replace_lines(0, get_line_count()-1, %d '
                          'readlines() items) [untimed]' % len(lines))
                ok = ed.replace_lines(0, ed.get_line_count() - 1, lines)
                del lines
                self.check('replace_lines returns True (setup)', ok, True)
                self.check('text after replace_lines setup',
                           ed.get_text_all(), corpus)
                self.check('line_count after replace_lines setup '
                           '(fake line incl.)',
                           ed.get_line_count(), nlines + 1)
                # ---- the manual test's timed commands ----
                ed.set_caret(0, ndel, 0, 0)
                pre = ed.get_carets()
                self.info('op', 'set_caret(0, %d, 0, 0) + '
                          'TextDeleteSelection + Undo + Redo' % ndel)
                # see MP1 for why this is necesary
                cudatext.app_proc(cudatext.PROC_IDLE, True)
                t1 = time.time()
                ed.cmd(cmds.cCommand_TextDeleteSelection)
                t2 = time.time()
                t_del = t2 - t1
                # Hang1 + Hang2, right after the delete
                hd1, hd2 = self._hang(ed)
                # ---- read-only checks (after that op's timing) ----
                self.check('text after delete', ed.get_text_all(),
                           after_del)
                self.check('line_count after delete (fake line incl.)',
                           ed.get_line_count(), nlines - ndel + 1)
                cudatext.app_proc(cudatext.PROC_IDLE, True)
                t1 = time.time()
                ed.cmd(cmds.cCommand_Undo)
                t2 = time.time()
                t_undo = t2 - t1
                hu1, hu2 = self._hang(ed)
                self.check('text after undo (full doc back)',
                           ed.get_text_all(), corpus)
                self.check('line_count after undo (fake line incl.)',
                           ed.get_line_count(), nlines + 1)
                self.check('selection restored after undo',
                           ed.get_carets(), pre)
                cudatext.app_proc(cudatext.PROC_IDLE, True)
                t1 = time.time()
                ed.cmd(cmds.cCommand_Redo)
                t2 = time.time()
                t_redo = t2 - t1
                hr1, hr2 = self._hang(ed)
                self.check('text after redo (delete result back)',
                           ed.get_text_all(), after_del)
                del corpus, after_del

                # ---- per-command profiles + thresholds (each command only)
                perf_fails = []
                perf_warns = []
                profiles = []
                profiles.append(self._judge_cmd(
                    'Delete', t_del, hd1, hd2, TH_DEL, TH_DEL_HANG,
                    perf_fails, perf_warns,
                    base_total=base_for(BASE_DEL, nlines) +
                               base_for(BASE_DEL_HANG, nlines)))
                profiles.append(self._judge_cmd(
                    'Undo', t_undo, hu1, hu2, TH_UNDO, TH_UNDO_HANG,
                    perf_fails, perf_warns,
                    base_total=base_for(BASE_UNDO, nlines) +
                               base_for(BASE_UNDO_HANG, nlines)))
                profiles.append(self._judge_cmd(
                    'Redo', t_redo, hr1, hr2, TH_REDO, TH_REDO_HANG,
                    perf_fails, perf_warns,
                    base_total=base_for(BASE_REDO, nlines) +
                               base_for(BASE_REDO_HANG, nlines)))
                t_hang = (hd1 + hd2) + (hu1 + hu2) + (hr1 + hr2)

                text_bad = self.cur['bad'] > 0
                status = ('FAIL' if (perf_fails or text_bad)
                          else ('WARN' if perf_warns else 'PASS'))
                self.perf.append({
                    'id': 'MP4', 'wrap': self.wrap, 'lines': nlines,
                    'del': t_del, 'undo': t_undo, 'redo': t_redo,
                    'hang': t_hang,
                    'profiles': profiles,
                    'status': status, 'note': '; '.join(perf_fails +
                                                        perf_warns),
                })
                if perf_fails:
                    self.cur['bad'] += 1
                    if self.cur['status'] != 'ERR':
                        self.cur['status'] = 'FAIL'
                    self.out('    FAIL  perf: %s' % '; '.join(perf_fails))
                elif perf_warns:
                    if self.cur['note']:
                        self.cur['note'] += '; '
                    self.cur['note'] = (self.cur['note'] + '; '.join(
                        perf_warns))[:200]
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
            finally:
                self._close_tab(ed)
            self.done()

    def test_MP5(self, nlines=1000000):
        '''Manual test5: file_open of the corpus file, word wrap off
        and on. The manual test timed the cold open of the big text
        file. A freshly opened tab inherits the app's GLOBAL wrap
        setting. Before each row this test sets that global setting
        via _disable_wrap_opts (wrap off) or _enable_wrap_opts
        (wrap on), then does a true cold open:
            # untimed: _disable_wrap_opts() or _enable_wrap_opts()
            t1; cudatext.file_open(fpath); t2          # open time
            t1; app_proc(PROC_IDLE, True); t2           # Hang1
            t1; ed.action(EDACTION_UPDATE, 1); t2       # Hang2
        Only file_open is profiled (Hang1/Hang2 included). Setting
        PROP_WRAP on the tab is not timed - wrap is established by
        the global option before the open. Each row starts with the
        corpus tab closed (the previous row closes it), so file_open
        is a true cold open.'''
        # ---- thresholds (warn, fail) per profiled quantity ----
        # th(baseline) = (2*base + 0.25, 3*base + 1.0): WARN above 2x /
        # FAIL above 3x with jitter slack (module docstring, THRESHOLDS).
        # MP5 is the SAME 1M-line file_open benchmark in both suites
        # (default nlines=1000000); the 300k row below only serves
        # console calls of test_MP5(300000).
        # Measured healthy baselines (manual benchmark):
        #   corpus   file_open   Hang1 + Hang2
        #   300k     2.9142s     0.0430 + 0.0190 = 0.0620s
        #   1M       8.8475s     0.0550 + 0.0190 = 0.0740s
        BASE_OPEN      = {300000: 2.9142, 1000000: 8.8475}
        BASE_OPEN_HANG = {300000: 0.0620, 1000000: 0.0740}
        TH_OPEN      = th(base_for(BASE_OPEN, nlines))       # 300k (6.08, 9.74), 1M (17.95, 27.54)
        TH_OPEN_HANG = th(base_for(BASE_OPEN_HANG, nlines))  # 300k (0.37, 1.19), 1M (0.40, 1.22)

        fpath, t_write = mp_corpus_file(nlines)
        items = open(fpath, 'r').readlines()
        sample = tuple(s[:-1] if s.endswith('\n') else s
                       for s in (items[0], items[nlines // 2],
                                 items[nlines - 1]))
        del items
        for w in (0, 1):
            self.wrap = w
            if not self.begin('MP5', 'MP5 (manual test5): file_open of %d '
                              'corpus-file lines (%s)' % (
                                  nlines, 'wrap off' if w == 0
                                  else 'wrap on')):
                self.done()
                continue
            ed = None
            try:
                self.info('doc', '%s: %d lines (+fake), %d bytes%s' % (
                    fpath, nlines, os.path.getsize(fpath),
                    (', written in %.1fs' % t_write) if t_write is not None
                    else ' (already written this run)'))
                # set global wrap opts BEFORE the timed open so the
                # new tab inherits the wanted setting (untimed)
                if w == 0:
                    self._disable_wrap_opts()
                else:
                    self._enable_wrap_opts()

                # these three lines are important otherwise
                # _enable_wrap_opts has no effect on the first run
                # of this test!
                cudatext.app_proc(cudatext.PROC_IDLE, True)
                edZ = self._ed_focused()
                edZ.action(cudatext.EDACTION_UPDATE, 1)

                # ---- the manual test's own commands, nothing else ----
                t1 = time.time()
                res = cudatext.file_open(fpath)
                t2 = time.time()
                t_open = t2 - t1
                ed = self._ed_focused()

                # Hang1 + Hang2 IMMEDIATELY after open (before any other work)
                ho1, ho2 = self._hang(ed)

                # configure after the timed open + hang (shared helper)
                self._configure_suite_tab(ed, tag='URTEST_LOAD')
                ed.set_prop(cudatext.PROP_TAB_TITLE,
                            self._tab_title('MP5', w))
                w_tab = ed.get_prop(cudatext.PROP_WRAP)
                if w == 0 and w_tab:
                    self.info('note', 'tab opened with wrap ON after '
                              '_disable_wrap_opts (unexpected)')
                    print('NOTE:tab opened with wrap ON after '
                              '_disable_wrap_opts (unexpected)')
                elif w == 1 and not w_tab:
                    self.info('note', 'tab opened with wrap OFF after '
                              '_enable_wrap_opts (unexpected)')
                    print('NOTE:tab opened with wrap OFF after '
                              '_enable_wrap_opts (unexpected)')
                # ---- read-only verification, after the timing ----
                self.check('file_open returns True', res, True)
                self.check('line count (fake line incl.)',
                           ed.get_line_count(), nlines + 1)
                self.check('first line', ed.get_text_line(0), sample[0])
                self.check('middle line', ed.get_text_line(nlines // 2),
                           sample[1])
                self.check('last line', ed.get_text_line(nlines - 1),
                           sample[2])

                # ---- per-command profile + thresholds (file_open only)
                perf_fails = []
                perf_warns = []
                profiles = [
                    self._judge_cmd(
                        'file_open', t_open, ho1, ho2, TH_OPEN, TH_OPEN_HANG,
                        perf_fails, perf_warns,
                        base_total=base_for(BASE_OPEN, nlines) +
                                   base_for(BASE_OPEN_HANG, nlines))
                ]
                t_hang = ho1 + ho2

                text_bad = self.cur['bad'] > 0
                status = ('FAIL' if (perf_fails or text_bad)
                          else ('WARN' if perf_warns else 'PASS'))
                self.perf.append({
                    'id': 'MP5', 'wrap': self.wrap, 'lines': nlines,
                    'del': t_open, 'undo': 0.0, 'redo': 0.0,
                    'hang': t_hang,
                    'profiles': profiles,
                    'status': status, 'note': '; '.join(
                        perf_fails + perf_warns),
                })
                if perf_fails:
                    self.cur['bad'] += 1
                    if self.cur['status'] != 'ERR':
                        self.cur['status'] = 'FAIL'
                    self.out('    FAIL  perf: %s' % '; '.join(perf_fails))
                elif perf_warns:
                    if self.cur['note']:
                        self.cur['note'] += '; '
                    self.cur['note'] = (self.cur['note'] + '; '.join(
                        perf_warns))[:200]
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
            finally:
                self._close_tab(ed)
            self.done()


# ----------------------------------------------------------------------------
# test registries: one catalog per corpus. MP1..MP4 are bound to the
# corpus's size (MP4 auto-picks its delete count: 200k-of-300k /
# 600k-of-1M); MP5 is the SAME 1M-line file_open benchmark in both
# suites. Every test stays standalone - the registries only reference
# them with the right corpus arguments.
# ----------------------------------------------------------------------------

# 300k corpus: 300,000 lines (~150 MB), cuda_undo_test_rand_300K.txt
TESTS_300K = [
    # exact replicas of the manual console benchmarks: the doc is
    # the corpus FILE opened via file_open, the timed commands are
    # the manual tests' own commands (see the MP section docstrings)
    ('MP1', 'manual test1: replace_lines of all 300k lines, '
            'readlines() input (file-opened doc, wrap off+on)',
     partial(Runner.test_MP1, nlines=300000)),
    ('MP2', 'manual test2: set_text_all of open().read() text, '
            '300k-line corpus (file-opened doc, wrap off+on)',
     partial(Runner.test_MP2, nlines=300000)),
    ('MP3', 'manual test3: select-all delete, undo, redo of 300k '
            'lines (file-opened doc, wrap off+on)',
     partial(Runner.test_MP3, nlines=300000)),
    ('MP4', 'manual test4: replace_lines load + delete first 200k '
            'of 300k lines, undo, redo (wrap off+on)',
     partial(Runner.test_MP4, nlines=300000)),
    ('MP5', 'manual test5: file_open of the 1M-line corpus file '
            '(same test in both suites; wrap off then wrap on, '
            'global wrap opts)',
     partial(Runner.test_MP5, nlines=1000000)),
]

# 1M corpus: 1,000,000 lines (~500 MB), cuda_undo_test_rand_1M.txt
TESTS_1M = [
    ('MP1', 'manual test1: replace_lines of all 1M lines, '
            'readlines() input (file-opened doc, wrap off+on)',
     partial(Runner.test_MP1, nlines=1000000)),
    ('MP2', 'manual test2: set_text_all of open().read() text, '
            '1M-line corpus (file-opened doc, wrap off+on)',
     partial(Runner.test_MP2, nlines=1000000)),
    ('MP3', 'manual test3: select-all delete, undo, redo of 1M '
            'lines (file-opened doc, wrap off+on)',
     partial(Runner.test_MP3, nlines=1000000)),
    ('MP4', 'manual test4: replace_lines load + delete first 600k '
            'of 1M lines, undo, redo (wrap off+on)',
     partial(Runner.test_MP4, nlines=1000000)),
    ('MP5', 'manual test5: file_open of the 1M-line corpus file '
            '(same test in both suites; wrap off then wrap on, '
            'global wrap opts)',
     partial(Runner.test_MP5, nlines=1000000)),
]

# ----------------------------------------------------------------------------
# plugin entry points
# ----------------------------------------------------------------------------

class Command:

    def run_all_300k(self):
        """Run ALL performance tests on the 300k-line corpus:
        MP1..MP4 at 300k lines (MP4 deletes the first 200k) plus
        MP5, the shared 1M-line file_open benchmark."""
        Runner().run('300k')

    def run_all_1M(self):
        """Run ALL performance tests on the 1M-line corpus:
        MP1..MP4 at 1M lines (MP4 deletes the first 600k) plus
        MP5, the shared 1M-line file_open benchmark."""
        Runner().run('1M')

    def run_single_300k(self):
        """Show the 300k corpus's list of all perf tests and run
        only the one chosen in the dialog."""
        self._run_single('300k')

    def run_single_1M(self):
        """Show the 1M corpus's list of all perf tests and run
        only the one chosen in the dialog."""
        self._run_single('1M')

    def _run_single(self, corpus):
        """Shared body of run_single_300k / run_single_1M: show the
        corpus's test catalog in a dialog and run the chosen test
        alone. dlg_menu returns the 0-based index of the chosen
        item, or None when cancelled."""
        r = Runner()
        cat = r.test_catalog(corpus)
        # 'id\tdescription': the part after the tab shows below the id
        items = ['%s\t%s' % (tid, label) for tid, label in cat]
        res = cudatext.dlg_menu(cudatext.DMENU_LIST_ALT, items, focused=0,
                                caption='Performance tests (%s corpus): '
                                        'select a test to run it alone'
                                        % corpus)
        if res is None:
            return
        r.run_single(cat[res][0], corpus)

    def about(self):
        """Open this module's docstring (help) in a new untitled tab."""
        if not cudatext.file_open(''):
            cudatext.ed.cmd(cmds.cmd_FileNew)
        h = cudatext.ed.get_prop(cudatext.PROP_HANDLE_SELF)
        ed = cudatext.Editor(h) if h else cudatext.ed
        ed.set_text_all(__doc__ or '')
        ed.set_prop(cudatext.PROP_TAB_TITLE, 'Perf tests: help')
        ed.set_prop(cudatext.PROP_MODIFIED, False)
        ed.set_caret(0, 0)
