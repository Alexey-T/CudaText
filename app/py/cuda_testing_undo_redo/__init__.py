"""
cuda_testing_undo_redo - extensive undo/redo regression test suite for CudaText.

PURPOSE
  Guards the undo/redo machinery against behavior changes and hidden bugs
  while the engine internals are being modified. Everything is tested
  through the public plugin API (black-box), with expected results computed
  by an independent pure-Python text model.

DESIGN: every test is standalone
  Each test (test_T01..test_T41,
  test_MP1..test_MP4 and test_MP6)
  is a single self-contained function: document setup, the operation,
  every check, the exact-count undo/redo steps, and (for perf tests)
  the threshold
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
  With grouping off, the full performance suite (300k-line docs,
  MP1-MP4, MP6) can consume >6 GB RAM.  Grouping must stay ON (True) globally; only
  individual tests that need exact per-op undo entries may temporarily
  set it to False and must restore True afterwards.

  Tests that need exact "1 API call = 1 undo entry" behaviour should
  disable grouping only for their own body:
      ed.set_prop(PROP_UNDO_GROUPED, False)
      try:
          ... ops and exact-count undo/redo ...
      finally:
          ed.set_prop(PROP_UNDO_GROUPED, True)

  Currently forced off (then restored) in: T06, T21, T22, T25, T35
  (and temporarily in some load-test undo/redo round-trips).

  Note: a prior bug in cCommand_TextDeleteSelection made redo of a
  selection-to-EOF incorrect when PROP_UNDO_GROUPED was False; that
  was fixed upstream (CudaText issue #6446).

_________________________________
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
  Undo, Redo, file_open, set wrap on) is followed by exactly these
  two calls; the console shows "op X (+Hang1 Y +Hang2 Z)", and the
  summary table's "hang" column is the sum of all Hang1+Hang2 of
  that perf record. Judged against each test's local hang thresholds (warn/fail), so a
  regression like the removed wrap-items cache (commit3 vs commit4:
  +12s hang on set_text_all with wrap on) trips a WARN/FAIL, not
  just a note.

_________________________________

COMMANDS
  Menu: Plugins / Testing / Testing of Undo/Redo /
    - Run full suite (incl. 300k/1M-line MP* perf; ~1-2 GB RAM, several minutes)
    - Run quick suite (core + load tests only; no heavy perf)
    - Run single test (filterable list of T01..T41, L1..L7,
      MP1..MP4, MP6)
    - Help: what is tested / how to read results (opens this docstring
      in a new tab)

  Each test opens its own temp tab (tag URTEST_TAB / URTEST_TAB2 /
  URTEST_LOAD) and closes it when done; your tabs are not modified.
  Do not touch the editor while the suite runs. When finished, a
  summary dialog is shown and a new tab opens with the full console log.

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
  - performance regression on large documents via the exact manual
    console benchmarks (MP1..MP4, MP6). Times and the deferred "hang"
    (Hang1 = app_proc(PROC_IDLE), Hang2 = ed.action(EDACTION_UPDATE, 1),
    see HANG TIME above) is measured and judged for every timed op
  - MP1..MP4 and MP6: the 5 manual console benchmarks replicated
    EXACTLY, command for command (scaled to the suite's 300k lines;
    MP4 200k-of-300k; call test_MP1(1000000) etc. from
    the console for the full 1M scale). The document is the corpus
    FILE opened with file_open into its own tab - exactly the manual
    session's state; no set_text_all, no suite-tab commands anywhere
    in the setup. The timed part is ONLY the manual test's own
    commands with nothing run in between, Hang1/Hang2 are the manual
    test's two timed calls, and every check is read-only and runs
    after the timing. The corpus file is written once per run with
    the manual benchmark's exact seeded generator (SEED 20260904,
    one bytes(rng.getrandbits(8) for _ in range(rng.randint(245,
    255))).hex() line + newline per line), so its content is
    byte-identical to the benchmark's own corpus file. The replicas:
    MP1 replace_lines(0, get_line_count()-1, open().readlines()),
    MP2 set_text_all(open().read()), MP3 replace_lines load then
    set_caret(0, get_line_count(), 0, 0) + TextDeleteSelection +
    Undo + Redo,
    MP4 replace_lines load then set_caret(0, ndel, 0, 0) +
    TextDeleteSelection + Undo + Redo (200k-of-300k),
    MP6 file_open + set_prop(PROP_WRAP, 1)
  - unicode replace_lines (T36/T37): 4000 EQUAL CJK lines and 4000
    DISTINCT CJK lines - the word-wrap calculation has separate code
    paths for pure-ASCII lines and for unicode (CJK) lines; text,
    line count, one-step undo and redo must stay exact with wrap on
    and off (suite runs both)
  - bulk undo/redo regression (T38..T41): the Sep 2, 2026 ATSynEdit
    bulk-run optimization (undo/redo runs of >= 25 undo items) is
    guarded by the exact fatal-bug repro (fresh tab, 24 vs 25 lines
    = the bulk-run threshold), a 23..27 boundary sweep, the
    aggravated 80-line-document variant, and CJK/Cyrillic documents
    with full, partial and middle replace ranges - after ONE undo
    + ONE redo the text must be exact, never an empty document
  - file loading (L1..L7): files written by the suite itself, in UTF-8
    and UTF-16 LE/BE and UTF-32 LE/BE (all with BOM), and with mixed
    per-line EOLs (LF/CRLF/CR), are opened via file_open(); detected
    encoding name (PROP_ENC), line count, full text and sampled CJK
    lines are checked; every test then also round-trips a small
    unicode insert and a small delete through undo/redo (a freshly
    opened tab has an EMPTY undo stack: one edit = one undo step,
    which must land exactly on the loaded content - undo data
    corrupted by an encoding round-trip shows up as mojibake or
    lost/duplicated lines here); L7 measures file_open() time plus
    the undo/redo of one small edit on the big loaded doc and
    records all of it in the perf table (see MP6 for the plain-text
    big-file open + hang)


OUTPUT
  All results go to the Console panel. Per test: check lines
  (ok / FAIL with got/expected / ERR), info lines (undo step
  counts, timings, Hang1/Hang2 per op). A SUMMARY is
  printed at the end: totals, list of failed tests, performance
  table (with a "hang" column), overall verdict.

HOW TO READ THE OUTPUT
  [Txx] test name (wrap=off/on)
    ok    <check>          check passed
    FAIL  <check>          mismatch; got/expected previews follow
    ERR   exception        the test crashed the API (bug or API change)
    info  ...              undo/redo step counts, timings, Hang1/Hang2
    WARN  perf: ...        a timing exceeded the warn threshold
  => PASS/FAIL/ERR/SKIP  (n ok, m failed)
  SUMMARY: totals, failed list, performance table (with hang column),
  overall verdict. A dialog repeats the short summary; a new tab holds
  the full log.

NOTES
  * Do not touch the editor while the suite runs. Any click or keypress
    you make during a perf test is processed by the PROC_IDLE pass and
    pollutes the hang numbers.
  * Full mode needs ~2 GB RAM (300k-line docs) and can take several
    minutes (much longer if undo is still slow/not patched - that is the point).
    Each timed op additionally runs one app_proc(PROC_IDLE) pass and
    one ed.action(EDACTION_UPDATE) pass (the hang measurement): on a
    healthy editor both return in milliseconds, on a regressed one
    they take seconds - that IS the hang being measured.
    MP tests write a ~150 MB corpus file into the system temp dir
    (once per run, shared by MP1..MP6).
  * Each test opens its own tab (tag URTEST_TAB / URTEST_TAB2 / URTEST_LOAD)
    and closes it when done; your tabs are not modified.
  * While the suite runs, user.json's "wrap_enabled_max_lines" is
    temporarily set to 1100000 (CudaText refuses to enable word wrap
    above that line count, and the suite wraps 300k/1M-line docs) and
    "wrap_mode" is temporarily set to 1 (word wrap on; tabs opened by
    the suite inherit it as the global wrap setting). Three helpers:
    Runner._enable_wrap_opts (high max + wrap on; from _setup and
    from MP6 when wrap is on), Runner._disable_wrap_opts (high max +
    wrap off; from MP6 when wrap is off), and
    Runner._restore_wrap_opts (user's original values back; from
    _cleanup, also on FATAL/error paths). All writes go through
    cudax_lib's get_opt/set_opt, which only change the file on disk:
    right after each write the helpers open user.json, save it with
    the editor's save command and close it again, because the running
    CudaText re-reads its options only when user.json is saved in the
    editor (or on restart). If the process is killed mid-run, restore
    the values by hand; the old ones are printed to the console when
    enable first runs.
  * PROP_UNDO_GROUPED stays True (the CudaText default) for the suite.
    Forcing it False globally exhausts RAM on the 300k-line perf tests
    (>6 GB).  Only tests that need exact per-op undo entries may
    temporarily set it False and must restore True.  See UNDO GROUPING.
  * Test data is seeded (SEED 20260904): identical documents on
    every run.
  * Caret positions are asserted only where the contract is solid
    (undo restores pre-op caret/selection). Redo carets are
    informational.
  * File-loading and MP tests keep corpus files under the system
    temp dir (cuda_testing_undo_redo); the suite removes that folder
    on cleanup. Delete it by hand to reclaim space if a run was
    killed mid-way.

"""

import os
import sys
import time
import random
import traceback
import tempfile

import cudatext
import cudatext_cmd as cmds
import cudax_lib

SEED = 20260904

# temp dir where the file-loading tests (L1..L7) write their corpora
LOAD_DIR = os.path.join(tempfile.gettempdir(), 'cuda_testing_undo_redo')

# user.json options patched for the duration of a run: CudaText
# refuses to enable word wrap on documents longer than
# "wrap_enabled_max_lines" lines. The suite enables wrap on 300k
# (MP1..MP4) and 1M-line (MP6) documents, so
# Runner._enable_wrap_opts bumps this limit to 1.1M lines and forces
# "wrap_mode" to 1 (word wrap on) so tabs the suite opens inherit
# wrap as the global setting. Runner._disable_wrap_opts keeps the
# high max but sets wrap_mode to 0 (MP6 wrap-off row).
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

_UNI_CACHE = {}

def make_uni_lines():
    """80 unicode-stress lines: CJK, Cyrillic, Latin, mixed CJK+Latin,
    tabs, empty, long - for the replace_lines + wrap-calc tests (the
    wrap calculation has separate paths for pure-ASCII lines and for
    unicode/CJK lines; both must give exact results)."""
    rng = random.Random(SEED + 7)
    lines = []
    for i in range(80):
        k = i % 8
        if k == 0:
            lines.append('')
        elif k == 1:
            lines.append('\u4e2d' * rng.randint(10, 60))
        elif k == 2:
            lines.append('unicode \u4e2d\u6587 line %d %s' % (
                i, '\u5b57' * rng.randint(10, 100)))
        elif k == 3:
            lines.append('\u4e2d\u82f1mixed \u4e2d\u82f1 line %d %s' % (
                i, 'x' * rng.randint(10, 100)))
        elif k == 4:
            lines.append('\u0441\u043b\u043e\u0432\u043e \u0422\u0435\u0441\u0442 %d %s' % (
                i, 'y' * rng.randint(10, 80)))
        elif k == 5:
            lines.append('\u00fcn\u00efc\u00f6d\u00e9 \u8a18\u53f7 \u30c6\u30b9\u30c8 line %d' % i)
        elif k == 6:
            lines.append('tab\t\u4e2d\u6587\tvalue\t%d' % i)
        else:
            lines.append('short %d' % i)
    return lines

def uni_lines():
    if 'uni80' not in _UNI_CACHE:
        _UNI_CACHE['uni80'] = make_uni_lines()
    return _UNI_CACHE['uni80']

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
        fpath = <LOAD_DIR>/cuda_undo_test_rand_<nlines>.txt
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
    fpath = os.path.join(LOAD_DIR, 'cuda_undo_test_rand_%d.txt' % nlines)
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

    def __init__(self, full):
        self.full = full
        self.TE = None          # current test editor (set per-test, not shared)
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

    def run(self):
        self.out('=' * 66)
        self.out(' CudaText Undo/Redo Regression Suite  (cuda_testing_undo_redo)')
        self.out(' mode=%s   seed=%d   %s' % (
            'full: incl. 300k/1M-line MP* perf tests' if self.full
            else 'quick: core + load tests only',
            SEED, time.strftime('%Y-%m-%d %H:%M:%S')))
        self.out(' NOTE: do not touch the editor while the suite is running.')
        if self.full:
            self.out(' NOTE: full mode needs ~1 GB RAM and several minutes;')
            self.out('       it takes much longer if undo/redo is still slow.')
        self.out('=' * 66)
        self._run_body(lambda: (self._core_suite(0), self._core_suite(1),
                                self._load_suite(), self._perf_suite()))

    def run_single(self, tid):
        """Run only one test, by id from test_catalog() ('T07', 'MP4', ...).
        Core tests run with word wrap off and on, like in the full suite;
        perf tests handle their wrap modes themselves."""
        self.out('=' * 66)
        self.out(' CudaText Undo/Redo Regression Suite  (cuda_testing_undo_redo)')
        self.out(' mode=single test %s   seed=%d   %s' % (
            tid, SEED, time.strftime('%Y-%m-%d %H:%M:%S')))
        self.out(' NOTE: do not touch the editor while the test is running.')
        if tid.startswith('P') or tid.startswith('M'):
            self.out(' NOTE: perf tests build big docs (up to 300k lines);')
            self.out('       they need RAM and can take a while.')
            if tid.startswith('M'):
                self.out(' NOTE: MP tests write the corpus file once per')
                self.out('       run (~16s for 300k lines) if needed.')
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
            
            # force wrap setting to take effect, enabling wrap for test MP6 does not work without this!
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
        Called from _setup and from MP6 when wrap is on."""
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
        Called from MP6 when wrap is off."""
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
        _UNI_CACHE.clear()
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
            self.out(' \n\nperformance results:')

            self.out('   (L7/MP6: "command" is the file_open time; L7 '
                     'undo/redo: one small edit undone/redone on the '
                     'loaded big doc; MP1: replace_lines; MP2: '
                     'set_text_all; MP6 wrap-on row: open + wrap-on)')
                     
            self.out('   ("hang" = sum of Hang1 (app_proc PROC_IDLE) + '
                     'Hang2 (ed.action EDACTION_UPDATE); - = not measured)')

            self.out('\n   %-6s %-4s %-8s %9s %9s %9s %8s  %-6s' % ('test', 'wrap', 'lines', 'command', 'undo', 'redo', 'hang', 'status'))
            
            for p in self.perf:
                hang = p.get('hang')
                hang_s = ('%7.2fs' % hang) if hang is not None else '       -'
                if p['id'].startswith('L'):
                    u_s = ('%8.2fs' % p['undo']) if p.get('undo') \
                        else '        -'
                    r_s = ('%8.2fs' % p['redo']) if p.get('redo') \
                        else '        -'
                    self.out('   %-6s %-4s %-8d %8.2fs %s %s  %s  %-6s' % (
                        p['id'], '-', p['lines'],
                        p['del'], u_s, r_s, hang_s, p['status']))
                else:
                    self.out('   %-6s %-4s %-8d %8.2fs %8.2fs %8.2fs  %s  %-6s' % (
                        p['id'], 'on' if p['wrap'] else 'off', p['lines'],
                        p['del'], p['undo'], p['redo'], hang_s, p['status']))
                if p['note']:
                    self.out('        %s' % p['note'][:200])
        perf_fail = any(p['status'] == 'FAIL' for p in self.perf)
        if st['FAIL'] == 0 and st['ERR'] == 0 and not perf_fail:
            overall = 'ALL TESTS PASSED'
        else:
            overall = 'FAILURES DETECTED - see details above'
        self.out(' overall: %s' % overall)
        self.out('=' * 66)
        cudatext.msg_status('Undo/Redo tests: %s (see console)' % overall)

        # ---- end-of-run UI: summary dialog + log tab ----
        lines = [
            'CudaText Undo/Redo tests',
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
        log_ed.set_prop(cudatext.PROP_TAB_TITLE, 'Undo/Redo test log')
        log_ed.set_prop(cudatext.PROP_MODIFIED, False)
        log_ed.set_caret(0, 0)

    # ---- suite driving (uses the TESTS registry at module level) ----

    def _core_suite(self, w):
        """Run all core (T*) tests once for the given wrap mode.
        Each test opens and closes its own tab via t()."""
        self.wrap = w
        self.out()
        self.out('------ core tests, word wrap = %s ------' % ('on' if w else 'off'))
        for tid, name, fn in TESTS:
            if not tid.startswith('T'):
                continue
            self.t(tid, name, lambda f=fn: f(self))

    def _perf_suite(self):
        self.out()
        self.out('------ performance & mass-op tests ------')
        # All remaining performance tests are the MP* exact manual
        # benchmark replicas. They are heavy (300k / 1M lines) and run
        # only in full mode. Quick mode skips this section entirely
        # (L7 still runs a smaller corpus via _load_suite).
        if not self.full:
            self.out('    (skipped in quick mode — run full suite for MP* perf tests)')
            return
        for tid, _name, fn in TESTS:
            if tid.startswith('M'):
                fn(self)

    def _load_suite(self):
        self.out()
        self.out('------ file-loading tests (UTF-8/16/32, LE/BE, mixed EOLs) ------')
        for tid, name, fn in TESTS:
            if not tid.startswith('L'):
                continue
            if tid == 'L7':
                # perf-style test: manages its own records; smaller corpus
                # in quick mode
                fn(self, 100000 if self.full else 30000)
                continue
            self.t(tid, name, lambda f=fn: f(self), own_tab=False)

    def _single_test(self, tid):
        """Run exactly one test from the catalog. A core test runs with
        word wrap off and then on (same contract as in the whole suite);
        a perf test manages its wrap modes itself."""
        for t_id, name, fn in TESTS:
            if t_id != tid:
                continue
            if tid.startswith('T'):
                for w in (0, 1):
                    self.wrap = w
                    self.out()
                    self.out('------ single test %s, word wrap = %s ------' % (
                        tid, 'on' if w else 'off'))
                    self.t(tid, name, lambda f=fn: f(self))
                return
            if tid.startswith('L'):
                # load test: independent of wrap; L7 manages its own records
                self.out()
                self.out('------ single load test %s ------' % tid)
                if tid == 'L7':
                    fn(self)
                else:
                    self.t(tid, name, lambda f=fn: f(self), own_tab=False)
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
    # STANDALONE CORE TESTS T01..T37
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
        ret = self.TE.insert(x, y, s)
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
        ret = self.TE.insert(x, y, s)
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
        ret = self.TE.insert(x, y, s)
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
        ret = self.TE.insert(x, y, s)
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
        ret = self.TE.insert(x, y, s)
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

    def test_T06(self):
        """Typing simulation: 12 adjacent single-char inserts.
        PROP_UNDO_GROUPED is forced False so each char is its own undo
        entry (un == 12).  Grouping is restored in finally."""
        self.TE.set_prop(cudatext.PROP_UNDO_GROUPED, False)
        try:
            L = make_small_lines()
            self.TE.set_text_all(m_join(L))
            self.TE.set_caret(0, 0)
            base = m_join(L)
            x, y = 2, 1
            exp = list(L)
            self.TE.set_caret(x, y)
            self.info('op', '12 adjacent single-char inserts (typing simulation)')
            for i in range(12):
                ch = chr(ord('a') + i)
                self.TE.insert(x + i, y, ch)
                exp = m_insert(exp, x + i, y, ch)
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
        finally:
            self.TE.set_prop(cudatext.PROP_UNDO_GROUPED, True)

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
        # typing simulation: goes through the command processor,
        # deletes the selection if any, groups undo like typing
        self.TE.cmd(cmds.cCommand_TextInsert, s)
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
        self.TE.cmd(cmds.cCommand_TextInsert, 'XYZ')
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
        self.TE.cmd(cmds.cCommand_KeyBackspace)
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
        self.TE.cmd(cmds.cCommand_KeyBackspace)
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
        self.TE.cmd(cmds.cCommand_KeyDelete)
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
        self.TE.cmd(cmds.cCommand_KeyDelete)
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
        self.TE.cmd(cmds.cCommand_TextDeleteSelection)
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
        self.TE.cmd(cmds.cCommand_TextDeleteSelection)
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
        self.TE.cmd(cmds.cCommand_TextDeleteSelection)
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
        self.TE.cmd(cmds.cCommand_TextDeleteSelection)
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
        self.TE.cmd(cmds.cCommand_TextDeleteSelection)
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
        self.TE.delete(x, y, x2, y2)
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
        self.TE.set_prop(cudatext.PROP_UNDO_GROUPED, False)
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
            self.TE.set_prop(cudatext.PROP_UNDO_GROUPED, True)
        
    def test_T22(self):
        """Tab char + EOL fidelity (raw snapshot compare): undo of a tab
        insert must restore EXACT raw text; then the same while the doc's
        EOL kind is toggled (PROP_NEWLINE).
        PROP_UNDO_GROUPED is forced False so the tab-insert redo and the
        later single-char insert stay as separate undo entries (with
        grouping on they can coalesce on a fresh fast tab, and one Undo
        would drop both the 'e' and the tab)."""
        self.TE.set_prop(cudatext.PROP_UNDO_GROUPED, False)
        e0 = None
        try:
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
            # line-ending kind is the str property PROP_NEWLINE ("lf"/"crlf"/"cr").
            # Re-snapshot AFTER the toggle so the undo baseline matches the
            # post-toggle document (toggle can change final-EOL by 1 char).
            e0 = self.TE.get_prop(cudatext.PROP_NEWLINE)
            self.TE.set_prop(cudatext.PROP_NEWLINE, 'crlf' if e0 != 'crlf' else 'lf')
            e1 = self.TE.get_prop(cudatext.PROP_NEWLINE)
            self.info('PROP_NEWLINE toggled', '%r -> %r' % (e0, e1))
            snap_eol = self.TE.get_text_all()
            if N(snap_eol) != N(snap):
                self.info('EOL toggle changed raw text length',
                          '%d -> %d (undo baseline refreshed)' % (
                              len(snap), len(snap_eol)))
            self.TE.set_caret(2, 2)
            self.TE.insert(2, 2, 'e')
            snap2 = self.TE.get_text_all()
            # exactly 1 edit under the toggled EOL: 1 undo, 1 redo
            self.TE.cmd(cmds.cCommand_Undo)
            self.check('raw text after undo with toggled EOL',
                       self.TE.get_text_all(), snap_eol)
            self.TE.cmd(cmds.cCommand_Redo)
            self.check('raw text after redo with toggled EOL',
                       self.TE.get_text_all(), snap2)
        finally:
            if e0 is not None:
                self.TE.set_prop(cudatext.PROP_NEWLINE, e0)
            self.TE.set_prop(cudatext.PROP_UNDO_GROUPED, True)

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
        self.TE.set_prop(cudatext.PROP_UNDO_GROUPED, False)
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
            self.TE.set_prop(cudatext.PROP_UNDO_GROUPED, True)

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
            self.TE.set_prop(cudatext.PROP_MODIFIED, False)
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
        self.TE.cmd(cmds.cCommand_Undo)
        self.check('undo with live selection: text==base',
                   self.TE.get_text_all(), base)
        # live selection again, then redo
        self.TE.set_caret(2, 50, 9, 50)
        self.TE.cmd(cmds.cCommand_Redo)
        self.check('redo with live selection: text==base+PRE',
                   self.TE.get_text_all(), after)

    def test_T28(self):
        """Tab switch away and back around an undo: switching focus must
        not corrupt the undo stack. Opens a temporary suite tab for the
        switch (does not use the user's original tab) and closes it."""
        L = make_small_lines()
        self.TE.set_text_all(m_join(L))
        self.TE.set_caret(0, 0)
        base = m_join(L)
        exp = m_insert(L, 3, 1, 'SWITCHED')
        self.TE.set_caret(3, 1)
        self.TE.insert(3, 1, 'SWITCHED')
        after = self.TE.get_text_all()
        # open a temporary tab to switch away to (never the original tab)
        ed2, _ = self._open_tab(
            '', tag='URTEST_TAB2', wrap=self.wrap,
            title=self._tab_title('T28', extra='switch'))
        try:
            ed2.focus()
            self.check('get_text_all on inactive test tab',
                       self.TE.get_text_all(), after)
        finally:
            self.TE.focus()
            self._close_tab(ed2)
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
            self.TE.set_prop(cudatext.PROP_WRAP, self.wrap)

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
        self.TE.cmd(cmds.cCommand_KeyEnter)
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
        self.TE.cmd(cmds.cCommand_KeyBackspace)
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
        self.TE.cmd(cmds.cCommand_TextDeleteSelection)
        self.check('text after 60k-char delete', self.TE.get_text_all(), m_join(exp))
        # exactly 1 delete command: 1 undo, 1 redo
        self.TE.cmd(cmds.cCommand_Undo)
        self.check('text after 1 undo', self.TE.get_text_all(), m_join(L2))
        self.TE.cmd(cmds.cCommand_Redo)
        self.check('text after 1 redo', self.TE.get_text_all(), m_join(exp))

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
        self.TE.insert(x, y, s)
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
        self.TE.set_prop(cudatext.PROP_UNDO_GROUPED, False)
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
            self.TE.set_prop(cudatext.PROP_UNDO_GROUPED, True)


    # ========================================================================
    # STANDALONE CORE TESTS T36..T37 (unicode replace_lines + wrap calc)
    # The word-wrap calculation has separate code paths for pure-ASCII
    # lines and for unicode (CJK) lines; these tests replace ALL lines
    # with big CJK content - equal lines (T36, like the artificial
    # benchmark docs) and distinct lines (T37, like real-life docs) -
    # and require text / line_count / one-step undo / one-step redo to
    # stay exact. The suite runs them with word wrap off and on.
    # ========================================================================

    def test_T36(self):
        """ed.replace_lines of ALL lines with 4000 EQUAL CJK lines of
        600 chars (like the 1M x 'x'*1000 benchmark, scaled): with word
        wrap on, the wrap calculation runs the full unicode path for
        every line; a caret jump to the last line then forces wrap-item
        indexing over the new lines. Text must stay exact at every
        step: after replace, after the caret jump, after 1 undo, after
        1 redo."""
        n = 4000
        L = uni_lines()
        self.TE.set_text_all(m_join(L))
        self.TE.set_caret(0, 0)
        base = m_join(L)
        new = ['\u4e2d' * 600] * n
        exp = m_join(new)
        self.info('op', 'replace_lines(0, %d, %d equal CJK lines of 600 chars)'
                  % (self.TE.get_line_count() - 1, n))
        ok = self.TE.replace_lines(0, self.TE.get_line_count() - 1, new)
        self.check('replace_lines returns True', ok, True)
        self.check('text after replace (equal CJK)', self.TE.get_text_all(), exp)
        self.check('line_count after replace', self.TE.get_line_count(), n)
        # caret to the last line: forces wrap-item indexing of new lines
        self.TE.set_caret(0, n - 1)
        self.check('text stable after caret to last line',
                   self.TE.get_text_all(), exp)
        # exactly ONE edit was made: 1 undo -> base, 1 redo -> replaced
        # (never drain blindly: set_text_all keeps one more entry below)
        self.TE.cmd(cmds.cCommand_Undo)
        self.check('text after 1 undo', self.TE.get_text_all(), base)
        self.check('line_count after undo', self.TE.get_line_count(), len(L))
        self.TE.cmd(cmds.cCommand_Redo)
        self.check('text after 1 redo', self.TE.get_text_all(), exp)

    def test_T37(self):
        """ed.replace_lines with 4000 DISTINCT CJK lines of random
        lengths 580..600 (like the 'random non similar lines' benchmark,
        scaled): in real-life documents adjacent lines are NOT equal, so
        nothing can be reused between lines during the wrap calculation;
        sampled line contents (incl. the CJK head and tail lines) are
        checked in addition to the full text, then one-step undo/redo."""
        n = 4000
        rng = random.Random(SEED + 8)
        L = uni_lines()
        self.TE.set_text_all(m_join(L))
        self.TE.set_caret(0, 0)
        base = m_join(L)
        new = ['\u7b2c%d\u884c %s' % (i, '\u4e2d' * rng.randint(580, 600))
               for i in range(n)]
        exp = m_join(new)
        self.info('op', 'replace_lines(0, %d, %d distinct CJK lines of 580-600 chars)'
                  % (self.TE.get_line_count() - 1, n))
        ok = self.TE.replace_lines(0, self.TE.get_line_count() - 1, new)
        self.check('replace_lines returns True', ok, True)
        self.check('text after replace (distinct CJK)', self.TE.get_text_all(), exp)
        self.check('line_count after replace', self.TE.get_line_count(), n)
        # sampled lines: first, last, CJK-head line and a middle one
        for i in (0, 1, n // 2, n - 2, n - 1):
            self.check('line %d content after replace' % i,
                       self.TE.get_text_line(i), new[i])
        self.TE.set_caret(0, n - 1)
        self.check('text stable after caret to last line',
                   self.TE.get_text_all(), exp)
        self.TE.cmd(cmds.cCommand_Undo)
        self.check('text after 1 undo', self.TE.get_text_all(), base)
        self.check('line_count after undo', self.TE.get_line_count(), len(L))
        self.TE.cmd(cmds.cCommand_Redo)
        self.check('text after 1 redo', self.TE.get_text_all(), exp)
        self.check('line %d content after redo' % (n - 1),
                   self.TE.get_text_line(n - 1), new[n - 1])
    # ========================================================================
    # STANDALONE REGRESSION TESTS T38..T41 (2026-09-07)
    # Guard the bulk undo/redo RUN paths (UndoRunInserts/UndoRunDeletes,
    # active for undo/redo runs of >= ATStrings_MinUndoRunCount = 25
    # undo items) against the fatal bug of Sep 2, 2026 (ATSynEdit
    # commits c8af8d8c + a474f020 + c5030c54; CudaText #6443,
    # ATSynEdit PR #367 and issue #368): after replace_lines of 25+
    # lines, ONE undo + ONE redo produced an EMPTY document - the
    # post-loop of UndoRunInserts (FList.DeleteRange + fake-line
    # bookkeeping) ran with both undo lists unlocked, so its
    # AddUndoItem wiped the redo list which the loop had just filled
    # with mirror items. 24 undo items take the classic per-item path
    # and never hit the bug - hence the 24/25 boundary pinned below.
    # T38 works on a FRESH untitled tab per sub-case (opened and closed
    # with the same tab discipline as the load tests: clear the
    # modified flag FIRST, focus, FileClose - never a save prompt;
    # self.TE is never reassigned, so an exception cannot orphan the
    # suite tab). T39/T40/T41 run on the suite tab.
    # ========================================================================

    def test_T38(self):
        """Exact fatal-bug repro from the bug report: on a FRESH
        untitled tab, replace lines 0..79 by cnt identical 'c' lines
        (cnt = 24 and 25), then ONE undo and ONE redo. 24 items take
        the classic per-item undo path, 25 items route through the
        bulk-run path (ATStrings_MinUndoRunCount = 25) - both must
        give the exact replaced text after redo, never an empty
        document; 3 further undo/redo cycles must stay stable."""
        for cnt, expect_len in ((24, 47), (25, 49)):
            ed2, _ = self._open_tab(
                '', tag='URTEST_TAB2', wrap=self.wrap,
                title=self._tab_title('T38', extra='cnt=%d' % cnt))
            try:
                lines = ['c'] * cnt
                want = ('c\n' * (cnt - 1)) + 'c'
                self.info('op', 'fresh tab, replace_lines(0, 79, %d lines '
                                 "'c'), 1 undo, 1 redo" % cnt)
                ok = ed2.replace_lines(0, 79, lines)
                self.check('replace_lines returns True (%d lines)' % cnt,
                           ok, True)
                self.check('text after replace (%d lines)' % cnt,
                           ed2.get_text_all(), want)
                ed2.cmd(cmds.cCommand_Undo)
                # fresh tab: one undo restores the pristine empty doc
                self.check('text after 1 undo back to empty (%d lines)' % cnt,
                           ed2.get_text_all(), '')
                ed2.cmd(cmds.cCommand_Redo)
                got = ed2.get_text_all()
                # the exact fatal symptom: empty text, length 0
                self.check('text after 1 redo is not empty (%d lines)' % cnt,
                           got != '', True)
                self.check('text after 1 redo, exact (%d lines)' % cnt,
                           got, want)
                self.check('text length after redo (%d lines)' % cnt,
                           len(got), expect_len)
                # 3 undo/redo cycles: the round-trip must stay stable
                for i in range(3):
                    ed2.cmd(cmds.cCommand_Undo)
                    ed2.cmd(cmds.cCommand_Redo)
                self.check('text after 3 undo/redo cycles (%d lines)' % cnt,
                           ed2.get_text_all(), want)
            finally:
                self._close_tab(ed2)

    def test_T39(self):
        """Boundary sweep around ATStrings_MinUndoRunCount = 25:
        replace with 23, 24, 25, 26, 27 DISTINCT lines - every count
        must behave identically (exact text after replace, after undo,
        after redo, after a bounded full drain and full redo restore).
        A bug that lives only in the bulk-run path (>= 25 items) shows
        up here as 23/24 passing and 25+ failing."""
        for cnt in (23, 24, 25, 26, 27):
            self.TE.set_text_all('')
            self.TE.set_caret(0, 0)
            lines = ['k%d' % i for i in range(cnt)]
            want = m_join(lines)
            self.info('op', 'replace_lines(0, 79, %d distinct lines)' % cnt)
            ok = self.TE.replace_lines(0, 79, lines)
            self.check('replace_lines returns True (%d lines)' % cnt,
                       ok, True)
            self.check('text after replace of %d distinct lines' % cnt,
                       self.TE.get_text_all(), want)
            self.TE.cmd(cmds.cCommand_Undo)
            self.check('text after 1 undo back to empty (%d lines)' % cnt,
                       self.TE.get_text_all(), '')
            self.TE.cmd(cmds.cCommand_Redo)
            self.check('text after 1 redo, exact (%d lines)' % cnt,
                       self.TE.get_text_all(), want)
            # drain: undo everything (the replace + the kept
            # set_text_all entry - see UNDO/REDO MODEL), then redo
            # until the replaced result is back (both bounded, never
            # blind loops)
            for i in range(6):
                before = self.TE.get_text_all()
                self.TE.cmd(cmds.cCommand_Undo)
                if self.TE.get_text_all() == before:
                    break
            self.check('undo drain ends at empty doc (%d lines)' % cnt,
                       self.TE.get_text_all(), '')
            for i in range(6):
                self.TE.cmd(cmds.cCommand_Redo)
                if N(self.TE.get_text_all()) == N(want):
                    break
            self.check('redo drain restores all %d lines' % cnt,
                       self.TE.get_text_all(), want)

    def test_T40(self):
        """The aggravated variant of the fatal bug (a474f020 'improve
        delete performance'): an 80-line initial document makes the
        REDO side use the bulk-run path too (consecutive-index run
        detection), which broke even the 24-line case. set_text_all of
        80 'x' lines, replace_lines(0, 79) of 24/25 'c' lines, ONE undo
        (must restore the 80 'x' lines), ONE redo (must restore the
        'c' block), then 3 stable cycles."""
        base = m_join(['x'] * 80)
        for cnt in (24, 25):
            self.TE.set_text_all(base)
            self.TE.set_caret(0, 0)
            lines = ['c'] * cnt
            want = ('c\n' * (cnt - 1)) + 'c'
            self.info('op', '80-line doc, replace_lines(0, 79, %d lines), '
                            '1 undo, 1 redo' % cnt)
            ok = self.TE.replace_lines(0, 79, lines)
            self.check('replace_lines returns True (80-doc, %d lines)' % cnt,
                       ok, True)
            self.check('text after replace over 80-line doc (%d lines)' % cnt,
                       self.TE.get_text_all(), want)
            self.TE.cmd(cmds.cCommand_Undo)
            self.check('text after 1 undo restores 80 x-lines (%d)' % cnt,
                       self.TE.get_text_all(), base)
            self.TE.cmd(cmds.cCommand_Redo)
            got = self.TE.get_text_all()
            self.check('text after 1 redo is not empty (80-doc, %d)' % cnt,
                       got != '', True)
            self.check('text after 1 redo, exact (80-doc, %d)' % cnt,
                       got, want)
            for i in range(3):
                self.TE.cmd(cmds.cCommand_Undo)
                self.TE.cmd(cmds.cCommand_Redo)
            self.check('text after 3 undo/redo cycles (80-doc, %d)' % cnt,
                       self.TE.get_text_all(), want)

    def test_T41(self):
        """Unicode/CJK content through the bulk run paths (multi-byte
        UTF-8, so the engine's fast ASCII insert path is NOT used):
        30 CJK + 30 Cyrillic lines replaced in one op (>= 25 undo
        items on both undo and redo), exact text after every step of
        replace -> undo -> redo, then a partial-range replace with
        DIFFERENT content (head replaced, Cyrillic tail kept) and a
        middle-range replace, each with its own undo/redo round-trip."""
        cjk = ['\u65e5\u672c\u8a9e\u306e\u884c %d' % i for i in range(30)]
        cjk2 = ['\u4e2d\u6587\u66ff\u6362\u884c %d' % i for i in range(30)]
        mix = ['\u0441\u0442\u0440\u043e\u043a\u0430 %d' % i
               for i in range(30)]
        mid = ['\u0440\u0443\u0441\u0441\u043a\u0430\u044f %d' % i
               for i in range(30)]
        doc = cjk + mix
        self.TE.set_text_all('')
        self.TE.set_caret(0, 0)
        want = m_join(doc)
        self.info('op', 'replace_lines(0, 79, 30 CJK + 30 Cyrillic lines)')
        ok = self.TE.replace_lines(0, 79, doc)
        self.check('replace_lines returns True (CJK doc)', ok, True)
        self.check('text after CJK replace', self.TE.get_text_all(), want)
        self.TE.cmd(cmds.cCommand_Undo)
        self.check('text after CJK undo back to empty',
                   self.TE.get_text_all(), '')
        self.TE.cmd(cmds.cCommand_Redo)
        got = self.TE.get_text_all()
        self.check('text after CJK redo is not empty', got != '', True)
        self.check('text after CJK redo, exact', got, want)
        # partial range, DIFFERENT content: lines 0..29 -> new CJK
        # block, the Cyrillic tail (lines 30..59) must survive
        want2 = m_join(cjk2 + mix)
        self.info('op', 'partial replace_lines(0, 29, new 30 CJK lines)')
        ok = self.TE.replace_lines(0, 29, cjk2)
        self.check('replace_lines returns True (partial range)', ok, True)
        self.check('text after partial CJK replace',
                   self.TE.get_text_all(), want2)
        self.TE.cmd(cmds.cCommand_Undo)
        self.check('text after partial undo restores full CJK doc',
                   self.TE.get_text_all(), want)
        self.TE.cmd(cmds.cCommand_Redo)
        self.check('text after partial redo, exact',
                   self.TE.get_text_all(), want2)
        # middle range: lines 5..34 (30 lines, bulk) -> Russian block;
        # the CJK head (0..4) and Cyrillic tail (35..59) must survive
        want3 = m_join(cjk2[:5] + mid + mix[5:])
        self.info('op', 'middle replace_lines(5, 34, 30 Russian lines)')
        ok = self.TE.replace_lines(5, 34, mid)
        self.check('replace_lines returns True (middle range)', ok, True)
        self.check('text after middle replace',
                   self.TE.get_text_all(), want3)
        self.TE.cmd(cmds.cCommand_Undo)
        self.check('text after middle undo restores partial state',
                   self.TE.get_text_all(), want2)
        self.TE.cmd(cmds.cCommand_Redo)
        self.check('text after middle redo, exact',
                   self.TE.get_text_all(), want3)


    # ========================================================================
    # STANDALONE FILE-LOADING TESTS L1..L7
    # (UTF-8 / UTF-16 LE+BE / UTF-32 LE+BE, mixed EOLs; file_open perf;
    # undo/redo round-trip of a small edit inside every loaded doc)
    # These tests write their own corpus files into LOAD_DIR (temp dir)
    # and open them with file_open(), which creates a separate tab per
    # file; each test checks the loaded document, then round-trips one
    # insert and one delete through undo/redo (undo data must survive
    # the encoding round-trip), and closes its tab
    # again (word wrap does not affect file loading, so they run once
    # per suite run, not once per wrap mode). The EOL scan of the loader
    # was optimized in 2026.09 - per-encoding tight loops - so these
    # tests pin its behavior for all encodings and mixed line endings.
    # ========================================================================

    # python codec + BOM bytes + expected CudaText PROP_ENC name
    LOAD_ENCODINGS = (
        ('utf-8',    'utf-8',    b'\xef\xbb\xbf',      'UTF-8 with BOM'),
        ('utf-16le', 'utf-16-le', b'\xff\xfe',           'UTF-16 LE with BOM'),
        ('utf-16be', 'utf-16-be', b'\xfe\xff',           'UTF-16 BE with BOM'),
        ('utf-32le', 'utf-32-le', b'\xff\xfe\x00\x00', 'UTF-32 LE with BOM'),
        ('utf-32be', 'utf-32-be', b'\x00\x00\xfe\xff', 'UTF-32 BE with BOM'),
        )

    def _load_make_lines(self, n):
        """Deterministic unicode corpus for the loading tests: CJK,
        Cyrillic, Latin, tabs, empty lines, long lines."""
        rng = random.Random(SEED + 9)
        lines = []
        for i in range(n):
            k = i % 8
            if k == 0:
                lines.append('')
            elif k == 1:
                lines.append('plain ascii line %d %s' % (
                    i, 'a' * rng.randint(10, 60)))
            elif k == 2:
                lines.append('mixed \u4e2d\u6587 line %d %s' % (
                    i, '\u5b57' * rng.randint(10, 30)))
            elif k == 3:
                lines.append('\u0441\u043b\u043e\u0432\u043e \u0422\u0435\u0441\u0442 %d' % i)
            elif k == 4:
                lines.append('code_%d(x); // %s' % (i, 'b' * rng.randint(10, 40)))
            elif k == 5:
                lines.append('x' * (80 + rng.randint(0, 900)))
            elif k == 6:
                lines.append('tab\tvalue\t%d' % i)
            else:
                lines.append('short %d' % i)
        return lines

    def _load_write_file(self, fn, lines, enc, raw_text=None):
        """Write the corpus with LF line endings + BOM in the given
        encoding ('utf-8' / 'utf-16le' / 'utf-16be' / 'utf-32le' /
        'utf-32be'). When raw_text is given, it is used instead of
        '\n'.join(lines) - for the mixed-EOL corpus of L6."""
        table = {e[0]: (e[1], e[2]) for e in self.LOAD_ENCODINGS}
        codec, bom = table[enc]
        text = '\n'.join(lines) if raw_text is None else raw_text
        os.makedirs(os.path.dirname(fn), exist_ok=True)
        with open(fn, 'wb') as f:
            f.write(bom)
            f.write(text.encode(codec))

    def _load_check_encoding(self, tag, enc, enc_name, n=1500):
        """Shared body of L1..L5: write an n-line unicode file in the
        encoding, file_open it, check the detected encoding name, line
        count, the full text and sampled lines (CJK included), the
        modified flag, then round-trip one unicode insert and one
        cross-line delete through undo/redo (_load_undo_redo);
        close the tab."""
        lines = self._load_make_lines(n)
        fn = os.path.join(LOAD_DIR, 'load_%s.txt' % enc)
        self._load_write_file(fn, lines, enc)
        self.info('file', '%s (%d lines, %d bytes)' % (
            fn, n, os.path.getsize(fn)))
        ed2, res = self._open_tab(
            fn, tag='URTEST_LOAD', title=self._tab_title(tag))
        try:
            self.check('%s: file_open returns True' % tag, res, True)
            self.check('%s: detected encoding name' % tag,
                       str(ed2.get_prop(cudatext.PROP_ENC)).lower(),
                       enc_name.lower())
            self.check('%s: line count' % tag, ed2.get_line_count(), n)
            self.check('%s: full text after load' % tag,
                       N(ed2.get_text_all()), '\n'.join(lines))
            # sampled lines: empty, pure ascii, CJK, Cyrillic, tab, long
            for i in (0, 1, 2, 3, 4, 5, 6, 7, n // 2, n - 1):
                self.check('%s: line %d content' % (tag, i),
                           ed2.get_text_line(i), lines[i])
            self.check('%s: modified flag not set' % tag,
                       ed2.get_prop(cudatext.PROP_MODIFIED), False)
            # undo/redo round-trip in the loaded encoding
            self._load_undo_redo(tag, ed2, lines)
        finally:
            self._close_tab(ed2)

    def _load_undo_redo(self, tag, ed, lines):
        """Undo/redo round-trip inside a freshly loaded file (L1..L5).
        A file_opened tab starts with an EMPTY undo stack - nothing
        kept, unlike after set_text_all - so ONE edit op gives exactly
        ONE undo step, and that undo must land on the exact loaded
        content. A multi-line unicode insert and a cross-line delete
        are each round-tripped op -> undo -> redo -> undo with the
        text checked against the model at every step: undo items that
        went through the encoding's conversion (UTF-8/16/32, LE/BE)
        come back as mojibake, lost or duplicated lines exactly here.
        Grouping is off for the block so '1 API call = 1 undo entry'
        holds exactly (restored True afterwards, see UNDO GROUPING)."""
        base = m_join(lines)
        # ed.set_prop(cudatext.PROP_UNDO_GROUPED, False)
        try:
            # --- insert cycle: multi-line unicode insert at (5, 2) ---
            x, y, s = 5, 2, 'undo \u4e2d\u6587\n\u4e2d\u82f1mixed \u00c4\u00df\u00e9'
            exp = m_insert(lines, x, y, s)
            self.info('%s: undo/redo op' % tag,
                      'insert(%d, %d, multi-line unicode) -> undo -> '
                      'redo -> undo' % (x, y))
            ed.set_caret(x, y)
            ed.insert(x, y, s)
            self.check('%s: text after insert' % tag,
                       ed.get_text_all(), m_join(exp))
            self.check('%s: line_count after insert' % tag,
                       ed.get_line_count(), len(exp))
            ed.cmd(cmds.cCommand_Undo)
            self.check('%s: text after undo (loaded content back)' % tag,
                       ed.get_text_all(), base)
            self.check('%s: line_count after undo' % tag,
                       ed.get_line_count(), len(lines))
            ed.cmd(cmds.cCommand_Redo)
            self.check('%s: text after redo (insert back)' % tag,
                       ed.get_text_all(), m_join(exp))
            ed.cmd(cmds.cCommand_Undo)
            self.check('%s: text after final undo (loaded content)' % tag,
                       ed.get_text_all(), base)
            # --- delete cycle: cross-line delete (0,3)-(4,4) ---
            d = m_delete(lines, 0, 3, 4, 4)
            ed.delete(0, 3, 4, 4)
            self.check('%s: text after delete' % tag,
                       ed.get_text_all(), m_join(d))
            self.check('%s: line_count after delete' % tag,
                       ed.get_line_count(), len(d))
            ed.cmd(cmds.cCommand_Undo)
            self.check('%s: text after undo (loaded content back)' % tag,
                       ed.get_text_all(), base)
            ed.cmd(cmds.cCommand_Redo)
            self.check('%s: text after redo (delete back)' % tag,
                       ed.get_text_all(), m_join(d))
            ed.cmd(cmds.cCommand_Undo)
            self.check('%s: text back to loaded state' % tag,
                       ed.get_text_all(), base)
            self.check('%s: modified flag after undo to loaded state' % tag,
                       ed.get_prop(cudatext.PROP_MODIFIED), False)
        finally:
            ed.set_prop(cudatext.PROP_UNDO_GROUPED, True)

    def test_L1(self):
        """Load a 1500-line unicode file saved as UTF-8 with BOM."""
        self._load_check_encoding('L1-utf8', 'utf-8', 'utf8_bom')

    def test_L2(self):
        """Load a 1500-line unicode file saved as UTF-16 LE with BOM."""
        self._load_check_encoding('L2-utf16le', 'utf-16le', 'utf16le_bom')

    def test_L3(self):
        """Load a 1500-line unicode file saved as UTF-16 BE with BOM."""
        self._load_check_encoding('L3-utf16be', 'utf-16be', 'utf16be_bom')

    def test_L4(self):
        """Load a 1500-line unicode file saved as UTF-32 LE with BOM."""
        self._load_check_encoding('L4-utf32le', 'utf-32le', 'utf32le_bom')

    def test_L5(self):
        """Load a 1500-line unicode file saved as UTF-32 BE with BOM."""
        self._load_check_encoding('L5-utf32be', 'utf-32be', 'utf32be_bom')

    def test_L6(self):
        """Load UTF-16 LE and UTF-32 BE files with MIXED per-line endings
        (LF, CRLF, CR): every line must get exactly the ending that was
        written into the file, and get_text_all() must round-trip the
        raw text byte-exactly. Rule for the corpus: a CR-ended line is
        never followed by an empty LF-ended line, because CR+LF bytes
        in the file then form ONE CRLF ending (standard behavior, not
        a bug). After the load checks, one insert and one delete are
        round-tripped through undo/redo; the exact raw text (every
        line's own ending included) is checked after every step."""
        texts = ['alpha one',
                 'beta \u4e2d\u6587 two',
                 'gamma 3',
                 '',
                 'delta \u4e2d\u82f1mixed four',
                 'epsilon five',
                 'zeta \u4e2d\u6587 six']
        ends = ['\n', '\r\n', '\n', '\n', '\r', '\r\n', '']
        raw = ''.join(t + e for t, e in zip(texts, ends))
        for enc in ('utf-16le', 'utf-32be'):
            fn = os.path.join(LOAD_DIR, 'mixed_%s.txt' % enc)
            self._load_write_file(fn, texts, enc, raw_text=raw)
            ed2, res = self._open_tab(
                fn, tag='URTEST_LOAD',
                title=self._tab_title('L6', extra=enc))
            try:
                self.check('%s: file_open returns True' % enc, res, True)
                self.check('%s: line count (mixed EOLs)' % enc,
                           ed2.get_line_count(), len(texts))
                self.check('%s: raw text round-trip' % enc,
                           ed2.get_text_all(), raw)
                self.check('%s: line 0 content' % enc,
                           ed2.get_text_line(0), texts[0])
                self.check('%s: line 1 content (CJK)' % enc,
                           ed2.get_text_line(1), texts[1])
                self.check('%s: line 6 content (last, no EOL)' % enc,
                           ed2.get_text_line(6), texts[6])
                # ---- undo/redo round-trip with the mixed EOLs ----
                # a freshly opened tab has an empty undo stack: one
                # edit = one undo step landing exactly on the loaded
                # raw text. Edits stay inside single lines, so every
                # line's own ending (LF / CRLF / CR / final none) must
                # survive the undo/redo data round-trip in both
                # encodings.
                ed2.set_prop(cudatext.PROP_UNDO_GROUPED, False)
                try:
                    # insert cycle: CJK text into line 1 (CRLF ending)
                    ins = ' \u4e2d\u6587 undo '
                    t1texts = list(texts)
                    t1texts[1] = texts[1][:5] + ins + texts[1][5:]
                    exp1 = ''.join(t + e for t, e in zip(t1texts, ends))
                    self.info('%s: undo/redo op' % enc,
                              'insert(5, 1, CJK) -> undo -> redo -> undo')
                    ed2.set_caret(5, 1)
                    ed2.insert(5, 1, ins)
                    self.check('%s: raw text after insert' % enc,
                               ed2.get_text_all(), exp1)
                    ed2.cmd(cmds.cCommand_Undo)
                    self.check('%s: raw text after undo' % enc,
                               ed2.get_text_all(), raw)
                    ed2.cmd(cmds.cCommand_Redo)
                    self.check('%s: raw text after redo' % enc,
                               ed2.get_text_all(), exp1)
                    ed2.cmd(cmds.cCommand_Undo)
                    self.check('%s: raw text after final undo' % enc,
                               ed2.get_text_all(), raw)
                    # delete cycle: chars inside line 4 (CR ending)
                    t4texts = list(texts)
                    t4texts[4] = texts[4][:6] + texts[4][11:]
                    exp4 = ''.join(t + e for t, e in zip(t4texts, ends))
                    ed2.delete(6, 4, 11, 4)
                    self.check('%s: raw text after delete' % enc,
                               ed2.get_text_all(), exp4)
                    ed2.cmd(cmds.cCommand_Undo)
                    self.check('%s: raw text after undo of delete' % enc,
                               ed2.get_text_all(), raw)
                    ed2.cmd(cmds.cCommand_Redo)
                    self.check('%s: raw text after redo of delete' % enc,
                               ed2.get_text_all(), exp4)
                    ed2.cmd(cmds.cCommand_Undo)
                    self.check('%s: raw text back to loaded state' % enc,
                               ed2.get_text_all(), raw)
                    self.check('%s: modified flag after undo to loaded'
                               % enc,
                               ed2.get_prop(cudatext.PROP_MODIFIED), False)
                finally:
                    ed2.set_prop(cudatext.PROP_UNDO_GROUPED, True)
            finally:
                self._close_tab(ed2)

    def test_L7(self, nlines=100000):
        """Perf: file_open() of big files (nlines lines: ~995-char ascii
        lines and ~320-char CJK lines) saved as UTF-16 LE and UTF-32 BE
        with BOM. The load time is measured and recorded in the perf
        table (column 'delete' shows the open time). After the load
        checks, ONE small CJK edit on line 7 is undone and redone: the
        undo/redo times fill the perf table's undo/redo columns, and
        the restored content is verified by line count + edited line +
        first/last lines (a full-text compare would cost seconds on a
        ~100 MB doc, and the bulk-undo bug class - empty document
        after undo - is caught by these checks too). Thresholds are
        generous regression trips, not patch gates."""
        # thresholds (warn, fail) — edit here to retune this test
        TH_OPEN = (5.0, 20.0)   # file_open
        TH_UNDO = (2.0, 10.0)   # undo of one small edit
        TH_REDO = (2.0, 10.0)   # redo of one small edit

        rng = random.Random(SEED + 10)
        lines = []
        for i in range(nlines):
            if i % 7 == 0:
                lines.append('\u4e2d\u6587 line %d %s' % (
                    i, '\u5b57' * rng.randint(300, 330)))
            else:
                lines.append('line %d %s' % (
                    i, 'x' * rng.randint(900, 990)))
        for enc in ('utf-16le', 'utf-32be'):
            if not self.begin('L7', 'L7: file_open of %d lines, %s' % (nlines, enc)):
                self.done()
                continue
            ed2 = None
            try:
                fn = os.path.join(LOAD_DIR, 'big_%s.txt' % enc)
                t0 = time.time()
                self._load_write_file(fn, lines, enc)
                t_write = time.time() - t0
                nsize = os.path.getsize(fn)
                self.info('file written: %.0f MB in %.1fs' % (
                    nsize / 1e6, t_write))
                t0 = time.time()
                res = cudatext.file_open(fn)
                t_open = time.time() - t0
                ed2 = self._ed_focused()
                self._configure_suite_tab(ed2, tag='URTEST_LOAD')
                ed2.set_prop(cudatext.PROP_TAB_TITLE,
                             self._tab_title('L7', extra=enc))
                self.check('%s: file_open returns True' % enc, res, True)
                self.check('%s: line count' % enc, ed2.get_line_count(), nlines)
                self.check('%s: first line' % enc,
                           ed2.get_text_line(0), lines[0])
                self.check('%s: CJK line 7' % enc,
                           ed2.get_text_line(7), lines[7])
                self.check('%s: last line' % enc,
                           ed2.get_text_line(nlines - 1), lines[nlines - 1])
                self.info('%s: file_open: %.2fs (%d lines, %.0f MB)' % (
                    enc, t_open, nlines, nsize / 1e6))
                # ---- undo/redo of ONE small edit on the big doc ----
                # Same contract as L1..L6: a freshly opened tab has an
                # EMPTY undo stack, so one small edit gives exactly one
                # undo step, which must land back on the loaded content.
                # The doc is huge, so full-text compares are replaced by
                # line-count + edited-line + first/last-line checks (the
                # bulk-undo bug class - empty document after undo - is
                # caught by these as well). Undo and redo are TIMED and
                # go into the perf record; the tab's wrap state is
                # inherited from the global setting here, so the hang is
                # recorded but not judged (the MP tests own that).
                y_ed = 7          # CJK line (i % 7 == 0)
                x_ed = 5
                ins = '\u4e2d\u6587 undo'
                ed2.set_prop(cudatext.PROP_UNDO_GROUPED, False)
                try:
                    ed2.set_caret(x_ed, y_ed)
                    ed2.insert(x_ed, y_ed, ins)
                    hi1, hi2 = self._hang(ed2, 'after small edit')
                    self.check('%s: line count after edit' % enc,
                               ed2.get_line_count(), nlines)
                    self.check('%s: edited line after edit' % enc,
                               ed2.get_text_line(y_ed),
                               lines[y_ed][:x_ed] + ins + lines[y_ed][x_ed:])
                    self.check('%s: first line unchanged' % enc,
                               ed2.get_text_line(0), lines[0])
                    self.check('%s: last line unchanged' % enc,
                               ed2.get_text_line(nlines - 1),
                               lines[nlines - 1])
                    t1 = time.time()
                    ed2.cmd(cmds.cCommand_Undo)
                    t2 = time.time()
                    t_undo = t2 - t1
                    hu1, hu2 = self._hang(ed2, 'after undo')
                    self.check('%s: line count after undo' % enc,
                               ed2.get_line_count(), nlines)
                    self.check('%s: edited line restored by undo' % enc,
                               ed2.get_text_line(y_ed), lines[y_ed])
                    self.check('%s: first line after undo' % enc,
                               ed2.get_text_line(0), lines[0])
                    self.check('%s: last line after undo' % enc,
                               ed2.get_text_line(nlines - 1),
                               lines[nlines - 1])
                    t1 = time.time()
                    ed2.cmd(cmds.cCommand_Redo)
                    t2 = time.time()
                    t_redo = t2 - t1
                    hr1, hr2 = self._hang(ed2, 'after redo')
                    self.check('%s: line count after redo' % enc,
                               ed2.get_line_count(), nlines)
                    self.check('%s: edited line after redo' % enc,
                               ed2.get_text_line(y_ed),
                               lines[y_ed][:x_ed] + ins + lines[y_ed][x_ed:])
                    self.check('%s: last line after redo' % enc,
                               ed2.get_text_line(nlines - 1),
                               lines[nlines - 1])
                    # leave the doc at its loaded (clean) state
                    ed2.cmd(cmds.cCommand_Undo)
                    self.check('%s: edited line after final undo' % enc,
                               ed2.get_text_line(y_ed), lines[y_ed])
                finally:
                    ed2.set_prop(cudatext.PROP_UNDO_GROUPED, True)
                t_hang = (hi1 + hi2) + (hu1 + hu2) + (hr1 + hr2)
                self.info('%s: undo %.3fs, redo %.3fs of one small edit '
                          '(hang total %.2fs)' % (enc, t_undo, t_redo,
                                                  t_hang))
                # ---- perf judging (generous) ----
                perf_fails = []
                perf_warns = []
                if t_open > TH_OPEN[1]:
                    perf_fails.append('open %.2fs exceeds FAIL threshold '
                                      '%.1fs' % (t_open, TH_OPEN[1]))
                elif t_open > TH_OPEN[0]:
                    perf_warns.append('open %.2fs exceeds warn threshold '
                                      '%.1fs' % (t_open, TH_OPEN[0]))
                for label, tv, w_, f_ in (
                        ('undo', t_undo, TH_UNDO[0], TH_UNDO[1]),
                        ('redo', t_redo, TH_REDO[0], TH_REDO[1])):
                    if tv > f_:
                        perf_fails.append('%s %.2fs exceeds FAIL threshold '
                                          '%.1fs' % (label, tv, f_))
                    elif tv > w_:
                        perf_warns.append('%s %.2fs exceeds warn threshold '
                                          '%.1fs' % (label, tv, w_))
                status = ('FAIL' if perf_fails
                          else ('WARN' if perf_warns else 'PASS'))
                self.perf.append({
                    'id': 'L7', 'wrap': 0, 'lines': nlines,
                    'del': t_open, 'undo': t_undo, 'redo': t_redo,
                    'hang': t_hang,
                    'status': status, 'note': '; '.join(
                        perf_fails + perf_warns) + ' (%s)' % enc,
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
                self._close_tab(ed2)
            self.done()


    # ========================================================================
    # STANDALONE MANUAL-BENCHMARK REPLICAS MP1..MP6 (2026-09-08)
    # The 6 manual console performance tests, replicated command for
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
    # sequence. Scaled to the suite's 300k lines (MP4 200k-of-300k);
    # call test_MP1(1000000) etc. from the console for the exact
    # 1M-line scale of the manual benchmarks.
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

        # thresholds (warn, fail) — edit here to retune this test
        TH_OP   = (2.0, 12.0)   # replace_lines
        TH_HANG = (4.0, 16.0)   # Hang1+Hang2 total

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
        # '''
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
        # '''
        
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
                h1, h2 = self._hang(ed, 'after replace_lines')
                
                # ---- read-only verification, after the timing ----
                # items end with their EOLs: concatenation = file text
                corpus = ''.join(lines)
                del lines
                self.check('text after replace (final EOL + fake line)',
                           ed.get_text_all(), corpus)
                self.check('line_count after replace (fake line incl.)',
                           ed.get_line_count(), nlines + 1)
                del corpus
                t_hang = h1 + h2
                self.info('times: replace_lines %.3fs (+Hang1 %.4fs '
                          '+Hang2 %.4fs) | hang total %.4fs' % (
                              t_op, h1, h2, t_hang))
                # ---- perf judging (inline) ----
                perf_fails = []
                perf_warns = []
                for label, tv, w_, f_ in (
                        ('replace_lines', t_op, TH_OP[0], TH_OP[1]),
                        ('hang(Hang1+Hang2)', t_hang, TH_HANG[0], TH_HANG[1])):
                    if tv > f_:
                        perf_fails.append('%s %.2fs exceeds FAIL threshold '
                                          '%.1fs' % (label, tv, f_))
                    elif tv > w_:
                        perf_warns.append('%s %.2fs exceeds warn threshold '
                                          '%.1fs' % (label, tv, w_))

                text_bad = self.cur['bad'] > 0
                status = ('FAIL' if (perf_fails or text_bad)
                          else ('WARN' if perf_warns else 'PASS'))
                self.perf.append({
                    'id': 'MP1', 'wrap': self.wrap, 'lines': nlines,
                    'del': t_op, 'undo': 0.0, 'redo': 0.0, 'hang': t_hang,
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
        # thresholds (warn, fail) — edit here to retune this test
        TH_OP   = (5.0, 12.0)   # set_text_all
        TH_HANG = (4.0, 16.0)   # Hang1+Hang2 total

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
                h1, h2 = self._hang(ed, 'after set_text_all')
                # ---- read-only verification, after the timing ----
                self.check('file_open returns True', res, True)
                self.check('text after set_text_all', ed.get_text_all(),
                           text)
                self.check('line_count after set_text_all (fake line '
                           'incl.)', ed.get_line_count(), nlines + 1)
                del text
                t_hang = h1 + h2
                self.info('times: set_text_all %.3fs (+Hang1 %.4fs '
                          '+Hang2 %.4fs) | hang total %.4fs' % (
                              t_op, h1, h2, t_hang))
                # ---- perf judging (inline) ----
                perf_fails = []
                perf_warns = []
                for label, tv, w_, f_ in (
                        ('set_text_all', t_op, TH_OP[0], TH_OP[1]),
                        ('hang(Hang1+Hang2)', t_hang, TH_HANG[0], TH_HANG[1])):
                    if tv > f_:
                        perf_fails.append('%s %.2fs exceeds FAIL threshold '
                                          '%.1fs' % (label, tv, f_))
                    elif tv > w_:
                        perf_warns.append('%s %.2fs exceeds warn threshold '
                                          '%.1fs' % (label, tv, w_))

                text_bad = self.cur['bad'] > 0
                status = ('FAIL' if (perf_fails or text_bad)
                          else ('WARN' if perf_warns else 'PASS'))
                self.perf.append({
                    'id': 'MP2', 'wrap': self.wrap, 'lines': nlines,
                    'del': t_op, 'undo': 0.0, 'redo': 0.0, 'hang': t_hang,
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
        # thresholds (warn, fail) — edit here to retune this test
        TH_DEL  = (2.0, 12.0)   # TextDeleteSelection
        TH_UNDO = (5.0, 20.0)   # Undo
        TH_REDO = (5.0, 20.0)   # Redo
        TH_HANG = (7.0, 20.0)   # Hang1+Hang2 total

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
                hd1, hd2 = self._hang(ed, 'after delete')
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
                hu1, hu2 = self._hang(ed, 'after undo')
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
                hr1, hr2 = self._hang(ed, 'after redo')
                self.check('text after redo (delete result back)',
                           ed.get_text_all(), '')
                self.check('line_count after redo', ed.get_line_count(), 1)
                del corpus
                t_hang = (hd1 + hd2) + (hu1 + hu2) + (hr1 + hr2)
                self.info('times: delete %.3fs (+Hang1 %.2fs +Hang2 %.2fs) '
                          '| undo %.3fs (+Hang1 %.2fs +Hang2 %.2fs) | '
                          'redo %.3fs (+Hang1 %.2fs +Hang2 %.2fs) | hang '
                          'total %.2fs' % (
                              t_del, hd1, hd2, t_undo, hu1, hu2, t_redo, hr1, hr2, t_hang))
                # ---- perf judging (inline) ----
                perf_fails = []
                perf_warns = []
                for label, tv, w_, f_ in (
                        ('delete', t_del, TH_DEL[0], TH_DEL[1]),
                        ('undo', t_undo, TH_UNDO[0], TH_UNDO[1]),
                        ('redo', t_redo, TH_REDO[0], TH_REDO[1]),
                        ('hang(Hang1+Hang2)', t_hang, TH_HANG[0], TH_HANG[1])):
                    if tv > f_:
                        perf_fails.append('%s %.2fs exceeds FAIL threshold '
                                          '%.1fs' % (label, tv, f_))
                    elif tv > w_:
                        perf_warns.append('%s %.2fs exceeds warn threshold '
                                          '%.1fs' % (label, tv, w_))

                text_bad = self.cur['bad'] > 0
                status = ('FAIL' if (perf_fails or text_bad)
                          else ('WARN' if perf_warns else 'PASS'))
                self.perf.append({
                    'id': 'MP3', 'wrap': self.wrap, 'lines': nlines,
                    'del': t_del, 'undo': t_undo, 'redo': t_redo,
                    'hang': t_hang,
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

    def test_MP4(self, nlines=300000, ndel=200000):
        '''Manual test4: load corpus via replace_lines (same setup as
        the manual console and as MP1/MP3), then delete the FIRST ndel
        lines, undo, redo - word wrap off and on. The manual test:
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
        # thresholds (warn, fail) — edit here to retune this test
        TH_DEL  = (2.0, 12.0)   # TextDeleteSelection
        TH_UNDO = (5.0, 20.0)   # Undo
        TH_REDO = (5.0, 20.0)   # Redo
        TH_HANG = (7.0, 20.0)   # Hang1+Hang2 total

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
                hd1, hd2 = self._hang(ed, 'after delete')
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
                hu1, hu2 = self._hang(ed, 'after undo')
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
                hr1, hr2 = self._hang(ed, 'after redo')
                self.check('text after redo (delete result back)',
                           ed.get_text_all(), after_del)
                del corpus, after_del
                t_hang = (hd1 + hd2) + (hu1 + hu2) + (hr1 + hr2)
                self.info('times: delete %.3fs (+Hang1 %.2fs +Hang2 %.2fs) '
                          '| undo %.3fs (+Hang1 %.2fs +Hang2 %.2fs) | '
                          'redo %.3fs (+Hang1 %.2fs +Hang2 %.2fs) | hang '
                          'total %.2fs' % (
                              t_del, hd1, hd2, t_undo, hu1, hu2, t_redo, hr1, hr2, t_hang))
                # ---- perf judging (inline) ----
                perf_fails = []
                perf_warns = []
                for label, tv, w_, f_ in (
                        ('delete', t_del, TH_DEL[0], TH_DEL[1]),
                        ('undo', t_undo, TH_UNDO[0], TH_UNDO[1]),
                        ('redo', t_redo, TH_REDO[0], TH_REDO[1]),
                        ('hang(Hang1+Hang2)', t_hang, TH_HANG[0], TH_HANG[1])):
                    if tv > f_:
                        perf_fails.append('%s %.2fs exceeds FAIL threshold '
                                          '%.1fs' % (label, tv, f_))
                    elif tv > w_:
                        perf_warns.append('%s %.2fs exceeds warn threshold '
                                          '%.1fs' % (label, tv, w_))

                text_bad = self.cur['bad'] > 0
                status = ('FAIL' if (perf_fails or text_bad)
                          else ('WARN' if perf_warns else 'PASS'))
                self.perf.append({
                    'id': 'MP4', 'wrap': self.wrap, 'lines': nlines,
                    'del': t_del, 'undo': t_undo, 'redo': t_redo,
                    'hang': t_hang,
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

    # def test_MP6(self, nlines=300000):
    def test_MP6(self, nlines=1000000):
        '''Manual test6: file_open of the corpus file + wrap on. The
        manual test timed the plain open of the big text file plus
        the open-with-wrap number. A freshly opened tab inherits the
        app's GLOBAL wrap setting. Before each row this test sets that
        global setting via _disable_wrap_opts (wrap off) or
        _enable_wrap_opts (wrap on), then does a true cold open:
            # untimed: _disable_wrap_opts() or _enable_wrap_opts()
            t1; cudatext.file_open(fpath); t2          # open time
            t1; app_proc(PROC_IDLE, True); t2           # Hang1
            t1; ed.action(EDACTION_UPDATE, 1); t2       # Hang2
            # wrap-on row only (global already on; also set on tab):
            t1; ed.set_prop(PROP_WRAP, 1); t2           # wrap-on time
            t1; app_proc(PROC_IDLE, True); t2           # Hang1
            t1; ed.action(EDACTION_UPDATE, 1); t2       # Hang2
        Each row starts with the corpus tab closed (the previous row
        closes it), so file_open is a true cold open.'''
        # thresholds (warn, fail) — edit here to retune this test
        TH_OPEN = (15.0, 25.0)   # file_open (+ optional wrap-on)
        TH_HANG = (1.0, 2.0)   # Hang1+Hang2 total

        fpath, t_write = mp_corpus_file(nlines)
        items = open(fpath, 'r').readlines()
        sample = tuple(s[:-1] if s.endswith('\n') else s
                       for s in (items[0], items[nlines // 2],
                                 items[nlines - 1]))
        del items
        for w in (0, 1):
            self.wrap = w
            if not self.begin('MP6', 'MP6 (manual test6): file_open of %d '
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
                    
                # this three lines are important otherwise _enable_wrap_opts have no effect in the first run of this test!
                cudatext.app_proc(cudatext.PROC_IDLE, True)
                edZ = self._ed_focused()
                edZ.action(cudatext.EDACTION_UPDATE, 1)
                
                
                # ---- the manual test's own commands, nothing else ----
                t1 = time.time()
                res = cudatext.file_open(fpath)
                t2 = time.time()
                t_open = t2 - t1
                ed = self._ed_focused()
                
                # sometimes this help too _enable_wrap_opts to take efect! 
                cudatext.app_proc(cudatext.PROC_IDLE, True)
                ed.action(cudatext.EDACTION_UPDATE,1)

                
                # configure after the timed open (shared helper)
                self._configure_suite_tab(ed, tag='URTEST_LOAD')
                ed.set_prop(cudatext.PROP_TAB_TITLE,
                            self._tab_title('MP6', w))
                # Hang1 + Hang2, right after the open
                ho1, ho2 = self._hang(ed, 'after file_open')
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
                t_wrap = 0.0
                hw1 = hw2 = 0.0
                if w == 1:
                    t1 = time.time()
                    ed.set_prop(cudatext.PROP_WRAP, 1)
                    t2 = time.time()
                    t_wrap = t2 - t1
                    hw1, hw2 = self._hang(ed, 'after set wrap on')
                # ---- read-only verification, after the timing ----
                self.check('file_open returns True', res, True)
                self.check('line count (fake line incl.)',
                           ed.get_line_count(), nlines + 1)
                self.check('first line', ed.get_text_line(0), sample[0])
                self.check('middle line', ed.get_text_line(nlines // 2),
                           sample[1])
                self.check('last line', ed.get_text_line(nlines - 1),
                           sample[2])
                t_total = t_open + t_wrap
                t_hang = (ho1 + ho2) + (hw1 + hw2)
                self.info('file_open: %.2fs%s | Hang1 %.4fs + Hang2 %.4fs%s'
                          % (t_open,
                             (' + wrap on %.2fs' % t_wrap) if w else '',
                             ho1, ho2,
                             (' + wrap Hang1 %.4fs + Hang2 %.4fs'
                              % (hw1, hw2)) if w else ''))
                # ---- perf judging (inline, generous like L7's gates) ----
                perf_fails = []
                perf_warns = []
                for label, tv, w_, f_ in (
                        ('open', t_total, TH_OPEN[0], TH_OPEN[1]),
                        ('hang(Hang1+Hang2)', t_hang, TH_HANG[0], TH_HANG[1])):
                    if tv > f_:
                        perf_fails.append('%s %.2fs exceeds FAIL threshold '
                                          '%.1fs' % (label, tv, f_))
                    elif tv > w_:
                        perf_warns.append('%s %.2fs exceeds warn threshold '
                                          '%.1fs' % (label, tv, w_))
                text_bad = self.cur['bad'] > 0
                status = ('FAIL' if (perf_fails or text_bad)
                          else ('WARN' if perf_warns else 'PASS'))
                self.perf.append({
                    'id': 'MP6', 'wrap': self.wrap, 'lines': nlines,
                    'del': t_total, 'undo': 0.0, 'redo': 0.0,
                    'hang': t_hang,
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
    ('T36', 'replace_lines: 4000 equal CJK lines (wrap stress)',
     Runner.test_T36),
    ('T37', 'replace_lines: 4000 distinct CJK lines (real-life wrap)',
     Runner.test_T37),
    ('T38', 'bulk-undo fatal bug: fresh tab, replace 24/25 lines, undo, redo',
     Runner.test_T38),
    ('T39', 'bulk-undo boundary sweep: 23..27 lines, undo, redo, drain',
     Runner.test_T39),
    ('T40', 'bulk-undo aggravated: 80-line doc, replace 24/25, undo, redo',
     Runner.test_T40),
    ('T41', 'bulk-undo CJK/Cyrillic: full, partial, middle replace ranges',
     Runner.test_T41),
    # file-loading tests (own tabs, independent of word wrap)
    ('L1', 'load UTF-8 with BOM file: encoding/content/lines',
     Runner.test_L1),
    ('L2', 'load UTF-16 LE with BOM file: encoding/content/lines',
     Runner.test_L2),
    ('L3', 'load UTF-16 BE with BOM file: encoding/content/lines',
     Runner.test_L3),
    ('L4', 'load UTF-32 LE with BOM file: encoding/content/lines',
     Runner.test_L4),
    ('L5', 'load UTF-32 BE with BOM file: encoding/content/lines',
     Runner.test_L5),
    ('L6', 'load UTF-16 LE/UTF-32 BE with mixed EOLs (LF/CRLF/CR)',
     Runner.test_L6),
    ('L7', 'perf: file_open of big UTF-16 LE / UTF-32 BE files',
     Runner.test_L7),
    # Performance coverage now comes exclusively from the MP* tests
    # (exact manual-benchmark replicas) and from L7. The older P1-P5
    # suite-tab tests were removed as redundant.
    # exact replicas of the 6 manual console benchmarks: the doc is
    # the corpus FILE opened via file_open, the timed commands are
    # the manual tests' own commands (see the MP section docstrings)
    ('MP1', 'manual test1: replace_lines of all lines, readlines() '
            'input (file-opened doc, wrap off+on)', Runner.test_MP1),
    ('MP2', 'manual test2: set_text_all of open().read() text '
            '(file-opened doc, wrap off+on)', Runner.test_MP2),
    ('MP3', 'manual test3: select-all delete, undo, redo '
            '(file-opened doc, wrap off+on)', Runner.test_MP3),
    ('MP4', 'manual test4: replace_lines load + delete first 200k of 300k, '
            'undo, redo (wrap off+on)', Runner.test_MP4),
    ('MP6', 'manual test6: file_open of the corpus file '
            '(wrap off then wrap on; toggles global wrap opts)',
     Runner.test_MP6),
]

# ----------------------------------------------------------------------------
# plugin entry points
# ----------------------------------------------------------------------------

class Command:

    # ---- menu commands ----------------------------------------------------

    def run_full(self):
        """Run the full suite, including 300k/1M-line MP* perf tests."""
        Runner(full=True).run()

    def run_quick(self):
        """Run the quick suite (core + load tests only; no heavy MP* perf)."""
        Runner(full=False).run()

    def run_single(self):
        """Show the list of all tests (core T01..T41,
        manual replicas MP1..MP4, MP6) and run
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
        """Open this module's docstring (help) in a new untitled tab."""
        if not cudatext.file_open(''):
            cudatext.ed.cmd(cmds.cmd_FileNew)
        h = cudatext.ed.get_prop(cudatext.PROP_HANDLE_SELF)
        ed = cudatext.Editor(h) if h else cudatext.ed
        ed.set_text_all(__doc__ or '')
        ed.set_prop(cudatext.PROP_TAB_TITLE, 'Undo/Redo tests: help')
        ed.set_prop(cudatext.PROP_MODIFIED, False)
        ed.set_caret(0, 0)
