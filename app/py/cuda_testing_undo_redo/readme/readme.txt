cuda_undo_redo_tests
====================

Extensive undo/redo regression test suite for CudaText.
All results are printed to the Console panel; a summary dialog and a
log tab are shown at the end. Expected results are computed by an
independent pure-Python text model (external oracle).

TESTS ARE STANDALONE
  Every test is one self-contained function (Runner.test_T01 ..
  test_T35, test_P1 .. test_P5): document setup, the operation, every
  check, the exact-count undo/redo steps, and the perf threshold
  judging are all inside the test function. To understand or debug one
  test, read only that function top to bottom. Only non-test
  infrastructure is shared: the text-model oracle (m_join/m_insert/
  m_delete), the document factories, the check/report channel, and the
  tab lifecycle. The TESTS list at module level registers every test
  (id, label, method) - that is also what the "run single test" picker
  shows.

UNDO/REDO MODEL (verified in the CudaText console, 2026-09-05)
  set_text_all() does NOT clear the undo stack. It keeps ONE entry:
  undoing it yields the EMPTY document ("it keeps one empty"), which
  is why the undo button stays enabled for exactly one extra click
  after set_text_all and greys out only after that click. Therefore
  every test undoes / redoes EXACTLY the number of edits it made
  (N edits = N steps), checks the text after every step, and never
  drains blindly. T23 pins the kept entry with the exact console
  trace: set_text_all(''), insert, undo -> '', undo -> '' (kept entry),
  extra undo = no-op, redo -> '', redo -> inserted text, extra redo
  = no-op.

PROP_UNDO_GROUPED (important)
  CudaText merges consecutive edits within a short time window into a
  single undo step (default True). That is normal typing behaviour.

  Do NOT force PROP_UNDO_GROUPED=False for the whole suite: with
  grouping off, the 300k-line performance tests can use more than
  6 GB of RAM.

  The suite keeps grouping ON globally. Only tests that need
  "1 API call = 1 undo entry" disable it for their own body and
  restore True in a finally block. Currently: T06, T21, T25, T35.

  Pattern:

      ed.set_prop(PROP_UNDO_GROUPED, False)
      try:
          ... ops and exact-count undo/redo ...
      finally:
          ed.set_prop(PROP_UNDO_GROUPED, True)

  Related: CudaText issue #6446 (TextDeleteSelection redo with
  grouping off).

COMMANDS
  Menu: Plugins / Testing / Testing of Undo/Redo /
    - Run full suite (incl. 300k-line perf; ~1 GB RAM, minutes)
    - Run quick suite (50k-line perf; ~1 minute)
    - Run single test (filterable list of T01..T35 and P1..P5)
    - Help: what is tested / how to read results

  A temp tab (tag URTEST_TAB) is created for the run and closed at the
  end. Do not touch the editor while the suite runs. When finished, a
  summary dialog is shown and a new tab opens with the full log.

WHAT IS TESTED (35 core tests, each with word wrap OFF and ON)
  T01-T05  inserts: single char, multi-line, doc start/end, empty doc
  T06      typing simulation (12 adjacent chars; grouping forced off)
  T07-T08  typing via cCommand_TextInsert without / with a selection
  T09-T12  key Backspace / Delete, mid-line and line ends (joins)
  T13-T17  delete-selection: forward, backward, multi-line, to EOF, all
  T18      ed.delete crossing a newline
  T19      30 sequential line deletions, full undo/redo
  T20      random storm: 150 mixed inserts and deletes
  T21      unicode insert + delete roundtrip (grouping forced off)
  T22      tab char and EOL-toggle fidelity (raw snapshot)
  T23      set_text_all keeps ONE undo entry (exact console trace)
  T24      redo idempotence (extra calls on empty redo stack)
  T25      redo stack invalidated by a new edit (grouping forced off)
  T26      modified flag / save-marker cycle
  T27      undo/redo while a live selection exists
  T28      tab switched away and back around an undo
  T29      word wrap toggled between undo and redo
  T30-T31  multi-caret Enter / Backspace
  T32      100k-char line: 60k-char selection delete
  T33      single 501-line insert
  T34      undo/redo state walk: every step lands on a valid state
  T35      caret/selection moves must not change text (grouping off)

PERF / MASS-EVENT TESTS
  P1   50k lines: delete first 30k, undo, redo (wrap off/on)
  P2   300k lines: delete first 200k, undo, redo (wrap off/on)
  P3   300k lines: select-all delete + undo (wrap on)
  P4   300k lines: delete/undo/redo cycles (wrap off x1 / on x3)
  P5   300k lines: big delete, undo, small insert, undo, redo (wrap on)

  Timings thresholds:
    50k:  warn delete>1s  undo>2s  redo>2s    fail >5s / >8s / >8s
    300k: warn delete>2s  undo>5s  redo>5s    fail >12s / >25s / >25s

  Event counts: one bulk op should fire only a few on_change/on_caret
  events ("mass events" regression).

HOW TO READ THE OUTPUT
  [Txx] test name (wrap=off/on)
    ok    <check>          check passed
    FAIL  <check>          mismatch; got/expected previews follow
    ERR   exception        the test crashed the API (bug or API change)
    info  ...              undo/redo step counts, event counts, timings
  => PASS/FAIL/ERR  (n ok, m failed)
  SUMMARY: totals, failed list, perf table, event totals, verdict.
  A dialog repeats the short summary; a new tab holds the full log.

NOTES
  - set_text_all does NOT clear the undo stack: it keeps ONE entry,
    and undoing it yields the empty document (verified in the
    CudaText console; that is why one undo click is still available
    right after set_text_all and the buttons grey out only after it).
    Every test therefore undoes / redoes EXACTLY the number of edits
    it made - N edits = N steps, text checked after every step, no
    blind "drain until nothing changes" loops - and T23 pins the
    kept entry with the exact console trace. No probe and no
    fresh-tab-per-test fallback: if the engine changes any of this,
    tests FAIL and show it.
  - Test data is seeded (20260904): identical documents on every run.
  - Caret positions are asserted only where the contract is solid
    (undo restores pre-op caret/selection). Redo carets are informational.
