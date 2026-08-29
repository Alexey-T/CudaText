============================================================
 cuda_testing_replace_lines
 Testing of replace_lines API
============================================================

Regression tests for CudaText's Editor.replace_lines() API.

Related issues:
  https://github.com/Alexey-T/CudaText/issues/6374
  https://github.com/Alexey-T/CudaText/issues/6432


What it does
------------
When you invoke the command, the plugin:

  1. Opens a fresh untitled tab named "[replace_lines tests]" as a
     sandbox, so your current work is never touched.
  2. Runs 75 tests in that tab. Each test:
       - sets up a known editor state with set_text_all()
       - calls ed.replace_lines(...)
       - verifies the resulting text, line count, return value,
         caret position, or undo state
  3. Prints a detailed per-test report to the Console panel
     (Setup / Call / Expected / Got / Result).
  4. Shows a summarized dialog with pass/fail counts and the list
     of failed tests (if any).
  5. Leaves the sandbox tab open so you can inspect the final
     editor state after the last test.


Commands
--------
Two commands are registered, both in the Command Palette (F1 / Ctrl+Shift+P):

  - "Testing of replace_lines API: Run all tests"
        Runs all 75 tests sequentially.

  - "Testing of replace_lines API: Run single test..."
        Shows a menu of all tests. Pick one to run only that test
        (useful for debugging a single failure).


Tests overview
--------------
Tests are organized in clean categorized sections. The numeric ID of
each test (1..75) matches its declaration order in the source file,
which in turn matches the order in which tests are run and listed in
the menu.

  Category                  Tests   Description
  ------------------------  ------  -----------------------------------------
  Original bug (issue #6374)  1-3    The original "no extra empty line"
                                     bug + set_text_all sanity reference.
  Empty-array handling        4-5    Empty array [] must leave one empty
                                     line (latest fix); ["] as proper clear.
  Documented behavior           6    Embedded "\n" inside item is
                                     prohibited: the call returns False
                                     and changes nothing (final behavior
                                     after the PR #364 discussion).
  2x2 combination matrix     7-10    All 4 combinations of:
                                       editor ends empty/not
                                       x replacement ends empty/not
  Boundary cases            11-18    Expanding, shrinking, partial deletion,
                                     middle-range replace, last-line only,
                                     multiple empty lines (in editor or
                                     in replacement).
  Invalid input             19-22    Overshooting y2 (clamps), inverted
                                     indices (y1>y2), negative y1,
                                     out-of-bounds y1=count. All must be
                                     ignored, return False, no crash.
  Undo/Redo integrity       23-25    Single Ctrl+Z reverts one
                                     replace_lines call (grouped undo);
                                     Redo restores the result.
  Caret tracking               26    Caret shifts down when lines are
                                     inserted above it.
  Unicode                      27    CJK + emoji rendered and stored.
  splitlines() integration  28-30    "".splitlines(), "ccc".splitlines(),
                                     "aaa\nbbb".splitlines() all behave
                                     the same as the equivalent explicit
                                     list.
  EOL marks (issue #6432)   31-53    EOL marks ("\r\n", "\n", "\r") at
                                     the end of list items: LF/CRLF/CR/
                                     mixed, with and without final EOL,
                                     single-item cases, pure-EOL item,
                                     old '' workaround still works,
                                     mid-document replacement (EOL
                                     override + preservation of other
                                     lines' EOLs), suffix replacement
                                     to end of document, unmarked items
                                     get default EOL (PROP_NEWLINE),
                                     overshooting y2 (the literal example
                                     from issue #6432), splitlines(True)
                                     round-trip, normalized state equals
                                     state after loading same text,
                                     trailing CR survives set_text_all
                                     (loader fix), undo/redo restore EOLs.
  CR LF safeguard         54-64    CR LF chars not at the very line
  (issue #6432)                   end are prohibited: the whole call
                                     returns False and changes nothing,
                                     atomically - even when only one item
                                     is bad (mid-doc / doc-end / CRLF /
                                     CR / all 3 kinds in one item / double
                                     trailing EOL / leading EOL); no
                                     phantom undo entry; carets stay;
                                     exactly one trailing EOL per item is
                                     still allowed (test 62) - the allowed
                                     side of the rule.
  set_text_line           65-75    ed.set_text_line rejects ANY CR LF
  safeguard (#6432)               char in text (even a trailing one,
                                     unlike replace_lines): returns False
                                     and changes nothing; -1/-2 appends
                                     are guarded the same way; no phantom
                                     undo entry; controls without EOLs
                                     still work and return None (72, 73);
                                     test 75 documents the multi-line
                                     recipe:
                                     replace_lines(n, n, text.splitlines(True)).


Configuration
-------------
At the top of __init__.py you can change:

  CLOSE_TEST_TAB_AFTER = False
        True  -> the sandbox tab is closed automatically when the
                 tests finish.
        False -> (default) the sandbox tab is left open so you can
                 inspect the final editor state.

  TEST_TAB_TITLE = '[replace_lines tests]'
        Title of the sandbox tab.


Author: Badr Elmers, https://github.com/badrelmers
License: MIT
