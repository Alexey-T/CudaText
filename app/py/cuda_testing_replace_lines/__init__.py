"""
CudaText plugin: cuda_testing_replace_lines
============================================

Regression tests for the Editor.replace_lines() API.

Discussed in: https://github.com/Alexey-T/CudaText/issues/6374

The plugin opens a fresh untitled tab, runs 30 tests organized by category:

  1-3   Original bug (issue #6374)
  4-5   Empty-array handling
  6     Documented behavior (embedded newline)
  7-10  2x2 combination matrix
  11-18 Boundary cases
  19-22 Invalid input
  23-25 Undo/Redo integrity
  26    Caret tracking
  27    Unicode
  28-30 splitlines() integration

Each test prints a detailed record to the Console panel.
A summary dialog is shown at the end.

Commands:
  - run          : run all tests
  - run_single   : pick one test from a menu and run it
"""

from cudatext import *
import cudatext_cmd as cmds

# ---- configuration --------------------------------------------------------

CLOSE_TEST_TAB_AFTER = False   # True: close the sandbox tab when done
TEST_TAB_TITLE        = '[replace_lines tests]'


# ---- editor command wrappers --------------------------------------------
def _do_undo():
    ed.cmd(cmds.cCommand_Undo)

def _do_redo():
    ed.cmd(cmds.cCommand_Redo)

def _do_file_close():
    ed.cmd(cmds.cmd_FileClose)


# ---- editor state helpers -------------------------------------------------

def _set(text):
    """Replace entire editor content. Uses set_text_all (loses undo)."""
    ed.set_text_all(text)

def _snap():
    """Snapshot of editor state used by tests for verification."""
    return {
        'text':   ed.get_text_all(),
        'lines':  ed.get_line_count(),
        'carets': ed.get_carets(),
    }

def _state_str(s):
    return 'text={!r}, lines={}'.format(s['text'], s['lines'])

def _caret_str(s):
    cs = s.get('carets', [])
    if not cs:
        return 'carets=[]'
    return 'caret[0]={}'.format(repr(cs[0]))

def _ret_str(ret):
    return 'returns={!r}'.format(ret)


# ---- test result container -----------------------------------------------

def _result(test_id, title, category, setup_desc, call_desc, expected_desc,
            passed, got_desc, details=''):
    return {
        'id':       test_id,
        'title':    title,
        'category': category,
        'setup':    setup_desc,
        'call':     call_desc,
        'expected': expected_desc,
        'passed':   bool(passed),
        'got':      got_desc,
        'details':  details or '',
    }


# ==========================================================================
# Tests
# ==========================================================================
# Tests are organized in clean categorized sections. The numeric ID of each
# test (1..30) matches its declaration order in the source file, which in
# turn matches the order in which tests are run and listed in the menu.
#
# Categories:
#   1-3   Original bug (issue #6374)
#   4-5   Empty-array handling
#   6     Documented behavior (embedded newline)
#   7-10  2x2 combination matrix
#   11-18 Boundary cases
#   19-22 Invalid input
#   23-25 Undo/Redo integrity
#   26    Caret tracking
#   27    Unicode
#   28-30 splitlines() integration


# ----- 1-3: Original bug (issue #6374) ------------------------------------

def t01():
    """Original bug - replace_lines must not add empty trailing line."""
    _set("aaa\n")                      # aaa + Enter = 2 lines, last empty
    n = ed.get_line_count()
    ret = ed.replace_lines(0, n - 1, ["ccc"])
    s = _snap()
    ok = (s['text'] == "ccc" and s['lines'] == 1 and ret is True)
    return _result(
        1,
        'Original bug - no extra empty line when editor ends with empty line',
        'Original bug',
        'set_text_all("aaa\\n")  # 2 lines, last empty',
        'replace_lines(0, {}, ["ccc"])'.format(n - 1),
        'text="ccc", lines=1, returns True',
        ok,
        _state_str(s) + ', ' + _ret_str(ret),
    )

def t02():
    """No-newline case - no extra empty line."""
    _set("aaa")                        # 1 line, no Enter
    n = ed.get_line_count()
    ret = ed.replace_lines(0, n - 1, ["ccc"])
    s = _snap()
    ok = (s['text'] == "ccc" and s['lines'] == 1 and ret is True)
    return _result(
        2,
        "No-newline case - no extra empty line when editor has no trailing newline",
        'Original bug',
        'set_text_all("aaa")  # 1 line',
        'replace_lines(0, {}, ["ccc"])'.format(n - 1),
        'text="ccc", lines=1, returns True',
        ok,
        _state_str(s) + ', ' + _ret_str(ret),
    )

def t03():
    """set_text_all sanity - does not add empty line (reference behavior)."""
    _set("aaa\n")                      # 2 lines
    ed.set_text_all("ccc")             # NOT replace_lines, just sanity
    s = _snap()
    ok = (s['text'] == "ccc" and s['lines'] == 1)
    return _result(
        3,
        'set_text_all sanity - does not add empty line (reference behavior)',
        'Original bug',
        'set_text_all("aaa\\n")  # 2 lines',
        'set_text_all("ccc")  # not replace_lines',
        'text="ccc", lines=1',
        ok,
        _state_str(s),
    )


# ----- 4-5: Empty-array handling ------------------------------------------

def t04():
    """Empty array [] - must leave exactly one empty line (latest fix)."""
    _set("aaa\nbbb\nccc")              # 3 lines
    ret = ed.replace_lines(0, 2, [])
    s = _snap()
    ok = (s['text'] == "" and s['lines'] == 1 and ret is True)
    return _result(
        4,
        'Empty array [] - must leave exactly one empty line (latest fix)',
        'Empty-array handling',
        'set_text_all("aaa\\nbbb\\nccc")  # 3 lines',
        'replace_lines(0, 2, [])',
        'text="", lines=1, returns True  (editor must stay usable)',
        ok,
        _state_str(s) + ', ' + _ret_str(ret),
    )

def t05():
    """Proper Clear with [""] - leaves exactly one empty line, no crash."""
    _set("aaa\nbbb")                   # 2 lines
    ret = ed.replace_lines(0, 1, [""])
    s = _snap()
    ok = (s['text'] == "" and s['lines'] == 1 and ret is True)
    return _result(
        5,
        'Proper Clear - replace_lines(0, 1, [""]) leaves exactly one empty line',
        'Empty-array handling',
        'set_text_all("aaa\\nbbb")  # 2 lines',
        'replace_lines(0, 1, [""])',
        'text="", lines=1, returns True  (no crash, no ghost lines)',
        ok,
        _state_str(s) + ', ' + _ret_str(ret),
    )


# ----- 6: Documented behavior (embedded newline) -------------------------

def t06():
    """Embedded newline - \\n inside item is stored literally, not split.

    CudaText stores the \\n character as a literal byte inside the single
    line; it is only re-parsed into two lines after a file reload.
    Confirmed by Alexey as expected behavior in issue #6374.
    """
    _set("aaa")                        # 1 line
    ret = ed.replace_lines(0, 0, ["111\n222"])
    s = _snap()
    ok = (s['text'] == "111\n222" and s['lines'] == 1 and ret is True)
    return _result(
        6,
        'Embedded newline - "\\n" inside item stored literally, not split (observed)',
        'Documented behavior',
        'set_text_all("aaa")  # 1 line',
        'replace_lines(0, 0, ["111\\n222"])',
        'text="111\\n222", lines=1, returns True  (\\n stored as literal byte; only splits on reload)',
        ok,
        _state_str(s) + ', ' + _ret_str(ret),
    )


# ----- 7-10: 2x2 combination matrix --------------------------------------

def t07():
    """Combination 1 - editor ends empty, replacement does NOT end empty."""
    _set("aaa\n")                      # 2 lines, last empty
    ret = ed.replace_lines(0, 1, ["ccc"])
    s = _snap()
    ok = (s['text'] == "ccc" and s['lines'] == 1 and ret is True)
    return _result(
        7,
        'Combination 1 - editor ends empty, replacement does NOT end empty',
        '2x2 combination',
        'set_text_all("aaa\\n")  # 2 lines, last empty',
        'replace_lines(0, 1, ["ccc"])',
        'text="ccc", lines=1, returns True',
        ok,
        _state_str(s) + ', ' + _ret_str(ret),
    )

def t08():
    """Combination 2 - editor ends empty, replacement DOES end empty."""
    _set("aaa\n")                      # 2 lines, last empty
    ret = ed.replace_lines(0, 1, ["ccc", ""])
    s = _snap()
    ok = (s['text'] == "ccc\n" and s['lines'] == 2 and ret is True)
    return _result(
        8,
        'Combination 2 - editor ends empty, replacement DOES end empty',
        '2x2 combination',
        'set_text_all("aaa\\n")  # 2 lines, last empty',
        'replace_lines(0, 1, ["ccc", ""])',
        'text="ccc\\n", lines=2, returns True',
        ok,
        _state_str(s) + ', ' + _ret_str(ret),
    )

def t09():
    """Combination 3 - editor doesn't end empty, replacement doesn't end empty."""
    _set("aaa")                        # 1 line
    ret = ed.replace_lines(0, 0, ["ccc"])
    s = _snap()
    ok = (s['text'] == "ccc" and s['lines'] == 1 and ret is True)
    return _result(
        9,
        "Combination 3 - editor doesn't end empty, replacement doesn't end empty",
        '2x2 combination',
        'set_text_all("aaa")  # 1 line',
        'replace_lines(0, 0, ["ccc"])',
        'text="ccc", lines=1, returns True',
        ok,
        _state_str(s) + ', ' + _ret_str(ret),
    )

def t10():
    """Combination 4 - editor doesn't end empty, replacement DOES end empty."""
    _set("aaa")                        # 1 line
    ret = ed.replace_lines(0, 0, ["ccc", ""])
    s = _snap()
    ok = (s['text'] == "ccc\n" and s['lines'] == 2 and ret is True)
    return _result(
        10,
        "Combination 4 - editor doesn't end empty, replacement DOES end empty",
        '2x2 combination',
        'set_text_all("aaa")  # 1 line',
        'replace_lines(0, 0, ["ccc", ""])',
        'text="ccc\\n", lines=2, returns True',
        ok,
        _state_str(s) + ', ' + _ret_str(ret),
    )


# ----- 11-18: Boundary cases ---------------------------------------------

def t11():
    """Expanding lines - replacing 1 with 3, no extra 4th line."""
    _set("aaa")                        # 1 line
    ret = ed.replace_lines(0, 0, ["111", "222", "333"])
    s = _snap()
    ok = (s['text'] == "111\n222\n333" and s['lines'] == 3 and ret is True)
    return _result(
        11,
        'Expanding lines - replacing 1 line with 3, no extra 4th line',
        'Boundary',
        'set_text_all("aaa")  # 1 line',
        'replace_lines(0, 0, ["111", "222", "333"])',
        'text="111\\n222\\n333", lines=3, returns True',
        ok,
        _state_str(s) + ', ' + _ret_str(ret),
    )

def t12():
    """Shrinking lines - replacing 3 with 1, no extra empty line."""
    _set("aaa\nbbb\nccc")              # 3 lines
    ret = ed.replace_lines(0, 2, ["111"])
    s = _snap()
    ok = (s['text'] == "111" and s['lines'] == 1 and ret is True)
    return _result(
        12,
        'Shrinking lines - replacing 3 lines with 1, no extra empty line',
        'Boundary',
        'set_text_all("aaa\\nbbb\\nccc")  # 3 lines',
        'replace_lines(0, 2, ["111"])',
        'text="111", lines=1, returns True',
        ok,
        _state_str(s) + ', ' + _ret_str(ret),
    )

def t13():
    """Partial deletion - removing middle line, others preserved."""
    _set("aaa\nbbb\nccc")              # 3 lines
    ret = ed.replace_lines(1, 1, [])
    s = _snap()
    ok = (s['text'] == "aaa\nccc" and s['lines'] == 2 and ret is True)
    return _result(
        13,
        'Partial deletion - removing middle line, others preserved',
        'Boundary',
        'set_text_all("aaa\\nbbb\\nccc")  # 3 lines',
        'replace_lines(1, 1, [])',
        'text="aaa\\nccc", lines=2, returns True',
        ok,
        _state_str(s) + ', ' + _ret_str(ret),
    )

def t14():
    """Replace 2-line range with single item - no extra empty line."""
    _set("aaa\nbbb")                   # 2 lines
    ret = ed.replace_lines(0, 1, ["xxx"])
    s = _snap()
    ok = (s['text'] == "xxx" and s['lines'] == 1 and ret is True)
    return _result(
        14,
        'Replace 2-line range with single item - no extra empty line',
        'Boundary',
        'set_text_all("aaa\\nbbb")  # 2 lines',
        'replace_lines(0, 1, ["xxx"])',
        'text="xxx", lines=1, returns True',
        ok,
        _state_str(s) + ', ' + _ret_str(ret),
    )

def t15():
    """Replace middle range - preceding and following lines preserved."""
    _set("aaa\nbbb\nccc\nddd")         # 4 lines
    ret = ed.replace_lines(1, 2, ["xxx"])
    s = _snap()
    ok = (s['text'] == "aaa\nxxx\nddd" and s['lines'] == 3 and ret is True)
    return _result(
        15,
        'Replace middle range - preceding and following lines preserved',
        'Boundary',
        'set_text_all("aaa\\nbbb\\nccc\\nddd")  # 4 lines',
        'replace_lines(1, 2, ["xxx"])',
        'text="aaa\\nxxx\\nddd", lines=3, returns True',
        ok,
        _state_str(s) + ', ' + _ret_str(ret),
    )

def t16():
    """Replacing only the last line - preceding lines preserved."""
    _set("aaa\nbbb\nccc")              # 3 lines
    ret = ed.replace_lines(2, 2, ["ddd"])
    s = _snap()
    ok = (s['text'] == "aaa\nbbb\nddd" and s['lines'] == 3 and ret is True)
    return _result(
        16,
        'Replacing only the last line - preceding lines preserved',
        'Boundary',
        'set_text_all("aaa\\nbbb\\nccc")  # 3 lines',
        'replace_lines(2, 2, ["ddd"])',
        'text="aaa\\nbbb\\nddd", lines=3, returns True',
        ok,
        _state_str(s) + ', ' + _ret_str(ret),
    )

def t17():
    """Multiple empty lines in editor - all stripped, single line remains."""
    _set("aaa\n\n\n")                   # 4 lines (aaa + 3 empty)
    n = ed.get_line_count()
    ret = ed.replace_lines(0, n - 1, ["ccc"])
    s = _snap()
    ok = (s['text'] == "ccc" and s['lines'] == 1 and ret is True)
    return _result(
        17,
        'Multiple empty lines in editor - all stripped, single "ccc" remains',
        'Boundary',
        'set_text_all("aaa\\n\\n\\n")  # 4 lines, last 3 empty',
        'replace_lines(0, {}, ["ccc"])'.format(n - 1),
        'text="ccc", lines=1, returns True',
        ok,
        _state_str(s) + ', ' + _ret_str(ret),
    )

def t18():
    """Multiple empty lines in replacement - exactly N trailing empties."""
    _set("aaa")                        # 1 line
    ret = ed.replace_lines(0, 0, ["ccc", "", ""])
    s = _snap()
    ok = (s['text'] == "ccc\n\n" and s['lines'] == 3 and ret is True)
    return _result(
        18,
        'Multiple empty lines in replacement - exactly 2 trailing empties',
        'Boundary',
        'set_text_all("aaa")  # 1 line',
        'replace_lines(0, 0, ["ccc", "", ""])',
        'text="ccc\\n\\n", lines=3, returns True',
        ok,
        _state_str(s) + ', ' + _ret_str(ret),
    )


# ----- 19-22: Invalid input ----------------------------------------------

def t19():
    """Overshooting end index - safely clamps to actual last line."""
    _set("aaa\nbbb")                   # 2 lines
    ret = ed.replace_lines(0, 50, ["ccc"])
    s = _snap()
    ok = (s['text'] == "ccc" and s['lines'] == 1 and ret is True)
    return _result(
        19,
        'Overshooting end index - clamps to actual last line',
        'Invalid input',
        'set_text_all("aaa\\nbbb")  # 2 lines',
        'replace_lines(0, 50, ["ccc"])  # y2 too big',
        'text="ccc", lines=1, returns True',
        ok,
        _state_str(s) + ', ' + _ret_str(ret),
    )

def t20():
    """Inverted indices (y1>y2) - ignored, returns False, text unchanged."""
    _set("aaa\nbbb\nccc")              # 3 lines
    ret = ed.replace_lines(2, 0, ["ddd"])
    s = _snap()
    ok = (ret is False and s['text'] == "aaa\nbbb\nccc" and s['lines'] == 3)
    return _result(
        20,
        'Inverted indices (y1>y2) - ignored, returns False, text unchanged',
        'Invalid input',
        'set_text_all("aaa\\nbbb\\nccc")  # 3 lines',
        'replace_lines(2, 0, ["ddd"])  # y1>y2',
        'text="aaa\\nbbb\\nccc", lines=3, returns False',
        ok,
        _state_str(s) + ', ' + _ret_str(ret),
    )

def t21():
    """Negative index - ignored, returns False, no crash."""
    _set("aaa\nbbb")                   # 2 lines
    ret = ed.replace_lines(-1, 1, ["ccc"])
    s = _snap()
    ok = (ret is False and s['text'] == "aaa\nbbb" and s['lines'] == 2)
    return _result(
        21,
        'Negative index - ignored, returns False, no crash',
        'Invalid input',
        'set_text_all("aaa\\nbbb")  # 2 lines',
        'replace_lines(-1, 1, ["ccc"])  # y1<0',
        'text="aaa\\nbbb", lines=2, returns False',
        ok,
        _state_str(s) + ', ' + _ret_str(ret),
    )

def t22():
    """Out-of-bounds appending (y1=count) - ignored, returns False."""
    _set("aaa")                        # 1 line, only index 0 exists
    ret = ed.replace_lines(1, 1, ["bbb"])   # y1=1 doesn't exist
    s = _snap()
    ok = (ret is False and s['text'] == "aaa" and s['lines'] == 1)
    return _result(
        22,
        'Out-of-bounds appending (y1=count) - ignored, returns False',
        'Invalid input',
        'set_text_all("aaa")  # 1 line, only index 0 exists',
        "replace_lines(1, 1, [\"bbb\"])  # y1=1 doesn't exist",
        'text="aaa", lines=1, returns False',
        ok,
        _state_str(s) + ', ' + _ret_str(ret),
    )


# ----- 23-25: Undo/Redo integrity ----------------------------------------

def t23():
    """Undo integrity - single Ctrl+Z reverts to pre-replace_lines state."""
    _set("aaa\n")                      # clears undo; text now "aaa\n" (2 lines)
    ed.replace_lines(0, 1, ["ccc"])    # text now "ccc", one undo entry
    _do_undo()
    s = _snap()
    ok = (s['text'] == "aaa\n" and s['lines'] == 2)
    return _result(
        23,
        'Undo integrity - single Ctrl+Z reverts to "aaa\\n" (2 lines)',
        'Undo/Redo',
        'set_text_all("aaa\\n"); replace_lines(0, 1, ["ccc"])',
        'ed.cmd(cmds.cCommand_Undo)',
        'text="aaa\\n", lines=2',
        ok,
        _state_str(s),
    )

def t24():
    """Grouped undo - single Ctrl+Z reverts 1->3 line expand."""
    _set("aaa")
    ed.replace_lines(0, 0, ["111", "222", "333"])   # 3 lines, grouped undo
    _do_undo()
    s = _snap()
    ok = (s['text'] == "aaa" and s['lines'] == 1)
    return _result(
        24,
        'Grouped undo - single Ctrl+Z reverts 1->3 line expand',
        'Undo/Redo',
        'set_text_all("aaa"); replace_lines(0, 0, ["111","222","333"])',
        'ed.cmd(cmds.cCommand_Undo)',
        'text="aaa", lines=1',
        ok,
        _state_str(s),
    )

def t25():
    """Redo after undo - restores the replace_lines result."""
    _set("aaa")
    ed.replace_lines(0, 0, ["111", "222"])     # text now "111\n222"
    _do_undo()                                 # text now "aaa"
    _do_redo()                                 # text now "111\n222" again
    s = _snap()
    ok = (s['text'] == "111\n222" and s['lines'] == 2)
    return _result(
        25,
        'Redo after undo - restores the replace_lines result',
        'Undo/Redo',
        'set_text_all("aaa"); replace_lines(0, 0, ["111","222"]); Undo; Redo',
        'ed.cmd(cmds.cCommand_Redo)',
        'text="111\\n222", lines=2',
        ok,
        _state_str(s),
    )


# ----- 26: Caret tracking ------------------------------------------------

def t26():
    """Caret tracking - caret shifts down when lines are inserted above it."""
    _set("line1\nline2\nline3")        # 3 lines
    ed.set_caret(0, 2)                 # caret on line3 (y=2)
    ret = ed.replace_lines(0, 0, ["new_line1", "inserted_line"])  # +1 line at top
    s = _snap()
    cs = s['carets']
    caret_y = cs[0][1] if cs else -1
    ok = (s['text'] == "new_line1\ninserted_line\nline2\nline3"
          and s['lines'] == 4
          and caret_y == 3
          and ret is True)
    return _result(
        26,
        'Caret tracking - caret shifts down when lines inserted above it',
        'Caret',
        'set_text_all("line1\\nline2\\nline3"); set_caret(0, 2)  # on line3',
        'replace_lines(0, 0, ["new_line1", "inserted_line"])',
        'text="new_line1\\ninserted_line\\nline2\\nline3", lines=4, caret.y=3, returns True',
        ok,
        _state_str(s) + ', ' + _caret_str(s) + ', ' + _ret_str(ret),
    )


# ----- 27: Unicode -------------------------------------------------------

def t27():
    """Unicode - CJK + emoji rendered and stored correctly."""
    _set("")
    ret = ed.replace_lines(0, 0, ["こんにちは", "world 😊"])
    s = _snap()
    ok = (s['text'] == "こんにちは\nworld 😊" and s['lines'] == 2 and ret is True)
    return _result(
        27,
        'Unicode - CJK + emoji rendered and stored correctly',
        'Unicode',
        'set_text_all("")  # empty',
        'replace_lines(0, 0, ["こんにちは", "world 😊"])',
        'text="こんにちは\\nworld 😊", lines=2, returns True',
        ok,
        _state_str(s) + ', ' + _ret_str(ret),
    )


# ----- 28-30: splitlines() integration -----------------------------------

def t28():
    '"".splitlines() returns [] - same behavior as Test 4 (empty array).'
    _set("aaa\nbbb\nccc")
    parts = "".splitlines()            # -> []
    ret = ed.replace_lines(0, 2, parts)
    s = _snap()
    ok = (s['text'] == "" and s['lines'] == 1 and ret is True)
    return _result(
        28,
        '"".splitlines() returns [] - same behavior as Test 4',
        'splitlines() integration',
        'set_text_all("aaa\\nbbb\\nccc"); "".splitlines() -> []',
        'replace_lines(0, 2, [])',
        'text="", lines=1, returns True',
        ok,
        _state_str(s) + ', ' + _ret_str(ret),
    )

def t29():
    '"ccc".splitlines() returns ["ccc"] - same behavior as Test 7.'
    _set("aaa\n")
    parts = "ccc".splitlines()         # -> ["ccc"]
    ret = ed.replace_lines(0, 1, parts)
    s = _snap()
    ok = (s['text'] == "ccc" and s['lines'] == 1 and ret is True)
    return _result(
        29,
        '"ccc".splitlines() returns ["ccc"] - same behavior as Test 7',
        'splitlines() integration',
        'set_text_all("aaa\\n"); "ccc".splitlines() -> ["ccc"]',
        'replace_lines(0, 1, ["ccc"])',
        'text="ccc", lines=1, returns True',
        ok,
        _state_str(s) + ', ' + _ret_str(ret),
    )

def t30():
    '"aaa\\nbbb".splitlines() returns ["aaa","bbb"] - same as explicit list.'
    _set("xxx\nyyy\nzzz")
    parts = "aaa\nbbb".splitlines()    # -> ["aaa", "bbb"]
    ret = ed.replace_lines(0, 2, parts)
    s = _snap()
    ok = (s['text'] == "aaa\nbbb" and s['lines'] == 2 and ret is True)
    return _result(
        30,
        '"aaa\\nbbb".splitlines() returns ["aaa","bbb"] - same as explicit list',
        'splitlines() integration',
        'set_text_all("xxx\\nyyy\\nzzz"); "aaa\\nbbb".splitlines() -> ["aaa","bbb"]',
        'replace_lines(0, 2, ["aaa", "bbb"])',
        'text="aaa\\nbbb", lines=2, returns True',
        ok,
        _state_str(s) + ', ' + _ret_str(ret),
    )


# ---- ordered test list ---------------------------------------------------
# Tests are declared above in clean categorized order (1..30). The list below
# simply enumerates them in the same order; no re-sorting is needed.

ALL_TESTS = [
    # 1-3:   Original bug (issue #6374)
    t01, t02, t03,
    # 4-5:   Empty-array handling
    t04, t05,
    # 6:     Documented behavior (embedded newline)
    t06,
    # 7-10:  2x2 combination matrix
    t07, t08, t09, t10,
    # 11-18: Boundary cases
    t11, t12, t13, t14, t15, t16, t17, t18,
    # 19-22: Invalid input
    t19, t20, t21, t22,
    # 23-25: Undo/Redo integrity
    t23, t24, t25,
    # 26:    Caret tracking
    t26,
    # 27:    Unicode
    t27,
    # 28-30: splitlines() integration
    t28, t29, t30,
]


# ==========================================================================
# Command class
# ==========================================================================

class Command:

    def run(self):
        """Run all replace_lines tests in a fresh sandbox tab."""
        self._run_tests(ALL_TESTS)

    def run_single(self):
        """Run a single test, chosen by the user from a menu."""
        items = []
        for i, t in enumerate(ALL_TESTS):
            doc = (t.__doc__ or t.__name__).strip().splitlines()[0]
            items.append('{:>2}. {}'.format(i + 1, doc))
        choice = dlg_menu(DMENU_LIST, items,
                          caption='replace_lines tests - choose one')
        if choice is None:
            msg_status('replace_lines test cancelled')
            return
        self._run_tests([ALL_TESTS[choice]])

    # ---- core runner -----------------------------------------------------

    def _run_tests(self, tests):
        # Open a fresh untitled tab as a sandbox so the user's work is safe.
        file_open('')
        ed.set_prop(PROP_TAB_TITLE, TEST_TAB_TITLE)
        app_proc(PROC_IDLE, True)
        ed.focus()
        msg_status('[replace_lines] running {} test(s)...'.format(len(tests)))

        results = []
        for test_fn in tests:
            try:
                r = test_fn()
            except Exception as e:
                import traceback
                tb = traceback.format_exc()
                r = {
                    'id':       '?',
                    'title':    (test_fn.__doc__ or test_fn.__name__).strip(),
                    'category': 'EXCEPTION',
                    'setup':    '',
                    'call':     '',
                    'expected': '',
                    'passed':   False,
                    'got':      'EXCEPTION: ' + repr(e),
                    'details':  tb,
                }
            results.append(r)
            status = 'PASS' if r['passed'] else 'FAIL'
            msg_status('[replace_lines] test {}: {}'.format(r['id'], status))

        # Always print detailed summary to console
        self._print_console(results)

        # Always show summary dialog
        self._show_dialog(results)

        # Optionally close the sandbox tab
        if CLOSE_TEST_TAB_AFTER:
            try:
                _do_file_close()
            except Exception:
                pass  # closing is best-effort

    # ---- console reporter (detailed) -------------------------------------

    def _print_console(self, results):
        sep = '=' * 74
        print()
        print(sep)
        print('  Testing replace_lines API  (cuda_testing_replace_lines)')
        print('  Source: https://github.com/Alexey-T/CudaText/issues/6374')
        print(sep)
        for r in results:
            status = 'PASS' if r['passed'] else 'FAIL'
            print()
            print('Test {} [{}]: {}'.format(r['id'], r['category'], r['title']))
            if r['setup']:
                print('  Setup:    {}'.format(r['setup']))
            if r['call']:
                print('  Call:     {}'.format(r['call']))
            if r['expected']:
                print('  Expected: {}'.format(r['expected']))
            print('  Got:      {}'.format(r['got']))
            print('  Result:   {}'.format(status))
            if r.get('details'):
                print('  Traceback:')
                for line in r['details'].splitlines():
                    print('    ' + line)
        print()
        print(sep)
        passed = sum(1 for r in results if r['passed'])
        total  = len(results)
        print('  Summary: {}/{} tests passed'.format(passed, total))
        if passed == total:
            print('  ALL TESTS PASSED')
        else:
            failed_ids = [str(r['id']) for r in results if not r['passed']]
            print('  Failed tests: #{}'.format(', #'.join(failed_ids)))
        print(sep)
        print()

    # ---- dialog reporter (summarized) ------------------------------------

    def _show_dialog(self, results):
        passed = sum(1 for r in results if r['passed'])
        total  = len(results)
        failed = [r for r in results if not r['passed']]

        L = []
        L.append('Testing replace_lines API')
        L.append('')
        L.append('  Total:  {}'.format(total))
        L.append('  Passed: {}'.format(passed))
        L.append('  Failed: {}'.format(len(failed)))
        L.append('')
        if failed:
            L.append('Failed tests:')
            L.append('')
            for r in failed:
                L.append('  #{}  {}'.format(r['id'], r['title']))
                if r['expected']:
                    L.append('       expected: {}'.format(r['expected']))
                L.append('       got:      {}'.format(r['got']))
                L.append('')
        else:
            L.append('All tests passed!')
            L.append('')
        L.append('Detailed summary printed to the Console panel.')
        L.append('')
        L.append('Sandbox tab "{}" is left open for inspection.'.format(TEST_TAB_TITLE))

        flags = MB_OK + (MB_ICONINFO if passed == total else MB_ICONERROR)
        msg_box('\n'.join(L), flags)
