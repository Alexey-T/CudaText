"""
CudaText plugin: cuda_testing_replace_lines
============================================

Regression tests for the Editor.replace_lines() API.

Discussed in: https://github.com/Alexey-T/CudaText/issues/6374
              https://github.com/Alexey-T/CudaText/issues/6432

The plugin opens a fresh untitled tab, runs 75 tests organized by category:

  1-3   Original bug (issue #6374)
  4-5   Empty-array handling
  6     Documented behavior (embedded newline is prohibited)
  7-10  2x2 combination matrix
  11-18 Boundary cases
  19-22 Invalid input
  23-25 Undo/Redo integrity
  26    Caret tracking
  27    Unicode
  28-30 splitlines() integration
  31-53 EOL marks at line ends (issue #6432)
  54-64 CR LF chars not at line end prohibited (issue #6432)
  65-75 set_text_line with CR LF chars (issue #6432)

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


# ---- EOL helpers (tests 31-53) --------------------------------------------

_LINEEND_NAMES = {
    LINEEND_NONE: 'NONE',
    LINEEND_WIN:  'CRLF',
    LINEEND_UNIX: 'LF',
    LINEEND_MAC:  'CR',
}

def _eol_default(kind):
    """Set document's default line-ending (PROP_NEWLINE) for determinism.
    Fresh tab's default is OS-dependent (Windows: CRLF, Unix: LF),
    so tests must not depend on it."""
    ed.set_prop(PROP_NEWLINE, kind)

def _snap_eol():
    """Snapshot of editor state with EOL details:
    'real' - text with original line-breaks, get_text_all(True);
    'ends' - per-line LINEEND_* codes, get_line_end()."""
    n = ed.get_line_count()
    return {
        'text':  ed.get_text_all(),        # all line-breaks normalized to "\n"
        'real':  ed.get_text_all(True),    # original line-breaks: "\r\n", "\r", "\n"
        'lines': n,
        'ends':  [ed.get_line_end(i) for i in range(n)],
    }

def _ends_str(codes):
    return '[' + ', '.join(_LINEEND_NAMES.get(c, str(c)) for c in codes) + ']'

def _state_str_eol(s):
    return 'real={!r}, ends={}, lines={}'.format(
        s['real'], _ends_str(s['ends']), s['lines'])


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
# test (1..75) matches its declaration order in the source file, which in
# turn matches the order in which tests are run and listed in the menu.
#
# Categories:
#   1-3   Original bug (issue #6374)
#   4-5   Empty-array handling
#   6     Documented behavior (embedded newline is prohibited)
#   7-10  2x2 combination matrix
#   11-18 Boundary cases
#   19-22 Invalid input
#   23-25 Undo/Redo integrity
#   26    Caret tracking
#   27    Unicode
#   28-30 splitlines() integration
#   31-53 EOL marks at line ends (issue #6432)
#   54-64 CR LF chars not at line end prohibited (issue #6432)
#   65-75 set_text_line with CR LF chars (issue #6432)


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
    """Embedded newline - \\n inside item is prohibited.

    Final behavior (after the PR #364 discussion, CudaText #6432):
    CR LF chars not at the very line end are prohibited in items of
    replace_lines(). The call returns False and changes nothing.
    (Before: chars were stored literally inside one line, rendered as
    hex escapes, and only split into lines after a file reload.)
    """
    _set("aaa")                        # 1 line
    before = _snap()
    ret = ed.replace_lines(0, 0, ["111\n222"])
    s = _snap()
    ok = (ret is False and s == before
          and s['text'] == "aaa" and s['lines'] == 1)
    return _result(
        6,
        'Embedded newline - "\\n" inside item is prohibited',
        'Documented behavior',
        'set_text_all("aaa")  # 1 line',
        'replace_lines(0, 0, ["111\\n222"])',
        'returns False, text="aaa", lines=1  (nothing changed)',
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


# ----- 31-53: EOL marks in items (issue #6432 fix) -------------------------
#
# replace_lines() supports EOL marks at the end of each list item:
#   "\r\n" -> CRLF line-ending, "\n" -> LF, "\r" -> CR.
# If the replaced range goes to the end of the document and the last item
# carries an EOL mark, the document gets the final line-break
# (internally: "last line with EOL + fake empty line", the same
# normalized state as after loading a file which ends with line-break).
# Items without an EOL mark get the document's default line-ending
# (PROP_NEWLINE). The old workaround - appending '' to the list -
# is not needed anymore, but still works and gives the same result.

def t31():
    """CRLF marks + final EOL - the exact case of issue #6432."""
    _eol_default('lf')
    _set("old")                        # 1 line
    ret = ed.replace_lines(0, 0, ["aa\r\n", "bb\r\n"])
    s = _snap_eol()
    ok = (s['real'] == "aa\r\nbb\r\n"
          and s['text'] == "aa\nbb\n"
          and s['lines'] == 3
          and s['ends'] == [LINEEND_WIN, LINEEND_WIN, LINEEND_NONE]
          and ret is True)
    return _result(
        31,
        'CRLF marks + final EOL - ["aa\\r\\n", "bb\\r\\n"] (issue #6432)',
        'EOL marks (#6432)',
        'set_text_all("old")  # 1 line',
        'replace_lines(0, 0, ["aa\\r\\n", "bb\\r\\n"])',
        'real="aa\\r\\nbb\\r\\n", text="aa\\nbb\\n" (normalized), lines=3, ends=[CRLF,CRLF,NONE], returns True',
        ok,
        _state_str_eol(s) + ', text={!r}'.format(s['text']) + ', ' + _ret_str(ret),
    )

def t32():
    """LF marks + final EOL."""
    _eol_default('lf')
    _set("old")                        # 1 line
    ret = ed.replace_lines(0, 0, ["aa\n", "bb\n"])
    s = _snap_eol()
    ok = (s['real'] == "aa\nbb\n"
          and s['lines'] == 3
          and s['ends'] == [LINEEND_UNIX, LINEEND_UNIX, LINEEND_NONE]
          and ret is True)
    return _result(
        32,
        'LF marks + final EOL - ["aa\\n", "bb\\n"]',
        'EOL marks (#6432)',
        'set_text_all("old")  # 1 line',
        'replace_lines(0, 0, ["aa\\n", "bb\\n"])',
        'real="aa\\nbb\\n", lines=3, ends=[LF,LF,NONE], returns True',
        ok,
        _state_str_eol(s) + ', ' + _ret_str(ret),
    )

def t33():
    """CR marks + final EOL."""
    _eol_default('lf')
    _set("old")                        # 1 line
    ret = ed.replace_lines(0, 0, ["aa\r", "bb\r"])
    s = _snap_eol()
    ok = (s['real'] == "aa\rbb\r"
          and s['lines'] == 3
          and s['ends'] == [LINEEND_MAC, LINEEND_MAC, LINEEND_NONE]
          and ret is True)
    return _result(
        33,
        'CR marks + final EOL - ["aa\\r", "bb\\r"]',
        'EOL marks (#6432)',
        'set_text_all("old")  # 1 line',
        'replace_lines(0, 0, ["aa\\r", "bb\\r"])',
        'real="aa\\rbb\\r", lines=3, ends=[CR,CR,NONE], returns True',
        ok,
        _state_str_eol(s) + ', ' + _ret_str(ret),
    )

def t34():
    """CRLF marks, last item without mark - no final EOL."""
    _eol_default('lf')
    _set("old")                        # 1 line
    ret = ed.replace_lines(0, 0, ["aa\r\n", "bb"])
    s = _snap_eol()
    ok = (s['real'] == "aa\r\nbb"
          and s['lines'] == 2
          and s['ends'] == [LINEEND_WIN, LINEEND_NONE]
          and ret is True)
    return _result(
        34,
        'CRLF marks, last item without mark - ["aa\\r\\n", "bb"]',
        'EOL marks (#6432)',
        'set_text_all("old")  # 1 line',
        'replace_lines(0, 0, ["aa\\r\\n", "bb"])',
        'real="aa\\r\\nbb", lines=2, ends=[CRLF,NONE], returns True',
        ok,
        _state_str_eol(s) + ', ' + _ret_str(ret),
    )

def t35():
    """LF marks, last item without mark - no final EOL."""
    _eol_default('lf')
    _set("old")                        # 1 line
    ret = ed.replace_lines(0, 0, ["aa\n", "bb"])
    s = _snap_eol()
    ok = (s['real'] == "aa\nbb"
          and s['lines'] == 2
          and s['ends'] == [LINEEND_UNIX, LINEEND_NONE]
          and ret is True)
    return _result(
        35,
        'LF marks, last item without mark - ["aa\\n", "bb"]',
        'EOL marks (#6432)',
        'set_text_all("old")  # 1 line',
        'replace_lines(0, 0, ["aa\\n", "bb"])',
        'real="aa\\nbb", lines=2, ends=[LF,NONE], returns True',
        ok,
        _state_str_eol(s) + ', ' + _ret_str(ret),
    )

def t36():
    """CR marks, last item without mark - no final EOL."""
    _eol_default('lf')
    _set("old")                        # 1 line
    ret = ed.replace_lines(0, 0, ["aa\r", "bb"])
    s = _snap_eol()
    ok = (s['real'] == "aa\rbb"
          and s['lines'] == 2
          and s['ends'] == [LINEEND_MAC, LINEEND_NONE]
          and ret is True)
    return _result(
        36,
        'CR marks, last item without mark - ["aa\\r", "bb"]',
        'EOL marks (#6432)',
        'set_text_all("old")  # 1 line',
        'replace_lines(0, 0, ["aa\\r", "bb"])',
        'real="aa\\rbb", lines=2, ends=[CR,NONE], returns True',
        ok,
        _state_str_eol(s) + ', ' + _ret_str(ret),
    )

def t37():
    """Single item with CRLF mark - one line + final EOL."""
    _eol_default('lf')
    _set("old")                        # 1 line
    ret = ed.replace_lines(0, 0, ["aa\r\n"])
    s = _snap_eol()
    ok = (s['real'] == "aa\r\n"
          and s['lines'] == 2
          and s['ends'] == [LINEEND_WIN, LINEEND_NONE]
          and ret is True)
    return _result(
        37,
        'Single item with CRLF mark - ["aa\\r\\n"] gives final EOL',
        'EOL marks (#6432)',
        'set_text_all("old")  # 1 line',
        'replace_lines(0, 0, ["aa\\r\\n"])',
        'real="aa\\r\\n", lines=2, ends=[CRLF,NONE], returns True',
        ok,
        _state_str_eol(s) + ', ' + _ret_str(ret),
    )

def t38():
    """Single item without mark - one line, no final EOL."""
    _eol_default('lf')
    _set("old")                        # 1 line
    ret = ed.replace_lines(0, 0, ["aa"])
    s = _snap_eol()
    ok = (s['real'] == "aa"
          and s['lines'] == 1
          and s['ends'] == [LINEEND_NONE]
          and ret is True)
    return _result(
        38,
        'Single item without mark - ["aa"] gives no final EOL',
        'EOL marks (#6432)',
        'set_text_all("old")  # 1 line',
        'replace_lines(0, 0, ["aa"])',
        'real="aa", lines=1, ends=[NONE], returns True',
        ok,
        _state_str_eol(s) + ', ' + _ret_str(ret),
    )

def t39():
    """Mixed EOLs, no final EOL."""
    _eol_default('lf')
    _set("old")                        # 1 line
    ret = ed.replace_lines(0, 0, ["aa\r\n", "bb\r", "cc\n", "dd"])
    s = _snap_eol()
    ok = (s['real'] == "aa\r\nbb\rcc\ndd"
          and s['lines'] == 4
          and s['ends'] == [LINEEND_WIN, LINEEND_MAC, LINEEND_UNIX, LINEEND_NONE]
          and ret is True)
    return _result(
        39,
        'Mixed EOLs, no final EOL - ["aa\\r\\n", "bb\\r", "cc\\n", "dd"]',
        'EOL marks (#6432)',
        'set_text_all("old")  # 1 line',
        'replace_lines(0, 0, ["aa\\r\\n", "bb\\r", "cc\\n", "dd"])',
        'real="aa\\r\\nbb\\rcc\\ndd", lines=4, ends=[CRLF,CR,LF,NONE], returns True',
        ok,
        _state_str_eol(s) + ', ' + _ret_str(ret),
    )

def t40():
    """Mixed EOLs + final EOL (CRLF mark on last item)."""
    _eol_default('lf')
    _set("old")                        # 1 line
    ret = ed.replace_lines(0, 0, ["aa\r\n", "bb\r", "cc\n", "dd\r\n"])
    s = _snap_eol()
    ok = (s['real'] == "aa\r\nbb\rcc\ndd\r\n"
          and s['lines'] == 5
          and s['ends'] == [LINEEND_WIN, LINEEND_MAC, LINEEND_UNIX, LINEEND_WIN, LINEEND_NONE]
          and ret is True)
    return _result(
        40,
        'Mixed EOLs + final EOL - ["aa\\r\\n", "bb\\r", "cc\\n", "dd\\r\\n"]',
        'EOL marks (#6432)',
        'set_text_all("old")  # 1 line',
        'replace_lines(0, 0, ["aa\\r\\n", "bb\\r", "cc\\n", "dd\\r\\n"])',
        'real="aa\\r\\nbb\\rcc\\ndd\\r\\n", lines=5, ends=[CRLF,CR,LF,CRLF,NONE], returns True',
        ok,
        _state_str_eol(s) + ', ' + _ret_str(ret),
    )

def t41():
    """All 3 EOL kinds in one call + final EOL with CR mark."""
    _eol_default('lf')
    _set("old")                        # 1 line
    ret = ed.replace_lines(0, 0, ["aa\n", "bb\r\n", "cc\r"])
    s = _snap_eol()
    ok = (s['real'] == "aa\nbb\r\ncc\r"
          and s['lines'] == 4
          and s['ends'] == [LINEEND_UNIX, LINEEND_WIN, LINEEND_MAC, LINEEND_NONE]
          and ret is True)
    return _result(
        41,
        'All 3 EOL kinds + final CR - ["aa\\n", "bb\\r\\n", "cc\\r"]',
        'EOL marks (#6432)',
        'set_text_all("old")  # 1 line',
        'replace_lines(0, 0, ["aa\\n", "bb\\r\\n", "cc\\r"])',
        'real="aa\\nbb\\r\\ncc\\r", lines=4, ends=[LF,CRLF,CR,NONE], returns True',
        ok,
        _state_str_eol(s) + ', ' + _ret_str(ret),
    )

def t42():
    """Old workaround (appending '') still works - same result as test 31."""
    _eol_default('lf')
    _set("old")                        # 1 line
    ret = ed.replace_lines(0, 0, ["aa\r\n", "bb\r\n", ""])
    s = _snap_eol()
    ok = (s['real'] == "aa\r\nbb\r\n"
          and s['lines'] == 3
          and s['ends'] == [LINEEND_WIN, LINEEND_WIN, LINEEND_NONE]
          and ret is True)
    return _result(
        42,
        'Old workaround ["aa\\r\\n", "bb\\r\\n", ""] - same result as test 31',
        'EOL marks (#6432)',
        'set_text_all("old")  # 1 line',
        'replace_lines(0, 0, ["aa\\r\\n", "bb\\r\\n", ""])',
        'real="aa\\r\\nbb\\r\\n", lines=3, ends=[CRLF,CRLF,NONE], returns True',
        ok,
        _state_str_eol(s) + ', ' + _ret_str(ret),
    )

def t43():
    """New style ["ccc\\n"] equals old style ["ccc", ""] of test 8."""
    _eol_default('lf')
    _set("aaa\n")                      # 2 lines, last empty
    ret = ed.replace_lines(0, 1, ["ccc\n"])
    s = _snap_eol()
    ok = (s['real'] == "ccc\n"
          and s['lines'] == 2
          and s['ends'] == [LINEEND_UNIX, LINEEND_NONE]
          and ret is True)
    return _result(
        43,
        'New style ["ccc\\n"] - same result as test 8 (["ccc", ""])',
        'EOL marks (#6432)',
        'set_text_all("aaa\\n")  # 2 lines, last empty',
        'replace_lines(0, 1, ["ccc\\n"])',
        'real="ccc\\n", lines=2, ends=[LF,NONE], returns True',
        ok,
        _state_str_eol(s) + ', ' + _ret_str(ret),
    )

def t44():
    """Mid-document replacement - EOL override, untouched lines keep EOLs."""
    _eol_default('lf')
    _set("aaa\r\nbbb\rccc")            # 3 lines: CRLF, CR, no-final
    ret = ed.replace_lines(1, 1, ["xx\n"])
    s = _snap_eol()
    ok = (s['real'] == "aaa\r\nxx\nccc"
          and s['lines'] == 3
          and s['ends'] == [LINEEND_WIN, LINEEND_UNIX, LINEEND_NONE]
          and ret is True)
    return _result(
        44,
        'Mid-doc replacement - override to LF, other lines keep CRLF/no-final',
        'EOL marks (#6432)',
        'set_text_all("aaa\\r\\nbbb\\rccc")  # 3 lines',
        'replace_lines(1, 1, ["xx\\n"])',
        'real="aaa\\r\\nxx\\nccc", lines=3, ends=[CRLF,LF,NONE], returns True',
        ok,
        _state_str_eol(s) + ', ' + _ret_str(ret),
    )

def t45():
    """Replace suffix to end of doc - final EOL from last item's mark."""
    _eol_default('lf')
    _set("aaa\nbbb\nccc")              # 3 lines
    ret = ed.replace_lines(1, 2, ["xx\r\n"])
    s = _snap_eol()
    ok = (s['real'] == "aaa\nxx\r\n"
          and s['lines'] == 3
          and s['ends'] == [LINEEND_UNIX, LINEEND_WIN, LINEEND_NONE]
          and ret is True)
    return _result(
        45,
        'Replace lines 1..2 (to end) with ["xx\\r\\n"] - doc gets final CRLF',
        'EOL marks (#6432)',
        'set_text_all("aaa\\nbbb\\nccc")  # 3 lines',
        'replace_lines(1, 2, ["xx\\r\\n"])',
        'real="aaa\\nxx\\r\\n", lines=3, ends=[LF,CRLF,NONE], returns True',
        ok,
        _state_str_eol(s) + ', ' + _ret_str(ret),
    )

def t46():
    """Items without EOL mark get document's default EOL (PROP_NEWLINE)."""
    _set("aaa\nbbb\nccc")              # 3 lines
    _eol_default('cr')                 # converts ALL existing EOLs to CR too
    ret = ed.replace_lines(1, 1, ["xx"])
    s = _snap_eol()
    ok = (s['real'] == "aaa\rxx\rccc"
          and s['lines'] == 3
          and s['ends'] == [LINEEND_MAC, LINEEND_MAC, LINEEND_NONE]
          and ret is True)
    return _result(
        46,
        'Item without EOL mark gets default EOL (PROP_NEWLINE="cr")',
        'EOL marks (#6432)',
        'set_text_all("aaa\\nbbb\\nccc"); set_prop(PROP_NEWLINE, "cr")',
        'replace_lines(1, 1, ["xx"])',
        'real="aaa\\rxx\\rccc", lines=3, ends=[CR,CR,NONE], returns True  '
        '(set_prop converts all existing EOLs first)',
        ok,
        _state_str_eol(s) + ', ' + _ret_str(ret),
    )

def t47():
    """Overshooting y2 + final EOL - the literal example from issue #6432."""
    _eol_default('lf')
    _set("old")                        # 1 line
    ret = ed.replace_lines(0, 100, ["aa\r\n", "bb\r\n"])
    s = _snap_eol()
    ok = (s['real'] == "aa\r\nbb\r\n"
          and s['lines'] == 3
          and s['ends'] == [LINEEND_WIN, LINEEND_WIN, LINEEND_NONE]
          and ret is True)
    return _result(
        47,
        'Overshooting y2 (0..100) with CRLF marks - the example of issue #6432',
        'EOL marks (#6432)',
        'set_text_all("old")  # 1 line',
        'replace_lines(0, 100, ["aa\\r\\n", "bb\\r\\n"])',
        'real="aa\\r\\nbb\\r\\n", lines=3, ends=[CRLF,CRLF,NONE], returns True',
        ok,
        _state_str_eol(s) + ', ' + _ret_str(ret),
    )

def t48():
    """Pure-EOL item ["\\n"] - one empty line with LF ending."""
    _eol_default('lf')
    _set("old")                        # 1 line
    ret = ed.replace_lines(0, 0, ["\n"])
    s = _snap_eol()
    ok = (s['real'] == "\n"
          and s['lines'] == 2
          and s['ends'] == [LINEEND_UNIX, LINEEND_NONE]
          and ret is True)
    return _result(
        48,
        'Pure-EOL item ["\\n"] - empty line with LF ending',
        'EOL marks (#6432)',
        'set_text_all("old")  # 1 line',
        'replace_lines(0, 0, ["\\n"])',
        'real="\\n", lines=2, ends=[LF,NONE], returns True',
        ok,
        _state_str_eol(s) + ', ' + _ret_str(ret),
    )

def t49():
    """splitlines(True) round-trip - full EOL preservation."""
    _eol_default('lf')
    _set("aaa\nbbb\r\n")               # 3 lines: aaa(LF), bbb(CRLF), fake
    text = ed.get_text_all(True)       # "aaa\nbbb\r\n"
    items = text.splitlines(True)      # ["aaa\n", "bbb\r\n"]
    ret = ed.replace_lines(0, ed.get_line_count()-1, items)
    s = _snap_eol()
    ok = (s['real'] == "aaa\nbbb\r\n"
          and s['lines'] == 3
          and s['ends'] == [LINEEND_UNIX, LINEEND_WIN, LINEEND_NONE]
          and ret is True)
    return _result(
        49,
        'splitlines(True) round-trip - mixed EOLs + final EOL preserved',
        'EOL marks (#6432)',
        'set_text_all("aaa\\nbbb\\r\\n"); text=get_text_all(True)',
        'replace_lines(0, count-1, text.splitlines(True))',
        'real="aaa\\nbbb\\r\\n", lines=3, ends=[LF,CRLF,NONE], returns True',
        ok,
        _state_str_eol(s) + ', ' + _ret_str(ret),
    )

def t50():
    """Normalized state - result equals state after loading same text."""
    _eol_default('lf')
    _set("old")                        # 1 line
    ed.replace_lines(0, 0, ["aa\r\n", "bb\r\n"])
    after_replace = _snap_eol()
    _set("aa\r\nbb\r\n")               # the same text, via loader
    after_load = _snap_eol()
    ok = (after_replace['real'] == after_load['real'] == "aa\r\nbb\r\n"
          and after_replace['lines'] == after_load['lines'] == 3
          and after_replace['ends'] == after_load['ends']
          == [LINEEND_WIN, LINEEND_WIN, LINEEND_NONE])
    return _result(
        50,
        'Normalized state - replace_lines result == state after set_text_all',
        'EOL marks (#6432)',
        'A: set_text_all("old"); replace_lines(0, 0, ["aa\\r\\n", "bb\\r\\n"])',
        'B: set_text_all("aa\\r\\nbb\\r\\n")  # compare snapshots',
        'A and B: real="aa\\r\\nbb\\r\\n", lines=3, ends=[CRLF,CRLF,NONE]',
        ok,
        'A: ' + _state_str_eol(after_replace) + '; B: ' + _state_str_eol(after_load),
    )

def t51():
    """Reference: trailing CR survives set_text_all (loader fix)."""
    _eol_default('lf')
    _set("aa\r")                       # text ends with CR
    s = _snap_eol()
    ok = (s['real'] == "aa\r"
          and s['lines'] == 2
          and s['ends'] == [LINEEND_MAC, LINEEND_NONE])
    return _result(
        51,
        'Reference (loader fix) - set_text_all("aa\\r") keeps trailing CR',
        'EOL marks (#6432)',
        'set_text_all("aa\\r")  # text ends with CR',
        'get_text_all(True)  # no replace_lines call',
        'real="aa\\r", lines=2, ends=[CR,NONE]',
        ok,
        _state_str_eol(s),
    )

def t52():
    """Undo restores original EOLs after EOL-marked replacement."""
    _eol_default('lf')
    _set("aaa\r\n")                    # 2 lines: aaa(CRLF) + fake
    ed.replace_lines(0, 1, ["bb\n", "cc\n"])   # -> "bb\ncc\n"
    _do_undo()
    s = _snap_eol()
    ok = (s['real'] == "aaa\r\n"
          and s['lines'] == 2
          and s['ends'] == [LINEEND_WIN, LINEEND_NONE])
    return _result(
        52,
        'Undo - single Ctrl+Z restores "aaa\\r\\n" with original CRLF',
        'EOL marks (#6432)',
        'set_text_all("aaa\\r\\n"); replace_lines(0, 1, ["bb\\n", "cc\\n"])',
        'ed.cmd(cmds.cCommand_Undo)',
        'real="aaa\\r\\n", lines=2, ends=[CRLF,NONE]',
        ok,
        _state_str_eol(s),
    )

def t53():
    """Redo restores the EOL-marked replacement result."""
    _eol_default('lf')
    _set("aaa\r\n")                    # 2 lines: aaa(CRLF) + fake
    ed.replace_lines(0, 1, ["bb\n", "cc\n"])   # -> "bb\ncc\n"
    _do_undo()                                 # -> "aaa\r\n"
    _do_redo()                                 # -> "bb\ncc\n" again
    s = _snap_eol()
    ok = (s['real'] == "bb\ncc\n"
          and s['lines'] == 3
          and s['ends'] == [LINEEND_UNIX, LINEEND_UNIX, LINEEND_NONE])
    return _result(
        53,
        'Redo - restores "bb\\ncc\\n" with LF marks and final EOL',
        'EOL marks (#6432)',
        'set_text_all("aaa\\r\\n"); replace_lines(0, 1, ["bb\\n", "cc\\n"]); Undo; Redo',
        'ed.cmd(cmds.cCommand_Redo)',
        'real="bb\\ncc\\n", lines=3, ends=[LF,LF,NONE]',
        ok,
        _state_str_eol(s),
    )


# ---- ordered test list ---------------------------------------------------
# Tests are declared above in clean categorized order (1..53). The list below
# simply enumerates them in the same order; no re-sorting is needed.


# ----- 54-64: CR LF chars not at line end prohibited (issue #6432) -------

def t54():
    """Inner "\\n" in item is prohibited: whole call returns False."""
    _set("aaa\r\nbbb\r\nccc")
    _eol_default('crlf')               # no-op: doc is all CRLF
    before = _snap_eol()
    ret = ed.replace_lines(1, 1, ["xx\nyy"])
    s = _snap_eol()
    ok = (ret is False and s == before
          and s['real'] == "aaa\r\nbbb\r\nccc"
          and s['lines'] == 3
          and s['ends'] == [LINEEND_WIN, LINEEND_WIN, LINEEND_NONE])
    return _result(
        54,
        'Inner "\\n" in item: replace_lines(1, 1, ["xx\\nyy"]) returns False',
        'CR LF safeguard (#6432)',
        'set_text_all("aaa\\r\\nbbb\\r\\nccc"); set_prop(PROP_NEWLINE, "crlf")',
        'replace_lines(1, 1, ["xx\\nyy"])',
        'returns False, real="aaa\\r\\nbbb\\r\\nccc", '
        'ends=[CRLF,CRLF,NONE], lines=3  (nothing changed)',
        ok,
        _state_str_eol(s) + ', ' + _ret_str(ret),
    )


def t55():
    """Replace to doc end with inner "\\n": returns False, no change."""
    _set("aaa\n")
    before = _snap_eol()
    ret = ed.replace_lines(1, 2, ["bb\ncc"])
    s = _snap_eol()
    ok = (ret is False and s == before
          and s['real'] == "aaa\n"
          and s['lines'] == 2
          and s['ends'] == [LINEEND_UNIX, LINEEND_NONE])
    return _result(
        55,
        'Replace to doc end with item "bb\\ncc": returns False, no change',
        'CR LF safeguard (#6432)',
        'set_text_all("aaa\\n")',
        'replace_lines(1, 2, ["bb\\ncc"])',
        'returns False, real="aaa\\n", ends=[LF,NONE], lines=2',
        ok,
        _state_str_eol(s) + ', ' + _ret_str(ret),
    )


def t56():
    """Inner "\\r\\n" in item is prohibited (not one EOL, not split)."""
    _set("aaa")
    before = _snap_eol()
    ret = ed.replace_lines(0, 0, ["x\r\ny"])
    s = _snap_eol()
    ok = (ret is False and s == before
          and s['real'] == "aaa"
          and s['lines'] == 1
          and s['ends'] == [LINEEND_NONE])
    return _result(
        56,
        'Item with inner "\\r\\n" ("x\\r\\ny"): returns False, no change',
        'CR LF safeguard (#6432)',
        'set_text_all("aaa")',
        'replace_lines(0, 0, ["x\\r\\ny"])',
        'returns False, real="aaa", ends=[NONE], lines=1',
        ok,
        _state_str_eol(s) + ', ' + _ret_str(ret),
    )


def t57():
    """Inner "\\r" in item is prohibited."""
    _set("aaa")
    before = _snap_eol()
    ret = ed.replace_lines(0, 0, ["x\ry"])
    s = _snap_eol()
    ok = (ret is False and s == before
          and s['real'] == "aaa"
          and s['lines'] == 1
          and s['ends'] == [LINEEND_NONE])
    return _result(
        57,
        'Item with inner "\\r" ("x\\ry"): returns False, no change',
        'CR LF safeguard (#6432)',
        'set_text_all("aaa")',
        'replace_lines(0, 0, ["x\\ry"])',
        'returns False, real="aaa", ends=[NONE], lines=1',
        ok,
        _state_str_eol(s) + ', ' + _ret_str(ret),
    )


def t58():
    """All 3 EOL kinds inside one item: prohibited."""
    _set("old\nold\nold")
    _eol_default('lf')
    before = _snap_eol()
    ret = ed.replace_lines(0, 0, ["a\nb\r\nc\rd"])
    s = _snap_eol()
    ok = (ret is False and s == before
          and s['real'] == "old\nold\nold"
          and s['lines'] == 3
          and s['ends'] == [LINEEND_UNIX, LINEEND_UNIX, LINEEND_NONE])
    return _result(
        58,
        'Item with all 3 EOL kinds inside ("a\\nb\\r\\nc\\rd"): returns False',
        'CR LF safeguard (#6432)',
        'set_text_all("old\\nold\\nold"); set_prop(PROP_NEWLINE, "lf")',
        'replace_lines(0, 0, ["a\\nb\\r\\nc\\rd"])',
        'returns False, real="old\\nold\\nold", ends=[LF,LF,NONE], lines=3',
        ok,
        _state_str_eol(s) + ', ' + _ret_str(ret),
    )


def t59():
    """One bad item among good ones: the WHOLE call is rejected."""
    _set("aaa\r\nbbb\r\n")
    before = _snap_eol()
    ret = ed.replace_lines(0, 1, ["new1\n", "ba\nd", "new2\n"])
    s = _snap_eol()
    ok = (ret is False and s == before
          and s['real'] == "aaa\r\nbbb\r\n"
          and s['lines'] == 3
          and s['ends'] == [LINEEND_WIN, LINEEND_WIN, LINEEND_NONE])
    return _result(
        59,
        '["new1\\n", "ba\\nd", "new2\\n"]: one bad item rejects everything',
        'CR LF safeguard (#6432)',
        'set_text_all("aaa\\r\\nbbb\\r\\n")',
        'replace_lines(0, 1, ["new1\\n", "ba\\nd", "new2\\n"])',
        'returns False, real="aaa\\r\\nbbb\\r\\n", ends=[CRLF,CRLF,NONE], '
        'lines=3  (even the good items are not applied)',
        ok,
        _state_str_eol(s) + ', ' + _ret_str(ret),
    )


def t60():
    """Double trailing EOL is prohibited; single trailing still allowed."""
    _set("aaa")
    before = _snap_eol()
    ret_bad = ed.replace_lines(0, 0, ["a\n\n"])
    s_bad = _snap_eol()
    _set("aaa")
    ret_ok = ed.replace_lines(0, 0, ["a\n"])
    s_ok = _snap_eol()
    ok = (ret_bad is False and s_bad == before
          and ret_ok is True
          and s_ok['real'] == "a\n"
          and s_ok['lines'] == 2
          and s_ok['ends'] == [LINEEND_UNIX, LINEEND_NONE])
    return _result(
        60,
        'Item "a\\n\\n" rejected (only ONE trailing EOL); "a\\n" accepted',
        'CR LF safeguard (#6432)',
        'set_text_all("aaa") (twice)',
        'replace_lines(0, 0, ["a\\n\\n"]) vs replace_lines(0, 0, ["a\\n"])',
        'first: False, doc unchanged; second: True, real="a\\n", '
        'ends=[LF,NONE], lines=2',
        ok,
        'double: ' + _state_str_eol(s_bad) + ', ' + _ret_str(ret_bad)
        + '; single: ' + _state_str_eol(s_ok) + ', ' + _ret_str(ret_ok),
    )


def t61():
    """Rejected call adds no undo entry: undo still undoes the last edit."""
    _set("aaa\r\n")
    ret1 = ed.replace_lines(0, 0, ["bb"])
    ret2 = ed.replace_lines(0, 0, ["cc\ndd"])
    _do_undo()
    s_undo = _snap_eol()
    _do_redo()
    s_redo = _snap_eol()
    ok = (ret1 is True and ret2 is False
          and s_undo['real'] == "aaa\r\n"
          and s_undo['lines'] == 2
          and s_undo['ends'] == [LINEEND_WIN, LINEEND_NONE]
          and s_redo['real'] == "bb\r\n"
          and s_redo['lines'] == 2
          and s_redo['ends'] == [LINEEND_WIN, LINEEND_NONE])
    return _result(
        61,
        'Undo/redo after rejected call: only the successful edit is undone',
        'CR LF safeguard (#6432)',
        'set_text_all("aaa\\r\\n")',
        'replace_lines(0, 0, ["bb"]); replace_lines(0, 0, ["cc\\ndd"]); '
        'undo; redo',
        'undo: real="aaa\\r\\n", lines=2; redo: real="bb\\r\\n", lines=2 '
        '(no phantom undo step from the rejected call)',
        ok,
        'undo: ' + _state_str_eol(s_undo) + '; redo: ' + _state_str_eol(s_redo)
        + ', rets: ' + repr(ret1) + '/' + repr(ret2),
    )


def t62():
    """Replacing the fake last line with item ending in EOL: still works."""
    _set("aaa\n")
    ret = ed.replace_lines(1, 1, ["bb\n"])
    s = _snap_eol()
    ok = (s['real'] == "aaa\nbb\n"
          and s['lines'] == 3
          and s['ends'] == [LINEEND_UNIX, LINEEND_UNIX, LINEEND_NONE]
          and ret is True)
    return _result(
        62,
        'Replace fake last line with "bb\\n": trailing EOL still allowed',
        'CR LF safeguard (#6432)',
        'set_text_all("aaa\\n")',
        'replace_lines(1, 1, ["bb\\n"])',
        'real="aaa\\nbb\\n", lines=3, ends=[LF,LF,NONE], returns True',
        ok,
        _state_str_eol(s) + ', ' + _ret_str(ret),
    )


def t63():
    """Rejected call does not move carets."""
    _set("l0\nl1\nl2\nl3\n")
    ed.set_caret(0, 3)
    n_before = ed.get_line_count()
    ret = ed.replace_lines(1, 1, ["x\ny"])
    s = _snap()
    ok = (ret is False
          and s['lines'] == n_before
          and s['lines'] == 5
          and s['carets'][0][:2] == (0, 3))
    return _result(
        63,
        'Caret below the range does not move when the call is rejected',
        'CR LF safeguard (#6432)',
        'set_text_all("l0\\nl1\\nl2\\nl3\\n"); set_caret(0, 3)',
        'replace_lines(1, 1, ["x\\ny"])',
        'returns False, lines=5, caret[0]=(0, 3) unchanged',
        ok,
        _state_str(s) + ', ' + _caret_str(s) + ', ' + _ret_str(ret),
    )


def t64():
    """Boundary matrix: exactly one trailing EOL allowed, all else False."""
    cases = [
        ("a\n",       True,  "a\n"),
        ("a\r\n",     True,  "a\r\n"),
        ("a\r",       True,  "a\r"),
        ("a\n\n",     False, None),
        ("a\r\n\r\n", False, None),
        ("\na",       False, None),
        ("a\rb",      False, None),
    ]
    results = []
    ok = True
    for item, want, real in cases:
        _set("z")
        before = _snap_eol()
        ret = ed.replace_lines(0, 0, [item])
        s = _snap_eol()
        if want:
            good = (ret is True and s['real'] == real and s['lines'] == 2)
        else:
            good = (ret is False and s == before)
        shown = item.replace('\r', '\\r').replace('\n', '\\n')
        results.append('"{}"->{}'.format(shown, ret))
        ok = ok and good
    return _result(
        64,
        'One trailing EOL of each kind allowed; doubled/inner/leading rejected',
        'CR LF safeguard (#6432)',
        'set_text_all("z") for each case',
        'replace_lines(0, 0, [item]) for: "a\\n", "a\\r\\n", "a\\r", '
        '"a\\n\\n", "a\\r\\n\\r\\n", "\\na", "a\\rb"',
        'True for the 3 single trailing EOLs (real=item, lines=2); '
        'False + no change for all others',
        ok,
        '; '.join(results),
    )


# ----- 65-75: set_text_line with CR LF chars (issue #6432) ---------------

def t65():
    """set_text_line with inner "\\n": returns False, nothing changes."""
    _set("aaa\nbbb\nccc")
    _eol_default('lf')
    before = _snap_eol()
    ret = ed.set_text_line(1, "xx\nyy")
    s = _snap_eol()
    ok = (ret is False and s == before
          and s['real'] == "aaa\nbbb\nccc"
          and s['lines'] == 3
          and s['ends'] == [LINEEND_UNIX, LINEEND_UNIX, LINEEND_NONE])
    return _result(
        65,
        'set_text_line(1, "xx\\nyy") returns False, line is not changed',
        'set_text_line safeguard (#6432)',
        'set_text_all("aaa\\nbbb\\nccc"); set_prop(PROP_NEWLINE, "lf")',
        'set_text_line(1, "xx\\nyy")',
        'returns False, real="aaa\\nbbb\\nccc", ends=[LF,LF,NONE], lines=3',
        ok,
        _state_str_eol(s) + ', ' + _ret_str(ret),
    )


def t66():
    """set_text_line with all 3 EOL kinds in text: returns False."""
    _set("old\nold\nold")
    _eol_default('lf')
    before = _snap_eol()
    ret = ed.set_text_line(0, "a\nb\r\nc\rd")
    s = _snap_eol()
    ok = (ret is False and s == before
          and s['real'] == "old\nold\nold"
          and s['lines'] == 3
          and s['ends'] == [LINEEND_UNIX, LINEEND_UNIX, LINEEND_NONE])
    return _result(
        66,
        'set_text_line(0, "a\\nb\\r\\nc\\rd") returns False, no change',
        'set_text_line safeguard (#6432)',
        'set_text_all("old\\nold\\nold"); set_prop(PROP_NEWLINE, "lf")',
        'set_text_line(0, "a\\nb\\r\\nc\\rd")',
        'returns False, real="old\\nold\\nold", ends=[LF,LF,NONE], lines=3',
        ok,
        _state_str_eol(s) + ', ' + _ret_str(ret),
    )


def t67():
    """set_text_line rejects even a TRAILING EOL (unlike replace_lines)."""
    _set("aaa\n")
    before = _snap_eol()
    ret = ed.set_text_line(1, "bb\n")
    s = _snap_eol()
    ok = (ret is False and s == before
          and s['real'] == "aaa\n"
          and s['lines'] == 2
          and s['ends'] == [LINEEND_UNIX, LINEEND_NONE])
    return _result(
        67,
        'set_text_line(1, "bb\\n") returns False (trailing EOL rejected too)',
        'set_text_line safeguard (#6432)',
        'set_text_all("aaa\\n")',
        'set_text_line(1, "bb\\n")',
        'returns False, real="aaa\\n", ends=[LF,NONE], lines=2',
        ok,
        _state_str_eol(s) + ', ' + _ret_str(ret),
    )


def t68():
    """set_text_line(-1, ...) with EOL chars: returns False, no append."""
    _set("aaa\n")
    before = _snap_eol()
    ret = ed.set_text_line(-1, "xx\nyy")
    s = _snap_eol()
    ok = (ret is False and s == before
          and s['real'] == "aaa\n"
          and s['lines'] == 2
          and s['ends'] == [LINEEND_UNIX, LINEEND_NONE])
    return _result(
        68,
        'set_text_line(-1, "xx\\nyy") returns False, nothing appended',
        'set_text_line safeguard (#6432)',
        'set_text_all("aaa\\n")',
        'set_text_line(-1, "xx\\nyy")',
        'returns False, real="aaa\\n", ends=[LF,NONE], lines=2',
        ok,
        _state_str_eol(s) + ', ' + _ret_str(ret),
    )


def t69():
    """set_text_line(-2, ...) with EOL chars: returns False, no append."""
    _set("aaa")
    _eol_default('lf')
    before = _snap_eol()
    ret = ed.set_text_line(-2, "xx\nyy")
    s = _snap_eol()
    ok = (ret is False and s == before
          and s['real'] == "aaa"
          and s['lines'] == 1
          and s['ends'] == [LINEEND_NONE])
    return _result(
        69,
        'set_text_line(-2, "xx\\nyy") returns False, nothing appended',
        'set_text_line safeguard (#6432)',
        'set_text_all("aaa"); set_prop(PROP_NEWLINE, "lf")',
        'set_text_line(-2, "xx\\nyy")',
        'returns False, real="aaa", ends=[NONE], lines=1',
        ok,
        _state_str_eol(s) + ', ' + _ret_str(ret),
    )


def t70():
    """Asymmetry: trailing EOL ok in replace_lines, rejected in set_text_line."""
    _set("aaa\n")
    before = _snap_eol()
    ret1 = ed.set_text_line(1, "bb\n")
    s1 = _snap_eol()
    _set("aaa\n")
    ret2 = ed.replace_lines(1, 1, ["bb\n"])
    s2 = _snap_eol()
    ok = (ret1 is False and s1 == before
          and ret2 is True
          and s2['real'] == "aaa\nbb\n"
          and s2['lines'] == 3
          and s2['ends'] == [LINEEND_UNIX, LINEEND_UNIX, LINEEND_NONE])
    return _result(
        70,
        'set_text_line(1, "bb\\n") is False, replace_lines(1, 1, ["bb\\n"]) is True',
        'set_text_line safeguard (#6432)',
        'set_text_all("aaa\\n") (twice)',
        'set_text_line(1, "bb\\n") vs replace_lines(1, 1, ["bb\\n"])',
        'set_text_line: False, doc unchanged; replace_lines: True, '
        'real="aaa\\nbb\\n", ends=[LF,LF,NONE], lines=3',
        ok,
        'set_text_line: ' + _state_str_eol(s1) + ', ' + _ret_str(ret1)
        + '; replace_lines: ' + _state_str_eol(s2) + ', ' + _ret_str(ret2),
    )


def t71():
    """Rejected set_text_line adds no undo entry."""
    _set("aaa\r\nbbb\r\n")
    ret1 = ed.set_text_line(1, "xy")
    ret2 = ed.set_text_line(1, "xx\nyy")
    _do_undo()
    s_undo = _snap_eol()
    _do_redo()
    s_redo = _snap_eol()
    ok = (ret1 is None and ret2 is False
          and s_undo['real'] == "aaa\r\nbbb\r\n"
          and s_undo['lines'] == 3
          and s_redo['real'] == "aaa\r\nxy\r\n"
          and s_redo['lines'] == 3)
    return _result(
        71,
        'Undo/redo after rejected set_text_line: only the good edit is undone',
        'set_text_line safeguard (#6432)',
        'set_text_all("aaa\\r\\nbbb\\r\\n")',
        'set_text_line(1, "xy"); set_text_line(1, "xx\\nyy"); undo; redo',
        'undo: real="aaa\\r\\nbbb\\r\\n", lines=3; '
        'redo: real="aaa\\r\\nxy\\r\\n", lines=3',
        ok,
        'undo: ' + _state_str_eol(s_undo) + '; redo: ' + _state_str_eol(s_redo)
        + ', rets: ' + repr(ret1) + '/' + repr(ret2),
    )


def t72():
    """Control: set_text_line without EOLs works and returns None."""
    _set("aaa\r\nbbb\r\nccc")
    ret = ed.set_text_line(1, "xy")
    s = _snap_eol()
    ok = (ret is None
          and s['real'] == "aaa\r\nxy\r\nccc"
          and s['lines'] == 3
          and s['ends'] == [LINEEND_WIN, LINEEND_WIN, LINEEND_NONE])
    return _result(
        72,
        'Control: set_text_line(1, "xy") without EOLs - works, returns None',
        'set_text_line safeguard (#6432)',
        'set_text_all("aaa\\r\\nbbb\\r\\nccc")',
        'set_text_line(1, "xy")',
        'returns None, real="aaa\\r\\nxy\\r\\nccc", '
        'ends=[CRLF,CRLF,NONE], lines=3',
        ok,
        _state_str_eol(s) + ', ' + _ret_str(ret),
    )


def t73():
    """Control: set_text_line -1/-2 without EOLs keep old append behavior."""
    _set("aaa\n")
    ret1 = ed.set_text_line(-1, "x")
    s1 = _snap_eol()
    _set("aaa")
    _eol_default('lf')
    ret2 = ed.set_text_line(-2, "y")
    s2 = _snap_eol()
    ok = (ret1 is None and ret2 is None
          and s1['real'] == "aaa\nx\n"
          and s1['lines'] == 3
          and s1['ends'] == [LINEEND_UNIX, LINEEND_UNIX, LINEEND_NONE]
          and s2['real'] == "aaa\ny"
          and s2['lines'] == 2
          and s2['ends'] == [LINEEND_UNIX, LINEEND_NONE])
    return _result(
        73,
        'Control: set_text_line(-1, "x") / (-2, "y") - old appends, None returns',
        'set_text_line safeguard (#6432)',
        'set_text_all("aaa\\n"); set_text_all("aaa"); '
        'set_prop(PROP_NEWLINE, "lf")',
        'set_text_line(-1, "x"); set_text_line(-2, "y")',
        '-1: returns None, real="aaa\\nx\\n", ends=[LF,LF,NONE], lines=3; '
        '-2: returns None, real="aaa\\ny", ends=[LF,NONE], lines=2',
        ok,
        '-1: ' + _state_str_eol(s1) + ', ' + _ret_str(ret1)
        + '; -2: ' + _state_str_eol(s2) + ', ' + _ret_str(ret2),
    )


def t74():
    """Bare EOL text is rejected too; empty text is accepted (returns None)."""
    _set("aaa\n")
    before = _snap_eol()
    ret1 = ed.set_text_line(0, "\n")
    ret2 = ed.set_text_line(0, "\r\n")
    after_rejects = _snap_eol()
    ret3 = ed.set_text_line(0, "")
    s = _snap_eol()
    ok = (ret1 is False and ret2 is False and after_rejects == before
          and ret3 is None
          and s['real'] == "\n"
          and s['lines'] == 2
          and s['ends'] == [LINEEND_UNIX, LINEEND_NONE])
    return _result(
        74,
        'set_text_line(0, "\\n") and (0, "\\r\\n") are False; (0, "") is None',
        'set_text_line safeguard (#6432)',
        'set_text_all("aaa\\n")',
        'set_text_line(0, "\\n"); set_text_line(0, "\\r\\n"); '
        'set_text_line(0, "")',
        'first two: False, doc unchanged; third: None, real="\\n", '
        'ends=[LF,NONE], lines=2',
        ok,
        'rejects: ' + _state_str_eol(after_rejects)
        + ', rets: ' + repr(ret1) + '/' + repr(ret2)
        + '; empty: ' + _state_str_eol(s) + ', ' + _ret_str(ret3),
    )


def t75():
    """Recipe: multi-line text via replace_lines(...splitlines(True))."""
    _set("aaa\nbbb\nccc")
    _eol_default('lf')
    ret = ed.replace_lines(1, 1, "xx\nyy".splitlines(True))
    s = _snap_eol()
    ok = (ret is True
          and s['real'] == "aaa\nxx\nyy\nccc"
          and s['lines'] == 4
          and s['ends'] == [LINEEND_UNIX, LINEEND_UNIX, LINEEND_UNIX,
                            LINEEND_NONE])
    return _result(
        75,
        'replace_lines(1, 1, "xx\\nyy".splitlines(True)) - the multi-line recipe',
        'set_text_line safeguard (#6432)',
        'set_text_all("aaa\\nbbb\\nccc"); set_prop(PROP_NEWLINE, "lf")',
        'replace_lines(1, 1, "xx\\nyy".splitlines(True))',
        'returns True, real="aaa\\nxx\\nyy\\nccc", '
        'ends=[LF,LF,LF,NONE], lines=4  (set_text_line cannot do this)',
        ok,
        _state_str_eol(s) + ', ' + _ret_str(ret),
    )


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
    # 31-53: EOL marks in items (issue #6432 fix)
    t31, t32, t33, t34, t35, t36, t37, t38, t39, t40, t41, t42, t43,
    t44, t45, t46, t47, t48, t49, t50, t51, t52, t53,
    # 54-64: CR LF chars not at line end prohibited (issue #6432)
    t54, t55, t56, t57, t58, t59, t60, t61, t62, t63, t64,
    # 65-75: set_text_line with CR LF chars (issue #6432)
    t65, t66, t67, t68, t69, t70, t71, t72, t73, t74, t75,
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
        print('          https://github.com/Alexey-T/CudaText/issues/6432')
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
