import os
from cudatext import *

def ed_set_text_all(lines):
    ed.set_text_all('\n'.join(lines)+'\n')

def ed_get_text_all():
    l = [ed.get_text_line(i) for i in range(ed.get_line_count())]
    # exclude last empty line
    if l and not l[-1].strip():
        l = l[:-1]
    return l

def ed_insert_to_lines(lines, line1, line2):
    cnt = ed.get_line_count()
    fix_last_empty = line2 == cnt-1 and ed.get_text_line(cnt-1) != ''

    if ed.replace_lines(line1, line2, lines):
        if fix_last_empty:
            cnt = ed.get_line_count()
            if cnt >= 2:
                if ed.get_text_line(cnt-1) == '':
                    ed.delete(ed.get_line_len(cnt-2), cnt-2, 0, cnt-1)

        cnt = ed.get_line_count()
        ed.set_caret(ed.get_line_len(cnt-1), cnt-1, 0, line1)

def ed_set_tab_title(s):
    ed.set_prop(PROP_TAB_TITLE, s)

def ed_convert_tabs_to_spaces(s):
    return ed.convert(CONVERT_LINE_TABS_TO_SPACES, 0, 0, s)
   
def msg_show_error(s):
    msg_box(s, MB_OK+MB_ICONERROR)

def ed_get_sel_lines():
    return ed.get_sel_lines()
