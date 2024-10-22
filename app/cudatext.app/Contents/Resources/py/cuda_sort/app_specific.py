from cudatext import *

def ed_set_text_all(lines):
    ed.replace_lines(0, ed.get_line_count()-1, lines)

def ed_get_text_all():
    l = [ed.get_text_line(i) for i in range(ed.get_line_count())]
    # exclude last empty line
    if l and not l[-1].strip():
        l = l[:-1]
    return l

def ed_insert_to_lines(lines, line1, line2):
    if ed.replace_lines(line1, line2, lines):
        cnt = ed.get_line_count()
        need_pos = (0, line1+len(lines))
        last_pos = (ed.get_line_len(cnt-1), cnt-1)
        if (need_pos[1], need_pos[0]) > (last_pos[1], last_pos[0]):
            need_pos = last_pos
        
        ed.set_caret(need_pos[0], need_pos[1], 0, line1)

def ed_set_tab_title(s):
    ed.set_prop(PROP_TAB_TITLE, s)

def ed_convert_tabs_to_spaces(s):
    return ed.convert(CONVERT_LINE_TABS_TO_SPACES, 0, 0, s)
   
def ed_get_sel_lines():
    return ed.get_sel_lines()
