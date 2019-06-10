import os
import webbrowser
from cudatext import *
from .proc_snip_insert import *

lexers_xml = ['XML', 'XML ^', 'XSL', 'XSLT']
lexers_css = ['CSS', 'SCSS', 'SASS', 'Sass', 'Stylus', 'LESS']

help_url = 'https://www.rj-texted.se/Help/Emmetcheatsheet.html'

def get_syntax():

    lexer = ed.get_prop(PROP_LEXER_CARET)
    if lexer in lexers_xml:
        return 'xml'
    elif lexer in lexers_css:
        return 'css'
    else:
        return 'html'

def get_profile():

    return ''


def find_abr():

    x, y, x1, y1 = ed.get_carets()[0]
    text = ed.get_text_line(y)
    if not text: return
    text = text[:x]
    if not text: return

    n = app_proc(PROC_EMMET_GET_POS, (text, len(text)))
    text = text[n:]
    return text


def do_insert_result(x0, y0, x1, y1, text):

    ed.set_caret(x0, y0)
    ed.delete(x0, y0, x1, y1)

    lines = text.splitlines()
    insert_snip_into_editor(ed, lines)


def tabstop(cnt):

    if cnt<9:
        return '${%d}'%(cnt+1)
    if cnt==9:
        return '${0}'
    return ''


def do_expand_abbrev(abr):

    res = app_proc(PROC_EMMET_EXPAND, (abr, get_syntax()))
    if res and res[0]:
        s = res[0]
        cnt = 0
        while True:
            n = s.find('|')
            if n<0:
                break
            s = s[:n] + tabstop(cnt) + s[n+1:]
            cnt += 1

        return s

    msg_status('Cannot expand Emmet abbreviation: '+abr)


class Command:

    def profiles(self):

        n = dlg_menu(MENU_LIST, '\n'.join(profiles))
        if n is None: return
        item = profiles[n]
        ini_write(fn_ini, ini_section, ini_key_profile, item)


    def help(self):

        webbrowser.open_new_tab(help_url)
        msg_status('Opened browser')


    def wrap_abbrev(self):

        x0, y0, x1, y1 = ed.get_carets()[0]
        if (y0, x0)>(y1, x1):
            x0, y0, x1, y1 = x1, y1, x0, y0

        text_sel = ed.get_text_sel().replace('\n', '\r\n')
        if not text_sel:
            msg_status('Text not selected')
            return

        abr = dlg_input('Emmet abbreviation:', 'div')
        if not abr:
            return

        res = app_proc(PROC_EMMET_WRAP, (abr, get_syntax(), text_sel))
        if res and res[0]:
            do_insert_result(x0, y0, x1, y1, res[0])


    def expand_abbrev(self):

        abr = find_abr()
        if not abr:
            msg_status('Cannot find Emmet abbreviation')
            return

        text = do_expand_abbrev(abr)
        if not text:
            return

        x0, y0, x1, y1 = ed.get_carets()[0]
        xstart = max(0, x0-len(abr))

        do_insert_result(xstart, y0, x0, y0, text)
