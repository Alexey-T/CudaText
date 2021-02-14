import os
import webbrowser
from cudatext import *
from cudax_lib import get_translation
from .proc_snip_insert import *
from .dlg_emmet import DialogEmmet

_   = get_translation(__file__)  # I18N

lexers_xml = ['XML', 'XML ^', 'XSL', 'XSLT']
lexers_css = ['CSS', 'SCSS', 'SASS', 'Sass', 'Stylus', 'LESS']

filename_help = os.path.join(os.path.dirname(__file__), 'help.html')


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

    crt = ed.get_carets()
    # don't work with mul-carets
    if len(crt)>1:
        return
    x, y, x1, y1 = crt[0]
    # don't work with selection
    if y1>=0:
        return
    text = ed.get_text_line(y)
    if not text: return
    text = text[:x]
    if not text: return

    n = emmet(EMMET_GET_POS, text, len(text))
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

    res = emmet(EMMET_EXPAND, abr, get_syntax())
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

    msg_status(_('Cannot expand Emmet abbreviation: ')+abr)


class Command:

    dlg = None

    def profiles(self):

        n = dlg_menu(DMENU_LIST, '\n'.join(profiles))
        if n is None: return
        item = profiles[n]
        ini_write(fn_ini, ini_section, ini_key_profile, item)


    def help(self):

        webbrowser.open_new_tab('file://'+filename_help)
        msg_status(_('Opened browser'))


    def wrap_abbrev(self):

        if ed.get_prop(PROP_RO): return

        x0, y0, x1, y1 = ed.get_carets()[0]
        if (y0, x0)>(y1, x1):
            x0, y0, x1, y1 = x1, y1, x0, y0

        text_sel = ed.get_text_sel()
        if not text_sel:
            msg_status(_('Text not selected'))
            return

        abr = dlg_input(_('Emmet abbreviation:'), 'div')
        if not abr:
            return

        res = emmet(EMMET_WRAP, abr, get_syntax(), text_sel)
        if res:
            do_insert_result(x0, y0, x1, y1, res)


    def expand_abbrev(self):

        if ed.get_prop(PROP_RO): return
        self.expand_ex(True)

    def expand_ex(self, with_msg):

        abr = find_abr()
        if not abr:
            if with_msg:
                msg_status(_('Cannot find Emmet abbreviation'))
            return

        text = do_expand_abbrev(abr)
        if not text:
            return

        x0, y0, x1, y1 = ed.get_carets()[0]
        xstart = max(0, x0-len(abr))

        do_insert_result(xstart, y0, x0, y0, text)
        return False

    def insert_text_at_caret(self, text):

        x0, y0, x1, y1 = ed.get_carets()[0]
        xstart = x0
        do_insert_result(xstart, y0, x0, y0, text)

    def dialog(self):

        if not self.dlg:
            self.dlg = DialogEmmet(do_expand_abbrev, self.insert_text_at_caret)
        self.dlg.show()

    def on_key(self, ed_self, key, state):

        if key==9 and state=='':
            return self.expand_ex(False)