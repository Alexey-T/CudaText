import os
import webbrowser
from cudatext import *
from .proc_snip_insert import *

lexers_xml = ['XML', 'XSL', 'XSLT']
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
    

def do_find_expand():

    x, y, x1, y1 = ed.get_carets()[0]
    text = ed.get_text_line(y)
    if not text: return
    text = text[:x]
    if not text: return
    
    n = app_proc(PROC_EMMET_GET_POS, (text, len(text)))
    text = text[n:]
    return text
    
    
def do_insert_result(x0, y0, x1, y1, text, text_insert):

    if text_insert:
        for i in [1,2,3,4,5,6,7,8,9,0]:
            text_rep = '${'+str(i)+'}'
            if text_rep in text:
                text = text.replace(text_rep, '${'+str(i)+':'+text_insert+'}', 1)
                break
    
    ed.delete(x0, y0, x1, y1)
    ed.set_caret(x0, y0)
    
    lines = text.splitlines()
    insert_snip_into_editor(ed, lines)


def do_expand_abbrev(text_ab):

    msg_status('Expanding: %s' % text_ab)
    res = app_proc(PROC_EMMET_EXPAND, (text_ab, get_syntax()))
    if res and res[0]:
        return res[0]
    msg_status('Cannot expand Emmet abbreviation: '+text_ab)


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
        #sort coords
        if (y1>y0) or ((y1==y0) and (x1>x0)):
            pass
        else:
            x0, y0, x1, y1 = x1, y1, x0, y0
        
        text_sel = ed.get_text_sel()
        if not text_sel:
            msg_status('Text not selected')
            return
            
        text_ab = dlg_input('Emmet abbreviation:', 'div')
        if not text_ab:
            return
        
        text = do_expand_abbrev(text_ab)
        if not text: return
                                               
        do_insert_result(x0, y0, x1, y1, text, text_sel)
        

    def expand_abbrev(self):

        abr = do_find_expand()
        if not abr:
            msg_status('Cannot find Emmet abbreviation')
            return

        text = do_expand_abbrev(abr)
        if not text:
            return
            
        x0, y0, x1, y1 = ed.get_carets()[0]
        xstart = max(0, x0-len(abr))

        do_insert_result(xstart, y0, x0, y0, text, '')
