import sys
import os

from cudatext import *
from . import csswork

TEXT_CSS_CLASS = 'css_class'
TEXT_CSS_ID = 'css_id'
MSG_CSS_WORK = 'CSS completion'
LOG = 0

def is_word(s):
    return s.isalnum() or s == '-' or s == '_'

def get_css_text(text):
    css_text = ''
    css_names = get_css_filenames(text)
    if css_names:
        if LOG: print('css_names:', css_names)
        for name in css_names:
            with open(name, encoding='cp437') as f:
                css_text += f.read() + '\n'

    css_style = csswork.html_find_style_content(text)
    if LOG:
        if css_style:
            print('css_text found')

    css_text += css_style + '\n'
    return css_text 

def get_css_filenames(text):
    fn_ed = ed.get_filename()
    if not fn_ed:
        return ''
    dir_ed = os.path.dirname(fn_ed)

    names = csswork.html_find_css_filenames(text)
    if not names:
        return ''
        
    names = [os.path.join(dir_ed, item) for item in names]
    names = [item for item in names if os.path.isfile(item)] 
    return names

def get_acp_text(acp_list, quote, need_quote_end, mode_class):
    quote_begin = '' if quote != '' else '"'
    quote_end = quote if quote != '' else '"'
    if not need_quote_end:
        quote_end = ''
    desc = TEXT_CSS_CLASS if mode_class else TEXT_CSS_ID    
    items = [desc + '|' + quote_begin + item + quote_end + '|\n' for item in acp_list]
    return ''.join(items)
    
def get_last_text(len):
    x0, y0, x1, y1 = ed.get_carets()[0]
    return ed.get_text_substr(max(0, x0-len), y0, x0, y0)
        

class Command:
    def on_complete(self, ed_self):
        posx, posy, endx, endy = ed.get_carets()[0]
        posx_orig, posy_orig = posx, posy
        
        while is_word(ed.get_text_substr(posx, posy, posx+1, posy)):
            posx += 1
        quote = ed.get_text_substr(posx, posy, posx+1, posy)
        need_quote_end = not (quote == '"' or quote == "'")
        
        acp_len = 0
        posx, posy = posx_orig, posy_orig
        while posx>0 and is_word(ed.get_text_substr(posx-1, posy, posx, posy)):
            acp_len += 1
            posx -= 1
        ed.set_caret(posx, posy)
    
        quote = get_last_text(1)
        is_quote = quote == '"' or quote == "'"
        if not is_quote:
            if get_last_text(1) != '=':
                return False
            is_quote = True
            quote = '"'
            ed.insert(posx, posy, quote)
            posx_orig += 1
            if need_quote_end:
                need_quote_end = False
                ed.insert(posx, posy, quote)
                ed.set_caret(posx+1, posy)
        
        need_text = 'class=' + quote
        last_text = get_last_text(len(need_text))
        is_work_class = last_text.upper() == need_text.upper()

        need_text = 'id=' + quote
        last_text = get_last_text(len(need_text))
        is_work_id = last_text.upper() == need_text.upper()
        
        if not (is_work_class or is_work_id):
            return False

        text = ed.get_text_all()
        text_css = get_css_text(text)
        if not text_css:
            return False
            
        tagname = csswork.html_find_tagname(ed.get_text_line(posy), posx)
        if not tagname:
            if LOG: print('tag not found')
            return False
        if LOG: print('tag:', tagname)
        
        acp_list = csswork.css_find_classes(text_css, tagname, is_work_class)
        if not acp_list:
            return False
            
        acp_text = get_acp_text(acp_list, quote, need_quote_end, is_work_class)
        ed.set_caret(posx_orig, posy_orig)
        ed.complete(acp_text, acp_len, 0)
                
        msg_status(MSG_CSS_WORK + ': <' + tagname + '>')
        return True            
