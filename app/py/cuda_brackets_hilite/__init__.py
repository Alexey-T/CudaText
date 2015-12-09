import os
import shutil
from cudatext import *
from .proc_brackets import *

NAME_INI = 'cuda_brackets_hilite.ini'
ini_app = os.path.join(app_path(APP_DIR_SETTINGS), NAME_INI)
ini_def = os.path.join(os.path.dirname(__file__), NAME_INI)

if not os.path.isfile(ini_app) and os.path.isfile(ini_def):
    shutil.copyfile(ini_def, ini_app)

COLOR_FONT = eval(ini_read(ini_app, 'colors', 'fore', '0x000000'))
COLOR_BG = eval(ini_read(ini_app, 'colors', 'back', '0x80c080'))


prev_lexer = None
prev_chars = ''

def get_chars():
    global prev_lexer
    global prev_chars
    
    lex = ed.get_prop(PROP_LEXER_CARET)
    if prev_lexer is not None:
        if lex==prev_lexer:
            return prev_chars

    defval = ini_read(ini_app, 'brackets', 'default', '')
    val = ini_read(ini_app, 'brackets', lex, defval)
    
    prev_lexer = lex
    prev_chars = val
    #print('chars for %s: "%s"'%(lex, val))
    return val
    

class Command:
    entered=False
    
    def config(self):
        if os.path.isfile(ini_app):
            file_open(ini_app)
        else:
            msg_box('Cannot find config: '+ini_app, MB_OK)

    def on_caret(self, ed_self):
        if self.entered: return
        self.entered=True
        
        try:
            marks = ed.attr(MARKERS_GET)
            if marks:
                ed.attr(MARKERS_DELETE_ALL)
    
            carets = ed.get_carets()
            if len(carets)!=1: 
                return
            x, y, x1, y1 = carets[0]
            if x1>=0:
                return

            chars = get_chars()
            if not chars: return
            
            res = find_matching_bracket(ed, x, y, chars)
            if res is None:
                return
            x1, y1 = res
        
            ed.attr(MARKERS_ADD, x, y, 1, COLOR_FONT, COLOR_BG)
            ed.attr(MARKERS_ADD, x1, y1, 1, COLOR_FONT, COLOR_BG)
        finally:    
            self.entered=False
        

    def run(self):
        carets = ed.get_carets()
        if len(carets)!=1:
            msg_status('Cannot goto bracket if multi-carets')
            return
        
        x, y, x1, y1 = carets[0]
        if x1>=0:
            msg_status('Cannot goto bracket if selection')
            return
        
        chars = get_chars()
        if not chars: return
        
        res = find_matching_bracket(ed, x, y, chars)
        if res is None:
            msg_status('Cannot find matching bracket')
            return
        x, y = res
        ed.set_caret(x, y)
        msg_status('Go to bracket done')
        
