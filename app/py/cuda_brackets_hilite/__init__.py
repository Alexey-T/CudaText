import os
import shutil
from cudatext import *
from .proc_brackets import *
from .proc_colors import *

MARKTAG = 10 #uniq value for all markers plugins
CANNOT_USE_SEL = False #cannot work if selection

NAME_INI = 'cuda_brackets_hilite.ini'
ini_app = os.path.join(app_path(APP_DIR_SETTINGS), NAME_INI)
ini_def = os.path.join(os.path.dirname(__file__), NAME_INI)

if not os.path.isfile(ini_app) and os.path.isfile(ini_def):
    shutil.copyfile(ini_def, ini_app)

COLOR_FONT = string_to_color(ini_read(ini_app, 'color', 'fore', '#000000'))
COLOR_BG = string_to_color(ini_read(ini_app, 'color', 'back', '#80c080'))

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
                ed.attr(MARKERS_DELETE_BY_TAG, MARKTAG)
    
            carets = ed.get_carets()
            if len(carets)!=1: 
                return
            x, y, x1, y1 = carets[0]
            if CANNOT_USE_SEL:
                if x1>=0:
                    return

            chars = get_chars()
            if not chars: return
            
            res = find_matching_bracket(ed, x, y, chars)
            if res is None:
                return
            x1, y1 = res
        
            ed.attr(MARKERS_ADD, MARKTAG, x, y, 1, COLOR_FONT, COLOR_BG)
            ed.attr(MARKERS_ADD, MARKTAG, x1, y1, 1, COLOR_FONT, COLOR_BG)
        finally:    
            self.entered=False
        
    def jump(self):
        self.do_find(True)
    def select(self):
        self.do_find(False)
        
    def do_find(self, is_jump):
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
        x1, y1 = res
        
        if is_jump:
            ed.set_caret(x1, y1)
            msg_status('Go to bracket')
        else:
            #select from (x,y) to (x1,y1)
            if (y1>y) or ((y1==y) and (x1>x)):
                ed.set_caret(x1+1, y1, x, y) #sel down
            else:
                ed.set_caret(x1, y1, x+1, y) #sel up
            msg_status('Selected to bracket')
