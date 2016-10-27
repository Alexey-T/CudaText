import os
from cudatext import *

DIR_NEWDOC = os.path.join(app_path(APP_DIR_DATA), 'newdoc')

class Command:
    def menu(self):
        if not os.path.isdir(DIR_NEWDOC): return
        files = os.listdir(DIR_NEWDOC)
        if not files:
            msg_status('No files in data/newdoc')
            return
        
        files = [(item, lexer_proc(LEXER_DETECT, item)) for item in files]
        
        lexers = sorted(list(set([item[1] for item in files])))
        #print('Templates found for:', ', '.join(lexers))
        
        res = dlg_menu(MENU_LIST, '\n'.join(lexers))
        if res is None: return
        
        lexer = lexers[res]
        files = sorted([item[0] for item in files if item[1]==lexer])
        if not files: return
        
        if len(files)==1:
            fn = files[0]
        else:
            res = dlg_menu(MENU_LIST, '\n'.join(files))
            if res is None: return
            fn = files[res]
            
        lexer = lexer_proc(LEXER_DETECT, fn)
        msg_status('New file from "%s", lexer "%s"' % (fn, lexer))

        fn = os.path.join(DIR_NEWDOC, fn)
        file_open('')
        ed.set_text_all(open(fn).read())
        
        if lexer:
            ed.set_prop(PROP_LEXER_FILE, lexer)
