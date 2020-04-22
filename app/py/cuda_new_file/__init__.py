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
        
        infos = []
        for item in files:
            lex = lexer_proc(LEXER_DETECT, item)
            if isinstance(lex, tuple):
                for l in lex:
                    infos += [(item, l)]
            else:
                infos += [(item, lex)]
        
        lexers = sorted(list(set([item[1] for item in infos if item[1]])))
        if not lexers: return
        
        res = dlg_menu(MENU_LIST, lexers, caption='Templates')
        if res is None: return
        
        lexer = lexers[res]
        files = sorted([item[0] for item in infos if item[1]==lexer])
        if not files: return
        
        if len(files)==1:
            fn = files[0]
        else:
            res = dlg_menu(MENU_LIST, files, caption='Templates: %s'%lexer)
            if res is None: return
            fn = files[res]
            
        msg_status('New file from "%s", lexer "%s"' % (fn, lexer))

        fn = os.path.join(DIR_NEWDOC, fn)
        file_open('')
        ed.set_text_all(open(fn).read())
        
        if lexer:
            ed.set_prop(PROP_LEXER_FILE, lexer)
