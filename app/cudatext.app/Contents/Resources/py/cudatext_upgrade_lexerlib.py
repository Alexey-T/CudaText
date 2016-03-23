from cudatext import *
import os

MSG = """
cannot find .lcf lexer files.
convert lexer-lib file to .lcf files? 
this must run once for new CudaText 1.3.2. 
lcf files will appear in data/lexlib."""

class Command:
    def run(self):
        if msg_box(MSG, MB_OKCANCEL+MB_ICONWARNING)!=ID_OK: return
        file_dir = app_path(APP_DIR_DATA)+os.sep+'lexlib'
        
        ss = lexer_proc(LEXER_GET_LIST, '')
        ss = ss.splitlines()
        for lexer in ss:
            fn_lexer = file_dir+os.sep+lexer+'.lcf'
            fn_ini = file_dir+os.sep+lexer+'.cuda-lexmap'
            lexer_proc(LEXER_EXPORT, lexer+';'+fn_lexer)
            links = lexer_proc(LEXER_GET_LINKS, lexer)
            if links:
                for (i, link) in enumerate(links.splitlines()):
                    ini_write(fn_ini, 'ref', str(i), link)

        msg_box('converted. made %d lcf files. you may delete file data/lexlib/*.lxl by hands.'% len(ss), MB_OK)
        