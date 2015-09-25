import os
from cudatext import *
from .workremote import get_url, get_item_url, get_avail_list, get_plugin_zip

class Command:
    def do_install(self):
        #get only lexer items
        items = get_avail_list()
        items = [l for l in items if l[0].startswith('Lexer:')]
        
        text = '\n'.join(l[0] for l in items)
        res = dlg_menu(MENU_LIST, text)
        if res is None: return
        res = items[res]
        url = get_item_url(res[2])
        fn = get_plugin_zip(url)
        if not os.path.isfile(fn): return
        file_open(fn) 
        
    def do_remove(self):
        msg_box('remov', MB_OK)
