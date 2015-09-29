import os
from cudatext import *
from .workremote import *
from .worklocal import *


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
        m = get_installed_choice()
        if m is None:
            return
        if msg_box('Remove plugin: '+get_name_of_module(m), MB_OKCANCEL+MB_ICONQUESTION)!=ID_OK:
            return
        do_remove_registering(m)
        if do_remove_module(m)==True:
            msg_box('Removed, restart program to see changes', MB_OK+MB_ICONINFO)

    def do_edit(self):
        m = get_installed_choice()
        if m is None: return
        fn = get_initpy_of_module(m)
        file_open(fn)
        msg_status('Opened: '+fn)
        
