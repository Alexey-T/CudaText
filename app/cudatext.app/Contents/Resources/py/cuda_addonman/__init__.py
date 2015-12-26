import os
import shutil
from cudatext import *
from .workremote import *
from .workremote2 import get_remote_addons_list, get_remote_download_all_list
from .worklocal import *
from urllib.parse import unquote

INIT_DL_DIR = os.path.expanduser('~')+os.sep+'CudaText_addons'


class Command:
    def do_download_all(self):
        dir_dl = dlg_input('Dir to save files:', INIT_DL_DIR)
        if not dir_dl: return
        if not os.path.isdir(dir_dl):
            os.mkdir(dir_dl)
        if not os.path.isdir(dir_dl):
            msg_box('Cannot create dir: '+dir_dl, MB_OK+MB_ICONERROR)
            return
    
        msg_status('Downloading list...')
        items = get_remote_download_all_list()
        msg_status('')
        if not items:
            msg_status('Cannot download list')
            return
            
        err = 0
        stopped = False
        app_proc(PROC_SET_ESCAPE, '0')
            
        for (i, url) in enumerate(items):
            if app_proc(PROC_GET_ESCAPE, '')==True:
                app_proc(PROC_SET_ESCAPE, '0')
                if msg_box('Stop downloading?', MB_OKCANCEL+MB_ICONQUESTION)==ID_OK:
                    stopped = True
                    break    
        
            msg_status('Downloading file: %d/%d'%(i+1, len(items)))
            try:
                url = urllib.request.urlopen(url).geturl()
            except:
                err += 1
                print('Cannot resolve URL: '+url)
                continue
                
            name = unquote(url.split('/')[-1])
            dir = os.path.join(dir_dl, name.split('.')[0])
            if not os.path.isdir(dir):
                os.mkdir(dir)
            fn = os.path.join(dir, name)
            get_url(url, fn)
            if not os.path.isfile(fn):
                err += 1
                print('Cannot download file: '+url)
                continue
                
        text = 'Download done' if not stopped else 'Download stopped'
        if err>0:
            text += '\nErrors occured, see Python console' 
        msg_box(text, MB_OK+MB_ICONINFO)
        

    def do_install_addon(self):
        msg_status('Downloading list...')
        items = get_remote_addons_list()
        msg_status('')
        if not items:
            msg_status('Cannot download list')
            return
        names = [l[1] for l in items]
        res = dlg_menu(MENU_LIST, '\n'.join(names))
        if res is None: return
        url = items[res][0]
        #resolve url
        msg_status('Downloading file...')
        try:
            res = urllib.request.urlopen(url)
            url = res.geturl()
        except:
            msg_status('Cannot resolve URL')
            return
        #download
        fn = get_plugin_zip(url)
        if not os.path.isfile(fn):
            msg_status('Cannot download file')
            return
        msg_status('Opened downloaded file')
        file_open(fn)
        

    def do_install_lexer(self):
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
        
