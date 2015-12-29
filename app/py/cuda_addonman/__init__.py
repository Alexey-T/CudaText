import os
import shutil
import json
from cudatext import *
from .work_local import *
from .work_remote import *
from urllib.parse import unquote

INIT_DL_DIR = os.path.expanduser('~')+os.sep+'CudaText_addons'
CONFIG_FILE = os.path.join(app_path(APP_DIR_SETTINGS), 'cuda_addonman.json')

URL_PLUG = 'http://sourceforge.net/projects/cudatext/files/addons/plugins/'
URL_SNIP = 'http://sourceforge.net/projects/cudatext/files/addons/snippets/'
URL_LEX = 'http://sourceforge.net/projects/synwrite-addons/files/Lexers/'
channels = [URL_PLUG, URL_SNIP, URL_LEX]


class Command:
    def __init__(self):
        global channels
        if os.path.isfile(CONFIG_FILE):
            f = open(CONFIG_FILE)
            op = json.loads(f.read())
            channels = op.get('channels', channels)
            print(channels) 
        

    def do_config(self):
        global channels
        
        id_memo = 1
        id_ok = 2
        
        c1 = chr(1)
        res = dlg_custom('Addons Manager config', 600, 240, '\n'.join([]+
          [c1.join(['type=label', 'pos=6,6,594,0', 'cap=Addons channels:'])]+
          [c1.join(['type=memo', 'pos=6,26,594,204', 'val='+'\t'.join(channels)])]+
          [c1.join(['type=button', 'pos=400,210,494,0', 'cap=OK'])]+ 
          [c1.join(['type=button', 'pos=500,210,594,0', 'cap=Cancel'])] 
          ))
        if res is None: return
        id, text = res
        if id!=id_ok: return
          
        channels = text.splitlines()[id_memo].split('\t')
        channels = [s for s in channels if s]
        print('Now channels:', channels) 
          
        op = {}
        op['channels'] = channels
        f = open(CONFIG_FILE, 'w')
        f.write(json.dumps(op, indent=4))
        

    def do_download_all(self):
        dir_dl = dlg_input('Dir to save files:', INIT_DL_DIR)
        if not dir_dl: return
        if not os.path.isdir(dir_dl):
            os.mkdir(dir_dl)
        if not os.path.isdir(dir_dl):
            msg_box('Cannot create dir: '+dir_dl, MB_OK+MB_ICONERROR)
            return
    
        msg_status('Downloading list...')
        items = get_remote_addons_list(channels)
        msg_status('')
        if not items:
            msg_status('Cannot download list')
            return
            
        err = 0
        stopped = False
        app_proc(PROC_SET_ESCAPE, '0')
            
        for (i, (url, title)) in enumerate(items):
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
        items = get_remote_addons_list(channels)
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
        
