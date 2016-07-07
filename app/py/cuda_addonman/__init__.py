import os
import shutil
import json
import collections
import webbrowser
from cudatext import *
from .work_local import *
from .work_remote import *
from .work_dlg_config import *
from urllib.parse import unquote

dir_for_all = os.path.join(os.path.expanduser('~'), 'CudaText_addons')
fn_config = os.path.join(app_path(APP_DIR_SETTINGS), 'cuda_addonman.json')

ch_user = []
ch_def = [
  'https://raw.githubusercontent.com/Alexey-T/CudaText-registry/master/registry-addons.txt',
  'https://raw.githubusercontent.com/Alexey-T/CudaText-registry/master/registry-lexers.txt',
  'https://raw.githubusercontent.com/kvichans/CudaText-registry/master/registry-addons.txt',
  ]
op_readme = True
  

class Command:
    def __init__(self):
        global ch_user
        global op_readme
        if os.path.isfile(fn_config):
            op = json.loads(open(fn_config).read(), object_pairs_hook=collections.OrderedDict)
            ch_user = op.get('channels_user', ch_user)
            op_readme = op.get('suggest_readme', True)
        

    def do_config(self):
        global ch_def, ch_user, op_readme
        res = dlg_config(ch_def, ch_user, op_readme)
        if res is None: return
        (ch_user, op_readme) = res
        print('Now channels_user:', ch_user) 
          
        op = {}
        op['channels_user'] = ch_user
        op['suggest_readme'] = op_readme
        with open(fn_config, 'w') as f:
            f.write(json.dumps(op, indent=4))
        

    def do_download_all(self):
        global dir_for_all
        res = dlg_input('Folder to save files:', dir_for_all)
        if not res: return
        dir_for_all = res
        if not os.path.isdir(dir_for_all):
            os.mkdir(dir_for_all)
        if not os.path.isdir(dir_for_all):
            msg_box('Cannot create dir: '+dir_for_all, MB_OK+MB_ICONERROR)
            return
    
        msg_status('Downloading list...')
        items = get_remote_addons_list(ch_def+ch_user)
        msg_status('')
        if not items:
            msg_status('Cannot download list')
            return
            
        err = 0
        stopped = False
        app_proc(PROC_SET_ESCAPE, '0')
            
        for (i, item) in enumerate(items):
            url = item[0]
            title = item[1]
            if app_proc(PROC_GET_ESCAPE, '')==True:
                app_proc(PROC_SET_ESCAPE, '0')
                if msg_box('Stop downloading?', MB_OKCANCEL+MB_ICONQUESTION)==ID_OK:
                    stopped = True
                    break    
        
            #must use msg_status(.., True)
            msg_status('Downloading file: %d/%d'%(i+1, len(items)), True)
            
            while True:
                try:
                    url = urllib.request.urlopen(url).geturl()
                    break
                except:
                    if msg_box('Cannot resolve URL:\n'+url+'\nRetry?', MB_RETRYCANCEL)==ID_CANCEL:
                        err += 1
                        break
                
            name = unquote(url.split('/')[-1])
            dir = os.path.join(dir_for_all, name.split('.')[0])
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
        items = get_remote_addons_list(ch_def+ch_user)
        msg_status('')
        if not items:
            msg_status('Cannot download list')
            return
        names = [l[1]+'\t'+l[2] for l in items]
        res = dlg_menu(MENU_LIST_ALT, '\n'.join(names))
        if res is None: return
        url = items[res][0]
        
        #check for CudaLint
        if 'linter.' in url:
            if not "cuda_lint" in get_installed_list():
                msg_box('This is linter, it needs CudaLint plugin installed. Install CudaLint first.', MB_OK+MB_ICONWARNING)
                return
        
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
        
        #suggest readme
        if op_readme:
            m = get_module_name_from_zip_filename(fn)
            if m:
                fn = get_readme_of_module(m)
                if fn:
                    if msg_box('Open plugin\'s readme file?', MB_OKCANCEL+MB_ICONQUESTION)==ID_OK:
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
        if do_remove_module(m)==True:
            msg_box('Removed, restart program to see changes', MB_OK+MB_ICONINFO)

    def do_edit(self):
        m = get_installed_choice()
        if m is None: return
        fn = get_initpy_of_module(m)
        file_open(fn)
        msg_status('Opened: '+fn)
        
    def do_homepage(self):
        m = get_installed_choice()
        if m is None: return
        s = get_homepage_of_module(m)
        if s:
            webbrowser.open_new_tab(s)
            msg_status('Opened browser: '+s)
        else:
            msg_box('Plugin "%s" doesn\'t have "homepage" field in install.inf' % \
              get_name_of_module(m), MB_OK+MB_ICONWARNING)

    def do_readme(self):
        m = get_installed_choice()
        if m is None: return
        s = get_readme_of_module(m)
        if s:
            file_open(s)
        else:
            msg_status('Plugin "%s" doesn\'t have readme' % get_name_of_module(m))
