import json
import os
from cudatext import *

fn_plugins = os.path.join(app_path(APP_DIR_SETTINGS), 'plugins.json')

def get_initpy_of_module(mod):
    return os.path.join(app_path(APP_DIR_PY), mod, '__init__.py')

def get_name_of_module(mod):
    fn_ini = os.path.join(app_path(APP_DIR_PY), mod, 'install.inf')
    return ini_read(fn_ini, 'info', 'title', mod)

def do_remove_registering(mod):
    with open(fn_plugins, 'r') as f:
        d = json.load(f)
        
    if 'commands' in d:
        if mod in d['commands']:
            del d['commands'][mod]
    if 'events' in d:
        if mod in d['events']:
            del d['events'][mod]
    
    text = json.dumps(d, indent=2)
    with open(fn_plugins, 'w') as f:
        f.write(text)
        

def do_remove_module(mod):
    dir_mod = os.path.join(app_path(APP_DIR_PY), mod)
    dir_trash = os.path.join(app_path(APP_DIR_PY), '__trash')
    dir_dest = os.path.join(dir_trash, mod)
    while os.path.isdir(dir_dest):
        dir_dest += '_'
        
    if not os.path.isdir(dir_mod):
        msg_box('Cannot find dir: '+dir_mod, MB_OK)
        return
    if not os.path.isdir(dir_trash):
        os.mkdir(dir_trash)

    try:
        os.rename(dir_mod, dir_dest)
    except OSError:
        msg_box('Cannot remove dir: '+dir_mod, MB_OK)
        return
    return True
    

def get_installed_list():
    d = json.load(open(fn_plugins, 'r'))
    
    lst = []
    if 'commands' in d:
        lst += list(d['commands'].keys()) 
    if 'events' in d:
        lst += list(d['events'].keys())
    
    return sorted(list(set(lst)))
    
def get_installed_choice():
    lmod = get_installed_list()
    ldesc = [get_name_of_module(l) for l in lmod]
    res = dlg_menu(MENU_LIST, '\n'.join(ldesc))
    if res is None:
        return None
    return lmod[res]
    
    