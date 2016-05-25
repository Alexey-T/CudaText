import json
import os
import collections
from cudatext import *

def get_installinf_of_module(mod):
    return os.path.join(app_path(APP_DIR_PY), mod, 'install.inf')

def get_initpy_of_module(mod):
    return os.path.join(app_path(APP_DIR_PY), mod, '__init__.py')

def get_name_of_module(mod):
    fn_ini = get_installinf_of_module(mod)
    return ini_read(fn_ini, 'info', 'title', mod)

def get_homepage_of_module(mod):
    fn_ini = get_installinf_of_module(mod)
    return ini_read(fn_ini, 'info', 'homepage', '')


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
    d = app_path(APP_DIR_PY)
    l = os.listdir(d)
    l = [s for s in l if not s.startswith('cuda_lint_') and not s.startswith('__')]
    l = [s for s in l if os.path.isfile(os.path.join(d, s, 'install.inf'))]
    return sorted(l)
    
def get_installed_choice():
    lmod = get_installed_list()
    ldesc = [get_name_of_module(l) for l in lmod]
    res = dlg_menu(MENU_LIST, '\n'.join(ldesc))
    if res is None:
        return None
    return lmod[res]
    
    