import json
import os
import collections
import zipfile
import tempfile
from cudatext import *

README_NAMES = (
    'readme.txt',
    'readme.html',
    'readme.htm',
    'readme.md',
    'README.md',
    'readme.rst',
    'README.rst',
    )

HISTORY_NAMES = (
    'history.txt',
    'history.html',
    'history.htm',
    'history.md',
    'history.rst',
    'history',
)

DATA_DIRS = (
    ('autocomplete', '.acp'),
    ('lang', '.ini'),
    ('newdoc', ''),
    ('snippets', ''),
    ('themes', ''),
    )


def get_module_name_from_zip_filename(zip_fn):
    temp_dir = tempfile.gettempdir()
    z = zipfile.ZipFile(zip_fn, 'r')
    z.extract('install.inf', temp_dir)
    z.close()
    fn = os.path.join(temp_dir, 'install.inf')
    if os.path.isfile(fn):
        return ini_read(fn, 'info', 'subdir', '')

def get_readme_of_module(mod):
    for name in README_NAMES:
        fn = os.path.join(app_path(APP_DIR_PY), mod, 'readme', name)
        if os.path.isfile(fn):
            return fn
        fn = os.path.join(app_path(APP_DIR_PY), mod, name)
        if os.path.isfile(fn):
            return fn

def get_history_of_module(mod):
    for name in HISTORY_NAMES:
        fn = os.path.join(app_path(APP_DIR_PY), mod, 'readme', name)
        if os.path.isfile(fn):
            return fn
        fn = os.path.join(app_path(APP_DIR_PY), mod, name)
        if os.path.isfile(fn):
            return fn


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
    """
    move folder for py-module mod, into py/__trash
    (make copy with _ suffix if nessesary)
    """
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


def do_remove_data(fn):
    """
    move filename/dirname from "data/..." to "data/__trash"
    add suffix _ if nessesary
    """
    dir_trash = os.path.join(app_path(APP_DIR_DATA), '__trash')
    fn_to = os.path.join(dir_trash, os.path.basename(fn))
    while os.path.exists(fn_to):
        fn_to += '_'

    if not os.path.isdir(dir_trash):
        os.mkdir(dir_trash)

    try:
        os.rename(fn, fn_to)
        print('Moved "%s" to "%s"' % (fn, fn_to))
    except OSError:
        msg_box('Cannot move file/dir:\n%s\nto:\n%s' % (fn, fn_to), MB_OK)
        return
    return True


def get_installed_list():
    """
    gets list of py-modules inside "py"
    """
    d = app_path(APP_DIR_PY)
    l = os.listdir(d)
    l = [s for s in l if not s.startswith('__')]
    l = [s for s in l if os.path.isfile(os.path.join(d, s, 'install.inf'))]
    return sorted(l)

def get_installed_choice(caption, exclude_list=None):
    """
    gets module of addon, from menu of installed addons
    """
    lmod = get_installed_list()
    if exclude_list:
        lmod = [i for i in lmod if not i in exclude_list]
    ldesc = [get_name_of_module(l) for l in lmod]
    res = dlg_menu(MENU_LIST, ldesc, caption=caption)
    if res is None:
        return None
    return lmod[res]

def get_installed_data_list():
    """
    gets list of filenames+dirnames inside "data", only 1 level deep
    """
    res = []
    dir_data = os.path.join(app_path(APP_DIR_DATA))
    for dir_item in DATA_DIRS:
        dir1 = os.path.join(dir_data, dir_item[0])
        names = os.listdir(dir1)
        #filter out incorrect ext
        if dir_item[1]:
            names = [name for name in names if name.endswith(dir_item[1])]
        names = [os.path.join(dir1, name) for name in names]
        res += names
    return sorted(res)


def get_installed_data_choice():
    """
    gets choice for get_installed_data_list()
    """
    names = get_installed_data_list()
    dir_data = os.path.join(app_path(APP_DIR_DATA))
    skip_len = len(dir_data)+1
    desc = [item[skip_len:] for item in names]
    res = dlg_menu(MENU_LIST, desc, caption='Remove data file')
    if res is None:
        return None
    return names[res]
