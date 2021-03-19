import json
import os
import collections
import zipfile
import tempfile
import configparser
from cudatext import *

from cudax_lib import get_translation
_   = get_translation(__file__)  # i18n

README_NAMES = (
    'readme.txt',
    'readme.md',
    )

HISTORY_NAMES = (
    'history.txt',
)

def _root_item(s):

    n = s.rfind('/')
    return n<0 or n==len(s)-1

def get_props_of_zip_filename(zip_fn):

    temp_dir = tempfile.gettempdir()
    z = zipfile.ZipFile(zip_fn, 'r')

    files = z.namelist()
    if 'install.inf' in files:
        files.remove('install.inf')
    files = [f for f in files if _root_item(f)]

    z.extract('install.inf', temp_dir)
    z.close()
    fn = os.path.join(temp_dir, 'install.inf')

    if os.path.isfile(fn):
        typ = ini_read(fn, 'info', 'type', '')
        subdir = ini_read(fn, 'info', 'subdir', '')

        if typ=='cudatext-plugin':
            d = 'py'
            files = [subdir+'/']
        elif typ=='cudatext-data':
            d = 'data/'+subdir
        elif typ=='lexer':
            d = 'data/lexlib'
        elif typ=='lexer-lite':
            d = 'data/lexliblite'
        else:
            d = ''

        os.remove(fn)
        #print('prop', (d, files, subdir))
        return (d, files, subdir)


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


def do_remove_dir(dir):
    """
    move folder to py/__trash
    (make copy with _ suffix if necessary)
    """
    #print(_('Deleting folder:'), dir)
    if not os.path.isdir(dir):
        return

    dir_trash = os.path.join(app_path(APP_DIR_PY), '__trash')
    dir_dest = os.path.join(dir_trash, os.path.basename(dir))
    while os.path.isdir(dir_dest):
        dir_dest += '_'

    if not os.path.isdir(dir_trash):
        os.mkdir(dir_trash)

    try:
        os.rename(dir, dir_dest)
    except OSError:
        msg_box(_('Cannot remove folder:\n')+dir, MB_OK+MB_ICONERROR)
        return
    return True


def get_installed_modules():
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
    lmod = get_installed_modules()
    if exclude_list:
        lmod = [i for i in lmod if not i in exclude_list]
    ldesc = [get_name_of_module(l) for l in lmod]
    res = dlg_menu(DMENU_LIST, ldesc, caption=caption)
    if res is None:
        return None
    return lmod[res]


def get_installed_addons(ignore={}):

    exclude_modules = ignore.get('plugins', []) 
    exclude_lexers = ignore.get('lexers', [])
    exclude_lexers_lite = ignore.get('lexers_lite', [])
    exclude_themes = ignore.get('themes', [])
    exclude_translations = ignore.get('lang', [])
    exclude_snippets = ignore.get('snippets', [])
    exclude_snippetsx = ignore.get('snippetsx', [])

    d = app_path(APP_DIR_PY)
    l = get_installed_modules()
    l = [i for i in l if not i in exclude_modules]
    res = [{
        'kind': 'plugin',
        'name': get_name_of_module(i),
        'module': i,
        'files': [
            os.path.join(d, i)+'/',
            ],
        } for i in l]

    d = os.path.join(app_path(APP_DIR_DATA), 'lexlib')
    d_acp = os.path.join(app_path(APP_DIR_DATA), 'autocomplete')
    if os.path.isdir(d):
        l = os.listdir(d)
        l = [i.split('.')[0] for i in l if i.endswith('.lcf')]
        l = [i for i in l if not i in exclude_lexers]
        l = sorted(l)
        res += [{
            'kind': 'lexer',
            'name': i,
            'files': [
                os.path.join(d, i+'.lcf'),
                os.path.join(d, i+'.cuda-lexmap'),
                os.path.join(d_acp, i+'.acp'),
                ],
            } for i in l]

    d = os.path.join(app_path(APP_DIR_DATA), 'lexliblite')
    if os.path.isdir(d):
        l = os.listdir(d)
        l = [i.split('.')[0] for i in l if i.endswith('.cuda-litelexer')]
        l = [i for i in l if not i in exclude_lexers_lite]
        l = sorted(l)
        res += [{
            'kind': 'lexer',
            'name': i+' ^',
            'files': [
                os.path.join(d, i+'.cuda-litelexer'),
                ],
            } for i in l]

    d = os.path.join(app_path(APP_DIR_DATA), 'snippets')
    if os.path.isdir(d):
        l = os.listdir(d)
        l = [i for i in l if not i in exclude_snippets]
        l = sorted(l)
        res += [{
            'kind': 'snippets',
            'name': i,
            'files': [
                os.path.join(d, i)+'/',
                ],
            } for i in l]

    d = os.path.join(app_path(APP_DIR_DATA), 'snippetsx')
    if os.path.isdir(d):
        l = os.listdir(d)
        l = [i for i in l if not i in exclude_snippetsx]
        l = sorted(l)
        res += [{
            'kind': 'snippetsx',
            'name': i,
            'files': [
                os.path.join(d, i)+'/',
                ],
            } for i in l]

    d = os.path.join(app_path(APP_DIR_DATA), 'themes')
    if os.path.isdir(d):
        l = os.listdir(d)
        l = [i.split('.')[0] for i in l if i.endswith('.cuda-theme-syntax') or i.endswith('.cuda-theme-ui')]
        l = [i for i in l if not i in exclude_themes]
        l = list(set(l)) # del duplicates
        l = sorted(l)
        res += [{
            'kind': 'theme',
            'name': i,
            'files': [
                os.path.join(d, i+'.cuda-theme-syntax'),
                os.path.join(d, i+'.cuda-theme-ui'),
                ],
            } for i in l]

    d = os.path.join(app_path(APP_DIR_DATA), 'lang')
    if os.path.isdir(d):
        l = os.listdir(d)
        l = [i.split('.')[0] for i in l if i.endswith('.ini')]
        l = [i for i in l if not i in exclude_translations]
        l = sorted(l)
        res += [{
            'kind': 'translation',
            'name': i,
            'files': [
                os.path.join(d, i+'.ini'),
                ],
            } for i in l]

    return res


def get_packages_ini():

    return os.path.join(app_path(APP_DIR_SETTINGS), 'packages.ini')


def do_save_version(url, fn, version):

    props = get_props_of_zip_filename(fn)
    if props:
        d, f, m = props
        fn = get_packages_ini()
        sec = os.path.basename(url)
        ini_write(fn, sec, 'd', d)
        ini_write(fn, sec, 'f', ';'.join(f))
        ini_write(fn, sec, 'v', version)
        return props


def get_addon_version(url):

    fn = get_packages_ini()
    return ini_read(fn, os.path.basename(url), 'v', '')


def do_remove_version_of_plugin(mod):

    fn = get_packages_ini()
    config = configparser.ConfigParser()
    config.read(fn)
    for sec in config.sections():
        cfg = config[sec]
        if cfg.get('d')=='py' and cfg.get('f')==mod+'/':
            del config[sec]
            with open(fn, 'w') as f:
                config.write(f, False)
