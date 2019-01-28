import os
import re
import shutil
import json
import collections
import webbrowser
from cudatext import *
from urllib.parse import unquote
from .work_local import *
from .work_remote import *
from .work_dlg_config import *
from .work_github import *
from .work_cudatext_updates__fosshub import check_cudatext
from . import opt

dir_for_all = os.path.join(os.path.expanduser('~'), 'CudaText_addons')
fn_config = os.path.join(app_path(APP_DIR_SETTINGS), 'cuda_addonman.json')

PREINST = 'preinstalled'
STD_MODULES = (
  'cuda_addonman',
  'cuda_comments',
  'cuda_insert_time',
  'cuda_make_plugin',
  'cuda_multi_installer',
  'cuda_new_file',
  'cuda_options_editor',
  'cuda_palette',
  'cuda_project_man',
  'cuda_show_unsaved',
  'cuda_sort',
  'cuda_tabs_list',
  'cuda_testing_code_tree',
  'cuda_testing_dlg_proc',
  'cuda_testing_gaps',
  'cudax_lib',
  )


class Command:

    def __init__(self):
        if os.path.isfile(fn_config):
            data = json.loads(open(fn_config).read(), object_pairs_hook=collections.OrderedDict)
            opt.ch_user = data.get('channels_user', opt.ch_user)
            opt.suggest_readme = data.get('suggest_readme', True)
            opt.install_confirm = data.get('install_confirm', True)
            opt.proxy = data.get('proxy', '')


    def do_config(self):
        res = do_config_dialog()
        if res is None: return

        data = {}
        data['channels_user'] = opt.ch_user
        data['suggest_readme'] = opt.suggest_readme
        data['install_confirm'] = opt.install_confirm
        data['proxy'] = opt.proxy

        with open(fn_config, 'w') as f:
            f.write(json.dumps(data, indent=4))


    def do_show_links(self):

        msg_status('Downloading list...')
        items = get_remote_addons_list(opt.ch_def+opt.ch_user)
        msg_status('')
        if not items:
            msg_status('Cannot download list')
            return

        res = sorted([item['url'] for item in items])
        file_open('')
        ed.set_text_all('\n'.join(res)+'\n')


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
        items = get_remote_addons_list(opt.ch_def+opt.ch_user)
        msg_status('')
        if not items:
            msg_status('Cannot download list')
            return

        err = 0
        stopped = False
        app_proc(PROC_SET_ESCAPE, False)

        for (i, item) in enumerate(items):
            url = item['url']

            if app_proc(PROC_GET_ESCAPE, '')==True:
                app_proc(PROC_SET_ESCAPE, False)
                if msg_box('Stop downloading?', MB_OKCANCEL+MB_ICONQUESTION)==ID_OK:
                    stopped = True
                    break

            app_proc(PROC_PROGRESSBAR, (i+1)*100//len(items))
            msg_status('Downloading: %d/%d'%(i+1, len(items)), True) #must be with True

            name = unquote(url.split('/')[-1])
            dir = os.path.join(dir_for_all, name.split('.')[0])
            if not os.path.isdir(dir):
                os.mkdir(dir)
            fn = os.path.join(dir, name)
            res = get_url(url, fn)
            if res == False: #abort button
                break
            if not os.path.isfile(fn):
                err += 1
                print('Cannot download file: '+url)
                continue

        app_proc(PROC_PROGRESSBAR, 0)
        app_proc(PROC_PROGRESSBAR, -1)

        text = 'Download done' if not stopped else 'Download stopped'
        if err>0:
            text += '\nErrors occured, see Python console'
        msg_box(text, MB_OK+MB_ICONINFO)


    def do_reinstall_addon(self):
        self.do_install_addon(True)

    def do_install_addon(self, reinstall=False):

        caption = 'Re-install' if reinstall else 'Install'
        msg_status('Downloading list...')
        items = get_remote_addons_list(opt.ch_def+opt.ch_user)
        msg_status('')
        if not items:
            msg_status('Cannot download list')
            return

        items = sorted(items,
            key=lambda item: (item['kind'], item['name'])
            )

        kinds = sorted(list(set([i['kind'] for i in items])))

        installed_list = get_installed_list()
        if reinstall:
            items = [i for i in items if i.get('module', '') in installed_list]
        else:
            items = [i for i in items if i.get('module', '') not in installed_list]

        names = ['<Category>'] + [ i['kind']+': '+i['name']+'\t'+i['desc'] for i in items ]

        res = dlg_menu(
            MENU_LIST_ALT+MENU_NO_FUZZY+MENU_NO_FULLFILTER,
            names,
            caption=caption
            )
        if res is None: return

        if res==0:
            res = dlg_menu(
                MENU_LIST,
                kinds,
                caption='Category'
                )
            if res is None: return

            need_kind = kinds[res]
            items = [ i for i in items if i['kind']==need_kind ]
            names = [ i['kind']+': '+i['name']+'\t'+i['desc'] for i in items ]

            res = dlg_menu(
                MENU_LIST_ALT+MENU_NO_FUZZY+MENU_NO_FULLFILTER,
                names,
                caption=caption+' / Category "'+need_kind+'"'
                )
            if res is None: return

            name = items[res]['name']
            url = items[res]['url']
            version = items[res]['v']
            kind = items[res]['kind']

        else:
            res -= 1
            name = items[res]['name']
            url = items[res]['url']
            version = items[res]['v']
            kind = items[res]['kind']

        self.do_install_single(name, url, version, kind)


    def do_install_single(self, name, url, version, kind):
        #check for CudaLint
        if 'linter.' in url:
            if not 'cuda_lint' in get_installed_list():
                msg_box('This is linter, it requires CudaLint plugin installed', MB_OK+MB_ICONWARNING)
                return

        #check for CudaTree
        if 'treehelper.' in url:
            if not 'cuda_tree' in get_installed_list():
                msg_box('This is TreeHelper, it requires CudaTree plugin installed', MB_OK+MB_ICONWARNING)
                return

        #download
        fn = get_plugin_zip(url)
        if not os.path.isfile(fn):
            msg_status('Cannot download file')
            return

        s_options = '' if opt.install_confirm else '/silent'
        ok = file_open(fn, options=s_options)

        msg_status('Addon installed' if ok else 'Installation cancelled')
        if not ok:
            os.remove(fn)
            return

        #save version
        props = do_save_version(url, fn, version)
        os.remove(fn)

        if props:
            #suggest readme
            m = props[2]
            if m and opt.suggest_readme:
                names = []
                fn = get_readme_of_module(m)
                if fn:
                    names.append((get_name_of_module(m)+': view readme', fn))
                fn = get_history_of_module(m)
                if fn:
                    names.append((get_name_of_module(m)+': view history', fn))

                if names:
                    res = dlg_menu(MENU_LIST, [s[0] for s in names])
                    if res is None: return
                    file_open(names[res][1])


    def do_install_lexer(self):
        """Not used. For future? Suggest only lexers to install"""

        items = get_avail_list()
        items = [l for l in items if l[0].startswith('Lexer:')]

        res = dlg_menu(MENU_LIST, [l[0] for l in items])
        if res is None: return
        res = items[res]
        url = get_item_url(res[2])
        fn = get_plugin_zip(url)
        if os.path.isfile(fn):
            file_open(fn)

    def do_remove(self):
        m = get_installed_choice('Remove', STD_MODULES)
        if not m:
            return
        if msg_box('Remove plugin: '+get_name_of_module(m), MB_OKCANCEL+MB_ICONQUESTION)!=ID_OK:
            return

        do_remove_version_of_plugin(m)
        if do_remove_module(m):
            msg_box('Removed, restart program to see changes', MB_OK+MB_ICONINFO)

    def do_remove_data(self):
        path = get_installed_data_choice()
        if path is None:
            return

        if os.path.isfile(path):
            msg = 'Remove data file:'
        else:
            msg = 'Remove data folder:'
        if msg_box(msg+'\n'+path, MB_OKCANCEL+MB_ICONQUESTION)!=ID_OK:
            return

        if do_remove_data(path):
            msg_box('Removed, restart program to see changes', MB_OK+MB_ICONINFO)


    def do_edit(self):
        m = get_installed_choice('Edit')
        if m is None: return
        fn = get_initpy_of_module(m)
        file_open(fn)
        msg_status('Opened: '+fn)

    def do_homepage(self):
        m = get_installed_choice('Visit homepage')
        if m is None: return
        s = get_homepage_of_module(m)
        if s:
            webbrowser.open_new_tab(s)
            msg_status('Opened browser: '+s)
        else:
            msg_box('Plugin "%s" doesn\'t have "homepage" field in install.inf' % \
              get_name_of_module(m), MB_OK+MB_ICONWARNING)

    def do_readme(self):
        m = get_installed_choice('Open readme')
        if m is None: return
        s = get_readme_of_module(m)
        if s:
            file_open(s)
        else:
            msg_status('Plugin "%s" doesn\'t have readme' % get_name_of_module(m))

    def do_history(self):
        m = get_installed_choice('Open history')
        if m is None: return
        s = get_history_of_module(m)
        if s:
            file_open(s)
        else:
            msg_status('Plugin "%s" doesn\'t have history' % get_name_of_module(m))


    def do_update(self):
    
        def fn2name(s, del_brackets):
            s = s.split('.')[0].replace(' ', '_')
            # strip additions in name for "gruvbox (Dark) (Medium)"
            if del_brackets:
                n = s.find('_(')
                if n>=0:
                    s = s[:n]
            return s

        msg_status('Downloading list...')
        addons = get_remote_addons_list(opt.ch_def+opt.ch_user)
        msg_status('')
        if not addons:
            msg_status('Cannot download list')
            return

        modules = get_installed_list()
        modules = [m for m in modules if m not in STD_MODULES] + [m for m in modules if m in STD_MODULES]

        dir_lexers = os.path.join(app_path(APP_DIR_DATA), 'lexlib')
        lexers = os.listdir(dir_lexers)
        lexers = [fn2name(s, False) for s in lexers if s.endswith('.lcf')]
        
        dir_langs = os.path.join(app_path(APP_DIR_DATA), 'lang')
        langs = os.listdir(dir_langs)
        langs = [fn2name(s, False) for s in langs if s.endswith('.ini')]

        dir_themes = os.path.join(app_path(APP_DIR_DATA), 'themes')
        themes = os.listdir(dir_themes)
        themes = [fn2name(s, True) for s in themes if '.cuda-theme' in s]

        addons = [a for a in addons if a['kind'] in ('plugin', 'treehelper', 'linter') and a.get('module', '') in modules] \
               + [a for a in addons if a['kind']=='lexer' and a['name'] in lexers] \
               + [a for a in addons if a['kind']=='translation' and a['name'] in langs] \
               + [a for a in addons if a['kind']=='theme' and a['name'] in themes]

        modules_web = [a.get('module', '') for a in addons]
        modules_web = [a for a in modules_web if a]
        modules_local = [m for m in modules if m not in modules_web]

        for a in addons:
            m = a.get('module', '')

            if a['kind']=='lexer':
                a['dir'] = 'data/lexlib'
            elif a['kind']=='translation':
                a['dir'] = 'data/lang'
            elif a['kind']=='theme':
                a['dir'] = 'data/themes'
            else:
                a['dir'] = 'py/'+m

            v_local = '?'
            if m in STD_MODULES:
                v_local = PREINST
            url = a['url']
            v_remote = a['v']
            v_local = get_addon_version(url) or v_local
            a['v_local'] = v_local

            a['check'] = (v_local!=PREINST) and ((v_local=='?') or (v_local<v_remote))

        text_headers = '\r'.join(('Name=260', 'Folder=180', 'Local=125', 'Available=125'))
        text_columns = ['\r'.join(('['+i['kind']+'] '+i['name'], i['dir'], i['v_local'], i['v'])) for i in addons]
        text_items = '\t'.join([text_headers]+text_columns)
        text_checks = ['1' if i['check'] else '0' for i in addons]
        text_val = '0;'+','.join(text_checks)
        text_val_initial = text_val

        RES_OK = 0
        RES_CANCEL = 1
        RES_LIST = 2
        RES_SEL_NONE = 3
        RES_SEL_NEW = 4
        c1 = chr(1)

        while True:
            text = '\n'.join([
              c1.join(['type=button', 'pos=514,500,614,0', 'cap=Update', 'props=1']),
              c1.join(['type=button', 'pos=620,500,720,0', 'cap=Cancel']),
              c1.join(['type=checklistview', 'pos=6,6,720,490', 'items='+text_items, 'val='+text_val, 'props=1']),
              c1.join(['type=button', 'pos=6,500,100,0', 'cap=Deselect all']),
              c1.join(['type=button', 'pos=106,500,200,0', 'cap=Select new']),
              ])

            res = dlg_custom('Update add-ons', 726, 532, text)
            if res is None: return

            res, text = res
            if res == RES_SEL_NONE:
                text_val = '0;'
                continue
            if res == RES_SEL_NEW:
                text_val = text_val_initial
                continue
            if res == RES_CANCEL:
                return
            if res == RES_OK:
                break

        text = text.splitlines()[RES_LIST]
        text = text.split(';')[1].split(',')

        addons = [a for (i, a) in enumerate(addons) if 0<=i<len(text) and text[i]=='1']
        if not addons:
            return
        print('Updating addons:')
        fail_count = 0

        for a in addons:
            print('  [%s] %s' % (a['kind'], a['name']))
            msg_status('Updating: [%s] %s' % (a['kind'], a['name']), True)

            m = a.get('module', '')
            if m:
                # delete old dir
                do_remove_module(m)

            url = a['url']
            if not url: continue

            fn = get_plugin_zip(url)
            if not fn: continue

            if os.path.isfile(fn) and file_open(fn, options='/silent'):
                do_save_version(url, fn, a['v'])
            else:
                fail_count += 1
                print('  Update failed: [%s] %s' % (a['kind'], a['name']) )

        s = 'Done'
        if fail_count>0:
            s += ', with %d fail(s)'%fail_count
        print(s)
        msg_status(s)


    def check_cudatext_updates(self):

        check_cudatext()

    def install_from_github(self):

        do_install_from_github()
