import os
import json
import tempfile
import subprocess
from datetime import datetime
from cudatext import *
import cudatext_cmd
from .work_remote import get_url
from .work_install_helper import after_install

from cudax_lib import get_translation
_   = get_translation(__file__)  # i18n

def get_datetime_short():
    t = datetime.now()
    return t.strftime('%Y.%m.%d')

def get_branch(url):

    fn = os.path.join(tempfile.gettempdir(), 'cudatext_git_branches.json')
    url_branches = url.replace('http://', 'https://').replace('//github.com/', '//api.github.com/repos/') + '/branches'
    get_url(url_branches, fn, True)
    if not os.path.isfile(fn):
        msg_box(_('Cannot read GitHub list of branches for that repo'), MB_OK+MB_ICONERROR)
        return

    with open(fn, 'r') as f:
        data = json.load(f)
        if not isinstance(data, list):
            msg_box(_('Got empty GitHub list of branches for that repo'), MB_OK+MB_ICONERROR)
            return
            
        items = [i.get('name') for i in data]
        if len(items)==1:
            branch = items[0]
        else:
            items2 = ['Git branch "%s"'%s for s in items]
            res = dlg_menu(DMENU_LIST, items2, caption=_('Git branches in repo'))
            if res is None: return
            branch = items[res]

        return branch


def dialog_github_install(history):
    c1 = chr(1)
    id_edit = 1
    id_ok = 2
    id_cancel = 3
    res = dlg_custom(_('Install from GitHub'), 456, 90, '\n'.join([]
      + [c1.join(['type=label', 'cap='+_('&GitHub repo URL'), 'pos=6,6,400,0'])]
      + [c1.join(['type=combo', 'items='+'\t'.join(history), 'pos=6,26,450,0', 'cap='+history[0]])]
      + [c1.join(['type=button', 'cap='+_('OK'), 'pos=246,60,346,0', 'ex0=1'])]
      + [c1.join(['type=button', 'cap='+_('Cancel'), 'pos=350,60,450,0'])]
      ))
    if not res: return
    btn, text = res
    if btn!=id_ok: return
    text = text.splitlines()
    url = text[id_edit]
    return url


fn_history = os.path.join(app_path(APP_DIR_SETTINGS), 'cuda_addonman_github.ini')
history = [
    'https://github.com/kvichans/cuda_find_in_files',
    ]

def rescan_plugins():
    pass
    #this commands hangs sometimes! commented
    #ed.cmd(cudatext_cmd.cmd_RescanPythonPluginsInfFiles)

def do_install_from_github():

    global history
    if os.path.isfile(fn_history):
        history = open(fn_history).read().splitlines()

    def save_history():

        global history
        #move new url to 1st item
        if url in history:
            history.remove(url)
        history = [url]+history

        with open(fn_history, 'w') as f:
            f.write('\n'.join(history))

    url = dialog_github_install(history)
    if not url: return
    if '://' not in url: return
    save_history()

    branch = get_branch(url)
    if not branch: return

    module_from_url = os.path.basename(url)

    fn = os.path.join(tempfile.gettempdir(), 'cudatext_addon.zip')
    fn_inf = os.path.join(tempfile.gettempdir(), 'cudatext_addon.inf')
    dir_py = app_path(APP_DIR_PY)
    dir_plugin = ''
    msg_status(_('Downloading...'))

    url_inf = url.replace('http://', 'https://').replace('https://github.com/', 'https://raw.githubusercontent.com/') + '/' + branch + '/install.inf'
    get_url(url_inf, fn_inf, True)

    valid = os.path.isfile(fn_inf) \
        and ini_read(fn_inf, 'info', 'type', '')=='cudatext-plugin'

    if valid:
        module = ini_read(fn_inf, 'info', 'subdir', '')
        os.remove(fn_inf)
        if module!=module_from_url:
            msg_box(_('Cannot install add-on due to mismatch:\n 1) install.inf "module":\n{}\n 2) repo name:\n{}').format(module, module_from_url), MB_OK+MB_ICONERROR)
            valid = False
            return
        dir_plugin = os.path.join(dir_py, module)
    else:
        msg_box(_('GitHub repository branch "%s" doesn\'t contain valid "install.inf" file. Cannot proceed.')%branch, MB_OK+MB_ICONERROR)
        return

    if os.path.isdir(os.path.join(dir_plugin, '.git')):
        msg_box(_('This repository is already cloned to your "py" folder, cannot proceed'), MB_OK+MB_ICONERROR)
        return

    do_clone = False
    if not os.path.isdir(dir_plugin):
        res = msg_box_ex(
                _('Addon Manager'),
                _('GitHub repository can be cloned (using "git clone") or can be downloaded as zip file. '+
                ' If you clone, Addon Manager\'s Update dialog will update add-on using "git pull", which is recommended.'),
                [_('Clone repo'), _('Download as zip'), _('Cancel')],
                MB_ICONQUESTION)
        if res==2 or res==None:
            return
        if res==0:
            do_clone = True

    if do_clone:
        try:
            subprocess.call(['git', 'clone', url], cwd=dir_py)
        except:
            msg_box(_('Error running Git command'), MB_OK+MB_ICONERROR)
            return

        if os.path.isdir(dir_plugin):
            after_install(module)
            #rescan_plugins()
            msg_box(_('Repo was cloned.\nRestart CudaText to make this plugin visible.'), MB_OK+MB_ICONINFO)
        else:
            msg_box(_('Could not clone the repo'), MB_OK+MB_ICONERROR)

        return

    get_url(url+'/zipball/'+branch, fn, True)
    msg_status('')
    if not os.path.isfile(fn):
        msg_status(_('Cannot download zip file'))
        return

    file_open(fn)
    os.remove(fn) #cleanup temp
    after_install(module)

    #rescan_plugins()
