import os
import tempfile
import subprocess
from datetime import datetime
from cudatext import *
import cudatext_cmd
from .work_remote import get_url
from .work_install_helper import after_install


def get_datetime_short():
    t = datetime.now()
    return t.strftime('%Y.%m.%d')

def dialog_github_install(history):
    c1 = chr(1)
    id_edit = 1
    id_ok = 2
    id_cancel = 3
    res = dlg_custom('Install from Github', 456, 90, '\n'.join([]
      + [c1.join(['type=label', 'cap=&Github repo URL', 'pos=6,6,400,0'])]
      + [c1.join(['type=combo', 'items='+'\t'.join(history), 'pos=6,26,450,0', 'cap='+history[0]])]
      + [c1.join(['type=button', 'cap=OK', 'pos=246,60,346,0', 'props=1'])]
      + [c1.join(['type=button', 'cap=Cancel', 'pos=350,60,450,0'])]
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

    module_from_url = os.path.basename(url)

    fn = os.path.join(tempfile.gettempdir(), 'cudatext_addon.zip')
    fn_inf = os.path.join(tempfile.gettempdir(), 'cudatext_addon.inf')
    dir_py = app_path(APP_DIR_PY)
    dir_plugin = ''
    msg_status('Downloading...')

    get_url(
        url.replace('http://', 'https://').replace('https://github.com/', 'https://raw.githubusercontent.com/')
        + '/master/install.inf', fn_inf, True)
    valid = os.path.isfile(fn_inf) \
        and ini_read(fn_inf, 'info', 'type', '')=='cudatext-plugin'

    if valid:
        module = ini_read(fn_inf, 'info', 'subdir', '')
        if module!=module_from_url:
            msg_box('Mismatch:\ninstall.inf "module": '+module+'\nrepo name: '+module_from_url, MB_OK+MB_ICONERROR)
            valid = False
            return
        dir_plugin = os.path.join(dir_py, module)
    else:
        msg_box('GitHub repository doesn\'t contain valid "install.inf" file. Cannot proceed.', MB_OK+MB_ICONERROR)
        return

    if os.path.isdir(os.path.join(dir_plugin, '.git')):
        msg_box('This repository is already cloned to your "py" folder, cannot proceed', MB_OK+MB_ICONERROR)
        return

    do_clone = False
    if not os.path.isdir(dir_plugin):
        res = msg_box_ex(
                'Addon Manager',
                'GitHub repository can be cloned (using "git clone") or can be downloaded as zip file. '+
                ' If you clone, Addon Manager\'s Update dialog will update add-on using "git pull", which is recommended.',
                ['Clone repo', 'Download as zip', 'Cancel'],
                MB_ICONQUESTION)
        if res==2 or res==None:
            return
        if res==0:
            do_clone = True

    if do_clone:
        try:
            subprocess.call(['git', 'clone', url], cwd=dir_py)
        except:
            msg_box('Error running Git command', MB_OK+MB_ICONERROR)
            return

        if os.path.isdir(dir_plugin):
            after_install(module)
            #rescan_plugins()
            msg_box('Repo was cloned.\nRestart CudaText to make this plugin visible.', MB_OK+MB_ICONINFO)
        else:
            msg_box('Could not clone the repo', MB_OK+MB_ICONERROR)

        return

    get_url(url+'/zipball/master', fn, True)
    msg_status('')
    if not os.path.isfile(fn):
        msg_status('Cannot download zip file')
        return

    file_open(fn)
    os.remove(fn) #cleanup temp
    after_install(module)

    #rescan_plugins()
