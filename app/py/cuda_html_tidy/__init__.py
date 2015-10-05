import os
import webbrowser
import cudatext_cmd
import tempfile
from cudatext import *
from .proc_run import *

if os.name=='posix':
    fn_exe = 'tidy'
else:
    fn_exe = os.path.join(os.path.dirname(__file__), 'tidy_win32', 'tidy.exe')

config_dir = os.path.join(os.path.dirname(__file__), 'configs')
help_url = 'http://www.htacg.org/tidy-html5/'
dir_temp = tempfile.gettempdir()


def do_log_clear():
    app_log(LOG_SET_PANEL, LOG_PANEL_VALIDATE)
    app_log(LOG_CLEAR, '')

def do_log(fn_ed, fn_err):
    app_log(LOG_SET_REGEX, r'line (\d+) column (\d+) .+')
    app_log(LOG_SET_LINE_ID, '1')
    app_log(LOG_SET_COL_ID, '2')
    app_log(LOG_SET_NAME_ID, '0')
    app_log(LOG_SET_FILENAME, fn_ed)
    
    text = open(fn_err).read().splitlines()
    if not text: return
    for s in text:
        app_log(LOG_ADD, s)
        
    ed.focus()    
    ed.cmd(cudatext_cmd.cmd_ShowPanelValidate)


def do_menu():
    l = os.listdir(config_dir)
    if not l: return
    l = sorted(l)
    l_full = [os.path.join(config_dir, s) for s in l]
    l_nice = [s[:s.find('.')] for s in l]
    n = dlg_menu(MENU_LIST, '\n'.join(l_nice))
    if n is None: return
    return l_full[n]


def do_tidy(validate_only):
    if app_api_version()<'1.0.102':
        msg_box('Need newer app version', MB_OK+MB_ICONERROR)
        return

    fn_ed = ed.get_filename()
    if not fn_ed:
        msg_status('Save file first')
        return
        
    file_save()

    if not validate_only:
        fn_cfg = do_menu()
        if not fn_cfg:
            return
                             
    fn_out = os.path.join(dir_temp, 'tidy_out.txt')
    fn_err = os.path.join(dir_temp, 'tidy_err.txt')
    if os.path.isfile(fn_out):
        os.remove(fn_out)
    if os.path.isfile(fn_err):
        os.remove(fn_err)
    
    if validate_only:
        command = [fn_exe, '-file', fn_err, '-errors', '-quiet', fn_ed]
    else:
        command = [fn_exe, '-output', fn_out, '-config', fn_cfg, '-file', fn_err, '-quiet', fn_ed]
    
    try:     
        do_run_hide(command)
    except:
        msg_box('Cannot run command "tidy", install it first', MB_OK+MB_ICONERROR)
        return    
    do_log_clear()
        
    if os.path.isfile(fn_err) and open(fn_err).read():
        msg_status('Tidy returned error')
        do_log(fn_ed, fn_err)
    elif os.path.isfile(fn_out):
        text = open(fn_out).read()
        if text:
            ed.set_text_all(text)
            msg_status('Tidy result inserted')
        else:
            msg_status('Tidy returned empty text')
    else:
        if validate_only:
            msg_status('Tidy shows OK result')
        else:
            msg_status('Tidy cannot handle this file')

    if os.path.isfile(fn_out):
        os.remove(fn_out)
    if os.path.isfile(fn_err):
        os.remove(fn_err)


class Command:
    def menu(self):
        do_tidy(False)

    def validate(self):
        do_tidy(True)

    def configs(self):
        if os.name=='nt':
            do_run_normal(['Explorer.exe', config_dir])
        else:
            msg_box('Command is for Windows only', MB_OK)

    def web(self):
        webbrowser.open_new_tab(help_url)
