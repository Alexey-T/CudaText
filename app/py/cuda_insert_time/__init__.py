import os
import shutil
from cudatext import *
from datetime import datetime
from time import strftime, gmtime

INI = 'cuda_insert_time.ini'

ini = os.path.join(app_path(APP_DIR_SETTINGS), INI)
ini0 = os.path.join(os.path.dirname(__file__), INI)
if not os.path.isfile(ini) and os.path.isfile(ini0):
    shutil.copyfile(ini0, ini)

COMMENT_CHAR = '#'
DEFAULT_CHAR = '@'


def get_format_lines():
    with open(ini, 'r') as f:
        res = f.read().splitlines()
    res = [s.lstrip(DEFAULT_CHAR) for s in res if s and not s.startswith(COMMENT_CHAR)]
    return res

def get_default_format():
    with open(ini, 'r') as f:
        res = f.read().splitlines()
    res = [s.lstrip(DEFAULT_CHAR) for s in res if s and s.startswith(DEFAULT_CHAR)]
    if res:
        return res[0]

def do_format(s):
    if s=='rfc':
        return strftime("%a, %d %b %Y %H:%M:%S +0000", gmtime())
    t = datetime.now()
    return t.strftime(s)


class Command:

    def config(self):

        if os.path.isfile(ini):
            file_open(ini)
        else:
            msg_status('Cannot find config file')


    def do_insert(self, s):

        x, y, x1, y1 = ed.get_carets()[0]
        x, y = ed.insert(x, y, s)
        ed.set_caret(x, y)
        msg_status('Date/time inserted')


    def dialog(self):

        lines = get_format_lines()
        lines = [do_format(s) for s in lines]

        res = dlg_menu(MENU_LIST, '\n'.join(lines))
        if res is None: return
        self.do_insert(lines[res])


    def ins_default(self):

        fmt = get_default_format()
        if not fmt:
            msg_box('No default time format is specified. To specify it, open config file (menu Options / Settings-plugins / Insert Time), and prefix some format with @ char.',
              MB_OK or MB_ICONINFO)
            return

        self.do_insert(do_format(fmt))
