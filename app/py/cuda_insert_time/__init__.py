import os
import shutil
from cudatext import *
from datetime import datetime

INI = 'cuda_insert_time.ini'

ini = os.path.join(app_path(APP_DIR_SETTINGS), INI)
ini0 = os.path.join(os.path.dirname(__file__), INI)
if not os.path.isfile(ini) and os.path.isfile(ini0):
    shutil.copyfile(ini0, ini)


def get_format_lines():
    with open(ini, 'r') as f:
        res = f.read().splitlines()
    res = [s for s in res if s and not s.startswith('#')]
    return res


class Command:
    def dialog(self):
        lines = get_format_lines()
        t = datetime.now()
        lines = ['Insert: '+t.strftime(s) for s in lines]

        res = dlg_menu(MENU_LIST, '\n'.join(lines))
        if res is None: return
        s = lines[res]

        caret = ed.get_carets()[0]
        ed.insert(caret[0], caret[1], s)
        msg_status('Date/time inserted')
