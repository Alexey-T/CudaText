import os
from datetime import datetime
from time import strftime, gmtime
import cudatext as app
from cudatext import ed
from cudax_lib import get_translation

_   = get_translation(__file__)  # i18n

DEF_CONFIG = '''#Documentation about formats: http://strftime.org/
%d/%m/%Y %H:%M:%S
%d.%m.%Y
%Y.%m.%d
%d. %B %Y
%d %b %Y
%A %d. %B.%Y
%H:%M:%S
rfc
'''

ini = os.path.join(app.app_path(app.APP_DIR_SETTINGS), 'cuda_insert_time.ini')
if not os.path.isfile(ini):
    with open(ini, 'w') as f:
        f.write(DEF_CONFIG)

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
            app.file_open(ini)

    def do_insert(self, s):

        x, y, x1, y1 = ed.get_carets()[0]
        if y1>=0:
            if (y, x)>(y1, x1):
                x, y, x1, y1 = x1, y1, x, y
            ed.set_caret(x, y)
            ed.replace(x, y, x1, y1, s)
        else:
            ed.insert(x, y, s)

        app.msg_status(_('Date/time inserted'))


    def dialog(self):

        lines = get_format_lines()
        lines = [do_format(s) for s in lines]

        res = app.dlg_menu(app.DMENU_LIST, lines, caption=_('Insert Time'))
        if res is None: return
        self.do_insert(lines[res])


    def ins_default(self):

        fmt = get_default_format()
        if not fmt:
            app.msg_box(_('No default time format is specified. To specify it, open config file (menu Options / Settings-plugins / Insert Time), and prefix some format with @ char.'),
              app.MB_OK or app.MB_ICONINFO)
            return

        self.do_insert(do_format(fmt))