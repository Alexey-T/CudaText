import cudatext_api

MB_OK = 0x00000000
MB_OKCANCEL = 0x00000001
MB_ABORTRETRYIGNORE = 0x00000002
MB_YESNOCANCEL = 0x00000003
MB_YESNO = 0x00000004
MB_RETRYCANCEL = 0x00000005
MB_ICONERROR = 0x00000010
MB_ICONQUESTION = 0x00000020
MB_ICONWARNING = 0x00000030
MB_ICONINFO = 0x00000040

ID_OK = 1
ID_CANCEL = 2
ID_ABORT = 3
ID_RETRY = 4
ID_IGNORE = 5
ID_YES = 6
ID_NO = 7

SEL_NORMAL = 0
SEL_COLUMN = 1

APP_DIR_EXE = 0
APP_DIR_SETTINGS = 1
APP_DIR_DATA = 2

LINESTATE_NORMAL  = 0
LINESTATE_CHANGED = 1
LINESTATE_ADDED   = 2
LINESTATE_SAVED   = 3

COLOR_NONE = 0x1FFFFFFF

BOOKMARK_GET = 0
BOOKMARK_SET = 1
BOOKMARK_CLEAR = 2
BOOKMARK_CLEAR_ALL = 3
BOOKMARK_SETUP = 4
BOOKMARK_GET_LIST = 5

TAB_SPLIT_NO = 0
TAB_SPLIT_HORZ = 1
TAB_SPLIT_VERT = 2


def app_version():
    return cudatext_api.app_version()
def app_api_version():
    return cudatext_api.app_api_version()
def app_path(id):
    return cudatext_api.app_path(id)

def msg_box(text, flags):
    return cudatext_api.msg_box(text, flags)
def msg_status(text):
    return cudatext_api.msg_status(text)
def dlg_input(label, defvalue):
    return cudatext_api.dlg_input(label, defvalue)

def dlg_input_ex(number, caption,
                 label1   , text1='', label2='', text2='', label3='', text3='',
                 label4='', text4='', label5='', text5='', label6='', text6='',
                 label7='', text7='', label8='', text8='', label9='', text9='',
                 label10='', text10=''):
    result = cudatext_api.dlg_input_ex(number, caption,
                 label1, text1, label2, text2, label3, text3,
                 label4, text4, label5, text5, label6, text6,
                 label7, text7, label8, text8, label9, text9,
                 label10, text10)
    if result is None:
        return None
    else:
        return result.splitlines()

def dlg_file(is_open, init_filename, init_dir, filters):
    res = cudatext_api.dlg_file(is_open, init_filename, init_dir, filters)
    if res is None:
        return None
    res = res.splitlines()
    if len(res)==1:
        res=res[0]
    return res

def file_open(filename):
    return cudatext_api.file_open(filename)
def file_save():
    return cudatext_api.file_save()

def ed_handles():
    r0, r1 = cudatext_api.ed_handles()
    return range(r0, r1+1)

#Editor
class Editor:
    h = 0
    def __init__(self, handle):
        self.h = handle

    def get_caret(self):
        return cudatext_api.ed_get_caret(self.h)
    def set_caret(self, x1, y1, x2=-1, y2=-1):
        return cudatext_api.ed_set_caret(self.h, x1, y1, x2, y2)
    def add_caret(self, x1, y1, x2=-1, y2=-1):
        return cudatext_api.ed_add_caret(self.h, x1, y1, x2, y2)
    def get_carets(self):
        return cudatext_api.ed_get_carets(self.h)

    def get_line_count(self):
        return cudatext_api.ed_get_line_count(self.h)
    def get_line_prop(self, num):
        return cudatext_api.ed_get_line_prop(self.h, num)

    def get_text_all(self):
        return cudatext_api.ed_get_text_all(self.h)
    def set_text_all(self, text):
        return cudatext_api.ed_set_text_all(self.h, text)
    def get_text_sel(self):
        return cudatext_api.ed_get_text_sel(self.h)
    def get_text_line(self, num):
        return cudatext_api.ed_get_text_line(self.h, num)
    def set_text_line(self, num, text):
        return cudatext_api.ed_set_text_line(self.h, num, text)
    def get_text_substr(self, x1, y1, x2, y2):
        return cudatext_api.ed_get_text_substr(self.h, x1, y1, x2, y2)

    def get_sel_mode(self):
        return cudatext_api.ed_get_sel_mode(self.h)
    def get_sel_lines(self):
        return cudatext_api.ed_get_sel_lines(self.h)
    def get_sel_rect(self):
        return cudatext_api.ed_get_sel_rect(self.h)
    def set_sel_rect(self, x1, y1, x2, y2):
        return cudatext_api.ed_set_sel_rect(self.h, x1, y1, x2, y2)

    def delete(self, x1, y1, x2, y2):
        return cudatext_api.ed_delete(self.h, x1, y1, x2, y2)
    def insert(self, x1, y1, text):
        return cudatext_api.ed_insert(self.h, x1, y1, text)

    def get_filename(self):
        return cudatext_api.ed_get_filename(self.h)

    def get_tabcolor(self):
        return cudatext_api.ed_get_tabcolor(self.h)
    def set_tabcolor(self, value):
        return cudatext_api.ed_set_tabcolor(self.h, value)

    def get_enc(self):
        return cudatext_api.ed_get_enc(self.h)
    def set_enc(self, value):
        return cudatext_api.ed_set_enc(self.h, value)
    def get_top(self):
        return cudatext_api.ed_get_top(self.h)
    def set_top(self, value):
        return cudatext_api.ed_set_top(self.h, value)

    def cmd(self, value):
        return cudatext_api.ed_cmd(self.h, value)
    def focus(self):
        return cudatext_api.ed_focus(self.h)
    def bookmark(self, id, nline, nkind=1, ncolor=-1, icon=''):
        return cudatext_api.ed_bookmark(self.h, id, nline, nkind, ncolor, icon)

    def lock(self):
        return cudatext_api.ed_lock(self.h)
    def unlock(self):
        return cudatext_api.ed_unlock(self.h)

    def get_split(self):
        return cudatext_api.ed_get_split(self.h)
    def set_split(self, state, value):
        return cudatext_api.ed_set_split(self.h, state, value)
    
    def complete(self, text, nchars):
        return cudatext_api.ed_complete(self.h, text, nchars)
    #test..

#objects
ed = Editor(0)
ed_bro = Editor(1)
