import cudatext_api as ct

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

CARET_SET_ONE = 0
CARET_ADD = 1
CARET_DELETE_ALL = 2
CARET_SET_INDEX = 100

APP_DIR_EXE = 0
APP_DIR_SETTINGS = 1
APP_DIR_DATA = 2
APP_DIR_PY = 3
APP_FILE_SESSION = 4

CONVERT_CHAR_TO_COL = 0
CONVERT_COL_TO_CHAR = 1

TOKEN_AT_POS = 0
TOKEN_INDEX = 1

LINESTATE_NORMAL  = 0
LINESTATE_CHANGED = 1
LINESTATE_ADDED   = 2
LINESTATE_SAVED   = 3

COLOR_NONE = 0x1FFFFFFF

MENU_LIST = 0
MENU_LIST_ALT = 1

BOOKMARK_GET = 0
BOOKMARK_SET = 1
BOOKMARK_CLEAR = 2
BOOKMARK_CLEAR_ALL = 3
BOOKMARK_SETUP = 4
BOOKMARK_GET_LIST = 5

MARKERS_GET = 0
MARKERS_ADD = 1
MARKERS_DELETE_ALL = 2
MARKERS_DELETE_LAST = 3
MARKERS_DELETE_BY_TAG = 4

TAB_SPLIT_NO = 0
TAB_SPLIT_HORZ = 1
TAB_SPLIT_VERT = 2

LOG_CLEAR         = 0
LOG_ADD           = 1
LOG_SET_PANEL     = 2
LOG_SET_REGEX     = 3
LOG_SET_LINE_ID   = 4
LOG_SET_COL_ID    = 5
LOG_SET_NAME_ID   = 6
LOG_SET_FILENAME  = 7
LOG_SET_ZEROBASE  = 8
LOG_GET_LINES     = 9
LOG_GET_LINEINDEX = 10
LOG_SET_LINEINDEX = 11
LOG_PANEL_ADD     = 12
LOG_PANEL_DELETE  = 13
LOG_PANEL_FOCUS   = 14
LOG_CONSOLE_CLEAR = 20
LOG_CONSOLE_ADD   = 21
LOG_CONSOLE_GET   = 22

LOG_PANEL_OUTPUT   = "0"
LOG_PANEL_VALIDATE = "1"

PROP_GUTTER_NUM     = 1
PROP_GUTTER_FOLD    = 2
PROP_GUTTER_BM      = 3
PROP_EOL            = 4
PROP_WRAP           = 5
PROP_RO             = 6
PROP_TAB_SPACES     = 7
PROP_TAB_SIZE       = 8
PROP_MARGIN         = 9
PROP_MARGIN_STRING  = 10
PROP_INSERT         = 11
PROP_MODIFIED       = 12
PROP_RULER          = 13
PROP_LINE_STATE     = 14
PROP_COLOR          = 15
PROP_LEXER_FILE     = 20
PROP_LEXER_POS      = 21
PROP_LEXER_CARET    = 22
PROP_INDEX_GROUP    = 23
PROP_INDEX_TAB      = 24
PROP_TAG            = 25
PROP_CARET_SHAPE     = 26
PROP_CARET_SHAPE_OVR = 27
PROP_CARET_SHAPE_RO  = 28
PROP_UNPRINTED_SHOW        = 30
PROP_UNPRINTED_SPACES      = 31
PROP_UNPRINTED_ENDS        = 32
PROP_UNPRINTED_END_DETAILS = 33
PROP_TAB_COLLECT_MARKERS = 35
PROP_MACRO_REC = 36
PROP_EXPORT_HTML = 37
PROP_MARKED_RANGE = 38
PROP_VISIBLE_LINES = 40
PROP_VISIBLE_COLUMNS = 41
PROP_LINE_BOTTOM = 42
PROP_PICTURE = 43
PROP_MINIMAP = 44
PROP_MICROMAP = 45

PROC_GET_CLIP = 0
PROC_SET_CLIP = 1
PROC_GET_COMMAND = 2
PROC_SAVE_SESSION = 3
PROC_LOAD_SESSION = 4
PROC_SET_SESSION = 5
PROC_MENU_CLEAR = 6
PROC_MENU_ADD = 7
PROC_MENU_ENUM = 8
PROC_SET_EVENTS = 10
PROC_GET_LAST_PLUGIN = 11
PROC_GET_GROUPING = 12
PROC_SET_GROUPING = 13
PROC_EXEC_PYTHON = 14
PROC_EXEC_PLUGIN = 15
PROC_SET_SUBCOMMANDS = 16
PROC_GET_ESCAPE = 17
PROC_SET_ESCAPE = 18
PROC_GET_COMMAND_PLUGIN = 19
PROC_GET_SPLIT = 20
PROC_SET_SPLIT = 21
PROC_GET_FIND_OPTIONS = 22
PROC_SET_FIND_OPTIONS = 23
PROC_SIDEPANEL_ADD = 24
PROC_SIDEPANEL_ACTIVATE = 25
PROC_SIDEPANEL_ENUM = 26
PROC_SIDEPANEL_GET_CONTROL = 27

TREE_ITEM_ENUM = 1
TREE_ITEM_ADD = 2
TREE_ITEM_DELETE = 3
TREE_ITEM_SET_TEXT = 4
TREE_ITEM_SET_ICON = 5
TREE_ITEM_SELECT = 6
TREE_ITEM_FOLD = 7
TREE_ITEM_FOLD_DEEP = 8
TREE_ITEM_UNFOLD = 9
TREE_ITEM_UNFOLD_DEEP = 10
TREE_ITEM_GET_SELECTED = 11
TREE_ITEM_GET_PROP = 12
TREE_ITEM_GET_PARENT = 13
TREE_ICON_ADD = 20
TREE_ICON_DELETE = 21
TREE_PROP_SHOW_ROOT = 30

LEXER_GET_LIST    = 0
LEXER_GET_ENABLED = 1
LEXER_GET_EXT     = 2
LEXER_GET_MODIFIED= 3
LEXER_GET_LINKS   = 4
LEXER_GET_STYLES  = 5
LEXER_GET_COMMENT = 6
LEXER_SET_NAME    = 10
LEXER_SET_ENABLED = 11
LEXER_SET_EXT     = 12
LEXER_SET_LINKS   = 13
LEXER_SAVE_LIB    = 20
LEXER_DELETE      = 21
LEXER_IMPORT      = 22
LEXER_EXPORT      = 23

GROUPS_ONE    = 1
GROUPS_2VERT  = 2
GROUPS_2HORZ  = 3
GROUPS_3VERT  = 4
GROUPS_3HORZ  = 5
GROUPS_3PLUS  = 6 #deprecated
GROUPS_1P2VERT = 6
GROUPS_1P2HORZ = 7
GROUPS_4VERT  = 8
GROUPS_4HORZ  = 9
GROUPS_4GRID  = 10
GROUPS_6GRID  = 11

def app_exe_version():
    return ct.app_exe_version()
def app_api_version():
    return ct.app_api_version()
def app_path(id):
    return ct.app_path(id)
def app_proc(id, text):
    return ct.app_proc(id, text)    

def app_log(id, text, tag=0):
    res = ct.app_log(id, text, tag)
    if id==LOG_CONSOLE_GET:
        return res.splitlines()
    else:
        return res    

def msg_box(text, flags):
    return ct.msg_box(text, flags)
def msg_status(text):
    return ct.msg_status(text)
def msg_status_alt(text, seconds):
    return ct.msg_status_alt(text, seconds)
    
def dlg_input(label, defvalue):
    return ct.dlg_input(label, defvalue)
def dlg_color(value):
    return ct.dlg_color(value)    

def dlg_input_ex(number, caption,
                 label1   , text1='', label2='', text2='', label3='', text3='',
                 label4='', text4='', label5='', text5='', label6='', text6='',
                 label7='', text7='', label8='', text8='', label9='', text9='',
                 label10='', text10=''):
    result = ct.dlg_input_ex(number, caption,
                 label1, text1, label2, text2, label3, text3,
                 label4, text4, label5, text5, label6, text6,
                 label7, text7, label8, text8, label9, text9,
                 label10, text10)
    if result is None:
        return None
    else:
        return result.splitlines()
        
def dlg_menu(id, text, focused=0):
    return ct.dlg_menu(id, text, focused)        

def dlg_file(is_open, init_filename, init_dir, filters):
    res = ct.dlg_file(is_open, init_filename, init_dir, filters)
    if res is None:
        return None
    res = res.splitlines()
    if len(res)==1:
        res=res[0]
    return res


def dlg_hotkeys(text):
    return ct.dlg_hotkeys(text)
    
def dlg_custom(title, size_x, size_y, text, focused=-1):    
    return ct.dlg_custom(title, size_x, size_y, text, focused)    

def file_open(filename, group=-1):
    return ct.file_open(filename, group)
def file_save(filename=''):
    return ct.file_save(filename)

def ed_handles():
    r0, r1 = ct.ed_handles()
    return range(r0, r1+1)

def ini_read(filename, section, key, value):
    return ct.ini_read(filename, section, key, value)
def ini_write(filename, section, key, value):
    return ct.ini_write(filename, section, key, value)
    
def lexer_proc(id, value):
    return ct.lexer_proc(id, value)

def tree_proc(id_tree, id_action, id_item=0, index=0, text='', image_index=-1):
    res = ct.tree_proc(id_tree, id_action, id_item, index, text, image_index)
    if res is None: return
    if id_action==TREE_ITEM_ENUM:
        res = res.splitlines()
        res = [r.split('=', 1) for r in res]
        res = [(int(r[0]), r[1]) for r in res]
    return res
    

#Editor
class Editor:
    h = 0
    def __init__(self, handle):
        self.h = handle

    def get_carets(self):
        big = 4294967295 #workaround for Py engine bug. it gives this, not -1.
        res = ct.ed_get_carets(self.h)
        for item in res:
            if item[2]==big: item[2]=-1
            if item[3]==big: item[3]=-1
        return res
        
    def set_caret(self, x1, y1, x2=-1, y2=-1, id=CARET_SET_ONE):
        return ct.ed_set_caret(self.h, x1, y1, x2, y2, id)

    def get_line_count(self):
        return ct.ed_get_line_count(self.h)

    def get_text_all(self):
        items = [self.get_text_line(i) for i in range(self.get_line_count())]
        return '\n'.join(items)
        
    def set_text_all(self, text):
        return ct.ed_set_text_all(self.h, text)
    def get_text_sel(self):
        return ct.ed_get_text_sel(self.h)
    def get_text_line(self, num):
        return ct.ed_get_text_line(self.h, num)
    def set_text_line(self, num, text):
        return ct.ed_set_text_line(self.h, num, text)
    def get_text_substr(self, x1, y1, x2, y2):
        return ct.ed_get_text_substr(self.h, x1, y1, x2, y2)

    def get_sel_mode(self):
        return ct.ed_get_sel_mode(self.h)
    def get_sel_lines(self):
        return ct.ed_get_sel_lines(self.h)
    def get_sel_rect(self):
        return ct.ed_get_sel_rect(self.h)
    def set_sel_rect(self, x1, y1, x2, y2):
        return ct.ed_set_sel_rect(self.h, x1, y1, x2, y2)

    def delete(self, x1, y1, x2, y2):
        return ct.ed_delete(self.h, x1, y1, x2, y2)
    def insert(self, x1, y1, text):
        return ct.ed_insert(self.h, x1, y1, text)

    def get_filename(self):
        return ct.ed_get_filename(self.h)

    def get_tabcolor(self):
        return ct.ed_get_tabcolor(self.h)
    def set_tabcolor(self, value):
        return ct.ed_set_tabcolor(self.h, value)

    def get_enc(self):
        return ct.ed_get_enc(self.h)
    def set_enc(self, value):
        return ct.ed_set_enc(self.h, value)
    def get_top(self):
        return ct.ed_get_top(self.h)
    def set_top(self, value):
        return ct.ed_set_top(self.h, value)

    def save(self, filename=''):
        return ct.ed_save(self.h, filename)
    def cmd(self, code, text=''):
        return ct.ed_cmd(self.h, code, text)
    def focus(self):
        return ct.ed_focus(self.h)
    def bookmark(self, id, nline, nkind=1, ncolor=-1, icon=''):
        return ct.ed_bookmark(self.h, id, nline, nkind, ncolor, icon)

    def lock(self):
        return ct.ed_lock(self.h)
    def unlock(self):
        return ct.ed_unlock(self.h)

    def get_split(self):
        return ct.ed_get_split(self.h)
    def set_split(self, state, value):
        return ct.ed_set_split(self.h, state, value)
        
    def get_prop(self, id, value=''):
        return ct.ed_get_prop(self.h, id, value)
    def set_prop(self, id, value):
        return ct.ed_set_prop(self.h, id, value)
    
    def complete(self, text, len1, len2):
        return ct.ed_complete(self.h, text, len1, len2)
        
    def convert(self, id, x, y):
        return ct.ed_convert(self.h, id, x, y)
        
    def get_ranges(self):
        return ct.ed_get_ranges(self.h)
        
    def markers(self, id, x=0, y=0, tag=0, len=0):
        return ct.ed_markers(self.h, id, x, y, tag, len)
        
    def attr(self, id, tag=0, x=0, y=0, len=0,
             color_font=0, color_bg=0, color_border=0,
             font_bold=0, font_italic=0, font_strikeout=0,
             border_left=0, border_right=0, border_down=0, border_up=0
             ):
        return ct.ed_attr(self.h, id, tag, x, y, len,   
                          color_font, color_bg, color_border,
                          font_bold, font_italic, font_strikeout,
                          border_left, border_right, border_down, border_up
                          )
                          
    def get_token(self, id, index1, index2):
        return ct.ed_get_token(self.h, id, index1, index2)
    #end

#objects
ed = Editor(0)
ed_bro = Editor(1)
