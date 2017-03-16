import json
import cudatext_api as ct

MB_OK               = 0x00000000
MB_OKCANCEL         = 0x00000001
MB_ABORTRETRYIGNORE = 0x00000002
MB_YESNOCANCEL      = 0x00000003
MB_YESNO            = 0x00000004
MB_RETRYCANCEL      = 0x00000005
MB_ICONERROR        = 0x00000010
MB_ICONQUESTION     = 0x00000020
MB_ICONWARNING      = 0x00000030
MB_ICONINFO         = 0x00000040

ID_OK     = 1
ID_CANCEL = 2
ID_ABORT  = 3
ID_RETRY  = 4
ID_IGNORE = 5
ID_YES    = 6
ID_NO     = 7

SEL_NORMAL = 0
SEL_COLUMN = 1

CARET_SET_ONE    = 0
CARET_ADD        = 1
CARET_DELETE_ALL = 2
CARET_SET_INDEX  = 100

APP_DIR_EXE             = 0
APP_DIR_SETTINGS        = 1
APP_DIR_DATA            = 2
APP_DIR_PY              = 3
APP_FILE_SESSION        = 4
APP_DIR_INSTALLED_ADDON = 5
APP_FILE_RECENTS        = 6

CONVERT_CHAR_TO_COL         = 0
CONVERT_COL_TO_CHAR         = 1
CONVERT_LINE_TABS_TO_SPACES = 2

TOKEN_AT_POS = 0
TOKEN_INDEX  = 1

LINESTATE_NORMAL  = 0
LINESTATE_CHANGED = 1
LINESTATE_ADDED   = 2
LINESTATE_SAVED   = 3

COLOR_NONE = 0x1FFFFFFF

MENU_LIST     = 0
MENU_LIST_ALT = 1

BOOKMARK_GET         = 0
BOOKMARK_SET         = 1
BOOKMARK_CLEAR       = 2
BOOKMARK_CLEAR_ALL   = 3
BOOKMARK_SETUP       = 4
BOOKMARK_GET_LIST    = 5
BOOKMARK_CLEAR_HINTS = 6

MARKERS_GET           = 0
MARKERS_ADD           = 1
MARKERS_DELETE_ALL    = 2
MARKERS_DELETE_LAST   = 3
MARKERS_DELETE_BY_TAG = 4

TAB_SPLIT_NO   = 0
TAB_SPLIT_HORZ = 1
TAB_SPLIT_VERT = 2

TIMER_START     = 0
TIMER_START_ONE = 1
TIMER_STOP      = 2
TIMER_DELETE    = 3

LOG_CLEAR           = 0
LOG_ADD             = 1
LOG_SET_PANEL       = 2
LOG_SET_REGEX       = 3
LOG_SET_LINE_ID     = 4
LOG_SET_COL_ID      = 5
LOG_SET_NAME_ID     = 6
LOG_SET_FILENAME    = 7
LOG_SET_ZEROBASE    = 8
LOG_GET_LINES       = 9
LOG_GET_LINEINDEX   = 10
LOG_SET_LINEINDEX   = 11
LOG_PANEL_ADD       = 12
LOG_PANEL_DELETE    = 13
LOG_PANEL_FOCUS     = 14
LOG_CONSOLE_CLEAR   = 20
LOG_CONSOLE_ADD     = 21
LOG_CONSOLE_GET     = 22
LOG_CONSOLE_GET_LOG = 23

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
PROP_LINE_TOP       = 16
PROP_ENC            = 17
PROP_TAB_TITLE      = 18
PROP_TAB_COLOR      = 19
PROP_LEXER_FILE     = 20
PROP_LEXER_POS      = 21
PROP_LEXER_CARET    = 22
PROP_INDEX_GROUP    = 23
PROP_INDEX_TAB      = 24
PROP_TAG            = 25
PROP_CARET_SHAPE           = 26
PROP_CARET_SHAPE_OVR       = 27
PROP_CARET_SHAPE_RO        = 28
PROP_CARET_VIRTUAL         = 29
PROP_UNPRINTED_SHOW        = 30
PROP_UNPRINTED_SPACES      = 31
PROP_UNPRINTED_ENDS        = 32
PROP_UNPRINTED_END_DETAILS = 33
PROP_TAB_COLLECT_MARKERS   = 35
PROP_MACRO_REC             = 36
PROP_EXPORT_HTML           = 37
PROP_MARKED_RANGE          = 38
PROP_VISIBLE_LINES         = 40
PROP_VISIBLE_COLUMNS       = 41
PROP_LINE_BOTTOM           = 42
PROP_PICTURE               = 43
PROP_MINIMAP               = 44
PROP_MICROMAP              = 45
PROP_LINK_AT_POS           = 46
PROP_MODIFIED_VERSION      = 47
PROP_TAB_ID                = 48
PROP_COLUMN_LEFT           = 49

PROC_SET_CLIP_ALT        = -1
PROC_GET_CLIP            = 0
PROC_SET_CLIP            = 1
PROC_GET_COMMAND         = 2
PROC_SAVE_SESSION        = 3
PROC_LOAD_SESSION        = 4
PROC_SET_SESSION         = 5
PROC_MENU_CLEAR          = 6
PROC_MENU_ADD            = 7
PROC_MENU_ENUM           = 8
PROC_GET_COMMAND_INITIAL = 9
PROC_SET_EVENTS          = 10
PROC_GET_LAST_PLUGIN     = 11
PROC_GET_GROUPING        = 12
PROC_SET_GROUPING        = 13
PROC_EXEC_PYTHON         = 14
PROC_EXEC_PLUGIN         = 15
PROC_SET_SUBCOMMANDS     = 16
PROC_GET_ESCAPE          = 17
PROC_SET_ESCAPE          = 18
PROC_GET_COMMAND_PLUGIN  = 19
PROC_GET_SPLIT           = 20
PROC_SET_SPLIT           = 21
PROC_GET_FIND_OPTIONS    = 22
PROC_SET_FIND_OPTIONS    = 23
#
PROC_SIDEPANEL_ADD         = 24
PROC_SIDEPANEL_ACTIVATE    = 25
PROC_SIDEPANEL_ENUM        = 26
PROC_SIDEPANEL_GET_CONTROL = 27
PROC_SIDEPANEL_REMOVE      = 29
#
PROC_TOOLBAR_ENUM          = 30
PROC_TOOLBAR_ADD           = 31
PROC_TOOLBAR_DELETE        = 32
PROC_TOOLBAR_DELETE_ALL    = 33
PROC_TOOLBAR_ICON_ADD      = 34
PROC_TOOLBAR_ICON_SET      = 35
PROC_TOOLBAR_ICON_GET_SIZE = 36
PROC_TOOLBAR_ICON_SET_SIZE = 38
#
PROC_GET_LANG          = 40
PROC_GET_HOTKEY        = 41
PROC_SET_HOTKEY        = 42
PROC_GET_KEYSTATE      = 43
PROC_GET_FIND_STRINGS  = 44
PROC_GET_GUI_HEIGHT    = 45
PROC_THEME_UI_GET      = 46
PROC_THEME_UI_SET      = 47
PROC_THEME_SYNTAX_GET  = 48
PROC_THEME_SYNTAX_SET  = 49
PROC_HOTKEY_INT_TO_STR = 60
PROC_HOTKEY_STR_TO_INT = 61
#
PROC_BOTTOMPANEL_ADD         = 80
PROC_BOTTOMPANEL_ACTIVATE    = 81
PROC_BOTTOMPANEL_ENUM        = 82
PROC_BOTTOMPANEL_GET_CONTROL = 83
PROC_BOTTOMPANEL_REMOVE      = 84
#
PROC_SHOW_STATUSBAR_GET   = 100
PROC_SHOW_STATUSBAR_SET   = 101
PROC_SHOW_TOOLBAR_GET     = 102
PROC_SHOW_TOOLBAR_SET     = 103
PROC_SHOW_SIDEPANEL_GET   = 104
PROC_SHOW_SIDEPANEL_SET   = 105
PROC_SHOW_BOTTOMPANEL_GET = 106
PROC_SHOW_BOTTOMPANEL_SET = 107
PROC_SHOW_TABS_GET        = 108
PROC_SHOW_TABS_SET        = 109
#
PROC_COORD_WINDOW_GET = 140
PROC_COORD_WINDOW_SET = 141
PROC_COORD_DESKTOP    = 142
PROC_COORD_MONITOR    = 143
PROC_COORD_MONITOR0   = 144
PROC_COORD_MONITOR1   = 145
PROC_COORD_MONITOR2   = 146
PROC_COORD_MONITOR3   = 147

TREE_ITEM_ENUM             = 1
TREE_ITEM_ADD              = 2
TREE_ITEM_DELETE           = 3
TREE_ITEM_SET_TEXT         = 4
TREE_ITEM_SET_ICON         = 5
TREE_ITEM_SELECT           = 6
TREE_ITEM_FOLD             = 7
TREE_ITEM_FOLD_DEEP        = 8
TREE_ITEM_UNFOLD           = 9
TREE_ITEM_UNFOLD_DEEP      = 10
TREE_ITEM_GET_SELECTED     = 11
TREE_ITEM_GET_PROP         = 12
TREE_ITEM_GET_PARENT       = 13
TREE_ITEM_GET_SYNTAX_RANGE = 14
TREE_ICON_ADD              = 20
TREE_ICON_DELETE           = 21
TREE_PROP_SHOW_ROOT        = 30
TREE_LOCK                  = 31
TREE_UNLOCK                = 32

LISTBOX_GET_COUNT    = 0
LISTBOX_ADD          = 1
LISTBOX_DELETE       = 2
LISTBOX_DELETE_ALL   = 3
LISTBOX_GET_ITEM     = 4
LISTBOX_SET_ITEM     = 5
LISTBOX_GET_SEL      = 10
LISTBOX_SET_SEL      = 11
LISTBOX_GET_TOP      = 14
LISTBOX_SET_TOP      = 15

LEXER_GET_LIST            = 0
LEXER_GET_ENABLED         = 1
LEXER_GET_EXT             = 2
LEXER_GET_LINKS           = 4
LEXER_GET_STYLES          = 5
LEXER_GET_COMMENT         = 6
LEXER_GET_COMMENT_STREAM  = 7
LEXER_GET_COMMENT_LINED   = 8
LEXER_SET_NAME            = 10
LEXER_SET_ENABLED         = 11
LEXER_SET_EXT             = 12
LEXER_SET_LINKS           = 13
LEXER_DETECT              = 20
LEXER_DELETE              = 21
LEXER_IMPORT              = 22
LEXER_GET_STYLES_COMMENTS = 30
LEXER_GET_STYLES_STRINGS  = 31

GROUPS_ONE     = 1
GROUPS_2VERT   = 2
GROUPS_2HORZ   = 3
GROUPS_3VERT   = 4
GROUPS_3HORZ   = 5
GROUPS_3PLUS   = 6 #deprecated
GROUPS_1P2VERT = 6
GROUPS_1P2HORZ = 7
GROUPS_4VERT   = 8
GROUPS_4HORZ   = 9
GROUPS_4GRID   = 10
GROUPS_6GRID   = 11

#EDSTATE_WORDWRAP = 1
EDSTATE_TAB_TITLE = 2
EDSTATE_MODIFIED  = 3

COLOR_ID_TextFont = 'EdTextFont'
COLOR_ID_TextBg = 'EdTextBg'
COLOR_ID_SelFont = 'EdSelFont'
COLOR_ID_SelBg = 'EdSelBg'
COLOR_ID_DisableFont = 'EdDisableFont'
COLOR_ID_DisableBg = 'EdDisableBg'
COLOR_ID_Caret = 'EdCaret'
COLOR_ID_Markers = 'EdMarkers'
COLOR_ID_CurLineBg = 'EdCurLineBg'
COLOR_ID_IndentVLine = 'EdIndentVLine'
COLOR_ID_UnprintFont = 'EdUnprintFont'
COLOR_ID_UnprintBg = 'EdUnprintBg'
COLOR_ID_UnprintHexFont = 'EdUnprintHexFont'
COLOR_ID_MinimapBorder = 'EdMinimapBorder'
COLOR_ID_MinimapSelBg = 'EdMinimapSelBg'
COLOR_ID_StateChanged = 'EdStateChanged'
COLOR_ID_StateAdded = 'EdStateAdded'
COLOR_ID_StateSaved = 'EdStateSaved'
COLOR_ID_BlockStaple = 'EdBlockStaple'
COLOR_ID_BlockSepLine = 'EdBlockSepLine'
COLOR_ID_LockedBg = 'EdLockedBg'
COLOR_ID_ComboArrow = 'EdComboArrow'
COLOR_ID_ComboArrowBg = 'EdComboArrowBg'
COLOR_ID_FoldMarkLine = 'EdFoldMarkLine'
COLOR_ID_FoldMarkFont = 'EdFoldMarkFont'
COLOR_ID_FoldMarkBorder = 'EdFoldMarkBorder'
COLOR_ID_FoldMarkBg = 'EdFoldMarkBg'
COLOR_ID_GutterFont = 'EdGutterFont'
COLOR_ID_GutterBg = 'EdGutterBg'
COLOR_ID_GutterCaretFont = 'EdGutterCaretFont'
COLOR_ID_GutterCaretBg = 'EdGutterCaretBg'
COLOR_ID_BookmarkBg = 'EdBookmarkBg'
COLOR_ID_RulerFont = 'EdRulerFont'
COLOR_ID_RulerBg = 'EdRulerBg'
COLOR_ID_FoldLine = 'EdFoldLine'
COLOR_ID_FoldBg = 'EdFoldBg'
COLOR_ID_FoldPlusLine = 'EdFoldPlusLine'
COLOR_ID_FoldPlusBg = 'EdFoldPlusBg'
COLOR_ID_MarginFixed = 'EdMarginFixed'
COLOR_ID_MarginCaret = 'EdMarginCaret'
COLOR_ID_MarginUser = 'EdMarginUser'
COLOR_ID_MarkedRangeBg = 'EdMarkedRangeBg'
COLOR_ID_Links = 'EdLinks'
COLOR_ID_Border = 'EdBorder'

CANVAS_SET_FONT      = 1
CANVAS_SET_PEN       = 2
CANVAS_SET_BRUSH     = 3
CANVAS_SET_ANTIALIAS = 4
CANVAS_SET_TESTPANEL = 9
#CANVAS_GET_FONT      = 11
#CANVAS_GET_PEN       = 12
#CANVAS_GET_BRUSH     = 13
CANVAS_GET_TEXT_SIZE = 15
CANVAS_TEXT          = 20
CANVAS_LINE          = 21
CANVAS_IMAGE         = 22
CANVAS_IMAGE_SIZED   = 23
CANVAS_PIXEL         = 24
CANVAS_RECT          = 30
CANVAS_RECT_FRAME    = 31
CANVAS_RECT_FILL     = 32
CANVAS_RECT_ROUND    = 33
CANVAS_POLYGON       = 35
CANVAS_ELLIPSE       = 40

FONT_B = 1
FONT_I = 2
FONT_U = 4
FONT_S = 8

#TFPPenStyle = (psSolid, psDash, psDot, psDashDot, psDashDotDot, psinsideFrame, psPattern, psClear);
PEN_STYLE_SOLID       = 0
PEN_STYLE_DASH        = 1
PEN_STYLE_DOT         = 2
PEN_STYLE_DASHDOT     = 3
PEN_STYLE_DASHDOTDOT  = 4
PEN_STYLE_INSIDEFRAME = 5
PEN_STYLE_PATTERN     = 6
PEN_STYLE_CLEAR       = 7

#TFPPenEndCap = (pecRound, pecSquare, pecFlat);
PEN_CAPS_ROUND  = 0
PEN_CAPS_SQUARE = 1
PEN_CAPS_FLAT   = 2

#TFPPenJoinStyle = (pjsRound, pjsBevel, pjsMiter);
PEN_JOIN_ROUND = 0
PEN_JOIN_BEVEL = 1
PEN_JOIN_MITER = 2

#TFPBrushStyle = (bsSolid, bsClear, bsHorizontal, bsVertical, bsFDiagonal,
#                 bsBDiagonal, bsCross, bsDiagCross, bsImage, bsPattern);
BRUSH_SOLID     = 0
BRUSH_CLEAR     = 1
BRUSH_HORZ      = 2
BRUSH_VERT      = 3
BRUSH_FDIAGONAL = 4
BRUSH_BDIAGONAL = 5
BRUSH_CROSS     = 6
BRUSH_DIAGCROSS = 7
#BRUSH_IMAGE     = 8
#BRUSH_PATTERN   = 9

ANTIALIAS_NONE = 0
ANTIALIAS_ON   = 1
ANTIALIAS_OFF  = 2

GAP_GET_LIST    = 0
GAP_MAKE_BITMAP = 1
GAP_ADD         = 2
GAP_DELETE      = 3
GAP_DELETE_ALL  = 4

FOLDING_GET_LIST           = 0
FOLDING_FOLD               = 1
FOLDING_UNFOLD             = 2
FOLDING_ADD                = 3
FOLDING_DELETE             = 4
FOLDING_DELETE_ALL         = 5
FOLDING_FIND               = 6
FOLDING_CHECK_RANGE_INSIDE = 10
FOLDING_CHECK_RANGES_SAME  = 11


def app_exe_version():
    return ct.app_exe_version()
def app_api_version():
    return ct.app_api_version()
def app_path(id):
    return ct.app_path(id)
def app_proc(id, text):
    return ct.app_proc(id, text)

def app_log(id, text, tag=0):
    return ct.app_log(id, text, tag)

def app_idle(wait=False):
    return ct.app_idle(wait)

def msg_box(text, flags):
    return ct.msg_box(text, flags)
def msg_status(text, process_messages=False):
    return ct.msg_status(text, process_messages)
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
    return ct.dlg_input_ex(number, caption,
                 label1, text1, label2, text2, label3, text3,
                 label4, text4, label5, text5, label6, text6,
                 label7, text7, label8, text8, label9, text9,
                 label10, text10)

def dlg_menu(id, text, focused=0):
    return ct.dlg_menu(id, text, focused)

def dlg_file(is_open, init_filename, init_dir, filters):
    return ct.dlg_file(is_open, init_filename, init_dir, filters)

def dlg_dir(init_dir):
    return ct.dlg_dir(init_dir)

def dlg_hotkey(title=''):
    return ct.dlg_hotkey(title)
def dlg_hotkeys(command, lexer=''):
    return ct.dlg_hotkeys(command, lexer)

def dlg_custom(title, size_x, size_y, text, focused=-1):
    return ct.dlg_custom(title, size_x, size_y, text, focused)

def file_open(filename, group=-1):
    return ct.file_open(filename, group)
def file_save(filename=''):
    return ct.file_save(filename)

def ed_handles():
    r0, r1 = ct.ed_handles()
    return range(r0, r1+1)
def ed_group(n):
    h = ct.ed_group(n)
    if h:
        return Editor(h)

def ini_read(filename, section, key, value):
    return ct.ini_read(filename, section, key, value)
def ini_write(filename, section, key, value):
    return ct.ini_write(filename, section, key, value)

def lexer_proc(id, value):
    return ct.lexer_proc(id, value)

def tree_proc(id_tree, id_action, id_item=0, index=0, text='', image_index=-1):
    return ct.tree_proc(id_tree, id_action, id_item, index, text, image_index)

def listbox_proc(id_listbox, id_action, index=0, text="", tag=0):
    return ct.listbox_proc(id_listbox, id_action, index, text, tag)

def canvas_proc(id_canvas, id_action, text='', color=-1, size=-1, x=-1, y=-1, x2=-1, y2=-1, style=-1, p1=-1, p2=-1):
    return ct.canvas_proc(id_canvas, id_action, text, color, size, x, y, x2, y2, style, p1, p2)

def timer_proc(id, name, value):
    return ct.timer_proc(id, name, value)


def to_str(v):
    if isinstance(v, list) or isinstance(v, tuple):
        return ','.join(map(to_str, v))

    if isinstance(v, bool):
        if v:
            return '1'
        else:
            return '0'

    return str(v)

#Editor
class Editor:
    h = 0
    def __init__(self, handle):
        self.h = handle

    def get_carets(self):
        return ct.ed_get_carets(self.h)

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

    def save(self, filename=''):
        return ct.ed_save(self.h, filename)
    def cmd(self, code, text=''):
        return ct.ed_cmd(self.h, code, text)
    def focus(self):
        return ct.ed_focus(self.h)
    def bookmark(self, id, nline, nkind=1, ncolor=-1, text=''):
        return ct.ed_bookmark(self.h, id, nline, nkind, ncolor, text)

    def lock(self):
        return ct.ed_lock(self.h)
    def unlock(self):
        return ct.ed_unlock(self.h)

    def get_split(self):
        return ct.ed_get_split(self.h)
    def set_split(self, state, value):
        return ct.ed_set_split(self.h, state, value)

    def get_prop(self, id, value=''):
        value = to_str(value)
        if id!=PROP_TAG:
            return ct.ed_get_prop(self.h, id, value)
        js_s = ct.ed_get_prop(self.h, PROP_TAG, '')
        key,dfv = value.split(':', 1) if ':' in value else ('_', value)
        if not js_s:
            return dfv
        js = json.loads(js_s)
        return js.get(key, dfv)

    def set_prop(self, id, value):
        value = to_str(value)
        if id!=PROP_TAG:
            return ct.ed_set_prop(self.h, id, value)
        key,val = value.split(':', 1) if ':' in value else ('_', value)
        js_s = ct.ed_get_prop(self.h, PROP_TAG, '')
        js_s = js_s if js_s else '{}'
        js = json.loads(js_s)
        js[key] = val
        js_s = json.dumps(js)
        return ct.ed_set_prop(self.h, PROP_TAG, js_s)

    def complete(self, text, len1, len2, selected=0, alt_order=False):
        return ct.ed_complete(self.h, text, len1, len2, selected, alt_order)
    def complete_alt(self, text, snippet_id, len_chars, selected=0):
        return ct.ed_complete_alt(self.h, text, snippet_id, len_chars, selected)

    def convert(self, id, x, y, text=''):
        return ct.ed_convert(self.h, id, x, y, text)

    def get_ranges(self):
        return ct.ed_get_ranges(self.h)

    def get_sublexer_ranges(self):
        res = ct.ed_get_sublexer_ranges(self.h)
        if res is None: return
        #split string to items
        #note: EControl gives duplicated ranges, cannot find reason, del them here
        res = res.rstrip(';').split(';')
        res = [ r.split(',') for (index, r) in enumerate(res) if (index==0) or (res[index]!=res[index-1]) ]
        res = [ (r[4], int(r[0]), int(r[1]), int(r[2]), int(r[3])) for r in res ]
        return res

    def markers(self, id, x=0, y=0, tag=0, len_x=0, len_y=0):
        return ct.ed_markers(self.h, id, x, y, tag, len_x, len_y)

    def attr(self, id, tag=0, x=0, y=0, len=0,
             color_font=COLOR_NONE, color_bg=COLOR_NONE, color_border=COLOR_NONE,
             font_bold=0, font_italic=0, font_strikeout=0,
             border_left=0, border_right=0, border_down=0, border_up=0
             ):
        if color_font==COLOR_NONE:
            color_font = self.get_prop(PROP_COLOR, COLOR_ID_TextFont)
        if color_border==COLOR_NONE:
            color_border = self.get_prop(PROP_COLOR, COLOR_ID_TextFont)
        return ct.ed_attr(self.h, id, tag, x, y, len,
                          color_font, color_bg, color_border,
                          font_bold, font_italic, font_strikeout,
                          border_left, border_right, border_down, border_up
                          )

    def get_token(self, id, index1, index2):
        return ct.ed_get_token(self.h, id, index1, index2)

    def gap(self, id, num1, num2, tag=-1):
        return ct.ed_gap(self.h, id, num1, num2, tag)

    def folding(self, id, index=-1, item_x=-1, item_y=-1, item_y2=-1, item_staple=False, item_hint=''):
        return ct.ed_folding(self.h, id, index, item_x, item_y, item_y2, item_staple, item_hint)

    def lexer_scan(self, num):
        return ct.ed_lexer_scan(self.h, num)
    #end

#objects
ed = Editor(0)
ed_bro = Editor(1)
