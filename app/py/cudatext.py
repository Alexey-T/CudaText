import cudatext_api as ct

MB_OK               = 0x00
MB_OKCANCEL         = 0x01
MB_ABORTRETRYIGNORE = 0x02
MB_YESNOCANCEL      = 0x03
MB_YESNO            = 0x04
MB_RETRYCANCEL      = 0x05
MB_ICONERROR        = 0x10
MB_ICONQUESTION     = 0x20
MB_ICONWARNING      = 0x30
MB_ICONINFO         = 0x40

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
CARET_DELETE_INDEX = 2000000

CARET_OPTION_NO_SCROLL = 1
CARET_OPTION_UNFOLD = 2

APP_DIR_EXE             = 0
APP_DIR_SETTINGS        = 1
APP_DIR_DATA            = 2
APP_DIR_PY              = 3
APP_FILE_SESSION        = 4
APP_DIR_INSTALLED_ADDON = 5
APP_FILE_RECENTS        = 6
APP_DIR_SETTINGS_DEF    = 7

CONVERT_CHAR_TO_COL         = 0
CONVERT_COL_TO_CHAR         = 1
CONVERT_LINE_TABS_TO_SPACES = 2
CONVERT_SCREEN_TO_LOCAL     = 5
CONVERT_LOCAL_TO_SCREEN     = 6
CONVERT_PIXELS_TO_CARET     = 7
CONVERT_CARET_TO_PIXELS     = 8

TOKEN_GET_KIND = 1
TOKEN_LIST     = 2
TOKEN_LIST_SUB = 3

LINESTATE_NORMAL  = 0
LINESTATE_CHANGED = 1
LINESTATE_ADDED   = 2
LINESTATE_SAVED   = 3

LINENUM_ALL    = 0
LINENUM_NONE   = 1
LINENUM_EACH10 = 2
LINENUM_EACH5  = 3

COLOR_NONE = 0x1FFFFFFF
COLOR_DEFAULT = 0x20000000

WRAP_OFF       = 0
WRAP_ON_WINDOW = 1
WRAP_ON_MARGIN = 2

#dlg_menu
###deprecated-- were used until API 362-- to delete after 2021.04
MENU_LIST     = 0
MENU_LIST_ALT = 1
MENU_NO_FUZZY = 16
MENU_NO_FULLFILTER = 32
MENU_CENTERED = 64
MENU_EDITORFONT = 128
###new OK constants
DMENU_LIST          = 0
DMENU_LIST_ALT      = 1
DMENU_NO_FUZZY      = 16
DMENU_NO_FULLFILTER = 32
DMENU_CENTERED      = 64
DMENU_EDITORFONT    = 128

CLIP_NONE   = 0
CLIP_LEFT   = 1
CLIP_MIDDLE = 2
CLIP_RIGHT  = 3

#menu_proc
MENU_CLEAR         = 0
MENU_ENUM          = 1
MENU_ADD           = 2
MENU_REMOVE        = 3
MENU_SET_CAPTION   = 4
MENU_SET_VISIBLE   = 5
MENU_SET_ENABLED   = 6
MENU_SET_CHECKED   = 7
MENU_SET_RADIOITEM = 8
MENU_SET_HOTKEY    = 9
MENU_CREATE        = 10
MENU_SHOW          = 12
MENU_GET_PROP      = 14
MENU_SET_IMAGELIST = 20
MENU_SET_IMAGEINDEX = 21

BOOKMARK_SET         = 1
BOOKMARK_CLEAR       = 2
BOOKMARK_CLEAR_ALL   = 3
BOOKMARK_SETUP       = 4
BOOKMARK_GET_LIST    = 5
BOOKMARK_GET_ALL     = 6
BOOKMARK_GET_PROP      = 7
BOOKMARK_DELETE_BY_TAG = 8

BOOKMARK2_SET         = 11
BOOKMARK2_CLEAR       = 12
BOOKMARK2_CLEAR_ALL   = 13
BOOKMARK2_GET_ALL     = 16
BOOKMARK2_GET_PROP      = 17
BOOKMARK2_DELETE_BY_TAG = 18

MARKERS_GET           = 0
MARKERS_ADD           = 1
MARKERS_DELETE_ALL    = 2
MARKERS_DELETE_LAST   = 3
MARKERS_DELETE_BY_TAG = 4
MARKERS_DELETE_BY_INDEX = 5
MARKERS_ADD_MANY      = 6
MARKERS_GET_DICT      = 7

TIMER_START     = 0
TIMER_START_ONE = 1
TIMER_STOP      = 2
TIMER_DELETE    = 3

LOG_CLEAR           = 0
LOG_ADD             = 1
LOG_SET_REGEX       = 3
LOG_SET_LINE_ID     = 4
LOG_SET_COL_ID      = 5
LOG_SET_NAME_ID     = 6
LOG_SET_FILENAME    = 7
LOG_SET_ZEROBASE    = 8
LOG_GET_LINEINDEX   = 10
LOG_SET_LINEINDEX   = 11
LOG_GET_LINES_LIST  = 12
LOG_CONSOLE_CLEAR   = 20
LOG_CONSOLE_ADD     = 21
LOG_CONSOLE_GET_COMBO_LINES = 24
LOG_CONSOLE_GET_MEMO_LINES  = 25

LOG_PANEL_OUTPUT   = "0"
LOG_PANEL_VALIDATE = "1"

PROP_GUTTER_ALL     = -1
PROP_GUTTER_STATES  = 0
PROP_GUTTER_NUM     = 1
PROP_GUTTER_FOLD    = 2
PROP_GUTTER_BM      = 3
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
PROP_CARET_VIEW           = 26
PROP_CARET_VIEW_OVR       = 27
PROP_CARET_VIEW_RO        = 28
PROP_CARET_VIRTUAL         = 29
PROP_UNPRINTED_SHOW        = 30
PROP_UNPRINTED_SPACES      = 31
PROP_UNPRINTED_ENDS        = 32
PROP_UNPRINTED_END_DETAILS = 33
PROP_TAB_ICON              = 34
PROP_TAB_COLLECT_MARKERS   = 35
PROP_MACRO_REC             = 36
PROP_TAB_PINNED            = 37
PROP_MARKED_RANGE          = 38
PROP_LINE_NUMBERS          = 39
PROP_VISIBLE_LINES         = 40
PROP_VISIBLE_COLUMNS       = 41
PROP_LINE_BOTTOM           = 42
PROP_PICTURE               = 43
PROP_MINIMAP               = 44
PROP_MICROMAP              = 45
PROP_LINK_AT_POS           = 46
PROP_MODIFIED_VERSION      = 47
PROP_TAB_ID                = 48
PROP_IN_SESSION            = 49
PROP_COORDS                = 50
PROP_ONE_LINE              = 51
PROP_SCROLL_VERT           = 52
PROP_SCROLL_HORZ           = 53
PROP_CODETREE              = 54
PROP_EDITORS_LINKED        = 55
PROP_IN_HISTORY            = 56
PROP_SCROLL_VERT_SMOOTH    = 57
PROP_SCROLL_HORZ_SMOOTH    = 58
PROP_KIND                  = 59
PROP_V_MODE                = 60
PROP_V_POS                 = 61
PROP_V_SEL_START           = 62
PROP_V_SEL_LEN             = 63
PROP_V_WIDTH               = 64
PROP_NEWLINE               = 65
PROP_SCROLL_VERT_INFO      = 66
PROP_SCROLL_HORZ_INFO      = 67
PROP_SCALE_FONT            = 68
PROP_ENC_RELOAD            = 69
PROP_CELL_SIZE             = 70
PROP_INDENT_SIZE           = 71
PROP_INDENT_KEEP_ALIGN     = 72
PROP_INDENT_AUTO           = 73
PROP_INDENT_KIND           = 74
PROP_LAST_LINE_ON_TOP      = 75
PROP_UNPRINTED_SPACES_TRAILING = 76
PROP_HILITE_CUR_COL            = 77
PROP_HILITE_CUR_LINE           = 78
PROP_HILITE_CUR_LINE_MINIMAL   = 79
PROP_HILITE_CUR_LINE_IF_FOCUS  = 80
PROP_CARET_STOP_UNFOCUSED      = 81
PROP_ACTIVATION_TIME           = 82
PROP_FOCUSED                   = 83
PROP_MODERN_SCROLLBAR          = 85
PROP_SAVE_HISTORY              = 86
PROP_PREVIEW                   = 87
PROP_UNDO_GROUPED              = 88
PROP_UNDO_LIMIT                = 89
PROP_UNDO_DATA                 = 90
PROP_REDO_DATA                 = 91
PROP_SPLIT                     = 92
PROP_SAVING_FORCE_FINAL_EOL    = 93
PROP_SAVING_TRIM_SPACES        = 94
PROP_SAVING_TRIM_FINAL_EMPTY_LINES = 95
PROP_ZEBRA                     = 96
PROP_ZEBRA_STEP                = 97
PROP_LINE_STATES               = 99
PROP_LINE_STATES_UPDATED       = 100
PROP_FOLD_TOOLTIP_SHOW         = 101
PROP_FOLD_ALWAYS               = 102
PROP_FOLD_ICONS                = 103
PROP_SCROLLSTYLE_HORZ          = 104
PROP_SCROLLSTYLE_VERT          = 105
PROP_LINKS_SHOW                = 106
PROP_LINKS_REGEX               = 107
PROP_LINKS_CLICKS              = 108
PROP_HANDLE_SELF        = 110
PROP_HANDLE_PRIMARY     = 111
PROP_HANDLE_SECONDARY   = 112
PROP_HANDLE_PARENT      = 113
PROP_RECT_CLIENT        = 115
PROP_RECT_TEXT          = 116
PROP_COMBO_ITEMS        = 119
PROP_CODETREE_MODIFIED_VERSION = 120
PROP_CODETREE_SUBLEXER         = 121
PROP_FONT                      = 122
PROP_FONT_B                    = 123
PROP_FONT_I                    = 124
PROP_FONT_BI                   = 125
PROP_RECT_GUTTER      = 130
#PROP_RECT_GUTTER_NUMS = 131
#PROP_RECT_GUTTER_FOLD = 132
#PROP_RECT_GUTTER_BM   = 133
PROP_RECT_MINIMAP     = 135
PROP_RECT_MICROMAP    = 136
PROP_RECT_RULER       = 137
#PROP_RECT_SCROLL_HORZ = 138
#PROP_RECT_SCROLL_VERT = 139
PROP_V_WIDTH_HEX        = 140
PROP_V_WIDTH_UHEX       = 141
PROP_MASKCHAR           = 150
PROP_MASKCHAR_USED      = 151
PROP_NUMBERS_ONLY       = 153
PROP_NUMBERS_NEGATIVE   = 154

SPLITTER_SIDE    = 0
SPLITTER_BOTTOM  = 1
SPLITTER_G1      = 5
SPLITTER_G2      = 6
SPLITTER_G3      = 7
SPLITTER_G4      = 8
SPLITTER_G5      = 9

PROC_SET_CLIP_ALT        = -1
PROC_GET_CLIP            = 0
PROC_SET_CLIP            = 1
PROC_GET_OS_SUFFIX       = 2
PROC_SAVE_SESSION        = 3
PROC_LOAD_SESSION        = 4
PROC_SET_SESSION         = 5
PROC_SET_FOLDER          = 7
PROC_GET_COMMANDS        = 8
PROC_SET_EVENTS          = 10
PROC_GET_LAST_PLUGIN     = 11
PROC_GET_GROUPING        = 12
PROC_SET_GROUPING        = 13
PROC_EXEC_PYTHON         = 14
PROC_EXEC_PLUGIN         = 15
PROC_SET_SUBCOMMANDS     = 16
PROC_GET_ESCAPE          = 17
PROC_SET_ESCAPE          = 18
PROC_GET_FIND_OPTIONS    = 22
PROC_SET_FIND_OPTIONS    = 23
#
PROC_SIDEPANEL_ACTIVATE    = 25
PROC_SIDEPANEL_ENUM        = 26
PROC_SIDEPANEL_GET_CONTROL = 27
PROC_SIDEPANEL_ENUM_ALL    = 28
PROC_SIDEPANEL_REMOVE      = 29
PROC_SIDEPANEL_ADD_DIALOG  = 30
PROC_SIDEPANEL_GET         = 31
PROC_SIDEPANEL_SET_PROP    = 32
PROC_SIDEPANEL_GET_IMAGELIST = 33
#
PROC_GET_FINDER_PROP   = 36
PROC_SET_FINDER_PROP   = 37
PROC_SPLITTER_GET      = 38
PROC_SPLITTER_SET      = 39
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
PROC_GET_SYSTEM_PPI    = 50
PROC_PROGRESSBAR       = 51
PROC_GET_TAB_IMAGELIST = 52
PROC_GET_MOUSE_POS     = 53
PROC_GET_CONSOLE_FORM  = 55
PROC_GET_CODETREE = 56
PROC_WINDOW_TOPMOST_GET = 57
PROC_WINDOW_TOPMOST_SET = 58
PROC_HOTKEY_INT_TO_STR = 60
PROC_HOTKEY_STR_TO_INT = 61
PROC_THEME_UI_DICT_GET     = 62
PROC_THEME_SYNTAX_DICT_GET = 63
PROC_GET_OUTPUT_FORM   = 64
PROC_GET_VALIDATE_FORM = 65
#
PROC_BOTTOMPANEL_ACTIVATE    = 81
PROC_BOTTOMPANEL_ENUM        = 82
PROC_BOTTOMPANEL_GET_CONTROL = 83
PROC_BOTTOMPANEL_REMOVE      = 84
PROC_BOTTOMPANEL_ADD_DIALOG  = 85
PROC_BOTTOMPANEL_GET         = 86
PROC_BOTTOMPANEL_SET_PROP    = 87
PROC_BOTTOMPANEL_ENUM_ALL    = 88
#
PROC_CLIP_ENUM            = 90
PROC_CLIP_SAVE_PIC        = 91
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
PROC_SHOW_SIDEBAR_GET     = 110
PROC_SHOW_SIDEBAR_SET     = 111
PROC_SHOW_FLOATGROUP1_GET = 112
PROC_SHOW_FLOATGROUP1_SET = 113
PROC_SHOW_FLOATGROUP2_GET = 114
PROC_SHOW_FLOATGROUP2_SET = 115
PROC_SHOW_FLOATGROUP3_GET = 116
PROC_SHOW_FLOATGROUP3_SET = 117
PROC_SHOW_TREEFILTER_GET  = 118
PROC_SHOW_TREEFILTER_SET  = 119
#
PROC_FLOAT_SIDE_GET       = 120
PROC_FLOAT_SIDE_SET       = 121
PROC_FLOAT_BOTTOM_GET     = 122
PROC_FLOAT_BOTTOM_SET     = 123
#
PROC_COORD_WINDOW_GET = 140
PROC_COORD_WINDOW_SET = 141
PROC_COORD_DESKTOP    = 142
PROC_COORD_MONITOR    = 143
PROC_COORD_MONITOR0   = 144
PROC_COORD_MONITOR1   = 145
PROC_COORD_MONITOR2   = 146
PROC_COORD_MONITOR3   = 147
#
PROC_SET_PROJECT      = 158
PROC_GET_UNIQUE_TAG   = 159
PROC_ENUM_FONTS       = 160
PROC_SEND_MESSAGE     = 161

PROC_CONFIG_READ           = 169
PROC_CONFIG_NEWDOC_EOL_GET = 170
PROC_CONFIG_NEWDOC_EOL_SET = 171
PROC_CONFIG_NEWDOC_ENC_GET = 172
PROC_CONFIG_NEWDOC_ENC_SET = 173
PROC_CONFIG_SCALE_GET      = 175
PROC_CONFIG_SCALE_SET      = 176

TREE_ITEM_ENUM_EX          = 0
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
TREE_ITEM_SET_RANGE        = 13
TREE_ITEM_GET_RANGE        = 14
TREE_ITEM_FOLD_LEVEL       = 15
TREE_ITEM_SHOW             = 16
TREE_ITEM_GET_PROPS        = 17
TREE_GET_IMAGELIST         = 25
TREE_PROP_SHOW_ROOT        = 30
TREE_LOCK                  = 31
TREE_UNLOCK                = 32
TREE_THEME                 = 33

LISTBOX_GET_COUNT    = 0
LISTBOX_ADD          = 1
LISTBOX_DELETE       = 2
LISTBOX_DELETE_ALL   = 3
LISTBOX_GET_ITEM     = 4
LISTBOX_SET_ITEM     = 5
LISTBOX_GET_HOTTRACK = 6
LISTBOX_SET_HOTTRACK = 7
LISTBOX_GET_ITEM_PROP = 8
LISTBOX_SET_ITEM_PROP = 9
LISTBOX_GET_SEL      = 10
LISTBOX_SET_SEL      = 11
LISTBOX_ADD_PROP     = 12
LISTBOX_GET_TOP      = 14
LISTBOX_SET_TOP      = 15
LISTBOX_GET_ITEM_H   = 16
LISTBOX_SET_ITEM_H   = 17
LISTBOX_GET_DRAWN    = 18
LISTBOX_SET_DRAWN    = 19
LISTBOX_THEME        = 20
LISTBOX_GET_COLUMN_SEP = 22
LISTBOX_SET_COLUMN_SEP = 23
LISTBOX_GET_COLUMNS = 24
LISTBOX_SET_COLUMNS = 25
LISTBOX_GET_SHOW_X  = 26
LISTBOX_SET_SHOW_X  = 27
LISTBOX_GET_SCROLLPOS_HORZ = 30
LISTBOX_SET_SCROLLPOS_HORZ = 31
LISTBOX_GET_SCROLLSTYLE_HORZ = 32
LISTBOX_SET_SCROLLSTYLE_HORZ = 33
LISTBOX_GET_SCROLLSTYLE_VERT = 34
LISTBOX_SET_SCROLLSTYLE_VERT = 35
LISTBOX_GET_HEADER  = 36
LISTBOX_SET_HEADER  = 37
LISTBOX_GET_HEADER_IMAGELIST = 38
LISTBOX_SET_HEADER_IMAGELIST = 39
LISTBOX_GET_HEADER_IMAGEINDEXES = 40
LISTBOX_SET_HEADER_IMAGEINDEXES = 41

SCROLLSTYLE_HIDE = 0
SCROLLSTYLE_SHOW = 1
SCROLLSTYLE_AUTO = 2

LEXER_GET_LEXERS     = 0
LEXER_GET_PROP       = 1
LEXER_DETECT         = 2
LEXER_REREAD_LIB     = 3
LEXER_GET_STYLES     = 4
LEXER_ADD_VIRTUAL    = 5

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
GROUPS_6VERT   = 11
GROUPS_6HORZ   = 12
GROUPS_6GRID   = 13

EMMET_EXPAND   = 0
EMMET_WRAP     = 1
EMMET_GET_POS  = 2

EDACTION_UPDATE         = 0
EDACTION_UNDOGROUP_BEGIN = 1
EDACTION_UNDOGROUP_END   = 2
EDACTION_REPLACE_ALL    = 4
EDACTION_REPLACE_ONE    = 5
EDACTION_FIND_ONE       = 6
EDACTION_FIND_ALL       = 7
EDACTION_FIND_BRACKETS  = 8
EDACTION_SHOW_POS       = 9
EDACTION_CODETREE_FILL  = 10
EDACTION_LEXER_SCAN     = 11
EDACTION_EXPORT_HTML    = 15

MICROMAP_GET    = 0
MICROMAP_ADD    = 1
MICROMAP_DELETE = 2

APPSTATE_LANG          = 20
APPSTATE_THEME_UI      = 21
APPSTATE_THEME_SYNTAX  = 22
APPSTATE_GROUPS        = 23
APPSTATE_CONFIG_REREAD = 24
APPSTATE_SESSION_LOAD  = 25
APPSTATE_SESSION_LOAD_BEGIN = 26
APPSTATE_SESSION_LOAD_FAIL = 27
APPSTATE_PROJECT       = 28
APPSTATE_CODETREE_CLEAR         = 40
APPSTATE_CODETREE_BEFORE_FILL   = 41
APPSTATE_CODETREE_AFTER_FILL    = 42
APPSTATE_CODETREE_SET_SELECTION = 43

EDSTATE_WRAP      = 1
EDSTATE_TAB_TITLE = 2
EDSTATE_MODIFIED  = 3
EDSTATE_PINNED    = 4
EDSTATE_READONLY  = 5

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
COLOR_ID_MinimapTooltipBg = 'EdMinimapTooltipBg'
COLOR_ID_MinimapTooltipBorder = 'EdMinimapTooltipBorder'
COLOR_ID_StateChanged = 'EdStateChanged'
COLOR_ID_StateAdded = 'EdStateAdded'
COLOR_ID_StateSaved = 'EdStateSaved'
COLOR_ID_BlockStaple = 'EdBlockStaple'
COLOR_ID_BlockStapleActive = 'EdBlockStapleActive'
COLOR_ID_BlockSepLine = 'EdBlockSepLine'
COLOR_ID_Links = 'EdLinks'
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
COLOR_ID_Border = 'EdBorder'
COLOR_ID_BorderFocused = 'EdBorderFocused'

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

GAP_MAKE_BITMAP = 1
GAP_ADD         = 2
GAP_DELETE      = 3
GAP_DELETE_ALL  = 4
GAP_GET_ALL     = 5

FOLDING_GET_LIST           = 0
FOLDING_FOLD               = 1
FOLDING_UNFOLD             = 2
FOLDING_ADD                = 3
FOLDING_DELETE             = 4
FOLDING_DELETE_ALL         = 5
FOLDING_FIND               = 6
FOLDING_UNFOLD_LINE        = 7
FOLDING_FOLD_ALL           = 8
FOLDING_UNFOLD_ALL         = 9
FOLDING_CHECK_RANGE_INSIDE = 10
FOLDING_CHECK_RANGES_SAME  = 11
FOLDING_FOLD_LEVEL         = 12
FOLDING_GET_LIST_FILTERED  = 13

COMMANDS_USUAL   = 1
COMMANDS_PLUGINS = 2
COMMANDS_LEXERS  = 4
COMMANDS_CONFIG  = 8
COMMANDS_CENTERED = 16
COMMANDS_FILES    = 32
COMMANDS_RECENTS  = 64
COMMANDS_CONFIG_LEXER = 128

TOOLBAR_UPDATE         = 1
TOOLBAR_GET_BUTTON_HANDLE = 2
TOOLBAR_GET_COUNT      = 3
TOOLBAR_GET_BUTTON_HANDLES = 4
TOOLBAR_GET_INDEX_HOVERED = 5
TOOLBAR_DELETE_ALL     = 6
TOOLBAR_DELETE_BUTTON  = 7
TOOLBAR_ADD_ITEM       = 8
TOOLBAR_ADD_MENU       = 9
TOOLBAR_GET_IMAGELIST  = 12
TOOLBAR_GET_WRAP       = 14
TOOLBAR_SET_WRAP       = 15
TOOLBAR_GET_VERTICAL   = 16
TOOLBAR_SET_VERTICAL   = 17
TOOLBAR_THEME          = 20

DLG_CREATE         = 0
DLG_FREE           = 1
DLG_LOCK           = 2
DLG_UNLOCK         = 3
DLG_SHOW_MODAL     = 5
DLG_SHOW_NONMODAL  = 6
DLG_HIDE           = 7
DLG_FOCUS          = 8
DLG_SCALE          = 9
DLG_PROP_GET       = 10
DLG_PROP_SET       = 11
DLG_DOCK           = 12
DLG_UNDOCK         = 13
DLG_TO_FRONT       = 14
#DLG_TO_BACK        = 15
DLG_CTL_COUNT      = 20
DLG_CTL_ADD        = 21
DLG_CTL_PROP_GET   = 22
DLG_CTL_PROP_SET   = 23
DLG_CTL_DELETE     = 24
DLG_CTL_DELETE_ALL = 25
DLG_CTL_TO_FRONT   = 26
DLG_CTL_TO_BACK    = 27
DLG_CTL_FOCUS      = 30
DLG_CTL_FIND       = 31
DLG_CTL_HANDLE     = 32
DLG_COORD_LOCAL_TO_SCREEN = 40
DLG_COORD_SCREEN_TO_LOCAL = 41
DLG_POS_GET_STR      = 45
DLG_POS_SET_FROM_STR = 46

#storage of live callbacks
_live = {}

IMAGE_CREATE      = 0
IMAGE_GET_SIZE    = 1
IMAGE_LOAD        = 2
IMAGE_PAINT       = 5
IMAGE_PAINT_SIZED = 6

IMAGELIST_CREATE     = 0
IMAGELIST_COUNT      = 1
IMAGELIST_GET_SIZE   = 2
IMAGELIST_SET_SIZE   = 3
IMAGELIST_ADD        = 5
IMAGELIST_DELETE     = 6
IMAGELIST_DELETE_ALL = 7
IMAGELIST_PAINT      = 10

BTN_UPDATE         = -1
BTN_GET_CHECKED    = 0
BTN_SET_CHECKED    = 1
BTN_GET_IMAGELIST  = 2
BTN_SET_IMAGELIST  = 3
BTN_GET_IMAGEINDEX = 4
BTN_SET_IMAGEINDEX = 5
BTN_GET_KIND       = 6
BTN_SET_KIND       = 7
BTN_GET_BOLD       = 8
BTN_SET_BOLD       = 9
BTN_GET_ENABLED    = 10
BTN_SET_ENABLED    = 11
BTN_GET_VISIBLE    = 12
BTN_SET_VISIBLE    = 13
BTN_GET_HINT       = 14
BTN_SET_HINT       = 15
BTN_GET_TEXT       = 16
BTN_SET_TEXT       = 17
BTN_GET_MENU       = 18
BTN_SET_MENU       = 19
BTN_GET_DATA1      = 20
BTN_SET_DATA1      = 21
BTN_GET_DATA2      = 22
BTN_SET_DATA2      = 23
BTN_GET_ARROW      = 30
BTN_SET_ARROW      = 31
BTN_GET_FOCUSABLE  = 32
BTN_SET_FOCUSABLE  = 33
BTN_GET_FLAT       = 34
BTN_SET_FLAT       = 35
BTN_GET_WIDTH      = 38
BTN_SET_WIDTH      = 39
BTN_GET_ITEMS      = 40
BTN_SET_ITEMS      = 41
BTN_GET_ITEMINDEX  = 42
BTN_SET_ITEMINDEX  = 43
BTN_GET_ARROW_ALIGN = 44
BTN_SET_ARROW_ALIGN = 45
BTN_GET_OVERLAY = 46
BTN_SET_OVERLAY = 47
BTN_GET_COLOR_LINE  = 48
BTN_SET_COLOR_LINE  = 49
BTN_GET_COLOR_LINE2 = 50
BTN_SET_COLOR_LINE2 = 51

BTNKIND_TEXT_ONLY      = 0
BTNKIND_ICON_ONLY      = 1
BTNKIND_TEXT_ICON_HORZ = 2
BTNKIND_TEXT_ICON_VERT = 3
BTNKIND_SEP_HORZ       = 4
BTNKIND_SEP_VERT       = 5
BTNKIND_TEXT_CHOICE    = 6

ALIGN_NONE    = 0
ALIGN_TOP     = 1
ALIGN_BOTTOM  = 2
ALIGN_LEFT    = 3
ALIGN_RIGHT   = 4
ALIGN_CLIENT  = 5

DIM_ENUM       = 0
DIM_ADD        = 1
DIM_DELETE     = 3
DIM_DELETE_ALL = 4

VMODE_TEXT        = 0
VMODE_BINARY      = 1
VMODE_HEX         = 2
VMODE_UNICODE     = 3
VMODE_UNICODE_HEX = 4
VMODE_NONE        = 10

STATUSBAR_GET_COUNT            = 0
STATUSBAR_DELETE_ALL           = 1
STATUSBAR_DELETE_CELL          = 2
STATUSBAR_ADD_CELL             = 3
STATUSBAR_FIND_CELL            = 4
STATUSBAR_SET_IMAGELIST        = 5
STATUSBAR_GET_IMAGELIST        = 6

STATUSBAR_SET_CELL_OVERLAY     = 14
STATUSBAR_SET_CELL_COLOR_LINE  = 15
STATUSBAR_SET_CELL_COLOR_LINE2 = 16
STATUSBAR_SET_CELL_CALLBACK    = 17
STATUSBAR_SET_CELL_FONT_NAME   = 18
STATUSBAR_SET_CELL_FONT_SIZE   = 19
STATUSBAR_SET_CELL_SIZE        = 20
STATUSBAR_SET_CELL_ALIGN       = 21
STATUSBAR_SET_CELL_TEXT        = 22
STATUSBAR_SET_CELL_IMAGEINDEX  = 23
STATUSBAR_SET_CELL_COLOR_FONT  = 24
STATUSBAR_SET_CELL_COLOR_BACK  = 25
STATUSBAR_SET_CELL_TAG         = 26
STATUSBAR_SET_CELL_AUTOSIZE    = 27
STATUSBAR_SET_CELL_AUTOSTRETCH = 28
STATUSBAR_SET_CELL_HINT        = 29

STATUSBAR_GET_CELL_SIZE        = 30
STATUSBAR_GET_CELL_ALIGN       = 31
STATUSBAR_GET_CELL_TEXT        = 32
STATUSBAR_GET_CELL_IMAGEINDEX  = 33
STATUSBAR_GET_CELL_COLOR_FONT  = 34
STATUSBAR_GET_CELL_COLOR_BACK  = 35
STATUSBAR_GET_CELL_TAG         = 36
STATUSBAR_GET_CELL_AUTOSIZE    = 37
STATUSBAR_GET_CELL_AUTOSTRETCH = 38
STATUSBAR_GET_CELL_HINT        = 39
STATUSBAR_GET_CELL_FONT_NAME   = 40
STATUSBAR_GET_CELL_FONT_SIZE   = 41
STATUSBAR_GET_CELL_CALLBACK    = 42
STATUSBAR_GET_CELL_COLOR_LINE  = 43
STATUSBAR_GET_CELL_COLOR_LINE2 = 44
STATUSBAR_GET_CELL_OVERLAY     = 45

STATUSBAR_SET_COLOR_BACK       = 50
STATUSBAR_SET_COLOR_BORDER_TOP = 52
STATUSBAR_SET_COLOR_BORDER_L   = 53
STATUSBAR_SET_COLOR_BORDER_R   = 54
STATUSBAR_SET_COLOR_BORDER_U   = 55
STATUSBAR_SET_COLOR_BORDER_D   = 56

STATUSBAR_GET_COLOR_BACK       = 60
STATUSBAR_GET_COLOR_BORDER_TOP = 62
STATUSBAR_GET_COLOR_BORDER_L   = 63
STATUSBAR_GET_COLOR_BORDER_R   = 64
STATUSBAR_GET_COLOR_BORDER_U   = 65
STATUSBAR_GET_COLOR_BORDER_D   = 66

HOTSPOT_GET_LIST      = 0
HOTSPOT_ADD           = 1
HOTSPOT_DELETE_ALL    = 2
HOTSPOT_DELETE_LAST   = 3
HOTSPOT_DELETE_BY_TAG = 4

DBORDER_NONE     = 0
DBORDER_SINGLE   = 1
DBORDER_SIZE     = 2
DBORDER_DIALOG   = 3
DBORDER_TOOL     = 4
DBORDER_TOOLSIZE = 5

DECOR_GET_ALL          = 0
DECOR_GET              = 1
DECOR_SET              = 2
DECOR_DELETE_BY_LINE   = 5
DECOR_DELETE_BY_TAG    = 6
DECOR_DELETE_ALL       = 7
DECOR_GET_IMAGELIST    = 10

INI_GET_SECTIONS     = 0
INI_GET_KEYS         = 1
INI_DELETE_KEY       = 2
INI_DELETE_SECTION   = 3

FINDER_CREATE           = 0
FINDER_FREE             = 1
FINDER_CLEAR            = 2
FINDER_SET_FINDTEXT     = 4
FINDER_GET_FINDTEXT     = 5
FINDER_SET_REPTEXT      = 6
FINDER_GET_REPTEXT      = 7
FINDER_SET_ED           = 8
FINDER_GET_ED           = 9
FINDER_SET_OPT          = 10
FINDER_GET_OPT          = 11
FINDER_SET_MAXLEN       = 12
FINDER_GET_MAXLEN       = 13
FINDER_SET_CARETS       = 14
FINDER_GET_CARETS       = 15
FINDER_SET_INDENTS      = 16
FINDER_GET_INDENTS      = 17
FINDER_SET_TAG          = 18
FINDER_GET_TAG          = 19
FINDER_SET_ON_REPLACE   = 20
FINDER_GET_ON_REPLACE   = 21
FINDER_FIND             = 30
FINDER_FIND_REP         = 31
FINDER_FIND_ALL         = 32
FINDER_COUNT            = 33
FINDER_REP_ALL          = 38
FINDER_REP_ALL_EX       = 39

HOWREP_CANCEL  = 0
HOWREP_REPLACE = 1
HOWREP_SKIP    = 2

HINTPOS_CARET         = 0
HINTPOS_CARET_BRACKET = 1
HINTPOS_TEXT          = 2
HINTPOS_TEXT_BRACKET  = 3
HINTPOS_WINDOW_TOP    = 8
HINTPOS_WINDOW_BOTTOM = 9

API, EXE_VER = ct.app_ver()

def app_exe_version():
    return EXE_VER

def app_api_version():
    return '1.0.'+str(API)

def app_path(id):
    return ct.app_path(id)

def app_proc(id, val):
    return ct.app_proc(id, to_str(val))

def app_log(id, text, tag=0, panel=''):
    if type(text) is not str:
        raise ValueError('app_log() parameter "text" must be string')
    return ct.app_log(id, text, tag, panel)

def app_idle(wait=False):
    return ct.app_idle(wait)

def emmet(id, text, p1='', p2=''):
    return ct.emmet(id, text, str(p1), str(p2))

def msg_box(text, flags):
    return ct.msg_box(text, flags)

def msg_box_ex(caption, text, buttons, icon, focused=0):
    return ct.msg_box_ex(caption, text, chr(1).join(buttons), icon, focused)

def msg_status(text, process_messages=False):
    return ct.msg_status(text, process_messages)

def msg_status_alt(text, pause, pos=HINTPOS_CARET, x=0, y=0):
    return ct.msg_status_alt(text, pause, pos, x, y)

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

def dlg_menu(id, items, focused=0, caption='', clip=0, w=0, h=0):
    if isinstance(items, str):
        s = items
    elif isinstance(items, (tuple, list)):
        s = '\n'.join(items)
    else:
        raise ValueError('Incorrect type in dlg_menu')
    return ct.dlg_menu(id, s, focused, caption, clip, w, h)

def dlg_file(is_open, init_filename, init_dir, filters, caption=''):
    if not caption:
        caption = 'Open file' if is_open else 'Save file'
    return ct.dlg_file(is_open, init_filename, init_dir, filters, caption)

def dlg_dir(init_dir, caption='Select folder'):
    return ct.dlg_dir(init_dir, caption)

def dlg_hotkey(title=''):
    return ct.dlg_hotkey(title)

def dlg_hotkeys(command, lexer=''):
    return ct.dlg_hotkeys(command, lexer)

def dlg_commands(options, title='', w=0, h=0):
    return ct.dlg_commands(options, title, w, h)

def _dlg_custom_dict(res, count):
    """Parse dlg_custom str result to dict"""
    clicked, vals = res
    vals = vals.splitlines()
    res = {}
    #res[i]
    for i in range(count):
        res[i] = vals[i]
    #res['clicked']
    res['clicked'] = clicked
    #res['focused']
    for i in range(count, len(vals)):
        s = vals[i].split('=', 1)
        s_key = s[0]
        s_val = s[1]
        if s_val.isdigit():
            s_val = int(s_val)
        res[s_key] = s_val
    return res

def dlg_custom(title, size_x, size_y, text, focused=-1, get_dict=False):
    res = ct.dlg_custom(title, size_x, size_y, text, focused)
    if res is None:
        return
    if not get_dict:
        return res
    else:
        return _dlg_custom_dict(res, count=len(text.splitlines()) )

def file_open(name, group=-1, options=''):
    if isinstance(name, (list, tuple)):
        return ct.file_open(name[0], name[1], group, options)
    else:
        return ct.file_open(name, '', group, options)

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

def ini_proc(id, filename, section='', key=''):
    return ct.ini_proc(id, filename, section, key)

def lexer_proc(id, value):
    return ct.lexer_proc(id, to_str(value))

def imagelist_proc(id_list, id_action, value=''):
    return ct.imagelist_proc(id_list, id_action, to_str(value))

def image_proc(id_image, id_action, value=''):
    return ct.image_proc(id_image, id_action, to_str(value))

def tree_proc(id_tree, id_action, id_item=0, index=0, text='', image_index=-1, data=''):
    return ct.tree_proc(id_tree, id_action, id_item, index, to_str(text), image_index, data)

def _menu_proc_callback_proxy(info=''):
    if info in _live:
        return _live[info]()

def menu_proc(id_menu, id_action, command="", caption="", index=-1, hotkey="", tag=""):
    if callable(command):
        sid_callback = str(command)
        _live[sid_callback] = command
        command = 'module={};func=_menu_proc_callback_proxy;info="{}";'.format(__name__, sid_callback)
    return ct.menu_proc(str(id_menu), id_action, to_str(command), caption, index, hotkey, tag)

def button_proc(id_button, id_action, value=''):
    if callable(value):
        sid_callback = str(value)
        _live[sid_callback] = value
        value = 'module={};func=_menu_proc_callback_proxy;info="{}";'.format(__name__, sid_callback)
    return ct.button_proc(id_button, id_action, to_str(value))

def listbox_proc(id_listbox, id_action, index=0, text="", tag=0):
    return ct.listbox_proc(id_listbox, id_action, index, to_str(text), to_str(tag))

def toolbar_proc(id_toolbar, id_action, text="", text2="", command=0, index=-1, index2=-1):
    if callable(command):
        sid_callback = str(command)
        _live[sid_callback] = command
        command = 'module={};func=_menu_proc_callback_proxy;info="{}";'.format(__name__, sid_callback)
    return ct.toolbar_proc(str(id_toolbar), id_action, text, text2, str(command), index, index2)

def statusbar_proc(id_statusbar, id_action, index=-1, tag=0, value=""):
    return ct.statusbar_proc(str(id_statusbar), id_action, index, tag, to_str(value))

def canvas_proc(id_canvas, id_action, text='', color=-1, size=-1, x=-1, y=-1, x2=-1, y2=-1, style=-1, p1=-1, p2=-1):
    return ct.canvas_proc(id_canvas, id_action, text, color, size, x, y, x2, y2, style, p1, p2)

def _timer_proc_callback_proxy(tag='', info=''):
    if info in _live:
        return _live[info](tag)

def timer_proc(id, callback, interval, tag=''):
    if callable(callback):
        sid_callback = str(callback)
        _live[sid_callback] = callback
        callback = 'module={};func=_timer_proc_callback_proxy;info="{}";'.format(__name__, sid_callback)
    return ct.timer_proc(id, callback, interval, tag)


def to_str(v, escape=False):
    def _pair(a, b):
        return to_str(a, True) + ':' + to_str(b, True)

    if v is None:
        return ''

    if isinstance(v, str):
        s = v
        if escape:
            s = s.replace(',', chr(2))
        return s

    if isinstance(v, bool):
        return ('1' if v else '0')

    if isinstance(v, (list, tuple)):
        l = [to_str(i, escape) for i in v]
        return ','.join(l)

    def _o(k):
        #props must go first: *min* *max* p
        #props must go last: val
        if k in ('p', 'w_min', 'w_max', 'h_min', 'h_max'):
            return 0
        if k in ('val', 'columns'):
            return 2
        return 1

    if isinstance(v, dict):
        res = chr(1).join(
            [_pair(k, vv) for k,vv in v.items() if _o(k)==0 ] +
            [_pair(k, vv) for k,vv in v.items() if _o(k)==1 ] +
            [_pair(k, vv) for k,vv in v.items() if _o(k)==2 ]
            )
        return '{'+res+'}'

    return str(v)


def _dlg_proc_wait(id_dialog):
    while True:
        app_idle()
        from time import sleep
        sleep(0.03) # 30 msec seems ok for CPU load

        d = ct.dlg_proc(id_dialog, DLG_PROP_GET, '', -1, -1, '')
        if isinstance(d, dict) and not d['vis']:
            return


def _dlg_proc_callback_proxy(id_dlg, id_ctl, data='', info=''):
    #print('dlg_proc_proxy: data:', data, ' info:', info)
    if info in _live:
        return _live[info](id_dlg, id_ctl, data=data)

def _alter_live(id_dialog, prop, callback_name):
    callback_param = prop[callback_name]
    if callable(callback_param):
        sid_callback = '{}:{}'.format(id_dialog, callback_param)
        _live[sid_callback] = callback_param
        prop[callback_name] = 'module={};func=_dlg_proc_callback_proxy;info="{}";'.format(__name__, sid_callback)

def dlg_proc(id_dialog, id_action, prop='', index=-1, index2=-1, name=''):
    #print('#dlg_proc id_action='+str(id_action)+' prop='+repr(prop))

    #cleanup storage of live callbacks
    if id_action == DLG_FREE:
        for k in [k for k in _live if k.startswith(str(id_dialog)+':')]:
            _live.pop(k)

    #support live callbacks by replacing them to str
    if isinstance(prop, dict):
        if 'resize' in prop and 'border' in prop:
            del prop['resize']

        for k in prop:
            if k.startswith('on_'):
                _alter_live(id_dialog, prop, k)

    res = ct.dlg_proc(id_dialog, id_action, to_str(prop), index, index2, name)
    if id_action == DLG_SHOW_MODAL:
        _dlg_proc_wait(id_dialog)
    return res

def finder_proc(id_finder, id_action, value="", setcaret=True):
    return ct.finder_proc(id_finder, id_action, to_str(value), setcaret)

def esc_z(s):
    # temp solution for null chars, later replace it to full solution with app patch
    if chr(0) in s:
        s = s.replace(chr(0), ' ')
    '''
    # by kvichans:
    s = s.replace('\\', r'\\') if '\\' in s else s
    s = s.replace(chr(0), r'\0') if chr(0) in s else s
    '''
    return s

#Editor
class Editor:
    h = 0
    def __init__(self, handle):
        self.h = handle

    def get_carets(self):
        return ct.ed_get_carets(self.h)

    def set_caret(self, x1, y1, x2=-1, y2=-1, id=CARET_SET_ONE, options=0):
        return ct.ed_set_caret(self.h, x1, y1, x2, y2, id, options)

    def get_line_count(self):
        return ct.ed_get_line_count(self.h)

    def get_text_all(self):
        return ct.ed_get_text_all(self.h)

    def set_text_all(self, text):
        return ct.ed_set_text_all(self.h, esc_z(text))

    def get_text_sel(self):
        return ct.ed_get_text_sel(self.h)

    def get_text_line(self, index, max_len=0):
        return ct.ed_get_text_line(self.h, index, max_len)

    def set_text_line(self, index, text):
        return ct.ed_set_text_line(self.h, index, esc_z(text))

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
        return ct.ed_insert(self.h, x1, y1, esc_z(text))

    def replace(self, x1, y1, x2, y2, text):
        return ct.ed_replace(self.h, x1, y1, x2, y2, esc_z(text))

    def replace_lines(self, y1, y2, lines):
        return ct.ed_replace_lines(self.h, y1, y2, [esc_z(l) for l in lines])

    def get_filename(self, options=''):
        return ct.ed_get_filename(self.h, options)

    def save(self, filename=''):
        return ct.ed_save(self.h, filename)

    def cmd(self, code, text=''):
        return ct.ed_cmd(self.h, code, esc_z(text))

    def focus(self):
        return ct.ed_focus(self.h)

    def bookmark(self, id, nline, nkind=1, ncolor=-1, text='', auto_del=True, show=True, tag=0):
        return ct.ed_bookmark(self.h, id, nline, nkind, ncolor, text, auto_del, show, tag)

    def decor(self, id, line=-1, tag=0, text='', color=0, bold=False, italic=False, image=-1, auto_del=True):
        return ct.ed_decor(self.h, id, line, tag, text, color, bold, italic, image, auto_del)

    def lock(self):
        return ct.ed_lock(self.h)

    def unlock(self):
        return ct.ed_unlock(self.h)

    def get_prop(self, id, value=''):
        value = to_str(value)
        if id!=PROP_TAG:
            return ct.ed_get_prop(self.h, id, value)
        js_s = ct.ed_get_prop(self.h, PROP_TAG, '')
        key,dfv = value.split(':', 1) if ':' in value else ('_', value)
        if not js_s:
            return dfv
        import json
        js = json.loads(js_s)
        return js.get(key, dfv)

    def set_prop(self, id, value):
        value = to_str(value)
        if id!=PROP_TAG:
            return ct.ed_set_prop(self.h, id, value)
        key,val = value.split(':', 1) if ':' in value else ('_', value)
        js_s = ct.ed_get_prop(self.h, PROP_TAG, '')
        js_s = js_s if js_s else '{}'
        import json
        js = json.loads(js_s)
        js[key] = val
        js_s = json.dumps(js)
        return ct.ed_set_prop(self.h, PROP_TAG, js_s)

    def complete(self, text, len1, len2, selected=0, alt_order=False):
        return ct.ed_complete(self.h, text, len1, len2, selected, alt_order)

    def complete_alt(self, text, snippet_id, len_chars, selected=0):
        return ct.ed_complete_alt(self.h, text, snippet_id, len_chars, selected)

    def convert(self, id, x, y, text=''):
        return ct.ed_convert(self.h, id, x, y, esc_z(text))

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

    def markers(self, id, x=0, y=0, tag=0, len_x=0, len_y=0, line_len=0):
        return ct.ed_markers(self.h, id, x, y, tag, len_x, len_y, line_len)

    def attr(self, id, tag=0, x=0, y=0, len=1,
             color_font=COLOR_NONE, color_bg=COLOR_NONE, color_border=COLOR_NONE,
             font_bold=0, font_italic=0, font_strikeout=0,
             border_left=0, border_right=0, border_down=0, border_up=0,
             show_on_map=False, map_only=0
             ):

        if id==MARKERS_ADD_MANY:
            if not isinstance(x, int):
                x = ','.join(map(str, x))
                y = ','.join(map(str, y))
                len = ','.join(map(str, len))
        else:
            x = str(x)
            y = str(y)
            len = str(len)

        column = 1 if show_on_map is True else -1 if show_on_map==False else show_on_map

        return ct.ed_attr(self.h, id, tag,
                          x, y, len,
                          color_font, color_bg, color_border,
                          font_bold, font_italic, font_strikeout,
                          border_left, border_right, border_down, border_up,
                          column, map_only
                          )

    def dim(self, id, index=0, index2=0, value=100):
        return ct.ed_dim(self.h, id, index, index2, value)

    def hotspots(self, id, tag=0, tag_str="", pos=""):
        return ct.ed_hotspots(self.h, id, tag, tag_str, to_str(pos))

    def get_token(self, id, index1=0, index2=0):
        return ct.ed_get_token(self.h, id, index1, index2)

    def gap(self, id, num1, num2, tag=-1, size=0, color=COLOR_NONE):
        return ct.ed_gap(self.h, id, num1, num2, tag, size, color)

    def folding(self, id, index=-1, item_x=-1, item_y=-1, item_y2=-1, item_staple=False, item_hint=''):
        return ct.ed_folding(self.h, id, index, item_x, item_y, item_y2, item_staple, item_hint)

    def get_wrapinfo(self):
        return ct.ed_get_wrapinfo(self.h)

    def action(self, id, param1='', param2='', param3=''):
        return ct.ed_action(self.h, id, to_str(param1), to_str(param2), to_str(param3))

    def micromap(self, id, param1=0, param2=0, param3=0):
        return ct.ed_micromap(self.h, id, param1, param2, param3)

    def __str__(self):
        return '<Editor id:{} title:"{}" gr:{} tab:{}>'.format(
            self.get_prop(PROP_TAB_ID),
            self.get_prop(PROP_TAB_TITLE),
            self.get_prop(PROP_INDEX_GROUP),
            self.get_prop(PROP_INDEX_TAB)
            )

    def __repr__(self):
        return self.__str__()

    def __eq__(self, other):
        return other and type(other) is Editor and self.get_prop(PROP_HANDLE_SELF) == other.get_prop(PROP_HANDLE_SELF)
    #end

#objects
ed = Editor(0)

#support input() in plugins
def _input(s):
    return dlg_input(s, '') or ''

import builtins
builtins.input = _input
