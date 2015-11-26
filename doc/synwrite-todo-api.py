ATTRIB_CLEAR_ALL       = -1
ATTRIB_CLEAR_SELECTION = -2
ATTRIB_COLOR_FONT      = 0
ATTRIB_COLOR_BG        = 1
ATTRIB_SET_BOLD        = 2
ATTRIB_SET_ITALIC      = 3
ATTRIB_SET_UNDERLINE   = 4
ATTRIB_SET_STRIKEOUT   = 5

PROC_LOCK_STATUS      = 3
PROC_UNLOCK_STATUS    = 4
PROC_ADD_RECENT_COLOR = 7

PROP_TEXT_EXTENT    = 11
PROP_ZOOM           = 12
PROP_VIS_LINES      = 15
PROP_VIS_COLS       = 16
PROP_TOKEN_TYPE     = 21
PROP_COLOR          = 25
PROP_KEEP_TRAIL_BLANKS   = 32
PROP_KEEP_CARET_IN_TEXT  = 33
PROP_AUTO_INDENT         = 34

PROP_COORD_WINDOW  = 100
PROP_COORD_TREE    = 101
PROP_COORD_CLIP    = 102
PROP_COORD_OUT     = 103
PROP_COORD_PRE     = 104
PROP_DOCK_TREE     = 105
PROP_DOCK_CLIP     = 106
PROP_DOCK_OUT      = 107
PROP_DOCK_PRE      = 108
PROP_COORD_DESKTOP  = 120
PROP_COORD_MONITOR  = 121
PROP_COORD_MONITOR0 = 122
PROP_COORD_MONITOR1 = 123
PROP_COORD_MONITOR2 = 124
PROP_COORD_MONITOR3 = 125
PROP_SPLIT_MAIN_POS = 129
PROP_FILENAME_SESSION = 132
PROP_FILENAME_PROJECT = 133
PROP_RECENT_FILES    = 135
PROP_RECENT_SESSIONS = 136
PROP_RECENT_PROJECTS = 137
PROP_RECENT_NEWDOC   = 138
PROP_RECENT_COLORS   = 139

FIND_ACTION_FIND_NEXT    = 0
FIND_ACTION_FIND_ALL     = 1
FIND_ACTION_COUNT        = 3
FIND_ACTION_REPLACE_NEXT = 5
FIND_ACTION_REPLACE_ALL  = 6

FIND_OP_CASE     = 1 << 0
FIND_OP_WORDS    = 1 << 1
FIND_OP_BACK     = 1 << 2
FIND_OP_SELONLY  = 1 << 3
FIND_OP_ENTIRE   = 1 << 4
FIND_OP_REGEX    = 1 << 5
FIND_OP_REGEX_S  = 1 << 6
FIND_OP_PROMPT   = 1 << 8
FIND_OP_WRAP     = 1 << 9
FIND_OP_SKIPCOL  = 1 << 10
FIND_OP_BOOKMARK = 1 << 14
FIND_OP_EXTSEL   = 1 << 15

TOKENS_ALL        = 0
TOKENS_CMT        = 1
TOKENS_STR        = 2
TOKENS_CMT_STR    = 3
TOKENS_NO_CMT_STR = 4

def dlg_snippet(name, alias, lexers, text):
    return sw_api.dlg_snippet(name, alias, lexers, text)

def app_lock(id):
    return sw_api.app_lock(id)

class Editor:
    def set_attr(self, id, color):
        return sw_api.ed_set_attr(self.h, id, color)
    def get_attr(self):
        return sw_api.ed_get_attr(self.h)
    def find(self, action, opt, tokens, sfind, sreplace):
        return sw_api.ed_find(self.h, action, opt, tokens, sfind, sreplace)

#noneed
    def get_alerts(self):
    def set_alerts(self, value):
    def get_left(self):
    def set_left(self, num):
