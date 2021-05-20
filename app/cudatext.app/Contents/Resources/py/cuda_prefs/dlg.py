import os
from contextlib import contextmanager
from collections import namedtuple

from cudatext import *
import cudax_lib as apx
FILE_OPTS = apx.OPT2PROP # to check if option can be in file scope

import traceback

# dbg
import time

"""
#DONE
* search
* OK,Cancel,Apply
* sorts
* scope change - update current value
* value editing for different values
* update list on opt change
* fix val display: int2s, str2s
* list columns choice
* ctrl+f for filter + hide/show
* column widths
* remember UI
* filter history
* user keys for numbers are sorted before others
* fisx sorting
* format other scope values
* edit and combo - fonts size
* focus to filter on filter
* <Escape> in filter to unfilter and hide filter
* better way to get option value+scoped
* options subsets
* opted 3d-party api
* test subset options
* test loading opted's state
* test with translation
* add help
* some options not applied (font)
* os scale
* change to modal dialog
* readme

#TODO:

?
* different color fo unapplied propery value in editor?
* list changes in dlg-statusbar + hover popup
* editor line state coloring for modified/unapplied("saved") options?
* show user/lexer/file option-values for selected option?
* indicate qued option change in list?
* different filter background?


file:///mnt/H/cuda/__FM/data/themes/cobalt.cuda-theme-ui
"""

_   = apx.get_translation(__file__)  # I18N


TITLE_DEFAULT = _('CudaText Preferences')

OptChange = namedtuple('OptChange', 'name scope value lexer old_value')

fn_icons = {
    'asc': 'asc.png', # ascending order
    'desc': 'desc.png', # descending
}
PLING_HISTORY_JSON  = os.path.join(app_path(APP_DIR_SETTINGS), 'plugin history.json')
FORMS_CFG_JSON = os.path.join(app_path(APP_DIR_SETTINGS), 'forms data.json')
PLING_KEY = 'dlg_preferences'
STATE_KEY_TREE_W = 'tree_w'
STATE_KEY_DESCR_MEMO_H = 'descr_memo_h'
STATE_KEY_FILTER_STR = 'filter_str'
STATE_KEY_FILTER_HIST = 'filter_history'
STATE_KEY_FILTER_VISIBLE = 'filter_visible' #TODO remove
STATE_KEY_COL_CFG = 'columns'
STATE_KEY_SORT_COL = 'sort_column'
STATE_KEY_SEL_OPT = 'selected_option'

SUBSET_KEYS = [
    STATE_KEY_FILTER_STR,
    STATE_KEY_FILTER_HIST,
    STATE_KEY_SEL_OPT,
]


IS_DBG = False
LOG = False


VK_ENTER = 13
VK_F = ord('F')
VK_ESCAPE = 27
LIST_SEP = chr(1)
IS_WIN = os.name=='nt'

BTN_H = app_proc(PROC_GET_GUI_HEIGHT, 'button')
BTN_W = BTN_H*3
PAD = 2

# colores
COL_FONT = 0
COL_SPLITTER = 0

GLOBAL_OP_CMT = 'Note: this option is global'
TREE_ITEM_ALL = _('[ All ]')

# columns
COL_SECTION     = 'Section'
COL_OPT_NAME    = 'Option'
COL_MODIFIED    = '!'
COL_VAL_DEFAULT = 'Default'
COL_VAL_USER    = 'User'
COL_VAL_LEX     = 'Lexer'
COL_VAL_FILE    = 'File'
COL_VAL_MAIN    = 'Value' # current value -- most specific f,l,u,def value

UI_COLUMNS = {
    COL_SECTION     : _('Section'),
    COL_OPT_NAME    : _('Option'),
    COL_VAL_DEFAULT : _('Default'),
    COL_VAL_USER    : _('User'),
    COL_VAL_LEX     : _('Lexer'),
    COL_VAL_FILE    : _('File'),
    COL_VAL_MAIN    : _('Current value'),
}

OPTS_COLUMN_MAP = {
    COL_SECTION     : 'chp',
    COL_OPT_NAME    : 'opt',
    COL_MODIFIED    : '!',
    COL_VAL_DEFAULT : 'def',
    COL_VAL_USER    : 'uval',
    COL_VAL_LEX     : 'lval',
    COL_VAL_FILE    : 'fval',
    # + Value - most specific scope value
}

# order in UI
COLS_LIST = [
    COL_SECTION,
    COL_OPT_NAME,
    COL_MODIFIED,
    COL_VAL_DEFAULT,
    COL_VAL_MAIN,
    COL_VAL_USER,
    COL_VAL_LEX,
    COL_VAL_FILE,
]


opt_col_cfg = [("Option", 70), ("Value", 100)]

ui_max_history_edits = 20
font_name = None
font_size = None

filter_history = []

def load_imagelist(ic_filename_map):
    ind_map = {}
    h_iml = imagelist_proc(0, IMAGELIST_CREATE)
    _icons_dir = os.path.join(os.path.dirname(os.path.realpath(__file__)), 'icons')
    for name,fn_icon in ic_filename_map.items():
        _path = os.path.join(_icons_dir, fn_icon)
        imind = imagelist_proc(h_iml, IMAGELIST_ADD, _path)
        ind_map[name] = imind
    return h_iml, ind_map

def get_tree_path_names(h_tree, item_id, l=None):
    ''' returns list, node names starting with deepest
    '''
    if l is None:
        l = []
    prop = tree_proc(h_tree, TREE_ITEM_GET_PROPS, id_item=item_id)
    l.append(prop['text'])

    parent_id = prop.get('parent')
    if parent_id:
        get_tree_path_names(h_tree, parent_id, l)
    return l

def get_tree_path(h_tree, item_id):
    """ tree path for tree item
    """
    path_names = get_tree_path_names(h_tree, item_id)
    path_names.reverse()
    return '/'.join(path_names)

@contextmanager
def ignore_edit(h, ed_):
    """ turns off PROP_RO + deactivates Editor -- then restores
        ? to not send `on_change` when changing `editor_combo` text
    """
    is_ro = ed_.get_prop(PROP_RO)
    if is_ro:
        ed_.set_prop(PROP_RO, False)

    h_ed = ed_.get_prop(PROP_HANDLE_SELF)
    #NOTE: widgets are never deleted here, so `DLG_CTL_COUNT` should not be a problem
    for n in range(dlg_proc(h, DLG_CTL_COUNT)):
        h_ctl = dlg_proc(h, DLG_CTL_HANDLE, index=n)
        if h_ed == h_ctl:
            # disable temporarily
            dlg_proc(h, DLG_CTL_PROP_SET, index=n, prop={'act': False})
            break

    try:
        yield
    finally:
        if is_ro:
            ed_.set_prop(PROP_RO, True)
        dlg_proc(h, DLG_CTL_PROP_SET, index=n, prop={'act': True})


def map_option_value(opt, val=None, caption=None):
    """ for map options - returns caption for provided 'val'ue, or value for provided 'caption'
        "val" -- 0;  "caption" -- (0) don't activate"
    """
    frm = opt['frm']
    if frm in ['int2s', 'str2s']:
        jdc = opt['jdc'] # list('(a) by Alt+click', ...)
        dct = opt['dct'] # list(['a', 'by Alt+click'], ...)

        if val is not None:
            for i,item in enumerate(dct):
                if item[0] == val:
                    return jdc[i]     # return str

        elif caption is not None:
            ind = jdc.index(caption)
            val, _cap = dct[ind]
            return val


        else:
            raise OptionMapValueError('require "val" or "caption"')

    elif frm in ['font', 'strs', 'font-e']:
        if val      is not None: return val
        if caption  is not None: return caption

    else:
        raise OptionMapValueError('Unsupported option format: {}'.format((opt["opt"], opt["frm"])))
    raise OptionMapValueError('Couldn"t find: {}, {}\n + {}'.format(val, caption, opt))


def json_update(path, key, val):
    """ loads json at 'path' if exists, puts 'k':'v' into it and saves
    """
    import json

    if os.path.exists(path):
        with open(path, 'r', encoding='utf-8') as f:
            j = json.load(f)
    else:
        j = {}

    j[key] = val
    j_str = json.dumps(j, indent=2)

    with open(path, 'w', encoding='utf-8') as f:
        f.write(j_str)

def format_opt_change(ch):
    scope_str = ch.scope
    if   ch.scope=='u': scope_str = ui_column(COL_VAL_USER)
    elif ch.scope=='l': scope_str = ui_column(COL_VAL_LEX) + str(ch.lexer)
    elif ch.scope=='f': scope_str = ui_column(COL_VAL_FILE)+': '+os.path.basename(ed.get_filename())

    if ch.value is None:    val_str = _('reset')
    else:                   val_str = '{} -> {}'.format(ch.old_value, ch.value)

    return '{} [{}] {}'.format(ch.name, scope_str, val_str)

def ui_column(colname):
    return UI_COLUMNS.get(colname, colname)


class DialogMK2:

    _h_list_iml = None # handle for option-list's imagelist
    _lb_icon_inds = {} # listbox icons

    def __init__(self, optman, title=None, subset=None, how=None):
        """ optman -- cd_opts_dlg.py/OptionsMan
            how --
            * how.get('hide_fil', False)
            * how.get('hide_lex_fil', False)
            * how.get('only_for_ul', not ens['tofi'])         # Forbid to switch fo File ops

            - how.get('stor_json', 'user.json')
            - how.get('only_with_def', False) # Forbid to switch fo User+Lexer ops
        """
        global ui_max_history_edits
        global font_name
        global font_size

        #TODO get value from options if not present
        ui_max_history_edits = optman.get_scope_value('ui_max_history_edits', scope='u',
                                                        default=ui_max_history_edits)
        _os_suffix = app_proc(PROC_GET_OS_SUFFIX, '')
        font_name = apx.get_opt('font_name'+_os_suffix)
        font_size = apx.get_opt('font_size'+_os_suffix)

        self._form_rect = {} # dict - x,y,w,h
        self._state = {}

        self.title = title or TITLE_DEFAULT
        self.optman = optman
        self.subset = subset # None - ok
        # gather not-available scopes
        self.hidden_scopes = [] # 'l' and/or 'f'
        if (how  and  how.get('hide_fil')  or  how.get('hide_lex_fil')  or  how.get('only_for_ul')) \
                                                                            or not ed.get_filename():
            self.hidden_scopes.append('f')
        if how.get('hide_lex_fil')  or  not ed.get_prop(PROP_LEXER_FILE):
            self.hidden_scopes.append('l')


        self._load_dlg_cfg()

        _sort_val = self._state.get(STATE_KEY_SORT_COL, COL_OPT_NAME)
        self.current_sort = _sort_val.lstrip('-')
        self.sort_reverse = _sort_val.startswith('-')
        self._last_applied_filter = None
        self._cur_opt_name = None
        self._closing = False

        self.h = None
        self._h_tree = None
        self._h_col_menu = None
        self._h_help = None
        self._cur_value_ed = 'str' # type for: _set_value_editor()

        self.val_eds = ValueEds(self.on_opt_val_edit)

        self._opt_changes = []
        self._list_opt_names = [] # current displayed list of option-names
        # '' -> User -- showing default value, edited value will be added to 'user' scope
        self._col_toggle_cmds = {} # column name -> toggle lambda -- for menu, to toggle list columns
        self._scope_captions = { # expanded alter
                'u'  : ui_column(COL_VAL_USER),
                ''   : ui_column(COL_VAL_USER),
                'def': ui_column(COL_VAL_USER),
                }


    @property
    def filter_val(self):
        if self.h:
            return self._filter_ed.get_text_all()
        return ''
    @filter_val.setter
    def filter_val(self, value): #SKIP
        if self.h:
            val_str = str(value)
            if val_str != self._filter_ed.get_text_all():
                self._filter_ed.set_text_all(val_str)

    @property
    def columns(self):
        """ returns (column_captions, column_widths)
        """
        captions = []
        widths = []
        _col_cfg = opt_col_cfg

        # hide lexer and file scopes  if disabled
        if self.hidden_columns:
            _col_cfg = [colcfg for colcfg in _col_cfg  if colcfg[0] not in self.hidden_columns]

        _total_w = sum(w for name,w in _col_cfg if isinstance(w, int))
        for caption,w in _col_cfg:
            captions.append(caption)

            # width: to negative percentages for listbox -- except '!' <- in px
            if isinstance(w, int):
                w = -round(w/_total_w*100)
            else:
                w = int(w[:-2]) # "100px" => 100
            widths.append(w)

        return captions,widths

    @property
    def hidden_columns(self):
        if not hasattr(self, '_hidden_columns'):
            self._hidden_columns = set()

            if self.hidden_scopes:
                if 'l' in self.hidden_scopes:   self._hidden_columns.add(COL_VAL_LEX)
                if 'f' in self.hidden_scopes:   self._hidden_columns.add(COL_VAL_FILE)

        return self._hidden_columns

    @property
    def scope(self):
        """ returns current scope char: u,l,f
        """
        scope_str = self.scope_ed.get_text_all()
        if scope_str == ui_column(COL_VAL_USER):             return 'u'
        elif scope_str.startswith(ui_column(COL_VAL_LEX)):   return 'l'
        elif scope_str.startswith(ui_column(COL_VAL_FILE)):  return 'f'


    def _load_dlg_cfg(self):
        import json

        if os.path.exists(FORMS_CFG_JSON):
            with open(FORMS_CFG_JSON, 'r', encoding='utf-8') as f:
                j_form = json.load(f).get(self.title)
            if j_form:
                self._form_rect = {k:v for k,v in j_form.items()
                                        if v  and  k in {'x', 'y', 'w', 'h'}}

        if os.path.exists(PLING_HISTORY_JSON):
            with open(PLING_HISTORY_JSON, 'r', encoding='utf-8') as f:
                j_all = json.load(f)

            j = j_all.get(PLING_KEY)
            if j:
                _state_keys = {
                    STATE_KEY_TREE_W,
                    STATE_KEY_DESCR_MEMO_H,
                    STATE_KEY_FILTER_STR,
                    STATE_KEY_FILTER_VISIBLE,
                    STATE_KEY_SEL_OPT,
                    STATE_KEY_SORT_COL,
                }
                self._state = {k:v for k,v in j.items()  if k in _state_keys}

                # if subset - overwrite general values with subset's
                _subsets = j.get('subsets')
                if self.subset  and  _subsets:
                    self._state.update(_subsets.get(self.subset, {}))


                # filter history
                _filt_hist = j.get(STATE_KEY_FILTER_HIST)
                if _filt_hist:
                    filter_history.clear()
                    filter_history.extend(_filt_hist)

                # list columns
                _col_cfg = j.get(STATE_KEY_COL_CFG)
                if _col_cfg:
                    import re

                    # check if only integers and str (~"100px")
                    for i in range(len(_col_cfg)):
                        item = _col_cfg[i]
                        colname,w = item
                        if not isinstance(w, int)  and  not (isinstance(w, str)
                                                                and re.match('^\d+px$', w)):
                            print(_('NOTE: {}: invalid column width format: {}')
                                        .format(self.title, item))
                            _col_cfg[i] = (colname,100)

                    opt_col_cfg.clear()
                    opt_col_cfg.extend(_col_cfg)
                pass;       LOG and print(' --- Loaded state: '+json.dumps(j, indent=4))

            # no history - load from opted plugin
            else:
                j_opted = j_all.get('cd_opts_dlg', {}).get('dlg')
                if j_opted:
                    opted_state = {
                        STATE_KEY_DESCR_MEMO_H: j_opted.get("df.cmnt_heght"),
                        STATE_KEY_SEL_OPT:      j_opted.get("df.cur_op"),
                    }
                    self._state = {k:v  for k,v in opted_state.items()  if v is not None}

                    filter_history.clear()
                    filter_history.extend(j_opted.get('df.h.cond', []))


    def _save_dlg_cfg(self):
        if self._closing is None:
            return

        # window position/dimensions
        form_prop = dlg_proc(self.h, DLG_PROP_GET)
        j_form = {'x':form_prop['x'], 'y':form_prop['y'], 'w':form_prop['w'], 'h':form_prop['h']}
        json_update(FORMS_CFG_JSON,  key=self.title,  val=j_form)

        # states
        j = {}
        j[STATE_KEY_TREE_W] = dlg_proc(self.h, DLG_CTL_PROP_GET, name='category_tree')['w']
        j[STATE_KEY_DESCR_MEMO_H] = dlg_proc(self.h, DLG_CTL_PROP_GET, name='panel_value')['h']
        j[STATE_KEY_FILTER_STR] = self.filter_val
        j[STATE_KEY_FILTER_HIST] = filter_history
        j[STATE_KEY_FILTER_VISIBLE] = dlg_proc(self.h, DLG_CTL_PROP_GET, name='panel_filter')['vis']
        j[STATE_KEY_SORT_COL] = self.current_sort if not self.sort_reverse else '-'+self.current_sort
        j[STATE_KEY_SEL_OPT] = self._cur_opt_name

        # save some options separately -- 3rd party options: move from `j` to `j/subsets/<subset>`
        if self.subset:
            j_subset = {k:j.pop(k) for k in SUBSET_KEYS}
            _subsets = j.setdefault('subsets', {})
            _subsets[self.subset] = j_subset

        j[STATE_KEY_COL_CFG] = opt_col_cfg

        json_update(PLING_HISTORY_JSON,  PLING_KEY,  j )

    def configure_columns(self):
        global opt_col_cfg

        caption = _('Columns widths. In pixels (50px) or relative (100)')

        _colnames, _widths = zip(*opt_col_cfg) # start values
        colnames, widths = list(_colnames), list(_widths) # working values
        while True:
            flat_columns = [str(a)   for item in zip(colnames,widths)   for a in item]
            res = dlg_input_ex(len(colnames), caption, *flat_columns)
            if not res:
                break
            else: # have result -> validate
                for i in range(len(colnames)):
                    item = res[i]
                    if item.isdecimal():
                        res[i] = int(item)
                    elif (item.endswith('px') and item[:-2].isdecimal()):
                        pass
                    else: # error
                        widths = res
                        colnames[i] = _colnames[i] + _(' (Error!)')
                        break

                else: # all is well - stop `While`
                    break

        if res:
            new_cfg = list(zip(_colnames, res))
            _start_cfg = opt_col_cfg[:]

            # try to apply new config -- revert if failed (jic)
            try:
                opt_col_cfg = new_cfg  # global

                self.update_list_layout()
                _opts = self.get_filtered_opts()
                self.update_list(_opts)
            except Exception as ex:
                opt_col_cfg = _start_cfg  # revert changes

                msg = _('failed to apply new columns config: {}. {}').format(new_cfg, ex)
                print('NOTE: {}: {}'.format(self.title, msg))



    def show(self):
        if not self.h:
            self.h, self.opt_comment_ed = self.init_form()

        self._fill_tree(self.optman.tree['kids'])

        self.update_list_layout()

        # restore filter
        _filter_val = self._state.get(STATE_KEY_FILTER_STR, '')
        self.set_filter(_filter_val)
        if _filter_val:
            self.toggle_filter(show=True)

        # restore selected-option (+show it)
        last_sel_opt = self._state.get(STATE_KEY_SEL_OPT)
        if self._list_opt_names:
            if last_sel_opt  and  last_sel_opt in self._list_opt_names:
                _ind = self._list_opt_names.index(last_sel_opt)
            else:   # if no saved selected opt - select first
                _ind = 0
            listbox_proc(self._h_list, LISTBOX_SET_SEL, index=_ind)
            _top = max(0, _ind-3)
            listbox_proc(self._h_list, LISTBOX_SET_TOP, index=_top)
            #### click event
            self._on_opt_click(id_dlg=self.h, id_ctl=-1)

        # focus filter on start
        timer_proc(TIMER_START_ONE,  callback=lambda *args,**vargs: self._filter_ed.focus(),   interval=100)

        # DBG #############
        if IS_DBG:
            DialogMK2._dlg = self
            cmds = [    'from cuda_prefs.dlg import DialogMK2',
                        'globals()["dlg"] = DialogMK2._dlg',]
            app_proc(PROC_EXEC_PYTHON, '\n'.join(cmds))
            del DialogMK2._dlg

            dlg_proc(self.h, DLG_SHOW_NONMODAL)
            return
        ###########

        dlg_proc(self.h, DLG_SHOW_MODAL)
        dlg_proc(self.h, DLG_FREE)

        del self.optman
        del self._list_opt_names
        del self._col_toggle_cmds

    def init_form(self):
        global COL_FONT
        global COL_SPLITTER

        # load icons only once
        def get_list_imagelist(): #SKIP
            if not DialogMK2._h_list_iml:
                DialogMK2._h_list_iml,  DialogMK2._lb_icon_inds = load_imagelist(fn_icons)

            return DialogMK2._h_list_iml

        h = dlg_proc(0, DLG_CREATE)

        colors = app_proc(PROC_THEME_UI_DICT_GET, '')
        COL_FONT = colors['EdTextFont']['color']
        COL_SPLITTER = colors['SplitMain']['color']
        color_form_bg = colors['TabBg']['color']

        ###### FORM #######################
        dlg_proc(h, DLG_PROP_SET, prop={
                'cap': self.title,
                'w': 600, 'h': 400,
                'w_min': 550, 'h_min': 250,
                'border': DBORDER_SIZE,
                'color': color_form_bg,
                #'on_mouse_exit': self.dlgcolor_mouse_exit,
                'keypreview': True,
                'on_key_down': self._on_key,
                'on_close': lambda *args, **vargs: self._save_dlg_cfg(),
                'topmost': True,
                })

        ###### MAIN PANEL
        n = dlg_proc(h, DLG_CTL_ADD, 'panel')
        dlg_proc(h, DLG_CTL_PROP_SET, index=n,   prop={
                'name': 'panel_main',
                'align': ALIGN_CLIENT,
                'sp_l': PAD*2, 'sp_t': PAD*2, 'sp_r': PAD*2, 'sp_b': PAD*2 + BTN_H + PAD*2,
                })


        ### tree ##########################
        n = dlg_proc(h, DLG_CTL_ADD, 'treeview')
        dlg_proc(h, DLG_CTL_PROP_SET, index=n, prop={
                'name': 'category_tree',
                'p': 'panel_main',
                'align': ALIGN_LEFT,
                'w': 100,
                #'sp_r': PAD,
                'on_change': self._on_tree_click,
                })
        self._h_tree = dlg_proc(h, DLG_CTL_HANDLE, index=n)
        tree_proc(self._h_tree, TREE_THEME)


        ### RIGHT PANEL #########################
        n = dlg_proc(h, DLG_CTL_ADD, 'panel')
        dlg_proc(h, DLG_CTL_PROP_SET, index=n,   prop={
                'name': 'panel_right',
                'p': 'panel_main',
                'align': ALIGN_CLIENT,
                #'sp_l': PAD,
                })
        # listbox ##########
        n = dlg_proc(h, DLG_CTL_ADD, 'listbox_ex')
        dlg_proc(h, DLG_CTL_PROP_SET, index=n, prop={
                'p': 'panel_right',
                'align': ALIGN_CLIENT,
                'sp_t': PAD,
                'border': True,
                'on_click': self._on_opt_click,
                'on_click_header': self._on_header_click,
                'on_menu': self.listbox_menu,
                })
        self._h_list = dlg_proc(h, DLG_CTL_HANDLE, index=n)

        ### FILTER panel ############################
        n = dlg_proc(h, DLG_CTL_ADD, 'panel')
        dlg_proc(h, DLG_CTL_PROP_SET, index=n, prop={
                'name': 'panel_filter',
                'p': 'panel_right',
                'align': ALIGN_TOP,  'h': BTN_H, 'max_h': BTN_H,
                #'vis': self._state.get(STATE_KEY_FILTER_VISIBLE, False),
                })
        # filter label
        n = dlg_proc(h, DLG_CTL_ADD, 'label')
        dlg_proc(h, DLG_CTL_PROP_SET, index=n, prop={
                'name': 'filter_label',
                'p': 'panel_filter',
                'a_l': ('', '['), 'a_t': ('', '-'),
                'sp_l': PAD*2,
                'cap': _('Filter: '),
                'font_color': COL_FONT,
                })
        # filter combo ##########
        n = dlg_proc(h, DLG_CTL_ADD, 'editor_combo')
        dlg_proc(h, DLG_CTL_PROP_SET, index=n, prop={
                'name': 'filter',
                'p': 'panel_filter',
                'h': BTN_H, 'max_h': BTN_H,
                #'align': ALIGN_CLIENT,
                'sp_r': 20,
                'a_l': ('filter_label', ']'),   'a_r': ('', ']'),  'a_t': ('filter_label', '-'),
                'on_change': self._on_filter,
                'on_key_down': self._on_filter, # for later -- live filter
                })
        h_ed = dlg_proc(h, DLG_CTL_HANDLE, index=n)
        self._filter_ed = Editor(h_ed)


        ### BOTTOM PANEL ###############################
        n = dlg_proc(h, DLG_CTL_ADD, 'panel')
        dlg_proc(h, DLG_CTL_PROP_SET, index=n,   prop={
                'name': 'panel_value',
                'p': 'panel_right',
                'align': ALIGN_BOTTOM,
                'h': 120,
                'sp_t': PAD,
                })
        # scope combo ##########
        n = dlg_proc(h, DLG_CTL_ADD, 'editor_combo')
        dlg_proc(h, DLG_CTL_PROP_SET, index=n, prop={
                'name': 'scope',
                'p': 'panel_value',
                'h': BTN_H, 'max_h': BTN_H,       'w': 100, 'max_w': 100,
                'a_l': None,   'a_r': ('', ']'),  'a_t': ('', '['),
                'act': True,
                'on_change': self._on_scope_change,
                })
        h_scope_ed = dlg_proc(h, DLG_CTL_HANDLE, index=n)
        # scope label ###
        n = dlg_proc(h, DLG_CTL_ADD, 'label')
        dlg_proc(h, DLG_CTL_PROP_SET, index=n, prop={
                'name': 'scope_label',
                'p': 'panel_value',
                'h': BTN_H, 'max_h': BTN_H,
                'a_l': None,   'a_r': ('scope', '['),  'a_t': ('scope', '-'),
                'sp_t': 3,
                'cap': _('Scope: '),
                'font_color': COL_FONT,
                })
        # btn reset ###########
        n = dlg_proc(h, DLG_CTL_ADD, 'button_ex')
        dlg_proc(h, DLG_CTL_PROP_SET, index=n, prop={
                'name': ValueEds.VALUE_ED_RESET,
                'p': 'panel_value',
                'h': BTN_H, 'max_h': BTN_H,
                'w': BTN_W, 'max_w': BTN_W,
                'a_l': None,   'a_r': ('scope_label', '['),  'a_t': ('', '['),
                'sp_l': PAD, 'sp_r': 32,
                'cap': _('Reset'),
                'on_change': self._on_reset,
                })
        # option description #########
        n = dlg_proc(h, DLG_CTL_ADD, 'editor')
        dlg_proc(h, DLG_CTL_PROP_SET, index=n, prop={
                'name': 'descr_memo',
                'p': 'panel_value',
                'sp_t': BTN_H + PAD,
                'align': ALIGN_CLIENT,
                'h': 100,
                })
        h_ed = dlg_proc(h, DLG_CTL_HANDLE, index=n)
        edt = Editor(h_ed)

        n = dlg_proc(h, DLG_CTL_ADD, 'label')
        dlg_proc(h, DLG_CTL_PROP_SET, index=n, prop={
                'name': 'mod_label',
                'p': 'panel_value',
                'a_t': ('scope', '-'),
                'sp_r': PAD,
                'cap': _('[mod]'),
                'font_color': COL_FONT,
                })


        ### SPLITTERS ###
        # list--opt_description
        n = dlg_proc(h, DLG_CTL_ADD, 'splitter')
        dlg_proc(h, DLG_CTL_PROP_SET, index=n, prop={
                'p': 'panel_right',
                'align': ALIGN_BOTTOM,
                'x': 0, 'y': 50, 'h': 4,
                'color': COL_SPLITTER,
                })
        # tree--list
        n = dlg_proc(h, DLG_CTL_ADD, 'splitter')
        dlg_proc(h, DLG_CTL_PROP_SET, index=n, prop={
                'p': 'panel_main',
                'align': ALIGN_LEFT,
                'x': 100, 'y': 0, 'w': 4,
                'color': COL_SPLITTER,
                })


        ### Bottom Btns ###################
        # OK #######
        n = dlg_proc(h, DLG_CTL_ADD, 'button_ex')
        dlg_proc(h, DLG_CTL_PROP_SET, index=n, prop={
                'name': 'btn_ok',
                'h': BTN_H, 'max_h': BTN_H,
                'w': BTN_W, 'max_w': BTN_W,
                'a_l': None, 'a_t': None, 'a_r': ('', ']'),  'a_b': ('', ']'),
                'sp_r': PAD*2, 'sp_b': PAD*2,
                'cap': _('OK'),
                'on_change': lambda *args, **vargs: (self.apply_changes(closing=True), self.close()),
                })
        # Apply #######
        n = dlg_proc(h, DLG_CTL_ADD, 'button_ex')
        dlg_proc(h, DLG_CTL_PROP_SET, index=n, prop={
                'name': 'btn_apply',
                'h': BTN_H, 'max_h': BTN_H,
                'w': BTN_W, 'max_w': BTN_W,
                'a_l': None, 'a_t': None, 'a_r': ('btn_ok', '['),  'a_b': ('', ']'),
                'sp_r': PAD*2, 'sp_b': PAD*2,
                'cap': _('Apply'),
                'on_change': lambda *args, **vargs: self.apply_changes(),
                })
        # Cancel #######
        n = dlg_proc(h, DLG_CTL_ADD, 'button_ex')
        dlg_proc(h, DLG_CTL_PROP_SET, index=n, prop={
                'name': 'btn_cancel',
                'h': BTN_H, 'max_h': BTN_H,
                'w': BTN_W, 'max_w': BTN_W,
                'a_l': None, 'a_t': None, 'a_r': ('btn_apply', '['),  'a_b': ('', ']'),
                'sp_r': PAD*2, 'sp_b': PAD*2,
                'cap': _('Cancel'),
                'on_change': lambda *args, **vargs: self.close(),
                })
        # help #######
        n = dlg_proc(h, DLG_CTL_ADD, 'button_ex')
        dlg_proc(h, DLG_CTL_PROP_SET, index=n, prop={
                'h': BTN_H, 'max_h': BTN_H,
                'w': BTN_W, 'max_w': BTN_W,
                'a_l': ('', '['),  'a_t': None, 'a_r': None, 'a_b': ('', ']'),
                'sp_l': PAD*2, 'sp_b': PAD*2,
                'cap': _('Help'),
                'on_change': self.dlg_help,
                })

        # reverse buttons for Windows: [Cancel, Apply, OK] => [OK, Apply, Cancel]
        if IS_WIN:
            dlg_proc(h, DLG_CTL_PROP_SET, name='btn_cancel',    prop={'a_r': ('',           ']')})
            dlg_proc(h, DLG_CTL_PROP_SET, name='btn_apply',     prop={'a_r': ('btn_cancel', '[')})
            dlg_proc(h, DLG_CTL_PROP_SET, name='btn_ok',        prop={'a_r': ('btn_apply',  '[')})


        ### listbox
        listbox_proc(self._h_list, LISTBOX_SET_COLUMN_SEP, text=LIST_SEP)
        # + icons
        h_iml = get_list_imagelist()
        listbox_proc(self._h_list, LISTBOX_SET_HEADER_IMAGELIST, text=h_iml)


        edt.set_prop(PROP_GUTTER_ALL, False)
        edt.set_prop(PROP_MINIMAP, False)
        edt.set_prop(PROP_MICROMAP, False)
        edt.set_prop(PROP_LAST_LINE_ON_TOP, False)
        edt.set_prop(PROP_HILITE_CUR_LINE, False)
        edt.set_prop(PROP_WRAP, WRAP_ON_WINDOW)

        # scopes combo
        scopes = [ui_column(COL_VAL_USER)]
        lex = ed.get_prop(PROP_LEXER_FILE)
        if lex  and  'l' not in self.hidden_scopes:
            scopes.append(ui_column(COL_VAL_LEX)+': '+lex)
            self._scope_captions['l'] = scopes[-1]
        if ed.get_filename()  and  'f' not in self.hidden_scopes:
            filename = os.path.split(ed.get_filename())[1]
            scopes.append(ui_column(COL_VAL_FILE)+': '+filename)
            self._scope_captions['f'] = scopes[-1]
        self.scope_ed = Editor(h_scope_ed)
        self.scope_ed.set_prop(PROP_RO, True)
        self.scope_ed.set_prop(PROP_COMBO_ITEMS, '\n'.join(scopes))

        font = (font_name, font_size)  if font_name and font_size else  None
        if font:
            edt.set_prop(PROP_FONT, font)
            self.scope_ed.set_prop(PROP_FONT, font)
            self._filter_ed.set_prop(PROP_FONT, font)

        dlg_proc(h, DLG_SCALE)

        # unscale saved-state dimensions
        if self._form_rect:
            dlg_proc(h, DLG_PROP_SET, prop=self._form_rect)
        if self._state.get(STATE_KEY_TREE_W):
            dlg_proc(h, DLG_CTL_PROP_SET, name='category_tree', prop={
                    'w': self._state.get(STATE_KEY_TREE_W),
                    })
        if self._state.get(STATE_KEY_DESCR_MEMO_H):
            dlg_proc(h, DLG_CTL_PROP_SET, name='panel_value', prop={
                    'h': self._state.get(STATE_KEY_DESCR_MEMO_H),
                    })

        return h, edt

    def update_list(self, opts):
        column_names, _col_widths = self.columns
        columns_items = []
        _value_cols = {COL_VAL_DEFAULT,  COL_VAL_USER,  COL_VAL_LEX,  COL_VAL_FILE}
        for col_title in column_names:
            if col_title == COL_VAL_MAIN:
                col_values = [str(self.optman.get_opt_active_value(op, is_ui=True)) for op in opts]

            elif col_title in _value_cols:
                _col_key = OPTS_COLUMN_MAP.get(col_title)
                _scope = 'def'  if _col_key == 'def' else  _col_key[0] # 'uval' -> 'u',  'def' -> 'def'
                col_values = [str(self.optman.get_opt_scope_value(op, _scope, is_ui=True)) for op in opts]

            else:
                _col_key = OPTS_COLUMN_MAP.get(col_title)
                col_values = [str(op.get(_col_key, '')) for op in opts] # can't use generator - _col_key

            columns_items.append(col_values)

        self._list_opt_names = [op['opt'] for op in opts]

        listbox_proc(self._h_list, LISTBOX_DELETE_ALL)

        _addedn = 0
        _max_seps = 0
        for row in zip(*columns_items):
            row_txt = LIST_SEP.join(row)
            _max_seps = row_txt.count(LIST_SEP)

            listbox_proc(self._h_list, LISTBOX_ADD, index=-1, text=row_txt)
            _addedn += 1
        _rows = listbox_proc(self._h_list, LISTBOX_GET_COUNT)

        # select  current option
        if self._list_opt_names:
            ind = 0
            if self._cur_opt_name  and  self._cur_opt_name in self._list_opt_names:
                ind = self._list_opt_names.index(self._cur_opt_name)
            #listbox_proc(self._h_list, LISTBOX_SET_TOP, max(0, ind-3)) # selecting is not helpful
            listbox_proc(self._h_list, LISTBOX_SET_SEL, ind)

        self._on_opt_click(id_dlg=self.h, id_ctl=-1)


    def update_list_layout(self):
        # columns
        column_captions, column_widths = self.columns
        column_widths[-1] = 0   # last col to 'fill' - to avoid h-scrollbar
        _ui_columns = map(lambda cap: ui_column(cap), column_captions) # generator
        column_captions_str = LIST_SEP.join(_ui_columns)
        listbox_proc(self._h_list, LISTBOX_SET_COLUMNS, text=column_widths) # width<0 means value in %
        listbox_proc(self._h_list, LISTBOX_SET_HEADER, text=column_captions_str)

        # sort-icons
        header_icon_cfg = []
        if self.current_sort and self.current_sort in column_captions:
            sort_col_ind = column_captions.index(self.current_sort)
            _order_name = 'asc'  if not self.sort_reverse else  'desc'
            _icon_ind = DialogMK2._lb_icon_inds[_order_name]
            header_icon_cfg = [-1]*sort_col_ind + [_icon_ind] # ~[-1, -1, -1, ind]
        listbox_proc(self._h_list, LISTBOX_SET_HEADER_IMAGEINDEXES, text=header_icon_cfg)

    def get_filtered_opts(self):
        sort_field = OPTS_COLUMN_MAP.get(self.current_sort, self.current_sort)
        return self.optman.get_list(self.filter_val, sort_field, reverse=self.sort_reverse)

    def set_filter(self, filter_str, tree_click=False):
        if not filter_str:
            self._filter_ed.set_text_all('')
        if self._last_applied_filter == filter_str:
            return

        if tree_click and not filter_str:
            self.toggle_filter(show=False)

        self._last_applied_filter = filter_str
        self.filter_val = filter_str

        opts = self.get_filtered_opts()
        pass;       LOG and print(' __ set_filter: opts len: {}'.format(len(opts)))
        self.update_list(opts)

        # history
        if filter_str:
            try:
                ind = filter_history.index(filter_str)
                del filter_history[ind]
            except ValueError:
                pass

            filter_history.append(filter_str)
            del filter_history[:-ui_max_history_edits]

        # update combo items
        self._filter_ed.set_prop(PROP_COMBO_ITEMS, '\n'.join(reversed(filter_history)))


    def set_sort(self, sort_name):
        pass;       LOG and print(' setting sort: {}'.format(sort_name))

        if self.current_sort == sort_name: # switch order
            self.sort_reverse = not self.sort_reverse
        else:
            self.current_sort = sort_name
            self.sort_reverse = False  # back to ascending
        self.update_list_layout()

        # if not present in map -- special value - send as is
        opts = self.get_filtered_opts()
        pass;       LOG and print(' __ set_sort: opts len: {}'.format(len(opts)))
        self.update_list(opts)


    # ignore no change
    def add_opt_change(self, name, scope, val=None):
        """ val=None -- remove option binding for scope
        """
        _old_val = self.optman.get_scope_value(name, scope)

        # check if already have a opt_change for this option+scope -> ovewrite (delete old)
        for i,change in enumerate(self._opt_changes):
            if change.name == name and change.scope == scope:
                del self._opt_changes[i]
                break

        if val is not None:  ### setting value
            if val == _old_val: # no change - ignore
                return
        else:  ### removing value
            if _old_val is None: # no chage - ignore
                return

        # if resetting value -- ask confirmation
        scam = app_proc(PROC_GET_KEYSTATE, '')
        if scam != 'c'  and  self.scope != 'f'  and  val is None:
            _scope_cap = self._scope_captions[self.scope]
            _jval = self.optman.get_opt_scope_value(self._cur_opt, scope=self.scope, is_ui=True)
            _msg = _('Remove option [{}]\n   {} = {!r}\n?').format(_scope_cap,  self._cur_opt_name,  _jval)
            res = msg_box(_msg, MB_OKCANCEL + MB_ICONQUESTION)
            if res != ID_OK:
                return

        lex = ed.get_prop(PROP_LEXER_FILE)  if scope == 'l' else None
        opt_change = OptChange(name,  scope,  val,  lexer=lex,  old_value=_old_val)
        pass;       LOG and print('NOTE: new option change: '+str(opt_change))
        msg_status(_('Preference: ') + format_opt_change(opt_change))
        self._opt_changes.append(opt_change)


    def _fill_tree(self, d, parent=0):
        if parent == 0:
            item_id = tree_proc(self._h_tree, TREE_ITEM_ADD, text=TREE_ITEM_ALL)

        for name,d_ in d.items():
            # add item
            item_id = tree_proc(self._h_tree, TREE_ITEM_ADD, id_item=parent, index=-1, text=name)
            d_['item_id'] = item_id

            items = d_.get('kids')
            if items:
                self._fill_tree(items, parent=item_id)


    def on_opt_val_edit(self, id_dlg, id_ctl, data='', info=''):
        """ "change" callback for: option-edit field, 'edit-value' btn
        """
        ed_name = self.val_eds.get_name(id_ctl)
        prop_type = self._cur_opt['frm']
        pass;       LOG and print(' + ed name: {} [{}]'.format(ed_name, prop_type))

        if ed_name == ValueEds.WGT_NAME__EDIT:       # str, int, float, -hotk  + ###
            if prop_type == '#rgb'  or  prop_type == '#rgb-e':
                self._update_rgb_edit()

            self.toggle_mod_indicator(by_timer=True)

            key_code, key_state = data
            if key_code != VK_ENTER:
                return

            val = self.val_eds.get_edited_value(self._cur_opt)
            if val is None:
                return

        elif ed_name == ValueEds.WGT_NAME__COMBO:   # font, int2s, str2s, strs ###
            val = self.val_eds.val_combo.get_text_all()
            # only accept values from combo-items
            if val not in self.val_eds.val_combo.get_prop(PROP_COMBO_ITEMS):
                pass;       LOG and print('NOTE: val not in combo: {}'.format(val))
                return

            val = map_option_value(self._cur_opt, caption=val)

        elif ed_name == ValueEds.WGT_NAME__CHECK:                       # bool ###
            val = self.val_eds.cb_value

        elif ed_name == ValueEds.WGT_NAME__BTN_EDIT: # edit btn: hotk, color, json, file ###
            val = self._dlg_value(prop_type)
            if val is not None:
                with ignore_edit(self.h, self.val_eds.val_edit):
                    self.val_eds.val_edit.set_text_all(str(val))

                if prop_type in {'#rgb', '#rgb-e'}:
                    self._update_rgb_edit()
            else: # canceled dialog
                return

        self.toggle_mod_indicator(show=True)
        self.add_opt_change(self._cur_opt_name, self.scope, val)


    def _on_opt_click(self, id_dlg, id_ctl, data='', info=''):
        #print('LIST CIKCK: {}'.format((id_dlg, id_ctl, data, info)))

        _sel_ind = listbox_proc(self._h_list, LISTBOX_GET_SEL)
        if _sel_ind == -1  or  not self._list_opt_names:  #  nothing selected disable bottom panel
            self._clear_opt_edits()
            dlg_proc(self.h, DLG_CTL_PROP_SET, name='panel_value', prop={'en':False})
            return

        # enable bottom panel before manipulations
        dlg_proc(self.h, DLG_CTL_PROP_SET, name='panel_value', prop={'en':True})

        self._cur_opt_name = self._list_opt_names[_sel_ind]
        self._cur_opt = self.optman.get_opt(self._cur_opt_name)
        self.opt_comment_ed.set_text_all(self._cur_opt.get('cmt', ''))

        # if have a change for this option -- show it
        is_opt_modified = False
        removed_scopes = set(self.hidden_scopes)
        for opt_change in reversed(self._opt_changes):
            if opt_change.name == self._cur_opt_name:
                is_opt_modified = True
                if opt_change.value is not None:  # setting value
                    # (scope, val) - [f],[l],[u], [def]
                    _opt = self.optman.get_opt(opt_change.name)
                    ui_val = self.optman.value2uival(_opt, opt_change.value)
                    active_scoped_val = (opt_change.scope,  ui_val)
                    pass;       LOG and print('NOTE: using change value: '+str(opt_change))
                    break
                else: # unsetting option
                    removed_scopes.add(opt_change.scope)
        else: # no matching changes
            #active_scoped_val = self.optman.get_opt_active_value(self._cur_opt, is_ui=False, with_scope=True)
            # skip values that were reset,
            scopes = (scope  for scope in ['f', 'l', 'u', 'def']    if scope not in removed_scopes)
            scoped_vals = ((sc, self.optman.get_opt_scope_value(self._cur_opt, sc, is_ui=False))    for sc in scopes)
            active_scope = next(sc for sc,val in  scoped_vals  if val is not None) # result - is not None
            active_scope_val = self.optman.get_opt_scope_value(self._cur_opt, active_scope, is_ui=True) # for UI
            active_scoped_val = (active_scope, active_scope_val)
            pass;       LOG and print(' *** using option value: {}; removed:{}'.format(active_scoped_val, removed_scopes))

        self.toggle_mod_indicator(show=is_opt_modified)

        new_scope, _new_val = active_scoped_val

        # set scope
        new_scope_name = self._scope_captions[new_scope]
        with ignore_edit(self.h, self.scope_ed):
            self.scope_ed.set_text_all(new_scope_name)
        self.val_eds.set_type(self.h,  self._cur_opt, scoped_val=active_scoped_val)

        # rgb stuff
        prop_type = self._cur_opt['frm']
        if prop_type in {'#rgb', '#rgb-e'}:
            self._update_rgb_edit()


    def _on_reset(self, id_dlg, id_ctl, data='', info=''):
        """ remove option for current scope
        """
        self.add_opt_change(self._cur_opt_name, self.scope, val=None)
        # update value for current scope
        self._on_scope_change(-1,-1)

    def _on_scope_change(self, id_dlg, id_ctl, data='', info=''):
        if not self._cur_opt:
            return

        # check if have 'value' for 'scope' in ._opt_changes
        for opt_change in reversed(self._opt_changes):
            if opt_change.name == self._cur_opt_name  and  opt_change.scope == self.scope:
                cur_scope_val = opt_change.value  or  ''
                self.toggle_mod_indicator(show=True)
                break
        else:
            cur_scope_val = self.optman.get_opt_scope_value(self._cur_opt, scope=self.scope, is_ui=True)
            self.toggle_mod_indicator(show=False)

        pass;       LOG and print(' -- scoped val:{}:[{}]'.format(self.scope, cur_scope_val))

        self.val_eds.set_type(self.h,  self._cur_opt, scoped_val=(self.scope, cur_scope_val))

    def _on_filter(self, id_dlg, id_ctl, data='', info=''):
        if isinstance(data, tuple):     # on_key_down
            key_code, key_state = data
            if key_code == VK_ENTER  and  not key_state:
                # reset tree selection if selected item not in new filter
                selected_node = tree_proc(self._h_tree, TREE_ITEM_GET_SELECTED)
                if selected_node:
                     path = get_tree_path(self._h_tree, item_id=selected_node)
                     if '@'+path not in self.filter_val.split():
                        tree_proc(self._h_tree, TREE_ITEM_SELECT, id_item=0)

                _t0 = time.time()
                self.set_filter(self.filter_val)
                _t1 = time.time()
                pass;       LOG and print('* set-filter time:{:.3f}s'.format(_t1-_t0))

        #else:   # on_change  (typing, pasting)
            #print('        . CHANGE')

    def _on_tree_click(self, id_dlg, id_ctl, data='', info=''):
        if data == 0:   # deselected items
            return

        path = get_tree_path(self._h_tree, item_id=data)
        if path == TREE_ITEM_ALL:  # show all
            self.set_filter('', tree_click=True)
        else:
            new_filter = '@'+path

            _keys = app_proc(PROC_GET_KEYSTATE, '')
            is_adding = set(_keys) == set('cL')
            if is_adding  and  self.filter_val:
                if new_filter in self.filter_val.split(): # already in filter - ignore
                    return
                new_filter = self.filter_val +' '+ new_filter

            self.set_filter(new_filter)

    def _on_header_click(self, id_dlg, id_ctl, data='', info=''):
        pass;       LOG and print('--- Header click-: {}'.format((id_dlg, id_ctl, data, info)))
        column_captions, _col_ws = self.columns
        col_ind = data
        self.set_sort(column_captions[col_ind])

    def _on_key(self, id_dlg, id_ctl, data='', info=''):
        key_code = id_ctl
        state = data
        #print(' on -key:{}'.format((key_code, state)))

        if key_code == VK_F  and  state == 'c': # Ctrl+F -- show+focus filter
            self.toggle_filter(show=True)
            self._filter_ed.focus()
            return False # consumed

        elif key_code == VK_ESCAPE  and  not state:  # <escape> in filter - clear
            if self._filter_ed.get_prop(PROP_FOCUSED)  and  self.filter_val:
                self.set_filter('')
                #self.toggle_filter(show=False)
                return False # consumed

    def listbox_menu(self, id_dlg, id_ctl, data='', info=''):
        if data['y'] < listbox_proc(self._h_list, LISTBOX_GET_ITEM_H): # is header click
            # create menu on first run
            if not self._h_col_menu:
                self._h_col_menu = menu_proc(0, MENU_CREATE)

                for colname in COLS_LIST:
                    if colname in self.hidden_columns:
                        continue

                    la = lambda col=colname: self.on_toggle_col(col)
                    ui_col_name = ui_column(colname)
                    item_id = menu_proc(self._h_col_menu, MENU_ADD,
                                command=la, caption=ui_col_name, tag=colname)

                    _enabled = colname != COL_OPT_NAME # 'option name' column - always shown
                    menu_proc(item_id, MENU_SET_ENABLED, command=_enabled)

                menu_proc(self._h_col_menu, MENU_ADD, caption='-')

                la = lambda: self.configure_columns()
                menu_proc(self._h_col_menu, MENU_ADD, command=la, caption=_('Configure...'))
            #end if


            # update check state
            current_columns, _col_ws = self.columns
            for prop in menu_proc(self._h_col_menu, MENU_ENUM):
                _checked = prop['tag'] in current_columns
                menu_proc(prop['id'], MENU_SET_CHECKED, command=_checked)

            menu_proc(self._h_col_menu, MENU_SHOW)

    def on_toggle_col(self, info):
        pass;       LOG and print('NOTE: toggling column: '+str(info))

        col_cfg = opt_col_cfg[:]

        colname = info
        cur_col_names = [name for name,_w in col_cfg]
        if colname in cur_col_names:  # disableg column
            del opt_col_cfg[cur_col_names.index(colname)]
        else:  # add new column
            new_col_w = 100
            if   colname == '!':          new_col_w = '19px'
            elif colname == COL_SECTION:  new_col_w = '120px'

            opt_col_cfg.append((colname, new_col_w))
            opt_col_cfg.sort(key=lambda item: COLS_LIST.index(item[0]))
        pass;       LOG and print(' -- new columns: '+str(opt_col_cfg))

        self.update_list_layout()
        _opts = self.get_filtered_opts()
        self.update_list(_opts)


    def _clear_opt_edits(self):
        """ disables: 'scope combo', 'option comment'
        """
        with ignore_edit(self.h, self.opt_comment_ed):
            self.opt_comment_ed.set_text_all('')
        with ignore_edit(self.h, self.scope_ed):
            self.scope_ed.set_text_all('')

        self.val_eds.clear_edits(self.h)

    def _dlg_value(self, prop_type):
        """ editing option value with a dialog
            returns: new value
        """

        if prop_type == 'hotk':    # HOTKEY
            val = dlg_hotkey(title=self._cur_opt_name)

        elif prop_type in {'#rgb', '#rgb-e'}:  # RGB;  val == None or correct html color
            # empty ('-e') -- only for edit field
            cur_scol = self.val_eds.val_edit.get_text_all()
            try:
                int_col = apx.html_color_to_int(cur_col)
            except:
                int_col = 0xffffff

            val = dlg_color(int_col)

            print(f' dlg color: {str(val)}')
            if val is not None:
                try:
                    val = apx.int_to_html_color(val)

                    print(f' proper color: {val}')
                except:
                    print(f' Exception color!')
                    val = None

        elif prop_type == 'file':
            caption = _('Choose file: {}').format(self._cur_opt_name)
            val = dlg_file(is_open=False, init_filename='', init_dir='', filters='', caption=caption)

        elif prop_type == 'json':
            from .dlg_json import JsonEd

            j_ed = JsonEd(self._cur_opt, self.scope)
            val = j_ed.edit_json()
        #end if

        return val


    def toggle_filter(self, show=False):
        #dlg_proc(self.h, DLG_CTL_PROP_SET, name='panel_filter', prop={'vis': show})

        if show == False:  # if hiding filter - reset tree selection to 'All'
            for item_id,name in tree_proc(self._h_tree, TREE_ITEM_ENUM):
                if name == TREE_ITEM_ALL:
                    tree_proc(self._h_tree, TREE_ITEM_SELECT, id_item=item_id)

    def toggle_mod_indicator(self, tag='', info='', show=True, by_timer=False):
        if by_timer:
            timer_proc(TIMER_START_ONE, self.toggle_mod_indicator, 30, tag='ed_check_state')
        else:
            if tag == 'ed_check_state':
                ed_line_state = self.val_eds.val_edit.get_prop(PROP_LINE_STATE, 0)
                if ed_line_state == LINESTATE_NORMAL:
                    return

            dlg_proc(self.h, DLG_CTL_PROP_SET, name='mod_label', prop={'vis':show})

    def apply_changes(self, closing=False):
        """ batch apply qued option changes
        """
        pass;       LOG and print('APPLY_CHANGES')

        # check if current value in edit is changed, create option change if it is
        try:
            edit_val = self.val_eds.get_edited_value(self._cur_opt)
        except ValueError:      # exception happens when trying to cast empty str to float|int
            edit_val = None
        if edit_val is not None  and  edit_val != '':
            self.add_opt_change(self._cur_opt_name, self.scope, edit_val)

        if not self._opt_changes  and  not closing:
            msg_status(_("No option changes has been made"))
            return

        for i,change in enumerate(self._opt_changes):
            is_last = i == len(self._opt_changes) - 1
            if change.value is not None: # set value
                self.optman.set_opt(name=change.name,  scope=change.scope,  val=change.value,
                        lexer=change.lexer,  apply_=is_last)
            else: # removing value
                self.optman.reset_opt(name=change.name,  scope=change.scope,
                        lexer=change.lexer,  apply_=is_last)

        self._opt_changes.clear()
        self.optman.on_opts_change()
        _opts = self.get_filtered_opts()
        self.update_list(_opts)

    def _update_rgb_edit(self, tag='', info=''):
        """ empty tag - ques in timer
        """
        if not tag:
            timer_proc(TIMER_START_ONE, self._update_rgb_edit, 100, tag='on_timer')
        else:
            if self._closing is not None:
                ValueEds.update_ed_color(self.val_eds.val_edit)


    def dlg_help(self, *args, **vargs):
        if self._h_help == None:
            w, h = 600, 450
            self._h_help = dlg_proc(0, DLG_CREATE)

            colors = app_proc(PROC_THEME_UI_DICT_GET, '')
            col_ed_bg = colors['EdTextBg']['color']
            col_ed_font = colors['EdTextFont']['color']
            color_form_bg = colors['ButtonBorderPassive']['color']

            dlg_proc(self._h_help, DLG_PROP_SET,
                        prop={'cap': _('Help'),
                            'w': w,
                            'h': h,
                            'resize': True,
                            'color': color_form_bg,
                            }
                        )

            n = dlg_proc(self._h_help, DLG_CTL_ADD, 'memo')
            dlg_proc(self._h_help, DLG_CTL_PROP_SET, index=n,
                        prop={
                            'name': 'help_memo',
                            'align': ALIGN_CLIENT,
                            'val': HELP_TEXT,
                            'sp_a':6,
                            'color': col_ed_bg,
                            'font_color': col_ed_font,
                            }
                        )

        dlg_proc(self._h_help, DLG_SHOW_MODAL)

    def close(self):
        self._save_dlg_cfg()

        self._closing = True

        dlg_proc(self.h, DLG_HIDE)


class ValueEds:
    """ * Responsible for: widgets for editing different value formats
        * Formats: bool, float, font, font-e, hotk, int, int2s, str, str2s, strs,
    """
    VALUE_ED_PANEL = 'panel_value'
    VALUE_ED_RESET = 'btn_val_reset'

    WGT_NAME__EDIT       = 'cur_val__edit'
    WGT_NAME__COMBO      = 'cur_val__combo'
    WGT_NAME__CHECK      = 'cur_val__check'

    WGT_NAME__BTN_EDIT   = 'cur_val__edit_btn'

    type_map = {
        'str':      WGT_NAME__EDIT,
        'bool':     WGT_NAME__CHECK,
        'float':    WGT_NAME__EDIT,
        'font':     WGT_NAME__COMBO,
        'font-e':   WGT_NAME__COMBO,
        'hotk':     WGT_NAME__EDIT,
        'int':      WGT_NAME__EDIT,
        'int2s':    WGT_NAME__COMBO,
        'str2s':    WGT_NAME__COMBO,
        'strs':     WGT_NAME__COMBO,

        '#rgb':     WGT_NAME__EDIT,
        '#rgb-e':   WGT_NAME__EDIT,
        'file':     WGT_NAME__EDIT,
        'json':     WGT_NAME__EDIT,
    }

    EXTRA_BTN_TYPES = {'hotk', '#rgb', '#rgb-e', 'file', 'json'}

    FN_CHECKBOX_ICONS = {
        False: 'cb_unckecked.png',
        None:  'cb_none.png',
        True:  'cb_checked.png',
    }

    _h_cb_iml = None
    _cb_icons = {} # False, None, True -> imagelist index

    def __init__(self, val_change_callback):
        self._val_change_callback = val_change_callback
        self._ctl_names = {} # id_ctl -> name
        self._current_type = None
        self.val_edit = None
        self.val_combo = None

        self._ignore_input = False

    @property
    def cb_value(self):
        """ returns True, False, None
        """
        imind = button_proc(self._h_cbx, BTN_GET_IMAGEINDEX)

        for val,ind in ValueEds._cb_icons.items():
            if ind == imind:
                return val

    def set_type(self, h, opt, scoped_val):
        M = ValueEds

        scope, value = scoped_val

        newtype = opt.get('frm')

        pass;       LOG and print('* SET type-value-ed: type:{}, val:{}'.format(
                                                                        newtype, (scope, value, type(value))))

        self._hide_val_ed(h)

        # unsupported option format
        if newtype not in M.type_map:
            print(_('PreferenesError: unsupported option type: ')+str(newtype))
            return


        # disable option editing?  (some options cannot be a file opt)
        if scope == 'f'  and  not opt['opt'] in FILE_OPTS \
                or  scope == 'l'  and  GLOBAL_OP_CMT in opt['cmt']:

            pass;       LOG and print('NOTE: option NA: disabling')
            n = self._wgt_ind(h, M.type_map['str'], show=True) # ~resets wgt props
            self._current_type = 'str'

            hint = _('Not available for a file')  if scope=='f' else  _('Not available for lexers')
            dlg_proc(h, DLG_CTL_PROP_SET, index=n, prop={
                    'en': False,
                    'texthint': hint,
                    })
            with ignore_edit(h, self.val_edit):
                self.val_edit.set_text_all('')
            return


        type_wgt_name = M.type_map[newtype]
        # show option-edit widget  and gets widget index
        n = self._wgt_ind(h, type_wgt_name, show=True) # ~resets wgt props

        if newtype == 'str':
            self.val_edit.set_text_all(value or '')

        elif newtype == 'bool':
            #NOTE: UI bool values are: True, False, and '' for empty scope value
            if   value is True:  imind = ValueEds._cb_icons[value]
            elif value is False: imind = ValueEds._cb_icons[value]
            else:                imind = ValueEds._cb_icons[None]

            button_proc(self._h_cbx, BTN_SET_IMAGEINDEX, imind)

        elif newtype == 'float':
            self.val_edit.set_text_all(str(value) or '')

        elif newtype == 'font'  or  newtype == 'font-e': # font-e already has empty value in list
            self.val_combo.set_prop(PROP_COMBO_ITEMS, '\n'.join(opt['lst']))
            with ignore_edit(h, self.val_combo):
                self.val_combo.set_text_all(value)

        elif newtype == 'hotk':
            self.val_edit.set_text_all(value)
            self.val_edit.set_prop(PROP_RO, True)

            self.layout_ed_btn(h, n, '...')

        elif newtype == 'int':
            self.val_edit.set_text_all(str(value))
            self.val_edit.set_prop(PROP_NUMBERS_ONLY, True)

        elif newtype == 'int2s':
            #ed_val = map_option_value(opt, caption=value)
            with ignore_edit(h, self.val_combo):
                self.val_combo.set_text_all(value)
            self.val_combo.set_prop(PROP_COMBO_ITEMS, '\n'.join(opt['jdc']))

        elif newtype == 'str2s':
            #ed_val = map_option_value(opt, caption=value)
            with ignore_edit(h, self.val_combo):
                self.val_combo.set_text_all(value)
            self.val_combo.set_prop(PROP_COMBO_ITEMS, '\n'.join(opt['jdc']))

        elif newtype == 'strs':
            with ignore_edit(h, self.val_combo):
                self.val_combo.set_text_all(value)
            self.val_combo.set_prop(PROP_COMBO_ITEMS, '\n'.join(opt['lst']))

        elif newtype == '#rgb-e'  or  newtype == '#rgb':
            self.val_edit.set_text_all(str(value))

            self.val_edit.set_prop(PROP_GUTTER_ALL, True)
            self.val_edit.set_prop(PROP_GUTTER_STATES, False)
            self.val_edit.set_prop(PROP_GUTTER_NUM, False)

            self.layout_ed_btn(h, n, '...')

        elif newtype == 'file':
            self.val_edit.set_text_all(str(value))

            self.layout_ed_btn(h, n, _('Choose file...'))

        elif newtype == 'json':
            self.val_edit.set_text_all(str(value))
            self.val_edit.set_prop(PROP_RO, True)

            self.layout_ed_btn(h, n, _('Edit...'))
        #end if

        # set line state: normal (not edited)
        if type_wgt_name == M.WGT_NAME__EDIT:
            self.val_edit.set_prop(PROP_LINE_STATE, (0, LINESTATE_NORMAL))

        self._current_type = newtype

    def clear_edits(self, h):
        M = ValueEds

        self._hide_val_ed(h)
        _n = self._wgt_ind(h, M.WGT_NAME__EDIT, show=True) # ~resets wgt props
        self.val_edit.set_text_all('')

    def get_name(self, id_ctl):
        return self._ctl_names.get(id_ctl)

    def get_edited_value(self, opt):
        """ if 'WGT_NAME__EDIT' is current editor - return parsed value
            else None

        """
        M = ValueEds

        type_wgt_name = M.type_map[self._current_type]

        if type_wgt_name == M.WGT_NAME__EDIT:
            val = self.val_edit.get_text_all()

            prop_type = opt['frm']
            if prop_type == 'int':
                val = int(val)
            elif prop_type == 'float':
                val = float(val)
            elif prop_type in {'#rgb', '#rgb-e'}:
                if val == '':
                    if prop_type != '#rgb-e':
                        msg_status(_('Option "{}" does not accept empty value').format(opt['opt']))
                        return
                else:
                    try:
                        apx.html_color_to_int(val)
                    except:
                        msg_status(_('Incorrect color token: ') + val)
                        return
            return val


    def _wgt_ind(self, h, name, show=False):
        """ creates widget if didn't exist
            returns: widget's form index
        """
        M = ValueEds

        default_props = {
            'name': name,
            'p': M.VALUE_ED_PANEL,
            'h': BTN_H, 'max_h': BTN_H,
            'a_l': ('mod_label', ']'),
            'a_t': (M.VALUE_ED_RESET, '['),
            'a_r': (M.VALUE_ED_RESET, '['),
            'a_b': (M.VALUE_ED_RESET, ']'),
            #'sp_l': PAD,
            'act': True, 'en': True,
        }

        #TODO validate name
        if name == M.WGT_NAME__EDIT:
            n = dlg_proc(h, DLG_CTL_FIND, prop=name)
            if n == -1:     # add if not already
                n = dlg_proc(h, DLG_CTL_ADD, 'editor_edit')
                h_ed = dlg_proc(h, DLG_CTL_HANDLE, index=n)
                self._ctl_names[n] = name
                self.val_edit = Editor(h_ed)
                if font_size and font_name:
                    self.val_edit.set_prop(PROP_FONT, (font_name, font_size))

            # resetting to defaults
            _props = {**default_props,
                    'on_key_down': self._val_change_callback,
                    'texthint': '',}
            dlg_proc(h, DLG_CTL_PROP_SET, index=n, prop=_props)

            ValueEds.reset_edt(self.val_edit)

        elif name == M.WGT_NAME__COMBO:
            n = dlg_proc(h, DLG_CTL_FIND, prop=name)
            if n == -1:     # add if not already
                n = dlg_proc(h, DLG_CTL_ADD, 'editor_combo')

                _props = {**default_props,   'on_change': self._val_change_callback,}
                dlg_proc(h, DLG_CTL_PROP_SET, index=n, prop=_props)

                h_ed = dlg_proc(h, DLG_CTL_HANDLE, index=n)
                self._ctl_names[n] = name
                self.val_combo = Editor(h_ed)
                self.val_combo.set_prop(PROP_RO, True)
                if font_size and font_name:
                    self.val_combo.set_prop(PROP_FONT, (font_name, font_size))

        elif name == M.WGT_NAME__CHECK:
            n = dlg_proc(h, DLG_CTL_FIND, prop=name)
            if n == -1:     # add if not already
                n = dlg_proc(h, DLG_CTL_ADD, 'button_ex')
                dlg_proc(h, DLG_CTL_PROP_SET, index=n, prop={
                        **default_props,
                        'cap': _('Enable'),
                        'act': True,
                        'on_change': self._on_cb_click_proxy,
                        'font_color': COL_FONT,
                        })
                self._h_cbx = dlg_proc(h, DLG_CTL_HANDLE, index=n)
                self._ctl_names[n] = name

                button_proc(self._h_cbx, BTN_SET_FLAT, True)
                button_proc(self._h_cbx, BTN_SET_KIND, BTNKIND_TEXT_ICON_HORZ)
                # icons  (checkbox)
                h_iml = ValueEds._get_checkbox_imagelist()
                button_proc(self._h_cbx, BTN_SET_IMAGELIST, h_iml)
            #end if

        # Extra
        elif name == M.WGT_NAME__BTN_EDIT:
            n = dlg_proc(h, DLG_CTL_FIND, prop=name)
            if n == -1:     # add if not already
                n = dlg_proc(h, DLG_CTL_ADD, 'button_ex')
                dlg_proc(h, DLG_CTL_PROP_SET, index=n, prop={
                        **default_props,
                        'cap': '...',
                        'on_change': self._val_change_callback,
                        })
                self._ctl_names[n] = name
        #end if

        if show:   # set visible
            dlg_proc(h, DLG_CTL_PROP_SET, index=n, prop={'vis': True})

        return n

    def _hide_val_ed(self, h):
        M = ValueEds

        if not self._current_type:
            return

        to_hide = [M.type_map[self._current_type]]

        if self._current_type in M.EXTRA_BTN_TYPES:
            to_hide.append(M.WGT_NAME__BTN_EDIT)

        for name in to_hide:
            dlg_proc(h, DLG_CTL_PROP_SET, name=name, prop={'vis':False})

    def _on_cb_click_proxy(self, id_dlg, id_ctl, data='', info=''):
        """ changes button_ex checkbox icon to proper value, and sends control click to default callback
        """
        cb_val = self.cb_value
        # cycle: True => False => None   => True...
        if   cb_val is True:  nextind = ValueEds._cb_icons[False]
        elif cb_val is False: nextind = ValueEds._cb_icons[None]
        else:                 nextind = ValueEds._cb_icons[True]

        button_proc(self._h_cbx, BTN_SET_IMAGEINDEX, nextind)

        self._val_change_callback(id_dlg, id_ctl, data, info)

    def layout_ed_btn(self, h, n, caption):
        M = ValueEds

        w = int(BTN_W*0.5) if caption == '...' else BTN_W

        btn_n = self._wgt_ind(h, M.WGT_NAME__BTN_EDIT, show=True)
        dlg_proc(h, DLG_CTL_PROP_SET, index=btn_n, prop={
                'a_l': None,       'a_r': (M.VALUE_ED_RESET, '['),
                'w': w, 'max_w': w, 'sp_l': 2,
                'cap': caption,
        })
        # ... edit
        dlg_proc(h, DLG_CTL_PROP_SET, index=n, prop={
                #'a_l': ('', '['),
                'a_r': (M.WGT_NAME__BTN_EDIT, '[')
        })

    def update_ed_color(edt):
        try:
            int_col = apx.html_color_to_int(edt.get_text_all())
        except:     # invalid color -> reset to theme color
            colors = app_proc(PROC_THEME_UI_DICT_GET, '')
            int_col = colors['EdGutterBg']['color']
        edt.set_prop(PROP_COLOR, (COLOR_ID_GutterBg, int_col))


    def reset_edt(edt):
        edt.set_prop(PROP_NUMBERS_ONLY, False)
        edt.set_prop(PROP_RO, False)
        edt.set_prop(PROP_GUTTER_ALL, False)

    @classmethod
    def _get_checkbox_imagelist(cls):
        # load icons only once
        if not ValueEds._h_cb_iml:
            ValueEds._h_cb_iml,  ValueEds._cb_icons = load_imagelist(ValueEds.FN_CHECKBOX_ICONS)

        return ValueEds._h_cb_iml

class OptionMapValueError(Exception):
    pass

HELP_TEXT = _("""About "Filter"
 Suitable options will contain all specified words.
 Tips and tricks:
 • Add "#" to search the words also in comments.
 • Add "@sec" to show options from section with "sec" in name.
   Several sections are allowed.
   Click item in sections tree with Ctrl to add it.
 • To show only overridden options:
   - Add "!"   to show only User+Lexer+File.
   - Add "!!"  to show only Lexer+File
   - Add "!!!" to show only File.
 • Use "<" or ">" for word boundary.
     Example:
       size> <tab
     selects "tab_size" but not "ui_tab_size" or "tab_size_x".

 • Values in table column "!"
     !   option is set in "user.json",
     !!  option is set in "lexer NNN.json",
     !!! option is set for current file,
     L   default value is from "settings_default/lexer NNN.json",
     +   not CudaText standard option.
""")
