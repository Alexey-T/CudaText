import json
import collections

from cudatext import *
from cudax_lib import get_translation, get_opt

_   = get_translation(__file__)  # I18N


PAD = 2
BTN_H = app_proc(PROC_GET_GUI_HEIGHT, 'button')
BTN_W = BTN_H*3
BTN_W_BIG = int(BTN_W*1.5)

COL_GREEN   = 0x7cc87c #7cc87c
COL_RED     = 0x7374d8 #d87473


class JsonEd:

    def __init__(self, opt, scope, state=None):
        self.opt = opt
        self.scope = scope
        self.h = None

        self.result = None

        self._state = None

    def edit_json(self):
        if not self.h:
            self.h = self.init_form()

        dlg_proc(self.h, DLG_SHOW_MODAL)

        # store window position
        form_prop = dlg_proc(self.h, DLG_PROP_GET)
        self._state = {'x':form_prop['x'], 'y':form_prop['y'], 'w':form_prop['w'], 'h':form_prop['h']}

        dlg_proc(self.h, DLG_FREE)
        self.h = None

        return self.result

    def get_state(self):
        return self._state


    def init_form(self):
        h = dlg_proc(0, DLG_CREATE)

        _colors = app_proc(PROC_THEME_UI_DICT_GET, '')
        color_form_bg = _colors['TabBg']['color']

        ###### FORM #######################
        dlg_proc(h, DLG_PROP_SET, prop={
                'cap': _('JSON option editor: {}').format(self.opt['opt']),
                'w': 600, 'h': 400,
                'border': DBORDER_SIZE,
                'color': color_form_bg,
                'topmost': True,
                })


        # option description #########
        n = dlg_proc(h, DLG_CTL_ADD, 'editor')
        dlg_proc(h, DLG_CTL_PROP_SET, index=n, prop={
                'name': 'editor',
                'align': ALIGN_CLIENT,
                'sp_l': PAD*2,  'sp_t': PAD*2, 'sp_r': PAD*2, 'sp_b': PAD*2,
                })
        h_ed = dlg_proc(h, DLG_CTL_HANDLE, index=n)
        self.edt = Editor(h_ed)

        # option description #########
        n = dlg_proc(h, DLG_CTL_ADD, 'editor')
        dlg_proc(h, DLG_CTL_PROP_SET, index=n, prop={
                'name': 'memo',
                'align': ALIGN_BOTTOM,
                'h': 200, 'max_h': 200,
                'sp_l': PAD*2,  'sp_r': PAD*2, 'sp_b': PAD + BTN_H + PAD*2,
                })
        h_ed = dlg_proc(h, DLG_CTL_HANDLE, index=n)
        self.memo = Editor(h_ed)


        ### Bottom Btns ###################
        # OK #######
        self.n_ok = dlg_proc(h, DLG_CTL_ADD, 'button_ex')
        dlg_proc(h, DLG_CTL_PROP_SET, index=self.n_ok, prop={
                'name': 'btn_ok',
                'h': BTN_H, 'max_h': BTN_H,
                'w': BTN_W,
                'a_l': None, 'a_t': None, 'a_r': ('', ']'),  'a_b': ('', ']'),
                'sp_r': PAD*2, 'sp_b': PAD*2,
                'cap': _('OK'),
                'on_change': self._on_btn_click,
                })
        # Cancel #######
        self.n_cancel = dlg_proc(h, DLG_CTL_ADD, 'button_ex')
        dlg_proc(h, DLG_CTL_PROP_SET, index=self.n_cancel, prop={
                'name': 'btn_cancel',
                'h': BTN_H, 'max_h': BTN_H,
                'w': BTN_W,
                'a_l': None, 'a_t': None, 'a_r': ('btn_ok', '['),  'a_b': ('', ']'),
                'sp_r': PAD*2, 'sp_b': PAD*2,
                'cap': _('Close'),
                'on_change': self._on_btn_click,
                })
        # Check #######
        self.n_check = dlg_proc(h, DLG_CTL_ADD, 'button_ex')
        dlg_proc(h, DLG_CTL_PROP_SET, index=self.n_check, prop={
                'h': BTN_H, 'max_h': BTN_H,
                'w': BTN_W,
                'a_l': None, 'a_t': None, 'a_r': ('btn_cancel', '['),  'a_b': ('', ']'),
                'sp_r': PAD*2, 'sp_b': PAD*2,
                'cap': _('Check'),
                'on_change': self._on_btn_click,
                })
        self.h_btn_check = dlg_proc(h, DLG_CTL_HANDLE, index=self.n_check)


        # Copy default #######
        self.n_load_def = dlg_proc(h, DLG_CTL_ADD, 'button_ex')
        dlg_proc(h, DLG_CTL_PROP_SET, index=self.n_load_def, prop={
                'name': 'btn_defaults',
                'h': BTN_H, 'max_h': BTN_H,
                'w': BTN_W_BIG,
                'a_l': ('', '['),  'a_t': None,  'a_b': ('', ']'),
                'sp_l': PAD*2, 'sp_b': PAD*2,
                'cap': _('Load default'),
                'on_change': self._on_btn_click,
                })
        # Undo changes #######
        self.n_undo = dlg_proc(h, DLG_CTL_ADD, 'button_ex')
        dlg_proc(h, DLG_CTL_PROP_SET, index=self.n_undo, prop={
                'h': BTN_H, 'max_h': BTN_H,
                'w': BTN_W_BIG,
                'a_l': ('btn_defaults', ']'), 'a_t': None,  'a_b': ('', ']'),
                'sp_l': PAD*2, 'sp_b': PAD*2,
                'cap': _('Undo changes'),
                'on_change': self._on_btn_click,
                })


        self.edt.set_prop(PROP_LEXER_FILE, 'JSON')

        _os_suffix = app_proc(PROC_GET_OS_SUFFIX, '')
        font = get_opt('font_name'+_os_suffix),  get_opt('font_size'+_os_suffix)

        for _edt in [self.edt, self.memo]:
            _edt.set_prop(PROP_GUTTER_ALL, False)
            _edt.set_prop(PROP_MINIMAP, False)
            _edt.set_prop(PROP_MICROMAP, False)
            _edt.set_prop(PROP_LAST_LINE_ON_TOP, False)
            _edt.set_prop(PROP_HILITE_CUR_LINE, False)

        self.edt.set_prop(PROP_GUTTER_ALL, True) # show line numbers on top editor
        self.edt.set_prop(PROP_GUTTER_NUM, True)

        self.edt.set_prop(PROP_GUTTER_BM, False)
        self.edt.set_prop(PROP_GUTTER_FOLD, False)
        self.edt.set_prop(PROP_GUTTER_STATES, False)


        self.undo_changes()

        dlg_proc(h, DLG_SCALE)

        return h

    def _on_btn_click(self, id_dlg, id_ctl, data='', info=''):
        if   id_ctl == self.n_ok:       self.ok()
        elif id_ctl == self.n_check:    self.check()
        elif id_ctl == self.n_load_def: self.copy_defaults()
        elif id_ctl == self.n_undo:     self.undo_changes()
        elif id_ctl == self.n_cancel:   self.close()


    def ok(self):
        j = self.check()
        if j != None:
            self.result = j
            self.close()


    def check(self):
        """ validate "json" in editor
            returns parsed `dict` or `None`
        """
        try:
            j = json.loads(self.edt.get_text_all(),  object_pairs_hook=collections.OrderedDict)
            check_col = COL_GREEN
            self.memo.set_text_all(self.opt['cmt'])
        except Exception as ex:
            j = None
            check_col = COL_RED

            self.memo.set_text_all(str(ex))

            self.edt.focus()
            self.edt.set_caret(ex.colno-1, ex.lineno-1)

        ## add green/red line to check btn
        button_proc(self.h_btn_check, BTN_SET_COLOR_LINE2, check_col)
        # reset after 1 sec
        timer_proc(TIMER_START_ONE, self._reset_check_col, 1000)
        button_proc(self.h_btn_check, BTN_UPDATE)

        return j

    def copy_defaults(self):
        self.edt.set_text_all(json.dumps(self.opt['def'], indent=2))

    def undo_changes(self):
        self.edt.set_text_all(self.opt['j{}vl'.format(self.scope)])
        self.memo.set_text_all(self.opt['cmt'])

    def close(self):
        dlg_proc(self.h, DLG_HIDE)

    def _reset_check_col(self, tag='', info=''):
        if self.h:
            button_proc(self.h_btn_check, BTN_SET_COLOR_LINE2, COLOR_NONE)
            button_proc(self.h_btn_check, BTN_UPDATE)

