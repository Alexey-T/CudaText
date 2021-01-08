import os
from cudatext import *
import cudatext_cmd as cmds
from cudax_lib import get_translation

_   = get_translation(__file__)  # I18N

fn_ini = os.path.join(app_path(APP_DIR_SETTINGS), 'plugins.ini')
W_all = 650
H_all = 500

class DialogEmmet:

    def __init__(self, do_expand, do_insert):

        self.do_expand = do_expand
        self.do_insert = do_insert

        self.h = dlg_proc(0, DLG_CREATE)
        dlg_proc(self.h, DLG_PROP_SET, prop={
            'w': W_all,
            'h': H_all,
            'cap': _('Emmet preview dialog'),
            'border': DBORDER_SIZE,
            'w_min': 300,
            'h_min': 150,
        })

        n = dlg_proc(self.h, DLG_CTL_ADD, prop='label')
        dlg_proc(self.h, DLG_CTL_PROP_SET, index=n, prop={
            'name': 'label1',
            'cap': _('Abbreviation:'),
            'x': 6,
            'y': 6,
        })

        n = dlg_proc(self.h, DLG_CTL_ADD, prop='button')
        dlg_proc(self.h, DLG_CTL_PROP_SET, index=n, prop={
            'name': 'btn_ok',
            'y': 26,
            'w': 90,
            'a_r': ('', ']'),
            'a_l': None,
            'sp_r': 6,
            'cap': _('Insert'),
            'ex0': True, #default for Enter
            'on_change': self.on_ok_click,
        })

        n = dlg_proc(self.h, DLG_CTL_ADD, prop='button')
        dlg_proc(self.h, DLG_CTL_PROP_SET, index=n, prop={
            'name': 'btn_copy',
            'a_l': ('btn_ok', '['),
            'a_r': ('btn_ok', ']'),
            'a_t': ('btn_ok', ']'),
            'sp_t': 6,
            'cap': _('Copy'),
            'on_change': self.on_copy_click,
        })

        n = dlg_proc(self.h, DLG_CTL_ADD, prop='edit')
        dlg_proc(self.h, DLG_CTL_PROP_SET, index=n, prop={
            'name': 'input',
            'x': 6,
            'y': 26,
            'a_r': ('btn_ok', '['),
            'sp_r': 6,
            'act': True,
            'on_change': self.on_edit_change,
            'tab_order': 0,
        })

        n = dlg_proc(self.h, DLG_CTL_ADD, prop='memo')
        dlg_proc(self.h, DLG_CTL_PROP_SET, index=n, prop={
            'name': 'preview',
            'ex0': True, #read only
            'a_l': ('', '['),
            'a_t': ('input', ']'),
            'a_r': ('input', ']'),
            'a_b': ('', ']'),
            'sp_t': 6,
            'sp_b': 6,
            'sp_l': 6,
        })


    def result(self, no_stops):
        
        text = dlg_proc(self.h, DLG_CTL_PROP_GET, name='input')['val']
        if text:
            text = self.do_expand(text)
            if text and no_stops:
                for i in range(10):
                    text = text.replace('${%d}'%i, '')
        return text

    def on_edit_change(self, id_dlg, id_ctl, data='', info=''):

        text = self.result(False) # better keep tabstops
        if text:
            s = text.replace('\t', '    ')
            s = s.split('\n')
            s = '\t'.join(s)
            dlg_proc(self.h, DLG_CTL_PROP_SET, name='preview', prop={
                'val': s,
            })

    def on_ok_click(self, id_dlg, id_ctl, data='', info=''):

        text = self.result(False)
        dlg_proc(self.h, DLG_HIDE)
        if text:
            self.do_insert(text)

    def on_copy_click(self, id_dlg, id_ctl, data='', info=''):

        text = self.result(True)
        if text:
            app_proc(PROC_SET_CLIP, text)

    def show(self):

        dlg_proc(self.h, DLG_CTL_FOCUS, name='input')
        self.pos_load()
        self.on_edit_change(0, 0)
        dlg_proc(self.h, DLG_SHOW_MODAL)
        self.pos_save()

    def close(self):

        dlg_proc(self.h, DLG_HIDE)

    def pos_load(self):

        x = int(ini_read(fn_ini, 'emmet', 'x', '-1'))
        y = int(ini_read(fn_ini, 'emmet', 'y', '-1'))
        w = int(ini_read(fn_ini, 'emmet', 'w', '-1'))
        h = int(ini_read(fn_ini, 'emmet', 'h', '-1'))
        if x<0: return

        dlg_proc(self.h, DLG_PROP_SET, prop={'x':x, 'y':y, 'w':w, 'h':h, })

    def pos_save(self):

        prop = dlg_proc(self.h, DLG_PROP_GET)
        if not prop: return
        x = prop['x']
        y = prop['y']
        w = prop['w']
        h = prop['h']

        ini_write(fn_ini, 'emmet', 'x', str(x))
        ini_write(fn_ini, 'emmet', 'y', str(y))
        ini_write(fn_ini, 'emmet', 'w', str(w))
        ini_write(fn_ini, 'emmet', 'h', str(h))
