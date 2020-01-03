from cudatext import *

W_all = 600
H_all = 500

class DialogEmmet:

    def __init__(self):

        self.h = dlg_proc(0, DLG_CREATE)
        dlg_proc(self.h, DLG_PROP_SET, prop={
            'w': W_all,
            'h': H_all,
            'cap': 'Emmet preview dialog',
            'border': DBORDER_SIZE,
        })

        n = dlg_proc(self.h, DLG_CTL_ADD, prop='label')
        dlg_proc(self.h, DLG_CTL_PROP_SET, index=n, prop={
            'name': 'label1',
            'cap': 'Abbreviation:',
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
            'cap': 'Insert',
            'on_click': self.on_ok_click,
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
            'a_r': ('', ']'),
            'a_b': ('', ']'),
            'sp_a': 6,
        })


    def on_edit_change(self, id_dlg, id_ctl, data='', info=''):

        pass

    def on_ok_click(self, id_dlg, id_ctl, data='', info=''):

        dlg_proc(self.h, DLG_HIDE)

    def show(self):

        dlg_proc(self.h, DLG_CTL_FOCUS, name='input')
        dlg_proc(self.h, DLG_SHOW_MODAL)

    def close(self):

        dlg_proc(self.h, DLG_HIDE)
