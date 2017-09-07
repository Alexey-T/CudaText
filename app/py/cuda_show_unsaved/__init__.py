import os
import difflib
from cudatext import *

class Command:
    def show_unsaved(self):
        fn = ed.get_filename()
        fn_base = os.path.basename(fn)
        if not fn: return

        lines_cur = ed.get_text_all().splitlines()
        lines_orig = open(fn, 'r').read().splitlines()
        diff = list(difflib.unified_diff(lines_orig, lines_cur,
            fn+' (disk)',
            fn+' (editor)',
            lineterm=''))

        if diff==[]:
            msg_box('File is not changed', MB_OK+MB_ICONINFO)
            return

        self.text = '\n'.join(diff)+'\n'
        self.filename = fn_base
        self.h_dlg = self.init_editor_dlg()

        dlg_proc(self.h_dlg, DLG_SHOW_MODAL)
        dlg_proc(self.h_dlg, DLG_FREE)


    def init_editor_dlg(self):

        h=dlg_proc(0, DLG_CREATE)
        dlg_proc(h, DLG_PROP_SET, prop={
            'cap': 'Unsaved changes: '+self.filename,
            'w': 900,
            'h': 500,
            'resize': True,
            'keypreview': True,
            })

        n=dlg_proc(h, DLG_CTL_ADD, 'editor')
        dlg_proc(h, DLG_CTL_PROP_SET, index=n, prop={
            'name': 'ed',
            'x': 6,
            'y': 6,
            'a_r': ('', ']'),
            'a_b': ('', ']'),
            'sp_l': 6,
            'sp_t': 6,
            'sp_r': 6,
            'sp_b': 38,
            })

        h_editor = dlg_proc(h, DLG_CTL_HANDLE, index=n)
        ed0 = Editor(h_editor)
        ed0.set_text_all(self.text)
        ed0.set_prop(PROP_MICROMAP, False)
        ed0.set_prop(PROP_MINIMAP, False)
        ed0.set_prop(PROP_RULER, False)
        ed0.set_prop(PROP_GUTTER_NUM, False)
        ed0.set_prop(PROP_GUTTER_BM, False)
        ed0.set_prop(PROP_RO, True)
        ed0.set_prop(PROP_LEXER_FILE, 'Diff')

        n=dlg_proc(h, DLG_CTL_ADD, 'button')
        dlg_proc(h, DLG_CTL_PROP_SET, index=n, prop={
            'name': 'btn_close',
            'cap': 'Close',
            'w': 110,
            'a_l': None,
            'a_t': None,
            'a_b': ('', ']'),
            'a_r': ('', ']'),
            'sp_a': 6,
            'on_change': self.callback_btn_close,
            })

        n=dlg_proc(h, DLG_CTL_ADD, 'button')
        dlg_proc(h, DLG_CTL_PROP_SET, index=n, prop={
            'name': 'btn_save',
            'cap': 'Save as...',
            'w': 110,
            'a_l': None,
            'a_t': None,
            'a_b': ('', ']'),
            'a_r': ('btn_close', '['),
            'sp_a': 6,
            'on_change': self.callback_btn_save,
            })

        dlg_proc(h, DLG_CTL_FOCUS, name='ed')
        return h


    def callback_btn_close(self, id_dlg, id_ctl, data='', info=''):

        dlg_proc(self.h_dlg, DLG_HIDE)


    def callback_btn_save(self, id_dlg, id_ctl, data='', info=''):

        res = dlg_file(False, self.filename+'.diff', '', '')
        if not res: return

        with open(res, 'w') as f:
            f.write(self.text)
        msg_status('Saved: '+res)
