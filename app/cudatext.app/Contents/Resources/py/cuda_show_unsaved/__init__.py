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

        text = '\n'.join(diff)+'\n'
        h = self.init_editor_dlg(fn_base, text)
        dlg_proc(h, DLG_SHOW_MODAL)
        dlg_proc(h, DLG_FREE)


    def init_editor_dlg(self, filename, text):

        h=dlg_proc(0, DLG_CREATE)
        dlg_proc(h, DLG_PROP_SET, prop={
            'cap': 'Unsaved changes: '+filename,
            'w': 900,
            'h': 500,
            'resize': True,
            'keypreview': True,
            })

        n=dlg_proc(h, DLG_CTL_ADD, 'editor')
        dlg_proc(h, DLG_CTL_PROP_SET, index=n, prop={
            'name': 'ed',
            'align': ALIGN_CLIENT,
            'sp_a': 6,
            #'h': 450,
            })

        h_editor = dlg_proc(h, DLG_CTL_HANDLE, index=n)
        ed0 = Editor(h_editor)
        ed0.set_text_all(text)
        ed0.set_prop(PROP_MICROMAP, False)
        ed0.set_prop(PROP_MINIMAP, False)
        ed0.set_prop(PROP_RULER, False)
        ed0.set_prop(PROP_GUTTER_NUM, False)
        ed0.set_prop(PROP_GUTTER_BM, False)
        ed0.set_prop(PROP_RO, True)
        ed0.set_prop(PROP_LEXER_FILE, 'Diff')

        dlg_proc(h, DLG_CTL_FOCUS, index=n)

        return h
