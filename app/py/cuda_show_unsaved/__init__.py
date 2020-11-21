import os
import difflib
from cudatext import *
from cudax_lib import get_translation

_   = get_translation(__file__)  # I18N

fn_ini = 'plugins.ini'

REPLACE_ENC = {
    'utf8_bom': 'utf-8-sig',
    'utf16le': 'utf-16-le',
    'utf16le_bom': 'utf-16-le',
    'utf16be': 'utf-16-be',
    'utf16be_bom': 'utf-16-be',
    }

class Command:

    def show_dialog(self, caption, text, filename, lexer):

        self.caption = caption
        self.text = text
        self.filename = filename
        self.lexer = lexer

        self.h_dlg = self.init_editor_dlg()

        self.pos_load()
        dlg_proc(self.h_dlg, DLG_SHOW_MODAL)
        self.pos_save()

        dlg_proc(self.h_dlg, DLG_FREE)


    def show_unsaved(self):

        fn = ed.get_filename()
        fn_base = os.path.basename(fn)
        if not fn: return

        enc = ed.get_prop(PROP_ENC, '')
        #convert value to python
        enc2 = REPLACE_ENC.get(enc, None)
        if enc2: enc = enc2

        lines_cur = ed.get_text_all().splitlines()
        lines_orig = open(fn, 'r', encoding=enc).read().splitlines()
        diff = list(difflib.unified_diff(lines_orig, lines_cur,
            fn+' (disk)',
            fn+' (editor)',
            lineterm=''))

        if diff==[]:
            msg_box(_('File is not changed'), MB_OK+MB_ICONINFO)
            return

        self.show_dialog(
            _('Unsaved changes: ')+fn_base,
            '\n'.join(diff)+'\n',
            fn_base,
            'Diff'
            )


    def init_editor_dlg(self):

        h=dlg_proc(0, DLG_CREATE)
        dlg_proc(h, DLG_PROP_SET, prop={
            'cap': self.caption,
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
        ed0.set_prop(PROP_LEXER_FILE, self.lexer)

        #set line states
        is_diff = self.lexer=='Diff'
        for i in range(ed0.get_line_count()):
            state = LINESTATE_NORMAL
            if is_diff:
                s = ed0.get_text_line(i)
                if s.startswith('+') and not s.startswith('+++'):
                    state = LINESTATE_ADDED
                elif s.startswith('-') and not s.startswith('---'):
                    state = LINESTATE_CHANGED
            ed0.set_prop(PROP_LINE_STATE, (i, state))

        n=dlg_proc(h, DLG_CTL_ADD, 'button')
        dlg_proc(h, DLG_CTL_PROP_SET, index=n, prop={
            'name': 'btn_close',
            'cap': _('Close'),
            'w': 120,
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
            'cap': _('Save as...'),
            'w': 120,
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

        #dont use '.diff', it is for any files
        res = dlg_file(False, '', '', '')
        if not res: return

        with open(res, 'w') as f:
            f.write(self.text)
        msg_status(_('Saved: ')+res)


    def pos_load(self):

        x = int(ini_read(fn_ini, 'show_unsaved', 'x', '-1'))
        y = int(ini_read(fn_ini, 'show_unsaved', 'y', '-1'))
        w = int(ini_read(fn_ini, 'show_unsaved', 'w', '-1'))
        h = int(ini_read(fn_ini, 'show_unsaved', 'h', '-1'))
        if x<0: return

        dlg_proc(self.h_dlg, DLG_PROP_SET, prop={'x':x, 'y':y, 'w':w, 'h':h, })


    def pos_save(self):

        prop = dlg_proc(self.h_dlg, DLG_PROP_GET)
        if not prop: return
        x = prop['x']
        y = prop['y']
        w = prop['w']
        h = prop['h']

        ini_write(fn_ini, 'show_unsaved', 'x', str(x))
        ini_write(fn_ini, 'show_unsaved', 'y', str(y))
        ini_write(fn_ini, 'show_unsaved', 'w', str(w))
        ini_write(fn_ini, 'show_unsaved', 'h', str(h))
