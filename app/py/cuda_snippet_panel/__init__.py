import os
from cudatext import *
import cudatext_cmd as cmds
from cudax_lib import get_translation
from .utils import *
from .snips import *

_   = get_translation(__file__)  # I18N

fn_config = 'plugins.ini'
fn_icon = os.path.join(os.path.dirname(__file__), 'snip.png')
dir_clips1 = os.path.join(os.path.dirname(__file__), 'clips')
dir_clips2 = os.path.join(app_path(APP_DIR_DATA), 'clips')

def bool_to_str(v): return '1' if v else '0'
def str_to_bool(s): return s=='1'


class Command:

    def __init__(self):

        self.init_dlg()
        self.folder = ini_read(fn_config, 'snippet_panel', 'folder', '')

    def open_dlg(self):

        self.update_combo()
        self.callback_btn_change(0, 0)

        title = 'Snippet Panel'    # No _(), the translation is offered in "translation template.ini".
        app_proc(PROC_SIDEPANEL_ADD_DIALOG, (title, self.h_dlg, fn_icon) )
        app_proc(PROC_SIDEPANEL_ACTIVATE, title)

    def init_dlg(self):

        h=dlg_proc(0, DLG_CREATE)
        dlg_proc(h, DLG_PROP_SET, prop={
            #'on_key_down': 'cuda_testing_dlg_proc.callback_tempdlg_on_key_down',
            })
        self.h_dlg = h

        n=dlg_proc(h, DLG_CTL_ADD, 'button_ex')
        dlg_proc(h, DLG_CTL_PROP_SET, index=n, prop={
            'name': 'btn',
            'align': ALIGN_TOP,
            'act': True,
            'on_change': self.callback_btn_change,
            })
        self.h_btn = dlg_proc(h, DLG_CTL_HANDLE, index=n)

        n=dlg_proc(h, DLG_CTL_ADD, 'listbox_ex')
        dlg_proc(h, DLG_CTL_PROP_SET, index=n, prop={
            'name': 'list',
            'align': ALIGN_CLIENT,
            'on_click_dbl': self.callback_list_dblclick,
            })
        self.h_list = dlg_proc(h, DLG_CTL_HANDLE, index=n)

        button_proc(self.h_btn, BTN_SET_KIND, BTNKIND_TEXT_CHOICE)
        button_proc(self.h_btn, BTN_SET_ARROW, True)

        listbox_proc(self.h_list, LISTBOX_THEME)



    def update_combo(self):

        self.folders = enum_dir(dir_clips1) + enum_dir(dir_clips2)
        self.folders_ = [os.path.basename(i) for i in self.folders]

        button_proc(self.h_btn, BTN_SET_ITEMS, '\n'.join(self.folders_))

        if self.folder in self.folders_:
            index = self.folders_.index(self.folder)
        else:
            index = 0

        button_proc(self.h_btn, BTN_SET_ITEMINDEX, index)


    def get_clips(self, fn):

        r = []
        lines = open_read(fn).splitlines()
        if fn.endswith('.synw-snippet'):
            d = parse_synwrite_snip(lines)
            if d:
                r.append(d)
        else:
            for s in lines:
                r.append(parse_usual_snip(s))
        return r


    def callback_list_dblclick(self, id_dlg, id_ctl, data='', info=''):

        if ed.get_prop(PROP_RO):
            return

        index = listbox_proc(self.h_list, LISTBOX_GET_SEL)
        if index<0 or index>=len(self.clips):
            return
        clip = self.clips[index]

        msg_status(_('Inserting: ')+clip['name'])
        text = clip['text']

        sel = ed.get_text_sel()
        text = text.replace('${sel}', sel)

        ed.cmd(cmds.cCommand_TextInsert, text)


    def callback_btn_change(self, id_dlg, id_ctl, data='', info=''):

        self.clips = []
        listbox_proc(self.h_list, LISTBOX_DELETE_ALL)
        listbox_proc(self.h_list, LISTBOX_SET_SEL, index=-1)

        index = button_proc(self.h_btn, BTN_GET_ITEMINDEX)
        if index<0: return

        self.folder = self.folders[index]
        self.folder_ = self.folders_[index]
        ini_write(fn_config, 'snippet_panel', 'folder', self.folder_)

        files = enum_dir(self.folder)
        files = [i for i in files if i.endswith('.txt') or i.endswith('.synw-snippet')]
        if not files: return

        for fn in files:
            self.clips += self.get_clips(fn)

        for i in self.clips:
            listbox_proc(self.h_list, LISTBOX_ADD, index=-1, text=i['name'])

        listbox_proc(self.h_list, LISTBOX_SET_SEL, index=0)
