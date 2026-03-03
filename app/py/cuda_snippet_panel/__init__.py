import os
from cudatext import *
import cudatext_cmd as cmds
from cudax_lib import get_translation
from .utils import *
from .snips import *

_   = get_translation(__file__)  # I18N

fn_config = os.path.join(app_path(APP_DIR_SETTINGS), 'plugins.ini')
fn_icon = 'snip.png' # without path, in CudaText folder
dir_clips1 = os.path.join(os.path.dirname(__file__), 'clips')
dir_clips2 = os.path.join(app_path(APP_DIR_DATA), 'clips')
cfg_section = 'snippet_panel'

def bool_to_str(v): return '1' if v else '0'
def str_to_bool(s): return s=='1'


class Command:

    font_name = 'default'
    font_size = 9
    clip_focused = ''

    def __init__(self):

        self.read_ops()
        self.init_dlg()

    def read_ops(self):

        self.font_name = ini_read(fn_config, cfg_section, 'font_name', self.font_name)
        self.font_size = int(ini_read(fn_config, cfg_section, 'font_size', str(self.font_size)))
        self.folder_ = ini_read(fn_config, cfg_section, 'folder', '')

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
            'font_name': self.font_name,
            'font_size': self.font_size,
            'on_click_dbl': self.callback_list_dblclick,
            })
        self.h_list = dlg_proc(h, DLG_CTL_HANDLE, index=n)

        button_proc(self.h_btn, BTN_SET_KIND, BTNKIND_TEXT_CHOICE)
        button_proc(self.h_btn, BTN_SET_ARROW, True)

        listbox_proc(self.h_list, LISTBOX_THEME)


    def get_folders(self):

        self.folders = enum_dir(dir_clips1) + enum_dir(dir_clips2)
        self.folders_ = [os.path.basename(i) for i in self.folders]


    def update_combo(self):

        self.get_folders()
        button_proc(self.h_btn, BTN_SET_ITEMS, '\n'.join(self.folders_))

        if self.folder_ in self.folders_:
            index = self.folders_.index(self.folder_)
        else:
            index = 0

        button_proc(self.h_btn, BTN_SET_ITEMINDEX, index)


    def get_clips_from_file(self, fn):

        r = []
        lines = open_read(fn).splitlines()
        if fn.endswith('.synw-snippet'):
            d = parse_synwrite_snip(lines)
            if d:
                r.append(d)
        else:
            for s in lines:
                d = parse_usual_snip(s)
                r.append(d)
        return r


    def callback_list_dblclick(self, id_dlg, id_ctl, data='', info=''):

        index = listbox_proc(self.h_list, LISTBOX_GET_SEL)
        if 0 <= index < len(self.clips):
            self.insert_clip(index)


    def insert_clip(self, index):

        if ed.get_prop(PROP_RO):
            msg_status(_('Cannot insert to read-only editor'))
            return

        clip = self.clips[index]
        self.clip_focused = clip['name']
        msg_status(_('Inserting: ')+self.clip_focused)
        text = clip['text']

        sel = ed.get_text_sel()
        text = text.replace('${sel}', sel)
        text = text.replace('${eol}', '\n')

        ed.cmd(cmds.cCommand_TextInsert, text)


    def get_folder_clips(self, index):

        self.clips = []
        self.folder = self.folders[index]
        self.folder_ = self.folders_[index]
        ini_write(fn_config, cfg_section, 'folder', self.folder_)

        files = enum_dir(self.folder)
        files = [i for i in files if i.endswith('.txt') or i.endswith('.synw-snippet')]
        if not files: return

        for fn in files:
            self.clips += self.get_clips_from_file(fn)


    def callback_btn_change(self, id_dlg, id_ctl, data='', info=''):

        listbox_proc(self.h_list, LISTBOX_DELETE_ALL)
        listbox_proc(self.h_list, LISTBOX_SET_SEL, index=-1)

        index = button_proc(self.h_btn, BTN_GET_ITEMINDEX)
        if index<0: return

        self.get_folder_clips(index)
        if not self.clips:
            return

        for i in self.clips:
            listbox_proc(self.h_list, LISTBOX_ADD, index=-1, text=i['name'])

        listbox_proc(self.h_list, LISTBOX_SET_SEL, index=0)
        listbox_proc(self.h_list, LISTBOX_SET_TOP, index=0)


    def config(self):
        ini_write(fn_config, cfg_section, 'font_name', self.font_name)
        ini_write(fn_config, cfg_section, 'font_size', str(self.font_size))
        file_open(fn_config)

        lines = [ed.get_text_line(i) for i in range(ed.get_line_count())]
        try:
            index = lines.index('['+cfg_section+']')
            ed.set_caret(0, index)
        except:
            pass


    def menu_dlg(self):

        if ed.get_prop(PROP_RO):
            msg_status(_('Cannot insert to read-only editor'))
            return

        self.get_folders()
        if not self.folders_:
            msg_status(_('No folders with snippets'))
            return

        if self.folder_ in self.folders_:
            focused = self.folders_.index(self.folder_)
        else:
            focused = 0

        index = dlg_menu(DMENU_LIST, self.folders_, focused=focused, caption=_('Snippet Panel')+': '+_('folders'))
        if index is None:
            msg_status(_('Menu cancelled'))
            return
        self.folder_ = self.folders_[index]

        self.get_folder_clips(index)
        if not self.clips:
            msg_status(_('No snippets in selected folder'))
            return

        clip_names = [i['name'] for i in self.clips]

        if self.clip_focused in clip_names:
            focused = clip_names.index(self.clip_focused)
        else:
            focused = 0

        while True:
            index = dlg_menu(DMENU_LIST, clip_names, focused=focused, caption=('%s: %s'% (_('Snippet Panel'), self.folder_)))
            if index is None:
                msg_status(_('Menu cancelled'))
                return
            focused = index
            self.insert_clip(index)
