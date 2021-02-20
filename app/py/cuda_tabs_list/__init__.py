import os
from cudatext import *
import cudatext_cmd

from cudax_lib import get_translation
_   = get_translation(__file__)  # I18N

fn_config = os.path.join(app_path(APP_DIR_SETTINGS), 'cuda_tabs_list.ini')
fn_icon = 'tabs.png'

def bool_to_str(v): return '1' if v else '0'
def str_to_bool(s): return s=='1'

THEME = app_proc(PROC_THEME_UI_DICT_GET, '')

class Command:
    title = 'Tabs'    # No _(), the translation is offered in "translation template.ini".
    h_dlg = None
    h_list = None
    h_menu = None
    busy_update = False
    show_index_group = False
    show_index_tab = False
    show_index_aligned = False
    font_name = 'default'
    font_size = 10
    column_name = 170
    column_folder = 0
    column_lexer = 80
    show_column_folder = False
    show_column_lexer = False

    def __init__(self):
        self.load_ops()

    def load_ops(self):
        self.show_index_group = str_to_bool(ini_read(fn_config, 'op', 'show_index_group', '0'))
        self.show_index_tab = str_to_bool(ini_read(fn_config, 'op', 'show_index_tab', '0'))
        self.show_index_aligned = str_to_bool(ini_read(fn_config, 'op', 'show_index_aligned', '0'))
        self.font_name = ini_read(fn_config, 'op', 'font_name', self.font_name)
        self.font_size = int(ini_read(fn_config, 'op', 'font_size', str(self.font_size)))
        self.column_name = int(ini_read(fn_config, 'columns', 'width_name', str(self.column_name)))
        self.column_folder = int(ini_read(fn_config, 'columns', 'width_folder', str(self.column_folder)))
        self.column_lexer = int(ini_read(fn_config, 'columns', 'width_lexer', str(self.column_lexer)))
        self.show_column_folder = str_to_bool(ini_read(fn_config, 'columns', 'show_folder', bool_to_str(self.show_column_folder)))
        self.show_column_lexer = str_to_bool(ini_read(fn_config, 'columns', 'show_lexer', bool_to_str(self.show_column_lexer)))

    def save_ops(self):
        ini_write(fn_config, 'op', 'show_index_group', bool_to_str(self.show_index_group))
        ini_write(fn_config, 'op', 'show_index_tab', bool_to_str(self.show_index_tab))
        ini_write(fn_config, 'op', 'show_index_aligned', bool_to_str(self.show_index_aligned))
        ini_write(fn_config, 'op', 'font_name', self.font_name)
        ini_write(fn_config, 'op', 'font_size', str(self.font_size))

        ini_write(fn_config, 'columns', '; width_ values: >0 - in pixels, <0 - in percents, =0 - auto-stretched', '')
        ini_write(fn_config, 'columns', '; show_ values: boolean, 0 or 1', '')
        ini_write(fn_config, 'columns', 'width_name', str(self.column_name))
        ini_write(fn_config, 'columns', 'width_folder', str(self.column_folder))
        ini_write(fn_config, 'columns', 'width_lexer', str(self.column_lexer))
        ini_write(fn_config, 'columns', 'show_folder', bool_to_str(self.show_column_folder))
        ini_write(fn_config, 'columns', 'show_lexer', bool_to_str(self.show_column_lexer))

    def open(self):

        if not self.h_dlg:
            self.init_form()
        and_focus = True
        app_proc(PROC_SIDEPANEL_ACTIVATE, (self.title, and_focus))
        self.update()

    def init_form(self):

        self.h_dlg = dlg_proc(0, DLG_CREATE)

        dlg_proc(self.h_dlg, DLG_PROP_SET, prop={
            'keypreview': True,
            'on_key_down': self.form_key_down,
        })

        n = dlg_proc(self.h_dlg, DLG_CTL_ADD, prop='listbox_ex')

        self.h_list = dlg_proc(self.h_dlg, DLG_CTL_HANDLE, index=n)
        listbox_proc(self.h_list, LISTBOX_SET_SHOW_X, index=2)
        listbox_proc(self.h_list, LISTBOX_SET_HOTTRACK, index=1)
        listbox_proc(self.h_list, LISTBOX_SET_COLUMN_SEP, text='|')

        sizes = [self.column_name]
        if self.show_column_folder:
            sizes.append(self.column_folder)
        if self.show_column_lexer:
            sizes.append(self.column_lexer)
        listbox_proc(self.h_list, LISTBOX_SET_COLUMNS, text=sizes)

        dlg_proc(self.h_dlg, DLG_CTL_PROP_SET, index=n, prop={
            'name':'list',
            'a_r':('',']'), #anchor to entire form: l,r,t,b
            'a_b':('',']'),
            'on_select': 'cuda_tabs_list.list_on_sel',
            'on_menu': 'cuda_tabs_list.list_on_menu',
            'on_click': 'cuda_tabs_list.list_on_click',
            'on_click_x': 'cuda_tabs_list.list_on_click_x',
            'font_name': self.font_name,
            'font_size': self.font_size,
            #'font_color': self.get_color_font(),
            #'color': self.get_color_back(),
            } )

        app_proc(PROC_SIDEPANEL_ADD_DIALOG, (self.title, self.h_dlg, fn_icon))

        self.h_menu = menu_proc(0, MENU_CREATE)
        menu_proc(self.h_menu, MENU_CLEAR)
        menu_proc(self.h_menu, MENU_ADD, caption=_('Close'), command='cuda_tabs_list.menu_close_sel')
        menu_proc(self.h_menu, MENU_ADD, caption=_('Close others'), command='cuda_tabs_list.menu_close_others')
        menu_proc(self.h_menu, MENU_ADD, caption='-', command='')
        menu_proc(self.h_menu, MENU_ADD, caption=_('Copy filename only'), command='cuda_tabs_list.menu_copy_file_name')
        menu_proc(self.h_menu, MENU_ADD, caption=_('Copy full filepath'), command='cuda_tabs_list.menu_copy_file_path')

    def on_focus(self, ed_self):
        self.update()

    def on_open(self, ed_self):
        self.update()

    def on_tab_move(self, ed_self):
        self.update()

    def clear_list(self):
        listbox_proc(self.h_list, LISTBOX_DELETE_ALL)

    def update(self):
        if self.h_list is None: return
        self.busy_update = True
        self.clear_list()

        ed.set_prop(PROP_TAG, 'tag')
        handles = ed_handles()

        hh = list(handles)
        count = hh[-1]-hh[0]+1
        format_len = 1 if count<10 else 2 if count<100 else 3 if count<1000 else 4

        for h in handles:
            edit = Editor(h)
            image_index = h-handles[0]

            prefix = ''
            show_g = self.show_index_group
            show_t = self.show_index_tab

            if show_g or show_t:
                n_group = edit.get_prop(PROP_INDEX_GROUP)+1
                if n_group<=6:
                    s_group = str(n_group)
                else:
                    s_group = 'f'+str(n_group-6)
                n_tab = edit.get_prop(PROP_INDEX_TAB)+1
                s_tab = str(n_tab)
                if self.show_index_aligned:
                    if len(s_tab)<format_len:
                        s_tab = ' '*(format_len-len(s_tab))+s_tab

                if show_g and show_t:
                    prefix = '%s:%s. '%(s_group, s_tab)
                elif show_g:
                    prefix = '%s: '%s_group
                elif show_t:
                    prefix = '%s. '%s_tab

            name = prefix + edit.get_prop(PROP_TAB_TITLE).lstrip('*')
            if self.show_column_folder:
                name += '|' + os.path.dirname(edit.get_filename())
            if self.show_column_lexer:
                name += '|' + edit.get_prop(PROP_LEXER_FILE)

            mod = edit.get_prop(PROP_MODIFIED)
            cnt = listbox_proc(self.h_list, LISTBOX_ADD_PROP, index=-1,
                text=name, tag={'modified': mod} )
            if edit.get_prop(PROP_TAG)=='tag':
                listbox_proc(self.h_list, LISTBOX_SET_SEL, index=cnt-1)

        ed.set_prop(PROP_TAG, '')

        self.busy_update = False


    def on_state_ed(self, ed_self, state):
        if state in [EDSTATE_TAB_TITLE, EDSTATE_MODIFIED]:
            self.update()

    def ed_of_sel(self):
        sel = listbox_proc(self.h_list, LISTBOX_GET_SEL)
        if sel<0: return
        h = ed_handles()[sel]
        return Editor(h)

    def menu_close_sel(self):
        e = self.ed_of_sel()
        if e:
            e.cmd(cudatext_cmd.cmd_FileClose)

    def menu_close_others(self):
        e = self.ed_of_sel()
        if e:
            e.cmd(cudatext_cmd.cmd_FileCloseOtherAll)

    def menu_copy_file_path(self):
        e = self.ed_of_sel()
        if e:
            e.cmd(cudatext_cmd.cmd_CopyFilenameFull)

    def menu_copy_file_name(self):
        e = self.ed_of_sel()
        if e:
            e.cmd(cudatext_cmd.cmd_CopyFilenameName)


    def list_on_sel(self, id_dlg, id_ctl, data='', info=''):
        if self.h_list is None: return
        if self.busy_update: return

        e = self.ed_of_sel()
        if e:
            e.focus()

    def list_on_menu(self, id_dlg, id_ctl, data='', info=''):
        if self.h_menu is None: return
        e = self.ed_of_sel()
        if e:
            e.focus()
        menu_proc(self.h_menu, MENU_SHOW, command='')

    def list_on_click_x(self, id_dlg, id_ctl, data='', info=''):
        e = self.ed_of_sel()
        if e:
            e.focus()
        self.menu_close_sel()

    def list_on_click(self, id_dlg, id_ctl, data='', info=''):
        e = self.ed_of_sel()
        if e:
            e.focus()

    def config(self):
        self.save_ops()
        file_open(fn_config)

    def get_color_font(self):
        return THEME['ListFont']['color']

    def get_color_back(self):
        return THEME['ListBg']['color']

    def form_key_down(self, id_dlg, id_ctl, data='', info=''):

        key = id_ctl
        state = data

        #handle Enter and Space
        if (key in [13, 32]) and (state==''):
            self.list_on_click(id_dlg, id_ctl)
            return False
