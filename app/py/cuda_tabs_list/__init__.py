import os
from cudatext import *
import cudatext_cmd

fn_config = os.path.join(app_path(APP_DIR_SETTINGS), 'cuda_tabs_list.ini')
fn_icon = 'tabs.png'

def bool_to_str(v): return '1' if v else '0'
def str_to_bool(s): return s=='1'

THEME = app_proc(PROC_THEME_UI_DATA_GET, '')

class Command:
    title = 'Tabs'
    h_dlg = None
    h_tree = None
    h_menu = None
    busy_update = False
    show_index_group = False
    show_index_tab = False
    font_name = 'default'
    font_size = 10

    def __init__(self):
        self.load_ops()

    def load_ops(self):
        self.show_index_group = str_to_bool(ini_read(fn_config, 'op', 'show_index_group', '0'))
        self.show_index_tab = str_to_bool(ini_read(fn_config, 'op', 'show_index_tab', '0'))
        self.font_name = ini_read(fn_config, 'op', 'font_name', self.font_name)
        self.font_size = int(ini_read(fn_config, 'op', 'font_size', str(self.font_size)))

    def save_ops(self):
        ini_write(fn_config, 'op', 'show_index_group', bool_to_str(self.show_index_group))
        ini_write(fn_config, 'op', 'show_index_tab', bool_to_str(self.show_index_tab))
        ini_write(fn_config, 'op', 'font_name', self.font_name)
        ini_write(fn_config, 'op', 'font_size', str(self.font_size))

    def open(self):

        if not self.h_dlg:
            self.init_form()
        app_proc(PROC_SIDEPANEL_ACTIVATE, self.title)
        self.update()

    def init_form(self):

        self.h_dlg = dlg_proc(0, DLG_CREATE)

        n = dlg_proc(self.h_dlg, DLG_CTL_ADD, prop='treeview')

        dlg_proc(self.h_dlg, DLG_CTL_PROP_SET, index=n, prop={
            'name':'tree',
            'a_r':('',']'), #anchor to entire form: l,r,t,b
            'a_b':('',']'),
            'on_select': 'cuda_tabs_list.tree_on_sel',
            'on_menu': 'cuda_tabs_list.tree_on_menu',
            'font_name': self.font_name,
            'font_size': self.font_size,
            #'font_color': self.get_color_font(),
            #'color': self.get_color_back(),
            } )

        self.h_tree = dlg_proc(self.h_dlg, DLG_CTL_HANDLE, index=n)
        tree_proc(self.h_tree, TREE_PROP_SHOW_ROOT, 0, 0, '0')
        tree_proc(self.h_tree, TREE_THEME)

        app_proc(PROC_SIDEPANEL_ADD_DIALOG, (self.title, self.h_dlg, fn_icon))

        self.h_menu = menu_proc(0, MENU_CREATE)
        menu_proc(self.h_menu, MENU_CLEAR)
        menu_proc(self.h_menu, MENU_ADD, caption='Close', command='cuda_tabs_list.menu_close_sel')
        menu_proc(self.h_menu, MENU_ADD, caption='Close others', command='cuda_tabs_list.menu_close_others')
        menu_proc(self.h_menu, MENU_ADD, caption='-', command='')
        menu_proc(self.h_menu, MENU_ADD, caption='Copy filename only', command='cuda_tabs_list.menu_copy_file_name')
        menu_proc(self.h_menu, MENU_ADD, caption='Copy full filepath', command='cuda_tabs_list.menu_copy_file_path')

    def on_focus(self, ed_self):
        self.update()

    def on_open(self, ed_self):
        self.update()

    def on_tab_move(self, ed_self):
        self.update()

    def clear_tree(self):
        tree_proc(self.h_tree, TREE_ITEM_DELETE, 0)

    def update(self):
        if self.h_tree is None: return
        self.busy_update = True
        self.clear_tree()

        ed.set_prop(PROP_TAG, 'tag')
        handles = ed_handles()
        for h in handles:
            edit = Editor(h)
            image_index = h-handles[0]

            #prefix_mod = '*' if edit.get_prop(PROP_MODIFIED) else ''

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
                if show_g and show_t:
                    prefix = '%s:%d. '%(s_group, n_tab)
                elif show_g:
                    prefix = '%s: '%s_group
                elif show_t:
                    prefix = '%d. '%n_tab

            name = prefix+edit.get_prop(PROP_TAB_TITLE)
            h_item = tree_proc(self.h_tree, TREE_ITEM_ADD, 0, -1, name, image_index)
            if edit.get_prop(PROP_TAG)=='tag':
                tree_proc(self.h_tree, TREE_ITEM_SELECT, h_item)
        ed.set_prop(PROP_TAG, '')

        self.busy_update = False


    def on_state(self, ed_self, state):
        if state in [EDSTATE_TAB_TITLE, EDSTATE_MODIFIED]:
            self.update()

    def ed_of_sel(self):
        h_item = tree_proc(self.h_tree, TREE_ITEM_GET_SELECTED)
        prop = tree_proc(self.h_tree, TREE_ITEM_GET_PROPS, h_item)
        if prop is None: return
        index = prop['icon'] #image_index
        h = ed_handles()[index]
        e = Editor(h)
        return e

    def menu_close_sel(self):
        e = self.ed_of_sel()
        e.cmd(cudatext_cmd.cmd_FileClose)

    def menu_close_others(self):
        e = self.ed_of_sel()
        e.cmd(cudatext_cmd.cmd_FileCloseOtherAll)

    def menu_copy_file_path(self):
        e = self.ed_of_sel()
        e.cmd(cudatext_cmd.cmd_CopyFilenameFull)

    def menu_copy_file_name(self):
        e = self.ed_of_sel()
        e.cmd(cudatext_cmd.cmd_CopyFilenameName)


    def tree_on_sel(self, id_dlg, id_ctl, data='', info=''):
        if self.h_tree is None: return
        if self.busy_update: return

        e = self.ed_of_sel()
        e.focus()

    def tree_on_menu(self, id_dlg, id_ctl, data='', info=''):
        if self.h_menu is None: return
        menu_proc(self.h_menu, MENU_SHOW, command='')

    def config(self):
        self.save_ops()
        file_open(fn_config)

    def get_color_font(self):
        for i in THEME:
            if i['name']=='ListFont':
                return i['color']

    def get_color_back(self):
        for i in THEME:
            if i['name']=='ListBg':
                return i['color']
