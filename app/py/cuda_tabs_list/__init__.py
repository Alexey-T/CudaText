import os
from cudatext import *
import cudatext_cmd

fn_config = 'cuda_tabs_list.ini'

class Command:
    title = 'Tabs'
    h_dlg = None
    h_tree = None
    busy_update = False
    open_on_start = False

    def __init__(self):
        self.open_on_start = ini_read(fn_config, 'op', 'on_start', '0')=='1'

    def open(self, activate_tab=True):
    
        ed.cmd(cudatext_cmd.cmd_ShowSidePanelAsIs)
        
        self.h_dlg = dlg_proc(0, DLG_CREATE)
        
        n = dlg_proc(self.h_dlg, DLG_CTL_ADD, prop='treeview')
        dlg_proc(self.h_dlg, DLG_CTL_PROP_SET, index=n, prop={'name':'tree', 'a_r':('',']'), 'a_b':('',']')  } )

        self.h_tree = dlg_proc(self.h_dlg, DLG_CTL_HANDLE, index=n)
        tree_proc(self.h_tree, TREE_THEME)
        tree_proc(self.h_tree, TREE_PROP_SHOW_ROOT, 0, 0, '0')
        
        app_proc(PROC_SIDEPANEL_ADD_DIALOG, (self.title, self.h_dlg, 'tabs.png'))
        if activate_tab:
            app_proc(PROC_SIDEPANEL_ACTIVATE, self.title)

        #self.h_menu = 'side:'+self.title
        #menu_proc(self.h_menu, MENU_CLEAR)
        #menu_proc(self.h_menu, MENU_ADD, caption='Close', command='cuda_tabs_list.menu_close_sel')
        #menu_proc(self.h_menu, MENU_ADD, caption='Close others', command='cuda_tabs_list.menu_close_others')
        #menu_proc(self.h_menu, MENU_ADD, caption='-', command='')
        #menu_proc(self.h_menu, MENU_ADD, caption='Copy filename only', command='cuda_tabs_list.menu_copy_file_name')
        #menu_proc(self.h_menu, MENU_ADD, caption='Copy full filepath', command='cuda_tabs_list.menu_copy_file_path')

        self.update()

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
            prefix_mod = '*' if edit.get_prop(PROP_MODIFIED) else ''
            name = prefix_mod + edit.get_prop(PROP_TAB_TITLE)
            h_item = tree_proc(self.h_tree, TREE_ITEM_ADD, 0, -1, name, image_index)
            if edit.get_prop(PROP_TAG)=='tag':
                tree_proc(self.h_tree, TREE_ITEM_SELECT, h_item)
        ed.set_prop(PROP_TAG, '')

        self.busy_update = False

    def on_panel(self, ed_self, id_control, id_event):
        if self.h_tree is None: return
        if self.h_tree!=id_control: return
        if self.busy_update: return

        if id_event=='on_sel':
            e = self.ed_of_sel()
            e.focus()

    def on_state(self, ed_self, state):
        if state in [EDSTATE_TAB_TITLE, EDSTATE_MODIFIED]:
            self.update()

    def on_start(self, ed_self):
        if self.open_on_start:
            self.open(False)

    def ed_of_sel(self):
        h_item = tree_proc(self.h_tree, TREE_ITEM_GET_SELECTED)
        prop = tree_proc(self.h_tree, TREE_ITEM_GET_PROP, h_item)
        if prop is None: return
        index = prop[2] #image_index
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


    def config(self):
        text = '\n'.join([
            'type=check\1pos=6,6,500,0\1cap=Show Tabs panel on application start\1val='+('1' if self.open_on_start else '0'),
            'type=button\1pos=324,70,404,0\1cap=OK\1props=1',
            'type=button\1pos=410,70,490,0\1cap=Cancel',
            ])

        res = dlg_custom('Tabs List options', 500, 100, text, get_dict=True)
        if res is None: return
        if res['clicked'] != 1: return

        self.open_on_start = res[0]=='1'
        ini_write(fn_config, 'op', 'on_start', '1' if self.open_on_start else '0')
