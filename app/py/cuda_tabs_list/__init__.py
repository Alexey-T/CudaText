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
    font_size = 9
    column_name = 170
    column_folder = 0
    column_lexer = 80
    show_column_folder = False
    show_column_lexer = False
    drag_start_index = -1
    drag_start_y = 0
    drag_source_handle_self = None
    is_dragging = False
    drag_threshold = 5

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

        if ini_read(fn_config, 'columns', 'width_name', '')=='':
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

        n = dlg_proc(self.h_dlg, DLG_CTL_ADD, prop='editor_edit')
        self.ed_filter = Editor(dlg_proc(self.h_dlg, DLG_CTL_HANDLE, index=n))
        dlg_proc(self.h_dlg, DLG_CTL_PROP_SET, index=n, prop={
            'name': 'filter',
            'align': ALIGN_TOP,
            'texthint': _('Filter'),
            'tab_stop': True,
            'on_change': 'cuda_tabs_list.filter_change',
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
            'a_t':('filter', ']'),
            'a_r':('',']'), #anchor to entire form: l,r,t,b
            'a_b':('',']'),
            'on_select': 'cuda_tabs_list.list_on_sel',
            'on_menu': 'cuda_tabs_list.list_on_menu',
            'on_click': 'cuda_tabs_list.list_on_click',
            'on_click_x': 'cuda_tabs_list.list_on_click_x',
            'on_mouse_down': 'cuda_tabs_list.list_mouse_down',
            'on_mouse_up': 'cuda_tabs_list.list_mouse_up',
            'font_name': self.font_name,
            'font_size': self.font_size,
            'tab_stop': True,
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
        self.listed_editors = []

        ed.set_prop(PROP_TAG, 'tag')
        handles = ed_handles()
        filter_text = self.ed_filter.get_text_line(0)

        hh = list(handles)
        count = hh[-1]-hh[0]+1
        format_len = 1 if count<10 else 2 if count<100 else 3 if count<1000 else 4

        for h in handles:
            edit = Editor(h)
            title = edit.get_prop(PROP_TAB_TITLE)
            if filter_text and not (filter_text.lower() in title.lower()):
                continue
            self.listed_editors.append(edit)

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

            name = prefix + title.lstrip('*').replace('|', '/') # ' | ' happens in file pair
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
        if 0 <= sel < len(self.listed_editors):
            return self.listed_editors[sel]

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
        if self.is_dragging:
            return
        e = self.ed_of_sel()
        if e:
            e.focus()

    def list_mouse_down(self, id_dlg, id_ctl, data='', info=''):
        if self.h_list is None: return
        if self.busy_update: return
        if not isinstance(data, dict):
            return
        button = data.get('btn', -1)
        if button!=0:
            self._reset_drag_state()
            return
        y = data.get('y', -1)
        if y<0:
            self._reset_drag_state()
            return
        item_index = self._index_from_y(y)
        if not (0 <= item_index < len(self.listed_editors)):
            self._reset_drag_state()
            return
        listbox_proc(self.h_list, LISTBOX_SET_SEL, index=item_index)
        self.drag_start_index = item_index
        self.drag_start_y = y
        # Store the unique handle (memory address) that won't change
        self.drag_source_handle_self = self.listed_editors[item_index].get_prop(PROP_HANDLE_SELF)
        self.is_dragging = False

    def list_mouse_up(self, id_dlg, id_ctl, data='', info=''):
        if self.h_list is None:
            self._reset_drag_state()
            return
        if not isinstance(data, dict):
            self._reset_drag_state()
            return
        button = data.get('btn', -1)
        if button!=0 or self.drag_start_index<0:
            self._reset_drag_state()
            return
        y = data.get('y', -1)
        if y<0:
            self._reset_drag_state()
            return
        if abs(y-self.drag_start_y) <= self.drag_threshold:
            if self.drag_source_handle_self:
                Editor(self.drag_source_handle_self).focus()
            self._reset_drag_state()
            return
        self.is_dragging = True
        if self.drag_start_index>=len(self.listed_editors):
            self._reset_drag_state()
            return
        target_index, insert_after = self._target_from_y(y)
        if target_index<0 or target_index>=len(self.listed_editors):
            self._reset_drag_state()
            return
        
        if target_index==self.drag_start_index:
            self._reset_drag_state()
            return
        source_editor = Editor(self.drag_source_handle_self)
        target_editor = self.listed_editors[target_index]
        
        # Temporarily disable updates to prevent cascade
        saved_busy = self.busy_update
        self.busy_update = True
        
        self._reorder_tab(source_editor, target_editor, insert_after)
        
        # Re-enable updates
        self.busy_update = saved_busy
        
        # Update the list to reflect new order
        self.update()
        
        # Focus the moved tab
        if source_editor:
            source_editor.focus()
        
        self._reset_drag_state()

    def _reset_drag_state(self):
        self.drag_start_index = -1
        self.drag_source_handle_self = None
        self.is_dragging = False
        self.drag_start_y = 0

    def _index_from_y(self, y):
        item_h = listbox_proc(self.h_list, LISTBOX_GET_ITEM_H)
        top_index = listbox_proc(self.h_list, LISTBOX_GET_TOP)
        if item_h<=0:
            return -1
        rel_index = y // item_h
        index = top_index + rel_index
        count = listbox_proc(self.h_list, LISTBOX_GET_COUNT)
        if index<0 or index>=count:
            return -1
        return index

    def _target_from_y(self, y):
        item_h = listbox_proc(self.h_list, LISTBOX_GET_ITEM_H)
        top_index = listbox_proc(self.h_list, LISTBOX_GET_TOP)
        count = listbox_proc(self.h_list, LISTBOX_GET_COUNT)
        if item_h<=0 or count<=0:
            return -1, False
        rel_index = y // item_h
        offset_y = y % item_h
        index = top_index + rel_index
        insert_after = offset_y >= (item_h//2)
        if index<0:
            index = 0
            insert_after = False
        if index>=count:
            index = count-1
            insert_after = True
        return index, insert_after

    def _reorder_tab(self, source_editor, target_editor, insert_after):

        if source_editor is None or target_editor is None:
            return
            
        source_group = source_editor.get_prop(PROP_INDEX_GROUP)
        source_tab_idx = source_editor.get_prop(PROP_INDEX_TAB)
        
        target_group = target_editor.get_prop(PROP_INDEX_GROUP)
        target_tab_idx = target_editor.get_prop(PROP_INDEX_TAB)
                
        # Check if moving between different groups
        if source_group != target_group:
            # First, move to target group at position 0
            source_editor.set_prop(PROP_INDEX_GROUP, target_group)
            # Set to position 0 first (it will be at the start of target group)
            source_editor.set_prop(PROP_INDEX_TAB, 0)
            # Now move to the desired position in the target group
            new_position = target_tab_idx
            if insert_after:
                new_position += 1
            source_editor.set_prop(PROP_INDEX_TAB, new_position)
            return
            
        # Same group - calculate the new position
        new_position = target_tab_idx
        if insert_after:
            new_position += 1
            
        
        if source_tab_idx == new_position:
            return
        
        # Just set the source tab to the new position
        # CudaText will automatically shift other tabs
        source_editor.set_prop(PROP_INDEX_TAB, new_position)

    def _focus_handle(self, handle):
        if handle is None:
            return
        try:
            Editor(handle).focus()
        except Exception:
            pass

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

    def filter_change(self, id_dlg, id_ctl, data='', info=''):
        self.update()
