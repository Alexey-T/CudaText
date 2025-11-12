import os
from cudatext import *
import cudatext_cmd

from cudax_lib import get_translation
_   = get_translation(__file__)  # I18N

fn_config = os.path.join(app_path(APP_DIR_SETTINGS), 'cuda_tabs_list.ini')
fn_icon = 'tabs.png'

def bool_to_str(v): return '1' if v else '0'
def str_to_bool(s): return s=='1'

CAPTION_GROUP = _('Group')
CAPTION_FLOATING = _('Floating')
CH_DIS = chr(1)
CH_GRP = '───'

AUTO_SCROLL_CALLBACK = "module=cuda_tabs_list;cmd=auto_scroll_callback;" # string callback works faster than callable

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

    # --- variables for Drag-and-Drop ---

    # Index of the listbox item where dragging started
    drag_start_index = -1
    # Starting Y-coordinate of the mouse
    drag_start_y = 0
    # Handle to the editor being dragged. We store the handle
    # (a unique ID) because the Editor() object itself might change.
    drag_source_handle_self = None
    # Flag to track if a drag operation is in progress
    is_dragging = False
    # Pixels the mouse must move before a click is considered a drag
    drag_threshold = 5

    # Control index for the listbox (needed for setting cursor)
    n_list = None
    
    # --- Auto-scroll settings ---
    auto_scroll_zone = 30  # Pixels from top/bottom to trigger auto-scroll
    auto_scroll_speed = 1  # Number of items to scroll per timer tick (configurable)
    scroll_timer_interval = 100  # Milliseconds between scroll attempts
    scroll_timer_tag = None  # Timer tag for canceling
    last_mouse_y = -1  # Track last mouse Y position for scrolling

    def __init__(self):
        self.load_ops()

    def load_ops(self):
        self.show_index_group = str_to_bool(ini_read(fn_config, 'op', 'show_index_group', '0'))
        self.show_index_tab = str_to_bool(ini_read(fn_config, 'op', 'show_index_tab', '0'))
        self.show_index_aligned = str_to_bool(ini_read(fn_config, 'op', 'show_index_aligned', '0'))
        self.font_name = ini_read(fn_config, 'op', 'font_name', self.font_name)
        self.font_size = int(ini_read(fn_config, 'op', 'font_size', str(self.font_size)))
        self.auto_scroll_speed = int(ini_read(fn_config, 'op', 'auto_scroll_speed', str(self.auto_scroll_speed)))
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
        ini_write(fn_config, 'op', 'auto_scroll_speed', str(self.auto_scroll_speed))

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
            'font_name': self.font_name,
            'font_size': self.font_size,
        })

        n = dlg_proc(self.h_dlg, DLG_CTL_ADD, prop='listbox_ex')
        self.n_list = n

        self.h_list = dlg_proc(self.h_dlg, DLG_CTL_HANDLE, index=n)
        listbox_proc(self.h_list, LISTBOX_SET_SHOW_X, index=2)
        listbox_proc(self.h_list, LISTBOX_SET_HOTTRACK, index=1)
        listbox_proc(self.h_list, LISTBOX_SET_COLUMN_SEP, text='|')
        listbox_proc(self.h_list, LISTBOX_SET_DISABLING_CHAR, text=CH_DIS)

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
            'cursor': CURSOR_DEFAULT,
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
        """
        rebuild the listbox content to reflect the current state of open files
        """
        if self.h_list is None: return
        self.busy_update = True
        self.clear_list()
        self.listed_editors = [] # This list will now contain Editor objects OR None (for headers)

        ed.set_prop(PROP_TAG, 'tag')
        handles = ed_handles()
        filter_text = self.ed_filter.get_text_line(0)

        hh = list(handles)
        count = hh[-1]-hh[0]+1
        format_len = 1 if count<10 else 2 if count<100 else 3 if count<1000 else 4

        # Group editors by their group index
        groups = {}
        for h in handles:
            edit = Editor(h)
            title = edit.get_prop(PROP_TAB_TITLE)
            if filter_text and not (filter_text.lower() in title.lower()):
                continue

            group_idx = edit.get_prop(PROP_INDEX_GROUP)
            if group_idx not in groups:
                groups[group_idx] = []
            groups[group_idx].append(edit)

        # Sort groups by index (e.g., Group 0, Group 1, ...)
        sorted_groups = sorted(groups.items())
        # Only show headers if there's more than one group
        show_group_headers = len(sorted_groups) > 1

        # Add items to list
        for group_idx, group_editors in sorted_groups:
            # Add group header if there are multiple groups
            if show_group_headers:
                if group_idx < 6:
                    group_name = f"{CAPTION_GROUP} {group_idx + 1}"
                else:
                    # Floating groups start at index 6
                    group_name = f"{CAPTION_FLOATING} {group_idx - 5}"

                # Add group header (non-selectable)
                listbox_proc(self.h_list, LISTBOX_ADD_PROP, index=-1,
                    text=CH_GRP+' '+group_name+' '+CH_GRP+CH_DIS,
                    tag={'is_header': True, 'group': group_idx})
                # Add a placeholder 'None' to our internal list to keep indices aligned
                self.listed_editors.append(None)

            # Sort editors by tab index within this group
            group_editors.sort(key=lambda e: e.get_prop(PROP_INDEX_TAB))

            # Add tabs for this group
            for edit in group_editors:
                # Add the real editor object to our list
                self.listed_editors.append(edit)

                title = edit.get_prop(PROP_TAB_TITLE)
                # image_index = edit.h - handles[0] # this was not used # TODO: add tab icon like in tabs bar

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

                # Reselect the active tab
                if edit.get_prop(PROP_TAG)=='tag':
                    listbox_proc(self.h_list, LISTBOX_SET_SEL, index=cnt-1)

        ed.set_prop(PROP_TAG, '')

        self.busy_update = False


    def on_state_ed(self, ed_self, state):
        if state in [EDSTATE_TAB_TITLE, EDSTATE_MODIFIED]:
            self.update()

    def ed_of_sel(self):
        """
        Gets the Editor object for the current listbox selection.
        Returns None if the selection is a group header.
        """
        sel = listbox_proc(self.h_list, LISTBOX_GET_SEL)
        if 0 <= sel < len(self.listed_editors):
            editor = self.listed_editors[sel]
            # Skip group headers (which are 'None' entries)
            if editor is None:
                return None
            return editor
        return None

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
        """
        Event handler for mouse button down in the listbox.
        Captures the start of a potential drag operation.
        """
        if self.h_list is None: return
        if self.busy_update: return
        if not isinstance(data, dict):
            return

        # Only start drag on left-click
        button = data.get('btn', -1)
        if button != 0:
            self._reset_drag_state()
            return

        y = data.get('y', -1)
        if y < 0:
            self._reset_drag_state()
            return

        item_index = self._index_from_y(y)
        if not (0 <= item_index < len(self.listed_editors)):
            self._reset_drag_state()
            return
        # Skip if clicking on a group header
        if self.listed_editors[item_index] is None:
            self._reset_drag_state()
            return

        # Select the item on mouse down to prepare for drag
        listbox_proc(self.h_list, LISTBOX_SET_SEL, index=item_index)

        # Store drag state
        self.drag_start_index = item_index
        self.drag_start_y = y
        self.last_mouse_y = y
        # Store the unique handle (memory address) that won't change
        self.drag_source_handle_self = self.listed_editors[item_index].get_prop(PROP_HANDLE_SELF)
        self.is_dragging = False # Not officially dragging until threshold is passed

        # Assign on_mouse_move handler to track dragging (we use it inside on_mouse_down for CPU efficiency)
        if self.n_list is not None:
            dlg_proc(self.h_dlg, DLG_CTL_PROP_SET, index=self.n_list, prop={
                'on_mouse_move': 'cuda_tabs_list.list_mouse_move',
            })

    def list_mouse_move(self, id_dlg, id_ctl, data='', info=''):
        """
        Event handler for mouse move in the listbox during potential drag.
        Updates cursor based on whether drag is active and target is valid.
        Also triggers auto-scrolling when near edges.
        """
        if self.h_list is None or self.n_list is None:
            return
        if not isinstance(data, dict):
            return

        # If we haven't started a drag yet, nothing to do
        if self.drag_start_index < 0:
            return

        y = data.get('y', -1)
        if y < 0:
            return

        # Store last mouse Y position for auto-scroll
        self.last_mouse_y = y
        
        # Check if we've moved beyond the drag threshold
        if not self.is_dragging and abs(y - self.drag_start_y) > self.drag_threshold:
            self.is_dragging = True
            # Start auto-scroll timer when drag begins
            self._start_auto_scroll_timer()

        # Update cursor based on drag state
        if self.is_dragging:
            # Determine if current mouse position is over a valid drop target
            target_index = self._target_from_y(y)

            # Check if it's a valid drop target (not a header, valid index)
            if (0 <= target_index < len(self.listed_editors) and
                self.listed_editors[target_index] is not None):
                # Valid drop target - show drag cursor
                cur_cursor = CURSOR_DRAG
            else:
                # Invalid drop target (header or out of bounds) - show no-drop cursor
                cur_cursor = CURSOR_NO_DROP

            dlg_proc(self.h_dlg, DLG_CTL_PROP_SET, index=self.n_list, prop={
                'cursor': cur_cursor
            })

    def list_mouse_up(self, id_dlg, id_ctl, data='', info=''):
        """
        Event handler for mouse button up in the listbox.
        Completes the drag-and-drop operation.
        """
        # Stop auto-scroll timer
        self._stop_auto_scroll_timer()
        
        # Remove on_mouse_move handler to keep CPU usage low
        if self.n_list is not None:
            dlg_proc(self.h_dlg, DLG_CTL_PROP_SET, index=self.n_list, prop={
                'on_mouse_move': '',
                'cursor': CURSOR_DEFAULT,  # Reset cursor to default
            })

        if self.h_list is None:
            self._reset_drag_state()
            return
        if not isinstance(data, dict):
            self._reset_drag_state()
            return

        button = data.get('btn', -1)
        # Check if we were in a valid drag-start state and button is left-click
        if button != 0 or self.drag_start_index < 0:
            self._reset_drag_state()
            return

        y = data.get('y', -1)
        if y < 0:
            self._reset_drag_state()
            return

        # --- Click vs. Drag Check ---
        # If mouse moved less than threshold, treat as a simple click
        if abs(y - self.drag_start_y) <= self.drag_threshold:
            # Check if it was a click (not a failed drag)
            if self.drag_source_handle_self:
                # Manually trigger focus since list_on_click might be blocked
                self._focus_handle(self.drag_source_handle_self)
            self._reset_drag_state()
            return

        # --- It's a Drag Operation ---
        self.is_dragging = True # Flag to prevent list_on_click

        if self.drag_start_index >= len(self.listed_editors):
            self._reset_drag_state()
            return

        # Find the target item under the mouse
        target_index = self._target_from_y(y)
        if target_index < 0 or target_index >= len(self.listed_editors):
            self._reset_drag_state()
            return
        # Skip if target is a group header
        if self.listed_editors[target_index] is None:
            self._reset_drag_state()
            return

        if target_index == self.drag_start_index:
            self._reset_drag_state()
            return

        # Get the actual editor objects
        source_editor = Editor(self.drag_source_handle_self)
        target_editor = self.listed_editors[target_index]

        # Temporarily disable updates to prevent cascade (flicker/re-entry)
        saved_busy = self.busy_update
        self.busy_update = True

        # Perform the reorder
        self._reorder_tab(source_editor, target_editor)

        # Re-enable updates
        self.busy_update = saved_busy

        # Update the list to reflect new order
        self.update()

        # Re-focus the tab that was just moved
        if source_editor:
            source_editor.focus()

        self._reset_drag_state()

    def _reset_drag_state(self):
        """
        Resets all drag-related state variables.
        """
        self.drag_start_index = -1
        self.drag_source_handle_self = None
        self.is_dragging = False
        self.drag_start_y = 0
        self.last_mouse_y = -1

    def _start_auto_scroll_timer(self):
        """
        Starts a timer for auto-scrolling during drag operations.
        """
        if self.scroll_timer_tag is None:
            self.scroll_timer_tag = timer_proc(
                TIMER_START_ONE,
                AUTO_SCROLL_CALLBACK,
                self.scroll_timer_interval,
                tag='auto_scroll'
            )

    def _stop_auto_scroll_timer(self):
        """
        Stops the auto-scroll timer.
        """
        if self.scroll_timer_tag is not None:
            timer_proc(TIMER_STOP, AUTO_SCROLL_CALLBACK, 0, tag='auto_scroll')
            self.scroll_timer_tag = None

    def auto_scroll_callback(self, tag='', info=''):
        """
        Timer callback that performs auto-scrolling when mouse is near edges.
        """
        if not self.is_dragging or self.h_list is None:
            self._stop_auto_scroll_timer()
            return
        
        # Get listbox height
        props = dlg_proc(self.h_dlg, DLG_CTL_PROP_GET, index=self.n_list)
        if not props:
            return
        list_height = props.get('h', 0)
        if list_height <= 0:
            return
        
        y = self.last_mouse_y
        
        # Check if mouse is in the top scroll zone
        if y < self.auto_scroll_zone:
            # Scroll up
            top_index = listbox_proc(self.h_list, LISTBOX_GET_TOP)
            if top_index > 0:
                new_top = max(0, top_index - self.auto_scroll_speed)
                listbox_proc(self.h_list, LISTBOX_SET_TOP, index=new_top)
        
        # Check if mouse is in the bottom scroll zone
        elif y > list_height - self.auto_scroll_zone:
            # Scroll down
            top_index = listbox_proc(self.h_list, LISTBOX_GET_TOP)
            count = listbox_proc(self.h_list, LISTBOX_GET_COUNT)
            item_h = listbox_proc(self.h_list, LISTBOX_GET_ITEM_H)
            
            if item_h > 0:
                visible_items = list_height // item_h
                max_top = max(0, count - visible_items)
                if top_index < max_top:
                    new_top = min(max_top, top_index + self.auto_scroll_speed)
                    listbox_proc(self.h_list, LISTBOX_SET_TOP, index=new_top)
        
        # Restart timer for continuous scrolling
        if self.is_dragging:
            self.scroll_timer_tag = timer_proc(
                TIMER_START_ONE,
                AUTO_SCROLL_CALLBACK,
                self.scroll_timer_interval,
                tag='auto_scroll'
            )

    def _index_from_y(self, y):
        """
        Helper function to get a listbox item index from a Y-coordinate.
        """
        item_h = listbox_proc(self.h_list, LISTBOX_GET_ITEM_H)
        top_index = listbox_proc(self.h_list, LISTBOX_GET_TOP)
        if item_h <= 0:
            return -1
        rel_index = y // item_h
        index = top_index + rel_index
        count = listbox_proc(self.h_list, LISTBOX_GET_COUNT)
        if index < 0 or index >= count:
            return -1
        return index

    def _target_from_y(self, y):
        """
        Helper function to get a listbox item index from a Y-coordinate,
        limiting the value to valid list bounds (for dropping at top/bottom).
        """
        item_h = listbox_proc(self.h_list, LISTBOX_GET_ITEM_H)
        top_index = listbox_proc(self.h_list, LISTBOX_GET_TOP)
        count = listbox_proc(self.h_list, LISTBOX_GET_COUNT)
        if item_h <= 0 or count <= 0:
            return -1
        rel_index = y // item_h
        index = top_index + rel_index

        # limit index to be within the list
        if index < 0:
            index = 0
        if index >= count:
            index = count - 1
        return index

    def _reorder_tab(self, source_editor, target_editor):
        """
        Moves the source_editor to the position of the target_editor,
        handling both in-group and cross-group moves.
        """
        if source_editor is None or target_editor is None:
            return

        source_group = source_editor.get_prop(PROP_INDEX_GROUP)
        source_tab_idx = source_editor.get_prop(PROP_INDEX_TAB)

        target_group = target_editor.get_prop(PROP_INDEX_GROUP)
        target_tab_idx = target_editor.get_prop(PROP_INDEX_TAB)

        # Check if moving between different groups
        if source_group != target_group:
            # 1. Set the editor to the new group.
            source_editor.set_prop(PROP_INDEX_GROUP, target_group)
            # 2. Set the tab index to the target's index. CudaText
            #    will handle shifting other tabs.
            source_editor.set_prop(PROP_INDEX_TAB, target_tab_idx)
            return

        # Same group - just take the target's position
        new_position = target_tab_idx
        if source_tab_idx == new_position:
            # Source and target are same position, nothing to do
            return

        # Just set the source tab to the target position.
        # CudaText will automatically shift other tabs.
        source_editor.set_prop(PROP_INDEX_TAB, new_position)

    def _focus_handle(self, handle):
        """
        Safely focuses an editor given its handle.
        """
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
