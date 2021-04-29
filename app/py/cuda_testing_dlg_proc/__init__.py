import os
from cudatext import *
from random import randint
from .repro_labels import *

def callback_main_close(id_dlg, id_ctl, data='', info=''):
    print('callback_main_close')

    dlg_proc(id_dlg, DLG_HIDE)

    #hide tab added to sidebar
    app_proc(PROC_SIDEPANEL_REMOVE, 'Side dialog')
    app_proc(PROC_SIDEPANEL_ACTIVATE, 'Tree')

def callback_main_menu(id_dlg, id_ctl, data='', info=''):
    print('callback_main_menu')
    nctl = dlg_proc(id_dlg, DLG_CTL_FIND, prop='btn_menu')
    d = dlg_proc(id_dlg, DLG_CTL_PROP_GET, index=nctl)

    nx = d['x']
    ny = d['y']+d['h']
    nx, ny = dlg_proc(id_dlg, DLG_COORD_LOCAL_TO_SCREEN, index=nx, index2=ny)

    h_menu = menu_proc(0, MENU_CREATE)
    menu_proc(h_menu, MENU_ADD, command=2700, caption='About1')
    menu_proc(h_menu, MENU_ADD, command=2700, caption='About2')
    menu_proc(h_menu, MENU_ADD, command=2700, caption='About3')
    menu_proc(h_menu, MENU_SHOW, command='%d,%d'%(nx, ny))


class Command:
    border = True
    btn_overlay = 0

    def run_modal(self):
        print('run_modal begin')
        h = self.init_maindlg()
        dlg_proc(h, DLG_SHOW_MODAL)
        self.show_form_prop(h)
        dlg_proc(h, DLG_FREE)
        print('run_modal end')

    def run_nonmodal(self):
        h = self.init_maindlg()
        dlg_proc(h, DLG_SHOW_NONMODAL)

    def run_dlgcustom(self):
        dlg_custom('TestDlg', 200, 100, 'type=label\1pos=6,6,200,0\1cap=Test')

    def callback_linklabel_click(self, id_dlg, id_ctl, data='', info=''):
        msg_box('Linklabel clicked', MB_OK)

    def callback_buttondlg(self, id_dlg, id_ctl, data='', info=''):
        h = id_dlg
        n_btn_border = dlg_proc(h, DLG_CTL_FIND, prop='btn_border')
        n_btn_icon = dlg_proc(h, DLG_CTL_FIND, prop='btn_icon')

        if id_ctl==n_btn_border:
            self.border = not self.border
            val = DBORDER_DIALOG if self.border else DBORDER_NONE
            dlg_proc(h, DLG_PROP_SET, prop={'border': val} )

        if id_ctl==n_btn_icon:
            id_btn = dlg_proc(h, DLG_CTL_HANDLE, name='btn_icon')

            #toggle button's checked
            b = button_proc(id_btn, BTN_GET_CHECKED)
            button_proc(id_btn, BTN_SET_CHECKED, not b)
            button_proc(id_btn, BTN_SET_BOLD, not b)

            #set imagelist+icon for button
            id_imglist = app_proc(PROC_GET_TAB_IMAGELIST, '')
            button_proc(id_btn, BTN_SET_KIND, BTNKIND_TEXT_ICON_VERT)
            button_proc(id_btn, BTN_SET_IMAGELIST, id_imglist)
            button_proc(id_btn, BTN_SET_IMAGEINDEX, int(b))

            #set overlay text
            self.btn_overlay+=1
            button_proc(id_btn, BTN_SET_OVERLAY, self.btn_overlay)

            #set colored lines
            color1 = 0x00ff00
            color2 = 0x0000ff
            button_proc(id_btn, BTN_SET_COLOR_LINE, color1)
            button_proc(id_btn, BTN_SET_COLOR_LINE2, color2)


    def callback_splitter_left(self, id_dlg, id_ctl, data='', info=''):
        h = id_dlg
        print('callback_splitter_left')

    def callback_combo_change(self, id_dlg, id_ctl, data='', info=''):

        p = dlg_proc(id_dlg, DLG_CTL_PROP_GET, name='c1')
        text = p['val']
        dlg_proc(id_dlg, DLG_PROP_SET, prop={'cap': 'text: '+text})

    def callback_combo_change2(self, id_dlg, id_ctl, data='', info=''):

        p = dlg_proc(id_dlg, DLG_CTL_PROP_GET, name='c2')
        index = p['val']
        dlg_proc(id_dlg, DLG_PROP_SET, prop={'cap': 'index: '+str(index)})

    def callback_maindlg(self, id_dlg, id_ctl, data='', info=''):
        print('callback_maindlg(info=%s)' % repr(info))
        h = id_dlg

        n_chk_panel = dlg_proc(h, DLG_CTL_FIND, prop='chk_panel')
        n_edit = dlg_proc(h, DLG_CTL_FIND, prop='edit1')
        n_btn_cap = dlg_proc(h, DLG_CTL_FIND, prop='btn_caption')
        n_btn_dlg = dlg_proc(h, DLG_CTL_FIND, prop='btn_dlg')
        n_btn_paint = dlg_proc(h, DLG_CTL_FIND, prop='btn_paint')
        n_color = dlg_proc(h, DLG_CTL_FIND, prop='color')
        n_chk_dock = dlg_proc(h, DLG_CTL_FIND, prop='chk_dock')

        if id_ctl==n_chk_panel:
            d = dlg_proc(h, DLG_CTL_PROP_GET, index=n_color)
            dlg_proc(h, DLG_CTL_PROP_SET, index=n_color, prop={'vis': not d['vis']} )

        if id_ctl==n_btn_cap:
            d = dlg_proc(h, DLG_CTL_PROP_GET, index=n_edit)
            dlg_proc(h, DLG_PROP_SET, prop={'cap': 'entered: '+d['val'] } )

        if id_ctl==n_btn_dlg:
            hh = self.init_tempdlg()
            docked = dlg_proc(h, DLG_CTL_PROP_GET, index=n_chk_dock)['val'] == '1'
            if docked:
                dlg_proc(hh, DLG_DOCK, prop='R', index=h)
                dlg_proc(hh, DLG_SHOW_NONMODAL)
            else:
                dlg_proc(hh, DLG_SHOW_MODAL)


    def callback_listbox_click_x(self, id_dlg, id_ctl, data='', info=''):

        index_sel = listbox_proc(self.id_listbox, LISTBOX_GET_SEL)
        print('listbox: x clicked for item %d'%index_sel)

    def callback_listbox_click_header(self, id_dlg, id_ctl, data='', info=''):

        print('listbox: header clicked for column %d'%data)

    def callback_listbox_drawitem(self, id_dlg, id_ctl, data='', info=''):

        #print('listbox on_draw_item, data:', data)
        id_canvas = data['canvas']
        index = data['index']
        rect = data['rect']
        index_sel = listbox_proc(self.id_listbox, LISTBOX_GET_SEL)

        show_x = listbox_proc(self.id_listbox, LISTBOX_GET_SHOW_X)>0
        inc_x = 14 if show_x else 0

        #set bold for each 5th
        if index%5==0:
            style = FONT_B + FONT_I
            color = 0xB00000
        else:
            style = 0
            color = 0
        canvas_proc(id_canvas, CANVAS_SET_FONT, text='default', color=color, style=style)

        if index==index_sel:
            back_color = 0x806000+(0xF<<index)
            if back_color>0xFFFF00:
                back_color = 0xFFFF00
            canvas_proc(id_canvas, CANVAS_SET_BRUSH, color=back_color, style=BRUSH_SOLID)
            canvas_proc(id_canvas, CANVAS_SET_PEN, color=0x00F0F0, size=1, style=PEN_STYLE_SOLID)
            canvas_proc(id_canvas, CANVAS_RECT, x=rect[0], y=rect[1], x2=rect[2], y2=rect[3])
        else:
            canvas_proc(id_canvas, CANVAS_SET_BRUSH, color=0x6060D0, style=BRUSH_SOLID)
            canvas_proc(id_canvas, CANVAS_RECT_FILL, x=rect[0], y=rect[1], x2=rect[2], y2=rect[3])

        canvas_proc(id_canvas, CANVAS_TEXT,
            text='item index %d'%index,
            x=rect[0] + 20 + index*4 + inc_x,
            y=rect[1] + 2 )

        #this imagelist has 2 test icons: 0, 1
        img_list = app_proc(PROC_GET_TAB_IMAGELIST, '')
        imagelist_proc(img_list, IMAGELIST_PAINT, (id_canvas, rect[0]+inc_x, rect[1], index%2))


    def callback_listbox_check(self, id_dlg, id_ctl, data='', info=''):

        print('listbox option "owner" click')
        prop = dlg_proc(id_dlg, DLG_CTL_PROP_GET, name='chk_owner')
        chk = prop['val'] == '1'

        listbox_proc(self.id_listbox, LISTBOX_SET_DRAWN, index=(1 if chk else 0))

    def callback_listbox_check_x(self, id_dlg, id_ctl, data='', info=''):

        val = listbox_proc(self.id_listbox, LISTBOX_GET_SHOW_X)
        val = (val+1)%3 # possible values: 0..2
        listbox_proc(self.id_listbox, LISTBOX_SET_SHOW_X, index=val)
        listbox_proc(self.id_listbox, LISTBOX_SET_HOTTRACK, index=(1 if val>0 else 0))


    def callback_listbox_columns(self, id_dlg, id_ctl, data='', info=''):

        print('listbox columns click')

        listbox_proc(self.id_listbox, LISTBOX_SET_COLUMN_SEP, text='|')
        cols = listbox_proc(self.id_listbox, LISTBOX_GET_COLUMNS)
        print('listbox columns:', cols)
        if not cols:
            listbox_proc(self.id_listbox, LISTBOX_SET_COLUMNS, text=[-50,30,0])
        else:
            listbox_proc(self.id_listbox, LISTBOX_SET_COLUMNS, text=[])


    def callback_editor_on_change(self, id_dlg, id_ctl, data='', info=''):

        print('editor on_change')

    def callback_editor_on_caret(self, id_dlg, id_ctl, data='', info=''):

        print('editor on_caret')

    def callback_editor_on_scroll(self, id_dlg, id_ctl, data='', info=''):

        print('editor on_scroll')

    def callback_editor_on_paste(self, id_dlg, id_ctl, data='', info=''):

        print('editor on_paste', data)

    def callback_editor_on_menu(self, id_dlg, id_ctl, data='', info=''):

        msg_box('Event handler on_menu blocked std context menu', MB_OK)
        return False

    def callback_editor_on_click_gutter(self, id_dlg, id_ctl, data='', info=''):

        print('editor on_click_gutter', data)

    def callback_editor_on_click_gap(self, id_dlg, id_ctl, data='', info=''):

        print('editor on_click_gap', data)

    def callback_editor_on_click_link(self, id_dlg, id_ctl, data='', info=''):

        print('editor on_click_link', data)

    def callback_editor_on_key_down(self, id_dlg, id_ctl, data='', info=''):

        print('editor on_key_down', data)

    def callback_editor_on_key_up(self, id_dlg, id_ctl, data='', info=''):

        print('editor on_key_up', data)


    def do_paint_mark(self, id_dlg, id_ctl):
        print('do_paint_mark')

        #paint circle of randon color
        n = randint(0, 0xfffff)
        canvas_id = dlg_proc(id_dlg, DLG_CTL_HANDLE, name='paint')
        canvas_proc(canvas_id, CANVAS_SET_PEN, color=0xA0)
        canvas_proc(canvas_id, CANVAS_SET_BRUSH, color=n+0xA0FF00)
        canvas_proc(canvas_id, CANVAS_RECT, x=0, y=0, x2=50, y2=50)
        canvas_proc(canvas_id, CANVAS_SET_BRUSH, color=n+0xA0A0)
        canvas_proc(canvas_id, CANVAS_ELLIPSE, x=0, y=0, x2=50, y2=50)

        #paint 2 first icons of standard file-tabs imagelist
        il = app_proc(PROC_GET_TAB_IMAGELIST, '')
        imagelist_proc(il, IMAGELIST_PAINT, (canvas_id, 0, 0, 0))
        imagelist_proc(il, IMAGELIST_PAINT, (canvas_id, 16, 16, 1))

    def callback_maindlg_paint_click(self, id_dlg, id_ctl, data='', info=''):
        self.do_paint_mark(id_dlg, id_ctl)

    def callback_maindlg_pos_save(self, id_dlg, id_ctl, data='', info=''):
        self.pos_str = dlg_proc(id_dlg, DLG_POS_GET_STR)

    def callback_maindlg_pos_load(self, id_dlg, id_ctl, data='', info=''):
        if hasattr(self, 'pos_str'):
            dlg_proc(id_dlg, DLG_POS_SET_FROM_STR, self.pos_str)

    def callback_maindlg_setprops(self, id_dlg, id_ctl, data='', info=''):
        res = dlg_input_ex(2,
            'Set sidebar icon props',
            'ImageIndex:',
            '0',
            'Hint:',
            'test_panel')
        if res is None: return
        index = str(res[0])
        title = res[1]
        print('set sidebar props:', index, title)
        app_proc(PROC_SIDEPANEL_SET_PROP, ('Side dialog', index, title))

    def callback_tempdlg_on_key_down(self, id_dlg, id_ctl, data='', info=''):
        print('callback_tempdlg_on_key_down')

        state = data
            #was needed before Cud 1.55.2:
            #state = app_proc(PROC_GET_KEYSTATE, '')
        str_key =\
            ('Meta+' if 'm' in state else '')+\
            ('Ctrl+' if 'c' in state else '')+\
            ('Alt+' if 'a' in state else '')+\
            ('Shift+' if 's' in state else '')+\
            app_proc(PROC_HOTKEY_INT_TO_STR, str(id_ctl))

        n_info = dlg_proc(id_dlg, DLG_CTL_FIND, 'label_info')
        dlg_proc(id_dlg, DLG_CTL_PROP_SET, index=n_info, prop={'cap': 'keypress: '+str_key })
        return True

    def callback_tempdlg_on_close_query(self, id_dlg, id_ctl, data='', info=''):
        print('callback_tempdlg_on_close_query')
        n_canclose = dlg_proc(id_dlg, DLG_CTL_FIND, 'chk_canclose')
        d = dlg_proc(id_dlg, DLG_CTL_PROP_GET, index=n_canclose)
        return d['val']=='1'

    def callback_tempdlg(self, id_dlg, id_ctl, data='', info=''):
        print('callback_tempdlg')

        n_close = dlg_proc(id_dlg, DLG_CTL_FIND, 'btn_close')
        n_clone = dlg_proc(id_dlg, DLG_CTL_FIND, 'btn_clonedlg')

        if id_ctl==n_close:
            dlg_proc(id_dlg, DLG_HIDE)

        if id_ctl==n_clone:
            print('  tempdlg begin')
            d = dlg_proc(id_dlg, DLG_PROP_GET)
            hh = self.init_tempdlg(d['x']+20, d['y']+20)
            dlg_proc(hh, DLG_SHOW_MODAL)
            dlg_proc(hh, DLG_FREE)
            print('  tempdlg end')


    def callback_statusbar_click(self, id_dlg, id_ctl, data='', info=''):
        print('callback_statusbar_click: id_dlg={}; id_ctl={}; data={}; info={};'.format(
            id_dlg, id_ctl, data, info))

    def callback_toolbar_menu(self, id_dlg, id_ctl, data='', info=''):
        print('callback_toolbar_menu')
        id_bar = dlg_proc(id_dlg, DLG_CTL_HANDLE, name='tb')
        cnt = toolbar_proc(id_bar, TOOLBAR_GET_COUNT)
        index = toolbar_proc(id_bar, TOOLBAR_GET_INDEX_HOVERED)
        print('buttons count: %d, index hovered: %d'%(cnt, index))

    def init_buttondlg(self):
        h=dlg_proc(0, DLG_CREATE)
        dlg_proc(h, DLG_PROP_SET, prop={
            'cap': 'button test',
            'w': 400,
            'h': 300,
            'w_min': 200,
            'h_min': 250
            })

        n=dlg_proc(h, DLG_CTL_ADD, 'button_ex')
        dlg_proc(h, DLG_CTL_PROP_SET, index=n, prop={
            'name': 'btn_border',
            'cap':'toggle window border',
            'x': 10,
            'y': 20,
            'w': 200,
            'on_change': 'cuda_testing_dlg_proc.callback_buttondlg'
            })

        n=dlg_proc(h, DLG_CTL_ADD, 'button_ex')
        dlg_proc(h, DLG_CTL_PROP_SET, index=n, prop={
            'name': 'btn_icon',
            'cap':'toggle bold/icon/overlay',
            'x': 10,
            'y': 50,
            'w': 200,
            'h': 50,
            'on_change': 'cuda_testing_dlg_proc.callback_buttondlg'
            })

        return h

    def init_splitterdlg(self):
        h=dlg_proc(0, DLG_CREATE)
        dlg_proc(h, DLG_PROP_SET, prop={
            'cap': 'splitter test',
            'w': 650,
            'h': 400,
            'border': DBORDER_SIZE,
            })

        n=dlg_proc(h, DLG_CTL_ADD, 'panel')
        dlg_proc(h, DLG_CTL_PROP_SET, index=n, prop={
            'name': 'panel_L',
            'cap': 'panel_L',
            'x': 0,
            'y': 0,
            'w': 200,
            'w_min': 150,
            'align': ALIGN_LEFT,
            'color': 0x60c060,
            })

        n=dlg_proc(h, DLG_CTL_ADD, 'panel')
        dlg_proc(h, DLG_CTL_PROP_SET, index=n, prop={
            'name': 'panel_R',
            'cap': 'panel_R',
            'x': 400,
            'y': 0,
            'w': 200,
            'align': ALIGN_RIGHT,
            'color': 0xd08060
            })

        n=dlg_proc(h, DLG_CTL_ADD, 'panel')
        dlg_proc(h, DLG_CTL_PROP_SET, index=n, prop={
            'name': 'panel_B',
            'cap': 'panel_B',
            'x': 0,
            'y': 0,
            'h': 100,
            'align': ALIGN_BOTTOM,
            'color': 0xa0e080
            })

        n=dlg_proc(h, DLG_CTL_ADD, 'panel')
        dlg_proc(h, DLG_CTL_PROP_SET, index=n, prop={
            'name': 'panel_LL',
            'cap': 'LL',
            'x': 150,
            'y': 100,
            'w': 60,
            'h': 60,
            'p': 'panel_L',
            'a_l': None,
            'a_t': ('','-'),
            'a_r': ('',']'),
            'color': 0x8080b0
            })

        n=dlg_proc(h, DLG_CTL_ADD, 'splitter')
        dlg_proc(h, DLG_CTL_PROP_SET, index=n, prop={
            'name': 'sp_L',
            'x': 210,
            'y': 0,
            'align': ALIGN_LEFT,
            'ex0': True,
            'ex1': True,
            'act': True,
            'color': 0xff,
            'on_change': self.callback_splitter_left,
            })

        n=dlg_proc(h, DLG_CTL_ADD, 'splitter')
        dlg_proc(h, DLG_CTL_PROP_SET, index=n, prop={
            'name': 'sp_R',
            'x': 220,
            'y': 0,
            'align': ALIGN_RIGHT,
            'ex0': True,
            'ex1': True,
            'color': 0xff,
            })

        n=dlg_proc(h, DLG_CTL_ADD, 'splitter')
        dlg_proc(h, DLG_CTL_PROP_SET, index=n, prop={
            'name': 'sp_B',
            'x': 0,
            'y': 0,
            'align': ALIGN_BOTTOM,
            'ex0': True,
            'ex1': True,
            })

        return h

    def init_maindlg(self):
        h=dlg_proc(0, DLG_CREATE)
        dlg_proc(h, DLG_PROP_SET, prop={
            'cap': 'main dlg',
            'x': 100,
            'y': 50,
            'w': 400,
            'h': 380,
            'w_min': 200,
            'h_min': 300,
            'border': DBORDER_SIZE,
            'topmost': True,
            })

        n=dlg_proc(h, DLG_CTL_ADD, 'label')
        dlg_proc(h, DLG_CTL_PROP_SET, index=n, prop={
            'name': 'label0',
            'cap': 'label',
            'x': 10,
            'y': 10,
            'w': 50,
            'tag': 'some_tag',
            })

        n=dlg_proc(h, DLG_CTL_ADD, 'check')
        dlg_proc(h, DLG_CTL_PROP_SET, index=n, prop={
            'name': 'chk_panel',
            'cap': 'show panel',
            'val': True,
            'x': 60,
            'y': 8,
            'w': 200,
            'act': True,
            'on_change': 'cuda_testing_dlg_proc.callback_maindlg'
            })

        n=dlg_proc(h, DLG_CTL_ADD, 'edit')
        dlg_proc(h, DLG_CTL_PROP_SET, index=n, prop={
            'name': 'edit1',
            'val':'edit1',
            'x': 10,
            'y': 30,
            'w': 200,
            })

        n=dlg_proc(h, DLG_CTL_ADD, 'button')
        dlg_proc(h, DLG_CTL_PROP_SET, index=n, prop={
            'name': 'btn_caption',
            'cap': 'upd caption',
            'x': 10,
            'y': 60,
            'w': 100,
            'on_change': 'cuda_testing_dlg_proc.callback_maindlg'
            })

        n=dlg_proc(h, DLG_CTL_ADD, 'button')
        dlg_proc(h, DLG_CTL_PROP_SET, index=n, prop={
            'name': 'btn_move',
            'cap': 'move button',
            'x': 120,
            'y': 60,
            'w': 100,
            'on_change': 'cuda_testing_dlg_proc.callback_main_movebtn'
            })

        n=dlg_proc(h, DLG_CTL_ADD, 'colorpanel')
        dlg_proc(h, DLG_CTL_PROP_SET, index=n, prop={
            'name': 'color',
            'cap': ' ',
            'x': 380,
            'y': 5,
            'w': 15,
            'h': 290,
            'ex0': 1,
            'ex1': 0xc0f0f0,
            'on_mouse_enter': lambda id_dlg, id_ctl, data='', info='': print('panel on_mouse_enter'),
            'on_mouse_exit': lambda id_dlg, id_ctl, data='', info='': print('panel on_mouse_exit'),
            'on_mouse_down': lambda id_dlg, id_ctl, data='', info='': print('panel on_mouse_down', data),
            'on_mouse_up': lambda id_dlg, id_ctl, data='', info='': print('panel on_mouse_up', data),
            })
        #anchors of colorpanel
        dlg_proc(h, DLG_CTL_PROP_SET, index=n, prop={
            'a_l': None,
            'a_r': ('', ']'),
            'a_b': ('', ']'),
            'sp_a': 6,
            })

        n=dlg_proc(h, DLG_CTL_ADD, 'button')
        dlg_proc(h, DLG_CTL_PROP_SET, index=n, prop={
            'name': 'btn_x_panel',
            'cap': '?',
            'x': 0,
            'y': 0,
            'w': 15,
            'h': 15,
            'p': 'color',
            'a_t': ('color', '-'),
            'on_change': 'cuda_testing_dlg_proc.callback_maindlg'
            })

        n=dlg_proc(h, DLG_CTL_ADD, 'button')
        dlg_proc(h, DLG_CTL_PROP_SET, index=n, prop={
            'name': 'btn_dlg',
            'cap': 'temp dlg',
            'x': 10,
            'y': 200,
            'w': 100,
            'on_change': 'cuda_testing_dlg_proc.callback_maindlg'
            })

        n=dlg_proc(h, DLG_CTL_ADD, 'button')
        dlg_proc(h, DLG_CTL_PROP_SET, index=n, prop={
            'name': 'btn_menu',
            'cap': 'menu here',
            'x': 10,
            'y': 230,
            'w': 100,
            'on_change': callback_main_menu
            })

        n=dlg_proc(h, DLG_CTL_ADD, 'button')
        dlg_proc(h, DLG_CTL_PROP_SET, index=n, prop={
            'name': 'btn_paint',
            'cap': 'paint here',
            'x': 10,
            'y': 260,
            'w': 100,
            'on_change': self.callback_maindlg_paint_click
            })

        n=dlg_proc(h, DLG_CTL_ADD, 'button')
        dlg_proc(h, DLG_CTL_PROP_SET, index=n, prop={
            'name': 'btn_pos_save',
            'cap': 'pos: to str',
            'x': 10,
            'y': 290,
            'w': 100,
            'on_change': self.callback_maindlg_pos_save
            })

        n=dlg_proc(h, DLG_CTL_ADD, 'button')
        dlg_proc(h, DLG_CTL_PROP_SET, index=n, prop={
            'name': 'btn_pos_load',
            'cap': 'pos: from str',
            'x': 10,
            'y': 320,
            'w': 100,
            'on_change': self.callback_maindlg_pos_load
            })

        n=dlg_proc(h, DLG_CTL_ADD, 'button')
        dlg_proc(h, DLG_CTL_PROP_SET, index=n, prop={
            'name': 'btn_setprops',
            'cap': 'sidebar icon...',
            'x': 10,
            'y': 350,
            'w': 100,
            'on_change': self.callback_maindlg_setprops
            })

        n=dlg_proc(h, DLG_CTL_ADD, 'button')
        dlg_proc(h, DLG_CTL_PROP_SET, index=n, prop={
            'name': 'btn_callbk',
            'cap': 'complex callback',
            'x': 120,
            'y': 200,
            'w': 120,
            'on_change': 'module=cuda_testing_dlg_proc.testcall;func=callback_main_complex;info=1234;'
            })

        n=dlg_proc(h, DLG_CTL_ADD, 'button')
        dlg_proc(h, DLG_CTL_PROP_SET, index=n, prop={
            'name': 'btn_ok',
            'cap': 'close',
            'x': 120,
            'y': 230,
            'w': 120,
            'on_change': callback_main_close
            })

        n=dlg_proc(h, DLG_CTL_ADD, 'linklabel')
        dlg_proc(h, DLG_CTL_PROP_SET, index=n, prop={
            'name': 'link1',
            'cap': 'linklabel',
            'x': 10,
            'y': 150,
            'w': 120,
            'on_click': self.callback_linklabel_click
            })

        n=dlg_proc(h, DLG_CTL_ADD, 'check')
        dlg_proc(h, DLG_CTL_PROP_SET, index=n, prop={
            'name': 'chk_dock',
            'cap': 'temp dlg: docked',
            'x': 10,
            'y': 170,
            'w': 120
            })

        #test for live callback
        n=dlg_proc(h, DLG_CTL_ADD, 'paintbox')
        dlg_proc(h, DLG_CTL_PROP_SET, index=n, prop={
            'name': 'paint',
            'x': 250,
            'y': 200,
            'w': 60,
            'h': 60,
            'on_click': self.callback_maindlg_paint_click
            })

        nfocus = dlg_proc(h, DLG_CTL_FIND, 'edit1')
        dlg_proc(h, DLG_CTL_FOCUS, index=nfocus)
        return h


    def show_form_prop(self, h):
        res = dlg_proc(h, DLG_PROP_GET)
        print('form prop:', res)

        cnt = dlg_proc(h, DLG_CTL_COUNT)
        for n in range(cnt):
            res = dlg_proc(h, DLG_CTL_PROP_GET, index=n)
            print('c%d:'%n, res)


    def init_listdlg(self):

        h=dlg_proc(0, DLG_CREATE)
        dlg_proc(h, DLG_PROP_SET, prop={
            'cap': 'listbox test',
            'w': 420,
            'h': 300
            })

        n=dlg_proc(h, DLG_CTL_ADD, 'listbox_ex')
        dlg_proc(h, DLG_CTL_PROP_SET, index=n, prop={
            'name': 'list1',
            'x': 10,
            'y': 10,
            'w': 400,
            'h': 200,
            'on_click_x': self.callback_listbox_click_x,
            'on_click_header': self.callback_listbox_click_header,
            'on_draw_item': self.callback_listbox_drawitem,
            })

        h_list = dlg_proc(h, DLG_CTL_HANDLE, index=n)
        self.id_listbox = h_list

        for i in range(40):
            listbox_proc(h_list, LISTBOX_ADD, index=-1, text='first-%d|2-%d|3-%d'%(i,i,i))
        listbox_proc(h_list, LISTBOX_SET_SEL, index=2)
        listbox_proc(h_list, LISTBOX_SET_ITEM_H, index=28)
        listbox_proc(h_list, LISTBOX_SET_DRAWN, index=1)
        listbox_proc(h_list, LISTBOX_SET_HEADER, text='header0|header1|header2')

        n=dlg_proc(h, DLG_CTL_ADD, 'check')
        dlg_proc(h, DLG_CTL_PROP_SET, index=n, prop={
            'name': 'chk_owner',
            'cap': 'Owner-drawn listbox',
            'x': 10,
            'y': 220,
            'w': 200,
            'val': True,
            'act': True,
            'on_change': self.callback_listbox_check,
            })

        n=dlg_proc(h, DLG_CTL_ADD, 'button')
        dlg_proc(h, DLG_CTL_PROP_SET, index=n, prop={
            'name': 'btn_columns',
            'cap': 'Make columns',
            'x': 10,
            'y': 250,
            'w': 120,
            'on_change': self.callback_listbox_columns,
            })

        n=dlg_proc(h, DLG_CTL_ADD, 'button')
        dlg_proc(h, DLG_CTL_PROP_SET, index=n, prop={
            'name': 'btn_x',
            'cap': 'Toggle x marks',
            'x': 130,
            'y': 250,
            'w': 140,
            'on_change': self.callback_listbox_check_x,
            })

        return h


    def init_combo_dlg(self):

        h=dlg_proc(0, DLG_CREATE)
        dlg_proc(h, DLG_PROP_SET, prop={
            'cap': 'combo test',
            'w': 420,
            'h': 300
            })

        n=dlg_proc(h, DLG_CTL_ADD, 'combo')
        dlg_proc(h, DLG_CTL_PROP_SET, index=n, prop={
            'name': 'c1',
            'x': 10,
            'y': 10,
            'w': 400,
            'items': '\t'.join(['Aaa', 'Bbb', 'Ccc', 'Ddd']),
            'val': 'init text',
            'act': True, # for on_change
            'on_change': self.callback_combo_change,
            })

        n=dlg_proc(h, DLG_CTL_ADD, 'combo_ro')
        dlg_proc(h, DLG_CTL_PROP_SET, index=n, prop={
            'name': 'c2',
            'x': 10,
            'y': 80,
            'w': 400,
            'items': '\t'.join(['Aaaaa', 'Bbbbb', 'Ccccc', 'Ddddd']),
            'val': 1,
            'act': True, # for on_change
            'on_change': self.callback_combo_change2,
            })

        return h


    def init_tempdlg(self, x=150, y=150):
        h=dlg_proc(0, DLG_CREATE)
        dlg_proc(h, DLG_PROP_SET, prop={
            'cap': 'temp dlg',
            'x': x,
            'y': y,
            'w': 300,
            'h': 200,
            'color': 0xc0c0c0,
            'on_key_down': 'cuda_testing_dlg_proc.callback_tempdlg_on_key_down',
            'on_close_query': 'cuda_testing_dlg_proc.callback_tempdlg_on_close_query',
            })

        n=dlg_proc(h, DLG_CTL_ADD, 'button')
        dlg_proc(h, DLG_CTL_PROP_SET, index=n, prop={
            'name': 'btn_close',
            'cap': 'close',
            'x': 20,
            'y': 20,
            'w': 100,
            'on_change': 'cuda_testing_dlg_proc.callback_tempdlg'
            })

        n=dlg_proc(h, DLG_CTL_ADD, 'button')
        dlg_proc(h, DLG_CTL_PROP_SET, index=n, prop={
            'name': 'btn_clonedlg',
            'cap': 'clone dlg',
            'x': 20,
            'y': 50,
            'w': 100,
            'on_change': 'cuda_testing_dlg_proc.callback_tempdlg'
            })

        n=dlg_proc(h, DLG_CTL_ADD, 'check')
        dlg_proc(h, DLG_CTL_PROP_SET, index=n, prop={
            'name': 'chk_canclose',
            'cap': 'can close form',
            'x': 20,
            'y': 80,
            'w': 100,
            'val': True
            })

        n=dlg_proc(h, DLG_CTL_ADD, 'label')
        dlg_proc(h, DLG_CTL_PROP_SET, index=n, prop={
            'name': 'label_info',
            'cap': '(shows key press)',
            'x': 20,
            'y': 160,
            'w': 100
            })

        return h


    def do_editor_gap(self, ed, num):

        id_bitmap, id_canvas = ed.gap(GAP_MAKE_BITMAP, 600, 50)
        canvas_proc(id_canvas, CANVAS_SET_BRUSH, color=0xa0ffa0)
        canvas_proc(id_canvas, CANVAS_SET_ANTIALIAS, style=ANTIALIAS_ON)
        canvas_proc(id_canvas, CANVAS_POLYGON, '200,0,300,30,200,49')
        canvas_proc(id_canvas, CANVAS_SET_BRUSH, color=0xffffff, style=BRUSH_CLEAR)
        canvas_proc(id_canvas, CANVAS_TEXT, x=230, y=10, text='gap %d'%(num+1))
        canvas_proc(id_canvas, CANVAS_SET_BRUSH, color=0xffffff, style=BRUSH_SOLID)
        ed.gap(GAP_ADD, num, id_bitmap, tag=10)


    def init_editor_dlg(self):

        h=dlg_proc(0, DLG_CREATE)
        dlg_proc(h, DLG_PROP_SET, prop={
            'cap': 'editor test',
            'w': 750,
            'h': 520
            })

        n=dlg_proc(h, DLG_CTL_ADD, 'editor')
        dlg_proc(h, DLG_CTL_PROP_SET, index=n, prop={
            'name': 'ed_main',
            'align': ALIGN_TOP,
            'sp_a': 6,
            'h': 350,
            'on_change': self.callback_editor_on_change,
            'on_caret': self.callback_editor_on_caret,
            'on_scroll': self.callback_editor_on_scroll,
            'on_key_down': self.callback_editor_on_key_down,
            'on_key_up': self.callback_editor_on_key_up,
            'on_click_gutter': self.callback_editor_on_click_gutter,
            'on_click_gap': self.callback_editor_on_click_gap,
            'on_click_link': self.callback_editor_on_click_link,
            'on_paste': self.callback_editor_on_paste,
            'on_menu': self.callback_editor_on_menu,
            })

        h_editor = dlg_proc(h, DLG_CTL_HANDLE, name='ed_main')
        ed0 = Editor(h_editor)
        ed0.set_text_all(
r"""#include <stdio.h>

int main(int argc, char *argv[])
{
  printf("Hello.\n");
  return 0;
}

/* clickable links: www.test.com , https://yahoo.com */
""")
        #ed0.set_caret(0, 3, 0, 2)
        #ed0.set_prop(PROP_CARET_SHAPE, 2)
        ed0.set_prop(PROP_MINIMAP, True)
        ed0.set_prop(PROP_LEXER_FILE, 'C++')
        self.do_editor_gap(ed0, 2)

        #-----------------------------------------
        idc=dlg_proc(h, DLG_CTL_ADD,"editor_edit");
        dlg_proc(h, DLG_CTL_PROP_SET, index=idc, prop={
            'name':'ed_edit', 'x':5, 'y':370, 'w':200, 'h':26
            })
        eh=dlg_proc(h, DLG_CTL_HANDLE, name='ed_edit')
        e1=Editor(eh)
        e1.set_prop(PROP_COMBO_ITEMS, '\n'.join(('aa','bb','ee')) )
        e1.set_text_all('editor_edit')

        #-----------------------------------------
        idc=dlg_proc(h, DLG_CTL_ADD,"editor_combo");
        dlg_proc(h, DLG_CTL_PROP_SET, index=idc, prop={
            'name':'ed_combo', 'x':5, 'y':400, 'w':200, 'h':26
            })

        eh=dlg_proc(h, DLG_CTL_HANDLE, name='ed_combo')
        e2=Editor(eh)
        e2.set_prop(PROP_COMBO_ITEMS, '\n'.join(('aa','bb','dd')) )
        e2.set_text_all('editor_combo')

        dlg_proc(h, DLG_CTL_FOCUS, name='ed_main')

        return h


    def callback_main_movebtn(self, id_dlg, id_ctl):
        print('callback_main_movebtn')
        d = dlg_proc(id_dlg, DLG_CTL_PROP_GET, index=id_ctl)
        dlg_proc(id_dlg, DLG_CTL_PROP_SET, index=id_ctl, prop={'x': d['x']+10, 'y': d['y']+8 } )


    def test_pages(self):
        id = dlg_proc(0, DLG_CREATE)

        dlg_proc(id, DLG_PROP_SET, {
            'w': 400,
            'h': 300,
            'cap': 'Test type=pages, tabs'
            })

        n = dlg_proc(id, DLG_CTL_ADD, 'pages')
        dlg_proc(id, DLG_CTL_PROP_SET, index=n, prop={
            'name': 'mypages',
            'x': 10,
            'y': 10,
            'w': 380,
            'h': 140,
            'items': '\t'.join(['page-A', 'page-B', 'page-C']),
            'on_change': self.callback_pages_on_change,
            'act': True,
            })

        n = dlg_proc(id, DLG_CTL_ADD, 'tabs')
        dlg_proc(id, DLG_CTL_PROP_SET, index=n, prop={
            'name': 'mytabs',
            'x': 10,
            'y': 160,
            'w': 380,
            'h': 50,
            'items': '\t'.join(['tab-1', 'tab-2', 'tab-3']),
            'on_change': self.callback_tabs_on_change,
            'act': True,
            })

        n = dlg_proc(id, DLG_CTL_ADD, 'label')
        dlg_proc(id, DLG_CTL_PROP_SET, index=n, prop={
            'name': 'tab_label',
            'x': 10,
            'y': 220,
            'w': 300,
            'cap': '(tabs not clicked)',
            })

        n = dlg_proc(id, DLG_CTL_ADD, 'check')
        dlg_proc(id, DLG_CTL_PROP_SET, index=n, prop={
            'name': 'check0',
            'x': 10,
            'y': 10,
            'w': 300,
            'cap': 'check-A',
            'p':'mypages.0'
            })

        n = dlg_proc(id, DLG_CTL_ADD, 'button')
        dlg_proc(id, DLG_CTL_PROP_SET, index=n, prop={
            'name': 'btn_a',
            'x': 10,
            'y': 40,
            'w': 150,
            'cap': 'toggle check-A',
            'p':'mypages.0',
            'on_change': self.callback_pages_button_a,
            })

        n = dlg_proc(id, DLG_CTL_ADD, 'check')
        dlg_proc(id, DLG_CTL_PROP_SET, index=n, prop={
            'name': 'check1',
            'x': 20,
            'y': 20,
            'w': 300,
            'cap': 'check-B',
            'p': 'mypages.1'
            })

        n = dlg_proc(id, DLG_CTL_ADD, 'label')
        dlg_proc(id, DLG_CTL_PROP_SET, index=n, prop={
            'name': 'lab_b',
            'x': 20,
            'y': 50,
            'w': 300,
            'cap': 'label-B',
            'p': 'mypages.1'
            })
        n = dlg_proc(id, DLG_CTL_ADD, 'label')
        dlg_proc(id, DLG_CTL_PROP_SET, index=n, prop={
            'name': 'lab_c',
            'x': 20,
            'y': 50,
            'w': 300,
            'cap': 'label-C',
            'p': 'mypages.2'
            })

        dlg_proc(id, DLG_SHOW_MODAL)
        dlg_proc(id, DLG_FREE)

    def test_toolbar(self):
        dir_icons = os.path.join(app_path(APP_DIR_DATA), 'sideicons', 'octicons_20x20')
        fn_icon1 = os.path.join(dir_icons, 'console.png')
        fn_icon2 = os.path.join(dir_icons, 'find.png')
        print('icon1:', fn_icon1)
        print('icon2:', fn_icon2)

        id = dlg_proc(0, DLG_CREATE)

        dlg_proc(id, DLG_PROP_SET, {
            'w': 500,
            'h': 300,
            'cap': 'Test type=toolbar/statusbar',
            'border': DBORDER_SIZE,
            })

        #------------
        n = dlg_proc(id, DLG_CTL_ADD, 'toolbar')
        dlg_proc(id, DLG_CTL_PROP_SET, index=n, prop={
            'name': 'tb',
            'x': 0,
            'y': 0,
            'w': 20,
            'h': 40,
            'align': ALIGN_TOP,
            'color': 0x80B080,
            'on_menu': self.callback_toolbar_menu,
            })

        tb_id = dlg_proc(id, DLG_CTL_HANDLE, index=n)

        imglist_id = toolbar_proc(tb_id, TOOLBAR_GET_IMAGELIST)
        imagelist_proc(imglist_id, IMAGELIST_SET_SIZE, value=(20,20))
        icon1 = imagelist_proc(imglist_id, IMAGELIST_ADD, value=fn_icon1)
        icon2 = imagelist_proc(imglist_id, IMAGELIST_ADD, value=fn_icon2)
        print('icon indexes:', icon1, icon2)

        toolbar_proc(tb_id, TOOLBAR_ADD_ITEM)
        count = toolbar_proc(tb_id, TOOLBAR_GET_COUNT)
        btn_id = toolbar_proc(tb_id, TOOLBAR_GET_BUTTON_HANDLE, index=count-1)
        button_proc(btn_id, BTN_SET_KIND, BTNKIND_TEXT_ICON_HORZ)
        button_proc(btn_id, BTN_SET_TEXT, 'About')
        button_proc(btn_id, BTN_SET_IMAGELIST, imglist_id)
        button_proc(btn_id, BTN_SET_IMAGEINDEX, icon1)
        button_proc(btn_id, BTN_SET_DATA1, self.show_about)

        toolbar_proc(tb_id, TOOLBAR_ADD_ITEM)
        count = toolbar_proc(tb_id, TOOLBAR_GET_COUNT)
        btn_id = toolbar_proc(tb_id, TOOLBAR_GET_BUTTON_HANDLE, index=count-1)
        button_proc(btn_id, BTN_SET_KIND, BTNKIND_TEXT_ICON_HORZ)
        button_proc(btn_id, BTN_SET_TEXT, 'Hotkey help')
        button_proc(btn_id, BTN_SET_IMAGELIST, imglist_id)
        button_proc(btn_id, BTN_SET_IMAGEINDEX, icon2)
        button_proc(btn_id, BTN_SET_DATA1, 2707)

        toolbar_proc(tb_id, TOOLBAR_ADD_ITEM)
        count = toolbar_proc(tb_id, TOOLBAR_GET_COUNT)
        btn_id = toolbar_proc(tb_id, TOOLBAR_GET_BUTTON_HANDLE, index=count-1)
        button_proc(btn_id, BTN_SET_KIND, BTNKIND_TEXT_CHOICE)
        button_proc(btn_id, BTN_SET_TEXT, '???')
        button_proc(btn_id, BTN_SET_ARROW, True)
        button_proc(btn_id, BTN_SET_ITEMS, '\n'.join(['choice-aa', 'choice-bbbb', 'choice-cccc', 'choice-ddd']))
        button_proc(btn_id, BTN_SET_ITEMINDEX, 3)
        button_proc(btn_id, BTN_SET_WIDTH, 150)
        button_proc(btn_id, BTN_SET_DATA1, lambda: print('choice in menu'))

        toolbar_proc(tb_id, TOOLBAR_UPDATE)

        #----------
        n = dlg_proc(id, DLG_CTL_ADD, 'statusbar')
        dlg_proc(id, DLG_CTL_PROP_SET, index=n, prop={
            'name': 'sb',
            'x': 0,
            'y': 0,
            'w': 20,
            'h': 28,
            'align': ALIGN_BOTTOM,
            'color': 0x40A0A0,
            })

        sb_id = dlg_proc(id, DLG_CTL_HANDLE, index=n)
        statusbar_proc(sb_id, STATUSBAR_SET_IMAGELIST, value=imglist_id)
        statusbar_proc(sb_id, STATUSBAR_ADD_CELL, tag=11)
        statusbar_proc(sb_id, STATUSBAR_ADD_CELL, tag=22)
        statusbar_proc(sb_id, STATUSBAR_ADD_CELL, tag=33)

        statusbar_proc(sb_id, STATUSBAR_SET_CELL_CALLBACK, tag=33, value=
            'module=cuda_testing_dlg_proc;cmd=callback_statusbar_click;')

        statusbar_proc(sb_id, STATUSBAR_SET_CELL_SIZE, tag=11, value=150)
        statusbar_proc(sb_id, STATUSBAR_SET_CELL_COLOR_BACK, tag=11, value=0xff00)
        statusbar_proc(sb_id, STATUSBAR_SET_CELL_COLOR_FONT, tag=11, value=0xff)
        statusbar_proc(sb_id, STATUSBAR_SET_CELL_TEXT, tag=11, value='cell-a')
        statusbar_proc(sb_id, STATUSBAR_SET_CELL_HINT, tag=11, value='hint for cell-a')
        statusbar_proc(sb_id, STATUSBAR_SET_CELL_IMAGEINDEX, tag=11, value=icon1)
        statusbar_proc(sb_id, STATUSBAR_SET_CELL_FONT_NAME, tag=11, value='Courier')
        statusbar_proc(sb_id, STATUSBAR_SET_CELL_FONT_SIZE, tag=11, value=18)

        statusbar_proc(sb_id, STATUSBAR_SET_CELL_SIZE, tag=22, value=50)
        statusbar_proc(sb_id, STATUSBAR_SET_CELL_COLOR_BACK, tag=22, value=0xffff)
        statusbar_proc(sb_id, STATUSBAR_SET_CELL_COLOR_FONT, tag=22, value=0xff00)
        statusbar_proc(sb_id, STATUSBAR_SET_CELL_TEXT, tag=22, value='cell-b')
        statusbar_proc(sb_id, STATUSBAR_SET_CELL_HINT, tag=22, value='hint for cell-b')

        statusbar_proc(sb_id, STATUSBAR_SET_CELL_SIZE, tag=33, value=150)
        statusbar_proc(sb_id, STATUSBAR_SET_CELL_TEXT, tag=33, value='cell-c')
        statusbar_proc(sb_id, STATUSBAR_SET_CELL_IMAGEINDEX, tag=33, value=icon2)
        statusbar_proc(sb_id, STATUSBAR_SET_CELL_ALIGN, tag=33, value='R')


        #----------
        dlg_proc(id, DLG_SHOW_MODAL)
        dlg_proc(id, DLG_FREE)


    def test_treeview(self):
        id = dlg_proc(0, DLG_CREATE)

        dlg_proc(id, DLG_PROP_SET, {
            'w': 400,
            'h': 300,
            'cap': 'Test type=treeview'
            })

        n = dlg_proc(id, DLG_CTL_ADD, 'treeview')
        dlg_proc(id, DLG_CTL_PROP_SET, index=n, prop={
            'name': 'my',
            'x': 10,
            'y': 10,
            'w': 380,
            'h': 280,
            'on_fold': self.callback_treeview_on_fold,
            'on_unfold': self.callback_treeview_on_unfold,
            'on_click_dbl': self.callback_treeview_on_click_dbl,
            })

        self.h_tree = dlg_proc(id, DLG_CTL_HANDLE, index=n)
        tree_proc(self.h_tree, TREE_THEME)

        item0a = tree_proc(self.h_tree, TREE_ITEM_ADD, id_item=0, index=-1, text='item 0a')
        item0b = tree_proc(self.h_tree, TREE_ITEM_ADD, id_item=0, index=-1, text='item 0b')
        item1a = tree_proc(self.h_tree, TREE_ITEM_ADD, id_item=item0a, index=-1, text='sub item 1a')
        item1b = tree_proc(self.h_tree, TREE_ITEM_ADD, id_item=item0a, index=-1, text='sub item 1b')
        item2a = tree_proc(self.h_tree, TREE_ITEM_ADD, id_item=item1a, index=-1, text='sub item 2a')

        tree_proc(self.h_tree, TREE_ITEM_UNFOLD_DEEP, id_item=0)

        #test get/set range
        rng = (2,2,5,5)
        tree_proc(self.h_tree, TREE_ITEM_SET_RANGE, id_item=item1a, text=rng)
        rng2 = tree_proc(self.h_tree, TREE_ITEM_GET_RANGE, id_item=item1a)
        print('Test get/set range: '+('ok' if rng==rng2 else 'failed'))

        dlg_proc(id, DLG_SHOW_MODAL)
        dlg_proc(id, DLG_FREE)


    def callback_treeview_on_fold(self, id_dlg, id_ctl, data='', info=''):
        prop = tree_proc(self.h_tree, TREE_ITEM_GET_PROPS, id_item=data)
        print('callback_treeview_on_unfold,', 'item:', prop)

    def callback_treeview_on_unfold(self, id_dlg, id_ctl, data='', info=''):
        prop = tree_proc(self.h_tree, TREE_ITEM_GET_PROPS, id_item=data)
        print('callback_treeview_on_unfold,', 'item:', prop)

    def callback_treeview_on_click_dbl(self, id_dlg, id_ctl, data='', info=''):
        print('callback_treeview_on_click_dbl')

    def callback_pages_button_a(self, id_dlg, id_ctl, data='', info=''):
        prop = dlg_proc(id_dlg, DLG_CTL_PROP_GET, name='check0')
        new_val = not (prop['val']=='1')
        dlg_proc(id_dlg, DLG_CTL_PROP_SET, name='check0', prop={
            'val': new_val
            })

    def callback_pages_on_change(self, id_dlg, id_ctl, data='', info=''):
        print('pages on_change')
        prop = dlg_proc(id_dlg, DLG_CTL_PROP_GET, name='mypages')
        print('tab hovered:', prop['tab_hovered'])

    def callback_tabs_on_change(self, id_dlg, id_ctl, data='', info=''):
        print('tabs on_change')
        prop = dlg_proc(id_dlg, DLG_CTL_PROP_GET, name='mytabs')
        print('tab hovered:', prop['tab_hovered'])

        n = int(prop['val'])
        dlg_proc(id_dlg, DLG_CTL_PROP_SET, name='tab_label', prop={
            'cap': '(clicked tab-'+str(n+1)+')',
            })

    def test_sidepanel(self):
        print('test_sidepanel')
        title = 'Side dialog'
        id_dlg = self.init_maindlg()
        icon_name = 'project.png'

        app_proc(PROC_SIDEPANEL_ADD_DIALOG, (title, id_dlg, icon_name) )
        app_proc(PROC_SIDEPANEL_ACTIVATE, title)

    def show_about(self):
        ed.cmd(2700)

    def test_btn(self):
        h = self.init_buttondlg()
        dlg_proc(h, DLG_SHOW_MODAL)
        dlg_proc(h, DLG_FREE)

    def test_splitter(self):
        h = self.init_splitterdlg()
        dlg_proc(h, DLG_SHOW_MODAL)
        dlg_proc(h, DLG_FREE)

    def test_listbox(self):
        h = self.init_listdlg()
        dlg_proc(h, DLG_SHOW_MODAL)
        dlg_proc(h, DLG_FREE)

    def test_editor(self):
        h = self.init_editor_dlg()
        dlg_proc(h, DLG_SHOW_MODAL)
        dlg_proc(h, DLG_FREE)

    def test_combo(self):
        h = self.init_combo_dlg()
        dlg_proc(h, DLG_SHOW_MODAL)
        dlg_proc(h, DLG_FREE)


    def test_listview(self):
        self.click_num   = 0
        self.click_col   = -1

        def when_click_header(id_dlg, id_ctl, data):
            self.click_num = 1 if self.click_col != data else (self.click_num+1)%3
            self.click_col = data
            cols_s = dlg_proc(id_dlg, DLG_CTL_PROP_GET, index=id_ctl)['columns']
            cols_i = [c.split('\r') for c in cols_s.split('\t')]
            cols_i[self.click_col][0] = cols_i[self.click_col][0].strip('+-')
            cols_i[self.click_col][0] += '++' if self.click_num==1 else '--' if self.click_num==2 else ''
            dlg_proc(id_dlg, DLG_CTL_PROP_SET, index=id_ctl, prop={
                'columns': '\t'.join(['\r'.join(c) for c in cols_i])
                })

        id = dlg_proc(0, DLG_CREATE)

        dlg_proc(id, DLG_PROP_SET, {
            'w': 500,
            'h': 400,
            'cap': 'Test type=listview',
            'border': DBORDER_SIZE,
            })

        n = dlg_proc(id, DLG_CTL_ADD, 'listview')
        dlg_proc(id, DLG_CTL_PROP_SET, index=n, prop={
            'name': 'my',
            'align': ALIGN_CLIENT,
            'sp_a': 10,
            'items': '1\r2\r3\tcell00\rcell01\rcell02\tcell10\rcell11\rcell12\tcell20\rcell21\rcell22',
            'val': 1,
            'columns': '\t'.join([
                '\r'.join(['aaa', '200', '180', '210', 'C']),
                '\r'.join(['bbb', '100', '', '', 'R']),
                '\r'.join(['ccc', '100', '', '', 'R']),
                ]),
            'on_click_header': when_click_header
            })

        #after ListView creation, get its ImageList handles
        props = dlg_proc(id, DLG_CTL_PROP_GET, index=n)
        id_listview1 = props['imagelist_small']
        id_listview2 = props['imagelist_large']

        dir_icons = os.path.join(app_path(APP_DIR_DATA), 'sideicons', 'octicons_20x20')
        fn_icon1 = os.path.join(dir_icons, 'console.png')
        fn_icon2 = os.path.join(dir_icons, 'find.png')
        print('icon1:', fn_icon1)
        print('icon2:', fn_icon2)

        imagelist_proc(id_listview1, IMAGELIST_SET_SIZE, value=(20,20))
        icon1 = imagelist_proc(id_listview1, IMAGELIST_ADD, value=fn_icon1)
        icon2 = imagelist_proc(id_listview1, IMAGELIST_ADD, value=fn_icon2)
        print('icon indexes:', icon1, icon2)

        dlg_proc(id, DLG_CTL_PROP_SET, index=n, prop={
            'imageindexes': '\t'.join([str(icon1), str(icon2)])
            })
        props = dlg_proc(id, DLG_CTL_PROP_GET, index=n)
        print('Listview props:', props)

        #s = dlg_proc(id, DLG_CTL_PROP_GET, index=n)['columns']
        #print(repr(s))

        dlg_proc(id, DLG_SHOW_MODAL)
        dlg_proc(id, DLG_FREE)

    def test_labels_render(self):
        test_labels_render()
