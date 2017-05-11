from cudatext import *


def callback_main_close(id_dlg, id_ctl, id_event='', info=''):
    print('callback_main_close')
    dlg_proc(id_dlg, DLG_HIDE)

def callback_main_menu(id_dlg, id_ctl, id_event='', info=''):
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

    def callback_maindlg(self, id_dlg, id_ctl, id_event='', info=''):
        print('callback_maindlg', id_event)
        h = id_dlg

        n_chk = dlg_proc(h, DLG_CTL_FIND, prop='chk1')
        n_edit = dlg_proc(h, DLG_CTL_FIND, prop='edit1')
        n_btn1 = dlg_proc(h, DLG_CTL_FIND, prop='btn1')
        n_btn_dlg = dlg_proc(h, DLG_CTL_FIND, prop='btn_dlg')
        n_color = dlg_proc(h, DLG_CTL_FIND, prop='color')
        n_chk_dock = dlg_proc(h, DLG_CTL_FIND, prop='chk_dock')

        if id_event=='on_change':
            if id_ctl==n_chk:
                d = dlg_proc(h, DLG_CTL_PROP_GET, index=n_color)
                dlg_proc(h, DLG_CTL_PROP_SET, index=n_color, prop={'vis': not d['vis']} )

            if id_ctl==n_btn1:
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


        #if id_event=='on_resize':
        #    d = dlg_proc(h, DLG_PROP_GET)
        #    dlg_proc(h, DLG_CTL_PROP_SET, index=n_color, prop={'x': d['w']-20, 'h': d['h']-10 } )


    def callback_tempdlg(self, id_dlg, id_ctl, id_event='', info=''):
        print('callback_tempdlg', id_event)

        n_close = dlg_proc(id_dlg, DLG_CTL_FIND, 'btn_close')
        n_clone = dlg_proc(id_dlg, DLG_CTL_FIND, 'btn_clonedlg')
        n_canclose = dlg_proc(id_dlg, DLG_CTL_FIND, 'chk_canclose')
        n_info = dlg_proc(id_dlg, DLG_CTL_FIND, 'label_info')

        if id_event=='on_key_down':
            state = app_proc(PROC_GET_KEYSTATE, '')
            str_key =\
                ('Meta+' if 'm' in state else '')+\
                ('Ctrl+' if 'c' in state else '')+\
                ('Alt+' if 'a' in state else '')+\
                ('Shift+' if 's' in state else '')+\
                app_proc(PROC_HOTKEY_INT_TO_STR, str(id_ctl))
            dlg_proc(id_dlg, DLG_CTL_PROP_SET, index=n_info, prop={'cap': 'keypress: '+str_key })
            return True

        if id_event=='on_close_query':
            d = dlg_proc(id_dlg, DLG_CTL_PROP_GET, index=n_canclose)
            #print('canclose:', d['val'])
            return d['val']=='1'

        if id_event=='on_change':
            if id_ctl==n_close:
                dlg_proc(id_dlg, DLG_HIDE)

            if id_ctl==n_clone:
                print('  tempdlg begin')
                d = dlg_proc(id_dlg, DLG_PROP_GET)
                hh = self.init_tempdlg(d['x']+20, d['y']+20)
                dlg_proc(hh, DLG_SHOW_MODAL)
                dlg_proc(hh, DLG_FREE)
                print('  tempdlg end')



    def init_maindlg(self):
        h=dlg_proc(0, DLG_CREATE)
        dlg_proc(h, DLG_PROP_SET, prop={'cap':'main dlg', 'x':100, 'y':50, 'w':400, 'h':300, 'resize':True, 'w_min': 200, 'h_min': 100, 'topmost':True, 'callback': 'cuda_testing_dlg_proc.callback_maindlg', 'events': '*' })

        n=dlg_proc(h, DLG_CTL_ADD, 'label')
        dlg_proc(h, DLG_CTL_PROP_SET, index=n, prop={'name': '', 'cap':'label', 'x':10, 'y':10, 'w':50, 'tag': 'some_tag' })

        n=dlg_proc(h, DLG_CTL_ADD, 'check')
        dlg_proc(h, DLG_CTL_PROP_SET, index=n, prop={'name': 'chk1', 'cap':'show panel', 'val':True, 'x':60, 'y':8, 'w':200, 'act':True })

        n=dlg_proc(h, DLG_CTL_ADD, 'edit')
        dlg_proc(h, DLG_CTL_PROP_SET, index=n, prop={'name': 'edit1', 'val':'edit1', 'x':10, 'y':30, 'w':200} )

        n=dlg_proc(h, DLG_CTL_ADD, 'button')
        dlg_proc(h, DLG_CTL_PROP_SET, index=n, prop={'name': 'btn1', 'cap':'Btn&1: caption', 'x':10, 'y':60, 'w':100} )

        n=dlg_proc(h, DLG_CTL_ADD, 'button')
        dlg_proc(h, DLG_CTL_PROP_SET, index=n, prop={'name': 'btn2', 'cap':'Btn&2: move', 'x':120, 'y':60, 'w':100, 'callback': 'cuda_testing_dlg_proc.callback_main_movebtn'} )

        n=dlg_proc(h, DLG_CTL_ADD, 'colorpanel')
        dlg_proc(h, DLG_CTL_PROP_SET, index=n, prop={'name': 'color', 'cap': ' ', 'x':380, 'y':5, 'w':15, 'h':290, 'props':(1, 0xc0f0f0) } )
        #anchors of colorpanel
        dlg_proc(h, DLG_CTL_PROP_SET, index=n, prop={ 'a_l': None, 'a_r': ('', ']'), 'a_b': ('', ']'), 'sp_a': 6  } )

        n=dlg_proc(h, DLG_CTL_ADD, 'button')
        dlg_proc(h, DLG_CTL_PROP_SET, index=n, prop={'name': 'btn_dlg', 'cap':'temp dlg', 'x':10, 'y':200, 'w':100} )

        n=dlg_proc(h, DLG_CTL_ADD, 'button')
        dlg_proc(h, DLG_CTL_PROP_SET, index=n, prop={'name': 'btn_menu', 'cap':'menu here', 'x':10, 'y':230, 'w':100, 'callback': 'module=cuda_testing_dlg_proc;func=callback_main_menu;'} )

        n=dlg_proc(h, DLG_CTL_ADD, 'button')
        dlg_proc(h, DLG_CTL_PROP_SET, index=n, prop={'name': 'btn_callbk', 'cap':'complex callback', 'x':120, 'y':200, 'w':120, 'callback': 'module=cuda_testing_dlg_proc.testcall;func=callback_main_complex;'} )

        n=dlg_proc(h, DLG_CTL_ADD, 'button')
        dlg_proc(h, DLG_CTL_PROP_SET, index=n, prop={'name': 'btn_ok', 'cap':'close', 'x':120, 'y':230, 'w':120, 'callback': 'module=cuda_testing_dlg_proc;func=callback_main_close;'} )

        n=dlg_proc(h, DLG_CTL_ADD, 'check')
        dlg_proc(h, DLG_CTL_PROP_SET, index=n, prop={'name': 'chk_dock', 'cap':'temp dlg: docked', 'x':10, 'y':170, 'w':120 } )

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


    def init_tempdlg(self, x=150, y=150):
        h=dlg_proc(0, DLG_CREATE)
        dlg_proc(h, DLG_PROP_SET, prop={'cap':'temp dlg', 'x':x, 'y':y, 'w':300, 'h':200, 'callback': 'cuda_testing_dlg_proc.callback_tempdlg', 'events': '*', 'color': 0xc0c0c0 })

        n=dlg_proc(h, DLG_CTL_ADD, 'button')
        dlg_proc(h, DLG_CTL_PROP_SET, index=n, prop={'name': 'btn_close', 'cap':'close', 'x':20, 'y':20, 'w':100 })

        n=dlg_proc(h, DLG_CTL_ADD, 'button')
        dlg_proc(h, DLG_CTL_PROP_SET, index=n, prop={'name': 'btn_clonedlg', 'cap':'clone dlg', 'x':20, 'y':50, 'w':100 })

        n=dlg_proc(h, DLG_CTL_ADD, 'check')
        dlg_proc(h, DLG_CTL_PROP_SET, index=n, prop={'name': 'chk_canclose', 'cap':'can close form', 'x':20, 'y':80, 'w':100, 'val':True })

        n=dlg_proc(h, DLG_CTL_ADD, 'label')
        dlg_proc(h, DLG_CTL_PROP_SET, index=n, prop={'name': 'label_info', 'cap':'(shows key press)', 'x':20, 'y':160, 'w':100 })

        n=dlg_proc(h, DLG_CTL_ADD, 'listbox_ex')
        dlg_proc(h, DLG_CTL_PROP_SET, index=n, prop={'name': 'list1', 'x':160, 'y':10, 'w':130, 'h': 140 })

        h_list = dlg_proc(h, DLG_CTL_HANDLE, index=n)
        listbox_proc(h_list, LISTBOX_ADD, index=-1, text='listitem-a')
        listbox_proc(h_list, LISTBOX_ADD, index=-1, text='listitem-b')
        listbox_proc(h_list, LISTBOX_ADD, index=-1, text='listitem-c')
        listbox_proc(h_list, LISTBOX_SET_SEL, index=2)

        return h


    def callback_main_movebtn(self, id_dlg, id_ctl, id_event=''):
        print('callback_main_movebtn')
        d = dlg_proc(id_dlg, DLG_CTL_PROP_GET, index=id_ctl)
        dlg_proc(id_dlg, DLG_CTL_PROP_SET, index=id_ctl, prop={'x': d['x']+10, 'y': d['y']+8 } )
