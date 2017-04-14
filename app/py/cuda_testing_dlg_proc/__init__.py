from cudatext import *

h=0

class Command:
    def on_dlg(self, ed_self, id_dlg, id_ctl, id_event):
        global h
        if id_dlg!=h: return
        print(id_event)

        n_chk = dlg_proc(h, DLG_CTL_FIND_NAME, 'chk1')
        n_edit = dlg_proc(h, DLG_CTL_FIND_NAME, 'edit1')
        n_btn1 = dlg_proc(h, DLG_CTL_FIND_NAME, 'btn1')
        n_btn2 = dlg_proc(h, DLG_CTL_FIND_NAME, 'btn2')
        n_btn_dlg = dlg_proc(h, DLG_CTL_FIND_NAME, 'btn_dlg')
        n_color = dlg_proc(h, DLG_CTL_FIND_NAME, 'color')

        if id_event=='on_change':
            if id_ctl==n_chk:
                d = dlg_proc(h, DLG_CTL_PROP_GET, index=n_color)
                dlg_proc(h, DLG_CTL_PROP_SET, index=n_color, prop={'vis': not d['vis']} )

            if id_ctl==n_btn2:
                d = dlg_proc(h, DLG_CTL_PROP_GET, index=n_btn2)
                dlg_proc(h, DLG_CTL_PROP_SET, index=n_btn2, prop={'x': d['x']+10, 'y': d['y']+8 } )

            if id_ctl==n_btn1:
                d = dlg_proc(h, DLG_CTL_PROP_GET, index=n_edit)
                dlg_proc(h, DLG_PROP_SET, prop={'cap': 'entered: '+d['val'] } )

            if id_ctl==n_btn_dlg:
                self.temp_dlg()

            self.show_res()

        if id_event=='on_resize':
            d = dlg_proc(h, DLG_PROP_GET)
            dlg_proc(h, DLG_CTL_PROP_SET, index=n_color, prop={'x': d['w']-20, 'h': d['h']-10 } )


    def init_dlg(self):
        global h
        h=dlg_proc(0, DLG_CREATE)
        dlg_proc(h, DLG_PROP_SET, prop={'cap':'TestDlg', 'x':100, 'y':50, 'w':400, 'h':300, 'resize':True, 'tag': 'form_tag' })

        n=dlg_proc(h, DLG_CTL_ADD, 'label')
        dlg_proc(h, DLG_CTL_PROP_SET, index=n, prop={'name': 'label1', 'cap':'label', 'x':10, 'y':10, 'w':50, 'tag': 'some_tag' })

        n=dlg_proc(h, DLG_CTL_ADD, 'check')
        dlg_proc(h, DLG_CTL_PROP_SET, index=n, prop={'name': 'chk1', 'cap':'show panel (active)', 'val':True, 'x':60, 'y':8, 'w':200, 'act':True })

        n=dlg_proc(h, DLG_CTL_ADD, 'edit')
        dlg_proc(h, DLG_CTL_PROP_SET, index=n, prop={'name': 'edit1', 'val':'edit1', 'x':10, 'y':30, 'w':200} )

        n=dlg_proc(h, DLG_CTL_ADD, 'button')
        dlg_proc(h, DLG_CTL_PROP_SET, index=n, prop={'name': 'btn1', 'cap':'Btn&1: caption', 'x':10, 'y':60, 'w':100} )

        n=dlg_proc(h, DLG_CTL_ADD, 'button')
        dlg_proc(h, DLG_CTL_PROP_SET, index=n, prop={'name': 'btn2', 'cap':'Btn&2: move', 'x':120, 'y':60, 'w':100} )

        n=dlg_proc(h, DLG_CTL_ADD, 'colorpanel')
        dlg_proc(h, DLG_CTL_PROP_SET, index=n, prop={'name': 'color', 'cap': ' ', 'x':380, 'y':5, 'w':15, 'h':290, 'props':(1, 0xc0f0f0) } )

        n=dlg_proc(h, DLG_CTL_ADD, 'button')
        dlg_proc(h, DLG_CTL_PROP_SET, index=n, prop={'name': 'btn_dlg', 'cap':'temp dlg', 'x':10, 'y':200, 'w':100} )

        dlg_proc(h, DLG_CTL_FOCUS, index=3)

    def nonmodal(self):
        global h
        self.init_dlg()
        dlg_proc(h, DLG_SHOW_NONMODAL)

    def modal(self):
        global h
        self.init_dlg()
        dlg_proc(h, DLG_SHOW_MODAL)
        self.show_res()
        dlg_proc(h, DLG_FREE)

    def show_res(self):
        global h
        res = dlg_proc(h, DLG_PROP_GET)
        print('dlg_proc:', res)

        cnt = dlg_proc(h, DLG_CTL_COUNT)
        for n in range(cnt):
            res = dlg_proc(h, DLG_CTL_PROP_GET, index=n)
            print('ctl%d:'%n, res)

    def temp_dlg(self):
        h=dlg_proc(0, DLG_CREATE)
        dlg_proc(h, DLG_PROP_SET, prop={'cap':'TempDlg', 'x':200, 'y':200, 'w':300, 'h':200 })

        n=dlg_proc(h, DLG_CTL_ADD, 'button')
        dlg_proc(h, DLG_CTL_PROP_SET, index=n, prop={'cap':'OK', 'x':100, 'y':50, 'w':100 })

        dlg_proc(h, DLG_SHOW_MODAL)
        dlg_proc(h, DLG_FREE)
