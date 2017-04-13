from cudatext import *

h=0

class Command:
    def on_dlg(self, ed_self, id_dlg, id_ctl, id_event):
        global h
        if id_dlg!=h: return

        if id_ctl==4:
            d = dlg_proc(h, DLG_CTL_PROP_GET, index=id_ctl)
            dlg_proc(h, DLG_CTL_PROP_SET, index=id_ctl, prop={'x': d['x']+10, 'y': d['y']+8 } )

        if id_ctl==3:
            d = dlg_proc(h, DLG_CTL_PROP_GET, index=2)
            dlg_proc(h, DLG_PROP_SET, prop={'cap': 'entered: '+d['val'] } )

        self.show_res()
        #dlg_proc(id_dlg, DLG_HIDE)

    def init_dlg(self):
        global h
        h=dlg_proc(0, DLG_CREATE)
        dlg_proc(h, DLG_PROP_SET, prop={'cap':'TestDlg', 'x':100, 'y':50, 'w':400, 'h':300, 'resize':True })

        n=dlg_proc(h, DLG_CTL_ADD, 'label')
        dlg_proc(h, DLG_CTL_PROP_SET, index=n, prop={'name': 'lab1', 'cap':'label', 'x':10, 'y':10, 'w':50})

        n=dlg_proc(h, DLG_CTL_ADD, 'check')
        dlg_proc(h, DLG_CTL_PROP_SET, index=n, prop={'name': 'chk1', 'cap':'chk (active)', 'x':60, 'y':10, 'w':200, 'act':True})

        n=dlg_proc(h, DLG_CTL_ADD, 'edit')
        dlg_proc(h, DLG_CTL_PROP_SET, index=n, prop={'val':'edit1', 'x':10, 'y':30, 'w':200} )

        n=dlg_proc(h, DLG_CTL_ADD, 'button')
        dlg_proc(h, DLG_CTL_PROP_SET, index=n, prop={'cap':'Btn&1: caption', 'x':10, 'y':60, 'w':100} )

        n=dlg_proc(h, DLG_CTL_ADD, 'button')
        dlg_proc(h, DLG_CTL_PROP_SET, index=n, prop={'cap':'Btn&2: move', 'x':120, 'y':60, 'w':100} )

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
