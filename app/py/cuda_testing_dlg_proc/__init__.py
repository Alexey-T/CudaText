from cudatext import *

h=0

class Command:
    def on_dlg(self, ed_self, id_dlg, id_ctl, id_event):
        print(id_event)
        self.show_res()
        #dlg_proc(id_dlg, DLG_HIDE)

    def init_dlg(self):
        global h
        h=dlg_proc(0, DLG_CREATE)
        dlg_proc(h, DLG_PROP_SET, prop={'cap':'TestDlg', 'x':100, 'y':50, 'w':400, 'h':300})

        n=dlg_proc(h, DLG_CTL_ADD, 'label')
        dlg_proc(h, DLG_CTL_PROP_SET, index=n, prop={'name': 'label1', 'cap':'label', 'x':10, 'y':10, 'w':50, 'act':True})

        n=dlg_proc(h, DLG_CTL_ADD, 'check')
        dlg_proc(h, DLG_CTL_PROP_SET, index=n, prop={'name': 'chk1', 'cap':'chk (active)', 'x':60, 'y':10, 'w':200, 'act':True})

        n=dlg_proc(h, DLG_CTL_ADD, 'edit')
        dlg_proc(h, DLG_CTL_PROP_SET, index=n, prop={'val':'edit1', 'x':10, 'y':30, 'w':200} )

        n=dlg_proc(h, DLG_CTL_ADD, 'button')
        dlg_proc(h, DLG_CTL_PROP_SET, index=n, prop={'cap':'&Btn1', 'x':10, 'y':60, 'w':80} )

        n=dlg_proc(h, DLG_CTL_ADD, 'button')
        dlg_proc(h, DLG_CTL_PROP_SET, index=n, prop={'cap':'&Btn2', 'x':100, 'y':60, 'w':80} )

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
