from cudatext import *

class Command:
    ini=False
    def run(self):
        msg_box('hhh', MB_OK)

    def on_focus(self, ed_self):
        return
        if self.ini: return
        self.ini=True
        msg_box('init', MB_OK)

        id_test=app_proc(PROC_MENU_ADD, 'top;0;Tst')
        id_n1=app_proc(PROC_MENU_ADD, id_test+';2700;Abt')
        id_n2=app_proc(PROC_MENU_ADD, id_test+';0;Sub')
        id_n3=app_proc(PROC_MENU_ADD, id_n2+';2700;Abt')
        id_n4=app_proc(PROC_MENU_ADD, id_n2+';cuda_mn,run;run')
        
        