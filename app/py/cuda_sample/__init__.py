from cudatext import *

class Command:
    def run(self):
        app_proc(PROC_SET_ESCAPE, '0')
        for i in range(500):
            print(i)
            msg_status(str(i))
            if app_proc(PROC_GET_ESCAPE, '')==True:
                msg_status('esc')
                return
