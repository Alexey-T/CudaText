from cudatext import *

class Command:
    def run(self):
        for i in range(400):
            r=app_proc(PROC_GET_COMMAND_PLUGIN, str(i))
            if not r: return
            print(r)
            
