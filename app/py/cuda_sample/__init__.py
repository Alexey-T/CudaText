from cudatext import *

class Command:
    nn=0
    def run(self):
        self.nn+=1
        msg_box('nnnn'+str(self.nn), MB_OK+MB_ICONINFO)
