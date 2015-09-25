from cudatext import *

class Command:
    def do_edit(self):
        msg_box('edit', MB_OK)
        msg_status('edit')
    def do_remove(self):
        msg_box('remov', MB_OK)
