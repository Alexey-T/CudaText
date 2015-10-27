from cudatext import *

class Command:
    def run(self):
        s = "Lines count: " + str(ed.get_line_count())
        msg_box(s, MB_OK+MB_ICONINFO)

    def on_open(self, ed_self):
        print('test3 open', ed_self.get_filename())
