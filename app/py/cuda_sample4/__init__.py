from cudatext import *

class Command:
    def r1(self):
        s = "Lines count: " + str(ed.get_line_count())
        msg_box(s, MB_OK+MB_ICONINFO)
    def r2(self):
        pass
    def r3(self):
        pass
