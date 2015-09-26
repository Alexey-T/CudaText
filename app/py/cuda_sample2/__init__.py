from cudatext import *

class Command:
    def run(self):
        s = "Lines count: " + str(ed.get_line_count())
        msg_box(s, MB_OK+MB_ICONINFO)
