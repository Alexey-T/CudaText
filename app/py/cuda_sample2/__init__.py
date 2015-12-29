from cudatext import *

class Command:
    def run(self):
        s = "Lines count: " + str(ed.get_line_count())
        msg_box(s, MB_OK+MB_ICONINFO)

    def on_output_nav(self, ed_self, text, tag):
        msg_box('out', MB_OK)
        print('oo:', text, 'tag:', tag)