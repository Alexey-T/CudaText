from cudatext import *

class Command:
    def run(self):
        s = "Lines count: " + str(ed.get_line_count())
        msg_box(s, MB_OK+MB_ICONINFO)

    def on_macro(self, ed_self, text):
        print('macro')
        print(text.splitlines())
        
    def test(self, par):
        print('macro par:', par)