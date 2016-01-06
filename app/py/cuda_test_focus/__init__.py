from cudatext import *

class Command:
    def on_focus(self, ed_self):
        print('focus: '+ed_self.get_filename())
        if ed.get_filename()!=ed_self.get_filename():
            print('not ok')
