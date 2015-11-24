from cudatext import *

class Command:
    def run(self):
        s = "Lines count: " + str(ed.get_line_count())
        msg_box(s, MB_OK+MB_ICONINFO)
        
    def on_open(self, ed_self):
        print('test1 open', ed_self.get_filename())
        
    def on_save(self, ed_self):
        print('test1 save', ed_self.get_filename())
        
    def on_complete(self, ed_self):
        pass
        #print('test1 acp')
        #return True
        
    def on_goto_def(self, ed_self):
        print('test1 gotodef')
        return True
        
    def on_console_nav(self, ed_self, s):
        print('nav '+s)
        