from cudatext import *

class Command:
    cnt=0
    def run(self):
        self.cnt=self.cnt+1
        print('my1: ', self.cnt)
        ed.set_text_line(-1, 'my1: '+str(self.cnt))
        