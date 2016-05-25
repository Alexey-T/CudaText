from cudatext import *
from datetime import datetime

#read this: http://strftime.org/
time_fmt = '%H:%M %d.%m.%Y'

class Command:
    def run(self):
        t = datetime.now()
        s = t.strftime(time_fmt)
        caret = ed.get_carets()[0]
        ed.insert(caret[0], caret[1], s)
        msg_status('Date/time inserted')
        
        
