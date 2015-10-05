from cudatext import *
from datetime import datetime
import os
import shutil

class Command:
    def run(self):
        fn = ed.get_filename()
        if not fn:
            msg_status('Cannot backup untitled tab')
            return
        
        t = datetime.now()
        bak0 = '.bak'
        bak1 = '.{:04}-{:02}-{:02}.bak'.format(t.year, t.month, t.day)
        bak2 = '.{:04}-{:02}-{:02}-{:02}h-{:02}m-{:02}s.bak'.format(t.year, t.month, t.day, t.hour, t.minute , t.second)
        suffix = bak0 if not os.path.isfile(fn+bak0) else bak1 if not os.path.isfile(fn+bak1) else bak2
        shutil.copyfile(fn, fn+suffix)
        msg_status('Backup: '+fn+suffix)
