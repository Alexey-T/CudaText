import os
import webbrowser
from cudatext import *

temp_fn = '_cudatext_preview.html'

class Command:
    names = []
    
    def run(self):
        fn = ed.get_filename()                  
        if not fn:
            msg_box('Cannot do preview in untitled tab', MB_OK)
            return

        #if selection- write it to file
        text = ed.get_text_sel()
        if text:
            fn = os.path.join(os.path.dirname(fn), temp_fn)
            if os.path.isfile(fn):
                os.remove(fn)
            with open(fn, 'w') as f:
                f.write(text)
                
        if not os.path.isfile(fn):
            msg_status('Cannot open file: '+fn)
            return
            
        self.names.append(fn)
        webbrowser.open_new_tab(fn)
        msg_status('Opened preview')
