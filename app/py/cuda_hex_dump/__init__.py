from cudatext import *
from .hexdump import hexdump

def do_dump(enc):
    x0, y0, x1, y1 = ed.get_carets()[0]
    if x1>=0:
        s = ed.get_text_sel()
    else:
        s = ed.get_text_all()
    
    try:
        data = bytes(s, enc)
    except:
        msg_box('Hex Dump: cannot convert to ASCII such Unicode text', MB_OK)
        return
    
    s = hexdump(data, 'return')
    file_open('')
    ed.set_text_all(s)

class Command:
    def dump_unicode(self):
        do_dump('utf16')
        
    def dump_dos(self):
        do_dump('ascii')
