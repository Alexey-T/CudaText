from cudatext import *
from .colorcodes import *
import string

chars = string.ascii_letters + string.digits + '#'

def get_info():
    x0, y0, x2, y2 = ed.get_carets()[0]
    text = ed.get_text_line(y0)
    if not text:
        return (x0, y0, 0, '')
    if x0>len(text):
        x0 = len(text)
    
    x1 = x0
    while x1>0 and text[x1-1] in chars: x1-=1
    x2 = x0
    while x2<len(text) and text[x2] in chars: x2+=1

    return (x1, y0, x2-x1, text[x1:x2])

class Command:
    def run(self):
        if app_api_version()<'1.0.101':
            msg_box('Plugin needs newer app version', MB_OK+MB_ICONERROR)
            return
    
        x0, y0, nlen, text = get_info()
        
        val = 0
        if text:
            try:
                val = HTMLColorToPILColor(text)
            except:
                val = 0
                
        val = dlg_color(val)
        if val is None: return
        val = PILColorToHTMLColor(val)

        ed.delete(x0, y0, x0+nlen, y0)
        ed.insert(x0, y0, val)
        msg_status('Inserted color: '+val)
