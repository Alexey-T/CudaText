from cudatext import *
from .colorcodes import *
from .word_proc import *       

class Command:
    def run(self):
        if app_api_version()<'1.0.101':
            msg_box('Plugin needs newer app version', MB_OK)
            return
    
        x0, y0, nlen, text = get_word_info()
        
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
