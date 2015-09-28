from cudatext import *
import base64
import quopri
from .uni_escape import *

def run(method):
    ENC = 'utf8'
    posx, posy, endx, endy = ed.get_carets()[0]
    if endx<0:
        msg_status('Text not selected')
        return
        
    #swap coords    
    if (posy>endy) or ((posy==endy) and (posx>endx)):
        posx, posy, endx, endy = endx, endy, posx, posy
        
    s = ed.get_text_sel()
    s = bytes(s, ENC)
    s = method(s)
    s = s.decode(ENC)

    ed.delete(posx, posy, endx, endy)
    newx, newy = ed.insert(posx, posy, s)    
    ed.set_caret(newx, newy, posx, posy)

    msg_status('Base Encoder: '+method.__name__)

class Command:
    def base64_encode(self):
        run(base64.b64encode)
    def base64_decode(self):
        run(base64.b64decode)

    def base32_encode(self):
        run(base64.b32encode)
    def base32_decode(self):
        run(base64.b32decode)

    def base16_encode(self):
        run(base64.b16encode)
    def base16_decode(self):
        run(base64.b16decode)

    def quotedpri_encode(self):
        run(quopri.encodestring)
    def quotedpri_decode(self):
        run(quopri.decodestring)

    def uniescape_encode(self):
        run(uni_encode)
    def uniescape_decode(self):
        run(uni_decode)
