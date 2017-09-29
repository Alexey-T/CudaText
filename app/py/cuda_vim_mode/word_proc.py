from cudatext import *
import cudatext_cmd as cc
import string

WORD_CHARS = string.ascii_letters + string.digits + '_'

def get_word_info(x0, y0):
    text = ed.get_text_line(y0)
    if not text:
        return (x0, y0, 0, '')
    if x0>len(text):
        x0 = len(text)

    x1 = x0
    while x1>0 and text[x1-1] in WORD_CHARS: x1-=1
    x2 = x0
    while x2<len(text) and text[x2] in WORD_CHARS: x2+=1

    return (x1, y0, x2-x1, text[x1:x2])


def goto_word_end():
    x0, y0, x1, y1 = ed.get_carets()[0]
    s = ed.get_text_line(y0)
    if not (0<=x0<len(s)):
        return

    on_word = s[x0] in WORD_CHARS
    if not on_word:
        ed.cmd(cc.cCommand_GotoWordNext)
        x0, y0, x1, y1 = ed.get_carets()[0]

    info = get_word_info(x0, y0)
    if not info: return
    xw, yw, nlen, str = info

    #at word end already?
    if xw+nlen-1 == x0:
        ed.cmd(cc.cCommand_KeyRight)
        ed.cmd(cc.cCommand_GotoWordNext)
        x0, y0, x1, y1 = ed.get_carets()[0]

        info = get_word_info(x0, y0)
        if not info: return
        xw, yw, nlen, str = info

    ed.set_caret(xw+nlen-1, yw)
