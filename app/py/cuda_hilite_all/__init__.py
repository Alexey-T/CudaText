import string
from cudatext import *

if app_api_version()<'1.0.114':
    msg_box('Hilite Occurrences needs newer app version', MB_OK+MB_ICONWARNING)


MIN_LEN = 2
COLOR_FONT = 0x000000
COLOR_BG = 0x80FFFF
WORDS_ONLY = True
CHARS = string.ascii_letters + string.digits + '_'
MARKTAG = 11 #uniq value for all markers plugins


def is_word(s):
    for ch in s:
        if not ch in CHARS:
            return False
    return True
        

def do_find_all(ed, text):
    if not is_word(text): return
    
    res = []
    for i in range(ed.get_line_count()):
        line = ed.get_text_line(i)
        if not line: continue
        
        n = 0
        while True:
            n = line.find(text, n)
            if n<0: break
            allow = True
            if WORDS_ONLY:
                if n>0 and is_word(line[n-1]): 
                    allow = False
                if allow:
                    n2 = n+len(text)
                    if n2<len(line) and is_word(line[n2]): 
                        allow = False
            if allow:
                res += [(i, n)]
            n += len(text)   
    return res


class Command:
    def on_caret(self, ed_self):
        ed.attr(MARKERS_DELETE_BY_TAG, MARKTAG)
        
        text = ed.get_text_sel()
        if not text: return
        if '\n' in text: return #no multiline
        if len(text)<MIN_LEN: return
        
        carets = ed.get_carets()
        if len(carets)!=1: return
        
        x0, y0, x1, y1 = carets[0]
        if x0>x1:
            x0, x1 = x1, x0
        
        items = do_find_all(ed, text)
        if not items: return
        if len(items)<2: return
        
        for item in items:
            if item==(y0, x0): continue
            ed.attr(MARKERS_ADD, MARKTAG, item[1], item[0], len(text),
                    COLOR_FONT, COLOR_BG)
                    
        msg_status('Matches hilited: %d' % len(items))
        
