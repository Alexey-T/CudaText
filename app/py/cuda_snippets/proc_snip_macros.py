import os
from datetime import datetime

MACRO_SEL = '${sel}'
MACRO_CLIP = '${cp}'
MACRO_FILENAME = '${fname}'
MACRO_DATE = '${date:' #no }

def snip_replace_macros_in_lines(items, text_sel, text_clip, text_filename):
    for index in range(len(items)):
        s = items[index]
        while True:
            n = s.find(MACRO_SEL)
            if n<0: break
            s = s[:n]+text_sel+s[n+len(MACRO_SEL):]
            
        while True:
            n = s.find(MACRO_CLIP)
            if n<0: break
            s = s[:n]+text_clip+s[n+len(MACRO_CLIP):]
            
        while True:
            n = s.find(MACRO_FILENAME)
            if n<0: break
            s = s[:n]+text_filename+s[n+len(MACRO_FILENAME):]
            
        while True:
            n = s.find(MACRO_DATE)
            if n<0: break
            text_date = s[n:]
            nn = text_date.find('}')
            if nn<0: break
            text_date = text_date[len(MACRO_DATE):nn]
            text_date = datetime.now().strftime(text_date)            
            s = s[:n]+text_date+s[n+nn+1:]
        
        if items[index]!=s:
            items[index] = s
