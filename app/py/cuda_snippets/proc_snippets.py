import os
import sys
import string

CHARS_SNIP = string.ascii_letters + string.digits + '_'
CHARS_ALLOWED_AFTER_SNIP = ' \t<>'

SNIP_NAME='name'
SNIP_ID='id'
SNIP_LEX='lex'
SNIP_TEXT='text'


def get_snip_name_from_editor(ed):
    #multi-carets? stop
    carets=ed.get_carets() 
    if len(carets)!=1: return
    x, y, x1, y1 = carets[0]
    
    #selection? stop
    if y1>=0: return
    #check line index 
    if y>=ed.get_line_count(): return
    
    line = ed.get_text_line(y)
    #caret after lineend? stop
    if x>len(line): return 
    #caret on incorr char? stop
    if x<len(line) and not line[x] in CHARS_ALLOWED_AFTER_SNIP: return
    
    x0=x
    while (x>0) and (line[x-1] in CHARS_SNIP): x-=1
    return line[x:x0]


def parse_snip_content_to_dict(text):
    res={SNIP_NAME:'', SNIP_ID:'', SNIP_LEX:'', SNIP_TEXT:'' }
    lines=text.splitlines()

    for (index, line) in enumerate(lines):
        if line==SNIP_TEXT+'=':
            res[SNIP_TEXT] = [lines[i] for i in range(index+1, len(lines))]
            break
            
        for prefix in [SNIP_NAME, SNIP_ID, SNIP_LEX]:
            if line.startswith(prefix+'='):
                res[prefix] = line[len(prefix)+1:]

    return res


def get_filelist_with_subdirs(dir):
    res=[]
    for root, subdirs, files in os.walk(dir):
        for f in files:
            res.append(os.path.join(root, f))
    return res


def get_snip_list_of_dicts(dir):
    res=get_filelist_with_subdirs(dir)
    res=[parse_snip_content_to_dict(open(fn).read()) for fn in res]
    res=sorted(res, key=lambda d: d[SNIP_NAME])
    return res
