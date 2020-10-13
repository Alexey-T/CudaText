from cudatext import *

def _sort(s, sep_k, sep_v):
    
    if not sep_k in s:
        return s
    key, val = s.split(sep_k, 1)
    vals = sorted(val.split(sep_v))
    return key+sep_k+sep_v.join(vals)

def do_sort_sep_values():

    res = dlg_input_ex(2, 
        'Sort: separator chars',
        'Separator of prefix key, to skip prefix:', '=',
        'Separator of list after prefix:', ',')
    if res is None:
        return
    sep_k, sep_v = res    

    cnt = 0    
    for i in range(ed.get_line_count()):
        s = ed.get_text_line(i)
        s2 = _sort(s, sep_k, sep_v)
        if s!=s2:
            ed.set_text_line(i, s2)
            cnt += 1

    if cnt>0:
        msg_status('Sorted, changed %d line(s)'%cnt)
    else:
        msg_status('Lines are already sorted')
