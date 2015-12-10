
#example of chars: "()[]{}"
def find_matching_bracket(ed, from_x, from_y, chars):
    line = ed.get_text_line(from_y)
    if not from_x in range(len(line)): return
    ch = line[from_x]
    pos = chars.find(ch)
    if pos<0: return
    
    if pos%2==0:
        ch_end = chars[pos+1]
        down = True
    else:
        ch_end = chars[pos-1]
        down = False 

    to_x = from_x
    to_y = from_y
    cnt = 0
    #print('find "%s" from (%d,%d)'%(ch,to_x,to_y))
    
    while True:
        for pos in (range(to_x, len(line)) if down else
                    range(to_x, -1, -1)):
            ch_now = line[pos]
            if ch_now==ch:
                cnt+=1
            elif ch_now==ch_end:
                cnt-=1
                if cnt==0:
                    return (pos, to_y)
                    
        if down:
            to_y+=1
        else:
            to_y-=1
        line = ed.get_text_line(to_y)
        if line is None: return
        if down:
            to_x=0
        else:
            to_x=len(line)-1
