import cudatext as ct
import cudatext_cmd


def insert_snip_into_editor(ed, snip_lines):
    items = list(snip_lines) #copy list value
    if not items: return
    
    carets = ed.get_carets()
    if len(carets)!=1: return
    x0, y0, x1, y1 = carets[0]

    tab_spaces = ed.get_prop(ct.PROP_TAB_SPACES)
    tab_size = ed.get_prop(ct.PROP_TAB_SIZE)
    
    #apply indent to lines from second
    x_col, y_col = ed.convert(ct.CONVERT_CHAR_TO_COL, x0, y0)
    indent = ' '*x_col
    
    if not tab_spaces:
        indent = indent.replace(' '*tab_size, '\t')
    
    for i in range(1, len(items)):
        items[i] = indent+items[i]

    #replace tab-chars
    if tab_spaces:
        indent = ' '*tab_size
        items = [item.replace('\t', indent) for item in items]

    #parse tabstops ${0}, ${0:text}
    stops = []
    for index in range(len(items)):
        s = items[index]
        while True:
            digit = 0
            deftext = ''
            
            n = s.find('${')
            if n<0: break
            if n+3>=len(s): break
            if not s[n+3] in ':}': break
            try:
                digit = int(s[n+2])
            except:
                break
                
            #text in tabstop    
            if s[n+3]==':':
                deftext = s[n+4:]
                nn = deftext.find('}')
                if nn<0: break
                deftext = deftext[:nn] 
                s = s[:n]+deftext+s[n+5+nn:]
            else:
                s = s[:n]+s[n+4:]
                
            stops += [(digit, deftext, index, n)]
            items[index] = s
    #print('tabstops', stops)        
    
    #insert    
    ed.insert(x0, y0, '\n'.join(items))
    
    #place markers
    mark_placed = False
    ed.markers(ct.MARKERS_DELETE_ALL)

    for digit in [0,9,8,7,6,5,4,3,2,1]:
        for stop in reversed(stops):
            if stop[0]==digit:
                pos_x = stop[3]
                pos_y = stop[2]
                if pos_y==0:
                    pos_x += x0
                pos_y += y0
                deftext = stop[1]
                ed.markers(ct.MARKERS_ADD, pos_x, pos_y, digit, len(deftext))
                mark_placed = True
    
    if mark_placed:
        ed.set_prop(ct.PROP_TAB_COLLECT_MARKERS, '1')
        ed.cmd(cudatext_cmd.cmd_Markers_GotoLastAndDelete)
