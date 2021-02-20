
def is_head(s):
    if not s.startswith('#'):
        return
    r = 0
    while (r < len(s)) and (s[r] == '#'):
        r += 1
    #if not need_space:
    #    return r
    if r < len(s) and s[r].isspace():
        return r


def is_ticks(s):
    #regex '^\s*`{3,}\s*\w*$'
    if not s:
        return
    i = 0
    while (i<len(s)) and (s[i] in ' \t'):
        i += 1
    r = 0
    while (i<len(s)) and (s[i]=='`'):
        r += 1
        i += 1
    return r >= 3


def is_after_head(s, char):
    if not s:
        return False
    for ch in s:
        if ch != char:
            return False
    return True


def get_headers(filename, lines):
    '''
    Gets list of nodes in format:
    ((x1, y1, x2, y2), level, title, icon)
    '''
    res = []
    tick = False
    tick_r = 0
    pre = False

    for i, s in enumerate(lines):
        s0 = s.strip()
        if not s0:
            continue
        if s0=='<pre>':
            pre = True
            continue
        if s0=='</pre>':
            pre = False
            continue
        if pre:
            continue

        r = is_ticks(s)
        if r:
            if tick and r == tick_r:
                tick = False
                tick_r = 0
            else:
                tick = True
                tick_r = r
            continue
        if tick:
            continue

        r = is_head(s)
        if r:
            res.append( ((0, i, 0, i+1), r, s.strip(' #'), -1) )
        else:
            if i+1 < len(lines) and \
                not s.startswith('-') and \
                not s.startswith('='):
                s2 = lines[i+1]
                if is_after_head(s2, '='):
                    res.append( ((0, i, 0, i+1), 1, s, -1) )
                elif is_after_head(s2, '-'):
                    res.append( ((0, i, 0, i+1), 2, s, -1) )

    return res
