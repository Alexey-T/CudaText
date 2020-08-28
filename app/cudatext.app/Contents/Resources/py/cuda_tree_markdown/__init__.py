import itertools
import re

RE_TICKS = '^\s*`{3,}\s*\w*$'
re_ticks = re.compile(RE_TICKS, 0)


def _is_pre(s, ch, need_space):
    if not s.startswith(ch):
        return
    r = len(list(itertools.takewhile(lambda c: ch == c, s)))
    if not need_space:
        return r
    if r < len(s) and s[r].isspace():
        return r


def is_line_ticks(s):
    return re_ticks.match(s) is not None 


def is_line_head(s):
    return _is_pre(s, '#', True)


def is_line_after_head(s, char):
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
    for i, s in enumerate(lines):
        if not s.strip():
            continue
        r = is_line_ticks(s)
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

        r = is_line_head(s)
        if r:
            res.append( ((0, i, 0, i+1), r, s.strip(' #'), -1) )
        else:
            if i+1 < len(lines) and \
                not s.startswith('-') and \
                not s.startswith('='):
                s2 = lines[i+1]
                if is_line_after_head(s2, '='):
                    res.append( ((0, i, 0, i+1), 1, s, -1) )
                elif is_line_after_head(s2, '-'):
                    res.append( ((0, i, 0, i+1), 2, s, -1) )

    return res
