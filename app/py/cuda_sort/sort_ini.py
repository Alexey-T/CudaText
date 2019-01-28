from cudatext import *

__version__ = '$Id: sort_ini.py 543 2008-12-19 13:44:59Z mn $'
# author: Michal Niklas
# adapted to CudaText: Alexey T.

def ini_sort_content(lines, and_keys):
    section = ''
    sections = {}
    for line in lines:
        line = line.strip()
        if line:
            if line.startswith('['):
                section = line
                continue
            if section:
                try:
                    sections[section].append(line)
                except KeyError:
                    sections[section] = [line, ]
    if sections:
        res = []
        sk = list(sections.keys())
        sk.sort()
        for k in sk:
            vals = sections[k]
            if and_keys:
                vals.sort()
            res += [k]
            res += vals
            res += ['']
        return res


def ini_sort(and_keys):
    lines = ed.get_text_all().splitlines()
    if not lines: return
    lines = ini_sort_content(lines, and_keys)
    if not lines: return
    ed.set_text_all('\n'.join(lines))
    ed.set_caret(0, 0)
    msg_status('Sorted ini lines')
