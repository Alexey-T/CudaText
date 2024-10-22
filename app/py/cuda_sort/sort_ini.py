# original author: Michal Niklas
# adapted to CudaText: Alexey Torgashin

from cudatext import ed

def ini_sort_content(lines, and_keys, case_sens):

    if case_sens:
        sortkey = lambda s: s
    else:
        sortkey = str.casefold

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
        sk.sort(key=sortkey)
        for k in sk:
            vals = sections[k]
            if and_keys:
                vals.sort(key=sortkey)
            res += [k]
            res += vals
            res += ['']
        return res


def ini_sort(and_keys, case_sens):

    lines = ed.get_text_all().splitlines()
    if not lines: return
    lines = ini_sort_content(lines, and_keys, case_sens)
    if not lines: return
    ed.set_caret(0, 0)
    ed.replace_lines(0, ed.get_line_count()-1, lines)
    return True
