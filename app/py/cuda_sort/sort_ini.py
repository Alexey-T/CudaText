import os
from cudatext import *
from cudax_lib import get_translation

_   = get_translation(__file__)  # I18N

# author: Michal Niklas
# adapted to CudaText: Alexey T.

fn_ini = os.path.join(app_path(APP_DIR_SETTINGS), 'plugins.ini')
op_section = 'sort'
op_ini_case = 'ini_files_case_sensitive'

def ini_sort_content(lines, and_keys):

    case_sens = ini_read(fn_ini, op_section, op_ini_case, '0')=='1'
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


def ini_sort(and_keys):
    lines = ed.get_text_all().splitlines()
    if not lines: return
    lines = ini_sort_content(lines, and_keys)
    if not lines: return
    ed.set_text_all('\n'.join(lines))
    ed.set_caret(0, 0)
    msg_status(_('Sorted ini lines'))
