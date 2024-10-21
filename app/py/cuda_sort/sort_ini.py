# original author: Michal Niklas
# adapted to CudaText: Alexey Torgashin

import os
from cudatext import *
from cudax_lib import get_translation

_   = get_translation(__file__)  # I18N

CONFIG_FN = os.path.join(app_path(APP_DIR_SETTINGS), 'plugins.ini')
CONFIG_SECTION = 'sort'

def ini_sort_content(lines, and_keys):

    case_sens = ini_read(CONFIG_FN, CONFIG_SECTION, 'ini_files_case_sensitive', '0')=='1'
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
