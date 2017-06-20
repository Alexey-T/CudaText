import os
from cudatext import *

MASKS_IGNORE = '.rar .exe .dll .git .svn'
MASKS_ZIP = '.zip .7z .tar .gz .rar .xz'
MASKS_IMAGES = '.png .jpg .jpeg .gif .bmp .ico'
MASKS_BINARY = '.exe .dll .o'


def dialog_config(op):

    id_ignore = 1
    id_recents = 3
    id_on_start = 4
    id_toolbar = 5
    id_ok = 6

    c1 = chr(1)
    text = '\n'.join([]
        +[c1.join(['type=label', 'pos=6,6,500,0', 'cap=&File/folder masks to ignore (space-separated):'])]
        +[c1.join(['type=edit', 'pos=6,24,500,0',
            'val='+op.get('masks_ignore', MASKS_IGNORE)])]
        +[c1.join(['type=label', 'pos=6,54,500,0', 'cap=&Recent projects:'])]
        +[c1.join(['type=memo', 'pos=6,74,500,180',
            'val='+'\t'.join(op.get('recent_projects', [])) ])]
        +[c1.join(['type=check', 'pos=6,186,400,0', 'cap=&Load on program start',
            'val='+('1' if op.get('on_start', False) else '0') ])]
        +[c1.join(['type=check', 'pos=6,210,400,0', 'cap=&Show toolbar',
            'val='+('1' if op.get('toolbar', True) else '0') ])]
        +[c1.join(['type=button', 'pos=300,260,400,0', 'cap=&OK', 'props=1'])]
        +[c1.join(['type=button', 'pos=406,260,502,0', 'cap=Cancel'])]
    )

    res = dlg_custom('Project Manager options', 508, 290, text, get_dict=True)
    if res is None:
        return

    if res['clicked'] != id_ok:
        return

    s = res[id_ignore].strip()
    while '  ' in s:
        s = s.replace('  ', ' ')
    op['masks_ignore'] = s

    s = res[id_recents].split('\t')
    op['recent_projects'] = s

    op['on_start'] = res[id_on_start]=='1'
    op['toolbar'] = res[id_toolbar]=='1'

    return True


def dialog_proj_prop(prop):

    list_vars = prop.get('vars', '')
    main_file = prop.get('mainfile', '')

    id_vars = 1
    id_ok = 4

    c1 = chr(1)
    text = '\n'.join([]
        +[c1.join(['type=label', 'pos=6,6,500,0', 'cap=&Variables (in form Name=Value)'])]
        +[c1.join(['type=memo', 'pos=6,26,500,180', 'val='+'\t'.join(list_vars) ])]
        +[c1.join(['type=label', 'pos=6,186,500,0', 'cap=&Main file (read-only, change in context menu)'])]
        +[c1.join(['type=edit', 'pos=6,206,500,0', 'props=1,0,1', 'val='+main_file])]
        +[c1.join(['type=button', 'pos=300,300,400,0', 'cap=&OK', 'props=1'])]
        +[c1.join(['type=button', 'pos=406,300,502,0', 'cap=Cancel'])]
    )

    res = dlg_custom('Project properties', 508, 330, text)
    if res is None:
        return

    res, text = res
    text = text.splitlines()

    if res != id_ok:
        return

    s = text[id_vars].split('\t')
    s = [item.strip() for item in s if '=' in item]
    prop['vars'] = s

    return True
