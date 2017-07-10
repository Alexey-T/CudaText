import os
from cudatext import *

MASKS_IGNORE = '.rar .exe .dll .git .svn'
MASKS_ZIP = '.zip .7z .tar .gz .rar .xz .cab .deb .rpm'
MASKS_IMAGES = '.png .jpg .jpeg .gif .bmp .ico'
MASKS_BINARY = '.exe .dll .o .msi .lib .obj .pdf'


def get_themes_list():

    dir = os.path.join(app_path(APP_DIR_DATA), 'filetypeicons')
    return sorted(os.listdir(dir))


def dialog_config(op):

    RES_IGNORE = 1
    RES_RECENTS = 3
    RES_ON_START = 4
    RES_TOOLBAR = 5
    RES_GOTO_OPEN = 6
    RES_ICONS = 8
    RES_OK = 9

    themes = get_themes_list()
    try:
        s = op.get('icon_theme', 'vscode_16x16')
        theme_index = themes.index(s)
    except:
        theme_index = -1

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
        +[c1.join(['type=check', 'pos=6,236,400,0', 'cap=&Open file after "Go to file" command',
            'val='+('1' if op.get('goto_open', False) else '0') ])]

        +[c1.join(['type=label', 'pos=6,270,130,0', 'cap=Icons theme:'])]    
        +[c1.join(['type=combo_ro', 'pos=130,265,350,0', 
            'items='+'\t'.join(themes),
            'val='+str(theme_index)
            ])]    

        +[c1.join(['type=button', 'pos=300,310,400,0', 'cap=&OK', 'props=1'])]
        +[c1.join(['type=button', 'pos=406,310,502,0', 'cap=Cancel'])]
    )

    res = dlg_custom('Project Manager options', 508, 344, text, get_dict=True)
    if res is None:
        return

    if res['clicked'] != RES_OK:
        return

    s = res[RES_IGNORE].strip()
    while '  ' in s:
        s = s.replace('  ', ' ')
    op['masks_ignore'] = s

    s = res[RES_RECENTS].split('\t')
    op['recent_projects'] = s

    op['on_start'] = res[RES_ON_START]=='1'
    op['toolbar'] = res[RES_TOOLBAR]=='1'
    op['goto_open'] = res[RES_GOTO_OPEN]=='1'
    
    index = int(res[RES_ICONS])
    if index>=0:
        op['icon_theme'] = themes[index] 

    return True


def dialog_proj_prop(prop):

    list_vars = prop.get('vars', '')
    main_file = prop.get('mainfile', '')

    RES_VARS = 1
    RES_OK = 4

    c1 = chr(1)
    text = '\n'.join([]
        +[c1.join(['type=label', 'pos=6,6,500,0', 'cap=&Variables (in form Name=Value)'])]
        +[c1.join(['type=memo', 'pos=6,26,500,180', 'val='+'\t'.join(list_vars) ])]
        +[c1.join(['type=label', 'pos=6,186,500,0', 'cap=&Main file (read-only, change in context menu)'])]
        +[c1.join(['type=edit', 'pos=6,206,500,0', 'props=1,0,1', 'val='+main_file])]
        +[c1.join(['type=button', 'pos=300,300,400,0', 'cap=&OK', 'props=1'])]
        +[c1.join(['type=button', 'pos=406,300,502,0', 'cap=Cancel'])]
    )

    res = dlg_custom('Project properties', 508, 330, text, get_dict=True)
    if res is None:
        return

    if res['clicked'] != RES_OK:
        return

    s = res[RES_VARS].split('\t')
    s = [item.strip() for item in s if '=' in item]
    prop['vars'] = s

    return True
