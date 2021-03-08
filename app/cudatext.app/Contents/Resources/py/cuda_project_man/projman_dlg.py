import os
from cudatext import *
from cudax_lib import get_translation
from .projman_glob import *

_   = get_translation(__file__)  # i18n


def bool_to_str(b):
    return '1' if b else '0'
def str_to_bool(s):
    return s=='1'


def get_themes_filetype():

    dir = os.path.join(app_path(APP_DIR_DATA), 'filetypeicons')
    return sorted(os.listdir(dir))

def get_themes_toolbar():

    dir = os.path.join(app_path(APP_DIR_DATA), 'projtoolbaricons')
    return sorted(os.listdir(dir))


def dialog_config(op):

    RES_NO_FILES = 1
    RES_NO_DIRS = 3
    RES_NO_HIDDEN = 4
    RES_RECENTS = 6
    RES_ON_START = 7
    RES_TOOLBAR = 8
    RES_GOTO_OPEN = 9
    RES_PREVIEW = 10
    RES_D_CLICK = 11
    RES_CHECK_GIT = 12
    RES_ICONS = 14
    RES_ICONS_TB = 16
    RES_OK = 19

    themes = get_themes_filetype()
    try:
        s = op.get('icon_theme', 'vscode_16x16')
        theme_index = themes.index(s)
    except:
        theme_index = -1

    themes_tb = get_themes_toolbar()
    try:
        s = op.get('toolbar_theme', 'default_16x16')
        theme_index_tb = themes_tb.index(s)
    except:
        theme_index_tb = -1

    c1 = chr(1)
    text = '\n'.join([]
        +[c1.join(['type=label', 'pos=6,4,110,0', 'cap='+_('Ignore &files:')])]
        +[c1.join(['type=edit', 'pos=116,4,500,0', 'val='+op.get('no_files', '')])]

        +[c1.join(['type=label', 'pos=6,34,110,0', 'cap='+_('Ignore fold&ers:')])]
        +[c1.join(['type=edit', 'pos=116,34,500,0', 'val='+op.get('no_dirs', '.git;.svn')])]

        +[c1.join(['type=check', 'pos=6,62,500,84', 'cap='+_('Ignore all &hidden files/folders'),
          'val='+bool_to_str(op.get('no_hidden', True))])]
        
        +[c1.join(['type=label', 'pos=6,88,500,0', 'cap='+_('&Recent projects:')])]
        +[c1.join(['type=memo', 'pos=6,104,500,180',
            'val='+'\t'.join(op.get('recent_projects', [])) ])]
        +[c1.join(['type=check', 'pos=6,186,400,0', 'cap='+_('&Load on app start, reopen last project (*)'),
            'val='+bool_to_str(op.get('on_start', False)) ])]
        +[c1.join(['type=check', 'pos=6,210,400,0', 'cap='+_('&Show toolbar'),
            'val='+bool_to_str(op.get('toolbar', True)) ])]
        +[c1.join(['type=check', 'pos=6,236,400,0', 'cap='+_('Open file after "&Go to file" command'),
            'val='+bool_to_str(op.get('goto_open', False)) ])]
        +[c1.join(['type=check', 'pos=6,262,400,0', 'cap='+_('&Use "preview tab" on item clicking'),
            'val='+bool_to_str(op.get('preview', True)) ])]
        +[c1.join(['type=check', 'pos=6,288,400,0', 'cap='+_('Open files by &double-click'),
            'val='+bool_to_str(op.get('d_click', False)) ])]
        +[c1.join(['type=check', 'pos=6,314,400,0', 'cap='+_('On opening file in Git/SVN repo, create project from repo (*)'),
            'val='+bool_to_str(op.get('check_git', True)) ])]

        +[c1.join(['type=label', 'pos=6,360,130,0', 'cap='+_('File type icons:')])]
        +[c1.join(['type=combo_ro', 'pos=160,355,400,0',
            'items='+'\t'.join(themes),
            'val='+str(theme_index)
            ])]

        +[c1.join(['type=label', 'pos=6,390,130,0', 'cap='+_('Toolbar icons:')])]
        +[c1.join(['type=combo_ro', 'pos=160,385,400,0',
            'items='+'\t'.join(themes_tb),
            'val='+str(theme_index_tb)
            ])]

        +[c1.join(['type=label', 'pos=6,416,600,0', 'cap='+_('For more icons, get add-ons of kind "filetypeicons", "projtoolbaricons"')])]
        +[c1.join(['type=label', 'pos=6,440,600,0', 'cap='+_('(*) - requires CudaText restart')])]
        +[c1.join(['type=button', 'pos=300,470,400,0', 'cap='+_('&OK'), 'ex0=1'])]
        +[c1.join(['type=button', 'pos=406,470,502,0', 'cap='+_('Cancel')])]
    )

    res = dlg_custom(_('Project Manager options'), 508, 504, text, get_dict=True)
    if res is None:
        return

    if res['clicked'] != RES_OK:
        return

    op['no_files'] = res[RES_NO_FILES].strip()
    op['no_dirs'] = res[RES_NO_DIRS].strip()
    op['no_hidden'] = str_to_bool(res[RES_NO_HIDDEN])

    s = res[RES_RECENTS].split('\t')
    op['recent_projects'] = s

    op['on_start'] = str_to_bool(res[RES_ON_START])
    op['toolbar'] = str_to_bool(res[RES_TOOLBAR])
    op['goto_open'] = str_to_bool(res[RES_GOTO_OPEN])
    op['preview'] = str_to_bool(res[RES_PREVIEW])
    op['d_click'] = str_to_bool(res[RES_D_CLICK])
    op['check_git'] = str_to_bool(res[RES_CHECK_GIT])

    index = int(res[RES_ICONS])
    if index>=0:
        op['icon_theme'] = themes[index]

    index = int(res[RES_ICONS_TB])
    if index>=0:
        op['toolbar_theme'] = themes_tb[index]

    return True


def dialog_proj_prop(prop):

    list_vars = prop.get('vars', '')
    main_file = prop.get('mainfile', '')

    RES_VARS = 1
    RES_OK = 4

    c1 = chr(1)
    text = '\n'.join([]
        +[c1.join(['type=label', 'pos=6,6,500,0', 'cap='+_('&Variables (in form Name=Value)')])]
        +[c1.join(['type=memo', 'pos=6,26,500,180', 'val='+'\t'.join(list_vars) ])]
        +[c1.join(['type=label', 'pos=6,186,500,0', 'cap='+_('&Main file (read-only, change in context menu)')])]
        +[c1.join(['type=edit', 'pos=6,206,500,0', 'ex0=1', 'ex1=0', 'ex2=1', 'val='+main_file])]
        +[c1.join(['type=button', 'pos=300,300,400,0', 'cap='+_('&OK'), 'ex0=1'])]
        +[c1.join(['type=button', 'pos=406,300,502,0', 'cap='+_('Cancel')])]
    )

    res = dlg_custom(_('Project properties'), 508, 330, text, get_dict=True)
    if res is None:
        return

    if res['clicked'] != RES_OK:
        return

    s = res[RES_VARS].split('\t')
    s = [item.strip() for item in s if '=' in item]
    prop['vars'] = s

    return True
