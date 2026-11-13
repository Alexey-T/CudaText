import os
from cudatext import *
from cudax_lib import get_translation
from .projman_glob import *
from .projman_backup import DEFAULT_BACKUP_COUNT, DEFAULT_BACKUP_FOLDER

_   = get_translation(__file__)  # i18n


def get_themes_filetype():

    dir = os.path.join(app_path(APP_DIR_DATA), 'filetypeicons')
    return sorted(os.listdir(dir))

def get_themes_toolbar():

    dir = os.path.join(app_path(APP_DIR_DATA), 'projtoolbaricons')
    return sorted(os.listdir(dir))


def _val_bool(v):
    """Boolean value of a check control ('1'/'0', or a bool on some
    builds)."""
    return str(v) in ('1', 'True', 'true')


def _val_int(v):
    """Int value of a combo/edit control, or -1 (a bad index must never
    crash the dialog handler)."""
    try:
        return int(str(v))
    except (TypeError, ValueError):
        return -1


def dialog_config(op):
    """Project Manager options dialog, built with dlg_proc (not
    dlg_custom): the settings are split into visually SEPARATED
    sections, the long hints are split into short lines that fit the
    window width, and the layout is compact. The dialog API has no
    scrollable panel, so a compact grouped layout is the way to survive
    small screens. Returns True when OK was pressed."""

    themes = get_themes_filetype()
    try:
        theme_index = themes.index(op.get('icon_theme', 'vscode_16x16'))
    except ValueError:
        theme_index = -1

    themes_tb = get_themes_toolbar()
    try:
        theme_index_tb = themes_tb.index(op.get('toolbar_theme', 'default_16x16'))
    except ValueError:
        theme_index_tb = -1

    items_sort_id = [
        '',
        'name',
        'ext',
        'size',
        'size-',
        'mtime',
        'mtime-',
        'ctime',
        'ctime-',
        ]
    items_sort_str = [
        _('none'),
        _('by name'),
        _('by extension'),
        _('by size'),
        _('by size, descending'),
        _('by date'),
        _('by date, descending'),
        _('by creation date'),
        _('by creation date, descending'),
        ]
    s = op.get('sort_order', 'ext')
    items_sort_val = items_sort_id.index(s if s in items_sort_id else 'ext')

    W = 640
    BH = app_proc(PROC_GET_GUI_HEIGHT, 'button') or 30

    h = dlg_proc(0, DLG_CREATE)
    dlg_proc(h, DLG_PROP_SET, prop={
        'cap': _('Project Manager options'),
        'w': W,
        'h': 120,               # the real height is set after the layout
        'border': DBORDER_DIALOG,
    })

    state = {'ok': False}
    # control names read back when OK is pressed
    READ_NAMES = ('no_files', 'no_dirs', 'no_hidden', 'recent',
                  'toolbar', 'goto_open', 'preview', 'd_click', 'always_sync',
                  'icon_theme', 'toolbar_theme', 'sort_order',
                  'backup_enabled', 'backup_folder', 'backup_count',
                  'projects_dir')

    def ctl(kind, name, prop):
        n = dlg_proc(h, DLG_CTL_ADD, prop=kind)
        if n is None:
            return None
        prop = dict(prop)
        prop['name'] = name
        dlg_proc(h, DLG_CTL_PROP_SET, index=n, prop=prop)
        return n

    def get_val(name):
        try:
            props = dlg_proc(h, DLG_CTL_PROP_GET, name=name) or {}
            return props.get('val', '')
        except Exception:
            return ''

    def on_ok(id_dlg, id_ctl, data='', info=''):
        # read the values while the dialog is definitely alive
        state['ok'] = True
        for nm in READ_NAMES:
            state[nm] = get_val(nm)
        dlg_proc(h, DLG_HIDE)

    def on_cancel(id_dlg, id_ctl, data='', info=''):
        state['ok'] = False
        dlg_proc(h, DLG_HIDE)

    #--------- small layout helpers (absolute coords, DLG_SCALE after)

    X0 = 12          # left margin
    LW = 175         # width of the label column
    EX = X0 + LW + 10
    EW = W - EX - X0

    y = [10]         # mutable layout cursor
    n_lab = [0]

    def lab(cap, x, w, dy=16):
        n_lab[0] += 1
        ctl('label', 'lab{}'.format(n_lab[0]), {
            'x': x, 'y': y[0], 'w': w, 'h': dy, 'cap': cap,
        })
        y[0] += dy

    def section(cap):
        """Section caption - together with sep() it gives the visible
        separation between the option groups."""
        y[0] += 2
        n_lab[0] += 1
        ctl('label', 'lab{}'.format(n_lab[0]), {
            'x': X0, 'y': y[0], 'w': W - 2 * X0, 'h': 17,
            'cap': cap + ':',
        })
        y[0] += 21

    def sep():
        """Separator line between the option groups."""
        y[0] += 4
        n_lab[0] += 1
        ctl('label', 'lab{}'.format(n_lab[0]), {
            'x': X0, 'y': y[0], 'w': W - 2 * X0, 'h': 6,
            'cap': '─' * 92,
        })
        y[0] += 12

    def hint(cap):
        """Small hint line - SHORT on purpose: the old dlg_custom dialog
        had hints wider than the window, so their ends were eaten."""
        n_lab[0] += 1
        ctl('label', 'lab{}'.format(n_lab[0]), {
            'x': X0 + 8, 'y': y[0], 'w': W - 2 * X0 - 8, 'h': 16, 'cap': cap,
        })
        y[0] += 16

    def check(name, cap, value):
        ctl('check', name, {
            'x': X0, 'y': y[0], 'w': W - 2 * X0, 'h': 22,
            'cap': cap,
            'val': '1' if value else '0',
        })
        y[0] += 23

    def row_edit(lab_cap, name, val, edit_w=None):
        n_lab[0] += 1
        ctl('label', 'lab{}'.format(n_lab[0]), {
            'x': X0, 'y': y[0] + 3, 'w': LW, 'h': 16, 'cap': lab_cap,
        })
        ctl('edit', name, {
            'x': EX, 'y': y[0], 'w': edit_w or EW, 'h': 23,
            'val': val,
        })
        y[0] += 28

    def row_combo(lab_cap, name, items, index):
        n_lab[0] += 1
        ctl('label', 'lab{}'.format(n_lab[0]), {
            'x': X0, 'y': y[0] + 3, 'w': LW, 'h': 16, 'cap': lab_cap,
        })
        ctl('combo_ro', name, {
            'x': EX, 'y': y[0], 'w': 300, 'h': 23,
            'items': '\t'.join(items),
            'val': str(index),
        })
        y[0] += 28

    #--------- the layout

    section(_('Project tree'))
    row_edit(_('Ignore &files:'), 'no_files', op.get('no_files', ''))
    row_edit(_('Ignore fold&ers:'), 'no_dirs', op.get('no_dirs', '.git;.svn'))
    check('no_hidden', _('Ignore all &hidden files/folders'), op.get('no_hidden', True))
    lab(_('&Recent projects:'), X0, W - 2 * X0, dy=17)
    # NOTE: the memo 'val' of the dialog API separates the lines with
    # the TAB character (a real tab inside a line is escaped as chr(3))
    # - with '\n' all projects were merged into a single line. The box
    # is ~4 lines tall (a line is ~17 px).
    ctl('memo', 'recent', {
        'x': X0, 'y': y[0], 'w': W - 2 * X0, 'h': 80,
        'val': '\t'.join(p.replace('\t', chr(3))
                         for p in op.get('recent_projects', [])),
    })
    y[0] += 85

    sep()

    section(_('Panel and opening files'))
    check('toolbar', _('&Show toolbar'), op.get('toolbar', True))
    check('goto_open', _('Open file after "&Go to file" command'), op.get('goto_open', False))
    check('preview', _('&Use "preview tab" on item clicking'), op.get('preview', True))
    check('d_click', _('Open files by &double-click'), op.get('d_click', False))
    check('always_sync', _('Automatically show current file in project tree'), op.get('always_sync', False))
    row_combo(_('File type icons (*):'), 'icon_theme', themes, theme_index)
    row_combo(_('Toolbar icons (*):'), 'toolbar_theme', themes_tb, theme_index_tb)
    row_combo(_('Sorting order (*):'), 'sort_order', items_sort_str, items_sort_val)

    sep()

    section(_('Project file backups'))
    check('backup_enabled', _('&Backup project file when its sessions change'),
          op.get('backup_enabled', True))
    row_edit(_('Backup &folder:'), 'backup_folder',
             op.get('backup_folder', DEFAULT_BACKUP_FOLDER))
    row_edit(_('Backups to &keep:'), 'backup_count',
             str(op.get('backup_count', DEFAULT_BACKUP_COUNT)), edit_w=90)
    hint(_('Relative folder is relative to the project file. Default: .cudatext/proj_bckp'))

    sep()

    section(_('New projects'))
    row_edit(_('Default projects &folder:'), 'projects_dir', op.get('projects_dir', ''))
    hint(_('For new projects: empty = last used folder;'))
    hint('$settings = ' + _('CudaText settings folder, e.g. $settings/projects'))

    sep()

    y[0] += 3
    hint(_('For more icons, get add-ons of kind "filetypeicons", "projtoolbaricons"'))
    hint(_('(*) - requires CudaText restart'))

    #--------- buttons (bottom-right) and the final height

    y[0] += 6
    ctl('button', 'btn_ok', {
        'x': W - X0 - 90 - 6 - 90, 'y': y[0], 'w': 90, 'h': BH,
        'cap': _('&OK'), 'ex0': True,          # default for Enter
        'on_change': on_ok,
    })
    ctl('button', 'btn_cancel', {
        'x': W - X0 - 90, 'y': y[0], 'w': 90, 'h': BH,
        'cap': _('Cancel'),
        'on_change': on_cancel,
    })
    H = y[0] + BH + 12

    dlg_proc(h, DLG_PROP_SET, prop={'h': H})
    dlg_proc(h, DLG_SCALE)
    dlg_proc(h, DLG_SHOW_MODAL)

    ok = state.pop('ok', False)
    if ok:
        op['no_files'] = str(state.get('no_files', '')).strip()
        op['no_dirs'] = str(state.get('no_dirs', '')).strip()
        op['no_hidden'] = _val_bool(state.get('no_hidden'))

        # the memo value separates the lines with TAB (see the comment
        # at the control); a tab inside a line comes back as chr(3).
        # Entries merged into one tab-joined line by the old (buggy)
        # builds are healed back into separate lines here.
        raw_recent = str(state.get('recent', '') or '')
        op['recent_projects'] = [s.replace(chr(3), '\t').strip()
                                 for s in raw_recent.split('\t')
                                 if s.strip()]

        op['toolbar'] = _val_bool(state.get('toolbar'))
        op['goto_open'] = _val_bool(state.get('goto_open'))
        op['preview'] = _val_bool(state.get('preview'))
        op['d_click'] = _val_bool(state.get('d_click'))
        op['always_sync'] = _val_bool(state.get('always_sync'))

        index = _val_int(state.get('icon_theme'))
        if 0 <= index < len(themes):
            op['icon_theme'] = themes[index]

        index = _val_int(state.get('toolbar_theme'))
        if 0 <= index < len(themes_tb):
            op['toolbar_theme'] = themes_tb[index]

        index = _val_int(state.get('sort_order'))
        if 0 <= index < len(items_sort_id):
            op['sort_order'] = items_sort_id[index]

        op['backup_enabled'] = _val_bool(state.get('backup_enabled'))
        op['backup_folder'] = str(state.get('backup_folder', '')).strip()
        cnt = _val_int(state.get('backup_count'))
        op['backup_count'] = max(0, cnt) if cnt >= 0 else DEFAULT_BACKUP_COUNT

        op['projects_dir'] = str(state.get('projects_dir', '')).strip()

    dlg_proc(h, DLG_FREE)
    return True if ok else None


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
