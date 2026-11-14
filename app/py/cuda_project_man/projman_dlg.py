import os
from cudatext import *
from cudatext_keys import *
from cudax_lib import get_translation
from .projman_glob import *
from .projman_backup import DEFAULT_BACKUP_COUNT, DEFAULT_BACKUP_FOLDER
from .favorites import get_theme_ui_colors

_   = get_translation(__file__)  # i18n


def _theme_props(colors, bg_key='edit_bg', font_key='edit_font'):
    """'color'/'font_color' props for a control, from the theme colors
    (only the keys the current theme defines; a missing key never adds
    a prop, so the control keeps the engine default)."""
    res = {}
    if colors.get(bg_key) is not None:
        res['color'] = colors[bg_key]
    if colors.get(font_key) is not None:
        res['font_color'] = colors[font_key]
    return res


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
    small screens. The dialog follows the CudaText UI theme like the
    built-in Preferences dialog: the form, the labels, the edit/memo/
    combo backgrounds and fonts and the checkbox captions are colored
    from app_proc(PROC_THEME_UI_DICT_GET), and the OK/Cancel buttons
    are the flat themed button_ex controls (the OS 'button' cannot be
    themed by the API; button_ex has no "default button" support, so
    Enter is handled by the form itself). The checkbox GLYPH stays
    OS-drawn - the dialog API has no themed checkbox.
    Returns True when OK was pressed."""

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
    colors = get_theme_ui_colors()

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

    def on_form_key(id_dlg, id_ctl, data='', info=''):
        """button_ex has no "default button" support, so the form
        handles Enter itself (Esc cancels the modal form - engine)."""
        if id_ctl == VK_ENTER and not (data or ''):
            on_ok(id_dlg, id_ctl)
            return False
        return None

    fprop = {
        'cap': _('Project Manager options'),
        'w': W,
        'h': 120,               # the real height is set after the layout
        'border': DBORDER_DIALOG,
        'keypreview': True,     # needed for on_key_down (Enter = OK)
        'on_key_down': on_form_key,
    }
    if colors['form_bg'] is not None:
        fprop['color'] = colors['form_bg']
    h = dlg_proc(0, DLG_CREATE)
    dlg_proc(h, DLG_PROP_SET, prop=fprop)

    #--------- small layout helpers (absolute coords, DLG_SCALE after)

    X0 = 12          # left margin
    LW = 175         # width of the label column
    EX = X0 + LW + 10
    EW = W - EX - X0

    y = [10]         # mutable layout cursor
    n_lab = [0]

    def lab(cap, x, w, dy=16):
        n_lab[0] += 1
        p = {'x': x, 'y': y[0], 'w': w, 'h': dy, 'cap': cap}
        p.update(_theme_props(colors, bg_key='form_bg', font_key='label_font'))
        ctl('label', 'lab{}'.format(n_lab[0]), p)
        y[0] += dy

    def section(cap):
        """Section caption - together with sep() it gives the visible
        separation between the option groups."""
        y[0] += 2
        n_lab[0] += 1
        p = {
            'x': X0, 'y': y[0], 'w': W - 2 * X0, 'h': 17,
            'cap': cap + ':',
        }
        p.update(_theme_props(colors, bg_key='form_bg', font_key='label_font'))
        ctl('label', 'lab{}'.format(n_lab[0]), p)
        y[0] += 21

    def sep():
        """Separator line between the option groups."""
        y[0] += 4
        n_lab[0] += 1
        p = {
            'x': X0, 'y': y[0], 'w': W - 2 * X0, 'h': 6,
            'cap': '─' * 92,
        }
        p.update(_theme_props(colors, bg_key='form_bg', font_key='label_font'))
        ctl('label', 'lab{}'.format(n_lab[0]), p)
        y[0] += 12

    def hint(cap):
        """Small hint line - SHORT on purpose: the old dlg_custom dialog
        had hints wider than the window, so their ends were eaten."""
        n_lab[0] += 1
        p = {'x': X0 + 8, 'y': y[0], 'w': W - 2 * X0 - 8, 'h': 16, 'cap': cap}
        p.update(_theme_props(colors, bg_key='form_bg', font_key='label_font'))
        ctl('label', 'lab{}'.format(n_lab[0]), p)
        y[0] += 16

    def check(name, cap, value):
        # the checkbox GLYPH stays OS-drawn (the dialog API has no
        # themed checkbox); the caption and the background follow the
        # theme
        p = {
            'x': X0, 'y': y[0], 'w': W - 2 * X0, 'h': 22,
            'cap': cap,
            'val': '1' if value else '0',
        }
        p.update(_theme_props(colors, bg_key='form_bg', font_key='label_font'))
        ctl('check', name, p)
        y[0] += 23

    def row_edit(lab_cap, name, val, edit_w=None):
        n_lab[0] += 1
        lp = {'x': X0, 'y': y[0] + 3, 'w': LW, 'h': 16, 'cap': lab_cap}
        lp.update(_theme_props(colors, bg_key='form_bg', font_key='label_font'))
        ctl('label', 'lab{}'.format(n_lab[0]), lp)
        ep = {
            'x': EX, 'y': y[0], 'w': edit_w or EW, 'h': 23,
            'val': val,
        }
        ep.update(_theme_props(colors))
        ctl('edit', name, ep)
        y[0] += 28

    def row_combo(lab_cap, name, items, index):
        n_lab[0] += 1
        lp = {'x': X0, 'y': y[0] + 3, 'w': LW, 'h': 16, 'cap': lab_cap}
        lp.update(_theme_props(colors, bg_key='form_bg', font_key='label_font'))
        ctl('label', 'lab{}'.format(n_lab[0]), lp)
        cp = {
            'x': EX, 'y': y[0], 'w': 300, 'h': 23,
            'items': '\t'.join(items),
            'val': str(index),
        }
        cp.update(_theme_props(colors))
        ctl('combo_ro', name, cp)
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
    mp = {
        'x': X0, 'y': y[0], 'w': W - 2 * X0, 'h': 80,
        'val': '\t'.join(p.replace('\t', chr(3))
                         for p in op.get('recent_projects', [])),
    }
    mp.update(_theme_props(colors))
    ctl('memo', 'recent', mp)
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
    # button_ex = the flat themed CudaText button (follows the UI
    # theme); the OS 'button' cannot be themed via the API. button_ex
    # draws its caption itself, so no '&' accelerators. Enter is
    # handled by the form (on_form_key), Esc cancels (engine).
    ctl('button_ex', 'btn_ok', {
        'x': W - X0 - 90 - 6 - 90, 'y': y[0], 'w': 90, 'h': BH,
        'cap': _('OK'),
        'on_change': on_ok,
    })
    ctl('button_ex', 'btn_cancel', {
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
    """Project properties dialog (variables + main file). Built with
    dlg_proc (the old dlg_custom version could not follow the UI
    theme): the form, the labels, the memo and the read-only edit are
    colored from the theme dict, and the buttons are the flat themed
    button_ex controls (Enter is handled by the form - button_ex has no
    "default button" support; Esc cancels the modal form - engine).
    Returns True when OK was pressed."""

    if isinstance(prop.get('vars'), list):
        list_vars = [str(v) for v in prop['vars']]
    else:
        list_vars = []
    main_file = str(prop.get('mainfile', ''))

    colors = get_theme_ui_colors()
    BH = app_proc(PROC_GET_GUI_HEIGHT, 'button') or 30

    W, H = 508, 330
    state = {'ok': False}

    def on_ok(id_dlg, id_ctl, data='', info=''):
        state['ok'] = True
        dlg_proc(h, DLG_HIDE)

    def on_cancel(id_dlg, id_ctl, data='', info=''):
        state['ok'] = False
        dlg_proc(h, DLG_HIDE)

    def on_form_key(id_dlg, id_ctl, data='', info=''):
        if id_ctl == VK_ENTER and not (data or ''):
            on_ok(id_dlg, id_ctl)
            return False
        return None

    fprop = {
        'cap': _('Project properties'),
        'w': W,
        'h': H,
        'border': DBORDER_DIALOG,
        'keypreview': True,
        'on_key_down': on_form_key,
    }
    if colors['form_bg'] is not None:
        fprop['color'] = colors['form_bg']
    h = dlg_proc(0, DLG_CREATE)
    dlg_proc(h, DLG_PROP_SET, prop=fprop)

    def ctl(kind, name, cprop):
        n = dlg_proc(h, DLG_CTL_ADD, prop=kind)
        if n is None:
            return None
        cprop = dict(cprop)
        cprop['name'] = name
        dlg_proc(h, DLG_CTL_PROP_SET, index=n, prop=cprop)
        return n

    def label(name, cap, x, y, w):
        p = {'x': x, 'y': y, 'w': w, 'h': 16, 'cap': cap}
        p.update(_theme_props(colors, bg_key='form_bg', font_key='label_font'))
        ctl('label', name, p)

    label('lab_vars', _('&Variables (in form Name=Value)'), 6, 6, W - 12)

    # the memo 'val' separates the lines with TAB; a real tab inside a
    # line is escaped as chr(3) (dialog API rule, same as the options
    # dialog's "Recent projects" box)
    mp = {
        'x': 6, 'y': 26, 'w': W - 12, 'h': 154,
        'val': '\t'.join(v.replace('\t', chr(3)) for v in list_vars),
    }
    mp.update(_theme_props(colors))
    ctl('memo', 'vars_memo', mp)

    label('lab_main', _('&Main file (read-only, change in context menu)'),
          6, 190, W - 12)

    ep = {
        'x': 6, 'y': 210, 'w': W - 12, 'h': 23,
        'val': main_file,
        'ex0': True,          # read-only (dialog API prop of edit/memo)
    }
    ep.update(_theme_props(colors))
    ctl('edit', 'main_file', ep)

    # flat themed buttons, bottom-right (button_ex draws its caption
    # itself, so no '&' accelerators)
    ctl('button_ex', 'btn_ok', {
        'x': W - 8 - 96 - 6 - 90, 'y': H - 8 - BH, 'w': 90, 'h': BH,
        'cap': _('OK'),
        'on_change': on_ok,
    })
    ctl('button_ex', 'btn_cancel', {
        'x': W - 8 - 96, 'y': H - 8 - BH, 'w': 96, 'h': BH,
        'cap': _('Cancel'),
        'on_change': on_cancel,
    })

    dlg_proc(h, DLG_SCALE)
    dlg_proc(h, DLG_SHOW_MODAL)

    ok = state.get('ok', False)
    if ok:
        try:
            props_memo = dlg_proc(h, DLG_CTL_PROP_GET, name='vars_memo') or {}
            raw = str(props_memo.get('val', '') or '')
        except Exception:
            raw = ''
        prop['vars'] = [item.strip().replace(chr(3), '\t')
                        for item in raw.split('\t') if '=' in item]

    dlg_proc(h, DLG_FREE)
    return True if ok else None
