import os
from cudatext import *
from cudatext_keys import *
from cudax_lib import get_translation
from .projman_glob import *
from .projman_backup import DEFAULT_BACKUP_COUNT, DEFAULT_BACKUP_FOLDER
from .utils import log_error
from .favorites import (get_theme_ui_colors, _theme_color, _theme_dict,
                        _unscale_px, _ui_scale, BTN_PAD)

_   = get_translation(__file__)  # i18n

#: width (unscaled px) kept free at the right of the options area for
#: the scrollbox's vertical scrollbar: the content panel is narrower
#: than the scrollbox client by this much, so the appearing vertical
#: scrollbar can never push a horizontal scrollbar out too
SCROLLBAR_W = 20
#: height (unscaled px) of the "Recent projects" editor: 5 text lines
#: (a line is ~17 px at the default editor font) plus the frame
RECENT_H = 92
#: the DEFAULT client height (unscaled px) of the options dialog: a
#: fixed modest size that fits every common screen (768 px tall
#: included), so the dialog never opens eating the whole screen. The
#: dialog is RESIZABLE, and the option area scrolls - smaller screens
#: are additionally protected by _monitor_client_h
DEF_CLIENT_H = 560
#: side of the checkbox square of a boolean option (unscaled px) - the
#: same size as the checkbox column of the Favorites window
CHECK_SZ = 15
#: the check mark of a checked boolean option: a text glyph (like the
#: star glyphs of the Favorites window - no image files), painted in
#: the theme's text color inside the drawn checkbox square
CHECK_GLYPH = '\u2713'    # '✓'


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


def _blend(c1, c2, k):
    """Linear blend of two 0xRRGGBB colors: k=0 -> c1, k=1 -> c2."""
    r = int(((c1 >> 16) & 255) + (((c2 >> 16) & 255) - ((c1 >> 16) & 255)) * k)
    g = int(((c1 >> 8) & 255) + (((c2 >> 8) & 255) - ((c1 >> 8) & 255)) * k)
    b = int((c1 & 255) + ((c2 & 255) - (c1 & 255)) * k)
    return (r << 16) | (g << 8) | b


def _monitor_client_h(scale):
    """Max unscaled CLIENT height a modal dialog may occupy: the
    current monitor's pixel height, minus a Windows-taskbar-sized strip
    (32-48 px) and the title bar (issue #6634 asked exactly for this).
    This is only the SAFETY CLAMP for small screens - the dialog's
    normal size is the fixed DEF_CLIENT_H, so a broken/odd monitor API
    can never make the dialog open full-screen (it is simply ignored).
    The layout is unscaled and DLG_SCALE multiplies it by 'scale', so
    the monitor's real pixels are divided back by 'scale'. Returns None
    when the monitor API is not usable - the caller then keeps the
    default size."""
    try:
        rect = app_proc(PROC_COORD_MONITOR)
        if not rect or len(rect) != 4:
            return None
        mon_h = int(rect[3]) - int(rect[1])
        if not 400 <= mon_h <= 20000:
            return None
        # 48 px taskbar + ~36 px dialog title bar, both in REAL pixels
        avail_px = mon_h - 48 - int(36 * scale)
        if avail_px <= 200:
            return None
        return int(avail_px / scale)
    # intentional silence: old CudaText builds / an unusable monitor
    # API - the caller keeps the default dialog size
    except Exception:
        return None


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
    dlg_custom). Every control follows the CudaText UI theme (like the
    built-in Preferences dialog), because ONLY theme-aware dialog-API
    control types are used:
    - form, labels, the scrollbox and its content panel: colored from
      app_proc(PROC_THEME_UI_DICT_GET);
    - one-line edits and the two filter-like fields: 'editor_edit'
      (ATSynEdit-based; the engine themes it incl. its border - the
      OS 'edit' border was too light on dark themes, issue #6633);
    - multi-line "Recent projects": 'editor' (a real mini-editor, 5
      lines; the OS 'memo' painted with OS colors, issue #6634);
    - boolean options: a drawn checkbox - a 'colorpanel' square with
      its own 1px border + the check mark as a text glyph, the same
      style as the checkbox column of the Favorites window, without
      any image files (the OS 'check' paints its glyph with OS
      colors, issue #6634);
    - list options: 'editor_combo' dropdowns (the OS 'combo_ro'
      painted with OS colors, issue #6634);
    - OK/Cancel: the flat themed 'button_ex' buttons.
    The option area is a 'scrollbox' (issue #6634): the dialog opens at
    a fixed default size (DEF_CLIENT_H) that fits every common screen,
    it is RESIZABLE (DBORDER_SIZE + the on_resize handler keeps the
    OK/Cancel buttons pinned to the dialog bottom and the scrollbox
    filling the rest), and whatever does not fit scrolls - the buttons
    stay fully visible at any scroll position and any window size. On
    small screens the height is additionally clamped by
    _monitor_client_h. Enter is handled by the form itself (button_ex
    has no "default button" support); Esc cancels the modal form
    (engine).
    Returns True when OK was pressed."""

    themes = get_themes_filetype()
    try:
        theme_index = themes.index(op.get('icon_theme', 'vscode_16x16'))
    except ValueError:
        theme_index = -1

    # the toolbar icon sets: 'auto' comes FIRST (the default: the
    # default icons colored for the current UI theme - a dark theme
    # gets the white set, a light one the black set), then every
    # installed folder as a FIXED choice, used as-is whatever the
    # theme (feather_*, custom folders, and the two default sets
    # themselves - only 'auto' follows the theme). The dropdown shows
    # friendlier labels for 'auto', so the displayed list and the
    # value list are separate
    themes_tb_val = ['auto'] + get_themes_toolbar()
    themes_tb_lbl = [_('auto (default icons, by UI theme)')] + themes_tb_val[1:]
    try:
        theme_index_tb = themes_tb_val.index(op.get('toolbar_theme', 'auto'))
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
    scale = _ui_scale()
    # unscale the DPI-scaled GUI height: the layout is unscaled and
    # DLG_SCALE scales it once at the end (see the favorites module's
    # "DPI NOTE")
    BH = _unscale_px(app_proc(PROC_GET_GUI_HEIGHT, 'button'), scale, 30)
    EH = _unscale_px(app_proc(PROC_GET_GUI_HEIGHT, 'edit'), scale, 28)
    colors = get_theme_ui_colors()

    state = {'ok': False}
    # live values of the editor-based controls, filled at creation and
    # read by on_ok (editor controls are not served by the dialog
    # 'val' prop - their text lives in the Editor objects)
    eds = {}      # name -> Editor (the 5 edit rows + 'recent')
    combos = {}   # name -> (Editor, items) of the dropdown rows
    checks = {}   # name -> bool of the boolean options
    # control index -> option name, for BOTH controls of a boolean
    # option (the drawn checkbox square AND its caption label - a
    # click on either toggles the option)
    check_names = {}
    # colors of the drawn checkbox, from the same theme keys the
    # Favorites window uses for its checkbox column (the square's
    # border is ButtonBorderPassive there; the check mark is the row
    # text color - here the label text color of the dialog). The
    # border is blended 40% toward the text color: plain
    # ButtonBorderPassive is nearly invisible on grey themes, and the
    # blend gives it a clearly visible step toward the text on every
    # theme (dark text darkens it on light themes, light text
    # brightens it on dark ones)
    th_dict = _theme_dict()
    check_border = _theme_color(th_dict, 'ButtonBorderPassive', 0x808080)
    if colors['label_font'] is not None:
        check_border = _blend(check_border, colors['label_font'], 0.4)

    def ctl(kind, name, prop):
        n = dlg_proc(h, DLG_CTL_ADD, prop=kind)
        if n is None:
            return None
        prop = dict(prop)
        prop['name'] = name
        dlg_proc(h, DLG_CTL_PROP_SET, index=n, prop=prop)
        return n

    def editor_of(index):
        """Editor object of the control added at index (never raises;
        None keeps the option out of 'eds' and its value unchanged)."""
        try:
            return Editor(dlg_proc(h, DLG_CTL_HANDLE, index=index) or 0)
        # intentional silence: the control was not created (an old
        # CudaText build) - the option keeps its value
        except Exception:
            return None

    def on_ok(id_dlg, id_ctl, data='', info=''):
        # read the values while the dialog is definitely alive
        state['ok'] = True
        for nm, ed in eds.items():
            try:
                state[nm] = ed.get_text_all()
            # intentional silence: the dialog is already closing - keep
            # '' for this field
            except Exception:
                state[nm] = ''
        for nm in checks:
            state[nm] = '1' if checks.get(nm) else '0'
        for nm, (ed, items) in combos.items():
            try:
                t = ed.get_text_all()
            # intentional silence: the dialog is already closing - keep
            # '' for this combo
            except Exception:
                t = ''
            state[nm] = str(items.index(t)) if t in items else '-1'
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

    def on_check_click(id_dlg, id_ctl, data='', info=''):
        """The checkbox square (either of its two panels) or its caption
        label was clicked: flip the option and show/hide the check mark
        glyph in the inner square panel."""
        nm = check_names.get(id_ctl)
        if nm is None:
            return
        checks[nm] = not checks.get(nm, False)
        try:
            dlg_proc(h, DLG_CTL_PROP_SET, name='chki_' + nm,
                     prop={'cap': CHECK_GLYPH if checks[nm] else ''})
        except Exception:
            log_error('Project Manager: cannot update '
                      'the options checkbox glyph')

    fprop = {
        'cap': _('Project Manager options'),
        'w': W,
        'h': 120,               # the real height is set after the layout
        # resizable window (issue #6634): the user can scale it, and
        # on_resize keeps the bottom buttons + the scrollbox in place
        'border': DBORDER_SIZE,
        'keypreview': True,     # needed for on_key_down (Enter = OK)
        'on_key_down': on_form_key,
    }
    if colors['form_bg'] is not None:
        fprop['color'] = colors['form_bg']
    h = dlg_proc(0, DLG_CREATE)
    dlg_proc(h, DLG_PROP_SET, prop=fprop)

    #--------- the scrollable options area (issue #6634)
    # Every option control is parented DIRECTLY into this scrollbox
    # (no intermediate panel): TScrollBox.AutoScroll then shows the
    # scrollbar whenever the controls' bottom is below the client
    # area. The controls are ~SCROLLBAR_W narrower than the box, so
    # the appearing vertical scrollbar never creates a horizontal
    # one. The OK/Cancel buttons are NOT in the scrollbox (added
    # after it, on the form itself): they stay at the dialog bottom
    # at any scroll position, and on_resize re-pins them when the
    # user resizes the window.
    # NOTE the scrollbox's own SCROLLBAR is painted by the OS (the
    # dialog API offers no themed scrollbar for it) - everything else
    # in the dialog is theme-drawn.
    sp = {
        'name': 'scr', 'x': 0, 'y': 0, 'w': W, 'h': 100,
        'border': False,
    }
    if colors['form_bg'] is not None:
        sp['color'] = colors['form_bg']
    n = dlg_proc(h, DLG_CTL_ADD, prop='scrollbox')
    dlg_proc(h, DLG_CTL_PROP_SET, index=n, prop=sp)

    # usable width inside the scrollbox = its width minus the
    # scrollbar reserve; all layout helpers below position in it
    PANEL_W = W - SCROLLBAR_W

    #--------- small layout helpers (absolute coords, DLG_SCALE after)
    # Every helper parents its control directly to the scrollbox 'scr'
    # ('p' is the FIRST prop: the x/y/w/h after it are relative to the
    # scrollbox, which sits at 0,0 of the form - so the numbers are
    # plain dialog coordinates).

    X0 = 12          # left margin
    LW = 175         # width of the label column
    EX = X0 + LW + 10
    EW = PANEL_W - EX - X0

    y = [10]         # mutable layout cursor
    n_lab = [0]

    def lab(cap, x, w, dy=16):
        n_lab[0] += 1
        p = {'p': 'scr', 'x': x, 'y': y[0], 'w': w, 'h': dy, 'cap': cap}
        p.update(_theme_props(colors, bg_key='form_bg', font_key='label_font'))
        ctl('label', 'lab{}'.format(n_lab[0]), p)
        y[0] += dy

    def section(cap):
        """Section caption - together with sep() it gives the visible
        separation between the option groups."""
        y[0] += 2
        n_lab[0] += 1
        p = {
            'p': 'scr',
            'x': X0, 'y': y[0], 'w': PANEL_W - 2 * X0, 'h': 17,
            'cap': cap + ':',
        }
        p.update(_theme_props(colors, bg_key='form_bg', font_key='label_font'))
        ctl('label', 'lab{}'.format(n_lab[0]), p)
        y[0] += 21

    def sep():
        """Separator line between the option groups: a 2px 'colorpanel'
        EXACTLY as wide as the option rows (PANEL_W-2*X0), filled with
        the checkbox border color. It was a label with a caption of 92
        '─' characters before - that caption renders WIDER than the
        scrollbox, which made TScrollBox show a horizontal scrollbar
        even though every option control fits; a panel has no text, so
        its width is always exact.
        The y is SNAPPED up to a multiple of 8 design px: DPI scaling
        (DLG_SCALE -> LCL AutoAdjustLayout) rounds every control's
        scaled Top and Bottom independently with round-half-to-even,
        so a 2px panel renders 2px or 3px thick depending on its y
        phase - at 125% the four separators rendered 3,2,2,3 px.
        Separators sharing one phase (mod 8: 8px at every 25% scale
        step is an even integer, so the phase and the rounding parity
        are fixed) always render equally thick."""
        y[0] += 4
        y[0] = ((y[0] + 7) // 8) * 8
        p = {
            'p': 'scr',
            'x': X0, 'y': y[0], 'w': PANEL_W - 2 * X0, 'h': 2,
            'ex0': 0,
            'color': check_border,
        }
        ctl('colorpanel', 'sep{}'.format(n_lab[0]), p)
        y[0] += 12

    def hint(cap):
        """Small hint line - SHORT on purpose: the old dlg_custom dialog
        had hints wider than the window, so their ends were eaten."""
        n_lab[0] += 1
        p = {
            'p': 'scr',
            'x': X0 + 8, 'y': y[0], 'w': PANEL_W - 2 * X0 - 8, 'h': 16,
            'cap': cap,
        }
        p.update(_theme_props(colors, bg_key='form_bg', font_key='label_font'))
        ctl('label', 'lab{}'.format(n_lab[0]), p)
        y[0] += 16

    def check(name, cap, value):
        """One boolean option = a DRAWN checkbox, the same style as the
        checkbox column of the Favorites window, and NO image files.
        The square is a SANDWICH of two 'colorpanel' controls
        (TATPanelColor):
        - the OUTER 15x15 panel draws only the 1px border, in
          ButtonBorderPassive blended 40% toward the theme's text
          color (plain ButtonBorderPassive was nearly invisible on
          grey themes);
        - the INNER 13x13 panel (1px inset) carries the check mark as
          the CHECK_GLYPH text-glyph caption, in the theme's label
          color.
        Two panels are needed because of a TATPanelColor paint detail:
        it draws its caption with Canvas.TextOut, whose BACKGROUND
        (the panel's own fill brush, as large as the text extent) is
        not clipped - on a single 15px panel the 'check' text extent
        spans the whole inner area and its background overpainted the
        1px border ring, so a CHECKED box lost its border. The inner
        panel is clipped to its own client area, 1px inside the ring,
        so the border can never be overpainted again.
        The caption is a plain themed label to the right. A click on
        the square (either panel) OR on the caption toggles the option
        (the live state lives in 'checks'; the inner panel's caption
        is swapped between the glyph and ''). The OS 'check' control
        was not usable: its glyph is painted with OS colors and
        ignored the theme (issue #6634). '&' accelerators are stripped
        - the label is a plain label, no focus wiring."""
        cap = cap.replace('&&', '\x01').replace('&', '').replace('\x01', '&')
        # the squares first, the caption label after them (the engine
        # wires a label created BEFORE a TWinControl as its FocusControl
        # - creating the label second avoids that bogus wiring)
        for nm_suffix, dx, dy, size, cap_txt in (
                ('chk_', 0, 0, CHECK_SZ, ''),
                ('chki_', 1, 1, CHECK_SZ - 2,
                 CHECK_GLYPH if value else '')):
            box = {
                'p': 'scr',
                'x': X0 + dx, 'y': y[0] + 5 + dy,
                'w': size, 'h': size,
                # 'act' is REQUIRED: the colorpanel reports its click
                # through on_change, and DoOnChange drops the event
                # for controls not marked 'act' (buttons get it by
                # default, colorpanels don't)
                'act': True,
                'on_change': on_check_click,
            }
            if nm_suffix == 'chk_':
                # the outer border ring: ex0 = width, ex3 = color
                box['ex0'] = 1
                box['ex3'] = check_border
            else:
                # the inner glyph panel: no border, the caption glyph
                box['ex0'] = 0
                box['cap'] = cap_txt
            if colors['form_bg'] is not None:
                box['color'] = colors['form_bg']
            if colors['label_font'] is not None:
                box['font_color'] = colors['label_font']
            n = ctl('colorpanel', nm_suffix + name, box)
            if n is not None:
                check_names[n] = name
        checks[name] = bool(value)
        n_lab[0] += 1
        lp = {
            'p': 'scr',
            'x': X0 + CHECK_SZ + 6, 'y': y[0] + 4,
            'w': PANEL_W - 2 * X0 - CHECK_SZ - 6, 'h': 16,
            'cap': cap,
            'on_click': on_check_click,
        }
        lp.update(_theme_props(colors, bg_key='form_bg', font_key='label_font'))
        n = ctl('label', 'chklab_' + name, lp)
        if n is not None and name in checks:
            check_names[n] = name
        y[0] += 26

    def row_edit(lab_cap, name, val, edit_w=None):
        """Label + one-line 'editor_edit' (ATSynEdit-based): the engine
        themes it entirely, border included - the OS 'edit' control had
        an OS-drawn border, too light on dark themes (issue #6633).
        The text is set/read through the Editor object: the dialog
        'val' prop does not serve editor controls."""
        n_lab[0] += 1
        lp = {
            'p': 'scr',
            'x': X0, 'y': y[0] + 3, 'w': LW, 'h': 16, 'cap': lab_cap,
        }
        lp.update(_theme_props(colors, bg_key='form_bg', font_key='label_font'))
        ctl('label', 'lab{}'.format(n_lab[0]), lp)
        n = ctl('editor_edit', name, {
            'p': 'scr',
            'x': EX, 'y': y[0], 'w': edit_w or EW, 'h': EH,
        })
        if n is not None:
            ed = editor_of(n)
            if ed is not None:
                eds[name] = ed
                try:
                    ed.set_text_all(str(val))
                except Exception:
                    log_error('Project Manager: cannot set the initial '
                              'value of an options field')
        y[0] += 28

    def row_combo(lab_cap, name, items, index):
        """Label + 'editor_combo' (TATComboEdit) - the themed dropdown
        the built-in Preferences dialog uses for its list-valued
        options; the OS 'combo_ro' painted its list with OS colors
        (issue #6634). Made read-only AFTER the initial text is set
        (a read-only editor refuses programmatic writes too), so the
        user can only pick from the list. The text is read back through
        the Editor object and mapped to an index."""
        n_lab[0] += 1
        lp = {
            'p': 'scr',
            'x': X0, 'y': y[0] + 3, 'w': LW, 'h': 16, 'cap': lab_cap,
        }
        lp.update(_theme_props(colors, bg_key='form_bg', font_key='label_font'))
        ctl('label', 'lab{}'.format(n_lab[0]), lp)
        n = ctl('editor_combo', name, {
            'p': 'scr',
            'x': EX, 'y': y[0], 'w': 300, 'h': EH,
        })
        if n is not None:
            ed = editor_of(n)
            if ed is not None:
                try:
                    ed.set_prop(PROP_COMBO_ITEMS, '\n'.join(items))
                    if 0 <= index < len(items):
                        ed.set_text_all(items[index])
                    ed.set_prop(PROP_RO, True)
                    combos[name] = (ed, items)
                except Exception:
                    log_error('Project Manager: cannot fill '
                              'an options combo')
        y[0] += 28

    #--------- the layout

    section(_('Project tree'))
    row_edit(_('Ignore &files:'), 'no_files', op.get('no_files', ''))
    row_edit(_('Ignore fold&ers:'), 'no_dirs', op.get('no_dirs', '.git;.svn'))
    check('no_hidden', _('Ignore all &hidden files/folders'), op.get('no_hidden', True))
    lab(_('&Recent projects:'), X0, PANEL_W - 2 * X0, dy=17)
    # a real mini-editor: 5 visible lines, one project per line, plain
    # '\n' separation (the old OS 'memo' needed the dialog API's TAB
    # line-separation plus the chr(3) tab-escaping - both gone with it)
    n = ctl('editor', 'recent', {
        'p': 'scr',
        'x': X0, 'y': y[0], 'w': PANEL_W - 2 * X0, 'h': RECENT_H,
    })
    if n is not None:
        ed = editor_of(n)
        if ed is not None:
            eds['recent'] = ed
            try:
                ed.set_prop(PROP_GUTTER_ALL, False)
                ed.set_text_all('\n'.join(op.get('recent_projects', []) or []))
            except Exception:
                log_error('Project Manager: cannot fill '
                          'the recent-projects list')
    y[0] += RECENT_H + 5

    sep()

    section(_('Panel and opening files'))
    check('toolbar', _('&Show toolbar'), op.get('toolbar', True))
    check('goto_open', _('Open file after "&Go to file" command'), op.get('goto_open', False))
    check('preview', _('&Use "preview tab" on item clicking'), op.get('preview', True))
    check('d_click', _('Open files by &double-click'), op.get('d_click', False))
    check('always_sync', _('Automatically show current file in project tree'), op.get('always_sync', False))
    row_combo(_('Sorting order:'), 'sort_order', items_sort_str, items_sort_val)
    row_combo(_('File type icons:'), 'icon_theme', themes, theme_index)
    row_combo(_('Toolbar icons:'), 'toolbar_theme', themes_tb_lbl, theme_index_tb)
    # the add-ons hint sits right below the two icon dropdowns it
    # talks about (it used to be at the very bottom of the dialog)
    hint(_('For more icons, get add-ons of kind "filetypeicons", "projtoolbaricons"'))

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

    #--------- buttons (bottom-right) and the final height

    # the full content height is known only now: give the content panel
    # its real height, clamp the dialog height to the monitor, and give
    # the scrollbox whatever is left
    content_h = y[0] + 2
    # button_ex = the flat themed CudaText button (follows the UI
    # theme); the OS 'button' cannot be themed via the API. button_ex
    # draws its caption itself, so no '&' accelerators. Enter is
    # handled by the form (on_form_key), Esc cancels (engine).
    # BTN_H adds the same comfortable vertical padding as the buttons
    # of the Favorites window (the raw GUI button height hugs the
    # caption too tightly - the buttons looked like thin slabs).
    BTN_H = BH + BTN_PAD
    BTN_ROW = BTN_H + 18      # the buttons + 8 px above + 10 px below
    needed_h = content_h + 8 + BTN_ROW

    # the fixed default client height (fits every common screen -
    # never a full-screen dialog), even lower only when the content
    # is shorter or on a small screen (the _monitor_client_h safety
    # clamp): what does not fit scrolls
    H = min(needed_h, DEF_CLIENT_H)
    max_h = _monitor_client_h(scale)
    if max_h is not None:
        H = min(H, max_h)
    H = max(H, 300)           # never a degenerate stub
    scr_h = H - BTN_ROW

    y_btn = H - 10 - BTN_H
    ctl('button_ex', 'btn_ok', {
        'x': W - X0 - 90 - 6 - 90, 'y': y_btn, 'w': 90, 'h': BTN_H,
        'cap': _('OK'),
        'on_change': on_ok,
    })
    ctl('button_ex', 'btn_cancel', {
        'x': W - X0 - 90, 'y': y_btn, 'w': 90, 'h': BTN_H,
        'cap': _('Cancel'),
        'on_change': on_cancel,
    })

    def on_form_resize(id_dlg, id_ctl, data='', info=''):
        """The user resized the (resizable) window: keep the option
        scrollbox filling everything above the bottom button row and
        the OK/Cancel buttons pinned to the dialog bottom - the
        scrollbox itself shows/hides its scrollbar automatically
        (TScrollBox.AutoScroll) as its client grows or shrinks. All
        sizes here are the LIVE SCALED pixels of the form
        (DLG_PROP_GET returns the client size), so the unscaled
        layout constants are multiplied by 'scale'."""
        try:
            prop = dlg_proc(id_dlg, DLG_PROP_GET)
            new_w = int(prop.get('w') or 0)
            new_h = int(prop.get('h') or 0)
            if new_w <= 0 or new_h <= 0:
                return
            bottom = int(BTN_ROW * scale)
            y_b = new_h - max(int(10 * scale), 8) - int(BTN_H * scale)
            dlg_proc(id_dlg, DLG_CTL_PROP_SET, name='scr',
                     prop={'w': new_w,
                           'h': max(60, new_h - bottom)})
            dlg_proc(id_dlg, DLG_CTL_PROP_SET, name='btn_ok',
                     prop={'y': y_b})
            dlg_proc(id_dlg, DLG_CTL_PROP_SET, name='btn_cancel',
                     prop={'y': y_b})
        except Exception:
            log_error('Project Manager: cannot resize the options dialog')

    dlg_proc(h, DLG_PROP_SET, prop={'h': H, 'on_resize': on_form_resize})
    dlg_proc(h, DLG_CTL_PROP_SET, name='scr', prop={'h': scr_h})
    dlg_proc(h, DLG_SCALE)
    # the resize constraints are raw pixels (not scaled by DLG_SCALE),
    # so set them AFTER the scaling with the scaled numbers
    dlg_proc(h, DLG_PROP_SET, prop={
        'w_min': int((W - 80) * scale),
        'h_min': int(320 * scale),
    })
    dlg_proc(h, DLG_SHOW_MODAL)

    ok = state.pop('ok', False)
    if ok:
        op['no_files'] = str(state.get('no_files', '')).strip()
        op['no_dirs'] = str(state.get('no_dirs', '')).strip()
        op['no_hidden'] = _val_bool(state.get('no_hidden'))

        # the "Recent projects" editor keeps one project per line -
        # plain '\n' lines now (no TAB/chr(3) escaping anymore)
        op['recent_projects'] = [
            s.strip() for s in str(state.get('recent', '') or '').splitlines()
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
        if 0 <= index < len(themes_tb_val):
            op['toolbar_theme'] = themes_tb_val[index]

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
    dlg_proc with theme-aware control types only: the form and labels
    are colored from the theme dict, the variables box is a themed
    'editor' (a real mini-editor - the OS 'memo' painted with OS
    colors), the read-only main-file field is a themed 'editor_edit'
    (the OS 'edit' border was too light on dark themes, issue #6633),
    and the buttons are the flat themed button_ex controls with the
    same vertical padding as the other dialogs (Enter is handled by
    the form - button_ex has no "default button" support; Esc cancels
    the modal form - engine).
    Returns True when OK was pressed."""

    if isinstance(prop.get('vars'), list):
        list_vars = [str(v) for v in prop['vars']]
    else:
        list_vars = []
    main_file = str(prop.get('mainfile', ''))

    colors = get_theme_ui_colors()
    scale = _ui_scale()
    # unscale the DPI-scaled GUI heights (see the favorites module's
    # "DPI NOTE"): the layout is unscaled, DLG_SCALE scales it once
    BH = _unscale_px(app_proc(PROC_GET_GUI_HEIGHT, 'button'),
                     scale, 30)
    EH = _unscale_px(app_proc(PROC_GET_GUI_HEIGHT, 'edit'),
                     scale, 28)
    # the same comfortable vertical padding as in the other dialogs
    BTN_H = BH + BTN_PAD

    W, H = 508, 330
    state = {'ok': False}

    def on_ok(id_dlg, id_ctl, data='', info=''):
        state['ok'] = True
        # read the memo text while the dialog is definitely alive
        try:
            state['vars'] = ed_vars.get_text_all()
        # intentional silence: the dialog is already closing - keep ''
        except Exception:
            state['vars'] = ''
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

    # a real mini-editor: one variable per line, plain '\n' separation
    # (the old OS 'memo' needed the dialog API's TAB line-separation
    # plus the chr(3) tab-escaping - both gone with it)
    ed_vars = None
    n = ctl('editor', 'vars_memo', {
        'x': 6, 'y': 26, 'w': W - 12, 'h': 154,
    })
    if n is not None:
        try:
            ed_vars = Editor(dlg_proc(h, DLG_CTL_HANDLE, index=n) or 0)
            ed_vars.set_prop(PROP_GUTTER_ALL, False)
            ed_vars.set_text_all('\n'.join(list_vars))
        except Exception:
            log_error('Project Manager: cannot set up the vars editor')
            ed_vars = None

    label('lab_main', _('&Main file (read-only, change in context menu)'),
          6, 190, W - 12)

    # themed one-line edit, read-only (PROP_RO - the OS 'edit' control
    # was not theme-aware, issue #6633); read-only is set AFTER the
    # text, a read-only editor refuses programmatic writes too
    n = ctl('editor_edit', 'main_file', {
        'x': 6, 'y': 210, 'w': W - 12, 'h': EH,
    })
    if n is not None:
        try:
            ed = Editor(dlg_proc(h, DLG_CTL_HANDLE, index=n) or 0)
            ed.set_text_all(main_file)
            ed.set_prop(PROP_RO, True)
        except Exception:
            log_error('Project Manager: cannot show the main file field')

    # flat themed buttons, bottom-right (button_ex draws its caption
    # itself, so no '&' accelerators)
    ctl('button_ex', 'btn_ok', {
        'x': W - 8 - 96 - 6 - 90, 'y': H - 8 - BTN_H, 'w': 90, 'h': BTN_H,
        'cap': _('OK'),
        'on_change': on_ok,
    })
    ctl('button_ex', 'btn_cancel', {
        'x': W - 8 - 96, 'y': H - 8 - BTN_H, 'w': 96, 'h': BTN_H,
        'cap': _('Cancel'),
        'on_change': on_cancel,
    })

    dlg_proc(h, DLG_SCALE)
    dlg_proc(h, DLG_SHOW_MODAL)

    ok = state.get('ok', False)
    if ok:
        raw = str(state.get('vars', '') or '')
        prop['vars'] = [item.strip()
                        for item in raw.splitlines() if '=' in item]

    dlg_proc(h, DLG_FREE)
    return True if ok else None
