'''Favorites (bookmarks) manager for the CudaText "Project Manager" plugin.

Stores three kinds of bookmarks - projects, files, folders - in
"cuda_project_man_favorites.json" in the CudaText settings folder.

One-way import from the separate "Favorites" plugin (cuda_favorites by
kvichans): its data file "cuda_favorites.json" is read when this plugin
starts and when a favorites window is opened; entries not present in
our own list are imported (duplicates removed by path). Our data is
NEVER written back to cuda_favorites.json. Entries deleted here are
remembered (in our own file) so the import does not resurrect them;
adding a path again clears that memory.

Main window (non-modal, resizable):
- tabs: Projects / Files / Folders
- owner-drawn list (listbox_ex) with columns: checkbox, star, Name,
  Path; the header line above the list is clickable to sort
  ("starred first, then by name" by default; a 3rd click on a column
  switches to manual (custom) order)
- checkbox column: mark several favorites, then Open / Delete / Move /
  Rename / Star / Copy path / Reveal act on ALL of them; Space toggles
  the checkbox of the selected row, Ctrl+A checks/unchecks all
- star column: the UTF-8 font glyphs ☆ (unstarred) / ★ (starred) -
  no image files; click the star cell to toggle the "favorite of
  favorites" star
- favorites whose path no longer exists on disk are drawn in RED
  (name prefixed with "? ")
- filter field at the top filters by name and path; Esc clears it
- vertical button panel on the left: Open / Add... / Add opened...,
  Move up / Move down, Delete; Close at the bottom
- while the window is focused: Ctrl+1..9 / Alt+1..9 open the item at
  that index (index in the currently visible, filtered list), Enter
  opens the selection, Del deletes it, F2 renames it,
  Alt+Up/Down move it, Esc clears the filter (or closes the window)
- window size is remembered only while CudaText runs (the maximized
  size is never restored from disk: a CudaText/LCL form recreated with
  the pixel size of a maximized window ends up taller than the work
  area and its bottom part lands off-screen, so the size is not saved
  to the json at all)

"Add opened..." shows the same chooser as "Add...": pick the opened
project, the current editor file, or the folder of the current file.

Opening a favorite: project -> opened by Project Manager; file ->
opened in the editor; folder -> simply opened in the OS file manager.
'''

import os
import sys
import json

from cudatext import *
from cudatext_keys import *
from cudax_lib import get_translation

_ = get_translation(__file__)  # i18n

IS_WIN = os.name == 'nt'
IS_MAC = sys.platform == 'darwin'
OS_SUFFIX = app_proc(PROC_GET_OS_SUFFIX, '')

#: our own storage file
FAVORITES_FILENAME = 'cuda_project_man_favorites.json'
#: data file of the "Favorites" plugin (cuda_favorites by kvichans);
#: import-only - never created or modified by this plugin
EXT_FAVORITES_FILENAME = 'cuda_favorites.json'

PROJECT_EXTENSION = '.cuda-proj'

CATS = ('projects', 'files', 'folders')
CAT_TITLES = {
    'projects': _('Project'),
    'files': _('File'),
    'folders': _('Folder'),
}

#: sort modes: 'star', 'name', 'path', 'manual'
SORT_COLS = ('star', 'name', 'path')
DEFAULT_SORT = 'star'

# drawn/listbox-header column indexes
COL_CHK = 0
COL_STAR = 1
COL_NAME = 2
COL_PATH = 3

#: unscaled px widths of the first columns; the Path column eats the rest
COL_CHK_W = 40
COL_STAR_W = 48
COL_NAME_W = 230
COL_TEXT_PAD = 8

#: UTF-8 font glyphs (no image files) for the checkbox column
GLYPH_CHECKED = '☑'
GLYPH_UNCHECKED = '☐'
#: TColor (BGR int) of the red text for favorites missing on disk
COLOR_DEAD = 0x0000FF  # clRed

MOD_COL = {
    COL_STAR: 'star',
    COL_NAME: 'name',
    COL_PATH: 'path',
}

BTN_W = 126
BTN_X = 8
BTN_GAP = 6
GRP_GAP = 18

FONT_W = 720
FONT_H = 470
FORM_MIN_W = 560
FORM_MIN_H = 350

#: fallback colors (TColor BGR ints) when the UI theme cannot be read
_COLOR_FALLBACK = {
    'bg': 0xFFFFFF,
    'sel_bg': 0xCCCCCC,
    'font': 0x000000,
    'sel_font': 0x000000,
}

_theme_colors_cache = None


def _theme_colors():
    """{bg, sel_bg, font, sel_font} with int TColor values of the current
    UI theme (keys 'ListBg'/'ListSelBg'/'ListFont'/'ListSelFont' of
    app_proc(PROC_THEME_UI_DICT_GET)). Falls back to plain black on
    white when the theme cannot be read."""
    global _theme_colors_cache
    if _theme_colors_cache is not None:
        return _theme_colors_cache
    res = dict(_COLOR_FALLBACK)
    try:
        theme = app_proc(PROC_THEME_UI_DICT_GET, '')
        if isinstance(theme, dict):
            for key_out, key_in in (('bg', 'ListBg'),
                                    ('sel_bg', 'ListSelBg'),
                                    ('font', 'ListFont'),
                                    ('sel_font', 'ListSelFont')):
                it = theme.get(key_in)
                color = it.get('color') if isinstance(it, dict) else None
                if isinstance(color, int) and color >= 0:
                    res[key_out] = color
    except Exception:
        pass
    _theme_colors_cache = res
    return res


def _ui_scale():
    """Current UI scale factor (1.0 at 100% DPI), for the drawn column
    widths - the listbox item font is scaled by CudaText, so the fixed
    column offsets must be scaled by the same value."""
    try:
        val = app_proc(PROC_CONFIG_SCALE_GET, '')
        return max(1.0, int(val[0]) / 100.0)
    except Exception:
        return 1.0


def _norm_path(path):
    """Normalized comparison key for deduplication (case-insensitive on
    Windows, case-sensitive elsewhere; '~' expanded)."""
    s = os.path.expanduser(str(path))
    s = os.path.normpath(s)
    if os.name == 'nt':
        s = os.path.normcase(s)
    return s


def _base_name(path):
    s = str(path).rstrip('/\\')
    s = os.path.basename(s)
    return s or str(path)


def _read_json(filename):
    try:
        with open(filename, encoding='utf8') as fin:
            return json.load(fin)
    except Exception:
        return None


def _write_json(filename, data):
    try:
        with open(filename, 'w', encoding='utf8') as fout:
            json.dump(data, fout, indent=4, ensure_ascii=False)
        return True
    except OSError as ex:
        msg_status(_('Favorites: cannot save ') + filename + ' (' + str(ex) + ')')
        return False


def get_current_project():
    """Full path of the project opened in Project Manager, or ''."""
    try:
        import cuda_project_man
        return cuda_project_man.global_project_info.get('filename', '') or ''
    except Exception:
        return ''


def reveal_in_file_manager(path):
    """Show the file/folder in the OS file manager (same logic as Project
    Manager's own 'Reveal in file manager')."""
    sfn = str(path)
    if OS_SUFFIX == '':
        # Windows
        import subprocess
        subprocess.Popen(('explorer.exe', '/select,', sfn), shell=True)
    elif OS_SUFFIX == '__mac':
        # macOS
        os.system('open --new --reveal ' + sfn.replace(' ', '\\ '))
    elif OS_SUFFIX == '__haiku':
        msg_status(_('Favorites: "Reveal in file manager" is not implemented for this OS'))
    else:
        # Linux and other Unixes
        uri = 'file://' + sfn
        if os.spawnvp(os.P_WAIT, 'dbus-send',
                      ('dbus-send', '--session',
                       '--dest=org.freedesktop.FileManager1',
                       '--type=method_call', '--print-reply',
                       '/org/freedesktop/FileManager1',
                       'org.freedesktop.FileManager1.ShowItems',
                       'array:string:' + uri, 'string:')):
            parent = os.path.dirname(sfn)
            os.spawnvp(os.P_WAIT, 'xdg-open', ('xdg-open', 'file://' + parent))


def open_folder_in_file_manager(path):
    """Open the folder itself in the OS file manager (Windows Explorer,
    Finder, ...)."""
    sfn = str(path)
    if OS_SUFFIX == '':
        # Windows
        os.startfile(sfn)
    elif OS_SUFFIX == '__mac':
        # macOS
        os.system('open --new ' + sfn.replace(' ', '\\ '))
    elif OS_SUFFIX == '__haiku':
        os.spawnvp(os.P_WAIT, 'open', ('open', sfn))
    else:
        # Linux and other Unixes
        os.spawnvp(os.P_WAIT, 'xdg-open', ('xdg-open', sfn))


def open_item(path, from_modal=False):
    """Open one favorites item:
    - project file -> opened by Project Manager (via on_open_pre)
    - file -> opened in editor
    - folder -> simply opened in the OS file manager
    Returns True when something was opened."""
    path = str(path)
    if not path:
        return False
    if not os.path.exists(path):
        msg_status(_('Favorites: path does not exist: ') + path)
        return False

    if path.endswith(PROJECT_EXTENSION) and os.path.isfile(path):
        app_proc(PROC_SET_FOLDER, os.path.dirname(path))
        file_open(path)  # Project Manager intercepts .cuda-proj opening
        return True

    if os.path.isdir(path):
        open_folder_in_file_manager(path)
        return True

    app_proc(PROC_SET_FOLDER, os.path.dirname(path))
    file_open(path)
    return True


class FavoritesCore:
    """Data storage: load/save of our json + one-way import from the
    cuda_favorites plugin's json (import-only, never exported back)."""

    def __init__(self):
        self.path_own = os.path.join(app_path(APP_DIR_SETTINGS), FAVORITES_FILENAME)
        self.path_ext = os.path.join(app_path(APP_DIR_SETTINGS), EXT_FAVORITES_FILENAME)
        self.data = {cat: [] for cat in CATS}
        self.ui = {}
        #: paths deleted by the user: the one-way import must not
        #: resurrect them (stored in our json as "_deleted")
        self.deleted = []
        self.last_imported = 0

    #------------- helpers

    @staticmethod
    def _valid_item(it):
        return isinstance(it, dict) and isinstance(it.get('path'), str) and it['path']

    def _normalize(self):
        for cat in CATS:
            items = []
            seen = set()
            for it in self.data.get(cat, []):
                if not self._valid_item(it):
                    continue
                key = _norm_path(it['path'])
                if not key or key in seen:
                    continue
                seen.add(key)
                it['name'] = str(it.get('name') or '') or _base_name(it['path'])
                it['star'] = bool(it.get('star', False))
                items.append(it)
            self.data[cat] = items
        # tombstones: a flat list of unique paths
        seen = set()
        deleted = []
        for p in self.deleted or []:
            if isinstance(p, str) and p:
                key = _norm_path(p)
                if key and key not in seen:
                    seen.add(key)
                    deleted.append(p)
        self.deleted = deleted

    def _is_deleted(self, path):
        key = _norm_path(path)
        return any(_norm_path(p) == key for p in self.deleted)

    def _forget_deleted(self, path):
        key = _norm_path(path)
        self.deleted = [p for p in self.deleted if _norm_path(p) != key]

    def _add_if_new(self, cat, path, name=None, star=False):
        """Append an entry unless the path is already in that category.
        Returns True when added."""
        path = str(path)
        if not path:
            return False
        key = _norm_path(path)
        for it in self.data[cat]:
            if _norm_path(it['path']) == key:
                return False
        self.data[cat].append({
            'path': path,
            'name': name or _base_name(path),
            'star': bool(star),
        })
        self._forget_deleted(path)  # re-adding clears the tombstone
        return True

    #------------- load / save

    def load(self):
        """Read our file, then import new entries from cuda_favorites.json
        (one-way: imported here, never exported back). Returns the
        number of imported entries."""
        own = _read_json(self.path_own)
        self.data = {cat: [] for cat in CATS}
        if isinstance(own, dict):
            for cat in CATS:
                val = own.get(cat)
                if isinstance(val, list):
                    self.data[cat] = val
        self.ui = own.get('_ui') if isinstance(own, dict) and isinstance(own.get('_ui'), dict) else {}
        # window size is NOT persisted (see the module docstring) - drop
        # the keys if an older build of this unreleased version wrote them
        self.ui.pop('w', None)
        self.ui.pop('h', None)
        self.deleted = [p for p in (own.get('_deleted') if isinstance(own, dict) else None) or []
                        if isinstance(p, str)]
        self._normalize()

        imported = 0
        ext = _read_json(self.path_ext)
        if isinstance(ext, dict):
            for p in ext.get('fv_projs') or []:
                if isinstance(p, str) and p and not self._is_deleted(p):
                    imported += 1 if self._add_if_new('projects', p) else 0
            for p in ext.get('fv_files') or []:
                if isinstance(p, str) and p and not self._is_deleted(p):
                    cat = 'folders' if os.path.isdir(p) else 'files'
                    imported += 1 if self._add_if_new(cat, p) else 0

        if imported:
            self.save()
        self.last_imported = imported
        return imported

    def save(self):
        """Write our own json - and nothing else: the sync with
        cuda_favorites.json is one-way (import-only), that file is
        never written here."""
        out = {cat: self.data[cat] for cat in CATS}
        out['_ui'] = self.ui
        out['_deleted'] = self.deleted
        _write_json(self.path_own, out)

    #------------- data operations

    def find(self, cat, path):
        key = _norm_path(path)
        for i, it in enumerate(self.data[cat]):
            if _norm_path(it['path']) == key:
                return i
        return -1

    def add(self, cat, path, name=None):
        """Returns (added, message)."""
        if self._add_if_new(cat, path, name):
            self.save()
            return True, None
        return False, _('Already in favorites: ') + str(path)

    def remove_at(self, cat, index):
        if 0 <= index < len(self.data[cat]):
            path = self.data[cat][index]['path']
            del self.data[cat][index]
            # remember the deletion so the one-way import from
            # cuda_favorites.json does not bring the entry back
            if not self._is_deleted(path):
                self.deleted.append(path)
            self.save()
            return True
        return False

    def move(self, cat, index, delta):
        """Move an entry in the DATA order (manual order); returns the new
        index, or the old one when the move is not possible."""
        lst = self.data[cat]
        new = index + delta
        if not (0 <= index < len(lst)) or not (0 <= new < len(lst)):
            return index
        lst[index], lst[new] = lst[new], lst[index]
        self.save()
        return new

    def toggle_star(self, cat, index):
        if 0 <= index < len(self.data[cat]):
            self.data[cat][index]['star'] = not self.data[cat][index]['star']
            self.save()
            return True
        return False

    def set_star(self, cat, index, value):
        """Set the star flag of one entry to the given value (used by the
        multi-selection Star command: star all / unstar all)."""
        if 0 <= index < len(self.data[cat]):
            if bool(self.data[cat][index]['star']) != bool(value):
                self.data[cat][index]['star'] = bool(value)
                self.save()
            return True
        return False

    def rename(self, cat, index, name):
        if 0 <= index < len(self.data[cat]):
            self.data[cat][index]['name'] = name
            self.save()
            return True
        return False

    #------------- views (sorted/filtered)

    def view(self, cat, sort_col, sort_desc, flt=''):
        """List of (data_index, item) for one category, filtered and
        sorted. 'manual' sort_col = the stored (custom) order."""
        flt = (flt or '').strip().lower()
        pairs = [(i, it) for (i, it) in enumerate(self.data[cat])]
        if flt:
            pairs = [(i, it) for (i, it) in pairs
                     if flt in it['name'].lower() or flt in it['path'].lower()]

        if sort_col == 'manual' or sort_col not in SORT_COLS:
            return pairs

        # two stable passes: secondary key first, then the primary key
        # (Python sort is stable, ties keep the secondary order even
        # with reverse=True)
        if sort_col == 'star':
            pairs.sort(key=lambda p: (p[1]['name'].lower(), p[1]['path'].lower()))
            pairs.sort(key=lambda p: not p[1]['star'], reverse=sort_desc)
        elif sort_col == 'name':
            pairs.sort(key=lambda p: p[1]['path'].lower())
            pairs.sort(key=lambda p: p[1]['name'].lower(), reverse=sort_desc)
        else:  # 'path'
            pairs.sort(key=lambda p: p[1]['name'].lower())
            pairs.sort(key=lambda p: p[1]['path'].lower(), reverse=sort_desc)
        return pairs

    def view_all(self, flt=''):
        """List of (category, data_index, item) across all categories,
        for the 'Find favorite' picker: projects, then files, then
        folders; starred first, then by name inside each."""
        flt = (flt or '').strip().lower()
        res = []
        for cat in CATS:
            for i, it in enumerate(self.data[cat]):
                if flt and flt not in it['name'].lower() and flt not in it['path'].lower():
                    continue
                res.append((cat, i, it))
            res_sorted = [p for p in res if p[0] == cat]
            res_sorted.sort(key=lambda p: (p[2]['name'].lower(), p[2]['path'].lower()))
            res_sorted.sort(key=lambda p: not p[2]['star'])
            res = [p for p in res if p[0] != cat] + res_sorted
        return res

    def counts(self, cat):
        items = self.data[cat]
        return len(items), sum(1 for it in items if it['star'])


#: module-level core, created lazily
_core = None
_form = None       # singleton of the main window
_finder = None     # current 'find favorite' modal dialog

#: window size remembered ONLY while CudaText runs (never saved to the
#: json - see the module docstring); (w, h) of the last NORMAL state
_last_size = None


def get_core():
    global _core
    if _core is None:
        _core = FavoritesCore()
    return _core


def init_on_start():
    """Called once when the package is imported at CudaText startup:
    load our data and run the one-way import from cuda_favorites.json
    (silently - no status-bar message at startup). Never raises: a
    broken or missing data file must not break the Project Manager."""
    try:
        get_core().load()
    except Exception as ex:
        print('Favorites: startup load failed: ' + str(ex))


def refresh_core():
    """(Re)load + one-way import from cuda_favorites.json; returns
    (core, imported_count). Used when a favorites window is opened."""
    core = get_core()
    imported = core.load()
    if imported:
        msg_status(_('Favorites: imported {} item(s) from cuda_favorites').format(imported))
    return core, imported


#==========================================================
# main window
#==========================================================

class FavoritesForm:

    def __init__(self, core):
        self.core = core
        self.h = 0
        self.h_list = 0        # handle of the owner-drawn listbox
        self.list_ctl_index = -1
        self._header_on = False # listbox header was set (needs CudaText 1.132+)

        self.tab = self._ui_get('tab', 0)
        self.sort_col = self._ui_get('sort_col', DEFAULT_SORT)
        self.sort_desc = bool(self._ui_get('sort_desc', False))
        self.filter = ''
        self.view = []          # [(data_index, item)] for the active tab
        self.rows = []          # what is drawn: dicts with cached dead/checked
        self.selected_data = -1 # data index to re-select after refill
        self.checked = set()    # paths of the checked (multi-select) rows
        self._closing = False

    def _ui_get(self, key, default):
        try:
            val = self.core.ui.get(key, default)
            return default if val is None else val
        except Exception:
            return default

    def cat(self):
        return CATS[self.tab] if 0 <= self.tab < len(CATS) else 'files'

    #------------- dialog construction

    def _ctl(self, kind, name, prop):
        n = dlg_proc(self.h, DLG_CTL_ADD, prop=kind)
        if n is None:
            return None
        prop = dict(prop)
        prop['name'] = name
        dlg_proc(self.h, DLG_CTL_PROP_SET, index=n, prop=prop)
        return n

    def build(self):
        BH = app_proc(PROC_GET_GUI_HEIGHT, 'button') or 30
        EH = app_proc(PROC_GET_GUI_HEIGHT, 'edit') or 28

        # window size: remembered only while CudaText runs (the maximized
        # size is never restored from disk - see the module docstring)
        w, h = FONT_W, FONT_H
        if _last_size:
            w = max(FORM_MIN_W, _last_size[0])
            h = max(FORM_MIN_H, _last_size[1])

        # scaled column widths (same factor as the scaled listbox font)
        scale = _ui_scale()
        self.chk_w = int(COL_CHK_W * scale)
        self.star_w = int(COL_STAR_W * scale)
        self.name_w = int(COL_NAME_W * scale)

        self.h = dlg_proc(0, DLG_CREATE)
        dlg_proc(self.h, DLG_PROP_SET, prop={
            'cap': _('Favorites'),
            'w': w,
            'h': h,
            'w_min': FORM_MIN_W,
            'h_min': FORM_MIN_H,
            'border': DBORDER_SIZE,     # resizable + minimize/maximize
            'keypreview': True,         # needed for on_key_down
            'topmost': True,            # stay above the CudaText window
            'on_key_down': self.form_on_key,
            'on_close': self.form_on_close,
            'on_resize': self.form_on_resize,
        })

        #---- left vertical button column ----
        y = 8
        self._ctl('button', 'btn_open', {
            'cap': _('&Open'),
            'x': BTN_X, 'y': y, 'w': BTN_W, 'h': BH,
            'tab_order': 3,
            'hint': _('Open the selected favorite\rEnter and double-click do the same.\rProjects open in Project Manager, files in the editor,\rfolders in the OS file manager'),
            'on_change': self.btn_open_click,
        })
        y += BH + BTN_GAP
        self._ctl('button', 'btn_add', {
            'cap': _('Add&...'),
            'x': BTN_X, 'y': y, 'w': BTN_W, 'h': BH,
            'tab_order': 4,
            'hint': _('Add a new favorite: project, file or folder'),
            'on_change': self.btn_add_click,
        })
        y += BH + BTN_GAP
        self._ctl('button', 'btn_add_opened', {
            'cap': _('Add &opened...'),
            'x': BTN_X, 'y': y, 'w': BTN_W, 'h': BH,
            'tab_order': 5,
            'hint': _('Add the opened project, the current editor file,\ror the folder of the current file - a menu lets you choose'),
            'on_change': self.btn_add_opened_click,
        })

        y += BH + GRP_GAP  # visible gap between the groups
        self._ctl('button', 'btn_up', {
            'cap': _('Move &up'),
            'x': BTN_X, 'y': y, 'w': BTN_W, 'h': BH,
            'tab_order': 6,
            'hint': _('Move the selected favorite up\r(switches the list to manual order)'),
            'on_change': self.btn_up_click,
        })
        y += BH + BTN_GAP
        self._ctl('button', 'btn_down', {
            'cap': _('Move do&wn'),
            'x': BTN_X, 'y': y, 'w': BTN_W, 'h': BH,
            'tab_order': 7,
            'hint': _('Move the selected favorite down\r(switches the list to manual order)'),
            'on_change': self.btn_down_click,
        })

        y += BH + GRP_GAP  # visible gap between the groups
        self._ctl('button', 'btn_del', {
            'cap': _('&Delete'),
            'x': BTN_X, 'y': y, 'w': BTN_W, 'h': BH,
            'tab_order': 8,
            'hint': _('Delete the selected favorite (Del key)'),
            'on_change': self.btn_del_click,
        })

        # Close is anchored to the bottom of the window
        self._ctl('button', 'btn_close', {
            'cap': _('Close'),
            'x': BTN_X, 'y': h - BH - 8, 'w': BTN_W, 'h': BH,
            'tab_order': 9,
            'a_t': None,          # detach top anchor
            'a_b': ('', ']'),     # stick to the bottom of the form
            'sp_b': 8,
            'on_change': self.close,
        })

        #---- right column: filter, tabs, list, status ----
        rx = BTN_X + BTN_W + 8

        self._ctl('edit', 'f_filter', {
            'x': rx, 'y': 8, 'w': w - rx - 8, 'h': EH,
            'tab_order': 0,
            'act': True,
            'texthint': _('Filter by name or path...'),
            'hint': _('Quickly filter the favorites list.\rEsc clears the filter'),
            'a_r': ('', ']'),
            'sp_r': 8,
            'on_change': self.on_filter_change,
        })

        self._ctl('tabs', 'tabs', {
            'x': rx, 'y': 8 + EH + BTN_GAP, 'w': w - rx - 8, 'h': BH,
            'tab_order': 1,
            'act': True,
            'items': '\t'.join([_('Projects'), _('Files'), _('Folders')]),
            'val': str(self.tab),
            'a_t': ('f_filter', ']'),
            'sp_t': BTN_GAP,
            'a_r': ('', ']'),
            'sp_r': 8,
            'on_change': self.on_tab_change,
        })

        # the status label must be created BEFORE the list: the list's
        # bottom anchor references it by name
        self._ctl('label', 'lbl_status', {
            'x': rx, 'y': h - 8 - 22, 'w': w - rx - 8, 'h': 22,
            'a_b': ('', ']'),
            'sp_b': 8,
            'a_r': ('', ']'),
            'sp_r': 8,
            'cap': '',
        })

        # owner-drawn list: the plain listview control of CudaText
        # dialogs cannot color single rows (needed for the RED text of
        # dead paths), so the list is a 'listbox_ex' painted here -
        # checkbox / star / name / path columns, per-row colors
        n = self._ctl('listbox_ex', 'list', {
            'x': rx, 'y': 8 + EH + BTN_GAP + BH + BTN_GAP,
            'w': w - rx - 8, 'h': h - (8 + EH + BTN_GAP + BH + BTN_GAP) - 8 - 22 - BTN_GAP,
            'tab_order': 2,
            'a_t': ('tabs', ']'),
            'sp_t': BTN_GAP,
            'a_b': ('lbl_status', '['),
            'sp_b': BTN_GAP,
            'a_r': ('', ']'),
            'sp_r': 8,
            'on_change': self.list_on_change,
            'on_click': self.list_on_click,
            'on_click_dbl': self.list_on_dbl_click,
            'on_click_header': self.list_on_header_click,
            'on_draw_item': self.list_draw_item,
            'on_menu': self.list_on_menu,
        })
        self.list_ctl_index = n if n is not None else -1
        self.h_list = dlg_proc(self.h, DLG_CTL_HANDLE, name='list')
        try:
            listbox_proc(self.h_list, LISTBOX_SET_DRAWN, 1)
            listbox_proc(self.h_list, LISTBOX_SET_COLUMNS,
                         text=[self.chk_w, self.star_w, self.name_w, -1])
        except Exception as ex:
            print('Favorites: cannot init the owner-drawn list: ', ex)

        dlg_proc(self.h, DLG_SCALE)
        self.fill_list()

    #------------- showing / closing

    def show(self):
        if self.h:
            dlg_proc(self.h, DLG_FOCUS)
            dlg_proc(self.h, DLG_TO_FRONT)
            return
        self.build()
        dlg_proc(self.h, DLG_SHOW_NONMODAL)
        # focus the filter: type-to-filter right away; Ctrl+1..9 etc work
        # from any control because the form has keypreview=True
        dlg_proc(self.h, DLG_CTL_FOCUS, name='f_filter')

    def close(self, id_dlg=0, id_ctl=0, data=''):
        if not self.h or self._closing:
            return
        self._closing = True
        self.save_ui_state()
        dlg_proc(self.h, DLG_HIDE)
        # free the dialog a bit later: freeing inside the callback
        # crashes LCL (same workaround as the kv-dlg library uses)
        h = self.h
        self.h = 0
        timer_proc(TIMER_START_ONE,
                   lambda tag=None: dlg_proc(h, DLG_FREE), 200)

    def save_ui_state(self):
        # NOTE: the window size is deliberately NOT saved to the json -
        # see the module docstring (a form recreated with the pixel size
        # of a maximized window ends up with its bottom off-screen).
        # _last_size is kept in memory by form_on_resize instead.
        self.core.ui['tab'] = self.tab
        self.core.ui['sort_col'] = self.sort_col
        self.core.ui['sort_desc'] = self.sort_desc
        self.core.save()

    def form_on_close(self, id_dlg, id_ctl=0, data=''):
        global _form
        self.close()
        if _form is self:
            _form = None

    def form_on_resize(self, id_dlg, id_ctl=0, data=''):
        # remember the size of the NORMAL window state (the size of a
        # maximized window is never remembered - restoring it re-created
        # the 'bottom is eaten' bug after a CudaText restart)
        global _last_size
        if self._closing or not self.h:
            return
        try:
            props = dlg_proc(self.h, DLG_PROP_GET) or {}
            if str(props.get('form_state', 'normal')) == 'normal':
                w = int(props.get('w', 0))
                h = int(props.get('h', 0))
                if w >= FORM_MIN_W and h >= FORM_MIN_H:
                    _last_size = (w, h)
        except Exception:
            pass

    #------------- list filling

    def _sort_caption(self, mode, base):
        """Column caption with the sort-direction marker."""
        if self.sort_col == 'manual' or mode != self.sort_col:
            return base
        return base + (' ↓' if self.sort_desc else ' ↑')

    def update_header(self):
        """Set the listbox header line (tab-separated column captions).
        Clicking the checkbox column checks/unchecks all rows; clicking
        the other columns changes the sort."""
        if not self.h_list:
            return
        try:
            listbox_proc(self.h_list, LISTBOX_SET_HEADER, text='\t'.join([
                GLYPH_CHECKED[0],  # '✓': click = check/uncheck all
                self._sort_caption('star', '★'),
                self._sort_caption('name', _('Name')),
                self._sort_caption('path', _('Path')),
            ]))
            self._header_on = True
        except Exception:
            self._header_on = False

    def fill_list(self, keep_selection=True):
        if not self.h:
            return
        cat = self.cat()
        self.view = self.core.view(cat, self.sort_col, self.sort_desc, self.filter)

        # what is drawn: dead/checked states are computed ONCE here, not
        # on every repaint (os.path.exists on each paint would be slow)
        self.rows = [{
            'data_idx': data_idx,
            'path': it['path'],
            'name': it['name'],
            'star': bool(it['star']),
            'checked': it['path'] in self.checked,
            'dead': not os.path.exists(it['path']),
        } for (data_idx, it) in self.view]

        # rows hidden by the filter must not stay checked (acting on
        # checked rows must never touch invisible items)
        visible = set(r['path'] for r in self.rows)
        self.checked &= visible
        for r in self.rows:
            r['checked'] = r['path'] in self.checked

        try:
            listbox_proc(self.h_list, LISTBOX_DELETE_ALL)
            for r in self.rows:
                listbox_proc(self.h_list, LISTBOX_ADD, -1, r['name'])
        except Exception:
            pass

        # restore selection
        sel = -1
        if keep_selection:
            for i, r in enumerate(self.rows):
                if r['data_idx'] == self.selected_data:
                    sel = i
                    break
        if sel < 0:
            self.selected_data = self.rows[0]['data_idx'] if self.rows else -1
            sel = 0 if self.rows else -1
        if sel >= 0:
            try:
                listbox_proc(self.h_list, LISTBOX_SET_SEL, sel)
            except Exception:
                pass

        self.update_header()
        self.update_status()

    def update_status(self):
        if not self.h:
            return
        cat = self.cat()
        total, starred = self.core.counts(cat)
        shown = len(self.rows)
        if self.sort_col == 'manual':
            sort_info = _('sort: manual order')
        elif self.sort_col == 'star':
            sort_info = _('sort: ★, then name')
        elif self.sort_col == 'name':
            sort_info = _('sort: name') + (' ↓' if self.sort_desc else ' ↑')
        else:
            sort_info = _('sort: path') + (' ↓' if self.sort_desc else ' ↑')

        parts = []
        if self.filter:
            parts.append(_('{} of {} shown').format(shown, total))
        else:
            parts.append(_('{} item(s), {} starred').format(total, starred))
        if self.checked:
            parts.append(_('{} checked').format(len(self.checked)))
        parts.append(sort_info)
        parts.append(_('Ctrl+1..9 / Alt+1..9 open item #'))
        try:
            dlg_proc(self.h, DLG_CTL_PROP_SET, name='lbl_status', prop={
                'cap': '   ·   '.join(parts),
            })
        except Exception:
            pass

        # enable/disable the buttons which need a target row
        has_sel = self.selected_row() is not None
        for name in ('btn_open', 'btn_up', 'btn_down', 'btn_del'):
            try:
                dlg_proc(self.h, DLG_CTL_PROP_SET, name=name, prop={'en': has_sel})
            except Exception:
                pass

    #------------- selection / view helpers

    def _sel_index(self):
        try:
            return int(listbox_proc(self.h_list, LISTBOX_GET_SEL))
        except Exception:
            return -1

    def selected_row(self):
        """The row dict of the selection, or None."""
        i = self._sel_index()
        if 0 <= i < len(self.rows):
            return self.rows[i]
        return None

    def _row_at(self, x, y):
        """Row index under the given listbox point, or -1."""
        if not self.h_list or not self.rows:
            return -1
        try:
            item_h = int(listbox_proc(self.h_list, LISTBOX_GET_ITEM_H)) or 20
            top = int(listbox_proc(self.h_list, LISTBOX_GET_TOP))
        except Exception:
            return -1
        yy = y - (item_h if self._header_on else 0)
        if yy < 0:
            return -1
        index = top + yy // item_h
        if 0 <= index < len(self.rows):
            return index
        return -1

    def _list_focused(self):
        """True when the keyboard focus is on the list itself (Space /
        Ctrl+A must not fire while the user types in the filter edit or
        presses a button with the keyboard)."""
        if self.list_ctl_index < 0 or not self.h:
            return False
        try:
            props = dlg_proc(self.h, DLG_PROP_GET) or {}
            return int(props.get('focused', -1)) == self.list_ctl_index
        except Exception:
            return False

    def _list_repaint(self):
        """Force a repaint of the owner-drawn list (LISTBOX_SET_COLUMNS
        always invalidates the control; the sizes stay the same)."""
        if not self.h_list:
            return
        try:
            listbox_proc(self.h_list, LISTBOX_SET_COLUMNS,
                         text=[self.chk_w, self.star_w, self.name_w, -1])
        except Exception:
            pass

    def targets(self):
        """Rows (in view order) the next action applies to: all checked
        rows, or the selected row when nothing is checked."""
        if self.checked:
            return [r for r in self.rows if r['checked']]
        row = self.selected_row()
        return [row] if row else []

    #------------- owner drawing

    def _draw_text(self, canvas, text, x, y1, row_h, centered=False):
        try:
            size = canvas_proc(canvas, CANVAS_GET_TEXT_SIZE, text=text)
            tw, th = int(size[0]), int(size[1])
        except Exception:
            tw, th = 0, row_h
        if centered:
            x = x - tw // 2
        canvas_proc(canvas, CANVAS_TEXT, text=text,
                    x=x, y=y1 + max(0, (row_h - th) // 2))

    def _fit_text(self, canvas, text, max_w):
        """Truncate text with '…' so it fits into max_w pixels."""
        if max_w <= 12 or not text:
            return ''
        try:
            size = canvas_proc(canvas, CANVAS_GET_TEXT_SIZE, text=text)
            if size and int(size[0]) <= max_w:
                return text
            while text:
                text = text[:-1]
                size = canvas_proc(canvas, CANVAS_GET_TEXT_SIZE, text=text + '…')
                if not size or int(size[0]) <= max_w:
                    break
        except Exception:
            return text
        return text + '…'

    def list_draw_item(self, id_dlg, id_ctl, data='', info=''):
        """Owner-drawn painting of one row: checkbox glyph, star glyph,
        name, path. Rows whose path is missing on disk are RED; the
        selected row gets the theme's selection colors. Must never
        raise: an exception inside LCL painting would break the form."""
        if self._closing or not self.h_list:
            return
        try:
            d = data if isinstance(data, dict) else {}
            canvas = int(d.get('canvas', 0) or 0)
            index = int(d.get('index', -1))
            rect = d.get('rect') or (0, 0, 0, 0)
            x1, y1, x2, y2 = int(rect[0]), int(rect[1]), int(rect[2]), int(rect[3])
        except Exception:
            return
        if not canvas or not (0 <= index < len(self.rows)):
            return
        row = self.rows[index]
        colors = _theme_colors()
        sel = (self._sel_index() == index)
        try:
            # background
            canvas_proc(canvas, CANVAS_SET_BRUSH,
                        color=colors['sel_bg'] if sel else colors['bg'],
                        style=BRUSH_SOLID)
            canvas_proc(canvas, CANVAS_RECT_FILL, x=x1, y=y1, x2=x2, y2=y2)
            canvas_proc(canvas, CANVAS_SET_BRUSH, style=BRUSH_CLEAR)
            # font color: RED for dead paths, else theme colors
            canvas_proc(canvas, CANVAS_SET_FONT,
                        color=COLOR_DEAD if row['dead']
                        else (colors['sel_font'] if sel else colors['font']))

            row_h = y2 - y1
            # checkbox column (before the star column)
            self._draw_text(canvas,
                            GLYPH_CHECKED if row['checked'] else GLYPH_UNCHECKED,
                            x1 + self.chk_w // 2, y1, row_h, centered=True)
            # star column
            self._draw_text(canvas,
                            '★' if row['star'] else '☆',
                            x1 + self.chk_w + self.star_w // 2, y1, row_h,
                            centered=True)
            # name column (dead paths get the '? ' prefix)
            name = ('? ' if row['dead'] else '') + row['name']
            name = self._fit_text(canvas, name, self.name_w - 2 * COL_TEXT_PAD)
            self._draw_text(canvas, name,
                            x1 + self.chk_w + self.star_w + COL_TEXT_PAD,
                            y1, row_h)
            # path column: eats the rest of the width
            self._draw_text(canvas, row['path'],
                            x1 + self.chk_w + self.star_w + self.name_w + COL_TEXT_PAD,
                            y1, row_h)
        except Exception:
            pass

    #------------- events

    def on_filter_change(self, id_dlg, id_ctl, data='', info=''):
        try:
            props = dlg_proc(self.h, DLG_CTL_PROP_GET, name='f_filter') or {}
            self.filter = str(props.get('val', '') or '')
        except Exception:
            self.filter = ''
        self.fill_list(keep_selection=False)

    def on_tab_change(self, id_dlg, id_ctl, data='', info=''):
        try:
            props = dlg_proc(self.h, DLG_CTL_PROP_GET, name='tabs') or {}
            self.tab = int(props.get('val', '0') or 0)
        except Exception:
            pass
        self.selected_data = -1
        self.checked = set()  # checks are per tab
        self.fill_list(keep_selection=False)

    def list_on_change(self, id_dlg, id_ctl, data='', info=''):
        """Selection in the list moved (also fires right after a click
        selected the clicked row)."""
        row = self.selected_row()
        self.selected_data = row['data_idx'] if row else -1
        self.update_status()

    def list_on_header_click(self, id_dlg, id_ctl, data='', info=''):
        """Click on a column header: the checkbox column checks/unchecks
        all rows; the others sort ascending, then descending, then switch
        to manual (custom) order."""
        try:
            col = int(data)
        except Exception:
            return
        if col == COL_CHK:
            self.check_all_toggle()
            return
        mode = MOD_COL.get(col)
        if not mode:
            return
        if self.sort_col != mode:
            self.sort_col = mode
            self.sort_desc = False
        elif not self.sort_desc:
            self.sort_desc = True
        else:
            self.sort_col = 'manual'
            msg_status(_('Favorites: manual order - Move up/down reorders the list'))
        self.fill_list()

    def list_on_click(self, id_dlg, id_ctl, data='', info=''):
        """Single click: the checkbox (first) column toggles the row's
        checkbox, the star (second) column toggles the star flag.
        Header clicks go to on_click_header, they don't get here."""
        if not isinstance(data, (tuple, list)) or len(data) != 2:
            return
        try:
            x = int(data[0])
            y = int(data[1])
        except Exception:
            return
        index = self._row_at(x, y)
        if index < 0:
            return
        if x < self.chk_w:
            self.toggle_checked(index)
            return
        if x < self.chk_w + self.star_w:
            row = self.rows[index]
            data_idx = self.core.find(self.cat(), row['path'])
            if data_idx >= 0:
                self.core.toggle_star(self.cat(), data_idx)
                self.fill_list()
            return
        # click on the text columns: TATListbox already moved the
        # selection; nothing else to do

    def list_on_dbl_click(self, id_dlg, id_ctl, data='', info=''):
        if not isinstance(data, (tuple, list)) or len(data) != 2:
            return
        try:
            x = int(data[0])
            y = int(data[1])
        except Exception:
            return
        index = self._row_at(x, y)
        if index < 0:
            return
        if x < self.chk_w + self.star_w:
            return  # checkbox/star cell: single clicks already acted
        self.open_targets()

    def list_on_menu(self, id_dlg, id_ctl, data='', info=''):
        """Right-click context menu of the favorites list. Commands act
        on ALL checked rows when any row is checked, else on the
        selected row."""
        rows = self.targets()
        n = len(rows)
        multi = ' ({})'.format(_('{} checked').format(n)) if n > 1 else ''
        menu = menu_proc(0, MENU_CREATE)
        menu_proc(menu, MENU_CLEAR)

        def add(caption, action, enabled=True):
            mid = menu_proc(menu, MENU_ADD,
                            command=('module=cuda_project_man;cmd=_favorites_ctx;info={};'.format(action)
                                     if action else ''),
                            caption=caption)
            if not enabled:
                menu_proc(mid, MENU_SET_ENABLED, command=False)
            return mid

        add(_('Open') + ' (Enter)' + multi, 'open', n > 0)
        add(_('Reveal in file manager') + multi, 'reveal', n > 0)
        add(_('Copy path') + ('s' if n > 1 else '') + multi, 'copy', n > 0)
        menu_proc(menu, MENU_ADD, caption='-')
        add(((_('Unstar') if (n and all(r['star'] for r in rows)) else _('Star'))
             + ' ★' + multi), 'star', n > 0)
        add(_('Rename...') + ' (F2)' + multi, 'rename', n > 0)
        add(_('Delete') + ' (Del)' + multi, 'delete', n > 0)
        menu_proc(menu, MENU_ADD, caption='-')
        add(_('Check all'), 'check_all')
        add(_('Uncheck all'), 'uncheck_all')
        menu_proc(menu, MENU_ADD, caption='-')
        add(_('Add current file'), 'add_file')
        add(_('Add folder of current file'), 'add_dir')
        add(_('Add current project'), 'add_proj')
        menu_proc(menu, MENU_SHOW, command='')

    #------------- form-level keys

    def form_on_key(self, id_dlg, id_ctl, data=''):
        """id_ctl = key code, data = modifiers ('c','a','s','m')."""
        key = id_ctl
        mods = data or ''

        # Ctrl+1..9 / Alt+1..9: open the Nth visible item
        if ('c' in mods or 'a' in mods) and not ('c' in mods and 'a' in mods) \
           and (ord('1') <= key <= ord('9') or VK_NUMPAD1 <= key <= VK_NUMPAD9):
            n = key - ord('1') if key <= ord('9') else key - VK_NUMPAD1
            if 0 <= n < len(self.rows):
                open_item(self.rows[n]['path'])
                self.focus_again()
            else:
                msg_status(_('Favorites: no item at #{}').format(n + 1))
            return False  # block key

        # plain keys
        if 'a' not in mods and 'c' not in mods and 's' not in mods and 'm' not in mods:
            if key == VK_ENTER:
                self.open_targets()
                return False
            if key == VK_DELETE:
                self.delete_targets()
                return False
            if key == VK_F2:
                self.rename_targets()
                return False
            if key == VK_SPACE and self._list_focused():
                # Space: toggle the checkbox of the selected row (only
                # when the list itself has focus - Space must keep
                # pressing a focused button)
                i = self._sel_index()
                if 0 <= i < len(self.rows):
                    self.toggle_checked(i)
                return False
            if key == VK_ESCAPE:
                if self.filter:
                    # first Esc clears the filter, the next one closes
                    # the form (the built-in Esc handling closes it when
                    # this handler does not block the key).
                    # NOTE: setting 'val' programmatically does NOT fire
                    # the edit's on_change event, so the list must be
                    # refilled here too - otherwise it kept showing the
                    # old filtered rows
                    try:
                        dlg_proc(self.h, DLG_CTL_PROP_SET, name='f_filter', prop={'val': ''})
                    except Exception:
                        pass
                    self.filter = ''
                    self.fill_list(keep_selection=False)
                    return False
                return None  # let the form close itself

        # Ctrl+A: check all visible rows (uncheck when all are checked);
        # only with the list focused, so the edit's select-all still works
        if 'c' in mods and 'a' not in mods and 's' not in mods and 'm' not in mods \
           and key in (ord('A'), ord('a')) and self._list_focused():
            self.check_all_toggle()
            return False

        # Alt+Up / Alt+Down: move the item(s)
        if 'a' in mods and 'c' not in mods and 's' not in mods:
            if key == VK_UP:
                self.move_targets(-1)
                return False
            if key == VK_DOWN:
                self.move_targets(+1)
                return False

        return None

    def focus_again(self):
        """Give the keyboard focus back to the favorites window after
        an editor tab was opened."""
        if self.h:
            dlg_proc(self.h, DLG_FOCUS)

    #------------- multi-selection helpers

    def toggle_checked(self, index):
        """Toggle the checkbox of the row at the given VIEW index."""
        if not (0 <= index < len(self.rows)):
            return
        row = self.rows[index]
        if row['checked']:
            self.checked.discard(row['path'])
            row['checked'] = False
        else:
            self.checked.add(row['path'])
            row['checked'] = True
        self._list_repaint()
        self.update_status()

    def check_all(self, value):
        for r in self.rows:
            r['checked'] = bool(value)
            if value:
                self.checked.add(r['path'])
            else:
                self.checked.discard(r['path'])
        self._list_repaint()
        self.update_status()

    def check_all_toggle(self):
        """Ctrl+A / the '✓' header: check everything, or uncheck when
        everything is checked already."""
        self.check_all(not (self.rows and all(r['checked'] for r in self.rows)))

    #------------- actions (act on all checked rows, else the selection)

    def open_targets(self):
        rows = self.targets()
        if not rows:
            msg_status(_('Favorites: nothing selected'))
            return
        for r in rows:
            open_item(r['path'])
        self.focus_again()

    def delete_targets(self):
        rows = self.targets()
        if not rows:
            return
        if len(rows) > 1:
            if msg_box(_('Favorites: delete {} item(s)?').format(len(rows)),
                       MB_OKCANCEL + MB_ICONQUESTION) != ID_OK:
                return
        cat = self.cat()
        # delete by descending data index so the indexes stay valid
        for r in sorted(rows, key=lambda rr: self.core.find(cat, rr['path']),
                        reverse=True):
            data_idx = self.core.find(cat, r['path'])
            if data_idx >= 0:
                self.core.remove_at(cat, data_idx)
            self.checked.discard(r['path'])
        msg_status(_('Favorites: removed {} item(s)').format(len(rows)))
        self.selected_data = -1
        self.fill_list(keep_selection=False)

    def rename_targets(self):
        rows = self.targets()
        if not rows:
            return
        cat = self.cat()
        for r in rows:
            new = dlg_input(_('New name of the favorite:'), r['name'])
            if new is None:
                break  # Cancel stops renaming the rest
            new = new.strip()
            if new and new != r['name']:
                data_idx = self.core.find(cat, r['path'])
                if data_idx >= 0:
                    self.core.rename(cat, data_idx, new)
        self.fill_list()

    def move_targets(self, delta):
        rows = self.targets()
        if not rows:
            return
        if self.sort_col != 'manual':
            self.sort_col = 'manual'
            msg_status(_('Favorites: manual order - Move up/down reorders the list'))
        cat = self.cat()
        # move the checked block as a block: process upwards moves
        # top-down, downwards moves bottom-up (so the internal order is
        # kept), and skip an item when the neighbor in that direction
        # belongs to the block too - it moved itself (or is blocked at
        # the edge), so moving would tear the block apart
        target_paths = set(r['path'] for r in rows)
        data = self.core.data[cat]
        order = rows if delta < 0 else list(reversed(rows))
        moved = False
        for r in order:
            idx = self.core.find(cat, r['path'])
            if idx < 0:
                continue
            new = idx + delta
            if not (0 <= new < len(data)):
                continue  # at the edge of the list
            if data[new]['path'] in target_paths:
                continue  # the neighbor is part of the moving block
            if self.core.move(cat, idx, delta) != idx:
                moved = True
        if not moved:
            msg_status(_('Favorites: already at the top') if delta < 0
                       else _('Favorites: already at the bottom'))
        self.selected_data = self.core.find(cat, rows[0]['path'])
        self.fill_list()

    def star_targets(self):
        rows = self.targets()
        if not rows:
            return
        cat = self.cat()
        # if any target is unstarred -> star all, else unstar all
        value = not all(r['star'] for r in rows)
        for r in rows:
            data_idx = self.core.find(cat, r['path'])
            if data_idx >= 0:
                self.core.set_star(cat, data_idx, value)
        self.fill_list()

    def copy_paths(self):
        rows = self.targets()
        if not rows:
            return
        app_proc(PROC_SET_CLIP, '\n'.join(r['path'] for r in rows))
        msg_status(_('Favorites: copied {} path(s)').format(len(rows)))

    def reveal_targets(self):
        for r in self.targets():
            reveal_in_file_manager(r['path'])

    def btn_open_click(self, id_dlg, id_ctl, data='', info=''):
        self.open_targets()

    def btn_add_click(self, id_dlg, id_ctl, data='', info=''):
        res = dlg_menu(DMENU_LIST, [_('Project...'), _('File...'), _('Folder...')],
                       caption=_('Add favorite'))
        if res is None:
            return
        if res == 0:
            fn = dlg_file(True, '', '', _('CudaText projects') + '|*' + PROJECT_EXTENSION)
            cat = 'projects'
        elif res == 1:
            fn = dlg_file(True, '', '', '')
            cat = 'files'
        else:
            fn = dlg_dir('')
            cat = 'folders'
        if not fn:
            return
        if os.path.isdir(fn) and cat == 'files':
            cat = 'folders'
        self._do_add(cat, fn)

    def btn_add_opened_click(self, id_dlg, id_ctl, data='', info=''):
        """Same chooser as Add...: a menu with the opened project, the
        current editor file, and the folder of the current file."""
        res = choose_opened_favorite()
        if res is None:
            return
        cat, path = res
        self._do_add(cat, path)

    def _do_add(self, cat, path):
        added, err = self.core.add(cat, path)
        if added:
            msg_status(_('Favorites: added ') + str(path))
            self.tab = CATS.index(cat)
            try:
                dlg_proc(self.h, DLG_CTL_PROP_SET, name='tabs', prop={'val': str(self.tab)})
            except Exception:
                pass
            self.selected_data = self.core.find(cat, path)
            self.fill_list()
        elif err:
            msg_status(err)

    def btn_up_click(self, id_dlg, id_ctl, data='', info=''):
        self.move_targets(-1)

    def btn_down_click(self, id_dlg, id_ctl, data='', info=''):
        self.move_targets(+1)

    def btn_del_click(self, id_dlg, id_ctl, data='', info=''):
        self.delete_targets()

    #------------- context-menu actions (called from Command._favorites_ctx)

    def context_action(self, action):
        if action == 'add_file':
            fn = ed.get_prop(PROP_FN, '*') or ''
            if fn:
                self._do_add('files', fn)
            else:
                msg_status(_('Favorites: current tab is untitled'))
            return
        if action == 'add_dir':
            fn = ed.get_prop(PROP_FN, '*') or ''
            if fn:
                self._do_add('folders', os.path.dirname(fn))
            else:
                msg_status(_('Favorites: current tab is untitled'))
            return
        if action == 'add_proj':
            proj = get_current_project()
            if proj:
                self._do_add('projects', proj)
            else:
                msg_status(_('Favorites: no opened project'))
            return
        if action == 'check_all':
            self.check_all(True)
            return
        if action == 'uncheck_all':
            self.check_all(False)
            return
        if not self.targets():
            return
        if action == 'open':
            self.open_targets()
        elif action == 'reveal':
            self.reveal_targets()
        elif action == 'copy':
            self.copy_paths()
        elif action == 'star':
            self.star_targets()
        elif action == 'rename':
            self.rename_targets()
        elif action == 'delete':
            self.delete_targets()


#==========================================================
# "Find favorite" quick picker (modal)
#==========================================================

class FavoritesFinder:
    """Small modal dialog: type-to-filter over ALL favorites (projects,
    files, folders), Enter/double-click opens the selection."""

    W = 660
    H = 400

    def __init__(self, core):
        self.core = core
        self.h = 0
        self.result = None      # path to open after the dialog closes
        self.filter = ''
        self.view = []          # [(cat, data_index, item)]

    def _ctl(self, kind, name, prop):
        n = dlg_proc(self.h, DLG_CTL_ADD, prop=kind)
        if n is None:
            return None
        prop = dict(prop)
        prop['name'] = name
        dlg_proc(self.h, DLG_CTL_PROP_SET, index=n, prop=prop)
        return n

    def show(self):
        BH = app_proc(PROC_GET_GUI_HEIGHT, 'button') or 30
        EH = app_proc(PROC_GET_GUI_HEIGHT, 'edit') or 28

        self.h = dlg_proc(0, DLG_CREATE)
        dlg_proc(self.h, DLG_PROP_SET, prop={
            'cap': _('Find favorite'),
            'w': self.W,
            'h': self.H,
            'border': DBORDER_DIALOG,
            'keypreview': True,
            'on_key_down': self.form_on_key,
        })

        self._ctl('edit', 'f_filter', {
            'x': 8, 'y': 8, 'w': self.W - 16, 'h': EH,
            'act': True,
            'texthint': _('Type to filter by name or path...'),
            'on_change': self.on_filter_change,
        })

        # buttons must be created BEFORE the listview: the listview's
        # bottom anchor references btn_ok by name
        self._ctl('button', 'btn_ok', {
            'cap': _('&Open'),
            'x': self.W - 8 - 90 - 6 - 90, 'y': self.H - BH - 8, 'w': 90, 'h': BH,
            'ex0': True,  # default for Enter
            'a_t': None,
            'a_b': ('', ']'),
            'sp_b': 8,
            'on_change': self.btn_ok_click,
        })
        self._ctl('button', 'btn_cancel', {
            'cap': _('Cancel'),
            'x': self.W - 8 - 90, 'y': self.H - BH - 8, 'w': 90, 'h': BH,
            'a_t': None,
            'a_b': ('', ']'),
            'sp_b': 8,
            'a_r': ('', ']'),
            'sp_r': 8,
            'on_change': self.btn_cancel_click,
        })

        self._ctl('listview', 'list', {
            'x': 8, 'y': 8 + EH + 6,
            'w': self.W - 16, 'h': self.H - EH - 6 - BH - 16 - 8 - 6,
            'ex0': True,
            'a_r': ('', ']'),
            'sp_r': 8,
            'a_t': ('f_filter', ']'),
            'sp_t': 6,
            'a_b': ('btn_ok', '['),
            'sp_b': 8,
            'on_click_dbl': self.list_on_dbl_click,
        })

        dlg_proc(self.h, DLG_SCALE)
        self.fill_list()
        dlg_proc(self.h, DLG_CTL_FOCUS, name='f_filter')

        dlg_proc(self.h, DLG_SHOW_MODAL)
        dlg_proc(self.h, DLG_FREE)
        self.h = 0

        # open the chosen item after the modal dialog is fully closed
        # (a timer is needed: right after DLG_SHOW_MODAL returns, the app
        # still restores focus to the previous UI group - same trick as
        # the cuda_favorites plugin uses)
        if self.result:
            path = self.result
            timer_proc(TIMER_START_ONE, lambda tag=None: open_item(path), 300)

    def fill_list(self):
        self.view = self.core.view_all(self.filter)
        rows = []
        for cat, _i, it in self.view:
            name = it['name']
            if not os.path.exists(it['path']):
                name = '? ' + name
            rows.append('\r'.join([
                '★' if it['star'] else '☆',   # UTF-8 font icons, no images
                name,
                CAT_TITLES[cat],
                it['path'],
            ]))

        items = '\t'.join(
            ['\r'.join([
                '★=C48',
                _('Name') + '=220',
                _('Type') + '=70',
                _('Path') + '=280',
            ])] + rows)

        dlg_proc(self.h, DLG_CTL_PROP_SET, name='list', prop={'items': items})
        if self.view:
            dlg_proc(self.h, DLG_CTL_PROP_SET, name='list', prop={'val': '0'})

    def selected_pair(self):
        try:
            props = dlg_proc(self.h, DLG_CTL_PROP_GET, name='list') or {}
            val = props.get('val', '')
            i = int(val) if val not in ('', None) else -1
        except Exception:
            i = -1
        if 0 <= i < len(self.view):
            return self.view[i]
        return None

    def accept(self):
        pair = self.selected_pair()
        if pair is None:
            if not self.view:
                msg_status(_('Favorites: no favorites yet'))
            return
        _cat, _i, it = pair
        self.result = it['path']
        dlg_proc(self.h, DLG_HIDE)

    def on_filter_change(self, id_dlg, id_ctl, data='', info=''):
        try:
            props = dlg_proc(self.h, DLG_CTL_PROP_GET, name='f_filter') or {}
            self.filter = str(props.get('val', '') or '')
        except Exception:
            self.filter = ''
        self.fill_list()

    def list_on_dbl_click(self, id_dlg, id_ctl, data='', info=''):
        self.accept()

    def btn_ok_click(self, id_dlg, id_ctl, data='', info=''):
        self.accept()

    def btn_cancel_click(self, id_dlg, id_ctl, data='', info=''):
        self.result = None
        dlg_proc(self.h, DLG_HIDE)

    def form_on_key(self, id_dlg, id_ctl, data=''):
        key = id_ctl
        mods = data or ''
        if ('c' in mods or 'a' in mods) and not ('c' in mods and 'a' in mods) \
           and (ord('1') <= key <= ord('9') or VK_NUMPAD1 <= key <= VK_NUMPAD9):
            n = key - ord('1') if key <= ord('9') else key - VK_NUMPAD1
            if 0 <= n < len(self.view):
                _cat, _i, it = self.view[n]
                self.result = it['path']
                dlg_proc(self.h, DLG_HIDE)
            else:
                msg_status(_('Favorites: no item at #{}').format(n + 1))
            return False
        # Esc is handled by the form itself (modal: cancels the dialog)
        return None


#==========================================================
# module API (used by cuda_project_man/__init__.py)
#==========================================================

def show_favorites():
    """Open (or focus) the Favorites window. The one-way import from
    cuda_favorites.json runs when the window is (re)created - not when
    it is only focused, and not on every list operation."""
    global _form
    if _form is None or not _form.h:
        refresh_core()
        _form = FavoritesForm(get_core())
    _form.show()
    if _form.h:
        dlg_proc(_form.h, DLG_TO_FRONT)


def find_favorite():
    """Show the 'Find favorite' quick picker (it is a favorites window
    too, so it refreshes the imported data the same way)."""
    global _finder
    refresh_core()
    core = get_core()
    if not any(core.data[cat] for cat in CATS):
        msg_status(_('Favorites: the list is empty'))
        return
    _finder = FavoritesFinder(core)
    _finder.show()
    _finder = None


def add_favorite(cat, path):
    """Add one entry with a status-bar message (used by the Project
    Manager context-menu item)."""
    core = get_core()
    added, err = core.add(cat, path)
    if added:
        msg_status(_('Favorites: added ') + str(path))
        if _form is not None and _form.h:
            _form.tab = CATS.index(cat)
            try:
                dlg_proc(_form.h, DLG_CTL_PROP_SET, name='tabs', prop={'val': str(_form.tab)})
            except Exception:
                pass
            _form.selected_data = core.find(cat, path)
            _form.fill_list()
        return True
    if err:
        msg_status(err)
    return False


def _opened_choices():
    """(caption, cat, path) for everything that can be bookmarked right
    now: the opened project, the current editor file, and the folder of
    the current file. Only the available ones are listed."""
    res = []
    proj = get_current_project()
    if proj:
        res.append((_('Opened project') + ': ' + proj, 'projects', proj))
    fn = ed.get_prop(PROP_FN, '*') or ''
    if fn:
        res.append((_('Opened file') + ': ' + fn, 'files', fn))
        res.append((_('Folder of opened file') + ': ' + os.path.dirname(fn),
                    'folders', os.path.dirname(fn)))
    return res


def choose_opened_favorite():
    """The 'Add opened...' dialog: a menu with the currently openable
    things - the opened project, the current editor file, or the folder
    of the current file (same chooser style as the Add... button).
    Returns (cat, path), or None when cancelled / nothing to add."""
    choices = _opened_choices()
    if not choices:
        msg_status(_('Favorites: no opened project, and the current tab is untitled'))
        return None
    res = dlg_menu(DMENU_LIST, [c[0] for c in choices],
                   caption=_('Add opened favorite'))
    if res is None:
        return None
    return choices[res][1], choices[res][2]


def add_opened():
    """Command 'Add opened...': a dialog to choose between the opened
    project, the opened editor file, and the folder of the opened
    file."""
    res = choose_opened_favorite()
    if res is None:
        return False
    cat, path = res
    return add_favorite(cat, path)


def add_current_file():
    fn = ed.get_prop(PROP_FN, '*') or ''
    if fn:
        return add_favorite('files', fn)
    msg_status(_('Favorites: current tab is untitled'))
    return False


def add_current_dir():
    fn = ed.get_prop(PROP_FN, '*') or ''
    if fn:
        return add_favorite('folders', os.path.dirname(fn))
    msg_status(_('Favorites: current tab is untitled'))
    return False


def add_current_project():
    proj = get_current_project()
    if proj:
        return add_favorite('projects', proj)
    msg_status(_('Favorites: no opened project'))
    return False


def context_action(action):
    """Context-menu action of the favorites list (runs the module-level
    command _favorites_ctx)."""
    global _form
    if _form is not None and _form.h:
        _form.context_action(action)
