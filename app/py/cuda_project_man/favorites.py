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
- NATIVE listview (the standard control of CudaText dialogs): the usual
  native look, and the column widths can be resized by dragging the
  header dividers; the Path column is re-stretched when the window is
  resized by 50+ px
  (an owner-drawn listbox was tried to get checkboxes + red rows for
  dead paths, but it lost the native look/behavior - reverted)
- columns: star / Name / Path / State; the star is the UTF-8 font
  glyph ☆/★ (no image files); the State column shows "?" when the
  path is dead (missing on disk) and nothing when it is fine - the
  dialog listview cannot color single rows
- clicking the star glyph cell of a row toggles the star: the
  on_click event reports the mouse position in the control, so a
  click on the star column is told apart from clicks elsewhere
- sorting: click a column header (the on_click_header event of the
  dialog API, data = column index): 1st click ascending, 2nd
  descending, 3rd switches to manual (custom) order; the right-click
  menu has the same Sort commands as a fallback
- filter field at the top filters by name and path; Esc clears it
- vertical button panel on the left: Open / Add... / Add opened...,
  Move up / Move down, Delete; Close at the bottom
- while the window is focused: Ctrl+1..9 / Alt+1..9 open the item at
  that index (index in the currently visible, filtered list), Enter
  opens the selection, Del deletes it, F2 renames it,
  Alt+Up/Down move it, Esc clears the filter (or closes the window)
- the window is only HIDDEN on close and reused on the next open, so
  its size and position are kept for the whole CudaText session; they
  reset only when CudaText restarts (the size is not saved to disk: a
  form recreated with the pixel size of a maximized window ends up
  taller than the work area)

"Add opened..." shows the same chooser as "Add...": pick the opened
project, the current editor file, or the folder of the current file.

Opening a favorite: project -> opened by Project Manager; file ->
opened in the editor; folder -> simply opened in the OS file manager.
Opens and "Reveal in file manager" do NOT run inside the dialog
callback but from a short timer afterwards: the shell call switches
the focus away, CudaText deactivates, and the nested event processing
crashed with "Access violation" in on_app_deactivate - the same
workaround the cuda_favorites plugin uses.
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

#: sort modes: 'star', 'name', 'path', 'state', 'manual'
SORT_COLS = ('star', 'name', 'path', 'state')
DEFAULT_SORT = 'star'

# listview column indexes
COL_STAR = 0
COL_NAME = 1
COL_PATH = 2
COL_STATE = 3

#: unscaled px widths of the star, name and state columns; the Path
#: column is stretched to the free width (and the user can drag all
#: dividers)
STAR_W = 48
NAME_W = 230
STATE_W = 56
PATH_MIN_W = 200

#: rough unscaled height of the listview column header - clicks with
#: y below it are item clicks, above it are header clicks (only
#: needed on Linux/Qt where a header click can also fire on_click;
#: on Windows the header is a child window and never fires it)
HEADER_H = 30

BTN_W = 126
BTN_X = 8
BTN_GAP = 6
GRP_GAP = 18

FONT_W = 720
FONT_H = 470
FORM_MIN_W = 560
FORM_MIN_H = 350

#: delay (ms) before a favorite is actually opened/revealed: the OS
#: shell call must not run inside the dialog callback (see the module
#: docstring)
OPEN_DELAY_MS = 250

#: how much the window width must change before the Path column is
#: re-stretched (the list is refilled then - doing it on every pixel
#: of an interactive resize would flicker)
STRETCH_STEP = 50


def _ui_scale():
    """Current UI scale factor (1.0 at 100% DPI). DLG_SCALE scales the
    control coordinates but not the column widths inside the listview
    'items' string, so those are scaled here."""
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
        elif sort_col == 'state':
            # alive (empty state) first, dead ('?') last; ties by name
            pairs.sort(key=lambda p: (p[1]['name'].lower(), p[1]['path'].lower()))
            pairs.sort(key=lambda p: not os.path.exists(p[1]['path']),
                       reverse=sort_desc)
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
_form = None       # singleton of the main window (hidden, not freed)

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

        self.tab = self._ui_get('tab', 0)
        self.sort_col = self._ui_get('sort_col', DEFAULT_SORT)
        self.sort_desc = bool(self._ui_get('sort_desc', False))
        self.filter = ''
        self.view = []          # [(data_index, item)] for the active tab
        self.rows = []          # what is shown: dicts with cached dead
        self.selected_data = -1 # data index to re-select after refill
        self._closing = False
        self._sb_w = 20         # scrollbar width, for the Path stretch
        self._path_w = 0        # current width of the Path column
        # column geometry (real values are computed in build(); these
        # are the DPI-unscaled fallbacks for the click hit-testing)
        self.star_w = STAR_W
        self.name_w = NAME_W
        self.state_w = STATE_W
        self._header_h = HEADER_H

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

        w, h = FONT_W, FONT_H

        # scaled column widths (DLG_SCALE scales control coordinates
        # but not the widths inside the listview 'items' string)
        scale = _ui_scale()
        self.star_w = int(STAR_W * scale)
        self.name_w = int(NAME_W * scale)
        self.state_w = int(STATE_W * scale)
        self._header_h = int(HEADER_H * scale)
        try:
            self._sb_w = int(app_proc(PROC_GET_GUI_HEIGHT, 'scrollbar') or 20) or 20
        except Exception:
            self._sb_w = 20

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

        # Close is anchored to the bottom of the window only (its top
        # anchor is detached, otherwise it would stretch vertically)
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
        # bottom anchor references it by name.
        # It is anchored to the BOTTOM ONLY ('a_t': None) - with the
        # default top anchor kept, the label stretched vertically and
        # ate half of the window when it was maximized/enlarged.
        self._ctl('label', 'lbl_status', {
            'x': rx, 'y': h - 8 - 22, 'w': w - rx - 8, 'h': 22,
            'a_t': None,          # detach top anchor (do not stretch!)
            'a_b': ('', ']'),
            'sp_b': 8,
            'a_r': ('', ']'),
            'sp_r': 8,
            'cap': '',
        })

        # the list: NATIVE listview - standard look, and the column
        # widths are resizable by dragging the header dividers.
        # 'items' format: the first line is the header (cells separated
        # by \r, each caption can end with '=W' or '=CW' for the width
        # / centered alignment), the other lines are the rows; lines
        # are separated by \t. The star column is the UTF-8 glyph
        # ☆/★ as text - image lists never rendered in the dialog
        # listview (see history), so no image files are used at all.
        # Events (engine-verified):
        # - on_click_header: click on a column header, data = column
        #   index -> sorting; the old 'on_click_col' prop name never
        #   existed in the API, that is why header sorting was dead
        # - on_click: any click, data = (x, y) in control coords ->
        #   a click on the star glyph cell toggles the star
        # - on_click_dbl: data = (x, y) too; a double-click on the
        #   star cell only toggles (already done by the 1st click),
        #   so it must not open the favorite
        self._ctl('listview', 'list', {
            'x': rx, 'y': 8 + EH + BTN_GAP + BH + BTN_GAP,
            'w': w - rx - 8,
            'h': h - (8 + EH + BTN_GAP + BH + BTN_GAP) - 8 - 22 - BTN_GAP,
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
            'on_menu': self.list_on_menu,
        })

        dlg_proc(self.h, DLG_SCALE)
        self.fill_list()

    #------------- showing / closing

    def show(self):
        if not self.h:
            self.build()
        dlg_proc(self.h, DLG_SHOW_NONMODAL)
        dlg_proc(self.h, DLG_TO_FRONT)
        # focus the filter: type-to-filter right away; Ctrl+1..9 etc work
        # from any control because the form has keypreview=True
        try:
            dlg_proc(self.h, DLG_CTL_FOCUS, name='f_filter')
        except Exception:
            pass

    def close(self, id_dlg=0, id_ctl=0, data=''):
        if not self.h or self._closing:
            return
        self._closing = True
        try:
            self.save_ui_state()
            dlg_proc(self.h, DLG_HIDE)
        finally:
            self._closing = False
        # NOTE: the form is NOT freed here - it is only hidden and
        # reused by the next show(). This is what keeps the window
        # size/position/column widths for the whole CudaText session
        # (they reset only when CudaText restarts), and it also removes
        # the "Access violation" crashes of dialog callbacks arriving
        # for a form that a timer was about to free.

    def save_ui_state(self):
        # NOTE: the window size is deliberately NOT saved to the json -
        # see the module docstring (a form recreated with the pixel size
        # of a maximized window ends up with its bottom off-screen).
        # The size lives on because the hidden form is simply reused.
        self.core.ui['tab'] = self.tab
        self.core.ui['sort_col'] = self.sort_col
        self.core.ui['sort_desc'] = self.sort_desc
        self.core.save()

    def form_on_close(self, id_dlg, id_ctl=0, data=''):
        # the X button of a non-modal CudaText dialog only hides the
        # form - keep it alive for the next show()
        self.close()

    def form_on_resize(self, id_dlg, id_ctl=0, data=''):
        """Keep the Path column eating the free width: the column widths
        live inside the listview 'items' string, so the list is refilled
        with a wider/narrower Path column when the window width changed
        by STRETCH_STEP or more (refilling on every pixel of an
        interactive resize would flicker)."""
        if self._closing or not self.h:
            return
        path_w = self._calc_path_w()
        if path_w and abs(path_w - self._path_w) >= STRETCH_STEP:
            self._path_w = path_w
            self.fill_list()

    def _calc_path_w(self):
        """Width of the Path column so that it fills the listview up to
        its right edge. Reads the LIVE width of the control ('w' is a
        live prop); returns 0 when it cannot be read."""
        try:
            props = dlg_proc(self.h, DLG_CTL_PROP_GET, name='list') or {}
            w = int(props.get('w', 0) or 0)
        except Exception:
            return 0
        if w <= 0:
            return 0
        return max(PATH_MIN_W, w - self.star_w - self.name_w - self.state_w
                   - self._sb_w - 4)

    def _live_col_widths(self):
        """Live widths of the 4 columns (the user can drag the header
        dividers, so the widths of the last 'items' string are only the
        initial ones). The 'columns' prop of the control reports:
        per column 'Caption\rWidth\r...', columns joined by TAB.
        Returns [star, name, path, state] or None."""
        try:
            props = dlg_proc(self.h, DLG_CTL_PROP_GET, name='list') or {}
            cols = str(props.get('columns', '') or '')
        except Exception:
            return None
        if not cols:
            return None
        res = []
        for col in cols.split('\t'):
            parts = col.split('\r')
            try:
                w = int(parts[1])
            except (IndexError, ValueError):
                return None
            if w < 0:
                return None
            res.append(w)
        return res if len(res) == 4 else None

    def _star_col_w(self):
        """Live width of the star column for the click hit-test (a drag
        of the divider must not break the star clicking)."""
        live = self._live_col_widths()
        return live[0] if live else self.star_w

    #------------- list filling

    def _sort_caption(self, mode, base):
        """Column caption with the sort-direction marker."""
        if self.sort_col == 'manual' or mode != self.sort_col:
            return base
        return base + (' ↓' if self.sort_desc else ' ↑')

    def fill_list(self, keep_selection=True):
        if not self.h:
            return
        cat = self.cat()
        self.view = self.core.view(cat, self.sort_col, self.sort_desc, self.filter)

        # what is shown: the dead state is computed ONCE here, not on
        # every repaint (os.path.exists on each paint would be slow)
        self.rows = [{
            'data_idx': data_idx,
            'path': it['path'],
            'name': it['name'],
            'star': bool(it['star']),
            'dead': not os.path.exists(it['path']),
        } for (data_idx, it) in self.view]

        if not self._path_w:
            self._path_w = self._calc_path_w() or PATH_MIN_W

        # the user-dragged column widths survive the refill (the Path
        # column is re-stretched to the free width below, as before);
        # on the first fill there are no columns yet and None comes
        # back, so the default widths are used
        live = self._live_col_widths()
        if live:
            self.star_w, self.name_w, _w_path, self.state_w = live

        # NOTE: the format (header first, cells by \r, lines by \t,
        # 'Caption=Width' / 'Caption=C Width' in the header) is the
        # documented listview format; it must not contain stray \r/\t
        # inside the cells
        header = '\r'.join([
            '★=C' + str(self.star_w),
            self._sort_caption('name', _('Name')) + '=' + str(self.name_w),
            self._sort_caption('path', _('Path')) + '=' + str(self._path_w),
            self._sort_caption('state', _('State')) + '=C' + str(self.state_w),
        ])
        lines = [header]
        for r in self.rows:
            lines.append('\r'.join([
                '★' if r['star'] else '☆',
                r['name'],
                r['path'],
                '?' if r['dead'] else '',
            ]))
        items = '\t'.join(lines)

        try:
            dlg_proc(self.h, DLG_CTL_PROP_SET, name='list', prop={'items': items})
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
                dlg_proc(self.h, DLG_CTL_PROP_SET, name='list', prop={'val': str(sel)})
            except Exception:
                pass

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
        elif self.sort_col == 'state':
            sort_info = _('sort: state, dead last') + (' ↓' if self.sort_desc else ' ↑')
        else:
            sort_info = _('sort: path') + (' ↓' if self.sort_desc else ' ↑')

        parts = []
        if self.filter:
            parts.append(_('{} of {} shown').format(shown, total))
        else:
            parts.append(_('{} item(s), {} starred').format(total, starred))
        parts.append(sort_info)
        parts.append(_('Ctrl+1..9 / Alt+1..9 open item #'))
        try:
            dlg_proc(self.h, DLG_CTL_PROP_SET, name='lbl_status', prop={
                'cap': '   ·   '.join(parts),
            })
        except Exception:
            pass

        # enable/disable the buttons which need a selected row
        has_sel = self.selected_row() is not None
        for name in ('btn_open', 'btn_up', 'btn_down', 'btn_del'):
            try:
                dlg_proc(self.h, DLG_CTL_PROP_SET, name=name, prop={'en': has_sel})
            except Exception:
                pass

    #------------- selection helpers

    def _sel_index(self):
        try:
            props = dlg_proc(self.h, DLG_CTL_PROP_GET, name='list') or {}
            val = props.get('val', '')
            return int(val) if val not in ('', None) else -1
        except Exception:
            return -1

    def selected_row(self):
        """The row dict of the selection, or None."""
        i = self._sel_index()
        if 0 <= i < len(self.rows):
            return self.rows[i]
        return None

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
        self.fill_list(keep_selection=False)

    def list_on_change(self, id_dlg, id_ctl, data='', info=''):
        """Selection in the list moved. The event data is not trusted
        (its shape differs between OSes/builds) - the value is re-read
        from the control."""
        row = self.selected_row()
        self.selected_data = row['data_idx'] if row else -1
        self.update_status()

    def _click_pos(self, data):
        """(x, y) of a click inside the list, from the event data (the
        engine sends the mouse position in the control coordinates);
        None when the event came without a position."""
        if not (isinstance(data, (tuple, list)) and len(data) == 2):
            return None
        try:
            return int(data[0]), int(data[1])
        except (TypeError, ValueError):
            return None

    def _click_on_star(self, data):
        """Is a click (x, y from on_click/on_click_dbl) on the star
        column of a ROW (below the header area)?"""
        pos = self._click_pos(data)
        if pos is None:
            return False
        x, y = pos
        return y > self._header_h and x < self._star_col_w()

    def list_on_click(self, id_dlg, id_ctl, data='', info=''):
        """Any single click inside the list: a click on the star glyph
        cell toggles the star of that row (as the old builds did - the
        on_click event reports the mouse position, so the star column
        is told apart from the rest). Clicks on the column header and
        on the other columns are ignored here."""
        if self._closing or not self.h:
            return
        if not self._click_on_star(data):
            return
        row = self.selected_row()   # the click has selected the row
        if row is None:
            return   # clicked the empty area below the rows
        data_idx = self.core.find(self.cat(), row['path'])
        if data_idx >= 0:
            self.core.toggle_star(self.cat(), data_idx)
            self.selected_data = data_idx
            self.fill_list()

    def list_on_dbl_click(self, id_dlg, id_ctl, data='', info=''):
        """Double-click opens the favorite - except on the star cell:
        there the 1st click already toggled the star, a double-click
        must not also open the item."""
        if self._click_on_star(data):
            return
        self.open_selected()

    def list_on_header_click(self, id_dlg, id_ctl, data='', info=''):
        """Click on a column header (the on_click_header event, data =
        column index): 1st click sorts ascending, 2nd descending, 3rd
        switches to manual (custom) order."""
        if isinstance(data, (tuple, list)) and data:
            data = data[0]
        try:
            col = int(data)
        except (TypeError, ValueError):
            return
        self.sort_by_column(col)

    def sort_by_column(self, col):
        mode = {COL_STAR: 'star', COL_NAME: 'name', COL_PATH: 'path',
                COL_STATE: 'state'}.get(col)
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

    def list_on_menu(self, id_dlg, id_ctl, data='', info=''):
        """Right-click context menu of the favorites list."""
        row = self.selected_row()
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

        add(_('Open') + ' (Enter)', 'open', row is not None)
        add(_('Reveal in file manager'), 'reveal', row is not None)
        add(_('Copy path'), 'copy', row is not None)
        menu_proc(menu, MENU_ADD, caption='-')
        add((_('Unstar') if (row and row['star']) else _('Star')) + ' ★', 'star',
            row is not None)
        add(_('Rename...') + ' (F2)', 'rename', row is not None)
        add(_('Delete') + ' (Del)', 'delete', row is not None)
        menu_proc(menu, MENU_ADD, caption='-')
        # sorting is also in the menu because column-header clicks are
        # not reported by all CudaText builds
        add(_('Sort: ★ first, then name'), 'sort_star')
        add(_('Sort: by name'), 'sort_name')
        add(_('Sort: by path'), 'sort_path')
        add(_('Sort: by state, dead last'), 'sort_state')
        add(_('Sort: manual order'), 'sort_manual')
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
                self._open_paths([self.rows[n]['path']])
            else:
                msg_status(_('Favorites: no item at #{}').format(n + 1))
            return False  # block key

        # plain keys
        if 'a' not in mods and 'c' not in mods and 's' not in mods and 'm' not in mods:
            if key == VK_ENTER:
                self.open_selected()
                return False
            if key == VK_DELETE:
                self.delete_selected()
                return False
            if key == VK_F2:
                self.rename_selected()
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

        # Alt+Up / Alt+Down: move the item
        if 'a' in mods and 'c' not in mods and 's' not in mods:
            if key == VK_UP:
                self.move_selected(-1)
                return False
            if key == VK_DOWN:
                self.move_selected(+1)
                return False

        return None

    def focus_again(self):
        """Give the keyboard focus back to the favorites window after
        an editor tab was opened."""
        if self.h:
            dlg_proc(self.h, DLG_FOCUS)

    #------------- actions (on the selected row)

    def _open_paths(self, paths):
        """Open favorites NOT inside the dialog callback: the OS shell
        call / the editor opening switches the focus away, CudaText
        deactivates, and the nested event processing crashed with
        "Access violation" (on_app_deactivate of several plugins). The
        timer lets the callback return first - the same trick the
        cuda_favorites plugin uses."""
        paths = [str(p) for p in paths if p]
        if not paths:
            return

        def do_open(tag=None):
            for p in paths:
                open_item(p)
            self.focus_again()

        timer_proc(TIMER_START_ONE, do_open, OPEN_DELAY_MS)

    def open_selected(self):
        row = self.selected_row()
        if row is None:
            msg_status(_('Favorites: nothing selected'))
            return
        self._open_paths([row['path']])

    def delete_selected(self):
        row = self.selected_row()
        if row is None:
            return
        cat = self.cat()
        data_idx = self.core.find(cat, row['path'])
        if data_idx < 0:
            return
        if msg_box(_('Favorites: delete "{}"?').format(row['name']),
                   MB_OKCANCEL + MB_ICONQUESTION) != ID_OK:
            return
        self.core.remove_at(cat, data_idx)
        msg_status(_('Favorites: removed'))
        self.selected_data = -1
        self.fill_list(keep_selection=False)

    def rename_selected(self):
        row = self.selected_row()
        if row is None:
            return
        new = dlg_input(_('New name of the favorite:'), row['name'])
        if new is None:
            return
        new = new.strip()
        if new and new != row['name']:
            data_idx = self.core.find(self.cat(), row['path'])
            if data_idx >= 0:
                self.core.rename(self.cat(), data_idx, new)
        self.fill_list()

    def move_selected(self, delta):
        row = self.selected_row()
        if row is None:
            return
        if self.sort_col != 'manual':
            self.sort_col = 'manual'
            msg_status(_('Favorites: manual order - Move up/down reorders the list'))
        cat = self.cat()
        data_idx = self.core.find(cat, row['path'])
        if data_idx < 0:
            return
        new = self.core.move(cat, data_idx, delta)
        if new == data_idx:
            msg_status(_('Favorites: already at the top') if delta < 0
                       else _('Favorites: already at the bottom'))
        self.selected_data = new
        self.fill_list()

    def star_selected(self):
        row = self.selected_row()
        if row is None:
            return
        data_idx = self.core.find(self.cat(), row['path'])
        if data_idx >= 0:
            self.core.toggle_star(self.cat(), data_idx)
            self.fill_list()

    def copy_path(self):
        row = self.selected_row()
        if row is None:
            return
        app_proc(PROC_SET_CLIP, row['path'])
        msg_status(_('Favorites: copied path'))

    def reveal_selected(self):
        row = self.selected_row()
        if row is None:
            return
        path = row['path']

        def do_reveal(tag=None):
            reveal_in_file_manager(path)

        # deferred, like opening (see _open_paths)
        timer_proc(TIMER_START_ONE, do_reveal, OPEN_DELAY_MS)

    def btn_open_click(self, id_dlg, id_ctl, data='', info=''):
        self.open_selected()

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
        self.move_selected(-1)

    def btn_down_click(self, id_dlg, id_ctl, data='', info=''):
        self.move_selected(+1)

    def btn_del_click(self, id_dlg, id_ctl, data='', info=''):
        self.delete_selected()

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
        if action == 'sort_manual':
            self.sort_col = 'manual'
            self.fill_list()
            return
        if action in ('sort_star', 'sort_name', 'sort_path', 'sort_state'):
            self.sort_by_column({'sort_star': COL_STAR,
                                 'sort_name': COL_NAME,
                                 'sort_path': COL_PATH,
                                 'sort_state': COL_STATE}[action])
            return
        if action == 'open':
            self.open_selected()
        elif action == 'reveal':
            self.reveal_selected()
        elif action == 'copy':
            self.copy_path()
        elif action == 'star':
            self.star_selected()
        elif action == 'rename':
            self.rename_selected()
        elif action == 'delete':
            self.delete_selected()

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
            rows.append('\r'.join([
                '★' if it['star'] else '☆',   # UTF-8 font icons, no images
                it['name'],
                CAT_TITLES[cat],
                it['path'],
                '?' if not os.path.exists(it['path']) else '',
            ]))

        items = '\t'.join(
            ['\r'.join([
                '★=C48',
                _('Name') + '=210',
                _('Type') + '=70',
                _('Path') + '=230',
                _('State') + '=C56',
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
    cuda_favorites.json runs on every open (the external file may have
    changed while the window was closed)."""
    global _form
    refresh_core()
    if _form is None or not _form.h:
        _form = FavoritesForm(get_core())
    else:
        # the core data was reloaded - refresh the list of the reused
        # (hidden) form before showing it
        _form.fill_list()
    _form.show()


def find_favorite():
    """Show the 'Find favorite' quick picker (it is a favorites window
    too, so it refreshes the imported data the same way)."""
    refresh_core()
    core = get_core()
    if not any(core.data[cat] for cat in CATS):
        msg_status(_('Favorites: the list is empty'))
        return
    finder = FavoritesFinder(core)
    finder.show()


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
    if _form is not None and _form.h:
        _form.context_action(action)
