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
- listview with columns: star, Name, Path; sortable per column,
  default sort is "starred first, then by name"; a 3rd click on a
  column header switches to manual (custom) order
- star column: the UTF-8 font icons ☆ (unstarred) / ★ (starred) -
  no image files; click the star cell to toggle the "favorite of
  favorites" star
- filter field at the top filters by name and path; Esc clears it
- vertical button panel on the left: Open / Add... / Add opened...,
  Move up / Move down, Delete; Close at the bottom
- while the window is focused: Ctrl+1..9 / Alt+1..9 open the item at
  that index (index in the currently visible, filtered list), Enter
  opens the selection, Del deletes it, F2 renames it,
  Alt+Up/Down move it, Esc clears the filter (or closes the window)

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
#: the same data in early versions, under the misspelled name; read
#: once for migration, removed after the renamed file is saved
LEGACY_FAVORITES_FILENAME = 'cuda_project_man_favorits.json'
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

# listview column indexes
COL_STAR = 0
COL_NAME = 1
COL_PATH = 2

COL_STAR_W = 48
COL_NAME_W = 230

BTN_W = 126
BTN_X = 8
BTN_GAP = 6
GRP_GAP = 18

FONT_W = 720
FONT_H = 470
FORM_MIN_W = 560
FORM_MIN_H = 350

MOD_COL = {
    COL_STAR: 'star',
    COL_NAME: 'name',
    COL_PATH: 'path',
}


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
        self.path_legacy = os.path.join(app_path(APP_DIR_SETTINGS), LEGACY_FAVORITES_FILENAME)
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
        """Read our file (migrating once from the old misspelled file
        name), then import new entries from cuda_favorites.json
        (one-way: imported here, never exported back). Returns the
        number of imported entries."""
        own = _read_json(self.path_own)
        if own is None and os.path.exists(self.path_legacy):
            # one-time migration from the old misspelled file name
            own = _read_json(self.path_legacy)
        self.data = {cat: [] for cat in CATS}
        if isinstance(own, dict):
            for cat in CATS:
                val = own.get(cat)
                if isinstance(val, list):
                    self.data[cat] = val
        self.ui = own.get('_ui') if isinstance(own, dict) and isinstance(own.get('_ui'), dict) else {}
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
        if not _write_json(self.path_own, out):
            return
        # one-time cleanup after the migration to the corrected file
        # name: drop the old misspelled file once the new one is safely
        # written
        try:
            if os.path.exists(self.path_legacy):
                os.remove(self.path_legacy)
        except OSError:
            pass

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
        self.selected_data = -1 # data index to re-select after refill
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

        w = int(self._ui_get('w', FONT_W))
        h = int(self._ui_get('h', FONT_H))
        if w < FORM_MIN_W:
            w = FONT_W
        if h < FORM_MIN_H:
            h = FONT_H

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

        # the status label must be created BEFORE the listview: the
        # listview's bottom anchor references it by name
        self._ctl('label', 'lbl_status', {
            'x': rx, 'y': h - 8 - 22, 'w': w - rx - 8, 'h': 22,
            'a_b': ('', ']'),
            'sp_b': 8,
            'a_r': ('', ']'),
            'sp_r': 8,
            'cap': '',
        })

        self._ctl('listview', 'list', {
            'x': rx, 'y': 8 + EH + BTN_GAP + BH + BTN_GAP,
            'w': w - rx - 8, 'h': h - (8 + EH + BTN_GAP + BH + BTN_GAP) - 8 - 22 - BTN_GAP,
            'tab_order': 2,
            'ex0': True,          # grid lines
            'a_t': ('tabs', ']'),
            'sp_t': BTN_GAP,
            'a_b': ('lbl_status', '['),
            'sp_b': BTN_GAP,
            'a_r': ('', ']'),
            'sp_r': 8,
            'on_click': self.list_on_click,
            'on_click_dbl': self.list_on_dbl_click,
            'on_click_header': self.list_on_header_click,
            'on_select': self.list_on_select,
            'on_menu': self.list_on_menu,
        })

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
        try:
            props = dlg_proc(self.h, DLG_PROP_GET) or {}
            self.core.ui['w'] = props.get('w', FONT_W)
            self.core.ui['h'] = props.get('h', FONT_H)
        except Exception:
            pass
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
        self.stretch_path_column()

    #------------- list filling

    def visible_width(self):
        try:
            props = dlg_proc(self.h, DLG_CTL_PROP_GET, name='list') or {}
            return int(props.get('w', 0))
        except Exception:
            return 0

    def stretch_path_column(self):
        """Make the Path column eat the free width of the listview."""
        if not self.h:
            return
        w = self.visible_width()
        if w <= 0:
            return
        try:
            scroll_w = int(app_proc(PROC_GET_GUI_HEIGHT, 'scrollbar') or 16)
        except Exception:
            scroll_w = 16
        path_w = max(80, w - COL_STAR_W - COL_NAME_W - int(scroll_w) - 4)
        try:
            dlg_proc(self.h, DLG_CTL_PROP_SET, name='list', prop={
                'columns': '\t'.join([
                    self.header_caption(COL_STAR) + '\r' + str(COL_STAR_W) + '\r0\r0\rC\r0\r1',
                    self.header_caption(COL_NAME) + '\r' + str(COL_NAME_W) + '\r40\r0\rL\r0\r1',
                    self.header_caption(COL_PATH) + '\r' + str(path_w) + '\r60\r0\rL\r0\r1',
                ]),
            })
        except Exception:
            pass

    def header_caption(self, col):
        """Column caption with the sort-direction marker."""
        base = ['★', _('Name'), _('Path')][col]
        mode = MOD_COL.get(col)
        if self.sort_col == 'manual' or mode != self.sort_col:
            return base
        return base + (' ↓' if self.sort_desc else ' ↑')

    def fill_list(self, keep_selection=True):
        if not self.h:
            return
        cat = self.cat()
        self.view = self.core.view(cat, self.sort_col, self.sort_desc, self.filter)

        rows = []
        for _data_idx, it in self.view:
            name = it['name']
            if not os.path.exists(it['path']):
                name = '? ' + name
            rows.append('\r'.join([
                '★' if it['star'] else '☆',   # UTF-8 font icons, no images
                name,
                it['path'],
            ]))

        items = '\t'.join(
            ['\r'.join([
                self.header_caption(COL_STAR) + '=C' + str(COL_STAR_W),
                self.header_caption(COL_NAME) + '=' + str(COL_NAME_W),
                self.header_caption(COL_PATH) + '=300',
            ])] + rows)

        dlg_proc(self.h, DLG_CTL_PROP_SET, name='list', prop={'items': items})

        # restore selection
        sel_view = -1
        if keep_selection:
            for i, (data_idx, _it) in enumerate(self.view):
                if data_idx == self.selected_data:
                    sel_view = i
                    break
        if sel_view < 0:
            self.selected_data = self.view[0][0] if self.view else -1
            sel_view = 0 if self.view else -1
        if sel_view >= 0:
            dlg_proc(self.h, DLG_CTL_PROP_SET, name='list', prop={'val': str(sel_view)})

        self.stretch_path_column()
        self.update_status()

    def update_status(self):
        if not self.h:
            return
        cat = self.cat()
        total, starred = self.core.counts(cat)
        shown = len(self.view)
        if self.sort_col == 'manual':
            sort_info = _('sort: manual order')
        elif self.sort_col == 'star':
            sort_info = _('sort: ★, then name')
        else:
            sort_info = _('sort: ') + self.header_caption(COL_NAME if self.sort_col == 'name' else COL_PATH).strip()

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

        # enable/disable the buttons which need a selection
        has_sel = self.selected_data >= 0
        for name in ('btn_open', 'btn_up', 'btn_down', 'btn_del'):
            try:
                dlg_proc(self.h, DLG_CTL_PROP_SET, name=name, prop={'en': has_sel})
            except Exception:
                pass

    #------------- selection / view helpers

    def selected_view_index(self):
        try:
            props = dlg_proc(self.h, DLG_CTL_PROP_GET, name='list') or {}
            val = props.get('val', '')
            return int(val) if val not in ('', None) else -1
        except Exception:
            return -1

    def selected_pair(self):
        """(data_index, item) of the selection, or None."""
        i = self.selected_view_index()
        if 0 <= i < len(self.view):
            return self.view[i]
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

    def list_on_select(self, id_dlg, id_ctl, data='', info=''):
        # data = (item_index, selected); only react to the selected row
        try:
            idx, sel = data[0], data[1]
        except Exception:
            return
        if not sel:
            return
        if 0 <= idx < len(self.view):
            self.selected_data = self.view[idx][0]
        self.update_status()

    def list_on_header_click(self, id_dlg, id_ctl, data='', info=''):
        """Click on a column header: 1st click sorts ascending, 2nd
        descending, 3rd switches to manual (custom) order."""
        try:
            col = int(data)
        except Exception:
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
        """Single click: the star (first) column toggles the star flag.
        Header clicks go to on_click_header, they don't get here."""
        if not isinstance(data, (tuple, list)) or len(data) != 2:
            return
        x = data[0]
        try:
            x = int(x)
        except Exception:
            return
        if x >= COL_STAR_W or not self.view:
            return
        pair = self.selected_pair()
        if pair is None:
            return
        data_idx, it = pair
        self.core.toggle_star(self.cat(), data_idx)
        self.fill_list()

    def list_on_dbl_click(self, id_dlg, id_ctl, data='', info=''):
        pair = self.selected_pair()
        if pair is None:
            return
        self.open_selected()

    def list_on_menu(self, id_dlg, id_ctl, data='', info=''):
        """Right-click context menu of the favorites list."""
        pair = self.selected_pair()
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

        add(_('Open') + ' (Enter)', 'open', pair is not None)
        add(_('Reveal in file manager'), 'reveal', pair is not None)
        add(_('Copy path'), 'copy', pair is not None)
        menu_proc(menu, MENU_ADD, caption='-')
        add((_('Unstar') + ' ★') if (pair and pair[1]['star']) else (_('Star') + ' ★'),
            'star', pair is not None)
        add(_('Rename...') + ' (F2)', 'rename', pair is not None)
        add(_('Delete') + ' (Del)', 'delete', pair is not None)
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
            if 0 <= n < len(self.view):
                data_idx, it = self.view[n]
                open_item(it['path'])
                self.focus_again()
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

        # Alt+Up / Alt+Down: move the selected item
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

    #------------- actions

    def open_selected(self):
        pair = self.selected_pair()
        if pair is None:
            msg_status(_('Favorites: nothing selected'))
            return
        _data_idx, it = pair
        if open_item(it['path']):
            self.focus_again()

    def delete_selected(self):
        pair = self.selected_pair()
        if pair is None:
            return
        data_idx, it = pair
        if self.core.remove_at(self.cat(), data_idx):
            msg_status(_('Favorites: removed ') + it['name'])
            self.selected_data = -1
            self.fill_list(keep_selection=False)

    def rename_selected(self):
        pair = self.selected_pair()
        if pair is None:
            return
        data_idx, it = pair
        new = dlg_input(_('New name of the favorite:'), it['name'])
        if new and new.strip():
            self.core.rename(self.cat(), data_idx, new.strip())
            self.fill_list()

    def move_selected(self, delta):
        pair = self.selected_pair()
        if pair is None:
            return
        data_idx, _it = pair
        if self.sort_col != 'manual':
            self.sort_col = 'manual'
            msg_status(_('Favorites: manual order - Move up/down reorders the list'))
        new_idx = self.core.move(self.cat(), data_idx, delta)
        if new_idx == data_idx:
            msg_status(_('Favorites: already at the top') if delta < 0
                       else _('Favorites: already at the bottom'))
        self.selected_data = new_idx
        self.fill_list()

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
        added, err = self.core.add(cat, fn)
        if added:
            msg_status(_('Favorites: added ') + fn)
            self.tab = CATS.index(cat)
            try:
                dlg_proc(self.h, DLG_CTL_PROP_SET, name='tabs', prop={'val': str(self.tab)})
            except Exception:
                pass
            self.selected_data = self.core.find(cat, fn)
            self.fill_list()
        elif err:
            msg_status(err)

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

        pair = self.selected_pair()
        if pair is None:
            return
        data_idx, it = pair
        if action == 'open':
            if open_item(it['path']):
                self.focus_again()
        elif action == 'reveal':
            reveal_in_file_manager(it['path'])
        elif action == 'copy':
            app_proc(PROC_SET_CLIP, it['path'])
            msg_status(_('Favorites: copied ') + it['path'])
        elif action == 'star':
            self.core.toggle_star(self.cat(), data_idx)
            self.fill_list()
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
