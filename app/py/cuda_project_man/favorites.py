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
- layout: the MAIN column on the left (filter, tab strip, list, status
  line) and the vertical BUTTON column on the RIGHT edge (Open /
  Add... / Add opened..., Checked..., Move up / Move down, Delete;
  Close in the bottom-right corner) - the buttons move with the
  window's right edge, the main column ends exactly where the button
  column begins
- "tabs": three flat themed buttons (Projects / Files / Folders) drawn
  as a TAB STRIP: equal FIXED widths (TAB_W - the tabs hug their
  captions like the tabs of a browser or VS Code, they do NOT stretch
  over the whole list width), joined side by side directly above the
  list; every tab carries a bottom bar in the small strip that joins
  the buttons and the list FLUSH (no gap): a bold GREEN bar under the
  ACTIVE tab, a bar in the tab's own button color under the inactive
  ones (the bar is a colored panel - a label would never paint its
  color, LCL labels are transparent); the bar's top TAB_OVERLAP px
  reach UP over the tab button's own bottom border to COVER it (the
  themed flat button always draws a 1px border on all four edges and
  the API cannot turn it off - without the overlap a dark line showed
  between the tab face and its bar); hovering an INACTIVE tab
  highlights its bar together with its face (on_mouse_enter/exit
  recolor the bar to the same theme 'ButtonBgOver' the hovered face
  uses - the tab and its bar read as one hovered element; the active
  tab's green bar never changes); the OS-drawn tab control could
  not be colored by the theme, so it was replaced
- the list is the "listbox_ex" control - the flat listbox of CudaText
  dialogs, painted by the UI theme like the built-in Preferences
  dialog: its column header, items, selection and scrollbar all follow
  the theme, live, also when the theme is switched while CudaText runs;
  the list is ANCHORED to the form, so it grows/shrinks with the window
- columns: check / star / Name / Path / State, separated by vertical
  lines (painted with the rows). Every row is painted by the plugin
  (on_draw_item owner-drawing) from the theme colors: checkbox, star
  glyph, texts, column separators
- checkbox column: click it to check rows, then run one command on ALL
  checked favorites at once - open, star/unstar, copy paths, rename,
  move up/down (as a block), delete - from the "Checked" button or
  from the right-click menu; click the "All" header cell to
  check/uncheck everything; Space checks/unchecks the selected row
- star column (★/☆): click the star cell of a row to toggle it (a
  double-click on the star cell only toggles, it does not open)
- sorting: click a column header (star / Name / Path / State): 1st
  click ascending, 2nd descending, 3rd switches to manual (custom)
  order; the right-click menu has the same Sort commands as a fallback;
  the direction is shown by an arrow in the header caption
- column widths are RESIZABLE: drag the vertical divider between two
  header captions (the mouse cursor changes to the split shape over a
  divider); the Path column is auto-stretched - it always fills the
  free width, also when the window is resized
- filter field at the top filters by name and path; Esc clears it
- while the window is focused: Ctrl+1..9 / Alt+1..9 open the item at
  that index (index in the currently visible, filtered list), Enter
  opens the selection, Del deletes it, F2 renames it, Space checks it,
  Alt+Up/Down move it, Esc clears the filter (or closes the window)
- the window is only HIDDEN on close and reused on the next open, so
  its size and position are kept for the whole CudaText session; they
  reset only when CudaText restarts (the size is not saved to disk: a
  form recreated with the pixel size of a maximized window ends up
  taller than the work area)

THEME COLORS: the whole window follows the CudaText UI theme, like the
built-in Preferences dialog (app_proc(PROC_THEME_UI_DICT_GET)). The
flat button_ex buttons and the listbox_ex list (header, rows,
selection, scrollbar) are themed by the engine itself; the form, the
filter field and the status line are colored from the theme dict, and
the owner-drawn list rows are painted from it too - so the colors are
re-read on every open and re-applied live when the theme is switched
while CudaText runs.

"Add opened..." shows the same chooser as "Add...": pick the opened
project, the current editor file, or the folder of the current file.

Opening a favorite: project -> opened by Project Manager; file ->
opened in the editor; folder -> simply opened in the OS file manager.
Opens and "Reveal in file manager" do NOT run inside the dialog
callback but from a short timer afterwards: the shell call switches
the focus away, CudaText deactivates, and the nested event processing
crashed with "Access violation" in on_app_deactivate - the same
workaround the cuda_favorites plugin uses. When SEVERAL favorites are
opened at once (Open all checked), each one gets its own timer tick,
so the app can process the events between the opens.

DPI NOTE: app_proc(PROC_GET_GUI_HEIGHT) returns the DPI-SCALED height
of a control; the layout here is computed in UNSCALED units and
DLG_SCALE scales it afterwards - so the returned heights are divided
by the UI scale factor first (a double-scaled button height used to
blow up the button column at 125%/150% DPI and make buttons disappear).
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

#: sort modes: 'star', 'name', 'path', 'state', 'manual'
SORT_COLS = ('star', 'name', 'path', 'state')
DEFAULT_SORT = 'star'

# listbox_ex column indexes
COL_CHECK = 0
COL_STAR = 1
COL_NAME = 2
COL_PATH = 3
COL_STATE = 4
NCOLS = 5

#: column separator inside listbox_ex item/header strings: a char that
#: never appears in a name or path
LIST_SEP = chr(1)

#: unscaled px widths of the check, star, name and state columns; the
#: Path column is AUTO-STRETCHED by the listbox itself (column size 0
#: = "share the free width" in listbox_ex) - it always fills the list
#: up to its right edge, also when the window is resized
CHECK_W = 38
STAR_W = 46
NAME_W = 210
STATE_W = 54
PATH_MIN_W = 160
#: unscaled listbox row height (the header row has the same height)
ITEM_H = 30
#: unscaled size of the checkbox square
CHECK_SZ = 15
#: half-width (px) of the header-divider drag zone
RESIZE_GRIP = 6
#: min widths (px, unscaled) of the columns while dragging
COL_MIN_W = (24, 26, 60, 100, 34)
#: a drag must move this many px before it counts as a resize (below
#: that, releasing over the header is a normal header click = sorting)
DRAG_THRESHOLD = 4

#: LCL mouse-cursor id of the horizontal-split (left-right arrows)
#: shape: shown over the draggable header dividers and while dragging
#: them (crHSplit in the LCL; crDefault = 0)
CURSOR_HSPLIT = -14

#: height (unscaled px) of the VISIBLE bottom bar of every tab: a BOLD
#: GREEN bar under the ACTIVE tab, a bar in the tab's own button color
#: under the inactive ones (browser/VS Code tab style; all three tabs
#: have the same size, the bar is their only difference). The bar is a
#: 'panel' control (a plain TPanel paints its Color; the first version
#: used a 'label', but an LCL label is TRANSPARENT - it never paints
#: its Color, so the green was never visible). The bar lives in the
#: TAB_ULINE_H strip between the tab buttons and the list, flush with
#: both - the list is created AFTER the panels and later-created
#: windowed siblings paint OVER earlier ones, so the panels must never
#: overlap the listbox (they don't: the list is top-anchored exactly
#: to the strip's bottom)
TAB_ULINE_H = 4
#: how far (unscaled px) the bar panels reach UP INTO their tab
#: button: the themed flat button (TATButton) ALWAYS paints its own
#: 1px border on all four edges - the dialog API has no prop to turn
#: that border off - and that bottom border row sat between the tab
#: face and its bar as a visible dark line ("a border seen between it
#: and the tab"). The bar panels are created AFTER the tab buttons and
#: later-created windowed siblings paint OVER earlier ones, so raising
#: the bar TAB_OVERLAP px over the button's bottom edge COVERS the
#: button's bottom border (and any 1px DLG_SCALE rounding gap): the
#: bar meets the tab face flush, like a browser tab. 3 px also covers
#: the wider hover/pressed borders (up to PressedBorderWidth=3) that
#: the button paints around its face while the mouse is on it.
TAB_OVERLAP = 3
#: the green of the active tab's underline (a BGR int): a saturated
#: green that reads well on both light and dark themes
TAB_GREEN = 0x37A800
#: unscaled FIXED width of every tab button (browser/VS Code tabs
#: hug their caption, they do NOT stretch over the whole list width -
#: full-width tabs looked bloated); when the window is narrowed below
#: the space needed by three fixed tabs, they shrink to equal thirds
#: (see _layout_tabs), never overflowing into the button column
TAB_W = 120
#: gap between the tab buttons (unscaled px): tight, so the three
#: buttons read as one tab strip, not as separate buttons
TAB_GAP = 2
#: extra vertical padding (unscaled px) added to the raw GUI button
#: height of the buttons/tabs: the raw height hugs the caption text
#: too tightly (the text and the button border almost touched)
BTN_PAD = 8

BTN_W = 150
#: margin (unscaled px) between the window edge and the button column
#: / the main column (filter, tabs, list, status line)
BTN_X = 8
BTN_GAP = 6
GRP_GAP = 18

FONT_W = 720
FONT_H = 500
FORM_MIN_W = 600
#: tall enough for the padded button column (7 buttons + 2 group gaps)
#: plus the Close button at the bottom - at the old 350 the last
#: button could reach into Close when the OS button height is big
FORM_MIN_H = 410

#: delay (ms) before a favorite is actually opened/revealed: the OS
#: shell call must not run inside the dialog callback (see the module
#: docstring); 400 ms like the cuda_favorites plugin - a shorter delay
#: still caught the app mid-deactivation sometimes
OPEN_DELAY_MS = 400

def _unscale_px(v, scale, default):
    """Unscaled layout value of a DPI-scaled pixel size.
    app_proc(PROC_GET_GUI_HEIGHT) returns the already-scaled height of
    a real control; the layout of these dialogs is computed in
    unscaled units and scaled once by DLG_SCALE at the end - so the
    value must be divided by the scale factor here (without this, the
    button/edit heights were scaled TWICE and the button column blew
    up / overflowed at 125%/150% DPI)."""
    try:
        v = int(v)
    except (TypeError, ValueError):
        return default
    if v <= 0:
        return default
    return max(1, int(round(v / scale))) if scale > 1.001 else v


def _ui_scale():
    """Current UI scale factor (1.0 at 100% DPI). DLG_SCALE scales the
    control coordinates but not the column widths / row height inside
    listbox_ex, so those are scaled here."""
    try:
        val = app_proc(PROC_CONFIG_SCALE_GET, '')
        return max(1.0, int(val[0]) / 100.0)
    except Exception:
        return 1.0


def get_theme_ui_colors():
    """Colors of the current CudaText UI theme, for coloring the
    plugin's dialogs the way the built-in Preferences dialog does
    (app_proc(PROC_THEME_UI_DICT_GET)). Returns a dict of int colors
    (or None for the keys a theme may not define - the caller skips
    those). Used for the form, the filter field and the status label;
    the listbox_ex list and the button_ex buttons are themed by the
    engine itself."""
    try:
        d = app_proc(PROC_THEME_UI_DICT_GET, '')
    except Exception:
        d = None

    def col(key):
        try:
            v = d.get(key)
            if isinstance(v, dict) and v.get('color') is not None:
                return int(v['color'])
        except Exception:
            pass
        return None

    return {
        # window background: the tab-bar color (what cuda_prefs uses)
        'form_bg': col('TabBg'),
        # text input: the editor colors
        'edit_bg': col('EdTextBg'),
        'edit_font': col('EdTextFont'),
        # plain labels on the form background
        'label_font': col('EdTextFont'),
    }


def _theme_dict():
    """Raw UI-theme dict (or None)."""
    try:
        return app_proc(PROC_THEME_UI_DICT_GET, '')
    except Exception:
        return None


def _theme_color(d, key, fallback):
    """One int color from a theme dict (TColor/BGR int), with a
    fallback for themes that do not define the key."""
    try:
        v = d.get(key)
        if isinstance(v, dict) and v.get('color') is not None:
            return int(v['color'])
    except Exception:
        pass
    return fallback


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

    def remove_many(self, cat, indexes):
        """Delete several entries at once (one save). Returns the number
        of removed entries."""
        idxset = {i for i in indexes if 0 <= i < len(self.data[cat])}
        if not idxset:
            return 0
        removed = [self.data[cat][i]['path'] for i in sorted(idxset)]
        self.data[cat] = [it for i, it in enumerate(self.data[cat])
                          if i not in idxset]
        for p in removed:
            if not self._is_deleted(p):
                self.deleted.append(p)
        self.save()
        return len(removed)

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
    """The main Favorites window. The list is a "listbox_ex" (the flat
    themed listbox of CudaText dialogs) in owner-drawn mode: the rows
    are painted here from the theme colors, so the checkbox column, the
    star column and the dead-path state are all theme-aware. The themed
    header is painted by the engine itself (LISTBOX_SET_HEADER +
    LISTBOX_SET_COLUMNS) and its dividers can be dragged to resize the
    columns."""

    def __init__(self, core):
        self.core = core
        self.h = 0
        self.h_list = 0          # handle of the listbox_ex control

        self.tab = self._ui_get('tab', 0)
        self.sort_col = self._ui_get('sort_col', DEFAULT_SORT)
        self.sort_desc = bool(self._ui_get('sort_desc', False))
        self.filter = ''
        self.view = []          # [(data_index, item)] for the active tab
        self.rows = []          # what is shown: dicts with cached dead
        self.selected_data = -1 # data index to re-select after refill
        self._closing = False
        #: hover state of the tab strip, as TWO order-independent flags
        #: (Windows can deliver a control's mouse-LEAVE after the next
        #: control's mouse-ENTER, so a single 'hovered tab' index would
        #: flicker): the index of the tab whose BUTTON FACE is under the
        #: mouse, or -1
        self._hover_btn = -1
        #: the index of the tab whose bottom BAR panel is under the
        #: mouse (the bar reaches up into its tab button, so the mouse
        #: crosses button<->bar inside one tab), or -1; a tab counts as
        #: hovered when EITHER flag names it
        self._hover_bar = -1
        self._sb_w = 20         # scrollbar width, for the Path stretch
        self._scale = 1.0
        #: norm-keys of the checked favorites (checkbox column)
        self.checked = set()
        #: colors for the owner-drawn rows (filled by _load_draw_theme)
        self._theme = {}
        #: active column-resize drag: dict(col, x0, orig_w, sign, moved)
        self._col_drag = None
        #: the split cursor is currently shown over the list
        self._cursor_split = False
        #: a resize drag just ended: swallow the header-click (sort)
        #: that the release would fire
        self._suppress_header_click = False
        #: column widths [check, star, name, path, state], scaled px
        self.cols = [CHECK_W, STAR_W, NAME_W, 400, STATE_W]

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
        scale = _ui_scale()
        # the GUI heights come back DPI-SCALED; the layout is unscaled
        # and DLG_SCALE scales it once at the end (see _unscale_px)
        BH = _unscale_px(app_proc(PROC_GET_GUI_HEIGHT, 'button'), scale, 30)
        EH = _unscale_px(app_proc(PROC_GET_GUI_HEIGHT, 'edit'), scale, 28)
        # the buttons and tabs get extra vertical padding: the raw GUI
        # button height hugs the caption text too tightly (the text
        # almost touched the button's border)
        BTN_H = BH + BTN_PAD
        # the tab strip is BTN_H tall PLUS the TAB_ULINE_H strip that
        # carries every tab's bottom bar (green = active, button color
        # = inactive; a colored panel - see the TAB_ULINE_H note), so
        # the tab buttons keep the FULL padding of the other buttons
        TABS_H = BTN_H + TAB_ULINE_H

        # min window height: the right button column (4 buttons, group
        # gap, 2 buttons, group gap, Delete) + Close + the status line
        # + some list - computed from the REAL button height, so a
        # system with tall buttons can never make Delete reach into
        # Close at the minimum size
        col_bottom = (8 + 4 * (BTN_H + BTN_GAP) + GRP_GAP
                      + 2 * (BTN_H + BTN_GAP) + GRP_GAP + BTN_H)
        min_h = col_bottom + BTN_GAP + BTN_H + 8 + 22 + 80

        w, h = FONT_W, max(FONT_H, min_h)

        # scaled column widths / row height (DLG_SCALE scales control
        # coordinates but not the listbox_ex internals)
        self._scale = scale
        self.cols = [
            int(CHECK_W * scale),
            int(STAR_W * scale),
            int(NAME_W * scale),
            0,   # Path: auto-stretched by the listbox (column size 0)
            int(STATE_W * scale),
        ]
        try:
            self._sb_w = int(app_proc(PROC_GET_GUI_HEIGHT, 'scrollbar') or 20) or 20
        except Exception:
            self._sb_w = 20

        # tab strip geometry (unscaled): the strip top ty; the strip is
        # TABS_H tall (BTN_H of button + TAB_ULINE_H of underline zone),
        # the list starts right below it at ty + TABS_H
        ty = 8 + EH + BTN_GAP          # top of the strip

        self.h = dlg_proc(0, DLG_CREATE)
        dlg_proc(self.h, DLG_PROP_SET, prop={
            'cap': _('Favorites'),
            'w': w,
            'h': h,
            'w_min': FORM_MIN_W,
            'h_min': max(FORM_MIN_H, min_h),
            'border': DBORDER_SIZE,     # resizable + minimize/maximize
            'keypreview': True,         # needed for on_key_down
            'on_key_down': self.form_on_key,
            'on_close': self.form_on_close,
            'on_resize': self.form_on_resize,
        })

        #---- RIGHT vertical button column ----
        # All buttons are 'button_ex' (the flat CudaText button): it is
        # painted with the global flat theme, so it follows the UI
        # theme automatically - the native 'button' control would stay
        # OS-light on a dark theme.
        # ANCHORS: every button carries 'a_l': None + 'a_r': ('', ']') -
        # the dialog engine maps a_l/a_r onto the LCL anchors and the
        # LCL default is akLeft=on, which would pin the left edge while
        # the right edge follows the window edge (= the button stretches
        # on every resize). With akLeft removed the buttons keep their
        # width and the whole column only MOVES with the window's right
        # edge. The main column (filter/tabs/list/status) anchors its
        # RIGHT side to btn_open's LEFT edge, so it always ends exactly
        # where the button column begins.
        bx = w - BTN_W - BTN_X
        RIGHT_ANCHORS = {
            'a_l': None,
            'a_r': ('', ']'),
            'sp_r': BTN_X,
        }
        y = 8
        self._ctl('button_ex', 'btn_open', {
            'cap': _('Open'),
            'x': bx, 'y': y, 'w': BTN_W, 'h': BTN_H,
            'tab_order': 3,
            'hint': _('Open the selected favorite\rEnter and double-click do the same.\rProjects open in Project Manager, files in the editor,\rfolders in the OS file manager'),
            'on_change': self.btn_open_click,
            **RIGHT_ANCHORS,
        })
        y += BTN_H + BTN_GAP
        self._ctl('button_ex', 'btn_add', {
            'cap': _('Add...'),
            'x': bx, 'y': y, 'w': BTN_W, 'h': BTN_H,
            'tab_order': 4,
            'hint': _('Add a new favorite: project, file or folder'),
            'on_change': self.btn_add_click,
            **RIGHT_ANCHORS,
        })
        y += BTN_H + BTN_GAP
        self._ctl('button_ex', 'btn_add_opened', {
            'cap': _('Add opened...'),
            'x': bx, 'y': y, 'w': BTN_W, 'h': BTN_H,
            'tab_order': 5,
            'hint': _('Add the opened project, the current editor file,\ror the folder of the current file - a menu lets you choose'),
            'on_change': self.btn_add_opened_click,
            **RIGHT_ANCHORS,
        })
        y += BTN_H + BTN_GAP
        self._ctl('button_ex', 'btn_checked', {
            'cap': _('Checked') + ' ▼',
            'x': bx, 'y': y, 'w': BTN_W, 'h': BTN_H,
            'tab_order': 6,
            'hint': _('Run one command on ALL checked favorites:\ropen, star/unstar, copy paths, rename,\rmove up/down as a block, delete.\rCheck rows with the checkbox column\r(click the "All" header cell to check all)'),
            'on_change': self.btn_checked_click,
            **RIGHT_ANCHORS,
        })

        y += BTN_H + GRP_GAP  # visible gap between the groups
        self._ctl('button_ex', 'btn_up', {
            'cap': _('Move up'),
            'x': bx, 'y': y, 'w': BTN_W, 'h': BTN_H,
            'tab_order': 7,
            'hint': _('Move the selected favorite up\r(switches the list to manual order)'),
            'on_change': self.btn_up_click,
            **RIGHT_ANCHORS,
        })
        y += BTN_H + BTN_GAP
        self._ctl('button_ex', 'btn_down', {
            'cap': _('Move down'),
            'x': bx, 'y': y, 'w': BTN_W, 'h': BTN_H,
            'tab_order': 8,
            'hint': _('Move the selected favorite down\r(switches the list to manual order)'),
            'on_change': self.btn_down_click,
            **RIGHT_ANCHORS,
        })

        y += BTN_H + GRP_GAP  # visible gap between the groups
        self._ctl('button_ex', 'btn_del', {
            'cap': _('Delete'),
            'x': bx, 'y': y, 'w': BTN_W, 'h': BTN_H,
            'tab_order': 9,
            'hint': _('Delete the selected favorite (Del key)'),
            'on_change': self.btn_del_click,
            **RIGHT_ANCHORS,
        })

        # Close sits in the window's BOTTOM-RIGHT corner (top anchor
        # detached, otherwise it would stretch vertically; left anchor
        # detached, otherwise it would stretch horizontally)
        self._ctl('button_ex', 'btn_close', {
            'cap': _('Close'),
            'x': bx, 'y': h - BTN_H - 8, 'w': BTN_W, 'h': BTN_H,
            'tab_order': 10,
            'a_l': None,          # detach left anchor (do not stretch)
            'a_r': ('', ']'),     # stick to the right of the form
            'sp_r': BTN_X,
            'a_t': None,          # detach top anchor (do not stretch)
            'a_b': ('', ']'),     # stick to the bottom of the form
            'sp_b': 8,
            'on_change': self.close,
        })

        #---- left/main column: filter, tabs, list, status ----
        # Everything here starts at the window's left edge (BTN_X) and
        # ends at the button column's left edge (anchored to
        # btn_open's '[' side with a BTN_X gap).
        MAIN_RIGHT = {
            'a_r': ('btn_open', '['),
            'sp_r': BTN_X,
        }

        self._ctl('edit', 'f_filter', {
            'x': BTN_X, 'y': 8, 'w': bx - 2 * BTN_X, 'h': EH,
            'tab_order': 0,
            'act': True,
            'texthint': _('Filter by name or path...'),
            'hint': _('Quickly filter the favorites list.\rEsc clears the filter'),
            'a_l': ('', '['),
            'sp_l': BTN_X,
            **MAIN_RIGHT,
            'on_change': self.on_filter_change,
        })

        # the status label must be created BEFORE the list: the list's
        # bottom anchor references it by name.
        # It is anchored to the BOTTOM ONLY ('a_t': None) - with the
        # default top anchor kept, the label stretched vertically and
        # ate half of the window when it was maximized/enlarged.
        self._ctl('label', 'lbl_status', {
            'x': BTN_X, 'y': h - 8 - 22, 'w': bx - 2 * BTN_X, 'h': 22,
            'a_l': ('', '['),
            'sp_l': BTN_X,
            'a_t': None,          # detach top anchor (do not stretch!)
            'a_b': ('', ']'),
            'sp_b': 8,
            **MAIN_RIGHT,
            'cap': '',
        })

        # the list: "listbox_ex" - the flat themed listbox of CudaText
        # dialogs (like the list in the built-in Preferences dialog).
        # The engine paints its themed column header (with vertical
        # lines between the captions) and its themed scrollbar; the rows
        # are painted by on_draw_item (owner-drawn mode,
        # LISTBOX_SET_DRAWN) so the checkbox column, the star glyphs,
        # the dead-path '?' and the vertical column lines are all drawn
        # from the theme colors. Column widths / the header captions
        # live in listbox_proc (LISTBOX_SET_COLUMNS / LISTBOX_SET_HEADER);
        # the header dividers are dragged to resize the columns
        # (list_on_mouse_down/move/up).
        # The list control itself is created BELOW the tab strip (its
        # top anchor references 'tab0').

        #---- the tab strip (three flat themed buttons) ----
        # The OS-drawn 'tabs' control could not be themed, so it is
        # button_ex buttons. The three buttons form a LEFT-ALIGNED strip
        # of EQUAL FIXED widths (TAB_W, browser/VS Code style - they hug
        # their captions and do NOT stretch over the whole list width),
        # joined with a small TAB_GAP, sitting directly above the list.
        # ALL THREE HAVE THE SAME HEIGHT - the active one differs ONLY
        # by its green bottom bar (uline0..2 below), not by its size.
        # Only the LEFT edges are anchored (chained to each other) -
        # anchoring a button's right edge to the next button's left
        # edge AND back would be a circular anchor pair, which the
        # engine cannot resolve (it used to break the strip after a
        # window resize: unequal widths, the first tab sliding over the
        # button column). _layout_tabs re-splits the widths on resizes
        # (fixed TAB_W, shrinking only in a too-narrow window).
        tab_caps = (_('Projects'), _('Files'), _('Folders'))
        tab_names = ('tab0', 'tab1', 'tab2')
        tab_w = TAB_W
        for i in range(3):
            prop = {
                'cap': tab_caps[i],
                'x': BTN_X + i * (tab_w + TAB_GAP),
                'y': ty,             # sane pre-anchor position; the top
                                     # anchor below places the strip
                'a_t': ('f_filter', ']'),   # BTN_GAP below the REAL filter
                'sp_t': BTN_GAP,            # bottom, at every scale/DPI
                'w': tab_w,
                'h': BTN_H,          # the FULLY padded height, like every
                                     # other button; the TAB_ULINE_H strip
                                     # below the buttons carries every
                                     # tab's bottom bar (green = active)
                'tab_order': 1,
                'act': True,
                'hint': _('Show {} favorites').format(tab_caps[i]),
                'on_change': (lambda id_dlg, id_ctl, data='', info='', _i=i:
                              self.on_tab_button(_i)),
                # hover of the whole tab = face + bar: the engine paints
                # the hovered face itself, these events recolor the bar
                # under it (inactive tabs only - see on_tab_hover)
                'on_mouse_enter': (lambda id_dlg, id_ctl, data='', info='',
                                   _i=i: self.on_tab_hover(_i, True, False)),
                'on_mouse_exit': (lambda id_dlg, id_ctl, data='', info='',
                                  _i=i: self.on_tab_hover(_i, False, False)),
            }
            if i == 0:
                # sp_l is REQUIRED: setting a_l re-glues the edge to the
                # form's left side + sp_l (sp_l defaults to 0)
                prop['a_l'] = ('', '[')
                prop['sp_l'] = BTN_X
            else:
                prop['a_l'] = (tab_names[i - 1], ']')
                prop['sp_l'] = TAB_GAP
            self._ctl('button_ex', tab_names[i], prop)

        #---- the BOTTOM BAR of every tab ----
        # A thin colored PANEL glued to the BOTTOM of its tab (left and
        # right edges anchored to that tab, so it follows the window
        # resizes and _layout_tabs' re-splits). The bar lives in the
        # TAB_ULINE_H strip BETWEEN the buttons and the list, FLUSH with
        # the list: the list below is top-anchored to tab0's bottom with
        # the spacing of exactly TAB_ULINE_H. A 'panel' (plain LCL
        # TPanel, bevels off) paints its Color - the first version used
        # a 'label', but LCL labels are TRANSPARENT and never paint
        # their Color, so the bar was never visible.
        # All three bars show a TAB_ULINE_H tall strip below the buttons:
        # GREEN for the active tab, the tab's own button color for the
        # inactive ones (set in _update_tabs) - so the strip reads as the
        # bottom edge of the tab row, not as empty form background.
        # The bar's TOP edge is TAB_OVERLAP px ABOVE its tab's bottom
        # edge (top-anchored to the tab's '[' side with the spacing of
        # BTN_H - TAB_OVERLAP, height TAB_ULINE_H + TAB_OVERLAP): the
        # panel is created AFTER the buttons, and later-created
        # windowed siblings paint OVER earlier ones, so those TAB_OVERLAP
        # px COVER the button's own bottom border - the themed flat
        # button always draws a 1px border on all four edges and that
        # line used to show between the tab face and its bar (the API
        # has no prop to disable the button border, so it is covered
        # instead). The visible bar below the button stays TAB_ULINE_H.
        for i in range(3):
            self._ctl('panel', 'uline{}'.format(i), {
                'x': BTN_X + i * (tab_w + TAB_GAP),
                'y': ty + BTN_H - TAB_OVERLAP,  # sane pre-anchor
                                             # position; the top anchor
                                             # places the strip
                'w': tab_w,
                'h': TAB_ULINE_H + TAB_OVERLAP,
                'a_l': (tab_names[i], '['),
                'sp_l': 0,
                'a_r': (tab_names[i], ']'),
                'sp_r': 0,
                'a_t': (tab_names[i], '['),
                'sp_t': BTN_H - TAB_OVERLAP,  # top edge INSIDE the tab,
                                             # TAB_OVERLAP over its border
                'color': TAB_GREEN,   # corrected per tab by _update_tabs
                # the bar is part of its tab for the hover too: moving the
                # mouse from the list up into the bar (or from the face
                # down into the bar) keeps the tab highlighted as one
                # element (the bar overlaps the button's bottom, so the
                # mouse crosses between the two controls inside one tab)
                'on_mouse_enter': (lambda id_dlg, id_ctl, data='', info='',
                                   _i=i: self.on_tab_hover(_i, True, True)),
                'on_mouse_exit': (lambda id_dlg, id_ctl, data='', info='',
                                  _i=i: self.on_tab_hover(_i, False, True)),
            })

        #---- the list ----
        # Created AFTER the tab strip: its top anchor references 'tab0',
        # and the dialog engine resolves a control name in an anchor only
        # if that control already exists. The top anchor chains the main
        # column FLUSH at every DPI / UI scale: filter -> (BTN_GAP) ->
        # tab buttons -> (TAB_ULINE_H of the bar strip) -> list. The old
        # layout placed the list at a fixed offset from the filter's
        # bottom - at a non-100% scale the scaled offsets and the
        # controls' real heights drifted apart and a wide empty gap
        # opened between the tabs and the list.
        self._ctl('listbox_ex', 'list', {
            'x': BTN_X, 'y': ty + TABS_H,   # sane pre-anchor position
            'w': bx - 2 * BTN_X,
            'h': h - (ty + TABS_H) - 8 - 22 - BTN_GAP,
            'tab_order': 2,
            'act': True,          # fire on_change when the selection moves
            'border': True,
            'a_l': ('', '['),
            'sp_l': BTN_X,
            **MAIN_RIGHT,
            'a_t': ('tab0', ']'),
            'sp_t': TAB_ULINE_H,   # exactly the bar strip: flush, no gap
            'a_b': ('lbl_status', '['),
            'sp_b': BTN_GAP,
            'on_change': self.list_on_change,
            'on_click': self.list_on_click,
            'on_click_dbl': self.list_on_dbl_click,
            'on_click_header': self.list_on_header_click,
            'on_menu': self.list_on_menu,
            'on_mouse_down': self.list_on_mouse_down,
            'on_mouse_move': self.list_on_mouse_move,
            'on_mouse_up': self.list_on_mouse_up,
            'on_mouse_exit': self.list_on_mouse_exit,
            'on_draw_item': self.list_on_draw_item,
        })
        self.h_list = dlg_proc(self.h, DLG_CTL_HANDLE, name='list') or 0
        if self.h_list:
            listbox_proc(self.h_list, LISTBOX_SET_COLUMN_SEP, text=LIST_SEP)
            listbox_proc(self.h_list, LISTBOX_SET_ITEM_H,
                         index=int(ITEM_H * scale))
            listbox_proc(self.h_list, LISTBOX_SET_DRAWN, index=1)

        dlg_proc(self.h, DLG_SCALE)
        self.apply_theme()   # also calls _update_tabs (tab bar colors)
        self._layout_tabs()
        self.fill_list()

    def apply_theme(self):
        """Re-color the window to the current UI theme - the same way
        the built-in Preferences dialog does it (the theme colors come
        from app_proc(PROC_THEME_UI_DICT_GET)). Called on build, on
        every show(), and when CudaText switches its UI theme while the
        window is open (see apply_theme_all). The button_ex buttons,
        the listbox_ex list (header, selection, scrollbar) and the
        owner-drawn rows are themed from the same dict; the form, the
        filter field and the status line are (re-)colored here."""
        if not self.h:
            return
        self._load_draw_theme()
        colors = get_theme_ui_colors()
        try:
            if colors['form_bg'] is not None:
                dlg_proc(self.h, DLG_PROP_SET, prop={
                    'color': colors['form_bg']})
            if colors['edit_bg'] is not None:
                dlg_proc(self.h, DLG_CTL_PROP_SET, name='f_filter',
                         prop={'color': colors['edit_bg']})
            if colors['edit_font'] is not None:
                dlg_proc(self.h, DLG_CTL_PROP_SET, name='f_filter',
                         prop={'font_color': colors['edit_font']})
            if colors['label_font'] is not None:
                dlg_proc(self.h, DLG_CTL_PROP_SET, name='lbl_status',
                         prop={'font_color': colors['label_font']})
        except Exception:
            pass
        # re-color the tab bars (the inactive bars take the new theme's
        # button face color; the active one stays green) - the theme can
        # be switched while the window is open
        self._update_tabs()
        # repaint the owner-drawn rows with the new colors
        if self.h_list and self.rows:
            self.fill_list()

    def _load_draw_theme(self):
        """Colors for painting the list rows. The listbox_ex control
        itself is themed by the engine from the same keys (its item bg
        comes from TreeBg, the selected bg from TreeSelBg - see
        formmain_themes.inc in the CudaText source), so painting the
        rows from the same keys keeps the look identical."""
        d = _theme_dict()
        self._theme = {
            'bg': _theme_color(d, 'TreeBg', 0xFFFFFF),
            'bg_sel': _theme_color(d, 'TreeSelBg', 0x808080),
            'font': _theme_color(d, 'TreeFont', 0x000000),
            'font_sel': _theme_color(d, 'TreeSelFont', 0x000000),
            'border': _theme_color(d, 'ButtonBorderPassive', 0x808080),
            'dead': _theme_color(d, 'TabFontMod', 0x0000A0),
            'path': _theme_color(d, 'TreeSign', None),
        }

    #------------- showing / closing

    def show(self):
        if not self.h:
            self.build()
        # drop stale hover flags (the window was closed while the mouse
        # was over a tab): without this, the next show could paint a
        # hover-colored bar with no mouse over it
        self._hover_btn = -1
        self._hover_bar = -1
        # the UI theme may have changed while the window was hidden:
        # re-read the colors on every show (the form is reused)
        self.apply_theme()
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

    def form_on_resize(self, id_dlg, id_ctl=0, data='', info=''):
        """Keep the tab strip inside the window: the three tab buttons
        keep their fixed TAB_W width (they only shrink when the window
        is too narrow - see _layout_tabs). The list itself is anchored
        to the window (it grows/shrinks with it) and the Path column is
        auto-stretched by the listbox engine (column size 0 = share the
        free width), so the columns always fill the list exactly -
        nothing else to recompute here."""
        if self._closing or not self.h or not self.h_list:
            return
        self._layout_tabs()
        self._repaint_list()

    def _layout_tabs(self):
        """Apply the tab widths after DLG_SCALE and on every window
        resize: the FIXED TAB_W (browser/VS Code tabs hug their
        captions, they do NOT stretch over the whole list width -
        full-width tabs looked bloated), and when the window is too
        narrow for three fixed tabs, three EQUAL shrunk thirds - so the
        strip can never overflow into the right button column. The
        buttons are left-anchored to each other, so only their widths
        are set here."""
        if not self.h:
            return
        sc = self._scale or 1.0
        gap = max(1, int(TAB_GAP * sc))
        left = int(BTN_X * sc)
        # right boundary of the strip = the button column's LIVE left
        # edge (btn_open) minus the same BTN_X gap the main column uses
        try:
            props = dlg_proc(self.h, DLG_CTL_PROP_GET, name='btn_open') or {}
            right = int(props.get('x', 0) or 0) - left
        except Exception:
            right = 0
        if right <= left + 3 * 46:
            return   # button column not placed yet / window too narrow:
                    # keep the last sane layout
        avail = right - left
        fixed = int(TAB_W * sc)
        tab_w = min(fixed, (avail - 2 * gap) // 3)
        for name in ('tab0', 'tab1', 'tab2'):
            try:
                dlg_proc(self.h, DLG_CTL_PROP_SET, name=name,
                         prop={'w': tab_w})
            except Exception:
                pass

    def _live_list_w(self):
        """Live width of the list control ('w' is a live prop), or 0."""
        try:
            props = dlg_proc(self.h, DLG_CTL_PROP_GET, name='list') or {}
            return int(props.get('w', 0) or 0)
        except Exception:
            return 0

    def _live_list_h(self):
        """Live height of the list control, or 0."""
        try:
            props = dlg_proc(self.h, DLG_CTL_PROP_GET, name='list') or {}
            return int(props.get('h', 0) or 0)
        except Exception:
            return 0

    def _live_cols(self):
        """Effective widths of the columns, in REAL pixels. Mirrors
        TATListbox.UpdateColumnWidths: sizes >0 are fixed, a size of 0
        is auto-stretched (it gets the free width of the listbox,
        minus the vertical scrollbar when the rows overflow it).
        self.cols keeps the SET sizes; this returns what is actually
        painted - used for hit-testing (checkbox/star cells) and for
        the header divider positions."""
        cols = list(self.cols)
        if 0 not in cols:
            return cols
        w = self._live_list_w()
        if w <= 0:
            # control size not readable yet: a sane fallback
            return [max(1, int(PATH_MIN_W * self._scale)) if c == 0 else c
                    for c in cols]
        ih = self._item_h()
        h = self._live_list_h()
        if h > 0 and ih > 0 and len(self.rows) * ih > h:
            # the vertical scrollbar is shown (it is inside the control)
            w -= self._sb_w
        fixed = sum(c for c in cols if c > 0)
        auto_w = max(0, w - fixed)
        return [auto_w if c == 0 else c for c in cols]

    #------------- list geometry helpers

    def _item_h(self):
        """Live row height (= the height of the header row too)."""
        try:
            v = int(listbox_proc(self.h_list, LISTBOX_GET_ITEM_H))
            if v > 0:
                return v
        except Exception:
            pass
        return int(ITEM_H * self._scale)

    def _col_bounds(self):
        """[(x0, x1), ...] of the NCOLS columns, from the LIVE widths
        (the Path column is auto-stretched, so its real width is only
        known from the control size)."""
        res = []
        x = 0
        for wc in self._live_cols():
            res.append((x, x + wc))
            x += wc
        return res

    def _row_at(self, y):
        """Visible row index at control-y (-1: header, below the rows,
        or no rows). Accounts for the vertical scroll position."""
        ih = self._item_h()
        if y < ih:
            return -1
        try:
            top = int(listbox_proc(self.h_list, LISTBOX_GET_TOP))
        except Exception:
            top = 0
        idx = top + (y - ih) // ih
        if 0 <= idx < len(self.rows):
            return idx
        return -1

    #------------- list filling

    def _sort_caption(self, mode, base):
        """Column caption with the sort-direction marker."""
        if self.sort_col == 'manual' or mode != self.sort_col:
            return base
        return base + (' ↓' if self.sort_desc else ' ↑')

    def _header_text(self):
        """The themed header line: 'All | ★ | Name | Path | State' with
        the sort arrow on the active column. The first cell ('All')
        toggles all checkboxes when clicked."""
        return LIST_SEP.join([
            _('All'),
            '★',
            self._sort_caption('name', _('Name')),
            self._sort_caption('path', _('Path')),
            self._sort_caption('state', _('State')),
        ])

    def _apply_columns(self):
        """Push the column widths + the header line into the listbox
        (this repaints the themed header)."""
        if not self.h_list:
            return
        try:
            listbox_proc(self.h_list, LISTBOX_SET_COLUMNS,
                         text=[int(c) for c in self.cols])
            listbox_proc(self.h_list, LISTBOX_SET_HEADER,
                         text=self._header_text())
        except Exception:
            pass

    def _repaint_list(self):
        """Best-effort repaint of the owner-drawn rows (without
        refilling): nudge the top item index - SetItemTop invalidates
        the control, and the two opposite nudges cancel out."""
        if not self.h_list:
            return
        try:
            n = int(listbox_proc(self.h_list, LISTBOX_GET_COUNT))
            if n >= 2:
                top = int(listbox_proc(self.h_list, LISTBOX_GET_TOP))
                if top > 0:
                    listbox_proc(self.h_list, LISTBOX_SET_TOP, index=0)
                    listbox_proc(self.h_list, LISTBOX_SET_TOP, index=top)
                else:
                    listbox_proc(self.h_list, LISTBOX_SET_TOP, index=1)
                    listbox_proc(self.h_list, LISTBOX_SET_TOP, index=0)
        except Exception:
            pass

    def fill_list(self, keep_selection=True):
        if not self.h or not self.h_list:
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
            'checked': _norm_path(it['path']) in self.checked,
        } for (data_idx, it) in self.view]

        # items: the visible text is painted by on_draw_item, the item
        # text only carries the data (name + path, sep-separated)
        try:
            listbox_proc(self.h_list, LISTBOX_DELETE_ALL)
            for r in self.rows:
                listbox_proc(self.h_list, LISTBOX_ADD, index=-1,
                             text=LIST_SEP.join((r['name'], r['path'])))
        except Exception:
            pass
        self._apply_columns()

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
                listbox_proc(self.h_list, LISTBOX_SET_SEL, index=sel)
            except Exception:
                pass

        self._update_tabs()
        self.update_status()

    def _update_tabs(self):
        """Redraw the tab strip for the current self.tab. All three tabs
        ALWAYS have the same caption style, position and size
        (browser/VS Code style) - the ONLY differences are the COLORS of
        the bottom bars ('ulineN' panels in the TAB_ULINE_H strip
        between the tab buttons and the list): GREEN for the active
        tab, the theme's button face color for the inactive ones (the
        same color the flat tab buttons paint themselves with), and the
        theme's button HOVER color for an inactive tab while the mouse
        is over it - the hovered tab face takes that very same color
        (the engine feeds both from the 'ButtonBgPassive'/'ButtonBgOver'
        theme keys - see formmain_themes.inc), so the tab and its bar
        read as ONE hovered element. The ACTIVE tab's green bar is
        never hover-colored: the green IS the state marker.
        The bar height is TAB_ULINE_H + TAB_OVERLAP: its top TAB_OVERLAP
        px reach UP over the button's own bottom border to hide the
        border line that used to show between the tab and the bar (see
        TAB_OVERLAP). The 'h' is re-applied here in REAL pixels (this
        runs after DLG_SCALE) to survive any scale rounding, and +1 px
        biases the bottom edge slightly UNDER the list (the list is
        created later and paints over the bars), so rounding can never
        open a hairline gap between the bar and the list."""
        sc = self._scale or 1.0
        # the theme's flat-button face color (what button_ex paints
        # itself with); falls back to the form color when the theme
        # does not define it
        d = _theme_dict()
        face = _theme_color(d, 'ButtonBgPassive', None)
        if face is None:
            face = _theme_color(d, 'TabBg', 0xE0E0E0)
        # the exact color the hovered flat-button face is painted with
        # (fed from the same 'ButtonBgOver' theme key), so the hovered
        # bar and the hovered face match perfectly
        hover = _theme_color(d, 'ButtonBgOver', 0xD0B0B0)
        ov = max(1, int(TAB_OVERLAP * sc))
        bar_h = int(TAB_ULINE_H * sc) + ov + 1
        for i in range(3):
            if i == self.tab:
                col = TAB_GREEN
            elif i == self._hover_btn or i == self._hover_bar:
                col = hover
            else:
                col = face
            try:
                dlg_proc(self.h, DLG_CTL_PROP_SET,
                         name='uline{}'.format(i),
                         prop={'h': bar_h,
                               'color': col})
            except Exception:
                pass

    def update_status(self):
        if not self.h:
            return
        cat = self.cat()
        total, starred = self.core.counts(cat)
        shown = len(self.rows)
        nchecked = sum(1 for r in self.rows if r['checked'])
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
        if nchecked:
            parts.append(_('{} checked').format(nchecked))
        parts.append(sort_info)
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
            return int(listbox_proc(self.h_list, LISTBOX_GET_SEL))
        except Exception:
            return -1

    def selected_row(self):
        """The row dict of the selection, or None."""
        i = self._sel_index()
        if 0 <= i < len(self.rows):
            return self.rows[i]
        return None

    #------------- owner drawing (on_draw_item)

    def _draw_text_cell(self, cnv, x, y, w, row_h, text, color,
                        centered=False, style=0):
        """Draw one text cell, vertically centered; long texts are cut
        with an ellipsis so they never bleed into the next column."""
        try:
            canvas_proc(cnv, CANVAS_SET_FONT, text='default',
                        color=color, style=style)
            tw, th = canvas_proc(cnv, CANVAS_GET_TEXT_SIZE, text=text)
            if tw > w - 8 and tw > 0 and len(text) > 1:
                # proportional cut, then trim until it fits
                k = max(1, int((w - 8 - 12) * len(text) / tw))
                text = text[:k] + '…'
                tw, th = canvas_proc(cnv, CANVAS_GET_TEXT_SIZE, text=text)
                while tw > w - 8 and len(text) > 2:
                    text = text[:-2] + '…'
                    tw, th = canvas_proc(cnv, CANVAS_GET_TEXT_SIZE, text=text)
            ty = y + max(0, (row_h - th) // 2)
            tx = x + (w - tw) // 2 if centered else x + 4
            canvas_proc(cnv, CANVAS_TEXT, text=text, x=tx, y=ty)
        except Exception:
            pass

    def list_on_draw_item(self, id_dlg, id_ctl, data='', info=''):
        """Paint one list row from the theme colors: row background
        (selected = TreeSelBg, normal = TreeBg - the same keys the
        engine themes the listbox itself from), the checkbox square,
        the star glyph, the name, the path and the dead-path '?'."""
        if not isinstance(data, dict):
            return
        cnv = data.get('canvas')
        index = data.get('index')
        rect = data.get('rect')
        if cnv is None or index is None or not rect:
            return
        try:
            index = int(index)
            x0, y0, x1, y1 = (int(rect[0]), int(rect[1]),
                              int(rect[2]), int(rect[3]))
        except Exception:
            return
        if not (0 <= index < len(self.rows)):
            return
        row = self.rows[index]
        th = self._theme
        if not th:
            self._load_draw_theme()
            th = self._theme
        row_h = y1 - y0
        if row_h <= 0:
            return

        selected = (index == self._sel_index())
        bg = th['bg_sel'] if selected else th['bg']
        fg = th['font_sel'] if selected else th['font']

        # row background
        try:
            canvas_proc(cnv, CANVAS_SET_BRUSH, color=bg, style=BRUSH_SOLID)
            canvas_proc(cnv, CANVAS_RECT_FILL, x=x0, y=y0, x2=x1, y2=y1)
        except Exception:
            return

        bounds = self._col_bounds()

        # checkbox (col 0)
        sz = int(CHECK_SZ * self._scale)
        sz = min(sz, row_h - 8, self.cols[COL_CHECK] - 8)
        if sz >= 8:
            cx = bounds[COL_CHECK][0] + (self.cols[COL_CHECK] - sz) // 2
            cy = y0 + (row_h - sz) // 2
            try:
                canvas_proc(cnv, CANVAS_SET_PEN, color=th['border'],
                            size=1, style=PEN_STYLE_SOLID)
                canvas_proc(cnv, CANVAS_SET_BRUSH, color=bg, style=BRUSH_SOLID)
                canvas_proc(cnv, CANVAS_RECT,
                            x=cx, y=cy, x2=cx + sz, y2=cy + sz)
                if row['checked']:
                    canvas_proc(cnv, CANVAS_SET_PEN, color=fg,
                                size=2, style=PEN_STYLE_SOLID)
                    m = max(2, sz // 5)
                    canvas_proc(cnv, CANVAS_LINE,
                                x=cx + m, y=cy + sz // 2,
                                x2=cx + sz // 2, y2=cy + sz - m)
                    canvas_proc(cnv, CANVAS_LINE,
                                x=cx + sz // 2, y=cy + sz - m,
                                x2=cx + sz - m, y2=cy + m)
            except Exception:
                pass

        # star glyph (col 1) - the UTF-8 glyphs, no image files
        self._draw_text_cell(cnv, bounds[COL_STAR][0], y0,
                             self.cols[COL_STAR], row_h,
                             '★' if row['star'] else '☆', fg, centered=True)

        # name (col 2) - dead paths painted in the theme's accent color
        name_col = th['dead'] if row['dead'] else fg
        self._draw_text_cell(cnv, bounds[COL_NAME][0], y0,
                             self.cols[COL_NAME], row_h,
                             row['name'], name_col)

        # path (col 3): refill its background first so a long name
        # cannot bleed into it, then draw the path (dimmed when the
        # theme defines a sign color). The cell width is the LIVE
        # width of the auto-stretched column (self.cols[COL_PATH] is
        # just the 0 = "stretch" marker - passing 0 as the width
        # truncated every path to a single letter + ellipsis)
        try:
            canvas_proc(cnv, CANVAS_SET_BRUSH, color=bg, style=BRUSH_SOLID)
            canvas_proc(cnv, CANVAS_RECT_FILL, x=bounds[COL_PATH][0], y=y0,
                        x2=bounds[COL_PATH][1], y2=y1)
        except Exception:
            pass
        path_col = fg
        if not selected and th.get('path') is not None:
            path_col = th['path']
        path_w = bounds[COL_PATH][1] - bounds[COL_PATH][0]
        self._draw_text_cell(cnv, bounds[COL_PATH][0], y0,
                             path_w, row_h,
                             row['path'], path_col)

        # state (col 4): fill to the right edge (covers a long path),
        # then the centered '?' for dead paths
        try:
            canvas_proc(cnv, CANVAS_SET_BRUSH, color=bg, style=BRUSH_SOLID)
            canvas_proc(cnv, CANVAS_RECT_FILL, x=bounds[COL_STATE][0], y=y0,
                        x2=x1, y2=y1)
        except Exception:
            pass
        if row['dead']:
            self._draw_text_cell(cnv, bounds[COL_STATE][0], y0,
                                 self.cols[COL_STATE], row_h,
                                 '?', th['dead'], centered=True)

        # vertical column separators: a 1px line at the right edge of
        # every column, over the whole row height (the themed header
        # draws the same lines for its captions; the default row
        # painting of listbox_ex draws them too, but owner-drawn rows
        # replace that painting - so they are drawn here)
        try:
            canvas_proc(cnv, CANVAS_SET_PEN, color=th['border'],
                        size=1, style=PEN_STYLE_SOLID)
            for b in bounds:
                lx = b[1] - 1
                if x0 <= lx < x1:
                    canvas_proc(cnv, CANVAS_LINE,
                                x=lx, y=y0, x2=lx, y2=y1 - 1)
        except Exception:
            pass

    #------------- events: filter, tabs, selection

    def on_filter_change(self, id_dlg, id_ctl, data='', info=''):
        try:
            props = dlg_proc(self.h, DLG_CTL_PROP_GET, name='f_filter') or {}
            self.filter = str(props.get('val', '') or '')
        except Exception:
            self.filter = ''
        self.fill_list(keep_selection=False)

    def on_tab_button(self, index):
        """A themed tab button was clicked: switch the category (the tab
        strip is recolored by _update_tabs via fill_list - the active
        tab's bottom bar turns green, the others take the button color;
        sizes never change)."""
        if index == self.tab:
            return
        self.tab = index
        self.selected_data = -1
        self.fill_list(keep_selection=False)

    def on_tab_hover(self, index, entered, on_bar):
        """The mouse entered/left a tab button or its bottom bar panel
        (on_mouse_enter / on_mouse_exit of the button_ex and panel
        controls; the bar overlaps the button's bottom edge, so the
        mouse crosses between the two controls inside one tab). While
        the mouse is anywhere over an INACTIVE tab, its bar takes the
        SAME theme hover color as the tab face, so the tab and its bar
        read as ONE hovered element. The ACTIVE tab's green bar is
        never hover-colored - the green is the state marker itself, and
        the hovered face of that tab still highlights on its own.
        The two flags (_hover_btn / _hover_bar) are kept SEPARATE and
        only merged in _update_tabs: Windows can deliver a control's
        mouse-LEAVE after the NEXT control's mouse-ENTER, and a single
        shared 'hovered tab' index would then clear the just-set hover
        (leave(tab1's face) arriving after enter(tab1's bar) would
        erase it); with separate flags the late leave only clears its
        own source."""
        if not self.h or self._closing:
            return
        if on_bar:
            self._hover_bar = index if entered else -1
        else:
            self._hover_btn = index if entered else -1
        self._update_tabs()

    def list_on_change(self, id_dlg, id_ctl, data='', info=''):
        """Selection in the list moved."""
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

    def _row_by_click(self, data):
        """Row dict under a click (x, y), or None."""
        pos = self._click_pos(data)
        if pos is None:
            return None
        idx = self._row_at(pos[1])
        if idx < 0:
            return None
        return self.rows[idx]

    def _toggle_check_row(self, row):
        """Flip the checkbox of one row and repaint."""
        key = _norm_path(row['path'])
        if key in self.checked:
            self.checked.discard(key)
            row['checked'] = False
        else:
            self.checked.add(key)
            row['checked'] = True
        self._apply_columns()
        self._repaint_list()
        self.update_status()

    def list_on_click(self, id_dlg, id_ctl, data='', info=''):
        """A single click inside the list: a click on the checkbox cell
        checks/unchecks that row, a click on the star glyph cell toggles
        the star of that row. Clicks on the column header (reported
        separately by on_click_header) and on the other columns are
        ignored here."""
        if self._closing or not self.h_list:
            return
        pos = self._click_pos(data)
        if pos is None:
            return
        x, y = pos
        if y < self._item_h():
            return   # the header: on_click_header handles it
        row = self._row_by_click(data)
        if row is None:
            return   # clicked the empty area below the rows

        bounds = self._col_bounds()
        if x < bounds[COL_CHECK][1]:
            self._toggle_check_row(row)
            return
        if x < bounds[COL_STAR][1]:
            data_idx = self.core.find(self.cat(), row['path'])
            if data_idx >= 0:
                self.core.toggle_star(self.cat(), data_idx)
                self.selected_data = data_idx
                self.fill_list()
            return

    def list_on_dbl_click(self, id_dlg, id_ctl, data='', info=''):
        """Double-click opens the favorite - except on the checkbox and
        star cells: there the 1st click already toggled the flag, a
        double-click must not also open the item."""
        pos = self._click_pos(data)
        if pos is not None:
            x, y = pos
            if y >= self._item_h():
                bounds = self._col_bounds()
                if x < bounds[COL_STAR][1]:
                    return
        self.open_selected()

    def list_on_header_click(self, id_dlg, id_ctl, data='', info=''):
        """Click on a themed column header (data = column index): the
        'All' cell checks/unchecks all rows, the other columns sort."""
        if self._suppress_header_click:
            # the release of a column-resize drag: not a sort click
            self._suppress_header_click = False
            return
        if isinstance(data, (tuple, list)) and data:
            data = data[0]
        try:
            col = int(data)
        except (TypeError, ValueError):
            return
        if col == COL_CHECK:
            self._toggle_check_all()
            return
        self.sort_by_column(col)

    def _toggle_check_all(self):
        """The 'All' header cell: check everything, or uncheck when
        everything is checked already."""
        if not self.rows:
            return
        keys = [_norm_path(r['path']) for r in self.rows]
        if all(r['checked'] for r in self.rows):
            for k in keys:
                self.checked.discard(k)
            for r in self.rows:
                r['checked'] = False
        else:
            for k in keys:
                self.checked.add(k)
            for r in self.rows:
                r['checked'] = True
        self._apply_columns()
        self._repaint_list()
        self.update_status()

    #------------- events: column resizing (header dividers)

    def _boundary_at(self, x):
        """Index of the header divider near x (-1 = none). Boundary k is
        between column k and k+1. Uses the LIVE column widths (the Path
        column is auto-stretched, so its right edge sits at the end of
        the header)."""
        best = -1
        best_d = RESIZE_GRIP
        pos = 0
        cols = self._live_cols()
        for k in range(NCOLS - 1):
            pos += cols[k]
            d = abs(x - pos)
            if d <= best_d:
                best = k
                best_d = d
        return best

    def _set_list_cursor(self, hsplit):
        """Show the horizontal-split (left-right arrows) cursor over
        the list - or the normal one. The 'cursor' prop is a live
        control prop (LCL cursor id; crHSplit = -14)."""
        try:
            dlg_proc(self.h, DLG_CTL_PROP_SET, name='list',
                     prop={'cursor': CURSOR_HSPLIT if hsplit else 0})
        except Exception:
            pass

    def list_on_mouse_down(self, id_dlg, id_ctl, data='', info=''):
        """Start a column resize when the press is on the header, near a
        divider between two captions (data = {'btn','state','x','y'},
        btn 0 = left button)."""
        if not isinstance(data, dict) or self._closing:
            return
        # any new press resets the "swallow the next header click" flag
        # (it may be stale when a resize drag ended outside the list)
        self._suppress_header_click = False
        try:
            btn = int(data.get('btn', -1))
            x = int(data.get('x', -1))
            y = int(data.get('y', -1))
        except (TypeError, ValueError):
            return
        if btn != 0 or y < 0:
            return
        if y >= self._item_h():
            return   # below the header: not a resize
        k = self._boundary_at(x)
        if k < 0:
            return
        # which column the drag resizes: normally the one LEFT of the
        # divider; but the Path|State divider resizes STATE - the Path
        # column is auto-stretched (size 0), it must stay 0 and simply
        # absorbs whatever width is left, so the divider still follows
        # the mouse exactly
        k_col = COL_STATE if k == COL_PATH else k
        self._col_drag = {
            'col': k_col,
            'x0': x,
            'orig_w': self.cols[k_col] if self.cols[k_col] > 0
                      else self._live_cols()[k_col],
            'sign': -1 if k == COL_PATH else 1,
            'moved': False,
        }
        self._set_list_cursor(True)

    def list_on_mouse_move(self, id_dlg, id_ctl, data='', info=''):
        """With a drag in progress: resize the dragged column live (the
        divider follows the mouse; the column cannot shrink below its
        minimum). Without a drag: show the split cursor while the mouse
        hovers a draggable header divider."""
        if not isinstance(data, dict) or self._closing:
            return
        try:
            x = int(data.get('x', -1))
            y = int(data.get('y', -1))
        except (TypeError, ValueError):
            return

        if self._col_drag is None:
            # hover: split cursor over a header divider, normal elsewhere
            on_divider = (0 <= y < self._item_h()
                          and self._boundary_at(x) >= 0)
            if on_divider != self._cursor_split:
                self._cursor_split = on_divider
                self._set_list_cursor(on_divider)
            return

        d = self._col_drag
        dx = x - d['x0']
        if not d['moved'] and abs(dx) < DRAG_THRESHOLD:
            return
        d['moved'] = True
        k = d['col']
        new_w = d['orig_w'] + d['sign'] * dx
        min_w = int(COL_MIN_W[k] * self._scale)
        self.cols[k] = max(min_w, new_w)
        self._apply_columns()

    def list_on_mouse_up(self, id_dlg, id_ctl, data='', info=''):
        """End the column resize; a real drag swallows the header click
        (sort) that the mouse-up would fire."""
        if self._col_drag is None:
            return
        moved = self._col_drag.get('moved')
        self._col_drag = None
        self._cursor_split = False
        self._set_list_cursor(False)
        if moved:
            self._suppress_header_click = True
            # final repaint of the owner-drawn rows
            self._repaint_list()

    def list_on_mouse_exit(self, id_dlg, id_ctl, data='', info=''):
        """Mouse left the list: drop the split cursor (and a resize drag
        that is still tracked - the release outside is not reported)."""
        if self._col_drag is not None:
            self._col_drag = None
            self._suppress_header_click = True
        if self._cursor_split:
            self._cursor_split = False
            self._set_list_cursor(False)

    #------------- events: context menu

    def list_on_menu(self, id_dlg, id_ctl, data='', info=''):
        """Right-click context menu of the favorites list. data is a
        dict with the click position in control coordinates; a click on
        the header offers the Sort commands, a click on a row selects
        it and offers the row + checked commands."""
        if not isinstance(data, dict):
            return
        try:
            y = int(data.get('y', -1))
        except (TypeError, ValueError):
            y = -1
        on_header = 0 <= y < self._item_h()

        if not on_header and y >= self._item_h():
            idx = self._row_at(y)
            if idx >= 0:
                try:
                    listbox_proc(self.h_list, LISTBOX_SET_SEL, index=idx)
                except Exception:
                    pass

        row = None if on_header else self.selected_row()
        nchecked = sum(1 for r in self.rows if r['checked'])
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

        if on_header:
            # the sort fallback menu (for users who don't find the
            # header-click sorting)
            add(_('Sort: ★ first, then name'), 'sort_star')
            add(_('Sort: by name'), 'sort_name')
            add(_('Sort: by path'), 'sort_path')
            add(_('Sort: by state, dead last'), 'sort_state')
            add(_('Sort: manual order'), 'sort_manual')
            menu_proc(menu, MENU_ADD, caption='-')
            add((_('Uncheck all') if nchecked else _('Check all')),
                'uncheck_all' if nchecked else 'check_all',
                bool(self.rows))
            menu_proc(menu, MENU_SHOW, command='')
            return

        add(_('Open') + ' (Enter)', 'open', row is not None)
        add(_('Reveal in file manager'), 'reveal', row is not None)
        add(_('Copy path'), 'copy', row is not None)
        menu_proc(menu, MENU_ADD, caption='-')
        add((_('Unstar') if (row and row['star']) else _('Star')) + ' ★', 'star',
            row is not None)
        add(_('Rename...') + ' (F2)', 'rename', row is not None)
        add(_('Delete') + ' (Del)', 'delete', row is not None)

        # commands on ALL checked favorites
        menu_proc(menu, MENU_ADD, caption='-')
        cap = _('Checked ({}):').format(nchecked) if nchecked else _('Checked (none):')
        add(cap, None, False)
        for caption, action in (
                (_('Open all checked'), 'ck_open'),
                (_('Star all checked'), 'ck_star'),
                (_('Unstar all checked'), 'ck_unstar'),
                (_('Copy paths of checked'), 'ck_copy'),
                (_('Rename checked...'), 'ck_rename'),
                (_('Move checked up'), 'ck_up'),
                (_('Move checked down'), 'ck_down'),
                (_('Delete checked...'), 'ck_delete'),
                (_('Uncheck all'), 'ck_uncheck'),
        ):
            add(caption, action, nchecked > 0)

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
            if key == VK_SPACE:
                row = self.selected_row()
                if row is not None:
                    self._toggle_check_row(row)
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
            try:
                dlg_proc(self.h, DLG_FOCUS)
            except Exception:
                pass

    #------------- actions (on the selected row)

    def _open_paths(self, paths):
        """Open favorites NOT inside the dialog callback: the OS shell
        call / the editor opening switches the focus away, CudaText
        deactivates, and the nested event processing crashed with
        "Access violation" (on_app_deactivate of several plugins). The
        timer lets the callback return first - the same trick the
        cuda_favorites plugin uses. When SEVERAL favorites are opened,
        each one gets its OWN timer tick (a chain of one-shot timers):
        opening them all inside a single callback made the app process
        a burst of nested events (tab switches, focus changes) while
        it was mid-deactivation - another source of the "Access
        violation" reports. The focus is given back to the window ONLY
        when nothing was opened in the OS file manager: a shell-opened
        folder made ANOTHER app active, and forcing the focus back in
        that moment fought the app deactivation."""
        paths = [str(p) for p in paths if p]
        if not paths:
            return
        # a folder favorite is opened by the OS shell (Explorer,
        # Finder...); projects and files are opened inside CudaText
        opened_shell = any(
            os.path.isdir(p) and not p.endswith(PROJECT_EXTENSION)
            for p in paths)

        def open_next(tag=None):
            if not paths:
                if not opened_shell:
                    self.focus_again()
                return
            p = paths.pop(0)
            try:
                open_item(p)
            except Exception as ex:
                print('Favorites: cannot open favorite: ' + str(ex))
            # chain the next one (or the final focus-restore)
            timer_proc(TIMER_START_ONE, open_next, OPEN_DELAY_MS)

        timer_proc(TIMER_START_ONE, open_next, OPEN_DELAY_MS)

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
        self.checked.discard(_norm_path(row['path']))
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
            try:
                reveal_in_file_manager(path)
            except Exception as ex:
                print('Favorites: cannot reveal in file manager: ' + str(ex))

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
            self._update_tabs()
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

    #------------- actions on the checked favorites

    def checked_rows(self):
        """The checked rows of the current view, in view order."""
        return [r for r in self.rows if r['checked']]

    def _checked_or_status(self):
        rows = self.checked_rows()
        if not rows:
            msg_status(_('Favorites: no checked favorites (click the checkbox column)'))
        return rows

    def op_checked_open(self):
        rows = self._checked_or_status()
        if rows:
            self._open_paths([r['path'] for r in rows])

    def op_checked_star(self, target):
        """Set the star of every checked favorite to target (True/
        False); one save, one refill."""
        rows = self._checked_or_status()
        if not rows:
            return
        cat = self.cat()
        for r in rows:
            if 0 <= r['data_idx'] < len(self.core.data[cat]):
                self.core.data[cat][r['data_idx']]['star'] = bool(target)
        self.core.save()
        self.fill_list()

    def op_checked_copy(self):
        rows = self._checked_or_status()
        if not rows:
            return
        app_proc(PROC_SET_CLIP, '\n'.join(r['path'] for r in rows))
        msg_status(_('Favorites: copied {} path(s)').format(len(rows)))

    def op_checked_rename(self):
        """Rename the checked favorites one by one; Cancel stops."""
        rows = self._checked_or_status()
        if not rows:
            return
        cat = self.cat()
        done = 0
        for i, r in enumerate(rows):
            new = dlg_input(_('New name for {} of {}: {}').format(
                i + 1, len(rows), r['name']), r['name'])
            if new is None:
                break
            new = new.strip()
            if new and new != r['name']:
                if 0 <= r['data_idx'] < len(self.core.data[cat]):
                    self.core.rename(cat, r['data_idx'], new)
                    done += 1
        if done:
            msg_status(_('Favorites: renamed {}').format(done))
        self.fill_list()

    def op_checked_move(self, delta):
        """Move the checked block up/down as a whole (keeps the inner
        order of the block); switches the list to manual order."""
        rows = self._checked_or_status()
        if not rows:
            return
        cat = self.cat()
        idxs = sorted(r['data_idx'] for r in rows)
        idxset = set(idxs)
        lst = self.core.data[cat]
        if len(idxs) >= len(lst):
            msg_status(_('Favorites: everything is checked'))
            return
        if delta < 0:
            # the first unchecked item above the block's top
            prev = next((i for i in range(idxs[0] - 1, -1, -1)
                         if i not in idxset), -1)
            if prev < 0:
                msg_status(_('Favorites: already at the top'))
                return
            block = [lst[i] for i in idxs]
            rest = [it for i, it in enumerate(lst) if i not in idxset]
            # insert the block BEFORE that item (its index in rest)
            pos = sum(1 for i in range(prev) if i not in idxset)
            self.core.data[cat] = rest[:pos] + block + rest[pos:]
        else:
            # the first unchecked item below the block's bottom
            nxt = next((i for i in range(idxs[-1] + 1, len(lst))
                        if i not in idxset), -1)
            if nxt < 0:
                msg_status(_('Favorites: already at the bottom'))
                return
            block = [lst[i] for i in idxs]
            rest = [it for i, it in enumerate(lst) if i not in idxset]
            # insert the block AFTER that item (its index in rest + 1)
            pos = sum(1 for i in range(nxt + 1) if i not in idxset)
            self.core.data[cat] = rest[:pos] + block + rest[pos:]
        if self.sort_col != 'manual':
            self.sort_col = 'manual'
            msg_status(_('Favorites: manual order - Move up/down reorders the list'))
        self.core.save()
        self.fill_list()

    def op_checked_delete(self):
        rows = self._checked_or_status()
        if not rows:
            return
        cat = self.cat()
        if msg_box(_('Favorites: delete {} checked favorite(s)?').format(len(rows)),
                   MB_OKCANCEL + MB_ICONQUESTION) != ID_OK:
            return
        n = self.core.remove_many(cat, [r['data_idx'] for r in rows])
        for r in rows:
            self.checked.discard(_norm_path(r['path']))
        if n:
            msg_status(_('Favorites: removed {}').format(n))
        self.selected_data = -1
        self.fill_list(keep_selection=False)

    def op_checked_uncheck(self):
        for r in self.rows:
            self.checked.discard(_norm_path(r['path']))
            r['checked'] = False
        self._apply_columns()
        self._repaint_list()
        self.update_status()

    def btn_checked_click(self, id_dlg, id_ctl, data='', info=''):
        """The 'Checked' button: a menu with the commands that run on
        ALL checked favorites (the same commands the right-click
        menu has)."""
        nchecked = sum(1 for r in self.rows if r['checked'])
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

        cap = _('Checked ({}):').format(nchecked) if nchecked else _('Checked (none):')
        add(cap, None, False)
        for caption, action in (
                (_('Open all checked'), 'ck_open'),
                (_('Star all checked'), 'ck_star'),
                (_('Unstar all checked'), 'ck_unstar'),
                (_('Copy paths of checked'), 'ck_copy'),
                (_('Rename checked...'), 'ck_rename'),
                (_('Move checked up'), 'ck_up'),
                (_('Move checked down'), 'ck_down'),
                (_('Delete checked...'), 'ck_delete'),
                (_('Uncheck all'), 'ck_uncheck'),
        ):
            add(caption, action, nchecked > 0)
        menu_proc(menu, MENU_SHOW, command='')

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
        if action == 'check_all':
            self._toggle_check_all()
            return
        if action == 'uncheck_all':
            self.op_checked_uncheck()
            return
        if action == 'ck_open':
            self.op_checked_open()
        elif action == 'ck_star':
            self.op_checked_star(True)
        elif action == 'ck_unstar':
            self.op_checked_star(False)
        elif action == 'ck_copy':
            self.op_checked_copy()
        elif action == 'ck_rename':
            self.op_checked_rename()
        elif action == 'ck_up':
            self.op_checked_move(-1)
        elif action == 'ck_down':
            self.op_checked_move(+1)
        elif action == 'ck_delete':
            self.op_checked_delete()
        elif action == 'ck_uncheck':
            self.op_checked_uncheck()
        elif action == 'open':
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

#==========================================================
# module API (used by cuda_project_man/__init__.py)
#==========================================================

def apply_theme_all():
    """Re-apply the current UI theme to the open favorites window.
    Called when CudaText switches its UI theme while the window is
    open (on_state / APPSTATE_THEME_UI in __init__.py); a hidden
    window is re-themed by its next show() anyway."""
    if _form is not None and _form.h:
        _form.apply_theme()


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


def add_favorite(cat, path):
    """Add one entry with a status-bar message (used by the Project
    Manager context-menu item)."""
    core = get_core()
    added, err = core.add(cat, path)
    if added:
        msg_status(_('Favorites: added ') + str(path))
        if _form is not None and _form.h:
            _form.tab = CATS.index(cat)
            _form._update_tabs()
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
