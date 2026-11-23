'''Favorites (bookmarks) manager for the CudaText "Project Manager" plugin.

Stores three kinds of bookmarks - projects, files, folders - in the
'favorites' namespace of "cuda_project_man_store.json" (the state store
shared with the Project Manager itself: recent projects, the opened
folder) in the CudaText settings folder. A data file of the older
versions, "cuda_project_man_favorites.json", is imported once and
removed (store.migrate_legacy_favorites).

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
  Add... / Add opened..., Batch actions..., Move up / Move down,
  Delete; Close in the bottom-right corner) - the buttons move with
  the window's right edge, the main column ends exactly where the
  button column begins
- "tabs": three tab rectangles (Projects / Files / Folders) drawn as
  'colorpanel' controls (TATPanelColor: a plain rectangle painting its
  own face color, centered caption and its own 1px border - ex0/ex3
  props): equal FIXED widths (TAB_W - the tabs hug their captions like
  the tabs of a browser or VS Code, they do NOT stretch over the whole
  list width), joined side by side directly above the list, FLUSH with
  it. The whole strip is recolored by _update_tabs from the UI theme:
  the ACTIVE tab keeps the theme's flat-button face color (the very
  color the old button_ex tabs were painted with), the INACTIVE tabs
  are DIMMED - their face keeps a clearly visible step below the
  active face on every theme (blended toward the window background,
  extrapolating past it when the theme paints the two nearly equal -
  see _dim_toward) and their caption blends toward that dimmed face -
  so the strip reads at a glance which tab is the open one; hovering
  any tab highlights it with the theme's button hover color. The
  themed button_ex could NOT be used
  for this: the flat button always paints one and the same theme face
  (its Color prop is ignored), so an inactive tab could not be dimmed;
  the OS-drawn tab control could not be themed either
- the list is the "listbox_ex" control - the flat listbox of CudaText
  dialogs, painted by the UI theme like the built-in Preferences
  dialog: its column header, items, selection and scrollbar all follow
  the theme, live, also when the theme is switched while CudaText runs;
  the list is ANCHORED to the form, so it grows/shrinks with the window
- columns: # / check / star / Name / Path / Attached projects /
  State, separated by vertical lines (painted with the rows). Every
  row is painted by the plugin (on_draw_item owner-drawing) from the
  theme colors: checkbox, star glyph, texts, column separators. The
  6th column is 'Attached projects' on the Files and Folders tabs
  and 'Attachments' on the Projects tab - the files/folders attached
  TO that project (see the attachments paragraph below)
- '#' column (first): the QUICK-SELECT label of every row - 1..9,
  then a..z, then aa..zz, then aaa..zzz and so on (quick_label -
  bijective base-26). Type a label with PLAIN keys (filter not
  focused) and that row is selected: a short type-ahead buffer with a
  KEY_SELECT_PAUSE-second pause, like the shortcut column of the
  cuda_favorites plugin but without its 1-9/a-z limit - typing 'aaa'
  selects the 711th row
- checkbox column: click it to check rows, then run one command on ALL
  checked favorites at once - open, star/unstar, copy paths, rename,
  move up/down (as a block), delete - from the "Batch actions" button
  or from the right-click menu's "Batch actions" submenu; click the
  "All" header cell to check/uncheck everything; Space checks/unchecks
  the selected row
- star column (★/☆): click the star cell of a row to toggle it (a
  double-click on the star cell only toggles, it does not open)
- sorting: click a column header (star / Name / Path / the attachment
  column / State): 1st click ascending, 2nd descending, 3rd switches
  to manual (custom) order; the right-click menus have the same
  commands in their "Sort" submenu as a fallback; the direction is
  shown by an arrow in the header caption
- column widths are RESIZABLE: drag the vertical divider between two
  header captions (the mouse cursor changes to the split shape over a
  divider); the Path column is auto-stretched - it always fills the
  free width, also when the window is resized
- filter field at the top filters by name, path and the attachment
  column's text (attached projects on the Files/Folders tabs,
  attachments on the Projects tab)
- tab switching from the keyboard, from any focused control:
  Ctrl+PgUp/PgDn and Ctrl+Left/Right cycle the Projects / Files /
  Folders tabs (with wrap-around), Alt+P / Alt+F / Alt+O jump straight
  to the Projects / Files / Folders tab
- while the window is focused: Ctrl+1..9 / Alt+1..9 open the item at
  that index (index in the currently visible, filtered list), typing
  an item's quick-select label (plain keys, see the '#' column)
  SELECTS it, Enter opens the selection, F2 renames it, Space checks
  it, Alt+Up/Down move it, Up/Down cycle the list (from any focused
  control, wrapping at the ends), Esc closes the window. Del deletes
  the SELECTED favorite - but while the filter field is focused, Del
  stays in the field and deletes the next letter, as in any editor
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

ATTACHMENTS: a file/folder favorite can be ATTACHED to one or several
project favorites ("Attach favorite to project..." in the context
menu; "Attach checked to project..." attaches every checked row of
the tab to one picked project at once). The links live in the item's
'projects' key of our json. The Files and Folders tabs show them in
the 'Attached projects' column, and the Projects tab shows the
reverse in its 'Attachments' column - the files/folders attached to
that project; both columns are filterable (the filter field) and
sortable; a project favorite's context menu has
"List attached files/folders..." (the same filterable list; picking
an entry opens it). Deleting a project favorite drops every link to
it; deleting a file/folder favorite drops its links with it.

"Find favorites..." (the plugin menu, the panel context menu and the
command palette) opens the dlg_menu list dialog - the CudaText API
dialog with its own filter field and close icon - over ALL favorites,
each entry prefixed 'project:' / 'file:' / 'folder:' and '?' before
the name when the path is absent on disk; Enter opens the pick.

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
import time

from cudatext import *
from cudatext_keys import *
from cudax_lib import get_translation
from .store import (migrate_legacy_favorites, store_path, store_read,
                     store_update)
from .utils import (editor_default_metrics, font_row_h, log_error,
                    option_font_size)

_ = get_translation(__file__)  # i18n

IS_WIN = os.name == 'nt'
IS_MAC = sys.platform == 'darwin'
OS_SUFFIX = app_proc(PROC_GET_OS_SUFFIX, '')

#: our own storage file
# the Favorites data lives in the 'favorites' namespace of the shared
# store file cuda_project_man_store.json (it used to be a file of its
# own, cuda_project_man_favorites.json - store.migrate_legacy_favorites
# imports a leftover old file once and removes it)
FAVORITES_FILENAME = 'cuda_project_man_store.json'
#: data file of the "Favorites" plugin (cuda_favorites by kvichans);
#: import-only - never created or modified by this plugin
EXT_FAVORITES_FILENAME = 'cuda_favorites.json'

PROJECT_EXTENSION = '.cuda-proj'

CATS = ('projects', 'files', 'folders')

#: sort modes: 'star', 'name', 'path', 'state', 'attach', 'manual'
#: ('attach' = the attachment column: 'Attached projects' on the
#: Files/Folders tabs, 'Attachments' on the Projects tab)
SORT_COLS = ('star', 'name', 'path', 'state', 'attach')
DEFAULT_SORT = 'star'

# listbox_ex column indexes
#: quick-select label (1-9, a-z, aa-zz, ... - see quick_label), the
#: FIRST column, like the shortcuts of the cuda_favorites plugin
COL_KEY = 0
COL_CHECK = 1
COL_STAR = 2
COL_NAME = 3
COL_PATH = 4
#: attachment column: 'Attached projects' on the Files and Folders
#: tabs (the projects the item is attached to), 'Attachments' on the
#: Projects tab (the files/folders attached TO that project) - one
#: column index, the text differs per tab (see _header_text /
#: fill_list)
COL_PROJECTS = 5
COL_STATE = 6
NCOLS = 7

#: column separator inside listbox_ex item/header strings: a char that
#: never appears in a name or path
LIST_SEP = chr(1)

#: unscaled px widths of the check, star, name and state columns; the
#: Path column is AUTO-STRETCHED by the listbox itself (column size 0
#: = "share the free width" in listbox_ex) - it always fills the list
#: up to its right edge, also when the window is resized
KEY_W = 40
CHECK_W = 38
STAR_W = 46
NAME_W = 210
STATE_W = 54
#: attachment column (see COL_PROJECTS) width
PROJECTS_W = 150
PATH_MIN_W = 160
#: unscaled listbox row height (the header row has the same height)
ITEM_H = 30
#: unscaled size of the checkbox square
CHECK_SZ = 15
#: the engine (TATListbox behind listbox_ex) indents its whole
#: column grid - the header captions and the engine-drawn cells -
#: by this many px right of the control's left edge (the IndentLeft
#: property, default 4; not DPI-scaled, and not exposed via
#: listbox_proc, so the default is mirrored here). The owner-drawn
#: rows and the divider hit-tests must use the same origin, or every
#: row column sits 4 px left of the header's (and the header loses
#: its last 4 px to the right edge - the engine's grid overflows by
#: the indent)
LIST_INDENT = 4
#: half-width (px) of the header-divider drag zone
RESIZE_GRIP = 6
#: min widths (px, unscaled) of the columns while dragging
COL_MIN_W = (18, 24, 26, 60, 100, 40, 34)
#: a drag must move this many px before it counts as a resize (below
#: that, releasing over the header is a normal header click = sorting)
DRAG_THRESHOLD = 4
#: seconds the quick-select type-ahead waits for the next key before
#: the typed label buffer starts over (see form_on_key)
KEY_SELECT_PAUSE = 1.0

#: LCL mouse-cursor id of the horizontal-split (left-right arrows)
#: shape: shown over the draggable header dividers and while dragging
#: them (crHSplit in the LCL; crDefault = 0)
CURSOR_HSPLIT = -14

#: how much (0.0..1.0) the INACTIVE tab's face is dimmed: its color is
#: blended this far from the theme's button face (the ACTIVE tab's
#: color, which stays as it was) toward the window background - the
#: inactive tabs read as "switched off" on light AND dark themes.
#: 0.0 would keep the inactive face = active face (no visible
#: difference), 1.0 would melt the inactive tabs into the window
#: background completely (only their border and dim caption remain).
#: When the background sits NEARER to the face than TAB_INACTIVE_DIST,
#: the blend extrapolates PAST it (see _dim_toward) so the dimming
#: step is always visible
TAB_INACTIVE_MIX = 0.5
#: guaranteed minimum distance (of 255) between the ACTIVE and the
#: INACTIVE tab faces, whatever the theme paints - see _dim_toward
TAB_INACTIVE_DIST = 22
#: how much (0.0..1.0) the INACTIVE tab's caption is dimmed: the theme
#: font color blended toward the tab's dimmed face, so the TEXT reads
#: as inactive too, not only the face
TAB_INACTIVE_FONT_MIX = 0.45
#: unscaled FIXED width of every tab (browser/VS Code tabs
#: hug their caption, they do NOT stretch over the whole list width -
#: full-width tabs looked bloated); when the window is narrowed below
#: the space needed by three fixed tabs, they shrink to equal thirds
#: (see _layout_tabs), never overflowing into the button column
TAB_W = 120
#: gap between the tabs (unscaled px): tight, so the three
#: rectangles read as one tab strip, not as separate boxes
TAB_GAP = 2

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
    # intentional silence: old CudaText builds without this API -
    # unscaled layout
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
    # intentional silence: old CudaText builds without the theme-dict
    # API - the default (untinted) colors are used
    except Exception:
        d = None

    def col(key):
        try:
            v = d.get(key)
            if isinstance(v, dict) and v.get('color') is not None:
                return int(v['color'])
        # intentional silence: a theme without the key / with a bad
        # color value - the documented fallback color is the point
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
        # flat buttons: the colors the tree panel's (x) cancel tile
        # uses for its passive look (the same face as CudaText's own
        # buttons - it turns red only while a search runs)
        'btn_bg': col('ButtonBgPassive'),
        'btn_font': col('ButtonFont'),
    }


def _theme_dict():
    """Raw UI-theme dict (or None)."""
    try:
        return app_proc(PROC_THEME_UI_DICT_GET, '')
    # intentional silence: old CudaText builds without the theme-dict
    # API - the callers use their fallback colors
    except Exception:
        return None


def _theme_color(d, key, fallback):
    """One int color from a theme dict (TColor/BGR int), with a
    fallback for themes that do not define the key."""
    try:
        v = d.get(key)
        if isinstance(v, dict) and v.get('color') is not None:
            return int(v['color'])
    # intentional silence: a theme without the key / with a bad color
    # value - the documented fallback color is the point here
    except Exception:
        pass
    return fallback


def _mix_color(c1, c2, t):
    """Blend two BGR int colors: t=0.0 returns c1, t=1.0 returns c2.
    Used to dim the inactive tabs (their face toward the window
    background, their caption toward the dimmed face) - a blend dims
    correctly on light AND dark themes, a fixed gray would not."""
    def part(a, b, shift):
        a = (int(a) >> shift) & 0xFF
        b = (int(b) >> shift) & 0xFF
        # clamped to the 0..255 color cube: the extrapolation of
        # _dim_toward (t > 1) can push a channel past the edge - amy
        # ships ButtonBgPassive #200020 with TabBg #130013, and
        # t = 2.54 turns the R/B channels into 32 - 33 = -1, so the
        # blend returned the int -1 (= clDither, an INVALID TColor)
        # and the engine painted the inactive tabs WHITE with a
        # black caption box. A blend that leaves the cube stops at
        # its edge (black/white) - a valid color always comes out.
        return max(0, min(255, int(round(a + (b - a) * t))))
    return ((part(c1, c2, 16) << 16) |
            (part(c1, c2, 8) << 8) |
            part(c1, c2, 0))


def _avg_dist(c1, c2):
    """Average per-channel distance of two BGR int colors (0..255)."""
    return (abs((c1 & 0xFF) - (c2 & 0xFF)) +
            abs(((c1 >> 8) & 0xFF) - ((c2 >> 8) & 0xFF)) +
            abs(((c1 >> 16) & 0xFF) - ((c2 >> 16) & 0xFF))) / 3.0


def _dim_toward(c, bg, t, min_dist):
    """Face color of an INACTIVE tab: the face 'c' dimmed toward the
    window background 'bg' by 't' - but never ending up closer than
    'min_dist' (of 255) to 'c': some themes paint the window barely
    different from the button face (measured: face #EEEEEE vs window
    #E1E2E3), and a blend that stops halfway between such twins is
    invisible. When bg sits nearer than min_dist, the blend
    EXTRAPOLATES past it (t grows above 1.0), so the inactive tab
    always keeps a clearly visible step below the active face (it may
    end up a bit darker than the window - like a recessed tab).
    When face and background are the SAME color, the target becomes
    black instead (less light = inactive, on light and dark themes
    alike)."""
    d = _avg_dist(c, bg)
    if d < 1.0:
        return _mix_color(c, 0x000000, min_dist / 255.0)
    t = max(t, min(min_dist / d, 2.8))
    return _mix_color(c, bg, t)


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


def quick_label(i):
    """Quick-select label of the row at VIEW index i (0-based):
    '1'..'9', then 'a'..'z', then 'aa'..'zz', then 'aaa'..'zzz' and so
    on - the digits exist only as single chars, the longer labels are
    pure letter sequences (bijective base-26: a..z = 1..26, aa = 27,
    ab = 28, ..., zz = 702, aaa = 703). 9 + 26 + 676 + 17576 + ...
    labels are far more rows than any favorites list ever has; a label
    longer than its column gets an ellipsis when drawn, but typing it
    still selects the row."""
    if i < 0:
        return ''
    if i < 9:
        return str(i + 1)
    k = i - 8          # bijective value: i=9 -> 1 -> 'a'
    s = ''
    while k > 0:
        k -= 1
        s = chr(ord('a') + k % 26) + s
        k //= 26
    return s


def _read_json(filename):
    """Parsed JSON of the file, or None when it is missing/unreadable.
    A CORRUPT file raises ValueError: the caller must decide how to
    keep the user's data instead of silently starting empty."""
    try:
        with open(filename, encoding='utf8') as fin:
            return json.load(fin)
    # intentional silence: missing/unreadable file - nothing to read
    except OSError:
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
    # intentional silence: Project Manager not loaded (standalone use
    # of the Favorites module) - no current project then
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
        # the store file, shared with the Project Manager's own state
        # (recent_projects, opened_folder); our data sits in its
        # 'favorites' namespace
        self.path_own = store_path()
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
                # 'projects': the project favorites this file/folder
                # favorite is ATTACHED to - a list of unique non-empty
                # paths (absent = attached to nothing)
                projs = it.get('projects')
                if isinstance(projs, list):
                    pseen = set()
                    pout = []
                    for p in projs:
                        if isinstance(p, str) and p:
                            pk = _norm_path(p)
                            if pk and pk not in pseen:
                                pseen.add(pk)
                                pout.append(p)
                    if pout:
                        it['projects'] = pout
                    else:
                        it.pop('projects', None)
                else:
                    it.pop('projects', None)
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
        # a leftover data file of the pre-store versions is imported
        # into the store's 'favorites' namespace once, then removed
        migrate_legacy_favorites()
        # the namespace read; a corrupt STORE is renamed to
        # '<name>.corrupt' by store_read itself (kept for manual
        # repair) and an empty store starts - the read_only guard of
        # the old separate-file flow is gone with the file
        own = store_read().get('favorites')
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
        try:
            ext = _read_json(self.path_ext)
        # intentional silence: the foreign plugin's file is corrupt -
        # the one-way import is just skipped then
        except ValueError:
            ext = None
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
        """Write our namespace of the store file - and nothing else:
        the sync with cuda_favorites.json is one-way (import-only),
        that file is never written here, and the store's other
        namespaces (recent_projects, opened_folder) are not touched."""
        out = {cat: self.data[cat] for cat in CATS}
        out['_ui'] = self.ui
        out['_deleted'] = self.deleted
        return store_update(favorites=out)

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
            # deleting a PROJECT favorite drops every attachment to it
            # (no dangling links in the 'Attached projects' column)
            if cat == 'projects':
                self.detach_all_of_project(path)
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
        if cat == 'projects':
            for p in removed:
                self.detach_all_of_project(p)
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

    #------------- attachments (files/folders <-> projects)

    def attach(self, cat, index, proj_path):
        """Attach a file/folder favorite to a project favorite (its
        path). Returns True when the attachment was added, False when
        it existed already."""
        if not (0 <= index < len(self.data[cat])):
            return False
        it = self.data[cat][index]
        projs = it.get('projects')
        if not isinstance(projs, list):
            projs = []
        key = _norm_path(proj_path)
        if any(_norm_path(p) == key for p in projs):
            return False
        projs.append(str(proj_path))
        it['projects'] = projs
        self.save()
        return True

    def detach(self, cat, index, proj_path):
        """Remove one attachment. Returns True when it was removed."""
        if not (0 <= index < len(self.data[cat])):
            return False
        it = self.data[cat][index]
        projs = it.get('projects')
        if not isinstance(projs, list):
            return False
        key = _norm_path(proj_path)
        new = [p for p in projs if _norm_path(p) != key]
        if len(new) == len(projs):
            return False
        if new:
            it['projects'] = new
        else:
            it.pop('projects', None)
        self.save()
        return True

    def attached_projects(self, cat, index):
        """Paths of the project favorites the item is attached to."""
        if 0 <= index < len(self.data[cat]):
            return list(self.data[cat][index].get('projects') or [])
        return []

    def attached_items_of(self, proj_path):
        """[(cat, item)] of every file/folder favorite attached to the
        project favorite at proj_path (data order)."""
        key = _norm_path(proj_path)
        res = []
        for cat in ('files', 'folders'):
            for it in self.data[cat]:
                if any(_norm_path(p) == key
                       for p in it.get('projects') or []):
                    res.append((cat, it))
        return res

    def detach_all_of_project(self, proj_path):
        """Drop every attachment to proj_path (the project favorite was
        deleted - the links must not dangle). Pure data op, NO save:
        the callers save. Returns the number of dropped links."""
        key = _norm_path(proj_path)
        n = 0
        for cat in ('files', 'folders'):
            for it in self.data[cat]:
                projs = it.get('projects')
                if not isinstance(projs, list):
                    continue
                new = [p for p in projs if _norm_path(p) != key]
                if len(new) != len(projs):
                    n += len(projs) - len(new)
                    if new:
                        it['projects'] = new
                    else:
                        it.pop('projects', None)
        return n

    def project_display_name(self, proj_path):
        """Caption for an attached project path: the project FAVORITE's
        name when it is still in the list, else the file base name
        (minus the .cuda-proj extension) - a dangling link still shows
        something readable."""
        key = _norm_path(proj_path)
        for it in self.data['projects']:
            if _norm_path(it['path']) == key:
                return it['name']
        base = _base_name(proj_path)
        if base.lower().endswith(PROJECT_EXTENSION):
            base = base[:-len(PROJECT_EXTENSION)]
        return base or proj_path

    def attached_projects_text(self, it):
        """Column text of a file/folder favorite: the joined display
        names of the project favorites it is attached to (the
        'Attached projects' column of the Files/Folders tabs)."""
        return ', '.join(self.project_display_name(p)
                         for p in it.get('projects') or [])

    def attached_items_text(self, it):
        """Column text of a project favorite: the joined names of the
        file/folder favorites attached to it (the 'Attachments'
        column of the Projects tab)."""
        return ', '.join(a_it['name']
                         for _cat, a_it in self.attached_items_of(it['path']))

    #------------- views (sorted/filtered)

    def view(self, cat, sort_col, sort_desc, flt=''):
        """List of (data_index, item) for one category, filtered and
        sorted. 'manual' sort_col = the stored (custom) order."""
        flt = (flt or '').strip().lower()
        pairs = [(i, it) for (i, it) in enumerate(self.data[cat])]

        if flt:
            # the filter also searches the attachment column's text:
            # attached projects on the Files/Folders tabs, attachments
            # on the Projects tab
            if cat == 'projects':
                pairs = [(i, it) for (i, it) in pairs
                         if flt in it['name'].lower() or flt in it['path'].lower()
                         or flt in self.attached_items_text(it).lower()]
            else:
                pairs = [(i, it) for (i, it) in pairs
                         if flt in it['name'].lower() or flt in it['path'].lower()
                         or flt in self.attached_projects_text(it).lower()]

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
        elif sort_col == 'attach':
            # the attachment column: by its joined text, ties by name
            # then path (the reverse index of the Projects tab is
            # computed per row - the favorites lists are small)
            pairs.sort(key=lambda p: (p[1]['name'].lower(), p[1]['path'].lower()))
            if cat == 'projects':
                pairs.sort(key=lambda p: self.attached_items_text(p[1]).lower(),
                           reverse=sort_desc)
            else:
                pairs.sort(key=lambda p: self.attached_projects_text(p[1]).lower(),
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
    except Exception:
        log_error('Favorites: startup load failed')


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
        #: Editor object of the filter field ('editor_edit' control;
        #: None before the window is built)
        self._filter_ed = None
        self.view = []          # [(data_index, item)] for the active tab
        self.rows = []          # what is shown: dicts with cached dead
        self.selected_data = -1 # data index to re-select after refill
        self._closing = False
        #: the index of the tab currently under the mouse, or -1
        #: (on_mouse_enter / on_mouse_exit of the colorpanel tabs;
        #: _update_tabs highlights it with the theme hover color)
        self._hover_btn = -1
        self._sb_w = 20         # scrollbar width, for the Path stretch
        self._scale = 1.0
        #: (ui, edit) font-size overrides the window was BUILT with
        #: (see _font_opts); show_favorites() compares them with the
        #: current options and rebuilds the window on a change
        self.font_opts = (None, None)
        #: 'ui_font_size' the window was built with (int, 0 = unset);
        #: _draw_text_cell paints the list text with it
        self._ui_fs = 0
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
        #: quick-select type-ahead: the typed label letters and the
        #: time of the last key press (see form_on_key)
        self._key_buf = ''
        self._key_time = 0.0
        #: column widths [key, check, star, name, path, attachment
        #: (COL_PROJECTS - 'Attached projects' / 'Attachments'),
        #: state], scaled px (path = 0: auto-stretched by the listbox)
        self.cols = [KEY_W, CHECK_W, STAR_W, NAME_W, 400, PROJECTS_W,
                     STATE_W]

    def _ui_get(self, key, default):
        # plain dict access, cannot fail - no error handling wanted
        val = self.core.ui.get(key, default)
        return default if val is None else val

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
        # the two font-size overrides of the plugin options (see
        # utils.option_font_size): with a size set, every row that
        # shows that font GROWS with it (font_row_h) - buttons and
        # tabs from BH, the filter row and the edit heights from EH;
        # None = the engine defaults. Stored for the list text
        # (_draw_text_cell) and the rebuild check (show_favorites)
        self.font_opts = _font_opts()
        ui_fs, edit_fs = self.font_opts
        self._ui_fs = ui_fs or 0
        BH = font_row_h(BH, ui_fs)
        if edit_fs:
            EH = font_row_h(EH, edit_fs)
        else:
            # unset: the filter field still renders at the USER'S
            # EDITOR FONT (see utils.editor_default_metrics) - grow
            # the row to the real metrics, the text is never eaten
            cell_h, _pitch = editor_default_metrics()
            if cell_h:
                EH = max(EH, int(cell_h / scale) + 8)
        BTN_H = BH

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
            int(KEY_W * scale),
            int(CHECK_W * scale),
            int(STAR_W * scale),
            int(NAME_W * scale),
            0,   # Path: auto-stretched by the listbox (column size 0)
            int(PROJECTS_W * scale),
            int(STATE_W * scale),
        ]
        try:
            self._sb_w = int(app_proc(PROC_GET_GUI_HEIGHT, 'scrollbar') or 20) or 20
        # intentional silence: old CudaText builds - the constant
        # scrollbar width
        except Exception:
            self._sb_w = 20

        # tab strip geometry (unscaled): the strip top ty; the strip is
        # BTN_H tall (the tabs sit directly on the list below, flush -
        # no bar strip anymore), the list starts right below at
        # ty + BTN_H
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
            'cap': _('Batch actions') + ' ▼',
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

        # 'editor_edit' = the ATSynEdit-based one-line edit: the engine
        # themes it entirely (OptThemed + EditorApplyTheme), so it
        # follows the UI theme on dark themes too - the OS 'edit' had
        # an OS-drawn border, too light on dark themes (issue #6633).
        # Editors are 'act':True by the engine (issue #3361), so the
        # on_change event fires without the prop. The text is read and
        # written through the Editor object ('val' does not serve
        # editor controls).
        n = self._ctl('editor_edit', 'f_filter', {
            'x': BTN_X, 'y': 8, 'w': bx - 2 * BTN_X, 'h': EH,
            'tab_order': 0,
            'texthint': _('Filter by name or path...'),
            'hint': _('Quickly filter the favorites list.\rUp/Down cycles the list, Esc closes the window'),
            'a_l': ('', '['),
            'sp_l': BTN_X,
            **MAIN_RIGHT,
            'on_change': self.on_filter_change,
        })
        try:
            self._filter_ed = Editor(
                dlg_proc(self.h, DLG_CTL_HANDLE, index=n) or 0)
        # intentional silence: the control was not created (an old
        # CudaText build) - the filter field is optional
        except Exception:
            self._filter_ed = None
        # 'edit_font_size' override: the engine default of dialog
        # editor controls is the EDITOR font, oversized in this small
        # field for big editor fonts - the exact case the option
        # solves (the row height already grew with EH above)
        if edit_fs:
            dlg_proc(self.h, DLG_CTL_PROP_SET, name='f_filter',
                     prop={'font_size': edit_fs})

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

        #---- the tab strip (three 'colorpanel' tabs) ----
        # Neither the OS-drawn 'tabs' control nor the themed button_ex
        # could show WHICH tab is active by color: the tabs control
        # cannot be themed at all, and the flat button always paints
        # one and the same theme face (its 'color' prop is ignored by
        # its Paint). So every tab is a 'colorpanel' (TATPanelColor):
        # a plain rectangle that paints its own face color, its own
        # centered caption and its own 1px border (the ex0/ex3 props
        # set the border width/color) - and _update_tabs recolors
        # face / caption / border per state:
        # - ACTIVE tab: the theme's flat-button face color (exactly the
        #   color the button_ex tabs used to be painted with) + the
        #   theme's button font and border - the active look did not
        #   change, only the bar below the strip is gone;
        # - INACTIVE tabs: DIMMED - their face keeps a clearly visible
        #   step below the active face on every theme (blended toward
        #   the window background, extrapolating past it when the theme
        #   paints the two nearly equal - see _dim_toward), and their
        #   caption blends toward that dimmed face (TAB_INACTIVE_MIX),
        #   so they read as "switched off";
        # - the hovered tab (any of the three) takes the theme's button
        #   hover color, like the hovered flat buttons do.
        # The three tabs form a LEFT-ALIGNED strip of EQUAL FIXED widths
        # (TAB_W, browser/VS Code style - they hug their captions and do
        # NOT stretch over the whole list width), joined with a small
        # TAB_GAP, sitting directly on the list below (flush).
        # Only the LEFT edges are anchored (chained to each other) -
        # anchoring a tab's right edge to the next tab's left edge
        # AND back would be a circular anchor pair, which the engine
        # cannot resolve (it used to break the strip after a window
        # resize: unequal widths, the first tab sliding over the
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
                'h': BTN_H,
                'tab_order': 1,
                # 'act': True is REQUIRED: the colorpanel reports its
                # click through on_change (OnClick -> DoOnChange), and
                # DoOnChange drops the event for controls not marked
                # 'act' (buttons get it by default, colorpanels don't)
                'act': True,
                'hint': _('Show {} favorites').format(tab_caps[i]),
                'on_change': (lambda id_dlg, id_ctl, data='', info='', _i=i:
                              self.on_tab_button(_i)),
                # hover highlight of the tab face (the recolor itself
                # is done by _update_tabs - see on_tab_hover)
                'on_mouse_enter': (lambda id_dlg, id_ctl, data='', info='',
                                   _i=i: self.on_tab_hover(_i, True)),
                'on_mouse_exit': (lambda id_dlg, id_ctl, data='', info='',
                                  _i=i: self.on_tab_hover(_i, False)),
                # the visible 1px border (ex0 = width, ex3 = color);
                # all three colors are corrected per tab by _update_tabs
                'ex0': 1,
                'color': 0xE0E0E0,
                'font_color': 0x000000,
                'ex3': 0x808080,
            }
            if i == 0:
                # sp_l is REQUIRED: setting a_l re-glues the edge to the
                # form's left side + sp_l (sp_l defaults to 0)
                prop['a_l'] = ('', '[')
                prop['sp_l'] = BTN_X
            else:
                prop['a_l'] = (tab_names[i - 1], ']')
                prop['sp_l'] = TAB_GAP
            self._ctl('colorpanel', tab_names[i], prop)

        #---- the list ----
        # Created AFTER the tab strip: its top anchor references 'tab0',
        # and the dialog engine resolves a control name in an anchor only
        # if that control already exists. The top anchor chains the main
        # column FLUSH at every DPI / UI scale: filter -> (BTN_GAP) ->
        # tabs -> (0) -> list - the tabs sit directly ON the list's top
        # edge, like the tabs of a browser sitting on the page. The old
        # layout placed the list at a fixed offset from the filter's
        # bottom - at a non-100% scale the scaled offsets and the
        # controls' real heights drifted apart and a wide empty gap
        # opened between the tabs and the list.
        self._ctl('listbox_ex', 'list', {
            'x': BTN_X, 'y': ty + BTN_H,   # sane pre-anchor position
            'w': bx - 2 * BTN_X,
            'h': h - (ty + BTN_H) - 8 - 22 - BTN_GAP,
            'tab_order': 2,
            'act': True,          # fire on_change when the selection moves
            'border': True,
            'a_l': ('', '['),
            'sp_l': BTN_X,
            **MAIN_RIGHT,
            'a_t': ('tab0', ']'),
            'sp_t': 0,           # flush: no gap below the tab strip
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
            # the row height grows with 'ui_font_size': the rows
            # draw their text at that size (see _draw_text_cell)
            listbox_proc(self.h_list, LISTBOX_SET_ITEM_H,
                         index=int(font_row_h(ITEM_H, ui_fs) * scale))
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
            # NOTE the filter field ('editor_edit') is NOT recolored
            # here: the engine themes editor controls entirely, they
            # follow every UI theme change on their own
            if colors['label_font'] is not None:
                dlg_proc(self.h, DLG_CTL_PROP_SET, name='lbl_status',
                         prop={'font_color': colors['label_font']})
        except Exception:
            log_error('Favorites: cannot apply the UI theme '
                      'to the window')
        # re-color the tab strip from the new theme (the active tab's
        # face, the dimming of the inactive ones) - the theme can be
        # switched while the window is open
        self._update_tabs()
        # repaint the owner-drawn rows with the new colors
        if self.h_list and self.rows:
            self.fill_list()
        # 'ui_font_size' pass (also re-applied here after a theme
        # change): the buttons, the tabs and the list. The list gets
        # font_color TOGETHER with font_size: a listbox with a set
        # font stops using its themed font (ThemedFont), so the
        # header text color must come from the theme dict explicitly
        if self.font_opts[0]:
            for _b in ('btn_open', 'btn_add', 'btn_add_opened',
                       'btn_checked', 'btn_up', 'btn_down', 'btn_del',
                       'btn_close', 'tab0', 'tab1', 'tab2'):
                try:
                    dlg_proc(self.h, DLG_CTL_PROP_SET, name=_b,
                             prop={'font_size': self.font_opts[0]})
                except Exception:
                    log_error('Favorites: cannot set a button font')
            try:
                dlg_proc(self.h, DLG_CTL_PROP_SET, name='list',
                         prop={'font_size': self.font_opts[0],
                               'font_color': self._theme['font']})
            except Exception:
                log_error('Favorites: cannot set the list font')

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
        # drop a stale hover flag (the window was closed while the mouse
        # was over a tab): without this, the next show could paint a
        # hover-colored tab with no mouse over it
        self._hover_btn = -1
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
            log_error('Favorites: cannot focus the filter field')

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
        tabs are left-anchored to each other, so only their widths
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
            log_error('Favorites: cannot measure the tab strip '
                      'for the layout')
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
                log_error('Favorites: cannot set the tab widths')

    def _live_list_w(self):
        """Live width of the list control ('w' is a live prop), or 0."""
        try:
            props = dlg_proc(self.h, DLG_CTL_PROP_GET, name='list') or {}
            return int(props.get('w', 0) or 0)
        except Exception:
            log_error('Favorites: cannot read the list width')
            return 0

    def _live_list_h(self):
        """Live height of the list control, or 0."""
        try:
            props = dlg_proc(self.h, DLG_CTL_PROP_GET, name='list') or {}
            return int(props.get('h', 0) or 0)
        except Exception:
            log_error('Favorites: cannot read the list height')
            return 0

    def _live_cols(self):
        """Effective widths of the columns, in REAL pixels. Mirrors
        TATListbox.UpdateColumnWidths: sizes >0 are fixed, a size of 0
        is auto-stretched (it gets the free width of the listbox,
        minus the vertical scrollbar when the rows overflow it).
        self.cols keeps the SET sizes; this returns what is actually
        painted - used for hit-testing (checkbox/star cells) and for
        the header divider positions. Every tab has the SAME columns:
        the 6th is 'Attached projects' / 'Attachments' (COL_PROJECTS),
        only its text differs per tab."""
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
        # intentional silence: API returned nothing usable (old build,
        # empty list) - the default row height below takes over
        except (TypeError, ValueError):
            pass
        return int(ITEM_H * self._scale)

    def _col_bounds(self):
        """[(x0, x1), ...] of the NCOLS columns, from the LIVE widths
        (the Path column is auto-stretched, so its real width is only
        known from the control size). The grid starts at LIST_INDENT:
        the engine indents its own column painting (the header
        included) by that much, and the owner-drawn rows must match
        it or every row column sits left of the header's."""
        res = []
        x = LIST_INDENT
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
        # intentional silence: no usable value (old build) - row 0
        except (TypeError, ValueError):
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
        """The themed header line: '# | All | ★ | Name | Path |
        Attached projects | State' with the sort arrow on the active
        column. The '#' cell holds the quick-select labels (typing one
        selects the row), the 'All' cell toggles all checkboxes when
        clicked. The 6th cell is 'Attached projects' on the Files and
        Folders tabs, 'Attachments' on the Projects tab; both sort by
        the 'attach' mode."""
        cells = [
            '#',
            _('All'),
            '★',
            self._sort_caption('name', _('Name')),
            self._sort_caption('path', _('Path')),
        ]
        if self.cat() == 'projects':
            cells.append(self._sort_caption('attach', _('Attachments')))
        else:
            cells.append(self._sort_caption('attach', _('Attached projects')))
        cells.append(self._sort_caption('state', _('State')))
        return LIST_SEP.join(cells)

    def _apply_columns(self):
        """Push the column widths + the header line into the listbox
        (this repaints the themed header). Every tab gets the SAME
        columns - the 6th is 'Attached projects' / 'Attachments'
        (COL_PROJECTS), only its text differs per tab."""
        if not self.h_list:
            return
        cols = list(self.cols)
        try:
            listbox_proc(self.h_list, LISTBOX_SET_COLUMNS,
                         text=[int(c) for c in cols])
            listbox_proc(self.h_list, LISTBOX_SET_HEADER,
                         text=self._header_text())
        except Exception:
            log_error('Favorites: cannot set up the list columns')

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
            log_error('Favorites: cannot scroll the list')

    def fill_list(self, keep_selection=True):
        if not self.h or not self.h_list:
            return
        cat = self.cat()
        self.view = self.core.view(cat, self.sort_col, self.sort_desc, self.filter)

        # what is shown: the dead state is computed ONCE here, not on
        # every repaint (os.path.exists on each paint would be slow);
        # so is the attachment column's text (name lookups per paint
        # would be slow too). The quick-select label follows the VIEW
        # position (index in the filtered, sorted list), like the
        # shortcuts of the cuda_favorites plugin - and every refill
        # starts the type-ahead from scratch
        self._key_buf = ''
        self.rows = [{
            'data_idx': data_idx,
            'path': it['path'],
            'name': it['name'],
            'star': bool(it['star']),
            'dead': not os.path.exists(it['path']),
            'checked': _norm_path(it['path']) in self.checked,
            'key': quick_label(i),
            # the attachment column's text: attached projects on the
            # Files/Folders tabs, attachments on the Projects tab
            'projects': (self.core.attached_items_text(it)
                         if cat == 'projects'
                         else self.core.attached_projects_text(it)),
        } for i, (data_idx, it) in enumerate(self.view)]

        # items: the visible text is painted by on_draw_item, the item
        # text only carries the data (name + path, sep-separated)
        try:
            listbox_proc(self.h_list, LISTBOX_DELETE_ALL)
            for r in self.rows:
                listbox_proc(self.h_list, LISTBOX_ADD, index=-1,
                             text=LIST_SEP.join((r['name'], r['path'])))
        except Exception:
            log_error('Favorites: cannot refill the list')
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
                log_error('Favorites: cannot select the row')

        self._update_tabs()
        self.update_status()

    def _update_tabs(self):
        """Recolor the tab strip for the current self.tab and hover
        state. All three tabs always have the same position and size -
        the ONLY differences are the COLORS of their faces, captions
        and borders ('colorpanel' controls, so every color is ours):
        - the ACTIVE tab keeps the theme's flat-button face color -
          exactly the color the old button_ex tabs were painted with,
          so the active look did not change - with the theme's button
          font color and 1px border;
        - the INACTIVE tabs are DIMMED: their face is blended toward
          the window background (TAB_INACTIVE_MIX) and their caption
          toward that dimmed face (TAB_INACTIVE_FONT_MIX), so they
          read as "switched off" on light and dark themes alike;
        - the tab under the mouse (active or not) takes the theme's
          button HOVER color and border, like a hovered flat button.
        Called on build, on every fill_list (tab switch), on hover
        enter/exit and on every theme change."""
        d = _theme_dict()
        # the theme's flat-button face color (what button_ex paints
        # itself with); falls back to the window color
        face = _theme_color(d, 'ButtonBgPassive', None)
        form_bg = _theme_color(d, 'TabBg', None)
        if face is None:
            face = form_bg if form_bg is not None else 0xE0E0E0
        if form_bg is None:
            form_bg = face
        font_c = _theme_color(d, 'ButtonFont', None)
        if font_c is None:
            font_c = _theme_color(d, 'EdTextFont', 0x000000)
        border = _theme_color(d, 'ButtonBorderPassive', 0x808080)
        # the exact colors a hovered flat button is painted with (fed
        # from the same 'ButtonBgOver'/'ButtonBorderOver' theme keys),
        # so the hovered tab matches the hovered buttons
        hover_bg = _theme_color(d, 'ButtonBgOver', None)
        if hover_bg is None:
            hover_bg = _mix_color(face, font_c, 0.12)
        hover_border = _theme_color(d, 'ButtonBorderOver', border)
        # the dimmed inactive look: the face keeps a clearly visible
        # step below the active face on EVERY theme (see _dim_toward),
        # the caption blends toward that dimmed face
        inact_face = _dim_toward(face, form_bg, TAB_INACTIVE_MIX,
                                 TAB_INACTIVE_DIST)
        inact_font = _mix_color(font_c, inact_face, TAB_INACTIVE_FONT_MIX)
        for i in range(3):
            if i == self._hover_btn:
                col, fnt, brd = hover_bg, font_c, hover_border
            elif i == self.tab:
                col, fnt, brd = face, font_c, border
            else:
                col, fnt, brd = inact_face, inact_font, border
            try:
                dlg_proc(self.h, DLG_CTL_PROP_SET,
                         name='tab{}'.format(i),
                         prop={'color': col,
                               'font_color': fnt,
                               'ex3': brd})
            # intentional silence: the 'ex3' border prop is missing in
            # older CudaText builds - a plain tab look is fine there
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
        elif self.sort_col == 'attach':
            sort_info = (_('sort: attachments') if cat == 'projects'
                         else _('sort: attached projects')) + \
                (' ↓' if self.sort_desc else ' ↑')
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
            log_error('Favorites: cannot update the status line')

        # enable/disable the buttons which need a selected row
        has_sel = self.selected_row() is not None
        for name in ('btn_open', 'btn_up', 'btn_down', 'btn_del'):
            try:
                dlg_proc(self.h, DLG_CTL_PROP_SET, name=name, prop={'en': has_sel})
            except Exception:
                log_error('Favorites: cannot update the buttons')

    #------------- selection helpers

    def _sel_index(self):
        try:
            return int(listbox_proc(self.h_list, LISTBOX_GET_SEL))
        # intentional silence: no selection (the API gives no int
        # then) - 'none selected'
        except (TypeError, ValueError):
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
                        color=color, style=style,
                        size=self._ui_fs or -1)
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
            log_error('Favorites: cannot draw a list cell')

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
        # intentional silence: malformed paint-event data - nothing
        # to draw
        except (TypeError, ValueError, IndexError):
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
            log_error('Favorites: cannot draw the row background')
            return

        bounds = self._col_bounds()

        # quick-select label (col 0): the centered label, dimmed like
        # the path - typing it selects the row (see form_on_key)
        key_col = fg
        if not selected and th.get('path') is not None:
            key_col = th['path']
        self._draw_text_cell(cnv, bounds[COL_KEY][0], y0,
                             self.cols[COL_KEY], row_h,
                             row['key'], key_col, centered=True)

        # checkbox (col 1)
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
                log_error('Favorites: cannot draw the checkbox')

        # star glyph (col 1) - the UTF-8 glyphs, no image files
        self._draw_text_cell(cnv, bounds[COL_STAR][0], y0,
                             self.cols[COL_STAR], row_h,
                             '★' if row['star'] else '☆', fg, centered=True)

        # name (col 3) - dead paths painted in the theme's accent color
        name_col = th['dead'] if row['dead'] else fg
        self._draw_text_cell(cnv, bounds[COL_NAME][0], y0,
                             self.cols[COL_NAME], row_h,
                             row['name'], name_col)

        # path (col 4): refill its background first so a long name
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
            log_error('Favorites: cannot draw the path cell')
        path_col = fg
        if not selected and th.get('path') is not None:
            path_col = th['path']
        path_w = bounds[COL_PATH][1] - bounds[COL_PATH][0]
        self._draw_text_cell(cnv, bounds[COL_PATH][0], y0,
                             path_w, row_h,
                             row['path'], path_col)

        # attachment column (col 5: 'Attached projects' on the
        # Files/Folders tabs, 'Attachments' on the Projects tab):
        # refill its background first (covers a long path bleeding
        # into it), then the joined names, dimmed like the path
        try:
            canvas_proc(cnv, CANVAS_SET_BRUSH, color=bg,
                        style=BRUSH_SOLID)
            canvas_proc(cnv, CANVAS_RECT_FILL,
                        x=bounds[COL_PROJECTS][0], y=y0,
                        x2=bounds[COL_PROJECTS][1], y2=y1)
        except Exception:
            log_error('Favorites: cannot draw the projects cell')
        self._draw_text_cell(cnv, bounds[COL_PROJECTS][0], y0,
                             self.cols[COL_PROJECTS], row_h,
                             row['projects'], path_col)

        # state (always the LAST column): fill to the right edge
        # (covers a long path), then the centered '?' for dead paths
        lc_state = len(bounds) - 1
        try:
            canvas_proc(cnv, CANVAS_SET_BRUSH, color=bg, style=BRUSH_SOLID)
            canvas_proc(cnv, CANVAS_RECT_FILL, x=bounds[lc_state][0], y=y0,
                        x2=x1, y2=y1)
        except Exception:
            log_error('Favorites: cannot draw the state cell')
        if row['dead']:
            self._draw_text_cell(cnv, bounds[lc_state][0], y0,
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
            log_error('Favorites: cannot draw the column lines')

    #------------- events: filter, tabs, selection

    def on_filter_change(self, id_dlg, id_ctl, data='', info=''):
        # the filter field is an 'editor_edit': read via the Editor
        # object, the dialog 'val' prop does not serve editor controls
        try:
            self.filter = (self._filter_ed.get_text_all()
                           if self._filter_ed else '')
        # intentional silence: hot path (every keystroke) with a
        # documented '' contract - the editor is dead only while the
        # dialog is closing
        except Exception:
            self.filter = ''
        self.fill_list(keep_selection=False)

    def switch_tab(self, delta, target=None):
        """Switch the tab strip from the KEYBOARD: delta -1/+1 cycles
        the tabs with wrap-around (Ctrl+PgUp/PgDn, Ctrl+Left/Right),
        `target` jumps to that tab index (Alt+P/F/O). Falls through to
        on_tab_button, which does the recolor + refill."""
        if target is None:
            target = (self.tab + delta) % len(CATS)
        if 0 <= target < len(CATS):
            self.on_tab_button(target)

    def _filter_focused(self):
        """True while the filter editor control owns the keyboard focus
        (the Del key edits the filter text then - see form_on_key)."""
        try:
            return bool(self._filter_ed
                        and self._filter_ed.get_prop(PROP_FOCUSED))
        # intentional silence: a dead editor object (the window is
        # closing) - 'not focused'
        except Exception:
            return False

    def on_tab_button(self, index):
        """A tab was clicked: switch the category (the strip is
        recolored by _update_tabs via fill_list - the newly active tab
        takes the bright button face, the others are dimmed; sizes
        never change)."""
        if index == self.tab:
            return
        self.tab = index
        self.selected_data = -1
        self.fill_list(keep_selection=False)

    def on_tab_hover(self, index, entered):
        """The mouse entered/left one of the three colorpanel tabs
        (on_mouse_enter / on_mouse_exit of the control). While the
        mouse is over a tab, _update_tabs paints it with the theme's
        button hover color (the same color the hovered flat buttons
        get), so the strip feels as alive as the button column; the
        active/inactive dimming of the OTHER tabs is not touched."""
        if not self.h or self._closing:
            return
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
        # every tab has the same columns, so the live column index IS
        # the column constant (see COL_PROJECTS)
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
        pos = LIST_INDENT   # the engine's grid origin (see _col_bounds)
        cols = self._live_cols()
        for k in range(len(cols) - 1):
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
        # intentional silence: the 'cursor' prop is missing in older
        # builds - the normal cursor
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
        # divider; but the divider right of the auto-stretched Path
        # column (size 0) resizes the column AFTER it - Path must stay
        # 0 and simply absorbs whatever width is left, so the divider
        # still follows the mouse exactly (the attachment column).
        # Every tab has the same columns, so the live index is the
        # self.cols index
        live = self._live_cols()
        li = k + 1 if k == COL_PATH else k
        if not (0 <= li < len(live)):
            return
        self._col_drag = {
            'col': li,
            'x0': x,
            'orig_w': self.cols[li] if self.cols[li] > 0 else live[li],
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
        the header offers the Sort submenu, a click on a row selects it
        and offers the row commands plus the "Batch actions" and "Sort"
        submenus."""
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
                    log_error('Favorites: cannot select the row')

        row = None if on_header else self.selected_row()
        nchecked = sum(1 for r in self.rows if r['checked'])
        cat = self.cat()
        attachable = cat in ('files', 'folders')
        menu = menu_proc(0, MENU_CREATE)
        menu_proc(menu, MENU_CLEAR)

        def add(caption, action, enabled=True, parent=None):
            if '|' in caption:
                caption, hotkey = caption.split('|', maxsplit=1)
            else:
                hotkey = ''
            mid = menu_proc(parent if parent is not None else menu, MENU_ADD,
                            command=('module=cuda_project_man;cmd=_favorites_ctx;info={};'.format(action)
                                     if action else ''),
                            caption=caption,
                            hotkey=hotkey)
            if not enabled:
                menu_proc(mid, MENU_SET_ENABLED, command=False)
            return mid

        # the items of the 'Sort' submenu (this menu and the
        # header-click sorting stay in sync); the attachment column's
        # caption depends on the active tab
        sort_items = (
            (_('Sort: ★ first, then name'), 'sort_star'),
            (_('Sort: by name'), 'sort_name'),
            (_('Sort: by path'), 'sort_path'),
            (_('Sort: by attachments') if cat == 'projects'
             else _('Sort: by attached projects'), 'sort_attach'),
            (_('Sort: by state, dead last'), 'sort_state'),
            (_('Sort: manual order'), 'sort_manual'),
        )

        if on_header:
            # the sort fallback menu (for users who don't find the
            # header-click sorting): the Sort commands live in their
            # own submenu
            sub = menu_proc(menu, MENU_ADD, caption=_('Sort'))
            for cap, act in sort_items:
                add(cap, act, parent=sub)
            menu_proc(menu, MENU_ADD, caption='-')
            add((_('Uncheck all') if nchecked else _('Check all')),
                'uncheck_all' if nchecked else 'check_all',
                bool(self.rows))
            menu_proc(menu, MENU_SHOW, command='')
            return

        add(_('Open') + '|Enter', 'open', row is not None)
        add(_('Reveal in file manager'), 'reveal', row is not None)
        add(_('Copy path'), 'copy', row is not None)
        menu_proc(menu, MENU_ADD, caption='-')
        add((_('Unstar') if (row and row['star']) else _('Star')) + ' ★', 'star',
            row is not None)
        add(_('Rename...') + '|F2', 'rename', row is not None)
        add(_('Delete') + '|Del', 'delete', row is not None)

        # attachments: files/folders attach TO a project favorite,
        # a project favorite lists what is attached TO IT
        if attachable:
            menu_proc(menu, MENU_ADD, caption='-')
            add(_('Attach favorite to project...'), 'attach',
                row is not None and bool(self.core.data['projects']))
            add(_('Detach from project...'), 'detach',
                row is not None and bool(row and row['projects']))
        else:
            menu_proc(menu, MENU_ADD, caption='-')
            add(_('List attached files/folders...'), 'list_attached',
                row is not None)

        # commands on ALL checked favorites - in their own submenu,
        # named like the "Batch actions" button
        menu_proc(menu, MENU_ADD, caption='-')
        sub = menu_proc(menu, MENU_ADD,
                        caption=_('Batch actions ({}):').format(nchecked)
                        if nchecked else _('Batch actions'))
        ck_items = [
            (_('Open all checked'), 'ck_open'),
            (_('Star all checked'), 'ck_star'),
            (_('Unstar all checked'), 'ck_unstar'),
            (_('Copy paths of checked'), 'ck_copy'),
            (_('Rename checked...'), 'ck_rename'),
            (_('Move checked up'), 'ck_up'),
            (_('Move checked down'), 'ck_down'),
            (_('Delete checked...'), 'ck_delete'),
            (_('Uncheck all'), 'ck_uncheck'),
        ]
        if attachable:
            # bulk attach: check several files/folders, pick ONE
            # project, all checked items attach to it at once
            ck_items.insert(1, (_('Attach checked to project...'), 'ck_attach'))
        for caption, action in ck_items:
            add(caption, action, nchecked > 0, parent=sub)

        menu_proc(menu, MENU_ADD, caption='-')
        # sorting is also in the menu because column-header clicks are
        # not reported by all CudaText builds
        sub = menu_proc(menu, MENU_ADD, caption=_('Sort'))
        for cap, act in sort_items:
            add(cap, act, parent=sub)
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

        # Ctrl+PgUp/PgDn and Ctrl+Left/Right: switch the Projects /
        # Files / Folders tabs (cycling with wrap-around), from any
        # focused control - the same switching as clicking a tab
        if 'c' in mods and 'a' not in mods and 's' not in mods:
            if key in (VK_PAGEUP, VK_LEFT):
                self.switch_tab(-1)
                return False
            if key in (VK_PAGEDOWN, VK_RIGHT):
                self.switch_tab(+1)
                return False

        # Alt+P / Alt+F / Alt+O: jump straight to the Projects / Files
        # / Folders tab (VK letter codes are the UPPERCASE ords)
        if 'a' in mods and 'c' not in mods and 's' not in mods:
            for i, ch in enumerate((ord('P'), ord('F'), ord('O'))):
                if key == ch:
                    self.switch_tab(0, target=i)
                    return False

        # plain keys
        if 'a' not in mods and 'c' not in mods and 's' not in mods and 'm' not in mods:
            if key == VK_ENTER:
                self.open_selected()
                return False
            if key == VK_DELETE:
                # Del edits the FILTER while it is focused (deletes the
                # next letter, the editor's usual Del) and deletes the
                # SELECTED favorite otherwise
                if self._filter_focused():
                    return None
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
            # Up/Down work from ANY focused control (the filter field is
            # focused right after the window opens - see show()): the
            # keys cycle the favorites list / the filtered list, without
            # the user having to click the list first
            if key == VK_UP:
                self.move_selection(-1)
                return False
            if key == VK_DOWN:
                self.move_selection(+1)
                return False
            if key == VK_ESCAPE:
                # Esc always closes the window (the built-in Esc handling
                # does it when this handler does not block the key); the
                # old two-step behavior (first Esc clears the filter,
                # the second one closes) was dropped on request - the
                # filter has its (x)-less text hint and the user can
                # clear it with the mouse / Shift+Home-style editing
                return None

        # quick-select labels: a PLAIN digit or letter, typed while the
        # filter is not focused, builds the label of a row (1-9, a-z,
        # aa-zz, ... - see the '#' column) and selects it; a pause of
        # KEY_SELECT_PAUSE seconds starts the label from scratch
        if not mods and not self._filter_focused():
            ch = ''
            if ord('1') <= key <= ord('9'):
                ch = chr(key)
            elif ord('A') <= key <= ord('Z') or ord('a') <= key <= ord('z'):
                ch = chr(key).lower()
            if ch:
                self._select_by_typed(ch)
                return False

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
                log_error('Favorites: cannot focus the window')

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
            except Exception:
                log_error('Favorites: cannot open favorite: ' + str(p))
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

    def move_selection(self, delta):
        """Up/Down from ANY focused control (the filter field included -
        it has the focus right after the window opens): move the list
        selection by `delta` rows, WRAPPING at the ends (Down on the last
        row selects the first one, Up on the first row selects the last
        one) - the keys always cycle the favorites list / the filtered
        list, never leave it, and never need a click on the list first.
        The listbox's own key handling cannot do this: it only works
        while the LIST is focused."""
        if not self.h_list or not self.rows:
            return
        i = self._sel_index()
        if i < 0:
            i = 0
        else:
            i = (i + delta) % len(self.rows)
        self._select_row(i)

    def _select_row(self, i):
        """Select row i and scroll it into view (Up/Down cycling and
        the quick-select labels both come here). Scrolls ONLY when the
        row sits outside the visible window (a blind SET_TOP of every
        select would jump the view: the selected row would always land
        on the first visible line). The listbox's own key handling
        cannot do any of this: it only works while the LIST is
        focused."""
        if not self.h_list or not (0 <= i < len(self.rows)):
            return
        try:
            ih = self._item_h()
            h = self._live_list_h()
            try:
                top = int(listbox_proc(self.h_list, LISTBOX_GET_TOP))
            except (TypeError, ValueError):
                top = 0
            visible = max(1, h // ih) if (ih > 0 and h > 0) else 1
            if i < top:
                listbox_proc(self.h_list, LISTBOX_SET_TOP, index=i)
            elif i >= top + visible:
                listbox_proc(self.h_list, LISTBOX_SET_TOP,
                             index=i - visible + 1)
            listbox_proc(self.h_list, LISTBOX_SET_SEL, index=i)
            # 'act':True fires on_change -> list_on_change updates
            # self.selected_data and the status line; do it by hand too,
            # so the state is right even in builds where the programmatic
            # selection does not fire the event
            row = self.rows[i]
            self.selected_data = row['data_idx']
            self.update_status()
        except Exception:
            log_error('Favorites: cannot move the list selection')

    def _select_by_typed(self, ch):
        """One key of the quick-select type-ahead (see form_on_key):
        append the char to the typed label and select the row carrying
        that label (or the first row whose label CONTINUES it - so 'a'
        selects 'a', then typing on builds 'aa'). A pause longer than
        KEY_SELECT_PAUSE starts the label from scratch; a label that
        matches nothing resets it."""
        now = time.time()
        if now - self._key_time > KEY_SELECT_PAUSE:
            self._key_buf = ''
        self._key_time = now
        if ch.isdigit():
            buf = ch            # 1-9 exist only as single-char labels
        else:
            buf = self._key_buf + ch
        idx = -1
        for i, r in enumerate(self.rows):
            if r['key'] == buf:
                idx = i
                break
        if idx < 0:
            for i, r in enumerate(self.rows):
                if r['key'].startswith(buf):
                    idx = i
                    break
        if idx < 0:
            self._key_buf = ''
            msg_status(_('Favorites: no favorite "{}"').format(buf))
            return
        self._key_buf = '' if ch.isdigit() else buf
        self._select_row(idx)

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
            except Exception:
                log_error('Favorites: cannot reveal in file manager: '
                          + str(path))

        # deferred, like opening (see _open_paths)
        timer_proc(TIMER_START_ONE, do_reveal, OPEN_DELAY_MS)

    #------------- attachment actions (files/folders <-> projects)

    def _choose_project_favorite(self):
        """The project-favorite picker of 'Attach favorite to project':
        a filterable dlg_menu over the project favorites (name; the
        path too when several favorites share one name). Returns the
        chosen item dict, or None (cancelled / no project favorites)."""
        items = self.core.data['projects']
        if not items:
            msg_status(_('Favorites: no project favorites - add the project first'))
            return None
        names = [it['name'] for it in items]
        dup = {n for n in names if names.count(n) > 1}
        caps = [it['name'] if it['name'] not in dup
                else '{} — {}'.format(it['name'], it['path'])
                for it in items]
        res = dlg_menu(DMENU_LIST, caps,
                       caption=_('Attach to which project favorite?'))
        if res is None:
            return None
        return items[res]

    def attach_selected(self):
        """Context 'Attach favorite to project...': attach the selected
        file/folder favorite to a picked project favorite."""
        row = self.selected_row()
        if row is None:
            return
        proj = self._choose_project_favorite()
        if proj is None:
            return
        data_idx = self.core.find(self.cat(), row['path'])
        if data_idx < 0:
            return
        if self.core.attach(self.cat(), data_idx, proj['path']):
            msg_status(_('Favorites: attached to "{}"').format(proj['name']))
        else:
            msg_status(_('Favorites: already attached to "{}"').format(proj['name']))
        self.fill_list()

    def detach_selected(self):
        """Context 'Detach from project...': pick one of the item's
        attached projects and remove that link."""
        row = self.selected_row()
        if row is None:
            return
        data_idx = self.core.find(self.cat(), row['path'])
        if data_idx < 0:
            return
        projs = self.core.attached_projects(self.cat(), data_idx)
        if not projs:
            msg_status(_('Favorites: not attached to any project'))
            return
        caps = [self.core.project_display_name(p) for p in projs]
        res = dlg_menu(DMENU_LIST, caps, caption=_('Detach from which project?'))
        if res is None:
            return
        self.core.detach(self.cat(), data_idx, projs[res])
        msg_status(_('Favorites: detached'))
        self.fill_list()

    def list_attached_selected(self):
        """Context 'List attached files/folders...' (Projects tab):
        the filterable list of everything attached to the selected
        project favorite; picking an entry opens it."""
        row = self.selected_row()
        if row is None:
            return
        items = self.core.attached_items_of(row['path'])
        if not items:
            msg_status(_('Favorites: nothing attached to this project'))
            return
        caps = []
        for cat, it in items:
            prefix = _('file:') if cat == 'files' else _('folder:')
            dead = '' if os.path.exists(it['path']) else '?'
            caps.append('{} {}{} — {}'.format(prefix, dead, it['name'],
                                              it['path']))
        res = dlg_menu(DMENU_LIST, caps,
                       caption=_('Attached to: ') + row['name'])
        if res is None:
            return
        self._open_paths([items[res][1]['path']])

    def op_checked_attach(self):
        """Bulk attach: every CHECKED file/folder favorite of the tab
        attaches to ONE picked project favorite at once (already
        attached items are skipped, one save)."""
        rows = self._checked_or_status()
        if not rows:
            return
        if self.cat() not in ('files', 'folders'):
            return
        proj = self._choose_project_favorite()
        if proj is None:
            return
        cat = self.cat()
        done = 0
        for r in rows:
            if 0 <= r['data_idx'] < len(self.core.data[cat]):
                if self.core.attach(cat, r['data_idx'], proj['path']):
                    done += 1
        if done:
            msg_status(_('Favorites: attached {} item(s) to "{}"').format(
                done, proj['name']))
        else:
            msg_status(_('Favorites: already attached to "{}"').format(proj['name']))
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
        """The 'Batch actions' button: a menu with the commands that
        run on ALL checked favorites (the same commands the right-click
        menu's 'Batch actions' submenu has)."""
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

        cap = _('Batch actions ({}):').format(nchecked) if nchecked else _('Batch actions:')
        add(cap, None, False)
        ck_items = [
            (_('Open all checked'), 'ck_open'),
            (_('Star all checked'), 'ck_star'),
            (_('Unstar all checked'), 'ck_unstar'),
            (_('Copy paths of checked'), 'ck_copy'),
            (_('Rename checked...'), 'ck_rename'),
            (_('Move checked up'), 'ck_up'),
            (_('Move checked down'), 'ck_down'),
            (_('Delete checked...'), 'ck_delete'),
            (_('Uncheck all'), 'ck_uncheck'),
        ]
        if self.cat() in ('files', 'folders'):
            ck_items.insert(1, (_('Attach checked to project...'), 'ck_attach'))
        for caption, action in ck_items:
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
        if action in ('sort_star', 'sort_name', 'sort_path', 'sort_state',
                      'sort_attach'):
            self.sort_by_column({'sort_star': COL_STAR,
                                 'sort_name': COL_NAME,
                                 'sort_path': COL_PATH,
                                 'sort_attach': COL_PROJECTS,
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
        elif action == 'ck_attach':
            self.op_checked_attach()
        elif action == 'attach':
            self.attach_selected()
        elif action == 'detach':
            self.detach_selected()
        elif action == 'list_attached':
            self.list_attached_selected()
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
                COL_PROJECTS: 'attach', COL_STATE: 'state'}.get(col)
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


def _font_opts():
    """(ui, edit) font-size overrides from the plugin options file:
    validated ints, or None for unset (the engine defaults). Read
    fresh at every window build/show: the options dialog saves the
    file on OK, so the next open always sees the current values."""
    ui = edit = None
    try:
        path = os.path.join(app_path(APP_DIR_SETTINGS),
                            'cuda_project_man.json')
        with open(path, encoding='utf8') as f:
            data = json.load(f)
        if isinstance(data, dict):
            ui = option_font_size(data.get('ui_font_size'))
            edit = option_font_size(data.get('edit_font_size'))
    except Exception:
        # intentional silence: no options file yet / unreadable ->
        # the engine defaults
        pass
    return ui, edit


def _rebuild_form():
    """Free the hidden form and build a fresh one (a font-size option
    changed - every row/button height follows them). Only ever called
    from show_favorites(), never from a dialog callback or a timer -
    i.e. from the very context the form was first created in, so no
    callback can arrive for the freed dialog (that crash pattern is
    the reason close() only hides the form)."""
    global _form
    old = _form
    _form = None
    if old is not None and old.h:
        try:
            old.save_ui_state()
        except Exception:
            log_error('Favorites: cannot save the window state '
                      'on rebuild')
        try:
            dlg_proc(old.h, DLG_FREE)
        except Exception:
            log_error('Favorites: cannot free the old window')
    _form = FavoritesForm(get_core())


def show_favorites():
    """Open (or focus) the Favorites window. The one-way import from
    cuda_favorites.json runs on every open (the external file may have
    changed while the window was closed). A changed font-size option
    rebuilds the window first: the hidden form is reused only while
    the options stay the same, because every row height was computed
    from the fonts at build time."""
    global _form
    refresh_core()
    if _form is not None and _form.h and _form.font_opts != _font_opts():
        _rebuild_form()
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


def auto_add_project(path):
    """The 'Add all opened projects to favorites' option of the
    Project Manager: add the project unless it is already favorited
    (silent skip) or was deleted from favorites by the user (the
    tombstone wins until a manual re-add clears it). Returns True
    when the project was added."""
    core = get_core()
    if core.find('projects', path) >= 0:
        return False
    if core._is_deleted(path):
        return False
    return add_favorite('projects', path)


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


def find_favorites():
    """Command 'Find favorites...': the dlg_menu list dialog (the
    CudaText API dialog with its own FILTER field and close icon) over
    ALL favorites - projects, files, folders - every entry prefixed
    'project:' / 'file:' / 'folder:'; a favorite whose path is absent
    on disk gets '?' before its name. Enter opens the picked favorite
    (from a timer, like every open of this plugin - see _open_paths).
    Returns True when something was picked."""
    core = refresh_core()[0]
    caps = []
    entries = []   # (cat, path) in the same order
    for cat, prefix in (('projects', _('project:')),
                        ('files', _('file:')),
                        ('folders', _('folder:'))):
        for it in core.data[cat]:
            dead = '?' if not os.path.exists(it['path']) else ''
            caps.append('{} {}{} — {}'.format(prefix, dead, it['name'],
                                              it['path']))
            entries.append((cat, it['path']))
    if not caps:
        msg_status(_('Favorites: nothing saved yet'))
        return False
    res = dlg_menu(DMENU_LIST, caps, caption=_('Find favorites'))
    if res is None:
        return False
    path = entries[res][1]

    def do_open(tag=None):
        try:
            open_item(path)
        except Exception:
            log_error('Favorites: cannot open favorite: ' + str(path))

    timer_proc(TIMER_START_ONE, do_open, OPEN_DELAY_MS)
    return True
