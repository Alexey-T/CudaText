import os
import sys
import re
import collections
import json
import stat
import copy
import string
import time
from fnmatch import fnmatch
from pathlib import Path, PurePosixPath
from .projman_glob import *

from cudatext import *
from cudatext_keys import *
import cudatext_cmd

from cudax_lib import get_translation
_   = get_translation(__file__)  # i18n

IS_WIN = os.name == 'nt'
IS_MAC = sys.platform == 'darwin'

S_CTRL_API = 'm' if IS_MAC else 'c'
S_CTRL_NAME = 'Meta' if IS_MAC else 'Ctrl'

ctypes = None
if IS_WIN:
    try:
        import ctypes
    except (ImportError, ModuleNotFoundError):
        pass

PROJECT_EXTENSION = ".cuda-proj"
PROJECT_DIALOG_FILTER = _("CudaText projects") + "|*" + PROJECT_EXTENSION
PROJECT_UNSAVED_NAME = _("(No project)")
NODE_PROJECT, NODE_DIR, NODE_FILE, NODE_BAD = range(4)
OS_SUFFIX = app_proc(PROC_GET_OS_SUFFIX, '')
DEF_SES = 'default'+OS_SUFFIX+'.cuda-session'

global_project_info = {}
sort_order = 'ext'

# Project file format version written by this plugin. Files without a
# "version" key are the old format (version 1) and are upgraded on open.
PROJECT_FORMAT_VERSION = 2


def load_project_file(path):
    """Load and validate a .cuda-proj file.
    Returns (data, error): exactly one of them is non-None.
    `data` is the parsed JSON dict; `error` is a string 'kind[:detail]'
    with kind one of: io, empty, json, type, nodes, sessions."""
    try:
        with open(path, encoding='utf8') as fin:
            text = fin.read()
    except OSError as ex:
        return None, 'io: %s' % ex

    if not text.strip():
        return None, 'empty'

    try:
        data = json.loads(text)
    except ValueError as ex:
        return None, 'json: %s' % ex

    if not isinstance(data, dict):
        return None, 'type'

    nodes = data.get('nodes')
    if nodes is not None:
        if not isinstance(nodes, list) or not all(isinstance(n, str) for n in nodes):
            return None, 'nodes'

    sessions = data.get('sessions')
    if sessions is not None and not isinstance(sessions, dict):
        return None, 'sessions'

    return data, None


def normalize_project_data(data):
    """Ensure a project dict has all standard keys with correct types.
    Only adds missing keys / fixes types; never removes user keys.
    - last_session: name of the session that was used last; filled when a
      session is switched to, and loaded when the project is opened.
    Does not touch the 'version' key (managed by the save/open code)."""
    if not isinstance(data, dict):
        data = {}
    if not isinstance(data.get('nodes'), list):
        data['nodes'] = []
    else:
        data['nodes'] = [n for n in data['nodes'] if isinstance(n, str)]
    if not isinstance(data.get('vars'), list):
        data['vars'] = []
    if not isinstance(data.get('mainfile'), str):
        data['mainfile'] = ''
    if not isinstance(data.get('unfold'), list):
        data['unfold'] = []
    if not isinstance(data.get('sessions'), dict):
        data['sessions'] = {}
    if not isinstance(data.get('last_session'), str):
        data['last_session'] = ''
    return data


def make_project_backup(path):
    """Copy a project file to a backup next to it, before an automatic
    upgrade rewrites the file. Backup name: '<file>.bak'; if that name is
    already taken (an earlier upgrade, or the user's own file), a
    timestamped name '<file>.<yymmdd_hhmmss>.bak' is used — an existing
    backup is never overwritten.
    Returns the backup path, or None when the copy failed (read-only
    folder, locked file, ...)."""
    from datetime import datetime
    import shutil
    p = Path(path)
    bak = p.with_name(p.name + '.bak')
    if bak.exists():
        stamp = datetime.now().strftime('%y%m%d_%H%M%S')
        bak = p.with_name('%s.%s.bak' % (p.name, stamp))
        if bak.exists():
            return None
    try:
        shutil.copy2(str(p), str(bak))
    except OSError:
        return None
    return bak


def is_session_name(s):
    allowed = string.ascii_letters+string.digits+'., ()-+_$%='
    for ch in s:
        if not ch in allowed:
            return False
    return True


def write_json_file(path, data):
    """Write `data` as pretty JSON to `path`, atomically: the JSON goes to a
    temporary file next to the target first, which then replaces the target
    in one step. A crash / power loss / full disk in the middle of the write
    can never leave a truncated file behind — for a .cuda-proj file that
    would lose ALL project data (nodes + every embedded session). Either
    the file is replaced completely, or the previous complete file stays
    intact and the error propagates to the caller (callers report OSError
    to the user)."""
    tmp = str(path) + '.tmp' + str(os.getpid())
    try:
        with open(tmp, 'w', encoding='utf8') as fout:
            json.dump(data, fout, indent=2)
        os.replace(tmp, str(path))
    finally:
        # the replace didn't happen -> remove the temp file
        if os.path.exists(tmp):
            try:
                os.unlink(tmp)
            except OSError:
                pass

def _file_open(fn, options=''):
    gr = ed.get_prop(PROP_INDEX_GROUP)
    #print('Opening file in group %d'%gr)
    file_open(fn, group=gr, options=options)

# don't allow spaces and punctuation in file extension
EXT_CH = string.ascii_letters + string.digits + '.-_'

def _file_ext(fn):
    _, s = os.path.splitext(fn)
    for ch in s:
        if not ch in EXT_CH:
            return ''
    return s

def _file_editor(fn):
    for h in ed_handles():
        e = Editor(h)
        if e.get_prop(PROP_FN, '*')==fn:
            return e
    return None

# https://stackoverflow.com/questions/377017/test-if-executable-exists-in-python
def which(program):
    def is_exe(fpath):
        return os.path.isfile(fpath) and os.access(fpath, os.X_OK)

    fpath, fname = os.path.split(program)
    if fpath:
        if is_exe(program):
            return program
    else:
        for path in os.environ["PATH"].split(os.pathsep):
            exe_file = os.path.join(path, program)
            if is_exe(exe_file):
                return exe_file

    return None

def project_variables():
    """
    gives dict with "project variables", which is ok for using from other plugins,
    e.g. ExtTools.
    add to names {} or $() if you want.
    1) predefined var ProjMainFile (defined by right-click menu in ProjMan)
    2) predefined var ProjDir (dir of .cuda-proj file)
    3) other vars are defined by user in Proj Properties dialog.
    """
    res = collections.OrderedDict()
    data = global_project_info
    res['ProjDir'] = os.path.dirname(data.get('filename', ''))

    fn = data.get('mainfile', '')
    res['ProjMainFile'] = fn
    res['ProjMainFileNameOnly'] = os.path.basename(fn)
    res['ProjMainFileNameNoExt'] = '.'.join(os.path.basename(fn).split('.')[0:-1])

    data = global_project_info.get('vars', [])
    for item in data:
        s1, s2 = item.split('=', maxsplit=1)
        res[s1] = s2
    return res

NodeInfo = collections.namedtuple("NodeInfo", "caption image")

_homedir = os.path.expanduser('~')
_appdir = os.path.dirname(app_path(APP_DIR_SETTINGS))

def collapse_macros(proj_dir, fn):
    if proj_dir and (fn+os.sep).startswith(proj_dir+os.sep):
        fn = fn.replace(proj_dir, '{ProjDir}', 1)
    if (fn+os.sep).startswith(_appdir+os.sep):
        fn = fn.replace(_appdir, '{AppDir}', 1)
    fn = fn.replace(os.sep, '/') if IS_WIN else fn
    return fn

def expand_macros(proj_dir, s):
    if proj_dir:
        s = s.replace('{ProjDir}', proj_dir, 1)
    s = s.replace('{AppDir}', _appdir, 1)
    return os.path.normpath(s)

def collapse_filename(fn):
    if (fn+'/').startswith(_homedir+'/'):
        fn = fn.replace(_homedir, '~', 1)
    return fn

def nice_filename(path):
    return os.path.basename(path) + ' ('+ collapse_filename(os.path.dirname(path)) + ')'


def is_simple_listed(name, masks):
    s = name.lower()
    for mask in masks.split(' '):
        if s.endswith(mask):
            return True
    return False

def is_mask_listed(s, masks):
    for mask in masks.split(';'):
        r = fnmatch(s, mask)
        #print("fnmatch('%s', '%s') = %d"%(s, mask, int(r)))
        if r:
            return True
    return False

# only Py 3.5 supports os.stat(s).st_file_attributes
# so this is to support Py 3.4
def is_hidden_win32(s):
    if not ctypes:
        return False
    try:
        attrs = ctypes.windll.kernel32.GetFileAttributesW(s)
        assert attrs != -1
        res = bool(attrs & 2)
    except (AttributeError, AssertionError):
        res = False
    return res

def is_hidden(s):
    if IS_WIN:
        if s=='':
            return False
        if s.endswith(':\\'):
            return False

        return is_hidden_win32(s)
        #try:
        #    return bool(os.stat(s).st_file_attributes & stat.FILE_ATTRIBUTE_HIDDEN)
        #except Exception:
        #    return True

    else:
        return os.path.basename(s).startswith('.')

def is_win_root(s):
    return IS_WIN and s.endswith(':\\')

def is_locked(s):
    if IS_WIN:
        if s.endswith(':\\'):
            return False
        return is_hidden_win32(s)
    else:
        return not os.access(s, os.R_OK)


def _toolbar_add_btn(h_bar, hint, icon=-1, command=''):
    toolbar_proc(h_bar, TOOLBAR_ADD_ITEM)
    cnt = toolbar_proc(h_bar, TOOLBAR_GET_COUNT)
    h_btn = toolbar_proc(h_bar, TOOLBAR_GET_BUTTON_HANDLE, index=cnt-1)
    if hint=='-':
        button_proc(h_btn, BTN_SET_KIND, BTNKIND_SEP_HORZ)
    else:
        button_proc(h_btn, BTN_SET_KIND, BTNKIND_ICON_ONLY)
        button_proc(h_btn, BTN_SET_HINT, hint)
        button_proc(h_btn, BTN_SET_IMAGEINDEX, icon)
        button_proc(h_btn, BTN_SET_DATA1, command)


class Command:
    goto_history = []
    cur_dir = ''
    project_file_path = None
    # guard for on_state(APPSTATE_SESSION_SET): True while the plugin
    # itself changes the app session (see Command._set_session)
    _session_set_by_plugin = False

    # mouse movement (in px) after mouse-down, which starts a drag gesture
    # in the project tree (same value as LCL's default drag threshold)
    TREE_DRAG_THRESHOLD = 5
    # hovering a FOLDED folder node for this many seconds during an inner
    # drag&drop (non-Windows mouse tracking) unfolds it, like OS file managers
    TREE_UNFOLD_HOVER_SEC = 0.7

    title ="Project" # No _() here, the translation is offered in "translation template.ini".
    menuitems = (
        # item_caption, item_parent, item_types, item_action, item_hotkey
        (_("Create new empty project..."), "proj", [None, NODE_PROJECT, NODE_DIR, NODE_FILE, NODE_BAD], "cuda_project_man.action_new_project", ""),
        (_("Create new project from open tabs..."), "proj", [None, NODE_PROJECT, NODE_DIR, NODE_FILE, NODE_BAD], "cuda_project_man.action_save_session_as_project", ""),
        (_("Open project..."), "proj", [None, NODE_PROJECT, NODE_DIR, NODE_FILE, NODE_BAD], "cuda_project_man.action_open_project", ""),
        (_("Recent projects"), "proj", [None, NODE_PROJECT, NODE_DIR, NODE_FILE, NODE_BAD], "collect_recent_projects", ""),
        ("-", "proj", [None, NODE_PROJECT, NODE_DIR, NODE_FILE, NODE_BAD], "", ""),
        (_("Save project"), "proj", [None, NODE_PROJECT, NODE_DIR, NODE_FILE, NODE_BAD], "cuda_project_man.action_save_project", ""),
        (_("Save project as..."), "proj", [None, NODE_PROJECT, NODE_DIR, NODE_FILE, NODE_BAD], "cuda_project_man.action_save_project_as", ""),
        (_("Close project"), "proj", [None, NODE_PROJECT, NODE_DIR, NODE_FILE, NODE_BAD], "cuda_project_man.action_close_project", ""),
        (_("Open project folder"), "proj", [None, NODE_PROJECT, NODE_DIR, NODE_FILE, NODE_BAD], "cuda_project_man.action_open_project_folder", ""),
        ("-", "proj", [None, NODE_PROJECT, NODE_DIR, NODE_FILE, NODE_BAD], "", ""),
        (_("Project properties..."), "proj", [None, NODE_PROJECT, NODE_DIR, NODE_FILE, NODE_BAD], "cuda_project_man.action_project_properties", ""),

        (_("Add folder..."), "nodes", [None, NODE_PROJECT, NODE_DIR, NODE_FILE, NODE_BAD], "cuda_project_man.action_add_folder", ""),
        (_("Add file..."), "nodes", [None, NODE_PROJECT, NODE_DIR, NODE_FILE, NODE_BAD], "cuda_project_man.action_add_file", ""),
        (_("Add current file"), "nodes", [None, NODE_PROJECT, NODE_DIR, NODE_FILE, NODE_BAD], "cuda_project_man.add_current_file", ""),
        (_("Add all opened files"), "nodes", [None, NODE_PROJECT, NODE_DIR, NODE_FILE, NODE_BAD], "cuda_project_man.add_opened_files", ""),
        (_("Add folder of current file"), "nodes", [None, NODE_PROJECT, NODE_DIR, NODE_FILE, NODE_BAD], "cuda_project_man.add_current_dir", ""),
        ("-", "nodes", [None, NODE_PROJECT, NODE_DIR, NODE_FILE, NODE_BAD], "", ""),
        (_("Stop tracking this folder/file"), "nodes", [None, NODE_PROJECT, NODE_DIR, NODE_FILE, NODE_BAD], "cuda_project_man.action_remove_node", ""),
        (_("Clean up missing paths..."), "nodes", [None, NODE_PROJECT, NODE_DIR, NODE_FILE, NODE_BAD], "cuda_project_man.action_remove_deleted_nodes", ""),

        (_("New file..."), "dir", [NODE_DIR], "cuda_project_man.action_new_file", S_CTRL_NAME + "+N"),
        (_("New folder..."), "dir", [NODE_DIR], "cuda_project_man.action_new_directory", "F7"),
        ("-", "dir", [NODE_DIR], "", ""),
        (_("Cut"), "dir", [NODE_DIR], "cuda_project_man.action_cut", S_CTRL_NAME + "+X"),
        (_("Copy"), "dir", [NODE_DIR], "cuda_project_man.action_copy", S_CTRL_NAME + "+C"),
        (_("Paste"), "dir", [NODE_DIR], "cuda_project_man.action_paste", S_CTRL_NAME + "+V"),
        ("-", "dir", [NODE_DIR], "", ""),
        (_("Copy path"), "dir", [NODE_DIR], "cuda_project_man.action_copy_path", ""),
        (_("Copy relative path") , "dir", [NODE_DIR], "cuda_project_man.action_copy_relative_path", ""),
        ("-", "dir", [NODE_DIR], "", ""),
        (_("Rename..."), "dir", [NODE_DIR], "cuda_project_man.action_rename", "F2"),
        (_("Duplicate..."), "dir", [NODE_DIR], "cuda_project_man.action_duplicate", ""),
        (_("Delete"), "dir", [NODE_DIR], "cuda_project_man.action_delete_directory", "Del"),
        ("-", "dir", [NODE_DIR], "", ""),
        (_("Find in folder..."), "dir", [NODE_DIR], "cuda_project_man.action_find_in_directory", ""),
        ("-", "dir", [NODE_DIR], "", ""),
        (_("Reveal in file manager"), "dir", [NODE_DIR], "cuda_project_man.action_focus_in_fileman", ""),
        (_("Properties..."), "dir", [NODE_DIR], "cuda_project_man.action_get_properties", "Alt+Enter"),

        (_("Open in default application"), "file", [NODE_FILE], "cuda_project_man.action_open_def", ""),
        (_("Set as main file"), "file", [NODE_FILE], "cuda_project_man.action_set_as_main_file", ""),
        ("-", "file", [NODE_FILE], "", ""),
        (_("Cut"), "file", [NODE_FILE], "cuda_project_man.action_cut", S_CTRL_NAME + "+X"),
        (_("Copy"), "file", [NODE_FILE], "cuda_project_man.action_copy", S_CTRL_NAME + "+C"),
        (_("Paste"), "file", [NODE_FILE], "cuda_project_man.action_paste", S_CTRL_NAME + "+V"),
        ("-", "file", [NODE_FILE], "", ""),
        (_("Copy path"), "file", [NODE_FILE], "cuda_project_man.action_copy_path", ""),
        (_("Copy relative path"), "file", [NODE_FILE], "cuda_project_man.action_copy_relative_path", ""),
        ("-", "file", [NODE_FILE], "", ""),
        (_("Rename..."), "file", [NODE_FILE], "cuda_project_man.action_rename", "F2"),
        (_("Duplicate..."), "file", [NODE_FILE], "cuda_project_man.action_duplicate", ""),
        (_("Delete"), "file", [NODE_FILE], "cuda_project_man.action_delete_file", "Del"),
        ("-", "file", [NODE_FILE], "", ""),
        (_("Reveal in file manager"), "file", [NODE_FILE], "cuda_project_man.action_focus_in_fileman", ""),
        (_("Properties..."), "file", [NODE_FILE], "cuda_project_man.action_get_properties", "Alt+Enter"),

        ("-"   , "", [None, NODE_PROJECT, NODE_DIR, NODE_FILE, NODE_BAD], "", ""),
        (_("Refresh"), "", [None, NODE_PROJECT, NODE_DIR, NODE_FILE, NODE_BAD], "cuda_project_man.action_refresh", "F5"),
        (_("Show current file in project tree"), "", [None, NODE_PROJECT, NODE_DIR, NODE_FILE, NODE_BAD], "cuda_project_man.sync_to_ed", ""),
        ("-"   , "", [None, NODE_PROJECT, NODE_DIR, NODE_FILE, NODE_BAD], "", ""),
        (_("Go to file...")  , "", [None, NODE_PROJECT, NODE_DIR, NODE_FILE, NODE_BAD], "cuda_project_man.action_go_to_file", ""),
        (_("Go to main file"), "", [None, NODE_PROJECT, NODE_DIR, NODE_FILE, NODE_BAD], "cuda_project_man.goto_main", ""),
        (_("Open main file"), "", [None, NODE_PROJECT, NODE_DIR, NODE_FILE, NODE_BAD], "cuda_project_man.open_main", ""),
        ("-"   , "", [None, NODE_PROJECT, NODE_DIR, NODE_FILE, NODE_BAD], "", ""),
        (_("Project sessions"), "", [None, NODE_PROJECT, NODE_DIR, NODE_FILE, NODE_BAD], "sessions_submenu", ""),
        ("-"   , "", [None, NODE_PROJECT, NODE_DIR, NODE_FILE, NODE_BAD], "", ""),
        (_("Config..."), "", [None, NODE_PROJECT, NODE_DIR, NODE_FILE, NODE_BAD], "cuda_project_man.action_config", ""),
    )

    options = {
        "recent_projects": [],
        "no_files": "",
        "no_dirs": ".git;.svn",
        "no_hidden": True,
        "toolbar": True,
        "preview": True,
        "d_click": False,
        "goto_open": False,
        "sort_order": "ext",
        "always_sync": False,
        "on_start": False,
        "on_start_activate": False,
        "icon_theme": "vscode_16x16",
        "toolbar_theme": "default_16x16",
    }
    # Default options used as the base when loading from disk: any key
    # missing from the loaded file is filled in from here, so settings
    # stay fresh after adding/removing options in code.

    tree = None
    tree_index = None
    h_dlg = None
    h_menu = None
    h_menu_cfg = None

    action_cut_activate = False

    def __init__(self):
        # state of the drag&drop in the project tree (tree_mouse_down/move/up):
        # _tree_drag_press: (x, y, item_handle) of the mouse-down, or None
        # _tree_drag_mode: None | 'inner' ('inner' = inner drag&drop tracked by
        #   mouse events; only used on OS-es without the OLE drag-out)
        # _tree_drop_target: item handle currently highlighted as drop target
        # _tree_drag_sel: selection to restore when a drag is cancelled
        # _tree_drag_bounds: (w, h) of the tree control, cached at drag start
        # _tree_hover_item/_tree_hover_time: for the unfold-on-hover feature
        # _tree_suppress_click: a drag gesture happened, don't open the file
        #   on the following OnClick (LCL fires OnClick even after a mouse
        #   drag inside a control, and it fires BEFORE OnMouseUp)
        # _tree_drag_out_item/_tree_drag_out_path: the node dragged out of the
        #   tree with CudaText's OLE drag ('drag_files'); on Windows the drop
        #   back onto this panel is reported to panel_on_drop_files with
        #   self_drag=True (fired after the OLE drag returned; when the drop
        #   was not accepted by any OLE target, the release position over the
        #   panel is reported the same way), which completes the inner
        #   drag&drop
        self._tree_drag_press = None
        self._tree_drag_mode = None
        self._tree_drop_target = None
        self._tree_drag_sel = None
        self._tree_drag_bounds = None
        self._tree_hover_item = None
        self._tree_hover_time = 0.0
        self._tree_suppress_click = False
        self._tree_drag_out_item = None
        self._tree_drag_out_path = ''

        settings_dir = Path(app_path(APP_DIR_SETTINGS))
        self.options_filename = settings_dir / "cuda_project_man.json"
        # Start from a fresh copy of the defaults, then merge the loaded
        # file on top. This ensures newly added options appear even in old
        # config files, while preserving the user's existing values.
        self.options = copy.deepcopy(self.options)
        if self.options_filename.exists():
            try:
                with self.options_filename.open(encoding='utf8') as fin:
                    loaded = json.load(fin)
                self.options.update(loaded)
                if "recent_projects" in self.options:
                    self.options["recent_projects"] = [expand_macros('', fn) for fn in self.options["recent_projects"]]
            except Exception:
                pass

        self.new_project() # reset to the "no project" state


    def init_form_main(self):

        show_toolbar = self.options.get("toolbar", True)
        toolbar_theme = self.options.get("toolbar_theme", "default_16x16")

        self.h_dlg = dlg_proc(0, DLG_CREATE)

        dlg_proc(self.h_dlg, DLG_PROP_SET, {
            'keypreview': True,
            'on_key_down': self.form_key_down,
            # files dropped from the OS file manager onto this panel are
            # added to the project (instead of being opened in the editor,
            # which is what CudaText does with drops outside of this panel);
            # on Windows the event also gets the drop-back of a node dragged
            # out of the tree (self_drag), which is the inner drag&drop
            'on_drop_files': self.panel_on_drop_files,
            } )

        n = dlg_proc(self.h_dlg, DLG_CTL_ADD, prop='toolbar')
        dlg_proc(self.h_dlg, DLG_CTL_PROP_SET, index=n, prop={
            'name':'bar',
            'a_r':('',']'), #anchor to top: l,r,t
            'vis': show_toolbar,
            'h': 24,
            'autosize': True,
            } )

        self.h_bar = dlg_proc(self.h_dlg, DLG_CTL_HANDLE, index=n)
        self.toolbar_imglist = toolbar_proc(self.h_bar, TOOLBAR_GET_IMAGELIST)
        self.set_imagelist_size(toolbar_theme, self.toolbar_imglist)

        dirname = os.path.join(app_path(APP_DIR_DATA), 'projtoolbaricons', toolbar_theme)
        icon_recents = imagelist_proc(self.toolbar_imglist, IMAGELIST_ADD, value = os.path.join(dirname, 'recents.png'))
        icon_open = imagelist_proc(self.toolbar_imglist, IMAGELIST_ADD, value = os.path.join(dirname, 'open.png'))
        icon_save = imagelist_proc(self.toolbar_imglist, IMAGELIST_ADD, value = os.path.join(dirname, 'save.png'))
        icon_add_file = imagelist_proc(self.toolbar_imglist, IMAGELIST_ADD, value = os.path.join(dirname, 'add-file.png'))
        icon_add_dir = imagelist_proc(self.toolbar_imglist, IMAGELIST_ADD, value = os.path.join(dirname, 'add-dir.png'))
        # icon_del = imagelist_proc(self.toolbar_imglist, IMAGELIST_ADD, value = os.path.join(dirname, 'del.png'))
        icon_cfg = imagelist_proc(self.toolbar_imglist, IMAGELIST_ADD, value = os.path.join(dirname, 'cfg.png'))
        icon_sync = imagelist_proc(self.toolbar_imglist, IMAGELIST_ADD, value = os.path.join(dirname, 'sync.png'))

        toolbar_proc(self.h_bar, TOOLBAR_THEME)
        _toolbar_add_btn(self.h_bar, hint=_('Recent projects'), icon=icon_recents, command='cuda_project_man.menu_recents' )
        _toolbar_add_btn(self.h_bar, hint=_('Open project'), icon=icon_open, command='cuda_project_man.action_open_project' )
        _toolbar_add_btn(self.h_bar, hint=_('Save project'), icon=icon_save, command='cuda_project_man.action_save_project' )
        _toolbar_add_btn(self.h_bar, hint='-' )
        _toolbar_add_btn(self.h_bar, hint=_('Add folder'), icon=icon_add_dir, command='cuda_project_man.action_add_folder' )
        _toolbar_add_btn(self.h_bar, hint=_('Add file'), icon=icon_add_file, command='cuda_project_man.action_add_file' )
        # _toolbar_add_btn(self.h_bar, hint=_('Remove node'), icon=icon_del, command='cuda_project_man.action_remove_node' )
        _toolbar_add_btn(self.h_bar, hint='-' )
        _toolbar_add_btn(self.h_bar, hint=_('Project sessions'), icon=icon_cfg, command='cuda_project_man.menu_cfg')
        _toolbar_add_btn(self.h_bar, hint=_('Show current file in project tree'), icon=icon_sync, command='cuda_project_man.sync_to_ed' )

        btn_count = toolbar_proc(self.h_bar, TOOLBAR_GET_COUNT)
        self.h_btn_sync = toolbar_proc(self.h_bar, TOOLBAR_GET_BUTTON_HANDLE, index=btn_count-1)
        button_proc(self.h_btn_sync, BTN_SET_VISIBLE, not self.options.get('always_sync', False))

        toolbar_proc(self.h_bar, TOOLBAR_SET_WRAP, index=True)
        toolbar_proc(self.h_bar, TOOLBAR_UPDATE)

        n = dlg_proc(self.h_dlg, DLG_CTL_ADD, prop='treeview')
        self.tree_index = n
        dlg_proc(self.h_dlg, DLG_CTL_PROP_SET, index=n, prop={
            'name':'tree',
            'a_t':('bar', ']'),
            'a_r':('',']'), #anchor to entire form
            'a_b':('',']'),
            'on_menu': 'cuda_project_man.tree_on_menu',
            'on_unfold': 'cuda_project_man.tree_on_unfold',
            'on_click': 'cuda_project_man.tree_on_click',
            'on_click_dbl': 'cuda_project_man.tree_on_click_dbl',
            'on_mouse_down': self.tree_mouse_down,
            'on_mouse_move': self.tree_mouse_move,
            'on_mouse_up': self.tree_mouse_up,
            } )

        self.tree = dlg_proc(self.h_dlg, DLG_CTL_HANDLE, index=n)
        self.tree_imglist = tree_proc(self.tree, TREE_GET_IMAGELIST)
        tree_proc(self.tree, TREE_PROP_SHOW_ROOT, text='0')
        tree_proc(self.tree, TREE_ITEM_DELETE, 0)

        self.icon_init()
        self.ICON_ALL = self.icon_get('_')
        self.ICON_DIR = self.icon_get('_dir')
        self.ICON_PROJ = self.icon_get('_proj')
        self.ICON_BAD = self.icon_get('_bad')
        self.ICON_ZIP = self.icon_get('_zip')
        self.ICON_BIN = self.icon_get('_bin')
        self.ICON_IMG = self.icon_get('_img')
        self.ICON_TEXT = self.icon_get('_text')
        self.ICON_AUDIO = self.icon_get('_audio')
        self.ICON_VIDEO = self.icon_get('_video')


    def init_panel(self, and_activate=True):
        # already inited?
        if self.tree:
            return

        self.init_form_main()

        #dlg_proc(self.h_dlg, DLG_SCALE)
        tree_proc(self.tree, TREE_THEME) #TREE_THEME only after DLG_SCALE

        app_proc(PROC_SIDEPANEL_ADD_DIALOG, (self.title, self.h_dlg, 'project.png'))

        if and_activate:
            self.do_show()

        self.action_refresh()
        self.generate_context_menu()


    def show_panel(self):
        self.do_show()

    def focus_panel(self):
        self.do_show()
        app_proc(PROC_SIDEPANEL_ACTIVATE, (self.title, True))

    def do_show(self):
        if not self.tree:
            self.init_panel(True)
        else:
            ed.cmd(cudatext_cmd.cmd_ShowSidePanelAsIs)
            app_proc(PROC_SIDEPANEL_ACTIVATE, self.title)

    def tree_mouse_down(self, id_dlg, id_ctl, data='', info=''):
        # any stale suppress-flag is for a click which already happened
        self._tree_suppress_click = False
        # a new mouse gesture cancels any stale drag state (eg an inner drag
        # of the previous gesture which never saw its mouse-up)
        self._tree_drag_mode = None
        self._tree_drop_target = None
        self._tree_hover_item = None
        self._tree_drag_out_item = None
        self._tree_drag_out_path = ''
        if isinstance(data, dict) and data.get('btn', -1) == 0:
            x = int(data.get('x', 0))
            y = int(data.get('y', 0))
            item = tree_proc(self.tree, TREE_ITEM_FIND_BY_POS, 0, -1,
                             '{},{}'.format(x, y))
            self._tree_drag_press = (x, y, item)
        else:
            self._tree_drag_press = None

    def tree_mouse_move(self, id_dlg, id_ctl, data='', info=''):
        press = self._tree_drag_press
        if press is None or not isinstance(data, dict):
            return
        x = int(data.get('x', 0))
        y = int(data.get('y', 0))

        if self._tree_drag_mode is None:
            if max(abs(x-press[0]), abs(y-press[1])) <= self.TREE_DRAG_THRESHOLD:
                return
            item = press[2]
            if not item or not self._is_draggable(item):
                return
            if IS_WIN:
                # the drag starts immediately, exactly like the drag of files
                # out of CudaText always worked: CudaText runs an OLE
                # CF_HDROP drag, so the node can be dropped into Explorer
                # and other apps. Dropping it back onto this panel is
                # reported to panel_on_drop_files (self_drag=True) and is
                # handled as the inner drag&drop (see _do_panel_self_drop).
                self._begin_drag_out(item)
            else:
                # OS-es without the OLE drag-out: track the inner drag&drop
                # by mouse events while the cursor stays over the tree
                self._tree_drag_mode = 'inner'
                self._tree_drop_target = None
                self._tree_hover_item = None
                self._tree_hover_time = time.time()
                self._tree_drag_sel = self.selected
                self._tree_suppress_click = True
                self._tree_drag_update(x, y)
            return

        if self._tree_drag_mode == 'inner':
            if not self._point_in_tree(x, y):
                # cursor left the tree: cancel the inner drag&drop
                self._restore_drag_selection()
                self._tree_drag_press = None
                self._tree_drag_mode = None
                self._tree_drop_target = None
                self._tree_hover_item = None
                return
            self._tree_drag_update(x, y)

    def tree_mouse_up(self, id_dlg, id_ctl, data='', info=''):
        press = self._tree_drag_press
        mode = self._tree_drag_mode
        target = self._tree_drop_target
        saved_sel = self._tree_drag_sel
        self._tree_drag_press = None
        self._tree_drag_mode = None
        self._tree_drop_target = None
        self._tree_drag_bounds = None
        self._tree_hover_item = None
        self._tree_drag_sel = None

        if mode != 'inner':
            return

        # a drag gesture ended with the button still held by our code:
        # the OnClick which LCL fires for this mouse-up must not open files
        self._tree_suppress_click = True

        if press and target:
            self._do_tree_drop(press[2], target)
        else:
            # dropped on nothing allowed: when the drop was on some node,
            # explain why it is not allowed; restore the selection
            if press and isinstance(data, dict):
                x = int(data.get('x', 0))
                y = int(data.get('y', 0))
                hit = tree_proc(self.tree, TREE_ITEM_FIND_BY_POS, 0, -1,
                                '{},{}'.format(x, y))
                if hit and hit != press[2]:
                    self._msg_drop_not_allowed(press[2], hit)
            self._select_item_if_alive(saved_sel)

    #---------------------------------------------------------------------
    # drag&drop helpers (all run in the context of the tree's mouse events)

    def _point_in_tree(self, x, y):
        """Is the point (tree control client coords) inside the tree?"""
        if self._tree_drag_bounds is None:
            w, h = 0, 0
            props = dlg_proc(self.h_dlg, DLG_CTL_PROP_GET, name='tree')
            if props:
                w = int(props.get('w', 0))
                h = int(props.get('h', 0))
            self._tree_drag_bounds = (w, h)
        w, h = self._tree_drag_bounds
        return 0 <= x < w and 0 <= y < h

    def _begin_drag_out(self, item):
        """Start dragging the item's file/folder out to other apps (Windows:
        OLE CF_HDROP drag, implemented by CudaText's 'drag_files'). The OLE
        drag owns the mouse from now on. When the node is dropped back onto
        this panel, CudaText reports it to panel_on_drop_files with
        self_drag=True (after the OLE drag returned; when no OLE target
        accepted the drop, the release position over the panel is reported
        instead), and _do_panel_self_drop() completes the inner drag&drop
        (reorder / move, with confirmation dialogs)."""
        self._tree_drag_press = None
        self._tree_drag_mode = None
        self._tree_suppress_click = True
        self._tree_drag_out_item = item
        path = str(self.get_location_by_index(item)) if item else ''
        self._tree_drag_out_path = path
        if not item or self.tree_index is None:
            return
        if path and os.path.exists(path):
            dlg_proc(self.h_dlg, DLG_CTL_PROP_SET, index=self.tree_index,
                     prop={'drag_files': path})

    def _restore_drag_selection(self):
        """Restore the tree selection saved at drag start (drag cancelled)."""
        sel = self._tree_drag_sel
        self._tree_drag_sel = None
        self._select_item_if_alive(sel)

    def _select_item_if_alive(self, item):
        """Select the item when its handle is still valid (the tree may
        have been rebuilt in the meantime)."""
        if item and tree_proc(self.tree, TREE_ITEM_GET_PROPS, item):
            tree_proc(self.tree, TREE_ITEM_SELECT, item)

    def _is_draggable(self, item):
        """Not the project root node, not a 'dummy' node: has a data-path."""
        props = tree_proc(self.tree, TREE_ITEM_GET_PROPS, item)
        return bool(props and props.get('data', ''))

    def _tree_drag_update(self, x, y):
        """Inner drag: highlight the drop target under the cursor."""
        item = tree_proc(self.tree, TREE_ITEM_FIND_BY_POS, 0, -1,
                         '{},{}'.format(x, y))
        press = self._tree_drag_press
        src_item = press[2] if press else None
        target = item if item and src_item and \
            self._drop_kind(src_item, item) else None
        if target == self._tree_drop_target:
            if target:
                self._hover_unfold(target)
            return

        self._tree_drop_target = target
        if target:
            tree_proc(self.tree, TREE_ITEM_SELECT, target)
            self._tree_hover_item = target
            self._tree_hover_time = time.time()
        else:
            self._tree_hover_item = None
            # keep the dragged node selected, like OS file managers do
            sel = src_item if src_item else self._tree_drag_sel
            if sel:
                tree_proc(self.tree, TREE_ITEM_SELECT, sel)

    def _hover_unfold(self, item):
        """Unfold a folder node after hovering on it during a drag."""
        if item != self._tree_hover_item:
            self._tree_hover_item = item
            self._tree_hover_time = time.time()
            return
        if time.time() - self._tree_hover_time < self.TREE_UNFOLD_HOVER_SEC:
            return
        props = tree_proc(self.tree, TREE_ITEM_GET_PROPS, item)
        if props and props.get('folded') and props.get('sub_items'):
            tree_proc(self.tree, TREE_ITEM_UNFOLD, item)
            # don't unfold it again while still hovering
            self._tree_hover_time = time.time() + 3600.0

    def _drop_kind(self, src_item, tgt_item):
        """What does dropping the node src_item onto the node tgt_item mean?
        Returns 'reorder', 'reorder_end' or 'move', or None (not allowed).
        Rules:
        - a project's root node (level 1) can only change its order among the
          other root nodes; it is never moved inside another node;
        - a file/folder inside a root folder node (level 2+) can be moved
          into another folder node (moved on disk, like Cut+Paste)."""
        src = tree_proc(self.tree, TREE_ITEM_GET_PROPS, src_item) \
            if src_item else None
        dst = tree_proc(self.tree, TREE_ITEM_GET_PROPS, tgt_item)
        if not src or not dst:
            return None
        src_lvl = src.get('level', 0)
        dst_lvl = dst.get('level', 0)
        src_path = src.get('data', '')
        dst_path = dst.get('data', '')

        if not src_path or src_lvl < 1:
            return None

        if src_lvl == 1:
            # a project's root node: reorder only, never inside another node
            if dst_lvl == 1:
                if self._same_path(dst_path, src_path):
                    return None
                return 'reorder'
            if dst_lvl == 0:
                # onto the project header: reorder to the end of the list
                return 'reorder_end'
            return None

        # a file/folder inside some root folder node: can be moved
        # into another folder node (never onto the project header)
        if dst_lvl == 0:
            return None
        if dst_path and os.path.isdir(dst_path) and not is_locked(dst_path):
            if self._same_path(dst_path, src_path):
                return None
            # not into a subfolder of itself (only matters for folders)
            if os.path.isdir(src_path) and \
              self._path_is_inside(dst_path, src_path):
                return None
            # not onto its own parent folder: nothing would change
            if self._same_path(os.path.dirname(src_path), dst_path):
                return None
            return 'move'
        return None

    @staticmethod
    def _same_path(p1, p2):
        if not p1 or not p2:
            return False
        return os.path.normcase(os.path.normpath(p1)) == \
               os.path.normcase(os.path.normpath(p2))

    @staticmethod
    def _path_is_inside(path, folder):
        return os.path.normcase(os.path.normpath(path)).startswith(
            os.path.normcase(os.path.normpath(folder)) + os.sep)

    def _do_tree_drop(self, src_item, tgt_item):
        """Perform the drop of src_item onto tgt_item (inner drag&drop).
        Asks the user for confirmation:
        - changing the order of a node (a root node dropped onto another
          root node / onto the project header);
        - moving a file/folder from its folder node into another folder node.
        A reorder also needs sorting to be off, else the manual order is
        overwritten by sorting: the user is asked to disable sorting."""
        kind = self._drop_kind(src_item, tgt_item)
        if kind is None:
            self._msg_drop_not_allowed(src_item, tgt_item)
            self._restore_drag_selection()
            return

        src_props = tree_proc(self.tree, TREE_ITEM_GET_PROPS, src_item)
        tgt_props = tree_proc(self.tree, TREE_ITEM_GET_PROPS, tgt_item)
        src_path = src_props.get('data', '')
        tgt_path = tgt_props.get('data', '')

        if kind in ('reorder', 'reorder_end'):
            if not self._confirm_disable_sorting_for_dnd():
                # user declined to disable sorting: cancel the drag&drop,
                # with sorting on the manual order is overwritten anyway
                self._restore_drag_selection()
                return
            name = os.path.basename(src_path.rstrip('/\\'))
            res = msg_box_ex(
                _('Project Manager'),
                _("Change the order of the node '{}' in the project?").format(name),
                [_('Accept'), _('Cancel')],
                MB_ICONQUESTION)
            if res != 0:
                self._restore_drag_selection()
                return
            if kind == 'reorder':
                self._reorder_root_node(src_path, tgt_path)
            else:
                self._reorder_root_node(src_path, None)

        elif kind == 'move':
            src_parent = os.path.dirname(src_path.rstrip('/\\'))
            res = msg_box_ex(
                _('Project Manager'),
                _("Move '{}' from folder '{}' to folder '{}'?").format(
                    os.path.basename(src_path.rstrip('/\\')),
                    os.path.basename(src_parent.rstrip('/\\')) or src_parent,
                    os.path.basename(tgt_path.rstrip('/\\')) or tgt_path),
                [_('Accept'), _('Cancel')],
                MB_ICONQUESTION)
            if res != 0:
                self._restore_drag_selection()
                return
            self._move_node_on_disk(src_path, tgt_path)

    def _confirm_disable_sorting_for_dnd(self):
        """A drag&drop which changes the order of nodes needs the option
        "Sorting order" to be empty ('none'), else the manual order is
        overwritten by sorting on the next refresh. Ask the user to disable
        sorting. Returns True when the reorder may continue (sorting was
        already off, or the user disabled it now)."""
        if not self.options.get('sort_order', 'ext'):
            return True
        res = msg_box_ex(
            _('Project Manager'),
            _('Drag & drop changes the order of project nodes, but the tree is sorted now: the new order would be overwritten by sorting.') + '\n\n' +
            _('Disable sorting (set "Sorting order" to "none")?'),
            [_('Disable sorting'), _('Cancel')],
            MB_ICONQUESTION)
        if res != 0:
            return False
        self.options['sort_order'] = ''
        self.save_options()
        self.action_refresh()
        msg_status(_('Sorting disabled (drag & drop can change the order of nodes now)'))
        return True

    def _msg_drop_not_allowed(self, src_item, tgt_item):
        """Show a statusbar message explaining why the drop of src_item onto
        tgt_item is not allowed."""
        src = tree_proc(self.tree, TREE_ITEM_GET_PROPS, src_item) \
            if src_item else None
        dst = tree_proc(self.tree, TREE_ITEM_GET_PROPS, tgt_item) \
            if tgt_item else None
        if not src or not dst:
            return
        src_lvl = src.get('level', 0)
        dst_lvl = dst.get('level', 0)
        src_path = src.get('data', '')
        dst_path = dst.get('data', '')

        if src_lvl == 1 and dst_lvl >= 2:
            msg_status(_("Project nodes can only change their order, they cannot be moved inside other nodes"))
        elif src_lvl >= 2 and dst_lvl == 0:
            msg_status(_("Cannot drop onto the project header: drop onto a folder node to move the file/folder"))
        elif src_lvl >= 2 and dst_path and os.path.isdir(dst_path) and \
                self._same_path(os.path.dirname(src_path), dst_path):
            msg_status(_("File/folder is already in this folder"))
        elif src_lvl >= 2 and dst_path and not os.path.isdir(dst_path):
            msg_status(_("Cannot drop onto a file: drop onto a folder node to move the file/folder"))
        else:
            msg_status(_("Drop not allowed here"))

    def _reorder_root_node(self, src_path, before_path):
        """Move the root project entry src_path before the root entry
        before_path (or to the end of the list when before_path is None)."""
        nodes = self.project.get('nodes', [])
        if src_path not in nodes:
            return
        nodes.remove(src_path)
        if before_path and before_path in nodes:
            nodes.insert(nodes.index(before_path), src_path)
        else:
            nodes.append(src_path)
        self.action_refresh()
        if self.project_file_path:
            self.action_save_project_as(self.project_file_path)
        msg_status(_('Project node moved'))

    def _move_node_on_disk(self, src_path, tgt_folder):
        """Move the file/folder src_path into tgt_folder, like the plugin's
        Cut+Paste does. Updates the project when the moved path was a root
        node or the main file."""
        import shutil
        dst_path = os.path.join(tgt_folder, os.path.basename(src_path))
        if os.path.exists(dst_path):
            msg_status(_('Cannot move, already exists: ') + collapse_filename(dst_path))
            return
        try:
            shutil.move(src_path, dst_path)
        except OSError as ex:
            msg_status(_('Cannot move: ') + str(ex))
            return

        # a root entry which was moved (a root folder dragged deeper is not
        # allowed, but be safe): point the project to the new location
        if src_path in self.project['nodes']:
            self.project['nodes'][self.project['nodes'].index(src_path)] = dst_path
        if self.project.get('mainfile', '') == src_path:
            self.project['mainfile'] = dst_path
            self.update_global_data()

        if self.project_file_path:
            self.action_save_project_as(self.project_file_path)
        self.action_refresh()
        msg_status(_('Moved to: ') + collapse_filename(dst_path))

    #---------------------------------------------------------------------
    # drop of files onto the panel

    def panel_on_drop_files(self, id_dlg, id_ctl, data='', info=''):
        """Files/folders dropped onto the Project panel.

        - Files/folders dropped from the OS file manager are added to the
          current project (CudaText opens files dropped anywhere else in
          the editor, as before).
        - On Windows, a node dragged out of the tree (OLE 'drag_files')
          and dropped back onto the panel is the inner drag&drop: reorder
          the node / move a file/folder into a folder node, with
          confirmation dialogs. CudaText reports it with self_drag=True;
          the event is fired after the OLE drag returned (deferred by
          proc_customdialog_dummy: never from inside IDropTarget::Drop),
          and when the drop was not accepted by any OLE drop target, the
          release position over the panel is reported the same way
          (fallback), so the inner drag&drop cannot get lost. The drop
          position is in the panel form's client coords.

        `data` is a dict: { 'files': str (paths separated by chr(1)),
        'x': int, 'y': int, 'self_drag': bool } ('x'/'y' are -1 when the
        position is unknown, eg drops on non-Windows OS-es)."""
        if not self.tree:
            return

        if isinstance(data, dict):
            files_str = str(data.get('files', ''))
            self_drag = bool(data.get('self_drag', False))
            try:
                x = int(data.get('x', -1))
                y = int(data.get('y', -1))
            except (TypeError, ValueError):
                x, y = -1, -1
        else:
            files_str = str(data or '')
            self_drag = False
            x, y = -1, -1

        if not files_str:
            # loud: a drop event without files is a bug, don't ignore it
            msg_status(_('Project Manager: drop event without files'))
            return

        if self_drag:
            # round-trip of our own drag-out: inner drag&drop
            # (safety: the dropped path must be the dragged node's path)
            path = self._tree_drag_out_path
            dropped = files_str.split(chr(1))
            if path and not any(self._same_path(path, fn)
                                for fn in dropped):
                # must not happen (the event carries the 'drag_files' list
                # of our own drag); loud, so it can never be silent
                self._tree_drag_out_item = None
                self._tree_drag_out_path = ''
                msg_status(_('Project Manager: inner drop ignored, dropped paths do not match the dragged node: ') +
                           files_str.replace(chr(1), ', '))
                return
            self._do_panel_self_drop(x, y)
            return

        added = []
        skipped = []
        for fn in files_str.split(chr(1)):
            fn = fn.strip().strip('"').strip("'")
            if not fn:
                continue
            if not (os.path.isfile(fn) or os.path.isdir(fn)):
                skipped.append(fn + ' (' + _('not found') + ')')
                continue
            if is_locked(fn):
                skipped.append(fn + ' (' + _('hidden/locked') + ')')
                continue
            self.add_node(fn)
            added.append(fn)

        if added:
            msg_status(_('Added to project') + ': ' + str(len(added)))
        else:
            # loud and diagnostic: show what was received, so a broken
            # drop can never be silent again
            received = files_str.replace(chr(1), ', ')
            detail = ('; ' + '; '.join(skipped)) if skipped else ''
            msg_status(_('Cannot add dropped paths to the project') + ': ' +
                       received + detail)

    def _do_panel_self_drop(self, x, y):
        """A node dragged out of the tree was dropped back onto this panel
        (Windows: the OLE drag of 'drag_files' ended on the panel; it is
        reported by the OLE drop target, or by the fallback in
        ExecuteAppFileDrag when the drop was not accepted by any OLE
        target). (x, y) are the drop position in the panel form's client
        coords: convert them to the tree's coords and perform the inner
        drop on the node under the cursor."""
        src_item = self._tree_drag_out_item
        self._tree_drag_out_item = None
        self._tree_drag_out_path = ''

        if not src_item or not tree_proc(self.tree, TREE_ITEM_GET_PROPS,
                                         src_item):
            msg_status(_('The dragged node is not in the tree anymore'))
            return
        if x < 0 or y < 0:
            msg_status(_('Drop position unknown'))
            return

        # panel form client coords -> tree client coords (live position
        # of the tree control inside the panel form)
        props = dlg_proc(self.h_dlg, DLG_CTL_PROP_GET, name='tree')
        if not props:
            msg_status(_('Cannot find the tree control'))
            return
        tx = x - int(props.get('x', 0))
        ty = y - int(props.get('y', 0))

        tgt_item = tree_proc(self.tree, TREE_ITEM_FIND_BY_POS, 0, -1,
                             '{},{}'.format(tx, ty))
        if not tgt_item:
            msg_status(_('Drop the node onto another node in the project tree'))
            return
        if tgt_item == src_item:
            return # dropped on itself: nothing to do
        self._do_tree_drop(src_item, tgt_item)

    @property
    def selected(self):
        # panel may be not created yet (plugin was lazy-loaded by a menu
        # command, side panel never shown): behave as "nothing selected"
        if not self.tree:
            return None
        return tree_proc(self.tree, TREE_ITEM_GET_SELECTED)

    def add_context_menu_node(self, parent, action, name):
        return menu_proc(parent, MENU_ADD, command=action, caption=name)


    def generate_context_menu(self):
        node_type = None
        if self.selected is not None:
            n = self.get_info(self.selected).image
            if n == self.ICON_PROJ: node_type = NODE_PROJECT
            elif n == self.ICON_DIR: node_type = NODE_DIR
            elif n == self.ICON_BAD: node_type = NODE_BAD
            else: node_type = NODE_FILE

        if not self.h_menu:
            self.h_menu = menu_proc(0, MENU_CREATE)

        menu_all = self.h_menu
        menu_proc(menu_all, MENU_CLEAR)
        menu_proj = self.add_context_menu_node(menu_all, "0", _("Project file"))
        menu_nodes = self.add_context_menu_node(menu_all, "0", _("Root nodes"))
        if node_type == NODE_FILE:
            menu_file = self.add_context_menu_node(menu_all, "0", _("Selected file"))
        if node_type == NODE_DIR:
            menu_dir = self.add_context_menu_node(menu_all, "0", _("Selected folder"))

        for item in self.menuitems:
            item_caption = item[0]
            item_parent = item[1]
            item_types = item[2]
            item_action = item[3]
            item_hotkey = item[4]
            if node_type not in item_types:
                continue

            if item_parent == "proj":
                menu_use = menu_proj
            elif item_parent == "nodes":
                menu_use = menu_nodes
            elif item_parent == "file":
                menu_use = menu_file
            elif item_parent == "dir":
                menu_use = menu_dir
            else:
                menu_use = menu_all

            if item_action in ("collect_recent_projects", "sessions_submenu"):
                action = ""
            else:
                action = item_action

            menu_added = self.add_context_menu_node(menu_use, action, item_caption)

            if item_hotkey:
                menu_proc(menu_added, MENU_SET_HOTKEY, item_hotkey)

            if item_action == "collect_recent_projects":
                for path in self.options["recent_projects"]:
                    if os.sep in path:
                        action = str.format("module=cuda_project_man;cmd=action_open_project;info={};", path)
                        self.add_context_menu_node(menu_added, action, nice_filename(path))

            elif item_action == "sessions_submenu":
                self.fill_sessions_menu(menu_added)

    @staticmethod
    def node_ordering(node):
        # sort folders first, then by extension
        path = Path(node)
        return path.is_file(), path.suffix.upper(), path.name.upper()

    @staticmethod
    def node_ordering_direntry(path):
        ''' node_ordering() for os.DirEntry and pathlib.Path '''
        global sort_order
        isfile = path.is_file()
        if sort_order=='ext':
            key = _file_ext(path.name).upper() if isfile else ''
        elif sort_order=='name':
            key = path.name.upper()
        elif sort_order=='size':
            key = -path.stat().st_size if isfile else ''
        elif sort_order=='size-':
            key = path.stat().st_size if isfile else ''
        elif sort_order=='mtime':
            key = -path.stat().st_mtime if isfile else ''
        elif sort_order=='mtime-':
            key = path.stat().st_mtime if isfile else ''
        elif sort_order=='ctime':
            key = -path.stat().st_ctime if isfile else ''
        elif sort_order=='ctime-':
            key = path.stat().st_ctime if isfile else ''
        elif sort_order=='':
            key = ''
        else:
            raise ValueError('Unknown sort_order: '+sort_order)

        return isfile, key, path.name.upper()

    def add_node(self, path):
        if path:
            if path in self.project["nodes"]:
                return

            msg_status(_("Adding to project: ") + collapse_filename(path), True)
            self.project["nodes"].append(path)
            self.project["nodes"].sort(key=Command.node_ordering)
            self.action_refresh()
            if self.project_file_path:
                self.action_save_project_as(self.project_file_path)

    def new_project(self):
        """Reset the plugin state to "no project". Only used at plugin
        startup, when nothing is open yet — it never touches sessions,
        tabs or files."""
        self.project = normalize_project_data(dict(nodes=[]))
        self.project_file_path = None
        self.update_global_data()
        self.goto_history = []
        self.cur_dir = ''

        app_proc(PROC_SET_FOLDER, '')
        app_proc(PROC_SET_PROJECT, '')

    def add_recent(self, path):
        recent = self.options["recent_projects"]
        if path in recent:
            recent.pop(recent.index(path))

        self.options["recent_projects"] = ([path] + recent)[:10]
        self.generate_context_menu()

    def action_new_file(self):
        location = Path(self.get_location_by_index(self.selected))
        if location.is_file():
            location = location.parent

        result = dlg_input(_("New file:"), "")
        if not result:
            return

        if os.sep in result:
            msg_status(_("Incorrect file name"))
            return

        path = location / result
        path.touch()
        self.action_refresh()

        #open new file
        self.jump_to_filename(str(path))
        if os.path.isfile(str(path)):
            _file_open(str(path))

    def action_open_def(self):
        fn = self.get_location_by_index(self.selected)
        sfn = str(fn)
        if not os.path.isfile(sfn):
            return
        suffix = OS_SUFFIX
        if suffix=='':
            #Windows
            #os.startfile(sfn) crashes with LSP plugin
            import subprocess
            subprocess.Popen('start "" "{}"'.format(sfn), shell=True, cwd=os.path.dirname(sfn))
        elif suffix=='__mac':
            #macOS
            os.system('open "%s"'%sfn)
        elif suffix=='__haiku':
            #Haiku
            msg_status('"Open in default application" is not implemented for this OS')
        else:
            #other Unixes
            os.spawnvp(os.P_WAIT, 'xdg-open', ('xdg-open', fn.as_uri()))

    def action_focus_in_fileman(self):
        fn = self.get_location_by_index(self.selected)
        sfn = str(fn)
        #if not os.path.isfile(sfn):
            #return
        suffix = OS_SUFFIX

        if suffix=='':
            #Windows
            #os.system('explorer.exe /select,'+sfn)
            import subprocess
            subprocess.Popen(('explorer.exe', '/select,', sfn), shell=True) # works better
        elif suffix=='__mac':
            #macOS
            #macOS cannot handle quoted filename
            os.system('open --new --reveal '+sfn.replace(' ', '\\ '))
        elif suffix=='__haiku':
            #Haiku
            msg_status('"Reveal in file manager" is not implemented for this OS')
        else:
            #Linux and others
            if os.spawnvp(os.P_WAIT, 'dbus-send', ('dbus-send', '--session', '--dest=org.freedesktop.FileManager1', '--type=method_call', '--print-reply', '/org/freedesktop/FileManager1', 'org.freedesktop.FileManager1.ShowItems', 'array:string:'+fn.as_uri(), 'string:')):
                os.spawnvp(os.P_WAIT, 'xdg-open', ('xdg-open', fn.parent.as_uri()))

    def action_rename(self):
        location = Path(self.get_location_by_index(self.selected))
        e = _file_editor(str(location))

        # TODO: Support unsaved changes when API available
        if e is not None \
        and e.get_prop(PROP_MODIFIED, '') == True \
        and msg_box(_('The file you are renaming has unsaved changes. Would you like to save the changes first?'), MB_OKCANCEL+MB_ICONWARNING) != ID_OK:
            return

        result = dlg_input(_("Rename to:"), str(location.name))
        if not result:
            return

        new_location = location.parent / result
        if location == new_location:
            return

        if e is not None:
            e.save(str(new_location))
            location.unlink(True)
        else:
            location.replace(new_location)

        if self.is_path_in_root(location):
            self.action_remove_node()
            self.add_node(str(new_location))

        # fix filename of existing ui-tab
        for h in ed_handles():
            e = Editor(h)
            if e.get_prop(PROP_FN, '*')==str(location):
                e.save(str(new_location))

        self.action_refresh()
        self.jump_to_filename(str(new_location))
        msg_status(_("Renamed to: ") + str(collapse_filename(str(new_location.name))))

    def action_duplicate(self):
        location = Path(self.get_location_by_index(self.selected))
        e = _file_editor(str(location))

        fn_parts = location.name.split('.')
        from datetime import datetime
        fn_1 = location.name if not fn_parts[0] else fn_parts[0]
        fn_2 = '' if not fn_parts[0] else ('.' + '.'.join(fn_parts[i+1] for i in range(len(fn_parts) - 1)))
        backup_name = fn_1 + '_' + datetime.now().strftime("%y%m%d_%H%M%S") + fn_2 if not os.path.isdir(str(location)) else location.name+' copy'
        result = dlg_input(_("Duplicate to:"), str(backup_name))
        if not result:
            return

        new_location = location.parent / result
        if location == new_location:
            return

        if e is not None:
            e.save(str(new_location), True)
        else:
            import shutil
            if os.path.isdir(location):
                shutil.copytree(location, new_location, symlinks=False)
            else:
                shutil.copy2(location, new_location)

        if self.is_path_in_root(location):
            self.add_node(str(new_location))

        self.action_refresh()
        msg_status(_("Copied to: ") + str(collapse_filename(str(new_location.name))))

    def action_delete_file(self):
        location = Path(self.get_location_by_index(self.selected))
        if msg_box(_("Delete file from disk:\n") + str(location), MB_OKCANCEL + MB_ICONWARNING) != ID_OK:
            return

        location.unlink()
        if self.is_path_in_root(location):
            self.action_remove_node()
        else:
            h_parent = tree_proc(self.tree, TREE_ITEM_GET_PROPS, self.selected)['parent']
            tree_proc(self.tree, TREE_ITEM_SELECT, h_parent)
            self.action_refresh(h_parent)
        msg_status(_("Deleted file: ") + str(location.name))

    def action_copy_relative_path(self):
        file_path = str(self.get_location_by_index(self.selected))
        project_node = ''
        for node in self.project["nodes"]:
            if (file_path+os.sep).startswith(node+os.sep):
                project_node = node
                break
        if project_node:
            file_path = file_path[len(os.path.dirname(project_node))+1:]
        app_proc(PROC_SET_CLIP, file_path)

    def action_copy_path(self):
        file_path = str(self.get_location_by_index(self.selected))
        proj_path = os.path.dirname(self.project_file_path or '')
        if not proj_path:
            return
        if file_path.startswith(proj_path+os.sep):
            file_path = file_path[len(proj_path)+1:]
        app_proc(PROC_SET_CLIP, file_path)

    def action_cut(self):
        selected = self.get_location_by_index(self.selected)
        if (selected.is_file() or selected.is_dir()):
            app_proc(PROC_SET_CLIP, str(selected))
        self.action_cut_activate = True

    def action_copy(self):
        selected = self.get_location_by_index(self.selected)
        if (selected.is_file() or selected.is_dir()):
            app_proc(PROC_SET_CLIP, str(selected))

    def action_paste(self):
        location = app_proc(PROC_GET_CLIP, '')
        new_location = Path(self.get_location_by_index(self.selected))
        if (Path(location).is_file() or Path(location).is_dir()):
            if new_location.is_file():
                new_location = new_location.parent
            if Path(location).is_file():
                new_location_path = str(new_location) + os.sep + str(Path(location).name)
                if location == new_location_path:
                    fn_parts = Path(location).name.split('.')
                    from datetime import datetime
                    fn_1 = Path(location).name if not fn_parts[0] else fn_parts[0]
                    fn_2 = '' if not fn_parts[0] else ('.' + '.'.join(fn_parts[i+1] for i in range(len(fn_parts) - 1)))
                    new_name = fn_1 + '_' + datetime.now().strftime("%y%m%d_%H%M%S") + fn_2
                    new_location = str(new_location) + os.sep + new_name
                import shutil
                if self.action_cut_activate:
                    shutil.move(location, new_location)
                else:
                    shutil.copy2(location, new_location)
            else:
                new_location = str(new_location) + os.sep + Path(location).name
                import shutil
                if self.action_cut_activate:
                    shutil.move(str(location), new_location)
                else:
                    # shutil, not the removed distutils (gone in Python 3.12);
                    # copytree refuses to overwrite an existing folder, while
                    # distutils copy_tree silently merged into it
                    try:
                        shutil.copytree(str(location), new_location)
                    except OSError as ex:
                        msg_status(_('Cannot copy folder: ') + str(ex))
                        return
            self.action_refresh()

    def get_w_h(self):
        w_ = 600
        h_ = 600
        r = app_proc(PROC_COORD_MONITOR, 0)
        if r:
            w_ = (r[2]-r[0]) // 3
            h_ = (r[3]-r[1]) // 4

        return w_, h_

    def callback_button_ok(self, id_dlg, id_ctl, data='', info=''):
        dlg_proc(id_dlg, DLG_HIDE)

    def show_memo(self, text, caption):
        w_, h_ = self.get_w_h()
        h = dlg_proc(0, DLG_CREATE)
        dlg_proc(h, DLG_PROP_SET, prop={
            'w': w_,
            'h': h_,
            'w_min': 300,
            'h_min': 150,
            'cap': caption,
            'border': DBORDER_SIZE,
        })

        n = dlg_proc(h, DLG_CTL_ADD, prop='button')
        dlg_proc(h, DLG_CTL_PROP_SET, index=n, prop={
            'name': 'btn_ok',
            'a_l': None,
            'a_t': None,
            'a_r': ('', ']'),
            'a_b': ('', ']'), 
            'sp_a': 6,
            'x': w_-100,
            'y': h_-6-25,
            'w': 100-6,
            'cap': _('&OK'),
            'on_change': 'module=cuda_project_man;cmd=callback_button_ok;',
            'ex0': True,
        })

        n = dlg_proc(h, DLG_CTL_ADD, prop='memo')
        dlg_proc(h, DLG_CTL_PROP_SET, index=n, prop={
            'name': 'memo_log',
            'val': text,
            'a_r': ('btn_ok', ']'),
            'a_b': ('btn_ok', '['), 
            'x': 6,
            'y': 6,
            'w': w_-6*2,
            'h': h_-6*3-25,
            'ex0': True,
            'ex1': True,
        })

        dlg_proc(h, DLG_CTL_FOCUS, name='btn_ok')
        dlg_proc(h, DLG_SCALE)
        dlg_proc(h, DLG_SHOW_MODAL)
        dlg_proc(h, DLG_FREE)

    def convert_size(self, size_bytes):
        size_bytes = int(size_bytes)
        if size_bytes == 0:
            return '0 b'
        size_name = ('b', 'kB', 'mB', 'gB')
        import math
        i = int(math.floor(math.log(size_bytes, 1024)))
        p = math.pow(1024, i)
        s = round(size_bytes / p, 2)
        return str("%s %s" % (s, size_name[i]))

    def action_get_properties(self):
        import os
        import datetime
        selected = self.get_location_by_index(self.selected)
        if os.path.exists(selected):
            if os.path.isfile(selected):
                text = _('File: ') + str(selected) + "\n"
                text += _('Size: ') + self.convert_size(os.path.getsize(selected)) + "\n"
                text += _('Date of creation: ') + str(datetime.datetime.fromtimestamp(int(os.path.getctime(selected)))) + "\n"
                text += _('Date of opening: ') + str(datetime.datetime.fromtimestamp(int(os.path.getatime(selected)))) + "\n"
                text += _('Date of modification: ') + str(datetime.datetime.fromtimestamp(int(os.path.getmtime(selected)))) + "\n"
            elif os.path.isdir(selected):
                text = _('Folder: ') + str(selected) + "\n"
                def folder_size(path):
                    total = 0
                    for entry in os.scandir(path):
                        if entry.is_file():
                            total += entry.stat().st_size
                        elif entry.is_dir():
                            total += folder_size(entry.path)
                    return total
                text += _('Size: ') + str(self.convert_size(folder_size(selected))) + "\n"
        else:
            text = _('Object not found')

        if text:
            self.show_memo(text, _('Properties'))

    def do_delete_dir(self, location):
        for path in location.glob("*"):
            if path.is_file():
                path.unlink()
            elif path.is_dir():
                self.do_delete_dir(path)
        location.rmdir()

    def action_delete_directory(self):
        location = Path(self.get_location_by_index(self.selected))
        if msg_box(_("Delete folder from disk:\n") + str(location), MB_OKCANCEL + MB_ICONWARNING) != ID_OK:
            return

        self.do_delete_dir(location)
        if self.is_path_in_root(location):
            self.action_remove_node()
        else:
            h_parent = tree_proc(self.tree, TREE_ITEM_GET_PROPS, self.selected)['parent']
            tree_proc(self.tree, TREE_ITEM_SELECT, h_parent)
            self.action_refresh(h_parent)

            path = self.get_location_by_index(self.selected)
            app_proc(PROC_SET_FOLDER, path)
            self.cur_dir = path

        msg_status(_("Deleted folder: ") + str(location.name))

    def action_new_directory(self):
        location = Path(self.get_location_by_index(self.selected))
        if location.is_file():
            location = location.parent
        result = dlg_input(_("New folder:"), "")
        if not result:
            return

        location = location / result
        location.mkdir()
        self.action_refresh()
        self.jump_to_filename(str(location))

    def action_find_in_directory(self):
        location = str(self.get_location_by_index(self.selected))
        try:
            import cuda_find_in_files as fif
            msg_status(_('Called "Find in Files" for "%s"') % location)
            fif.show_dlg(what="", opts={"fold": location})
        except ImportError:
            try:
                from cuda_find_in_files4 import show_fif4
                msg_status(_('Called "Find in Files 4" for "%s"') % location)
                show_fif4({'with': {
                    'in_what': '',
                    'wk_fold': location,
                    'wk_incl': '*'
                    }})
            except ImportError:
                msg_box(_('Plugin "Find in Files" or "Find in Files 4" is not installed; install it first'), MB_OK + MB_ICONERROR)


    def action_refresh(self, parent=None):
        # panel may be not created yet (plugin was lazy-loaded by a menu
        # command, side panel never shown): nothing to refresh, the tree
        # is filled by init_panel() when the panel is shown
        if not self.tree:
            return

        sel_fn = ''
        id = self.selected
        if id:
            prop = tree_proc(self.tree, TREE_ITEM_GET_PROPS, id)
            if prop:
                sel_fn = prop.get('data', '')

        # it was hard to add TREE_LOCK/UNLOCK directly into action_refresh_int
        tree_proc(self.tree, TREE_LOCK)
        try:
            unfolds = []
            self.enum_all_getfolds(unfolds)
            #msg_box('unfolds:\n'+', '.join(unfolds), MB_OK)

            self.action_refresh_int(parent)

            self.enum_all_setfolds(unfolds)
        finally:
            tree_proc(self.tree, TREE_UNLOCK)

        if sel_fn:
            #print('sel to:', sel_fn)
            self.enum_all_sel(sel_fn)


    def get_project_name(self):

        if self.project_file_path is None:
            return PROJECT_UNSAVED_NAME
        else:
            return self.project_file_path.stem

    def action_refresh_int(self, parent=None):

        global sort_order
        sort_order = self.options.get('sort_order', 'ext')

        unfold = parent is None
        if parent is None:
            # clear tree
            tree_proc(self.tree, TREE_ITEM_DELETE, 0)
            project_name = self.get_project_name()

            parent = tree_proc(
                self.tree,
                TREE_ITEM_ADD,
                0,
                -1,
                project_name,
                self.ICON_PROJ,
            )

            #select 1st node
            items_root = tree_proc(self.tree, TREE_ITEM_ENUM_EX, 0)
            tree_proc(self.tree, TREE_ITEM_SELECT, items_root[0]['id'])

            nodes = map(Path, self.project.get("nodes", []))
            if sort_order != '':
                nodes = sorted(nodes, key=Command.node_ordering_direntry)
        else:
            fn = str(self.get_location_by_index(parent)) # str() is required for old Python 3.5 for os.scandir()
            if not fn: return
            #print('Reading dir:', fn)
            try:
                if hasattr(os, "scandir") and callable(os.scandir):
                    if sort_order == '':
                        scandir_ = list(os.scandir(fn))
                        dirs_ = [i for i in scandir_ if i.is_dir()]
                        files_ = [i for i in scandir_ if not i.is_dir()]
                        nodes = dirs_ + files_
                    else:
                        nodes = sorted(os.scandir(fn), key=Command.node_ordering_direntry)
                else:
                    # support for old Python 3.4
                    if sort_order == '':
                        nodes = Path(fn).iterdir()
                    else:
                        nodes = sorted(Path(fn).iterdir(), key=Command.node_ordering)
            except Exception:
                tree_proc(self.tree, TREE_ITEM_SET_ICON, parent, image_index=self.ICON_BAD)
                raise # good to see the error
                return

        for path in nodes:
            # DirEntry or Path?
            if isinstance(path, Path):
                spath = str(path)
            else:
                spath = path.path
            is_dir = path.is_dir()
            sname = path.name
            if is_win_root(spath) or (not sname):
                sname = spath
            elif self.options.get("no_hidden", True) and is_hidden(spath):
                continue
            elif self.is_filename_ignored(spath, is_dir):
                continue

            if is_locked(spath):
                imageindex = self.ICON_BAD
            elif is_dir:
                imageindex = self.ICON_DIR
            elif is_simple_listed(path.name, MASKS_IMAGES):
                imageindex = self.ICON_IMG
            elif is_simple_listed(path.name, MASKS_ZIP):
                imageindex = self.ICON_ZIP
            elif is_simple_listed(path.name, MASKS_BINARY):
                imageindex = self.ICON_BIN
            elif is_simple_listed(path.name, MASKS_AUDIO):
                imageindex = self.ICON_AUDIO
            elif is_simple_listed(path.name, MASKS_VIDEO):
                imageindex = self.ICON_VIDEO
            else:
                lexname = lexer_proc(LEXER_DETECT, path.name)
                if lexname:
                    imageindex = self.icon_get(lexname)
                elif is_simple_listed(path.name, MASKS_TEXT):
                    imageindex = self.ICON_TEXT
                else:
                    imageindex = self.ICON_ALL

            index = tree_proc(
                self.tree,
                TREE_ITEM_ADD,
                parent,
                -1,
                sname,
                imageindex,
                data=spath
                )

            # dummy nested node for folders
            if imageindex == self.ICON_DIR:
                tree_proc(
                    self.tree,
                    TREE_ITEM_ADD,
                    index,
                    -1,
                    'dummy',
                    -1
                    )

        if unfold:
            tree_proc(self.tree, TREE_ITEM_UNFOLD, parent)

    def action_new_project(self, info=None):
        """Create a new project. Prompts for save location, creates an empty
        session called 'session1' inside the project, and loads it."""

        path = info
        if path is None:
            path = dlg_file(False, "", "", PROJECT_DIALOG_FILTER)
        if not path:
            return

        path = Path(path)
        if path.suffix != PROJECT_EXTENSION:
            path = path.parent / (path.name + PROJECT_EXTENSION)

        if (self.project_file_path
        and os.path.normpath(str(self.project_file_path)) == os.path.normpath(str(path))):
            msg_status(_('Project is already open'))
            return

        self._create_project_file(path, close_tabs=True)
        msg_status(_("Created project: ") + os.path.basename(str(path)))

    def _create_project_file(self, path, close_tabs=True):
        """Create a new project file at `path` and make it the current
        project. Saves the current project (and its session slot) first.
        With close_tabs=True all tabs are closed and session 'session1'
        starts empty (used by New project); with close_tabs=False the
        current tabs are embedded as session 'session1' (used by
        'New project from open tabs...')."""

        # The command can run with the side panel not created yet: the
        # plugin was lazy-loaded by the menu command itself and the panel
        # was never shown (issue #6582). The new project must be visible
        # in the panel, and the code below reads the panel tree (saving
        # the previous project, refresh) — create the panel first.
        if not self.tree:
            self.init_panel()

        path_str = str(path)

        # Save the current project and its session slot before switching
        if self.project_file_path:
            self.action_save_project_as(self.project_file_path)
            cur_fn = str(self.project_file_path)
            cur_sess = self.session_cur_name()
            if cur_fn and cur_sess:
                app_proc(PROC_SAVE_SESSION, cur_fn + '|/sessions/' + cur_sess)

        if close_tabs:
            # Close all tabs (the current state is saved into its session
            # first, so unsaved text is not lost — see
            # _close_tabs_saving_state)
            self._close_tabs_saving_state()

        # Reset project state
        self.project = normalize_project_data(dict(nodes=[]))
        self.project_file_path = path
        self.update_global_data()
        self.goto_history = []
        self.cur_dir = ''

        app_proc(PROC_SET_FOLDER, '')
        app_proc(PROC_SET_PROJECT, path_str)

        # Save the initial project file (current format version)
        d = copy.deepcopy(self.project)
        d['version'] = PROJECT_FORMAT_VERSION
        d['last_session'] = 'session1'
        write_json_file(path, d)

        # Create session1 inside the project and set as active
        sess1 = path_str + '|/sessions/session1'
        app_proc(PROC_SAVE_SESSION, sess1)
        self._set_session(sess1)

        # refresh the in-memory project from the file on disk (it now
        # contains session1 and last_session)
        try:
            with open(path_str, 'r', encoding='utf8') as f:
                data = json.load(f)
            self.project = normalize_project_data(data)
        except (OSError, ValueError):
            pass
        self.project['last_session'] = 'session1'

        self.update_global_data()
        self.add_recent(path_str)
        self.options['on_start'] = True
        self.save_events()
        self.save_options()

        self.action_refresh()

    def action_save_session_as_project(self, info=None):
        """Save the current editor state as a NEW project: prompts for the
        .cuda-proj location, embeds the currently open tabs as session
        'session1', makes it the default session and switches to the new
        project. Open tabs are NOT closed — the current state becomes the
        new project's session. (Migration helper for the old workflow which
        kept sessions in external .cuda-session files.)"""

        path = info
        if path is None:
            path = dlg_file(False, "", "", PROJECT_DIALOG_FILTER)
        if not path:
            return

        path = Path(path)
        if path.suffix != PROJECT_EXTENSION:
            path = path.parent / (path.name + PROJECT_EXTENSION)

        if (self.project_file_path
        and os.path.normpath(str(self.project_file_path)) == os.path.normpath(str(path))):
            msg_status(_('This file is already the current project'))
            return

        self._create_project_file(path, close_tabs=False)
        msg_status(_("Created project from current session: ") + os.path.basename(str(path)))

    def action_save_project(self):
        """Save project in place. Falls back to Save as... if no path yet."""
        if self.project_file_path:
            self.action_save_project_as(self.project_file_path)
        else:
            self.action_save_project_as()

    def action_close_project(self):
        """Close the current project: save session, close all tabs, load
        default.cuda-session (restoring, not overwriting, its tabs)."""

        # Saving the project also saves the current session into its slot
        # inside the project file
        if self.project_file_path:
            self.action_save_project_as(self.project_file_path)

        # Close all tabs. The current state is saved into the session it
        # belongs to first (the project's session slot / the app's session
        # file), so unsaved text is not lost — see _close_tabs_saving_state.
        self._close_tabs_saving_state()

        # Forget the project BEFORE touching the app session: the
        # _set_session/PROC_LOAD_SESSION calls below fire app state events,
        # and the on_state handler must not see a foreign session with a
        # still-open project.
        self._close_project_silent()

        # No project is loaded anymore: the plugin must not load at the
        # next CudaText start. Remove the on_start subscription from
        # plugins.ini [events] (a later open/create/save of a project
        # subscribes it again).
        self._update_on_start_subscription(False)

        # Load default session via PROC_LOAD_SESSION (reads, doesn't overwrite)
        self._set_session(DEF_SES)
        default_sess_path = os.path.join(app_path(APP_DIR_SETTINGS), DEF_SES)
        app_proc(PROC_LOAD_SESSION, default_sess_path)

        msg_status(_("Project closed"))

    def _set_session(self, ptr):
        """app_proc(PROC_SET_SESSION, ...) wrapper for session changes made
        by the plugin itself. Sets a guard flag so the on_state handler
        doesn't treat our own change as an external session switch (which
        would close the project); see APPSTATE_SESSION_SET, issue #6366.
        Also records project sessions as the project's "last_session" —
        the session loaded the next time the project is opened."""
        self._session_set_by_plugin = True
        try:
            app_proc(PROC_SET_SESSION, ptr)
        finally:
            self._session_set_by_plugin = False
        self._remember_last_session(ptr)

    def _remember_last_session(self, ptr):
        """Store the session named by `ptr` as the project's "last_session"
        (in memory and in the project file on disk), so the project reopens
        with the session that was used last.
        Only INTERNAL session pointers (own .cuda-proj path +
        "|/sessions/" + name) are recorded — external session pointers
        (e.g. the default cuda-session) never change, and never overwrite,
        the project's last used session. Also drops the removed
        "def_session" key still present in older project files."""
        fn_proj = str(self.project_file_path) if self.project_file_path else ''
        if not fn_proj or '|/sessions/' not in ptr:
            return
        if not ptr.startswith(fn_proj + '|/sessions/'):
            return
        name = ptr.split('|/sessions/', 1)[1]
        if not name:
            return
        try:
            with open(fn_proj, 'r', encoding='utf8') as f:
                data = json.load(f)
            if data.get('last_session') != name or 'def_session' in data:
                data['last_session'] = name
                data.pop('def_session', None)
                write_json_file(fn_proj, data)
        except (OSError, ValueError):
            pass
        self.project['last_session'] = name

    @staticmethod
    def _read_session_keys(path):
        """Read the 'sessions' and 'last_session' keys from a .cuda-proj
        file on disk. Returns a dict holding only the keys that were found
        with valid types; {} when the file cannot be read or parsed.
        CudaText writes session sub-keys straight into the project file
        (PROC_SAVE_SESSION on session switches, the ~40 s session
        autosave, the save on exit), bypassing every in-memory copy —
        the file on disk is the only up-to-date source for them."""
        try:
            with open(path, 'r', encoding='utf8') as f:
                data = json.load(f)
        except (OSError, ValueError):
            return {}
        res = {}
        if isinstance(data, dict):
            if isinstance(data.get('sessions'), dict):
                res['sessions'] = data['sessions']
            if isinstance(data.get('last_session'), str):
                res['last_session'] = data['last_session']
        return res

    def _sync_sessions_from_disk(self):
        """Refresh the in-memory project's 'sessions' and 'last_session'
        from the project file on disk. Must be called after anything
        saves a session into the file (PROC_SAVE_SESSION), so that a
        later project-file save can never write stale session data back
        over the file — that reverted every non-current session to an
        old state and silently lost typed text (found via on_exit)."""
        if not self.project_file_path:
            return
        fresh = self._read_session_keys(str(self.project_file_path))
        if 'sessions' in fresh:
            self.project['sessions'] = fresh['sessions']
        if 'last_session' in fresh:
            self.project['last_session'] = fresh['last_session']

    def _close_project_silent(self):
        """Reset project state without touching tabs or sessions. Used when
        an external session switch is detected — the foreign session is now
        active and we must detach from the project without disrupting it."""
        self.project = normalize_project_data(dict(nodes=[]))
        self.project_file_path = None
        self.update_global_data()
        self.goto_history = []
        self.cur_dir = ''

        app_proc(PROC_SET_FOLDER, '')
        app_proc(PROC_SET_PROJECT, '')

        self.action_refresh()

    def action_open_project(self, info=None, load_session=True):
        if self.project_file_path:
            self.action_save_project_as(self.project_file_path)

        path = info
        if path is None:
            path = dlg_file(True, "", "", PROJECT_DIALOG_FILTER)
        if not path:
            return

        if not Path(path).exists():
            msg_status(_("Project file not found: ") + collapse_filename(str(path)))
            return

        proj_dir = os.path.dirname(path)

        # Load and validate the project file BEFORE closing any tab, so a
        # broken file leaves the editor state untouched
        print(_('Loading project: ') + collapse_filename(path))
        data, err = load_project_file(path)
        if err:
            self._report_project_load_error(path, err)
            return

        ver = data.get('version', 1)
        if not isinstance(ver, int):
            ver = 1
        if ver > PROJECT_FORMAT_VERSION:
            msg_box(
                _('Cannot open project:\n%s\n\nThe file uses project format version %d,\nbut this Project Manager supports only version %d.\nPlease update CudaText and its plugins.') %
                    (collapse_filename(str(path)), ver, PROJECT_FORMAT_VERSION),
                MB_OK + MB_ICONERROR)
            return

        if ver < PROJECT_FORMAT_VERSION:
            # Old format: normalize the structure in memory, then upgrade the
            # file on disk automatically. The upgrade only adds missing
            # standard keys and a backup of the original is kept next to the
            # file, so it is lossless and reversible — no confirmation dialog
            # is needed (a blocking prompt would only prevent users from
            # opening their own projects; the file is rewritten on exit anyway).
            data = normalize_project_data(data)
            self._migrate_last_session(data)
            self.upgrade_project_file_on_disk(path, data)
        else:
            data = normalize_project_data(data)
            self._migrate_last_session(data)

        # "Open project" can run with the side panel not created yet: the
        # command was called from the "Plugins" menu while the panel was
        # never shown (issue #6582). The opened project must appear in the
        # panel, and the code below reads the panel tree (unfolds,
        # refresh, context menu) — create the panel first.
        if not self.tree:
            self.init_panel()

        # Save the current session to its slot, then close all tabs.
        # Skipped when load_session=False (on_start when CudaText
        # already loaded the project session from history.json).
        if load_session:
            self._close_tabs_saving_state()

        # Adopt the validated project
        self.project = data
        for i in range(len(self.project['nodes'])):
            node = self.project['nodes'][i]
            # normalize os separators
            node = os.path.normpath(node)
            self.project['nodes'][i] = expand_macros(proj_dir, node)

        # detect orphan items (only reported, not removed)
        bads = [fn for fn in self.project["nodes"] if not os.path.exists(fn)]

        self.project_file_path = Path(path)
        self.add_recent(path)
        self.action_refresh()
        self.options['on_start'] = True
        self.save_events()
        self.save_options()

        self.update_global_data()
        self.goto_history = []

        for fn in self.project["nodes"]:
            if os.path.isdir(fn):
                app_proc(PROC_SET_FOLDER, fn)
                self.cur_dir = fn
                break

        app_proc(PROC_SET_PROJECT, path)

        s = _("Opened project: ") + os.path.basename(path)
        if bads:
            s += ', ' + _('%d missing item(s)')%len(bads)
        msg_status(s)

        if load_session:
            names = list(self.project.get('sessions', {}).keys())
            if names:
                # load the session that was used last (the first one when
                # the project has no "last_session" yet or it names a
                # session that no longer exists)
                sess = self.project.get('last_session', '')
                if sess not in names:
                    sess = names[0]
                sess_path = str(path) + '|/sessions/' + sess
                self._set_session(sess_path)
                app_proc(PROC_LOAD_SESSION, sess_path)
            else:
                # A project always keeps at least one session (a project is
                # itself a session): create 'session1' from the current
                # (empty) state, so the app session stays inside the project
                # and can never leak onto an external session file.
                sess_path = str(path) + '|/sessions/session1'
                app_proc(PROC_SAVE_SESSION, sess_path)
                self._set_session(sess_path)
                try:
                    with open(path, 'r', encoding='utf8') as f:
                        self.project['sessions'] = json.load(f).get('sessions', {})
                except (OSError, ValueError):
                    pass
        elif not self.project.get('sessions'):
            # Startup path (load_session=False): CudaText is restoring the
            # project's session from history.json itself. If the file has
            # no sessions at all (left over by old builds), create
            # 'session1' from the restored state now — a project is itself
            # a session, it always keeps one (otherwise the ~40 s session
            # autosave would write the app session pointer's sub-key into
            # the file only much later, and the sessions menu would be
            # empty meanwhile).
            sess_path = str(path) + '|/sessions/session1'
            if app_proc(PROC_SAVE_SESSION, sess_path):
                self._set_session(sess_path)
                self._sync_sessions_from_disk()
        if 'unfold' in self.project:
            unfolds = self.project['unfold']
            for i in range(len(unfolds)):
                unfolds[i] = expand_macros(proj_dir, unfolds[i])
            self.enum_all_setfolds(unfolds)

    @staticmethod
    def _migrate_last_session(data):
        """Migrate the removed 'def_session' key (written by earlier
        builds) to 'last_session'. Files with neither key get their
        last_session filled when a session is switched to / the project
        is opened."""
        if 'def_session' in data:
            # 'not data.get(...)' also covers the '' normalize may have
            # just inserted for a missing key
            if not data.get('last_session'):
                data['last_session'] = data['def_session']
            del data['def_session']
        if not isinstance(data.get('last_session'), str):
            data['last_session'] = ''

    def upgrade_project_file_on_disk(self, path, data):
        """Rewrite an old-format (version-less) project file in the current
        format, after normalize_project_data() was applied to `data`.

        A backup of the original file is saved next to it first (see
        make_project_backup). When the backup or the write fails (read-only
        folder, locked file, ...), the project is still opened with the
        upgraded data in memory and the file on disk is upgraded on the
        first successful save — opening a project must never be blocked.
        The user is informed via the status bar and the console panel.
        """
        data['version'] = PROJECT_FORMAT_VERSION

        bak = make_project_backup(path)
        if bak is None:
            msg_status(_('Project opened; its file could not be upgraded on disk'))
            print(_('Project Manager: cannot back up old project file, '
                'the file on disk was not upgraded: ') + collapse_filename(path))
            print(_('Project Manager: it will get the current format on the first successful save'))
            return

        try:
            write_json_file(path, data)
        except OSError as ex:
            msg_status(_('Project opened; its file could not be upgraded on disk'))
            print(_('Project Manager: cannot upgrade project file on disk: %s') % ex)
            print(_('Project Manager: the original file is kept in the backup: ') +
                collapse_filename(str(bak)))
            print(_('Project Manager: it will get the current format on the first successful save'))
            return

        msg_status(_('Project file upgraded to v%d (backup: %s)') %
            (PROJECT_FORMAT_VERSION, os.path.basename(bak)))
        print(_('Project Manager: old project file was upgraded to format v%d: ') %
            PROJECT_FORMAT_VERSION + collapse_filename(path))
        print(_('Project Manager: backup of the original saved as: ') +
            collapse_filename(str(bak)))
        print(_('Project Manager: you can delete the backup file if everything works fine'))

        # tip for the old (pre-versioning) workflow: a sibling external
        # session file can be embedded into the project
        ses = Path(path).with_suffix('.cuda-session')
        if ses.exists():
            print(_('Project Manager: found an external session file: ') +
                collapse_filename(str(ses)))
            print(_('Project Manager: to embed it into the project, use '
                '"Import external session..." in the sessions menu'))

    def _report_project_load_error(self, path, err):
        """Show a dialog for a project file that failed validation.
        An empty (0-byte) file is special-cased: it holds no data, so the
        user is offered to create a new empty project in it."""

        err_kind = err.split(':', 1)[0]
        err_detail = err.split(':', 1)[1] if ':' in err else ''

        if err_kind == 'empty':
            if msg_box(
                _('The project file is empty:\n%s\n\nCreate a new empty project in this file?') %
                    collapse_filename(str(path)),
                MB_OKCANCEL + MB_ICONQUESTION) == ID_OK:
                self._create_project_file(Path(path), close_tabs=True)
            else:
                msg_status(_('No project open'))
            return

        reasons = {
            'io': _('Cannot read the file.'),
            'json': _('The file does not contain valid JSON.'),
            'type': _('The file is not a CudaText project (a JSON object is expected).'),
            'nodes': _('The file is not a valid CudaText project ("nodes" must be a list of paths).'),
            'sessions': _('The file is not a valid CudaText project ("sessions" must be an object).'),
        }
        reason = reasons.get(err_kind, err)
        if err_detail:
            reason += '\n' + err_detail

        msg_box(_('Cannot open project file:\n%s\n\n%s') % (collapse_filename(str(path)), reason),
            MB_OK + MB_ICONERROR)

    def action_add_folder(self):
        fn = dlg_dir("")
        self.add_node(fn)

    def action_add_file(self):
        fn = dlg_file(True, "", "", "")
        self.add_node(fn)

    def action_remove_node(self):
        index = self.selected
        while True:
            prop = tree_proc(self.tree, TREE_ITEM_GET_PROPS, index)
            if prop["level"] == 0:
                return
            if prop["level"] == 1:
                path = prop["data"]
                break
            index = prop["parent"]

        # fix for network UNC paths '\\server\share\'
        if path.endswith('\\'):
            path = path[:-1]

        tree_proc(self.tree, TREE_ITEM_DELETE, index)
        if path in self.project["nodes"]:
            self.project["nodes"].remove(path)
            if (self.cur_dir+os.sep).startswith(path+os.sep):
                app_proc(PROC_SET_FOLDER, '')
                self.cur_dir = ''
        else:
            msg_box(_('Cannot find path "%s" in root nodes list') % path, MB_OK + MB_ICONERROR)

        if self.project_file_path:
            self.action_save_project_as(self.project_file_path)

    def action_remove_deleted_nodes(self):

        if msg_box(_("Remove missing paths from the project?"), MB_OKCANCEL + MB_ICONWARNING) != ID_OK:
            return

        nodes_deleted = []
        # iterate over a COPY: removing from the same list that is being
        # iterated skips every second matching entry (two adjacent missing
        # paths -> only the first one was removed)
        for node in list(self.project["nodes"]):
            path_ = Path(node)
            if not path_.exists():
                self.project["nodes"].remove(str(path_))
                nodes_deleted.append(str(path_))

        if len(nodes_deleted) > 0:
            self.action_refresh()

            if self.project_file_path:
                self.action_save_project_as(self.project_file_path)
            msg_status(_('Removed missing paths: ') + ', '.join(nodes_deleted))
        else:
            msg_status(_('No missing paths'))

    def action_set_as_main_file(self):

        path = self.get_location_by_index(self.selected)
        self.project["mainfile"] = str(path)
        self.update_global_data()

        if self.project_file_path:
            self.action_save_project_as(self.project_file_path)

    def action_save_project_as(self, path=None):

        need_refresh = path is None
        if path is None:
            if self.project_file_path:
                project_path = str(self.project_file_path.parent)
            else:
                project_path = ""
            path = dlg_file(False, "", project_path, PROJECT_DIALOG_FILTER)

        if path:
            proj_dir = os.path.dirname(str(path))

            path = Path(path)
            if path.suffix != PROJECT_EXTENSION:
                path = path.parent / (path.name + PROJECT_EXTENSION)

            # pre-processing of dict before saving
            d = copy.deepcopy(self.project)
            if 'nodes' in d:
                for i in range(len(d['nodes'])):
                    d['nodes'][i] = collapse_macros(proj_dir, d['nodes'][i])
            d.setdefault('nodes', [])

            unfolds = []
            self.enum_all_getfolds(unfolds)
            unfolds = list(map(lambda x: collapse_macros(proj_dir, x), unfolds))
            d['unfold'] = unfolds
            d['version'] = PROJECT_FORMAT_VERSION

            # 'sessions' and 'last_session' in the in-memory project can be
            # STALE: CudaText writes session sub-keys straight into the
            # project file on disk (PROC_SAVE_SESSION on session switches,
            # the ~40 s session autosave, the save on exit), and those
            # writes don't pass through this dict. Dumping the dict here
            # would revert every session except the current one to an old
            # state — silent data loss (text typed in a session before
            # switching away from it reappeared as the old text after a
            # CudaText restart). Always take the fresh values from the
            # file on disk; the in-memory copy is only the fallback for a
            # first-ever save. For Save-As to a new path, the sessions of
            # the OLD project file are carried over to the new file.
            old_path = str(self.project_file_path) if self.project_file_path else ''
            target = str(path)
            carry_name = ''
            if old_path and os.path.normpath(old_path) != os.path.normpath(target):
                src = old_path      # Save As: carry the open project's sessions
                # Save the active session into the OLD project first, so it
                # doesn't lag one state behind, and remember its name — the
                # current state must land in the new file under the SAME
                # session name, not a generic 'session1'
                cur_sess = self.session_cur_name()
                if cur_sess:
                    app_proc(PROC_SAVE_SESSION, old_path + '|/sessions/' + cur_sess)
                    carry_name = cur_sess
            elif os.path.isfile(target):
                src = target        # normal save: the file being rewritten
            else:
                src = ''
            if src:
                fresh = self._read_session_keys(src)
                if 'sessions' in fresh:
                    d['sessions'] = fresh['sessions']
                if 'last_session' in fresh:
                    d['last_session'] = fresh['last_session']

            # Point the plugin to the new path BEFORE saving the session,
            # so the session is embedded into the new file, not the old one
            self.project_file_path = path
            write_json_file(path, d)

            # Save the current session into the file. This round-trips the
            # JSON file on disk, so it runs AFTER the initial write — no
            # final rewrite with stale data (previously the last write
            # clobbered the just-saved session). _set_session inside also
            # persists "last_session".
            self.session_save_as(False, sess_name=carry_name)

            # any saving of project file makes on_start On: a project is
            # in use, so the plugin must load at the next CudaText start
            # (save_options persists the on_start flag for consistency;
            # plugins.ini is what CudaText actually reads on start)
            self.options['on_start'] = True
            self.save_events()
            self.save_options()

            self.update_global_data()
            print(_('Saving project: ') + collapse_filename(str(path)))
            msg_status(_("Project saved"))

            if need_refresh:
                self.add_recent(str(path))
                self.action_refresh()
                self.save_options()

    def action_go_to_file(self):
        self.menu_goto()

    def action_project_properties(self):
        self.config_proj()

    def action_config(self):
        self.config()

    def menu_cfg(self):
        """Toolbar button "Project sessions": show the session menu."""
        if self.h_menu_cfg is None:
            self.h_menu_cfg = menu_proc(0, MENU_CREATE)
        menu_proc(self.h_menu_cfg, MENU_CLEAR)

        self.fill_sessions_menu(self.h_menu_cfg)

        menu_proc(self.h_menu_cfg, MENU_SHOW)

    def fill_sessions_menu(self, menu):
        """Fill the given menu with the project-session commands: the list
        of the project's sessions plus save/delete/default/import items.
        Used both for the toolbar button menu (menu_cfg) and for the
        "Project sessions" submenu of the panel context menu."""
        cur_name = self.session_cur_name()

        names = self.session_get_names()
        if names:
            for (index, name) in enumerate(names):
                id = menu_proc(menu, MENU_ADD,
                    command="module=cuda_project_man;cmd=session_load;info=%s;"%name,
                    caption=_('Project session:')+' '+name
                    )
                if name==cur_name:
                    menu_proc(id, MENU_SET_ENABLED, command=False)
        else:
            id = menu_proc(menu, MENU_ADD, caption=_('Project session:'))
            menu_proc(id, MENU_SET_ENABLED, command=False)

        menu_proc(menu, MENU_ADD, caption='-')

        id = menu_proc(menu, MENU_ADD, command='cuda_project_man.session_save_inplace', caption=_('Save session'))
        if (not self.project_file_path) or (not cur_name):
            menu_proc(id, MENU_SET_ENABLED, command=False)

        id = menu_proc(menu, MENU_ADD, command='cuda_project_man.session_save_as', caption=_('Save session as...'))
        if not self.project_file_path:
            menu_proc(id, MENU_SET_ENABLED, command=False)

        id = menu_proc(menu, MENU_ADD, command='cuda_project_man.session_delete', caption=_('Delete session...'))
        # a project must always keep at least one session
        if len(names) < 2:
            menu_proc(id, MENU_SET_ENABLED, command=False)

        id = menu_proc(menu, MENU_ADD, command='cuda_project_man.action_import_external_session', caption=_('Import external session...'))
        if not self.project_file_path:
            menu_proc(id, MENU_SET_ENABLED, command=False)

    def update_global_data(self):
        global global_project_info
        global_project_info['filename'] = str(self.project_file_path) if self.project_file_path else ''
        global_project_info['nodes'] = self.project.get('nodes', [])
        global_project_info['vars'] = self.project.setdefault('vars', [])
        global_project_info['mainfile'] = self.project.setdefault('mainfile', '')

    def get_info(self, index):
        if index is None:
            return
        info = tree_proc(self.tree, TREE_ITEM_GET_PROPS, index)
        if info:
            return NodeInfo(info['text'], info['icon'])

    def get_location_by_index(self, index):
        '''
        path = []
        while index and index not in self.top_nodes:
            path.append(self.get_info(index).caption)
            index = tree_proc(self.tree, TREE_ITEM_GET_PROPS, index)['parent']

        path.reverse()
        node = self.top_nodes.get(index, None)
        full_path = Path(node / str.join(os.sep, path)) if node else Path('')

        return full_path
        '''
        # same guard as get_info(): a None index (nothing selected) or an
        # unknown item must not reach tree_proc/props parsing (issue #6582
        # error class: tree_proc with a None param)
        if index is None:
            return Path('')
        p = tree_proc(self.tree, TREE_ITEM_GET_PROPS, index)
        return Path(p.get('data', '')) if p else Path('')

    def save_options(self):
        # Only save keys that exist in the current defaults dict. This
        # automatically drops stale keys that were removed from the code,
        # keeping the config file clean and in sync with the code.
        defaults = type(self).options
        d = {k: copy.deepcopy(v) for k, v in self.options.items() if k in defaults}
        d["recent_projects"] = [collapse_macros('', fn) for fn in d.get("recent_projects", [])]
        write_json_file(self.options_filename, d)

    def menu_recents(self):
        items = list(self.options["recent_projects"])

        if '' in items:
            items.remove('')
        if str(self.project_file_path) in items:
            items.remove(str(self.project_file_path))
        items = [item for item in items if os.path.isfile(item)]
        if not items:
            return

        items_nice = [os.path.basename(fn)+'\t'+collapse_filename(str(os.path.dirname(fn))) for fn in items]
        res = dlg_menu(DMENU_LIST, items_nice, caption=_('Recent projects'))
        if res is None:
            return

        self.init_panel()
        self.action_open_project(items[res])

    def do_unfold_first(self):
        """unfold 1st item under root"""
        items = tree_proc(self.tree, TREE_ITEM_ENUM_EX, 0)
        if not items:
            return
        h_first = items[0]['id']
        items = tree_proc(self.tree, TREE_ITEM_ENUM_EX, h_first)
        if not items:
            return
        tree_proc(self.tree, TREE_ITEM_UNFOLD, h_first)
        tree_proc(self.tree, TREE_ITEM_SELECT, h_first)

        path = self.get_location_by_index(self.selected)
        app_proc(PROC_SET_FOLDER, path)
        self.cur_dir = path

    def _new_project_from_dir(self, fn):
        """Create a project from folder `fn` in one step: the project file
        is created inside the folder, named after the folder (e.g. "myapp"
        -> "myapp/myapp.cuda-proj") — no second "save as" dialog. If the
        folder already contains that project file, it is simply opened
        instead."""

        if not os.path.isdir(fn):
            print(_('Project Manager: folder not found: ') + fn)
            return

        #expand "." to fully qualified name
        fn = os.path.abspath(fn)

        if is_locked(fn):
            print(_('Project Manager: folder is locked: ') + fn)
            return

        base = os.path.basename(fn.rstrip('\\/')) or 'project'
        proj = os.path.join(fn, base + PROJECT_EXTENSION)

        if os.path.isfile(proj):
            # the folder already has a project: open it instead
            self.action_open_project(proj)
            return

        if not os.access(fn, os.W_OK):
            msg_status(_('Cannot create project file in a read-only folder'))
            return

        try:
            self._create_project_file(Path(proj))
            self.add_node(fn)
            self.do_unfold_first()
        except OSError as ex:
            msg_status(_('Cannot create project file: %s') % ex)
            return

        app_proc(PROC_SIDEPANEL_ACTIVATE, self.title)
        msg_status(_('Created project: ') + os.path.basename(proj))

    def new_project_open_dir(self):
        """CudaText's "File - Open folder..." command (the core calls this
        method by name, it is not in this plugin's own menus): pick a
        folder and make a project from it in one step, see
        _new_project_from_dir."""

        fn = dlg_dir("")
        if fn is None: return

        self.init_panel()
        self._new_project_from_dir(fn)

    def open_dir(self, dirname, new_proj=False):
        """Called by CudaText itself when a folder is dragged from the OS
        file manager and dropped onto the editor (the core calls this
        method by name, it is not in this plugin's own menus). The folder
        is added as a root node of the current project; with no project
        open, of a new untitled project (it is saved to disk when the
        user saves the project). new_proj=True (not used by the current
        core) makes a new project from the folder in one step, like
        "File - Open folder..."."""

        if not os.path.isdir(dirname):
            print(_('Project Manager: folder not found: ') + dirname)
            return

        #expand "." to fully qualified name
        dirname = os.path.abspath(dirname)

        if is_locked(dirname):
            print(_('Project Manager: folder is locked: ') + dirname)
            return

        self.init_panel()
        if new_proj:
            self._new_project_from_dir(dirname)
            return

        self.add_node(dirname)
        app_proc(PROC_SIDEPANEL_ACTIVATE, self.title)

    def on_open_pre(self, ed_self, filename):
        if filename.endswith(PROJECT_EXTENSION):
            self.init_panel()
            self.action_open_project(filename)
            return False #block opening

    def on_exit(self, ed_self):
        if self.project_file_path:
            self.action_save_project_as(self.project_file_path)
        else:
            # No project is loaded at exit: the plugin must not load at
            # the next CudaText start. It would only find a non-project
            # session in history.json and detach again. Remove the on_start
            # subscription from plugins.ini [events].
            self._update_on_start_subscription(False)

    def config(self):

        from .projman_dlg import dialog_config
        if dialog_config(self.options):
            print(_('Project Manager: saving options'))
            self.save_options()

            if self.h_dlg:
                dlg_proc(self.h_dlg, DLG_CTL_PROP_SET, name='bar', prop={
                    'vis': self.options.get('toolbar', True)
                    })
                button_proc(self.h_btn_sync, BTN_SET_VISIBLE, not self.options.get('always_sync', False))

            # keep the on_start subscription in sync with the project
            # state (the hidden on_start option is not in the dialog; a
            # stale True in a hand-edited options file must not
            # re-subscribe the plugin with no project open)
            self._update_on_start_subscription(bool(self.project_file_path))

    def _update_on_start_subscription(self, loaded):
        """Keep the on_start subscription in plugins.ini [events] in sync
        with whether a project is loaded. CudaText loads this plugin at
        start ONLY to restore an open project's session (the lazy '~'
        events of install.inf still work and load it on demand), so
        plugins.ini must not keep a stale on_start key.

        The key is subscribed by every project open/create/save (they set
        options['on_start']=True and call save_events). This method is the
        single unsubscribe point, called on EVERY path that ends with no
        project: "Close project", the on_start detach branches (foreign
        or plain session restored at start), the external-session-switch
        close in on_state, and on_exit with no project. With no project
        loaded the plugin then never loads at the next CudaText start.

        The hidden on_start option is set off, which makes save_events()
        delete the plugins.ini key, and the option file records it too."""
        self.options['on_start'] = bool(loaded)
        self.save_events()
        self.save_options()

    def save_events(self):
        # The on_start event is subscribed via plugins.ini (in addition
        # to the lazy '~' events of install.inf). CudaText loads this
        # plugin on start only when on_start is subscribed here, so
        # users who don't use Project Manager get a fast start: the
        # plugin is not loaded at all.
        ev = []

        if self.options.get('on_start', False):
            ev.append('on_start')

        # save events to plugins.ini [events], they work additionally to install.inf events
        if ev:
            ini_write('plugins.ini', 'events', 'cuda_project_man', ','.join(ev))
        else:
            ini_proc(INI_DELETE_KEY, 'plugins.ini', 'events', 'cuda_project_man')

    def config_proj(self):
        if not self.tree:
            msg_status(_('No project open'))
            return

        from .projman_dlg import dialog_proj_prop
        if dialog_proj_prop(self.project):
            self.update_global_data()
            if self.project_file_path:
                self.action_save_project_as(self.project_file_path)

    def is_filename_ignored(self, fn, is_dir):
        if is_dir:
            msk = self.options.get("no_dirs", "")
        else:
            msk = self.options.get("no_files", "")
        if msk:
            return is_mask_listed(os.path.basename(fn), msk)
        else:
            return False

    def on_start(self, ed_self):
        # on_start is subscribed via plugins.ini, so it only runs when the
        # plugin was loaded on start, i.e. when a project was used on the
        # previous run. Auto-open the most recent project, but only if
        # CudaText is restoring that project's session (detected via
        # history.json). If history.json points at a different session,
        # detach silently to avoid overwriting it.
        and_activate = self.options.get("on_start_activate", False)
        self.init_panel(and_activate)

        items = self.options.get("recent_projects", [])
        if items:
            # Read history.json to see which session CudaText is loading.
            # The "session" key is "project_path|/sessions/name" for a
            # project session, or a plain filename (e.g. default.cuda-session)
            # for a non-project session.
            history_path = os.path.join(app_path(APP_DIR_SETTINGS), 'history.json')
            sess_ptr = ''
            try:
                with open(history_path, encoding='utf8') as f:
                    hist = json.load(f)
                sess_ptr = hist.get('session', '')
            except Exception:
                pass

            if sess_ptr and '|' in sess_ptr:
                proj_in_hist = sess_ptr.split('|', 1)[0]
                if os.path.normpath(proj_in_hist) == os.path.normpath(items[0]):
                    # CudaText is loading this project's session — let it
                    # proceed, just load the project metadata (no session
                    # save/close/load, which would prompt for modified tabs).
                    # CudaText restores the last used session itself; only
                    # record it in the project file as "last_session".
                    self.action_open_project(items[0], load_session=False)
                    self._remember_last_session(sess_ptr)
                else:
                    # CudaText is loading a session that belongs to a
                    # DIFFERENT project. This happens if history.json was
                    # changed manually after CudaText was closed. Close the
                    # project silently (no tab closing, no DEF_SES load)
                    # so the foreign session's tabs stay open and the
                    # project's session isn't overwritten.
                    self._close_project_silent()
                    # No project is in use now — also drop the on_start
                    # subscription, so the next start doesn't load the
                    # plugin just to detach again.
                    self._update_on_start_subscription(False)
            else:
                # No project session in history.json (either empty or a
                # plain session like default.cuda-session). Don't auto-open
                # the project — let the foreign/default session stand.
                self._close_project_silent()
                # No project is in use now — also drop the on_start
                # subscription, so the next start doesn't load the
                # plugin just to detach again.
                self._update_on_start_subscription(False)

    def enum_all(self, callback):
        """
        Enum for all items.
        Until callback gets false.
        """
        if not self.tree:
            return
        items = tree_proc(self.tree, TREE_ITEM_ENUM_EX, 0)
        if items:
            return self.enum_subitems(items[0]['id'], callback)

    def enum_subitems(self, item, callback):
        """
        Callback for all subitems of given item.
        Until callback gets false.
        """
        items = tree_proc(self.tree, TREE_ITEM_ENUM_EX, item)
        if items:
            for i in items:
                subitem = i['id']
                fn = i.get('data', '')
                if not callback(fn, subitem):
                    return False
                if not self.enum_subitems(subitem, callback):
                    return False
        return True


    def enum_all_getfolds(self, unfolds):
        # no panel yet: no unfolded items to collect
        if not self.tree:
            return
        items = tree_proc(self.tree, TREE_ITEM_ENUM_EX, 0)
        if items:
            return self.enum_subitems_getfolds(items[0]['id'], unfolds)

    def enum_subitems_getfolds(self, item, unfolds):
        items = tree_proc(self.tree, TREE_ITEM_ENUM_EX, item)
        if items:
            for i in items:
                if i['sub_items']:
                    id = i['id']
                    prop = tree_proc(self.tree, TREE_ITEM_GET_PROPS, id)
                    if not prop['folded']:
                        fn = i.get('data', '')
                        if fn:
                            unfolds.append(fn)
                    self.enum_subitems_getfolds(id, unfolds)


    def enum_all_setfolds(self, unfolds):
        if not unfolds:
            return
        if not self.tree:
            return
        items = tree_proc(self.tree, TREE_ITEM_ENUM_EX, 0)
        if items:
            return self.enum_subitems_setfolds(items[0]['id'], unfolds)

    def enum_subitems_setfolds(self, item, unfolds):
        items = tree_proc(self.tree, TREE_ITEM_ENUM_EX, item)
        if items:
            for i in items:
                if i['sub_items']:
                    id = i['id']
                    fn = i.get('data', '')
                    if fn in unfolds:
                        tree_proc(self.tree, TREE_ITEM_UNFOLD, id)
                        unfolds.remove(fn)
                    #else:
                    #    tree_proc(self.tree, TREE_ITEM_FOLD, id)
                    self.enum_subitems_setfolds(id, unfolds)


    def enum_all_fn(self, filename, and_open):
        """
        Enum for all items.
        Find 'filename', and focus its node.
        """
        if not self.tree:
            return
        items = tree_proc(self.tree, TREE_ITEM_ENUM_EX, 0)
        if items:
            return self.enum_subitems_fn(items[0]['id'], filename, and_open)

    def enum_subitems_fn(self, item_src, filename, and_open):
        """
        Callback for all subitems of given item_src.
        When found 'filename', focus it and return False
        """
        def _need(dirpath):
            return filename.startswith(dirpath+os.sep)

        prop_list = tree_proc(self.tree, TREE_ITEM_ENUM_EX, item_src) or []
        for prop in prop_list:
            fn = prop['data']
            is_dir = prop['sub_items']

            if is_dir:
                if _need(fn):
                    node = prop['id']
                    tree_proc(self.tree, TREE_ITEM_UNFOLD, node)
                    if not self.enum_subitems_fn(node, filename, and_open):
                        return False

            elif fn==filename:
                node = prop['id']
                tree_proc(self.tree, TREE_ITEM_SELECT, node)
                tree_proc(self.tree, TREE_ITEM_SHOW, node)
                if is_dir:
                    tree_proc(self.tree, TREE_ITEM_UNFOLD, node)
                if and_open:
                    _file_open(fn)
                return False

        return True


    def enum_all_sel(self, filename):
        """
        Enum for all items.
        Find 'filename', and select/show its node.
        """
        if not self.tree:
            return
        items = tree_proc(self.tree, TREE_ITEM_ENUM_EX, 0)
        if items:
            return self.enum_subitems_sel(items[0]['id'], filename)

    def enum_subitems_sel(self, item_src, filename):
        """
        Callback for all subitems of given item_src.
        When found 'filename', focus it and return False
        """

        prop_list = tree_proc(self.tree, TREE_ITEM_ENUM_EX, item_src) or []
        for prop in prop_list:
            fn = prop['data']
            is_dir = prop['sub_items']

            if is_dir:
                node = prop['id']
                if not self.enum_subitems_sel(node, filename):
                    return False

            elif fn==filename:
                node = prop['id']
                tree_proc(self.tree, TREE_ITEM_SELECT, node)
                tree_proc(self.tree, TREE_ITEM_SHOW, node)
                #print('enum_subitems_sel found node!')
                return False

        return True


    def menu_goto(self):
        """ Show menu-dialog with all files in project, and jump to chosen file """
        if not self.tree:
            msg_status(_('No project open'))
            return

        files = self.enum_all_files()
        if self.is_project_empty():
            msg_status(_('Project is empty'))
            return

        files.sort()
        for item in self.goto_history:
            if item in files:
                files.remove(item)
        files = self.goto_history + files
        files_nice = [os.path.basename(fn)+'\t'+collapse_filename(os.path.dirname(fn)) for fn in files]

        res = dlg_menu(DMENU_LIST_ALT+DMENU_NO_FULLFILTER, #fuzzy search is needed for users
                       files_nice,
                       caption=_('Go to file')
                       )
        if res is None:
            return
        fn = files[res]

        if fn in self.goto_history:
            self.goto_history.remove(fn)
        self.goto_history.insert(0, fn)

        and_open = self.options.get('goto_open', False) \
            or not app_proc(PROC_SHOW_SIDEPANEL_GET, '') \
            or app_proc(PROC_SIDEPANEL_GET, '')!='Project'
        self.jump_to_filename(fn, and_open)

    def jump_to_filename(self, filename, and_open=False):
        """ Find filename in entire project and focus its tree node """
        msg_status(_('Located in project: ') + collapse_filename(filename))
        return self.enum_all_fn(filename, and_open)

    def sync_to_ed(self):
        """ Jump to active editor file, if it's in project """
        if not self.tree:
            msg_status(_('No project open'))
            return

        fn = ed.get_prop(PROP_FN, '*')
        if fn:
            if self.jump_to_filename(fn): #gets False if found
                msg_status(_('Not found in project: ') + collapse_filename(fn))


    def tree_on_unfold(self, id_dlg, id_ctl, data='', info=''):
        info = self.get_info(data)
        path = self.get_location_by_index(data)

        if not path.is_dir():
            tree_proc(self.tree, TREE_ITEM_DELETE, data)
            if str(path) in self.project["nodes"]:
                self.project["nodes"].remove(str(path))
            return

        if info.image != self.ICON_DIR:
            return
        items = tree_proc(self.tree, TREE_ITEM_ENUM_EX, data)
        if items:
            for item in items:
                tree_proc(self.tree, TREE_ITEM_DELETE, item['id'])

        self.action_refresh_int(data) # call _int version, to avoid recursion

    def tree_on_menu(self, id_dlg, id_ctl, data='', info=''):

        self.generate_context_menu()
        menu_proc(self.h_menu, MENU_SHOW, command='')


    def do_open_current_file(self, options):

        info = self.get_info(self.selected)
        if not info:
            return
        path = self.get_location_by_index(self.selected)
        if not path:
            return

        if info.image == self.ICON_DIR:
            tree_proc(self.tree, TREE_ITEM_UNFOLD if tree_proc(self.tree, TREE_ITEM_GET_PROPS, self.selected)['folded'] else TREE_ITEM_FOLD, self.selected)
            return
        elif info.image in [self.ICON_BAD, self.ICON_PROJ]:
            return

        if not os.path.isfile(str(path)):
            tree_proc(self.tree, TREE_ITEM_SET_ICON, self.selected, image_index=self.ICON_BAD)
            return

        open_def_ext = tuple(MASKS_ZIP.split(' ') + MASKS_BINARY.split(' ') + MASKS_AUDIO.split(' ') + MASKS_VIDEO.split(' '))
        if info.caption.endswith(open_def_ext):
            self.action_open_def()
            return

        _file_open(str(path), options=options)

        # avoid stealing focus by editor
        dlg_proc(self.h_dlg, DLG_FOCUS)


    def get_open_options(self):

        s = '/preview' if self.options.get('preview', True) else ''
        s += ' /nozip /nontext-view-text'
        return s

    def tree_on_click(self, id_dlg, id_ctl, data='', info=''):

        # a drag gesture in the tree just ended on this mouse-up: LCL fires
        # OnClick even after a drag inside a control (and before OnMouseUp),
        # don't let it open the dropped-on node
        if self._tree_suppress_click:
            self._tree_suppress_click = False
            return

        # set folder in project as current folder for Open/Save-as dialogs
        node = self.selected
        if not node: # may be from some OnClick events
            return
        s = str(self.get_location_by_index(node))
        if s and not s.startswith('.'): # skip parasitic '.' for project root node
            if os.path.isdir(s):
                app_proc(PROC_SET_FOLDER, s)
                self.cur_dir = s
            elif os.path.isfile(s):
                app_proc(PROC_SET_FOLDER, os.path.dirname(s))
                self.cur_dir = os.path.dirname(s)

        if self.options.get('d_click', False):
            return
        self.do_open_current_file(self.get_open_options())

    def tree_on_click_dbl(self, id_dlg, id_ctl, data='', info=''):

        if not self.options.get('d_click', False):
            #turn off 'preview' tab kind on dbl-click
            ed.set_prop(PROP_PREVIEW, False)
            return
        self.do_open_current_file(self.get_open_options())


    def set_imagelist_size(self, theme_name, imglist):

        res = re.match(r'^\S+x(\d+)$', theme_name)
        if not res:
            return msg_box(_('Project Manager: bad icons folder name: "%s"') % theme_name, MB_OK+MB_ICONERROR)
        n = int(res.group(1))
        if not 8<=n<=64:
            return msg_box(_('Project Manager: bad icons size: "%s"') % theme_name, MB_OK+MB_ICONERROR)

        imagelist_proc(imglist, IMAGELIST_SET_SIZE, (n, n))

    def icon_init(self):

        self.icon_theme = self.options.get('icon_theme', 'vscode_16x16')
        self.set_imagelist_size(self.icon_theme, self.tree_imglist)

        self.icon_dir = os.path.join(app_path(APP_DIR_DATA), 'filetypeicons', self.icon_theme)
        if not os.path.isdir(self.icon_dir):
            self.icon_dir = os.path.join(app_path(APP_DIR_DATA), 'filetypeicons', 'vscode_16x16')

        self.icon_json = os.path.join(self.icon_dir, 'icons.json')
        self.icon_json_dict = json.loads(open(self.icon_json, encoding='utf8').read())
        self.icon_indexes = {}


    def icon_get(self, key):

        s = self.icon_indexes.get(key, None)
        if s:
            return s

        fn = self.icon_json_dict.get(key, None)
        if fn is None:
            n = self.ICON_ALL
            self.icon_indexes[key] = n
            return n

        fn = os.path.join(self.icon_dir, fn)
        n = imagelist_proc(self.tree_imglist, IMAGELIST_ADD, value=fn)
        if n is None:
            print(_('Project Manager: incorrect filetype icon:'), fn)
            n = self.ICON_ALL
        self.icon_indexes[key] = n
        return n

    def get_node_type(self):
        node_type = None
        if self.selected is not None:
            n = self.get_info(self.selected).image
            if n == self.ICON_PROJ: node_type = NODE_PROJECT
            elif n == self.ICON_DIR: node_type = NODE_DIR
            elif n == self.ICON_BAD: node_type = NODE_BAD
            else: node_type = NODE_FILE
        return node_type

    def form_key_down(self, id_dlg, id_ctl, data):
        reg_key = ('a' in data) or ('c' in data) or ('s' in data) or ('m' in data)
        if id_ctl in [VK_SPACE, VK_ENTER, VK_F4] and not reg_key:
            self.do_open_current_file(self.get_open_options())
            return False #block key
        elif id_ctl == VK_DELETE and not reg_key:
            node_type = self.get_node_type()
            if node_type == NODE_FILE:
                self.action_delete_file()
            elif node_type == NODE_DIR:
                self.action_delete_directory()
            return False #block key
        elif (id_ctl == VK_F5) or (data == S_CTRL_API and (id_ctl in (ord('r'), ord('R')))):
            self.action_refresh()
            return False #block key
        elif id_ctl == VK_F2 and not reg_key:
            self.action_rename()
            return False #block key
        elif id_ctl == VK_F7 and not reg_key:
            self.action_new_directory()
            return False #block key
        elif (data == S_CTRL_API and (id_ctl in (ord('n'), ord('N')))):
            self.action_new_file()
            return False #block key
        elif (data == S_CTRL_API and (id_ctl in (ord('x'), ord('X')))):
            self.action_cut()
            return False #block key
        elif (data == S_CTRL_API and (id_ctl in (ord('c'), ord('C')))):
            self.action_copy()
            return False #block key
        elif (data == S_CTRL_API and (id_ctl in (ord('v'), ord('V')))):
            self.action_paste()
            return False #block key
        elif (data == 'a' and id_ctl == VK_ENTER):
            self.action_get_properties()
            return False #block key

    def add_current_file(self):

        if not self.tree:
            self.init_panel(False)

        fn = ed.get_prop(PROP_FN, '*')
        self.add_node(fn)

    def add_current_dir(self):

        if not self.tree:
            self.init_panel(False)

        fn = ed.get_prop(PROP_FN, '*')
        if fn:
            d = os.path.dirname(fn)
            if not (IS_WIN and d.endswith(':\\')):
                self.add_node(d)

    def add_opened_files(self):

        if not self.tree:
            self.init_panel(False)

        for h in ed_handles():
            e = Editor(h)
            fn = e.get_prop(PROP_FN, '*')
            self.add_node(fn)

    def add_folder(self):
        """Command "Add to project: folder...": pick a folder and add it
        to the project."""
        self.init_panel()
        self.action_add_folder()

    def add_file(self):
        """Command "Add to project: file...": pick a file and add it
        to the project."""
        self.init_panel()
        self.action_add_file()

    def action_open_project_folder(self):
        """Open the folder that contains the project's .cuda-proj file in
        the OS file manager (Windows Explorer, Finder, ...). Added folders
        are not used here — they already have "Reveal in file manager"
        in their own context menu."""
        if not self.project_file_path:
            msg_status(_('No project open'))
            return

        path = str(self.project_file_path.parent)
        if not path or not os.path.isdir(path):
            msg_status(_('Project folder not found: ') + path)
            return

        suffix = OS_SUFFIX
        if suffix=='':
            #Windows
            import subprocess
            subprocess.Popen(('explorer.exe', path), shell=True)
        elif suffix=='__mac':
            #macOS
            os.system('open "%s"'%path)
        elif suffix=='__haiku':
            #Haiku
            msg_status('"Open project folder" is not implemented for this OS')
        else:
            #Linux and other Unixes
            os.spawnvp(os.P_WAIT, 'xdg-open', ('xdg-open', Path(path).as_uri()))


    def goto_main(self):
        if not self.tree:
            msg_status(_('No project open'))
            return

        fn = self.project.get('mainfile', '')
        if not fn:
            msg_status(_('Project main file is not set'))
            return
        self.jump_to_filename(fn)

    def open_main(self):
        fn = self.project.get('mainfile', '')
        if fn:
            _file_open(fn)
        else:
            msg_status(_('Project main file is not set'))

    def is_project_empty(self):
        return not bool(self.project.get('nodes'))

    def enum_all_files(self):
        files, dirs = [], []
        for root in self.project['nodes']:
            if os.path.isdir(root):
                dirs.append(root)
            elif os.path.isfile(root):
                files.append(root)

        while dirs:
            try:
                next_dir = dirs.pop(0)
                for found in os.scandir(next_dir):
                    # Ignoring symlinks prevents infinite loops with cyclic directory layouts
                    if found.is_dir() and not found.is_symlink() and not self.is_filename_ignored(found.path, True):
                        dirs.append(found.path)
                    elif found.is_file() and not self.is_filename_ignored(found.path, False):
                        files.append(found.path)
            except (OSError, FileNotFoundError):
                pass # Permissions issue. Not much we can do
        return files

    def open_all(self):
        if not self.tree:
            msg_status(_('No project open'))
            return

        files = self.enum_all_files()
        if self.is_project_empty():
            msg_status(_('Project is empty'))
            return

        if msg_box(_('Open all %d file(s) in editor?') % len(files), MB_OKCANCEL+MB_ICONQUESTION)!=ID_OK:
            return

        for (i, fn) in enumerate(files):
            _file_open(fn, options="/nontext-cancel")
            if i%10==0:
                app_proc(PROC_IDLE, False)

    def session_cur_name(self):

        fn_proj = str(self.project_file_path) if self.project_file_path else ''
        if not fn_proj:
            return ''

        s = ''
        sess = app_path(APP_FILE_SESSION)
        if '|' in sess:
            l = sess.split('|')
            if l[0] == fn_proj:
                s = l[1]
                k = '/sessions/'
                if s.startswith(k):
                    s = s[len(k):]
        return s

    def session_get_names(self):

        res = []
        fn = str(self.project_file_path) if self.project_file_path else ''
        if fn and os.path.isfile(fn):
            try:
                with open(fn, 'r', encoding='utf8') as f:
                    data = json.load(f)
                    k = data.get('sessions')
                    if type(k)==dict:
                        res = list(k.keys())
            except (OSError, ValueError):
                pass
        return res

    def session_delete(self):
        """Delete a session from the project.

        A project must always keep at least one session (a project is
        itself a session), so the last remaining session cannot be
        deleted — the menu item is disabled and the command explains why.

        When the deleted session is the active one, the user picks another
        session OF THE PROJECT to load instead. The app session never
        leaves the project for an external one (switching to e.g.
        default.cuda-session while the project stays open would let the
        session autosave overwrite that external file — data loss)."""

        names = self.session_get_names()
        if not names:
            return msg_status(_('No project sessions'))

        if len(names) == 1:
            msg_box(
                _('The project has only one session, "%s".\n\nA project always keeps at least one session, so it cannot be deleted.') % names[0],
                MB_OK + MB_ICONINFO)
            return

        res = dlg_menu(DMENU_LIST, names, caption=_('Delete project session'))
        if res is None:
            return
        name = names[res]

        fn = self.project_file_path
        if not (fn and os.path.isfile(fn)):
            return

        # deleting the ACTIVE session: let the user pick another session
        # of this project to load instead (never an external session)
        if name == self.session_cur_name():
            rest = [n for n in names if n != name]
            res2 = dlg_menu(DMENU_LIST, rest,
                caption=_('Session to load instead of "%s"') % name)
            if res2 is None:
                return
            repl = rest[res2]
            # no need to save the doomed session's state — it is deleted.
            # Tabs keep their 'modified' flag, so CudaText asks "save
            # changes?" for unsaved text — the session is deleted on
            # purpose, but files can still be saved to disk.
            self.close_all_tabs(silent=False)
            ptr = str(fn) + '|/sessions/' + repl
            self._set_session(ptr)
            app_proc(PROC_LOAD_SESSION, ptr)

        # 1. delete the session from the .cuda-proj file
        with open(fn, 'r', encoding='utf8') as f:
            data = json.load(f)
        if isinstance(data.get('sessions'), dict):
            data['sessions'].pop(name, None)
        if data.get('last_session') == name:
            rest = list((data.get('sessions') or {}).keys())
            data['last_session'] = rest[0] if rest else ''
        data.pop('def_session', None)
        write_json_file(fn, data)

        # 2. delete from the in-memory project dict too (pop, not del: the
        # in-memory copy may lag the file — a KeyError here would crash
        # the command right after the file was already updated)
        if isinstance(self.project.get('sessions'), dict):
            self.project['sessions'].pop(name, None)
        if self.project.get('last_session') == name:
            self.project['last_session'] = data.get('last_session', '')
        elif not self.project.get('last_session'):
            self.project['last_session'] = data.get('last_session', '')

        msg_status(_('Deleted session: ') + name)

    def session_save_as(self, is_request=True, sess_name=None):
        """Save the current editor state as a session of the project.

        Interactive (menu command "Save session as..."): asks for the new
        session's name. The state is written BOTH to the new session and
        to the currently active session — the changes were made while it
        was active, so they belong to it (switching sessions always saves
        the active one too). No tabs are closed: the full current state
        (including tabs not belonging to the project) is saved as is.

        Non-interactive (called after saving the project file): saves the
        current state into the current session slot, or 'session1' if
        none is active. `sess_name` (used by "Save project as...") keeps
        the active session's NAME in the new file instead of the generic
        'session1' — the session keeps its identity across the save-as."""

        fn = str(self.project_file_path) if self.project_file_path else ''
        if not fn:
            msg_status(_('Untitled project'))
            return

        names = self.session_get_names()
        if is_request:
            s = 'new'
            while True:
                s = dlg_input(_('Save session with name:'), s)
                if s is None:
                    return
                s = s.strip()
                if not s:
                    msg_status(_('Empty session name'))
                    continue
                if not is_session_name(s):
                    msg_status(_('Not allowed char(s) in the session name'))
                    continue
                if s in names:
                    msg_status(_('Session "%s" already exists')%s)
                    continue
                break
        else:
            s = sess_name or self.session_cur_name() or 'session1'

        # the changes were made while the current session was active:
        # save them into it as well, before switching to the new one
        cur_sess = self.session_cur_name()
        if is_request and cur_sess and cur_sess != s:
            app_proc(PROC_SAVE_SESSION, fn + '|/sessions/' + cur_sess)

        sess = fn+'|/sessions/'+s
        app_proc(PROC_SAVE_SESSION, sess)
        self._set_session(sess)

        # the app wrote the session straight into the project file;
        # refresh the in-memory copy (see _sync_sessions_from_disk)
        self._sync_sessions_from_disk()

    def session_load_menu(self):

        names = self.session_get_names()
        res = dlg_menu(DMENU_LIST, names, caption=_('Open project session'))
        if res is None:
            return
        self.session_load(names[res])

    def session_load(self, name=''):
        """Load another session of the current project.

        The active session is ALWAYS saved into its slot first — changes
        are never lost on switching, and no prompt is shown: CudaText's
        session autosave writes the state out every ~40 seconds anyway,
        so asking adds nothing (the only thing the "No" answer did was
        create a state the autosave would overwrite moments later)."""

        if not name:
            return

        if not name in self.session_get_names():
            msg_status(_('Project session "%s" not found')%name)
            return

        fn = str(self.project_file_path) if self.project_file_path else ''
        if not fn:
            msg_status(_('Untitled project'))
            return

        # always save the current session before switching (or RE-loading
        # the same one: its unsaved text goes into the slot and is restored
        # from there — skipping the save discarded it silently)
        cur_sess = self.session_cur_name()
        saved = False
        if cur_sess:
            saved = bool(app_proc(PROC_SAVE_SESSION, fn + '|/sessions/' + cur_sess))
            # the app wrote the session straight into the project file;
            # refresh the in-memory copy (see _sync_sessions_from_disk)
            self._sync_sessions_from_disk()

        # Close ALL tabs in ALL groups, without switching the session
        # pointer to DEF_SES — we're switching between sessions within
        # the same project, not leaving the project. When the slot save
        # above failed, tabs keep their 'modified' flag so CudaText asks
        # "save changes?" — unsaved text is never discarded silently.
        self.close_all_tabs(silent=saved)

        fn += '|/sessions/'+name
        self._set_session(fn)
        app_proc(PROC_LOAD_SESSION, fn)

    def action_import_external_session(self):
        """Import an external .cuda-session file into the current project
        as an internal session. Migration helper for the old (pre-
        versioning) workflow which allowed external session files to be
        used with projects."""

        if not self.project_file_path:
            msg_status(_('No project open'))
            return

        fn = dlg_file(True, "", "", _("CudaText sessions") + "|*.cuda-session")
        if not fn:
            return

        try:
            with open(fn, encoding='utf8') as f:
                ses_data = json.load(f)
        except (OSError, ValueError) as ex:
            msg_box(_('Cannot read session file:\n%s\n\n%s') % (collapse_filename(fn), ex),
                MB_OK + MB_ICONERROR)
            return

        if not isinstance(ses_data, dict):
            msg_box(_('File is not a valid CudaText session:\n%s') % collapse_filename(fn),
                MB_OK + MB_ICONERROR)
            return

        # session name from the file name, sanitized and made unique
        base = os.path.splitext(os.path.basename(fn))[0]
        allowed = string.ascii_letters + string.digits + '., ()-+_$%='
        base = ''.join(ch if ch in allowed else '_' for ch in base) or 'imported'
        names = self.session_get_names()
        name = base
        n = 2
        while name in names:
            name = '%s_%d' % (base, n)
            n += 1

        # store the session inside the project file
        fn_proj = str(self.project_file_path)
        try:
            with open(fn_proj, 'r', encoding='utf8') as f:
                data = json.load(f)
        except (OSError, ValueError) as ex:
            msg_box(_('Cannot read project file:\n%s\n\n%s') % (collapse_filename(fn_proj), ex),
                MB_OK + MB_ICONERROR)
            return

        data.setdefault('sessions', {})
        data['sessions'][name] = ses_data
        data.pop('def_session', None)
        if data.get('last_session', '') not in data['sessions']:
            data['last_session'] = name

        try:
            write_json_file(fn_proj, data)
        except OSError as ex:
            msg_box(_('Cannot write project file:\n%s\n\n%s') % (collapse_filename(fn_proj), ex),
                MB_OK + MB_ICONERROR)
            return

        # update the in-memory project
        self.project['sessions'] = data['sessions']
        self.project['last_session'] = data['last_session']

        msg_box(
            _('Imported session "%s" into the project.\n\nLoad it when needed via the "Project sessions" menu.') % name,
            MB_OK + MB_ICONINFO)

    def close_foreign_tabs(self, confirm=True):
        """Close editor tabs that don't belong to the current project.

        confirm=True (menu command "Close tabs not belonging to project"):
        asks once whether to close the foreign tabs, then closes them.
        PROP_MODIFIED is not cleared, so tabs with unsaved changes get
        CudaText's own "save changes?" prompt — nothing is lost silently.

        confirm=False (menu command "... (no prompt)" and internal callers
        like session saving): all foreign tabs are closed at once, without
        any dialogs — unsaved changes in them are discarded."""

        # don't detect if project is empty
        items = self.project['nodes']
        if not items:
            return

        files, dirs = [], []
        for root in items:
            if os.path.isdir(root):
                dirs.append(root)
            else:
                files.append(root)

        def is_from_proj(fn):
            if not fn: # untitled tabs
                return False
            for d in dirs:
                if fn.startswith(d+os.sep):
                    return True
            for f in files:
                if f==fn:
                    return True
            return False

        import cudatext_cmd as cmds

        to_close = []
        for h in ed_handles():
            e = Editor(h)
            if not is_from_proj(e.get_prop(PROP_FN, '*')):
                to_close.append(e)

        if to_close:
            msg = _('CudaText has opened %d tab(s) not belonging to the project "%s". Close them?') \
                % (len(to_close), self.get_project_name())

            if not confirm or msg_box(msg, MB_OKCANCEL+MB_ICONQUESTION)==ID_OK:
                for e in reversed(to_close):
                    if not confirm:
                        e.set_prop(PROP_MODIFIED, False)
                    e.cmd(cmds.cmd_FileClose)
                    time.sleep(0.2)

    def close_foreign_tabs_force(self):
        """Menu command "Close tabs not belonging to project (no prompt)":
        closes all foreign tabs without any dialogs, discarding unsaved
        changes (the old behavior of the main command)."""
        self.close_foreign_tabs(confirm=False)

    def close_all_tabs(self, silent=True):
        """Close all tabs in all groups without changing the session pointer.

        silent=True: clears PROP_MODIFIED and unpins pinned tabs first, so
        no "save changes?" prompt appears — ONLY for callers that have just
        saved the state into a session (PROC_SAVE_SESSION also stores the
        text of modified and untitled tabs, so a saved state loses nothing),
        or that discard the state on purpose.

        silent=False: keeps PROP_MODIFIED, so CudaText itself asks "save
        changes?" for every modified tab — used when the state could NOT be
        saved into any session; unsaved text is never discarded silently.
        Does NOT switch to DEF_SES — that's the caller's responsibility."""
        import cudatext_cmd as cmds
        for h in ed_handles():
            e = Editor(h)
            if silent:
                e.set_prop(PROP_MODIFIED, False)
            if e.get_prop(PROP_TAB_PINNED):
                e.set_prop(PROP_TAB_PINNED, False)
        ed.cmd(cmds.cmd_FileCloseAll)

    def _save_session_before_close_tabs(self):
        """Best-effort save of the current editor state into the session it
        belongs to, right before close_all_tabs() closes the tabs — the
        state (INCLUDING the text of modified and untitled tabs, which the
        app embeds into the session) must be stored somewhere before the
        buffers are gone.

        - While a project session is active: the project's session slot.
        - Otherwise (no project open, e.g. about to open/create a project):
          the app's current session file (typically default.cuda-session) —
          the very file CudaText's ~40 s session autosave and its exit save
          write anyway, so this cannot harm an external session; with no
          project open it IS the user's working session.

        Returns True when the state was stored. Callers pass this to
        close_all_tabs(silent=...): when False, CudaText's own "save
        changes?" prompt appears for modified tabs instead."""
        fn_proj = str(self.project_file_path) if self.project_file_path else ''
        cur = self.session_cur_name()
        if fn_proj and cur:
            if bool(app_proc(PROC_SAVE_SESSION, fn_proj + '|/sessions/' + cur)):
                self._sync_sessions_from_disk()
                return True
            return False
        ptr = app_path(APP_FILE_SESSION)
        if not ptr:
            return False
        return bool(app_proc(PROC_SAVE_SESSION, ptr))

    def _close_tabs_saving_state(self):
        """Close all tabs, saving the current state into the session it
        belongs to first (see _save_session_before_close_tabs). Used by
        open/close/create-project flows, where the tabs about to be closed
        don't belong to the session that is loaded next."""
        saved = self._save_session_before_close_tabs()
        self.close_all_tabs(silent=saved)

    def on_delete_file(self, ed_self, fn):

        #print('on_delete_file', fn)
        if fn in self.project["nodes"]:
            self.project["nodes"].remove(fn)
            self.action_refresh()
            if self.project_file_path:
                self.action_save_project_as(self.project_file_path)

    def session_save_inplace(self):
        """Save current state into the current session slot (no prompt).
        Menu entry point for 'Save session'."""
        if not self.project_file_path:
            msg_status(_('No project open'))
            return
        cur_sess = self.session_cur_name()
        if not cur_sess:
            msg_status(_('No active session'))
            return
        self.session_save()
        msg_status(_('Session saved: ') + cur_sess)

    def session_save(self):
        """Save the current state into the current session slot (no prompt,
        no session switch)."""
        cur_fn = str(self.project_file_path)
        cur_sess = self.session_cur_name()
        if cur_fn and cur_sess:
            sess = cur_fn+'|/sessions/'+cur_sess
            app_proc(PROC_SAVE_SESSION, sess)
            # the app wrote the session straight into the project file;
            # refresh the in-memory copy (see _sync_sessions_from_disk)
            self._sync_sessions_from_disk()

    def is_path_in_root(self, path):

        return str(path) in self.project['nodes']

    def on_tab_change(self, ed_self):

        if self.options.get('always_sync', False):
            if self.tree:
                self.sync_to_ed()

    def on_state(self, ed_self, state):
        """Detect external session switching via app state events (issue
        #6366). If a project is open and the app's session no longer
        belongs to it, close the project silently (no tab closing, no
        DEF_SES load), so the project's session and the foreign session
        don't fight over the app's session slot.

        - APPSTATE_SESSION_SET: another plugin (e.g. a session manager)
          changed the app's session via PROC_SET_SESSION.
        - APPSTATE_SESSION_LOAD: a session was loaded that doesn't belong
          to the open project.

        Session changes made by the plugin itself are guarded with
        _session_set_by_plugin and ignored here."""
        if state in (APPSTATE_SESSION_SET, APPSTATE_SESSION_LOAD):
            if self._session_set_by_plugin:
                return
            if self.project_file_path:
                if not self.session_cur_name():
                    print(_('Project Manager: app session was changed outside the project, closing the project'))
                    self._close_project_silent()
                    # No project is in use anymore — drop the on_start
                    # subscription too, so the next CudaText start doesn't
                    # load the plugin for a project that is no longer open.
                    self._update_on_start_subscription(False)
                    msg_status(_('Project closed: another session is now active'))
