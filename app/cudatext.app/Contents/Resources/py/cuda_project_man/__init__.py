import os
import re
import collections
import json
import stat
from fnmatch import fnmatch
from pathlib import Path, PurePosixPath
from .projman_glob import *

from cudatext import *
import cudatext_cmd

from cudax_lib import get_translation
_   = get_translation(__file__)  # i18n

IS_WIN = os.name == 'nt'
PROJECT_EXTENSION = ".cuda-proj"
PROJECT_DIALOG_FILTER = _("CudaText projects|*") + PROJECT_EXTENSION
PROJECT_UNSAVED_NAME = _("(Unsaved project)")
NODE_PROJECT, NODE_DIR, NODE_FILE, NODE_BAD = range(4)
global_project_info = {}

def _file_open(fn, options=''):
    gr = ed.get_prop(PROP_INDEX_GROUP)
    #print('Opening file in group %d'%gr)
    file_open(fn, group=gr, options=options)

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
    import ctypes # import here to avoid it on Unix
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
        #except:
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

    title ="Project"    # No _() here, the translation is offered in "translation template.ini".
    menuitems = (
        #   item_caption , item_parent , item_types , item_action
        (_("New project")          , "proj", [None, NODE_PROJECT, NODE_DIR, NODE_FILE, NODE_BAD], "cuda_project_man.action_new_project"),
        (_("Open project...")      , "proj", [None, NODE_PROJECT, NODE_DIR, NODE_FILE, NODE_BAD], "cuda_project_man.action_open_project"),
        (_("Recent projects")      , "proj", [None, NODE_PROJECT, NODE_DIR, NODE_FILE, NODE_BAD], "collect_recent_projects"),
        (_("Save project as...")   , "proj", [None, NODE_PROJECT, NODE_DIR, NODE_FILE, NODE_BAD], "cuda_project_man.action_save_project_as"),

        (_("Add folder...")        , "nodes", [None, NODE_PROJECT, NODE_DIR, NODE_FILE, NODE_BAD], "cuda_project_man.action_add_folder"),
        (_("Add file...")          , "nodes", [None, NODE_PROJECT, NODE_DIR, NODE_FILE, NODE_BAD], "cuda_project_man.action_add_file"),
        (_("Clear project")        , "nodes", [None, NODE_PROJECT, NODE_DIR, NODE_FILE, NODE_BAD], "cuda_project_man.action_clear_project"),
        (_("Remove node")          , "nodes", [None, NODE_PROJECT, NODE_DIR, NODE_FILE, NODE_BAD], "cuda_project_man.action_remove_node"),

        (_("New file...")          , "dir", [NODE_DIR], "cuda_project_man.action_new_file"),
        (_("Rename...")            , "dir", [NODE_DIR], "cuda_project_man.action_rename"),
        (_("Delete directory")     , "dir", [NODE_DIR], "cuda_project_man.action_delete_directory"),
        (_("New directory...")     , "dir", [NODE_DIR], "cuda_project_man.action_new_directory"),
        (_("Find in directory...") , "dir", [NODE_DIR], "cuda_project_man.action_find_in_directory"),

        (_("Rename...")            , "file", [NODE_FILE], "cuda_project_man.action_rename"),
        (_("Delete file")          , "file", [NODE_FILE], "cuda_project_man.action_delete_file"),
        (_("Set as main file")     , "file", [NODE_FILE], "cuda_project_man.action_set_as_main_file"),

        ("-"                       , "", [None, NODE_PROJECT, NODE_DIR, NODE_FILE, NODE_BAD], ""),
        (_("Refresh")              , "", [None, NODE_PROJECT, NODE_DIR, NODE_FILE, NODE_BAD], "cuda_project_man.action_refresh"),
        ("-"                       , "", [None, NODE_PROJECT, NODE_DIR, NODE_FILE, NODE_BAD], ""),
        (_("Go to file...")        , "", [None, NODE_PROJECT, NODE_DIR, NODE_FILE, NODE_BAD], "cuda_project_man.action_go_to_file"),
        (_("Project properties..."), "", [None, NODE_PROJECT, NODE_DIR, NODE_FILE, NODE_BAD], "cuda_project_man.action_project_properties"),
        (_("Config...")            , "", [None, NODE_PROJECT, NODE_DIR, NODE_FILE, NODE_BAD], "cuda_project_man.action_config"),
    )

    options = {
        "recent_projects": [],
        "no_files": "",
        "no_dirs": ".git;.svn",
        "no_hidden": True,
        "toolbar": True,
        "preview": True,
        "d_click": False,
    }

    tree = None
    h_dlg = None
    h_menu = None

    def __init__(self):
        settings_dir = Path(app_path(APP_DIR_SETTINGS))
        self.options_filename = settings_dir / "cuda_project_man.json"
        if self.options_filename.exists():
            with self.options_filename.open(encoding='utf8') as fin:
                self.options = json.load(fin)

        self.new_project()


    def init_form_main(self):

        show_toolbar = self.options.get("toolbar", True)
        toolbar_theme = self.options.get("toolbar_theme", "default_16x16")

        self.h_dlg = dlg_proc(0, DLG_CREATE)

        dlg_proc(self.h_dlg, DLG_PROP_SET, {
            'keypreview': True,
            'on_key_down': self.form_key_down,
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
        icon_open = imagelist_proc(self.toolbar_imglist, IMAGELIST_ADD, value = os.path.join(dirname, 'open.png'))
        icon_save = imagelist_proc(self.toolbar_imglist, IMAGELIST_ADD, value = os.path.join(dirname, 'save.png'))
        icon_add_file = imagelist_proc(self.toolbar_imglist, IMAGELIST_ADD, value = os.path.join(dirname, 'add-file.png'))
        icon_add_dir = imagelist_proc(self.toolbar_imglist, IMAGELIST_ADD, value = os.path.join(dirname, 'add-dir.png'))
        icon_del = imagelist_proc(self.toolbar_imglist, IMAGELIST_ADD, value = os.path.join(dirname, 'del.png'))
        icon_cfg = imagelist_proc(self.toolbar_imglist, IMAGELIST_ADD, value = os.path.join(dirname, 'cfg.png'))

        toolbar_proc(self.h_bar, TOOLBAR_THEME)
        _toolbar_add_btn(self.h_bar, hint=_('Open project'), icon=icon_open, command='cuda_project_man.action_open_project' )
        _toolbar_add_btn(self.h_bar, hint=_('Save project as'), icon=icon_save, command='cuda_project_man.action_save_project_as' )
        _toolbar_add_btn(self.h_bar, hint='-' )
        _toolbar_add_btn(self.h_bar, hint=_('Add folder'), icon=icon_add_dir, command='cuda_project_man.action_add_folder' )
        _toolbar_add_btn(self.h_bar, hint=_('Add file'), icon=icon_add_file, command='cuda_project_man.action_add_file' )
        _toolbar_add_btn(self.h_bar, hint=_('Remove node'), icon=icon_del, command='cuda_project_man.action_remove_node' )
        _toolbar_add_btn(self.h_bar, hint='-' )
        _toolbar_add_btn(self.h_bar, hint=_('Config'), icon=icon_cfg, command='cuda_project_man.action_config' )
        toolbar_proc(self.h_bar, TOOLBAR_UPDATE)

        n = dlg_proc(self.h_dlg, DLG_CTL_ADD, prop='treeview')
        dlg_proc(self.h_dlg, DLG_CTL_PROP_SET, index=n, prop={
            'name':'tree',
            'a_t':('bar', ']'),
            'a_r':('',']'), #anchor to entire form
            'a_b':('',']'),
            'on_menu': 'cuda_project_man.tree_on_menu',
            'on_unfold': 'cuda_project_man.tree_on_unfold',
            'on_change': 'cuda_project_man.tree_on_click',
            'on_click_dbl': 'cuda_project_man.tree_on_click_dbl',
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


    def init_panel(self, and_activate=True):
        # already inited?
        if self.tree:
            return

        self.init_form_main()

        #dlg_proc(self.h_dlg, DLG_SCALE)
        tree_proc(self.tree, TREE_THEME) #TREE_THEME only after DLG_SCALE

        app_proc(PROC_SIDEPANEL_ADD_DIALOG, (self.title, self.h_dlg, 'project.png'))

        if and_activate:
            self.do_show(True)

        self.action_refresh()
        self.generate_context_menu()


    def show_panel(self):
        self.do_show(False)

    def focus_panel(self):
        self.do_show(True)

    def do_show(self, and_focus):
        if not self.tree:
            self.init_panel(True)
        else:
            ed.cmd(cudatext_cmd.cmd_ShowSidePanelAsIs)
            app_proc(PROC_SIDEPANEL_ACTIVATE, self.title)

    @property
    def selected(self):
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
            menu_dir = self.add_context_menu_node(menu_all, "0", _("Selected directory"))

        for item in self.menuitems:
            item_caption = item[0]
            item_parent = item[1]
            item_types = item[2]
            item_action = item[3]
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

            if item_action == "collect_recent_projects":
                action = ""
            else:
                action = item_action

            menu_added = self.add_context_menu_node(menu_use, action, item_caption)
            if item_action == "collect_recent_projects":
                for path in self.options["recent_projects"]:
                    if os.sep in path:
                        action = str.format("module=cuda_project_man;cmd=action_open_project;info={};", path)
                        self.add_context_menu_node(menu_added, action, nice_filename(path))

    @staticmethod
    def node_ordering(node):
        # sort folders first, then by extension
        path = Path(node)
        return path.is_file(), path.suffix.upper(), path.name.upper()

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
        self.project = dict(nodes=[])
        self.project_file_path = None
        self.update_global_data()
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

    def action_rename(self):
        location = Path(self.get_location_by_index(self.selected))
        result = dlg_input(_("Rename to"), str(location.name))
        if not result:
            return

        new_location = location.parent / result
        if location == new_location:
            return

        location.replace(new_location)
        if location in self.top_nodes.values():
            self.action_remove_node()
            self.add_node(str(new_location))

        self.action_refresh()
        self.jump_to_filename(str(new_location))
        msg_status(_("Renamed to: ") + str(new_location.name))

    def action_delete_file(self):
        location = Path(self.get_location_by_index(self.selected))
        if msg_box(_("Delete file from disk:\n") + str(location), MB_OKCANCEL + MB_ICONWARNING) != ID_OK:
            return

        location.unlink()
        if location in self.top_nodes.values():
            self.action_remove_node()
        else:
            self.action_refresh()
            self.jump_to_filename(str(location.parent))
        msg_status(_("Deleted file: ") + str(location.name))

    def do_delete_dir(self, location):
        for path in location.glob("*"):
            if path.is_file():
                path.unlink()
            elif path.is_dir():
                self.do_delete_dir(path)
        location.rmdir()

    def action_delete_directory(self):
        location = Path(self.get_location_by_index(self.selected))
        if msg_box(_("Delete directory from disk:\n") + str(location), MB_OKCANCEL + MB_ICONWARNING) != ID_OK:
            return

        self.do_delete_dir(location)
        if location in self.top_nodes.values():
            self.action_remove_node()
        else:
            self.action_refresh()
            self.jump_to_filename(str(location.parent))
        msg_status(_("Deleted dir: ") + str(location.name))

    def action_new_directory(self):
        location = Path(self.get_location_by_index(self.selected))
        if location.is_file():
            location = location.parent
        result = dlg_input(_("New directory"), "")
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
                msg_box(_('Plugin "Find in Files" or "Find in Files 4" is not installed, install it first'), MB_OK + MB_ICONERROR)


    def action_refresh(self, parent=None):
        unfold = parent is None
        if parent is None:
            # clear tree
            tree_proc(self.tree, TREE_ITEM_DELETE, 0)

            if self.project_file_path is None:
                project_name = PROJECT_UNSAVED_NAME
            else:
                project_name = self.project_file_path.stem

            parent = tree_proc(
                self.tree,
                TREE_ITEM_ADD,
                0,
                -1,
                project_name,
                self.ICON_PROJ,
            )

            #select 1st node
            items_root = tree_proc(self.tree, TREE_ITEM_ENUM, 0)
            tree_proc(self.tree, TREE_ITEM_SELECT, items_root[0][0])

            nodes = self.project["nodes"]
            self.top_nodes = {}
        else:
            fn = self.get_location_by_index(parent)
            if not fn: return
            #print('Reading dir:', fn)
            try:
                nodes = sorted(Path(fn).iterdir(), key=Command.node_ordering)
            except:
                tree_proc(self.tree, TREE_ITEM_SET_ICON, parent, image_index=self.ICON_BAD)
                return

        for path in map(Path, nodes):
            spath = str(path)
            sname = path.name
            if is_win_root(spath):
                sname = spath
            elif self.options.get("no_hidden", True) and is_hidden(spath):
                continue
            elif self.is_filename_ignored(spath):
                continue

            if is_locked(spath):
                imageindex = self.ICON_BAD
            elif path.is_dir():
                imageindex = self.ICON_DIR
            elif is_simple_listed(path.name, MASKS_IMAGES):
                imageindex = self.ICON_IMG
            elif is_simple_listed(path.name, MASKS_ZIP):
                imageindex = self.ICON_ZIP
            elif is_simple_listed(path.name, MASKS_BINARY):
                imageindex = self.ICON_BIN
            else:
                lexname = lexer_proc(LEXER_DETECT, path.name)
                if lexname:
                    imageindex = self.icon_get(lexname)
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
            if nodes is self.project["nodes"]:
                self.top_nodes[index] = path

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

    def action_new_project(self):
        self.new_project()
        self.action_refresh()

    def action_open_project(self, info=None):
        path = info
        if path is None:
            path = dlg_file(True, "", "", PROJECT_DIALOG_FILTER)
        if path:
            if Path(path).exists():
                print(_('Loading project: ') + collapse_filename(path))
                with open(path, encoding='utf8') as fin:
                    self.project = json.load(fin)
                    self.project_file_path = Path(path)
                    self.add_recent(path)
                    self.action_refresh()
                    self.save_options()

                self.update_global_data()

                for fn in self.project["nodes"]:
                    if os.path.isdir(fn):
                        app_proc(PROC_SET_FOLDER, fn)
                        break

                app_proc(PROC_SET_PROJECT, path)
                msg_status(_("Project opened: ") + path)
            else:
                msg_status(_("Project filename is not found: ") + path)

    def action_add_folder(self):
        fn = dlg_dir("")
        self.add_node(fn)

    def action_add_file(self):
        fn = dlg_file(True, "", "", "")
        self.add_node(fn)

    def action_remove_node(self):
        index = self.selected
        while index and index not in self.top_nodes:
            index = tree_proc(self.tree, TREE_ITEM_GET_PROPS, index)["parent"]

        if index in self.top_nodes:
            tree_proc(self.tree, TREE_ITEM_DELETE, index)
            path = self.top_nodes.pop(index)
            if str(path) in self.project["nodes"]:
                self.project["nodes"].remove(str(path))
            if self.project_file_path:
                self.action_save_project_as(self.project_file_path)

    def action_clear_project(self):
        self.project["nodes"].clear()
        self.action_refresh()

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
            path = Path(path)
            if path.suffix != PROJECT_EXTENSION:
                path = path.parent / (path.name + PROJECT_EXTENSION)

            self.project_file_path = path
            with path.open("w", encoding='utf8') as fout:
                json.dump(self.project, fout, indent=4)

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

    def update_global_data(self):
        global global_project_info
        global_project_info['filename'] = str(self.project_file_path) if self.project_file_path else ''
        global_project_info['nodes'] = self.project['nodes']
        global_project_info['vars'] = self.project.get('vars', [])
        global_project_info['mainfile'] = self.project.get('mainfile', '')

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
        p = tree_proc(self.tree, TREE_ITEM_GET_PROPS, index)
        return Path(p.get('data', ''))

    def save_options(self):
        with self.options_filename.open(mode="w", encoding='utf8') as fout:
            json.dump(self.options, fout, indent=4)

    def menu_recents(self):
        items = self.options["recent_projects"]
        if not items:
            return

        items_nice = [os.path.basename(fn)+'\t'+os.path.dirname(fn) for fn in items]
        res = dlg_menu(DMENU_LIST, items_nice, caption=_('Recent projects'))
        if res is None:
            return

        self.init_panel()
        self.action_open_project(items[res])

    def do_unfold_first(self):
        """unfold 1st item under root"""
        items = tree_proc(self.tree, TREE_ITEM_ENUM, 0)
        if not items:
            return
        items = tree_proc(self.tree, TREE_ITEM_ENUM, items[0][0])
        if not items:
            return
        tree_proc(self.tree, TREE_ITEM_UNFOLD, items[0][0])
        tree_proc(self.tree, TREE_ITEM_SELECT, items[0][0])

    def new_project_open_dir(self):

        fn = dlg_dir("")
        if fn is None: return

        if is_locked(fn):
            print(_('Project Manager: folder is locked: ') + fn)
            return

        self.init_panel()
        self.action_new_project()
        self.add_node(fn)
        self.do_unfold_first()
        app_proc(PROC_SIDEPANEL_ACTIVATE, self.title)

    def open_dir(self, dirname, new_proj=False):

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
            self.action_new_project()
        self.add_node(dirname)
        if new_proj:
            self.do_unfold_first()

        app_proc(PROC_SIDEPANEL_ACTIVATE, self.title)

    def on_open_pre(self, ed_self, filename):
        if filename.endswith(PROJECT_EXTENSION):
            self.init_panel()
            self.action_open_project(filename)
            return False #block opening

    def config(self):

        from .projman_dlg import dialog_config
        if dialog_config(self.options):
            print(_('ProjectManager: saving options'))
            self.save_options()

            if self.h_dlg:
                dlg_proc(self.h_dlg, DLG_CTL_PROP_SET, name='bar', prop={
                    'vis': self.options.get('toolbar', True)
                    })

            ev = []
            if self.options['on_start']:
                ev += ['on_start']
            if self.options['check_git']:
                ev += ['on_open']
            if ev:
                ini_write('plugins.ini', 'events', 'cuda_project_man', ','.join(ev))
            else:
                ini_proc(INI_DELETE_KEY, 'plugins.ini', 'events', 'cuda_project_man')

    def config_proj(self):
        if not self.tree:
            msg_status(_('Project not loaded'))
            return

        from .projman_dlg import dialog_proj_prop
        if dialog_proj_prop(self.project):
            self.update_global_data()
            if self.project_file_path:
                self.action_save_project_as(self.project_file_path)

    def is_filename_ignored(self, fn):
        if os.path.isdir(fn):
            msk = self.options.get("no_dirs", "")
        else:
            msk = self.options.get("no_files", "")
        if msk:
            return is_mask_listed(os.path.basename(fn), msk)
        else:
            return False

    def on_start(self, ed_self):
        and_activate = self.options.get("on_start_activate", False)
        self.init_panel(and_activate)

        items = self.options.get("recent_projects", [])
        if items:
            self.action_open_project(items[0])

    def contextmenu_add_dir(self):
        self.init_panel()
        self.action_add_folder()

    def contextmenu_add_file(self):
        self.init_panel()
        self.action_add_file()

    def contextmenu_new_proj(self):
        self.init_panel()
        self.action_new_project()

    def contextmenu_open_proj(self):
        self.init_panel()
        self.action_open_project()

    def contextmenu_save_proj_as(self):
        self.init_panel()
        self.action_save_project_as()

    def contextmenu_refresh(self):
        self.init_panel()
        self.action_refresh()

    def contextmenu_remove_node(self):
        self.init_panel()
        self.action_remove_node()

    def contextmenu_clear_proj(self):
        self.init_panel()
        self.action_clear_project()

    def contextmenu_set_as_main_file(self):
        self.init_panel()
        self.action_set_as_main_file()

    def enum_all(self, callback):
        """
        Callback for all items.
        Until callback gets false.
        """
        items = tree_proc(self.tree, TREE_ITEM_ENUM, 0)
        if items:
            return self.enum_subitems(items[0][0], callback)

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

    def enum_all_fn(self, filename, and_open):
        """
        Callback for all items.
        Find 'filename', and focus its node.
        """
        items = tree_proc(self.tree, TREE_ITEM_ENUM, 0)
        if items:
            return self.enum_subitems_fn(items[0][0], filename, and_open)

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

    def menu_goto(self):
        """ Show menu-dialog with all files in project, and jump to chosen file """
        if not self.tree:
            msg_status(_('Project not opened'))
            return

        files = self.enum_all_files()
        if not files:
            msg_status(_('Project is empty'))
            return

        files.sort()
        files_nice = [os.path.basename(fn)+'\t'+collapse_filename(os.path.dirname(fn)) for fn in files]

        res = dlg_menu(DMENU_LIST_ALT+DMENU_NO_FULLFILTER, #fuzzy search is needed for users
                       files_nice,
                       caption=_('Go to file')
                       )
        if res is None:
            return

        and_open = self.options.get('goto_open', False)
        self.jump_to_filename(files[res], and_open)

    def jump_to_filename(self, filename, and_open=False):
        """ Find filename in entire project and focus its tree node """
        msg_status(_('Jumping to: ') + filename)
        return self.enum_all_fn(filename, and_open)

    def sync_to_ed(self):
        """ Jump to active editor file, if it's in project """
        if not self.tree:
            msg_status(_('Project not loaded'))
            return

        fn = ed.get_filename()
        if fn:
            if self.jump_to_filename(fn): #gets False if found
                msg_status(_('Cannot jump to file: ') + fn)


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
        items = tree_proc(self.tree, TREE_ITEM_ENUM, data)
        if items:
            for handle, _ in items:
                tree_proc(self.tree, TREE_ITEM_DELETE, handle)
        self.action_refresh(data)

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

        if info.image in [self.ICON_BAD, self.ICON_DIR, self.ICON_PROJ]:
            return

        if not os.path.isfile(str(path)):
            tree_proc(self.tree, TREE_ITEM_SET_ICON, self.selected, image_index=self.ICON_BAD)
            return

        _file_open(str(path), options=options)


    def get_open_options(self):

        s = '/preview' if self.options.get('preview', True) else ''
        s += ' /nozip /nontext-view-text'
        return s

    def tree_on_click(self, id_dlg, id_ctl, data='', info=''):

        # set folder in project as current folder for Open/Save-as dialogs
        node = self.selected
        if not node: # may be from some OnClick events
            return
        s = str(self.get_location_by_index(node))
        if s and not s.startswith('.'): # skip parasitic '.' for project root node
            if os.path.isdir(s):
                app_proc(PROC_SET_FOLDER, s)
            elif os.path.isfile(s):
                app_proc(PROC_SET_FOLDER, os.path.dirname(s))

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

        res = re.match('^\S+x(\d+)$', theme_name)
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
        self.icon_json_dict = json.loads(open(self.icon_json).read())
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
            print(_('ProjectManager: incorrect filetype icon:'), fn)
            n = self.ICON_ALL
        self.icon_indexes[key] = n
        return n

    def form_key_down(self, id_dlg, id_ctl, data):

        if id_ctl==13: #Enter
            self.do_open_current_file(self.get_open_options())
            return False #block key

    def add_current_file(self):

        if not self.tree:
            self.init_panel(False)

        fn = ed.get_filename()
        self.add_node(fn)

    def add_opened_files(self):

        if not self.tree:
            self.init_panel(False)

        for h in ed_handles():
            e = Editor(h)
            fn = e.get_filename()
            self.add_node(fn)


    def goto_main(self):
        if not self.tree:
            msg_status(_('Project not opened'))
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

    def enum_all_files(self):

        import glob
        files = []

        for root in self.project['nodes']:
            if os.path.isdir(root):
                f = glob.glob(os.path.join(root, '**', '*'), recursive=True)
                f = [fn for fn in f if os.path.isfile(fn)]
                files.extend(f)
            elif os.path.isfile(root):
                files.append(root)

        return files

    def open_all(self):
        if not self.tree:
            msg_status(_('Project not opened'))
            return

        files = self.enum_all_files()
        if not files:
            msg_status(_('Project is empty'))
            return

        if msg_box(_('Open all %d file(s) in editor?') % len(files), MB_OKCANCEL+MB_ICONQUESTION)!=ID_OK:
            return

        for (i, fn) in enumerate(files):
            _file_open(fn, options="/nontext-cancel")
            if i%10==0:
                app_idle(False)

    def on_open(self, ed_self):

        self.init_panel(False)

        if not self.project_file_path:
            self.action_project_for_git(ed_self.get_filename('*'))

    def action_project_for_git(self, filename):

        dir = os.path.dirname(filename)
        while True:
            fn = os.path.join(dir, '.git')
            fn2 = os.path.join(dir, '.svn')
            if os.path.isdir(fn) or os.path.isdir(fn2):
                self.init_panel()
                self.new_project()
                self.add_node(dir)
                self.jump_to_filename(filename)
                return

            d = os.path.dirname(dir)
            if d=='/':
                return
            if d==dir:
                return
            dir = d
