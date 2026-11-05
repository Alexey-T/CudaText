# cuda_project_man — documentation for developers

This document is for plugin authors/maintainers who want to change this
plugin. The end-user documentation is `readme/readme.txt` — this file
describes the internals: the design concepts, the invariants that MUST
hold, and the traps that have already caused data-loss bugs (every one
of the rules below exists because its violation lost somebody's data).

Reference discussions: CudaText issues #6352 (project/session redesign)
and #6366 (external session switch detection).

---
## Contents

1. Core concepts
2. The session pointer system
3. Who writes the .cuda-proj file (the #1 trap)
4. Invariants (checklist before merging any change)
5. Tab closing rules
6. Startup / shutdown flow
7. External session switches
8. Project file format and versioning
9. Options, events, install.inf
10. API for other plugins
11. Writing files
12. Known non-obvious behaviors
13. Drag & drop

---
## 1. Core concepts

**A project is a session by itself.**
This is the central idea of the redesign (#6352): a `.cuda-proj` file is
not only a list of folders — it *contains* the project's sessions under
the `sessions` key. A project without sessions cannot exist:

- opening a session-less project file auto-creates `session1`;
- the last remaining session cannot be deleted (menu item disabled,
  command explains it in a dialog);
- at startup, if CudaText restores a project's session from a file that
  somehow has no `sessions` key, `session1` is created from the restored
  state.

Consequence: while a project is open, the *app's session* is always one
of the project's internal sessions. The app session never "leaks" onto
an external session file.

**`last_session`, not "default session".**
The project file stores the name of the session that was used last. It
is updated on every session switch (`Command._remember_last_session`)
and used when the project is opened. There is deliberately no
"user-chosen default session" anymore — that concept (the removed
`def_session` key) fought with both the save flow and CudaText's own
restore; old files carrying `def_session` are migrated on open.

**The tree shows roots only.**
`nodes` holds the top-level paths (roots). Folder contents are
enumerated on demand when a node is expanded (`tree_on_unfold` →
`action_refresh_int`). Project files stay small even for huge
codebases. "Stop tracking" always walks up to the root entry.

**In-memory project vs file on disk.**
`Command.project` is the in-memory dict (nodes, vars, mainfile, unfold,
plus a *cached copy* of sessions/last_session). `Command.project_file_path`
is a `Path` or `None` (no project open). `global_project_info` is the
module-level export for other plugins (see §10).

**Why an in-memory copy exists at all** (a fair question — keeping a
second copy of data that lives in a file is what once caused a data
loss, see §3): the tree, menus, node add/remove and macros all need
project data without re-reading and re-parsing the file on every click,
and CudaText offers no API to enumerate the sessions inside a project
file — the plugin must know the session names itself. The copy is a
*cache*: correct for everything CudaText does not write (nodes, vars,
mainfile, unfold), but only a cache for `sessions`/`last_session`,
which the app writes behind the plugin's back (see §3).

---
## 2. The session pointer system

CudaText addresses sessions with pointer strings:

    <path-to-file>|/sessions/<name>     internal (project) session
    <path-to-file>                      external session file,
                                        e.g. settings/default.cuda-session

Relevant API calls:

- `app_proc(PROC_SET_SESSION, ptr)` — make `ptr` the app's current
  session (subsequent autosaves write into it);
- `app_proc(PROC_SAVE_SESSION, ptr)` — write the current editor state
  into `ptr` (the app does a read-modify-write of the *file on disk*;
  for a project pointer it updates only that session's sub-key);
- `app_proc(PROC_LOAD_SESSION, ptr)` — load tabs from `ptr`;
- `app_path(APP_FILE_SESSION)` — the app's current session pointer.

Plugin rules:

- **Never call `PROC_SET_SESSION` directly.** Always go through
  `Command._set_session(ptr)`: it sets the guard flag
  `_session_set_by_plugin` (so `on_state` does not treat our own change
  as an external switch, see §7) and records project sessions as
  `last_session`.
- The **only** allowed external pointer set is `_set_session(DEF_SES)`
  inside `action_close_project`, and it runs strictly AFTER
  `_close_project_silent()` has forgotten the project.
- `session_cur_name()` returns the active internal session name, or
  `''` when the app session is not inside the open project file.
- Ordering: always `_set_session(ptr)` **before**
  `app_proc(PROC_LOAD_SESSION, ptr)`. `PROC_LOAD_SESSION` fires
  `APPSTATE_SESSION_LOAD`, and the `on_state` handler must already see
  the pointer inside the project, otherwise it closes the project.

---
## 3. Who writes the .cuda-proj file (the #1 trap)

The `.cuda-proj` file has TWO writers, and only one of them is this
plugin:

1. **CudaText itself** writes session sub-keys straight into the file:
   on every `PROC_SAVE_SESSION`, on the ~40-second session autosave,
   and on exit (after plugin `on_exit` handlers have run). These writes
   update `sessions.<name>` (and nothing else) via the app's own
   read-modify-write. They do not pass through the plugin and never
   update `Command.project`.
2. **The plugin** writes the whole file in `action_save_project_as`
   (also used for in-place saves), plus read-modify-write helpers
   (`_remember_last_session`, `session_delete`,
   `action_import_external_session`, `upgrade_project_file_on_disk`,
   `_create_project_file`).

This two-writer setup caused the worst data-loss bug this plugin ever
had (fixed 2026.10, see history.txt). To understand it, remember that
the plugin used to dump its **whole in-memory project dict** — with the
`sessions` it had read when the project was *opened* — into the file on
every save, including the automatic save on exit. The victim's script:

1. Open a project. The plugin caches `sessions` — the state of every
   session **at this moment**.
2. Work in session "A": type text, don't save. Switch to session "B".
   CudaText writes the current state of "A" into the project file
   (as it does on every switch, autosave and exit).
3. Close CudaText. The plugin's exit save writes its cached copy —
   the one from step 1 — over the file. The text typed in "A" in
   step 2 is gone; switching back to "A" shows its old content.
   No error message anywhere — silent, and it also fired on every
   ordinary project save, not just on exit.

Every session except the currently active one was reverted to its
step-1 state this way. Two rules prevent any recurrence:

- **`action_save_project_as` always takes `sessions`/`last_session`
  fresh from the file on disk** (`_read_session_keys`), read AFTER the
  last app call in the function (the tree enumeration) and immediately
  before the write. The in-memory copy is only the fallback for a
  first-ever save. For Save-As to a new path, the fresh values come
  from the OLD file (carry-over), after the active session was saved
  into it once more.
- **After every `PROC_SAVE_SESSION` the plugin performs, call
  `_sync_sessions_from_disk()`** so the cache can never regress.

If you add any code that writes the project file, it must either go
through those helpers or re-read the session keys first.

Also note: a failed session save does not raise into the plugin — the
app swallows write errors and `PROC_SAVE_SESSION` merely returns False.
Check the return value when the next step closes tabs (see §5).

---
## 4. Invariants (checklist before merging any change)

1. While a project is open, the app session pointer is INSIDE that
   project's file (or the project is being closed: pointer switched to
   DEF_SES only after the project is forgotten).
2. A project always keeps at least one session (`sessions` never
   becomes empty).
3. `last_session` is only ever set to a session that exists in the same
   project file; `_remember_last_session` ignores external pointers and
   also strips the legacy `def_session` key.
4. The plugin never writes an external session file while a project is
   open. The only external write is in
   `_save_session_before_close_tabs`, which runs when NO project
   session is active and saves the app's own current session — the very
   file the app's autosave/exit write anyway.
5. Nothing closes tabs silently unless the state was just saved into a
   session, or the discard is the user's explicit intent (§5).
6. `sessions`/`last_session` written to disk are always the fresh
   on-disk values (§3).
7. Project files are written atomically (§11); a crash must never leave
   a truncated file.
8. Opening a project never closes tabs before the file passed
   validation (`load_project_file`) — a broken file must leave the
   editor state untouched.
9. Old-format files are upgraded losslessly: only missing standard keys
   are added, a backup is kept next to the file (never overwritten),
   and failure to write never blocks opening.
10. Version check: files with `"version"` newer than
    `PROJECT_FORMAT_VERSION` are refused with an explanation.
11. A public `Command` method is never removed as "unused" without
    checking the CudaText core first: some methods have no install.inf
    entry and no caller inside this plugin, yet the core calls them by
    name (see §9). This exact mistake once removed `open_dir()` and
    broke drag&drop of folders onto the editor.
12. Drag&drop never deletes or overwrites anything without the checks
    in `_drop_kind`/`_move_node_on_disk` (no overwrite, no move of a
    folder into itself / its own subfolder, root nodes are only
    reordered, never relocated). See §13.

---
## 5. Tab closing rules

Two closing regimes exist.

**Regime A — the session load closes the tabs (all switch flows).**
`PROC_LOAD_SESSION` itself closes every ui-tab (all groups, pinned
included — its `DoFileCloseAll(true, true)` runs with
`AppSessionIsClosing`, which suppresses the "Close pinned tab?"
confirm) BEFORE restoring the target session's tabs. The closes fire
`on_close_pre`/`on_close` under `APPSTATE_SESSION_LOAD_BEGIN_PRE`
(fired before everything else a load does, issue #6496), so plugins
can tell them from user closes: cuda_differ (Differ 2) keeps the
closing tabs' per-session registrations in `cuda_differ2_state.json`
this way and re-attaches the tabs (toolbar, overview, title color)
when their session is loaded again. Flows using this regime:
`action_open_project` (target has sessions), `action_close_project`
(default session file exists), `_create_project_file(close_tabs=True)`
(loads the not-yet-existing `session1`: the load closes the old tabs
and restores none), `session_load`. Before the load, the caller must:

1. save the state into the session it belongs to
   (`_save_session_before_close_tabs()` — see below), and
2. call `_clear_modified_all_tabs()` when that save succeeded: the
   app's load first runs `ConfirmAllFramesAreSaved`, which asks
   "Save modified tab?" for every tab still marked modified, and a
   **Cancel there aborts the WHOLE load** (before any tab is closed —
   the confirm is a pre-pass). When the save failed, keep the flags:
   the load then asks, and unsaved text is never discarded silently.

Every switch load runs through `_load_session_repairing_abort(ptr_new,
ptr_old, msg)`. On the cancelled shape (tabs still open) it re-points
the app session to `ptr_old` — the session owning the still-open tabs,
captured before the flow re-pointed it (`_current_state_session_ptr()`) —
so the ~40 s session autosave / the exit save write them into their
owner's slot instead of clobbering the not-loaded target session (the
cancel runs before any close, so the repaired state is exactly the
pre-switch tab set). The switch itself is not unwound (the on_state
guard closes the project at the next session event when its session no
longer belongs to it). On the parse-fail shape (tabs already closed,
nothing restored) it only reports — re-pointing would aim the autosave
at ptr_old's slot and wipe it with the now-empty state. Callers that
must not continue on an abort branch on the result + `ed_handles()`:
`_create_project_file` skips the session1 save (it would bake the old
tabs into the new project), `session_delete` skips the deletion (the
doomed session's tabs are still open), `action_close_project` skips the
"Project closed" status (and repairs the no-default-file close path the
same way).

**Regime B — the plugin closes the tabs itself** (`close_all_tabs`):

- `close_all_tabs(silent=True)` — clears `PROP_MODIFIED`, unpins, then
  `cmd_FileCloseAll`. **Only** allowed when the caller has just saved
  the state into a session, or the discard is deliberate. Never
  touches the session pointer.
- `close_all_tabs(silent=False)` — keeps `PROP_MODIFIED`, so CudaText
  itself shows "save changes?" for modified tabs. Used when the state
  could NOT be saved (e.g. read-only project file) and when deleting
  the active session.

Regime B is used ONLY where no session load follows (so nothing else
would close the tabs): `action_open_project` for a legacy
session-less project file, `action_close_project` when the default
session file does not exist (`PROC_LOAD_SESSION` exits before closing
anything when the target file is missing), and `session_delete` of
the ACTIVE session — the doomed session's per-tab registrations must
NOT survive, the session never comes back. `cmd_FileCloseAll` alone
would also SKIP pinned tabs (the app respects pins by design — see
the comment in the core's `formmain_cmd.inc`), which is why
`close_all_tabs` unpins first.

CudaText sessions store the TEXT of modified and untitled tabs
(`'text'` + `'modified'` keys in the session JSON; restored as
unsaved buffers on load). Therefore "saved into a session" == "no
unsaved text lost". Every flow that closes tabs (Regime A or B) must
first call `_save_session_before_close_tabs()`:

- active project session → save into the project session slot;
- no project open (opening/creating/closing a project) → save into the
  app's current session file (`APP_FILE_SESSION`, typically
  default.cuda-session). This is the app's own autosave target, so it
  is safe and loses nothing.

`close_foreign_tabs` (foreign tabs only, the two menu commands) is
separate and has its own prompt/no-prompt contract — it is not part
of session switching.

Do not re-add a "save current state first?" question on session
switch: the ~40 s session autosave overwrites the "don't save" answer
moments later, so the prompt protected nothing (decided in #6352).

---
## 6. Startup / shutdown flow

**The on_start subscription mirrors "a project is in use".**
install.inf declares no event that loads the plugin unconditionally:
`on_open_pre` is key-filtered (`keys=cuda-proj` — it loads the plugin
only when a project file itself is opened, which is intended), and
`on_delete_file~`, `on_tab_change~`, `on_state~` are lazy (the `~`
suffix means "never load the plugin for this event"; `on_exit` is
always lazy in CudaText core no matter how it is declared). The plugin
additionally subscribes `on_start` in `plugins.ini [events]` (key
`cuda_project_man`), which is its ONLY unconditioned subscription —
this key alone decides whether CudaText loads the plugin at start.

Invariant: the key exists ⟺ a project is in use.

- **Subscribed** (`options['on_start']=True` + `save_events()`) by every
  project open / create / save — `action_open_project`,
  `_create_project_file`, `action_save_project_as`.
- **Removed** (`_update_on_start_subscription(False)`, the single
  unsubscribe point) on EVERY path that ends with no project:
  - `action_close_project` ("Close project"),
  - the detach branches of `on_start` (history.json restored a foreign
    or plain non-project session),
  - the external-session-switch close in `on_state` (§7),
  - `on_exit` when no project is open.
  Removing sets the hidden `on_start` option off, so `save_events()`
  calls `ini_proc(INI_DELETE_KEY, 'plugins.ini', 'events',
  'cuda_project_man')` and the options file records it too.
- `config()` re-enforces the invariant after any options change with
  `_update_on_start_subscription(bool(project_file_path))` — the
  hidden option is not in the dialog, and a stale `true` in a
  hand-edited options file must not re-subscribe the plugin with no
  project open.

Because the key is dropped the moment the project is gone (not just at
exit), the invariant survives a crash, and with no project loaded the
plugin never loads at the next CudaText start — the earlier behavior
("Close project" left the key in plugins.ini, so the plugin kept
loading and re-detaching on every start) is what this fixes. Users who
never use projects never load the plugin at all.

- **`on_start`**: reads `history.json` (CudaText's state file; its
  `session` key holds the session pointer being restored). If it
  points into the most-recent project, the project is adopted with
  `action_open_project(path, load_session=False)` — no tab closing, no
  session load; CudaText is already restoring that session; the plugin
  only records `last_session` (and re-subscribes on_start, per the
  invariant). Otherwise (foreign session / plain default session) the
  plugin detaches silently (`_close_project_silent`) so the two
  sessions don't fight — and unsubscribes.
- **`on_exit`**: with a project open, saves it
  (`action_save_project_as`) — which per §3 takes fresh sessions from
  disk and then saves the current session slot (and keeps on_start
  subscribed: the project is in use, the next start must restore it).
  CudaText saves the app session AFTER plugin exit handlers (verified
  in formmain.pas), so the app's final write lands on the
  already-correct file. With NO project open, it unsubscribes
  on_start instead (see the invariant above).

---
## 7. External session switches

`on_state` handles `APPSTATE_SESSION_SET` and
`APPSTATE_SESSION_LOAD` (issue #6366). If a project is open and
`session_cur_name()` is empty (app session no longer inside the
project), the project is closed silently: no tabs closed, no DEF_SES
loaded — the foreign session just takes over and the project's session
data is protected from being overwritten by the ~40 s autosave. The
on_start subscription is removed as well (§6): no project is in use
anymore.

- Own changes are ignored via the `_session_set_by_plugin` guard flag
  (set only around the `PROC_SET_SESSION` call inside `_set_session`).
- Own `PROC_LOAD_SESSION` calls are safe because they always follow
  `_set_session` with the same pointer (§2).

If you add a new way to switch sessions, it must go through
`_set_session` or must consciously detach the project first.

---
## 8. Project file format and versioning

Current format version: `PROJECT_FORMAT_VERSION = 2`, written as
`"version": 2`.

```
{
  "version": 2,
  "nodes":       ["C:\\code\\myapp"],          # roots; {ProjDir} macros allowed
  "vars":        ["Name=Value"],               # project variables
  "mainfile":    "",                           # optional "main" file
  "unfold":      ["C:\\code\\myapp"],          # expanded tree nodes
  "last_session": "session1",                  # session used last
  "sessions":    { "session1": { ...app-written session JSON... } }
}
```

- `load_project_file(path)` validates and returns `(data, error)` with
  error kinds `io`, `empty`, `json`, `type`, `nodes`, `sessions`.
  An empty (0-byte) file is special-cased: the user is offered to
  create a new project in it.
- `normalize_project_data(data)` only ADDS missing keys / fixes types
  (never removes user keys). Unknown keys are preserved everywhere.
- `_migrate_last_session(data)` moves `def_session` → `last_session`.
- Paths support `{ProjDir}` (= directory of the project file) and
  `{AppDir}` macros: `collapse_macros` on save, `expand_macros` on
  open. This makes projects portable when moved together with their
  folders.
- Session internals are written by CudaText's session API; the plugin
  treats them as opaque except when copying dicts around.
- Unknown/extra keys in hand-edited files are kept on every save.

---
## 9. Options, events, install.inf

- Options live in `settings/cuda_project_man.json`.
  `Command.options` (class attr) is the source of defaults;
  `__init__` merges the file on top; `save_options` writes back only
  keys that still exist in the defaults (stale keys from removed
  options are dropped automatically). `recent_projects` paths are
  stored macro-collapsed and expanded on load.
- install.inf: `[item2]`/`[item3]` subscribe the events (see §6 for
  the laziness rules). Commands live in `[itemN] section=commands`
  entries; `caption=&Project Manager\...` builds the Plugins menu.
  `menu=op` marks commands that also appear in CudaText's Settings
  menu — used exactly for `config` (Config...) and `config_proj`
  (Project properties...), nothing else. Commands needing a selected
  tree node are context-menu only and are not declared in install.inf.
- **Commands called by CudaText itself** (no install.inf entry, no
  caller inside this plugin — grep the plugin for callers and you find
  nothing, the callers live in the CudaText source, `formmain.pas` /
  `formmain_cmd.inc`, via `DoPyCommand('cuda_project_man', ...)`):
  - `open_dir(dirname, new_proj)` — a folder dragged from the OS file
    manager and dropped onto the editor (`DoFolderOpen`); adds the
    folder as a root node of the current project;
  - `new_project_open_dir()` — CudaText's "File - Open folder..."
    command (`DoFolderAdd`); makes a project from a picked folder in
    one step;
  - `show_panel` / `focus_panel` — the side-panel button
    (`[sidebar1] method=show_panel`) and its command entry.
  Removing or renaming any of these breaks a core integration with an
  AttributeError at runtime — this happened to `open_dir` once.
  When cleaning up "unused" methods, always search the CudaText repo
  for `'cuda_project_man'` string usages first.
- The panel context menu is built from `Command.menuitems`
  (caption, parent, node types, action, hotkey); parents: `proj`,
  `nodes`, `dir`, `file`, `''` (top level). Session menus (toolbar +
  context submenu + Plugins submenu) are all built by
  `fill_sessions_menu`.
- `menu_cfg()` is the toolbar sessions button, NOT the options dialog.

---
## 10. API for other plugins

- `cuda_project_man.project_variables()` → OrderedDict with `ProjDir`,
  `ProjMainFile`, `ProjMainFileNameOnly`, `ProjMainFileNameNoExt` and
  the user variables from Project properties. Used e.g. by ExtTools.
- `global_project_info` (module level) mirrors filename/nodes/vars/
  mainfile for the current project; updated by `update_global_data()`.

If you change these names, grep the CudaText ecosystem for users first.

---
## 11. Writing files

Every JSON write goes through `write_json_file(path, data)`:
write to `<path>.tmp<pid>`, then `os.replace()` over the target.
Either the file is replaced completely, or the previous complete file
stays intact and the OSError propagates to the caller (callers report
it). This is what keeps a crash / power loss / full disk from
destroying a .cuda-proj (nodes + all embedded sessions) or the options
file. Do not add bare `open(path, 'w')` + `json.dump` calls.

The upgrade path additionally makes a backup first:
`make_project_backup` → `<file>.bak`, or `<file>.<yymmdd_hhmmss>.bak`
when that name is taken; an existing backup is NEVER overwritten, and
when the backup fails the file is not touched (the project still opens
with upgraded data in memory; the disk is upgraded on the first
successful save).

---
## 12. Known non-obvious behaviors

- `action_save_project_as(path)` is the universal save: in-place saves
  and automatic saves (add node, properties, exit, ...) all funnel
  into it. It ends with `session_save_as(False, ...)` which saves the
  current session slot AFTER writing the file (the app's session write
  round-trips the JSON, so it must come last) and re-points the app
  session via `_set_session`.
- "Save project as..." to a new path first saves the active session
  into the OLD file, then carries `sessions`/`last_session` and the
  active session NAME over to the new file.
- `add_node` saves the whole project after each add (deliberate:
  roots survive a crash right after adding).
- `session_get_names()` reads the file from disk on every call — menus
  always reflect reality even if the app created/removed sub-keys.
- `tree_on_unfold` removes a root node from the in-memory project when
  its path no longer exists (the tree prunes dead roots); the file
  follows on the next save (including on_exit).
- `on_open_pre` returns False for `.cuda-proj` files — opening a
  project file from File>Open / command line opens it as a project
  instead of loading raw JSON into the editor.
- `DEF_SES` = `default<os-suffix>.cuda-session` (os suffix from
  `PROC_GET_OS_SUFFIX`).
- Sorting: `sort_order` is a module-level global consumed by
  `node_ordering_direntry` (perf: no attribute lookups in the sort
  key).
- The side panel is built ONCE per CudaText run, on the first
  `init_panel()` call (`init_form_main`: dialog, toolbar, tree, both
  icon imagelists, `icon_init`). CudaText has no API to recreate a
  docked plugin panel in place, so the icon-theme options can only
  take effect on a panel that does not exist yet — that is why the
  options dialog marks them with "(*)": new file-type/toolbar icon
  themes need a CudaText restart. (Changed before the panel is first
  shown, they apply without a restart.)

---
## 13. Drag & drop

Three directions exist; each uses a different mechanism. (A fourth one,
dragging selected TEXT from the editor onto the tree, was implemented via
dialog API events `on_drag_over`/`on_drag_drop` and REMOVED again: it was
not wanted. Dragging a ui-TAB onto the tree is not implementable in this
plugin: the tab dragging lives in the external ATTabs component.)

### a) Tree item → other applications (drag-out, Windows)

The tree control's `drag_files` property (dialog API). Setting it during
`on_mouse_move` makes CudaText start an OLE `DoDragDrop` with CF_HDROP
(the app defers it out of the mouse-move handler and checks the button
state, see `proc_customdialog_dummy`/`proc_windows_dragfiles`).

`_begin_drag_out()` sets the property on the first
`TREE_DRAG_THRESHOLD`-px move of a left-button press on a draggable
item, and remembers the dragged item/path in `_tree_drag_out_item` /
`_tree_drag_out_path`. IMPORTANT: the drag-out must start right on the
mouse drag, while the cursor is still over the tree — the tree control
does NOT receive mouse events once the cursor is outside its bounds, so
a "start the drag-out when the cursor leaves the tree" logic can never
fire (this was tried in 1.239.1 and broke the drag-out entirely).

### b) OS file manager → panel (drop-in of files)

Form event `on_drop_files` (dialog API, added in app 1.239.0). On
Windows the dialog form is registered as an OLE drop target
(`proc_ole_droptarget.AttachWithFilesHandler`): OLE routes a drop to
the deepest registered target, so the panel takes the files dropped on
it before the main window's handler (which opens them as editors).
`panel_on_drop_files()` receives the dict `{files, x, y, self_drag}`
(files = paths chr(1)-separated; x, y = drop position in the panel
form's client coords, -1 when unknown; self_drag = the drop is the
round-trip of the plugin's own drag-out, see c)) and runs each path
through `add_node` (dedup + save). Non-Windows goes through the LCL
`AllowDropFiles`/`OnDropFiles` support (best effort, no position).

### c) Inside the tree (inner drag&drop)

Windows: the inner drag&drop is the ROUND-TRIP of the drag-out from a).
Dropping the dragged node back onto the panel hits the panel's OLE drop
target; `proc_customdialog_dummy` recognizes it (the app's own
`DoDragDrop` is running at that moment, see `AppFileDragInProgress`).
The event is NOT fired from inside IDropTarget::Drop (that runs during
DoDragDrop; the plugin's modal confirmation dialogs there are unsafe):
the drop is recorded and fired by the drag-out code
(`ExecuteAppFileDrag` → `FirePendingSelfDropEvent`) right after the
OLE drag returned, with the exact drop position. When the drop was not
accepted by any drop target (eg no drop target resolved the release
position) and the button was released over the panel, the RELEASE
POSITION is reported as the self-drop instead (fallback), so the inner
drag&drop cannot get lost. Only the form which started the drag records
the drop-back; a drop of that drag on another form is ignored.
`panel_on_drop_files()` gets it with `self_drag=True`;
`_do_panel_self_drop()` converts the position from panel-form client
coords to tree client coords (live `x`,`y` of the tree control from
`DLG_CTL_PROP_GET`) and hit-tests the target node with
`TREE_ITEM_FIND_BY_POS`. Every failure is reported in the statusbar
(stale dragged node, unknown position, missing tree control, no node at
the drop position), so a broken inner drop can never be silent.

Non-Windows (no OLE drag-out exists): a hand-rolled state machine on
`on_mouse_down/move/up` (the LCL tree drag is NOT used — the treeview
must not start drags itself):

- `_tree_drag_press`: (x, y, hit-tested item) at mouse-down; item via
  the `tree_proc` API `TREE_ITEM_FIND_BY_POS` (added in app 1.239.0).
- move > `TREE_DRAG_THRESHOLD` px, pressed item draggable (has a
  data-path: not the project header, not a 'dummy') → mode `'inner'`;
  selection is saved, the drop target under the cursor is hit-tested
  and highlighted with `TREE_ITEM_SELECT`.
- `_drop_kind(src, tgt)` decides what a drop means, and doubles as the
  "is this a valid target" check for the highlight: root→root /
  root→header = `reorder` (header = to the end of the list),
  child→folder = `move` (on disk, like Cut+Paste); everything else
  (file targets, self, own subfolder, own parent, root→deeper,
  child→header) is refused → no highlight, no drop.
- cursor leaves the tree → cancel the inner drag&drop, restore the
  saved selection.
- mouse-up: perform `_do_tree_drop` or cancel+restore selection.

`_do_tree_drop()` asks the user for confirmation before doing anything
(msg_box_ex):
- reorder: when the option "Sorting order" is not "none", first a
  dialog offers to disable sorting ("Disable sorting" / "Cancel";
  cancelling aborts the whole drag&drop — with sorting on, the manual
  order would be overwritten by the next refresh anyway). Then a dialog
  "Change the order of the node '...'?" ("Accept" / "Cancel").
- move: a dialog "Move '...' from folder '...' to folder '...'?"
  ("Accept" / "Cancel"); `_move_node_on_disk` keeps its safety checks
  (no overwrite, no move into itself / own subfolder).
- not-allowed drops explain themselves in the statusbar
  (`_msg_drop_not_allowed`).

The dialogs are shown AFTER the node paths have been read from the item
handles, and the operations work on paths (not handles), so the tree
rebuild done by `action_refresh` (called by the sorting dialog and by
the operations themselves) cannot invalidate anything.

Two LCL details the code compensates for (non-Windows inner drag):
- OnClick fires on mouse-up EVEN after a drag inside the control, and
  BEFORE OnMouseUp — `_tree_suppress_click` (set when the gesture
  starts, cleared on the click or on the next mouse-down) prevents the
  dropped-on node from being opened by that click.

### d) Unfold-on-hover during any drag (`TREE_UNFOLD_HOVER_SEC`)

Hovering a folded folder for `TREE_UNFOLD_HOVER_SEC` seconds during a
drag&drop unfolds it (`_drag_hover_tick`); the unfold triggers
`tree_on_unfold`, which rebuilds only that folder's children, so the
dragged item's handle (which lives elsewhere and must be visible to be
draggable) stays valid. Works for all drags which hover the tree:

- non-Windows inner drag&drop: positions from `tree_mouse_move`;
- Windows inner drag&drop (the OLE round-trip of the drag-out) and files
  dragged in from the OS file manager: positions from the form's
  `on_drag_over` event (dialog API, fired by the OLE drop target on
  DragEnter/DragOver, `(-1,-1)` on DragLeave).

A timer (`timer_proc`, `DRAG_HOVER_TIMER_MS`) runs the hover state
machine, NOT the events alone: the cursor is usually held STILL over
the folder, and a still cursor fires no mouse-move/drag-over which
itself could time anything. The session state lives in
`_drag_hover_kind`/`_drag_hover_pos`/`_drag_hover_self`; it is ended by
mouse-up (inner), `on_drag_over` with negative coords / `on_drop_files`
(OLE), and as a safety net by the tick itself when no mouse button is
held anymore (`PROC_GET_KEYSTATE`). For inner drags only nodes which
are valid drop targets (`_drop_kind`) unfold — the same nodes the
drop-target highlight accepts; for file-manager drags any node unfolds
(navigation preview, like in OS file managers).

### API additions this plugin depends on (app 1.239.0)

- `dlg_proc` form event `on_drop_files` (dict data: files/x/y/self_drag)
- `dlg_proc` form event `on_drag_over` (dict data: x/y/self_drag)
- `tree_proc` action `TREE_ITEM_FIND_BY_POS`
