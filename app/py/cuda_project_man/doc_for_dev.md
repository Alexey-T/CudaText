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

---
## 5. Tab closing rules

Two closers exist:

- `close_all_tabs(silent=True)` — clears `PROP_MODIFIED`, unpins, then
  `cmd_FileCloseAll`. **Only** allowed when the caller has just saved
  the state into a session, or the discard is deliberate (deleting the
  active session). Never touches the session pointer.
- `close_all_tabs(silent=False)` — keeps `PROP_MODIFIED`, so CudaText
  itself shows "save changes?" for modified tabs. Used when the state
  could NOT be saved (e.g. read-only project file) and when deleting
  the active session.

CudaText sessions store the TEXT of modified and untitled tabs
(`'text'` + `'modified'` keys in the session JSON; restored as
unsaved buffers on load). Therefore "saved into a session" == "no
unsaved text lost". Every flow that closes tabs must first call
`_save_session_before_close_tabs()`:

- active project session → save into the project session slot;
- no project open (opening/creating/closing a project) → save into the
  app's current session file (`APP_FILE_SESSION`, typically
  default.cuda-session). This is the app's own autosave target, so it
  is safe and loses nothing.

`_close_tabs_saving_state()` wraps the pair
(`saved = ...; close_all_tabs(silent=saved)`). `close_foreign_tabs`
(foreign tabs only, the two menu commands) is separate and has its own
prompt/no-prompt contract — it is not part of session switching.

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
