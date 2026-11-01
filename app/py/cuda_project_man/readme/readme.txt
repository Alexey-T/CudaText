Project Manager for CudaText
============================

A side-panel plugin for CudaText that lets you group folders and files into
"projects" (files with the .cuda-proj extension). A project stores the
top-level paths you add (folders and files), plus optional sessions that
remember which tabs were open and how the editor was configured.

The plugin lives in the "Project" side panel. Activate it via the sidebar
icon, or via the menu: Project Manager > Show Project panel.


------------------------------------------------------------------------------
CONCEPTS
------------------------------------------------------------------------------

Project file (.cuda-proj)
  A JSON file that stores:
  - version: project file format version (currently 2); files without
    this key are the old format and are upgraded on first open
  - nodes: list of top-level folders/files you added (the project "roots")
  - vars: optional name=value variables used by macros
  - mainfile: optional path to the project's "main" file
  - unfold: which tree nodes should be expanded
  - last_session: name of the session that was used last (the session
    loaded when the project is opened)
  - sessions: named sessions, each storing tab layout, cursors, etc.

  The tree only shows root nodes and their immediate contents. Folder
  contents are enumerated on demand, not stored in the project file. This
  keeps project files small even for huge codebases.

Session
  A saved snapshot of the editor state: which tabs are open, in which
  groups, cursor positions, scroll positions, etc. Each project can have
  multiple named sessions. Sessions live inside the .cuda-proj file under
  the "sessions" key.

  The default CudaText session (default.cuda-session in the settings
  folder) is separate from project sessions. It is used when no project
  is open.

Project-session binding
  A project and its active session are tightly coupled — a project is
  itself a session, so it always keeps at least one. When you open a
  project, its last used session loads. When you close a project, the
  default CudaText session is restored. While a project is open, only
  that project's internal sessions are ever loaded or saved — the
  plugin never switches the app onto an external session file (and
  never lets an external session be overwritten). If an external
  session switch is detected (via another session manager or manual
  API call), the project automatically detaches to prevent the foreign
  session from overwriting the project's session data.


------------------------------------------------------------------------------
MENU STRUCTURE
------------------------------------------------------------------------------

The context menu (right-click in the panel) is organized into sections:

PROJECT FILE
  Create new empty project...
      Prompts for a save location, creates an empty .cuda-proj file with
      a session named "session1" inside it, and loads that session. Any
      currently open project is saved and closed first. Cancel the dialog
      to abort — no untitled or temporary project is created.

  Create new project from open tabs...
      Turns the current working set into a NEW project: prompts for a
      .cuda-proj location, embeds the currently open tabs as a session
      named "session1", makes it the last used session, and switches to
      the new project. Tabs are not closed. Also handy to migrate the old
      workflow that kept sessions in external .cuda-session files.

  Open project...
      Opens an existing .cuda-proj file. Saves the current project's
      session, closes all tabs, loads the new project, and loads its
      last used session.

  Recent projects >
      Submenu listing recently opened projects. Click one to open it.

  Save project
      Saves the current project in place (no dialog). If the project has
      no file path yet, falls back to Save project as... Shows "Project
      saved" briefly in the status bar.

  Save project as...
      Prompts for a new file location and saves the project there. The
      current session is saved into the project file under its current
      name (or "session1" if none is active).

  Close project
      Saves the current session, closes all tabs, and loads the default
      CudaText session (default.cuda-session). The default session is
      restored, not overwritten — any tabs you had there before opening
      a project come back. Resets the panel to "no project" state.

  Open project folder
      Opens the folder that CONTAINS the .cuda-proj file in the OS file
      manager (Windows Explorer, Finder, ...). (Folders added to the
      project have their own "Reveal in file manager" item.)

  Project properties...
      Opens a dialog to edit project variables (Name=Value pairs, one
      per line) and view the main file. Variables can be used in macros
      via {VarName} syntax.

ROOT NODES (Contents)
  Add folder...
      Adds a folder to the project's root list. A file dialog opens to
      pick the folder.

  Add file...
      Adds a file to the project's root list.

  Stop tracking this folder/file
      Removes the selected top-level entry from the project. The actual
      file or folder on disk is NOT deleted — it is simply no longer
      listed in the project.

  Clean up missing paths...
      Scans the project's root list and removes any entries whose paths
      no longer exist on disk (e.g., deleted or moved folders). Shows
      a confirmation dialog listing the paths to be removed.

SELECTED FOLDER (when a folder is right-clicked)
  New file... (Ctrl+N), New folder... (F7)
  Cut / Copy / Paste (Ctrl+X / Ctrl+C / Ctrl+V)
  Copy path, Copy relative path
  Rename... (F2), Duplicate..., Delete (Del)
  Find in folder...
  Reveal in file manager
  Properties... (Alt+Enter)

SELECTED FILE (when a file is right-clicked)
  Open in default application
  Cut / Copy / Paste
  Copy path, Copy relative path
  Rename... (F2), Duplicate..., Delete (Del)
  Reveal in file manager
  Set as main file
  Properties... (Alt+Enter)

PROJECT SESSIONS (toolbar gear button, or the context-menu
"Project sessions" submenu)
  Project session: <name>
      One item per session in the project; the currently active one is
      greyed out. Click a session to switch to it.

      Switching always saves first: the active session's state is
      written into its slot before the new session loads, so changes
      are never lost (no prompt — CudaText's session autosave would
      write the state out within ~40 seconds anyway, so asking first
      would only create a state the autosave overwrites moments later).
      "Save session as..." behaves the same way: the current session
      gets the changes too, and the new session starts as a copy of it.

  Save session
      Saves the current editor state into the current session slot
      (no prompt). Disabled if no session is active.

  Save session as...
      Prompts for a name and saves the current editor state into a new
      session slot inside the project; the new session becomes the
      active one. No tabs are closed — the full current state (including
      tabs not belonging to the project) is saved as is, and the
      previously active session is updated with the same state.

  Delete session...
      Prompts to pick a session and deletes it from the project. The
      last remaining session cannot be deleted (a project is itself a
      session, it must keep one) — the menu item is disabled then, and
      the command explains it in a dialog. If the deleted session is
      the active one, you pick another session of the project to load
      instead; the app session never leaves the project for an
      external one.

  Import external session...
      Imports an external .cuda-session file into the project as an
      internal session, named after the file. Load it afterwards from
      this same session menu. Migration helper for the old
      (pre-versioning) workflow that kept project sessions in separate
      files.

GENERAL (context menu top level)
  Refresh (F5)
      Re-reads the project file and refreshes the tree.

  Go to file...
      Opens a dialog to quickly jump to any file in the project tree.


------------------------------------------------------------------------------
TOOLBAR
------------------------------------------------------------------------------

The toolbar (top of the panel) has these buttons, left to right:

  [Recent projects]  - shows the recent projects menu
  [Open project]     - opens an existing project
  [Save project]     - saves the current project in place
  ---
  [Add folder]       - adds a folder to the project
  [Add file]         - adds a file to the project
  ---
  [Project sessions] - shows the sessions menu (same items as the
                       context-menu "Project sessions" submenu)
  [Show current file in project tree] - highlights the current
                       editor file in the tree

The "Show current file" button is hidden if "Automatically show current
file in project tree" is enabled in options (since it then happens
automatically on every tab switch).


------------------------------------------------------------------------------
PLUGINS MENU COMMANDS
------------------------------------------------------------------------------

All commands are in the top menu: Plugins > Project Manager (and in the
command palette). They are grouped by purpose, most used first, with
separators between the groups:

  Show Project panel / Show and focus Project panel
      Open (and focus) the Project side panel.

  Go to file..., Go to main file, Open main file,
  Open all project files..., Show current file in project tree
      Quick navigation inside the project.

  Create new empty project... / Create new project from open tabs... /
  Open project... / Open recent project... / Save project /
  Save project as... / Close project / Open project folder
      The project lifecycle: create, open, save, close. "Open project
      folder" opens the folder containing the .cuda-proj file in the OS
      file manager.

  Add to project: folder... / file... / current file /
  all opened files / folder of the current file
      Adding roots to the project; "folder..." and "file..." open a
      browse dialog (same actions as in the panel context menu).

  Project sessions >
      Submenu with Open session..., Save session, Save session as...,
      Delete session..., Import external session... — the same items as
      the sessions menu of the panel.

  Close tabs not belonging to project
      Closes editor tabs whose files are not part of the project. Tabs
      with unsaved changes and untitled tabs get CudaText's own "save
      changes?" prompt, so nothing is lost silently.

  Close tabs not belonging to project (no prompt)
      Like the command above, but closes all foreign tabs at once,
      without any dialogs — unsaved changes in them are discarded.

  Project properties... / Config...
      Project variables and main file; plugin options.


------------------------------------------------------------------------------
OPTIONS
------------------------------------------------------------------------------

Open via the menu: Project Manager > Config... or right-click > Options...

  Ignore files:           Semicolon-separated mask of filenames to hide
                          in the tree (e.g., "*.tmp;*.bak").

  Ignore folders:         Semicolon-separated mask of folder names to hide
                          (default: ".git;.svn").

  Ignore all hidden
  files/folders:          Hides files/folders with the hidden attribute.

  Recent projects:        Editable list of recently opened projects.

  Show toolbar:           Shows/hides the panel toolbar.

  Open file after
  "Go to file" command:   If checked, Go to file opens the file in the
                          editor. If unchecked, it only highlights it in
                          the tree.

  Use "preview tab" on
  item clicking:          Single-click opens files in a preview tab
                          (temporary, replaced on next click).

  Open files by
  double-click:           If unchecked, single-click opens files. If
                          checked, single-click previews, double-click
                          opens permanently.

  Automatically show
  current file in
  project tree:           Automatically highlights the current editor
                          file in the tree on every tab change. When
                          enabled, the "Show current file" toolbar button
                          is hidden.

  File type icons:        Icon theme for file types in the tree.

  Toolbar icons:          Icon theme for the toolbar buttons.

  Sorting order (*):      How to sort items in the tree: by name,
                          extension, size, date, etc.

  (*) - requires CudaText restart


------------------------------------------------------------------------------
AUTOMATIC BEHAVIORS
------------------------------------------------------------------------------

Startup
  When CudaText starts, if a project was previously open, the plugin
  reads history.json to determine which session CudaText is restoring:

  - If the session belongs to the most-recent project, the project
    metadata is loaded and CudaText's session restore proceeds
    normally — CudaText itself restores the last used session (it is
    also recorded in the project file as "last_session", so the
    project reopens with it later). No extra prompts, no session
    switching.

  - If the session belongs to a different project (e.g., history.json
    was manually edited), or is a plain non-project session, the
    plugin detaches from the project silently. The session's tabs
    load without interruption, and the project panel stays empty.

External session switch detection
  While a project is open, only that project's internal sessions are
  ever loaded or saved. If the app's session no longer belongs to the
  open project, the plugin detaches from the project silently. No tabs
  are closed, no default session is loaded — the foreign session simply
  takes over, and the project's saved session is protected from being
  overwritten. This is detected via two app state events:
  - APPSTATE_SESSION_SET: another plugin (e.g. a session manager)
    changed the app's session via PROC_SET_SESSION (issue #6366);
  - APPSTATE_SESSION_LOAD: a session that doesn't belong to the open
    project was loaded (a direct PROC_LOAD_SESSION call).
  Session changes made by the Project Manager itself never trigger
  this.

Old project format (automatic upgrade)
  Project files carry a "version" key (current format version: 2).
  Files created by older plugin versions have no version key. Such a
  file is upgraded automatically when it is opened — no dialog is
  shown. First a backup of the original file is saved next to it:
  "<file>.bak" (if that name is already taken, "<file>.<date>.bak" —
  an existing backup is never overwritten); then the file is
  rewritten. The upgrade only ADDS missing standard keys ("nodes",
  "vars", "mainfile", "unfold", "last_session", "version") — no
  existing content is removed. If the project contains sessions but no
  "last_session", the first session is used when it is opened.

  The status bar and the console panel report the upgrade and the
  backup file name; you can delete the backup file when everything
  works fine. If the folder is read-only or the file cannot be
  written, the project still opens with the upgraded data in memory,
  and the file on disk is upgraded on the first successful save.

  Tip for the old external-session workflow: if a
  "<project>.cuda-session" file exists next to the project, a hint is
  printed in the console panel — use "Import external session..." in
  the sessions menu to embed it into the project.

  Files marked with a version newer than the plugin supports are
  refused with an explanation (update CudaText / plugins first).

Broken project files
  Before opening, project files are validated. Unreadable files,
  invalid JSON, non-object JSON, or wrong key types ("nodes" must be a
  list of paths, "sessions" an object) are reported with a dialog and
  the project is not opened; the editor state is left untouched.
  An empty (0-byte) file is special-cased: since it holds no data, the
  plugin offers to create a new empty project in it.

Exit
  On CudaText exit, the current project is saved (if it has a file
  path).


------------------------------------------------------------------------------
PROJECT FILE FORMAT
------------------------------------------------------------------------------

A .cuda-proj file is JSON. Every file written by the plugin carries
"version": 2 (the current format version). Minimal example (no sessions):

  {
    "version": 2,
    "nodes": ["C:\\code\\myapp"],
    "vars": ["ProjDir=C:\\code\\myapp"],
    "mainfile": "",
    "unfold": ["C:\\code\\myapp"]
  }

With a session:

  {
    "version": 2,
    "nodes": ["C:\\code\\myapp"],
    "vars": [],
    "mainfile": "",
    "unfold": ["C:\\code\\myapp"],
    "last_session": "session1",
    "sessions": {
      "session1": {
        "tabs": [...],
        "carets": [...]
      }
    }
  }

  The session internals ("tabs", "carets", etc.) are managed by
  CudaText's session API.

Files without the "version" key (created by older plugin versions) are
opened normally: the structure is normalized in memory and the file is
upgraded on disk automatically, with a backup of the original kept
next to it (see AUTOMATIC BEHAVIORS above). Missing standard keys are
always auto-filled, so hand-edited or partial files load fine as long
as the present keys have the right types.

Paths inside "nodes" and "unfold" support the {ProjDir} macro: if a
path starts with the project file's directory, it is stored relative
to it using {ProjDir}. This makes projects portable (you can move the
project file and its referenced folders together).


------------------------------------------------------------------------------
TECHNICAL NOTES
------------------------------------------------------------------------------

- Only root nodes (top-level folders/files) are stored in the project.
  Folder contents are enumerated on demand when you expand a node.

- "Stop tracking this folder/file" only removes the top-level entry
  you right-clicked. If you right-click a file inside a tracked
  folder, the command walks up to the folder's root entry.

- Opening a .cuda-proj file directly (via File > Open or command line)
  loads it as a project instead of showing raw JSON. This is handled
  by the on_open_pre event.

- The plugin subscribes to these events (declared in install.inf):
  on_open_pre (for .cuda-proj files only), on_delete_file, on_tab_change,
  on_exit, on_state. All of them are lazy (the '~' suffix in
  install.inf): lazy events never load the plugin, their handlers only
  run when the plugin is already loaded (on_exit is always lazy in
  CudaText anyway). So install.inf never causes the plugin to load on
  start.

- on_start is subscribed additionally via plugins.ini [events], which
  the plugin writes itself, when a project was opened/created/saved
  (the hidden "on_start" option). This way CudaText loads the plugin
  on start only for users who really used projects on the previous
  run; users who don't use Project Manager keep a fast start, the
  plugin is not loaded at all.

- on_start auto-opens the most recent project on CudaText startup.
  It reads history.json to check which session CudaText is restoring:
  if it matches the most-recent project, the project metadata loads
  (CudaText's session restore proceeds normally). If it doesn't match
  (e.g. history.json was manually edited, or a different session is
  active), the project detaches silently to protect its session data.

- Sessions are stored inside the .cuda-proj file using CudaText's
  "project|/sessions/name" session pointer format.

- "last_session" (the session to load when the project is opened) is
  updated in the project file every time a project session becomes
  active, so the project always reopens with the session that was used
  last. Older project files carrying the removed "def_session" key are
  migrated to "last_session" on open.

- While a project is open, every session pointer the plugin sets points
  INSIDE that project's file. The only moment an external session is
  loaded is "Close project" — the project is forgotten first, then the
  default CudaText session is restored. This is what keeps external
  session files (like default.cuda-session) from being overwritten by
  the ~40-second session autosave while project tabs are open.


------------------------------------------------------------------------------
AUTHORS
------------------------------------------------------------------------------

- Nikita Melentev  https://github.com/pohmelie
- Alexey Torgashin (CudaText)
- ildar r. khasanshin  https://github.com/ildarkhasanshin
- Badr ELmers  https://github.com/badrelmers

License: MIT
