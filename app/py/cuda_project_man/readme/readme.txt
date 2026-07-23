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
  - nodes: list of top-level folders/files you added (the project "roots")
  - vars: optional name=value variables used by macros
  - mainfile: optional path to the project's "main" file
  - unfold: which tree nodes should be expanded
  - def_session: name of the default session for this project
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
  A project and its active session are tightly coupled. When you open a
  project, its default session loads. When you close a project, the
  default CudaText session is restored. If a session switch is detected
  (via another session manager or manual API call), the project
  automatically detaches to prevent the foreign session from overwriting
  the project's session data.


------------------------------------------------------------------------------
MENU STRUCTURE
------------------------------------------------------------------------------

The context menu (right-click in the panel) is organized into sections:

PROJECT FILE
  New project...
      Prompts for a save location, creates an empty .cuda-proj file with
      a session named "session1" inside it, and loads that session. Any
      currently open project is saved and closed first. Cancel the dialog
      to abort — no untitled or temporary project is created.

  Open project...
      Opens an existing .cuda-proj file. Saves the current project's
      session, closes all tabs, loads the new project, and loads its
      default session.

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

  Stop tracking this Folder/File
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
  Focus in file manager
  Properties... (Alt+Enter)

SELECTED FILE (when a file is right-clicked)
  Open in default application
  Cut / Copy / Paste
  Copy path, Copy relative path
  Rename... (F2), Duplicate..., Delete (Del)
  Focus in file manager
  Set as main file
  Properties... (Alt+Enter)

SESSION (gear icon in toolbar)
  Project session: <name>
      Shows the currently active session (greyed out).

  Save session as...
      Prompts for a name and saves the current editor state into a new
      session slot inside the project.

  Save session
      Saves the current editor state into the current session slot
      (no prompt). Disabled if no session is active.

  Delete session...
      Prompts to pick a session and deletes it from the project.

  Set default session...
      Prompts to pick a session and marks it as the project's default.
      The default session is the one that loads when the project opens.

GENERAL
  Refresh (F5)
      Re-reads the project file and refreshes the tree.

  Go to file...
      Opens a dialog to quickly jump to any file in the project tree.

  Sync to editor file
      Highlights the current editor file in the project tree.


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
  [Project sessions] - shows the session menu (gear icon)
  [Sync]             - syncs the tree to the current editor file

The Sync button is hidden if "Always sync" is enabled in options
(since syncing then happens automatically).


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

  On opening file in
  Git/SVN repo, create
  temporary project
  from repo (*):          When you open a file that lives inside a
                          Git or SVN working copy, the plugin walks up
                          to the repo root and creates a temporary
                          project (saved as "temporary.cuda-proj" in
                          the settings folder) with the repo root as
                          its only node. The file you opened stays
                          open, and the tree jumps to it. Use "Save
                          project as..." to persist the temporary
                          project to a real location.

                          Disabled by default. Enable it if you want
                          the plugin to react to files opened from
                          version-controlled folders.

  Always sync project-
  tree with current
  editor file:           Automatically highlights the current editor
                          file in the tree on every tab change.

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
    normally. No extra prompts.

  - If the session belongs to a different project (e.g., history.json
    was manually edited), or is a plain non-project session, the
    plugin detaches from the project silently. The session's tabs
    load without interruption, and the project panel stays empty.

External session switch detection
  If a project is open and a session is loaded that doesn't belong to
  it (via another session manager or a direct PROC_LOAD_SESSION call),
  the plugin detects this via the APPSTATE_SESSION_LOAD event and
  detaches from the project silently. No tabs are closed, no default
  session is loaded — the foreign session simply takes over, and the
  project's saved session is protected from being overwritten.

Exit
  On CudaText exit, the current project is saved (if it has a file
  path). The temporary git auto-project is excluded — it is never
  saved on exit, to prevent its session from becoming the default.


------------------------------------------------------------------------------
PROJECT FILE FORMAT
------------------------------------------------------------------------------

A .cuda-proj file is JSON. Minimal example (no sessions):

  {
    "nodes": ["C:\\code\\myapp"],
    "vars": ["ProjDir=C:\\code\\myapp"],
    "mainfile": "",
    "unfold": ["C:\\code\\myapp"]
  }

With a default session:

  {
    "nodes": ["C:\\code\\myapp"],
    "vars": [],
    "mainfile": "",
    "unfold": ["C:\\code\\myapp"],
    "def_session": "session1",
    "sessions": {
      "session1": {
        "tabs": [...],
        "carets": [...]
      }
    }
  }

  The session internals ("tabs", "carets", etc.) are managed by
  CudaText's session API.

Paths inside "nodes" and "unfold" support the {ProjDir} macro: if a
path starts with the project file's directory, it is stored relative
to it using {ProjDir}. This makes projects portable (you can move the
project file and its referenced folders together).


------------------------------------------------------------------------------
TECHNICAL NOTES
------------------------------------------------------------------------------

- Only root nodes (top-level folders/files) are stored in the project.
  Folder contents are enumerated on demand when you expand a node.

- "Stop tracking this Folder/File" only removes the top-level entry
  you right-clicked. If you right-click a file inside a tracked
  folder, the command walks up to the folder's root entry.

- Opening a .cuda-proj file directly (via File > Open or command line)
  loads it as a project instead of showing raw JSON. This is handled
  by the on_open_pre event.

- The plugin subscribes to these events (declared in install.inf):
  on_open_pre (for .cuda-proj files), on_delete_file, on_tab_change,
  on_exit, on_state, on_start. Most are always subscribed. on_open
  is conditional — only subscribed when the "check_git" option is
  enabled (managed via plugins.ini to avoid overhead when the
  feature is off, since on_open fires on every file open).

- on_start auto-opens the most recent project on CudaText startup.
  It reads history.json to check which session CudaText is restoring:
  if it matches the most-recent project, the project metadata loads
  (CudaText's session restore proceeds normally). If it doesn't match
  (e.g. history.json was manually edited, or a different session is
  active), the project detaches silently to protect its session data.

- Sessions are stored inside the .cuda-proj file using CudaText's
  "project|/sessions/name" session pointer format.


------------------------------------------------------------------------------
AUTHORS
------------------------------------------------------------------------------

- Nikita Melentev  https://github.com/pohmelie
- Alexey Torgashin (CudaText)
- ildar r. khasanshin  https://github.com/ildarkhasanshin
- Badr ELmers  https://github.com/badrelmers

License: MIT
