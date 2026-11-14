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
    this key are the old format and are upgraded on first open (see
    "UPGRADE TO PROJECT FILE FORMAT V2")
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
UPGRADE TO PROJECT FILE FORMAT V2
------------------------------------------------------------------------------

Project files (.cuda-proj) carry a "version" key since format v2; files
written by older plugin versions have no version key ("the old
format") and are upgraded automatically the first time they are opened.
The upgrade is fully automatic and lossless: a backup of the original
file is saved next to it ("<file>.bak"), the missing standard keys are
added, your currently open tabs are saved into the project as its
first internal session (named after the external session plus
"auto-imported"), and a one-time dialog explains all of this.

The one breaking change to know: the session is now always stored
INSIDE the project file, so a project and an external session (e.g.
default.cuda-session) can no longer be open at the same time. This
rule fixed real bugs of lost and overwritten sessions (issues #6352
and #6600).

The complete guide to the v2 change — why it was needed, the two
kinds of sessions, how the workflow looked before and looks now, the
bugs solved, all breaking changes, and answers to common questions
("My tabs are gone after the update!") — is in the separate file
"upgrade_v2.txt", kept in the plugin's readme folder next to this
readme; the upgrade dialog's "Open documentation" button opens that
file directly.


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
      current session keeps its name in the new file, and both the old
      and the new file get the latest session states.

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
      via {VarName} syntax. A tab inside a value is kept (it does not
      split the variable). The dialog follows the UI theme like the
      other plugin windows.

ROOT NODES (Contents)
  Add folder...
      Adds a folder to the project's root list. A file dialog opens to
      pick the folder.

  Add file...
      Adds a file to the project's root list.

  Add current file
      Adds the file open in the editor to the project's root list.

  Add all opened files
      Adds every file open in the editor (all tab groups) to the
      project's root list.

  Add folder of current file
      Adds the folder containing the file open in the editor to the
      project's root list.

  Stop tracking this folder/file
      Removes the selected top-level entry from the project. The actual
      file or folder on disk is NOT deleted — it is simply no longer
      listed in the project.

  Clean up missing paths...
      Scans the project's root list and removes any entries whose paths
      no longer exist on disk (e.g., deleted or moved folders). Shows
      a confirmation dialog listing the paths to be removed.
      Also in the Plugins menu.

SELECTED FOLDER (when a folder is right-clicked)
  New file... (Ctrl+N), New folder... (F7)
  Cut / Copy / Paste (Ctrl+X / Ctrl+C / Ctrl+V)
  Copy path, Copy relative path
  Rename... (F2), Duplicate..., Delete (Del)
  Find in folder...
  Reveal in file manager
  Properties... (Alt+Enter)
      Read-only info window (size, dates, ...). It follows the UI
      theme like the other plugin dialogs.

SELECTED FILE (when a file is right-clicked)
  Open in default application
  Set as main file
      Marks the file as the project's "main" file (used by "Go to main
      file", "Open main file" and the {ProjMainFile} variables).
  Cut / Copy / Paste
  Copy path, Copy relative path
  Rename... (F2), Duplicate..., Delete (Del)
  Reveal in file manager
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
      The saved state includes the text of unsaved tabs (see
      AUTOMATIC BEHAVIORS below). "Save session as..." behaves the same
      way: the current session gets the changes too, and the new
      session starts as a copy of it.

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
      files. Rarely needed since the automatic import on opening an
      old-format project (see "Old project format (automatic
      upgrade)") covers the typical migration; use it for extra
      external session files, e.g. a "<project>.cuda-session" kept
      next to the project file.

GENERAL (context menu top level)
  Refresh (F5)
      Re-reads the project file and refreshes the tree.

  Show current file in project tree
      Highlights the current editor file in the tree (same as the
      toolbar button and the Plugins-menu command).

  Go to file...
      Opens a dialog to quickly jump to any file in the project tree.

  Go to main file
      Highlights the project's main file in the tree.

  Open main file
      Opens the project's main file in the editor.

  Favorites >
      The bookmarks submenu - see FAVORITES (BOOKMARKS) below. "Add
      to favorites" bookmarks the right-clicked node (disabled when
      nothing is selected); the rest is the same as the Plugins-menu
      Favorites submenu.

  Project sessions >
      Same items as the toolbar sessions menu.

  Config...
      Opens the plugin options dialog. Also in the Plugins menu and in
      CudaText's Settings menu.


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

The last button, [☆ Favorites], opens the Favorites (bookmarks) window;
it is a text button that shows the ☆ character itself, so it follows the
editor font and needs no icon file.


------------------------------------------------------------------------------
FILTERING AND TYPE-AHEAD IN THE TREE
------------------------------------------------------------------------------

The field between the toolbar and the tree filters the project tree:
type a few letters and only the files and folders whose NAME contains
them are shown. Folders whose own name matches are shown with their
whole content; other folders appear only when something matches
inside them, together with the path down to the match. Everything is
shown unfolded, and the "Ignore files/folders/hidden" options are
respected while scanning. Esc clears the field and restores the tree
with the folds and the selection from before the filtering.

The scan runs in a BACKGROUND thread, so even a very big folder tree
never hangs CudaText while it is searched (the very first scan of a
huge tree is the slow one; it is also cached for 20 s, so the next
keystrokes reuse it). While the filter field is not empty, a status
line right under it always tells what the search is doing: "Filter:
searching... N match(es) so far" while the background scan runs, and
"Filter: finished, N match(es) found" when it ended; the tree is
refilled from the results at that moment (a result of many thousands
of rows is added in chunks, so even that does not block the UI for
long). Clearing the field removes the line. The filtering is also
debounced: the scan starts 0.4 s after the typing pauses, not on
every keystroke. Clearing the filter drops the scan cache, so the
next filtering sees the current disk state.

The field, the status line and the panel background are colored with
the current UI theme (like the tree and the toolbar around them), and
they are re-colored when the theme is switched.

While the panel has the keyboard focus (and you are not typing in the
filter field), plain letters and digits jump to the first node of the
current level whose name starts with the typed text — "myf" selects
"myfile". Pressing the same letter again jumps to the next match; the
typed text is forgotten after about a second of pause, and any other
key or a mouse click restarts it. Like in Windows Explorer, the search
is scoped to the folder you are in: only the siblings of the selected
node are searched, so typing never jumps out of the current folder.


------------------------------------------------------------------------------
PLUGINS MENU COMMANDS
------------------------------------------------------------------------------

All commands are in the top menu: Plugins > Project Manager (and in the
command palette). They are grouped by purpose, most used first, with
separators between the groups.

The panel context menu and the Plugins menu share every command that
makes sense in both places: project lifecycle (create/open/save/close),
"Add..." commands and "Clean up missing paths...", navigation (Go to
file / Go to main file / Open main file / Show current file in project
tree), sessions and options. Commands that need a selected tree node
(New file/folder, Cut/Copy/Paste, Rename, Delete, Find in folder,
Set as main file, ...) live only in the context menu; commands about
the editor as a whole (Show/focus panel, Open all project files...,
Close tabs not belonging to project) live only in the Plugins menu.

  Show Project panel / Show and focus Project panel
      Open (and focus) the Project side panel.

  Go to file..., Go to main file, Open main file,
  Open all project files..., Show current file in project tree
      Quick navigation inside the project. "Show current file in project
      tree" is also in the panel context menu and on the toolbar.

  Create new empty project... / Create new project from open tabs... /
  Open project... / Open recent project... / Save project /
  Save project as... / Backup project file now / Close project /
  Open project folder
      The project lifecycle: create, open, save, close. "Backup project
      file now" makes one rotating backup of the project file on
      demand (see "Project file backups"); it is also in the panel
      context menu, under "Project file", right after "Save project
      as...". "Open project folder" opens the folder containing the
      .cuda-proj file in the OS file manager.

  Add to project: folder... / file... / current file /
  all opened files / folder of the current file
  Clean up missing paths...
      Managing the project's root list: adding roots ("folder..." and
      "file..." open a browse dialog) and dropping roots whose paths no
      longer exist. Same actions as the "Root nodes" section of the
      panel context menu (there, "Stop tracking this folder/file"
      removes the selected root as well).

  Project sessions >
      Submenu with Open session..., Save session, Save session as...,
      Delete session..., Import external session... — the same items as
      the sessions menu of the panel.

  Favorites >
      Submenu with the bookmarks commands: Show Favorites window...,
      Find favorite..., Add to favorites: current file / current folder /
      current project, Add opened... — see FAVORITES (BOOKMARKS) below.

  Close tabs not belonging to project
      Closes editor tabs whose files are not part of the project. Tabs
      with unsaved changes and untitled tabs get CudaText's own "save
      changes?" prompt, so nothing is lost silently.

  Close tabs not belonging to project (no prompt)
      Like the command above, but closes all foreign tabs at once,
      without any dialogs — unsaved changes in them are discarded.

  Project properties... / Config...
      Project variables and main file; plugin options. Both are also
      added to CudaText's Settings menu; "Config..." is in the panel
      context menu as well. Both dialogs follow the UI theme (form,
      labels, input fields, flat themed OK/Cancel buttons). The
      boolean options are themed TOGGLE ROWS: flat buttons painted in
      their "checked" state while the option is on, with a check /
      cross mark in the caption - click a row to flip it.


------------------------------------------------------------------------------
FAVORITES (BOOKMARKS)
------------------------------------------------------------------------------

Favorites are quick bookmarks for projects, files and folders,
independent of the opened project. They live in the "Favorites"
submenus (panel context menu and Plugins menu), on the ☆ toolbar
button, and in their own window.

THE FAVORITES WINDOW (☆ toolbar button, or Favorites > Show Favorites
window...)
  Resizable, non-modal; the active tab and the sort order are
  remembered between sessions, and the window size/position are kept
  for the WHOLE CudaText session: closing the window only hides it
  and the next open reuses the same form, so the size resets only
  when CudaText restarts (the size is not saved to disk: a form
  recreated with the pixel size of a maximized window ends up taller
  than the work area). The list is anchored to the window, so it
  grows and shrinks with it. Three tabs: Projects, Files, Folders -
  a TAB STRIP of flat themed buttons: equal widths, joined side by
  side directly above the list, the active tab taller and connected
  to the list (it covers the list's top border), the inactive tabs
  slightly lifted, the active one also marked with a leading
  bullet. Columns: check | ★ | Name | Path | State, separated by
  vertical lines. The list is the flat themed listbox of CudaText
  dialogs (listbox_ex, the same control the built-in Preferences
  dialog uses), so the list, its column header, the selection and the
  scrollbar all follow the UI theme - also live, when the theme is
  switched while CudaText runs. The rows are painted by the plugin
  from the theme colors (owner-drawing). The column widths are
  RESIZABLE: drag the vertical divider between two header captions
  (the mouse cursor takes the split shape over a divider); the Path
  column is auto-stretched - it always fills the list up to its
  right edge, also when the window is resized, and the widths you
  drag survive the list refills (sorting, filtering, starring...).

  Checkbox column
      The first column holds a checkbox per row. Check several rows,
      then run ONE command on all of them at once: open, star, unstar,
      copy paths, rename (one by one, Cancel stops), move up/down
      (the checked rows move as a block), delete. The commands are in
      the "With checked..." button and in the right-click menu (the
      "Checked (N):" section); the status line shows how many rows are
      checked. Click the "All" header cell (or right-click the header)
      to check/uncheck everything; Space checks/unchecks the selected
      row. Checking never changes the selection - it is a separate
      mechanism on top of it.

  Star column (★/☆)
      The "favorite of favorites" flag, drawn with the UTF-8 font
      glyphs ☆/★ (no image files); click the star cell of a row to
      toggle it (a double-click on the star cell only toggles, it
      does not open the item), or use Star/Unstar in the right-click
      menu. The default sort is starred first, then by name — so
      starred bookmarks float to the top without any manual ordering.

  State column
      Shows "?" when the favorite's path no longer exists on disk
      (a dead favorite), and nothing when everything is fine; the
      name of a dead favorite is painted in the theme's accent color
      too. Sort by it (click the header, or the right-click menu) to
      gather the dead favorites at one end: ascending puts them last.
      The old "? " prefix on the name is gone — the state lives in
      its own column now.

  Filter
      The field at the top filters by name AND path as you type; Esc
      clears it and shows the full list again (a second Esc closes the
      window).

  Sorting
      Click a column header (★ / Name / Path / State): 1st click
      sorts ascending, 2nd click descending, 3rd click switches to
      manual (custom) order — the order that Move up/down and
      Alt+Up/Alt+Down edit. The right-click menu always has the same
      Sort commands, whatever the build reports. Sorting by ★ always
      also sorts by name inside each group; sorting by State puts the
      dead ('?') favorites last when ascending, first when
      descending. The status line at the bottom shows the active
      sort.

  Left button panel (grouped with visible gaps between the groups)
      Open             open the selected favorite: projects open in
                       Project Manager, files in the editor, folders
                       in the OS file manager
      Add...           pick a project / file / folder in a dialog
      Add opened...    a menu with the currently openable things: the
                       opened project, the current editor file, or the
                       folder of the current file (only the available
                       ones are listed)
      With checked...  a menu with the commands that run on ALL
                       checked favorites: open, star/unstar, copy
                       paths, rename, move up/down as a block,
                       delete, uncheck all
      Move up /
      Move down       reorder the selected favorite (switches to
                       manual order)
      Delete           remove the selected favorite
      Close (bottom)   closes the window

  The Open / Move / Delete buttons act on the selected row; the
  commands of "With checked..." act on the checked rows.

  Opening a favorite (and "Reveal in file manager") runs from a short
  timer after the click, not inside the click handler itself: the OS
  file manager/editor steals the focus, CudaText deactivates, and the
  nested event processing crashed with "Access violation" errors (in
  on_app_deactivate) when it ran inside the dialog callback. Opening
  SEVERAL favorites at once (Open all checked) chains one timer per
  item, so the app can process the events between the opens.

  Keyboard (active while the window has the keyboard focus — the
  shortcuts never clash with your editor hotkeys):
      Ctrl+1..9 / Alt+1..9    open the favorite at that position in
                               the visible (filtered) list
      Enter / double-click    open the selected favorite
      Del                     delete it
      F2                      rename it
      Space                   check/uncheck it (checkbox column)
      Alt+Up / Alt+Down       move it up/down
      Esc                     clear the filter; second Esc closes
                               the window

  Right-click menu: Open, Reveal in file manager, Copy path,
  Star/Unstar, Rename..., Delete, the "Checked (N):" commands (open /
  star / unstar / copy paths / rename / move up / move down / delete /
  uncheck all), Sort (★ first / by name / by path / by state, dead
  last / manual order), Add current file/folder/project. A right-
  click on the header offers the Sort commands + Check/Uncheck all.

FIND FAVORITE (Favorites > Find favorite...)
  A compact modal picker over ALL favorites with a type-to-filter
  field; columns ★ / Name / Type / Path / State; Enter or double-click
  opens the choice; Ctrl+1..9 / Alt+1..9 work here too. Handy when
  bound to a hotkey. The list is the same flat themed listbox (with
  its themed header), and the dialog is colored with the current UI
  theme (like the Favorites window).

WHERE THE DATA LIVES
  settings/cuda_project_man_favorites.json (in the CudaText settings
  folder):

    {
      "projects": [ {"path": "...", "name": "...", "star": false} ],
      "files":    [ ... ],
      "folders":  [ ... ],
      "_ui":      { ...remembered tab and sort order... },
      "_deleted": [ ...paths the import must not resurrect... ]
    }

  Each entry keeps its path, display name (rename with F2) and star
  flag. The file is created on the first change. The window size is
  deliberately NOT stored: a CudaText form recreated with the pixel
  size of a maximized window ends up taller than the work area (its
  bottom lands off-screen). The closed window is only hidden and its
  form reused, so the size still lives for the whole CudaText session
  and resets only when CudaText restarts.

IMPORT FROM THE "FAVORITES" PLUGIN (cuda_favorites)
  If you also use kvichans' "Favorites" plugin, its
  settings/cuda_favorites.json is READ (never written): when Project
  Manager starts and whenever a favorites window is opened, entries
  missing from our list are imported — duplicates removed by path,
  files vs folders told apart by the path itself. The sync is strictly
  one-way: nothing is ever exported back, and the file is never
  created if it does not exist. Entries you delete here are remembered
  in "_deleted" so the next import does not resurrect them; adding a
  path again (Add... / Add opened...) clears that memory.


------------------------------------------------------------------------------
OPTIONS
------------------------------------------------------------------------------

Open via the menu: Project Manager > Config..., the panel context menu
(Config..., at the bottom), or CudaText's Settings menu

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

  File type icons (*):    Icon theme for file types in the tree.

  Toolbar icons (*):      Icon theme for the toolbar buttons.

  Sorting order (*):      How to sort items in the tree: by name,
                          extension, size, date, etc.

  Backup project file
  when its sessions
  change:                Switches the AUTOMATIC backups on/off (see
                          "Project file backups" under AUTOMATIC
                          BEHAVIORS). The command "Backup project file
                          now" works even when this is unchecked.
                          Default: on.

  Backup folder:          Folder for the backups: an absolute path, or
                          a path relative to the project file
                          (default: ".cudatext/proj_bckp", so the
                          backups live next to the project). Both "/"
                          and "\" work as separators on Windows,
                          macOS and Linux. Empty value = the default.

  Backups to keep:        How many backup files to keep per project,
                          rotating (default: 4; 0 keeps none of the
                          automatic ones — "Backup project file now"
                          still works and always keeps the single
                          newest copy).

  Default projects
  folder:                The folder where "Create new empty project...",
                          "Create new project from open tabs..." and
                          "Save project as..." save new projects: the
                          dialog opens directly in it, so you only type
                          the project name. The folder is created when
                          it does not exist. The value supports
                          $settings — the CudaText settings folder —
                          e.g. "$settings" or "$settings/projects", and
                          "~" for the home folder. Empty (default) =
                          the dialog starts in the last used folder
                          (for "Save project as...", in the folder of
                          the current project file), as before.

  (*) - requires CudaText restart


------------------------------------------------------------------------------
AUTOMATIC BEHAVIORS
------------------------------------------------------------------------------

Startup
  When CudaText starts, if a project was previously open, the plugin
  checks which session CudaText is restoring:

  - If the session belongs to the most-recent project, the project
    metadata is loaded and CudaText's session restore proceeds
    normally — CudaText itself restores the last used session (it is
    also recorded in the project file, so the project reopens with it
    later). No extra prompts, no session switching.

  - If the session belongs to a different project, or is a plain
    non-project session, the plugin detaches from the project silently.
    The session's tabs load without interruption, and the project
    panel stays empty.

External session switch detection
  While a project is open, only that project's sessions are ever
  loaded or saved. If the app is switched to a session that does not
  belong to the open project (by another session-manager plugin, or a
  manual command), the plugin detaches from the project silently.
  No tabs are closed, no default session is loaded — the other session
  simply takes over, and the project's saved sessions are protected
  from being overwritten by it. Session switches made by the Project
  Manager itself never trigger this.

Unsaved text when switching sessions or projects
  Sessions store more than the list of open tabs: the text typed but
  not yet saved to disk is stored inside the session too, and comes
  back as an unsaved tab when the session is loaded again. Before any
  command closes tabs (switching sessions, opening/creating/closing a
  project), the current state is saved into the session it belongs to,
  so unsaved text is never lost. If storing the state fails (for
  example a read-only project file), CudaText's own "save changes?"
  prompt appears for modified tabs instead — nothing is discarded
  silently.

Old project format (automatic upgrade)
  Project files carry a "version" key (current format version: 2).
  Files created by older plugin versions have no version key. Such a
  file is upgraded automatically when it is opened — no confirmation
  is asked, the open is never blocked. First a backup of the original
  file is saved next to it:
  "<file>.bak" (if that name is already taken, "<file>.<date>.bak" —
  an existing backup is never overwritten); then the file is
  rewritten. The upgrade only ADDS missing standard keys ("nodes",
  "vars", "mainfile", "unfold", "last_session", "version") — no
  existing content is removed. If the project contains sessions but no
  "last_session", the first session is used when it is opened.

  After the project finishes opening, a one-time information dialog
  tells the user about the upgrade, the backup file, and the v2 rule
  that a project and an external session cannot be open at the same
  time; its "Open documentation" button opens the separate migration
  guide upgrade_v2.txt (kept next to this readme, in the plugin's
  readme folder). The dialog appears once per project file: after a
  successful upgrade the file carries "version": 2 and is never
  old-format again. It also appears on the startup auto-open of an
  old-format project. When the file could not be rewritten on disk,
  the dialog says so (the upgrade is still pending, so it reappears
  on the next open).

  The status bar and the console panel report the upgrade and the
  backup file name; you can delete the backup file when everything
  works fine. If the folder is read-only or the file cannot be
  written, the project still opens with the upgraded data in memory,
  and the file on disk is upgraded on the first successful save.

  Old external-session workflow (automatic import). When the upgraded
  file has no internal sessions and no project was open before it was
  opened — the typical situation of the old workflow, where the open
  tabs belong to an external session like default.cuda-session — the
  currently open tabs are saved into the project as its first internal
  session, so the session automatically becomes part of the project
  file. The tabs simply stay open; from now on they load with the
  project, and no manual "Import external session..." is needed.
  The imported session is named after the external session plus
  "auto-imported" (e.g. "default auto-imported"); if that name is
  taken, a number is appended. The external .cuda-session file itself
  is kept on disk (updated with the final state, never deleted), and
  the .bak backup next to the project still holds the original file.
  Nothing is imported when another project was open before the open
  (those tabs belong to that project, not this one) or when no tabs
  are open (an empty "session1" is created, as before). When the
  session cannot be written into the project file (read-only folder,
  locked file), the empty "session1" is created as before — nothing
  is lost, the state is still saved into the external session file.

  Tip for the old external-session workflow: if a
  "<project>.cuda-session" file exists next to the project, a hint is
  printed in the console panel — use "Import external session..." in
  the sessions menu to embed it into the project (useful when the
  sibling file holds a DIFFERENT tab set than the session that was
  imported automatically).

  The full story of the v2 change — why it was needed, what bugs it
  solved, and the breaking changes — is in the separate migration
  guide upgrade_v2.txt (next to this readme, in the plugin's readme
  folder).

  Files marked with a version newer than the plugin supports are
  refused with an explanation (update CudaText / plugins first).

Project file backups
  The plugin keeps rotating backups of the open project file: the
  whole .cuda-proj (nodes, vars and every embedded session) is copied
  into the backup folder whenever the file changed on disk. CudaText's
  own session backup (the option ui_backup_sessions, the
  .N.cuda-session files) never covers project files: it rotates plain
  session files, while a project's sessions live inside the project
  file — so without this feature the .cuda-proj is a single point of
  failure for the project tree AND all its sessions.

  A backup is refreshed right after every session save into the
  project (CudaText's APPSTATE_SESSION_SAVE_AFTER event): the ~40 s
  session autosave (which itself only saves when a tab really changed:
  a different tab set, other files in tabs, or edited text), every
  session switch, every project save/switch, and the save on exit.
  The copy itself is skipped when the file did not change since the
  last backup (size + modification time, detected across restarts
  too) — saves which rewrote nothing write nothing. Note that a
  project save or a session switch rewrites the file even when its
  content ends up identical, so those events make one backup copy.
  A command which saves the session several times in a row makes ONE
  backup, not one per save: "Save session as..." saves the currently
  active session and then the new one (two rewrites of the whole
  project file), "Save project as..." into a new path saves the old
  project's session and the new file's. Backing up after every single
  save would rotate the set on half-done intermediate states — the
  first copy after "Save session as..." lacked the very session the
  command was creating. The backups are made when the command ends,
  from the final file: one backup per project file the command
  touched.

  IMPORTANT — dependence on CudaText's session saving. The automatic
  backups ride on session saves: the plugin is notified AFTER a
  session is saved, and copies the project file then. The periodic
  ~40 s saves come from CudaText's session autosave, which runs only
  while CudaText's session saving is enabled: the "Restore session"
  option (user.json key "ui_reopen_session") checked, and the
  session autosave interval ("ui_session_save_interval", default
  40 seconds) nonzero; starting CudaText with a no-session command
  line disables it as well. If you disable CudaText's session
  saving, the session autosave stops — and with it the periodic
  automatic project backups. A backup is then made only when a
  session is switched, when the project is saved/switched/closed,
  and on exit. The manual command "Backup project file now" is not
  affected and always works. Keep session saving enabled if you
  want the hands-off protection of the periodic backups.

  Reporting: an automatic backup is reported to the console panel
  only — never to the status bar, it would fire every ~40 seconds
  while typing — as one line with the trigger and the time, e.g.

      Project Manager: project backup by timer at 4:23:07 AM:
      D:\work\myproj\.cudatext\proj_bckp\myproj.cuda-proj.1.bak

  "by timer" labels the saves the app makes on its own (the session
  autosave, or the session manager writing the current session);
  "on session save" / "on session switch" / "on project save" /
  "on project switch" / "on project close" / "on exit" label the
  saves made by the Project Manager's own commands — tree edits
  (adding/removing/reordering nodes, moving a node on disk, "Set as
  main file", the project properties) save the project right away
  and are labeled "on project save" too. The manual
  "Backup project file now" reports to the status bar (an action
  you asked for deserves a visible confirmation).

  Backups are named "<project>.cuda-proj.1.bak" (newest) up to
  "<project>.cuda-proj.N.bak" (oldest, dropped when the set rotates),
  N = the "Backups to keep" option. The copy is written to a temp
  name and renamed, so an interrupted copy cannot corrupt the chain;
  a project file which does not end like a complete JSON object (a
  torn write) is never propagated into the backups — the older
  backups are kept instead. The command "Backup project file now"
  (Plugins > Project Manager, and the panel context menu under
  "Project file") makes one snapshot on demand, even when the file
  did not change. It ignores the backup options: it works with the
  automatic backups switched off, and with "Backups to keep" at 0
  (a manual snapshot always keeps the single newest copy).

  To restore a backup: close the project, copy the chosen .bak file
  over the .cuda-proj file, reopen the project.

  Note: if several projects with the same file name share ONE
  absolute backup folder, their backup sets collide (same backup
  names). Keep the default per-project folder, or use unique project
  file base names.

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
DRAG AND DROP
------------------------------------------------------------------------------

The project tree supports drag&drop in both directions. On Windows the
drag to other applications uses the CF_HDROP format, so files can be
dropped into Explorer, archivers, mail clients, browser upload dialogs
etc.

Dragging a tree item to other applications
  Press the left mouse button on a tree item and move the mouse: the
  dragged file/folder follows the cursor and can be dropped into any
  application which accepts files. Esc cancels the drag. (On Windows the
  drag starts right away; dropping the item back onto the Project panel
  is the inner drag&drop, see below. On other OS-es the drag-out to other
  apps is not supported and the gesture is always the inner drag&drop
  while the cursor stays over the tree.)

Dropping files from the file manager onto the panel
  Files/folders dropped from the OS file manager onto the Project panel
  are added to the current project as root nodes (they are NOT opened in
  the editor — that is what happens with drops onto the rest of the
  CudaText window). With no project open they are collected in the
  untitled project, which is saved to disk when the project is saved.
  Paths which cannot be added (do not exist on disk, or are hidden) are
  not added silently: the statusbar message shows every received path
  with the reason it was rejected.

Drag&drop inside the tree
  Drag a node and drop it onto another node of the tree (on Windows: drag
  it and drop it back onto the panel's tree). Every drop asks for
  confirmation:

  - Dragging a NODE of the project (a top-level item) only changes its
    order, it is never moved inside another node. A confirmation dialog
    "Change the order of the node '...'?" is shown; dropping onto another
    node puts the dragged node at that node's place, dropping onto the
    project header (the top-most row) moves it to the end of the list.
    When the option "Sorting order" is not "none", a dialog offers to
    disable sorting first (with sorting on, the manual order would be
    overwritten by sorting on the next refresh; cancelling the dialog
    cancels the drag&drop).
  - Dragging a FILE/FOLDER from inside a node to another FOLDER node
    moves it on disk into that folder, exactly like the plugin's
    Cut+Paste. A confirmation dialog "Move '...' from folder '...' to
    folder '...'?" is shown; the tree is refreshed and the project is
    updated when the moved path was the project's main file. Refused:
    overwriting an existing path, moving a folder into itself or into its
    own subfolder, moving into the folder it already is in.
  - Other drops are refused with an explaining statusbar message: a
    project node cannot be moved inside another node, a file/folder
    cannot be dropped onto the project header or onto a file node.

  On non-Windows OS-es (drag&drop tracked by mouse events over the tree):
  the node which would receive the drop is highlighted, and moving the
  cursor out of the tree cancels the drag.

Unfolding a folder by hovering on it
  During any drag&drop which hovers the project tree (dragging a node
  inside the tree, or dragging files in from the OS file manager), a
  folded folder node unfolds after the cursor rests on it for a moment
  (0.7 s), like in OS file managers. During the inner drag&drop this
  works for nodes which are valid drop targets; during a drag from the
  file manager it works for any node. After the unfold, hovering a
  subfolder unfolds it too, so nested folders can be opened one by one
  without interrupting the drag.


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
NOTES
------------------------------------------------------------------------------

- "Stop tracking this folder/file" only removes the top-level entry
  you right-clicked. If you right-click a file inside a tracked
  folder, the command walks up to the folder's root entry.

- Opening a .cuda-proj file directly (via File > Open or the command
  line) opens it as a project instead of showing raw JSON.

- The most recent project reopens automatically on CudaText start.
  The plugin itself is only loaded on start when a project is in use
  (open at exit, or used earlier and not closed). When you close the
  project — or when the restored session is not the project's — the
  plugin removes its startup registration, so it is not loaded at all
  on the next start; users who don't use projects keep CudaText's fast
  startup.

- The session loaded when a project is opened is the one that was used
  last in it; it is remembered in the project file on every session
  switch.

- Developer documentation (design concepts, session handling rules,
  file format details for plugin authors) is in the file
  doc_for_dev.md next to the plugin source code.


------------------------------------------------------------------------------
AUTHORS
------------------------------------------------------------------------------

- Nikita Melentev  https://github.com/pohmelie
- Alexey Torgashin (CudaText)
- ildar r. khasanshin  https://github.com/ildarkhasanshin
- Badr ELmers  https://github.com/badrelmers

License: MIT
