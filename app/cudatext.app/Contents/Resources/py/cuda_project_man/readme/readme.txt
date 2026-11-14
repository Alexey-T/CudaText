Project Manager for CudaText
============================

A side-panel plugin for CudaText that lets you group folders and files into
"projects" (files with the .cuda-proj extension). A project stores the
top-level paths you add (folders and files), plus optional sessions that
remember which tabs were open and how the editor was configured.

The plugin lives in the "Project" side panel. Activate it via the sidebar
icon, or via the menu: Project Manager > Show Project panel.


QUICK START
-----------

1. Plugins > Project Manager > Create new empty project... - pick where
   to save the .cuda-proj file.
2. Right-click in the panel and use Add folder... / Add file... (or drag
   folders from the OS file manager onto the panel).
3. Browse the tree, click files to open them; save with "Save project".
4. Optional: right-click a file > Set as main file; bookmark often-used
   items in Favorites; keep several named sessions per project.

Nothing you delete from this panel is removed permanently on Windows,
macOS or Linux - see "DELETING AND TRASH SAFETY" below. (On the rare
OSes without a trash the plugin warns you explicitly before any
permanent deletion.)


------------------------------------------------------------------------------
DELETING AND TRASH SAFETY
------------------------------------------------------------------------------

On Windows, macOS and Linux no command of this plugin ever deletes a
file or folder permanently.

- Deleting a file or folder INSIDE a node (the "Delete" context-menu
  item, or the Del key) moves it to the OS trash - the Recycle Bin on
  Windows, the Trash on macOS and Linux. The confirmation dialog's
  button says "Move to trash" (not a bare "OK"), and its text reminds
  you that the item can be restored from there. Restore it with your
  file manager whenever you need it back. If the move is not possible
  (a drive without a Recycle Bin, a locked file), an error dialog
  explains why and NOTHING happens to the file.

- On an operating system WITHOUT trash support (e.g. the BSDs,
  Haiku) the delete command shows a DIFFERENT dialog: a warning icon,
  the explanation that this OS cannot move items to the trash and
  that the deletion would be permanent, and the buttons
  "DELETE PERMANENTLY" and "Cancel". The file is permanently deleted
  ONLY after you press "DELETE PERMANENTLY" - and this is the only
  permanent deletion the plugin can ever perform.

- A TOP-LEVEL NODE (a root folder/file you added to the project) cannot
  be deleted from here at all: the "Delete" item does not even show for
  it. Pressing Del on it (or running any delete command on it) asks
  "Are you sure you want to stop tracking this file/folder?" (the
  button says "Stop tracking") and then only untracks it - the entry is
  removed from the project tree, while the file/folder itself stays on
  the disk, untouched. Re-add it any time with Add folder... /
  Add file....


------------------------------------------------------------------------------
CONCEPTS
------------------------------------------------------------------------------

Project file (.cuda-proj)
  A JSON file that stores:
  - nodes: list of top-level folders/files you added (the project "roots")
  - vars: optional name=value variables used by macros
  - mainfile: optional path to the project's "main" file
  - unfold: which tree nodes should be expanded
  - last_session: name of the session that was used last
  - sessions: named sessions, each storing tab layout, cursors, etc.

  The tree only shows root nodes and their contents, which are read from
  the disk on demand - not stored in the project file. This keeps
  project files small even for huge codebases.

Session
  A saved snapshot of the editor state: which tabs are open, in which
  groups, cursor and scroll positions, etc. Each project can have
  multiple named sessions; they live inside the .cuda-proj file. The
  default CudaText session (default.cuda-session) is separate and is
  used when no project is open. While a project is open, only that
  project's sessions are ever loaded or saved: switching to a foreign
  session (from another session-manager plugin) detaches from the
  project silently, so the project's saved sessions can never be
  overwritten by it.


------------------------------------------------------------------------------
UPGRADE TO PROJECT FILE FORMAT V2
------------------------------------------------------------------------------

Project files (.cuda-proj) carry a "version" key since format v2; files
written by older plugin versions have no version key and are upgraded
automatically the first time they are opened - no confirmation is asked,
the open is never blocked. The upgrade is lossless: a backup of the
original file is saved next to it ("<file>.bak"), the missing standard
keys are added, your currently open tabs are saved into the project as
its first internal session (named after the external session plus
"auto-imported"), and a one-time dialog explains all of this.

The one breaking change to know: the session is now always stored INSIDE
the project file, so a project and an external session (e.g.
default.cuda-session) can no longer be open at the same time.

The complete guide to the v2 change - why it was needed, the workflows
before and after, all breaking changes, and answers to common questions
("My tabs are gone after the update!") - is in the separate file
"upgrade_v2.txt", kept in the plugin's readme folder next to this
readme; the upgrade dialog's "Open documentation" button opens it.


------------------------------------------------------------------------------
MENU STRUCTURE
------------------------------------------------------------------------------

The context menu (right-click in the panel) is organized into sections.

PROJECT FILE
  Create new empty project...
      Prompts for a save location, creates an empty .cuda-proj file with
      a session named "session1" inside it, and loads that session. Any
      currently open project is saved and closed first. Cancel the
      dialog to abort - no untitled or temporary project is created.

  Create new project from open tabs...
      Turns the current working set into a NEW project: prompts for a
      .cuda-proj location, embeds the currently open tabs as a session
      named "session1", and switches to the new project. Tabs are not
      closed.

  Open project...
      Opens an existing .cuda-proj file. Saves the current project's
      session, closes all tabs, loads the new project and its last used
      session.

  Recent projects >
      Submenu listing recently opened projects. Click one to open it.

  Save project
      Saves the current project in place (no dialog). Falls back to
      Save project as... when the project has no file path yet.

  Save project as...
      Prompts for a new file location and saves the project there. The
      current session keeps its name in the new file; both the old and
      the new file get the latest session states.

  Backup project file now
      Makes one backup copy of the project file on demand (see
      "Project file backups"). Works even with the automatic backups
      disabled.

  Close project
      Saves the current session, closes all tabs, and restores the
      default CudaText session (restored, not overwritten - tabs you
      had there before opening a project come back).

  Open project folder
      Opens the folder that CONTAINS the .cuda-proj file in the OS file
      manager (Windows Explorer, Finder, ...).

  Project properties...
      Edits project variables (Name=Value pairs, one per line) and the
      main file. Variables can be used in macros via {VarName}.

ROOT NODES
  Add folder... / Add file...
      Adds a folder/file to the project's root list (a browse dialog
      opens).

  Add current file / Add all opened files / Add folder of current file
      Adds the file open in the editor / every opened file (all tab
      groups) / the folder of the current file.

  Stop tracking this folder/file
      Removes the selected top-level entry from the project. The actual
      file or folder on disk is NOT deleted - it is simply no longer
      listed in the project. When you right-click a file inside a
      tracked folder, the command walks up to the folder's root entry.

  Clean up missing paths...
      Removes every root entry whose path no longer exists on disk
      (e.g. deleted or moved folders), after a confirmation.

SELECTED FOLDER / SELECTED FILE (when a folder/file is right-clicked)
  New file... (Ctrl+N), New folder... (F7)
  Cut / Copy / Paste (Ctrl+X / Ctrl+C / Ctrl+V)
  Copy path, Copy relative path
  Rename... (F2), Duplicate...
  Delete (Del)
      Moves the file/folder to the OS trash (never deletes permanently)
      after a confirmation - see "DELETING AND TRASH SAFETY". This item
      is not shown for top-level nodes: they can only be untracked
      ("Stop tracking this folder/file").
  Find in folder... (folders only)
  Reveal in file manager
      Opens the folder of the clicked item in the OS file manager, with
      the item selected. For a path that no longer exists, opens its
      parent folder.
  Open in default application, Set as main file (files only)
  Properties... (Alt+Enter)
      Read-only info window (size, dates, ...).

PROJECT SESSIONS (also on the toolbar button)
  Project session: <name>
      One item per session in the project; the currently active one is
      greyed out. Click a session to switch to it. Switching always
      saves first: the active session's state is written into its slot
      before the new one loads, so changes are never lost. The saved
      state includes the text of unsaved tabs.

  Save session
      Saves the current editor state into the current session slot
      (no prompt). Disabled if no session is active.

  Save session as...
      Prompts for a name and saves the current editor state into a new
      session slot; the new session becomes the active one. No tabs are
      closed - the full current state is saved as is, and the previously
      active session is updated with the same state.

  Delete session...
      Prompts to pick a session and deletes it from the project. The
      last remaining session cannot be deleted (a project is itself a
      session, it must keep one) - the item is disabled then. If the
      deleted session is the active one, you pick another session of
      the project to load instead.

  Import external session...
      Imports an external .cuda-session file into the project as an
      internal session, named after the file. Migration helper for the
      old workflow that kept project sessions in separate files; rarely
      needed since old-format projects import their session
      automatically on first open.

GENERAL (context menu top level)
  Refresh (F5)
      Re-reads the project file and refreshes the tree.

  Show current file in project tree
      Highlights the current editor file in the tree (same as the
      toolbar button).

  Go to file...
      Opens a dialog to quickly jump to any file in the project tree.

  Go to main file / Open main file
      Highlights / opens the project's main file in the editor.

  Open containing folder
      Opens the folder of whatever is clicked: the folder of the
      .cuda-proj file for the project row and an empty click (what
      "Open project folder" does); the containing folder of a node,
      file or folder, with the item selected (what "Reveal in file
      manager" does). Visible for every click target.

  Project sessions >
      Same items as the toolbar sessions menu.

  Favorites >
      The bookmarks submenu: "Show Favorites window..." first, then
      "Add to favorites" (bookmarks the right-clicked node; disabled
      when nothing is selected) and "Add opened..." - see FAVORITES
      (BOOKMARKS) below.

  Project Manager options...
      Opens the plugin options dialog (see OPTIONS).


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
  [Project sessions] - shows the sessions menu
  [Show current file in project tree]

The "Show current file" button is hidden if "Automatically show current
file in project tree" is enabled in options (it then happens on every
tab switch). The last button, [Favorites], opens the Favorites window.


------------------------------------------------------------------------------
FILTERING AND TYPE-AHEAD IN THE TREE
------------------------------------------------------------------------------

The field between the toolbar and the tree filters the project tree:
type a few letters and only the files and folders whose NAME contains
them are shown. A folder is shown when its own name matches, or when
something matches inside it - together with the path down to the match;
a folder whose name matches does not bring its whole content. The
"Ignore files/folders/hidden" options are respected while scanning.

The search runs in the background, so even a huge folder tree never
hangs CudaText, and the result is LIVE: rows appear while the scan runs
(a status line under the field counts the matches), and deeper levels
load lazily when you expand them - a big match list never floods the
tree at once. The x button at the right of the field cancels a running
search and KEEPS everything found so far; with no search running, x (or
Esc) clears the filter and restores the tree with the folds and the
selection from before the filtering. The scan starts 0.4 s after the
typing pauses, not on every keystroke.

While the panel has the keyboard focus (and you are not typing in the
filter field), plain letters and digits jump to the first node of the
current level whose name starts with the typed text - "myf" selects
"myfile". Pressing the same letter again jumps to the next match; the
typed text is forgotten after about a second of pause. Like in Windows
Explorer, the search is scoped to the folder you are in.


------------------------------------------------------------------------------
PLUGINS MENU COMMANDS
------------------------------------------------------------------------------

All commands are in the top menu: Plugins > Project Manager (and in the
command palette), grouped by purpose, most used first. The panel context
menu and the Plugins menu share every command that makes sense in both
places; commands that need a selected tree node (New file/folder,
Cut/Copy/Paste, Rename, Delete, ...) live only in the context menu.

  Show Project panel / Show and focus Project panel
      Open (and focus) the Project side panel.

  Go to file..., Go to main file, Open main file,
  Open all project files..., Show current file in project tree
      Quick navigation inside the project.

  Create new empty project... / Create new project from open tabs... /
  Open project... / Open recent project... / Save project /
  Save project as... / Backup project file now / Close project /
  Open project folder
      The project lifecycle: create, open, save, close.

  Add to project: folder... / file... / current file /
  all opened files / folder of the current file
  Clean up missing paths...
      Managing the project's root list.

  Project sessions >
      Open session..., Save session, Save session as...,
      Delete session..., Import external session... - the same items as
      the sessions menu of the panel.

  Favorites >
      Show Favorites window..., Add to favorites: current file /
      current folder / current project, Add opened... - see FAVORITES
      (BOOKMARKS) below.

  Close tabs not belonging to project
      Closes editor tabs whose files are not part of the project. Tabs
      with unsaved changes and untitled tabs get CudaText's own "save
      changes?" prompt, so nothing is lost silently. The "(no prompt)"
      variant closes all foreign tabs at once - unsaved changes in them
      are discarded.

  Project properties... / Project Manager options...
      Project variables and main file; plugin options. Both are also
      added to CudaText's Settings menu.


------------------------------------------------------------------------------
FAVORITES (BOOKMARKS)
------------------------------------------------------------------------------

Favorites are quick bookmarks for projects, files and folders,
independent of the opened project. They live in the "Favorites" submenus
(panel context menu and Plugins menu), on the toolbar button, and in
their own window.

THE FAVORITES WINDOW (toolbar button, or Favorites > Show Favorites
window...)
  A resizable, non-modal window with three tabs: Projects, Files,
  Folders. The active tab and the sort order are remembered between
  CudaText sessions. Columns: check | star | Name | Path | State; the
  column widths are resizable (drag the divider between two header
  captions), the Path column fills the remaining width.

  Checkbox column
      Check several rows, then run ONE command on all of them at once:
      open, star/unstar, copy paths, rename (one by one, Cancel stops),
      move up/down as a block, delete. The commands are in the
      "Checked" button and in the right-click menu; the status line
      shows how many rows are checked. Space checks/unchecks the
      selected row; checking never changes the selection.

  Star column
      The "favorite of favorites" flag: click the star cell of a row to
      toggle it. The default sort is starred first, then by name.

  State column
      Shows "?" when the favorite's path no longer exists on disk (a
      dead favorite); its name is painted in the theme's accent color
      too. Sort by it to gather the dead favorites at one end.

  Filter
      The field at the top filters by name AND path as you type; Esc
      clears it (a second Esc closes the window).

  Sorting
      Click a column header: 1st click sorts ascending, 2nd descending,
      3rd switches to manual (custom) order - the order that Move
      up/down and Alt+Up/Alt+Down edit. The right-click menu always has
      the same Sort commands.

  Buttons (right edge): Open, Add..., Add opened..., Checked,
  Move up / Move down, Delete, Close.

  Keyboard (active while the window has the focus):
      Ctrl+1..9 / Alt+1..9    open the favorite at that position
      Enter / double-click    open the selected favorite
      Del                     delete it (from the favorites list)
      F2                      rename it
      Space                   check/uncheck it
      Alt+Up / Alt+Down       move it up/down
      Esc                     clear the filter; second Esc closes

  Right-click menu: Open, Reveal in file manager, Copy path,
  Star/Unstar, Rename..., Delete, the "Checked (N):" commands, Sort
  commands, Add current file/folder/project. A right-click on the
  header offers the Sort commands + Check/Uncheck all.

  "Delete" here only removes the BOOKMARK from the list - it never
  touches the file/folder on the disk.

WHERE THE DATA LIVES
  settings/cuda_project_man_favorites.json (in the CudaText settings
  folder). Each entry keeps its path, display name (rename with F2) and
  star flag. The file is created on the first change.

IMPORT FROM THE "FAVORITES" PLUGIN (cuda_favorites)
  If you also use kvichans' "Favorites" plugin, its
  settings/cuda_favorites.json is READ (never written): entries missing
  from our list are imported when Project Manager starts and when a
  favorites window is opened. The sync is strictly one-way; entries you
  delete here are not resurrected by the next import.


------------------------------------------------------------------------------
OPTIONS
------------------------------------------------------------------------------

Open via the menu: Project Manager > Project Manager options..., the
panel context menu (at the bottom), or CudaText's Settings menu.

  Ignore files:           Semicolon-separated mask of filenames to hide
                          in the tree (e.g., "*.tmp;*.bak").

  Ignore folders:         Semicolon-separated mask of folder names to
                          hide (default: ".git;.svn").

  Ignore all hidden
  files/folders:          Hides files/folders with the hidden attribute.

  Recent projects:        Editable list of recently opened projects.

  Show toolbar:           Shows/hides the panel toolbar.

  Open file after
  "Go to file" command:   If checked, Go to file opens the file in the
                          editor; if unchecked, it only highlights it in
                          the tree.

  Use "preview tab" on
  item clicking:          Single-click opens files in a preview tab
                          (temporary, replaced on the next click).

  Open files by
  double-click:           If unchecked, single-click opens files. If
                          checked, single-click previews, double-click
                          opens permanently.

  Automatically show
  current file in
  project tree:           Highlights the current editor file in the tree
                          on every tab change. When enabled, the
                          "Show current file" toolbar button is hidden.

  File type icons (*):    Icon theme for file types in the tree.

  Toolbar icons (*):      Icon theme for the toolbar buttons.

  Sorting order (*):      How to sort items in the tree: by name,
                          extension, size, date, etc.

  Backup project file
  when its sessions
  change:                 Switches the AUTOMATIC backups on/off (see
                          "Project file backups"). Default: on. The
                          command "Backup project file now" works even
                          when this is unchecked.

  Backup folder:          Folder for the backups: an absolute path, or
                          a path relative to the project file (default:
                          ".cudatext/proj_bckp", so the backups live
                          next to the project). Supports $settings
                          (the CudaText settings folder) and "~" (the
                          home folder). Empty = the default.

  Backups to keep:        How many backup files to keep per project,
                          rotating (default: 4; 0 keeps none of the
                          automatic ones - "Backup project file now"
                          still works and always keeps the single
                          newest copy).

  Default projects
  folder:                 The folder where the "Create new project..."
                          and "Save project as..." dialogs start, so
                          you only type the project name. The folder is
                          created when it does not exist. Empty
                          (default) = the dialog starts in the last
                          used folder.

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
  Sessions store more than the list of open tabs: text typed but not
  yet saved to disk is stored inside the session too, and comes back as
  an unsaved tab when the session loads again. Before any command
  closes tabs (switching sessions, opening/creating/closing a project),
  the current state is saved into the session it belongs to, so unsaved
  text is never lost. If storing the state fails (e.g. a read-only
  project file), CudaText's own "save changes?" prompt appears for
  modified tabs instead - nothing is discarded silently.

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
  The plugin keeps rotating backups of the open project file: the whole
  .cuda-proj (nodes, vars and every embedded session) is copied into
  the backup folder whenever the file changed on disk - after every
  session save (including the ~40 s autosave), session switch and
  project save/switch/close, and on exit. Saves which rewrote nothing
  write nothing; a command that saves several times in a row makes one
  backup of the final state.

  NOTE: the periodic backups ride on CudaText's session autosave. If
  you disable CudaText's session saving ("Restore session" option, or
  a zero session-save interval), the periodic backups stop; a backup is
  then made only when a session is switched, when the project is
  saved/switched/closed, and on exit. The manual "Backup project file
  now" always works.

  Automatic backups are reported to the console panel (one line with
  the trigger and the time); the manual "Backup project file now"
  reports to the status bar.

  Backups are named "<project>.cuda-proj.1.bak" (newest) up to
  "<project>.cuda-proj.N.bak" (oldest, dropped when the set rotates).
  To restore one: close the project, copy the chosen .bak file over
  the .cuda-proj file, reopen the project.

  Caution: several projects with the same file name sharing ONE
  absolute backup folder collide (same backup names) - keep the default
  per-project folder, or use unique project file base names.

Broken project files
  Before opening, project files are validated: unreadable files,
  invalid JSON, or wrong key types are reported with a dialog and the
  project is not opened. An empty (0-byte) file is special-cased: the
  plugin offers to create a new empty project in it.

Exit
  On CudaText exit, the current project is saved (if it has a file
  path).


------------------------------------------------------------------------------
DRAG AND DROP
------------------------------------------------------------------------------

The project tree supports drag&drop in both directions. On Windows, the
drag to other applications uses the standard file format, so items can
be dropped into Explorer, archivers, mail clients, browser upload
dialogs etc.

Dragging a tree item to other applications
  Press the left mouse button on a tree item and move the mouse: the
  dragged file/folder follows the cursor and can be dropped into any
  application which accepts files. Esc cancels the drag. (On other
  OS-es the drag-out is not supported and the gesture is the inner
  drag&drop below.)

Dropping files from the file manager onto the panel
  Files/folders dropped from the OS file manager are added to the
  current project as root nodes (they are NOT opened in the editor -
  that is what drops onto the rest of the CudaText window do). Paths
  which cannot be added are not added silently: the statusbar message
  shows every rejected path with the reason.

Drag&drop inside the tree
  Drag a node and drop it onto another node of the tree. Every drop
  asks for confirmation:

  - Dragging a top-level NODE only changes its order (it is never moved
    inside another node). Dropping onto the project header (the
    top-most row) moves it to the end of the list. When "Sorting
    order" is not "none", a dialog offers to disable sorting first.
  - Dragging a FILE/FOLDER from inside a node onto a FOLDER moves it on
    disk into that folder, exactly like the plugin's Cut+Paste;
    the tree is refreshed. Refused (with an explaining message):
    overwriting an existing path, moving a folder into itself, moving
    into the folder it already is in.
  - Other drops are refused with an explaining statusbar message.

  A folded folder node unfolds after the cursor rests on it for a
  moment (0.7 s), like in OS file managers, so nested folders can be
  opened without interrupting the drag.


------------------------------------------------------------------------------
PROJECT FILE FORMAT
------------------------------------------------------------------------------

A .cuda-proj file is JSON. Minimal example (no sessions):

  {
    "version": 2,
    "nodes": ["C:\\code\\myapp"],
    "vars": ["ProjDir=C:\\code\\myapp"],
    "mainfile": "",
    "unfold": ["C:\\code\\myapp"]
  }

Sessions are stored under "sessions" as named objects; their internals
("tabs", "carets", ...) are managed by CudaText's session API. Files
without the "version" key (older plugin versions) are upgraded
automatically on open (see AUTOMATIC BEHAVIORS). Missing standard keys
are always auto-filled, so hand-edited or partial files load fine as
long as the present keys have the right types.

Paths inside "nodes" and "unfold" support the {ProjDir} macro: paths
inside the project file's folder are stored relative to it. This makes
projects portable - you can move the project file and its referenced
folders together.


------------------------------------------------------------------------------
KEYBOARD SHORTCUTS (project tree)
------------------------------------------------------------------------------

  Enter, Space or double-click   open the selected file / unfold the
                                 selected folder
  Del                            move to trash (files/folders inside a
                                 node; the dialog button says "Move to
                                 trash"), or - only on an OS without
                                 trash support - permanent deletion after
                                 the explicit warning dialog; a root
                                 node: stop tracking (nothing is deleted
                                 from the disk)
  F2                             rename
  F5 (or Ctrl+R)                 refresh the tree
  F7                             new folder
  Ctrl+N                         new file
  Ctrl+X / Ctrl+C / Ctrl+V       cut / copy / paste
  Alt+Enter                      properties
  letters, digits                type-ahead: jump to the node starting
                                 with the typed text
  Esc                            clear the tree filter

(Favorites-window shortcuts are listed in FAVORITES (BOOKMARKS).)


------------------------------------------------------------------------------
NOTES
------------------------------------------------------------------------------

- "Stop tracking this folder/file" only removes the top-level entry you
  right-clicked. If you right-click a file inside a tracked folder, the
  command walks up to the folder's root entry.

- Opening a .cuda-proj file directly (via File > Open or the command
  line) opens it as a project instead of showing raw JSON.

- The most recent project reopens automatically on CudaText start. The
  plugin is only loaded on start when a project is in use; users who
  don't use projects keep CudaText's fast startup.

- The session loaded when a project is opened is the one that was used
  last in it; it is remembered in the project file on every session
  switch.

- Developer documentation (design concepts, session handling rules,
  file format details for plugin authors) is in the file
  doc_for_dev.md next to the plugin source code.

- The trash support is powered by the bundled Send2Trash library
  (https://pypi.org/project/Send2Trash/) - it ships inside the
  plugin's send2trash/ subfolder, so nothing extra needs to be
  installed. It is loaded privately inside the plugin only: it never
  touches the global Python module path, so it cannot conflict with
  other plugins or with a Send2Trash you may have installed yourself.


------------------------------------------------------------------------------
AUTHORS
------------------------------------------------------------------------------

- Nikita Melentev  https://github.com/pohmelie
- Alexey Torgashin (CudaText)
- ildar r. khasanshin  https://github.com/ildarkhasanshin
- Badr ELmers  https://github.com/badrelmers

License: MIT. The plugin bundles the BSD-licensed Send2Trash library
(https://pypi.org/project/Send2Trash/) in its send2trash/ subfolder;
its license text is kept in send2trash/LICENSE.
