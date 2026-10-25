=======================================================================
  cuda_testing_lexers - "Testing of Lexers" plugin for CudaText
  Testing of lexers before/after patches of the lexer engine (EControl)
  (github.com/Alexey-T/CudaText issue #6565)
=======================================================================

WHAT IT IS
----------

This plugin checks that patches of the CudaText lexer engine (EControl,
the "speed patches") make parsing FASTER but do NOT change what it
parses.

It works like this: it parses a corpus of test files with CudaText's
real lexers, saves the lexer data (tokens + folding) of every file as
JSON snapshots, and compares snapshots made by different CudaText
builds. If a speed patch changes even one token or one fold range,
the comparison reports that file as DIFFERENT.

IMPORTANT: THE results_base FOLDER
----------------------------------

The plugin creates and uses a special folder, "results_base", inside
the plugin folder. It is the BASELINE (reference) snapshot:

  results_base contains the JSONs captured in CudaText BEFORE adding
  the speed patches.

It is made once, with the unpatched CudaText build, by the command
"Make snapshot" (see below). After you add the speed patches to the
lexer engine and rebuild CudaText, every new snapshot you make is
compared against results_base:

  - all files IDENTICAL  ->  the speed patches did not change the
                              parsing results (only the speed), which
                              is the goal;
  - any file DIFFERENT   ->  the patch changed the parser output for
                              that file - the patch needs fixing.

Do not delete or edit results_base while you are testing patches: it
is the "known good" data everything is compared to. (You can recreate
it at any time: run "Make snapshot" and enter the name "base" to
overwrite it, or delete the folder and run "Make snapshot" again.)

REQUIREMENTS
------------

- CudaText with Python plugin support (a recent 1.237.x build).
- The plugin needs two APIs in your CudaText build:
  - ed.action(EDACTION_LEXER_SCAN, ...) - forced full parse.
    Without it, "Make snapshot" stops with a clear "Update CudaText"
    error and writes nothing.
  - app_proc(PROC_CONFIG_READ, ...) - pinning of session options
    (see "PINNED SESSION SETTINGS" below). On a very old build it is
    skipped; the snapshot then reports "settings NOT pinned" and big
  files (over ~2 MB) may be skipped as "no lexer".


FOLDER LAYOUT
-------------

  cuda_testing_lexers/
    lexers/                      put your test files here (flat list,
                                 no subfolders)
    results_base/                BASELINE snapshot: JSONs captured in
                                 CudaText BEFORE adding the speed
                                 patches (auto-created by the plugin)
    results_<name>/              snapshots you make later, e.g.
                                 results_patch1/

Every file from "lexers" gets two JSONs in the snapshot folder:

  <filename>_tokens.json    tokens: {"lexer", "line_count",
                           "token_count", "tokens": [
                           {"str","style","x1","y1","x2","y2","ki","ks"}, ...]}
  <filename>_fold.json      folds:  {"lexer", "fold_count",
                           "folds": [
                           {"x","y","x2","y2","staple","folded","hint","tag"}, ...]}

Data is saved EXACTLY as the CudaText API returned it - nothing is
sorted or normalized - so ANY change in tokens/folding is detected.
Each snapshot folder also gets "_info.txt" with the date, CudaText
version, parse/verification method, and the pinned settings.

COMMANDS
--------

Menu "Testing / Testing of Lexers" (or Command Palette):

1. Make snapshot
   Parses all files from the "lexers" folder.
   - If results_base does not exist yet, it is created (this is how
     you make the BEFORE-patches baseline).
   - If results_base already exists, the plugin asks for a name and
     saves to "results_<name>" (enter "base" to overwrite the
     baseline).
   - Files without a lexer, and files of "lite lexers" (they have no
     EControl tokens), are skipped and listed at the end.
   - Files which could not be parsed to the end (after all retries)
     are reported as errors - wrong data is never silently written.

2. Compare results
   Shows a menu of found "results_*" folders (results_base excluded),
   then compares the chosen folder against results_base, JSON-by-JSON,
   pair-by-name. Reports IDENTICAL / DIFFERENT per file, plus files
     present only in one folder.

3. Compare two folders...
   Asks for ANY two folders (two directory-picker dialogs) and
   compares their JSON snapshots pair-by-name. Use it to compare
   snapshots kept anywhere on disk, e.g. made on two different
   machines or two different CudaText builds.

4. Open folder...
   Opens the plugin folder, the "lexers" folder, or a "results_*"
   folder in the OS file manager.

Both compare commands show a summary dialog and a full report in a
new editor tab. Only statuses and item counts are shown - contents of
differences are not shown.

HOW TO USE IT: TESTING SPEED PATCHES
------------------------------------

1. Put your test files (all lexer types you care about, any sizes -
   also multi-MB ones) into the plugin's "lexers" folder.

2. Run CudaText built WITHOUT the speed patches (original build).

3. Menu: Testing / Testing of Lexers / Make snapshot.
   -> results_base is created: the JSONs captured in CudaText BEFORE
   adding the speed patches. This is your reference data.

4. Add the speed patches to the lexer engine, rebuild CudaText,
   restart it.

5. Menu: Testing / Testing of Lexers / Make snapshot.
   Enter a name, e.g. "patch1" -> results_patch1 is created.

6. Menu: Testing / Testing of Lexers / Compare results.
   Pick "results_patch1" - it is compared against results_base.

7. Read the report:
   - all files IDENTICAL: the speed patches kept the parsing results
     unchanged - success;
   - some files DIFFERENT: the patch changed the lexer output for
     those files - investigate before keeping the patch;
   - files only in one folder: the corpus changed between runs.

8. Repeat 4-6 for every new patch version (patch2, patch3, ...).
   Every new comparison is again made against the same results_base.

PINNED SESSION SETTINGS (CONSTANT RESULTS)
------------------------------------------

Before a snapshot parses the corpus, the plugin applies this JSON via
app_proc(PROC_CONFIG_READ, ...):

  {"auto_fold_comments": 2,
   "lexer_folding_max_lines": 1500000,
   "ui_max_size_lexer": 500,
   "fold_style": 4,
   "max_line_len_parsed": 40000000,
   "wrap_enabled_max_lines": 2100000,
   "max_line_len_for_editing_keeping_lexer": 0}

Why: all these options change what the lexer engine does with the
same file (lexer enabled/disabled by file size, folding computed or
not, fold "folded" flags, word wrap for huge files, dropped lexer on
long edited lines...). Their defaults differ between machines and
sessions - without pinning, two snapshots of the same files could
differ because of OPTIONS, not because of the tested patch. For
example, "ui_max_size_lexer" defaults to 2 MB, which silently turns
a 9 MB corpus file into "no lexer" and makes snapshots inconsistent.

The pinning is TEMPORARY: CudaText loads these values into memory
only, your "user.json" config is never touched, and after a CudaText
restart your own option values are back. Each snapshot's "_info.txt"
records which values were pinned.

BIG FILES (e.g. 9 MB)
---------------------

CudaText normally disables lexers for files over "ui_max_size_lexer"
(2 MB by default). The plugin pins that option to 500 MB for the
session (see above), so big corpus files parse without manual setup.

For big files the plugin also:
- forces ONE full parse per attempt (ed.action(EDACTION_LEXER_SCAN,
  "0") - synchronous, whole file from line 0);
- reads the token list in line chunks, pumping the UI between chunks
  (one giant API call would freeze CudaText for a long time);
- writes each file's JSONs right after that file, so big token lists
  never pile up in memory;
- verifies cheaply (a small window of tokens near the file end must
  reach the last non-blank line and stay stable) before reading the
  full list, and retries (up to 12 attempts, re-asserting the lexer
  from the 3rd attempt) only while data really does not reach the
  file end.

Verification exists because the engine publishes parse results
asynchronously: ed.action() can return before the data is published
(aborted parse also looks "finished"), which in earlier plugin
versions silently produced JSONs with "token_count: 0". Such files
are now detected and re-parsed, or reported as errors.

TROUBLESHOOTING
---------------

- "no lexer" for a big file:
  you are on a very old CudaText without the settings-pinning API.
  Update CudaText, or raise option "ui_max_size_lexer" in Options
  Editor yourself and run the snapshot again.
- File reported as error (never settled after retries):
  check the file in an editor tab manually - it may be crashing the
  parser; please report it together with your CudaText version.
- Different results for the same files across snapshots:
  make sure both snapshots were made by this plugin version or newer
  (old snapshots from earlier versions may contain empty token JSONs
  - remake results_base), and that the corpus files were not edited
  between runs.
- Plugin command does nothing / "Update CudaText" message:
  your CudaText is too old, update it (see REQUIREMENTS).
