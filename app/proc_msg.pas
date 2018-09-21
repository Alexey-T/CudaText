(*
This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at http://mozilla.org/MPL/2.0/.

Copyright (c) Alexey Torgashin
*)
unit proc_msg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
  cAppExeVersion = '1.61.2.0';
  cAppApiVersion = '1.0.256';

const
  cOptionSystemSuffix =
    {$ifdef windows} '' {$endif}
    {$ifdef linux} '__linux' {$endif}
    {$ifdef darwin} '__mac' {$endif}
    {$ifdef freebsd} '__freebsd' {$endif}
    ;

const
  msgTitle = 'CudaText'; //no need i18n
  msgPythonListError = 'Cannot create new list object'; //no need i18n
  msgCallbackBad = 'Bad API callback, report to plugin author: %s'; //no i18n
  msgCallbackDeprecated = 'Deprecated API callback, report to plugin author: %s'; //no i18n
  msgApiDeprecated = 'Deprecated API usage: %s'; //no i18n
  msgLiteLexerSuffix = ' ^'; //no i18n
  msgButtonX = '×'; //no i18n

  msgClearFilterTooltip: string = 'Clear filter';
  msgUntitledTab: string = 'Untitled';
  msgAllFiles: string = 'All files';
  msgNoLexer: string = '(none)';
  msgThemeDefault: string = '(default)';
  msgThemeName: string = 'Theme name:';

  msgPanelMenu_Init = 'Menu';
  msgPanelTree_Init = 'Code tree';
  msgPanelProject_Init = 'Project';
  msgPanelTabs_Init = 'Tabs';
  msgPanelConsole_Init = 'Console';
  msgPanelOutput_Init = 'Output';
  msgPanelValidate_Init = 'Validate';
  msgPanelSearch_Init = 'Search';

  msgPanelMenu: string = msgPanelMenu_Init;
  msgPanelTree: string = msgPanelTree_Init;
  msgPanelProject: string = msgPanelProject_Init;
  msgPanelTabs: string = msgPanelTabs_Init;

  msgFinderHintRegex: string = 'regex';
  msgFinderHintCase: string = 'case';
  msgFinderHintWords: string = 'words';
  msgFinderHintBack: string = 'back';
  msgFinderHintWrapped: string = 'wrapped';
  msgFinderHintInSel: string = 'in-sel';
  msgFinderHintFromCaret: string = 'from-caret';

  msgButtonOk: string = 'OK';
  msgButtonCancel: string = 'Cancel';
  msgButtonApply: string = 'Apply';
  msgButtonClose: string = 'Close';
  msgButtonYes: string = 'Yes';
  msgButtonNo: string = 'No';
  msgButtonYesAll: string = 'Yes to all';
  msgButtonNoAll: string = 'No to all';
  msgButtonAbort: string = 'Abort';
  msgButtonRetry: string = 'Retry';
  msgButtonIgnore: string = 'Ignore';

  msgEditCut: string = 'Cut';
  msgEditCopy: string = 'Copy';
  msgEditPaste: string = 'Paste';
  msgEditDelete: string = 'Delete';
  msgEditSelectAll: string = 'Select all';
  msgEditUndo: string = 'Undo';
  msgEditRedo: string = 'Redo';
  msgFileClearList: string = 'Clear list';
  msgCopySub: string = 'Copy to clipboard';
  msgCopyFilenameName: string = 'Copy filename only';
  msgCopyFilenameDir: string = 'Copy filepath only';
  msgCopyFilenameFull: string = 'Copy full filepath';

  msgEncReloadAs: string = 'Reload as';
  msgEncConvertTo: string = 'Convert to';
  msgEncEuropean: string = 'European';
  msgEncAsian: string = 'Asian';
  msgEncMisc: string = 'Misc';

  msgEndWin: string = 'Win';
  msgEndUnix: string = 'Unix';
  msgEndMac: string = 'MacOS9';

  msgCannotInitPython1: string = 'No Python engine (3.x) found. Python plugins don''t work now. To make it ok:';
  {$ifdef darwin}
  msgCannotInitPython2: string = 'install Python 3.x from www.python.org, it should be found by CudaText then.';
  {$else}
  msgCannotInitPython2: string = 'write option "pylib'+
                                 {$ifdef unix}cOptionSystemSuffix+{$endif}
                                 '" to user.json. See info in default config: Options / Settings-default.';
  {$endif}

  msgCannotOpenFile: string = 'Cannot open file:';
  msgCannotFindFile: string = 'Cannot find file:';
  msgCannotFindLexerInLibrary: string = 'Cannot find lexer in library:';
  msgCannotFindLexerFile: string = 'Cannot find lexer file:';
  msgCannotFindLexersAll: string = 'Cannot find lexers in data/lexlib/';
  msgCannotFindSublexerInLibrary: string = 'Cannot find linked sublexer:';
  msgCannotCreateDir: string = 'Cannot create dir:';
  msgCannotSaveFile: string = 'Cannot save file:';
  msgCannotSaveUserConf: string = 'Cannot save user config (read only?)';
  msgCannotReadConf: string = 'Cannot read/parse config:';
  msgCannotReloadUntitledTab: string = 'Cannot reload untitled tab';
  msgCannotFindInMultiSel: string = 'Cannot find in multi-selections, yet';
  msgCannotFindMatch: string = 'Cannot find';
  msgCannotFindInstallInfInZip: string = 'Cannot find install.inf in zip file';
  msgCannotFindBookmarks: string = 'Cannot find bookmarks in text';
  msgCannotHandleZip: string = 'Cannot handle zip file:';
  msgCannotSetHotkey: string = 'Cannot set hotkey for this item';
  msgCannotInstallAddonApi: string = 'Cannot install add-on "%s", it needs newer application version (API %s)';
  msgCannotInstallOnOS: string = 'Cannot install add-on "%s", it requires another OS (%s)';
  msgCannotInstallReqPlugin: string = 'Cannot install "%s", it requires missing plugin(s): %s';
  msgCannotInstallReqLexer: string = 'Cannot install "%s", it requires missing lexer(s): %s';
  msgCannotAutocompleteMultiCarets: string = 'Cannot auto-complete with multi-carets';

  msgStatusTabSize: string = 'Tab size';
  msgStatusWrapStates: array[0..2] of string = ('no wrap', 'wrap', 'wrap m');
  msgStatusIncorrectInstallInfInZip: string = 'Incorrect install.inf in zip';
  msgStatusUnsupportedAddonType: string = 'Unsupported addon type:';
  msgStatusPackageContains: string = 'This package contains:';
  msgStatusPackageName: string = 'name:';
  msgStatusPackageType: string = 'type:';
  msgStatusPackageDesc: string = 'description:';
  msgStatusPackageCommand: string = 'command:';
  msgStatusPackageEvents: string = 'events:';
  msgStatusPackageLexer: string = 'lexer:';
  msgStatusPackageAutoCompletion: string = 'static auto-completion:';
  msgStatusPackageMissedLexerMap: string = 'lexer misses themes support (.cuda-lexmap file)';
  msgStatusInstalledNeedRestart: string = 'Program should be restarted to see new plugin';
  msgStatusCommandOnlyForLexers: string = 'Command is only for lexers:';
  msgStatusOpenedBrowser: string = 'Opened browser';
  msgStatusCopiedLink: string = 'Copied link';
  msgStatusAddonInstalled: string = 'Addon installed';
  msgStatusOpened: string = 'Opened:';
  msgStatusReopened: string = 'Re-opened:';
  msgStatusBadRegex: string = 'Incorrect regex passed:';
  msgStatusFoundNextMatch: string = 'Found next match';
  msgStatusTryingAutocomplete: string = 'Trying auto-complete for:';
  msgStatusHelpOnShowCommands: string = 'Commands: F9 to configure keys; "@key" to find hotkey';
  msgStatusNoLineCmtDefined: string = 'No line comment defined for lexer';
  msgStatusReplaceCount: string = 'Replaces made: %d';
  msgStatusFindCount: string = 'Count of "%s": %d';
  msgStatusReadingOps: string = 'Reading options';
  msgStatusSavedFile: string = 'Saved:';
  msgStatusReadonly: string = '[Read Only]';
  msgStatusMacroRec: string = '[Macro Rec]';
  msgStatusPictureNxN: string = 'Image %dx%d';
  msgStatusHexViewer: string = 'Hex';
  msgStatusCancelled: string = 'Cancelled';
  msgStatusBadLineNum: string = 'Incorrect number entered';
  msgStatusEndsChanged: string = 'Line ends changed';
  msgStatusEncChanged: string = 'Encoding changed';
  msgStatusGotoLine: string = 'Go to line: %d';
  msgStatusGotoFileLineCol: string = 'File "%s", Line %d Col %d';
  msgStatusHelpOnKeysConfig: string = 'To customize hotkeys, call Help/Commands dialog, focus needed command, and press F9, you''ll see additional dialog';
  msgStatusClickingLogLine: string = 'Clicking log line';
  msgStatusNoGotoDefinitionPlugins: string = 'No goto-definition plugins installed for this lexer';
  msgStatusFilenameAlreadyOpened: string = 'File name is already opened in another tab:';
  msgStatusNeedToCloseTabSavedOrDup: string = 'You need to close tab: saved-as or duplicate.';
  msgStatusHotkeyBusy: string = 'Hotkey is busy: %s';
  msgStatusChangedLinesCount: string = 'Changed %d lines';

  msgConfirmHotkeyBusy: string = 'Hotkey is already occupied by command:'#13'%s'#13#13'Overwrite it?';
  msgConfirmSyntaxThemeSameName: string = 'Syntax theme exists, with the same name as UI theme. Do you want to apply it too?';
  msgConfirmInstallIt: string = 'Do you want to install it?';
  msgConfirmFileChangedOutside: string = 'File was changed outside:';
  msgConfirmReloadIt: string = 'Reload it?';
  msgConfirmReloadItHotkeys: string = '(Yes: reload. No: don''t reload. Cancel [Esc]: no more notifications about this file.)';
  msgConfirmReloadItHotkeysSess: string = '(Yes: reload. No: load text from previous session.)';
  msgConfirmOpenCreatedDoc: string = 'Open created document?';
  msgConfirmSaveColorsToFile: string = 'Save theme to file?';
  msgConfirmSaveModifiedTab: string = 'Tab is modified:'#13'%s'#13#13'Save it first?';
  msgConfirmReopenModifiedTab: string = 'Tab is modified:'#13'%s'#13#13'Reload it?';
  msgConfirmReloadFileWithEnc: string = 'Encoding is changed in memory.'#13'Do you also want to reload file?';
  msgConfirmCreateNewFile: string = 'File not found:'#13'%s'#13#13'Create it?';
  msgConfirmCreateUserConf: string = 'User config not found. Create it?';
  msgConfirmCloseDelFile: string = 'Close tab and delete its file?';
  msgConfirmDeleteLexer: string = 'Delete lexer "%s"?';
  msgConfirmRemoveStylesFromBackup: string = 'Remove checked styles from backup file?';

  msgAboutCredits =
      'Lazarus IDE'+sLineBreak+
      '    http://www.lazarus-ide.org'+sLineBreak+
      'ATSynEdit, ATTabs, ATFlatControls, Python wrapper'+sLineBreak+
      '    https://github.com/Alexey-T/'+sLineBreak+
      'EControl syntax parser'+sLineBreak+
      '    Delphi version: http://www.econtrol.ru'+sLineBreak+
      '    Lazarus port: https://github.com/Alexey-T/'+sLineBreak+
      'Helper Python code: Andrey Kvichanskiy'+sLineBreak+
      '    https://github.com/kvichans/'+sLineBreak+
      'Icons'+sLineBreak+
      '    Main icon'+sLineBreak+
      '      FTurtle'+sLineBreak+
      '    Theme for LibreOffice'+sLineBreak+
      '      https://github.com/libodesign/icons'+sLineBreak+
      '      License: Creative Commons BY-SA 3.0, http://creativecommons.org/licenses/by-sa/3.0/'+sLineBreak+
      '    Octicons'+sLineBreak+
      '      https://octicons.github.com/'+sLineBreak+
      '      License: MIT License'+sLineBreak+
      '    Visual Studio Code icons'+sLineBreak+
      '      https://github.com/vscode-icons/vscode-icons'+sLineBreak+
      '      License: MIT License'+sLineBreak+
      '    Hourglass/floppy icons'+sLineBreak+
      '      https://www.iconfinder.com/snipicons'+sLineBreak+
      '      License: Creative Commons BY-NC 3.0 Unported, http://creativecommons.org/licenses/by-nc/3.0/'+sLineBreak+
      '';

  msgCommandLineHelp =
      'Usage:'+sLineBreak+
      '  cudatext [ key ... ] filename ...'+sLineBreak+
      ''+sLineBreak+
      'Supported keys:'+sLineBreak+
      '  -h, --help      - Show this help'+sLineBreak+
      '  -v, --version   - Show application version'+sLineBreak+
      '  -n              - Ignore option "ui_one_instance", force new app window'+sLineBreak+
      '  -z=[text|binary|hex|unicode] - Open in viewer, with given mode'+sLineBreak+
      '  -r              - Open all files from command line in read-only mode'+sLineBreak+
      '  -e=value        - Open all files from command line in given encoding'+sLineBreak+
      '  -el             - Show supported encoding names'+sLineBreak+
      '  -nh             - Ignore saved file history'+sLineBreak+
      '  -w=left,top,width,height - Set position/size of app window'+sLineBreak+
      ''+sLineBreak+
      'Filenames can be with ":line" or ":line:column" suffix to place caret.'+sLineBreak+
      'Folder can be passed, will be opened in Project Manager plugin.'+sLineBreak+
      'Projects (.cuda-proj) can be passed, will be opened in Project Manager.'+sLineBreak+
      'Sessions (.cuda-session) can be passed, if Session Manager installed.'+sLineBreak;

  msgCommandLineUnknownOption =
      'CudaText: unknown key %s'#10'Run "cudatext --help" to see command line help.';
  msgCommandLineUnknownEncoding =
      'CudaText: unknown encoding: %s';
  msgCommandLineVersion =
      'CudaText version %s';


function GetUntitledCaption: string;


implementation

var
  FUntitledCount: integer = 0;

function GetUntitledCaption: string;
begin
  Inc(FUntitledCount);
  Result:= msgUntitledTab+IntToStr(FUntitledCount);
end;

end.

