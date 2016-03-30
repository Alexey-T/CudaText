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
  cAppExeVersion = '1.3.4.4';
  cAppApiVersion = '1.0.133';

const
  msgTitle = 'CudaText';

const
  msgUntitledTab: string = 'Untitled';
  msgAllFiles: string = 'All files';
  msgNoLexer: string = '(none)';
  msgThemeDefault: string = '(default)';
  msgThemeName: string = 'Theme name:';

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

  msgEncReloadAs: string = 'Reload as';
  msgEncConvertTo: string = 'Convert to';
  msgEncEuropean: string = 'European';
  msgEncAsian: string = 'Asian';
  msgEncMisc: string = 'Misc';

  msgEndWin: string = 'Win';
  msgEndUnix: string = 'Unix';
  msgEndMac: string = 'MacOS9';

  msgCannotOpenFile: string = 'Cannot open file:';
  msgCannotFindFile: string = 'Cannot find file:';
  msgCannotFindLexerInLibrary: string = 'Cannot find lexer in library:';
  msgCannotFindLexerFile: string = 'Cannot find lexer file:';
  msgCannotFindSublexerInLibrary: string = 'Cannot find linked sublexer in library:';
  msgCannotCreateDir: string = 'Cannot create dir:';
  msgCannotSaveFile: string = 'Cannot save file:';
  msgCannotSaveUserConf: string = 'Cannot save user config (read only?)';
  msgCannotReadConf: string = 'Cannot read/parse config:';
  msgCannotReloadUntitledTab: string = 'Cannot reload untitled tab';
  msgCannotFindMatch: string = 'Cannot find';
  msgCannotFindInstallInfInZip: string = 'Cannot find install.inf in zip';
  msgCannotFindBookmarks: string = 'Cannot find bookmarks in text';

  msgStatusIncorrectFilename: string = 'Incorrect filename:';
  msgStatusIncorrectInstallInfInZip: string = 'Incorrect install.inf in zip';
  msgStatusUnsupportedAddonType: string = 'Unsupported addon type:';
  msgStatusPackageContains: string = 'This package contains:';
  msgStatusPackageName: string = 'name:';
  msgStatusPackageType: string = 'type:';
  msgStatusPackageDesc: string = 'description:';
  msgStatusPackageCommand: string = 'command:';
  msgStatusPackageEvents: string = 'events:';
  msgStatusInstalledNeedRestart: string = 'Program should be restarted to see new plugin';
  msgStatusErrorInConfigFile: string = 'Error in config file';
  msgStatusCommandOnlyForLexers: string = 'Command is only for lexers:';
  msgStatusOpenedBrowser: string = 'Opened browser';
  msgStatusCopiedLink: string = 'Copied link';
  msgStatusInstalled: string = 'Installed:';
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
  msgStatusCancelled: string = 'Cancelled';
  msgStatusBadLineNum: string = 'Incorrect number entered';
  msgStatusEndsChanged: string = 'Line ends changed';
  msgStatusEncChanged: string = 'Encoding changed';
  msgStatusGotoLine: string = 'Go to line: %d';
  msgStatusSorted: string = 'Sorted %d lines';
  msgStatusHelpOnKeysConfig: string = 'To customize hotkeys, call Commands dialog, focus any command, and press F9, you''ll see additional dialog';
  msgStatusClickingLogLine: string = 'Clicking log line';
  msgStatusNoGotoDefinitionPlugins: string = 'No goto-definition plugins installed for this lexer';
  msgStatusFilenameAlreadyOpened: string = 'File name is already opened in another tab:';
  msgStatusNeedToCloseTabSavedOrDup: string = 'You need to close tab: saved-as or duplicate.';
  msgStatusCommandsHaveSameHotkeys: string = 'Commands have same hotkeys:';
  msgStatusCorrectOneOfTheseHotkeys: string = 'Please correct one of these hotkeys.';

  msgConfirmInstallIt: string = 'Do you want to install it?';
  msgConfirmFileChangedOutside: string = 'File was changed outside:';
  msgConfirmReloadIt: string = 'Reload it?';
  msgConfirmReloadItHotkeys: string = '(Yes: reload. No: don''t reload. Cancel [Esc]: no more notifications about this file.)';
  msgConfirmOpenCreatedDoc: string = 'Open created document?';
  msgConfirmSaveColorsToFile: string = 'Save theme to file?';
  msgConfirmOpenNotText: string = 'File is maybe not text:'#13'%s'#13#13'Do you want to open it?';
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
      'ATSynEdit, ATTabs, ATGroups, ATListbox, ATButton, Python wrapper: https://github.com/Alexey-T/'+sLineBreak+
      'EControl parser: http://www.econtrol.ru, https://github.com/Alexey-T/'+sLineBreak+
      'Helper Python code: Andrey Kvichanskiy, https://github.com/kvichans/'+sLineBreak+
      'Toolbar icons: Silk icons, http://www.famfamfam.com'+sLineBreak+
      '    License: CC BY 2.5, http://creativecommons.org/licenses/by/2.5/'+sLineBreak+
      'App icon: Snipicons, http://www.snipicons.com/'+sLineBreak+
      '    License: CC BY-NC 3.0, http://creativecommons.org/licenses/by-nc/3.0/'+sLineBreak+
      '';

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

