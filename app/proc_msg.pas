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
  cAppExeVersion = '1.2.28tst';
  cAppApiVersion = '1.0.131';

const
  msgTitle = 'CudaText';
  msgUntitledTab: string = 'Untitled';
  msgAllFiles: string = 'All files';
  msgDefTheme: string = '(default)';
  msgNoLexer: string = '(none)';

  msgEditCut: string = 'Cut';
  msgEditCopy: string = 'Copy';
  msgEditPaste: string = 'Paste';
  msgEditDelete: string = 'Delete';
  msgEditSelectAll: string = 'Select all';
  msgEditUndo: string = 'Undo';
  msgEditRedo: string = 'Redo';
  msgFileClearList: string = 'Clear list';

  msgButtonOk: string = 'OK';
  msgButtonCancel: string = 'Cancel';
  msgButtonApply: string = 'Apply';

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
  msgCannotFindLexlib: string = 'Cannot find lexer-library:';
  msgCannotSaveFile: string = 'Cannot save file:';
  msgCannotSaveUserConf: string = 'Cannot save user config (read only?)';
  msgCannotReadConf: string = 'Cannot read/parse config:';
  msgErrRegex: string = 'Incorrect regex passed:';
  msgThemeName: string = 'Theme name:';

  msgStatusNoCmtStr: string = 'No line comment defined for lexer';
  msgStatusReplaceCount: string = 'Replaces made: ';
  msgStatusFindCount: string = 'Count of "%s": ';
  msgStatusReadOps: string = 'Reading options';
  msgStatusSavedFile: string = 'Saved:';
  msgStatusReadonly: string = '[Read Only]';
  msgStatusMacroRec: string = '[Macro Rec]';
  msgStatusPicture: string = 'Image %dx%d';
  msgStatusCancel: string = 'Cancelled';
  msgStatusBadNum: string = 'Incorrect number entered';
  msgStatusEndsChanged: string = 'Line ends changed';
  msgStatusEncChanged: string = 'Encoding changed';
  msgStatusGotoLine: string = 'Go to line: %d';
  msgStatusGotoTab: string = 'Go to tab: %s';
  msgStatusSorted: string = 'Sorted %d lines';
  msgStatusLexlibSave: string = 'Lexer-library saved';

  msgConfirmSaveColors: string = 'Save theme to file?';
  msgConfirmOpenNotText: string = 'File is maybe not text:'#13'%s'#13#13'Do you want to open it?';
  msgConfirmReplaceText: string = 'Replace match at line %d ?';
  msgConfirmSaveModifiedTab: string = 'Tab is modified:'#13'%s'#13#13'Save it first?';
  msgConfirmReopenModifiedTab: string = 'Tab is modified:'#13'%s'#13#13'Reload it?';
  msgConfirmReloadFileWithEnc: string = 'Encoding is changed in memory.'#13'Do you also want to reload file?';
  msgConfirmCreateNewFile: string = 'File not found:'#13'%s'#13#13'Create it?';
  msgConfirmCreateUserConf: string = 'User config not found. Create it?';
  msgConfirmCloseDel: string = 'Close tab and delete its file?';
  msgConfirmDelLexer: string = 'Delete lexer "%s"?';
  msgConfirmSaveLib: string = 'Lexer-library modified. Save it?';

  msgAboutCredits =
      'Credits:'+sLineBreak+sLineBreak+
      'Lazarus IDE'+sLineBreak+
      'ATSynEdit, ATTabs, ATListbox, ATButton, Python wrapper: https://github.com/Alexey-T/'+sLineBreak+
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

