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
  Classes, SysUtils,
  ATBinHex,
  ATSynEdit;

const
  cAppExeVersion = '1.194.1.0';
  cAppApiVersion = 445;

const
  cOptionSystemSuffix =
    {$ifdef windows} '' {$endif}
    {$ifdef linux} '__linux' {$endif}
    {$ifdef darwin} '__mac' {$endif}
    {$ifdef freebsd} '__freebsd' {$endif}
    {$ifdef netbsd} '__netbsd' {$endif}
    {$ifdef openbsd} '__openbsd' {$endif}
    {$ifdef dragonfly} '__dragonfly' {$endif}
    {$ifdef solaris} '__solaris' {$endif}
    {$ifdef haiku} '__haiku' {$endif}
    ;

  cSystemLibDir =
    {$ifdef windows} '' {$endif}
    {$ifdef linux} '/usr/lib' {$endif}
    {$ifdef darwin} '/Library/Frameworks' {$endif}
    {$ifdef freebsd} '/usr/local/lib' {$endif}
    {$ifdef netbsd} '/usr/pkg/lib' {$endif}
    {$ifdef openbsd} '/usr/local/lib' {$endif}
    {$ifdef dragonfly} '/usr/lib' {$endif}
    {$ifdef solaris} '/usr/lib/amd64' {$endif}
    {$ifdef haiku} '/system/lib' {$endif}
    ;

  cSystemHasPkExec =
    {$ifdef windows}   false {$endif}
    {$ifdef linux}     true {$endif}
    {$ifdef darwin}    false {$endif}
    {$ifdef freebsd}   true {$endif}
    {$ifdef netbsd}    true {$endif}
    {$ifdef openbsd}   true {$endif}
    {$ifdef dragonfly} true {$endif}
    {$ifdef solaris}   true {$endif}
    {$ifdef haiku}     false {$endif}
    ;

const
  EOL = #10;
  msgPythonListError = 'Cannot create new list object'; //no need i18n
  msgCallbackBad = 'NOTE: Bad API callback, report to plugin author: %s'; //no i18n
  msgCallbackDeprecated = 'NOTE: Deprecated API callback, report to plugin author: %s'; //no i18n
  msgApiDeprecated = 'NOTE: Deprecated API usage: %s'; //no i18n
  msgErrorInTheme = 'NOTE: Theme "%s" misses item "%s"';
  msgRescannedAllPlugins = 'Rescanned all plugins';
  msgWelcomeTabTitle = 'Welcome';
  msgSavedPythonLibOption = 'Saved the option "pylib'+cOptionSystemSuffix+'". Restart CudaText to apply it.';
  msgSearchingInDir = 'Searching:';
  msgErrorLowDiskSpaceMb = 'Free disk space is less than %d Mb. Try to free additional space, then press Retry.';
  msgErrorNullBytesInFile = 'Config file is broken, because its leading bytes are NULLs:'#10'%s'#10'Press OK to delete it.';

  msgCmdPaletteCaption: string = 'Command palette';
  msgCmdPaletteTextHint: string = 'F9: set hotkey; input "@hotkey": search';
  msgCmdPalettePrefixHelp: string = '#p – plugins'+EOL+'#l – lexers'+EOL+'#f – opened files'+EOL+'#r – recent files';

  msgErrorPluginIgnored = 'NOTE: Plugin %s is in ignore-list, please remove it';
  msgErrorTooManyFileTabs = 'NOTE: Too many editor-tabs are opened, cannot add tab';
  msgCannotFindLexers = 'NOTE: Cannot find lexers: %s';
  msgCannotFindData = 'ERROR: Cannot find data: %s';
  msgCannotFindSessionFile = 'NOTE: File from session not found: %s';
  msgBadLexerName = 'ERROR: Wrong lexer name in "newdoc_lexer": "%s"';

  msgTitle = 'CudaText'; //no i18n
  msgModified: array[boolean] of string = ('', '*'); //no i18n
  msgLiteLexerSuffix = ' ^'; //no i18n
  msgStatusbarCellColumnMarks: array[boolean] of string = ('-', '||');
  msgStatusbarCellInsOvr: array[boolean] of string = ('Ins', 'Ovr');

  msgDialogTitleOpen: string = 'Open file';
  msgDialogTitleSaveAs: string = 'Save file as';
  msgDialogTitleSelFolder: string = 'Select folder';

  msgFileMaybeNotText: string = 'File is maybe not text:';
  msgFileTooBig: string = 'File is too big to edit:';

  msgLinkOpenSite: string = 'Open link';
  msgLinkOpenEmail: string = 'Send e-mail';

  msgTooltipClearFilter: string = 'Clear filter';
  msgTooltipCloseTab: string = 'Close tab';
  msgTooltipAddTab: string = 'Add tab';
  msgTooltipArrowLeft: string = 'Scroll tabs left';
  msgTooltipArrowRight: string = 'Scroll tabs right';
  msgTooltipArrowMenu: string = 'Show tabs menu';

  msgUntitledEnglish = 'Untitled';
  msgUntitledTab: string = msgUntitledEnglish;
  msgAllFiles: string = 'All files';
  msgNoLexer: string = '(none)';
  msgThemeDefault: string = '(default)';
  msgThemeName: string = 'Theme name:';
  msgGotoDialogTooltip: string = '(10, 10:10, 10%%, d100, xFFF, %s)';
  msgGotoDialogInfoExt: string = 'with "+": select';
  msgIgnoredCommandIfNotFocused: string = 'Ignoring the command, editor isn''t focused';
  msgSuggestOptEditor = '"Options Editor" provides the dialog - click here to open';

  msgMenuTranslations: string = 'Translations';
  msgMenuThemesUI: string = 'UI themes';
  msgMenuThemesSyntax: string = 'Syntax themes';
  msgMenuLexersForFile: string = 'Lexer for "%s"';

  msgPanelMenu_Init = 'Menu';
  msgPanelTree_Init = 'Code tree';
  msgPanelProject_Init = 'Project';
  msgPanelTabs_Init = 'Tabs';
  msgPanelSnippet_Init = 'Snippet Panel';

  msgPanelConsole_Init = 'Console';
  msgPanelOutput_Init = 'Output';
  msgPanelValidate_Init = 'Validate';
  msgPanelSearch_Init = 'Search';

  msgPanelMenu: string = msgPanelMenu_Init;
  msgPanelTree: string = msgPanelTree_Init;
  msgPanelProject: string = msgPanelProject_Init;
  msgPanelTabs: string = msgPanelTabs_Init;
  msgPanelSnippet: string = msgPanelSnippet_Init;

  msgPanelConsole: string = msgPanelConsole_Init;
  msgPanelOutput: string = msgPanelOutput_Init;
  msgPanelValidate: string = msgPanelValidate_Init;
  msgPanelSearch: string = msgPanelSearch_Init;

  msgFinderHintRegex: string = 'regex';
  msgFinderHintCase: string = 'case';
  msgFinderHintWords: string = 'words';
  msgFinderHintBack: string = 'back';
  msgFinderHintWrapped: string = 'wrapped';
  msgFinderHintInSel: string = 'in-sel';
  msgFinderHintFromCaret: string = 'from-caret';
  msgFinderHintSearchWrapped: string = 'edge-cross';
  msgFinderHintPresCase: string = 'pres-case';
  msgFinderRegexMathes: string = 'RegEx matches';

  msgButtonOk: string = 'OK';
  msgButtonCancel: string = 'Cancel';
  msgButtonApply: string = 'Apply';
  msgButtonClose: string = 'Close';
  msgButtonYes: string = '&Yes';
  msgButtonNo: string = '&No';
  msgButtonYesAll: string = 'Yes to &all';
  msgButtonNoAll: string = 'No to all';
  msgButtonAbort: string = 'Abort';
  msgButtonRetry: string = 'Retry';
  msgButtonIgnore: string = 'Ignore';

  msgFileNew: string = 'New file';
  msgFileOpen: string = 'Open file...';
  msgFileSave: string = 'Save file';
  msgFileClearList: string = 'Clear list';
  msgCopySub: string = 'Copy to clipboard';
  msgCopyFilenameName: string = 'Copy filename only';
  msgCopyFilenameDir: string = 'Copy filepath only';
  msgCopyFilenameFull: string = 'Copy full filepath';
  msgCopyCurrentLine: string = 'Copy current line';

  msgEncReloadAs: string = 'Reload as';
  msgEncConvertTo: string = 'Convert to';

  msgEndWin = 'CRLF';
  msgEndUnix = 'LF';
  msgEndMac = 'CR';

  msgTabsizeUseSpaces: string = 'Use spaces';
  msgTabsizeConvTabs: string = 'Convert indentation to spaces';
  msgTabsizeConvSpaces: string = 'Convert indentation to tabs';

  msgPythonFindCaption: string = 'Find Python library';
  msgPythonFindCaptionLong = 'Python engine was not found; find it...';
  msgPythonFindFromDir: string = 'Search from folder:';
  msgCannotFindPython: string = 'Cannot find Python library';

  msgCannotInitPython1: string = 'NOTE: No Python 3 engine found. Python plugins don''t work now. To fix this:';
  {$ifdef darwin}
  msgCannotInitPython2: string = 'install Python 3.x from www.python.org, it should be found by CudaText then.';
  msgCannotInitPython2b: string = '';
  {$else}
    {$ifdef windows}
    msgCannotInitPython2: string = 'place near cudatext.exe: python3x.dll, python3x.zip, python3xdlls\*.pyd, MS VS Runtime.';
    msgCannotInitPython2b: string = '';
    {$else}
    msgCannotInitPython2: string = '* write "pylib'+cOptionSystemSuffix+
                                   '" to user.json. Read about "pylib" in "Options / Settings-default".';
    msgCannotInitPython2b: string = '* or use the menu item "Plugins / '+msgPythonFindCaptionLong+'"';
    {$endif}
  {$endif}

  msgCannotOpenFile: string = 'Cannot open file:';
  msgCannotFindFile: string = 'Cannot find file:';
  msgCannotHandleSplittedTab: string = 'Cannot handle the splitted UI-tab';
  msgCannotHandleUntitledTab: string = 'Cannot handle the untitled document';
  msgCannotFindLexerInLibrary: string = 'Cannot find lexer in library:';
  msgCannotFindLexerFile: string = 'Cannot find lexer file:';
  msgCannotLoadLexerFile: string = 'Cannot load lexer file:';
  msgCannotFindSublexerInLibrary: string = 'Cannot find linked sublexer:';
  msgCannotFindWithoutCaret: string = 'Cannot find/replace without caret';
  msgCannotCreateDir: string = 'Cannot create folder:';
  msgCannotSaveFile: string = 'Cannot save file:';
  msgCannotSaveFileWithEnc: string = 'Could not save file because encoding "%s" cannot handle Unicode text. Program has saved file in UTF-8 encoding.';
  msgCannotSaveUserConf: string = 'Cannot save user config (read only?)';
  msgCannotLoadFileInUTF8: string = 'Cannot load file in UTF-8, incorrect UTF-8 sequence';
  msgCannotReadConfig: string = 'Cannot parse config:';
  msgCannotReloadUntitledTab: string = 'Cannot reopen untitled tab';
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
  msgCannotFindPkExec: string = 'Cannot find "pkexec" program to copy as root.';
  msgCannotSetWrap: string = 'Cannot set word-wrap mode. Line count %d is bigger than value of option "wrap_enabled_max_lines": %d.';

  msgStatusbarTextTab: string = 'Tab';
  msgStatusbarTextSpaces: string = 'Spaces';

  msgStatusbarTextLine: string = 'Ln';
  msgStatusbarTextCol: string = 'Col';
  msgStatusbarTextSel: string = 'sel';
  msgStatusbarTextLinesSel: string = 'lines sel';
  msgStatusbarTextCarets: string = 'carets';

  msgStatusbarWrapStates: array[0..Ord(High(TATEditorWrapMode))] of string =
    ('no wrap', 'wrap', 'margin');

  msgStatusbarHintCaret: string = 'Caret position, selection';
  msgStatusbarHintEnc: string = 'File encoding';
  msgStatusbarHintLexer: string = 'Lexer (language)';
  msgStatusbarHintEnds: string = 'End-of-line chars';
  msgStatusbarHintSelMode: string = 'Mouse selection mode (normal/column) and Read-Only state';
  msgStatusbarHintTabSize: string = 'Tabulation width, by space-chars';
  msgStatusbarHintInsOvr: string = 'Insert/Overwrite mode';
  msgStatusbarHintWrap: string = 'Word wrap (off, by window, by fixed margin)';
  msgStatusbarHintZoom: string = 'Zoom value';

  msgStatusI18nEnglishAfterRestart: string = 'English translation will be applied after program restart';
  msgStatusI18nPluginsMenuAfterRestart: string = 'Translations of Plugins menu and plugin''s dialogs will be applied after program restart';

  msgStatusSavedTempFile: string = 'Saved to a temporary file:';
  msgStatusSaveIsIgnored: string = '"Save" command ignored (file is not modified yet)';
  msgStatusPluginHotkeyBusy: string = 'Note: hotkey [%s] is busy, skipped it';
  msgStatusSyntaxThemesOff: string = 'Syntax themes are turned off by option "ui_lexer_themes": false. So the following dialog will have no effect. To customize styles, use "Lexer properties" dialog.';
  msgStatusIncorrectInstallInfInZip: string = 'Incorrect install.inf in zip';
  msgStatusUnsupportedAddonType: string = 'Unsupported addon type:';
  msgStatusPackageContains: string = 'This package contains:';
  msgStatusPackageName: string = 'name:';
  msgStatusPackageType: string = 'type:';
  msgStatusPackageDesc: string = 'description:';
  msgStatusPackageCommand: string = 'command:';
  msgStatusPackageEvents: string = 'events:';
  msgStatusPackageLexer: string = 'lexer:';
  msgStatusPackageLexerSettings: string = 'lexer settings:';
  msgStatusPackageAutoCompletion: string = 'static auto-completion:';
  msgStatusPackageData: string = 'data:';
  msgStatusPackagePackage: string = 'package:';
  msgStatusPackageMissedLexerMap: string = 'lexer misses themes support (.cuda-lexmap file)';
  msgStatusInstalledNeedRestart: string = 'Package will take effect after program restart';
  msgStatusCommandOnlyForLexers: string = 'Command is only for lexers:';
  msgStatusOpenedBrowser: string = 'Opened browser';
  msgStatusCopiedLink: string = 'Copied link';
  msgStatusAddonInstalled: string = 'Package installed';
  msgStatusAddonsInstalled: string = 'Installed several packages (up to %d)';
  msgStatusOpened: string = 'Opened:';
  msgStatusReopened: string = 'Reopened:';
  msgStatusBadRegex: string = 'Incorrect regex passed:';
  msgStatusFoundNextMatch: string = 'Found next match';
  msgStatusTryingAutocomplete: string = 'Trying auto-complete for:';
  msgStatusNoLineCmtDefined: string = 'No line comment defined for lexer';
  msgStatusReplaceCount: string = 'Replaces made: %d';
  msgStatusFindCount: string = 'Count of "%s": %d';
  msgStatusFoundFragments: string = 'Found %d different fragment(s)';
  msgStatusReadingOps: string = 'Reading options';
  msgStatusSavedFile: string = 'Saved:';
  msgStatusPictureNxN: string = 'Image %dx%d';
  msgStatusHexViewer: string = 'Hex';
  msgStatusCancelled: string = 'Cancelled';
  msgStatusBadLineNum: string = 'Incorrect number entered';
  msgStatusEndsChanged: string = 'Line ends changed';
  msgStatusEncChanged: string = 'Encoding changed';
  msgStatusGotoFileLineCol: string = 'File "%s", Line %d Col %d';
  msgStatusClickingLogLine: string = 'Clicking log line';
  msgStatusNoGotoDefinitionPlugins: string = 'No goto-definition plugins installed for this lexer';
  msgStatusFilenameAlreadyOpened: string = 'File name is already opened in another tab:';
  msgStatusNeedToCloseTabSavedOrDup: string = 'You need to close tab: saved-as or duplicate.';
  msgStatusHotkeyBusy: string = 'Hotkey is busy: %s';
  msgStatusChangedLinesCount: string = 'Changed %d lines';
  msgStatusFontSizeChanged: string = 'Font size changed to %d%%';
  msgStatusLexerDisabledBySize: string = 'Option "ui_max_size_lexer":%d disables lexer "%s" in this file (%d Mb)';

  msgConfirmHotkeyBusy: string = 'Hotkey is already occupied by command:'#10'%s'#10#10'Overwrite it?';
  msgConfirmHotkeyList: string = 'hotkeys (%d): %s';
  msgConfirmOkNoHotkeys: string = 'OK without hotkeys';
  msgConfirmSyntaxThemeSameName: string = 'Syntax theme exists, with the same name as UI theme. Do you want to apply it too?';
  msgConfirmInstallIt: string = 'Do you want to install it?';
  msgConfirmFileChangedOutside: string = 'File was changed outside:';
  msgConfirmFileDeletedOutside: string = 'File was deleted outside:';
  msgConfirmReloadIt: string = 'Reload it?';
  msgConfirmReloadYes: string = 'Reload';
  msgConfirmReloadNoMore: string = 'No more notifications';
  msgConfirmReloadItHotkeysSess: string = '(Yes: reopen. No: open text from previous session.)';
  msgConfirmOpenCreatedDoc: string = 'Open created document?';
  msgConfirmSaveColorsToFile: string = 'Save theme to file?';
  msgConfirmSaveModifiedTab: string = 'Tab is modified:'#10'%s'#10#10'Save it first?';
  msgConfirmClosePinnedTab: string = 'Tab is pinned:'#10'%s'#10#10'Are you sure you want to close it?';
  msgConfirmReopenModifiedTab: string = 'Tab is modified:'#10'%s'#10#10'Reopen it?';
  msgConfirmReloadFileWithEnc: string = 'Encoding is changed in memory.'#10'Do you also want to reopen file?';
  msgConfirmCreateNewFile: string = 'File not found:'#10'"%s"'#10#10'Create it?';
  msgConfirmCreateUserConfig: string = 'User config not found. Create it?';
  msgConfirmCloseAndDeleteFile: string = 'Close tab and delete its file?';
  msgConfirmDeleteLexer: string = 'Delete lexer "%s"?';
  msgConfirmRemoveStylesFromBackup: string = 'Remove checked styles from backup file?';
  msgConfirmReplaceGlobal: string = 'This will perform mass replace in all opened documents. This will also reset all selections. Continue?';

  msgCommandNeedsPython: string =
    'This command requires Python engine.'
    {$ifndef windows}
    +' Set proper value of "pylib'+cOptionSystemSuffix+'" in the user.json.'
    {$endif}
    ;

  msgCommandLineHelp =
      'Usage:'+EOL+
      '  cudatext [ key ... ] filename ...'+EOL+
      ''+EOL+
      'Supported keys:'+EOL+
      '  -h, --help      - Show this help and exit'+EOL+
      '  -v, --version   - Show application version and exit'+EOL+
      '  -z=[text|binary|hex|unicode|uhex] - Open arguments in internal viewer'+EOL+
      '  -r              - Open arguments in read-only mode'+EOL+
      '  -e=value        - Open arguments in given encoding'+EOL+
      '  -el             - Show supported encoding names, exit'+EOL+
      '  -n              - Ignore option "ui_one_instance", open new app window'+EOL+
      '  -nh             - Ignore saved file history'+EOL+
      '  -nsl            - No session loading on start'+EOL+
      '  -nss            - No session saving on exit'+EOL+
      '  -ns             - The same as -nsl and -nss'+EOL+
      '  -nn             - Don''t suggest to create new file if filename not found'+EOL+
      '  -s=folder       - Set full path of "settings" folder'+EOL+
      '  -i              - Open contents of stdin in new tab (Unix only)'+EOL+
      '  -verbose        - Copy Python messages to stdout (Unix only)'+EOL+
      '  -id=name - Set single-instance id, for groups of instances (Unix, default: cudatext.0)'+EOL+
      '  -w=left,top,width,height        - Set position/size of the main window'+EOL+
      '  -c=cuda_module,method           - Run command plugin on startup'+EOL+
      '  -p=cuda_module#param1#param2... - Run event "on_cli" on startup'+EOL+
      ''+EOL+
      'Filenames can be with ":line" or ":line:column" suffix to place caret.'+EOL+
      'Folder can be passed, will be opened in Project Manager plugin.'+EOL+
      'Projects (*.cuda-proj) can be passed, will be opened in Project Manager.'+EOL+
      'Sessions (*.cuda-session) can be passed, even without Session Manager plugin.'+EOL;

  msgFirstStartInfo =
      '            ____          _      _____         _   '+EOL+
      '           / ___|   _  __| | __ |_   _|____  _| |_ '+EOL+
      '          | |  | | | |/ _` |/ _` || |/ _ \ \/ / __|'+EOL+
      '          | |__| |_| | (_| | (_| || |  __/>  <| |_ '+EOL+
      '           \____\__,_|\__,_|\__,_||_|\___/_/\_\\__|'+EOL+
      '---------------------------------------------------------------'+EOL+
      'This is the first CudaText start (file history.json not found).'+EOL+
      'If Python engine is set up, you can install popular add-ons'+EOL+
      'using menu item "Plugins / Multi Installer".'+EOL+
      '---------------------------------------------------------------'+EOL+
      'You can double-click this video link:'+EOL+
      '* tour in 14 minutes: https://www.youtube.com/watch?v=q8edzSd400Y'+EOL+
      '---------------------------------------------------------------'+EOL;

const
  msgDefault: string = 'Default';
  msgTreeSorted: string = 'Sorted';

  msgViewer: string = 'Viewer';
  msgViewerModes: array[TATBinHexMode] of string = (
    'Text',
    'Binary',
    'Hex',
    'Unicode',
    'Unicode/Hex'
    );

  msgTextCaseMenu: string = 'Convert case';
  msgTextCaseUpper: string = 'Upper case';
  msgTextCaseLower: string = 'Lower case';
  msgTextCaseTitle: string = 'Title case';
  msgTextCaseInvert: string = 'Invert case';
  msgTextCaseSentence: string = 'Sentence case';

  msgCommentLineAdd: string = 'Line comment: add';
  msgCommentLineDel: string = 'Line comment: remove';
  msgCommentLineToggle: string = 'Line comment: toggle';
  msgCommentStreamToggle: string = 'Stream comment: toggle';

  msgConsoleClear: string = 'Clear';
  msgConsoleToggleWrap: string = 'Toggle word wrap';
  msgConsoleNavigate: string = 'Navigate';

  msgFindHint_InputFind: string = 'Find what';
  msgFindHint_InputRep: string = 'Replace with';
  msgFindHint_FindFirst: string = 'Find first';
  msgFindHint_FindNext: string = 'Find next';
  msgFindHint_FindPrev: string = 'Find previous';
  msgFindHint_Rep: string = 'Replace next match';
  msgFindHint_RepAll: string = 'Replace all matches in current document';
  msgFindHint_RepGlobal: string = 'Replace all matches in all opened tabs';
  msgFindHint_Regex: string = 'Regular expressions';
  msgFindHint_RegexSubst: string = 'RegEx substitution for ''Replace with''';
  msgFindHint_Case: string = 'Case sensitive';
  msgFindHint_Words: string = 'Whole words';
  msgFindHint_Wrapped: string = 'Wrapped search';
  msgFindHint_ConfirmRep: string = 'Confirm on replace';
  msgFindHint_MultiLine: string = 'Multi-line inputs (Ctrl+Enter for new-line)';
  msgFindHint_InSelect: string = 'Search in selection';
  msgFindHint_Tokens: string = 'Allowed syntax elements';
  msgFindHint_HiAll: string = 'Highlight all matches';
  msgFindHint_PresCase: string = 'Preserve case on replacement';

function msgTranslatedPanelCaption(const ACaption: string): string;
function msgFinderRegexMatchesNumbered: string;
function msgTranslatedUntitledTab(const ACaption: string): string;

implementation

uses
  ATStringProc;

function msgTranslatedPanelCaption(const ACaption: string): string;
begin
  case ACaption of
    msgPanelTree_Init:
      Result:= msgPanelTree;
    msgPanelProject_Init:
      Result:= msgPanelProject;
    msgPanelTabs_Init:
      Result:= msgPanelTabs;
    msgPanelSnippet_Init:
      Result:= msgPanelSnippet;
    msgPanelConsole_Init:
      Result:= msgPanelConsole;
    msgPanelOutput_Init:
      Result:= msgPanelOutput;
    msgPanelValidate_Init:
      Result:= msgPanelValidate;
    else
      Result:= ACaption;
  end;
end;

function msgFinderRegexMatchesNumbered: string;
const
  NCounter: integer = 0;
begin
  Inc(NCounter);
  Result:= msgFinderRegexMathes+' '+IntToStr(NCounter);
end;

function msgTranslatedUntitledTab(const ACaption: string): string;
var
  StrIndex: string;
begin
  if SBeginsWith(ACaption, msgUntitledEnglish) then
  begin
    StrIndex:= Copy(ACaption, Length(msgUntitledEnglish)+1, MaxInt);
    Result:= msgUntitledTab+StrIndex;
  end
  else
    Result:= ACaption;
end;


end.

