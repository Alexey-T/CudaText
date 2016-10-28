(*
This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at http://mozilla.org/MPL/2.0/.

Copyright (c) Alexey Torgashin
*)
unit proc_globdata;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Menus,
  Dialogs, Graphics, ExtCtrls, ComCtrls,
  LclProc, LclType, LazFileUtils, LazUTF8,
  IniFiles, jsonConf,
  Process,
  ATSynEdit,
  ATSynEdit_Keymap,
  ATSynEdit_Keymap_Init,
  ATStringProc,
  ATButtons,
  ATListbox,
  proc_cmd,
  proc_lexer,
  proc_msg,
  proc_scrollbars,
  proc_keymap_undolist,
  ecSyntAnal;

var
  AppBookmarkSetup: array[1..255] of
    record ImageIndex: integer; Color: TColor; end;
  AppBookmarkImagelist: TImageList = nil;
  AppFolderOfLastInstalledAddon: string = '';

const
  AppExtensionThemeUi = '.cuda-theme-ui';
  AppExtensionThemeSyntax = '.cuda-theme-syntax';


type
  TAppPathId = (
    cDirSettings,
    cDirSettingsDef,
    cDirData,
    cDirDataLexlib,
    cDirDataNewdoc,
    cDirDataThemes,
    cDirDataAcp,
    cDirDataAcpSpec,
    cDirDataLangs,
    cDirReadme,
    cDirLastInstalledAddon,
    cDirPy,
    cFileOptHistory,
    cFileOptDefault,
    cFileOptUser,
    cFileOptFiletypes,
    cFileOptKeymap,
    cFileHistoryList,
    cFileLexerStyles,
    cFileReadmeHist,
    cFileReadmeMouse,
    cFileReadmeLexerInst
    );

type
  TUiOps = record
    VarFontName: string;
    VarFontSize: integer;

    OutputFontName: string;
    OutputFontSize: integer;

    DoubleBuffered: boolean;

    PyLibrary: string;
    LexerThemes: boolean;
    PictureTypes: string;
    MaxFileSizeToOpen: integer;
    MaxFileSizeForLexer: integer;

    AutocompleteCss: boolean;
    AutocompleteHtml: boolean;
    AutocompleteAutoshowChars: integer;
    AutocompleteAutoshowLexers: string;
    AutoCloseBrackets: string;

    ListboxSizeX: integer;
    ListboxSizeY: integer;
    ListboxCompleteSizeX: integer;
    ListboxCompleteSizeY: integer;
    ListboxFuzzySearch: boolean;

    TabWidth: integer;
    TabHeight: integer;
    TabHeightInner: integer;
    TabIndentTop: integer;
    TabIndentInit: integer;
    TabAngle: integer;
    TabBottom: boolean;
    TabColorFull: boolean;
    TabShowX: integer;
    TabShowPlus: boolean;
    TabDblClickClose: boolean;
    TabNumbers: boolean;

    MaxHistoryEdits: integer;
    MaxHistoryMenu: integer;
    MaxHistoryFiles: integer;

    FindSuggestSel: boolean;
    FindSuggestWord: boolean;
    FindSelCase: integer;
    FindShowFindfirst: boolean;
    FindIndentVert: integer;
    FindIndentHorz: integer;
    FindMultiLineScale: double;
    FindSeparateForm: boolean;

    EscapeClose: boolean;
    EscapeCloseConsole: boolean;
    InitialDir: string;

    ExportHtmlNumbers: boolean;
    ExportHtmlFontName: string;
    ExportHtmlFontSize: integer;

    TreeAutoSync: boolean;
    TreeTimeFill: integer;
    TreeTimeFocus: integer;
    TreeShowLines: boolean;
    TreeShowIcons: boolean;
    PyChangeSlow: integer;

    NewdocLexer: string;
    NewdocEnc: string;
    NewdocEnds: integer;

    DefaultEncUtf8: boolean;

    StatusNoSel: string;
    StatusSmallSel: string;
    StatusStreamSel: string;
    StatusColSel: string;
    StatusCarets: string;
    StatusPanels: string;
    StatusHeight: integer;
    StatusTime: integer;
    StatusAltTime: integer;
    StatusTabsize: string;

    ShowTitlePath: boolean;
    ShowLastFiles: boolean;
    OneInstance: boolean;
    NotifEnabled: boolean;
    NotifTimeSec: integer;
    NonTextFiles: integer; //0: prompt, 1: open, 2: don't open
    NonTextFilesBufferKb: integer;
    LexerMenuGrouped: boolean;
    ReloadFollowTail: boolean;
    BackupMode: string;
    FullScreenHide: string;
  end;
var
  UiOps: TUiOps;

const
  cOptionSystemSuffix =
    {$ifdef windows} '' {$endif}
    {$ifdef linux} '__linux' {$endif}
    {$ifdef darwin} '__osx' {$endif} ;
  str_FontName = 'font_name'+cOptionSystemSuffix;
  str_FontSize = 'font_size'+cOptionSystemSuffix;
  str_FontQuality = 'font_quality'+cOptionSystemSuffix;
  str_UiFontName = 'ui_font_name'+cOptionSystemSuffix;
  str_UiFontSize = 'ui_font_size'+cOptionSystemSuffix;
  str_UiFontOutputName = 'ui_font_output_name'+cOptionSystemSuffix;
  str_UiFontOutputSize = 'ui_font_output_size'+cOptionSystemSuffix;
  str_UiDoubleBuffered = 'ui_buffered'+cOptionSystemSuffix;

type
  TEditorOps = record
    OpFontName: string;
    OpFontSize: integer;
    OpFontQuality: TFontQuality;

    OpSpacingX: integer;
    OpSpacingY: integer;
    OpTabSize: integer;
    OpTabSpaces: boolean;

    OpOvrSel: boolean;
    OpOvrOnPaste: boolean;
    OpUnderlineColorFiles: string;
    OpUnderlineColorSize: integer;
    OpLinks: boolean;
    OpLinksRegex: string;

    //view
    OpGutterShow: boolean;
    OpGutterFold: boolean;
    OpGutterFoldAlways: boolean;
    OpGutterBookmk: boolean;
    OpNumbersShow: boolean;
    OpNumbersFontSize: integer;
    OpNumbersStyle: integer;
    OpNumbersForCarets: boolean;
    OpNumbersCenter: boolean;
    OpRulerShow: boolean;
    OpRulerFontSize: integer;
    OpRulerSize: integer;
    OpRulerTextIndent: integer;
    OpMinimapShow: boolean;
    OpMinimapShowSelAlways: boolean;
    OpMinimapShowSelBorder: boolean;
    OpMinimapCharWidth: integer;
    OpMinimapAtLeft: boolean;
    OpMicromapShow: boolean;
    OpMicromapWidth: integer;
    OpMargin: integer;
    OpMarginString: string;
    OpStaplesShow: boolean;

    //unprinted
    OpUnprintedShow: boolean;
    OpUnprintedSpaces: boolean;
    OpUnprintedEnds: boolean;
    OpUnprintedEndDetails: boolean;
    OpUnprintedReplaceSpec: boolean;
    OpUnprintedReplaceToCode: string;

    OpUnprintedEndArrow: boolean;
    OpUnprintedTabArrowLen: integer;
    OpUnprintedSpaceDotScale: integer;
    OpUnprintedEndDotScale: integer;
    OpUnprintedEndFontScale: integer;
    OpUnprintedTabPointerScale: integer;

    //wrap
    OpWrapMode: integer;
    OpWrapIndented: boolean;

    //undo
    OpUndoLimit: integer;
    OpUndoGrouped: boolean;
    OpUndoAfterSave: boolean;

    //caret
    OpCaretBlinkTime: integer;
    OpCaretBlinkEn: boolean;
    OpCaretShapeNorm: integer;
    OpCaretShapeOvr: integer;
    OpCaretShapeRO: integer;
    OpCaretVirtual: boolean;
    OpCaretMulti: boolean;
    OpCaretAfterPasteColumn: integer;

    //general
    OpShowCurLine: boolean;
    OpShowCurLineMinimal: boolean;
    OpShowCurLineOnlyFocused: boolean;
    OpShowCurCol: boolean;
    OpShowLastLineOnTop: boolean;
    OpShowSelectBgFull: boolean;
    OpShowSyntaxBgFull: boolean;
    OpCopyLineIfNoSel: boolean;
    OpCutLineIfNoSel: boolean;
    OpSavingTrimSpaces: boolean;
    OpSavingForceFinalEol: boolean;
    OpShowHintOnVertScroll: boolean;
    OpDynHilite: boolean;

    OpWordChars: UnicodeString;
    OpHexChars: UnicodeString;
    OpFoldStyle: integer;

    //indent
    OpIndentAuto: boolean;
    OpIndentAutoKind: integer;
    OpIndentSize: integer;
    OpUnIndentKeepsAlign: boolean;

    //mouse
    OpMouse2ClickDragSelectsWords: boolean;
    OpMouseDragDrop: boolean;
    OpMouseDragDropFocusTarget: boolean;
    OpMouseNiceScroll: boolean;
    OpMouseRightClickMovesCaret: boolean;
    OpMouseEnableColumnSelection: boolean;
    OpMouseHideCursorOnType: boolean; //don't work on lin
    OpMouseGutterClickSelectedLine: boolean;
    OpMouseWheelZoom: boolean;

    //keys
    OpKeyBackspaceUnindent: boolean;
    OpKeyTabIndents: boolean;
    OpKeyHomeToNonSpace: boolean;
    OpKeyHomeEndNavigateWrapped: boolean;
    OpKeyEndToNonSpace: boolean;
    OpKeyPageKeepsRelativePos: boolean;
    OpKeyPageUpDownSize: integer;
    OpKeyUpDownKeepColumn: boolean;
    OpKeyUpDownNavigateWrapped: boolean;
    OpKeyLeftRightSwapSel: boolean;
    OpKeyLeftRightSwapSelAndSelect: boolean;
  end;
var
  EditorOps: TEditorOps;

function GetAppPath(id: TAppPathId): string;
function GetAppLangFilename: string;

function GetAppLexerFilename(const ALexName: string): string;
function GetAppLexerMapFilename(const ALexName: string): string;
function GetAppLexerOverrideFilename(AName: string): string;
function GetAppLexerPropInCommentsSection(const ALexerName, AKey: string): string;

function GetActiveControl(Form: TWinControl): TWinControl;
function GetListboxItemHeight(const AFontName: string; AFontSize: integer): integer;
function GetAppCommandCodeFromCommandStringId(const AId: string): integer;
function MsgBox(const Str: string; Flags: Longint): integer;

function GetAppKeymapOverrideFilename(AName: string): string;
function GetAppKeymapHotkey(const ACmdString: string): string;
function SetAppKeymapHotkey(AParams: string): boolean;
function AppKeymapHasDuplicates: boolean;
function AppKeymapHasDuplicateForKey(AHotkey, AKeyComboSeparator: string): boolean;
procedure AppKeymap_ApplyUndoList(AUndoList: TATKeymapUndoList);

procedure DoOps_SaveKeyItem(K: TATKeymapItem; const path, ALexerName: string);
procedure DoOps_SaveKey_ForPluginModuleAndMethod(AOverwriteKey: boolean;
  const AMenuitemCaption, AModuleName, AMethodName, ALexerName, AHotkey: string);

function DoLexerFindByFilename(const fn: string): TecSyntAnalyzer;
procedure DoLexerEnum(L: TStringList; AlsoDisabled: boolean = false);
procedure DoLexerExportFromLibToFile(an: TecSyntAnalyzer);

function CommandPlugins_GetIndexFromModuleAndMethod(AStr: string): integer;
procedure CommandPlugins_UpdateSubcommands(AStr: string);
procedure CommandPlugins_DeleteItem(AIndex: integer);

var
  AppManager: TecSyntaxManager = nil;
  AppKeymap: TATKeymap = nil;
  AppKeymapInitial: TATKeymap = nil;
  AppShortcutEscape: TShortcut = 0;
  AppLangName: string = '';

type
  TStrEvent = procedure(Sender: TObject; const ARes: string) of object;
  TStrFunction = function(const AStr: string): boolean of object;

const
  cEncNameUtf8_WithBom = 'UTF-8 with BOM';
  cEncNameUtf8_NoBom = 'UTF-8';
  cEncNameUtf16LE_WithBom = 'UTF-16 LE with BOM';
  cEncNameUtf16LE_NoBom = 'UTF-16 LE';
  cEncNameUtf16BE_WithBom = 'UTF-16 BE with BOM';
  cEncNameUtf16BE_NoBom = 'UTF-16 BE';
  cEncNameAnsi = 'ANSI';

  cEncNameCP1250 = 'CP1250';
  cEncNameCP1251 = 'CP1251';
  cEncNameCP1252 = 'CP1252';
  cEncNameCP1253 = 'CP1253';
  cEncNameCP1254 = 'CP1254';
  cEncNameCP1255 = 'CP1255';
  cEncNameCP1256 = 'CP1256';
  cEncNameCP1257 = 'CP1257';
  cEncNameCP1258 = 'CP1258';
  cEncNameCP437 = 'CP437';
  cEncNameCP850 = 'CP850';
  cEncNameCP852 = 'CP852';
  cEncNameCP866 = 'CP866';
  cEncNameCP874 = 'CP874';
  cEncNameISO1 = 'ISO-8859-1';
  cEncNameISO2 = 'ISO-8859-2';
  cEncNameMac = 'Macintosh';
  cEncNameCP932 = 'CP932';
  cEncNameCP936 = 'CP936';
  cEncNameCP949 = 'CP949';
  cEncNameCP950 = 'CP950';

type
  TAppPyEvent = (
    cEventOnOpen,
    cEventOnOpenBefore,
    cEventOnClose,
    cEventOnSaveAfter,
    cEventOnSaveBefore,
    cEventOnKey,
    cEventOnKeyUp,
    cEventOnChange,
    cEventOnChangeSlow,
    cEventOnCaret,
    cEventOnClick,
    cEventOnClickDbl,
    cEventOnState,
    cEventOnFocus,
    cEventOnStart,
    cEventOnLexer,
    cEventOnComplete,
    cEventOnGotoDef,
    cEventOnFuncHint,
    cEventOnTabMove,
    cEventOnPanel,
    cEventOnConsole,
    cEventOnConsoleNav,
    cEventOnOutputNav,
    cEventOnSnippet,
    cEventOnMacro
    );
  TAppPyEvents = set of TAppPyEvent;
  TAppPyEventsPrior = array[TAppPyEvent] of byte; //0: default, 1,2...: higher priority

const
  cAppPyEvent: array[TAppPyEvent] of string = (
    'on_open',
    'on_open_pre',
    'on_close',
    'on_save',
    'on_save_pre',
    'on_key',
    'on_key_up',
    'on_change',
    'on_change_slow',
    'on_caret',
    'on_click',
    'on_click_dbl',
    'on_state',
    'on_focus',
    'on_start',
    'on_lexer',
    'on_complete',
    'on_goto_def',
    'on_func_hint',
    'on_tab_move',
    'on_panel',
    'on_console',
    'on_console_nav',
    'on_output_nav',
    'on_snippet',
    'on_macro'
    );

const
  cMaxItemsInInstallInf = 400;

type
  TAppPluginCmd = record
    ItemModule: string;
    ItemProc: string;
    ItemProcParam: string;
    ItemCaption: string;
    ItemLexers: string;
    ItemInMenu: boolean;
    ItemFromApi: boolean;
  end;
  TAppPluginCmdArray = array[0..400] of TAppPluginCmd;

type
  TAppPluginEvent = record
    ItemModule: string;
    ItemLexers: string;
    ItemEvents: TAppPyEvents;
    ItemEventsPrior: TAppPyEventsPrior;
    ItemKeys: string;
  end;
  TAppPluginEventArray = array[0..100] of TAppPluginEvent;

var
  FPluginsCmd: TAppPluginCmdArray;
  FPluginsEvents: TAppPluginEventArray;

type
  TTreeView = type TTreeViewMy;
  TATListbox = type TATListboxMy;

type
  TAppSidePanel = record
    ItemCaption: string;
    ItemTreeview: TTreeView;
    ItemListbox: TATListbox;
    ItemListboxStrings: TStringList;
    ItemImagelist: TImageList;
    ItemMenu: TPopupMenu;
  end;

var
  FAppSidePanels: array[0..20] of TAppSidePanel;


implementation

function MsgBox(const Str: string; Flags: Longint): integer;
begin
  Result:= Application.MessageBox(PChar(Str), PChar(msgTitle), Flags);
end;

function InitPyLibraryPath: string;
begin
  {$ifdef windows}
    Result:= 'python35.dll';
  {$endif}
  {$ifdef linux}
    Result:= 'libpython3.4m.so.1.0';
  {$endif}
  {$ifdef darwin}
    Result:= '/Library/Frameworks/Python.framework/Versions/3.4/lib/libpython3.4.dylib';
    if FileExists(Result) then exit;
    Result:= '/Library/Frameworks/Python.framework/Versions/3.5/lib/libpython3.5.dylib';
    if FileExists(Result) then exit;
    Result:= '/Library/Frameworks/Python.framework/Versions/3.6/lib/libpython3.6.dylib';
    if FileExists(Result) then exit;
    Result:= '/Library/Frameworks/Python.framework/Versions/3.7/lib/libpython3.7.dylib';
    if FileExists(Result) then exit;
    Result:= '/Library/Frameworks/Python.framework/Versions/3.8/lib/libpython3.8.dylib';
    if FileExists(Result) then exit;
  {$endif} ;
end;

var
  OpDirExe: string = '';
  OpDirLocal: string = '';
  OpDirPrecopy: string = '';

function GetDirPrecopy: string;
begin
  Result:=
  {$ifdef windows} '' {$endif}
  {$ifdef linux} '/usr/share/cudatext' {$endif}
  {$ifdef darwin} ExtractFileDir(OpDirExe)+'/Resources' {$endif}
end;

function GetAppPath(id: TAppPathId): string;
begin
  case id of
    cDirSettings:
      begin
        Result:= OpDirLocal+DirectorySeparator+'settings';
        CreateDirUTF8(Result);
      end;
    cDirSettingsDef:
      begin
        Result:= OpDirLocal+DirectorySeparator+'settings_default';
      end;
    cDirData:
      begin
        Result:= OpDirLocal+DirectorySeparator+'data';
      end;
    cDirDataLexlib:
      begin
        Result:= OpDirLocal+DirectorySeparator+'data'+DirectorySeparator+'lexlib';
      end;
    cDirDataNewdoc:
      begin
        Result:= OpDirLocal+DirectorySeparator+'data'+DirectorySeparator+'newdoc';
      end;
    cDirDataThemes:
      begin
        Result:= OpDirLocal+DirectorySeparator+'data'+DirectorySeparator+'themes';
      end;
    cDirDataAcp:
      begin
        Result:= OpDirLocal+DirectorySeparator+'data'+DirectorySeparator+'autocomplete';
      end;
    cDirDataAcpSpec:
      begin
        Result:= OpDirLocal+DirectorySeparator+'data'+DirectorySeparator+'autocompletespec';
      end;
    cDirDataLangs:
      begin
        Result:= OpDirLocal+DirectorySeparator+'data'+DirectorySeparator+'lang';
      end;
    cDirReadme:
      begin
        Result:= OpDirLocal+DirectorySeparator+'readme';
      end;
    cDirLastInstalledAddon:
      begin
        Result:= AppFolderOfLastInstalledAddon;
      end;
    cDirPy:
      begin
        Result:= OpDirLocal+DirectorySeparator+'py';
      end;

    cFileOptDefault:
      begin
        Result:= GetAppPath(cDirSettingsDef)+DirectorySeparator+'default.json';
      end;
    cFileOptHistory:
      begin
        Result:= GetAppPath(cDirSettings)+DirectorySeparator+'history.json';
      end;
    cFileOptUser:
      begin
        Result:= GetAppPath(cDirSettings)+DirectorySeparator+'user.json';
      end;
    cFileOptFiletypes:
      begin
        Result:= GetAppPath(cDirSettings)+DirectorySeparator+'filetypes.json';
      end;
    cFileOptKeymap:
      begin
        Result:= GetAppPath(cDirSettings)+DirectorySeparator+'keys.json';
      end;
    cFileHistoryList:
      begin
        Result:= GetAppPath(cDirSettings)+DirectorySeparator+'history files.json';
      end;
    cFileLexerStyles:
      begin
        Result:= GetAppPath(cDirSettings)+DirectorySeparator+'lexer styles backup.ini';
      end;

    cFileReadmeHist:
      begin
        Result:= GetAppPath(cDirReadme)+DirectorySeparator+'history.txt';
      end;
    cFileReadmeMouse:
      begin
        Result:= GetAppPath(cDirReadme)+DirectorySeparator+'help mouse.txt';
      end;
    cFileReadmeLexerInst:
      begin
        Result:= GetAppPath(cDirReadme)+DirectorySeparator+'help lexers install.txt';
      end;
  end;
end;

procedure InitDirs;
var
  S: string;
begin
  OpDirExe:= ExtractFileDir(ParamStrUTF8(0));
  OpDirPrecopy:= GetDirPrecopy;

  if DirectoryExistsUTF8(
      OpDirExe+
      DirectorySeparator+'data'+
      DirectorySeparator+'lexlib') then
    OpDirLocal:= OpDirExe
  else
  begin
    {$ifdef windows}
    OpDirLocal:= GetEnvironmentVariableUTF8('appdata')+'\CudaText';
    {$else}
    OpDirLocal:= GetEnvironmentVariableUTF8('HOME')+'/.cudatext';
    {$endif}
    CreateDirUTF8(OpDirLocal);

    if DirectoryExistsUTF8(OpDirPrecopy) then
    begin
      {$ifdef linux}
      RunCommand(Format('cp -R -u -t %s /usr/share/cudatext/py /usr/share/cudatext/data /usr/share/cudatext/readme /usr/share/cudatext/settings_default', [OpDirLocal]), S);
      {$endif}
      {$ifdef darwin}
      //see rsync help. need options:
      // -u (update)
      // -r (recursive)
      // -t (preserve times)
      RunCommand(Format('rsync -urt "%s/" "%s"', [OpDirPrecopy, OpDirLocal]), S);
      {$endif}
    end;
  end;
end;

procedure InitEditorOps(var Op: TEditorOps);
begin
  with Op do
  begin
    OpFontName:=
      {$ifdef windows} 'Consolas' {$endif}
      {$ifdef linux} 'Courier New' {$endif}
      {$ifdef darwin} 'Monaco' {$endif} ;
    OpFontSize:=
      {$ifdef windows} 10 {$endif}
      {$ifdef linux} 11 {$endif}
      {$ifdef darwin} 12 {$endif} ;
    OpFontQuality:= fqDefault;

    OpSpacingX:= 0;
    OpSpacingY:= 1;

    OpTabSize:= 8;
    OpTabSpaces:= false;

    OpOvrSel:= true;
    OpOvrOnPaste:= false;

    OpUnderlineColorFiles:= '*';
    OpUnderlineColorSize:= 3;
    OpLinks:= true;
    OpLinksRegex:= ATSynEdit.cUrlRegexInitial;

    OpGutterShow:= true;
    OpGutterFold:= true;
    OpGutterFoldAlways:= true;
    OpGutterBookmk:= true;

    OpNumbersShow:= true;
    OpNumbersFontSize:= 0;
    OpNumbersStyle:= Ord(cNumbersAll);
    OpNumbersForCarets:= false;
    OpNumbersCenter:= true;

    OpRulerShow:= false;
    OpRulerFontSize:= 8;
    OpRulerSize:= 20;
    OpRulerTextIndent:= 0;

    OpMinimapShow:= false;
    OpMinimapShowSelAlways:= false;
    OpMinimapShowSelBorder:= false;
    OpMinimapCharWidth:= 0;
    OpMinimapAtLeft:= false;

    OpMicromapShow:= false;
    OpMicromapWidth:= 12;

    OpMargin:= cInitMarginRight;
    OpMarginString:= '';
    OpStaplesShow:= true;

    OpUnprintedShow:= false;
    OpUnprintedSpaces:= true;
    OpUnprintedEnds:= true;
    OpUnprintedEndDetails:= false;
    OpUnprintedReplaceSpec:= false;
    OpUnprintedReplaceToCode:= 'A4';

    OpUnprintedEndArrow:= true;
    OpUnprintedTabArrowLen:= 1;
    OpUnprintedSpaceDotScale:= 15;
    OpUnprintedEndDotScale:= 30;
    OpUnprintedEndFontScale:= 80;
    OpUnprintedTabPointerScale:= 22;

    OpWrapMode:= 0;
    OpWrapIndented:= true;

    OpUndoLimit:= 5000;
    OpUndoGrouped:= true;
    OpUndoAfterSave:= false;

    OpCaretBlinkTime:= cInitTimerBlink;
    OpCaretBlinkEn:= true;
    OpCaretShapeNorm:= Ord(cInitCaretShapeIns);
    OpCaretShapeOvr:= Ord(cInitCaretShapeOvr);
    OpCaretShapeRO:= Ord(cInitCaretShapeRO);
    OpCaretVirtual:= false;
    OpCaretMulti:= true;
    OpCaretAfterPasteColumn:= Ord(cPasteCaretColumnRight);

    OpShowCurLine:= false;
    OpShowCurLineMinimal:= true;
    OpShowCurLineOnlyFocused:= false;
    OpShowCurCol:= false;
    OpShowLastLineOnTop:= false;
    OpShowSelectBgFull:= false;
    OpShowSyntaxBgFull:= true;
    OpCopyLineIfNoSel:= true;
    OpCutLineIfNoSel:= false;
    OpSavingTrimSpaces:= false;
    OpSavingForceFinalEol:= false;
    OpShowHintOnVertScroll:= false;
    OpDynHilite:= true;

    OpWordChars:= '';
    OpHexChars:= '';
    OpFoldStyle:= 1;

    OpIndentAuto:= true;
    OpIndentAutoKind:= Ord(cIndentAsIs);
    OpIndentSize:= 2;
    OpUnIndentKeepsAlign:= true;

    OpMouse2ClickDragSelectsWords:= true;
    OpMouseDragDrop:= true;
    OpMouseDragDropFocusTarget:= true;
    OpMouseNiceScroll:= true;
    OpMouseRightClickMovesCaret:= false;
    OpMouseEnableColumnSelection:= true;
    OpMouseHideCursorOnType:= false;
    OpMouseGutterClickSelectedLine:= true;
    OpMouseWheelZoom:= false;

    OpKeyBackspaceUnindent:= true;
    OpKeyTabIndents:= true;
    OpKeyHomeToNonSpace:= true;
    OpKeyHomeEndNavigateWrapped:= true;
    OpKeyEndToNonSpace:= true;
    OpKeyPageKeepsRelativePos:= true;
    OpKeyPageUpDownSize:= Ord(cPageSizeFullMinus1);
    OpKeyUpDownKeepColumn:= true;
    OpKeyUpDownNavigateWrapped:= true;
    OpKeyLeftRightSwapSel:= true;
    OpKeyLeftRightSwapSelAndSelect:= false;
  end;
end;


function IsDoubleBufferedNeeded: boolean;
begin
  Result:= false;

  {$ifdef windows}
  exit(true);
  {$endif}

  {$ifdef linux}
  exit(true);
  {$endif}

  {$ifdef darwin}
  exit(false);
  {$endif}
end;


procedure InitUiOps(var Op: TUiOps);
begin
  with Op do
  begin
    VarFontName:= 'default';
    VarFontSize:=
      {$ifdef windows} 9 {$endif}
      {$ifdef linux} 10 {$endif}
      {$ifdef darwin} 12 {$endif} ;

    OutputFontName:= VarFontName;
    OutputFontSize:= VarFontSize;

    DoubleBuffered:= IsDoubleBufferedNeeded;

    LexerThemes:= true;
    PyLibrary:= InitPyLibraryPath;
    PictureTypes:= 'bmp,png,jpg,jpeg,gif,ico';
    MaxFileSizeToOpen:= 30;
    MaxFileSizeForLexer:= 4;

    AutocompleteCss:= true;
    AutocompleteHtml:= true;
    AutocompleteAutoshowChars:= 0;
    AutocompleteAutoshowLexers:= '';
    AutoCloseBrackets:= '([{';

    ListboxSizeX:= 450;
    ListboxSizeY:= 300;
    ListboxCompleteSizeX:= 550;
    ListboxCompleteSizeY:= 200;
    ListboxFuzzySearch:= true;

    TabWidth:= 170;
    TabHeight:= 25;
    TabHeightInner:= TabHeight-1;
    TabIndentTop:= 0;
    TabIndentInit:= 5;
    TabAngle:= 3;
    TabBottom:= false;
    TabColorFull:= false;
    TabShowX:= 1; //show all
    TabShowPlus:= true;
    TabDblClickClose:= false;
    TabNumbers:= false;

    MaxHistoryEdits:= 20;
    MaxHistoryMenu:= 10;
    MaxHistoryFiles:= 25;

    FindSuggestSel:= false;
    FindSuggestWord:= true;
    FindSelCase:= 2;
    FindShowFindfirst:= true;
    FindIndentVert:= -5;
    FindIndentHorz:= 10;
    FindMultiLineScale:= 2.5;
    FindSeparateForm:= false;

    EscapeClose:= false;
    EscapeCloseConsole:= true;
    InitialDir:= '';

    ExportHtmlNumbers:= false;
    ExportHtmlFontSize:= 12;
    ExportHtmlFontName:= 'Courier New';

    TreeAutoSync:= true;
    TreeTimeFill:= 2000;
    TreeTimeFocus:= 300;
    TreeShowLines:= true;
    TreeShowIcons:= true;
    PyChangeSlow:= 2000;

    NewdocLexer:= '';
    NewdocEnc:= cEncNameUtf8_NoBom;
    NewdocEnds:= {$ifdef windows} Ord(cEndWin) {$else} Ord(cEndUnix) {$endif};

    DefaultEncUtf8:= {$ifdef windows} false {$else} true {$endif};

    StatusNoSel:= 'Ln {y}, Col {x}';
    StatusSmallSel:= 'Ln {y}, Col {x}, sel';
    StatusStreamSel:= 'Ln {y}, Col {x}, {sel} lines sel';
    StatusColSel:= '{sel}x{cols} column';
    StatusCarets:= '{carets} carets, {sel} lines sel';
    StatusPanels:= 'caret,C,170|enc,C,115|ends,C,50|lexer,C,140|tabsize,C,80|msg,L,4000';
    StatusHeight:= TabHeight;
    StatusTime:= 5;
    StatusAltTime:= 7;
    StatusTabsize:= 'Tab size {tab}{_}';

    ShowTitlePath:= false;
    ShowLastFiles:= true;
    OneInstance:= false;
    NotifEnabled:= true;
    NotifTimeSec:= 2;
    NonTextFiles:= 0;
    NonTextFilesBufferKb:= 64;
    LexerMenuGrouped:= true;
    ReloadFollowTail:= true;
    FullScreenHide:= 't';
    BackupMode:= '';
  end;
end;


procedure SReplaceSpecialFilenameChars(var S: string);
begin
  S:= StringReplace(S, '/', '_', [rfReplaceAll]);
  S:= StringReplace(S, '\', '_', [rfReplaceAll]);
  S:= StringReplace(S, '*', '_', [rfReplaceAll]);
  S:= StringReplace(S, ':', '_', [rfReplaceAll]);
  S:= StringReplace(S, '<', '_', [rfReplaceAll]);
  S:= StringReplace(S, '>', '_', [rfReplaceAll]);
end;

function GetAppLexerOverrideFilename(AName: string): string;
begin
  SReplaceSpecialFilenameChars(AName);
  Result:= GetAppPath(cDirSettings)+DirectorySeparator+'lexer '+AName+'.json';
end;

function GetAppKeymapOverrideFilename(AName: string): string;
begin
  SReplaceSpecialFilenameChars(AName);
  Result:= GetAppPath(cDirSettings)+DirectorySeparator+'keys lexer '+AName+'.json';
end;


function GetAppCommandCodeFromCommandStringId(const AId: string): integer;
begin
  //plugin item 'module,method'
  if Pos(',', AId)>0 then
  begin
    Result:= CommandPlugins_GetIndexFromModuleAndMethod(AId);
    if Result>=0 then
      Inc(Result, cmdFirstPluginCommand);
  end
  else
    //usual item
    Result:= StrToIntDef(AId, -1);
end;


function DoLexerFindByFilename(const fn: string): TecSyntAnalyzer;
var
  c: TJsonConfig;
  fn_opt, s, ext: string;
begin
  fn_opt:= GetAppPath(cFileOptFiletypes);
  if FileExistsUTF8(fn_opt) then
  begin
    c:= TJsonConfig.Create(nil);
    try
      c.FileName:= fn_opt;

      //by filename
      s:= c.GetValue(ExtractFileName(fn), '');
      if s<>'' then
      begin
        Result:= AppManager.FindAnalyzer(s);
        Exit
      end;

      //by extention
      ext:= ExtractFileExt(fn);
      if ext<>'' then
      begin
        s:= c.GetValue('*'+ext, '');
        if s<>'' then
        begin
          Result:= AppManager.FindAnalyzer(s);
          Exit
        end;
      end;
    finally
      c.Free;
    end;
  end;

  Result:= DoFindLexerForFilename(AppManager, fn);
end;


procedure DoOps_SaveKeyItem(K: TATKeymapItem; const path, ALexerName: string);
var
  c: TJSONConfig;
  sl: TStringList;
  i: integer;
begin
  c:= TJSONConfig.Create(nil);
  sl:= TStringlist.create;
  try
    c.Formatted:= true;

    if ALexerName<>'' then
      c.Filename:= GetAppKeymapOverrideFilename(ALexerName)
    else
      c.Filename:= GetAppPath(cFileOptKeymap);

    c.SetValue(path+'/name', K.Name);

    sl.clear;
    for i:= 0 to High(TATKeyArray) do
      if K.Keys1[i]<>0 then
        sl.Add(ShortCutToText(K.Keys1[i]));
    c.SetValue(path+'/s1', sl);

    sl.clear;
    for i:= 0 to High(TATKeyArray) do
      if K.Keys2[i]<>0 then
        sl.Add(ShortCutToText(K.Keys2[i]));
    c.SetValue(path+'/s2', sl);
  finally
    c.Free;
    sl.Free;
  end;
end;


procedure DoOps_SaveKey_ForPluginModuleAndMethod(AOverwriteKey: boolean;
  const AMenuitemCaption, AModuleName, AMethodName, ALexerName, AHotkey: string);
const
  cKeyComboSeparator = '|';
var
  c: TJSONConfig;
  sl: TStringList;
  path, s_items, s_item: string;
begin
  //check-1: is key registered for any other command?
  if not AOverwriteKey then
    if AppKeymapHasDuplicateForKey(AHotkey, cKeyComboSeparator) then exit;

  c:= TJSONConfig.Create(nil);
  sl:= TStringlist.create;
  try
    c.Formatted:= true;

    if ALexerName<>'' then
      c.Filename:= GetAppKeymapOverrideFilename(ALexerName)
    else
      c.Filename:= GetAppPath(cFileOptKeymap);

    path:= AModuleName+','+AMethodName;

    //check-2: this command has already any key?
    if not AOverwriteKey then
      if c.GetValue(path+'/s1', sl, '') then exit;

    c.SetValue(path+'/name', Utf8Decode(AMenuitemCaption));

    sl.Clear;
    s_items:= AHotkey;
    repeat
      s_item:= SGetItem(s_items, cKeyComboSeparator);
      if s_item='' then Break;
      sl.Add(s_item);
    until false;
    c.SetValue(path+'/s1', sl);
  finally
    c.Free;
    sl.Free;
  end;
end;


function GetActiveControl(Form: TWinControl): TWinControl;
var
  Ctl: TControl;
  i, j: integer;
begin
  Result:= nil;
  for i:= 0 to Form.ControlCount-1 do
  begin
    Ctl:= Form.Controls[i];
    if (Ctl is TWinControl) then
      if (Ctl as TWinControl).Focused then
        begin Result:= Ctl as TWinControl; exit end;
    if Ctl is TPanel then
    begin
      Result:= GetActiveControl(Ctl as TPanel);
      if Result<>nil then exit;
    end;
  end;
end;

function GetListboxItemHeight(const AFontName: string; AFontSize: integer): integer;
var
  bmp: TBitmap;
begin
  bmp:= TBitmap.Create;
  try
    bmp.Canvas.Font.Name:= AFontName;
    bmp.Canvas.Font.Size:= AFontSize;
    Result:= bmp.Canvas.TextHeight('Pyj')+3;
  finally
    FreeAndNil(bmp);
  end;
end;


procedure DoLexerEnum(L: TStringList; AlsoDisabled: boolean = false);
var
  i: Integer;
begin
  with AppManager do
    for i:= 0 to AnalyzerCount-1 do
      if AlsoDisabled or not Analyzers[i].Internal then
        L.Add(Analyzers[i].LexerName);
end;

procedure DoLexerExportFromLibToFile(an: TecSyntAnalyzer);
begin
  if Assigned(an) then
    an.SaveToFile(GetAppLexerFilename(an.LexerName));
end;


procedure CommandPlugins_DeleteItem(AIndex: integer);
var
  i: integer;
begin
  if (AIndex>=Low(FPluginsCmd)) and (AIndex<=High(FPluginsCmd)) then
  begin
    for i:= AIndex to High(FPluginsCmd)-1 do
    begin
      FPluginsCmd[i].ItemModule:= FPluginsCmd[i+1].ItemModule;
      FPluginsCmd[i].ItemProc:= FPluginsCmd[i+1].ItemProc;
      FPluginsCmd[i].ItemProcParam:= FPluginsCmd[i+1].ItemProcParam;
      FPluginsCmd[i].ItemCaption:= FPluginsCmd[i+1].ItemCaption;
      FPluginsCmd[i].ItemLexers:= FPluginsCmd[i+1].ItemLexers;
      FPluginsCmd[i].ItemInMenu:= FPluginsCmd[i+1].ItemInMenu;
      FPluginsCmd[i].ItemFromApi:= FPluginsCmd[i+1].ItemFromApi;
    end;
  end;
  with FPluginsCmd[High(FPluginsCmd)] do
  begin
    ItemModule:= '';
    ItemProc:= '';
    ItemProcParam:= '';
    ItemFromApi:= false;
  end;
end;

function CommandPlugins_GetIndexFromModuleAndMethod(AStr: string): integer;
var
  i: integer;
  SModule, SProc, SProcParam: string;
begin
  Result:= -1;

  SModule:= SGetItem(AStr);
  SProc:= SGetItem(AStr);
  SProcParam:= SGetItem(AStr);

  if SModule='' then exit;
  if SProc='' then exit;

  for i:= Low(FPluginsCmd) to High(FPluginsCmd) do
    with FPluginsCmd[i] do
    begin
      if ItemModule='' then Break;
      if (ItemModule=SModule) and (ItemProc=SProc) and (ItemProcParam=SProcParam) then exit(i);
    end;
end;


procedure CommandPlugins_UpdateSubcommands(AStr: string);
const
  cSepRoot=';';
  cSepParams=#10;
  cSepNameParam=#9;
var
  SModule, SProc, SParams, SItem, SItemParam, SItemCaption: string;
  N: integer;
begin
  SModule:= SGetItem(AStr, cSepRoot);
  SProc:= SGetItem(AStr, cSepRoot);
  SParams:= AStr;

  //del items for module/method
  for N:= High(FPluginsCmd) downto Low(FPluginsCmd) do
    with FPluginsCmd[N] do
      if (ItemModule=SModule) and (ItemProc=SProc) and (ItemProcParam<>'') then
        CommandPlugins_DeleteItem(N);

  //find index of first free item
  N:= Low(FPluginsCmd);
  repeat
    if FPluginsCmd[N].ItemModule='' then break;
    Inc(N);
    if N>High(FPluginsCmd) then exit;
  until false;

  //add items for SParams
  repeat
    SItem:= SGetItem(SParams, cSepParams);
    if SItem='' then break;

    SItemCaption:= SGetItem(SItem, cSepNameParam);
    SItemParam:= SItem;

    with FPluginsCmd[N] do
    begin
      ItemModule:= SModule;
      ItemProc:= SProc;
      ItemProcParam:= SItemParam;
      ItemCaption:= SItemCaption;
      ItemFromApi:= true;
    end;
    Inc(N);
    if N>High(FPluginsCmd) then exit;
  until false;
end;


function GetAppLangFilename: string;
begin
  if AppLangName='' then
    Result:= ''
  else
    Result:= GetAppPath(cDirDataLangs)+DirectorySeparator+AppLangName+'.ini';
end;

function GetLexerFilenameWithExt(ALexName, AExt: string): string;
begin
  if ALexName<>'' then
  begin
    ALexName:= StringReplace(ALexName, ':', '_', [rfReplaceAll]);
    ALexName:= StringReplace(ALexName, '/', '_', [rfReplaceAll]);
    ALexName:= StringReplace(ALexName, '\', '_', [rfReplaceAll]);
    ALexName:= StringReplace(ALexName, '*', '_', [rfReplaceAll]);
    Result:= GetAppPath(cDirDataLexlib)+DirectorySeparator+ALexName+AExt;
  end
  else
    Result:= '';
end;

function GetAppLexerMapFilename(const ALexName: string): string;
begin
  Result:= GetLexerFilenameWithExt(ALexName, '.cuda-lexmap');
end;

function GetAppLexerFilename(const ALexName: string): string;
begin
  Result:= GetLexerFilenameWithExt(ALexName, '.lcf');
end;


function GetAppKeymapHotkey(const ACmdString: string): string;
var
  NCode, NIndex: integer;
begin
  Result:= '';
  if Pos(',', ACmdString)=0 then
    NCode:= StrToIntDef(ACmdString, 0)
  else
  begin
    NIndex:= CommandPlugins_GetIndexFromModuleAndMethod(ACmdString);
    if NIndex<0 then exit;
    NCode:= NIndex+cmdFirstPluginCommand;
  end;

  NIndex:= AppKeymap.IndexOf(NCode);
  if NIndex<0 then exit;
  with AppKeymap[NIndex] do
    Result:= KeyArrayToString(Keys1)+'|'+KeyArrayToString(Keys2);
end;


function SetAppKeymapHotkey(AParams: string): boolean;
var
  NCode, NIndex: integer;
  SCmd, SKey1, SKey2: string;
begin
  Result:= false;
  SCmd:= SGetItem(AParams, '|');
  SKey1:= SGetItem(AParams, '|');
  SKey2:= SGetItem(AParams, '|');

  if Pos(',', SCmd)=0 then
    NCode:= StrToIntDef(SCmd, 0)
  else
  begin
    NIndex:= CommandPlugins_GetIndexFromModuleAndMethod(SCmd);
    if NIndex<0 then exit;
    NCode:= NIndex+cmdFirstPluginCommand;
  end;

  NIndex:= AppKeymap.IndexOf(NCode);
  if NIndex<0 then exit;
  with AppKeymap[NIndex] do
  begin
    KeyArraySetFromString(Keys1, SKey1);
    KeyArraySetFromString(Keys2, SKey2);

    //save to keys.json
    DoOps_SaveKeyItem(AppKeymap[NIndex], SCmd,
      ''); //Py API: no need lexer override
  end;
  Result:= true;
end;


function AppKeymapHasDuplicates: boolean;
var
  i, j: integer;
  item1, item2: TATKeymapItem;
begin
  Result:= false;
  for i:= 0 to AppKeymap.Count-1 do
    for j:= i+1 to AppKeymap.Count-1 do
    begin
      item1:= AppKeymap.Items[i];
      item2:= AppKeymap.Items[j];
      if KeyArraysEqualNotEmpty(item1.Keys1, item2.Keys1) or
         KeyArraysEqualNotEmpty(item1.Keys2, item2.Keys2) or
         KeyArraysEqualNotEmpty(item1.Keys1, item2.Keys2) or
         KeyArraysEqualNotEmpty(item1.Keys2, item2.Keys1) then
        begin
          MsgBox(msgStatusCommandsHaveSameHotkeys+#13+
            item1.Name+#13+
            item2.Name+#13+
            #13+msgStatusCorrectOneOfTheseHotkeys,
            MB_OK or MB_ICONWARNING);
          Result:= true;
          Exit
        end;
    end;
end;

function AppKeymapHasDuplicateForKey(AHotkey, AKeyComboSeparator: string): boolean;
var
  item: TATKeymapItem;
  i: integer;
begin
  Result:= false;
  if AHotkey='' then exit;

  //KeyArrayToString has separator ' * '
  AHotkey:= StringReplace(AHotkey, AKeyComboSeparator, ' * ', [rfReplaceAll]);

  for i:= 0 to AppKeymap.Count-1 do
  begin
    item:= AppKeymap.Items[i];
    if (KeyArrayToString(item.Keys1)=AHotkey) or
       (KeyArrayToString(item.Keys1)=AHotkey) then exit(true);
  end;
end;


procedure AppKeymap_ApplyUndoList(AUndoList: TATKeymapUndoList);
var
  UndoItem: TATKeymapUndoItem;
  i, ncmd, nitem: integer;
begin
  for i:= 0 to AUndoList.Count-1 do
  begin
    UndoItem:= AUndoList[i];

    ncmd:= GetAppCommandCodeFromCommandStringId(UndoItem.StrId);
    if ncmd<0 then Continue;

    nitem:= AppKeymap.IndexOf(ncmd);
    if nitem<0 then Continue;

    AppKeymap.Items[nitem].Keys1:= UndoItem.KeyArray1;
    AppKeymap.Items[nitem].Keys2:= UndoItem.KeyArray2;
  end;
end;

function GetAppLexerPropInCommentsSection(const ALexerName, AKey: string): string;
begin
  with TIniFile.Create(GetAppLexerMapFilename(ALexerName)) do
  try
    Result:= Trim(ReadString('comments', AKey, ''));
  finally
    Free
  end;
end;


initialization
  InitDirs;
  InitEditorOps(EditorOps);
  InitUiOps(UiOps);

  AppKeymap:= TATKeymap.Create;
  InitKeymapFull(AppKeymap);
  InitKeymapForApplication(AppKeymap);

  FillChar(AppBookmarkSetup, SizeOf(AppBookmarkSetup), 0);
  AppBookmarkImagelist:= TImageList.Create(nil);

  FillChar(FAppSidePanels, SizeOf(FAppSidePanels), 0);

  AppShortcutEscape:= ShortCut(vk_escape, []);
  Mouse.DragImmediate:= false;

finalization
  FreeAndNil(AppKeymap);
  FreeAndNil(AppBookmarkImagelist);

end.

