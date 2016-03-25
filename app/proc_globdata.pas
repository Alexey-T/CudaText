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
  jsonConf,
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
  ecSyntAnal;

var
  AppBookmarkSetup: array[1..255] of
    record ImageIndex: integer; Color: TColor; end;
  AppBookmarkImagelist: TImageList = nil;

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

    PyLibrary: string;
    LexerLibFilename: string;
    LexerThemes: boolean;
    PictureTypes: string;

    AutocompleteCss: boolean;
    AutocompleteHtml: boolean;
    AutocompleteAutoshowChars: integer;
    AutocompleteAutoshowLexers: string;

    ListboxWidth: integer;
    ListboxItemCountCmd: integer;
    ListboxItemCountBm: integer;
    ListboxItemCountTabs: integer;
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
    TabShowX: boolean;
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
    PyChangeSlow: integer;

    NewdocLexer: string;
    NewdocEnc: string;
    NewdocEnds: integer;

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
  end;
var
  UiOps: TUiOps;

const
  str_OsSuffix =
    {$ifdef windows} '' {$endif}
    {$ifdef linux} '__linux' {$endif}
    {$ifdef darwin} '__osx' {$endif} ;
  str_FontName = 'font_name'+str_OsSuffix;
  str_FontSize = 'font_size'+str_OsSuffix;
  str_FontQuality = 'font_quality'+str_OsSuffix;
  str_UiFontName = 'ui_font_name'+str_OsSuffix;
  str_UiFontSize = 'ui_font_size'+str_OsSuffix;
  str_UiFontOutputName = 'ui_font_output_name'+str_OsSuffix;
  str_UiFontOutputSize = 'ui_font_output_size'+str_OsSuffix;

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

    //unprinted
    OpUnprintedShow: boolean;
    OpUnprintedSpaces: boolean;
    OpUnprintedEnds: boolean;
    OpUnprintedEndDetails: boolean;
    OpUnprintedReplaceSpec: boolean;

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
    OpShowCurLineMin: boolean;
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
function GetActiveControl(Form: TWinControl): TWinControl;
function GetListboxItemHeight(const AFontName: string; AFontSize: integer): integer;

function MsgBox(const Str: string; Flags: Longint): integer;
function AppFindLexer(const fn: string): TecSyntAnalyzer;
procedure DoSaveKeyItem(K: TATKeymapItem; const path: string);
procedure DoEnumLexers(L: TStringList; AlsoDisabled: boolean = false);
procedure DoLexerExportFromLibToFile(an: TecSyntAnalyzer);

function CommandPlugins_GetIndexFromModuleAndMethod(AStr: string): integer;
procedure CommandPlugins_UpdateSubcommands(AStr: string);

var
  AppManager: TecSyntaxManager = nil;
  AppKeymap: TATKeymap = nil;
  AppShortcutEscape: TShortcut = 0;
  AppLangName: string = '';

type
  TStrEvent = procedure(Sender: TObject; const ARes: string) of object;
  TStrFunction = function(const AStr: string): boolean of object;

const
  cEncNameUtf8 = 'UTF-8';
  cEncNameUtf8NoBom = 'UTF-8 no bom';
  cEncNameUtf16LE = 'UTF-16 LE';
  cEncNameUtf16BE = 'UTF-16 BE';
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
    cEventOnClose,
    cEventOnSaveAfter,
    cEventOnSaveBefore,
    cEventOnKey,
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
    cEventOnMacro
    );
  TAppPyEvents = set of TAppPyEvent;

const
  cAppPyEvent: array[TAppPyEvent] of string = (
    'on_open',
    'on_close',
    'on_save',
    'on_save_pre',
    'on_key',
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
    'on_macro'
    );

const
  cMaxItemsInInstallInf = 200;

type
  TAppPluginCmd = record
    ItemModule: string;
    ItemProc: string;
    ItemProcParam: string;
    ItemCaption: string;
    ItemLexers: string;
    ItemInMenu: boolean;
  end;
  TAppPluginCmdArray = array[0..400] of TAppPluginCmd;

type
  TAppPluginEvent = record
    ItemModule: string;
    ItemLexers: string;
    ItemEvents: TAppPyEvents;
    ItemKeys: string;
  end;
  TAppPluginEventArray = array[0..100] of TAppPluginEvent;

var
  FPluginsCmd: TAppPluginCmdArray;
  FPluginsEvents: TAppPluginEventArray;

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
  FAppSidePanels: array[0..10] of TAppSidePanel;


implementation

function MsgBox(const Str: string; Flags: Longint): integer;
begin
  Result:= Application.MessageBox(PChar(Str), PChar(msgTitle), Flags);
end;

function InitPyLibraryPath: string;
begin
  {$ifdef windows}
    Result:= 'python33.dll';
  {$endif}
  {$ifdef linux}
    Result:= 'libpython3.4m.so.1.0';
  {$endif}
  {$ifdef darwin}
    Result:= '/Library/Frameworks/Python.framework/Versions/3.3/lib/libpython3.3.dylib';
    if FileExists(Result) then exit;
    Result:= '/Library/Frameworks/Python.framework/Versions/3.4/lib/libpython3.4.dylib';
    if FileExists(Result) then exit;
    Result:= '/Library/Frameworks/Python.framework/Versions/3.5/lib/libpython3.5.dylib';
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
    OpFontName:= {$ifndef darwin} 'Courier New' {$else} 'Monaco' {$endif};
    OpFontSize:= {$ifndef darwin} 9 {$else} 11 {$endif};
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
    OpNumbersStyle:= Ord(cNumbersEach10th);
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

    OpUnprintedShow:= false;
    OpUnprintedSpaces:= true;
    OpUnprintedEnds:= true;
    OpUnprintedEndDetails:= false;
    OpUnprintedReplaceSpec:= true;

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
    OpCaretVirtual:= true;
    OpCaretMulti:= true;
    OpCaretAfterPasteColumn:= Ord(cPasteCaretColumnRight);

    OpShowCurLine:= false;
    OpShowCurLineMin:= true;
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


procedure InitUiOps(var Op: TUiOps);
begin
  with Op do
  begin
    VarFontName:= 'default';
    VarFontSize:= {$ifdef windows} 9 {$else} 10 {$endif};

    OutputFontName:= VarFontName;
    OutputFontSize:= VarFontSize;

    LexerLibFilename:= GetAppPath(cDirDataLexlib)+DirectorySeparator+'lib.lxl';
    LexerThemes:= true;
    PyLibrary:= InitPyLibraryPath;
    PictureTypes:= 'bmp,png,jpg,jpeg,gif,ico';

    AutocompleteCss:= true;
    AutocompleteHtml:= true;
    AutocompleteAutoshowChars:= 0;
    AutocompleteAutoshowLexers:= '';

    ListboxWidth:= 450;
    ListboxItemCountCmd:= 15;
    ListboxItemCountBm:= 10;
    ListboxItemCountTabs:= 30;
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
    TabShowX:= true;
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

    EscapeClose:= false;
    EscapeCloseConsole:= true;
    InitialDir:= '';

    ExportHtmlNumbers:= false;
    ExportHtmlFontSize:= 12;
    ExportHtmlFontName:= 'Courier New';

    TreeAutoSync:= true;
    TreeTimeFill:= 2000;
    TreeTimeFocus:= 2000;
    TreeShowLines:= true;
    PyChangeSlow:= 2000;

    NewdocLexer:= '';
    NewdocEnc:= cEncNameUtf8;
    NewdocEnds:= {$ifdef windows} Ord(cEndWin) {$else} Ord(cEndUnix) {$endif};

    StatusNoSel:= 'Ln {y}, Col {x}';
    StatusSmallSel:= 'Ln {y}, Col {x}, sel';
    StatusStreamSel:= 'Ln {y}, Col {x}, {sel} lines sel';
    StatusColSel:= '{sel}x{cols} column';
    StatusCarets:= '{carets} carets, {sel} lines sel';
    StatusPanels:= 'caret,C,170|enc,C,105|ends,C,50|lexer,C,140|tabsize,C,80|msg,L,4000';
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
    BackupMode:= '';
  end;
end;

function GetAppLexerOverrideFilename(AName: string): string;
begin
  AName:= StringReplace(AName, '/', '_', [rfReplaceAll]);
  AName:= StringReplace(AName, '\', '_', [rfReplaceAll]);
  AName:= StringReplace(AName, '*', '_', [rfReplaceAll]);
  Result:= GetAppPath(cDirSettings)+DirectorySeparator+'lexer '+AName+'.json';
end;

function AppFindLexer(const fn: string): TecSyntAnalyzer;
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


procedure DoSaveKeyItem(K: TATKeymapItem; const path: string);
var
  c: TJSONConfig;
  i: integer;
  sl: tstringlist;
begin
  c:= TJSONConfig.Create(nil);
  sl:= TStringlist.create;
  try
    c.Formatted:= true;
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

    c.DeleteValue(path+'/k1');
    c.DeleteValue(path+'/k2');
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


procedure DoEnumLexers(L: TStringList; AlsoDisabled: boolean = false);
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
    end;
  end;
  with FPluginsCmd[High(FPluginsCmd)] do
  begin
    ItemModule:= '';
    ItemProc:= '';
    ItemProcParam:= '';
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

finalization
  FreeAndNil(AppKeymap);
  FreeAndNil(AppBookmarkImagelist);

end.

