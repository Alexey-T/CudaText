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
  {$ifdef windows}
  Windows,
  {$endif}
  Classes, SysUtils, Forms, Controls, Menus,
  Dialogs, Graphics, ExtCtrls, ComCtrls,
  InterfaceBase,
  LclProc, LclType, LazFileUtils, LazUTF8,
  FileUtil, IniFiles, StrUtils,
  Process,
  ATSynEdit,
  ATSynEdit_Keymap,
  ATSynEdit_Keymap_Init,
  ATSynEdit_Adapter_litelexer,
  ATStringProc,
  ATButtons,
  ATListbox,
  ATStatusBar,
  at__jsonconf,
  proc_cmd,
  proc_msg,
  proc_keymap_undolist,
  ec_LexerList,
  ec_proc_lexer,
  ec_SyntAnal;

var
  //ATSynEdit has range for bookmarks 0..63, 0=none
  AppBookmarkSetup: array[1..63] of
    record ImageIndex: integer; Color: TColor; end;
  AppBookmarkImagelist: TImageList = nil;
  AppFolderOfLastInstalledAddon: string = '';

const
  AppExtensionThemeUi = '.cuda-theme-ui';
  AppExtensionThemeSyntax = '.cuda-theme-syntax';


type
  TAppPathId = (
    cDirSettings,
    cDirSettingsDefault,
    cDirPy,
    cDirData,
    cDirDataLexers,
    cDirDataLexersLite,
    cDirDataNewdoc,
    cDirDataThemes,
    cDirDataAutocomplete,
    cDirDataAutocompleteSpec,
    cDirDataLangs,
    cDirDataSideIcons,
    cDirDataTreeIcons,
    cDirDataToolBarIcons,
    cDirReadme,
    cDirLastInstalledAddon,
    cFileOptionsHistory,
    cFileOptionsDefault,
    cFileOptionsUser,
    cFileOptionsFiletypes,
    cFileOptionsKeymap,
    cFileOptionsHistoryFiles,
    cFileLexerStylesBackup,
    cFileReadmeHistory,
    cFileReadmeHelpMouse,
    cFileReadmeHelpLexers
    );

type
  TUiOps = record
    ScreenScale: integer;

    VarFontName: string;
    VarFontSize: integer;
    OutputFontName: string;
    OutputFontSize: integer;
    DoubleBuffered: boolean;

    PyLibrary: string;
    PyChangeSlow: integer;
    PyInitLog: boolean;

    LexerThemes: boolean;
    LexerMenuGrouped: boolean;
    LexerDelayedParsingPause: integer;
    LexerDelayedParsingSize: integer;

    ToolBarTheme: string;

    SidebarShow: boolean;
    SidebarTheme: string;
    PictureTypes: string;
    MaxFileSizeToOpen: integer;
    MaxFileSizeForLexer: integer;
    MaxLinesForTree: integer;

    AutocompleteCss: boolean;
    AutocompleteHtml: boolean;
    AutocompleteAutoshowCharCount: integer;
    AutocompleteTriggerChars: string;
    AutocompleteAddOpeningBracket: boolean;
    AutoCloseBrackets: string;

    ListboxSizeX: integer;
    ListboxSizeY: integer;
    ListboxCompleteSizeX: integer;
    ListboxCompleteSizeY: integer;
    ListboxFuzzySearch: boolean;

    TabMultiline: boolean;
    TabAngled: boolean;
    TabFlat: boolean;
    TabWidth: integer;
    TabWidthMin: integer;
    TabHeight: integer;
    TabHeightInner: integer;
    TabSpacer: integer;
    TabPosition: integer;
    TabColorFull: boolean;
    TabShowX: integer;
    TabShowPlus: boolean;
    TabDblClickClose: boolean;
    TabNumbers: boolean;
    TabNewNearCurrent: boolean;
    TabButtonLayout: string;
    TabPreviewFontStyle: string;

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
    ConsoleWordWrap: boolean;
    InitialDir: string;

    ExportHtmlNumbers: boolean;
    ExportHtmlFontName: string;
    ExportHtmlFontSize: integer;

    TreeTheme: string;
    TreeAutoSync: boolean;
    TreeTimeFill: integer;
    TreeTimeFocus: integer;
    TreeShowLines: boolean;
    TreeShowIcons: boolean;
    TreeShowTooltips: boolean;
    TreeCache: boolean;

    NewdocLexer: string;
    NewdocEnc: string;
    NewdocEnds: integer;

    DefaultEncUtf8: boolean;
    ViewerBinaryWidth: integer;

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
    StatusWrap: array[0..2] of string;

    ShowActiveBorder: boolean;
    ShowSidebarCaptions: boolean;
    ShowTitlePath: boolean;
    ShowLastFiles: boolean;
    ShowFormsOnTop: boolean;

    OneInstance: boolean;
    NotifEnabled: boolean;
    NotifTimeSec: integer;
    NonTextFiles: integer; //0: prompt, 1: open, 2: don't open
    NonTextFilesBufferKb: integer;
    ReloadUnsavedConfirm: boolean;
    ReloadFollowTail: boolean;
    FullScreen: string;
    MouseGotoDefinition: string;
    DebugLog: boolean;

    HotkeyFindDialog,
    HotkeyReplaceDialog,
    HotkeyFindFirst,
    HotkeyFindNext,
    HotkeyFindPrev,
    HotkeyReplaceAndFindNext,
    HotkeyReplaceNoFindNext,
    HotkeyReplaceAll,
    HotkeyCountAll,
    HotkeySelectAll,
    HotkeyMarkAll,
    HotkeyToggleRegex,
    HotkeyToggleCaseSens,
    HotkeyToggleWords,
    HotkeyToggleWrapped,
    HotkeyToggleInSelect,
    HotkeyToggleMultiline,
    HotkeyToggleConfirmRep
      : string;
  end;
var
  UiOps: TUiOps;

const
  str_FontName = 'font_name'+cOptionSystemSuffix;
  str_FontSize = 'font_size'+cOptionSystemSuffix;
  str_FontQuality = 'font_quality'+cOptionSystemSuffix;
  str_FontLigatures = 'font_ligatures'; //+cOptionSystemSuffix;
  str_UiFontName = 'ui_font_name'+cOptionSystemSuffix;
  str_UiFontSize = 'ui_font_size'+cOptionSystemSuffix;
  str_UiFontOutputName = 'ui_font_output_name'+cOptionSystemSuffix;
  str_UiFontOutputSize = 'ui_font_output_size'+cOptionSystemSuffix;
  str_UiDoubleBuffered = 'ui_buffered'+cOptionSystemSuffix;
  str_DefEncodingIsUtf8 = 'def_encoding_utf8'+cOptionSystemSuffix;

type
  TEditorOps = record
    OpFontName: string;
    OpFontSize: integer;
    OpFontQuality: TFontQuality;
    OpFontLigatures: boolean;

    OpSpacingX: integer;
    OpSpacingY: integer;
    OpTabSize: integer;
    OpTabSpaces: boolean;
    OpTabMaxPosExpanded: integer;

    OpOverwriteSel: boolean;
    OpOverwriteOnPaste: boolean;
    OpUnderlineColorFiles: string;
    OpUnderlineColorSize: integer;
    OpLinks: boolean;
    OpLinksRegex: string;
    OpAllowWideChars: boolean;

    //view
    OpGutterShow: boolean;
    OpGutterFold: boolean;
    OpGutterFoldAlways: boolean;
    OpGutterFoldIcons: integer;
    OpGutterBookmarks: boolean;

    OpNumbersShow: boolean;
    OpNumbersFontSize: integer;
    OpNumbersStyle: integer;
    OpNumbersForCarets: boolean;
    OpNumbersCenter: boolean;

    OpRulerShow: boolean;
    OpRulerNumeration: integer;
    OpRulerFontSize: integer;
    OpRulerSize: integer;
    OpRulerTextIndent: integer;

    OpMinimapShow: boolean;
    OpMinimapShowSelAlways: boolean;
    OpMinimapShowSelBorder: boolean;
    OpMinimapCharWidth: integer;
    OpMinimapAtLeft: boolean;
    OpMinimapTooltipShow: boolean;
    OpMinimapTooltipLineCount: integer;
    OpMicromapShow: boolean;
    OpMicromapWidth: integer;
    OpMicromapWidthSmall: integer;
    OpMarginFixed: integer;
    OpMarginString: string;
    OpStaplesStyle: integer;

    //unprinted
    OpUnprintedShow: boolean;
    OpUnprintedSpaces: boolean;
    OpUnprintedSpacesTrailing: boolean;
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
    OpWrapEnabledMaxLines: integer;

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
    OpCaretsAddedToColumnSel: boolean;

    //general
    OpShowCurLine: boolean;
    OpShowCurLineMinimal: boolean;
    OpShowCurLineOnlyFocused: boolean;
    OpShowCurCol: boolean;
    OpShowLastLineOnTop: boolean;
    OpShowFullBackgroundSel: boolean;
    OpShowFullBackgroundSyntax: boolean;
    OpCopyLineIfNoSel: boolean;
    OpCutLineIfNoSel: boolean;
    OpCopyColumnAlignedBySpaces: boolean;
    OpSavingTrimSpaces: boolean;
    OpSavingForceFinalEol: boolean;
    OpShowHintOnVertScroll: boolean;
    OpCenteringWidth: integer;
    OpCenteringForDistractionFree: integer;
    OpHideHorizScrollbar: boolean;
    OpLexerDynamicHiliteEnabled: boolean;
    OpLexerDynamicHiliteMaxLines: integer;
    OpLexerLineSeparators: boolean;

    OpWordChars: UnicodeString;
    OpHexChars: UnicodeString;
    OpFoldStyle: integer;

    //indent
    OpIndentAuto: boolean;
    OpIndentAutoKind: integer;
    OpIndentSize: integer;
    OpUnIndentKeepsAlign: boolean;
    OpIndentMakesWholeLineSel: boolean;

    //mouse
    OpMouse2ClickDragSelectsWords: boolean;
    OpMouseDragDrop: boolean;
    OpMouseDragDropFocusTarget: boolean;
    OpMouseMiddleClickNiceScroll: boolean;
    OpMouseMiddleClickPaste: boolean;
    OpMouseRightClickMovesCaret: boolean;
    OpMouseEnableColumnSelection: boolean;
    OpMouseHideCursorOnType: boolean; //don't work on lin
    OpMouseGutterClickSelectedLine: boolean;
    OpMouseWheelZoom: boolean;
    OpMouseWheelSpeedVert: integer;
    OpMouseWheelSpeedHorz: integer;
    OpMouseClickNumberSelectsEol: boolean;

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
function GetAppLexerSpecificConfig(AName: string): string;
function GetAppLexerPropInCommentsSection(const ALexerName, AKey: string): string;

//function GetActiveControl(Form: TWinControl): TWinControl;
function GetListboxItemHeight(const AFontName: string; AFontSize: integer): integer;
function MsgBox(const Str: string; Flags: Longint): integer;
procedure MsgStdout(const Str: string; AllowMsgBox: boolean = false);

function GetAppKeymap_LexerSpecificConfig(AName: string): string;
function GetAppKeymapHotkey(const ACmdString: string): string;
function SetAppKeymapHotkey(AParams: string): boolean;

function AppKeymapCheckDuplicateForCommand(
  AKeymapItem: TATKeymapItem;
  const ALexerName: string;
  AOverwriteAndSave: boolean): integer;
function AppKeymapHasDuplicateForKey(AHotkey, AKeyComboSeparator: string): boolean;
procedure AppKeymap_ApplyUndoList(AUndoList: TATKeymapUndoList);

function DoOps_HotkeyStringId_To_CommandCode(const AId: string): integer;
function DoOps_CommandCode_To_HotkeyStringId(ACmd: integer): string;
procedure DoOps_SaveKeyItem(K: TATKeymapItem; const path, ALexerName: string; ALexerSpecific: boolean);
procedure DoOps_SaveKey_ForPluginModuleAndMethod(AOverwriteKey: boolean;
  const AMenuitemCaption, AModuleName, AMethodName, ALexerName, AHotkey: string);

function DoLexerFindByFilename(const fn: string): TecSyntAnalyzer;
procedure DoLexerEnum(L: TStringList; AlsoDisabled: boolean = false);
procedure DoLexerExportFromLibToFile(an: TecSyntAnalyzer);

var
  AppManager: TecLexerList = nil;
  AppManagerLite: TATLiteLexers = nil;
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
  cEncNameOem = 'OEM';

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
  TAppEncodingRecord = record
    Sub,
    Name,
    ShortName: string;
  end;

const
  AppEncodings: array[0..31] of TAppEncodingRecord = (
    (Sub: ''; Name: cEncNameUtf8_NoBom; ShortName: 'utf8'),
    (Sub: ''; Name: cEncNameUtf8_WithBom; ShortName: 'utf8_bom'),
    (Sub: ''; Name: cEncNameUtf16LE_NoBom; ShortName: 'utf16le'),
    (Sub: ''; Name: cEncNameUtf16LE_WithBom; ShortName: 'utf16le_bom'),
    (Sub: ''; Name: cEncNameUtf16BE_NoBom; ShortName: 'utf16be'),
    (Sub: ''; Name: cEncNameUtf16BE_WithBom; ShortName: 'utf16be_bom'),
    (Sub: ''; Name: cEncNameAnsi; ShortName: 'ansi'),
    (Sub: ''; Name: cEncNameOem; ShortName: 'oem'),
    (Sub: ''; Name: '-'; ShortName: ''),
    (Sub: 'eu'; Name: cEncNameCP1250; ShortName: cEncNameCP1250),
    (Sub: 'eu'; Name: cEncNameCP1251; ShortName: cEncNameCP1251),
    (Sub: 'eu'; Name: cEncNameCP1252; ShortName: cEncNameCP1252),
    (Sub: 'eu'; Name: cEncNameCP1253; ShortName: cEncNameCP1253),
    (Sub: 'eu'; Name: cEncNameCP1257; ShortName: cEncNameCP1257),
    (Sub: 'eu'; Name: '-'; ShortName: ''),
    (Sub: 'eu'; Name: cEncNameCP437; ShortName: cEncNameCP437),
    (Sub: 'eu'; Name: cEncNameCP850; ShortName: cEncNameCP850),
    (Sub: 'eu'; Name: cEncNameCP852; ShortName: cEncNameCP852),
    (Sub: 'eu'; Name: cEncNameCP866; ShortName: cEncNameCP866),
    (Sub: 'eu'; Name: '-'; ShortName: ''),
    (Sub: 'eu'; Name: cEncNameISO1; ShortName: cEncNameISO1),
    (Sub: 'eu'; Name: cEncNameISO2; ShortName: cEncNameISO2),
    (Sub: 'eu'; Name: cEncNameMac; ShortName: 'mac'),
    (Sub: 'mi'; Name: cEncNameCP1254; ShortName: cEncNameCP1254),
    (Sub: 'mi'; Name: cEncNameCP1255; ShortName: cEncNameCP1255),
    (Sub: 'mi'; Name: cEncNameCP1256; ShortName: cEncNameCP1256),
    (Sub: 'as'; Name: cEncNameCP874; ShortName: cEncNameCP874),
    (Sub: 'as'; Name: cEncNameCP932; ShortName: cEncNameCP932),
    (Sub: 'as'; Name: cEncNameCP936; ShortName: cEncNameCP936),
    (Sub: 'as'; Name: cEncNameCP949; ShortName: cEncNameCP949),
    (Sub: 'as'; Name: cEncNameCP950; ShortName: cEncNameCP950),
    (Sub: 'as'; Name: cEncNameCP1258; ShortName: cEncNameCP1258)
  );


type
  TAppPyEvent = (
    cEventOnKey,
    cEventOnKeyUp,
    cEventOnHotspot,
    cEventOnInsert,
    cEventOnChange,
    cEventOnChangeSlow,
    cEventOnCaret,
    cEventOnScroll,
    cEventOnClick,
    cEventOnClickDbl,
    cEventOnClickGap,
    cEventOnState,
    cEventOnFocus,
    cEventOnStart,
    cEventOnOpen,
    cEventOnOpenBefore,
    cEventOnClose,
    cEventOnSaveAfter,
    cEventOnSaveBefore,
    cEventOnLexer,
    cEventOnComplete,
    cEventOnGotoEnter,
    cEventOnGotoCaret,
    cEventOnGotoChange,
    cEventOnGotoKey,
    cEventOnGotoKeyUp,
    cEventOnGotoDef,
    cEventOnFuncHint,
    cEventOnTabChange,
    cEventOnTabMove,
    cEventOnPaste,
    cEventOnGroup,
    cEventOnConsole,
    cEventOnConsoleNav,
    cEventOnOutputNav,
    cEventOnSnippet,
    cEventOnMacro
    );
  TAppPyEvents = set of TAppPyEvent;
  TAppPyEventsPrior = array[TAppPyEvent] of byte;
    //0: default, 1,2...: higher priority

const
  cAppPyEvent: array[TAppPyEvent] of string = (
    'on_key',
    'on_key_up',
    'on_hotspot',
    'on_insert',
    'on_change',
    'on_change_slow',
    'on_caret',
    'on_scroll',
    'on_click',
    'on_click_dbl',
    'on_click_gap',
    'on_state',
    'on_focus',
    'on_start',
    'on_open',
    'on_open_pre',
    'on_close',
    'on_save',
    'on_save_pre',
    'on_lexer',
    'on_complete',
    'on_goto_enter',
    'on_goto_caret',
    'on_goto_change',
    'on_goto_key',
    'on_goto_key_up',
    'on_goto_def',
    'on_func_hint',
    'on_tab_change',
    'on_tab_move',
    'on_paste',
    'on_group',
    'on_console',
    'on_console_nav',
    'on_output_nav',
    'on_snippet',
    'on_macro'
    );

const
  cMaxItemsInInstallInf = 400;
  cMaxSidebarItemsInInstallInf = 3;
  cMaxCommandPlugins = 800;
  cMaxEventPlugins = 100;
  cMaxSidePanels = 40;
  cMaxBottomPanels = 40;

type
  TAppPluginCmd = record
    ItemModule: string;
    ItemProc: string;
    ItemProcParam: string;
    ItemCaption: string;
    ItemLexers: string;
    ItemInMenu: string;
    ItemFromApi: boolean;
  end;
  TAppPluginCmdArray = array[0..cMaxCommandPlugins-1] of TAppPluginCmd;

type
  TAppPluginEvent = record
    ItemModule: string;
    ItemLexers: string;
    ItemEvents: TAppPyEvents;
    ItemEventsPrior: TAppPyEventsPrior;
    ItemKeys: string;
  end;
  TAppPluginEventArray = array[0..cMaxEventPlugins-1] of TAppPluginEvent;

type
  TAppSidePanel = record
    ItemCaption: string;
    ItemControl: TCustomControl;
    ItemModule: string;
    ItemMethod: string;
  end;

var
  AppPluginsCommand: TAppPluginCmdArray;
  AppPluginsEvent: TAppPluginEventArray;
  AppSidePanels: array[0..cMaxSidePanels-1] of TAppSidePanel;
  AppBottomPanels: array[0..cMaxBottomPanels-1] of TAppSidePanel;

type
  PAppPanelProps = ^TAppPanelProps;
  TAppPanelProps = record
    Listbox: TATListbox;
    RegexStr: string;
    RegexIdLine,
    RegexIdCol,
    RegexIdName: integer;
    DefFilename: string;
    ZeroBase: boolean;
    Encoding: string;
  end;

type
  TAppMenuProps = class
  public
    CommandCode: integer;
    CommandString: string;
    TagString: string;
  end;

function CommandPlugins_GetIndexFromModuleAndMethod(AStr: string): integer;
procedure CommandPlugins_UpdateSubcommands(AStr: string);
procedure CommandPlugins_DeleteItem(AIndex: integer);
procedure CommandPlugins_AssignItem(var Dst, Src: TAppPluginCmd);

function AppEncodingShortnameToFullname(const S: string): string;
function AppEncodingFullnameToShortname(const S: string): string;
function AppEncodingListAsString: string;
function AppEncodingOem: string;

procedure UpdateFormOnTop(F: TForm);
procedure DoStatusbarTextByTag(AStatus: TATStatus; ATag: PtrInt; const AText: string);
function IsFileTooBigForOpening(const AFilename: string): boolean;
function IsFileTooBigForLexer(const AFilename: string): boolean;
procedure DoLexerDetect(const AFilename: string;
  out Lexer: TecSyntAnalyzer;
  out LexerLite: TATLiteLexer;
  out LexerName: string);


implementation

function MsgBox(const Str: string; Flags: Longint): integer;
begin
  Result:= Application.MessageBox(PChar(Str), PChar(msgTitle), Flags);
end;


function InitPyLibraryPath: string;
  //
  function GetMacPath(NMinorVersion: integer): string;
  begin
    Result:= Format('/Library/Frameworks/Python.framework/Versions/3.%d/lib/libpython3.%d.dylib',
      [NMinorVersion, NMinorVersion]);
  end;
  //
var
  N: integer;
begin
  Result:= '';

  {$ifdef windows}
  exit('python35.dll');
  {$endif}

  {$ifdef linux}
  exit('libpython3.5m.so.1.0');
  {$endif}

  {$ifdef freebsd}
  exit('libpython3.6m.so');
  {$endif}

  {$ifdef darwin}
  for N:= 4 to 9 do
  begin
    Result:= GetMacPath(N);
    if FileExists(Result) then exit;
  end;
  {$endif}
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
  {$ifdef freebsd} '' {$endif}
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
    cDirSettingsDefault:
      begin
        Result:= OpDirLocal+DirectorySeparator+'settings_default';
      end;
    cDirPy:
      begin
        Result:= OpDirLocal+DirectorySeparator+'py';
      end;

    cDirData:
      begin
        Result:= OpDirLocal+DirectorySeparator+'data';
      end;
    cDirDataLexers:
      begin
        Result:= OpDirLocal+DirectorySeparator+'data'+DirectorySeparator+'lexlib';
      end;
    cDirDataLexersLite:
      begin
        Result:= OpDirLocal+DirectorySeparator+'data'+DirectorySeparator+'lexliblite';
      end;
    cDirDataNewdoc:
      begin
        Result:= OpDirLocal+DirectorySeparator+'data'+DirectorySeparator+'newdoc';
      end;
    cDirDataThemes:
      begin
        Result:= OpDirLocal+DirectorySeparator+'data'+DirectorySeparator+'themes';
      end;
    cDirDataAutocomplete:
      begin
        Result:= OpDirLocal+DirectorySeparator+'data'+DirectorySeparator+'autocomplete';
      end;
    cDirDataAutocompleteSpec:
      begin
        Result:= OpDirLocal+DirectorySeparator+'data'+DirectorySeparator+'autocompletespec';
      end;
    cDirDataLangs:
      begin
        Result:= OpDirLocal+DirectorySeparator+'data'+DirectorySeparator+'lang';
      end;
    cDirDataSideIcons:
      begin
        Result:= OpDirLocal+DirectorySeparator+'data'+DirectorySeparator+'sideicons'+DirectorySeparator+UiOps.SidebarTheme;
      end;
    cDirDataTreeIcons:
      begin
        Result:= OpDirLocal+DirectorySeparator+'data'+DirectorySeparator+'codetreeicons'+DirectorySeparator+UiOps.TreeTheme;
      end;
    cDirDataToolBarIcons:
      begin
        Result:= OpDirLocal+DirectorySeparator+'data'+DirectorySeparator+'toolbaricons'+DirectorySeparator+UiOps.ToolBarTheme;
      end;

    cDirReadme:
      begin
        Result:= OpDirLocal+DirectorySeparator+'readme';
      end;
    cDirLastInstalledAddon:
      begin
        Result:= AppFolderOfLastInstalledAddon;
      end;
    cFileOptionsDefault:
      begin
        Result:= GetAppPath(cDirSettingsDefault)+DirectorySeparator+'default.json';
      end;
    cFileOptionsHistory:
      begin
        Result:= GetAppPath(cDirSettings)+DirectorySeparator+'history.json';
      end;
    cFileOptionsUser:
      begin
        Result:= GetAppPath(cDirSettings)+DirectorySeparator+'user.json';
      end;
    cFileOptionsFiletypes:
      begin
        Result:= GetAppPath(cDirSettings)+DirectorySeparator+'filetypes.json';
      end;
    cFileOptionsKeymap:
      begin
        Result:= GetAppPath(cDirSettings)+DirectorySeparator+'keys.json';
      end;
    cFileOptionsHistoryFiles:
      begin
        Result:= GetAppPath(cDirSettings)+DirectorySeparator+'history files.json';
      end;
    cFileLexerStylesBackup:
      begin
        Result:= GetAppPath(cDirSettings)+DirectorySeparator+'lexer styles backup.ini';
      end;

    cFileReadmeHistory:
      begin
        Result:= GetAppPath(cDirReadme)+DirectorySeparator+'history.txt';
      end;
    cFileReadmeHelpMouse:
      begin
        Result:= GetAppPath(cDirReadme)+DirectorySeparator+'help mouse.txt';
      end;
    cFileReadmeHelpLexers:
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
      RunCommand('cp', ['-R', '-u', '-t',
        OpDirLocal,
        '/usr/share/cudatext/py',
        '/usr/share/cudatext/data',
        '/usr/share/cudatext/readme',
        '/usr/share/cudatext/settings_default'
        ], S);
      {$endif}
      {$ifdef darwin}
      //see rsync help. need options:
      // -u (update)
      // -r (recursive)
      // -t (preserve times)
      RunCommand('rsync', ['-urt',
        OpDirPrecopy+'/',
        OpDirLocal
        ], S);
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
      {$ifdef freebsd} 'Courier New' {$endif}
      {$ifdef darwin} 'Monaco' {$endif} ;

    OpFontSize:= {$ifdef LCLCocoa}13{$else}10{$endif};
    OpFontQuality:= fqDefault;
    OpFontLigatures:= false;

    OpSpacingX:= 0;
    OpSpacingY:= 1;

    OpTabSize:= 8;
    OpTabSpaces:= false;
    OpTabMaxPosExpanded:= 500;

    OpOverwriteSel:= true;
    OpOverwriteOnPaste:= false;

    OpUnderlineColorFiles:= '*';
    OpUnderlineColorSize:= 3;
    OpLinks:= true;
    OpLinksRegex:= ATSynEdit.cUrlRegexInitial;
    OpAllowWideChars:= true;

    OpGutterShow:= true;
    OpGutterFold:= true;
    OpGutterFoldAlways:= true;
    OpGutterBookmarks:= true;
    OpGutterFoldIcons:= 0;

    OpNumbersShow:= true;
    OpNumbersFontSize:= 0;
    OpNumbersStyle:= Ord(cNumbersAll);
    OpNumbersForCarets:= false;
    OpNumbersCenter:= true;

    OpRulerShow:= false;
    OpRulerNumeration:= 0;
    OpRulerFontSize:= 8;
    OpRulerSize:= 20;
    OpRulerTextIndent:= 0;

    OpMinimapShow:= false;
    OpMinimapShowSelAlways:= false;
    OpMinimapShowSelBorder:= true;
    OpMinimapCharWidth:= 0;
    OpMinimapAtLeft:= false;
    OpMinimapTooltipShow:= true;
    OpMinimapTooltipLineCount:= 6;

    OpMicromapShow:= false;
    OpMicromapWidth:= 12;
    OpMicromapWidthSmall:= 4;

    OpMarginFixed:= 2000; //hide margin
    OpMarginString:= '';
    OpStaplesStyle:= 1; //Ord(cLineStyleSolid)

    OpUnprintedShow:= false;
    OpUnprintedSpaces:= true;
    OpUnprintedSpacesTrailing:= false;
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
    OpWrapEnabledMaxLines:= 60*1000;

    OpUndoLimit:= 5000;
    OpUndoGrouped:= true;
    OpUndoAfterSave:= true;

    OpCaretBlinkTime:= cInitTimerBlink;
    OpCaretBlinkEn:= true;
    OpCaretShapeNorm:= Ord(cInitCaretShapeIns);
    OpCaretShapeOvr:= Ord(cInitCaretShapeOvr);
    OpCaretShapeRO:= Ord(cInitCaretShapeRO);
    OpCaretVirtual:= false;
    OpCaretMulti:= true;
    OpCaretAfterPasteColumn:= Ord(cPasteCaretColumnRight);
    OpCaretsAddedToColumnSel:= true;

    OpShowCurLine:= false;
    OpShowCurLineMinimal:= true;
    OpShowCurLineOnlyFocused:= false;
    OpShowCurCol:= false;
    OpShowLastLineOnTop:= true;
    OpShowFullBackgroundSel:= false;
    OpShowFullBackgroundSyntax:= true;
    OpCopyLineIfNoSel:= true;
    OpCutLineIfNoSel:= false;
    OpCopyColumnAlignedBySpaces:= true;
    OpSavingTrimSpaces:= false;
    OpSavingForceFinalEol:= false;
    OpShowHintOnVertScroll:= false;
    OpCenteringWidth:= 0;
    OpCenteringForDistractionFree:= 100;
    OpHideHorizScrollbar:= false;
    OpLexerDynamicHiliteEnabled:= false;
    OpLexerDynamicHiliteMaxLines:= 2000;
    OpLexerLineSeparators:= false;

    OpWordChars:= '';
    OpHexChars:= '';
    OpFoldStyle:= 1;

    OpIndentAuto:= true;
    OpIndentAutoKind:= Ord(cIndentAsPrevLine);
    OpIndentSize:= 2;
    OpUnIndentKeepsAlign:= true;
    OpIndentMakesWholeLineSel:= false;

    OpMouse2ClickDragSelectsWords:= true;
    OpMouseDragDrop:= true;
    OpMouseDragDropFocusTarget:= true;
    OpMouseMiddleClickNiceScroll:= true;
    OpMouseMiddleClickPaste:= false;
    OpMouseRightClickMovesCaret:= false;
    OpMouseEnableColumnSelection:= true;
    OpMouseHideCursorOnType:= false;
    OpMouseGutterClickSelectedLine:= true;
    OpMouseWheelZoom:= false;
    OpMouseWheelSpeedVert:= 3;
    OpMouseWheelSpeedHorz:= 10;
    OpMouseClickNumberSelectsEol:= true;

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
  {$ifdef linux}
  //Qt needs true (else caret dont blink, and tab angled borders paint bad)
  Exit(true);
  {$endif}

  Result:= WidgetSet.GetLCLCapability(lcCanDrawOutsideOnPaint) = LCL_CAPABILITY_YES;
end;


procedure InitUiOps(var Op: TUiOps);
begin
  with Op do
  begin
    ScreenScale:= 100;

    VarFontName:= 'default';
    VarFontSize:= {$ifdef LCLCocoa}13{$else}9{$endif};

    OutputFontName:= VarFontName;
    OutputFontSize:= VarFontSize;

    DoubleBuffered:= IsDoubleBufferedNeeded;

    LexerThemes:= true;
    LexerMenuGrouped:= true;
    LexerDelayedParsingPause:= 400;
    LexerDelayedParsingSize:= 100*1000;

    SidebarShow:= true;
    SidebarTheme:= 'common_20x20';
    TreeTheme:= 'default_16x16';
    ToolBarTheme:= 'default_24x24';

    PyLibrary:= InitPyLibraryPath;
    PictureTypes:= 'bmp,png,jpg,jpeg,gif,ico';

    MaxFileSizeToOpen:= 1000;
    MaxFileSizeForLexer:= 2;
    MaxLinesForTree:= 8000;

    AutocompleteCss:= true;
    AutocompleteHtml:= true;
    AutocompleteAutoshowCharCount:= 0;
    AutocompleteTriggerChars:= '';
    AutocompleteAddOpeningBracket:= true;
    AutoCloseBrackets:= '([{';

    ListboxSizeX:= 450;
    ListboxSizeY:= 300;
    ListboxCompleteSizeX:= 550;
    ListboxCompleteSizeY:= 200;
    ListboxFuzzySearch:= true;

    TabMultiline:= false;
    TabAngled:= {$ifdef darwin} false {$else} true {$endif};
    TabFlat:= false;
    TabWidth:= 170;
    TabWidthMin:= 40;
    TabHeight:= 25;
    TabHeightInner:= TabHeight-1;
    TabSpacer:= 2;
    TabPosition:= 0;
    TabColorFull:= false;
    TabShowX:= 1; //show all
    TabShowPlus:= true;
    TabDblClickClose:= false;
    TabNumbers:= false;
    TabNewNearCurrent:= false;
    TabButtonLayout:= '<>,v';
    TabPreviewFontStyle:= 'iu';

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
    ConsoleWordWrap:= true;
    InitialDir:= '';

    ExportHtmlNumbers:= false;
    ExportHtmlFontSize:= 12;
    ExportHtmlFontName:= 'Courier New';

    TreeAutoSync:= true;
    TreeTimeFill:= 2000;
    TreeTimeFocus:= 300;
    TreeShowLines:= false;
    TreeShowIcons:= true;
    TreeShowTooltips:= true;
    TreeCache:= true;

    PyChangeSlow:= 2000;
    PyInitLog:= true;

    NewdocLexer:= '';
    NewdocEnc:= 'utf8';
    NewdocEnds:= {$ifdef windows} Ord(cEndWin) {$else} Ord(cEndUnix) {$endif};

    DefaultEncUtf8:= {$ifdef windows} false {$else} true {$endif};
    ViewerBinaryWidth:= 100;

    StatusNoSel:= 'Ln {y}, Col {xx}';
    StatusSmallSel:= 'Ln {y}, Col {xx}, sel';
    StatusStreamSel:= 'Ln {y}, Col {xx}, {sel} lines sel';
    StatusColSel:= '{sel}x{cols} column';
    StatusCarets:= '{carets} carets, {sel} lines sel';
    StatusPanels:= 'caret,C,170|enc,C,125|ends,C,50|lexer,C,140|tabsize,C,80|selmode,C,15|msg,L,4000';
    StatusHeight:= TabHeight;
    StatusTime:= 5;
    StatusAltTime:= 7;
    StatusTabsize:= 'Tab size {tab}{_}';
    StatusWrap[0]:= 'no-wrap';
    StatusWrap[1]:= 'wrap';
    StatusWrap[2]:= 'wrap-m';

    ShowActiveBorder:= true;
    ShowSidebarCaptions:= false;
    ShowTitlePath:= false;
    ShowLastFiles:= true;
    ShowFormsOnTop:= false;

    OneInstance:= false;
    NotifEnabled:= true;
    NotifTimeSec:= 2;
    NonTextFiles:= 0;
    NonTextFilesBufferKb:= 64;
    ReloadFollowTail:= true;
    ReloadUnsavedConfirm:= true;
    FullScreen:= 'tp';
    MouseGotoDefinition:= 'a';
    DebugLog:= false;

    HotkeyFindDialog:= 'Ctrl+F';
    HotkeyReplaceDialog:= 'Ctrl+R';
    HotkeyFindFirst:= 'Alt+Enter';
    HotkeyFindNext:= '';
    HotkeyFindPrev:= 'Shift+Enter';
    HotkeyReplaceAndFindNext:= 'Alt+Z';
    HotkeyReplaceNoFindNext:= 'Ctrl+Alt+Z';
    HotkeyReplaceAll:= 'Alt+A';
    HotkeyCountAll:= 'Alt+O';
    HotkeySelectAll:= 'Alt+E';
    HotkeyMarkAll:= 'Alt+K';
    HotkeyToggleRegex:= 'Alt+R';
    HotkeyToggleCaseSens:= 'Alt+C';
    HotkeyToggleWords:= 'Alt+W';
    HotkeyToggleWrapped:= 'Alt+N';
    HotkeyToggleInSelect:= 'Alt+X';
    HotkeyToggleMultiline:= 'Alt+M';
    HotkeyToggleConfirmRep:= 'Alt+Y';
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

function GetAppLexerSpecificConfig(AName: string): string;
begin
  //support none-lexer here
  if AName='' then
    AName:= '-';
  SReplaceSpecialFilenameChars(AName);
  Result:= GetAppPath(cDirSettings)+DirectorySeparator+'lexer '+AName+'.json';
end;

function GetAppKeymap_LexerSpecificConfig(AName: string): string;
begin
  //support none-lexer
  if AName='' then
    AName:= '-';
  SReplaceSpecialFilenameChars(AName);
  Result:= GetAppPath(cDirSettings)+DirectorySeparator+'keys lexer '+AName+'.json';
end;


function DoOps_HotkeyStringId_To_CommandCode(const AId: string): integer;
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
  fn_opt:= GetAppPath(cFileOptionsFiletypes);
  if FileExistsUTF8(fn_opt) then
  begin
    c:= TJsonConfig.Create(nil);
    try
      c.FileName:= fn_opt;

      //by filename
      s:= c.GetValue(ExtractFileName(fn), '');
      if s<>'' then
      begin
        Result:= AppManager.FindLexerByName(s);
        Exit
      end;

      //by extention
      ext:= ExtractFileExt(fn);
      if ext<>'' then
      begin
        s:= c.GetValue('*'+ext, '');
        if s<>'' then
        begin
          Result:= AppManager.FindLexerByName(s);
          Exit
        end;
      end;
    finally
      c.Free;
    end;
  end;

  Result:= AppManager.FindLexerByFilename(fn);
end;

function DoOps_CommandCode_To_HotkeyStringId(ACmd: integer): string;
begin
  Result:= IntToStr(ACmd);

  if (ACmd>=cmdFirstPluginCommand) and
     (ACmd<=cmdLastPluginCommand) then
    with AppPluginsCommand[ACmd-cmdFirstPluginCommand] do
      Result:= ItemModule+','+ItemProc+IfThen(ItemProcParam<>'', ','+ItemProcParam);
end;

procedure DoOps_SaveKeyItem(K: TATKeymapItem; const path, ALexerName: string;
  ALexerSpecific: boolean);
var
  c: TJSONConfig;
  sl: TStringList;
  i: integer;
begin
  c:= TJSONConfig.Create(nil);
  sl:= TStringlist.create;
  try
    c.Formatted:= true;

    if ALexerSpecific then
      c.Filename:= GetAppKeymap_LexerSpecificConfig(ALexerName)
    else
      c.Filename:= GetAppPath(cFileOptionsKeymap);

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
      c.Filename:= GetAppKeymap_LexerSpecificConfig(ALexerName)
    else
      c.Filename:= GetAppPath(cFileOptionsKeymap);

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


(*
function GetActiveControl(Form: TWinControl): TWinControl;
var
  Ctl: TControl;
  i: integer;
begin
  Result:= nil;
  for i:= 0 to Form.ControlCount-1 do
  begin
    Ctl:= Form.Controls[i];
    if (Ctl is TWinControl) then
      if (Ctl as TWinControl).Focused then
        exit(Ctl as TWinControl);
    if Ctl is TPanel then
    begin
      Result:= GetActiveControl(Ctl as TPanel);
      if Assigned(Result) then exit;
    end;
    if Ctl is TATPanelSimple then
    begin
      Result:= GetActiveControl(Ctl as TATPanelSimple);
      if Assigned(Result) then exit;
    end;
  end;
end;
*)

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
  i: integer;
begin
  with AppManager do
    for i:= 0 to LexerCount-1 do
      if AlsoDisabled or not Lexers[i].Internal then
        L.Add(Lexers[i].LexerName);

  with AppManagerLite do
    for i:= 0 to LexerCount-1 do
      L.Add(Lexers[i].LexerName+msgLiteLexerSuffix);
end;

procedure DoLexerExportFromLibToFile(an: TecSyntAnalyzer);
begin
  if Assigned(an) then
    an.SaveToFile(GetAppLexerFilename(an.LexerName));
end;

procedure CommandPlugins_AssignItem(var Dst, Src: TAppPluginCmd);
begin
  Dst.ItemModule:= Src.ItemModule;
  Dst.ItemProc:= Src.ItemProc;
  Dst.ItemProcParam:= Src.ItemProcParam;
  Dst.ItemCaption:= Src.ItemCaption;
  Dst.ItemLexers:= Src.ItemLexers;
  Dst.ItemInMenu:= Src.ItemInMenu;
  Dst.ItemFromApi:= Src.ItemFromApi;
end;

procedure CommandPlugins_DeleteItem(AIndex: integer);
var
  i: integer;
begin
  if (AIndex>=Low(AppPluginsCommand)) and (AIndex<=High(AppPluginsCommand)) then
  begin
    for i:= AIndex to High(AppPluginsCommand)-1 do
      CommandPlugins_AssignItem(AppPluginsCommand[i], AppPluginsCommand[i+1])
  end;
  with AppPluginsCommand[High(AppPluginsCommand)] do
  begin
    ItemModule:= '';
    ItemProc:= '';
    ItemProcParam:= '';
    ItemFromApi:= false;
    ItemInMenu:= '';
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

  for i:= Low(AppPluginsCommand) to High(AppPluginsCommand) do
    with AppPluginsCommand[i] do
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
  for N:= High(AppPluginsCommand) downto Low(AppPluginsCommand) do
    with AppPluginsCommand[N] do
      if (ItemModule=SModule) and (ItemProc=SProc) and (ItemProcParam<>'') then
        CommandPlugins_DeleteItem(N);

  //find index of first free item
  N:= Low(AppPluginsCommand);
  repeat
    if AppPluginsCommand[N].ItemModule='' then break;
    Inc(N);
    if N>High(AppPluginsCommand) then exit;
  until false;

  //add items for SParams
  repeat
    SItem:= SGetItem(SParams, cSepParams);
    if SItem='' then break;

    SItemCaption:= SGetItem(SItem, cSepNameParam);
    SItemParam:= SItem;

    with AppPluginsCommand[N] do
    begin
      ItemModule:= SModule;
      ItemProc:= SProc;
      ItemProcParam:= SItemParam;
      ItemCaption:= SItemCaption;
      ItemFromApi:= true;
    end;
    Inc(N);
    if N>High(AppPluginsCommand) then exit;
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
    Result:= GetAppPath(cDirDataLexers)+DirectorySeparator+ALexName+AExt;
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
    //Py API: no need lexer-specific
    DoOps_SaveKeyItem(AppKeymap[NIndex], SCmd, '', false);
  end;
  Result:= true;
end;


function AppKeymapCheckDuplicateForCommand(
  AKeymapItem: TATKeymapItem;
  const ALexerName: string;
  AOverwriteAndSave: boolean): integer;
var
  item: TATKeymapItem;
  itemKeyPtr: ^TATKeyArray;
  StrId: string;
  i: integer;
begin
  Result:= 0;

  for i:= 0 to AppKeymap.Count-1 do
  begin
    item:= AppKeymap.Items[i];
    if item.Command=AKeymapItem.Command then Continue;

    if KeyArraysEqualNotEmpty(AKeymapItem.Keys1, item.Keys1) or
       KeyArraysEqualNotEmpty(AKeymapItem.Keys2, item.Keys1) then itemKeyPtr:= @item.Keys1 else
    if KeyArraysEqualNotEmpty(AKeymapItem.Keys1, item.Keys2) or
       KeyArraysEqualNotEmpty(AKeymapItem.Keys2, item.Keys2) then itemKeyPtr:= @item.Keys2 else
    Continue;

    if AOverwriteAndSave then
    begin
      //clear in memory
      KeyArrayClear(itemKeyPtr^);

      StrId:= DoOps_CommandCode_To_HotkeyStringId(item.Command);

      //save to: user.json
      DoOps_SaveKeyItem(item, StrId, '', false);
      //save to: lexer*.json
      if ALexerName<>'' then
        DoOps_SaveKeyItem(item, StrId, ALexerName, true);
    end
    else
      exit(item.Command);
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

    ncmd:= DoOps_HotkeyStringId_To_CommandCode(UndoItem.StrId);
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

procedure MsgStdout(const Str: string; AllowMsgBox: boolean = false);
begin
  {$ifdef windows}
  if AllowMsgBox then
    MsgBox(Str, MB_OK+MB_ICONINFORMATION);
  {$else}
  System.Writeln(Str);
  {$endif}
end;


function AppEncodingShortnameToFullname(const S: string): string;
var
  i: integer;
begin
  Result:= '';
  if S='' then exit;
  for i:= Low(AppEncodings) to High(AppEncodings) do
    with AppEncodings[i] do
      if SameText(S, ShortName) then
        Exit(Name);
end;

function AppEncodingFullnameToShortname(const S: string): string;
var
  i: integer;
begin
  Result:= '';
  if S='' then exit;
  for i:= Low(AppEncodings) to High(AppEncodings) do
    with AppEncodings[i] do
      if SameText(S, Name) then
        Exit(LowerCase(ShortName));
end;

function AppEncodingListAsString: string;
var
  i: integer;
begin
  Result:= '';
  for i:= Low(AppEncodings) to High(AppEncodings) do
    with AppEncodings[i] do
      if ShortName<>'' then
        Result:= Result + LowerCase(ShortName) + #10;
end;

procedure UpdateFormOnTop(F: TForm);
begin
  if UiOps.ShowFormsOnTop then
    F.FormStyle:= fsSystemStayOnTop
  else
    F.FormStyle:= fsNormal;
end;

function AppEncodingOem: string;
begin
  {$ifdef windows}
  case Windows.GetOEMCP of
    437: Result:= 'CP437';
    850: Result:= 'CP850';
    852: Result:= 'CP852';
    866: Result:= 'CP866';
    874: Result:= 'CP874';
    932: Result:= 'CP932';
    936: Result:= 'CP936';
    949: Result:= 'CP949';
    950: Result:= 'CP950';
    else Result:= 'CP437';
  end;
  {$else}
  Result:= 'CP437';
  {$endif}
end;


procedure DoStatusbarTextByTag(AStatus: TATStatus; ATag: PtrInt; const AText: string);
var
  NIndex: integer;
begin
  NIndex:= AStatus.FindPanel(ATag);
  if NIndex>=0 then
    AStatus.Captions[NIndex]:= AText;
end;

function IsFileTooBigForOpening(const AFilename: string): boolean;
begin
  Result:= (AFilename<>'') and (FileSize(AFileName) div (1024*1024) >= UiOps.MaxFileSizeToOpen);
end;

function IsFileTooBigForLexer(const AFilename: string): boolean;
begin
  Result:= (AFilename<>'') and (FileSize(AFilename) div (1024*1024) >= UiOps.MaxFileSizeForLexer);
end;


procedure DoLexerDetect(const AFilename: string;
  out Lexer: TecSyntAnalyzer;
  out LexerLite: TATLiteLexer;
  out LexerName: string);
begin
  LexerName:= '';
  Lexer:= nil;
  LexerLite:= nil;

  if IsFileTooBigForLexer(AFilename) then
  begin
    LexerLite:= AppManagerLite.FindLexerByFilename(AFilename);
  end
  else
  begin
    Lexer:= DoLexerFindByFilename(AFilename);
    if Lexer=nil then
      LexerLite:= AppManagerLite.FindLexerByFilename(AFilename);
  end;

  if Assigned(Lexer) then
    LexerName:= Lexer.LexerName
  else
  if Assigned(LexerLite) then
    LexerName:= LexerLite.LexerName+msgLiteLexerSuffix;
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

  FillChar(AppSidePanels, SizeOf(AppSidePanels), 0);
  FillChar(AppBottomPanels, SizeOf(AppBottomPanels), 0);

  AppShortcutEscape:= ShortCut(vk_escape, []);
  Mouse.DragImmediate:= false;

finalization
  FreeAndNil(AppKeymap);
  FreeAndNil(AppBookmarkImagelist);

end.

