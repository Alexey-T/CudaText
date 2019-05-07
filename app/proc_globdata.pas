(*
This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at http://mozilla.org/MPL/2.0/.

Copyright (c) Alexey Torgashin
*)
unit proc_globdata;

{$mode objfpc}{$H+}
{$IOChecks off}

interface

uses
  {$ifdef windows}
  Windows,
  {$endif}
  Classes, SysUtils, Forms, Controls, Menus,
  Dialogs, Graphics, ExtCtrls, ComCtrls,
  Math,
  InterfaceBase,
  LclProc, LclType, LazFileUtils,
  LazUTF8,
  LConvEncoding,
  FileUtil, IniFiles, StrUtils,
  Process,
  ATSynEdit,
  ATSynEdit_Keymap,
  ATSynEdit_Keymap_Init,
  ATSynEdit_Adapter_litelexer,
  ATSynEdit_ScrollBar,
  ATStringProc,
  ATButtons,
  ATFlatToolbar,
  ATListbox,
  ATStatusBar,
  ATScrollBar,
  at__jsonconf,
  proc_cmd,
  proc_msg,
  proc_str,
  proc_keymap_undolist,
  ec_LexerList,
  ec_proc_lexer,
  ec_SyntAnal;

var
  //ATSynEdit has range for bookmarks 0..63, 0=none
  AppBookmarkSetup: array[1..63] of
    record
      ImageIndex: integer;
      Color: TColor;
    end;
var
  AppBookmarkImagelist: TImageList = nil;
  AppFolderOfLastInstalledAddon: string = '';

type
  TAppKeyValue = class
    Key: string;
    Value: string;
  end;

type
  { TAppKeyValues }

  TAppKeyValues = class(TList)
  public
    procedure Add(const AKey, AValue: string);
    function GetValue(const AKey, ADefValue: string): string;
  end;

var
  AppConfig_Detect: TAppKeyValues;
  AppConfig_DetectLine: TAppKeyValues;
  AppConfig_PGroups: TAppKeyValues;

const
  AppExtensionThemeUi = '.cuda-theme-ui';
  AppExtensionThemeSyntax = '.cuda-theme-syntax';

const
  AppDefaultMonospacedFont =
    {$ifdef windows} 'Consolas' {$endif}
    {$ifdef linux} 'Courier New' {$endif}
    {$ifdef freebsd} 'Courier New' {$endif}
    {$ifdef solaris} 'Courier New' {$endif}
    {$ifdef darwin} 'Monaco' {$endif}
    ;

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
    cFileOptionsKeymap,
    cFileOptionsHistoryFiles
    );

type
  TUiOps = record
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
    LexerPostponeUntilShown: boolean;

    ToolBarTheme: string;

    SidebarShow: boolean;
    SidebarOnRight: boolean;
    SidebarTheme: string;
    PictureTypes: string;
    MaxFileSizeToOpen: integer;
    MaxFileSizeForLexer: integer;
    MaxLinesForTree: integer;

    AutocompleteHtml: boolean;
    AutocompleteCss: boolean;
    AutocompleteHtml_Lexers: string;
    AutocompleteCss_Lexers: string;

    AutocompleteAutoshowCharCount: integer;
    AutocompleteTriggerChars: string;
    AutocompleteCommitChars: string;
    AutocompleteCloseChars: string;
    AutocompleteAddOpeningBracket: boolean;
    AutocompleteUpDownAtEdge: integer;
    AutoCloseBrackets: string;

    ListboxCentered: boolean;
    ListboxSizeX: integer;
    ListboxSizeY: integer;
    ListboxCompleteSizeX: integer;
    ListboxCompleteSizeY: integer;
    ListboxFuzzySearch: boolean;
    ListboxHotkeyFontSizeDelta: integer;

    TabAnimation: boolean;
    TabsDisabled: boolean;
    TabVarWidth: boolean;
    TabMultiline: boolean;
    TabAngled: boolean;
    TabFlat: boolean;
    TabWidth: integer;
    TabWidthMin: integer;
    TabWidthMax: integer;
    TabHeight: integer;
    TabHeightInner: integer;
    TabSpacer: integer;
    TabPosition: integer;
    TabColorFull: boolean;
    TabShowX: integer;
    TabShowXSize: integer;
    TabShowPlus: boolean;
    TabDblClickClose: boolean;
    TabNumbers: boolean;
    TabNewNearCurrent: boolean;
    TabRecentOnClose: boolean;
    TabButtonLayout: string;
    TabPreviewFontStyle: string;

    MaxHistoryEdits: integer;
    MaxHistoryMenu: integer;
    MaxHistoryFiles: integer;

    FindSuggestSel: boolean;
    FindSuggestWord: boolean;
    FindSuggestInSelection: boolean;
    FindSelCase: integer;
    FindShow_FindFirst: boolean;
    FindShow_MarkALl: boolean;
    FindShow_SelectAll: boolean;
    FindShow_Extract: boolean;
    FindIndentVert: integer;
    FindIndentHorz: integer;
    FindMultiLineScale: double;
    FindSeparateForm: boolean;

    EscapeClose: boolean;
    EscapeCloseConsole: boolean;
    ConsoleCompact: boolean;
    ConsoleWordWrap: boolean;
    InputHeight: integer;
    InitialDir: string;

    ExportHtmlNumbers: boolean;
    ExportHtmlFontName: string;
    ExportHtmlFontSize: integer;

    TreeTheme: string;
    TreeAutoSync: boolean;
    TreeTimeFill: integer;
    TreeTimeCaret: integer;
    TreeShowIcons: boolean;
    TreeShowTooltips: boolean;
    TreeFilterLayout: integer;

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

    ScrollbarWidth: integer;
    ScrollbarBorderSize: integer;
    ScrollbarArrowSize: integer;

    ProgressbarWidth: integer;
    ProgressbarHeightSmall: integer;

    ShowMenubar: boolean;
    ShowStatusbar: boolean;
    ShowToolbar: boolean;
    ShowActiveBorder: boolean;
    ShowSidebarCaptions: boolean;
    ShowTitlePath: boolean;
    Scale: integer;
    ScaleFont: integer;

    ReopenSession: boolean;
    AutoSaveSession: boolean;
    ShowFormsOnTop: boolean;
    ShowMenuDialogsWithBorder: boolean;
    UndoPersistent: string;

    FloatGroupsInTaskbar: boolean;
    OneInstance: boolean;
    NotifEnabled: boolean;
    NotifTimeSec: integer;
    NonTextFiles: integer; //0: prompt, 1: open, 2: don't open
    NonTextFilesBufferKb: integer;
    ReloadUnsavedConfirm: boolean;
    ReloadFollowTail: boolean;
    FullScreen: string;
    MouseGotoDefinition: string;
    LogDebug: boolean;
    LogConsole: boolean;

    HotkeyFindDialog,
    HotkeyReplaceDialog,
    HotkeyFindFirst,
    HotkeyFindNext,
    HotkeyFindPrev,
    HotkeyReplaceAndFindNext,
    HotkeyReplaceNoFindNext,
    HotkeyReplaceAll,
    HotkeyCountAll,
    HotkeyExtractAll,
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
  str_FontName_i = 'font_name_i'+cOptionSystemSuffix;
  str_FontName_b = 'font_name_b'+cOptionSystemSuffix;
  str_FontName_bi = 'font_name_bi'+cOptionSystemSuffix;
  str_FontSize = 'font_size'+cOptionSystemSuffix;
  str_FontSize_i = 'font_size_i'+cOptionSystemSuffix;
  str_FontSize_b = 'font_size_b'+cOptionSystemSuffix;
  str_FontSize_bi = 'font_size_bi'+cOptionSystemSuffix;
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
    OpFontName_i: string;
    OpFontName_b: string;
    OpFontName_bi: string;
    OpFontSize: integer;
    OpFontSize_i: integer;
    OpFontSize_b: integer;
    OpFontSize_bi: integer;
    OpFontQuality: TFontQuality;
    OpFontLigatures: boolean;

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
    OpRulerMarkCaret: integer;

    OpMinimapShow: boolean;
    OpMinimapShowSelAlways: boolean;
    OpMinimapShowSelBorder: boolean;
    OpMinimapCharWidth: integer;
    OpMinimapAtLeft: boolean;
    OpMinimapTooltipShow: boolean;
    OpMinimapTooltipLineCount: integer;
    OpMinimapTooltipWidth: integer;
    OpMicromapShow: boolean;
    OpMicromapWidth: integer;
    OpMicromapWidthSmall: integer;
    OpMarginFixed: integer;
    OpMarginString: string;

    OpStaplesStyle: integer;
    OpStaplesProps: string;

    //unprinted
    OpUnprintedShow: boolean;
    OpUnprintedContent: string;

    OpUnprintedReplaceSpec: boolean;
    OpUnprintedReplaceToCode: string;

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
    OpCaretViewNormal: string;
    OpCaretViewOverwrite: string;
    OpCaretViewReadonly: string;
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
    OpShowMouseSelFrame: boolean;
    OpCopyLineIfNoSel: boolean;
    OpCutLineIfNoSel: boolean;
    OpCopyColumnAlignedBySpaces: boolean;
    OpSavingTrimSpaces: boolean;
    OpSavingTrimFinalEmptyLines: boolean;
    OpSavingForceFinalEol: boolean;
    OpShowHintOnVertScroll: boolean;
    OpSmoothScroll: boolean;
    OpCenteringWidth: integer;
    OpCenteringForDistractionFree: integer;
    OpHideHorizScrollbar: boolean;
    OpLexerDynamicHiliteEnabled: boolean;
    OpLexerDynamicHiliteMaxLines: integer;
    OpLexerLineSeparators: boolean;
    OpZebra: integer;

    OpWordChars: UnicodeString;
    OpHexChars: UnicodeString;
    OpFoldStyle: integer;
    OpFoldTooltipShow: boolean;

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
function GetAppUndoFilename(const fn: string; IsRedo: boolean): string;

function EscapeLexerFilename(const ALexName: string): string;
function GetAppLexerFilename(const ALexName: string): string;
function GetAppLexerMapFilename(const ALexName: string): string;
function GetAppLexerOpsFilename(const ALexName: string): string;
function GetAppLexerAcpFilename(const ALexName: string): string;
function GetAppLexerSpecificConfig(AName: string): string;
function GetAppLexerPropInCommentsSection(const ALexerName, AKey: string): string;
function IsLexerStyleCommentOrString(const ALexer, AStyle: string): boolean;

function MsgBox(const Str: string; Flags: Longint): integer;
procedure MsgBadConfig(const fn: string);
procedure MsgStdout(const Str: string; AllowMsgBox: boolean = false);

function AppScale(AValue: integer): integer;
function AppScaleFont(AValue: integer): integer;
procedure AppScaleToolbar(C: TATFlatToolbar);
procedure AppScaleScrollbar(C: TATScroll);

function GetListboxItemHeight(const AFontName: string; AFontSize: integer): integer;
function FixFontMonospaced(const AName: string): string;
procedure FixFormPositionToDesktop(F: TForm);
procedure FixRectPositionToDesktop(var F: TRect);

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

function DoLexerDetectByFilenameOrContent(const AFilename: string): TecSyntAnalyzer;
procedure DoLexerEnum(L: TStringList; AlsoDisabled: boolean = false);

function DoReadOneStringFromFile(const AFilename: string): string;
function DoReadContentFromFile(const AFilename: string): string;
procedure DoWriteStringToFile(const AFilename, AText: string);

function SCollapseHomeDirInFilename(const AFilename: string): string;
function SExpandHomeDirInFilename(const AFilename: string): string;

var
  AppManager: TecLexerList = nil;
  AppManagerLite: TATLiteLexers = nil;
  AppKeymap: TATKeymap = nil;
  AppKeymapInitial: TATKeymap = nil;
  AppShortcutEscape: TShortcut = 0;
  AppShortcutShiftTab: TShortcut = 0;
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

type
  TAppEncodingRecord = record
    Sub,
    Name,
    ShortName: string;
  end;

const
  AppEncodings: array[0..29] of TAppEncodingRecord = (
    (Sub: ''; Name: cEncNameUtf8_NoBom; ShortName: 'utf8'),
    (Sub: ''; Name: cEncNameUtf8_WithBom; ShortName: 'utf8_bom'),
    (Sub: ''; Name: cEncNameUtf16LE_NoBom; ShortName: 'utf16le'),
    (Sub: ''; Name: cEncNameUtf16LE_WithBom; ShortName: 'utf16le_bom'),
    (Sub: ''; Name: cEncNameUtf16BE_NoBom; ShortName: 'utf16be'),
    (Sub: ''; Name: cEncNameUtf16BE_WithBom; ShortName: 'utf16be_bom'),
    (Sub: ''; Name: '-'; ShortName: ''),
    (Sub: 'eu'; Name: EncodingCP1250; ShortName: EncodingCP1250),
    (Sub: 'eu'; Name: EncodingCP1251; ShortName: EncodingCP1251),
    (Sub: 'eu'; Name: EncodingCP1252; ShortName: EncodingCP1252),
    (Sub: 'eu'; Name: EncodingCP1253; ShortName: EncodingCP1253),
    (Sub: 'eu'; Name: EncodingCP1257; ShortName: EncodingCP1257),
    (Sub: 'eu'; Name: '-'; ShortName: ''),
    (Sub: 'eu'; Name: EncodingCP437; ShortName: EncodingCP437),
    (Sub: 'eu'; Name: EncodingCP850; ShortName: EncodingCP850),
    (Sub: 'eu'; Name: EncodingCP852; ShortName: EncodingCP852),
    (Sub: 'eu'; Name: EncodingCP866; ShortName: EncodingCP866),
    (Sub: 'eu'; Name: '-'; ShortName: ''),
    (Sub: 'eu'; Name: EncodingCPISO1; ShortName: EncodingCPISO1),
    (Sub: 'eu'; Name: EncodingCPISO2; ShortName: EncodingCPISO2),
    (Sub: 'eu'; Name: EncodingCPMac; ShortName: 'mac'),
    (Sub: 'mi'; Name: EncodingCP1254; ShortName: EncodingCP1254),
    (Sub: 'mi'; Name: EncodingCP1255; ShortName: EncodingCP1255),
    (Sub: 'mi'; Name: EncodingCP1256; ShortName: EncodingCP1256),
    (Sub: 'as'; Name: EncodingCP874; ShortName: EncodingCP874),
    (Sub: 'as'; Name: EncodingCP932; ShortName: EncodingCP932),
    (Sub: 'as'; Name: EncodingCP936; ShortName: EncodingCP936),
    (Sub: 'as'; Name: EncodingCP949; ShortName: EncodingCP949),
    (Sub: 'as'; Name: EncodingCP950; ShortName: EncodingCP950),
    (Sub: 'as'; Name: EncodingCP1258; ShortName: EncodingCP1258)
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
    cEventOnMouseStop,
    cEventOnClick,
    cEventOnClickDbl,
    cEventOnClickGutter,
    cEventOnClickGap,
    cEventOnState,
    cEventOnFocus,
    cEventOnStart,
    cEventOnOpen,
    cEventOnOpenBefore,
    cEventOnOpenNone,
    cEventOnClose,
    cEventOnCloseBefore,
    cEventOnSaveAfter,
    cEventOnSaveBefore,
    cEventOnSaveNaming,
    cEventOnLexer,
    cEventOnLexerParsed,
    cEventOnComplete,
    cEventOnGotoDef,
    cEventOnGotoEnter,
    cEventOnFuncHint,
    cEventOnTabSwitch,
    cEventOnTabChange,
    cEventOnTabMove,
    cEventOnPaste,
    cEventOnConsoleNav,
    cEventOnOutputNav,
    cEventOnSnippet,
    cEventOnMacro,
    cEventOnExit
    );
  TAppPyEvents = set of TAppPyEvent;
  TAppPyEventsPrior = array[TAppPyEvent] of byte;
    //0: default, 1,2...: higher priority
  TAppPyEventsLazy = array[TAppPyEvent] of boolean;

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
    'on_mouse_stop',
    'on_click',
    'on_click_dbl',
    'on_click_gutter',
    'on_click_gap',
    'on_state',
    'on_focus',
    'on_start',
    'on_open',
    'on_open_pre',
    'on_open_none',
    'on_close',
    'on_close_pre',
    'on_save',
    'on_save_pre',
    'on_save_naming',
    'on_lexer',
    'on_lexer_parsed',
    'on_complete',
    'on_goto_def',
    'on_goto_enter',
    'on_func_hint',
    'on_tab_switch',
    'on_tab_change',
    'on_tab_move',
    'on_paste',
    'on_console_nav',
    'on_output_nav',
    'on_snippet',
    'on_macro',
    'on_exit'
    );

type
  TAppCommand = class
  public
    ItemModule: string;
    ItemProc: string;
    ItemProcParam: string;
    ItemCaption: string;
    ItemLexers: string;
    ItemInMenu: string;
    ItemFromApi: boolean;
  end;

type
  TAppEvent = class
    ItemModule: string;
    ItemLexers: string;
    ItemEvents: TAppPyEvents;
    ItemEventsPrior: TAppPyEventsPrior;
    ItemEventsLazy: TAppPyEventsLazy;
    ItemKeys: string;
  end;

type
  TAppSidePanel = class
    ItemCaption: string;
    ItemControl: TCustomControl;
    ItemModule: string;
    ItemMethod: string;
  end;

type
  TAppTreeHelper = class
    ItemModule: string;
    ItemProc: string;
    ItemLexers: string;
  end;

var
  AppCommandList: TList;
  AppEventList: TList;
  AppSidePanels: TList;
  AppBottomPanels: TList;
  AppTreeHelpers: TList;

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

function AppEncodingShortnameToFullname(const S: string): string;
function AppEncodingFullnameToShortname(const S: string): string;
function AppEncodingListAsString: string;

procedure UpdateFormOnTop(F: TForm);
procedure DoStatusbarTextByTag(AStatus: TATStatus; ATag: PtrInt; const AText: string);
procedure DoStatusbarHintByTag(AStatus: TATStatus; ATag: PtrInt; const AText: string);
function IsFileTooBigForOpening(const AFilename: string): boolean;
function IsFileTooBigForLexer(const AFilename: string): boolean;
procedure DoLexerDetect(const AFilename: string;
  out Lexer: TecSyntAnalyzer;
  out LexerLite: TATLiteLexer;
  out LexerName: string);

type
  TATSynCaretShape = (
    cCaretShapeFull,
    cCaretShapeVertPixels1,
    cCaretShapeVertPixels2,
    cCaretShapeVertPixels3,
    cCaretShapeVertPixels4,
    cCaretShapeVertPercents10,
    cCaretShapeVertPercents15,
    cCaretShapeVertPercents20,
    cCaretShapeVertPercents25,
    cCaretShapeVertPercents30,
    cCaretShapeVertPercents35,
    cCaretShapeVertPercents40,
    cCaretShapeVertPercents50,
    cCaretShapeHorzPixels1,
    cCaretShapeHorzPixels2,
    cCaretShapeHorzPixels3,
    cCaretShapeHorzPixels4,
    cCaretShapeHorzPixels5,
    cCaretShapeHorzPercents10,
    cCaretShapeHorzPercents15,
    cCaretShapeHorzPercents20,
    cCaretShapeHorzPercents25,
    cCaretShapeHorzPercents30,
    cCaretShapeHorzPercents35,
    cCaretShapeHorzPercents40,
    cCaretShapeHorzPercents50,
    cCaretShapeFrameFull
    );


implementation

function MsgBox(const Str: string; Flags: Longint): integer;
begin
  Result:= Application.MessageBox(PChar(Str), PChar(msgTitle), Flags);
end;

procedure MsgBadConfig(const fn: string);
begin
  MsgBox(msgCannotReadConf+#10+fn, MB_OK+MB_ICONERROR);
end;

function InitPyLibraryPath: string;
{$ifdef darwin}
var
  N: integer;
  S: string;
{$endif}
begin
  Result:= '';

  {$ifdef windows}
  exit('python35.dll');
  {$endif}

  {$ifdef linux}
  exit('libpython3.so');
  {$endif}

  {$ifdef freebsd}
  exit('libpython3.6m.so');
  {$endif}

  {$ifdef darwin}
  for N:= 4 to 9 do
  begin
    S:= Format('/Library/Frameworks/Python.framework/Versions/3.%d/lib/libpython3.%d.dylib',
      [N, N]);
    if FileExists(S) then exit(S);
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
  {$ifdef solaris} '' {$endif}
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
    cFileOptionsKeymap:
      begin
        Result:= GetAppPath(cDirSettings)+DirectorySeparator+'keys.json';
      end;
    cFileOptionsHistoryFiles:
      begin
        Result:= GetAppPath(cDirSettings)+DirectorySeparator+'history files.json';
      end;
  end;
end;

//from https://github.com/graemeg/freepascal/blob/master/rtl/unix/sysutils.pp
function _GetHomeDir: String;
begin
  Result:=GetEnvironmentVariable('HOME');
  If (Result<>'') then
    Result:=IncludeTrailingPathDelimiter(Result);
end;

function _XdgConfigHome: String;
begin
  Result:=GetEnvironmentVariable('XDG_CONFIG_HOME');
  if (Result='') then
    Result:= _GetHomeDir + '.config/'
  else
    Result:= IncludeTrailingPathDelimiter(Result);
end;


function SCollapseHomeDirInFilename(const AFilename: string): string;
var
  S: string;
begin
  Result:= AFilename;
  S:= _GetHomeDir;
  if SBeginsWith(Result, S) then
    Result:= '~'+DirectorySeparator+Copy(Result, Length(S)+1, MaxInt);
end;

function SExpandHomeDirInFilename(const AFilename: string): string;
begin
  Result:= AFilename;
  if SBeginsWith(Result, '~'+DirectorySeparator) then
    Result:= _GetHomeDir+Copy(Result, 3, MaxInt);
end;


procedure InitDirs;
var
  S: string;
begin
  OpDirExe:= ExtractFileDir(ParamStrUTF8(0));
  OpDirPrecopy:= GetDirPrecopy;

  //portable folder of app
  if DirectoryExistsUTF8(
      OpDirExe+
      DirectorySeparator+'data'+
      DirectorySeparator+'lexlib') then
    OpDirLocal:= OpDirExe
  else
  //not portable folder, use Home dir
  begin
    {$ifdef windows}
    OpDirLocal:= GetEnvironmentVariableUTF8('appdata')+'\CudaText';
    {$else}
      {$ifdef darwin}
      OpDirLocal:= _GetHomeDir+'Library/Application Support/CudaText';
      {$else}
      OpDirLocal:= _XdgConfigHome+'cudatext';
      {$endif}
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
    OpFontName:= AppDefaultMonospacedFont;
    OpFontName_i:= '';
    OpFontName_b:= '';
    OpFontName_bi:= '';

    OpFontSize:= 9; //now Win, Carbon and Cocoa use the same font size
    OpFontSize_i:= OpFontSize;
    OpFontSize_b:= OpFontSize;
    OpFontSize_bi:= OpFontSize;

    OpFontQuality:= fqDefault;
    OpFontLigatures:= true;

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
    OpRulerMarkCaret:= 1;

    OpMinimapShow:= false;
    OpMinimapShowSelAlways:= false;
    OpMinimapShowSelBorder:= true;
    OpMinimapCharWidth:= 0;
    OpMinimapAtLeft:= false;
    OpMinimapTooltipShow:= false;
    OpMinimapTooltipLineCount:= 6;
    OpMinimapTooltipWidth:= 60;

    OpMicromapShow:= false;
    OpMicromapWidth:= 12;
    OpMicromapWidthSmall:= 4;

    OpMarginFixed:= 2000; //hide margin
    OpMarginString:= '';

    OpStaplesStyle:= 1; //Ord(cLineStyleSolid)
    OpStaplesProps:= '-1,40,1,1';

    OpUnprintedShow:= false;
    OpUnprintedContent:= 'se';
    OpUnprintedReplaceSpec:= false;
    OpUnprintedReplaceToCode:= 'A4';

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

    OpCaretBlinkTime:= cInitCaretBlinkTime;
    OpCaretBlinkEn:= true;
    OpCaretViewNormal:= '2,-100';
    OpCaretViewOverwrite:= '-100,-100';
    OpCaretViewReadonly:= '-100,2';
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
    OpShowMouseSelFrame:= true;
    OpCopyLineIfNoSel:= true;
    OpCutLineIfNoSel:= false;
    OpCopyColumnAlignedBySpaces:= true;
    OpSavingTrimSpaces:= false;
    OpSavingTrimFinalEmptyLines:= false;
    OpSavingForceFinalEol:= false;
    OpShowHintOnVertScroll:= false;
    OpSmoothScroll:= true;
    OpCenteringWidth:= 0;
    OpCenteringForDistractionFree:= 100;
    OpHideHorizScrollbar:= false;
    OpLexerDynamicHiliteEnabled:= false;
    OpLexerDynamicHiliteMaxLines:= 2000;
    OpLexerLineSeparators:= false;
    OpZebra:= 0;

    OpWordChars:= '';
    OpHexChars:= '';
    OpFoldStyle:= 1;
    OpFoldTooltipShow:= false;

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
    VarFontName:= 'default';
    VarFontSize:= 9;

    OutputFontName:= VarFontName;
    OutputFontSize:= VarFontSize;

    DoubleBuffered:= IsDoubleBufferedNeeded;

    LexerThemes:= true;
    LexerMenuGrouped:= true;
    LexerDelayedParsingPause:= 400;
    LexerDelayedParsingSize:= 100*1000;
    LexerPostponeUntilShown:= true;

    SidebarShow:= true;
    SidebarOnRight:= false;
    SidebarTheme:= 'common_20x20';
    TreeTheme:= 'default_16x16';
    ToolBarTheme:= 'default_24x24';

    AutocompleteHtml_Lexers:= '.*HTML.*|PHP';
    AutocompleteCss_Lexers:= 'CSS';

    PyLibrary:= InitPyLibraryPath;
    PictureTypes:= 'bmp,png,jpg,jpeg,gif,ico';

    MaxFileSizeToOpen:= 1000;
    MaxFileSizeForLexer:= 2;
    MaxLinesForTree:= 8000;

    AutocompleteCss:= true;
    AutocompleteHtml:= true;
    AutocompleteAutoshowCharCount:= 0;
    AutocompleteTriggerChars:= '';
    AutocompleteCommitChars:= ' ,;/\''"';
    AutocompleteCloseChars:= '<>()[]{}=';
    AutocompleteAddOpeningBracket:= true;
    AutocompleteUpDownAtEdge:= 1; //cudWrap
    AutoCloseBrackets:= '([{';

    ListboxCentered:= false;
    ListboxSizeX:= 450;
    ListboxSizeY:= 300;
    ListboxCompleteSizeX:= 550;
    ListboxCompleteSizeY:= 200;
    ListboxFuzzySearch:= true;
    ListboxHotkeyFontSizeDelta:= 0; //2 gives too small hotkey font on Lin/Win

    TabAnimation:= false;
    TabsDisabled:= false;
    TabVarWidth:= false;
    TabMultiline:= false;
    TabAngled:= {$ifdef darwin} false {$else} true {$endif};
    TabFlat:= false;
    TabWidth:= 170;
    TabWidthMin:= 40;
    TabWidthMax:= 300;
    TabHeight:= 26;
    TabHeightInner:= TabHeight-1;
    TabSpacer:= 2;
    TabPosition:= 0;
    TabColorFull:= false;
    TabShowX:= 1; //show all
    TabShowXSize:= 14;
    TabShowPlus:= true;
    TabDblClickClose:= false;
    TabNumbers:= false;
    TabNewNearCurrent:= false;
    TabRecentOnClose:= false;
    TabButtonLayout:= '<>,v';
    TabPreviewFontStyle:= 'iu';

    MaxHistoryEdits:= 20;
    MaxHistoryMenu:= 10;
    MaxHistoryFiles:= 25;

    FindSuggestSel:= false;
    FindSuggestWord:= true;
    FindSuggestInSelection:= false;
    FindSelCase:= 2;
    FindShow_FindFirst:= true;
    FindShow_MarkALl:= true;
    FindShow_SelectAll:= true;
    FindShow_Extract:= true;
    FindIndentVert:= -5;
    FindIndentHorz:= 10;
    FindMultiLineScale:= 2.5;
    FindSeparateForm:= false;

    EscapeClose:= false;
    EscapeCloseConsole:= true;
    ConsoleCompact:= false;
    ConsoleWordWrap:= true;
    InputHeight:= 26;
    InitialDir:= '';

    ExportHtmlNumbers:= false;
    ExportHtmlFontSize:= 12;
    ExportHtmlFontName:= 'Courier New';

    TreeAutoSync:= true;
    TreeTimeFill:= 2000;
    TreeTimeCaret:= 300;
    TreeShowIcons:= true;
    TreeShowTooltips:= true;
    TreeFilterLayout:= 1;

    PyChangeSlow:= 2000;
    PyInitLog:= true;

    NewdocLexer:= '';
    NewdocEnc:= 'utf8';
    NewdocEnds:= 0;

    DefaultEncUtf8:= {$ifdef windows} false {$else} true {$endif};
    ViewerBinaryWidth:= 100;

    StatusNoSel:= '{_ln} {y}, {_col} {xx}';
    StatusSmallSel:= '{_ln} {y}, {_col} {xx}, {_sel}';
    StatusStreamSel:= '{_ln} {y}, {_col} {xx}, {sel} {_linesel}';
    StatusColSel:= '{sel}x{cols} {_sel}';
    StatusCarets:= '{carets} {_carets}, {sel} {_linesel}';

    StatusPanels:= 'caret,C,180|enc,C,125|ends,A,45|lexer,C,140|tabsize,A,75|selmode,A,15|msg,L,4000';
    StatusHeight:= TabHeight;
    StatusTime:= 5;
    StatusAltTime:= 7;

    ScrollbarWidth:= 14;
    ScrollbarBorderSize:= 0;
    ScrollbarArrowSize:= 3;

    ProgressbarWidth:= 50;
    ProgressbarHeightSmall:= 6;

    ShowMenubar:= true;
    ShowStatusbar:= true;
    ShowToolbar:= false;
    ShowActiveBorder:= true;
    ShowSidebarCaptions:= false;
    ShowTitlePath:= false;

    Scale:= 100;
    ScaleFont:= {$ifdef windows} 100 {$else} 0 {$endif};

    ReopenSession:= true;
    AutoSaveSession:= false;
    ShowFormsOnTop:= false;
    ShowMenuDialogsWithBorder:= {$ifdef LCLGTK2} true {$else} false {$endif};
    UndoPersistent:= '';

    FloatGroupsInTaskbar:= true;
    OneInstance:= false;
    NotifEnabled:= true;
    NotifTimeSec:= 2;
    NonTextFiles:= 0;
    NonTextFilesBufferKb:= 64;
    ReloadFollowTail:= true;
    ReloadUnsavedConfirm:= true;
    FullScreen:= 'tp';
    MouseGotoDefinition:= 'a';
    LogDebug:= false;
    LogConsole:= false;

    HotkeyFindDialog:= 'Ctrl+F';
    HotkeyReplaceDialog:= 'Ctrl+R';
    HotkeyFindFirst:= 'Alt+Enter';
    HotkeyFindNext:= '';
    HotkeyFindPrev:= 'Shift+Enter';
    HotkeyReplaceAndFindNext:= 'Alt+Z';
    HotkeyReplaceNoFindNext:= 'Ctrl+Alt+Z';
    HotkeyReplaceAll:= 'Alt+A';
    HotkeyCountAll:= 'Alt+O';
    HotkeyExtractAll:= 'Alt+Q';
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


function DoReadOneStringFromFile(const AFilename: string): string;
var
  f: TextFile;
begin
  Result:= '';
  Assign(f, AFilename);
  Reset(f);
  if IOResult=0 then
  begin
    if not Eof(f) then
      Readln(f, Result);
    CloseFile(f);
  end;
end;

function DoReadContentFromFile(const AFilename: string): string;
var
  L: TStringList;
begin
  Result:= '';
  if not FileExistsUTF8(AFilename) then exit;

  L:= TStringList.Create;
  try
    L.LoadFromFile(AFilename);
    L.TextLineBreakStyle:= tlbsLF;
    if L.Count>0 then
      Result:= L.Text;
  finally
    FreeAndNil(L);
  end;
end;

procedure DoWriteStringToFile(const AFilename, AText: string);
var
  f: TextFile;
begin
  Assign(f, AFilename);
  Rewrite(f);
  if IOResult=0 then
  begin
    Write(f, AText);
    CloseFile(f);
  end;
end;


function DoLexerDetectByFilenameOrContent(const AFilename: string): TecSyntAnalyzer;
var
  Item: TAppKeyValue;
  ext, sLine, res: string;
  i: integer;
begin
  //detect by filename
  res:= AppConfig_Detect.GetValue(ExtractFileName(AFilename), '');
  if res<>'' then
    exit(AppManager.FindLexerByName(res));

  //detect by extention
  ext:= ExtractFileExt(AFilename);
  if ext<>'' then
  begin
    res:= AppConfig_Detect.GetValue('*'+ext, '');
    if res<>'' then
      exit(AppManager.FindLexerByName(res));
  end;

  //detect by first line
  if AppConfig_DetectLine.Count>0 then
  begin
    sLine:= DoReadOneStringFromFile(AFilename);
    if sLine<>'' then
    begin
      for i:= 0 to AppConfig_DetectLine.Count-1 do
      begin
        Item:= TAppKeyValue(AppConfig_DetectLine[i]);
        if SRegexMatchesString(sLine, Item.Key, true) then
          exit(AppManager.FindLexerByName(Item.Value));
      end;
    end;
  end;

  Result:= AppManager.FindLexerByFilename(AFilename);
end;

function DoOps_CommandCode_To_HotkeyStringId(ACmd: integer): string;
begin
  Result:= IntToStr(ACmd);

  if (ACmd>=cmdFirstPluginCommand) and
     (ACmd<=cmdLastPluginCommand) then
    with TAppCommand(AppCommandList[ACmd-cmdFirstPluginCommand]) do
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
    try
      c.Formatted:= true;
      if ALexerSpecific then
        c.Filename:= GetAppKeymap_LexerSpecificConfig(ALexerName)
      else
        c.Filename:= GetAppPath(cFileOptionsKeymap);
    except
      exit;
    end;

    c.SetValue(path+'/name', K.Name);

    sl.clear;
    for i:= 0 to High(TATKeyArray.Data) do
      if K.Keys1.Data[i]<>0 then
        sl.Add(ShortCutToText(K.Keys1.Data[i]));
    c.SetValue(path+'/s1', sl);

    sl.clear;
    for i:= 0 to High(TATKeyArray.Data) do
      if K.Keys2.Data[i]<>0 then
        sl.Add(ShortCutToText(K.Keys2.Data[i]));
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
    try
      c.Formatted:= true;
      if ALexerName<>'' then
        c.Filename:= GetAppKeymap_LexerSpecificConfig(ALexerName)
      else
        c.Filename:= GetAppPath(cFileOptionsKeymap);
    except
      exit;
    end;

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

{
procedure DoLexerSave(an: TecSyntAnalyzer);
begin
  if Assigned(an) then
    an.SaveToFile(GetAppLexerFilename(an.LexerName));
end;
}

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

  for i:= 0 to AppCommandList.Count-1 do
    with TAppCommand(AppCommandList[i]) do
      if (ItemModule=SModule) and (ItemProc=SProc) and (ItemProcParam=SProcParam) then
        exit(i);
end;


procedure CommandPlugins_UpdateSubcommands(AStr: string);
const
  cSepRoot=';';
  cSepParams=#10;
  cSepNameParam=#9;
var
  SModule, SProc, SParams, SItem, SItemParam, SItemCaption: string;
  CmdItem: TAppCommand;
  N: integer;
begin
  SModule:= SGetItem(AStr, cSepRoot);
  SProc:= SGetItem(AStr, cSepRoot);
  SParams:= AStr;

  //del items for module/method
  for N:= AppCommandList.Count-1 downto 0 do
    with TAppCommand(AppCommandList[N]) do
      if (ItemModule=SModule) and (ItemProc=SProc) and (ItemProcParam<>'') then
        AppCommandList.Delete(N);

  //add items for SParams
  repeat
    SItem:= SGetItem(SParams, cSepParams);
    if SItem='' then break;

    SItemCaption:= SGetItem(SItem, cSepNameParam);
    SItemParam:= SItem;

    CmdItem:= TAppCommand.Create;
    with CmdItem do
    begin
      ItemModule:= SModule;
      ItemProc:= SProc;
      ItemProcParam:= SItemParam;
      ItemCaption:= SItemCaption;
      ItemFromApi:= true;
    end;
    AppCommandList.Add(CmdItem);
  until false;
end;


function GetAppLangFilename: string;
begin
  if AppLangName='' then
    Result:= ''
  else
    Result:= GetAppPath(cDirDataLangs)+DirectorySeparator+AppLangName+'.ini';
end;

function EscapeLexerFilename(const ALexName: string): string;
begin
  Result:= ALexName;
  if Result<>'' then
  begin
    Result:= StringReplace(Result, ':', '_', [rfReplaceAll]);
    Result:= StringReplace(Result, '/', '_', [rfReplaceAll]);
    Result:= StringReplace(Result, '\', '_', [rfReplaceAll]);
    Result:= StringReplace(Result, '*', '_', [rfReplaceAll]);
  end;
end;

function GetLexerFilenameWithExt(ALexName, AExt: string): string;
begin
  if ALexName<>'' then
    Result:= GetAppPath(cDirDataLexers)+DirectorySeparator+EscapeLexerFilename(ALexName)+AExt
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

function GetAppLexerOpsFilename(const ALexName: string): string;
begin
  Result:= GetAppPath(cDirSettings)+DirectorySeparator+EscapeLexerFilename(ALexName)+'.cuda-lexops';
end;

function GetAppLexerAcpFilename(const ALexName: string): string;
begin
  Result:= GetAppPath(cDirDataAutocomplete)+DirectorySeparator+EscapeLexerFilename(ALexName)+'.acp';
end;

function GetAppUndoFilename(const fn: string; IsRedo: boolean): string;
const
  Ext: array[boolean] of string = ('.undo', '.redo');
begin
  Result:= ExtractFileDir(fn)+DirectorySeparator+
    '.cudatext'+DirectorySeparator+
    ExtractFileName(fn)+Ext[IsRedo];
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
    Result:= Keys1.ToString+'|'+Keys2.ToString;
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
    Keys1.SetFromString(SKey1);
    Keys2.SetFromString(SKey2);

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

    if (AKeymapItem.Keys1=item.Keys1) or
       (AKeymapItem.Keys2=item.Keys1) then itemKeyPtr:= @item.Keys1 else
    if (AKeymapItem.Keys1=item.Keys2) or
       (AKeymapItem.Keys2=item.Keys2) then itemKeyPtr:= @item.Keys2 else
    Continue;

    if AOverwriteAndSave then
    begin
      //clear in memory
      itemKeyPtr^.Clear;

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
    if (item.Keys1.ToString=AHotkey) or
       (item.Keys2.ToString=AHotkey) then exit(true);
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

function IsLexerStyleCommentOrString(const ALexer, AStyle: string): boolean;
var
  Styles: string;
begin
  if (ALexer='') or (AStyle='') then exit(false);
  Styles:=
    GetAppLexerPropInCommentsSection(ALexer, 'styles_cmt') + ',' +
    GetAppLexerPropInCommentsSection(ALexer, 'styles_str');
  Result:= Pos(','+AStyle+',', ','+Styles+',' )>0;
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


procedure DoStatusbarTextByTag(AStatus: TATStatus; ATag: PtrInt; const AText: string);
var
  NIndex: integer;
begin
  NIndex:= AStatus.FindPanel(ATag);
  if NIndex>=0 then
    AStatus.Captions[NIndex]:= AText;
end;

procedure DoStatusbarHintByTag(AStatus: TATStatus; ATag: PtrInt; const AText: string);
var
  NIndex: integer;
begin
  NIndex:= AStatus.FindPanel(ATag);
  if NIndex>=0 then
    AStatus.Hints[NIndex]:= AText;
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
  if AFilename='' then exit;

  if IsFileTooBigForLexer(AFilename) then
  begin
    LexerLite:= AppManagerLite.FindLexerByFilename(AFilename);
  end
  else
  begin
    Lexer:= DoLexerDetectByFilenameOrContent(AFilename);
    if Lexer=nil then
      LexerLite:= AppManagerLite.FindLexerByFilename(AFilename);
  end;

  if Assigned(Lexer) then
    LexerName:= Lexer.LexerName
  else
  if Assigned(LexerLite) then
    LexerName:= LexerLite.LexerName+msgLiteLexerSuffix;
end;


function FixFontMonospaced(const AName: string): string; inline;
begin
  Result:= AName;
  {
  //commented, it slows down start by 10-20ms
  if (AName='') or (Screen.Fonts.IndexOf(AName)>=0) then
    Result:= AName
  else
    Result:= 'Courier';
    }
end;


procedure FixFormPositionToDesktop(F: TForm);
const
  cReservePixels = 100;
var
  R: TRect;
begin
  R:= Screen.DesktopRect;
  F.Left:= Max(F.Left, R.Left);
  F.Left:= Min(F.Left, R.Right-F.Width);
  F.Top:= Min(F.Top, R.Bottom-cReservePixels);
end;

procedure FixRectPositionToDesktop(var F: TRect);
const
  cReservePixels = 200;
var
  R: TRect;
  w, h: integer;
begin
  w:= F.Width;
  h:= F.Height;

  R:= Screen.DesktopRect;
  F.Left:= Max(F.Left, R.Left);
  F.Left:= Min(F.Left, R.Right-F.Width);
  F.Top:= Min(F.Top, R.Bottom-cReservePixels);

  F.Right:= F.Left+w;
  F.Bottom:= F.Top+h;
end;


{ TAppKeyValues }

procedure TAppKeyValues.Add(const AKey, AValue: string);
var
  Item: TAppKeyValue;
begin
  Item:= TAppKeyValue.Create;
  Item.Key:= AKey;
  Item.Value:= AValue;
  inherited Add(Item);
end;

function TAppKeyValues.GetValue(const AKey, ADefValue: string): string;
var
  Item: TAppKeyValue;
  i: integer;
begin
  for i:= 0 to Count-1 do
  begin
    Item:= TAppKeyValue(Items[i]);
    if Item.Key=AKey then
      exit(Item.Value);
  end;
  Result:= ADefValue;
end;

function AppScale(AValue: integer): integer; inline;
begin
  Result:= AValue * UiOps.Scale div 100;
end;

function AppScaleFont(AValue: integer): integer;
begin
  if UiOps.ScaleFont=0 then
    Result:= AppScale(AValue)
  else
    Result:= AValue * UiOps.ScaleFont div 100;
end;

procedure AppScaleToolbar(C: TATFlatToolbar);
begin
  if Assigned(C.Images) then
    C.ButtonWidth:= C.Images.Width+6;
  C.UpdateControls();
end;

procedure AppScaleScrollbar(C: TATScroll);
begin
  C.WidthInitial:= UiOps.ScrollbarWidth;
  C.ScalePercents:= UiOps.Scale;
end;


initialization
  InitDirs;
  InitEditorOps(EditorOps);
  InitUiOps(UiOps);

  AppCommandList:= TList.Create;
  AppEventList:= TList.Create;
  AppSidePanels:= TList.Create;
  AppBottomPanels:= TList.Create;
  AppTreeHelpers:= TList.Create;

  AppKeymap:= TATKeymap.Create;
  InitKeymapFull(AppKeymap);
  InitKeymapForApplication(AppKeymap);

  FillChar(AppBookmarkSetup, SizeOf(AppBookmarkSetup), 0);
  AppBookmarkImagelist:= TImageList.Create(nil);

  AppShortcutEscape:= ShortCut(VK_ESCAPE, []);
  AppShortcutShiftTab:= ShortCut(VK_TAB, [ssShift]);

  Mouse.DragImmediate:= false;
  Mouse.DragThreshold:= 12;

  AppConfig_Detect:= TAppKeyValues.Create;
  AppConfig_DetectLine:= TAppKeyValues.Create;
  AppConfig_PGroups:= TAppKeyValues.Create;

  ////detection of Shell files
  ////disabled: it detects Python files with shebang
  //AppConfig_DetectLine_Keys.Add('\#!.+');
  //AppConfig_DetectLine_Values.Add('Bash script');

  //detection of XML
  AppConfig_DetectLine.Add('<\?xml .+', 'XML');

finalization
  FreeAndNil(AppConfig_PGroups);
  FreeAndNil(AppConfig_DetectLine);
  FreeAndNil(AppConfig_Detect);
  FreeAndNil(AppKeymap);
  FreeAndNil(AppBookmarkImagelist);

  FreeAndNil(AppTreeHelpers);
  FreeAndNil(AppBottomPanels);
  FreeAndNil(AppSidePanels);
  FreeAndNil(AppEventList);
  FreeAndNil(AppCommandList);

end.

