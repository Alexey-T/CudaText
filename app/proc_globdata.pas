(*
This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at http://mozilla.org/MPL/2.0/.

Copyright (c) Alexey Torgashin
*)
unit proc_globdata;

{$mode objfpc}{$H+}
{$IOChecks off}
{$ModeSwitch advancedrecords}

interface

uses
  {$ifdef windows}
  Windows,
  {$endif}
  Classes, SysUtils, Forms, Controls, Menus, ExtCtrls,
  Dialogs, Graphics,
  StrUtils,
  Math,
  syncobjs,
  gqueue,
  InterfaceBase,
  LclProc, LclType, LazFileUtils,
  FileUtil,
  IniFiles,
  Process,
  EncConv,
  ATSynEdit,
  ATSynEdit_Globals,
  ATSynEdit_Keymap,
  ATSynEdit_Keymap_Init,
  ATSynEdit_Adapter_litelexer,
  ATSynEdit_Commands,
  ATSynEdit_CharSizeArray,
  ATSynEdit_ClipRecents,
  ATStringProc,
  ATStringProc_Separator,
  ATFlatThemes,
  ATStatusBar,
  ATScrollBar,
  ATTabs,
  at__jsonconf,
  proc_cmd,
  proc_msg,
  proc_str,
  AppUniqueInstance,
  ec_LexerList,
  ec_SyntAnal;

type
  TAppConsoleQueue = specialize TQueue<UnicodeString>;

  TAppCommandDelayed = record
    Code: integer;
    Tabs: TATTabs;
    TabIndex: integer;
    Invoke: TATCommandInvoke;
    EdAddress: pointer;
    EdIndex: integer;
  end;

  TAppCommandsDelayed = specialize TQueue<TAppCommandDelayed>;

  TAppStringArray = array of string;

{$ifdef unix}
var
  AppUniqInst: TUniqueInstance = nil;

function IsAnotherInstanceRunning: boolean;
{$endif}

var
  AppFormShowCompleted: boolean = false;
  AppAllowFrameParsing: boolean = false; //must be set in FormMain.OnShow
  AppSessionIsLoading: boolean = false;
  AppSessionIsClosing: boolean = false;
  AppCommandHandlerIsBusy: boolean = false; //currently is set by 'close all' command only
  AppActiveForm: TObject = nil;
  AppThemeStatusbar: TATFlatTheme;
  AppApiOnStartActivated: boolean = false;
  AppApiDialogCounter: integer = 0;
  AppDroppedFiles: array of string = nil;
  AppDroppingFiles: boolean = false;
  AppClosingTabs: boolean = false;
  AppOpeningFile: boolean = false;

  AppCodetreeState: record
    Editor: TATSynEdit;
    Lexer: string;
    Version: Int64;
    SelLine: integer;
    DblClicking: boolean;
    NeedsSelJump: boolean;
  end;

  AppLexersLastDetected: TStringList = nil;

type
  TApp3States = (
    a3sOff,
    a3sOn,
    a3sPassive
    );

  TAppHistoryElement = (
    ahhText,
    ahhCaret,
    ahhCaretSel,
    ahhTopLine,
    ahhTabSize,
    ahhEncoding,
    ahhBookmarks,
    ahhLexer,
    ahhWordWrap,
    ahhMinimap,
    ahhMicromap,
    ahhRuler,
    ahhUnprinted,
    ahhLineNumbers,
    ahhScale,
    ahhFolding,
    ahhMarkers,
    ahhTabColor,
    ahhCodeTreeFilter,
    ahhTabSplit,
    ahhMargin,
    ahhUndoRedo
    );

const
  cAppHistoryElementChar: array[TAppHistoryElement] of char =
    ('t', 'c', 'h', 's', 'T', 'e', 'b', 'l', 'w', 'M', 'm', 'r', 'u', 'n',
     'S', 'f', 'k', 'C', 'F', 'i', 'I', 'U');

const
  cAppSessionDefaultBase = 'history session';
  cAppSessionDefault = cAppSessionDefaultBase+'.json';

const
  cAppMaxGroup = Pred(6+3); //6 normal groups + 3 floating groups

var
  //ATSynEdit has range for bookmarks 0..63, 0=none
  AppBookmarkSetup: array[1..63] of
    record
      ImageIndex: integer;
      Color: TColor;
    end;
var
  AppListRecents: TStringList = nil;
  AppListTimers: TStringList = nil;
  AppStatusbarMessages: TStringList = nil;
  AppBookmarkImagelist: TImageList = nil;
  AppApiFlatTheme: TATFlatTheme;
  AppAlwaysNewInstance: boolean = false;
  AppSessionName: string = '';
  AppServerId: string = 'cudatext.0'; //used by TUniqueInstance (which is used only on Unix)

var
  AppFrameList1: TFPList = nil;
    //all frames - for main thread

  AppFrameList2: TFPList = nil;
    //all frames - for file watcher thread

  AppFrameListDeleting: TFPList = nil;
    //frames which need to be Free'd
    //we don't free frames instantly, because watcher thread can access them

  AppEventLister: TEvent = nil; //event set to signaled, when main thread has done AppFrameList2 updating
  AppEventWatcher: TEvent = nil; //event set to signaled, when watcher thread is not busy

type
  { TAppKeyValues }

  TAppKeyValues = class(TFPList)
  private type
    TAppKeyValue = class
      Key: string;
      Value: string;
    end;
  public
    procedure Add(const AKey, AValue: string);
    destructor Destroy; override;
    function GetValue(const AKey, ADefValue: string): string;
    function GetValueByRegex(ALine: string; ACaseSens: boolean): string;
  end;

var
  AppConfig_Detect: TAppKeyValues = nil;
  AppConfig_DetectLine: TAppKeyValues = nil;
  AppConfig_PGroups: TAppKeyValues = nil;

const
  AppExtensionThemeUi = '.cuda-theme-ui';
  AppExtensionThemeSyntax = '.cuda-theme-syntax';

type
  TUiOpsFindCaseSensitive = (
    ufcsCaseIgnore,
    ufcsCaseSens,
    ufcsCaseFromDialog
    );

type
  TUiOps = record
    VarFontName: string;
    VarFontSize: integer;
    OutputFontName: string;
    OutputFontSize: integer;
    StatusbarFontName: string;
    StatusbarFontSize: integer;
    DoubleBuffered: boolean;

    //timer delay for many commands. see proc_cmd.pas, function IsCommandNeedTimer.
    //if too low, we have risk of crash in 'close tab' commands. 150ms is safe.
    CommandTimerInterval: integer;

    //Sleep() delay used when plugin calls Editor.cmd() (for the same list of commands).
    PyCommandSleepInterval: integer;

    PyLibrary: string;
    PyCaretSlow: integer;
    PyChangeSlow: integer;
    PyOutputCopyToStdout: boolean;

    MaxLineLenForEditingKeepingLexer: integer;
    MaxSizeForSession: integer;
    MaxLinesForMicromapPaint: integer;
    InfoAboutOptionsEditor: boolean;
    AllowRunPkExec: boolean;
    AllowCheckConfigsForNullBytes: boolean;

    LogPluginIniting: boolean;
    LogSessions: boolean;
    //LogDebug: boolean;
    LogConsole: boolean;
    LogConsoleDetailedStartupTime: boolean;

    LexerThemes: boolean;
    LexerMenuGrouped: boolean;
    LexerPostponeUntilShown: boolean;
    LexerParsingMinTimeForEvent: integer;

    ToolBarTheme: string;
    LangName: string;

    ThemeUi: string;
    ThemeSyntax: string;
    ThemeUi_Loaded: boolean;
    ThemeSyntax_Loaded: boolean;
    ThemedMainMenu: boolean;
    ThemedMainMenuFontName: string;
    ThemedMainMenuFontSize: integer;

    SidebarShow: boolean;
    SidebarOnRight: boolean;
    SidebarTheme: string;
    SidepanelOnStart: integer;
    BottomOnStart: integer;
    PictureTypes: string;
    DefaultTabSplitIsHorz: boolean;
    MaxFileSizeToOpen: integer;
    MaxFileSizeForLexer: integer;
    MaxFileSizeWithoutProgressForm: integer;
    MaxStatusbarMessages: integer;
    MaxMaxStatusbarMessages: integer;
    MaxUndoSizeForSessionFile: integer;

    AutocompleteAcpFiles: boolean;
    AutocompleteHtml: boolean;
    AutocompleteHtml_AutoClose: boolean;
    AutocompleteHtml_Lexers: string;
    AutocompleteCss: boolean;
    AutocompleteCss_Lexers: string;
    AutocompleteFileURI: boolean;
    AutocompleteInComments: boolean;
    AutocompleteInCommentsHTML: boolean;
    AutocompleteInStrings: boolean;
    AutocompleteClosingDelay: integer;

    HtmlBackgroundColorPair: array[boolean] of TColor;
    CharMapFontIncreasing: integer;

    ListboxCentered: boolean;
    ListboxSizeX: integer;
    ListboxSizeY: integer;
    ListboxAutoCompleteWidth: integer;
    ListboxAutoCompleteMaxItems: integer;
    ListboxFuzzySearch: boolean;
    ListboxHotkeyFontSizeDelta: integer;
    ListboxTopItemIndent: integer;

    TabsShowFoldersSuffix: boolean;
    TabsShowFoldersMaxLevels: integer;
    TabsResetUntitledCounter: boolean;
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
    TabSpaceBeforeText: integer;
    TabSpaceAfterText: integer;
    TabPosition: integer;
    TabColorFull: boolean;
    TabFontScale: integer;
    TabShowX: integer;
    TabShowXSize: integer;
    TabShowXRounded: boolean;
    TabShowPlus: boolean;
    TabDblClickClose: boolean;
    TabNumbers: boolean;
    TabNewNearCurrent: boolean;
    TabRecentOnClose: boolean;
    TabButtonLayout: string;
    TabPreviewFontStyle: string;
    TabSwitcherDialog: boolean;

    MaxHistoryEdits: integer;
    MaxHistoryMenu: integer;
    MaxHistoryFiles: integer;

    CmdPaletteFilterKeep: boolean;
    CmdPaletteFilterText: string;
    CmdPaletteFilterText_Forced: string;

    HistoryDisabledStr: string;
    HistoryItems: array[TAppHistoryElement] of boolean;

    FindSuggestSel: boolean;
    FindSuggestWord: boolean;
    FindSuggestInSelection: boolean;
    FindCurrentWordCaseSensitive: TUiOpsFindCaseSensitive;
    FindShowNoResultsByInputBgColor: boolean;

    FindHiddenButtons: string;
    FindShow_FindFirst: boolean;
    FindShow_FindNext: boolean;
    FindShow_FindPrev: boolean;
    FindShow_ReplaceAll: boolean;
    FindShow_ReplaceGlobal: boolean;
    FindShow_RegEx: boolean;
    FindShow_CaseSens: boolean;
    FindShow_WholeWords: boolean;
    FindShow_Wrapped: boolean;
    FindShow_InSel: boolean;
    FindShow_MultiLine: boolean;
    FindShow_SyntaxElements: boolean;
    FindShow_HiAll: boolean;
    FindShow_ConfirmRep: boolean;
    FindShow_RegexSubst: boolean;
    FindShow_PreserveCase: boolean;

    FindIndentVert: integer;
    FindIndentHorz: integer;
    FindMultiLineScale: double;
    FindSeparateForm: boolean;
    FindHiAll_MaxLines: integer;
    FindHiAll_TagValue: Int64;
    //FindHiAll_MoveCaret: boolean;
    FindOccur_TagValue: Int64;
    FindWrapAtEdge_Delay: integer;
    FindWrapAtEdge_ThemeItem: string;

    AllowProgramUpdates: boolean;
    EscapeClose: boolean;
    EscapeCloseConsole: boolean;
    EscapeCloseFinder: boolean;
    ConsoleWordWrap: boolean;
    InputHeight: integer;
    InitialDir: string;
    OpenDir: string;
    ConfirmLinksClicks: boolean;
    ConfirmSaveEmptyUntitledTab: boolean;
    SplittersUsePoorStyle: boolean;

    ExportHtmlNumbers: boolean;
    ExportHtmlFontName: string;
    ExportHtmlFontSize: integer;

    TreeFontScale: integer;
    TreeTheme: string;
    TreeAutoSync: boolean;
    TreeTimeFill: integer;
    //TreeTimeCaret: integer;
    TreeShowIcons: boolean;
    TreeShowTooltips: boolean;
    TreeFilterLayout: integer;
    TreeSublexers: boolean;
    TreeIconFilenames: string;
    TreeFillMaxTime: integer;
    TreeFillMaxTimeForAPI: integer;

    NewdocLexer: string;
    NewdocEnc: string;
    NewdocEnds: integer;

    ViewerBinaryWidth: integer;
    ViewerNonPrintable: boolean;

    StatusNoSel: string;
    StatusSmallSel: string;
    StatusStreamSel: string;
    StatusColSel: string;
    StatusCarets: string;
    StatusPanels: string;
    StatusTime: integer;
    StatusHeightPercents: integer;
    StatusHeightMin: integer;

    AltTooltipTime: integer;
    AltTooltipTimeMax: integer;
    AltTooltipPaddingX: integer;
    AltTooltipPaddingY: integer;

    ProgressbarWidth: integer;
    ProgressbarHeightSmall: integer;

    ShowMenubar: boolean;
    ShowStatusbar: boolean;
    ShowToolbar: boolean;
    ShowTitlePath: boolean;
    ShowSidebarMenuButton: integer;

    ReopenSession: boolean;
    ReopenSessionWithCmdLine: boolean;
    SessionSaveInterval: integer;
    SessionSaveOnExit: boolean;
    BackupLastSessions: integer;
    SaveModifiedTabsOnClose: boolean;

    ShowFormsOnTop: boolean;
    ShowMenuDialogsWithBorder: boolean;
    UndoPersistent: string;
    AllowSaveOfUnmodifiedFile: boolean;

    PluginDialogsShowInTaskbar: boolean;
    PluginDialogsModalFormStyle: TFormStyle;
    FloatGroupsShowInTaskbar: TShowInTaskbar;
    OneInstance: boolean;

    NotificationEnabled: boolean;
    NotificationTimeSeconds: integer;
    NotificationConfirmReload: integer;
    NotificationPanelHeight: integer;
    NotificationButtonHeight: integer;
    NotificationButtonsDistance: integer;

    NonTextFiles: integer; //0: prompt, 1: open, 2: don't open
    NonTextFilesBufferKb: integer;
    ReloadUnsavedConfirm: boolean;
    ReloadFollowTail: boolean;
    CheckLowDiskSpace: Int64; //minimal disk free space in bytes; can be 0 to disable the check
    FullScreen: string;

    Emmet_AddSlashToEmptyTags: boolean;
    Emmet_CommentTags: boolean;
    Emmet_IndentNested: boolean;
    Emmet_SingleLine: boolean;
    Emmet_TrimLineMarkers: boolean;
    Emmet_WordWrap: boolean;

    HotkeyFindDialog,
    HotkeyReplaceDialog,
    HotkeyFindFirst,
    HotkeyFindNext,
    HotkeyFindPrev,
    HotkeyReplaceAndFindNext,
    HotkeyReplaceNoFindNext,
    HotkeyReplaceAll,
    HotkeyReplaceGlobal,
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
    HotkeyToggleConfirmRep,
    HotkeyToggleTokens,
    HotkeyToggleHiAll,
    HotkeyTogglePresCase,
    HotkeyFindMenu
      : string;
  end;
var
  UiOps: TUiOps;

const
  OpStr_UiFontName = 'ui_font_name'+cOptionSystemSuffix;
  OpStr_UiFontSize = 'ui_font_size'+cOptionSystemSuffix;
  OpStr_UiFontOutputName = 'ui_font_output_name'+cOptionSystemSuffix;
  OpStr_UiFontOutputSize = 'ui_font_output_size'+cOptionSystemSuffix;

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
    OpFontSize_original: integer;
    OpFontSize_original_i: integer;
    OpFontSize_original_b: integer;
    OpFontSize_original_bi: integer;

    OpFontQuality: TFontQuality;
    OpFontLigatures: boolean;
    OpFlickerReducingPause: integer;

    OpScrollbarsNew: boolean;
    OpSpacingY: integer;
    OpTabSize: integer;
    OpTabSpaces: boolean;
    OpTabSmart: boolean;

    OpMaxLineLenForBracketFinder: integer;

    OpActiveBorderRaw: integer;
    OpActiveBorderInControls: boolean;
    OpActiveBorderInEditor: boolean;
    OpActiveBorderWidth: integer;

    OpOverwriteSel: boolean;
    OpOverwriteOnPaste: boolean;
    OpPasteWithEolAtLineStart: boolean;

    OpAutoCloseBracketsMultiCarets: boolean;
    OpAutoCloseBrackets: string;
    OpAutocompleteAutoshowCharCount: integer;
    OpAutocompleteTriggerChars: string;
    OpAutocompleteCommitChars: string;
    OpAutocompleteCommitOnEnter: boolean;
    OpAutocompleteCloseChars: string;
    OpAutocompleteAddOpeningBracket: boolean;
    OpAutocompleteUpDownAtEdge: integer;
    OpAutocompleteCommitIfSingleItem: boolean;

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
    OpGutterLineStates: boolean;
    OpGutterIconSize: integer;

    OpNumbersShow: boolean;
    OpNumbersStyle: integer;
    OpNumbersForCarets: boolean;
    OpNumbersCenter: boolean;

    OpRulerShow: boolean;
    OpRulerNumeration: integer;
    OpRulerMarkCaret: integer;
    OpRulerHeight: integer;

    OpMinimapShow: boolean;
    OpMinimapShowSelAlways: boolean;
    OpMinimapShowSelBorder: boolean;
    OpMinimapCharWidth: integer;
    OpMinimapAtLeft: boolean;
    OpMinimapScale: integer;
    OpMinimapTooltipShow: boolean;
    OpMinimapTooltipHeight: integer;
    OpMinimapTooltipWidth: integer;
    OpMinimapTooltipFontSize: integer;
    OpMinimapDragImmediately: boolean;

    OpMicromapShow: boolean;
    OpMicromapOnScrollbar: boolean;
    OpMicromapLineStates: boolean;
    OpMicromapSelections: boolean;
    OpMicromapBookmarks: boolean;
    OpMicromapSmallMarkSizePercents: integer;
    OpMicromapMinMarkHeight: integer;

    OpMarginFixed: integer;
    OpMarginString: string;

    OpMarkerSize: integer;
    OpStaplesStyle: integer;
    OpStaplesProps: string;
    OpStapleIndentConsidersEnd: boolean;

    //unprinted
    OpUnprintedShow: boolean;
    OpUnprintedContent: string;

    //wrap
    OpWrapMode: integer;
    OpWrapIndented: boolean;
    OpWrapEnabledMaxLines: integer;

    //undo
    OpUndoLimit: integer;
    OpUndoGrouped: boolean;
    OpUndoAfterSave: boolean;
    OpUndoMaxCarets: integer;
    OpUndoIndentVert: integer;
    OpUndoIndentHorz: integer;
    OpUndoPause: integer;
    OpUndoMouseClicks: boolean;

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
    OpCaretsPrimitiveColumnSel: boolean;
    OpCaretKeepVisibleOnScroll: boolean;
    OpCaretProximityVert: integer;
    OpCaretOnLoadingLimitByLineEnds: boolean;

    //general
    OpKeepSelFontColor: boolean;
    OpShowCurLine: boolean;
    OpShowCurLineMinimal: boolean;
    OpShowCurLineOnlyFocused: boolean;
    OpShowCurCol: boolean;
    OpShowLastLineOnTop: boolean;
    OpShowFullBackgroundSel: boolean;
    OpShowFullBackgroundSyntax: boolean;
    OpShowMouseSelFrame: boolean;
    OpShowIndentLines: boolean;
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
    OpScrollStyleHorz: integer;
    OpLexerDynamicHiliteMaxLines: integer;
    OpLexerLineSeparators: boolean;
    OpZebra: integer;
    OpZebraStep: integer;
    OpDimUnfocused: integer;
    OpCommandLogMaxCount: integer;

    OpNonWordChars: UnicodeString;
    OpFoldStyle: integer;
    OpFoldTooltipShow: boolean;
    OpFoldIconForMinimalRangeHeight: integer;

    //indent
    OpIndentAuto: boolean;
    OpIndentAutoKind: integer;
    OpIndentSize: integer;
    OpIndentAutoRule: string;
    OpUnIndentKeepsAlign: boolean;
    OpIndentMakesWholeLineSel: boolean;

    //mouse
    OpMouseGotoDefinition: string;
    OpMouse2ClickDragSelectsWords: boolean;
    OpMouseDragDrop: boolean;
    OpMouseMiddleClickAction: integer;
    OpMouseRightClickMovesCaret: boolean;
    OpMouseEnableColumnSelection: boolean;
    OpMouseHideCursorOnType: boolean; //don't work on lin
    OpMouseGutterClickSelectedLine: boolean;
    OpMouseWheelZoom: boolean;
    OpMouseWheelSpeedVert: integer;
    OpMouseWheelSpeedHorz: integer;
    OpMouseClickNumberSelectsEol: boolean;
    OpMouseClickLinks: integer;

    //keys
    OpKeyBackspaceUnindent: boolean;
    OpKeyBackspaceWrap: boolean;
    OpKeyTabIndents: boolean;
    OpKeyHomeToNonSpace: boolean;
    OpKeyHomeEndNavigateWrapped: boolean;
    OpKeyEndToNonSpace: boolean;
    OpKeyPageKeepsRelativePos: boolean;
    OpKeyPageUpDownSize: integer;
    OpKeyUpDownKeepColumn: boolean;
    OpKeyUpDownNavigateWrapped: boolean;
    OpKeyUpDownAllowToEdge: boolean;
    OpKeyLeftRightGoToNextLineWithCarets: boolean;
    OpKeyLeftRightSwapSel: boolean;
    OpKeyLeftRightSwapSelAndSelect: boolean;

    OpBracketHilite: boolean;
    OpBracketSymbols: string;
    OpBracketDistance: integer;
  end;
var
  EditorOps: TEditorOps;

var
  EditorOps_CenteringWidth: TStringList;
  EditorOps_CenteringDistFree: TStringList;

var
  AppUserName: string = '';
  AppDir_Home: string = '';
  AppDir_Settings: string = '';
  AppDir_SettingsDefault: string = '';
  AppDir_Py: string = '';
  AppDir_Data: string = '';
  AppDir_Lexers: string = '';
  AppDir_LexersLite: string = '';
  AppDir_DataThemes: string = '';
  AppDir_DataAutocomplete: string = '';
  AppDir_DataAutocompleteSpec: string = '';
  AppDir_DataLang: string = '';
  AppDir_DataSidebarIcons: string = '';
  AppDir_DataCodetreeIcons: string = '';
  AppDir_DataToolbarIcons: string = '';
  AppDir_LastInstalledAddon: string = '';
  AppFile_OptionsDefault: string = '';
  AppFile_OptionsUserInit: string = '';
  AppFile_OptionsUser: string = '';
  AppFile_History: string = '';
  AppFile_HistoryFiles: string = '';
  AppFile_Hotkeys: string = '';
  AppFile_PluginsIni: string = '';
  AppFile_LogConsole: string = '';

function AppFile_Session: string;
function AppFile_Language: string;
function AppFile_UndoRedo(const fn: string; IsRedo: boolean): string;

function AppFile_Lexer(const ALexName: string): string;
function AppFile_LexerMap(const ALexName: string): string;
function AppFile_LexerOps(const ALexName: string): string;
function AppFile_LexerAcp(const ALexName: string): string;
function AppFile_LexerSpecificConfig(ALexer: string; ADefaultConfig: boolean=false): string;
function AppFile_IsIgnoredForSession(const AFilename: string): boolean;

function AppSessionName_ForHistoryFile: string;
function IsDefaultSession(const S: string): boolean;
function IsDefaultSessionActive: boolean;

function IsSetToOneInstance: boolean;
function InitPyLibraryPath: string;

function MsgBox(const AText: string; AFlags: Longint): integer;
procedure MsgBadConfig(const fn, msg: string);
procedure MsgStdout(const Str: string; AllowMsgBox: boolean = false);
procedure MsgLogConsole(const AText: string);
procedure MsgLogToFilename(const AText, AFilename: string; AWithTime: boolean);
procedure MsgOldApi(const s: string);
procedure MsgFileFromSessionNotFound(const fn: string);

function AppListboxItemHeight(AScale, ADoubleHeight: boolean): integer;
procedure AppUpdateWatcherFrames(AMaxWorkTime: integer = 500);
procedure AppStopListTimers;

procedure FixFormPositionToDesktop(F: TForm);
procedure FixRectPositionToDesktop(var F: TRect);
function IsColorDark(C: TColor): boolean;

procedure AppApplyRendererTweaks(const s: string);
procedure AppApplyScrollbarStyles(const s: string);
procedure AppApplyUnprintedSymbolsScale(const s: string);
procedure AppApplyFallbackEncoding(const s: string);
procedure AppApplyAutoCopyToClipboard(const s: string);

function AppOption_LoadFromStringlist(L: TStringList; const AKey: string; ADefault: integer): integer;
procedure AppOption_SaveToStringlist(L: TStringList; const AKey: string; AValue: integer);

type
  { TKeymapHelper }

  TKeymapHelper = class
  public
    class procedure ClearKey(AKeymap: TATKeymap; AItemIndex, AKeyIndex: integer);
    class procedure ClearKeyInAll(AItemIndex, AKeyIndex: integer);

    class function GetHotkey(AKeymap: TATKeymap; const ACmdString: string): string;
    class function SetHotkey(AKeymap: TATKeymap; const AParams: string; AndSaveFile: boolean): boolean;

    class function CheckDuplicateForCommand(
      AKeymapItem: TATKeymapItem;
      const ALexerName: string;
      AOverwriteAndSave: boolean): integer;

    class function GetForLexer(const ALexer: string): TATKeymap;
    class procedure LoadConfig(AKeymap: TATKeymap; const AFileName: string; AForLexer: boolean);

    class procedure ItemSaveToConfig(K: TATKeymapItem; const path, ALexerName: string; ALexerSpecific: boolean);
    class procedure ItemDeleteInConfig(K: TATKeymapItem; const path, ALexerName: string; ALexerSpecific: boolean);

    class function SaveKey_ForPlugin(AKeymap: TATKeymap; AOverwriteKey: boolean;
      const AMenuitemCaption, AModuleName, AMethodName, ALexerName, AHotkey: string): boolean;
  end;

function DoReadOneStringFromFile(const AFilename: string): string;
function DoReadContentFromFile(const AFilename: string): string;
procedure DoWriteStringToFile(const AFilename, AText: string);

function AppCollapseHomeDirInFilename(const fn: string): string;
function AppExpandHomeDirInFilename(const fn: string): string;
function AppExpandFileNameWithDir(const AFileName, ADir: string): string;
function AppConfigKeyForBookmarks(Ed: TATSynEdit): string;
procedure AppDiskCheckFreeSpace(const fn: string);
function AppKeyIsAllowedAsCustomHotkey(Key: Word; Shift: TShiftState): boolean;

var
  AppManager: TecLexerList = nil;
  AppManagerLite: TATLiteLexers = nil;
  AppShortcutEscape: TShortcut = 0;
  AppShortcutShiftTab: TShortcut = 0;

  AppKeymapMain: TATKeymap = nil;
  AppKeymapInitial: TATKeymap = nil; //inited only by API calls
  AppKeymapLexers: TStringList = nil;

type
  TAppStringEvent = procedure(Sender: TObject; const AStr: string) of object;
  TAppStringFunction = function(const AStr: string): boolean of object;

type
  TAppEncodingRecord = record
    Name,
    ShortName: string;
  end;

const
  AppEncodings: array[0..48] of TAppEncodingRecord = (
    (Name: cEncNameUtf8_NoBom; ShortName: 'utf8'),
    (Name: cEncNameUtf8_WithBom; ShortName: 'utf8_bom'),
    (Name: cEncNameUtf16LE_NoBom; ShortName: 'utf16le'),
    (Name: cEncNameUtf16LE_WithBom; ShortName: 'utf16le_bom'),
    (Name: cEncNameUtf16BE_NoBom; ShortName: 'utf16be'),
    (Name: cEncNameUtf16BE_WithBom; ShortName: 'utf16be_bom'),
    (Name: cEncNameUtf32LE_NoBom; ShortName: 'utf32le'),
    (Name: cEncNameUtf32LE_WithBom; ShortName: 'utf32le_bom'),
    (Name: cEncNameUtf32BE_NoBom; ShortName: 'utf32be'),
    (Name: cEncNameUtf32BE_WithBom; ShortName: 'utf32be_bom'),
    (Name: 'cp1250'; ShortName: 'cp1250'),
    (Name: 'cp1251'; ShortName: 'cp1251'),
    (Name: 'cp1252'; ShortName: 'cp1252'),
    (Name: 'cp1253'; ShortName: 'cp1253'),
    (Name: 'cp1254'; ShortName: 'cp1254'),
    (Name: 'cp1255'; ShortName: 'cp1255'),
    (Name: 'cp1256'; ShortName: 'cp1256'),
    (Name: 'cp1257'; ShortName: 'cp1257'),
    (Name: 'cp1258'; ShortName: 'cp1258'),
    (Name: 'cp437'; ShortName: 'cp437'),
    (Name: 'cp850'; ShortName: 'cp850'),
    (Name: 'cp852'; ShortName: 'cp852'),
    (Name: 'cp861'; ShortName: 'cp861'),
    (Name: 'cp865'; ShortName: 'cp865'),
    (Name: 'cp866'; ShortName: 'cp866'),
    (Name: 'iso-8859-1'; ShortName: 'iso-8859-1'),
    (Name: 'iso-8859-2'; ShortName: 'iso-8859-2'),
    (Name: 'iso-8859-3'; ShortName: 'iso-8859-3'),
    (Name: 'iso-8859-4'; ShortName: 'iso-8859-4'),
    (Name: 'iso-8859-5'; ShortName: 'iso-8859-5'),
    (Name: 'iso-8859-7'; ShortName: 'iso-8859-7'),
    (Name: 'iso-8859-9'; ShortName: 'iso-8859-9'),
    (Name: 'iso-8859-10'; ShortName: 'iso-8859-10'),
    (Name: 'iso-8859-13'; ShortName: 'iso-8859-13'),
    (Name: 'iso-8859-14'; ShortName: 'iso-8859-14'),
    (Name: 'iso-8859-15'; ShortName: 'iso-8859-15'),
    (Name: 'iso-8859-16'; ShortName: 'iso-8859-16'),
    (Name: 'mac'; ShortName: 'mac'),
    (Name: 'koi8r'; ShortName: 'koi8r'),
    (Name: 'koi8u'; ShortName: 'koi8u'),
    (Name: 'koi8ru'; ShortName: 'koi8ru'),
    (Name: 'cp874'; ShortName: 'cp874'),
    (Name: 'shift-jis'; ShortName: 'shift-jis'),
    (Name: 'gbk'; ShortName: 'gbk'),
    (Name: 'cns'; ShortName: 'cns'),
    (Name: 'uhc'; ShortName: 'uhc'),
    (Name: 'big5'; ShortName: 'big5'),
    (Name: 'gb2312'; ShortName: 'gb2312'),
    (Name: 'euc-kr'; ShortName: 'euc-kr')
  );

type
  TAppPyEventResult = record
    Val: (evrTrue, evrFalse, evrInt, evrString, evrOther);
    Int: integer;
    Str: string;
  end;

type
  TAppPyEvent = (
    cEventOnKey,
    cEventOnKeyUp,
    cEventOnHotspot,
    cEventOnInsert,
    cEventOnChange,
    cEventOnChangeSlow,
    cEventOnCaret,
    cEventOnCaretSlow,
    cEventOnScroll,
    cEventOnMouseStop,
    cEventOnClick,
    cEventOnClickDbl,
    cEventOnClickGutter,
    cEventOnClickGap,
    cEventOnClickLink,
    cEventOnClickRight,
    cEventOnState,
    cEventOnStateEd,
    cEventOnFocus,
    cEventOnStart,
    cEventOnStart2,
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
    cEventOnTabChange,
    cEventOnTabMove,
    cEventOnTabMenu,
    cEventOnPaste,
    cEventOnMessage,
    cEventOnConsoleNav,
    cEventOnConsoleComplete,
    cEventOnOutputNav,
    cEventOnSnippet,
    cEventOnMacro,
    cEventOnAppActivate,
    cEventOnAppDeactivate,
    cEventOnDeleteFile,
    cEventOnSidebarPopup,
    cEventOnCLI,
    cEventOnExit
    );
  TAppPyEvents = set of TAppPyEvent;
  TAppPyEventsPrior = array[TAppPyEvent] of byte;
    //0: default, 1,2...: higher priority
  TAppPyEventsLazy = array[TAppPyEvent] of boolean;

var
  AppEventsMaxPriorities: array[TAppPyEvent] of integer;

const
  cAppPyEvent: array[TAppPyEvent] of string = (
    'on_key',
    'on_key_up',
    'on_hotspot',
    'on_insert',
    'on_change',
    'on_change_slow',
    'on_caret',
    'on_caret_slow',
    'on_scroll',
    'on_mouse_stop',
    'on_click',
    'on_click_dbl',
    'on_click_gutter',
    'on_click_gap',
    'on_click_link',
    'on_click_right',
    'on_state',
    'on_state_ed',
    'on_focus',
    'on_start',
    'on_start2',
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
    'on_tab_change',
    'on_tab_move',
    'on_tab_menu',
    'on_paste',
    'on_message',
    'on_console_nav',
    'on_console_complete',
    'on_output_nav',
    'on_snippet',
    'on_macro',
    'on_app_activate',
    'on_app_deactivate',
    'on_delete_file',
    'on_sidebar_popup',
    'on_cli',
    'on_exit'
    );

type

  { TAppCommandInfo }

  TAppCommandInfo = class
  public
    ItemModule: string;
    ItemProc: string;
    ItemProcParam: string;
    ItemCaption: string;
    ItemLexers: string;
    ItemInMenu: string;
    ItemFromApi: boolean;
    function CommaStr: string;
  end;

type
  TAppEventInfo = class
    ItemModule: string;
    ItemLexers: string;
    ItemEvents: TAppPyEvents;
    ItemEventsPrior: TAppPyEventsPrior;
    ItemEventsLazy: TAppPyEventsLazy;
    ItemKeys: string;
  end;

type
  TAppTreeHelper = class
    ItemModule: string;
    ItemProc: string;
    ItemLexers: string;
  end;

var
  AppConsoleQueue: TAppConsoleQueue = nil;
  AppCommandsDelayed: TAppCommandsDelayed = nil;
  AppCommandList: TFPList = nil;
  AppEventList: TFPList = nil;
  AppTreeHelpers: TFPList = nil;

procedure AppClearPluginLists;

type
  TAppMenuProps = class(TComponent)
  public
    CommandCode: integer;
    CommandString: string;
    TagString: string;
  end;

type
  { TPluginHelper }

  TPluginHelper = class
  public
    class function CommandCategory(Cmd: integer): TAppCommandCategory;
    class function CommandHasConfigurableHotkey(Cmd: integer): boolean;
    class procedure CommandsClearButKeepApiItems;
    class function CommandGetIndexFromModuleAndMethod(const AText: string): integer;
    class procedure CommandUpdateSubcommands(const AText: string);

    class function EventIsUsed(AEvent: TAppPyEvent): boolean;
    class procedure EventStringToEventData(const AEventStr: string;
      out AEvents: TAppPyEvents;
      out AEventsPrior: TAppPyEventsPrior;
      out AEventsLazy: TAppPyEventsLazy);
    class procedure EventsUpdate(const AModuleName, AEventStr, ALexerStr, AKeyStr: string);
    class procedure EventsMaxPrioritiesUpdate;

    class function CommandCode_To_HotkeyStringId(ACmd: integer): string;
    class function HotkeyStringId_To_CommandCode(const AId: string): integer;

    class function Debug_PluginCommands(const AModule: string): string;
  end;


function AppEncodingShortnameToFullname(const S: string): string;
function AppEncodingFullnameToShortname(const S: string): string;
function AppEncodingListAsString: string;

procedure UpdateFormOnTop(F: TForm);

procedure DoStatusbarTextByTag(AStatus: TATStatus; ATag: PtrInt; const AText: string);
procedure DoStatusbarHintByTag(AStatus: TATStatus; ATag: PtrInt; const AText: string);
procedure DoStatusbarColorByTag(AStatus: TATStatus; ATag: PtrInt; AColor: TColor);

function IsFileTooBigForOpening(const AFilename: string): boolean;
function IsFileTooBigForLexer(const AFilename: string): boolean;
function IsFilenameForLexerDetecter(const AFileName: string): boolean;
function IsOsFullPath(const S: string): boolean;

procedure Lexer_DetectByFilename(const AFilename: string;
  out Lexer: TecSyntAnalyzer;
  out LexerLite: TATLiteLexer;
  out LexerName: string;
  out ATooBigForLexer: boolean;
  AChooseFunc: TecLexerChooseFunc);
function Lexer_DetectByFilenameOrContent(const AFilename: string;
  AChooseFunc: TecLexerChooseFunc): TecSyntAnalyzer;
procedure Lexer_EnumAll(L: TStringList; AlsoDisabled: boolean = false);
function Lexer_IsNameCorrect(AName: string): boolean;

procedure DoMenuitemEllipsis(c: TMenuItem);

procedure AppOnLexerLoaded(Sender: TObject; ALexer: TecSyntAnalyzer);
procedure AppLoadLexers;

type
  { TAppManagerThread }

  TAppManagerThread = class(TThread)
  public
    procedure Execute; override;
  end;

var
  AppManagerThread: TAppManagerThread = nil;

type
  TAppAutocompleteCallback = procedure(Ed: TATSynEdit; AActivate: boolean) of object;

var
  AppRunAutocomplete: TAppAutocompleteCallback = nil;

  //'c': by hotkey or command
  //'a': by 'autocomplete_autoshow_chars' option
  //'r': by showing again by typing or left/right
  AppAutocompleteInvoke: char = 'c';

const
  AppDefaultEdFont: string = '';
  AppDefaultEdFonts: array[0..2] of string =
    {$ifdef windows}
    ('Consolas', 'Courier New', 'Courier');
    {$else}
      {$ifdef darwin}
      ('Monaco', 'Liberation Mono', 'DejaVu Sans Mono');
      {$else}
        {$ifdef haiku}
        ('Noto Sans Mono', '', '');
        {$else}
        ('DejaVu Sans Mono', 'Liberation Mono', 'Courier New');
        {$endif}
      {$endif}
    {$endif}

  AppDefaultEdFontSize =
    {$ifdef haiku}
    12;
    {$else}
    9;
    {$endif}


implementation

uses
  ATCanvasPrimitives,
  ATSynEdit_LineParts,
  ATSynEdit_Adapter_EControl,
  ec_syntax_format,
  proc_files,
  proc_colors,
  proc_lexer_styles;

function MsgBox(const AText: string; AFlags: Longint): integer;
//Application.MessageBox is not used, to translate button captions
var
  Typ: TMsgDlgType;
  Res: TModalResult;
begin
  case AFlags and $F0 of
    MB_ICONERROR:
      Typ:= mtError;
    MB_ICONWARNING:
      Typ:= mtWarning;
    MB_ICONINFORMATION:
      Typ:= mtInformation;
    MB_ICONQUESTION:
      Typ:= mtConfirmation
    else
      Typ:= mtInformation;
  end;

  case AFlags and $0F of
    MB_OK:
      Res:= QuestionDlg(msgTitle, AText, Typ,
            [mrOk, msgButtonOk],
            0);
    MB_OKCANCEL:
      Res:= QuestionDlg(msgTitle, AText, Typ,
            [mrOk, msgButtonOk,
            mrCancel, msgButtonCancel],
            0);
    MB_ABORTRETRYIGNORE:
      Res:= QuestionDlg(msgTitle, AText, Typ,
            [mrAbort, msgButtonAbort,
            mrRetry, msgButtonRetry,
            mrIgnore, msgButtonIgnore],
            0);
    MB_YESNO:
      Res:= QuestionDlg(msgTitle, AText, Typ,
            [mrYes, msgButtonYes,
            mrNo, msgButtonNo],
            0);
    MB_YESNOCANCEL:
      Res:= QuestionDlg(msgTitle, AText, Typ,
            [mrYes, msgButtonYes,
            mrNo, msgButtonNo,
            mrCancel, msgButtonCancel],
            0);
    MB_RETRYCANCEL:
      Res:= QuestionDlg(msgTitle, AText, Typ,
            [mrRetry, msgButtonRetry,
            mrCancel, msgButtonCancel],
            0);
    else
      exit(ID_CANCEL);
  end;

  case Res of
    mrOK:
      Result:= ID_OK;
    mrCancel:
      Result:= ID_CANCEL;
    mrYes:
      Result:= ID_YES;
    mrNo:
      Result:= ID_NO;
    mrAbort:
      Result:= ID_ABORT;
    mrRetry:
      Result:= ID_RETRY;
    mrIgnore:
      Result:= ID_IGNORE;
    mrClose:
      Result:= ID_CLOSE;
    else
      Result:= ID_CANCEL;
  end;
end;

procedure MsgBadConfig(const fn, msg: string);
begin
  //MsgBox(msgCannotReadConfig+#10+fn+#10#10+msg, MB_OK+MB_ICONERROR);
  MsgLogConsole('ERROR: '+msgCannotReadConfig+' '+ExtractFileName(fn)+'; '+msg);
end;

function InitPyLibraryPath: string;
const
  cMaxVersion = 12; //last supported is 3.12
  cMinVersionUnix = 5; //first supported on Unix is 3.5
  cMinVersionWindows = 4; //first supported on Windows is 3.4 (for WinXP)
{$ifdef windows}
var
  N, NMaxVersion: integer;
  S, SFile: string;
{$endif}
{$ifdef darwin}
var
  N: integer;
  S: string;
{$endif}
{$ifdef unix}
var
  Dir: string;
  FileInfo: TSearchRec;
{$endif}
begin
  Result:= '';

  {$ifdef windows}
  //Windows XP? limit max Python version to 3.4
  if (Win32MajorVersion=5) and (Win32MinorVersion=1) then
    NMaxVersion:= 4
  else
    NMaxVersion:= cMaxVersion;
  //detect latest existing file python3x.dll in app folder
  S:= ExtractFilePath(Application.ExeName);
  for N:= NMaxVersion downto cMinVersionWindows do
  begin
    SFile:= Format('python3%d.dll', [N]);
    //don't return full filename, this loads DLL with full filename and plugins cannot load
    if FileExists(S+SFile) then
      exit(SFile);
  end;
  exit;
  {$endif}

  {$ifdef darwin}
  for N:= cMaxVersion downto cMinVersionUnix do
  begin
    S:= Format('/Library/Frameworks/Python.framework/Versions/3.%d/lib/libpython3.%d.dylib',
      [N, N]);
    if FileExists(S) then exit(S);
  end;
  exit;
  {$endif}

  {$ifdef freebsd}
  exit('/usr/local/lib/libpython3.6m.so');
  {$endif}

  {$ifdef openbsd}
  exit('/usr/local/lib/libpython3.9.so.0.0');
  {$endif}

  {$ifdef netbsd}
  exit('/usr/pkg/lib/libpython3.7.so');
  {$endif}

  {$ifdef solaris}
  exit('/usr/lib/amd64/libpython3.5m.so');
  {$endif}

  {$ifdef haiku}
  exit('libpython3.10.so.1.0');
  {$endif}

  {$ifdef unix}
  for Dir in [
              '/usr/lib64',
              '/usr/lib',
              '/usr/lib/x86_64-linux-gnu'
             ] do
    if FindFirst(Dir+'/'+'libpython3.*.so', faAnyFile, FileInfo)=0 then
    begin
      Result:= Dir+'/'+FileInfo.Name;
      FindClose(FileInfo);
      exit;
    end;
  {$endif}
end;

var
  OpFileExe: string = '';
  OpDirExe: string = '';
  OpDirLocal: string = '';
  OpDirPrecopy: string = '';

function GetDirPrecopy: string;
begin
  {$ifdef linux}
  exit('/usr/share/cudatext');
  {$endif}

  {$ifdef darwin}
  exit(ExtractFileDir(OpDirExe)+'/Resources');
  {$endif}

  Result:= '';
end;

function AppCollapseHomeDirInFilename(const fn: string): string;
{$ifndef windows}
var
  S: string;
{$endif}
begin
  Result:= fn;

  {$ifndef windows}
  S:= AppDir_Home;

  if fn+DirectorySeparator=S then
    exit('~');

  if SBeginsWith(Result, S) then
    Result:= '~'+DirectorySeparator+Copy(Result, Length(S)+1, MaxInt);
  {$endif}
end;

function AppExpandHomeDirInFilename(const fn: string): string;
begin
  Result:= fn;
  {$ifndef windows}
  if SBeginsWith(Result, '~'+DirectorySeparator) then
    Result:= AppDir_Home+Copy(Result, 3, MaxInt);
  {$endif}
end;

function AppExpandFileNameWithDir(const AFileName, ADir: string): string;
var
  S: string;
begin
  S:= GetCurrentDir;
  SetCurrentDir(ADir);
  Result:= ExpandFileName(AFileName);
  SetCurrentDir(S);
end;


function IsDistroUpdateNeeded: boolean;
begin
  with TIniFile.Create(AppDir_Settings+DirectorySeparator+'packages.ini') do
  try
    Result:= ReadString('app', 'ver', '')<>cAppExeVersion;
    if Result then
      WriteString('app', 'ver', cAppExeVersion);
  finally
    Free
  end;
end;

function AppDirSettingsFromCommandLine: string;
const
  cParam = '-s=';
var
  S: string;
  i: integer;
begin
  Result:= '';
  for i:= 1{not 0} to ParamCount do
  begin
    S:= ParamStr(i);
    if SBeginsWith(S, cParam) then
    begin
      Delete(S, 1, Length(cParam));
      exit(AppExpandHomeDirInFilename(S));
    end;
  end;
end;

function Which(const Executable: string): string;
var
  ExeName,FoundExe:string;
  //Output: string;
begin
  result:='';

  ExeName:=Executable;

  {$ifdef Windows}
  if ExtractFileExt(ExeName)='' then ExeName:=ExeName+'.exe';
  {$endif}

  if FileExists(ExeName) then
    exit(ExeName)
  else
  begin
    FoundExe := ExeSearch(ExeName, '');
    if FileExists(FoundExe) then
      result:=FoundExe
    else
      result:=FindDefaultExecutablePath(ExeName);
  end;

  (*
  {$IFNDEF FREEBSD}
  if (NOT FileIsExecutable(result)) then result:='';
  {$ENDIF}
  *)

  (*
  {$IFDEF UNIX}
  if (NOT FileExists(result)) then
  begin
    RunCommand('which',[ExeName],Output,[poUsePipes, poStderrToOutPut]{$IF DEFINED(FPC_FULLVERSION) AND (FPC_FULLVERSION >= 30200)},swoHide{$ENDIF});
    Output:=Trim(Output);
    if ((Output<>'') and FileExists(Output)) then result:=Output;
  end;
  {$ENDIF}
  *)
end;

procedure InitDirs_Windows;
begin
  AppDir_Home:= '';
end;

procedure InitDirs_macOS;
begin
  //from https://github.com/graemeg/freepascal/blob/master/rtl/unix/sysutils.pp
  AppDir_Home:= GetEnvironmentVariable('HOME');
  AppUserName:= ExtractFileName(AppDir_Home);
  if AppDir_Home<>'' then
    AppDir_Home:= IncludeTrailingPathDelimiter(AppDir_Home);
  OpDirLocal:= AppDir_Home+'Library/Application Support/CudaText';
  CreateDir(OpDirLocal);
end;

procedure InitDirs_Haiku;
var
  HomeConfig: string;
begin
  AppDir_Home:= '/boot/home';
  HomeConfig:= AppDir_Home+'/config/settings';
  OpDirLocal:= HomeConfig+'/cudatext';
  CreateDir(OpDirLocal);
end;

procedure InitDirs_UnixCommon;
var
  HomeConfig: string;
  SPathOrig, SPath: RawByteString;
  bAppPortable: boolean;
begin
  //from https://github.com/graemeg/freepascal/blob/master/rtl/unix/sysutils.pp
  AppDir_Home:= GetEnvironmentVariable('HOME');
  AppUserName:= ExtractFileName(AppDir_Home);
  if AppDir_Home<>'' then
    AppDir_Home:= IncludeTrailingPathDelimiter(AppDir_Home);

  SPathOrig:= Which(OpFileExe);
  //MsgStdout('CudaText binary: '+SPathOrig);
  {$if FPC_FULLVERSION>30200}
  //FileGetSymLinkTarget was added in FPC 3.2
  if FileGetSymLinkTarget(SPathOrig, SPath) then
  begin
    //support relative target of symlink like '../dir/cudatext'
    if not SBeginsWith(SPath, '/') then
      SPath:= ExtractFilePath(SPathOrig)+SPath;
    MsgStdout('CudaText starts via symlink to: '+SPath);
    OpDirLocal:= ExtractFileDir(SPath);
  end
  else
  {$endif}
  begin
    bAppPortable:= DirectoryExists(OpDirExe+'/data/lexlib') and
      not SBeginsWith(OpDirExe, '/opt/');
    if not bAppPortable then
    begin
      HomeConfig:= GetEnvironmentVariable('XDG_CONFIG_HOME');
      if HomeConfig='' then
        HomeConfig:= AppDir_Home + '.config/'
      else
        HomeConfig:= IncludeTrailingPathDelimiter(HomeConfig);

      OpDirLocal:= HomeConfig+'cudatext';
      CreateDir(OpDirLocal);
      //MsgStdout('CudaText starts not portable: '+OpDirLocal);
    end;
  end;
end;

procedure InitDirs;
var
  S: string;
begin
  OpFileExe:= ParamStr(0);
  OpDirExe:= ExtractFileDir(OpFileExe);
  OpDirPrecopy:= GetDirPrecopy;
  OpDirLocal:= OpDirExe;

  {$ifdef windows}
  InitDirs_Windows;
  {$else}
    {$ifdef darwin}
    InitDirs_macOS;
    {$else}
      {$ifdef haiku}
      InitDirs_Haiku;
      {$else}
      InitDirs_UnixCommon;
      {$endif}
    {$endif}
  {$endif}

  //support command line key -s=folder
  AppDir_Settings:= AppDirSettingsFromCommandLine;
  if AppDir_Settings='' then
    AppDir_Settings:= OpDirLocal+DirectorySeparator+'settings';

  if not DirectoryExists(AppDir_Settings) then
    if not CreateDir(AppDir_Settings) then
    begin
      MsgStdout('Cannot create folder: '+AppDir_Settings, true);
      MsgStdout('  Variable "HOME": '+GetEnvironmentVariable('HOME'));
      MsgStdout('  Variable "XDG_CONFIG_HOME": '+GetEnvironmentVariable('XDG_CONFIG_HOME'));
      MsgStdout('  OpDirLocal: '+OpDirLocal);
      MsgStdout('  OpDirExe: '+OpDirExe);
      Halt;
    end;

  AppDir_SettingsDefault:= OpDirLocal+DirectorySeparator+'settings_default';

  {$ifdef linux}
  if OpDirLocal<>OpDirExe then
    if IsDistroUpdateNeeded then
      if DirectoryExists(OpDirPrecopy) then
      begin
        RunCommand('cp', ['-R', '-u', '-t',
          OpDirLocal,
          '/usr/share/cudatext/py',
          '/usr/share/cudatext/data',
          '/usr/share/cudatext/settings_default'
          ], S);
        //set permissions +w for dir+subdirs
        RunCommand('chmod', ['-R', '+w', OpDirLocal], S);
      end;
  {$endif}
  {$ifdef darwin}
  if IsDistroUpdateNeeded then
    if DirectoryExists(OpDirPrecopy) then
      //see rsync help. need options:
      // -u (update)
      // -r (recursive)
      // -t (preserve times)
      RunCommand('rsync', ['-urt',
        OpDirPrecopy+'/',
        OpDirLocal
        ], S);
  {$endif}

  AppDir_Py:= OpDirLocal+DirectorySeparator+'py';
  AppDir_Data:= OpDirLocal+DirectorySeparator+'data';
  {$ifdef haiku}
  AppDir_Py:= '/boot/home/config/non-packaged/data/cudatext/py';
  AppDir_Data:= '/boot/home/config/non-packaged/data/cudatext/data';
  {$endif}
  AppDir_Lexers:= AppDir_Data+DirectorySeparator+'lexlib';
  AppDir_LexersLite:= AppDir_Data+DirectorySeparator+'lexliblite';
  AppDir_DataThemes:= AppDir_Data+DirectorySeparator+'themes';
  AppDir_DataAutocomplete:= AppDir_Data+DirectorySeparator+'autocomplete';
  AppDir_DataAutocompleteSpec:= AppDir_Data+DirectorySeparator+'autocompletespec';
  AppDir_DataLang:= AppDir_Data+DirectorySeparator+'lang';
  AppDir_DataSidebarIcons:= AppDir_Data+DirectorySeparator+'sideicons';
  AppDir_DataCodetreeIcons:= AppDir_Data+DirectorySeparator+'codetreeicons';
  AppDir_DataToolbarIcons:= AppDir_Data+DirectorySeparator+'toolbaricons';
  AppFile_OptionsDefault:= AppDir_SettingsDefault+DirectorySeparator+'default.json';
  AppFile_OptionsUserInit:= AppDir_SettingsDefault+DirectorySeparator+'userinit.json';
  AppFile_OptionsUser:= AppDir_Settings+DirectorySeparator+'user.json';
  AppFile_History:= AppDir_Settings+DirectorySeparator+'history.json';
  AppFile_HistoryFiles:= AppDir_Settings+DirectorySeparator+'history files.json';
  AppFile_Hotkeys:= AppDir_Settings+DirectorySeparator+'keys.json';
  AppFile_PluginsIni:= AppDir_Settings+DirectorySeparator+'plugins.ini';
  AppFile_LogConsole:= AppDir_Settings+DirectorySeparator+'console.log';

  if not FileExists(AppFile_OptionsUser)
    and FileExists(AppFile_OptionsUserInit) then
    CopyFile(AppFile_OptionsUserInit, AppFile_OptionsUser, [], false);
end;

function InitAppDefaultEdFont: string;
var
  i: integer;
begin
  for i:= 0 to High(AppDefaultEdFonts) do
  begin
    Result:= AppDefaultEdFonts[i];
    if Screen.Fonts.IndexOf(Result)>=0 then exit;
  end;
end;

procedure InitEditorOps(var Op: TEditorOps);
begin
  if AppDefaultEdFont='' then
    AppDefaultEdFont:= InitAppDefaultEdFont;

  with Op do
  begin
    OpFontName:= AppDefaultEdFont;
    OpFontName_i:= '';
    OpFontName_b:= '';
    OpFontName_bi:= '';

    OpFontSize:= AppDefaultEdFontSize;
    OpFontSize_i:= OpFontSize;
    OpFontSize_b:= OpFontSize;
    OpFontSize_bi:= OpFontSize;
    OpFontSize_original:= OpFontSize;
    OpFontSize_original_i:= OpFontSize;
    OpFontSize_original_b:= OpFontSize;
    OpFontSize_original_bi:= OpFontSize;

    OpFontQuality:= fqDefault;
    OpFontLigatures:= true;
    OpFlickerReducingPause:= 0;

    OpScrollbarsNew:= true;
    OpSpacingY:= 1;

    OpTabSize:= 4;
    OpTabSpaces:= false;
    OpTabSmart:= false;

    OpMaxLineLenForBracketFinder:= 1000;

    OpActiveBorderRaw:= 1;
    OpActiveBorderInControls:= true;
    OpActiveBorderInEditor:= false;
    OpActiveBorderWidth:= 1;

    OpOverwriteSel:= true;
    OpOverwriteOnPaste:= false;
    OpPasteWithEolAtLineStart:= false; //maybe change it later to True (like Sublime, VSCode)

    OpAutoCloseBracketsMultiCarets:= true; //must be True, issue #3235
    OpAutoCloseBrackets:= '([{';
    OpAutocompleteAutoshowCharCount:= 0;
    OpAutocompleteTriggerChars:= '';
    OpAutocompleteCommitChars:= ',;';
    OpAutocompleteCommitOnEnter:= true;
    OpAutocompleteCloseChars:= '<>()[]{}=';
    OpAutocompleteAddOpeningBracket:= true;
    OpAutocompleteUpDownAtEdge:= 1; //cudWrap
    OpAutocompleteCommitIfSingleItem:= false;

    OpUnderlineColorFiles:= '*';
    OpUnderlineColorSize:= 3;
    OpLinks:= true;
    OpLinksRegex:= TATSynEdit.cUrlRegexInitial;

    OpGutterShow:= true;
    OpGutterFold:= true;
    OpGutterFoldAlways:= true;
    OpGutterBookmarks:= true;
    OpGutterLineStates:= true;
    OpGutterFoldIcons:= 0;
    OpGutterIconSize:= 4;

    OpNumbersShow:= true;
    OpNumbersStyle:= Ord(TATEditorNumbersStyle.All);
    OpNumbersForCarets:= false;
    OpNumbersCenter:= true;

    OpRulerShow:= false;
    OpRulerNumeration:= 0;
    OpRulerMarkCaret:= 1;
    OpRulerHeight:= 100;

    OpMinimapShow:= false;
    OpMinimapShowSelAlways:= false;
    OpMinimapShowSelBorder:= true;
    OpMinimapCharWidth:= 0;
    OpMinimapAtLeft:= false;
    OpMinimapScale:= 0;
    OpMinimapTooltipShow:= false;
    OpMinimapTooltipHeight:= 6;
    OpMinimapTooltipWidth:= 60;
    OpMinimapTooltipFontSize:= 0;
    OpMinimapDragImmediately:= false;

    OpMicromapShow:= false;
    OpMicromapOnScrollbar:= false;
    OpMicromapLineStates:= true;
    OpMicromapSelections:= true;
    OpMicromapBookmarks:= false;
    OpMicromapSmallMarkSizePercents:= 50;
    OpMicromapMinMarkHeight:= 4;

    OpMarginFixed:= 2000; //hide margin
    OpMarginString:= '';

    OpMarkerSize:= 30;
    OpStaplesStyle:= 1; //Ord(cLineStyleSolid)
    OpStaplesProps:= '-1,40,1,1';
    OpStapleIndentConsidersEnd:= true;

    OpUnprintedShow:= false;
    OpUnprintedContent:= 'se';

    OpWrapMode:= 0;
    OpWrapIndented:= true;
    OpWrapEnabledMaxLines:= 60*1000;

    OpUndoLimit:= TATSynEdit.cInitUndoLimit;
    OpUndoGrouped:= true;
    OpUndoAfterSave:= true;
    OpUndoMaxCarets:= TATSynEdit.cInitUndoMaxCarets;
    OpUndoIndentVert:= -5;
    OpUndoIndentHorz:= 10;
    OpUndoPause:= 300;
    OpUndoMouseClicks:= false;

    OpCaretBlinkTime:= TATSynEdit.cInitCaretBlinkTime;
    OpCaretBlinkEn:= true;
    OpCaretViewNormal:= '2,-100';
    OpCaretViewOverwrite:= '-100,-100';
    OpCaretViewReadonly:= '-100,2';
    OpCaretVirtual:= false;
    OpCaretMulti:= true;
    OpCaretAfterPasteColumn:= Ord(TATEditorPasteCaret.ColumnRight);
    OpCaretsAddedToColumnSel:= true;
    OpCaretKeepVisibleOnScroll:= true;
    OpCaretsPrimitiveColumnSel:= true;
    OpCaretProximityVert:= 0;
    OpCaretOnLoadingLimitByLineEnds:= true;

    OpKeepSelFontColor:= false;
    OpShowCurLine:= false;
    OpShowCurLineMinimal:= true;
    OpShowCurLineOnlyFocused:= false;
    OpShowCurCol:= false;
    OpShowLastLineOnTop:= false;
    OpShowFullBackgroundSel:= false;
    OpShowFullBackgroundSyntax:= true;
    OpShowMouseSelFrame:= true;
    OpShowIndentLines:= true;
    OpCopyLineIfNoSel:= true;
    OpCutLineIfNoSel:= false;
    OpCopyColumnAlignedBySpaces:= true;
    OpSavingTrimSpaces:= false;
    OpSavingTrimFinalEmptyLines:= false;
    OpSavingForceFinalEol:= false;
    OpShowHintOnVertScroll:= false;
    OpSmoothScroll:= true;
    OpCenteringWidth:= 0;
    OpCenteringForDistractionFree:= 0;
    OpScrollStyleHorz:= 2; //hide, show, auto
    OpLexerDynamicHiliteMaxLines:= 4000;
    OpLexerLineSeparators:= false;
    OpZebra:= 0;
    OpZebraStep:= 2;
    OpDimUnfocused:= 0;
    OpCommandLogMaxCount:= 200;

    OpNonWordChars:= ATEditorOptions.DefaultNonWordChars;
    OpFoldStyle:= 1;
    OpFoldTooltipShow:= false;
    OpFoldIconForMinimalRangeHeight:= 0;

    OpIndentAuto:= true;
    OpIndentAutoKind:= Ord(TATEditorAutoIndentKind.AsPrevLine);
    OpIndentSize:= 0;
    OpIndentAutoRule:= '';
    OpUnIndentKeepsAlign:= false;
    OpIndentMakesWholeLineSel:= false;

    OpMouseGotoDefinition:= 'ca';
    OpMouse2ClickDragSelectsWords:= true;
    OpMouseDragDrop:= true;
    OpMouseMiddleClickAction:= Ord(TATEditorMiddleClickAction.Scrolling);
    OpMouseRightClickMovesCaret:= false;
    OpMouseEnableColumnSelection:= true;
    OpMouseHideCursorOnType:= false;
    OpMouseGutterClickSelectedLine:= true;
    OpMouseWheelZoom:= true;
    OpMouseWheelSpeedVert:= 3;
    OpMouseWheelSpeedHorz:= 10;
    OpMouseClickNumberSelectsEol:= true;
    OpMouseClickLinks:= 2;

    OpKeyBackspaceUnindent:= true;
    OpKeyBackspaceWrap:= true;
    OpKeyTabIndents:= true;
    OpKeyHomeToNonSpace:= true;
    OpKeyHomeEndNavigateWrapped:= true;
    OpKeyEndToNonSpace:= true;
    OpKeyPageKeepsRelativePos:= true;
    OpKeyPageUpDownSize:= Ord(TATEditorPageDownSize.FullMinus1);
    OpKeyUpDownKeepColumn:= true;
    OpKeyUpDownNavigateWrapped:= true;
    OpKeyUpDownAllowToEdge:= false;
    OpKeyLeftRightGoToNextLineWithCarets:= true;
    OpKeyLeftRightSwapSel:= true;
    OpKeyLeftRightSwapSelAndSelect:= false;

    OpBracketHilite:= true;
    OpBracketSymbols:= '()[]{}';
    OpBracketDistance:= 150;
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

{$ifdef windows}
procedure Win32GetUserFont(var AFontName: string; var AFontSize: Integer);
// https://github.com/Alexey-T/CudaText/issues/3039
var
  lf: LOGFONT;
begin
  ZeroMemory(@lf, SizeOf(lf));
  if SystemParametersInfo(SPI_GETICONTITLELOGFONT, SizeOf(lf), @lf, 0) then
  begin
    AFontName := PChar(Addr(lf.lfFaceName[0]));
    AFontSize := MulDiv(-lf.lfHeight, 72, GetDeviceCaps(GetDC(0), LOGPIXELSY));
  end;
end;
{$endif}

procedure InitUiOps(var Op: TUiOps);
var
  element: TAppHistoryElement;
begin
  with Op do
  begin
    VarFontName:= 'default';
    VarFontSize:= 9;

    OutputFontName:= EditorOps.OpFontName;
    OutputFontSize:= EditorOps.OpFontSize;

    DoubleBuffered:= IsDoubleBufferedNeeded;

    CommandTimerInterval:= 50;
    PyCommandSleepInterval:= CommandTimerInterval+40;

    LexerThemes:= true;
    LexerMenuGrouped:= true;
    LexerPostponeUntilShown:= true;
    LexerParsingMinTimeForEvent:= 600;

    SidebarShow:= true;
    SidebarOnRight:= false;
    SidebarTheme:= 'common_20x20';
    SidepanelOnStart:= 0;
    BottomOnStart:= 0;
    TreeTheme:= 'default_16x16';
    ToolBarTheme:= 'default_24x24';

    LangName:= '';
    ThemeUi:= '';
    ThemeSyntax:= '';
    ThemeUi_Loaded:= false;
    ThemeSyntax_Loaded:= false;

    ThemedMainMenu:= true;
    ThemedMainMenuFontName:= 'default';
    ThemedMainMenuFontSize:= 9;
    {$ifdef windows}
    if Win32MajorVersion<6 then //Win XP
      ThemedMainMenuFontSize:= 8;
    Win32GetUserFont(ThemedMainMenuFontName, ThemedMainMenuFontSize);
    {$endif}

    AutocompleteAcpFiles:= true;
    AutocompleteHtml:= true;
    AutocompleteHtml_AutoClose:= true;
    AutocompleteHtml_Lexers:= '.*HTML.*|\bPHP\b';
    AutocompleteCss:= true;
    AutocompleteCss_Lexers:= 'CSS';
    AutocompleteFileURI:= true;
    AutocompleteInComments:= false;
    AutocompleteInCommentsHTML:= true;
    AutocompleteInStrings:= true;
    AutocompleteClosingDelay:= 300;

    HtmlBackgroundColorPair[false]:= $F0F0F0;
    HtmlBackgroundColorPair[true]:= $101010;
    CharMapFontIncreasing:= 150;

    PictureTypes:= 'bmp,png,jpg,jpeg,gif,ico,webp,psd,tga,cur';

    DefaultTabSplitIsHorz:= false;
    MaxFileSizeToOpen:= 500;
    MaxFileSizeForLexer:= 2;
    MaxFileSizeWithoutProgressForm:= 10*1024*1024;
    MaxStatusbarMessages:= 35; //Linux gtk2 shows maximal ~38 lines in tooltip
    MaxMaxStatusbarMessages:= 35;
    MaxUndoSizeForSessionFile:= 1000000;

    ListboxCentered:= true;
    ListboxSizeX:= 450;
    ListboxSizeY:= 300;
    ListboxAutoCompleteWidth:= 550;
    ListboxAutoCompleteMaxItems:= 12;
    ListboxFuzzySearch:= true;
    ListboxHotkeyFontSizeDelta:= 0; //2 gives too small hotkey font on Lin/Win
    ListboxTopItemIndent:= 4; //listbox TopItem will be ItemIndex-N

    TabsShowFoldersSuffix:= true;
    TabsShowFoldersMaxLevels:= 3;
    TabsResetUntitledCounter:= true;
    TabsDisabled:= false;
    TabVarWidth:= false;
    TabMultiline:= false;
    TabAngled:= false;
    TabFlat:= false;
    TabWidth:= 170;
    TabWidthMin:= 40;
    TabWidthMax:= 300;
    TabHeight:= 26;
    TabHeightInner:= TabHeight-1;
    TabSpacer:= 2;
    TabSpaceBeforeText:= 6;
    TabSpaceAfterText:= 6;
    TabPosition:= 0;
    TabColorFull:= false;
    TabFontScale:= 100;
    TabShowX:= 1; //show all
    TabShowXSize:= 14;
    TabShowXRounded:= true;
    TabShowPlus:= true;
    TabDblClickClose:= false;
    TabNumbers:= false;
    TabNewNearCurrent:= false;
    TabRecentOnClose:= false;
    TabButtonLayout:= '<>,v';
    TabPreviewFontStyle:= 'iu';
    TabSwitcherDialog:= false;

    MaxHistoryEdits:= 20;
    MaxHistoryMenu:= 10;
    MaxHistoryFiles:= 25;

    CmdPaletteFilterKeep:= false;
    CmdPaletteFilterText:= '';

    HistoryDisabledStr:= '';
    for element:= Low(element) to High(element) do
      HistoryItems[element]:= true;

    FindSuggestSel:= true;
    FindSuggestWord:= false;
    FindSuggestInSelection:= false;
    FindCurrentWordCaseSensitive:= ufcsCaseFromDialog;
    FindShowNoResultsByInputBgColor:= true;

    FindHiddenButtons:= '';
    FindShow_FindFirst:= true;
    FindShow_FindNext:= true;
    FindShow_FindPrev:= true;
    FindShow_ReplaceAll:= true;
    FindShow_ReplaceGlobal:= true;
    FindShow_RegEx:= true;
    FindShow_CaseSens:= true;
    FindShow_WholeWords:= true;
    FindShow_Wrapped:= true;
    FindShow_InSel:= true;
    FindShow_MultiLine:= true;
    FindShow_SyntaxElements:= true;
    FindShow_HiAll:= true;
    FindShow_ConfirmRep:= true;
    FindShow_RegexSubst:= true;
    FindShow_PreserveCase:= true;

    FindIndentVert:= -5;
    FindIndentHorz:= 10;
    FindMultiLineScale:= 2.5;
    FindSeparateForm:= false;
    FindHiAll_MaxLines:= 1000;
    FindHiAll_TagValue:= 99; //GET_UNIQUE_TAG starts with 120
    FindOccur_TagValue:= 98;
    FindWrapAtEdge_Delay:= 350;
    FindWrapAtEdge_ThemeItem:= ''; //'EdMarkedRangeBg';

    AllowProgramUpdates:= true;
    EscapeClose:= false;
    EscapeCloseConsole:= true;
    EscapeCloseFinder:= true;
    ConsoleWordWrap:= true;
    InputHeight:= 26;
    InitialDir:= '';
    OpenDir:= 'pflih';
    ConfirmLinksClicks:= true;
    ConfirmSaveEmptyUntitledTab:= false;
    SplittersUsePoorStyle:= true;

    ExportHtmlNumbers:= false;
    ExportHtmlFontSize:= 12;
    ExportHtmlFontName:= '';

    TreeFontScale:= 100;
    TreeAutoSync:= true;
    TreeTimeFill:= 2000;
    //TreeTimeCaret:= 300;
    TreeShowIcons:= true;
    TreeShowTooltips:= {$if defined(LCLQt5) or defined(LCLQt6)} false; {$else} true; {$endif} //solve issue #3642
    TreeFilterLayout:= 1;
    TreeSublexers:= false;
    TreeIconFilenames:= 'dir,st1,st2,st3,box,fx,ar1,ar2,';
    TreeFillMaxTime:= 1000;
    TreeFillMaxTimeForAPI:= 6*1000;

    PyLibrary:= '';
    PyCaretSlow:= 600;
    PyChangeSlow:= 2000;
    PyOutputCopyToStdout:= false;

    MaxLineLenForEditingKeepingLexer:= 2000;
    MaxSizeForSession:= 50*1000*1000;
    MaxLinesForMicromapPaint:= 300*1024;

    InfoAboutOptionsEditor:= true;
    AllowRunPkExec:= true;
    AllowCheckConfigsForNullBytes:= true;

    LogPluginIniting:= true;
    LogSessions:= true;
    //LogDebug:= false;
    LogConsole:= false;
    LogConsoleDetailedStartupTime:= false;

    NewdocLexer:= '';
    NewdocEnc:= 'utf8';
    NewdocEnds:= 0;

    ViewerBinaryWidth:= 100;
    ViewerNonPrintable:= false;

    StatusNoSel:= '{_ln} {y}, {_col} {xx}';
    StatusSmallSel:= '{_ln} {y}, {_col} {xx}, {_sel}';
    StatusStreamSel:= '{_ln} {y}, {_col} {xx}, {sel} {_linesel}';
    StatusColSel:= '{sel}x{cols} {_sel}';
    StatusCarets:= '{carets} {_carets}, {sel} {_linesel}';

    StatusPanels:= 'caret,C,180|enc,C,125|ends,A,|lexer,C,140|tabsize,A,|selmode,A,|msg,L,';
    StatusTime:= 5;
    StatusHeightPercents:= 180;
    StatusHeightMin:= 20;

    AltTooltipTime:= 9;
    AltTooltipTimeMax:= 60;
    AltTooltipPaddingX:= 6;
    AltTooltipPaddingY:= 3;

    ProgressbarWidth:= 50;
    ProgressbarHeightSmall:= 6;

    ShowMenubar:= true;
    ShowStatusbar:= true;
    ShowToolbar:= false;
    ShowTitlePath:= false;
    ShowSidebarMenuButton:= 2;

    ReopenSession:= true;
    ReopenSessionWithCmdLine:= true;
    SessionSaveInterval:= 30;
    SessionSaveOnExit:= true;
    BackupLastSessions:= 2;
    SaveModifiedTabsOnClose:= true;

    ShowFormsOnTop:= false;
    ShowMenuDialogsWithBorder:= {$ifdef LCLGTK2} true {$else} false {$endif};
    UndoPersistent:= '';
    AllowSaveOfUnmodifiedFile:= true;

    PluginDialogsShowInTaskbar:= {$ifdef windows} false {$else} true {$endif}; //to fix issue #3078 on Linux
    PluginDialogsModalFormStyle:= fsStayOnTop; //issue #4221, before it was: {$ifdef LCLQT5} fsNormal {$else} fsStayOnTop {$endif};

    FloatGroupsShowInTaskbar:= stAlways;
    OneInstance:= true;

    NotificationEnabled:= true;
    NotificationTimeSeconds:= 2;
    NotificationConfirmReload:= 1;
    NotificationPanelHeight:= 31;
    NotificationButtonHeight:= 25;
    NotificationButtonsDistance:= 4;

    NonTextFiles:= 0;
    NonTextFilesBufferKb:= 64;
    ReloadFollowTail:= true;
    ReloadUnsavedConfirm:= true;
    CheckLowDiskSpace:= 1*1024*1024;
    FullScreen:= 'tp';

    Emmet_AddSlashToEmptyTags:= true;
    Emmet_CommentTags:= false;
    Emmet_IndentNested:= true;
    Emmet_SingleLine:= false;
    Emmet_TrimLineMarkers:= true;
    Emmet_WordWrap:= false;

    HotkeyFindDialog:= 'Ctrl+F';
    HotkeyReplaceDialog:= 'Ctrl+R';
    HotkeyFindFirst:= 'Alt+Enter';
    HotkeyFindNext:= 'F3';
    HotkeyFindPrev:= 'Shift+Enter';
    HotkeyReplaceAndFindNext:= 'Ctrl+Alt+Z';
    HotkeyReplaceNoFindNext:= 'Ctrl+Alt+Shift+Z';
    HotkeyReplaceAll:= 'Ctrl+Alt+A';
    HotkeyReplaceGlobal:= '';
    HotkeyCountAll:= 'Ctrl+Alt+O';
    HotkeyExtractAll:= 'Ctrl+Alt+Q';
    HotkeySelectAll:= 'Ctrl+Alt+E';
    HotkeyMarkAll:= 'Ctrl+Alt+K';
    HotkeyToggleRegex:= 'Ctrl+Alt+R';
    HotkeyToggleCaseSens:= 'Ctrl+Alt+C';
    HotkeyToggleWords:= 'Ctrl+Alt+W';
    HotkeyToggleWrapped:= 'Ctrl+Alt+N';
    HotkeyToggleInSelect:= 'Ctrl+Alt+X';
    HotkeyToggleMultiline:= 'Ctrl+Alt+M';
    HotkeyToggleConfirmRep:= 'Ctrl+Alt+Y';
    HotkeyToggleTokens:= '';
    HotkeyToggleHiAll:= '';
    HotkeyTogglePresCase:= '';
    HotkeyFindMenu:= 'Ctrl+Alt+D';
  end;
end;


function Lexer_EscapeFilename(const ALexName: string): string;
const
  cBadFilenameChars = '/\*:<>';
var
  i: integer;
begin
  Result:= ALexName;
  for i:= 1 to Length(Result) do
    if Pos(Result[i], cBadFilenameChars)>0 then
      Result[i]:= '_';
end;

function AppFile_LexerSpecificConfig(ALexer: string; ADefaultConfig: boolean=false): string;
var
  dir: string;
begin
  //support none-lexer here
  if ALexer='' then
    ALexer:= '-';
  ALexer:= Lexer_EscapeFilename(ALexer);

  if ADefaultConfig then
    dir:= AppDir_SettingsDefault
  else
    dir:= AppDir_Settings;

  Result:= dir+DirectorySeparator+'lexer '+ALexer+'.json';
end;

function AppFile_IsIgnoredForSession(const AFilename: string): boolean;
begin
  if SameFileName(AFilename, AppFile_History) then
    exit(true);

  if SameFileName(AFilename, AppFile_HistoryFiles) then
    exit(true);

  if SameFileName(AFilename, AppDir_Settings+DirectorySeparator+'history session.json') then
    exit(true);

  Result:= false;
end;

function AppFile_HotkeysForLexer(AName: string): string;
begin
  //support none-lexer
  if AName='' then
    AName:= '-';
  AName:= Lexer_EscapeFilename(AName);
  Result:= AppDir_Settings+DirectorySeparator+'keys lexer '+AName+'.json';
end;

class function TPluginHelper.HotkeyStringId_To_CommandCode(const AId: string): integer;
begin
  //plugin item 'module,method'
  if Pos(',', AId)>0 then
  begin
    Result:= CommandGetIndexFromModuleAndMethod(AId);
    if Result>=0 then
      Inc(Result, cmdFirstPluginCommand);
  end
  else
  begin
    //str(number) item
    Result:= StrToIntDef(AId, -1);

    //for broken keys config which has incorrect int keys, issue #4590
    if (Result>=cmdFirstPluginCommand) and (Result<=cmdLastPluginCommand) then
    begin
      MsgLogConsole(Format('ERROR: Hotkeys config: bad key "%d", in the range [%d, %d]', [Result, cmdFirstPluginCommand, cmdLastPluginCommand]));
      Result:= -1;
    end;
  end;
end;

class function TPluginHelper.Debug_PluginCommands(const AModule: string): string;
var
  CmdItem: TAppCommandInfo;
  i: integer;
begin
  Result:= '';
  for i:= 0 to AppCommandList.Count-1 do
  begin
    CmdItem:= TAppCommandInfo(AppCommandList[i]);
    if CmdItem.ItemModule=AModule then
      Result+= CmdItem.CommaStr+#10;
  end;
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
  if not FileExists(AFilename) then exit;

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


function Lexer_DetectByFilenameOrContent(const AFilename: string;
  AChooseFunc: TecLexerChooseFunc): TecSyntAnalyzer;
const
  cSignUTF8: string = #$EF#$BB#$BF;
var
  SNameOnly: string;
  ext, sLine, res: string;
  i: integer;
begin
  SNameOnly:= ExtractFileName(AFilename);

  //detect by filename
  res:= AppConfig_Detect.GetValue(SNameOnly, '');
  if res<>'' then
    exit(AppManager.FindLexerByName(res));

  //detect by double extention
  if SFindCharCount(SNameOnly, '.')>1 then
  begin
    i:= RPos('.', SNameOnly);
    i:= RPosEx('.', SNameOnly, i-1);
    ext:= Copy(SNameOnly, i, MaxInt);
    if ext<>'' then
    begin
      res:= AppConfig_Detect.GetValue('*'+ext, '');
      if res<>'' then
        exit(AppManager.FindLexerByName(res));
    end;
  end;

  //detect by usual extention
  ext:= ExtractFileExt(SNameOnly);
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
      //skip UTF8 signature, needed for XMLs
      if SBeginsWith(sLine, cSignUTF8) then
        System.Delete(sLine, 1, Length(cSignUTF8));
      res:= AppConfig_DetectLine.GetValueByRegex(sLine, true);
      if res<>'' then
        exit(AppManager.FindLexerByName(res));
    end;
  end;

  Result:= AppManager.FindLexerByFilename(AFilename, AChooseFunc);
end;

class function TPluginHelper.CommandCode_To_HotkeyStringId(ACmd: integer): string;
begin
  if CommandCategory(ACmd) in [categ_Plugin, categ_PluginSub] then
    Result:= TAppCommandInfo(AppCommandList[ACmd-cmdFirstPluginCommand]).CommaStr
  else
    Result:= IntToStr(ACmd);
end;

class procedure TKeymapHelper.ItemSaveToConfig(K: TATKeymapItem; const path, ALexerName: string;
  ALexerSpecific: boolean);
var
  SFilename: string;
  c: TJSONConfig;
  sl: TStringList;
  i: integer;
begin
  if ALexerSpecific then
    SFilename:= AppFile_HotkeysForLexer(ALexerName)
  else
    SFilename:= AppFile_Hotkeys;

  c:= TJSONConfig.Create(nil);
  sl:= TStringlist.create;
  try
    try
      c.Formatted:= true;
      c.Filename:= SFilename;
    except
      exit;
    end;

    c.SetValue(path+'/name', K.Name);

    sl.Clear;
    for i:= 0 to High(TATKeyArray.Data) do
      if K.Keys1.Data[i]<>0 then
        sl.Add(ShortCutToText(K.Keys1.Data[i]));
    c.SetValue(path+'/s1', sl);

    sl.clear;
    for i:= 0 to High(TATKeyArray.Data) do
      if K.Keys2.Data[i]<>0 then
        sl.Add(ShortCutToText(K.Keys2.Data[i]));

    if sl.Count>0 then
      c.SetValue(path+'/s2', sl)
    else
      c.DeleteValue(path+'/s2')
  finally
    c.Free;
    sl.Free;
  end;
end;


class procedure TKeymapHelper.ItemDeleteInConfig(K: TATKeymapItem; const path, ALexerName: string;
  ALexerSpecific: boolean);
var
  SFilename: string;
  c: TJSONConfig;
begin
  if ALexerSpecific then
    SFilename:= AppFile_HotkeysForLexer(ALexerName)
  else
    SFilename:= AppFile_Hotkeys;

  c:= TJSONConfig.Create(nil);
  try
    try
      c.Formatted:= true;
      c.Filename:= SFilename;
    except
      exit;
    end;

    if c.GetValue(path+'/name', '')<>'' then
      c.DeletePath(path);
  finally
    c.Free;
  end;
end;


class function TKeymapHelper.SaveKey_ForPlugin(AKeymap: TATKeymap;
  AOverwriteKey: boolean;
  const AMenuitemCaption, AModuleName, AMethodName, ALexerName, AHotkey: string): boolean;
const
  cKeyComboSeparator = '|';
var
  SFilename: string;
  c: TJSONConfig;
  sl: TStringList;
  path, s_item: string;
  Sep: TATStringSeparator;
begin
  Result:= false;

  if ALexerName<>'' then
    SFilename:= AppFile_HotkeysForLexer(ALexerName)
  else
    SFilename:= AppFile_Hotkeys;

  c:= TJSONConfig.Create(nil);
  sl:= TStringlist.create;
  try
    try
      c.Formatted:= true;
      c.Filename:= SFilename;
    except
      exit;
    end;

    path:= AModuleName+','+AMethodName;

    //check: this command has already any hotkey?
    if not AOverwriteKey then
      if c.GetValue(path+'/s1', sl, '') then exit;

    c.SetValue(path+'/name', AMenuitemCaption);

    sl.Clear;
    Sep.Init(AHotkey, cKeyComboSeparator);
    while Sep.GetItemStr(s_item) do
      sl.Add(s_item);

    c.SetValue(path+'/s1', sl);
    Result:= true;
  finally
    c.Free;
    sl.Free;
  end;
end;

function AppListboxItemHeight(AScale, ADoubleHeight: boolean): integer;
begin
  Result:= CanvasFontSizeToPixels(UiOps.VarFontSize);

  {$ifdef windows}
  Result:= Result * Screen.PixelsPerInch div 96;
  {$endif}

  if ADoubleHeight then
    Result:= Result * 185 div 100;
  if AScale then
    Result:= ATEditorScaleFont(Result);
end;


procedure Lexer_EnumAll(L: TStringList; AlsoDisabled: boolean = false);
var
  an: TecSyntAnalyzer;
  i: integer;
begin
  with AppManager do
    for i:= 0 to LexerCount-1 do
    begin
      an:= Lexers[i];
      if an.Deleted then Continue;
      if an.Internal and not AlsoDisabled then Continue;
      L.Add(an.LexerName);
    end;

  with AppManagerLite do
    for i:= 0 to LexerCount-1 do
      L.Add(Lexers[i].LexerName+msgLiteLexerSuffix);
end;


function Lexer_IsNameCorrect(AName: string): boolean;
begin
  if AName='' then
    exit(true);
  if SEndsWith(AName, msgLiteLexerSuffix) then
  begin
    SetLength(AName, Length(AName)-Length(msgLiteLexerSuffix));
    Result:= AppManagerLite.FindLexerByName(AName)<>nil;
  end
  else
  begin
    Result:= AppManager.FindLexerByName(AName)<>nil;
  end;
end;


class function TPluginHelper.CommandGetIndexFromModuleAndMethod(const AText: string): integer;
var
  Sep: TATStringSeparator;
  SModule, SProc, SProcParam: string;
  AppCmd: TAppCommandInfo;
  i: integer;
begin
  Result:= -1;

  Sep.Init(AText);
  Sep.GetItemStr(SModule);
  Sep.GetItemStr(SProc);
  Sep.GetItemStr(SProcParam);

  if SModule='' then exit;
  if SProc='' then exit;

  for i:= 0 to AppCommandList.Count-1 do
  begin
    AppCmd:= TAppCommandInfo(AppCommandList[i]);
    if (AppCmd.ItemModule=SModule) and
      (AppCmd.ItemProc=SProc) and
      (AppCmd.ItemProcParam=SProcParam) then
      exit(i);
  end;
end;


class procedure TPluginHelper.CommandUpdateSubcommands(const AText: string);
const
  cSepRoot=';';
  cSepParams=#10;
  cSepNameParam=#9;
var
  Sep: TATStringSeparator;
  SModule, SProc, SParams, SItem, SItemParam, SItemCaption: string;
  CmdItem: TAppCommandInfo;
  N: integer;
begin
  Sep.Init(AText, cSepRoot);
  Sep.GetItemStr(SModule);
  Sep.GetItemStr(SProc);
  Sep.GetRest(SParams);

  //del items for module/method
  for N:= AppCommandList.Count-1 downto 0 do
    with TAppCommandInfo(AppCommandList[N]) do
      if (ItemModule=SModule) and (ItemProc=SProc) and (ItemProcParam<>'') then
        AppCommandList.Delete(N);

  //add items for SParams
  Sep.Init(SParams, cSepParams);
  repeat
    if not Sep.GetItemStr(SItem) then Break;
    SSplitByChar(SItem, cSepNameParam, SItemCaption, SItemParam);

    CmdItem:= TAppCommandInfo.Create;
    CmdItem.ItemModule:= SModule;
    CmdItem.ItemProc:= SProc;
    CmdItem.ItemProcParam:= SItemParam;
    CmdItem.ItemCaption:= SItemCaption;
    CmdItem.ItemFromApi:= true;

    if AppCommandList.Count>(cmdLastPluginCommand-cmdFirstPluginCommand) then
      MsgLogConsole('ERROR: Too many plugin commands');
    AppCommandList.Add(CmdItem);
  until false;
end;


function AppFile_Language: string;
begin
  if UiOps.LangName='' then
    Result:= ''
  else
    Result:= AppDir_DataLang+DirectorySeparator+UiOps.LangName+'.ini';
end;

function AppFile_LexerExtension(ALexName, AExt: string): string;
begin
  if ALexName<>'' then
    Result:= AppDir_Lexers+DirectorySeparator+Lexer_EscapeFilename(ALexName)+AExt
  else
    Result:= '';
end;

function AppFile_LexerMap(const ALexName: string): string;
begin
  Result:= AppFile_LexerExtension(ALexName, '.cuda-lexmap');
end;

function AppFile_Lexer(const ALexName: string): string;
begin
  Result:= AppFile_LexerExtension(ALexName, '.lcf');
end;

function AppFile_LexerOps(const ALexName: string): string;
begin
  Result:= AppDir_Settings+DirectorySeparator+Lexer_EscapeFilename(ALexName)+'.cuda-lexops';
end;

function AppFile_LexerAcp(const ALexName: string): string;
begin
  Result:= AppDir_DataAutocomplete+DirectorySeparator+Lexer_EscapeFilename(ALexName)+'.acp';
end;

function AppFile_UndoRedo(const fn: string; IsRedo: boolean): string;
const
  Ext: array[boolean] of string = ('.undx', '.redx');
begin
  Result:= ExtractFileDir(fn)+DirectorySeparator+
    '.cudatext'+DirectorySeparator+
    ExtractFileName(fn)+Ext[IsRedo];
end;

function AppSessionName_ForHistoryFile: string;
var
  sDir, sFilename, sJsonPath: string;
begin
  if not UiOps.ReopenSession then exit('');

  SSplitByChar(AppSessionName, '|', sFilename, sJsonPath);

  sDir:= ExtractFileDir(sFilename);
  if sDir='' then exit(AppSessionName);

  if SameFileName(sDir, AppDir_Settings) then
    Result:= ExtractFileName(sFilename)+IfThen(sJsonPath<>'', '|'+sJsonPath)
  else
    Result:= AppSessionName;
end;

class function TKeymapHelper.GetHotkey(AKeymap: TATKeymap; const ACmdString: string): string;
var
  NCode, NIndex: integer;
begin
  Result:= '';
  if Pos(',', ACmdString)=0 then
    NCode:= StrToIntDef(ACmdString, 0)
  else
  begin
    NIndex:= TPluginHelper.CommandGetIndexFromModuleAndMethod(ACmdString);
    if NIndex<0 then exit;
    NCode:= NIndex+cmdFirstPluginCommand;
  end;

  NIndex:= AKeymap.IndexOf(NCode);
  if NIndex<0 then exit;
  with AKeymap[NIndex] do
    Result:= Keys1.ToString+'|'+Keys2.ToString;
end;


class function TKeymapHelper.SetHotkey(AKeymap: TATKeymap; const AParams: string; AndSaveFile: boolean): boolean;
var
  Sep: TATStringSeparator;
  NCode, NIndex: integer;
  SCmd, SKey1, SKey2: string;
begin
  Result:= false;

  Sep.Init(AParams, '|');
  Sep.GetItemStr(SCmd);
  Sep.GetItemStr(SKey1);
  Sep.GetItemStr(SKey2);

  if Pos(',', SCmd)=0 then
    NCode:= StrToIntDef(SCmd, 0)
  else
  begin
    NIndex:= TPluginHelper.CommandGetIndexFromModuleAndMethod(SCmd);
    if NIndex<0 then exit;
    NCode:= NIndex+cmdFirstPluginCommand;
  end;

  NIndex:= AKeymap.IndexOf(NCode);
  if NIndex<0 then exit;
  with AKeymap[NIndex] do
  begin
    Keys1.SetFromString(SKey1);
    Keys2.SetFromString(SKey2);

    //save to keys.json
    //Py API: no need lexer-specific
    if AndSaveFile then
      ItemSaveToConfig(AKeymap[NIndex], SCmd, '', false);
  end;
  Result:= true;
end;


class procedure TKeymapHelper.ClearKey(AKeymap: TATKeymap; AItemIndex, AKeyIndex: integer);
begin
  with AKeymap do
    if IsIndexValid(AItemIndex) then
      with Items[AItemIndex] do
        if AKeyIndex=0 then
          Keys1.Clear
        else
          Keys2.Clear;
end;

class procedure TKeymapHelper.ClearKeyInAll(AItemIndex, AKeyIndex: integer);
var
  iMap: integer;
begin
  ClearKey(AppKeymapMain, AItemIndex, AKeyIndex);

  for iMap:= 0 to AppKeymapLexers.Count-1 do
    ClearKey(
      TATKeymap(AppKeymapLexers.Objects[iMap]),
      AItemIndex, AKeyIndex);
end;

class function TKeymapHelper.CheckDuplicateForCommand(
  AKeymapItem: TATKeymapItem;
  const ALexerName: string;
  AOverwriteAndSave: boolean): integer;
var
  Map: TATKeymap;
  MapItem: TATKeymapItem;
  ShortKeys1: TATKeyArray;
  ShortKeys2: TATKeyArray;
  StrId: string;
  NKeyIndex: integer;
  iCmd: integer;
begin
  Result:= 0;

  Map:= AppKeymapMain;
  NKeyIndex:= 0;

  ShortKeys1.Clear;
  ShortKeys2.Clear;
  if AKeymapItem.Keys1.Length>1 then
    ShortKeys1:= AKeymapItem.ShortenedKeys1;
  if AKeymapItem.Keys2.Length>1 then
    ShortKeys2:= AKeymapItem.ShortenedKeys2;

  for iCmd:= 0 to Map.Count-1 do
  begin
    MapItem:= Map.Items[iCmd];
    if MapItem.Command=AKeymapItem.Command then Continue;

    if (AKeymapItem.Keys1.IsConflictWith(MapItem.Keys1)) or
       (AKeymapItem.Keys2.IsConflictWith(MapItem.Keys1)) then
       NKeyIndex:= 0
    else
    if (AKeymapItem.Keys1.IsConflictWith(MapItem.Keys2)) or
       (AKeymapItem.Keys2.IsConflictWith(MapItem.Keys2)) then
       NKeyIndex:= 1
    else
      Continue;

    if AOverwriteAndSave then
    begin
      //clear item in ALL existing keymaps
      ClearKeyInAll(iCmd, NKeyIndex);

      StrId:= TPluginHelper.CommandCode_To_HotkeyStringId(MapItem.Command);

      //save to "keys.json"
      ItemSaveToConfig(MapItem, StrId, '', false);

      //save to "keys nn.json"
      if ALexerName<>'' then
        ItemSaveToConfig(MapItem, StrId, ALexerName, true);
    end
    else
      Result:= MapItem.Command;

    Break;
  end;
end;

class procedure TKeymapHelper.LoadConfig(AKeymap: TATKeymap; const AFileName: string; AForLexer: boolean);
var
  cfg: TJSONConfig;
  slist, skeys: TStringList;
  //
  procedure DoReadConfigToKeys(const path: string; var keys: TATKeyArray);
  var
    j: integer;
  begin
    keys.Clear;
    cfg.GetValue(path, skeys, '');
    for j:= 0 to skeys.count-1 do
    begin
      if skeys[j]<>'' then
        keys.Data[j]:= TextToShortCut(skeys[j]);
    end;
  end;
  //
var
  StrId: string;
  ncmd, nitem, i: integer;
begin
  cfg:= TJSONConfig.Create(nil);
  slist:= TStringList.Create;
  skeys:= TStringList.Create;

  try
    try
      cfg.Formatted:= true;
      cfg.Filename:= AFileName;
    except
      exit;
    end;

    cfg.EnumSubKeys('/', slist);
    for i:= 0 to slist.count-1 do
    begin
      StrId:= slist[i];
      ncmd:= TPluginHelper.HotkeyStringId_To_CommandCode(StrId);
      if ncmd<0 then Continue;

      nitem:= AKeymap.IndexOf(ncmd);
      if nitem<0 then Continue;

      DoReadConfigToKeys(StrId+'/s1', AKeymap[nitem].Keys1);
      DoReadConfigToKeys(StrId+'/s2', AKeymap[nitem].Keys2);
      AKeymap[nitem].LexerSpecific:= AForLexer;

      { //debug
      if Pos('cuda_project_man,menu_goto', AKeymap[nitem].Description)>0 then
        ShowMessage(Format('i %d, nitem %d,'#10'name: %s'#10'slist[i]: %s', [
          i,
          nitem,
          AKeymap[nitem].Name,
          StrId
          ]));
          }
    end;
  finally
    skeys.Free;
    slist.Free;
    cfg.Free;
  end;
end;


class function TKeymapHelper.GetForLexer(const ALexer: string): TATKeymap;
var
  Keymap: TATKeymap;
  N: integer;
begin
  //we must block too early loading of keys-config.
  //it is called too early from FormShow._Init_ApiOnStart:
  //  Project Manager restores project, and restores project's _default_ session,
  //  which loads .txt file, which sets none-lexer, which loads keys-config.
  if not AppApiOnStartActivated then
    exit(AppKeymapMain);

  N:= AppKeymapLexers.IndexOf(ALexer);
  if N>=0 then
    exit(TATKeymap(AppKeymapLexers.Objects[N]));

  Keymap:= TATKeymap.Create;
  Keymap.Assign(AppKeymapMain);
  AppKeymapLexers.AddObject(ALexer, Keymap);

  LoadConfig(Keymap,
    AppFile_HotkeysForLexer(ALexer), true);

  Result:= Keymap;
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

procedure MsgLogConsole(const AText: string);
var
  Sep: TATStringSeparator;
  S: UnicodeString;
begin
  if Pos(#10, AText)=0 then
    AppConsoleQueue.Push(AText)
  else
  begin
    Sep.Init(AText, #10);
    while Sep.GetItemStr(S) do
      AppConsoleQueue.Push(S);
  end;
end;


procedure MsgLogToFilename(const AText, AFilename: string; AWithTime: boolean);
var
  f: TextFile;
  S: string;
begin
  AssignFile(f, AFileName);
  {$Push}
  {$I-}
  Append(f);
  if IOResult<>0 then
    Rewrite(f);
  S:= AText;
  if AWithTime then
    S:= FormatDateTime('[MM.DD hh:nn] ', Now) + S;
  Writeln(f, S);
  CloseFile(f);
  {$Pop}
end;

procedure MsgOldApi(const s: string);
begin
  MsgLogConsole(Format(msgApiDeprecated, [s]));
end;

procedure MsgFileFromSessionNotFound(const fn: string);
begin
  if not StartsStr(GetTempDir, fn) then
    MsgLogConsole(Format(msgCannotFindSessionFile, [AppCollapseHomeDirInFilename(fn)]));
end;

procedure AppClearPluginLists;
var
  i: integer;
begin
  for i:= AppCommandList.Count-1 downto 0 do
    TObject(AppCommandList[i]).Free;
  FreeAndNil(AppCommandList);

  for i:= AppEventList.Count-1 downto 0 do
    TObject(AppEventList[i]).Free;
  FreeAndNil(AppEventList);

  for i:= AppTreeHelpers.Count-1 downto 0 do
    TObject(AppTreeHelpers[i]).Free;
  FreeAndNil(AppTreeHelpers);
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

procedure DoStatusbarColorByTag(AStatus: TATStatus; ATag: PtrInt; AColor: TColor);
var
  NIndex: integer;
  Data: TATStatusData;
begin
  NIndex:= AStatus.FindPanel(ATag);
  if NIndex>=0 then
  begin
    Data:= AStatus.GetPanelData(NIndex);
    Data.ColorFont:= AColor;
    AStatus.Invalidate;
  end;
end;

function IsFileTooBigForOpening(const AFilename: string): boolean;
begin
  Result:= (AFilename<>'') and (FileSize(AFileName) div (1024*1024) >= UiOps.MaxFileSizeToOpen);
end;

function IsFileTooBigForLexer(const AFilename: string): boolean;
begin
  Result:= (AFilename<>'') and (FileSize(AFilename) div (1024*1024) >= UiOps.MaxFileSizeForLexer);
end;

function IsFilenameForLexerDetecter(const AFileName: string): boolean;
begin
  if IsFileTooBigForLexer(AFileName) then //fixing issue #3449
    Result:= false
  else
  if ExtractFileName(AFileName)='CMakeLists.txt' then //CMakeLists.txt is for lexer CMake
    Result:= true
  else
    Result:= LowerCase(ExtractFileExt(AFileName))<>'.txt';
end;



procedure Lexer_DetectByFilename(const AFilename: string;
  out Lexer: TecSyntAnalyzer;
  out LexerLite: TATLiteLexer;
  out LexerName: string;
  out ATooBigForLexer: boolean;
  AChooseFunc: TecLexerChooseFunc);
begin
  LexerName:= '';
  Lexer:= nil;
  LexerLite:= nil;
  ATooBigForLexer:= false;
  if AFilename='' then exit;

  if IsFileTooBigForLexer(AFilename) then
  begin
    LexerLite:= AppManagerLite.FindLexerByFilename(AFilename);
    ATooBigForLexer:= true;
  end
  else
  begin
    Lexer:= Lexer_DetectByFilenameOrContent(AFilename, AChooseFunc);
    if Lexer=nil then
      LexerLite:= AppManagerLite.FindLexerByFilename(AFilename);
  end;

  if Assigned(Lexer) then
    LexerName:= Lexer.LexerName
  else
  if Assigned(LexerLite) then
    LexerName:= LexerLite.LexerName+msgLiteLexerSuffix;
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

procedure EditorClear(Ed: TATSynEdit);
begin
  Ed.Strings.Clear;
  Ed.Strings.ActionAddFakeLineIfNeeded;
  Ed.DoCaretSingle(0, 0);
  Ed.Update(true);
  Ed.Modified:= false;
end;

{ TAppManagerThread }

procedure TAppManagerThread.Execute;
begin
  //AppManager.AllowedThreadId:= Self.ThreadID;
  AppLoadLexers;
  //AppManager.AllowedThreadId:= 0;
end;

{ TAppCommandInfo }

function TAppCommandInfo.CommaStr: string;
begin
  if ItemModule='' then
    Result:= ''
  else
  begin
    Result:= ItemModule+','+ItemProc;
    if ItemProcParam<>'' then
      Result+= ','+ItemProcParam;
  end;
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

destructor TAppKeyValues.Destroy;
var
  Item: TAppKeyValue;
  i: integer;
begin
  for i:= Count-1 downto 0 do
  begin
    Item:= TAppKeyValue(Items[i]);
    Item.Free;
  end;
  inherited Destroy;
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

function TAppKeyValues.GetValueByRegex(ALine: string; ACaseSens: boolean): string;
var
  Item: TAppKeyValue;
  i: integer;
begin
  Result:= '';
  for i:= 0 to Count-1 do
  begin
    Item:= TAppKeyValue(Items[i]);
    if SRegexMatchesString(ALine, Item.Key, ACaseSens) then
      exit(Item.Value);
  end;
end;


procedure DoMenuitemEllipsis(c: TMenuItem);
var
  s: string;
begin
  if c=nil then exit;
  s:= c.Caption;
  while (s<>'') and (s[Length(s)]='.') do
    SetLength(s, Length(s)-1);
  c.Caption:= s+'...';
end;


procedure AppUpdateWatcherFrames(AMaxWorkTime: integer = 500);
var
  Frame: TObject;
  NTick: QWord;
  NCount: integer;
begin
  //function is called in IdleTimer, so just exit if watcher thread is busy,
  //we will try this again on next timer tick
  if AppEventWatcher.WaitFor(1)<>wrSignaled then exit;

  AppEventLister.ResetEvent;
  try
    NTick:= GetTickCount64;
    repeat
      NCount:= AppFrameListDeleting.Count;
      if NCount=0 then
        Break;
      Frame:= TObject(AppFrameListDeleting[NCount-1]);

      //hide AV when user makes N>1 clicks on ui-tab X icon,
      //while this ui-tab has huge file and parsing is running
      try
        if Frame.ClassName<>'' then
          Frame.Free;
      except
      end;

      AppFrameListDeleting.Count:= NCount-1;
      if GetTickCount64-NTick>=AMaxWorkTime then
        Break;
      if Application.Terminated then
        Break;
    until false;

    AppFrameList2.Assign(AppFrameList1);
  finally
    AppEventLister.SetEvent;
    if AppFrameListDeleting.Count=0 then
      AppCommandHandlerIsBusy:= false;
  end;
end;


class function TPluginHelper.CommandCategory(Cmd: integer): TAppCommandCategory;
var
  N: integer;
begin
  case Cmd of
    cmdFirstPluginCommand..cmdLastPluginCommand:
      begin
        Result:= categ_Plugin;
        N:= Cmd-cmdFirstPluginCommand;
        if N<AppCommandList.Count then
        begin
          if TAppCommandInfo(AppCommandList[N]).ItemFromApi then
            Result:= categ_PluginSub;
        end
        else
          //we are here when e.g. in plugin Macros user deletes a macro,
          //so code detects category of deleted command-code
          Result:= categ_PluginSub;
      end;
    cmdFirstLexerCommand..cmdLastLexerCommand:
      Result:= categ_Lexer;
    cmdFirstFileCommand..cmdLastFileCommand:
      Result:= categ_OpenedFile;
    cmdFirstRecentCommand..cmdLastRecentCommand:
      Result:= categ_RecentFile;
    else
      Result:= categ_Normal;
  end;
end;

class function TPluginHelper.CommandHasConfigurableHotkey(Cmd: integer): boolean;
begin
  Result:= CommandCategory(Cmd) in [categ_Normal, categ_Plugin, categ_PluginSub];
end;

procedure InitBasicCommandLineOptions;
var
  S: string;
  i: integer;
begin
  for i:= 1 to ParamCount do
  begin
    S:= ParamStr(i);

    if (S='--version') or (S='-v') then
    begin
      MsgStdout('CudaText '+cAppExeVersion, true);
      Halt;
    end;

    if (S='--help') or (S='-h') then
    begin
      MsgStdout(msgCommandLineHelp, true);
      Halt;
    end;

    if (S='-el') then
    begin
      MsgStdout(AppEncodingListAsString, true);
      Halt;
    end;

    if S='-n' then
    begin
      AppAlwaysNewInstance:= true;
      Continue;
    end;

    if SBeginsWith(S, '-id=') then
    begin
      Delete(S, 1, Length('-id='));
      if S<>'' then
        AppServerId:= S;
      Continue;
    end;
  end;
end;

class function TPluginHelper.EventIsUsed(AEvent: TAppPyEvent): boolean;
var
  NPlugin: integer;
  Plugin: TAppEventInfo;
begin
  Result:= false;
  for NPlugin:= 0 to AppEventList.Count-1 do
  begin
    Plugin:= TAppEventInfo(AppEventList[NPlugin]);
    if AEvent in Plugin.ItemEvents then
      exit(true);
  end;
end;

class procedure TPluginHelper.EventStringToEventData(const AEventStr: string;
  out AEvents: TAppPyEvents;
  out AEventsPrior: TAppPyEventsPrior;
  out AEventsLazy: TAppPyEventsLazy);
const
  MaxPriority = 4;
var
  Sep: TATStringSeparator;
  S: string;
  event: TAppPyEvent;
  nPrior: byte;
  bLazy: boolean;
begin
  AEvents:= [];
  FillChar(AEventsPrior{%H-}, SizeOf(AEventsPrior), 0);
  FillChar(AEventsLazy{%H-}, SizeOf(AEventsLazy), 0);

  Sep.Init(AEventStr);
  while Sep.GetItemStr(S) do
  begin
    nPrior:= 0;
    while S[Length(S)]='+' do
    begin
      Inc(nPrior);
      SetLength(S, Length(S)-1);
    end;

    if nPrior>MaxPriority then
      nPrior:= MaxPriority;

    bLazy:= false;
    if S[Length(S)]='~' then
    begin
      bLazy:= true;
      SetLength(S, Length(S)-1);
    end;

    for event in TAppPyEvent do
      if S=cAppPyEvent[event] then
      begin
        Include(AEvents, event);
        AEventsPrior[event]:= nPrior;
        AEventsLazy[event]:= bLazy;
        Break
      end;
  end;
end;


class procedure TPluginHelper.EventsUpdate(const AModuleName, AEventStr, ALexerStr, AKeyStr: string);
var
  EventItem: TAppEventInfo;
  i: integer;
begin
  //find index of plugin (get first empty index if not listed)
  EventItem:= nil;
  for i:= 0 to AppEventList.Count-1 do
    with TAppEventInfo(AppEventList[i]) do
      if (ItemModule=AModuleName) then
      begin
        EventItem:= TAppEventInfo(AppEventList[i]);
        Break
      end;

  if EventItem=nil then
  begin
    EventItem:= TAppEventInfo.Create;
    AppEventList.Add(EventItem);
  end;

  //update item
  with EventItem do
  begin
    if ItemModule='' then
      ItemModule:= AModuleName;
    EventStringToEventData(AEventStr, ItemEvents, ItemEventsPrior, ItemEventsLazy);
    ItemLexers:= ALexerStr;
    ItemKeys:= AKeyStr;
  end;

  EventsMaxPrioritiesUpdate;
end;

class procedure TPluginHelper.CommandsClearButKeepApiItems;
var
  i: integer;
begin
  for i:= AppCommandList.Count-1 downto 0 do
    with TAppCommandInfo(AppCommandList[i]) do
      if (ItemModule<>'') and (not ItemFromApi) then
        AppCommandList.Delete(i);
end;

class procedure TPluginHelper.EventsMaxPrioritiesUpdate;
var
  ev: TAppPyEvent;
  Plugin: TAppEventInfo;
  Value, i: integer;
begin
  for ev in TAppPyEvent do
  begin
    Value:= -1;
    for i:= 0 to AppEventList.Count-1 do
    begin
      Plugin:= TAppEventInfo(AppEventList[i]);
      if ev in Plugin.ItemEvents then
        Value:= Max(Value, Plugin.ItemEventsPrior[ev]);
    end;
    AppEventsMaxPriorities[ev]:= Value;
  end;
end;


function IsOsFullPath(const S: string): boolean;
begin
  {$ifdef windows}
  //'D:\path'
  //'\\UNCpath'
  Result:=
    (Length(S)>2) and
    ((S[2]=':') or ((S[1]='\') and (S[2]='\')));
  {$else}
  Result:= SBeginsWith(S, '/');
  {$endif}
end;


procedure AppOnLexerLoaded(Sender: TObject; ALexer: TecSyntAnalyzer);
var
  fn_ops: string;
begin
  //load *.cuda-lexops
  fn_ops:= AppFile_LexerOps(ALexer.LexerName);
  if FileExists(fn_ops) then
    Lexer_LoadStylesFromFile_JsonLexerOps(ALexer, fn_ops, UiOps.LexerThemes);
end;

procedure AppOnLexerLoadError(const AFileName, AError: string);
begin
  MsgLogConsole('ERROR: '+msgCannotLoadLexerFile+' '+ExtractFileName(AFileName)+'; '+AError);
end;


procedure AppLoadLexers;
var
  cfg: TJsonConfig;
  SErrorLines: string;
  SErrorItem: string;
  Sep: TATStringSeparator;
{
var
  NCountNormal, NCountLite: integer;
  NTickNormal, NTickLite: QWord;
  }
begin
  //must read UiOps.LexerThemes here, AppLoadLexers runs in a thread
  //before loading all options, and we need this option already
  cfg:= TJsonConfig.Create(nil);
  try
    try
      cfg.Filename:= AppFile_OptionsUser;
      UiOps.LexerThemes:= cfg.GetValue('ui_lexer_themes', UiOps.LexerThemes);
    except
    end;
  finally
    cfg.Free;
  end;

  //1) load lite lexers
  //NTickLite:= GetTickCount64;

  AppManagerLite.Clear;
  AppManagerLite.LoadFromDir(AppDir_LexersLite);

  {
  NTickLite:= GetTickCount64-NTickLite;
  NCountLite:= AppManagerLite.LexerCount;
  if NCountLite=0 then
    MsgLogConsole(Format(msgCannotFindLexers, [AppDir_LexersLite]));
    }

  //2) load EControl lexers
  //NTickNormal:= GetTickCount64;

  AppManager.OnLexerLoaded:= @AppOnLexerLoaded;
  AppManager.OnLexerLoadError:= @AppOnLexerLoadError;
  AppManager.InitLibrary(AppDir_Lexers, SErrorLines);

  if SErrorLines<>'' then
  begin
    Sep.Init(SErrorLines, #10);
    while Sep.GetItemStr(SErrorItem) do
      MsgLogConsole('NOTE: '+SErrorItem);
  end;

  {
  NTickNormal:= GetTickCount64-NTickNormal;
  NCountNormal:= AppManager.LexerCount;
  if NCountNormal=0 then
    MsgLogConsole(Format(msgCannotFindLexers, [AppDir_Lexers]));
    }
end;


function LiteLexer_GetStyleHash(const AStyleName: string): integer;
var
  iStyle: TAppThemeStyleId;
begin
  Result:= -1;
  for iStyle:= Low(iStyle) to High(iStyle) do
    if SameStr(AStyleName, AppTheme.Styles[iStyle].DisplayName) then
      exit(Ord(iStyle));
end;

procedure LiteLexer_ApplyStyle(AStyleHash: integer; var APart: TATLinePart);
var
  st: TecSyntaxFormat;
begin
  if AStyleHash<0 then exit;
  st:= AppTheme.Styles[TAppThemeStyleId(AStyleHash)];
  ApplyPartStyleFromEcontrolStyle(APart, st);
end;

function IsDefaultSession(const S: string): boolean;
var
  sFilename, sJsonPath: string;
begin
  if S='' then
    exit(true);
  SSplitByChar(S, '|', sFilename, sJsonPath);
  Result:=
    (sJsonPath='') and
    (ChangeFileExt(ExtractFileName(sFilename), '')=cAppSessionDefaultBase);
end;

function IsDefaultSessionActive: boolean;
begin
  Result:= IsDefaultSession(AppSessionName);
end;

function AppFile_Session: string;
var
  sFilename, sJsonPath: string;
begin
  Result:= AppSessionName;
  if Result='' then
    Result:= cAppSessionDefault;

  SSplitByChar(Result, '|', sFilename, sJsonPath);
  if ExtractFileDir(sFilename)='' then
    Result:= AppDir_Settings+DirectorySeparator+Result;
end;


function AppConfigKeyForBookmarks(Ed: TATSynEdit): string;
begin
  if Ed.FileName<>'' then
    Result:= '/bookmarks/'+SMaskFilenameSlashes(AppCollapseHomeDirInFilename(Ed.FileName))
  else
    Result:= '';
end;

function AppDiskGetFreeSpace(const fn: string): Int64;
begin
  {$ifdef linux}
  //this crashes on FreeBSD 12 x64
  exit(SysUtils.DiskFree(SysUtils.AddDisk(ExtractFileDir(fn))));
  {$endif}

  {$ifdef windows}
  exit(SysUtils.DiskFree(SysUtils.GetDriveIDFromLetter(ExtractFileDrive(fn))));
  {$endif}

  //cannot detect
  exit(-1);
end;

procedure AppDiskCheckFreeSpace(const fn: string);
var
  NSpace, NSpaceShow: Int64;
begin
  if UiOps.CheckLowDiskSpace<=0 then exit;
  repeat
    NSpace:= AppDiskGetFreeSpace(fn);
    if NSpace<0 then exit; //cannot detect free space
    if NSpace>=UiOps.CheckLowDiskSpace then exit;

    NSpaceShow:= NSpace div (1024*1024);
    if NSpaceShow=0 then
      NSpaceShow:= 1;

    if MsgBox(
      Format(msgErrorLowDiskSpaceMb, [NSpaceShow]),
      MB_RETRYCANCEL or MB_ICONWARNING) = ID_CANCEL then exit;
  until false;
end;

function AppKeyIsAllowedAsCustomHotkey(Key: Word; Shift: TShiftState): boolean;
begin
  Result:= true;

  //don't allow to reassign system keys: Alt/Ctrl/Shift/Win
  if (Key=VK_MENU) or
     (Key=VK_LMENU) or
     (Key=VK_RMENU) or
     (Key=VK_CONTROL) or
     (Key=VK_LCONTROL) or
     (Key=VK_RCONTROL) or
     (Key=VK_SHIFT) or
     (Key=VK_LSHIFT) or
     (Key=VK_RSHIFT) or
     (Key=VK_LWIN) or
     (Key=VK_RWIN) then
    exit(false);

  //don't allow to reassign these
  //VK_RETURN is not here - issue #4510
  if (Key in [VK_SPACE, {VK_RETURN,} VK_TAB, VK_BACK]) and (Shift=[]) then
    exit(false);
end;

function IsColorDark(C: TColor): boolean;
const
  cMargin = $60;
var
  r, g, b: byte;
begin
  RedGreenBlue(C, r, g, b);
  Result:= (r<=cMargin) and (g<=cMargin) and (b<=cMargin);
end;

procedure AppStopListTimers;
var
  Obj: TObject;
  i: integer;
begin
  for i:= AppListTimers.Count-1 downto 0 do
  begin
    Obj:= AppListTimers.Objects[i];
    if Assigned(Obj) then
      TTimer(Obj).Enabled:= false;
  end;
end;

{
procedure AppFreeListTimers;
var
  Obj: TObject;
  i: integer;
begin
  for i:= AppListTimers.Count-1 downto 0 do
  begin
    Obj:= AppListTimers.Objects[i];
    if Assigned(Obj) then
    begin
      TTimer(Obj).Enabled:= false;
      Obj.Free;
    end;
  end;
  FreeAndNil(AppListTimers);
end;
}

function IsSetToOneInstance: boolean;
var
  c: TJSONConfig;
begin
  //default must be True, issue #3337
  Result := True;
  c := TJSONConfig.Create(nil);
  try
    try
      c.Filename := AppFile_OptionsUser;
    except
      on E: Exception do
      begin
        MsgBadConfig(AppFile_OptionsUser, E.Message);
        Exit;
      end;
    end;
    Result := c.GetValue('ui_one_instance', Result);
  finally
    c.Free;
  end;
end;

procedure GetParamsForUniqueInstance(out AParams: TAppStringArray);
var
  N, i: integer;
  S, WorkDir: string;
  bAddDir: boolean;
begin
  WorkDir:= GetCurrentDirUTF8;

  N:= ParamCount;
  AParams:= nil;
  SetLength(AParams, N);

  for i:= 1 to N do
  begin
    S:= ParamStr(i);
    S:= AppExpandFilename(S);

    bAddDir :=
      (S[1] <> '-') and
      (WorkDir <> '') and
      not IsOsFullPath(S);

    if bAddDir then
      S:= WorkDir+DirectorySeparator+S;

    AParams[i-1]:= S;
  end;
end;

{$ifdef unix}
type
  TAppUniqInstDummy = class
  public
    procedure HandleOtherInstance(Sender: TObject; ParamCount: Integer; const Parameters: array of String);
  end;

var
  AppUniqInstDummy: TAppUniqInstDummy = nil;

function IsAnotherInstanceRunning: boolean;
var
  CmdParams: TAppStringArray;
begin
  AppUniqInstDummy:= TAppUniqInstDummy.Create;
  AppUniqInst:= TUniqueInstance.Create(nil);
  AppUniqInst.UpdateInterval:= 100; //work faster, issue #5081
  AppUniqInst.Identifier:= AppUserName+'_'+AppServerId; //added username to fix CudaText #4079
  AppUniqInst.OnOtherInstance:= @AppUniqInstDummy.HandleOtherInstance;
  AppUniqInst.Enabled:= true;

  GetParamsForUniqueInstance(CmdParams);
  AppUniqInst.Loaded(CmdParams);
  Result:= AppUniqInst.PriorInstanceRunning;
end;

procedure TAppUniqInstDummy.HandleOtherInstance(Sender: TObject;
  ParamCount: Integer; const Parameters: array of String);
begin
  //dummy
end;
{$endif}


procedure AppApplyRendererTweaks(const s: string);
const
  cCharEllipsis = $2026;
  cHexShow: array[boolean] of byte = (uw_space, uw_hexshow);
var
  bValue: boolean;
begin
  if Pos('e', s)>0 then
    FixedSizes[cCharEllipsis]:= uw_normal
  else
    FixedSizes[cCharEllipsis]:= uw_fullwidth;

  ATEditorOptions.RenderSpaceBgAtLineEOL:= Pos('n', s)=0;
  ATEditorOptions.PreciseCalculationOfCharWidth:= Pos('w', s)=0;
  ATEditorOptions.TextoutNeedsOffsets:= Pos('o', s)>0;
  ATEditorOptions.CaretTextOverInvertedRect:= Pos('c', s)>0;
  ATEditorOptions.EnableLigaturesOnLineWithCaret:= Pos('l', s)>0;

  bValue:= Pos('s', s)=0;
  FixedSizes[$1680]:= cHexShow[bValue];
  FixedSizes[$2007]:= cHexShow[bValue];
  FixedSizes[$200B]:= cHexShow[bValue];
  FixedSizes[$202F]:= cHexShow[bValue];
  FixedSizes[$205F]:= cHexShow[bValue];
  FixedSizes[$2060]:= cHexShow[bValue];
  FixedSizes[$3000]:= cHexShow[bValue];
end;

procedure AppApplyScrollbarStyles(const s: string);
var
  Sep: TATStringSeparator;
  N: integer;
begin
  Sep.Init(s);
  if Sep.GetItemInt(N, -1) then
    if (N>=0) and (N<=Ord(High(TATScrollbarArrowsStyle))) then
      ATScrollbarTheme.ArrowStyleH:= TATScrollbarArrowsStyle(N);
  if Sep.GetItemInt(N, -1) then
    if (N>=0) and (N<=Ord(High(TATScrollbarArrowsStyle))) then
      ATScrollbarTheme.ArrowStyleV:= TATScrollbarArrowsStyle(N);
end;

procedure AppApplyUnprintedSymbolsScale(const s: string);
var
  Sep: TATStringSeparator;
begin
  Sep.Init(s);
  Sep.GetItemInt(ATEditorOptions.UnprintedSpaceDotScale, ATEditorOptions.UnprintedSpaceDotScale, 1, 100);
  Sep.GetItemInt(ATEditorOptions.UnprintedEndDotScale, ATEditorOptions.UnprintedEndDotScale, 1, 100);
  Sep.GetItemInt(ATEditorOptions.UnprintedEndFontScale, ATEditorOptions.UnprintedEndFontScale * 10 div 6, 5, 100);
  ATEditorOptions.UnprintedPilcrowScale:= ATEditorOptions.UnprintedEndFontScale;
  ATEditorOptions.UnprintedEndFontScale:= ATEditorOptions.UnprintedEndFontScale * 6 div 10;
  Sep.GetItemInt(ATEditorOptions.UnprintedTabPointerScale, ATEditorOptions.UnprintedTabPointerScale, 0, 100);
end;

procedure AppApplyFallbackEncoding(const s: string);
begin
  case s of
    'ansi':
      ATEditorOptions.FallbackEncoding:= EncConvGetANSI;
    'oem':
      ATEditorOptions.FallbackEncoding:= EncConvGetOEM;
    else
      begin
        ATEditorOptions.FallbackEncoding:= EncConvFindEncoding(s, eidCP1252);
        if ATEditorOptions.FallbackEncoding<=eidLastUnicode then
          ATEditorOptions.FallbackEncoding:= eidCP1252;
      end;
  end;
end;

procedure AppApplyAutoCopyToClipboard(const s: string);
var
  N: integer;
begin
  ATEditorOptions.AutoCopyToClipboard:= Pos('c', s)>0;
  ATEditorOptions.AutoCopyToPrimarySel:= Pos('p', s)>0;
  N:= SExtractNumberFromStringAfterChar(s, 'm', 0);
  if N>=1000 then
    ATEditorOptions.AutoCopyMaxTextSize:= N;
end;


function AppOption_LoadFromStringlist(L: TStringList; const AKey: string; ADefault: integer): integer;
var
  NIndex: integer;
begin
  if L.Find(AKey, NIndex) then
    Result:= integer(PtrInt(L.Objects[NIndex]))
  else
    Result:= ADefault;
end;

procedure AppOption_SaveToStringlist(L: TStringList; const AKey: string; AValue: integer);
var
  NIndex: integer;
begin
  if L.Find(AKey, NIndex) then
    L.Objects[NIndex]:= TObject(PtrInt(AValue))
  else
    L.AddObject(AKey, TObject(PtrInt(AValue)));
end;



initialization

  InitDirs;
  InitEditorOps(EditorOps);
  InitUiOps(UiOps);
  InitBasicCommandLineOptions;

  AppConsoleQueue:= TAppConsoleQueue.Create;
  AppCommandsDelayed:= TAppCommandsDelayed.Create;
  AppCommandList:= TFPList.Create;
  AppEventList:= TFPList.Create;
  AppTreeHelpers:= TFPList.Create;

  AppKeymapMain:= TATKeymap.Create;
  InitKeymapFull(AppKeymapMain);
  Keymap_AddCudatextItems(AppKeymapMain);

  AppKeymapLexers:= TStringList.Create;
  AppKeymapLexers.UseLocale:= false; //speedup Find()
  AppKeymapLexers.Sorted:= true;
  AppKeymapLexers.OwnsObjects:= true;

  FillChar(AppCodetreeState, SizeOf(AppCodetreeState), 0);
  AppCodetreeState.SelLine:= -1;

  FillChar(AppEventsMaxPriorities, SizeOf(AppEventsMaxPriorities), 0);
  FillChar(AppBookmarkSetup, SizeOf(AppBookmarkSetup), 0);
  AppBookmarkImagelist:= TImageList.Create(nil);

  AppShortcutEscape:= ShortCut(VK_ESCAPE, []);
  AppShortcutShiftTab:= ShortCut(VK_TAB, [ssShift]);

  Mouse.DragImmediate:= false;
  Mouse.DragThreshold:= 12;

  AppConfig_Detect:= TAppKeyValues.Create;
  AppConfig_DetectLine:= TAppKeyValues.Create;
  AppConfig_PGroups:= TAppKeyValues.Create;

  AppConfig_DetectLine.Add('<\?xml .+', 'XML');
  AppConfig_DetectLine.Add('\#!\/bin\/(ba)?sh', 'Bash script');
  AppConfig_DetectLine.Add('\#!\/usr\/bin\/env (ba)?sh', 'Bash script');
  AppConfig_DetectLine.Add('\#!\/usr\/bin\/env python\d*', 'Python');

  AppFrameList1:= TFPList.Create;
  AppFrameList2:= TFPList.Create;
  AppFrameListDeleting:= TFPList.Create;
  AppEventLister:= TEvent.Create(nil, true, true, '');
  AppEventWatcher:= TEvent.Create(nil, true, true, '');

  AppApiFlatTheme:= ATFlatTheme;
  AppListRecents:= TStringList.Create;
  AppListRecents.TextLineBreakStyle:= tlbsLF; //for APP_FILE_RECENTS
  AppListTimers:= TStringList.Create;

  ATSynEdit_Commands.cCommand_GotoDefinition:= cmd_GotoDefinition;

  AppManager:= TecLexerList.Create(nil);
  AppManagerLite:= TATLiteLexers.Create(nil);
  AppManagerLite.OnGetStyleHash:= @LiteLexer_GetStyleHash;
  AppManagerLite.OnApplyStyle:= @LiteLexer_ApplyStyle;
  AppManagerThread:= TAppManagerThread.Create(false);

  ATEditorMaxClipboardRecents:= 15;

  AppStatusbarMessages:= TStringList.Create;
  AppStatusbarMessages.TextLineBreakStyle:= tlbsLF;
  AppStatusbarMessages.TrailingLineBreak:= false;

  AppApplyRendererTweaks(
    {$if defined(darwin) or defined(LCLQt5) or defined(LCLQt6)}
    's'
    {$else}
      {$ifdef windows}
      'wos'
      {$else}
      'ws'
      {$endif}
    {$endif}
    );

  EditorOps_CenteringWidth:= TStringList.Create;
  EditorOps_CenteringWidth.Sorted:= true;
  EditorOps_CenteringWidth.UseLocale:= false;

  EditorOps_CenteringDistFree:= TStringList.Create;
  EditorOps_CenteringDistFree.Sorted:= true;
  EditorOps_CenteringDistFree.UseLocale:= false;


finalization

  FreeAndNil(EditorOps_CenteringDistFree);
  FreeAndNil(EditorOps_CenteringWidth);

  FreeAndNil(AppManagerThread);
  FreeAndNil(AppManagerLite);
  FreeAndNil(AppManager);

  FreeAndNil(AppStatusbarMessages);
  FreeAndNil(AppListRecents);
  FreeAndNil(AppEventWatcher);
  FreeAndNil(AppEventLister);
  FreeAndNil(AppFrameListDeleting);
  FreeAndNil(AppFrameList2);
  FreeAndNil(AppFrameList1);

  FreeAndNil(AppConfig_PGroups);
  FreeAndNil(AppConfig_DetectLine);
  FreeAndNil(AppConfig_Detect);
  FreeAndNil(AppKeymapLexers);
  FreeAndNil(AppKeymapMain);
  FreeAndNil(AppBookmarkImagelist);

  AppClearPluginLists;
  //FreeAndNil(AppTreeHelpers);
  //FreeAndNil(AppEventList);
  //FreeAndNil(AppCommandList);

  AppConsoleQueue.Push(''); // fix for #5037: Adds dummy data to avoid exception on free
  FreeAndNil(AppConsoleQueue);
  FreeAndNil(AppCommandsDelayed);

  if Assigned(AppLexersLastDetected) then
    FreeAndNil(AppLexersLastDetected);

  //AppFreeListTimers; //somehow gives crash on exit, if TerminalPlus was used, in timer_proc(TIMER_DELETE...)
  //AppClearPluginLists;

  {$ifdef unix}
  if Assigned(AppUniqInst) then
    FreeAndNil(AppUniqInst);
  if Assigned(AppUniqInstDummy) then
    FreeAndNil(AppUniqInstDummy);
  {$endif}

end.
