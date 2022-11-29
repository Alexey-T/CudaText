(*
This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at http://mozilla.org/MPL/2.0/.

Copyright (c) Alexey Torgashin
*)
unit FormFrame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Forms, Controls, Dialogs,
  ExtCtrls, Menus, StdCtrls, StrUtils, ComCtrls, Clipbrd,
  LCLIntf, LCLProc, LCLType, LazUTF8, LazFileUtils, FileUtil,
  IniFiles,
  ATTabs,
  ATGroups,
  ATScrollBar,
  ATGauge,
  ATSynEdit,
  ATSynEdit_Globals,
  ATSynEdit_Finder,
  ATSynEdit_Keymap_Init,
  ATSynEdit_Adapters,
  ATSynEdit_Adapter_EControl,
  ATSynEdit_Adapter_LiteLexer,
  ATSynEdit_Carets,
  ATSynEdit_Gaps,
  ATSynEdit_Markers,
  ATSynEdit_LineParts,
  ATSynEdit_Commands,
  ATSynEdit_Bookmarks,
  ATSynEdit_CanvasProc,
  ATStrings,
  ATStringProc,
  ATStringProc_Separator,
  ATStringProc_HtmlColor,
  ATCanvasPrimitives,
  ATButtons,
  ATBinHex,
  ATStreamSearch,
  ATImageBox,
  BGRABitmap,
  BGRABitmapTypes,
  proc_appvariant,
  proc_globdata,
  proc_editor,
  proc_editor_saving,
  proc_cmd,
  proc_colors,
  proc_files,
  proc_msg,
  proc_str,
  proc_py,
  proc_py_const,
  proc_miscutils,
  proc_lexer_styles,
  proc_customdialog,
  proc_customdialog_dummy,
  ec_SyntAnal,
  ec_proc_lexer,
  ec_lexerlist,
  formlexerstylemap,
  at__jsonconf,
  math;

type
  TEditorFramePyEvent = function(AEd: TATSynEdit; AEvent: TAppPyEvent;
    const AParams: TAppVariantArray): TAppPyEventResult of object;
  TEditorFrameStringEvent = procedure(Sender: TObject; const S: string) of object;

type
  TAppOpenMode = (
    cOpenModeNone,
    cOpenModeEditor,
    cOpenModeViewText,
    cOpenModeViewBinary,
    cOpenModeViewHex,
    cOpenModeViewUnicode,
    cOpenModeViewUHex
    );

  TATEditorFrameKind = (
    efkEditor,
    efkBinaryViewer,
    efkImageViewer
    );

const
  cATEditorFrameKindStr: array[TATEditorFrameKind] of string = (
    'text',
    'bin',
    'pic'
    );

type
  TFrameGetSaveDialog = procedure(var ASaveDlg: TSaveDialog) of object;

type
  TFrameNotificationControls = record
    Panel: TPanel;
    InfoPanel: TPanel;
    ButtonYes,
    ButtonNo,
    ButtonStop: TATButton;
  end;

const
  cFrameMaxEdIndex=1; //count of editors minus 1

type
  { TEditorFrame }

  TEditorFrame = class(TFrame)
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    TimerCaret: TTimer;
    TimerChange: TTimer;
    procedure NotifReloadStopClick(Sender: TObject);
    procedure NotifReloadNoClick(Sender: TObject);
    procedure NotifReloadYesClick(Sender: TObject);
    procedure NotifDeletedStopClick(Sender: TObject);
    procedure NotifDeletedNoClick(Sender: TObject);
    procedure NotifDeletedYesClick(Sender: TObject);
    procedure TimerCaretTimer(Sender: TObject);
    procedure TimerChangeTimer(Sender: TObject);
  private
    { private declarations }
    FFormDummy: TFormDummy;
    Adapter1: TATAdapterEControl;
    Adapter2: TATAdapterEControl;
    PanelInfo: TPanel;
    PanelNoHilite: TPanel;
    NotifReloadControls: array[0..cFrameMaxEdIndex] of TFrameNotificationControls;
    NotifDeletedControls: array[0..cFrameMaxEdIndex] of TFrameNotificationControls;
    FTabCaption: string;
    FTabCaptionAddon: string;
    FTabCaptionUntitled: string;
    FTabCaptionFromApi: boolean;
    FTabImageIndex: integer;
    FTabId: integer;
    FFileName: string;
    FFileName2: string;
    FFileWasBig: array[0..1] of boolean;
    FTextCharsTyped: integer;
    FTextChange: array[0..1] of boolean;
    FTextChangeSlow: array[0..1] of boolean;
    FActivationTime: Int64;
    FCodetreeFilter: string;
    FCodetreeFilterHistory: TStringList;
    FCodetreeSortType: TSortType;
    FEnabledCodeTree: array[0..1] of boolean;
    FNotifEnabled: boolean;
    FNotifDeletedEnabled: boolean;
    FOnChangeCaption: TNotifyEvent;
    FOnChangeSlow: TNotifyEvent;
    FOnProgress: TATFinderProgress;
    FOnUpdateStatusbar: TNotifyEvent;
    FOnUpdateState: TNotifyEvent;
    FOnUpdateZoom: TNotifyEvent;
    FOnFocusEditor: TNotifyEvent;
    FOnEditorCommand: TATSynEditCommandEvent;
    FOnEditorChangeCaretPos: TNotifyEvent;
    FOnEditorScroll: TNotifyEvent;
    FOnSaveFile: TEditorFrameStringEvent;
    FOnAddRecent: TNotifyEvent;
    FOnPyEvent: TEditorFramePyEvent;
    FOnInitAdapter: TNotifyEvent;
    FOnLexerChange: TATEditorEvent;
    FSplitPos: double;
    FSplitHorz: boolean;
    FActiveSecondaryEd: boolean;
    FLocked: boolean;
    FTabColor: TColor;
    FTabFontColor: TColor;
    FTabPinned: boolean;
    FTabSizeChanged: boolean;
    FTabKeyCollectMarkers: boolean;
    FInSession: boolean;
    FInHistory: boolean;
    FMacroRecord: boolean;
    FImageBox: TATImageBox;
    FBin: TATBinHex;
    FBinStream: TFileStream;
    FCheckFilenameOpened: TAppStringFunction;
    FOnMsgStatus: TAppStringEvent;
    FSaveDialog: TSaveDialog;
    FWasVisible: boolean;
    FInitialLexer1: TecSyntAnalyzer;
    FInitialLexer2: TecSyntAnalyzer;
    FSaveHistory: boolean;
    FEditorsLinked: boolean;
    FTabExtModified: array[0..1] of boolean;
    FTabExtDeleted: array[0..1] of boolean;
    FCachedTreeview: array[0..1] of TTreeView;
    FLexerBackup: array[0..1] of TATAdapterHilite;
    FLexerChooseFunc: TecLexerChooseFunc;
    FLexerNameBackup: string;
    FBracketHilite: boolean;
    FBracketHiliteUserChanged: boolean;
    FBracketSymbols: string;
    FBracketMaxDistance: integer;
    FMicromapBmp: TBGRABitmap;
    FOnGetSaveDialog: TFrameGetSaveDialog;
    FOnAppClickLink: TATSynEditClickLinkEvent;
    FProgressForm: TForm;
    FProgressGauge: TATGauge;
    FProgressOldProgress: integer;
    FProgressOldHandler: TNotifyEvent;

    procedure ApplyThemeToInfoPanel(APanel: TPanel);
    procedure BinaryOnKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure BinaryOnScroll(Sender: TObject);
    procedure BinaryOnProgress(const ACurrentPos, AMaximalPos: Int64;
      var AContinueSearching: Boolean);
    procedure CancelAutocompleteAutoshow;
    procedure DoDeactivatePictureMode;
    procedure DoDeactivateViewerMode;
    procedure DoFileOpen_Ex(Ed: TATSynEdit; const AFileName: string;
      AAllowLoadHistory, AAllowLoadHistoryEnc, AAllowLoadBookmarks,
      AAllowLexerDetect, AAllowErrorMsgBox,
      AKeepScroll, AAllowLoadUndo: boolean; AOpenMode: TAppOpenMode);
    procedure DoImageboxScroll(Sender: TObject);
    procedure DoOnChangeCaption;
    procedure DoOnUpdateState;
    procedure DoOnUpdateZoom;
    procedure DoOnUpdateStatusbar;
    procedure EditorClickEndSelect(Sender: TObject; APrevPnt, ANewPnt: TPoint);
    procedure EditorClickMoveCaret(Sender: TObject; APrevPnt, ANewPnt: TPoint);
    procedure EditorDrawMicromap(Sender: TObject; ACanvas: TCanvas; const ARect: TRect);
    procedure EditorDrawScrollbarVert(Sender: TObject;
      AType: TATScrollbarElemType; ACanvas: TCanvas; const ARect,
      ARect2: TRect; var ACanDraw: boolean);
    function EditorObjToTreeviewIndex(Ed: TATSynEdit): integer; inline;
    procedure EditorOnChange(Sender: TObject);
    procedure EditorOnChangeModified(Sender: TObject);
    procedure EditorOnChangeCaretPos(Sender: TObject);
    procedure EditorOnChangeState(Sender: TObject);
    procedure EditorOnChangeBookmarks(Sender: TObject);
    procedure EditorOnChangeZoom(Sender: TObject);
    procedure EditorOnClick(Sender: TObject);
    procedure EditorOnClickGap(Sender: TObject; AGapItem: TATGapItem; APos: TPoint);
    procedure EditorOnClickGutter(Sender: TObject; ABand, ALine: integer);
    procedure EditorOnClickDouble(Sender: TObject; var AHandled: boolean);
    procedure EditorOnClickLink(Sender: TObject; const ALink: string);
    procedure EditorOnClickMicroMap(Sender: TObject; AX, AY: integer);
    procedure EditorOnCommand(Sender: TObject; ACmd: integer; AInvoke: TATEditorCommandInvoke; const AText: string; var AHandled: boolean);
    procedure EditorOnCommandAfter(Sender: TObject; ACommand: integer; const AText: string);
    procedure EditorOnDrawBookmarkIcon(Sender: TObject; C: TCanvas; ALineNum: integer; const ARect: TRect);
    procedure EditorOnEnter(Sender: TObject);
    procedure EditorOnDrawLine(Sender: TObject; C: TCanvas; ALineIndex, AX, AY: integer;
      const AStr: atString; const ACharSize: TATEditorCharSize; constref AExtent: TATIntFixedArray);
    procedure EditorOnCalcBookmarkColor(Sender: TObject; ABookmarkKind: integer; var AColor: TColor);
    procedure EditorOnHotspotEnter(Sender: TObject; AHotspotIndex: integer);
    procedure EditorOnHotspotExit(Sender: TObject; AHotspotIndex: integer);
    procedure EditorOnKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure EditorOnPaste(Sender: TObject; var AHandled: boolean; AKeepCaret, ASelectThen: boolean);
    procedure EditorOnScroll(Sender: TObject);
    function GetAdapter(Ed: TATSynEdit): TATAdapterEControl;
    function GetCachedTreeviewInited(Ed: TATSynEdit): boolean;
    function GetCachedTreeview(Ed: TATSynEdit): TTreeView;
    function GetCommentString(Ed: TATSynEdit): string;
    function GetTextChangeSlow(EdIndex: integer): boolean;
    procedure HandleStringsProgress(Sender: TObject);
    procedure SetTextChangeSlow(EdIndex: integer; AValue: boolean);
    function GetEnabledCodeTree(Ed: TATSynEdit): boolean;
    function GetEnabledFolding: boolean;
    function GetFileWasBig(Ed: TATSynEdit): boolean;
    function GetInitialLexer(Ed: TATSynEdit): TecSyntAnalyzer;
    function GetLineEnds(Ed: TATSynEdit): TATLineEnds;
    function GetPictureScale: integer;
    function GetReadOnly(Ed: TATSynEdit): boolean;
    function GetSaveDialog: TSaveDialog;
    function GetSplitPosCurrent: double;
    function GetSplitted: boolean;
    function GetTabExtModified(EdIndex: integer): boolean;
    function GetTabExtDeleted(EdIndex: integer): boolean;
    function GetTabKeyCollectMarkers: boolean;
    function GetTabVisible: boolean;
    function GetUnprintedEnds: boolean;
    function GetUnprintedEndsDetails: boolean;
    function GetUnprintedShow: boolean;
    function GetUnprintedSpaces: boolean;
    procedure InitEditor(var ed: TATSynEdit; const AName: string);
    procedure InitNotificationPanel(Index: integer;
      AIsDeleted: boolean;
      var AControls: TFrameNotificationControls;
      AClickYes, AClickNo, AClickStop: TNotifyEvent);
    procedure InitPanelInfo(var APanel: TPanel; const AText: string;
      AOnClick: TNotifyEvent; ARequirePython: boolean);
    procedure UpdateNotificationPanel(
      Index: integer;
      var AControls: TFrameNotificationControls;
      const ACaptionYes, ACaptionNo, ACaptionStop, ALabel: string);
    procedure PaintMicromap(Ed: TATSynEdit; ACanvas: TCanvas; const ARect: TRect);
    procedure PanelInfoClick(Sender: TObject);
    procedure PanelNoHiliteClick(Sender: TObject);
    procedure SetBracketHilite(AValue: boolean);
    procedure SetEnabledCodeTree(Ed: TATSynEdit; AValue: boolean);
    procedure SetEnabledFolding(AValue: boolean);
    procedure SetFileName(const AValue: string);
    procedure SetFileName2(const AValue: string);
    procedure SetFileWasBig(Ed: TATSynEdit; AValue: boolean);
    procedure SetInitialLexer(Ed: TATSynEdit; AValue: TecSyntAnalyzer);
    procedure SetLocked(AValue: boolean);
    procedure SetPictureScale(AValue: integer);
    procedure SetReadOnly(Ed: TATSynEdit; AValue: boolean);
    procedure SetTabCaptionAddon(const AValue: string);
    procedure SetTabColor(AColor: TColor);
    procedure SetTabFontColor(AColor: TColor);
    procedure SetTabExtModified(EdIndex: integer; AValue: boolean);
    procedure SetTabExtDeleted(EdIndex: integer; AValue: boolean);
    procedure SetTabPinned(AValue: boolean);
    procedure SetTabImageIndex(AValue: integer);
    procedure SetTabVisible(AValue: boolean);
    procedure SetUnprintedEnds(AValue: boolean);
    procedure SetUnprintedEndsDetails(AValue: boolean);
    procedure SetUnprintedShow(AValue: boolean);
    procedure SetSplitHorz(AValue: boolean);
    procedure SetSplitPos(AValue: double);
    procedure SetSplitted(AValue: boolean);
    procedure SetTabCaption(const AValue: string);
    procedure SetLineEnds(Ed: TATSynEdit; AValue: TATLineEnds);
    procedure SetUnprintedSpaces(AValue: boolean);
    procedure SetEditorsLinked(AValue: boolean);
    procedure SplitterMoved(Sender: TObject);
    procedure TreeOnDeletion(Sender: TObject; Node: TTreeNode);
    procedure UpdateEds(AUpdateWrapInfo: boolean=false);
    function GetLexer(Ed: TATSynEdit): TecSyntAnalyzer;
    function GetLexerLite(Ed: TATSynEdit): TATLiteLexer;
    function GetLexerName(Ed: TATSynEdit): string;
    procedure SetLexer(Ed: TATSynEdit; an: TecSyntAnalyzer);
    procedure SetLexerLite(Ed: TATSynEdit; an: TATLiteLexer);
    procedure SetLexerName(Ed: TATSynEdit; const AValue: string);
    procedure UpdateTabTooltip;
    function GetIsPreview: boolean;
    procedure SetIsPreview(AValue: boolean);

    procedure DoSaveUndo(Ed: TATSynEdit; const AFileName: string);
    procedure DoLoadUndo(Ed: TATSynEdit);
    procedure DoSaveHistory_Caret(Ed: TATSynEdit; c: TJsonConfig; const path: UnicodeString);
    procedure DoSaveHistory_Bookmarks(Ed: TATSynEdit; c: TJsonConfig; const path: UnicodeString);
    procedure RestoreSavedLexer(Ed: TATSynEdit);

  protected
    procedure DoOnResize; override;
  public
    { public declarations }
    Ed1: TATSynEdit;
    Ed2: TATSynEdit;
    Splitter: TSplitter;
    Groups: TATGroups;
    MacroStrings: TStringList;
    VersionInSession: Int64;
    FileProps: array[0..1] of TAppFileProps;

    constructor Create(AOwner: TComponent; AApplyCentering: boolean); reintroduce;
    destructor Destroy; override;
    function Editor: TATSynEdit;
    function EditorBro: TATSynEdit;
    function Modified(ACheckOnSessionClosing: boolean=false): boolean;
    property Adapter[Ed: TATSynEdit]: TATAdapterEControl read GetAdapter;
    procedure EditorOnKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure DoShow;

    function EditorIndexToObj(N: integer): TATSynEdit;
    function EditorObjToIndex(Ed: TATSynEdit): integer;
    property ReadOnly[Ed: TATSynEdit]: boolean read GetReadOnly write SetReadOnly;
    property TabCaption: string read FTabCaption write SetTabCaption;
    property TabCaptionAddon: string read FTabCaptionAddon write SetTabCaptionAddon;
    property TabCaptionUntitled: string read FTabCaptionUntitled write FTabCaptionUntitled;
    property TabImageIndex: integer read FTabImageIndex write SetTabImageIndex;
    property TabCaptionFromApi: boolean read FTabCaptionFromApi write FTabCaptionFromApi;
    property TabId: integer read FTabId;
    property TabIsPreview: boolean read GetIsPreview write SetIsPreview;
    property TabVisible: boolean read GetTabVisible write SetTabVisible;
    procedure UpdateCaptionFromFilename;

    property CachedTreeViewInited[Ed: TATSynEdit]: boolean read GetCachedTreeviewInited;
    property CachedTreeView[Ed: TATSynEdit]: TTreeView read GetCachedTreeview;
    property SaveHistory: boolean read FSaveHistory write FSaveHistory;
    procedure UpdateModified(Ed: TATSynEdit; AWithEvent: boolean= true);
    procedure UpdatePinned(Ed: TATSynEdit; AWithEvent: boolean);
    procedure UpdateReadOnlyFromFile(Ed: TATSynEdit);
    procedure UpdateFrame(AUpdatedText: boolean);
    procedure FixLexerIfDeleted(Ed: TATSynEdit; const ALexerName: string);

    property NotifEnabled: boolean read FNotifEnabled write FNotifEnabled;
    property NotifDeletedEnabled: boolean read FNotifDeletedEnabled write FNotifDeletedEnabled;
    procedure NotifyAboutChange(Ed: TATSynEdit);

    property FileName: string read FFileName;
    property FileName2: string read FFileName2;
    property LexerChooseFunc: TecLexerChooseFunc read FLexerChooseFunc write FLexerChooseFunc;
    function GetFileName(Ed: TATSynEdit): string;
    procedure SetFileName(Ed: TATSynEdit; const AFileName: string);
    property FileWasBig[Ed: TATSynEdit]: boolean read GetFileWasBig write SetFileWasBig;

    property Lexer[Ed: TATSynEdit]: TecSyntAnalyzer read GetLexer write SetLexer;
    property LexerLite[Ed: TATSynEdit]: TATLiteLexer read GetLexerLite write SetLexerLite;
    property LexerName[Ed: TATSynEdit]: string read GetLexerName write SetLexerName;
    property LexerInitial[Ed: TATSynEdit]: TecSyntAnalyzer read GetInitialLexer write SetInitialLexer;

    procedure LexerBackupSave;
    procedure LexerBackupRestore;

    property Locked: boolean read FLocked write SetLocked;
    property CommentString[Ed: TATSynEdit]: string read GetCommentString;
    property TabColor: TColor read FTabColor write SetTabColor;
    property TabFontColor: TColor read FTabFontColor write SetTabFontColor;
    property TabPinned: boolean read FTabPinned write SetTabPinned;
    property TabExtModified[EdIndex: integer]: boolean read GetTabExtModified write SetTabExtModified;
    property TabExtDeleted[EdIndex: integer]: boolean read GetTabExtDeleted write SetTabExtDeleted;
    property TabSizeChanged: boolean read FTabSizeChanged write FTabSizeChanged;
    property TabKeyCollectMarkers: boolean read GetTabKeyCollectMarkers write FTabKeyCollectMarkers;
    property InSession: boolean read FInSession write FInSession;
    property InHistory: boolean read FInHistory write FInHistory;
    property TextCharsTyped: integer read FTextCharsTyped write FTextCharsTyped;
    property TextChangeSlow[EdIndex: integer]: boolean read GetTextChangeSlow write SetTextChangeSlow;
    property EnabledCodeTree[Ed: TATSynEdit]: boolean read GetEnabledCodeTree write SetEnabledCodeTree;
    property CodetreeFilter: string read FCodetreeFilter write FCodetreeFilter;
    property CodetreeFilterHistory: TStringList read FCodetreeFilterHistory;
    property CodetreeSortType: TSortType read FCodetreeSortType write FCodetreeSortType;
    property ActivationTime: Int64 read FActivationTime write FActivationTime;
    function IsEmpty: boolean;
    procedure ApplyLexerStyleMap;
    procedure LexerReparse;
    procedure ApplyTheme;
    function IsEditorFocused: boolean;
    function FrameKind: TATEditorFrameKind;
    procedure SetFocus; reintroduce;
    function PictureSizes: TPoint;
    property PictureScale: integer read GetPictureScale write SetPictureScale;
    property Binary: TATBinHex read FBin;
    function BinaryFindFirst(AFinder: TATEditorFinder; AShowAll: boolean): boolean;
    function BinaryFindNext(ABack: boolean): boolean;
    //
    property BracketHilite: boolean read FBracketHilite write SetBracketHilite;
    property BracketHiliteUserChanged: boolean read FBracketHiliteUserChanged write FBracketHiliteUserChanged;
    property BracketSymbols: string read FBracketSymbols write FBracketSymbols;
    property BracketDistance: integer read FBracketMaxDistance write FBracketMaxDistance;
    procedure BracketJump(Ed: TATSynEdit);
    procedure BracketSelect(Ed: TATSynEdit);
    procedure BracketSelectInside(Ed: TATSynEdit);
    //
    property LineEnds[Ed: TATSynEdit]: TATLineEnds read GetLineEnds write SetLineEnds;
    property UnprintedShow: boolean read GetUnprintedShow write SetUnprintedShow;
    property UnprintedSpaces: boolean read GetUnprintedSpaces write SetUnprintedSpaces;
    property UnprintedEnds: boolean read GetUnprintedEnds write SetUnprintedEnds;
    property UnprintedEndsDetails: boolean read GetUnprintedEndsDetails write SetUnprintedEndsDetails;
    property Splitted: boolean read GetSplitted write SetSplitted;
    property SplitHorz: boolean read FSplitHorz write SetSplitHorz;
    property SplitPos: double read FSplitPos write SetSplitPos;
    property EditorsLinked: boolean read FEditorsLinked write SetEditorsLinked;
    property EnabledFolding: boolean read GetEnabledFolding write SetEnabledFolding;
    property SaveDialog: TSaveDialog read GetSaveDialog;
    property WasVisible: boolean read FWasVisible;
    function GetTabPages: TATPages;
    function GetTabGroups: TATGroups;
    function IsTreeBusy: boolean;
    function IsParsingBusy: boolean;
    //file
    procedure DoFileClose;
    procedure DoFileOpen(const AFileName, AFileName2: string;
      AAllowLoadHistory, AAllowLoadBookmarks, AAllowLexerDetect,
      AAllowErrorMsgBox, AAllowLoadUndo: boolean; AOpenMode: TAppOpenMode);
    procedure DoFileOpen_AsBinary(const AFileName: string; AMode: TATBinHexMode);
    procedure DoFileOpen_AsPicture(const AFileName: string);
    function DoFileSave(ASaveAs, AAllEditors: boolean): boolean;
    function DoFileSave_Ex(Ed: TATSynEdit; ASaveAs: boolean): boolean;
    procedure DoFileReload_DisableDetectEncoding(Ed: TATSynEdit);
    function DoFileReload(Ed: TATSynEdit): boolean;
    procedure DoLexerFromFilename(Ed: TATSynEdit; const AFileName: string);
    //history
    procedure DoSaveHistory(Ed: TATSynEdit);
    procedure DoSaveHistoryEx(Ed: TATSynEdit; c: TJsonConfig; const path: UnicodeString; AForSession: boolean);
    procedure DoLoadHistory(Ed: TATSynEdit; AllowLoadEncoding, AllowLoadHistory,
      AllowLoadBookmarks: boolean);
    procedure DoLoadHistoryEx(Ed: TATSynEdit; c: TJsonConfig; const path: UnicodeString; AllowEnc: boolean);
    procedure DoLoadHistory_Bookmarks(Ed: TATSynEdit; c: TJsonConfig; const path: UnicodeString);
    //misc
    function DoPyEvent(AEd: TATSynEdit; AEvent: TAppPyEvent; const AParams: TAppVariantArray): TAppPyEventResult;
    procedure DoPyEventState(Ed: TATSynEdit; AState: integer);
    function DoPyEvent_Macro(const AText: string): boolean;
    procedure DoGotoPos(Ed: TATSynEdit; APosX, APosY: integer);
    procedure DoRemovePreviewStyle;
    procedure DoToggleFocusSplitEditors;
    procedure DoFocusNotificationPanel;
    procedure DoHideNotificationPanels;
    procedure DoHideNotificationPanel(const AControls: TFrameNotificationControls);
    //macro
    procedure DoMacroStartOrStop;
    property MacroRecord: boolean read FMacroRecord;

    //events
    property OnGetSaveDialog: TFrameGetSaveDialog read FOnGetSaveDialog write FOnGetSaveDialog;
    property OnProgress: TATFinderProgress read FOnProgress write FOnProgress;
    property OnCheckFilenameOpened: TAppStringFunction read FCheckFilenameOpened write FCheckFilenameOpened;
    property OnMsgStatus: TAppStringEvent read FOnMsgStatus write FOnMsgStatus;
    property OnFocusEditor: TNotifyEvent read FOnFocusEditor write FOnFocusEditor;
    property OnChangeCaption: TNotifyEvent read FOnChangeCaption write FOnChangeCaption;
    property OnChangeSlow: TNotifyEvent read FOnChangeSlow write FOnChangeSlow;
    property OnUpdateStatusbar: TNotifyEvent read FOnUpdateStatusbar write FOnUpdateStatusbar;
    property OnUpdateState: TNotifyEvent read FOnUpdateState write FOnUpdateState;
    property OnUpdateZoom: TNotifyEvent read FOnUpdateZoom write FOnUpdateZoom;
    property OnEditorCommand: TATSynEditCommandEvent read FOnEditorCommand write FOnEditorCommand;
    property OnEditorChangeCaretPos: TNotifyEvent read FOnEditorChangeCaretPos write FOnEditorChangeCaretPos;
    property OnEditorScroll: TNotifyEvent read FOnEditorScroll write FOnEditorScroll;
    property OnSaveFile: TEditorFrameStringEvent read FOnSaveFile write FOnSaveFile;
    property OnAddRecent: TNotifyEvent read FOnAddRecent write FOnAddRecent;
    property OnPyEvent: TEditorFramePyEvent read FOnPyEvent write FOnPyEvent;
    property OnInitAdapter: TNotifyEvent read FOnInitAdapter write FOnInitAdapter;
    property OnLexerChange: TATEditorEvent read FOnLexerChange write FOnLexerChange;
    property OnAppClickLink: TATSynEditClickLinkEvent read FOnAppClickLink write FOnAppClickLink;
  end;

procedure GetFrameLocation(Frame: TEditorFrame;
  out AGroups: TATGroups; out APages: TATPages;
  out ALocalGroupIndex, AGlobalGroupIndex, ATabIndex: integer);

procedure UpdateTabPreviewStyle(D: TATTabData; AValue: boolean);


implementation

{$R *.lfm}

const
  cSplitHorzToAlign: array[boolean] of TAlign = (alRight, alBottom);

const
  cHistory_Lexer       = '/lexer';
  cHistory_Enc         = '/enc';
  cHistory_TopLine     = '/top';
  cHistory_TopLine2    = '/top2';
  cHistory_Wrap        = '/wrap_mode';
  cHistory_ReadOnly    = '/ro';
  cHistory_Ruler       = '/ruler';
  cHistory_Minimap     = '/minimap';
  cHistory_Micromap    = '/micromap';
  cHistory_TabSize     = '/tab_size';
  cHistory_TabSpace    = '/tab_spaces';
  cHistory_LineNums    = '/nums';
  cHistory_FontScale   = '/scale';
  cHistory_Unpri        = '/unprinted_show';
  cHistory_Unpri_Spaces = '/unprinted_spaces';
  cHistory_Unpri_Ends   = '/unprinted_ends';
  cHistory_Unpri_Detail = '/unprinted_end_details';
  cHistory_Caret       = '/crt';
  cHistory_TabColor    = '/color';
  cHistory_FoldingShow  = '/fold';
  cHistory_FoldedRanges = '/folded';
  cHistory_CodeTreeFilter = '/codetree_filter';
  cHistory_CodeTreeFilters = '/codetree_filters';
  cHistory_TabSplit    = '/split';
  cHistory_TabSplit_Mul = 1e5; //instead of float 0.6, save as int 0.6*1e5

var
  FLastTabId: integer = 0;

function msgSuggestOptionsEditor: string;
var
  ini: TIniFile;
  fn: string;
  s: string;
begin
  S:= '"Options Editor" provides the dialog - click here to open';

  fn:= AppFile_Language;
  if FileExists(fn) then
  begin
    ini:= TIniFile.Create(fn);
    try
      S:= ini.ReadString('si', 'CallOptEditor', S);
    finally
      FreeAndNil(ini);
    end;
  end;

  Result:= S;
end;


procedure GetFrameLocation(Frame: TEditorFrame;
  out AGroups: TATGroups; out APages: TATPages;
  out ALocalGroupIndex, AGlobalGroupIndex, ATabIndex: integer);
begin
  Assert(Assigned(Frame.Parent));
  APages:= Frame.GetTabPages;
  AGroups:= Frame.GetTabGroups;
  AGroups.FindPositionOfControl(Frame, ALocalGroupIndex, ATabIndex);

  AGlobalGroupIndex:= ALocalGroupIndex;
  if AGroups.Tag<>0 then
    Inc(AGlobalGroupIndex, High(TATGroupsNums) + AGroups.Tag);
end;

procedure UpdateTabPreviewStyle(D: TATTabData; AValue: boolean);
begin
  D.TabSpecial:= AValue;
  if AValue then
    D.TabFontStyle:= Lexer_StringToFontStyles(UiOps.TabPreviewFontStyle)
  else
    D.TabFontStyle:= [];
end;


{ TEditorFrame }

procedure TEditorFrame.SetTabCaption(const AValue: string);
var
  bUpdate: boolean;
begin
  if AValue='?' then Exit;
  bUpdate:= FTabCaption<>AValue;

  FTabCaption:= AValue; //don't check bUpdate here (for Win32)

  if bUpdate then
    DoPyEventState(Ed1, EDSTATE_TAB_TITLE);

  DoOnChangeCaption;
end;

procedure TEditorFrame.SetTabCaptionAddon(const AValue: string);
var
  Gr: TATGroups;
  Pages: TATPages;
  NLocalGroups, NGlobalGroup, NTab: integer;
  D: TATTabData;
begin
  if FTabCaptionAddon=AValue then Exit;
  FTabCaptionAddon:= AValue;

  GetFrameLocation(Self, Gr, Pages, NLocalGroups, NGlobalGroup, NTab);
  D:= Pages.Tabs.GetTabData(NTab);
  if Assigned(D) then
    D.TabCaptionAddon:= FTabCaptionAddon;
end;


procedure TEditorFrame.UpdateCaptionFromFilename;
var
  Name1, Name2, SFinalCaption: string;
begin
  //avoid updating caption if API already had set it
  if FTabCaptionFromApi then
  begin
    DoOnChangeCaption; //remove 'modified' font color, repaint
    exit;
  end;

  if EditorsLinked then
  begin
    if FFileName='' then
      Name1:= FTabCaptionUntitled
    else
      Name1:= ExtractFileName_Fixed(FFileName);
    //Name1:= msgModified[Ed1.Modified]+Name1;

    SFinalCaption:= Name1;
  end
  else
  begin
    if (FFileName='') and (FFileName2='') then
      SFinalCaption:= FTabCaptionUntitled
    else
    begin
      Name1:= ExtractFileName_Fixed(FFileName);
      if Name1='' then
        Name1:= msgUntitledTab;
      //Name1:= msgModified[Ed1.Modified]+Name1;

      Name2:= ExtractFileName_Fixed(FFileName2);
      if Name2='' then
        Name2:= msgUntitledTab;
      //Name2:= msgModified[Ed2.Modified]+Name2;

      SFinalCaption:= Name1+' | '+Name2;
    end;
  end;

  TabCaption:= SFinalCaption;
  UpdateTabTooltip;
end;

procedure TEditorFrame.EditorOnClick(Sender: TObject);
var
  Ed: TATSynEdit;
  StateString: string;
begin
  Ed:= Sender as TATSynEdit;

  StateString:= ConvertShiftStateToString(KeyboardStateToShiftState);
  CancelAutocompleteAutoshow;

  if Ed.Markers.DeleteWithTag(UiOps.FindOccur_TagValue) then
    Ed.Update;

  if EditorOps.OpMouseGotoDefinition<>'' then
    if StateString=EditorOps.OpMouseGotoDefinition then
    begin
      DoPyEvent(Ed, cEventOnGotoDef, []);
      exit;
    end;

  DoPyEvent(Ed, cEventOnClick, [AppVariant(StateString)]);
end;

function TEditorFrame.GetSplitPosCurrent: double;
begin
  if not Splitted then
    Result:= 0
  else
  if FSplitHorz then
    Result:= Ed2.Height/Max(Height, 1)
  else
    Result:= Ed2.Width/Max(Width, 1);
end;

function TEditorFrame.GetSplitted: boolean;
begin
  Result:= Ed2.Visible;
end;

procedure TEditorFrame.EditorOnKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  Res: TAppPyEventResult;
begin
  //result=False: block the key
  Res:= DoPyEvent(Sender as TATSynEdit, cEventOnKey,
    [
    AppVariant(Key),
    AppVariant(ConvertShiftStateToString(Shift))
    ]);
  if Res.Val=evrFalse then
  begin
    Key:= 0;
    Exit
  end;
end;

procedure TEditorFrame.EditorOnKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  //fire on_key_up only for keys Ctrl, Alt, Shift
  //event result is ignored
  case Key of
    VK_CONTROL,
    VK_MENU,
    VK_SHIFT,
    VK_RSHIFT:
      DoPyEvent(Sender as TATSynEdit, cEventOnKeyUp,
        [
        AppVariant(Key),
        AppVariant(ConvertShiftStateToString(Shift))
        ]);
  end;
end;

procedure TEditorFrame.EditorOnPaste(Sender: TObject; var AHandled: boolean;
  AKeepCaret, ASelectThen: boolean);
begin
  if DoPyEvent(Sender as TATSynEdit, cEventOnPaste,
    [
    AppVariant(AKeepCaret),
    AppVariant(ASelectThen)
    ]).Val = evrFalse then
    AHandled:= true;
end;

procedure TEditorFrame.EditorOnScroll(Sender: TObject);
begin
  if Assigned(FOnEditorScroll) then
    FOnEditorScroll(Sender);

  DoPyEvent(Sender as TATSynEdit, cEventOnScroll, []);
end;

function TEditorFrame.GetTabPages: TATPages;
begin
  Result:= Parent as TATPages;
end;

function TEditorFrame.GetTabGroups: TATGroups;
begin
  Result:= (Parent as TATPages).Owner as TATGroups;
end;

procedure TEditorFrame.DoShow;
var
  an: TecSyntAnalyzer;
begin
  //analyze file, when frame is shown for the 1st time
  if AppAllowFrameParsing and not FWasVisible then
  begin
    FWasVisible:= true;

    an:= LexerInitial[Ed1];
    if Assigned(an) then
    begin
      Lexer[Ed1]:= an;
      LexerInitial[Ed1]:= nil;
    end;

    if not EditorsLinked then
    begin
      an:= LexerInitial[Ed2];
      if Assigned(an) then
      begin
        Lexer[Ed2]:= an;
        LexerInitial[Ed2]:= nil;
      end;
    end;

    //fix #4559
    EditorForceUpdateIfWrapped(Ed1);
    if Splitted then
      EditorForceUpdateIfWrapped(Ed2);
  end;
end;

procedure TEditorFrame.RestoreSavedLexer(Ed: TATSynEdit);
var
  EdIndex: integer;
  Ada: TATAdapterHilite;
begin
  EdIndex:= EditorObjToIndex(Ed);
  if EdIndex<0 then exit;
  Ada:= FLexerBackup[EdIndex];
  if Assigned(Ada) then
  begin
    Ed.AdapterForHilite:= Ada;
    FLexerBackup[EdIndex]:= nil;
    Ed.Update;
    DoOnUpdateStatusbar; //replace lexer name 'none' to restored one
  end;
end;

procedure TEditorFrame.TimerChangeTimer(Sender: TObject);
var
  EdIndex: integer;
begin
  TimerChange.Enabled:= false;

  RestoreSavedLexer(Ed1);
  if Splitted then
    RestoreSavedLexer(Ed2);

  for EdIndex:= 0 to 1 do
    if FTextChange[EdIndex] then
    begin
      FTextChange[EdIndex]:= false;

      if Assigned(FOnChangeSlow) then
        FOnChangeSlow(EditorIndexToObj(EdIndex));

      FTextChangeSlow[EdIndex]:= true;
    end;
end;

procedure TEditorFrame.TimerCaretTimer(Sender: TObject);
var
  Ed: TATSynEdit;
begin
  TimerCaret.Enabled:= false;

  Ed:= Editor;
  DoPyEvent(Ed, cEventOnCaretSlow, []);

  if FBracketHilite then
    EditorBracket_Action(Ed,
      bracketActionHilite,
      FBracketSymbols,
      FBracketMaxDistance
      );
end;


procedure TEditorFrame.NotifReloadYesClick(Sender: TObject);
var
  EdIndex: integer;
  Ed: TATSynEdit;
begin
  EdIndex:= (Sender as TComponent).Tag;
  Ed:= EditorIndexToObj(EdIndex);
  if Ed=nil then exit;
  DoFileReload(Ed);
  EditorFocus(Ed);
end;

procedure TEditorFrame.NotifReloadNoClick(Sender: TObject);
var
  EdIndex: integer;
  Ed: TATSynEdit;
begin
  EdIndex:= (Sender as TComponent).Tag;
  Ed:= EditorIndexToObj(EdIndex);
  if Ed=nil then exit;
  DoHideNotificationPanel(NotifReloadControls[EdIndex]);
  EditorFocus(Ed);
end;

procedure TEditorFrame.NotifReloadStopClick(Sender: TObject);
begin
  NotifEnabled:= false;
  NotifReloadNoClick(Sender);
end;

procedure TEditorFrame.NotifDeletedStopClick(Sender: TObject);
begin
  NotifDeletedEnabled:= false;
  NotifDeletedNoClick(Sender);
end;

procedure TEditorFrame.NotifDeletedNoClick(Sender: TObject);
var
  EdIndex: integer;
  Ed: TATSynEdit;
begin
  EdIndex:= (Sender as TComponent).Tag;
  Ed:= EditorIndexToObj(EdIndex);
  if Ed=nil then exit;
  DoHideNotificationPanel(NotifDeletedControls[EdIndex]);
  EditorFocus(Ed);
end;

procedure TEditorFrame.NotifDeletedYesClick(Sender: TObject);
var
  EdIndex: integer;
  Ed: TATSynEdit;
begin
  EdIndex:= (Sender as TComponent).Tag;
  Ed:= EditorIndexToObj(EdIndex);
  if Ed=nil then exit;
  Ed.DoCommand(cmd_FileClose, cInvokeAppInternal);
end;

procedure TEditorFrame.EditorOnCalcBookmarkColor(Sender: TObject;
  ABookmarkKind: integer; var AColor: TColor);
begin
  if ABookmarkKind>1 then
  begin
    AColor:= AppBookmarkSetup[ABookmarkKind].Color;
    if AColor=clDefault then
      AColor:= GetAppColor(apclEdBookmarkBg);
  end;
end;

procedure TEditorFrame.EditorOnChangeCaretPos(Sender: TObject);
{$ifdef linux}
const
  cMaxSelectedLinesForAutoCopy = 200;
{$endif}
var
  Ed: TATSynEdit;
begin
  if AppSessionIsLoading or AppSessionIsClosing then exit;

  Ed:= Sender as TATSynEdit;
  if Assigned(FOnEditorChangeCaretPos) then
    FOnEditorChangeCaretPos(Sender);

  DoOnUpdateStatusbar;

  //support Primary Selection on Linux
  {$ifdef linux}
  EditorCopySelToPrimarySelection(Ed, cMaxSelectedLinesForAutoCopy);
  {$endif}

  //on_caret, now
  DoPyEvent(Editor, cEventOnCaret, []);

  //on_caret_slow, later
  TimerCaret.Enabled:= false;
  TimerCaret.Interval:= UiOps.PyCaretSlow;
  TimerCaret.Enabled:= true;
end;

procedure TEditorFrame.EditorOnHotspotEnter(Sender: TObject; AHotspotIndex: integer);
begin
  DoPyEvent(Sender as TATSynEdit, cEventOnHotspot,
    [
    AppVariant(true), //hotspot enter
    AppVariant(AHotspotIndex)
    ]);
end;

procedure TEditorFrame.EditorOnHotspotExit(Sender: TObject; AHotspotIndex: integer);
begin
  DoPyEvent(Sender as TATSynEdit, cEventOnHotspot,
    [
    AppVariant(false), //hotspot exit
    AppVariant(AHotspotIndex)
    ]);
end;

function _ContrastColor(AColor: TColor): TColor;
var
  bLight: boolean;
  red, green, blue: integer;
begin
  red:= AColor and $FF;
  green:= AColor shr 8 and $FF;
  blue:= AColor shr 16 and $FF;
  // Use different scaling with red, green, and blue to account
  // for perceived intensity. Addresses issue #3624
  // See https://www.w3.org/TR/AERT/#color-contrast
  // Color brightness can determined by the following formula:
  // ((Red value X 299) + (Green value X 587) + (Blue value X 114)) / 1000
  // ((299+587+114) * 128) = 128000
  // ((299+587+114) * $80) = $1f400
  bLight:= red*299 + green*587 + blue*114 > $1f400;
  Result:= UiOps.HtmlBackgroundColorPair[bLight];
end;


procedure TEditorFrame.EditorOnDrawLine(Sender: TObject; C: TCanvas;
  ALineIndex, AX, AY: integer; const AStr: atString; const ACharSize: TATEditorCharSize;
  constref AExtent: TATIntFixedArray);
var
  Ed: TATSynEdit;
  NLineWidth: integer;
  bColorizeBack, bColorizeInBrackets, bFoundBrackets: boolean;
  X1, X2, Y, NLen, NStartPos: integer;
  NColor: TColor;
  i: integer;
begin
  if AStr='' then Exit;

  if not IsFilenameListedInExtensionList(FileName, EditorOps.OpUnderlineColorFiles)
    then exit;

  //skip lines in binary files, e.g. *.dll
  for i:= 1 to Length(AStr) do
    case Ord(AStr[i]) of
      0..8:
        exit;
    end;

  NLineWidth:= EditorOps.OpUnderlineColorSize;
  bColorizeBack:= NLineWidth>=10;
  bColorizeInBrackets:= NLineWidth=11;

  //avoid background hilite for lines with selection
  if bColorizeBack then
  begin
    if Sender=nil then
      raise Exception.Create('Sender=nil in TEditorFrame.EditorOnDrawLine');
    Ed:= Sender as TATSynEdit;
    if Ed.Carets.IsLineWithSelection(ALineIndex) then exit;
  end;

  for i:= 1 to Length(AStr)-3 do
  begin
    NColor:= clNone;
    NLen:= 0;
    bFoundBrackets:= false;

    if (i>1) and (AStr[i-1]='&') then Continue; //skip HTML tokens like &#123; and &nnnn;

    case AStr[i] of
      '#':
        begin
          //find #rgb, #rrggbb
          if IsCharHexDigit(AStr[i+1]) then
          begin
            //don't allow word-char before
            if (i>1) and IsCharWord(AStr[i-1], ATEditorOptions.DefaultNonWordChars) then Continue;

            NColor:= TATHtmlColorParserW.ParseTokenRGB(@AStr[i+1], NLen, clNone);
            Inc(NLen);
          end;
        end;
      'r':
        begin
          //find rgb(...), rgba(...)
          if (AStr[i+1]='g') and
            (AStr[i+2]='b')
          then
          begin
            //don't allow word-char before
            if (i>1) and IsCharWord(AStr[i-1], ATEditorOptions.DefaultNonWordChars) then Continue;

            NColor:= TATHtmlColorParserW.ParseFunctionRGB(AStr, i, NLen);
            bFoundBrackets:= true;
          end;
        end;
      'h':
        begin
          //find hsl(...), hsla(...)
          if (AStr[i+1]='s') and
            (AStr[i+2]='l')
          then
          begin
            //don't allow word-char before
            if (i>1) and IsCharWord(AStr[i-1], ATEditorOptions.DefaultNonWordChars) then Continue;

            NColor:= TATHtmlColorParserW.ParseFunctionHSL(AStr, i, NLen);
            bFoundBrackets:= true;
          end;
        end;
    end;

    //render the found color
    if NColor<>clNone then
    begin
      if bColorizeInBrackets and bFoundBrackets then
      begin
        NStartPos:= PosEx('(', AStr, i+2)+1;
        NLen:= Max(1, NLen-1-(NStartPos-i))
      end
      else
        NStartPos:= i;

      if (NStartPos-2>=0) and (NStartPos-2<cMaxFixedArray) then
        X1:= AX+AExtent.Data[NStartPos-2]
      else
        X1:= AX;

      if NStartPos-2+NLen<cMaxFixedArray then
        X2:= AX+AExtent.Data[NStartPos-2+NLen]
      else
        X2:= 0;

      Y:= AY+ACharSize.Y;

      if bColorizeBack then
      begin
        C.Font.Color:= _ContrastColor(NColor);
        C.Font.Style:= [];
        C.Brush.Color:= NColor;
        CanvasTextOutSimplest(C, X1, AY, Copy(AStr, NStartPos, NLen));
      end
      else
      begin
        C.Brush.Color:= NColor;
        C.FillRect(X1, Y-NLineWidth, X2, Y);
      end;
    end;
  end;
end;


function TEditorFrame.GetLineEnds(Ed: TATSynEdit): TATLineEnds;
begin
  Result:= Ed.Strings.Endings;
end;

function TEditorFrame.GetPictureScale: integer;
begin
  if Assigned(FImageBox) then
    Result:= FImageBox.ImageZoom
  else
    Result:= 100;
end;

function TEditorFrame.GetReadOnly(Ed: TATSynEdit): boolean;
begin
  Result:= Ed.ModeReadOnly;
end;

function TEditorFrame.GetSaveDialog: TSaveDialog;
begin
  if not Assigned(FSaveDialog) then
    if Assigned(FOnGetSaveDialog) then
      FOnGetSaveDialog(FSaveDialog);
  Result:= FSaveDialog;
end;

function TEditorFrame.GetTabKeyCollectMarkers: boolean;
begin
  Result:= FTabKeyCollectMarkers and (Editor.Markers.Count>0);
end;

function TEditorFrame.GetUnprintedEnds: boolean;
begin
  Result:= Ed1.OptUnprintedEnds;
end;

function TEditorFrame.GetUnprintedEndsDetails: boolean;
begin
  Result:= Ed1.OptUnprintedEndsDetails;
end;

function TEditorFrame.GetUnprintedShow: boolean;
begin
  Result:= Ed1.OptUnprintedVisible;
end;

function TEditorFrame.GetUnprintedSpaces: boolean;
begin
  Result:= Ed1.OptUnprintedSpaces;
end;

procedure TEditorFrame.SetFileName(const AValue: string);
begin
  if SameFileName(FFileName, AValue) then Exit;
  FFileName:= AValue;
  AppGetFileProps(FFileName, FileProps[0]);
end;

procedure TEditorFrame.UpdateTabTooltip;
var
  Gr: TATGroups;
  Pages: TATPages;
  NLocalGroups, NGlobalGroup, NTab: integer;
  D: TATTabData;
  SHint: string;
begin
  GetFrameLocation(Self, Gr, Pages, NLocalGroups, NGlobalGroup, NTab);
  D:= Pages.Tabs.GetTabData(NTab);
  if Assigned(D) then
  begin
    SHint:= '';
    if EditorsLinked then
    begin
      if FFileName<>'' then
        SHint:= AppCollapseHomeDirInFilename(FFileName);
    end
    else
    begin
      if (FFileName<>'') or (FFileName2<>'') then
        SHint:= AppCollapseHomeDirInFilename(FFileName) + #10 +
                AppCollapseHomeDirInFilename(FFileName2);
    end;

    D.TabHint:= SHint;
    Pages.Tabs.UpdateTabTooltip;
  end;
end;

procedure TEditorFrame.SetFileName2(const AValue: string);
begin
  if SameFileName(FFileName2, AValue) then Exit;
  FFileName2:= AValue;
  AppGetFileProps(FFileName2, FileProps[1]);
end;

procedure TEditorFrame.SetFileWasBig(Ed: TATSynEdit; AValue: boolean);
var
  EdIndex: integer;
begin
  if EditorsLinked then
  begin
    FFileWasBig[0]:= AValue;
    FFileWasBig[1]:= AValue;
    if AValue then
    begin
      EditorAdjustForBigFile(Ed1);
      EditorAdjustForBigFile(Ed2);
    end;
  end
  else
  begin
    EdIndex:= EditorObjToIndex(Ed);
    if EdIndex<0 then exit;
    FFileWasBig[EdIndex]:= AValue;
    if AValue then
      EditorAdjustForBigFile(Ed);
  end;
end;

procedure TEditorFrame.SetInitialLexer(Ed: TATSynEdit; AValue: TecSyntAnalyzer);
var
  EdIndex: integer;
begin
  if EditorsLinked then
    FInitialLexer1:= AValue
  else
  begin
    EdIndex:= EditorObjToIndex(Ed);
    if EdIndex<0 then exit;
    if EdIndex=0 then
      FInitialLexer1:= AValue
    else
      FInitialLexer2:= AValue;
  end;
end;

procedure TEditorFrame.SetLocked(AValue: boolean);
begin
  if AValue=FLocked then exit;
  FLocked:= AValue;

  if FLocked then
  begin
    Ed1.BeginUpdate;
    Ed2.BeginUpdate;
  end
  else
  begin
    Ed1.EndUpdate;
    Ed2.EndUpdate;
  end;
end;

procedure TEditorFrame.SetPictureScale(AValue: integer);
begin
  if Assigned(FImageBox) then
  begin
    if AValue>0 then
      FImageBox.ImageZoom:= AValue
    else
    if AValue=-1 then
      FImageBox.OptFitToWindow:= true;
  end;
end;

procedure TEditorFrame.SetReadOnly(Ed: TATSynEdit; AValue: boolean);
begin
  if Ed.ModeReadOnly=AValue then exit;
  Ed.ModeReadOnly:= AValue;
  if (Ed=Ed1) and EditorsLinked then
    Ed2.ModeReadOnly:= AValue;
  DoPyEventState(Ed, EDSTATE_READONLY);
end;

procedure TEditorFrame.UpdateEds(AUpdateWrapInfo: boolean = false);
begin
  Ed2.OptUnprintedVisible:= Ed1.OptUnprintedVisible;
  Ed2.OptUnprintedSpaces:= Ed1.OptUnprintedSpaces;
  Ed2.OptUnprintedEnds:= Ed1.OptUnprintedEnds;
  Ed2.OptUnprintedEndsDetails:= Ed1.OptUnprintedEndsDetails;

  Ed1.Update(AUpdateWrapInfo);
  Ed2.Update(AUpdateWrapInfo);
end;

function TEditorFrame.GetLexer(Ed: TATSynEdit): TecSyntAnalyzer;
begin
  Result:= LexerInitial[Ed];
  if Assigned(Result) then exit;

  if Ed.AdapterForHilite is TATAdapterEControl then
    Result:= TATAdapterEControl(Ed.AdapterForHilite).Lexer
  else
    Result:= nil;
end;

function TEditorFrame.GetLexerLite(Ed: TATSynEdit): TATLiteLexer;
begin
  if Ed.AdapterForHilite is TATLiteLexer then
    Result:= TATLiteLexer(Ed.AdapterForHilite)
  else
    Result:= nil;
end;

function TEditorFrame.GetLexerName(Ed: TATSynEdit): string;
var
  CurAdapter: TATAdapterHilite;
  an: TecSyntAnalyzer;
begin
  Result:= '';

  an:= LexerInitial[Ed];
  if Assigned(an) then
    exit(an.LexerName);

  CurAdapter:= Ed.AdapterForHilite;
  if CurAdapter=nil then exit;

  if CurAdapter is TATAdapterEControl then
  begin
    an:= TATAdapterEControl(CurAdapter).Lexer;
    if Assigned(an) then
      Result:= an.LexerName;
  end
  else
  if CurAdapter is TATLiteLexer then
  begin
    Result:= TATLiteLexer(CurAdapter).LexerName+msgLiteLexerSuffix;
  end;
end;


procedure TEditorFrame.SetLexerName(Ed: TATSynEdit; const AValue: string);
var
  SName: string;
  anLite: TATLiteLexer;
begin
  if AValue='' then
  begin
    if EditorsLinked then
    begin
      Ed1.AdapterForHilite:= nil;
      Ed2.AdapterForHilite:= nil;
      Adapter1.Lexer:= nil;
    end
    else
    begin
      Ed.AdapterForHilite:= nil;
      Lexer[Ed]:= nil;
      Ed.Update;
    end;
    exit;
  end;

  if SEndsWith(AValue, msgLiteLexerSuffix) then
  begin
    SName:= Copy(AValue, 1, Length(AValue)-Length(msgLiteLexerSuffix));
    anLite:= AppManagerLite.FindLexerByName(SName);
    if Assigned(anLite) then
      LexerLite[Ed]:= anLite
    else
      Lexer[Ed]:= nil;
  end
  else
  begin
    Lexer[Ed]:= AppManager.FindLexerByName(AValue);
  end;
end;

procedure TEditorFrame.SetSplitHorz(AValue: boolean);
var
  al: TAlign;
begin
  FSplitHorz:= AValue;
  if FrameKind<>efkEditor then exit;

  al:= cSplitHorzToAlign[AValue];
  Splitter.Align:= al;
  Ed2.Align:= al;

  //restore relative splitter ratio (e.g. 50%)
  SplitPos:= SplitPos;
end;

procedure TEditorFrame.SetSplitPos(AValue: double);
const
  Delta = 70;
var
  N: integer;
begin
  if FrameKind<>efkEditor then exit;
  if not Splitted then exit;

  AValue:= Max(0.0, Min(1.0, AValue));
  FSplitPos:= AValue;

  if FSplitHorz then
  begin
    N:= Round(AValue*Height);
    Ed2.Height:= Max(Delta, Min(Height-Delta, N));
    Splitter.Top:= 0;
  end
  else
  begin
    N:= Round(AValue*Width);
    Ed2.Width:= Max(Delta, Min(Width-Delta, N));
    Splitter.Left:= 0;
  end;
end;

procedure TEditorFrame.SetSplitted(AValue: boolean);
begin
  if FrameKind<>efkEditor then exit;
  if GetSplitted=AValue then exit;

  if not AValue and Ed2.Focused then
    if Ed1.CanFocus then
      Ed1.SetFocus;

  Ed2.Visible:= AValue;
  Splitter.Visible:= AValue;

  if AValue then
  begin
    SplitPos:= 0.5;
    //enable linking
    if FEditorsLinked then
    begin
      Ed2.Strings:= Ed1.Strings;
      //Ed1.BrotherEditor:= Ed2;
      //Ed2.BrotherEditor:= Ed1;
    end;
  end
  else
  begin
    //disable linking
    Ed2.Strings:= nil;
    //Ed1.BrotherEditor:= nil;
    //Ed2.BrotherEditor:= nil;
  end;

  Ed2.Update(true);
end;

procedure TEditorFrame.EditorOnChange(Sender: TObject);
var
  Ed, EdOther: TATSynEdit;
  Caret: TATCaretItem;
  St: TATStrings;
  EdIndex: integer;
  bChangedLexer, bChanged1, bChanged2: boolean;
begin
  if AppSessionIsLoading then exit; //fix issue #4436

  Ed:= Sender as TATSynEdit;
  St:= Ed.Strings;

  if Ed=Ed1 then
    EdOther:= Ed2
  else
    EdOther:= Ed1;

  bChangedLexer:= false;
  bChanged1:= false;
  bChanged2:= false;

  //temporary turn off lexer, when editing too long line
  if Assigned(Ed.AdapterForHilite) and (Ed.Carets.Count>0) then
  begin
    Caret:= Ed.Carets[0];
    if St.IsIndexValid(Caret.PosY) then
      if (UiOps.MaxLineLenForEditingKeepingLexer>0) and
        (St.LinesLen[Caret.PosY]>=UiOps.MaxLineLenForEditingKeepingLexer) then
        begin
          FLexerBackup[EditorObjToIndex(Ed)]:= Ed.AdapterForHilite;
          Ed.AdapterForHilite:= nil;

          if Splitted and EditorsLinked then
          begin
           FLexerBackup[EditorObjToIndex(EdOther)]:= EdOther.AdapterForHilite;
           EdOther.AdapterForHilite:= nil;
          end;

          bChangedLexer:= true;
        end;
  end;

  bChanged1:= Ed.Markers.DeleteWithTag(UiOps.FindOccur_TagValue);
  if FBracketHilite then
    bChanged2:= EditorBracket_ClearHilite(Ed);

  if bChangedLexer or bChanged1 or bChanged2 then
    Ed.Update;

  //sync changes in 2 editors, when frame is splitted
  if Splitted and EditorsLinked then
  begin
    EdOther.DoCaretsFixIncorrectPos(false);
    EdOther.Update(true);
  end;

  DoPyEvent(Ed, cEventOnChange, []);

  TimerChange.Enabled:= false;
  TimerChange.Interval:= UiOps.PyChangeSlow;
  TimerChange.Enabled:= true;

  EdIndex:= EditorObjToIndex(Ed);
  if EdIndex>=0 then
    FTextChange[EdIndex]:= true;
end;

procedure TEditorFrame.EditorOnChangeModified(Sender: TObject);
begin
  if AppSessionIsLoading then exit; //fix issue #4436
  UpdateModified(Sender as TATSynEdit);
end;

procedure TEditorFrame.EditorOnChangeState(Sender: TObject);
begin
  DoOnUpdateState;
end;

procedure TEditorFrame.EditorOnChangeBookmarks(Sender: TObject);
begin
  DoPyEventState(Sender as TATSynEdit, EDSTATE_BOOKMARK);
end;

procedure TEditorFrame.EditorOnChangeZoom(Sender: TObject);
begin
  DoOnUpdateZoom;
  DoPyEventState(Sender as TATSynEdit, EDSTATE_ZOOM);
end;

procedure TEditorFrame.UpdateModified(Ed: TATSynEdit; AWithEvent: boolean);
begin
  //when modified, remove "Preview tab" style (italic caption)
  if (Ed=Ed1) and Ed.Modified then
    DoRemovePreviewStyle;

  UpdateCaptionFromFilename;

  if AWithEvent then
    DoPyEventState(Ed, EDSTATE_MODIFIED);

  DoOnUpdateStatusbar;
end;

procedure TEditorFrame.UpdatePinned(Ed: TATSynEdit; AWithEvent: boolean);
begin
  if TabPinned then
    DoRemovePreviewStyle;

  if AWithEvent then
    DoPyEventState(Ed, EDSTATE_PINNED);
end;


procedure TEditorFrame.EditorOnEnter(Sender: TObject);
var
  IsEd2: boolean;
begin
  IsEd2:= Sender=Ed2;
  if IsEd2<>FActiveSecondaryEd then
  begin
    FActiveSecondaryEd:= IsEd2;
    DoOnUpdateStatusbar;
  end;

  if Assigned(FOnFocusEditor) then
    FOnFocusEditor(Sender);

  DoPyEvent(Sender as TATSynEdit, cEventOnFocus, []);

  FActivationTime:= GetTickCount64;
end;

procedure TEditorFrame.EditorOnCommand(Sender: TObject; ACmd: integer;
  AInvoke: TATEditorCommandInvoke; const AText: string; var AHandled: boolean);
var
  Ed: TATSynEdit;
  NCarets: integer;
  ch: char;
begin
  Ed:= Sender as TATSynEdit;
  NCarets:= Ed.Carets.Count;
  if NCarets=0 then exit;

  case ACmd of
    cCommand_TextInsert:
      begin
        if NCarets>1 then
          if not Ed.OptAutoPairForMultiCarets then
            exit;

        if Length(AText)=1 then
        begin
          ch:= AText[1];

          //auto-close bracket
          if Pos(ch, Ed.OptAutoPairChars)>0 then
            if EditorAutoPairChar(Ed, ch) then
            begin
              AHandled:= true;
              Ed.DoEventCarets; //to highlight pair brackets after typing bracket
              exit
            end;

          //auto-skip closing bracket in case 'f(|)'
          if EditorAutoSkipClosingBracket(Ed, ch) then
          begin
            AHandled:= true;
            Ed.Update;
            exit;
          end;
        end;
      end;

    cCommand_KeyBackspace:
      begin
        if EditorAutoDeleteClosingBracket(Ed) then
        begin
          AHandled:= true;
          Ed.DoEventChange(Ed.Carets[0].FirstTouchedLine);
          exit;
        end;
      end;

    cCommand_KeyTab,
    cCommand_KeyEnter,
    cCommand_TextDeleteLine,
    cCommand_TextDeleteToLineBegin,
    cCommand_KeyUp,
    cCommand_KeyUp_Sel,
    cCommand_KeyDown,
    cCommand_KeyDown_Sel,
    cCommand_KeyLeft,
    cCommand_KeyLeft_Sel,
    cCommand_KeyRight,
    cCommand_KeyRight_Sel,
    cCommand_KeyHome,
    cCommand_KeyHome_Sel,
    cCommand_KeyEnd,
    cCommand_KeyEnd_Sel,
    cCommand_KeyPageUp,
    cCommand_KeyPageUp_Sel,
    cCommand_KeyPageDown,
    cCommand_KeyPageDown_Sel,
    cCommand_TextDeleteWordNext,
    cCommand_TextDeleteWordPrev:
      begin
        CancelAutocompleteAutoshow;
      end;

    cCommand_ClipboardPaste,
    cCommand_ClipboardPaste_Select,
    cCommand_ClipboardPaste_KeepCaret,
    cCommand_ClipboardPaste_Column,
    cCommand_ClipboardPaste_ColumnKeepCaret,
    cCommand_ClipboardPasteAndIndent:
      begin
        Adapter[Ed].StopTreeUpdate;
        Adapter[Ed].Stop;
      end;
  end;

  if Assigned(FOnEditorCommand) then
    FOnEditorCommand(Sender, ACmd, AInvoke, AText, AHandled);
end;

procedure TEditorFrame.EditorOnCommandAfter(Sender: TObject; ACommand: integer;
  const AText: string);
var
  Ed: TATSynEdit;
  Caret: TATCaretItem;
  bTypedChar: boolean;
  NValue: integer;
  charW: WideChar;
begin
  Ed:= Sender as TATSynEdit;

  if FrameKind=efkBinaryViewer then
  begin
    case ACommand of
      cCommand_ZoomIn:
        begin
          Binary.IncreaseFontSize(true);
        end;
      cCommand_ZoomOut:
        begin
          Binary.IncreaseFontSize(false);
        end;
      cCommand_ZoomReset:
        begin
          Binary.Font.Size:= EditorOps.OpFontSize;
          Binary.Redraw;
        end;
      cCommand_ToggleUnprinted:
        begin
          Binary.TextNonPrintable:= not Binary.TextNonPrintable;
          Binary.Redraw;
        end;
    end;
    exit;
  end;

  if Ed.Carets.Count=0 then exit; //allow multi-carets, for auto-pairing of tags
  Caret:= Ed.Carets[0];
  if not Ed.Strings.IsIndexValid(Caret.PosY) then exit;
  bTypedChar:= false;

  //some commands affect FTextCharsTyped
  case ACommand of
    cCommand_KeyBackspace:
      begin
        if FTextCharsTyped>0 then
        begin
          Dec(FTextCharsTyped);
          CancelAutocompleteAutoshow;
        end;
        exit;
      end;

    cCommand_ToggleReadOnly:
      begin
        DoPyEventState(Ed, EDSTATE_READONLY);
        exit;
      end;

    cCommand_ToggleWordWrap,
    cCommand_ToggleWordWrapAlt:
      begin
        DoPyEventState(Ed, EDSTATE_WRAP);
        exit;
      end;

    cCommand_ZoomIn,
    cCommand_ZoomOut,
    cCommand_ZoomReset:
      begin
        if ACommand=cCommand_ZoomReset then
        begin
          NValue:= ATEditorScaleFontPercents;
          if NValue=0 then
            NValue:= ATEditorScalePercents;
        end
        else
          NValue:= Ed.OptScaleFont;
        OnMsgStatus(Self, Format(msgStatusFontSizeChanged, [NValue]));
        exit;
      end;

    cCommand_TextInsert:
      begin
        if AText='' then exit;

        if AText='>' then
        begin
          EditorAutoCloseOpeningHtmlTag(Ed, Caret.PosX, Caret.PosY);
          Ed.Update; //fix missed repainting
          exit;
        end;

        if AText='/' then
        begin
          EditorAutoCloseClosingHtmlTag(Ed, Caret.PosX, Caret.PosY);
          Ed.Update; //fix missed repainting
          exit;
        end;

        //autoshow autocompletion after typing N letters
        bTypedChar:= (Length(AText)=1) or (UTF8Length(AText)=1);
        if bTypedChar then
        begin
          if EditorAutoCompletionAfterTypingChar(Ed, AText, FTextCharsTyped) then
            exit;

          if Length(AText)=1 then
            charW:= WideChar(Ord(AText[1]))
          else
            charW:= UTF8Decode(AText)[1];

          if not IsCharWord(charW, Ed.OptNonWordChars) then
            CancelAutocompleteAutoshow;
        end;
      end;
  end; //case ACommand of

  if Ed.LastCommandChangedLines>0 then
    if Assigned(FOnMsgStatus) then
      FOnMsgStatus(Self, Format(msgStatusChangedLinesCount, [Ed.LastCommandChangedLines]));
end;

procedure TEditorFrame.EditorOnClickDouble(Sender: TObject; var AHandled: boolean);
var
  Res: TAppPyEventResult;
begin
  Res:= DoPyEvent(Sender as TATSynEdit, cEventOnClickDbl,
    [
    AppVariant(ConvertShiftStateToString(KeyboardStateToShiftState))
    ]);
  AHandled:= Res.Val=evrFalse;
end;

procedure TEditorFrame.EditorOnClickLink(Sender: TObject; const ALink: string);
var
  Res: TAppPyEventResult;
  bHandled: boolean;
begin
  Res:= DoPyEvent(Sender as TATSynEdit, cEventOnClickLink,
    [
    AppVariant(ConvertShiftStateToString(KeyboardStateToShiftState)),
    AppVariant(ALink)
    ]);
  bHandled:= Res.Val=evrFalse;
  if not bHandled then
    if Assigned(FOnAppClickLink) then
      FOnAppClickLink(Sender, ALink);
end;

procedure TEditorFrame.EditorOnClickMicroMap(Sender: TObject; AX, AY: integer);
var
  Ed: TATSynEdit;
begin
  Ed:= Sender as TATSynEdit;
  AY:= AY * Ed.Strings.Count div Ed.ClientHeight;
  Ed.DoGotoPos(
    Point(0, AY),
    Point(-1, -1),
    UiOps.FindIndentHorz,
    UiOps.FindIndentVert,
    false{APlaceCaret}, //user asked to not move caret on micromap click
    true
    );
end;

procedure TEditorFrame.EditorOnClickGap(Sender: TObject;
  AGapItem: TATGapItem; APos: TPoint);
var
  Ed: TATSynEdit;
  W, H: integer;
begin
  if not Assigned(AGapItem) then exit;
  Ed:= Sender as TATSynEdit;

  if Assigned(AGapItem.Bitmap) then
  begin
    W:= AGapItem.Bitmap.Width;
    H:= AGapItem.Bitmap.Height;
  end
  else
  begin
    W:= 0;
    H:= 0;
  end;

  DoPyEvent(Ed, cEventOnClickGap,
    [
    AppVariant(ConvertShiftStateToString(KeyboardStateToShiftState)),
    AppVariant(AGapItem.LineIndex),
    AppVariant(AGapItem.Tag),
    AppVariant(W),
    AppVariant(H),
    AppVariant(APos.X),
    AppVariant(APos.Y)
    ]);
end;


procedure TEditorFrame.DoOnResize;
var
  N: integer;
begin
  //for debugging only
  N:= Width;

  inherited;

  //this keeps ratio of splitter (e.g. 50%) on form resize
  SplitPos:= SplitPos;
end;

procedure TEditorFrame.InitEditor(var ed: TATSynEdit; const AName: string);
begin
  ed:= TATSynEdit.Create(FFormDummy);
  ed.ParentFrameObject:= Self;
  ed.Name:= AName;
  ed.Parent:= FFormDummy;
  DoControl_InitPropsObject(ed, FFormDummy, 'editor');

  ed.DoubleBuffered:= UiOps.DoubleBuffered;

  ed.Font.Name:= EditorOps.OpFontName;
  ed.FontItalic.Name:= EditorOps.OpFontName_i;
  ed.FontBold.Name:= EditorOps.OpFontName_b;
  ed.FontBoldItalic.Name:= EditorOps.OpFontName_bi;

  ed.Font.Size:= EditorOps.OpFontSize;
  ed.FontItalic.Size:= EditorOps.OpFontSize_i;
  ed.FontBold.Size:= EditorOps.OpFontSize_b;
  ed.FontBoldItalic.Size:= EditorOps.OpFontSize_bi;

  ed.Font.Quality:= EditorOps.OpFontQuality;

  ed.BorderStyle:= bsNone;
  ed.Keymap:= AppKeymapMain;
  ed.TabStop:= false;
  ed.OptUnprintedVisible:= EditorOps.OpUnprintedShow;
  ed.OptRulerVisible:= EditorOps.OpRulerShow;
  ed.OptBorderVisible:= false;
  ed.CommandLog.MaxCount:= EditorOps.OpCommandLogMaxCount;

  ed.OnClick:= @EditorOnClick;
  ed.OnClickLink:=@EditorOnClickLink;
  ed.OnClickDouble:= @EditorOnClickDouble;
  ed.OnClickMoveCaret:= @EditorClickMoveCaret;
  ed.OnClickEndSelect:= @EditorClickEndSelect;
  ed.OnClickGap:= @EditorOnClickGap;
  ed.OnClickMicromap:= @EditorOnClickMicroMap;
  ed.OnEnter:= @EditorOnEnter;
  ed.OnChange:= @EditorOnChange;
  ed.OnChangeModified:= @EditorOnChangeModified;
  ed.OnChangeCaretPos:= @EditorOnChangeCaretPos;
  ed.OnChangeState:= @EditorOnChangeState;
  ed.OnChangeZoom:= @EditorOnChangeZoom;
  ed.OnChangeBookmarks:= @EditorOnChangeBookmarks;
  ed.OnCommand:= @EditorOnCommand;
  ed.OnCommandAfter:= @EditorOnCommandAfter;
  ed.OnClickGutter:= @EditorOnClickGutter;
  ed.OnCalcBookmarkColor:= @EditorOnCalcBookmarkColor;
  ed.OnDrawBookmarkIcon:= @EditorOnDrawBookmarkIcon;
  ed.OnDrawLine:= @EditorOnDrawLine;
  ed.OnKeyDown:= @EditorOnKeyDown;
  ed.OnKeyUp:= @EditorOnKeyUp;
  ed.OnDrawMicromap:= @EditorDrawMicromap;
  ed.OnPaste:=@EditorOnPaste;
  ed.OnScroll:=@EditorOnScroll;
  ed.OnHotspotEnter:=@EditorOnHotspotEnter;
  ed.OnHotspotExit:=@EditorOnHotspotExit;
  ed.ScrollbarVert.OnOwnerDraw:= @EditorDrawScrollbarVert;
end;

constructor TEditorFrame.Create(AOwner: TComponent; AApplyCentering: boolean);
begin
  inherited Create(AOwner);

  FNotifEnabled:= true;
  FNotifDeletedEnabled:= true;

  //we need TFormDummy object to allow working with controls via API dlg_proc
  FFormDummy:= TFormDummy.Create(Self);
  FFormDummy.Visible:= false;
  FFormDummy.BorderStyle:= bsNone;
  FFormDummy.KeyPreview:= false; //to not break the 'cancel carets (Esc)' command
  FFormDummy.Align:= alClient;
  FFormDummy.Parent:= Self;
  FFormDummy.Visible:= true;

  Splitter:= TSplitter.Create(FFormDummy);
  Splitter.Parent:= FFormDummy;
  Splitter.Name:= 'split';
  Splitter.Align:= alBottom;
  Splitter.AutoSnap:= false;
  Splitter.ResizeStyle:= rsPattern;
  Splitter.MinSize:= 100;
  Splitter.Color:= GetAppColor(apclSplitMain);
  DoControl_InitPropsObject(Splitter, FFormDummy, 'splitter');

  FFileName:= '';
  FFileName2:= '';
  FActiveSecondaryEd:= false;
  FTabColor:= clNone;
  FTabFontColor:= clNone;
  Inc(FLastTabId);
  FTabId:= FLastTabId;
  FTabImageIndex:= -1;
  FInSession:= false;
  FEnabledCodeTree[0]:= true;
  FEnabledCodeTree[1]:= true;
  FSaveHistory:= true;
  FEditorsLinked:= true;
  FCodetreeFilterHistory:= TStringList.Create;
  FCodetreeSortType:= stNone;
  FCachedTreeview[0]:= nil;
  FCachedTreeview[1]:= nil;

  MacroStrings:= TStringList.Create;
  MacroStrings.TextLineBreakStyle:= tlbsLF;

  FBracketHilite:= EditorOps.OpBracketHilite;
  FBracketSymbols:= EditorOps.OpBracketSymbols;
  FBracketMaxDistance:= EditorOps.OpBracketDistance;

  InitEditor(Ed1, 'ed1');
  InitEditor(Ed2, 'ed2');

  Ed1.Strings.GutterDecor1:= Ed1.GutterDecor;
  Ed1.Strings.GutterDecor2:= Ed2.GutterDecor;

  Ed2.Visible:= false;
  Splitter.Visible:= false;

  Ed1.EditorIndex:= 0;
  Ed2.EditorIndex:= 1;

  FSplitHorz:= UiOps.DefaultTabSplitIsHorz;
  Splitted:= false;
  Ed1.Align:= alClient;
  Ed2.Align:= cSplitHorzToAlign[FSplitHorz];
  Splitter.Align:= cSplitHorzToAlign[FSplitHorz];
  Splitter.OnMoved:= @SplitterMoved;

  Adapter1:= TATAdapterEControl.Create(Self);
  Adapter1.EnabledSublexerTreeNodes:= UiOps.TreeSublexers;
  Adapter1.AddEditor(Ed1);
  Adapter1.AddEditor(Ed2);

  //load options
  EditorApplyOps(Ed1, EditorOps, true, true, AApplyCentering, false);
  EditorApplyOps(Ed2, EditorOps, true, true, AApplyCentering, false);
  EditorApplyTheme(Ed1);
  EditorApplyTheme(Ed2);

  //newdoc props
  case UiOps.NewdocEnds of
    0: Ed1.Strings.Endings:= {$ifdef windows} cEndWin {$else} cEndUnix {$endif};
    1: Ed1.Strings.Endings:= cEndUnix;
    2: Ed1.Strings.Endings:= cEndWin;
    3: Ed1.Strings.Endings:= cEndMac;
  end;
  Ed2.Strings.Endings:= Ed1.Strings.Endings;

  Ed1.Strings.ClearUndo;
  Ed1.Strings.EncodingDetectDefaultUtf8:= UiOps.DefaultEncUtf8;

  Ed1.EncodingName:= AppEncodingShortnameToFullname(UiOps.NewdocEnc);

  //must clear Modified, it was set on initing
  Ed1.Modified:= false;
  Ed2.Modified:= false;
end;

destructor TEditorFrame.Destroy;
var
  NTick1, NTick2: QWord;
begin
  NTick1:= GetTickCount64;

  if Assigned(FBin) then
  begin
    FBin.OpenStream(nil, False); //ARedraw=False to not paint on Win desktop with DC=0
    FreeAndNil(FBin);
  end;

  if Assigned(FMicromapBmp) then
    FreeAndNil(FMicromapBmp);

  if Assigned(FBinStream) then
    FreeAndNil(FBinStream);

  if Assigned(FImageBox) then
    FreeAndNil(FImageBox);

  Ed1.AdapterForHilite:= nil;
  Ed2.AdapterForHilite:= nil;

  Ed1.Strings.GutterDecor1:= nil;
  Ed1.Strings.GutterDecor2:= nil;
  if not FEditorsLinked then
  begin
    Ed2.Strings.GutterDecor1:= nil;
    Ed2.Strings.GutterDecor2:= nil;
  end;

  FreeAndNil(MacroStrings);
  FreeAndNil(FCodetreeFilterHistory);

  NTick2:= GetTickCount64;
  Inc(AppTimeOfFreeing, NTick2-NTick1);

  if not Application.Terminated then //prevent crash on exit
    DoPyEvent(Ed1, cEventOnClose, []);

  Inc(AppTimeOfOnClose, GetTickCount64-NTick2);

  inherited;
end;

function TEditorFrame.Editor: TATSynEdit;
begin
  if FActiveSecondaryEd then
    Result:= Ed2
  else
    Result:= Ed1;
end;

function TEditorFrame.EditorBro: TATSynEdit;
begin
  if not FActiveSecondaryEd then
    Result:= Ed2
  else
    Result:= Ed1;
end;

function TEditorFrame.GetAdapter(Ed: TATSynEdit): TATAdapterEControl;
begin
  if (Ed=Ed1) or EditorsLinked then
    Result:= Adapter1
  else
    Result:= Adapter2;
end;

function TEditorFrame.EditorObjToTreeviewIndex(Ed: TATSynEdit): integer;
begin
  if (Ed=Ed1) or EditorsLinked then
    Result:= 0
  else
    Result:= 1;
end;

function TEditorFrame.GetCachedTreeviewInited(Ed: TATSynEdit): boolean;
var
  N: integer;
begin
  N:= EditorObjToTreeviewIndex(Ed);
  Result:= Assigned(FCachedTreeview[N]);
end;

function TEditorFrame.GetCachedTreeview(Ed: TATSynEdit): TTreeView;
var
  N: integer;
begin
  N:= EditorObjToTreeviewIndex(Ed);
  if FCachedTreeview[N]=nil then
  begin
    FCachedTreeview[N]:= TTreeView.Create(Self);
    FCachedTreeview[N].OnDeletion:=@TreeOnDeletion;
  end;
  Result:= FCachedTreeview[N];
end;

function TEditorFrame.IsEmpty: boolean;
begin
  //dont check Modified here
  if EditorsLinked then
    Result:= (FFileName='') and EditorIsEmpty(Ed1)
  else
    Result:= (FFileName='') and (FFileName2='') and
      EditorIsEmpty(Ed1) and EditorIsEmpty(Ed2);
end;

procedure TEditorFrame.ApplyThemeToInfoPanel(APanel: TPanel);
begin
  APanel.Font.Name:= UiOps.VarFontName;
  APanel.Font.Size:= ATEditorScaleFont(UiOps.VarFontSize);
  APanel.Color:= GetAppColor(apclEdMarkedRangeBg); //GetAppColor(apclListBg);
  APanel.Font.Color:= GetAppColor(apclListFont);
end;

procedure TEditorFrame.ApplyLexerStyleMap;
var
  An, AnIncorrect: TecSyntAnalyzer;
begin
  An:= Lexer[Ed1];
  if Assigned(An) then
    DoApplyLexerStylesMap(An, AnIncorrect);

  if not EditorsLinked then
  begin
    An:= Lexer[Ed2];
    if Assigned(An) then
      DoApplyLexerStylesMap(An, AnIncorrect);
  end;
end;

procedure TEditorFrame.LexerReparse;
var
  Ada: TATAdapterEControl;
begin
  Ada:= Adapter[Ed1];
  if Assigned(Ada) and Assigned(Ada.AnClient) then
    Ada.ParseFromLine(0, true);

  if not EditorsLinked then
  begin
    Ada:= Adapter[Ed2];
    if Assigned(Ada) and Assigned(Ada.AnClient) then
      Ada.ParseFromLine(0, true);
  end;
end;

procedure TEditorFrame.ApplyTheme;
begin
  EditorApplyTheme(Ed1);
  EditorApplyTheme(Ed2);

  if Assigned(FBin) then
  begin
    ViewerApplyTheme(FBin);
    FBin.Redraw();
  end;

  Splitter.Color:= GetAppColor(apclSplitMain);

  if Assigned(PanelInfo) then
    ApplyThemeToInfoPanel(PanelInfo);

  if Assigned(PanelNoHilite) then
    ApplyThemeToInfoPanel(PanelNoHilite);
end;

function TEditorFrame.IsEditorFocused: boolean;
begin
  Result:= Ed1.Focused or Ed2.Focused;
end;

function TEditorFrame.FrameKind: TATEditorFrameKind;
begin
  if Assigned(FBin) and FBin.Visible then
    Result:= efkBinaryViewer
  else
  if Assigned(FImageBox) and FImageBox.Visible then
    Result:= efkImageViewer
  else
    Result:= efkEditor;
end;


procedure TEditorFrame.SetLexer(Ed: TATSynEdit; an: TecSyntAnalyzer);
var
  an2: TecSyntAnalyzer;
  Ada: TATAdapterEControl;
begin
  //important to check Visible. avoids parser start in _passive_ tabs when opening 100 files
  //by drag-n-drop. closing of these files is much faster.
  if not (Visible and AppAllowFrameParsing) then
  begin
    LexerInitial[Ed]:= an;
    DoPyEvent(Ed, cEventOnLexer, []);
    exit;
  end;

  {
  //it breaks code-tree, issue #3348
  ada:= Adapter[Ed];
  if Assigned(ada) and Assigned(ada.Lexer) then
    if not ada.Stop then exit;
  }

  if (an=nil) or IsFileTooBigForLexer(GetFileName(Ed)) then
  begin
    if EditorsLinked then
    begin
      Ed1.AdapterForHilite:= nil;
      Ed2.AdapterForHilite:= nil;
      if Assigned(Adapter1) then
        Adapter1.Lexer:= nil;
      if Assigned(Adapter2) then
        Adapter2.Lexer:= nil;
    end
    else
    begin
      Ed.AdapterForHilite:= nil;
      if Ed=Ed1 then
      begin
        if Assigned(Adapter1) then
          Adapter1.Lexer:= nil;
      end
      else
      begin
        if Assigned(Adapter2) then
          Adapter2.Lexer:= nil;
      end;
    end;
    exit;
  end;

  if Assigned(an) then
  begin
    if EditorsLinked then
    begin
      Ed1.AdapterForHilite:= Adapter1;
      Ed2.AdapterForHilite:= Adapter1;
    end
    else
    begin
      if Ed=Ed1 then
        Ed1.AdapterForHilite:= Adapter1
      else
      begin
        if Adapter2=nil then
        begin
          Adapter2:= TATAdapterEControl.Create(Self);
          Adapter2.EnabledSublexerTreeNodes:= UiOps.TreeSublexers;
          OnInitAdapter(Adapter2);
        end;
        Ed2.AdapterForHilite:= Adapter2;
      end;
    end;

    if not DoApplyLexerStylesMap(an, an2) then
      DoDialogLexerStylesMap(an2);
  end
  else
  begin
    Ed.Fold.Clear;
    Ed.Update;
    if (Ed=Ed1) and EditorsLinked then
    begin
      Ed2.Fold.Clear;
      Ed2.Update;
    end;
  end;

  if Ed.AdapterForHilite is TATAdapterEControl then
  begin
    Ada:= TATAdapterEControl(Ed.AdapterForHilite);
    Ada.Lexer:= an;

    if Assigned(an) then
      EditorStartParse(Ed);
  end;
end;

procedure TEditorFrame.SetLexerLite(Ed: TATSynEdit; an: TATLiteLexer);
begin
  Lexer[Ed]:= nil;

  Ed.AdapterForHilite:= an;
  Ed.Update;

  if (Ed=Ed1) and EditorsLinked then
  begin
    Ed2.AdapterForHilite:= an;
    Ed2.Update;
  end;

  //to apply lexer-specific config
  OnLexerChange(Ed);
end;

procedure TEditorFrame.DoFileOpen_AsBinary(const AFileName: string; AMode: TATBinHexMode);
begin
  FFileName:= AFileName;
  UpdateCaptionFromFilename;

  Ed1.Hide;
  Ed2.Hide;
  Splitter.Hide;
  ReadOnly[Ed1]:= true;

  if Assigned(FImageBox) then
    FImageBox.Hide;

  if Assigned(FBin) then
    FBin.OpenStream(nil)
  else
  begin
    FBin:= TATBinHex.Create(FFormDummy);
    FBin.Hide; //reduce flicker with initial size
    FBin.OnKeyDown:= @BinaryOnKeyDown;
    FBin.OnScroll:= @BinaryOnScroll;
    FBin.OnOptionsChange:= @BinaryOnScroll;
    FBin.OnSearchProgress:= @BinaryOnProgress;
    FBin.Parent:= FFormDummy;
    FBin.Align:= alClient;
    FBin.BorderStyle:= bsNone;
    FBin.ResizeFollowTail:= false; //fixes scrolling to the end on file loading
    FBin.TextGutter:= true;
    FBin.TextPopupCommands:= [vpCmdCopy, vpCmdCopyHex, vpCmdSelectAll];
    FBin.TextPopupCaption[vpCmdCopy]:= ATEditorOptions.TextMenuitemCopy;
    FBin.TextPopupCaption[vpCmdCopyHex]:= ATEditorOptions.TextMenuitemCopy+' (hex)';
    FBin.TextPopupCaption[vpCmdSelectAll]:= ATEditorOptions.TextMenuitemSelectAll;
    FBin.Show;
  end;

  FBin.TextWidth:= UiOps.ViewerBinaryWidth;
  FBin.TextNonPrintable:= UiOps.ViewerNonPrintable;
  FBin.Mode:= AMode;

  if Assigned(FBinStream) then
    FreeAndNil(FBinStream);
  FBinStream:= TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);

  ViewerApplyTheme(FBin);
  FBin.Show;
  FBin.OpenStream(FBinStream);
  FBin.PosBegin;

  if Visible and FBin.Visible and FBin.CanFocus then
    FBin.SetFocus;

  //DoOnChangeCaption; //needed?
end;


procedure TEditorFrame.DoFileOpen_AsPicture(const AFileName: string);
begin
  FFileName:= AFileName;
  UpdateCaptionFromFilename;

  Ed1.Hide;
  Ed2.Hide;
  Splitter.Hide;
  ReadOnly[Ed1]:= true;

  if not Assigned(FImageBox) then
  begin
    FImageBox:= TATImageBox.Create(FFormDummy);
    FImageBox.Parent:= FFormDummy;
    FImageBox.Align:= alClient;
    FImageBox.BorderStyle:= bsNone;
    FImageBox.OptFitToWindow:= true;
    FImageBox.OnScroll:= @DoImageboxScroll;
    FImageBox.OnKeyDown:= @BinaryOnKeyDown;
  end;

  try
    FImageBox.Show;
    FImageBox.LoadFromFile(AFileName);
  except
  end;

  //DoOnChangeCaption; //needed?
end;

procedure TEditorFrame.DoImageboxScroll(Sender: TObject);
begin
  DoOnUpdateStatusbar;
end;


procedure TEditorFrame.DoDeactivatePictureMode;
begin
  if Assigned(FImageBox) then
  begin
    FreeAndNil(FImageBox);
    Ed1.Show;
    ReadOnly[Ed1]:= false;
  end;
end;

procedure TEditorFrame.DoDeactivateViewerMode;
begin
  if Assigned(FBin) then
  begin
    FBin.OpenStream(nil, false);
    FreeAndNil(FBin);

    if Assigned(FBinStream) then
      FreeAndNil(FBinStream);

    Ed1.Show;
    ReadOnly[Ed1]:= false;
  end;
end;

procedure TEditorFrame.DoFileOpen(const AFileName, AFileName2: string;
  AAllowLoadHistory, AAllowLoadBookmarks, AAllowLexerDetect, AAllowErrorMsgBox, AAllowLoadUndo: boolean;
  AOpenMode: TAppOpenMode);
var
  bFilename2Valid: boolean;
begin
  NotifEnabled:= false; //for binary-viewer and pictures, NotifEnabled must be False
  FileProps[0].Inited:= false; //loading of new filename must not trigger notif-thread
  FileProps[1].Inited:= false;

  if Assigned(FBin) then
    FBin.Hide;
  if Assigned(FImageBox) then
    FImageBox.Hide;

  if not FileExists(AFileName) then exit;
  if (AFileName2<>'') then
    if not FileExists(AFileName2) then exit;

  if UiOps.InfoAboutOptionsEditor then
    if (CompareFilenames(AFileName, AppFile_OptionsUser)=0) or
      (CompareFilenames(AFileName, AppFile_OptionsDefault)=0) then
      InitPanelInfo(PanelInfo, msgSuggestOptionsEditor, @PanelInfoClick, true);

  Lexer[Ed1]:= nil;
  if not EditorsLinked then
    Lexer[Ed2]:= nil;

  case AOpenMode of
    cOpenModeViewText:
      begin
        DoFileOpen_AsBinary(AFileName, vbmodeText);
        exit;
      end;
    cOpenModeViewBinary:
      begin
        DoFileOpen_AsBinary(AFileName, vbmodeBinary);
        exit;
      end;
    cOpenModeViewHex:
      begin
        DoFileOpen_AsBinary(AFileName, vbmodeHex);
        exit;
      end;
    cOpenModeViewUnicode:
      begin
        DoFileOpen_AsBinary(AFileName, vbmodeUnicode);
        exit;
      end;
    cOpenModeViewUHex:
      begin
        DoFileOpen_AsBinary(AFileName, vbmodeUHex);
        exit;
      end;
  end;

  if IsFilenameListedInExtensionList(AFileName, UiOps.PictureTypes) then
  begin
    DoFileOpen_AsPicture(AFileName);
    exit;
  end;

  DoDeactivatePictureMode;
  DoDeactivateViewerMode;

  bFilename2Valid:= (AFileName2<>'') and not SameFileName(AFileName, AFileName2);
  if bFilename2Valid then
    EditorsLinked:= false; //set it before opening 1st file

  DoFileOpen_Ex(Ed1, AFileName,
    AAllowLoadHistory,
    AAllowLoadHistory,
    AAllowLoadBookmarks,
    AAllowLexerDetect,
    AAllowErrorMsgBox,
    false,
    AAllowLoadUndo,
    AOpenMode);

  if bFilename2Valid then
  begin
    SplitHorz:= false;
    Splitted:= true;
    DoFileOpen_Ex(Ed2, AFileName2,
      AAllowLoadHistory,
      AAllowLoadHistory,
      AAllowLoadBookmarks,
      AAllowLexerDetect,
      AAllowErrorMsgBox,
      false,
      AAllowLoadUndo,
      AOpenMode);
  end;

  DoOnUpdateStatusbar;
end;

procedure TEditorFrame.HandleStringsProgress(Sender: TObject);
const
  //avoid too many form updates
  cStepPercents = 8;
var
  St: TATStrings;
begin
  St:= TATStrings(Sender);
  if St.ProgressValue-FProgressOldProgress<cStepPercents then exit;
  if FProgressGauge=nil then exit;
  FProgressGauge.Progress:= St.ProgressValue;
  FProgressOldProgress:= St.ProgressValue;
  Application.ProcessMessages;
end;

procedure TEditorFrame.DoFileOpen_Ex(Ed: TATSynEdit; const AFileName: string;
  AAllowLoadHistory, AAllowLoadHistoryEnc, AAllowLoadBookmarks, AAllowLexerDetect,
  AAllowErrorMsgBox, AKeepScroll, AAllowLoadUndo: boolean; AOpenMode: TAppOpenMode);
var
  NFileSize: Int64;
begin
  FProgressForm:= nil;
  if not AppFormShowCompleted then
  begin
    NFileSize:= FileSize(AFileName);
    if NFileSize>UiOps.MaxFileSizeWithoutProgressForm then
    begin
      AppInitProgressForm(FProgressForm, FProgressGauge, Format('%s (%d Mb)', [
          ExtractFileName(AFileName),
          //AppCollapseHomeDirInFilename(ExtractFileDir(AFileName)),
          NFileSize div (1024*1024)
          ]));
      FProgressOldProgress:= 0;
      FProgressOldHandler:= Ed.Strings.OnProgress;
      Ed.Strings.OnProgress:= @HandleStringsProgress;
      FProgressForm.Show;
      Application.ProcessMessages;
    end;
  end;

  try
    try
      if AKeepScroll then
        Ed.Strings.EncodingDetect:= false;
      Ed.LoadFromFile(AFileName, AKeepScroll);
      Ed.Strings.EncodingDetect:= true;
      SetFileName(Ed, AFileName);
      UpdateCaptionFromFilename;
    finally
      if Assigned(FProgressForm) then
      begin
        Ed.Strings.OnProgress:= FProgressOldHandler;
        FProgressForm.Free;
        FProgressForm:= nil;
        FProgressGauge:= nil;
      end;
    end;
  except
    if AAllowErrorMsgBox then
      MsgBox(msgCannotOpenFile+#10+AFileName, MB_OK or MB_ICONERROR);

    SetFileName(Ed, '');
    UpdateCaptionFromFilename;

    EditorClear(Ed);
    exit
  end;

  //turn off opts for huge files
  FileWasBig[Ed]:= Ed.Strings.Count>EditorOps.OpWrapEnabledMaxLines;

  if AAllowLoadUndo then
    DoLoadUndo(Ed);

  DoLoadHistory(Ed, AAllowLoadHistoryEnc, AAllowLoadHistory, AAllowLoadBookmarks);

  //save temp-options, to later know which options are changed,
  //during loading of lexer-specific config
  EditorSaveTempOptions(Ed, Ed.InitialOptions);

  if AAllowLexerDetect then
    DoLexerFromFilename(Ed, AFileName);

  UpdateReadOnlyFromFile(Ed);
  NotifEnabled:= true;
end;

procedure TEditorFrame.UpdateReadOnlyFromFile(Ed: TATSynEdit);
var
  b: boolean;
begin
  if Ed.IsReadOnlyChanged then exit;
  b:= AppIsFileReadonly(GetFileName(Ed));
  ReadOnly[Ed]:= b;
  if b then
    Ed.IsReadOnlyAutodetected:= true;
end;

procedure TEditorFrame.UpdateFrame(AUpdatedText: boolean);
var
  Ad: TATAdapterEControl;
begin
  Ed1.DoCaretsFixIncorrectPos(false);
  Ed2.DoCaretsFixIncorrectPos(false);

  Ed1.Update(AUpdatedText);
  Ed2.Update(AUpdatedText);

  if AUpdatedText then
  begin
    Ad:= Adapter[Ed1];
    Ad.OnEditorChange(Ed1);

    if not EditorsLinked then
    begin
      Ad:= Adapter[Ed2];
      if Assigned(Ad) then
        Ad.OnEditorChange(Ed2);
    end;
  end;
end;

procedure TEditorFrame.FixLexerIfDeleted(Ed: TATSynEdit; const ALexerName: string);
begin
  if LexerName[Ed]=ALexerName then
    Lexer[Ed]:= nil;
  //fix crash:
  //F.InitialLexer is C#, user deletes C# in lexer lib, and switches tab
  if Assigned(LexerInitial[Ed]) and (LexerInitial[Ed].LexerName=ALexerName) then
    LexerInitial[Ed]:= nil;
end;

function TEditorFrame.GetFileName(Ed: TATSynEdit): string;
begin
  if EditorsLinked or (Ed=Ed1) then
    Result:= FFileName
  else
    Result:= FFileName2;
end;

procedure TEditorFrame.SetFileName(Ed: TATSynEdit; const AFileName: string);
begin
  if EditorsLinked then
  begin
    Ed1.FileName:= AFileName;
    Ed2.FileName:= AFileName;
  end
  else
    Ed.FileName:= AFileName;

  if EditorsLinked or (Ed=Ed1) then
    FFileName:= AFileName
  else
    FFileName2:= AFileName;
end;

function TEditorFrame.DoFileSave(ASaveAs, AAllEditors: boolean): boolean;
begin
  Result:= true;
  if not EditorsLinked and AAllEditors then
  begin
    Result:= DoFileSave_Ex(Ed1, ASaveAs);
    Result:= DoFileSave_Ex(Ed2, ASaveAs);
  end
  else
  begin
    Result:= DoFileSave_Ex(Editor, ASaveAs);
  end;

  if ASaveAs then
    UpdateTabTooltip;
end;

function TEditorFrame.DoFileSave_Ex(Ed: TATSynEdit; ASaveAs: boolean): boolean;
var
  An: TecSyntAnalyzer;
  EdIndex: integer;
  bNameChanged, bNotifWasEnabled: boolean;
  NameCounter: integer;
  SFileName, NameTemp, NameInitial: string;
  EventRes: TAppPyEventResult;
begin
  Result:= true;
  if FrameKind<>efkEditor then exit(true); //disable saving, but close

  EventRes:= DoPyEvent(Ed, cEventOnSaveBefore, []);
  if EventRes.Val=evrFalse then exit(true); //disable saving, but close

  EdIndex:= EditorObjToIndex(Ed);
  if EdIndex<0 then exit(false);

  DoHideNotificationPanel(NotifReloadControls[EdIndex]);
  DoHideNotificationPanel(NotifDeletedControls[EdIndex]);

  SFileName:= Ed.FileName;
  bNameChanged:= ASaveAs or (SFileName='');

  if bNameChanged then
  begin
    An:= Lexer[Ed];
    if Assigned(An) then
    begin
      SaveDialog.DefaultExt:= Lexer_GetDefaultExtension(An);
      SaveDialog.Filter:= Lexer_GetFileFilterString(An, msgAllFiles);
    end
    else
    begin
      SaveDialog.DefaultExt:= '';
      SaveDialog.Filter:= '';
    end;

    if SFileName='' then
    begin
      NameInitial:= '';
      EventRes:= DoPyEvent(Ed, cEventOnSaveNaming, []);
      if EventRes.Val=evrString then
        NameInitial:= EventRes.Str;
      if NameInitial='' then
        NameInitial:= 'new';

      //get first free filename: new.txt, new1.txt, new2.txt, ...
      NameCounter:= 0;
      repeat
        NameTemp:= SaveDialog.InitialDir+DirectorySeparator+
                   NameInitial+IfThen(NameCounter>0, IntToStr(NameCounter))+
                   SaveDialog.DefaultExt; //DefaultExt with dot
        if not FileExists(NameTemp) then
        begin
          SaveDialog.FileName:= ExtractFileName(NameTemp);
          Break
        end;
        Inc(NameCounter);
      until false;
    end
    else
    begin
      SaveDialog.FileName:= ExtractFileName(SFileName);
      SaveDialog.InitialDir:= ExtractFileDir(SFileName);
    end;

    if not SaveDialog.Execute then
      exit(false);

    if OnCheckFilenameOpened(SaveDialog.FileName) then
    begin
      MsgBox(
        msgStatusFilenameAlreadyOpened+#10+
        ExtractFileName(SaveDialog.FileName)+#10#10+
        msgStatusNeedToCloseTabSavedOrDup, MB_OK or MB_ICONWARNING);
      exit;
    end;

    //add to recents previous filename
    if Assigned(FOnAddRecent) then
      FOnAddRecent(Ed);

    SFileName:= SaveDialog.FileName;

    //remove read-only (it may be set for original file)
    ReadOnly[Ed]:= false;
  end;

  bNotifWasEnabled:= NotifEnabled;
  NotifEnabled:= false;

  //EditorSaveFileAs is big:
  //handles save errors,
  //handles exception from encoding conversion (saves in UTF8 if exception)
  Result:= EditorSaveFileAs(Ed, SFileName);

  if bNameChanged then
    DoLexerFromFilename(Ed, SFileName);

  if Result then
  begin
    SetFileName(Ed, SFileName);
    TabFontColor:= clNone;
    TabExtModified[EdIndex]:= false;
    TabExtDeleted[EdIndex]:= false;

    //add to recents new filename
    if bNameChanged then
      if Assigned(FOnAddRecent) then
        FOnAddRecent(Ed);

    UpdateCaptionFromFilename;

    DoSaveUndo(Ed, SFileName);
    DoPyEvent(Ed, cEventOnSaveAfter, []);
    if Assigned(FOnSaveFile) then
      FOnSaveFile(Ed, SFileName);
  end;

  AppGetFileProps(SFileName, FileProps[EdIndex]);

  NotifEnabled:= bNotifWasEnabled or bNameChanged;
end;

procedure TEditorFrame.DoFileReload_DisableDetectEncoding(Ed: TATSynEdit);
var
  SFileName: string;
begin
  SFileName:= GetFileName(Ed);
  if SFileName='' then exit;
  if Ed.Modified then
    if MsgBox(
      Format(msgConfirmReopenModifiedTab, [AppCollapseHomeDirInFilename(SFileName)]),
      MB_OKCANCEL or MB_ICONWARNING
      ) <> ID_OK then exit;

  Ed.Strings.EncodingDetect:= false;
  Ed.Strings.LoadFromFile(SFileName);
  Ed.Strings.EncodingDetect:= true;
  UpdateEds(true);
end;

function TEditorFrame.DoFileReload(Ed: TATSynEdit): boolean;
var
  EdIndex: integer;
  PrevCaretX, PrevCaretY: integer;
  PrevTail: boolean;
  Mode: TAppOpenMode;
  SFileName: string;
begin
  Result:= true;
  SFileName:= GetFileName(Ed);
  if SFileName='' then exit(false);
  EdIndex:= EditorObjToIndex(Ed);
  if EdIndex<0 then exit;

  if not FileExists(SFileName) then
  begin
    OnMsgStatus(Self, msgCannotFindFile+' '+ExtractFileName(SFileName));
    exit(false);
  end;

  DoHideNotificationPanel(NotifReloadControls[EdIndex]);
  DoHideNotificationPanel(NotifDeletedControls[EdIndex]);

  TabExtModified[EdIndex]:= false;
  TabExtDeleted[EdIndex]:= false;

  FileProps[EdIndex].Inited:= false;

  //remember props
  PrevCaretX:= 0;
  PrevCaretY:= 0;

  if Ed.Carets.Count>0 then
    with Ed.Carets[0] do
      begin
        PrevCaretX:= PosX;
        PrevCaretY:= PosY;
      end;

  PrevTail:= UiOps.ReloadFollowTail and
    (Ed.Strings.Count>0) and
    (PrevCaretY=Ed.Strings.Count-1);

  Mode:= cOpenModeEditor;
  if FrameKind=efkBinaryViewer then
    case FBin.Mode of
      vbmodeText:
        Mode:= cOpenModeViewText;
      vbmodeBinary:
        Mode:= cOpenModeViewBinary;
      vbmodeHex:
        Mode:= cOpenModeViewHex;
      vbmodeUnicode:
        Mode:= cOpenModeViewUnicode;
      vbmodeUHex:
        Mode:= cOpenModeViewUHex;
      else
        Mode:= cOpenModeViewHex;
    end;

  //reopen
  DoSaveHistory(Ed);
  DoFileOpen_Ex(Ed, SFileName,
    true{AllowLoadHistory},
    false{AllowLoadHistoryEnc},
    false{AllowLoadBookmarks},
    false{AllowLexerDetect},
    false{AllowMsgBox},
    true{KeepScroll},
    true{AllowLoadUndo},
    Mode);

  //fix issue #3394
  if EditorsLinked and Splitted and (Ed=Ed1) then
  begin
    Ed2.Update(true);
    Ed2.DoCaretsFixIncorrectPos(false);
  end;

  if Ed.Strings.Count=0 then exit;

  //restore props
  PrevCaretY:= Min(PrevCaretY, Ed.Strings.Count-1);
  if PrevTail then
  begin
    PrevCaretX:= 0;
    PrevCaretY:= Ed.Strings.Count-1;
  end;

  Application.ProcessMessages; //for DoGotoPos

  Ed.DoGotoPos(
    Point(PrevCaretX, PrevCaretY),
    Point(-1, -1),
    1,
    1, //indentVert must be >0
    true,
    false
    );

  OnUpdateStatusbar(Self);

  //fire 'on_change_slow' and disable its timer
  TimerChangeTimer(nil);
end;

procedure TEditorFrame.SetLineEnds(Ed: TATSynEdit; AValue: TATLineEnds);
begin
  if GetLineEnds(Ed)=AValue then Exit;

  Ed.Strings.Endings:= AValue;
  Ed.Update;
  if (Ed=Ed1) and EditorsLinked then
    Ed2.Update;
end;

procedure TEditorFrame.SetUnprintedShow(AValue: boolean);
begin
  Ed1.OptUnprintedVisible:= AValue;
  UpdateEds;
end;

procedure TEditorFrame.SetUnprintedSpaces(AValue: boolean);
begin
  Ed1.OptUnprintedSpaces:= AValue;
  UpdateEds;
end;

procedure TEditorFrame.SetEditorsLinked(AValue: boolean);
begin
  if FEditorsLinked=AValue then exit;
  FEditorsLinked:= AValue;

  if FEditorsLinked then
    Ed2.Strings:= Ed1.Strings
  else
    Ed2.Strings:= nil;

  Ed1.Strings.GutterDecor1:= Ed1.GutterDecor;
  if FEditorsLinked then
  begin
    Ed1.Strings.GutterDecor2:= Ed2.GutterDecor;
  end
  else
  begin
    Ed1.Strings.GutterDecor2:= nil;
    Ed2.Strings.GutterDecor1:= Ed2.GutterDecor;
    Ed2.Strings.GutterDecor2:= nil;
  end;

  Adapter1.AddEditor(nil);
  if Assigned(Adapter2) then
    Adapter2.AddEditor(nil);

  if FEditorsLinked then
  begin
    Adapter1.AddEditor(Ed1);
    Adapter1.AddEditor(Ed2);
  end
  else
  begin
    if Adapter2=nil then
    begin
      Adapter2:= TATAdapterEControl.Create(Self);
      Adapter2.EnabledSublexerTreeNodes:= UiOps.TreeSublexers;
      OnInitAdapter(Adapter2);
    end;
    Adapter1.AddEditor(Ed1);
    Adapter2.AddEditor(Ed2);
  end;

  Ed2.Fold.Clear;
  Ed2.Update(true);
end;

procedure TEditorFrame.SplitterMoved(Sender: TObject);
begin
  FSplitPos:= GetSplitPosCurrent;
end;

procedure TEditorFrame.TreeOnDeletion(Sender: TObject; Node: TTreeNode);
begin
  if Assigned(Node.Data) then
  begin
    TObject(Node.Data).Free;
    Node.Data:= nil;
  end;
end;

procedure TEditorFrame.SetUnprintedEnds(AValue: boolean);
begin
  Ed1.OptUnprintedEnds:= AValue;
  UpdateEds;
end;

procedure TEditorFrame.SetUnprintedEndsDetails(AValue: boolean);
begin
  Ed1.OptUnprintedEndsDetails:= AValue;
  UpdateEds;
end;

procedure TEditorFrame.EditorOnClickGutter(Sender: TObject; ABand, ALine: integer);
var
  Ed: TATSynEdit;
begin
  Ed:= Sender as TATSynEdit;

  if DoPyEvent(Ed, cEventOnClickGutter,
    [
    AppVariant(ConvertShiftStateToString(KeyboardStateToShiftState)),
    AppVariant(ALine),
    AppVariant(ABand)
    ]).Val = evrFalse then exit;

  if ABand=Ed.Gutter.FindIndexByTag(ATEditorOptions.GutterTagBookmarks) then
    ed.BookmarkToggleForLine(ALine, 1, '', bmadOption, true, 0);
end;

procedure TEditorFrame.EditorOnDrawBookmarkIcon(Sender: TObject; C: TCanvas; ALineNum: integer;
  const ARect: TRect);
var
  Ed: TATSynEdit;
  R: TRect;
  dx: integer;
  index, kind: integer;
begin
  R:= ARect;
  if R.Left>=R.Right then exit;

  Ed:= Sender as TATSynEdit;
  index:= Ed.Strings.Bookmarks.Find(ALineNum);
  if index<0 then exit;

  kind:= Ed.Strings.Bookmarks[index]^.Data.Kind;
  if kind<=1 then
  begin
    c.Brush.Color:= GetAppColor(apclEdBookmarkIcon);
    c.Pen.Color:= c.Brush.Color;
    inc(R.Top, 1);
    inc(R.Left, 4);
    dx:= R.Height div 2-1;
    c.Polygon([
      Point(R.Left, R.Top),
      Point(R.Left+dx, R.Top+dx),
      Point(R.Left, R.Top+2*dx)
      ]);
  end
  else
  if (kind>=Low(AppBookmarkSetup)) and (kind<=High(AppBookmarkSetup)) then
  begin
    AppBookmarkImagelist.Draw(c,
      R.Left,
      (R.Top+R.Bottom-AppBookmarkImagelist.Height) div 2,
      AppBookmarkSetup[kind].ImageIndex);
  end;
end;


function TEditorFrame.GetCommentString(Ed: TATSynEdit): string;
var
  an: TecSyntAnalyzer;
begin
  Result:= '';
  an:= Adapter[Ed].Lexer;
  if Assigned(an) then
    Result:= an.LineComment;
end;

function TEditorFrame.GetTextChangeSlow(EdIndex: integer): boolean;
begin
  Result:= FTextChangeSlow[EdIndex];
end;

procedure TEditorFrame.SetTextChangeSlow(EdIndex: integer; AValue: boolean);
begin
  FTextChangeSlow[EdIndex]:= AValue;
end;

function TEditorFrame.GetEnabledCodeTree(Ed: TATSynEdit): boolean;
begin
  if (Ed=Ed1) or EditorsLinked then
    Result:= FEnabledCodeTree[0]
  else
    Result:= FEnabledCodeTree[1];
end;

function TEditorFrame.GetEnabledFolding: boolean;
begin
  Result:= Editor.OptFoldEnabled;
end;

function TEditorFrame.GetFileWasBig(Ed: TATSynEdit): boolean;
var
  EdIndex: integer;
begin
  EdIndex:= EditorObjToIndex(Ed);
  if EdIndex>=0 then
    Result:= FFileWasBig[EdIndex]
  else
    Result:= false;
end;

function TEditorFrame.GetInitialLexer(Ed: TATSynEdit): TecSyntAnalyzer;
var
  EdIndex: integer;
begin
  if EditorsLinked then
    Result:= FInitialLexer1
  else
  begin
    EdIndex:= EditorObjToIndex(Ed);
    if EdIndex=0 then
      Result:= FInitialLexer1
    else
      Result:= FInitialLexer2;
  end;
end;

procedure TEditorFrame.DoOnChangeCaption;
begin
  if Assigned(FOnChangeCaption) then
    FOnChangeCaption(Self);
end;

procedure TEditorFrame.DoMacroStartOrStop;
begin
  if FMacroRecord then
  begin
    FMacroRecord:= false;
    DoPyEvent_Macro(MacroStrings.Text);
  end
  else
  begin
    FMacroRecord:= true;
    MacroStrings.Clear;
  end;

  Ed1.ModeMacroRecording:= FMacroRecord;
  Ed1.Update;
  Ed2.ModeMacroRecording:= FMacroRecord;
  Ed2.Update;
end;

procedure TEditorFrame.DoOnUpdateStatusbar;
begin
  if Assigned(FOnUpdateStatusbar) then
    FOnUpdateStatusbar(Self);
end;

procedure TEditorFrame.DoOnUpdateState;
begin
  if Assigned(FOnUpdateState) then
    FOnUpdateState(Self);
end;

procedure TEditorFrame.DoOnUpdateZoom;
begin
  if Assigned(FOnUpdateZoom) then
    FOnUpdateZoom(Self);
end;

procedure TEditorFrame.EditorClickMoveCaret(Sender: TObject; APrevPnt, ANewPnt: TPoint);
var
  Pnt: TPoint;
begin
  if MacroRecord then
  begin
    Pnt:= ConvertTwoPointsToDiffPoint(APrevPnt, ANewPnt);
    MacroStrings.Add(Format('%d,%d,%d', [cmd_MouseClickNearCaret, Pnt.X, Pnt.Y]));
  end;
end;

procedure TEditorFrame.EditorDrawMicromap(Sender: TObject; ACanvas: TCanvas; const ARect: TRect);
begin
  PaintMicromap(Sender as TATSynEdit, ACanvas, ARect);
end;

procedure TEditorFrame.EditorDrawScrollbarVert(Sender: TObject;
  AType: TATScrollbarElemType; ACanvas: TCanvas; const ARect, ARect2: TRect;
  var ACanDraw: boolean);
var
  Ed: TATSynEdit;
begin
  Ed:= (Sender as TATScrollBar).Parent as TATSynEdit;
  if Ed.OptMicromapVisible and Ed.OptMicromapOnScrollbar and (AType=aseBackAndThumbV) then
  begin
    ACanDraw:= false;
    PaintMicromap(Ed, ACanvas, ARect);
  end
  else
    ACanDraw:= true;
end;

procedure TEditorFrame.PaintMicromap(Ed: TATSynEdit; ACanvas: TCanvas; const ARect: TRect);
{
  micromap has columns:
    column_0: width 50% of char cell, it's used for line states
    column_1: width 50% of char cell, it's used for boomkarks + plugins marks
    right edge column: width 50% of char cell, it's used for selections
  so, for different micromap rect widths, some columns may overlap, e.g. right_edge and column_1
}
type
  TAppMicromapMarkPos = (markColumn, markFull, markRight);
const
  cTagOccurrences = 101; //see plugin 'Highlight Occurrences'
  cTagSpellChecker = 105; //see plugin 'Spell Checker'
  cTagColumnFullsized = -2;
var
  NWidthSmall: integer;
  NScaleDiv: integer;
//
  function GetItemRect(AColumn, NLine1, NLine2: integer; AMarkPos: TAppMicromapMarkPos): TRect;
  begin
    Result:= EditorRectMicromapMark(Ed, AColumn, NLine1, NLine2, ARect.Height, EditorOps.OpMicromapMinMarkHeight, NScaleDiv);
    case AMarkPos of
      markRight:
        begin
          Result.Right:= ARect.Width;
          Result.Left:= Result.Right - NWidthSmall;
        end;
      markFull:
        begin
          Result.Left:= 0;
          Result.Right:= ARect.Width;
        end;
    end;
  end;
//
var
  St: TATStrings;
  Caret: TATCaretItem;
  LineState: TATLineState;
  Marker: TATMarkerItem;
  Bookmarks: TATBookmarks;
  BookmarkPtr: PATBookmarkItem;
  XColor, XColorSelected, XColorOccur, XColorSpell: TBGRAPixel;
  NColor: TColor;
  RectMark: TRect;
  NLine1, NLine2, NIndex, i: integer;
begin
  St:= Ed.Strings;
  if St.Count=0 then exit;
  NWidthSmall:= Ed.TextCharSize.XScaled * EditorOps.OpMicromapSmallMarkSizePercents div 100 div ATEditorCharXScale;

  NScaleDiv:= Max(1, St.Count);
  if Ed.OptLastLineOnTop then
    NScaleDiv:= Max(1, NScaleDiv+Ed.GetVisibleLines-1);

  if FMicromapBmp=nil then
    FMicromapBmp:= TBGRABitmap.Create;
  FMicromapBmp.SetSize(ARect.Width, ARect.Height);

  XColor.FromColor(GetAppColor(apclEdMicromapBg));
  FMicromapBmp.Fill(XColor);

  //paint full-width area of current visible area
  RectMark:= GetItemRect(0, Ed.LineTop, Ed.LineBottom, markFull);
  XColor.FromColor(GetAppColor(apclEdMicromapViewBg));
  FMicromapBmp.FillRect(RectMark, XColor);

  XColorSelected.FromColor(Ed.Colors.TextSelBG);
  XColorOccur.FromColor(GetAppColor(apclEdMicromapOccur));
  XColorSpell.FromColor(GetAppColor(apclEdMicromapSpell));

  //paint line states
  if Ed.OptMicromapLineStates and (St.Count>=Ed.OptMicromapShowForMinCount) then
    for i:= 0 to St.Count-1 do
    begin
      //if Ed.IsLineFolded(i) then
      //  Continue;
      LineState:= St.LinesState[i];
      case LineState of
        cLineStateNone: Continue;
        cLineStateAdded: XColor.FromColor(Ed.Colors.StateAdded);
        cLineStateChanged: XColor.FromColor(Ed.Colors.StateChanged);
        cLineStateSaved: XColor.FromColor(Ed.Colors.StateSaved);
        else Continue;
      end;
      RectMark:= GetItemRect(0{column_0}, i, i, markColumn);
      FMicromapBmp.FillRect(RectMark, XColor);
    end;

  //paint selections
  if Ed.OptMicromapSelections then
    for i:= 0 to Ed.Carets.Count-1 do
    begin
      Caret:= Ed.Carets[i];
      Caret.GetSelLines(NLine1, NLine2, false);
      if NLine1<0 then Continue;
      RectMark:= GetItemRect(0, NLine1, NLine2, markRight);
      FMicromapBmp.FillRect(RectMark, XColorSelected);
    end;

  //paint background of columns added from Py API
  for i:= 2{after default columns} to Length(Ed.Micromap.Columns)-1 do
  begin
    NColor:= Ed.Micromap.Columns[i].NColor;
    if NColor<>clNone then
    begin
      XColor.FromColor(NColor);
      RectMark:= EditorRectMicromapMark(Ed, i, -1, -1, ARect.Height, EditorOps.OpMicromapMinMarkHeight, NScaleDiv);
      FMicromapBmp.FillRect(RectMark, XColor);
    end;
  end;

  //paint bookmarks
  if Ed.OptMicromapBookmarks then
  begin
    Bookmarks:= Ed.Strings.Bookmarks;
    XColor.FromColor(Ed.Colors.StateAdded); //not sure what color to take
    for i:= 0 to Bookmarks.Count-1 do
    begin
      BookmarkPtr:= Bookmarks.ItemPtr[i];
      NIndex:= BookmarkPtr^.Data.LineNum;
      //if Ed.IsLineFolded(NIndex) then
      //  Continue;
      RectMark:= EditorRectMicromapMark(Ed, 1{column}, NIndex, NIndex, ARect.Height, EditorOps.OpMicromapMinMarkHeight, NScaleDiv);
      FMicromapBmp.FillRect(RectMark, XColor);
    end;
  end;

  //paint marks for plugins
  for i:= 0 to Ed.Attribs.Count-1 do
  begin
    Marker:= Ed.Attribs[i];

    NLine1:= Marker.PosY;
    NLine2:= NLine1;
    //negative LenX means we need multiline Marker, its height is abs(LenX)
    if Marker.SelX<0 then
      Inc(NLine2, -Marker.SelX-1);

    case Marker.Tag of
      cTagSpellChecker:
        begin
          RectMark:= GetItemRect(1{column_1}, NLine1, NLine2, markColumn);
          FMicromapBmp.FillRect(RectMark, XColorSpell);
        end;
      cTagOccurrences:
        begin
          RectMark:= GetItemRect(1{column_1}, NLine1, NLine2, markColumn);
          FMicromapBmp.FillRect(RectMark, XColorOccur);
        end;
      else
        begin
          if Marker.TagEx>0 then
          begin
            NIndex:= Ed.Micromap.ColumnFromTag(Marker.TagEx);
            if NIndex>=0 then
            begin
              //if ColorBG=none, it may be find-all-matches with custom border color, use border color
              if Marker.LinePart.ColorBG<>clNone then
                XColor.FromColor(Marker.LinePart.ColorBG)
              else
                XColor.FromColor(Marker.LinePart.ColorBorder);
              RectMark:= GetItemRect(NIndex, NLine1, NLine2, markColumn);
              FMicromapBmp.FillRect(RectMark, XColor);
            end;
          end
          else
          if Marker.TagEx=cTagColumnFullsized then
          begin
            RectMark:= GetItemRect(0, NLine1, NLine2, markFull);
            //todo: not tested with BGRABitmap - it must give inverted colors
            XColor.FromColor(Marker.LinePart.ColorBG);
            FMicromapBmp.FillRect(RectMark, XColor, dmDrawWithTransparency, $8000);
          end;
        end;
      end;
  end;

  FMicromapBmp.Draw(ACanvas, ARect.Left, ARect.Top);
end;

procedure TEditorFrame.EditorClickEndSelect(Sender: TObject; APrevPnt, ANewPnt: TPoint);
var
  Pnt: TPoint;
begin
  if MacroRecord then
  begin
    Pnt:= ConvertTwoPointsToDiffPoint(APrevPnt, ANewPnt);
    MacroStrings.Add(Format('%d,%d,%d', [cmd_MouseClickNearCaretAndSelect, Pnt.X, Pnt.Y]));
  end;
end;


procedure TEditorFrame.DoSaveHistory(Ed: TATSynEdit);
var
  cfg: TJSONConfig;
  SFileName: string;
  SKeyForFile: string;
  items: TStringlist;
  i: integer;
begin
  if not FSaveHistory then exit;
  if UiOps.MaxHistoryFiles<2 then exit;

  SFileName:= Ed.FileName;
  if SFileName='' then exit;
  SKeyForFile:= SMaskFilenameSlashes(SFileName);

  cfg:= TJsonConfig.Create(nil);
  try
    try
      cfg.Formatted:= true;
      cfg.Filename:= AppFile_HistoryFiles;
    except
      on E: Exception do
      begin
        MsgBadConfig(AppFile_HistoryFiles, E.Message);
        exit
      end;
    end;

    items:= TStringList.Create;
    try
      cfg.DeletePath(SKeyForFile);
      cfg.EnumSubKeys('/', items);

      //key 'bookmarks' is saved together with usual items, skip it
      i:= items.IndexOf('bookmarks');
      if i>=0 then
        items.Delete(i);

      while items.Count>=UiOps.MaxHistoryFiles do
      begin
        cfg.DeletePath('/'+items[0]);
        items.Delete(0);
      end;
    finally
      FreeAndNil(items);
    end;

    DoSaveHistoryEx(Ed, cfg, SKeyForFile, false);

    //bookmarks are always saved to 'history files.json'
    if UiOps.HistoryItems[ahhBookmarks] then
      DoSaveHistory_Bookmarks(Ed, cfg, SKeyForFile);
  finally
    cfg.Free;
  end;
end;

procedure TEditorFrame.DoSaveHistory_Caret(Ed: TATSynEdit; c: TJsonConfig; const path: UnicodeString);
var
  Caret: TATCaretItem;
  EndX, EndY: integer;
begin
  if Ed.Carets.Count>0 then
  begin
    Caret:= Ed.Carets[0];
    if UiOps.HistoryItems[ahhCaretSel] then
    begin
      EndX:= Caret.EndX;
      EndY:= Caret.EndY;
    end
    else
    begin
      EndX:= -1;
      EndY:= -1;
    end;

    //note: don't use c.SetDeleteValue here because non-empty value is always needed:
    //app loads file from session and skips history if key is empty
    c.SetValue(path+cHistory_Caret,
      Format('%d,%d,%d,%d,', [Caret.PosX, Caret.PosY, EndX, EndY])
      );
   end;
end;

procedure TEditorFrame.DoLoadHistory_Bookmarks(Ed: TATSynEdit; c: TJsonConfig; const path: UnicodeString);
var
  SKey, SValue: UnicodeString;
begin
  if Ed.FileName<>'' then
  begin
    SKey:= AppConfigKeyForBookmarks(Ed);
    SValue:= c.GetValue(SKey, '');
    if SValue<>'' then
      EditorStringToBookmarks(Ed, SValue);
  end;
end;

procedure TEditorFrame.DoSaveHistory_Bookmarks(Ed: TATSynEdit; c: TJsonConfig; const path: UnicodeString);
var
  SKey: UnicodeString;
begin
  if Ed.FileName<>'' then
  begin
    SKey:= AppConfigKeyForBookmarks(Ed);
    if Ed.Strings.Bookmarks.Count>0 then
      c.SetValue(SKey, EditorBookmarksToString(Ed))
    else
      c.DeleteValue(SKey);
  end;
end;

procedure TEditorFrame.DoSaveHistoryEx(Ed: TATSynEdit; c: TJsonConfig; const path: UnicodeString;
  AForSession: boolean);
begin
  //save 'split' value only when we have single file splitted,
  //but not 2 different files (like user.json + default.json)
  if FileName2='' then
  if UiOps.HistoryItems[ahhTabSplit] then
  begin
    if Splitted then
      c.SetValue(path+cHistory_TabSplit, Format('%s,%d', [
        BoolToStr(SplitHorz, '1', '0'),
        Round(SplitPos*cHistory_TabSplit_Mul)
        ]))
    else
      c.DeleteValue(path+cHistory_TabSplit);
  end;

  if UiOps.HistoryItems[ahhLexer] then
    c.SetDeleteValue(path+cHistory_Lexer, LexerName[Ed], '');

  if UiOps.HistoryItems[ahhEncoding] then
    c.SetValue(path+cHistory_Enc, Ed.EncodingName);

  if UiOps.HistoryItems[ahhTopLine] then
  begin
    c.SetDeleteValue(path+cHistory_TopLine, Ed.LineTop, 0);
    if EditorsLinked and Splitted then
      c.SetDeleteValue(path+cHistory_TopLine2, Ed2.LineTop, 0);
  end;

  if UiOps.HistoryItems[ahhWordWrap] then
    c.SetDeleteValue(path+cHistory_Wrap, Ord(Ed.OptWrapMode), Ord(EditorOps.OpWrapMode));

  if not Ed.IsReadOnlyAutodetected then
    c.SetDeleteValue(path+cHistory_ReadOnly, ReadOnly[Ed], false);

  if UiOps.HistoryItems[ahhRuler] then
    c.SetDeleteValue(path+cHistory_Ruler, Ord(Ed.OptRulerVisible), Ord(EditorOps.OpRulerShow));

  if UiOps.HistoryItems[ahhMinimap] then
    c.SetDeleteValue(path+cHistory_Minimap, Ord(Ed.OptMinimapVisible), Ord(EditorOps.OpMinimapShow));

  if UiOps.HistoryItems[ahhMicromap] then
    c.SetDeleteValue(path+cHistory_Micromap, Ord(Ed.OptMicromapVisible), Ord(EditorOps.OpMicromapShow));

  if UiOps.HistoryItems[ahhTabSize] then
  begin
    c.SetValue(path+cHistory_TabSize, Ed.OptTabSize);
    c.SetValue(path+cHistory_TabSpace, Ed.OptTabSpaces);
  end;

  if UiOps.HistoryItems[ahhUnprinted] then
  begin
    c.SetDeleteValue(path+cHistory_Unpri,        Ord(Ed.OptUnprintedVisible),     Ord(EditorOps.OpUnprintedShow));
    c.SetDeleteValue(path+cHistory_Unpri_Spaces, Ord(Ed.OptUnprintedSpaces),      Ord(Pos('s', EditorOps.OpUnprintedContent)>0));
    c.SetDeleteValue(path+cHistory_Unpri_Ends,   Ord(Ed.OptUnprintedEnds),        Ord(Pos('e', EditorOps.OpUnprintedContent)>0));
    c.SetDeleteValue(path+cHistory_Unpri_Detail, Ord(Ed.OptUnprintedEndsDetails), Ord(Pos('d', EditorOps.OpUnprintedContent)>0));
  end;

  if UiOps.HistoryItems[ahhLineNumbers] then
    c.SetDeleteValue(path+cHistory_LineNums, Ord(Ed.Gutter[Ed.Gutter.FindIndexByTag(ATEditorOptions.GutterTagNumbers)].Visible), 1);

  if UiOps.HistoryItems[ahhScale] then
    c.SetDeleteValue(path+cHistory_FontScale, Ed.OptScaleFont, 0);

  if UiOps.HistoryItems[ahhFolding] then
  begin
    c.SetDeleteValue(path+cHistory_FoldingShow, Ord(Ed.Gutter[Ed.Gutter.FindIndexByTag(ATEditorOptions.GutterTagFolding)].Visible), 1);
    c.SetDeleteValue(path+cHistory_FoldedRanges, Ed.FoldingAsString, '');
  end;

  if UiOps.HistoryItems[ahhTabColor] then
  begin
    if TabColor=clNone then
      c.DeleteValue(path+cHistory_TabColor)
    else
      c.SetValue(path+cHistory_TabColor, ColorToString(TabColor));
  end;

  if UiOps.HistoryItems[ahhCaret] then
    DoSaveHistory_Caret(Ed, c, path);

  {
  //it's bad to save markers along with other history items,
  //because if a snippet (with markers) inserted + file was not saved after it,
  //we have wrong markers to save
  if UiOps.HistoryItems[ahhMarkers] then
    c.SetDeleteValue(path+cHistory_Markers, Ed.Markers.AsMarkerString, '');
  }

  if UiOps.HistoryItems[ahhCodeTreeFilter] then
  begin
    c.SetDeleteValue(path+cHistory_CodeTreeFilter, FCodetreeFilter, '');

    if FCodetreeFilterHistory.Count>0 then
      c.SetValue(path+cHistory_CodeTreeFilters, FCodetreeFilterHistory)
    else
      c.DeleteValue(path+cHistory_CodeTreeFilters);
  end;
end;

procedure _WriteStringToFileInHiddenDir(const fn, s: string);
var
  dir: string;
begin
  if s<>'' then
  begin
    dir:= ExtractFileDir(fn);
    if not DirectoryExists(dir) then
    begin
      CreateDir(dir);
      {$ifdef windows}
      FileSetAttr(dir, faHidden);
      {$endif}
    end;
    DoWriteStringToFile(fn, s);
  end
  else
  begin
    if FileExists(fn) then
      DeleteFile(fn);
  end;
end;

procedure TEditorFrame.DoSaveUndo(Ed: TATSynEdit; const AFileName: string);
begin
  if IsFilenameListedInExtensionList(AFileName, UiOps.UndoPersistent) then
  begin
    _WriteStringToFileInHiddenDir(AppFile_UndoRedo(AFileName, false), Ed.UndoAsString);
    _WriteStringToFileInHiddenDir(AppFile_UndoRedo(AFileName, true), Ed.RedoAsString);
  end;
end;

procedure TEditorFrame.DoLoadHistory(Ed: TATSynEdit; AllowLoadEncoding, AllowLoadHistory, AllowLoadBookmarks: boolean);
var
  cfg: TJSONConfig;
  SFileName: string;
  path: string;
begin
  SFileName:= GetFileName(Ed);
  if SFileName='' then exit;

  path:= SMaskFilenameSlashes(SFileName);

  if not (AllowLoadHistory or AllowLoadBookmarks) then exit;
  if UiOps.MaxHistoryFiles<2 then exit;

  AppFileCheckForNullBytes(AppFile_HistoryFiles);

  cfg:= TJsonConfig.Create(nil);
  try
    try
      cfg.Formatted:= true;
      cfg.Filename:= AppFile_HistoryFiles;
    except
      on E: Exception do
      begin
        MsgBadConfig(AppFile_HistoryFiles, E.Message);
        exit
      end;
    end;

    if AllowLoadHistory then
      DoLoadHistoryEx(Ed, cfg, path, AllowLoadEncoding);

    if AllowLoadBookmarks then
      DoLoadHistory_Bookmarks(Ed, cfg, path);
  finally
    cfg.Free;
  end;
end;


procedure TEditorFrame.DoLoadHistoryEx(Ed: TATSynEdit; c: TJsonConfig;
  const path: UnicodeString; AllowEnc: boolean);
var
  str, str0, sFileName, sCaretString: string;
  Caret: TATCaretItem;
  NCaretPosX, NCaretPosY,
  NCaretEndX, NCaretEndY: integer;
  nTop, i: integer;
  Sep: TATStringSeparator;
  NFlag: integer;
begin
  sFileName:= GetFileName(Ed);

  //file not listed in history file? exit
  sCaretString:= c.GetValue(path+cHistory_Caret, '');
  if sCaretString='' then exit;

  {
  //markers
  //it's bad to save markers along with other history items,
  //so this is commented
  Ed.Markers.AsMarkerString:= c.GetValue(path+cHistory_Markers, '');
  }

  //split state
  str:= c.GetValue(path+cHistory_TabSplit, '');
  if str<>'' then
  begin
    Sep.Init(str);
    Sep.GetItemInt(i, 0);
    SplitHorz:= i=1;
    Splitted:= true;
    Sep.GetItemInt(i, 0);
    if i>0 then
      SplitPos:= i/cHistory_TabSplit_Mul;
  end;

  //top line
  //better set it before setting lexer (to not update syntax highlight because of OnScroll)
  nTop:= c.GetValue(path+cHistory_TopLine, 0);
  if nTop>0 then
    Ed.LineTop:= nTop;

  if EditorsLinked and Splitted then
  begin
    nTop:= c.GetValue(path+cHistory_TopLine2, 0);
    if nTop>0 then
      Ed2.LineTop:= nTop;
  end;

  //lexer
  str0:= LexerName[Ed];
  str:= c.GetValue(path+cHistory_Lexer, ''); //missed value means none-lexer (for Cud 1.104 or older)
  if (str='') or (str<>str0) then //better call it for none-lexer to apply "keys lexer -.json"
    LexerName[Ed]:= str;

  //encoding
  if AllowEnc then
  begin
    str0:= Ed.EncodingName;
    str:= c.GetValue(path+cHistory_Enc, str0);
    if str<>str0 then
    begin
      Ed.EncodingName:= str;
      //reread in encoding
      //but only if not modified (modified means other text is loaded)
      if sFileName<>'' then
        if not Ed.Modified then
        begin
          Ed.Strings.EncodingDetect:= false;
          Ed.LoadFromFile(sFileName);
          Ed.Strings.EncodingDetect:= true;
        end;
    end;
  end;

  TabColor:= StringToColorDef(c.GetValue(path+cHistory_TabColor, ''), clNone);

  if not Ed.IsReadOnlyAutodetected then
    ReadOnly[Ed]:= c.GetValue(path+cHistory_ReadOnly, ReadOnly[Ed]);

  if not FileWasBig[Ed] then
  begin
    NFlag:= Min(
      c.GetValue(path+cHistory_Wrap, -1),
      Ord(High(TATEditorWrapMode)) );
    if NFlag>=0 then
      if Ed.OptWrapMode<>TATEditorWrapMode(NFlag) then
      begin
        Ed.OptWrapMode:= TATEditorWrapMode(NFlag);
        Ed.IsModifiedWrapMode:= true;
        //DoPyEventState(Ed, EDSTATE_WRAP); //is not needed for session loading
      end;

    NFlag:= c.GetValue(path+cHistory_Minimap, -1);
    if NFlag>=0 then
    begin
      Ed.OptMinimapVisible:= NFlag=1;
      Ed.IsModifiedMinimapVisible:= true;
    end;

    NFlag:= c.GetValue(path+cHistory_Micromap, -1);
    if NFlag>=0 then
    begin
      Ed.OptMicromapVisible:= NFlag=1;
      Ed.IsModifiedMicromapVisible:= true;
    end;
  end;

  NFlag:= c.GetValue(path+cHistory_Ruler, -1);
  if NFlag>=0 then
  begin
    Ed.OptRulerVisible:= NFlag=1;
    Ed.IsModifiedRulerVisible:= true;
  end;

  Ed.OptTabSize:= c.GetValue(path+cHistory_TabSize, Ed.OptTabSize);
  Ed.OptTabSpaces:= c.GetValue(path+cHistory_TabSpace, Ed.OptTabSpaces);

  NFlag:= c.GetValue(path+cHistory_Unpri, -1);
  if NFlag>=0 then
  begin
    Ed.OptUnprintedVisible:= NFlag=1;
    Ed.IsModifiedUnprintedVisible:= true;
  end;

  NFlag:= c.GetValue(path+cHistory_Unpri_Spaces, -1);
  if NFlag>=0 then
  begin
    Ed.OptUnprintedSpaces:= NFlag=1;
    Ed.IsModifiedUnprintedSpaces:= true;
  end;

  NFlag:= c.GetValue(path+cHistory_Unpri_Ends, -1);
  if NFlag>=0 then
  begin
    Ed.OptUnprintedEnds:= NFlag=1;
    Ed.IsModifiedUnprintedEnds:= true;
  end;

  NFlag:= c.GetValue(path+cHistory_Unpri_Detail, -1);
  if NFlag>=0 then
  begin
    Ed.OptUnprintedEndsDetails:= NFlag=1;
    Ed.IsModifiedUnprintedEndDetails:= true;
  end;

  NFlag:= c.GetValue(path+cHistory_LineNums, -1);
  if NFlag>=0 then
  begin
    Ed.Gutter[Ed.Gutter.FindIndexByTag(ATEditorOptions.GutterTagNumbers)].Visible:= NFlag=1;
    Ed.IsModifiedGutterNumbersVisible:= true;
  end;

  NFlag:= c.GetValue(path+cHistory_FoldingShow, -1);
  if NFlag>=0 then
  begin
    Ed.Gutter[Ed.Gutter.FindIndexByTag(ATEditorOptions.GutterTagFolding)].Visible:= NFlag=1;
    Ed.IsModifiedGutterFoldingVisible:= true;
  end;

  Ed.OptScaleFont:= c.GetValue(path+cHistory_FontScale, 0);

  if Assigned(Lexer[Ed]) then
    Ed.FoldingAsStringTodo:= c.GetValue(path+cHistory_FoldedRanges, '');

  //caret
  Sep.Init(sCaretString);
  Sep.GetItemInt(NCaretPosX, 0);
  Sep.GetItemInt(NCaretPosY, 0);
  Sep.GetItemInt(NCaretEndX, -1);
  Sep.GetItemInt(NCaretEndY, -1);

  if Ed.Carets.Count<>1 then
    Ed.DoCaretSingle(0, 0);
  Caret:= Ed.Carets[0];
  if Caret.Change(NCaretPosX, NCaretPosY, NCaretEndX, NCaretEndY) then
  begin
    Ed.DoCaretsFixIncorrectPos(EditorOps.OpCaretOnLoadingLimitByLineEnds);
    Ed.DoEventCarets;

    //scroll to caret: needed for caret on a huge wrapped line
    if Ed.OptWrapMode=cWrapOff then
      if Caret.PosX>=Ed.GetVisibleColumns then
        Application.ProcessMessages;

    Ed.DoGotoPos(
      Point(Caret.PosX, Caret.PosY),
      Point(Caret.EndX, Caret.EndY),
      UiOps.FindIndentHorz,
      UiOps.FindIndentVert,
      false,
      true
      );
  end;

  //solve CudaText #3288, so Undo jumps to initial caret pos
  Ed.Strings.ActionSaveLastEditionPos(NCaretPosX, NCaretPosY);

  FCodetreeFilter:= c.GetValue(path+cHistory_CodeTreeFilter, '');
  c.GetValue(path+cHistory_CodeTreeFilters, FCodetreeFilterHistory, '');

  Ed.Update;
  if Splitted and EditorsLinked then
    Ed2.Update;
end;

procedure TEditorFrame.DoLoadUndo(Ed: TATSynEdit);
var
  SFileName, STemp: string;
begin
  SFileName:= GetFileName(Ed);
  if SFileName='' then exit;
  if IsFilenameListedInExtensionList(SFileName, UiOps.UndoPersistent) then
  begin
    STemp:= AppFile_UndoRedo(SFileName, false);
    if FileExists(STemp) then
      Ed.UndoAsString:= DoReadContentFromFile(STemp);

    STemp:= AppFile_UndoRedo(SFileName, true);
    if FileExists(STemp) then
      Ed.RedoAsString:= DoReadContentFromFile(STemp);
  end;
end;

function TEditorFrame.DoPyEvent(AEd: TATSynEdit; AEvent: TAppPyEvent;
  const AParams: TAppVariantArray): TAppPyEventResult;
begin
  if Assigned(FOnPyEvent) then
    Result:= FOnPyEvent(AEd, AEvent, AParams)
  else
  begin
    Result.Val:= evrOther;
    Result.Str:= '';
  end;
end;

procedure TEditorFrame.DoPyEventState(Ed: TATSynEdit; AState: integer);
begin
  DoPyEvent(Ed, cEventOnStateEd, [AppVariant(AState)]);
end;

function TEditorFrame.DoPyEvent_Macro(const AText: string): boolean;
begin
  Result:= DoPyEvent(Editor, cEventOnMacro, [AppVariant(AText)]).Val <> evrFalse;
end;


procedure TEditorFrame.SetTabColor(AColor: TColor);
var
  Gr: TATGroups;
  Pages: TATPages;
  NLocalGroups, NGlobalGroup, NTab: integer;
  D: TATTabData;
begin
  FTabColor:= AColor;
  GetFrameLocation(Self, Gr, Pages, NLocalGroups, NGlobalGroup, NTab);
  D:= Pages.Tabs.GetTabData(NTab);
  if Assigned(D) then
  begin
    D.TabColor:= AColor;
    Pages.Tabs.Invalidate;
  end;
end;

procedure TEditorFrame.SetTabFontColor(AColor: TColor);
var
  Gr: TATGroups;
  Pages: TATPages;
  NLocalGroups, NGlobalGroup, NTab: integer;
  D: TATTabData;
begin
  if FTabFontColor=AColor then exit;
  FTabFontColor:= AColor;
  GetFrameLocation(Self, Gr, Pages, NLocalGroups, NGlobalGroup, NTab);
  D:= Pages.Tabs.GetTabData(NTab);
  if Assigned(D) then
  begin
    D.TabFontColor:= AColor;
    Pages.Tabs.Invalidate;
  end;
end;

function TEditorFrame.GetTabExtModified(EdIndex: integer): boolean;
begin
  Result:= FTabExtModified[EdIndex];
end;

function TEditorFrame.GetTabExtDeleted(EdIndex: integer): boolean;
begin
  Result:= FTabExtDeleted[EdIndex];
end;

procedure TEditorFrame.SetTabExtModified(EdIndex: integer; AValue: boolean);
var
  Gr: TATGroups;
  Pages: TATPages;
  NLocalGroups, NGlobalGroup, NTab: integer;
  D: TATTabData;
begin
  if FTabExtModified[EdIndex]=AValue then exit;
  FTabExtModified[EdIndex]:= AValue;
  GetFrameLocation(Self, Gr, Pages, NLocalGroups, NGlobalGroup, NTab);
  D:= Pages.Tabs.GetTabData(NTab);
  if Assigned(D) then
  begin
    if EdIndex=0 then
      D.TabExtModified:= AValue
    else
      D.TabExtModified2:= AValue;
    Pages.Tabs.Invalidate;
  end;
end;

procedure TEditorFrame.SetTabExtDeleted(EdIndex: integer; AValue: boolean);
var
  Gr: TATGroups;
  Pages: TATPages;
  NLocalGroups, NGlobalGroup, NTab: integer;
  D: TATTabData;
begin
  if FTabExtDeleted[EdIndex]=AValue then exit;
  FTabExtDeleted[EdIndex]:= AValue;
  GetFrameLocation(Self, Gr, Pages, NLocalGroups, NGlobalGroup, NTab);
  D:= Pages.Tabs.GetTabData(NTab);
  if Assigned(D) then
  begin
    if EdIndex=0 then
      D.TabExtDeleted:= AValue
    else
      D.TabExtDeleted2:= AValue;
    Pages.Tabs.Invalidate;
  end;
end;

procedure TEditorFrame.SetTabPinned(AValue: boolean);
var
  Gr: TATGroups;
  Pages: TATPages;
  NLocalGroups, NGlobalGroup, NTab: integer;
  D: TATTabData;
begin
  if FTabPinned=AValue then exit;
  FTabPinned:= AValue;

  GetFrameLocation(Self, Gr, Pages, NLocalGroups, NGlobalGroup, NTab);
  D:= Pages.Tabs.GetTabData(NTab);
  if Assigned(D) then
  begin
    D.TabPinned:= AValue;
    Pages.Tabs.Invalidate;
  end;

  UpdatePinned(Ed1, true);
end;

procedure TEditorFrame.DoRemovePreviewStyle;
var
  Gr: TATGroups;
  Pages: TATPages;
  NLocalGroup, NGlobalGroup, NTab: integer;
  D: TATTabData;
begin
  GetFrameLocation(Self, Gr, Pages, NLocalGroup, NGlobalGroup, NTab);
  D:= Pages.Tabs.GetTabData(NTab);
  if Assigned(D) then
  begin
    UpdateTabPreviewStyle(D, false);
    Pages.Tabs.Invalidate;
  end;
end;

procedure TEditorFrame.SetTabImageIndex(AValue: integer);
var
  Gr: TATGroups;
  Pages: TATPages;
  NLocalGroup, NGlobalGroup, NTab: integer;
  D: TATTabData;
begin
  if FTabImageIndex=AValue then exit;
  FTabImageIndex:= AValue;

  GetFrameLocation(Self, Gr, Pages, NLocalGroup, NGlobalGroup, NTab);
  D:= Pages.Tabs.GetTabData(NTab);
  if Assigned(D) then
  begin
    D.TabImageIndex:= AValue;
    Pages.Tabs.Invalidate;
  end;
end;


function TEditorFrame.GetTabVisible: boolean;
var
  Gr: TATGroups;
  Pages: TATPages;
  NLocalGroup, NGlobalGroup, NTab: integer;
  D: TATTabData;
begin
  GetFrameLocation(Self, Gr, Pages, NLocalGroup, NGlobalGroup, NTab);
  D:= Pages.Tabs.GetTabData(NTab);
  if Assigned(D) then
    Result:= D.TabVisible
  else
    Result:= true;
end;

procedure TEditorFrame.SetTabVisible(AValue: boolean);
var
  Gr: TATGroups;
  Pages: TATPages;
  NLocalGroup, NGlobalGroup, NTab: integer;
  D: TATTabData;
begin
  GetFrameLocation(Self, Gr, Pages, NLocalGroup, NGlobalGroup, NTab);
  D:= Pages.Tabs.GetTabData(NTab);
  if Assigned(D) then
    if D.TabVisible<>AValue then
    begin
      D.TabVisible:= AValue;
      Pages.Tabs.Invalidate;
    end;
end;

procedure TEditorFrame.InitPanelInfo(var APanel: TPanel; const AText: string; AOnClick: TNotifyEvent; ARequirePython: boolean);
begin
  if ARequirePython then
    if not AppPython.Inited then exit;

  if not Assigned(APanel) then
  begin
    APanel:= TPanel.Create(Self);
    APanel.Parent:= Self;
    APanel.Align:= alTop;
    APanel.Visible:= false;
    APanel.Height:= ATEditorScale(26);
    APanel.BevelOuter:= bvNone;
  end;

  ApplyThemeToInfoPanel(APanel);

  APanel.Caption:= AText;
  APanel.OnClick:= AOnClick;

  APanel.Show;
end;

procedure TEditorFrame.InitNotificationPanel(Index: integer;
  AIsDeleted: boolean;
  var AControls: TFrameNotificationControls;
  AClickYes, AClickNo, AClickStop: TNotifyEvent);
var
  NPanelHeight, NBtnHeight, NBtnDistance: integer;
begin
  if Assigned(AControls.Panel) then exit;

  NPanelHeight:= ATEditorScale(UiOps.NotificationPanelHeight);
  NBtnHeight:= ATEditorScale(UiOps.NotificationButtonHeight);
  NBtnDistance:= ATEditorScale(UiOps.NotificationButtonsDistance);

  AControls.Panel:= TPanel.Create(Self);
  AControls.Panel.Visible:= false;
  AControls.Panel.Parent:= Self;
  AControls.Panel.Align:= alTop;
  AControls.Panel.Height:= NPanelHeight;
  AControls.Panel.BevelOuter:= bvNone;

  AControls.InfoPanel:= TPanel.Create(Self);
  AControls.InfoPanel.Parent:= AControls.Panel;
  AControls.InfoPanel.Height:= NPanelHeight;
  AControls.InfoPanel.BevelOuter:= bvNone;
  AControls.InfoPanel.ParentColor:= true;
  AControls.InfoPanel.AnchorSideLeft.Control:= AControls.Panel;
  AControls.InfoPanel.AnchorSideTop.Control:= AControls.Panel;
  AControls.InfoPanel.AnchorSideTop.Side:= asrCenter;

  AControls.ButtonStop:= TATButton.Create(Self);
  AControls.ButtonStop.Tag:= Index;
  AControls.ButtonStop.Parent:= AControls.Panel;
  AControls.ButtonStop.AnchorSideTop.Control:= AControls.Panel;
  AControls.ButtonStop.AnchorSideTop.Side:= asrCenter;
  AControls.ButtonStop.AnchorSideRight.Control:= AControls.Panel;
  AControls.ButtonStop.AnchorSideRight.Side:= asrBottom;
  AControls.ButtonStop.Anchors:= [akTop, akRight];
  AControls.ButtonStop.Height:= NBtnHeight;
  AControls.ButtonStop.BorderSpacing.Right:= NBtnDistance;
  AControls.ButtonStop.OnClick:= AClickStop;

  AControls.ButtonNo:= TATButton.Create(Self);
  AControls.ButtonNo.Tag:= Index;
  AControls.ButtonNo.Parent:= AControls.Panel;
  AControls.ButtonNo.AnchorSideTop.Control:= AControls.ButtonStop;
  AControls.ButtonNo.AnchorSideRight.Control:= AControls.ButtonStop;
  AControls.ButtonNo.Anchors:= [akTop, akRight];
  AControls.ButtonNo.Height:= NBtnHeight;
  AControls.ButtonNo.BorderSpacing.Right:= NBtnDistance;
  AControls.ButtonNo.OnClick:= AClickNo;
  AControls.ButtonNo.Visible:= not AIsDeleted;

  AControls.ButtonYes:= TATButton.Create(Self);
  AControls.ButtonYes.Tag:= Index;
  AControls.ButtonYes.Parent:= AControls.Panel;
  AControls.ButtonYes.AnchorSideTop.Control:= AControls.ButtonStop;;
  AControls.ButtonYes.AnchorSideRight.Control:= AControls.ButtonNo;
  AControls.ButtonYes.Anchors:= [akTop, akRight];
  AControls.ButtonYes.Height:= NBtnHeight;
  AControls.ButtonYes.BorderSpacing.Right:= NBtnDistance;
  AControls.ButtonYes.OnClick:= AClickYes;

  AControls.InfoPanel.AnchorSideRight.Control:= AControls.ButtonYes;
  AControls.InfoPanel.AnchorSideRight.Side:= asrLeft;
  AControls.InfoPanel.Anchors:= [akTop, akLeft, akRight];

  AControls.ButtonYes.TabOrder:= 0;
  AControls.ButtonNo.TabOrder:= 1;
  AControls.ButtonStop.TabOrder:= 2;
end;

procedure TEditorFrame.UpdateNotificationPanel(
  Index: integer;
  var AControls: TFrameNotificationControls;
  const ACaptionYes, ACaptionNo, ACaptionStop, ALabel: string);
begin
  AControls.ButtonYes.Caption:= ACaptionYes;
  AControls.ButtonNo.Caption:= ACaptionNo;
  AControls.ButtonStop.Caption:= ACaptionStop;

  AControls.ButtonYes.AutoSize:= true;
  AControls.ButtonNo.AutoSize:= true;
  AControls.ButtonStop.AutoSize:= true;

  AControls.InfoPanel.Caption:= ALabel;
end;

procedure TEditorFrame.NotifyAboutChange(Ed: TATSynEdit);
var
  EdIndex: integer;
  SFileName: string;
  bNewDeleted, bDeletedChanged: boolean;
  bShowPanel: boolean;
begin
  EdIndex:= EditorObjToIndex(Ed);
  if EdIndex<0 then exit;
  if EditorsLinked and (EdIndex>0) then exit;
  SFileName:= GetFileName(Ed);
  if SFileName='' then exit;

  bNewDeleted:= not FileExists(SFileName);
  bDeletedChanged:= TabExtDeleted[EdIndex]<>bNewDeleted;
  TabExtDeleted[EdIndex]:= bNewDeleted;

  if not bDeletedChanged then
    TabExtModified[EdIndex]:= true;

  if TabExtDeleted[EdIndex] or bDeletedChanged then
    TabExtModified[EdIndex]:= false;

  if TabExtDeleted[EdIndex] then
  begin
    DoHideNotificationPanel(NotifReloadControls[EdIndex]);
    if not NotifDeletedEnabled then exit;
  end
  else
  if TabExtModified[EdIndex] then
    DoHideNotificationPanel(NotifDeletedControls[EdIndex]);

  if TabExtDeleted[0] or TabExtDeleted[1] then
    TabFontColor:= GetAppColor(apclTabMarks)
  else
    TabFontColor:= clNone;

  if not TabExtDeleted[EdIndex] and TabExtModified[EdIndex] then
  begin
    case UiOps.NotificationConfirmReload of
      1:
        bShowPanel:= Ed.Modified or not Ed.Strings.UndoEmpty;
      2:
        bShowPanel:= Ed.Modified; //like Notepad++
      else
        bShowPanel:= true;
    end;

    if not bShowPanel then
    begin
      DoFileReload(Ed);
      exit
    end;
  end;

  InitNotificationPanel(EdIndex, false, NotifReloadControls[EdIndex], @NotifReloadYesClick, @NotifReloadNoClick, @NotifReloadStopClick);
  InitNotificationPanel(EdIndex, true, NotifDeletedControls[EdIndex], @NotifDeletedYesClick, @NotifDeletedNoClick, @NotifDeletedStopClick);

  ApplyThemeToInfoPanel(NotifReloadControls[EdIndex].Panel);
  ApplyThemeToInfoPanel(NotifDeletedControls[EdIndex].Panel);

  SFileName:= ExtractFileName(SFileName);
  UpdateNotificationPanel(EdIndex, NotifReloadControls[EdIndex], msgConfirmReloadYes, msgButtonCancel, msgConfirmReloadNoMore, msgConfirmFileChangedOutside+' '+SFileName);
  UpdateNotificationPanel(EdIndex, NotifDeletedControls[EdIndex], msgTooltipCloseTab, msgButtonCancel, msgConfirmReloadNoMore, msgConfirmFileDeletedOutside+' '+SFileName);

  NotifReloadControls[EdIndex].Panel.Visible:= TabExtModified[EdIndex] and not TabExtDeleted[EdIndex];
  NotifDeletedControls[EdIndex].Panel.Visible:= TabExtDeleted[EdIndex];
end;

procedure TEditorFrame.SetEnabledCodeTree(Ed: TATSynEdit; AValue: boolean);
var
  N: integer;
begin
  N:= EditorObjToTreeviewIndex(Ed);
  if FEnabledCodeTree[N]=AValue then Exit;
  FEnabledCodeTree[N]:= AValue;

  if not AValue then
    if Assigned(FCachedTreeview[N]) then
      FCachedTreeview[N].Items.Clear;
end;

procedure TEditorFrame.SetEnabledFolding(AValue: boolean);
begin
  Ed1.OptFoldEnabled:= AValue;
  Ed2.OptFoldEnabled:= AValue;
end;

function TEditorFrame.PictureSizes: TPoint;
begin
  if Assigned(FImageBox) then
    Result:= Point(FImageBox.ImageWidth, FImageBox.ImageHeight)
  else
    Result:= Point(0, 0);
end;


procedure TEditorFrame.DoGotoPos(Ed: TATSynEdit; APosX, APosY: integer);
begin
  if APosY<0 then exit;
  if APosX<0 then APosX:= 0; //allow x<0

  Ed.LineTop:= APosY;
  Ed.DoGotoPos(
    Point(APosX, APosY),
    Point(-1, -1),
    UiOps.FindIndentHorz,
    UiOps.FindIndentVert,
    true,
    true
    );
  Ed.Update;
end;

procedure TEditorFrame.DoLexerFromFilename(Ed: TATSynEdit; const AFileName: string);
var
  TempLexer: TecSyntAnalyzer;
  TempLexerLite: TATLiteLexer;
  SName: string;
  bTooBigForLexer: boolean;
begin
  if AFileName='' then exit;
  Lexer_DetectByFilename(
    AFileName,
    TempLexer,
    TempLexerLite,
    SName,
    bTooBigForLexer,
    FLexerChooseFunc
    );

  if Assigned(TempLexer) then
    Lexer[Ed]:= TempLexer
  else
  if Assigned(TempLexerLite) then
    LexerLite[Ed]:= TempLexerLite;

  if bTooBigForLexer and (TempLexer=nil) and (TempLexerLite=nil) then
  begin
    TempLexer:= AppManager.FindLexerByFilename(AFileName, nil);
    if Assigned(TempLexer) then
      InitPanelInfo(
        PanelNoHilite,
        Format(msgStatusLexerDisabledBySize, [
            UiOps.MaxFileSizeForLexer,
            TempLexer.LexerName,
            FileSize(AFileName) div (1024*1024)
            ]),
        @PanelNoHiliteClick,
        false
        );
  end;
end;

procedure TEditorFrame.SetFocus;
var
  Ed: TATSynEdit;
begin
  DoOnChangeCaption;
  DoShow;

  if Visible and Enabled then
  begin
    case FrameKind of
      efkEditor:
        begin
          Ed:= Editor;
          if Ed.Visible and Ed.Enabled then
            EditorFocus(Ed);
        end;

      efkBinaryViewer:
        begin
          if Assigned(FBin) and FBin.Visible and FBin.CanFocus then
            EditorFocus(FBin);
        end;

      efkImageViewer:
        begin
          if Assigned(FImageBox) and FImageBox.Visible and FImageBox.CanFocus then
            FImageBox.SetFocus;
        end;
    end;
  end;
end;

type
  TATSynEdit_Hack = class(TATSynEdit);

procedure TEditorFrame.BinaryOnKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Shift=[]) then
    if (Key=VK_UP) or
       (Key=VK_DOWN) or
       (Key=VK_LEFT) or
       (Key=VK_RIGHT) or
       (Key=VK_HOME) or
       (Key=VK_END) then
    exit;

  TATSynEdit_Hack(Editor).KeyDown(Key, Shift);
end;

procedure TEditorFrame.BinaryOnScroll(Sender: TObject);
begin
  DoOnUpdateStatusbar;
end;

procedure TEditorFrame.BinaryOnProgress(const ACurrentPos,
  AMaximalPos: Int64; var AContinueSearching: Boolean);
begin
  if Assigned(FOnProgress) then
    FOnProgress(nil, ACurrentPos, AMaximalPos, AContinueSearching);
end;

function TEditorFrame.BinaryFindFirst(AFinder: TATEditorFinder; AShowAll: boolean): boolean;
var
  Ops: TATStreamSearchOptions;
begin
  Ops:= [];
  if AFinder.OptCase then Include(Ops, asoCaseSens);
  if AFinder.OptWords then Include(Ops, asoWholeWords);
  if AShowAll then Include(Ops, asoShowAll);

  Result:= FBin.FindFirst(
    UTF8Encode(AFinder.StrFind), Ops, 0);
end;

function TEditorFrame.BinaryFindNext(ABack: boolean): boolean;
begin
  if FBinStream=nil then
    Result:= false
  else
    Result:= FBin.FindNext(ABack);
end;

procedure TEditorFrame.DoFileClose;
begin
  //clear adapters
  Lexer[Ed1]:= nil;
  if not EditorsLinked then
    Lexer[Ed2]:= nil;

  FFileName:= '';
  FFileName2:= '';
  UpdateCaptionFromFilename;

  //clear viewer
  DoDeactivateViewerMode;

  //clear picture
  DoDeactivatePictureMode;

  //clear editors
  EditorClear(Ed1);
  if not EditorsLinked then
    EditorClear(Ed2);

  UpdateModified(Ed1);
end;

procedure TEditorFrame.DoToggleFocusSplitEditors;
var
  Ed: TATSynEdit;
begin
  if Splitted then
  begin
    Ed:= EditorBro;
    if Ed.Enabled and Ed.Visible then
    begin
      FActiveSecondaryEd:= Ed=Ed2;
      Ed.SetFocus;
    end;
  end;
end;

procedure TEditorFrame.DoFocusNotificationPanel;
var
  i: integer;
begin
  if not Visible then exit;
  for i:= 0 to cFrameMaxEdIndex do
    if Assigned(NotifReloadControls[i].Panel) then
      if NotifReloadControls[i].Panel.Visible then
        NotifReloadControls[i].ButtonYes.SetFocus;
end;

procedure TEditorFrame.DoHideNotificationPanel(const AControls: TFrameNotificationControls);
begin
  if Assigned(AControls.Panel) then
    if AControls.Panel.Visible then
    begin
      if Visible then
        if AControls.ButtonYes.Focused or
           AControls.ButtonNo.Focused or
           AControls.ButtonStop.Focused then
          EditorFocus(Editor);
      AControls.Panel.Hide;
    end;
end;

procedure TEditorFrame.DoHideNotificationPanels;
var
  i: integer;
begin
  for i:= 0 to cFrameMaxEdIndex do
  begin
    DoHideNotificationPanel(NotifReloadControls[i]);
    DoHideNotificationPanel(NotifDeletedControls[i]);
  end;
end;

function TEditorFrame.GetIsPreview: boolean;
var
  Gr: TATGroups;
  Pages: TATPages;
  NLocalGroups, NGlobalGroup, NTab: integer;
  D: TATTabData;
begin
  Result:= false;
  GetFrameLocation(Self, Gr, Pages, NLocalGroups, NGlobalGroup, NTab);
  if NTab>=0 then
  begin
    D:= Pages.Tabs.GetTabData(NTab);
    if Assigned(D) then
      Result:= D.TabSpecial;
  end;
end;

procedure TEditorFrame.SetIsPreview(AValue: boolean);
var
  Gr: TATGroups;
  Pages: TATPages;
  NLocalGroups, NGlobalGroup, NTab: integer;
  D: TATTabData;
begin
  //can change only to False!
  if AValue then exit;

  GetFrameLocation(Self, Gr, Pages, NLocalGroups, NGlobalGroup, NTab);
  if NTab>=0 then
  begin
    D:= Pages.Tabs.GetTabData(NTab);
    if Assigned(D) then
    begin
      UpdateTabPreviewStyle(D, AValue);
      Pages.Tabs.Invalidate;
    end;
  end;
end;


function TEditorFrame.IsTreeBusy: boolean;
begin
  if EditorsLinked then
    Result:=
      Assigned(Adapter1) and Adapter1.TreeBusy
  else
    Result:=
      (Assigned(Adapter1) and Adapter1.TreeBusy) or
      (Assigned(Adapter2) and Adapter2.TreeBusy);
end;

function TEditorFrame.IsParsingBusy: boolean;
begin
  if EditorsLinked then
    Result:=
      Assigned(Adapter1) and Adapter1.IsParsingBusy
  else
    Result:=
      (Assigned(Adapter1) and Adapter1.IsParsingBusy) or
      (Assigned(Adapter2) and Adapter2.IsParsingBusy);
end;

function TEditorFrame.EditorObjToIndex(Ed: TATSynEdit): integer;
begin
  if Ed=Ed1 then
    Result:= 0
  else
  if Ed=Ed2 then
    Result:= 1
  else
    Result:= -1;
end;

function TEditorFrame.EditorIndexToObj(N: integer): TATSynEdit;
begin
  if N=0 then
    Result:= Ed1
  else
  if N=1 then
    Result:= Ed2
  else
    Result:= nil;
end;

procedure TEditorFrame.BracketJump(Ed: TATSynEdit);
begin
  EditorBracket_Action(Ed,
    bracketActionJump,
    FBracketSymbols,
    MaxInt
    );
end;

procedure TEditorFrame.BracketSelect(Ed: TATSynEdit);
begin
  EditorBracket_Action(Ed,
    bracketActionSelect,
    FBracketSymbols,
    MaxInt
    );
end;

procedure TEditorFrame.BracketSelectInside(Ed: TATSynEdit);
begin
  EditorBracket_Action(Ed,
    bracketActionSelectInside,
    FBracketSymbols,
    MaxInt
    );
end;

procedure TEditorFrame.SetBracketHilite(AValue: boolean);
begin
  if FBracketHilite=AValue then Exit;
  FBracketHilite:= AValue;

  EditorBracket_ClearHilite(Ed1);
  EditorBracket_ClearHilite(Ed2);

  if FBracketHilite then
  begin
    EditorOnChangeCaretPos(Ed1);
    EditorOnChangeCaretPos(Ed2);
  end;
end;

function TEditorFrame.Modified(ACheckOnSessionClosing: boolean=false): boolean;
begin
  if FEditorsLinked then
    Result:= EditorIsModifiedEx(Ed1)
  else
    Result:= EditorIsModifiedEx(Ed1) or EditorIsModifiedEx(Ed2);

  {
  //asked in issue #3891 - close modified untitled tabs w/o confirmation
  // it is to use in 2 places:
  // 1. TfmMain.DoFileCloseAll
  // 2. TfmMain.DoOnTabClose

  if Result and ACheckOnSessionClosing then
  begin
    //don't ask to save tab, if we are closing session with untitled tab,
    //and this tab was just saved (to session file) by Auto Save plugin
    if AppSessionIsClosing and
      EditorsLinked and
      (FileName='') and
      (VersionInSession=Ed1.Strings.ModifiedVersion) then
     Result:= false;
  end;
  }
end;

procedure TEditorFrame.PanelInfoClick(Sender: TObject);
begin
  if Assigned(PanelInfo) then
    PanelInfo.Hide;
  AppPython.RunCommand('cuda_prefs', 'dlg_cuda_options', []);
end;

procedure TEditorFrame.PanelNoHiliteClick(Sender: TObject);
begin
  if Assigned(PanelNoHilite) then
    PanelNoHilite.Hide;
end;

procedure TEditorFrame.CancelAutocompleteAutoshow;
begin
  FTextCharsTyped:= 0;
  AppRunAutocomplete:= false;
  AppRunAutocompleteInEditor:= nil;
end;

procedure TEditorFrame.LexerBackupSave;
begin
  if FLexerNameBackup<>'' then
    raise Exception.Create('Unexpected non-empty Frame.LexerNameBackup');
  FLexerNameBackup:= LexerName[Ed1];

  Lexer[Ed1]:= nil;
  if not EditorsLinked then
    Lexer[Ed2]:= nil;

  //fix crash: lexer is active in passive tab, LoadLexerLib deletes all lexers, user switches tab
  LexerInitial[Ed1]:= nil;
  LexerInitial[Ed2]:= nil;
end;

procedure TEditorFrame.LexerBackupRestore;
begin
  LexerName[Ed1]:= FLexerNameBackup;
  FLexerNameBackup:= '';
end;


end.
