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
  GraphUtil,
  ATTabs,
  ATGroups,
  ATSynEdit,
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
  proc_cmd,
  proc_colors,
  proc_files,
  proc_msg,
  proc_str,
  proc_py,
  proc_py_const,
  proc_miscutils,
  proc_lexer_styles,
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

type
  TFrameGetSaveDialog = procedure(var ASaveDlg: TSaveDialog) of object;

type
  { TEditorFrame }

  TEditorFrame = class(TFrame)
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    Splitter: TSplitter;
    TimerChange: TTimer;
    procedure btnReloadNoneClick(Sender: TObject);
    procedure btnReloadNoClick(Sender: TObject);
    procedure btnReloadYesClick(Sender: TObject);
    procedure TimerChangeTimer(Sender: TObject);
  private
    { private declarations }
    Adapter1: TATAdapterEControl;
    Adapter2: TATAdapterEControl;
    PanelReload: array[0..1] of TPanel;
    LabelReload: array[0..1] of TLabel;
    btnReloadYes: array[0..1] of TATButton;
    btnReloadNo: array[0..1] of TATButton;
    btnReloadNone: array[0..1] of TATButton;
    FTabCaption: string;
    FTabCaptionUntitled: string;
    FTabCaptionFromApi: boolean;
    FTabImageIndex: integer;
    FTabId: integer;
    FFileName: string;
    FFileName2: string;
    FFileWasBig: array[0..1] of boolean;
    FTextCharsTyped: integer;
    FActivationTime: Int64;
    FCodetreeFilter: string;
    FCodetreeFilterHistory: TStringList;
    FEnabledCodeTree: array[0..1] of boolean;
    FNotifEnabled: boolean;
    FOnChangeCaption: TNotifyEvent;
    FOnProgress: TATFinderProgress;
    FOnUpdateStatus: TNotifyEvent;
    FOnFocusEditor: TNotifyEvent;
    FOnEditorCommand: TATSynEditCommandEvent;
    FOnEditorChangeCaretPos: TNotifyEvent;
    FOnEditorScroll: TNotifyEvent;
    FOnSaveFile: TNotifyEvent;
    FOnAddRecent: TNotifyEvent;
    FOnPyEvent: TEditorFramePyEvent;
    FOnInitAdapter: TNotifyEvent;
    FOnLexerChange: TATEditorEvent;
    FSplitPos: double;
    FSplitHorz: boolean;
    FActiveSecondaryEd: boolean;
    FLocked: boolean;
    FTabColor: TColor;
    FTabSizeChanged: boolean;
    FFoldTodo: string;
    FTopLineTodo: integer;
    FTabKeyCollectMarkers: boolean;
    FInSession: boolean;
    FInHistory: boolean;
    FMacroRecord: boolean;
    FMacroString: string;
    FImageBox: TATImageBox;
    FBin: TATBinHex;
    FBinStream: TFileStream;
    FCheckFilenameOpened: TStrFunction;
    FOnMsgStatus: TStrEvent;
    FSaveDialog: TSaveDialog;
    FWasVisible: boolean;
    FInitialLexer1: TecSyntAnalyzer;
    FInitialLexer2: TecSyntAnalyzer;
    FSaveHistory: boolean;
    FEditorsLinked: boolean;
    FCachedTreeview: array[0..1] of TTreeView;
    FLexerChooseFunc: TecLexerChooseFunc;
    FBracketHilite: boolean;
    FBracketHiliteUserChanged: boolean;
    FBracketSymbols: string;
    FBracketMaxDistance: integer;
    FMicromapBmp: TBGRABitmap;
    FOnGetSaveDialog: TFrameGetSaveDialog;
    FOnAppClickLink: TATSynEditClickLinkEvent;

    procedure BinaryOnKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure BinaryOnScroll(Sender: TObject);
    procedure BinaryOnProgress(const ACurrentPos, AMaximalPos: Int64;
      var AContinueSearching: Boolean);
    procedure DoDeactivatePictureMode;
    procedure DoDeactivateViewerMode;
    procedure DoFileOpen_Ex(Ed: TATSynEdit; const AFileName: string;
      AAllowLoadHistory, AAllowLoadHistoryEnc, AAllowLexerDetect, AAllowErrorMsgBox, AKeepScroll: boolean; AOpenMode: TAppOpenMode);
    procedure DoImageboxScroll(Sender: TObject);
    procedure DoOnChangeCaption;
    procedure DoOnUpdateStatus;
    procedure EditorClickEndSelect(Sender: TObject; APrevPnt, ANewPnt: TPoint);
    procedure EditorClickMoveCaret(Sender: TObject; APrevPnt, ANewPnt: TPoint);
    procedure EditorDrawMicromap(Sender: TObject; ACanvas: TCanvas; const ARect: TRect);
    function EditorObjToTreeviewIndex(Ed: TATSynEdit): integer; inline;
    procedure EditorOnChange(Sender: TObject);
    procedure EditorOnChangeModified(Sender: TObject);
    procedure EditorOnChangeCaretPos(Sender: TObject);
    procedure EditorOnChangeState(Sender: TObject);
    procedure EditorOnClick(Sender: TObject);
    procedure EditorOnClickGap(Sender: TObject; AGapItem: TATGapItem; APos: TPoint);
    procedure EditorOnClickGutter(Sender: TObject; ABand, ALine: integer);
    procedure EditorOnClickDouble(Sender: TObject; var AHandled: boolean);
    procedure EditorOnClickLink(Sender: TObject; const ALink: string);
    procedure EditorOnClickMicroMap(Sender: TObject; AX, AY: integer);
    procedure EditorOnCommand(Sender: TObject; ACmd: integer; const AText: string; var AHandled: boolean);
    procedure EditorOnCommandAfter(Sender: TObject; ACommand: integer; const AText: string);
    procedure EditorOnDrawBookmarkIcon(Sender: TObject; C: TCanvas; ALineNum: integer; const ARect: TRect);
    procedure EditorOnEnter(Sender: TObject);
    procedure EditorOnDrawLine(Sender: TObject; C: TCanvas; AX, AY: integer;
      const AStr: atString; ACharSize: TPoint; const AExtent: TATIntArray);
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
    function GetTabKeyCollectMarkers: boolean;
    function GetUnprintedEnds: boolean;
    function GetUnprintedEndsDetails: boolean;
    function GetUnprintedShow: boolean;
    function GetUnprintedSpaces: boolean;
    procedure InitEditor(var ed: TATSynEdit);
    procedure InitPanelReload(Index: integer);
    function IsCaretInsideCommentOrString(Ed: TATSynEdit; AX, AY: integer): boolean;
    procedure SetBracketHilite(AValue: boolean);
    procedure SetEnabledCodeTree(Ed: TATSynEdit; AValue: boolean);
    procedure SetEnabledFolding(AValue: boolean);
    procedure SetFileName(const AValue: string);
    procedure SetFileName2(AValue: string);
    procedure SetFileWasBig(Ed: TATSynEdit; AValue: boolean);
    procedure SetInitialLexer(Ed: TATSynEdit; AValue: TecSyntAnalyzer);
    procedure SetLocked(AValue: boolean);
    procedure SetPictureScale(AValue: integer);
    procedure SetReadOnly(Ed: TATSynEdit; AValue: boolean);
    procedure SetTabColor(AColor: TColor);
    procedure SetTabImageIndex(AValue: integer);
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
    procedure UpdateEditorForBigFilesize(Ed: TATSynEdit);
    procedure UpdateEds(AUpdateWrapInfo: boolean=false);
    procedure UpdateCaptionFromFilename;
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

  protected
    procedure DoOnResize; override;
  public
    { public declarations }
    Ed1: TATSynEdit;
    Ed2: TATSynEdit;
    Groups: TATGroups;
    FileProps: TAppFileProps;
    FileProps2: TAppFileProps;

    constructor Create(AOwner: TComponent; AApplyCentering: boolean); reintroduce;
    destructor Destroy; override;
    function Editor: TATSynEdit;
    function EditorBro: TATSynEdit;
    function Modified: boolean;
    property Adapter[Ed: TATSynEdit]: TATAdapterEControl read GetAdapter;
    procedure EditorOnKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure DoShow;

    function EditorIndexToObj(N: integer): TATSynEdit;
    function EditorObjToIndex(Ed: TATSynEdit): integer;
    property ReadOnly[Ed: TATSynEdit]: boolean read GetReadOnly write SetReadOnly;
    property TabCaption: string read FTabCaption write SetTabCaption;
    property TabCaptionUntitled: string read FTabCaptionUntitled write FTabCaptionUntitled;
    property TabImageIndex: integer read FTabImageIndex write SetTabImageIndex;
    property TabCaptionFromApi: boolean read FTabCaptionFromApi write FTabCaptionFromApi;
    property TabId: integer read FTabId;
    property TabIsPreview: boolean read GetIsPreview write SetIsPreview;

    property CachedTreeViewInited[Ed: TATSynEdit]: boolean read GetCachedTreeviewInited;
    property CachedTreeView[Ed: TATSynEdit]: TTreeView read GetCachedTreeview;
    property SaveHistory: boolean read FSaveHistory write FSaveHistory;
    procedure UpdateModified(Ed: TATSynEdit; AWithEvent: boolean= true);
    procedure UpdateReadOnlyFromFile(Ed: TATSynEdit);
    procedure UpdateFrame(AUpdatedText: boolean);
    procedure FixLexerIfDeleted(Ed: TATSynEdit; const ALexerName: string);

    property NotifEnabled: boolean read FNotifEnabled write FNotifEnabled;
    procedure NotifyAboutChange(Ed: TATSynEdit);

    property FileName: string read FFileName write SetFileName;
    property FileName2: string read FFileName2;
    property LexerChooseFunc: TecLexerChooseFunc read FLexerChooseFunc write FLexerChooseFunc;
    function GetFileName(Ed: TATSynEdit): string;
    procedure SetFileName(Ed: TATSynEdit; const AFileName: string);
    property FileWasBig[Ed: TATSynEdit]: boolean read GetFileWasBig write SetFileWasBig;

    property Lexer[Ed: TATSynEdit]: TecSyntAnalyzer read GetLexer write SetLexer;
    property LexerLite[Ed: TATSynEdit]: TATLiteLexer read GetLexerLite write SetLexerLite;
    property LexerName[Ed: TATSynEdit]: string read GetLexerName write SetLexerName;
    property LexerInitial[Ed: TATSynEdit]: TecSyntAnalyzer read GetInitialLexer write SetInitialLexer;
    function LexerNameAtPos(Ed: TATSynEdit; APos: TPoint): string;

    property Locked: boolean read FLocked write SetLocked;
    property CommentString[Ed: TATSynEdit]: string read GetCommentString;
    property TabColor: TColor read FTabColor write SetTabColor;
    property TabSizeChanged: boolean read FTabSizeChanged write FTabSizeChanged;
    property TabKeyCollectMarkers: boolean read GetTabKeyCollectMarkers write FTabKeyCollectMarkers;
    property InSession: boolean read FInSession write FInSession;
    property InHistory: boolean read FInHistory write FInHistory;
    property TopLineTodo: integer read FTopLineTodo write FTopLineTodo; //always use it instead of Ed.LineTop
    property TextCharsTyped: integer read FTextCharsTyped write FTextCharsTyped;
    property EnabledCodeTree[Ed: TATSynEdit]: boolean read GetEnabledCodeTree write SetEnabledCodeTree;
    property CodetreeFilter: string read FCodetreeFilter write FCodetreeFilter;
    property CodetreeFilterHistory: TStringList read FCodetreeFilterHistory;
    property ActivationTime: Int64 read FActivationTime write FActivationTime;
    function IsEmpty: boolean;
    procedure ApplyTheme;
    function IsEditorFocused: boolean;
    procedure SetFocus; reintroduce;
    function IsText: boolean;
    function IsPicture: boolean;
    function IsBinary: boolean;
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
    function IsParsingBusy: boolean;
    //file
    procedure DoFileClose;
    procedure DoFileOpen(const AFileName, AFileName2: string; AAllowLoadHistory, AAllowLexerDetect,
      AAllowErrorMsgBox: boolean; AOpenMode: TAppOpenMode);
    procedure DoFileOpen_AsBinary(const AFileName: string; AMode: TATBinHexMode);
    procedure DoFileOpen_AsPicture(const AFileName: string);
    function DoFileSave(ASaveAs, AAllEditors: boolean): boolean;
    function DoFileSave_Ex(Ed: TATSynEdit; ASaveAs: boolean): boolean;
    procedure DoFileReload_DisableDetectEncoding(Ed: TATSynEdit);
    function DoFileReload(Ed: TATSynEdit): boolean;
    procedure DoLexerFromFilename(Ed: TATSynEdit; const AFileName: string);
    //history
    procedure DoSaveHistory(Ed: TATSynEdit);
    procedure DoSaveHistoryEx(Ed: TATSynEdit; c: TJsonConfig; const path: UnicodeString);
    procedure DoLoadHistory(Ed: TATSynEdit; AllowEnc: boolean);
    procedure DoLoadHistoryEx(Ed: TATSynEdit; c: TJsonConfig; const path: UnicodeString; AllowEnc: boolean);
    //misc
    function DoPyEvent(AEd: TATSynEdit; AEvent: TAppPyEvent; const AParams: TAppVariantArray): TAppPyEventResult;
    procedure DoGotoPos(Ed: TATSynEdit; APosX, APosY: integer);
    procedure DoRestoreFolding(Ed: TATSynEdit);
    procedure DoRemovePreviewStyle;
    procedure DoToggleFocusSplitEditors;
    procedure DoFocusNotificationPanel;
    procedure DoHideNotificationPanels;
    procedure DoHideNotificationPanel(Index: integer);
    //macro
    procedure DoMacroStart;
    procedure DoMacroStop(ACancel: boolean);
    property MacroRecord: boolean read FMacroRecord;
    property MacroString: string read FMacroString write FMacroString;

    //events
    property OnGetSaveDialog: TFrameGetSaveDialog read FOnGetSaveDialog write FOnGetSaveDialog;
    property OnProgress: TATFinderProgress read FOnProgress write FOnProgress;
    property OnCheckFilenameOpened: TStrFunction read FCheckFilenameOpened write FCheckFilenameOpened;
    property OnMsgStatus: TStrEvent read FOnMsgStatus write FOnMsgStatus;
    property OnFocusEditor: TNotifyEvent read FOnFocusEditor write FOnFocusEditor;
    property OnChangeCaption: TNotifyEvent read FOnChangeCaption write FOnChangeCaption;
    property OnUpdateStatus: TNotifyEvent read FOnUpdateStatus write FOnUpdateStatus;
    property OnEditorCommand: TATSynEditCommandEvent read FOnEditorCommand write FOnEditorCommand;
    property OnEditorChangeCaretPos: TNotifyEvent read FOnEditorChangeCaretPos write FOnEditorChangeCaretPos;
    property OnEditorScroll: TNotifyEvent read FOnEditorScroll write FOnEditorScroll;
    property OnSaveFile: TNotifyEvent read FOnSaveFile write FOnSaveFile;
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

var
  //must be set in FormMain.OnShow
  AllowFrameParsing: boolean = false;


implementation

{$R *.lfm}

const
  cHistory_Lexer       = '/lexer';
  cHistory_Enc         = '/enc';
  cHistory_TopLine     = '/top';
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
  //cHistory_Markers     = '/mrk';
  cHistory_TabColor    = '/color';
  cHistory_Bookmark    = '/bm';
  cHistory_BookmarkKind = '/bm_kind';
  cHistory_Fold        = '/folded';
  cHistory_CodeTreeFilter = '/codetree_filter';
  cHistory_CodeTreeFilters = '/codetree_filters';

var
  FLastTabId: integer = 0;


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
    D.TabFontStyle:= StringToFontStyles(UiOps.TabPreviewFontStyle)
  else
    D.TabFontStyle:= [];
end;


{ TEditorFrame }

procedure TEditorFrame.SetTabCaption(const AValue: string);
var
  Params: TAppVariantArray;
  Upd: boolean;
begin
  if AValue='?' then Exit;
  Upd:= FTabCaption<>AValue;

  FTabCaption:= AValue; //don't check Upd here (for Win32)

  if Upd then
  begin
    SetLength(Params, 1);
    Params[0]:= AppVariant(EDSTATE_TAB_TITLE);
    DoPyEvent(Ed1, cEventOnStateEd, Params);
  end;

  DoOnChangeCaption;
end;

procedure TEditorFrame.UpdateCaptionFromFilename;
var
  Name1, Name2: string;
begin
  //avoid updating caption if API already had set it
  if FTabCaptionFromApi then exit;

  if EditorsLinked then
  begin
    if FFileName='' then
      Name1:= FTabCaptionUntitled
    else
      Name1:= ExtractFileName_Fixed(FFileName);
    Name1:= msgModified[Ed1.Modified]+Name1;

    TabCaption:= Name1;
  end
  else
  begin
    if (FFileName='') and (FFileName2='') then
      TabCaption:= FTabCaptionUntitled
    else
    begin
      Name1:= ExtractFileName_Fixed(FFileName);
      if Name1='' then Name1:= msgUntitledTab;
      Name1:= msgModified[Ed1.Modified]+Name1;

      Name2:= ExtractFileName_Fixed(FFileName2);
      if Name2='' then Name2:= msgUntitledTab;
      Name2:= msgModified[Ed2.Modified]+Name2;

      TabCaption:= Name1+' | '+Name2;
    end;
  end;

  UpdateTabTooltip;
end;

procedure TEditorFrame.EditorOnClick(Sender: TObject);
var
  Ed: TATSynEdit;
  StateString: string;
  Params: TAppVariantArray;
begin
  Ed:= Sender as TATSynEdit;

  StateString:= ConvertShiftStateToString(KeyboardStateToShiftState);
  FTextCharsTyped:= 0; //reset count for option "autocomplete_autoshow_chars"

  if UiOps.MouseGotoDefinition<>'' then
    if StateString=UiOps.MouseGotoDefinition then
    begin
      SetLength(Params, 0);
      DoPyEvent(Ed, cEventOnGotoDef, Params);
      exit;
    end;

  SetLength(Params, 1);
  Params[0]:= AppVariant(StateString);
  DoPyEvent(Ed, cEventOnClick, Params);
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
  Params: TAppVariantArray;
  Res: TAppPyEventResult;
begin
  SetLength(Params, 2);
  Params[0]:= AppVariant(Key);
  Params[1]:= AppVariant(ConvertShiftStateToString(Shift));

  //result=False: block the key
  Res:= DoPyEvent(Sender as TATSynEdit, cEventOnKey, Params);
  if Res.Val=evrFalse then
  begin
    Key:= 0;
    Exit
  end;
end;

procedure TEditorFrame.EditorOnKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  Params: TAppVariantArray;
begin
  SetLength(Params, 2);
  Params[0]:= AppVariant(Key);
  Params[1]:= AppVariant(ConvertShiftStateToString(Shift));

  //fire on_key_up only for keys Ctrl, Alt, Shift
  //event result is ignored
  case Key of
    VK_CONTROL,
    VK_MENU,
    VK_SHIFT,
    VK_RSHIFT:
      DoPyEvent(Sender as TATSynEdit, cEventOnKeyUp, Params);
  end;
end;

procedure TEditorFrame.EditorOnPaste(Sender: TObject; var AHandled: boolean;
  AKeepCaret, ASelectThen: boolean);
var
  Params: TAppVariantArray;
begin
  SetLength(Params, 2);
  Params[0]:= AppVariant(AKeepCaret);
  Params[1]:= AppVariant(ASelectThen);

  if DoPyEvent(Sender as TATSynEdit, cEventOnPaste, Params).Val = evrFalse then
    AHandled:= true;
end;

procedure TEditorFrame.EditorOnScroll(Sender: TObject);
var
  Params: TAppVariantArray;
begin
  if Assigned(FOnEditorScroll) then
    FOnEditorScroll(Sender);

  SetLength(Params, 0);
  DoPyEvent(Sender as TATSynEdit, cEventOnScroll, Params);
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
  //analize file, when frame is shown for the 1st time
  if AllowFrameParsing and not FWasVisible then
  begin
    FWasVisible:= true;
    //ShowMessage('show frame: '+FileName); ////debug

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
  end;
end;

procedure TEditorFrame.TimerChangeTimer(Sender: TObject);
var
  Params: TAppVariantArray;
begin
  SetLength(Params, 0);
  TimerChange.Enabled:= false;
  DoPyEvent(Editor, cEventOnChangeSlow, Params);
end;

procedure TEditorFrame.btnReloadYesClick(Sender: TObject);
var
  Index: integer;
  Ed: TATSynEdit;
begin
  Index:= (Sender as TComponent).Tag;
  Ed:= EditorIndexToObj(Index);
  if Ed=nil then exit;
  DoFileReload(Ed);
  EditorFocus(Ed);
end;

procedure TEditorFrame.btnReloadNoClick(Sender: TObject);
var
  Index: integer;
  Ed: TATSynEdit;
begin
  Index:= (Sender as TComponent).Tag;
  Ed:= EditorIndexToObj(Index);
  if Ed=nil then exit;
  DoHideNotificationPanel(Index);
  EditorFocus(Ed);
end;

procedure TEditorFrame.btnReloadNoneClick(Sender: TObject);
begin
  NotifEnabled:= false;
  btnReloadNoClick(Sender);
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
const
  cMaxSelectedLinesForAutoCopy = 200;
var
  Params: TAppVariantArray;
  Ed: TATSynEdit;
begin
  Ed:= Sender as TATSynEdit;
  if Assigned(FOnEditorChangeCaretPos) then
    FOnEditorChangeCaretPos(Sender);

  DoOnUpdateStatus;

  if FBracketHilite then
    EditorBracket_Action(Ed,
      bracketActionHilite,
      FBracketSymbols,
      FBracketMaxDistance
      );

  //support Primary Selection on Linux
  {$ifdef linux}
  EditorCopySelToPrimarySelection(Ed, cMaxSelectedLinesForAutoCopy);
  {$endif}

  SetLength(Params, 0);
  DoPyEvent(Ed, cEventOnCaret, Params);
end;

procedure TEditorFrame.EditorOnHotspotEnter(Sender: TObject; AHotspotIndex: integer);
var
  Params: TAppVariantArray;
begin
  SetLength(Params, 2);
  Params[0]:= AppVariant(true); //hotspot enter
  Params[1]:= AppVariant(AHotspotIndex);

  DoPyEvent(Sender as TATSynEdit, cEventOnHotspot, Params);
end;

procedure TEditorFrame.EditorOnHotspotExit(Sender: TObject; AHotspotIndex: integer);
var
  Params: TAppVariantArray;
begin
  SetLength(Params, 2);
  Params[0]:= AppVariant(false); //hotspot exit
  Params[1]:= AppVariant(AHotspotIndex);

  DoPyEvent(Sender as TATSynEdit, cEventOnHotspot, Params);
end;

procedure TEditorFrame.EditorOnDrawLine(Sender: TObject; C: TCanvas; AX,
  AY: integer; const AStr: atString; ACharSize: TPoint;
  const AExtent: TATIntArray);
const
  cRegexRGB = 'rgba?\(\s*(\d{1,3})\s*,\s*(\d{1,3})\s*,\s*(\d{1,3})\s*(,\s*[\.\d]+\s*)?\)';
  cRegexHSL = 'hsla?\(\s*(\d{1,3})\s*,\s*(\d{1,3})%\s*,\s*(\d{1,3})%\s*(,\s*[\.\d%]+\s*)?\)';
var
  X1, X2, Y, NLen: integer;
  NColor: TColor;
  Parts: TRegexParts;
  Ch: atChar;
  ValueR, ValueG, ValueB: byte;
  ValueH, ValueS, ValueL: word;
  Substr: string;
  i: integer;
begin
  if AStr='' then Exit;
  if not IsFilenameListedInExtensionList(FileName, EditorOps.OpUnderlineColorFiles)
    then exit;

  for i:= 1 to Length(AStr)-3 do
  begin
    Ch:= AStr[i];

    //find #rgb, #rrggbb
    if (Ch='#') and IsCharHexDigit(AStr[i+1]) then
    begin
      Substr:= Copy(AStr, i+1, 8+1);
      NColor:= SHtmlColorToColor(PChar(Substr), NLen, clNone);
      if NColor=clNone then Continue;

      if i-2>=0 then
        X1:= AX+AExtent[i-2]
      else
        X1:= AX;
      X2:= AX+AExtent[i-1+NLen];
      Y:= AY+ACharSize.Y;

      C.Brush.Color:= NColor;
      C.FillRect(X1, Y-EditorOps.OpUnderlineColorSize, X2, Y);
    end
    else
    //find rgb(...), rgba(...)
    if (Ch='r') and
      (i+6<=Length(AStr)) and
      (AStr[i+1]='g') and
      (AStr[i+2]='b') and
      ((AStr[i+3]='(') or ((AStr[i+3]='a') and (AStr[i+4]='('))) and
      ((i=1) or not IsCharWord(AStr[i-1], cDefaultNonWordChars)) //word boundary
    then
    begin
      if SRegexFindParts(cRegexRGB, Copy(AStr, i, MaxInt), Parts) then
        if Parts[0].Pos=1 then //need at i-th char
        begin
          ValueR:= Min(255, StrToIntDef(Parts[1].Str, 0));
          ValueG:= Min(255, StrToIntDef(Parts[2].Str, 0));
          ValueB:= Min(255, StrToIntDef(Parts[3].Str, 0));

          NColor:= RGB(ValueR, ValueG, ValueB);
          NLen:= Parts[0].Len;

          if i-2>=0 then
            X1:= AX+AExtent[i-2]
          else
            X1:= AX;
          X2:= AX+AExtent[i-2+NLen];
          Y:= AY+ACharSize.Y;

          C.Brush.Color:= NColor;
          C.FillRect(X1, Y-EditorOps.OpUnderlineColorSize, X2, Y);
        end;
    end
    else
    //find hsl(...), hsla(...)
    if (Ch='h') and
      (i+6<=Length(AStr)) and
      (AStr[i+1]='s') and
      (AStr[i+2]='l') and
      ((AStr[i+3]='(') or ((AStr[i+3]='a') and (AStr[i+4]='('))) and
      ((i=1) or not IsCharWord(AStr[i-1], cDefaultNonWordChars)) //word boundary
      then
      begin
        if SRegexFindParts(cRegexHSL, Copy(AStr, i, MaxInt), Parts) then
          if Parts[0].Pos=1 then //need at i-th char
          begin
            ValueH:= StrToIntDef(Parts[1].Str, 0) * 255 div 360; //degrees -> 0..255
            ValueS:= StrToIntDef(Parts[2].Str, 0) * 255 div 100; //percents -> 0..255
            ValueL:= StrToIntDef(Parts[3].Str, 0) * 255 div 100; //percents -> 0..255

            NColor:= HLStoColor(ValueH, ValueL, ValueS);
            NLen:= Parts[0].Len;

            if i-2>=0 then
              X1:= AX+AExtent[i-2]
            else
              X1:= AX;
            X2:= AX+AExtent[i-2+NLen];
            Y:= AY+ACharSize.Y;

            C.Brush.Color:= NColor;
            C.FillRect(X1, Y-EditorOps.OpUnderlineColorSize, X2, Y);
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
  AppGetFileProps(FFileName, FileProps);
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
  end;
end;

procedure TEditorFrame.SetFileName2(AValue: string);
begin
  if SameFileName(FFileName2, AValue) then Exit;
  FFileName2:= AValue;
  AppGetFileProps(FFileName2, FileProps2);
end;

procedure TEditorFrame.UpdateEditorForBigFilesize(Ed: TATSynEdit);
begin
  Ed.OptWrapMode:= cWrapOff;
  Ed.OptMicromapVisible:= false;
  Ed.OptMinimapVisible:= false;
end;

procedure TEditorFrame.SetFileWasBig(Ed: TATSynEdit; AValue: boolean);
var
  Index: integer;
begin
  if EditorsLinked then
  begin
    FFileWasBig[0]:= AValue;
    FFileWasBig[1]:= AValue;
    if AValue then
    begin
      UpdateEditorForBigFilesize(Ed1);
      UpdateEditorForBigFilesize(Ed2);
    end;
  end
  else
  begin
    Index:= EditorObjToIndex(Ed);
    if Index<0 then exit;
    FFileWasBig[Index]:= AValue;
    if AValue then
      UpdateEditorForBigFilesize(Ed);
  end;
end;

procedure TEditorFrame.SetInitialLexer(Ed: TATSynEdit; AValue: TecSyntAnalyzer);
var
  Index: integer;
begin
  if EditorsLinked then
    FInitialLexer1:= AValue
  else
  begin
    Index:= EditorObjToIndex(Ed);
    if Index=0 then
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
  Ed.ModeReadOnly:= AValue;
  if (Ed=Ed1) and EditorsLinked then
    Ed2.ModeReadOnly:= AValue;
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


function TEditorFrame.LexerNameAtPos(Ed: TATSynEdit; APos: TPoint): string;
var
  CurAdapter: TATAdapterHilite;
  an: TecSyntAnalyzer;
begin
  Result:= '';
  CurAdapter:= Ed.AdapterForHilite;
  if CurAdapter=nil then exit;

  if CurAdapter is TATAdapterEControl then
  begin
    an:= TATAdapterEControl(CurAdapter).LexerAtPos(APos);
    if Assigned(an) then
      Result:= an.LexerName;
  end
  else
  if CurAdapter is TATLiteLexer then
    Result:= TATLiteLexer(CurAdapter).LexerName+msgLiteLexerSuffix;
end;

procedure TEditorFrame.SetSplitHorz(AValue: boolean);
var
  al: TAlign;
begin
  FSplitHorz:= AValue;
  if not IsText then exit;

  if FSplitHorz then
    al:= alBottom
  else
    al:= alRight;
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
  if not IsText then exit;
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
  if not IsText then exit;
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
  Params: TAppVariantArray;
begin
  Ed:= Sender as TATSynEdit;

  if FBracketHilite then
    EditorBracket_ClearHilite(Ed);

  //sync changes in 2 editors, when frame is splitted
  if Splitted and EditorsLinked then
  begin
    if Ed=Ed1 then
      EdOther:= Ed2
    else
      EdOther:= Ed1;

    EdOther.DoCaretsFixIncorrectPos(false);
    EdOther.Update(true);
  end;

  SetLength(Params, 0);
  DoPyEvent(Ed, cEventOnChange, Params);

  TimerChange.Enabled:= false;
  TimerChange.Interval:= UiOps.PyChangeSlow;
  TimerChange.Enabled:= true;
end;

procedure TEditorFrame.EditorOnChangeModified(Sender: TObject);
begin
  UpdateModified(Sender as TATSynEdit);
end;

procedure TEditorFrame.EditorOnChangeState(Sender: TObject);
begin
  //
end;

procedure TEditorFrame.UpdateModified(Ed: TATSynEdit; AWithEvent: boolean);
var
  Params: TAppVariantArray;
begin
  //when modified, remove "Preview tab" style (italic caption)
  if (Ed=Ed1) and Ed.Modified then
    DoRemovePreviewStyle;

  UpdateCaptionFromFilename;
  DoOnChangeCaption;

  if AWithEvent then
  begin
    SetLength(Params, 1);
    Params[0]:= AppVariant(EDSTATE_MODIFIED);
    DoPyEvent(Ed, cEventOnStateEd, Params);
  end;

  DoOnUpdateStatus;
end;

procedure TEditorFrame.EditorOnEnter(Sender: TObject);
var
  Params: TAppVariantArray;
  IsEd2: boolean;
begin
  IsEd2:= Sender=Ed2;
  if IsEd2<>FActiveSecondaryEd then
  begin
    FActiveSecondaryEd:= IsEd2;
    DoOnUpdateStatus;
  end;

  if Assigned(FOnFocusEditor) then
    FOnFocusEditor(Sender);

  SetLength(Params, 0);
  DoPyEvent(Sender as TATSynEdit, cEventOnFocus, Params);

  FActivationTime:= GetTickCount64;
end;

procedure TEditorFrame.EditorOnCommand(Sender: TObject; ACmd: integer;
  const AText: string; var AHandled: boolean);
var
  Ed: TATSynEdit;
  Caret: TATCaretItem;
  Str: atString;
  ch: char;
begin
  Ed:= Sender as TATSynEdit;
  if Ed.Carets.Count=0 then exit;
  Caret:= Ed.Carets[0];

  case ACmd of
    cCommand_TextInsert:
      begin
        //improve auto-closing brackets, avoid duplicate )]}
        //when closing bracket is typed over itself
        if Ed.Strings.IsIndexValid(Caret.PosY) then
          if Length(AText)=1 then
          begin
            ch:= EditorBracket_GetPairForClosingBracketOrQuote(AText[1]);
            if (ch<>#0) and (Pos(ch, Ed.OptAutoCloseBrackets)>0) then
            begin
              Str:= Ed.Strings.Lines[Caret.PosY];
              if (Caret.PosX<Length(Str)) then
                if Str[Caret.PosX+1] = AText[1] then
                begin
                  Ed.DoCommand(cCommand_KeyRight);
                  AHandled:= true;
                  exit;
                end;
            end;
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
    cCommand_TextDeleteWordNext,
    cCommand_TextDeleteWordPrev:
      begin
        FTextCharsTyped:= 0;
      end;

    cCommand_ClipboardPaste,
    cCommand_ClipboardPaste_Select,
    cCommand_ClipboardPaste_KeepCaret,
    cCommand_ClipboardPaste_Column,
    cCommand_ClipboardPaste_ColumnKeepCaret:
      begin
        Adapter[Ed].StopTreeUpdate;
        Adapter[Ed].Stop;
      end;
  end;

  if Assigned(FOnEditorCommand) then
    FOnEditorCommand(Sender, ACmd, AText, AHandled);
end;

procedure TEditorFrame.EditorOnCommandAfter(Sender: TObject; ACommand: integer;
  const AText: string);
var
  Ed: TATSynEdit;
  Caret: TATCaretItem;
  SLexerName: string;
  bWordChar, bIdentChar: boolean;
begin
  Ed:= Sender as TATSynEdit;
  if Ed.Carets.Count<>1 then exit;
  Caret:= Ed.Carets[0];
  if not Ed.Strings.IsIndexValid(Caret.PosY) then exit;

  //some commands affect FTextCharsTyped
  if (ACommand=cCommand_KeyBackspace) then
  begin
    if FTextCharsTyped>0 then
      Dec(FTextCharsTyped);
    exit;
  end;

  //autoshow autocompletion
  if (ACommand=cCommand_TextInsert) and
    (Length(AText)=1) then
  begin
    //autoshow by trigger chars
    if (Ed.OptAutocompleteTriggerChars<>'') and
      (Pos(AText[1], Ed.OptAutocompleteTriggerChars)>0) then
    begin
      //check that we are not inside comment/string
      if IsCaretInsideCommentOrString(Ed, Caret.PosX, Caret.PosY) then exit;

      FTextCharsTyped:= 0;
      Ed.DoCommand(cmd_AutoComplete);
      exit;
    end;

    //other conditions need word-char
    bWordChar:= IsCharWordInIdentifier(AText[1]);
    if not bWordChar then
    begin
      FTextCharsTyped:= 0;
      exit;
    end;

    SLexerName:= LexerNameAtPos(Ed, Point(Caret.PosX, Caret.PosY));

    //autoshow for HTML
    if UiOps.AutocompleteHtml and (Pos('HTML', SLexerName)>0) then
    begin
      if Ed.Strings.LineCharAt(Caret.PosY, Caret.PosX-1)='<' then
        Ed.DoCommand(cmd_AutoComplete);
      exit;
    end;

    (*
    //autoshow for CSS
    //seems bad, so commented, CSS must work like other lexers with AutoshowCharCount
    if UiOps.AutocompleteCss and (SLexerName='CSS') then
    begin
      if EditorIsAutocompleteCssPosition(Ed, Caret.PosX-1, Caret.PosY) then
        Ed.DoCommand(cmd_AutoComplete);
      exit;
    end;
    *)

    //autoshow for others, when typed N chars
    if (Ed.OptAutocompleteAutoshowCharCount>0) then
    begin
      //ignore if number typed
      bIdentChar:= bWordChar and not IsCharDigit(AText[1]);
      if (FTextCharsTyped=0) and (not bIdentChar) then exit;

      //check that we are not inside comment/string
      if IsCaretInsideCommentOrString(Ed, Caret.PosX, Caret.PosY) then exit;

      Inc(FTextCharsTyped);
      if FTextCharsTyped=Ed.OptAutocompleteAutoshowCharCount then
      begin
        FTextCharsTyped:= 0;
        Ed.DoCommand(cmd_AutoComplete);
        exit;
      end;
    end
    else
      FTextCharsTyped:= 0;
  end;

  if Ed.LastCommandChangedLines>0 then
    if Assigned(FOnMsgStatus) then
      FOnMsgStatus(Self, Format(msgStatusChangedLinesCount, [Ed.LastCommandChangedLines]));
end;

procedure TEditorFrame.EditorOnClickDouble(Sender: TObject; var AHandled: boolean);
var
  Params: TAppVariantArray;
  Res: TAppPyEventResult;
begin
  SetLength(Params, 1);
  Params[0]:= AppVariant(ConvertShiftStateToString(KeyboardStateToShiftState));

  Res:= DoPyEvent(Sender as TATSynEdit, cEventOnClickDbl, Params);
  AHandled:= Res.Val=evrFalse;
end;

procedure TEditorFrame.EditorOnClickLink(Sender: TObject; const ALink: string);
var
  Params: TAppVariantArray;
  Res: TAppPyEventResult;
  bHandled: boolean;
begin
  SetLength(Params, 2);
  Params[0]:= AppVariant(ConvertShiftStateToString(KeyboardStateToShiftState));
  Params[1]:= AppVariant(ALink);

  Res:= DoPyEvent(Sender as TATSynEdit, cEventOnClickLink, Params);
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
  Params: TAppVariantArray;
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

  SetLength(Params, 7);
  Params[0]:= AppVariant(ConvertShiftStateToString(KeyboardStateToShiftState));
  Params[1]:= AppVariant(AGapItem.LineIndex);
  Params[2]:= AppVariant(AGapItem.Tag);
  Params[3]:= AppVariant(W);
  Params[4]:= AppVariant(H);
  Params[5]:= AppVariant(APos.X);
  Params[6]:= AppVariant(APos.Y);

  DoPyEvent(Ed, cEventOnClickGap, Params);
end;


procedure TEditorFrame.DoOnResize;
begin
  inherited;

  //this keeps ratio of splitter (e.g. 50%) on form resize
  SplitPos:= SplitPos;
end;

procedure TEditorFrame.InitEditor(var ed: TATSynEdit);
begin
  ed:= TATSynEdit.Create(Self);
  ed.Parent:= Self;

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
  ed.OptScrollbarsNew:= true;

  SetLength(ed.Micromap.Columns, 2);
  ed.Micromap.Columns[1].NWidthPercents:= 40; //for marks from Spell Checker, Highlight Occur
  ed.Micromap.Columns[1].NTag:= 1;

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
  ed.OnChangeState:= @EditorOnChangeState;
  ed.OnChangeCaretPos:= @EditorOnChangeCaretPos;
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
end;

constructor TEditorFrame.Create(AOwner: TComponent; AApplyCentering: boolean);
begin
  inherited Create(AOwner);

  FFileName:= '';
  FFileName2:= '';
  FActiveSecondaryEd:= false;
  FTabColor:= clNone;
  Inc(FLastTabId);
  FTabId:= FLastTabId;
  FTabImageIndex:= -1;
  FInSession:= false;
  FEnabledCodeTree[0]:= true;
  FEnabledCodeTree[1]:= true;
  FSaveHistory:= true;
  FEditorsLinked:= true;
  FCodetreeFilterHistory:= TStringList.Create;
  FCachedTreeview[0]:= nil;
  FCachedTreeview[1]:= nil;

  FBracketHilite:= EditorOps.OpBracketHilite;
  FBracketSymbols:= EditorOps.OpBracketSymbols;
  FBracketMaxDistance:= EditorOps.OpBracketDistance;

  InitEditor(Ed1);
  InitEditor(Ed2);

  Ed1.Strings.GutterDecor1:= Ed1.GutterDecor;
  Ed1.Strings.GutterDecor2:= Ed2.GutterDecor;

  Ed2.Visible:= false;
  Splitter.Visible:= false;
  Ed1.Align:= alClient;
  Ed2.Align:= alBottom;

  Ed1.EditorIndex:= 0;
  Ed2.EditorIndex:= 1;

  Splitter.OnMoved:= @SplitterMoved;

  FSplitHorz:= true;
  Splitted:= false;

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

  Ed1.Strings.DoClearUndo;
  Ed1.Strings.EncodingDetectDefaultUtf8:= UiOps.DefaultEncUtf8;

  Ed1.EncodingName:= AppEncodingShortnameToFullname(UiOps.NewdocEnc);

  //must clear Modified, it was set on initing
  Ed1.Modified:= false;
  Ed2.Modified:= false;
end;

destructor TEditorFrame.Destroy;
var
  Params: TAppVariantArray;
begin
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

  if not Application.Terminated then //prevent crash on exit
  begin
    SetLength(Params, 0);
    DoPyEvent(Ed1, cEventOnClose, Params);
  end;

  FreeAndNil(FCodetreeFilterHistory);

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
    FCachedTreeview[N]:= TTreeView.Create(Self);
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

procedure TEditorFrame.ApplyTheme;
begin
  EditorApplyTheme(Ed1);
  EditorApplyTheme(Ed2);
  Ed1.Update;
  Ed2.Update;

  if Assigned(FBin) then
  begin
    ViewerApplyTheme(FBin);
    FBin.Redraw();
  end;
end;

function TEditorFrame.IsEditorFocused: boolean;
begin
  Result:= Ed1.Focused or Ed2.Focused;
end;

function TEditorFrame.IsText: boolean;
begin
  Result:=
    not IsPicture and
    not IsBinary;
end;

function TEditorFrame.IsPicture: boolean;
begin
  Result:= Assigned(FImageBox) and FImageBox.Visible;
end;

function TEditorFrame.IsBinary: boolean;
begin
  Result:= Assigned(FBin) and FBin.Visible;
end;


procedure TEditorFrame.SetLexer(Ed: TATSynEdit; an: TecSyntAnalyzer);
var
  an2: TecSyntAnalyzer;
  ada: TATAdapterEControl;
  Params: TAppVariantArray;
begin
  ada:= Adapter[Ed];
  if Assigned(ada.Lexer) then
    if not ada.Stop then exit;

  if IsFileTooBigForLexer(GetFileName(Ed)) then
  begin
    if EditorsLinked or (Ed=Ed1) then
      Adapter1.Lexer:= nil
    else
    if Assigned(Adapter2) then
      Adapter2.Lexer:= nil;
    exit;
  end;

  if AllowFrameParsing then
  begin
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
      TATAdapterEControl(Ed.AdapterForHilite).Lexer:= an;
      //if Assigned(an) then
      //  ShowMessage('lexer '+an.LexerName);
    end;
  end
  else
  begin
    LexerInitial[Ed]:= an;
    //support on_lexer
    SetLength(Params, 0);
    DoPyEvent(Ed, cEventOnLexer, Params);
  end;
end;

procedure TEditorFrame.SetLexerLite(Ed: TATSynEdit; an: TATLiteLexer);
begin
  Lexer[Ed]:= nil;

  Ed.InvalidateHilitingCache;
  Ed.AdapterForHilite:= an;
  Ed.Update;

  if (Ed=Ed1) and EditorsLinked then
  begin
    Ed2.InvalidateHilitingCache;
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
    FBin:= TATBinHex.Create(Self);
    FBin.Hide; //reduce flicker with initial size
    FBin.OnKeyDown:= @BinaryOnKeyDown;
    FBin.OnScroll:= @BinaryOnScroll;
    FBin.OnOptionsChange:= @BinaryOnScroll;
    FBin.OnSearchProgress:= @BinaryOnProgress;
    FBin.Parent:= Self;
    FBin.Align:= alClient;
    FBin.BorderStyle:= bsNone;
    FBin.TextGutter:= true;
    FBin.TextWidth:= UiOps.ViewerBinaryWidth;
    FBin.TextPopupCommands:= [vpCmdCopy, vpCmdCopyHex, vpCmdSelectAll];
    FBin.TextPopupCaption[vpCmdCopy]:= cStrMenuitemCopy;
    FBin.TextPopupCaption[vpCmdCopyHex]:= cStrMenuitemCopy+' (hex)';
    FBin.TextPopupCaption[vpCmdSelectAll]:= cStrMenuitemSelectAll;
    FBin.Show;
  end;

  if Assigned(FBinStream) then
    FreeAndNil(FBinStream);
  FBinStream:= TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);

  ViewerApplyTheme(FBin);
  FBin.Show;
  FBin.Mode:= AMode;
  FBin.OpenStream(FBinStream);

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
    FImageBox:= TATImageBox.Create(Self);
    FImageBox.Parent:= Self;
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
  DoOnUpdateStatus;
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
  AAllowLoadHistory, AAllowLexerDetect, AAllowErrorMsgBox: boolean;
  AOpenMode: TAppOpenMode);
begin
  NotifEnabled:= false; //for binary-viewer and pictures, NotifEnabled must be False
  FileProps.Inited:= false; //loading of new filename must not trigger notif-thread

  if Assigned(FBin) then
    FBin.Hide;
  if Assigned(FImageBox) then
    FImageBox.Hide;

  if not FileExists(AFileName) then exit;
  if (AFileName2<>'') then
    if not FileExists(AFileName2) then exit;

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

  DoFileOpen_Ex(Ed1, AFileName, AAllowLoadHistory, AAllowLoadHistory, AAllowLexerDetect, AAllowErrorMsgBox, false, AOpenMode);

  if AFileName2<>'' then
  begin
    EditorsLinked:= false;
    SplitHorz:= false;
    Splitted:= true;
    DoFileOpen_Ex(Ed2, AFileName2, AAllowLoadHistory, AAllowLoadHistory, AAllowLexerDetect, AAllowErrorMsgBox, false, AOpenMode);
  end;
end;

procedure TEditorFrame.DoFileOpen_Ex(Ed: TATSynEdit; const AFileName: string;
  AAllowLoadHistory, AAllowLoadHistoryEnc, AAllowLexerDetect,
  AAllowErrorMsgBox, AKeepScroll: boolean; AOpenMode: TAppOpenMode);
begin
  try
    if AKeepScroll then
      Ed.Strings.EncodingDetect:= false;
    Ed.LoadFromFile(AFileName, AKeepScroll);
    Ed.Strings.EncodingDetect:= true;
    SetFileName(Ed, AFileName);
    UpdateCaptionFromFilename;
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

  if AAllowLoadHistory then
  begin
    DoLoadUndo(Ed);
    DoLoadHistory(Ed, AAllowLoadHistoryEnc);
  end;

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
  attr: integer;
  PrevEnabled, bNameChanged: boolean;
  NameCounter: integer;
  SFileName, NameTemp, NameInitial: string;
  EventRes: TAppPyEventResult;
  Params: TAppVariantArray;
begin
  Result:= true;
  if not IsText then exit(true); //disable saving, but close

  SetLength(Params, 0);
  EventRes:= DoPyEvent(Ed, cEventOnSaveBefore, Params);
  if EventRes.Val=evrFalse then exit(true); //disable saving, but close

  DoHideNotificationPanel(EditorObjToIndex(Ed));

  SFileName:= GetFileName(Ed);
  bNameChanged:= false;

  if ASaveAs or (SFileName='') then
  begin
    An:= Lexer[Ed];
    if Assigned(An) then
    begin
      SaveDialog.DefaultExt:= DoGetLexerDefaultExt(An);
      SaveDialog.Filter:= DoGetLexerFileFilter(An, msgAllFiles);
    end
    else
    begin
      SaveDialog.DefaultExt:= '';
      SaveDialog.Filter:= '';
    end;

    if SFileName='' then
    begin
      NameInitial:= '';
      EventRes:= DoPyEvent(Ed, cEventOnSaveNaming, Params);
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
    SetFileName(Ed, SFileName);
    bNameChanged:= true;

    //remove read-only (it may be set for original file)
    ReadOnly[Ed]:= false;

    //add to recents new filename
    if Assigned(FOnAddRecent) then
      FOnAddRecent(Ed);
  end;

  PrevEnabled:= NotifEnabled;
  NotifEnabled:= false;

  while true do
  try
    AppFileAttrPrepare(SFileName, attr);
    Ed.BeginUpdate;
    try
      try
        Ed.SaveToFile(SFileName);
      except
        on E: EConvertError do
          begin
            NameTemp:= Ed.EncodingName;
            Ed.EncodingName:= cEncNameUtf8_NoBom;
            Ed.SaveToFile(SFileName);
            MsgBox(Format(msgCannotSaveFileWithEnc, [NameTemp]), MB_OK or MB_ICONWARNING);
          end
        else
          raise;
      end;
    finally
      Ed.EndUpdate;
    end;
    AppFileAttrRestore(SFileName, attr);
    Break;
  except
    if MsgBox(msgCannotSaveFile+#10+SFileName,
      MB_RETRYCANCEL or MB_ICONERROR) = IDCANCEL then
    begin
      Result:= false;
      Break;
    end;
  end;

  if bNameChanged then
    DoLexerFromFilename(Ed, GetFileName(Ed));

  if EditorsLinked or (Ed=Ed1) then
    AppGetFileProps(GetFileName(Ed), FileProps)
  else
    AppGetFileProps(GetFileName(Ed), FileProps2);

  NotifEnabled:= PrevEnabled or bNameChanged;

  if not TabCaptionFromApi then
    UpdateCaptionFromFilename;

  if Result then
  begin
    DoSaveUndo(Ed, SFileName);
    DoPyEvent(Ed, cEventOnSaveAfter, Params);
    if Assigned(FOnSaveFile) then
      FOnSaveFile(Self);
  end;
end;

procedure TEditorFrame.DoFileReload_DisableDetectEncoding(Ed: TATSynEdit);
var
  SFileName: string;
begin
  SFileName:= GetFileName(Ed);
  if SFileName='' then exit;
  if Ed.Modified then
    if MsgBox(
      Format(msgConfirmReopenModifiedTab, [ExtractFileName(SFileName)]),
      MB_OKCANCEL or MB_ICONWARNING
      ) <> ID_OK then exit;

  Ed.Strings.EncodingDetect:= false;
  Ed.Strings.LoadFromFile(SFileName);
  Ed.Strings.EncodingDetect:= true;
  UpdateEds(true);
end;

function TEditorFrame.DoFileReload(Ed: TATSynEdit): boolean;
var
  PrevCaretX, PrevCaretY: integer;
  PrevTail: boolean;
  Mode: TAppOpenMode;
  SFileName: string;
  Params: TAppVariantArray;
begin
  Result:= true;
  SFileName:= GetFileName(Ed);
  if SFileName='' then exit(false);

  if not FileExists(SFileName) then
  begin
    OnMsgStatus(Self, msgCannotFindFile+' '+ExtractFileName(SFileName));
    exit(false);
  end;

  DoHideNotificationPanel(EditorObjToIndex(Ed));

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
  if IsBinary then
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
    false{AllowLexerDetect},
    false{AllowMsgBox},
    true{KeepScroll},
    Mode);
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

  OnUpdateStatus(Self);

  SetLength(Params, 0);
  DoPyEvent(Ed, cEventOnChangeSlow, Params);
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
  Params: TAppVariantArray;
begin
  Ed:= Sender as TATSynEdit;

  SetLength(Params, 3);
  Params[0]:= AppVariant(ConvertShiftStateToString(KeyboardStateToShiftState));
  Params[1]:= AppVariant(ALine);
  Params[2]:= AppVariant(ABand);

  if DoPyEvent(Ed, cEventOnClickGutter, Params).Val = evrFalse then exit;

  if ABand=Ed.GutterBandBookmarks then
    ed.BookmarkToggleForLine(ALine, 1, '', bmadOption, true, 0);
end;

procedure TEditorFrame.EditorOnDrawBookmarkIcon(Sender: TObject; C: TCanvas; ALineNum: integer;
  const ARect: TRect);
var
  Ed: TATSynEdit;
  r: TRect;
  dx: integer;
  index, kind: integer;
begin
  r:= ARect;
  if r.Left>=r.Right then exit;

  Ed:= Sender as TATSynEdit;
  index:= Ed.Strings.Bookmarks.Find(ALineNum);
  if index<0 then exit;

  kind:= Ed.Strings.Bookmarks[index].Data.Kind;
  if kind<=1 then
  begin
    c.brush.color:= GetAppColor(apclEdBookmarkIcon);
    c.pen.color:= c.brush.color;
    inc(r.top, 1);
    inc(r.left, 4);
    dx:= r.Height div 2-1;
    c.Polygon([Point(r.left, r.top), Point(r.left+dx, r.top+dx), Point(r.left, r.top+2*dx)]);
  end
  else
  if (kind>=Low(AppBookmarkSetup)) and (kind<=High(AppBookmarkSetup)) then
  begin
    AppBookmarkImagelist.Draw(c, r.left, r.top,
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
  Index: integer;
begin
  Index:= EditorObjToIndex(Ed);
  if Index>=0 then
    Result:= FFileWasBig[Index]
  else
    Result:= false;
end;

function TEditorFrame.GetInitialLexer(Ed: TATSynEdit): TecSyntAnalyzer;
var
  Index: integer;
begin
  if EditorsLinked then
    Result:= FInitialLexer1
  else
  begin
    Index:= EditorObjToIndex(Ed);
    if Index=0 then
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

procedure TEditorFrame.DoRestoreFolding(Ed: TATSynEdit);
var
  S: string;
begin
  if FFoldTodo<>'' then
  begin
    S:= FFoldTodo;
    FFoldTodo:= '';
    EditorSetFoldString(Ed, S);
  end;

  if FTopLineTodo>0 then
  begin
    Ed.LineTop:= FTopLineTodo;
    FTopLineTodo:= 0;
  end;
end;

procedure TEditorFrame.DoMacroStart;
begin
  FMacroRecord:= true;
  FMacroString:= '';
end;

procedure TEditorFrame.DoMacroStop(ACancel: boolean);
begin
  FMacroRecord:= false;
  if ACancel then
    FMacroString:= '';
end;

procedure TEditorFrame.DoOnUpdateStatus;
begin
  if Assigned(FOnUpdateStatus) then
    FOnUpdateStatus(Self);
end;

procedure TEditorFrame.EditorClickMoveCaret(Sender: TObject; APrevPnt, ANewPnt: TPoint);
var
  Pnt: TPoint;
begin
  if MacroRecord then
  begin
    Pnt:= ConvertTwoPointsToDiffPoint(APrevPnt, ANewPnt);
    FMacroString+= Format('%d,%d,%d', [cmd_MouseClickNearCaret, Pnt.X, Pnt.Y])+#10;
  end;
end;

procedure TEditorFrame.EditorDrawMicromap(Sender: TObject; ACanvas: TCanvas; const ARect: TRect);
type
  TAppMicromapMark = (markColumn, markFull, markLeft, markRight);
const
  cTagOccurrences = 101; //see plugin Hilite Occurrences
  cTagSpellChecker = 105; //see plugin SpellChecker
  cTagColumnFullsized = -2;
var
  Ed: TATSynEdit;
  NWidthSmall: integer;
//
  function GetItemRect(AColumn, NLine1, NLine2: integer; APos: TAppMicromapMark): TRect; inline;
  begin
    Result:= Ed.RectMicromapMark(AColumn, NLine1, NLine2);
    OffsetRect(Result, -ARect.Left, -ARect.Top);
    case APos of
      markLeft:
        Result.Right:= Result.Left + NWidthSmall;
      markRight:
        Result.Left:= Result.Right - NWidthSmall;
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
  State: TATLineState;
  Mark: TATMarkerItem;
  XColor, XColorSelected, XColorOccur, XColorSpell: TBGRAPixel;
  NColor: TColor;
  R1: TRect;
  NLine1, NLine2, NIndex, i: integer;
  Obj: TATLinePartClass;
begin
  Ed:= Sender as TATSynEdit;
  St:= Ed.Strings;
  if St.Count=0 then exit;
  NWidthSmall:= EditorScale(EditorOps.OpMicromapWidthSmall);

  if FMicromapBmp=nil then
    FMicromapBmp:= TBGRABitmap.Create;
  FMicromapBmp.SetSize(ARect.Width, ARect.Height);

  XColor.FromColor(GetAppColor(apclEdMicromapBg));
  FMicromapBmp.Fill(XColor);

  //paint full-width area of current view
  R1:= GetItemRect(0, Ed.LineTop, Ed.LineBottom, markFull);

  XColor.FromColor(GetAppColor(apclEdMicromapViewBg));
  FMicromapBmp.FillRect(R1, XColor);

  XColorSelected.FromColor(Ed.Colors.TextSelBG);
  XColorOccur.FromColor(GetAppColor(apclEdMicromapOccur));
  XColorSpell.FromColor(GetAppColor(apclEdMicromapSpell));

  //paint line states
  for i:= 0 to St.Count-1 do
  begin
    State:= St.LinesState[i];
    case State of
      cLineStateNone: Continue;
      cLineStateAdded: XColor.FromColor(Ed.Colors.StateAdded);
      cLineStateChanged: XColor.FromColor(Ed.Colors.StateChanged);
      cLineStateSaved: XColor.FromColor(Ed.Colors.StateSaved);
      else Continue;
    end;
    FMicromapBmp.FillRect(GetItemRect(0, i, i, markLeft), XColor);
  end;

  //paint selections
  for i:= 0 to Ed.Carets.Count-1 do
  begin
    Caret:= Ed.Carets[i];
    Caret.GetSelLines(NLine1, NLine2, false);
    if NLine1<0 then Continue;
    R1:= GetItemRect(0, NLine1, NLine2, markRight);
    FMicromapBmp.FillRect(R1, XColorSelected);
  end;

  //paint background of columns
  for i:= 2{after default columns} to Length(Ed.Micromap.Columns)-1 do
  begin
    NColor:= Ed.Micromap.Columns[i].NColor;
    if NColor<>clNone then
    begin
      XColor.FromColor(NColor);
      R1:= Ed.RectMicromapMark(i, -1, -1);
      FMicromapBmp.FillRect(R1, XColor);
    end;
  end;

  //paint marks for plugins
  for i:= 0 to Ed.Attribs.Count-1 do
  begin
    Mark:= Ed.Attribs[i];
    Obj:= TATLinePartClass(Mark.Ptr);

    NLine1:= Mark.PosY;
    NLine2:= NLine1;
    //negative LenX means we need multiline mark, its height is abs(LenX)
    if Mark.SelX<0 then
      Inc(NLine2, -Mark.SelX-1);

    case Mark.Tag of
      cTagSpellChecker:
        begin
          R1:= GetItemRect(1{column-1}, NLine1, NLine2, markColumn);
          FMicromapBmp.FillRect(R1, XColorSpell);
        end;
      cTagOccurrences:
        begin
          R1:= GetItemRect(1{column-1}, NLine1, NLine2, markColumn);
          FMicromapBmp.FillRect(R1, XColorOccur);
        end;
      else
        begin
          if Obj.ColumnTag>0 then
          begin
            NIndex:= Ed.Micromap.ColumnFromTag(Obj.ColumnTag);
            if NIndex>=0 then
            begin
              XColor.FromColor(Obj.Data.ColorBG);
              R1:= GetItemRect(NIndex, NLine1, NLine2, markColumn);
              FMicromapBmp.FillRect(R1, XColor);
            end;
          end
          else
          if Obj.ColumnTag=cTagColumnFullsized then
          begin
            R1:= GetItemRect(0, NLine1, NLine2, markFull);
            //todo: not tested with BGRABitmap - it must give inverted colors
            XColor.FromColor(Obj.Data.ColorBG);
            FMicromapBmp.FillRect(R1, XColor, dmDrawWithTransparency, $8000);
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
    FMacroString+= Format('%d,%d,%d', [cmd_MouseClickNearCaretAndSelect, Pnt.X, Pnt.Y])+#10;
  end;
end;


procedure TEditorFrame.DoSaveHistory(Ed: TATSynEdit);
var
  cfg: TJSONConfig;
  SFileName: string;
  path: string;
  items: TStringlist;
begin
  if not FSaveHistory then exit;
  if UiOps.MaxHistoryFiles<2 then exit;

  SFileName:= GetFileName(Ed);
  if SFileName='' then exit;
  path:= SMaskFilenameSlashes(SFileName);

  cfg:= TJsonConfig.Create(nil);
  try
    try
      cfg.Formatted:= true;
      cfg.Filename:= AppFile_HistoryFiles;
    except
      MsgBadConfig(AppFile_HistoryFiles);
      exit
    end;

    items:= TStringList.Create;
    try
      cfg.DeletePath(path);
      cfg.EnumSubKeys('/', items);
      while items.Count>=UiOps.MaxHistoryFiles do
      begin
        cfg.DeletePath('/'+items[0]);
        items.Delete(0);
      end;
    finally
      FreeAndNil(items);
    end;

    DoSaveHistoryEx(Ed, cfg, path);
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

procedure TEditorFrame.DoSaveHistory_Bookmarks(Ed: TATSynEdit; c: TJsonConfig; const path: UnicodeString);
var
  items, items2: TStringList;
  bookmark: TATBookmarkItem;
  i: integer;
begin
  items:= TStringList.Create;
  items2:= TStringList.Create;
  try
    for i:= 0 to Ed.Strings.Bookmarks.Count-1 do
    begin
      bookmark:= Ed.Strings.Bookmarks[i];
      //save usual bookmarks and numbered bookmarks (kind=1..10)
      if (bookmark.Data.Kind>10) then Continue;
      items.Add(IntToStr(bookmark.Data.LineNum));
      items2.Add(IntToStr(bookmark.Data.Kind));
    end;

    if items.Count>0 then
      c.SetValue(path+cHistory_Bookmark, items)
    else
      c.DeleteValue(path+cHistory_Bookmark);

    if items2.Count>0 then
      c.SetValue(path+cHistory_BookmarkKind, items2)
    else
      c.DeleteValue(path+cHistory_BookmarkKind);

  finally
    FreeAndNil(items2);
    FreeAndNil(items);
  end;
end;

procedure TEditorFrame.DoSaveHistoryEx(Ed: TATSynEdit; c: TJsonConfig; const path: UnicodeString);
begin
  if UiOps.HistoryItems[ahhLexer] then
    c.SetDeleteValue(path+cHistory_Lexer, LexerName[Ed], '');

  if UiOps.HistoryItems[ahhEncoding] then
    c.SetValue(path+cHistory_Enc, Ed.EncodingName);

  if UiOps.HistoryItems[ahhTopLine] then
    c.SetDeleteValue(path+cHistory_TopLine, Ed.LineTop, 0);

  if UiOps.HistoryItems[ahhWordWrap] then
    c.SetDeleteValue(path+cHistory_Wrap, Ord(Ed.OptWrapMode), 0);

  if not Ed.IsReadOnlyAutodetected then
    c.SetDeleteValue(path+cHistory_ReadOnly, ReadOnly[Ed], false);

  if UiOps.HistoryItems[ahhRuler] then
    c.SetDeleteValue(path+cHistory_Ruler, Ed.OptRulerVisible, false);

  if UiOps.HistoryItems[ahhMinimap] then
    c.SetDeleteValue(path+cHistory_Minimap, Ed.OptMinimapVisible, false);

  if UiOps.HistoryItems[ahhMicromap] then
    c.SetDeleteValue(path+cHistory_Micromap, Ed.OptMicromapVisible, false);

  if UiOps.HistoryItems[ahhTabSize] then
  begin
    c.SetValue(path+cHistory_TabSize, Ed.OptTabSize);
    c.SetValue(path+cHistory_TabSpace, Ed.OptTabSpaces);
  end;

  if UiOps.HistoryItems[ahhUnprinted] then
  begin
    c.SetDeleteValue(path+cHistory_Unpri, Ed.OptUnprintedVisible, false);
    c.SetDeleteValue(path+cHistory_Unpri_Spaces, Ed.OptUnprintedSpaces, true);
    c.SetDeleteValue(path+cHistory_Unpri_Ends, Ed.OptUnprintedEnds, true);
    c.SetDeleteValue(path+cHistory_Unpri_Detail, Ed.OptUnprintedEndsDetails, false);
  end;

  if UiOps.HistoryItems[ahhLineNumbers] then
    c.SetDeleteValue(path+cHistory_LineNums, Ed.Gutter[Ed.GutterBandNumbers].Visible, true);

  if UiOps.HistoryItems[ahhScale] then
    c.SetDeleteValue(path+cHistory_FontScale, Ed.OptScaleFont, 0);

  if UiOps.HistoryItems[ahhFolding] then
    c.SetDeleteValue(path+cHistory_Fold, EditorGetFoldString(Ed), '');

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
    c.SetDeleteValue(path+cHistory_Markers, Ed.Markers.AsString, '');
  }

  if UiOps.HistoryItems[ahhBookmarks] then
    DoSaveHistory_Bookmarks(Ed, c, path);

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
    _WriteStringToFileInHiddenDir(GetAppUndoFilename(AFileName, false), Ed.UndoAsString);
    _WriteStringToFileInHiddenDir(GetAppUndoFilename(AFileName, true), Ed.RedoAsString);
  end;
end;

procedure TEditorFrame.DoLoadHistory(Ed: TATSynEdit; AllowEnc: boolean);
var
  cfg: TJSONConfig;
  SFileName: string;
  path: string;
begin
  SFileName:= GetFileName(Ed);
  if SFileName='' then exit;
  path:= SMaskFilenameSlashes(SFileName);

  if UiOps.MaxHistoryFiles<2 then exit;

  cfg:= TJsonConfig.Create(nil);
  try
    try
      cfg.Formatted:= true;
      cfg.Filename:= AppFile_HistoryFiles;
    except
      MsgBadConfig(AppFile_HistoryFiles);
      exit
    end;

    DoLoadHistoryEx(Ed, cfg, path, AllowEnc);
  finally
    cfg.Free;
  end;
end;


procedure TEditorFrame.DoLoadHistoryEx(Ed: TATSynEdit; c: TJsonConfig;
  const path: UnicodeString; AllowEnc: boolean);
var
  str, str0, sFileName, sCarets: string;
  Caret: TATCaretItem;
  NPosX, NPosY, NEndX, NEndY: integer;
  nTop, nKind, i: integer;
  items, items2: TStringlist;
  BmData: TATBookmarkData;
  Sep: TATStringSeparator;
begin
  sFileName:= GetFileName(Ed);

  FillChar(BmData, SizeOf(BmData), 0);
  BmData.ShowInBookmarkList:= true;

  //file not listed in history file?
  sCarets:= c.GetValue(path+cHistory_Caret, '');
  FInHistory:= sCarets<>'';
  if not FInHistory then exit;

  {
  //markers
  //it's bad to save markers along with other history items,
  //so this is commented
  Ed.Markers.AsString:= c.GetValue(path+cHistory_Markers, '');
  }

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
    Ed.OptWrapMode:= TATSynWrapMode(c.GetValue(path+cHistory_Wrap, Ord(Ed.OptWrapMode)));
    Ed.OptMinimapVisible:= c.GetValue(path+cHistory_Minimap, Ed.OptMinimapVisible);
    Ed.OptMicromapVisible:= c.GetValue(path+cHistory_Micromap, Ed.OptMicromapVisible);
  end;
  Ed.OptRulerVisible:= c.GetValue(path+cHistory_Ruler, Ed.OptRulerVisible);
  Ed.OptTabSize:= c.GetValue(path+cHistory_TabSize, Ed.OptTabSize);
  Ed.OptTabSpaces:= c.GetValue(path+cHistory_TabSpace, Ed.OptTabSpaces);
  Ed.OptUnprintedVisible:= c.GetValue(path+cHistory_Unpri, Ed.OptUnprintedVisible);
  Ed.OptUnprintedSpaces:= c.GetValue(path+cHistory_Unpri_Spaces, Ed.OptUnprintedSpaces);
  Ed.OptUnprintedEnds:= c.GetValue(path+cHistory_Unpri_Ends, Ed.OptUnprintedEnds);
  Ed.OptUnprintedEndsDetails:= c.GetValue(path+cHistory_Unpri_Detail, Ed.OptUnprintedEndsDetails);

  with Ed.Gutter[Ed.GutterBandNumbers] do
    Visible:= c.GetValue(path+cHistory_LineNums, Visible);

  Ed.OptScaleFont:= c.GetValue(path+cHistory_FontScale, 0);

  nTop:= c.GetValue(path+cHistory_TopLine, 0);
  if Assigned(Lexer[Ed]) then
  begin
    FFoldTodo:= c.GetValue(path+cHistory_Fold, '');
    if nTop>0 then
    begin
      FTopLineTodo:= nTop; //restore LineTop after parsing done
      Ed.LineTop:= nTop; //scroll immediately
    end;
  end
  else
  begin
    //now it's not needed to do Ed.Update(true) nor Application.ProcessMessages
    if nTop>0 then
      Ed.LineTop:= nTop;
  end;

  //caret
  if Ed.Carets.Count>0 then
  begin
    Sep.Init(sCarets);
    Sep.GetItemInt(NPosX, 0);
    Sep.GetItemInt(NPosY, 0);
    Sep.GetItemInt(NEndX, -1);
    Sep.GetItemInt(NEndY, -1);
    caret:= Ed.Carets[0];
    if caret.Change(NPosX, NPosY, NEndX, NEndY) then
    begin
      Ed.DoCaretsFixIncorrectPos(true);
      Ed.DoEventCarets;
    end;
  end;

  //bookmarks
  items:= TStringList.create;
  items2:= TStringList.create;
  try
    c.GetValue(path+cHistory_Bookmark, items, '');
    c.GetValue(path+cHistory_BookmarkKind, items2, '');
    for i:= 0 to items.Count-1 do
    begin
      nTop:= StrToIntDef(items[i], -1);
      if i<items2.Count then
        nKind:= StrToIntDef(items2[i], 1)
      else
        nKind:= 1;
      if Ed.Strings.IsIndexValid(nTop) then
      begin
        BmData.LineNum:= nTop;
        BmData.Kind:= nKind;
        BmData.AutoDelete:= bmadOption;
        Ed.Strings.Bookmarks.Add(BmData);
      end;
    end;
  finally
    FreeAndNil(items2);
    FreeAndNil(items);
  end;

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
    STemp:= GetAppUndoFilename(SFileName, false);
    if FileExists(STemp) then
      Ed.UndoAsString:= DoReadContentFromFile(STemp);

    STemp:= GetAppUndoFilename(SFileName, true);
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

procedure TEditorFrame.InitPanelReload(Index: integer);
var
  NPanelHeight, NBtnHeight: integer;
begin
  if Assigned(PanelReload[Index]) then exit;

  NPanelHeight:= AppScale(31);
  NBtnHeight:= AppScale(25);

  PanelReload[Index]:= TPanel.Create(Self);
  PanelReload[Index].Parent:= Self;
  PanelReload[Index].Align:= alTop;
  PanelReload[Index].Visible:= false;
  PanelReload[Index].Height:= NPanelHeight;
  PanelReload[Index].BevelOuter:= bvNone;

  LabelReload[Index]:= TLabel.Create(Self);
  LabelReload[Index].Parent:= PanelReload[Index];
  LabelReload[Index].BorderSpacing.Left:= 4;
  LabelReload[Index].ParentColor:= false;
  LabelReload[Index].AnchorSideLeft.Control:= PanelReload[Index];
  LabelReload[Index].AnchorSideTop.Control:= PanelReload[Index];
  LabelReload[Index].AnchorSideTop.Side:= asrCenter;

  btnReloadNone[Index]:= TATButton.Create(Self);
  btnReloadNone[Index].Tag:= Index;
  btnReloadNone[Index].Parent:= PanelReload[Index];
  btnReloadNone[Index].AnchorSideTop.Control:= PanelReload[Index];
  btnReloadNone[Index].AnchorSideTop.Side:= asrCenter;
  btnReloadNone[Index].AnchorSideRight.Control:= PanelReload[Index];
  btnReloadNone[Index].AnchorSideRight.Side:= asrBottom;
  btnReloadNone[Index].Anchors:= [akTop, akRight];
  btnReloadNone[Index].Height:= NBtnHeight;
  btnReloadNone[Index].BorderSpacing.Right:= 4;
  btnReloadNone[Index].OnClick:= @btnReloadNoneClick;

  btnReloadNo[Index]:= TATButton.Create(Self);
  btnReloadNo[Index].Tag:= Index;
  btnReloadNo[Index].Parent:= PanelReload[Index];
  btnReloadNo[Index].AnchorSideTop.Control:= btnReloadNone[Index];
  btnReloadNo[Index].AnchorSideRight.Control:= btnReloadNone[Index];
  btnReloadNo[Index].Anchors:= [akTop, akRight];
  btnReloadNo[Index].Height:= NBtnHeight;
  btnReloadNo[Index].BorderSpacing.Right:= 0;
  btnReloadNo[Index].OnClick:= @btnReloadNoClick;

  btnReloadYes[Index]:= TATButton.Create(Self);
  btnReloadYes[Index].Tag:= Index;
  btnReloadYes[Index].Parent:= PanelReload[Index];
  btnReloadYes[Index].AnchorSideTop.Control:= btnReloadNone[Index];
  btnReloadYes[Index].AnchorSideRight.Control:= btnReloadNo[Index];
  btnReloadYes[Index].Anchors:= [akTop, akRight];
  btnReloadYes[Index].Height:= NBtnHeight;
  btnReloadYes[Index].BorderSpacing.Right:= 0;
  btnReloadYes[Index].OnClick:= @btnReloadYesClick;

  btnReloadYes[Index].TabOrder:= 0;
  btnReloadNo[Index].TabOrder:= 1;
  btnReloadNone[Index].TabOrder:= 2;
end;

procedure TEditorFrame.NotifyAboutChange(Ed: TATSynEdit);
var
  bMsg: boolean;
  Index: integer;
begin
  case UiOps.NotificationConfirmReload of
    1:
      bMsg:= Ed.Modified or not Ed.Strings.UndoEmpty;
    2:
      bMsg:= Ed.Modified; //like Notepad++
    else
      bMsg:= true;
  end;

  if not bMsg then
  begin
    DoFileReload(Ed);
    exit
  end;

  Index:= EditorObjToIndex(Ed);
  if Index<0 then exit;
  InitPanelReload(Index);

  PanelReload[Index].Color:= GetAppColor(apclListBg);
  PanelReload[Index].Font.Name:= UiOps.VarFontName;
  PanelReload[Index].Font.Size:= AppScaleFont(UiOps.VarFontSize);
  PanelReload[Index].Font.Color:= GetAppColor(apclListFont);

  btnReloadYes[Index].Caption:= msgConfirmReloadYes;
  btnReloadNo[Index].Caption:= msgButtonCancel;
  btnReloadNone[Index].Caption:= msgConfirmReloadNoMore;

  btnReloadYes[Index].AutoSize:= true;
  btnReloadNo[Index].AutoSize:= true;
  btnReloadNone[Index].AutoSize:= true;

  LabelReload[Index].Caption:= msgConfirmFileChangedOutside+' '+ExtractFileName(GetFileName(Ed));
  PanelReload[Index].Show;
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
      ClearTreeviewWithData(FCachedTreeview[N]);
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
  TopLineTodo:= APosY; //check is it still needed
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
begin
  if AFileName='' then exit;
  DoLexerDetect(AFileName, TempLexer, TempLexerLite, SName, FLexerChooseFunc);
  if Assigned(TempLexer) then
    Lexer[Ed]:= TempLexer
  else
  if Assigned(TempLexerLite) then
    LexerLite[Ed]:= TempLexerLite;
end;

procedure TEditorFrame.SetFocus;
begin
  DoOnChangeCaption;
  DoShow;

  if Visible and Enabled then
  begin
    if IsText then
    begin
      if Editor.Visible and Editor.Enabled then
        EditorFocus(Editor);
    end
    else
    if IsBinary then
    begin
      if Assigned(FBin) and FBin.Visible and FBin.CanFocus then
        EditorFocus(FBin);
    end
    else
    if IsPicture then
    begin
      if Assigned(FImageBox) and FImageBox.Visible and FImageBox.CanFocus then
        FImageBox.SetFocus;
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
  DoOnUpdateStatus;
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
  for i:= Low(PanelReload) to High(PanelReload) do
    if Assigned(PanelReload[i]) then
      if PanelReload[i].Visible then
        btnReloadYes[i].SetFocus;
end;

procedure TEditorFrame.DoHideNotificationPanel(Index: integer);
begin
  if Index<0 then exit;
  if Assigned(PanelReload[Index]) then
    if PanelReload[Index].Visible then
    begin
      if Visible then
        if btnReloadYes[Index].Focused or
           btnReloadNo[Index].Focused or
           btnReloadNone[Index].Focused then
          EditorFocus(EditorIndexToObj(Index));
      PanelReload[Index].Hide;
    end;
end;

procedure TEditorFrame.DoHideNotificationPanels;
var
  i: integer;
begin
  for i:= Low(PanelReload) to High(PanelReload) do
    DoHideNotificationPanel(i);
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


function TEditorFrame.IsCaretInsideCommentOrString(Ed: TATSynEdit; AX, AY: integer): boolean;
var
  Kind: TATTokenKind;
begin
  Kind:= EditorGetTokenKind(Ed, AX, AY);
  Result:= (Kind=atkComment) or (Kind=atkString);
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

function TEditorFrame.Modified: boolean;
begin
  if FEditorsLinked then
    Result:= EditorIsModifiedEx(Ed1)
  else
    Result:= EditorIsModifiedEx(Ed1) or EditorIsModifiedEx(Ed2);
end;

end.

