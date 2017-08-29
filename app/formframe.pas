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
  ExtCtrls, Menus, StdCtrls, StrUtils,
  LCLIntf, LCLProc, LCLType, LazUTF8, LazFileUtils, FileUtil,
  ATTabs,
  ATGroups,
  ATSynEdit,
  ATSynEdit_Keymap_Init,
  ATSynEdit_Adapter_EControl,
  ATSynEdit_Carets,
  ATSynEdit_Gaps,
  ATSynEdit_Markers,
  ATSynEdit_CanvasProc,
  ATSynEdit_Commands,
  ATStrings,
  ATStringProc,
  ATStringProc_HtmlColor,
  ATFileNotif,
  ATButtons,
  ATPanelSimple,
  ecSyntAnal,
  proc_globdata,
  proc_lexer,
  proc_editor,
  proc_cmd,
  proc_colors,
  proc_files,
  proc_msg,
  proc_str,
  proc_py,
  proc_py_const,
  proc_miscutils,
  formlexerstylemap,
  jsonConf,
  math;

type
  TEditorFramePyEvent = function(AEd: TATSynEdit; AEvent: TAppPyEvent; const AParams: array of string): string of object;


type
  { TEditorFrame }

  TEditorFrame = class(TFrame)
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    Splitter: TSplitter;
    TimerChange: TTimer;
    procedure FrameResize(Sender: TObject);
    procedure SplitterMoved(Sender: TObject);
    procedure TimerChangeTimer(Sender: TObject);
  private
    { private declarations }
    Ed1, Ed2: TATSynEdit;
    FTabCaption: string;
    FTabCaptionFromApi: boolean;
    FTabImageIndex: integer;
    FTabId: integer;
    FFileName: string;
    FFileWasBig: boolean;
    FModified: boolean;
    FNotif: TATFileNotif;
    FTextCharsTyped: integer;
    FOnChangeCaption: TNotifyEvent;
    FOnUpdateStatus: TNotifyEvent;
    FOnEditorClickMoveCaret: TATSynEditClickMoveCaretEvent;
    FOnEditorClickEndSelect: TATSynEditClickMoveCaretEvent;
    FOnFocusEditor: TNotifyEvent;
    FOnEditorCommand: TATSynEditCommandEvent;
    FOnEditorChangeCaretPos: TNotifyEvent;
    FOnSaveFile: TNotifyEvent;
    FOnAddRecent: TNotifyEvent;
    FOnPyEvent: TEditorFramePyEvent;
    FSplitted: boolean;
    FSplitHorz: boolean;
    FSplitPos: double;
    FActiveAlt: boolean;
    FLocked: boolean;
    FTabColor: TColor;
    FTabSizeChanged: boolean;
    FFoldTodo: string;
    FTopLineTodo: integer;
    FTabKeyCollectMarkers: boolean;
    FTagString: string;
    FNotInRecents: boolean;
    FMacroRecord: boolean;
    FMacroString: string;
    FImage: TImage;
    FImagePanel: TATPanelSimple;
    FImageFilename: string;
    FCheckFilenameOpened: TStrFunction;
    FSaveDialog: TSaveDialog;

    procedure DoFileOpen_AsPicture(const fn: string);
    procedure DoImagePanelPaint(Sender: TObject);
    procedure DoOnChangeCaption;
    procedure DoOnChangeCaretPos;
    procedure DoOnUpdateStatus;
    procedure EditorClickEndSelect(Sender: TObject; APrevPnt, ANewPnt: TPoint);
    procedure EditorClickMoveCaret(Sender: TObject; APrevPnt, ANewPnt: TPoint);
    procedure EditorDrawMicromap(Sender: TObject; C: TCanvas; const ARect: TRect);
    procedure EditorOnChangeCommon(Sender: TObject);
    procedure EditorOnChange1(Sender: TObject);
    procedure EditorOnChange2(Sender: TObject);
    procedure EditorOnClick(Sender: TObject);
    procedure EditorOnClickGap(Sender: TObject; AGapItem: TATSynGapItem; APos: TPoint);
    procedure EditorOnClickGutter(Sender: TObject; ABand, ALine: integer);
    procedure EditorOnClickDouble(Sender: TObject; var AHandled: boolean);
    procedure EditorOnClickMicroMap(Sender: TObject; AX, AY: integer);
    procedure EditorOnClickMiddle(Sender: TObject; var AHandled: boolean);
    procedure EditorOnCommand(Sender: TObject; ACmd: integer; const AText: string; var AHandled: boolean);
    procedure EditorOnCommandAfter(Sender: TObject; ACommand: integer; const AText: string);
    procedure EditorOnDrawBookmarkIcon(Sender: TObject; C: TCanvas; ALineNum: integer; const ARect: TRect);
    procedure EditorOnEnter(Sender: TObject);
    procedure EditorOnDrawLine(Sender: TObject; C: TCanvas; AX, AY: integer;
      const AStr: atString; ACharSize: TPoint; const AExtent: TATIntArray);
    procedure EditorOnCalcBookmarkColor(Sender: TObject; ABookmarkKind: integer; out AColor: TColor);
    procedure EditorOnChangeCaretPos(Sender: TObject);
    procedure EditorOnKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure EditorOnPaste(Sender: TObject; var AHandled: boolean; AKeepCaret,
      ASelectThen: boolean);
    function GetCommentString: string;
    function GetEnabledFolding: boolean;
    function GetEncodingName: string;
    function GetLineEnds: TATLineEnds;
    function GetNotifEnabled: boolean;
    function GetNotifTime: integer;
    function GetReadOnly: boolean;
    function GetTabKeyCollectMarkers: boolean;
    function GetUnprintedEnds: boolean;
    function GetUnprintedEndsDetails: boolean;
    function GetUnprintedShow: boolean;
    function GetUnprintedSpaces: boolean;
    procedure InitEditor(var ed: TATSynEdit);
    procedure NotifChanged(Sender: TObject);
    procedure SetEnabledFolding(AValue: boolean);
    procedure SetEncodingName(const Str: string);
    procedure SetFileName(const AValue: string);
    procedure SetFileWasBig(AValue: boolean);
    procedure SetLocked(AValue: boolean);
    procedure SetNotifEnabled(AValue: boolean);
    procedure SetNotifTime(AValue: integer);
    procedure SetReadOnly(AValue: boolean);
    procedure SetTabColor(AColor: TColor);
    procedure SetTabImageIndex(AValue: integer);
    procedure SetUnprintedEnds(AValue: boolean);
    procedure SetUnprintedEndsDetails(AValue: boolean);
    procedure SetUnprintedShow(AValue: boolean);
    procedure SetSplitHorz(AValue: boolean);
    procedure SetSplitPos(AValue: double);
    procedure SetSplitted(AValue: boolean);
    procedure SetTabCaption(const AValue: string);
    procedure SetLineEnds(Value: TATLineEnds);
    procedure SetUnprintedSpaces(AValue: boolean);
    procedure UpdateEds(AUpdateWrapInfo: boolean=false);
    function GetLexer: TecSyntAnalyzer;
    procedure SetLexer(an: TecSyntAnalyzer);
  protected
    procedure DoOnResize; override;
  public
    { public declarations }
    Adapter: TATAdapterEControl;
    Groups: TATGroups;
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    function Editor: TATSynEdit;
    function Editor2: TATSynEdit;
    procedure EditorOnKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    property ReadOnly: boolean read GetReadOnly write SetReadOnly;
    property FileName: string read FFileName write SetFileName;
    property FileWasBig: boolean read FFileWasBig write SetFileWasBig;
    property TabCaption: string read FTabCaption write SetTabCaption;
    property TabImageIndex: integer read FTabImageIndex write SetTabImageIndex;
    property TabCaptionFromApi: boolean read FTabCaptionFromApi write FTabCaptionFromApi;
    property TabId: integer read FTabId;
    property Modified: boolean read FModified;
    procedure UpdateModifiedState;
    procedure UpdateReadOnlyFromFile;
    property NotifEnabled: boolean read GetNotifEnabled write SetNotifEnabled;
    property NotifTime: integer read GetNotifTime write SetNotifTime;
    property Lexer: TecSyntAnalyzer read GetLexer write SetLexer;
    function LexerName: string;
    function LexerNameAtPos(Pnt: TPoint): string;
    property Locked: boolean read FLocked write SetLocked;
    property CommentString: string read GetCommentString;
    property TabColor: TColor read FTabColor write SetTabColor;
    property TabSizeChanged: boolean read FTabSizeChanged write FTabSizeChanged;
    property TabKeyCollectMarkers: boolean read GetTabKeyCollectMarkers write FTabKeyCollectMarkers;
    property TagString: string read FTagString write FTagString;
    property NotInRecents: boolean read FNotInRecents write FNotInRecents;
    property TopLineTodo: integer read FTopLineTodo write FTopLineTodo; //always use it instead of Ed.LineTop
    property TextCharsTyped: integer read FTextCharsTyped write FTextCharsTyped;
    function IsEmpty: boolean;
    //picture support
    function IsText: boolean;
    property PictureFileName: string read FImageFilename;
    function PictureSizes: TPoint;
    //
    property LineEnds: TATLineEnds read GetLineEnds write SetLineEnds;
    property EncodingName: string read GetEncodingName write SetEncodingName;
    property UnprintedShow: boolean read GetUnprintedShow write SetUnprintedShow;
    property UnprintedSpaces: boolean read GetUnprintedSpaces write SetUnprintedSpaces;
    property UnprintedEnds: boolean read GetUnprintedEnds write SetUnprintedEnds;
    property UnprintedEndsDetails: boolean read GetUnprintedEndsDetails write SetUnprintedEndsDetails;
    property Splitted: boolean read FSplitted write SetSplitted;
    property SplitHorz: boolean read FSplitHorz write SetSplitHorz;
    property SplitPos: double read FSplitPos write SetSplitPos;
    property EnabledFolding: boolean read GetEnabledFolding write SetEnabledFolding;
    property SaveDialog: TSaveDialog read FSaveDialog write FSaveDialog;
    //file
    procedure DoFileOpen(const fn: string; AAllowLoadHistory, AAllowErrorMsgBox: boolean);
    function DoFileSave(ASaveAs: boolean): boolean;
    procedure DoFileReload_DisableDetectEncoding;
    procedure DoFileReload;
    procedure DoSaveHistory;
    procedure DoSaveHistoryEx(c: TJsonConfig; const path: string);
    procedure DoLoadHistory;
    procedure DoLoadHistoryEx(c: TJsonConfig; const path: string);
    //misc
    function DoPyEvent(AEd: TATSynEdit; AEvent: TAppPyEvent; const AParams: array of string): string;
    procedure DoGotoPos(APosX, APosY: integer);
    procedure DoRestoreFolding;
    //macro
    procedure DoMacroStart;
    procedure DoMacroStop(ACancel: boolean);
    property MacroRecord: boolean read FMacroRecord;
    property MacroString: string read FMacroString write FMacroString;

    //events
    property OnCheckFilenameOpened: TStrFunction read FCheckFilenameOpened write FCheckFilenameOpened;
    property OnFocusEditor: TNotifyEvent read FOnFocusEditor write FOnFocusEditor;
    property OnChangeCaption: TNotifyEvent read FOnChangeCaption write FOnChangeCaption;
    property OnUpdateStatus: TNotifyEvent read FOnUpdateStatus write FOnUpdateStatus;
    property OnEditorClickMoveCaret: TATSynEditClickMoveCaretEvent read FOnEditorClickMoveCaret write FOnEditorClickMoveCaret;
    property OnEditorClickEndSelect: TATSynEditClickMoveCaretEvent read FOnEditorClickEndSelect write FOnEditorClickEndSelect;
    property OnEditorCommand: TATSynEditCommandEvent read FOnEditorCommand write FOnEditorCommand;
    property OnEditorChangeCaretPos: TNotifyEvent read FOnEditorChangeCaretPos write FOnEditorChangeCaretPos;
    property OnSaveFile: TNotifyEvent read FOnSaveFile write FOnSaveFile;
    property OnAddRecent: TNotifyEvent read FOnAddRecent write FOnAddRecent;
    property OnPyEvent: TEditorFramePyEvent read FOnPyEvent write FOnPyEvent;
  end;

implementation

{$R *.lfm}

const
  cHistory_Lexer       = '/lexer';
  cHistory_Enc         = '/enc';
  cHistory_Top         = '/top';
  cHistory_Wrap        = '/wrap_mode';
  cHistory_RO          = '/ro';
  cHistory_Ruler       = '/ruler';
  cHistory_Minimap     = '/minimap';
  cHistory_Micromap    = '/micromap';
  cHistory_TabSize     = '/tab_size';
  cHistory_TabSpace    = '/tab_spaces';
  cHistory_Nums        = '/nums';
  cHistory_Unpri        = '/unprinted_show';
  cHistory_Unpri_Spaces = '/unprinted_spaces';
  cHistory_Unpri_Ends   = '/unprinted_ends';
  cHistory_Unpri_Detail = '/unprinted_end_details';
  cHistory_Caret       = '/caret';
  cHistory_TabColor    = '/color';
  cHistory_Bookmark    = '/bm';
  cHistory_Fold        = '/folded';

var
  FLastTabId: integer = 0;

{ TEditorFrame }

procedure TEditorFrame.SetTabCaption(const AValue: string);
var
  Upd: boolean;
begin
  if AValue='?' then Exit;
  Upd:= FTabCaption<>AValue;

  FTabCaption:= AValue; //don't check Upd here (for Win32)

  if Upd then
    DoPyEvent(Editor, cEventOnState, [IntToStr(EDSTATE_TAB_TITLE)]);
  DoOnChangeCaption;
end;

procedure TEditorFrame.EditorOnClick(Sender: TObject);
var
  NewAlt: boolean;
  State: TShiftState;
  StateString: string;
begin
  NewAlt:= Sender=Ed2;
  if NewAlt<>FActiveAlt then
  begin
    FActiveAlt:= NewAlt;
    DoOnUpdateStatus;
  end;

  State:= KeyboardStateToShiftState;
  StateString:= ConvertShiftStateToString(State);

  if UiOps.MouseGotoDefinition<>'' then
    if StateString=UiOps.MouseGotoDefinition then
    begin
      DoPyEvent(Sender as TATSynEdit, cEventOnGotoDef, []);
      exit;
    end;

  DoPyEvent(Sender as TATSynEdit, cEventOnClick, ['"'+StateString+'"']);
end;

procedure TEditorFrame.SplitterMoved(Sender: TObject);
begin
  if FSplitted then
    if FSplitHorz then
      FSplitPos:= Ed2.height/height
    else
      FSplitPos:= Ed2.width/width;
end;

procedure TEditorFrame.FrameResize(Sender: TObject);
var
  R: TRect;
begin
  if Assigned(FImage) and Assigned(FImage.Picture) then
  begin
    R:= Rect(0, 0, FImage.Picture.Width, FImage.Picture.Height);
    if R.Right<ClientWidth then
      R.Left:= (ClientWidth-R.Right) div 2;
    if R.Bottom<ClientHeight then
      R.Top:= (ClientHeight-R.Bottom) div 2;
    FImagePanel.Left:= R.Left;
    FImagePanel.Top:= R.Top;
    FImagePanel.Width:= FImage.Picture.Width;
    FImagePanel.Height:= FImage.Picture.Height;
  end;
end;

procedure TEditorFrame.EditorOnKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  //res=False: block key
  if DoPyEvent(Sender as TATSynEdit,
    cEventOnKey,
    [
      IntToStr(Key),
      '"'+ConvertShiftStateToString(Shift)+'"'
    ]) = cPyFalse then
    begin
      Key:= 0;
      Exit
    end;
end;

procedure TEditorFrame.EditorOnPaste(Sender: TObject; var AHandled: boolean;
  AKeepCaret, ASelectThen: boolean);
const
  cBool: array[boolean] of string = (cPyFalse, cPyTrue);
begin
  if DoPyEvent(Sender as TATSynEdit,
    cEventOnPaste,
    [
      cBool[AKeepCaret],
      cBool[ASelectThen]
    ]) = cPyFalse then
    AHandled:= true;
end;

procedure TEditorFrame.EditorOnKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  //keyup: only for Ctrl/Shift/Alt
  //no res.
  case Key of
    VK_CONTROL,
    VK_MENU,
    VK_SHIFT,
    VK_RSHIFT:
      DoPyEvent(Sender as TATSynEdit,
        cEventOnKeyUp,
        [
          IntToStr(Key),
          '"'+ConvertShiftStateToString(Shift)+'"'
        ]);
  end;
end;


procedure TEditorFrame.TimerChangeTimer(Sender: TObject);
begin
  TimerChange.Enabled:= false;
  DoPyEvent(Editor, cEventOnChangeSlow, []);
end;

procedure TEditorFrame.EditorOnCalcBookmarkColor(Sender: TObject;
  ABookmarkKind: integer; out AColor: TColor);
begin
  if ABookmarkKind<=1 then
    AColor:= (Sender as TATSynEdit).Colors.BookmarkBG
  else
    AColor:= AppBookmarkSetup[ABookmarkKind].Color;
end;

procedure TEditorFrame.EditorOnChangeCaretPos(Sender: TObject);
begin
  DoOnChangeCaretPos;
  DoOnUpdateStatus;
  DoPyEvent(Sender as TATSynEdit, cEventOnCaret, []);
end;

procedure TEditorFrame.EditorOnDrawLine(Sender: TObject; C: TCanvas; AX,
  AY: integer; const AStr: atString; ACharSize: TPoint;
  const AExtent: TATIntArray);
var
  X1, X2, Y, NLen: integer;
  NColor: TColor;
  i: integer;
begin
  if AStr='' then Exit;
  if not IsFilenameListedInExtensionList(FileName, EditorOps.OpUnderlineColorFiles)
    then exit;

  for i:= 1 to Length(AStr) do
    if AStr[i]='#' then
    begin
      NColor:= SHtmlColorToColor(Copy(AStr, i+1, 7), NLen, clNone);
      if NColor=clNone then Continue;

      if i-2>=0 then
        X1:= AX+AExtent[i-2]
      else
        X1:= AX;
      X2:= AX+AExtent[i-1+NLen];
      Y:= AY+ACharSize.Y;

      C.Brush.Color:= NColor;
      C.FillRect(X1, Y-EditorOps.OpUnderlineColorSize, X2, Y);
    end;
end;

function TEditorFrame.GetEncodingName: string;
begin
  case Editor.Strings.Encoding of
    cEncAnsi:
      begin
        Result:= Editor.Strings.EncodingCodepage;
        if Result='' then Result:= cEncNameAnsi;
      end;
    cEncUTF8:
      begin
        if Editor.Strings.SaveSignUtf8 then
          Result:= cEncNameUtf8_WithBom
        else
          Result:= cEncNameUtf8_NoBom;
      end;
    cEncWideLE:
      begin
        if Editor.Strings.SaveSignWide then
          Result:= cEncNameUtf16LE_WithBom
        else
          Result:= cEncNameUtf16LE_NoBom;
      end;
    cEncWideBE:
      begin
        if Editor.Strings.SaveSignWide then
          Result:= cEncNameUtf16BE_WithBom
        else
          Result:= cEncNameUtf16BE_NoBom;
      end;
    else
      Result:= '?';
  end;
end;

function TEditorFrame.GetLineEnds: TATLineEnds;
begin
  Result:= Ed1.Strings.Endings;
end;

function TEditorFrame.GetNotifEnabled: boolean;
begin
  Result:= FNotif.Timer.Enabled;
end;

function TEditorFrame.GetNotifTime: integer;
begin
  Result:= FNotif.Timer.Interval;
end;

function TEditorFrame.GetReadOnly: boolean;
begin
  Result:= Ed1.ModeReadOnly;
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

procedure TEditorFrame.SetEncodingName(const Str: string);
begin
  if Str='' then exit;
  if SameText(Str, GetEncodingName) then exit;

  if SameText(Str, cEncNameUtf8_WithBom) then begin Editor.Strings.Encoding:= cEncUTF8; Editor.Strings.SaveSignUtf8:= true; end else
   if SameText(Str, cEncNameUtf8_NoBom) then begin Editor.Strings.Encoding:= cEncUTF8; Editor.Strings.SaveSignUtf8:= false; end else
    if SameText(Str, cEncNameUtf16LE_WithBom) then begin Editor.Strings.Encoding:= cEncWideLE; Editor.Strings.SaveSignWide:= true; end else
     if SameText(Str, cEncNameUtf16LE_NoBom) then begin Editor.Strings.Encoding:= cEncWideLE; Editor.Strings.SaveSignWide:= false; end else
      if SameText(Str, cEncNameUtf16BE_WithBom) then begin Editor.Strings.Encoding:= cEncWideBE; Editor.Strings.SaveSignWide:= true; end else
       if SameText(Str, cEncNameUtf16BE_NoBom) then begin Editor.Strings.Encoding:= cEncWideBE; Editor.Strings.SaveSignWide:= false; end else
        if SameText(Str, cEncNameAnsi) then begin Editor.Strings.Encoding:= cEncAnsi; Editor.Strings.EncodingCodepage:= ''; end else
         begin
           Editor.Strings.Encoding:= cEncAnsi;
           Editor.Strings.EncodingCodepage:= Str;
         end;
end;

procedure TEditorFrame.SetFileName(const AValue: string);
begin
  if SameFileName(FFileName, AValue) then Exit;
  FFileName:= AValue;

  //update Notif obj
  NotifEnabled:= NotifEnabled;
end;

procedure TEditorFrame.SetFileWasBig(AValue: boolean);
begin
  FFileWasBig:= AValue;
  if AValue then
  begin
    Editor.OptWrapMode:= cWrapOff;
    Editor2.OptWrapMode:= cWrapOff;
    Editor.OptMicromapVisible:= false;
    Editor2.OptMicromapVisible:= false;
    Editor.OptMinimapVisible:= false;
    Editor2.OptMinimapVisible:= false;
  end;
end;

procedure TEditorFrame.SetLocked(AValue: boolean);
begin
  if AValue=FLocked then exit;
  FLocked:= AValue;

  if FLocked then
  begin
    Editor.BeginUpdate;
    Editor2.BeginUpdate;
  end
  else
  begin
    Editor.EndUpdate;
    Editor2.EndUpdate;
  end;
end;

procedure TEditorFrame.SetNotifEnabled(AValue: boolean);
begin
  FNotif.Timer.Enabled:= false;
  FNotif.FileName:= '';

  if IsText and AValue and FileExistsUTF8(FileName) then
  begin
    FNotif.FileName:= FileName;
    FNotif.Timer.Enabled:= true;
  end;
end;

procedure TEditorFrame.SetNotifTime(AValue: integer);
begin
  FNotif.Timer.Interval:= AValue;
end;

procedure TEditorFrame.SetReadOnly(AValue: boolean);
begin
  Ed1.ModeReadOnly:= AValue;
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

function TEditorFrame.GetLexer: TecSyntAnalyzer;
begin
  Result:= Adapter.Lexer;
end;

function TEditorFrame.LexerName: string;
var
  an: TecSyntAnalyzer;
begin
  if Adapter=nil then exit('');
  an:= Adapter.Lexer;
  if an=nil then
    Result:= ''
  else
    Result:= an.LexerName;
end;

function TEditorFrame.LexerNameAtPos(Pnt: TPoint): string;
var
  an: TecSyntAnalyzer;
begin
  an:= Adapter.LexerAtPos(Pnt);
  if an=nil then
    Result:= ''
  else
    Result:= an.LexerName;
end;

procedure TEditorFrame.SetSplitHorz(AValue: boolean);
var
  al: TAlign;
begin
  if not IsText then exit;
  FSplitHorz:= AValue;

  if FSplitHorz then al:= alBottom else al:= alRight;
  Splitter.Align:= al;
  Ed2.Align:= al;

  SplitPos:= SplitPos;
end;

procedure TEditorFrame.SetSplitPos(AValue: double);
const
  cMin = 10;
begin
  if not IsText then exit;
  FSplitPos:= AValue;

  if FSplitHorz then
  begin
    Ed2.Height:= Max(cMin, trunc(FSplitPos*Height));
    Splitter.Top:= 0;
  end
  else
  begin
    Ed2.Width:= Max(cMin, trunc(FSplitPos*Width));
    Splitter.Left:= 0;
  end;
end;

procedure TEditorFrame.SetSplitted(AValue: boolean);
begin
  if not IsText then exit;

  FSplitted:= AValue;
  Ed2.Visible:= AValue;
  Splitter.Visible:= AValue;

  if AValue then
  begin
    SplitPos:= SplitPos;
    Ed2.Strings:= Ed1.Strings;
  end
  else
  begin
    Ed2.Strings:= nil;
  end;

  Ed2.Update(true);
end;

procedure TEditorFrame.EditorOnChange1(Sender: TObject);
begin
  EditorOnChangeCommon(Sender);

  if Splitted then
  begin
    Ed2.UpdateIncorrectCaretPositions;
    Ed2.Update(true);
  end;

  DoPyEvent(Editor, cEventOnChange, []);

  TimerChange.Enabled:= false;
  TimerChange.Interval:= UiOps.PyChangeSlow;
  TimerChange.Enabled:= true;
end;

procedure TEditorFrame.EditorOnChange2(Sender: TObject);
begin
  EditorOnChangeCommon(Sender);

  Ed1.UpdateIncorrectCaretPositions;
  Ed1.Update(true);
end;

procedure TEditorFrame.UpdateModifiedState;
begin
  if FModified<>Editor.Modified then
  begin
    FModified:= Editor.Modified;
    DoOnChangeCaption;
    DoPyEvent(Editor, cEventOnState, [IntToStr(EDSTATE_MODIFIED)]);
  end;

  DoOnUpdateStatus;
end;

procedure TEditorFrame.EditorOnChangeCommon(Sender: TObject);
begin
  UpdateModifiedState;
end;

procedure TEditorFrame.EditorOnEnter(Sender: TObject);
begin
  if Assigned(FOnFocusEditor) then
    FOnFocusEditor(Editor);

  DoPyEvent(Sender as TATSynEdit, cEventOnFocus, []);
end;

procedure TEditorFrame.EditorOnCommand(Sender: TObject; ACmd: integer;
  const AText: string; var AHandled: boolean);
begin
  if Assigned(FOnEditorCommand) then
    FOnEditorCommand(Sender, ACmd, AText, AHandled);
end;

procedure TEditorFrame.EditorOnCommandAfter(Sender: TObject; ACommand: integer;
  const AText: string);
var
  Ed: TATSynEdit;
  Caret: TATCaretItem;
  Str: atString;
  SLexerName: string;
  bWordChar: boolean;
begin
  Ed:= Sender as TATSynEdit;
  if Ed.Carets.Count<>1 then exit;
  Caret:= Ed.Carets[0];
  if not Ed.Strings.IsIndexValid(Caret.PosY) then exit;

  //autoshow autocompletion
  if (ACommand=cCommand_TextInsert) and
    (Length(AText)=1) then
  begin
    //autoshow by trigger chars
    if (UiOps.AutocompleteTriggerChars<>'') and
      (Pos(AText, UiOps.AutocompleteTriggerChars)>0) then
    begin
      Ed.DoCommand(cmd_AutoComplete);
      exit;
    end;

    //other conditions need word-char
    bWordChar:= IsCharWord(AText[1], '');
    if not bWordChar then exit;

    SLexerName:= LexerNameAtPos(Point(Caret.PosX, Caret.PosY));
    if SLexerName='' then SLexerName:= '-';

    //autoshow for HTML
    if UiOps.AutocompleteHtml and (Pos('HTML', SLexerName)>0) then
    begin
      Str:= Ed.Strings.Lines[Caret.PosY];
      if Copy(Str, Caret.PosX-1, 1)='<' then
        Ed.DoCommand(cmd_AutoComplete);
      exit;
    end;

    //autoshow for CSS
    if UiOps.AutocompleteCss and (SLexerName='CSS') then
    begin
      Str:= Ed.Strings.Lines[Caret.PosY];
      if EditorIsAutocompleteCssPosition(Ed, Caret.PosX-1, Caret.PosY) then
        Ed.DoCommand(cmd_AutoComplete);
      exit;
    end;

    //autoshow for others, when typed N chars
    if (UiOps.AutocompleteAutoshowCharCount>0) {and
       (UiOps.AutocompleteAutoshowLexers<>'')} then
    begin
      Inc(FTextCharsTyped);
      if FTextCharsTyped=UiOps.AutocompleteAutoshowCharCount then
        //if IsLexerListed(SLexerName, UiOps.AutocompleteAutoshowLexers) then
        begin
          FTextCharsTyped:= 0;
          Ed.DoCommand(cmd_AutoComplete);
          exit;
        end;
    end
    else
      FTextCharsTyped:= 0;
  end;
end;

procedure TEditorFrame.EditorOnClickDouble(Sender: TObject; var AHandled: boolean);
var
  Str: string;
begin
  Str:= DoPyEvent(Sender as TATSynEdit, cEventOnClickDbl,
          ['"'+ConvertShiftStateToString(KeyboardStateToShiftState)+'"']);
  AHandled:= Str=cPyFalse;
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
    true,
    true
    );
end;

procedure TEditorFrame.EditorOnClickMiddle(Sender: TObject; var AHandled: boolean);
begin
  AHandled:= false;
  if EditorOps.OpMouseMiddleClickPaste then
  begin
    AHandled:= true;
    (Sender as TATSynEdit).DoCommand(cmd_MouseClickAtCursor);
    (Sender as TATSynEdit).DoCommand(cCommand_ClipboardAltPaste); //uses PrimarySelection:TClipboard
    exit;
  end;
end;

procedure TEditorFrame.EditorOnClickGap(Sender: TObject;
  AGapItem: TATSynGapItem; APos: TPoint);
var
  Ed: TATSynEdit;
begin
  if not Assigned(AGapItem) then exit;
  Ed:= Sender as TATSynEdit;

  //Str:=
  DoPyEvent(Ed, cEventOnClickGap, [
    '"'+ConvertShiftStateToString(KeyboardStateToShiftState)+'"',
    IntToStr(AGapItem.LineIndex),
    IntToStr(AGapItem.Tag),
    IntToStr(AGapItem.Bitmap.Width),
    IntToStr(AGapItem.Bitmap.Height),
    IntToStr(APos.X),
    IntToStr(APos.Y)
    ]);
  //AHandled:= Str=cPyFalse;
end;


procedure TEditorFrame.DoOnResize;
begin
  inherited;
  SplitPos:= SplitPos;
end;

procedure TEditorFrame.InitEditor(var ed: TATSynEdit);
begin
  ed:= TATSynEdit.Create(Self);
  ed.Parent:= Self;

  ed.DoubleBuffered:= UiOps.DoubleBuffered;
  ed.AutoAdjustLayout(lapDefault, 100, UiOps.ScreenScale, 1, 1);

  ed.Font.Name:= EditorOps.OpFontName;
  ed.Font.Size:= EditorOps.OpFontSize;
  ed.Font.Quality:= EditorOps.OpFontQuality;

  ed.BorderStyle:= bsNone;
  ed.Keymap:= AppKeymap;
  ed.TabStop:= false;
  ed.OptUnprintedVisible:= EditorOps.OpUnprintedShow;
  ed.OptRulerVisible:= EditorOps.OpRulerShow;
  ed.OptScrollbarsNew:= true;

  ed.OnClick:= @EditorOnClick;
  ed.OnClickDouble:= @EditorOnClickDouble;
  ed.OnClickMiddle:= @EditorOnClickMiddle;
  ed.OnClickMoveCaret:= @EditorClickMoveCaret;
  ed.OnClickEndSelect:= @EditorClickEndSelect;
  ed.OnClickGap:= @EditorOnClickGap;
  ed.OnClickMicromap:= @EditorOnClickMicroMap;
  ed.OnEnter:= @EditorOnEnter;
  ed.OnChangeState:= @EditorOnChangeCommon;
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
end;

constructor TEditorFrame.Create(TheOwner: TComponent);
begin
  inherited;

  FFileName:= '';
  FModified:= false;
  FActiveAlt:= false;
  FTabColor:= clNone;
  Inc(FLastTabId);
  FTabId:= FLastTabId;
  FTabImageIndex:= -1;
  FNotInRecents:= false;

  InitEditor(Ed1);
  InitEditor(Ed2);

  Ed2.Visible:= false;
  Splitter.Visible:= false;
  Ed1.Align:= alClient;
  Ed2.Align:= alBottom;

  Ed1.OnChange:= @EditorOnChange1;
  Ed2.OnChange:= @EditorOnChange2;
  Ed1.EditorIndex:= 0;
  Ed2.EditorIndex:= 1;

  FSplitHorz:= true;
  FSplitPos:= 0.5;
  Splitted:= false;

  Adapter:= TATAdapterEControl.Create(Self);
  Adapter.DynamicHiliteEnabled:= EditorOps.OpLexerDynamicHiliteEnabled;
  Adapter.DynamicHiliteMaxLines:= EditorOps.OpLexerDynamicHiliteMaxLines;
  Adapter.EnabledLineSeparators:= EditorOps.OpLexerLineSeparators;

  Adapter.AddEditor(Ed1);
  Adapter.AddEditor(Ed2);

  //load options
  EditorApplyOps(Ed1, EditorOps, true, true);
  EditorApplyOps(Ed2, EditorOps, true, true);
  EditorApplyTheme(Ed1);
  EditorApplyTheme(Ed2);

  //newdoc props
  Ed1.Strings.Endings:= TATLineEnds(UiOps.NewdocEnds);
  Ed1.Strings.DoClearUndo;
  Ed1.Strings.Modified:= false;
  Ed1.Strings.EncodingDetectDefaultUtf8:= UiOps.DefaultEncUtf8;

  EncodingName:= AppEncodingShortnameToFullname(UiOps.NewdocEnc);
  Lexer:= AppManager.FindAnalyzer(UiOps.NewdocLexer);

  FNotif:= TATFileNotif.Create(Self);
  FNotif.Timer.Interval:= 1000;
  FNotif.Timer.Enabled:= false;
  FNotif.OnChanged:= @NotifChanged;
end;

destructor TEditorFrame.Destroy;
begin
  if not Application.Terminated then //prevent crash on exit
    DoPyEvent(Editor, cEventOnClose, []);

  inherited;
end;

function TEditorFrame.Editor: TATSynEdit;
begin
  if FActiveAlt then
    Result:= Ed2
  else
    Result:= Ed1;
end;

function TEditorFrame.Editor2: TATSynEdit;
begin
  if not FActiveAlt then
    Result:= Ed2
  else
    Result:= Ed1;
end;

function TEditorFrame.IsEmpty: boolean;
var
  Str: TATStrings;
begin
  //dont check Modified here better
  Str:= Ed1.Strings;
  Result:=
    (FileName='') and
    ((Str.Count=0) or ((Str.Count=1) and (Str.Lines[0]='')));
end;

function TEditorFrame.IsText: boolean;
begin
  Result:= not Assigned(FImage);
end;


procedure TEditorFrame.SetLexer(an: TecSyntAnalyzer);
var
  anNotCorrect: TecSyntAnalyzer;
begin
  if (FileName<>'') and (FileSize(FileName) div (1024*1024) >= UiOps.MaxFileSizeForLexer) then
  begin
    Adapter.Lexer:= nil;
    exit
  end;

  if not DoApplyLexerStylesMap(an, anNotCorrect) then
    DoDialogLexerStylesMap(anNotCorrect);
  Adapter.Lexer:= an;
end;

procedure TEditorFrame.DoFileOpen_AsPicture(const fn: string);
begin
  TabCaption:= ExtractFileName(fn);
  FFileName:= '?';

  Ed1.Hide;
  Ed2.Hide;
  Splitter.Hide;
  ReadOnly:= true;

  FImage:= TImage.Create(Self);
  FImage.Parent:= Self;
  FImage.Align:= alClient;
  try
    FImage.Picture.LoadFromFile(fn);
    FImage.Transparent:= true;
    FImageFilename:= fn;
  except
    FImageFilename:= '';
  end;

  FImagePanel:= TATPanelSimple.Create(Self);
  FImagePanel.OnPaint:= @DoImagePanelPaint;
  FImagePanel.Parent:= Self;
  FImagePanel.SetBounds(0, 0, 400, 400);
  FImagePanel.BorderStyle:= bsNone;
  FImagePanel.Color:= clSkyBlue;
  FImage.Parent:= FImagePanel;

  FrameResize(Self);
  DoOnChangeCaption;
end;


procedure TEditorFrame.DoFileOpen(const fn: string; AAllowLoadHistory, AAllowErrorMsgBox: boolean);
begin
  if not FileExistsUTF8(fn) then Exit;
  SetLexer(nil);

  if IsFilenameListedInExtensionList(fn, UiOps.PictureTypes) then
  begin
    DoFileOpen_AsPicture(fn);
    exit;
  end;

  try
    Editor.LoadFromFile(fn);
    FFileName:= fn;
    TabCaption:= ExtractFileName_Fixed(FFileName);
      //_fixed to show ":streamname" at end
  except
    if AAllowErrorMsgBox then
      MsgBox(msgCannotOpenFile+#13+fn, MB_OK or MB_ICONERROR);

    Editor.Strings.Clear;
    Editor.Strings.LineAdd('');
    Editor.DoCaretSingle(0, 0);
    Editor.Update(true);
    TabCaption:= GetUntitledCaption;
    exit
  end;

  //turn off opts for huge files
  FileWasBig:= Editor.Strings.Count>EditorOps.OpWrapEnabledMaxLines;

  SetLexer(DoLexerFindByFilename(fn));
  if AAllowLoadHistory then
    DoLoadHistory;
  UpdateReadOnlyFromFile;

  NotifEnabled:= UiOps.NotifEnabled;
end;

procedure TEditorFrame.UpdateReadOnlyFromFile;
begin
  if IsFileReadonly(FileName) then
    ReadOnly:= true;
end;

function TEditorFrame.DoFileSave(ASaveAs: boolean): boolean;
var
  an: TecSyntAnalyzer;
  attr: integer;
  PrevEnabled: boolean;
  NameCounter: integer;
  NameTemp: string;
begin
  Result:= false;
  if not IsText then exit(true); //disable saving, but close
  if DoPyEvent(Editor, cEventOnSaveBefore, [])=cPyFalse then exit(true); //disable saving, but close

  if ASaveAs or (FFileName='') then
  begin
    an:= Lexer;
    if Assigned(an) then
    begin
      SaveDialog.DefaultExt:= DoGetLexerDefaultExt(an);
      SaveDialog.Filter:= DoGetLexerFileFilter(an, msgAllFiles);
    end
    else
    begin
      SaveDialog.DefaultExt:= '.txt';
      SaveDialog.Filter:= '';
    end;

    if FFileName='' then
    begin
      //get first free filename: new.txt, new1.txt, new2.txt, ...
      NameCounter:= 0;
      repeat
        NameTemp:= SaveDialog.InitialDir+DirectorySeparator+
                   'new'+IfThen(NameCounter>0, IntToStr(NameCounter))+
                   SaveDialog.DefaultExt; //DefaultExt with dot
        if not FileExistsUTF8(NameTemp) then
        begin
          SaveDialog.FileName:= ExtractFileName(NameTemp);
          Break
        end;
        Inc(NameCounter);
      until false;
    end
    else
    begin
      SaveDialog.FileName:= ExtractFileName(FFileName);
      SaveDialog.InitialDir:= ExtractFileDir(FFileName);
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

    FFileName:= SaveDialog.FileName;
    Lexer:= DoLexerFindByFilename(FFileName);

    //add to recents saved-as file:
    if Assigned(FOnAddRecent) then
      FOnAddRecent(Self);
  end;

  PrevEnabled:= NotifEnabled;
  NotifEnabled:= false;

  while true do
  try
    FFileAttrPrepare(FFileName, attr);
    Editor.BeginUpdate;
    try
      Editor.SaveToFile(FFileName);
    finally
      Editor.EndUpdate;
    end;
    FFileAttrRestore(FFileName, attr);
    Break;
  except
    if MsgBox(msgCannotSaveFile+#10+FFileName,
      MB_RETRYCANCEL or MB_ICONERROR) = IDCANCEL then
      Exit(false);
  end;

  NotifEnabled:= PrevEnabled;

  Editor.OnChange(Editor); //modified
  if not TabCaptionFromApi then
    TabCaption:= ExtractFileName(FFileName);

  DoPyEvent(Editor, cEventOnSaveAfter, []);
  if Assigned(FOnSaveFile) then
    FOnSaveFile(Self);
  Result:= true;
end;

procedure TEditorFrame.DoFileReload_DisableDetectEncoding;
begin
  if FileName='' then exit;
  Editor.Strings.EncodingDetect:= false;
  Editor.Strings.LoadFromFile(FileName);
  Editor.Strings.EncodingDetect:= true;
  UpdateEds(true);
end;

procedure TEditorFrame.DoFileReload;
var
  PrevCaretX, PrevCaretY: integer;
  PrevTail: boolean;
  PrevLexer: string;
begin
  if FileName='' then exit;

  //remember props
  PrevLexer:= LexerName;
  PrevCaretX:= 0;
  PrevCaretY:= 0;

  if Editor.Carets.Count>0 then
    with Editor.Carets[0] do
      begin
        PrevCaretX:= PosX;
        PrevCaretY:= PosY;
      end;

  PrevTail:= UiOps.ReloadFollowTail and
    (Editor.Strings.Count>0) and
    (PrevCaretY=Editor.Strings.Count-1);

  //reopen
  DoFileOpen(FileName, false, false);
  if Editor.Strings.Count=0 then exit;

  //restore props
  Lexer:= AppManager.FindAnalyzer(PrevLexer);
  PrevCaretY:= Min(PrevCaretY, Editor.Strings.Count-1);
  if PrevTail then
  begin
    PrevCaretX:= 0;
    PrevCaretY:= Editor.Strings.Count-1;
  end;

  Application.ProcessMessages; //for DoGotoPos

  Editor.DoGotoPos(
    Point(PrevCaretX, PrevCaretY),
    Point(-1, -1),
    1,
    1, //indentVert must be >0
    true,
    false
    );

  OnUpdateStatus(Self);
end;

procedure TEditorFrame.SetLineEnds(Value: TATLineEnds);
begin
  if GetLineEnds=Value then Exit;
  Ed1.Strings.Endings:= Value;
  Ed1.Update;
  Ed2.Update;

  EditorOnChangeCommon(Self);
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
  ed: TATSynEdit;
begin
  ed:= Sender as TATSynEdit;
  if ABand=ed.GutterBandBm then
    EditorBookmarkSet(ed, ALine, 1, bmOpToggle, '');
end;

procedure TEditorFrame.EditorOnDrawBookmarkIcon(Sender: TObject; C: TCanvas; ALineNum: integer;
  const ARect: TRect);
var
  r: TRect;
  dx: integer;
  kind: integer;
begin
  r:= arect;
  if r.Left>=r.Right then exit;
  kind:= (Sender as TATSynEdit).Strings.LinesBm[ALineNum];
  if kind<=1 then
  begin
    c.brush.color:= GetAppColor('EdBookmarkIcon');
    c.pen.color:= c.brush.color;
    inc(r.top, 1);
    inc(r.left, 4);
    dx:= (r.bottom-r.top) div 2-1;
    c.Polygon([Point(r.left, r.top), Point(r.left+dx, r.top+dx), Point(r.left, r.top+2*dx)]);
  end
  else
  if (kind>=Low(AppBookmarkSetup)) and (kind<=High(AppBookmarkSetup)) then
  begin
    AppBookmarkImagelist.Draw(c, r.left, r.top,
      AppBookmarkSetup[kind].ImageIndex);
  end;
end;


function TEditorFrame.GetCommentString: string;
var
  an: TecSyntAnalyzer;
begin
  Result:= '';
  an:= Adapter.Lexer;
  if Assigned(an) then
    Result:= an.LineComment;
end;

function TEditorFrame.GetEnabledFolding: boolean;
begin
  Result:= Editor.OptFoldEnabled;
end;

procedure TEditorFrame.DoOnChangeCaption;
begin
  if Assigned(FOnChangeCaption) then
    FOnChangeCaption(Self);
end;

procedure TEditorFrame.DoImagePanelPaint(Sender: TObject);
const
  cell=8;
var
  c: TCanvas;
  i, j: integer;
begin
  c:= FImagePanel.Canvas;
  c.Brush.Color:= clWhite;
  c.FillRect(0, 0, FImagePanel.ClientWidth, FImagePanel.ClientHeight);

  for i:= 0 to FImagePanel.ClientWidth div cell + 1 do
    for j:= 0 to FImagePanel.ClientHeight div cell + 1 do
      if not (odd(i) xor odd(j)) then
      begin
        c.Brush.Color:= clLtGray;
        c.FillRect(i*cell, j*cell, (i+1)*cell, (j+1)*cell);
      end;
end;

procedure TEditorFrame.DoRestoreFolding;
var
  S: string;
begin
  if FFoldTodo<>'' then
  begin
    S:= FFoldTodo;
    FFoldTodo:= '';
    EditorSetFoldString(Editor, S);
  end;

  if FTopLineTodo>0 then
  begin
    Editor.LineTop:= FTopLineTodo;
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

procedure TEditorFrame.EditorClickMoveCaret(Sender: TObject; APrevPnt,
  ANewPnt: TPoint);
begin
  if Assigned(FOnEditorClickMoveCaret) then
    FOnEditorClickMoveCaret(Self, APrevPnt, ANewPnt);
end;

procedure TEditorFrame.EditorDrawMicromap(Sender: TObject; C: TCanvas;
  const ARect: TRect);
const
  cTagOccur = 101; //see plugin Hilite Occurrences
  cTagSpell = 105; //see SpellChecker plugin
var
  NScale: double;
//
  function GetItemRect(NLine1, NLine2: integer; ALeft: boolean): TRect;
  begin
    if ALeft then
    begin
      Result.Left:= ARect.Left;
      Result.Right:= Result.Left + EditorOps.OpMicromapWidthSmall;
    end
    else
    begin
      Result.Right:= ARect.Right;
      Result.Left:= Result.Right - EditorOps.OpMicromapWidthSmall;
    end;
    Result.Top:= ARect.Top+Trunc(NLine1*NScale);
    Result.Bottom:= Max(Result.Top+2, ARect.Top+Trunc((NLine2+1)*NScale));
  end;
//
var
  Ed: TATSynEdit;
  NColor: TColor;
  Caret: TATCaretItem;
  State: TATLineState;
  Mark: TATMarkerItem;
  NColorSelected, NColorOccur, NColorSpell: TColor;
  R1: TRect;
  NLine1, NLine2, i: integer;
begin
  Ed:= Sender as TATSynEdit;
  if Ed.Strings.Count=0 then exit;
  NScale:= (ARect.Bottom-ARect.Top) / Ed.Strings.Count;

  C.Brush.Color:= GetAppColor('EdMicromapBg');
  C.FillRect(ARect);

  R1:= GetItemRect(Ed.LineTop, Ed.LineBottom, true);
  R1.Right:= ARect.Right;

  C.Brush.Color:= GetAppColor('EdMicromapViewBg');
  C.FillRect(R1);

  NColorSelected:= Ed.Colors.TextSelBG;
  NColorOccur:= GetAppColor('EdMicromapOccur');
  NColorSpell:= GetAppColor('EdMicromapSpell');

  //paint line states
  for i:= 0 to Ed.Strings.Count-1 do
  begin
    State:= Ed.Strings.LinesState[i];
    case State of
      cLineStateNone: Continue;
      cLineStateAdded: NColor:= Ed.Colors.StateAdded;
      cLineStateChanged: NColor:= Ed.Colors.StateChanged;
      cLineStateSaved: NColor:= Ed.Colors.StateSaved;
      else Continue;
    end;
    C.Brush.Color:= NColor;
    C.FillRect(GetItemRect(i, i, true));
  end;

  //paint selections
  C.Brush.Color:= NColorSelected;
  for i:= 0 to Ed.Carets.Count-1 do
  begin
    Caret:= Ed.Carets[i];
    Caret.GetSelLines(NLine1, NLine2, false);
    if NLine1<0 then Continue;
    R1:= GetItemRect(NLine1, NLine2, false);
    C.FillRect(R1);
  end;

  //paint marks for plugins
  for i:= 0 to Ed.Attribs.Count-1 do
  begin
    Mark:= Ed.Attribs[i];
    case Mark.Tag of
      cTagSpell:
        begin
          C.Brush.Color:= NColorSpell;
          C.FillRect(GetItemRect(Mark.PosY, Mark.PosY, false));
        end;
      cTagOccur:
        begin
          C.Brush.Color:= NColorOccur;
          C.FillRect(GetItemRect(Mark.PosY, Mark.PosY, false));
        end;
    end;
  end;
end;

procedure TEditorFrame.EditorClickEndSelect(Sender: TObject; APrevPnt,
  ANewPnt: TPoint);
begin
  if Assigned(FOnEditorClickEndSelect) then
    FOnEditorClickEndSelect(Self, APrevPnt, ANewPnt);
end;


procedure TEditorFrame.DoOnChangeCaretPos;
begin
  if Assigned(FOnEditorChangeCaretPos) then
    FOnEditorChangeCaretPos(Self);
end;


procedure TEditorFrame.DoSaveHistory;
var
  c: TJSONConfig;
  path: string;
  items: TStringlist;
begin
  if FileName='' then exit;
  if UiOps.MaxHistoryFiles<2 then exit;

  c:= TJsonConfig.Create(nil);
  try
    try
      c.Formatted:= true;
      c.Filename:= GetAppPath(cFileOptionsHistoryFiles);
    except
      Showmessage(msgCannotReadConf+#13+c.Filename);
      exit
    end;

    path:= SMaskFilenameSlashes(FileName);
    items:= TStringlist.Create;
    try
      c.DeletePath(path);
      c.EnumSubKeys('/', items);
      while items.Count>=UiOps.MaxHistoryFiles do
      begin
        c.DeletePath('/'+items[0]);
        items.Delete(0);
      end;
    finally
      FreeAndNil(items);
    end;

    DoSaveHistoryEx(c, path);
  finally
    c.Free;
  end;
end;

procedure TEditorFrame.DoSaveHistoryEx(c: TJsonConfig; const path: string);
var
  lexname: string;
  caret: TATCaretItem;
  items: TStringList;
  N, i: integer;
begin
  if Lexer=nil then
    lexname:= ''
  else
    lexname:= Lexer.LexerName;

  c.SetValue(path+cHistory_Lexer, lexname);
  c.SetValue(path+cHistory_Enc, EncodingName);
  c.SetValue(path+cHistory_Top, Editor.LineTop);
  c.SetValue(path+cHistory_Wrap, Ord(Editor.OptWrapMode));
  c.SetValue(path+cHistory_RO, ReadOnly);
  c.SetValue(path+cHistory_Ruler, Editor.OptRulerVisible);
  c.SetValue(path+cHistory_Minimap, Editor.OptMinimapVisible);
  c.SetValue(path+cHistory_Micromap, Editor.OptMicromapVisible);
  c.SetValue(path+cHistory_TabSize, Editor.OptTabSize);
  c.SetValue(path+cHistory_TabSpace, Editor.OptTabSpaces);
  c.SetValue(path+cHistory_Unpri, Editor.OptUnprintedVisible);
  c.SetValue(path+cHistory_Unpri_Spaces, Editor.OptUnprintedSpaces);
  c.SetValue(path+cHistory_Unpri_Ends, Editor.OptUnprintedEnds);
  c.SetValue(path+cHistory_Unpri_Detail, Editor.OptUnprintedEndsDetails);
  c.SetValue(path+cHistory_Nums, Editor.Gutter[Editor.GutterBandNum].Visible);
  c.SetValue(path+cHistory_Fold, EditorGetFoldString(Editor));

  if TabColor=clNone then
    c.SetValue(path+cHistory_TabColor, '')
  else
    c.SetValue(path+cHistory_TabColor, ColorToString(TabColor));

  if Editor.Carets.Count>0 then
  begin
    caret:= Editor.Carets[0];
    c.SetValue(path+cHistory_Caret+'/x', caret.PosX);
    c.SetValue(path+cHistory_Caret+'/y', caret.PosY);
    c.SetValue(path+cHistory_Caret+'/x2', caret.EndX);
    c.SetValue(path+cHistory_Caret+'/y2', caret.EndY);
  end;

  items:= TStringList.Create;
  try
    for i:= 0 to Editor.Strings.Count-1 do
    begin
      N:= Editor.Strings.LinesBm[i];
      if (N>0) and EditorBookmarkIsStandard(N) then
        items.Add(IntToStr(i));
    end;
    c.SetValue(path+cHistory_Bookmark, items);
  finally
    FreeAndNil(items);
  end;
end;

procedure TEditorFrame.DoLoadHistory;
var
  c: TJSONConfig;
begin
  if FileName='' then exit;
  if UiOps.MaxHistoryFiles<2 then exit;

  c:= TJsonConfig.Create(nil);
  try
    try
      c.Formatted:= true;
      c.Filename:= GetAppPath(cFileOptionsHistoryFiles);
    except
      Showmessage(msgCannotReadConf+#13+c.Filename);
      exit
    end;

    DoLoadHistoryEx(c, SMaskFilenameSlashes(FileName));
  finally
    c.Free;
  end;
end;


procedure TEditorFrame.DoLoadHistoryEx(c: TJsonConfig; const path: string);
var
  str, str0: string;
  Caret: TATCaretItem;
  nTop, i: integer;
  items: TStringlist;
begin
  //file not listed?
  if c.GetValue(path+cHistory_Top, -1)<0 then exit;

  //lexer
  if Lexer=nil then str0:= '' else str0:= Lexer.LexerName;
  str:= c.GetValue(path+cHistory_Lexer, str0);
  if str='PHP' then
    str:= 'HTML'; //Cud 1.14: HTML lexer handles php files
  if (str<>'') and (str<>str0) then
    Lexer:= AppManager.FindAnalyzer(str);

  //enc
  str0:= EncodingName;
  str:= c.GetValue(path+cHistory_Enc, str0);
  if str<>str0 then
  begin
    EncodingName:= str;
    //reread in enc
    //but only if not modified (modified means other text is loaded)
    if FileName<>'' then
      if not Editor.Modified then
        Editor.LoadFromFile(FileName);
  end;

  TabColor:= StringToColorDef(c.GetValue(path+cHistory_TabColor, ''), clNone);

  ReadOnly:= c.GetValue(path+cHistory_RO, ReadOnly);
  if not FileWasBig then
  begin
    Editor.OptWrapMode:= TATSynWrapMode(c.GetValue(path+cHistory_Wrap, Ord(Editor.OptWrapMode)));
    Editor.OptMinimapVisible:= c.GetValue(path+cHistory_Minimap, Editor.OptMinimapVisible);
    Editor.OptMicromapVisible:= c.GetValue(path+cHistory_Micromap, Editor.OptMicromapVisible);
  end;
  Editor.OptRulerVisible:= c.GetValue(path+cHistory_Ruler, Editor.OptRulerVisible);
  Editor.OptTabSize:= c.GetValue(path+cHistory_TabSize, Editor.OptTabSize);
  Editor.OptTabSpaces:= c.GetValue(path+cHistory_TabSpace, Editor.OptTabSpaces);
  Editor.OptUnprintedVisible:= c.GetValue(path+cHistory_Unpri, Editor.OptUnprintedVisible);
  Editor.OptUnprintedSpaces:= c.GetValue(path+cHistory_Unpri_Spaces, Editor.OptUnprintedSpaces);
  Editor.OptUnprintedEnds:= c.GetValue(path+cHistory_Unpri_Ends, Editor.OptUnprintedEnds);
  Editor.OptUnprintedEndsDetails:= c.GetValue(path+cHistory_Unpri_Detail, Editor.OptUnprintedEndsDetails);

  if Assigned(Lexer) then
  begin
    //this seems ok: works even for open-file via cmdline
    FFoldTodo:= c.GetValue(path+cHistory_Fold, '');
    //linetop
    nTop:= c.GetValue(path+cHistory_Top, 0);
    ////FTopLineTodo:= nTop; //restore LineTop after analize done
    Editor.LineTop:= nTop; //scroll immediately
  end
  else
  begin
    //for open-file from app: ok
    //for open via cmdline: not ok (maybe need to do it after form shown? how?)
    Editor.Update(true);
    Application.ProcessMessages;
    Editor.LineTop:= c.GetValue(path+cHistory_Top, 0);
  end;

  with Editor.Gutter[Editor.GutterBandNum] do
    Visible:= c.GetValue(path+cHistory_Nums, Visible);

  //caret
  if Editor.Carets.Count>0 then
  begin
    caret:= Editor.Carets[0];
    caret.PosX:= c.GetValue(path+cHistory_Caret+'/x', 0);
    caret.PosY:= c.GetValue(path+cHistory_Caret+'/y', 0);
    caret.EndX:= c.GetValue(path+cHistory_Caret+'/x2', -1);
    caret.EndY:= c.GetValue(path+cHistory_Caret+'/y2', -1);
    Editor.UpdateIncorrectCaretPositions;
    Editor.DoEventCarets;
  end;

  //bookmarks
  items:= TStringlist.create;
  try
    c.GetValue(path+cHistory_Bookmark, items, '');
    for i:= 0 to items.Count-1 do
    begin
      nTop:= StrToIntDef(items[i], -1);
      if Editor.Strings.IsIndexValid(nTop) then
        Editor.Strings.LinesBm[nTop]:= 1;
    end;
  finally
    FreeAndNil(items);
  end;

  Editor.Update;
  if Splitted then
    Editor2.Update;
end;

function TEditorFrame.DoPyEvent(AEd: TATSynEdit; AEvent: TAppPyEvent;
  const AParams: array of string): string;
begin
  Result:= '';
  if Assigned(FOnPyEvent) then
    Result:= FOnPyEvent(AEd, AEvent, AParams);
end;


procedure TEditorFrame.SetTabColor(AColor: TColor);
var
  NPages, NTab: integer;
  Pages: TATPages;
  D: TATTabData;
begin
  FTabColor:= AColor;

  Groups.PagesAndTabIndexOfControl(Self, NPages, NTab);
  Pages:= Groups.Pages[NPages];
  D:= Pages.Tabs.GetTabData(NTab);
  if Assigned(D) then
  begin
    D.TabColor:= AColor;
    Pages.Tabs.Invalidate;
  end;
end;

procedure TEditorFrame.SetTabImageIndex(AValue: integer);
var
  NPages, NTab: integer;
  Pages: TATPages;
  D: TATTabData;
begin
  if FTabImageIndex=AValue then exit;
  FTabImageIndex:= AValue;

  Groups.PagesAndTabIndexOfControl(Self, NPages, NTab);
  Pages:= Groups.Pages[NPages];
  D:= Pages.Tabs.GetTabData(NTab);
  if Assigned(D) then
  begin
    D.TabImageIndex:= AValue;
    Pages.Tabs.Invalidate;
  end;
end;

procedure TEditorFrame.NotifChanged(Sender: TObject);
begin
  if not Modified then
  begin
    DoFileReload;
    exit
  end;

  case MsgBox(msgConfirmFileChangedOutside+#10+FileName+
         #10#10+msgConfirmReloadIt+#10+msgConfirmReloadItHotkeys,
         MB_YESNOCANCEL or MB_ICONQUESTION) of
    ID_YES:
      DoFileReload;
    ID_CANCEL:
      NotifEnabled:= false;
  end;
end;

procedure TEditorFrame.SetEnabledFolding(AValue: boolean);
begin
  Editor.OptFoldEnabled:= AValue;
  Editor2.OptFoldEnabled:= AValue;
end;

function TEditorFrame.PictureSizes: TPoint;
begin
  if Assigned(FImage) and Assigned(FImage.Picture) then
    Result:= Point(FImage.Picture.Width, FImage.Picture.Height)
  else
    Result:= Point(0, 0);
end;


procedure TEditorFrame.DoGotoPos(APosX, APosY: integer);
begin
  if APosY<0 then exit;
  if APosX<0 then APosX:= 0; //allow x<0

  Editor.LineTop:= APosY;
  TopLineTodo:= APosY; //check is it still needed
  Editor.DoGotoPos(
    Point(APosX, APosY),
    Point(-1, -1),
    UiOps.FindIndentHorz,
    UiOps.FindIndentVert,
    true,
    true
    );
  Editor.Update;
end;

end.

