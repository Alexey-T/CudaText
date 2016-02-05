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
  ExtCtrls, Menus,
  FileUtil,
  LCLIntf, LCLProc, LCLType, StdCtrls,
  ATTabs,
  ATGroups,
  ATSynEdit,
  ATSynEdit_Keymap_Init,
  ATSynEdit_Adapter_EControl,
  ATSynEdit_Carets,
  ATSynEdit_CanvasProc,
  ATSynEdit_Commands,
  ATStrings,
  ATStringProc,
  ATStringProc_HtmlColor,
  ATFileNotif,
  ATButtons,
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
  proc_miscutils,
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
    FFileName: string;
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
    FOnSetLexer: TNotifyEvent;
    FOnAddRecent: TNotifyEvent;
    FOnPyEvent: TEditorFramePyEvent;
    FSplitted: boolean;
    FSplitHorz: boolean;
    FSplitPos: double;
    FActiveAlt: boolean;
    FLocked: boolean;
    FTabColor: TColor;
    FFoldTodo: string;
    FTopLineTodo: integer;
    FTabKeyCollectMarkers: boolean;
    FTagString: string;
    FNotInRecents: boolean;
    FMacroRecord: boolean;
    FMacroString: string;
    FImage: TImage;
    FImagePanel: TPanel;
    FImageFilename: string;
    procedure DoImagePanelPaint(Sender: TObject);
    procedure DoOnChangeCaption;
    procedure DoOnChangeCaretPos;
    procedure DoOnUpdateStatus;
    procedure EditorClickEndSelect(Sender: TObject; APrevPnt, ANewPnt: TPoint);
    procedure EditorClickMoveCaret(Sender: TObject; APrevPnt, ANewPnt: TPoint);
    procedure EditorOnChangeCommon(Sender: TObject);
    procedure EditorOnChange1(Sender: TObject);
    procedure EditorOnChange2(Sender: TObject);
    procedure EditorOnClick(Sender: TObject);
    procedure EditorOnClickGutter(Sender: TObject; ABand, ALine: integer);
    procedure EditorOnClickDouble(Sender: TObject; var AHandled: boolean);
    procedure EditorOnCommand(Sender: TObject; ACmd: integer; const AText: string; var AHandled: boolean);
    procedure EditorOnCommandAfter(Sender: TObject; ACommand: integer;
      const AText: string);
    procedure EditorOnDrawBookmarkIcon(Sender: TObject; C: TCanvas; ALineNum: integer; const ARect: TRect);
    procedure EditorOnEnter(Sender: TObject);
    procedure EditorOnDrawLine(Sender: TObject; C: TCanvas; AX, AY: integer;
      const AStr: atString; ACharSize: TPoint; const AExtent: TATIntArray);
    procedure EditorOnCalcBookmarkColor(Sender: TObject; ABookmarkKind: integer; out AColor: TColor);
    procedure EditorOnChangeCaretPos(Sender: TObject);
    procedure EditorOnKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    function GetCommentString: string;
    function GetEncodingName: string;
    function GetLineEnds: TATLineEnds;
    function GetNotifEnabled: boolean;
    function GetNotifTime: integer;
    function GetReadonly: boolean;
    function GetUnprintedEnds: boolean;
    function GetUnprintedEndsDetails: boolean;
    function GetUnprintedShow: boolean;
    function GetUnprintedSpaces: boolean;
    procedure InitEditor(var ed: TATSynEdit);
    procedure NotifChanged(Sender: TObject);
    procedure SetEncodingName(const Str: string);
    procedure SetLocked(AValue: boolean);
    procedure SetNotifEnabled(AValue: boolean);
    procedure SetNotifTime(AValue: integer);
    procedure SetTabColor(AColor: TColor);
    procedure SetUnprintedEnds(AValue: boolean);
    procedure SetUnprintedEndsDetails(AValue: boolean);
    procedure SetUnprintedShow(AValue: boolean);
    procedure SetSplitHorz(AValue: boolean);
    procedure SetSplitPos(AValue: double);
    procedure SetSplitted(AValue: boolean);
    procedure SetTabCaption(const AValue: string);
    procedure SetLineEnds(Value: TATLineEnds);
    procedure SetUnprintedSpaces(AValue: boolean);
    procedure UpdateEds;
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
    property ReadOnly: boolean read GetReadonly;
    property FileName: string read FFileName write FFileName;
    property TabCaption: string read FTabCaption write SetTabCaption;
    property Modified: boolean read FModified;
    property NotifEnabled: boolean read GetNotifEnabled write SetNotifEnabled;
    property NotifTime: integer read GetNotifTime write SetNotifTime;
    property Lexer: TecSyntAnalyzer read GetLexer write SetLexer;
    function LexerName: string;
    function LexerNameAtPos(Pnt: TPoint): string;
    property Locked: boolean read FLocked write SetLocked;
    property CommentString: string read GetCommentString;
    property TabColor: TColor read FTabColor write SetTabColor;
    property TabKeyCollectMarkers: boolean read FTabKeyCollectMarkers write FTabKeyCollectMarkers;
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
    //file
    procedure DoFileOpen(const fn: string; AllowFollowTail: boolean=false);
    function DoFileSave(ASaveAs: boolean; ASaveDlg: TSaveDialog): boolean;
    procedure DoFileReload(ADetectEnc: boolean);
    procedure DoSaveHistory;
    procedure DoSaveHistoryEx(c: TJsonConfig; const path: string);
    procedure DoLoadHistory;
    procedure DoLoadHistoryEx(c: TJsonConfig; const path: string);
    function DoPyEvent(AEd: TATSynEdit; AEvent: TAppPyEvent; const AParams: array of string): string;
    procedure DoRestoreFolding;
    //macro
    procedure DoMacroStart;
    procedure DoMacroStop(ACancel: boolean);
    property MacroRecord: boolean read FMacroRecord;
    property MacroString: string read FMacroString write FMacroString;

    //event
    property OnFocusEditor: TNotifyEvent read FOnFocusEditor write FOnFocusEditor;
    property OnChangeCaption: TNotifyEvent read FOnChangeCaption write FOnChangeCaption;
    property OnUpdateStatus: TNotifyEvent read FOnUpdateStatus write FOnUpdateStatus;
    property OnEditorClickMoveCaret: TATSynEditClickMoveCaretEvent read FOnEditorClickMoveCaret write FOnEditorClickMoveCaret;
    property OnEditorClickEndSelect: TATSynEditClickMoveCaretEvent read FOnEditorClickEndSelect write FOnEditorClickEndSelect;
    property OnEditorCommand: TATSynEditCommandEvent read FOnEditorCommand write FOnEditorCommand;
    property OnEditorChangeCaretPos: TNotifyEvent read FOnEditorChangeCaretPos write FOnEditorChangeCaretPos;
    property OnSaveFile: TNotifyEvent read FOnSaveFile write FOnSaveFile;
    property OnSetLexer: TNotifyEvent read FOnSetLexer write FOnSetLexer;
    property OnAddRecent: TNotifyEvent read FOnAddRecent write FOnAddRecent;
    property OnPyEvent: TEditorFramePyEvent read FOnPyEvent write FOnPyEvent;
  end;

implementation

{$R *.lfm}

const
  cSavLexer       = '/lexer';
  cSavEnc         = '/enc';
  cSavTop         = '/top';
  cSavWrap        = '/wrap_mode';
  cSavRO          = '/ro';
  cSavRuler       = '/ruler';
  cSavMinimap     = '/minimap';
  cSavTabSize     = '/tab_size';
  cSavTabSpace    = '/tab_spaces';
  cSavNums        = '/nums';
  cSavUnpri       = '/unprinted_show';
  cSavUnpriSp     = '/unprinted_spaces';
  cSavUnpriEnd    = '/unprinted_ends';
  cSavUnpriEndDet = '/unprinted_end_details';
  cSavCaret       = '/caret';
  cSavColor       = '/color';
  cSavBookmark    = '/bm';
  cSavFold        = '/folded';


{ TEditorFrame }

procedure TEditorFrame.SetTabCaption(const AValue: string);
begin
  if AValue='?' then Exit;
  if FTabCaption= AValue then Exit;
  FTabCaption:= AValue;
  DoOnChangeCaption;
end;

procedure TEditorFrame.EditorOnClick(Sender: TObject);
var
  NewAlt: boolean;
begin
  NewAlt:= Sender=Ed2;
  if NewAlt<>FActiveAlt then
  begin
    FActiveAlt:= NewAlt;
    DoOnUpdateStatus;
  end;

  DoPyEvent(Sender as TATSynEdit, cEventOnClick,
    ['"'+ConvertShiftStateToString(KeyboardStateToShiftState)+'"']);
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
  if DoPyEvent(Sender as TATSynEdit,
    cEventOnKey,
    [IntToStr(Key), '"'+ConvertShiftStateToString(Shift)+'"']) = cPyFalse then
    begin
      Key:= 0;
      Exit
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
          Result:= cEncNameUtf8
        else
          Result:= cEncNameUtf8NoBom;
      end;
    cEncWideLE:
      Result:= cEncNameUtf16LE;
    cEncWideBE:
      Result:= cEncNameUtf16BE;
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

function TEditorFrame.GetReadonly: boolean;
begin
  Result:= Ed1.Strings.ReadOnly;
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
  if Str=GetEncodingName then exit;
  if Str=cEncNameUtf8 then begin Editor.Strings.Encoding:= cEncUTF8; Editor.Strings.SaveSignUtf8:= true; end else
   if Str=cEncNameUtf8NoBom then begin Editor.Strings.Encoding:= cEncUTF8; Editor.Strings.SaveSignUtf8:= false; end else
    if Str=cEncNameUtf16LE then begin Editor.Strings.Encoding:= cEncWideLE end else
     if Str=cEncNameUtf16BE then begin Editor.Strings.Encoding:= cEncWideBE end else
      if Str=cEncNameAnsi then begin Editor.Strings.Encoding:= cEncAnsi; Editor.Strings.EncodingCodepage:= ''; end else
       begin Editor.Strings.Encoding:= cEncAnsi; Editor.Strings.EncodingCodepage:= Str; end;
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

  if AValue then
  begin
    FNotif.FileName:= FileName;
    FNotif.Timer.Enabled:= true;
  end;
end;

procedure TEditorFrame.SetNotifTime(AValue: integer);
begin
  FNotif.Timer.Interval:= AValue;
end;

procedure TEditorFrame.UpdateEds;
begin
  Ed2.OptUnprintedVisible:= Ed1.OptUnprintedVisible;
  Ed2.OptUnprintedSpaces:= Ed1.OptUnprintedSpaces;
  Ed2.OptUnprintedEnds:= Ed1.OptUnprintedEnds;
  Ed2.OptUnprintedEndsDetails:= Ed1.OptUnprintedEndsDetails;

  Ed1.Update;
  Ed2.Update;
end;

function TEditorFrame.GetLexer: TecSyntAnalyzer;
begin
  Result:= Adapter.Lexer;
end;

function TEditorFrame.LexerName: string;
var
  an: TecSyntAnalyzer;
begin
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

procedure TEditorFrame.EditorOnChangeCommon(Sender: TObject);
begin
  if FModified<>Editor.Modified then
  begin
    FModified:= Editor.Modified;
    DoOnChangeCaption;
  end;

  DoOnUpdateStatus;
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
begin
  Ed:= Sender as TATSynEdit;
  if (UiOps.AutocompleteAutoshowChars>0) and
     (UiOps.AutocompleteAutoshowLexers<>'') and
     (ACommand=cCommand_TextInsert) and
     (Ed.Carets.Count=1) and
     (Length(AText)=1) and IsCharWord(AText[1], '') then
  begin
    Inc(FTextCharsTyped);
    if FTextCharsTyped=UiOps.AutocompleteAutoshowChars then
      with Ed.Carets[0] do
        if IsLexerListed(LexerNameAtPos(Point(PosX, PosY)), UiOps.AutocompleteAutoshowLexers) then
        begin
          FTextCharsTyped:= 0;
          Ed.DoCommand(cmd_AutoComplete);
        end;
  end
  else
    FTextCharsTyped:= 0;
end;

procedure TEditorFrame.EditorOnClickDouble(Sender: TObject; var AHandled: boolean);
var
  Str: string;
begin
  Str:= DoPyEvent(Sender as TATSynEdit, cEventOnClickDbl,
          ['"'+ConvertShiftStateToString(KeyboardStateToShiftState)+'"']);
  AHandled:= Str=cPyFalse;
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

  ed.Font.Name:= EditorOps.OpFontName;
  ed.Font.Size:= EditorOps.OpFontSize;
  ed.Font.Quality:= EditorOps.OpFontQuality;

  ed.BorderStyle:= bsNone;
  ed.Keymap:= Keymap;
  ed.TabStop:= false;
  ed.OptUnprintedVisible:= false;
  ed.OptRulerVisible:= false;

  ed.OnClick:= @EditorOnClick;
  ed.OnClickDouble:= @EditorOnClickDouble;
  ed.OnClickMoveCaret:= @EditorClickMoveCaret;
  ed.OnClickEndSelect:= @EditorClickEndSelect;
  ed.OnEnter:= @EditorOnEnter;
  ed.OnChangeState:= @EditorOnChangeCommon;
  ed.OnChangeCaretPos:= @EditorOnChangeCaretPos;
  ed.OnCommand:= @EditorOnCommand;
  ed.OnCommandAfter:=@EditorOnCommandAfter;
  ed.OnClickGutter:= @EditorOnClickGutter;
  ed.OnCalcBookmarkColor:= @EditorOnCalcBookmarkColor;
  ed.OnDrawBookmarkIcon:= @EditorOnDrawBookmarkIcon;
  ed.OnDrawLine:= @EditorOnDrawLine;
  ed.OnKeyDown:= @EditorOnKeyDown;
end;

constructor TEditorFrame.Create(TheOwner: TComponent);
begin
  inherited;

  FFileName:= '';
  FModified:= false;
  FActiveAlt:= false;
  FTabColor:= clNone;
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
  Adapter.AddEditor(Ed1);
  Adapter.AddEditor(Ed2);
  Ed1.AdapterHilite:= Adapter;
  Ed2.AdapterHilite:= Adapter;

  //load options
  EditorApplyOps(Ed1, EditorOps, true);
  EditorApplyOps(Ed2, EditorOps, true);
  EditorApplyTheme(Ed1);
  EditorApplyTheme(Ed2);

  //newdoc props
  Ed1.Strings.Endings:= TATLineEnds(UiOps.NewdocEnds);
  Ed1.Strings.DoClearUndo;
  Ed1.Strings.Modified:= false;

  EncodingName:= UiOps.NewdocEnc;
  Lexer:= Manager.FindAnalyzer(UiOps.NewdocLexer);

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
begin
  Adapter.Lexer:= an;

  if Assigned(an) then
    if Assigned(FOnSetLexer) then
      FOnSetLexer(Self);
end;

procedure TEditorFrame.DoFileOpen(const fn: string; AllowFollowTail: boolean=false);
var
  bTail: boolean;
begin
  if not FileExistsUTF8(fn) then Exit;
  SetLexer(nil);

  if IsFilenameListedInExtensionList(fn, UiOps.PictureTypes) then
  begin
    TabCaption:= ExtractFileName(fn);
    FileName:= '?';

    Ed1.Hide;
    Ed2.Hide;
    Ed1.ModeReadOnly:= true;
    Ed2.ModeReadOnly:= true;
    Splitter.Hide;

    FImage:= TImage.Create(Self);
    FImage.Parent:= Self;
    FImage.Align:= alClient;
    try
      FImage.Picture.LoadFromFile(fn);
      FImageFilename:= fn;
    except
      FImageFilename:= '';
    end;

    FImagePanel:= TPanel.Create(Self);
    FImagePanel.OnPaint:=@DoImagePanelPaint;
    FImagePanel.Parent:= Self;
    FImagePanel.SetBounds(0, 0, 400, 400);
    FImagePanel.BorderStyle:= bsNone;
    FImagePanel.BevelInner:= bvNone;
    FImagePanel.BevelOuter:= bvNone;
    FImagePanel.Color:= clSkyBlue;
    FImage.Parent:= FImagePanel;
    FrameResize(Self);

    exit
  end;

  bTail:=
    AllowFollowTail and
    UiOps.ReloadFollowTail and
    (Editor.Strings.Count>0) and
    (Editor.Carets.Count>0) and
    (Editor.Carets[0].PosY=Editor.Strings.Count-1);

  try
    Editor.LoadFromFile(fn);
    FFileName:= fn;
    TabCaption:= ExtractFileName(FFileName);
  except
    MsgBox(msgCannotOpenFile+#13+fn, MB_OK or MB_ICONERROR);
    Editor.Strings.Clear;
    Editor.Strings.LineAdd('');
    Editor.DoCaretSingle(0, 0);
    Editor.Update(true);
    TabCaption:= GetUntitlesStr;
    exit
  end;

  SetLexer(AppFindLexer(fn));
  DoLoadHistory;

  if bTail then
    if Editor.Strings.Count>0 then
    begin
      Editor.DoCaretSingle(0, Editor.Strings.Count-1);
      Editor.Update;
      Editor.LineTop:= Editor.Strings.Count-1; //no lexer
      FTopLineTodo:= Editor.Strings.Count-1; //lexer active, must use this instead of Ed.LineTop
    end;

  if IsFileReadonly(fn) then
    Editor.ModeReadOnly:= true;

  NotifEnabled:= NotifEnabled;
end;

function TEditorFrame.DoFileSave(ASaveAs: boolean; ASaveDlg: TSaveDialog): boolean;
var
  an: TecSyntAnalyzer;
  attr: integer;
  PrevEnabled: boolean;
  NameBak: string;
begin
  Result:= false;
  if not IsText then Exit;
  if DoPyEvent(Editor, cEventOnSaveBefore, [])=cPyFalse then Exit;

  if ASaveAs or (FFileName='') then
  begin
    ASaveDlg.FileName:= ExtractFileName(Self.FileName);
    ASaveDlg.InitialDir:= ExtractFileDir(Self.FileName);

    an:= Lexer;
    if an<>nil then
    begin
      ASaveDlg.DefaultExt:= DoGetLexerDefaultExt(an);
      ASaveDlg.Filter:= DoGetLexerFileFilter(an, msgAllFiles);
    end
    else
    begin
      ASaveDlg.DefaultExt:= 'txt';
      ASaveDlg.Filter:= '';
    end;

    if not ASaveDlg.Execute then Exit;
    FFileName:= ASaveDlg.FileName;
    Lexer:= AppFindLexer(FFileName);

    //add to recents saved-as file:
    if Assigned(FOnAddRecent) then
      FOnAddRecent(Self);
  end;

  NameBak:= SGetFilenameBackup(FFileName, UiOps.BackupMode);
  if NameBak<>'' then
    if FileExistsUTF8(FFileName) then
      CopyFile(FFileName, NameBak, true{PreserveTime});

  try
    PrevEnabled:= NotifEnabled;
    NotifEnabled:= false;

    FFileAttrPrepare(FFileName, attr);
    Editor.SaveToFile(FFileName);
    FFileAttrRestore(FFileName, attr);

    NotifEnabled:= PrevEnabled;
  except
    MsgBox(msgCannotSaveFile+#13+FFileName, MB_OK or MB_ICONERROR);
    Exit;
  end;

  Editor.OnChange(Editor); //modified
  TabCaption:= ExtractFileName(FFileName);

  DoPyEvent(Editor, cEventOnSaveAfter, []);
  if Assigned(FOnSaveFile) then
    FOnSaveFile(Self);
  Result:= true;
end;

procedure TEditorFrame.DoFileReload(ADetectEnc: boolean);
begin
  if FileName='' then Exit;
  Editor.Strings.EncodingDetect:= ADetectEnc;
  Editor.Strings.LoadFromFile(FileName);
  Editor.Strings.EncodingDetect:= true;
  UpdateEds;
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
    EditorBookmarkSet(ed, ALine, 1, bmOpToggle);
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
  for i:= 0 to FImagePanel.ClientWidth div cell + 1 do
    for j:= 0 to FImagePanel.ClientHeight div cell + 1 do
    begin
      if odd(i) xor odd(j) then
        c.Brush.Color:= clWhite
      else
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
      c.Filename:= GetAppPath(cFileHistoryList);
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
  items: TStringlist;
  N, i: integer;
begin
  if FileName='' then exit;
  if Lexer=nil then
    lexname:= ''
  else
    lexname:= Lexer.LexerName;

  c.SetValue(path+cSavLexer, lexname);
  c.SetValue(path+cSavEnc, EncodingName);
  c.SetValue(path+cSavTop, Editor.LineTop);
  c.SetValue(path+cSavWrap, Ord(Editor.OptWrapMode));
  c.SetValue(path+cSavRO, Editor.ModeReadOnly);
  c.SetValue(path+cSavRuler, Editor.OptRulerVisible);
  c.SetValue(path+cSavMinimap, Editor.OptMinimapVisible);
  c.SetValue(path+cSavTabSize, Editor.OptTabSize);
  c.SetValue(path+cSavTabSpace, Editor.OptTabSpaces);
  c.SetValue(path+cSavUnpri, Editor.OptUnprintedVisible);
  c.SetValue(path+cSavUnpriSp, Editor.OptUnprintedSpaces);
  c.SetValue(path+cSavUnpriEnd, Editor.OptUnprintedEnds);
  c.SetValue(path+cSavUnpriEndDet, Editor.OptUnprintedEndsDetails);
  c.SetValue(path+cSavNums, Editor.Gutter[Editor.GutterBandNum].Visible);
  c.SetValue(path+cSavFold, EditorGetFoldString(Editor));

  if TabColor=clNone then
    c.SetValue(path+cSavColor, '')
  else
    c.SetValue(path+cSavColor, ColorToString(TabColor));

  if Editor.Carets.Count>0 then
  begin
    caret:= Editor.Carets[0];
    c.SetValue(path+cSavCaret+'/x', caret.PosX);
    c.SetValue(path+cSavCaret+'/y', caret.PosY);
    c.SetValue(path+cSavCaret+'/x2', caret.EndX);
    c.SetValue(path+cSavCaret+'/y2', caret.EndY);
  end;

  items:= TStringlist.Create;
  try
    for i:= 0 to Editor.Strings.Count-1 do
    begin
      N:= Editor.Strings.LinesBm[i];
      if (N>0) and AppBookmarkKindStandard(N) then
        items.Add(Inttostr(i));
    end;
    c.SetValue(path+cSavBookmark, items);
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
      c.Filename:= GetAppPath(cFileHistoryList);
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
  if FileName='' then exit;

  //file not listed?
  if c.GetValue(path+cSavTop, -1)<0 then exit;

  //lexer
  if Lexer=nil then str0:= '' else str0:= Lexer.LexerName;
  str:= c.GetValue(path+cSavLexer, str0);
  if str<>str0 then
    Lexer:= Manager.FindAnalyzer(str);

  //enc
  str0:= EncodingName;
  str:= c.GetValue(path+cSavEnc, str0);
  if str<>str0 then
  begin
    EncodingName:= str;
    Editor.LoadFromFile(FileName); //reread in enc
  end;

  TabColor:= StringToColorDef(c.GetValue(path+cSavColor, ''), clNone);

  Editor.OptWrapMode:= TATSynWrapMode(c.GetValue(path+cSavWrap, Ord(Editor.OptWrapMode)));
  Editor.ModeReadOnly:= c.GetValue(path+cSavRO, Editor.ModeReadOnly);
  Editor.OptRulerVisible:= c.GetValue(path+cSavRuler, Editor.OptRulerVisible);
  Editor.OptMinimapVisible:= c.GetValue(path+cSavMinimap, Editor.OptMinimapVisible);
  Editor.OptTabSize:= c.GetValue(path+cSavTabSize, Editor.OptTabSize);
  Editor.OptTabSpaces:= c.GetValue(path+cSavTabSpace, Editor.OptTabSpaces);
  Editor.OptUnprintedVisible:= c.GetValue(path+cSavUnpri, Editor.OptUnprintedVisible);
  Editor.OptUnprintedSpaces:= c.GetValue(path+cSavUnpriSp, Editor.OptUnprintedSpaces);
  Editor.OptUnprintedEnds:= c.GetValue(path+cSavUnpriEnd, Editor.OptUnprintedEnds);
  Editor.OptUnprintedEndsDetails:= c.GetValue(path+cSavUnpriEndDet, Editor.OptUnprintedEndsDetails);

  if Assigned(Lexer) then
  begin
    //this seems ok: works even for open-file via cmdline
    FFoldTodo:= c.GetValue(path+cSavFold, '');
    FTopLineTodo:= c.GetValue(path+cSavTop, 0); //must use, not Ed.LineTop
  end
  else
  begin
    //for open-file from app: ok
    //for open via cmdline: not ok (maybe need to do it after form shown? how?)
    Editor.Update(true);
    Application.ProcessMessages;
    Editor.LineTop:= c.GetValue(path+cSavTop, 0);
  end;

  with Editor.Gutter[Editor.GutterBandNum] do
    Visible:= c.GetValue(path+cSavNums, Visible);

  //caret
  if Editor.Carets.Count>0 then
  begin
    caret:= Editor.Carets[0];
    caret.PosX:= c.GetValue(path+cSavCaret+'/x', 0);
    caret.PosY:= c.GetValue(path+cSavCaret+'/y', 0);
    caret.EndX:= c.GetValue(path+cSavCaret+'/x2', -1);
    caret.EndY:= c.GetValue(path+cSavCaret+'/y2', -1);
    Editor.UpdateIncorrectCaretPositions;
    Editor.DoEventCarets;
  end;

  //bookmarks
  items:= TStringlist.create;
  try
    c.GetValue(path+cSavBookmark, items, '');
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
  Groups.PagesAndTabIndexOfControl(Self, NPages, NTab);
  Pages:= Groups.Pages[NPages];
  D:= Pages.Tabs.GetTabData(NTab);
  if D=nil then Exit;
  D.TabColor:= AColor;
  FTabColor:= AColor;
  Pages.Invalidate;
end;

procedure TEditorFrame.NotifChanged(Sender: TObject);
begin
  if not Modified then
  begin
    DoFileOpen(FileName, true);
    exit
  end;

  case MsgBox('File was changed outside:'#13+FileName+
         #13#13'Reload?'#13+
        '(Yes: reload. No: don''t reload. Cancel [Esc]: no more notifications about this file.)',
        MB_YESNOCANCEL or MB_ICONQUESTION) of
    ID_YES:
      DoFileOpen(FileName, true);
    ID_CANCEL:
      NotifEnabled:= false;
  end;
end;

function TEditorFrame.PictureSizes: TPoint;
begin
  if Assigned(FImage) and Assigned(FImage.Picture) then
    Result:= Point(FImage.Picture.Width, FImage.Picture.Height)
  else
    Result:= Point(0, 0);
end;

end.

