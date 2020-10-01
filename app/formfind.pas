(*
This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at http://mozilla.org/MPL/2.0/.

Copyright (c) Alexey Torgashin
*)
unit formfind;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls,
  LclType, LclProc, IniFiles, Math,
  ATButtons,
  ATPanelSimple,
  ATStringProc,
  ATSynEdit,
  ATSynEdit_Edits,
  ATSynEdit_Commands,
  ATSynEdit_Finder,
  ATSynEdit_Adapter_EControl,
  proc_msg,
  proc_globdata,
  proc_miscutils,
  proc_colors,
  proc_editor,
  ec_SyntAnal,
  formlexerstylemap;

type
  TAppFinderOperation = (
    afoNone,
    afoCloseDlg,
    afoFindFirst,
    afoFindNext,
    afoFindPrev,
    afoCountAll,
    afoExtractAll,
    afoFindSelectAll,
    afoFindMarkAll,
    afoReplace,
    afoReplaceStop,
    afoReplaceAll,
    afoReplaceGlobal
    );

const
  cAppFinderOperationString: array[TAppFinderOperation] of string = (
    '-',
    'x',
    'findfirst',
    'findnext',
    'findprev',
    'findcnt',
    'findget',
    'findsel',
    'findmark',
    'rep',
    'repstop',
    'repall',
    'repglobal'
    );

type
  TAppFinderOperationCategory = (
    afcNone,
    afcCloseDlg,
    afcFind,
    afcReplaceOne,
    afcReplaceGlobal
    );

const
  cAppFinderOperationCategory: array[TAppFinderOperation] of TAppFinderOperationCategory = (
    afcNone,
    afcCloseDlg,
    afcFind,
    afcFind,
    afcFind,
    afcFind,
    afcFind,
    afcFind,
    afcFind,
    afcReplaceOne,
    afcReplaceOne,
    afcReplaceOne,
    afcReplaceGlobal
    );

type
  TAppFinderOperationEvent = procedure(Sender: TObject; Op: TAppFinderOperation) of object;

function AppFinderOperationFromString(const Str: string): TAppFinderOperation;

type
  { TfmFind }

  TfmFind = class(TForm)
    bCount: TATButton;
    bExtract: TATButton;
    bFindFirst: TATButton;
    bFindNext: TATButton;
    bFindPrev: TATButton;
    bMarkAll: TATButton;
    bRep: TATButton;
    bRepAll: TATButton;
    bRepGlobal: TATButton;
    bSelectAll: TATButton;
    chkCase: TATButton;
    chkConfirm: TATButton;
    chkInSel: TATButton;
    chkMulLine: TATButton;
    bTokens: TATButton;
    chkRegex: TATButton;
    chkWords: TATButton;
    chkWrap: TATButton;
    edFind: TATComboEdit;
    edRep: TATComboEdit;
    LabelFind: TLabel;
    LabelRep: TLabel;
    PanelAll: TATPanelSimple;
    PanelBtn: TATPanelSimple;
    PanelBtnRep: TATPanelSimple;
    PanelOps: TATPanelSimple;
    PanelOps1: TATPanelSimple;
    PanelOps2: TATPanelSimple;
    PanelTop: TATPanelSimple;
    PanelTopOps: TATPanelSimple;
    procedure bExtractClick(Sender: TObject);
    procedure bFindNextClick(Sender: TObject);
    procedure bFindPrevClick(Sender: TObject);
    procedure bMarkAllClick(Sender: TObject);
    procedure bRepClick(Sender: TObject);
    procedure bRepAllClick(Sender: TObject);
    procedure bCountClick(Sender: TObject);
    procedure bCancelClick(Sender: TObject);
    procedure bRepGlobalClick(Sender: TObject);
    procedure bSelectAllClick(Sender: TObject);
    procedure bTokensClick(Sender: TObject);
    procedure chkCaseClick(Sender: TObject);
    procedure chkConfirmClick(Sender: TObject);
    procedure chkInSelClick(Sender: TObject);
    procedure chkMulLineClick(Sender: TObject);
    procedure chkRegexClick(Sender: TObject);
    procedure chkRepChange(Sender: TObject);
    procedure bFindFirstClick(Sender: TObject);
    procedure chkRepClick(Sender: TObject);
    procedure chkWordsClick(Sender: TObject);
    procedure chkWrapClick(Sender: TObject);
    procedure edFindChange(Sender: TObject);
    procedure edFindCommand(Sender: TObject; ACommand: integer;
      const AText: string; var AHandled: boolean);
    procedure edFindEnter(Sender: TObject);
    procedure edFindKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure edRepEnter(Sender: TObject);
    procedure edRepExit(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
    FReplace: boolean;
    FMultiLine: boolean;
    FNarrow: boolean;
    FOnResult: TAppFinderOperationEvent;
    FOnChangeOptions: TNotifyEvent;
    FLexerRegexThemed: boolean;
    Adapter: TATAdapterEControl;
    AdapterActive: boolean;
    procedure DoResult(Str: TAppFinderOperation);
    procedure SetIsDoubleBuffered(AValue: boolean);
    procedure SetMultiLine(AValue: boolean);
    procedure SetNarrow(AValue: boolean);
    procedure SetReplace(AValue: boolean);
    procedure UpdateButtonBold;
    procedure UpdateRegexHighlight;
  public
    { public declarations }
    FCaptionFind,
    FCaptionReplace: string;
    FBinaryMode: boolean;
    procedure Localize;
    procedure DoOnChange;
    procedure UpdateFormHeight;
    procedure UpdateState;
    procedure UpdateFonts;
    procedure UpdateFocus(AFindMode: boolean);
    procedure UpdateInputFind(const AText: UnicodeString);
    procedure UpdateInputReplace(const AText: UnicodeString);
    property OnResult: TAppFinderOperationEvent read FOnResult write FOnResult;
    property OnChangeOptions: TNotifyEvent read FOnChangeOptions write FOnChangeOptions;
    property IsReplace: boolean read FReplace write SetReplace;
    property IsMultiLine: boolean read FMultiLine write SetMultiLine;
    property IsNarrow: boolean read FNarrow write SetNarrow;
    property IsDoubleBuffered: boolean write SetIsDoubleBuffered;
  end;

var
  fmFind: TfmFind = nil;

implementation

{$R *.lfm}

const
  cTokensDesc: array[TATFinderTokensAllowed] of string = (
    'Any',
    'Only comments',
    'Only strings',
    'Only comments/strings',
    'Except comments',
    'Except strings',
    'Except comments/strings'
    );

  cTokensShorts: array[TATFinderTokensAllowed] of string = (
    '*',
    '+c',
    '+s',
    '+cs',
    '-c',
    '-s',
    '-cs'
    );

function AppFinderOperationFromString(const Str: string): TAppFinderOperation;
var
  op: TAppFinderOperation;
begin
  for op:= Low(TAppFinderOperation) to High(TAppFinderOperation) do
    if Str=cAppFinderOperationString[op] then
      exit(op);
  Result:= afoNone;
end;

{ TfmFind }

procedure TfmFind.chkRegexClick(Sender: TObject);
begin
  with chkRegex do
    Checked:= not Checked;
  UpdateState;
  DoOnChange;
end;

procedure TfmFind.bRepClick(Sender: TObject);
begin
  if IsReplace then
    DoResult(afoReplace);
end;

procedure TfmFind.bFindNextClick(Sender: TObject);
begin
  DoResult(afoFindNext);
end;

procedure TfmFind.bExtractClick(Sender: TObject);
begin
  DoResult(afoExtractAll);
end;

procedure TfmFind.bFindPrevClick(Sender: TObject);
begin
  DoResult(afoFindPrev);
end;

procedure TfmFind.bMarkAllClick(Sender: TObject);
begin
  DoResult(afoFindMarkAll);
end;

procedure TfmFind.bRepAllClick(Sender: TObject);
begin
  if IsReplace then
    DoResult(afoReplaceAll);
end;

procedure TfmFind.bCountClick(Sender: TObject);
begin
  DoResult(afoCountAll);
end;

procedure TfmFind.bCancelClick(Sender: TObject);
begin
  DoResult(afoCloseDlg);
end;

procedure TfmFind.bRepGlobalClick(Sender: TObject);
begin
  if IsReplace then
    if MsgBox(msgConfirmReplaceGlobal, MB_OKCANCEL or MB_ICONWARNING)=ID_OK then
      DoResult(afoReplaceGlobal);
end;

procedure TfmFind.bSelectAllClick(Sender: TObject);
begin
  DoResult(afoFindSelectAll);
end;

procedure TfmFind.bTokensClick(Sender: TObject);
begin
  DoOnChange;
end;

procedure TfmFind.chkCaseClick(Sender: TObject);
begin
  with chkCase do
    Checked:= not Checked;
  UpdateState;
  DoOnChange;
end;

procedure TfmFind.chkConfirmClick(Sender: TObject);
begin
  with chkConfirm do
    Checked:= not Checked;
  UpdateState;
  DoOnChange;
end;

procedure TfmFind.chkInSelClick(Sender: TObject);
begin
  with chkInSel do
    Checked:= not Checked;
  UpdateState;
  DoOnChange;
end;

procedure TfmFind.chkMulLineClick(Sender: TObject);
begin
  IsMultiLine:= not IsMultiLine;
  DoOnChange;
end;

procedure TfmFind.chkRepChange(Sender: TObject);
begin
  UpdateState;
end;

procedure TfmFind.bFindFirstClick(Sender: TObject);
begin
  DoResult(afoFindFirst);
end;

procedure TfmFind.chkRepClick(Sender: TObject);
begin
  UpdateState;
end;

procedure TfmFind.chkWordsClick(Sender: TObject);
begin
  with chkWords do
    Checked:= not Checked;
  UpdateState;
  DoOnChange;
end;

procedure TfmFind.chkWrapClick(Sender: TObject);
begin
  with chkWrap do
    Checked:= not Checked;
  UpdateState;
  DoOnChange;
end;

procedure TfmFind.edFindChange(Sender: TObject);
begin
  UpdateState;
  if AdapterActive then
    EditorHighlightBadRegexBrackets(edFind, false);
end;

procedure TfmFind.edFindCommand(Sender: TObject; ACommand: integer;
  const AText: string; var AHandled: boolean);
begin
  //auto turn on multi-line
  if ACommand=cCommand_KeyEnter then
  begin
    IsMultiLine:= true;
    exit
  end;
end;

procedure TfmFind.edFindEnter(Sender: TObject);
begin
  edFind.DoCommand(cCommand_SelectAll);
  UpdateButtonBold;
end;

procedure TfmFind.edFindKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  //Ctrl+Down: copy find_edit to replace_edit
  if (Key=VK_DOWN) and (Shift=[ssCtrl]) then
  begin
    edRep.Text:= edFind.Text;
    edRep.Update(true);
    key:= 0;
    exit
  end;
end;

procedure TfmFind.edRepEnter(Sender: TObject);
begin
  edRep.DoCommand(cCommand_SelectAll);
  UpdateButtonBold;
end;

procedure TfmFind.edRepExit(Sender: TObject);
begin
  UpdateButtonBold;
end;

procedure TfmFind.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction:= caHide;
end;

procedure TfmFind.FormCreate(Sender: TObject);
var
  kind: TATFinderTokensAllowed;
begin
  FCaptionFind:= 'Find';
  FCaptionReplace:= 'Replace';

  edFind.OptTabSize:= 4;
  edRep.OptTabSize:= 4;

  edFind.Strings.Endings:= cEndUnix;
  edRep.Strings.Endings:= cEndUnix;

  edFind.OptUnprintedSpaces:= false;
  edRep.OptUnprintedSpaces:= false;
  edFind.OptUnprintedEndsDetails:= false;
  edRep.OptUnprintedEndsDetails:= false;

  edFind.OptPasteAtEndMakesFinalEmptyLine:= false;
  edRep.OptPasteAtEndMakesFinalEmptyLine:= false;

  IsDoubleBuffered:= UiOps.DoubleBuffered;
  DoScalePanelControls(Self);

  bFindFirst.Hint:= UiOps.HotkeyFindFirst;
  bFindNext.Hint:= UiOps.HotkeyFindNext;
  bFindPrev.Hint:= UiOps.HotkeyFindPrev;
  bRep.Hint:= UiOps.HotkeyReplaceAndFindNext;
  bRepAll.Hint:= UiOps.HotkeyReplaceAll;
  bRepGlobal.Hint:= UiOps.HotkeyReplaceGlobal;
  bCount.Hint:= UiOps.HotkeyCountAll;
  bExtract.Hint:= UiOps.HotkeyExtractAll;
  bSelectAll.Hint:= UiOps.HotkeySelectAll;
  bMarkAll.Hint:= UiOps.HotkeyMarkAll;

  for kind in TATFinderTokensAllowed do
  begin
    bTokens.Items.Add(cTokensDesc[kind]);
    bTokens.ItemsShort.Add(cTokensShorts[kind]);
  end;

  //bTokens.Arrow:= true;
  bTokens.ShowShortItems:= true;
  bTokens.TextAlign:= taCenter;
  bTokens.ItemIndex:= 0;
  bTokens.Checkable:= true;

  Adapter:= TATAdapterEControl.Create(Self);
  Adapter.Lexer:= AppManager.FindLexerByName('RegEx');
end;


procedure TfmFind.UpdateFonts;
begin
  with LabelFind do
  begin
    Font.Name:= UiOps.VarFontName;
    Font.Size:= AppScaleFont(UiOps.VarFontSize);
    Font.Color:= GetAppColor(apclTabFont);
  end;
  LabelRep.Font.Assign(LabelFind.Font);

  with edFind do
  begin
    Font.Name:= EditorOps.OpFontName;
    Font.Size:= EditorOps.OpFontSize;
    Font.Quality:= EditorOps.OpFontQuality;
    OptBorderFocusedActive:= UiOps.ShowActiveBorder;
    EditorApplyTheme(edFind);
    Update;
  end;

  with edRep do
  begin
    Font.Name:= EditorOps.OpFontName;
    Font.Size:= EditorOps.OpFontSize;
    Font.Quality:= EditorOps.OpFontQuality;
    OptBorderFocusedActive:= UiOps.ShowActiveBorder;
    EditorApplyTheme(edRep);
    Update;
  end;

  //bCancel.Font.Assign(LabelFind.Font);
end;

procedure TfmFind.UpdateFocus(AFindMode: boolean);
begin
  if (not Visible) or (not Enabled) then exit;

  if AFindMode then
  begin
    if edFind.Visible and edFind.Enabled and edFind.CanFocus then
      edFind.SetFocus;
  end
  else
  if edRep.Visible and edRep.Enabled and edRep.CanFocus then
    edRep.SetFocus;
end;

procedure TfmFind.UpdateInputFind(const AText: UnicodeString);
begin
  IsMultiLine:= Pos(#10, AText)>0;
  edFind.Text:= AText;
  edFind.DoEventChange(); //for lexer RegEx
end;

procedure TfmFind.UpdateInputReplace(const AText: UnicodeString);
begin
  edRep.Text:= AText;
end;

procedure TfmFind.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  Str: string;
begin
  Str:= ShortcutToText(KeyToShortCut(Key, Shift));
  if Str='' then exit; //only Shift is pressed

  if Key=VK_ESCAPE then
  begin
    DoResult(afoCloseDlg);
    key:= 0;
    exit;
  end;

  if Str=UiOps.HotkeyFindFirst then
  begin
    DoResult(afoFindFirst);
    key:= 0;
    exit
  end;

  if (Str=UiOps.HotkeyFindNext) and (Str<>'Enter') then
  begin
    DoResult(afoFindNext);
    key:= 0;
    exit
  end;

  if Str='Enter' then
  begin
    //Enter: action depends on focus
    if IsReplace and edRep.Focused then
      DoResult(afoReplace)
    else
      DoResult(afoFindNext);
    key:= 0;
    exit
  end;

  if Str=UiOps.HotkeyFindPrev then
  begin
    DoResult(afoFindPrev);
    key:= 0;
    exit
  end;

  if (Str=UiOps.HotkeyReplaceAndFindNext) and IsReplace then
  begin
    if IsReplace then
      DoResult(afoReplace);
    key:= 0;
    exit
  end;

  if (Str=UiOps.HotkeyReplaceNoFindNext) and IsReplace then
  begin
    if IsReplace then
      DoResult(afoReplaceStop);
    key:= 0;
    exit
  end;

  if Str=UiOps.HotkeyFindDialog then
  begin
    IsReplace:= false;
    UpdateState;
    key:= 0;
    exit;
  end;

  if Str=UiOps.HotkeyReplaceDialog then
  begin
    IsReplace:= true;
    UpdateState;
    key:= 0;
    exit;
  end;

  if Str=UiOps.HotkeyToggleRegex then
  begin
    chkRegexClick(Self);
    UpdateState;
    key:= 0;
    exit
  end;

  if Str=UiOps.HotkeyToggleCaseSens then
  begin
    chkCaseClick(Self);
    UpdateState;
    key:= 0;
    exit
  end;

  if Str=UiOps.HotkeyToggleWords then
  begin
    chkWordsClick(Self);
    UpdateState;
    key:= 0;
    exit
  end;

  if Str=UiOps.HotkeyToggleConfirmRep then
  begin
    chkConfirmClick(Self);
    UpdateState;
    key:= 0;
    exit
  end;

  if Str=UiOps.HotkeyToggleWrapped then
  begin
    chkWrapClick(Self);
    UpdateState;
    key:= 0;
    exit
  end;

  if Str=UiOps.HotkeyToggleInSelect then
  begin
    chkInSelClick(Self);
    UpdateState;
    key:= 0;
    exit
  end;

  if Str=UiOps.HotkeyToggleTokens then
  begin
    bTokens.Click;
    UpdateState;
    key:= 0;
    exit
  end;

  if Str=UiOps.HotkeyToggleMultiline then
  begin
    chkMulLineClick(Self);
    UpdateState;
    key:= 0;
    exit
  end;

  if (Str=UiOps.HotkeyReplaceAll) and IsReplace then
  begin
    bRepAllClick(Self);
    UpdateState;
    key:= 0;
    exit
  end;

  if (Str=UiOps.HotkeyReplaceGlobal) and IsReplace then
  begin
    bRepGlobalClick(Self);
    UpdateState;
    key:= 0;
    exit
  end;

  if Str=UiOps.HotkeyCountAll then
  begin
    bCountClick(Self);
    UpdateState;
    key:= 0;
    exit
  end;

  if (Str=UiOps.HotkeyExtractAll) and bExtract.Enabled then
  begin
    bExtractClick(Self);
    UpdateState;
    key:= 0;
    exit
  end;

  if Str=UiOps.HotkeySelectAll then
  begin
    bSelectAllClick(Self);
    UpdateState;
    key:= 0;
    exit
  end;

  if Str=UiOps.HotkeyMarkAll then
  begin
    bMarkAllClick(Self);
    UpdateState;
    key:= 0;
    exit
  end;
end;

function BtnSize(C: TControl): integer;
begin
  if C.Visible then
    Result:= C.Width
  else
    Result:= 0;
end;

procedure TfmFind.FormResize(Sender: TObject);
var
  Size1, Size2: integer;
begin
  Size1:=
    BtnSize(bFindFirst)+
    BtnSize(bFindNext)+
    BtnSize(bFindPrev)+
    BtnSize(bCount)+
    BtnSize(bExtract)+
    BtnSize(bSelectAll)+
    BtnSize(bMarkAll);
  Size2:=
    BtnSize(bRep)+
    BtnSize(bRepAll)+
    BtnSize(bRepGlobal);

  edFind.Left:= Max(LabelFind.Width, LabelRep.Width)+4;
  edFind.Width:= Max(45,
    ClientWidth
    - edFind.Left
    - Max(Size1, Size2)
    - IfThen(not IsNarrow, PanelOps.Width)
    - 12
    );
  edRep.Width:= edFind.Width;

  PanelTopOps.Left:= edFind.Left;
end;


procedure TfmFind.FormShow(Sender: TObject);
begin
  UpdateFormHeight;
  UpdateFonts;
  FixFormPositionToDesktop(Self);
  OnResize(Self);
end;

procedure TfmFind.DoResult(Str: TAppFinderOperation);
begin
  if edFind.Text='' then
    if Str<>afoCloseDlg then exit;

  if Assigned(FOnResult) then
    FOnResult(Self, Str);

  if Str<>afoCloseDlg then
  begin
    edFind.DoAddLineToHistory(edFind.Text, UiOps.MaxHistoryEdits);
    edRep.DoAddLineToHistory(edRep.Text, UiOps.MaxHistoryEdits);
  end;

  UpdateState;
end;

procedure TfmFind.SetIsDoubleBuffered(AValue: boolean);
begin
  edFind.DoubleBuffered:= AValue;
  edRep.DoubleBuffered:= AValue;

  {
  //no need
  chkRegex.DoubleBuffered:= AValue;
  chkCase.DoubleBuffered:= AValue;
  chkWords.DoubleBuffered:= AValue;
  chkWrap.DoubleBuffered:= AValue;
  chkMulLine.DoubleBuffered:= AValue;
  chkConfirm.DoubleBuffered:= AValue;

  bFindFirst.DoubleBuffered:= AValue;
  bFindNext.DoubleBuffered:= AValue;
  bFindPrev.DoubleBuffered:= AValue;
  bCount.DoubleBuffered:= AValue;
  bSelectAll.DoubleBuffered:= AValue;
  bMarkAll.DoubleBuffered:= AValue;
  bRep.DoubleBuffered:= AValue;
  bRepAll.DoubleBuffered:= AValue;
  bCancel.DoubleBuffered:= AValue;
  }
end;

procedure TfmFind.SetMultiLine(AValue: boolean);
var
  NSizeY: integer;
begin
  FMultiLine:= AValue;
  chkMulLine.Checked:= AValue;

  edFind.ModeOneLine:= not FMultiLine;
  edRep.ModeOneLine:= not FMultiLine;

  edFind.OptUnprintedVisible:= FMultiLine;
  edRep.OptUnprintedVisible:= FMultiLine;

  NSizeY:= bFindFirst.Height;
  if FMultiLine then NSizeY:= Trunc(NSizeY*UiOps.FindMultiLineScale);

  edFind.Height:= NSizeY;
  edRep.Height:= NSizeY;

  UpdateState;
end;

procedure TfmFind.SetNarrow(AValue: boolean);
begin
  if FNarrow=AValue then Exit;
  FNarrow:= AValue;

  if FNarrow then
  begin
    chkRegex.Parent:= PanelTopOps;
    chkCase.Parent:= PanelTopOps;
    chkWords.Parent:= PanelTopOps;
    chkWrap.Parent:= PanelTopOps;
    chkInSel.Parent:= PanelTopOps;
    chkMulLine.Parent:= PanelTopOps;
    bTokens.Parent:= PanelTopOps;
    chkConfirm.Parent:= PanelTopOps;
    chkConfirm.Left:= 400; //to right
  end;

  PanelTopOps.Left:= edFind.Left;
end;

procedure TfmFind.SetReplace(AValue: boolean);
begin
  if FReplace=AValue then Exit;
  FReplace:= AValue;

  //in find mode: focus input, because old focused control maybe hidden now
  if not FReplace then
    if edFind.CanFocus then
      edFind.SetFocus;

  UpdateState;
end;

procedure TfmFind.UpdateFormHeight;
  //
  function MaxY(C: TControl): integer;
  var
    P: TPoint;
  begin
    if not C.Visible then exit(0);
    P:= Point(0, C.Height);
    P:= C.ClientToScreen(P);
    P:= Self.ScreenToClient(P);
    Result:= P.Y;
    //Lazarus sometimes gets big negative p.x/p.y
    if Result<0 then
      Result:= 0;
  end;
  //
const
  cMinHeight = 30;
  cHeightIncrease = 4;
var
  N: integer;
begin
  N:= Max(cMinHeight, IfThen(IsReplace, MaxY(edRep), MaxY(edFind)) + cHeightIncrease);
  Constraints.MinHeight:= N;
  Constraints.MaxHeight:= N;
  Height:= N;
end;

procedure TfmFind.UpdateState;
var
  N, i: integer;
begin
  if IsReplace then
    Caption:= FCaptionReplace
  else
    Caption:= FCaptionFind;

  PanelTop.Visible:= IsNarrow;
  PanelOps.Visible:= not IsNarrow;
  chkWords.Enabled:= not chkRegex.Checked and (edFind.Strings.Count<2); //disable "w" for multi-line input
  chkConfirm.Visible:= IsReplace or IsNarrow;
  edRep.Visible:= IsReplace;
  LabelRep.Visible:= IsReplace;
  PanelBtnRep.Visible:= IsReplace;
  chkConfirm.Enabled:= IsReplace;
  //bCount.Enabled:= not IsReplace;
  //bSelectAll.Enabled:= not IsReplace;
  //bMarkAll.Enabled:= not IsReplace;

  bFindFirst.Enabled:= true;
  bFindNext.Enabled:= true;
  bExtract.Enabled:= chkRegex.Checked;
  edRep.Enabled:= IsReplace;
  bRep.Enabled:= IsReplace;
  bRepAll.Enabled:= IsReplace;
  bRepGlobal.Enabled:= IsReplace;

  if FBinaryMode then
  begin
    chkRegex.Enabled:= false;
    chkWrap.Enabled:= false;
    chkInSel.Enabled:= false;
  end;

  ControlAutosizeOnlyByWidth(PanelBtn);

  UpdateButtonBold;
  UpdateFormHeight;

  UpdateRegexHighlight;
end;

procedure TfmFind.UpdateRegexHighlight;
var
  bActive: boolean;
  TempLexer: TecSyntAnalyzer;
begin
  bActive:= chkRegex.Checked;
  if AdapterActive<>bActive then
  begin
    AdapterActive:= bActive;
    if AdapterActive then
    begin
      if not FLexerRegexThemed then
      begin
        FLexerRegexThemed:= true;
        if Assigned(Adapter.Lexer) then
          DoApplyLexerStylesMap(Adapter.Lexer, TempLexer);
      end;

      Adapter.AddEditor(edFind);
      edFind.DoEventChange();
      edFind.InvalidateHilitingCache;
      Adapter.DoAnalyzeFromLine(0, true);
    end
    else
    begin
      edFind.AdapterForHilite:= nil;
      Adapter.AddEditor(nil);
      EditorHighlightBadRegexBrackets(edFind, true);
    end;
    edFind.Update;
  end;
end;

procedure TfmFind.UpdateButtonBold;
begin
  bFindNext.BoldFont:= not edRep.Focused;
  bRep.BoldFont:= edRep.Focused;
end;

procedure TfmFind.DoOnChange;
begin
  if Assigned(FOnChangeOptions) then
    FOnChangeOptions(nil);
end;

procedure TfmFind.Localize;
const
  section = 'd_f';
const
  init_HintRegex = 'Regular expressions';
  init_HintCase = 'Case sensitive';
  init_HintWords = 'Whole words';
  init_HintWrapped = 'Wrapped search';
  init_HintConfRep = 'Confirm on replace';
  init_HintMulLine = 'Multi-line inputs (Ctrl+Enter for new-line)';
  init_HintInSelect = 'Search in selection';
  init_HintTokens = 'Allowed syntax elements';
var
  fn: string;
  ini: TIniFile;
begin
  with chkRegex do Hint:= init_HintRegex     +' - '+UiOps.HotkeyToggleRegex;
  with chkCase do Hint:= init_HintCase       +' - '+UiOps.HotkeyToggleCaseSens;
  with chkWords do Hint:= init_HintWords     +' - '+UiOps.HotkeyToggleWords;
  with chkWrap do Hint:= init_HintWrapped    +' - '+UiOps.HotkeyToggleWrapped;
  with chkConfirm do Hint:= init_HintConfRep +' - '+UiOps.HotkeyToggleConfirmRep;
  with chkInSel do Hint:= init_HintInSelect  +' - '+UiOps.HotkeyToggleInSelect;
  with chkMulLine do Hint:= init_HintMulLine +' - '+UiOps.HotkeyToggleMultiline;
  with bTokens do Hint:= init_HintTokens     +' - '+UiOps.HotkeyToggleTokens;

  fn:= GetAppLangFilename;
  if FileExists(fn) then
  begin
    ini:= TIniFile.Create(fn);
    try
      FCaptionFind:= ini.ReadString(section, '_f', FCaptionFind);
      FCaptionReplace:= ini.ReadString(section, '_r', FCaptionReplace);
      with bFindFirst do Caption:= ini.ReadString(section, 'f_f', Caption);
      with bFindNext do Caption:= ini.ReadString(section, 'f_n', Caption);
      with bFindPrev do Caption:= ini.ReadString(section, 'f_p', Caption);
      with bCount do Caption:= ini.ReadString(section, 'cnt', Caption);
      with bExtract do Caption:= ini.ReadString(section, 'get', Caption);
      with bSelectAll do Caption:= ini.ReadString(section, 'sel', Caption);
      with bMarkAll do Caption:= ini.ReadString(section, 'mk', Caption);
      with bRep do Caption:= ini.ReadString(section, 'r_c', Caption);
      with bRepAll do Caption:= ini.ReadString(section, 'r_a', Caption);
      with bRepGlobal do Caption:= ini.ReadString(section, 'r_gl', Caption);
      with LabelFind do Caption:= ini.ReadString(section, 'f_tx', Caption);
      with LabelRep do Caption:= ini.ReadString(section, 'r_tx', Caption);

      with chkRegex do Hint:= ini.ReadString(section, 'h_re', init_HintRegex)      +' - '+UiOps.HotkeyToggleRegex;
      with chkCase do Hint:= ini.ReadString(section, 'h_ca', init_HintCase)        +' - '+UiOps.HotkeyToggleCaseSens;
      with chkWords do Hint:= ini.ReadString(section, 'h_wo', init_HintWords)      +' - '+UiOps.HotkeyToggleWords;
      with chkWrap do Hint:= ini.ReadString(section, 'h_wr', init_HintWrapped)     +' - '+UiOps.HotkeyToggleWrapped;
      with chkConfirm do Hint:= ini.ReadString(section, 'h_cf', init_HintConfRep)  +' - '+UiOps.HotkeyToggleConfirmRep;
      with chkInSel do Hint:= ini.ReadString(section, 'h_sel', init_HintInSelect)  +' - '+UiOps.HotkeyToggleInSelect;
      with chkMulLine do Hint:= ini.ReadString(section, 'h_mul', init_HintMulLine) +' - '+UiOps.HotkeyToggleMultiline;
      with bTokens do Hint:= ini.ReadString(section, 'h_tok', init_HintTokens)     +' - '+UiOps.HotkeyToggleTokens;

      with bTokens do
      begin
        Items[0]:= ini.ReadString(section, 'tk_a', Items[0]);
        Items[1]:= ini.ReadString(section, 'tk_y_c', Items[1]);
        Items[2]:= ini.ReadString(section, 'tk_y_s', Items[2]);
        Items[3]:= ini.ReadString(section, 'tk_y_cs', Items[3]);
        Items[4]:= ini.ReadString(section, 'tk_n_c', Items[4]);
        Items[5]:= ini.ReadString(section, 'tk_n_s', Items[5]);
        Items[6]:= ini.ReadString(section, 'tk_n_cs', Items[6]);
      end;
    finally
      FreeAndNil(ini);
    end;
  end;

  bFindFirst.AutoSize:= true;
  bFindNext.AutoSize:= true;
  bFindPrev.AutoSize:= true;
  bCount.AutoSize:= true;
  bExtract.AutoSize:= true;
  bSelectAll.AutoSize:= true;
  bMarkAll.AutoSize:= true;
  bRep.AutoSize:= true;
  bRepAll.AutoSize:= true;
  bRepGlobal.AutoSize:= true;
end;

end.

