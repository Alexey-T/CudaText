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
  LclType, LclProc, Math,
  ATButtons, ATPanelSimple,
  ATStringProc,
  ATSynEdit,
  ATSynEdit_Edits,
  ATSynEdit_Commands,
  proc_globdata,
  proc_miscutils,
  proc_colors,
  proc_editor;

const
  cOpFindFirst='findfirst';
  cOpFindNext='findnext';
  cOpFindPrev='findprev';
  cOpFindRep='rep';
  cOpFindRepAndStop='repstop';
  cOpFindRepAll='repall';
  cOpFindCount='findcnt';
  cOpFindSelectAll='findsel';
  cOpFindMarkAll='findmark';
  cOpFindClose='x';

type
  { TfmFind }

  TfmFind = class(TForm)
    bCount: TATButton;
    bFindFirst: TATButton;
    bFindNext: TATButton;
    bFindPrev: TATButton;
    bMarkAll: TATButton;
    bRep: TATButton;
    bRepAll: TATButton;
    bSelectAll: TATButton;
    chkCase: TATButton;
    chkConfirm: TATButton;
    chkInSel: TATButton;
    chkMulLine: TATButton;
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
    procedure bFindNextClick(Sender: TObject);
    procedure bFindPrevClick(Sender: TObject);
    procedure bMarkAllClick(Sender: TObject);
    procedure bRepClick(Sender: TObject);
    procedure bRepAllClick(Sender: TObject);
    procedure bCountClick(Sender: TObject);
    procedure bCancelClick(Sender: TObject);
    procedure bSelectAllClick(Sender: TObject);
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
    FOnResult: TStrEvent;
    FOnChangeOptions: TNotifyEvent;
    procedure DoOnChange;
    procedure DoResult(const Str: string);
    procedure SetIsDoubleBuffered(AValue: boolean);
    procedure SetMultiLine(AValue: boolean);
    procedure SetNarrow(AValue: boolean);
    procedure SetReplace(AValue: boolean);
    procedure UpdateButtonBold;
  public
    { public declarations }
    FCaptionFind,
    FCaptionReplace: string;
    FBinaryMode: boolean;
    procedure UpdateSize;
    procedure UpdateState;
    procedure UpdateFonts;
    procedure UpdateFocus(AFindMode: boolean);
    property OnResult: TStrEvent read FOnResult write FOnResult;
    property OnChangeOptions: TNotifyEvent read FOnChangeOptions write FOnChangeOptions;
    property IsReplace: boolean read FReplace write SetReplace;
    property IsMultiLine: boolean read FMultiLine write SetMultiLine;
    property IsNarrow: boolean read FNarrow write SetNarrow;
    property IsDoubleBuffered: boolean write SetIsDoubleBuffered;
  end;

var
  fmFind: TfmFind;


implementation

{$R *.lfm}

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
  DoResult(cOpFindRep);
end;

procedure TfmFind.bFindNextClick(Sender: TObject);
begin
  DoResult(cOpFindNext);
end;

procedure TfmFind.bFindPrevClick(Sender: TObject);
begin
  DoResult(cOpFindPrev);
end;

procedure TfmFind.bMarkAllClick(Sender: TObject);
begin
  DoResult(cOpFindMarkAll);
end;

procedure TfmFind.bRepAllClick(Sender: TObject);
begin
  DoResult(cOpFindRepAll);
end;

procedure TfmFind.bCountClick(Sender: TObject);
begin
  DoResult(cOpFindCount);
end;

procedure TfmFind.bCancelClick(Sender: TObject);
begin
  DoResult(cOpFindClose);
end;

procedure TfmFind.bSelectAllClick(Sender: TObject);
begin
  DoResult(cOpFindSelectAll);
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
  DoResult(cOpFindFirst);
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
  bCount.Hint:= UiOps.HotkeyCountAll;
  bSelectAll.Hint:= UiOps.HotkeySelectAll;
  bMarkAll.Hint:= UiOps.HotkeyMarkAll;
end;


procedure TfmFind.UpdateFonts;
begin
  with LabelFind do
  begin
    Font.Name:= UiOps.VarFontName;
    Font.Size:= UiOps.VarFontSize;
    Font.Color:= GetAppColor('TabFont');
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

procedure TfmFind.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  Str: string;
begin
  Str:= ShortcutToText(KeyToShortCut(Key, Shift));

  if Key=VK_ESCAPE then
  begin
    DoResult(cOpFindClose);
    key:= 0;
    exit;
  end;

  if Str=UiOps.HotkeyFindFirst then
  begin
    DoResult(cOpFindFirst);
    key:= 0;
    exit
  end;

  if (Str=UiOps.HotkeyFindNext) and (Str<>'Enter') and (Str<>'') then
  begin
    DoResult(cOpFindNext);
    key:= 0;
    exit
  end;

  if Str='Enter' then
  begin
    //Enter: action depends on focus
    if IsReplace and edRep.Focused then
      DoResult(cOpFindRep)
    else
      DoResult(cOpFindNext);
    key:= 0;
    exit
  end;

  if Str=UiOps.HotkeyFindPrev then
  begin
    DoResult(cOpFindPrev);
    key:= 0;
    exit
  end;

  if Str=UiOps.HotkeyReplaceAndFindNext then
  begin
    if IsReplace then
      DoResult(cOpFindRep);
    key:= 0;
    exit
  end;

  if Str=UiOps.HotkeyReplaceNoFindNext then
  begin
    if IsReplace then
      DoResult(cOpFindRepAndStop);
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

  if Str=UiOps.HotkeyToggleMultiline then
  begin
    chkMulLineClick(Self);
    UpdateState;
    key:= 0;
    exit
  end;

  if Str=UiOps.HotkeyReplaceAll then
  begin
    bRepAllClick(Self);
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
begin
  edFind.Width:= Max(45,
    ClientWidth
    - edFind.Left
    - BtnSize(bFindFirst)
    - BtnSize(bFindNext)
    - BtnSize(bFindPrev)
    - BtnSize(bCount)
    - BtnSize(bSelectAll)
    - BtnSize(bMarkAll)
    - IfThen(not IsNarrow, PanelOps.Width)
    - 12
    );
  edRep.Width:= edFind.Width;
end;


procedure TfmFind.FormShow(Sender: TObject);
const
  cReservePixels = 80;
begin
  UpdateSize;
  UpdateFonts;

  //fit form to screen
  if Left>=Screen.DesktopWidth-cReservePixels then
    Left:= Screen.DesktopWidth-Width;
  if Top>=Screen.DesktopHeight-cReservePixels then
    Top:= Screen.DesktopHeight-200;
end;

procedure TfmFind.DoResult(const Str: string);
begin
  if Str=cOpFindPrev then
    if chkRegex.Checked then exit;

  if edFind.Text='' then
    if Str<>cOpFindClose then exit;

  if Assigned(FOnResult) then
    FOnResult(Self, Str);

  if Str<>cOpFindClose then
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

procedure TfmFind.UpdateSize;
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
  end;
  //
var
  N: integer;
begin
  N:= IfThen(IsReplace, MaxY(edRep), MaxY(edFind)) + 4;
  Constraints.MinHeight:= N;
  Constraints.MaxHeight:= N;
  Height:= N;
end;

procedure TfmFind.UpdateState;
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
  bFindPrev.Enabled:= not chkRegex.Checked;
  edRep.Enabled:= IsReplace;
  bRep.Enabled:= IsReplace;
  bRepAll.Enabled:= IsReplace;

  if FBinaryMode then
  begin
    chkRegex.Enabled:= false;
    chkWrap.Enabled:= false;
    chkInSel.Enabled:= false;
  end;

  UpdateButtonBold;
  UpdateSize;
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

end.

