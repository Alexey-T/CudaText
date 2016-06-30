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
  StdCtrls, Buttons, ExtCtrls,
  LclType, LclProc, Math,
  ATButtons,
  ATSynEdit,
  ATSynEdit_Edits,
  ATSynEdit_Commands,
  proc_globdata,
  proc_colors,
  proc_miscutils;

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
    bCancel: TSpeedButton;
    bCount: TATButton;
    bFindFirst: TATButton;
    bFindNext: TATButton;
    bFindPrev: TATButton;
    bMarkAll: TATButton;
    bSelectAll: TATButton;
    bRep: TATButton;
    bRepAll: TATButton;
    chkCase: TATButton;
    chkConfirm: TATButton;
    chkMulLine: TATButton;
    chkRegex: TATButton;
    chkWords: TATButton;
    chkWrap: TATButton;
    edFind: TATComboEdit;
    edRep: TATComboEdit;
    LabelFind: TLabel;
    LabelRep: TLabel;
    PanelBtn: TPanel;
    PanelBtnRep: TPanel;
    PanelOps1: TPanel;
    PanelOps2: TPanel;
    PanelX: TPanel;
    PanelOps: TPanel;
    PanelLabels: TPanel;
    PanelAll: TPanel;
    procedure bFindNextClick(Sender: TObject);
    procedure bFindPrevClick(Sender: TObject);
    procedure bMarkAllClick(Sender: TObject);
    procedure bRepClick(Sender: TObject);
    procedure bRepAllClick(Sender: TObject);
    procedure bCountClick(Sender: TObject);
    procedure bCancelClick(Sender: TObject);
    procedure bSelectAllClick(Sender: TObject);
    procedure chkMulLineClick(Sender: TObject);
    procedure chkRegexChange(Sender: TObject);
    procedure chkRepChange(Sender: TObject);
    procedure bFindFirstClick(Sender: TObject);
    procedure chkRepClick(Sender: TObject);
    procedure edFindChange(Sender: TObject);
    procedure edFindEnter(Sender: TObject);
    procedure edFindKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure edRepEnter(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
    FOnDone: TStrEvent;
    FReplace: boolean;
    FMulLine: boolean;
    procedure DoDone(const Str: string);
    procedure SetMulLine(Value: boolean);
  public
    { public declarations }
    FHotkeyFind,
    FHotkeyRep: TShortCut;
    //StringHintMarkAll: string;
    procedure UpdateState;
    procedure UpdateFonts;
    property Replace: boolean read FReplace write FReplace;
    property OnDone: TStrEvent read FOnDone write FOnDone;
    property MulLine: boolean read FMulLine write SetMulLine;
  end;

var
  fmFind: TfmFind;

implementation

{$R *.lfm}

{ TfmFind }

procedure TfmFind.chkRegexChange(Sender: TObject);
begin
  UpdateState;
end;

procedure TfmFind.bRepClick(Sender: TObject);
begin
  DoDone(cOpFindRep);
end;

procedure TfmFind.bFindNextClick(Sender: TObject);
begin
  DoDone(cOpFindNext);
end;

procedure TfmFind.bFindPrevClick(Sender: TObject);
begin
  DoDone(cOpFindPrev);
end;

procedure TfmFind.bMarkAllClick(Sender: TObject);
begin
  DoDone(cOpFindMarkAll);
end;

procedure TfmFind.bRepAllClick(Sender: TObject);
begin
  DoDone(cOpFindRepAll);
end;

procedure TfmFind.bCountClick(Sender: TObject);
begin
  DoDOne(cOpFindCount);
end;

procedure TfmFind.bCancelClick(Sender: TObject);
begin
  DoDone(cOpFindClose);
end;

procedure TfmFind.bSelectAllClick(Sender: TObject);
begin
  DoDone(cOpFindSelectAll);
end;

procedure TfmFind.chkMulLineClick(Sender: TObject);
begin
  MulLine:= not MulLine;
  UpdateState;
end;

procedure TfmFind.chkRepChange(Sender: TObject);
begin
  UpdateState;
end;

procedure TfmFind.bFindFirstClick(Sender: TObject);
begin
  DoDone(cOpFindFirst);
end;

procedure TfmFind.chkRepClick(Sender: TObject);
begin
  UpdateState;
end;

procedure TfmFind.edFindChange(Sender: TObject);
begin
  UpdateState;
end;

procedure TfmFind.edFindEnter(Sender: TObject);
begin
  edFind.DoCommand(cCommand_SelectAll);
end;

procedure TfmFind.edFindKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  //Ctrl+Down: copy Find to Replace
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
end;

procedure TfmFind.FormCreate(Sender: TObject);
begin
  edFind.OptTabSize:= 4;
  edRep.OptTabSize:= 4;
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
    Colors.TextFont:= GetAppColor('EdTextFont');
    Colors.TextBG:= GetAppColor('EdTextBg');
    Colors.TextSelFont:= GetAppColor('EdSelFont');
    Colors.TextSelBG:= GetAppColor('EdSelBg');
    Colors.ComboboxArrow:= GetAppColor('EdComboArrow');
    Colors.ComboboxArrowBG:= GetAppColor('EdComboArrowBg');
    Colors.TextDisabledFont:= GetAppColor('EdDisableFont');
    Colors.TextDisabledBG:= GetAppColor('EdDisableBg');
    Colors.BorderLine:= GetAppColor('EdBorder');
    Update;
  end;

  with edRep do
  begin
    Font.Name:= EditorOps.OpFontName;
    Font.Size:= EditorOps.OpFontSize;
    Font.Quality:= EditorOps.OpFontQuality;
    Colors.TextFont:= GetAppColor('EdTextFont');
    Colors.TextBG:= GetAppColor('EdTextBg');
    Colors.TextSelFont:= GetAppColor('EdSelFont');
    Colors.TextSelBG:= GetAppColor('EdSelBg');
    Colors.ComboboxArrow:= GetAppColor('EdComboArrow');
    Colors.ComboboxArrowBG:= GetAppColor('EdComboArrowBg');
    Colors.TextDisabledFont:= GetAppColor('EdDisableFont');
    Colors.TextDisabledBG:= GetAppColor('EdDisableBg');
    Colors.BorderLine:= GetAppColor('EdBorder');
    Update;
  end;

  bCancel.Font.Assign(LabelFind.Font);
end;

procedure TfmFind.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if key=VK_RETURN then
  begin
    //Enter: find next
    if Shift=[] then DoDone(cOpFindNext);
    //Shift+Enter: find prev
    if Shift=[ssShift] then DoDone(cOpFindPrev);
    //Ctrl+Enter: dont catch here, combobox must handle it as new-line
    if Shift=[ssCtrl] then exit;

    if Replace then
    begin
      //Alt+Enter: replace
      if Shift=[ssAlt] then DoDone(cOpFindRep);
      //Ctrl+Alt+Enter: replace and dont find next
      if Shift=[ssAlt, ssCtrl] then DoDone(cOpFindRepAndStop);
    end;

    key:= 0;
    exit
  end;

  if key=VK_ESCAPE then
  begin
    DoDone(cOpFindClose);
    key:= 0;
    exit;
  end;

  //handle Tab/ShiftTab: needed coz Mainmenu item handles ShiftTab (unindent)
  if key=VK_TAB then
  begin
    SelectNext(GetActiveControl(Self), not (ssShift in Shift), true);
    key:= 0;
    exit
  end;

  if (FHotkeyFind<>0) and (FHotkeyFind=KeyToShortCut(Key, Shift)) then
  begin
    FReplace:= false;
    UpdateState;
    key:= 0;
    exit;
  end;

  if (FHotkeyRep<>0) and (FHotkeyRep=KeyToShortCut(Key, Shift)) then
  begin
    FReplace:= true;
    UpdateState;
    key:= 0;
    exit;
  end;

  if (key=VK_R) and (Shift=[ssAlt]) then
    begin with chkRegex do checked:= not checked; chkRegexChange(Self); key:= 0; exit end;
  if (key=VK_C) and (Shift=[ssAlt]) then
    begin with chkCase do checked:= not checked; key:= 0; exit end;
  if (key=VK_W) and (Shift=[ssAlt]) then
    begin with chkWords do checked:= not checked; key:= 0; exit end;
  if (key=VK_Y) and (Shift=[ssAlt]) then
    begin with chkConfirm do checked:= not checked; key:= 0; exit end;
  if (key=VK_N) and (Shift=[ssAlt]) then
    begin with chkWrap do checked:= not checked; key:= 0; exit end;
  if (key=VK_M) and (Shift=[ssAlt]) then
    begin with chkMulLine do checked:= not checked; key:= 0; exit end;

  if (key=VK_A) and (Shift=[ssAlt]) then
    begin bRepAllClick(Self); key:= 0; exit end;
  if (key=VK_5) and (Shift=[ssAlt]) then
    begin bCountClick(Self); key:= 0; exit end;
  if (key=VK_6) and (Shift=[ssAlt]) then
    begin bSelectAllClick(Self); key:= 0; exit end;
  if (key=VK_7) and (Shift=[ssAlt]) then
    begin bMarkAllClick(Self); key:= 0; exit end;
end;

procedure TfmFind.FormShow(Sender: TObject);
begin
  UpdateFonts;
  UpdateButtonIconX(bCancel);
end;

procedure TfmFind.DoDone(const Str: string);
begin
  if Str=cOpFindPrev then
    if chkRegex.Checked then exit;

  if edFind.Text='' then
    if Str<>cOpFindClose then exit;

  if Assigned(FOnDone) then
    FOnDone(Self, Str);

  if Str<>cOpFindClose then
  begin
    edFind.DoAddLineToHistory(edFind.Text, UiOps.MaxHistoryEdits);
    edRep.DoAddLineToHistory(edRep.Text, UiOps.MaxHistoryEdits);
  end;
end;

procedure TfmFind.SetMulLine(Value: boolean);
const
  cScale = 2.6;
var
  NSizeY, NSmall: integer;
begin
  FMulLine:= Value;

  NSmall:= 4;
  NSizeY:= bFindFirst.Height;
  if FMulLine then NSizeY:= Trunc(NSizeY*cScale);

  edFind.Height:= NSizeY;
  edRep.Height:= NSizeY;
  edRep.Top:= edFind.Top+edFind.Height+NSmall;

  LabelRep.Top:= edRep.Top+NSmall;
  PanelBtnRep.Top:= edRep.Top;

  PanelAll.Height:= IfThen(Replace,
    edRep.Top+edRep.Height+NSmall,
    edFind.Top+edFind.Height+NSmall);
  ClientHeight:= PanelAll.Height;

  edFind.ModeOneLine:= not FMulLine;
  edRep.ModeOneLine:= not FMulLine;
end;


procedure TfmFind.UpdateState;
var
  rep, fill: boolean;
begin
  rep:= FReplace;
  fill:= true; //edFind.Text<>'';

  Height:= IfThen(rep, edRep.Top+edRep.Height+4, edFind.Top+edFind.Height+4);

  chkMulLine.Checked:= MulLine;
  chkWords.Enabled:= not chkRegex.Checked;
  chkConfirm.Visible:= rep;
  LabelRep.Visible:= rep;
  edRep.Visible:= rep;
  PanelLabels.Visible:= rep;
  PanelBtnRep.Visible:= rep;
  PanelLabels.Left:= PanelOps.Left+4;
  bCount.Visible:= not rep;
  bSelectAll.Visible:= not rep;
  bMarkAll.Visible:= not rep;

  bFindFirst.Enabled:= fill;
  bFindNext.Enabled:= fill;
  bFindPrev.Enabled:= fill and not chkRegex.Checked;
  bRep.Enabled:= fill;
  bRepAll.Enabled:= fill;
  bCount.Enabled:= fill;
  bSelectAll.Enabled:= fill;
  bMarkAll.Enabled:= fill;
end;

end.

