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
  StdCtrls,
  LclType, LclProc, Buttons,
  ATButtons,
  ATSynEdit,
  ATSynEdit_Edits,
  proc_globdata,
  proc_colors,
  proc_str;

const
  cOpFind='find';
  cOpFindNext='findnext';
  cOpFindRep='rep';
  cOpFindRepAll='repall';
  cOpFindCount='findcnt';
  cOpFindMarkAll='findmark';
  cOpFindClose='x';

type
  { TfmFind }

  TfmFind = class(TForm)
    bCancel: TSpeedButton;
    bCount: TATButton;
    bFindNext: TATButton;
    bRepAll: TATButton;
    bMarkAll: TATButton;
    chkRegex: TATButton;
    chkCase: TATButton;
    bRep: TATButton;
    chkRep: TATButton;
    chkWords: TATButton;
    chkBack: TATButton;
    chkConfirm: TATButton;
    bFindFirst: TATButton;
    edFind: TATComboEdit;
    edRep: TATComboEdit;
    LabelFind: TLabel;
    procedure bFindNextClick(Sender: TObject);
    procedure bMarkAllClick(Sender: TObject);
    procedure bRepClick(Sender: TObject);
    procedure bRepAllClick(Sender: TObject);
    procedure bCountClick(Sender: TObject);
    procedure bCancelClick(Sender: TObject);
    procedure chkRegexChange(Sender: TObject);
    procedure chkRepChange(Sender: TObject);
    procedure bFindFirstClick(Sender: TObject);
    procedure chkRepClick(Sender: TObject);
    procedure edFindChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
    FOnDone: TStrEvent;
    procedure DoDone(const Str: string);
  public
    { public declarations }
    FHotkeyFind,
    FHotkeyRep: TShortCut;
    procedure UpdateState;
    procedure UpdateFonts;
    property OnDone: TStrEvent read FOnDone write FOnDone;
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

procedure TfmFind.chkRepChange(Sender: TObject);
begin
  UpdateState;
end;

procedure TfmFind.bFindFirstClick(Sender: TObject);
begin
  DoDone(cOpFind);
end;

procedure TfmFind.chkRepClick(Sender: TObject);
begin
  UpdateState;
end;

procedure TfmFind.edFindChange(Sender: TObject);
begin
  UpdateState;
end;

procedure TfmFind.FormCreate(Sender: TObject);
begin
end;

procedure TfmFind.UpdateFonts;
begin
  with LabelFind do
  begin
    Font.Name:= UiOps.VarFontName;
    Font.Size:= UiOps.VarFontSize;
    Font.Color:= GetAppColor('TabFont');
  end;
  with edFind do
  begin
    Font.Name:= EditorOps.OpFontName;
    Font.Size:= EditorOps.OpFontSize;
    Colors.TextFont:= GetAppColor('EdTextFont');
    Colors.TextBG:= GetAppColor('EdTextBg');
    Colors.TextSelFont:= GetAppColor('EdSelFont');
    Colors.TextSelBG:= GetAppColor('EdSelBg');
    Colors.ComboboxArrow:= GetAppColor('EdComboArrow');
    Colors.ComboboxArrowBG:= GetAppColor('EdComboArrowBg');
    Colors.TextDisabledFont:= GetAppColor('EdDisableFont');
    Colors.TextDisabledBG:= GetAppColor('EdDisableBg');
  end;
  with edRep do
  begin
    Font.Name:= EditorOps.OpFontName;
    Font.Size:= EditorOps.OpFontSize;
    Colors.TextFont:= GetAppColor('EdTextFont');
    Colors.TextBG:= GetAppColor('EdTextBg');
    Colors.TextSelFont:= GetAppColor('EdSelFont');
    Colors.TextSelBG:= GetAppColor('EdSelBg');
    Colors.ComboboxArrow:= GetAppColor('EdComboArrow');
    Colors.ComboboxArrowBG:= GetAppColor('EdComboArrowBg');
    Colors.TextDisabledFont:= GetAppColor('EdDisableFont');
    Colors.TextDisabledBG:= GetAppColor('EdDisableBg');
  end;

  bCancel.Font.Assign(LabelFind.Font);
end;

procedure TfmFind.FormDestroy(Sender: TObject);
begin
  //
end;

procedure TfmFind.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if key=VK_RETURN then
  begin
    if chkRep.Checked then
      DoDone(cOpFindRep)
    else
      DoDone(cOpFind);
    key:= 0;
    exit
  end;
  if key=VK_ESCAPE then
  begin
    DoDone(cOpFindClose);
    key:= 0;
    exit;
  end;

  if (FHotkeyFind<>0) and (FHotkeyFind=KeyToShortCut(Key, Shift)) then
  begin
    chkRep.Checked:= false;
    UpdateState;
    key:= 0;
    exit;
  end;

  if (FHotkeyRep<>0) and (FHotkeyRep=KeyToShortCut(Key, Shift)) then
  begin
    chkRep.Checked:= true;
    UpdateState;
    key:= 0;
    exit;
  end;

  if (key=ord('R')) and (Shift=[ssAlt]) then
    begin with chkRegex do checked:= not checked; key:= 0; exit end;
  if (key=ord('C')) and (Shift=[ssAlt]) then
    begin with chkCase do checked:= not checked; key:= 0; exit end;
  if (key=ord('W')) and (Shift=[ssAlt]) then
    begin with chkWords do checked:= not checked; key:= 0; exit end;
  if (key=ord('9')) and (Shift=[ssAlt]) then
    begin with chkBack do checked:= not checked; key:= 0; exit end;
  if (key=ord('0')) and (Shift=[ssAlt]) then
    begin with chkConfirm do checked:= not checked; key:= 0; exit end;

  if (key=VK_F3) and (Shift=[]) then
    begin bFindNextClick(Self); key:= 0; exit end;
  if (key=ord('1')) and (Shift=[ssAlt]) then
    begin bRepClick(Self); key:= 0; exit end;
  if (key=ord('2')) and (Shift=[ssAlt]) then
    begin bRepAllClick(Self); key:= 0; exit end;
  if (key=ord('5')) and (Shift=[ssAlt]) then
    begin bCountClick(Self); key:= 0; exit end;
  if (key=ord('6')) and (Shift=[ssAlt]) then
    begin bMarkAllClick(Self); key:= 0; exit end;
end;

procedure TfmFind.FormShow(Sender: TObject);
begin
  UpdateFonts;
end;

procedure TfmFind.DoDone(const Str: string);
begin
  if Assigned(FOnDone) then
    FOnDone(Self, Str);

  if Str<>cOpFindClose then
  begin
    SAddStringToHistory(edFind.Text, edFind.Items, UiOps.MaxHistoryMenu);
    SAddStringToHistory(edRep.Text, edRep.Items, UiOps.MaxHistoryMenu);
  end;
end;


procedure TfmFind.UpdateState;
var
  rep, fill: boolean;
begin
  rep:= chkRep.Checked;
  fill:= edFind.Text<>'';

  chkWords.Enabled:= not chkRegex.Checked;
  chkBack.Enabled:= not chkRegex.Checked;
  chkConfirm.Enabled:= rep;
  edRep.Enabled:= rep;
  bCount.Visible:= not rep;
  bMarkAll.Visible:= not rep;
  bRep.Visible:= rep;
  bRepAll.Visible:= rep;

  bFindFirst.Enabled:= fill;
  bFindNext.Enabled:= fill;
  bRep.Enabled:= fill;
  bRepAll.Enabled:= fill;
  bCount.Enabled:= fill;
  bMarkAll.Enabled:= fill;
end;

end.

