(*
This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at http://mozilla.org/MPL/2.0/.

Copyright (c) Alexey Torgashin
*)
unit form_tabsplit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Spin,
  ComCtrls, FormFrame,
  proc_customdialog;

type
  { TfmTabSplit }

  TfmTabSplit = class(TForm)
    btnClose: TButton;
    btnNoSplit: TRadioButton;
    btnHorz: TRadioButton;
    btnVert: TRadioButton;
    barValue: TTrackBar;
    procedure barValueChange(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure btnHorzChange(Sender: TObject);
    procedure btnNoSplitChange(Sender: TObject);
    procedure btnVertChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    Splitted: boolean;
    SplitHorz: boolean;
    SplitPercent: integer;
    procedure DoChanged;
  public
    WorkFrame: TEditorFrame;
  end;

implementation

{$R *.lfm}

{ TfmTabSplit }

procedure TfmTabSplit.btnCloseClick(Sender: TObject);
begin
  ModalResult:= mrOk;
end;

procedure TfmTabSplit.barValueChange(Sender: TObject);
begin
  SplitPercent:= barValue.Position;
  DoChanged;
end;

procedure TfmTabSplit.btnNoSplitChange(Sender: TObject);
begin
  Splitted:= false;
  DoChanged;
end;

procedure TfmTabSplit.btnHorzChange(Sender: TObject);
begin
  Splitted:= true;
  SplitHorz:= true;
  DoChanged;
end;

procedure TfmTabSplit.btnVertChange(Sender: TObject);
begin
  Splitted:= true;
  SplitHorz:= false;
  DoChanged;
end;

procedure TfmTabSplit.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction:= caFree;
end;

procedure TfmTabSplit.FormCreate(Sender: TObject);
begin
  DoForm_ScaleAuto(Self, true);
end;

procedure TfmTabSplit.FormShow(Sender: TObject);
begin
  if Application.MainForm.FormStyle=fsStayOnTop then
    FormStyle:= fsStayOnTop;

  Splitted:= WorkFrame.Splitted;
  SplitHorz:= WorkFrame.SplitHorz;
  SplitPercent:= round(WorkFrame.SplitPos*100);

  btnNoSplit.Checked:= not Splitted;
  btnHorz.Checked:= Splitted and SplitHorz;
  btnVert.Checked:= Splitted and not SplitHorz;
  barValue.Position:= 100-SplitPercent;
end;

procedure TfmTabSplit.DoChanged;
begin
  WorkFrame.Splitted:= Splitted;
  if WorkFrame.Splitted then
  begin
    WorkFrame.SplitHorz:= SplitHorz;
    WorkFrame.SplitPos:= (100-SplitPercent)/100;
  end;
end;

end.

