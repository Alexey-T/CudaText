(*
This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at http://mozilla.org/MPL/2.0/.

Copyright (c) Alexey Torgashin
*)
unit formconfirmrep;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  LclType,
  ATButtons,
  proc_globdata,
  proc_colors;

type
  { TfmConfirmReplace }

  TfmConfirmReplace = class(TForm)
    bYesAll: TATButton;
    bYes: TATButton;
    bNo: TATButton;
    bNoAll: TATButton;
    LabelInfo: TLabel;
    procedure bNoAllClick(Sender: TObject);
    procedure bNoClick(Sender: TObject);
    procedure bYesAllClick(Sender: TObject);
    procedure bYesClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  fmConfirmReplace: TfmConfirmReplace;

implementation

{$R *.lfm}

{ TfmConfirmReplace }

procedure TfmConfirmReplace.FormCreate(Sender: TObject);
begin
  LabelInfo.Font.Name:= UiOps.VarFontName;
  LabelInfo.Font.Size:= UiOps.VarFontSize;
end;

procedure TfmConfirmReplace.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key=VK_Y then begin ModalResult:= mrYes; exit end;
  if Key=VK_A then begin ModalResult:= mrYesToAll; exit end;
  if Key=VK_N then begin ModalResult:= mrNo; exit end;
  if Key=VK_ESCAPE then begin ModalResult:= mrNoToAll; exit end;
end;

procedure TfmConfirmReplace.bYesClick(Sender: TObject);
begin
  ModalResult:= mrYes;
end;

procedure TfmConfirmReplace.bNoClick(Sender: TObject);
begin
  ModalResult:= mrNo;
end;

procedure TfmConfirmReplace.bNoAllClick(Sender: TObject);
begin
  ModalResult:= mrNoToAll;
end;

procedure TfmConfirmReplace.bYesAllClick(Sender: TObject);
begin
  ModalResult:= mrYesToAll;
end;

end.

