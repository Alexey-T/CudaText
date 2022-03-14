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
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  LclType, IniFiles,
  LazUTF8, LazFileUtils,
  ATButtons,
  proc_globdata,
  proc_miscutils,
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
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
    procedure Localize;
    procedure CloseWithResult(AValue: TModalResult);
  public
    { public declarations }
    MsgReplaceMatch: string;
    MsgLineNumber: integer;
  end;

var
  fmConfirmReplace: TfmConfirmReplace = nil;

implementation

{$R *.lfm}

{ TfmConfirmReplace }

procedure TfmConfirmReplace.Localize;
const
  section = 'd_cfm_rep';
var
  ini: TIniFile;
  fn: string;
begin
  fn:= AppFile_Language;
  if not FileExists(fn) then exit;
  ini:= TIniFile.Create(fn);
  try
    Caption:= ini.ReadString(section, '_', Caption);
    with bYes do Caption:= ini.ReadString(section, 'yes', Caption);
    with bYesAll do Caption:= ini.ReadString(section, 'yes_a', Caption);
    with bNo do Caption:= ini.ReadString(section, 'no', Caption);
    with bNoAll do Caption:= ini.ReadString(section, 'no_a', Caption);
    MsgReplaceMatch:= ini.ReadString(section, 'msg', MsgReplaceMatch);
  finally
    FreeAndNil(ini);
  end;
end;

procedure TfmConfirmReplace.CloseWithResult(AValue: TModalResult);
begin
  ModalResult:= AValue;
end;

procedure TfmConfirmReplace.FormCreate(Sender: TObject);
begin
  MsgReplaceMatch:= 'Replace match at line %d ?';
  MsgLineNumber:= 0;
  Localize;

  LabelInfo.Font.Name:= UiOps.VarFontName;
  LabelInfo.Font.Size:= UiOps.VarFontSize;

  AppScalePanelControls(Self);
end;

procedure TfmConfirmReplace.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key=VK_Y then
  begin
    CloseWithResult(mrYes);
    Key:= 0;
    exit
  end;

  if Key=VK_A then
  begin
    CloseWithResult(mrYesToAll);
    Key:= 0;
    exit
  end;

  if Key=VK_N then
  begin
    CloseWithResult(mrNo);
    Key:= 0;
    exit
  end;

  if (Key=VK_ESCAPE) and (Shift=[]) then
  begin
    CloseWithResult(mrNoToAll);
    Key:= 0;
    exit
  end;
end;

procedure TfmConfirmReplace.FormShow(Sender: TObject);
begin
  UpdateFormOnTop(Self);

  LabelInfo.Caption:= Format(MsgReplaceMatch, [MsgLineNumber]);

  Self.ClientWidth:= bNoAll.Left+bNoAll.Width+8;
  Self.ClientHeight:= bYes.Top+bYes.Height+8;
end;

procedure TfmConfirmReplace.bYesClick(Sender: TObject);
begin
  CloseWithResult(mrYes);
end;

procedure TfmConfirmReplace.bNoClick(Sender: TObject);
begin
  CloseWithResult(mrNo);
end;

procedure TfmConfirmReplace.bNoAllClick(Sender: TObject);
begin
  CloseWithResult(mrNoToAll);
end;

procedure TfmConfirmReplace.bYesAllClick(Sender: TObject);
begin
  CloseWithResult(mrYesToAll);
end;

end.

