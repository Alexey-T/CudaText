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
  public
    { public declarations }
    MsgReplaceMatch: string;
    MsgLineNumber: integer;
  end;

var
  fmConfirmReplace: TfmConfirmReplace;

procedure DoLocalize_FormConfirmReplace(F: TfmConfirmReplace);


implementation

{$R *.lfm}

procedure DoLocalize_FormConfirmReplace(F: TfmConfirmReplace);
const
  section = 'd_cfm_rep';
var
  ini: TIniFile;
  fn: string;
begin
  fn:= GetAppLangFilename;
  if not FileExists(fn) then exit;
  ini:= TIniFile.Create(fn);
  try
    with F do Caption:= ini.ReadString(section, '_', Caption);
    with F.bYes do Caption:= ini.ReadString(section, 'yes', Caption);
    with F.bYesAll do Caption:= ini.ReadString(section, 'yes_a', Caption);
    with F.bNo do Caption:= ini.ReadString(section, 'no', Caption);
    with F.bNoAll do Caption:= ini.ReadString(section, 'no_a', Caption);
    F.MsgReplaceMatch:= ini.ReadString(section, 'msg', F.MsgReplaceMatch);
  finally
    FreeAndNil(ini);
  end;
end;


{ TfmConfirmReplace }

procedure TfmConfirmReplace.FormCreate(Sender: TObject);
begin
  MsgReplaceMatch:= 'Replace match at line %d ?';
  MsgLineNumber:= 0;

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

procedure TfmConfirmReplace.FormShow(Sender: TObject);
begin
  LabelInfo.Caption:= Format(MsgReplaceMatch, [MsgLineNumber]);
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

