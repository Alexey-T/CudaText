(*
This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at http://mozilla.org/MPL/2.0/.

Copyright (c) Alexey Torgashin
*)
unit form_rename_file;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ButtonPanel, StdCtrls,
  StrUtils, IniFiles,
  proc_globdata,
  proc_msg;

type

  { TfmRenameFile }

  TfmRenameFile = class(TForm)
    ButtonPanel1: TButtonPanel;
    EditExt: TEdit;
    EditName: TEdit;
    LabelPrompt: TLabel;
    procedure EditNameChange(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormShow(Sender: TObject);
  private
    OldPath: string;
    procedure Localize;
  public
    OldFileName: string;
    function NewFileName: string;

  end;

function DoDialogRenameFile(const AOldFileName: string; out ANewFileName: string): boolean;

implementation

{$R *.lfm}

function DoDialogRenameFile(const AOldFileName: string; out ANewFileName: string): boolean;
var
  Dlg: TfmRenameFile;
begin
  Result:= false;
  ANewFileName:= '';

  Dlg:= TfmRenameFile.Create(nil);
  try
    Dlg.OldFileName:= AOldFileName;
    if Dlg.ShowModal=mrOk then
    begin
      ANewFileName:= Dlg.NewFileName;
      Result:= true;
    end;
  finally
    FreeAndNil(Dlg);
  end;
end;

{ TfmRenameFile }

procedure TfmRenameFile.FormShow(Sender: TObject);
begin
  Localize;

  EditName.Text:= ChangeFileExt(ExtractFileName(OldFileName), '');
  EditExt.Text:= Copy(ExtractFileExt(OldFileName), 2, MaxInt);

  OldPath:= ExtractFilePath(OldFileName);

  if Assigned(EditName.OnChange) then
    EditName.OnChange(nil);
end;

procedure TfmRenameFile.Localize;
var
  ini: TIniFile;
  fn: string;
begin
  fn:= AppFile_Language;
  if not FileExists(fn) then exit;
  ini:= TIniFile.Create(fn);
  try
    Caption:= StringReplace(msgFileRename, '...', '', []);
    LabelPrompt.Caption:= ini.ReadString('d_about', 'ren_to', Caption);
    with ButtonPanel1.OKButton do Caption:= msgButtonOk;
    with ButtonPanel1.CancelButton do Caption:= msgButtonCancel;
  finally
    FreeAndNil(ini);
  end;
end;


function TfmRenameFile.NewFileName: string;
begin
  Result:=
    OldPath+
    EditName.Text+
    IfThen(EditExt.Text<>'', '.'+EditExt.Text);
end;

procedure TfmRenameFile.EditNameChange(Sender: TObject);
begin
  ButtonPanel1.OKButton.Enabled:=
    (EditName.Text<>'') and
    (PosSet(['/', '\', '*', ':'], EditName.Text+'.'+EditExt.Text)=0) and
    not SameFileName(NewFileName, OldFileName);
end;

procedure TfmRenameFile.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if ModalResult=mrOk then
    if not RenameFile(OldFileName, NewFileName) then
    begin
      MessageDlg('CudaText', 'Cannot rename file to:'+#10+NewFileName, mtError, [mbOK], 0);
      CanClose:= false;
    end;
end;

end.

