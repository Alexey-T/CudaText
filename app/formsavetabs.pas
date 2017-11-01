(*
This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at http://mozilla.org/MPL/2.0/.

Copyright (c) Alexey Torgashin
*)
unit formsavetabs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs,
  IniFiles, CheckLst, ExtCtrls, StdCtrls, StrUtils,
  LazUTF8, LazFileUtils, LCLType,
  ATPanelSimple,
  proc_globdata,
  proc_msg;

type
  { TfmSaveTabs }

  TfmSaveTabs = class(TForm)
    btnCancel: TButton;
    btnDontSave: TButton;
    btnDontSaveKeep: TButton;
    btnSave: TButton;
    List: TCheckListBox;
    Panel1: TATPanelSimple;
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  fmSaveTabs: TfmSaveTabs;

procedure DoLocalize_FormSaveTabs(F: TfmSaveTabs);


implementation

{$R *.lfm}

procedure DoLocalize_FormSaveTabs(F: TfmSaveTabs);
const
  section = 'd_save_tabs';
var
  ini: TIniFile;
  fn: string;
begin
  fn:= GetAppLangFilename;
  if not FileExists(fn) then exit;
  ini:= TIniFile.Create(fn);
  try
    with F do Caption:= ini.ReadString(section, '_', Caption);
    with F.btnSave do Caption:= ini.ReadString(section, 'sav', Caption);
    with F.btnDontSave do Caption:= ini.ReadString(section, 'no', Caption);
    with F.btnDontSaveKeep do Caption:= ini.ReadString(section, 'no_ses', Caption);
    with F.btnCancel do Caption:= msgButtonCancel;
  finally
    FreeAndNil(ini);
  end;
end;


{ TfmSaveTabs }

procedure TfmSaveTabs.FormShow(Sender: TObject);
begin
  UpdateFormOnTop(Self);

  with List do
    if Items.Count>0 then
      ItemIndex:= 0;

  //btnDontSave.Visible:= not UiOps.ShowLastFiles;
  btnDontSaveKeep.Visible:= UiOps.ShowLastFiles;
end;

procedure TfmSaveTabs.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  C: TWinControl;
begin
  //workaround for LCL bug: Enter press on focused buttons [Dont save], [Cancel] dont work
  if (Key=VK_RETURN) and (Shift=[]) then
  begin
    C:= ActiveControl;
    if C is TButton then
      (C as TButton).Click;
    key:= 0;
    exit
  end;
end;

end.

