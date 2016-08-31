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
  LazUTF8, LazFileUtils,
  proc_globdata,
  proc_msg;

type
  { TfmSaveTabs }

  TfmSaveTabs = class(TForm)
    btnDontSaveKeep: TButton;
    btnSave: TButton;
    btnDontSave: TButton;
    btnCancel: TButton;
    List: TCheckListBox;
    Panel1: TPanel;
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
  with List do
    if Items.Count>0 then
      ItemIndex:= 0;

  //btnDontSave.Visible:= not UiOps.ShowLastFiles;
  btnDontSaveKeep.Visible:= UiOps.ShowLastFiles;
end;

end.

