(*
This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at http://mozilla.org/MPL/2.0/.

Copyright (c) Alexey Torgashin
*)
unit formkeyinput;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics,
  ButtonPanel, ExtCtrls, Menus, IniFiles,
  LCLProc, LCLType,
  proc_globdata,
  proc_customdialog,
  proc_msg;

type
  { TfmKeyInput }

  TfmKeyInput = class(TForm)
    ButtonPanel1: TButtonPanel;
    PanelPress: TPanel;
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
    procedure Localize;
  public
    { public declarations }
    FHotkey: string;
  end;

function DoDialogHotkeyInput(ATitle: string): string;


implementation

{$R *.lfm}

function DoDialogHotkeyInput(ATitle: string): string;
var
  Form: TfmKeyInput;
begin
  Result:= '';
  Form:= TfmKeyInput.Create(nil);
  try
    if ATitle<>'' then
      Form.Caption:= ATitle;
    if Form.ShowModal=mrOk then
      Result:= Form.FHotkey;
  finally
    FreeAndNil(Form);
  end;
end;

{ TfmKeyInput }

procedure TfmKeyInput.Localize;
const
  section = 'd_keys';
var
  ini: TIniFile;
  fn: string;
begin
  fn:= GetAppLangFilename;
  if not FileExists(fn) then exit;
  ini:= TIniFile.Create(fn);
  try
    Caption:= ini.ReadString(section, '_', Caption);
    with ButtonPanel1.OKButton do Caption:= msgButtonOk;
    with ButtonPanel1.CancelButton do Caption:= msgButtonCancel;
    with PanelPress do Caption:= ini.ReadString(section, 'wait', Caption);
  finally
    FreeAndNil(ini);
  end;
end;

procedure TfmKeyInput.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if not AppKeyIsAllowedAsCustomHotkey(Key, Shift) then
  begin
    Key:= 0;
    exit
  end;

  if (Key=VK_ESCAPE) and (Shift=[]) then
  begin
    Key:= 0;
    ModalResult:= mrCancel;
    exit
  end;

  FHotkey:= ShortCutToText(ShortCut(Key, Shift));
  ModalResult:= mrOk;
  Key:= 0;
end;

procedure TfmKeyInput.FormShow(Sender: TObject);
begin
  Localize;
  DoForm_ScaleAuto(Self);
  UpdateFormOnTop(Self);
end;

end.

