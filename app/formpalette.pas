(*
This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at http://mozilla.org/MPL/2.0/.

Copyright (c) Alexey Torgashin
*)
unit formpalette;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ButtonPanel,
  LazUTF8, LazFileUtils,
  IniFiles, ColorPalette,
  proc_globdata,
  proc_msg;

type
  { TfmPalette }

  TfmPalette = class(TForm)
    ButtonPanel1: TButtonPanel;
    Pal: TColorPalette;
    procedure CloseButtonClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure PalColorPick(Sender: TObject; AColor: TColor; Shift: TShiftState);
  private
    { private declarations }
  public
    { public declarations }
    ResColor: TColor;
  end;

var
  fmPalette: TfmPalette;

procedure DoLocalize_FormPalette(F: TfmPalette);


implementation

{$R *.lfm}

procedure DoLocalize_FormPalette(F: TfmPalette);
const
  section = 'd_tab_color';
var
  ini: TIniFile;
  fn: string;
begin
  fn:= GetAppLangFilename;
  if not FileExists(fn) then exit;
  ini:= TIniFile.Create(fn);
  try
    with F do Caption:= ini.ReadString(section, '_', Caption);
    with F.ButtonPanel1.CloseButton do Caption:= ini.ReadString(section, 'res', Caption);
    with F.ButtonPanel1.CancelButton do Caption:= msgButtonCancel;
  finally
    FreeAndNil(ini);
  end;
end;


{ TfmPalette }

procedure TfmPalette.FormShow(Sender: TObject);
var
  i: integer;
begin
  Pal.PickedIndex:= -1;
  for i:= 0 to Pal.ColorCount-1 do
    if Pal.Colors[i]=ResColor then
    begin
      Pal.PickedIndex:= i;
      Break
    end;
end;

procedure TfmPalette.CloseButtonClick(Sender: TObject);
begin
  ResColor:= clNone;
  Modalresult:= mrNo;
end;

procedure TfmPalette.PalColorPick(Sender: TObject; AColor: TColor;
  Shift: TShiftState);
begin
  ResColor:= AColor;
  Modalresult:= mrOk;
end;

end.

