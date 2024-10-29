(*
This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at http://mozilla.org/MPL/2.0/.

Copyright (c) Alexey Torgashin
*)
unit form_about;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, StdCtrls,
  Buttons, ButtonPanel,
  IniFiles, Clipbrd,
  LCLProc, LCLType, LCLIntf,
  ScrollingText,
  proc_msg,
  proc_globdata,
  proc_customdialog,
  ATLinkLabel;

type
  { TfmAbout }

  TfmAbout = class(TForm)
    ButtonPanel1: TButtonPanel;
    labelName: TLabel;
    labelPlatform: TLabel;
    labelVersion: TLabel;
    btnCopyToClp: TSpeedButton;
    procedure bCreditsClick(Sender: TObject);
    procedure bOkClick(Sender: TObject);
    procedure btnCopyToClpClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
    Credits: TScrollingText;
    procedure Localize;
  public
    { public declarations }
    FLabelLink: TATLabelLink;
  end;

implementation

uses InterfaceBase;

{$R *.lfm}

{ TfmAbout }

procedure TfmAbout.Localize;
const
  section = 'd_about';
var
  ini: TIniFile;
  fn: string;
begin
  fn:= AppFile_Language;
  if not FileExists(fn) then exit;
  ini:= TIniFile.Create(fn);
  try
    Caption:= ini.ReadString(section, '_', Caption);
    with ButtonPanel1.OKButton do Caption:= msgButtonOk;
    with ButtonPanel1.HelpButton do Caption:= ini.ReadString(section, 'cre', Caption);
  finally
    FreeAndNil(ini);
  end;
end;

procedure TfmAbout.bOkClick(Sender: TObject);
begin
  ModalResult:= mrCancel;
end;

procedure TfmAbout.btnCopyToClpClick(Sender: TObject);
begin
  Clipboard.AsText:= 'CudaText '+labelVersion.Caption+', '+labelPlatform.Caption;
end;

procedure TfmAbout.FormCreate(Sender: TObject);
begin
  Localize;

  FLabelLink:= TATLabelLink.Create(Self);
  FLabelLink.Parent:= Self;
  FLabelLink.Caption:= 'CudaText.github.io';
  FLabelLink.Link:= 'https://cudatext.github.io/';
  FLabelLink.Left:= labelName.Left;
  FLabelLink.AnchorSideTop.Control:= labelPlatform;
  FLabelLink.AnchorSideTop.Side:= asrBottom;
  FLabelLink.BorderSpacing.Top:= labelPlatform.BorderSpacing.Top;

  labelPlatform.Caption:= Format('%s-%s-%s, fpc %s', [
    LowerCase({$I %FPCTARGETOS%}),
    {$I %FPCTARGETCPU%},
    GetLCLWidgetTypeName,
    {$I %FPCVersion%}
    ]);

  Credits:= TScrollingText.Create(Self);
  Credits.Hide;
  Credits.Parent:= Self;
  Credits.Align:= alClient;
  Credits.Font.Color:= clWindowText;
  Credits.LinkFont.Color:= FLabelLink.ColorLinkNormal;
  Credits.LinkFont.Style:= [fsUnderline];

  Credits.Lines.Clear;
  Credits.Lines.Add('#Lazarus IDE');
  Credits.Lines.Add('http://www.lazarus-ide.org');
  Credits.Lines.Add('');
  Credits.Lines.Add('#Application, ATSynEdit, ATFlatControls');
  Credits.Lines.Add('Alexey Torgashin');
  Credits.Lines.Add('https://github.com/Alexey-T');
  Credits.Lines.Add('');
  Credits.Lines.Add('#EControl syntax parser');
  Credits.Lines.Add('Delphi version by Michael Zakharov');
  Credits.Lines.Add('http://www.econtrol.ru');
  Credits.Lines.Add('Lazarus port and rework by Alexey Torgashin');
  Credits.Lines.Add('https://github.com/Alexey-T');
  Credits.Lines.Add('');
  Credits.Lines.Add('#Python plugins, QA');
  Credits.Lines.Add('Andrey Kvichanskiy');
  Credits.Lines.Add('https://github.com/kvichans');
  Credits.Lines.Add('Oleh Lutsak');
  Credits.Lines.Add('https://github.com/OlehL');
  Credits.Lines.Add('Shovel');
  Credits.Lines.Add('https://github.com/halfbrained');
  Credits.Lines.Add('Ildar R. Khasanshin');
  Credits.Lines.Add('https://github.com/ildarkhasanshin');
  Credits.Lines.Add('Yuriy Balyuk');
  Credits.Lines.Add('https://github.com/veksha');
  Credits.Lines.Add('');
  Credits.Lines.Add('#Optimizations of Python wrapper, plugins');
  Credits.Lines.Add('Artem Gavrilov');
  Credits.Lines.Add('https://github.com/Artem3213212');
  Credits.Lines.Add('');
  Credits.Lines.Add('#Patches, Windows shell extension, plugins');
  Credits.Lines.Add('Andreas Heim');
  Credits.Lines.Add('https://github.com/dinkumoil');
  Credits.Lines.Add('');
  Credits.Lines.Add('————————————————————————');
  Credits.Lines.Add('');
  Credits.Lines.Add('#Python for Delphi');
  Credits.Lines.Add('Dietmar Budelsky, Morgan Martinet, Kiriakos Vlahos');
  Credits.Lines.Add('https://github.com/pyscripter/python4delphi');
  Credits.Lines.Add('Adaptation for Lazarus by Alexey Torgashin');
  Credits.Lines.Add('');
  Credits.Lines.Add('#BGRABitmap');
  Credits.Lines.Add('https://bgrabitmap.github.io');
  Credits.Lines.Add('');
  Credits.Lines.Add('#Emmet for Pascal');
  Credits.Lines.Add('Rickard Johansson');
  Credits.Lines.Add('https://rj-texted.se');
  Credits.Lines.Add('');
  Credits.Lines.Add('————————————————————————');
  Credits.Lines.Add('');
  Credits.Lines.Add('#Main icon');
  Credits.Lines.Add('FTurtle');
  Credits.Lines.Add('');
  Credits.Lines.Add('#Theme for LibreOffice');
  Credits.Lines.Add('https://github.com/libodesign/icons');
  Credits.Lines.Add('License: Creative Commons BY-SA 3.0');
  Credits.Lines.Add('http://creativecommons.org/licenses/by-sa/3.0/');
  Credits.Lines.Add('');
  Credits.Lines.Add('#Octicons');
  Credits.Lines.Add('https://octicons.github.com/');
  Credits.Lines.Add('License: MIT License');
  Credits.Lines.Add('');
  Credits.Lines.Add('#Visual Studio Code icons');
  Credits.Lines.Add('https://github.com/vscode-icons/vscode-icons');
  Credits.Lines.Add('License: MIT License');
  Credits.Lines.Add('');
  Credits.Lines.Add('#Hourglass/floppy icons');
  Credits.Lines.Add('https://www.iconfinder.com/snipicons');
  Credits.Lines.Add('License: Creative Commons BY-NC 3.0 Unported');
  Credits.Lines.Add('http://creativecommons.org/licenses/by-nc/3.0/');

  //big title
  labelName.Font.Style:= [fsBold];
  labelName.Font.Size:= 20;
end;

procedure TfmAbout.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key=VK_ESCAPE) and (Shift=[]) then
  begin
    Close;
    Key:= 0;
    exit;
  end;
end;


procedure TfmAbout.FormShow(Sender: TObject);
var
  fnIcon: string;
  bitmap: TPortableNetworkGraphic;
begin
  DoForm_ScaleAuto(Self, true);
  UpdateFormOnTop(Self);

  fnIcon:= AppDir_DataToolbarIcons+DirectorySeparator+'default_24x24'+DirectorySeparator+'e_copy.png';
  if FileExists(fnIcon) then
  begin
    bitmap:= TPortableNetworkGraphic.Create;
    try
      bitmap.LoadFromFile(fnIcon);
      btnCopyToClp.Glyph.Assign(bitmap);
    finally
      bitmap.Free;
    end;
  end;

  btnCopyToClp.Hint:= msgCopySub;
  btnCopyToClp.ShowHint:= true;
end;

procedure TfmAbout.bCreditsClick(Sender: TObject);
begin
  labelName.Hide;
  labelVersion.Hide;
  labelPlatform.Hide;
  FLabelLink.Hide;
  ButtonPanel1.HelpButton.Enabled:= false;
  btnCopyToClp.Hide;

  Credits.Show;
  Credits.Active:= true;
end;

end.

