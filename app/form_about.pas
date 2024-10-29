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

const
  CreditsArray: array of string = (
    '#Lazarus IDE',
    'http://www.lazarus-ide.org',
    '',
    '#Application, ATSynEdit, ATFlatControls',
    'Alexey Torgashin',
    'https://github.com/Alexey-T',
    '',
    '#EControl syntax parser',
    'Delphi version by Michael Zakharov',
    'http://www.econtrol.ru',
    'Lazarus port and rework by Alexey Torgashin',
    'https://github.com/Alexey-T',
    '',
    '#Python plugins, QA',
    'Andrey Kvichanskiy',
    'https://github.com/kvichans',
    'Oleh Lutsak',
    'https://github.com/OlehL',
    'Shovel',
    'https://github.com/halfbrained',
    'Ildar R. Khasanshin',
    'https://github.com/ildarkhasanshin',
    'Yuriy Balyuk',
    'https://github.com/veksha',
    '',
    '#Optimizations of Python wrapper, plugins',
    'Artem Gavrilov',
    'https://github.com/Artem3213212',
    '',
    '#Patches, Windows shell extension, plugins',
    'Andreas Heim',
    'https://github.com/dinkumoil',
    '',
    '————————————————————————',
    '',
    '#Python for Delphi',
    'Dietmar Budelsky, Morgan Martinet, Kiriakos Vlahos',
    'https://github.com/pyscripter/python4delphi',
    'Adaptation for Lazarus by Alexey Torgashin',
    '',
    '#BGRABitmap',
    'https://bgrabitmap.github.io',
    '',
    '#Emmet for Pascal',
    'Rickard Johansson',
    'https://rj-texted.se',
    '',
    '————————————————————————',
    '',
    '#Main icon',
    'FTurtle',
    '',
    '#Theme for LibreOffice',
    'https://github.com/libodesign/icons',
    'License: Creative Commons BY-SA 3.0',
    'http://creativecommons.org/licenses/by-sa/3.0/',
    '',
    '#Octicons',
    'https://octicons.github.com/',
    'License: MIT License',
    '',
    '#Visual Studio Code icons',
    'https://github.com/vscode-icons/vscode-icons',
    'License: MIT License',
    '',
    '#Hourglass/floppy icons',
    'https://www.iconfinder.com/snipicons',
    'License: Creative Commons BY-NC 3.0 Unported',
    'http://creativecommons.org/licenses/by-nc/3.0/',
    '');

procedure TfmAbout.FormCreate(Sender: TObject);
var
  S: string;
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
  for S in CreditsArray do
    Credits.Lines.Add(S);

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

