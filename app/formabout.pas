(*
This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at http://mozilla.org/MPL/2.0/.

Copyright (c) Alexey Torgashin
*)
unit formabout;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, StdCtrls,
  ButtonPanel, IniFiles,
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
    procedure bCreditsClick(Sender: TObject);
    procedure bOkClick(Sender: TObject);
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

procedure TfmAbout.FormCreate(Sender: TObject);
var
  fn: string;
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

  fn:= AppDir_DataLang+DirectorySeparator+'credits.txt';
  if FileExists(fn) then
    Credits.Lines.LoadFromFile(fn)
  else
  begin
    Credits.Lines.Clear;
    Credits.Lines.Add(msgCannotFindFile);
    Credits.Lines.Add(fn);
  end;
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
begin
  DoForm_ScaleAuto(Self, true);
  UpdateFormOnTop(Self);

  //big title
  labelName.Font.Style:= [fsBold];
  labelName.Font.Size:= 20;

  with Credits do
  begin
    LinkFont.Color:= clBlue;
    LinkFont.Style:= [fsUnderline];
  end;
end;

procedure TfmAbout.bCreditsClick(Sender: TObject);
begin
  labelName.Hide;
  labelVersion.Hide;
  labelPlatform.Hide;
  FLabelLink.Hide;
  ButtonPanel1.HelpButton.Enabled:= false;

  Credits.Show;
  Credits.Active:= true;
end;

end.

