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
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Menus, ButtonPanel, IniFiles,
  LCLProc, LCLType, LCLIntf, ScrollingText,
  proc_msg,
  proc_globdata,
  proc_editor,
  proc_customdialog,
  ATLinkLabel,
  ATSynEdit,
  ATSynEdit_Commands;

type
  { TfmAbout }

  TfmAbout = class(TForm)
    ButtonPanel1: TButtonPanel;
    labelName: TLabel;
    labelPlatform: TLabel;
    labelVersion: TLabel;
    MenuItem37: TMenuItem;
    mnuTextCopy: TMenuItem;
    mnuTextOpenUrl: TMenuItem;
    mnuTextSel: TMenuItem;
    PopupText: TPopupMenu;
    Credits: TScrollingText;
    procedure bCreditsClick(Sender: TObject);
    procedure bOkClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
    procedure Localize;
  public
    { public declarations }
    EditorOnClickLink: TATSynEditClickLinkEvent;
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
  fn:= GetAppLangFilename;
  if not FileExists(fn) then exit;
  ini:= TIniFile.Create(fn);
  try
    Caption:= ini.ReadString(section, '_', Caption);
    with ButtonPanel1.OKButton do Caption:= msgButtonOk;
    with ButtonPanel1.HelpButton do Caption:= ini.ReadString(section, 'cre', Caption);

    with mnuTextCopy do Caption:= ini.ReadString('m_e', 'cp', Caption);
    with mnuTextSel do Caption:= ini.ReadString('m_se', 'al', Caption);
    with mnuTextOpenUrl do Caption:= ini.ReadString('ct', 'url', Caption);
  finally
    FreeAndNil(ini);
  end;
end;

procedure TfmAbout.bOkClick(Sender: TObject);
begin
  ModalResult:= mrCancel;
end;

procedure TfmAbout.FormCreate(Sender: TObject);
begin
  Localize;

  FLabelLink:= TATLabelLink.Create(Self);
  FLabelLink.Parent:= Self;
  FLabelLink.Caption:= 'UVviewsoft.com';
  FLabelLink.Link:= 'http://uvviewsoft.com';
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

  Credits.Hide;
  Credits.Align:= alClient;
end;

procedure TfmAbout.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key=VK_ESCAPE then Close;
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

