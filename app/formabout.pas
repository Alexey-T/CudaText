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
  LCLProc, LCLType, LCLIntf,
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
    LabelName: TLabel;
    labelPlatform: TLabel;
    labelVersion: TLabel;
    memo: TATSynEdit;
    MenuItem37: TMenuItem;
    mnuTextCopy: TMenuItem;
    mnuTextOpenUrl: TMenuItem;
    mnuTextSel: TMenuItem;
    PopupText: TPopupMenu;
    procedure bCreditsClick(Sender: TObject);
    procedure bOkClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure mnuTextCopyClick(Sender: TObject);
    procedure mnuTextOpenUrlClick(Sender: TObject);
    procedure mnuTextSelClick(Sender: TObject);
  private
    { private declarations }
    procedure Localize;
  public
    { public declarations }
    EditorOnClickLink: TATSynEditClickLinkEvent;
    FLabelLink: TATLabelLink;
    FCredits: string;
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

  memo.DoubleBuffered:= UiOps.DoubleBuffered;
  memo.Font.Name:= EditorOps.OpFontName;
  memo.Font.Size:= EditorOps.OpFontSize;
  memo.PopupText:= PopupText;
  memo.OptMouseClickOpensURL:= true;

  FLabelLink:= TATLabelLink.Create(Self);
  FLabelLink.Parent:= Self;
  FLabelLink.Caption:= 'UVviewsoft.com';
  FLabelLink.Link:= 'http://uvviewsoft.com';
  FLabelLink.Left:= LabelName.Left;
  FLabelLink.AnchorSideTop.Control:= labelPlatform;
  FLabelLink.AnchorSideTop.Side:= asrBottom;
  FLabelLink.BorderSpacing.Top:= labelPlatform.BorderSpacing.Top;

  labelPlatform.Caption:= Format('%s-%s-%s, fpc %s', [
    LowerCase({$I %FPCTARGETOS%}),
    {$I %FPCTARGETCPU%},
    GetLCLWidgetTypeName,
    {$I %FPCVersion%}
    ]);
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

  memo.Hide;
  memo.Align:= alClient;
  memo.Strings.Clear;
  memo.Strings.LoadFromString(msgAboutCredits);
  memo.DoCaretSingle(0, 0);
  memo.ModeReadOnly:= true;
  memo.Font.Name:= EditorOps.OpFontName;
  memo.Font.Size:= EditorOps.OpFontSize-2;
  memo.OnClickLink:= EditorOnClickLink;
end;

procedure TfmAbout.mnuTextCopyClick(Sender: TObject);
begin
  memo.DoCommand(cCommand_ClipboardCopy);
end;

procedure TfmAbout.mnuTextOpenUrlClick(Sender: TObject);
var
  Str: string;
begin
  Str:= EditorGetLinkAtScreenCoord(memo, PopupText.PopupPoint);
  if Str<>'' then
    OpenURL(Str);
end;

procedure TfmAbout.mnuTextSelClick(Sender: TObject);
begin
  memo.DoCommand(cCommand_SelectAll);
end;

procedure TfmAbout.bCreditsClick(Sender: TObject);
begin
  memo.Show;
  ButtonPanel1.HelpButton.Enabled:= false;
end;

end.

