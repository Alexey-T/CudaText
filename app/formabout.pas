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
  LCLProc, LCLType, LCLIntf, LCLPlatformDef,
  proc_msg,
  proc_globdata,
  proc_editor,
  ATLinkLabel,
  ATSynEdit,
  ATSynEdit_Commands;

type
  { TfmAbout }

  TfmAbout = class(TForm)
    ButtonPanel1: TButtonPanel;
    LabelName: TLabel;
    labelInf: TLabel;
    labelVer: TLabel;
    memo: TATSynEdit;
    MenuItem37: TMenuItem;
    mnuTextCopy: TMenuItem;
    mnuTextOpenUrl: TMenuItem;
    mnuTextSel: TMenuItem;
    PopupText: TPopupMenu;
    procedure bCreditsClick(Sender: TObject);
    procedure bOkClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure mnuTextCopyClick(Sender: TObject);
    procedure mnuTextOpenUrlClick(Sender: TObject);
    procedure mnuTextSelClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    FLabelLink: TLinkLabel;
    FCredits: string;
  end;

procedure DoLocalize_FormAbout(F: TfmAbout);


implementation

uses InterfaceBase;

{$R *.lfm}

procedure DoLocalize_FormAbout(F: TfmAbout);
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
    with F do Caption:= ini.ReadString(section, '_', Caption);
    with F.ButtonPanel1.OKButton do Caption:= msgButtonOk;
    with F.ButtonPanel1.HelpButton do Caption:= ini.ReadString(section, 'cre', Caption);

    with F.mnuTextCopy do Caption:= ini.ReadString('m_e', 'cp', Caption);
    with F.mnuTextSel do Caption:= ini.ReadString('m_se', 'al', Caption);
    with F.mnuTextOpenUrl do Caption:= ini.ReadString('ct', 'url', Caption);
  finally
    FreeAndNil(ini);
  end;
end;

{ TfmAbout }

procedure TfmAbout.bOkClick(Sender: TObject);
begin
  ModalResult:= mrCancel;
end;

const
  PlatfNames: array[TLCLPlatform] of string = (
        'gtk1',
        'gtk2',
        'gtk3',
        'win32/win64',
        'wince',
        'carbon',
        'qt',
        'fpGUI',
        'NoGUI',
        'cocoa',
        'customdraw'
      );

procedure TfmAbout.FormCreate(Sender: TObject);
var
  SWidget: string;
begin
  memo.DoubleBuffered:= UiOps.DoubleBuffered;

  FLabelLink:= TLinkLabel.Create(Self);
  FLabelLink.Parent:= Self;
  FLabelLink.Caption:= 'UVviewsoft.com';
  FLabelLink.Link:= 'http://uvviewsoft.com';
  FLabelLink.Left:= LabelName.Left;
  FLabelLink.AnchorSideTop.Control:= labelInf;
  FLabelLink.AnchorSideTop.Side:= asrBottom;
  FLabelLink.BorderSpacing.Top:= labelInf.BorderSpacing.Top;

  SWidget:= '';
  if WidgetSet<>nil then
    SWidget:= PlatfNames[WidgetSet.LCLPlatform];

  labelInf.Caption:= Format('%s-%s-%s, fpc %s', [
    Lowercase({$I %FPCTARGETOS%}),
    {$I %FPCTARGETCPU%},
    SWidget,
    {$I %FPCVersion%}
    ]);
end;

procedure TfmAbout.FormShow(Sender: TObject);
begin
  memo.Hide;
  memo.Align:= alClient;
  memo.Strings.Clear;
  memo.Strings.LoadFromString(msgAboutCredits);
  memo.DoCaretSingle(0, 0);
  memo.ModeReadOnly:= true;
  memo.Font.Name:= EditorOps.OpFontName;
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

