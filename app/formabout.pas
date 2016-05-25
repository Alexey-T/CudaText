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
  ButtonPanel, IniFiles,
  LclProc, LclType, LclIntf,
  LazUTF8, LazFileUtils,
  proc_msg, proc_globdata, ATLinkLabel;

type
  { TfmAbout }

  TfmAbout = class(TForm)
    ButtonPanel1: TButtonPanel;
    Label1: TLabel;
    labelInf: TLabel;
    labelVer: TLabel;
    memo: TMemo;
    procedure bCreditsClick(Sender: TObject);
    procedure bOkClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
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
  Plat: string;
begin
  FLabelLink:= TLinkLabel.Create(Self);
  FLabelLink.Parent:= Self;
  FLabelLink.Caption:= 'UVviewsoft.com';
  FLabelLink.Link:= 'http://uvviewsoft.com';
  FLabelLink.Left:= Label1.Left;
  FLabelLink.Top:= ClientHeight div 2;

  Plat:= '';
  if WidgetSet<>nil then
    Plat:= PlatfNames[WidgetSet.LCLPlatform];
  labelInf.Caption:=
    Lowercase({$I %FPCTARGETOS%}) + '-' + {$I %FPCTARGETCPU%} + '-' + Plat +
    ', fpc '+{$I %FPCVersion%};
end;

procedure TfmAbout.FormShow(Sender: TObject);
begin
  memo.visible:= false;
  memo.top:= 6;
end;

procedure TfmAbout.bCreditsClick(Sender: TObject);
begin
  memo.Align:= alClient;
  memo.Lines.Clear;
  memo.Lines.Add(msgAboutCredits);
  memo.CaretPos:= Point(0, 0);
  memo.visible:= true;
  ButtonPanel1.HelpButton.enabled:= false;
end;

end.

