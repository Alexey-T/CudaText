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
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  LclProc, LclType, LclIntf, ButtonPanel,
  proc_msg, proc_globdata;

type
  { TfmAbout }

  TfmAbout = class(TForm)
    ButtonPanel1: TButtonPanel;
    Label1: TLabel;
    labelInf: TLabel;
    labelVer: TLabel;
    Label3: TLabel;
    memo: TMemo;
    procedure bCreditsClick(Sender: TObject);
    procedure bOkClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Label3Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    FCredits: string;
  end;

implementation

uses InterfaceBase;

{$R *.lfm}

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

procedure TfmAbout.Label3Click(Sender: TObject);
begin
  OpenUrl('http://uvviewsoft.com/');
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

