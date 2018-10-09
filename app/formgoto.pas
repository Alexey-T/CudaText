(*
This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at http://mozilla.org/MPL/2.0/.

Copyright (c) Alexey Torgashin
*)
unit formgoto;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics,
  StdCtrls, Dialogs,
  LclProc, LclType,
  ExtCtrls,
  IniFiles,
  ATSynEdit_Edits,
  proc_globdata,
  proc_colors,
  proc_editor,
  proc_miscutils,
  math;

type
  { TfmGoto }

  TfmGoto = class(TForm)
    edInput: TATEdit;
    plCaption: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
    procedure SetIsDoubleBuffered(AValue: boolean);
  public
    { public declarations }
    procedure UpdateFonts;
    property IsDoubleBuffered: boolean write SetIsDoubleBuffered;
  end;

var
  fmGoto: TfmGoto;

implementation

{$R *.lfm}

{ TfmGoto }

procedure TfmGoto.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if key=VK_ESCAPE then
  begin
    ModalResult:= mrCancel;
    key:= 0;
    exit
  end;
  if key=VK_RETURN then
  begin
    ModalResult:= mrOk;
    key:= 0;
    exit;
  end;
end;

procedure TfmGoto.FormShow(Sender: TObject);
begin
  plCaption.Font.Name:= UiOps.VarFontName;
  plCaption.Font.Size:= UiOps.VarFontSize;
  plCaption.Font.Color:= GetAppColor('ListFont');

  UpdateFonts;
  UpdateFormOnTop(Self);

  Height:= plCaption.Height + edInput.BorderSpacing.Around*2 + edInput.Height;
  edInput.Text:= '';

  with TIniFile.Create(GetAppLangFilename) do
  try
    plCaption.Caption:= ReadString('d_f', 'go_', 'Go to');
  finally
    Free
  end;
end;

procedure TfmGoto.SetIsDoubleBuffered(AValue: boolean);
begin
  edInput.DoubleBuffered:= AValue;
end;

procedure TfmGoto.FormCreate(Sender: TObject);
begin
  edInput.BorderStyle:= bsNone;

  IsDoubleBuffered:= UiOps.DoubleBuffered;

  DoScalePanelControls(Self);
end;

procedure TfmGoto.UpdateFonts;
begin
  edInput.Font.Name:= EditorOps.OpFontName;
  edInput.Font.Size:= EditorOps.OpFontSize;
  edInput.Font.Quality:= EditorOps.OpFontQuality;

  edInput.OptIgnoreWordChars:= true;

  Color:= GetAppColor('ListBg');
  EditorApplyTheme(edInput);
end;

end.

