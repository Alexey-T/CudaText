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
  UpdateFonts;
  UpdateFormOnTop(Self);

  Height:= edInput.Top*2 + edInput.Height;
  edInput.Text:= '';
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

  Color:= GetAppColor('ListBg');
  EditorApplyTheme(edInput);
end;

end.

