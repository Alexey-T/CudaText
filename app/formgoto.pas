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
  StdCtrls, ExtCtrls, Dialogs,
  ATSynEdit_Edits,
  ATButtons,
  proc_globdata,
  proc_colors,
  proc_editor,
  proc_miscutils;

const
  cOpGotoLine='gotoline';
  cOpGotoClose='x';

type
  { TfmGoto }

  TfmGoto = class(TForm)
    edInput: TATEdit;
    bGoto: TATButton;
    LabelGoto: TLabel;
    procedure bCloseClick(Sender: TObject);
    procedure bGotoClick(Sender: TObject);
    procedure edInputChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
    FOnDone: TStrEvent;
    procedure DoDone(const S: string);
    procedure SetIsDoubleBuffered(AValue: boolean);
  public
    { public declarations }
    procedure UpdateState;
    procedure UpdateFonts;
    property OnDone: TStrEvent read FOnDone write FOnDone;
    property IsDoubleBuffered: boolean write SetIsDoubleBuffered;
  end;

var
  fmGoto: TfmGoto;

implementation

uses LclProc, LclType;

{$R *.lfm}

{ TfmGoto }

procedure TfmGoto.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if key=VK_ESCAPE then
  begin
    DoDone(cOpGotoClose);
    key:= 0;
    exit
  end;
  if key=VK_RETURN then
  begin
    DoDone(cOpGotoLine);
    key:= 0;
    exit;
  end;
end;

procedure TfmGoto.FormShow(Sender: TObject);
begin
  UpdateFonts;
  ClientHeight:= bGoto.Height+8;
end;

procedure TfmGoto.DoDone(const S: string);
begin
  if Assigned(FOnDone) then
    FOnDone(Self, S);
end;

procedure TfmGoto.SetIsDoubleBuffered(AValue: boolean);
begin
  edInput.DoubleBuffered:= AValue;
  {
  //no need
  bGoto.DoubleBuffered:= AValue;
  bClose.DoubleBuffered:= AValue;
  }
end;

procedure TfmGoto.UpdateState;
begin
  bGoto.Enabled:= edInput.Text<>'';
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
  edInput.OptBorderFocusedActive:= true;

  EditorApplyTheme(edInput);

  LabelGoto.Font.Name:= UiOps.VarFontName;
  LabelGoto.Font.Size:= UiOps.VarFontSize;
  LabelGoto.Font.Color:= GetAppColor('TabFont');
end;

procedure TfmGoto.bGotoClick(Sender: TObject);
begin
  DoDOne(cOpGotoLine);
end;

procedure TfmGoto.edInputChange(Sender: TObject);
begin
  UpdateState;
end;

procedure TfmGoto.bCloseClick(Sender: TObject);
begin
  DoDone(cOpGotoClose);
end;

end.

