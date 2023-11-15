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
  Dialogs,
  LclProc, LclType,
  ExtCtrls,
  IniFiles,
  ATSynEdit_Globals,
  ATSynEdit_Edits,
  ATButtons,
  proc_msg,
  proc_globdata,
  proc_colors,
  proc_editor,
  math;

type
  { TfmGoto }

  TfmGoto = class(TForm)
    ButtonCancel: TATButton;
    edInput: TATEdit;
    plCaption: TPanel;
    procedure ButtonCancelClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
    procedure SetIsDoubleBuffered(AValue: boolean);
    procedure EditCheckInput(Sender: TObject; AChar: WideChar; var AllowInput: boolean);
    procedure DoLocalize;
  public
    { public declarations }
    property IsDoubleBuffered: boolean write SetIsDoubleBuffered;
  end;

var
  fmGoto: TfmGoto = nil;

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

procedure TfmGoto.DoLocalize;
var
  STitle: string;
begin
  with TIniFile.Create(AppFile_Language) do
  try
    STitle:= ReadString('d_f', 'go_', 'Go to');
  finally
    Free
  end;

  STitle:= STitle+' '+
    Format(msgGotoDialogTooltip, [msgGotoDialogInfoExt]);

  Caption:= STitle;
  plCaption.Caption:= STitle;
end;

procedure TfmGoto.FormShow(Sender: TObject);
begin
  edInput.Height:= ATEditorScale(UiOps.InputHeight);
  edInput.Font.Name:= EditorOps.OpFontName;
  edInput.Font.Size:= EditorOps.OpFontSize;
  edInput.Font.Quality:= EditorOps.OpFontQuality;

  edInput.OptCaretBlinkEnabled:= EditorOps.OpCaretBlinkEn;
  edInput.OptCaretBlinkTime:= EditorOps.OpCaretBlinkTime;

  Color:= GetAppColor(apclListBg);
  EditorApplyTheme(edInput);

  plCaption.Height:= ATEditorScale(26);
  plCaption.Font.Name:= UiOps.VarFontName;
  plCaption.Font.Size:= ATEditorScaleFont(UiOps.VarFontSize);
  plCaption.Font.Color:= GetAppColor(apclListFont);

  UpdateFormOnTop(Self);

  DoLocalize;

  if UiOps.ShowMenuDialogsWithBorder then
  begin
    BorderStyle:= bsDialog;
    plCaption.Hide;
  end;

  ClientHeight:=
    IfThen(plCaption.Visible, plCaption.Height) +
    ATEditorScale(2*edInput.BorderSpacing.Around) +
    edInput.Height;

  ButtonCancel.Width:= ButtonCancel.Height;
end;

procedure TfmGoto.SetIsDoubleBuffered(AValue: boolean);
begin
  edInput.DoubleBuffered:= AValue;
end;

procedure TfmGoto.EditCheckInput(Sender: TObject; AChar: WideChar; var AllowInput: boolean);
begin
  AllowInput:=
    //allow all symbol chars for plugins, to extend input in them
    //Pos(AChar, '0123456789:;.,~`!@#$%^&*-+()[]{}_=/\')>0;
    true;
end;

procedure TfmGoto.FormCreate(Sender: TObject);
begin
  edInput.Keymap:= AppKeymapMain;
  edInput.BorderStyle:= bsNone;
  edInput.OnCheckInput:= @EditCheckInput;

  edInput.OptCaretBlinkEnabled:= EditorOps.OpCaretBlinkEn;
  edInput.OptCaretBlinkTime:= EditorOps.OpCaretBlinkTime;

  IsDoubleBuffered:= UiOps.DoubleBuffered;
end;

procedure TfmGoto.ButtonCancelClick(Sender: TObject);
begin
  ModalResult:= mrCancel;
end;

end.
