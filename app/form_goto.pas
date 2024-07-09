(*
This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at http://mozilla.org/MPL/2.0/.

Copyright (c) Alexey Torgashin
*)
unit form_goto;

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
    FGotoTooltip: string;
    FGotoInfoExt: string;
    procedure SetIsDoubleBuffered(AValue: boolean);
    procedure EditCheckInput(Sender: TObject; AChar: WideChar; var AllowInput: boolean);
    procedure Localize;
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

procedure TfmGoto.Localize;
var
  STitle: string;
begin
  FGotoTooltip:= '(10, 10:10, 10%%, d100, xFFF, %s)';
  FGotoInfoExt:= 'with "+": select';

  with TIniFile.Create(AppFile_Language) do
  try
    STitle:= ReadString('d_f', 'go_', 'Go to');
    FGotoInfoExt:= ReadString('si', 'GotoInfoExt', FGotoInfoExt);
  finally
    Free
  end;

  STitle:= STitle+' '+
    Format(FGotoTooltip, [FGotoInfoExt]);

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

  Color:= GetAppColor(TAppThemeColor.ListBg);
  EditorApplyTheme(edInput);

  plCaption.Height:= ATEditorScale(26);
  plCaption.Font.Name:= UiOps.VarFontName;
  plCaption.Font.Size:= ATEditorScaleFont(UiOps.VarFontSize);
  plCaption.Font.Color:= GetAppColor(TAppThemeColor.ListFont);

  UpdateFormOnTop(Self);

  Localize;

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
  edInput.OptMaxLen:= 80;

  IsDoubleBuffered:= UiOps.DoubleBuffered;

  EditorCaretShapeFromString(edInput.CaretShapeNormal, EditorOps.OpCaretViewNormal);
  EditorCaretShapeFromString(edInput.CaretShapeOverwrite, EditorOps.OpCaretViewOverwrite);
end;

procedure TfmGoto.ButtonCancelClick(Sender: TObject);
begin
  ModalResult:= mrCancel;
end;

end.
