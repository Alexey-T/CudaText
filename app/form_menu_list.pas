(*
This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at http://mozilla.org/MPL/2.0/.

Copyright (c) Alexey Torgashin
*)
unit form_menu_list;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  LclType, LclProc,
  ExtCtrls, Buttons,
  ATStringProc,
  ATSynEdit,
  ATSynEdit_Globals,
  ATListbox,
  ATButtons,
  proc_globdata,
  proc_colors;

type
  TAppListSelectEvent = procedure(AIndex: integer; const AStr: string) of object;

type
  { TfmMenuList }

  TfmMenuList = class(TForm)
    ButtonCancel: TATButton;
    List: TATListbox;
    plCaption: TPanel;
    procedure ButtonCancelClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure ListDrawItem(Sender: TObject; C: TCanvas; AIndex: integer;
      const ARect: TRect);
    procedure ListClick(Sender: TObject);
  private
    FColorBg: TColor;
    FColorBgSel: TColor;
    FColorFont: TColor;
    FColorFontSel: TColor;
    FColorFontAlt: TColor;
    FOnListSelect: TAppListSelectEvent;
    procedure SetListCaption(const AValue: string);
    procedure DoListChangedSel(Sender: TObject);
    procedure UpdateColors;
    { private declarations }
  public
    { public declarations }
    InitialItemIndex: integer;
    ResultIndex: integer;
    Items: TStringlist;
    CloseOnCtrlRelease: boolean;
    property OnListSelect: TAppListSelectEvent read FOnListSelect write FOnListSelect;
  end;

var
  fmMenuList: TfmMenuList;

implementation

{$R *.lfm}

{ TfmMenuList }

procedure TfmMenuList.FormShow(Sender: TObject);
begin
  SetListCaption(Caption);
  UpdateFormOnTop(Self);
  List.VirtualItemCount:= Items.Count;
  List.ItemIndex:= InitialItemIndex;
  ButtonCancel.Width:= ButtonCancel.Height;
end;

procedure TfmMenuList.ListDrawItem(Sender: TObject; C: TCanvas; AIndex: integer;
  const ARect: TRect);
var
  pnt: TPoint;
  str1, str2: string;
  NColorFont, NColorBack: TColor;
begin
  if (AIndex<0) or (AIndex>=Items.Count) then exit;
  SSplitByChar(Items[AIndex], #9, str1, str2);

  if AIndex=List.ItemIndex then
  begin
    NColorFont:= FColorFontSel;
    NColorBack:= FColorBgSel;
  end
  else
  begin
    NColorFont:= FColorFont;
    NColorBack:= FColorBg;
  end;

  c.Brush.Color:= NColorBack;
  c.Pen.Color:= NColorBack;
  c.Font.Color:= NColorFont;
  c.FillRect(ARect);

  pnt:= Point(ARect.Left+4, ARect.Top+1);
  c.TextOut(pnt.x, pnt.y, str1);

  c.Font.Color:= FColorFontAlt;
  c.TextOut(ARect.Right-c.TextWidth(str2)-4, pnt.y, str2);
end;

procedure TfmMenuList.ListClick(Sender: TObject);
begin
  ResultIndex:= List.ItemIndex;
  Close;
end;

procedure TfmMenuList.UpdateColors;
begin
  FColorBg:= GetAppColor(apclListBg);
  FColorBgSel:= GetAppColor(apclListSelBg);
  FColorFont:= GetAppColor(apclListFont);
  FColorFontSel:= GetAppColor(apclListSelFont);
  FColorFontAlt:= GetAppColor(apclListFontHotkey);

  self.Color:= FColorBg;
  List.Color:= FColorBg;
end;

procedure TfmMenuList.FormCreate(Sender: TObject);
begin
  if UiOps.ShowMenuDialogsWithBorder then
    BorderStyle:= bsDialog;

  List.DoubleBuffered:= UiOps.DoubleBuffered;

  UpdateColors;

  plCaption.Height:= ATEditorScale(26);
  plCaption.Font.Name:= UiOps.VarFontName;
  plCaption.Font.Size:= ATEditorScaleFont(UiOps.VarFontSize);
  plCaption.Font.Color:= GetAppColor(apclListFont);

  self.Width:= ATEditorScale(UiOps.ListboxSizeX);
  self.Height:= ATEditorScale(UiOps.ListboxSizeY);

  List.OnChangedSel:= @DoListChangedSel;

  Items:= nil;
  ResultIndex:= -1;
end;

procedure TfmMenuList.ButtonCancelClick(Sender: TObject);
begin
  ModalResult:= mrCancel;
end;

procedure TfmMenuList.FormDestroy(Sender: TObject);
begin
end;

procedure TfmMenuList.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key=VK_DOWN) or (Key=VK_TAB) or ((key=VK_J) and (Shift=[ssCtrl])) then
  begin
    if List.ItemIndex=List.ItemCount-1 then
      List.ItemIndex:= 0
    else
      List.ItemIndex:= List.ItemIndex+1;
    key:= 0;
  end;

  if (Key=VK_UP) or ((key=VK_K) and (Shift=[ssCtrl])) then
  begin
    if List.ItemIndex=0 then
      List.ItemIndex:= List.ItemCount-1
    else
      List.ItemIndex:= List.ItemIndex-1;
    key:= 0;
  end;

  if (key=VK_HOME) and (Shift=[ssCtrl]) then
  begin
    List.ItemIndex:= 0;
    key:= 0;
  end;

  if (key=VK_END) and (Shift=[ssCtrl]) then
  begin
    List.ItemIndex:= List.ItemCount-1;
    key:= 0;
  end;

  if key=VK_ESCAPE then
  begin
    Close;
    key:= 0;
  end;

  if key=VK_RETURN then
  begin
    if (List.ItemIndex>=0) and (List.ItemCount>0) then
    begin
      ResultIndex:= List.ItemIndex;
      Close;
    end;
    key:= 0;
  end;
end;

procedure TfmMenuList.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if CloseOnCtrlRelease and (Key=VK_CONTROL) then
  begin
    Key:= 0;
    ResultIndex:= List.ItemIndex;
    Close;
  end;
end;

procedure TfmMenuList.SetListCaption(const AValue: string);
begin
  if UiOps.ShowMenuDialogsWithBorder then
  begin
    Caption:= AValue;
    plCaption.Hide;
  end
  else
  begin
    plCaption.Caption:= AValue;
  end;
end;

procedure TfmMenuList.DoListChangedSel(Sender: TObject);
var
  N: integer;
  S: string;
begin
  N:= List.ItemIndex;
  if (N>=0) and (N<Items.Count) then
    if Assigned(FOnListSelect) then
    begin
      if N>0 then
        S:= Items[N]
      else
        S:= '';
      FOnListSelect(N, S);
      UpdateColors;
      List.Invalidate;
    end;
end;

end.
