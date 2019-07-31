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
  ExtCtrls,
  ATStringProc,
  ATSynEdit,
  ATListbox,
  proc_globdata,
  proc_colors,
  proc_scrollbars;

type
  TAppIntegerEvent = procedure(AValue: integer) of object;

type
  { TfmMenuList }

  TfmMenuList = class(TForm)
    List: TATListbox;
    plCaption: TPanel;
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
    FOnListSelect: TAppIntegerEvent;
    procedure SetListCaption(const AValue: string);
    procedure DoListChangedSel(Sender: TObject);
    { private declarations }
  public
    { public declarations }
    InitialItemIndex: integer;
    ResultIndex: integer;
    Items: TStringlist;
    CloseOnCtrlRelease: boolean;
    property OnListSelect: TAppIntegerEvent read FOnListSelect write FOnListSelect;
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
end;

procedure TfmMenuList.ListDrawItem(Sender: TObject; C: TCanvas; AIndex: integer;
  const ARect: TRect);
var
  pnt: TPoint;
  str0, str1, str2: string;
  NColorFont, NColorBack: TColor;
begin
  str0:= Items[AIndex];
  str1:= SGetItem(str0, #9);
  str2:= str0;

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

procedure TfmMenuList.FormCreate(Sender: TObject);
begin
  if UiOps.ShowMenuDialogsWithBorder then
    BorderStyle:= bsDialog;

  List.DoubleBuffered:= UiOps.DoubleBuffered;

  FColorBg:= GetAppColor('ListBg');
  FColorBgSel:= GetAppColor('ListSelBg');
  FColorFont:= GetAppColor('ListFont');
  FColorFontSel:= GetAppColor('ListSelFont');
  FColorFontAlt:= GetAppColor('ListFontHotkey');

  self.Color:= FColorBg;
  List.Color:= FColorBg;

  plCaption.Height:= AppScale(26);
  plCaption.Font.Name:= UiOps.VarFontName;
  plCaption.Font.Size:= AppScaleFont(UiOps.VarFontSize);
  plCaption.Font.Color:= GetAppColor('ListFont');

  self.Width:= AppScale(UiOps.ListboxSizeX);
  self.Height:= AppScale(UiOps.ListboxSizeY);

  List.OnChangedSel:= @DoListChangedSel;

  Items:= nil;
  ResultIndex:= -1;
end;

procedure TfmMenuList.FormDestroy(Sender: TObject);
begin
end;

procedure TfmMenuList.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key=VK_DOWN) or (Key=VK_TAB) then
  begin
    if List.ItemIndex=List.ItemCount-1 then
      List.ItemIndex:= 0
    else
      List.ItemIndex:= List.ItemIndex+1;
    key:= 0;
  end;

  if Key=VK_UP then
  begin
    if List.ItemIndex=0 then
      List.ItemIndex:= List.ItemCount-1
    else
      List.ItemIndex:= List.ItemIndex-1;
    key:= 0;
  end;

  if key=VK_HOME then
  begin
    List.ItemIndex:= 0;
    key:= 0;
  end;

  if key=VK_END then
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
begin
  if Assigned(FOnListSelect) then
    FOnListSelect(List.ItemIndex);
end;

end.

