(*
This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at http://mozilla.org/MPL/2.0/.

Copyright (c) Alexey Torgashin
*)
unit formmenuapi;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics,
  ExtCtrls, Dialogs,
  ATSynEdit,
  ATSynEdit_Edits,
  ATStringProc,
  ATListbox,
  LclProc,
  LclType,
  LclIntf,
  proc_globdata,
  proc_colors,
  math;

type
  { TfmMenuApi }

  TfmMenuApi = class(TForm)
    edit: TATEdit;
    list: TATListbox;
    procedure editChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure listClick(Sender: TObject);
    procedure listDrawItem(Sender: TObject; C: TCanvas; AIndex: integer;
      const ARect: TRect);
  private
    { private declarations }
    listItemsFiltered: TStringlist;
    procedure DoFilter;
    function GetResultCmd: integer;
    function IsFiltered(AOrigIndex: integer): boolean;
  public
    { public declarations }
    listItems: TStringList;
    ResultCode: integer;
  end;

implementation

{$R *.lfm}

{ TfmMenuApi }

procedure TfmMenuApi.FormShow(Sender: TObject);
begin
  DoFilter;
end;

procedure TfmMenuApi.listClick(Sender: TObject);
var
  key: word;
begin
  key:= VK_RETURN;
  FormKeyDown(Self, key, []);
end;

procedure TfmMenuApi.FormCreate(Sender: TObject);
begin
  list.Font.Name:= UiOps.VarFontName;
  list.Font.Size:= UiOps.VarFontSize;
  edit.Font.Name:= EditorOps.OpFontName;
  edit.Font.Size:= EditorOps.OpFontSize;

  self.Color:= GetAppColor('ListBg');
  edit.Colors.TextFont:= GetAppColor('EdTextFont');
  edit.Colors.TextBG:= GetAppColor('EdTextBg');
  edit.Colors.TextSelFont:= GetAppColor('EdSelFont');
  edit.Colors.TextSelBG:= GetAppColor('EdSelBg');
  list.Color:= GetAppColor('ListBg');

  ResultCode:= -1;
  list.ItemHeight:= Trunc(UiOps.VarFontSize*UiOps.ListboxItemHeightScale);
  self.Width:= UiOps.ListboxWidth;

  listItems:= TStringlist.Create;
  listItemsFiltered:= TStringlist.Create;
end;

procedure TfmMenuApi.editChange(Sender: TObject);
begin
  DoFilter;
end;

procedure TfmMenuApi.FormDestroy(Sender: TObject);
begin
  FreeAndNil(listItemsFiltered);
  FreeAndNil(listItems);
end;

procedure TfmMenuApi.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if key=vk_down then
  begin
    if list.ItemIndex=list.ItemCount-1 then
      list.ItemIndex:= 0
    else
      list.ItemIndex:= list.ItemIndex+1;
    key:= 0;
    exit
  end;

  if key=vk_up then
  begin
    if list.ItemIndex=0 then
      list.ItemIndex:= list.ItemCount-1
    else
      list.ItemIndex:= list.ItemIndex-1;
    key:= 0;
    exit
  end;

  if key=VK_HOME then
  begin
    list.ItemIndex:= 0;
    key:= 0;
    exit
  end;
  if key=VK_END then
  begin
    list.ItemIndex:= list.ItemCount-1;
    key:= 0;
    exit
  end;

  if key=VK_NEXT then
  begin
    list.ItemIndex:= Min(list.ItemCount-1, list.ItemIndex+list.Height div list.ItemHeight);
    key:= 0;
    exit
  end;
  if key=VK_PRIOR then
  begin
    list.ItemIndex:= Max(0, list.ItemIndex-list.Height div list.ItemHeight);
    key:= 0;
    exit
  end;

  if key=VK_ESCAPE then
  begin
    Close;
    key:= 0;
    exit
  end;

  if key=VK_RETURN then
  begin
    if (list.ItemIndex>=0) and (list.ItemCount>0) then
    begin
      ResultCode:= GetResultCmd;
      Close;
    end;
    key:= 0;
    exit
  end;
end;

function TfmMenuApi.GetResultCmd: integer;
begin
  if list.ItemIndex>=0 then
    Result:= PtrInt(listItemsFiltered.Objects[list.ItemIndex])
  else
    Result:= -1;
end;

procedure TfmMenuApi.listDrawItem(Sender: TObject; C: TCanvas;
  AIndex: integer; const ARect: TRect);
var
  cl: TColor;
  n, i: integer;
  str, strname, strkey, strfind: string;
  ar: TATIntArray;
  pnt: TPoint;
  r1: TRect;
  buf: string;
begin
  if AIndex=list.ItemIndex then
    cl:= GetAppColor('ListSelBg')
  else
    cl:= list.Color;
  c.Brush.Color:= cl;
  c.Pen.Color:= cl;
  c.FillRect(ARect);
  c.Font.Color:= GetAppColor('ListFont');

  str:= listItems[PtrInt(listItemsFiltered.Objects[AIndex])];
  strname:= SGetItem(str, #9);
  strkey:= SGetItem(str, #9);
  strfind:= Utf8Encode(Trim(edit.Text));

  pnt:= Point(ARect.Left+4, ARect.Top+1);
  c.TextOut(pnt.x, pnt.y, strname);

  c.Font.Color:= GetAppColor('ListFontHilite');
  ar:= SFindFuzzyPositions(strname, strfind);
  for i:= Low(ar) to High(ar) do
  begin
    buf:= strname[ar[i]];
    n:= c.TextWidth(Copy(strname, 1, ar[i]-1));
    r1:= Rect(pnt.x+n, pnt.y, pnt.x+n+c.TextWidth(buf), ARect.Bottom);
    ExtTextOut(c.Handle,
      r1.Left, r1.Top,
      ETO_CLIPPED+ETO_OPAQUE,
      @r1,
      PChar(buf),
      Length(buf),
      nil);
  end;

  if strkey<>'' then
  begin
    n:= ARect.Right-c.TextWidth(strkey)-4;
    c.Font.Color:= GetAppColor('ListFontHotkey');
    c.TextOut(n, pnt.y, strkey);
  end;
end;

procedure TfmMenuApi.DoFilter;
var
  i: integer;
begin
  listItemsFiltered.Clear;
  for i:= 0 to listItems.Count-1 do
    if IsFiltered(i) then
      listItemsFiltered.AddObject(listItems[i], TObject(PtrInt(i)));

  list.ItemIndex:= 0;
  list.ItemTop:= 0;
  list.ItemCount:= listItemsFiltered.Count;
  list.Invalidate;
end;

function TfmMenuApi.IsFiltered(AOrigIndex: integer): boolean;
var
  Str: atString;
  Ar: TATIntArray;
begin
  Str:= Utf8Encode(Trim(edit.Text));
  if Str='' then begin result:= true; exit end;
  Ar:= SFindFuzzyPositions(listItems[AOrigIndex], Str);
  Result:= Length(Ar)>0;
end;

end.

