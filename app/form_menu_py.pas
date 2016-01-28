(*
This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at http://mozilla.org/MPL/2.0/.

Copyright (c) Alexey Torgashin
*)
unit form_menu_py;

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
  proc_str,
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
    FMultiline: boolean;
    listFiltered: TList;
    procedure DoFilter;
    function GetResultCmd: integer;
    function IsFiltered(AOrigIndex: integer): boolean;
    procedure SetMultiline(AValue: boolean);
  public
    { public declarations }
    listItems: TStringList;
    ResultCode: integer;
    InitItemIndex: integer;
    property Multiline: boolean read FMultiline write SetMultiline;
  end;

implementation

{$R *.lfm}

{ TfmMenuApi }

procedure TfmMenuApi.FormShow(Sender: TObject);
begin
  DoFilter;
  if (InitItemIndex>=0) and (InitItemIndex<List.ItemCount) then
  begin
    List.ItemIndex:= InitItemIndex;
    List.Invalidate;
  end;
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
  edit.Font.Quality:= EditorOps.OpFontQuality;

  self.Color:= GetAppColor('ListBg');
  edit.Colors.TextFont:= GetAppColor('EdTextFont');
  edit.Colors.TextBG:= GetAppColor('EdTextBg');
  edit.Colors.TextSelFont:= GetAppColor('EdSelFont');
  edit.Colors.TextSelBG:= GetAppColor('EdSelBg');
  list.Color:= GetAppColor('ListBg');

  ResultCode:= -1;
  list.ItemHeight:= GetDefaultListItemHeight;
  self.Width:= UiOps.ListboxWidth;

  listItems:= TStringlist.Create;
  listFiltered:= TList.Create;
end;

procedure TfmMenuApi.editChange(Sender: TObject);
begin
  DoFilter;
end;

procedure TfmMenuApi.FormDestroy(Sender: TObject);
begin
  FreeAndNil(listFiltered);
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
    Result:= PtrInt(listFiltered[list.ItemIndex])
  else
    Result:= -1;
end;

procedure TfmMenuApi.listDrawItem(Sender: TObject; C: TCanvas;
  AIndex: integer; const ARect: TRect);
const
  cIndent = 4;
  cIndent2 = 10;
var
  cl: TColor;
  n, i: integer;
  str, buf: string;
  strname, strkey, strfind: UnicodeString;
  ar: TATIntArray;
  pnt: TPoint;
  r1: TRect;
begin
  if AIndex=list.ItemIndex then
    cl:= GetAppColor('ListSelBg')
  else
    cl:= list.Color;
  c.Brush.Color:= cl;
  c.Pen.Color:= cl;
  c.FillRect(ARect);
  c.Font.Color:= GetAppColor('ListFont');

  str:= listItems[PtrInt(listFiltered[AIndex])]; //ansi
  strname:= Utf8Decode(SGetItem(str, #9)); //uni
  strkey:= Utf8Decode(SGetItem(str, #9)); //uni
  strfind:= Trim(edit.Text); //uni

  pnt:= Point(ARect.Left+cIndent, ARect.Top+1);
  c.TextOut(pnt.x, pnt.y, Utf8Encode(strname));

  c.Font.Color:= GetAppColor('ListFontHilite');

  if UiOps.ListboxFuzzySearch then
  begin
    ar:= SFindFuzzyPositions(strname, strfind);
    for i:= Low(ar) to High(ar) do
    begin
      buf:= Utf8Encode(UnicodeString(strname[ar[i]]));
      n:= c.TextWidth(Utf8Encode(Copy(strname, 1, ar[i]-1)));
      r1:= Rect(pnt.x+n, pnt.y, pnt.x+n+c.TextWidth(buf), ARect.Bottom);
      ExtTextOut(c.Handle,
        r1.Left, r1.Top,
        ETO_CLIPPED+ETO_OPAQUE,
        @r1,
        PChar(buf),
        Length(buf),
        nil);
    end;
  end;
  {//no support for n words
  else
  begin
    n:= Pos(Lowercase(strfind), Lowercase(strname));
    if n>0 then
    begin
      buf:= Copy(strname, n, Length(strfind));
      n:= c.TextWidth(Copy(strname, 1, n-1));
      r1:= Rect(pnt.x+n, pnt.y, pnt.x+n+c.TextWidth(buf), ARect.Bottom);
      ExtTextOut(c.Handle,
        r1.Left, r1.Top,
        ETO_CLIPPED+ETO_OPAQUE,
        @r1,
        PChar(buf),
        Length(buf),
        nil);
    end;
  end;
  }

  if strkey<>'' then
  begin
    if not Multiline then
      pnt:= Point(ARect.Right-cIndent-c.TextWidth(Utf8Encode(strkey)), pnt.y)
    else
      pnt:= Point(ARect.Left+cIndent2, pnt.y+list.ItemHeight div 2-2);

    c.Font.Color:= GetAppColor('ListFontHotkey');
    c.TextOut(pnt.x, pnt.y, Utf8Encode(strkey));
  end;
end;

procedure TfmMenuApi.DoFilter;
var
  i: integer;
begin
  listFiltered.Clear;
  for i:= 0 to listItems.Count-1 do
    if IsFiltered(i) then
      listFiltered.Add(Pointer(PtrInt(i)));

  list.ItemIndex:= 0;
  list.ItemTop:= 0;
  list.ItemCount:= listFiltered.Count;
  list.Invalidate;
end;

function TfmMenuApi.IsFiltered(AOrigIndex: integer): boolean;
var
  StrFind: atString;
  StrText: string;
  Ar: TATIntArray;
begin
  StrText:= listItems[AOrigIndex];
  StrFind:= Utf8Encode(Trim(edit.Text));
  if StrFind='' then begin result:= true; exit end;

  if UiOps.ListboxFuzzySearch then
  begin
    Ar:= SFindFuzzyPositions(StrText, StrFind);
    Result:= Length(Ar)>0;
  end
  else
  begin
    Result:= SFindWordsInString(StrText, StrFind);
  end;
end;

procedure TfmMenuApi.SetMultiline(AValue: boolean);
begin
  if FMultiline=AValue then Exit;
  FMultiline:=AValue;
  list.ItemHeight:= Trunc(GetDefaultListItemHeight*IfThen(FMultiline, 1.8, 1));
  list.Invalidate;
end;

end.

