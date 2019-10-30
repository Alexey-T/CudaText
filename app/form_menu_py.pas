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
    PanelCaption: TPanel;
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
    FColorBg: TColor;
    FColorBgSel: TColor;
    FColorFont: TColor;
    FColorFontSel: TColor;
    FColorFontAlt: TColor;
    FColorFontHilite: TColor;
    procedure DoFilter;
    function GetResultCmd: integer;
    function IsFiltered(AOrigIndex: integer): boolean;
    procedure SetListCaption(const AValue: string);
  public
    { public declarations }
    listItems: TStringList;
    ResultCode: integer;
    InitItemIndex: integer;
    DisableFuzzy: boolean;
    DisableFullFilter: boolean;
    property Multiline: boolean read FMultiline write FMultiline;
    property ListCaption: string write SetListCaption;
  end;

implementation

{$R *.lfm}

{ TfmMenuApi }

procedure TfmMenuApi.FormShow(Sender: TObject);
begin
  UpdateFormOnTop(Self);
  FixFormPositionToDesktop(Self);

  if FMultiline then
    list.ItemHeightPercents:= 185
  else
    list.ItemHeightPercents:= 100;

  DoFilter;

  List.ItemIndex:= InitItemIndex; //check of index not needed
end;

procedure TfmMenuApi.listClick(Sender: TObject);
var
  Pnt: TPoint;
  NIndex: integer;
begin
  Pnt:= list.ScreenToClient(Mouse.CursorPos);
  NIndex:= list.GetItemIndexAt(Pnt);
  if NIndex<0 then exit;
  ResultCode:= GetResultCmd;
  Close;
end;

procedure TfmMenuApi.FormCreate(Sender: TObject);
begin
  FColorBg:= GetAppColor('ListBg');
  FColorBgSel:= GetAppColor('ListSelBg');
  FColorFont:= GetAppColor('ListFont');
  FColorFontSel:= GetAppColor('ListSelFont');
  FColorFontAlt:= GetAppColor('ListFontHotkey');
  FColorFontHilite:= GetAppColor('ListFontHilite');

  if UiOps.ShowMenuDialogsWithBorder then
    BorderStyle:= bsDialog;

  edit.DoubleBuffered:= UiOps.DoubleBuffered;
  list.DoubleBuffered:= UiOps.DoubleBuffered;

  list.Color:= FColorBg;

  edit.Height:= AppScale(UiOps.InputHeight);
  edit.Font.Name:= EditorOps.OpFontName;
  edit.Font.Size:= EditorOps.OpFontSize;
  edit.Font.Quality:= EditorOps.OpFontQuality;
  edit.Colors.TextFont:= GetAppColor('EdTextFont');
  edit.Colors.TextBG:= GetAppColor('EdTextBg');
  edit.Colors.TextSelFont:= GetAppColor('EdSelFont');
  edit.Colors.TextSelBG:= GetAppColor('EdSelBg');
  edit.Colors.BorderLine:= GetAppColor('EdBorder');

  PanelCaption.Height:= AppScale(26);
  PanelCaption.Font.Name:= UiOps.VarFontName;
  PanelCaption.Font.Size:= AppScaleFont(UiOps.VarFontSize);
  PanelCaption.Font.Color:= FColorFont;

  self.Color:= FColorBg;
  self.Width:= AppScale(UiOps.ListboxSizeX);
  self.Height:= AppScale(UiOps.ListboxSizeY);

  ResultCode:= -1;
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
  buf, temp1, temp2: string;
  strname, strkey, strfind: UnicodeString;
  ar: TATIntArray;
  pnt: TPoint;
  r1: TRect;
begin
  if AIndex<0 then exit;
  if AIndex=list.ItemIndex then
  begin
    c.Font.Color:= FColorFontSel;
    cl:= FColorBgSel;
  end
  else
  begin
    c.Font.Color:= FColorFont;
    cl:= FColorBg;
  end;
  c.Brush.Color:= cl;
  c.Pen.Color:= cl;
  c.FillRect(ARect);

  SSplitByChar(listItems[PtrInt(listFiltered[AIndex])], #9, temp1, temp2);
  strname:= Utf8Decode(temp1); //uni
  strkey:= Utf8Decode(temp2); //uni
  strfind:= Trim(edit.Text); //uni

  pnt:= Point(ARect.Left+cIndent, ARect.Top+1);
  c.TextOut(pnt.x, pnt.y, Utf8Encode(strname));

  c.Font.Color:= FColorFontHilite;

  if UiOps.ListboxFuzzySearch and not DisableFuzzy then
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
      pnt:= Point(ARect.Left+List.ClientWidth-cIndent-c.TextWidth(Utf8Encode(strkey)), pnt.y)
    else
      pnt:= Point(ARect.Left+cIndent2, pnt.y+list.ItemHeight div 2);

    if not FMultiline then
      c.FillRect(pnt.x-2, pnt.y, list.ClientWidth, pnt.y+list.ItemHeight-1);

    c.Font.Color:= FColorFontAlt;
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
  list.VirtualItemCount:= listFiltered.Count;
  list.Invalidate;
end;

function TfmMenuApi.IsFiltered(AOrigIndex: integer): boolean;
var
  SFind, SText: string;
begin
  SText:= listItems[AOrigIndex];
  if DisableFullFilter then
    SText:= SGetItem(SText, #9);

  SFind:= Trim(UTF8Encode(edit.Text));
  if SFind='' then exit(true);

  if UiOps.ListboxFuzzySearch and not DisableFuzzy then
    Result:= STextListsFuzzyInput(SText, SFind)
  else
    Result:= STextListsAllWords(SText, SFind);
end;

procedure TfmMenuApi.SetListCaption(const AValue: string);
begin
  if UiOps.ShowMenuDialogsWithBorder then
  begin
    Caption:= AValue;
    PanelCaption.Hide;
  end
  else
  begin
    PanelCaption.Caption:= AValue;
    PanelCaption.Visible:= AValue<>'';
  end;
end;

end.

