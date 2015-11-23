(*
This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at http://mozilla.org/MPL/2.0/.

Copyright (c) Alexey Torgashin
*)
unit form_menu_commands;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics,
  ExtCtrls, Dialogs,
  ATSynEdit,
  ATSynEdit_Edits,
  ATSynEdit_Keymap,
  ATStringProc,
  ATListbox,
  LclProc,
  LclType,
  LclIntf,
  proc_globdata,
  proc_colors,
  proc_str,
  formkeys,
  jsonConf,
  math;

type
  { TfmCommands }

  TfmCommands = class(TForm)
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
    keymapList: TList;
    procedure DoConfigKey(Cmd: integer);
    procedure DoFilter;
    function DoFindDupKeys: boolean;
    procedure DoResetKey(K: TATKeymapItem);
    function GetResultCmd: integer;
    function IsFiltered(Item: TATKeymapItem): boolean;
  public
    { public declarations }
    keymap: TATKeymap;
    ResultNum: integer;
  end;

var
  fmCommands: TfmCommands;

implementation

uses StrUtils;

{$R *.lfm}

{ TfmCommands }

procedure TfmCommands.FormShow(Sender: TObject);
begin
  DoFilter;
end;

procedure TfmCommands.listClick(Sender: TObject);
var
  key: word;
begin
  key:= VK_RETURN;
  FormKeyDown(Self, key, []);
end;

procedure TfmCommands.FormCreate(Sender: TObject);
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

  ResultNum:= 0;
  list.ItemHeight:= GetDefaultListItemHeight;
  self.Width:= UiOps.ListboxWidth;
  keymapList:= TList.Create;
end;

procedure TfmCommands.editChange(Sender: TObject);
begin
  DoFilter;
end;

procedure TfmCommands.FormDestroy(Sender: TObject);
begin
  FreeAndNil(keymapList);
end;

procedure TfmCommands.FormKeyDown(Sender: TObject; var Key: Word;
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
      ResultNum:= GetResultCmd;
      Close;
    end;
    key:= 0;
    exit
  end;

  if (key=vk_F9) and (shift=[]) then
  begin
    DoConfigKey(GetResultCmd);
    key:= 0;
    exit
  end;
end;

function TfmCommands.GetResultCmd: integer;
begin
  Result:= TATKeymapItem(keymapList.Items[list.ItemIndex]).Command;
end;

procedure TfmCommands.DoConfigKey(Cmd: integer);
var
  n: integer;
  StrId: string;
begin
  if (Cmd>=cmdFirstLexerCommand) and
     (Cmd<=cmdLastLexerCommand) then exit;

  n:= keymap.IndexOf(Cmd);
  if n<0 then exit;

  //number (usual cmd) or
  //'module,proc' (plugin)
  StrId:= IntToStr(keymap[n].Command);

  if (Cmd>=cmdFirstPluginCommand) and
     (Cmd<=cmdLastPluginCommand) then
    with FPluginsCmd[Cmd-cmdFirstPluginCommand] do
      StrId:= ItemModule+','+ItemProc;

  with TfmKeys.Create(Self) do
  try
    Keys1:= keymap[n].Keys1;
    Keys2:= keymap[n].Keys2;

    case ShowModal of
      mrOk:
        begin
          keymap[n].Keys1:= Keys1;
          keymap[n].Keys2:= Keys2;
          DoSaveKeyItem(keymap[n], StrId);
          DoFilter;
        end;
    end;
  finally
    Free
  end;

  DoFindDupKeys;
end;

procedure TfmCommands.DoResetKey(K: TATKeymapItem);
var
  c: TJSONConfig;
  path: string;
begin
  path:= IntToStr(K.Command);
  c:= TJSONConfig.Create(Self);
  try
    c.Formatted:= true;
    c.Filename:= GetAppPath(cFileOptKeymap);
    c.DeletePath(path);
  finally
    c.Free;
  end;
end;

procedure TfmCommands.listDrawItem(Sender: TObject; C: TCanvas;
  AIndex: integer; const ARect: TRect);
var
  cl: TColor;
  n, i: integer;
  strname, strkey, strfind: string;
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

  //name, key
  strname:= TATKeymapItem(keymapList[AIndex]).Name;
  strkey:= KeyArrayToString(TATKeymapItem(keymapList[AIndex]).Keys1);
  //add key2
  strfind:= KeyArrayToString(TATKeymapItem(keymapList[AIndex]).Keys2);
  if strfind<>'' then strkey:= strkey+' / '+strfind;

  strfind:= Utf8Encode(Trim(edit.Text));

  pnt:= Point(ARect.Left+4, ARect.Top+1);
  c.TextOut(pnt.x, pnt.y, strname);

  c.Font.Color:= GetAppColor('ListFontHilite');

  if UiOps.ListboxFuzzySearch then
  begin
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
  end;
  {//no support to hilite n words
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
    n:= ARect.Right-c.TextWidth(strkey)-4;
    c.Font.Color:= GetAppColor('ListFontHotkey');
    c.TextOut(n, pnt.y, strkey);
  end;
end;

procedure TfmCommands.DoFilter;
var
  i: integer;
begin
  keymapList.Clear;
  for i:= 0 to keymap.Count-1 do
    if IsFiltered(keymap.Items[i]) then
      keymapList.Add(keymap.Items[i]);

  list.ItemIndex:= 0;
  list.ItemTop:= 0;
  list.ItemCount:= keymapList.Count;
  list.Invalidate;
end;

function TfmCommands.IsFiltered(Item: TATKeymapItem): boolean;
var
  Str: atString;
  Ar: TATIntArray;
begin
  Result:= false;
  Str:= Trim(edit.Text);
  if Str='' then exit(true);

  //first @ char means search in hotkey
  if Str[1]='@' then
  begin
    Delete(Str, 1, 1);
    Result:=
      (Pos(LowerCase(Str), LowerCase(KeyArrayToString(Item.Keys1)))>0) or
      (Pos(LowerCase(Str), LowerCase(KeyArrayToString(Item.Keys2)))>0);
  end
  else
  //normal search in name
  if UiOps.ListboxFuzzySearch then
  begin
    Ar:= SFindFuzzyPositions(Item.Name, Str);
    Result:= Length(Ar)>0;
  end
  else
  begin
    Result:= SFindWordsInString(Item.Name, Str);
  end;
end;

function TfmCommands.DoFindDupKeys: boolean;
var
  i, j: integer;
  item1, item2: TATKeymapItem;
begin
  Result:= false;
  for i:= 0 to keymap.Count-1 do
    for j:= i+1 to keymap.Count-1 do
    begin
      item1:= keymap.Items[i];
      item2:= keymap.Items[j];
      if KeyArraysEqualNotEmpty(item1.Keys1, item2.Keys1) or
         KeyArraysEqualNotEmpty(item1.Keys2, item2.Keys2) or
         KeyArraysEqualNotEmpty(item1.Keys1, item2.Keys2) or
         KeyArraysEqualNotEmpty(item1.Keys2, item2.Keys1) then
        begin
          MsgBox('Commands have same hotkeys:'#13+
            item1.Name+#13+
            item2.Name+#13+
            #13'Please correct one of these hotkeys.',
            MB_OK or MB_ICONWARNING);
          Result:= true;
          Exit
        end;
    end;
end;

end.

