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
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Types,
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
  proc_msg,
  proc_cmd,
  proc_colors,
  proc_str,
  proc_keysdialog,
  proc_scrollbars,
  at__jsonconf,
  math;

type
  { TfmCommands }

  TfmCommands = class(TForm)
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
    keymapList: TList;
    FOnMsg: TStrEvent;
    FColorBg: TColor;
    FColorBgSel: TColor;
    FColorFont: TColor;
    FColorFontSel: TColor;
    FColorFontHilite: TColor;
    FColorFontHotkey: TColor;
    procedure DoConfigKey(Cmd: integer);
    procedure DoFilter;
    procedure DoResetKey(K: TATKeymapItem);
    function GetListCaption: string;
    function GetResultCmd: integer;
    function IsFiltered(Item: TATKeymapItem): boolean;
    procedure DoMsgStatus(const S: string);
    procedure SetListCaption(const AValue: string);
  public
    { public declarations }
    Keymap: TATKeymap;
    ResultCommand: integer;
    ResultHotkeysChanged: boolean;
    CurrentLexerName: string;
    OptShowUsual: boolean;
    OptShowPlugins: boolean;
    OptShowLexers: boolean;
    OptAllowConfig: boolean;
    OptFocusedCommand: integer;
    property OnMsg: TStrEvent read FOnMsg write FOnMsg;
    property ListCaption: string read GetListCaption write SetListCaption;
  end;

var
  fmCommands: TfmCommands;

implementation

{$R *.lfm}

{ TfmCommands }

procedure TfmCommands.FormShow(Sender: TObject);
var
  i: integer;
begin
  edit.Height:= AppScale(UiOps.InputHeight);
  edit.Font.Name:= EditorOps.OpFontName;
  edit.Font.Size:= EditorOps.OpFontSize;
  edit.Font.Quality:= EditorOps.OpFontQuality;
  panelCaption.Font.Name:= UiOps.VarFontName;
  panelCaption.Font.Size:= AppScaleFont(UiOps.VarFontSize);

  self.Color:= FColorBg;
  edit.Colors.TextFont:= GetAppColor('EdTextFont');
  edit.Colors.TextBG:= GetAppColor('EdTextBg');
  edit.Colors.TextSelFont:= GetAppColor('EdSelFont');
  edit.Colors.TextSelBG:= GetAppColor('EdSelBg');
  edit.Colors.BorderLine:= GetAppColor('EdBorder');
  list.Color:= FColorBg;
  panelCaption.Font.Color:= FColorFont;

  self.Width:= AppScale(UiOps.ListboxSizeX);
  self.Height:= AppScale(UiOps.ListboxSizeY);

  UpdateFormOnTop(Self);
  FixFormPositionToDesktop(Self);

  DoFilter;

  if OptFocusedCommand>0 then
    for i:= 0 to keymapList.Count-1 do
      if TATKeymapItem(keymapList.Items[i]).Command = OptFocusedCommand then
      begin
        list.ItemIndex:= i;
        list.ItemTop:= Max(0, i-2);
      end;
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
  FColorBg:= GetAppColor('ListBg');
  FColorBgSel:= GetAppColor('ListSelBg');
  FColorFont:= GetAppColor('ListFont');
  FColorFontSel:= GetAppColor('ListSelFont');
  FColorFontHilite:= GetAppColor('ListFontHilite');
  FColorFontHotkey:= GetAppColor('ListFontHotkey');

  if UiOps.ShowMenuDialogsWithBorder then
    BorderStyle:= bsDialog;

  OptShowUsual:= true;
  OptShowPlugins:= true;
  OptShowLexers:= true;
  OptAllowConfig:= true;
  OptFocusedCommand:= 0;

  edit.DoubleBuffered:= UiOps.DoubleBuffered;
  list.DoubleBuffered:= UiOps.DoubleBuffered;

  ResultCommand:= 0;
  ResultHotkeysChanged:= false;

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
    list.ItemIndex:= Min(list.ItemCount-1, list.ItemIndex+list.VisibleItems);
    key:= 0;
    exit
  end;
  if key=VK_PRIOR then
  begin
    list.ItemIndex:= Max(0, list.ItemIndex-list.VisibleItems);
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
      ResultCommand:= GetResultCmd;
      Close;
    end;
    key:= 0;
    exit
  end;

  if (key=VK_F9) and (shift=[]) then
  begin
    if OptAllowConfig then
    begin
      DoConfigKey(GetResultCmd);
      ResultHotkeysChanged:= true;
    end;
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
  N: integer;
begin
  DoMsgStatus('');

  if (Cmd>=cmdFirstLexerCommand) and
     (Cmd<=cmdLastLexerCommand) then
  begin
    DoMsgStatus(msgCannotSetHotkey);
    exit
  end;

  N:= list.ItemIndex;
  if DoDialogHotkeys(Cmd, CurrentLexerName) then
  begin
    DoFilter;
    list.ItemIndex:= N;
  end;
end;

procedure TfmCommands.DoResetKey(K: TATKeymapItem);
var
  c: TJSONConfig;
begin
  c:= TJSONConfig.Create(nil);
  try
    try
      c.Formatted:= true;
      c.Filename:= GetAppPath(cFileOptionsKeymap);
      c.DeletePath(IntToStr(K.Command));
    except
    end;
  finally
    c.Free;
  end;
end;

function TfmCommands.GetListCaption: string;
begin
  Result:= PanelCaption.Caption;
end;

procedure TfmCommands.listDrawItem(Sender: TObject; C: TCanvas;
  AIndex: integer; const ARect: TRect);
var
  cl: TColor;
  n, nPrevSize, i: integer;
  strname, strkey, strfind: string;
  ar: TATIntArray;
  pnt: TPoint;
  r1: TRect;
  buf: string;
  TextSize: TSize;
begin
  if AIndex<0 then exit;
  if AIndex=list.ItemIndex then
  begin
    cl:= FColorBgSel;
    c.Font.Color:= FColorFontSel;
  end
  else
  begin
    cl:= FColorBg;
    c.Font.Color:= FColorFont;
  end;
  c.Brush.Color:= cl;
  c.Pen.Color:= cl;
  c.FillRect(ARect);

  //name, key
  strname:= TATKeymapItem(keymapList[AIndex]).Name;
  strkey:= TATKeymapItem(keymapList[AIndex]).Keys1.ToString;
  //add key2
  strfind:= TATKeymapItem(keymapList[AIndex]).Keys2.ToString;
  if strfind<>'' then strkey:= strkey+' / '+strfind;

  strfind:= Utf8Encode(Trim(edit.Text));

  pnt:= Point(ARect.Left+4, ARect.Top);
  c.TextOut(pnt.x, pnt.y, strname);

  c.Font.Color:= FColorFontHilite;

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
    nPrevSize:= c.Font.Size;
    c.Font.Size:= nPrevSize-UiOps.ListboxHotkeyFontSizeDelta;
    TextSize:= c.TextExtent(strkey);
    n:= list.ClientWidth-TextSize.cx-4;
    c.FillRect(n-2, pnt.y, list.ClientWidth, pnt.y+list.ItemHeight);
    c.Font.Color:= FColorFontHotkey;
    c.TextOut(
      n,
      pnt.y + (list.ItemHeight-TextSize.cy) div 2,
      strkey);
    c.Font.Size:= nPrevSize;
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
  list.VirtualItemCount:= keymapList.Count;
  list.Invalidate;
end;

function TfmCommands.IsFiltered(Item: TATKeymapItem): boolean;
var
  StrFind: string;
begin
  Result:= false;

  //filter by options
  if (Item.Command>=cmdFirstPluginCommand) and (Item.Command<=cmdLastPluginCommand) then
  begin
    if not OptShowPlugins then exit(false);
  end
  else
  if (Item.Command>=cmdFirstLexerCommand) and (Item.Command<=cmdLastLexerCommand) then
  begin
    if not OptShowLexers then exit(false);
  end
  else
  if (Item.Command>0) then
  begin
    if not OptShowUsual then exit(false);
  end;

  //filter by input field
  StrFind:= Trim(UTF8Encode(edit.Text));
  if StrFind='' then exit(true);

  //first @ char means search in hotkey
  if StrFind[1]='@' then
  begin
    Delete(StrFind, 1, 1);
    Result:=
      (Pos(LowerCase(StrFind), LowerCase(Item.Keys1.ToString))>0) or
      (Pos(LowerCase(StrFind), LowerCase(Item.Keys2.ToString))>0);
  end
  else
  //normal search in name
  if UiOps.ListboxFuzzySearch then
    Result:= STextListsFuzzyInput(Item.Name, StrFind)
  else
    Result:= STextListsAllWords(Item.Name, StrFind);
end;

procedure TfmCommands.DoMsgStatus(const S: string);
begin
  if Assigned(FOnMsg) then
    FOnMsg(Self, S);
end;

procedure TfmCommands.SetListCaption(const AValue: string);
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

