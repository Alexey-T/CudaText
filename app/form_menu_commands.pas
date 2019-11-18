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
  StdCtrls, ExtCtrls, Dialogs,
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
    PanelInfo: TLabel;
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
    OptShowFiles: boolean;
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
        list.ItemTop:= Max(0, i-4);
        Break
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
  OptShowFiles:= true;
  OptAllowConfig:= true;
  OptFocusedCommand:= 0;

  edit.DoubleBuffered:= UiOps.DoubleBuffered;
  list.DoubleBuffered:= UiOps.DoubleBuffered;

  ResultCommand:= 0;
  ResultHotkeysChanged:= false;

  keymapList:= TList.Create;

  PanelInfo:= TLabel.Create(Self);
  PanelInfo.Hide;
  PanelInfo.Parent:= Self;
  PanelInfo.Align:= alClient;
  PanelInfo.Font.Name:= UiOps.VarFontName;
  PanelInfo.Font.Size:= UiOps.VarFontSize;
  PanelInfo.BorderSpacing.Around:= 20;
  PanelInfo.Caption:= '#p – plugins'#10'#l – lexers'#10'#f – opened files';
end;

procedure TfmCommands.editChange(Sender: TObject);
var
  bHelp: boolean;
begin
  bHelp:= edit.Text='#';
  PanelInfo.Visible:= bHelp;
  list.Visible:= not bHelp;
  if not bHelp then
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

  if (key=VK_HOME) and (Shift=[ssCtrl]) then
  begin
    list.ItemIndex:= 0;
    key:= 0;
    exit
  end;
  if (key=VK_END) and (Shift=[ssCtrl]) then
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

  if (Cmd>=cmdFirstFileCommand) and
     (Cmd<=cmdLastFileCommand) then
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
      c.Filename:= AppFile_Hotkeys;
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
  NCmd: integer;
  StrFind: string;
  bItemLexer, bItemPlugin, bItemFile: boolean;
  bPrefixLexer, bPrefixPlugin, bPrefixFile: boolean;
begin
  Result:= false;

  NCmd:= Item.Command;
  bItemLexer:= (NCmd>=cmdFirstLexerCommand) and (NCmd<=cmdLastLexerCommand);
  bItemPlugin:= (NCmd>=cmdFirstPluginCommand) and (NCmd<=cmdLastPluginCommand);
  bItemFile:= (NCmd>=cmdFirstFileCommand) and (NCmd<=cmdLastFileCommand);

  //filter by options
  if bItemPlugin and not OptShowPlugins then exit(false);
  if bItemLexer and not OptShowLexers then exit(false);
  if bItemFile and not OptShowFiles then exit(false);
  if (NCmd>0) and not OptShowUsual then exit(false);

  //filter by input field
  StrFind:= Trim(edit.Text);
  if StrFind='' then exit(true);

  bPrefixLexer:= Pos('#l', StrFind)>0;
  bPrefixPlugin:= Pos('#p', StrFind)>0;
  bPrefixFile:= Pos('#f', StrFind)>0;

  if bPrefixLexer and not bItemLexer then exit(false);
  if bPrefixPlugin and not bItemPlugin then exit(false);
  if bPrefixFile and not bItemFile then exit(false);

  StrFind:= StringReplace(StrFind, '#l', '', []);
  StrFind:= StringReplace(StrFind, '#p', '', []);
  StrFind:= StringReplace(StrFind, '#f', '', []);
  StrFind:= Trim(StrFind);
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

