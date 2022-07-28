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
  StdCtrls, ExtCtrls, Dialogs, IniFiles,
  ATSynEdit,
  ATSynEdit_Globals,
  ATSynEdit_Edits,
  ATSynEdit_Keymap,
  ATStringProc,
  ATListbox,
  ATButtons,
  LclProc,
  LclType,
  LclIntf, Buttons,
  proc_globdata,
  proc_msg,
  proc_cmd,
  proc_colors,
  proc_str,
  proc_keysdialog,
  at__jsonconf,
  math;

type
  { TfmCommands }

  TfmCommands = class(TForm)
    ButtonCancel: TATButton;
    edit: TATEdit;
    list: TATListbox;
    PanelCaption: TPanel;
    procedure ButtonCancelClick(Sender: TObject);
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
    keymapList: TFPList;
    keymapList_Simple: TFPList;
    keymapList_Fuzzy: TFPList;
    FOnMsg: TAppStringEvent;
    FColorBg: TColor;
    FColorBgSel: TColor;
    FColorFont: TColor;
    FColorFontSel: TColor;
    FColorFontHilite: TColor;
    FColorFontHotkey: TColor;
    PanelInfo: TStaticText;
    procedure DoConfigKey(Cmd: integer);
    procedure DoFilter;
    procedure DoResetKey(K: TATKeymapItem);
    function GetListCaption: string;
    function GetResultCmd: integer;
    function IsFiltered(Item: TATKeymapItem; out AWordMatch: boolean): boolean;
    procedure DoMsgStatus(const S: string);
    procedure Localize;
    procedure SetListCaption(const AValue: string);
  public
    { public declarations }
    Keymap: TATKeymap;
    ResultCommand: integer;
    ResultHotkeysChanged: boolean;
    CurrentLexerName: string;
    CurrentFilterText: string;
    OptShowUsual: boolean;
    OptShowPlugins: boolean;
    OptShowLexers: boolean;
    OptShowFiles: boolean;
    OptShowRecents: boolean;
    OptAllowConfig: boolean;
    OptAllowConfigForLexer: boolean;
    OptFocusedCommand: integer;
    property OnMsg: TAppStringEvent read FOnMsg write FOnMsg;
    property ListCaption: string read GetListCaption write SetListCaption;
  end;

var
  fmCommands: TfmCommands;

implementation

{$R *.lfm}

function _GetPrefix(var S: string; Letter: char): boolean;
begin
  Result:= false;
  if SBeginsWith(S, '#'+Letter) then
  begin
    Delete(S, 1, 2);
    S:= Trim(S);
    exit(true);
  end;
  if SEndsWith(S, '#'+Letter) then
  begin
    Delete(S, Length(S)-1, 2);
    S:= Trim(S);
    exit(true);
  end;
end;

{ TfmCommands }

procedure TfmCommands.FormShow(Sender: TObject);
var
  i: integer;
begin
  Localize;

  edit.Height:= ATEditorScale(UiOps.InputHeight);
  edit.Font.Name:= EditorOps.OpFontName;
  edit.Font.Size:= EditorOps.OpFontSize;
  edit.Font.Quality:= EditorOps.OpFontQuality;

  edit.OptCaretBlinkEnabled:= EditorOps.OpCaretBlinkEn;
  edit.OptCaretBlinkTime:= EditorOps.OpCaretBlinkTime;

  PanelCaption.Font.Name:= UiOps.VarFontName;
  PanelCaption.Font.Size:= ATEditorScaleFont(UiOps.VarFontSize);

  ButtonCancel.Width:= ButtonCancel.Height;

  self.Color:= FColorBg;
  edit.Colors.TextFont:= GetAppColor(apclEdTextFont);
  edit.Colors.TextBG:= GetAppColor(apclEdTextBg);
  edit.Colors.TextSelFont:= GetAppColor(apclEdSelFont);
  edit.Colors.TextSelBG:= GetAppColor(apclEdSelBg);
  edit.Colors.BorderLine:= GetAppColor(apclEdBorder);
  list.Color:= FColorBg;
  PanelCaption.Font.Color:= FColorFont;
  PanelInfo.Font.Color:= FColorFont;

  UpdateFormOnTop(Self);
  FixFormPositionToDesktop(Self);

  edit.Text:= CurrentFilterText;
  if edit.Text<>'' then
  begin
    edit.DoSelect_All;
    if Assigned(edit.OnChange) then
      edit.OnChange(nil);
  end;

  DoFilter;

  if OptFocusedCommand>0 then
    for i:= 0 to keymapList.Count-1 do
      if TATKeymapItem(keymapList.Items[i]).Command = OptFocusedCommand then
      begin
        list.ItemIndex:= i;
        list.ItemTop:= Max(0, i-4);
        Break
      end;

  if OptAllowConfig then
    edit.OptTextHint:= msgCmdPaletteTextHint;
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
  FColorBg:= GetAppColor(apclListBg);
  FColorBgSel:= GetAppColor(apclListSelBg);
  FColorFont:= GetAppColor(apclListFont);
  FColorFontSel:= GetAppColor(apclListSelFont);
  FColorFontHilite:= GetAppColor(apclListFontHilite);
  FColorFontHotkey:= GetAppColor(apclListFontHotkey);

  if UiOps.ShowMenuDialogsWithBorder then
    BorderStyle:= bsDialog;

  OptShowUsual:= true;
  OptShowPlugins:= true;
  OptShowLexers:= true;
  OptShowFiles:= true;
  OptShowRecents:= true;
  OptAllowConfig:= true;
  OptAllowConfigForLexer:= true;
  OptFocusedCommand:= 0;

  edit.DoubleBuffered:= UiOps.DoubleBuffered;
  list.DoubleBuffered:= UiOps.DoubleBuffered;

  edit.Keymap:= AppKeymapMain;

  ResultCommand:= 0;
  ResultHotkeysChanged:= false;

  keymapList:= TFPList.Create;
  keymapList_Simple:= TFPList.Create;
  keymapList_Fuzzy:= TFPList.Create;

  PanelInfo:= TStaticText.Create(Self);
  PanelInfo.Hide;
  PanelInfo.Parent:= Self;
  PanelInfo.Align:= alClient;
  PanelInfo.Font.Name:= UiOps.VarFontName;
  PanelInfo.Font.Size:= ATEditorScaleFont(UiOps.VarFontSize);
  PanelInfo.BorderSpacing.Around:= 20;
  PanelInfo.Caption:= msgCmdPalettePrefixHelp;

  Width:= ATEditorScale(UiOps.ListboxSizeX);
  Height:= ATEditorScale(UiOps.ListboxSizeY);
end;

procedure TfmCommands.editChange(Sender: TObject);
var
  bHelp: boolean;
begin
  CurrentFilterText:= edit.Text;
  bHelp:= edit.Text='#';
  PanelInfo.Visible:= bHelp;
  list.Visible:= not bHelp;
  if not bHelp then
    DoFilter;
end;

procedure TfmCommands.ButtonCancelClick(Sender: TObject);
begin
  ModalResult:= mrCancel;
end;

procedure TfmCommands.FormDestroy(Sender: TObject);
begin
  FreeAndNil(keymapList_Fuzzy);
  FreeAndNil(keymapList_Simple);
  FreeAndNil(keymapList);
end;

procedure TfmCommands.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (key=VK_DOWN) or ((key=VK_J) and (Shift=[ssCtrl])) then
  begin
    if list.ItemIndex=list.ItemCount-1 then
      list.ItemIndex:= 0
    else
      list.ItemIndex:= list.ItemIndex+1;
    key:= 0;
    exit
  end;

  if (key=VK_UP) or ((key=VK_K) and (Shift=[ssCtrl])) then
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
  SLexer: string;
  N: integer;
begin
  DoMsgStatus('');

  if not TPluginHelper.CommandHasConfigurableHotkey(Cmd) then
  begin
    DoMsgStatus(msgCannotSetHotkey);
    exit
  end;

  if OptAllowConfigForLexer then
    SLexer:= CurrentLexerName
  else
    SLexer:= '?';

  N:= list.ItemIndex;
  if DoDialogHotkeys(Keymap, Cmd, SLexer) then
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
  WordResults: TAppSearchWordsResults;
  FuzzyResults: TATIntArray;
  strname, strkey, strfind: string;
  pnt: TPoint;
  RectClip: TRect;
  buf: string;
  TextSize: TSize;
  bFound: boolean;
  cl: TColor;
  n, nPrevSize, i: integer;
begin
  if (AIndex<0) or (AIndex>=keymapList.Count) then exit;

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
  if strfind<>'' then
    strkey:= strkey+' / '+strfind;

  strfind:= Trim(edit.Text);
  _GetPrefix(strfind, 'p');
  _GetPrefix(strfind, 'l');
  _GetPrefix(strfind, 'f');
  _GetPrefix(strfind, 'r');

  pnt:= Point(ARect.Left+4, ARect.Top);
  c.TextOut(pnt.x, pnt.y, strname);

  c.Font.Color:= FColorFontHilite;

  bFound:= STextListsFuzzyInput(
    strname,
    strfind,
    WordResults,
    FuzzyResults,
    UiOps.ListboxFuzzySearch);

  if bFound then
  begin
    if Length(FuzzyResults)>0 then
    begin
      for i:= Low(FuzzyResults) to High(FuzzyResults) do
      begin
        buf:= strname[FuzzyResults[i]];
        n:= c.TextWidth(Copy(strname, 1, FuzzyResults[i]-1));
        RectClip:= Rect(
          pnt.x+n,
          pnt.y,
          pnt.x+n+c.TextWidth(buf),
          ARect.Bottom
          );
        ExtTextOut(c.Handle,
          RectClip.Left,
          RectClip.Top,
          ETO_CLIPPED+ETO_OPAQUE,
          @RectClip,
          PChar(buf),
          Length(buf),
          nil
          );
      end;
    end
    else
    if WordResults.MatchesCount>0 then
    begin
      for i:= 0 to WordResults.MatchesCount-1 do
      begin
        buf:= Copy(strname, WordResults.MatchesArray[i].WordPos, WordResults.MatchesArray[i].WordLen);
        n:= c.TextWidth(Copy(strname, 1, WordResults.MatchesArray[i].WordPos-1));
        RectClip:= Rect(
          pnt.x+n,
          pnt.y,
          pnt.x+n+c.TextWidth(buf),
          ARect.Bottom
          );
        ExtTextOut(c.Handle,
          RectClip.Left,
          RectClip.Top,
          ETO_CLIPPED+ETO_OPAQUE,
          @RectClip,
          PChar(buf),
          Length(buf),
          nil
          );
      end;
    end;
  end;

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

function IsIgnoredCommand(N: integer): boolean;
begin
  Result:= false;
  (*
  if not UiOps.CmdPaletteHideSimpleCommands then
    exit(false);

  case N of
    cCommand_KeyLeft,
    cCommand_KeyLeft_Sel,
    cCommand_KeyRight,
    cCommand_KeyRight_Sel,
    cCommand_KeyUp,
    cCommand_KeyUp_Sel,
    cCommand_KeyDown,
    cCommand_KeyDown_Sel,
    cCommand_KeyHome,
    cCommand_KeyHome_Sel,
    cCommand_KeyEnd,
    cCommand_KeyEnd_Sel,
    cCommand_KeyPageUp,
    cCommand_KeyPageUp_Sel,
    cCommand_KeyPageDown,
    cCommand_KeyPageDown_Sel,

    {
    //let's show these always
    cCommand_ColSelectLeft,
    cCommand_ColSelectRight,
    cCommand_ColSelectUp,
    cCommand_ColSelectDown,
    cCommand_ColSelectPageUp,
    cCommand_ColSelectPageDown,
    cCommand_ColSelectToLineBegin,
    cCommand_ColSelectToLineEnd,
    }

    cCommand_KeyBackspace,
    cCommand_KeyDelete,
    cCommand_KeyEnter,
    cCommand_KeyTab:
      Result:= true;
    else
      Result:= false;
  end;
  *)
end;

procedure TfmCommands.DoFilter;
var
  Item: TATKeymapItem;
  bSimple: boolean;
  i: integer;
begin
  keymapList.Clear;
  keymapList_Simple.Clear;
  keymapList_Fuzzy.Clear;

  for i:= 0 to keymap.Count-1 do
  begin
    Item:= keymap.Items[i];
    if IsIgnoredCommand(Item.Command) then
      Continue;
    if IsFiltered(Item, bSimple) then
    begin
      if bSimple then
        keymapList_Simple.Add(Item)
      else
        keymapList_Fuzzy.Add(Item)
    end;
  end;

  keymapList.AddList(keymapList_Simple);
  keymapList.AddList(keymapList_Fuzzy);

  list.ItemIndex:= 0;
  list.ItemTop:= 0;
  list.VirtualItemCount:= keymapList.Count;
  list.Invalidate;
end;

function TfmCommands.IsFiltered(Item: TATKeymapItem; out AWordMatch: boolean): boolean;
var
  WordResults: TAppSearchWordsResults;
  FuzzyResults: TATIntArray;
  NCmd: integer;
  StrFind: string;
  Category: TAppCommandCategory;
  bPrefixLexer, bPrefixPlugin, bPrefixFile, bPrefixRecent: boolean;
begin
  Result:= false;
  AWordMatch:= false;

  NCmd:= Item.Command;
  Category:= TPluginHelper.CommandCategory(NCmd);

  //filter by options
  if (Category=categ_Plugin) and not OptShowPlugins then exit(false);
  if (Category=categ_Lexer) and not OptShowLexers then exit(false);
  if (Category=categ_OpenedFile) and not OptShowFiles then exit(false);
  if (Category=categ_RecentFile) and not OptShowRecents then exit(false);
  if (Category=categ_Normal) and not OptShowUsual then exit(false);

  //filter by input field
  StrFind:= Trim(edit.Text);
  if StrFind='' then exit(true);

  bPrefixLexer:= _GetPrefix(StrFind, 'l');
  bPrefixPlugin:= _GetPrefix(StrFind, 'p');
  bPrefixFile:= _GetPrefix(StrFind, 'f');
  bPrefixRecent:= _GetPrefix(StrFind, 'r');

  if bPrefixLexer and (Category<>categ_Lexer) then exit(false);
  if bPrefixPlugin and (Category<>categ_Plugin) then exit(false);
  if bPrefixFile and (Category<>categ_OpenedFile) then exit(false);
  if bPrefixRecent and (Category<>categ_RecentFile) then exit(false);

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
  //search in name
  begin
    Result:= STextListsFuzzyInput(
               Item.Name,
               StrFind,
               WordResults,
               FuzzyResults,
               UiOps.ListboxFuzzySearch);
    AWordMatch:= WordResults.MatchesCount>0;
  end;
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

procedure TfmCommands.Localize;
const
  section = 'd_about';
var
  fn: string;
  ini: TIniFile;
begin
  fn:= AppFile_Language;
  if FileExists(fn) then
  begin
    ini:= TIniFile.Create(fn);
    try
      msgCmdPaletteTextHint:= ini.ReadString(section, 'cmd_tip', msgCmdPaletteTextHint);
    finally
      FreeAndNil(ini);
    end;
  end;
end;

end.
