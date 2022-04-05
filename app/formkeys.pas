(*
This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at http://mozilla.org/MPL/2.0/.

Copyright (c) Alexey Torgashin
*)
unit formkeys;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ButtonPanel,
  StdCtrls, Menus, ExtCtrls, IniFiles,
  LclType, LclProc, LazUTF8, LazFileUtils,
  ATSynEdit_Keymap,
  proc_globdata,
  proc_customdialog,
  proc_msg;

type
  { TfmKeys }

  TfmKeys = class(TForm)
    bAdd1: TButton;
    bAdd2: TButton;
    bClear1: TButton;
    bClear2: TButton;
    bSet1: TButton;
    bSet2: TButton;
    bCancelInput: TButton;
    chkForLexer: TCheckBox;
    LabelDupInfo: TLabel;
    labelKey1: TLabel;
    labelKey2: TLabel;
    panelInput: TPanel;
    panelBtn: TButtonPanel;
    panelPress: TPanel;
    TimerAdd: TTimer;
    procedure bAdd1Click(Sender: TObject);
    procedure bAdd2Click(Sender: TObject);
    procedure bCancelInputClick(Sender: TObject);
    procedure bClear1Click(Sender: TObject);
    procedure bClear2Click(Sender: TObject);
    procedure bSet1Click(Sender: TObject);
    procedure bSet2Click(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
    procedure panelPressMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure TimerAddTimer(Sender: TObject);
  private
    { private declarations }
    FKeyPressed: integer;
    procedure UpdateState;
    procedure AddHotkey(var K: TATKeyArray);
    function GetHotkey: integer;
    procedure Localize;
  public
    { public declarations }
    Keymap: TATKeymap;
    LexerName: string;
    CommandCode: integer;
    Keys1, Keys2: TATKeyArray;
  end;

var
  fmKeys: TfmKeys;

implementation

{$R *.lfm}

{ TfmKeys }

procedure TfmKeys.Localize;
const
  section = 'd_keys';
var
  ini: TIniFile;
  fn: string;
begin
  fn:= AppFile_Language;
  if not FileExists(fn) then exit;
  ini:= TIniFile.Create(fn);
  try
    Caption:= ini.ReadString(section, '_', Caption);
    with panelBtn.OKButton do Caption:= msgButtonOk;
    with panelBtn.CancelButton do Caption:= msgButtonCancel;
    bCancelInput.Caption:= msgButtonCancel;

    with bSet1 do Caption:= ini.ReadString(section, 'set', Caption);
    with bClear1 do Caption:= ini.ReadString(section, 'clr', Caption);
    with bAdd1 do Caption:= ini.ReadString(section, 'ex', Caption);

    bSet2.Caption:= bSet1.Caption;
    bClear2.Caption:= bClear1.Caption;
    bAdd2.Caption:= bAdd1.Caption;

    with panelPress do Caption:= ini.ReadString(section, 'wait', Caption);
    with chkForLexer do Caption:= ini.ReadString(section, 'lex', Caption);
  finally
    FreeAndNil(ini);
  end;
end;

procedure TfmKeys.FormShow(Sender: TObject);
begin
  Localize;
  DoForm_ScaleAuto(Self, true);
  UpdateFormOnTop(Self);

  //OK btn needs confirmtion
  panelBtn.OKButton.ModalResult:= mrNone;

  UpdateState;

  //if no hotkeys, user wants to add it, so auto-press Extend here
  if (Keys1.Length=0) and (Keys2.Length=0) then
    TimerAdd.Enabled:= true;
end;

procedure TfmKeys.HelpButtonClick(Sender: TObject);
begin
  ModalResult:= mrNo;
end;

procedure TfmKeys.OKButtonClick(Sender: TObject);
var
  Item: TATKeymapItem;
  SDesc: string;
  N: integer;
begin
  //don't check for duplicates, if "For current kexer" checked
  //to fix https://github.com/Alexey-T/CudaText/issues/1656
  if chkForLexer.Checked then
  begin
    ModalResult:= mrOk;
    exit;
  end;

  Item:= TATKeymapItem.Create;
  try
    Item.Command:= CommandCode;
    Item.Keys1:= Keys1;
    Item.Keys2:= Keys2;

    N:= TKeymapHelper.CheckDuplicateForCommand(Item, LexerName, false);
    if N=0 then
    begin
      ModalResult:= mrOk;
      exit;
    end;

    N:= Keymap.IndexOf(N);
    if N>=0 then
      SDesc:= Keymap.Items[N].Name
    else
      SDesc:= '??';

    if MsgBox(
         Format(msgConfirmHotkeyBusy, [SDesc]),
         MB_OKCANCEL or MB_ICONWARNING) = ID_OK then
    begin
      TKeymapHelper.CheckDuplicateForCommand(Item, LexerName, true);
      ModalResult:= mrOk;
    end;
  finally
    Item.Free;
  end;
end;

procedure TfmKeys.panelPressMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  NKey: word;
begin
  case Button of
    mbExtra1:
      begin
        NKey:= VK_BROWSER_BACK;
        KeyDown(NKey, Shift);
        exit;
      end;
    mbExtra2:
      begin
        NKey:= VK_BROWSER_FORWARD;
        KeyDown(NKey, Shift);
        exit;
      end;
  end;
end;

procedure TfmKeys.TimerAddTimer(Sender: TObject);
begin
  TimerAdd.Enabled:= false;
  bAdd1.Click;
end;

procedure TfmKeys.bClear1Click(Sender: TObject);
begin
  Keys1.Clear;
  UpdateState;
end;

procedure TfmKeys.bClear2Click(Sender: TObject);
begin
  Keys2.Clear;
  UpdateState;
end;

procedure TfmKeys.bSet1Click(Sender: TObject);
begin
  Keys1.Clear;
  AddHotkey(Keys1);
  UpdateState;
end;

procedure TfmKeys.bSet2Click(Sender: TObject);
begin
  Keys2.Clear;
  AddHotkey(Keys2);
  UpdateState;
end;

procedure TfmKeys.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if not panelPress.Visible then exit;

  if not AppKeyIsAllowedAsCustomHotkey(Key, Shift) then
  begin
    Key:= 0;
    exit
  end;

  FKeyPressed:= ShortCut(Key, Shift);
  key:= 0;
end;

procedure TfmKeys.bAdd1Click(Sender: TObject);
begin
  AddHotkey(Keys1);
  UpdateState;
end;

procedure TfmKeys.bAdd2Click(Sender: TObject);
begin
  AddHotkey(Keys2);
  UpdateState;
end;

procedure TfmKeys.bCancelInputClick(Sender: TObject);
begin
  FKeyPressed:= -1;
end;

procedure TfmKeys.AddHotkey(var K: TATKeyArray);
var
  newkey, index, i: integer;
begin
  newkey:= GetHotkey;
  if newkey=0 then exit;

  index:= -1;
  for i:= 0 to High(K.Data) do
    if K.Data[i]=0 then
      begin index:= i; break end;
  if index<0 then exit;

  K.Data[index]:= newkey;
end;

function TfmKeys.GetHotkey: integer;
begin
  Result:= 0;

  panelPress.Align:= alClient;
  panelInput.Hide;
  panelBtn.Hide;
  panelPress.Show;

  try
    FKeyPressed:= 0;
    repeat
      Application.ProcessMessages;
      if Application.Terminated then Break;
      if ModalResult=mrCancel then Break;
      if FKeyPressed<>0 then
      begin
        //value -1 means "cancel input"
        if FKeyPressed>0 then
          Result:= FKeyPressed;
        Break;
      end;
    until false;
  finally
    panelPress.Hide;
    panelBtn.Show;
    panelInput.Show;
  end;
end;

procedure TfmKeys.UpdateState;
var
  Item: TATKeymapItem;
  SDesc: string;
  N: integer;
begin
  labelKey1.caption:= '1) '+Keys1.ToString;
  labelKey2.caption:= '2) '+Keys2.ToString;

  bClear1.Enabled:= Keys1.Length>0;
  bClear2.Enabled:= Keys2.Length>0;
  bAdd1.Enabled:= (Keys1.Length>0) and (Keys1.Length<Length(TATKeyArray.Data));
  bAdd2.Enabled:= (Keys2.Length>0) and (Keys2.Length<Length(TATKeyArray.Data));

  if bSet1.Visible and bSet1.CanFocus then
    ActiveControl:= bSet1;

  //check dups
  Item:= TATKeymapItem.Create;
  try
    Item.Command:= CommandCode;
    Item.Keys1:= Keys1;
    Item.Keys2:= Keys2;

    N:= TKeymapHelper.CheckDuplicateForCommand(Item, LexerName, false);
    if N>0 then
    begin
      N:= Keymap.IndexOf(N);
      if N>=0 then
        SDesc:= Keymap.Items[N].Name
      else
        SDesc:= '??';

      LabelDupInfo.Show;
      LabelDupInfo.Caption:= Format(msgStatusHotkeyBusy, [SDesc]);
    end
    else
      LabelDupInfo.Hide;
  finally
    Item.Free;
  end;
end;

end.

