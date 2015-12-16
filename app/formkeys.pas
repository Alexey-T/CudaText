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
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ButtonPanel,
  StdCtrls, Menus,
  LclType, LclProc, ExtCtrls,
  ATSynEdit_Keymap;

type
  { TfmKeys }

  TfmKeys = class(TForm)
    bAdd1: TButton;
    bAdd2: TButton;
    bClear1: TButton;
    bClear2: TButton;
    ButtonPanel1: TButtonPanel;
    labelKey1: TLabel;
    labelKey2: TLabel;
    panelPress: TPanel;
    procedure bAdd1Click(Sender: TObject);
    procedure bAdd2Click(Sender: TObject);
    procedure bClear1Click(Sender: TObject);
    procedure bClear2Click(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
  private
    { private declarations }
    FKeyPressed: integer;
    procedure DoAddKey(var K: TATKeyArray);
    procedure DoClearKey(var K: TATKeyArray);
    procedure DoUpdate;
    function GetHotkey: integer;
  public
    { public declarations }
    Keys1, Keys2: TATKeyArray;
  end;

var
  fmKeys: TfmKeys;

implementation

{$R *.lfm}

{ TfmKeys }

procedure TfmKeys.FormShow(Sender: TObject);
begin
  DoUpdate;
end;

procedure TfmKeys.HelpButtonClick(Sender: TObject);
begin
  Modalresult:= mrNo;
end;

procedure TfmKeys.bClear1Click(Sender: TObject);
begin
  DoClearKey(Keys1);
  DoUpdate;
end;

procedure TfmKeys.bClear2Click(Sender: TObject);
begin
  DoClearKey(Keys2);
  DoUpdate;
end;

procedure TfmKeys.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState
  );
begin
  if panelPress.Visible then
  begin
    if (Key=VK_LMENU) or (key=VK_RMENU) or (key=VK_MENU) or (key=VK_CONTROL)
      or (key=VK_SHIFT) then
        begin key:= 0; exit end;
    FKeyPressed:= ShortCut(Key, Shift);
    key:= 0;
    exit
  end;
end;

procedure TfmKeys.DoClearKey(var K: TATKeyArray);
var
  i: integer;
begin
  for i:= 0 to High(K) do K[i]:= 0;
end;

procedure TfmKeys.bAdd1Click(Sender: TObject);
begin
  DoAddKey(Keys1);
  DoUpdate;
end;

procedure TfmKeys.bAdd2Click(Sender: TObject);
begin
  DoAddKey(Keys2);
  DoUpdate;
end;

procedure TfmKeys.DoAddKey(var K: TATKeyArray);
var
  newkey, index, i: integer;
begin
  newkey:= GetHotkey;
  if newkey=0 then exit;

  index:= -1;
  for i:= 0 to High(K) do
    if K[i]=0 then
      begin index:= i; break end;
  if index<0 then exit;

  K[index]:= newkey;
end;

function TfmKeys.GetHotkey: integer;
var
  s: string;
begin
  result:= 0;
  panelPress.Align:= alClient;
  panelPress.Show;
  FKeyPressed:= 0;
  repeat Application.ProcessMessages until FKeyPressed<>0;
  panelPress.Hide;
  result:= FKeyPressed;
end;

procedure TfmKeys.DoUpdate;
begin
  labelKey1.caption:= '1) '+ KeyArrayToString(Keys1);
  labelKey2.caption:= '2) '+ KeyArrayToString(Keys2);

  bAdd1.Enabled:= KeyArrayLength(Keys1)<cMaxKeyCombo;
  bAdd2.Enabled:= KeyArrayLength(Keys2)<cMaxKeyCombo;
end;

end.

