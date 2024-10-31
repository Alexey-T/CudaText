(*
This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at http://mozilla.org/MPL/2.0/.

Copyright (c) Alexey Torgashin
*)
unit proc_KeyBackupClass;

{$mode objfpc}{$H+}

interface

uses
  SysUtils,
  Classes,
  ATSynEdit_Keymap,
  fgl;

type
  TAppHotkeyPair = record
    Keys1, Keys2: TATKeyArray;
  end;

  TAppHotkeyBackupMap = specialize TFPGMap<string, TAppHotkeyPair>;

type
  { TAppHotkeyBackup }

  TAppHotkeyBackup = class
  private
    D: TAppHotkeyBackupMap;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Add(AMapItem: TATKeymapItem; const AStr: string);
    procedure Get(AMapItem: TATKeymapItem; const AStr: string);
    function DebugText: string;
  end;

implementation

{ TAppHotkeyBackup }

constructor TAppHotkeyBackup.Create;
begin
  inherited;
  D:= TAppHotkeyBackupMap.Create;
  D.Sorted:= true;
end;

destructor TAppHotkeyBackup.Destroy;
begin
  Clear;
  FreeAndNil(D);
  inherited;
end;

procedure TAppHotkeyBackup.Clear;
var
  i: integer;
begin
  for i:= D.Count-1 downto 0 do
    D.Delete(i);
end;

procedure TAppHotkeyBackup.Add(AMapItem: TATKeymapItem; const AStr: string);
var
  Pair: TAppHotkeyPair;
begin
  if AStr='' then
  begin
    //this should not happen! debug if we are here.
    exit;
  end;
  if (AMapItem.Keys1.Length>0) or
    (AMapItem.Keys2.Length>0) then
  begin
    Pair:= Default(TAppHotkeyPair);
    Pair.Keys1:= AMapItem.Keys1;
    Pair.Keys2:= AMapItem.Keys2;
    D[AStr]:= Pair;
  end;
end;

procedure TAppHotkeyBackup.Get(AMapItem: TATKeymapItem; const AStr: string);
var
  N: integer;
  Pair: TAppHotkeyPair;
begin
  if D.Find(AStr, N) then
  begin
    Pair:= D.Data[N];
    AMapItem.Keys1:= Pair.Keys1;
    AMapItem.Keys2:= Pair.Keys2;
  end;
end;

function TAppHotkeyBackup.DebugText: string;
var
  Pair: TAppHotkeyPair;
  i: integer;
begin
  Result:= '';
  for i:= 0 to D.Count-1 do
  begin
    Pair:= D.Data[i];
    Result+= Format('%s - (%s, %s)'#10, [D.Keys[i], Pair.Keys1.ToString, Pair.Keys2.ToString]);
  end;
end;

end.

