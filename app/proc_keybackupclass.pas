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
  ATSynEdit_Keymap;

type
  { TAppHotkeyBackup }

  TAppHotkeyBackup = class
  private
    L: TStringList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(AMapItem: TATKeymapItem; const AStr: string);
    procedure Get(AMapItem: TATKeymapItem; const AStr: string);
    function DebugText: string;
  end;

implementation

type
  TMyKeyPair = class
    Keys1, Keys2: TATKeyArray;
  end;

{ TAppHotkeyBackup }

constructor TAppHotkeyBackup.Create;
begin
  inherited;
  L:= TStringList.Create;
  L.OwnsObjects:= true;
  L.Duplicates:= dupIgnore;
  L.UseLocale:= false;
  L.Sorted:= true;
end;

destructor TAppHotkeyBackup.Destroy;
begin
  FreeAndNil(L);
  inherited;
end;

procedure TAppHotkeyBackup.Add(AMapItem: TATKeymapItem; const AStr: string);
var
  Pair: TMyKeyPair;
begin
  if AStr='' then
  begin
    //this should not happen! debug if we are here.
    exit;
  end;
  if (AMapItem.Keys1.Length>0) or
    (AMapItem.Keys2.Length>0) then
  begin
    Pair:= TMyKeyPair.Create;
    Pair.Keys1:= AMapItem.Keys1;
    Pair.Keys2:= AMapItem.Keys2;
    L.AddObject(AStr, Pair);
  end;
end;

procedure TAppHotkeyBackup.Get(AMapItem: TATKeymapItem; const AStr: string);
var
  N: integer;
  Pair: TMyKeyPair;
begin
  N:= L.IndexOf(AStr);
  if N>=0 then
  begin
    Pair:= TMyKeyPair(L.Objects[N]);
    AMapItem.Keys1:= Pair.Keys1;
    AMapItem.Keys2:= Pair.Keys2;
  end;
end;

function TAppHotkeyBackup.DebugText: string;
var
  Pair: TMyKeyPair;
  i: integer;
begin
  Result:= '';
  for i:= 0 to L.Count-1 do
  begin
    Pair:= TMyKeyPair(L.Objects[i]);
    Result+= Format('%s - (%s, %s)'#10, [L[i], Pair.Keys1.ToString, Pair.Keys2.ToString]);
  end;
end;

end.

