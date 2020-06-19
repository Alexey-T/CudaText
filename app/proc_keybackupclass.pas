unit proc_KeyBackupClass;

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

end.

