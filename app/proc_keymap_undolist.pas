unit proc_keymap_undolist;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  ATSynEdit_Keymap;

type
  TATKeymapUndoItem = class
    StrId: string;
    KeyArray1,
    KeyArray2: TATKeyArray;
  end;

type
  { TATKeymapUndoList }

  TATKeymapUndoList = class
  private
    FList: TList;
    function GetItem(AIndex: integer): TATKeymapUndoItem;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function Count: integer;
    procedure Clear;
    procedure Add(AItem: TATKeymapUndoItem);
    property Items[AIndex: integer]: TATKeymapUndoItem read GetItem; default;
  end;

implementation

{ TATKeymapUndoList }

constructor TATKeymapUndoList.Create;
begin
  inherited;
  FList:= TList.Create;
end;

destructor TATKeymapUndoList.Destroy;
begin
  Clear;
  FreeAndNil(FList);
  inherited;
end;

function TATKeymapUndoList.Count: integer;
begin
  Result:= FList.Count;
end;

procedure TATKeymapUndoList.Clear;
var
  i: integer;
begin
  for i:= FList.Count-1 downto 0 do
    TObject(FList[i]).Free;
  FList.Clear;
end;

procedure TATKeymapUndoList.Add(AItem: TATKeymapUndoItem);
begin
  FList.Add(AItem);
end;

function TATKeymapUndoList.GetItem(AIndex: integer): TATKeymapUndoItem;
begin
  Result:= TATKeymapUndoItem(FList[AIndex]);
end;

end.

