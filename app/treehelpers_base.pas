(*
This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at http://mozilla.org/MPL/2.0/.

Copyright (c) Alexey Torgashin
*)
unit TreeHelpers_Base;

{$mode ObjFPC}{$H+}
{$ModeSwitch advancedrecords}

interface

uses
  SysUtils, Math,
  Classes,
  ComCtrls,
  ATSynEdit_fgl;
    
type
  { TATTreeHelperRecord }

  PATTreeHelperRecord = ^TATTreeHelperRecord;
  TATTreeHelperRecord = record
    X1, Y1, X2, Y2: integer;
    Level: integer;
    Icon: integer;
    Title: string;
    class operator =(const A, B: TATTreeHelperRecord): boolean;
    function ToString: string;
  end;

type
  { TATTreeHelperRecords }

  TATTreeHelperRecords = class(specialize TFPGList<TATTreeHelperRecord>)
  private
    function GetItemPtr(N: integer): PATTreeHelperRecord;
  protected
    procedure Deref(Item: Pointer); override;
  public
    property ItemPtr[N: integer]: PATTreeHelperRecord read GetItemPtr;
    function ToString: string; reintroduce;
  end;

type
  { TATTreeHelperParents }

  TATTreeHelperParents = record
    Nodes: array[1..12] of TTreeNode;
    procedure Clear(AFromLevel: integer);
    function FindParent(ALevel: integer): TTreeNode;
    procedure SetNode(ALevel: integer; ANode: TTreeNode);
  end;

implementation

{ TATTreeHelperParents }

procedure TATTreeHelperParents.Clear(AFromLevel: integer);
var
  i: integer;
begin
  for i:= Max(Low(Nodes), AFromLevel) to High(Nodes) do
    Nodes[i]:= nil;
end;

function TATTreeHelperParents.FindParent(ALevel: integer): TTreeNode;
var
  i: integer;
begin
  for i:= Min(High(Nodes), ALevel-1) downto Low(Nodes) do
    if Assigned(Nodes[i]) then
      exit(Nodes[i]);
  Result:= nil;
end;

procedure TATTreeHelperParents.SetNode(ALevel: integer; ANode: TTreeNode);
begin
  if (ALevel>=Low(Nodes)) and (ALevel<=High(Nodes)) then
  begin
    Nodes[ALevel]:= ANode;
    Clear(ALevel+1);
  end;
end;

{ TATTreeHelperRecord }

class operator TATTreeHelperRecord.=(const A, B: TATTreeHelperRecord): boolean;
begin
  Result:= false;
end;

function TATTreeHelperRecord.ToString: string;
begin
  Result:= Format('level %d; (%d, %d)-(%d, %d); "%s"', [Level, X1, Y1, X2, Y2, Title]);
end;

{ TATTreeHelperRecords }

function TATTreeHelperRecords.GetItemPtr(N: integer): PATTreeHelperRecord;
begin
  Result:= PATTreeHelperRecord(InternalGet(N));
end;

procedure TATTreeHelperRecords.Deref(Item: Pointer);
begin
  PATTreeHelperRecord(Item)^.Title:= '';
end;

function TATTreeHelperRecords.ToString: string;
var
  L: TStringList;
  i: integer;
begin
  L:= TStringList.Create;
  try
    L.TextLineBreakStyle:= tlbsLF;
    for i:= 0 to Count-1 do
      L.Add(GetItemPtr(i)^.ToString);
    Result:= L.Text;
  finally
    FreeAndNil(L);
  end;
end;

end.
