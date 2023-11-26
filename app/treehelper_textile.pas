(*
This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at http://mozilla.org/MPL/2.0/.

Copyright (c) Alexey Torgashin
*)
unit TreeHelper_Textile;

{$mode objfpc}{$H+}

interface

uses
  SysUtils,
  ATStrings,
  ATSynEdit,
  TreeHelpers_Base;

type
  TTreeHelperTextile = class
  private
  public
    class procedure GetHeaders(Ed: TATSynEdit; Data: TATTreeHelperRecords);
  end;

implementation

class procedure TTreeHelperTextile.GetHeaders(Ed: TATSynEdit; Data: TATTreeHelperRecords);
var
  PrevHeadIndex: array[1..6] of integer = (-1, -1, -1, -1, -1, -1);
  //
  procedure ClosePrevHeader(head, iLine: integer);
  var
    ItemPtr: PATTreeHelperRecord;
    iHead: integer;
  begin
    for iHead:= head to High(PrevHeadIndex) do
      if PrevHeadIndex[iHead]>=0 then
      begin
        ItemPtr:= Data._GetItemPtr(PrevHeadIndex[iHead]);
        if ItemPtr^.Y2<0 then
          ItemPtr^.Y2:= iLine-1;
      end;

    if (head>=Low(PrevHeadIndex)) and (head<=High(PrevHeadIndex)) then
      PrevHeadIndex[head]:= Data.Count-1;
  end;
  //
var
  DataItem: TATTreeHelperRecord;
  St: TATStrings;
  HeadLevel: integer;
  NLen, iLine: integer;
  ch: WideChar;
begin
  Data.Clear;
  St:= Ed.Strings;

  for iLine:= 0 to St.Count-1 do
  begin
    NLen:= St.LinesLen[iLine];

    if NLen<5 then Continue;

    //header must be 'h1. Text'
    ch:= St.LineCharAt(iLine, 1);
    if ch<>'h' then Continue;
    ch:= St.LineCharAt(iLine, 2);
    HeadLevel:= Ord(ch)-Ord('0');
    if not ((HeadLevel>=1) and (HeadLevel<=6)) then Continue;
    ch:= St.LineCharAt(iLine, 3);
    if ch<>'.' then Continue;
    ch:= St.LineCharAt(iLine, 4);
    if ch<>' ' then Continue;

    DataItem.X1:= 0;
    DataItem.Y1:= iLine;
    DataItem.X2:= 0;
    DataItem.Y2:= -1;
    DataItem.Level:= HeadLevel;
    DataItem.Title:= Copy(St.Lines[iLine], 5);
    DataItem.Icon:= -1;
    Data.Add(DataItem);
    ClosePrevHeader(HeadLevel, iLine);
  end;

  ClosePrevHeader(Low(PrevHeadIndex), St.Count-1);
end;

end.

