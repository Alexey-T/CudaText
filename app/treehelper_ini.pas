(*
This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at http://mozilla.org/MPL/2.0/.

Copyright (c) Alexey Torgashin
*)
unit TreeHelper_Ini;

{$mode objfpc}{$H+}

interface

uses
  SysUtils,
  ATStrings,
  ATSynEdit,
  TreeHelpers_Base;

type
  TTreeHelperIni = class
  public
    class procedure GetHeaders(Ed: TATSynEdit; Data: TATTreeHelperRecords);
  end;

implementation

class procedure TTreeHelperIni.GetHeaders(Ed: TATSynEdit; Data: TATTreeHelperRecords);
const
  cIconFolder = 0;
  cIconArrow = 7;
var
  DataItem: TATTreeHelperRecord;
  PrevHeaderIndex: integer = -1;
  St: TATStrings;
  S: UnicodeString;
  iLine, NFirst, NSymbol, NLen: integer;
begin
  Data.Clear;
  St:= Ed.Strings;
  for iLine:= 0 to St.Count-1 do
  begin
    S:= St.Lines[iLine];
    NLen:= Length(S);
    if NLen=0 then Continue;

    //skip commented lines
    NFirst:= 1;
    while (NFirst<=NLen) and (S[NFirst]=' ') do
      Inc(NFirst);
    if NFirst>NLen then Continue;
    if S[NFirst] in [';', '#', '='] then Continue;

    if (NLen>=3) and (S[1]='[') and (S[NLen]=']') then
    begin
      DataItem.X1:= 0;
      DataItem.Y1:= iLine;
      DataItem.X2:= 0;
      DataItem.Y2:= iLine;
      DataItem.Level:= 1;
      DataItem.Title:= S;
      DataItem.Icon:= cIconFolder;
      Data.Add(DataItem);

      if PrevHeaderIndex>=0 then
        Data._GetItemPtr(PrevHeaderIndex)^.Y2:= iLine;
      PrevHeaderIndex:= Data.Count-1;
    end
    else
    begin
      NSymbol:= Pos('=', S);
      if NSymbol>0 then
      begin
        DataItem.X1:= 0;
        DataItem.Y1:= iLine;
        DataItem.X2:= NLen;
        DataItem.Y2:= iLine;
        DataItem.Level:= 2;
        DataItem.Title:= Copy(S, NFirst, NSymbol-NFirst);
        DataItem.Icon:= cIconArrow;
        Data.Add(DataItem);
      end;
    end;
  end;

  if PrevHeaderIndex>=0 then
    Data._GetItemPtr(PrevHeaderIndex)^.Y2:= St.Count-1;
end;

end.
