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
  St: TATStrings;
  S: UnicodeString;
  iLine, iSymbol: integer;
begin
  Data.Clear;
  St:= Ed.Strings;
  for iLine:= 0 to St.Count-1 do
  begin
    S:= St.Lines[iLine];
    if S='' then Continue;

    //skip commented lines
    iSymbol:= 1;
    while (iSymbol<=Length(S)) and (S[iSymbol]=' ') do
      Inc(iSymbol);
    if iSymbol>Length(S) then Continue;
    if S[iSymbol]=';' then Continue;
    if S[iSymbol]='#' then Continue;

    if (Length(S)>=3) and (S[1]='[') and (S[Length(S)]=']') then
    begin
      DataItem.X1:= 0;
      DataItem.Y1:= iLine;
      DataItem.X2:= Length(S);
      DataItem.Y2:= iLine;
      DataItem.Level:= 1;
      DataItem.Title:= S;
      DataItem.Icon:= cIconFolder;
      Data.Add(DataItem);
    end
    else
    begin
      iSymbol:= Pos('=', S);
      if iSymbol>0 then
      begin
        DataItem.X1:= 0;
        DataItem.Y1:= iLine;
        DataItem.X2:= Length(S);
        DataItem.Y2:= iLine;
        DataItem.Level:= 2;
        DataItem.Title:= Copy(S, 1, iSymbol-1);
        DataItem.Icon:= cIconArrow;
        Data.Add(DataItem);
      end;
    end;
  end;
end;

end.
