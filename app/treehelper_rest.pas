(*
This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at http://mozilla.org/MPL/2.0/.

Copyright (c) Alexey Torgashin
*)
unit TreeHelper_reST;

{$mode objfpc}{$H+}

interface

uses
  SysUtils,
  ATStrings,
  ATSynEdit,
  TreeHelpers_Base;

type
  TTreeHelperRest = class
  private
    class function IsHeaderOfChar(const S: UnicodeString; ch: WideChar): boolean;
    class function IsHeaderLine(const S: UnicodeString): boolean;
    class function GetLevel(NewChar, PrevChar: WideChar): integer;
  public
    class procedure GetHeaders(Ed: TATSynEdit; Data: TATTreeHelperRecords);
  end;

implementation

const
  cHeaderChars: UnicodeString = '-=\''"`:^~_*+#<>';

class function TTreeHelperRest.IsHeaderOfChar(const S: UnicodeString; ch: WideChar): boolean;
var
  i: integer;
begin
  Result:= false;
  if S='' then exit;
  for i:= 1 to Length(S) do
    if S[i]<>ch then
      exit;
  Result:= true;
end;

class function TTreeHelperRest.IsHeaderLine(const S: UnicodeString): boolean;
var
  i: integer;
begin
  for i:= 1 to Length(cHeaderChars) do
    if IsHeaderOfChar(S, cHeaderChars[i]) then
      exit(true);
  Result:= false;
end;

class function TTreeHelperRest.GetLevel(NewChar, PrevChar: WideChar): integer;
begin
  case NewChar of
    '=': Result:= 1;
    '-': Result:= 2;
    '~': Result:= 3;
    '"': Result:= 4;
    else Result:= 1;
  end;
end;

class procedure TTreeHelperRest.GetHeaders(Ed: TATSynEdit; Data: TATTreeHelperRecords);
var
  DataItem: TATTreeHelperRecord;
  St: TATStrings;
  S: UnicodeString;
  PrevHeaderChar, NewHeaderChar: WideChar;
  NLen, iLine: integer;
begin
  Data.Clear;
  PrevHeaderChar:= #0;
  NewHeaderChar:= #0;
  St:= Ed.Strings;
  for iLine:= 1{not 0} to St.Count-1 do
  begin
    S:= St.Lines[iLine];
    if S='' then Continue;
    if IsHeaderLine(S) then
    begin
      PrevHeaderChar:= NewHeaderChar;
      NewHeaderChar:= S[1];
      NLen:= St.LinesLen[iLine-1];
      if (NLen>0) and (NLen<=Length(S)) then
      begin
        DataItem.X1:= 0;
        DataItem.Y1:= iLine-1;
        DataItem.X2:= 0;
        DataItem.Y2:= iLine;
        DataItem.Level:= GetLevel(NewHeaderChar, PrevHeaderChar);
        DataItem.Title:= St.Lines[iLine-1];
        DataItem.Icon:= -1;
        Data.Add(DataItem)
      end;
    end;
  end;
end;

end.

