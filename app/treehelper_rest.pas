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
  public
    class procedure GetHeaders(Ed: TATSynEdit; Data: TATTreeHelperRecords);
  end;

implementation

const
  cHeaderChars = '-=\''"`:^~_*+#<>';

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

class procedure TTreeHelperRest.GetHeaders(Ed: TATSynEdit; Data: TATTreeHelperRecords);
var
  DataItem: TATTreeHelperRecord;
  St: TATStrings;
  S: UnicodeString;
  NLen, NLevel, iLine, iLevel: integer;
  HeaderChar: WideChar;
  HeaderLevels: array[1..Length(cHeaderChars)+1] of WideChar;
begin
  Data.Clear;
  St:= Ed.Strings;
  FillChar(HeaderLevels, SizeOf(HeaderLevels), 0);
  for iLine:= 1{not 0} to St.Count-1 do
  begin
    S:= St.Lines[iLine];
    if S='' then Continue;
    if IsHeaderLine(S) then
    begin
      NLen:= St.LinesLen[iLine-1];
      if (NLen>0) and (NLen<=Length(S)) then
      begin
        HeaderChar:= S[1];

        //calculate level of char, dynamically for each new document
        NLevel:= 1;
        for iLevel:= Low(HeaderLevels) to High(HeaderLevels) do
        begin
          if HeaderLevels[iLevel]=#0 then
          begin
            NLevel:= iLevel;
            HeaderLevels[iLevel]:= HeaderChar;
            Break;
          end;
          if HeaderLevels[iLevel]=HeaderChar then
          begin
            NLevel:= iLevel;
            Break;
          end;
        end;

        DataItem.X1:= 0;
        DataItem.Y1:= iLine-1;
        DataItem.X2:= 0;
        DataItem.Y2:= iLine;
        DataItem.Level:= NLevel;
        DataItem.Title:= St.Lines[iLine-1];
        DataItem.Icon:= -1;
        Data.Add(DataItem)
      end;
    end;
  end;
end;

end.

