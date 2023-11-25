(*
This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at http://mozilla.org/MPL/2.0/.

Copyright (c) Alexey Torgashin
*)
unit TreeHelper_MediaWiki;

{$mode objfpc}{$H+}

interface

uses
  SysUtils,
  ATStrings,
  ATSynEdit,
  TreeHelpers_Base;

type
  TTreeHelperMediawiki = class
  private
    class function TrimHead(const S: UnicodeString): UnicodeString;
    class function GetHeadLevel(const S: UnicodeString): integer;
  public
    class procedure GetHeaders(Ed: TATSynEdit; Data: TATTreeHelperRecords);
  end;

implementation

class function TTreeHelperMediawiki.TrimHead(const S: UnicodeString): UnicodeString;
var
  i, j: integer;
begin
  i:= 1;
  while (i<=Length(S)) and ((S[i]='=') or (S[i]=' ')) do
    Inc(i);
  j:= Length(S);
  while (j>0) and ((S[j]='=') or (S[j]=' ')) do
    Dec(j);
  Result:= Copy(S, i, j-i+1);
end;


class function TTreeHelperMediawiki.GetHeadLevel(const S: UnicodeString): integer;
var
  NLen, r, r2, i: integer;
begin
  NLen:= Length(S);
  r:= 0;
  while (r<NLen) and (S[r+1]='=') do
    Inc(r);

  r2:= 0;
  i:= NLen;
  while (i>0) and (s[i]='=') do
  begin
    Dec(i);
    Inc(r2);
  end;

  if r2>=r then
    Result:= r
  else
    Result:= 0;
end;

class procedure TTreeHelperMediawiki.GetHeaders(Ed: TATSynEdit; Data: TATTreeHelperRecords);
var
  DataItem: TATTreeHelperRecord;
  St: TATStrings;
  HeadLevel: integer;
  S: UnicodeString;
  iLine: integer;
begin
  Data.Clear;
  St:= Ed.Strings;
  for iLine:= 0 to St.Count-1 do
  begin
    S:= St.Lines[iLine];
    if S='' then Continue;
    if S[1]<>'=' then Continue;
    if S[Length(S)]<>'=' then Continue;
    HeadLevel:= GetHeadLevel(S);
    if HeadLevel>0 then
    begin
      DataItem.X1:= 0;
      DataItem.Y1:= iLine;
      DataItem.X2:= 0;
      DataItem.Y2:= iLine+1;
      DataItem.Level:= HeadLevel;
      DataItem.Title:= TrimHead(S);
      DataItem.Icon:= -1;
      Data.Add(DataItem)
    end
  end;
end;

end.

