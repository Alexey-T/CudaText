(*
This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at http://mozilla.org/MPL/2.0/.

Copyright (c) Alexey Torgashin
*)
unit TreeHelper_WikidPad;

{$mode objfpc}{$H+}

interface

uses
  SysUtils,
  ATStrings,
  ATSynEdit,
  TreeHelpers_Base;

type
  TTreeHelperWikidpad = class
  private
    class function TrimHead(const S: UnicodeString): UnicodeString;
    class function GetHeadLevel(const S: UnicodeString): integer;
  public
    class procedure GetHeaders(Ed: TATSynEdit; Data: TATTreeHelperRecords);
  end;

implementation

class function TTreeHelperWikidpad.TrimHead(const S: UnicodeString): UnicodeString;
var
  i, j: integer;
begin
  i:= 1;
  while (i<=Length(S)) and ((S[i]='+') or (S[i]=' ')) do
    Inc(i);
  j:= Length(S);
  Result:= Copy(S, i, j-i+1);
end;


class function TTreeHelperWikidpad.GetHeadLevel(const S: UnicodeString): integer;
var
  NLen, r: integer;
begin
  NLen:= Length(S);
  r:= 0;
  while (r<NLen) and (S[r+1]='+') do
    Inc(r);
  Result:= r;
end;

class procedure TTreeHelperWikidpad.GetHeaders(Ed: TATSynEdit; Data: TATTreeHelperRecords);
var
  PrevHeadIndex: array[1..8] of integer = (-1, -1, -1, -1, -1, -1, -1, -1);
  St: TATStrings;
  //
  procedure ClosePrevHeader(AHeadLevel, ALine: integer);
  var
    ItemPtr: PATTreeHelperRecord;
    iHead: integer;
  begin
    for iHead:= AHeadLevel to High(PrevHeadIndex) do
      if PrevHeadIndex[iHead]>=0 then
      begin
        ItemPtr:= Data._GetItemPtr(PrevHeadIndex[iHead]);
        if ItemPtr^.Y2<0 then
        begin
          ItemPtr^.Y2:= ALine-1;
          ItemPtr^.X2:= St.LinesLen[ALine-1];
        end;
      end;

    if (AHeadLevel>=Low(PrevHeadIndex)) and (AHeadLevel<=High(PrevHeadIndex)) then
      PrevHeadIndex[AHeadLevel]:= Data.Count-1;
  end;
  //
var
  DataItem: TATTreeHelperRecord;
  HeadLevel: integer;
  SHead: UnicodeString;
  bPreformatted: boolean;
  NLen, iLine, iChar: integer;
  ch1, ch2: WideChar;
begin
  Data.Clear;
  St:= Ed.Strings;
  bPreformatted:= false;

  for iLine:= 0 to St.Count-1 do
  begin
    NLen:= St.LinesLen[iLine];

    //detect we are inside << ... >> block, skip it
    iChar:= 0;
    while (iChar<NLen) and (St.LineCharAt(iLine, iChar+1)=' ') do
      Inc(iChar);
    if (iChar+2<=NLen) then
    begin
      ch1:= St.LineCharAt(iLine, iChar+1);
      case ch1 of
        '<':
          begin
            ch2:= St.LineCharAt(iLine, iChar+2);
            if ch2='<' then
              bPreformatted:= true;
          end;
        '>':
          begin
            ch2:= St.LineCharAt(iLine, iChar+2);
            if ch2='>' then
              bPreformatted:= false;
          end;
      end;
    end;
    if bPreformatted then Continue;

    //detect header '++ Text'
    if NLen<3 then Continue; //at least 3 chars: '+ A'
    if St.LineCharAt(iLine, 1)<>'+' then Continue;

    SHead:= St.Lines[iLine];
    HeadLevel:= GetHeadLevel(SHead);

    SHead:= TrimHead(SHead);
    if SHead='' then Continue;

    if HeadLevel>0 then
    begin
      DataItem.X1:= 0;
      DataItem.Y1:= iLine;
      DataItem.X2:= 0;
      DataItem.Y2:= -1;
      DataItem.Level:= HeadLevel;
      DataItem.Title:= SHead;
      DataItem.Icon:= -1;
      Data.Add(DataItem);
      ClosePrevHeader(HeadLevel, iLine);
    end
  end;
  ClosePrevHeader(Low(PrevHeadIndex), St.Count-1);
end;

end.

