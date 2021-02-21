(*
This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at http://mozilla.org/MPL/2.0/.

Copyright (c) Alexey Torgashin
*)
unit TreeHelpers_Proc;

{$mode ObjFPC}{$H+}
{$ModeSwitch advancedrecords}

interface

uses
  Classes, SysUtils,
  ATStrings,
  ATSynEdit,
  ATSynEdit_fgl;

type
  { TATTreeHelperRecord }

  PATTreeHelperRecord = ^TATTreeHelperRecord;
  TATTreeHelperRecord = record
    X1, Y1, X2, Y2: integer;
    Level: integer;
    Icon: integer;
    Title: string[63];
    class operator =(const A, B: TATTreeHelperRecord): boolean;
  end;

type
  { TATTreeHelperRecords }

  TATTreeHelperRecords = class(specialize TFPGList<TATTreeHelperRecord>)
  private
    function GetItemPtr(N: integer): PATTreeHelperRecord;
  public
    property ItemPtr[N: integer]: PATTreeHelperRecord read GetItemPtr;
  end;

function TreeHelperInPascal(Ed: TATSynEdit; const ALexer: string;
  Data: TATTreeHelperRecords): boolean;


implementation

type
  TTreeHelperMarkdown = class
  private
    class function IsHead(const S: UnicodeString): integer;
    class function IsTicks(const S: UnicodeString): boolean;
    class function IsAfterHead(const S: UnicodeString; ch: WideChar): boolean;
    class function TrimHead(const S: UnicodeString): UnicodeString;
  public
    class procedure GetHeaders(Ed: TATSynEdit; Data: TATTreeHelperRecords);
  end;

class function TTreeHelperMarkdown.IsHead(const S: UnicodeString): integer;
var
  r, NLen: integer;
begin
  Result:= 0;
  if S[1]<>'#' then exit;
  r:= 0;
  NLen:= Length(S);
  while (r<NLen) and (S[r+1]='#') do
    Inc(r);
  if (r<NLen) and (S[r+1]=' ') then
    Result:= r;
end;

class function TTreeHelperMarkdown.IsTicks(const S: UnicodeString): boolean;
//regex '^\s*`{3,}\s*\w*$'
var
  NLen, i, r: integer;
begin
  Result:= false;
  NLen:= Length(S);
  if NLen=0 then exit;
  i:= 0;
  while (i<NLen) and ((S[i+1]=' ') or (S[i+1]=#9)) do
    Inc(i);
  r:= 0;
  while (i<NLen) and (S[i+1]='`') do
  begin
    Inc(r);
    Inc(i);
  end;
  Result:= r>=3;
end;

class function TTreeHelperMarkdown.IsAfterHead(const S: UnicodeString; ch: WideChar): boolean;
var
  NLen, i: integer;
begin
  Result:= false;
  NLen:= Length(S);
  if NLen=0 then exit;
  for i:= 1 to Length(S) do
    if S[i]<>ch then exit;
  Result:= true;
end;

class function TTreeHelperMarkdown.TrimHead(const S: UnicodeString): UnicodeString;
var
  i: integer;
begin
  i:= 1;
  while (i<=Length(S)) and ((S[i]='#') or (S[i]=' ')) do
    Inc(i);
  Result:= Copy(S, i, MaxInt);
end;

class procedure TTreeHelperMarkdown.GetHeaders(Ed: TATSynEdit; Data: TATTreeHelperRecords);
var
  DataItem: TATTreeHelperRecord;
  St: TATStrings;
  tick, tick_r, pre, r: boolean;
  head: integer;
  S, S0, S2: UnicodeString;
  iLine: integer;
begin
  Data.Clear;
  tick:= false;
  tick_r:= false;
  pre:= false;
  St:= Ed.Strings;

  for iLine:= 0 to St.Count-1 do
  begin
    S:= St.Lines[iLine];
    if S='' then Continue;
    S0:= Trim(S);
    if S0='' then Continue;
    if S0='<pre>' then
    begin
      pre:= true;
      Continue;
    end;
    if S0='</pre>' then
    begin
      pre:= false;
      Continue;
    end;
    if pre then
      Continue;

    r:= IsTicks(S);
    if r then
    begin
      if tick and (r=tick_r) then
      begin
        tick:= false;
        tick_r:= false;
      end
      else
      begin
        tick:= true;
        tick_r:= r;
      end;
      Continue;
    end;
    if tick then
      Continue;

    head:= IsHead(S);
    if head>0 then
    begin
      DataItem.X1:= 0;
      DataItem.Y1:= iLine;
      DataItem.X2:= 0;
      DataItem.Y2:= iLine+1;
      DataItem.Level:= head;
      DataItem.Title:= TrimHead(S);
      DataItem.Icon:= -1;
      Data.Add(DataItem)
    end
    else
    begin
      if (iLine+1<St.Count) and (S[1]<>'-') and (S[1]<>'=') then
      begin
        S2:= St.Lines[iLine+1];
        if IsAfterHead(S2, '=') then
          head:= 1
        else
        if IsAfterHead(S2, '-') then
          head:= 2
        else
          head:= 0;
        if head>0 then
        begin
          DataItem.X1:= 0;
          DataItem.Y1:= iLine;
          DataItem.X2:= 0;
          DataItem.Y2:= iLine+1;
          DataItem.Level:= head;
          DataItem.Title:= S0;
          DataItem.Icon:= -1;
          Data.Add(DataItem)
        end;
      end;
    end;
  end;
end;


type
  TTreeHelperMediawiki = class
  private
    class function TrimHead(const S: UnicodeString): UnicodeString;
    class function GetHead(const S: UnicodeString): integer;
  public
    class procedure GetHeaders(Ed: TATSynEdit; Data: TATTreeHelperRecords);
  end;

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


class function TTreeHelperMediawiki.GetHead(const S: UnicodeString): integer;
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
  head: integer;
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
    head:= GetHead(S);
    if head>0 then
    begin
      DataItem.X1:= 0;
      DataItem.Y1:= iLine;
      DataItem.X2:= 0;
      DataItem.Y2:= iLine+1;
      DataItem.Level:= head;
      DataItem.Title:= TrimHead(S);
      DataItem.Icon:= -1;
      Data.Add(DataItem)
    end
  end;
end;

type
  TTreeHelperRest = class
  private
    class function IsHead(const S: UnicodeString; ch: WideChar): boolean;
    class function IsArr(const S: UnicodeString): boolean;
    class function GetLevel(ch: WideChar): integer;
  public
    class procedure GetHeaders(Ed: TATSynEdit; Data: TATTreeHelperRecords);
  end;

class function TTreeHelperRest.IsHead(const S: UnicodeString; ch: WideChar): boolean;
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

class function TTreeHelperRest.IsArr(const S: UnicodeString): boolean;
const
  arr: UnicodeString = '-=\''"`:^~_*+#<>';
var
  i: integer;
begin
  for i:= 1 to Length(arr) do
    if IsHead(S, arr[i]) then
      exit(true);
  Result:= false;
end;

class function TTreeHelperRest.GetLevel(ch: WideChar): integer;
begin
  case ch of
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
  NLen, iLine: integer;
begin
  Data.Clear;
  St:= Ed.Strings;
  for iLine:= 1{not 0} to St.Count-1 do
  begin
    S:= St.Lines[iLine];
    if S='' then Continue;
    if IsArr(S) then
    begin
      NLen:= St.LinesLen[iLine-1];
      if (NLen>0) and (NLen<=Length(S)) then
      begin
        DataItem.X1:= 0;
        DataItem.Y1:= iLine-1;
        DataItem.X2:= 0;
        DataItem.Y2:= iLine;
        DataItem.Level:= GetLevel(S[1]);
        DataItem.Title:= St.Lines[iLine-1];
        DataItem.Icon:= -1;
        Data.Add(DataItem)
      end;
    end;
  end;
end;

//--------------------------------------------------------------
function TreeHelperInPascal(Ed: TATSynEdit; const ALexer: string;
  Data: TATTreeHelperRecords): boolean;
begin
  Result:= false;
  Data.Clear;
  case ALexer of
    'Markdown':
      begin
        Result:= true;
        TTreeHelperMarkdown.GetHeaders(Ed, Data);
      end;
    'MediaWiki':
      begin
        Result:= true;
        TTreeHelperMediawiki.GetHeaders(Ed, Data);
      end;
    'reStructuredText':
      begin
        Result:= true;
        TTreeHelperRest.GetHeaders(Ed, Data);
      end;
  end;
end;

{ TATTreeHelperRecords }

function TATTreeHelperRecords.GetItemPtr(N: integer): PATTreeHelperRecord;
begin
  Result:= PATTreeHelperRecord(InternalGet(N));
end;


{ TATTreeHelperRecord }

class operator TATTreeHelperRecord.=(const A, B: TATTreeHelperRecord): boolean;
begin
  Result:= false;
end;

end.

