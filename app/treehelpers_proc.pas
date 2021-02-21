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
    Title: string[39];
    class operator =(const A, B: TATTreeHelperRecord): boolean;
  end;

  { TATTreeHelperRecords }

  TATTreeHelperRecords = class(specialize TFPGList<TATTreeHelperRecord>)
  private
    function GetItemPtr(N: integer): PATTreeHelperRecord;
  public
    property ItemPtr[N: integer]: PATTreeHelperRecord read GetItemPtr;
  end;

procedure TreeHelperInPascal(Ed: TATSynEdit; const ALexer: string;
  Data: TATTreeHelperRecords);

implementation

function Markdown_IsHead(const S: UnicodeString): integer;
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

function Markdown_IsTicks(const S: UnicodeString): boolean;
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

function Markdown_IsAfterHead(const S: UnicodeString; ch: WideChar): boolean;
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

function Markdown_TrimHead(const S: UnicodeString): UnicodeString;
var
  i: integer;
begin
  i:= 1;
  while (i<=Length(S)) and ((S[i]='#') or (S[i]=' ')) do
    Inc(i);
  Result:= Copy(S, i, MaxInt);
end;

procedure Markdown_GetHeaders(Ed: TATSynEdit; Data: TATTreeHelperRecords);
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

    r:= Markdown_IsTicks(S);
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

    head:= Markdown_IsHead(S);
    if head>0 then
    begin
      DataItem.X1:= 0;
      DataItem.Y1:= iLine;
      DataItem.X2:= 0;
      DataItem.Y2:= iLine+1;
      DataItem.Level:= head;
      DataItem.Title:= Markdown_TrimHead(S);
      DataItem.Icon:= -1;
      Data.Add(DataItem)
    end
    else
    begin
      if (iLine+1<St.Count) and (S[1]<>'-') and (S[1]<>'=') then
      begin
        S2:= St.Lines[iLine+1];
        if Markdown_IsAfterHead(S2, '=') then
          head:= 1
        else
        if Markdown_IsAfterHead(S2, '-') then
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

procedure TreeHelperInPascal(Ed: TATSynEdit; const ALexer: string;
  Data: TATTreeHelperRecords);
begin
  Data.Clear;
  case ALexer of
    'Markdown':
      begin
        Markdown_GetHeaders(Ed, Data);
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

