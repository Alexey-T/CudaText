{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2025 by Michael Van Canneyt

    Markdown text scanner.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit Markdown.Scanner;

{$mode ObjFPC}{$H+}

interface

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.SysUtils,
{$ELSE}
  SysUtils,
{$ENDIF}
  Markdown.Utils, Markdown.Elements;

type

  { TMarkdownTextScanner }

  TMarkdownTextScanner = class
  private
    FText : TMarkdownString;
    FCursor : integer;
    FMark  : Integer;
    FMarkPos : TPosition;
    FPos : TPosition;
    FLineNo : integer;
    function GetEOF: boolean;
    function GetPeek: TMarkdownChar;
    function GetPeekNext: TMarkdownChar;
    function GetPeekLast: TMarkdownChar;
    function GetPeekEndRun: TMarkdownChar;
  Protected
    Property Cursor : Integer Read FCursor Write FCursor;
    Property LineNo : Integer Read FLineNo;
  public
    // Create a scanner for text, aLineNo is the line number in the markdown document.
    constructor Create(const aText : TMarkdownString; aLineNo : integer);
    destructor Destroy; override;
    // Bookmark current position of cursor
    procedure BookMark;
    // Return to bookmarked  position of cursor
    procedure GotoBookMark;
    // Check if there is a run (same characters) starting at current pos, and return the run.
    // If checkbefore is true, checks whether the previous character is NOT part of the run and returns empty if it is.
    function PeekRun(checkBefore : boolean): TMarkdownString;
    // Peek at the aLength next characters and return them.
    // Do not modify cursor position
    function PeekLen(aLength : integer) : TMarkdownString;
    // Peek at the next characters till one of the characters in aMatch is encountered.
    // return them. Do not modify cursor position
    function PeekUntil(aMatch : TSysCharSet) : TMarkdownString;
    // Peek at the next characters till one of the characters in aMatch is encountered.
    // return them. Do not modify cursor position
    function PeekWhile(aMatch : TSysCharSet) : TMarkdownString;
    // Look at the next character in the text. Advance cursor position.
    function NextChar : TMarkdownChar;
    // Look at the next run of characters in the text. Advance cursor position.
    function NextEquals : TMarkdownString; overload;
    // Get the next aCount characters. Advance the cursor position.
    function NextChars(aCount : integer) : TMarkdownString;
    // Check that the text has S starting at the next character position
    function Has(const S : TMarkdownString) : boolean;
    // Check if there is a second occurrence after an occurrence of string S at current position.
    function FindMatchingOccurrence(const S : TMarkdownString) : boolean;
    // Check if there is a second occurrence after an occurrence of string S at current position, second occurrence may not be preceded by ExcludeBefore.
    function FindMatchingOccurrence(const S : TMarkdownString; ExcludeBefore : TMarkdownChar) : boolean;
    // Skip all whitespace, advance cursor position
    procedure SkipWhitespace;
    // Current location. Takes into account offset from aLineNo
    function Location : TPosition;
    // Text from position aStart up to, not including, position aEnd.
    function TextRange(aStart, aEnd : Integer) : TMarkdownString;
    // Position of the first occurrence of aText at or after the cursor position plus aOffset, 0 if there is none.
    function FindText(const aText : TMarkdownString; aOffset : Integer = 0) : Integer;
    // Cursor position in the text
    property CursorPos : Integer read FCursor;
    // Are we at the end of the text ?
    property EOF : boolean read GetEOF;
    // Character at the current cursor position. Do not modify cursor position
    property Peek : TMarkdownChar read GetPeek;
    // Character at the next character position. Do not modify cursor position
    property PeekNext : TMarkdownChar read GetPeekNext;
    // Character at the next character position. Do not modify cursor position
    property PeekPrevious : TMarkdownChar read GetPeekLast;
    // Character after the next run of characters. Do not modify cursor position
    property PeekEndRun : TMarkdownChar read GetPeekEndRun;
  end;

implementation

{ TMarkdownTextScanner }

constructor TMarkdownTextScanner.Create(const aText: TMarkdownString; aLineNo: integer);
begin
  inherited Create;
  FText:=aText;
  FCursor:=1;
  FLineNo:=aLineNo;
  FPos.Line:=0;
  FPos.Col:=1;
end;

destructor TMarkdownTextScanner.Destroy;
begin
  inherited;
end;

procedure TMarkdownTextScanner.GotoBookMark;
begin
  FCursor:=FMark;
  FPos:=FMarkPos;
end;

function TMarkdownTextScanner.FindMatchingOccurrence(const S: TMarkdownString): boolean;
var
  i, len, LenText,lMax : integer;
begin
  Result:=false;
  LenText:=Length(FText);
  len:=Length(S);
  lMax:=LenText-Len+1;
  i:=FCursor+len+1;
  while (not Result) and (i<=lMax) do
    begin
    Result:=(FText[i-1]<>s[1])
            and ((i=lMax) or (FText[i+len] <> s[1]))
            and (copy(FText,i,len) = s);
    Inc(i);
    end;
end;

function TMarkdownTextScanner.FindMatchingOccurrence(const S: TMarkdownString; ExcludeBefore: TMarkdownChar): boolean;
var
  i, len, lenText, lMax : integer;
begin
  Result:=false;
  len:=Length(s);
  i:=FCursor+len;
  LenText:=Length(FText);
  lMax:=LenText-Len+1;
  while not Result and (I<=lMax) do
    begin
    if FText[i]=ExcludeBefore then
      exit(false);
    Result:=(FText[i-1]<>s[1])
            and ((i=lMax) or (FText[i+len]<>s[1]))
            and (Copy(FText,i,len) = s);
    Inc(i);
    end;
end;

procedure TMarkdownTextScanner.SkipWhitespace;
begin
  while isWhitespaceChar(peek) do
    NextChar();
end;

function TMarkdownTextScanner.GetEOF: boolean;
begin
  Result:=FCursor>Length(FText);
end;

function TMarkdownTextScanner.GetPeek: TMarkdownChar;
begin
  if EOF then
    Result:=#0
  else
    Result:=FText[FCursor];
end;

function TMarkdownTextScanner.GetPeekEndRun: TMarkdownChar;
var
  i,Len : integer;
  c : TMarkdownChar;
begin
  Len:=Length(FText);
  if (FCursor>=Len) then
    Result:=#0
  else
    begin
    i:=FCursor;
    c:=FText[i];
    while (i<=Len) and (FText[i]=c) do
      Inc(i);
    if (i>Len) then
      Result:=#0
    else
      Result:=FText[i];
    end;
end;

function TMarkdownTextScanner.GetPeekLast: TMarkdownChar;
begin
  if FCursor = 1 then
    Result:=#0
  else
    Result:=FText[FCursor-1];
end;

function TMarkdownTextScanner.GetPeekNext: TMarkdownChar;
begin
  if FCursor >= Length(FText) then
    Result:=#0
  else
    Result:=FText[FCursor+1];
end;

function TMarkdownTextScanner.PeekRun(checkBefore: boolean): TMarkdownString;
var
  i,Len : integer;
  c : TMarkdownChar;
begin
  c:=peek;
  i:=FCursor;
  if CheckBefore and (i>2) and (FText[i-1] = c) then
    exit('');
  Len:=Length(FText);
  while (i<=Len) and (FText[i] = c) do
    Inc(i);
  Result:=Copy(FText,FCursor,I-FCursor);
end;

function TMarkdownTextScanner.NextChars(aCount: integer): TMarkdownString;

var
  i,lCount : integer;

begin
  Result:='';
  SetLength(Result,aCount);
  lCount:=0;
  for i:=1 to aCount do
    begin
    if not EOF then
      begin
      Result[I]:=NextChar();
      Inc(lCount);
      end;
    end;
  if lCount<aCount then
    SetLength(Result,lCount);
end;

function TMarkdownTextScanner.NextEquals: TMarkdownString;

const
  Delta = 10;

var
  c : TMarkdownChar;
  lLen,lCount : Integer;
begin
  Result:='';
  lLen:=0;
  lCount:=0;
  c:=peek;
  while peek=c do
    begin
    if lCount=lLen then
      begin
      Inc(lLen,Delta);
      SetLength(Result,LLen);
      end;
    Inc(lCount);
    Result[lCount]:=nextChar;
    end;
  SetLength(Result,lCount);
end;

function TMarkdownTextScanner.NextChar: TMarkdownChar;

begin
  if EOF then
    Result:=#0
  else
    begin
    Result:=FText[FCursor];
    Inc(FCursor);
    if Result=#10 then
    begin
      FPos.Col:=1;
      Inc(FPos.Line);
    end
    else
      Inc(FPos.Col);
    end;
end;

function TMarkdownTextScanner.Has(const S: TMarkdownString): boolean;
begin
  Result:=peekLen(Length(s))=s;
end;

function TMarkdownTextScanner.Location: TPosition;
begin
  Result:=fPos;
  Inc(Result.line,FLineNo);
end;

procedure TMarkdownTextScanner.BookMark;
begin
  FMark:=FCursor;
  FMarkPos:=FPos;
end;

function TMarkdownTextScanner.PeekUntil(aMatch : TSysCharSet) : TMarkdownString;
var
  i,Len : integer;
begin
  i:=FCursor;
  Len:=Length(FText);
  while (i<=Len) and not CharInSet(FText[i],aMatch) do
    Inc(i);
  if (i>Len) then
    Result:=''
  else
    Result:=Copy(FText,FCursor,I-FCursor);
end;

function TMarkdownTextScanner.PeekWhile(aMatch: TSysCharSet): TMarkdownString;
var
  i,Len : integer;
begin
  i:=FCursor;
  Len:=Length(FText);
  while (i<=Len) and CharInSet(FText[i],aMatch) do
    Inc(i);
  Result:=Copy(FText,FCursor,I-FCursor);
end;

function TMarkdownTextScanner.PeekLen(aLength: integer): TMarkdownString;

begin
  Result:=Copy(FText,FCursor,aLength);
end;


function TMarkdownTextScanner.TextRange(aStart, aEnd: Integer): TMarkdownString;

begin
  Result:=Copy(FText,aStart,aEnd-aStart);
end;


function TMarkdownTextScanner.FindText(const aText: TMarkdownString; aOffset: Integer): Integer;

begin
  Result:=Pos(aText,FText,FCursor+aOffset);
end;

end.

