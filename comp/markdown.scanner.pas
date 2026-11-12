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
  Markdown.Elements;

type

  { TMarkdownTextScanner }

  TMarkdownTextScanner = class
  private
    FText : String;
    FCursor : integer;
    FMark  : Integer;
    FMarkPos : TPosition;
    FPos : TPosition;
    FLineNo : integer;
    function GetEOF: boolean;
    function GetPeek: char;
    function GetPeekNext: char;
    function GetPeekLast: char;
    function GetPeekEndRun: char;
  Protected
    Property Cursor : Integer Read FCursor Write FCursor;
    Property LineNo : Integer Read FLineNo;
  public
    // Create a scanner for text, aLineNo is the line number in the markdown document.
    constructor Create(const aText : String; aLineNo : integer);
    destructor Destroy; override;
    // Bookmark current position of cursor
    procedure BookMark;
    // Return to bookmarked  position of cursor
    procedure GotoBookMark;
    // Check if there is a run (same characters) starting at current pos, and return the run.
    // If checkbefore is true, checks whether the previous character is NOT part of the run and returns empty if it is.
    function PeekRun(checkBefore : boolean): String;
    // Peek at the aLength next characters and return them.
    // Do not modify cursor position
    function PeekLen(aLength : integer) : String;
    // Peek at the next characters till one of the characters in aMatch is encountered.
    // return them. Do not modify cursor position
    function PeekUntil(aMatch : TSysCharSet) : String;
    // Peek at the next characters till one of the characters in aMatch is encountered.
    // return them. Do not modify cursor position
    function PeekWhile(aMatch : TSysCharSet) : String;
    // Look at the next character in the text. Advance cursor position.
    function NextChar : char;
    // Look at the next run of characters in the text. Advance cursor position.
    function NextEquals : String; overload;
    // Get the next aCount characters. Advance the cursor position.
    function NextChars(aCount : integer) : String;
    // Check that the text has S starting at the next character position
    function Has(const S : string) : boolean;
    // Check if there is a second occurrence after an occurrence of string S at current position.
    function FindMatchingOccurrence(const S : String) : boolean;
    // Check if there is a second occurrence after an occurrence of string S at current position, second occurrence may not be preceded by ExcludeBefore.
    function FindMatchingOccurrence(const S : String; ExcludeBefore : char) : boolean;
    // Skip all whitespace, advance cursor position
    procedure SkipWhitespace;
    // Current location. Takes into account offset from aLineNo
    function Location : TPosition;
    // Are we at the end of the text ?
    property EOF : boolean read GetEOF;
    // Character at the current cursor position. Do not modify cursor position
    property Peek : Char read GetPeek;
    // Character at the next character position. Do not modify cursor position
    property PeekNext : Char read GetPeekNext;
    // Character at the next character position. Do not modify cursor position
    property PeekPrevious : Char read GetPeekLast;
    // Character after the next run of characters. Do not modify cursor position
    property PeekEndRun : Char read GetPeekEndRun;
  end;

implementation

uses Markdown.Utils;

{ TMarkdownTextScanner }

constructor TMarkdownTextScanner.Create(const aText: String; aLineNo: integer);
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

function TMarkdownTextScanner.FindMatchingOccurrence(const S: String): boolean;
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

function TMarkdownTextScanner.FindMatchingOccurrence(const S: String; ExcludeBefore: char): boolean;
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

function TMarkdownTextScanner.GetPeek: char;
begin
  if EOF then
    Result:=#0
  else
    Result:=FText[FCursor];
end;

function TMarkdownTextScanner.GetPeekEndRun: char;
var
  i,Len : integer;
  c : char;
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

function TMarkdownTextScanner.GetPeekLast: char;
begin
  if FCursor = 1 then
    Result:=#0
  else
    Result:=FText[FCursor-1];
end;

function TMarkdownTextScanner.GetPeekNext: char;
begin
  if FCursor >= Length(FText) then
    Result:=#0
  else
    Result:=FText[FCursor+1];
end;

function TMarkdownTextScanner.PeekRun(checkBefore: boolean): String;
var
  i,Len : integer;
  c : char;
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

function TMarkdownTextScanner.NextChars(aCount: integer): String;

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

function TMarkdownTextScanner.NextEquals: String;

const
  Delta = 10;

var
  c : char;
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

function TMarkdownTextScanner.NextChar: char;

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

function TMarkdownTextScanner.Has(const S: string): boolean;
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

function TMarkdownTextScanner.PeekUntil(aMatch : TSysCharSet) : String;
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

function TMarkdownTextScanner.PeekWhile(aMatch: TSysCharSet): String;
var
  i,Len : integer;
begin
  i:=FCursor;
  Len:=Length(FText);
  while (i<=Len) and CharInSet(FText[i],aMatch) do
    Inc(i);
  Result:=Copy(FText,FCursor,I-FCursor);
end;

function TMarkdownTextScanner.PeekLen(aLength: integer): String;

begin
  Result:=Copy(FText,FCursor,aLength);
end;

end.

