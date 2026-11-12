{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2025 by Michael Van Canneyt

    Markdown single markdown line

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit Markdown.Line;

{$mode ObjFPC}{$H+}

interface

uses Markdown.Utils;

type
  { TMarkdownLine }

  TMarkdownLine = class
  private
    FLine : AnsiString;
    FLineNo : integer;
    FCursor : integer;
    FMark : integer;
  public
    constructor create(const aLine : AnsiString; aLineNo : integer);
    procedure Reset;
    procedure Mark;
    procedure Rewind;
    procedure Advance(len : integer); // advance x number of spaces or characters
    // Skip whitespace from the current cursor position. Returns first non-whitespace character
    Function SkipWhiteSpace : Char;
    // Is the cursor at end of line ?
    function IsEmpty : boolean;
    // Remainder of the line, starting at cursor
    function Remainder : AnsiString;
    // Returns count of whitespaces from cursor. Tab acts as 4 position tab.
    function LeadingWhitespace : integer; inline;
    // Returns count of whitespaces from cursor. Tab acts as 4 position tab.
    // returns first non-whitespace character
    function LeadingWhitespace(out aFirstNonWhitespaceChar : Char) : integer;
    function isWhitespace : boolean; // if everything after cursor is whitespace
    // Line is the text with initial tabs replaced by spaces.
    property Line : AnsiString Read FLine;
    property LineNo : integer Read FLineNo;
    Property CursorPos : Integer Read FCursor;
  end;
  TMarkdownLineList = class (specialize TGFPObjectList<TMarkdownLine>);

implementation

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.Classes, System.SysUtils;
{$ELSE}
  Classes, SysUtils;
{$ENDIF}

{ TMarkdownLine }

constructor TMarkdownLine.create(const aLine: AnsiString; aLineNo: integer);

begin
  inherited create;
  FLine:=TransformTabs(aline);
  FLineNo:=aLineNo;
  Reset;
end;


procedure TMarkdownLine.Reset;

begin
  FCursor:=1;
end;


procedure TMarkdownLine.Rewind;
begin
  FCursor:=FMark;
end;


procedure TMarkdownLine.Advance(len: integer);
begin
  inc(FCursor,len);
end;


function TMarkdownLine.isEmpty: boolean;

begin
  Result:=FCursor>Length(FLine);
end;


function TMarkdownLine.isWhitespace: boolean;

var
  i,lLen: integer;

begin
  Result:=true;
  lLen:=Length(FLine);
  i:=FCursor;
  While Result and (i<=lLen) do
    begin
    Result:=isWhitespaceChar(Fline[i]);
    inc(I);
    end;
end;

procedure TMarkdownLine.Mark;
begin
  FMark:=FCursor;
end;


function TMarkdownLine.LeadingWhitespace : integer;
var
  lDummy : char;
begin
  Result:=LeadingWhitespace(lDummy);
end;

function TMarkdownLine.LeadingWhitespace(out aFirstNonWhitespaceChar: Char) : integer;

var
  lIndex,lLen : integer;

begin
  Result:=0;
  lIndex:=FCursor;
  lLen:=Length(FLine);
  while (lIndex<=lLen) and isWhitespaceChar(FLine[lindex]) do
    begin
    inc(Result,1);
    inc(lIndex);
    end;
  if (lIndex<=lLen) then
    aFirstNonWhitespaceChar:=Fline[lIndex]
  else
    aFirstNonWhitespaceChar:=#0;
end;


function TMarkdownLine.Remainder: AnsiString;

begin
  Result:=Copy(FLine,FCursor,Length(FLine)-FCursor+1);
end;


function TMarkdownLine.SkipWhiteSpace: Char;

var
  lLen : Integer;

begin
  lLen:=Length(FLine);
  while (FCursor<=lLen) and isWhitespaceChar(FLine[FCursor]) do
    inc(FCursor);
  if FCursor<lLen then
    Result:=FLine[FCursor+1]
  else
    Result:=#0;
end;


end.

