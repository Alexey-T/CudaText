{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2025 by Michael Van Canneyt

    Various text processing routines & helper classes for parsing Markdown.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit Markdown.Utils;

{$mode ObjFPC}{$H+}

interface

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.Classes, System.SysUtils, System.Contnrs;
{$ELSE}
  Classes, SysUtils, Contnrs;
{$ENDIF}

const
  cSchemeStartChars =  ['a'..'z', 'A'..'Z'];
  cSchemeChars = cSchemeStartChars + ['0'..'9', '+', '.', '-'];

Type
  // Markdown text: UTF-8 bytes in an 8-bit string, with any RTL.
  TMarkdownString = AnsiString;
  // A single byte of markdown text.
  TMarkdownChar = AnsiChar;
{$IF SIZEOF(CHAR)=2}
  TMarkdownStringArray = array of TMarkdownString;
{$ELSE}
  TMarkdownStringArray = TStringArray;
{$ENDIF}
  TMarkdownStringBuilder = TAnsiStringBuilder;

  TUnicodeCharDynArray = array of unicodechar;

  { THashTable }

  THashTable = class(TFPStringHashTable)
    function Contains(const aKey : TMarkdownString) : boolean;
    function TryGet(const aKey : TMarkdownString; out aValue : TMarkdownString) : boolean;
  end;

  { TGFPObjectList }
  // Included here to be able to compile with 3.2.X
  generic TGFPObjectList<T : TObject> = class (TFPObjectList)
  private
    Type
       { TObjectEnum }
       TObjectEnum = Class
         FList : TFPObjectList;
         FIdx : Integer;
         constructor Create(aList : TFPObjectList);
         function GetCurrent : T;
         function MoveNext: Boolean;
         property Current : T read GetCurrent;
       end;
    function GetElement(aIndex: Integer): T;
    procedure SetElement(aIndex: Integer; AValue: T);
  Public
    function GetEnumerator : TObjectEnum;
    function Add(aElement : T) : integer;
    property Elements[aIndex: Integer] : T read GetElement Write SetElement; default;
  end;

{
  string operations
  Tab characters are counted as tabstops every 4 characters,
  Note that this is *not* the same as saying that "a tab equals 4 space characters".
  so #32#9 and #32#32#9 are equivalent to 4 spaces, not 5 or 6 respectively...
}

// UTF-8 encoding of S as markdown text, without code page conversion.
function UnicodeToMarkdown(const S : UnicodeString) : TMarkdownString;
// Decode the UTF-8 markdown text S.
function MarkdownToUnicode(const S : TMarkdownString) : UnicodeString;
// Markdown text for the native string S. With the Unicode RTL, S is UTF-8 encoded.
function StringToMarkdown(const S : String) : TMarkdownString; {$IF SIZEOF(CHAR)=1}inline;{$ENDIF}
// Native string for the markdown text S. With the Unicode RTL, S is UTF-8 decoded.
function MarkdownToString(const S : TMarkdownString) : String; {$IF SIZEOF(CHAR)=1}inline;{$ENDIF}
// Decimal representation of aValue.
function IntToMarkdown(aValue : Int64) : TMarkdownString;
// Convert the decimal number in S. Returns False when S is not a valid integer.
function TryMarkdownToInt(const S : TMarkdownString; out aValue : Integer) : Boolean;
// Convert the decimal number in S, aDefault when S is not a valid integer.
function MarkdownToIntDef(const S : TMarkdownString; aDefault : Integer) : Integer;
// S as a UTF8String, without code page conversion.
function MarkdownToUTF8(const S : TMarkdownString) : UTF8String;
// Content of aBuilder, without code page conversion.
function BuilderToMarkdown(aBuilder : TMarkdownStringBuilder) : TMarkdownString;

// Is aChar a whitespace character ?
function IsWhitespaceChar(aChar: TMarkdownChar): boolean;
// Does S consist of only whitespace characters ?
function IsWhitespace(const S: TMarkdownString): boolean;
// must character aChar be escaped ?
function MustEscape(C : TMarkdownChar) : boolean;
// Are all characters in S identical ?
function IsStringOfChar(const S : TMarkdownString) : boolean;
// Copy from the start of S all characters up to, but not including, the first character in aExclude
function CopyUpTo(const S : TMarkdownString; aExclude : TSysCharSet) : TMarkdownString;
// Copy from the start of S all characters after characters in aSkip
function CopySkipped(const S : TMarkdownString; aSkip : TSysCharSet) : TMarkdownString;
// Copy from the start of S all characters that match
function CopyMatching(const S : TMarkdownString; aMatches : TSysCharSet) : TMarkdownString;
{
  allows up to aWSLen whitespace characters before aMatch;
  Returns true if a match was found.
  Additionally returns the number of whitespace characters to remove from the start to get to aMatch.
}

function StartsWithWhitespace(const S : TMarkdownString; aMatch : AnsiChar; out aLength : integer; aWSLen : integer = 3) : boolean; overload;
function StartsWithWhitespace(const S : TMarkdownString; aMatch : TSysCharSet; out aLength : integer; aWSLen : integer = 3) : boolean; overload;
function StartsWithWhitespace(const S : TMarkdownString; const aMatch : TMarkdownString; out aLength : integer; aWSLen : integer = 3) : boolean; overload;
// Returns the number of space characters. Tab is a tabulator of 4
function LeadingWhitespace(const S : TMarkdownString) : integer; inline;
// Returns the number of space characters. Tab is a tabulator of 4.
function LeadingWhitespace(const S : TMarkdownString; out aTabs : integer) : integer; inline;
// Returns the number of space characters. Tab is a tabulator of 4. Returns the number of tab characters and characters considered whitespace.
function LeadingWhitespace(const S : TMarkdownString; out aTabs, aWhitespaceChars : integer) : integer;
// Returns the number of space characters. Tab is a tabulator of 4
function LengthWhitespaceCorrected(const S : TMarkdownString) : integer;
// Remove up to count spaces. Tabs are taken into account.
function RemoveLeadingWhiteSpace(const S : TMarkdownString; aCount : integer) : TMarkdownString;
// Remove ALL whitespace from S
function StripWhitespace(const S : TMarkdownString) : TMarkdownString;
// HTML escaping of < > & and " for a single character aChar
function HtmlEscape(aChar : TMarkdownChar) : TMarkdownString; overload;
// HTML escaping of < > & and " for all characters in S
function HtmlEscape(const S : TMarkdownString) : TMarkdownString; overload;
// URL escape of aChar: percent-encode characters not allowed in a URL
function UrlEscape(aChar : UnicodeChar) : TMarkdownString; overload;
// URL escape of all characters in S
function UrlEscape(const S : TMarkdownString) : TMarkdownString; overload;
// Is the string S an absolute URI ?
function IsAbsoluteUri(const S : TMarkdownString) : boolean;
// Is S a valid email address
function IsEmailLike(const S : TMarkdownString) : boolean;
// UTF-8 for a Unicode codepoint. Zero, surrogates and out-of-range values give U+FFFD
function CodePointToUTF8(aCodePoint : Integer) : TMarkdownString;
// Parse entity string
function ParseEntityString(aEntities : TFPStringHashTable; const aEntity : TMarkdownString): TMarkdownString;
// Check if S ends on an entity start character, and if so, return the length of the entity
function CheckForTrailingEntity(Const S : TMarkdownString) : integer;
// Is aChar a Unicode punctuation character ?
function IsUnicodePunctuation(aChar : UnicodeChar) : boolean;
// Count the number of characters aChar at the start of aLine
function CountStartChars(const aLine : TMarkdownString; aChar : TMarkdownChar) : integer;
// Convert the string S to an array of unicode characters
function ToUnicodeChars(const S : TMarkdownString) : TUnicodeCharDynArray;
// Transform tabulators to spaces in leading whitespace, taking into account the above definition of tabulator.
Function TransformTabs(const aLine : TMarkdownString) : TMarkdownString;
// Parse an attribute specification {#id .class key=value key="quoted value"}.
// Values use backslash escapes. aAttrs receives key=value pairs and may be Nil.
// Returns False, and leaves aAttrs unchanged, when aText is not a valid specification.
function ParseAttributeSpec(const aText : TMarkdownString; out aID : TMarkdownString; out aClasses : TMarkdownStringArray; aAttrs : TStrings) : Boolean;
// Remove a valid attribute specification from the end of aText and return it. Returns an empty string when there is none.
function ExtractTrailingAttributeSpec(var aText : TMarkdownString) : TMarkdownString;
// Parse the text of a marker comment, name[argument]: value or name: value.
// Returns False when aText is not a marker.
function ParseMarker(const aText : TMarkdownString; out aName, aArgument, aValue : TMarkdownString) : Boolean;
// Normalize a link label for lookup: trimmed, inner whitespace collapsed to one space, lowercase.
function NormalizeLinkLabel(const aLabel : TMarkdownString) : TMarkdownString;
// Identifier made from heading text: lowercase, letters, digits, marks, spaces, - and _ kept, spaces become -.
function HeadingTextToID(const aText : TMarkdownString) : TMarkdownString;

implementation

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.StrUtils, System.CodePages.unicodedata;
{$ELSE}
  StrUtils, UnicodeData;
{$ENDIF}

function UnicodeToMarkdown(const S : UnicodeString) : TMarkdownString;

var
  lUTF8 : RawByteString;

begin
  Result:='';
  lUTF8:=UTF8Encode(S);
  SetLength(Result,Length(lUTF8));
  if Length(lUTF8)>0 then
    Move(lUTF8[1],Result[1],Length(lUTF8));
end;


function MarkdownToUnicode(const S : TMarkdownString) : UnicodeString;

begin
  Result:=UTF8Decode(S);
end;


function StringToMarkdown(const S : String) : TMarkdownString;

begin
{$IF SIZEOF(CHAR)=2}
  Result:=UnicodeToMarkdown(S);
{$ELSE}
  Result:=S;
{$ENDIF}
end;


function MarkdownToString(const S : TMarkdownString) : String;

begin
{$IF SIZEOF(CHAR)=2}
  Result:=MarkdownToUnicode(S);
{$ELSE}
  Result:=S;
{$ENDIF}
end;


function IntToMarkdown(aValue : Int64) : TMarkdownString;

begin
  Str(aValue,Result);
end;


function TryMarkdownToInt(const S : TMarkdownString; out aValue : Integer) : Boolean;

var
  lCode : Integer;

begin
  Val(S,aValue,lCode);
  Result:=(lCode=0) and (Length(S)>0);
end;


function MarkdownToIntDef(const S : TMarkdownString; aDefault : Integer) : Integer;

begin
  if not TryMarkdownToInt(S,Result) then
    Result:=aDefault;
end;


function MarkdownToUTF8(const S : TMarkdownString) : UTF8String;

begin
  Result:='';
  SetLength(Result,Length(S));
  if Length(S)>0 then
    Move(S[1],Result[1],Length(S));
end;


function BuilderToMarkdown(aBuilder : TMarkdownStringBuilder) : TMarkdownString;

begin
  Result:=aBuilder.ToString(0,aBuilder.Length);
end;


// %XX encoding of aByte.
function PercentEncode(aByte : Byte) : TMarkdownString;

const
  HexDigits : Array[0..15] of TMarkdownChar = '0123456789ABCDEF';

begin
  Result:='%  ';
  Result[2]:=HexDigits[aByte shr 4];
  Result[3]:=HexDigits[aByte and $F];
end;


function CountStartChars(const aLine : TMarkdownString; aChar : TMarkdownChar) : integer;
var
  I : integer;
begin
  Result:=0;
  For I:=1 to length(aLine) do
    begin
    if aLine[I]<>aChar then
      exit;
    inc(Result);
    end;
end;

function ToUnicodeChars(const S: TMarkdownString): TUnicodeCharDynArray;

var
  i, lLen: integer;
  U: UnicodeString;

begin
  Result:=[];
  U:=MarkdownToUnicode(s);
  lLen:=Length(U);
  SetLength(Result,lLen);
  for i:=1 to lLen do
    Result[i-1]:=U[i];
end;


function IsUnicodePunctuation(aChar: UnicodeChar): boolean;

var
  Cat : Byte;

begin
  // fast check
  Result:=Not (aChar in ['0'..'9','a'..'z','A'..'Z','_']);
  if not Result then
    Exit;
  Result:=(Ord(aChar)<128) or (Ord(aChar)>=LOW_SURROGATE_BEGIN);
  if Result then
    exit;
  Cat:=GetProps(Ord(aChar))^.Category;
  Result:=(UGC_OtherNumber<Cat);
end;


function HtmlEscape(aChar: TMarkdownChar): TMarkdownString;

begin
  case aChar of
    '<' : Result:='&lt;';
    '>' : Result:='&gt;';
    '"' : Result:='&quot;';
    '&' : Result:='&amp;';
  else
    Result:=aChar;
  end;
end;


function HtmlEscape(const S: TMarkdownString): TMarkdownString;

var
  lPos,
  lTotalLen,
  lCharLen : Integer;
  lChar : TMarkdownChar;
  lRepl : String[6];

begin
  Result:='';
  lTotalLen:=0;
  lCharLen:=0;
  for lChar in S do
    begin
    Case lChar of
      '<',
      '>' : lCharLen:=4;
      '"' : lCharLen:=6;
      '&' : lCharLen:=5;
    else
      lCharLen:=1;
    end;
    Inc(lTotalLen,lCharLen);
    end;
  SetLength(Result,lTotalLen);
  lPos:=1;
  for lChar in S do
    begin
    lCharLen:=0;
    Case lChar of
      '<' : lRepl:='&lt;';
      '>' : lRepl:='&gt;';
      '"' : lRepl:='&quot;';
      '&' : lRepl:='&amp;';
    else
      Result[lPos]:=lChar;
      lCharLen:=1;
    end;
    if lCharLen=0 then
      begin
      lCharlen:=Length(lRepl);
      Move(lRepl[1],Result[lPos],lCharLen);
      end;
    Inc(lPos,lCharLen);
    end;
end;


function CopySkipped(const S: TMarkdownString; aSkip: TSysCharSet): TMarkdownString;

var
  lLen,Idx : integer;

begin
  Result:=S;
  lLen:=Length(S);
  Idx:=0;
  while (Idx<lLen) and (CharInSet(S[Idx+1],aSkip)) do
    Inc(Idx);
  Delete(Result,1,Idx);
end;


function IsStringOfChar(const S: TMarkdownString): boolean;

var
  lLen,I : integer;
  C : TMarkdownChar;

begin
  Result:=true;
  lLen:=Length(S);
  if lLen=0 then
    Exit;
  C:=S[1];
  i:=2;
  While Result and (I<=lLen) do
    begin
    Result:=(C=S[i]);
    inc(i);
    end;
end;

function CopyUpTo(const S: TMarkdownString; aExclude: TSysCharSet): TMarkdownString;

var
  lLen, Idx : Integer;

begin
  Result:=S;
  lLen:=Length(S);
  Idx:=1;
  while (Idx<=lLen) and not CharInSet(S[Idx],aExclude) do
    inc(Idx);
  SetLength(Result,Idx-1);
end;


function CopyMatching(const S: TMarkdownString; aMatches: TSysCharSet): TMarkdownString;

var
  lLen, Idx : integer;

begin
  Idx:=1;
  lLen:=Length(S);
  while (Idx<=lLen) and CharInSet(S[Idx],aMatches) do
    inc(Idx);
  if (Idx>Length(S)) then
    Result:=S
  else
    Result:=copy(S,1,Idx-1);
end;


function StartsWithWhitespace(const S: TMarkdownString; aMatch: AnsiChar; out aLength: integer; aWSLen: integer): boolean;

var
  lLen, Idx : integer;

begin
  if S='' then
    exit(false);
  aLength:=1;
  lLen:=Length(S);
  // todo: change to use leadingwhitespace to handle tabs.
  for Idx:=1 to aWSLen do
    begin
    if (aLength<=lLen) and (S[aLength]=' ') then
      inc(aLength);
    end;
  Result:=S[aLength]=aMatch;
  if Result then
    Dec(aLength);
end;

function StartsWithWhitespace(const S: TMarkdownString; aMatch: TSysCharSet; out aLength: integer; aWSLen: integer): boolean;
var
  lLen, Idx : integer;

begin
  if S='' then
    exit(false);
  aLength:=1;
  lLen:=Length(S);
  // todo: change to use leadingwhitespace to handle tabs.
  for Idx:=1 to aWSLen do
    begin
    if (aLength<=lLen) and (S[aLength]=' ') then
      inc(aLength);
    end;
  Result:=S[aLength] in aMatch;
  if Result then
    Dec(aLength);
end;

function StartsWithWhitespace(const S: TMarkdownString; const aMatch: TMarkdownString; out aLength: integer; aWSLen: integer): boolean;

var
  Len, I : integer;

begin
  if S='' then
    exit(false);
  aLength:=1;
  Len:=Length(S);
  // todo: change to use leadingwhitespace to handle tabs.
  for i:=1 to aWSLen do
    begin
    if (aLength<=Len) and (S[aLength]=' ') then
      inc(aLength);
    end;
  Result:=Copy(s,aLength,Length(aMatch))=aMatch;
  if Result then
    Dec(aLength);
end;


function LengthWhitespaceCorrected(const S: TMarkdownString): integer;

var
  lDelta,i : integer;

begin
  Result:=0;
  for i:=1 to length(S) do
    begin
    if s[i]=#9 then
      lDelta:=4-((i-1) mod 4)
    else
      lDelta:=1;
    inc(Result,lDelta);
    end;
end;

function LeadingWhitespace(const S: TMarkdownString; out aTabs, aWhitespaceChars: integer): integer;

var
  i,lLen,lDelta : integer;

begin
  aTabs:=0;
  Result:=0;
  i:=0;
  lLen:=Length(S);
  while (i<lLen) and CharInSet(S[i+1], [' ',#9]) do
    begin
    if S[i+1]=#9 then
      begin
      inc(aTabs);
      lDelta:=4-(Result mod 4);
      end
    else
      lDelta:=1;
    inc(Result,lDelta);
    inc(i);
    end;
  aWhitespaceChars:=i;
end;

function LeadingWhitespace(const S: TMarkdownString): integer;

var
  lTabs,lChars : integer;

begin
  Result:=LeadingWhitespace(S,lTabs,lChars);
end;

function LeadingWhitespace(const S: TMarkdownString; out aTabs: integer): integer;

var
  lChars : integer;

begin
  Result:=LeadingWhitespace(S,aTabs,lChars);
end;


function RemoveLeadingWhiteSpace(const S: TMarkdownString; aCount: integer): TMarkdownString;

var
  Len, Idx, Delta, lCount : integer;

begin
  Result:='';
  Idx:=0; // Index of first non-whitespace char
  lCount:=0; // whitespace count, taking into account tabstop
  Len:=Length(S);
  while (Idx<=Len) and (lCount<aCount) do
    begin
    inc(Idx);
    if (Idx<=Len) and (S[Idx] in [' ',#9]) then
      begin
      if (S[Idx]=' ') then
        Delta:=1
      else if s[Idx]=#9 then
        Delta:=4 - (lCount mod 4);
      inc(lCount,Delta);
      end
    else
      break;
    end;
  // create remainder spaces
  if lCount>aCount then
    Result:=StringOfChar(' ',lCount-aCount);
  // add non-whitespace
  Result:=Result+Copy(S,Idx+1,Len-Idx);
end;

function StripWhitespace(const S: TMarkdownString): TMarkdownString;

var
  lCount : integer;
  C : TMarkdownChar;

begin
  Result:='';
  SetLength(Result,Length(S));
  lCount:=0;
  for C in S do
    if not isWhitespace(C) then
      begin
      inc(lCount);
      Result[lCount]:=c;
      end;
  SetLength(Result,lCount);
end;

function UrlEscape(aChar: UnicodeChar): TMarkdownString;

begin
  Result:=UrlEscape(UnicodeToMarkdown(aChar));
end;

function UrlEscape(const S: TMarkdownString): TMarkdownString;

var
  C : TMarkdownChar;
  lBuilder : TMarkdownStringBuilder;

begin
  lBuilder:=TMarkdownStringBuilder.Create(Length(S));
  try
    for C in S do
      if (C in ['\', '[', ']', '"', '`']) or (Ord(C)>126) then
        lBuilder.Append(PercentEncode(Ord(C)))
      else
        lBuilder.Append(C);
    Result:=BuilderToMarkdown(lBuilder);
  finally
    lBuilder.Free;
  end;
end;

function IsWhitespaceChar(aChar: TMarkdownChar): boolean;

begin
  Result:=CharInSet(aChar,[#9,#10,#32]);
end;

function IsWhitespace(const S: TMarkdownString): boolean;

var
  C : TMarkdownChar;

begin
  Result:=true;
  for C in s do
    if not isWhitespaceChar(C) then
      exit(false);
end;

function MustEscape(C : TMarkdownChar) : boolean;

begin
  Result:=Pos(C,'!"#$%&''()*+,-./:;<=>?@[\]^_`{|}~')>0;
end;

function CodePointToUTF8(aCodePoint: Integer): TMarkdownString;

const
  MaxCodePoint = $10FFFF;

var
  U : UnicodeString;

begin
  if (aCodePoint<=0) or (aCodePoint>MaxCodePoint)
     or ((aCodePoint>=$D800) and (aCodePoint<=$DFFF)) then
    aCodePoint:=$FFFD;
  if aCodePoint<=$FFFF then
    begin
    SetLength(U,1);
    U[1]:=UnicodeChar(aCodePoint);
    end
  else
    begin
    Dec(aCodePoint,$10000);
    SetLength(U,2);
    U[1]:=UnicodeChar($D800 or (aCodePoint shr 10));
    U[2]:=UnicodeChar($DC00 or (aCodePoint and $3FF));
    end;
  Result:=UnicodeToMarkdown(U);
end;


function ParseEntityString(aEntities : TFPStringHashTable; const aEntity : TMarkdownString): TMarkdownString;

var
  S : TMarkdownString;
  i, Len : integer;

begin
  S:=copy(aEntity,2,length(aEntity)-2); // Strip & and ;
  Result:=aEntities.Items[S];
  if Result<>'' then
    Exit;
  Len:=Length(S);
  if (Len>0) and (Len<=9) and (S[1]='#') and TryMarkdownToInt(Copy(S,2,Len-1),i)  then
    Result:=CodePointToUTF8(i);
end;

function CheckForTrailingEntity(const S: TMarkdownString): integer;

var
  Tmp : TMarkdownString;
  C : TMarkdownChar;
  p,Len : Integer;

begin
  Result:=0;
  P:=RPos('&',S);
  if P=0 then
    exit;
  Len:=Length(S);
  Tmp:=Copy(S,P+1,Len-P-1);
  for C in Tmp do
    if not CharInSet(C, ['a'..'z', 'A'..'Z', '0'..'9']) then
      exit;
   exit(Length(tmp)+2);
end;

function TransformTabs(const aLine: TMarkdownString): TMarkdownString;

var
  len,tabs,wsCount : integer;

begin
  Len:=LeadingWhitespace(aLine,tabs,wsCount);
  if (Len=0) or (Tabs=0) then
    Result:=aLine
  else
    Result:=StringOfChar(' ',Len)+Copy(aLine,wsCount+1,Length(aLine)-wsCount);
end;


{ THashTable }

function THashTable.Contains(const aKey: TMarkdownString): boolean;

begin
  Result:=Find(aKey)<>Nil;
end;

function THashTable.TryGet(const aKey: TMarkdownString; out aValue: TMarkdownString): boolean;

var
  N : THTStringNode;
begin
  N:=THTStringNode(Find(aKey));
  Result:=Assigned(N);
  if Result then
    aValue:=N.Data;
end;


{ TGFPObjectList }

function TGFPObjectList.GetElement(aIndex: Integer): T;

begin
  Result:=T(Items[aIndex]);
end;

procedure TGFPObjectList.SetElement(aIndex: Integer; AValue: T);

begin
  Items[aIndex]:=aValue;
end;

function TGFPObjectList.GetEnumerator: TObjectEnum;

begin
  Result:=TObjectEnum.Create(Self);
end;

function TGFPObjectList.Add(aElement: T): integer;
begin
  Result:=Inherited add(aElement);
end;

{ TGFPObjectList.TObjectEnum }

constructor TGFPObjectList.TObjectEnum.Create(aList: TFPObjectList);
begin
  FList:=aList;
  FIdx:=-1;
end;

function TGFPObjectList.TObjectEnum.GetCurrent: T;
begin
  If FIdx<0 then
    Result:=Nil
  else
    Result:=T(FList[FIdx]);
end;

function TGFPObjectList.TObjectEnum.MoveNext: Boolean;
begin
  Inc(FIdx);
  Result:=FIdx<FList.Count;
end;


function IsAbsoluteUri(const S : TMarkdownString) : boolean;

var
  lScheme, lTail : TMarkdownString;
  i,p,lLen : integer;

begin
  Result:=False;
  p:=Pos(':',S);
  if p=0 then
    Exit;
  lScheme:=Copy(S,1,P-1);
  lLen:=Length(lScheme);
  if (lLen<2) or (lLen>32) then
    Exit;
  if not CharInSet(lScheme[1], cSchemeStartChars) then
    Exit;
  for i:=2 to lLen do
    if not CharInSet(lScheme[i], cSchemeChars) then
      Exit;
  lTail:=S;
  Delete(lTail,1,P);
  lLen:=Length(lTail);
  for i:=1 to lLen do
    if CharInSet(lTail[i], [' ', #9, #10, '<']) then
      Exit;
  Result:=true;
end;

function IsEmailLike(const S : TMarkdownString) : boolean;

type
  TState = (sNeutral,sUser,sHost,sDomain);

var
  lState : TState;
  c,prev : TMarkdownChar;

begin
  Result:=False;
  prev:=#0;
  if s.CountChar('@') <> 1 then
    Exit;
  lState:=sNeutral;
  for c in s do
    begin
    Case lState of
    sNeutral:
      if c='@' then
        Exit
      else
        lState:=sUser;
    sUser:
      if c='@' then
        lState:=sHost;
    else
      if c='.' then
        if prev='@' then
          exit
        else  
          lState:=sDomain
      else if c='+' then
        Exit;
    end;
    prev:=c;    
    end;
  if prev in ['@','.'] then
    exit;
  Result:=lState=sDomain;
end;


// Read an attribute token or value from aText at aPos, resolving backslash escapes.
function ReadAttributeToken(const aText : TMarkdownString; var aPos : Integer; aLast : Integer; aStopChars : TSysCharSet; out aToken : TMarkdownString) : Boolean;

var
  lQuote : TMarkdownChar;

begin
  aToken:='';
  Result:=False;
  if (aPos<=aLast) and (aText[aPos] in ['"','''']) then
    begin
    lQuote:=aText[aPos];
    Inc(aPos);
    while (aPos<=aLast) and (aText[aPos]<>lQuote) do
      begin
      if (aText[aPos]='\') and (aPos<aLast) and MustEscape(aText[aPos+1]) then
        Inc(aPos);
      aToken:=aToken+aText[aPos];
      Inc(aPos);
      end;
    if aPos>aLast then
      Exit;
    Inc(aPos);
    Result:=True;
    end
  else
    begin
    while (aPos<=aLast) and not (aText[aPos] in aStopChars) do
      begin
      if (aText[aPos]='\') and (aPos<aLast) and MustEscape(aText[aPos+1]) then
        Inc(aPos);
      aToken:=aToken+aText[aPos];
      Inc(aPos);
      end;
    Result:=aToken<>'';
    end;
end;


function ParseAttributeSpec(const aText : TMarkdownString; out aID : TMarkdownString; out aClasses : TMarkdownStringArray; aAttrs : TStrings) : Boolean;

const
  WhiteSpaceChars = [' ',#9,#10,#13];
  KeyChars = ['a'..'z','A'..'Z','0'..'9','_','-',':','.'];

var
  S,lToken,lKey : TMarkdownString;
  lPos,lLast : Integer;
  lPairs : TMarkdownStringArray;
  lKind : TMarkdownChar;

begin
  aID:='';
  aClasses:=[];
  lPairs:=[];
  Result:=False;
  S:=Trim(aText);
  lLast:=Length(S);
  if (lLast<2) or (S[1]<>'{') or (S[lLast]<>'}') then
    Exit;
  Dec(lLast);
  lPos:=2;
  while lPos<=lLast do
    begin
    if S[lPos] in WhiteSpaceChars then
      begin
      Inc(lPos);
      Continue;
      end;
    case S[lPos] of
      '#','.':
        begin
        lKind:=S[lPos];
        Inc(lPos);
        if not ReadAttributeToken(S,lPos,lLast,WhiteSpaceChars+['{','}','"','''','='],lToken) then
          Exit;
        if lKind='#' then
          aID:=lToken
        else
          aClasses:=Concat(aClasses,[lToken]);
        end;
      'a'..'z','A'..'Z','0'..'9','_':
        begin
        lKey:='';
        while (lPos<=lLast) and (S[lPos] in KeyChars) do
          begin
          lKey:=lKey+S[lPos];
          Inc(lPos);
          end;
        if (lPos>lLast) or (S[lPos]<>'=') then
          Exit;
        Inc(lPos);
        if not ReadAttributeToken(S,lPos,lLast,WhiteSpaceChars+['{','}','"',''''],lToken) then
          Exit;
        lPairs:=Concat(lPairs,[lKey+'='+lToken]);
        end;
    else
      Exit;
    end;
    if (lPos<=lLast) and not (S[lPos] in WhiteSpaceChars) then
      Exit;
    end;
  if Assigned(aAttrs) then
    for lToken in lPairs do
      aAttrs.Add(MarkdownToString(lToken));
  Result:=True;
end;


function ExtractTrailingAttributeSpec(var aText : TMarkdownString) : TMarkdownString;

var
  S,lID : TMarkdownString;
  lClasses : TMarkdownStringArray;
  P : Integer;

begin
  Result:='';
  S:=TrimRight(aText);
  if (S='') or (S[Length(S)]<>'}') then
    Exit;
  P:=Length(S)-1;
  while P>0 do
    begin
    if (S[P]='{') and ((P=1) or (S[P-1]<>'\')) then
      if ParseAttributeSpec(Copy(S,P,Length(S)-P+1),lID,lClasses,Nil) then
        begin
        Result:=Copy(S,P,Length(S)-P+1);
        aText:=TrimRight(Copy(S,1,P-1));
        Exit;
        end;
    Dec(P);
    end;
end;


function ParseMarker(const aText : TMarkdownString; out aName, aArgument, aValue : TMarkdownString) : Boolean;

const
  NameChars = ['a'..'z','A'..'Z','0'..'9','-','_'];

var
  S : TMarkdownString;
  lPos,lLen,lEnd : Integer;

begin
  Result:=False;
  aName:='';
  aArgument:='';
  aValue:='';
  S:=Trim(aText);
  lLen:=Length(S);
  lPos:=1;
  while (lPos<=lLen) and (S[lPos] in NameChars) do
    Inc(lPos);
  if lPos=1 then
    Exit;
  aName:=Copy(S,1,lPos-1);
  if (lPos<=lLen) and (S[lPos]='[') then
    begin
    lEnd:=Pos(']',S,lPos);
    if lEnd=0 then
      Exit;
    aArgument:=Trim(Copy(S,lPos+1,lEnd-lPos-1));
    lPos:=lEnd+1;
    end;
  if (lPos>lLen) or (S[lPos]<>':') then
    begin
    aName:='';
    aArgument:='';
    Exit;
    end;
  aValue:=Trim(Copy(S,lPos+1,lLen-lPos));
  Result:=True;
end;


function NormalizeLinkLabel(const aLabel : TMarkdownString) : TMarkdownString;

var
  C : TMarkdownChar;
  lSpace : Boolean;

begin
  Result:='';
  lSpace:=False;
  for C in Trim(aLabel) do
    if IsWhitespaceChar(C) or (C in [#10,#13]) then
      lSpace:=True
    else
      begin
      if lSpace then
        Result:=Result+' ';
      lSpace:=False;
      Result:=Result+C;
      end;
  Result:=UnicodeToMarkdown(UnicodeLowerCase(MarkdownToUnicode(Result)));
end;


function HeadingTextToID(const aText : TMarkdownString) : TMarkdownString;

var
  U,lResult : UnicodeString;
  C : UnicodeChar;
  lKeep : Boolean;

begin
  U:=UnicodeLowerCase(MarkdownToUnicode(aText));
  lResult:='';
  for C in U do
    begin
    case C of
      'a'..'z','0'..'9','-','_' : lKeep:=True;
      ' ' : lKeep:=True;
    else
      if Ord(C)<128 then
        lKeep:=False
      else if (Ord(C)>=HIGH_SURROGATE_BEGIN) and (Ord(C)<=LOW_SURROGATE_END) then
        lKeep:=True
      else
        lKeep:=GetProps(Ord(C))^.Category in [UGC_UppercaseLetter..UGC_OtherNumber,UGC_ConnectPunctuation];
    end;
    if lKeep then
      if C=' ' then
        lResult:=lResult+'-'
      else
        lResult:=lResult+C;
    end;
  Result:=UnicodeToMarkdown(lResult);
end;

end.

