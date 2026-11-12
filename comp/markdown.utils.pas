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
  TUnicodeCharDynArray = array of unicodechar;

  { THashTable }

  THashTable = class(TFPStringHashTable)
    function Contains(const aKey : string) : boolean;
    function TryGet(const aKey : string; out aValue : string) : boolean;
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
         constructor create(aList : TFPObjectList);
         function GetCurrent : T;
         function MoveNext: Boolean;
         property Current : T read GetCurrent;
       end;
    function GetElement(aIndex: Integer): T;
    procedure SetElement(aIndex: Integer; AValue: T);
  Public
    function getenumerator : TObjectEnum;
    function add(aElement : T) : integer;
    property Elements[aIndex: Integer] : T read GetElement Write SetElement; default;
  end;

{
  string operations
  Tab characters are counted as tabstops every 4 characters,
  Note that this is *not* the same as saying that "a tab equals 4 space characters".
  so #32#9 and #32#32#9 are equivalent to 4 spaces, not 5 or 6 respectively...
}

// Is aChar a whitespace character ?
function IsWhitespaceChar(aChar: char): boolean;
// Does S consist of only whitespace characters ?
function IsWhitespace(const S: String): boolean;
// must character aChar be escaped ?
function MustEscape(C : char) : boolean;
// Are all characters in S identical ?
function IsStringOfChar(const S : String) : boolean;
// Copy from the start of S all characters up to, but not including, the first character in aExclude
function CopyUpTo(const S : String; aExclude : TSysCharSet) : String;
// Copy from the start of S all characters after characters in aSkip
function CopySkipped(const S : String; aSkip : TSysCharSet) : String;
// Copy from the start of S all characters that match
function CopyMatching(const S : String; aMatches : TSysCharSet) : String;
{
  allows up to aWSLen whitespace characters before aMatch;
  Returns true if a match was found.
  Additionally returns the number of whitespace characters to remove from the start to get to aMatch.
}

function StartsWithWhitespace(const S : String; aMatch : AnsiChar; out aLength : integer; aWSLen : integer = 3) : boolean; overload;
function StartsWithWhitespace(const S : String; aMatch : TSysCharSet; out aLength : integer; aWSLen : integer = 3) : boolean; overload;
function StartsWithWhitespace(const S : String; const aMatch : String; out aLength : integer; aWSLen : integer = 3) : boolean; overload;
// Returns the number of space characters. Tab is a tabulator of 4
function LeadingWhitespace(const S : String) : integer; inline;
// Returns the number of space characters. Tab is a tabulator of 4.
function LeadingWhitespace(const S : String; out aTabs : integer) : integer; inline;
// Returns the number of space characters. Tab is a tabulator of 4. Returns the number of tab characters and characters considered whitespace.
function LeadingWhitespace(const S : String; out aTabs, aWhitespaceChars : integer) : integer;
// Returns the number of space characters. Tab is a tabulator of 4
function LengthWhitespaceCorrected(const S : String) : integer;
// Remove up to count spaces. Tabs are taken into account.
function RemoveLeadingWhiteSpace(const S : String; aCount : integer) : String;
// Remove ALL whitespace from S
function StripWhitespace(const S : String) : String;
// HTML escaping of < > & and " for a single character aChar
function HtmlEscape(aChar : char) : String; overload;
// HTML escaping of < > & and " for all characters in S
function HtmlEscape(const S : String) : String; overload;
// URL escape of aChar for display in HTML (so & is escaped)
function UrlEscape(aChar : UnicodeChar) : String; overload;
// URL escape of all characters in S
function UrlEscape(const S : String) : String; overload;
// Is the string S an absolute URI ?
function isAbsoluteUri(const S : String) : boolean;
// Is S a valid email address
function IsEmailLike(const S : String) : boolean;
// Parse entity string
function ParseEntityString(aEntities : TFPStringHashTable; const aEntity : String): String;
// Check if S ends on an entity start character, and if so, return the length of the entity
function CheckForTrailingEntity(Const S : String) : integer;
// Is aChar a Unicode punctuation character ?
function IsUnicodePunctuation(aChar : UnicodeChar) : boolean;
// Count the number of characters aChar at the start of aLine
function CountStartChars(const aLine : string; aChar : Char) : integer;
// Convert the string S to an array of unicode characters
function ToUnicodeChars(const S : String) : TUnicodeCharDynArray;
// Transform tabulators to spaces in leading whitespace, taking into account the above definition of tabulator.
Function TransformTabs(const aLine : string) : string;

implementation

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.StrUtils, System.CodePages.unicodedata;
{$ELSE}
  StrUtils, UnicodeData;
{$ENDIF}

function CountStartChars(const aLine : string; aChar : Char) : integer;
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

function ToUnicodeChars(const S: String): TUnicodeCharDynArray;

var
  i, lLen: integer;
  U: UnicodeString;

begin
  Result:=[];
  U:=UTF8Decode(s);
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


function HtmlEscape(aChar: char): String;

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


function HtmlEscape(const S: String): String;

var
  lPos,
  lTotalLen,
  lCharLen : Integer;
  lChar : char;
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


function CopySkipped(const S: String; aSkip: TSysCharSet): String;

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


function IsStringOfChar(const S: String): boolean;

var
  lLen,I : integer;
  C : char;

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

function CopyUpTo(const S: String; aExclude: TSysCharSet): String;

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


function CopyMatching(const S: String; aMatches: TSysCharSet): String;

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


function StartsWithWhitespace(const S: String; aMatch: AnsiChar; out aLength: integer; aWSLen: integer): boolean;

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

function StartsWithWhitespace(const S: String; aMatch: TSysCharSet; out aLength: integer; aWSLen: integer): boolean;
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

function StartsWithWhitespace(const S: String; const aMatch: String; out aLength: integer; aWSLen: integer): boolean;

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


function LengthWhitespaceCorrected(const S: String): integer;

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

function LeadingWhitespace(const S: String; out aTabs, aWhitespaceChars: integer): integer;

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

function LeadingWhitespace(const S: String): integer;

var
  lTabs,lChars : integer;

begin
  Result:=LeadingWhitespace(S,lTabs,lChars);
end;

function LeadingWhitespace(const S: String; out aTabs: integer): integer;

var
  lChars : integer;

begin
  Result:=LeadingWhitespace(S,aTabs,lChars);
end;


function RemoveLeadingWhiteSpace(const S: String; aCount: integer): String;

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

function StripWhitespace(const S: String): String;

var
  lCount : integer;
  C : Char;

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

function UrlEscape(aChar: UnicodeChar): String;

var
  b : TBytes;
  i : integer;

begin
  case aChar of
    '&' :
      // not escaped in URL but is escaped in html
      Result:='&amp;';
    '\', '[', ']', '"', '`' :
      Result:='%'+IntToHex(ord(aChar),2);
  else
    if ord(aChar) > $7F then
      begin
      b:=TEncoding.UTF8.GetBytes(aChar);
      Result:='';
      for i:=0 to length(b) - 1 do
        Result:=Result + '%'+IntToHex(b[i],2);
      end
    else if ord(aChar) > 126 then
      Result:='%'+IntToHex(ord(aChar),2)
    else
      Result:=AnsiChar(aChar);
  end;
end;

function UrlEscape(const S: String): String;

var
  C : UnicodeChar;

begin
  Result:='';
  for C in ToUnicodeChars(S) do
    Result:=Result+urlEscape(C);
end;

function IsWhitespaceChar(aChar: char): boolean;

begin
  Result:=CharInSet(aChar,[#9,#10,#32]);
end;

function IsWhitespace(const S: String): boolean;

var
  C : Char;

begin
  Result:=true;
  for C in s do
    if not isWhitespaceChar(C) then
      exit(false);
end;

function MustEscape(C : char) : boolean;

begin
  Result:=Pos(C,'!"#$%&''()*+,-./:;<=>?@[\]^_`{|}~')>0;
end;

function ParseEntityString(aEntities : TFPStringHashTable; const aEntity : String): String;

var
  S : String;
  C : UnicodeChar;
  i, Len : integer;

begin
  S:=copy(aEntity,2,length(aEntity)-2); // Strip & and ;
  Result:=aEntities.Items[S];
  if Result<>'' then
    Exit;
  Len:=Length(S);
  if (Len>0) and (Len<=9) and (S[1]='#') and TryStrToInt(Copy(S,2,Len-1),i)  then
    begin
    if (I<=0) or (i>65535) then
      C:=#$FFFD
    else
      C:=UnicodeChar(i);
    Result:=UTF8Encode(C);
    end;
end;

function CheckForTrailingEntity(const S: String): integer;

var
  Tmp : String;
  C : char;
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

function TransformTabs(const aLine: string): string;

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

function THashTable.Contains(const aKey: string): boolean;

begin
  Result:=Find(aKey)<>Nil;
end;

function THashTable.TryGet(const aKey: string; out aValue: string): boolean;

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

function TGFPObjectList.getenumerator: TObjectEnum;

begin
  Result:=TObjectEnum.Create(Self);
end;

function TGFPObjectList.add(aElement: T): integer;
begin
  Result:=Inherited add(aElement);
end;

{ TGFPObjectList.TObjectEnum }

constructor TGFPObjectList.TObjectEnum.create(aList: TFPObjectList);
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


function isAbsoluteUri(const S : String) : boolean;

var
  lScheme, lTail : String;
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

function IsEmailLike(const S : String) : boolean;

type
  TState = (sNeutral,sUser,sHost,sDomain);

var
  lState : TState;
  c,prev : char;

begin
  Result:=False;
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

end.

