(*
This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at http://mozilla.org/MPL/2.0/.

Copyright (c) Alexey Torgashin
*)
unit proc_str;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes,
  StrUtils,
  LazFileUtils,
  ATStringProc,
  ATStringProc_Separator,
  ATSynEdit_RegExpr,
  ec_RegExpr;

type
  TAppSearchWordsResults = record
    MatchesCount: integer;
    MatchesArray: array[0..10] of record
      WordPos, WordLen: integer;
    end;
  end;

function SWrapLongString(const S: string; MaxLen: integer; SepChar: char): string;
function STextWholeWordSelection(const S: UnicodeString; OffsetBegin, OffsetEnd: integer;
  const ANonWordChars: UnicodeString): boolean;
procedure SDeleteDuplicateSpaces(var S: string);
function SDeleteCurlyBrackets(const S: string): string;
function STextListsFuzzyInput(const AText, AFind: string;
  out AWordResults: TAppSearchWordsResults;
  out AFuzzyResults: TATIntArray;
  AEnableFuzzy: boolean): boolean;
function SRegexReplaceSubstring(const AStr, AStrFind, AStrReplace: string; AUseSubstitute: boolean): string;
function SRegexMatchesString(const ASubject, ARegex: string; ACaseSensitive: boolean): boolean;

function SSurroundByCommas(const S: string): string;
function IsLexerListed(const AItem, AItemList: string): boolean;
function IsFilenameListedInExtensionList(const AFilename, AExtList: string): boolean;

type
  TRegexParts = array[0..8] of
    record
      Pos, Len: integer;
      Str: string;
    end;
function SRegexFindParts(const ARegex, AStr: string; out AParts: TRegexParts): boolean;
function SStringToPythonString(const Str: string; AndQuote: boolean=true): string;

function SMaskFilenameSlashes(const fn: string): string;
procedure SParseFilenameWithTwoNumbers(var fn: string; out NLine, NColumn: integer);
function IsPythonExpression(const S: string): boolean;

function SParseIconFilenameWithWidthHeight(const AStr: string;
  {out AName: string;} out AWidth, AHeight: integer): boolean;

function SEscapeRegexSpecialChars(const S: Unicodestring): Unicodestring;
function SEscapeRegexSpecialChars(const S: string): string;

function SSimpleHash(const S: string): integer;

implementation

uses
  Math;

function SStringToPythonString(const Str: string; AndQuote: boolean=true): string;
var
  i: integer;
begin
  Result:= Str;
  UniqueString(Result);
  for i:= Length(Result) downto 1 do
    case Result[i] of
      '\', '"':
        Insert('\', Result, i);
      #10:
        begin
          Result[i]:= 'n';
          Insert('\', Result, i);
        end;
      #13:
        begin
          Result[i]:= 'r';
          Insert('\', Result, i);
        end;
    end;
  if AndQuote then
    Result:= '"'+Result+'"';
end;

function SMaskFilenameSlashes(const fn: string): string;
begin
  result:= fn;
  result:= StringReplace(result, '/', '|', [rfReplaceAll]);
  result:= StringReplace(result, '\', '|', [rfReplaceAll]);
end;

function SFindFuzzyPositions(const SText, SFind: UnicodeString): TATIntArray;
var
  STextUpper, SFindUpper: UnicodeString;
  {
  //
  function IsCharSep(const ch: WideChar): boolean;
  begin
    Result:= Pos(ch, ' _.,:;/\-+*()[]{}=|''"<>?!@^&~')>0;
  end;
  //
  function IsCharUpperLetter(const ch: WideChar): boolean;
  begin
    Result:= (ch>='A') and (ch<='Z');
  end;
  //
  }
var
  N, i: integer;
begin
  Result:= nil;
  if SText='' then exit;
  if SFind='' then exit;
  STextUpper:= UnicodeUpperCase(SText);
  SFindUpper:= UnicodeUpperCase(SFind);

  {
  //commented 2024.05: block is not called at all
  //if simple match is found, don't calculate complex fuzzy matches
  N:= Pos(SFindUpper, STextUpper);
  if N>0 then
  begin
    SetLength(Result, Length(SFind));
    for i:= 0 to High(Result) do
      Result[i]:= N+i;
    Exit;
  end;
  }

  //calculate complex matches
  N:= 0;
  SetLength(Result, Length(SFindUpper));
  for i:= 1 to Length(SFindUpper) do
  begin
    N:= PosEx(SFindUpper[i], STextUpper, N+1);
    if N=0 then Exit(nil);
    Result[i-1]:= N;
  end;
end;


function SRegexFindParts(const ARegex, AStr: string; out AParts: TRegexParts): boolean;
var
  Obj: TRegExpr;
  NCount, i: integer;
begin
  Result:= false;
  for i:= Low(AParts) to High(AParts) do
  begin
    AParts[i].Pos:= -1;
    AParts[i].Len:= 0;
    AParts[i].Str:= '';
  end;

  if ARegex='' then exit;
  if AStr='' then exit;

  Obj:= TRegExpr.Create;
  try
    Obj.ModifierS:= false; //don't catch all text by .*
    Obj.ModifierM:= true; //allow to work with ^$
    Obj.ModifierI:= false;

    try
      Obj.Expression:= ARegex;
      Obj.InputString:= AStr;
      Result:= Obj.Exec;
    except
      Result:= false;
    end;

    if Result then
    begin
      NCount:= Obj.SubExprMatchCount;
      for i:= Low(AParts) to Min(High(AParts), NCount) do
      begin
        AParts[i].Pos:= Obj.MatchPos[i];
        AParts[i].Len:= Obj.MatchLen[i];
        AParts[i].Str:= Obj.Match[i];
      end;
    end;
  finally
    FreeAndNil(Obj);
  end;
end;

procedure SDeleteDuplicateSpaces(var S: string);
var
  i: integer;
begin
  for i:= Length(S) downto 2{not 1} do
    if (S[i]=' ') and (S[i-1]=' ') then
      Delete(S, i, 1);
end;

function STextListsAllWords(const AText, AFind: string;
  out AWordResults: TAppSearchWordsResults): boolean;
var
  Sep: TATStringSeparator;
  STextLower, SWordList, SWordItem: string;
  NPos: integer;
begin
  Result:= false;
  AWordResults:= Default(TAppSearchWordsResults);

  STextLower:= LowerCase(AText);
  SWordList:= LowerCase(Trim(AFind));
  SDeleteDuplicateSpaces(SWordList);

  Sep.Init(SWordList, ' ');
  repeat
    if not Sep.GetItemStr(SWordItem) then
    begin
      if AWordResults.MatchesCount>0 then
        exit(true)
      else
        Break;
    end;
    NPos:= Pos(SWordItem, STextLower);
    if NPos>0 then
    begin
      if AWordResults.MatchesCount>High(AWordResults.MatchesArray) then
        exit(false);
      Inc(AWordResults.MatchesCount);
      AWordResults.MatchesArray[AWordResults.MatchesCount-1].WordPos:= NPos;
      AWordResults.MatchesArray[AWordResults.MatchesCount-1].WordLen:= Length(SWordItem);
    end
    else
      Break;
  until false;

  AWordResults.MatchesCount:= 0;
end;

function SSurroundByCommas(const S: string): string;
begin
  SetLength(Result, Length(S)+2);
  Result[1]:= ',';
  Result[Length(Result)]:= ',';
  if S<>'' then
    Move(S[1], Result[2], Length(S));
end;

function IsLexerListed(const AItem, AItemList: string): boolean;
const
  cRegexPrefix = 'regex:';
var
  S: string;
begin
  if AItemList='' then exit(true);
  if AItem='' then exit(false);

  if SBeginsWith(AItemList, cRegexPrefix) then
  begin
    S:= Copy(AItemList, Length(cRegexPrefix)+1, MaxInt);
    Result:= SRegexMatchesString(AItem, S, true);
  end
  else
  begin
    Result:= Pos(SSurroundByCommas(AItem), SSurroundByCommas(AItemList))>0;
  end;
end;

function IsFilenameListedInExtensionList(const AFilename, AExtList: string): boolean;
var
  Ext: string;
begin
  if AExtList='*' then exit(true);
  if AExtList='' then exit(false);
  Ext:= LowerCase(ExtractFileExt(AFilename));
  if Ext='' then exit(false);
  if Ext[1]='.' then Delete(Ext, 1, 1);
  Result:= Pos(SSurroundByCommas(Ext), SSurroundByCommas(AExtList))>0;
end;

function STextListsFuzzyInput(const AText, AFind: string;
  out AWordResults: TAppSearchWordsResults;
  out AFuzzyResults: TATIntArray;
  AEnableFuzzy: boolean): boolean;
begin
  Result:= false;
  AWordResults:= Default(TAppSearchWordsResults);
  AFuzzyResults:= nil;

  if STextListsAllWords(AText, AFind, AWordResults) then
    Result:= true
  else
  if AEnableFuzzy then
  begin
    AFuzzyResults:= SFindFuzzyPositions(
      UTF8Decode(AText),
      UTF8Decode(AFind)
      );
    Result:= Length(AFuzzyResults)>0;
  end;
end;

function SRegexReplaceSubstring(const AStr, AStrFind, AStrReplace: string; AUseSubstitute: boolean): string;
var
  Obj: TRegExpr;
begin
  Result:= AStr;
  if AStr='' then exit;

  Obj:= TRegExpr.Create;
  try
    try
      Obj.ModifierS:= false;
      Obj.ModifierI:= false;
      Obj.Expression:= AStrFind;
      Result:= Obj.Replace(AStr, AStrReplace, AUseSubstitute);
    except
    end;
  finally
    Obj.Free;
  end;
end;

(*
//todo: use ATryOnce=true in new RegExpr
function SRegexMatchesString(const ASubject, ARegex: string; ACaseSensitive: boolean): boolean;
var
  Obj: TRegExpr;
begin
  Result:= false;
  if ARegex='' then exit;
  if ASubject='' then exit;

  Obj:= TRegExpr.Create;
  try
    Obj.Expression:= ARegex;
    Obj.ModifierI:= not ACaseSensitive;
    Obj.ModifierS:= false; //don't catch all text by .*
    Obj.ModifierM:= true; //allow to work with ^$
    Obj.ModifierX:= false; //don't ignore spaces
    Result:= Obj.Exec(ASubject) and (Obj.MatchPos[0]=1);
  finally
    Obj.Free;
  end;
end;
*)

function SRegexMatchesString(const ASubject, ARegex: string; ACaseSensitive: boolean): boolean;
var
  Obj: TecRegExpr;
  NPos: integer;
begin
  Obj:= TecRegExpr.Create;
  try
    Obj.Expression:= UTF8Decode(ARegex);
    Obj.ModifierI:= not ACaseSensitive;
    Obj.ModifierS:= false; //don't catch all text by .*
    Obj.ModifierM:= true; //allow to work with ^$
    Obj.ModifierX:= false; //don't ignore spaces
    NPos:= 1;
    Result:= Obj.Match(UTF8Decode(ASubject), NPos);
  finally
    Obj.Free;
  end;
end;


function SParseFilenameWithNumber(var fn: string): integer;
var
  sNum: string;
  n: integer;
begin
  Result:= 0;

  n:= RPos('@', fn);
  if n=0 then exit;

  sNum:= Copy(fn, n+1, MaxInt);

  //allow only number chars, if not then it is NTFS stream "c:/path/filename:stream"
  for n:= 1 to Length(sNum) do
    if not IsCharDigit(sNum[n]) then exit;

  Result:= StrToIntDef(sNum, 1);
  if Result=0 then
    Result:= 1;
  if Result>0 then
    SetLength(fn, Length(fn)-Length(sNum)-1);
end;

procedure SParseFilenameWithTwoNumbers(var fn: string; out NLine, NColumn: integer);
var
  n1, n2: integer;
begin
  n1:= SParseFilenameWithNumber(fn);
  n2:= SParseFilenameWithNumber(fn);

  if n2>0 then
  begin
    NLine:= n2;
    NColumn:= n1;
  end
  else
  begin
    NLine:= n1;
    NColumn:= 0;
  end;
end;

function SDeletePythonStrings(const s: string): string;
var
  status: char;
  skip: boolean;
  i: integer;
begin
  skip:= false;
  Result:= '';
  status:= #0;
  for i:= 1 to Length(s) do
    if skip then
      skip:= false
    else
    if status=#0 then
    begin
      if (s[i]=#$27) or (s[i]='"') then
      begin
        status:= s[i];
        Result+= '_';
      end
      else
        Result+= s[i]
    end
    else
    begin
      if s[i]='\' then
        skip:= true;
      if s[i]=status then
        status:= #0;
    end;
end;

function IsPythonExpression(const S: string): boolean;
const
  cTest =
    '(.*(assert|return|del|import|pass|raise|yield|def|for|with|while|if|print)\b.*)|(.*[^=><!][=][^=><].*)|(.+;.+)';
begin
  Result:= not SRegexMatchesString(
    SDeletePythonStrings(S),
    cTest,
    false);
end;

function STextWholeWordSelection(const S: UnicodeString; OffsetBegin, OffsetEnd: integer;
  const ANonWordChars: UnicodeString): boolean;
var
  ok1, ok2: boolean;
begin
  if OffsetBegin<0 then exit(false);
  if OffsetEnd>Length(S) then exit(false);
  if OffsetBegin>=OffsetEnd then exit(false);

  ok1:= (OffsetBegin=0) or not IsCharWord(S[OffsetBegin], ANonWordChars);
  ok2:= (OffsetEnd=Length(S)) or not IsCharWord(S[OffsetEnd+1], ANonWordChars);
  Result:= ok1 and ok2;
end;

function SDeleteCurlyBrackets(const S: string): string;
begin
  if (S<>'') and (S[1]='{') then
    Result:= Copy(S, 2, Length(S)-2)
  else
    Result:= S;
end;

function SWrapLongString(const S: string; MaxLen: integer; SepChar: char): string;
var
  n: integer;
begin
  if Length(S)<=MaxLen then
    exit(S);
  n:= RPosEx(' ', S, MaxLen);
  if n=0 then
    n:= MaxLen;
  Result:= Copy(S, 1, n)+SepChar+SWrapLongString(Copy(S, n+1, MaxInt), MaxLen, SepChar);
end;


function SParseIconFilenameWithWidthHeight(const AStr: string;
  {out AName: string;} out AWidth, AHeight: integer): boolean;
var
  NLen, NSep, NX, i: integer;
begin
  Result:= false;
  AWidth:= 0;
  AHeight:= 0;

  NLen:= Length(AStr);
  if NLen<5 then exit;

  i:= NLen;
  while (i>0) and IsCharDigit(AStr[i]) do Dec(i);
  NX:= i;
  if NX=0 then exit;
  if NX=NLen then exit;
  if AStr[NX]<>'x' then exit;

  Dec(i);
  while (i>0) and IsCharDigit(AStr[i]) do Dec(i);
  NSep:= i;
  if NSep=0 then exit;
  if NSep=NX then exit;
  if AStr[NSep]<>'_' then exit;

  {
  AName:= Copy(AStr, 1, NSep-1);
  if AName='' then exit;
  }

  AWidth:= StrToIntDef(Copy(AStr, NSep+1, NX-NSep-1), 0);
  if AWidth=0 then exit;

  AHeight:= StrToIntDef(Copy(AStr, NX+1, MaxInt), 0);
  if AHeight=0 then exit;

  Result:= true;
end;


const
  cRegexSpecialChars = '-+*=\()[]{}<>.,:?#$^|';

function SEscapeRegexSpecialChars(const S: Unicodestring): Unicodestring;
var
  i: integer;
begin
  Result:= '';
  for i:= 1 to Length(S) do
  begin
    if Pos(S[i], cRegexSpecialChars)=0 then
      Result+= S[i]
    else
      Result+= '\'+S[i];
  end;
end;

function SEscapeRegexSpecialChars(const S: string): string;
var
  i: integer;
begin
  Result:= '';
  for i:= 1 to Length(S) do
  begin
    if Pos(S[i], cRegexSpecialChars)=0 then
      Result+= S[i]
    else
      Result+= '\'+S[i];
  end;
end;

{$push}
{$RANGECHECKS OFF}
{$OVERFLOWCHECKS OFF}
function SSimpleHash(const S: string): integer;
var
  R: integer;
//
  procedure _Add(const Value: integer); inline;
  begin
    R:= (R shl 5) or (R shr 27);
    R:= R+Value;
  end;
//
var
  i: integer;
begin
  R:= 0;
  for i:= 1 to Length(S) do
    _Add(Ord(S[i]));
  Result:= R;
end;
{$pop}


{
var
  s: string;
initialization
  s:= SSurroundByCommas('');
  s:= SSurroundByCommas('a');
  s:= SSurroundByCommas('eee');
  s:= '';
}

end.

