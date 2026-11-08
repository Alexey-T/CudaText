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
  ATStringProc;

const
  AppLexerPrefixRegex = 'regex:';

type
  TAppSearchWordsResults = record
    MatchesCount: integer;
    MatchesArray: array[0..10] of record
      WordPos, WordLen: integer;
    end;
  end;

function SWrapLongStringBySeparator(const S: string; AMaxLen: integer; ASepChar: char): string;
function STextWholeWordSelection(const S: UnicodeString; AOffsetBegin, AOffsetEnd: integer;
  const ANonWordChars: UnicodeString): boolean;
function SDeleteCurlyBrackets(const S: string): string;
function STextListsFuzzyInput(const AText, AFind: string;
  out AWordResults: TAppSearchWordsResults;
  out AFuzzyResults: TATIntArray;
  AEnableFuzzy: boolean): boolean;

function SRegexReplaceSubstring(const AStr, AStrFind, AStrReplace: string; AUseSubstitute: boolean): string;
function SRegexMatchesString(const ASubject, ARegex: string; ACaseSensitive: boolean): boolean;
function SRegexMatches_HtmlLexers(const ASubject: string; const AOptionRegex: UnicodeString): boolean;
function SRegexMatches_CssLexers(const ASubject: string; const AOptionRegex: UnicodeString): boolean;

function SMatchesBeginWithTrailingDigits(const ASubject, ABegin: string): boolean;

function SSurroundByCommas(const S: string): string;
function IsLexerListed(const AItem, AItemList: string): boolean;
function IsFilenameListedInExtensionList(const AFilename, AExtList: string): boolean;

type
  TAppRegexParts = array[0..8] of
    record
      Pos, Len: integer;
      Str: string;
    end;

function SRegexFindParts(const ARegex, AStr: string; out AParts: TAppRegexParts): boolean;
function SStringToPythonString(const Str: string; AndQuote: boolean=true): string;
function SStringHasBinaryChars(const S: string): boolean;
function SStringHasBinaryChars(const S: UnicodeString): boolean;

function SMaskFilenameSlashes(const S: string): string;
procedure SParseFilenameWithTwoNumbers(var AFilename: string; out ALine, AColumn: integer);
function IsPythonExpression(const S: string): boolean;

function SParseIconFilenameWithWidthHeight(const AStr: string;
  {out AName: string;} out AWidth, AHeight: integer): boolean;

function SEscapeRegexSpecialChars(const S: Unicodestring): Unicodestring;
function SEscapeRegexSpecialChars(const S: string): string;

function ConvertMillisecondsToNiceString(const N: QWord): string;

implementation

uses
  SysUtils,
  Math,
  Classes,
  StrUtils,
  LazFileUtils,
  ATStringProc_Separator,
  ATSynEdit_RegExpr;

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

function SMaskFilenameSlashes(const S: string): string;
var
  i: SizeInt;
begin
  Result:= S;
  UniqueString(Result);
  for i:= 1 to Length(Result) do
    if (Result[i]='/') or (Result[i]='\') then
      Result[i]:= '|';
end;

function SFindFuzzyPositions(const SText, SFind: UnicodeString): TATIntArray;
var
  STextUpper, SFindUpper: UnicodeString;
  N, i: integer;
begin
  Result:= nil;
  if SText='' then exit;
  if SFind='' then exit;
  STextUpper:= UnicodeUpperCase(SText);
  SFindUpper:= UnicodeUpperCase(SFind);

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


function SRegexFindParts(const ARegex, AStr: string; out AParts: TAppRegexParts): boolean;
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
  SCompressRepeatingSpacesAndEols(SWordList);

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
var
  NLen: SizeInt;
begin
  NLen:= Length(S);
  SetLength(Result, NLen+2);
  Result[1]:= ',';
  Result[Length(Result)]:= ',';
  if NLen>0 then
    Move(S[1], Result[2], NLen);
end;

function IsLexerListed(const AItem, AItemList: string): boolean;
var
  S: string;
begin
  if AItemList='' then exit(true);
  if AItem='' then exit(false);

  if SBeginsWith(AItemList, AppLexerPrefixRegex) then
  begin
    S:= Copy(AItemList, Length(AppLexerPrefixRegex)+1, MaxInt);
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
  iMax, i: SizeInt;
begin
  if AExtList='*' then exit(true);
  if AExtList='' then exit(false);

  Ext:= ExtractFileExt(AFileName);
  if Ext='' then exit(false);

  //ascii lowercase
  for i:= 1 to Length(Ext) do
    if (Ord(Ext[i])>=Ord('A')) and (Ord(Ext[i])<=Ord('Z')) then
      Inc(Ext[i], 32);
  if Ext[1]='.' then
    Delete(Ext, 1, 1);

  if Ext='' then exit(false);

  iMax:= Length(AExtList)-Length(Ext)+1; // 'aaa,bbb'
  if iMax<1 then exit(false);

  i:= 0;
  repeat
    i:= Pos(Ext, AExtList, i+1);
    if i=0 then exit(false);
    if ((i=1) or (AExtList[i-1]=',')) and
      ((i=iMax) or (AExtList[i+Length(Ext)]=',')) then exit(true);
  until false;
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

function SRegexMatchesString(const ASubject, ARegex: string; ACaseSensitive: boolean): boolean;
var
  Obj: TRegExpr;
begin
  Result:= false;
  if ARegex='' then exit;

  Obj:= TRegExpr.Create;
  try
    try
      Obj.ModifierI:= not ACaseSensitive;
      Obj.ModifierS:= false; //don't catch all text by .*
      Obj.ModifierM:= true; //allow to work with ^$
      Obj.ModifierX:= false; //don't ignore spaces
      Obj.Expression:= UTF8Decode(ARegex);
      Obj.InputString:= UTF8Decode(ASubject); //ExecPos() uses already set InputString
      Result:= Obj.ExecPos(1, true, false); //ATryOnce=true: match only at offset 1
    except
      on ERegExpr do
        Result:= false; //bad regex, like it was with TecRegExpr
    end;
  finally
    Obj.Free;
  end;
end;

function SRegexMatches_AnyLexers(const ASubject: string; const AOptionRegex: UnicodeString; var AObj: TRegExpr): boolean;
begin
  if not Assigned(AObj) then
  begin
    AObj:= TRegExpr.Create;
    AObj.ModifierI:= true;
    AObj.ModifierS:= false; //don't catch all text by .*
    AObj.ModifierM:= true; //allow to work with ^$
    AObj.ModifierX:= false; //don't ignore spaces
  end;

  Result:= false;
  try
    if AObj.Expression<>AOptionRegex then
    begin
      AObj.Expression:= AOptionRegex;
      AObj.Compile;
    end;

    AObj.InputString:= UTF8Decode(ASubject); //ExecPos() uses already set InputString
    Result:= AObj.ExecPos(1, true, false); //ATryOnce=true: match only at offset 1
  except
    on ERegExpr do
      Result:= false; //bad regex
  end;
end;

var
  _CachedHtmlRe: TRegExpr = nil;
  _CachedCssRe: TRegExpr = nil;

function SRegexMatches_HtmlLexers(const ASubject: string; const AOptionRegex: UnicodeString): boolean;
begin
  Result:= SRegexMatches_AnyLexers(ASubject, AOptionRegex, _CachedHtmlRe);
end;

function SRegexMatches_CssLexers(const ASubject: string; const AOptionRegex: UnicodeString): boolean;
begin
  Result:= SRegexMatches_AnyLexers(ASubject, AOptionRegex, _CachedCssRe);
end;

function SMatchesBeginWithTrailingDigits(const ASubject, ABegin: string): boolean;
var
  i: integer;
begin
  Result:= false;
  if ASubject='' then exit;
  if ABegin='' then exit;
  if Length(ASubject)<=Length(ABegin) then exit;
  for i:= 1 to Length(ABegin) do
    if ASubject[i]<>ABegin[i] then exit;
  for i:= Length(ABegin)+1 to Length(ASubject) do
    if not (ASubject[i] in ['0'..'9']) then exit;
  Result:= true;
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

procedure SParseFilenameWithTwoNumbers(var AFilename: string; out ALine, AColumn: integer);
var
  n1, n2: integer;
begin
  n1:= SParseFilenameWithNumber(AFilename);
  n2:= SParseFilenameWithNumber(AFilename);

  if n2>0 then
  begin
    ALine:= n2;
    AColumn:= n1;
  end
  else
  begin
    ALine:= n1;
    AColumn:= 0;
  end;
end;

function SMaskPythonStrConsts(const S: string): string;
//Delete string-constants (single/double quoted, quote-char in string can be escaped by backslash)
//from Python expression, ie replace them with '_'
var
  i, j: integer;
  StrChar: char;
  bSkip: boolean;
begin
  SetLength(Result, Length(S));
  bSkip:= false;
  j:= 0;
  StrChar:= #0;
  for i:= 1 to Length(S) do
    if bSkip then
      bSkip:= false
    else
    if StrChar=#0 then
    begin
      Inc(j);
      if (S[i]='''') or (S[i]='"') then
      begin
        StrChar:= S[i];
        Result[j]:= '_';
      end
      else
        Result[j]:= S[i];
    end
    else
    begin
      if S[i]='\' then
        bSkip:= true;
      if S[i]=StrChar then
        StrChar:= #0;
    end;
  SetLength(Result, j);
end;

function IsPythonExpression(const S: string): boolean;
const
  cTest =
    '(.*\b(assert|return|del|import|pass|raise|yield|((async\s+|await\s+)?def)|for|with|while|if|print)\b.*)|(.*[^=><!][=][^=><].*)|(.+;.+)';
begin
  Result:= not SRegexMatchesString(
    SMaskPythonStrConsts(S),
    cTest,
    true);
end;

(*
function SExtractFileDoubleExt(const AFileName: string): string;
var
  fn: string;
  i: SizeInt;
begin
  Result:= '';
  fn:= ExtractFileName(AFileName);
  i:= RPos('.', fn);
  if i>1 then //>1, not first char
  begin
    i:= RPosEx('.', fn, i-1);
    if i>1 then //>1, not first char
      Result:= Copy(fn, i, MaxInt);
  end;
end;
*)

function STextWholeWordSelection(const S: UnicodeString; AOffsetBegin, AOffsetEnd: integer;
  const ANonWordChars: UnicodeString): boolean;
var
  ok1, ok2: boolean;
begin
  if AOffsetBegin<0 then exit(false);
  if AOffsetEnd>Length(S) then exit(false);
  if AOffsetBegin>=AOffsetEnd then exit(false);

  ok1:= (AOffsetBegin=0) or not IsCharWord(S[AOffsetBegin], ANonWordChars);
  ok2:= (AOffsetEnd=Length(S)) or not IsCharWord(S[AOffsetEnd+1], ANonWordChars);
  Result:= ok1 and ok2;
end;

function SDeleteCurlyBrackets(const S: string): string;
begin
  if (S<>'') and (S[1]='{') then
    Result:= Copy(S, 2, Length(S)-2)
  else
    Result:= S;
end;

function SWrapLongStringBySeparator(const S: string; AMaxLen: integer; ASepChar: char): string;
var
  n: integer;
begin
  if Length(S)<=AMaxLen then
    exit(S);
  n:= RPosEx(' ', S, AMaxLen);
  if n=0 then
    n:= AMaxLen;
  Result:= Copy(S, 1, n)+ASepChar+SWrapLongStringBySeparator(Copy(S, n+1, MaxInt), AMaxLen, ASepChar);
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
  cRegexChars: set of byte = [
    Ord('-'), Ord('+'), Ord('*'), Ord('\'),
    Ord('('), Ord(')'), Ord('['), Ord(']'), Ord('{'), Ord('}'),
    Ord('.'), Ord('?'), Ord('#'), Ord('$'), Ord('^'), Ord('|')
    ];

function SEscapeRegexSpecialChars(const S: Unicodestring): Unicodestring;
var
  i, i2: SizeInt;
begin
  SetLength(Result, Length(S)*2); //worst case: each char needs 2 chars
  i2:= 0;
  for i:= 1 to Length(S) do
    if (Ord(S[i])<256) and (Ord(S[i]) in cRegexChars) then
    begin
      Result[i2+1]:= '\';
      Result[i2+2]:= S[i];
      Inc(i2, 2);
    end
    else
    begin
      Result[i2+1]:= S[i];
      Inc(i2);
    end;
  SetLength(Result, i2); //truncate to really used chars
end;

function SEscapeRegexSpecialChars(const S: string): string;
var
  i, i2: SizeInt;
begin
  SetLength(Result, Length(S)*2); //worst case: each char needs 2 chars
  i2:= 0;
  for i:= 1 to Length(S) do
    if Ord(S[i]) in cRegexChars then
    begin
      Result[i2+1]:= '\';
      Result[i2+2]:= S[i];
      Inc(i2, 2);
    end
    else
    begin
      Result[i2+1]:= S[i];
      Inc(i2);
    end;
  SetLength(Result, i2); //truncate to really used chars
end;

function ConvertMillisecondsToNiceString(const N: QWord): string;
begin
  if N>=1000 then
    Result:= IntToStr(N div 1000)+'s'+IntToStr(N mod 1000)+'ms'
  else
    Result:= IntToStr(N)+'ms';
end;

function SStringHasBinaryChars(const S: string): boolean;
var
  i: SizeInt;
  N: integer;
begin
  for i:= 1 to Length(S) do
  begin
    N:= Ord(S[i]);
    if (N<32) and (N<>9) then Exit(true);
  end;
  Result:= false;
end;

function SStringHasBinaryChars(const S: UnicodeString): boolean;
var
  i: SizeInt;
  N: integer;
begin
  for i:= 1 to Length(S) do
  begin
    N:= Ord(S[i]);
    if (N<32) and (N<>9) then Exit(true);
  end;
  Result:= false;
end;

end.

