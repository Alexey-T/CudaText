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
function SExtractNumberFromStringAfterChar(const S: string; ch: char; Default: integer): integer;


implementation

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

function SFindFuzzyPositions(SText, SFind: UnicodeString): TATIntArray;
var
  i, N: integer;
begin
  Result:= nil;

  SText:= UnicodeLowerCase(SText);
  SFind:= UnicodeLowerCase(SFind);

  //if simple match is found, don't calculate complex fuzzy matches
  N:= Pos(SFind, SText);
  if N>0 then
  begin
    SetLength(Result, Length(SFind));
    Result[0]:= N;
    for i:= 1 to High(Result) do
      Result[i]:= Result[i-1]+1;
    exit;
  end;

  //calculate complex matches
  N:= 0;
  SetLength(Result, Length(SFind));
  for i:= 1 to Length(SFind) do
  begin
    N:= PosEx(SFind[i], SText, N+1);
    if N=0 then Exit(nil);
    Result[i-1]:= N;
  end;
end;


function SRegexFindParts(const ARegex, AStr: string; out AParts: TRegexParts): boolean;
var
  Obj: TRegExpr;
  i: integer;
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
      for i:= Low(AParts) to High(AParts) do
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

function IsLexerListed(const AItem, AItemList: string): boolean;
const
  cRegexPrefix = 'regex:';
var
  SRegex: string;
begin
  if AItemList='' then exit(true);
  if AItem='' then exit(false);

  if SBeginsWith(AItemList, cRegexPrefix) then
  begin
    SRegex:= Copy(AItemList, Length(cRegexPrefix)+1, MaxInt);
    Result:= SRegexMatchesString(AItem, SRegex, true);
  end
  else
  begin
    Result:= Pos(
      ','+LowerCase(AItem)+',',
      ','+LowerCase(AItemList)+',' )>0;
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
  Result:= Pos(','+Ext+',', ','+AExtList+',' )>0;
end;

function STextListsFuzzyInput(const AText, AFind: string;
  out AWordResults: TAppSearchWordsResults;
  out AFuzzyResults: TATIntArray;
  AEnableFuzzy: boolean): boolean;
begin
  Result:= false;
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

  n:= Length(fn);
  while (n>0) and (fn[n]<>':') do Dec(n);
  if n=0 then exit;

  sNum:= Copy(fn, n+1, MaxInt);
  Result:= StrToIntDef(sNum, 0);
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

function SExtractNumberFromStringAfterChar(const S: string; ch: char; Default: integer): integer;
var
  N1, N2: integer;
begin
  Result:= Default;
  N1:= Pos(ch, S);
  if N1=0 then exit;
  Inc(N1);
  N2:= N1;
  while (N2<=Length(S)) and IsCharDigit(S[N2]) do
    Inc(N2);
  Result:= StrToIntDef(Copy(S, N1, N2-N1), Default);
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

end.

