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
  LazFileUtils,
  jsonConf,
  ATStringProc,
  ATSynEdit_RegExpr;

//function SReadOptionFromJson(const fn, path, def_value: string): string;
//procedure SWriteOptionToJson(const fn, path, value: string);

type
  TStringReplacePart = record
    SFrom, STo: string;
  end;

function SFindFuzzyPositions(SText, SFind: UnicodeString): TATIntArray;
function SFindWordsInString(SText, SFind: string): boolean;
function SRegexReplaceSubstring(const AStr, AStrFind, AStrReplace: string; AUseSubstitute: boolean): string;
function SRegexMatchesString(const AStr, AStrRegex: string; ACaseSensitive: boolean): boolean;

function IsLexerListed(const ALexer, ANameList: string): boolean;
function IsFilenameListedInExtensionList(const AFilename, AExtList: string): boolean;

type
  TRegexParts = array[1..8] of string;
function SRegexFindParts(const ARegex, AStr: string; out AParts: TRegexParts): boolean;

function SEscapeForPython(const Str: string): string;
function SStringToPythonString(const Str: string): string;

procedure SLoadStringsFromFile(cfg: TJsonConfig; const path: string; List: TStrings; MaxItems: integer);
procedure SSaveStringsToFile(cfg: TJsonConfig; const path: string; List: TStrings; MaxItems: integer);
function SMaskFilenameSlashes(const fn: string): string;
procedure SParseFilenameWithTwoNumbers(var fn: string; out NLine, NColumn: integer);


implementation

function SReplaceParts(const S: string; const Decode: array of TStringReplacePart): string;
var
  i, j: Integer;
  DoDecode: Boolean;
begin
  Result := '';
  i := 1;
  repeat
    if i > Length(S) then Break;
    DoDecode := False;
    for j := Low(Decode) to High(Decode) do
      with Decode[j] do
        if (SFrom <> '') and (SFrom = Copy(S, i, Length(SFrom))) then
        begin
          DoDecode := True;
          Result := Result + STo;
          Inc(i, Length(SFrom));
          Break
        end;
    if DoDecode then Continue;
    Result := Result + S[i];
    Inc(i);
  until False;
end;

function SEscapeForPython(const Str: string): string;
const
  Decode: array[0..3] of TStringReplacePart =
    (
      (SFrom: '"'; STo: '\"'),
      (SFrom: #10; STo: '\n'),
      (SFrom: #13; STo: '\r'),
      (SFrom: #9; STo: '\t')
    );
begin
  Result:= SReplaceParts(Str, Decode);
end;

function SStringToPythonString(const Str: string): string;
const
  Decode: array[0..4] of TStringReplacePart =
    (
      (SFrom: '\'; STo: '\\'),
      (SFrom: '"'; STo: '"+''"''+"'),
      (SFrom: #10; STo: '\n'),
      (SFrom: #13; STo: '\r'),
      (SFrom: #9; STo: '\t')
    );
begin
  Result:= SReplaceParts(Str, Decode);
  Result:= '"'+Result+'"';
end;

procedure SLoadStringsFromFile(cfg: TJsonConfig; const path: string;
  List: TStrings; MaxItems: integer);
begin
  cfg.GetValue(path, List, '');
end;

procedure SSaveStringsToFile(cfg: TJsonConfig; const path: string;
  List: TStrings; MaxItems: integer);
begin
  cfg.SetValue(path, List);
end;

function SMaskFilenameSlashes(const fn: string): string;
begin
  result:= fn;
  result:= StringReplace(result, '/', '|', [rfReplaceAll]);
  result:= StringReplace(result, '\', '|', [rfReplaceAll]);
end;

function SUnicodePosEx(const substr, str: UnicodeString; frompos: integer): integer;
begin
  Result:= Pos(substr, Copy(str, frompos, MaxInt));
  if Result>0 then
    Inc(Result, frompos-1);
end;

function SFindFuzzyPositions(SText, SFind: UnicodeString): TATIntArray;
var
  i, N: integer;
begin
  SetLength(result, 0);

  SText:= UnicodeLowerCase(SText);
  SFind:= UnicodeLowercase(SFind);

  N:= 0;
  for i:= 1 to Length(SFind) do
  begin
    N:= SUnicodePosEx(SFind[i], SText, N+1);
    if N=0 then
    begin
      SetLength(result, 0);
      Exit
    end;
    SetLength(result, Length(result)+1);
    result[high(result)]:= N;
  end;
end;


function SRegexFindParts(const ARegex, AStr: string; out AParts: TRegexParts): boolean;
var
  Obj: TRegExpr;
  i: integer;
begin
  Result:= false;
  for i:= Low(AParts) to High(AParts) do
    AParts[i]:= '';

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
      Result:= Obj.ExecPos(1);
    except
      Result:= false;
    end;

    if Result then
    begin
      for i:= Low(AParts) to High(AParts) do
        AParts[i]:= Obj.Match[i];
    end;
  finally
    FreeAndNil(Obj);
  end;
end;

function SFindWordsInString(SText, SFind: string): boolean;
var
  SItem: string;
begin
  //such UnicodeLowerCase works with rus chars
  SText:= Trim(UTF8Encode(UnicodeLowerCase(UTF8Decode(SText))));
  SFind:= Trim(UTF8Encode(UnicodeLowerCase(UTF8Decode(SFind))));

  if SText='' then exit(false);
  if SFind='' then exit(false);

  repeat
    SItem:= Trim(SGetItem(SFind, ' '));
    SFind:= Trim(SFind);

    if SItem='' then exit(true);
    if Pos(SItem, SText)=0 then exit(false);
  until false;
end;

function IsLexerListed(const ALexer, ANameList: string): boolean;
const
  cRegexPrefix = 'regex:';
var
  SRegex: string;
begin
  if ANameList='' then exit(true);
  if ALexer='' then exit(false);

  if SBeginsWith(ANameList, cRegexPrefix) then
  begin
    SRegex:= ANameList;
    Delete(SRegex, 1, Length(cRegexPrefix));
    Result:= SRegexMatchesString(ALexer, SRegex, true);
  end
  else
  begin
    Result:= Pos(
      ','+LowerCase(ALexer)+',',
      ','+LowerCase(ANameList)+',' )>0;
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

function SRegexMatchesString(const AStr, AStrRegex: string; ACaseSensitive: boolean): boolean;
var
  Obj: TRegExpr;
begin
  Result:= false;
  if AStr='' then exit;
  if AStrRegex='' then exit;

  Obj:= TRegExpr.Create;
  try
    try
      Obj.ModifierS:= false;
      Obj.ModifierI:= not ACaseSensitive;
      Obj.Expression:= AStrRegex;
      Result:= Obj.Exec(AStr) and (Obj.MatchLen[0]=Length(AStr));
    except
    end;
  finally
    Obj.Free;
  end;
end;



function SReadOptionFromJson(const fn, path, def_value: string): string;
var
  cfg: TJSONConfig;
begin
  if not FileExistsUTF8(fn) then exit(def_value);

  cfg:= TJSONConfig.Create(nil);
  try
    try
      cfg.Filename:= fn;
      Result:= cfg.GetValue(path, def_value);
    except
    end;
  finally
    cfg.Free;
  end;
end;


procedure SWriteOptionToJson(const fn, path, value: string);
var
  cfg: TJSONConfig;
begin
  cfg:= TJSONConfig.Create(nil);
  try
    try
      cfg.Formatted:= true;
      cfg.Filename:= fn;
      cfg.SetDeleteValue(path, value, '');
    except
    end;
  finally
    cfg.Free;
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


initialization
  //debug
  //ShowMessage(inttostr(ord(SRegexMatchesString('wwwa--ddd', '\w+\-+\w{3}'))));

end.

