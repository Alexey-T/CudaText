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
  SysUtils, Classes, StrUtils,
  ATStringProc,
  jsonConf;

type
  TStringDecodeRecW = record
    SFrom, STo: WideString;
  end;

function SDecodeW(const S: WideString; const Decode: array of TStringDecodeRecW): WideString;
function SWideStringToPythonString(const Str: Widestring): string;
procedure SAddStringToHistory(const S: string; List: TStrings; MaxItems: integer);
procedure SLoadStringsFromFile(cfg: TJsonConfig; const path: string; List: TStrings; MaxItems: integer);
procedure SSaveStringsToFile(cfg: TJsonConfig; const path: string; List: TStrings; MaxItems: integer);
function SMaskFilenameSlashes(const fn: string): string;


implementation

function SDecodeW(const S: WideString; const Decode: array of TStringDecodeRecW): WideString;
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

function SWideStringToPythonString(const Str: Widestring): string;
const
  Decode: array[0..0] of TStringDecodeRecW =
    ((SFrom: '"'; STo: '"+''"''+"'));
begin
  Result:= UTF8Encode(SDecodeW(Str, Decode));
  Result:= 'r"'+Result+'"';
end;

procedure SAddStringToHistory(const S: string; List: TStrings; MaxItems: integer);
var
  n: integer;
begin
  if s<>'' then
  begin
    n:= List.IndexOf(s);
    if n>=0 then
      List.Delete(n);
    List.Insert(0, s);
  end;

  while List.Count>MaxItems do
    List.Delete(List.Count-1);
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


end.

