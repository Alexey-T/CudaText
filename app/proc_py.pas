(*
This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at http://mozilla.org/MPL/2.0/.

Copyright (c) Alexey Torgashin
*)
unit proc_py;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, Variants,
  PythonEngine,
  proc_str;

procedure Py_SetSysPath(const Dirs: array of string; DoAdd: boolean);
function Py_RunPlugin_Command(const SModule, SCmd: string): string;
//function Py_StringList(List: TStrings): PPyObject; cdecl; //dont work


implementation

procedure Py_SetSysPath(const Dirs: array of string; DoAdd: boolean);
var
  Str, Sign: string;
  i: Integer;
begin
  Str:= '';
  for i:= 0 to Length(Dirs)-1 do
    Str:= Str + 'r"' + Dirs[i] + '"' + ',';
  if DoAdd then
    Sign:= '+='
  else
    Sign:= '=';
  Str:= Format('sys.path %s [%s]', [Sign, Str]);
  GetPythonEngine.ExecString(Str);
end;

function Py_RunPlugin_Command(const SModule, SCmd: string): string;
var
  SObj: string;
  SCmd1, SCmd2: string;
begin
  SObj:= '_cudacmd_' + SModule;
  SCmd1:=
    Format('import %s               ', [SModule]) + SLineBreak +
    Format('if "%s" not in locals():', [SObj]) + SLineBreak +
    Format('    %s = %s.%s()        ', [SObj, SModule, 'Command']);
  SCmd2:=
    Format('%s.%s()', [SObj, SCmd]);

  try
    GetPythonEngine.ExecString(SCmd1);
    Result:= GetPythonEngine.EvalStringAsStr(SCmd2);
  except
  end;
end;

(*
function Py_StringList(List: TStrings): PPyObject; cdecl;
var
  NLen, i: Integer;
  ComArray: Variant;
begin
  with GetPythonEngine do
  begin
    NLen:= List.Count;
    if NLen>0 then
    begin
      ComArray:= VarArrayCreate([0, NLen-1], varOleStr);
      for i:= 0 to NLen-1 do
        ComArray[i]:= Utf8Decode(List[i]);
      Result:= VariantAsPyObject(ComArray);
    end
    else
      Result:= ReturnNone;
  end;
end;
*)

end.

