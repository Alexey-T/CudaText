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
  ATSynEdit,
  PythonEngine,
  proc_str;

procedure Py_SetSysPath(const Dirs: array of string; DoAdd: boolean);
function Py_RunPlugin_Command(const SModule, SCmd, SParam: string): string;
function Py_RunPlugin_Event(const SModule, SCmd: string;
  AEd: TATSynEdit; const AParams: array of string): string;
//function Py_StringList(List: TStrings): PPyObject; cdecl; //dont work

const
  cPyTrue = 'True';
  cPyFalse = 'False';
  cPyNone = 'None';

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

function Py_RunPlugin_Command(const SModule, SCmd, SParam: string): string;
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
    Format('%s.%s(%s)', [SObj, SCmd, SParam]);

  try
    GetPythonEngine.ExecString(SCmd1);
    Result:= GetPythonEngine.EvalStringAsStr(SCmd2);
  except
  end;
end;

function Py_EvalStringAsString(const command: string): string;
begin
  Result:= GetPythonEngine.EvalStringAsStr(command);
end;

//var
//  _EventBusy: boolean = false;

function Py_RunPlugin_Event(const SModule, SCmd: string;
  AEd: TATSynEdit; const AParams: array of string): string;
var
  SObj: string;
  SCmd1, SCmd2: string;
  SParams: string;
  H: PtrInt;
  i: integer;
begin
  ////this lock gives only bad: with it some events dont fire (while on_key? or more cases?)
  //if _EventBusy then exit('');
  //_EventBusy:= true;

  H:= PtrInt(Pointer(AEd));
  SParams:= Format('cudatext.Editor(%d)', [H]);
  for i:= 0 to Length(AParams)-1 do
    SParams:= SParams + ', ' + AParams[i];

  SObj:= '_cudacmd_' + SModule;
  SCmd1:= 'import cudatext' + SLineBreak +
    Format('import %s               ', [SModule]) + SLineBreak +
    Format('if "%s" not in locals():', [SObj]) + SLineBreak +
    Format('    %s = %s.%s()        ', [SObj, SModule, 'Command']);
  SCmd2:=
    Format('%s.%s(%s)', [SObj, SCmd, SParams]);

  try
    try
      GetPythonEngine.ExecString(SCmd1);
      Result:= Py_EvalStringAsString(SCmd2);
    except
    end;
  finally
    //_EventBusy:= false;
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

