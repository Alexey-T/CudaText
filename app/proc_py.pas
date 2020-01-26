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
  SysUtils, Classes, Variants, Controls,
  Forms,
  ATSynEdit,
  PythonEngine,
  proc_globdata;

procedure Py_SetSysPath(const Dirs: array of string; DoAdd: boolean);
function Py_RunPlugin_Command(const AModule, AMethod: string; const AParams: array of string): string;
function Py_RunPlugin_Event(const AModule, ACmd: string;
  AEd: TATSynEdit; const AParams: array of string; ALazy: boolean): string;
function Py_RunModuleFunction(const AModule, AFunc: string; AParams: array of PPyObject;const AParamNames: array of string): PPyObject;
function Py_RunModuleFunction(const AModule, AFunc: string; AParams: array of PPyObject): PPyObject;

function Py_rect(const R: TRect): PPyObject; cdecl;
function Py_rect_monitor(N: Integer): PPyObject; cdecl;
function Py_rect_control(C: TControl): PPyObject; cdecl;
function Py_SimpleValueFromString(const S: string): PPyObject;
function Py_SimpleValueToString(Obj: PPyObject; QuoteStrings: boolean): string;

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
  Str:= Format('import sys; sys.path %s [%s]', [Sign, Str]);
  GetPythonEngine.ExecString(Str);
end;


function Py_ArgListToString(const AParams: array of string): string;
var
  i: integer;
begin
  Result:= '';
  for i:= 0 to Length(AParams)-1 do
  begin
    if Result<>'' then
      Result:= Result+', ';
    Result:= Result+AParams[i];
  end;
end;


function Py_RunPlugin_Command(const AModule, AMethod: string;
  const AParams: array of string): string;
var
  SObj: string;
  SCmd1, SCmd2: string;
begin
  SObj:= '_cudacmd_' + AModule;

  SCmd1:=
    Format('import %s               ', [AModule]) + SLineBreak +
    Format('if "%s" not in locals():', [SObj]) + SLineBreak +
    Format('    %s = %s.Command()   ', [SObj, AModule]) + SLineBreak;
  if UiOps.PyInitLog then
    SCmd1:= SCmd1+
    Format('    print("Init: %s")',    [AModule]);

  SCmd2:=
    Format('%s.%s(%s)', [SObj, AMethod, Py_ArgListToString(AParams)]);

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

function Py_RunPlugin_Event(const AModule, ACmd: string;
  AEd: TATSynEdit; const AParams: array of string;
  ALazy: boolean): string;
var
  SObj, Str1, Str2, SParams: string;
  H: PtrInt;
  i: integer;
begin
  H:= PtrInt(Pointer(AEd));
  SParams:= Format('cudatext.Editor(%d)', [H]);
  for i:= 0 to Length(AParams)-1 do
    SParams:= SParams + ', ' + AParams[i];

  SObj:= '_cudacmd_' + AModule;

  if not ALazy then
  begin
    Str1:= 'import cudatext' + SLineBreak +
      Format('import %s',                [AModule]) + SLineBreak +
      Format('if "%s" not in locals():', [SObj]) + SLineBreak +
      Format('    %s = %s.Command()',    [SObj, AModule]) + SLineBreak;
    if UiOps.PyInitLog then
      Str1:= Str1+
      Format('    print("Init: %s")',    [AModule]);
    Str2:=
      Format('%s.%s(%s)', [SObj, ACmd, SParams]);

    try
      GetPythonEngine.ExecString(Str1);
      Result:= Py_EvalStringAsString(Str2);
    except
    end;
  end
  else
  begin
    Str1:=
      'import cudatext' + SLineBreak +
      '_ = None' + SLineBreak +
      Format('if "%s" in locals():', [SObj]) + SLineBreak +
      Format('    _ = %s.%s(%s)',    [SObj, ACmd, SParams]);

    try
      GetPythonEngine.ExecString(Str1);
      Result:= Py_EvalStringAsString('_');
    except
    end;
  end;
end;

function Py_rect(const R: TRect): PPyObject; cdecl;
begin
  with GetPythonEngine do
    Result:= Py_BuildValue('(iiii)', R.Left, R.Top, R.Right, R.Bottom);
end;

function Py_rect_monitor(N: Integer): PPyObject; cdecl;
begin
  if (N>=0) and (N<Screen.MonitorCount) then
    Result:= Py_rect(Screen.Monitors[N].BoundsRect)
  else
    Result:= GetPythonEngine.ReturnNone;
end;


function Py_rect_control(C: TControl): PPyObject; cdecl;
var
  P1, P2: TPoint;
  R: TRect;
begin
  P1:= C.ClientToScreen(Point(0, 0));
  P2:= C.ClientToScreen(Point(C.Width, C.Height));
  R.Left:= P1.X;
  R.Top:= P1.Y;
  R.Right:= P2.X;
  R.Bottom:= P2.Y;
  Result:= Py_rect(R);
end;


(*
function Py_RunModuleFunction(const AModule, AFunc: string; AParams: array of string): PPyObject;
var
  SCmd1, SCmd2: string;
begin
  SCmd1:= Format('import %s', [AModule]);
  SCmd2:= Format('%s.%s(%s)', [AModule, AFunc, Py_ArgListToString(AParams)]);

  try
    with GetPythonEngine do
    begin
      ExecString(SCmd1);
      Result:= EvalString(SCmd2);
    end;
  except
  end;
end;
*)

function Py_RunModuleFunction(const AModule,AFunc:string;AParams:array of PPyObject;const AParamNames:array of string):PPyObject;
var
  Module,ModuleDic,Func,Params,ParamsDic:PPyObject;
  i,UnnamedCount:integer;
begin
  Result:=nil;
  with GetPythonEngine do
  begin
    Module:=PyImport_ImportModule(PChar(AModule));
    if Assigned(Module) then
    try
      ModuleDic:=PyModule_GetDict(Module);
      if Assigned(ModuleDic) then
      begin
        Func:=PyDict_GetItemString(ModuleDic,PChar(AFunc));
        if Assigned(Func) then
        begin
          UnnamedCount:=Length(AParams)-Length(AParamNames);
          Params:=PyTuple_New(UnnamedCount);
          if Assigned(Params) then
            try
              ParamsDic:=PyDict_New();
              if Assigned(ParamsDic) then
                try
                  for i:=0 to UnnamedCount-1 do
                    if PyTuple_SetItem(Params,i,AParams[i])<>0 then
                      RaiseError;
                  for i:=0 to Length(AParamNames)-1 do
                    if PyDict_SetItemString(ParamsDic,PChar(AParamNames[i]),AParams[UnnamedCount+i])<>0 then
                      RaiseError;
                  Result:=PyObject_Call(Func,Params,ParamsDic);
                finally
                  Py_DECREF(ParamsDic);
                end;
            finally
              Py_DECREF(Params);
            end;
        end;
      end;
    finally
      Py_DECREF(Module);
    end;
  end;
end;

function Py_RunModuleFunction(const AModule, AFunc: string; AParams: array of PPyObject): PPyObject;
var
  Module,ModuleDic,Func,Params:PPyObject;
  i:integer;
begin
  Result:=nil;
  with GetPythonEngine do
  begin
    Module:=PyImport_ImportModule(PChar(AModule));
    if Assigned(Module) then
    try
      ModuleDic:=PyModule_GetDict(Module);
      if Assigned(ModuleDic) then
      begin
        Func:=PyDict_GetItemString(ModuleDic,PChar(AFunc));
        if Assigned(Func) then
        begin
          Params:=PyTuple_New(Length(AParams));
          if Assigned(Params) then
          try
            for i:=0 to Length(AParams)-1 do
              if PyTuple_SetItem(Params,i,AParams[i])<>0 then
                RaiseError;
            Result:=PyObject_Call(Func,Params,nil);
          finally
            Py_DECREF(Params);
          end;
        end;
      end;
    finally
      Py_DECREF(Module);
    end;
  end;
end;


function Py_SimpleValueFromString(const S: string): PPyObject;
var
  Num: Int64;
begin
  with GetPythonEngine do
  begin
    if S='' then
      Result:= ReturnNone
    else
    if (S[1]='"') or (S[1]='''') then
      Result:= PyString_FromString(PChar( Copy(S, 2, Length(S)-2) ))
    else
    if S=cPyFalse then
      Result:= PyBool_FromLong(0)
    else
    if S=cPyTrue then
      Result:= PyBool_FromLong(1)
    else
    if TryStrToInt64(S, Num) then
      Result:= PyLong_FromLongLong(Num)
    else
      Result:= ReturnNone;
  end;
end;

function Py_SimpleValueToString(Obj: PPyObject; QuoteStrings: boolean): string;
// the same as TPythonEngine.PyObjectAsString but also quotes str values
var
  s: PPyObject;
  w: UnicodeString;
begin
  Result:= '';
  if not Assigned(Obj) then
    Exit;

  with GetPythonEngine do
  begin
    if PyUnicode_Check(Obj) then
    begin
      w:= PyUnicode_AsWideString(Obj);
      if QuoteStrings then
        w:= '"'+w+'"';
      Result:= w;
      Exit;
    end;

    s:= PyObject_Str(Obj);
    if Assigned(s) and PyString_Check(s) then
      Result:= PyString_AsDelphiString(s);
    Py_XDECREF(s);
  end;
end;


end.

