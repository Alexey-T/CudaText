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
function Py_RunPlugin_Command(const AModule, AMethod: string; const AParams: array of string): boolean;
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

procedure PyClearLoadedModuleLists;
procedure PyDisableEventTimes;
function PyEventTimesReport: string;

implementation

var
  _EventTime: QWord = 0;
  _EventTimes: TStringList;

var
  _LoadedModuleCudatext: boolean = false;
  _LoadedLocals: TStringList = nil;
  _LoadedModules: TStringList = nil;

const
  _LoadedPrefix = 'xx';
  //must be the same as in file py/cudatext_reset_plugins.py

function _IsLoadedLocal(const S: string): boolean; inline;
begin
  Result:= _LoadedLocals.IndexOf(S)>=0;
end;

procedure PyDisableEventTimes;
begin
  FreeAndNil(_EventTimes);
end;

function PyEventTimesReport: string;
var
  i: integer;
  tick: PtrInt;
begin
  Result:= IntToStr(_EventTime div 10 * 10)+'ms (';
  for i:= 0 to _EventTimes.Count-1 do
  begin
    tick:= PtrInt(_EventTimes.Objects[i]);
    if tick<1 then Continue;
    if i>0 then
      Result+= ', ';
    Result+=
      Copy(_EventTimes[i], 6, MaxInt)+' '+
      IntToStr(tick)+'ms';
  end;
  Result+= ')';
end;

procedure Py_SetSysPath(const Dirs: array of string; DoAdd: boolean);
var
  Str, Sign: string;
  i: Integer;
begin
  Str:= '';
  for i:= 0 to Length(Dirs)-1 do
    Str:= Str + 'r"' + Dirs[i] + '",';
  if DoAdd then
    Sign:= '+='
  else
    Sign:= '=';
  Str:= Format('sys.path %s [%s]', [Sign, Str]);

  with GetPythonEngine do
  begin
    ExecString(Str+';print("Python %d.%d"%sys.version_info[:2])');
  end;
end;


function _StrArrayToString(const AParams: array of string): string;
var
  i: integer;
begin
  Result:= '';
  for i:= 0 to Length(AParams)-1 do
    Result+= AParams[i]+',';
  if Result<>'' then
    SetLength(Result, Length(Result)-1);
end;


function _MethodEval(const AObject, AMethod, AParams: string): PPyObject;
begin
  try
    with GetPythonEngine do
      Result:= EvalString( Format('%s.%s(%s)', [AObject, AMethod, AParams]) );
  except
  end;
end;

function _MethodEvalEx(const AObject, AMethod, AParams: string): string;
var
  Obj: PPyObject;
begin
  with GetPythonEngine do
  begin
    Obj:= _MethodEval(AObject, AMethod, AParams);
    if Assigned(Obj) then
    begin
      Result:= PyObjectAsString(Obj);
      Py_XDECREF(Obj);
    end
    else
      Result:= '';
  end;
end;


procedure _ImportCommand(const AObject, AModule: string);
begin
  try
    with GetPythonEngine do
      ExecString(Format('import %s;%s=%s.Command()', [AModule, AObject, AModule]));
  except
  end;
end;

function Py_RunPlugin_Command(const AModule, AMethod: string;
  const AParams: array of string): boolean;
var
  SObj: string;
  Obj: PPyObject;
begin
  SObj:= _LoadedPrefix+AModule;

  if not _IsLoadedLocal(SObj) then
  begin
    if UiOps.PyInitLog then
      MsgLogConsole('Init: '+AModule);
    try
      _ImportCommand(SObj, AModule);
      _LoadedLocals.Add(SObj);
    except
    end;
  end;

  Obj:= _MethodEval(SObj, AMethod, _StrArrayToString(AParams));
  if Assigned(Obj) then
    with GetPythonEngine do
    begin
      Result:= not PyBool_Check(Obj) or (PyObject_IsTrue(Obj)=1);
      Py_XDECREF(Obj);
    end;
end;

function Py_RunPlugin_Event(const AModule, ACmd: string;
  AEd: TATSynEdit; const AParams: array of string;
  ALazy: boolean): string;
var
  SObj, SParams: string;
var
  tick: QWord;
  i: integer;
begin
  Result:= '';

  if Assigned(_EventTimes) then
    tick:= GetTickCount64;

  if AEd=nil then
    SParams:= 'None'
  else
    SParams:= Format('cudatext.Editor(%d)', [PtrUInt(Pointer(AEd))]);

  for i:= 0 to Length(AParams)-1 do
    SParams:= SParams + ',' + AParams[i];

  SObj:= _LoadedPrefix+AModule;

  if not _LoadedModuleCudatext then
  begin
    with GetPythonEngine do
      ExecString('import cudatext');
    _LoadedModuleCudatext:= true;
  end;

  if not ALazy then
  begin
    if not _IsLoadedLocal(SObj) then
    begin
      if UiOps.PyInitLog then
        MsgLogConsole('Init: '+AModule);
      try
        _ImportCommand(SObj, AModule);
        _LoadedLocals.Add(SObj);
      except
      end;
    end;

    Result:= _MethodEvalEx(SObj, ACmd, SParams);
  end
  else
  //lazy event: run only of SObj already created
  if _IsLoadedLocal(SObj) then
    Result:= _MethodEvalEx(SObj, ACmd, SParams);

  if Assigned(_EventTimes) then
  begin
    tick:= GetTickCount64-tick;
    Inc(_EventTime, tick);

    i:= _EventTimes.IndexOf(AModule);
    if i>=0 then
      _EventTimes.Objects[i]:= TObject(PtrInt(_EventTimes.Objects[i])+tick)
    else
      _EventTimes.AddObject(AModule, TObject(tick));
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
  Pnt: TPoint;
  R: TRect;
begin
  Pnt:= C.ClientToScreen(Point(0, 0));
  R.Left:= Pnt.X;
  R.Top:= Pnt.Y;
  R.Right:= Pnt.X + C.Width;
  R.Bottom:= Pnt.Y + C.Height;
  Result:= Py_rect(R);
end;

function _ImportModuleCached(const AModule: string): PPyObject;
var
  N: integer;
begin
  N:= _LoadedModules.IndexOf(AModule);
  if N>=0 then
    Result:= PPyObject(_LoadedModules.Objects[N])
  else
  begin
    if UiOps.PyInitLog then
      MsgLogConsole('Init: '+AModule);
    Result:= GetPythonEngine.PyImport_ImportModule(PChar(AModule));
    _LoadedModules.AddObject(AModule, TObject(Result))
  end;
end;

function Py_RunModuleFunction(const AModule,AFunc:string;AParams:array of PPyObject;const AParamNames:array of string):PPyObject;
var
  Module,ModuleDic,Func,Params,ParamsDic:PPyObject;
  i,UnnamedCount:integer;
begin
  Result:=nil;
  with GetPythonEngine do
  begin
    Module:=_ImportModuleCached(AModule);
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
    Module:=_ImportModuleCached(AModule);
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

procedure PyClearLoadedModuleLists;
var
  i: integer;
  Obj: PPyObject;
begin
  _LoadedModuleCudatext:= false;

  _LoadedLocals.Clear;

  with GetPythonEngine do
    for i:= 0 to _LoadedModules.Count-1 do
    begin
      Obj:= PPyObject(_LoadedModules.Objects[i]);
      Py_XDECREF(Obj);
    end;
  _LoadedModules.Clear;
end;

initialization

  _LoadedLocals:= TStringList.Create;
  _LoadedModules:= TStringList.Create;
  _EventTimes:= TStringList.Create;

finalization

  if Assigned(_EventTimes) then
    FreeAndNil(_EventTimes);
  FreeAndNil(_LoadedModules);
  FreeAndNil(_LoadedLocals);

end.

