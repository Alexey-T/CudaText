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
  SysUtils, Classes,
  PythonEngine,
  proc_str,
  proc_globdata;

type
  { TAppPython }

  TAppPython = class
  private const
    NamePrefix = 'xx'; //the same as in py/cudatext_reset_plugins.py
  private
    FInited: boolean;
    FEngine: TPythonEngine;
    FRunning: boolean;
    FLastCommandModule: string;
    FLastCommandMethod: string;
    FLastCommandParam: string;
    EventTime: QWord;
    EventTimes: TStringList;
    LoadedLocals: TStringList;
    LoadedModuleCudatext: boolean;
    LoadedModules: TStringList;
    MainModule: PPyObject;
    Globals: PPyObject;
    procedure ImportCommand(const AObject, AModule: string);
    function ImportModuleCached(const AModule: string): PPyObject;
    function IsLoadedLocal(const S: string): boolean;
    function MethodEval(const AObject, AMethod, AParams: string): PPyObject;
    function MethodEvalEx(const AObject, AMethod, AParams: string): TAppPyEventResult;
    function StrArrayToString(const V: array of string): string;
    function AppVariantItemToPyObject(const V: TAppVariantItem): PPyObject;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Initialize;
    property Inited: boolean read FInited;
    property Engine: TPythonEngine read FEngine;
    property IsRunning: boolean read FRunning;
    property LastCommandModule: string read FLastCommandModule;
    property LastCommandMethod: string read FLastCommandMethod;
    property LastCommandParam: string read FLastCommandParam;

    function Eval(const Command: string; UseFileMode: boolean=false): PPyObject;
    procedure Exec(const Command: string);
    function RunCommand(const AModule, AMethod: string; const AParams: array of string): boolean;
    function RunEvent(const AModule, ACmd: string;
      AEd: TObject; const AParams: array of string; ALazy: boolean): TAppPyEventResult;
    function RunModuleFunction(const AModule, AFunc: string; AParams: array of PPyObject; const AParamNames: array of string): PPyObject;
    function RunModuleFunction(const AModule, AFunc: string; AParams: array of PPyObject): PPyObject;

    function ValueFromString(const S: string): PPyObject;
    function ValueToString(Obj: PPyObject; QuoteStrings: boolean): string;

    procedure SetPath(const Dirs: array of string; DoAdd: boolean);
    procedure ClearCache;
    procedure DisableTiming;
    function GetTimingReport: string;

    function AppVariantToPyObject(const V: TAppVariant): PPyObject;
    function AppVariantArrayToPyObject(const V: TAppVariantArray): PPyObject;
  end;

const
  cPyTrue = 'True';
  cPyFalse = 'False';
  cPyNone = 'None';

var
  AppPython: TAppPython;

implementation

{ TAppPython }

function TAppPython.AppVariantItemToPyObject(const V: TAppVariantItem): PPyObject;
begin
  with FEngine do
    case V.Typ of
      avdBool:
        Result:= PyBool_FromLong(Ord(V.Bool));
      avdInt:
        Result:= PyLong_FromLongLong(V.Int);
      avdStr:
        Result:= PyString_FromString(PChar(string(V.Str)));
      avdRect:
        Result:= Py_BuildValue('(iiii)', V.Rect.Left, V.Rect.Top, V.Rect.Right, V.Rect.Bottom);
      else
        raise Exception.Create('Unhandled item in AppVariantDictItemToPyObject');
    end;
end;

function TAppPython.AppVariantToPyObject(const V: TAppVariant): PPyObject;
var
  i: integer;
begin
  with FEngine do
    case V.Typ of
      avrNil:
        raise Exception.Create('Nil type in AppVariantToPyObject');

      avrInt:
        Result:= PyLong_FromLongLong(V.Int);

      avrStr:
        Result:= PyString_FromString(PChar(string(V.Str)));

      avrBool:
        Result:= PyBool_FromLong(Ord(V.Bool));

      avrDict:
        begin
          Result:= PyDict_New();
          for i:= 0 to V.Len-1 do
            PyDict_SetItemString(Result,
              PChar(string(V.Items[i].KeyName)),
              AppVariantItemToPyObject(V.Items[i])
              );
        end;

      avrTuple:
        begin
          Result:= PyTuple_New(V.Len);
          for i:= 0 to V.Len-1 do
            PyTuple_SetItem(Result, i,
              AppVariantItemToPyObject(V.Items[i])
              );
        end;

      else
        raise Exception.Create('Unhandled type in AppVariantToPyObject');
    end;
end;

function TAppPython.AppVariantArrayToPyObject(const V: TAppVariantArray): PPyObject;
var
  i: integer;
begin
  with FEngine do
  begin
    Result:= PyTuple_New(Length(V));
    for i:= 0 to Length(V)-1 do
      PyTuple_SetItem(Result, i, AppVariantToPyObject(V[i]));
  end;
end;


constructor TAppPython.Create;
begin
  inherited Create;
  LoadedLocals:= TStringList.Create;
  LoadedModules:= TStringList.Create;
  EventTimes:= TStringList.Create;
end;

destructor TAppPython.Destroy;
begin
  if Assigned(EventTimes) then
    FreeAndNil(EventTimes);
  FreeAndNil(LoadedModules);
  FreeAndNil(LoadedLocals);
  inherited Destroy;
end;

procedure TAppPython.Initialize;
begin
  FInited:= PythonOK;
  if FInited then
    FEngine:= GetPythonEngine;
end;

function TAppPython.IsLoadedLocal(const S: string): boolean; inline;
begin
  Result:= LoadedLocals.IndexOf(S)>=0;
end;

procedure TAppPython.DisableTiming;
begin
  FreeAndNil(EventTimes);
end;

function TAppPython.GetTimingReport: string;
var
  i: integer;
  tick: PtrInt;
begin
  Result:= IntToStr(EventTime div 10 * 10)+'ms (';
  for i:= 0 to EventTimes.Count-1 do
  begin
    tick:= PtrInt(EventTimes.Objects[i]);
    if i>0 then
      Result+= ', ';
    Result+=
      Copy(EventTimes[i], 6, MaxInt)+' '+
      IntToStr(tick)+'ms';
  end;
  Result+= ')';
end;

function TAppPython.StrArrayToString(const V: array of string): string;
var
  i: integer;
begin
  Result:= '';
  for i:= 0 to Length(V)-1 do
    Result+= V[i]+',';
  if Result<>'' then
    SetLength(Result, Length(Result)-1);
end;

function TAppPython.Eval(const Command: string; UseFileMode: boolean=false): PPyObject;
var
  Mode: integer;
begin
  Result := nil;
  if not FInited then exit;

  if UseFileMode then
    Mode:= file_input
  else
    Mode:= eval_input;

  with FEngine do
  begin
    Traceback.Clear;
    CheckError(False);

    if MainModule=nil then
    begin
      MainModule:= GetMainModule;
      if MainModule=nil then
        raise EPythonError.Create('Python: can''t create __main__');
      //if _Locals=nil then
      //  _Locals:= PyModule_GetDict(MainModule);
      if Globals=nil then
        Globals:= PyModule_GetDict(MainModule);
    end;

    try
      //PythonEngine used PChar(CleanString(Command)) - is it needed?
      Result := PyRun_String(PChar(Command), Mode, Globals, Globals); //seems no need separate Locals
      if Result = nil then
        CheckError(False);
    except
      if PyErr_Occurred <> nil then
        CheckError(False)
      else
        raise;
    end;
  end;
end;

procedure TAppPython.Exec(const Command: string);
begin
  if not FInited then exit;
  with FEngine do
    Py_XDECREF(Eval(Command, true));
    //UseFileMode=True to allow running several statements with ";"
end;

function TAppPython.MethodEval(const AObject, AMethod, AParams: string): PPyObject;
begin
  Result:= Eval( Format('%s.%s(%s)', [AObject, AMethod, AParams]) );
end;

function TAppPython.MethodEvalEx(const AObject, AMethod, AParams: string): TAppPyEventResult;
var
  Obj: PPyObject;
begin
  Result.Val:= evrOther;
  Result.Str:= '';

  with FEngine do
  begin
    Obj:= MethodEval(AObject, AMethod, AParams);
    if Assigned(Obj) then
    try
      if Pointer(Obj)=Pointer(Py_True) then
        Result.Val:= evrTrue
      else
      if Pointer(Obj)=Pointer(Py_False) then
        Result.Val:= evrFalse
      {
      if Obj^.ob_type=PyBool_Type then
      begin
        if PPyIntObject(Obj)^.ob_ival>0 then
          Result.Val:= evrTrue
        else
          Result.Val:= evrFalse;
      end
      }
      else
      if Obj^.ob_type=PyUnicode_Type then
      begin
        Result.Val:= evrString;
        Result.Str:= PyUnicode_AsWideString(Obj);
      end;
    finally
      Py_XDECREF(Obj);
    end;
  end;
end;


procedure TAppPython.ImportCommand(const AObject, AModule: string);
begin
  Exec(Format('import %s;%s=%s.Command()', [AModule, AObject, AModule]));
end;

function TAppPython.RunCommand(const AModule, AMethod: string;
  const AParams: array of string): boolean;
var
  SObj: string;
  Obj: PPyObject;
begin
  FRunning:= true;
  FLastCommandModule:= AModule;
  FLastCommandMethod:= AMethod;
  if Length(AParams)>0 then
    FLastCommandParam:= AParams[0]
  else
    FLastCommandParam:= '';

  SObj:= NamePrefix+AModule;

  if not IsLoadedLocal(SObj) then
  begin
    if UiOps.PyInitLog then
      MsgLogConsole('Init: '+AModule);
    try
      ImportCommand(SObj, AModule);
      LoadedLocals.Add(SObj);
    except
    end;
  end;

  try
    Obj:= MethodEval(SObj, AMethod, StrArrayToString(AParams));
    if Assigned(Obj) then
      with FEngine do
      begin
        //only check for False
        Result:= Pointer(Obj)<>Pointer(Py_False);
        Py_XDECREF(Obj);
      end;
  finally
    FRunning:= false;
  end;
end;

function TAppPython.RunEvent(const AModule, ACmd: string; AEd: TObject;
  const AParams: array of string; ALazy: boolean): TAppPyEventResult;
var
  SObj, SParams: string;
var
  tick: QWord;
  i: integer;
begin
  Result.Val:= evrOther;
  Result.Str:= '';

  FRunning:= true;
  if Assigned(EventTimes) then
    tick:= GetTickCount64;

  if AEd=nil then
    SParams:= 'None'
  else
    SParams:= 'cudatext.Editor('+IntToStr(PtrInt(AEd))+')';

  for i:= 0 to Length(AParams)-1 do
    SParams:= SParams + ',' + AParams[i];

  SObj:= NamePrefix+AModule;

  if not LoadedModuleCudatext then
  begin
    Exec('import cudatext');
    LoadedModuleCudatext:= true;
  end;

  if not ALazy then
  begin
    if not IsLoadedLocal(SObj) then
    begin
      if UiOps.PyInitLog then
        MsgLogConsole('Init: '+AModule);
      try
        ImportCommand(SObj, AModule);
        LoadedLocals.Add(SObj);
      except
      end;
    end;

    Result:= MethodEvalEx(SObj, ACmd, SParams);
  end
  else
  //lazy event: run only of SObj already created
  if IsLoadedLocal(SObj) then
    Result:= MethodEvalEx(SObj, ACmd, SParams);

  if Assigned(EventTimes) then
  begin
    tick:= GetTickCount64-tick;
    if tick>0 then
    begin
      Inc(EventTime, tick);
      i:= EventTimes.IndexOf(AModule);
      if i>=0 then
        EventTimes.Objects[i]:= TObject(PtrInt(EventTimes.Objects[i])+PtrInt(tick))
      else
        EventTimes.AddObject(AModule, TObject(PtrInt(tick)));
    end;
  end;

  FRunning:= false;
end;


function TAppPython.ImportModuleCached(const AModule: string): PPyObject;
var
  N: integer;
begin
  N:= LoadedModules.IndexOf(AModule);
  if N>=0 then
    Result:= PPyObject(LoadedModules.Objects[N])
  else
  begin
    if UiOps.PyInitLog then
      MsgLogConsole('Init: '+AModule);
    Result:= FEngine.PyImport_ImportModule(PChar(AModule));
    LoadedModules.AddObject(AModule, TObject(Result))
  end;
end;

function TAppPython.RunModuleFunction(const AModule,AFunc:string;AParams:array of PPyObject;const AParamNames:array of string):PPyObject;
var
  Module,ModuleDic,Func,Params,ParamsDic:PPyObject;
  i,UnnamedCount:integer;
begin
  Result:=nil;
  if not FInited then exit;
  with FEngine do
  begin
    Module:=ImportModuleCached(AModule);
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

function TAppPython.RunModuleFunction(const AModule, AFunc: string; AParams: array of PPyObject): PPyObject;
var
  Module,ModuleDic,Func,Params:PPyObject;
  i:integer;
begin
  Result:=nil;
  if not FInited then exit;
  with FEngine do
  begin
    Module:=ImportModuleCached(AModule);
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


function TAppPython.ValueFromString(const S: string): PPyObject;
var
  Num: Int64;
begin
  with FEngine do
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

function TAppPython.ValueToString(Obj: PPyObject; QuoteStrings: boolean): string;
// the same as TPythonEngine.PyObjectAsString but also quotes str values
var
  s: PPyObject;
  w: UnicodeString;
begin
  Result:= '';
  if not Assigned(Obj) then
    Exit;

  with FEngine do
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

procedure TAppPython.ClearCache;
var
  i: integer;
  Obj: PPyObject;
begin
  LoadedModuleCudatext:= false;

  LoadedLocals.Clear;

  with FEngine do
    for i:= 0 to LoadedModules.Count-1 do
    begin
      Obj:= PPyObject(LoadedModules.Objects[i]);
      Py_XDECREF(Obj);
    end;
  LoadedModules.Clear;
end;


procedure TAppPython.SetPath(const Dirs: array of string; DoAdd: boolean);
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

  Exec(Str+';print("Python %d.%d"%sys.version_info[:2])');
end;


initialization

  AppPython:= TAppPython.Create;

finalization

  FreeAndNil(AppPython);

end.

