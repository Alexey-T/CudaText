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
  proc_globdata,
  proc_appvariant;

type
  { TAppPython }

  TAppPython = class
  private
    const NamePrefix = 'xx'; //the same as in py/cudatext_reset_plugins.py
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
    LoadedModules: TStringList;
    ModuleMain: PPyObject;
    ModuleCud: PPyObject;
    GlobalsMain: PPyObject;
    GlobalsCud: PPyObject;
    procedure MaskFPU(AValue: boolean);
    procedure TimeTrackBegin(var tick: QWord);
    procedure TimeTrackEnd(const AModule: string; var tick: QWord);
    procedure InitModuleMain;
    procedure InitModuleCud;
    procedure ImportCommand(const AObjectName, AModule: string);
    function ImportModuleCached(const AModule: string): PPyObject;
    function IsLoadedLocal(const S: string): boolean;
    function MethodEvalEx(const AObject, AMethod: string; const AParams: array of PPyObject): TAppPyEventResult;
    function MethodEvalObjects(const AObject, AFunc: string; const AParams: array of PPyObject): PPyObject;
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

    function RunCommand(const AModule, AMethod: string;
      const AParams: TAppVariantArray): boolean;
    function RunEvent(const AModule, ACmd: string; AEd: TObject;
      const AParams: TAppVariantArray; ALazy: boolean): TAppPyEventResult;

    function RunModuleFunction(const AModule, AFunc: string;
      const AParams: array of PPyObject;
      const AParamNames: array of string): PPyObject;
    function RunModuleFunction(const AModule, AFunc: string;
      const AParams: array of PPyObject): PPyObject;

    function ValueFromString(const S: string): PPyObject;
    function ValueToString(Obj: PPyObject; QuoteStrings: boolean): string;
    procedure ValueToRecord(Obj: PPyObject; out R: TAppPyEventResult);
    function ObjectToPyInt(Obj: TObject): PPyObject;

    procedure SetPath(const Dirs: array of string; DoAdd: boolean);
    procedure ClearCache;
    procedure DisableTiming;
    function GetTimingReport: string;
  end;

var
  AppPython: TAppPython;

implementation

{ TAppPython }

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

procedure TAppPython.ValueToRecord(Obj: PPyObject; out R: TAppPyEventResult);
begin
  R.Val:= evrOther;
  R.Str:= '';

  with FEngine do
    if Assigned(Obj) then
    try
      if Pointer(Obj)=Pointer(Py_True) then
        R.Val:= evrTrue
      else
      if Pointer(Obj)=Pointer(Py_False) then
        R.Val:= evrFalse
      else
      if Obj^.ob_type=PyUnicode_Type then
      begin
        R.Val:= evrString;
        R.Str:= PyUnicode_AsWideString(Obj);
      end
      else
      if (Obj^.ob_type=PyLong_Type) then
      begin
        R.Val:= evrInt;
        R.Int:= PyLong_AsLong(Obj);
      end;
    finally
      Py_XDECREF(Obj);
    end;
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

procedure TAppPython.InitModuleMain;
begin
  with FEngine do
    if ModuleMain=nil then
    begin
      ModuleMain:= PyImport_AddModule('__main__'); //same as PythonEngine.GetMainModule
      if ModuleMain=nil then
        raise EPythonError.Create('Python: cannot init __main__');
      if GlobalsMain=nil then
        GlobalsMain:= PyModule_GetDict(ModuleMain);
    end;
end;

procedure TAppPython.InitModuleCud;
begin
  with FEngine do
    if ModuleCud=nil then
    begin
      ModuleCud:= PyImport_ImportModule('cudatext');

      //handle import error (e.g. syntax errors)
      if ModuleCud=nil then
        CheckError(False);

      if GlobalsCud=nil then
        GlobalsCud:= PyModule_GetDict(ModuleCud);
    end;
end;

function TAppPython.Eval(const Command: string; UseFileMode: boolean=false): PPyObject;
var
  Mode: integer;
begin
  Result:= nil;
  if not FInited then exit;
  InitModuleMain;

  if UseFileMode then
    Mode:= file_input
  else
    Mode:= eval_input;

  with FEngine do
  begin
    Traceback.Clear;
    CheckError(False);

    try
      //PythonEngine used PChar(CleanString(Command)) - is it needed?
      Result := PyRun_String(PChar(Command), Mode, GlobalsMain, GlobalsMain); //seems no need separate Locals
      if Result = nil then
        CheckError(False);
    except
      on E: Exception do
      begin
        MsgLogConsole('ERROR: Exception in CudaText: '+E.Message);
        if PyErr_Occurred <> nil then
          CheckError(False)
        else
          raise;
      end;
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

function TAppPython.MethodEvalObjects(const AObject, AFunc: string;
  const AParams: array of PPyObject): PPyObject;
var
  CurrObject, Func, Params: PPyObject;
  i: integer;
begin
  Result:=nil;
  with FEngine do
  begin
    CurrObject:=PyDict_GetItemString(GlobalsMain,PChar(AObject));
    if Assigned(CurrObject) then
    begin
      Func:=PyObject_GetAttrString(CurrObject,PChar(AFunc));
      if Func=nil then
        RaiseError;

      try
        Params:=PyTuple_New(Length(AParams));
        if Assigned(Params) then
        try
          for i:=0 to Length(AParams)-1 do
            if PyTuple_SetItem(Params,i,AParams[i])<>0 then
              RaiseError;

          try
            MaskFPU(true);
            Result:=PyObject_Call(Func,Params,nil);
            if Result = nil then
              CheckError(False);
          except
            on E: Exception do
            begin
              MsgLogConsole(Format('ERROR: Exception in CudaText for %s: %s', [AFunc, E.Message]));
              if PyErr_Occurred <> nil then
                CheckError(False)
              else
                raise;
            end;
          end;
        finally
          Py_DECREF(Params);
        end;
      finally
        Py_DECREF(Func);
      end;
    end;
  end;
end;

function TAppPython.MethodEvalEx(const AObject, AMethod: string;
  const AParams: array of PPyObject): TAppPyEventResult;
var
  Obj: PPyObject;
begin
  Obj:= MethodEvalObjects(AObject, AMethod, AParams);
  ValueToRecord(Obj, Result);
end;


procedure TAppPython.ImportCommand(const AObjectName, AModule: string);
var
  ObjModule, ObjDict, ObjClass, ObjObject: PPyObject;
begin
  with FEngine do
  begin
    ObjModule:= ImportModuleCached(AModule);
    if Assigned(ObjModule) then
    begin
      ObjDict:= PyModule_GetDict(ObjModule);
      if Assigned(ObjDict) then
      begin
        ObjClass:= PyDict_GetItemString(ObjDict, 'Command');
        if Assigned(ObjClass) then
        try
          MaskFPU(true);
          ObjObject:= PyObject_CallObject(ObjClass, nil);
          if Assigned(ObjObject) then
            PyDict_SetItemString(GlobalsMain, PChar(AObjectName), ObjObject);
        finally
          Py_DECREF(ObjClass);
        end;
      end;
    end;
  end;

  //old slow code:
  //Exec(Format('import %s;%s=%s.Command()', [AModule, AObjectName, AModule]));
end;

function TAppPython.RunCommand(const AModule, AMethod: string; const AParams: TAppVariantArray): boolean;
var
  ObjName: string;
  ParamObjs: array of PPyObject;
  Obj: PPyObject;
  tick: QWord;
  i: integer;
begin
  if not FInited then exit(false);
  InitModuleMain;

  FRunning:= true;
  tick:= 0;
  TimeTrackBegin(tick);

  FLastCommandModule:= AModule;
  FLastCommandMethod:= AMethod;
  FLastCommandParam:= '';
  if Length(AParams)>0 then
    if AParams[0].Typ=avrStr then
      FLastCommandParam:= AParams[0].Str;

  ObjName:= NamePrefix+AModule;

  if not IsLoadedLocal(ObjName) then
  begin
    try
      ImportCommand(ObjName, AModule);
      LoadedLocals.Add(ObjName);
    except
      on E: Exception do
      begin
        MsgLogConsole(Format('ERROR: Exception in CudaText for %s.%s: %s', [AModule, AMethod, E.Message]));
        if FEngine.PyErr_Occurred <> nil then
          FEngine.CheckError(False)
        else
          raise;
      end;
    end;
  end;

  try
    SetLength(ParamObjs, Length(AParams));
    for i:= 0 to Length(AParams)-1 do
      ParamObjs[i]:= AppVariantToPyObject(AParams[i]);

    try
      //Obj:= MethodEval(ObjName, AMethod, AppVariantArrayToString(AParams));
      Obj:= MethodEvalObjects(ObjName, AMethod, ParamObjs);
      if Assigned(Obj) then
        with FEngine do
        begin
          //only check for False
          Result:= Pointer(Obj)<>Pointer(Py_False);
          Py_XDECREF(Obj);
        end;
    except
      on E: Exception do
      begin
        MsgLogConsole(Format('ERROR: Exception in CudaText for %s.%s: %s', [AModule, AMethod, E.Message]));
        if FEngine.PyErr_Occurred <> nil then
          FEngine.CheckError(False)
        else
          raise;
      end;
    end;
  finally
    TimeTrackEnd(AModule, tick);
    FRunning:= false;
  end;
end;

procedure TAppPython.TimeTrackBegin(var tick: QWord);
begin
  if Assigned(EventTimes) then
    tick:= GetTickCount64;
end;

procedure TAppPython.TimeTrackEnd(const AModule: string; var tick: QWord);
var
  i: integer;
begin
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
end;

function TAppPython.RunEvent(const AModule, ACmd: string; AEd: TObject;
  const AParams: TAppVariantArray; ALazy: boolean): TAppPyEventResult;
var
  ParamsObj: array of PPyObject;
//
  procedure InitParamsObj;
  var
    ObjEditor, ObjEditorArgs: PPyObject;
    i: integer;
  begin
    SetLength(ParamsObj, Length(AParams)+1);

    //first param must be None or Editor(AEd_handle)
    with FEngine do
      if AEd=nil then
        ParamsObj[0]:= ReturnNone
      else
      begin
        InitModuleCud;
        ObjEditor:= PyDict_GetItemString(GlobalsCud, 'Editor');
        if ObjEditor=nil then
          raise EPythonError.Create('Python: cannot find cudatext.Editor');
        ObjEditorArgs:= PyTuple_New(1);
        try
          PyTuple_SetItem(ObjEditorArgs, 0, PyLong_FromLongLong(PtrInt(AEd)));
          MaskFPU(true);
          ParamsObj[0]:= PyObject_CallObject(ObjEditor, ObjEditorArgs);
        finally
          Py_XDECREF(ObjEditorArgs);
        end;
      end;

    for i:= 0 to Length(AParams)-1 do
      ParamsObj[i+1]:= AppVariantToPyObject(AParams[i]);
  end;
  //
var
  ObjName: string;
  tick: QWord;
begin
  Result.Val:= evrOther;
  Result.Str:= '';

  if not FInited then exit;
  InitModuleMain;

  FRunning:= true;
  tick:= 0;
  TimeTrackBegin(tick);

  ObjName:= NamePrefix+AModule;

  try
    if not ALazy then
    begin
      if not IsLoadedLocal(ObjName) then
      begin
        ImportCommand(ObjName, AModule);
        LoadedLocals.Add(ObjName);
      end;

      InitParamsObj;
      Result:= MethodEvalEx(ObjName, ACmd, ParamsObj);
    end
    else
    //lazy event: run only of ObjName already created
    if IsLoadedLocal(ObjName) then
    begin
      InitParamsObj;
      Result:= MethodEvalEx(ObjName, ACmd, ParamsObj);
    end;
  except
    on E: Exception do
    begin
      MsgLogConsole(Format('ERROR: Exception in CudaText for %s.%s: %s', [AModule, ACmd, E.Message]));
      if FEngine.PyErr_Occurred <> nil then
        FEngine.CheckError(False)
      else
        raise;
    end;
  end;

  FRunning:= false;
  TimeTrackEnd(AModule, tick);
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
    if UiOps.LogPluginIniting then
      MsgLogConsole('Init: '+AModule);

    Result:= FEngine.PyImport_ImportModule(PChar(AModule));

    //handle import error (e.g. syntax errors)
    if Result=nil then
      FEngine.CheckError(False);

    LoadedModules.AddObject(AModule, TObject(Result))
  end;
end;

function TAppPython.RunModuleFunction(const AModule,AFunc:string;
  const AParams:array of PPyObject;
  const AParamNames:array of string):PPyObject;
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
                  MaskFPU(true);
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

function TAppPython.RunModuleFunction(const AModule, AFunc: string;
  const AParams: array of PPyObject): PPyObject;
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
            MaskFPU(true);
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
  if not FInited then exit(nil);
  with FEngine do
  begin
    if S='' then
      Result:= ReturnNone
    else
    if (S[1]='"') or (S[1]='''') then
      Result:= PyUnicodeFromString( Copy(S, 2, Length(S)-2) )
    else
    if S='False' then
      Result:= PyBool_FromLong(0)
    else
    if S='True' then
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
  if not FInited then exit;
  if Obj=nil then exit;

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
    if Assigned(s) and PyUnicode_Check(s) then
      Result:= PyUnicode_AsWideString(s);
    Py_XDECREF(s);
  end;
end;

procedure TAppPython.ClearCache;
var
  i: integer;
  Obj: PPyObject;
begin
  if not FInited then exit;
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

  Exec(Str+';print("Python %d.%d.%d"%sys.version_info[:3])');
end;

procedure TAppPython.MaskFPU(AValue: boolean);
//needed for plugin cuda_palette on Win64, to avoid "floating point exception"
begin
  {$if defined(windows)}
  MaskFPUExceptions(AValue);
  {$endif}
end;

function TAppPython.ObjectToPyInt(Obj: TObject): PPyObject;
begin
  Result:= FEngine.PyLong_FromLongLong(Int64(PtrInt(Obj)))
end;


initialization

  AppPython:= TAppPython.Create;

finalization

  FreeAndNil(AppPython);

end.

