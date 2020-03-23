unit Plugins.Python;

{$mode Delphi}{$H+}

interface

uses
  Classes, SysUtils,    
  {$ifdef windows}Windows,{$endif}   
  IniFiles,
  proc_globdata,
  proc_console,
  proc_str,
  ATStringProc,
  PythonEngine,
  Plugins.Interfaces,
  Plugins.Utils;

type
  TPyPlugin=class abstract(TPlugin,IPlugin)   
    protected
      Module,ModuleDic:PPyObject;
      ModuleName:String;
    public
      constructor Create(const PluginPath,FolderName:String;Conf:TMemIniFile);
      function IsLoaded():boolean;
      procedure Load();virtual;
      procedure ReLoad();virtual;
      destructor Destroy;override;
  end;
  TPyStdPlugin=class(TPyPlugin,IStdPlugin)  
    //constructor Create(const PluginPath,FolderName:String;Conf:TMemIniFile);
    
  end;
  TPyTreePlugin=class(TPyPlugin,ITreeHelper) 
    protected type
      TFunc=record
        Name:String;
        Func:PPyObject;
      end;
      TLexers=TStringPull<byte>;
    protected var
      Functions:array[1..5]of TFunc;
      Lexers:TLexers;
    public
      constructor Create(const PluginPath,FolderName:String;Conf:TMemIniFile);
      function IsLexerSupported(Lexer:String):boolean;
      procedure Load();override;
      procedure ReLoad();override;
      destructor Destroy;override;
  end;

implementation

var
  PythonEng: TPythonEngine = nil;
  PythonModule: TPythonModule = nil;
  PythonIO: TPythonInputOutput = nil;

procedure InitPyEngine(PythonIOSendUniData:TSendUniDataEvent;PythonEngineAfterInit,PythonModuleInitialization:TNotifyEvent;const PyLibrary:String);
begin
  {$ifdef windows}
  Windows.SetEnvironmentVariable('PYTHONIOENCODING', 'UTF-8');
  {$endif}

  PythonIO:= TPythonInputOutput.Create(nil);
  PythonIO.MaxLineLength:= 2000;
  PythonIO.OnSendUniData:= PythonIOSendUniData;
  PythonIO.UnicodeIO:= True;
  PythonIO.RawOutput:= False;

  PythonEng:= TPythonEngine.Create(nil);
  PythonEng.AutoLoad:= false;
  PythonEng.FatalAbort:= false;
  PythonEng.FatalMsgDlg:= false;
  PythonEng.PyFlags:= [pfIgnoreEnvironmentFlag];
  PythonEng.OnAfterInit:= PythonEngineAfterInit;
  PythonEng.IO:= PythonIO;

  PythonModule:= TPythonModule.Create(nil);
  PythonModule.Engine:= PythonEng;
  PythonModule.ModuleName:= 'cudatext_api';
  PythonModule.OnInitialization:= PythonModuleInitialization;

  PythonEng.UseLastKnownVersion:= False;
  PythonEng.DllPath:= ExtractFilePath(PyLibrary);
  PythonEng.DllName:= ExtractFileName(PyLibrary);
  PythonEng.LoadDll;

  {if not AppPython.Inited then
  begin
    FConsoleMustShow:= true;
    MsgLogConsole(msgCannotInitPython1);
    MsgLogConsole(msgCannotInitPython2);
    DisablePluginMenuItems;
  end;}
end;

//TPyPlugin     
constructor TPyPlugin.Create(const PluginPath,FolderName:String;Conf:TMemIniFile);
begin          
  ModuleName:=FolderName;
  Module:=nil;
end;

function TPyPlugin.IsLoaded():boolean;
begin
  Result:=Assigned(Module);
end;

procedure TPyPlugin.Load();
begin
  with PythonEng do
  begin
    Module:=PyImport_ImportModule(PChar(ModuleName));
    if Assigned(Module) then
    begin
      if UiOps.PyInitLog then
         MsgLogConsole('Init: '+ModuleName);
      ModuleDic:=PyModule_GetDict(Module);
    end
    else
      raise EPluginLoadErr.Create('Error when loading treehelper: '+ModuleName);
  end;
end;

procedure TPyPlugin.ReLoad();
begin
  if IsLoaded() then
    with PythonEng do
    begin
      Module:=PyImport_ReloadModule(Module);
      if Assigned(Module) then
      begin
        if UiOps.PyInitLog then
           MsgLogConsole('Reload: '+ModuleName);
        ModuleDic:=PyModule_GetDict(Module);
      end
      else
        raise EPluginLoadErr.Create('Error when reloading treehelper: '+ModuleName);
    end
  else
    Load();
end;

destructor TPyPlugin.Destroy;
begin

end;

//TPyStdPlugin
//TPyTreePlugin
constructor TPyTreePlugin.Create(const PluginPath,FolderName:String;Conf:TMemIniFile);
var
  i:integer;
  CurrLexer:String;
  Sep:TATStringSeparator;
begin
  inherited;
  Lexers:=TLexers.Create();
  for i:=1 to 5 do
  begin
    Sep.Init(Conf.ReadString('treehelper'+inttostr(i),'lexers',''),',');
    while Sep.GetItemStr(CurrLexer) do
      Lexers.Add(CurrLexer,i);
    functions[i].Name:=Conf.ReadString('treehelper'+inttostr(i),'method','');
    functions[i].Func:=nil;
  end;
end;  

function TPyTreePlugin.IsLexerSupported(Lexer:String):boolean;
begin
  Result:=Lexers.IsIn(Lexer);
end;

procedure TPyTreePlugin.Load();
var
  i:byte;
begin
  if not IsLoaded then
  begin
    inherited;
    for i:=1 to 5 do
    begin
      Functions[i].Func:=PythonEng.PyDict_GetItemString(ModuleDic,PChar(Functions[i].Name));
      if not Assigned(Functions[i].Func) then
        raise EPluginLoadErr.Create('Error when loading treehelper function: "'+Functions[i].Name+'" in '+Self.ModuleName);
    end;
  end;
end;

procedure TPyTreePlugin.ReLoad();  
var
  i:byte;
begin
  if IsLoaded() then  
  begin
    inherited;
    for i:=1 to 5 do
    begin
      if Assigned(Functions[i].Func)then
        PythonEng.PyObject_Free(Functions[i].Func);
      Functions[i].Func:=PythonEng.PyDict_GetItemString(ModuleDic,PChar(Functions[i].Name));
      if not Assigned(Functions[i].Func) then
        raise EPluginReLoadErr.Create('Error when reloading treehelper function: "'+Functions[i].Name+'" in '+Self.ModuleName);
    end;
  end
  else
    Load();
end;
             
destructor TPyTreePlugin.Destroy;
var
  i:TFunc;
begin
  inherited;
  if IsLoaded() then    
    for i in Functions do    
      if Assigned(i.Func) then
         PythonEng.PyObject_Free(i.Func);
end;

finalization
  FreeAndNil(PythonEng);
  FreeAndNil(PythonModule);    
  FreeAndNil(PythonIO);
end.
