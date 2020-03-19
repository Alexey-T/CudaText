unit Plugins.Python;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,    
  {$ifdef windows}Windows,{$endif}
  Plugins.Interfaces,
  proc_globdata,
  proc_console,
  proc_str,
  ATStringProc,
  PythonEngine,
  IniFiles;

type
  TPyPlugin=class abstract(TPlugin,IPlugin)
    Module,ModuleDic:PPyObject;
    ModuleName:String;
    constructor Create(const PluginPath,FolderName:String;Conf:TMemIniFile);
    function IsLoaded():boolean;
    procedure Load();
    procedure ReLoad();
    destructor Destroy;override;
  end;
  TPyStdPlugin=class(TPyPlugin,IStdPlugin)  
    //constructor Create(const PluginPath,FolderName:String;Conf:TMemIniFile);
    
  end;
  TPyTreePlugin=class(TPyPlugin,ITreeHelper)
    type
      TFunc=record
        Name:String;
        Func:PPyObject;
      end;
      TLexer=record
        Name:String;
        Num:byte;
      end;
    var
      Functions:array[1..5]of TFunc;
      Lexers:array of TLexer;
    constructor Create(const PluginPath,FolderName:String;Conf:TMemIniFile);
    function IsLexerSupported(Lexer:String):boolean; 
    procedure Load();   
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
    end;
  end;
end;

function TPyPlugin.ReLoad():boolean;
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
      end;
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
  i,i0:integer;
  Sep:TATStringSeparator;
begin
  inherited;
  i0:=0;
  for i:=1 to 5 do
  begin
    Sep.Init(Conf.ReadString('treehelper'+inttostr(i),'lexers',''),',');
    SetLength(Lexers,i0+1);
    while Sep.GetItemStr(Lexers[i0].Name) do
      Lexers[i0].Num:=i;
      inc(i0);
      SetLength(i0+1);
    end          
    SetLength(Lexers,i0);
    functions[i].Name:=Conf.ReadString('treehelper'+inttostr(i),'method','');
  end
end;  

function TPyTreePlugin.IsLexerSupported(Lexer:String):boolean;    
var
  Item:TLexer;
begin
  Result:=False;
  for Item in Lexers do
    if Item.Name=Lexer then
      Result:=True;
end;

finalization 
  FreeAndNil(PythonEng);
  FreeAndNil(PythonModule);    
  FreeAndNil(PythonIO);
end.
