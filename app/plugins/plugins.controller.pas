unit Plugins.Controller;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil,
  proc_console,
  proc_msg,
  ATStringProc,
  IniFiles,
  Plugins.Interfaces,
  Plugins.Python;

type
  TPluginClass=class of TPlugin;

  TPluginsController=class
    PluginsList,TreeHelpersList:TFPList;
    constructor Create();
    //Initialization
    procedure InitPlugins(const PluginDir:String);
    procedure AddPlugin(const PluginPath,FolderName:String);
    procedure AddTreeHelper(TreeHelper:ITreeHelper);
    function ClassifyPlugin(Config:TMemIniFile;out IsTreeHelper:boolean):TPluginClass;         
    //PluginAPI
    //TreeHelperAPI

    destructor Destroy();override;
  end;

{$i plugins.deprecated.inc}

var
  AppPluginsController:TPluginsController;

implementation

constructor TPluginsController.Create();
begin
  PluginsList:=TFPList.Create();     
  TreeHelpersList:=TFPList.Create();
end;

procedure TPluginsController.InitPlugins(const PluginDir:String);
var
  list: TStringlist;
  SItem: string;
begin
  list:= TStringlist.Create;
  try
    FindAllDirectories(list, PluginDir, false);
    list.Sort;
    for SItem in list do
      AddPlugin(SItem,ExtractFileName(SItem));
  finally
    FreeAndNil(list);
  end;
end;

procedure TPluginsController.AddPlugin(const PluginPath,FolderName:String);
  function IsPluginDeprecated(const S: string): boolean;
  var
    i: integer;
  begin
    Result:= false;
    for i:= 0 to High(DeprecatedPlugins) do
      if S=DeprecatedPlugins[i] then exit(true);
  end;      
var          
  ConfPath:String;
  Conf:TMemIniFile;
  IsTreeHelper:boolean;
  PluginClass:TPluginClass;
begin
  if IsPluginDeprecated(FolderName) then
    MsgLogConsole(Format(msgPluginIgnored, [FolderName]))
  else
  begin

    ConfPath:= PluginPath+DirectorySeparator+'install.inf';
    if FileExists(ConfPath) then
    begin
       Conf:=TMemIniFile.Create(ConfPath);
       PluginClass:=ClassifyPlugin(Conf,IsTreeHelper);
       if Assigned(PluginClass) then
         if IsTreeHelper then
           AddTreeHelper(PluginClass.Create(PluginPath,FolderName,Conf)as ITreeHelper)
         //else
           //AddSTDPlugin(PluginClass.Create(PluginDir,PluginName,Conf));
    end;
  end;
end;      

procedure TPluginsController.AddTreeHelper(TreeHelper:ITreeHelper);
begin
  TreeHelpersList.Add(TreeHelper);
end;
       
function TPluginsController.ClassifyPlugin(Config:TMemIniFile;out IsTreeHelper:boolean):TPluginClass;  
var
  sections:TStringList;
  ini_section:string;
begin
  PluginType:=Config.ReadString('info','type','');

  IsTreeHelper:=False;
  Config.ReadSections(sections);
  for ini_section in sections do
    if SBeginsWith(ini_section, 'treehelper') then
    begin
      IsTreeHelper:=True;
      if PluginType='cudatext-plugin' then
         exit(TPyTreePlugin);
    end;

  if PluginType='cudatext-plugin' then
    exit(TPyStdPlugin);
  Result:=nil;
end;



destructor TPluginsController.Destroy();
begin
  TFPList.ForEachCall();
end;

end.
