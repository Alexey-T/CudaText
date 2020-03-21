unit Plugins.Interfaces;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  IniFiles;

type
  EPluginLoadErr=class(Exception);
  EPluginReLoadErr=class(Exception);
  ETreeHelperCallErr=class(Exception);

  IPlugin=interface
    function IsLoaded():boolean;
    procedure Load();
    procedure ReLoad();
  end;
  IStdPlugin=interface(IPlugin) 
    ['{da19a1ea-cd73-4474-b2b9-cbdfb563aee8}']
    
  end;
  ITreeHelper=interface(IPlugin)
    ['{4e3d4c5c-9dc7-41a6-afec-f3ba032683ef}']
    function IsLexerSupported(Lexer:String):boolean;  
    //function Call(Lexer:String);
  end;

  TPlugin=class abstract(TInterfacedObject)
    constructor Create(const PluginPath,FolderName:String;Conf:TMemIniFile);
  end;

implementation
          
constructor TPlugin.Create(const PluginPath,FolderName:String;Conf:TMemIniFile);
begin

end;

end.
