unit Plugins.Interfaces;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  IniFiles;

type
  IPlugin=interface
    function IsLoaded():boolean;
    procedure Load();
    procedure ReLoad();
  end;
  IStdPlugin=interface(IPlugin)
    
  end;
  ITreeHelper=interface(IPlugin)
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
