(*
This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at http://mozilla.org/MPL/2.0/.

Copyright (c) Alexey Torgashin
*)
unit proc_install_zip;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, FileUtil,
  ecSyntAnal;

function DoInstallAddonFromZip(const fn_zip: string;
  Manager: TecSyntaxManager;
  const dir_acp: string;
  out s_report: string): boolean;

var
  cInstallLexerZipTitle: string = 'Install addon';

implementation

uses
  LCLIntf, LCLProc, LCLType,
  StrUtils,
  IniFiles,
  jsonConf,
  Zipper,
  proc_files,
  proc_globdata;

const
  cTypeLexer = 'lexer';
  cTypePlugin = 'cudatext-plugin';
  cTypeData = 'cudatext-data';

procedure DoInstallData(
  const fn_inf: string;
  var s_report: string);
var
  ini: TIniFile;
  s_subdir, dir_from, dir_target: string;
begin
  s_report:= '';

  ini:= TIniFile.Create(fn_inf);
  try
    s_subdir:= ini.ReadString('info', 'subdir', '');
    if s_subdir='' then exit;
  finally
    FreeAndNil(ini);
  end;

  DeleteFile(fn_inf);
  dir_from:= ExtractFileDir(fn_inf);
  dir_target:= GetAppPath(cDirData)+DirectorySeparator+s_subdir;
  FCopyDir(dir_from, dir_target);

  s_report:= 'data files: '+dir_target;
end;

procedure DoInstallPlugin(
  const fn_inf: string;
  var s_report: string);
var
  ini: TIniFile;
  cfg: TJSONConfig;
  s_section, s_caption, s_module, s_method, s_hotkey, s_lexers,
  s_events, s_keys, path: string;
  s_inmenu: boolean;
  i: integer;
begin
  s_report:= '';

  ini:= TIniFile.Create(fn_inf);
  cfg:= TJSONConfig.Create(nil);
  try
    s_module:= ini.ReadString('info', 'subdir', '');
    if s_module='' then exit;

    try
      cfg.Filename:= GetAppPath(cFileOptPlugins);
      cfg.Formatted:= true;
    except
      exit;
    end;

    FCopyDir(ExtractFileDir(fn_inf), GetAppPath(cDirPy)+DirectorySeparator+s_module);

    for i:= 1 to 200 do
    begin
      s_section:= ini.ReadString('item'+Inttostr(i), 'section', '');
      s_caption:= ini.ReadString('item'+Inttostr(i), 'caption', '');
      s_method:= ini.ReadString('item'+Inttostr(i), 'method', '');
      s_hotkey:= ini.ReadString('item'+Inttostr(i), 'hotkey', '');
      s_lexers:= ini.ReadString('item'+Inttostr(i), 'lexers', '');
      s_events:= ini.ReadString('item'+Inttostr(i), 'events', '');
      s_keys:= ini.ReadString('item'+Inttostr(i), 'keys', '');
      s_inmenu:= ini.ReadBool('item'+Inttostr(i), 'menu', true);

      if s_section='commands' then
      begin
        if s_caption='' then Continue;
        if s_method='' then Continue;

        path:= '/'+s_section+'/'+s_module+'/'+Format('%2.2d', [i-1])+'/';
        cfg.SetValue(path+'caption', s_caption);
        cfg.SetValue(path+'proc', s_method);
        cfg.SetDeleteValue(path+'lexers', s_lexers, '');
        cfg.SetDeleteValue(path+'hotkey', s_hotkey, '');
        cfg.SetDeleteValue(path+'menu', s_inmenu, true);

        s_report:= s_report+'command: '+s_caption+#13;
      end;

      if s_section='events' then
      begin
        if s_events='' then Continue;

        path:= '/'+s_section+'/'+s_module+'/';
        cfg.SetValue(path+'events', s_events);
        cfg.SetDeleteValue(path+'lexers', s_lexers, '');
        cfg.SetDeleteValue(path+'keys', s_keys, '');

        s_report:= s_report+'events: '+s_events+#13;
      end;
    end;

    s_report:= s_report+#13+'Program should be restarted to see new plugin';
  finally
    FreeAndNil(cfg);
    FreeAndNil(ini);
  end;
end;

procedure DoInstallLexer(
  const fn_inf, dir_acp: string;
  Manager: TecSyntaxManager;
  var s_report: string);
var
  i_lexer, i_sub: integer;
  s_lexer, fn_lexer, fn_acp: string;
  an, an_sub: TecSyntAnalyzer;
begin
  s_report:= '';
  with TIniFile.Create(fn_inf) do
  try
    for i_lexer:= 1 to 20 do
    begin
      s_lexer:= ReadString('lexer'+Inttostr(i_lexer), 'file', '');
      if s_lexer='' then Break;

      //lexer file
      fn_lexer:= ExtractFileDir(fn_inf)+DirectorySeparator+s_lexer+'.lcf';
      if not FileExists(fn_lexer) then
      begin
        MsgBox('Cannot find lexer file: '+fn_lexer, mb_ok or mb_iconerror);
        exit
      end;

      fn_acp:= ExtractFileDir(fn_inf)+DirectorySeparator+s_lexer+'.acp';
      if FileExists(fn_acp) then
        if dir_acp<>'' then
          CopyFile(fn_acp, dir_acp+DirectorySeparator+s_lexer+'.acp');

      an:= Manager.FindAnalyzer(s_lexer);
      if an=nil then
        an:= Manager.AddAnalyzer;
      an.LoadFromFile(fn_lexer);
      s_report:= s_report+s_lexer+#13;

      //links
      for i_sub:= 0 to an.SubAnalyzers.Count-1 do
      begin
        s_lexer:= ReadString('lexer'+Inttostr(i_lexer), 'link'+Inttostr(i_sub+1), '');
        if s_lexer='' then Continue;
        if s_lexer='Style sheets' then s_lexer:= 'CSS';
        if s_lexer='Assembler' then s_lexer:= 'Assembly';

        an_sub:= Manager.FindAnalyzer(s_lexer);
        if an_sub<>nil then
        begin
          an.SubAnalyzers.Items[i_sub].SyntAnalyzer:= an_sub;
          //MsgBox('Linked lexer "'+an.LexerName+'" to "'+s_lexer+'"', mb_ok or MB_ICONINFORMATION);
        end
        else
        begin
          MsgBox('Cannot find linked sublexer in library: '+s_lexer, MB_OK or MB_ICONWARNING);
          Continue;
        end;
      end;
    end;
  finally
    Free
  end;
end;

function GetTempDirCounted: string;
var
  n: integer;
begin
  for n:= 0 to 5000 do
  begin
    Result:= GetTempDir+DirectorySeparator+'cudatext_'+Format('%5.5d', [n]);
    if not DirectoryExists(Result) then break;
  end;
end;

function DoInstallAddonFromZip(const fn_zip: string;
  Manager: TecSyntaxManager;
  const dir_acp: string;
  out s_report: string): boolean;
var
  unzip: TUnZipper;
  list: TStringlist;
  dir, fn_inf: string;
  s_title, s_type, s_desc: string;
begin
  Result:= false;
  dir:= GetTempDirCounted;

  if not DirectoryExists(dir) then
    CreateDir(dir);
  if not DirectoryExists(dir) then
  begin
    MsgBox('Cannot create dir:'#13+dir, mb_ok or mb_iconerror);
    exit
  end;

  fn_inf:= dir+DirectorySeparator+'install.inf';
  if FileExists(fn_inf) then
    DeleteFile(fn_inf);

  unzip:= TUnZipper.Create;
  try
    unzip.FileName:= fn_zip;
    unzip.OutputPath:= dir;
    unzip.Examine;

    list:= TStringlist.create;
    try
      list.Add('install.inf');
      unzip.UnZipFiles(list);
    finally
      FreeAndNil(list);
    end;

    if not FileExists(fn_inf) then
    begin
      MsgBox('Cannot find install.inf in zip', mb_ok or mb_iconerror);
      exit
    end;

    unzip.Files.Clear;
    unzip.UnZipAllFiles;
  finally
    unzip.Free;
  end;

  with TIniFile.Create(fn_inf) do
  try
    s_title:= ReadString('info', 'title', '');
    s_desc:= ReadString('info', 'desc', '');
    s_type:= ReadString('info', 'type', '');
  finally
    Free
  end;

  if (s_title='') or (s_type='') then
  begin
    MsgBox('Incorrect install.inf in zip', mb_ok or mb_iconerror);
    exit
  end;

  if (s_type<>cTypeLexer) and
    (s_type<>cTypePlugin) and
    (s_type<>cTypeData) then
  begin
    MsgBox('Unsupported addon type: '+s_type, mb_ok or mb_iconerror);
    exit
  end;

  if MsgBox('This package contains:'#13#13+
    'name: '+s_title+#13+
    IfThen(s_desc<>'', 'description: '+s_desc+#13)+
    'type: '+s_type+#13+
    #13+
    'Do you want to install it?',
    MB_OKCANCEL or MB_ICONQUESTION)<>id_ok then exit;

  s_report:= '';
  if s_type=cTypeLexer then DoInstallLexer(fn_inf, dir_acp, Manager, s_report) else
   if s_type=cTypePlugin then DoInstallPlugin(fn_inf, s_report) else
    if s_type=cTypeData then DoInstallData(fn_inf, s_report);

  Result:= true;
end;


end.

