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

type
  TAppAddonType = (
    cAddonTypeUnknown,
    cAddonTypePlugin,
    cAddonTypeLexer,
    cAddonTypeData
    );

procedure DoInstallAddonFromZip(
  const fn_zip: string;
  const dir_acp: string;
  out StrReport, StrMessage: string;
  out IsInstalled: boolean;
  out NAddonType: TAppAddonType;
  out DirTarget: string;
  Silent: boolean);

var
  cInstallLexerZipTitle: string = 'Install addon';

implementation

uses
  LCLIntf, LCLProc, LCLType,
  StrUtils,
  IniFiles,
  Zipper,
  ATStringProc,
  proc_files,
  proc_globdata,
  proc_msg,
  proc_lexer_styles;

const
  cTypeLexer = 'lexer';
  cTypePlugin = 'cudatext-plugin';
  cTypeData = 'cudatext-data';

procedure DoInstallData(
  const fn_inf: string;
  out s_report: string;
  out dir_target: string);
var
  ini: TIniFile;
  s_subdir, dir_from: string;
begin
  s_report:= '';
  dir_target:= '';

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
  out s_report: string;
  out dir_target: string);
var
  ini: TIniFile;
  s_section, s_caption, s_module, s_method, s_events,
  s_lexers, s_hotkey, s_lexer_item, s_caption_nice: string;
  i: integer;
begin
  s_report:= '';
  dir_target:= '';

  ini:= TIniFile.Create(fn_inf);
  try
    s_module:= ini.ReadString('info', 'subdir', '');
    if s_module='' then exit;

    dir_target:= GetAppPath(cDirPy)+DirectorySeparator+s_module;
    FCopyDir(ExtractFileDir(fn_inf), dir_target);

    for i:= 1 to cMaxItemsInInstallInf do
    begin
      s_section:= ini.ReadString('item'+Inttostr(i), 'section', '');
      s_caption:= ini.ReadString('item'+Inttostr(i), 'caption', '');
      s_method:= ini.ReadString('item'+Inttostr(i), 'method', '');
      s_events:= ini.ReadString('item'+Inttostr(i), 'events', '');
      s_lexers:= ini.ReadString('item'+Inttostr(i), 'lexers', '');
      s_hotkey:= ini.ReadString('item'+Inttostr(i), 'hotkey', '');

      if s_section='commands' then
      begin
        if s_caption='' then Continue;
        if s_method='' then Continue;

        s_caption_nice:= s_caption;
        s_caption_nice:= StringReplace(s_caption_nice, '&', '', [rfReplaceAll]);
        s_caption_nice:= StringReplace(s_caption_nice, '\', ': ', [rfReplaceAll]);

        if not SEndsWith(s_caption, '\-') then
          s_report:= s_report+msgStatusPackageCommand+' '+s_caption_nice+
            IfThen(s_hotkey<>'', '  ['+s_hotkey+']')+#10;

        //handle "hotkey"
        if s_hotkey<>'' then
        begin
          if s_lexers='' then
            //set in keys.json
            DoOps_SaveKey_ForPluginModuleAndMethod(false,
              'plugin: '+s_caption_nice, s_module, s_method, '', s_hotkey)
          else
          repeat
            //set in "keys lexer nnnn.json" for all items in s_lexers
            s_lexer_item:= SGetItem(s_lexers);
            if s_lexer_item='' then Break;
            DoOps_SaveKey_ForPluginModuleAndMethod(false,
              'plugin: '+s_caption_nice, s_module, s_method, s_lexer_item, s_hotkey);
          until false;
        end;
      end;

      if s_section='events' then
      begin
        if s_events='' then Continue;
        s_report:= s_report+msgStatusPackageEvents+' '+s_events+#10;
      end;
    end;
  finally
    FreeAndNil(ini);
  end;
end;

procedure DoInstallLexer(
  const fn_inf, dir_acp: string;
  Manager: TecSyntaxManager;
  out s_report: string;
  out dir_lexlib: string);
var
  i_lexer, i_sub: integer;
  s_lexer, fn_lexer, fn_acp, fn_lexmap: string;
  an, an_sub: TecSyntAnalyzer;
  ini_lexmap: TIniFile;
begin
  s_report:= '';
  dir_lexlib:= GetAppPath(cDirDataLexerlib);

  with TIniFile.Create(fn_inf) do
  try
    for i_lexer:= 1 to 20 do
    begin
      s_lexer:= ReadString('lexer'+Inttostr(i_lexer), 'file', '');
      if s_lexer='' then Break;

      //copy lexer file
      fn_lexer:= ExtractFileDir(fn_inf)+DirectorySeparator+s_lexer+'.lcf';
      fn_lexmap:= ExtractFileDir(fn_inf)+DirectorySeparator+s_lexer+'.cuda-lexmap';
      fn_acp:= ExtractFileDir(fn_inf)+DirectorySeparator+s_lexer+'.acp';

      if not FileExists(fn_lexer) then
      begin
        MsgBox(msgCannotFindLexerFile+' '+fn_lexer, mb_ok or mb_iconerror);
        exit
      end;

      if FileExists(fn_lexer) then
      begin
        CopyFile(fn_lexer, dir_lexlib+DirectorySeparator+ExtractFileName(fn_lexer));
        s_report:= s_report+msgStatusPackageLexer+' '+s_lexer+#10;
      end;

      if FileExists(fn_lexmap) then
        CopyFile(fn_lexmap, dir_lexlib+DirectorySeparator+ExtractFileName(fn_lexmap))
      else
        s_report:= s_report+msgStatusPackageMissedLexerMap+#10;

      if FileExists(fn_acp) then
      begin
        if dir_acp<>'' then
          CopyFile(fn_acp, dir_acp+DirectorySeparator+ExtractFileName(fn_acp));
        s_report:= s_report+msgStatusPackageAutoCompletion+' '+s_lexer+#10;
      end;

      //install from file
      an:= Manager.FindAnalyzer(s_lexer);
      if an=nil then
        an:= Manager.AddAnalyzer;
      an.LoadFromFile(fn_lexer);

      //also "restore lexer styles"
      if not UiOps.LexerThemes then
      begin
        DoLoadLexerStylesFromFile(an, GetAppPath(cFileLexerStylesBackup));
        DoLexerExportFromLibToFile(an);
      end;

      //set sublexer links
      for i_sub:= 0 to an.SubAnalyzers.Count-1 do
      begin
        s_lexer:= ReadString('lexer'+Inttostr(i_lexer), 'link'+Inttostr(i_sub+1), '');
        if s_lexer='' then Continue;
        if s_lexer='Style sheets' then s_lexer:= 'CSS';
        if s_lexer='Assembler' then s_lexer:= 'Assembly';

        //write [ref] section in cuda-lexmap
        ini_lexmap:= TIniFile.Create(dir_lexlib+DirectorySeparator+ExtractFileName(fn_lexmap));
        try
          ini_lexmap.WriteString('ref', IntToStr(i_sub), s_lexer);
        finally
          FreeAndNil(ini_lexmap);
        end;

        an_sub:= Manager.FindAnalyzer(s_lexer);
        if an_sub<>nil then
        begin
          an.SubAnalyzers.Items[i_sub].SyntAnalyzer:= an_sub;
          //MsgBox('Linked lexer "'+an.LexerName+'" to "'+s_lexer+'"', mb_ok or MB_ICONINFORMATION);
        end
        else
        begin
          s_report:= s_report+(msgCannotFindSublexerInLibrary+' '+s_lexer)+#10;
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

function GetZipDirForInstallInf(unzip: TUnZipper): string;
var
  dir: string;
  i: integer;
begin
  Result:= '';
  //find .inf in root
  for i:= 0 to unzip.Entries.Count-1 do
    if unzip.Entries[i].ArchiveFileName = 'install.inf' then exit;

  //get name of 1st subdir
  with unzip.Entries[0] do
    if (Attributes and faDirectory)<>0 then
    begin
      dir:= ArchiveFileName;
      //find .inf in this subdir
      for i:= 0 to unzip.Entries.Count-1 do
        if unzip.Entries[i].ArchiveFileName = dir+'install.inf' then exit(dir);
    end;
end;

procedure DoInstallAddonFromZip(
  const fn_zip: string;
  const dir_acp: string;
  out StrReport, StrMessage: string;
  out IsInstalled: boolean;
  out NAddonType: TAppAddonType;
  out DirTarget: string;
  Silent: boolean);
var
  unzip: TUnZipper;
  list: TStringlist;
  dir, dir_zipped, fn_inf: string;
  s_title, s_type, s_desc, s_api: string;
begin
  StrReport:= '';
  StrMessage:= '';
  IsInstalled:= false;
  NAddonType:= cAddonTypeUnknown;
  DirTarget:= '';
  dir:= GetTempDirCounted;

  if not DirectoryExists(dir) then
    CreateDir(dir);
  if not DirectoryExists(dir) then
  begin
    MsgBox(msgCannotCreateDir+#10+dir, MB_OK+MB_ICONERROR);
    exit
  end;

  unzip:= TUnZipper.Create;
  try
    unzip.FileName:= fn_zip;
    unzip.OutputPath:= dir;
    try
      unzip.Examine;
      if unzip.Entries.Count=0 then
        raise Exception.Create('Zip is empty');
    except
      MsgBox(msgCannotHandleZip+#10+fn_zip, MB_OK+MB_ICONERROR);
      exit;
    end;

    dir_zipped:= GetZipDirForInstallInf(unzip);
    fn_inf:= dir+DirectorySeparator+dir_zipped+'install.inf';
    if FileExists(fn_inf) then
      DeleteFile(fn_inf);

    list:= TStringList.create;
    try
      list.Add(dir_zipped+'install.inf');
      unzip.UnZipFiles(list);
    finally
      FreeAndNil(list);
    end;

    if not FileExists(fn_inf) then
    begin
      MsgBox(msgCannotFindInstallInfInZip, mb_ok or mb_iconerror);
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
    s_api:= ReadString('info', 'api', '');
  finally
    Free
  end;

  if (s_api<>'') and (s_api>cAppApiVersion) then
  begin
    MsgBox(Format(msgCannotInstallAddonApi, [s_title, s_api]), MB_OK or MB_ICONERROR);
    exit
  end;

  if (s_title='') or (s_type='') then
  begin
    MsgBox(msgStatusIncorrectInstallInfInZip, MB_OK or MB_ICONERROR);
    exit
  end;

  if (s_type<>cTypeLexer) and
    (s_type<>cTypePlugin) and
    (s_type<>cTypeData) then
  begin
    MsgBox(msgStatusUnsupportedAddonType+' '+s_type, MB_OK or MB_ICONERROR);
    exit
  end;

  if not Silent then
  if MsgBox(msgStatusPackageContains+#10#10+
    msgStatusPackageName+' '+s_title+#10+
    IfThen(s_desc<>'', msgStatusPackageDesc+' '+s_desc+#10)+
    msgStatusPackageType+' '+s_type+#10+
    #10+
    msgConfirmInstallIt,
    MB_OKCANCEL or MB_ICONQUESTION)<>ID_OK then exit;

  StrReport:= '';
  if s_type=cTypeLexer then
  begin
    NAddonType:= cAddonTypeLexer;
    DoInstallLexer(fn_inf, dir_acp, AppManager, StrReport, DirTarget)
  end
  else
  if s_type=cTypePlugin then
  begin
    NAddonType:= cAddonTypePlugin;
    StrMessage:= msgStatusInstalledNeedRestart;
    DoInstallPlugin(fn_inf, StrReport, DirTarget)
  end
  else
  if s_type=cTypeData then
  begin
    NAddonType:= cAddonTypeData;
    DoInstallData(fn_inf, StrReport, DirTarget)
  end;

  IsInstalled:= true;
end;


end.

