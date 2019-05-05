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
  proc_str,
  ec_SyntAnal;

type
  TAppAddonType = (
    cAddonTypeUnknown,
    cAddonTypePlugin,
    cAddonTypeLexer,
    cAddonTypeLexerLite,
    cAddonTypeData
    );

procedure DoInstallAddonFromZip(
  const AFilenameZip: string;
  const ADirAcp: string;
  out AStrReport, AStrMessage: string;
  out AIsInstalled: boolean;
  out AAddonType: TAppAddonType;
  out ADirTarget: string;
  ASilent: boolean);

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
  cTypeLexerLite = 'lexer-lite';
  cTypePlugin = 'cudatext-plugin';
  cTypeData = 'cudatext-data';


function CheckValue_ReqPlugin(S: string): boolean;
var
  SItem, DirPy: string;
begin
  Result:= true;
  DirPy:= GetAppPath(cDirPy);

  repeat
    SItem:= SGetItem(S);
    if SItem='' then Break;
    if not DirectoryExists(DirPy+DirectorySeparator+SItem) then
      exit(false);
  until false;
end;


function CheckValue_ReqLexer(S: string): boolean;
var
  SItem: string;
begin
  Result:= true;
  repeat
    SItem:= SGetItem(S);
    if SItem='' then Break;
    if AppManager.FindLexerByName(SItem)=nil then
      exit(false);
  until false;
end;


function CheckValue_OS(S: string): boolean;
var
  CpuString: string;
begin
  Result:= false;
  if S='' then exit(true);
  S:= ','+LowerCase(S)+',';

  CpuString:= LowerCase({$I %FPCTARGETCPU%});

  {$ifdef windows}
  if Pos(',win,', S)>0 then exit(true);
  if Pos(',win-'+CpuString+',', S)>0 then exit(true);
  {$endif}

  {$ifdef linux}
  if Pos(',linux,', S)>0 then exit(true);
  if Pos(',linux-'+CpuString+',', S)>0 then exit(true);
  {$endif}

  {$ifdef darwin}
  if Pos(',macos,', S)>0 then exit(true);
  {$endif}

  {$ifdef freebsd}
  if Pos(',freebsd,', S)>0 then exit(true);
  if Pos(',freebsd-'+CpuString+',', S)>0 then exit(true);
  {$endif}

  {$ifdef solaris}
  if Pos(',solaris,', S)>0 then exit(true);
  if Pos(',solaris-'+CpuString+',', S)>0 then exit(true);
  {$endif}
end;


procedure DoInstallData(
  const AFilenameInf: string;
  out AReport: string;
  out ADirTarget: string);
var
  ini: TIniFile;
  SSubDir, SDirFrom: string;
begin
  AReport:= '';
  ADirTarget:= '';

  ini:= TIniFile.Create(AFilenameInf);
  try
    SSubDir:= ini.ReadString('info', 'subdir', '');
    if SSubDir='' then exit;
  finally
    FreeAndNil(ini);
  end;

  DeleteFile(AFilenameInf);
  SDirFrom:= ExtractFileDir(AFilenameInf);
  ADirTarget:= GetAppPath(cDirData)+DirectorySeparator+SSubDir;
  FCopyDir(SDirFrom, ADirTarget);

  AReport:= 'data files: '+ADirTarget;
end;

procedure DoInstallLexerLite(
  const AFilenameInf: string;
  out AReport: string);
var
  ini: TIniFile;
  SName, SDirFrom, SDirTo: string;
begin
  AReport:= '';

  ini:= TIniFile.Create(AFilenameInf);
  try
    SName:= ini.ReadString('info', 'title', '');
    if SName='' then exit;
  finally
    FreeAndNil(ini);
  end;

  DeleteFile(AFilenameInf);
  SDirFrom:= ExtractFileDir(AFilenameInf);
  SDirTo:= GetAppPath(cDirDataLexersLite);
  FCopyDir(SDirFrom, SDirTo);

  AReport:= 'lite lexer: '+SName;
end;


procedure DoInstallPlugin(
  const AFilenameInf: string;
  out AReport: string;
  out ADirTarget: string);
var
  ini: TIniFile;
  sections: TStringList;
  ini_section, s_section, s_caption, s_module, s_method, s_events,
  s_lexers, s_hotkey, s_lexer_item, s_caption_nice: string;
begin
  AReport:= '';
  ADirTarget:= '';

  ini:= TIniFile.Create(AFilenameInf);
  sections:= TStringList.Create;

  try
    s_module:= ini.ReadString('info', 'subdir', '');
    if s_module='' then exit;
    ini.ReadSections(sections);

    ADirTarget:= GetAppPath(cDirPy)+DirectorySeparator+s_module;
    FCopyDir(ExtractFileDir(AFilenameInf), ADirTarget);

    for ini_section in sections do
    begin
      if not SRegexMatchesString(ini_section, 'item\d+', true) then Continue;

      s_section:= ini.ReadString(ini_section, 'section', '');
      s_caption:= ini.ReadString(ini_section, 'caption', '');
      s_method:= ini.ReadString(ini_section, 'method', '');
      s_events:= ini.ReadString(ini_section, 'events', '');
      s_lexers:= ini.ReadString(ini_section, 'lexers', '');
      s_hotkey:= ini.ReadString(ini_section, 'hotkey', '');

      if s_section='commands' then
      begin
        if s_caption='' then Continue;
        if s_method='' then Continue;

        s_caption_nice:= s_caption;
        s_caption_nice:= StringReplace(s_caption_nice, '&', '', [rfReplaceAll]);
        s_caption_nice:= StringReplace(s_caption_nice, '\', ': ', [rfReplaceAll]);

        if not SEndsWith(s_caption, '\-') then
          AReport:= AReport+msgStatusPackageCommand+' '+s_caption_nice+
            IfThen(s_hotkey<>'', '  ['+s_hotkey+']')+#10;

        //handle "hotkey"
        if s_hotkey<>'' then
        begin
          if AppKeymap.GetCommandFromHotkeyString(s_hotkey, '|')>=0 then
            AReport:= Format(msgStatusPluginHotkeyBusy, [s_hotkey])+#10+AReport
          else
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
        AReport:= AReport+msgStatusPackageEvents+' '+s_events+#10;
      end;
    end;
  finally
    FreeAndNil(sections);
    FreeAndNil(ini);
  end;
end;

procedure DoInstallLexer(
  const AFilenameInf, ADirAcp: string;
  out AReport: string;
  out ADirLexlib: string);
var
  i_sub: integer;
  ini_section,
  s_lexer, fn_lexer, fn_acp, fn_lexmap: string;
  an, an_sub: TecSyntAnalyzer;
  ini_file, ini_lexmap: TIniFile;
  sections: TStringList;
begin
  AReport:= '';
  ADirLexlib:= GetAppPath(cDirDataLexers);

  ini_file:= TIniFile.Create(AFilenameInf);
  sections:= TStringList.Create;

  try
    ini_file.ReadSections(sections);
    for ini_section in sections do
    begin
      if not SRegexMatchesString(ini_section, 'lexer\d+', true) then Continue;

      s_lexer:= ini_file.ReadString(ini_section, 'file', '');
      if s_lexer='' then Break;

      //copy lexer file
      fn_lexer:= ExtractFileDir(AFilenameInf)+DirectorySeparator+s_lexer+'.lcf';
      fn_lexmap:= ExtractFileDir(AFilenameInf)+DirectorySeparator+s_lexer+'.cuda-lexmap';
      fn_acp:= ExtractFileDir(AFilenameInf)+DirectorySeparator+s_lexer+'.acp';

      if not FileExists(fn_lexer) then
      begin
        MsgBox(msgCannotFindLexerFile+' '+fn_lexer, mb_ok or mb_iconerror);
        exit
      end;

      if FileExists(fn_lexer) then
      begin
        CopyFile(fn_lexer, ADirLexlib+DirectorySeparator+ExtractFileName(fn_lexer));
        AReport:= AReport+msgStatusPackageLexer+' '+s_lexer+#10;
      end;

      if FileExists(fn_lexmap) then
        CopyFile(fn_lexmap, ADirLexlib+DirectorySeparator+ExtractFileName(fn_lexmap))
      else
        AReport:= AReport+msgStatusPackageMissedLexerMap+#10;

      if FileExists(fn_acp) then
      begin
        if ADirAcp<>'' then
          CopyFile(fn_acp, ADirAcp+DirectorySeparator+ExtractFileName(fn_acp));
        AReport:= AReport+msgStatusPackageAutoCompletion+' '+s_lexer+#10;
      end;

      //install from file
      an:= AppManager.FindLexerByName(s_lexer);
      if an=nil then
        an:= AppManager.AddLexer;
      an.LoadFromFile(fn_lexer);

      //also "restore lexer styles"
      if not UiOps.LexerThemes then
      begin
        DoLoadLexerStylesFromFile_JsonLexerOps(an, GetAppLexerOpsFilename(an.LexerName), false);
      end;

      //set sublexer links
      for i_sub:= 0 to an.SubAnalyzers.Count-1 do
      begin
        s_lexer:= ini_file.ReadString(ini_section, 'link'+Inttostr(i_sub+1), '');
        if s_lexer='' then Continue;
        if s_lexer='Style sheets' then s_lexer:= 'CSS';
        if s_lexer='Assembler' then s_lexer:= 'Assembly';

        //write [ref] section in cuda-lexmap
        ini_lexmap:= TIniFile.Create(ADirLexlib+DirectorySeparator+ExtractFileName(fn_lexmap));
        try
          ini_lexmap.WriteString('ref', IntToStr(i_sub), s_lexer);
        finally
          FreeAndNil(ini_lexmap);
        end;

        an_sub:= AppManager.FindLexerByName(s_lexer);
        if an_sub<>nil then
        begin
          an.SubAnalyzers.Items[i_sub].SyntAnalyzer:= an_sub;
          //MsgBox('Linked lexer "'+an.LexerName+'" to "'+s_lexer+'"', mb_ok or MB_ICONINFORMATION);
        end
        else
        begin
          AReport:= AReport+(msgCannotFindSublexerInLibrary+' '+s_lexer)+#10;
          Continue;
        end;
      end;
    end;
  finally
    FreeAndNil(sections);
    FreeAndNil(ini_file);
  end;
end;

function GetTempDirCounted: string;
var
  n: integer;
begin
  for n:= 0 to 100 do
  begin
    Result:= GetTempDir+DirectorySeparator+'cudatext_'+Format('%3.3d', [n]);
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
  const AFilenameZip: string;
  const ADirAcp: string;
  out AStrReport, AStrMessage: string;
  out AIsInstalled: boolean;
  out AAddonType: TAppAddonType;
  out ADirTarget: string;
  ASilent: boolean);
var
  unzip: TUnZipper;
  list: TStringlist;
  dir_temp, dir_zipped, fn_inf: string;
  s_title, s_type, s_subdir, s_desc, s_api, s_os: string;
begin
  AStrReport:= '';
  AStrMessage:= '';
  AIsInstalled:= false;
  AAddonType:= cAddonTypeUnknown;
  ADirTarget:= '';
  dir_temp:= GetTempDirCounted;

  if not DirectoryExists(dir_temp) then
    CreateDir(dir_temp);
  if not DirectoryExists(dir_temp) then
  begin
    MsgBox(msgCannotCreateDir+#10+dir_temp, MB_OK+MB_ICONERROR);
    exit
  end;

 try
  unzip:= TUnZipper.Create;
  try
    unzip.FileName:= AFilenameZip;
    unzip.OutputPath:= dir_temp;
    try
      unzip.Examine;
      if unzip.Entries.Count=0 then
        raise Exception.Create('Zip is empty');
    except
      MsgBox(msgCannotHandleZip+#10+AFilenameZip, MB_OK+MB_ICONERROR);
      exit;
    end;

    dir_zipped:= GetZipDirForInstallInf(unzip);
    fn_inf:= dir_temp+DirectorySeparator+dir_zipped+'install.inf';
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
    s_subdir:= ReadString('info', 'subdir', '');
    s_api:= ReadString('info', 'api', '');
    s_os:= ReadString('info', 'os', '');
  finally
    Free
  end;

  if not CheckValue_OS(s_os) then
  begin
    if not ASilent then
      MsgBox(Format(msgCannotInstallOnOS, [s_title, s_os]), MB_OK or MB_ICONERROR);
    exit
  end;

  (*
  if not CheckValue_ReqPlugin(s_req) then
  begin
    if not ASilent then
      MsgBox(Format(msgCannotInstallReqPlugin, [s_title, s_req]), MB_OK or MB_ICONERROR);
    exit
  end;

  if not CheckValue_ReqLexer(s_req_lexer) then
  begin
    if not ASilent then
      MsgBox(Format(msgCannotInstallReqLexer, [s_title, s_req_lexer]), MB_OK or MB_ICONERROR);
    exit
  end;
  *)

  if (s_api<>'') and (s_api>cAppApiVersion) then
  begin
    if not ASilent then
      MsgBox(Format(msgCannotInstallAddonApi, [s_title, s_api]), MB_OK or MB_ICONERROR);
    exit
  end;

  if (s_title='') or (s_type='') then
  begin
    MsgBox(msgStatusIncorrectInstallInfInZip, MB_OK or MB_ICONERROR);
    exit
  end;

  if (s_type<>cTypeLexer) and
    (s_type<>cTypeLexerLite) and
    (s_type<>cTypePlugin) and
    (s_type<>cTypeData) then
  begin
    MsgBox(msgStatusUnsupportedAddonType+' '+s_type, MB_OK or MB_ICONERROR);
    exit
  end;

  if not ASilent then
  if MsgBox(msgStatusPackageContains+#10#10+
    msgStatusPackageName+' '+s_title+#10+
    IfThen(s_desc<>'', msgStatusPackageDesc+' '+s_desc+#10)+
    msgStatusPackageType+' '+s_type+ IfThen(s_type=cTypeData, ' / '+s_subdir)+ #10+
    #10+
    msgConfirmInstallIt,
    MB_OKCANCEL or MB_ICONQUESTION)<>ID_OK then exit;

  AStrReport:= '';
  if s_type=cTypeLexer then
  begin
    AAddonType:= cAddonTypeLexer;
    DoInstallLexer(fn_inf, ADirAcp, AStrReport, ADirTarget)
  end
  else
  if s_type=cTypeLexerLite then
  begin
    AAddonType:= cAddonTypeLexerLite;
    DoInstallLexerLite(fn_inf, AStrReport);
  end
  else
  if s_type=cTypePlugin then
  begin
    AAddonType:= cAddonTypePlugin;
    AStrMessage:= msgStatusInstalledNeedRestart;
    DoInstallPlugin(fn_inf, AStrReport, ADirTarget)
  end
  else
  if s_type=cTypeData then
  begin
    AAddonType:= cAddonTypeData;
    DoInstallData(fn_inf, AStrReport, ADirTarget)
  end;

 finally
  //cleanup
  DeleteDirectory(dir_temp, false);
 end;

  AIsInstalled:= true;
end;


end.

