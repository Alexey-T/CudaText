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
  Classes, SysUtils, Controls, Forms, FileUtil,
  proc_str,
  ec_SyntAnal;

type
  TAppAddonType = (
    cAddonTypeUnknown,
    cAddonTypePlugin,
    cAddonTypeLexer,
    cAddonTypeLexerLite,
    cAddonTypeData,
    cAddonTypePackage
    );

const
  cAppAddonTypeString: array[TAppAddonType] of string = (
    '',
    'cudatext-plugin',
    'lexer',
    'lexer-lite',
    'cudatext-data',
    'cudatext-package'
    );

function AppAddonKindFromString(const AId: string): TAppAddonType;

procedure DoInstallAddonFromZip(
  const AFilenameZip: string;
  const ADirAcp: string;
  out AStrReport, AStrMessage: string;
  out AIsInstalled: boolean;
  out AAddonType: TAppAddonType;
  out ADirTarget: string;
  out ANeedRestart: boolean;
  const ASilent: boolean);

function CheckValue_OS(S: string): boolean;

implementation

uses
  LCLIntf, LCLProc, LCLType,
  InterfaceBase,
  StrUtils,
  IniFiles,
  Zipper,
  ATStringProc,
  ATStringProc_Separator,
  ATSynEdit_Keymap,
  proc_files,
  proc_globdata,
  proc_msg,
  proc_lexer_styles;


function CheckValue_ReqPlugin(const AText: string): boolean;
var
  Sep: TATStringSeparator;
  SItem: string;
begin
  Result:= true;

  Sep.Init(AText);
  repeat
    if not Sep.GetItemStr(SItem) then Break;
    if not DirectoryExists(AppDir_Py+DirectorySeparator+SItem) then
      exit(false);
  until false;
end;


function CheckValue_ReqLexer(const AText: string): boolean;
var
  Sep: TATStringSeparator;
  SItem: string;
begin
  Result:= true;
  Sep.Init(AText);
  repeat
    if not Sep.GetItemStr(SItem) then Break;
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

  {$ifdef openbsd}
  if Pos(',openbsd,', S)>0 then exit(true);
  if Pos(',openbsd-'+CpuString+',', S)>0 then exit(true);
  {$endif}

  {$ifdef netbsd}
  if Pos(',netbsd,', S)>0 then exit(true);
  if Pos(',netbsd-'+CpuString+',', S)>0 then exit(true);
  {$endif}

  {$ifdef dragonfly}
  if Pos(',dragonfly,', S)>0 then exit(true);
  if Pos(',dragonfly-'+CpuString+',', S)>0 then exit(true);
  {$endif}

  {$ifdef solaris}
  if Pos(',solaris,', S)>0 then exit(true);
  if Pos(',solaris-'+CpuString+',', S)>0 then exit(true);
  {$endif}

  {$ifdef haiku}
  if Pos(',haiku,', S)>0 then exit(true);
  if Pos(',haiku-'+CpuString+',', S)>0 then exit(true);
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
  finally
    FreeAndNil(ini);
  end;

  DeleteFile(AFilenameInf);
  SDirFrom:= ExtractFileDir(AFilenameInf);
  ADirTarget:= AppDir_Data;
  if SSubDir<>'' then
    ADirTarget+= DirectorySeparator+SSubDir;
  AppCopyDir(SDirFrom, ADirTarget);

  AReport:= msgStatusPackageData+' '+ADirTarget;
end;

procedure DoInstallPackage(
  const AFilenameInf: string;
  out AReport: string;
  out ADirTarget: string);
var
  SDirFrom, S: string;
  List: TStringList;
  i: integer;
begin
  AReport:= '';
  DeleteFile(AFilenameInf);
  SDirFrom:= ExtractFileDir(AFilenameInf);
  ADirTarget:= ExtractFileDir(AppDir_Data);

  List:= TStringList.Create;
  try
    //report "data" folders
    FindAllDirectories(List, SDirFrom+DirectorySeparator+'data', false);
    for i:= 0 to List.Count-1 do
    begin
      S:= ExtractFileName(List[i]);
      AReport+= 'folder: data/'+S+#10;
    end;

    //delete plugins, if already installed in CudaText/py
    FindAllDirectories(List, SDirFrom+DirectorySeparator+'py', false);
    for i:= 0 to List.Count-1 do
    begin
      S:= ExtractFileName(List[i]);
      AReport+= 'folder: py/'+S+#10;
      S:= AppDir_Py+DirectorySeparator+S;
      if DirectoryExists(S) then
        DeleteDirectory(S, false);
    end;
  finally
    FreeAndNil(List);
  end;

  AppCopyDir(SDirFrom, ADirTarget);

  AReport+= msgStatusPackagePackage+' '+ADirTarget;
end;

procedure DoInstallLexerLite(
  const AFilenameInf: string;
  out AReport: string);
var
  ini: TIniFile;
  sections: TStringList;
  section, STitle, SLexer,
  DirFrom, DirSettings: string;
  fn_lexer, fn_json: string;
begin
  AReport:= '';
  DirFrom:= ExtractFileDir(AFilenameInf);
  DirSettings:= AppDir_SettingsDefault;

  ini:= TIniFile.Create(AFilenameInf);
  sections:= TStringList.Create;
  try
    STitle:= ini.ReadString('info', 'title', '');
    if STitle='' then exit;

    ini.ReadSections(sections);
    for section in sections do
    begin
      if not SRegexMatchesString(section, 'lexer\d+', true) then Continue;

      SLexer:= ini.ReadString(section, 'file', '');
      if SLexer='' then Break;

      fn_lexer:= DirFrom+DirectorySeparator+SLexer+'.cuda-litelexer';
      fn_json:= DirFrom+DirectorySeparator+'lexer '+SLexer+'.json';

      if FileExists(fn_lexer) then
      begin
        CopyFile(fn_lexer, AppDir_LexersLite+DirectorySeparator+ExtractFileName(fn_lexer));
        AReport:= AReport+msgStatusPackageLexer+' '+SLexer+#10;
      end;

      if FileExists(fn_json) then
      begin
        CopyFile(fn_json, DirSettings+DirectorySeparator+ExtractFileName(fn_json));
        AReport:= AReport+msgStatusPackageLexerSettings+' '+SLexer+#10;
      end;
    end;
  finally
    FreeAndNil(sections);
    FreeAndNil(ini);
  end;
end;

procedure DoInstallPlugin_GetHotkeys(ini: TIniFile; out AHotkeys: string; out AHotkeysCount: integer);
var
  sections: TStringList;
  sec, s_hotkey: string;
begin
  AHotkeys:= '';
  AHotkeysCount:= 0;

  sections:= TStringList.Create;
  try
    ini.ReadSections(sections);

    for sec in sections do
    begin
      if not SRegexMatchesString(sec, 'item\d+', true) then Continue;
      s_hotkey:= ini.ReadString(sec, 'hotkey', '');
      if s_hotkey<>'' then
      begin
        Inc(AHotkeysCount);
        if AHotkeys<>'' then
          AHotkeys+= ', ';
        AHotkeys+= s_hotkey;
      end;
    end;
  finally
    FreeAndNil(sections);
  end;
end;

procedure DoInstallPlugin(
  const AFilenameInf: string;
  out AReport: string;
  out ADirTarget: string;
  out ANeedRestart: boolean;
  const AllowHotkeys: boolean);
var
  ini: TIniFile;
  sections: TStringList;
  ini_section, s_section,
  s_caption, s_module, s_method, s_events,
  s_lexers, s_hotkey, s_lexer_item, s_caption_nice: string;
  Sep: TATStringSeparator;
  Keymap: TATKeymap;
begin
  AReport:= '';
  ADirTarget:= '';

  ini:= TIniFile.Create(AFilenameInf);
  sections:= TStringList.Create;

  try
    s_module:= ini.ReadString('info', 'subdir', '');
    if s_module='' then exit;
    ini.ReadSections(sections);

    ADirTarget:= AppDir_Py+DirectorySeparator+s_module;
    AppCopyDir(ExtractFileDir(AFilenameInf), ADirTarget);

    for ini_section in sections do
    begin
      if not SRegexMatchesString(ini_section, 'item\d+', true) then Continue;

      s_section:= ini.ReadString(ini_section, 'section', '');
      s_caption:= ini.ReadString(ini_section, 'caption', '');
      s_method:= ini.ReadString(ini_section, 'method', '');
      s_events:= ini.ReadString(ini_section, 'events', '');
      s_lexers:= ini.ReadString(ini_section, 'lexers', '');
      if AllowHotkeys then
        s_hotkey:= ini.ReadString(ini_section, 'hotkey', '')
      else
        s_hotkey:= '';

      if s_section='commands' then
      begin
        if s_caption='' then Continue;
        if s_method='' then Continue;

        s_caption_nice:= s_caption;
        s_caption_nice:= StringReplace(s_caption_nice, '&', '', [rfReplaceAll]);
        s_caption_nice:= StringReplace(s_caption_nice, '\', ': ', [rfReplaceAll]);

        if not SEndsWith(s_caption, '\-') then
        begin
          AReport+= msgStatusPackageCommand+' '+s_caption_nice;
          if s_hotkey<>'' then
            AReport+= '  ['+s_hotkey+']';
          AReport+= #10;
        end;

        //handle "hotkey"
        if s_hotkey<>'' then
        begin
          if s_lexers='' then
          begin
            //save to keys.json
            Keymap:= AppKeymapMain;
            if Keymap.GetCommandFromHotkeyString(s_hotkey, '|')>=0 then
              AReport:= Format(msgStatusPluginHotkeyBusy, [s_hotkey])+#10+AReport
            else
              TKeymapHelper.SaveKey_ForPlugin(Keymap, false, 'plugin: '+s_caption_nice, s_module, s_method, '', s_hotkey)
          end
          else
          begin
            //save to "keys nn.json" for all items in s_lexers
            Sep.Init(s_lexers);
            while Sep.GetItemStr(s_lexer_item) do
            begin
              Keymap:= TKeymapHelper.GetForLexer(s_lexer_item);
              if Keymap.GetCommandFromHotkeyString(s_hotkey, '|')>=0 then
                AReport:= Format(msgStatusPluginHotkeyBusy, [s_hotkey])+#10+AReport
              else
                TKeymapHelper.SaveKey_ForPlugin(Keymap, false, 'plugin: '+s_caption_nice, s_module, s_method, s_lexer_item, s_hotkey);
            end;
          end;
        end;
      end;

      if s_section='events' then
      begin
        if s_events='' then Continue;
        ANeedRestart:= true;
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
  out AReport: string);
var
  i_sub: integer;
  ini_section,
  s_lexer, fn_lexer, fn_acp, fn_lexmap, fn_lexmap_final, fn_json: string;
  an, an_sub: TecSyntAnalyzer;
  ini_file: TIniFile;
  sections: TStringList;
  DirLexers, DirSettings: string;
begin
  AReport:= '';
  DirLexers:= AppDir_Lexers;
  DirSettings:= AppDir_SettingsDefault;

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
      fn_json:= ExtractFileDir(AFilenameInf)+DirectorySeparator+'lexer '+s_lexer+'.json';
      fn_acp:= ExtractFileDir(AFilenameInf)+DirectorySeparator+s_lexer+'.acp';

      if not FileExists(fn_lexer) then
      begin
        MsgBox(msgCannotFindLexerFile+' '+fn_lexer, mb_ok or mb_iconerror);
        exit
      end;

      if FileExists(fn_lexer) then
      begin
        CopyFile(fn_lexer, DirLexers+DirectorySeparator+ExtractFileName(fn_lexer));
        AReport:= AReport+msgStatusPackageLexer+' '+s_lexer+#10;
      end;

      if FileExists(fn_lexmap) then
      begin
        fn_lexmap_final:= DirLexers+DirectorySeparator+ExtractFileName(fn_lexmap);
        CopyFile(fn_lexmap, fn_lexmap_final);
      end
      else
        AReport:= AReport+msgStatusPackageMissedLexerMap+#10;

      if FileExists(fn_json) then
      begin
        CopyFile(fn_json, DirSettings+DirectorySeparator+ExtractFileName(fn_json));
        AReport:= AReport+msgStatusPackageLexerSettings+' '+s_lexer+#10;
      end;

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
        DoLoadLexerStylesFromFile_JsonLexerOps(an, AppFile_LexerOps(an.LexerName), false);
      end;

      //set sublexer links
      for i_sub:= 0 to an.SubAnalyzers.Count-1 do
      begin
        s_lexer:= ini_file.ReadString(ini_section, 'link'+Inttostr(i_sub+1), '');
        if s_lexer='' then Continue;

        {
        //write [ref] section in cuda-lexmap
        ini_lexmap:= TIniFile.Create(fn_lexmap_final);
        try
          ini_lexmap.WriteString('ref', IntToStr(i_sub), s_lexer);
        finally
          FreeAndNil(ini_lexmap);
        end;
        }

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

function AppAddonKindFromString(const AId: string): TAppAddonType;
var
  kind: TAppAddonType;
begin
  Result:= cAddonTypeUnknown;
  for kind:= Succ(Low(TAppAddonType)) to High(TAppAddonType) do
    if cAppAddonTypeString[kind]=AId then
      exit(kind);
end;

procedure DoInstallAddonFromZip(
  const AFilenameZip: string;
  const ADirAcp: string;
  out AStrReport, AStrMessage: string;
  out AIsInstalled: boolean;
  out AAddonType: TAppAddonType;
  out ADirTarget: string;
  out ANeedRestart: boolean;
  const ASilent: boolean);
var
  unzip: TUnZipper;
  list: TStringlist;
  ini: TIniFile;
  dir_temp, dir_zipped, fn_inf: string;
  s_title, s_type, s_subdir, s_desc, s_api, s_os: string;
  s_allhotkeys: string;
  s_msgbox: string;
  Num, NumHotkeys: integer;
  bAllowHotkeys: boolean;
  Buttons: TDialogButtons;
  ok: boolean;
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

  ini:= TIniFile.Create(fn_inf);
  try
    s_title:= ini.ReadString('info', 'title', '');
    s_desc:= ini.ReadString('info', 'desc', '');
    s_type:= ini.ReadString('info', 'type', '');
    s_subdir:= ini.ReadString('info', 'subdir', '');
    s_api:= ini.ReadString('info', 'api', '');
    s_os:= ini.ReadString('info', 'os', '');

    s_allhotkeys:= '';
    NumHotkeys:= 0;
    if not ASilent then
      DoInstallPlugin_GetHotkeys(ini, s_allhotkeys, NumHotkeys);
  finally
    FreeAndNil(ini);
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

  if (s_api<>'') then
  begin
    //support 'api' key in 2 formats:
    // number 300
    // string '1.0.300'
    Num:= StrToIntDef(s_api, 0);
    if Num>0 then
      ok:= Num<=cAppApiVersion
    else
      ok:= s_api <= '1.0.'+IntToStr(cAppApiVersion);
    if not ok then
    begin
      if not ASilent then
        MsgBox(Format(msgCannotInstallAddonApi, [s_title, s_api]), MB_OK or MB_ICONERROR);
      exit;
    end;
  end;

  if (s_title='') or (s_type='') then
  begin
    MsgBox(msgStatusIncorrectInstallInfInZip, MB_OK or MB_ICONERROR);
    exit
  end;

  AAddonType:= AppAddonKindFromString(s_type);
  if AAddonType=cAddonTypeUnknown then
  begin
    MsgBox(msgStatusUnsupportedAddonType+' '+s_type, MB_OK or MB_ICONERROR);
    exit
  end;

  bAllowHotkeys:= false;
  if not ASilent then
  begin
    s_msgbox:=
      msgStatusPackageContains+#10#10+
      msgStatusPackageName+' '+s_title+#10+
      IfThen(s_desc<>'', msgStatusPackageDesc+' '+s_desc+#10)+
      msgStatusPackageType+' '+s_type+ IfThen(AAddonType=cAddonTypeData, ' / '+s_subdir)+
      IfThen(NumHotkeys>0, #10+Format(msgConfirmHotkeyList, [NumHotkeys, s_allhotkeys]))+
      #10#10+msgConfirmInstallIt;

    Buttons:= TDialogButtons.Create(TDialogButton);
    try
      with Buttons.Add do
      begin
        Caption:= msgButtonOk;
        ModalResult:= mrOk;
      end;
      if NumHotkeys>0 then
        with Buttons.Add do
        begin
          Caption:= msgConfirmOkNoHotkeys;
          ModalResult:= mrNo;
        end;
      with Buttons.Add do
      begin
        Caption:= msgButtonCancel;
        ModalResult:= mrCancel;
      end;
      Buttons.DefaultButton:= Buttons.Items[0];

      case AskUser(msgTitle, s_msgbox, idDialogConfirm, Buttons, 0) of
        mrOk:
          bAllowHotkeys:= true;
        mrNo:
          bAllowHotkeys:= false;
        mrCancel:
          exit;
      end;
    finally
      FreeAndNil(Buttons);
    end;
  end;

  AStrReport:= '';
  case AAddonType of
    cAddonTypeLexer:
      begin
        DoInstallLexer(fn_inf, ADirAcp, AStrReport);
        ADirTarget:= AppDir_Lexers;
      end;
    cAddonTypeLexerLite:
      begin
        DoInstallLexerLite(fn_inf, AStrReport);
        ADirTarget:= AppDir_LexersLite;
      end;
    cAddonTypePlugin:
      begin
        AStrMessage:= msgStatusInstalledNeedRestart;
        DoInstallPlugin(fn_inf, AStrReport, ADirTarget, ANeedRestart, bAllowHotkeys)
      end;
    cAddonTypeData:
      begin
        DoInstallData(fn_inf, AStrReport, ADirTarget)
      end;
    cAddonTypePackage:
      begin
        DoInstallPackage(fn_inf, AStrReport, ADirTarget);
      end;
  end;

 finally
  //cleanup
  DeleteDirectory(dir_temp, false);
 end;

  AIsInstalled:= true;
end;


end.

