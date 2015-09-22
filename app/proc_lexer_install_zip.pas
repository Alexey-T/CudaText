(*
This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at http://mozilla.org/MPL/2.0/.

Copyright (c) Alexey Torgashin
*)
unit proc_lexer_install_zip;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, FileUtil,
  ecSyntAnal;

function DoInstallLexerFromZip(const fn_zip: string;
  Manager: TecSyntaxManager;
  const dir_acp: string;
  out s_installed: string): boolean;

var
  cInstallLexerZipTitle: string = 'Install zip';

implementation

uses
  LCLIntf, LCLProc, LCLType,
  IniFiles,
  Zipper;

function MsgBox(const msg: string; flags: integer): integer;
begin
  Result:= Application.MessageBox(PChar(msg), PChar(cInstallLexerZipTitle), flags);
end;

function DoInstallLexerFromZip(const fn_zip: string;
  Manager: TecSyntaxManager;
  const dir_acp: string;
  out s_installed: string): boolean;
var
  unzip: TUnZipper;
  list: TStringlist;
  dir, fn_inf, fn_lexer, fn_acp: string;
  s_title, s_type, s_lexer: string;
  an, an_sub: TecSyntAnalyzer;
  i_lexer, i_sub: integer;
begin
  Result:= false;
  dir:= GetTempDir(false)+DirectorySeparator+'zip_lexer';
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
    s_type:= ReadString('info', 'type', '');
    //s_subdir:= ReadString('info', 'subdir', '');
  finally
    Free
  end;

  if (s_title='') or (s_type='') then
  begin
    MsgBox('Incorrect install.inf in zip', mb_ok or mb_iconerror);
    exit
  end;

  if (s_type<>'lexer') then
  begin
    MsgBox('Unsupported addon type: '+s_type, mb_ok or mb_iconerror);
    exit
  end;

  if MsgBox('This package contains:'#13#13+
    'title: '+s_title+#13+
    'type: '+s_type+#13#13+
    'Do you want to install it?',
    MB_OKCANCEL or MB_ICONQUESTION)<>id_ok then exit;

  s_installed:= '';
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
      s_installed:= s_installed+s_lexer+#13;

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

  Result:= true;
end;


end.

