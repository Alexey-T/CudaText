(*
This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at http://mozilla.org/MPL/2.0/.

Copyright (c) Alexey Torgashin
*)
unit proc_editor_saving;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, LCLType,
  ATSynEdit;

function EditorSaveFileAs(Ed: TATSynEdit; const AFileName: string): boolean;


implementation

uses
  FileUtil, LazFileUtils,
  UTF8Process,
  proc_files,
  proc_msg,
  proc_globdata;


function EditorSaveFileAs(Ed: TATSynEdit; const AFileName: string): boolean;
  //
  procedure SaveSimple(const fn: string);
  begin
    {
    //atomic file saving is NOT a good thing, people write they loose file properties,
    //and even loose data
    if UiOps.AtomicFileSave then
      ....
    else
    }
    Ed.SaveToFile(fn);
  end;
  //
  function IsBadResultFile(const fn: string): boolean;
  begin
    Result:= (not FileExists(fn)) or (FileUtil.FileSize(fn)=0);
  end;
  //
  procedure SaveViaTempCopy(const fn: string);
  var
    fnTemp: string;
    fnPkExec: string;
  begin
    fnTemp:= GetTempFileName('', 'cudatext_');
    SaveSimple(fnTemp);
    if IsBadResultFile(fnTemp) then
      raise EFileNotFoundException.Create(msgCannotSaveFile+#10+fnTemp);

    {$ifdef windows}
    CopyFile(fnTemp, fn)
    {$else}
    //try to run command 'pkexec /bin/cp "temp_filename" "final_filename"' to copy as root
    if DirectoryIsWritable(ExtractFileDir(fn)) then
      CopyFile(fnTemp, fn)
    else
    begin
      fnPkExec:= FindDefaultExecutablePath('pkexec');
      if fnPkExec='' then
        raise EFileNotFoundException.Create('Cannot find "pkexec" program to copy as root. Saved to a temporary file:'#10+fnTemp);
      RunCmdFromPath(fnPkExec, Format('/bin/mv -T "%s" "%s"', [fnTemp, fn]));
      exit;
    end;
    {$endif}

    if IsBadResultFile(fn) then
      raise EFileNotFoundException.Create(msgCannotSaveFile+#10+fn+#10'Saved to a temporary file:'#10+fnTemp);
    DeleteFile(fnTemp);
  end;
  //
var
  OldEncoding: string;
  OldAttr: Longint;
begin
  Result:= true;
  while true do
  try
    AppFileAttrPrepare(AFileName, OldAttr);
    Ed.BeginUpdate;
    try
      try
        SaveSimple(AFileName);
      except
        on E: EConvertError do
          begin
            OldEncoding:= Ed.EncodingName;
            Ed.EncodingName:= cEncNameUtf8_NoBom;
            SaveSimple(AFileName);
            MsgBox(Format(msgCannotSaveFileWithEnc, [OldEncoding]), MB_OK or MB_ICONWARNING);
          end;
        on E: EFOpenError do
          begin
            SaveViaTempCopy(AFileName);
          end;
        on E: EWriteError do //on Linux, saving to smb folder fails, issue #3435
          begin
            SaveViaTempCopy(AFileName);
          end;
        else
          raise;
      end;
    finally
      Ed.EndUpdate;
    end;
    AppFileAttrRestore(AFileName, OldAttr);
    exit;
  except
    on E: Exception do
    begin
      if MsgBox(E.ClassName+#10+E.Message, MB_RETRYCANCEL or MB_ICONERROR) = IDCANCEL then
        exit(false);
    end;
  end;
end;

end.

