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
  ATSynEdit;

function EditorSaveFileAs(Ed: TATSynEdit; const AFileName: string): boolean;


implementation

uses
  SysUtils, Classes, Controls, LCLType,
  FileUtil, LazFileUtils,
  Process,
  proc_editor,
  proc_files,
  proc_msg,
  proc_globdata;

procedure SaveSimple(Ed: TATSynEdit; const fn: string);
begin
  Ed.SaveToFile(fn);
end;

function IsBadResultFile(const fn: string; AllowEmpty: boolean): boolean;
begin
  Result:= (not FileExists(fn)) or
    (not AllowEmpty and (FileUtil.FileSize(fn)=0));
end;

procedure SaveViaTempCopy(Ed: TATSynEdit; const fn: string);
var
  fnTemp: string;
  SOutput: string;
  bDocEmpty: boolean;
begin
  bDocEmpty:= EditorIsEmpty(Ed);
  fnTemp:= GetTempFileName('', 'cudatext_');
  SaveSimple(Ed, fnTemp);
  Ed.FileName:= fn; //Ed.FileName was changed to fnTemp

  if IsBadResultFile(fnTemp, bDocEmpty) then
    raise EFileNotFoundException.Create(msgCannotSaveFile+#10+AppCollapseHomeDirInFilename(fnTemp));

  if cSystemHasPkExec and UiOps.AllowRunPkExec then
  begin
    if FileIsWritable(fn) then
    begin
      if not CopyFile(fnTemp, fn) then
        raise EWriteError.Create(msgCannotSaveFile+#10+AppCollapseHomeDirInFilename(fn));
    end
    else
    begin
      if not RunCommand('pkexec', ['/bin/cp', '-T', fnTemp, fn], SOutput, [poWaitOnExit]) then
        raise EFileNotFoundException.Create(msgCannotFindPkExec+#10+msgStatusSavedTempFile+#10+AppCollapseHomeDirInFilename(fnTemp));
    end;
  end
  else
  begin
    if not CopyFile(fnTemp, fn) then
      raise EWriteError.Create(msgCannotSaveFile+#10+AppCollapseHomeDirInFilename(fn));
  end;

  if IsBadResultFile(fn, bDocEmpty) then
    raise EFileNotFoundException.Create(
          msgCannotSaveFile+#10+AppCollapseHomeDirInFilename(fn)+#10+
          msgStatusSavedTempFile+#10+AppCollapseHomeDirInFilename(fnTemp));
  DeleteFile(fnTemp);
end;


function EditorSaveFileAs(Ed: TATSynEdit; const AFileName: string): boolean;
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
        SaveSimple(Ed, AFileName);
      except
        on E: EConvertError do
          begin
            OldEncoding:= Ed.EncodingName;
            Ed.EncodingName:= cEncNameUtf8_NoBom;
            SaveSimple(Ed, AFileName);
            MsgBox(Format(msgCannotSaveFileWithEnc, [OldEncoding]), MB_OK or MB_ICONWARNING);
          end;
        on E: EFOpenError do
          begin
            SaveViaTempCopy(Ed, AFileName);
          end;
        on E: EWriteError do //on Linux, saving to smb folder fails, issue #3435
          begin
            SaveViaTempCopy(Ed, AFileName);
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

