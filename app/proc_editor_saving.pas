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
  {$ifdef windows}
  proc_windows_elevated,
  {$else}
  Process,
  proc_editor,
  {$endif}
  proc_files,
  proc_msg,
  proc_globdata;

procedure SaveSimple(Ed: TATSynEdit; const fn: string);
var
  dir: string;
begin
  dir:= ExtractFileDir(fn);
  if not DirectoryExists(dir) then
  begin
    if ForceDirectory(dir) then
      MsgLogConsole('NOTE: Saving, file''s folder does not exist, recreated: '+dir)
    else
      MsgLogConsole('ERROR: Saving, file''s folder does not exist, cannot recreate: '+dir);

    if not DirectoryExists(dir) then
    begin
      MsgBox(
        msgCannotSaveFile+#10+AppCollapseHomeDirInFilename(fn)+#10#10+
        msgCannotFindFolder+#10+AppCollapseHomeDirInFilename(dir),
        MB_OK or MB_ICONERROR);
      exit;
    end;
  end;

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
  //
  procedure MsgBox_ErrorAndSavedTempFile;
  begin
    MsgBox(msgCannotSaveFile+#10+AppCollapseHomeDirInFilename(fn)+#10#10+
           msgStatusSavedTempFile+#10+AppCollapseHomeDirInFilename(fnTemp),
           MB_OK or MB_ICONERROR);
  end;
  //
var
  bDocEmpty: boolean;
  {$ifdef windows}
  SCopyParams: UnicodeString;
  {$else}
  SOutput: string;
  {$endif}
  dir: string;
begin
  dir:= ExtractFileDir(fn);
  if not DirectoryExists(dir) then
  begin
    MsgBox(msgCannotSaveFile+#10+AppCollapseHomeDirInFilename(fn)+#10#10+
           msgCannotFindFolder+#10+AppCollapseHomeDirInFilename(dir),
           MB_OK or MB_ICONERROR);
    exit;
  end;

  bDocEmpty:= Ed.IsEmpty;
  fnTemp:= GetTempFileName('', 'cudatext_');
  SaveSimple(Ed, fnTemp);
  Ed.FileName:= fn; //Ed.FileName was changed to fnTemp

  if IsBadResultFile(fnTemp, bDocEmpty) then
  begin
    MsgBox(msgCannotSaveFile+#10+AppCollapseHomeDirInFilename(fnTemp),
           MB_OK or MB_ICONERROR);
    exit;
  end;

  {$ifdef windows}
  SCopyParams:= UnicodeFormat('/C echo F | xcopy "%s" "%s" /r /h /y', [fnTemp, fn]);
  if not RunElevated('cmd.exe', SCopyParams, true, 6000) then
  begin
    MsgBox_ErrorAndSavedTempFile;
    exit;
  end;
  {$else}
  if cSystemHasPkExec and UiOps.AllowRunPkExec then
  begin
    if FileIsWritable(fn) then
    begin
      if not CopyFile(fnTemp, fn) then
      begin
        MsgBox_ErrorAndSavedTempFile;
        exit;
      end;
    end
    else
    if cCannotSaveToWriteProtectedDirButDontWantPkExec then
      MsgBox(msgCannotSaveAndDontWantToRunPkExec+#10#10+
             Format('cp -T "%s" "%s"', [fnTemp, fn]), MB_OK or MB_ICONWARNING)
    else
    if not RunCommand('pkexec', ['/bin/cp', '-T', fnTemp, fn], SOutput, [poWaitOnExit]) then
    begin
      MsgBox_ErrorAndSavedTempFile;
      exit;
    end;
  end
  else
  if not CopyFile(fnTemp, fn) then
  begin
    MsgBox_ErrorAndSavedTempFile;
    exit;
  end;
  {$endif}

  if IsBadResultFile(fn, bDocEmpty) then
  begin
    MsgBox_ErrorAndSavedTempFile;
    exit;
  end;

  DeleteFile(fnTemp);
end;


function EditorSaveFileAs(Ed: TATSynEdit; const AFileName: string): boolean;
var
  OldEncoding: string;
  OldAttr: Longint;
begin
  Result:= true;
  while true do //loop attempts forever, until correct saving which calls Break
    try
      AppFileAttrPrepare(AFileName, OldAttr);

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

      Ed.Update; //'line states' maybe changed by saving
      AppFileAttrRestore(AFileName, OldAttr);
      Break; //break the 'while true do'
    except
      on E: Exception do
      begin
        if MsgBox(E.ClassName+#10+E.Message, MB_RETRYCANCEL or MB_ICONERROR) = IDCANCEL then
          exit(false);
      end;
    end;
end;

end.

