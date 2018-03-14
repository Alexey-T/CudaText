(*
This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at http://mozilla.org/MPL/2.0/.

Copyright (c) Alexey Torgashin
*)
{$ifdef nn}begin end;{$endif}

procedure TfmMain.DoOps_LoadCommandLineOptions;
var
  SParam: string;
  i: integer;
begin
  FOption_OpenReadOnly:= false;
  FOption_OpenNewWindow:= false;
  FOption_WindowPos:= '';
  FOption_Encoding:= '';

  for i:= 1 to ParamCount do
  begin
    SParam:= ParamStrUTF8(i);
    if not SBeginsWith(SParam, '-') then Continue;

    if (SParam='--readonly') or (SParam='-r') then
    begin
      FOption_OpenReadOnly:= true;
      Continue;
    end;
    if (SParam='--new') or (SParam='-n') then
    begin
      FOption_OpenNewWindow:= true;
      Continue;
    end;
    if (SParam='--version') or (SParam='-v') then
    begin
      MsgStdout(Format(msgCommandLineVersion, [cAppExeVersion]), true);
      Halt;
    end;
    if (SParam='--help') or (SParam='-h') or (SParam='-?') then
    begin
      MsgStdout(msgCommandLineHelp, true);
      Halt;
    end;

    if SBeginsWith(SParam, '--window=') or SBeginsWith(SParam, '-w=') then
    begin
      Delete(SParam, 1, Pos('=', SParam));
      FOption_WindowPos:= SParam;
      Continue;
    end;

    if SBeginsWith(SParam, '--enc=') or SBeginsWith(SParam, '-e=') then
    begin
      Delete(SParam, 1, Pos('=', SParam));
      FOption_Encoding:= AppEncodingShortnameToFullname(SParam);
      if FOption_Encoding='' then
        MsgStdout(Format(msgCommandLineUnknownEncoding, [SParam]));
      Continue;
    end;

    if (SParam='--enclist') or (SParam='-el') then
    begin
      MsgStdout(AppEncodingListAsString, true);
      Halt;
    end;

    {$ifdef darwin}
    //macOS gives param -psn** for debugger
    if SBeginsWith(SParam, '-psn') then Continue;
    {$endif}

    MsgStdout(Format(msgCommandLineUnknownOption, [SParam]));
    Halt;
  end;
end;

procedure TfmMain.DoLoadCommandLine;
const
  cOptionPassive = '/passive /nonear';
  cOptionActive = '/nonear';
var
  Frame: TEditorFrame;
  fn, SOption: string;
  NumLine, NumColumn: integer;
  i: integer;
begin
  for i:= 0 to Length(FFileNamesDroppedInitially)-1 do
  begin
    fn:= FFileNamesDroppedInitially[i];
    if DirectoryExistsUTF8(fn) then
      DoFolderOpen(fn, False)
    else
    if FileExistsUTF8(fn) then
      DoFileOpen(fn, nil, cOptionPassive);
  end;

  //if passed single file, must activate it, else not
  if ParamCount<2 then
    SOption:= cOptionActive
  else
    SOption:= cOptionPassive;

  for i:= 1 to ParamCount do
  begin
    fn:= ParamStrUTF8(i);

    //ignore special params
    if SBeginsWith(fn, '-') then Continue;

    //if dir, open in ProjManager
    if DirectoryExistsUTF8(fn) then
    begin
      MsgStdout('CudaText: opening dir: '+fn);
      DoFolderOpen(fn, True);
      Continue;
    end;

    //get line number (cut from fn)
    SParseFilenameWithTwoNumbers(fn, NumLine, NumColumn);

    Frame:= nil;
    if FileExistsUTF8(fn) then
    begin
      MsgStdout('CudaText: opened file: '+fn);
      Frame:= DoFileOpen(fn, nil, SOption);
    end
    else
    if MsgBox(
      Format(msgConfirmCreateNewFile, [fn]),
      MB_OKCANCEL or MB_ICONQUESTION) = ID_OK then
    begin
      if FCreateFile(fn) then
        MsgStdout('CudaText: created file: '+fn)
      else
        MsgStdout('CudaText: cannot create file: '+fn);
      if FileExistsUTF8(fn) then
        Frame:= DoFileOpen(fn, nil, SOption);
    end;

    if Assigned(Frame) then
    begin
      if NumLine>0 then
        Frame.DoGotoPos(NumColumn-1, NumLine-1);

      if FOption_OpenReadOnly then
      begin
        Frame.ReadOnly:= true;
        MsgStatus(''); //show "[read-only]"
      end;

      if FOption_Encoding<>'' then
      begin
        SetFrameEncoding(Frame, FOption_Encoding, true);
      end;
    end;
  end;
end;


