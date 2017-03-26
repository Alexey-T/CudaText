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
    if SBeginsWith(SParam, '--window=') then
    begin
      FOption_WindowPos:= Copy(SParam, Length('--window=')+1, MaxInt);
      Continue;
    end;
    if SBeginsWith(SParam, '-w=') then
    begin
      FOption_WindowPos:= Copy(SParam, Length('-w=')+1, MaxInt);
      Continue;
    end;

    MsgStdout(Format(msgCommandLineUnknownOption, [SParam]));
    Halt;
  end;
end;

procedure TfmMain.DoLoadCommandLine;
var
  Frame: TEditorFrame;
  fn: string;
  NumLine, NumColumn: integer;
  i: integer;
begin
  for i:= 0 to Length(FFileNamesDroppedInitially)-1 do
  begin
    fn:= FFileNamesDroppedInitially[i];
    if FileExistsUTF8(fn) then
      DoFileOpen(fn);
  end;

  for i:= 1 to ParamCount do
  begin
    fn:= ParamStrUTF8(i);

    {$ifdef darwin}
    //OSX 10.8 gives param "-psn**"
    if SBeginsWith(fn, '-psn') then Continue;
    {$endif}

    //ignore special params
    if SBeginsWith(fn, '--') then Continue;

    //don't take folder
    if DirectoryExistsUTF8(fn) then
    begin
      MsgStdout('CudaText: cannot open folder: '+fn);
      Continue;
    end;

    //get line number (cut from fn)
    SParseFilenameWithTwoNumbers(fn, NumLine, NumColumn);

    Frame:= nil;
    if FileExistsUTF8(fn) then
    begin
      MsgStdout('CudaText: opened file: '+fn);
      Frame:= DoFileOpen(fn)
    end
    else
    if MsgBox(
      Format(msgConfirmCreateNewFile, [fn]),
      MB_OKCANCEL or MB_ICONQUESTION) = ID_OK then
    begin
      MsgStdout('CudaText: created file: '+fn);
      FCreateFile(fn);
      if FileExistsUTF8(fn) then
        Frame:= DoFileOpen(fn);
    end;

    if Assigned(Frame) then
    begin
      if NumLine>0 then
      begin
        Frame.Editor.LineTop:= NumLine-1;
        Frame.TopLineTodo:= NumLine-1;
        Frame.Editor.DoGotoPos_AndUnfold(
          Point(IfThen(NumColumn>0, NumColumn-1, 0), NumLine-1),
          Point(-1, -1),
          UiOps.FindIndentHorz,
          UiOps.FindIndentVert);
        Frame.Editor.Update;
      end;

      if FOption_OpenReadOnly then
      begin
        Frame.ReadOnly:= true;
        MsgStatus(''); //show "[read-only]"
      end;
    end;
  end;
end;


