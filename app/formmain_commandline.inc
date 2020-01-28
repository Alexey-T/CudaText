(*
This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at http://mozilla.org/MPL/2.0/.

Copyright (c) Alexey Torgashin
*)
{$ifdef nn}begin end;{$endif}

procedure TfmMain.DoOps_LoadCommandLineOptions;
var
  Items: array of string;
  i: integer;
begin
  SetLength(Items, ParamCount);
  for i:= 0 to Length(Items)-1 do
    Items[i]:= ParamStrUTF8(i+1);
  DoOps_LoadCommandLineOptionsEx(Items, true);
end;

procedure TfmMain.DoOps_LoadCommandLineOptionsEx(const AItems: array of string; AHaltOnBadParam: boolean);
var
  SParam: string;
  i: integer;
begin
  FOption_OpenReadOnly:= false;
  FOption_OpenNewWindow:= false;
  FOption_WindowPos:= '';
  FOption_Encoding:= '';
  FOption_FileOpenOptions:= '';

  for i:= 0 to Length(AItems)-1 do
  begin
    SParam:= AItems[i];
    if not SBeginsWith(SParam, '-') then Continue;

    if (SParam='-r') then
    begin
      FOption_OpenReadOnly:= true;
      Continue;
    end;

    if (SParam='-n') then
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

    if SBeginsWith(SParam, '-w=') then
    begin
      Delete(SParam, 1, Pos('=', SParam));
      FOption_WindowPos:= SParam;
      Continue;
    end;

    if SBeginsWith(SParam, '-e=') then
    begin
      Delete(SParam, 1, Pos('=', SParam));
      FOption_Encoding:= AppEncodingShortnameToFullname(SParam);
      if FOption_Encoding='' then
        MsgStdout(Format(msgCommandLineUnknownEncoding, [SParam]));
      Continue;
    end;

    if (SParam='-el') then
    begin
      MsgStdout(AppEncodingListAsString, true);
      Halt;
    end;

    if (SParam='-nh') then
    begin
      FOption_FileOpenOptions+= '/nohistory';
      Continue;
    end;

    if SBeginsWith(SParam, '-z=') then
    begin
      Delete(SParam, 1, Pos('=', SParam));
      case SParam of
        'text':    FOption_FileOpenOptions+= '/view-text';
        'binary':  FOption_FileOpenOptions+= '/view-binary';
        'hex':     FOption_FileOpenOptions+= '/view-hex';
        'unicode': FOption_FileOpenOptions+= '/view-unicode';
      end;
      Continue;
    end;

    {$ifdef darwin}
    //macOS gives param -psn** for debugger
    if SBeginsWith(SParam, '-psn') then Continue;
    {$endif}

    if AHaltOnBadParam then
    begin
      MsgStdout(Format(msgCommandLineUnknownOption, [SParam]));
      Halt;
    end;
  end;
end;

procedure TfmMain.DoLoadCommandLine_SecondInstance(const AParams: array of string);
var
  Frame: TEditorFrame;
  SParam: string;
  NLine, NColumn, i: integer;
  bReadOnly: boolean;
  sOpenOptions, sEncoding: string;
begin
  bReadOnly:= false;
  sOpenOptions:= '';
  sEncoding:= '';

  for i:= 0 to Length(AParams)-1 do
  begin
    SParam:= AParams[i];
    if SParam='' then
      Continue;

    if SParam='-r' then
    begin
      bReadOnly:= true;
      Continue;
    end;

    if SParam='-nh' then
    begin
      sOpenOptions+= '/nohistory';
      Continue;
    end;

    if SBeginsWith(SParam, '-z=') then
    begin
      // '-z=text' must give '/view-text'
      sOpenOptions+= '/view-'+Copy(SParam, 4, MaxInt);
      Continue;
    end;

    if SBeginsWith(SParam, '-e=') then
    begin
      sEncoding:= Copy(SParam, 4, MaxInt);
      Continue;
    end;

    SParseFilenameWithTwoNumbers(SParam, NLine, NColumn);

    //if folder, open it in ProjManager
    if DirectoryExistsUTF8(SParam) then
    begin
      DoFolderOpen(SParam, False);
    end
    else
    if FileExistsUTF8(SParam) then
    begin
      Frame:= DoFileOpen(SParam, '', nil, sOpenOptions);
      if Assigned(Frame) then
      begin
        if sEncoding<>'' then
          SetFrameEncoding(Frame, sEncoding, true);

        if (NLine>0) then
          Frame.DoGotoPos(Frame.Ed1, NColumn-1, NLine-1);

        if bReadOnly then
          Frame.ReadOnly[Frame.Ed1]:= true;
      end;
    end;
  end;
end;


{$ifdef windows}
procedure TfmMain.SecondInstance(const Msg: TBytes);
var
  Sep: TATStringSeparator;
  Params: array of string;
  S: string;
begin
  if not IsAllowedToOpenFileNow then Exit;

  SetLength(Params, 0);
  S:= '';

  Sep.Init(UTF8Encode(TEncoding.UTF8.GetString(Msg)), '|');
  while Sep.GetItemStr(S) do
  begin
    SetLength(Params, Length(Params)+1);
    Params[Length(Params)-1]:= S;
  end;

  DoLoadCommandLine_SecondInstance(Params);

  if WindowState = wsMinimized then
  begin
    WindowState:= wsNormal;
    Application.ProcessMessages;
  end;
end;
{$endif}


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
      DoFileOpen(fn, '', nil, cOptionPassive);
  end;

  //if passed single file, must activate it, else not
  if ParamCount<2 then
    SOption:= cOptionActive
  else
    SOption:= cOptionPassive;
  SOption+= FOption_FileOpenOptions;

  //if FOption_FileOpenOptions<>'' then
  //  MsgStdout('CudaText: options "'+FOption_FileOpenOptions+'"');

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
      Frame:= DoFileOpen(fn, '', nil, SOption);
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
        Frame:= DoFileOpen(fn, '', nil, SOption);
    end;

    if Assigned(Frame) then
    begin
      if NumLine>0 then
        Frame.DoGotoPos(Frame.Ed1, NumColumn-1, NumLine-1);

      if FOption_OpenReadOnly then
      begin
        Frame.ReadOnly[Frame.Ed1]:= true;
        MsgStatus(''); //show "[read-only]"
      end;

      if FOption_Encoding<>'' then
      begin
        SetFrameEncoding(Frame, FOption_Encoding, true);
      end;
    end;
  end;
end;


