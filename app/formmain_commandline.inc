(*
This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at http://mozilla.org/MPL/2.0/.

Copyright (c) Alexey Torgashin
*)
{$ifdef nn}begin end;{$endif}

procedure TfmMain.DoLoadCommandLineBaseOptions;
var
  SParam: string;
  i: integer;
begin
  FOption_OpenNewWindow:= false;
  FOption_WindowPos:= '';

  for i:= 1 to ParamCount do
  begin
    SParam:= ParamStr(i);
    if not SBeginsWith(SParam, '-') then
      Continue;

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

    if (SParam='-el') then
    begin
      MsgStdout(AppEncodingListAsString, true);
      Halt;
    end;
  end;
end;

procedure TfmMain.DoLoadCommandParams(const AParams: array of string;
  AOpenOptions: string);
// Function handles passed file names, folder names, and such params as -r -nh -z -e,
// while others params (-w -el -n) are handled in DoLoadCommandLineBaseOptions
var
  Frame: TEditorFrame;
  SParam: string;
  NLine, NColumn, i: integer;
  bReadOnly: boolean;
  sEncoding: string;
begin
  bReadOnly:= false;
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
      AOpenOptions+= '/nohistory';
      Continue;
    end;

    if SBeginsWith(SParam, '-z=') then
    begin
      // '-z=text' must give '/view-text'
      AOpenOptions+= '/view-'+Copy(SParam, 4, MaxInt);
      Continue;
    end;

    if SBeginsWith(SParam, '-e=') then
    begin
      sEncoding:= Copy(SParam, 4, MaxInt);
      Continue;
    end;

    {$ifdef darwin}
    //macOS gives param -psn** for debugger
    if SBeginsWith(SParam, '-psn') then
      Continue;
    {$endif}

    SParseFilenameWithTwoNumbers(SParam, NLine, NColumn);

    //if folder, open it in ProjManager
    if DirectoryExistsUTF8(SParam) then
    begin
      DoFolderOpen(SParam, False);
    end
    else
    if FileExistsUTF8(SParam) then
    begin
      Frame:= DoFileOpen(SParam, '', nil, AOpenOptions);
      if Assigned(Frame) then
      begin
        if sEncoding<>'' then
          SetFrameEncoding(Frame, sEncoding, true);

        if (NLine>0) then
          Frame.DoGotoPos(Frame.Ed1, NColumn-1, NLine-1);

        if bReadOnly then
          Frame.ReadOnly[Frame.Ed1]:= true;
      end;
    end
    else
    //file not exists? suggest to create it
    if MsgBox(Format(msgConfirmCreateNewFile, [SParam]),
              MB_OKCANCEL or MB_ICONQUESTION) = ID_OK then
    begin
      if FCreateFile(SParam) then
        Frame:= DoFileOpen(SParam, '', nil, AOpenOptions);
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

  DoLoadCommandParams(Params, '');

  if WindowState = wsMinimized then
  begin
    WindowState:= wsNormal;
    Application.ProcessMessages;
  end;
end;
{$endif}


procedure TfmMain.UniqInstanceOtherInstance(Sender: TObject;
  ParamCount: Integer; Parameters: array of String);
begin
  if not IsAllowedToOpenFileNow then exit;

  DoLoadCommandParams(Parameters, '');

  if WindowState=wsMinimized then
  begin
    WindowState:= wsNormal;
    Application.ProcessMessages;
  end;

  {$ifdef windows}
  // Those two calls below conflicts with Windows SwitchToThisWindow API call
  // so they are left for other platforms
  {$else}
  Application.BringToFront;
  DoFocusWindow(Handle);
  {$ifend}
end;


procedure TfmMain.DoLoadCommandLine;
const
  cOptionPassive = '/passive /nonear';
  cOptionActive = '/nonear';
var
  Params: array of string;
  Options: string;
  i: integer;
begin
  DoLoadCommandParams(FFileNamesDroppedInitially, cOptionPassive);

  //if passed single file, must activate it, else not
  if ParamCount<2 then
    Options:= cOptionActive
  else
    Options:= cOptionPassive;

  SetLength(Params, ParamCount);
  for i:= 1 to Length(Params) do
    Params[i-1]:= ParamStrUTF8(i);

  DoLoadCommandParams(Params, Options);
end;


