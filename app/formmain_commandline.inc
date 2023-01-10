(*
This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at http://mozilla.org/MPL/2.0/.

Copyright (c) Alexey Torgashin
*)
{$ifdef nn}begin end;{$endif}

procedure InitAdditionalCommandLineOptions(
  out AWindowPos: string;
  out AAllowSessionLoad, AAllowSessionSave: TAppAllowSomething;
  out AStartupCommand: string;
  out AFileFolderCount: integer);
//Option -n is handled via proc_globdata.AppAlwaysNewInstance
var
  SParam: string;
  i: integer;
begin
  AWindowPos:= '';
  AAllowSessionLoad:= aalsEnable;
  AAllowSessionSave:= aalsEnable;
  AStartupCommand:= '';
  AFileFolderCount:= 0;

  for i:= 1 to ParamCount do
  begin
    SParam:= ParamStr(i);
    if not SBeginsWith(SParam, '-') then
    begin
      if AAllowSessionLoad=aalsEnable then
        AAllowSessionLoad:= aalsNotGood; //block loading last session if any filenames present
      Inc(AFileFolderCount);
      Continue;
    end;

    if SBeginsWith(SParam, '-w=') then
    begin
      AWindowPos:= Copy(SParam, 4, MaxInt);
      Continue;
    end;

    if SBeginsWith(SParam, '-c=') then
    begin
      AStartupCommand:= Copy(SParam, 4, MaxInt);
      Continue;
    end;

    if (SParam='-ns') then
    begin
      AAllowSessionLoad:= aalsDisable;
      AAllowSessionSave:= aalsDisable;
      Continue;
    end;

    if (SParam='-nsl') then
    begin
      AAllowSessionLoad:= aalsDisable;
      Continue;
    end;

    if (SParam='-nss') then
    begin
      AAllowSessionSave:= aalsDisable;
      Continue;
    end;

    if (SParam='-verbose') then
    begin
      UiOps.PyOutputCopyToStdout:= true;
      Continue;
    end;
  end;
end;


function _GetStdin: string;
var
  s: string;
begin
  Result:= '';
  Reset(Input);
  while not EOF(Input) do
  begin
    ReadLn(Input, s);
    Result+= s+#10;
  end;
end;

procedure TfmMain.DoLoadCommandParams(const AParams: array of string; AOpenOptions: string);
// Function handles passed file names, folder names, and such params as -r -nh -z -e,
// while others params are handled in InitAdditionalCommandLineOptions
var
  Frame: TEditorFrame;
  SParam: string;
  CliModule: string;
  CliParams: TAppStringArray;
  NLine, NColumn, i: integer;
  bReadOnly: boolean;
  bAllowCreate: boolean;
  bFirstFolder: boolean;
  sEncoding, sCli: string;
  SepCli: TATStringSeparator;
  List: TStringList;
begin
  bReadOnly:= false;
  bAllowCreate:= true;
  bFirstFolder:= true;
  sEncoding:= '';

  for i:= 0 to Length(AParams)-1 do
  begin
    SParam:= AParams[i];
    if SParam='' then
      Continue;

    //remove " quotes, CudaText issue #3726
    SParam:= SParam.DeQuotedString('"');

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

    if SParam='-nn' then
    begin
      bAllowCreate:= false;
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
      sEncoding:= AppEncodingShortnameToFullname( Copy(SParam, 4, MaxInt) );
      Continue;
    end;

    if SBeginsWith(SParam, '-p=') then
    begin
      SepCli.Init(Copy(SParam, 4, MaxInt), '#');
      SepCli.GetItemStr(CliModule);
      CliParams:= nil;
      while SepCli.GetItemStr(sCli) do
      begin
        SetLength(CliParams, Length(CliParams)+1);
        CliParams[High(CliParams)]:= sCli;
      end;
      DoApplyCli(CliModule, CliParams);
      Continue;
    end;

    {$ifdef unix}
    if SParam='-i' then
    begin
      Frame:= Frames[0];
      if not Frame.IsEmpty then
        Frame:= DoFileOpen('', '', nil, '/nohistory');
      if Assigned(Frame) then
      begin
        Frame.TabCaption:= 'stdin';
        Frame.TabCaptionReason:= tcrUnsavedSpecial;
        Frame.Ed1.Strings.LoadFromString(_GetStdin);
        //Frame.Ed1.Modified:= false;
      end;
      Continue;
    end;
    {$endif}

    (*
    {$ifdef darwin}
    //macOS gives param -psn** for debugger
    if SBeginsWith(SParam, '-psn') then
      Continue;
    {$endif}
    *)

    //skip all other keys, to not see them as filenames
    if SBeginsWith(SParam, '-') then
      Continue;

    SParam:= AppExpandFilename(SParam);

    //here we assume that SParam is filename/dirname/filemask
    if not IsOsFullPath(SParam) then
      SParam:= GetCurrentDirUTF8+DirectorySeparator+SParam;

    SParseFilenameWithTwoNumbers(SParam, NLine, NColumn);

    //if folder, open it in ProjManager
    if DirectoryExistsUTF8(SParam) then
    begin
      Application.ProcessMessages; //fix issue #3928

      DoFolderOpen(SParam, bFirstFolder, cInvokeAppInternal);
      bFirstFolder:= false;
    end
    else
    if FileExists(SParam) then
    begin
      Application.ProcessMessages; //fix issue #3928

      Frame:= DoFileOpen(SParam, '', nil, AOpenOptions);
      if Assigned(Frame) then
      begin
        if sEncoding<>'' then
          SetFrameEncoding(Frame.Ed1, sEncoding, true);

        if (NLine>0) then
          Frame.DoGotoPos(Frame.Ed1, NColumn-1, NLine-1);

        if bReadOnly then
          Frame.ReadOnly[Frame.Ed1]:= true;
      end;
    end
    else
    //file mask with '*' char?
    if Pos('*', SParam)>0 then
    begin
      List:= TStringList.Create;
      try
        AppFindFilesByMask(List, SParam);
        for SParam in List do
        begin
          Frame:= DoFileOpen(SParam, '', nil, AOpenOptions);
          if Assigned(Frame) then
          begin
            if sEncoding<>'' then
              SetFrameEncoding(Frame.Ed1, sEncoding, true);

            if bReadOnly then
              Frame.ReadOnly[Frame.Ed1]:= true;
          end;
        end;
      finally
        FreeAndNil(List);
      end;
    end
    else
    //file not exists? suggest to create it
    if bAllowCreate and AppFileExtentionCreatable(SParam) then
      if MsgBox(Format(msgConfirmCreateNewFile, [SParam]),
              MB_OKCANCEL or MB_ICONQUESTION) = ID_OK then
        if AppCreateFile(SParam) then
        begin
          Frame:= DoFileOpen(SParam, '', nil, AOpenOptions);
          if Assigned(Frame) then
          begin
            if sEncoding<>'' then
              SetFrameEncoding(Frame.Ed1, sEncoding, true);
          end;
        end;
  end;

  //if session file was opened, we must update caption
  UpdateCaption;
end;


{$ifdef windows}
procedure TfmMain.SecondInstance(const Msg: TBytes);
var
  Sep: TATStringSeparator;
  Params: array of string;
  S: string;
begin
  if not IsAllowedToOpenFileNow then Exit;

  Params:= nil;
  S:= '';

  Sep.Init(UTF8Encode(TEncoding.UTF8.GetString(Msg)), '|');
  while Sep.GetItemStr(S) do
  begin
    SetLength(Params, Length(Params)+1);
    Params[Length(Params)-1]:= S;
  end;

  DoLoadCommandParams(
    Params,
    GetFileOpenOptionsString(Length(Params))
    );

  if WindowState = wsMinimized then
  begin
    Application.Restore;
    Application.ProcessMessages;
  end;
end;
{$endif}

function TfmMain.GetFileOpenOptionsString(AFileCount: integer): string;
begin
  if AFileCount>1 then
    Result:= '/passive /nonear'
  else
    Result:= '/nonear';
end;

procedure TfmMain.HandleOtherInstance(Sender: TObject;
  ParamCount: Integer; const Parameters: array of String);
begin
  if not IsAllowedToOpenFileNow then exit;

  DoLoadCommandParams(
    Parameters,
    GetFileOpenOptionsString(ParamCount)
    );

  if WindowState=wsMinimized then
  begin
    Application.Restore;
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
var
  Params: array of string;
  i: integer;
begin
  DoLoadCommandParams(
    AppDroppedFiles,
    GetFileOpenOptionsString(100) //open with /passive flag
    );

  Params:= nil;
  SetLength(Params, ParamCount);
  for i:= 1 to Length(Params) do
    Params[i-1]:= ParamStrUTF8(i);

  DoLoadCommandParams(
    Params,
    GetFileOpenOptionsString(Length(Params))
    );
end;

procedure TfmMain.DoLoadCommandLine_FromString(const AText: string);
var
  StrArray: TStringArray;
begin
  if Trim(AText)='' then exit;
  StrArray:= AText.Split(' ', '"', '"', TStringSplitOptions.ExcludeEmpty);
  DoLoadCommandParams(StrArray, '');
end;

