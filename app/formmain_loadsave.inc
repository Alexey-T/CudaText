(*
This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at http://mozilla.org/MPL/2.0/.

Copyright (c) Alexey Torgashin
*)
{$ifdef nn}begin end;{$endif}

procedure TfmMain.DoOps_SaveOptionBool(const APath: string; AValue: boolean);
var
  cfg: TJSONConfig;
begin
  cfg:= TJSONConfig.Create(nil);
  try
    try
      cfg.Formatted:= true; //needs last at__jsonconf.pp
      cfg.Filename:= AppFile_OptionsUser;
    except
      exit;
    end;

    cfg.SetValue(APath, AValue);
  finally
    cfg.Free;
  end;
end;

procedure TfmMain.DoOps_SaveOptionString(const APath, AValue: string);
var
  cfg: TJSONConfig;
begin
  cfg:= TJSONConfig.Create(nil);
  try
    try
      cfg.Formatted:= true; //needs last at__jsonconf.pp
      cfg.Filename:= AppFile_OptionsUser;
    except
      exit;
    end;

    cfg.SetValue(APath, AValue);
  finally
    cfg.Free;
  end;
end;


procedure TfmMain.DoOps_SaveThemes;
var
  cfg: TJSONConfig;
begin
  cfg:= TJSONConfig.Create(nil);
  try
    try
      cfg.Formatted:= true;
      cfg.FileName:= AppFile_OptionsUser;
    except
      exit
    end;
    cfg.SetValue('/ui_theme', UiOps.ThemeUi);
    cfg.SetValue('/ui_theme_syntax', UiOps.ThemeSyntax);
  finally
    cfg.Free;
  end;
end;

procedure TfmMain.DoOps_ClearConfigHistory(AMode: TAppConfigHistoryElements);
var
  cfg: TJSONConfig;
begin
  cfg:= TJSONConfig.Create(nil);
  try
    try
      cfg.Formatted:= true; //needs last at__jsonconf.pp
      cfg.Filename:= AppFile_History;
    except
      exit;
    end;

    if acheRecentFiles in AMode then
    begin
      cfg.DeleteValue('/recent_files');
      //
      AppListRecents.Clear;
      UpdateMenuRecent(nil);
      //
      DeleteFileUTF8(AppFile_HistoryFiles);
    end;

    if acheSearch in AMode then
    begin
      cfg.DeleteValue('/list_find');
      cfg.DeleteValue('/list_replace');
      if Assigned(fmFind) then
      begin
        fmFind.edFind.Items.Clear;
        fmFind.edRep.Items.Clear;
      end;
    end;

    if acheConsole in AMode then
    begin
      cfg.DeleteValue('/list_console');
      if Assigned(fmConsole) then
        fmConsole.EdInput.Items.Clear;
    end;
  finally
    cfg.Free;
  end;
end;

procedure TfmMain.DoOps_SaveHistory(ASaveModifiedTabs: boolean);
var
  cfg: TJSONConfig;
  bMax: boolean;
  fnBase: string;
begin
  AppDiskCheckFreeSpace(AppFile_History);

  cfg:= TJSONConfig.Create(nil);
  try
    try
      cfg.Formatted:= true; //needs last at__jsonconf.pp
      cfg.Filename:= AppFile_History;
    except
      exit;
    end;

    ////form
    if not ShowFullscreen then
    begin
      bMax:= WindowState=wsMaximized;
      cfg.SetValue('/pos/main_max', bMax);
      if bMax then
        cfg.SetValue('/pos/main_mon', Monitor.MonitorNum);

      if not IsWindowMaximizedOrFullscreen then
        cfg.SetValue('/pos/main', FormPosGetAsString(Self, false));

      if AppPanels[cPaneSide].Floating then
        cfg.SetValue('/pos/side', FormPosGetAsString(AppPanels[cPaneSide].FormFloat, false));

      if AppPanels[cPaneOut].Floating then
        cfg.SetValue('/pos/bottom', FormPosGetAsString(AppPanels[cPaneOut].FormFloat, false));

      if FloatGroups then
      begin
        cfg.SetValue('/pos/g1', FormPosGetAsString(FFormFloatGroups1, false));
        cfg.SetValue('/pos/g2', FormPosGetAsString(FFormFloatGroups2, false));
        cfg.SetValue('/pos/g3', FormPosGetAsString(FFormFloatGroups3, false));
      end;

      cfg.SetValue('show_sidepanel', AppPanels[cPaneSide].Visible);
      cfg.SetValue('show_bottom', AppPanels[cPaneOut].Visible);

      cfg.SetValue('float_side', AppPanels[cPaneSide].Floating);
      cfg.SetValue('float_bottom', AppPanels[cPaneOut].Floating);

      with AppPanels[cPaneSide] do
        if not Floating then
          cfg.SetValue('size_side', PanelSize);

      with AppPanels[cPaneOut] do
        if not Floating then
          cfg.SetValue('size_bottom', PanelSize);

      cfg.SetValue('tab_left', AppPanels[cPaneSide].LastActivePanel);
      cfg.SetValue('tab_bottom', AppPanels[cPaneOut].LastActivePanel);
    end;

    ////recents
    cfg.SetValue('/recent_files', AppListRecents);

    ////find dialog
    if Assigned(fmFind) then
    begin
      cfg.SetValue('/list_find', fmFind.edFind.Items);
      cfg.SetValue('/list_replace', fmFind.edRep.Items);
      if fmFind.IsNarrow then
        cfg.SetValue('/pos/find', FormPosGetAsString(fmFind, false));
      cfg.SetValue('/finder/mline', fmFind.IsMultiLine);
      cfg.SetValue('/finder/hi', fmFind.IsHiAll);
      cfg.SetValue('/finder/regex_subst', fmFind.chkRegexSubst.Checked);
    end;

    if Assigned(fmConsole) then
      cfg.SetValue('/list_console', fmConsole.EdInput.Items);

    ////finder
    cfg.SetValue('/finder/regex', FFinder.OptRegex);
    cfg.SetValue('/finder/case', FFinder.OptCase);
    cfg.SetValue('/finder/words', FFinder.OptWords);
    cfg.SetValue('/finder/back', FFinder.OptBack);
    cfg.SetValue('/finder/confirm', FFinder.OptConfirmReplace);
    cfg.SetValue('/finder/wrap', FFinder.OptWrapped);
    cfg.SetValue('/finder/insel', FFinder.OptInSelection);
    cfg.SetValue('/finder/tokens', Ord(FFinder.OptTokens));
    cfg.SetValue('/finder/prescase', FFinder.OptPreserveCase);

    ////state
    cfg.SetValue('lang', UiOps.LangName);
    cfg.SetValue('session', AppSessionName_ForHistoryFile);
    cfg.SetValue('last_cmd', FLastSelectedCommand);

    DoOps_SaveHistory_GroupView(cfg, '');
  finally
    cfg.Free;
  end;

  if UiOps.ReopenSession and
    ((FOption_AllowSessionSave=aalsEnable) or not IsDefaultSessionActive)
    //// see http://synwrite.sourceforge.net/forums/viewtopic.php?f=20&t=2576
    //// @tmsg wants to save session always, even with -ns cmdline opt
  then
  begin
    fnBase:= AppFile_Session;
    DoOps_SaveSessionsBackups(fnBase);
    DoOps_SaveSession(fnBase, ASaveModifiedTabs, ASaveModifiedTabs, false);
  end;
end;


procedure TfmMain.DoOps_SaveSessionsBackups(const ASessionFilename: string);
//keep backups of last N unnamed sessions as 'history session.N.cuda-session'
begin
  AppMakeBackupFiles(ASessionFilename, '.cuda-session', UiOps.BackupLastSessions);
end;

function TfmMain.DoOps_SaveSession(const ASessionId: string;
  ASaveModifiedFiles, ASaveUntitledTabs, AByTimer: boolean): boolean;
const
  keysToIgnore: array of UTF8String = ('text', 'undo', 'redo');
var
  cfg: TJSONConfigEx;
  Frame: TEditorFrame;
  Gr: TATGroups;
  Pages: TATPages;
  sSessionFileName, sTabFilename, sTabFilename2, sRootPath: string;
  nLocalGroup, nGlobalGroup, nTabInGroup: integer;
  nCountFiles, nCountUntitled, nSavedTabIndex: integer;
  nFrameCount, nPrevFrameCount: integer;
  path, sTabKind, sConsole, sUndo: string;
  bModified, bUntitled: boolean;
  bSessionUpdated: boolean;
  iFrame, iGroup: integer;
  currFramesEditState: array of TFrameEditState;
  fileWriteTimeBeforeSaving, fileWriteTimeAfterSaving: TDateTime;
begin
  Result:= false;
  if ASessionId='' then exit;

  SSplitByChar(ASessionId, '|', sSessionFileName, sRootPath);

  {
  // this code breaks the save-only-updated-session feature
  if sRootPath='' then
  begin
    AppDiskCheckFreeSpace(sSessionFileName);
    DeleteFile(sSessionFileName);
  end;
  }

  bSessionUpdated:= false;
  nCountFiles:= 0;
  nCountUntitled:= 0;
  nFrameCount:= FrameCount;

  cfg:= TJSONConfigEx.Create(nil);
  try
    try
      cfg.Formatted:= true;
      cfg.Filename:= sSessionFileName;
      if sRootPath<>'' then
        cfg.DeletePath(sRootPath); //otherwise sections '000' will not be removed
    except
      on E: Exception do
      begin
        MsgBadConfig(sSessionFileName, E.Message);
        exit;
      end;
    end;

    if not FileExists(sSessionFileName) then
      bSessionUpdated:= true; // the session file has been physically deleted

    if FPrevJsonObj=nil then
      FPrevJsonObj:= CloneJsonObj(cfg.GetJsonObj, keysToIgnore);

    DoOps_SaveHistory_GroupView(cfg, sRootPath);

    for iGroup:= 0 to cAppMaxGroup do
    begin
      Pages:= TGroupsHelper.GetPagesOfGroupIndex(iGroup);
      if Pages=nil then Continue;
      cfg.SetValue(sRootPath+'/tab_index/g'+IntToStr(iGroup), Pages.Tabs.TabIndex);
    end;
    cfg.SetValue(sRootPath+'/tab_index/g_active', Groups.FindPages(Groups.PagesCurrent));

    cfg.SetValue(sRootPath+'/panels/side_show', AppPanels[cPaneSide].Visible);
    cfg.SetValue(sRootPath+'/panels/side_panel', AppPanels[cPaneSide].LastActivePanel);

    with AppPanels[cPaneSide] do
      if not Floating then
        cfg.SetValue(sRootPath+'/panels/side_size', PanelSize);

    cfg.SetValue(sRootPath+'/panels/bottom_show', AppPanels[cPaneOut].Visible);
    cfg.SetValue(sRootPath+'/panels/bottom_panel', AppPanels[cPaneOut].LastActivePanel);

    with AppPanels[cPaneOut] do
      if not Floating then
        cfg.SetValue(sRootPath+'/panels/bottom_size', PanelSize);

    SetLength(currFramesEditState, nFrameCount);
    nPrevFrameCount:= Length(FPrevFramesEditState);
    if nFrameCount<>nPrevFrameCount then
    begin
      bSessionUpdated:= true; //number of opened documents has changed

      //cleanup session keys, left from the previous saving
      for iFrame:= nFrameCount to nPrevFrameCount-1 do
      begin
        path:= sRootPath+'/'+Format('%3.3d', [iFrame]);
        cfg.DeleteValue(path);
      end;
    end;

    nSavedTabIndex:= 0;
    for iFrame:= 0 to nFrameCount-1 do
    begin
      Frame:= Frames[iFrame];
      if Frame.IsEmpty then Continue;
      if Frame.TabCaption=msgWelcomeTabTitle then Continue; //avoid reopening of '(welcome)' filler text

      sTabFilename:= Frame.GetFileName(Frame.Ed1);
      if Frame.EditorsLinked then
        sTabFilename2:= ''
      else
        sTabFilename2:= Frame.GetFileName(Frame.Ed2);

      if AppFile_IsIgnoredForSession(sTabFilename) then
      begin
        MsgLogConsole(Format('NOTE: File "%s" is not saved to session', [sTabFilename]));
        Continue;
      end;

      bUntitled:= sTabFilename='';
      if bUntitled then
        Inc(nCountUntitled)
      else
        Inc(nCountFiles);

      path:= sRootPath+'/'+Format('%3.3d', [nSavedTabIndex]);
      Inc(nSavedTabIndex);

      GetFrameLocation(Frame, Gr, Pages, nLocalGroup, nGlobalGroup, nTabInGroup);
      bModified:= Frame.Ed1.Modified or (bUntitled and not Frame.IsEmpty);

      currFramesEditState[iFrame].Assign(Frame);

      if not bSessionUpdated then
      begin
        if currFramesEditState[iFrame]<>FPrevFramesEditState[iFrame] then
          bSessionUpdated:= true; // frame's FileName or unsaved text has been updated
      end;

      cfg.SetValue(path+'/modified', bModified);
      if not bModified then
        cfg.DeleteValue(path+'/text');
      cfg.SetValue(path+'/file', AppCollapseHomeDirInFilename(sTabFilename));

      if bModified then
        cfg.SetValue(path+'/tab_title', Frame.TabCaption);
      cfg.SetValue(path+'/group', nLocalGroup+1); //support old sessions with 1-based index
      cfg.SetDeleteValue(path+'/pinned', Frame.TabPinned, false);

      case Frame.FrameKind of
        efkEditor:
          sTabKind:= '';
        efkBinaryViewer:
          sTabKind:= 'bin,'+IntToStr(Ord(Frame.Binary.Mode));
        efkImageViewer:
          sTabKind:= 'pic';
      end;
      cfg.SetValue(path+'/kind', sTabKind);

      {
      //TODO: add together with restoring of 'v_pos'
      if Frame.IsBinary then
        cfg.SetValue(path+'/v_pos', Frame.Binary.PosOffset)
      else
        cfg.DeleteValue(path+'/v_pos');
        }

      if bModified and UiOps.HistoryItems[ahhText] then
        if (not bUntitled and ASaveModifiedFiles) or
          (bUntitled and ASaveUntitledTabs) then
          if not Frame.Ed1.Strings.IsSizeBig(UiOps.MaxSizeForSession) then
        begin
          cfg.SetValue(path+'/text', Frame.Ed1.Text);
          if (not bUntitled) and FileExists(sTabFilename) then
            cfg.SetValue(path+'/file_date', FormatFileDateAsNiceString(sTabFilename));
          Frame.VersionInSession:= Frame.Ed1.Strings.ModifiedVersion;

          if UiOps.HistoryItems[ahhUndoRedo] then
          begin
            sUndo:= Frame.Ed1.UndoAsString;
            if Length(sUndo)<=UiOps.MaxUndoSizeForSessionFile then
              cfg.SetValue(path+'/undo', sUndo);
            sUndo:= Frame.Ed1.RedoAsString;
            if Length(sUndo)<=UiOps.MaxUndoSizeForSessionFile then
              cfg.SetValue(path+'/redo', sUndo);
          end;
        end;

      Frame.DoSaveHistoryEx(Frame.Ed1, cfg, path, true{AForSession});

      //save info about paired-tab to subkey "pair"
      if sTabFilename2<>'' then
      begin
        path+= '/pair';

        bModified:= Frame.Ed2.Modified;
        cfg.SetValue(path+'/modified', bModified);
        if not bModified then
          cfg.DeleteValue(path+'/text');
        cfg.SetValue(path+'/file', AppCollapseHomeDirInFilename(sTabFilename2));

        if bModified and UiOps.HistoryItems[ahhText] then
          if not Frame.Ed2.Strings.IsSizeBig(UiOps.MaxSizeForSession) then
        begin
          cfg.SetValue(path+'/text', Frame.Ed2.Text);
          if FileExists(sTabFilename2) then
            cfg.SetValue(path+'/file_date', FormatFileDateAsNiceString(sTabFilename2));
          if UiOps.HistoryItems[ahhUndoRedo] then
          begin
            sUndo:= Frame.Ed2.UndoAsString;
            if Length(sUndo)<=UiOps.MaxUndoSizeForSessionFile then
              cfg.SetValue(path+'/undo', sUndo);
            sUndo:= Frame.Ed2.RedoAsString;
            if Length(sUndo)<=UiOps.MaxUndoSizeForSessionFile then
              cfg.SetValue(path+'/redo', sUndo);
          end;
        end;

        Frame.DoSaveHistoryEx(Frame.Ed2, cfg, path, true{AForSession});
      end;
    end;

    FPrevFramesEditState:= Copy(currFramesEditState, 0, Length(currFramesEditState));

    Result:= true;
  finally
    try
      if not bSessionUpdated then
      begin
        if not IsJsonObjEqual(cfg.GetJsonObj, FPrevJsonObj, keysToIgnore) then
          bSessionUpdated:= true; // the JSON config has been updated
      end;

      if bSessionUpdated then
      begin
        FPrevJsonObj.Free;
        FPrevJsonObj:= CloneJsonObj(cfg.GetJsonObj, keysToIgnore);
        if not FileAge(sSessionFileName, fileWriteTimeBeforeSaving) then
          fileWriteTimeBeforeSaving:= Default(TDateTime);
      end
      else
        cfg.SetModified(false); // no need to save the session

      cfg.Free; //saves json file when its property 'Modified' is 'true'
    except
      //for incorrect sess folder
    end;
  end;

  if bSessionUpdated then
  begin
    if not FileAge(sSessionFileName, fileWriteTimeAfterSaving) then
      fileWriteTimeAfterSaving:= Default(TDateTime);
    if fileWriteTimeBeforeSaving=fileWriteTimeAfterSaving then
    begin
      FPrevJsonObj.Free;
      FPrevJsonObj:= TJSONNull.Create; // the session has not been saved!!!
    end;
  end;

  if UiOps.LogSessions and bSessionUpdated then
  begin
    if fileWriteTimeBeforeSaving<>fileWriteTimeAfterSaving then
      sConsole:= Format('Saved session: "%s", %d file(s)', [ExtractFileName(sSessionFileName), nCountFiles])
    else
      sConsole:= Format('Failed to save session: "%s", %d file(s)', [ExtractFileName(sSessionFileName), nCountFiles]);
    if nCountUntitled>0 then
      sConsole+= Format(' + %d untitled', [nCountUntitled]);
    if AByTimer then
      sConsole+= Format(', by timer at %s', [TimeToStr(Now)]);
    MsgLogConsole(sConsole);
  end;
end;

function IsHistorySectionForFilename(const s: string): boolean;
begin
  Result:= StrToIntDef(s, -1)>=0;
end;

function TfmMain.DoOps_LoadSession(const ASessionId: string; AllowShowPanels: boolean): boolean;
var
  cfg: TJSONConfig;
  Frame: TEditorFrame;
  Ed: TATSynEdit;
  Pages: TATPages;
  ListSessionKeys: TStringList;
  sSessionFileName, sTabFilename, sTabFilename2, sRootPath,
  sTabTitle, sTabKind, sOpenOptions,
  item_path, item_path2: string;
  nCountFiles, nCountFilesModified, nCountFilesBefore: integer;
  bModified, bModified2: boolean;
  bAllowOnState: boolean;
  sText: UnicodeString;
  sUndoString, sRedoString: string;
  bExist, bExist2: boolean;
  sDateSaved, sDateCurrent, sConsole: string;
  CurTabs: TATTabs;
  nTick: QWord;
  nGroupIndex, nTabInGroup: integer;
  iList, iGroup, iFrame: integer;
const
  SOptionPassive = '/passive /nonear';
  SOptionNoHistory = '/nohistory /nolexerdetect /noevent /noopenedevent /nononeevent';
begin
  Result:= false;
  if ASessionId='' then exit;

  ////don't do it, it will skip startup session loading
  //if ASessionId=AppSessionName then exit;

  SSplitByChar(ASessionId, '|', sSessionFileName, sRootPath);
  if not FileExists(sSessionFileName) then exit;

  //fix issue #3734
  if not IsDefaultSessionActive then
  begin
    if not SameFileName(ASessionId, AppFile_Session) then //fix issue #3744
      DoOps_SaveSession(AppFile_Session, false, false, false);
  end;

  nCountFilesBefore:= FrameCount;
  if nCountFilesBefore=1 then
    if Frames[0].IsEmpty then
      nCountFilesBefore:= 0;

  AppSessionIsClosing:= true;
  try
    if not DoFileCloseAll(true, true) then
    begin
      DoPyEvent_AppState(APPSTATE_SESSION_LOAD_BEGIN);
      DoPyEvent_AppState(APPSTATE_SESSION_LOAD_FAIL);
      exit;
    end;
  finally
    AppSessionIsClosing:= false;
  end;

  nTick:= GetTickCount64;
  nCountFiles:= 0;
  nCountFilesModified:= 0;

  AppFileCheckForNullBytes(sSessionFileName);

  cfg:= TJSONConfig.Create(nil);
  ListSessionKeys:= TStringList.Create;
  try
    try
      cfg.Filename:= sSessionFileName;
    except
      on E: Exception do
      begin
        MsgBadConfig(sSessionFileName, E.Message);
        DoPyEvent_AppState(APPSTATE_SESSION_LOAD_BEGIN);
        DoPyEvent_AppState(APPSTATE_SESSION_LOAD_FAIL);
        exit;
      end;
    end;

    //avoid firing API on_state for empty session
    bAllowOnState:= true;
    if nCountFilesBefore=0 then
      if (cfg.GetValue(sRootPath+'/000/group', -1)=-1) and
        (cfg.GetValue(sRootPath+'/001/group', -1)=-1) then
        bAllowOnState:= false;

    if bAllowOnState then
      DoPyEvent_AppState(APPSTATE_SESSION_LOAD_BEGIN);

    AppSessionIsLoading:= true;
    DoOps_LoadHistory_GroupView(cfg, sRootPath); //reads to variables
    DoApplyInitialGroupSizes; //applies that variables

    cfg.EnumSubKeys(sRootPath+'/', ListSessionKeys);
    ListSessionKeys.Sort;

    for iList:= 0 to ListSessionKeys.Count-1 do
    begin
      //handle only sections like "000".."001"..etc, ignore sections like "panels", "gr"
      item_path:= ListSessionKeys[iList];
      if not IsHistorySectionForFilename(item_path) then Continue;
      item_path:= sRootPath+'/'+item_path+'/';
      item_path2:= item_path+'pair/';

      sTabFilename:= AppExpandHomeDirInFilename(cfg.GetValue(item_path+'file', ''));
      sTabFilename2:= AppExpandHomeDirInFilename(cfg.GetValue(item_path2+'file', ''));

      bExist:= (sTabFilename<>'') and FileExists(sTabFilename);
      bExist2:= (sTabFilename2<>'') and FileExists(sTabFilename2);

      if (sTabFilename<>'') and not bExist then
      begin
        MsgFileFromSessionNotFound(sTabFilename);
      end;

      if (sTabFilename2<>'') and not bExist2 then
      begin
        MsgFileFromSessionNotFound(sTabFilename2);
        sTabFilename2:= '';
      end;

      sText:= cfg.GetValue(item_path+'text', '');
      sUndoString:= cfg.GetValue(item_path+'undo', '');
      sRedoString:= cfg.GetValue(item_path+'redo', '');
      sTabKind:= cfg.GetValue(item_path+'kind', '');
      bModified:= cfg.GetValue(item_path+'modified', false);
      if sText='' then //don't load this file from session
        bModified:= false;

      //support old sessions with 1-based group index
      nGroupIndex:= cfg.GetValue(item_path+'group', 1)-1;

      //check, if text saved in session, but file was changed outside
      if bModified and bExist then
      begin
        sDateSaved:= cfg.GetValue(item_path+'file_date', '');
        sDateCurrent:= FormatFileDateAsNiceString(sTabFilename);
        if sDateSaved<>sDateCurrent then
          if MsgBox(
            msgConfirmFileChangedOutside+#10+sTabFilename+#10#10+
            msgConfirmReloadIt+#10+
            msgConfirmReloadItHotkeysSess,
            MB_YESNO+MB_ICONQUESTION)=ID_YES then
           bModified:= false;
      end;

      if (nGroupIndex<0) or (nGroupIndex>High(TATGroupsNums)) then
        nGroupIndex:= 0;
      Pages:= Groups.Pages[nGroupIndex];

      if SBeginsWith(sTabKind, 'bin') then
      begin
        case StrToIntDef(sTabKind[Length(sTabKind)], -1) of
          0: sOpenOptions:= '/view-text';
          1: sOpenOptions:= '/view-binary';
          2: sOpenOptions:= '/view-hex';
          3: sOpenOptions:= '/view-unicode';
          4: sOpenOptions:= '/view-uhex';
          else sOpenOptions:= '/view-binary';
        end;
      end
      else
        sOpenOptions:= '';

      Frame:= nil;
      //modified tabs must restore text, and set filename (if not empty)
      if bModified then
      begin
        Inc(nCountFilesModified);
        if (FrameCount>0) and Frames[0].IsEmpty then
          Frame:= Frames[0]
        else
          Frame:= DoFileOpen('', '', Pages, SOptionPassive+SOptionNoHistory);
        if sTabFilename<>'' then
        begin
          Frame.SetFileName(Frame.Ed1, sTabFilename);
          Frame.TabCaption:= ExtractFileName(sTabFilename);
          Frame.TabCaptionReason:= tcrFromFilename;
        end;
        if sText<>'' then
          Frame.Editor.Text:= sText;
        if sUndoString<>'' then
          Frame.Editor.UndoAsString:= sUndoString;
        if sRedoString<>'' then
          Frame.Editor.RedoAsString:= sRedoString;
        Frame.Editor.Modified:= bModified;
        sTabTitle:= cfg.GetValue(item_path+'tab_title', '');
        if sTabTitle<>'' then
          Frame.TabCaption:= sTabTitle;
        if sTabFilename<>'' then
          Frame.TabCaptionReason:= tcrFromFilename
        else
          Frame.TabCaptionReason:= tcrUnsaved;
        Frame.UpdateModified(Frame.Editor);
      end
      else
      //unmodified+named tabs must load file again
      if bExist then
      begin
        Inc(nCountFiles);
        Frame:= DoFileOpen(sTabFilename, sTabFilename2, Pages, sOpenOptions+SOptionPassive+SOptionNoHistory);
      end;

      if Assigned(Frame) then
      begin
        Frame.InSession:= true;
        Frame.DoLoadHistoryEx(Frame.Ed1, cfg, item_path, true);

        if sTabFilename2<>'' then
        begin
          sText:= cfg.GetValue(item_path2+'text', '');
          sUndoString:= cfg.GetValue(item_path2+'undo', '');
          sRedoString:= cfg.GetValue(item_path2+'redo', '');
          bModified2:= cfg.GetValue(item_path2+'modified', false);
          if sText='' then
            bModified2:= false;

          if bModified2 then
          begin
            Inc(nCountFilesModified);
            if sText<>'' then
              Frame.Ed2.Text:= sText;
            if sUndoString<>'' then
              Frame.Ed2.UndoAsString:= sUndoString;
            if sRedoString<>'' then
              Frame.Ed2.RedoAsString:= sRedoString;
            Frame.Ed2.Modified:= bModified2;
            Frame.UpdateModified(Frame.Ed2);
          end
          else
          if bExist2 then
            Inc(nCountFiles);

          Frame.DoLoadHistoryEx(Frame.Ed2, cfg, item_path2, true);
        end;

        Frame.TabPinned:= cfg.GetValue(item_path+'pinned', false);

        //event on_open was blocked in DoFileOpen, call it here
        //(plugin "Tab Icons" needs it)
        if (sTabFilename<>'') or Assigned(Frame.Ed1.AdapterForHilite) then
          DoPyEvent_Open(Frame.Ed1);

        if sTabFilename2<>'' then
          DoPyEvent_Open(Frame.Ed2);
      end;
    end;

    //restore tab-index in all groups
    for iGroup:= Low(TATGroupsNums) to High(TATGroupsNums) do
    begin
      CurTabs:= Groups.Pages[iGroup].Tabs;
      nTabInGroup:= cfg.GetValue(sRootPath+'/tab_index/g'+IntToStr(iGroup), -1);
      nTabInGroup:= Max(0, Min(nTabInGroup, CurTabs.TabCount-1));
      //don't check that index is different, force set index; this fixes e.g. bug #3892
      if CurTabs.TabCount>0 then
        CurTabs.TabIndex:= nTabInGroup;
    end;

    //restore group-index
    nGroupIndex:= cfg.GetValue(sRootPath+'/tab_index/g_active', Low(TATGroupsNums));
    if (nGroupIndex>=Low(TATGroupsNums)) and (nGroupIndex<=High(TATGroupsNums)) then
    begin
      Pages:= Groups.Pages[nGroupIndex];
      if Pages.Tabs.TabCount>0 then
        if Groups.PagesCurrent<>Pages then
        begin
          Groups.PagesCurrent:= Pages;
          Pages.Tabs.TabIndex:= Pages.Tabs.TabIndex;
        end;
    end;

    //clear tick values, to CudaExt tab switcher feels good
    //(it shows frames with tick=0 in second listbox)
    for iFrame:= 0 to FrameCount-1 do
    begin
      Frame:= Frames[iFrame];
      if Frame.Visible then
        Frame.ActivationTime:= GetTickCount64
      else
        Frame.ActivationTime:= 0;
    end;

    //restore panel sizes
    if AllowShowPanels then
    begin
      AppPanels[cPaneSide].Visible:= cfg.GetValue(sRootPath+'/panels/side_show', AppPanels[cPaneSide].Visible);
      AppPanels[cPaneOut].Visible:= cfg.GetValue(sRootPath+'/panels/bottom_show', AppPanels[cPaneOut].Visible);
    end;

    with AppPanels[cPaneSide] do
      PanelSize:= Min(cfg.GetValue(sRootPath+'/panels/side_size', PanelSize), ClientWidth div 2);

    with AppPanels[cPaneOut] do
      PanelSize:= Min(cfg.GetValue(sRootPath+'/panels/bottom_size', PanelSize), ClientHeight div 2);

    SText:= cfg.GetValue(sRootPath+'/panels/side_panel', '');
    if AppPanels[cPaneSide].Visible and (SText<>'') then
      AppPanels[cPaneSide].UpdatePanels(SText, false, true);

    SText:= cfg.GetValue(sRootPath+'/panels/bottom_panel', '');
    if AppPanels[cPaneOut].Visible and (SText<>'') then
      AppPanels[cPaneOut].UpdatePanels(SText, false, true);

    Result:= true;
  finally
    AppSessionIsLoading:= false;
    ListSessionKeys.Free;
    cfg.Free;
  end;

  if UiOps.LogSessions then
  begin
    nTick:= (GetTickCount64-nTick) div 10 * 10;
    sConsole:= Format('Loaded session: "%s", %dms, %d file(s)', [ExtractFileName(sSessionFileName), nTick, nCountFiles]);
    if nCountFilesModified>0 then
      sConsole+= Format(' + %d modified', [nCountFilesModified]);
    MsgLogConsole(sConsole);
  end;

  //don't wait until plugin sets session name in PROC_SET_SESSION,
  //it's needed in on_state
  AppSessionName:= ASessionId;

  if bAllowOnState then
  begin
    DoPyEvent_AppState(APPSTATE_SESSION_LOAD);
    {
    if (nCountFilesBefore>0) or (nCountFiles+nCountFilesModified>0) then
      DoPyEvent_AppState(APPSTATE_SESSION_LOAD)
    else
      DoPyEvent_AppState(APPSTATE_SESSION_LOAD_FAIL);
    }
  end;

  //avoid getting N tabs for untitled of which only N-1 are translated
  DoLocalizeTabTitles;

  //fix disabled parsing for passive tab-groups (issue #4995)
  DoShowForVisibleFrames;

  Frame:= CurrentFrame;
  if Assigned(Frame) then
  begin
    DoOnTabFocusFinalization(Frame, nCountFiles+nCountFilesModified>0);

    //python 'on_caret' is disabled during session loading, so fire it here
    Ed:= Frame.Ed1;
    if not EditorIsEmpty(Ed) and not EditorCaretIsOnStart(Ed) then
      Ed.DoEventCarets;
  end;

  SetLength(FPrevFramesEditState, FrameCount);
  for iFrame:= 0 to Length(FPrevFramesEditState)-1 do
  begin
    Frame:= Frames[iFrame];
    FPrevFramesEditState[iFrame].Assign(Frame);
  end;
end;

procedure TfmMain.DoOps_LoadHistory_GroupView(cfg: TJsonConfig; const AJsonPath: string);
var
  Sep: TATStringSeparator;
  i: integer;
begin
  FOption_GroupMode:= TATGroupsMode(cfg.GetValue(AJsonPath+'/groups', Ord(gmOne)));

  Sep.Init(cfg.GetValue(AJsonPath+'/gr/panel', ''));
  Sep.GetItemInt(FOption_GroupPanelSize.x, 50);
  Sep.GetItemInt(FOption_GroupPanelSize.y, 50);

  Sep.Init(cfg.GetValue(AJsonPath+'/gr/pages', ''));
  for i in TATGroupsNums do
  begin
    Sep.GetItemInt(FOption_GroupSizes[i].x, 50);
    Sep.GetItemInt(FOption_GroupSizes[i].y, 50);
  end;
end;

procedure TfmMain.DoOps_SaveHistory_GroupView(cfg: TJsonConfig; const AJsonPath: string);
var
  SizePanel: TPoint;
  SizePage: TATGroupsPoints;
  S: string;
  i: integer;
begin
  cfg.SetValue(AJsonPath+'/groups', Ord(Groups.Mode));

  Groups.GetSizes(SizePanel, SizePage);

  S:= '';
  for i in TATGroupsNums do
    S+= Format('%d,%d,', [SizePage[i].x, SizePage[i].y]);

  cfg.SetValue(AJsonPath+'/gr/panel', Format('%d,%d', [SizePanel.x, SizePanel.y]));
  cfg.SetValue(AJsonPath+'/gr/pages', S);
end;


procedure DoOps_LoadFormRect(Cfg: TJSONConfig; const APath: string; var R: TRect);
var
  S: string;
begin
  S:= Cfg.GetValue(APath, '');
  if S<>'' then
  begin
    RectSetFromString(R, S, false);
    FixRectPositionToDesktop(R);
  end;
end;


procedure TfmMain.DoOps_LoadHistory;
//this should be called in OnCreate, to fix window flickering if window
//was maximized (Win32).
var
  cfg: TJSONConfig;
  bFlag: boolean;
  NTick: QWord;
  sFilename, sRootPath: string;
begin
  NTick:= GetTickCount64;

  AppFileCheckForNullBytes(AppFile_History);

  cfg:= TJSONConfig.Create(nil);
  try
    try
      cfg.Filename:= AppFile_History;
    except
      exit;
    end;

    DoOps_LoadFormRect(cfg, '/pos/main', FBoundsMain);
    DoOps_LoadFormRect(cfg, '/pos/side', AppPanels[cPaneSide].FormFloatBounds);
    DoOps_LoadFormRect(cfg, '/pos/bottom', AppPanels[cPaneOut].FormFloatBounds);
    DoOps_LoadFormRect(cfg, '/pos/g1', FBoundsFloatGroups1);
    DoOps_LoadFormRect(cfg, '/pos/g2', FBoundsFloatGroups2);
    DoOps_LoadFormRect(cfg, '/pos/g3', FBoundsFloatGroups3);

    //note: BoundsRect change works very long (150-200ms) on Linux gtk2
    BoundsRect:= FBoundsMain;

    FLastMaximized:= cfg.GetValue('/pos/main_max', false);
    FLastMaximizedMonitor:= cfg.GetValue('/pos/main_mon', 0);

    //console
    if Assigned(fmConsole) then
      cfg.GetValue('/list_console', fmConsole.EdInput.Items, '');

    ////recents
    cfg.GetValue('/recent_files', AppListRecents, '');
    UpdateMenuRecent(nil);

    ////finder
    FFinder.OptRegex:= cfg.GetValue('/finder/regex', false);
    FFinder.OptRegexSubst:= cfg.GetValue('/finder/regex_subst', true);
    FFinder.OptCase:= cfg.GetValue('/finder/case', false);
    FFinder.OptWords:= cfg.GetValue('/finder/words', false);
    FFinder.OptBack:= cfg.GetValue('/finder/back', false);
    FFinder.OptConfirmReplace:= cfg.GetValue('/finder/confirm', false);
    FFinder.OptWrapped:= cfg.GetValue('/finder/wrap', false);
    FFinder.OptInSelection:= cfg.GetValue('/finder/insel', false);
    FFinder.OptTokens:= TATFinderTokensAllowed(cfg.GetValue('/finder/tokens', 0));
    FFinder.OptPreserveCase:= cfg.GetValue('/finder/prescase', false);

    ////state
    case UiOps.SidepanelOnStart of
      0: bFlag:= cfg.GetValue('show_sidepanel', false);
      1: bFlag:= true;
      2: bFlag:= false;
    end;
    AppPanels[cPaneSide].Visible:= bFlag;

    case UiOps.BottomOnStart of
      0: bFlag:= cfg.GetValue('show_bottom', false);
      1: bFlag:= true;
      2: bFlag:= false;
    end;
    AppPanels[cPaneOut].Visible:= bFlag;

    AppPanels[cPaneSide].PanelSize:= Min(cfg.GetValue('size_side', 200), ClientWidth div 2);
    AppPanels[cPaneOut].PanelSize:= Min(cfg.GetValue('size_bottom', 120), ClientHeight div 2);

    AppPanels[cPaneSide].Floating:= cfg.GetValue('float_side', false);
    AppPanels[cPaneOut].Floating:= cfg.GetValue('float_bottom', false);

    UpdateThemes(
      IfThen(UiOps.ThemeUi_Loaded, UiOps.ThemeUi, '?'),
      IfThen(UiOps.ThemeSyntax_Loaded, UiOps.ThemeSyntax, '?')
      );

    UiOps.LangName:= cfg.GetValue('lang', '');
    if UiOps.LangName<>'' then
      DoLocalize;

    if FOption_AllowSessionLoad=aalsEnable then
    begin
      AppSessionName:= cfg.GetValue('session', AppSessionName);

      //workaround for bad saved values like '../../../', with and without filename
      while SBeginsWith(AppSessionName, '..'+DirectorySeparator) do
        Delete(AppSessionName, 1, 3);

      if not IsDefaultSessionActive then
      begin
        AppSessionName:= AppExpandFileNameWithDir(AppSessionName, AppDir_Settings);
        SSplitByChar(AppSessionName, '|', sFilename, sRootPath);
        if not FileExists(sFilename) then
          AppSessionName:= '';
      end;

      FLastSelectedCommand:= cfg.GetValue('last_cmd', FLastSelectedCommand);
      DoOps_LoadHistory_GroupView(cfg, '');
    end;

    FOption_SidebarTab:= cfg.GetValue('tab_left', '');
    FOption_BottomTab:= cfg.GetValue('tab_bottom', '');
  finally
    cfg.Free;
  end;

  if UiOps.LogConsoleDetailedStartupTime then
  begin
    NTick:= GetTickCount64-NTick;
    MsgLogConsole(Format('Loaded history: %dms', [NTick]));
  end;
end;


procedure TfmMain.DoApplyInitialSidebarPanel;
var
  S: string;
begin
  //if session was loaded on start, and sidepanel was inited, exit
  if AppPanels[cPaneSide].LastActivePanel<>'' then exit;

  //1)
  S:= FOption_SidebarTab;
  //if plugin didn't load, caption not valid
  if AppPanels[cPaneSide].CaptionToButtonIndex(S)<0 then
    S:= msgPanelTree_Init;

  if (S<>'-') and (S<>'') then
    if AppPanels[cPaneSide].Visible then
      AppPanels[cPaneSide].UpdatePanels(S, false, true);

  //2) the same for bottom-bar
  S:= FOption_BottomTab;
  //if plugin didn't load, caption not valid
  if AppPanels[cPaneOut].CaptionToButtonIndex(S)<0 then
    S:= msgPanelConsole_Init;

  if (S<>'-') and (S<>'') then
    if AppPanels[cPaneOut].Visible then
      AppPanels[cPaneOut].UpdatePanels(S, false, true);
end;

procedure TfmMain.DoApplyInitialWindowPos;
var
  Sep: TATStringSeparator;
  NLeft, NTop, NWidth, NHeight: integer;
begin
  if FOption_WindowPos<>'' then
  begin
    Sep.Init(FOption_WindowPos);
    Sep.GetItemInt(NLeft, Left);
    Sep.GetItemInt(NTop, Top);
    Sep.GetItemInt(NWidth, Width);
    Sep.GetItemInt(NHeight, Height);
    SetBounds(NLeft, NTop, NWidth, NHeight);
  end;
end;

procedure TfmMain.DoApplyInitialGroupSizes;
//groups sizes must be restored in OnShow, OnCreate is too early
begin
  UpdateGroupsMode(FOption_GroupMode);
  Groups.SetSizes(FOption_GroupPanelSize, FOption_GroupSizes);
  Groups.SaveSplitPos; //apply current sizes to internal Groups vars
end;


procedure TfmMain.DoOps_LoadOptionsFromString(const AString: string);
var
  fn: string;
begin
  if AString='' then exit;
  fn:= GetTempFileName+'.json';

  with TFileStream.Create(fn, fmCreate) do
  try
    WriteBuffer(AString[1], Length(AString));
  finally
    Free;
  end;

  if FileExists(fn) then
  try
    FLastLoadedConfig:= fn;
    DoOps_LoadOptions(fn, EditorOps, true);
    DoApplyAllOps;
  finally
    DeleteFileUTF8(fn);
  end;
end;

procedure TfmMain.DoOps_LoadOptions_Global(cfg: TJSONConfig);
const
  cMaxScale = 500;
var
  List: TStringList;
  s: string;
  N: integer;
begin
  Application.{%H-}MainFormOnTaskBar:= cfg.GetValue('ui_taskbar_mode'+cOptionSystemSuffix, Application.{%H-}MainFormOnTaskBar);

  N:= cfg.GetValue('ui_scale'+cOptionSystemSuffix, -1);
  if (N>=0) and (N<=cMaxScale) then
  begin
    ATEditorScalePercents:= N;
    if ATEditorScalePercents=0 then
      ATEditorScalePercents:= Max(100, 100*Screen.PixelsPerInch div 96); //some Linux'es return PixelsPerInch=91
  end;

  N:= cfg.GetValue('ui_scale_font'+cOptionSystemSuffix, -1);
  if (N>=0) and (N<=cMaxScale) then
    ATEditorScaleFontPercents:= N;

  if ATEditorScaleFontPercents>100 then //not always, as GTK2 widgetset has a bug: hint is clipped on bottom
    Screen.HintFont.Size:= ATEditorScaleFont(UiOps.VarFontSize);

  ATEditorOptions.CharSizeProportional:= cfg.GetValue('unicode_proportional', ATEditorOptions.CharSizeProportional);
  ATEditorOptions.CharScaleFullWidth:= cfg.GetValue('unicode_full_width', ATEditorOptions.CharScaleFullWidth);
  ATEditorOptions.BookmarksAutoDelete:= cfg.GetValue('auto_delete_bookmarks', ATEditorOptions.BookmarksAutoDelete);
  ATEditorOptions.DebugTiming:= cfg.GetValue('log_timing', ATEditorOptions.DebugTiming);

  ATEditorOptions.MouseDragDropFocusesTargetEditor:= cfg.GetValue('mouse_drag_drop_focus_target', ATEditorOptions.MouseDragDropFocusesTargetEditor);
  ATEditorOptions.MaxTabPositionToExpand:= cfg.GetValue('tab_max_pos_expanded', ATEditorOptions.MaxTabPositionToExpand);
  ATEditorOptions.MaxLinesForStatusbarOffsetsCalc:= cfg.GetValue('max_lines_to_calc_offsets', ATEditorOptions.MaxLinesForStatusbarOffsetsCalc);
  ATEditorOptions.MaxLineLenForAccurateCharWidths:= 1024;
  ATEditorOptions.MaxLineLenForUndo:= cfg.GetValue('max_line_len_for_undo', ATEditorOptions.MaxLineLenForUndo);
  ATLiteLexerMaxLineLength:= cfg.GetValue('max_line_len_parsed', ATLiteLexerMaxLineLength);
  ATEditorMaxClipboardRecents:= cfg.GetValue('ui_max_history_clipboard', ATEditorMaxClipboardRecents);
  ATEditorOptions.MinimapColoringTime:= cfg.GetValue('minimap_coloring_time', ATEditorOptions.MinimapColoringTime);
  ATEditorOptions.SpeedScrollAutoVert:= cfg.GetValue('mouse_scroll_speed_vert', ATEditorOptions.SpeedScrollAutoVert);

  ATEditorOptions.UnprintedTabCharLength:= cfg.GetValue('unprinted_tab_len', ATEditorOptions.UnprintedTabCharLength);
  ATEditorOptions.UnprintedReplaceSpec:= cfg.GetValue('unprinted_replace_spec', ATEditorOptions.UnprintedReplaceSpec);
  s:= cfg.GetValue('unprinted_replace_to_code', '');
  if s<>'' then
    ATEditorOptions.UnprintedReplaceSpecToCode:= StrToIntDef('$'+s, ATEditorOptions.UnprintedReplaceSpecToCode);

  UiOps.MaxLineLenForEditingKeepingLexer:= cfg.GetValue('max_line_len_for_editing_keeping_lexer', UiOps.MaxLineLenForEditingKeepingLexer);

  s:= cfg.GetValue('fallback_encoding', '');
  if s<>'' then
    AppApplyFallbackEncoding(s);

  s:= cfg.GetValue('unprinted_symbols_scale', '');
  if s<>'' then
    AppApplyUnprintedSymbolsScale(s);

  s:= cfg.GetValue('auto_copy_clp', '?');
  if s<>'?' then
    AppApplyAutoCopyToClipboard(s);

  EControlOptions.AutoFoldComments:= cfg.GetValue('auto_fold_comments',
    EControlOptions.AutoFoldComments);
  EControlOptions.MaxLinesWhenParserEnablesFolding:= cfg.GetValue('lexer_folding_max_lines',
    EControlOptions.MaxLinesWhenParserEnablesFolding);

  ATFlatTheme.EnableColorBgOver:= cfg.GetValue('ui_button_hover',
    ATFlatTheme.EnableColorBgOver);
  ATScrollbarTheme.DirectJumpOnClickPageUpDown:= cfg.GetValue('scrollbar_click_jump',
    ATScrollbarTheme.DirectJumpOnClickPageUpDown);

  s:= cfg.GetValue('scrollbar_arrows', '');
  if s<>'' then
    AppApplyScrollbarStyles(s);

  s:= cfg.GetValue('renderer_tweaks'+cOptionSystemSuffix, '?');
  if s<>'?' then
    AppApplyRendererTweaks(s);

  //additional complex options with sub-keys
  List:= TStringList.Create;
  try
    cfg.EnumValues('/plugin_groups', List);
    for s in List do
      AppConfig_PGroups.Add(s, cfg.GetValue('/plugin_groups/'+s, ''));

    cfg.EnumValues('/detect', List);
    for s in List do
      AppConfig_Detect.Add(s, cfg.GetValue('/detect/'+s, ''));

    cfg.EnumValues('/detect_line', List);
    for s in List do
      AppConfig_DetectLine.Add(s, cfg.GetValue('/detect_line/'+s, ''));
  finally
    FreeAndNil(List);
  end;
end;

procedure TfmMain.DoOps_LoadOptions_Editor(cfg: TJSONConfig; var Op: TEditorOps);
const
  cMinFontSize = 7;
begin
  Op.OpFontName:= cfg.GetValue('font_name'+cOptionSystemSuffix, Op.OpFontName);
  Op.OpFontName_i:= cfg.GetValue('font_name_i'+cOptionSystemSuffix, Op.OpFontName_i);
  Op.OpFontName_b:= cfg.GetValue('font_name_b'+cOptionSystemSuffix, Op.OpFontName_b);
  Op.OpFontName_bi:= cfg.GetValue('font_name_bi'+cOptionSystemSuffix, Op.OpFontName_bi);

  //gtk2 crashes with empty 'font_name'
  if Op.OpFontName='' then
    Op.OpFontName:= AppDefaultEdFont;

  Op.OpFontSize:= Abs(cfg.GetValue('font_size'+cOptionSystemSuffix, Op.OpFontSize)); //Abs() for macOS
  Op.OpFontSize_i:= Abs(cfg.GetValue('font_size_i'+cOptionSystemSuffix, Op.OpFontSize_i));
  Op.OpFontSize_b:= Abs(cfg.GetValue('font_size_b'+cOptionSystemSuffix, Op.OpFontSize_b));
  Op.OpFontSize_bi:= Abs(cfg.GetValue('font_size_bi'+cOptionSystemSuffix, Op.OpFontSize_bi));

  if Op.OpFontSize<cMinFontSize then
    Op.OpFontSize:= AppDefaultEdFontSize;
  if Op.OpFontSize_i<cMinFontSize then
    Op.OpFontSize_i:= AppDefaultEdFontSize;
  if Op.OpFontSize_b<cMinFontSize then
    Op.OpFontSize_b:= AppDefaultEdFontSize;
  if Op.OpFontSize_bi<cMinFontSize then
    Op.OpFontSize_bi:= AppDefaultEdFontSize;

  Op.OpFontSize_original:= Op.OpFontSize;
  Op.OpFontSize_original_i:= Op.OpFontSize_i;
  Op.OpFontSize_original_b:= Op.OpFontSize_b;
  Op.OpFontSize_original_bi:= Op.OpFontSize_bi;

  Op.OpFontQuality:= TFontQuality(cfg.GetValue('font_quality'+cOptionSystemSuffix, Ord(Op.OpFontQuality)));
  Op.OpFontLigatures:= cfg.GetValue('font_ligatures', Op.OpFontLigatures);
  Op.OpFlickerReducingPause:= cfg.GetValue('renderer_anti_flicker'+cOptionSystemSuffix, Op.OpFlickerReducingPause);

  Op.OpActiveBorderRaw:= cfg.GetValue('ui_active_border', Op.OpActiveBorderRaw);
  Op.OpActiveBorderInControls:= Op.OpActiveBorderRaw > 0;
  Op.OpActiveBorderInEditor:= Op.OpActiveBorderRaw >= 100;
  Op.OpActiveBorderWidth:= Op.OpActiveBorderRaw mod 100;

  Op.OpScrollbarsNew:= cfg.GetValue('scrollbar_themed', Op.OpScrollbarsNew);
  Op.OpSpacingY:= cfg.GetValue('spacing_y'+cOptionSystemSuffix, Op.OpSpacingY);

  Op.OpTabSize:= cfg.GetValue('tab_size', Op.OpTabSize);
  Op.OpTabSpaces:= cfg.GetValue('tab_spaces', Op.OpTabSpaces);
  Op.OpTabSmart:= cfg.GetValue('tab_smart', Op.OpTabSmart);

  Op.OpMaxLineLenForBracketFinder:= cfg.GetValue('max_line_len_brackets', Op.OpMaxLineLenForBracketFinder);

  Op.OpOverwriteSel:= cfg.GetValue('ovr_sel', Op.OpOverwriteSel);
  Op.OpOverwriteOnPaste:= cfg.GetValue('ovr_on_paste', Op.OpOverwriteOnPaste);
  Op.OpPasteWithEolAtLineStart:= cfg.GetValue('paste_multiline_at_line_start', Op.OpPasteWithEolAtLineStart);

  Op.OpAutoCloseBrackets:= cfg.GetValue('auto_close_brackets', Op.OpAutoCloseBrackets);
  Op.OpAutocompleteAutoshowCharCount:= cfg.GetValue('autocomplete_autoshow_chars', Op.OpAutocompleteAutoshowCharCount);
  Op.OpAutocompleteTriggerChars:= cfg.GetValue('autocomplete_trigger_chars', Op.OpAutocompleteTriggerChars);
  Op.OpAutocompleteCommitChars:= cfg.GetValue('autocomplete_commit_chars', Op.OpAutocompleteCommitChars);
  Op.OpAutocompleteCloseChars:= cfg.GetValue('autocomplete_close_chars', Op.OpAutocompleteCloseChars);
  Op.OpAutocompleteAddOpeningBracket:= cfg.GetValue('autocomplete_add_opening_bracket', Op.OpAutocompleteAddOpeningBracket);
  Op.OpAutocompleteUpDownAtEdge:= cfg.GetValue('autocomplete_up_down_at_edge', Op.OpAutocompleteUpDownAtEdge);
  Op.OpAutocompleteCommitIfSingleItem:= cfg.GetValue('autocomplete_commit_single_item', Op.OpAutocompleteCommitIfSingleItem);

  Op.OpUnderlineColorFiles:= cfg.GetValue('underline_color_files', Op.OpUnderlineColorFiles);
  Op.OpUnderlineColorSize:= cfg.GetValue('underline_color_size', Op.OpUnderlineColorSize);

  Op.OpLinks:= cfg.GetValue('links_hilite', Op.OpLinks);
  Op.OpLinksRegex:= cfg.GetValue('links_regex', Op.OpLinksRegex);

  Op.OpGutterShow:= cfg.GetValue('gutter_show', Op.OpGutterShow);
  Op.OpGutterFold:= cfg.GetValue('gutter_fold', Op.OpGutterFold);
  Op.OpGutterFoldAlways:= cfg.GetValue('gutter_fold_always', Op.OpGutterFoldAlways);
  Op.OpGutterFoldIcons:= cfg.GetValue('gutter_fold_icons', Op.OpGutterFoldIcons);
  Op.OpGutterBookmarks:= cfg.GetValue('gutter_bookmarks', Op.OpGutterBookmarks);
  Op.OpGutterLineStates:= cfg.GetValue('gutter_line_states', Op.OpGutterLineStates);
  Op.OpGutterIconSize:= cfg.GetValue('gutter_icon_size', Op.OpGutterIconSize);

  Op.OpNumbersShow:= cfg.GetValue('numbers_show', Op.OpNumbersShow);
  //Op.OpNumbersFontSize:= cfg.GetValue('numbers_font_size', Op.OpNumbersFontSize);
  Op.OpNumbersStyle:= cfg.GetValue('numbers_style', Op.OpNumbersStyle);
  Op.OpNumbersForCarets:= cfg.GetValue('numbers_for_carets', Op.OpNumbersForCarets);
  Op.OpNumbersCenter:= cfg.GetValue('numbers_center', Op.OpNumbersCenter);

  Op.OpRulerShow:= cfg.GetValue('ruler_show', Op.OpRulerShow);
  Op.OpRulerNumeration:= cfg.GetValue('ruler_numeration', Op.OpRulerNumeration);
  Op.OpRulerMarkCaret:= cfg.GetValue('ruler_mark_caret', Op.OpRulerMarkCaret);
  Op.OpRulerHeight:= cfg.GetValue('ruler_height', Op.OpRulerHeight);

  Op.OpMinimapShow:= cfg.GetValue('minimap_show', Op.OpMinimapShow);
  Op.OpMinimapShowSelAlways:= cfg.GetValue('minimap_sel_always', Op.OpMinimapShowSelAlways);
  Op.OpMinimapShowSelBorder:= cfg.GetValue('minimap_sel_border', Op.OpMinimapShowSelBorder);
  Op.OpMinimapCharWidth:= cfg.GetValue('minimap_char_width', Op.OpMinimapCharWidth);
  Op.OpMinimapAtLeft:= cfg.GetValue('minimap_at_left', Op.OpMinimapAtLeft);
  Op.OpMinimapScale:= cfg.GetValue('minimap_scale', Op.OpMinimapScale);
  Op.OpMinimapTooltipShow:= cfg.GetValue('minimap_tooltip_show', Op.OpMinimapTooltipShow);
  Op.OpMinimapTooltipHeight:= cfg.GetValue('minimap_tooltip_height', Op.OpMinimapTooltipHeight);
  Op.OpMinimapTooltipWidth:= cfg.GetValue('minimap_tooltip_width', Op.OpMinimapTooltipWidth);
  Op.OpMinimapTooltipFontSize:= cfg.GetValue('minimap_tooltip_font_size', Op.OpMinimapTooltipFontSize);
  Op.OpMinimapDragImmediately:= cfg.GetValue('minimap_drag_immediately', Op.OpMinimapDragImmediately);

  Op.OpMicromapShow:= cfg.GetValue('micromap_show', Op.OpMicromapShow);
  Op.OpMicromapOnScrollbar:= cfg.GetValue('micromap_on_scrollbar', Op.OpMicromapOnScrollbar);
  Op.OpMicromapLineStates:= cfg.GetValue('micromap_line_states', Op.OpMicromapLineStates);
  Op.OpMicromapBookmarks:= cfg.GetValue('micromap_bookmarks', Op.OpMicromapBookmarks);

  Op.OpMarginFixed:= cfg.GetValue('margin', Op.OpMarginFixed);
  Op.OpMarginString:= cfg.GetValue('margin_string', Op.OpMarginString);
  Op.OpMarkerSize:= cfg.GetValue('marker_size', Op.OpMarkerSize);
  Op.OpStaplesStyle:= cfg.GetValue('staples_style', Op.OpStaplesStyle);
  Op.OpStaplesProps:= cfg.GetValue('staples_props', Op.OpStaplesProps);

  Op.OpUnprintedShow:= cfg.GetValue('unprinted_show', Op.OpUnprintedShow);
  Op.OpUnprintedContent:= cfg.GetValue('unprinted_content', Op.OpUnprintedContent);

  Op.OpBracketHilite:= cfg.GetValue('bracket_highlight', Op.OpBracketHilite);
  Op.OpBracketSymbols:= cfg.GetValue('bracket_symbols', op.OpBracketSymbols);
  Op.OpBracketDistance:= cfg.GetValue('bracket_distance', op.OpBracketDistance);

  Op.OpWrapMode:= Min(cfg.GetValue('wrap_mode', Op.OpWrapMode), Ord(High(TATEditorWrapMode)));
  Op.OpWrapIndented:= cfg.GetValue('wrap_indented', Op.OpWrapIndented);
  Op.OpWrapEnabledMaxLines:= cfg.GetValue('wrap_enabled_max_lines', Op.OpWrapEnabledMaxLines);

  Op.OpUndoLimit:= cfg.GetValue('undo_limit', Op.OpUndoLimit);
  Op.OpUndoGrouped:= cfg.GetValue('undo_grouped', Op.OpUndoGrouped);
  Op.OpUndoAfterSave:= cfg.GetValue('undo_after_save', Op.OpUndoAfterSave);
  Op.OpUndoMaxCarets:= cfg.GetValue('undo_max_carets', Op.OpUndoMaxCarets);
  Op.OpUndoPause:= cfg.GetValue('undo_pause', Op.OpUndoPause);
  Op.OpUndoMouseClicks:= cfg.GetValue('undo_mouse_clicks', Op.OpUndoMouseClicks);

  Op.OpCaretBlinkTime:= cfg.GetValue('caret_blink_time', Op.OpCaretBlinkTime);
  Op.OpCaretBlinkEn:= cfg.GetValue('caret_blink_en', Op.OpCaretBlinkEn);
  Op.OpCaretViewNormal:= cfg.GetValue('caret_view', Op.OpCaretViewNormal);
  Op.OpCaretViewOverwrite:= cfg.GetValue('caret_view_ovr', Op.OpCaretViewOverwrite);
  Op.OpCaretViewReadonly:= cfg.GetValue('caret_view_ro', Op.OpCaretViewReadonly);
  Op.OpCaretVirtual:= cfg.GetValue('caret_after_end', Op.OpCaretVirtual);
  Op.OpCaretMulti:= cfg.GetValue('caret_multi', Op.OpCaretMulti);
  Op.OpCaretAfterPasteColumn:= cfg.GetValue('caret_after_paste_column', Op.OpCaretAfterPasteColumn);
  Op.OpCaretsAddedToColumnSel:= cfg.GetValue('carets_with_column_sel', Op.OpCaretsAddedToColumnSel);
  Op.OpCaretsPrimitiveColumnSel:= cfg.GetValue('carets_primitive_column_sel', Op.OpCaretsPrimitiveColumnSel);
  Op.OpCaretKeepVisibleOnScroll:= cfg.GetValue('caret_keep_visible_on_scroll', Op.OpCaretKeepVisibleOnScroll);
  Op.OpCaretProximityVert:= cfg.GetValue('caret_proximity_vert', Op.OpCaretProximityVert);

  Op.OpKeepSelFontColor:= cfg.GetValue('keep_sel_font_color', Op.OpKeepSelFontColor);
  Op.OpShowCurLine:= cfg.GetValue('show_cur_line', Op.OpShowCurLine);
  Op.OpShowCurLineMinimal:= cfg.GetValue('show_cur_line_minimal', Op.OpShowCurLineMinimal);
  Op.OpShowCurLineOnlyFocused:= cfg.GetValue('show_cur_line_only_focused', Op.OpShowCurLineOnlyFocused);
  Op.OpShowCurCol:= cfg.GetValue('show_cur_column', Op.OpShowCurCol);
  Op.OpShowLastLineOnTop:= cfg.GetValue('show_last_line_on_top', Op.OpShowLastLineOnTop);
  Op.OpShowFullBackgroundSel:= cfg.GetValue('show_full_sel_bg', Op.OpShowFullBackgroundSel);
  Op.OpShowFullBackgroundSyntax:= cfg.GetValue('show_full_syntax_bg', Op.OpShowFullBackgroundSyntax);
  Op.OpShowMouseSelFrame:= cfg.GetValue('show_mouse_sel_frame', Op.OpShowMouseSelFrame);
  Op.OpShowIndentLines:= cfg.GetValue('show_indent_lines', Op.OpShowIndentLines);
  Op.OpCopyLineIfNoSel:= cfg.GetValue('copy_line_if_no_sel', Op.OpCopyLineIfNoSel);
  Op.OpCutLineIfNoSel:= cfg.GetValue('cut_line_if_no_sel', Op.OpCutLineIfNoSel);
  Op.OpCopyColumnAlignedBySpaces:= cfg.GetValue('copy_column_aligned_by_spaces', Op.OpCopyColumnAlignedBySpaces);
  Op.OpSavingTrimSpaces:= cfg.GetValue('saving_trim_spaces', Op.OpSavingTrimSpaces);
  Op.OpSavingTrimFinalEmptyLines:= cfg.GetValue('saving_trim_final_empty', Op.OpSavingTrimFinalEmptyLines);
  Op.OpSavingForceFinalEol:= cfg.GetValue('saving_force_final_eol', Op.OpSavingForceFinalEol);
  Op.OpShowHintOnVertScroll:= cfg.GetValue('show_hint_on_vert_scroll', Op.OpShowHintOnVertScroll);
  Op.OpSmoothScroll:= cfg.GetValue('smooth_scroll', Op.OpSmoothScroll);
  Op.OpCenteringWidth:= cfg.GetValue('centering_width', Op.OpCenteringWidth);
  Op.OpCenteringForDistractionFree:= cfg.GetValue('centering_for_distraction_free', Op.OpCenteringForDistractionFree);
  Op.OpScrollStyleHorz:= cfg.GetValue('scroll_style_horz', Op.OpScrollStyleHorz);

  Op.OpLexerDynamicHiliteMaxLines:= cfg.GetValue('dynamic_highlight', Op.OpLexerDynamicHiliteMaxLines);
  Op.OpLexerLineSeparators:= false; //cfg.GetValue('lexer_line_separators', Op.OpLexerLineSeparators);

  Op.OpZebra:= cfg.GetValue('zebra', Op.OpZebra);
  Op.OpZebraStep:= cfg.GetValue('zebra_step', Op.OpZebraStep);
  Op.OpDimUnfocused:= cfg.GetValue('dim_unfocused', Op.OpDimUnfocused);

  Op.OpNonWordChars:= cfg.GetValue('nonword_chars', Op.OpNonWordChars);
  Op.OpFoldStyle:= cfg.GetValue('fold_style', Op.OpFoldStyle);
  Op.OpFoldTooltipShow:= cfg.GetValue('fold_tooltip_show', Op.OpFoldTooltipShow);
  Op.OpFoldIconForMinimalRangeHeight:= cfg.GetValue('fold_icon_min_range', Op.OpFoldIconForMinimalRangeHeight);

  Op.OpIndentAuto:= cfg.GetValue('indent_auto', Op.OpIndentAuto);
  Op.OpIndentAutoKind:= cfg.GetValue('indent_kind', Op.OpIndentAutoKind);
  Op.OpIndentSize:= cfg.GetValue('indent_size', Op.OpIndentSize);
  Op.OpIndentAutoRule:= cfg.GetValue('indent_auto_rule', Op.OpIndentAutoRule);
  Op.OpUnIndentKeepsAlign:= cfg.GetValue('unindent_keeps_align', Op.OpUnIndentKeepsAlign);
  Op.OpIndentMakesWholeLineSel:= cfg.GetValue('indent_makes_whole_line_sel', Op.OpIndentMakesWholeLineSel);

  Op.OpMouseGotoDefinition:= cfg.GetValue('mouse_goto_definition', Op.OpMouseGotoDefinition);
  Op.OpMouse2ClickDragSelectsWords:= cfg.GetValue('mouse_2click_drag_select_words', Op.OpMouse2ClickDragSelectsWords);
  Op.OpMouseDragDrop:= cfg.GetValue('mouse_drag_drop', Op.OpMouseDragDrop);
  Op.OpMouseMiddleClickAction:= cfg.GetValue('mouse_middle_click', Op.OpMouseMiddleClickAction);
  Op.OpMouseRightClickMovesCaret:= cfg.GetValue('mouse_rt_click_moves_caret', Op.OpMouseRightClickMovesCaret);
  Op.OpMouseEnableColumnSelection:= cfg.GetValue('mouse_column_selection', Op.OpMouseEnableColumnSelection);
  Op.OpMouseHideCursorOnType:= cfg.GetValue('mouse_hide_cursor', Op.OpMouseHideCursorOnType);
  Op.OpMouseGutterClickSelectedLine:= cfg.GetValue('mouse_gutter_click_sel', Op.OpMouseGutterClickSelectedLine);
  Op.OpMouseWheelZoom:= cfg.GetValue('mouse_wheel_zoom', Op.OpMouseWheelZoom);
  Op.OpMouseWheelSpeedVert:= cfg.GetValue('mouse_wheel_speed_vert', Op.OpMouseWheelSpeedVert);
  Op.OpMouseWheelSpeedHorz:= cfg.GetValue('mouse_wheel_speed_horz', Op.OpMouseWheelSpeedHorz);
  Op.OpMouseClickNumberSelectsEol:= cfg.GetValue('mouse_click_number_selects_eol', Op.OpMouseClickNumberSelectsEol);
  Op.OpMouseClickLinks:= cfg.GetValue('mouse_click_links', Op.OpMouseClickLinks);

  Op.OpKeyBackspaceUnindent:= cfg.GetValue('key_bksp_unindent', Op.OpKeyBackspaceUnindent);
  Op.OpKeyBackspaceWrap:= cfg.GetValue('key_bksp_wrap', Op.OpKeyBackspaceWrap);
  Op.OpKeyTabIndents:= cfg.GetValue('key_tab_indents_sel', Op.OpKeyTabIndents);
  Op.OpKeyHomeToNonSpace:= cfg.GetValue('key_home_to_nonspace', Op.OpKeyHomeToNonSpace);
  Op.OpKeyHomeEndNavigateWrapped:= cfg.GetValue('key_home_end_nav_wrapped', Op.OpKeyHomeEndNavigateWrapped);
  Op.OpKeyEndToNonSpace:= cfg.GetValue('key_end_to_nonspace', Op.OpKeyEndToNonSpace);
  Op.OpKeyPageKeepsRelativePos:= cfg.GetValue('key_page_keeps_rel_pos', Op.OpKeyPageKeepsRelativePos);
  Op.OpKeyPageUpDownSize:= cfg.GetValue('key_page_size', Op.OpKeyPageUpDownSize);
  Op.OpKeyUpDownKeepColumn:= cfg.GetValue('key_up_down_keeps_column', Op.OpKeyUpDownKeepColumn);
  Op.OpKeyUpDownNavigateWrapped:= cfg.GetValue('key_up_down_nav_wrapped', Op.OpKeyUpDownNavigateWrapped);
  Op.OpKeyUpDownAllowToEdge:= cfg.GetValue('key_up_down_allow_edge', Op.OpKeyUpDownAllowToEdge);
  Op.OpKeyLeftRightGoToNextLineWithCarets:= cfg.GetValue('key_left_right_wrap_with_carets', Op.OpKeyLeftRightGoToNextLineWithCarets);
  Op.OpKeyLeftRightSwapSel:= cfg.GetValue('key_left_right_swap_sel', Op.OpKeyLeftRightSwapSel);
  Op.OpKeyLeftRightSwapSelAndSelect:= cfg.GetValue('key_left_right_swap_sel_and_select', Op.OpKeyLeftRightSwapSelAndSelect);
end;


procedure TfmMain.DoOps_LoadOptions_Ui(cfg: TJSONConfig);
var
  S: string;
  element: TAppHistoryElement;
begin
  UiOps.VarFontName:= cfg.GetValue(OpStr_UiFontName, UiOps.VarFontName);
  UiOps.VarFontSize:= Abs(cfg.GetValue(OpStr_UiFontSize, UiOps.VarFontSize)); //Abs for macOS

  UiOps.OutputFontName:= cfg.GetValue(OpStr_UiFontOutputName, UiOps.OutputFontName);
  UiOps.OutputFontSize:= Abs(cfg.GetValue(OpStr_UiFontOutputSize, UiOps.OutputFontSize)); //Abs for macOS

  UiOps.StatusbarFontName:= cfg.GetValue('ui_font_statusbar_name'+cOptionSystemSuffix, UiOps.StatusbarFontName);
  UiOps.StatusbarFontSize:= Abs(cfg.GetValue('ui_font_statusbar_size'+cOptionSystemSuffix, UiOps.StatusbarFontSize)); //Abs for macOS

  UiOps.DoubleBuffered:= cfg.GetValue('ui_buffered'+cOptionSystemSuffix, UiOps.DoubleBuffered);

  UiOps.StatusNoSel:= cfg.GetValue('ui_statusbar_no_sel', UiOps.StatusNoSel);
  UiOps.StatusSmallSel:= cfg.GetValue('ui_statusbar_small_sel', UiOps.StatusSmallSel);
  UiOps.StatusStreamSel:= cfg.GetValue('ui_statusbar_str_sel', UiOps.StatusStreamSel);
  UiOps.StatusColSel:= cfg.GetValue('ui_statusbar_col_sel', UiOps.StatusColSel);
  UiOps.StatusCarets:= cfg.GetValue('ui_statusbar_carets', UiOps.StatusCarets);
  UiOps.StatusPanels:= cfg.GetValue('ui_statusbar_panels', UiOps.StatusPanels);
  UiOps.StatusTime:= Max(1, Min(60, cfg.GetValue('ui_statusbar_time', UiOps.StatusTime)));

  UiOps.ShowTitlePath:= cfg.GetValue('ui_title_path', UiOps.ShowTitlePath);

  UiOps.ReopenSession:= cfg.GetValue('ui_reopen_session', UiOps.ReopenSession);
  UiOps.ReopenSessionWithCmdLine:= cfg.GetValue('ui_reopen_session_cmdline', UiOps.ReopenSessionWithCmdLine);
  if UiOps.ReopenSessionWithCmdLine then
    if FOption_AllowSessionLoad=aalsNotGood then
      FOption_AllowSessionLoad:= aalsEnable;
  UiOps.SessionSaveInterval:= cfg.GetValue('ui_session_save_interval', UiOps.SessionSaveInterval);
  UiOps.SessionSaveOnExit:= cfg.GetValue('ui_auto_save_session', UiOps.SessionSaveOnExit);
  UiOps.BackupLastSessions:= cfg.GetValue('ui_backup_sessions', UiOps.BackupLastSessions);

  UiOps.PluginDialogsShowInTaskbar:= cfg.GetValue('ui_dialogs_in_taskbar'+cOptionSystemSuffix, UiOps.PluginDialogsShowInTaskbar);
  UiOps.OneInstance:= cfg.GetValue('ui_one_instance', UiOps.OneInstance);
  UiOps.UndoPersistent:= cfg.GetValue('undo_persistent', UiOps.UndoPersistent);
  UiOps.AllowSaveOfUnmodifiedFile:= cfg.GetValue('ui_save_always', UiOps.AllowSaveOfUnmodifiedFile);

  UiOps.NotificationEnabled:= cfg.GetValue('ui_notif', UiOps.NotificationEnabled);
  UiOps.NotificationTimeSeconds:= Max(1, cfg.GetValue('ui_notif_time', UiOps.NotificationTimeSeconds)); //don't allow 0
  UiOps.NotificationConfirmReload:= cfg.GetValue('ui_notif_confirm', UiOps.NotificationConfirmReload);

  UiOps.ConfirmLinksClicks:= cfg.GetValue('ui_links_confirm', UiOps.ConfirmLinksClicks);

  UiOps.NonTextFiles:= cfg.GetValue('ui_non_text', UiOps.NonTextFiles);
  UiOps.NonTextFilesBufferKb:= Max(1, Min(1024, cfg.GetValue('ui_non_text_buffer', UiOps.NonTextFilesBufferKb)));
  UiOps.ReloadFollowTail:= cfg.GetValue('ui_reload_tail', UiOps.ReloadFollowTail);
  UiOps.ReloadUnsavedConfirm:= cfg.GetValue('ui_reload_unsaved_confirm', UiOps.ReloadUnsavedConfirm);
  UiOps.FullScreen:= cfg.GetValue('ui_fullscreen', UiOps.FullScreen);

  UiOps.Emmet_AddSlashToEmptyTags:= cfg.GetValue('emmet_add_slash_to_empty_tags', UiOps.Emmet_AddSlashToEmptyTags);
  UiOps.Emmet_CommentTags:= cfg.GetValue('emmet_comment_tags', UiOps.Emmet_CommentTags);
  UiOps.Emmet_IndentNested:= cfg.GetValue('emmet_indent_nested', UiOps.Emmet_IndentNested);
  UiOps.Emmet_SingleLine:= cfg.GetValue('emmet_single_line', UiOps.Emmet_SingleLine);
  UiOps.Emmet_TrimLineMarkers:= cfg.GetValue('emmet_trim_line_markers', UiOps.Emmet_TrimLineMarkers);
  UiOps.Emmet_WordWrap:= cfg.GetValue('emmet_word_wrap', UiOps.Emmet_WordWrap);

  UiOps.HotkeyFindDialog:= cfg.GetValue('find_hotkey_find_dlg', UiOps.HotkeyFindDialog);
  UiOps.HotkeyReplaceDialog:= cfg.GetValue('find_hotkey_replace_dlg', UiOps.HotkeyReplaceDialog);
  UiOps.HotkeyFindFirst:= cfg.GetValue('find_hotkey_find_first', UiOps.HotkeyFindFirst);
  UiOps.HotkeyFindNext:= cfg.GetValue('find_hotkey_find_next', UiOps.HotkeyFindNext);
  UiOps.HotkeyFindPrev:= cfg.GetValue('find_hotkey_find_prev', UiOps.HotkeyFindPrev);
  UiOps.HotkeyReplaceAndFindNext:= cfg.GetValue('find_hotkey_replace', UiOps.HotkeyReplaceAndFindNext);
  UiOps.HotkeyReplaceNoFindNext:= cfg.GetValue('find_hotkey_replace_no_next', UiOps.HotkeyReplaceNoFindNext);
  UiOps.HotkeyReplaceAll:= cfg.GetValue('find_hotkey_replace_all', UiOps.HotkeyReplaceAll);
  UiOps.HotkeyReplaceGlobal:= cfg.GetValue('find_hotkey_replace_global', UiOps.HotkeyReplaceGlobal);
  UiOps.HotkeyCountAll:= cfg.GetValue('find_hotkey_count_all', UiOps.HotkeyCountAll);
  UiOps.HotkeyExtractAll:= cfg.GetValue('find_hotkey_extract', UiOps.HotkeyExtractAll);
  UiOps.HotkeySelectAll:= cfg.GetValue('find_hotkey_select_all', UiOps.HotkeySelectAll);
  UiOps.HotkeyMarkAll:= cfg.GetValue('find_hotkey_mark_all', UiOps.HotkeyMarkAll);
  UiOps.HotkeyToggleRegex:= cfg.GetValue('find_hotkey_op_regex', UiOps.HotkeyToggleRegex);
  UiOps.HotkeyToggleCaseSens:= cfg.GetValue('find_hotkey_op_case', UiOps.HotkeyToggleCaseSens);
  UiOps.HotkeyToggleWords:= cfg.GetValue('find_hotkey_op_words', UiOps.HotkeyToggleWords);
  UiOps.HotkeyToggleWrapped:= cfg.GetValue('find_hotkey_op_wrapped', UiOps.HotkeyToggleWrapped);
  UiOps.HotkeyToggleInSelect:= cfg.GetValue('find_hotkey_op_in_sel', UiOps.HotkeyToggleInSelect);
  UiOps.HotkeyToggleMultiline:= cfg.GetValue('find_hotkey_op_multiline', UiOps.HotkeyToggleMultiline);
  UiOps.HotkeyToggleConfirmRep:= cfg.GetValue('find_hotkey_op_confirm_rep', UiOps.HotkeyToggleConfirmRep);
  UiOps.HotkeyToggleTokens:= cfg.GetValue('find_hotkey_op_tokens', UiOps.HotkeyToggleTokens);
  UiOps.HotkeyToggleHiAll:= cfg.GetValue('find_hotkey_op_hi', UiOps.HotkeyToggleHiAll);
  UiOps.HotkeyTogglePresCase:= cfg.GetValue('find_hotkey_op_prescase', UiOps.HotkeyTogglePresCase);

  UiOps.LexerThemes:= cfg.GetValue('ui_lexer_themes', UiOps.LexerThemes);
  UiOps.LexerMenuGrouped:= cfg.GetValue('ui_lexers_grouped', UiOps.LexerMenuGrouped);
  UiOps.LexerPostponeUntilShown:= cfg.GetValue('lexer_postpone_until_shown', UiOps.LexerPostponeUntilShown);
  if not UiOps.LexerPostponeUntilShown then
    AppAllowFrameParsing:= true;

  UiOps.ShowMenubar:= cfg.GetValue('ui_menu_show', UiOps.ShowMenubar);
  UiOps.ShowStatusbar:= cfg.GetValue('ui_statusbar_show', UiOps.ShowStatusbar);
  UiOps.ShowToolbar:= cfg.GetValue('ui_toolbar_show', UiOps.ShowToolbar);
  UiOps.ShowSidebarMenuButton:= cfg.GetValue('ui_menu_button', UiOps.ShowSidebarMenuButton);

  if ToolbarSideTop.ButtonCount>0 then
    with ToolbarSideTop.Buttons[0] do
      if OnClick=@DoSidebar_MainMenuClick then
        case UiOps.ShowSidebarMenuButton of
          0: Visible:= false;
          1: Visible:= true;
          2: Visible:= not UiOps.ShowMenubar;
        end;

  UiOps.SidebarShow:= cfg.GetValue('ui_sidebar_show', UiOps.SidebarShow);
  UiOps.SidebarOnRight:= cfg.GetValue('ui_sidebar_on_right', UiOps.SidebarOnRight);
  UiOps.SidebarTheme:= cfg.GetValue('ui_sidebar_theme', UiOps.SidebarTheme);
  UiOps.SidepanelOnStart:= cfg.GetValue('ui_sidepanel_on_start', UiOps.SidepanelOnStart);
  UiOps.BottomOnStart:= cfg.GetValue('ui_bottom_on_start', UiOps.BottomOnStart);
  UiOps.TreeTheme:= cfg.GetValue('ui_tree_theme', UiOps.TreeTheme);
  UiOps.ToolBarTheme:= cfg.GetValue('ui_toolbar_theme', UiOps.ToolBarTheme);

  {$ifdef windows}
  UiOps.ThemedMainMenu:= cfg.GetValue('ui_menu_themed', UiOps.ThemedMainMenu);
  UiOps.ThemedMainMenuFontSize:= cfg.GetValue('ui_menu_themed_font_size', UiOps.ThemedMainMenuFontSize);
  {$endif}

  {$ifndef windows}
  //option is ignored on windows
  UiOps.PyLibrary:= cfg.GetValue('pylib'+cOptionSystemSuffix, UiOps.PyLibrary);
  {$endif}
  if UiOps.PyLibrary='' then
    UiOps.PyLibrary:= InitPyLibraryPath;

  UiOps.PictureTypes:= cfg.GetValue('picture_types', UiOps.PictureTypes);

  UiOps.MaxFileSizeToOpen:= cfg.GetValue('ui_max_size_open', UiOps.MaxFileSizeToOpen);
  UiOps.MaxFileSizeToOpen:= Min(UiOps.MaxFileSizeToOpen, 2*1024-1); //because TStream.ReadBuffer is called, it can handle only 32-bit LongInt
  UiOps.MaxFileSizeForLexer:= cfg.GetValue('ui_max_size_lexer', UiOps.MaxFileSizeForLexer);
  UiOps.MaxStatusbarMessages:= cfg.GetValue('ui_statusbar_hint_lines', UiOps.MaxStatusbarMessages);

  UiOps.AutocompleteAcpFiles:= cfg.GetValue('autocomplete_acp', UiOps.AutocompleteAcpFiles);
  UiOps.AutocompleteHtml:= cfg.GetValue('autocomplete_html', UiOps.AutocompleteHtml);
  UiOps.AutocompleteHtml_AutoClose:= cfg.GetValue('autocomplete_html_autoclose', UiOps.AutocompleteHtml_AutoClose);
  UiOps.AutocompleteHtml_Lexers:= cfg.GetValue('autocomplete_html_lexers', UiOps.AutocompleteHtml_Lexers);
  UiOps.AutocompleteCss:= cfg.GetValue('autocomplete_css', UiOps.AutocompleteCss);
  UiOps.AutocompleteCss_Lexers:= cfg.GetValue('autocomplete_css_lexers', UiOps.AutocompleteCss_Lexers);
  UiOps.AutocompleteFileURI:= cfg.GetValue('autocomplete_file_uri', UiOps.AutocompleteFileURI);
  UiOps.AutocompleteClosingDelay:= cfg.GetValue('autocomplete_closing_delay', UiOps.AutocompleteClosingDelay);

  UiOps.FindSuggestSel:= cfg.GetValue('find_suggest_sel', UiOps.FindSuggestSel);
  UiOps.FindSuggestWord:= cfg.GetValue('find_suggest_cur_word', UiOps.FindSuggestWord);
  UiOps.FindSuggestInSelection:= cfg.GetValue('find_suggest_in_selection', UiOps.FindSuggestInSelection);
  UiOps.FindCurrentWordCaseSensitive:= TUiOpsFindCaseSensitive(cfg.GetValue('find_sel_case', Ord(UiOps.FindCurrentWordCaseSensitive)));

  UiOps.FindHiddenButtons:= cfg.GetValue('find_hidden_buttons', UiOps.FindHiddenButtons);
  UiOps.FindShow_FindFirst:=     Pos('f', UiOps.FindHiddenButtons)=0;
  UiOps.FindShow_FindNext:=      Pos('n', UiOps.FindHiddenButtons)=0;
  UiOps.FindShow_FindPrev:=      Pos('p', UiOps.FindHiddenButtons)=0;
  UiOps.FindShow_ReplaceAll:=    Pos('a', UiOps.FindHiddenButtons)=0;
  UiOps.FindShow_ReplaceGlobal:= Pos('g', UiOps.FindHiddenButtons)=0;
  UiOps.FindShow_RegEx:=         Pos('R', UiOps.FindHiddenButtons)=0;
  UiOps.FindShow_CaseSens:=      Pos('S', UiOps.FindHiddenButtons)=0;
  UiOps.FindShow_WholeWords:=    Pos('W', UiOps.FindHiddenButtons)=0;
  UiOps.FindShow_Wrapped:=       Pos('O', UiOps.FindHiddenButtons)=0;
  UiOps.FindShow_InSel:=         Pos('I', UiOps.FindHiddenButtons)=0;
  UiOps.FindShow_MultiLine:=     Pos('M', UiOps.FindHiddenButtons)=0;
  UiOps.FindShow_SyntaxElements:=Pos('X', UiOps.FindHiddenButtons)=0;
  UiOps.FindShow_HiAll:=         Pos('H', UiOps.FindHiddenButtons)=0;
  UiOps.FindShow_ConfirmRep:=    Pos('C', UiOps.FindHiddenButtons)=0;
  UiOps.FindShow_RegexSubst:=    Pos('E', UiOps.FindHiddenButtons)=0;
  UiOps.FindShow_PreserveCase:=  Pos('P', UiOps.FindHiddenButtons)=0;

  UiOps.FindIndentVert:= cfg.GetValue('find_indent_vert', UiOps.FindIndentVert);
  UiOps.FindIndentHorz:= cfg.GetValue('find_indent_horz', UiOps.FindIndentHorz);
  UiOps.FindMultiLineScale:= cfg.GetValue('find_multiline_scale', UiOps.FindMultiLineScale);
  {$ifndef LCLGTK2}
  //disabled on GTK2 until we solve issue #3990
  UiOps.FindSeparateForm:= cfg.GetValue('find_separate_form', UiOps.FindSeparateForm);
  {$endif}
  UiOps.FindHiAll_MaxLines:= cfg.GetValue('find_hi_max_lines', UiOps.FindHiAll_MaxLines);
  //UiOps.FindHiAll_MoveCaret:= cfg.GetValue('find_hi_move_caret', UiOps.FindHiAll_MoveCaret);

  EditorOps.OpUndoIndentVert:= UiOps.FindIndentVert;
  EditorOps.OpUndoIndentHorz:= UiOps.FindIndentHorz;

  ShowTabsMain:= cfg.GetValue('ui_tab_show', ShowTabsMain);
  UiOps.TabsDisabled:= cfg.GetValue('ui_tab_disabled', UiOps.TabsDisabled);
  UiOps.TabVarWidth:= cfg.GetValue('ui_tab_variable_width', UiOps.TabVarWidth);
  UiOps.TabMultiline:= cfg.GetValue('ui_tab_multiline', UiOps.TabMultiline);
  if ATTabsStretchDrawEnabled then
    UiOps.TabAngled:= cfg.GetValue('ui_tab_angled', UiOps.TabAngled);
  UiOps.TabFlat:= cfg.GetValue('ui_tab_flat', UiOps.TabFlat);
  UiOps.TabWidth:= cfg.GetValue('ui_tab_size_x', UiOps.TabWidth);
  UiOps.TabWidthMin:= cfg.GetValue('ui_tab_size_x_min', UiOps.TabWidthMin);
  UiOps.TabWidthMax:= cfg.GetValue('ui_tab_size_x_max', UiOps.TabWidthMax);
  UiOps.TabHeight:= cfg.GetValue('ui_tab_size_y', UiOps.TabHeight);
  UiOps.TabHeightInner:= UiOps.TabHeight-1;
  UiOps.TabSpaceBeforeText:= cfg.GetValue('ui_tab_padding', UiOps.TabSpaceBeforeText);
  UiOps.TabPosition:= cfg.GetValue('ui_tab_position', UiOps.TabPosition);
  UiOps.TabColorFull:= cfg.GetValue('ui_tab_fullcolor', UiOps.TabColorFull);
  UiOps.TabFontScale:= cfg.GetValue('ui_tab_font_scale', UiOps.TabFontScale);
  UiOps.TabShowX:= cfg.GetValue('ui_tab_show_x', UiOps.TabShowX);
  UiOps.TabShowXSize:= cfg.GetValue('ui_tab_show_x_size', UiOps.TabShowXSize);
  UiOps.TabShowXRounded:= cfg.GetValue('ui_tab_x_rounded', UiOps.TabShowXRounded);
  UiOps.TabShowPlus:= cfg.GetValue('ui_tab_show_plus', UiOps.TabShowPlus);
  UiOps.TabDblClickClose:= cfg.GetValue('ui_tab_2click_closes', UiOps.TabDblClickClose);
  UiOps.TabNumbers:= cfg.GetValue('ui_tab_numbers', UiOps.TabNumbers);
  UiOps.TabNewNearCurrent:= cfg.GetValue('ui_tab_new_near_current', UiOps.TabNewNearCurrent);
  UiOps.TabRecentOnClose:= cfg.GetValue('ui_tab_recent_on_close', UiOps.TabRecentOnClose);
  UiOps.TabButtonLayout:= cfg.GetValue('ui_tab_button_layout', UiOps.TabButtonLayout);
  UiOps.TabPreviewFontStyle:= cfg.GetValue('ui_tab_preview_font_style', UiOps.TabPreviewFontStyle);
  UiOps.TabSwitcherDialog:= cfg.GetValue('ui_tab_switcher_dialog', UiOps.TabSwitcherDialog);

  //UiOps.ListboxCentered:= cfg.GetValue('ui_listbox_centered', UiOps.ListboxCentered);
  UiOps.ListboxSizeX:= cfg.GetValue('ui_listbox_size_x', UiOps.ListboxSizeX);
  UiOps.ListboxSizeY:= cfg.GetValue('ui_listbox_size_y', UiOps.ListboxSizeY);
  UiOps.ListboxAutoCompleteWidth:= cfg.GetValue('ui_listbox_complete_size_x', UiOps.ListboxAutoCompleteWidth);
  UiOps.ListboxAutoCompleteMaxItems:= cfg.GetValue('ui_listbox_complete_size_items', UiOps.ListboxAutoCompleteMaxItems);
  UiOps.ListboxFuzzySearch:= cfg.GetValue('ui_listbox_fuzzy', UiOps.ListboxFuzzySearch);

  UiOps.MaxHistoryEdits:= Max(0, Min(200, cfg.GetValue('ui_max_history_edits', UiOps.MaxHistoryEdits)));
  UiOps.MaxHistoryMenu:= Max(0, Min(50, cfg.GetValue('ui_max_history_menu', UiOps.MaxHistoryMenu)));
  UiOps.MaxHistoryFiles:= Max(0, Min(400, cfg.GetValue('ui_max_history_files', UiOps.MaxHistoryFiles)));

  UiOps.CmdPaletteFilterKeep:= cfg.GetValue('ui_cmdpalette_filter_keep', UiOps.CmdPaletteFilterKeep);

  UiOps.HistoryDisabledStr:= cfg.GetValue('ui_history_disabled', UiOps.HistoryDisabledStr);
  for element:= Low(element) to High(element) do
    UiOps.HistoryItems[element]:= Pos(cAppHistoryElementChar[element], UiOps.HistoryDisabledStr)=0;

  UiOps.EscapeClose:= cfg.GetValue('ui_esc_close', UiOps.EscapeClose);
  UiOps.EscapeCloseConsole:= cfg.GetValue('ui_esc_close_console', UiOps.EscapeCloseConsole);
  UiOps.EscapeCloseFinder:= cfg.GetValue('ui_esc_close_finder', UiOps.EscapeCloseFinder);
  UiOps.ConsoleWordWrap:= cfg.GetValue('ui_console_wrap', UiOps.ConsoleWordWrap);
  UiOps.InitialDir:= cfg.GetValue('ui_initial_dir', UiOps.InitialDir);
  UiOps.OpenDir:= cfg.GetValue('ui_open_dir', UiOps.OpenDir);

  UiOps.ExportHtmlNumbers:= cfg.GetValue('export_html_numbers', UiOps.ExportHtmlNumbers);
  UiOps.ExportHtmlFontName:= cfg.GetValue('export_html_font_name', UiOps.ExportHtmlFontName);
  UiOps.ExportHtmlFontSize:= Abs(cfg.GetValue('export_html_font_size', UiOps.ExportHtmlFontSize));

  UiOps.TreeAutoSync:= cfg.GetValue('ui_tree_auto_sync', UiOps.TreeAutoSync);
  UiOps.TreeTimeFill:= cfg.GetValue('ui_tree_time_fill', UiOps.TreeTimeFill);
  UiOps.TreeShowIcons:= cfg.GetValue('ui_tree_icons', UiOps.TreeShowIcons);
  UiOps.TreeShowTooltips:= cfg.GetValue('ui_tree_show_tooltips', UiOps.TreeShowTooltips);
  UiOps.TreeFilterLayout:= cfg.GetValue('ui_tree_filter_layout', UiOps.TreeFilterLayout);
  UiOps.TreeSublexers:= cfg.GetValue('ui_tree_sublexers', UiOps.TreeSublexers);
  UiOps.TreeFillMaxTime:= cfg.GetValue('ui_tree_fill_max_time', UiOps.TreeFillMaxTime);

  UiOps.PyCaretSlow:= Max(100, cfg.GetValue('py_caret_slow', UiOps.PyCaretSlow));
  UiOps.PyChangeSlow:= Max(500, cfg.GetValue('py_change_slow', UiOps.PyChangeSlow));

  UiOps.LogPluginIniting:= cfg.GetValue('py_init_log', UiOps.LogPluginIniting);
  UiOps.LogSessions:= cfg.GetValue('log_sessions', UiOps.LogSessions);
  UiOps.LogConsole:= cfg.GetValue('log_console', UiOps.LogConsole);

  UiOps.NewdocLexer:= cfg.GetValue('newdoc_lexer', UiOps.NewdocLexer);
  UiOps.NewdocEnc:= cfg.GetValue('newdoc_encoding', UiOps.NewdocEnc);
  UiOps.NewdocEnds:= cfg.GetValue('newdoc_ends', UiOps.NewdocEnds);

  UiOps.ViewerBinaryWidth:= cfg.GetValue('viewer_binary_width', UiOps.ViewerBinaryWidth);
  UiOps.ViewerNonPrintable:= cfg.GetValue('viewer_unprinted', UiOps.ViewerNonPrintable);

  UiOps.AltTooltipTime:= cfg.GetValue('ui_alt_hint_time', UiOps.AltTooltipTime);

  S:= cfg.GetValue('ui_theme', '?');
  UiOps.ThemeUi_Loaded:= S<>'?';
  if UiOps.ThemeUi_Loaded then
    UiOps.ThemeUi:= S;

  S:= cfg.GetValue('ui_theme_syntax', '?');
  UiOps.ThemeSyntax_Loaded:= S<>'?';
  if UiOps.ThemeSyntax_Loaded then
    UiOps.ThemeSyntax:= S;
end;


procedure TfmMain.DoOps_LoadOptions(const AFileName: string; var Ops: TEditorOps; AllowGlobalOps: boolean);
var
  cfg: TJsonConfig;
  NTick, NTick2: QWord;
begin
  NTick:= GetTickCount64;

  cfg:= TJsonConfig.Create(nil);
  try
    try
      cfg.Filename:= AFileName;
    except
      on E: Exception do
      begin
        MsgBadConfig(AFileName, E.Message);
        Exit
      end;
    end;

    if AllowGlobalOps then
    begin
      DoOps_LoadOptions_Global(cfg);
      DoOps_LoadOptions_Ui(cfg);
    end;

    NTick2:= GetTickCount64;
    DoOps_LoadOptions_Editor(cfg, Ops);
    if UiOps.LogConsoleDetailedStartupTime then
    begin
      NTick2:= GetTickCount64-NTick2;
      MsgLogConsole(Format('Loaded lexer config "%s": %dms', [ExtractFileName(ExtractFileDir(AFileName))+DirectorySeparator+ExtractFileName(AFileName), NTick2]));
    end;

    //don't show if we only load lexer-specific config
    if UiOps.LogConsoleDetailedStartupTime then
      if AllowGlobalOps then
      begin
        NTick:= GetTickCount64-NTick;
        MsgLogConsole(Format('Loaded configs: %dms', [NTick]));
      end;
  finally
    cfg.Free;
  end;
end;


procedure TfmMain.DoOps_LoadOptionsLexerSpecific(F: TEditorFrame; Ed: TATSynEdit);
  //
  procedure ApplyOps(const Ops: TEditorOps);
  var
    PrevTabSize: integer;
    PrevTabSizeChanged: boolean;
    PrevTabSpaces: boolean;
    PrevTabSpacesChanged: boolean;
    TempOps: TATEditorTempOptions;
  begin
    PrevTabSize:= Ed.OptTabSize;
    PrevTabSizeChanged:= F.TabSizeChanged;
    PrevTabSpaces:= Ed.OptTabSpaces;
    PrevTabSpacesChanged:= F.TabSpacesChanged;

    EditorSaveTempOptions(Ed, TempOps);
    TempOps.FontSize:= Ops.OpFontSize; //support overriding "font_size" in lexer-specific config
    DoApplyFrameOps(F, Ops, true);
    EditorRestoreTempOptions(Ed, TempOps, Ed.InitialOptions);

    if PrevTabSizeChanged then
      Ed.OptTabSize:= PrevTabSize;
    if PrevTabSpacesChanged then
      Ed.OptTabSpaces:= PrevTabSpaces;
  end;
  //
var
  FilenameDefault, FilenameUser, CurLexer: string;
  bExistsDefault, bExistsUser: boolean;
  SNewLoadedConfig: string;
begin
  if F=nil then exit;

  CurLexer:= F.LexerName[Ed];
  SNewLoadedConfig:= 'lexer:'+CurLexer;
  if FLastLoadedConfig=SNewLoadedConfig then //fix issue #4225
  begin
    ApplyOps(FLastLoadedEditorOps);
    exit;
  end;
  FLastLoadedConfig:= SNewLoadedConfig;
  FLastLoadedEditorOps:= EditorOps;

  FilenameDefault:= AppFile_LexerSpecificConfig(CurLexer, true);
  FilenameUser:= AppFile_LexerSpecificConfig(CurLexer, false);

  bExistsDefault:= FileExists(FilenameDefault);
  bExistsUser:= FileExists(FilenameUser);

  if bExistsDefault or bExistsUser then
  begin
    if bExistsDefault then
    begin
      DoOps_LoadOptions(FilenameDefault, FLastLoadedEditorOps, false);
    end;

    if bExistsUser then
    begin
      DoOps_LoadOptions(FilenameUser, FLastLoadedEditorOps, false);

      //store 'centering...' options in special way
      AppOption_SaveToStringlist(EditorOps_CenteringWidth, CurLexer, FLastLoadedEditorOps.OpCenteringWidth);
      AppOption_SaveToStringlist(EditorOps_CenteringDistFree, CurLexer, FLastLoadedEditorOps.OpCenteringForDistractionFree);
    end;

    ApplyOps(FLastLoadedEditorOps);
  end;
end;


procedure TfmMain.DoOps_DialogFont_Text;
begin
  DoOps_DialogFont(
    EditorOps.OpFontName,
    EditorOps.OpFontSize,
    'font_name'+cOptionSystemSuffix,
    'font_size'+cOptionSystemSuffix);
  DoApplyFont_Text;
end;

procedure TfmMain.DoOps_DialogFont_Ui;
begin
  DoOps_DialogFont(
    UiOps.VarFontName,
    UiOps.VarFontSize,
    OpStr_UiFontName,
    OpStr_UiFontSize);
  DoApplyFont_Ui;
end;

procedure TfmMain.DoOps_DialogFont_Output;
begin
  DoOps_DialogFont(
    UiOps.OutputFontName,
    UiOps.OutputFontSize,
    OpStr_UiFontOutputName,
    OpStr_UiFontOutputSize);
  DoApplyFont_Output;
end;

procedure TfmMain.DoOps_FontSizeChange(AIncrement: integer);
const
  cMinSize = 6;
begin
  EditorOps.OpFontSize   := Max(cMinSize, EditorOps.OpFontSize   +AIncrement);
  EditorOps.OpFontSize_i := Max(cMinSize, EditorOps.OpFontSize_i +AIncrement);
  EditorOps.OpFontSize_b := Max(cMinSize, EditorOps.OpFontSize_b +AIncrement);
  EditorOps.OpFontSize_bi:= Max(cMinSize, EditorOps.OpFontSize_bi+AIncrement);
  DoApplyFont_Text;
  DoPyEvent_EdState(nil, EDSTATE_ZOOM);
end;

procedure TfmMain.DoOps_FontSizeReset;
begin
  EditorOps.OpFontSize:= EditorOps.OpFontSize_original;
  EditorOps.OpFontSize_i:= EditorOps.OpFontSize_original_i;
  EditorOps.OpFontSize_b:= EditorOps.OpFontSize_original_b;
  EditorOps.OpFontSize_bi:= EditorOps.OpFontSize_original_bi;
  DoApplyFont_Text;
  DoPyEvent_EdState(nil, EDSTATE_ZOOM);
end;


procedure TfmMain.DoOps_DialogFont(
  var OpName: string; var OpSize: integer;
  const AConfigStrName, AConfigStrSize: string);
var
  dlg: TFontDialog;
  cfg: TJsonConfig;
  fn: string;
  bSizeValid: boolean;
begin
  fn:= AppFile_OptionsUser;
  if not FileExists(fn) then
  begin
    AppCreateFileJSON(fn);
    if not FileExists(fn) then exit;
  end;

  dlg:= TFontDialog.Create(nil);
  try
    dlg.Font.Name:= OpName;
    dlg.Font.Size:= OpSize;
    if not dlg.Execute then exit;
    OpName:= dlg.Font.Name;
    bSizeValid:= dlg.Font.Size>=7; //workaround for issue #3900: Win10 returns font size=0
    if bSizeValid then
      OpSize:= dlg.Font.Size;
  finally
    FreeAndNil(dlg);
  end;

  cfg:= TJSONConfig.Create(nil);
  try
    try
      cfg.Formatted:= true;
      cfg.Filename:= fn;
      cfg.SetValue(AConfigStrName, OpName);
      if bSizeValid then
        cfg.SetValue(AConfigStrSize, OpSize);
    except
    end;
  finally
    try
      cfg.Free;
    except
      MsgBox(msgCannotSaveUserConf, MB_OK or MB_ICONERROR);
    end;
  end;
end;

procedure TfmMain.DoOps_LoadOptionsAndApplyAll;
// called by command "settings: reload/apply config"
var
  bDisFree: boolean;
begin
  bDisFree:= ShowDistractionFree;
  FLastLoadedConfig:= 'user';
  DoOps_LoadOptions(AppFile_OptionsUser, EditorOps, true);
  DoApplyAllOps;

  MsgStatus(msgStatusReadingOps);
  DoPyEvent_AppState(APPSTATE_CONFIG_REREAD);

  if bDisFree then
    SetShowDistractionFree_Forced;
end;

