(*
This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at http://mozilla.org/MPL/2.0/.

Copyright (c) Alexey Torgashin
*)
{$ifdef nn}begin end;{$endif}

procedure TfmMain.DoOps_SaveOptionBool(const APath: string; AValue: boolean);
var
  c: TJSONConfig;
begin
  c:= TJSONConfig.Create(nil);
  try
    try
      c.Formatted:= true; //needs last at__jsonconf.pp
      c.Filename:= AppFile_OptionsUser;
    except
      exit;
    end;

    c.SetValue(APath, AValue);
  finally
    c.Free;
  end;
end;

procedure TfmMain.DoOps_SaveThemes;
var
  c: TJSONConfig;
begin
  c:= TJSONConfig.Create(nil);
  try
    try
      c.Formatted:= true;
      c.FileName:= AppFile_OptionsUser;
    except
      exit
    end;
    c.SetValue('/ui_theme', UiOps.ThemeUi);
    c.SetValue('/ui_theme_syntax', UiOps.ThemeSyntax);
  finally
    c.Free;
  end;
end;

procedure TfmMain.DoOps_SaveHistory;
var
  c: TJSONConfig;
  bMax: boolean;
begin
  c:= TJSONConfig.Create(nil);
  try
    try
      c.Formatted:= true; //needs last at__jsonconf.pp
      c.Filename:= AppFile_History;
    except
      exit;
    end;

    ////form
    if not ShowFullscreen then
    begin
      bMax:= WindowState=wsMaximized;
      c.SetValue('/dialog/max', bMax);
      if bMax then
        c.SetValue('/dialog/max_mon', Monitor.MonitorNum);

      if not IsWindowMaximizedOrFullscreen then
      begin
        c.SetValue('/dialog/posx', Left);
        c.SetValue('/dialog/posy', Top);
        c.SetValue('/dialog/sizex', Width);
        c.SetValue('/dialog/sizey', Height);
      end;

      if AppPanels[cPaneSide].Floating then
      begin
        c.SetValue('/dialog/fl_side/posx',  AppPanels[cPaneSide].FormFloat.Left);
        c.SetValue('/dialog/fl_side/posy',  AppPanels[cPaneSide].FormFloat.Top);
        c.SetValue('/dialog/fl_side/sizex', AppPanels[cPaneSide].FormFloat.Width);
        c.SetValue('/dialog/fl_side/sizey', AppPanels[cPaneSide].FormFloat.Height);
      end;

      if AppPanels[cPaneOut].Floating then
      begin
        c.SetValue('/dialog/fl_btm/posx',  AppPanels[cPaneOut].FormFloat.Left);
        c.SetValue('/dialog/fl_btm/posy',  AppPanels[cPaneOut].FormFloat.Top);
        c.SetValue('/dialog/fl_btm/sizex', AppPanels[cPaneOut].FormFloat.Width);
        c.SetValue('/dialog/fl_btm/sizey', AppPanels[cPaneOut].FormFloat.Height);
      end;

      if FloatGroups then
      begin
        c.SetValue('/dialog/fl_g1/posx',  FFormFloatGroups1.Left);
        c.SetValue('/dialog/fl_g1/posy',  FFormFloatGroups1.Top);
        c.SetValue('/dialog/fl_g1/sizex', FFormFloatGroups1.Width);
        c.SetValue('/dialog/fl_g1/sizey', FFormFloatGroups1.Height);

        c.SetValue('/dialog/fl_g2/posx',  FFormFloatGroups2.Left);
        c.SetValue('/dialog/fl_g2/posy',  FFormFloatGroups2.Top);
        c.SetValue('/dialog/fl_g2/sizex', FFormFloatGroups2.Width);
        c.SetValue('/dialog/fl_g2/sizey', FFormFloatGroups2.Height);

        c.SetValue('/dialog/fl_g3/posx',  FFormFloatGroups3.Left);
        c.SetValue('/dialog/fl_g3/posy',  FFormFloatGroups3.Top);
        c.SetValue('/dialog/fl_g3/sizex', FFormFloatGroups3.Width);
        c.SetValue('/dialog/fl_g3/sizey', FFormFloatGroups3.Height);
      end;

      c.SetValue('show_sidepanel', AppPanels[cPaneSide].Visible);
      c.SetValue('show_bottom', AppPanels[cPaneOut].Visible);

      c.SetValue('float_side', AppPanels[cPaneSide].Floating);
      c.SetValue('float_bottom', AppPanels[cPaneOut].Floating);

      with AppPanels[cPaneSide] do
        if not Floating then
          c.SetValue('size_side', PanelSize);

      with AppPanels[cPaneOut] do
        if not Floating then
          c.SetValue('size_bottom', PanelSize);

      c.SetValue('tab_left', AppPanels[cPaneSide].LastActivePanel);
    end;

    ////recents
    c.SetValue('/recent_files', FListRecents);

    ////find dialog
    if Assigned(fmFind) then
    begin
      c.SetValue('/list_find', fmFind.edFind.Items);
      c.SetValue('/list_replace', fmFind.edRep.Items);
      if fmFind.IsNarrow then
      begin
        c.SetValue('/find/pos_x', fmFind.Left);
        c.SetValue('/find/pos_y', fmFind.Top);
        c.SetValue('/find/size_x', fmFind.Width);
      end;
      c.SetValue('/find/multi_line', fmFind.IsMultiLine);
    end;

    c.SetValue('/list_console', fmConsole.EdInput.Items);

    ////finder
    c.SetValue('/finder/regex', FFinder.OptRegex);
    c.SetValue('/finder/case', FFinder.OptCase);
    c.SetValue('/finder/words', FFinder.OptWords);
    c.SetValue('/finder/back', FFinder.OptBack);
    c.SetValue('/finder/confirm', FFinder.OptConfirmReplace);
    c.SetValue('/finder/wrap', FFinder.OptWrapped);
    c.SetValue('/finder/insel', FFinder.OptInSelection);
    c.SetValue('/finder/tokens', Ord(FFinder.OptTokens));

    ////state
    c.SetValue('lang', UiOps.LangName);
    c.SetValue('session', UTF8Decode(IfThen(UiOps.ReopenSession, FSessionName, '')));
    c.SetValue('last_cmd', FLastSelectedCommand);

    DoOps_SaveHistory_GroupView(c);
  finally
    c.Free;
  end;

  if UiOps.ReopenSession then
    DoOps_SaveSession(GetSessionFilename);
end;


function TfmMain.DoOps_SaveSession(fn_session: string): boolean;
var
  c: TJSONConfig;
  F: TEditorFrame;
  Gr: TATGroups;
  Pages: TATPages;
  nLocalGroup, nGlobalGroup, nTab, i: integer;
  path, value: string;
  bModified: boolean;
begin
  Result:= false;
  if fn_session='' then exit;
  DeleteFile(fn_session);

  //MsgLogConsole('Saving session: '+ExtractFileName(fn_session));

  c:= TJSONConfig.Create(nil);
  try
    try
      c.Formatted:= true;
      c.Filename:= fn_session;
    except
      MsgBadConfig(fn_session);
      exit;
    end;

    DoOps_SaveHistory_GroupView(c);

    for i:= Low(TATGroupsNums) to High(TATGroupsNums) do
      c.SetValue('/tab_index/g'+Inttostr(i), Groups.Pages[i].Tabs.TabIndex);
    c.SetValue('/tab_index/g_active', Groups.PagesIndexOf(Groups.PagesCurrent));

    c.SetValue('/panels/side_show', AppPanels[cPaneSide].Visible);
    c.SetValue('/panels/side_panel', AppPanels[cPaneSide].LastActivePanel);

    with AppPanels[cPaneSide] do
      if not Floating then
        c.SetValue('/panels/side_size', PanelSize);

    c.SetValue('/panels/bottom_show', AppPanels[cPaneOut].Visible);
    c.SetValue('/panels/bottom_panel', AppPanels[cPaneOut].LastActivePanel);

    with AppPanels[cPaneOut] do
      if not Floating then
        c.SetValue('/panels/bottom_size', PanelSize);

    for i:= 0 to FrameCount-1 do
    begin
      F:= Frames[i];
      path:= '/'+Format('%3.3d', [i]);

      GetFrameLocation(F, Gr, Pages, nLocalGroup, nGlobalGroup, nTab);
      bModified:= F.Ed1.Modified or ((F.GetFileName(F.Ed1)='') and not F.IsEmpty);

      c.SetValue(path+'/modified', bModified);
      c.SetValue(path+'/file', UTF8Decode(SCollapseHomeDirInFilename(F.Filename)));
      c.SetValue(path+'/tab_title', UTF8Decode(F.TabCaption));
      c.SetValue(path+'/group', nLocalGroup+1); //support old sessions with 1-based index

      if F.IsText then value:= ''
      else
      if F.IsBinary then value:= 'bin,'+IntToStr(Ord(F.Binary.Mode))
      else
      if F.IsPicture then value:= 'pic';
      c.SetValue(path+'/kind', UTF8Decode(value));

      if bModified then
      begin
        c.SetValue(path+'/text', F.Editor.Text);
        if F.FileName<>'' then
          c.SetValue(path+'/file_date', FormatFileDateAsNiceString(F.FileName));
      end;

      F.DoSaveHistoryEx(F.Ed1, c, path);
    end;

    Result:= true;
  finally
    try
      c.Free; //saves json file
    except
      //for incorrect sess folder
    end;
  end;
end;

function TfmMain.DoOps_LoadSession(fn_session: string): boolean;
var
  c: TJSONConfig;
  F: TEditorFrame;
  list: TStringlist;
  sFilename, sTabTitle, sKind, sOpenOptions, item_path: string;
  nCountFiles, nCountFilesModified: integer;
  nIndex, i: integer;
  Pages: TATPages;
  bModified: boolean;
  sText: UnicodeString;
  sDateSaved, sDateCurrent: string;
const
  SOptionPassive = '/passive /nonear';
begin
  Result:= false;
  if fn_session='' then exit;
  if not FileExists(fn_session) then exit;

  if not DoFileCloseAll(true) then exit;

  nCountFiles:= 0;
  nCountFilesModified:= 0;

  c:= TJSONConfig.Create(nil);
  list:= TStringList.Create;
  try
    try
      c.Filename:= fn_session;
    except
      MsgBadConfig(fn_session);
      exit;
    end;

    DoOps_LoadHistory_GroupView(c); //reads to variables
    DoApplyInitialGroupSizes; //applies that variables

    c.EnumSubKeys('/', list);
    list.Sort;

    for i:= 0 to list.Count-1 do
    begin
      item_path:= '/'+list[i]+'/';
      sFilename:= UTF8Encode(c.GetValue(item_path+'file', ''));
      sFilename:= SExpandHomeDirInFilename(sFilename);
      sTabTitle:= UTF8Encode(c.GetValue(item_path+'tab_title', ''));
      sText:= UTF8Encode(c.GetValue(item_path+'text', ''));
      sKind:= UTF8Encode(c.GetValue(item_path+'kind', ''));
      bModified:= c.GetValue(item_path+'modified', false);

      //support old sessions with 1-based group index
      nIndex:= c.GetValue(item_path+'group', 1)-1;

      //check, if text saved in session, but file was changed outside
      if bModified and (sFilename<>'') and FileExistsUTF8(sFileName) then
      begin
        sDateSaved:= c.GetValue(item_path+'file_date', '');
        sDateCurrent:= FormatFileDateAsNiceString(sFilename);
        if sDateSaved<>sDateCurrent then
          if MsgBox(
            msgConfirmFileChangedOutside+#10+sFilename+#10#10+
            msgConfirmReloadIt+#10+
            msgConfirmReloadItHotkeysSess,
            MB_YESNO+MB_ICONQUESTION)=ID_YES then
           bModified:= false;
      end;

      if (nIndex<Low(TATGroupsNums)) or (nIndex>High(TATGroupsNums)) then
        nIndex:= Low(TATGroupsNums);
      Pages:= Groups.Pages[nIndex];

      if SBeginsWith(sKind, 'bin') then
        sOpenOptions:= '/view-binary'
      else
        sOpenOptions:= '';

      F:= nil;
      //modified tabs must restore text, and set filename (if not empty)
      if bModified then
      begin
        Inc(nCountFilesModified);
        if (FrameCount>0) and Frames[0].IsEmpty then
          F:= Frames[0]
        else
          F:= DoFileOpen('', '', Pages, SOptionPassive);
        if sFilename<>'' then
        begin
          F.FileName:= sFilename;
          F.TabCaption:= ExtractFileName(sFilename);
        end;
        F.Editor.Text:= sText;
        F.Editor.Modified:= bModified;
        if sTabTitle<>'' then
          F.TabCaption:= sTabTitle;
        F.UpdateModified(F.Editor);
      end
      else
      //unmodified+named tabs must load file again
      if FileExistsUTF8(sFilename) then
      begin
        Inc(nCountFiles);
        F:= DoFileOpen(sFilename, '', Pages, sOpenOptions+SOptionPassive);
      end;

      if Assigned(F) then
      begin
        F.NotInRecents:= true;
      end;
    end;

    //restore tab-index in all groups
    for i:= Low(TATGroupsNums) to High(TATGroupsNums) do
    begin
      nIndex:= c.GetValue('/tab_index/g'+IntToStr(i), -1);
      if (nIndex<0) or (nIndex>=Groups.Pages[i].Tabs.TabCount) then
        nIndex:= 0;
      Groups.Pages[i].Tabs.TabIndex:= nIndex;
    end;

    //restore group-index
    nIndex:= c.GetValue('/tab_index/g_active', Low(TATGroupsNums));
    if (nIndex>=Low(TATGroupsNums)) and (nIndex<=High(TATGroupsNums)) then
    begin
      Groups.PagesCurrent:= Groups.Pages[nIndex];
      with Groups.PagesCurrent do
        Tabs.TabIndex:= Tabs.TabIndex;
    end;

    //clear tick values, to CudaExt tab switcher feels good
    //(it shows frames with tick=0 in second listbox)
    for i:= 0 to FrameCount-1 do
    begin
      F:= Frames[i];
      if F.Visible then
        F.ActivationTime:= GetTickCount64
      else
        F.ActivationTime:= 0;
    end;

    //restore panel sizes
    AppPanels[cPaneSide].Visible:= c.GetValue('/panels/side_show', AppPanels[cPaneSide].Visible);
    AppPanels[cPaneOut].Visible:= c.GetValue('/panels/bottom_show', AppPanels[cPaneOut].Visible);

    with AppPanels[cPaneSide] do
      PanelSize:= Min(c.GetValue('/panels/side_size', PanelSize), ClientWidth div 2);

    with AppPanels[cPaneOut] do
      PanelSize:= Min(c.GetValue('/panels/bottom_size', PanelSize), ClientHeight div 2);

    SText:= c.GetValue('/panels/side_panel', '');
    if AppPanels[cPaneSide].Visible and (SText<>'') then
      AppPanels[cPaneSide].UpdatePanels(SText, false, true);

    SText:= c.GetValue('/panels/bottom_panel', '');
    if AppPanels[cPaneOut].Visible and (SText<>'') then
      AppPanels[cPaneOut].UpdatePanels(SText, false, true);

    Result:= true;
  finally
    list.Free;
    c.Free;
  end;

  sText:= 'Loaded session: '+ExtractFileName(fn_session)+', '+IntToStr(nCountFiles)+' file(s)';
  if nCountFilesModified>0 then
    sText+= ' + '+IntToStr(nCountFilesModified)+' modified';
  MsgLogConsole(sText);

  DoPyEvent(nil, cEventOnState, [AppVariant(APPSTATE_SESSION_LOAD)]);
end;

procedure TfmMain.DoOps_LoadHistory_GroupView(c: TJsonConfig);
var
  Sep: TATStringSeparator;
  i: integer;
begin
  FOption_GroupMode:= TATGroupsMode(c.GetValue('groups', Ord(gmOne)));

  Sep.Init(c.GetValue('/gr/panel', ''));
  Sep.GetItemInt(FOption_GroupPanelSize.x, 50);
  Sep.GetItemInt(FOption_GroupPanelSize.y, 50);

  Sep.Init(c.GetValue('/gr/pages', ''));
  for i in TATGroupsNums do
  begin
    Sep.GetItemInt(FOption_GroupSizes[i].x, 50);
    Sep.GetItemInt(FOption_GroupSizes[i].y, 50);
  end;
end;

procedure TfmMain.DoOps_SaveHistory_GroupView(c: TJsonConfig);
var
  SizePanel: TPoint;
  SizePage: TATGroupsPoints;
  S: string;
  i: integer;
begin
  c.SetValue('groups', Ord(Groups.Mode));

  Groups.GetSizes(SizePanel, SizePage);

  S:= '';
  for i in TATGroupsNums do
    S+= Format('%d,%d,', [SizePage[i].x, SizePage[i].y]);

  c.SetValue('/gr/panel', Format('%d,%d', [SizePanel.x, SizePanel.y]));
  c.SetValue('/gr/pages', S);
end;


procedure DoOps_LoadFormRect(c: TJSONConfig; const path: string; var R: TRect);
begin
  R.Left:= c.GetValue(path+'/posx', R.Left);
  R.Top:= c.GetValue(path+'/posy', R.Top);
  R.Width:= c.GetValue(path+'/sizex', R.Width);
  R.Height:= c.GetValue(path+'/sizey', R.Height);

  FixRectPositionToDesktop(R);
end;


procedure TfmMain.DoOps_LoadHistory;
//this should be called in OnCreate, to fix window flickering if window
//was maximized (Win32).
var
  c: TJSONConfig;
begin
  c:= TJSONConfig.Create(nil);
  try
    try
      c.Filename:= AppFile_History;
    except
      exit;
    end;

    DoOps_LoadFormRect(c, '/dialog', FBoundsMain);
    DoOps_LoadFormRect(c, '/dialog/fl_side', AppPanels[cPaneSide].FormFloatBounds);
    DoOps_LoadFormRect(c, '/dialog/fl_btm', AppPanels[cPaneOut].FormFloatBounds);
    DoOps_LoadFormRect(c, '/dialog/fl_g1', FBoundsFloatGroups1);
    DoOps_LoadFormRect(c, '/dialog/fl_g2', FBoundsFloatGroups2);
    DoOps_LoadFormRect(c, '/dialog/fl_g3', FBoundsFloatGroups3);

    //note: BoundsRect change works very long (150-200ms) on Linux gtk2
    BoundsRect:= FBoundsMain;

    FLastMaximized:= c.GetValue('/dialog/max', false);
    FLastMaximizedMonitor:= c.GetValue('/dialog/max_mon', 0);

    ////recents
    c.GetValue('/recent_files', FListRecents, '');
    c.GetValue('/list_console', fmConsole.EdInput.Items, '');
    UpdateMenuRecent(nil, '');

    ////finder
    FFinder.OptRegex:= c.GetValue('/finder/regex', false);
    FFinder.OptCase:= c.GetValue('/finder/case', false);
    FFinder.OptWords:= c.GetValue('/finder/words', false);
    FFinder.OptBack:= c.GetValue('/finder/back', false);
    FFinder.OptConfirmReplace:= c.GetValue('/finder/confirm', false);
    FFinder.OptWrapped:= c.GetValue('/finder/wrap', false);
    FFinder.OptInSelection:= c.GetValue('/finder/insel', false);
    FFinder.OptTokens:= TATFinderTokensAllowed(c.GetValue('/finder/tokens', 0));

    ////state
    AppPanels[cPaneSide].Visible:= c.GetValue('show_sidepanel', true);
    AppPanels[cPaneOut].Visible:= c.GetValue('show_bottom', false);

    AppPanels[cPaneSide].PanelSize:= Min(c.GetValue('size_side', 200), ClientWidth div 2);
    AppPanels[cPaneOut].PanelSize:= Min(c.GetValue('size_bottom', 200), ClientHeight div 2);

    AppPanels[cPaneSide].Floating:= c.GetValue('float_side', false);
    AppPanels[cPaneOut].Floating:= c.GetValue('float_bottom', false);

    //note: reading 'theme_ui', 'theme_syntax' from history is deprecated in 2019.09
    //now it's read from user.json
    if UiOps.ThemeUi_Loaded then
      ThemeUi:= UiOps.ThemeUi
    else
      ThemeUi:= c.GetValue('theme_ui', '');

    if UiOps.ThemeSyntax_Loaded then
      ThemeSyntax:= UiOps.ThemeSyntax
    else
      ThemeSyntax:= c.GetValue('theme_syntax', '');

    UiOps.LangName:= c.GetValue('lang', '');
    if UiOps.LangName<>'' then
      DoLocalize;

    if FOption_AllowSession then
    begin
      FSessionName:= UTF8Encode(c.GetValue('session', UTF8Decode(FSessionName)));
      FLastSelectedCommand:= c.GetValue('last_cmd', FLastSelectedCommand);
      DoOps_LoadHistory_GroupView(c);
    end;

    FOption_SidebarTab:= c.GetValue('tab_left', '');
  finally
    c.Free;
  end;
end;


procedure TfmMain.DoApplyInitialSidebarPanel;
var
  S: string;
begin
  S:= FOption_SidebarTab;
  //if plugin didnt load, caption not valid
  if AppPanels[cPaneSide].CaptionToButtonIndex(S)<0 then
    S:= msgPanelTree_Init;

  if (S<>'-') and (S<>'') then
    if AppPanels[cPaneSide].Visible then
      AppPanels[cPaneSide].UpdatePanels(S, false, true);
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
  Groups.Mode:= FOption_GroupMode;
  Groups.SetSizes(FOption_GroupPanelSize, FOption_GroupSizes);
  Groups.SaveSplitPos; //apply current sizes to internal Groups vars
end;

const
  cDefaultKeymapFile: string =
    '{ "cuda_comments,cmt_toggle_line_body": {"name": "plugin: Comments: Toggle line comment, at non-space char", "s1": ["Ctrl+/"], "s2": [] } }';

procedure TfmMain.DoOps_LoadKeymap(AUseCache: boolean);
var
  F: TEditorFrame;
  CurLexer: string;
begin
  //func called few times before OnShow because of OnTabFocus, disable these
  if not FAllowLoadKeymap then exit;

  if not FileExists(AppFile_Hotkeys) then
    DoWriteStringToFile(AppFile_Hotkeys, cDefaultKeymapFile);

  AppKeymap_ApplyUndoList(FKeymapUndoList);
  DoOps_LoadKeymapFrom(AppFile_Hotkeys, nil);
  FKeymapUndoList.Clear;

  F:= CurrentFrame;
  if F=nil then exit;

  CurLexer:= F.LexerName[F.Editor];
  if AUseCache then
    //dont load lexer-specific cfg if tab changed, and same lexer active
    if FLastLexerOfKeymap=CurLexer then exit;
  FLastLexerOfKeymap:= CurLexer;

  DoOps_LoadKeymapFrom(
    GetAppKeymap_LexerSpecificConfig(FLastLexerOfKeymap),
    FKeymapUndoList);
end;


procedure TfmMain.DoOps_LoadKeymapFrom(const AFilenameKeymap: string;
  AUndoList: TATKeymapUndoList);
var
  c: TJSONConfig;
  slist, skeys: TStringList;
  //
  procedure DoReadConfigToKeys(const path: string; var keys: TATKeyArray);
  var
    j: integer;
  begin
    FillChar(keys, SizeOf(keys), 0);
    c.GetValue(path, skeys, '');
    for j:= 0 to skeys.count-1 do
      if skeys[j]<>'' then
        keys.Data[j]:= TextToShortCut(skeys[j]);
  end;
  //
var
  UndoItem: TATKeymapUndoItem;
  StrId: string;
  ncmd, nitem, i: integer;
begin
  c:= TJSONConfig.Create(nil);
  slist:= TStringList.Create;
  skeys:= TStringList.Create;

  try
    try
      c.Formatted:= true;
      c.Filename:= AFilenameKeymap;
    except
      exit;
    end;

    c.EnumSubKeys('/', slist);
    for i:= 0 to slist.count-1 do
    begin
      StrId:= slist[i];
      ncmd:= DoOps_HotkeyStringId_To_CommandCode(StrId);
      if ncmd<0 then Continue;

      nitem:= AppKeymap.IndexOf(ncmd);
      if nitem<0 then Continue;

      if Assigned(AUndoList) then
      begin
        UndoItem:= TATKeymapUndoItem.Create;
        UndoItem.StrId:= StrId;
        UndoItem.KeyArray1:= AppKeymap[nitem].Keys1;
        UndoItem.KeyArray2:= AppKeymap[nitem].Keys2;
        AUndoList.Add(UndoItem);
      end;

      DoReadConfigToKeys(StrId+'/s1', AppKeymap[nitem].Keys1);
      DoReadConfigToKeys(StrId+'/s2', AppKeymap[nitem].Keys2);
    end;
  finally
    skeys.Free;
    slist.Free;
    c.Free;
  end;
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

  if FileExistsUTF8(fn) then
  try
    DoOps_LoadOptions(fn, EditorOps);
    DoApplyAllOps;
  finally
    DeleteFileUTF8(fn);
  end;
end;


procedure TfmMain.DoOps_LoadOptions_Global(c: TJSONConfig);
var
  List: TStringList;
  s: string;
begin
  List:= TStringList.Create;
  try
    c.EnumValues('/plugin_groups', List);
    for s in List do
      AppConfig_PGroups.Add(s, c.GetValue('/plugin_groups/'+s, ''));

    c.EnumValues('/detect', List);
    for s in List do
      AppConfig_Detect.Add(s, c.GetValue('/detect/'+s, ''));

    c.EnumValues('/detect_line', List);
    for s in List do
      AppConfig_DetectLine.Add(s, c.GetValue('/detect_line/'+s, ''));
  finally
    FreeAndNil(List);
  end;
end;

procedure TfmMain.DoOps_LoadOptions_Editor(c: TJSONConfig; var Op: TEditorOps);
begin
  Op.OpFontName:= FixFontMonospaced(UTF8Encode(c.GetValue(str_FontName, UTF8Decode(Op.OpFontName))));
  Op.OpFontName_i:= FixFontMonospaced(UTF8Encode(c.GetValue(str_FontName_i, UTF8Decode(Op.OpFontName_i))));
  Op.OpFontName_b:= FixFontMonospaced(UTF8Encode(c.GetValue(str_FontName_b, UTF8Decode(Op.OpFontName_b))));
  Op.OpFontName_bi:= FixFontMonospaced(UTF8Encode(c.GetValue(str_FontName_bi, UTF8Decode(Op.OpFontName_bi))));

  Op.OpFontSize:= Abs(c.GetValue(str_FontSize, Op.OpFontSize)); //Abs() for macOS
  Op.OpFontSize_i:= Abs(c.GetValue(str_FontSize_i, Op.OpFontSize_i));
  Op.OpFontSize_b:= Abs(c.GetValue(str_FontSize_b, Op.OpFontSize_b));
  Op.OpFontSize_bi:= Abs(c.GetValue(str_FontSize_bi, Op.OpFontSize_bi));
  Op.OpFontSize_original:= Op.OpFontSize;
  Op.OpFontSize_original_i:= Op.OpFontSize_i;
  Op.OpFontSize_original_b:= Op.OpFontSize_b;
  Op.OpFontSize_original_bi:= Op.OpFontSize_bi;

  Op.OpFontQuality:= TFontQuality(c.GetValue(str_FontQuality, Ord(Op.OpFontQuality)));
  Op.OpFontLigatures:= c.GetValue(str_FontLigatures, Op.OpFontLigatures);

  OptCharSizeProportional:= c.GetValue('unicode_proportional', OptCharSizeProportional);
  OptCharSizeWideAllowed:= c.GetValue('unicode_wide_allowed', OptCharSizeWideAllowed);
  OptCharScaleFullWidth:= c.GetValue('unicode_full_width', OptCharScaleFullWidth);

  Op.OpSpacingY:= c.GetValue('spacing_y', Op.OpSpacingY);

  Op.OpTabSize:= c.GetValue('tab_size', Op.OpTabSize);
  Op.OpTabSpaces:= c.GetValue('tab_spaces', Op.OpTabSpaces);
  Op.OpTabMaxPosExpanded:= c.GetValue('tab_max_pos_expanded', Op.OpTabMaxPosExpanded);

  Op.OpOverwriteSel:= c.GetValue('ovr_sel', Op.OpOverwriteSel);
  Op.OpOverwriteOnPaste:= c.GetValue('ovr_on_paste', Op.OpOverwriteOnPaste);

  Op.OpUnderlineColorFiles:= c.GetValue('underline_color_files', Op.OpUnderlineColorFiles);
  Op.OpUnderlineColorSize:= c.GetValue('underline_color_size', Op.OpUnderlineColorSize);

  Op.OpLinks:= c.GetValue('links_hilite', Op.OpLinks);
  Op.OpLinksRegex:= c.GetValue('links_regex', Op.OpLinksRegex);

  Op.OpGutterShow:= c.GetValue('gutter_show', Op.OpGutterShow);
  Op.OpGutterFold:= c.GetValue('gutter_fold', Op.OpGutterFold);
  Op.OpGutterFoldAlways:= c.GetValue('gutter_fold_always', Op.OpGutterFoldAlways);
  Op.OpGutterFoldIcons:= c.GetValue('gutter_fold_icons', Op.OpGutterFoldIcons);
  Op.OpGutterBookmarks:= c.GetValue('gutter_bookmarks', Op.OpGutterBookmarks);

  Op.OpNumbersShow:= c.GetValue('numbers_show', Op.OpNumbersShow);
  //Op.OpNumbersFontSize:= c.GetValue('numbers_font_size', Op.OpNumbersFontSize);
  Op.OpNumbersStyle:= c.GetValue('numbers_style', Op.OpNumbersStyle);
  Op.OpNumbersForCarets:= c.GetValue('numbers_for_carets', Op.OpNumbersForCarets);
  Op.OpNumbersCenter:= c.GetValue('numbers_center', Op.OpNumbersCenter);

  Op.OpRulerShow:= c.GetValue('ruler_show', Op.OpRulerShow);
  Op.OpRulerNumeration:= c.GetValue('ruler_numeration', Op.OpRulerNumeration);
  Op.OpRulerMarkCaret:= c.GetValue('ruler_mark_caret', Op.OpRulerMarkCaret);
  //Op.OpRulerTextIndent:= c.GetValue('ruler_text_indent', Op.OpRulerTextIndent);

  Op.OpMinimapShow:= c.GetValue('minimap_show', Op.OpMinimapShow);
  Op.OpMinimapShowSelAlways:= c.GetValue('minimap_sel_always', Op.OpMinimapShowSelAlways);
  Op.OpMinimapShowSelBorder:= c.GetValue('minimap_sel_border', Op.OpMinimapShowSelBorder);
  Op.OpMinimapCharWidth:= c.GetValue('minimap_char_width', Op.OpMinimapCharWidth);
  Op.OpMinimapAtLeft:= c.GetValue('minimap_at_left', Op.OpMinimapAtLeft);
  Op.OpMinimapTooltipShow:= c.GetValue('minimap_tooltip_show', Op.OpMinimapTooltipShow);
  Op.OpMinimapTooltipLineCount:= c.GetValue('minimap_tooltip_line_count', Op.OpMinimapTooltipLineCount);
  Op.OpMinimapTooltipWidth:= c.GetValue('minimap_tooltip_width', Op.OpMinimapTooltipWidth);

  Op.OpMicromapShow:= c.GetValue('micromap_show', Op.OpMicromapShow);

  Op.OpMarginFixed:= c.GetValue('margin', Op.OpMarginFixed);
  Op.OpMarginString:= c.GetValue('margin_string', Op.OpMarginString);
  Op.OpStaplesStyle:= c.GetValue('staples_style', Op.OpStaplesStyle);
  Op.OpStaplesProps:= c.GetValue('staples_props', Op.OpStaplesProps);

  Op.OpUnprintedShow:= c.GetValue('unprinted_show', Op.OpUnprintedShow);
  Op.OpUnprintedContent:= c.GetValue('unprinted_content', Op.OpUnprintedContent);

  Op.OpUnprintedReplaceSpec:= c.GetValue('unprinted_replace_spec', Op.OpUnprintedReplaceSpec);
  Op.OpUnprintedReplaceToCode:= c.GetValue('unprinted_replace_to_code', Op.OpUnprintedReplaceToCode);

  Op.OpUnprintedTabArrowLen:= c.GetValue('unprinted_tab_len', Op.OpUnprintedTabArrowLen);
  Op.OpUnprintedSpaceDotScale:= c.GetValue('unprinted_space_dot_scale', Op.OpUnprintedSpaceDotScale);
  //Op.OpUnprintedEndDotScale:= c.GetValue('unprinted_end_dot_scale', Op.OpUnprintedEndDotScale);
  Op.OpUnprintedEndFontScale:= c.GetValue('unprinted_end_font_scale', Op.OpUnprintedEndFontScale);
  Op.OpUnprintedTabPointerScale:= c.GetValue('unprinted_tab_ptr_scale', Op.OpUnprintedTabPointerScale);

  Op.OpBracketHilite:= c.GetValue('bracket_highlight', Op.OpBracketHilite);
  Op.OpBracketSymbols:= c.GetValue('bracket_symbols', op.OpBracketSymbols);
  Op.OpBracketDistance:= c.GetValue('bracket_distance', op.OpBracketDistance);

  Op.OpWrapMode:= c.GetValue('wrap_mode', Op.OpWrapMode);
  Op.OpWrapIndented:= c.GetValue('wrap_indented', Op.OpWrapIndented);
  Op.OpWrapEnabledMaxLines:= c.GetValue('wrap_enabled_max_lines', Op.OpWrapEnabledMaxLines);

  Op.OpUndoLimit:= c.GetValue('undo_limit', Op.OpUndoLimit);
  Op.OpUndoGrouped:= c.GetValue('undo_grouped', Op.OpUndoGrouped);
  Op.OpUndoAfterSave:= c.GetValue('undo_after_save', Op.OpUndoAfterSave);

  Op.OpCaretBlinkTime:= c.GetValue('caret_blink_time', Op.OpCaretBlinkTime);
  Op.OpCaretBlinkEn:= c.GetValue('caret_blink_en', Op.OpCaretBlinkEn);
  Op.OpCaretViewNormal:= c.GetValue('caret_view', Op.OpCaretViewNormal);
  Op.OpCaretViewOverwrite:= c.GetValue('caret_view_ovr', Op.OpCaretViewOverwrite);
  Op.OpCaretViewReadonly:= c.GetValue('caret_view_ro', Op.OpCaretViewReadonly);
  Op.OpCaretVirtual:= c.GetValue('caret_after_end', Op.OpCaretVirtual);
  Op.OpCaretMulti:= c.GetValue('caret_multi', Op.OpCaretMulti);
  Op.OpCaretAfterPasteColumn:= c.GetValue('caret_after_paste_column', Op.OpCaretAfterPasteColumn);
  Op.OpCaretsAddedToColumnSel:= c.GetValue('carets_with_column_sel', Op.OpCaretsAddedToColumnSel);
  Op.OpCaretKeepVisibleOnScroll:= c.GetValue('caret_keep_visible_on_scroll', Op.OpCaretKeepVisibleOnScroll);

  Op.OpShowCurLine:= c.GetValue('show_cur_line', Op.OpShowCurLine);
  Op.OpShowCurLineMinimal:= c.GetValue('show_cur_line_minimal', Op.OpShowCurLineMinimal);
  Op.OpShowCurLineOnlyFocused:= c.GetValue('show_cur_line_only_focused', Op.OpShowCurLineOnlyFocused);
  Op.OpShowCurCol:= c.GetValue('show_cur_column', Op.OpShowCurCol);
  Op.OpShowLastLineOnTop:= c.GetValue('show_last_line_on_top', Op.OpShowLastLineOnTop);
  Op.OpShowFullBackgroundSel:= c.GetValue('show_full_sel_bg', Op.OpShowFullBackgroundSel);
  Op.OpShowFullBackgroundSyntax:= c.GetValue('show_full_syntax_bg', Op.OpShowFullBackgroundSyntax);
  Op.OpShowMouseSelFrame:= c.GetValue('show_mouse_sel_frame', Op.OpShowMouseSelFrame);
  Op.OpCopyLineIfNoSel:= c.GetValue('copy_line_if_no_sel', Op.OpCopyLineIfNoSel);
  Op.OpCutLineIfNoSel:= c.GetValue('cut_line_if_no_sel', Op.OpCutLineIfNoSel);
  Op.OpCopyColumnAlignedBySpaces:= c.GetValue('copy_column_aligned_by_spaces', Op.OpCopyColumnAlignedBySpaces);
  Op.OpSavingTrimSpaces:= c.GetValue('saving_trim_spaces', Op.OpSavingTrimSpaces);
  Op.OpSavingTrimFinalEmptyLines:= c.GetValue('saving_trim_final_empty', Op.OpSavingTrimFinalEmptyLines);
  Op.OpSavingForceFinalEol:= c.GetValue('saving_force_final_eol', Op.OpSavingForceFinalEol);
  Op.OpShowHintOnVertScroll:= c.GetValue('show_hint_on_vert_scroll', Op.OpShowHintOnVertScroll);
  Op.OpSmoothScroll:= c.GetValue('smooth_scroll', Op.OpSmoothScroll);
  Op.OpCenteringWidth:= c.GetValue('centering_width', Op.OpCenteringWidth);
  Op.OpCenteringForDistractionFree:= c.GetValue('centering_for_distraction_free', Op.OpCenteringForDistractionFree);
  Op.OpScrollStyleHorz:= c.GetValue('scroll_style_horz', Op.OpScrollStyleHorz);

  Op.OpLexerDynamicHiliteEnabled:= c.GetValue('lexer_dynamic_hilite', Op.OpLexerDynamicHiliteEnabled);
  Op.OpLexerDynamicHiliteMaxLines:= c.GetValue('lexer_dynamic_hilite_max_lines', Op.OpLexerDynamicHiliteMaxLines);
  Op.OpLexerLineSeparators:= false; //c.GetValue('lexer_line_separators', Op.OpLexerLineSeparators);
  Op.OpZebra:= c.GetValue('zebra', Op.OpZebra);

  Op.OpNonWordChars:= c.GetValue('nonword_chars', Op.OpNonWordChars);
  Op.OpHexChars:= c.GetValue('hex_chars', Op.OpHexChars);
  Op.OpFoldStyle:= c.GetValue('fold_style', Op.OpFoldStyle);
  Op.OpFoldTooltipShow:= c.GetValue('fold_tooltip_show', Op.OpFoldTooltipShow);

  Op.OpIndentAuto:= c.GetValue('indent_auto', Op.OpIndentAuto);
  Op.OpIndentAutoKind:= c.GetValue('indent_kind', Op.OpIndentAutoKind);
  Op.OpIndentSize:= c.GetValue('indent_size', Op.OpIndentSize);
  Op.OpIndentAutoRule:= c.GetValue('indent_auto_rule', Op.OpIndentAutoRule);
  Op.OpUnIndentKeepsAlign:= c.GetValue('unindent_keeps_align', Op.OpUnIndentKeepsAlign);
  Op.OpIndentMakesWholeLineSel:= c.GetValue('indent_makes_whole_line_sel', Op.OpIndentMakesWholeLineSel);

  Op.OpMouse2ClickDragSelectsWords:= c.GetValue('mouse_2click_drag_select_words', Op.OpMouse2ClickDragSelectsWords);
  Op.OpMouseDragDrop:= c.GetValue('mouse_drag_drop', Op.OpMouseDragDrop);
  Op.OpMouseDragDropFocusTarget:= c.GetValue('mouse_drag_drop_focus_target', Op.OpMouseDragDropFocusTarget);
  Op.OpMouseMiddleClickNiceScroll:= c.GetValue('mouse_mid_click_scroll', Op.OpMouseMiddleClickNiceScroll);
  Op.OpMouseMiddleClickPaste:= c.GetValue('mouse_mid_click_paste', Op.OpMouseMiddleClickPaste);
  Op.OpMouseRightClickMovesCaret:= c.GetValue('mouse_rt_click_moves_caret', Op.OpMouseRightClickMovesCaret);
  Op.OpMouseEnableColumnSelection:= c.GetValue('mouse_column_selection', Op.OpMouseEnableColumnSelection);
  Op.OpMouseHideCursorOnType:= c.GetValue('mouse_hide_cursor', Op.OpMouseHideCursorOnType);
  Op.OpMouseGutterClickSelectedLine:= c.GetValue('mouse_gutter_click_sel', Op.OpMouseGutterClickSelectedLine);
  Op.OpMouseWheelZoom:= c.GetValue('mouse_wheel_zoom', Op.OpMouseWheelZoom);
  Op.OpMouseWheelSpeedVert:= c.GetValue('mouse_wheel_speed_vert', Op.OpMouseWheelSpeedVert);
  Op.OpMouseWheelSpeedHorz:= c.GetValue('mouse_wheel_speed_horz', Op.OpMouseWheelSpeedHorz);
  Op.OpMouseClickNumberSelectsEol:= c.GetValue('mouse_click_number_selects_eol', Op.OpMouseClickNumberSelectsEol);
  cSpeedScrollAutoVert:= c.GetValue('mouse_scroll_speed_vert', cSpeedScrollAutoVert);

  Op.OpKeyBackspaceUnindent:= c.GetValue('key_bksp_unindent', Op.OpKeyBackspaceUnindent);
  Op.OpKeyBackspaceWrap:= c.GetValue('key_bksp_wrap', Op.OpKeyBackspaceWrap);
  Op.OpKeyTabIndents:= c.GetValue('key_tab_indents_sel', Op.OpKeyTabIndents);
  Op.OpKeyHomeToNonSpace:= c.GetValue('key_home_to_nonspace', Op.OpKeyHomeToNonSpace);
  Op.OpKeyHomeEndNavigateWrapped:= c.GetValue('key_home_end_nav_wrapped', Op.OpKeyHomeEndNavigateWrapped);
  Op.OpKeyEndToNonSpace:= c.GetValue('key_end_to_nonspace', Op.OpKeyEndToNonSpace);
  Op.OpKeyPageKeepsRelativePos:= c.GetValue('key_page_keeps_rel_pos', Op.OpKeyPageKeepsRelativePos);
  Op.OpKeyPageUpDownSize:= c.GetValue('key_page_size', Op.OpKeyPageUpDownSize);
  Op.OpKeyUpDownKeepColumn:= c.GetValue('key_up_down_keeps_column', Op.OpKeyUpDownKeepColumn);
  Op.OpKeyUpDownNavigateWrapped:= c.GetValue('key_up_down_nav_wrapped', Op.OpKeyUpDownNavigateWrapped);
  Op.OpKeyLeftRightSwapSel:= c.GetValue('key_left_right_swap_sel', Op.OpKeyLeftRightSwapSel);
  Op.OpKeyLeftRightSwapSelAndSelect:= c.GetValue('key_left_right_swap_sel_and_select', Op.OpKeyLeftRightSwapSelAndSelect);
end;


procedure TfmMain.DoOps_LoadOptions_UiAutoCompletion(c: TJSONConfig);
begin
  UiOps.AutocompleteAutoshowCharCount:= c.GetValue('autocomplete_autoshow_chars', UiOps.AutocompleteAutoshowCharCount);
  //UiOps.AutocompleteAutoshowLexers:= c.GetValue('autocomplete_autoshow_lexers', UiOps.AutocompleteAutoshowLexers);
  UiOps.AutocompleteTriggerChars:= c.GetValue('autocomplete_trigger_chars', UiOps.AutocompleteTriggerChars);
  UiOps.AutocompleteCommitChars:= c.GetValue('autocomplete_commit_chars', UiOps.AutocompleteCommitChars);
  UiOps.AutocompleteCloseChars:= c.GetValue('autocomplete_close_chars', UiOps.AutocompleteCloseChars);
  UiOps.AutocompleteAddOpeningBracket:= c.GetValue('autocomplete_add_opening_bracket', UiOps.AutocompleteAddOpeningBracket);
  UiOps.AutocompleteUpDownAtEdge:= c.GetValue('autocomplete_up_down_at_edge', UiOps.AutocompleteUpDownAtEdge);
  UiOps.AutoCloseBrackets:= c.GetValue('auto_close_brackets', UiOps.AutoCloseBrackets);
end;

procedure TfmMain.DoOps_LoadOptions_Ui(c: TJSONConfig);
var
  S: string;
begin
  UiOps.VarFontName:= UTF8Encode(c.GetValue(str_UiFontName, UTF8Decode(UiOps.VarFontName)));
  UiOps.VarFontSize:= Abs(c.GetValue(str_UiFontSize, UiOps.VarFontSize)); //Abs for macOS
  UiOps.OutputFontName:= UTF8Encode(c.GetValue(str_UiFontOutputName, UTF8Decode(UiOps.OutputFontName)));
  UiOps.OutputFontSize:= Abs(c.GetValue(str_UiFontOutputSize, UiOps.OutputFontSize)); //Abs for macOS

  UiOps.DoubleBuffered:= c.GetValue(str_UiDoubleBuffered, UiOps.DoubleBuffered);

  UiOps.StatusNoSel:= c.GetValue('ui_statusbar_no_sel', UiOps.StatusNoSel);
  UiOps.StatusSmallSel:= c.GetValue('ui_statusbar_small_sel', UiOps.StatusSmallSel);
  UiOps.StatusStreamSel:= c.GetValue('ui_statusbar_str_sel', UiOps.StatusStreamSel);
  UiOps.StatusColSel:= c.GetValue('ui_statusbar_col_sel', UiOps.StatusColSel);
  UiOps.StatusCarets:= c.GetValue('ui_statusbar_carets', UiOps.StatusCarets);
  UiOps.StatusPanels:= c.GetValue('ui_statusbar_panels', UiOps.StatusPanels);
  //UiOps.StatusHeight:= c.GetValue('ui_statusbar_height', UiOps.StatusHeight); //assigned from "ui_tab_size_y"
  UiOps.StatusTime:= Max(1, Min(60, c.GetValue('ui_statusbar_time', UiOps.StatusTime)));

  UiOps.ShowActiveBorder:= c.GetValue('ui_active_border', UiOps.ShowActiveBorder);
  UiOps.ShowTitlePath:= c.GetValue('ui_title_path', UiOps.ShowTitlePath);

  UiOps.Scale:= c.GetValue('ui_scale'+cOptionSystemSuffix, 0);
  UiOps.ScaleFont:= c.GetValue('ui_scale_font'+cOptionSystemSuffix, 100);
  if UiOps.Scale=0 then
   UiOps.Scale:= 100*Screen.PixelsPerInch div 96;

  UiOps.ReopenSession:= c.GetValue('ui_reopen_session', UiOps.ReopenSession);
  UiOps.AutoSaveSession:= c.GetValue('ui_auto_save_session', UiOps.AutoSaveSession);
  //UiOps.FloatGroupsInTaskbar:= c.GetValue('ui_float_groups_in_taskbar', UiOps.FloatGroupsInTaskbar);
  UiOps.OneInstance:= c.GetValue('ui_one_instance', UiOps.OneInstance);
  UiOps.UndoPersistent:= c.GetValue('undo_persistent', UiOps.UndoPersistent);

  UiOps.NotificationEnabled:= c.GetValue('ui_notif', UiOps.NotificationEnabled);
  UiOps.NotificationTimeSeconds:= Max(1, c.GetValue('ui_notif_time', UiOps.NotificationTimeSeconds)); //don't allow 0

  UiOps.NonTextFiles:= c.GetValue('ui_non_text', UiOps.NonTextFiles);
  UiOps.NonTextFilesBufferKb:= Max(1, Min(1024, c.GetValue('ui_non_text_buffer', UiOps.NonTextFilesBufferKb)));
  UiOps.ReloadFollowTail:= c.GetValue('ui_reload_tail', UiOps.ReloadFollowTail);
  UiOps.ReloadUnsavedConfirm:= c.GetValue('ui_reload_unsaved_confirm', UiOps.ReloadUnsavedConfirm);
  UiOps.FullScreen:= c.GetValue('ui_fullscreen', UiOps.FullScreen);

  UiOps.MouseGotoDefinition:= c.GetValue('mouse_goto_definition', UiOps.MouseGotoDefinition);
  //UiOps.LogDebug:= c.GetValue('log_debug', UiOps.LogDebug);
  UiOps.LogConsole:= c.GetValue('log_console', UiOps.LogConsole);

  UiOps.Emmet_AddSlashToEmptyTags:= c.GetValue('emmet_add_slash_to_empty_tags', UiOps.Emmet_AddSlashToEmptyTags);
  UiOps.Emmet_CommentTags:= c.GetValue('emmet_comment_tags', UiOps.Emmet_CommentTags);
  UiOps.Emmet_IndentNested:= c.GetValue('emmet_indent_nested', UiOps.Emmet_IndentNested);
  UiOps.Emmet_SingleLine:= c.GetValue('emmet_single_line', UiOps.Emmet_SingleLine);
  UiOps.Emmet_TrimLineMarkers:= c.GetValue('emmet_trim_line_markers', UiOps.Emmet_TrimLineMarkers);
  UiOps.Emmet_WordWrap:= c.GetValue('emmet_word_wrap', UiOps.Emmet_WordWrap);

  UiOps.HotkeyFindDialog:= c.GetValue('find_hotkey_find_dlg', UiOps.HotkeyFindDialog);
  UiOps.HotkeyReplaceDialog:= c.GetValue('find_hotkey_replace_dlg', UiOps.HotkeyReplaceDialog);
  UiOps.HotkeyFindFirst:= c.GetValue('find_hotkey_find_first', UiOps.HotkeyFindFirst);
  UiOps.HotkeyFindNext:= c.GetValue('find_hotkey_find_next', UiOps.HotkeyFindNext);
  UiOps.HotkeyFindPrev:= c.GetValue('find_hotkey_find_prev', UiOps.HotkeyFindPrev);
  UiOps.HotkeyReplaceAndFindNext:= c.GetValue('find_hotkey_replace', UiOps.HotkeyReplaceAndFindNext);
  UiOps.HotkeyReplaceNoFindNext:= c.GetValue('find_hotkey_replace_no_next', UiOps.HotkeyReplaceNoFindNext);
  UiOps.HotkeyReplaceAll:= c.GetValue('find_hotkey_replace_all', UiOps.HotkeyReplaceAll);
  UiOps.HotkeyReplaceGlobal:= c.GetValue('find_hotkey_replace_global', UiOps.HotkeyReplaceGlobal);
  UiOps.HotkeyCountAll:= c.GetValue('find_hotkey_count_all', UiOps.HotkeyCountAll);
  UiOps.HotkeyExtractAll:= c.GetValue('find_hotkey_extract', UiOps.HotkeyExtractAll);
  UiOps.HotkeySelectAll:= c.GetValue('find_hotkey_select_all', UiOps.HotkeySelectAll);
  UiOps.HotkeyMarkAll:= c.GetValue('find_hotkey_mark_all', UiOps.HotkeyMarkAll);
  UiOps.HotkeyToggleRegex:= c.GetValue('find_hotkey_op_regex', UiOps.HotkeyToggleRegex);
  UiOps.HotkeyToggleCaseSens:= c.GetValue('find_hotkey_op_case', UiOps.HotkeyToggleCaseSens);
  UiOps.HotkeyToggleWords:= c.GetValue('find_hotkey_op_words', UiOps.HotkeyToggleWords);
  UiOps.HotkeyToggleWrapped:= c.GetValue('find_hotkey_op_wrapped', UiOps.HotkeyToggleWrapped);
  UiOps.HotkeyToggleInSelect:= c.GetValue('find_hotkey_op_in_sel', UiOps.HotkeyToggleInSelect);
  UiOps.HotkeyToggleMultiline:= c.GetValue('find_hotkey_op_multiline', UiOps.HotkeyToggleMultiline);
  UiOps.HotkeyToggleConfirmRep:= c.GetValue('find_hotkey_op_confirm_rep', UiOps.HotkeyToggleConfirmRep);
  UiOps.HotkeyToggleTokens:= c.GetValue('find_hotkey_op_tokens', UiOps.HotkeyToggleTokens);

  UiOps.LexerThemes:= c.GetValue('ui_lexer_themes', UiOps.LexerThemes);
  UiOps.LexerMenuGrouped:= c.GetValue('ui_lexers_grouped', UiOps.LexerMenuGrouped);
  UiOps.LexerDelayedParsingPause:= c.GetValue('lexer_delayed_parsing_pause', UiOps.LexerDelayedParsingPause);
  UiOps.LexerDelayedParsingSize:= c.GetValue('lexer_delayed_parsing_size', UiOps.LexerDelayedParsingSize);
  UiOps.LexerPostponeUntilShown:= c.GetValue('lexer_postpone_until_shown', UiOps.LexerPostponeUntilShown);
  if not UiOps.LexerPostponeUntilShown then
   AllowFrameParsing:= true;

  UiOps.ShowMenubar:= c.GetValue('ui_menu_show', UiOps.ShowMenubar);
  UiOps.ShowStatusbar:= c.GetValue('ui_statusbar_show', UiOps.ShowStatusbar);
  UiOps.ShowToolbar:= c.GetValue('ui_toolbar_show', UiOps.ShowToolbar);

  UiOps.SidebarShow:= c.GetValue('ui_sidebar_show', UiOps.SidebarShow);
  UiOps.SidebarOnRight:= c.GetValue('ui_sidebar_on_right', UiOps.SidebarOnRight);
  UiOps.SidebarTheme:= c.GetValue('ui_sidebar_theme', UiOps.SidebarTheme);
  UiOps.TreeTheme:= c.GetValue('ui_tree_theme', UiOps.TreeTheme);
  UiOps.ToolBarTheme:= c.GetValue('ui_toolbar_theme', UiOps.ToolBarTheme);

  {$ifndef windows}
  //option is ignored on win
  UiOps.PyLibrary:= c.GetValue('pylib'+cOptionSystemSuffix, UiOps.PyLibrary);
  {$endif}

  UiOps.PictureTypes:= c.GetValue('picture_types', UiOps.PictureTypes);
  UiOps.MaxFileSizeToOpen:= c.GetValue('ui_max_size_open', UiOps.MaxFileSizeToOpen);
  UiOps.MaxFileSizeForLexer:= c.GetValue('ui_max_size_lexer', UiOps.MaxFileSizeForLexer);
  UiOps.MaxRangesForCodeTree:= c.GetValue('ui_max_ranges_codetree', UiOps.MaxRangesForCodeTree);

  UiOps.AutocompleteHtml:= c.GetValue('autocomplete_html', UiOps.AutocompleteHtml);
  UiOps.AutocompleteCss:= c.GetValue('autocomplete_css', UiOps.AutocompleteCss);

  UiOps.AutocompleteHtml_Lexers:= c.GetValue('autocomplete_html_lexers', UiOps.AutocompleteHtml_Lexers);
  UiOps.AutocompleteCss_Lexers:= c.GetValue('autocomplete_css_lexers', UiOps.AutocompleteCss_Lexers);

  UiOps.FindSuggestSel:= c.GetValue('find_suggest_sel', UiOps.FindSuggestSel);
  UiOps.FindSuggestWord:= c.GetValue('find_suggest_cur_word', UiOps.FindSuggestWord);
  UiOps.FindSuggestInSelection:= c.GetValue('find_suggest_in_selection', UiOps.FindSuggestInSelection);
  UiOps.FindSelCase:= c.GetValue('find_sel_case', UiOps.FindSelCase);
  UiOps.FindShow_FindFirst:= c.GetValue('find_show_findfirst', UiOps.FindShow_FindFirst);
  UiOps.FindShow_MarkALl:= c.GetValue('find_show_mark_all', UiOps.FindShow_MarkALl);
  UiOps.FindShow_SelectAll:= c.GetValue('find_show_select_all', UiOps.FindShow_SelectAll);
  UiOps.FindShow_Extract:= c.GetValue('find_show_extract', UiOps.FindShow_Extract);
  UiOps.FindShow_ReplaceGlobal:= c.GetValue('find_show_replace_global', UiOps.FindShow_ReplaceGlobal);
  UiOps.FindIndentVert:= c.GetValue('find_indent_vert', UiOps.FindIndentVert);
  UiOps.FindIndentHorz:= c.GetValue('find_indent_horz', UiOps.FindIndentHorz);
  UiOps.FindMultiLineScale:= c.GetValue('find_multiline_scale', UiOps.FindMultiLineScale);
  UiOps.FindSeparateForm:= c.GetValue('find_separate_form', UiOps.FindSeparateForm);

  UiOps.TabsDisabled:= c.GetValue('ui_tab_disabled', UiOps.TabsDisabled);
  UiOps.TabVarWidth:= c.GetValue('ui_tab_variable_width', UiOps.TabVarWidth);
  UiOps.TabMultiline:= c.GetValue('ui_tab_multiline', UiOps.TabMultiline);
  UiOps.TabAngled:= c.GetValue('ui_tab_angled', UiOps.TabAngled);
  UiOps.TabFlat:= c.GetValue('ui_tab_flat', UiOps.TabFlat);
  UiOps.TabWidth:= c.GetValue('ui_tab_size_x', UiOps.TabWidth);
  UiOps.TabWidthMin:= c.GetValue('ui_tab_size_x_min', UiOps.TabWidthMin);
  UiOps.TabWidthMax:= c.GetValue('ui_tab_size_x_max', UiOps.TabWidthMax);
  UiOps.TabHeight:= c.GetValue('ui_tab_size_y', UiOps.TabHeight);
   UiOps.StatusHeight:= UiOps.TabHeight;
  UiOps.TabHeightInner:= UiOps.TabHeight-1;
  UiOps.TabPosition:= c.GetValue('ui_tab_position', UiOps.TabPosition);
  UiOps.TabColorFull:= c.GetValue('ui_tab_fullcolor', UiOps.TabColorFull);
  UiOps.TabShowX:= c.GetValue('ui_tab_show_x', UiOps.TabShowX);
  UiOps.TabShowXSize:= c.GetValue('ui_tab_show_x_size', UiOps.TabShowXSize);
  UiOps.TabShowPlus:= c.GetValue('ui_tab_show_plus', UiOps.TabShowPlus);
  UiOps.TabDblClickClose:= c.GetValue('ui_tab_2click_closes', UiOps.TabDblClickClose);
  UiOps.TabNumbers:= c.GetValue('ui_tab_numbers', UiOps.TabNumbers);
  UiOps.TabNewNearCurrent:= c.GetValue('ui_tab_new_near_current', UiOps.TabNewNearCurrent);
  UiOps.TabRecentOnClose:= c.GetValue('ui_tab_recent_on_close', UiOps.TabRecentOnClose);
  UiOps.TabButtonLayout:= c.GetValue('ui_tab_button_layout', UiOps.TabButtonLayout);
  UiOps.TabPreviewFontStyle:= c.GetValue('ui_tab_preview_font_style', UiOps.TabPreviewFontStyle);
  UiOps.TabSwitcherDialog:= c.GetValue('ui_tab_switcher_dialog', UiOps.TabSwitcherDialog);

  //UiOps.ListboxCentered:= c.GetValue('ui_listbox_centered', UiOps.ListboxCentered);
  UiOps.ListboxSizeX:= c.GetValue('ui_listbox_size_x', UiOps.ListboxSizeX);
  UiOps.ListboxSizeY:= c.GetValue('ui_listbox_size_y', UiOps.ListboxSizeY);
  UiOps.ListboxCompleteSizeX:= c.GetValue('ui_listbox_complete_size_x', UiOps.ListboxCompleteSizeX);
  UiOps.ListboxCompleteSizeY:= c.GetValue('ui_listbox_complete_size_y', UiOps.ListboxCompleteSizeY);
  UiOps.ListboxFuzzySearch:= c.GetValue('ui_listbox_fuzzy', UiOps.ListboxFuzzySearch);

  UiOps.MaxHistoryEdits:= Max(0, Min(200, c.GetValue('ui_max_history_edits', UiOps.MaxHistoryEdits)));
  UiOps.MaxHistoryMenu:= Max(0, Min(50, c.GetValue('ui_max_history_menu', UiOps.MaxHistoryMenu)));
  UiOps.MaxHistoryFiles:= Max(0, Min(400, c.GetValue('ui_max_history_files', UiOps.MaxHistoryFiles)));

  UiOps.EscapeClose:= c.GetValue('ui_esc_close', UiOps.EscapeClose);
  UiOps.EscapeCloseConsole:= c.GetValue('ui_esc_close_console', UiOps.EscapeCloseConsole);
  UiOps.ConsoleWordWrap:= c.GetValue('ui_console_wrap', UiOps.ConsoleWordWrap);
  //UiOps.ConsoleCompact:= c.GetValue('ui_console_compact', UiOps.ConsoleCompact);
  UiOps.InitialDir:= c.GetValue('ui_initial_dir', UiOps.InitialDir);

  UiOps.ExportHtmlNumbers:= c.GetValue('export_html_numbers', UiOps.ExportHtmlNumbers);
  UiOps.ExportHtmlFontName:= UTF8Encode(c.GetValue('export_html_font_name', UTF8Decode(UiOps.ExportHtmlFontName)));
  UiOps.ExportHtmlFontSize:= Abs(c.GetValue('export_html_font_size', UiOps.ExportHtmlFontSize));

  UiOps.TreeAutoSync:= c.GetValue('ui_tree_auto_sync', UiOps.TreeAutoSync);
  UiOps.TreeTimeFill:= c.GetValue('ui_tree_time_fill', UiOps.TreeTimeFill);
  //UiOps.TreeTimeCaret:= c.GetValue('ui_tree_time_focus', UiOps.TreeTimeCaret);
  //UiOps.TreeShowLines:= c.GetValue('ui_tree_show_lines', UiOps.TreeShowLines);
  UiOps.TreeShowIcons:= c.GetValue('ui_tree_icons', UiOps.TreeShowIcons);
  UiOps.TreeShowTooltips:= c.GetValue('ui_tree_show_tooltips', UiOps.TreeShowTooltips);
  //UiOps.TreeCache:= c.GetValue('ui_tree_cache', UiOps.TreeCache);
  UiOps.TreeFilterLayout:= c.GetValue('ui_tree_filter_layout', UiOps.TreeFilterLayout);
  UiOps.TreeSublexers:= c.GetValue('ui_tree_sublexers', UiOps.TreeSublexers);

  UiOps.PyChangeSlow:= c.GetValue('py_change_slow', UiOps.PyChangeSlow);
  UiOps.PyInitLog:= c.GetValue('py_init_log', UiOps.PyInitLog);

  UiOps.NewdocLexer:= c.GetValue('newdoc_lexer', UiOps.NewdocLexer);
  UiOps.NewdocEnc:= c.GetValue('newdoc_encoding', UiOps.NewdocEnc);
  UiOps.NewdocEnds:= c.GetValue('newdoc_ends', UiOps.NewdocEnds);

  UiOps.DefaultEncUtf8:= c.GetValue(str_DefEncodingIsUtf8, UiOps.DefaultEncUtf8);
  UiOps.ViewerBinaryWidth:= c.GetValue('viewer_binary_width', UiOps.ViewerBinaryWidth);

  S:= c.GetValue('ui_theme', '?');
  UiOps.ThemeUi_Loaded:= S<>'?';
  if UiOps.ThemeUi_Loaded then
    UiOps.ThemeUi:= S;

  S:= c.GetValue('ui_theme_syntax', '?');
  UiOps.ThemeSyntax_Loaded:= S<>'?';
  if UiOps.ThemeSyntax_Loaded then
    UiOps.ThemeSyntax:= S;
end;


procedure TfmMain.DoOps_LoadOptions(const fn: string; var Op: TEditorOps;
  AllowUiOps: boolean=true; AllowGlobalOps: boolean=true);
var
  c: TJsonConfig;
begin
  c:= TJsonConfig.Create(nil);
  try
    try
      c.Filename:= fn;
    except
      on E: Exception do
      begin
        MsgBadConfig(fn+#10#10+E.Message);
        Exit
      end;
    end;

    if AllowGlobalOps then
      DoOps_LoadOptions_Global(c);

    if AllowUiOps then
      DoOps_LoadOptions_Ui(c);

    DoOps_LoadOptions_UiAutoCompletion(c);
    DoOps_LoadOptions_Editor(c, Op);

  finally
    c.Free;
  end;
end;


procedure TfmMain.DoOps_ResetLexerSpecificOptions;
begin
  //some options are for lexer-specific configs.
  //they must reset on any lexer change.
  //otherwise, option in NN lexer will be active in MM lexer (after tab with NN focuses).
  UiOps.AutocompleteAutoshowCharCount:= 0;
  UiOps.AutocompleteTriggerChars:= '';
end;

procedure TfmMain.DoOps_LoadOptionsLexerSpecific(Ed: TATSynEdit);
var
  F: TEditorFrame;
  Ops: TEditorOps;
  FilenameDefault, FilenameUser, CurLexer: string;
  bExistsDefault, bExistsUser: boolean;
  TempOps: TATEditorTempOptions;
begin
  F:= GetEditorFrame(Ed);
  if F=nil then exit;

  DoOps_ResetLexerSpecificOptions;

  CurLexer:= F.LexerName[Ed];
  FilenameDefault:= GetAppLexerSpecificConfig(CurLexer, true);
  FilenameUser:= GetAppLexerSpecificConfig(CurLexer, false);

  bExistsDefault:= FileExistsUTF8(FilenameDefault);
  bExistsUser:= FileExistsUTF8(FilenameUser);

  if bExistsDefault or bExistsUser then
  begin
    Ops:= EditorOps;
    EditorSaveTempOptions(Ed, TempOps);

    if bExistsDefault then
    begin
      DoOps_LoadOptions(FilenameDefault, Ops, false, false);
    end;

    if bExistsUser then
    begin
      DoOps_LoadOptions(FilenameUser, Ops, false, false);
    end;

    DoApplyFrameOps(F, Ops, true);
    EditorRestoreTempOptions(Ed, TempOps);
  end;
end;


procedure TfmMain.DoOps_DialogFont_Text;
begin
  DoOps_DialogFont(
    EditorOps.OpFontName,
    EditorOps.OpFontSize,
    str_FontName,
    str_FontSize);
  DoApplyFont_Text;
end;

procedure TfmMain.DoOps_DialogFont_Ui;
begin
  DoOps_DialogFont(
    UiOps.VarFontName,
    UiOps.VarFontSize,
    str_UiFontName,
    str_UiFontSize);
  DoApplyFont_Ui;
end;

procedure TfmMain.DoOps_DialogFont_Output;
begin
  DoOps_DialogFont(
    UiOps.OutputFontName,
    UiOps.OutputFontSize,
    str_UiFontOutputName,
    str_UiFontOutputSize);
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
end;

procedure TfmMain.DoOps_FontSizeReset;
begin
  EditorOps.OpFontSize:= EditorOps.OpFontSize_original;
  EditorOps.OpFontSize_i:= EditorOps.OpFontSize_original_i;
  EditorOps.OpFontSize_b:= EditorOps.OpFontSize_original_b;
  EditorOps.OpFontSize_bi:= EditorOps.OpFontSize_original_bi;
  DoApplyFont_Text;
end;


procedure TfmMain.DoOps_DialogFont(
  var OpName: string; var OpSize: integer;
  const AConfigStrName, AConfigStrSize: string);
var
  dlg: TFontDialog;
  cfg: TJsonConfig;
  fn: string;
begin
  fn:= AppFile_OptionsUser;
  if not FileExistsUTF8(fn) then
  begin
    FCreateFile(fn, true);
    if not FileExistsUTF8(fn) then exit;
  end;

  dlg:= TFontDialog.Create(nil);
  try
    dlg.Font.Name:= OpName;
    dlg.Font.Size:= OpSize;
    if not dlg.Execute then exit;
    OpName:= dlg.Font.Name;
    OpSize:= dlg.Font.Size;
  finally
    FreeAndNil(dlg);
  end;

  cfg:= TJSONConfig.Create(nil);
  try
    try
      cfg.Formatted:= true;
      cfg.Filename:= fn;
      cfg.SetValue(AConfigStrName, UTF8Decode(OpName));
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
begin
  DoOps_LoadOptions(AppFile_OptionsUser, EditorOps);
  DoApplyAllOps;
  DoOps_LoadKeymap(false);

  MsgStatus(msgStatusReadingOps);
  DoPyEvent(nil, cEventOnState, [AppVariant(APPSTATE_CONFIG_REREAD)]);
end;


