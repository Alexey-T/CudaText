(*
This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at http://mozilla.org/MPL/2.0/.

Copyright (c) Alexey Torgashin
*)
{$ifdef nn}begin end;{$endif}

procedure TfmMain.DoOps_SaveHistory;
var
  c: TJSONConfig;
begin
  c:= TJSONConfig.Create(nil);
  try
    c.Formatted:= true; //needs last at__jsonconf.pp
    c.Filename:= GetAppPath(cFileOptionsHistory);

    ////form
    if not ShowFullscreen then
    begin
      c.SetValue('/dialog/max', WindowState=wsMaximized);
      if WindowState<>wsMaximized then
      begin
        c.SetValue('/dialog/posx', Left);
        c.SetValue('/dialog/posy', Top);
        c.SetValue('/dialog/sizex', Width);
        c.SetValue('/dialog/sizey', Height);
      end;

      if FloatSide then
      begin
        c.SetValue('/dialog/fl_side/posx',  FFormFloatSide.Left);
        c.SetValue('/dialog/fl_side/posy',  FFormFloatSide.Top);
        c.SetValue('/dialog/fl_side/sizex', FFormFloatSide.Width);
        c.SetValue('/dialog/fl_side/sizey', FFormFloatSide.Height);
      end;

      if FloatBottom then
      begin
        c.SetValue('/dialog/fl_btm/posx',  FFormFloatBottom.Left);
        c.SetValue('/dialog/fl_btm/posy',  FFormFloatBottom.Top);
        c.SetValue('/dialog/fl_btm/sizex', FFormFloatBottom.Width);
        c.SetValue('/dialog/fl_btm/sizey', FFormFloatBottom.Height);
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

      c.SetValue('show_toolbar', ShowToolbar);
      c.SetValue('show_status', ShowStatus);
      c.SetValue('show_sidepanel', ShowSidePanel);
      c.SetValue('show_bottom', ShowBottom);
      c.SetValue('show_menu', ShowMenu);

      c.SetValue('float_side', FloatSide);
      c.SetValue('float_bottom', FloatBottom);

      if not FloatSide then
        c.SetValue('size_side', PanelLeft.Width);
      if not FloatBottom then
        c.SetValue('size_bottom', PanelBottom.Height);
      c.SetValue('tab_left', SidebarPanel);
    end;

    ////recents
    SSaveStringsToFile(c, '/recent_files', FListRecents, UiOps.MaxHistoryMenu);

    ////find dialog
    if Assigned(fmFind) then
    begin
      SSaveStringsToFile(c, '/list_find', fmFind.edFind.Items, UiOps.MaxHistoryEdits);
      SSaveStringsToFile(c, '/list_replace', fmFind.edRep.Items, UiOps.MaxHistoryEdits);
      if fmFind.IsNarrow then
      begin
        c.SetValue('/find/pos_x', fmFind.Left);
        c.SetValue('/find/pos_y', fmFind.Top);
        c.SetValue('/find/size_x', fmFind.Width);
      end;
      c.SetValue('/find/multi_line', fmFind.IsMultiLine);
    end;

    SSaveStringsToFile(c, '/list_console', fmConsole.ed.Items, UiOps.MaxHistoryEdits);

    ////finder
    c.SetValue('/finder/regex', FFinder.OptRegex);
    c.SetValue('/finder/case', FFinder.OptCase);
    c.SetValue('/finder/words', FFinder.OptWords);
    c.SetValue('/finder/back', FFinder.OptBack);
    c.SetValue('/finder/confirm', FFinder.OptConfirmReplace);
    c.SetValue('/finder/wrap', FFinder.OptWrapped);
    c.SetValue('/finder/insel', FFinder.OptInSelection);

    ////state
    c.SetValue('theme_ui', FThemeUi);
    c.SetValue('theme_syntax', FThemeSyntax);
    c.SetValue('lang', AppLangName);
    c.SetValue('session', IfThen(UiOps.ShowLastFiles, FSessionName, ''));

    DoOps_SaveHistory_GroupView(c);
  finally
    c.Free;
  end;

  if UiOps.ShowLastFiles then
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
  MsgLogConsole('Saving session: '+ExtractFileName(fn_session));

  c:= TJSONConfig.Create(nil);
  try
    c.Formatted:= true;
    c.Filename:= fn_session;

    DoOps_SaveHistory_GroupView(c);

    for i:= Low(TATGroupsNums) to High(TATGroupsNums) do
      c.SetValue('/tab_index/g'+Inttostr(i), Groups.Pages[i].Tabs.TabIndex);
    c.SetValue('/tab_index/g_active', Groups.PagesIndexOf(Groups.PagesCurrent));

    c.SetValue('/panels/side_show', ShowSidePanel);
    if not FloatSide then
      c.SetValue('/panels/side_size', PanelLeft.Width);

    c.SetValue('/panels/bottom_show', ShowBottom);
    if not FloatBottom then
      c.SetValue('/panels/bottom_size', PanelBottom.Height);

    for i:= 0 to FrameCount-1 do
    begin
      F:= Frames[i];
      path:= '/'+Format('%3.3d', [i]);

      GetFrameLocation(F, Gr, Pages, nLocalGroup, nGlobalGroup, nTab);
      bModified:= F.Modified or ((F.FileName='') and not F.IsEmpty);

      c.SetValue(path+'/modified', bModified);
      c.SetValue(path+'/file', F.Filename);
      c.SetValue(path+'/tab_title', F.TabCaption);
      c.SetValue(path+'/group', nLocalGroup+1); //support old sessions with 1-based index

      if F.IsText then value:= ''
      else
      if F.IsBinary then value:= 'bin,'+IntToStr(Ord(F.Binary.Mode))
      else
      if F.IsPicture then value:= 'pic';
      c.SetValue(path+'/kind', value);

      if bModified then
      begin
        c.SetValue(path+'/text', F.Editor.Text);
        if F.FileName<>'' then
          c.SetValue(path+'/file_date', FormatFileDateAsNiceString(F.FileName));
      end;

      F.DoSaveHistoryEx(c, path);
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
  nGroup, i: integer;
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
  if not DoFileCloseAll then exit;

  c:= TJSONConfig.Create(nil);
  list:= TStringList.Create;
  try
    c.Filename:= fn_session;
    MsgLogConsole('Loading session: '+ExtractFileName(fn_session));

    DoOps_LoadHistory_GroupView(c);

    c.EnumSubKeys('/', list);
    list.Sort;

    for i:= 0 to list.Count-1 do
    begin
      item_path:= '/'+list[i]+'/';
      sFilename:= c.GetValue(item_path+'file', '');
      sTabTitle:= c.GetValue(item_path+'tab_title', '');
      sText:= c.GetValue(item_path+'text', '');
      sKind:= c.GetValue(item_path+'kind', '');
      bModified:= c.GetValue(item_path+'modified', false);

      //support old sessions with 1-based group index
      nGroup:= c.GetValue(item_path+'group', 1)-1;

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

      if not (nGroup in [Low(TATGroupsNums)..High(TATGroupsNums)]) then
        nGroup:= Low(TATGroupsNums);
      Pages:= Groups.Pages[nGroup];

      if SBeginsWith(sKind, 'bin') then
        sOpenOptions:= '/view-binary'
      else
        sOpenOptions:= '';

      F:= nil;
      //modified tabs must restore text, and set filename (if not empty)
      if bModified then
      begin
        if (FrameCount>0) and Frames[0].IsEmpty then
          F:= Frames[0]
        else
          F:= DoFileOpen('', Pages, SOptionPassive);
        if sFilename<>'' then
        begin
          F.FileName:= sFilename;
          F.TabCaption:= ExtractFileName(sFilename);
        end;
        F.Editor.Text:= sText;
        F.Editor.Modified:= bModified;
        if sTabTitle<>'' then
          F.TabCaption:= sTabTitle;
        F.UpdateModifiedState;
      end
      else
      //unmodified+named tabs must load file again
      if FileExistsUTF8(sFilename) then
        F:= DoFileOpen(sFilename, Pages, sOpenOptions+SOptionPassive);

      //load other props (caret/encoding/etc)
      if Assigned(F) then
      begin
        F.NotInRecents:= true;
        F.DoLoadHistoryEx(c, '/'+list[i]);
      end;
    end;

    //restore tab-index, group-index
    for i:= Low(TATGroupsNums) to High(TATGroupsNums) do
      Groups.Pages[i].Tabs.TabIndex:= c.GetValue('/tab_index/g'+Inttostr(i), 0);

    nGroup:= c.GetValue('/tab_index/g_active', Low(TATGroupsNums));
    if nGroup in [Low(TATGroupsNums)..High(TATGroupsNums)] then
    begin
      Groups.PagesCurrent:= Groups.Pages[nGroup];
      with Groups.PagesCurrent do
        Tabs.TabIndex:= Tabs.TabIndex;
    end;

    //restore panel sizes
    ShowSidePanel:= c.GetValue('/panels/side_show', ShowSidePanel);
    ShowBottom:= c.GetValue('/panels/bottom_show', ShowBottom);
    PanelLeft.Width:= Min(c.GetValue('/panels/side_size', PanelLeft.Width), ClientWidth div 2);
    PanelBottom.Height:= Min(c.GetValue('/panels/bottom_size', PanelBottom.Height), ClientHeight div 2);

    Result:= true;
  finally
    list.Free;
    c.Free;
  end;
end;

procedure TfmMain.DoOps_LoadHistory_GroupView(c: TJsonConfig);
var
  i: integer;
begin
  Groups.Mode:= TATGroupsMode(c.GetValue('groups', Ord(gmOne)));

  for i:= Low(TATGroupsNums) to High(TATGroupsNums) do
    with Groups.Pages[i] do
    begin
      Width:= c.GetValue('/gr/g'+Inttostr(i)+'_x', Width);
      Height:= c.GetValue('/gr/g'+Inttostr(i)+'_y', Height);
    end;

  with Groups.Panel1 do
  begin
    Width:= c.GetValue('/gr/p1_x', Width);
    Height:= c.GetValue('/gr/p1_y', Height);
  end;

  Groups.SaveSplitPos; //apply current sizes to internal Groups vars
end;

procedure TfmMain.DoOps_SaveHistory_GroupView(c: TJsonConfig);
var
  i: integer;
begin
  c.SetValue('groups', Ord(Groups.Mode));

  for i:= Low(TATGroupsNums) to High(TATGroupsNums) do
  begin
    c.SetValue('/gr/g'+Inttostr(i)+'_x', Groups.Pages[i].Width);
    c.SetValue('/gr/g'+Inttostr(i)+'_y', Groups.Pages[i].Height);
  end;

  c.SetValue('/gr/p1_x', Groups.Panel1.Width);
  c.SetValue('/gr/p1_y', Groups.Panel1.Height);
end;


procedure TfmMain.DoOps_LoadHistory_AfterOnStart;
var
  c: TJSONConfig;
  SCaption: string;
begin
  c:= TJSONConfig.Create(nil);
  try
    c.Filename:= GetAppPath(cFileOptionsHistory);
    SCaption:= c.GetValue('tab_left', '');

    //if plugin didnt load, caption not valid
    if DoSidebar_CaptionToTabIndex(SCaption)<0 then
      SCaption:= msgPanelTree_Init;

    SidebarPanel:= SCaption;
  finally
    FreeAndNil(c);
  end;
end;


procedure DoOps_LoadFormRect(c: TJSONConfig; const path: string; var R: TRect);
const
  cReservePixels=70;
begin
  R.Left:= c.GetValue(path+'/posx', R.Left);
  R.Top:= c.GetValue(path+'/posy', R.Top);
  R.Width:= c.GetValue(path+'/sizex', R.Width);
  R.Height:= c.GetValue(path+'/sizey', R.Height);

  //fit form to desktop
  if R.Left>Screen.DesktopWidth-cReservePixels then
    R.Left:= Screen.DesktopWidth-R.Width;
  if R.Top>Screen.DesktopHeight-cReservePixels then
    R.Top:= Screen.DesktopHeight-R.Height;
end;


procedure TfmMain.DoOps_LoadHistory;
var
  c: TJSONConfig;
  RectMain: TRect;
begin
  c:= TJSONConfig.Create(nil);
  try
    c.Filename:= GetAppPath(cFileOptionsHistory);

    RectMain:= Rect(100, 100, 900, 650);
    DoOps_LoadFormRect(c, '/dialog', RectMain);
    DoOps_LoadFormRect(c, '/dialog/fl_side', FBoundsFloatSide);
    DoOps_LoadFormRect(c, '/dialog/fl_btm', FBoundsFloatBottom);
    DoOps_LoadFormRect(c, '/dialog/fl_g1', FBoundsFloatGroups1);
    DoOps_LoadFormRect(c, '/dialog/fl_g2', FBoundsFloatGroups2);
    DoOps_LoadFormRect(c, '/dialog/fl_g3', FBoundsFloatGroups3);
    BoundsRect:= RectMain;

    if c.GetValue('/dialog/max', false) then
      WindowState:= wsMaximized;

    ////recents
    SLoadStringsFromFile(c, '/recent_files', FListRecents, UiOps.MaxHistoryMenu);
    SLoadStringsFromFile(c, '/list_console', fmConsole.ed.Items, UiOps.MaxHistoryEdits);
    UpdateMenuRecent(nil);

    ////finder
    FFinder.OptRegex:= c.GetValue('/finder/regex', false);
    FFinder.OptCase:= c.GetValue('/finder/case', false);
    FFinder.OptWords:= c.GetValue('/finder/words', false);
    FFinder.OptBack:= c.GetValue('/finder/back', false);
    FFinder.OptConfirmReplace:= c.GetValue('/finder/confirm', false);
    FFinder.OptWrapped:= c.GetValue('/finder/wrap', false);
    FFinder.OptInSelection:= c.GetValue('/finder/insel', false);

    ////state
    ShowToolbar:= c.GetValue('show_toolbar', false);
    ShowStatus:= c.GetValue('show_status', true);
    ShowSidePanel:= c.GetValue('show_sidepanel', true);
    ShowBottom:= c.GetValue('show_bottom', false);
    ShowMenu:= c.GetValue('show_menu', true);

    PanelLeft.Width:= Min(c.GetValue('size_side', 200), ClientWidth div 2);
    PanelBottom.Height:= Min(c.GetValue('size_bottom', 200), ClientHeight div 2);

    FloatSide:= c.GetValue('float_side', false);
    FloatBottom:= c.GetValue('float_bottom', false);

    if ShowBottom or FConsoleMustShow then
      DoShowConsole(false);

    ////AppTheme
    ThemeUi:= c.GetValue('theme_ui', '');
    ThemeSyntax:= c.GetValue('theme_syntax', '');
    AppLangName:= c.GetValue('lang', '');
    FSessionName:= c.GetValue('session', FSessionName);

    if AppLangName<>'' then
      DoLocalize;

    DoOps_LoadHistory_GroupView(c);
  finally
    c.Free;
  end;

  if UiOps.ShowLastFiles then
    DoOps_LoadSession(GetSessionFilename);
end;


procedure TfmMain.DoOps_LoadKeymap;
var
  F: TEditorFrame;
begin
  //func called few times before OnShow because of OnTabFocus, disable these
  if not FAllowLoadKeymap then exit;

  AppKeymap_ApplyUndoList(FKeymapUndoList);
  DoOps_LoadKeymapFrom(GetAppPath(cFileOptionsKeymap), nil);
  FKeymapUndoList.Clear;

  F:= CurrentFrame;
  if Assigned(F) then
  begin
    //dont load lexer-specific cfg if tab changed, and same lexer active
    if FKeymapLastLexer=F.LexerName then exit;
    FKeymapLastLexer:= F.LexerName;

    DoOps_LoadKeymapFrom(
      GetAppKeymap_LexerSpecificConfig(FKeymapLastLexer),
      FKeymapUndoList);
  end;
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
        keys[j]:= TextToShortCut(skeys[j]);
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
    c.Formatted:= true;
    c.Filename:= AFilenameKeymap;
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


procedure TfmMain.DoOps_LoadOptions(const fn: string; var Op: TEditorOps);
var
  c: TJsonConfig;
  S: string;
begin
  c:= TJsonConfig.Create(nil);
  try
    try
      c.Filename:= fn;
    except
      on E: Exception do
      begin
        MsgBox(msgStatusErrorInConfigFile+#13+fn+#13#13+E.Message, mb_ok or mb_iconerror);
        Exit
      end;
    end;

    Op.OpFontName:= c.GetValue(str_FontName, Op.OpFontName);
    Op.OpFontName_i:= c.GetValue(str_FontName_i, Op.OpFontName_i);
    Op.OpFontName_b:= c.GetValue(str_FontName_b, Op.OpFontName_b);
    Op.OpFontName_bi:= c.GetValue(str_FontName_bi, Op.OpFontName_bi);

    Op.OpFontSize:= c.GetValue(str_FontSize, Op.OpFontSize);
    Op.OpFontSize_i:= c.GetValue(str_FontSize_i, Op.OpFontSize_i);
    Op.OpFontSize_b:= c.GetValue(str_FontSize_b, Op.OpFontSize_b);
    Op.OpFontSize_bi:= c.GetValue(str_FontSize_bi, Op.OpFontSize_bi);

    Op.OpFontQuality:= TFontQuality(c.GetValue(str_FontQuality, Ord(Op.OpFontQuality)));
    Op.OpFontLigatures:= c.GetValue(str_FontLigatures, Op.OpFontLigatures);

    UiOps.VarFontName:= c.GetValue(str_UiFontName, UiOps.VarFontName);
    UiOps.VarFontSize:= c.GetValue(str_UiFontSize, UiOps.VarFontSize);
    UiOps.OutputFontName:= c.GetValue(str_UiFontOutputName, UiOps.OutputFontName);
    UiOps.OutputFontSize:= c.GetValue(str_UiFontOutputSize, UiOps.OutputFontSize);
    UiOps.DoubleBuffered:= c.GetValue(str_UiDoubleBuffered, UiOps.DoubleBuffered);

    OptCharSizeProportional:= c.GetValue('unicode_proportional', OptCharSizeProportional);
    OptCharSizeWideAllowed:= c.GetValue('unicode_wide_allowed', OptCharSizeWideAllowed);
    OptCharScaleFullWidth:= c.GetValue('unicode_full_width', OptCharScaleFullWidth);

    UiOps.StatusNoSel:= c.GetValue('ui_statusbar_no_sel', UiOps.StatusNoSel);
    UiOps.StatusSmallSel:= c.GetValue('ui_statusbar_small_sel', UiOps.StatusSmallSel);
    UiOps.StatusStreamSel:= c.GetValue('ui_statusbar_str_sel', UiOps.StatusStreamSel);
    UiOps.StatusColSel:= c.GetValue('ui_statusbar_col_sel', UiOps.StatusColSel);
    UiOps.StatusCarets:= c.GetValue('ui_statusbar_carets', UiOps.StatusCarets);
    UiOps.StatusPanels:= c.GetValue('ui_statusbar_panels', UiOps.StatusPanels);
    UiOps.StatusHeight:= c.GetValue('ui_statusbar_height', UiOps.StatusHeight);
    UiOps.StatusTime:= Max(1, Min(60, c.GetValue('ui_statusbar_time', UiOps.StatusTime)));
    UiOps.StatusTabsize:= c.GetValue('ui_statusbar_tabsize', UiOps.StatusTabsize);

    S:= c.GetValue('ui_statusbar_wrap', UiOps.StatusWrap[0]+','+UiOps.StatusWrap[1]+','+UiOps.StatusWrap[2]);
    UiOps.StatusWrap[0]:= SGetItem(S);
    UiOps.StatusWrap[1]:= SGetItem(S);
    UiOps.StatusWrap[2]:= SGetItem(S);

    UiOps.ShowActiveBorder:= c.GetValue('ui_active_border', UiOps.ShowActiveBorder);
    UiOps.ShowTitlePath:= c.GetValue('ui_title_path', UiOps.ShowTitlePath);
    UiOps.ShowLastFiles:= c.GetValue('ui_reopen_session', UiOps.ShowLastFiles);
    UiOps.FloatGroupsInTaskbar:= c.GetValue('ui_float_groups_in_taskbar', UiOps.FloatGroupsInTaskbar);
    UiOps.OneInstance:= c.GetValue('ui_one_instance', UiOps.OneInstance);

    UiOps.NotifEnabled:= c.GetValue('ui_notif', UiOps.NotifEnabled);
    UiOps.NotifTimeSec:= Max(1, c.GetValue('ui_notif_time', UiOps.NotifTimeSec));

    UiOps.NonTextFiles:= c.GetValue('ui_non_text', UiOps.NonTextFiles);
    UiOps.NonTextFilesBufferKb:= Max(1, Min(1024, c.GetValue('ui_non_text_buffer', UiOps.NonTextFilesBufferKb)));
    UiOps.ReloadFollowTail:= c.GetValue('ui_reload_tail', UiOps.ReloadFollowTail);
    UiOps.ReloadUnsavedConfirm:= c.GetValue('ui_reload_unsaved_confirm', UiOps.ReloadUnsavedConfirm);
    UiOps.FullScreen:= c.GetValue('ui_fullscreen', UiOps.FullScreen);

    //option deleted
    if c.GetValue('backup', '')<>'' then
      MsgBox('Option "backup" was deleted, remove it from user config.'#10+
             'Use good plugin "Backup File" instead.',
             MB_OK or MB_ICONWARNING);

    UiOps.MouseGotoDefinition:= c.GetValue('mouse_goto_definition', UiOps.MouseGotoDefinition);
    UiOps.LogDebug:= c.GetValue('log_debug', UiOps.LogDebug);
    UiOps.LogConsole:= c.GetValue('log_console', UiOps.LogConsole);

    UiOps.HotkeyFindDialog:= c.GetValue('find_hotkey_find_dlg', UiOps.HotkeyFindDialog);
    UiOps.HotkeyReplaceDialog:= c.GetValue('find_hotkey_replace_dlg', UiOps.HotkeyReplaceDialog);
    UiOps.HotkeyFindFirst:= c.GetValue('find_hotkey_find_first', UiOps.HotkeyFindFirst);
    UiOps.HotkeyFindNext:= c.GetValue('find_hotkey_find_next', UiOps.HotkeyFindNext);
    UiOps.HotkeyFindPrev:= c.GetValue('find_hotkey_find_prev', UiOps.HotkeyFindPrev);
    UiOps.HotkeyReplaceAndFindNext:= c.GetValue('find_hotkey_replace', UiOps.HotkeyReplaceAndFindNext);
    UiOps.HotkeyReplaceNoFindNext:= c.GetValue('find_hotkey_replace_no_next', UiOps.HotkeyReplaceNoFindNext);
    UiOps.HotkeyReplaceAll:= c.GetValue('find_hotkey_replace_all', UiOps.HotkeyReplaceAll);
    UiOps.HotkeyCountAll:= c.GetValue('find_hotkey_count_all', UiOps.HotkeyCountAll);
    UiOps.HotkeySelectAll:= c.GetValue('find_hotkey_select_all', UiOps.HotkeySelectAll);
    UiOps.HotkeyMarkAll:= c.GetValue('find_hotkey_mark_all', UiOps.HotkeyMarkAll);
    UiOps.HotkeyToggleRegex:= c.GetValue('find_hotkey_op_regex', UiOps.HotkeyToggleRegex);
    UiOps.HotkeyToggleCaseSens:= c.GetValue('find_hotkey_op_case', UiOps.HotkeyToggleCaseSens);
    UiOps.HotkeyToggleWords:= c.GetValue('find_hotkey_op_words', UiOps.HotkeyToggleWords);
    UiOps.HotkeyToggleWrapped:= c.GetValue('find_hotkey_op_wrapped', UiOps.HotkeyToggleWrapped);
    UiOps.HotkeyToggleInSelect:= c.GetValue('find_hotkey_op_in_sel', UiOps.HotkeyToggleInSelect);
    UiOps.HotkeyToggleMultiline:= c.GetValue('find_hotkey_op_multiline', UiOps.HotkeyToggleMultiline);
    UiOps.HotkeyToggleConfirmRep:= c.GetValue('find_hotkey_op_confirm_rep', UiOps.HotkeyToggleConfirmRep);

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
    Op.OpNumbersFontSize:= c.GetValue('numbers_font_size', Op.OpNumbersFontSize);
    Op.OpNumbersStyle:= c.GetValue('numbers_style', Op.OpNumbersStyle);
    Op.OpNumbersForCarets:= c.GetValue('numbers_for_carets', Op.OpNumbersForCarets);
    Op.OpNumbersCenter:= c.GetValue('numbers_center', Op.OpNumbersCenter);

    Op.OpRulerShow:= c.GetValue('ruler_show', Op.OpRulerShow);
    Op.OpRulerNumeration:= c.GetValue('ruler_numeration', Op.OpRulerNumeration);
    Op.OpRulerFontSize:= c.GetValue('ruler_font_size', Op.OpRulerFontSize);
    Op.OpRulerSize:= c.GetValue('ruler_size', Op.OpRulerSize);
    Op.OpRulerTextIndent:= c.GetValue('ruler_text_indent', Op.OpRulerTextIndent);

    Op.OpMinimapShow:= c.GetValue('minimap_show', Op.OpMinimapShow);
    Op.OpMinimapShowSelAlways:= c.GetValue('minimap_sel_always', Op.OpMinimapShowSelAlways);
    Op.OpMinimapShowSelBorder:= c.GetValue('minimap_sel_border', Op.OpMinimapShowSelBorder);
    Op.OpMinimapCharWidth:= c.GetValue('minimap_char_width', Op.OpMinimapCharWidth);
    Op.OpMinimapAtLeft:= c.GetValue('minimap_at_left', Op.OpMinimapAtLeft);
    Op.OpMinimapTooltipShow:= c.GetValue('minimap_tooltip_show', Op.OpMinimapTooltipShow);
    Op.OpMinimapTooltipLineCount:= c.GetValue('minimap_tooltip_line_count', Op.OpMinimapTooltipLineCount);
    Op.OpMinimapTooltipWidth:= c.GetValue('minimap_tooltip_width', Op.OpMinimapTooltipWidth);

    Op.OpMicromapShow:= c.GetValue('micromap_show', Op.OpMicromapShow);
    Op.OpMicromapWidth:= c.GetValue('micromap_width', Op.OpMicromapWidth);

    Op.OpMarginFixed:= c.GetValue('margin', Op.OpMarginFixed);
    Op.OpMarginString:= c.GetValue('margin_string', Op.OpMarginString);
    Op.OpStaplesStyle:= c.GetValue('staples_style', Op.OpStaplesStyle);

    Op.OpUnprintedShow:= c.GetValue('unprinted_show', Op.OpUnprintedShow);
    Op.OpUnprintedSpaces:= c.GetValue('unprinted_spaces', Op.OpUnprintedSpaces);
    Op.OpUnprintedSpacesTrailing:= c.GetValue('unprinted_spaces_trailing', Op.OpUnprintedSpacesTrailing);
    Op.OpUnprintedEnds:= c.GetValue('unprinted_ends', Op.OpUnprintedEnds);
    Op.OpUnprintedEndDetails:= c.GetValue('unprinted_end_details', Op.OpUnprintedEndDetails);
    Op.OpUnprintedReplaceSpec:= c.GetValue('unprinted_replace_spec', Op.OpUnprintedReplaceSpec);
    Op.OpUnprintedReplaceToCode:= c.GetValue('unprinted_replace_to_code', Op.OpUnprintedReplaceToCode);

    Op.OpUnprintedEndArrow:= c.GetValue('unprinted_end_arrow', Op.OpUnprintedEndArrow);
    Op.OpUnprintedTabArrowLen:= c.GetValue('unprinted_tab_len', Op.OpUnprintedTabArrowLen);
    Op.OpUnprintedSpaceDotScale:= c.GetValue('unprinted_space_dot_scale', Op.OpUnprintedSpaceDotScale);
    Op.OpUnprintedEndDotScale:= c.GetValue('unprinted_end_dot_scale', Op.OpUnprintedEndDotScale);
    Op.OpUnprintedEndFontScale:= c.GetValue('unprinted_end_font_scale', Op.OpUnprintedEndFontScale);
    Op.OpUnprintedTabPointerScale:= c.GetValue('unprinted_tab_ptr_scale', Op.OpUnprintedTabPointerScale);

    Op.OpWrapMode:= c.GetValue('wrap_mode', Op.OpWrapMode);
    Op.OpWrapIndented:= c.GetValue('wrap_indented', Op.OpWrapIndented);
    Op.OpWrapEnabledMaxLines:= c.GetValue('wrap_enabled_max_lines', Op.OpWrapEnabledMaxLines);

    Op.OpUndoLimit:= c.GetValue('undo_limit', Op.OpUndoLimit);
    Op.OpUndoGrouped:= c.GetValue('undo_grouped', Op.OpUndoGrouped);
    Op.OpUndoAfterSave:= c.GetValue('undo_after_save', Op.OpUndoAfterSave);

    Op.OpCaretBlinkTime:= c.GetValue('caret_blink_time', Op.OpCaretBlinkTime);
    Op.OpCaretBlinkEn:= c.GetValue('caret_blink_en', Op.OpCaretBlinkEn);
    Op.OpCaretShapeNorm:= c.GetValue('caret_shape', Op.OpCaretShapeNorm);
    Op.OpCaretShapeOvr:= c.GetValue('caret_shape_ovr', Op.OpCaretShapeOvr);
    Op.OpCaretShapeRO:= c.GetValue('caret_shape_ro', Op.OpCaretShapeRO);
    Op.OpCaretVirtual:= c.GetValue('caret_after_end', Op.OpCaretVirtual);
    Op.OpCaretMulti:= c.GetValue('caret_multi', Op.OpCaretMulti);
    Op.OpCaretAfterPasteColumn:= c.GetValue('caret_after_paste_column', Op.OpCaretAfterPasteColumn);
    Op.OpCaretsAddedToColumnSel:= c.GetValue('carets_with_column_sel', Op.OpCaretsAddedToColumnSel);

    Op.OpShowCurLine:= c.GetValue('show_cur_line', Op.OpShowCurLine);
    Op.OpShowCurLineMinimal:= c.GetValue('show_cur_line_minimal', Op.OpShowCurLineMinimal);
    Op.OpShowCurLineOnlyFocused:= c.GetValue('show_cur_line_only_focused', Op.OpShowCurLineOnlyFocused);
    Op.OpShowCurCol:= c.GetValue('show_cur_column', Op.OpShowCurCol);
    Op.OpShowLastLineOnTop:= c.GetValue('show_last_line_on_top', Op.OpShowLastLineOnTop);
    Op.OpShowFullBackgroundSel:= c.GetValue('show_full_sel_bg', Op.OpShowFullBackgroundSel);
    Op.OpShowFullBackgroundSyntax:= c.GetValue('show_full_syntax_bg', Op.OpShowFullBackgroundSyntax);
    Op.OpCopyLineIfNoSel:= c.GetValue('copy_line_if_no_sel', Op.OpCopyLineIfNoSel);
    Op.OpCutLineIfNoSel:= c.GetValue('cut_line_if_no_sel', Op.OpCutLineIfNoSel);
    Op.OpCopyColumnAlignedBySpaces:= c.GetValue('copy_column_aligned_by_spaces', Op.OpCopyColumnAlignedBySpaces);
    Op.OpSavingTrimSpaces:= c.GetValue('saving_trim_spaces', Op.OpSavingTrimSpaces);
    Op.OpSavingForceFinalEol:= c.GetValue('saving_force_final_eol', Op.OpSavingForceFinalEol);
    Op.OpShowHintOnVertScroll:= c.GetValue('show_hint_on_vert_scroll', Op.OpShowHintOnVertScroll);
    Op.OpCenteringWidth:= c.GetValue('centering_width', Op.OpCenteringWidth);
    Op.OpCenteringForDistractionFree:= c.GetValue('centering_for_distraction_free', Op.OpCenteringForDistractionFree);
    Op.OpHideHorizScrollbar:= c.GetValue('hide_horiz_scrollbar', Op.OpHideHorizScrollbar);

    Op.OpLexerDynamicHiliteEnabled:= c.GetValue('lexer_dynamic_hilite', Op.OpLexerDynamicHiliteEnabled);
    Op.OpLexerDynamicHiliteMaxLines:= c.GetValue('lexer_dynamic_hilite_max_lines', Op.OpLexerDynamicHiliteMaxLines);
    Op.OpLexerLineSeparators:= c.GetValue('lexer_line_separators', Op.OpLexerLineSeparators);

    Op.OpWordChars:= c.GetValue('word_chars', Op.OpWordChars);
    Op.OpHexChars:= c.GetValue('hex_chars', Op.OpHexChars);
    Op.OpFoldStyle:= c.GetValue('fold_style', Op.OpFoldStyle);
    Op.OpFoldTooltipShow:= c.GetValue('fold_tooltip_show', Op.OpFoldTooltipShow);

    UiOps.LexerThemes:= c.GetValue('ui_lexer_themes', UiOps.LexerThemes);
    UiOps.LexerMenuGrouped:= c.GetValue('ui_lexers_grouped', UiOps.LexerMenuGrouped);
    UiOps.LexerDelayedParsingPause:= c.GetValue('lexer_delayed_parsing_pause', UiOps.LexerDelayedParsingPause);
    UiOps.LexerDelayedParsingSize:= c.GetValue('lexer_delayed_parsing_size', UiOps.LexerDelayedParsingSize);

    UiOps.SidebarShow:= c.GetValue('ui_sidebar_show', UiOps.SidebarShow);
    UiOps.SidebarOnRight:= c.GetValue('ui_sidebar_on_right', UiOps.SidebarOnRight);
    UiOps.SidebarTheme:= c.GetValue('ui_sidebar_theme', UiOps.SidebarTheme);
    UiOps.TreeTheme:= c.GetValue('ui_tree_theme', UiOps.TreeTheme);
    UiOps.ToolBarTheme:= c.GetValue('ui_toolbar_theme', UiOps.ToolBarTheme);

    UiOps.PyLibrary:= c.GetValue('pylib'+cOptionSystemSuffix, UiOps.PyLibrary);
    UiOps.PictureTypes:= c.GetValue('picture_types', UiOps.PictureTypes);
    UiOps.MaxFileSizeToOpen:= c.GetValue('ui_max_size_open', UiOps.MaxFileSizeToOpen);
    UiOps.MaxFileSizeForLexer:= c.GetValue('ui_max_size_lexer', UiOps.MaxFileSizeForLexer);
    UiOps.MaxLinesForTree:= c.GetValue('ui_max_lines_codetree', UiOps.MaxLinesForTree);

    UiOps.AutocompleteHtml:= c.GetValue('autocomplete_html', UiOps.AutocompleteHtml);
    UiOps.AutocompleteCss:= c.GetValue('autocomplete_css', UiOps.AutocompleteCss);

    UiOps.AutocompleteHtml_LexersRegex:= c.GetValue('autocomplete_html_lexers', UiOps.AutocompleteHtml_LexersRegex);
    UiOps.AutocompleteCss_LexersRegex:= c.GetValue('autocomplete_css_lexers', UiOps.AutocompleteCss_LexersRegex);

    UiOps.AutocompleteAutoshowCharCount:= c.GetValue('autocomplete_autoshow_chars', UiOps.AutocompleteAutoshowCharCount);
    //UiOps.AutocompleteAutoshowLexers:= c.GetValue('autocomplete_autoshow_lexers', UiOps.AutocompleteAutoshowLexers);
    UiOps.AutocompleteTriggerChars:= c.GetValue('autocomplete_trigger_chars', '');
    UiOps.AutocompleteAddOpeningBracket:= c.GetValue('autocomplete_add_opening_bracket', UiOps.AutocompleteAddOpeningBracket);
    UiOps.AutocompleteUpDownAtEdge:= c.GetValue('autocomplete_up_down_at_edge', UiOps.AutocompleteUpDownAtEdge);
    UiOps.AutoCloseBrackets:= c.GetValue('auto_close_brackets', UiOps.AutoCloseBrackets);

    UiOps.FindSuggestSel:= c.GetValue('find_suggest_sel', UiOps.FindSuggestSel);
    UiOps.FindSuggestWord:= c.GetValue('find_suggest_cur_word', UiOps.FindSuggestWord);
    UiOps.FindSuggestInSelection:= c.GetValue('find_suggest_in_selection', UiOps.FindSuggestInSelection);
    UiOps.FindSelCase:= c.GetValue('find_sel_case', UiOps.FindSelCase);
    UiOps.FindShow_FindFirst:= c.GetValue('find_show_findfirst', UiOps.FindShow_FindFirst);
    UiOps.FindShow_MarkALl:= c.GetValue('find_show_mark_all', UiOps.FindShow_MarkALl);
    UiOps.FindShow_SelectAll:= c.GetValue('find_show_select_all', UiOps.FindShow_SelectAll);
    UiOps.FindIndentVert:= c.GetValue('find_indent_vert', UiOps.FindIndentVert);
    UiOps.FindIndentHorz:= c.GetValue('find_indent_horz', UiOps.FindIndentHorz);
    UiOps.FindMultiLineScale:= c.GetValue('find_multiline_scale', UiOps.FindMultiLineScale);
    UiOps.FindSeparateForm:= c.GetValue('find_separate_form', UiOps.FindSeparateForm);

    UiOps.TabsDisabled:= c.GetValue('ui_tab_disabled', UiOps.TabsDisabled);
    UiOps.TabMultiline:= c.GetValue('ui_tab_multiline', UiOps.TabMultiline);
    UiOps.TabAngled:= c.GetValue('ui_tab_angled', UiOps.TabAngled);
    UiOps.TabFlat:= c.GetValue('ui_tab_flat', UiOps.TabFlat);
    UiOps.TabWidth:= c.GetValue('ui_tab_size_x', UiOps.TabWidth);
    UiOps.TabWidthMin:= c.GetValue('ui_tab_size_x_min', UiOps.TabWidthMin);
    UiOps.TabHeight:= c.GetValue('ui_tab_size_y', UiOps.TabHeight);
    UiOps.TabHeightInner:= UiOps.TabHeight-1;
    UiOps.TabPosition:= c.GetValue('ui_tab_position', UiOps.TabPosition);
    UiOps.TabColorFull:= c.GetValue('ui_tab_fullcolor', UiOps.TabColorFull);
    UiOps.TabShowX:= c.GetValue('ui_tab_show_x', UiOps.TabShowX);
    UiOps.TabShowPlus:= c.GetValue('ui_tab_show_plus', UiOps.TabShowPlus);
    UiOps.TabDblClickClose:= c.GetValue('ui_tab_2click_closes', UiOps.TabDblClickClose);
    UiOps.TabNumbers:= c.GetValue('ui_tab_numbers', UiOps.TabNumbers);
    UiOps.TabNewNearCurrent:= c.GetValue('ui_tab_new_near_current', UiOps.TabNewNearCurrent);
    UiOps.TabButtonLayout:= c.GetValue('ui_tab_button_layout', UiOps.TabButtonLayout);
    UiOps.TabPreviewFontStyle:= c.GetValue('ui_tab_preview_font_style', UiOps.TabPreviewFontStyle);

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
    UiOps.InitialDir:= c.GetValue('ui_initial_dir', UiOps.InitialDir);

    UiOps.ExportHtmlNumbers:= c.GetValue('export_html_numbers', UiOps.ExportHtmlNumbers);
    UiOps.ExportHtmlFontName:= c.GetValue('export_html_font_name', UiOps.ExportHtmlFontName);
    UiOps.ExportHtmlFontSize:= c.GetValue('export_html_font_size', UiOps.ExportHtmlFontSize);

    UiOps.TreeAutoSync:= c.GetValue('ui_tree_auto_sync', UiOps.TreeAutoSync);
    UiOps.TreeTimeFill:= c.GetValue('ui_tree_time_fill', UiOps.TreeTimeFill);
    UiOps.TreeTimeFocus:= c.GetValue('ui_tree_time_focus', UiOps.TreeTimeFocus);
    UiOps.TreeShowLines:= c.GetValue('ui_tree_show_lines', UiOps.TreeShowLines);
    UiOps.TreeShowIcons:= c.GetValue('ui_tree_icons', UiOps.TreeShowIcons);
    UiOps.TreeShowTooltips:= c.GetValue('ui_tree_show_tooltips', UiOps.TreeShowTooltips);
    UiOps.TreeCache:= c.GetValue('ui_tree_cache', UiOps.TreeCache);

    UiOps.PyChangeSlow:= c.GetValue('py_change_slow', UiOps.PyChangeSlow);
    UiOps.PyInitLog:= c.GetValue('py_init_log', UiOps.PyInitLog);

    UiOps.NewdocLexer:= c.GetValue('newdoc_lexer', UiOps.NewdocLexer);
    UiOps.NewdocEnc:= c.GetValue('newdoc_encoding', UiOps.NewdocEnc);
    UiOps.NewdocEnds:= c.GetValue('newdoc_eol', UiOps.NewdocEnds);

    UiOps.DefaultEncUtf8:= c.GetValue(str_DefEncodingIsUtf8, UiOps.DefaultEncUtf8);
    UiOps.ViewerBinaryWidth:= c.GetValue('viewer_binary_width', UiOps.ViewerBinaryWidth);

    Op.OpIndentAuto:= c.GetValue('indent_auto', Op.OpIndentAuto);
    Op.OpIndentAutoKind:= c.GetValue('indent_kind', Op.OpIndentAutoKind);
    Op.OpIndentSize:= c.GetValue('indent_size', Op.OpIndentSize);
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

    Op.OpKeyBackspaceUnindent:= c.GetValue('key_bksp_unindent', Op.OpKeyBackspaceUnindent);
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

procedure TfmMain.DoOps_LoadOptionsLexerSpecific(F: TEditorFrame);
var
  fn: string;
  Ops: TEditorOps;
begin
  if F=nil then exit;
  DoOps_ResetLexerSpecificOptions;

  fn:= GetAppLexerSpecificConfig(F.LexerName);
  if FileExistsUTF8(fn) then
  begin
    Ops:= EditorOps;
    DoOps_LoadOptions(fn, Ops);
    DoApplyFrameOps(F, Ops, true);
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


procedure TfmMain.DoOps_DialogFont(
  var OpName: string; var OpSize: integer;
  const AConfigStrName, AConfigStrSize: string);
var
  cfg: TJsonConfig;
  fn: string;
begin
  fn:= GetAppPath(cFileOptionsUser);
  if not FileExistsUTF8(fn) then
  begin
    FCreateFile(fn, true);
    if not FileExistsUTF8(fn) then exit;
  end;

  FontDlg.Font.Name:= OpName;
  FontDlg.Font.Size:= OpSize;
  if not FontDlg.Execute then exit;
  OpName:= FontDlg.Font.Name;
  OpSize:= FontDlg.Font.Size;

  cfg:= TJSONConfig.Create(nil);
  try
    cfg.Formatted:= true;
    cfg.Filename:= fn;
    cfg.SetValue(AConfigStrName, OpName);
    cfg.SetValue(AConfigStrSize, OpSize);
  finally
    try
      cfg.Free;
    except
      MsgBox(msgCannotSaveUserConf, MB_OK or MB_ICONERROR);
    end;
  end;
end;

procedure TfmMain.DoOps_LoadOptionsAndApplyAll;
begin
  DoOps_LoadOptions(GetAppPath(cFileOptionsUser), EditorOps);
  DoApplyAllOps;
  MsgStatus(msgStatusReadingOps);
end;


