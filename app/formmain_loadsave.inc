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
    c.Formatted:= true; //needs last jsonConf.pp
    c.Filename:= GetAppPath(cFileOptHistory);

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

      c.SetValue('show_toolbar', ShowToolbar);
      c.SetValue('show_status', ShowStatus);
      c.SetValue('show_sidepanel', ShowSidePanel);
      c.SetValue('show_bottom', ShowBottom);
      c.SetValue('size_side', PanelLeft.Width);
      c.SetValue('size_bottom', PanelBottom.Height);
    end;

    ////recents
    SSaveStringsToFile(c, '/recent_files', FListRecents, UiOps.MaxHistoryMenu);
    if Assigned(fmFind) then
    begin
      SSaveStringsToFile(c, '/list_find', fmFind.edFind.Items, UiOps.MaxHistoryEdits);
      SSaveStringsToFile(c, '/list_replace', fmFind.edRep.Items, UiOps.MaxHistoryEdits);
    end;

    SSaveStringsToFile(c, '/list_console', fmConsole.ed.Items, UiOps.MaxHistoryEdits);

    ////finder
    c.SetValue('/finder/regex', FFinder.OptRegex);
    c.SetValue('/finder/case', FFinder.OptCase);
    c.SetValue('/finder/words', FFinder.OptWords);
    c.SetValue('/finder/back', FFinder.OptBack);
    c.SetValue('/finder/confirm', FFinder.OptConfirmReplace);
    c.SetValue('/finder/wrap', FFinder.OptWrapped);

    ////state
    c.SetValue('theme', FThemeName);
    c.SetValue('lang', FLangName);
    c.SetValue('session', FSessionFilename);

    DoOps_SaveHistory_GroupView(c);
  finally
    c.Free;
  end;

  if UiOps.ShowLastFiles then
    DoOps_SaveSession(FSessionFilename);
end;


procedure TfmMain.DoOps_SaveSession(fn_session: string);
var
  c: TJSONConfig;
  F: TEditorFrame;
  nGroup, nTab, i: integer;
  path: string;
begin
  if fn_session='' then exit;
  if ExtractFileDir(fn_session)='' then
    fn_session:= GetAppPath(cDirSettings)+DirectorySeparator+fn_session;

  DeleteFile(fn_session);

  c:= TJSONConfig.Create(nil);
  try
    c.Filename:= fn_session;
    c.Formatted:= true;

    DoOps_SaveHistory_GroupView(c);

    for i:= Low(TATGroupsNums) to High(TATGroupsNums) do
      c.SetValue('/tab_index/g'+Inttostr(i), Groups.Pages[i].Tabs.TabIndex);
    c.SetValue('/tab_index/g_active', Groups.PagesIndexOf(Groups.PagesCurrent));

    for i:= 0 to FrameCount-1 do
    begin
      F:= Frames[i];
      if F.Filename='' then Continue;
      if not F.IsText then Continue;

      path:= '/'+Format('%3.3d', [i]);
      Groups.PagesAndTabIndexOfControl(F, nGroup, nTab);

      c.SetValue(path+'/file', F.Filename);
      c.SetValue(path+'/group', nGroup);
      F.DoSaveHistoryEx(c, path);
    end;
  finally
    c.Free;
  end;
end;

procedure TfmMain.DoOps_LoadSession(fn_session: string);
var
  c: TJSONConfig;
  list: TStringlist;
  fn: string;
  F: TEditorFrame;
  nGroup, i: integer;
begin
  if fn_session='' then exit;
  if ExtractFileDir(fn_session)='' then
    fn_session:= GetAppPath(cDirSettings)+DirectorySeparator+fn_session;

  DoFileCloseAll;

  c:= TJSONConfig.Create(nil);
  list:= TStringlist.Create;
  try
    c.Filename:= fn_session;

    DoOps_LoadHistory_GroupView(c);

    c.EnumSubKeys('/', list);
    list.Sort;

    for i:= 0 to list.Count-1 do
    begin
      fn:= c.GetValue('/'+list[i]+'/file', '');
      nGroup:= c.GetValue('/'+list[i]+'/group', Low(TATGroupsNums));

      if nGroup in [Low(TATGroupsNums)..High(TATGroupsNums)] then
        Groups.PagesCurrent:= Groups.Pages[nGroup];

      if FileExistsUTF8(fn) then
      begin
        F:= DoFileOpen(fn);
        if F<>nil then
        begin
          F.NotInRecents:= true;
          F.DoLoadHistoryEx(c, '/'+list[i]);
        end;
      end;
    end;

    for i:= Low(TATGroupsNums) to High(TATGroupsNums) do
      Groups.Pages[i].Tabs.TabIndex:= c.GetValue('/tab_index/g'+Inttostr(i), 0);

    nGroup:= c.GetValue('/tab_index/g_active', Low(TATGroupsNums));
    if nGroup in [Low(TATGroupsNums)..High(TATGroupsNums)] then
      Groups.PagesCurrent:= Groups.Pages[nGroup];
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

procedure TfmMain.DoOps_LoadHistory;
const
  cReservePixels=70;
var
  c: TJSONConfig;
  nPosX, nPosY, nSizeX, nSizeY: integer;
begin
  c:= TJSONConfig.Create(nil);
  try
    c.Filename:= GetAppPath(cFileOptHistory);

    ////form size
    nPosX:= c.GetValue('/dialog/posx', 100);
    nPosY:= c.GetValue('/dialog/posy', 100);

    if (nPosX>Screen.DesktopWidth-cReservePixels) or
       (nPosY>Screen.DesktopHeight-cReservePixels) then
    begin
      nPosX:= 0;
      nPosY:= 0;
    end;

    nSizeX:= c.GetValue('/dialog/sizex', 800);
    nSizeY:= c.GetValue('/dialog/sizey', 550);
    SetBounds(nPosX, nPosY, nSizeX, nSizeY);

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

    ////state
    ShowToolbar:= c.GetValue('show_toolbar', true);
    ShowStatus:= c.GetValue('show_status', true);
    ShowSidePanel:= c.GetValue('show_sidepanel', true);
    ShowBottom:= c.GetValue('show_bottom', false);
    PanelLeft.Width:= c.GetValue('size_side', 200);
    PanelBottom.Height:= c.GetValue('size_bottom', 200);

    ////theme
    FThemeName:= c.GetValue('theme', '');
    FLangName:= c.GetValue('lang', '');
    FSessionFilename:= c.GetValue('session', FSessionFilename);

    if FLangName<>'' then
      DoApplyLang;

    if FThemeName<>'' then
    begin
      DoLoadTheme(GetAppPath(cDirDataThemes)+DirectorySeparator+FThemeName+'.json', Theme);
      DoApplyTheme;
    end;

    DoOps_LoadHistory_GroupView(c);
  finally
    c.Free;
  end;

  if UiOps.ShowLastFiles then
    DoOps_LoadSession(FSessionFilename);
end;

procedure TfmMain.DoOps_LoadKeymap;
var
  c: TJSONConfig;
  slist, skeys: TStringList;
  //
  procedure DoReadConfigToKeys(path: string; var keys: TATKeyArray);
  var
    j: integer;
  begin
    c.GetValue(path, skeys, '');
    for j:= 0 to skeys.count-1 do
      if skeys[j]<>'' then
        keys[j]:= TextToShortCut(skeys[j]);
  end;
  //
  procedure DoReadConfigToKeys_Old(path: string; var keys: TATKeyArray);
  var
    j: integer;
  begin
    c.GetValue(path, skeys, '');
    for j:= 0 to skeys.count-1 do
      if skeys[j]<>'' then
        keys[j]:= StrToIntDef(skeys[j], 0);
  end;
  //
  procedure DoReadConfigOneKey(const path_s, path_k: string; var KeyItem: TATKeyArray);
  var
    sl: TStringlist;
    i: integer;
  begin
    FillChar(KeyItem, SizeOf(KeyItem), 0);
    DoReadConfigToKeys(path_s, KeyItem);

    //this crap is support of old config: k1/k2 keys are old, s1/s2 keys are new,
    //let's read k1/k2 and write s1/s2, and del k1/k2
    if KeyItem[0]=0 then
    begin
      DoReadConfigToKeys_Old(path_k, KeyItem);
      if KeyItem[0]=0 then exit;

      sl:= TStringlist.Create;
      try
        sl.clear;
        for i:= 0 to High(KeyItem) do
          if KeyItem[i]<>0 then
            sl.Add(ShortCutToText(KeyItem[i]));
        c.SetValue(path_s, sl);
        c.DeleteValue(path_k);
      finally
        sl.Free;
      end;
    end;
  end;
  //
var
  ncmd, nitem, i: integer;
begin
  c:= TJSONConfig.Create(nil);
  c.Formatted:= true;
  slist:= TStringList.Create;
  skeys:= TStringList.Create;

  try
    c.Filename:= GetAppPath(cFileOptKeymap);
    c.EnumSubKeys('/', slist);
    for i:= 0 to slist.count-1 do
    begin
      //plugin item 'module,method'
      if Pos(',', slist[i])>0 then
      begin
        ncmd:= CommandPlugins_GetIndexFromModuleAndMethod(slist[i]);
        if ncmd<0 then Continue;
        Inc(ncmd, cmdFirstPluginCommand);
      end
      else //usual item
      begin
        ncmd:= StrToIntDef(slist[i], -1);
        if ncmd<0 then Continue;
      end;

      nitem:= Keymap.IndexOf(ncmd);
      if nitem<0 then Continue;

      DoReadConfigOneKey(slist[i]+'/s1', slist[i]+'/k1', Keymap[nitem].Keys1);
      DoReadConfigOneKey(slist[i]+'/s2', slist[i]+'/k2', Keymap[nitem].Keys2);
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
begin
  c:= TJsonConfig.Create(nil);
  try
    try
      c.Filename:= fn;
    except
      on E: Exception do
      begin
        MsgBox('Error in config file'#13+fn+#13#13+E.Message, mb_ok or mb_iconerror);
        Exit
      end;
    end;

    Op.OpFontName:= c.GetValue(str_FontName, Op.OpFontName);
    Op.OpFontSize:= c.GetValue(str_FontSize, Op.OpFontSize);
    Op.OpFontQuality:= TFontQuality(c.GetValue(str_FontQuality, Ord(Op.OpFontQuality)));

    UiOps.VarFontName:= c.GetValue(str_UiFontName, UiOps.VarFontName);
    UiOps.VarFontSize:= c.GetValue(str_UiFontSize, UiOps.VarFontSize);

    UiOps.OutputFontName:= c.GetValue(str_UiFontOutputName, UiOps.OutputFontName);
    UiOps.OutputFontSize:= c.GetValue(str_UiFontOutputSize, UiOps.OutputFontSize);

    UiOps.StatusNoSel:= c.GetValue('ui_statusbar_no_sel', UiOps.StatusNoSel);
    UiOps.StatusSmallSel:= c.GetValue('ui_statusbar_small_sel', UiOps.StatusSmallSel);
    UiOps.StatusStreamSel:= c.GetValue('ui_statusbar_str_sel', UiOps.StatusStreamSel);
    UiOps.StatusColSel:= c.GetValue('ui_statusbar_col_sel', UiOps.StatusColSel);
    UiOps.StatusCarets:= c.GetValue('ui_statusbar_carets', UiOps.StatusCarets);
    UiOps.StatusPanels:= c.GetValue('ui_statusbar_panels', UiOps.StatusPanels);
    UiOps.StatusHeight:= c.GetValue('ui_statusbar_height', UiOps.StatusHeight);
    UiOps.StatusTime:= Max(1, Min(60, c.GetValue('ui_statusbar_time', UiOps.StatusTime)));
    UiOps.StatusTabsize:= c.GetValue('ui_statusbar_tabsize', UiOps.StatusTabsize);

    UiOps.ShowTitlePath:= c.GetValue('ui_title_path', UiOps.ShowTitlePath);
    UiOps.ShowLastFiles:= c.GetValue('ui_reopen_session', UiOps.ShowLastFiles);
    UiOps.OneInstance:= c.GetValue('ui_one_instance', UiOps.OneInstance);

    UiOps.NotifEnabled:= c.GetValue('ui_notif', UiOps.NotifEnabled);
    UiOps.NotifTimeSec:= Max(1, c.GetValue('ui_notif_time', UiOps.NotifTimeSec));

    UiOps.NonTextFiles:= c.GetValue('ui_non_text', UiOps.NonTextFiles);
    UiOps.NonTextFilesBufferKb:= Max(1, Min(1024, c.GetValue('ui_non_text_buffer', UiOps.NonTextFilesBufferKb)));
    UiOps.LexerMenuGrouped:= c.GetValue('ui_lexers_grouped', UiOps.LexerMenuGrouped);
    UiOps.ReloadFollowTail:= c.GetValue('ui_reload_tail', UiOps.ReloadFollowTail);
    UiOps.BackupMode:= c.GetValue('backup', UiOps.BackupMode);

    Op.OpSpacingX:= c.GetValue('spacing_x', Op.OpSpacingX);
    Op.OpSpacingY:= c.GetValue('spacing_y', Op.OpSpacingY);

    Op.OpTabSize:= c.GetValue('tab_size', Op.OpTabSize);
    Op.OpTabSpaces:= c.GetValue('tab_spaces', Op.OpTabSpaces);

    Op.OpOvrSel:= c.GetValue('ovr_sel', Op.OpOvrSel);
    Op.OpOvrOnPaste:= c.GetValue('ovr_on_paste', Op.OpOvrOnPaste);

    Op.OpUnderlineColorFiles:= c.GetValue('underline_color_files', Op.OpUnderlineColorFiles);
    Op.OpUnderlineColorSize:= c.GetValue('underline_color_size', Op.OpUnderlineColorSize);

    Op.OpLinks:= c.GetValue('links_hilite', Op.OpLinks);
    Op.OpLinksRegex:= c.GetValue('links_regex', Op.OpLinksRegex);

    Op.OpGutterShow:= c.GetValue('gutter_show', Op.OpGutterShow);
    Op.OpGutterFold:= c.GetValue('gutter_fold', Op.OpGutterFold);
    Op.OpGutterFoldAlways:= c.GetValue('gutter_fold_always', Op.OpGutterFoldAlways);
    Op.OpGutterBookmk:= c.GetValue('gutter_bookmarks', Op.OpGutterBookmk);

    Op.OpNumbersShow:= c.GetValue('numbers_show', Op.OpNumbersShow);
    Op.OpNumbersFontSize:= c.GetValue('numbers_font_size', Op.OpNumbersFontSize);
    Op.OpNumbersStyle:= c.GetValue('numbers_style', Op.OpNumbersStyle);
    Op.OpNumbersForCarets:= c.GetValue('numbers_for_carets', Op.OpNumbersForCarets);
    Op.OpNumbersCenter:= c.GetValue('numbers_center', Op.OpNumbersCenter);

    Op.OpRulerShow:= c.GetValue('ruler_show', Op.OpRulerShow);
    Op.OpRulerFontSize:= c.GetValue('ruler_font_size', Op.OpRulerFontSize);
    Op.OpRulerSize:= c.GetValue('ruler_size', Op.OpRulerSize);
    Op.OpRulerTextIndent:= c.GetValue('ruler_text_indent', Op.OpRulerTextIndent);

    Op.OpMinimapShow:= c.GetValue('minimap_show', Op.OpMinimapShow);
    Op.OpMinimapShowSelAlways:= c.GetValue('minimap_sel_always', Op.OpMinimapShowSelAlways);
    Op.OpMinimapShowSelBorder:= c.GetValue('minimap_sel_border', Op.OpMinimapShowSelBorder);
    Op.OpMinimapCharWidth:= c.GetValue('minimap_char_width', Op.OpMinimapCharWidth);
    Op.OpMinimapAtLeft:= c.GetValue('minimap_at_left', Op.OpMinimapAtLeft);

    Op.OpMicromapShow:= c.GetValue('micromap_show', Op.OpMicromapShow);
    Op.OpMicromapWidth:= c.GetValue('micromap_width', Op.OpMicromapWidth);

    Op.OpMargin:= c.GetValue('margin', Op.OpMargin);
    Op.OpMarginString:= c.GetValue('margin_string', Op.OpMarginString);

    Op.OpUnprintedShow:= c.GetValue('unprinted_show', Op.OpUnprintedShow);
    Op.OpUnprintedSpaces:= c.GetValue('unprinted_spaces', Op.OpUnprintedSpaces);
    Op.OpUnprintedEnds:= c.GetValue('unprinted_ends', Op.OpUnprintedEnds);
    Op.OpUnprintedEndDetails:= c.GetValue('unprinted_end_details', Op.OpUnprintedEndDetails);
    Op.OpUnprintedReplaceSpec:= c.GetValue('unprinted_replace_spec', Op.OpUnprintedReplaceSpec);

    Op.OpUnprintedEndArrow:= c.GetValue('unprinted_end_arrow', Op.OpUnprintedEndArrow);
    Op.OpUnprintedTabArrowLen:= c.GetValue('unprinted_tab_len', Op.OpUnprintedTabArrowLen);
    Op.OpUnprintedSpaceDotScale:= c.GetValue('unprinted_space_dot_scale', Op.OpUnprintedSpaceDotScale);
    Op.OpUnprintedEndDotScale:= c.GetValue('unprinted_end_dot_scale', Op.OpUnprintedEndDotScale);
    Op.OpUnprintedEndFontScale:= c.GetValue('unprinted_end_font_scale', Op.OpUnprintedEndFontScale);
    Op.OpUnprintedTabPointerScale:= c.GetValue('unprinted_tab_ptr_scale', Op.OpUnprintedTabPointerScale);

    Op.OpWrapMode:= c.GetValue('wrap_mode', Op.OpWrapMode);
    Op.OpWrapIndented:= c.GetValue('wrap_indented', Op.OpWrapIndented);

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

    Op.OpShowCurLine:= c.GetValue('show_cur_line', Op.OpShowCurLine);
    Op.OpShowCurLineMin:= c.GetValue('show_cur_line_minimal', Op.OpShowCurLineMin);
    Op.OpShowCurCol:= c.GetValue('show_cur_column', Op.OpShowCurCol);
    Op.OpShowLastLineOnTop:= c.GetValue('show_last_line_on_top', Op.OpShowLastLineOnTop);
    Op.OpShowSelectBgFull:= c.GetValue('show_full_sel_bg', Op.OpShowSelectBgFull);
    Op.OpShowSyntaxBgFull:= c.GetValue('show_full_syntax_bg', Op.OpShowSyntaxBgFull);
    Op.OpCopyLineIfNoSel:= c.GetValue('copy_line_if_no_sel', Op.OpCopyLineIfNoSel);
    Op.OpCutLineIfNoSel:= c.GetValue('cut_line_if_no_sel', Op.OpCutLineIfNoSel);
    Op.OpSavingTrimSpaces:= c.GetValue('saving_trim_spaces', Op.OpSavingTrimSpaces);
    Op.OpSavingForceFinalEol:= c.GetValue('saving_force_final_eol', Op.OpSavingForceFinalEol);
    Op.OpShowHintOnVertScroll:= c.GetValue('show_hint_on_vert_scroll', Op.OpShowHintOnVertScroll);
    Op.OpDynHilite:= c.GetValue('dyn_hilite', Op.OpDynHilite);

    Op.OpWordChars:= c.GetValue('word_chars', Op.OpWordChars);
    Op.OpHexChars:= c.GetValue('hex_chars', Op.OpHexChars);

    UiOps.LexerLibFilename:= c.GetValue('lexlib', UiOps.LexerLibFilename);
    UiOps.PyLibrary:= c.GetValue('pylib', UiOps.PyLibrary);
    UiOps.PictureTypes:= c.GetValue('picture_types', UiOps.PictureTypes);

    UiOps.AutocompleteHtml:= c.GetValue('autocomplete_html', UiOps.AutocompleteHtml);
    UiOps.AutocompleteCss:= c.GetValue('autocomplete_css', UiOps.AutocompleteCss);
    UiOps.AutocompleteAutoshowChars:= c.GetValue('autocomplete_autoshow_chars', UiOps.AutocompleteAutoshowChars);
    UiOps.AutocompleteAutoshowLexers:= c.GetValue('autocomplete_autoshow_lexers', UiOps.AutocompleteAutoshowLexers);

    UiOps.FindSuggestSel:= c.GetValue('find_suggest_sel', UiOps.FindSuggestSel);
    UiOps.FindSuggestWord:= c.GetValue('find_suggest_cur_word', UiOps.FindSuggestWord);
    UiOps.FindSelCase:= c.GetValue('find_sel_case', UiOps.FindSelCase);
    UiOps.FindShowFindfirst:= c.GetValue('find_show_findfirst', UiOps.FindShowFindfirst);
    UiOps.FindIndentVert:= c.GetValue('find_indent_vert', UiOps.FindIndentVert);
    UiOps.FindIndentHorz:= c.GetValue('find_indent_horz', UiOps.FindIndentHorz);

    UiOps.TabWidth:= c.GetValue('ui_tab_size_x', UiOps.TabWidth);
    UiOps.TabHeight:= c.GetValue('ui_tab_size_y', UiOps.TabHeight);
    UiOps.TabHeightInner:= UiOps.TabHeight-1;
    UiOps.TabIndentInit:= c.GetValue('ui_tab_indent_x', UiOps.TabIndentInit);
    UiOps.TabAngle:= Max(0, Min(10, c.GetValue('ui_tab_angle', UiOps.TabAngle)));
    UiOps.TabBottom:= c.GetValue('ui_tab_bottom', UiOps.TabBottom);
    UiOps.TabColorFull:= c.GetValue('ui_tab_fullcolor', UiOps.TabColorFull);
    UiOps.TabShowX:= c.GetValue('ui_tab_show_x', UiOps.TabShowX);
    UiOps.TabShowPlus:= c.GetValue('ui_tab_show_plus', UiOps.TabShowPlus);
    UiOps.TabDblClickClose:= c.GetValue('ui_tab_2click_closes', UiOps.TabDblClickClose);
    UiOps.TabNumbers:= c.GetValue('ui_tab_numbers', UiOps.TabNumbers);

    UiOps.ListboxWidth:= c.GetValue('ui_listbox_size_x', UiOps.ListboxWidth);
    UiOps.ListboxCompleteSizeX:= c.GetValue('ui_listbox_complete_size_x', UiOps.ListboxCompleteSizeX);
    UiOps.ListboxCompleteSizeY:= c.GetValue('ui_listbox_complete_size_y', UiOps.ListboxCompleteSizeY);
    UiOps.ListboxFuzzySearch:= c.GetValue('ui_listbox_fuzzy', UiOps.ListboxFuzzySearch);

    UiOps.MaxHistoryEdits:= Max(0, Min(200, c.GetValue('ui_max_history_edits', UiOps.MaxHistoryEdits)));
    UiOps.MaxHistoryMenu:= Max(0, Min(20, c.GetValue('ui_max_history_menu', UiOps.MaxHistoryMenu)));
    UiOps.MaxHistoryFiles:= Max(0, Min(400, c.GetValue('ui_max_history_files', UiOps.MaxHistoryFiles)));

    UiOps.EscapeClose:= c.GetValue('ui_esc_close', UiOps.EscapeClose);
    UiOps.EscapeCloseConsole:= c.GetValue('ui_esc_close_console', UiOps.EscapeCloseConsole);
    UiOps.InitialDir:= c.GetValue('ui_initial_dir', UiOps.InitialDir);

    UiOps.ExportHtmlNumbers:= c.GetValue('export_html_numbers', UiOps.ExportHtmlNumbers);
    UiOps.ExportHtmlFontName:= c.GetValue('export_html_font_name', UiOps.ExportHtmlFontName);
    UiOps.ExportHtmlFontSize:= c.GetValue('export_html_font_size', UiOps.ExportHtmlFontSize);

    UiOps.TreeAutoSync:= c.GetValue('ui_tree_auto_sync', UiOps.TreeAutoSync);
    UiOps.TreeTimeFill:= c.GetValue('ui_tree_time_fill', UiOps.TreeTimeFill);
    UiOps.TreeTimeFocus:= c.GetValue('ui_tree_time_focus', UiOps.TreeTimeFocus);
    UiOps.TreeShowLines:= c.GetValue('ui_tree_show_lines', UiOps.TreeShowLines);
    UiOps.PyChangeSlow:= c.GetValue('py_change_slow', UiOps.PyChangeSlow);

    UiOps.NewdocLexer:= c.GetValue('newdoc_lexer', UiOps.NewdocLexer);
    UiOps.NewdocEnc:= c.GetValue('newdoc_encoding', UiOps.NewdocEnc);
    UiOps.NewdocEnds:= c.GetValue('newdoc_eol', UiOps.NewdocEnds);

    Op.OpIndentAuto:= c.GetValue('indent_auto', Op.OpIndentAuto);
    Op.OpIndentAutoKind:= c.GetValue('indent_kind', Op.OpIndentAutoKind);
    Op.OpIndentSize:= c.GetValue('indent_size', Op.OpIndentSize);
    Op.OpUnIndentKeepsAlign:= c.GetValue('unindent_keeps_align', Op.OpUnIndentKeepsAlign);

    Op.OpMouse2ClickDragSelectsWords:= c.GetValue('mouse_2click_drag_select_words', Op.OpMouse2ClickDragSelectsWords);
    Op.OpMouseDragDrop:= c.GetValue('mouse_drag_drop', Op.OpMouseDragDrop);
    Op.OpMouseNiceScroll:= c.GetValue('mouse_browser_scroll', Op.OpMouseNiceScroll);
    Op.OpMouseRightClickMovesCaret:= c.GetValue('mouse_rt_click_moves_caret', Op.OpMouseRightClickMovesCaret);
    Op.OpMouseEnableColumnSelection:= c.GetValue('mouse_column_selection', Op.OpMouseEnableColumnSelection);
    Op.OpMouseHideCursorOnType:= c.GetValue('mouse_hide_cursor', Op.OpMouseHideCursorOnType);
    Op.OpMouseGutterClickSelectedLine:= c.GetValue('mouse_gutter_click_sel', Op.OpMouseGutterClickSelectedLine);

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


procedure TfmMain.DoOps_LoadOptionsOverride(F: TEditorFrame);
var
  an: TecSyntAnalyzer;
  fn: string;
  Ops: TEditorOps;
begin
  an:= F.Lexer;
  if an=nil then exit;
  if an.LexerName='' then exit;

  fn:= GetLexerOverrideFN(an.LexerName);
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
  fn:= GetAppPath(cFileOptUser);
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
  DoOps_LoadOptions(GetAppPath(cFileOptUser), EditorOps);
  DoApplyAllOps;
  MsgStatus(msgStatusReadOps);
end;


