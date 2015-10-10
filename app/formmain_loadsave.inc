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
  i: integer;
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
      SSaveStringsToFile(c, '/list_find', fmFind.edFind.Items, UiOps.MaxHistoryMenu);
      SSaveStringsToFile(c, '/list_replace', fmFind.edRep.Items, UiOps.MaxHistoryMenu);
    end;

    SSaveStringsToFile(c, '/list_console', fmConsole.ed.Items, UiOps.MaxHistoryMenu);

    ////finder
    c.SetValue('/finder/regex', FFinder.OptRegex);
    c.SetValue('/finder/case', FFinder.OptCase);
    c.SetValue('/finder/words', FFinder.OptWords);
    c.SetValue('/finder/back', FFinder.OptBack);
    c.SetValue('/finder/confirm', FFinder.OptConfirmReplace);

    ////state
    c.SetValue('groups', Ord(Groups.Mode));
    c.SetValue('theme', FThemeName);
  finally
    c.Free;
  end;

  if UiOps.ShowLastFiles then
    DoOps_SaveSession(GetAppPath(cFileHistorySession));
end;


procedure TfmMain.DoOps_SaveSession(const fn_session: string);
var
  c: TJSONConfig;
  F: TEditorFrame;
  nGroup, nTab, i: integer;
  path: string;
begin
  DeleteFile(fn_session);

  c:= TJSONConfig.Create(nil);
  try
    c.Filename:= fn_session;
    c.Formatted:= true;

    Groups.PagesAndTabIndexOfControl(CurrentFrame, nGroup, nTab);
    c.SetValue('/n_group', nGroup);
    c.SetValue('/n_tab', nTab);

    for i:= 0 to FrameCount-1 do
    begin
      F:= Frames[i];
      if F.Filename='' then Continue;
      path:= '/'+Format('%3.3d', [i])+'/';
      Groups.PagesAndTabIndexOfControl(F, nGroup, nTab);

      c.SetValue(path+'file', F.Filename);
      c.SetValue(path+'group', nGroup);
    end;
  finally
    c.Free;
  end;
end;

procedure TfmMain.DoOps_LoadSession(const fn_session: string);
var
  c: TJSONConfig;
  list: TStringlist;
  nGroup, nTab, i: integer;
  D: TATTabData;
  fn: string;
begin
  c:= TJSONConfig.Create(nil);
  list:= TStringlist.Create;
  try
    c.Filename:= fn_session;

    c.EnumSubKeys('/', list);
    list.Sort;

    for i:= 0 to list.Count-1 do
    begin
      fn:= c.GetValue('/'+list[i]+'/file', '');
      nGroup:= c.GetValue('/'+list[i]+'/group', Low(TATGroupsNums));

      if nGroup in [Low(TATGroupsNums)..High(TATGroupsNums)] then
      begin
        Groups.PagesCurrent:= Groups.Pages[nGroup];
      end;

      if FileExistsUTF8(fn) then
        DoFileOpen(fn);
    end;

    nGroup:= c.GetValue('/n_group', Low(TATGroupsNums));
    nTab:= c.GetValue('/n_tab', 0);

    if nGroup in [Low(TATGroupsNums)..High(TATGroupsNums)] then
    begin
      Groups.PagesCurrent:= Groups.Pages[nGroup];
      D:= Groups.PagesCurrent.Tabs.GetTabData(nTab);
      if Assigned(D) then
        SetFrame(D.TabObject as TEditorFrame);
    end;
  finally
    list.Free;
    c.Free;
  end;
end;

procedure TfmMain.DoOps_LoadHistory;
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
    nSizeX:= c.GetValue('/dialog/sizex', 800);
    nSizeY:= c.GetValue('/dialog/sizey', 550);
    SetBounds(nPosX, nPosY, nSizeX, nSizeY);

    if c.GetValue('/dialog/max', false) then
      WindowState:= wsMaximized;

    ////recents
    SLoadStringsFromFile(c, '/recent_files', FListRecents, UiOps.MaxHistoryMenu);
    SLoadStringsFromFile(c, '/list_console', fmConsole.ed.Items, UiOps.MaxHistoryMenu);
    UpdateMenuRecent(nil);

    ////finder
    FFinder.OptRegex:= c.GetValue('/finder/regex', false);
    FFinder.OptCase:= c.GetValue('/finder/case', false);
    FFinder.OptWords:= c.GetValue('/finder/words', false);
    FFinder.OptBack:= c.GetValue('/finder/back', false);
    FFinder.OptConfirmReplace:= c.GetValue('/finder/confirm', false);

    ////state
    Groups.Mode:= TATGroupsMode(c.GetValue('groups', Ord(gmOne)));
    ShowToolbar:= c.GetValue('show_toolbar', true);
    ShowStatus:= c.GetValue('show_status', true);
    ShowSidePanel:= c.GetValue('show_sidepanel', true);
    ShowBottom:= c.GetValue('show_bottom', false);
    PanelLeft.Width:= c.GetValue('size_side', 200);
    PanelBottom.Height:= c.GetValue('size_bottom', 200);

    ////theme
    FThemeName:= c.GetValue('theme', '');
    if FThemeName<>'' then
    begin
      DoLoadTheme(GetAppPath(cDirDataThemes)+DirectorySeparator+FThemeName+'.json', Theme);
      DoApplyTheme;
    end;
  finally
    c.Free;
  end;

  if UiOps.ShowLastFiles then
    DoOps_LoadSession(GetAppPath(cFileHistorySession));
end;

procedure TfmMain.DoOps_LoadKeymap;
var
  c: TJSONConfig;
  sl, sl2: tstringlist;
  ncmd, nitem, i, j: integer;
begin
  c:= TJSONConfig.Create(nil);
  sl:= tstringlist.create;
  sl2:= tstringlist.create;
  try
    c.Filename:= GetAppPath(cFileOptKeymap);
    c.EnumSubKeys('/', sl);
    for i:= 0 to sl.count-1 do
    begin
      ncmd:= StrToIntDef(sl[i], -1);
      if ncmd<0 then Continue;
      nitem:= Keymap.IndexOf(ncmd);
      if nitem<0 then Continue;

      Fillchar(Keymap[nitem].Keys1, sizeof(TATKeyArray), 0);
      Fillchar(Keymap[nitem].Keys2, sizeof(TATKeyArray), 0);

      c.GetValue(sl[i]+'/k1', sl2, '');
      for j:= 0 to sl2.count-1 do
        Keymap[nitem].Keys1[j]:= StrToIntDef(sl2[j], 0);

      c.GetValue(sl[i]+'/k2', sl2, '');
      for j:= 0 to sl2.count-1 do
        Keymap[nitem].Keys2[j]:= StrToIntDef(sl2[j], 0);
    end;
  finally
    sl2.Free;
    sl.Free;
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

    Op.OpFontName:= c.GetValue('font_name', Op.OpFontName);
    Op.OpFontSize:= c.GetValue('font_size', Op.OpFontSize);
    UiOps.VarFontName:= c.GetValue('ui_font_name', UiOps.VarFontName);
    UiOps.VarFontSize:= c.GetValue('ui_font_size', UiOps.VarFontSize);

    UiOps.StatusNoSel:= c.GetValue('ui_stat_no_sel', UiOps.StatusNoSel);
    UiOps.StatusSmallSel:= c.GetValue('ui_stat_small_sel', UiOps.StatusSmallSel);
    UiOps.StatusStreamSel:= c.GetValue('ui_stat_str_sel', UiOps.StatusStreamSel);
    UiOps.StatusColSel:= c.GetValue('ui_stat_col_sel', UiOps.StatusColSel);
    UiOps.StatusCarets:= c.GetValue('ui_stat_carets', UiOps.StatusCarets);
    UiOps.StatusSizeX:= c.GetValue('ui_stat_size_x', UiOps.StatusSizeX);
    UiOps.StatusSizeY:= c.GetValue('ui_stat_size_y', UiOps.StatusSizeY);
    UiOps.StatusCenter:= c.GetValue('ui_stat_center', UiOps.StatusCenter);

    UiOps.ShowTitlePath:= c.GetValue('ui_title_path', UiOps.ShowTitlePath);
    UiOps.ShowLastFiles:= c.GetValue('ui_reopen_session', UiOps.ShowLastFiles);

    Op.OpSpaceX:= c.GetValue('spacing_x', Op.OpSpaceX);
    Op.OpSpaceY:= c.GetValue('spacing_y', Op.OpSpaceY);

    Op.OpTabSize:= c.GetValue('tab_size', Op.OpTabSize);
    Op.OpTabSpaces:= c.GetValue('tab_spaces', Op.OpTabSpaces);

    Op.OpOvrSel:= c.GetValue('ovr_sel', Op.OpOvrSel);
    Op.OpOvrOnPaste:= c.GetValue('ovr_on_paste', Op.OpOvrOnPaste);
    Op.OpUnderlineColor:= c.GetValue('under_color', Op.OpUnderlineColor);
    Op.OpUnderlineColorSize:= c.GetValue('under_color_size', Op.OpUnderlineColorSize);

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

    UiOps.LexlibFilename:= c.GetValue('lexlib', UiOps.LexlibFilename);
    UiOps.PyLibrary:= c.GetValue('pylib', UiOps.PyLibrary);

    UiOps.FindSuggestSel:= c.GetValue('find_suggest_sel', UiOps.FindSuggestSel);
    UiOps.FindSuggestWord:= c.GetValue('find_suggest_cur_word', UiOps.FindSuggestWord);
    UiOps.FindSelCaseSens:= c.GetValue('find_sel_case_sens', UiOps.FindSelCaseSens);

    UiOps.TabSizeX:= c.GetValue('ui_tab_size_x', UiOps.TabSizeX);
    UiOps.TabSizeY:= c.GetValue('ui_tab_size_y', UiOps.TabSizeY);
    UiOps.TabIndentX:= c.GetValue('ui_tab_indent_x', UiOps.TabIndentX);
    UiOps.TabIndentY:= c.GetValue('ui_tab_indent_y', UiOps.TabIndentY);
    UiOps.TabAngle:= Max(0, Min(10, c.GetValue('ui_tab_angle', UiOps.TabAngle)));
    UiOps.TabBottom:= c.GetValue('ui_tab_bottom', UiOps.TabBottom);
    UiOps.TabColorFull:= c.GetValue('ui_tab_fullcolor', UiOps.TabColorFull);
    UiOps.TabShowX:= c.GetValue('ui_tab_show_x', UiOps.TabShowX);
    UiOps.TabShowPlus:= c.GetValue('ui_tab_show_plus', UiOps.TabShowPlus);
    UiOps.TabDblClickClose:= c.GetValue('ui_tab_2click_closes', UiOps.TabDblClickClose);

    UiOps.ListboxWidth:= c.GetValue('ui_listbox_size_x', UiOps.ListboxWidth);
    UiOps.ListboxCompleteSizeX:= c.GetValue('ui_listbox_complete_size_x', UiOps.ListboxCompleteSizeX);
    UiOps.ListboxCompleteSizeY:= c.GetValue('ui_listbox_complete_size_y', UiOps.ListboxCompleteSizeY);

    UiOps.MaxHistoryMenu:= Max(0, Min(20, c.GetValue('ui_max_history_menu', UiOps.MaxHistoryMenu)));
    UiOps.MaxHistoryFiles:= Max(0, Min(400, c.GetValue('ui_max_history_files', UiOps.MaxHistoryFiles)));
    UiOps.EscapeClose:= c.GetValue('ui_esc_close', UiOps.EscapeClose);
    UiOps.InitialDir:= c.GetValue('ui_initial_dir', UiOps.InitialDir);

    UiOps.ExportHtmlNumbers:= c.GetValue('export_html_numbers', UiOps.ExportHtmlNumbers);
    UiOps.ExportHtmlFontName:= c.GetValue('export_html_font_name', UiOps.ExportHtmlFontName);
    UiOps.ExportHtmlFontSize:= c.GetValue('export_html_font_size', UiOps.ExportHtmlFontSize);

    UiOps.TreeAutoSync:= c.GetValue('ui_tree_auto_sync', UiOps.TreeAutoSync);
    UiOps.TreeTimeFill:= c.GetValue('ui_tree_time_fill', UiOps.TreeTimeFill);
    UiOps.TreeTimeFocus:= c.GetValue('ui_tree_time_focus', UiOps.TreeTimeFocus);

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
    DoApplyFrameOps(F, Ops);
  end;
end;


procedure TfmMain.DoOps_DlgFontText;
begin
  DoOps_DlgFont(
    EditorOps.OpFontName,
    EditorOps.OpFontSize,
    'font_name',
    'font_size');
  DoApplyFontFixed;
end;

procedure TfmMain.DoOps_DlgFontUi;
begin
  DoOps_DlgFont(
    UiOps.VarFontName,
    UiOps.VarFontSize,
    'ui_font_name',
    'ui_font_size');
  DoApplyFontVar;
end;

procedure TfmMain.DoOps_DlgFont(
  var OpName: string; var OpSize: integer;
  const ConfStrName, ConfStrSize: string);
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
    cfg.SetValue(ConfStrName, OpName);
    cfg.SetValue(ConfStrSize, OpSize);
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


