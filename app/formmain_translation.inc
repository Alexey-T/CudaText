{$ifdef nn}begin end;{$endif}

procedure DoTranslateToolbarButtonHint(AToolbar: TATFlatToolbar; AIndex: integer; const AText: string);
var
  Btn: TATButton;
begin
  Btn:= AToolbar.Buttons[AIndex];
  Btn.Hint:= AText;
end;

procedure DoLocMenuitem(ini: TIniFile; c: TMenuItem; const ASection, AKey: string);
begin
  if Assigned(c) then
    c.Caption:= ini.ReadString(ASection, AKey, c.Caption);
end;

procedure DoLocMenuitem(mi: TMenuItem; const AStr: string);
begin
  if Assigned(mi) then
    mi.Caption:= AStr;
end;

procedure DoLocMenuitem(mi, mi_from: TMenuItem);
begin
  if Assigned(mi) then
    if Assigned(mi_from) then
      mi.Caption:= mi_from.Caption;
end;

procedure DoLocControlHint(ini: TIniFile; c: TControl; const ASection, AKey: string);
begin
  if Assigned(c) then
    c.Hint:= ini.ReadString(ASection, AKey, c.Hint);
end;

procedure DoLocControlHint(c: TControl; AStr: string);
begin
  SReplaceAll(AStr, '&', '');
  if Assigned(c) then
    c.Hint:= AStr;
end;

procedure DoLocString(var AStr: string; ini: TIniFile; const ASection, AKey: string);
begin
  AStr:= ini.ReadString(ASection, AKey, AStr);
  SReplaceAll(AStr, '\n', #10);
end;

procedure DoLocToolbarButtonHint(AToolbar: TATFlatToolbar; const ABtnCaption, ABtnNewHint: string);
var
  Btn: TATButton;
  i: integer;
begin
  for i:= 0 to AToolbar.ButtonCount-1 do
  begin
    Btn:= AToolbar.Buttons[i];
    if Btn.Caption=ABtnCaption then
    begin
      Btn.Hint:= StringReplace(ABtnNewHint, '&', '', [rfReplaceAll]);
      exit;
    end;
  end;
end;

procedure TfmMain.DoLocalize;
var
  fn: string;
  ini: TIniFile;
  s: string;
begin
  fn:= GetAppLangFilename;
  if not FileExistsUTF8(fn) then exit;

  ini:= TIniFile.Create(fn);
  try
    DoLocMenuitem(ini, mnuFile, 'm_f', '_');
    DoLocMenuitem(ini, mnuFileNew, 'm_f', 'new');
    DoLocMenuitem(ini, mnuFileNewMenu, 'm_f', 'newm');
    DoLocMenuitem(ini, mnuFileOpen, 'm_f', 'op');
    DoLocMenuitem(ini, mnuFileOpenSub, 'm_f', 'rec');
    DoLocMenuitem(ini, mnuFileOpenDir, 'm_f', 'op_dir');
    DoLocMenuitem(ini, mnuFileReopen, 'm_f', 'reop');
    DoLocMenuitem(ini, mnuFileSave, 'm_f', 's');
    DoLocMenuitem(ini, mnuFileSaveAs, 'm_f', 's_as');
    DoLocMenuitem(ini, mnuFileSaveAll, 'm_f', 's_all');
    DoLocMenuitem(ini, mnuFileClose, 'm_f', 'cl');
    DoLocMenuitem(ini, mnuFileCloseAll, 'm_f', 'cl_all');
    DoLocMenuitem(ini, mnuFileCloseOther, 'm_f', 'cl_oth');
    DoLocMenuitem(ini, mnuFileCloseDel, 'm_f', 'cl_del');
    DoLocMenuitem(ini, mnuFileEnc, 'm_f', 'e_');
    DoLocMenuitem(ini, mnuFileEnds, 'm_f', 'z_');
    DoLocMenuitem(ini, mnuFileHtml, 'm_f', 'html');
    DoLocMenuitem(ini, mnuFileExit, 'm_f', 'q');

    msgEncReloadAs:= ini.ReadString('m_f', 'e_r', msgEncReloadAs);
    msgEncConvertTo:= ini.ReadString('m_f', 'e_t', msgEncConvertTo);
    msgEncEuropean:= ini.ReadString('m_f', 'e_eu', msgEncEuropean);
    msgEncAsian:= ini.ReadString('m_f', 'e_as', msgEncAsian);
    msgEncMisc:= ini.ReadString('m_f', 'e_mi', msgEncMisc);
    UpdateMenuEnc(mnuFileEnc);
    UpdateMenuEnc(PopupEnc.Items);

    msgEndWin:= ini.ReadString('m_f', 'z_w', msgEndWin);
    msgEndUnix:= ini.ReadString('m_f', 'z_u', msgEndUnix);
    msgEndMac:= ini.ReadString('m_f', 'z_m', msgEndMac);

    DoLocMenuItem(mnuFileEndWin, msgEndWin);
    DoLocMenuItem(mnuFileEndUn, msgEndUnix);
    DoLocMenuItem(mnuFileEndMac, msgEndMac);

    DoLocMenuItem(mnuEndsWin, msgEndWin);
    DoLocMenuItem(mnuEndsUnix, msgEndUnix);
    DoLocMenuItem(mnuEndsMac, msgEndMac);

    msgEditUndo:= ini.ReadString('m_e', 'undo', msgEditUndo);
    msgEditRedo:= ini.ReadString('m_e', 'redo', msgEditRedo);
    msgEditCut:= ini.ReadString('m_e', 'cut', msgEditCut);
    msgEditCopy:= ini.ReadString('m_e', 'cp', msgEditCopy);
    msgEditPaste:= ini.ReadString('m_e', 'pst', msgEditPaste);
    msgEditDelete:= ini.ReadString('m_e', 'del', msgEditDelete);
    msgEditSelectAll:= ini.ReadString('m_se', 'al', msgEditSelectAll);

    DoLocMenuitem(mnuEditUndo, msgEditUndo);
    DoLocMenuitem(mnuEditRedo, msgEditRedo);
    DoLocMenuitem(mnuEditCut, msgEditCut);
    DoLocMenuitem(mnuEditCopy, msgEditCopy);
    DoLocMenuitem(mnuEditPaste, msgEditPaste);
    DoLocMenuitem(mnuEditDel, msgEditDelete);

    DoLocMenuitem(ini, mnuEdit, 'm_e', '_');
    DoLocMenuitem(ini, mnuEditUndo, 'm_e', 'undo');
    DoLocMenuitem(ini, mnuEditRedo, 'm_e', 'redo');
    DoLocMenuitem(ini, mnuEditCut, 'm_e', 'cut');
    DoLocMenuitem(ini, mnuEditCopy, 'm_e', 'cp');
    DoLocMenuitem(ini, mnuEditPaste, 'm_e', 'pst');
    DoLocMenuitem(ini, mnuEditDel, 'm_e', 'del');

    DoLocMenuitem(ini, mnuEditCopySub, 'm_e', 'cp_');
    DoLocMenuitem(ini, mnuEditCopyLine, 'm_e', 'cp_ln');
    DoLocMenuitem(ini, mnuEditCopyAppend, 'm_e', 'cp_add');
    DoLocMenuitem(ini, mnuEditCopyFFull, 'm_e', 'cp_full');
    DoLocMenuitem(ini, mnuEditCopyFName, 'm_e', 'cp_name');
    DoLocMenuitem(ini, mnuEditCopyFDir, 'm_e', 'cp_dir');

    DoLocMenuitem(ini, mnuEditLineOp, 'm_e', 'ln_');
    DoLocMenuitem(ini, mnuEditLineDel, 'm_e', 'ln_del');
    DoLocMenuitem(ini, mnuEditLineDup, 'm_e', 'ln_dup');
    DoLocMenuitem(ini, mnuEditLineMoveUp, 'm_e', 'ln_up');
    DoLocMenuitem(ini, mnuEditLineMoveDown, 'm_e', 'ln_dn');

    DoLocMenuitem(ini, mnuEditIndentSub, 'm_e', 'bl_');
    DoLocMenuitem(ini, mnuEditIndent, 'm_e', 'bl_ind');
    DoLocMenuitem(ini, mnuEditUnindent, 'm_e', 'bl_uind');
    DoLocMenuitem(ini, mnuEditTrim, 'm_e', 'tr_a');
    DoLocMenuitem(ini, mnuEditTrimL, 'm_e', 'tr_l');
    DoLocMenuitem(ini, mnuEditTrimR, 'm_e', 'tr_r');
    DoLocMenuitem(ini, mnuEditTabToSp, 'm_e', 'con_t');
    DoLocMenuitem(ini, mnuEditSpToTab, 'm_e', 'con_s');

    DoLocMenuitem(ini, mnuCaseSub, 'm_e', 'cas_');
    DoLocMenuitem(ini, mnuCaseUp, 'm_e', 'cas_u');
    DoLocMenuitem(ini, mnuCaseLow, 'm_e', 'cas_l');
    DoLocMenuitem(ini, mnuCaseTitle, 'm_e', 'cas_t');
    DoLocMenuitem(ini, mnuCaseInvert, 'm_e', 'cas_i');
    DoLocMenuitem(ini, mnuCaseSent, 'm_e', 'cas_s');

    DoLocMenuitem(ini, mnuEditCharmap, 'm_e', 'map');

    DoLocMenuitem(ini, mnuSel, 'm_se', '_');
    DoLocMenuitem(mnuSelAll, msgEditSelectAll);
    DoLocMenuitem(ini, mnuSelSplit, 'm_se', 'sp');
    DoLocMenuitem(ini, mnuSelExtLine, 'm_se', 'ex');
    DoLocMenuitem(ini, mnuSelExtWord, 'm_se', 'ex_wo');
    DoLocMenuitem(ini, mnuSelInvert, 'm_se', 'in');

    DoLocMenuitem(ini, mnuCaretsCancel, 'm_se', 'can');
    DoLocMenuitem(ini, mnuCaretsExtSub, 'm_se', 'm_');
    DoLocMenuitem(ini, mnuCaretsUp1Line, 'm_se', 'u_l');
    DoLocMenuitem(ini, mnuCaretsUp1Page, 'm_se', 'u_p');
    DoLocMenuitem(ini, mnuCaretsUpBegin, 'm_se', 'u_z');
    DoLocMenuitem(ini, mnuCaretsDown1Line, 'm_se', 'd_l');
    DoLocMenuitem(ini, mnuCaretsDown1Page, 'm_se', 'd_p');
    DoLocMenuitem(ini, mnuCaretsDownEnd, 'm_se', 'd_z');

    DoLocMenuitem(ini, mnuSr, 'm_sr', '_');
    DoLocMenuitem(ini, mnuFindDlg, 'm_sr', 'f');
    DoLocMenuitem(ini, mnuFindRepDialog, 'm_sr', 'r');

    DoLocMenuitem(ini, mnuFindNext, 'm_sr', 'f_n');
    DoLocMenuitem(ini, mnuFindPrev, 'm_sr', 'f_p');
    DoLocMenuitem(ini, mnuFindWordNext, 'm_sr', 'w_n');
    DoLocMenuitem(ini, mnuFindWordPrev, 'm_sr', 'w_p');

    DoLocMenuitem(mnuFind2Next, mnuFindNext);
    DoLocMenuitem(mnuFind2Prev, mnuFindPrev);
    DoLocMenuitem(mnuFind2WordNext, mnuFindWordNext);
    DoLocMenuitem(mnuFind2WordPrev, mnuFindWordPrev);

    DoLocMenuitem(ini, mnuGotoLine, 'm_sr', 'g_l');
    DoLocMenuitem(ini, mnuGotoBm, 'm_sr', 'g_b');

    DoLocMenuitem(ini, mnuBmSub, 'm_sr', 'b_');
    DoLocMenuitem(ini, mnuBmNext, 'm_sr', 'b_n');
    DoLocMenuitem(ini, mnuBmPrev, 'm_sr', 'b_p');
    DoLocMenuitem(ini, mnuBmToggle, 'm_sr', 'b_to');
    DoLocMenuitem(ini, mnuBmInvert, 'm_sr', 'b_in');
    DoLocMenuitem(ini, mnuBmPlaceCarets, 'm_sr', 'b_ca');
    DoLocMenuitem(ini, mnuBmPlaceOnCarets, 'm_sr', 'b_ca2');
    DoLocMenuitem(ini, mnuBmClear, 'm_sr', 'b_cl');
    DoLocMenuitem(ini, mnuBmCopyLines, 'm_sr', 'b_cp');
    DoLocMenuitem(ini, mnuBmDeleteLines, 'm_sr', 'b_de');

    DoLocMenuitem(ini, mnuView, 'm_v', '_');
    DoLocMenuitem(ini, mnuGroups, 'm_v', 'g_');
    DoLocMenuitem(ini, mnuGr1, 'm_v', 'g1');
    DoLocMenuitem(ini, mnuGr2H, 'm_v', 'g2h');
    DoLocMenuitem(ini, mnuGr2V, 'm_v', 'g2v');
    DoLocMenuitem(ini, mnuGr3H, 'm_v', 'g3h');
    DoLocMenuitem(ini, mnuGr3V, 'm_v', 'g3v');
    DoLocMenuitem(ini, mnuGr1p2H, 'm_v', 'g12h');
    DoLocMenuitem(ini, mnuGr1p2V, 'm_v', 'g12v');
    DoLocMenuitem(ini, mnuGr4H, 'm_v', 'g4h');
    DoLocMenuitem(ini, mnuGr4V, 'm_v', 'g4v');
    DoLocMenuitem(ini, mnuGr4G, 'm_v', 'g4g');
    DoLocMenuitem(ini, mnuGr6, 'm_v', 'g6');

    DoLocMenuitem(ini, mnuViewSplitSub, 'm_v', 's_');
    DoLocMenuitem(ini, mnuViewSplitDo, 'm_v', 's_s');
    DoLocMenuitem(ini, mnuViewSplitHorz, 'm_v', 's_h');

    DoLocMenuitem(ini, mnuViewWrap, 'm_v', 't_ww');
    DoLocMenuitem(ini, mnuViewNums, 'm_v', 't_nu');
    DoLocMenuitem(ini, mnuViewFold, 'm_v', 't_fo');
    DoLocMenuitem(ini, mnuViewRuler, 'm_v', 't_ru');
    DoLocMenuitem(ini, mnuViewMinimap, 'm_v', 't_mi');
    DoLocMenuitem(ini, mnuViewMicromap, 'm_v', 't_mo');

    DoLocMenuitem(ini, mnuViewUnpri, 'm_v', 'u_');
    DoLocMenuitem(ini, mnuViewUnpriShow, 'm_v', 'u_sh');
    DoLocMenuitem(ini, mnuViewUnpriSpaces, 'm_v', 'u_sp');
    DoLocMenuitem(ini, mnuViewUnpriSpacesTail, 'm_v', 'u_spt');
    DoLocMenuitem(ini, mnuViewUnpriEnds, 'm_v', 'u_e');
    DoLocMenuitem(ini, mnuViewUnpriEndsDet, 'm_v', 'u_ed');

    DoLocMenuitem(ini, mnuViewToolbar, 'm_v', 't_tb');
    DoLocMenuitem(ini, mnuViewStatus, 'm_v', 't_sb');
    DoLocMenuitem(ini, mnuViewSide, 'm_v', 't_si');
    DoLocMenuitem(ini, mnuViewBottom, 'm_v', 't_bo');
    DoLocMenuitem(ini, mnuViewFullscr, 'm_v', 't_fs');
    DoLocMenuitem(ini, mnuViewDistFree, 'm_v', 't_dis');
    DoLocMenuitem(ini, mnuViewOnTop, 'm_v', 't_ao');

    DoLocMenuitem(ini, mnuPlugins, 'm_o', 'pl_');
    DoLocMenuitem(ini, mnuOp, 'm_o', '_');

    DoLocMenuitem(ini, mnuOpDefault, 'm_o', 'df');
    DoLocMenuitem(ini, mnuOpUser, 'm_o', 'us');
    DoLocMenuitem(ini, mnuOpMore, 'm_o', 'mr');
    DoLocMenuitem(ini, mnuOpLexer, 'm_o', 'lx');
    DoLocMenuitem(ini, mnuOpFileTypes, 'm_o', 'fi');
    DoLocMenuitem(ini, mnuOpThemeUi, 'm_o', 'cou');
    DoLocMenuitem(ini, mnuOpThemeSyntax, 'm_o', 'cos');
    DoLocMenuitem(ini, mnuOpKeys, 'm_o', 'ke');

    DoLocMenuitem(ini, mnuFontSub, 'm_o', 'f_');
    DoLocMenuitem(ini, mnuFontText, 'm_o', 'f_t');
    DoLocMenuitem(ini, mnuFontUi, 'm_o', 'f_u');
    DoLocMenuitem(ini, mnuFontOutput, 'm_o', 'f_o');

    DoLocMenuitem(ini, mnuLexers, 'm_o', 'l_');
    DoLocMenuitem(ini, mnuOpLexSub, 'm_o', 'l_');
    DoLocMenuitem(ini, mnuOpLexProp, 'm_o', 'l_pr');
    DoLocMenuitem(ini, mnuOpLexLib, 'm_o', 'l_li');
    DoLocMenuitem(ini, mnuOpLexMap, 'm_o', 'l_map');
    DoLocMenuitem(ini, mnuOpPlugins, 'm_o', 'pl');

    DoLocMenuitem(ini, mnuThemes, 'm_o', 'co_');
    DoLocMenuitem(ini, mnuThemesUI, 'm_o', 'cou_');
    DoLocMenuitem(ini, mnuThemesSyntax, 'm_o', 'cos_');
    DoLocMenuitem(ini, mnuLang, 'm_o', 'tr_');

    DoLocMenuitem(ini, mnuHelp, 'm_h', '_');
    DoLocMenuitem(ini, mnuHelpCmd, 'm_h', 'co');
    DoLocMenuitem(ini, mnuHelpForum, 'm_h', 'fo');
    DoLocMenuitem(ini, mnuHelpWiki, 'm_h', 'wi');
    DoLocMenuitem(ini, mnuHelpMouse, 'm_h', 'mo');
    DoLocMenuitem(ini, mnuHelpLexers, 'm_h', 'lx');
    DoLocMenuitem(ini, mnuHelpChangelog, 'm_h', 'ch');
    DoLocMenuitem(ini, mnuHelpIssues, 'm_h', 'iss');
    DoLocMenuitem(ini, mnuHelpAbout, 'm_h', 'ab');
    DoLocMenuitem(ini, mnuHelpCheckUpd, 'm_h', 'chk');

    msgPanelTree:= ini.ReadString('ui', 't_tre', msgPanelTree);
    msgPanelProject:= ini.ReadString('ui', 't_proj', msgPanelProject);
    msgPanelTabs:= ini.ReadString('ui', 't_tabs', msgPanelTabs);
    //
    DoTranslateToolbarButtonHint(ToolbarSideTop, 0, msgPanelTree);
    //
    DoTranslateToolbarButtonHint(ToolbarSideLow, 0, ini.ReadString('ui', 't_con', 'Console'));
    DoTranslateToolbarButtonHint(ToolbarSideLow, 1, ini.ReadString('ui', 't_out', 'Output'));
    DoTranslateToolbarButtonHint(ToolbarSideLow, 2, ini.ReadString('ui', 't_val', 'Validate'));
    //
    DoTranslateToolbarButtonHint(ToolbarSideMid, 0, ini.ReadString('ui', 't_find', 'Search'));
    DoTranslateToolbarButtonHint(ToolbarSideMid, 1, ini.ReadString('ui', 't_goto', 'Go to'));

    msgUntitledTab:= ini.ReadString('ui', 'unt', msgUntitledTab);
    msgAllFiles:= ini.ReadString('ui', 'allf', msgAllFiles);
    msgThemeDefault:= ini.ReadString('ui', 'deft', msgThemeDefault);
    msgThemeName:= ini.ReadString('ui', 'th_nm', msgThemeName);
    cTextEditorLocked:= ini.ReadString('ui', 'ed_w', cTextEditorLocked);
    cHintScrollPrefix:= ini.ReadString('ui', 'line', cHintScrollPrefix);
    msgNoLexer:= ini.ReadString('ui', 'nlex', msgNoLexer);
    UpdateMenuLexers;
    msgFileClearList:= ini.ReadString('ui', 'clr', msgFileClearList);
    UpdateMenuRecent(nil);

    DoLocString(msgFinderHintRegex,     ini, 'ui', 'fnd_re');
    DoLocString(msgFinderHintCase,      ini, 'ui', 'fnd_case');
    DoLocString(msgFinderHintWords,     ini, 'ui', 'fnd_word');
    DoLocString(msgFinderHintBack,      ini, 'ui', 'fnd_back');
    DoLocString(msgFinderHintWrapped,   ini, 'ui', 'fnd_wrap');
    DoLocString(msgFinderHintInSel,     ini, 'ui', 'fnd_sel');
    DoLocString(msgFinderHintFromCaret, ini, 'ui', 'fnd_crt');

    DoLocString(msgButtonOk,     ini, 'ui', 'b_ok');
    DoLocString(msgButtonCancel, ini, 'ui', 'b_can');
    DoLocString(msgButtonApply,  ini, 'ui', 'b_ap');
    DoLocString(msgButtonClose,  ini, 'ui', 'b_cl');
    DoLocString(msgButtonYes,    ini, 'ui', 'b_y');
    DoLocString(msgButtonYesAll, ini, 'ui', 'b_ya');
    DoLocString(msgButtonNo,     ini, 'ui', 'b_n');
    DoLocString(msgButtonNoAll,  ini, 'ui', 'b_na');
    DoLocString(msgButtonAbort,  ini, 'ui', 'b_ab');
    DoLocString(msgButtonRetry,  ini, 'ui', 'b_re');
    DoLocString(msgButtonIgnore, ini, 'ui', 'b_ig');

    DoLocToolbarButtonHint(ToolbarMain, 'f_new', ini.ReadString('ui', 'tb_new', ''));
    DoLocToolbarButtonHint(ToolbarMain, 'f_open', ini.ReadString('ui', 'tb_op', ''));
    DoLocToolbarButtonHint(ToolbarMain, 'f_save', ini.ReadString('ui', 'tb_sav', ''));
    DoLocToolbarButtonHint(ToolbarMain, 'find', ini.ReadString('ui', 'tb_find', ''));
    DoLocToolbarButtonHint(ToolbarMain, 'goto', ini.ReadString('ui', 'tb_go', ''));
    DoLocToolbarButtonHint(ToolbarMain, 'e_cut', msgEditCut);
    DoLocToolbarButtonHint(ToolbarMain, 'e_copy', msgEditCopy);
    DoLocToolbarButtonHint(ToolbarMain, 'e_paste', msgEditPaste);
    //DoLocToolbarButtonHint(ToolbarMain, 'e_del', msgEditDelete);
    //DoLocToolbarButtonHint(ToolbarMain, 'e_sel', msgEditSelectAll);
    DoLocToolbarButtonHint(ToolbarMain, 'e_undo', msgEditUndo);
    DoLocToolbarButtonHint(ToolbarMain, 'e_redo', msgEditRedo);
    if Assigned(mnuViewUnpri) then
      DoLocToolbarButtonHint(ToolbarMain, 'unpri', mnuViewUnpri.Caption);
    if Assigned(mnuViewSide) then
      DoLocToolbarButtonHint(ToolbarMain, 'p_side', mnuViewSide.Caption);
    if Assigned(mnuViewBottom) then
      DoLocToolbarButtonHint(ToolbarMain, 'p_btm', mnuViewBottom.Caption);
    if Assigned(mnuViewMinimap) then
      DoLocToolbarButtonHint(ToolbarMain, 'map', mnuViewMinimap.Caption);

    DoLocMenuitem(mnuTextUndo, msgEditUndo);
    DoLocMenuitem(mnuTextRedo, msgEditRedo);
    DoLocMenuitem(mnuTextCut, msgEditCut);
    DoLocMenuitem(mnuTextCopy, msgEditCopy);
    DoLocMenuitem(mnuTextPaste, msgEditPaste);
    DoLocMenuitem(mnuTextDelete, msgEditDelete);
    DoLocMenuitem(mnuTextSel, msgEditSelectAll);
    DoLocMenuitem(ini, mnuTextGotoDef, 'ct', 'godef');
    DoLocMenuitem(ini, mnuTextOpenUrl, 'ct', 'url');

    DoLocMenuitem(ini, fmConsole.mnuTextClear, 'ct', 'clr');
    DoLocMenuitem(ini, fmConsole.mnuTextNav, 'ct', 'nav');
    DoLocMenuitem(ini, fmConsole.mnuTextWrap, 'ct', 'wr');

    with fmConsole.ed do
    begin
      MenuitemTextCut.Caption:= msgEditCut;
      MenuitemTextCopy.Caption:= msgEditCopy;
      MenuitemTextPaste.Caption:= msgEditPaste;
      MenuitemTextDelete.Caption:= msgEditDelete;
      MenuitemTextSelAll.Caption:= msgEditSelectAll;
      MenuitemTextUndo.Caption:= msgEditUndo;
      MenuitemTextRedo.Caption:= msgEditRedo;
    end;
    with fmConsole.memo do
    begin
      MenuitemTextCut.Caption:= msgEditCut;
      MenuitemTextCopy.Caption:= msgEditCopy;
      MenuitemTextPaste.Caption:= msgEditPaste;
      MenuitemTextDelete.Caption:= msgEditDelete;
      MenuitemTextSelAll.Caption:= msgEditSelectAll;
      MenuitemTextUndo.Caption:= msgEditUndo;
      MenuitemTextRedo.Caption:= msgEditRedo;
    end;

    DoLocMenuitem(ini, mnuTabCloseThis, 'ct', 'c_tab');
    DoLocMenuitem(ini, mnuTabCloseSub, 'ct', 'c_');
    DoLocMenuitem(ini, mnuTabCloseOtherSame, 'ct', 'c_oth_s');
    DoLocMenuitem(ini, mnuTabCloseOtherAll, 'ct', 'c_oth_a');
    DoLocMenuitem(ini, mnuTabCloseAllSame, 'ct', 'c_all_s');
    DoLocMenuitem(ini, mnuTabCloseAllAll, 'ct', 'c_all_a');
    DoLocMenuitem(ini, mnuTabCloseLeft, 'ct', 'c_lt_s');
    DoLocMenuitem(ini, mnuTabCloseRight, 'ct', 'c_rt_s');

    DoLocMenuitem(ini, mnuTabSave, 'ct', 'sv');
    DoLocMenuitem(ini, mnuTabSaveAs, 'ct', 'sv_as');
    DoLocMenuitem(ini, mnuTabColor, 'ct', 'col_t');

    DoLocMenuitem(ini, mnuTabMoveSub, 'ct', 'mov_');
    DoLocMenuitem(ini, mnuTabMoveNext, 'ct', 'mov_n');
    DoLocMenuitem(ini, mnuTabMovePrev, 'ct', 'mov_p');

    DoLocMenuitem(ini, mnuTreeFoldAll, 'ct', 'fld_a');
    DoLocMenuitem(ini, mnuTreeUnfoldAll, 'ct', 'fld_un');
    DoLocMenuitem(ini, mnuTreeFoldLevel, 'ct', 'fld_lv');
    DoLocMenuitem(ini, mnuTreeSorted, 'ct', 'tr_sr');

    cStrMenuitemFoldAll:= mnuTreeFoldAll.Caption;
    cStrMenuitemUnfoldAll:= mnuTreeUnfoldAll.Caption;
    cStrMenuitemFoldLevel:= mnuTreeFoldLevel.Caption;

    DoLocMenuitem(ini, mnuTabsizeSpace, 'ct', 'tsz_sp');

    ButtonCancel.Caption:= msgButtonCancel;

    //----------------strings
    DoLocString(msgCannotOpenFile                , ini, 'se', 'OpenFile');
    DoLocString(msgCannotFindFile                , ini, 'se', 'FindFile');
    DoLocString(msgCannotFindLexerInLibrary      , ini, 'se', 'FindLexerInLibrary');
    DoLocString(msgCannotFindLexerFile           , ini, 'se', 'FindLexerFile');
    DoLocString(msgCannotFindSublexerInLibrary   , ini, 'se', 'FindSublexerInLibrary');
    DoLocString(msgCannotCreateDir               , ini, 'se', 'CreateDir');
    DoLocString(msgCannotSaveFile                , ini, 'se', 'SaveFile');
    DoLocString(msgCannotSaveUserConf            , ini, 'se', 'SaveUserConf');
    DoLocString(msgCannotReadConf                , ini, 'se', 'ReadConf');
    DoLocString(msgCannotReloadUntitledTab       , ini, 'se', 'ReloadUntitledTab');
    DoLocString(msgCannotFindMatch               , ini, 'se', 'FindMatch');
    DoLocString(msgCannotFindInstallInfInZip     , ini, 'se', 'FindInstallInfInZip');
    DoLocString(msgCannotFindBookmarks           , ini, 'se', 'FindBookmarks');
    DoLocString(msgCannotHandleZip               , ini, 'se', 'HandleZip');
    DoLocString(msgCannotInstallAddonApi         , ini, 'se', 'AddonApi');

    DoLocString(msgStatusChangedLinesCount       , ini, 'si', 'ChangedLineCount');
    DoLocString(msgStatusIncorrectFilename       , ini, 'si', 'IncorrectFilename');
    DoLocString(msgStatusIncorrectInstallInfInZip, ini, 'si', 'IncorrectInstallInfInZip');
    DoLocString(msgStatusUnsupportedAddonType    , ini, 'si', 'UnsupportedAddonType');
    DoLocString(msgStatusPackageContains         , ini, 'si', 'PackageContains');
    DoLocString(msgStatusPackageName             , ini, 'si', 'PackageName');
    DoLocString(msgStatusPackageType             , ini, 'si', 'PackageType');
    DoLocString(msgStatusPackageDesc             , ini, 'si', 'PackageDesc');
    DoLocString(msgStatusPackageCommand          , ini, 'si', 'PackageCommand');
    DoLocString(msgStatusPackageEvents           , ini, 'si', 'PackageEvents');
    DoLocString(msgStatusPackageLexer            , ini, 'si', 'PackageLexer');
    DoLocString(msgStatusPackageAutoCompletion   , ini, 'si', 'PackageAutocompl');
    DoLocString(msgStatusPackageMissedLexerMap   , ini, 'si', 'PackageMissLexmap');
    DoLocString(msgStatusInstalledNeedRestart    , ini, 'si', 'InstalledNeedRestart');
    DoLocString(msgStatusErrorInConfigFile       , ini, 'si', 'ErrorInConfigFile');
    DoLocString(msgStatusCommandOnlyForLexers    , ini, 'si', 'CommandOnlyForLexers');
    DoLocString(msgStatusOpenedBrowser           , ini, 'si', 'OpenedBrowser');
    DoLocString(msgStatusCopiedLink              , ini, 'si', 'CopiedLink');
    DoLocString(msgStatusAddonInstalled          , ini, 'si', 'AddonInstalled');
    DoLocString(msgStatusOpened                  , ini, 'si', 'Opened');
    DoLocString(msgStatusReopened                , ini, 'si', 'Reopened');
    DoLocString(msgStatusBadRegex                , ini, 'si', 'BadRegex');
    DoLocString(msgStatusFoundNextMatch          , ini, 'si', 'FoundNextMatch');
    DoLocString(msgStatusTryingAutocomplete      , ini, 'si', 'TryingAutocomplete');
    DoLocString(msgStatusHelpOnShowCommands      , ini, 'si', 'HelpOnShowCommands');
    DoLocString(msgStatusNoLineCmtDefined        , ini, 'si', 'NoLineCmtDefined');
    DoLocString(msgStatusReplaceCount            , ini, 'si', 'ReplaceCount');
    DoLocString(msgStatusFindCount               , ini, 'si', 'FindCount');
    DoLocString(msgStatusReadingOps              , ini, 'si', 'ReadingOps');
    DoLocString(msgStatusSavedFile               , ini, 'si', 'SavedFile');
    DoLocString(msgStatusReadonly                , ini, 'si', 'Readonly');
    DoLocString(msgStatusMacroRec                , ini, 'si', 'MacroRec');
    DoLocString(msgStatusPictureNxN              , ini, 'si', 'PictureNxN');
    DoLocString(msgStatusCancelled               , ini, 'si', 'Cancelled');
    DoLocString(msgStatusBadLineNum              , ini, 'si', 'BadLineNum');
    DoLocString(msgStatusEndsChanged             , ini, 'si', 'EndsChanged');
    DoLocString(msgStatusEncChanged              , ini, 'si', 'EncChanged');
    DoLocString(msgStatusGotoLine                , ini, 'si', 'GotoLine');
    DoLocString(msgStatusGotoFileLineCol         , ini, 'si', 'FileLineCol');
    DoLocString(msgStatusHelpOnKeysConfig        , ini, 'si', 'HelpOnKeysConfig');
    DoLocString(msgStatusClickingLogLine         , ini, 'si', 'ClickingLogLine');
    DoLocString(msgStatusNoGotoDefinitionPlugins , ini, 'si', 'NoGotoDefinitionPlugins');
    DoLocString(msgStatusFilenameAlreadyOpened   , ini, 'si', 'FilenameAlreadyOpened');
    DoLocString(msgStatusNeedToCloseTabSavedOrDup, ini, 'si', 'NeedToCloseTabSavedOrDup');
    DoLocString(msgStatusHotkeyBusy              , ini, 'si', 'HotkeyBusy');

    DoLocString(msgConfirmSyntaxThemeSameName    , ini, 'sc', 'ThemeSyntaxSame');
    DoLocString(msgConfirmInstallIt              , ini, 'sc', 'InstallIt');
    DoLocString(msgConfirmFileChangedOutside     , ini, 'sc', 'FileChangedOutside');
    DoLocString(msgConfirmReloadIt               , ini, 'sc', 'ReloadIt');
    DoLocString(msgConfirmReloadItHotkeys        , ini, 'sc', 'ReloadItHotkeys');
    DoLocString(msgConfirmReloadItHotkeysSess    , ini, 'sc', 'ReloadItHotkeysSess');
    DoLocString(msgConfirmOpenCreatedDoc         , ini, 'sc', 'OpenCreatedDoc');
    DoLocString(msgConfirmSaveColorsToFile       , ini, 'sc', 'SaveColorsToFile');
    DoLocString(msgConfirmSaveModifiedTab        , ini, 'sc', 'SaveModifiedTab');
    DoLocString(msgConfirmReopenModifiedTab      , ini, 'sc', 'ReopenModifiedTab');
    DoLocString(msgConfirmReloadFileWithEnc      , ini, 'sc', 'ReloadFileWithEnc');
    DoLocString(msgConfirmCreateNewFile          , ini, 'sc', 'CreateNewFile');
    DoLocString(msgConfirmCreateUserConf         , ini, 'sc', 'CreateUserConf');
    DoLocString(msgConfirmCloseDelFile           , ini, 'sc', 'CloseDelFile');
    DoLocString(msgConfirmDeleteLexer            , ini, 'sc', 'DeleteLexer');
    DoLocString(msgConfirmRemoveStylesFromBackup , ini, 'sc', 'RemoveStylesFromBackup');
    DoLocString(msgConfirmHotkeyBusy             , ini, 'sc', 'HotkeyBusy');

  finally
    FreeAndNil(ini);
  end;

  DoLocalize_FormFind;
  DoLocalize_FormGoto;
  if Assigned(fmCharmaps) then
    DoLocalize_FormCharmap(fmCharmaps);

  //hack to localize already opened tab title
  if FrameCount>0 then
    with Frames[0] do
      if TabCaption='Untitled1' then
        TabCaption:= msgUntitledTab+'1';
end;

procedure TfmMain.DoLocalize_FormFind;
const
  section = 'd_f';
const
  init_HintRegex = 'Regular expressions';
  init_HintCase = 'Case sensitive';
  init_HintWords = 'Whole words';
  init_HintWrapped = 'Wrapped search';
  init_HintConfRep = 'Confirm on replace';
  init_HintMulLine = 'Multi-line inputs (Ctrl+Enter for new-line)';
  init_HintInSelect = 'Search in selection';
var
  fn: string;
  ini: TIniFile;
begin
  if not Assigned(fmFind) then exit;

  with fmFind.chkRegex do Hint:= init_HintRegex     +' - '+UiOps.HotkeyToggleRegex;
  with fmFind.chkCase do Hint:= init_HintCase       +' - '+UiOps.HotkeyToggleCaseSens;
  with fmFind.chkWords do Hint:= init_HintWords     +' - '+UiOps.HotkeyToggleWords;
  with fmFind.chkWrap do Hint:= init_HintWrapped    +' - '+UiOps.HotkeyToggleWrapped;
  with fmFind.chkConfirm do Hint:= init_HintConfRep +' - '+UiOps.HotkeyToggleConfirmRep;
  with fmFind.chkInSel do Hint:= init_HintInSelect  +' - '+UiOps.HotkeyToggleInSelect;
  with fmFind.chkMulLine do Hint:= init_HintMulLine +' - '+UiOps.HotkeyToggleMultiline;

  fn:= GetAppLangFilename;
  if not FileExists(fn) then exit;

  ini:= TIniFile.Create(fn);
  try
    with fmFind do FCaptionFind:= ini.ReadString(section, '_f', FCaptionFind);
    with fmFind do FCaptionReplace:= ini.ReadString(section, '_r', FCaptionReplace);
    with fmFind.bFindFirst do Caption:= ini.ReadString(section, 'f_f', Caption);
    with fmFind.bFindNext do Caption:= ini.ReadString(section, 'f_n', Caption);
    with fmFind.bFindPrev do Caption:= ini.ReadString(section, 'f_p', Caption);
    with fmFind.bCount do Caption:= ini.ReadString(section, 'cnt', Caption);
    with fmFind.bSelectAll do Caption:= ini.ReadString(section, 'sel', Caption);
    with fmFind.bMarkAll do Caption:= ini.ReadString(section, 'mk', Caption);
    with fmFind.bRep do Caption:= ini.ReadString(section, 'r_c', Caption);
    with fmFind.bRepAll do Caption:= ini.ReadString(section, 'r_a', Caption);
    with fmFind.LabelFind do Caption:= ini.ReadString(section, 'f_tx', Caption);
    with fmFind.LabelRep do Caption:= ini.ReadString(section, 'r_tx', Caption);

    with fmFind.chkRegex do Hint:= ini.ReadString(section, 'h_re', init_HintRegex)      +' - '+UiOps.HotkeyToggleRegex;
    with fmFind.chkCase do Hint:= ini.ReadString(section, 'h_ca', init_HintCase)        +' - '+UiOps.HotkeyToggleCaseSens;
    with fmFind.chkWords do Hint:= ini.ReadString(section, 'h_wo', init_HintWords)      +' - '+UiOps.HotkeyToggleWords;
    with fmFind.chkWrap do Hint:= ini.ReadString(section, 'h_wr', init_HintWrapped)     +' - '+UiOps.HotkeyToggleWrapped;
    with fmFind.chkConfirm do Hint:= ini.ReadString(section, 'h_cf', init_HintConfRep)  +' - '+UiOps.HotkeyToggleConfirmRep;
    with fmFind.chkInSel do Hint:= ini.ReadString(section, 'h_sel', init_HintInSelect)  +' - '+UiOps.HotkeyToggleInSelect;
    with fmFind.chkMulLine do Hint:= ini.ReadString(section, 'h_mul', init_HintMulLine) +' - '+UiOps.HotkeyToggleMultiline;

    with fmFind.edFind do
    begin
      MenuitemTextCut.Caption:= msgEditCut;
      MenuitemTextCopy.Caption:= msgEditCopy;
      MenuitemTextPaste.Caption:= msgEditPaste;
      MenuitemTextDelete.Caption:= msgEditDelete;
      MenuitemTextSelAll.Caption:= msgEditSelectAll;
      MenuitemTextUndo.Caption:= msgEditUndo;
      MenuitemTextRedo.Caption:= msgEditRedo;
    end;
    with fmFind.edRep do
    begin
      MenuitemTextCut.Caption:= msgEditCut;
      MenuitemTextCopy.Caption:= msgEditCopy;
      MenuitemTextPaste.Caption:= msgEditPaste;
      MenuitemTextDelete.Caption:= msgEditDelete;
      MenuitemTextSelAll.Caption:= msgEditSelectAll;
      MenuitemTextUndo.Caption:= msgEditUndo;
      MenuitemTextRedo.Caption:= msgEditRedo;
    end;

  finally
    FreeAndNil(ini);
  end;
end;

procedure TfmMain.DoLocalize_FormGoto;
const
  section = 'd_f';
var
  fn: string;
  ini: TIniFile;
begin
  if not Assigned(fmGoto) then exit;

  fn:= GetAppLangFilename;
  if not FileExistsUTF8(fn) then exit;

  ini:= TIniFile.Create(fn);
  try
    with fmGoto.LabelGoto do Caption:= ini.ReadString(section, 'go_tx', Caption);
    with fmGoto.bGoto do Caption:= ini.ReadString(section, 'go_b', Caption);

    with fmGoto.edInput do
    begin
      MenuitemTextCut.Caption:= msgEditCut;
      MenuitemTextCopy.Caption:= msgEditCopy;
      MenuitemTextPaste.Caption:= msgEditPaste;
      MenuitemTextDelete.Caption:= msgEditDelete;
      MenuitemTextSelAll.Caption:= msgEditSelectAll;
      MenuitemTextUndo.Caption:= msgEditUndo;
      MenuitemTextRedo.Caption:= msgEditRedo;
    end;

  finally
    FreeAndNil(ini);
  end;
end;

