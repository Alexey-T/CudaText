{$ifdef nn}begin end;{$endif}

procedure TfmMain.DoApplyLang;
var
  fn: string;
  ini: TIniFile;
  s: string;
  data: TATTabData;
begin
  fn:= GetLangFilename;
  if not FileExistsUTF8(fn) then exit;

  ini:= TIniFile.Create(fn);
  with ini do
  try
    with mnuFile do Caption:= ini.ReadString('m_f', '_', Caption);
    with mnuFileNew do Caption:= ini.ReadString('m_f', 'new', Caption);
    with mnuFileOpen do Caption:= ini.ReadString('m_f', 'op', Caption);
    with mnuFileOpenSub do Caption:= ini.ReadString('m_f', 'rec', Caption);
    with mnuFileReopen do Caption:= ini.ReadString('m_f', 'reop', Caption);
    with mnuFileSave do Caption:= ini.ReadString('m_f', 's', Caption);
    with mnuFileSaveAs do Caption:= ini.ReadString('m_f', 's_as', Caption);
    with mnuFileSaveAll do Caption:= ini.ReadString('m_f', 's_all', Caption);
    with mnuFileClose do Caption:= ini.ReadString('m_f', 'cl', Caption);
    with mnuFileCloseAll do Caption:= ini.ReadString('m_f', 'cl_all', Caption);
    with mnuFileCloseOther do Caption:= ini.ReadString('m_f', 'cl_oth', Caption);
    with mnuFileCloseDel do Caption:= ini.ReadString('m_f', 'cl_del', Caption);
    with mnuFileEnc do Caption:= ini.ReadString('m_f', 'e_', Caption);
    with mnuFileEnds do Caption:= ini.ReadString('m_f', 'z_', Caption);
    with mnuFileHtml do Caption:= ini.ReadString('m_f', 'html', Caption);
    with mnuFileExit do Caption:= ini.ReadString('m_f', 'q', Caption);

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

    with mnuFileEndWin do Caption:= msgEndWin;
    with mnuFileEndUn do Caption:= msgEndUnix;
    with mnuFileEndMac do Caption:= msgEndMac;

    with mnuEndsWin do Caption:= msgEndWin;
    with mnuEndsUnix do Caption:= msgEndUnix;
    with mnuEndsMac do Caption:= msgEndMac;

    msgEditUndo:= ini.ReadString('m_e', 'undo', msgEditUndo);
    msgEditRedo:= ini.ReadString('m_e', 'redo', msgEditRedo);
    msgEditCut:= ini.ReadString('m_e', 'cut', msgEditCut);
    msgEditCopy:= ini.ReadString('m_e', 'cp', msgEditCopy);
    msgEditPaste:= ini.ReadString('m_e', 'pst', msgEditPaste);
    msgEditDelete:= ini.ReadString('m_e', 'del', msgEditDelete);
    msgEditSelectAll:= ini.ReadString('m_se', 'al', msgEditSelectAll);

    with mnuEditUndo do Caption:= msgEditUndo;
    with mnuEditRedo do Caption:= msgEditRedo;
    with mnuEditCut do Caption:= msgEditCut;
    with mnuEditCopy do Caption:= msgEditCopy;
    with mnuEditPaste do Caption:= msgEditPaste;
    with mnuEditDel do Caption:= msgEditDelete;

    with mnuEdit do Caption:= ini.ReadString('m_e', '_', Caption);
    with mnuEditUndo do Caption:= ini.ReadString('m_e', 'undo', Caption);
    with mnuEditRedo do Caption:= ini.ReadString('m_e', 'redo', Caption);
    with mnuEditCut do Caption:= ini.ReadString('m_e', 'cut', Caption);
    with mnuEditCopy do Caption:= ini.ReadString('m_e', 'cp', Caption);
    with mnuEditPaste do Caption:= ini.ReadString('m_e', 'pst', Caption);
    with mnuEditDel do Caption:= ini.ReadString('m_e', 'del', Caption);

    with mnuEditCopySub do Caption:= ini.ReadString('m_e', 'cp_', Caption);
    with mnuEditCopyLine do Caption:= ini.ReadString('m_e', 'cp_ln', Caption);
    with mnuEditCopyAppend do Caption:= ini.ReadString('m_e', 'cp_add', Caption);
    with mnuEditCopyFFull do Caption:= ini.ReadString('m_e', 'cp_full', Caption);
    with mnuEditCopyFName do Caption:= ini.ReadString('m_e', 'cp_name', Caption);
    with mnuEditCopyFDir do Caption:= ini.ReadString('m_e', 'cp_dir', Caption);

    with mnuEditLineOp do Caption:= ini.ReadString('m_e', 'ln_', Caption);
    with mnuEditLineDel do Caption:= ini.ReadString('m_e', 'ln_del', Caption);
    with mnuEditLineDup do Caption:= ini.ReadString('m_e', 'ln_dup', Caption);
    with mnuEditLineMoveUp do Caption:= ini.ReadString('m_e', 'ln_up', Caption);
    with mnuEditLineMoveDown do Caption:= ini.ReadString('m_e', 'ln_dn', Caption);

    with mnuEditIndentSub do Caption:= ini.ReadString('m_e', 'bl_', Caption);
    with mnuEditIndent do Caption:= ini.ReadString('m_e', 'bl_ind', Caption);
    with mnuEditUnindent do Caption:= ini.ReadString('m_e', 'bl_uind', Caption);
    with mnuEditTrim do Caption:= ini.ReadString('m_e', 'tr_a', Caption);
    with mnuEditTrimL do Caption:= ini.ReadString('m_e', 'tr_l', Caption);
    with mnuEditTrimR do Caption:= ini.ReadString('m_e', 'tr_r', Caption);
    with mnuEditTabToSp do Caption:= ini.ReadString('m_e', 'con_t', Caption);
    with mnuEditSpToTab do Caption:= ini.ReadString('m_e', 'con_s', Caption);

    with mnuCaseSub do Caption:= ini.ReadString('m_e', 'cas_', Caption);
    with mnuCaseUp do Caption:= ini.ReadString('m_e', 'cas_u', Caption);
    with mnuCaseLow do Caption:= ini.ReadString('m_e', 'cas_l', Caption);
    with mnuCaseTitle do Caption:= ini.ReadString('m_e', 'cas_t', Caption);
    with mnuCaseInvert do Caption:= ini.ReadString('m_e', 'cas_i', Caption);
    with mnuCaseSent do Caption:= ini.ReadString('m_e', 'cas_s', Caption);

    with mnuSortSub do Caption:= ini.ReadString('m_e', 'sr_', Caption);
    with mnuSortAsc do Caption:= ini.ReadString('m_e', 'sr_a', Caption);
    with mnuSortDesc do Caption:= ini.ReadString('m_e', 'sr_d', Caption);

    with mnuCmtSub do Caption:= ini.ReadString('m_e', 'com_', Caption);
    with mnuCmtAdd do Caption:= ini.ReadString('m_e', 'com_l_a', Caption);
    with mnuCmtRemove do Caption:= ini.ReadString('m_e', 'com_l_d', Caption);
    with mnuCmtToggle do Caption:= ini.ReadString('m_e', 'com_l_t', Caption);
    with mnuCmtToggleStr do Caption:= ini.ReadString('m_e', 'com_s_t', Caption);

    with mnuEditCharmap do Caption:= ini.ReadString('m_e', 'map', Caption);

    with mnuSel do Caption:= ini.ReadString('m_se', '_', Caption);
    with mnuSelAll do Caption:= msgEditSelectAll;
    with mnuSelSplit do Caption:= ini.ReadString('m_se', 'sp', Caption);
    with mnuSelExtLine do Caption:= ini.ReadString('m_se', 'ex', Caption);
    with mnuSelInvert do Caption:= ini.ReadString('m_se', 'in', Caption);

    with mnuCaretsCancel do Caption:= ini.ReadString('m_se', 'can', Caption);
    with mnuCaretsExtSub do Caption:= ini.ReadString('m_se', 'm_', Caption);
    with mnuCaretsUp1Line do Caption:= ini.ReadString('m_se', 'u_l', Caption);
    with mnuCaretsUp1Page do Caption:= ini.ReadString('m_se', 'u_p', Caption);
    with mnuCaretsUpBegin do Caption:= ini.ReadString('m_se', 'u_z', Caption);
    with mnuCaretsDown1Line do Caption:= ini.ReadString('m_se', 'd_l', Caption);
    with mnuCaretsDown1Page do Caption:= ini.ReadString('m_se', 'd_p', Caption);
    with mnuCaretsDownEnd do Caption:= ini.ReadString('m_se', 'd_z', Caption);

    with mnuSr do Caption:= ini.ReadString('m_sr', '_', Caption);
    with mnuFindDlg do Caption:= ini.ReadString('m_sr', 'f', Caption);
    with mnuFindRepDialog do Caption:= ini.ReadString('m_sr', 'r', Caption);
    with mnuFindNext do Caption:= ini.ReadString('m_sr', 'f_n', Caption);
    with mnuFindPrev do Caption:= ini.ReadString('m_sr', 'f_p', Caption);
    with mnuFindWordNext do Caption:= ini.ReadString('m_sr', 'w_n', Caption);
    with mnuFindWordPrev do Caption:= ini.ReadString('m_sr', 'w_p', Caption);

    mnuFind2Next.Caption:= mnuFindNext.Caption;
    mnuFind2Prev.Caption:= mnuFindPrev.Caption;
    mnuFind2WordNext.Caption:= mnuFindWordNext.Caption;
    mnuFind2WordPrev.Caption:= mnuFindWordPrev.Caption;

    with mnuGotoLine do Caption:= ini.ReadString('m_sr', 'g_l', Caption);
    with mnuGotoBm do Caption:= ini.ReadString('m_sr', 'g_b', Caption);

    with mnuBmSub do Caption:= ini.ReadString('m_sr', 'b_', Caption);
    with mnuBmNext do Caption:= ini.ReadString('m_sr', 'b_n', Caption);
    with mnuBmPrev do Caption:= ini.ReadString('m_sr', 'b_p', Caption);
    with mnuBmToggle do Caption:= ini.ReadString('m_sr', 'b_to', Caption);
    with mnuBmInvert do Caption:= ini.ReadString('m_sr', 'b_in', Caption);
    with mnuBmClear do Caption:= ini.ReadString('m_sr', 'b_cl', Caption);

    with mnuView do Caption:= ini.ReadString('m_v', '_', Caption);
    with mnuGroups do Caption:= ini.ReadString('m_v', 'g_', Caption);
    with mnuGr1 do Caption:= ini.ReadString('m_v', 'g1', Caption);
    with mnuGr2H do Caption:= ini.ReadString('m_v', 'g2h', Caption);
    with mnuGr2V do Caption:= ini.ReadString('m_v', 'g2v', Caption);
    with mnuGr3H do Caption:= ini.ReadString('m_v', 'g3h', Caption);
    with mnuGr3V do Caption:= ini.ReadString('m_v', 'g3v', Caption);
    with mnuGr1p2H do Caption:= ini.ReadString('m_v', 'g12h', Caption);
    with mnuGr1p2V do Caption:= ini.ReadString('m_v', 'g12v', Caption);
    with mnuGr4H do Caption:= ini.ReadString('m_v', 'g4h', Caption);
    with mnuGr4V do Caption:= ini.ReadString('m_v', 'g4v', Caption);
    with mnuGr4G do Caption:= ini.ReadString('m_v', 'g4g', Caption);
    with mnuGr6 do Caption:= ini.ReadString('m_v', 'g6', Caption);

    with mnuViewSplitSub do Caption:= ini.ReadString('m_v', 's_', Caption);
    with mnuViewSplitDo do Caption:= ini.ReadString('m_v', 's_s', Caption);
    with mnuViewSplitHorz do Caption:= ini.ReadString('m_v', 's_h', Caption);

    with mnuViewWrap do Caption:= ini.ReadString('m_v', 't_ww', Caption);
    with mnuViewNums do Caption:= ini.ReadString('m_v', 't_nu', Caption);
    with mnuViewFold do Caption:= ini.ReadString('m_v', 't_fo', Caption);
    with mnuViewRuler do Caption:= ini.ReadString('m_v', 't_ru', Caption);
    with mnuViewMinimap do Caption:= ini.ReadString('m_v', 't_mi', Caption);

    with mnuViewUnpri do Caption:= ini.ReadString('m_v', 'u_', Caption);
    with mnuViewUnpriShow do Caption:= ini.ReadString('m_v', 'u_sh', Caption);
    with mnuViewUnpriSpaces do Caption:= ini.ReadString('m_v', 'u_sp', Caption);
    with mnuViewUnpriEnds do Caption:= ini.ReadString('m_v', 'u_e', Caption);
    with mnuViewUnpriEndsDet do Caption:= ini.ReadString('m_v', 'u_ed', Caption);

    with mnuViewToolbar do Caption:= ini.ReadString('m_v', 't_tb', Caption);
    with mnuViewStatus do Caption:= ini.ReadString('m_v', 't_sb', Caption);
    with mnuViewSide do Caption:= ini.ReadString('m_v', 't_si', Caption);
    with mnuViewBottom do Caption:= ini.ReadString('m_v', 't_bo', Caption);
    with mnuViewFullscr do Caption:= ini.ReadString('m_v', 't_fs', Caption);

    with mnuPlug do Caption:= ini.ReadString('m_o', 'pl_', Caption);
    with mnuOp do Caption:= ini.ReadString('m_o', '_', Caption);

    with mnuOpDefault do Caption:= ini.ReadString('m_o', 'df', Caption);
    with mnuOpUser do Caption:= ini.ReadString('m_o', 'us', Caption);
    with mnuOpMore do Caption:= ini.ReadString('m_o', 'mr', Caption);
    with mnuOpLexer do Caption:= ini.ReadString('m_o', 'lx', Caption);
    with mnuOpFileTypes do Caption:= ini.ReadString('m_o', 'fi', Caption);
    with mnuOpColors do Caption:= ini.ReadString('m_o', 'co', Caption);
    with mnuOpKeys do Caption:= ini.ReadString('m_o', 'ke', Caption);

    with mnuFontSub do Caption:= ini.ReadString('m_o', 'f_', Caption);
    with mnuFontText do Caption:= ini.ReadString('m_o', 'f_t', Caption);
    with mnuFontUi do Caption:= ini.ReadString('m_o', 'f_u', Caption);
    with mnuFontOutput do Caption:= ini.ReadString('m_o', 'f_o', Caption);

    with mnuOpLexSub do Caption:= ini.ReadString('m_o', 'l_', Caption);
    with mnuOpLexProp do Caption:= ini.ReadString('m_o', 'l_pr', Caption);
    with mnuOpLexLib do Caption:= ini.ReadString('m_o', 'l_li', Caption);

    with mnuThemes do Caption:= ini.ReadString('m_o', 'co_', Caption);
    with mnuLang do Caption:= ini.ReadString('m_o', 'tr_', Caption);

    with mnuHelp do Caption:= ini.ReadString('m_h', '_', Caption);
    with mnuHelpCmd do Caption:= ini.ReadString('m_h', 'co', Caption);
    with mnuHelpForum do Caption:= ini.ReadString('m_h', 'fo', Caption);
    with mnuHelpWiki do Caption:= ini.ReadString('m_h', 'wi', Caption);
    with mnuHelpMouse do Caption:= ini.ReadString('m_h', 'mo', Caption);
    with mnuHelpLexers do Caption:= ini.ReadString('m_h', 'lx', Caption);
    with mnuHelpChangelog do Caption:= ini.ReadString('m_h', 'ch', Caption);
    with mnuHelpAbout do Caption:= ini.ReadString('m_h', 'ab', Caption);

    data:= TabsBottom.GetTabData(0);
    if Assigned(data) then data.TabCaption:= ini.ReadString('ui', 't_con', 'Console');
    data:= TabsBottom.GetTabData(1);
    if Assigned(data) then data.TabCaption:= ini.ReadString('ui', 't_out', 'Output');
    data:= TabsBottom.GetTabData(2);
    if Assigned(data) then data.TabCaption:= ini.ReadString('ui', 't_val', 'Validate');
    data:= TabsBottom.GetTabData(3);
    if Assigned(data) then data.TabCaption:= ini.ReadString('ui', 't_sr', 'Search Results');
    data:= TabsLeft.GetTabData(0);
    if Assigned(data) then data.TabCaption:= ini.ReadString('ui', 't_tre', 'Tree');

    msgUntitledTab:= ini.ReadString('ui', 'unt', msgUntitledTab);
    msgNoLexer:= ini.ReadString('ui', 'nlex', msgNoLexer);
    UpdateMenuLexers;
    msgFileClearList:= ini.ReadString('ui', 'clr', msgFileClearList);
    UpdateMenuRecent(nil);

    cHintScrollPrefix:= ini.ReadString('ui', 'line', cHintScrollPrefix);

    with tbNew do Hint:= ini.ReadString('ui', 'tb_new', Hint);
    with tbOpen do Hint:= ini.ReadString('ui', 'tb_op', Hint);
    with tbSave do Hint:= ini.ReadString('ui', 'tb_sav', Hint);
    with tbFind do Hint:= ini.ReadString('ui', 'tb_find', Hint);
    with tbGoto do Hint:= ini.ReadString('ui', 'tb_go', Hint);

    with tbCut do Hint:= msgEditCut;
    with tbCopy do Hint:= msgEditCopy;
    with tbPaste do Hint:= msgEditPaste;
    with tbDel do Hint:= msgEditDelete;
    with tbSelAll do Hint:= msgEditSelectAll;
    with tbUndo do Hint:= msgEditUndo;
    with tbRedo do Hint:= msgEditRedo;
    with tbUnpri do Hint:= mnuViewUnpri.Caption;
    with tbSidePanel do Hint:= mnuViewSide.Caption;
    with tbBtmPanel do Hint:= mnuViewBottom.Caption;
    with tbMinimap do Hint:= mnuViewMinimap.Caption;

    with mnuTextUndo do Caption:= msgEditUndo;
    with mnuTextRedo do Caption:= msgEditRedo;
    with mnuTextCut do Caption:= msgEditCut;
    with mnuTextCopy do Caption:= msgEditCopy;
    with mnuTextPaste do Caption:= msgEditPaste;
    with mnuTextDelete do Caption:= msgEditDelete;
    with mnuTextSel do Caption:= msgEditSelectAll;
    with mnuTextGotoDef do Caption:= ini.ReadString('ct', 'godef', Caption);
    with mnuTextOpenUrl do Caption:= ini.ReadString('ct', 'url', Caption);

    with fmConsole.mnuTextClear do Caption:= ini.ReadString('ct', 'clr', Caption);
    with fmConsole.mnuTextNav do Caption:= ini.ReadString('ct', 'nav', Caption);

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

    with mnuTabCloseThis do Caption:= ini.ReadString('ct', 'c_tab', Caption);
    with mnuTabCloseSub do Caption:= ini.ReadString('ct', 'c_', Caption);
    with mnuTabCloseOtherSame do Caption:= ini.ReadString('ct', 'c_oth_s', Caption);
    with mnuTabCloseOtherAll do Caption:= ini.ReadString('ct', 'c_oth_a', Caption);
    with mnuTabCloseAllSame do Caption:= ini.ReadString('ct', 'c_all_s', Caption);
    with mnuTabCloseAllAll do Caption:= ini.ReadString('ct', 'c_all_a', Caption);
    with mnuTabCloseLeft do Caption:= ini.ReadString('ct', 'c_lt_s', Caption);
    with mnuTabCloseRight do Caption:= ini.ReadString('ct', 'c_rt_s', Caption);

    with mnuTabSave do Caption:= ini.ReadString('ct', 'sv', Caption);
    with mnuTabSaveAs do Caption:= ini.ReadString('ct', 'sv_as', Caption);
    with mnuTabColor do Caption:= ini.ReadString('ct', 'col_t', Caption);

    with mnuTabMoveSub do Caption:= ini.ReadString('ct', 'mov_', Caption);
    with mnuTabMoveNext do Caption:= ini.ReadString('ct', 'mov_n', Caption);
    with mnuTabMovePrev do Caption:= ini.ReadString('ct', 'mov_p', Caption);

    with mnuTreeFoldAll do Caption:= ini.ReadString('ct', 'fld_a', Caption);
    with mnuTreeUnfoldAll do Caption:= ini.ReadString('ct', 'fld_un', Caption);
    with mnuTreeFoldLevel do Caption:= ini.ReadString('ct', 'fld_lv', Caption);

    cStrMenuitemFoldAll:= mnuTreeFoldAll.Caption;
    cStrMenuitemUnfoldAll:= mnuTreeUnfoldAll.Caption;
    cStrMenuitemFoldLevel:= mnuTreeFoldLevel.Caption;

    s:= ini.ReadString('ct', 'tsz', 'Tab size:');
    with mnuTabsize1 do Caption:= s+' 1';
    with mnuTabsize2 do Caption:= s+' 2';
    with mnuTabsize4 do Caption:= s+' 4';
    with mnuTabsize8 do Caption:= s+' 8';
    with mnuTabsizeSpace do Caption:= ini.ReadString('ct', 'tsz_sp', Caption);

  finally
    FreeAndNil(ini);
  end;

  DoApplyLang_FormFind;
  DoApplyLang_FormGoto;
end;

procedure TfmMain.DoApplyLang_FormFind;
var
  fn: string;
  ini: TIniFile;
begin
  if not Assigned(fmFind) then exit;

  fn:= GetLangFilename;
  if not FileExistsUTF8(fn) then exit;

  ini:= TIniFile.Create(fn);
  try
    with fmFind.bFindFirst do Caption:= ini.ReadString('d_f', 'f_f', Caption);
    with fmFind.bFindNext do Caption:= ini.ReadString('d_f', 'f_n', Caption);
    with fmFind.bFindPrev do Caption:= ini.ReadString('d_f', 'f_p', Caption);
    with fmFind.bCount do Caption:= ini.ReadString('d_f', 'cnt', Caption);
    with fmFind.bMarkAll do Caption:= ini.ReadString('d_f', 'mk', Caption);
    with fmFind.bRep do Caption:= ini.ReadString('d_f', 'r_c', Caption);
    with fmFind.bRepAll do Caption:= ini.ReadString('d_f', 'r_a', Caption);
    with fmFind.LabelFind do Caption:= ini.ReadString('d_f', 'f_tx', Caption);
    with fmFind.LabelRep do Caption:= ini.ReadString('d_f', 'r_tx', Caption);

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

procedure TfmMain.DoApplyLang_FormGoto;
var
  fn: string;
  ini: TIniFile;
begin
  if not Assigned(fmGoto) then exit;

  fn:= GetLangFilename;
  if not FileExistsUTF8(fn) then exit;

  ini:= TIniFile.Create(fn);
  try
    with fmGoto.LabelGoto do Caption:= ini.ReadString('d_f', 'go_tx', Caption);
    with fmGoto.bGoto do Caption:= ini.ReadString('d_f', 'go_b', Caption);

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

