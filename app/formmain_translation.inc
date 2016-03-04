{$ifdef nn}begin end;{$endif}

procedure TfmMain.DoApplyLanguage(const fn: string);
var
  ini: TIniFile;
begin
  if not FileExistsUTF8(fn) then
  begin
    MsgBox(msgCannotFindFile+#10+fn, MB_OK or MB_ICONERROR);
    exit
  end;

  ini:= TIniFile.Create(fn);
  with ini do
  try
    with mnuFile do Caption:= ini.ReadString('m_file', '_', Caption);
    with mnuFileNew do Caption:= ini.ReadString('m_file', 'new', Caption);
    with mnuFileOpen do Caption:= ini.ReadString('m_file', 'op', Caption);
    with mnuFileOpenSub do Caption:= ini.ReadString('m_file', 'rec', Caption);
    with mnuFileReopen do Caption:= ini.ReadString('m_file', 'reop', Caption);
    with mnuFileSave do Caption:= ini.ReadString('m_file', 's', Caption);
    with mnuFileSaveAs do Caption:= ini.ReadString('m_file', 's_as', Caption);
    with mnuFileSaveAll do Caption:= ini.ReadString('m_file', 's_all', Caption);
    with mnuFileClose do Caption:= ini.ReadString('m_file', 'cl', Caption);
    with mnuFileCloseAll do Caption:= ini.ReadString('m_file', 'cl_all', Caption);
    with mnuFileCloseOther do Caption:= ini.ReadString('m_file', 'cl_oth', Caption);
    with mnuFileCloseDel do Caption:= ini.ReadString('m_file', 'cl_del', Caption);
    with mnuFileEnc do Caption:= ini.ReadString('m_file', 'e', Caption);
    with mnuFileEnds do Caption:= ini.ReadString('m_file', 'end', Caption);
    with mnuFileHtml do Caption:= ini.ReadString('m_file', 'html', Caption);
    with mnuFileExit do Caption:= ini.ReadString('m_file', 'q', Caption);

    msgFileClearList:= ini.ReadString('m_file', 'clr', msgFileClearList);
    msgEncReloadAs:= ini.ReadString('m_file', 'e_r', msgEncReloadAs);
    msgEncConvertTo:= ini.ReadString('m_file', 'e_t', msgEncConvertTo);
    msgEncEuropean:= ini.ReadString('m_file', 'e_eu', msgEncEuropean);
    msgEncAsian:= ini.ReadString('m_file', 'e_as', msgEncAsian);
    msgEncMisc:= ini.ReadString('m_file', 'e_mi', msgEncMisc);
    UpdateMenuEnc(mnuFileEnc);
    UpdateMenuEnc(PopupEnc.Items);

    msgEndWin:= ini.ReadString('m_file', 'z_w', msgEndWin);
    msgEndUnix:= ini.ReadString('m_file', 'z_u', msgEndUnix);
    msgEndMac:= ini.ReadString('m_file', 'z_m', msgEndMac);

    with mnuFileEndWin do Caption:= msgEndWin;
    with mnuFileEndUn do Caption:= msgEndUnix;
    with mnuFileEndMac do Caption:= msgEndMac;

    with mnuEndsWin do Caption:= msgEndWin;
    with mnuEndsUnix do Caption:= msgEndUnix;
    with mnuEndsMac do Caption:= msgEndMac;

    with mnuEdit do Caption:= ini.ReadString('m_edit', '_', Caption);
    with mnuEditUndo do Caption:= ini.ReadString('m_edit', 'undo', Caption);
    with mnuEditRedo do Caption:= ini.ReadString('m_edit', 'redo', Caption);
    with mnuEditCut do Caption:= ini.ReadString('m_edit', 'cut', Caption);
    with mnuEditCopy do Caption:= ini.ReadString('m_edit', 'cp', Caption);
    with mnuEditPaste do Caption:= ini.ReadString('m_edit', 'pst', Caption);
    with mnuEditDel do Caption:= ini.ReadString('m_edit', 'del', Caption);

    with mnuEditCopySub do Caption:= ini.ReadString('m_edit', 'cp_', Caption);
    with mnuEditCopyLine do Caption:= ini.ReadString('m_edit', 'cp_ln', Caption);
    with mnuEditCopyAppend do Caption:= ini.ReadString('m_edit', 'cp_add', Caption);
    with mnuEditCopyFFull do Caption:= ini.ReadString('m_edit', 'cp_full', Caption);
    with mnuEditCopyFName do Caption:= ini.ReadString('m_edit', 'cp_name', Caption);
    with mnuEditCopyFDir do Caption:= ini.ReadString('m_edit', 'cp_dir', Caption);

    with mnuEditLineOp do Caption:= ini.ReadString('m_edit', 'ln_', Caption);
    with mnuEditLineDel do Caption:= ini.ReadString('m_edit', 'ln_del', Caption);
    with mnuEditLineDup do Caption:= ini.ReadString('m_edit', 'ln_dup', Caption);
    with mnuEditLineMoveUp do Caption:= ini.ReadString('m_edit', 'ln_up', Caption);
    with mnuEditLineMoveDown do Caption:= ini.ReadString('m_edit', 'ln_dn', Caption);

    with mnuEditIndentSub do Caption:= ini.ReadString('m_edit', 'bl_', Caption);
    with mnuEditIndent do Caption:= ini.ReadString('m_edit', 'bl_ind', Caption);
    with mnuEditUnindent do Caption:= ini.ReadString('m_edit', 'bl_uind', Caption);
    with mnuEditTrim do Caption:= ini.ReadString('m_edit', 'tr_a', Caption);
    with mnuEditTrimL do Caption:= ini.ReadString('m_edit', 'tr_l', Caption);
    with mnuEditTrimR do Caption:= ini.ReadString('m_edit', 'tr_r', Caption);
    with mnuEditTabToSp do Caption:= ini.ReadString('m_edit', 'con_t', Caption);
    with mnuEditSpToTab do Caption:= ini.ReadString('m_edit', 'con_s', Caption);

    with mnuCaseSub do Caption:= ini.ReadString('m_edit', 'cas_', Caption);
    with mnuCaseUp do Caption:= ini.ReadString('m_edit', 'cas_u', Caption);
    with mnuCaseLow do Caption:= ini.ReadString('m_edit', 'cas_l', Caption);
    with mnuCaseTitle do Caption:= ini.ReadString('m_edit', 'cas_t', Caption);
    with mnuCaseInvert do Caption:= ini.ReadString('m_edit', 'cas_i', Caption);
    with mnuCaseSent do Caption:= ini.ReadString('m_edit', 'cas_s', Caption);

    with mnuSortSub do Caption:= ini.ReadString('m_edit', 'sr_', Caption);
    with mnuSortAsc do Caption:= ini.ReadString('m_edit', 'sr_a', Caption);
    with mnuSortDesc do Caption:= ini.ReadString('m_edit', 'sr_d', Caption);

    with mnuCmtSub do Caption:= ini.ReadString('m_edit', 'com_', Caption);
    with mnuCmtAdd do Caption:= ini.ReadString('m_edit', 'com_l_a', Caption);
    with mnuCmtRemove do Caption:= ini.ReadString('m_edit', 'com_l_d', Caption);
    with mnuCmtToggle do Caption:= ini.ReadString('m_edit', 'com_l_t', Caption);
    with mnuCmtToggleStr do Caption:= ini.ReadString('m_edit', 'com_s_t', Caption);

    with mnuEditCharmap do Caption:= ini.ReadString('m_edit', 'map', Caption);

    with mnuSel do Caption:= ini.ReadString('m_sel', '_', Caption);
    with mnuSelAll do Caption:= ini.ReadString('m_sel', 'al', Caption);
    with mnuSelSplit do Caption:= ini.ReadString('m_sel', 'sp', Caption);
    with mnuSelExtLine do Caption:= ini.ReadString('m_sel', 'ex', Caption);
    with mnuSelInvert do Caption:= ini.ReadString('m_sel', 'in', Caption);

    with mnuCaretsCancel do Caption:= ini.ReadString('m_sel', 'can', Caption);
    with mnuCaretsExtSub do Caption:= ini.ReadString('m_sel', 'm_', Caption);
    with mnuCaretsUp1Line do Caption:= ini.ReadString('m_sel', 'u_l', Caption);
    with mnuCaretsUp1Page do Caption:= ini.ReadString('m_sel', 'u_p', Caption);
    with mnuCaretsUpBegin do Caption:= ini.ReadString('m_sel', 'u_z', Caption);
    with mnuCaretsDown1Line do Caption:= ini.ReadString('m_sel', 'd_l', Caption);
    with mnuCaretsDown1Page do Caption:= ini.ReadString('m_sel', 'd_p', Caption);
    with mnuCaretsDownEnd do Caption:= ini.ReadString('m_sel', 'd_z', Caption);

    with mnuSr do Caption:= ini.ReadString('m_sr', '_', Caption);
    with mnuFindDlg do Caption:= ini.ReadString('m_sr', 'f', Caption);
    with mnuFindRepDialog do Caption:= ini.ReadString('m_sr', 'r', Caption);
    with mnuFindNext do Caption:= ini.ReadString('m_sr', 'f_n', Caption);
    with mnuFindPrev do Caption:= ini.ReadString('m_sr', 'f_p', Caption);
    with mnuFindWordNext do Caption:= ini.ReadString('m_sr', 'w_n', Caption);
    with mnuFindWordPrev do Caption:= ini.ReadString('m_sr', 'w_p', Caption);
    with mnuGotoLine do Caption:= ini.ReadString('m_sr', 'g_l', Caption);
    with mnuGotoBm do Caption:= ini.ReadString('m_sr', 'g_b', Caption);

    with mnuBookmarksSub do Caption:= ini.ReadString('m_sr', 'b_', Caption);
    with mnuBmNext do Caption:= ini.ReadString('m_sr', 'b_n', Caption);
    with mnuBmPrev do Caption:= ini.ReadString('m_sr', 'b_p', Caption);
    with mnuBmToggle do Caption:= ini.ReadString('m_sr', 'b_to', Caption);
    with mnuBmInvert do Caption:= ini.ReadString('m_sr', 'b_in', Caption);
    with mnuBmClear do Caption:= ini.ReadString('m_sr', 'b_cl', Caption);

  finally
    FreeAndNil(ini);
  end;
end;

