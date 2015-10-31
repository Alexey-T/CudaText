(*
This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at http://mozilla.org/MPL/2.0/.

Copyright (c) Alexey Torgashin
*)
{$ifdef nn}begin end;{$endif}

procedure TfmMain.UpdateCaption;
var
  F: TEditorFrame;
  SName, SMark, SSession: string;
begin
  F:= CurrentFrame;
  if UiOps.ShowTitlePath then
  begin
    SName:= F.FileName;
    if SName='' then SName:= F.TabCaption;
  end
  else
    SName:= F.TabCaption;

  SMark:= IfThen(F.Editor.Modified, '*');

  SSession:= ChangeFileExt(ExtractFileName(FSessionFilename), '');
  if SSession<>'history session' then
    SSession:= ' {'+SSession+'}'
  else
    SSession:= '';

  Caption:= SMark + SName + SSession + ' - ' + msgTitle;
  Application.Title:= SMark + F.TabCaption + ' - ' + msgTitle;
end;

procedure TfmMain.UpdateStatus;
var
  Ed: TATSynEdit;
  F: TEditorFrame;
  S, fmt: string;
  an: TecSyntAnalyzer;
begin
  F:= CurrentFrame;
  Ed:= CurrentEditor;

  //------
  case EditorGetStatusType(Ed) of
    selSmall:   fmt:= UiOps.StatusSmallSel;
    selStream:  fmt:= UiOps.StatusStreamSel;
    selCol:     fmt:= UiOps.StatusColSel;
    selCarets:  fmt:= UiOps.StatusCarets;
    else        fmt:= UiOps.StatusNoSel;
  end;
  Status[cStatusPos]:= EditorFormatStatus(Ed, fmt);

  //------
  S:= F.EncodingName;
  Status[cStatusEnc]:= S;

  //------
  case F.LineEnds of
    cEndWin: S:= 'Win';
    cEndUnix: S:= 'Unix';
    cEndMac: S:= 'Mac';
    else S:= '?';
  end;
  Status[cStatusEnds]:= S;

  //------
  an:= F.Lexer;
  if an=nil then
    S:= msgNoLexer
  else
    S:= an.LexerName;
  Status[cStatusLexer]:= S;

  //------
  S:= Format('Tab size %d%s', [Ed.OptTabSize, IfThen(Ed.OptTabSpaces, '_')]);
  Status[cStatusTabsize]:= S;
end;

procedure TfmMain.InitStatusButton;
begin
  with Gauge do
  begin
    Parent:= Status;
    AnchorSideRight.Control:= Status;
    AnchorSideRight.Side:= asrBottom;
    AnchorSideBottom.Control:= Status;
    AnchorSideBottom.Side:= asrCenter;
    Anchors:= [akRight, akBottom];
    BorderSpacing.Right:= 5;
  end;
  with btnStop do
  begin
    Parent:= Status;
    AnchorSideRight.Control:= Gauge;
    AnchorSideRight.Side:= asrLeft;
    AnchorSideBottom.Control:= Status;
    AnchorSideBottom.Side:= asrCenter;
    Anchors:= [akRight, akBottom];
    BorderSpacing.Right:= 5;
  end;
  Gauge.Hide;
  btnStop.Hide;
end;

procedure TfmMain.UpdateFrame(AUpdatedText: boolean = false);
var
  F: TEditorFrame;
begin
  F:= CurrentFrame;

  F.Editor.UpdateIncorrectCaretPositions;
  F.Editor2.UpdateIncorrectCaretPositions;

  F.Editor.Update(AUpdatedText);
  F.Editor2.Update;

  if AUpdatedText then
  begin
    F.Adapter.OnEditorChange(F.Editor);
  end;
end;

procedure TfmMain.UpdateInputForm(Form: TForm; APreferHeight: integer);
var
  Ed: TATSynEdit;
  P: TPoint;
begin
  Ed:= CurrentEditor;
  P:= Ed.ClientToScreen(Point(0, 0));
  Form.Left:= Max(0, P.X+(Ed.Width-Form.Width) div 2);
  Form.Top:= Max(0, P.Y);
  Form.Height:= Min(Ed.Height, APreferHeight);
end;


procedure TfmMain.UpdateEnabledAll(b: boolean);
begin
  Groups.Enabled:= b;
  if Assigned(fmFind) then
    fmFind.Enabled:= b;
end;

procedure TfmMain.UpdateAppForSearch(AStart: boolean);
var
  Ed: TATSynEdit;
begin
  Ed:= CurrentEditor;
  if AStart then
  begin
    FFinder.Editor:= Ed;
    FFindStop:= false;
    FFindConfirmAll:= mrNone;
    btnStop.Show;
    Gauge.Show;
    Gauge.Progress:= 0;
    Ed.BeginUpdate;
    Ed.Enabled:= false;
    UpdateEnabledAll(false);
  end
  else
  begin
    btnStop.Hide;
    Gauge.Hide;
    UpdateEnabledAll(true);
    Ed.Enabled:= true;
    Ed.EndUpdate;

    if Assigned(fmFind) and fmFind.Visible then
      fmFind.edFind.SetFocus
    else
      Ed.SetFocus;
  end;
end;


procedure TfmMain.DoApplyFontFixed;
var
  F: TEditorFrame;
  i: integer;
begin
  fmConsole.Font.Name:= EditorOps.OpFontName;
  fmConsole.Font.Size:= EditorOps.OpFontSize;
  fmConsole.ed.Font:= fmConsole.Font;
  fmConsole.memo.Font:= fmConsole.Font;

  for i:= 0 to FrameCount-1 do
  begin
    F:= Frames[i];
    F.Editor.Font.Name:= EditorOps.OpFontName;
    F.Editor2.Font.Name:= EditorOps.OpFontName;
    F.Editor.Font.Size:= EditorOps.OpFontSize;
    F.Editor2.Font.Size:= EditorOps.OpFontSize;
    DoOps_LoadOptionsOverride(F);
  end;

  if Assigned(fmFind) then fmFind.UpdateFonts;
  if Assigned(fmGoto) then fmGoto.UpdateFonts;

  ListboxOut.ItemHeight:= GetDefaultListItemHeight;
  ListboxVal.ItemHeight:= ListboxOut.ItemHeight;
end;

procedure TfmMain.DoApplyFontVar;
begin
  Self.Font.Name:= UiOps.VarFontName;
  Self.Font.Size:= UiOps.VarFontSize;

  Groups.SetTabFont(Self.Font);
  TabsBottom.Font:= Self.Font;
  Status.Font:= Self.Font;
  Gauge.Font:= Self.Font;
  Tree.Font:= Self.Font;

  ATButtonTheme.FontName:= UiOps.VarFontName;
  ATButtonTheme.FontSize:= UiOps.VarFontSize;

  if Assigned(fmFind) then fmFind.UpdateFonts;
  if Assigned(fmGoto) then fmGoto.UpdateFonts;

  cCompleteFontName:= UiOps.VarFontName;
  cCompleteFontSize:= UiOps.VarFontSize;
  cCompleteItemHeight:= GetDefaultListItemHeight;
end;

procedure TfmMain.UpdateMenuRecent(F: TEditorFrame);
var
  sub: TMenuItem;
  mi: TMenuItem;
  i: integer;
begin
  if F=nil then
    SAddStringToHistory('', FListRecents, UiOps.MaxHistoryMenu)
  else
  begin
    if F.FileName='' then exit;
    SAddStringToHistory(F.FileName, FListRecents, UiOps.MaxHistoryMenu);
    F.DoSaveHistory;
  end;

  sub:= mnuFileOpenSub;
  if sub=nil then exit;
  sub.Clear;

  for i:= 0 to FListRecents.Count-1 do
  begin;
    mi:= TMenuItem.Create(Self);
    mi.Caption:= IntToStr(i+1)+'.  '+
      ExtractFileName(FListRecents[i])+'  ('+
      ExtractFileDir(FListRecents[i])+')';
    mi.Tag:= i;
    mi.OnClick:= @MenuRecentsClick;
    sub.Add(mi);
  end;

  mi:= TMenuItem.Create(Self);
  mi.Caption:= '-';
  sub.Add(mi);

  mi:= TMenuItem.Create(Self);
  mi.Caption:= 'Clear list';
  mi.OnClick:= @MenuRecentsClear;
  sub.Add(mi);

  //----------
  PopupRecents.Items.Clear;
  for i:= 0 to sub.Count-1 do
  begin
    mi:= TMenuItem.Create(Self);
    mi.Caption:= sub.Items[i].Caption;
    mi.Tag:= sub.Items[i].Tag;
    mi.OnClick:= sub.Items[i].OnClick;
    PopupRecents.Items.Add(mi);
  end;
end;

procedure TfmMain.UpdateMenuNewdoc;
var
  sub: TPopupMenu;
  mi, miSub: TMenuItem;
  list: TStringList;
  i, index: integer;
  subname: string;
  an: TecSyntAnalyzer;
begin
  sub:= PopupNewdoc;
  sub.Items.Clear;

  list:= TStringList.Create;
  try
    FFindFilesInDir(GetAppPath(cDirDataNewdoc), GetAllFilesMask, FListNewdoc);

    for i:= 0 to FListNewdoc.Count-1 do
    begin
      an:= AppFindLexer(FListNewdoc[i]);
      if an=nil then Continue;

      list.AddObject(
        an.LexerName+' - '+ExtractFileNameOnly(FListNewdoc[i]),
        TObject(ptrint(i)));
      list.Sort;
    end;

    miSub:= nil;
    for i:= 0 to list.Count-1 do
    begin
      index:= ptrint(list.Objects[i]);
      subname:= list[i][1];

      if (miSub=nil) or (miSub.Caption<>subname) then
      begin
        miSub:= TMenuItem.Create(Self);
        miSub.Caption:= subname;
        sub.Items.Add(miSub);
      end;

      if miSub<>nil then
      begin
        mi:= TMenuItem.Create(Self);
        mi.Caption:= list[i];
        mi.Tag:= index;
        mi.OnClick:= @MenuNewdocClick;
        miSub.Add(mi);
      end;
    end;
  finally
    list.Free;
  end;
end;

procedure TfmMain.DoApplyAllOps;
var
  F: TEditorFrame;
  i: integer;
begin
  DoApplyFontFixed;
  DoApplyFontVar;
  DoApplyUiOps;

  for i:= 0 to FrameCount-1 do
  begin
    F:= Frames[i];
    DoApplyFrameOps(F, EditorOps);
    DoOps_LoadOptionsOverride(F);
  end;

  UpdateStatus;
end;

procedure TfmMain.DoApplyFrameOps(F: TEditorFrame; const Op: TEditorOps);
begin
  EditorApplyOps(F.Editor, Op, false);
  EditorApplyOps(F.Editor2, Op, false);
  F.Adapter.DynamicHiliteEnabled:= Op.OpDynHilite;

  F.Editor.Update;
  if F.Splitted then
    F.Editor2.Update;

  UpdateStatus;
end;

procedure TfmMain.UpK(mi: TMenuItem; cmd: integer);
begin
  mi.Tag:= cmd;
  mi.OnClick:= @MenuMainClick;
  mi.ShortCut:= Keymap.GetShortcutFromCommand(cmd);
end;

procedure TfmMain.UpdateMenuHotkeys;
begin
  Upk(mnuFileNew, cmd_FileNew);
  Upk(mnuFileOpen, cmd_FileOpen);
  Upk(mnuFileReopen, cmd_FileReopen);
  Upk(mnuFileSave, cmd_FileSave);
  Upk(mnuFileSaveAs, cmd_FileSaveAs);
  Upk(mnuFileSaveAll, cmd_FileSaveAll);
  Upk(mnuFileClose, cmd_FileClose);
  Upk(mnuFileCloseAll, cmd_FileCloseAll);
  Upk(mnuFileCloseOther, cmd_FileCloseOtherThis);
  Upk(mnuFileCloseDel, cmd_FileCloseAndDelete);
  Upk(mnuFileExit, cmd_FileExit);
  Upk(mnuFileHtml, cmd_FileExportHtml);

  Upk(mnuSelAll, cCommand_SelectAll);
  Upk(mnuSelSplit, cCommand_SelectSplitToLines);
  Upk(mnuSelExtLine, cCommand_SelectExtendByLine);
  Upk(mnuSelInvert, cCommand_SelectInverted);

  Upk(mnuCaretsCancel, cCommand_Cancel);
  Upk(mnuCaretsUp1Line, cCommand_CaretsExtendUpLine);
  Upk(mnuCaretsUp1Page, cCommand_CaretsExtendUpPage);
  Upk(mnuCaretsUpBegin, cCommand_CaretsExtendUpToTop);
  Upk(mnuCaretsDown1Line, cCommand_CaretsExtendDownLine);
  Upk(mnuCaretsDown1Page, cCommand_CaretsExtendDownPage);
  Upk(mnuCaretsDownEnd, cCommand_CaretsExtendDownToEnd);

  Upk(mnuEditUndo, cCommand_Undo);
  Upk(mnuEditRedo, cCommand_Redo);
  Upk(mnuEditCut, cCommand_ClipboardCut);
  Upk(mnuEditCopy, cCommand_ClipboardCopy);
  Upk(mnuEditPaste, cCommand_ClipboardPaste);
  Upk(mnuEditDel, cCommand_TextDeleteSelection);

  upk(mnuEditCopyLine, cmd_CopyLine);
  upk(mnuEditCopyAppend, cCommand_ClipboardCopyAdd);
  upk(mnuEditCopyFFull, cmd_CopyFilenameFull);
  upk(mnuEditCopyFDir, cmd_CopyFilenameDir);
  upk(mnuEditCopyFName, cmd_CopyFilenameName);

  upk(mnuEditIndent, cCommand_TextIndent);
  upk(mnuEditUnindent, cCommand_TextUnindent);
  upk(mnuEditTrim, cCommand_TextTrimSpacesAll);
  upk(mnuEditTrimL, cCommand_TextTrimSpacesLeft);
  upk(mnuEditTrimR, cCommand_TextTrimSpacesRight);

  upk(mnuEditLineDel, cCommand_TextDeleteLine);
  upk(mnuEditLineDup, cCommand_TextDuplicateLine);
  upk(mnuEditLineMoveUp, cCommand_MoveSelectionUp);
  upk(mnuEditLineMoveDown, cCommand_MoveSelectionDown);

  upk(mnuCaseUp, cCommand_TextCaseUpper);
  upk(mnuCaseLow, cCommand_TextCaseLower);
  upk(mnuCaseTitle, cCommand_TextCaseTitle);
  upk(mnuCaseInvert, cCommand_TextCaseInvert);
  upk(mnuCaseSent, cCommand_TextCaseSentence);

  upk(mnuSortAsc, cmd_SortAsc);
  upk(mnuSortDesc, cmd_SortDesc);

  upk(mnuCmtAdd, cmd_CommentLineAdd_AtNonspace);
  upk(mnuCmtRemove, cmd_CommentLineRemove);
  upk(mnuCmtToggle, cmd_CommentLineToggle_AtNonspace);
  upk(mnuCmtToggleStr, cmd_CommentStreamToggle);

  upk(mnuFindDlg, cmd_DlgFind);
  upk(mnuFindRepDialog, cmd_DlgReplace);
  upk(mnuFindNext, cmd_FindNext);
  upk(mnuFindPrev, cmd_FindPrev);
  upk(mnuFindWordNext, cmd_FindCurWordNext);
  upk(mnuFindWordPrev, cmd_FindCurWordPrev);

  upk(mnuGotoLine, cmd_DlgGoto);
  upk(mnuGotoTab, cmd_DlgTabs);
  upk(mnuGotoBm, cmd_DlgGotoBm);

  upk(mnuBmNext, cmd_BookmarkGotoNext);
  upk(mnuBmPrev, cmd_BookmarkGotoPrev);
  upk(mnuBmToggle, cmd_BookmarkToggle);
  upk(mnuBmInvert, cmd_BookmarkInvertAll);
  upk(mnuBmClear, cmd_BookmarkClearAll);

  upk(mnuGroupsOne, cmd_Groups1);
  upk(mnuGroups2Vert, cmd_Groups2horz);
  upk(mnuGroups2Horz, cmd_Groups2vert);
  upk(mnuGroups3Vert, cmd_Groups3horz);
  upk(mnuGroups3Horz, cmd_Groups3vert);
  upk(mnuGroups3as12, cmd_Groups3plus);
  upk(mnuGroups4Vert, cmd_Groups4horz);
  upk(mnuGroups4Horz, cmd_Groups4vert);
  upk(mnuGroups4Grid, cmd_Groups4grid);
  upk(mnuGroups6Grid, cmd_Groups6grid);

  upk(mnuViewSplitDo, cmd_SplitTabToggle);
  upk(mnuViewSplitHorz, cmd_SplitTabHorzVert);

  upk(mnuViewWrap, cCommand_ToggleWordWrap);
  upk(mnuViewNums, cCommand_ToggleLineNums);
  upk(mnuViewFold, cCommand_ToggleFolding);
  upk(mnuViewRuler, cCommand_ToggleRuler);
  upk(mnuViewMinimap, cCommand_ToggleMinimap);

  upk(mnuViewUnpriShow, cCommand_ToggleUnprinted);
  upk(mnuViewUnpriSpaces, cCommand_ToggleUnprintedSpaces);
  upk(mnuViewUnpriEnds, cCommand_ToggleUnprintedEnds);
  upk(mnuViewUnpriEndsDet, cCommand_ToggleUnprintedEndDetails);

  upk(mnuViewFullscr, cmd_ToggleFullScreen);
  upk(mnuViewSide, cmd_ToggleSidePanel);
  upk(mnuViewBottom, cmd_ToggleBottomPanel);

  upk(mnuOpDefault, cmd_OpsOpenDefault);
  upk(mnuOpUser, cmd_OpsOpenUser);
  upk(mnuOpLexer, cmd_OpsOpenLexerOvr);
  upk(mnuOpFileTypes, cmd_OpsOpenFileTypes);
  upk(mnuFontText, cmd_OpsFontText);
  upk(mnuFontUi, cmd_OpsFontUi);
  upk(mnuOpLexProp, cmd_DlgLexerProp);
  upk(mnuOpLexLib, cmd_DlgLexerLib);
  upk(mnuOpColors, cmd_DlgColors);

  upk(mnuHelpCmd, cmd_DlgCommands);

end;

procedure TfmMain.UpdateEditorTabsize(N: integer);
begin
  case N of
    -1: CurrentEditor.OptTabSpaces:= true;
    -2: CurrentEditor.OptTabSpaces:= false;
    1..20: CurrentEditor.OptTabSize:= N;
  end;

  UpdateFrame;
  UpdateStatus;
end;

procedure TfmMain.UpdateKeymapDynamicItems;
var
  i: integer;
  sl: tstringlist;
begin
  //del all dynamic items
  while Keymap[Keymap.Count-1].Command>=cmdFirstLexerCommand do
    Keymap.Delete(Keymap.Count-1);

  //dynamic lexer items
  sl:= TStringList.Create;
  try
    for i:= 0 to Manager.AnalyzerCount-1 do
      sl.AddObject('lexer: '+Manager.Analyzers[i].LexerName, TObject(cmdFirstLexerCommand+i));
    sl.Sort;
    for i:= 0 to sl.count-1 do
      Keymap.Add(PtrInt(sl.Objects[i]), sl[i], [], []);
  finally
    FreeAndNil(sl);
  end;

  //dynamic plugin items
  for i:= 0 to High(FPluginsCmd) do
  begin
    if FPluginsCmd[i].ItemModule='' then Break;
    if SEndsWith(FPluginsCmd[i].ItemCaption, '-') then Continue;
    Keymap.Add(
      cmdFirstPluginCommand+i,
      'plugin: '+StringReplace(FPluginsCmd[i].ItemCaption, '\', ' - ', [rfReplaceAll]),
      [], []);
  end;
end;


procedure TfmMain.UpdateMenuThemes(sub: TMenuItem);
var
  mi: tmenuitem;
  cap: string;
  i: integer;
begin
  sub.Clear;

  mi:= tmenuitem.create(Self);
  mi.caption:= msgDefTheme;
  mi.OnClick:= @MenuThemeDefClick;
  mi.Checked:= FThemeName='';
  sub.Add(mi);

  FFindFilesInDir(GetAppPath(cDirDataThemes), '*.json', FListThemes);
  if FListThemes.count=0 then exit;
  FListThemes.Sort;

  for i:= 0 to FListThemes.count-1 do
  begin
    cap:= LowerCase(ExtractFileNameOnly(FListThemes[i]));
    if sub.IndexOfCaption(cap)>=0 then Continue; //for Qt
    mi:= tmenuitem.create(Self);
    mi.caption:= cap;
    mi.tag:= i;
    mi.OnClick:= @MenuThemesClick;
    mi.Checked:= LowerCase(FThemeName)=LowerCase(mi.Caption);
    sub.Add(mi);
  end;
end;

procedure TfmMain.DoApplyTheme;
var
  F: TEditorFrame;
  i: integer;
begin
  UpdateMenuThemes(mnuThemes);

  ListboxOut.Color:= GetAppColor('ListBg');
  ListboxOut.Invalidate;
  ListboxVal.Color:= ListboxOut.Color;
  ListboxVal.Invalidate;

  for i:= 0 to 9 do
    AppBookmarkSetup[240+i].Color:= GetAppColor('EdBookmarkBg');

  fmConsole.ed.Colors.TextFont:= GetAppColor('EdTextFont');
  fmConsole.ed.Colors.TextBG:= GetAppColor('EdTextBg');
  fmConsole.ed.Colors.TextSelFont:= GetAppColor('EdSelFont');
  fmConsole.ed.Colors.TextSelBG:= GetAppColor('EdSelBg');
  fmConsole.ed.Colors.ComboboxArrow:= GetAppColor('EdComboArrow');
  fmConsole.ed.Colors.ComboboxArrowBG:= GetAppColor('EdComboArrowBg');
  fmConsole.ed.Update;

  fmConsole.memo.Colors.TextFont:= GetAppColor('EdTextFont');
  fmConsole.memo.Colors.TextBG:= GetAppColor('EdTextBg');
  fmConsole.memo.Colors.TextSelFont:= GetAppColor('EdSelFont');
  fmConsole.memo.Colors.TextSelBG:= GetAppColor('EdSelBg');
  fmConsole.memo.Update;

  ToolbarMain.Color:= GetAppColor('TabBg');
  Status.Font.Color:= GetAppColor('TabFont');
  Status.Color:= GetAppColor('TabBg');
  Status.ColorBorderTop:= GetAppColor('TabBorderPassive');
  Status.ColorBorderR:= GetAppColor('TabBorderPassive');
  Gauge.Font.Color:= GetAppColor('TabFont');
  Gauge.ForeColor:= GetAppColor('GaugeFill');
  Gauge.BackColor:= GetAppColor('GaugeBg');
  Gauge.BorderColor:= GetAppColor('ButtonBorderPassive');

  ATButtonTheme.ColorFont:= GetAppColor('ButtonFont');
  ATButtonTheme.ColorFontDisabled:= GetAppColor('ButtonFontDisabled');
  ATButtonTheme.ColorBgPassive:= GetAppColor('ButtonBgPassive');
  ATButtonTheme.ColorBgOver:= GetAppColor('ButtonBgOver');
  ATButtonTheme.ColorBgChecked:= GetAppColor('ButtonBgChecked');
  ATButtonTheme.ColorBgDisabled:= GetAppColor('ButtonBgDisabled');
  ATButtonTheme.ColorBorderPassive:= GetAppColor('ButtonBorderPassive');
  ATButtonTheme.ColorBorderOver:= GetAppColor('ButtonBorderOver');
  ATButtonTheme.ColorBorderFocused:= GetAppColor('ButtonBorderFocused');

  Groups.SetTabOption(tabColorText, GetAppColor('TabFont'));
  Groups.SetTabOption(tabColorTextModified, GetAppColor('TabFontMod'));
  Groups.SetTabOption(tabColorBg, GetAppColor('TabBg'));
  Groups.SetTabOption(tabColorBgActive, GetAppColor('TabActive'));
  Groups.SetTabOption(tabColorBgPassive, GetAppColor('TabPassive'));
  Groups.SetTabOption(tabColorBgPassiveOver, GetAppColor('TabOver'));
  Groups.SetTabOption(tabColorBorderActive, GetAppColor('TabBorderActive'));
  Groups.SetTabOption(tabColorBorderPassive, GetAppColor('TabBorderPassive'));
  Groups.SetTabOption(tabColorArrow, GetAppColor('TabArrow'));
  Groups.SetTabOption(tabColorArrowOver, GetAppColor('TabArrowOver'));
  Groups.SetTabOption(tabColorCloseBg, GetAppColor('TabCloseBg'));
  Groups.SetTabOption(tabColorCloseBgOver, GetAppColor('TabCloseBgOver'));
  Groups.SetTabOption(tabColorCloseBorderOver, GetAppColor('TabCloseBorderOver'));
  Groups.SetTabOption(tabColorCloseX, GetAppColor('TabCloseX'));
  Groups.SetTabOption(tabColorCloseXOver, GetAppColor('TabCloseXOver'));
  Groups.Invalidate;

  TabsBottom.Font.Color:= GetAppColor('TabFont');
  TabsBottom.ColorBg:= GetAppColor('TabBg');
  TabsBottom.ColorTabActive:= GetAppColor('TabActive');
  TabsBottom.ColorTabPassive:= GetAppColor('TabPassive');
  TabsBottom.ColorTabOver:= GetAppColor('TabOver');
  TabsBottom.ColorBorderActive:= GetAppColor('TabBorderActive');
  TabsBottom.ColorBorderPassive:= GetAppColor('TabBorderPassive');
  TabsBottom.Invalidate;

  if assigned(fmGoto) then
  begin
    fmGoto.Color:= GetAppColor('TabBg');
    fmGoto.LabelGoto.Font.Color:= GetAppColor('TabFont');
    fmGoto.edInput.Colors.TextFont:= GetAppColor('EdTextFont');
    fmGoto.edInput.Colors.TextBG:= GetAppColor('EdTextBg');
    fmGoto.edInput.Colors.TextSelFont:= GetAppColor('EdSelFont');
    fmGoto.edInput.Colors.TextSelBG:= GetAppColor('EdSelBg');
    fmGoto.edInput.Update;
    fmGoto.Invalidate;
  end;

  if assigned(fmFind) then
  begin
    fmFind.Color:= GetAppColor('TabBg');
    fmFind.LabelFind.Font.Color:= GetAppColor('ListFont');
    fmFind.edFind.Colors.TextFont:= GetAppColor('EdTextFont');
    fmFind.edFind.Colors.TextBG:= GetAppColor('EdTextBg');
    fmFind.edFind.Colors.TextSelFont:= GetAppColor('EdSelFont');
    fmFind.edFind.Colors.TextSelBG:= GetAppColor('EdSelBg');
    fmFind.edFind.Colors.ComboboxArrow:= GetAppColor('EdComboArrow');
    fmFind.edFind.Colors.ComboboxArrowBG:= GetAppColor('EdComboArrowBg');
    fmFind.edFind.Update;
    fmFind.edRep.Colors.TextFont:= GetAppColor('EdTextFont');
    fmFind.edRep.Colors.TextBG:= GetAppColor('EdTextBg');
    fmFind.edRep.Colors.TextSelFont:= GetAppColor('EdSelFont');
    fmFind.edRep.Colors.TextSelBG:= GetAppColor('EdSelBg');
    fmFind.edRep.Colors.TextDisabledFont:= GetAppColor('EdDisableFont');
    fmFind.edRep.Colors.TextDisabledBG:= GetAppColor('EdDisableBg');
    fmFind.edRep.Colors.ComboboxArrow:= GetAppColor('EdComboArrow');
    fmFind.edRep.Colors.ComboboxArrowBG:= GetAppColor('EdComboArrowBg');
    fmFind.edRep.Update;
    fmFind.Invalidate;
  end;

  Tree.Font.Color:=GetAppColor('TreeFont');
  Tree.BackgroundColor:= GetAppColor('TreeBg');
  Tree.SelectionFontColor:= GetAppColor('TreeSelFont'); //lew Laz
  Tree.SelectionFontColorUsed:= true; //new Laz
  Tree.SelectionColor:= GetAppColor('TreeSelBg');
  Tree.TreeLineColor:= GetAppColor('TreeLines');
  Tree.ExpandSignColor:= GetAppColor('TreeSign');
  Tree.Invalidate;

  for i:= 0 to FrameCount-1 do
  begin
    F:= Frames[i];
    EditorApplyTheme(F.Editor);
    EditorApplyTheme(F.Editor2);
    F.Editor.Update;
    F.Editor2.Update;
  end;

  SplitterHorz.Invalidate;
  SplitterVert.Invalidate;

  cCompleteColorBg:= GetAppColor('ListBg');
  cCompleteColorSelBg:= GetAppColor('ListSelBg');
  cCompleteColorFont[0]:= GetAppColor('ListCompletePrefix');
  cCompleteColorFont[1]:= GetAppColor('ListFont');
  cCompleteColorFont[2]:= GetAppColor('ListCompleteParams');
end;


procedure TfmMain.UpdateMenuChecked;
var
  F: TEditorFrame;
  Ed: TATSynEdit;
begin
  F:= CurrentFrame;
  Ed:= CurrentEditor;
  if F=nil then exit;
  if Ed=nil then exit;

  mnuViewWrap.Checked:= Ed.OptWrapMode<>cWrapOff;
  mnuViewNums.Checked:= Ed.Gutter.Items[Ed.GutterBandNum].Visible;
  mnuViewFold.Checked:= Ed.Gutter.Items[Ed.GutterBandFold].Visible;
  mnuViewRuler.Checked:= Ed.OptRulerVisible;
  mnuViewMinimap.Checked:= Ed.OptMinimapVisible;

  mnuViewSplitDo.Checked:= F.Splitted;
  mnuViewSplitHorz.Checked:= F.SplitHorz;

  mnuViewUnpriShow.Checked:= Ed.OptUnprintedVisible;
  mnuViewUnpriSpaces.Checked:= Ed.OptUnprintedSpaces;
  mnuViewUnpriEnds.Checked:= Ed.OptUnprintedEnds;
  mnuViewUnpriEndsDet.Checked:= Ed.OptUnprintedEndsDetails;

  mnuViewToolbar.Checked:= ShowToolbar;
  mnuViewStatus.Checked:= ShowStatus;
  mnuViewFullscr.Checked:= ShowFullscreen;
  mnuViewSide.Checked:= ShowSidePanel;
  mnuViewBottom.Checked:= ShowBottom;

  mnuGroupsOne.Checked:= Groups.Mode= gmOne;
  mnuGroups2Horz.Checked:= Groups.Mode= gm2Vert;
  mnuGroups2Vert.Checked:= Groups.Mode= gm2Horz;
  mnuGroups3Horz.Checked:= Groups.Mode= gm3Vert;
  mnuGroups3Vert.Checked:= Groups.Mode= gm3Horz;
  mnuGroups3as12.Checked:= Groups.Mode= gm3Plus;
  mnuGroups4Horz.Checked:= Groups.Mode= gm4Vert;
  mnuGroups4Vert.Checked:= Groups.Mode= gm4Horz;
  mnuGroups4Grid.Checked:= Groups.Mode= gm4Grid;
  mnuGroups6Grid.Checked:= Groups.Mode= gm6Grid;
end;

procedure TfmMain.UpdateTree(AFill: boolean);
var
  Caret: TATCaretItem;
begin
  if not ShowSidePanel then exit;

  Caret:= CurrentEditor.Carets[0];
  with CurrentFrame do
  begin
    while Adapter.TreeBusy do Application.ProcessMessages;
    if AFill then
    begin
      //BeginUpdate/EndUpdate is to fix tree issue with incorrect vert-scrollbar,
      //also need to change Scrollbars and restore it
      Tree.BeginUpdate;
      try
        Adapter.TreeFill(Tree);
      finally
        Tree.Scrollbars:= ssNone;
        Tree.Scrollbars:= ssVertical;
        Tree.EndUpdate;
      end;
    end;
    if UiOps.TreeAutoSync then
      Adapter.TreeShowItemForCaret(Tree, Point(Caret.PosX, Caret.PosY));
  end;
end;

