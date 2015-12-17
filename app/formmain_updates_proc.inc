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
    if F.NotInRecents then exit;
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
    DoApplyFrameOps(F, EditorOps, false);
    DoOps_LoadOptionsOverride(F);
  end;

  UpdateStatus;
end;

procedure TfmMain.DoApplyFrameOps(F: TEditorFrame;
  const Op: TEditorOps; AForceApply: boolean);
begin
  EditorApplyOps(F.Editor, Op, AForceApply);
  EditorApplyOps(F.Editor2, Op, AForceApply);
  F.Adapter.DynamicHiliteEnabled:= Op.OpDynHilite;

  F.Editor.Update;
  if F.Splitted then
    F.Editor2.Update;

  UpdateStatus;
end;

procedure TfmMain.UpKey(mi: TMenuItem; cmd: integer);
begin
  if not Assigned(mi) then exit;
  mi.Tag:= cmd;
  mi.OnClick:= @MenuMainClick;
  mi.ShortCut:= Keymap.GetShortcutFromCommand(cmd);
end;

procedure TfmMain.UpdateMenuHotkeys;
begin
  UpKey(mnuFileNew, cmd_FileNew);
  UpKey(mnuFileOpen, cmd_FileOpen);
  UpKey(mnuFileReopen, cmd_FileReopen);
  UpKey(mnuFileSave, cmd_FileSave);
  UpKey(mnuFileSaveAs, cmd_FileSaveAs);
  UpKey(mnuFileSaveAll, cmd_FileSaveAll);
  UpKey(mnuFileClose, cmd_FileClose);
  UpKey(mnuFileCloseAll, cmd_FileCloseAll);
  UpKey(mnuFileCloseOther, cmd_FileCloseOtherThis);
  UpKey(mnuFileCloseDel, cmd_FileCloseAndDelete);
  UpKey(mnuFileExit, cmd_FileExit);
  UpKey(mnuFileHtml, cmd_FileExportHtml);

  UpKey(mnuSelAll, cCommand_SelectAll);
  UpKey(mnuSelSplit, cCommand_SelectSplitToLines);
  UpKey(mnuSelExtLine, cCommand_SelectExtendByLine);
  UpKey(mnuSelInvert, cCommand_SelectInverted);

  UpKey(mnuCaretsCancel, cCommand_Cancel);
  UpKey(mnuCaretsUp1Line, cCommand_CaretsExtendUpLine);
  UpKey(mnuCaretsUp1Page, cCommand_CaretsExtendUpPage);
  UpKey(mnuCaretsUpBegin, cCommand_CaretsExtendUpToTop);
  UpKey(mnuCaretsDown1Line, cCommand_CaretsExtendDownLine);
  UpKey(mnuCaretsDown1Page, cCommand_CaretsExtendDownPage);
  UpKey(mnuCaretsDownEnd, cCommand_CaretsExtendDownToEnd);

  UpKey(mnuEditUndo, cCommand_Undo);
  UpKey(mnuEditRedo, cCommand_Redo);
  UpKey(mnuEditCut, cCommand_ClipboardCut);
  UpKey(mnuEditCopy, cCommand_ClipboardCopy);
  UpKey(mnuEditPaste, cCommand_ClipboardPaste);
  UpKey(mnuEditDel, cCommand_TextDeleteSelection);

  UpKey(mnuEditCopyLine, cmd_CopyLine);
  UpKey(mnuEditCopyAppend, cCommand_ClipboardCopyAdd);
  UpKey(mnuEditCopyFFull, cmd_CopyFilenameFull);
  UpKey(mnuEditCopyFDir, cmd_CopyFilenameDir);
  UpKey(mnuEditCopyFName, cmd_CopyFilenameName);

  UpKey(mnuEditIndent, cCommand_TextIndent);
  UpKey(mnuEditUnindent, cCommand_TextUnindent);
  UpKey(mnuEditTrim, cCommand_TextTrimSpacesAll);
  UpKey(mnuEditTrimL, cCommand_TextTrimSpacesLeft);
  UpKey(mnuEditTrimR, cCommand_TextTrimSpacesRight);

  UpKey(mnuEditTabToSp, cmd_ConvertTabsToSpaces);
  UpKey(mnuEditSpToTab, cmd_ConvertSpacesToTabsLeading);

  UpKey(mnuEditLineDel, cCommand_TextDeleteLine);
  UpKey(mnuEditLineDup, cCommand_TextDuplicateLine);
  UpKey(mnuEditLineMoveUp, cCommand_MoveSelectionUp);
  UpKey(mnuEditLineMoveDown, cCommand_MoveSelectionDown);

  UpKey(mnuCaseUp, cCommand_TextCaseUpper);
  UpKey(mnuCaseLow, cCommand_TextCaseLower);
  UpKey(mnuCaseTitle, cCommand_TextCaseTitle);
  UpKey(mnuCaseInvert, cCommand_TextCaseInvert);
  UpKey(mnuCaseSent, cCommand_TextCaseSentence);

  UpKey(mnuSortAsc, cmd_SortAsc);
  UpKey(mnuSortDesc, cmd_SortDesc);

  UpKey(mnuCmtAdd, cmd_CommentLineAdd_AtNonspace);
  UpKey(mnuCmtRemove, cmd_CommentLineRemove);
  UpKey(mnuCmtToggle, cmd_CommentLineToggle_AtNonspace);
  UpKey(mnuCmtToggleStr, cmd_CommentStreamToggle);

  UpKey(mnuEditCharmap, cmd_DialogCharMap);

  UpKey(mnuFindDlg, cmd_DialogFind);
  UpKey(mnuFindRepDialog, cmd_DialogReplace);
  UpKey(mnuFindNext, cmd_FindNext);
  UpKey(mnuFindPrev, cmd_FindPrev);
  UpKey(mnuFindWordNext, cmd_FindCurWordNext);
  UpKey(mnuFindWordPrev, cmd_FindCurWordPrev);

  UpKey(mnuGotoLine, cmd_DialogGoto);
  UpKey(mnuGotoBm, cmd_DialogGotoBookmark);

  UpKey(mnuBmNext, cmd_BookmarkGotoNext);
  UpKey(mnuBmPrev, cmd_BookmarkGotoPrev);
  UpKey(mnuBmToggle, cmd_BookmarkToggle);
  UpKey(mnuBmInvert, cmd_BookmarkInvertAll);
  UpKey(mnuBmClear, cmd_BookmarkClearAll);

  UpKey(mnuGroupsOne, cmd_Groups1);
  UpKey(mnuGroups2Vert, cmd_Groups2horz);
  UpKey(mnuGroups2Horz, cmd_Groups2vert);
  UpKey(mnuGroups3Vert, cmd_Groups3horz);
  UpKey(mnuGroups3Horz, cmd_Groups3vert);
  UpKey(mnuGroups3as12, cmd_Groups3plus);
  UpKey(mnuGroups4Vert, cmd_Groups4horz);
  UpKey(mnuGroups4Horz, cmd_Groups4vert);
  UpKey(mnuGroups4Grid, cmd_Groups4grid);
  UpKey(mnuGroups6Grid, cmd_Groups6grid);

  UpKey(mnuViewSplitDo, cmd_SplitTabToggle);
  UpKey(mnuViewSplitHorz, cmd_SplitTabHorzVert);

  UpKey(mnuViewWrap, cCommand_ToggleWordWrap);
  UpKey(mnuViewNums, cCommand_ToggleLineNums);
  UpKey(mnuViewFold, cCommand_ToggleFolding);
  UpKey(mnuViewRuler, cCommand_ToggleRuler);
  UpKey(mnuViewMinimap, cCommand_ToggleMinimap);

  UpKey(mnuViewUnpriShow, cCommand_ToggleUnprinted);
  UpKey(mnuViewUnpriSpaces, cCommand_ToggleUnprintedSpaces);
  UpKey(mnuViewUnpriEnds, cCommand_ToggleUnprintedEnds);
  UpKey(mnuViewUnpriEndsDet, cCommand_ToggleUnprintedEndDetails);

  UpKey(mnuViewFullscr, cmd_ToggleFullScreen);
  UpKey(mnuViewSide, cmd_ToggleSidePanel);
  UpKey(mnuViewBottom, cmd_ToggleBottomPanel);
  UpKey(mnuViewToolbar, cmd_ToggleToolbar);
  UpKey(mnuViewStatus, cmd_ToggleStatusbar);

  UpKey(mnuOpDefault, cmd_OpsOpenDefault);
  UpKey(mnuOpUser, cmd_OpsOpenUser);
  UpKey(mnuOpLexer, cmd_OpsOpenLexerOvr);
  UpKey(mnuOpFileTypes, cmd_OpsOpenFileTypes);
  UpKey(mnuFontText, cmd_OpsFontText);
  UpKey(mnuFontUi, cmd_OpsFontUi);
  UpKey(mnuOpLexProp, cmd_DialogLexerProp);
  UpKey(mnuOpLexLib, cmd_DialogLexerLib);
  UpKey(mnuOpColors, cmd_DialogColors);

  UpKey(mnuHelpCmd, cmd_DialogCommands);
  UpKey(mnuHelpForum, cmd_HelpForum);
  UpKey(mnuHelpWiki, cmd_HelpWiki);
  UpKey(mnuHelpMouse, cmd_HelpMouse);
  UpKey(mnuHelpLexers, cmd_HelpLexers);
  UpKey(mnuHelpChangelog, cmd_HelpChangelog);
  UpKey(mnuHelpAbout, cmd_HelpAbout);

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
  if sub=nil then exit;
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


procedure TfmMain.UpdateMenuEnc(AMenu: TMenuItem);
var
  cList: array[0..28] of record Sub, Name: string end = (
    (Sub: ''; Name: cEncNameAnsi),
    (Sub: ''; Name: cEncNameUtf8),
    (Sub: ''; Name: cEncNameUtf8NoBom),
    (Sub: ''; Name: cEncNameUtf16LE),
    (Sub: ''; Name: cEncNameUtf16BE),
    (Sub: ''; Name: '-'),
    (Sub: 'European'; Name: cEncNameCP1250),
    (Sub: 'European'; Name: cEncNameCP1251),
    (Sub: 'European'; Name: cEncNameCP1252),
    (Sub: 'European'; Name: cEncNameCP1253),
    (Sub: 'European'; Name: cEncNameCP1257),
    (Sub: 'European'; Name: '-'),
    (Sub: 'European'; Name: cEncNameCP437),
    (Sub: 'European'; Name: cEncNameCP850),
    (Sub: 'European'; Name: cEncNameCP852),
    (Sub: 'European'; Name: cEncNameCP866),
    (Sub: 'European'; Name: '-'),
    (Sub: 'European'; Name: cEncNameISO1),
    (Sub: 'European'; Name: cEncNameISO2),
    (Sub: 'European'; Name: cEncNameMac),
    (Sub: 'Misc'; Name: cEncNameCP1254),
    (Sub: 'Misc'; Name: cEncNameCP1255),
    (Sub: 'Misc'; Name: cEncNameCP1256),
    (Sub: 'Asian'; Name: cEncNameCP874),
    (Sub: 'Asian'; Name: cEncNameCP932),
    (Sub: 'Asian'; Name: cEncNameCP936),
    (Sub: 'Asian'; Name: cEncNameCP949),
    (Sub: 'Asian'; Name: cEncNameCP950),
    (Sub: 'Asian'; Name: cEncNameCP1258)
  );
  //
  procedure DoAdd(AMenu: TMenuItem; const Sub, SName: string; AReloadFile: boolean);
  var
    mi, miSub: TMenuItem;
    n: integer;
  begin
    miSub:= nil;

    if Sub<>'' then
    begin
      n:= AMenu.IndexOfCaption(Sub);
      if n<0 then
      begin
        mi:= TMenuItem.Create(Self);
        mi.Caption:= Sub;
        AMenu.Add(mi);
        n:= AMenu.IndexOfCaption(Sub);
      end;
      miSub:= AMenu.Items[n]
    end;

    if miSub=nil then miSub:= AMenu;
    mi:= TMenuItem.Create(Self);
    mi.Caption:= SName;
    if AReloadFile then
      mi.OnClick:= @MenuEncWithReloadClick
    else
      mi.OnClick:= @MenuEncNoReloadClick;

    miSub.Add(mi);
  end;
  //
var
  miReload, miNoReload: TMenuItem;
  i: integer;
begin
  AMenu.Clear;

  miReload:= TMenuItem.Create(Self);
  miReload.Caption:= 'Reload as';
  miNoReload:= TMenuItem.Create(Self);
  miNoReload.Caption:= 'Convert to';
  AMenu.Add(miReload);
  AMenu.Add(miNoReload);

  for i:= Low(cList) to High(cList) do
  begin
    DoAdd(miReload, cList[i].Sub, cList[i].Name, true);
    DoAdd(miNoReload, cList[i].Sub, cList[i].Name, false);
  end;
end;


