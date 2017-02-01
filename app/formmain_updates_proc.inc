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
    if F.IsText then
      SName:= F.FileName
    else
      SName:= F.PictureFileName;
    if SName='' then
      SName:= F.TabCaption;
  end
  else
    SName:= F.TabCaption;

  SMark:= IfThen(F.Editor.Modified, '*');

  SSession:= ChangeFileExt(ExtractFileName(FSessionName), '');
  if (SSession<>'') and (SSession<>'history session') then
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
  Size: TPoint;
begin
  F:= CurrentFrame;
  Ed:= CurrentEditor;

  if not F.IsText then
  begin
    Size:= F.PictureSizes;
    Status[cStatusCaret]:= Format(msgStatusPictureNxN, [Size.X, Size.Y]);
    Status[cStatusEnc]:= '';
    Status[cStatusEnds]:= '';
    Status[cStatusLexer]:= '';
    Status[cStatusTabsize]:= '';
    Status[cStatusInsOvr]:= '';
    exit;
  end;

  //------
  case EditorGetStatusType(Ed) of
    selSmall:   fmt:= UiOps.StatusSmallSel;
    selStream:  fmt:= UiOps.StatusStreamSel;
    selCol:     fmt:= UiOps.StatusColSel;
    selCarets:  fmt:= UiOps.StatusCarets;
    else        fmt:= UiOps.StatusNoSel;
  end;
  Status[cStatusCaret]:= EditorFormatStatus(Ed, fmt);

  //------
  S:= F.EncodingName;
  Status[cStatusEnc]:= S;

  //------
  case F.LineEnds of
    cEndWin: S:= msgEndWin;
    cEndUnix: S:= msgEndUnix;
    cEndMac: S:= msgEndMac;
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
  Status[cStatusTabsize]:= EditorFormatTabsize(Ed, UiOps.StatusTabsize);

  //------
  S:= IfThen(Ed.ModeOverwrite, 'Ovr', 'Ins');
  Status[cStatusInsOvr]:= S;
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
  with ButtonCancel do
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
  ButtonCancel.Hide;
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

procedure TfmMain.UpdateInputForm(Form: TForm);
var
  Ed: TATSynEdit;
  P: TPoint;
begin
  Ed:= CurrentEditor;
  P:= Ed.ClientToScreen(Point(0, 0));
  Form.Left:= Max(0, P.X+(Ed.Width-Form.Width) div 2);
  Form.Top:= Max(0, P.Y);
  Form.Height:= UiOps.ListboxSizeY;
end;


procedure TfmMain.UpdateEnabledAll(b: boolean);
begin
  Groups.Enabled:= b;
  if Assigned(fmFind) then
    fmFind.Enabled:= b;
end;

procedure TfmMain.UpdateAppForSearch(AStart: boolean; AEdLock: boolean);
var
  Ed: TATSynEdit;
begin
  Ed:= CurrentEditor;
  if AStart then
  begin
    FFinder.Editor:= Ed;
    FFindStop:= false;
    FFindConfirmAll:= mrNone;
    ButtonCancel.Show;
    Gauge.Show;
    Gauge.Progress:= 0;
    UpdateEnabledAll(false);
    if AEdLock then
    begin
      Ed.BeginUpdate;
      Ed.Enabled:= false;
    end;
  end
  else
  begin
    ButtonCancel.Hide;
    Gauge.Hide;
    UpdateEnabledAll(true);
    Ed.Enabled:= true;
    Ed.EndUpdate;

    if Assigned(fmFind) and fmFind.Visible then
      fmFind.edFind.SetFocus
    else
      EditorFocus(Ed);
  end;

  Ed.DoEventCarets;
end;


procedure TfmMain.DoApplyFont_Text;
var
  F: TEditorFrame;
  i: integer;
begin
  fmConsole.Font.Name:= EditorOps.OpFontName;
  fmConsole.Font.Size:= EditorOps.OpFontSize;
  fmConsole.Font.Quality:= EditorOps.OpFontQuality;
  fmConsole.ed.Font:= fmConsole.Font;
  fmConsole.memo.Font:= fmConsole.Font;

  for i:= 0 to FrameCount-1 do
  begin
    F:= Frames[i];
    F.Editor.Font.Name:= EditorOps.OpFontName;
    F.Editor2.Font.Name:= EditorOps.OpFontName;
    F.Editor.Font.Size:= EditorOps.OpFontSize;
    F.Editor2.Font.Size:= EditorOps.OpFontSize;
    F.Editor.Font.Quality:= EditorOps.OpFontQuality;
    F.Editor2.Font.Quality:= EditorOps.OpFontQuality;
    DoOps_LoadOptionsOverride(F);
  end;

  if Assigned(fmFind) then fmFind.UpdateFonts;
  if Assigned(fmGoto) then fmGoto.UpdateFonts;
end;

procedure TfmMain.DoApplyFont_Ui;
begin
  Self.Font.Name:= UiOps.VarFontName;
  Self.Font.Size:= UiOps.VarFontSize;
  Self.Font.Color:= GetAppColor('TabFont');

  Groups.SetTabFont(Self.Font);
  TabsBottom.Font:= Self.Font;
  TabsLeft.Font:= Self.Font;
  Status.Font:= Self.Font;
  StatusAlt.Font:= Self.Font;
  Gauge.Font:= Self.Font;
  Tree.Font:= Self.Font;

  ATButtonTheme.FontName:= UiOps.VarFontName;
  ATButtonTheme.FontSize:= UiOps.VarFontSize;

  if Assigned(fmFind) then fmFind.UpdateFonts;
  if Assigned(fmGoto) then fmGoto.UpdateFonts;

  cCompleteFontName:= UiOps.VarFontName;
  cCompleteFontSize:= UiOps.VarFontSize;
  cCompleteItemHeight:= GetListboxItemHeight(UiOps.VarFontName, UiOps.VarFontSize);
end;

procedure TfmMain.DoApplyFont_Output;
begin
  ListboxOut.Font.Name:= UiOps.OutputFontName;
  ListboxOut.Font.Size:= UiOps.OutputFontSize;
  ListboxVal.Font.Name:= UiOps.OutputFontName;
  ListboxVal.Font.Size:= UiOps.OutputFontSize;
  ListboxOut.ItemHeight:= GetListboxItemHeight(UiOps.OutputFontName, UiOps.OutputFontSize);
  ListboxVal.ItemHeight:= ListboxOut.ItemHeight;
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
    if not F.IsText then exit;
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
  mi.Caption:= msgFileClearList;
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

procedure TfmMain.DoApplyAllOps;
var
  F: TEditorFrame;
  i: integer;
begin
  DoApplyFont_Text;
  DoApplyFont_Ui;
  DoApplyFont_Output;
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

  F.Adapter.DynamicHiliteEnabled:= Op.OpLexerDynamicHiliteEnabled;
  F.Adapter.DynamicHiliteMaxLines:= Op.OpLexerDynamicHiliteMaxLines;
  F.Adapter.EnabledLineSeparators:= Op.OpLexerLineSeparators;

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
  mi.ShortCut:= AppKeymap.GetShortcutFromCommand(cmd);

  //don't set Esc here: for OSX/ Qt, else Esc blocked in Find/ Goto/ closing app
  if mi.Shortcut=AppShortcutEscape then
    mi.ShortCut:= 0;
end;

procedure TfmMain.UpdateMenuHotkeys;
begin
  UpKey(mnuFileEndWin, cmd_LineEndWin);
  UpKey(mnuFileEndUn, cmd_LineEndUnix);
  UpKey(mnuFileEndMac, cmd_LineEndMac);

  UpKey(mnuEndsWin, cmd_LineEndWin);
  UpKey(mnuEndsUnix, cmd_LineEndUnix);
  UpKey(mnuEndsMac, cmd_LineEndMac);

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
  UpKey(mnuBmCarets, cmd_BookmarkPlaceCarets);
  UpKey(mnuBmClear, cmd_BookmarkClearAll);
  UpKey(mnuBmCopyLines, cmd_BookmarkCopyMarkedLines);
  UpKey(mnuBmDeleteLines, cmd_BookmarkDeleteMarkedLines);

  UpKey(mnuGr1, cmd_Groups1);
  UpKey(mnuGr2V, cmd_Groups2horz);
  UpKey(mnuGr2H, cmd_Groups2vert);
  UpKey(mnuGr3V, cmd_Groups3horz);
  UpKey(mnuGr3H, cmd_Groups3vert);
  UpKey(mnuGr1p2V, cmd_Groups3plus);
  UpKey(mnuGr1p2H, cmd_Groups3plushorz);
  UpKey(mnuGr4V, cmd_Groups4horz);
  UpKey(mnuGr4H, cmd_Groups4vert);
  UpKey(mnuGr4G, cmd_Groups4grid);
  UpKey(mnuGr6, cmd_Groups6grid);

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
  UpKey(mnuOpKeys, cmd_HelpHotkeys);

  UpKey(mnuFontText, cmd_OpsFontText);
  UpKey(mnuFontUi, cmd_OpsFontUi);
  UpKey(mnuFontOutput, cmd_OpsFontOutput);

  UpKey(mnuOpLexProp, cmd_DialogLexerProp);
  UpKey(mnuOpLexLib, cmd_DialogLexerLib);
  UpKey(mnuOpLexMap, cmd_DialogLexerStyleMap);
  UpKey(mnuOpThemeUi, cmd_DialogThemeUi);
  UpKey(mnuOpThemeSyntax, cmd_DialogThemeSyntax);

  UpKey(mnuHelpCmd, cmd_DialogCommands);
  UpKey(mnuHelpForum, cmd_HelpForum);
  UpKey(mnuHelpWiki, cmd_HelpWiki);
  UpKey(mnuHelpMouse, cmd_HelpMouse);
  UpKey(mnuHelpLexers, cmd_HelpLexers);
  UpKey(mnuHelpIssues, cmd_HelpIssues);
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
  sl: TStringList;
  str: string;
  i: integer;
begin
  //del all dynamic items
  while AppKeymap[AppKeymap.Count-1].Command>=cmdFirstLexerCommand do
    AppKeymap.Delete(AppKeymap.Count-1);

  //dynamic lexer items
  sl:= TStringList.Create;
  try
    for i:= 0 to AppManager.AnalyzerCount-1 do
      sl.AddObject('lexer: '+AppManager.Analyzers[i].LexerName, TObject(cmdFirstLexerCommand+i));
    sl.Sort;

    //insert "none" at list begin
    sl.InsertObject(0, 'lexer: '+msgNoLexer, TObject(cmdLastLexerCommand));

    for i:= 0 to sl.count-1 do
      AppKeymap.Add(PtrInt(sl.Objects[i]), sl[i], [], []);
  finally
    FreeAndNil(sl);
  end;

  //dynamic plugin items
  for i:= 0 to High(FPluginsCmd) do
  begin
    if FPluginsCmd[i].ItemModule='' then Break;
    if SEndsWith(FPluginsCmd[i].ItemCaption, '-') then Continue;

    Str:= FPluginsCmd[i].ItemCaption;
    Str:= StringReplace(Str, '\', ': ', [rfReplaceAll]);
    Str:= StringReplace(Str, '&', '', [rfReplaceAll]);

    AppKeymap.Add(
      cmdFirstPluginCommand+i,
      'plugin: '+Str,
      [], []);
  end;
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

  //menu cleared? check View and one item only..
  if mnuView=nil then exit;
  if mnuViewWrap=nil then exit;

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

  mnuGr1.Checked:= Groups.Mode= gmOne;
  mnuGr2H.Checked:= Groups.Mode= gm2Vert;
  mnuGr2V.Checked:= Groups.Mode= gm2Horz;
  mnuGr3H.Checked:= Groups.Mode= gm3Vert;
  mnuGr3V.Checked:= Groups.Mode= gm3Horz;
  mnuGr1p2V.Checked:= Groups.Mode= gm1plus2Vert;
  mnuGr1p2H.Checked:= Groups.Mode= gm1plus2Horz;
  mnuGr4H.Checked:= Groups.Mode= gm4Vert;
  mnuGr4V.Checked:= Groups.Mode= gm4Horz;
  mnuGr4G.Checked:= Groups.Mode= gm4Grid;
  mnuGr6.Checked:= Groups.Mode= gm6Grid;
end;


procedure TfmMain.UpdateTree(AFill: boolean; AConsiderTreeVisible: boolean;
  AForceUpdateAll: boolean);
var
  Frame: TEditorFrame;
  Ed: TATSynEdit;
  Caret: TATCaretItem;
begin
  if UiOps.TreeShowIcons then
    Tree.Images:= ImageListTree
  else
    Tree.Images:= nil;

  if AConsiderTreeVisible then
    if not ShowSidePanel then exit;

  Ed:= CurrentEditor;
  Frame:= GetEditorFrame(Ed);
  if Ed.Strings.Count=0 then exit;

  Caret:= Ed.Carets[0];
  with Frame do
  begin
    while Adapter.TreeBusy do
      Application.ProcessMessages;

    if AForceUpdateAll then
    begin
      Adapter.DoAnalize(Ed, true);
      Ed.DoEventChange;
    end;

    if AFill then
    begin
      Tree.BeginUpdate;
      try
        Adapter.TreeFill(Tree);
      finally
        Tree.EndUpdate;
      end;

      //force update scrollbar
      Tree.Perform(CM_CHANGED, 0, 0);
    end;

    if UiOps.TreeAutoSync then
      Adapter.TreeShowItemForCaret(Tree, Point(Caret.PosX, Caret.PosY));
  end;
end;


procedure TfmMain.UpdateMenuEnc(AMenu: TMenuItem);
var
  cList: array[0..30] of record Sub, Name: string end = (
    (Sub: ''; Name: cEncNameUtf8_NoBom),
    (Sub: ''; Name: cEncNameUtf8_WithBom),
    (Sub: ''; Name: cEncNameUtf16LE_NoBom),
    (Sub: ''; Name: cEncNameUtf16LE_WithBom),
    (Sub: ''; Name: cEncNameUtf16BE_NoBom),
    (Sub: ''; Name: cEncNameUtf16BE_WithBom),
    (Sub: ''; Name: cEncNameAnsi),
    (Sub: ''; Name: '-'),
    (Sub: 'eu'; Name: cEncNameCP1250),
    (Sub: 'eu'; Name: cEncNameCP1251),
    (Sub: 'eu'; Name: cEncNameCP1252),
    (Sub: 'eu'; Name: cEncNameCP1253),
    (Sub: 'eu'; Name: cEncNameCP1257),
    (Sub: 'eu'; Name: '-'),
    (Sub: 'eu'; Name: cEncNameCP437),
    (Sub: 'eu'; Name: cEncNameCP850),
    (Sub: 'eu'; Name: cEncNameCP852),
    (Sub: 'eu'; Name: cEncNameCP866),
    (Sub: 'eu'; Name: '-'),
    (Sub: 'eu'; Name: cEncNameISO1),
    (Sub: 'eu'; Name: cEncNameISO2),
    (Sub: 'eu'; Name: cEncNameMac),
    (Sub: 'mi'; Name: cEncNameCP1254),
    (Sub: 'mi'; Name: cEncNameCP1255),
    (Sub: 'mi'; Name: cEncNameCP1256),
    (Sub: 'as'; Name: cEncNameCP874),
    (Sub: 'as'; Name: cEncNameCP932),
    (Sub: 'as'; Name: cEncNameCP936),
    (Sub: 'as'; Name: cEncNameCP949),
    (Sub: 'as'; Name: cEncNameCP950),
    (Sub: 'as'; Name: cEncNameCP1258)
  );
  //
  procedure DoAdd(AMenu: TMenuItem; Sub, SName: string; AReloadFile: boolean);
  var
    mi, miSub: TMenuItem;
    n: integer;
  begin
    miSub:= nil;
    if Sub='eu' then Sub:= msgEncEuropean else
     if Sub='as' then Sub:= msgEncAsian else
      if Sub='mi' then Sub:= msgEncMisc;

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
  if AMenu=nil then exit;
  AMenu.Clear;

  miReload:= TMenuItem.Create(Self);
  miReload.Caption:= msgEncReloadAs;
  miReload.Hint:= 'enc_reload';
  miNoReload:= TMenuItem.Create(Self);
  miNoReload.Caption:= msgEncConvertTo;
  miNoReload.Hint:= 'enc_noreload';

  AMenu.Add(miReload);
  AMenu.Add(miNoReload);

  for i:= Low(cList) to High(cList) do
  begin
    DoAdd(miReload, cList[i].Sub, cList[i].Name, true);
    DoAdd(miNoReload, cList[i].Sub, cList[i].Name, false);
  end;
end;


procedure TfmMain.UpdateStatusbarPanelsFromString(AStr: string);
var
  SPanel, SItem: string;
  NIndex, NSize: integer;
  Al: TATStatusAlign;
begin
  Status.DeletePanels;
  cStatusCaret:= -1;
  cStatusEnc:= -1;
  cStatusEnds:= -1;
  cStatusLexer:= -1;
  cStatusTabsize:= -1;
  cStatusInsOvr:= -1;
  cStatusMsg:= -1;

  repeat
    SPanel:= SGetItem(AStr, '|');
    if SPanel='' then Break;

    NIndex:= Status.PanelCount;
    SItem:= SGetItem(SPanel);
    if SItem='caret' then cStatusCaret:= NIndex else
     if SItem='enc' then cStatusEnc:= NIndex else
      if SItem='ends' then cStatusEnds:= NIndex else
       if SItem='lexer' then cStatusLexer:= NIndex else
        if SItem='tabsize' then cStatusTabsize:= NIndex else
         if SItem='ins' then cStatusInsOvr:= NIndex else
          if SItem='msg' then cStatusMsg:= NIndex else
           Continue;

    SItem:= SGetItem(SPanel);
    if SItem='C' then Al:= saMiddle else
     if SItem='L' then Al:= saLeft else
      if SItem='R' then Al:= saRight else
       Al:= saMiddle;

    SItem:= SGetItem(SPanel);
    NSize:= Max(20, StrToIntDef(SItem, 200));

    Status.AddPanel(NSize, Al, '');
  until false;

  UpdateStatusbarPanelAutosize;
end;


procedure TfmMain.DoInvalidateEditors;
var
  F: TEditorFrame;
  i: integer;
begin
  for i:= 0 to FrameCount-1 do
  begin
    F:= Frames[i];
    if not F.Visible then Continue;
    F.Editor.Invalidate;
    F.Editor2.Invalidate;
    F.Splitter.Invalidate;
  end;

  Groups.Splitter1.Invalidate;
  Groups.Splitter2.Invalidate;
  Groups.Splitter3.Invalidate;
  Groups.Splitter4.Invalidate;
  Groups.Splitter5.Invalidate;
end;

procedure TfmMain.UpdateStatusbarPanelAutosize;
begin
  Status.DoPanelAutosize(cStatusMsg);
end;

procedure TfmMain.StatusResize(Sender: TObject);
begin
  UpdateStatusbarPanelAutosize;
end;


procedure TfmMain.UpdateMenuLangs(sub: TMenuItem);
var
  mi: tmenuitem;
  cap: string;
  i: integer;
begin
  if sub=nil then exit;
  sub.Clear;

  mi:= tmenuitem.create(Self);
  mi.Caption:= 'English (built-in)';
  mi.Tag:= -1;
  mi.OnClick:= @MenuLangClick;
  mi.Checked:= AppLangName='';
  sub.Add(mi);

  FFindFilesInDir(GetAppPath(cDirDataLangs), '*.ini', FListLangs);
  if FListLangs.count=0 then exit;
  FListLangs.Sort;

  for i:= 0 to FListLangs.count-1 do
  begin
    cap:= ExtractFileNameOnly(FListLangs[i]);
    if sub.IndexOfCaption(cap)>=0 then Continue; //for Qt
    mi:= TMenuItem.Create(Self);
    mi.Caption:= cap;
    mi.Tag:= i;
    mi.OnClick:= @MenuLangClick;
    mi.Checked:= LowerCase(AppLangName)=LowerCase(mi.Caption);
    sub.Add(mi);
  end;
end;


procedure TfmMain.InitToolbar;
begin
  Toolbar.AddButton(0, @tbNewClick, 'f_new', 'Create new tab', '', false);
  Toolbar.AddDropdown(nil, @DoNewFileMenu);
  Toolbar.AddButton(1, @tbOpenClick, 'f_open', 'Open file', '', false);
  Toolbar.AddDropdown(PopupRecents);
  Toolbar.AddButton(2, @tbSaveClick, 'f_save', 'Save file', '', false);
  Toolbar.AddSep;
  Toolbar.AddButton(3, @tbFindClick, 'find', 'Dialog "Find"', '', false);
  Toolbar.AddDropdown(PopupFind);
  Toolbar.AddButton(4, @tbGotoClick, 'goto', 'Dialog "Go to"', '', false);
  Toolbar.AddSep;
  Toolbar.AddButton(7, @tbCutClick, 'e_cut', 'Cut', '', false);
  Toolbar.AddButton(6, @tbCopyClick, 'e_copy', 'Copy', '', false);
  Toolbar.AddButton(8, @tbPasteClick, 'e_paste', 'Paste', '', false);
  Toolbar.AddButton(11, @tbUndoClick, 'e_undo', 'Undo', '', false);
  Toolbar.AddButton(12, @tbRedoClick, 'e_redo', 'Redo', '', false);
  Toolbar.AddSep;
  Toolbar.AddButton(14, @tbUnpriClick, 'unpri', 'Toggle unprinted chars', '', false);
  Toolbar.AddButton(17, @tbSidePanelClick, 'p_side', 'Toggle side panel', '', false);
  Toolbar.AddButton(16, @tbBtmPanelClick, 'p_btm', 'Toggle bottom panel', '', false);
  Toolbar.AddButton(15, @tbMinimapClick, 'map', 'Toggle minimap', '', false);

  UpdateToolbar;
end;

procedure TfmMain.UpdateToolbar;
begin
  Toolbar.UpdateControls;
end;


