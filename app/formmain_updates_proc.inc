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

procedure TfmMain.UpdateMenuItemHotkey(mi: TMenuItem; cmd: integer);
begin
  if not Assigned(mi) then exit;
  mi.Tag:= cmd;
  mi.OnClick:= @MenuMainClick;
  mi.ShortCut:= AppKeymap.GetShortcutFromCommand(cmd);

  //don't set Esc here: for OSX/ Qt, else Esc blocked in Find/ Goto/ closing app
  if mi.Shortcut=AppShortcutEscape then
    mi.ShortCut:= 0;
end;

procedure TfmMain.UpdateMenuItemAltObject(mi: TMenuItem; cmd: integer);
  //
  procedure DoSave(mi: TMenuItem; var save: TATMenuItemsAlt); inline;
  begin
    if save.active0 then save.item0:= mi else save.item1:= mi;
    save.active0:= not save.active0;
  end;
  //
begin
  //fmConsole.DoLogConsoleLine('cmd'+inttostr(cmd));
  if cmd>0 then
  case cmd of
    cCommand_ToggleWordWrap : DoSave(mi, mnuViewWrap_Alt);
    cCommand_ToggleLineNums : DoSave(mi, mnuViewNums_Alt);
    cCommand_ToggleFolding : DoSave(mi, mnuViewFold_Alt);
    cCommand_ToggleRuler : DoSave(mi, mnuViewRuler_Alt);
    cCommand_ToggleMinimap : DoSave(mi, mnuViewMinimap_Alt);
    cmd_SplitTabToggle : DoSave(mi, mnuViewSplitDo_Alt);
    cmd_SplitTabHorzVert : DoSave(mi, mnuViewSplitHorz_Alt);
    cCommand_ToggleUnprinted : DoSave(mi, mnuViewUnpriShow_Alt);
    cCommand_ToggleUnprintedSpaces : DoSave(mi, mnuViewUnpriSpaces_Alt);
    cCommand_ToggleUnprintedEnds : DoSave(mi, mnuViewUnpriEnds_Alt);
    cCommand_ToggleUnprintedEndDetails : DoSave(mi, mnuViewUnpriEndsDet_Alt);
    cmd_ToggleToolbar : DoSave(mi, mnuViewToolbar_Alt);
    cmd_ToggleStatusbar : DoSave(mi, mnuViewStatus_Alt);
    cmd_ToggleFullScreen : DoSave(mi, mnuViewFullscr_Alt);
    cmd_ToggleSidePanel : DoSave(mi, mnuViewSide_Alt);
    cmd_ToggleBottomPanel : DoSave(mi, mnuViewBottom_Alt);
    //cmd_Groups1 : mnuGr1_Alt:= mi;
    //cmd_Groups2horz : mnuGr2H_Alt:= mi;
    //cmd_Groups2vert : mnuGr2V_Alt:= mi;
    //cmd_Groups3horz : mnuGr3H_Alt:= mi;
    //cmd_Groups3vert : mnuGr3V_Alt:= mi;
    //cmd_Groups : mnuGr1p2V_Alt:= mi;
    //cmd_Groups : mnuGr1p2H_Alt:= mi;
    //cmd_Group : mnuGr4H_Alt:= mi;
    //cmd_Group : mnuGr4V_Alt:= mi;
    //cmd_Group : mnuGr4G_Alt:= mi;
    //cmd_Group : mnuGr6_Alt:= mi;
  end;
end;

procedure TfmMain.UpdateMenuHotkeys;
begin
  UpdateMenuItemHotkey(mnuFileEndWin, cmd_LineEndWin);
  UpdateMenuItemHotkey(mnuFileEndUn, cmd_LineEndUnix);
  UpdateMenuItemHotkey(mnuFileEndMac, cmd_LineEndMac);

  UpdateMenuItemHotkey(mnuEndsWin, cmd_LineEndWin);
  UpdateMenuItemHotkey(mnuEndsUnix, cmd_LineEndUnix);
  UpdateMenuItemHotkey(mnuEndsMac, cmd_LineEndMac);

  UpdateMenuItemHotkey(mnuFileNew, cmd_FileNew);
  UpdateMenuItemHotkey(mnuFileOpen, cmd_FileOpen);
  UpdateMenuItemHotkey(mnuFileReopen, cmd_FileReopen);
  UpdateMenuItemHotkey(mnuFileSave, cmd_FileSave);
  UpdateMenuItemHotkey(mnuFileSaveAs, cmd_FileSaveAs);
  UpdateMenuItemHotkey(mnuFileSaveAll, cmd_FileSaveAll);
  UpdateMenuItemHotkey(mnuFileClose, cmd_FileClose);
  UpdateMenuItemHotkey(mnuFileCloseAll, cmd_FileCloseAll);
  UpdateMenuItemHotkey(mnuFileCloseOther, cmd_FileCloseOtherThis);
  UpdateMenuItemHotkey(mnuFileCloseDel, cmd_FileCloseAndDelete);
  UpdateMenuItemHotkey(mnuFileExit, cmd_FileExit);
  UpdateMenuItemHotkey(mnuFileHtml, cmd_FileExportHtml);

  UpdateMenuItemHotkey(mnuSelAll, cCommand_SelectAll);
  UpdateMenuItemHotkey(mnuSelSplit, cCommand_SelectSplitToLines);
  UpdateMenuItemHotkey(mnuSelExtLine, cCommand_SelectExtendByLine);
  UpdateMenuItemHotkey(mnuSelInvert, cCommand_SelectInverted);

  UpdateMenuItemHotkey(mnuCaretsCancel, cCommand_Cancel);
  UpdateMenuItemHotkey(mnuCaretsUp1Line, cCommand_CaretsExtendUpLine);
  UpdateMenuItemHotkey(mnuCaretsUp1Page, cCommand_CaretsExtendUpPage);
  UpdateMenuItemHotkey(mnuCaretsUpBegin, cCommand_CaretsExtendUpToTop);
  UpdateMenuItemHotkey(mnuCaretsDown1Line, cCommand_CaretsExtendDownLine);
  UpdateMenuItemHotkey(mnuCaretsDown1Page, cCommand_CaretsExtendDownPage);
  UpdateMenuItemHotkey(mnuCaretsDownEnd, cCommand_CaretsExtendDownToEnd);

  UpdateMenuItemHotkey(mnuEditUndo, cCommand_Undo);
  UpdateMenuItemHotkey(mnuEditRedo, cCommand_Redo);
  UpdateMenuItemHotkey(mnuEditCut, cCommand_ClipboardCut);
  UpdateMenuItemHotkey(mnuEditCopy, cCommand_ClipboardCopy);
  UpdateMenuItemHotkey(mnuEditPaste, cCommand_ClipboardPaste);
  UpdateMenuItemHotkey(mnuEditDel, cCommand_TextDeleteSelection);

  UpdateMenuItemHotkey(mnuEditCopyLine, cmd_CopyLine);
  UpdateMenuItemHotkey(mnuEditCopyAppend, cCommand_ClipboardCopyAdd);
  UpdateMenuItemHotkey(mnuEditCopyFFull, cmd_CopyFilenameFull);
  UpdateMenuItemHotkey(mnuEditCopyFDir, cmd_CopyFilenameDir);
  UpdateMenuItemHotkey(mnuEditCopyFName, cmd_CopyFilenameName);

  UpdateMenuItemHotkey(mnuEditIndent, cCommand_TextIndent);
  UpdateMenuItemHotkey(mnuEditUnindent, cCommand_TextUnindent);
  UpdateMenuItemHotkey(mnuEditTrim, cCommand_TextTrimSpacesAll);
  UpdateMenuItemHotkey(mnuEditTrimL, cCommand_TextTrimSpacesLeft);
  UpdateMenuItemHotkey(mnuEditTrimR, cCommand_TextTrimSpacesRight);

  UpdateMenuItemHotkey(mnuEditTabToSp, cmd_ConvertTabsToSpaces);
  UpdateMenuItemHotkey(mnuEditSpToTab, cmd_ConvertSpacesToTabsLeading);

  UpdateMenuItemHotkey(mnuEditLineDel, cCommand_TextDeleteLine);
  UpdateMenuItemHotkey(mnuEditLineDup, cCommand_TextDuplicateLine);
  UpdateMenuItemHotkey(mnuEditLineMoveUp, cCommand_MoveSelectionUp);
  UpdateMenuItemHotkey(mnuEditLineMoveDown, cCommand_MoveSelectionDown);

  UpdateMenuItemHotkey(mnuCaseUp, cCommand_TextCaseUpper);
  UpdateMenuItemHotkey(mnuCaseLow, cCommand_TextCaseLower);
  UpdateMenuItemHotkey(mnuCaseTitle, cCommand_TextCaseTitle);
  UpdateMenuItemHotkey(mnuCaseInvert, cCommand_TextCaseInvert);
  UpdateMenuItemHotkey(mnuCaseSent, cCommand_TextCaseSentence);

  UpdateMenuItemHotkey(mnuEditCharmap, cmd_DialogCharMap);

  UpdateMenuItemHotkey(mnuFindDlg, cmd_DialogFind);
  UpdateMenuItemHotkey(mnuFindRepDialog, cmd_DialogReplace);
  UpdateMenuItemHotkey(mnuFindNext, cmd_FindNext);
  UpdateMenuItemHotkey(mnuFindPrev, cmd_FindPrev);
  UpdateMenuItemHotkey(mnuFindWordNext, cmd_FindCurWordNext);
  UpdateMenuItemHotkey(mnuFindWordPrev, cmd_FindCurWordPrev);

  UpdateMenuItemHotkey(mnuGotoLine, cmd_DialogGoto);
  UpdateMenuItemHotkey(mnuGotoBm, cmd_DialogGotoBookmark);

  UpdateMenuItemHotkey(mnuBmNext, cmd_BookmarkGotoNext);
  UpdateMenuItemHotkey(mnuBmPrev, cmd_BookmarkGotoPrev);
  UpdateMenuItemHotkey(mnuBmToggle, cmd_BookmarkToggle);
  UpdateMenuItemHotkey(mnuBmInvert, cmd_BookmarkInvertAll);
  UpdateMenuItemHotkey(mnuBmCarets, cmd_BookmarkPlaceCarets);
  UpdateMenuItemHotkey(mnuBmClear, cmd_BookmarkClearAll);
  UpdateMenuItemHotkey(mnuBmCopyLines, cmd_BookmarkCopyMarkedLines);
  UpdateMenuItemHotkey(mnuBmDeleteLines, cmd_BookmarkDeleteMarkedLines);

  UpdateMenuItemHotkey(mnuGr1, cmd_Groups1);
  UpdateMenuItemHotkey(mnuGr2V, cmd_Groups2horz);
  UpdateMenuItemHotkey(mnuGr2H, cmd_Groups2vert);
  UpdateMenuItemHotkey(mnuGr3V, cmd_Groups3horz);
  UpdateMenuItemHotkey(mnuGr3H, cmd_Groups3vert);
  UpdateMenuItemHotkey(mnuGr1p2V, cmd_Groups3plus);
  UpdateMenuItemHotkey(mnuGr1p2H, cmd_Groups3plushorz);
  UpdateMenuItemHotkey(mnuGr4V, cmd_Groups4horz);
  UpdateMenuItemHotkey(mnuGr4H, cmd_Groups4vert);
  UpdateMenuItemHotkey(mnuGr4G, cmd_Groups4grid);
  UpdateMenuItemHotkey(mnuGr6, cmd_Groups6grid);

  UpdateMenuItemHotkey(mnuViewSplitDo, cmd_SplitTabToggle);
  UpdateMenuItemHotkey(mnuViewSplitHorz, cmd_SplitTabHorzVert);

  UpdateMenuItemHotkey(mnuViewWrap, cCommand_ToggleWordWrap);
  UpdateMenuItemHotkey(mnuViewNums, cCommand_ToggleLineNums);
  UpdateMenuItemHotkey(mnuViewFold, cCommand_ToggleFolding);
  UpdateMenuItemHotkey(mnuViewRuler, cCommand_ToggleRuler);
  UpdateMenuItemHotkey(mnuViewMinimap, cCommand_ToggleMinimap);

  UpdateMenuItemHotkey(mnuViewUnpriShow, cCommand_ToggleUnprinted);
  UpdateMenuItemHotkey(mnuViewUnpriSpaces, cCommand_ToggleUnprintedSpaces);
  UpdateMenuItemHotkey(mnuViewUnpriEnds, cCommand_ToggleUnprintedEnds);
  UpdateMenuItemHotkey(mnuViewUnpriEndsDet, cCommand_ToggleUnprintedEndDetails);

  UpdateMenuItemHotkey(mnuViewFullscr, cmd_ToggleFullScreen);
  UpdateMenuItemHotkey(mnuViewSide, cmd_ToggleSidePanel);
  UpdateMenuItemHotkey(mnuViewBottom, cmd_ToggleBottomPanel);
  UpdateMenuItemHotkey(mnuViewToolbar, cmd_ToggleToolbar);
  UpdateMenuItemHotkey(mnuViewStatus, cmd_ToggleStatusbar);

  UpdateMenuItemHotkey(mnuOpDefault, cmd_OpsOpenDefault);
  UpdateMenuItemHotkey(mnuOpUser, cmd_OpsOpenUser);
  UpdateMenuItemHotkey(mnuOpLexer, cmd_OpsOpenLexerOvr);
  UpdateMenuItemHotkey(mnuOpFileTypes, cmd_OpsOpenFileTypes);
  UpdateMenuItemHotkey(mnuOpKeys, cmd_HelpHotkeys);

  UpdateMenuItemHotkey(mnuFontText, cmd_OpsFontText);
  UpdateMenuItemHotkey(mnuFontUi, cmd_OpsFontUi);
  UpdateMenuItemHotkey(mnuFontOutput, cmd_OpsFontOutput);

  UpdateMenuItemHotkey(mnuOpLexProp, cmd_DialogLexerProp);
  UpdateMenuItemHotkey(mnuOpLexLib, cmd_DialogLexerLib);
  UpdateMenuItemHotkey(mnuOpLexMap, cmd_DialogLexerStyleMap);
  UpdateMenuItemHotkey(mnuOpThemeUi, cmd_DialogThemeUi);
  UpdateMenuItemHotkey(mnuOpThemeSyntax, cmd_DialogThemeSyntax);

  UpdateMenuItemHotkey(mnuHelpCmd, cmd_DialogCommands);
  UpdateMenuItemHotkey(mnuHelpForum, cmd_HelpForum);
  UpdateMenuItemHotkey(mnuHelpWiki, cmd_HelpWiki);
  UpdateMenuItemHotkey(mnuHelpMouse, cmd_HelpMouse);
  UpdateMenuItemHotkey(mnuHelpLexers, cmd_HelpLexers);
  UpdateMenuItemHotkey(mnuHelpIssues, cmd_HelpIssues);
  UpdateMenuItemHotkey(mnuHelpChangelog, cmd_HelpChangelog);
  UpdateMenuItemHotkey(mnuHelpAbout, cmd_HelpAbout);

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


procedure UpdateMenuItemChecked(mi: TMenuItem; save: TATMenuItemsAlt; Value: boolean); inline;
begin
  if Assigned(mi) then mi.Checked:= Value;
  if Assigned(save.item0) then save.item0.Checked:= Value;
  if Assigned(save.item1) then save.item1.Checked:= Value;
end;

procedure TfmMain.UpdateMenuChecks;
var
  F: TEditorFrame;
  Ed: TATSynEdit;
begin
  F:= CurrentFrame;
  Ed:= CurrentEditor;
  if F=nil then exit;
  if Ed=nil then exit;

  UpdateMenuItemChecked(mnuViewWrap, mnuViewWrap_Alt, Ed.OptWrapMode<>cWrapOff);
  UpdateMenuItemChecked(mnuViewNums, mnuViewNums_Alt, Ed.Gutter.Items[Ed.GutterBandNum].Visible);
  UpdateMenuItemChecked(mnuViewFold, mnuViewFold_Alt, Ed.Gutter.Items[Ed.GutterBandFold].Visible);
  UpdateMenuItemChecked(mnuViewRuler, mnuViewRuler_Alt, Ed.OptRulerVisible);
  UpdateMenuItemChecked(mnuViewMinimap, mnuViewMinimap_Alt, Ed.OptMinimapVisible);

  UpdateMenuItemChecked(mnuViewSplitDo, mnuViewSplitDo_Alt, F.Splitted);
  UpdateMenuItemChecked(mnuViewSplitHorz, mnuViewSplitHorz_Alt, F.SplitHorz);

  UpdateMenuItemChecked(mnuViewUnpriShow, mnuViewUnpriShow_Alt, Ed.OptUnprintedVisible);
  UpdateMenuItemChecked(mnuViewUnpriSpaces, mnuViewUnpriSpaces_Alt, Ed.OptUnprintedSpaces);
  UpdateMenuItemChecked(mnuViewUnpriEnds, mnuViewUnpriEnds_Alt, Ed.OptUnprintedEnds);
  UpdateMenuItemChecked(mnuViewUnpriEndsDet, mnuViewUnpriEndsDet_Alt, Ed.OptUnprintedEndsDetails);

  UpdateMenuItemChecked(mnuViewToolbar, mnuViewToolbar_Alt, ShowToolbar);
  UpdateMenuItemChecked(mnuViewStatus, mnuViewStatus_Alt, ShowStatus);
  UpdateMenuItemChecked(mnuViewFullscr, mnuViewFullscr_Alt, ShowFullscreen);
  UpdateMenuItemChecked(mnuViewSide, mnuViewSide_Alt, ShowSidePanel);
  UpdateMenuItemChecked(mnuViewBottom, mnuViewBottom_Alt, ShowBottom);

  UpdateMenuItemChecked(mnuGr1, mnuGr1_Alt, Groups.Mode= gmOne);
  UpdateMenuItemChecked(mnuGr2H, mnuGr2H_Alt, Groups.Mode= gm2Vert);
  UpdateMenuItemChecked(mnuGr2V, mnuGr2V_Alt, Groups.Mode= gm2Horz);
  UpdateMenuItemChecked(mnuGr3H, mnuGr3H_Alt, Groups.Mode= gm3Vert);
  UpdateMenuItemChecked(mnuGr3V, mnuGr3V_Alt, Groups.Mode= gm3Horz);
  UpdateMenuItemChecked(mnuGr1p2V, mnuGr1p2V_Alt, Groups.Mode= gm1plus2Vert);
  UpdateMenuItemChecked(mnuGr1p2H, mnuGr1p2H_Alt, Groups.Mode= gm1plus2Horz);
  UpdateMenuItemChecked(mnuGr4H, mnuGr4H_Alt, Groups.Mode= gm4Vert);
  UpdateMenuItemChecked(mnuGr4V, mnuGr4V_Alt, Groups.Mode= gm4Horz);
  UpdateMenuItemChecked(mnuGr4G, mnuGr4G_Alt, Groups.Mode= gm4Grid);
  UpdateMenuItemChecked(mnuGr6, mnuGr6_Alt, Groups.Mode= gm6Grid);
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


