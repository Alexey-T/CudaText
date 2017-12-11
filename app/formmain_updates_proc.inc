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
  SName, SMark, SSession, SCurMode: string;
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

  SCurMode:= '';
  if F.IsBinary then
    SCurMode:= ' [Hex viewer]';

  Caption:= SMark + SName + SSession + SCurMode + ' - ' + msgTitle;
  Application.Title:= SMark + F.TabCaption + ' - ' + msgTitle;
end;


procedure TfmMain.UpdateStatus_ToolButton(AToolbar: TATButtonsToolbar;
  ACmd: integer; AChecked: boolean);
var
  Btn: TATButton;
  i: integer;
begin
  for i:= 0 to AToolbar.ButtonCount-1 do
  begin
    Btn:= AToolbar.Buttons[i];
    if Btn.DataString=IntToStr(ACmd) then
    begin
      Btn.Checked:= AChecked;
      exit
    end;
  end;
end;


procedure TfmMain.UpdateStatus;
var
  Ed: TATSynEdit;
  F: TEditorFrame;
  S, fmt: string;
  Size: TPoint;
begin
  F:= CurrentFrame;
  Ed:= CurrentEditor;
  if F=nil then exit;
  if Ed=nil then exit;

  UpdateMenuChecks;

  ToolbarSideMid.Buttons[0].Checked:= Assigned(fmFind) and fmFind.Visible;
  ToolbarSideMid.Buttons[1].Checked:= Assigned(fmGoto) and fmGoto.Visible;

  UpdateStatus_ToolButton(ToolbarMain, cCommand_ToggleMinimap, Ed.OptMinimapVisible);
  UpdateStatus_ToolButton(ToolbarMain, cCommand_ToggleMicromap, Ed.OptMicromapVisible);
  UpdateStatus_ToolButton(ToolbarMain, cCommand_ToggleUnprinted, Ed.OptUnprintedVisible);

  if not F.IsText then
  begin
    DoStatusbarTextByTag(Status, StatusbarTag_Caret, '');
    DoStatusbarTextByTag(Status, StatusbarTag_Enc, '');
    DoStatusbarTextByTag(Status, StatusbarTag_LineEnds, '');

    if F.IsBinary then
      S:= msgStatusHexViewer
    else
    begin
      Size:= F.PictureSizes;
      S:= Format(msgStatusPictureNxN, [Size.X, Size.Y]);
    end;
    DoStatusbarTextByTag(Status, StatusbarTag_Lexer, S);

    DoStatusbarTextByTag(Status, StatusbarTag_TabSize, '');
    DoStatusbarTextByTag(Status, StatusbarTag_InsOvr, '');
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
  DoStatusbarTextByTag(Status, StatusbarTag_Caret, EditorFormatStatus(Ed, fmt));

  //------
  S:= F.EncodingName;
  DoStatusbarTextByTag(Status, StatusbarTag_Enc, S);

  //------
  case F.LineEnds of
    cEndWin: S:= msgEndWin;
    cEndUnix: S:= msgEndUnix;
    cEndMac: S:= msgEndMac;
    else S:= '?';
  end;
  DoStatusbarTextByTag(Status, StatusbarTag_LineEnds, S);

  //------
  S:= F.LexerName;
  if S='' then
    S:= msgNoLexer;
  DoStatusbarTextByTag(Status, StatusbarTag_Lexer, S);

  //------
  DoStatusbarTextByTag(Status, StatusbarTag_TabSize, EditorFormatTabsize(Ed, UiOps.StatusTabsize));

  //------
  S:= IfThen(Ed.ModeOverwrite, 'Ovr', 'Ins');
  DoStatusbarTextByTag(Status, StatusbarTag_InsOvr, S);

  //------
  DoStatusbarTextByTag(Status, StatusbarTag_SelMode, IfThen(Ed.OptMouseColumnSelectionWithoutKey, '| |', '-'));
  DoStatusbarTextByTag(Status, StatusbarTag_WrapMode, UiOps.StatusWrap[Ord(Ed.OptWrapMode)]);
end;

procedure TfmMain.InitStatusButton;
begin
  //place StatusProgress+ButtonCancel onto Statusbar and anchor to right
  with StatusProgress do
  begin
    Parent:= Status;
    AnchorSideRight.Control:= Status;
    AnchorSideRight.Side:= asrBottom;
    AnchorSideBottom.Control:= Status;
    AnchorSideBottom.Side:= asrBottom;
    AnchorSideTop.Control:= Status;
    AnchorSideTop.Side:= asrTop;
    Anchors:= [akRight, akTop, akBottom];
  end;
  with ButtonCancel do
  begin
    Parent:= Status;
    AnchorSideRight.Control:= StatusProgress;
    AnchorSideBottom.Control:= StatusProgress;
    AnchorSideBottom.Side:= asrBottom;
    AnchorSideTop.Control:= StatusProgress;
    Anchors:= [akRight, akTop, akBottom];
  end;
  StatusProgress.Hide;
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
  Form.Height:= MulDiv(UiOps.ListboxSizeY, UiOps.ScreenScale, 100);
end;


procedure TfmMain.UpdateEnabledAll(b: boolean);
begin
  Groups.Enabled:= b;
  if Assigned(fmFind) then
    fmFind.Enabled:= b;
end;

procedure TfmMain.UpdateAppForSearch(AStart, AEdLock, AFindMode: boolean);
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
    StatusProgress.Show;
    StatusProgress.Progress:= 0;
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
    StatusProgress.Hide;
    UpdateEnabledAll(true);
    Ed.Enabled:= true;
    Ed.EndUpdate;

    if Assigned(fmFind) and fmFind.Visible and fmFind.Enabled then
      fmFind.UpdateFocus(AFindMode)
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
    DoOps_LoadOptionsLexerSpecific(F);
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
  Status.Font:= Self.Font;
  StatusAlt.Font:= Self.Font;
  StatusProgress.Font:= Self.Font;

  //dont set font for PanelLeft/PanelBottom, side effects
  Tree.Font.Name:= UiOps.VarFontName;
  Tree.Font.Size:= UiOps.VarFontSize;
  Tree.Font.PixelsPerInch:= Screen.PixelsPerInch;
  PanelLeftTitle.Font.Name:= UiOps.VarFontName;
  PanelLeftTitle.Font.Size:= UiOps.VarFontSize;
  PanelLeftTitle.Font.PixelsPerInch:= Screen.PixelsPerInch;

  ATButtonTheme.FontName:= UiOps.VarFontName;
  ATButtonTheme.FontSize:= UiOps.VarFontSize;

  if Assigned(fmFind) then fmFind.UpdateFonts;
  if Assigned(fmGoto) then fmGoto.UpdateFonts;

  CompletionOps.FontName:= UiOps.VarFontName;
  CompletionOps.FontSize:= UiOps.VarFontSize;
  CompletionOps.ItemHeight:= GetListboxItemHeight(UiOps.VarFontName, UiOps.VarFontSize);
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
    DoOps_LoadOptionsLexerSpecific(F);
  end;

  UpdateStatus;
end;

procedure TfmMain.DoApplyFrameOps(F: TEditorFrame;
  const Op: TEditorOps; AForceApply: boolean);
begin
  EditorApplyOps(F.Editor, Op, AForceApply, not F.TabSizeChanged);
  EditorApplyOps(F.Editor2, Op, AForceApply, not F.TabSizeChanged);

  F.Adapter.DynamicHiliteEnabled:= Op.OpLexerDynamicHiliteEnabled;
  F.Adapter.DynamicHiliteMaxLines:= Op.OpLexerDynamicHiliteMaxLines;
  F.Adapter.EnabledLineSeparators:= Op.OpLexerLineSeparators;

  F.Editor.Update;
  if F.Splitted then
    F.Editor2.Update;

  UpdateStatus;
end;

procedure TfmMain.UpdateMenuItemHint(mi: TMenuItem; const AHint: string);
begin
  if not Assigned(mi) then exit;

  if mi.Tag=0 then
    mi.Tag:= PtrInt(TAppMenuProps.Create);

  TAppMenuProps(mi.Tag).CommandString:= AHint;
end;

procedure TfmMain.UpdateMenuItemHotkey(mi: TMenuItem; cmd: integer);
begin
  if not Assigned(mi) then exit;

  if mi.Tag=0 then
    mi.Tag:= PtrInt(TAppMenuProps.Create);

  TAppMenuProps(mi.Tag).CommandCode:= cmd;
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
    cCommand_ToggleMicromap : DoSave(mi, mnuViewMicromap_Alt);
    cmd_SplitTabToggle : DoSave(mi, mnuViewSplitDo_Alt);
    cmd_SplitTabHorzVert : DoSave(mi, mnuViewSplitHorz_Alt);
    cCommand_ToggleUnprinted : DoSave(mi, mnuViewUnpriShow_Alt);
    cCommand_ToggleUnprintedSpaces : DoSave(mi, mnuViewUnpriSpaces_Alt);
    cCommand_ToggleUnprintedSpacesTrailing : DoSave(mi, mnuViewUnpriSpacesTail_Alt);
    cCommand_ToggleUnprintedEnds : DoSave(mi, mnuViewUnpriEnds_Alt);
    cCommand_ToggleUnprintedEndDetails : DoSave(mi, mnuViewUnpriEndsDet_Alt);

    cmd_ToggleToolbar : DoSave(mi, mnuViewToolbar_Alt);
    cmd_ToggleStatusbar : DoSave(mi, mnuViewStatus_Alt);
    cmd_ToggleFullScreen : DoSave(mi, mnuViewFullscr_Alt);
    cmd_ToggleDistractionFree : DoSave(mi, mnuViewDistFree_Alt);
    cmd_ToggleSidePanel : DoSave(mi, mnuViewSide_Alt);
    cmd_ToggleBottomPanel : DoSave(mi, mnuViewBottom_Alt);
    cmd_ToggleOnTop: DoSave(mi, mnuViewOnTop_Alt);

    cmd_Groups1 : DoSave(mi, mnuGr1_Alt);
    cmd_Groups2horz : DoSave(mi, mnuGr2H_Alt);
    cmd_Groups2vert : DoSave(mi, mnuGr2V_Alt);
    cmd_Groups3horz : DoSave(mi, mnuGr3H_Alt);
    cmd_Groups3vert : DoSave(mi, mnuGr3V_Alt);
    cmd_Groups3plus : DoSave(mi, mnuGr1p2V_Alt);
    cmd_Groups3plushorz : DoSave(mi, mnuGr1p2H_Alt);
    cmd_Groups4horz : DoSave(mi, mnuGr4H_Alt);
    cmd_Groups4vert : DoSave(mi, mnuGr4V_Alt);
    cmd_Groups4grid : DoSave(mi, mnuGr4G_Alt);
    cmd_Groups6grid : DoSave(mi, mnuGr6_Alt);
  end;
end;

procedure TfmMain.UpdateMenuHotkeys;
begin
  UpdateMenuItemHotkey(mnuApple_About, cmd_HelpAbout);
  UpdateMenuItemHotkey(mnuApple_CheckUpd, cmd_HelpCheckUpdates);

  UpdateMenuItemHotkey(mnuFileEndWin, cmd_LineEndWin);
  UpdateMenuItemHotkey(mnuFileEndUn, cmd_LineEndUnix);
  UpdateMenuItemHotkey(mnuFileEndMac, cmd_LineEndMac);

  UpdateMenuItemHotkey(mnuEndsWin, cmd_LineEndWin);
  UpdateMenuItemHotkey(mnuEndsUnix, cmd_LineEndUnix);
  UpdateMenuItemHotkey(mnuEndsMac, cmd_LineEndMac);

  UpdateMenuItemHotkey(mnuFileNew, cmd_FileNew);
  UpdateMenuItemHotkey(mnuFileNewMenu, cmd_FileNewMenu);
  UpdateMenuItemHotkey(mnuFileOpen, cmd_FileOpen);
  UpdateMenuItemHotkey(mnuFileOpenDir, cmd_FileOpenFolder);
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
  UpdateMenuItemHotkey(mnuBmPlaceCarets, cmd_BookmarkPlaceCarets);
  UpdateMenuItemHotkey(mnuBmPlaceOnCarets, cmd_BookmarkPlaceBookmarksOnCarets);
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
  UpdateMenuItemHotkey(mnuViewMicromap, cCommand_ToggleMicromap);

  UpdateMenuItemHotkey(mnuViewUnpriShow, cCommand_ToggleUnprinted);
  UpdateMenuItemHotkey(mnuViewUnpriSpaces, cCommand_ToggleUnprintedSpaces);
  UpdateMenuItemHotkey(mnuViewUnpriSpacesTail, cCommand_ToggleUnprintedSpacesTrailing);
  UpdateMenuItemHotkey(mnuViewUnpriEnds, cCommand_ToggleUnprintedEnds);
  UpdateMenuItemHotkey(mnuViewUnpriEndsDet, cCommand_ToggleUnprintedEndDetails);

  UpdateMenuItemHotkey(mnuViewFullscr, cmd_ToggleFullScreen);
  UpdateMenuItemHotkey(mnuViewDistFree, cmd_ToggleDistractionFree);
  UpdateMenuItemHotkey(mnuViewSide, cmd_ToggleSidePanel);
  UpdateMenuItemHotkey(mnuViewBottom, cmd_ToggleBottomPanel);
  UpdateMenuItemHotkey(mnuViewToolbar, cmd_ToggleToolbar);
  UpdateMenuItemHotkey(mnuViewStatus, cmd_ToggleStatusbar);
  UpdateMenuItemHotkey(mnuViewOnTop, cmd_ToggleOnTop);

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
  UpdateMenuItemHotkey(mnuHelpCheckUpd, cmd_HelpCheckUpdates);
  UpdateMenuItemHotkey(mnuHelpAbout, cmd_HelpAbout);
end;

procedure TfmMain.UpdateEditorTabsize(N: integer);
begin
  case N of
    -1: CurrentEditor.OptTabSpaces:= true;
    -2: CurrentEditor.OptTabSpaces:= false;
    1..20: CurrentEditor.OptTabSize:= N;
  end;

  CurrentFrame.TabSizeChanged:= true;

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
    for i:= 0 to AppManager.LexerCount-1 do
      sl.AddObject('lexer: '+AppManager.Lexers[i].LexerName, TObject(cmdFirstLexerCommand+i));
    sl.Sort;

    //insert "none" at list begin
    sl.InsertObject(0, 'lexer: '+msgNoLexer, TObject(cmdLastLexerCommand));

    for i:= 0 to sl.count-1 do
      AppKeymap.Add(PtrInt(sl.Objects[i]), sl[i], [], []);
  finally
    FreeAndNil(sl);
  end;

  //dynamic plugin items
  for i:= 0 to High(AppPluginsCommand) do
  begin
    if AppPluginsCommand[i].ItemModule='' then Break;
    if SEndsWith(AppPluginsCommand[i].ItemCaption, '-') then Continue;

    Str:= AppPluginsCommand[i].ItemCaption;
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
  UpdateMenuItemChecked(mnuViewMicromap, mnuViewMicromap_Alt, Ed.OptMicromapVisible);

  UpdateMenuItemChecked(mnuViewSplitDo, mnuViewSplitDo_Alt, F.Splitted);
  UpdateMenuItemChecked(mnuViewSplitHorz, mnuViewSplitHorz_Alt, F.SplitHorz);

  UpdateMenuItemChecked(mnuViewUnpriShow, mnuViewUnpriShow_Alt, Ed.OptUnprintedVisible);
  UpdateMenuItemChecked(mnuViewUnpriSpaces, mnuViewUnpriSpaces_Alt, Ed.OptUnprintedSpaces);
  UpdateMenuItemChecked(mnuViewUnpriSpacesTail, mnuViewUnpriSpacesTail_Alt, Ed.OptUnprintedSpacesTrailing);
  UpdateMenuItemChecked(mnuViewUnpriEnds, mnuViewUnpriEnds_Alt, Ed.OptUnprintedEnds);
  UpdateMenuItemChecked(mnuViewUnpriEndsDet, mnuViewUnpriEndsDet_Alt, Ed.OptUnprintedEndsDetails);

  UpdateMenuItemChecked(mnuViewToolbar, mnuViewToolbar_Alt, ShowToolbar);
  UpdateMenuItemChecked(mnuViewStatus, mnuViewStatus_Alt, ShowStatus);
  UpdateMenuItemChecked(mnuViewFullscr, mnuViewFullscr_Alt, ShowFullscreen);
  UpdateMenuItemChecked(mnuViewDistFree, mnuViewDistFree_Alt, ShowDistractionFree);
  UpdateMenuItemChecked(mnuViewSide, mnuViewSide_Alt, ShowSidePanel);
  UpdateMenuItemChecked(mnuViewBottom, mnuViewBottom_Alt, ShowBottom);
  UpdateMenuItemChecked(mnuViewOnTop, mnuViewOnTop_Alt, ShowOnTop);

  UpdateMenuItemChecked(mnuGr1, mnuGr1_Alt, Groups.Mode= gmOne);
  UpdateMenuItemChecked(mnuGr2V, mnuGr2V_Alt, Groups.Mode= gm2Horz);
  UpdateMenuItemChecked(mnuGr2H, mnuGr2H_Alt, Groups.Mode= gm2Vert);
  UpdateMenuItemChecked(mnuGr3V, mnuGr3V_Alt, Groups.Mode= gm3Horz);
  UpdateMenuItemChecked(mnuGr3H, mnuGr3H_Alt, Groups.Mode= gm3Vert);
  UpdateMenuItemChecked(mnuGr1p2V, mnuGr1p2V_Alt, Groups.Mode= gm1plus2Vert);
  UpdateMenuItemChecked(mnuGr1p2H, mnuGr1p2H_Alt, Groups.Mode= gm1plus2Horz);
  UpdateMenuItemChecked(mnuGr4V, mnuGr4V_Alt, Groups.Mode= gm4Horz);
  UpdateMenuItemChecked(mnuGr4H, mnuGr4H_Alt, Groups.Mode= gm4Vert);
  UpdateMenuItemChecked(mnuGr4G, mnuGr4G_Alt, Groups.Mode= gm4Grid);
  UpdateMenuItemChecked(mnuGr6, mnuGr6_Alt, Groups.Mode= gm6Grid);
end;


procedure TfmMain.UpdateTree(AFill: boolean; AConsiderTreeVisible: boolean;
  AForceUpdateAll: boolean);
var
  Frame: TEditorFrame;
  Ed: TATSynEdit;
begin
  if UiOps.TreeShowIcons then
    Tree.Images:= ImageListTree
  else
    Tree.Images:= nil;

  if AConsiderTreeVisible then
    if not ShowSidePanel then exit;

  Ed:= CurrentEditor;
  Frame:= GetEditorFrame(Ed);

  if (Ed.Strings.Count=0) or
    (Ed.Strings.Count>UiOps.MaxLinesForTree) then
  begin
    Tree.Items.Clear;
    exit;
  end;

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
        //Lazarus dont sort new items
        if Tree.SortType<>stNone then
          Tree.AlphaSort;
      finally
        Tree.EndUpdate;
      end;

      //force update scrollbar
      Tree.Perform(CM_CHANGED, 0, 0);

      if UiOps.TreeCache then
        DoTreeviewCopy(Tree, Frame.CachedTreeview);
    end;

    UpdateTreeSelection;
  end;
end;

procedure TfmMain.UpdateTreeSelection;
var
  Ed: TATSynEdit;
  Frame: TEditorFrame;
  Caret: TATCaretItem;
begin
  Ed:= CurrentEditor;
  Frame:= GetEditorFrame(Ed);
  if Ed.Carets.Count=0 then exit;
  Caret:= Ed.Carets[0];

  if UiOps.TreeAutoSync then
    Frame.Adapter.TreeShowItemForCaret(Tree, Point(Caret.PosX, Caret.PosY));
end;

procedure TfmMain.UpdateMenuEnc(AMenu: TMenuItem);
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

  for i:= Low(AppEncodings) to High(AppEncodings) do
  begin
    DoAdd(miReload, AppEncodings[i].Sub, AppEncodings[i].Name, true);
    DoAdd(miNoReload, AppEncodings[i].Sub, AppEncodings[i].Name, false);
  end;
end;


procedure TfmMain.UpdateStatusbarPanelsFromString(AStr: string);
var
  SPanel, SItem: string;
  NIndex, NSize, NTag: integer;
  Al: TAlignment;
begin
  //complex deletion of panels, touch only CudaText std panels
  for NTag:= StatusbarTag_Caret{minimal tag} to StatusbarTag_Msg{maximal tag} do
  begin
    NIndex:= Status.FindPanel(NTag);
    Status.DeletePanel(NIndex);
  end;

  repeat
    SPanel:= SGetItem(AStr, '|');
    if SPanel='' then Break;

    NTag:= 0;
    NIndex:= Status.PanelCount;
    SItem:= SGetItem(SPanel);
    case SItem of
      'caret':   begin NTag:= StatusbarTag_Caret; end;
      'enc':     begin NTag:= StatusbarTag_Enc; end;
      'ends':    begin NTag:= StatusbarTag_LineEnds; end;
      'lexer':   begin NTag:= StatusbarTag_Lexer; end;
      'tabsize': begin NTag:= StatusbarTag_TabSize; end;
      'ins':     begin NTag:= StatusbarTag_InsOvr; end;
      'msg':     begin NTag:= StatusbarTag_Msg; end;
      'selmode': begin NTag:= StatusbarTag_SelMode; end;
      'wrap':    begin NTag:= StatusbarTag_WrapMode; end;
      else Continue;
    end;

    SItem:= SGetItem(SPanel);
    case SItem of
      'L': Al:= taLeftJustify;
      'R': Al:= taRightJustify
       else Al:= taCenter;
    end;

    SItem:= SGetItem(SPanel);
    NSize:= Max(16, StrToIntDef(SItem, 200));

    Status.AddPanel(NSize, Al, '', -1, NTag);
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
var
  NIndex: integer;
begin
  NIndex:= Status.FindPanel(StatusbarTag_Msg);
  if NIndex>=0 then
    Status.DoPanelAutosize(NIndex);
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
  ToolbarMain.AddButton(0, @DoToolbarClick, '', 'Create new tab', IntToStr(cmd_FileNew), false);
  ToolbarMain.AddDropdown(-1, nil, @DoFileNewMenu);
  ToolbarMain.AddButton(1, @DoToolbarClick, '', 'Open file', IntToStr(cmd_FileOpen), false);
  ToolbarMain.AddDropdown(-1, PopupRecents);
  ToolbarMain.AddButton(2, @DoToolbarClick, '', 'Save file', IntToStr(cmd_FileSave), false);
  ToolbarMain.AddSep;
  ToolbarMain.AddButton(3, @DoToolbarClick, '', 'Cut', IntToStr(cCommand_ClipboardCut), false);
  ToolbarMain.AddButton(4, @DoToolbarClick, '', 'Copy', IntToStr(cCommand_ClipboardCopy), false);
  ToolbarMain.AddButton(5, @DoToolbarClick, '', 'Paste', IntToStr(cCommand_ClipboardPaste), false);
  ToolbarMain.AddButton(6, @DoToolbarClick, '', 'Undo', IntToStr(cCommand_Undo), false);
  ToolbarMain.AddButton(7, @DoToolbarClick, '', 'Redo', IntToStr(cCommand_Redo), false);
  ToolbarMain.AddSep;
  ToolbarMain.AddButton(8, @DoToolbarClick, '', 'Toggle unprinted chars', IntToStr(cCommand_ToggleUnprinted), false);
  ToolbarMain.AddButton(9, @DoToolbarClick, '', 'Toggle minimap', IntToStr(cCommand_ToggleMinimap), false);
  ToolbarMain.AddSep;
  ToolbarMain.AddButton(10, @DoToolbarClick, '', 'Indent block', IntToStr(cCommand_TextIndent), false);
  ToolbarMain.AddButton(11, @DoToolbarClick, '', 'Unindent block', IntToStr(cCommand_TextUnIndent), false);

  ToolbarMain.UpdateControls;
end;


const
  cSidebarIconTree     = 0;
  cSidebarIconConsole  = 1;
  cSidebarIconOutput   = 2;
  cSidebarIconValidate = 3;
  cSidebarIconFind     = 4;
  cSidebarIconGoto     = 5;

procedure TfmMain.DoOps_LoadSidebarIcons;
var
  Parts: TRegexParts;
  Str: string;
begin
  SRegexFindParts('.+_(\d+)x(\d+)', UiOps.SidebarTheme, Parts);

  ImageListSide.Width:= StrToIntDef(Parts[1].Str, ImageListSide.Width);
  ImageListSide.Height:= StrToIntDef(Parts[2].Str, ImageListSide.Height);
  ImageListSide.Clear;

  //set ButtonWidth for vertical bars
  ToolbarSideTop.ButtonWidth:= MulDiv(ImageListSide.Width+8, UiOps.ScreenScale, 100);
  ToolbarSideLow.ButtonWidth:= ToolbarSideTop.ButtonWidth;
  ToolbarSideMid.ButtonWidth:= ToolbarSideTop.ButtonWidth;

  Str:= GetAppPath(cDirDataSideIcons)+DirectorySeparator;
  UpdateImagelistWithIconFromFile(ImageListSide, Str+'tree.png');
  UpdateImagelistWithIconFromFile(ImageListSide, Str+'console.png');
  UpdateImagelistWithIconFromFile(ImageListSide, Str+'output.png');
  UpdateImagelistWithIconFromFile(ImageListSide, Str+'validate.png');
  UpdateImagelistWithIconFromFile(ImageListSide, Str+'find.png');
  UpdateImagelistWithIconFromFile(ImageListSide, Str+'goto.png');

  ToolbarSideTop.UpdateControls;
  ToolbarSideLow.UpdateControls;
  ToolbarSideMid.UpdateControls;
end;


procedure TfmMain.DoOps_LoadTreeIcons;
var
  Parts: TRegexParts;
  Str: string;
begin
  SRegexFindParts('.+_(\d+)x(\d+)', UiOps.TreeTheme, Parts);

  ImageListTree.Width:= StrToIntDef(Parts[1].Str, ImageListTree.Width);
  ImageListTree.Height:= StrToIntDef(Parts[2].Str, ImageListTree.Height);
  ImageListTree.Clear;

  Str:= GetAppPath(cDirDataTreeIcons)+DirectorySeparator;
  UpdateImagelistWithIconFromFile(ImageListTree, Str+'dir.png');
  UpdateImagelistWithIconFromFile(ImageListTree, Str+'st1.png');
  UpdateImagelistWithIconFromFile(ImageListTree, Str+'st2.png');
  UpdateImagelistWithIconFromFile(ImageListTree, Str+'st3.png');
  UpdateImagelistWithIconFromFile(ImageListTree, Str+'box.png');
  UpdateImagelistWithIconFromFile(ImageListTree, Str+'fx.png');
  UpdateImagelistWithIconFromFile(ImageListTree, Str+'ar1.png');
  UpdateImagelistWithIconFromFile(ImageListTree, Str+'ar2.png');

  Tree.Invalidate;
end;


procedure TfmMain.InitSidebar;
begin
  //
  ToolbarSideTop.AddButton(cSidebarIconTree, @DoSidebar_OnTabClick, 'Tree', 'Tree', '', UiOps.ShowSidebarCaptions);
  //
  ToolbarSideLow.AddButton(cSidebarIconConsole, @DoBottom_OnTabClick, 'Console', 'Console', '', UiOps.ShowSidebarCaptions);
  ToolbarSideLow.AddButton(cSidebarIconOutput, @DoBottom_OnTabClick, 'Output', 'Output', '', UiOps.ShowSidebarCaptions);
  ToolbarSideLow.AddButton(cSidebarIconValidate, @DoBottom_OnTabClick, 'Validate', 'Validate', '', UiOps.ShowSidebarCaptions);
  //
  ToolbarSideMid.AddButton(cSidebarIconFind, @DoBottom_FindClick, 'Find', 'Search', '', UiOps.ShowSidebarCaptions);
  ToolbarSideMid.AddButton(cSidebarIconGoto, @DoBottom_GotoClick, 'Goto', 'Go to', '', UiOps.ShowSidebarCaptions);
  ToolbarSideMid.AddSep;
end;


(*
procedure TfmMain.FormConstrainedResize(Sender: TObject; var MinWidth, MinHeight,
  MaxWidth, MaxHeight: TConstraintSize);
const
  cMinHeight = 200;
var
  RWork: TRect;
begin
  //event needed only on Mac, to disable resize below Dock
  {$ifndef darwin} exit; {$endif}

  RWork:= Screen.PrimaryMonitor.WorkareaRect;
  MaxHeight:= Max(cMinHeight, RWork.Bottom - RWork.Top - Top);
end;
*)

