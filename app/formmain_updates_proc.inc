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
  SName, SSession, Name1, Name2: string;
begin
  F:= CurrentFrame;
  if F=nil then exit;

  if UiOps.ShowTitlePath then
  begin
    if F.EditorsLinked then
    begin
      SName:= F.FileName;
      if SName='' then
        SName:= F.TabCaption;
    end
    else
    begin
      Name1:= F.GetFileName(F.Ed1);
      Name2:= F.GetFileName(F.Ed2);
      if Name1='' then Name1:= msgUntitledTab;
      if Name2='' then Name2:= msgUntitledTab;
      Name1:= msgModified[F.Ed1.Modified]+Name1;
      Name2:= msgModified[F.Ed2.Modified]+Name2;
      SName:= Name1+' | '+Name2;
    end;
  end
  else
    SName:= F.TabCaption;

  SSession:= ChangeFileExt(ExtractFileName(FSessionName), '');
  if (SSession<>'') and (SSession<>'history session') then
    SSession:= ' {'+SSession+'}'
  else
    SSession:= '';

  Caption:= SName + SSession + ' - ' + msgTitle;

  Application.Title:= F.TabCaption + ' - ' + msgTitle;
end;


procedure TfmMain.UpdateStatus_ToolButton(AToolbar: TATFlatToolbar;
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
begin
  TimerStatusBusy.Enabled:= false;
  TimerStatusBusy.Enabled:= true;
end;

procedure TfmMain.UpdateStatus_RealWork;
var
  F: TEditorFrame;
  Ed: TATSynEdit;
begin
  F:= CurrentFrame;
  if F=nil then exit;
  Ed:= F.Editor;

  UpdateTabCaptionsFromFolders; //maybe it's called too often here?
  UpdateMenuChecks;

  ToolbarSideMid.Buttons[0].Checked:= Assigned(fmFind) and fmFind.Visible;

  UpdateStatus_ToolButton(ToolbarMain, cCommand_ToggleMinimap, Ed.OptMinimapVisible);
  UpdateStatus_ToolButton(ToolbarMain, cCommand_ToggleMicromap, Ed.OptMicromapVisible);
  UpdateStatus_ToolButton(ToolbarMain, cCommand_ToggleUnprinted, Ed.OptUnprintedVisible);

  UpdateStatus_ForFrame(Status, F);
end;

procedure TfmMain.UpdateStatusbarHints;
begin
  DoStatusbarHintByTag(Status, StatusbarTag_Caret, msgStatusbarHintCaret);
  DoStatusbarHintByTag(Status, StatusbarTag_Enc, msgStatusbarHintEnc);
  DoStatusbarHintByTag(Status, StatusbarTag_LineEnds, msgStatusbarHintEnds);
  DoStatusbarHintByTag(Status, StatusbarTag_Lexer, msgStatusbarHintLexer);
  DoStatusbarHintByTag(Status, StatusbarTag_SelMode, msgStatusbarHintSelMode);
  DoStatusbarHintByTag(Status, StatusbarTag_TabSize, msgStatusbarHintTabSize);
  DoStatusbarHintByTag(Status, StatusbarTag_InsOvr, msgStatusbarHintInsOvr);
  DoStatusbarHintByTag(Status, StatusbarTag_WrapMode, msgStatusbarHintWrap);
end;

procedure TfmMain.UpdateStatus_ForFrame(AStatus: TATStatus; F: TEditorFrame);
var
  Ed: TATSynEdit;
  S, fmt: string;
  Size: TPoint;
begin
  Ed:= F.Editor;

  //hide hints for pictures/viewer
  AStatus.ShowHint:= F.IsText;

  if not F.IsText then
  begin
    if F.IsBinary then
      S:= IntToStr(F.Binary.PosPercent)+'%'
    else
      S:= '';
    DoStatusbarTextByTag(AStatus, StatusbarTag_Caret, S);

    if F.IsBinary then
    begin
      S:= F.Binary.TextEncoding;
      if S='' then S:= '?';
    end
    else
      S:= '';
    DoStatusbarTextByTag(AStatus, StatusbarTag_Enc, S);

    DoStatusbarTextByTag(AStatus, StatusbarTag_LineEnds, '');

    if F.IsBinary then
      S:= ViewerModeString(F.Binary)
    else
    if F.IsPicture then
    begin
      Size:= F.PictureSizes;
      S:= Format(msgStatusPictureNxN, [Size.X, Size.Y]);
    end;
    DoStatusbarTextByTag(AStatus, StatusbarTag_Lexer, S);

    if F.IsBinary then
      S:= ''
    else
    if F.IsPicture then
      S:= IntToStr(F.PictureScale)+'%';
    DoStatusbarTextByTag(AStatus, StatusbarTag_TabSize, S);

    DoStatusbarTextByTag(AStatus, StatusbarTag_InsOvr, '');
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
  DoStatusbarTextByTag(AStatus, StatusbarTag_Caret, EditorFormatStatus(Ed, fmt));

  //------
  S:= Ed.EncodingName;
  DoStatusbarTextByTag(AStatus, StatusbarTag_Enc, S);

  //------
  case Ed.Strings.Endings of
    cEndWin: S:= msgEndWin;
    cEndUnix: S:= msgEndUnix;
    cEndMac: S:= msgEndMac;
    else S:= '?';
  end;
  DoStatusbarTextByTag(AStatus, StatusbarTag_LineEnds, S);

  //------
  S:= F.LexerName[Ed];
  if S='' then
    S:= msgNoLexer;
  DoStatusbarTextByTag(AStatus, StatusbarTag_Lexer, S);

  //------
  if Ed.OptTabSpaces then
    S:= msgStatusbarTextSpaces+': '+IntToStr(Ed.OptTabSize)
  else
    S:= msgStatusbarTextTab+': '+IntToStr(Ed.OptTabSize);
  DoStatusbarTextByTag(AStatus, StatusbarTag_TabSize, S);

  //------
  if Ed.ModeOverwrite then
    S:= 'Ovr'
  else
    S:= 'Ins';
  DoStatusbarTextByTag(AStatus, StatusbarTag_InsOvr, S);

  //------
  DoStatusbarTextByTag(AStatus, StatusbarTag_SelMode, IfThen(Ed.OptMouseColumnSelectionWithoutKey, '||', '-'));
  DoStatusbarTextByTag(AStatus, StatusbarTag_WrapMode, msgStatusbarWrapStates[Ord(Ed.OptWrapMode)]);
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
  Adapter: TATAdapterEControl;
begin
  F:= CurrentFrame;

  F.Ed1.UpdateIncorrectCaretPositions;
  F.Ed2.UpdateIncorrectCaretPositions;

  F.Ed1.Update(AUpdatedText);
  F.Ed2.Update(AUpdatedText);

  if AUpdatedText then
  begin
    Adapter:= F.Adapter[F.Ed1];
    Adapter.OnEditorChange(F.Ed1);

    if not F.EditorsLinked then
    begin
      Adapter:= F.Adapter[F.Ed2];
      if Assigned(Adapter) then
        Adapter.OnEditorChange(F.Ed2);
    end;
  end;
end;

procedure TfmMain.UpdateInputForm(Form: TForm; AndHeight: boolean=true);
var
  Ed: TATSynEdit;
  P: TPoint;
begin
  if UiOps.ListboxCentered then
    Form.Position:= poScreenCenter
  else
  begin
    Form.Position:= poDesigned;
    Ed:= CurrentEditor;
    P:= Ed.ClientToScreen(Point(0, 0));
    Form.Left:= P.X+(Ed.Width-Form.Width) div 2;
    Form.Top:= P.Y;
    FixFormPositionToDesktop(Form);
  end;

  if AndHeight then
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
      CurrentFrame.SetFocus;
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

    F.Ed1.Font.Name:= EditorOps.OpFontName;
    F.Ed1.FontItalic.Name:= EditorOps.OpFontName_i;
    F.Ed1.FontBold.Name:= EditorOps.OpFontName_b;
    F.Ed1.FontBoldItalic.Name:= EditorOps.OpFontName_bi;
    F.Ed2.Font.Name:= EditorOps.OpFontName;
    F.Ed2.FontItalic.Name:= EditorOps.OpFontName_i;
    F.Ed2.FontBold.Name:= EditorOps.OpFontName_b;
    F.Ed2.FontBoldItalic.Name:= EditorOps.OpFontName_bi;

    F.Ed1.Font.Size:= EditorOps.OpFontSize;
    F.Ed1.FontItalic.Size:= EditorOps.OpFontSize_i;
    F.Ed1.FontBold.Size:= EditorOps.OpFontSize_b;
    F.Ed1.FontBoldItalic.Size:= EditorOps.OpFontSize_bi;
    F.Ed2.Font.Size:= EditorOps.OpFontSize;
    F.Ed2.FontItalic.Size:= EditorOps.OpFontSize_i;
    F.Ed2.FontBold.Size:= EditorOps.OpFontSize_b;
    F.Ed2.FontBoldItalic.Size:= EditorOps.OpFontSize_bi;

    F.Ed1.Font.Quality:= EditorOps.OpFontQuality;
    F.Ed2.Font.Quality:= EditorOps.OpFontQuality;

    DoOps_LoadOptionsLexerSpecific(F);
  end;

  if Assigned(fmFind) then fmFind.UpdateFonts;
end;


procedure TfmMain.DoApplyFont_Ui;
begin
  Self.Font.Name:= UiOps.VarFontName;
  Self.Font.Size:= UiOps.VarFontSize;
  Self.Font.Color:= GetAppColor('TabFont');

  Groups.SetTabFont(Self.Font);
  if FloatGroups then
  begin
    GroupsF1.SetTabFont(Self.Font);
    GroupsF2.SetTabFont(Self.Font);
    GroupsF3.SetTabFont(Self.Font);
  end;

  Status.Font:= Self.Font;
  StatusAlt.Font:= Self.Font;
  StatusProgress.Font:= Self.Font;

  //dont set font for PanelLeft/PanelBottom, side effects
  CodeTree.Tree.Font.Name:= UiOps.VarFontName;
  CodeTree.Tree.Font.Size:= UiOps.VarFontSize;
  CodeTree.Tree.Font.PixelsPerInch:= Screen.PixelsPerInch;

  CodeTreeFilterInput.Font.Name:= EditorOps.OpFontName;
  CodeTreeFilterInput.Font.Size:= EditorOps.OpFontSize;
  CodeTreeFilterInput.Font.PixelsPerInch:= Screen.PixelsPerInch;

  PanelLeftTitle.Font.Name:= UiOps.VarFontName;
  PanelLeftTitle.Font.Size:= UiOps.VarFontSize;
  PanelLeftTitle.Font.PixelsPerInch:= Screen.PixelsPerInch;

  ATButtonTheme.FontName:= UiOps.VarFontName;
  ATButtonTheme.FontSize:= UiOps.VarFontSize;

  if Assigned(fmFind) then fmFind.UpdateFonts;

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
  SName: string;
begin
  if F=nil then
    SAddStringToHistory('', FListRecents, UiOps.MaxHistoryMenu)
  else
  begin
    if not F.IsText then exit;
    if F.FileName='' then exit;
    if F.NotInRecents then exit;
    SAddStringToHistory(
      SCollapseHomeDirInFilename(F.FileName),
      FListRecents,
      UiOps.MaxHistoryMenu);
    F.DoSaveHistory(F.Ed1);
    if not F.EditorsLinked then
      F.DoSaveHistory(F.Ed2);
  end;

  sub:= mnuFileOpenSub;
  if sub=nil then exit;
  sub.Clear;

  for i:= 0 to FListRecents.Count-1 do
  begin;
    mi:= TMenuItem.Create(Self);
    SName:= IntToStr(i+1)+'.  '+
      ExtractFileName(FListRecents[i])+'  ('+
      ExtractFileDir(FListRecents[i])+')';
    mi.Caption:= StringReplace(SName, '&', '&&', [rfReplaceAll]);
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
  EditorApplyOps(F.Ed1, Op, AForceApply, not F.TabSizeChanged, Groups.Mode=gmOne);
  EditorApplyOps(F.Ed2, Op, AForceApply, not F.TabSizeChanged, Groups.Mode=gmOne);

  F.Adapter[F.Ed1].DynamicHiliteEnabled:= Op.OpLexerDynamicHiliteEnabled;
  F.Adapter[F.Ed1].DynamicHiliteMaxLines:= Op.OpLexerDynamicHiliteMaxLines;

  F.Adapter[F.Ed2].DynamicHiliteEnabled:= Op.OpLexerDynamicHiliteEnabled;
  F.Adapter[F.Ed2].DynamicHiliteMaxLines:= Op.OpLexerDynamicHiliteMaxLines;

  F.Ed1.Update;
  if F.Splitted then
    F.Ed2.Update;

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
  //don't allow Shift+Tab in menu: it blocks tabbing in Find/Replace
  if mi.Shortcut=AppShortcutShiftTab then
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
  //MsgLogConsole('cmd'+inttostr(cmd));
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
    cmd_ToggleFloatSide : DoSave(mi, mnuViewFloatSide_Alt);
    cmd_ToggleFloatBottom : DoSave(mi, mnuViewFloatBottom_Alt);
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
    cmd_Groups6horz : DoSave(mi, mnuGr6H_Alt);
    cmd_Groups6vert : DoSave(mi, mnuGr6V_Alt);
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
  UpdateMenuItemHotkey(mnuSelExtWord, cmd_SelectExpandToWord);
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
  UpdateMenuItemHotkey(mnuGr2V, cmd_Groups2vert);
  UpdateMenuItemHotkey(mnuGr2H, cmd_Groups2horz);
  UpdateMenuItemHotkey(mnuGr3V, cmd_Groups3vert);
  UpdateMenuItemHotkey(mnuGr3H, cmd_Groups3horz);
  UpdateMenuItemHotkey(mnuGr1p2V, cmd_Groups3plus);
  UpdateMenuItemHotkey(mnuGr1p2H, cmd_Groups3plushorz);
  UpdateMenuItemHotkey(mnuGr4V, cmd_Groups4vert);
  UpdateMenuItemHotkey(mnuGr4H, cmd_Groups4horz);
  UpdateMenuItemHotkey(mnuGr4G, cmd_Groups4grid);
  UpdateMenuItemHotkey(mnuGr6V, cmd_Groups6vert);
  UpdateMenuItemHotkey(mnuGr6H, cmd_Groups6horz);
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
  UpdateMenuItemHotkey(mnuViewSidebar, cmd_ToggleSidebar);
  UpdateMenuItemHotkey(mnuViewSide, cmd_ToggleSidePanel);
  UpdateMenuItemHotkey(mnuViewBottom, cmd_ToggleBottomPanel);
  UpdateMenuItemHotkey(mnuViewFloatSide, cmd_ToggleFloatSide);
  UpdateMenuItemHotkey(mnuViewFloatBottom, cmd_ToggleFloatBottom);
  UpdateMenuItemHotkey(mnuViewToolbar, cmd_ToggleToolbar);
  UpdateMenuItemHotkey(mnuViewStatus, cmd_ToggleStatusbar);
  UpdateMenuItemHotkey(mnuViewOnTop, cmd_ToggleOnTop);

  UpdateMenuItemHotkey(mnuOpDefault, cmd_OpsOpenDefault);
  UpdateMenuItemHotkey(mnuOpUser, cmd_OpsOpenUser);
  UpdateMenuItemHotkey(mnuOpDefaultUser, cmd_OpsOpenDefaultAndUser);
  UpdateMenuItemHotkey(mnuOpLexer, cmd_OpsOpenLexerSpecific);
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
  CmdItem: TAppCommand;
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
    //usual lexers
    for i:= 0 to AppManager.LexerCount-1 do
      sl.AddObject('lexer: '+AppManager.Lexers[i].LexerName,
                   TObject(cmdFirstLexerCommand+i));
    //lite lexers
    for i:= 0 to AppManagerLite.LexerCount-1 do
      sl.AddObject('lexer: '+AppManagerLite.Lexers[i].LexerName+msgLiteLexerSuffix,
                   TObject(cmdFirstLexerCommand+i+AppManager.LexerCount));

    sl.Sort;

    //insert "none" at list begin
    sl.InsertObject(0, 'lexer: '+msgNoLexer, TObject(cmdLastLexerCommand));

    for i:= 0 to sl.count-1 do
      AppKeymap.Add(PtrInt(sl.Objects[i]), sl[i], [], []);
  finally
    FreeAndNil(sl);
  end;

  //dynamic plugin items
  for i:= 0 to AppCommandList.Count-1 do
  begin
    CmdItem:= TAppCommand(AppCommandList[i]);
    if CmdItem.ItemModule='' then Break;
    if SEndsWith(CmdItem.ItemCaption, '-') then Continue;

    Str:= CmdItem.ItemCaption;
    Str:= StringReplace(Str, '\', ': ', [rfReplaceAll]);
    Str:= StringReplace(Str, '&', '', [rfReplaceAll]);

    AppKeymap.Add(
      cmdFirstPluginCommand+i,
      'plugin: '+Str,
      [], []);
  end;
end;


procedure TfmMain.UpdateMenuItemChecked(mi: TMenuItem; saved: TATMenuItemsAlt; AValue: boolean);
begin
  if Assigned(mi) then mi.Checked:= AValue;
  if Assigned(saved.item0) then saved.item0.Checked:= AValue;
  if Assigned(saved.item1) then saved.item1.Checked:= AValue;
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
  UpdateMenuItemChecked(mnuViewSidebar, mnuViewSidebar_Alt, ShowSideBar);
  UpdateMenuItemChecked(mnuViewSide, mnuViewSide_Alt, ShowSidePanel);
  UpdateMenuItemChecked(mnuViewBottom, mnuViewBottom_Alt, ShowBottom);
  UpdateMenuItemChecked(mnuViewFloatSide, mnuViewFloatSide_Alt, FloatSide);
  UpdateMenuItemChecked(mnuViewFloatBottom, mnuViewFloatBottom_Alt, FloatBottom);
  UpdateMenuItemChecked(mnuViewOnTop, mnuViewOnTop_Alt, ShowOnTop);

  UpdateMenuItemChecked(mnuGr1, mnuGr1_Alt, Groups.Mode= gmOne);
  UpdateMenuItemChecked(mnuGr2V, mnuGr2V_Alt, Groups.Mode= gm2v);
  UpdateMenuItemChecked(mnuGr2H, mnuGr2H_Alt, Groups.Mode= gm2h);
  UpdateMenuItemChecked(mnuGr3V, mnuGr3V_Alt, Groups.Mode= gm3v);
  UpdateMenuItemChecked(mnuGr3H, mnuGr3H_Alt, Groups.Mode= gm3h);
  UpdateMenuItemChecked(mnuGr1p2V, mnuGr1p2V_Alt, Groups.Mode= gm1plus2v);
  UpdateMenuItemChecked(mnuGr1p2H, mnuGr1p2H_Alt, Groups.Mode= gm1plus2h);
  UpdateMenuItemChecked(mnuGr4V, mnuGr4V_Alt, Groups.Mode= gm4v);
  UpdateMenuItemChecked(mnuGr4H, mnuGr4H_Alt, Groups.Mode= gm4h);
  UpdateMenuItemChecked(mnuGr4G, mnuGr4G_Alt, Groups.Mode= gm4Grid);
  UpdateMenuItemChecked(mnuGr6V, mnuGr6V_Alt, Groups.Mode= gm6v);
  UpdateMenuItemChecked(mnuGr6H, mnuGr6H_Alt, Groups.Mode= gm6h);
  UpdateMenuItemChecked(mnuGr6, mnuGr6_Alt, Groups.Mode= gm6Grid);
end;

procedure TfmMain.UpdateTreeContents; inline;
begin
  TimerTreeFill.Enabled:= false;
  TimerTreeFill.Enabled:= true;
end;

procedure TfmMain.UpdateTreeFilter; inline;
begin
  CodeTreeFilter.InvalidateFilter;
end;

procedure TfmMain.UpdateTree(AFill: boolean; AConsiderTreeVisible: boolean;
  AForceUpdateAll: boolean);
var
  Frame: TEditorFrame;
  Ed: TATSynEdit;
  Tree: TTreeView;
begin
  if UiOps.TreeShowIcons then
    CodeTree.Tree.Images:= ImageListTree
  else
    CodeTree.Tree.Images:= nil;

  if AConsiderTreeVisible then
    if not ShowSidePanel then exit;

  Frame:= CurrentFrame;
  if Frame=nil then exit;
  Ed:= Frame.Editor;

  if not Frame.EnabledCodeTree[Ed] then
  begin
    //don't clear tree, it may be filled by plugin
    UpdateTreeSelection(Frame, Ed); //needed even for disabled CodeTree (for plugins)
    exit;
  end;

  if (Ed.Strings.Count=0) or
    (Ed.Strings.Count>UiOps.MaxLinesForTree) then
  begin
    ClearTreeviewWithData(CodeTree.Tree);
    exit;
  end;

  while Frame.Adapter[Ed].TreeBusy do
  begin
    Application.ProcessMessages;
    if Application.Terminated then exit;
  end;

  if AForceUpdateAll then
  begin
    Frame.Adapter[Ed].DoAnalize(Ed, true);
    Ed.DoEventChange;
  end;

  if AFill then
  begin
    Tree:= Frame.CachedTreeview[Ed];
    Frame.Adapter[Ed].TreeFill(Tree);

    //Lazarus dont sort new items
    if CodeTree.Tree.SortType<>stNone then
      Tree.AlphaSort;

    DoTreeviewCopy(Tree, CodeTree.Tree);
    CodeTree.SourceObject:= Ed;

    //force update scrollbar
    CodeTree.Tree.Perform(CM_CHANGED, 0, 0);
  end;

  UpdateTreeFilter;
  UpdateTreeSelection(Frame, Ed);
end;

procedure TfmMain.UpdateTreeSelection(Frame: TEditorFrame; Ed: TATSynEdit);
var
  Caret: TATCaretItem;
begin
  if not UiOps.TreeAutoSync then exit;

  if Ed.Carets.Count=0 then exit;
  Caret:= Ed.Carets[0];

  CodetreeSelectItemForPosition(
    CodeTree.Tree,
    Point(Caret.PosX, Caret.PosY)
    );
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
  bAutoSize, bStretch: boolean;
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
    bStretch:= false;

    SItem:= SGetItem(SPanel);
    case SItem of
      'caret':   begin NTag:= StatusbarTag_Caret; end;
      'enc':     begin NTag:= StatusbarTag_Enc; end;
      'ends':    begin NTag:= StatusbarTag_LineEnds; end;
      'lexer':   begin NTag:= StatusbarTag_Lexer; end;
      'tabsize': begin NTag:= StatusbarTag_TabSize; end;
      'ins':     begin NTag:= StatusbarTag_InsOvr; end;
      'msg':     begin NTag:= StatusbarTag_Msg; bStretch:= true; end;
      'selmode': begin NTag:= StatusbarTag_SelMode; end;
      'wrap':    begin NTag:= StatusbarTag_WrapMode; end;
      else Continue;
    end;

    SItem:= SGetItem(SPanel);
    bAutoSize:= SItem='A';
    if bAutoSize then
      Al:= taLeftJustify
    else
      Al:= AppStringToAlignment(SItem);

    SItem:= SGetItem(SPanel);
    NSize:= Max(16, StrToIntDef(SItem, 200));

    Status.AddPanel(-1, NSize, Al, '', -1, NTag, bAutoSize, bStretch);
  until false;
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
    F.Ed1.Invalidate;
    F.Ed2.Invalidate;
    F.Splitter.Invalidate;
  end;

  Groups.Splitter1.Invalidate;
  Groups.Splitter2.Invalidate;
  Groups.Splitter3.Invalidate;
  Groups.Splitter4.Invalidate;
  Groups.Splitter5.Invalidate;
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
  mi.RadioItem:= true;
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
    mi.RadioItem:= true;
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

  ToolbarMain.AddDropdown(12, PopupToolbarCase);
  ToolbarMain.AddDropdown(13, PopupToolbarSort);
  ToolbarMain.AddDropdown(14, PopupToolbarComment);

  ToolbarMain.UpdateControls;
end;


const
  cSidebarIconTree     = 0;
  cSidebarIconConsole  = 1;
  cSidebarIconOutput   = 2;
  cSidebarIconValidate = 3;
  cSidebarIconFind     = 4;
  cSidebarIconMenu     = 5;

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
  UpdateImagelistWithIconFromFile(ImageListSide, Str+'menu.png');

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

  CodeTree.Tree.Invalidate;
end;

procedure TfmMain.DoOps_LoadToolBarIcons;
var
  Parts: TRegexParts;
  Str: string;
begin
  SRegexFindParts('.+_(\d+)x(\d+)', UiOps.ToolBarTheme, Parts);

  Str:= GetAppPath(cDirDataToolBarIcons)+DirectorySeparator;

  if not DirectoryExists(Str) then Exit;

  ImageListToolbar.Width:= StrToIntDef(Parts[1].Str, ImageListToolbar.Width);
  ImageListToolbar.Height:= StrToIntDef(Parts[2].Str, ImageListToolbar.Height);
  ImageListToolbar.Clear;

  UpdateImagelistWithIconFromFile(ImageListToolbar, Str+'f_new.png');
  UpdateImagelistWithIconFromFile(ImageListToolbar, Str+'f_open.png');
  UpdateImagelistWithIconFromFile(ImageListToolbar, Str+'f_save.png');
  UpdateImagelistWithIconFromFile(ImageListToolbar, Str+'e_cut.png');
  UpdateImagelistWithIconFromFile(ImageListToolbar, Str+'e_copy.png');
  UpdateImagelistWithIconFromFile(ImageListToolbar, Str+'e_paste.png');
  UpdateImagelistWithIconFromFile(ImageListToolbar, Str+'e_undo.png');
  UpdateImagelistWithIconFromFile(ImageListToolbar, Str+'e_redo.png');
  UpdateImagelistWithIconFromFile(ImageListToolbar, Str+'unpri.png');
  UpdateImagelistWithIconFromFile(ImageListToolbar, Str+'map.png');
  UpdateImagelistWithIconFromFile(ImageListToolbar, Str+'indent.png');
  UpdateImagelistWithIconFromFile(ImageListToolbar, Str+'unindent.png');
  UpdateImagelistWithIconFromFile(ImageListToolbar, Str+'case.png');
  UpdateImagelistWithIconFromFile(ImageListToolbar, Str+'sort.png');
  UpdateImagelistWithIconFromFile(ImageListToolbar, Str+'comment.png');

  ToolbarMain.UpdateControls;
end;


procedure TfmMain.InitSidebar;
begin
  //
  ToolbarSideTop.AddButton(cSidebarIconMenu, @DoSidebar_MainMenuClick, msgPanelMenu_Init, msgPanelMenu_Init, '', UiOps.ShowSidebarCaptions);
  ToolbarSideTop.AddButton(cSidebarIconTree, @DoSidebar_OnTabClick, msgPanelTree_Init, msgPanelTree_Init, '', UiOps.ShowSidebarCaptions);
  //
  ToolbarSideLow.AddButton(cSidebarIconConsole, @DoBottom_OnTabClick, msgPanelConsole_Init, msgPanelConsole_Init, '', UiOps.ShowSidebarCaptions);
  ToolbarSideLow.AddButton(cSidebarIconOutput, @DoBottom_OnTabClick, msgPanelOutput_Init, msgPanelOutput_Init, '', UiOps.ShowSidebarCaptions);
  ToolbarSideLow.AddButton(cSidebarIconValidate, @DoBottom_OnTabClick, msgPanelValidate_Init, msgPanelValidate_Init, '', UiOps.ShowSidebarCaptions);
  //
  ToolbarSideMid.AddButton(cSidebarIconFind, @DoBottom_FindClick, msgPanelSearch_Init, msgPanelSearch_Init, '', UiOps.ShowSidebarCaptions);
  ToolbarSideMid.AddSep;
end;


procedure TfmMain.UpdateMenuTabsize;
const
  cMinSize = 1;
  cMaxSize = 10;
var
  SPrefix: string;
  mi: TMenuItem;
  i: integer;
begin
  with TIniFile.Create(GetAppLangFilename) do
    SPrefix:= ReadString('ct', 'tsz', 'Tab size:');

  for i:= cMaxSize downto cMinSize do
  begin
    mi:= TMenuItem.Create(Self);
    mi.Caption:= SPrefix+' '+IntToStr(i);
    mi.Tag:= i;
    mi.OnClick:= @MenuTabsizeClick;
    PopupTabSize.Items.Insert(0, mi);
  end;
end;


procedure TfmMain.UpdateMenuPicScale;
const
  cSizes: array[0..8] of integer =
    (-1, 33, 50, 100, 150, 200, 500, 1000, 1500);
var
  mi: TMenuItem;
  i: integer;
begin
  for i:= Low(cSizes) to High(cSizes) do
  begin
    mi:= TMenuItem.Create(Self);
    if cSizes[i]=-1 then
      mi.Caption:= 'default'
    else
      mi.Caption:= IntToStr(cSizes[i])+'%';
    mi.Tag:= cSizes[i];
    mi.OnClick:= @MenuPicScaleClick;
    PopupPicScale.Items.Add(mi);
  end;
end;

