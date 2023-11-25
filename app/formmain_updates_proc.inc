(*
This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at http://mozilla.org/MPL/2.0/.

Copyright (c) Alexey Torgashin
*)
{$ifdef nn}begin end;{$endif}

procedure TfmMain.UpdateCaption;
begin
  // before, we only set FNeedUpdateCaption to True, so TimerAppIdle
  // did the work. now, we update caption immediately.
  //FNeedUpdateCaption:= true;

  UpdateCaption_RealWork;
end;

procedure TfmMain.UpdateCaption_RealWork;
var
  F: TEditorFrame;
  SName, SSession, SSessionRootPath,
  Name1, Name2: string;
begin
  F:= CurrentFrame;
  if F=nil then exit;

  if UiOps.ShowTitlePath then
  begin
    if F.EditorsLinked then
    begin
      if F.FileName<>'' then
        SName:= AppCollapseHomeDirInFilename(F.FileName)
      else
        SName:= F.TabCaption;
      SName:= msgModified[F.Modified]+SName;
    end
    else
    begin
      Name1:= AppCollapseHomeDirInFilename(F.GetFileName(F.Ed1));
      Name2:= AppCollapseHomeDirInFilename(F.GetFileName(F.Ed2));
      if Name1='' then Name1:= msgUntitledTab;
      if Name2='' then Name2:= msgUntitledTab;
      Name1:= msgModified[F.Ed1.Modified]+Name1;
      Name2:= msgModified[F.Ed2.Modified]+Name2;
      SName:= Name1+' | '+Name2;
    end;
  end
  else
  begin
    SName:= F.TabCaption;
    SName:= msgModified[F.Modified]+SName;
  end;

  SSplitByChar(AppSessionName, '|', SSession, SSessionRootPath);
  SSession:= ChangeFileExt(ExtractFileName(SSession), '');
  if (SSession<>'') and (SSession<>'history session') then
  begin
    if SBeginsWith(SSessionRootPath, '/sessions/') then
      Delete(SSessionRootPath, 1, Length('/sessions/'));
    SSession:= ' {'+SSession+IfThen(SSessionRootPath<>'', ':'+SSessionRootPath)+'}'
  end
  else
    SSession:= '';

  if SName='' then //CudaText #3280
  begin
    if F.FileName<>'' then
      SName:= ExtractFileName(F.FileName)
    else
      SName:= msgUntitledTab;
  end;

  Caption:= SName + SSession + (' - ' + msgTitle);

  Application.Title:= F.TabCaption + (' - ' + msgTitle);
end;


procedure TfmMain.UpdateToolbarButton(AToolbar: TATFlatToolbar;
  ACmd: integer; AChecked, AEnabled: boolean);
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
      Btn.Enabled:= AEnabled;
      exit
    end;
  end;
end;


procedure TfmMain.UpdateStatusbar;
begin
  TimerStatusWork.Enabled:= false;
  TimerStatusWork.Enabled:= true;
end;

procedure TfmMain.UpdateSidebarButtonFind;
begin
  if ToolbarSideMid.ButtonCount>0 then
    ToolbarSideMid.Buttons[0].Checked:= Assigned(fmFind) and fmFind.Visible;
end;

procedure TfmMain.UpdateToolbarButtons(F: TEditorFrame);
var
  Ed: TATSynEdit;
  bEditor, bEditable, bBinViewer: boolean;
  bShowNonPrinted: boolean;
begin
  if F=nil then exit;
  Ed:= F.Editor;

  case F.FrameKind of
    efkEditor:
      begin
        bEditor:= true;
        bEditable:= not Ed.ModeReadOnly;
        bBinViewer:= false;
        bShowNonPrinted:= Ed.OptUnprintedVisible;
      end;
    efkBinaryViewer:
      begin
        bEditor:= false;
        bEditable:= false;
        bBinViewer:= true;
        bShowNonPrinted:= F.Binary.TextNonPrintable;
      end;
    efkImageViewer:
      begin
        bEditor:= false;
        bEditable:= false;
        bBinViewer:= false;
        bShowNonPrinted:= false;
      end
    else
      raise Exception.Create('Unknown FrameKind');
  end;

  UpdateToolbarButton(ToolbarMain, cmd_FileSave, false, bEditor);
  UpdateToolbarButton(ToolbarMain, cCommand_ClipboardCopy, false, bEditor or bBinViewer);
  UpdateToolbarButton(ToolbarMain, cCommand_ClipboardCut, false, bEditable);
  UpdateToolbarButton(ToolbarMain, cCommand_ClipboardPaste, false, bEditable);
  UpdateToolbarButton(ToolbarMain, cCommand_Undo, false, bEditable and not Ed.Strings.UndoEmpty);
  UpdateToolbarButton(ToolbarMain, cCommand_Redo, false, bEditable and not Ed.Strings.RedoEmpty);
  UpdateToolbarButton(ToolbarMain, cCommand_TextIndent, false, bEditable);
  UpdateToolbarButton(ToolbarMain, cCommand_TextUnindent, false, bEditable);

  UpdateToolbarButton(ToolbarMain, cCommand_ToggleOverwrite, Ed.ModeOverwrite, bEditor);
  UpdateToolbarButton(ToolbarMain, cCommand_ToggleReadOnly, Ed.ModeReadOnly, bEditor);
  UpdateToolbarButton(ToolbarMain, cCommand_ToggleWordWrap, Ed.OptWrapMode<>TATEditorWrapMode.ModeOff, bEditor);
  UpdateToolbarButton(ToolbarMain, cCommand_ToggleMinimap, Ed.OptMinimapVisible, bEditor);
  UpdateToolbarButton(ToolbarMain, cCommand_ToggleMicromap, Ed.OptMicromapVisible, bEditor);
  UpdateToolbarButton(ToolbarMain, cCommand_ToggleRuler, Ed.OptRulerVisible, bEditor);
  UpdateToolbarButton(ToolbarMain, cCommand_ToggleUnprinted, bShowNonPrinted, bEditor or bBinViewer);

  {
  //not needed yet
  UpdateToolbarButton(ToolbarMain, cCommand_ToggleLineNums, Ed.Gutter.Items[Ed.Gutter.FindIndexByTag(ATEditorOptions.GutterTagNumbers)].Visible, bEditor);
  UpdateToolbarButton(ToolbarMain, cCommand_ToggleFolding, Ed.Gutter.Items[Ed.Gutter.FindIndexByTag(ATEditorOptions.GutterTagFolding)].Visible, bEditor);
  }

  UpdateToolbarButton(ToolbarMain, cmd_ToggleFileNotifications, UiOps.NotificationEnabled, true);
end;

procedure TfmMain.UpdateStatusbar_RealWork;
var
  F: TEditorFrame;
begin
  F:= CurrentFrame;
  if F=nil then exit;

  UpdateStatusbar_ForFrame(Status, F);
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
  DoStatusbarHintByTag(Status, StatusbarTag_Zoom, msgStatusbarHintZoom);
end;

procedure TfmMain.UpdateStatusbar_ForFrame(AStatus: TATStatus; F: TEditorFrame);
var
  Ed: TATSynEdit;
  S, fmt: string;
  Size: TPoint;
  N: integer;
  FrameKind: TAppFrameKind;
begin
  if F=nil then exit;
  Ed:= F.Editor;
  FrameKind:= F.FrameKind;

  //hide hints for pictures/viewer
  AStatus.ShowHint:= FrameKind=efkEditor;

  if FrameKind<>efkEditor then
  begin
    case FrameKind of
      efkBinaryViewer:
        S:= IntToStr(F.Binary.PosPercent)+'%';
      else
        S:= '';
    end;
    DoStatusbarTextByTag(AStatus, StatusbarTag_Caret, S);

    case FrameKind of
      efkBinaryViewer:
        S:= cEncConvNames[F.Binary.TextEncoding];
      else
        S:= '';
    end;
    DoStatusbarTextByTag(AStatus, StatusbarTag_Enc, S);

    DoStatusbarTextByTag(AStatus, StatusbarTag_LineEnds, '');

    case FrameKind of
      efkBinaryViewer:
        S:= msgViewer+': '+msgViewerModes[F.Binary.Mode];
      efkImageViewer:
        begin
          Size:= F.PictureSizes;
          S:= Format(msgStatusPictureNxN, [Size.X, Size.Y]);
        end;
    end;
    DoStatusbarTextByTag(AStatus, StatusbarTag_Lexer, S);

    case FrameKind of
      efkBinaryViewer:
        S:= '';
      efkImageViewer:
        S:= IntToStr(F.PictureScale)+'%';
    end;
    DoStatusbarTextByTag(AStatus, StatusbarTag_TabSize, S);

    DoStatusbarTextByTag(AStatus, StatusbarTag_InsOvr, '');
    exit;
  end;

  //------
  case EditorGetSelectionKind(Ed) of
    selkindSmallSel:
      fmt:= UiOps.StatusSmallSel;
    selkindStreamSel:
      fmt:= UiOps.StatusStreamSel;
    selkindColumnSel:
      fmt:= UiOps.StatusColSel;
    selkindCarets:
      fmt:= UiOps.StatusCarets;
    else
      fmt:= UiOps.StatusNoSel;
  end;
  DoStatusbarTextByTag(AStatus, StatusbarTag_Caret, EditorFormatStatus(Ed, fmt));

  //------
  S:= Ed.EncodingName;
  DoStatusbarTextByTag(AStatus, StatusbarTag_Enc, S);

  //------
  case Ed.Strings.Endings of
    TATLineEnds.Windows: S:= msgEndWin;
    TATLineEnds.Unix: S:= msgEndUnix;
    TATLineEnds.Mac: S:= msgEndMac;
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
  S:= msgStatusbarCellInsOvr[Ed.ModeOverwrite];
  DoStatusbarTextByTag(AStatus, StatusbarTag_InsOvr, S);

  //------
  S:= msgStatusbarCellColumnMarks[Ed.OptMouseColumnSelectionWithoutKey];
  if Ed.ModeReadOnly then
    S+= ' RO';
  DoStatusbarTextByTag(AStatus, StatusbarTag_SelMode, S);

  //------
  DoStatusbarTextByTag(AStatus, StatusbarTag_WrapMode, msgStatusbarWrapStates[Ord(Ed.OptWrapMode)]);

  N:= Ed.OptScaleFont;
  if N=0 then N:= 100;
  DoStatusbarTextByTag(AStatus, StatusbarTag_Zoom, IntToStr(N)+'%');

  //to update '[Read only]' in the message
  DoStatusbarTextByTag(AStatus, StatusbarTag_Msg, GetStatusbarPrefix(F)+FLastStatusbarMessage);
end;

procedure TfmMain.InitStatusProgress;
begin
  if not Assigned(StatusProgress) then
  begin
    StatusProgress:= TATGauge.Create(Self);
    with StatusProgress do
    begin
      Visible:= false;
      Parent:= Status;
      AnchorSideRight.Control:= Status;
      AnchorSideRight.Side:= asrBottom;
      AnchorSideBottom.Control:= Status;
      AnchorSideBottom.Side:= asrBottom;
      AnchorSideTop.Control:= Status;
      AnchorSideTop.Side:= asrTop;
      Anchors:= [akRight, akTop, akBottom];
      BorderSpacing.Around:= 2;
      MinValue:= 0;
      MaxValue:= 100;
      Progress:= 0;
    end;
  end;

  StatusProgress.DoubleBuffered:= UiOps.DoubleBuffered;
  StatusProgress.Width:= ATEditorScale(UiOps.ProgressbarWidth);
  StatusProgress.Font.Assign(Self.Font);
end;

procedure TfmMain.InitButtonCancel;
begin
  if not Assigned(ButtonCancel) then
  begin
    InitStatusProgress;
    ButtonCancel:= TATButton.Create(Self);
    with ButtonCancel do
    begin
      Visible:= false;
      Parent:= Status;
      OnClick:= @ButtonCancelClick;
      AnchorSideRight.Control:= StatusProgress;
      AnchorSideBottom.Control:= StatusProgress;
      AnchorSideBottom.Side:= asrBottom;
      AnchorSideTop.Control:= StatusProgress;
      Anchors:= [akRight, akTop, akBottom];
      BorderSpacing.Right:= 2;
    end;
  end;

  ButtonCancel.DoubleBuffered:= UiOps.DoubleBuffered;
  ButtonCancel.Caption:= msgButtonCancel;
  ButtonCancel.Width:= ATEditorScale(UiOps.ProgressbarWidth);
end;

procedure TfmMain.UpdateCurrentFrame(AUpdatedText: boolean = false); inline;
begin
  UpdateFrameEx(CurrentFrame, AUpdatedText);
end;

procedure TfmMain.UpdateFrameEx(F: TEditorFrame; AUpdatedText: boolean); inline;
begin
  if Assigned(F) then
    F.UpdateFrame(AUpdatedText);
end;

procedure TfmMain.UpdateInputForm(Form: TForm; AndHeight: boolean=true);
var
  Ed: TATSynEdit;
  P: TPoint;
begin
  if UiOps.ListboxCentered then
  begin
    Form.Position:= poDesigned;
    P:= Frames[0].ClientToScreen(Point(0, 0));

    if Assigned(FFormTooltip) and FFormTooltip.Visible then
      Inc(P.Y, UiOps.TabHeight*2);

    Form.Left:= Left + Width div 2 - Form.Width div 2;
    Form.Top:= P.Y;
    FixFormPositionToDesktop(Form);
  end
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
    Form.Height:= ATEditorScale(UiOps.ListboxSizeY);
end;


procedure TfmMain.UpdateEnabledAll(b: boolean);
begin
  Groups.Enabled:= b;

  //fixes issue #5125
  AppPanels[cPaneOut].PanelGrouper.Enabled:= b;
  AppPanels[cPaneSide].PanelGrouper.Enabled:= b;

  if Assigned(fmFind) then
    fmFind.Enabled:= b;
end;

procedure TfmMain.UpdateAppForSearch(AStart, AEdLock, AFindMode, AUpdateEnableAll, AFocusFindDlg: boolean);
var
  Frame: TEditorFrame;
  Ed: TATSynEdit;
begin
  Frame:= CurrentFrame;
  if Frame=nil then exit;
  Ed:= Frame.Editor;

  if AStart then
  begin
    FFinder.Editor:= Ed;
    FFindStop:= false;
    FFindConfirmAll:= mrNone;
    InitButtonCancel;
    ButtonCancel.Show;
    UpdateGlobalProgressbar(0, true);
    if AUpdateEnableAll then
      UpdateEnabledAll(false);
    if AEdLock then
    begin
      Ed.BeginUpdate;
      Ed.Enabled:= false;
    end;
  end
  else
  begin
    if Assigned(ButtonCancel) then
      ButtonCancel.Hide;
    UpdateGlobalProgressbar(0, false);
    if AUpdateEnableAll then
      UpdateEnabledAll(true);
    //if AEdLock then //works ok even without this check
    begin
      Ed.Enabled:= true;
      Ed.EndUpdate;
    end;

    if Assigned(fmFind) and fmFind.Visible and fmFind.Enabled then
    begin
      if AFocusFindDlg then
        fmFind.UpdateFocus(AFindMode);
      //else
      //  raise Exception.Create('unexpected AFocusFindDlg=False');
    end
    else
      Frame.SetFocus;
  end;

  ////removed to fix issue:
  ////search fails, but OnCaret fired and Highlight Occurrences gives statusbar message "hilited N matches"
  //Ed.DoEventCarets;
end;


procedure TfmMain.DoApplyFont_Text;
var
  F: TEditorFrame;
  i: integer;
begin
  if Assigned(fmConsole) then
  begin
    fmConsole.Font.Name:= EditorOps.OpFontName;
    fmConsole.Font.Size:= EditorOps.OpFontSize;
    fmConsole.Font.Quality:= EditorOps.OpFontQuality;
    fmConsole.EdInput.Font:= fmConsole.Font;
    fmConsole.EdMemo.Font:= fmConsole.Font;
  end;

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

    DoOps_LoadOptionsLexerSpecific(F, F.Ed1);
    if not F.EditorsLinked then
      DoOps_LoadOptionsLexerSpecific(F, F.Ed2);
  end;

  if Assigned(fmFind) then
    fmFind.UpdateFonts;

  fmValidate.Ed.Font.Name:= EditorOps.OpFontName;
  fmValidate.Ed.Font.Size:= EditorOps.OpFontSize;
  fmValidate.Ed.Font.Quality:= EditorOps.OpFontQuality;
end;

procedure TfmMain.DoApplyFont_Ui;
var
  id: TAppPanelId;
begin
  Self.Font.Name:= UiOps.VarFontName;
  Self.Font.Size:= ATEditorScaleFont(UiOps.VarFontSize);
  Self.Font.Color:= GetAppColor(apclTabFont);

  Groups.SetTabFont(Self.Font);
  if FloatGroups then
  begin
    GroupsF1.SetTabFont(Self.Font);
    GroupsF2.SetTabFont(Self.Font);
    GroupsF3.SetTabFont(Self.Font);
  end;

  //dont set font for PanelLeft/PanelBottom, id effects
  CodeTree.Tree.Font.Name:= UiOps.VarFontName;
  CodeTree.Tree.Font.Size:= ATEditorScaleFont(UiOps.VarFontSize) * UiOps.TreeFontScale div 100;

  CodeTreeFilterInput.Font.Name:= EditorOps.OpFontName;
  CodeTreeFilterInput.Font.Size:= EditorOps.OpFontSize;

  for id in TAppPanelId do
    if id<>cPaneNone then
      with AppPanels[id] do
      begin
        PanelTitle.Font.Name:= UiOps.VarFontName;
        PanelTitle.Font.Size:= ATEditorScaleFont(UiOps.VarFontSize);
      end;

  ATFlatTheme.FontName:= UiOps.VarFontName;
  ATFlatTheme.FontSize:= UiOps.VarFontSize;
  ATFlatTheme.MonoFontName:= EditorOps.OpFontName;
  ATFlatTheme.MonoFontSize:= EditorOps.OpFontSize;
  //ATFlatTheme.FontQuality:= EditorOps.OpFontQuality; //not helpful
  ATFlatTheme.ScalePercents:= ATEditorScalePercents;
  ATFlatTheme.ScaleFontPercents:= ATEditorScaleFontPercents;

  if Assigned(fmFind) then
    fmFind.UpdateFonts;

  DoApplyFont_UiStatusbar;
end;

procedure TfmMain.DoApplyFont_UiStatusbar;
begin
  UpdateThemeStatusbar;
  Status.HeightInitial:= Max(UiOps.StatusHeightMin, ATEditorScale(AppThemeStatusbar.FontSize * UiOps.StatusHeightPercents div 100));
  Status.Height:= Status.HeightInitial;
end;


procedure TfmMain.DoApplyFont_Output;
begin
  fmOutput.Ed.Font.Name:= UiOps.OutputFontName;
  fmOutput.Ed.Font.Size:= UiOps.OutputFontSize;
  fmOutput.Ed.Update;
end;

procedure TfmMain.UpdateMenuRecents(sub: TMenuItem);
var
  mi: TMenuItem;
  SName, SAccel: string;
  i: integer;
begin
  if sub=nil then exit;
  sub.Clear;

  for i:= 0 to AppListRecents.Count-1 do
  begin;
    if i<9 then
      SAccel:= '&'+IntToStr(i+1)+'.  '
    else
    if i<10+Ord('Z')-Ord('A') then
      SAccel:= '&'+Chr(i-9+Ord('A'))+'.  '
    else
      SAccel:= '';
    SName:=
      ExtractFileName(AppListRecents[i])+'  ('+
      ExtractFileDir(AppListRecents[i])+')';
    SName:= StringReplace(SName, '&', '&&', [rfReplaceAll]);

    mi:= TMenuItem.Create(sub);
    mi.Caption:= SAccel + SName;
    mi.Tag:= i;
    mi.OnClick:= @MenuRecentItemClick;
    sub.Add(mi);
  end;

  mi:= TMenuItem.Create(Self);
  mi.Caption:= '-';
  sub.Add(mi);

  mi:= TMenuItem.Create(Self);
  mi.Caption:= msgFileClearList;
  mi.OnClick:= @MenuRecentsClear;
  mi.Enabled:= AppListRecents.Count>0;
  sub.Add(mi);
end;

procedure TfmMain.UpdateMenuRecent(Ed: TATSynEdit);
var
  Frame: TEditorFrame;
  SFileName: string;
begin
  //Ed=nil means: don't add string item, but limit the max count in the list (param MaxItems)
  if Ed=nil then
  begin
    SAddStringToHistory('', AppListRecents, UiOps.MaxHistoryMenu);
    exit;
  end;

  Frame:= TGroupsHelper.GetEditorFrame(Ed);
  if Frame=nil then exit;

  SFileName:= Frame.FileName;
  if SFileName='' then exit;

  if Frame.FrameKind=efkEditor then
    Frame.DoSaveHistory(Ed);

  if (not Frame.InSession) or IsDefaultSessionActive then
    SAddStringToHistory(
      AppCollapseHomeDirInFilename(SFileName),
      AppListRecents,
      UiOps.MaxHistoryMenu);
end;

procedure TfmMain.PopupRecentsPopup(Sender: TObject);
begin
  UpdateMenuRecents(PopupRecents.Items);
end;

procedure TfmMain.MenuRecentsPopup(Sender: TObject);
begin
  UpdateMenuRecents(mnuFileOpenSub);
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
    DoOps_LoadOptionsLexerSpecific(F, F.Ed1);
    if not F.EditorsLinked then
      DoOps_LoadOptionsLexerSpecific(F, F.Ed2);
  end;

  if Assigned(fmFind) then
  begin
    fmFind.edFind.OptMouseMiddleClickAction:= TATEditorMiddleClickAction(EditorOps.OpMouseMiddleClickAction);
    fmFind.edRep.OptMouseMiddleClickAction:= TATEditorMiddleClickAction(EditorOps.OpMouseMiddleClickAction);
  end;

  if Assigned(fmConsole) then
  begin
    fmConsole.EdInput.OptMouseMiddleClickAction:= TATEditorMiddleClickAction(EditorOps.OpMouseMiddleClickAction);
  end;

  if Assigned(CodeTreeFilterInput) then
  begin
    CodeTreeFilterInput.OptMouseMiddleClickAction:= TATEditorMiddleClickAction(EditorOps.OpMouseMiddleClickAction);
  end;

  UpdateStatusbar;
end;

procedure TfmMain.DoApplyFrameOps(F: TEditorFrame; const Op: TEditorOps; AForceApply: boolean);
begin
  if F=nil then exit;
  EditorApplyOps(F.Ed1, Op, AForceApply, not F.TabSizeChanged, Groups.Mode=gmOne);
  EditorApplyOps(F.Ed2, Op, AForceApply, not F.TabSizeChanged, Groups.Mode=gmOne);

  F.Adapter[F.Ed1].DynamicHiliteEnabled:= Op.OpLexerDynamicHiliteMaxLines>0;
  F.Adapter[F.Ed1].DynamicHiliteMaxLines:= Op.OpLexerDynamicHiliteMaxLines;

  F.Adapter[F.Ed2].DynamicHiliteEnabled:= Op.OpLexerDynamicHiliteMaxLines>0;
  F.Adapter[F.Ed2].DynamicHiliteMaxLines:= Op.OpLexerDynamicHiliteMaxLines;

  if not F.BracketHiliteUserChanged then
    F.BracketHilite:= Op.OpBracketHilite;
  F.BracketSymbols:= Op.OpBracketSymbols;
  F.BracketDistance:= Op.OpBracketDistance;

  F.Ed1.Update;
  if F.Splitted then
    F.Ed2.Update;

  UpdateStatusbar;
end;

procedure TfmMain.UpdateMenuItemHint(mi: TMenuItem; const AHint: string);
begin
  if not Assigned(mi) then exit;

  if mi.Tag=0 then
    mi.Tag:= PtrInt(TAppMenuProps.Create(Self));

  TAppMenuProps(mi.Tag).CommandString:= AHint;
end;

procedure TfmMain.UpdateMenuItemHotkey(mi: TMenuItem; ACmd: integer; AllowSetShortcut: boolean=true);
begin
  if not Assigned(mi) then exit;

  if mi.Tag=0 then
    mi.Tag:= PtrInt(TAppMenuProps.Create(Self));

  TAppMenuProps(mi.Tag).CommandCode:= ACmd;
  mi.OnClick:= @MenuMainClick;

  {$if not defined (LCLQt5) and not defined(LCLQt6)}
  //seems Qt5 version has slowdown because of MainMenu hotkeys?
  if AllowSetShortcut then
  begin
    //if Copy/Paste/SelectAll hotkeys set in MainMenu, Qt5 version cannot handle them in Console/Find/Replace
    if (ACmd<>cCommand_ClipboardCopy) and
      (ACmd<>cCommand_ClipboardPaste) and
      (ACmd<>cCommand_ClipboardCut) and
      (ACmd<>cCommand_Undo) and
      (ACmd<>cCommand_Redo) and
      (ACmd<>cCommand_SelectAll) then
      mi.ShortCut:= AppKeymapMain.GetShortcutFromCommand(ACmd);

    //don't set Esc here: for OSX/ Qt, else Esc blocked in Find/ Goto/ closing app
    if mi.Shortcut=AppShortcutEscape then
      mi.ShortCut:= 0;
    //don't allow Shift+Tab in menu: it blocks tabbing in Find/Replace
    if mi.Shortcut=AppShortcutShiftTab then
      mi.ShortCut:= 0;
  end;
  {$endif}
end;


procedure TfmMain.UpdateMenuItem_SetShortcutFromProps(mi: TMenuItem);
var
  NCmd: integer;
  Obj: TObject;
begin
  if not Assigned(mi) then exit;
  if mi.Count>0 then exit;
  if mi.Tag=0 then exit;
  if (mi.Tag>0) and (mi.Tag<=1000) then exit; //prevent crash on special Tag values, e.g. 27

  Obj:= TObject(mi.Tag);
  if not (Obj is TAppMenuProps) then exit;

  NCmd:= TAppMenuProps(Obj).CommandCode;
  if NCmd<=0 then exit;

  //if Copy/Paste/SelectAll hotkeys set in MainMenu, Qt5 version cannot handle them in Console/Find/Replace
  if (NCmd<>cCommand_ClipboardCopy) and
    (NCmd<>cCommand_ClipboardPaste) and
    (NCmd<>cCommand_ClipboardCut) and
    (NCmd<>cCommand_Undo) and
    (NCmd<>cCommand_Redo) and
    (NCmd<>cCommand_SelectAll) then
    mi.ShortCut:= AppKeymapMain.GetShortcutFromCommand(NCmd);

  //don't set Esc here
  if mi.Shortcut=AppShortcutEscape then
    mi.ShortCut:= 0;

  //don't allow Shift+Tab in menu
  if mi.Shortcut=AppShortcutShiftTab then
    mi.ShortCut:= 0;
end;


procedure TfmMain.UpdateMenuItem_SetShortcutsRecursively(AMenuItem: TMenuItem; AMaxMenuLevel: integer);
const
  NMenuLevel: integer = 0;
var
  mi: TMenuItem;
  i: integer;
begin
  for i:= 0 to AMenuItem.Count-1 do
  begin
    mi:= AMenuItem.Items[i];
    if (mi.Count>0) and (NMenuLevel<AMaxMenuLevel) then
    begin
      Inc(NMenuLevel);
      try
        UpdateMenuItem_SetShortcutsRecursively(mi, AMaxMenuLevel);
      finally
        Dec(NMenuLevel);
      end;
    end
    else
      UpdateMenuItem_SetShortcutFromProps(mi);
  end;
end;


procedure TfmMain.UpdateMenuItemAltObject(mi: TMenuItem; ACmd: integer);
  //
  procedure DoSave(mi: TMenuItem; var save: TATMenuItemsAlt);
  begin
    if save.active0 then
      save.item0:= mi
    else
      save.item1:= mi;
    save.active0:= not save.active0;
  end;
  //
begin
  //MsgLogConsole('cmd'+inttostr(ACmd));
  if ACmd>0 then
  case ACmd of
    cCommand_ToggleWordWrap : DoSave(mi, mnuViewWrap_Alt);
    cCommand_ToggleLineNums : DoSave(mi, mnuViewNums_Alt);
    cCommand_ToggleFolding : DoSave(mi, mnuViewFold_Alt);
    cCommand_ToggleRuler : DoSave(mi, mnuViewRuler_Alt);
    cCommand_ToggleMinimap : DoSave(mi, mnuViewMinimap_Alt);
    cCommand_ToggleMicromap : DoSave(mi, mnuViewMicromap_Alt);
    cCommand_ToggleUnprinted : DoSave(mi, mnuViewUnpriShow_Alt);
    cCommand_ToggleUnprintedSpaces : DoSave(mi, mnuViewUnpriSpaces_Alt);
    cCommand_ToggleUnprintedSpacesTrailing : DoSave(mi, mnuViewUnpriSpacesTail_Alt);
    cCommand_ToggleUnprintedEnds : DoSave(mi, mnuViewUnpriEnds_Alt);
    cCommand_ToggleUnprintedEndDetails : DoSave(mi, mnuViewUnpriEndsDet_Alt);

    cmd_SplitTabNo: DoSave(mi, mnuViewSplitNo_Alt);
    cmd_SplitTabVert: DoSave(mi, mnuViewSplitV_Alt);
    cmd_SplitTabHorz: DoSave(mi, mnuViewSplitH_Alt);

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

  UpdateMenuItemHotkey(mnuFileEnc, cmd_MenuEnc);
  UpdateMenuItemHotkey(mnuFileEnds, cmd_MenuEnds);

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
  UpdateMenuItemHotkey(mnuEditPasteIndent, cCommand_ClipboardPasteAndIndent);
  UpdateMenuItemHotkey(mnuEditPasteHist, cCommand_ClipboardPasteFromRecents);

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

  UpdateMenuItemHotkey(mnuViewSplitNo, cmd_SplitTabNo);
  UpdateMenuItemHotkey(mnuViewSplitV, cmd_SplitTabVert);
  UpdateMenuItemHotkey(mnuViewSplitH, cmd_SplitTabHorz);

  UpdateMenuItemHotkey(mnuViewWrap, cCommand_ToggleWordWrap);
  UpdateMenuItemHotkey(mnuViewNums, cCommand_ToggleLineNums);
  UpdateMenuItemHotkey(mnuViewFold, cCommand_ToggleFolding);
  UpdateMenuItemHotkey(mnuViewRuler, cCommand_ToggleRuler);
  UpdateMenuItemHotkey(mnuViewMinimap, cCommand_ToggleMinimap);
  UpdateMenuItemHotkey(mnuViewMicromap, cCommand_ToggleMicromap);

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
  UpdateMenuItemHotkey(mnuLexers, cmd_DialogLexerMenu);

  UpdateMenuItemHotkey(mnuOpDefault, cmd_OpsOpenDefault);
  UpdateMenuItemHotkey(mnuOpUser, cmd_OpsOpenUser);
  UpdateMenuItemHotkey(mnuOpDefaultUser, cmd_OpsOpenDefaultAndUser);
  UpdateMenuItemHotkey(mnuOpLexer, cmd_OpsOpenLexerSpecific);

  UpdateMenuItemHotkey(mnuFontText, cmd_OpsFontText);
  UpdateMenuItemHotkey(mnuFontUi, cmd_OpsFontUi);
  UpdateMenuItemHotkey(mnuFontOutput, cmd_OpsFontOutput);

  UpdateMenuItemHotkey(mnuOpLexProp, cmd_DialogLexerProp);
  UpdateMenuItemHotkey(mnuOpLexLib, cmd_DialogLexerLib);
  UpdateMenuItemHotkey(mnuOpLexMap, cmd_DialogLexerStyleMap);
  UpdateMenuItemHotkey(mnuOpThemeUi, cmd_DialogThemeUi);
  UpdateMenuItemHotkey(mnuOpThemeSyntax, cmd_DialogThemeSyntax);
  UpdateMenuItemHotkey(mnuOpUnprinted, cmd_DialogUnprinted);
  UpdateMenuItemHotkey(mnuOpThemes, cmd_ChooseThemeUI);
  UpdateMenuItemHotkey(mnuOpLangs, cmd_ChooseTranslation);

  UpdateMenuItemHotkey(mnuHelpCmd, cmd_DialogCommands);
  UpdateMenuItemHotkey(mnuHelpForum, cmd_HelpForum);
  UpdateMenuItemHotkey(mnuHelpWiki, cmd_HelpWiki);
  UpdateMenuItemHotkey(mnuHelpIssues, cmd_HelpIssues);
  UpdateMenuItemHotkey(mnuHelpCheckUpd, cmd_HelpCheckUpdates);
  UpdateMenuItemHotkey(mnuHelpAbout, cmd_HelpAbout);
end;

procedure TfmMain.UpdateEditorTabsize(AValue: integer);
var
  F: TEditorFrame;
  Ed: TATSynEdit;
begin
  F:= CurrentFrame;
  if F=nil then exit;
  Ed:= F.Editor;

  case AValue of
    -1:
      Ed.OptTabSpaces:= true;
    -2:
      Ed.OptTabSpaces:= false;
    -3:
      Ed.OptTabSpaces:= not Ed.OptTabSpaces;
    1..100:
      Ed.OptTabSize:= AValue;
  end;

  F.TabSizeChanged:= true;

  UpdateFrameEx(F, false);
  UpdateStatusbar;
end;

procedure TfmMain.UpdateMenuItemChecked(mi: TMenuItem; saved: TATMenuItemsAlt; AValue: boolean);
begin
  if Assigned(mi) then mi.Checked:= AValue;
  if Assigned(saved.item0) then saved.item0.Checked:= AValue;
  if Assigned(saved.item1) then saved.item1.Checked:= AValue;
end;

procedure TfmMain.UpdateMenuChecks_Frame(F: TEditorFrame);
var
  Ed: TATSynEdit;
begin
  if F=nil then exit;
  Ed:= F.Editor;

  UpdateMenuItemChecked(mnuViewWrap, mnuViewWrap_Alt, Ed.OptWrapMode<>TATEditorWrapMode.ModeOff);
  UpdateMenuItemChecked(mnuViewNums, mnuViewNums_Alt, Ed.Gutter.Items[Ed.Gutter.FindIndexByTag(ATEditorOptions.GutterTagNumbers)].Visible);
  UpdateMenuItemChecked(mnuViewFold, mnuViewFold_Alt, Ed.Gutter.Items[Ed.Gutter.FindIndexByTag(ATEditorOptions.GutterTagFolding)].Visible);
  UpdateMenuItemChecked(mnuViewRuler, mnuViewRuler_Alt, Ed.OptRulerVisible);
  UpdateMenuItemChecked(mnuViewMinimap, mnuViewMinimap_Alt, Ed.OptMinimapVisible);
  UpdateMenuItemChecked(mnuViewMicromap, mnuViewMicromap_Alt, Ed.OptMicromapVisible);
end;

procedure TfmMain.UpdateMenuChecks_Global;
begin
  UpdateMenuItemChecked(mnuViewToolbar, mnuViewToolbar_Alt, ShowToolbar);
  UpdateMenuItemChecked(mnuViewStatus, mnuViewStatus_Alt, ShowStatus);
  UpdateMenuItemChecked(mnuViewFullscr, mnuViewFullscr_Alt, ShowFullscreen);
  UpdateMenuItemChecked(mnuViewDistFree, mnuViewDistFree_Alt, ShowDistractionFree);
  UpdateMenuItemChecked(mnuViewSidebar, mnuViewSidebar_Alt, ShowSideBar);
  UpdateMenuItemChecked(mnuViewSide, mnuViewSide_Alt, AppPanels[cPaneSide].Visible);
  UpdateMenuItemChecked(mnuViewBottom, mnuViewBottom_Alt, AppPanels[cPaneOut].Visible);
  UpdateMenuItemChecked(mnuViewFloatSide, mnuViewFloatSide_Alt, AppPanels[cPaneSide].Floating);
  UpdateMenuItemChecked(mnuViewFloatBottom, mnuViewFloatBottom_Alt, AppPanels[cPaneOut].Floating);
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

procedure TfmMain.UpdateMenuChecks_FrameSplit(F: TEditorFrame);
begin
  if F=nil then exit;
  UpdateMenuItemChecked(mnuViewSplitNo, mnuViewSplitNo_Alt, not F.Splitted);
  UpdateMenuItemChecked(mnuViewSplitV, mnuViewSplitV_Alt, F.Splitted and not F.SplitHorz);
  UpdateMenuItemChecked(mnuViewSplitH, mnuViewSplitH_Alt, F.Splitted and F.SplitHorz);
end;

procedure TfmMain.UpdateTreeByTimer;
begin
  TimerTreeFill.Enabled:= false;
  TimerTreeFill.Enabled:= true;
end;

procedure TfmMain.UpdateTreeFilter;
begin
  CodeTreeFilter.InvalidateFilter;
end;

procedure TfmMain.UpdateTreeImagelistActivity;
begin
  if UiOps.TreeShowIcons then
    CodeTree.Tree.Images:= ImageListTree
  else
    CodeTree.Tree.Images:= nil;
end;

procedure TfmMain.UpdateTree(AFill: boolean; AConsiderTreeVisible: boolean);
var
  Frame: TEditorFrame;
  Ed: TATSynEdit;
  Ada: TATAdapterEControl;
  bLiteLexer: boolean;
  //Tick: QWord;
begin
  Frame:= CurrentFrame;
  if Frame=nil then exit;
  CodeTree.Tree.SortType:= Frame.CodetreeSortType;

  Ed:= Frame.Editor;
  bLiteLexer:= Ed.AdapterForHilite is TATLiteLexer;

  if bLiteLexer then
  begin
    Ada:= nil;
    //don't clear tree, don't exit - we can run tree-helper for lite lexer
  end
  else
  begin
    Ada:= Frame.Adapter[Ed];
    if (Ada=nil) or (Ada.AnClient=nil) then
    begin
      DoCodetree_Clear;
      exit;
    end;
  end;

  if AConsiderTreeVisible then
    if not AppPanels[cPaneSide].Visible then
    begin
      RunTreeHelper(
        Frame,
        nil, //ATree
        true, //PascalHelpers, only they give folding yet
        false //PythonHelpers
        );
      exit;
    end;

  if not Frame.EnabledCodeTree[Ed] then
  begin
    //don't clear tree, it may be filled by plugin
    UpdateTreeSelection(Ed); //needed even for disabled CodeTree (for plugins)
    exit;
  end;

  if Assigned(Ada) then
    while Ada.TreeBusy do
    begin
      Application.ProcessMessages;
      if Application.Terminated then exit;
    end;

  if AFill then
  begin
    InitImageListCodetree;
    UpdateTreeImagelistActivity;
    DoPyEvent_AppState(APPSTATE_CODETREE_BEFORE_FILL);

    if not Assigned(FCodetreeBuffer) then
      FCodetreeBuffer:= TTreeView.Create(Self);

    if not RunTreeHelper(Frame, FCodetreeBuffer, true, true) then
      if Assigned(Ada) then
      begin
        //Tick:= GetTickCount64;
        Ada.TreeFill(FCodetreeBuffer, UiOps.TreeFillMaxTime);
        //Tick:= GetTickCount64-Tick;
        //MsgLogConsole('Tree filling: '+IntToStr(Tick)+'ms');
      end;

    if FCodetreeBuffer.Items.Count>0 then
      DoTreeviewCopy(FCodetreeBuffer, Frame.CachedTreeview[Ed]);

    //Lazarus doesn't sort new items
    if CodeTree.Tree.SortType<>stNone then
      FCodetreeBuffer.AlphaSort;

    DoTreeviewCopy(FCodetreeBuffer, CodeTree.Tree);

    //force update scrollbar
    CodeTree.Tree.Font.Size:= ATEditorScaleFont(UiOps.VarFontSize) * UiOps.TreeFontScale div 100;
    CodeTree.Tree.Perform(CM_CHANGED, 0, 0);

    DoCodetree_UpdateVersion(Ed);
    DoPyEvent_AppState(APPSTATE_CODETREE_AFTER_FILL);
  end;

  UpdateTreeFilter;
  UpdateTreeSelection(Ed);
end;

procedure TfmMain.UpdateTreeSelection(Ed: TATSynEdit);
var
  Caret: TATCaretItem;
  NSelLine: integer;
begin
  if not UiOps.TreeAutoSync then exit;

  if Ed=nil then exit;
  if Ed.Carets.Count=0 then exit;
  Caret:= Ed.Carets[0];

  CodetreeSelectItemForPosition(
    CodeTree.Tree,
    Caret.PosX,
    Caret.PosY,
    NSelLine
    );

  if (AppCodetreeState.SelLine<>NSelLine) or
    (AppCodetreeState.Editor<>Ed) then
  begin
    AppCodetreeState.Editor:= Ed;
    AppCodetreeState.SelLine:= NSelLine;
    if AppCodetreeState.SelLine>=0 then
      DoPyEvent_AppState(APPSTATE_CODETREE_SET_SELECTION);
  end;
end;


procedure TfmMain.UpdateStatusbarPanelsFromString(const AText: string);
var
  SPanel, SItem: string;
  NIndex, NSize, NTag: integer;
  Al: TAlignment;
  SepAll, SepItem: TATStringSeparator;
  bAutoSize, bStretch, bHotTrack: boolean;
begin
  //complex deletion of panels, touch only CudaText std panels
  for NTag:= StatusbarTag_Caret{minimal tag} to StatusbarTag_Msg{maximal tag} do
  begin
    NIndex:= Status.FindPanel(NTag);
    Status.DeletePanel(NIndex);
  end;

  SepAll.Init(AText, '|');
  repeat
    if not SepAll.GetItemStr(SPanel) then Break;

    SepItem.Init(SPanel);
    NTag:= 0;
    NIndex:= Status.PanelCount;
    bStretch:= false;
    bHotTrack:= false;

    SepItem.GetItemStr(SItem);
    case SItem of
      'caret':   begin NTag:= StatusbarTag_Caret; bHotTrack:= true; end;
      'enc':     begin NTag:= StatusbarTag_Enc; bHotTrack:= true; end;
      'ends':    begin NTag:= StatusbarTag_LineEnds; bHotTrack:= true; end;
      'lexer':   begin NTag:= StatusbarTag_Lexer; bHotTrack:= true; end;
      'tabsize': begin NTag:= StatusbarTag_TabSize; bHotTrack:= true; end;
      'ins':     begin NTag:= StatusbarTag_InsOvr; bHotTrack:= true; end;
      'msg':     begin NTag:= StatusbarTag_Msg; bStretch:= true; end;
      'selmode': begin NTag:= StatusbarTag_SelMode; bHotTrack:= true; end;
      'wrap':    begin NTag:= StatusbarTag_WrapMode; bHotTrack:= true; end;
      'zoom':    begin NTag:= StatusbarTag_Zoom; bHotTrack:= false; end;
      else Continue;
    end;

    SepItem.GetItemStr(SItem);
    bAutoSize:= SItem='A';
    if bAutoSize then
      Al:= taLeftJustify
    else
      Al:= AppStringToAlignment(SItem);

    SepItem.GetItemStr(SItem);
    NSize:= Max(16, StrToIntDef(SItem, 200));

    Status.AddPanel(-1, NSize, Al, '', -1, NTag, bAutoSize, bStretch, clNone, bHotTrack);
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


procedure TfmMain.InitToolbar;
begin
  ToolbarMain.Images:= TImageList.Create(Self);
  ToolbarMain.Images.AllocBy:= 20;

  ToolbarMain.AddButton(0, @DoToolbarClick, nil, 'f_new', 'New file', IntToStr(cmd_FileNew), false);
  ToolbarMain.AddDropdown(-1, nil, @DoFileNewMenu_ToolbarClick);
  ToolbarMain.AddButton(1, @DoToolbarClick, nil, 'f_open', 'Open file', IntToStr(cmd_FileOpen), false);
  ToolbarMain.AddDropdown(-1, PopupRecents);
  ToolbarMain.AddButton(2, @DoToolbarClick, nil, 'f_save', 'Save file', IntToStr(cmd_FileSave), false);
  ToolbarMain.AddSep;
  ToolbarMain.AddButton(3, @DoToolbarClick, nil, 'e_cut', 'Cut', IntToStr(cCommand_ClipboardCut), false);
  ToolbarMain.AddButton(4, @DoToolbarClick, nil, 'e_copy', 'Copy', IntToStr(cCommand_ClipboardCopy), false);
  ToolbarMain.AddButton(5, @DoToolbarClick, nil, 'e_paste', 'Paste', IntToStr(cCommand_ClipboardPaste), false);
  ToolbarMain.AddButton(6, @DoToolbarClick, nil, 'e_undo', 'Undo', IntToStr(cCommand_Undo), false);
  ToolbarMain.AddButton(7, @DoToolbarClick, nil, 'e_redo', 'Redo', IntToStr(cCommand_Redo), false);
  ToolbarMain.AddSep;
  ToolbarMain.AddButton(8, @DoToolbarClick, nil, 'unpri', 'Toggle unprinted chars', IntToStr(cCommand_ToggleUnprinted), false);
  ToolbarMain.AddButton(9, @DoToolbarClick, nil, 'map', 'Toggle minimap', IntToStr(cCommand_ToggleMinimap), false);
  ToolbarMain.AddSep;
  ToolbarMain.AddButton(10, @DoToolbarClick, nil, 'indent', 'Indent block', IntToStr(cCommand_TextIndent), false);
  ToolbarMain.AddButton(11, @DoToolbarClick, nil, 'unindent', 'Unindent block', IntToStr(cCommand_TextUnIndent), false);

  ToolbarMain.AddDropdown(12, PopupToolbarCase);
  ToolbarMain.AddDropdown(13, PopupToolbarComment);
  ToolbarMain.AddButton(14, @DoToolbarClick, nil, 'opt', 'Options Editor', 'module=cuda_prefs;cmd=dlg_cuda_options;', false);
end;


const
  cSidebarIconTree     = 0;
  cSidebarIconConsole  = 1;
  cSidebarIconOutput   = 2;
  cSidebarIconValidate = 3;
  cSidebarIconFind     = 4;
  cSidebarIconMenu     = 5;

procedure TfmMain.DoOps_LoadSidebarIcons;
const
  cCallerAPI = 'DoOps_LoadSidebarIcons';
var
  Str: string;
  NTick: QWord;
  W, H: integer;
begin
  NTick:= GetTickCount64;

  if not ParseNameWithWidthAndHeight(UiOps.SidebarTheme, W, H) then exit;

  ImageListSide.Width:= W;
  ImageListSide.Height:= H;
  ImageListSide.Clear;

  Str:= AppDir_DataSidebarIcons+DirectorySeparator+UiOps.SidebarTheme;
  if not DirectoryExists(Str) then
  begin
    MsgLogConsole(Format(msgCannotFindData, [Str]));
    MsgStdout(Format(msgCannotFindData, [Str]));
    ToolbarSideMid.UpdateControls;
    exit;
  end;

  Str+= DirectorySeparator;

  UpdateImagelistWithIconFromFile(ImageListSide, Str+'tree.png', cCallerAPI);
  UpdateImagelistWithIconFromFile(ImageListSide, Str+'console.png', cCallerAPI);
  UpdateImagelistWithIconFromFile(ImageListSide, Str+'output.png', cCallerAPI);
  UpdateImagelistWithIconFromFile(ImageListSide, Str+'validate.png', cCallerAPI);
  UpdateImagelistWithIconFromFile(ImageListSide, Str+'find.png', cCallerAPI);
  UpdateImagelistWithIconFromFile(ImageListSide, Str+'menu.png', cCallerAPI);

  if UiOps.LogConsoleDetailedStartupTime then
  begin
    NTick:= GetTickCount64-NTick;
    MsgLogConsole(Format('Loaded sidebar icons: %dms', [NTick]));
  end;
end;


procedure TfmMain.DoOps_LoadCodetreeIcons;
var
  SPath, SItem: string;
  Sep: TATStringSeparator;
  W, H: integer;
begin
  if not ParseNameWithWidthAndHeight(UiOps.TreeTheme, W, H) then exit;

  if not Assigned(ImageListTree) then
    raise Exception.Create('ImageListTree nil');
  ImageListTree.Width:= W;
  ImageListTree.Height:= H;
  ImageListTree.Clear;

  SPath:= AppDir_DataCodetreeIcons+DirectorySeparator+UiOps.TreeTheme;
  if not DirectoryExists(SPath) then
  begin
    MsgLogConsole(Format(msgCannotFindData, [SPath]));
    MsgStdout(Format(msgCannotFindData, [SPath]));
    exit;
  end;
  SPath+= DirectorySeparator;

  Sep.Init(UiOps.TreeIconFilenames);
  while Sep.GetItemStr(SItem) do
    UpdateImagelistWithIconFromFile(ImageListTree, SPath+SItem+'.png', 'DoOps_LoadCodetreeIcons');
end;

procedure TfmMain.DoOps_LoadToolbarIcons;
const
  cCallerAPI = 'DoOps_LoadToolbarIcons';
var
  Str: string;
  Img: TImageList;
  NTick: QWord;
  W, H: integer;
begin
  NTick:= GetTickCount64;

  if not ParseNameWithWidthAndHeight(UiOps.ToolBarTheme, W, H) then exit;

  Str:= AppDir_DataToolbarIcons+DirectorySeparator+UiOps.ToolBarTheme;
  if not DirectoryExists(Str) then
  begin
    MsgLogConsole(Format(msgCannotFindData, [Str]));
    MsgStdout(Format(msgCannotFindData, [Str]));
    Exit;
  end;

  Img:= ToolbarMain.Images;
  Img.Width:= W;
  Img.Height:= H;
  Img.Clear;

  Str+= DirectorySeparator;
  UpdateImagelistWithIconFromFile(Img, Str+'f_new.png', cCallerAPI);
  UpdateImagelistWithIconFromFile(Img, Str+'f_open.png', cCallerAPI);
  UpdateImagelistWithIconFromFile(Img, Str+'f_save.png', cCallerAPI);
  UpdateImagelistWithIconFromFile(Img, Str+'e_cut.png', cCallerAPI);
  UpdateImagelistWithIconFromFile(Img, Str+'e_copy.png', cCallerAPI);
  UpdateImagelistWithIconFromFile(Img, Str+'e_paste.png', cCallerAPI);
  UpdateImagelistWithIconFromFile(Img, Str+'e_undo.png', cCallerAPI);
  UpdateImagelistWithIconFromFile(Img, Str+'e_redo.png', cCallerAPI);
  UpdateImagelistWithIconFromFile(Img, Str+'unpri.png', cCallerAPI);
  UpdateImagelistWithIconFromFile(Img, Str+'map.png', cCallerAPI);
  UpdateImagelistWithIconFromFile(Img, Str+'indent.png', cCallerAPI);
  UpdateImagelistWithIconFromFile(Img, Str+'unindent.png', cCallerAPI);
  UpdateImagelistWithIconFromFile(Img, Str+'case.png', cCallerAPI);
  UpdateImagelistWithIconFromFile(Img, Str+'comment.png', cCallerAPI);
  UpdateImagelistWithIconFromFile(Img, Str+'opt.png', cCallerAPI);

  if UiOps.LogConsoleDetailedStartupTime then
  begin
    NTick:= GetTickCount64-NTick;
    MsgLogConsole(Format('Loaded toolbar icons: %dms', [NTick]));
  end;
end;


procedure TfmMain.InitSidebar;
begin
  ToolbarSideTop.AddButton(cSidebarIconMenu, @DoSidebar_MainMenuClick, nil, msgPanelMenu_Init, msgPanelMenu_Init, '', false);

  ToolbarSideMid.AddButton(cSidebarIconFind, @DoBottom_FindClick, nil, msgPanelSearch_Init, msgPanelSearch_Init, '', false);
  ToolbarSideMid.AddSep;

  AppPanels[cPaneSide].Add(msgPanelTree_Init, cSidebarIconTree, PtrInt(Pointer(PanelCodeTreeAll)), @DoSidebar_OnShowCodeTree);

  AppPanels[cPaneOut].Add(msgPanelConsole_Init, cSidebarIconConsole, PtrInt(Pointer(fmConsole)), nil);
  AppPanels[cPaneOut].Add(msgPanelOutput_Init, cSidebarIconOutput, PtrInt(Pointer(fmOutput)), nil);
  AppPanels[cPaneOut].Add(msgPanelValidate_Init, cSidebarIconValidate, PtrInt(Pointer(fmValidate)), nil);
end;


procedure TfmMain.InitPopupTabSize;
var
  Ed: TATSynEdit;
  Msg: string;
  NTab, i: integer;
  mi: TMenuItem;
begin
  if PopupTabSize=nil then
  begin
    PopupTabSize:= TPopupMenu.Create(Self);

    mi:= TMenuItem.Create(Self);
    mi.Caption:= '-';
    PopupTabSize.Items.Add(mi);

    mnuTabsizeSpace:= TMenuItem.Create(Self);
    mnuTabsizeSpace.OnClick:= @mnuTabsizeSpaceClick;
    PopupTabSize.Items.Add(mnuTabsizeSpace);

    mi:= TMenuItem.Create(Self);
    mi.Caption:= '-';
    PopupTabSize.Items.Add(mi);

    mnuTabsizeConvTabs:= TMenuItem.Create(Self);
    PopupTabSize.Items.Add(mnuTabsizeConvTabs);

    mnuTabsizeConvSpaces:= TMenuItem.Create(Self);
    PopupTabSize.Items.Add(mnuTabsizeConvSpaces);

    for i:= cMenuTabsizeMax downto cMenuTabsizeMin do
    begin
      mi:= TMenuItem.Create(Self);
      mi.Caption:= IntToStr(i);
      mi.Tag:= i;
      mi.OnClick:= @MenuTabsizeClick;
      mnuTabsizesValue[i]:= mi;
      PopupTabSize.Items.Insert(0, mi);
    end;
  end;

  mnuTabsizeSpace.Caption:= msgTabsizeUseSpaces;
  mnuTabsizeConvSpaces.Caption:= msgTabsizeConvSpaces;
  mnuTabsizeConvTabs.Caption:= msgTabsizeConvTabs;

  UpdateMenuItemHotkey(mnuTabsizeConvSpaces, cmd_ConvertSpacesToTabsLeading);
  UpdateMenuItemHotkey(mnuTabsizeConvTabs, cmd_ConvertTabsToSpacesLeading);

  Ed:= CurrentEditor;
  if Ed.OptTabSpaces then
    Msg:= msgStatusbarTextSpaces
  else
    Msg:= msgStatusbarTextTab;
  NTab:= Ed.OptTabSize;

  for i:= cMenuTabsizeMin to cMenuTabsizeMax do
  begin
    mnuTabsizesValue[i].Caption:= Msg+': '+IntToStr(i);
    mnuTabsizesValue[i].Checked:= NTab=i;
  end;

  mnuTabsizeSpace.Checked:= Ed.OptTabSpaces;
end;

procedure TfmMain.InitPopupTree;
var
  mi: TMenuItem;
begin
  if PopupTree=nil then
  begin
    PopupTree:= TPopupMenu.Create(Self);

    mnuTreeFoldAll:= TMenuItem.Create(Self);
    mnuTreeFoldAll.OnClick:= @mnuTreeFoldAllClick;
    PopupTree.Items.Add(mnuTreeFoldAll);

    mnuTreeUnfoldAll:= TMenuItem.Create(Self);
    mnuTreeUnfoldAll.OnClick:= @mnuTreeUnfoldAllClick;
    PopupTree.Items.Add(mnuTreeUnfoldAll);

    mnuTreeFoldLevel:= TMenuItem.Create(Self);
    PopupTree.Items.Add(mnuTreeFoldLevel);

    mnuTreeFold2:= TMenuItem.Create(Self);
    mnuTreeFold2.Caption:= '2';
    mnuTreeFold2.OnClick:= @mnuTreeFold2Click;
    mnuTreeFoldLevel.Add(mnuTreeFold2);

    mnuTreeFold3:= TMenuItem.Create(Self);
    mnuTreeFold3.Caption:= '3';
    mnuTreeFold3.OnClick:= @mnuTreeFold3Click;
    mnuTreeFoldLevel.Add(mnuTreeFold3);

    mnuTreeFold4:= TMenuItem.Create(Self);
    mnuTreeFold4.Caption:= '4';
    mnuTreeFold4.OnClick:= @mnuTreeFold4Click;
    mnuTreeFoldLevel.Add(mnuTreeFold4);

    mnuTreeFold5:= TMenuItem.Create(Self);
    mnuTreeFold5.Caption:= '5';
    mnuTreeFold5.OnClick:= @mnuTreeFold5Click;
    mnuTreeFoldLevel.Add(mnuTreeFold5);

    mnuTreeFold6:= TMenuItem.Create(Self);
    mnuTreeFold6.Caption:= '6';
    mnuTreeFold6.OnClick:= @mnuTreeFold6Click;
    mnuTreeFoldLevel.Add(mnuTreeFold6);

    mnuTreeFold7:= TMenuItem.Create(Self);
    mnuTreeFold7.Caption:= '7';
    mnuTreeFold7.OnClick:= @mnuTreeFold7Click;
    mnuTreeFoldLevel.Add(mnuTreeFold7);

    mnuTreeFold8:= TMenuItem.Create(Self);
    mnuTreeFold8.Caption:= '8';
    mnuTreeFold8.OnClick:= @mnuTreeFold8Click;
    mnuTreeFoldLevel.Add(mnuTreeFold8);

    mnuTreeFold9:= TMenuItem.Create(Self);
    mnuTreeFold9.Caption:= '9';
    mnuTreeFold9.OnClick:= @mnuTreeFold9Click;
    mnuTreeFoldLevel.Add(mnuTreeFold9);

    mi:= TMenuItem.Create(Self);
    mi.Caption:= '-';
    PopupTree.Items.Add(mi);

    mnuTreeSorted:= TMenuItem.Create(Self);
    mnuTreeSorted.OnClick:= @mnuTreeSortedClick;
    PopupTree.Items.Add(mnuTreeSorted);
  end;

  mnuTreeFoldAll.Caption:= ATEditorOptions.TextMenuitemFoldAll;
  mnuTreeUnfoldAll.Caption:= ATEditorOptions.TextMenuitemUnfoldAll;
  mnuTreeFoldLevel.Caption:= ATEditorOptions.TextMenuitemFoldLevel;
  mnuTreeSorted.Caption:= msgTreeSorted;

  mnuTreeSorted.Checked:= CodeTree.Tree.SortType<>stNone;
end;

procedure TfmMain.InitPopupPicScale;
const
  cSizes: array[0..8] of integer =
    (-1, 33, 50, 100, 150, 200, 500, 1000, 1500);
var
  mi: TMenuItem;
  i: integer;
begin
  if PopupPicScale=nil then
  begin
    PopupPicScale:= TPopupMenu.Create(Self);
    for i:= Low(cSizes) to High(cSizes) do
    begin
      mi:= TMenuItem.Create(Self);
      if cSizes[i]=-1 then
        mi.Caption:= msgDefault
      else
        mi.Caption:= IntToStr(cSizes[i])+'%';
      mi.Tag:= cSizes[i];
      mi.OnClick:= @MenuPicScaleClick;
      PopupPicScale.Items.Add(mi);
    end;
  end;
end;

procedure TfmMain.UpdateEditorShowCaret;
//solves issue #4022
//it makes sense for all Wrap modes
var
  Ed: TATSynEdit;
  Caret: TATCaretItem;
begin
  Ed:= CurrentEditor;
  if Assigned(Ed) and (Ed.Carets.Count=1) then
  begin
    Caret:= Ed.Carets[0];
    Application.ProcessMessages; //must have
    Ed.DoShowPos(
      Point(Caret.PosX, Caret.PosY),
      UiOps.FindIndentHorz,
      UiOps.FindIndentVert,
      false, //Unfold
      true, //AllowUpdate
      true //AllowProximity
      );
  end;
end;

procedure TfmMain.UpdateMenuSidebarButton(AWhenAutoShow: boolean);
begin
  if ToolbarSideTop.ButtonCount>0 then
    with ToolbarSideTop.Buttons[0] do
      if OnClick=@DoSidebar_MainMenuClick then
        case UiOps.ShowSidebarMenuButton of
          0: Visible:= false;
          1: Visible:= true;
          2: Visible:= AWhenAutoShow;
        end;
end;

