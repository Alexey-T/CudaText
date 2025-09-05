(*
This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at http://mozilla.org/MPL/2.0/.

Copyright (c) Alexey Torgashin
*)
{$ifdef nn}begin end;{$endif}

procedure TfmMain.FinderOnFound(Sender: TObject; APos1, APos2: TPoint);
var
  Ed: TATSynEdit;
  NLen: integer;
begin
  Ed:= FFinder.Editor;
  Assert(Assigned(Ed), 'FinderOnFound: Finder.Editor is nil');

  case FFindMarkingMode of
    TAppFinderMarking.None:
      begin
        //don't do Ed.DoGotoPos here, because it resets selection,
        //while we need to keep sel, on 'find next' with 'in selection'
        Ed.DoShowPos(
          APos1,
          UiOps.FindIndentHorz,
          UiOps.FindIndentVert,
          true,
          true,
          true);
      end;

    TAppFinderMarking.Selections:
      begin
        if FFindMarkingCaret1st then
        begin
          FFindMarkingCaret1st:= false;
          Ed.Carets.Clear;
        end;
        Ed.Carets.Add(APos2.X, APos2.Y, APos1.X, APos1.Y);
        //Ed.Carets.Sort; //better comment, to not merge many selections, on "Select all"
      end;

    TAppFinderMarking.Markers:
      begin
        //show marker line, if range is one-liner
        if APos2.Y=APos1.Y then
          NLen:= APos1.X-APos2.X
        else
          NLen:= 0;
        Ed.Markers.Add(
          Point(APos2.X, APos2.Y),
          Point(0, 0),
          TATMarkerTags.Init(0, 0),
          nil,
          TATMarkerMicromapMode.TextOnly,
          NLen
          );
      end;

    TAppFinderMarking.Bookmarks:
      begin
        Ed.BookmarkSetForLine(APos1.Y, 1, '', TATBookmarkAutoDelete.ByOption, true, 0);
      end;
  end;
end;


function TfmMain.FinderReplaceAll(Ed: TATSynEdit; AResetCaret: boolean): integer;
var
  NTop, NLeft: integer;
  NCaretX, NCaretY: integer;
  SavedCarets: TATPointPairArray;
begin
  Result:= 0;
  if Ed.ModeReadOnly then exit;
  SavedCarets:= Ed.Carets.AsArray;

  NTop:= Ed.LineTop;
  NLeft:= Ed.ColumnLeft;
  if Ed.Carets.Count>0 then
    with Ed.Carets[0] do
    begin
      NCaretX:= PosX;
      NCaretY:= PosY;
    end
  else
  begin
    NCaretX:= 0;
    NCaretY:= 0;
  end;

  if AResetCaret then
    Ed.DoCaretSingle(0, 0);

  Ed.Enabled:= false;
  Ed.Strings.BeginUndoGroup;
  try
    //avoid big slowdown when 'Hi' highlighted 10k chars (Undo saves all attribs)
    Ed.Attribs.DeleteWithTag(UiOps.FindHiAll_TagValue);
    Ed.Attribs.DeleteWithTag(UiOps.FindOccur_TagValue);
    //plugin 'Highlight Occurrences' makes many attribs too
    Ed.Attribs.DeleteWithTag(UiOps.PluginHiOccur_TagValue);
    Ed.Attribs.DeleteWithTag(UiOps.PluginSpellChecker_TagValue);

    FFinder.Editor:= Ed;
    Result:= FFinder.DoAction_ReplaceAll;
  finally
    Ed.Strings.EndUndoGroup;
    Ed.Enabled:= true;
    Ed.EndUpdate; //editor can be locked during replace confirmation
  end;

  FinderUpdateEditor(Result>0, false);

  if Result>0 then
    Ed.DoCaretSingle(NCaretX, NCaretY)
  else
    Ed.Carets.AsArray:= SavedCarets;

  Ed.DoCaretsFixIncorrectPos(true);
  Ed.LineTop:= NTop;
  Ed.ColumnLeft:= NLeft;
end;

procedure TfmMain.FinderShowReplaceReport(ACounter, ATime: integer);
begin
  MsgStatus(
    Format(msgStatusReplaceCount, [ACounter])+
    Format(' (%d s)', [ATime]),
    true);
end;


procedure TfmMain.FindDialogDone2(Sender: TObject; Res: TAppFinderOperation;
  AEnableUpdateAll, ADocumentIsSmall: boolean);
begin
  FindDialogDone(Sender, Res, AEnableUpdateAll, ADocumentIsSmall);
end;

procedure TfmMain.FindDialogDone_ExtractRegexMatches;
var
  Matches: TStringList;
  Frame: TEditorFrame;
  Ed: TATSynEdit;
begin
  Matches:= TStringList.Create;
  try
    FFinder.OptRegex:= true;
    FFinder.DoAction_ExtractAll(false, Matches, false, FFinder.OptCase, dupAccept);
    if Matches.Count>0 then
    begin
      StringsDeduplicate(Matches, FFinder.OptCase);
      Frame:= DoFileOpen('', '');
      if Frame=nil then exit;
      Frame.TabCaption:= msgFinderRegexMatchesNumbered;
      Frame.TabCaptionReason:= TAppTabCaptionReason.UnsavedSpecial;
      Ed:= Frame.Ed1;
      Ed.Strings.LoadFromStrings(Matches, TATLineEnds.Unix, false);
      Ed.UpdateWrapInfo(true, false); //AForceUpdate=True to fix #4609
      Ed.Update;
      Ed.DoEventChange;
      MsgStatus(Format(msgStatusFoundFragments, [Matches.Count]), true);
    end
    else
      FinderShowResult(false, false, FFinder);
  finally
    FreeAndNil(Matches);
  end;
end;

procedure TfmMain.FindDialogDone_SelectAll(var ACounter: integer);
begin
  DoFindMarkingInit(TAppFinderMarking.Selections);
  ACounter:= FFinder.DoAction_CountAll(true);
  DoFindMarkingInit(TAppFinderMarking.None);
  FinderUpdateEditor(false);

  if ACounter>0 then
  begin
    Assert(Assigned(FFinder.Editor), 'FindAndSelectAll: Finder.Editor is nil');
    FFinder.Editor.DoCommand(cCommand_ScrollToCaretTop, TATCommandInvoke.AppInternal);
    //FFinder.Editor.DoGotoCaret(cEdgeTop); //also works, but it's more complicated, less wanted
  end;

  if (ACounter=0) and FFinder.IsRegexBad then
    MsgStatusErrorInRegex
  else
    MsgStatus(
      Format(msgStatusFoundFragments, [ACounter]) +
      FinderOptionsToHint(FFinder, false),
      true);
end;

procedure TfmMain.FindDialogDone_MarkAll(var ACounter: integer);
begin
  DoFindMarkingInit(TAppFinderMarking.Markers);
  ACounter:= FFinder.DoAction_CountAll(true);
  DoFindMarkingInit(TAppFinderMarking.None);
  FinderUpdateEditor(false);
  if (ACounter=0) and FFinder.IsRegexBad then
    MsgStatusErrorInRegex
  else
    MsgStatus(
      Format(msgStatusFoundFragments, [ACounter]) +
      FinderOptionsToHint(FFinder, false),
      true);
end;

procedure TfmMain.FindDialogDone_CountAll(var ACounter: integer);
begin
  DoFindMarkingInit(TAppFinderMarking.None);
  ACounter:= FFinder.DoAction_CountAll(false);
  if (ACounter=0) and FFinder.IsRegexBad then
    MsgStatusErrorInRegex
  else
    MsgStatus(
      Format(msgStatusFoundFragments, [ACounter])+
      FinderOptionsToHint(FFinder, false),
      true);
end;

procedure TfmMain.FindDialogDone_FindAndStop(ABack: boolean);
var
  ok, bChanged: boolean;
begin
  FFinder.OptBack:= ABack;
  ok:= FFinder.DoAction_FindOrReplace(
    false, false, bChanged, true);
  FinderUpdateEditor(false);

  //even if 'Hi' option is on, show results here.
  //TimerHiAllTick will show results again,
  //but we still need to show results here for the F3 pressing in editor (TimerHiAllTick is not called).
  FinderShowResult(ok, false, FFinder);
end;

procedure TfmMain.FindDialogDone_ReplaceOne(AndStop: boolean);
var
  ok, bChanged: boolean;
begin
  //replace match
  ok:= FFinder.DoAction_ReplaceSelected(true);
  //after match is replaced, do find-next
  if not AndStop then
    if ok then
      ok:= FFinder.DoAction_FindOrReplace(false, false, bChanged, true);
  FinderUpdateEditor(true);
  FinderShowResult(ok, true, FFinder);
end;

procedure TfmMain.FindDialogDone_ReplaceAll(var ACounter: integer);
var
  NTick: QWord;
  NTime: integer;
begin
  Assert(Assigned(FFinder.Editor), 'FindAndReplaceAll: Finder.Editor is nil');
  NTick:= GetTickCount64;
  ACounter:= FinderReplaceAll(FFinder.Editor, false);
  NTime:= (GetTickCount64-NTick) div 1000;
  UpdateStatusbar;
  FinderShowReplaceReport(ACounter, NTime);
end;

procedure TfmMain.FindDialogDone_ReplaceInAllFrames(AFramePrevious: TEditorFrame; var ACounter: integer);
var
  NTick: QWord;
  FrameNew: TEditorFrame;
  NTime, IndexFrame: integer;
  bChangeFrame: boolean;
begin
  ACounter:= 0;
  NTick:= GetTickCount64;
  bChangeFrame:= FFinder.OptConfirmReplace and (FrameCount>1);

  for IndexFrame:= 0 to FrameCount-1 do
  begin
    FrameNew:= Frames[IndexFrame];
    if FrameNew.FrameKind<>TAppFrameKind.Editor then Continue;

    if bChangeFrame then
      SetFrame(FrameNew);

    ACounter+= FinderReplaceAll(FrameNew.Ed1, true);
    if not FrameNew.EditorsLinked then
      ACounter+= FinderReplaceAll(FrameNew.Ed2, true);
  end;

  NTime:= (GetTickCount64-NTick) div 1000;
  if bChangeFrame then
    SetFrame(AFramePrevious);
  UpdateStatusbar;
  FinderShowReplaceReport(ACounter, NTime);
end;

procedure TfmMain.FindDialogDone(Sender: TObject; Res: TAppFinderOperation;
  AEnableUpdateAll, ADocumentIsSmall: boolean);
var
  Category: TAppFinderOperationCategory;
  Frame: TEditorFrame;
  ok, bDoLock, bFindMode, bHiliteAll, bFocusFindDlg: boolean;
  OperationsFocusEditor: set of TAppFinderOperation;
  NCounter: integer;
begin
  InitFormFind;
  Frame:= CurrentFrame;
  Category:= cAppFinderOperationCategory[Res];
  if Category=TAppFinderOperationCategory.None then exit;
  bHiliteAll:= false;
  bFocusFindDlg:= Assigned(fmFind) and fmFind.Visible and fmFind.Enabled and (fmFind.edFind.Focused or fmFind.edRep.Focused);
  NCounter:= 0;

  //fixes issue #5605
  //maybe we need to reset OptDisableOnProgress at the end of function?
  FFinder.OptDisableOnProgress:= ADocumentIsSmall;

  if Category in [TAppFinderOperationCategory.Find, TAppFinderOperationCategory.ReplaceOne] then
  begin
    if Frame.FrameKind=TAppFrameKind.ImageViewer then exit;
    if Frame.Editor.Carets.Count=0 then
    begin
      MsgStatus(msgCannotFindWithoutCaret);
      exit;
    end;
  end;

  if Category=TAppFinderOperationCategory.ReplaceOne then
  begin
    if Frame.FrameKind<>TAppFrameKind.Editor then exit;
    if Frame.Editor.ModeReadOnly then exit;
  end;

  //keep focus in Find field, else focus find or rep field
  if fmFind.edFind.Focused then
    bFindMode:= true
  else
  if fmFind.edRep.Focused then
    bFindMode:= false
  else
    bFindMode:= Category=TAppFinderOperationCategory.Find;

  if Category=TAppFinderOperationCategory.CloseDlg then
  begin
    //handy to reset in-sel, on closing dialog
    FFinder.OptInSelection:= false;

    if IsFocusedFind then
      Frame.SetFocus;
    fmFind.Hide;
    EditorClearHiAllMarkers(Frame.Editor);
    UpdateAppForSearch(false, false, bFindMode, AEnableUpdateAll, bFocusFindDlg);
    Exit;
  end;

  //Sender=nil allows to not use dialog
  if Assigned(Sender) then
  with fmFind do
  begin
    if edFind.IsEmpty then Exit;
    FFinder.StrFind:= edFind.Text;
    FFinder.StrReplace:= edRep.Text;
    FFinder.OptBack:= false;
    FFinder.OptCase:= chkCase.Checked;
    FFinder.OptWords:= chkWords.Checked;
    FFinder.OptRegex:= chkRegex.Checked;
    FFinder.OptRegexSubst:= chkRegexSubst.Checked;
    FFinder.OptWrapped:= chkWrap.Checked;
    FFinder.OptInSelection:= chkInSel.Checked;
    FFinder.OptFromCaret:= (Res=TAppFinderOperation.FindNext) or (Res=TAppFinderOperation.FindPrev) or (Res=TAppFinderOperation.Replace);
    FFinder.OptConfirmReplace:= chkConfirm.Checked;
    FFinder.OptTokens:= TATFinderTokensAllowed(bTokens.ItemIndex);
    FFinder.OptPreserveCase:= chkPreserveCase.Checked;
    bHiliteAll:= chkHiAll.Checked;
  end;

  if Frame.FrameKind=TAppFrameKind.BinaryViewer then
  begin
    UpdateAppForSearch(true, false, true, AEnableUpdateAll, bFocusFindDlg);
    Application.ProcessMessages;

    case Res of
      TAppFinderOperation.FindFirst:
        begin
          ok:= Frame.ViewerFind(FFinder, bHiliteAll, false, false);
          FinderShowResultSimple(ok, FFinder);
        end;

      TAppFinderOperation.FindNext,
      TAppFinderOperation.FindPrev:
        begin
          if (not Frame.Viewer.SearchStarted) or (Frame.Viewer.SearchString<>FFinder.StrFind) then
            ok:= Frame.ViewerFind(FFinder, bHiliteAll, false, false)
          else
            ok:= Frame.ViewerFind(FFinder, bHiliteAll, true, Res=TAppFinderOperation.FindPrev);
          FinderShowResultSimple(ok, FFinder);
        end;
    end;

    UpdateAppForSearch(false, false, true, AEnableUpdateAll, bFocusFindDlg);
    exit;
  end;

  //if FFinder.OptInSelection then
  //  if CurrentEditor.Carets.Count>1 then
  //    MsgBox(msgCannotFindInMultiSel, MB_OK+MB_ICONWARNING);

  //format must be this:
  // https://wiki.freepascal.org/CudaText_API#Format_of_text_for_cmd_FinderAction
  if Frame.MacroRecord then
    Frame.MacroStrings.Add(
        IntToStr(cmd_FinderAction)+','+
        cAppFinderOperationString[Res]+#1+
        UTF8Encode(FFinder.StrFind)+#1+
        UTF8Encode(FFinder.StrReplace)+#1+
        FinderOptionsToString(FFinder)
        );

  bDoLock:=
    //(Res=afoReplace) or //don't lock for single replace, issue #1216
    //(Res=afoReplaceStop) or
    (Res=TAppFinderOperation.ReplaceAll) or
    (Res=TAppFinderOperation.ReplaceGlobal);
  UpdateAppForSearch(true, bDoLock, bFindMode, AEnableUpdateAll, bFocusFindDlg);

  if bDoLock then
  begin
    Assert(Assigned(FFinder.Editor), 'FindDialogDone: Finder.Editor is nil');
    //add undo group, so Undo won't take too much actions
    FFinder.Editor.Strings.BeginUndoGroup;
    FFinder.Editor.Strings.EndUndoGroup;
  end;

  case Res of
    TAppFinderOperation.FindFirst,
    TAppFinderOperation.FindNext,
    TAppFinderOperation.FindPrev:
      FindDialogDone_FindAndStop(Res=TAppFinderOperation.FindPrev);

    TAppFinderOperation.Replace,
    TAppFinderOperation.ReplaceStop:
      FindDialogDone_ReplaceOne(Res=TAppFinderOperation.ReplaceStop);

    TAppFinderOperation.ReplaceAll:
      FindDialogDone_ReplaceAll(NCounter);

    TAppFinderOperation.ReplaceGlobal:
      FindDialogDone_ReplaceInAllFrames(Frame, NCounter);

    TAppFinderOperation.CountAll:
      FindDialogDone_CountAll(NCounter);

    TAppFinderOperation.ExtractAll:
      FindDialogDone_ExtractRegexMatches;

    TAppFinderOperation.FindSelectAll:
      FindDialogDone_SelectAll(NCounter);

    TAppFinderOperation.FindMarkAll:
      FindDialogDone_MarkAll(NCounter);
  end; //case Res of

  UpdateAppForSearch(false, false, bFindMode, AEnableUpdateAll, bFocusFindDlg);

  //close dialog after 'Select all'/'Mark all', issue #4503
  if Res in [TAppFinderOperation.FindSelectAll, TAppFinderOperation.FindMarkAll] then
    if NCounter>0 then
    begin
      fmFind.Hide;
      Frame:= CurrentFrame;
      if Assigned(Frame) then
      begin
        EditorClearHiAllMarkers(Frame.Editor);
        Frame.SetFocus;
      end;
    end;

  OperationsFocusEditor:= [
    {$ifdef LCLgtk2}
    //Linux gtk2: UpdateAppForSearch don't put focus anywhere (Laz bug?) after "confirmation" form;
    //make workaround
    TAppFinderOperation.ReplaceAll,
    TAppFinderOperation.ReplaceGlobal,
    {$endif}
    TAppFinderOperation.ExtractAll
    ];

  if Res in OperationsFocusEditor then
  begin
    Frame:= CurrentFrame;
    if Assigned(Frame) then
      Frame.SetFocus;
  end;
end;

procedure TfmMain.InitFormFind;
var
  cfg: TAppJsonConfig;
begin
  if not Assigned(fmFind) then
  begin
    fmFind:= TfmFind.Create(Self);
    fmFind.OnResult:= @FindDialogDone2;
    fmFind.OnChangeOptions:= @FindDialogOnChangeOptions;
    fmFind.OnChangeVisible:= @FindDialogOnChangeVisible;
    fmFind.OnFocusEditor:= @FindDialogOnFocusEditor;
    fmFind.OnGetMainEditor:= @FindDialogOnGetMainEditor;
    fmFind.OnResetSearchString:= @FindDialogOnResetSearchString;
    fmFind.OnGetToken:= @FinderOnGetToken;
    fmFind.OnHandleKeyDown:= @FindDialogOnHandleKeyDown;
    fmFind.OnShowResultInStatusbar:= @FindDialogOnShowResultInStatusbar;
    fmFind.OnShowCountInStatusbar:= @FindDialogOnShowCountInStatusbar;
    fmFind.OnPyEvent:= @DoPyEvent_StateFindbar;

    fmFind.Color:= GetAppColor(TAppThemeColor.TabBg);
    fmFind.UpdateCaretPosVar;

    if not UiOps.FindSeparateForm then
    begin
      fmFind.Parent:= PanelMain;
      fmFind.Align:= alBottom;
      fmFind.BorderStyle:= bsNone;
      fmFind.FormStyle:= fsNormal;
    end
    else
    begin
      fmFind.IsNarrow:= true;
      fmFind.Width:= 700;
      fmFind.Constraints.MinWidth:= 400;
      if UiOps.ShowFormsOnTop then
        fmFind.FormStyle:= fsSystemStayOnTop;
      //else: it's already fsStayOnTop
    end;

    cfg:= TAppJsonConfig.Create(nil);
    try
      try
        cfg.Filename:= AppFile_History;

        cfg.GetValue('/list_find', fmFind.edFind.Items, '');
        cfg.GetValue('/list_replace', fmFind.edRep.Items, '');
        if fmFind.IsNarrow then
          FormPosSetFromString(fmFind, cfg.GetValue('/pos/find', ''), false, Screen.DesktopRect);
        fmFind.IsMultiLine:= cfg.GetValue('/finder/mline', false);
        fmFind.IsHiAll:= cfg.GetValue('/finder/hi', false);
        fmFind.IsImmediate:= cfg.GetValue('/finder/im', false);
        fmFind.chkRegexSubst.Checked:= cfg.GetValue('/finder/regex_subst', true);
      except
      end;
    finally
      cfg.Free
    end;
  end;
end;


procedure TfmMain.UpdateFindDialogFromSuggestions;
var
  Frame: TEditorFrame;
  Ed: TATSynEdit;
  StrSel, StrWord, StrSuggested: UnicodeString;
  bMultiLineSel: boolean;
  bPrevImmediate: boolean;
begin
  if not Assigned(fmFind) then
    raise Exception.Create('fmFind is not created');

  Frame:= CurrentFrame;
  if Frame=nil then exit;
  Ed:= Frame.Editor;

  StrSel:= '';
  StrWord:= '';
  StrSuggested:= '';
  bMultiLineSel:= false;

  if Frame.FrameKind=TAppFrameKind.Editor then
  begin
    StrWord:= Ed.TextCurrentWord;

    if not Ed.IsSelColumn then
      StrSel:= Ed.TextSelected;

    if Ed.Carets.Count=1 then
      bMultiLineSel:= Ed.Carets[0].IsMultilineSelection
    else
    if Ed.Carets.Count>1 then
      bMultiLineSel:= Ed.Carets.IsSelectionInAllCarets;

    if (StrSel='') then
    begin
      fmFind.chkInSel.Checked:= false;
      FFinder.OptInSelection:= false;
    end;

    //change Find field only if options SuggestSel/SuggestWord on,
    //else dont touch Find field
    if UiOps.FindSuggestSel and (StrSel<>'') then
    begin
      if not FFinder.OptInSelection then
        StrSuggested:= STrimNewlineChars(StrSel);
    end
    else
    if UiOps.FindSuggestWord and (StrWord<>'') then
      StrSuggested:= StrWord;

    if UiOps.FindSuggestInSelection and bMultiLineSel then
    begin
      fmFind.chkInSel.Checked:= true;
      StrSuggested:= '';
    end
    else
      fmFind.chkInSel.Checked:= FFinder.OptInSelection;
  end;

  fmFind.UpdateCaretPosVar;

  if StrSuggested<>'' then
  begin
    if FFinder.OptRegex then //fmFind.chkRegex.Checked is not yet set
      StrSuggested:= SEscapeRegexSpecialChars(StrSuggested);
    bPrevImmediate:= fmFind.IsImmediate;
    fmFind.IsImmediate:= false; //avoid finder action on showing the find dlg
    fmFind.UpdateInputFind(StrSuggested);
    fmFind.IsImmediate:= bPrevImmediate;
  end;

  fmFind.edFind.DoSelect_All;
  fmFind.edRep.DoSelect_All;
end;

procedure TfmMain.UpdateFindDialogChecks;
begin
  if fmFind=nil then
    raise Exception.Create('fmFind is not created');
  fmFind.chkCase.Checked:= FFinder.OptCase;
  fmFind.chkWords.Checked:= FFinder.OptWords;
  fmFind.chkRegex.Checked:= FFinder.OptRegex;
  fmFind.chkConfirm.Checked:= FFinder.OptConfirmReplace;
  fmFind.chkWrap.Checked:= FFinder.OptWrapped;
  fmFind.bTokens.ItemIndex:= Ord(FFinder.OptTokens);
  fmFind.chkPreserveCase.Checked:= FFinder.OptPreserveCase;
end;

procedure TfmMain.DoDialogFind(AReplaceMode, AUpdateFocus: boolean);
begin
  CloseFormAutoCompletion;

  InitFormFind;
  if AUpdateFocus then
    UpdateFindDialogFromSuggestions;
  UpdateFindDialogChecks;
  UpdateFindDialogParent;
  fmFind.IsReplace:= AReplaceMode;

  //fmFind.UpdateState(false); //param False! to disable jumping to first match on Ctrl+F with find-bar visible
  //(commented 2014.12 coz UpdateState(false) is called in fmFind.FormShow)
  fmFind.Show;
  //fmFind.UpdateFormHeight; //update 'Replace' buttons visibility, when dlg is in the floating form
  //(commented 2014.12 coz UpdateFormHeight is called in fmFind.FormShow)

  if AUpdateFocus then
  begin
    UpdateFindDialogEnabled(CurrentFrame);
    if fmFind.Enabled then
      fmFind.UpdateFocus(true);
  end;
end;

procedure TfmMain.UpdateFindDialogEnabled(Frame: TEditorFrame);
var
  bEnabledPrev, bEnabledNow: boolean;
  bBinaryPrev, bBinaryNow: boolean;
begin
  if fmFind=nil then exit;

  bEnabledPrev:= fmFind.Enabled;
  bEnabledNow:= Frame.FrameKind in [TAppFrameKind.Editor, TAppFrameKind.BinaryViewer];

  bBinaryPrev:= fmFind.ForViewer;
  bBinaryNow:= Frame.FrameKind=TAppFrameKind.BinaryViewer;

  fmFind.Enabled:= bEnabledNow;
  fmFind.ForViewer:= bBinaryNow;

  if (bEnabledPrev<>bEnabledNow) or
    (bBinaryPrev<>bBinaryNow) then
    fmFind.UpdateState(false);
end;

procedure TfmMain.UpdateFindDialogOnTabFocusing(F: TEditorFrame);
//apply "Highlight all matches" on focusing new tab
begin
  if Assigned(fmFind) and fmFind.Visible then
  begin
    if Assigned(F) then
      UpdateFindDialogEnabled(F);

    if fmFind.Enabled then
      fmFind.UpdateState(false); //if 'true', we have issue #3244
                                 //if 'true', also the 'select-all' result is reset on focusing editor
  end;
end;

procedure TfmMain.DoDialogFind_Hide;
var
  Frame: TEditorFrame;
begin
  if Assigned(fmFind) and fmFind.Visible then
  begin
    //handy to reset in-sel, on closing dialog
    FFinder.OptInSelection:= false;

    Frame:= CurrentFrame;
    if Assigned(Frame) then
      Frame.SetFocus;

    fmFind.Hide;
    UpdateAppForSearch(false, false, true, true, false);
  end;
end;


procedure TfmMain.DoDialogFind_Toggle(AReplaceMode, AAndFocus: boolean);
var
  Frame: TEditorFrame;
  bFocusedBottom: boolean;
begin
  bFocusedBottom:= IsFocusedFind;
  InitFormFind;

  if fmFind.Visible then
  begin
    if fmFind.IsReplace<>AReplaceMode then
    begin
      UpdateFindDialogParent;
      fmFind.IsReplace:= AReplaceMode;
    end
    else
      fmFind.Hide;
  end
  else
  begin
    UpdateFindDialogChecks;
    UpdateFindDialogFromSuggestions;
    UpdateFindDialogParent;
    fmFind.IsReplace:= AReplaceMode;
    fmFind.Show;
  end;

  if fmFind.Visible then
  begin
    if AAndFocus then
    begin
      {$ifndef windows}
      //fix focusing Find-dlg when command is called from cmd-palette
      Application.ProcessMessages;
      {$endif}
      fmFind.UpdateFocus(not AReplaceMode);
    end;
  end
  else
  begin
    if bFocusedBottom then
    begin
      Frame:= CurrentFrame;
      if Assigned(Frame) then
        Frame.SetFocus;
    end;
  end;
end;


procedure TfmMain.DoFindFirst;
var
  Frame: TEditorFrame;
  ok, bChanged: boolean;
begin
  Frame:= CurrentFrame;
  if Frame=nil then exit;
  //if Assigned(fmFind) then
  //  FFinder.StrFind:= fmFind.edFind.Text;

  if FFinder.StrFind='' then
  begin
    DoDialogFind(false, true);
    Exit
  end;

  if Frame.FrameKind=TAppFrameKind.BinaryViewer then
  begin
    FindDialogDone(Frame, TAppFinderOperation.FindFirst, false, false);
    exit;
  end;

  FFinder.Editor:= Frame.Editor;
  FFinder.OptBack:= false;
  FFinder.OptFromCaret:= false;
  FFinder.OptDisableOnProgress:= false;

  ok:= FFinder.DoAction_FindOrReplace(false, false, bChanged, true);
  FinderUpdateEditor(false);
  FinderShowResult(ok, false, FFinder);
end;

procedure TfmMain.DoFindNext(ANext: boolean);
var
  Frame: TEditorFrame;
  Op: TAppFinderOperation;
begin
  {
  1) when user _holds_ Shift+F3 in Find dialog, command queue can be filled too much
    (because F3 and Shift+F3 call Application.ProcessMessages when search
    wraps at document edge). FFindIsBusy checking fixes this.
  2) checking was disabled to fix ignored F3 presses:
    https://github.com/Alexey-T/CudaText/issues/5215#issuecomment-1709890637
  }
  //if FFindIsBusy then exit;

  Frame:= CurrentFrame;
  if Frame=nil then exit;
  if Frame.FrameKind=TAppFrameKind.ImageViewer then exit;

  InitFormFind;

  //always update Finder string when "Find next" called. issue #2182
  if not fmFind.edFind.IsEmpty then
    FFinder.StrFind:= fmFind.edFind.Text
  else
  if FFinder.StrFind='' then
  begin
    DoDialogFind(false, true);
    Exit
  end;

  //works for both editor and binary-viewer
  if ANext then
    Op:= TAppFinderOperation.FindNext
  else
    Op:= TAppFinderOperation.FindPrev;
  FindDialogDone(Frame, Op, false, false);

  //add string to history, issue #3326
  fmFind.edFind.DoAddLineToHistory(FFinder.StrFind, UiOps.MaxHistoryEdits);
end;


procedure TfmMain.DoFindNextAndReplace;
var
  Frame: TEditorFrame;
begin
  Frame:= CurrentFrame;
  if Frame=nil then exit;
  if Frame.FrameKind<>TAppFrameKind.Editor then exit;

  InitFormFind;

  //always update Finder string when "Find next" called
  if not fmFind.edFind.IsEmpty then
  begin
    FFinder.StrFind:= fmFind.edFind.Text;
    FFinder.StrReplace:= fmFind.edRep.Text;
  end
  else
  if FFinder.StrFind='' then
  begin
    DoDialogFind(true, true);
    Exit
  end;

  FindDialogDone(Frame, TAppFinderOperation.Replace, false, false);

  //add strings to history
  fmFind.edFind.DoAddLineToHistory(FFinder.StrFind, UiOps.MaxHistoryEdits);
  fmFind.edRep.DoAddLineToHistory(FFinder.StrReplace, UiOps.MaxHistoryEdits);
end;


procedure TfmMain.DoFindMarkingInit(AMode: TAppFinderMarking);
var
  Ed: TATSynEdit;
begin
  Ed:= FFinder.Editor;
  Assert(Assigned(Ed), 'DoFindMarkingInit: Finder.Editor is nil');

  FFindMarkingMode:= AMode;
  FFindMarkingCaret1st:= true;

  case AMode of
    TAppFinderMarking.Selections:
      begin
      end;
    TAppFinderMarking.Markers:
      begin
        Ed.Markers.Clear;
      end;
    TAppFinderMarking.Bookmarks:
      begin
        Ed.DoCommand(cmd_BookmarkClearAll, TATCommandInvoke.AppInternal);
      end;
    else
      begin end;
  end;
end;

procedure TfmMain.DoFindMarkAll(AMode: TAppFinderMarking);
var
  Ed: TATSynEdit;
  SSearchText: UnicodeString;
  bTextFromEditor, bFocusFindDlg: boolean;
  NCount: integer;
begin
  Ed:= CurrentEditor;
  if Ed=nil then exit;

  SSearchText:= '';
  bTextFromEditor:= false;

  if Assigned(fmFind) and fmFind.Visible then
    SSearchText:= fmFind.edFind.Text;

  if SSearchText='' then
  begin
    SSearchText:= EditorPreselectSearchString(Ed);
    bTextFromEditor:= SSearchText<>'';
  end;

  if SSearchText='' then
  begin
    FinderShowResult(false, false, FFinder);
    Exit
  end;

  FFinder.Editor:= Ed;
  FFinder.StrFind:= SSearchText;
  FFinder.OptBack:= false;
  if bTextFromEditor then
    FFinder.OptRegex:= false;
  FFinder.OptFromCaret:= false;
  FFinder.OptDisableOnProgress:= false;

  bFocusFindDlg:= Assigned(fmFind) and fmFind.Visible and fmFind.Enabled and (fmFind.edFind.Focused or fmFind.edRep.Focused);

  DoFindMarkingInit(AMode);
  NCount:= FFinder.DoAction_CountAll(true);
  DoFindMarkingInit(TAppFinderMarking.None);

  FinderUpdateEditor(false);
  MsgStatus(
    Format(msgStatusFoundFragments, [NCount])+
    FinderOptionsToHint(FFinder, false),
    true);

  UpdateAppForSearch(false, false, true, true, bFocusFindDlg); //hide search progressbar
end;


procedure TfmMain.FinderOnProgress(Sender: TObject; const ACurPos, AMaxPos: Int64;
  var AContinue: boolean);
var
  NValue: Int64;
begin
  if AMaxPos<=0 then exit;
  NValue:= ACurPos * 100 div AMaxPos;
  UpdateGlobalProgressbar(NValue, true);
  Application.ProcessMessages;
  if FFindStop or Application.Terminated then
    AContinue:= false;
end;

function TfmMain.FinderOptionsToHint(AFinder: TATEditorFinder; AIsReplace: boolean): string;
const
  Sep=', ';
begin
  Result:= '';
  if AFinder.OptRegex then Result:= Result+msgFinderHintRegex+Sep;
  if AFinder.OptCase then Result:= Result+msgFinderHintCase+Sep;
  if AFinder.OptWords then Result:= Result+msgFinderHintWords+Sep;
  if AFinder.OptBack then Result:= Result+msgFinderHintBack+Sep;
  if AFinder.OptWrapped then Result:= Result+msgFinderHintWrapped+Sep;
  if AFinder.IsSearchWrapped then Result:= Result+msgFinderHintSearchWrapped+Sep;
  if AFinder.OptInSelection then Result:= Result+msgFinderHintInSel+Sep;
  if AFinder.OptFromCaret then Result:= Result+msgFinderHintFromCaret+Sep;
  if AIsReplace then
    if AFinder.OptPreserveCase then Result:= Result+msgFinderHintPresCase+Sep;

  if SEndsWith(Result, Sep) then
    SetLength(Result, Length(Result)-Length(Sep));
  Result:= ' ('+Result+')';
end;

procedure TfmMain.FinderGetHiAllIndexes(AFinder: TATEditorFinder; out AIndex, ACount: integer);
var
  Ed: TATSynEdit;
  CurAttribs: TATMarkers;
  MatchPos: TPoint;
  ItemPtr: PATMarkerItem;
  NAttrCount, iItem: integer;
begin
  AIndex:= -1;
  ACount:= 0;

  if AFinder=nil then exit;
  Ed:= AFinder.Editor;
  //Assert(Assigned(Ed), 'FinderGetHiAllIndexes: Finder.Editor is nil');
  if Ed=nil then exit;

  //take minimum if MatchEdPos / MatchEdEnd
  if IsPosSorted(
    AFinder.MatchEdPos.X,
    AFinder.MatchEdPos.Y,
    AFinder.MatchEdEnd.X,
    AFinder.MatchEdEnd.Y,
    false)
  then
    MatchPos:= AFinder.MatchEdPos
  else
    MatchPos:= AFinder.MatchEdEnd;
  if MatchPos.Y<0 then exit;

  CurAttribs:= Ed.Attribs;
  NAttrCount:= CurAttribs.Count;
  for iItem:= 0 to NAttrCount-1 do
  begin
    ItemPtr:= CurAttribs.ItemPtr(iItem);
    if ItemPtr^.Tag<>UiOps.FindHiAll_TagValue then Continue;
    Inc(ACount);
    if AIndex<0 then
      if ItemPtr^.PosY=MatchPos.Y then
        //exact X pos is OK
        if (ItemPtr^.PosX=MatchPos.X) or
          //not exact X pos can be also OK: when searching backwards for 'dd' with line 'ddd'
          ({AFinder.OptBack and} (ItemPtr^.PosX<MatchPos.X) and (ItemPtr^.PosX+ItemPtr^.SelX>MatchPos.X)) then
          AIndex:= ACount-1;
  end;
end;

function TfmMain.FinderGetHiAllIndexesString(AFinder: TATEditorFinder): string;
var
  NIndex, NCount: integer;
begin
  Result:= '';
  if AFinder=nil then exit;
  FinderGetHiAllIndexes(AFinder, NIndex, NCount);
  if NIndex>=0 then
    Result:= Format(' [%d/%d]', [NIndex+1, NCount])
  else if NCount>0 then
    Result:= Format(' [?/%d]', [NCount]);
end;

procedure TfmMain.FinderShowResult(AFound, AIsReplace: boolean;
  AFinder: TATEditorFinder; AScrollToMatch: boolean=true);
var
  Details: string;
  Ed: TATSynEdit;
begin
  if AFound then
  begin
    Details:= FinderGetHiAllIndexesString(AFinder);
    Details+= FinderOptionsToHint(AFinder, AIsReplace);
    MsgStatus(msgStatusFoundNextMatch+Details, true);

    if AScrollToMatch then
    begin
      Ed:= AFinder.Editor;
      Assert(Assigned(Ed), 'FinderShowResult: Finder.Editor is nil');
      Ed.EndUpdate;

      if not AFinder.OptDisableOnProgress then
        Application.ProcessMessages;

      //not DoGotoPos, we may need to show marker, not caret
      Ed.DoShowPos(
        AFinder.MatchEdPos,
        AFinder.IndentHorz,
        AFinder.IndentVert,
        true,
        true,
        true);
    end;
  end
  else
  if AFinder.IsRegexBad then
    MsgStatusErrorInRegex
  else
    MsgStatus(
      msgCannotFindMatch+FinderOptionsToHint(AFinder, AIsReplace)+': '+Utf8Encode(AFinder.StrFind),
      true);

  if Assigned(fmFind) and fmFind.Visible then
    fmFind.UpdateInputReddishIndicator(AFound);
end;

procedure TfmMain.FinderShowResultSimple(AFound: boolean; AFinder: TATEditorFinder);
var
  Details: string;
begin
  if AFound then
  begin
    Details:= FinderGetHiAllIndexesString(AFinder);
    MsgStatus(msgStatusFoundNextMatch+Details, true);
  end
  else
    FinderShowResult(false, false, AFinder);
end;

procedure TfmMain.FinderOnConfirmReplace_API(Sender: TObject; APos1,
  APos2: TPoint; AForMany: boolean; var AConfirm, AContinue: boolean;
  var AReplacement: UnicodeString);
const
  HOWREP_CANCEL  = 0;
  HOWREP_REPLACE = 1;
  HOWREP_SKIP    = 2;
var
  ThisFinder: TATEditorFinder;
  Callback: string;
  EventRes: TAppPyEventResult;
  Ed: TATSynEdit;
begin
  AConfirm:= true;
  AContinue:= true;

  ThisFinder:= Sender as TATEditorFinder;
  Ed:= ThisFinder.Editor;
  Assert(Assigned(Ed), 'FinderOnConfirmReplace_API: Finder.Editor is nil');

  Callback:= ThisFinder.CallbackString;
  if Callback='' then exit;

  Ed.DoCaretSingle(APos1.X, APos1.Y, APos2.X, APos2.Y);
  Ed.DoShowPos(APos1, ThisFinder.IndentHorz, ThisFinder.IndentVert, true, false, true);
  Ed.Update(true);

  EventRes:= DoPyCallbackFromAPI_2(Callback, Ed,
    [
    AppVariant(APos1.X),
    AppVariant(APos1.Y),
    AppVariant(APos2.X),
    AppVariant(APos2.Y),
    AppVariant(AReplacement)
    ]);

  case EventRes.Val of
    TAppPyEventValue.Int:
      case EventRes.Int of
        HOWREP_CANCEL:
          begin
            AContinue:= false;
            AConfirm:= false;
          end;
        HOWREP_SKIP:
          begin
            AContinue:= true;
            AConfirm:= false;
          end;
      end;
    TAppPyEventValue.Str:
      AReplacement:= EventRes.Str;
  end;
end;

procedure TfmMain.FinderOnConfirmReplace(Sender: TObject; APos1, APos2: TPoint;
  AForMany: boolean; var AConfirm, AContinue: boolean;
  var AReplacement: UnicodeString);
var
  Res: TModalResult;
  Ed: TATSynEdit;
  Pnt: TPoint;
begin
  case FFindConfirmAll of
    mrYesToAll: begin AConfirm:= true; exit end;
    mrNoToAll: begin AConfirm:= false; exit end;
  end;

  Ed:= (Sender as TATEditorFinder).Editor;

  with Ed.Carets[0] do
  begin
    PosX:= APos1.X;
    PosY:= APos1.Y;
    EndX:= APos2.X;
    EndY:= APos2.Y;
  end;

  Ed.EndUpdate;
  Ed.DoGotoPos(
    APos1,
    APos2,
    UiOps.FindIndentHorz,
    UiOps.FindIndentVert,
    false{place caret},
    TATEditorActionIfFolded.Unfold{unfold}
    );
  Ed.Update(true);

  if fmConfirmReplace=nil then
    fmConfirmReplace:= TfmConfirmReplace.Create(Self);

  fmConfirmReplace.MsgLineNumber:= APos1.Y+1;
  fmConfirmReplace.bYesAll.Enabled:= AForMany;
  fmConfirmReplace.bNoAll.Enabled:= AForMany;
  if Assigned(fmFind) then
  begin
    Pnt:= fmFind.ClientToScreen(Point(0, 0));
    fmConfirmReplace.Left:= Pnt.X;
    fmConfirmReplace.Top:= Pnt.Y;
    //fmConfirmReplace.Width:= fmFind.Width;
  end;
  Res:= fmConfirmReplace.ShowModal;

  Ed.BeginUpdate;
  AConfirm:= Res in [mrYes, mrYesToAll];
  AContinue:= Res<>mrNoToAll;
  if Res in [mrYesToAll, mrNoToAll] then
    FFindConfirmAll:= Res;
end;

procedure TfmMain.MsgStatusErrorInRegex;
begin
  MsgBox(
    msgStatusBadRegex+#10+
    Utf8Encode(FFinder.StrFind)+#10+
    FFinder.RegexErrorMsg,
    MB_OK or MB_ICONERROR);
end;

procedure TfmMain.FinderOnGetToken(Sender: TObject; AX, AY: integer; out AKind: TATTokenKind);
begin
  AKind:= EditorGetTokenKind(Sender as TATSynEdit, AX, AY)
end;

procedure TfmMain.FinderOnWrapAtEdge(Sender: TObject);
begin
  if UiOps.FindWrapAtEdge_ThemeItem='' then exit;
  if Assigned(fmFind) and fmFind.Visible then
    fmFind.IsInputColored:= true;
end;


procedure TfmMain.FinderUpdateEditor(AUpdateText: boolean; AUpdateStatusbar: boolean=true);
var
  Ed: TATSynEdit;
begin
  Ed:= FFinder.Editor;
  Assert(Assigned(Ed), 'FinderUpdateEditor: Finder.Editor is nil');

  if AUpdateText then
    Ed.DoEventChange(FFinder.MatchEdPos.Y);

  Ed.Update(AUpdateText);

  {
  //2021.05: lot of DoShowPos calls already for finder
  //not DoGotoPos, we may need to show marker, not caret
  Ed.DoShowPos(
    FFinder.MatchEdPos,
    FFinder.IndentHorz,
    FFinder.IndentVert,
    true,
    true
    );
    }

  if AUpdateStatusbar then
    UpdateStatusbar;
end;

procedure TfmMain.DoFindCurrentWordOrSel(Ed: TATSynEdit; ANext, AWordOrSel: boolean);
var
  Str: UnicodeString;
  ok, bCase, bHiAll: boolean;
begin
  bCase:= true;
  case UiOps.FindCurrentWordCaseSensitive of
    TUiOpsFindCaseSensitive.CaseIgnore:
      bCase:= false;
    TUiOpsFindCaseSensitive.CaseSens:
      bCase:= true;
    TUiOpsFindCaseSensitive.FromDialog:
      begin
        if Assigned(fmFind) then
          bCase:= fmFind.chkCase.Checked;
      end;
  end;

  //'Hi' dialog option messes up with our new search, issue #3572
  bHiAll:= false;
  if Assigned(fmFind) then
  begin
    bHiAll:= fmFind.chkHiAll.Checked;
    if bHiAll then
    begin
      fmFind.chkHiAll.Checked:= false;
      fmFind.UpdateHiAll(false); //param False to fix issue #5933
    end;
  end;

  ok:= EditorFindCurrentWordOrSel(Ed, ANext, AWordOrSel, bCase, FFinder.OptWrapped, Str);
  FinderShowResultSimple(ok, FFinder);

  //sync Find dialog with text
  if Assigned(fmFind) then
  begin
    if fmFind.edFind.Text<>Str then //issue #3353
      fmFind.UpdateInputFind(Str);
  end;

  if FFinder.OptRegex then
    FFinder.StrFind:= SEscapeRegexSpecialChars(Str)
  else
    FFinder.StrFind:= Str;

  if Assigned(fmFind) then
    if bHiAll then
    begin
      fmFind.chkHiAll.Checked:= true;
      //fmFind.UpdateHiAll(true); //breaks issue #3572
    end;
end;


function TfmMain.FindDialog_GetButtonsPyDict: PPyObject;
begin
  with AppPython.Engine do
  begin
    if not Assigned(fmFind) then
      exit(ReturnNone);
    Result:= Py_BuildValue('{sOsOsOsOsOsOsOsOsOsOsOsOsOsOsOsOsOsOsO}',
      'FindFirst', PyHelper_ObjectHandle(fmFind.bFindFirst),
      'FindNext', PyHelper_ObjectHandle(fmFind.bFindNext),
      'FindPrev', PyHelper_ObjectHandle(fmFind.bFindPrev),
      'Rep', PyHelper_ObjectHandle(fmFind.bRep),
      'RepAll', PyHelper_ObjectHandle(fmFind.bRepAll),
      'RepGlobal', PyHelper_ObjectHandle(fmFind.bRepGlobal),
      'More', PyHelper_ObjectHandle(fmFind.bMore),
      'Tokens', PyHelper_ObjectHandle(fmFind.bTokens),
      'Case', PyHelper_ObjectHandle(fmFind.chkCase),
      'Words', PyHelper_ObjectHandle(fmFind.chkWords),
      'RegEx', PyHelper_ObjectHandle(fmFind.chkRegex),
      'RegExSubst', PyHelper_ObjectHandle(fmFind.chkRegexSubst),
      'InSel', PyHelper_ObjectHandle(fmFind.chkInSel),
      'Wrap', PyHelper_ObjectHandle(fmFind.chkWrap),
      'MulLine', PyHelper_ObjectHandle(fmFind.chkMulLine),
      'Confirm', PyHelper_ObjectHandle(fmFind.chkConfirm),
      'Hi', PyHelper_ObjectHandle(fmFind.chkHiAll),
      'Im', PyHelper_ObjectHandle(fmFind.chkImmediate),
      'PreserveCase', PyHelper_ObjectHandle(fmFind.chkPreserveCase)
      );
  end;
end;

function TfmMain.FindDialog_GetOptionsPyDict: PPyObject;
var
  oFind, oRep,
  oFindHist, oRepHist,
  bCase, bWord, bRegex, bRegexSubst, bCfm,
  bInSel, bWrap, bMulLine, bHiAll, bImmediate, bPresCase, oTokens,
  oDlgIsRep, oDlgFocus: PPyObject;
begin
  with AppPython.Engine do
  begin
    if not Assigned(FFinder) then
      exit(ReturnNone);

    oFind:= ReturnNone;
    oRep:= ReturnNone;
    oFindHist:= ReturnNone;
    oRepHist:= ReturnNone;
    bCase:= ReturnNone;
    bWord:= ReturnNone;
    bRegex:= ReturnNone;
    bRegexSubst:= ReturnNone;
    bCfm:= ReturnNone;
    bInSel:= ReturnNone;
    bWrap:= ReturnNone;
    bMulLine:= ReturnNone;
    bHiAll:= ReturnNone;
    bImmediate:= ReturnNone;
    bPresCase:= ReturnNone;
    oTokens:= ReturnNone;
    oDlgIsRep:= ReturnNone;
    oDlgFocus:= ReturnNone;

    if Assigned(fmFind) then
    begin
      oFind:= PyUnicodeFromString(fmFind.edFind.Text);
      oRep:= PyUnicodeFromString(fmFind.edRep.Text);
      oFindHist:= StringsToPyList(fmFind.edFind.Items);
      oRepHist:= StringsToPyList(fmFind.edRep.Items);
      bCase:= PyBool_FromLong(Ord(fmFind.chkCase.Checked));
      bWord:= PyBool_FromLong(Ord(fmFind.chkWords.Checked));
      bRegex:= PyBool_FromLong(Ord(fmFind.chkRegex.Checked));
      bRegexSubst:= PyBool_FromLong(Ord(fmFind.chkRegexSubst.Checked));
      bCfm:= PyBool_FromLong(Ord(fmFind.chkConfirm.Checked));
      bInSel:= PyBool_FromLong(Ord(fmFind.chkInSel.Checked));
      bWrap:= PyBool_FromLong(Ord(fmFind.chkWrap.Checked));
      bMulLine:= PyBool_FromLong(Ord(fmFind.chkMulLine.Checked));
      bHiAll:= PyBool_FromLong(Ord(fmFind.IsHiAll));
      bImmediate:= PyBool_FromLong(Ord(fmFind.IsImmediate));
      bPresCase:= PyBool_FromLong(Ord(fmFind.chkPreserveCase.Checked));
      oTokens:= PyLong_FromLong(Ord(fmFind.bTokens.ItemIndex));
      oDlgIsRep:= PyBool_FromLong(Ord(fmFind.IsReplace));
      if fmFind.edFind.Focused then
        oDlgFocus:= PyUnicodeFromString('edFind')
      else if fmFind.edRep.Focused then
        oDlgFocus:= PyUnicodeFromString('edRep');
    end;

    Result:= Py_BuildValue('{sOsOsOsOsOsOsOsOsOsOsOsOsOsOsOsOsOsOsOsOsOsOsOsOsOsOsOsOsOsOsOsO}',
      'find', PyUnicodeFromString(FFinder.StrFind),
      'rep', PyUnicodeFromString(FFinder.StrReplace),

      'find_d', oFind,
      'rep_d', oRep,
      'find_h', oFindHist,
      'rep_h', oRepHist,

      'op_case', PyBool_FromLong(Ord(FFinder.OptCase)),
      'op_back', PyBool_FromLong(Ord(FFinder.OptBack)),
      'op_word', PyBool_FromLong(Ord(FFinder.OptWords)),
      'op_regex', PyBool_FromLong(Ord(FFinder.OptRegex)),
      'op_regex_subst', PyBool_FromLong(Ord(FFinder.OptRegexSubst)),
      'op_cfm', PyBool_FromLong(Ord(FFinder.OptConfirmReplace)),
      'op_fromcaret', PyBool_FromLong(Ord(FFinder.OptFromCaret)),
      'op_insel', PyBool_FromLong(Ord(FFinder.OptInSelection)),
      'op_wrap', PyBool_FromLong(Ord(FFinder.OptWrapped)),
      'op_wrap_c', PyBool_FromLong(Ord(FFinder.OptWrappedConfirm)),
      'op_tokens', PyLong_FromLong(Ord(FFinder.OptTokens)),
      'op_prescase', PyBool_FromLong(Ord(FFinder.OptPreserveCase)),

      'op_case_d', bCase,
      'op_word_d', bWord,
      'op_regex_d', bRegex,
      'op_regex_subst_d', bRegexSubst,
      'op_cfm_d', bCfm,
      'op_insel_d', bInSel,
      'op_wrap_d', bWrap,
      'op_mulline_d', bMulLine,
      'op_tokens_d', oTokens,
      'op_hi_d', bHiAll,
      'op_im_d', bImmediate,
      'op_prescase_d', bPresCase,

      'is_replace', oDlgIsRep,
      'focus', oDlgFocus
      );
  end;
end;


procedure TfmMain.FindDialog_ApplyOptionsString(const AText: string);
var
  Sep, Sep2: TATStringSeparator;
  SItem, SKey, SValue: string;
  bListValue: boolean;
begin
  //text is '{key1:value1;key2:value2}' from to_str()
  Sep.Init(SDeleteCurlyBrackets(AText), #1);
  while Sep.GetItemStr(SItem) do
  begin
    SSplitByChar(SItem, ':', SKey, SValue);

    bListValue:= (SKey='find_h') or (SKey='rep_h');
    if not bListValue then
      SValue:= StringReplace(SValue, #2, ',', [rfReplaceAll]);

    case SKey of
      'find':
        FFinder.StrFind:= UTF8Decode(SValue);
      'rep':
        FFinder.StrReplace:= UTF8Decode(SValue);

      'find_d':
        begin
          if Assigned(fmFind) then
          begin
            fmFind.UpdateInputFind(UTF8Decode(SValue));
            fmFind.UpdateState(false);
            fmFind.DoOnChange;
          end;
        end;

      'rep_d':
        begin
          if Assigned(fmFind) then
          begin
            fmFind.UpdateInputReplace(UTF8Decode(SValue));
            fmFind.UpdateState(false);
            fmFind.DoOnChange;
          end;
        end;

      'find_h':
        begin
          if Assigned(fmFind) then
          begin
            fmFind.edFind.Items.Clear;
            Sep2.Init(SValue);
            while Sep2.GetItemStr(SItem) do
            begin
              SItem:= StringReplace(SItem, #2, ',', [rfReplaceAll]);
              fmFind.edFind.Items.Add(SItem);
            end;
          end;
        end;

      'rep_h':
        begin
          if Assigned(fmFind) then
          begin
            fmFind.edRep.Items.Clear;
            Sep2.Init(SValue);
            while Sep2.GetItemStr(SItem) do
            begin
              SItem:= StringReplace(SItem, #2, ',', [rfReplaceAll]);
              fmFind.edRep.Items.Add(SItem);
            end;
          end;
        end;

      'op_case':
        FFinder.OptCase:= AppStrToBool(SValue);
      'op_back':
        FFinder.OptBack:= AppStrToBool(SValue);
      'op_word':
        FFinder.OptWords:= AppStrToBool(SValue);
      'op_regex':
        FFinder.OptRegex:= AppStrToBool(SValue);
      'op_regex_subst':
        FFinder.OptRegexSubst:= AppStrToBool(SValue);
      'op_cfm':
        FFinder.OptConfirmReplace:= AppStrToBool(SValue);
      'op_fromcaret':
        FFinder.OptFromCaret:= AppStrToBool(SValue);
      'op_insel':
        FFinder.OptInSelection:= AppStrToBool(SValue);
      'op_wrap':
        FFinder.OptWrapped:= AppStrToBool(SValue);
      'op_wrap_c':
        FFinder.OptWrappedConfirm:= AppStrToBool(SValue);
      'op_tokens':
        FFinder.OptTokens:= TATFinderTokensAllowed(StrToIntDef(SValue, 0));
      'op_prescase':
        FFinder.OptPreserveCase:= AppStrToBool(SValue);

      'op_case_d':
        begin
          if Assigned(fmFind) then
          begin
            fmFind.chkCase.Checked:= AppStrToBool(SValue);
            fmFind.UpdateState(false);
            fmFind.DoOnChange;
          end;
        end;
      'op_word_d':
        begin
          if Assigned(fmFind) then
          begin
            fmFind.chkWords.Checked:= AppStrToBool(SValue);
            fmFind.UpdateState(false);
            fmFind.DoOnChange;
          end;
        end;
      'op_regex_d':
        begin
          if Assigned(fmFind) then
          begin
            fmFind.chkRegex.Checked:= AppStrToBool(SValue);
            fmFind.UpdateState(false);
            fmFind.DoOnChange;
          end;
        end;
      'op_regex_subst_d':
        begin
          if Assigned(fmFind) then
          begin
            fmFind.chkRegexSubst.Checked:= AppStrToBool(SValue);
            fmFind.UpdateState(false);
            fmFind.DoOnChange;
          end;
        end;
      'op_cfm_d':
        begin
          if Assigned(fmFind) then
          begin
            fmFind.chkConfirm.Checked:= AppStrToBool(SValue);
            fmFind.UpdateState(false);
            fmFind.DoOnChange;
          end;
        end;
      'op_insel_d':
        begin
          if Assigned(fmFind) then
          begin
            fmFind.chkInSel.Checked:= AppStrToBool(SValue);
            fmFind.UpdateState(false);
            fmFind.DoOnChange;
          end;
        end;
      'op_wrap_d':
        begin
          if Assigned(fmFind) then
          begin
            fmFind.chkWrap.Checked:= AppStrToBool(SValue);
            fmFind.UpdateState(false);
            fmFind.DoOnChange;
          end;
        end;
      'op_mulline_d':
        begin
          if Assigned(fmFind) then
          begin
            fmFind.IsMultiLine:= AppStrToBool(SValue);
            fmFind.DoOnChange;
          end;
        end;
      'op_tokens_d':
        begin
          if Assigned(fmFind) then
          begin
            fmFind.bTokens.ItemIndex:= StrToIntDef(SValue, 0);
            fmFind.bTokens.Invalidate;
          end;
        end;
      'op_hi_d':
        begin
          if Assigned(fmFind) then
          begin
            fmFind.IsHiAll:= AppStrToBool(SValue);
            fmFind.chkHiAll.Invalidate;
            fmFind.DoOnChange;
          end;
        end;
      'op_im_d':
        begin
          if Assigned(fmFind) then
          begin
            fmFind.IsImmediate:= AppStrToBool(SValue);
            fmFind.chkImmediate.Invalidate;
            fmFind.DoOnChange;
          end;
        end;
      'op_prescase_d':
        begin
          if Assigned(fmFind) then
          begin
            fmFind.chkPreserveCase.Checked:= AppStrToBool(SValue);
            fmFind.DoOnChange;
          end;
        end;

      'is_replace':
        begin
          if Assigned(fmFind) then
            fmFind.IsReplace:= AppStrToBool(SValue);
        end;

      'focus':
        begin
          if Assigned(fmFind) and fmFind.Visible and fmFind.Enabled then
          begin
            if (SValue=fmFind.edFind.Name) and fmFind.edFind.CanFocus then
              fmFind.edFind.SetFocus
            else
            if (SValue=fmFind.edRep.Name) and fmFind.edRep.CanFocus then
              fmFind.edRep.SetFocus;
          end;
        end;
    end;
  end;
end;


procedure TfmMain.DoFindActionFromString(const AStr: string);
var
  Sep: TATStringSeparator;
  SAction, SFind, SRep, SOpt: string;
  SOptOld: string;
begin
  Sep.Init(AStr, #1);
  Sep.GetItemStr(SAction);
  Sep.GetItemStr(SFind);
  Sep.GetItemStr(SRep);
  Sep.GetItemStr(SOpt);

  //CR LF -> LF
  SFind:= StringReplace(SFind, #13#10, #10, [rfReplaceAll]);
  SRep:= StringReplace(SRep, #13#10, #10, [rfReplaceAll]);

  FFinder.Editor:= CurrentEditor;
  FFinder.StrFind:= UTF8Decode(SFind);
  FFinder.StrReplace:= UTF8Decode(SRep);

  SOptOld:= FinderOptionsToString(FFinder);
  FinderOptionsFromString(FFinder, SOpt);
  FindDialogDone(
    nil, //skip dialog
    AppFinderOperationFromString(SAction),
    false, //'false' to solve issue #3303
    false
    );
  FinderOptionsFromString(FFinder, SOptOld);
end;

procedure TfmMain.FindDialogOnChangeOptions(Sender: TObject);
begin
  //options are used for some commands
  //apply options immediately, so user dont need to do a search to apply them
  with fmFind do
  begin
    FFinder.OptCase:= chkCase.Checked;
    FFinder.OptWords:= chkWords.Checked;
    FFinder.OptRegex:= chkRegex.Checked;
    FFinder.OptRegexSubst:= chkRegexSubst.Checked;
    FFinder.OptWrapped:= chkWrap.Checked;
    FFinder.OptInSelection:= chkInSel.Checked;
    FFinder.OptConfirmReplace:= chkConfirm.Checked;
    FFinder.OptTokens:= TATFinderTokensAllowed(bTokens.ItemIndex);
    FFinder.OptPreserveCase:= chkPreserveCase.Checked;
  end;
end;

procedure TfmMain.FindDialogOnFocusEditor(Sender: TObject);
begin
  DoFocusEditor(CurrentEditor);
end;

function TfmMain.FindDialogOnHandleKeyDown(AKey: word; AShiftState: TShiftState): boolean;
var
  Ed: TATSynEdit;
  NCmd: integer;
  KeyHistory: TATKeyArray;
begin
  Result:= false;
  Ed:= CurrentEditor;
  if Ed=nil then exit;

  KeyHistory.Clear;
  NCmd:= Ed.Keymap.GetCommandFromShortcut(ShortCut(AKey, AShiftState), KeyHistory);
  if IsCommandHandledFromFindDialog(NCmd) then
  begin
    Ed.DoCommand(NCmd, TATCommandInvoke.Hotkey);
    Result:= true;
  end;
end;

procedure TfmMain.FindDialogOnShowResultInStatusbar(AFound: boolean);
begin
  FinderShowResult(AFound, false, FFinder, false{AScrollToMatch});
end;

procedure TfmMain.FindDialogOnShowCountInStatusbar(ACount: integer; AInVisibleArea: boolean);
var
  S: string;
begin
  S:= Format(msgStatusFoundFragments, [ACount]);
  if AInVisibleArea then
    S+= ' '+msgStatusbarFoundInVisibleArea;
  MsgStatus(S, true);
end;

procedure TfmMain.FindDialogOnChangeVisible(Sender: TObject);
begin
  UpdateSidebarButtonFind;
end;

procedure TfmMain.UpdateFindDialogParent;
var
  F: TCustomForm;
begin
  if UiOps.FindSeparateForm then exit;
  if fmFind=nil then exit;
  if fmFind.Parent=nil then exit;

  F:= Screen.ActiveForm;
  if (F=nil) or (F=Self) then
    fmFind.Parent:= Self.PanelMain
  else
  if (F=FFormFloating1) or
    (F=FFormFloating2) or
    (F=FFormFloating3) then
    fmFind.Parent:= F;
end;

