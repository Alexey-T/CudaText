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
  case FFindMarkingMode of
    markingNone:
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

    markingSelections:
      begin
        if FFindMarkingCaret1st then
        begin
          FFindMarkingCaret1st:= false;
          Ed.Carets.Clear;
        end;
        Ed.Carets.Add(APos2.X, APos2.Y, APos1.X, APos1.Y);
        //Ed.Carets.Sort; //better comment, to not merge many selections, on "Select all"
      end;

    markingMarkers:
      begin
        //show marker line, if range is one-liner
        if APos2.Y=APos1.Y then
          NLen:= APos1.X-APos2.X
        else
          NLen:= 0;
        Ed.Markers.Add(APos2.X, APos2.Y, 0, 0, 0, nil, 0, mmmShowInTextOnly, NLen);
      end;

    markingBookmarks:
      begin
        ed.BookmarkSetForLine(APos1.Y, 1, '', bmadOption, true, 0);
      end;
  end;
end;


function TfmMain.FinderReplaceAll(Ed: TATSynEdit; AResetCaret: boolean): integer;
var
  NTop, NLeft: integer;
  NCaretX, NCaretY: integer;
begin
  Result:= 0;
  if Ed.ModeReadOnly then exit;

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
    FFinder.Editor:= Ed;
    Result:= FFinder.DoAction_ReplaceAll;
  finally
    Ed.Strings.EndUndoGroup;
    Ed.Enabled:= true;
    Ed.EndUpdate; //editor can be locked during replace confirmation
  end;

  FinderUpdateEditor(Result>0, false);

  Ed.DoCaretSingle(NCaretX, NCaretY);
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


procedure TfmMain.FindDialogDone2(Sender: TObject; Res: TAppFinderOperation);
begin
  FindDialogDone(Sender, Res, true);
end;

procedure TfmMain.FindDialogDone(Sender: TObject; Res: TAppFinderOperation; AEnableUpdateAll: boolean);
var
  Category: TAppFinderOperationCategory;
  ListFound: TStringList;
  Frame, FrameNew, FramePrev: TEditorFrame;
  Ed: TATSynEdit;
  ok, bChanged, bDoLock, bFindMode, bChangeFrame: boolean;
  bHiliteAll: boolean;
  NCounter, IndexFrame: integer;
  NTime: integer;
  NTick: QWord;
begin
  InitFormFind;
  Frame:= CurrentFrame;
  Category:= cAppFinderOperationCategory[Res];
  if Category=afcNone then exit;
  bHiliteAll:= false;

  if Category in [afcFind, afcReplaceOne] then
  begin
    if Frame.FrameKind=efkImageViewer then exit;
    if Frame.Editor.Carets.Count=0 then
    begin
      MsgStatus(msgCannotFindWithoutCaret);
      exit;
    end;
  end;

  if Category=afcReplaceOne then
  begin
    if Frame.FrameKind<>efkEditor then exit;
    if Frame.Editor.ModeReadOnly then exit;
  end;

  //keep focus in Find field, else focus find or rep field
  if fmFind.edFind.Focused then
    bFindMode:= true
  else
  if fmFind.edRep.Focused then
    bFindMode:= false
  else
    bFindMode:= Category=afcFind;

  if Category=afcCloseDlg then
  begin
    //handy to reset in-sel, on closing dialog
    FFinder.OptInSelection:= false;

    if IsFocusedFind then
      Frame.SetFocus;
    fmFind.Hide;
    fmFind.ClearHiAll;
    UpdateAppForSearch(false, false, bFindMode, AEnableUpdateAll);
    Exit;
  end;

  //Sender=nil allows to not use dialog
  if Assigned(Sender) then
  with fmFind do
  begin
    if edFind.Text='' then Exit;
    FFinder.StrFind:= edFind.Text;
    FFinder.StrReplace:= edRep.Text;
    FFinder.OptBack:= false;
    FFinder.OptCase:= chkCase.Checked;
    FFinder.OptWords:= chkWords.Checked;
    FFinder.OptRegex:= chkRegex.Checked;
    FFinder.OptRegexSubst:= chkRegexSubst.Checked;
    FFinder.OptWrapped:= chkWrap.Checked;
    FFinder.OptInSelection:= chkInSel.Checked;
    FFinder.OptFromCaret:= (Res=afoFindNext) or (Res=afoFindPrev) or (Res=afoReplace);
    FFinder.OptConfirmReplace:= chkConfirm.Checked;
    FFinder.OptTokens:= TATFinderTokensAllowed(bTokens.ItemIndex);
    bHiliteAll:= chkHiAll.Checked;
  end;

  if Frame.FrameKind=efkBinaryViewer then
  begin
    UpdateAppForSearch(true, false, true, AEnableUpdateAll);
    Application.ProcessMessages;

    case Res of
      afoFindFirst:
        begin
          ok:= Frame.BinaryFindFirst(FFinder, bHiliteAll);
          FinderShowResultSimple(ok, FFinder);
        end;

      afoFindNext,
      afoFindPrev:
        begin
          if (not Frame.Binary.SearchStarted) or (Frame.Binary.SearchString<>FFinder.StrFind) then
            ok:= Frame.BinaryFindFirst(FFinder, bHiliteAll)
          else
            ok:= Frame.BinaryFindNext(Res=afoFindPrev);
          FinderShowResultSimple(ok, FFinder);
        end;
    end;

    UpdateAppForSearch(false, false, true, AEnableUpdateAll);
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
        FFinder.StrFind+#1+
        FFinder.StrReplace+#1+
        FinderOptionsToString(FFinder)
        );

  bDoLock:=
    //(Res=afoReplace) or //don't lock for single replace, issue #1216
    //(Res=afoReplaceStop) or
    (Res=afoReplaceAll) or
    (Res=afoReplaceGlobal);
  UpdateAppForSearch(true, bDoLock, bFindMode, AEnableUpdateAll);

  if bDoLock then
  begin
    //add undo group, so Undo won't take too much actions
    FFinder.Editor.Strings.BeginUndoGroup;
    FFinder.Editor.Strings.EndUndoGroup;
  end;

  case Res of
    afoFindFirst,
    afoFindNext,
    afoFindPrev:
    begin
      FFinder.OptBack:= (Res=afoFindPrev);
      ok:= FFinder.DoAction_FindOrReplace(
        false, false, bChanged, true);
      FinderUpdateEditor(false);
      FinderShowResult(ok, FFinder);
    end;

    afoReplace,
    afoReplaceStop:
    begin
      //replace match
      ok:= FFinder.DoAction_ReplaceSelected(true);
      //after match is replaced, do find-next
      if Res=afoReplace then
        if ok then
          ok:= FFinder.DoAction_FindOrReplace(false, false, bChanged, true);
      FinderUpdateEditor(true);
      FinderShowResult(ok, FFinder);
    end;

    afoReplaceAll:
    begin
      NTick:= GetTickCount64;
      NCounter:= FinderReplaceAll(FFinder.Editor, false);
      NTime:= (GetTickCount64-NTick) div 1000;
      UpdateStatusbar;
      FinderShowReplaceReport(NCounter, NTime);
    end;

    afoReplaceGlobal:
    begin
      NCounter:= 0;
      NTick:= GetTickCount64;
      FramePrev:= Frame;
      bChangeFrame:= FFinder.OptConfirmReplace;

      for IndexFrame:= 0 to FrameCount-1 do
      begin
        FrameNew:= Frames[IndexFrame];
        if FrameNew.FrameKind<>efkEditor then Continue;

        if bChangeFrame then
          SetFrame(FrameNew);

        NCounter+= FinderReplaceAll(FrameNew.Ed1, true);
        if not FrameNew.EditorsLinked then
          NCounter+= FinderReplaceAll(FrameNew.Ed2, true);
      end;

      NTime:= (GetTickCount64-NTick) div 1000;
      if bChangeFrame then
        SetFrame(FramePrev);
      UpdateStatusbar;
      FinderShowReplaceReport(NCounter, NTime);
    end;

    afoCountAll:
    begin
      DoFindMarkingInit(markingNone);
      NCounter:= FFinder.DoAction_CountAll(false);
      if (NCounter=0) and FFinder.IsRegexBad then
        MsgStatusErrorInRegex
      else
        MsgStatus(
          Format(msgStatusFindCount, [Utf8Encode(FFinder.StrFind), NCounter])+FinderOptionsToHint(FFinder),
          true);
    end;

    afoExtractAll:
    begin
      ListFound:= TStringList.Create;
      try
        FFinder.DoAction_ExtractAll(false, ListFound, true, FFinder.OptCase, dupIgnore);
        if ListFound.Count>0 then
        begin
          FrameNew:= DoFileOpen('', '');
          FrameNew.TabCaptionUntitled:= msgFinderRegexMatchesNumbered;
          Ed:= FrameNew.Editor;
          try
            Ed.Strings.LoadFromStrings(ListFound, cEndUnix);
            MsgStatus(Format(msgStatusFoundFragments, [ListFound.Count]), true);
          finally
            Ed.DoEventChange;
          end;
        end
        else
          FinderShowResult(false, FFinder);
      finally
        FreeAndNil(ListFound);
      end;
    end;

    afoFindSelectAll:
    begin
      DoFindMarkingInit(markingSelections);
      NCounter:= FFinder.DoAction_CountAll(true);
      DoFindMarkingInit(markingNone);
      FinderUpdateEditor(false);
      if (NCounter=0) and FFinder.IsRegexBad then
        MsgStatusErrorInRegex
      else
        MsgStatus(
          Format(msgStatusFindCount, [Utf8Encode(FFinder.StrFind), NCounter]) +
          FinderOptionsToHint(FFinder),
          true);
    end;

    afoFindMarkAll:
    begin
      DoFindMarkingInit(markingMarkers);
      NCounter:= FFinder.DoAction_CountAll(true);
      DoFindMarkingInit(markingNone);
      FinderUpdateEditor(false);
      if (NCounter=0) and FFinder.IsRegexBad then
        MsgStatusErrorInRegex
      else
        MsgStatus(
          Format(msgStatusFindCount, [Utf8Encode(FFinder.StrFind), NCounter]) +
          FinderOptionsToHint(FFinder),
          true);
    end;
  end; //case Res of

  UpdateAppForSearch(false, false, bFindMode, AEnableUpdateAll);

  {$ifdef LCLgtk2}
  //Linux gtk2: UpdateAppForSearch don't put focus anywhere (Laz bug?) after "confirmation" form
  //make workaround
  if (Res=afoReplaceAll) or
     (Res=afoReplaceGlobal) then
  begin
    Frame:= CurrentFrame;
    if Assigned(Frame) then
      Frame.SetFocus;
  end;
  {$endif}
end;

procedure TfmMain.InitFormFind;
var
  cfg: TJsonConfig;
begin
  if not Assigned(fmFind) then
  begin
    fmFind:= TfmFind.Create(Self);
    fmFind.OnResult:= @FindDialogDone2;
    fmFind.OnChangeOptions:= @DoFindOptions_OnChange;
    fmFind.OnFocusEditor:= @FindDialogFocusEditor;
    fmFind.OnGetMainEditor:= @FindDialogGetMainEditor;
    fmFind.OnGetToken:= @FinderOnGetToken;
    fmFind.OnShowMatchesCount:= @FinderShowMatchesCount;
    fmFind.OnHandleKeyDown:= @FinderHandleKeyDown;
    fmFind.Color:= GetAppColor(apclTabBg);
    fmFind.UpdateInitialCaretPos;

    UiOps.HotkeyFindDialog:= ShortcutToText(AppKeymapMain.GetShortcutFromCommand(cmd_DialogFind));
    UiOps.HotkeyReplaceDialog:= ShortcutToText(AppKeymapMain.GetShortcutFromCommand(cmd_DialogReplace));

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

    cfg:= TJSONConfig.Create(nil);
    try
      try
        cfg.Filename:= AppFile_History;

        cfg.GetValue('/list_find', fmFind.edFind.Items, '');
        cfg.GetValue('/list_replace', fmFind.edRep.Items, '');
        if fmFind.IsNarrow then
          FormPosSetFromString(fmFind, cfg.GetValue('/pos/find', ''), false);
        fmFind.IsMultiLine:= cfg.GetValue('/finder/mline', false);
        fmFind.IsHiAll:= cfg.GetValue('/finder/hi', false);
        fmFind.chkRegexSubst.Checked:= cfg.GetValue('/finder/regex_subst', true);
      except
      end;
    finally
      cfg.Free
    end;
  end;

  fmFind.Localize;
end;

procedure TfmMain.DoDialogFind(AReplaceMode: boolean);
var
  Frame: TEditorFrame;
  Ed: TATSynEdit;
  StrSel, StrWord, StrSuggested: atString;
  bMultiLineSel: boolean;
begin
  Frame:= CurrentFrame;
  Ed:= Frame.Editor;

  StrWord:= Ed.TextCurrentWord;
  StrSuggested:= '';

  if Ed.IsSelColumn then
    StrSel:= ''
  else
    StrSel:= Ed.TextSelected;

  bMultiLineSel:= false;
  if Ed.Carets.Count=1 then
    bMultiLineSel:= Ed.Carets[0].IsMultilineSelection;

  //MsgLogDebug('find dlg: init');
  InitFormFind;

  with fmFind do
  begin
    if StrSel='' then
    begin
      fmFind.chkInSel.Checked:= false;
      FFinder.OptInSelection:= false;
    end;

    //change Find field only if options SuggestSel/SuggestWord on,
    //else dont touch Find field
    if UiOps.FindSuggestSel and (StrSel<>'') then
    begin
      if not FFinder.OptInSelection then
        StrSuggested:= StrSel;
    end
    else
    if UiOps.FindSuggestWord and (StrWord<>'') then
      StrSuggested:= StrWord;

    MsgLogDebug('find dlg: select all in input');
    edFind.DoCommand(cCommand_SelectAll, cInvokeAppInternal);
    edRep.DoCommand(cCommand_SelectAll, cInvokeAppInternal);

    chkCase.Checked:= FFinder.OptCase;
    chkWords.Checked:= FFinder.OptWords;
    chkRegex.Checked:= FFinder.OptRegex;
    chkConfirm.Checked:= FFinder.OptConfirmReplace;
    chkWrap.Checked:= FFinder.OptWrapped;
    bTokens.ItemIndex:= Ord(FFinder.OptTokens);

    if UiOps.FindSuggestInSelection and bMultiLineSel then
    begin
      chkInSel.Checked:= true;
      StrSuggested:= '';
    end
    else
      chkInSel.Checked:= FFinder.OptInSelection;

    UpdateInitialCaretPos;
    UpdateInputFind(StrSuggested);
    IsReplace:= AReplaceMode;

    //MsgLogDebug('find dlg: update state');
    UpdateState(true);
    //MsgLogDebug('find dlg: show');
    Show;
    //MsgLogDebug('find dlg: focus');
  end;

  UpdateFindDialogEnabled(Frame);
  if fmFind.Enabled then
    fmFind.UpdateFocus(true);
end;

procedure TfmMain.UpdateFindDialogEnabled(Frame: TEditorFrame);
var
  bEnabledPrev, bEnabledNow: boolean;
  bBinaryPrev, bBinaryNow: boolean;
begin
  if fmFind=nil then exit;

  bEnabledPrev:= fmFind.Enabled;
  bEnabledNow:= Frame.FrameKind in [efkEditor, efkBinaryViewer];

  bBinaryPrev:= fmFind.FForViewer;
  bBinaryNow:= Frame.FrameKind=efkBinaryViewer;

  fmFind.Enabled:= bEnabledNow;
  fmFind.FForViewer:= bBinaryNow;

  if (bEnabledPrev<>bEnabledNow) or
    (bBinaryPrev<>bBinaryNow) then
    fmFind.UpdateState(false);
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
    UpdateAppForSearch(false, false, true, true);
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
    DoDialogFind(false);
    Exit
  end;

  if Frame.FrameKind=efkBinaryViewer then
  begin
    FindDialogDone(Frame, afoFindFirst, false);
    exit;
  end;

  FFinder.Editor:= Frame.Editor;
  FFinder.OptBack:= false;
  FFinder.OptFromCaret:= false;

  ok:= FFinder.DoAction_FindOrReplace(false, false, bChanged, true);
  FinderUpdateEditor(false);
  FinderShowResult(ok, FFinder);
end;

procedure TfmMain.DoFindNext(ANext: boolean);
var
  Frame: TEditorFrame;
  Op: TAppFinderOperation;
  ok, bChanged: boolean;
begin
  Frame:= CurrentFrame;
  if Frame=nil then exit;
  InitFormFind;

  ////always update Finder string when "Find next" called. issue #2182
  if fmFind.edFind.Text<>'' then
    FFinder.StrFind:= fmFind.edFind.Text
  else
  if FFinder.StrFind='' then
  begin
    DoDialogFind(false);
    Exit
  end;

  if Frame.FrameKind=efkBinaryViewer then
  begin
    if ANext then
      Op:= afoFindNext
    else
      Op:= afoFindPrev;
    FindDialogDone(Frame, Op, false);
    exit;
  end;

  FFinder.Editor:= Frame.Editor;
  FFinder.OptFromCaret:= true;
  FFinder.OptBack:= not ANext;

  ok:= FFinder.DoAction_FindOrReplace(false, false, bChanged, true);
  FinderUpdateEditor(false);
  FinderShowResult(ok, FFinder);
  UpdateAppForSearch(false, false, true, true); //hide search progressbar

  //add string to history, issue #3326
  fmFind.edFind.DoAddLineToHistory(FFinder.StrFind, UiOps.MaxHistoryEdits);
end;

procedure TfmMain.DoFindMarkingInit(AMode: TATFindMarkingMode);
var
  Ed: TATSynEdit;
begin
  Ed:= FFinder.Editor;
  if Ed=nil then
    raise Exception.Create('DoFindMarkingInit: Finder.Editor=nil');

  FFindMarkingMode:= AMode;
  FFindMarkingCaret1st:= true;

  case AMode of
    markingSelections:
      begin
      end;
    markingMarkers:
      begin
        Ed.Markers.Clear;
      end;
    markingBookmarks:
      begin
        Ed.DoCommand(cmd_BookmarkClearAll, cInvokeAppInternal);
      end;
    else
      begin end;
  end;
end;

procedure TfmMain.DoFindMarkAll(AMode: TATFindMarkingMode);
var
  cnt: integer;
begin
  if Assigned(fmFind) {and (fmFind.edFind.Text<>'')} then
    FFinder.StrFind:= fmFind.edFind.Text;

  if FFinder.StrFind='' then
  begin
    FinderShowResult(false, FFinder);
    Exit
  end;

  FFinder.Editor:= CurrentEditor;
  FFinder.OptBack:= false;
  FFinder.OptFromCaret:= false;

  DoFindMarkingInit(AMode);
  cnt:= FFinder.DoAction_CountAll(true);
  DoFindMarkingInit(markingNone);

  FinderUpdateEditor(false);
  MsgStatus(
    Format(msgStatusFindCount, [Utf8Encode(FFinder.StrFind), cnt])+FinderOptionsToHint(FFinder),
    true);

  UpdateAppForSearch(false, false, true, true); //hide search progressbar
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

function TfmMain.FinderOptionsToHint(AFinder: TATEditorFinder): string;
const
  Sep=', ';
begin
  Result:= '';
  if AFinder.OptRegex then Result:= Result+msgFinderHintRegex+Sep;
  if AFinder.OptCase then Result:= Result+msgFinderHintCase+Sep;
  if AFinder.OptWords then Result:= Result+msgFinderHintWords+Sep;
  if AFinder.OptBack then Result:= Result+msgFinderHintBack+Sep;
  if AFinder.OptWrapped then Result:= Result+msgFinderHintWrapped+Sep;
  if AFinder.OptInSelection then Result:= Result+msgFinderHintInSel+Sep;
  if AFinder.OptFromCaret then Result:= Result+msgFinderHintFromCaret+Sep;
  if SEndsWith(Result, Sep) then
    SetLength(Result, Length(Result)-Length(Sep));
  Result:= ' ('+Result+')';
end;

procedure TfmMain.FinderGetHiAllIndexes(AFinder: TATEditorFinder; out AIndex, ACount: integer);
var
  Ed: TATSynEdit;
  Pnt: TPoint;
  ItemPtr: PATMarkerItem;
  NTagValue: Int64;
  iItem: integer;
begin
  AIndex:= -1;
  ACount:= 0;

  Ed:= AFinder.Editor;
  if Ed=nil then exit;

  NTagValue:= UiOps.FindHiAll_TagValue;

  Pnt:= AFinder.MatchEdPos;
  if Pnt.Y<0 then exit;

  for iItem:= 0 to Ed.Attribs.Count-1 do
  begin
    ItemPtr:= Ed.Attribs.ItemPtr(iItem);
    if ItemPtr^.Tag<>NTagValue then Continue;
    Inc(ACount);
    if AIndex<0 then
      if ItemPtr^.PosY=Pnt.Y then
        if ItemPtr^.PosX=Pnt.X then
          AIndex:= ACount-1;
  end;
end;

function TfmMain.FinderGetHiAllIndexesString(AFinder: TATEditorFinder): string;
var
  NCurrent, NTotal: integer;
begin
  Result:= '';
  FinderGetHiAllIndexes(AFinder, NCurrent, NTotal);
  if NCurrent>=0 then
    Result:= Format(' [%d/%d]', [NCurrent+1, NTotal])
end;

procedure TfmMain.FinderShowResult(ok: boolean; AFinder: TATEditorFinder);
begin
  if ok then
  begin
    MsgStatus(
      msgStatusFoundNextMatch+FinderGetHiAllIndexesString(AFinder)+FinderOptionsToHint(AFinder),
      true);

    AFinder.Editor.EndUpdate;
    Application.ProcessMessages;

    //not DoGotoPos, we may need to show marker, not caret
    AFinder.Editor.DoShowPos(
      AFinder.MatchEdPos,
      AFinder.IndentHorz,
      AFinder.IndentVert,
      true,
      true,
      true);
  end
  else
  if AFinder.IsRegexBad then
    MsgStatusErrorInRegex
  else
    MsgStatus(
      msgCannotFindMatch+FinderOptionsToHint(AFinder)+': '+Utf8Encode(AFinder.StrFind),
      true);
end;

procedure TfmMain.FinderShowResultSimple(ok: boolean; AFinder: TATEditorFinder);
begin
  if ok then
    MsgStatus(msgStatusFoundNextMatch{+FinderGetHiAllIndexesString}, true)
  else
    FinderShowResult(false, AFinder);
end;

procedure TfmMain.FinderShowMatchesCount(AMatchCount, ATime: integer);
var
  S: string;
begin
  if AMatchCount>0 then
    S:= Format(msgStatusFoundFragments, [AMatchCount])
  else
    S:= msgCannotFindMatch;
  S+= Format(' (%dms)', [ATime]);
  MsgStatus(S, true);
end;

procedure TfmMain.FinderOnConfirmReplace_API(Sender: TObject; APos1,
  APos2: TPoint; AForMany: boolean; var AConfirm, AContinue: boolean;
  var AReplacement: UnicodeString);
var
  ThisFinder: TATEditorFinder;
  Params: TAppVariantArray;
  Callback: string;
  EventRes: TAppPyEventResult;
  Ed: TATSynEdit;
begin
  AConfirm:= true;
  AContinue:= true;

  ThisFinder:= Sender as TATEditorFinder;
  Ed:= ThisFinder.Editor;
  Callback:= ThisFinder.CallbackString;
  if Callback='' then exit;

  Ed.DoCaretSingle(APos1.X, APos1.Y, APos2.X, APos2.Y);
  Ed.DoShowPos(APos1, ThisFinder.IndentHorz, ThisFinder.IndentVert, true, false, true);
  Ed.Update(true);

  SetLength(Params, 5);
  Params[0]:= AppVariant(APos1.X);
  Params[1]:= AppVariant(APos1.Y);
  Params[2]:= AppVariant(APos2.X);
  Params[3]:= AppVariant(APos2.Y);
  Params[4]:= AppVariant(AReplacement);

  EventRes:= DoPyCallbackFromAPI_2(Callback, Ed, Params);

  case EventRes.Val of
    evrInt:
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
    evrString:
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
    true{unfold}
    );
  Ed.Update(true);

  fmConfirmReplace:= TfmConfirmReplace.Create(Self);
  try
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
  finally
    FreeAndNil(fmConfirmReplace);
  end;

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

  {
  if Assigned(fmFind) then
    fmFind.UpdateCaption(msgStatusBadRegex+': '+FFinder.RegexErrorMsg);
    }
end;

procedure TfmMain.FinderOnGetToken(Sender: TObject; AX, AY: integer; out AKind: TATTokenKind);
begin
  AKind:= EditorGetTokenKind(Sender as TATSynEdit, AX, AY)
end;

procedure TfmMain.FinderUpdateEditor(AUpdateText: boolean; AUpdateStatusbar: boolean=true);
var
  Ed: TATSynEdit;
begin
  Ed:= FFinder.Editor;
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
    ufcsCaseIgnore:
      bCase:= false;
    ufcsCaseSens:
      bCase:= true;
    ufcsCaseFromDialog:
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
      fmFind.UpdateHiAll(true);
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

  //make command "find next" working with new text, issue #2803
  FFinder.StrFind:= Str;
  //and clear regex option (issue #2812)
  FFinder.OptRegex:= false;

  if Assigned(fmFind) then
    if bHiAll then
    begin
      fmFind.chkHiAll.Checked:= true;
      //fmFind.UpdateHiAll(true); //breaks issue #3572
    end;
end;


function TfmMain.DoFindOptions_GetDict: PPyObject;
var
  oFind, oRep,
  oFindHist, oRepHist,
  bCase, bWord, bRegex, bRegexSubst, bCfm,
  bInSel, bWrap, bMulLine, bHiAll, oTokens: PPyObject;
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
    oTokens:= ReturnNone;

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
      oTokens:= PyLong_FromLong(Ord(fmFind.bTokens.ItemIndex));
    end;

    Result:= Py_BuildValue('{sOsOsOsOsOsOsOsOsOsOsOsOsOsOsOsOsOsOsOsOsOsOsOsOsOsO}',
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
      'op_tokens', PyLong_FromLong(Ord(FFinder.OptTokens)),

      'op_case_d', bCase,
      'op_word_d', bWord,
      'op_regex_d', bRegex,
      'op_regex_subst_d', bRegexSubst,
      'op_cfm_d', bCfm,
      'op_insel_d', bInSel,
      'op_wrap_d', bWrap,
      'op_mulline_d', bMulLine,
      'op_tokens_d', oTokens,
      'op_hi_d', bHiAll
      );
  end;
end;


procedure TfmMain.DoFindOptions_ApplyDict(const AText: string);
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
      'op_tokens':
        FFinder.OptTokens:= TATFinderTokensAllowed(StrToIntDef(SValue, 0));

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
    end;
  end;
end;


procedure TfmMain.DoFindActionFromString(const AStr: string);
var
  Sep: TATStringSeparator;
  SAction, SFind, SRep, SOpt: string;
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
  FinderOptionsFromString(FFinder, SOpt);
  FindDialogDone(
    nil, //skip dialog
    AppFinderOperationFromString(SAction),
    false //'false' to solve issue #3303
    );
end;

procedure TfmMain.DoFindOptions_OnChange(Sender: TObject);
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
  end;
end;

procedure TfmMain.FindDialogFocusEditor(Sender: TObject);
begin
  DoFocusEditor(CurrentEditor);
end;

function TfmMain.FinderHandleKeyDown(AKey: word; AShiftState: TShiftState): boolean;
var
  Ed: TATSynEdit;
  NCmd: integer;
  KeyHistory: TATKeyArray;
begin
  Result:= false;
  Ed:= CurrentEditor;
  if Ed=nil then exit;

  FillChar(KeyHistory{%H-}, SizeOf(KeyHistory), 0);
  NCmd:= Ed.Keymap.GetCommandFromShortcut(ShortCut(AKey, AShiftState), KeyHistory);
  case NCmd of
    cmd_SwitchTab_HotkeyNext,
    cmd_SwitchTab_HotkeyPrev,
    cmd_SwitchTab_SimpleNext,
    cmd_SwitchTab_SimplePrev:
      begin
        Ed.DoCommand(NCmd, cInvokeHotkey);
        Result:= true;
      end;
  end;
end;

