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
          true
          );
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
        //marker selection is single line
        if APos2.Y=APos1.Y then
          NLen:= Abs(APos2.X-APos1.X)
        else
          NLen:= 0;
        Ed.Markers.Add(APos1.X, APos1.Y, 0, NLen, 0);
      end;

    markingBookmarks:
      begin
        ed.BookmarkSetForLine(APos1.Y, 1, '', false, true, 0);
      end;
  end;
end;


procedure TfmMain.DoFindOptions_ResetInSelection;
begin
  //2020.10: don't reset sel, "find next" supports "in selection"
  {
  FFinder.OptInSelection:= false;
  if Assigned(fmFind) then
    fmFind.chkInSel.Checked:= false;
    }
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
    Format(' (%d s)', [ATime]) );
end;


procedure TfmMain.FindDialogDone(Sender: TObject; Res: TAppFinderOperation);
var
  Category: TAppFinderOperationCategory;
  ListFound: TStringList;
  Frame, FrameNew, FramePrev: TEditorFrame;
  Ed: TATSynEdit;
  ok, bChanged, bDoLock, bFindMode, bChangeFrame: boolean;
  NCounter, IndexFrame: integer;
  NTime: integer;
  NTick: QWord;
begin
  InitFormFind;
  Frame:= CurrentFrame;
  Category:= cAppFinderOperationCategory[Res];
  if Category=afcNone then exit;

  if Category in [afcFind, afcReplaceOne] then
  begin
    if Frame.IsPicture then exit;
    if Frame.Editor.Carets.Count=0 then
    begin
      MsgStatus(msgCannotFindWithoutCaret);
      exit;
    end;
  end;

  if Category=afcReplaceOne then
  begin
    if not Frame.IsText then exit;
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
    UpdateAppForSearch(false, false, bFindMode);
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
    FFinder.OptWrapped:= chkWrap.Checked;
    FFinder.OptInSelection:= chkInSel.Checked;
    FFinder.OptFromCaret:= (Res=afoFindNext) or (Res=afoFindPrev) or (Res=afoReplace);
    FFinder.OptConfirmReplace:= chkConfirm.Checked;
    FFinder.OptTokens:= TATFinderTokensAllowed(bTokens.ItemIndex);
  end;

  if Frame.IsBinary then
  begin
    UpdateAppForSearch(true, false, true);
    Application.ProcessMessages;

    case Res of
      afoFindFirst:
        DoFindResultSimple(Frame.BinaryFindFirst(FFinder, false));

      afoFindNext,
      afoFindPrev:
        DoFindResultSimple(Frame.BinaryFindNext(Res=afoFindPrev));
    end;

    UpdateAppForSearch(false, false, true);
    exit;
  end;

  //if FFinder.OptInSelection then
  //  if CurrentEditor.Carets.Count>1 then
  //    MsgBox(msgCannotFindInMultiSel, MB_OK+MB_ICONWARNING);

  //format must be this:
  // https://wiki.freepascal.org/CudaText_API#Format_of_text_for_cmd_FinderAction
  if Frame.MacroRecord then
    Frame.MacroString:= Frame.MacroString+
       (IntToStr(cmd_FinderAction)+','+
        cAppFinderOperationString[Res]+#1+
        FFinder.StrFind+#1+
        FFinder.StrReplace+#1+
        FinderOptionsToString(FFinder)+
        #10);

  bDoLock:=
    //(Res=afoReplace) or //don't lock for single replace, issue #1216
    //(Res=afoReplaceStop) or
    (Res=afoReplaceAll) or
    (Res=afoReplaceGlobal);
  UpdateAppForSearch(true, bDoLock, bFindMode);

  if bDoLock then
  begin
    //add undo group, so Undo won't take too much actions
    FFinder.Editor.Strings.BeginUndoGroup;
    FFinder.Editor.Strings.EndUndoGroup;
  end;

  if (Res=afoFindFirst) or (Res=afoFindNext) or (Res=afoFindPrev) then
  begin
    FFinder.OptBack:= (Res=afoFindPrev);
    ok:= FFinder.DoAction_FindOrReplace(
      false, false, bChanged, true);
    FinderUpdateEditor(false);
    DoFindResult(ok);
    if ok then
      DoFindOptions_ResetInSelection;
  end;

  if (Res=afoReplace) or (Res=afoReplaceStop) then
  begin
    //replace match
    ok:= FFinder.DoAction_ReplaceSelected(true);
    //after match is replaced, do find-next
    if Res=afoReplace then
      if ok then
        ok:= FFinder.DoAction_FindOrReplace(false, false, bChanged, true);
    FinderUpdateEditor(true);
    DoFindResult(ok);
    if ok then
      DoFindOptions_ResetInSelection;
  end;

  if Res=afoReplaceAll then
  begin
    NTick:= GetTickCount64;
    NCounter:= FinderReplaceAll(FFinder.Editor, false);
    NTime:= (GetTickCount64-NTick) div 1000;
    UpdateStatus;
    FinderShowReplaceReport(NCounter, NTime);
  end;

  if Res=afoReplaceGlobal then
  begin
    NCounter:= 0;
    NTick:= GetTickCount64;
    FramePrev:= Frame;
    bChangeFrame:= FFinder.OptConfirmReplace;

    for IndexFrame:= 0 to FrameCount-1 do
    begin
      FrameNew:= Frames[IndexFrame];
      if not FrameNew.IsText then Continue;

      if bChangeFrame then
        SetFrame(FrameNew);

      NCounter+= FinderReplaceAll(FrameNew.Ed1, true);
      if not FrameNew.EditorsLinked then
        NCounter+= FinderReplaceAll(FrameNew.Ed2, true);
    end;

    NTime:= (GetTickCount64-NTick) div 1000;
    if bChangeFrame then
      SetFrame(FramePrev);
    UpdateStatus;
    FinderShowReplaceReport(NCounter, NTime);
  end;

  if Res=afoCountAll then
  begin
    DoFindMarkingInit(markingNone);
    NCounter:= FFinder.DoAction_CountAll(false);
    if (NCounter=0) and FFinder.IsRegexBad then
      MsgStatusErrorInRegex
    else
      MsgStatus(Format(msgStatusFindCount, [Utf8Encode(FFinder.StrFind), NCounter]));
  end;

  if Res=afoExtractAll then
  begin
    ListFound:= TStringList.Create;
    try
      FFinder.DoAction_ExtractAll(false, ListFound, true, dupIgnore);
      if ListFound.Count>0 then
      begin
        FrameNew:= DoFileOpen('', '');
        FrameNew.TabCaption:= fmFind.bExtract.Caption;
        Ed:= FrameNew.Editor;
        try
          Ed.Strings.LoadFromStrings(ListFound, cEndUnix);
          MsgStatus(Format(msgStatusFoundFragments, [ListFound.Count]));
        finally
          Ed.DoEventChange;
        end;
      end
      else
      if FFinder.IsRegexBad then
        MsgStatusErrorInRegex
      else
        MsgStatus(msgCannotFindMatch);
    finally
      FreeAndNil(ListFound);
    end;
  end;

  if Res=afoFindSelectAll then
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
        ' ('+FinderOptionsToHint+')');
    DoFindOptions_ResetInSelection;
  end;

  if Res=afoFindMarkAll then
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
        ' ('+FinderOptionsToHint+')');
  end;

  UpdateAppForSearch(false, false, bFindMode);

  {$ifdef LCLgtk2}
  //Linux gtk2: UpdateAppForSearch don't put focus anywhere (Laz bug?) after "confirmation" form
  //make workaround
  if (Res=afoReplaceAll) or
     (Res=afoReplaceGlobal) then
    CurrentFrame.SetFocus;
  {$endif}
end;

procedure TfmMain.InitFormFind;
var
  cfg: TJsonConfig;
begin
  if not Assigned(fmFind) then
  begin
    fmFind:= TfmFind.Create(Self);
    fmFind.OnResult:= @FindDialogDone;
    fmFind.OnChangeOptions:= @DoFindOptions_OnChange;
    fmFind.Color:= GetAppColor(apclTabBg);

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
  Ed: TATSynEdit;
  StrSel, StrWord: atString;
  bMultiLineSel: boolean;
  Caret: TATCaretItem;
begin
  Ed:= CurrentEditor;

  StrWord:= Ed.TextCurrentWord;
  StrSel:= Ed.TextSelected;

  //ignore multi-line sel
  //if Pos(#10, StrSel)>0 then StrSel:= '';

  if not Ed.IsSelRectEmpty then StrSel:= '';

  bMultiLineSel:= false;
  if Ed.Carets.Count=1 then
  begin
    Caret:= Ed.Carets[0];
    bMultiLineSel:= (Caret.EndY>=0) and (Caret.EndY<>Caret.PosY);
  end;

  MsgLogDebug('find dlg: init');
  InitFormFind;
  with fmFind do
  begin
    //change Find field only if options SuggestSel/SuggestWord on,
    //else dont touch Find field
    if UiOps.FindSuggestSel and (StrSel<>'') then
      UpdateInputFind(StrSel)
    else
    if UiOps.FindSuggestWord and (StrWord<>'') then
      UpdateInputFind(StrWord);

    MsgLogDebug('find dlg: select all in input');
    edFind.DoCommand(cCommand_SelectAll);
    edRep.DoCommand(cCommand_SelectAll);

    chkCase.Checked:= FFinder.OptCase;
    chkWords.Checked:= FFinder.OptWords;
    chkRegex.Checked:= FFinder.OptRegex;
    chkConfirm.Checked:= FFinder.OptConfirmReplace;
    chkWrap.Checked:= FFinder.OptWrapped;
    bTokens.ItemIndex:= Ord(FFinder.OptTokens);

    if UiOps.FindSuggestInSelection and bMultiLineSel then
      chkInSel.Checked:= true
    else
      chkInSel.Checked:= FFinder.OptInSelection;

    IsReplace:= AReplaceMode;

    MsgLogDebug('find dlg: update state');
    UpdateState;
    MsgLogDebug('find dlg: show');
    Show;
    MsgLogDebug('find dlg: focus');
    UpdateFocus(true);
  end;
end;

procedure TfmMain.DoDialogFind_Hide;
begin
  if Assigned(fmFind) and fmFind.Visible then
  begin
    //handy to reset in-sel, on closing dialog
    FFinder.OptInSelection:= false;

    CurrentFrame.SetFocus;
    fmFind.Hide;
    UpdateAppForSearch(false, false, true);
  end;
end;


procedure TfmMain.DoFindFirst;
var
  ok, bChanged: boolean;
begin
  //if Assigned(fmFind) then
  //  FFinder.StrFind:= fmFind.edFind.Text;

  if FFinder.StrFind='' then
  begin
    DoDialogFind(false);
    Exit
  end;

  FFinder.Editor:= CurrentEditor;
  FFinder.OptBack:= false;
  FFinder.OptFromCaret:= false;

  ok:= FFinder.DoAction_FindOrReplace(false, false, bChanged, true);
  FinderUpdateEditor(false);
  DoFindResult(ok);
end;

procedure TfmMain.DoFindNext(ANext: boolean);
var
  ok, bChanged: boolean;
begin
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

  FFinder.Editor:= CurrentEditor;
  FFinder.OptFromCaret:= true;
  FFinder.OptBack:= not ANext;

  ok:= FFinder.DoAction_FindOrReplace(false, false, bChanged, true);
  FinderUpdateEditor(false);
  DoFindResult(ok);
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
        Ed.DoCommand(cmd_BookmarkClearAll);
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
    MsgStatus(msgCannotFindMatch);
    Exit
  end;

  FFinder.Editor:= CurrentEditor;
  FFinder.OptBack:= false;
  FFinder.OptFromCaret:= false;

  DoFindMarkingInit(AMode);
  cnt:= FFinder.DoAction_CountAll(true);
  DoFindMarkingInit(markingNone);

  FinderUpdateEditor(false);
  MsgStatus(Format(msgStatusFindCount, [Utf8Encode(FFinder.StrFind), cnt]));
end;


procedure TfmMain.FinderOnProgress(Sender: TObject; const ACurPos, AMaxPos: Int64;
  var AContinue: boolean);
var
  NValue: Int64;
begin
  if AMaxPos<=0 then exit;
  NValue:= ACurPos * 100 div AMaxPos;
  StatusProgress.Progress:= NValue;
  Application.ProcessMessages;
  if FFindStop or Application.Terminated then
    AContinue:= false;
end;

function TfmMain.FinderOptionsToHint: string;
const
  Sep=', ';
begin
  Result:= '';
  if FFinder.OptRegex then Result:= Result+msgFinderHintRegex+Sep;
  if FFinder.OptCase then Result:= Result+msgFinderHintCase+Sep;
  if FFinder.OptWords then Result:= Result+msgFinderHintWords+Sep;
  if FFinder.OptBack then Result:= Result+msgFinderHintBack+Sep;
  if FFinder.OptWrapped then Result:= Result+msgFinderHintWrapped+Sep;
  if FFinder.OptInSelection then Result:= Result+msgFinderHintInSel+Sep;
  if FFinder.OptFromCaret then Result:= Result+msgFinderHintFromCaret+Sep;
  if SEndsWith(Result, Sep) then
    SetLength(Result, Length(Result)-Length(Sep));
end;

procedure TfmMain.DoFindResult(ok: boolean);
var
  SOptions: string;
begin
  SOptions:= FinderOptionsToHint;

  if ok then
  begin
    MsgStatus(msgStatusFoundNextMatch+' ('+SOptions+')');

    //these 3 lines- to scroll to found pos
    //(was bad, if found pos in folded blk)
    FFinder.Editor.EndUpdate;
    Application.ProcessMessages;
    FFinder.Editor.DoShowPos( //not DoGotoPos, we may need to show marker, not caret
      FFinder.MatchEdPos,
      FFinder.IndentHorz,
      FFinder.IndentVert,
      true);
  end
  else
  if FFinder.IsRegexBad then
    MsgStatusErrorInRegex
  else
    MsgStatus(msgCannotFindMatch+' ('+SOptions+'): '+Utf8Encode(FFinder.StrFind));
end;

procedure TfmMain.DoFindResultSimple(ok: boolean);
begin
  if ok then
    MsgStatus(msgStatusFoundNextMatch)
  else
  if FFinder.IsRegexBad then
    MsgStatusErrorInRegex
  else
    MsgStatus(msgCannotFindMatch);
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
  Ed.DoShowPos(APos1, ThisFinder.IndentHorz, ThisFinder.IndentVert, false);
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
    Ed.DoEventChange;

  Ed.Update(AUpdateText);

  //not DoGotoPos, we may need to show marker, not caret
  Ed.DoShowPos(
    FFinder.MatchEdPos,
    FFinder.IndentHorz,
    FFinder.IndentVert,
    true
    );

  if AUpdateStatusbar then
    UpdateStatus;
end;

procedure TfmMain.DoFindCurrentWordOrSel(Ed: TATSynEdit; ANext, AWordOrSel: boolean);
var
  Str: UnicodeString;
  ok, bCase: boolean;
begin
  bCase:= true;
  case UiOps.FindSelCase of
    0: bCase:= false;
    1: bCase:= true;
    2:
      begin
        if Assigned(fmFind) then
          bCase:= fmFind.chkCase.Checked;
      end;
  end;

  ok:= EditorFindCurrentWordOrSel(Ed, ANext, AWordOrSel, bCase, FFinder.OptWrapped, Str);
  DoFindResultSimple(ok);

  //sync Find dialog with text
  if Assigned(fmFind) then
    fmFind.UpdateInputFind(Str);

  //make command "find next" working with new text (issue #2803)
  FFinder.StrFind:= Str;
  //and clear regex option (issue #2812)
  FFinder.OptRegex:= false;
end;


function TfmMain.DoFindOptions_GetDict: PPyObject;
var
  oFind, oRep,
  oFindHist, oRepHist,
  bCase, bWord, bRegex, bCfm,
  bInSel, bWrap, bMulLine, oTokens: PPyObject;
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
    bCfm:= ReturnNone;
    bInSel:= ReturnNone;
    bWrap:= ReturnNone;
    bMulLine:= ReturnNone;
    oTokens:= ReturnNone;

    if Assigned(fmFind) then
    begin
      oFind:= PyUnicode_FromWideString(fmFind.edFind.Text);
      oRep:= PyUnicode_FromWideString(fmFind.edRep.Text);
      oFindHist:= StringsToPyList(fmFind.edFind.Items);
      oRepHist:= StringsToPyList(fmFind.edRep.Items);
      bCase:= PyBool_FromLong(Ord(fmFind.chkCase.Checked));
      bWord:= PyBool_FromLong(Ord(fmFind.chkWords.Checked));
      bRegex:= PyBool_FromLong(Ord(fmFind.chkRegex.Checked));
      bCfm:= PyBool_FromLong(Ord(fmFind.chkConfirm.Checked));
      bInSel:= PyBool_FromLong(Ord(fmFind.chkInSel.Checked));
      bWrap:= PyBool_FromLong(Ord(fmFind.chkWrap.Checked));
      bMulLine:= PyBool_FromLong(Ord(fmFind.chkMulLine.Checked));
      oTokens:= PyInt_FromLong(Ord(fmFind.bTokens.ItemIndex));
    end;

    Result:= Py_BuildValue('{sOsOsOsOsOsOsOsOsOsOsOsOsOsOsOsOsOsOsOsOsOsOsO}',
      'find', PyUnicode_FromWideString(FFinder.StrFind),
      'rep', PyUnicode_FromWideString(FFinder.StrReplace),

      'find_d', oFind,
      'rep_d', oRep,
      'find_h', oFindHist,
      'rep_h', oRepHist,

      'op_case', PyBool_FromLong(Ord(FFinder.OptCase)),
      'op_back', PyBool_FromLong(Ord(FFinder.OptBack)),
      'op_word', PyBool_FromLong(Ord(FFinder.OptWords)),
      'op_regex', PyBool_FromLong(Ord(FFinder.OptRegex)),
      'op_cfm', PyBool_FromLong(Ord(FFinder.OptConfirmReplace)),
      'op_fromcaret', PyBool_FromLong(Ord(FFinder.OptFromCaret)),
      'op_insel', PyBool_FromLong(Ord(FFinder.OptInSelection)),
      'op_wrap', PyBool_FromLong(Ord(FFinder.OptWrapped)),
      'op_tokens', PyInt_FromLong(Ord(FFinder.OptTokens)),

      'op_case_d', bCase,
      'op_word_d', bWord,
      'op_regex_d', bRegex,
      'op_cfm_d', bCfm,
      'op_insel_d', bInSel,
      'op_wrap_d', bWrap,
      'op_mulline_d', bMulLine,
      'op_tokens_d', oTokens
      );
  end;
end;


procedure TfmMain.DoFindOptions_ApplyDict(const AText: string);
var
  Sep: TATStringSeparator;
  SItem, SKey, SValue: string;
  bListValue: boolean;
begin
  //text is '{key1:value1;key2:value2}' from to_str()
  Sep.Init(SDeleteCurlyBrackets(AText), #1);
  repeat
    if not Sep.GetItemStr(SItem) then Break;
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
            fmFind.UpdateState;
            fmFind.DoOnChange;
          end;
        end;

      'rep_d':
        begin
          if Assigned(fmFind) then
          begin
            fmFind.UpdateInputReplace(UTF8Decode(SValue));
            fmFind.UpdateState;
            fmFind.DoOnChange;
          end;
        end;

      'find_h':
        begin
          if Assigned(fmFind) then
          begin
            fmFind.edFind.Items.Clear;
            Sep.Init(SValue);
            while Sep.GetItemStr(SItem) do
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
            Sep.Init(SValue);
            while Sep.GetItemStr(SItem) do
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
            fmFind.UpdateState;
            fmFind.DoOnChange;
          end;
        end;
      'op_word_d':
        begin
          if Assigned(fmFind) then
          begin
            fmFind.chkWords.Checked:= AppStrToBool(SValue);
            fmFind.UpdateState;
            fmFind.DoOnChange;
          end;
        end;
      'op_regex_d':
        begin
          if Assigned(fmFind) then
          begin
            fmFind.chkRegex.Checked:= AppStrToBool(SValue);
            fmFind.UpdateState;
            fmFind.DoOnChange;
          end;
        end;
      'op_cfm_d':
        begin
          if Assigned(fmFind) then
          begin
            fmFind.chkConfirm.Checked:= AppStrToBool(SValue);
            fmFind.UpdateState;
            fmFind.DoOnChange;
          end;
        end;
      'op_insel_d':
        begin
          if Assigned(fmFind) then
          begin
            fmFind.chkInSel.Checked:= AppStrToBool(SValue);
            fmFind.UpdateState;
            fmFind.DoOnChange;
          end;
        end;
      'op_wrap_d':
        begin
          if Assigned(fmFind) then
          begin
            fmFind.chkWrap.Checked:= AppStrToBool(SValue);
            fmFind.UpdateState;
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
    end;
  until false;
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
  FindDialogDone(nil{skip dialog}, AppFinderOperationFromString(SAction));
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
    FFinder.OptWrapped:= chkWrap.Checked;
    FFinder.OptInSelection:= chkInSel.Checked;
    FFinder.OptConfirmReplace:= chkConfirm.Checked;
    FFinder.OptTokens:= TATFinderTokensAllowed(bTokens.ItemIndex);
  end;
end;


