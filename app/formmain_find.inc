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
        Ed.DoGotoPos(
          APos1,
          APos2,
          UiOps.FindIndentHorz,
          UiOps.FindIndentVert,
          true,
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
        //marker sel is only 1 line
        if APos2.Y=APos1.Y then
          NLen:= Abs(APos2.X-APos1.X)
        else
          NLen:= 0;

        //ShowMessage(Format('mark at %d:%d', [APos1.Y, APos1.X]));//debug
        Ed.Markers.Add(APos1.X, APos1.Y, 0, NLen, 0, nil, true{InsertToBegin});
      end;

    markingBookmarks:
      begin
        EditorBookmarkSet(Ed, APos1.Y, 1, bmOpSet, '');
      end;
  end;
end;


procedure TfmMain.DoFindOptions_ResetInSelection;
begin
  //todo: maybe rework and dont reset opt
  FFinder.OptInSelection:= false;
  if Assigned(fmFind) then
    fmFind.chkInSel.Checked:= false;
end;

procedure TfmMain.FindDialogDone(Sender: TObject; const Res: string);
var
  ok, bChanged, bDoLock, bFindMode: boolean;
  cnt, NTop, NLeft,
  NCaretX, NCaretY: integer;
  NTick, NTime: QWord;
  Frame: TEditorFrame;
begin
  Frame:= CurrentFrame;

  //keep focus in Find field, else focus find or rep field
  if fmFind.edFind.Focused then
    bFindMode:= true
  else
  if fmFind.edRep.Focused then
    bFindMode:= false
  else
    bFindMode:=
      (Res<>cOpFindRep) and
      (Res<>cOpFindRepAll) and
      (Res<>cOpFindRepAndStop);

  if Res=cOpFindClose then
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
    FFinder.OptFromCaret:= (Res=cOpFindNext) or (Res=cOpFindPrev) or (Res=cOpFindRep);
    FFinder.OptConfirmReplace:= chkConfirm.Checked;
  end;

  if Frame.IsBinary then
  begin
    UpdateAppForSearch(true, false, true);
    Application.ProcessMessages;

    case Res of
      cOpFindFirst:
        begin
          if Frame.BinaryFindFirst(FFinder, false) then
            MsgStatus(msgStatusFoundNextMatch)
          else
            MsgStatus(msgCannotFindMatch);
        end;

      cOpFindNext,
      cOpFindPrev:
        begin
          if Frame.BinaryFindNext(Res=cOpFindPrev) then
            MsgStatus(msgStatusFoundNextMatch)
          else
            MsgStatus(msgCannotFindMatch);
        end;
    end;

    UpdateAppForSearch(false, false, true);
    exit;
  end;

  //if FFinder.OptInSelection then
  //  if CurrentEditor.Carets.Count>1 then
  //    MsgBox(msgCannotFindInMultiSel, MB_OK+MB_ICONWARNING);
  if Frame.MacroRecord then
    Frame.MacroString:= Frame.MacroString+(Format('%d,%s'#1'%s'#1'%s'#1'%s',
      [cmd_FinderAction, Res, FFinder.StrFind, FFinder.StrReplace, DoFindOptionsToString])+#10);

  bDoLock:=
    (Res=cOpFindRep) or
    (Res=cOpFindRepAndStop) or
    (Res=cOpFindRepAll);
  UpdateAppForSearch(true, bDoLock, bFindMode);

  if (Res=cOpFindFirst) or (Res=cOpFindNext) or (Res=cOpFindPrev) then
  begin
    FFinder.OptBack:= (Res=cOpFindPrev);
    ok:= FFinder.DoAction_FindOrReplace(false, false, false, bChanged);
    FinderUpdateEditor(false);
    DoFindResult(ok);
    if ok then
      DoFindOptions_ResetInSelection;
  end;

  if (Res=cOpFindRep) or (Res=cOpFindRepAndStop) then
  begin
    //replace match
    ok:= FFinder.DoAction_ReplaceSelected;
    //after match is replaced, do find-next
    if Res=cOpFindRep then
      if ok then
        ok:= FFinder.DoAction_FindOrReplace(false, false, false, bChanged);
    FinderUpdateEditor(true);
    DoFindResult(ok);
    if ok then
      DoFindOptions_ResetInSelection;
  end;

  if Res=cOpFindRepAll then
  begin
    NTop:= FFinder.Editor.LineTop;
    NLeft:= FFinder.Editor.ColumnLeft;
    with FFinder.Editor do
      if Carets.Count>0 then
      begin
        NCaretX:= Carets[0].PosX;
        NCaretY:= Carets[0].PosY;
      end;

    NTick:= GetTickCount64;
    FFinder.Editor.Strings.BeginUndoGroup;
    cnt:= FFinder.DoAction_ReplaceAll;
    FFinder.Editor.Strings.EndUndoGroup;
    FinderUpdateEditor(true);
    NTime:= (GetTickCount64-NTick) div 1000;

    FFinder.Editor.DoCaretSingle(NCaretX, NCaretY);
    FFinder.Editor.LineTop:= NTop;
    FFinder.Editor.ColumnLeft:= NLeft;
    MsgStatus(
      Format(msgStatusReplaceCount, [cnt])+
      Format(' (%d s)', [NTime]) );
  end;

  if Res=cOpFindCount then
  begin
    DoFindMarkingInit(markingNone);
    cnt:= FFinder.DoAction_CountAll(false);
    MsgStatus(Format(msgStatusFindCount, [Utf8Encode(FFinder.StrFind), cnt]));
  end;

  if Res=cOpFindSelectAll then
  begin
    DoFindMarkingInit(markingSelections);
    cnt:= FFinder.DoAction_CountAll(true);
    DoFindMarkingInit(markingNone);
    FinderUpdateEditor(false);
    MsgStatus(
      Format(msgStatusFindCount, [Utf8Encode(FFinder.StrFind), cnt]) +
      ' ('+SFindOptionsToTextHint+')');
    DoFindOptions_ResetInSelection;
  end;

  if Res=cOpFindMarkAll then
  begin
    DoFindMarkingInit(markingMarkers);
    cnt:= FFinder.DoAction_CountAll(true);
    DoFindMarkingInit(markingNone);
    FinderUpdateEditor(false);
    MsgStatus(
      Format(msgStatusFindCount, [Utf8Encode(FFinder.StrFind), cnt]) +
      ' ('+SFindOptionsToTextHint+')');
  end;

  UpdateAppForSearch(false, false, bFindMode);
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
    fmFind.Color:= GetAppColor('TabBg');

    UiOps.HotkeyFindDialog:= ShortcutToText(AppKeymap.GetShortcutFromCommand(cmd_DialogFind));
    UiOps.HotkeyReplaceDialog:= ShortcutToText(AppKeymap.GetShortcutFromCommand(cmd_DialogReplace));

    if not UiOps.FindSeparateForm then
    begin
      fmFind.Parent:= PanelMain;
      fmFind.Align:= alBottom;
      fmFind.BorderStyle:= bsNone;
    end
    else
    begin
      fmFind.IsNarrow:= true;
      UpdateFormOnTop(fmFind);
    end;

    cfg:= TJSONConfig.Create(nil);
    try
      cfg.Filename:= GetAppPath(cFileOptionsHistory);
      SLoadStringsFromFile(cfg, '/list_find', fmFind.edFind.Items, UiOps.MaxHistoryEdits);
      SLoadStringsFromFile(cfg, '/list_replace', fmFind.edRep.Items, UiOps.MaxHistoryEdits);
      if fmFind.IsNarrow then
      begin
        fmFind.Left:= cfg.GetValue('/find/pos_x', 200);
        fmFind.Top:= cfg.GetValue('/find/pos_y', 400);
      end;
      fmFind.IsMultiLine:= cfg.GetValue('/find/multi_line', false);
    finally
      cfg.Free
    end;
  end;

  DoLocalize_FormFind;
end;

procedure TfmMain.DoDialogFind(AReplaceMode: boolean);
var
  Ed: TATSynEdit;
  StrSel, StrWord: atString;
begin
  Ed:= CurrentEditor;

  StrWord:= Ed.TextCurrentWord;
  StrSel:= Ed.TextSelected;

  //ignore multi-line sel
  if Pos(#10, StrSel)>0 then StrSel:= '';
  if Pos(#13, StrSel)>0 then StrSel:= '';
  if not Ed.IsSelRectEmpty then StrSel:= '';

  InitFormFind;
  with fmFind do
  begin
    bFindFirst.Visible:= UiOps.FindShowFindfirst;

    //change Find field only if options SuggestSel/SuggestWord on,
    //else dont touch Find field
    if UiOps.FindSuggestSel and (StrSel<>'') then
      edFind.Text:= StrSel
    else
    if UiOps.FindSuggestWord and (StrWord<>'') then
      edFind.Text:= StrWord;

    edFind.DoCommand(cCommand_SelectAll);
    edRep.DoCommand(cCommand_SelectAll);

    chkCase.Checked:= FFinder.OptCase;
    chkWords.Checked:= FFinder.OptWords;
    chkRegex.Checked:= FFinder.OptRegex;
    chkConfirm.Checked:= FFinder.OptConfirmReplace;
    chkWrap.Checked:= FFinder.OptWrapped;
    chkInSel.Checked:= FFinder.OptInSelection;
    IsReplace:= AReplaceMode;

    UpdateState;
    Show;
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

  ok:= FFinder.DoAction_FindOrReplace(false, false, false, bChanged);
  FinderUpdateEditor(false);
  DoFindResult(ok);
end;

procedure TfmMain.DoFindNext(ANext: boolean);
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
  FFinder.OptFromCaret:= true;
  if not ANext then
    FFinder.OptRegex:= false;
  FFinder.OptBack:= not ANext;

  ok:= FFinder.DoAction_FindOrReplace(false, false, false, bChanged);
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
  end;
end;

procedure TfmMain.DoFindMarkAll(AMode: TATFindMarkingMode);
var
  cnt: integer;
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
  if FFindStop then AContinue:= false;
end;

function TfmMain.SFindOptionsToTextHint: string;
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
begin
  if ok then
  begin
    MsgStatus(msgStatusFoundNextMatch);

    //these 3 lines- to scroll to found pos
    //(was bad, if found pos in folded blk)
    FFinder.Editor.EndUpdate;
    Application.ProcessMessages;
    FFinder.Editor.DoGotoCaret(cEdgeTop);
  end
  else
    MsgStatus(msgCannotFindMatch+' ('+SFindOptionsToTextHint+'): '+Utf8Encode(FFinder.StrFind));
end;

procedure TfmMain.FinderOnConfirmReplace(Sender: TObject; APos1, APos2: TPoint;
  AForMany: boolean; var AConfirm, AContinue: boolean);
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
  Ed.DoGotoCaret(cEdgeTop);
  Ed.Update(true);

  fmConfirmReplace:= TfmConfirmReplace.Create(Self);
  try
    DoLocalize_FormConfirmReplace(fmConfirmReplace);
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

procedure TfmMain.FinderOnBadRegex(Sender: TObject);
begin
  MsgBox(
    msgStatusBadRegex+#10+Utf8Encode(FFinder.StrFind),
    MB_OK or MB_ICONERROR);
end;

procedure TfmMain.FinderUpdateEditor(AUpdateText: boolean);
var
  Pnt: TPoint;
begin
  if AUpdateText then
    FFinder.Editor.DoEventChange;

  with FFinder.Editor.Carets[0] do
  begin
    Pnt.X:= PosX;
    Pnt.Y:= PosY;
  end;

  FFinder.Editor.Update(AUpdateText);
  FFinder.Editor.DoGotoPos(
    Pnt,
    Point(-1, -1),
    UiOps.FindIndentHorz,
    UiOps.FindIndentVert,
    false,
    true
    );
  UpdateStatus;
end;

procedure TfmMain.DoFindCurrentWordOrSel(ANext: boolean; AWordOrSel: boolean);
var
  Str: atString;
  Ed: TATSynEdit;
  ok, bFlag: boolean;
  bSavedWords, bSavedRegex, bSavedCase: boolean;
begin
  Ed:= CurrentEditor;
  if Ed.Carets.Count>1 then exit;

  if AWordOrSel then
  begin
    Str:= Ed.TextCurrentWord;
    Ed.DoCommand(cCommand_SelectWords);
  end
  else
  begin
    Str:= Ed.TextSelected;
    if Str='' then
    begin
      Str:= Ed.TextCurrentWord;
      Ed.DoCommand(cCommand_SelectWords);
    end;
  end;
  if Str='' then exit;

  //SavedFind:= FFinder.StrFind; //don't save/restore
  bSavedWords:= FFinder.OptWords;
  bSavedRegex:= FFinder.OptRegex;
  bSavedCase:= FFinder.OptCase;

  FFinder.StrFind:= Str;
  FFinder.StrReplace:= '';
  FFinder.Editor:= Ed;

  FFinder.OptRegex:= false;
  FFinder.OptWords:= AWordOrSel;
  FFinder.OptFromCaret:= true;
  FFinder.OptBack:= not ANext;
  FFinder.OptInSelection:= false;

  case UiOps.FindSelCase of
    0: FFinder.OptCase:= false;
    1: FFinder.OptCase:= true;
    2:
      begin
        if Assigned(fmFind) then
          FFinder.OptCase:= fmFind.chkCase.Checked;
      end;
  end;

  ok:= FFinder.DoAction_FindOrReplace(false, false, false, bFlag);
  FinderUpdateEditor(false);
  DoFindResult(ok);

  //FFinder.StrFind:= SavedFind;
  FFinder.OptWords:= bSavedWords;
  FFinder.OptRegex:= bSavedRegex;
  FFinder.OptCase:= bSavedCase;
end;


function TfmMain.DoFindOptionsToString: string;
begin
  Result:=
    //ignore OptBack
    IfThen(FFinder.OptCase, 'c')+
    IfThen(FFinder.OptRegex, 'r')+
    IfThen(FFinder.OptWords, 'w')+
    IfThen(FFinder.OptFromCaret, 'f')+
    IfThen(FFinder.OptInSelection, 's')+
    IfThen(FFinder.OptConfirmReplace, 'o')+
    IfThen(FFinder.OptWrapped, 'a');
end;

procedure TfmMain.DoFindOptionsFromString(const S: string);
begin
  FFinder.OptBack:= false; //ignore OptBack
  FFinder.OptCase:= Pos('c', S)>0;
  FFinder.OptRegex:= Pos('r', S)>0;
  FFinder.OptWords:= Pos('w', S)>0;
  FFinder.OptFromCaret:= Pos('f', S)>0;
  FFinder.OptInSelection:= Pos('s', S)>0;
  FFinder.OptConfirmReplace:= Pos('o', S)>0;
  FFinder.OptWrapped:= Pos('a', S)>0;
end;

procedure TfmMain.DoFindOptions_GetStrings(out AFind, AReplace: string);
begin
  AFind:= Utf8Encode(FFinder.StrFind);
  AReplace:= Utf8Encode(FFinder.StrReplace);
end;

procedure TfmMain.DoFindActionFromString(AStr: string);
var
  SAction, SFind, SRep, SOpt: string;
begin
  SAction:= SGetItem(AStr, #1);
  SFind:= SGetItem(AStr, #1);
  SRep:= SGetItem(AStr, #1);
  SOpt:= SGetItem(AStr, #1);

  FFinder.StrFind:= UTF8Decode(SFind);
  FFinder.StrReplace:= UTF8Decode(SRep);
  DoFindOptionsFromString(SOpt);
  FindDialogDone(nil{skip dialog}, SAction);
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
  end;
end;


