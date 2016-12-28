(*
This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at http://mozilla.org/MPL/2.0/.

Copyright (c) Alexey Torgashin
*)
{$ifdef nn}begin end;{$endif}

procedure TfmMain.FinderFound(Sender: TObject; APos1, APos2: TPoint);
var
  Ed: TATSynEdit;
  NLen: integer;
begin
  Ed:= FFinder.Editor;
  case FFindMarkingMode of
    markingNone:
      begin
        Ed.DoGotoPos_AndUnfold(
          APos1,
          APos2,
          UiOps.FindIndentHorz,
          UiOps.FindIndentVert);
      end;

    markingSelections:
      begin
        if FFindMarkingCaret1st then
        begin
          FFindMarkingCaret1st:= false;
          Ed.Carets.Clear;
        end;
        Ed.Carets.Add(APos2.X, APos2.Y, APos1.X, APos1.Y);
        Ed.Carets.Sort;
      end;

    markingMarkers:
      begin
        //marker sel is only 1 line
        if APos2.Y=APos1.Y then
          NLen:= Abs(APos2.X-APos1.X)
        else
          NLen:= 0;
        Ed.Markers.Add(APos1.X, APos1.Y, 0, NLen, 0, nil, true{InsertToBegin});
      end;

    markingBookmarks:
      begin
        EditorBookmarkSet(Ed, APos1.Y, 1, bmOpSet, '');
      end;
  end;
end;


procedure TfmMain.FindDialogDone(Sender: TObject; const Res: string);
var
  ok, bChanged: boolean;
  cnt, NTop, NLeft,
  NCaretX, NCaretY: integer;
begin
  if Res=cOpFindClose then
  begin
    if IsFocusedFind then
      EditorFocus(CurrentEditor);
    fmFind.Hide;
    UpdateAppForSearch(false);
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
    FFinder.OptFromCaret:= (Res=cOpFindNext) or (Res=cOpFindPrev) or (Res=cOpFindRep);
    FFinder.OptConfirmReplace:= chkConfirm.Checked;
  end;

  with CurrentFrame do
    if MacroRecord then
      MacroString:= MacroString+(Format('%d,%s'#1'%s'#1'%s'#1'%s',
        [cmd_FinderAction, Res, FFinder.StrFind, FFinder.StrReplace, DoFindOptionsToString])+#10);


  UpdateAppForSearch(true);
  FFinder.UpdateBuffer;

  if (Res=cOpFindFirst) or (Res=cOpFindNext) or (Res=cOpFindPrev) then
  begin
    FFinder.OptBack:= (Res=cOpFindPrev);
    ok:= FFinder.DoFindOrReplace(false, false, false, bChanged);
    FinderUpdateEditor(false);
    DoFindResult(ok);
  end;

  if (Res=cOpFindRep) or (Res=cOpFindRepAndStop) then
  begin
    //replace match
    ok:= FFinder.DoReplaceSelectedMatch;
    //after match is replaced, do find-next
    if Res=cOpFindRep then
      if ok then
        ok:= FFinder.DoFindOrReplace(false, false, false, bChanged);
    FinderUpdateEditor(true);
    DoFindResult(ok);
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

    FFinder.Editor.Strings.BeginUndoGroup;
    cnt:= FFinder.DoReplaceAll;
    FFinder.Editor.Strings.EndUndoGroup;
    FinderUpdateEditor(true);

    FFinder.Editor.DoCaretSingle(NCaretX, NCaretY);
    FFinder.Editor.LineTop:= NTop;
    FFinder.Editor.ColumnLeft:= NLeft;
    MsgStatus(Format(msgStatusReplaceCount, [cnt]));
  end;

  if Res=cOpFindCount then
  begin
    DoFindMarkingInit(markingNone);
    cnt:= FFinder.DoCountAll(false);
    MsgStatus(Format(msgStatusFindCount, [Utf8Encode(FFinder.StrFind), cnt]));
  end;

  if Res=cOpFindSelectAll then
  begin
    DoFindMarkingInit(markingSelections);
    cnt:= FFinder.DoCountAll(true);
    DoFindMarkingInit(markingNone);
    FinderUpdateEditor(false);
    MsgStatus(Format(msgStatusFindCount, [Utf8Encode(FFinder.StrFind), cnt]));
  end;

  if Res=cOpFindMarkAll then
  begin
    DoFindMarkingInit(markingMarkers);
    cnt:= FFinder.DoCountAll(true);
    DoFindMarkingInit(markingNone);
    FinderUpdateEditor(false);
    MsgStatus(Format(msgStatusFindCount, [Utf8Encode(FFinder.StrFind), cnt]));
  end;

  UpdateAppForSearch(false);
end;

procedure TfmMain.InitFormFind;
var
  cfg: TJsonConfig;
begin
  if not Assigned(fmFind) then
  begin
    fmFind:= TfmFind.Create(Self);
    fmFind.OnDone:= @FindDialogDone;
    fmFind.FHotkeyFind:= AppKeymap.GetShortcutFromCommand(cmd_DialogFind);
    fmFind.FHotkeyRep:= AppKeymap.GetShortcutFromCommand(cmd_DialogReplace);
    fmFind.Color:= GetAppColor('TabBg');

    if not UiOps.FindSeparateForm then
    begin
      fmFind.Parent:= PanelMain;
      fmFind.Align:= alBottom;
      fmFind.BorderStyle:= bsNone;
    end
    else
    begin
      fmFind.IsNarrow:= true;
    end;

    cfg:= TJSONConfig.Create(nil);
    try
      cfg.Filename:= GetAppPath(cFileOptHistory);
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
  StrSel:= Ed.TextSelected;
  StrWord:= Ed.TextCurrentWord;

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
    //chkFromCaret.Checked:= FFinder.OptFromCaret;
    chkConfirm.Checked:= FFinder.OptConfirmReplace;
    chkWrap.Checked:= FFinder.OptWrapped;
    IsReplace:= AReplaceMode;

    UpdateState;
    Show;
    edFind.SetFocus;
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

  FFinder.UpdateBuffer;
  ok:= FFinder.DoFindOrReplace(false, false, false, bChanged);
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

  FFinder.UpdateBuffer;
  ok:= FFinder.DoFindOrReplace(false, false, false, bChanged);
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
        Ed.DoCaretSingleAsIs;
      end;
    markingMarkers:
      begin
        Ed.DoCaretSingleAsIs;
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

  FFinder.UpdateBuffer;

  DoFindMarkingInit(AMode);
  cnt:= FFinder.DoCountAll(true);
  DoFindMarkingInit(markingNone);

  FinderUpdateEditor(false);
  MsgStatus(Format(msgStatusFindCount, [Utf8Encode(FFinder.StrFind), cnt]));
end;


procedure TfmMain.FinderProgress(Sender: TObject; ACurPos, AMaxPos: integer;
  var AContinue: boolean);
begin
  Gauge.Progress:= ACurPos * 100 div AMaxPos;
  Application.ProcessMessages;
  if FFindStop then AContinue:= false;
end;

function TfmMain.SFindOptionsToTextHint: string;
begin
  Result:= '';
  if FFinder.OptRegex then Result:= Result+'regex, ';
  if FFinder.OptCase then Result:= Result+'case, ';
  if FFinder.OptWords then Result:= Result+'words, ';
  if FFinder.OptBack then Result:= Result+'back, ';
  if FFinder.OptWrapped then Result:= Result+'wrapped, ';
  if FFinder.OptFromCaret then Result:= Result+'from-caret, ';
  if SEndsWith(Result, ', ') then
    SetLength(Result, Length(Result)-2);
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

procedure TfmMain.FinderConfirmReplace(Sender: TObject; APos1, APos2: TPoint;
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
      fmConfirmReplace.Width:= fmFind.Width;
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

procedure TfmMain.FinderBadRegex(Sender: TObject);
begin
  MsgBox(
    msgStatusBadRegex+#13+Utf8Encode(FFinder.StrFind),
    mb_ok or mb_iconerror);
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
  FFinder.Editor.DoGotoPos(Pnt, UiOps.FindIndentHorz, UiOps.FindIndentVert);
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

  if AWordOrSel then
    Str:= Ed.TextCurrentWord
  else
  begin
    Str:= Ed.TextSelected;
    if Str='' then
      Str:= Ed.TextCurrentWord;
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

  case UiOps.FindSelCase of
    0: FFinder.OptCase:= false;
    1: FFinder.OptCase:= true;
    2:
      begin
        if Assigned(fmFind) then
          FFinder.OptCase:= fmFind.chkCase.Checked;
      end;
  end;

  FFinder.UpdateBuffer;
  ok:= FFinder.DoFindOrReplace(false, false, false, bFlag);
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
    //IfThen(FFinder.OptBack, 'b')+
    IfThen(FFinder.OptCase, 'c')+
    IfThen(FFinder.OptRegex, 'r')+
    IfThen(FFinder.OptWords, 'w')+
    IfThen(FFinder.OptFromCaret, 'f')+
    IfThen(FFinder.OptConfirmReplace, 'o')+
    IfThen(FFinder.OptWrapped, 'a')
end;

procedure TfmMain.DoFindOptionsFromString(const S: string);
begin
  FFinder.OptBack:= false; //Pos('b', S)>0;
  FFinder.OptCase:= Pos('c', S)>0;
  FFinder.OptRegex:= Pos('r', S)>0;
  FFinder.OptWords:= Pos('w', S)>0;
  FFinder.OptFromCaret:= Pos('f', S)>0;
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

