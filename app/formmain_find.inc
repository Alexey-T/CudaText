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
begin
  if FFindMark then
  begin
    Ed:= FFinder.Editor;
    Ed.Carets.Add(APos1.X, APos1.Y, APos2.X, APos2.Y);
    Ed.Carets.Sort;
  end;
end;

procedure TfmMain.FindDialogDone(Sender: TObject; const Res: string);
var
  ok, bChanged: boolean;
  cnt, NTop: integer;
begin
  with fmFind do
  begin
    if Res=cOpFindClose then
    begin
      if IsFocusedFind then
        CurrentEditor.SetFocus; //must for Win
      fmFind.Hide;
      UpdateAppForSearch(false);
      Exit;
    end;

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

  UpdateAppForSearch(true);
  FFinder.UpdateBuffer;

  if (Res=cOpFindFirst) or (Res=cOpFindNext) or (Res=cOpFindPrev) then
  begin
    FFinder.OptBack:= (Res=cOpFindPrev);
    ok:= FFinder.DoFindOrReplace(false, false, false, bChanged);
    FinderUpdateEditor(false);
    DoFindResult(ok);
  end;

  if Res=cOpFindRep then
  begin
    //replace match
    ok:= FFinder.DoReplaceSelectedMatch;
    //after match is replaced, do find-next
    if ok then
      ok:= FFinder.DoFindOrReplace(false, false, false, bChanged);
    FinderUpdateEditor(true);
    DoFindResult(ok);
  end;

  if Res=cOpFindRepAll then
  begin
    NTop:= FFinder.Editor.LineTop;
    cnt:= FFinder.DoReplaceAll;
    FinderUpdateEditor(true);
    FFinder.Editor.LineTop:= NTop;
    MsgStatus(msgStatusReplaceCount+Inttostr(cnt));
  end;

  if Res=cOpFindCount then
  begin
    cnt:= FFinder.DoCountAll(false);
    MsgStatus(Format(msgStatusFindCount, [Utf8Encode(FFinder.StrFind)])+Inttostr(cnt));
  end;

  if Res=cOpFindMarkAll then
  begin
    FFindMark:= true;
    cnt:= FFinder.DoCountAll(true);
    FFindMark:= false;
    FinderUpdateEditor(false);
    MsgStatus(Format(msgStatusFindCount, [Utf8Encode(FFinder.StrFind)])+Inttostr(cnt));
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
    fmFind.Parent:= PanelMain;
    fmFind.Align:= alBottom;
    fmFind.FHotkeyFind:= Keymap.GetShortcutFromCommand(cmd_DialogFind);
    fmFind.FHotkeyRep:= Keymap.GetShortcutFromCommand(cmd_DialogReplace);
    fmFind.Color:= GetAppColor('TabBg');

    cfg:= TJSONConfig.Create(nil);
    try
      cfg.Filename:= GetAppPath(cFileOptHistory);
      SLoadStringsFromFile(cfg, '/list_find', fmFind.edFind.Items, UiOps.MaxHistoryMenu);
      SLoadStringsFromFile(cfg, '/list_replace', fmFind.edRep.Items, UiOps.MaxHistoryMenu);
    finally
      cfg.Free
    end;
  end;
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

    if UiOps.FindSuggestSel and (StrSel<>'') then
      edFind.Text:= StrSel
    else
    if UiOps.FindSuggestWord and (StrWord<>'') then
      edFind.Text:= StrWord
    else
      edFind.Text:= FFinder.StrFind;
    edFind.DoCommand(cCommand_SelectAll);

    edRep.Text:= FFinder.StrReplace;
    edRep.DoCommand(cCommand_SelectAll);

    chkCase.Checked:= FFinder.OptCase;
    chkWords.Checked:= FFinder.OptWords;
    chkRegex.Checked:= FFinder.OptRegex;
    //chkFromCaret.Checked:= FFinder.OptFromCaret;
    chkConfirm.Checked:= FFinder.OptConfirmReplace;
    chkWrap.Checked:= FFinder.OptWrapped;
    Replace:= AReplaceMode;

    Show;
    UpdateState;
    edFind.SetFocus;
  end;
end;

procedure TfmMain.DoFindNext(ANext: boolean);
var
  ok, fchanged: boolean;
begin
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
  ok:= FFinder.DoFindOrReplace(false, false, false, fchanged);
  FinderUpdateEditor(false);
  DoFindResult(ok);
end;

procedure TfmMain.FinderProgress(Sender: TObject; ACurPos, AMaxPos: integer;
  var AContinue: boolean);
begin
  Gauge.Progress:= ACurPos * 100 div AMaxPos;
  Application.ProcessMessages;
  if FFindStop then AContinue:= false;
end;

procedure TfmMain.DoFindResult(ok: boolean);
begin
  if ok then
    MsgStatus(msgStatusFoundText)
  else
    MsgStatus(msgStatusCannotFindText+Utf8Encode(FFinder.StrFind));
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
    fmConfirmReplace.LabelInfo.Caption:= Format(msgConfirmReplaceText, [APos1.Y+1]);
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
    msgErrRegex+#13+Utf8Encode(FFinder.StrFind),
    mb_ok or mb_iconerror);
end;

procedure TfmMain.FinderUpdateEditor(AUpdateText: boolean);
var
  Pnt: TPoint;
begin
  //call to AdapterHilite:
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
end;

