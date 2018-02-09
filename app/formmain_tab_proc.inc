(*
This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at http://mozilla.org/MPL/2.0/.

Copyright (c) Alexey Torgashin
*)
{$ifdef nn}begin end;{$endif}

procedure TfmMain.InitFrameEvents(F: TEditorFrame);
begin
  F.OnFocusEditor:= @FrameOnFocus;
  F.OnChangeCaption:= @FrameOnChangeCaption;
  F.OnUpdateStatus:= @FrameOnUpdateStatus;
  F.OnEditorCommand:= @FrameOnCommand;
  F.OnEditorChangeCaretPos:= @FrameOnChangeCaretPos;
  F.OnEditorClickMoveCaret:= @FrameOnEditorClickMoveCaret;
  F.OnEditorClickEndSelect:= @FrameOnEditorClickEndSelect;
  F.OnSaveFile:= @FrameOnSaveFile;
  F.OnAddRecent:= @FrameAddRecent;
  F.OnPyEvent:= @DoPyEvent;
  F.OnCheckFilenameOpened:= @DoCheckFilenameOpened;
  F.OnMsgStatus:= @FrameOnMsgStatus;
  F.OnProgress:= @FinderOnProgress;

  F.Groups:= Groups;
  F.SaveDialog:= SaveDlg;

  //F.Adapter.OnParseBegin:= @FrameParseBegin;
  F.Adapter.OnParseDone:= @FrameParseDone;
  F.Adapter.OnLexerChange:= @FrameLexerChange;

  F.Editor.PopupText:= PopupText;
  F.Editor2.PopupText:= PopupText;

  F.NotifEnabled:= false;
  F.NotifTime:= UiOps.NotifTimeSec*1000;

  F.Splitter.OnPaint:= @SplitterOnPaint_Gr;
end;

function TfmMain.DoTabAdd(APages: TATPages; const ACaption: string): TATTabData;
var
  F: TEditorFrame;
  NTabEnd, NTabNear: integer;
begin
  F:= TEditorFrame.Create(Self);
  F.Name:= '';
  F.Visible:= false;
  F.TabCaption:= ACaption;

  InitFrameEvents(F);

  NTabNear:= APages.Tabs.TabIndex+1;
  APages.AddTab(F, F.TabCaption, false);
  NTabEnd:= APages.Tabs.TabCount-1;

  if UiOps.TabNewNearCurrent and (NTabEnd<>NTabNear) then
    APages.Tabs.MoveTab(NTabEnd, NTabNear, true)
  else
    APages.Tabs.TabIndex:= NTabEnd;

  Result:= APages.Tabs.GetTabData(APages.Tabs.TabIndex);
end;

procedure TfmMain.DoOnTabAdd(Sender: TObject);
var
  Pages: TATPages;
begin
  Pages:= (Sender as TATTabs).Parent as TATPages;
  DoTabAdd(Pages, GetUntitledCaption);
end;

procedure TfmMain.DoOnTabFocus(Sender: TObject);
var
  Tabs: TATTabs;
  D: TATTabData;
  F: TEditorFrame;
begin
  Tabs:= Sender as TATTabs;
  D:= Tabs.GetTabData(Tabs.TabIndex);
  if D=nil then exit;

  F:= D.TabObject as TEditorFrame;
  F.SetFocus;

  //load lexer-specific config+keymap
  DoOps_LoadOptionsLexerSpecific(F);
  DoOps_LoadKeymap;

  UpdateStatus;
end;

procedure TfmMain.UpdateTabsActiveColor(F: TEditorFrame);
var
  NColorAct, NColorOther: TColor;
  NPages, NTab, i: integer;
begin
  Groups.PagesAndTabIndexOfControl(F, NPages, NTab);
  NColorAct:= GetAppColor('TabActive');
  NColorOther:= GetAppColor('TabActiveOthers');

  for i:= Low(TATGroupsNums) to High(TATGroupsNums) do
    with Groups.Pages[i] do
    begin
      Tabs.ColorTabActive:= IfThen(i=NPages, NColorAct, NColorOther);
      Tabs.Invalidate;
    end;
end;

procedure TfmMain.DoOnTabClose(Sender: TObject; ATabIndex: Integer;
  var ACanClose, ACanContinue: boolean);
var
  D: TATTabData;
  Frame: TEditorFrame;
  Btn, Res: Integer;
  Msg: string;
  bNeedTreeUpd, bSavedOk: boolean;
begin
  D:= (Sender as TATTabs).GetTabData(ATabIndex);
  Frame:= (D.TabObject as TEditorFrame);
  bNeedTreeUpd:= Frame.Visible;
  bSavedOk:= true;

  if Frame.IsEmpty then
    if FrameCount=1 then
    begin
      ACanClose:= false;
      Exit
    end;

  if Application.Terminated then
    Res:= ID_OK
  else
  if not D.TabModified then
    Res:= ID_OK
  else
  begin
    if ACanContinue then Btn:= MB_YESNOCANCEL else Btn:= MB_OKCANCEL;
    Msg:= Format(msgConfirmSaveModifiedTab, [D.TabCaption]);
    Res:= MsgBox(Msg, Btn or MB_ICONQUESTION);
    if (Res=ID_OK) or (Res=ID_YES) then
      bSavedOk:= Frame.DoFileSave(false);
  end;

  ACanClose:= (Res<>ID_CANCEL) and bSavedOk;
  ACanContinue:= (Res<>ID_CANCEL);

  if ACanClose then
  begin
    UpdateMenuRecent(Frame);
    Frame.Editor.AdapterForHilite:= nil;
    Frame.Editor2.AdapterForHilite:= nil;
    Frame.Adapter.StopTreeUpdate;
    Frame.Adapter.Stop;
    Frame.Free;
  end;

  if not Application.Terminated then
    if bNeedTreeUpd then
      ClearTreeviewWithData(CodeTree.Tree);
end;

procedure TfmMain.DoOnTabPopup(Sender: TObject);
begin
  if Groups.PopupPages=nil then exit;
  if Groups.PopupTabIndex<0 then exit;
  PopupTab.Popup;
end;

procedure TfmMain.mnuTabCloseAllAllClick(Sender: TObject);
begin
  Groups.CloseTabs(tabCloseAll, false);
end;

procedure TfmMain.mnuTabCloseAllSameClick(Sender: TObject);
begin
  Groups.CloseTabs(tabCloseAllThisPage, true);
end;

procedure TfmMain.mnuTabCloseLeftClick(Sender: TObject);
begin
  Groups.CloseTabs(tabCloseLefterThisPage, true);
end;

procedure TfmMain.mnuTabCloseOtherAllClick(Sender: TObject);
begin
  Groups.CloseTabs(tabCloseOthersAllPages, true);
end;

procedure TfmMain.mnuTabCloseOtherSameClick(Sender: TObject);
begin
  Groups.CloseTabs(tabCloseOthersThisPage, true);
end;

procedure TfmMain.mnuTabCloseRightClick(Sender: TObject);
begin
  Groups.CloseTabs(tabCloseRighterThisPage, true);
end;

procedure TfmMain.mnuTabCloseThisClick(Sender: TObject);
begin
  Groups.PopupPages.Tabs.DeleteTab(Groups.PopupTabIndex, true, true);
end;

procedure TfmMain.DoMoveTabTo(AIndex: Integer);
var
  Pages: TATPages;
  NIndex: integer;
begin
  Pages:= Groups.PopupPages;
  NIndex:= Groups.PopupTabIndex;

  //force 2 groups if only one
  if (AIndex=1) and (Groups.Mode=gmOne) then
  begin
    Groups.Mode:= gm2Horz;
    Groups.Update; //maybe no need
    Pages:= Groups.Pages1; //maybe no need
  end;

  Groups.MoveTab(Pages, NIndex, Groups.Pages[AIndex], -1, false);
end;

procedure TfmMain.mnuTabMove1Click(Sender: TObject);
begin
  DoMoveTabTo(0);
end;

procedure TfmMain.mnuTabMove2Click(Sender: TObject);
begin
  DoMoveTabTo(1);
end;

procedure TfmMain.mnuTabMove3Click(Sender: TObject);
begin
  DoMoveTabTo(2);
end;

procedure TfmMain.mnuTabMove4Click(Sender: TObject);
begin
  DoMoveTabTo(3);
end;

procedure TfmMain.mnuTabMove5Click(Sender: TObject);
begin
  DoMoveTabTo(4);
end;

procedure TfmMain.mnuTabMove6Click(Sender: TObject);
begin
  DoMoveTabTo(5);
end;

procedure TfmMain.mnuTabMoveNextClick(Sender: TObject);
begin
  Groups.MovePopupTabToNext(true);
end;

procedure TfmMain.mnuTabMovePrevClick(Sender: TObject);
begin
  Groups.MovePopupTabToNext(false);
end;

procedure TfmMain.mnuTabSaveAsClick(Sender: TObject);
var
  F: TEditorFrame;
begin
  F:= FrameOfPopup;
  if F=nil then exit;
  F.DoFileSave(true);
end;

procedure TfmMain.mnuTabSaveClick(Sender: TObject);
var
  F: TEditorFrame;
begin
  F:= FrameOfPopup;
  if F=nil then exit;
  F.DoFileSave(false);
end;

procedure TfmMain.mnuTabsizeSpaceClick(Sender: TObject);
begin
  if CurrentEditor.OptTabSpaces then
    UpdateEditorTabsize(-2)
  else
    UpdateEditorTabsize(-1)
end;

procedure TfmMain.DoOnTabOver(Sender: TObject; ATabIndex: Integer);
var
  D: TATTabData;
  F: TEditorFrame;
begin
  if ATabIndex<0 then exit;
  D:= (Sender as TATTabs).GetTabData(ATabIndex);
  if D=nil then exit;
  F:= D.TabObject as TEditorFrame;
  if F=nil then exit;

  if not F.IsText then
    MsgStatus(F.PictureFileName)
  else
    MsgStatus(F.FileName);
end;

procedure TfmMain.DoOnTabMove(Sender: TObject; NFrom, NTo: Integer);
begin
  //tab closed: set flag
  if NTo=-1 then
    PyEditorMaybeDeleted:= true;

  DoPyEvent(CurrentEditor, cEventOnTabMove, []);
end;


