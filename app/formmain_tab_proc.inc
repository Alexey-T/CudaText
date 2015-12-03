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
  F.OnSetLexer:= @FrameOnSetLexer;
  F.OnAddRecent:= @FrameAddRecent;
  F.OnPyEvent:= @DoPyEvent;

  F.Groups:= Groups;

  F.Adapter.OnParseBegin:=@FrameParseBegin;
  F.Adapter.OnParseDone:=@FrameParseDone;

  F.Editor.PopupText:= PopupText;
  F.Editor2.PopupText:= PopupText;

  F.NotifEnabled:= UiOps.NotifEnabled;
  F.NotifTime:= UiOps.NotifTimeSec*1000;
end;

function TfmMain.DoTabAdd(Pages: TATPages): TATTabData;
var
  F: TEditorFrame;
  NTab: integer;
begin
  F:= TEditorFrame.Create(Self);
  F.Name:= '';
  F.Visible:= false;

  InitFrameEvents(F);

  Pages.AddTab(F, 'tab', false);
  NTab:= Pages.Tabs.TabCount-1;
  Result:= Pages.Tabs.GetTabData(NTab);
  Pages.Tabs.TabIndex:= NTab;
end;

procedure TfmMain.DoOnTabAdd(Sender: TObject);
var
  D: TATTabData;
begin
  D:= DoTabAdd((Sender as TATTabs).Parent as TATPages);
  DoTabUntitled(D);
end;

procedure TfmMain.DoTabUntitled(D: TATTabData);
begin
  (D.TabObject as TEditorFrame).TabCaption:= GetUntitlesStr;
end;

procedure TfmMain.DoOnTabFocus(Sender: TObject);
var
  Tabs: TATTabs;
  D: TATTabData;
  F: TEditorFrame;
begin
  Tabs:= Sender as TATTabs;
  D:= Tabs.GetTabData(Tabs.TabIndex);
  if D<>nil then
  begin
    F:= D.TabObject as TEditorFrame;
    F.Editor.SetFocus;
  end;

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
  NeedTreeUpd: boolean;
begin
  D:= (Sender as TATTabs).GetTabData(ATabIndex);
  Frame:= (D.TabObject as TEditorFrame);
  NeedTreeUpd:= Frame.Visible;

  if Frame.IsEmpty then
    if FrameCount=1 then
    begin
      ACanClose:= false;
      Exit
    end;

  if not D.TabModified then
    Res:= idok
  else
  begin
    if ACanContinue then Btn:= mb_yesnocancel else Btn:= mb_okcancel;
    Msg:= Format(msgConfirmSaveModifiedTab, [D.TabCaption]);
    Res:= MsgBox(Msg, Btn or mb_iconquestion);
    if (Res=idok) or (Res=idyes) then
      Frame.DoFileSave(false, SaveDlg);
  end;

  ACanClose:= Res<>idcancel;
  ACanContinue:= Res<>idcancel;

  if ACanClose then
  begin
    UpdateMenuRecent(Frame);
    D.TabObject.Free;
  end;

  if NeedTreeUpd then
    FrameParseBegin(nil);
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

procedure TfmMain.DoMoveTabTo(Num: Integer);
begin
  if (Num=2) and (Groups.Mode=gmOne) then
    Groups.Mode:= gm2Horz;
  Groups.MoveTab(Groups.PopupPages, Groups.PopupTabIndex, Groups.Pages[Num], -1, false);
end;

procedure TfmMain.mnuTabMove1Click(Sender: TObject);
begin
  DoMoveTabTo(1);
end;

procedure TfmMain.mnuTabMove2Click(Sender: TObject);
begin
  DoMoveTabTo(2);
end;

procedure TfmMain.mnuTabMove3Click(Sender: TObject);
begin
  DoMoveTabTo(3);
end;

procedure TfmMain.mnuTabMove4Click(Sender: TObject);
begin
  DoMoveTabTo(4);
end;

procedure TfmMain.mnuTabMove5Click(Sender: TObject);
begin
  DoMoveTabTo(5);
end;

procedure TfmMain.mnuTabMove6Click(Sender: TObject);
begin
  DoMoveTabTo(6);
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
  F.DoFileSave(true, SaveDlg);
end;

procedure TfmMain.mnuTabSaveClick(Sender: TObject);
var
  F: TEditorFrame;
begin
  F:= FrameOfPopup;
  if F=nil then exit;
  F.DoFileSave(false, SaveDlg);
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

  MsgStatus(F.FileName);
end;


