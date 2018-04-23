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

function TfmMain.DoTabAdd(APages: TATPages; const ACaption: string;
  AndActivate: boolean; AAllowNearCurrent: boolean): TATTabData;
var
  F: TEditorFrame;
  NToIndex: integer;
begin
  if (Groups.Pages1.Tabs.TabCount>0) and UiOps.TabsDisabled then
    exit(nil);

  F:= TEditorFrame.Create(Self);
  F.Name:= '';
  F.Visible:= false;
  F.TabCaption:= ACaption;

  InitFrameEvents(F);

  NToIndex:= -1;
  if AAllowNearCurrent and UiOps.TabNewNearCurrent then
    if APages.Tabs.TabIndex<APages.Tabs.TabCount-1 then
      NToIndex:= APages.Tabs.TabIndex+1;

  NToIndex:= APages.AddTab(NToIndex, F, F.TabCaption, false, clNone, AndActivate);
  APages.Tabs.MakeVisible(NToIndex);

  Result:= APages.Tabs.GetTabData(NToIndex);
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

  //prevent ListIndexError on file_open(..)
  ClearTreeviewWithData(CodeTree.Tree);

  F:= D.TabObject as TEditorFrame;
  F.SetFocus;

  //load lexer-specific config+keymap
  DoOps_LoadOptionsLexerSpecific(F);
  DoOps_LoadKeymap;

  UpdateStatus;
  DoPyEvent(CurrentEditor, cEventOnTabChange, []);
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

procedure TfmMain.DoClearSingleFirstTab;
var
  D: TATTabData;
  Frame: TEditorFrame;
  Msg: string;
begin
  D:= Groups.Pages1.Tabs.GetTabData(0);
  if not Assigned(D) then
    raise Exception.Create('Cannot get first tab');

  Frame:= (D.TabObject as TEditorFrame);
  if Frame.Modified then
  begin
    Msg:= Format(msgConfirmSaveModifiedTab, [Frame.TabCaption]);
    if MsgBox(Msg, MB_OKCANCEL or MB_ICONQUESTION) = ID_OK then
      Frame.DoFileSave(false);
  end;

  Frame.DoFileClose;
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
  if D=nil then exit;
  Frame:= (D.TabObject as TEditorFrame);
  bNeedTreeUpd:= Frame.Visible;
  bSavedOk:= true;

  if Frame.Editor.IsLocked then
  begin
    ACanClose:= false;
    Exit
  end;

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
    //need to activate tab, before msgbox
    (Sender as TATTabs).TabIndex:= ATabIndex;

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


function _LeveledTabCaption(const fn: string; NLevel: integer): string;
const
  cLine = ' • ';
var
  dir, dirs: string;
  i: integer;
begin
  Result:= ExtractFileName(fn);
  if NLevel>0 then
  begin
    dir:= ExtractFileDir(fn);
    dirs:= ExtractFileName(dir);
    for i:= 2 to NLevel do
    begin
      dir:= ExtractFileDir(dir);
      if (dir='') or (dir='/') then Break;
      dirs:= ExtractFileName(dir)+'/'+dirs;
    end;
    Result:= Result+cLine+dirs;
  end;
end;


procedure TfmMain.UpdateTabCaptionsFromFolders;
const
  cMaxLevels = 6;
var
  Frame: TEditorFrame;
  L_fn, L_cap: TStringList;
  NLevel, i, j: integer;
begin
  L_fn:= TStringList.Create;
  L_cap:= TStringList.Create;
  try
    for i:= 0 to FrameCount-1 do
    begin
      Frame:= Frames[i];
      if Assigned(Frame) then
        L_fn.Add(Frame.FileName);
    end;

    for i:= 0 to L_fn.Count-1 do
      L_cap.Add(ExtractFileName(L_fn[i]));

    for NLevel:= 0 to cMaxLevels do
      for i:= 0 to L_cap.Count-2 do
        if PtrInt(L_cap.Objects[i]) = NLevel then
          for j:= i+1 to L_cap.Count-1 do
            if L_cap[i]=L_cap[j] then
            begin
              L_cap[i]:= _LeveledTabCaption(L_fn[i], NLevel+1);
              L_cap[j]:= _LeveledTabCaption(L_fn[j], NLevel+1);
              L_cap.Objects[i]:= TObject(NLevel+1);
              L_cap.Objects[j]:= TObject(NLevel+1);
            end;

    for i:= 0 to L_fn.Count-1 do
      if L_fn[i]<>'' then
      begin
        Frame:= Frames[i];
        if Assigned(Frame) then
          Frame.TabCaption:= L_cap[i];
      end;
  finally
    FreeAndNil(L_cap);
    FreeAndNil(L_fn);
  end;
end;

