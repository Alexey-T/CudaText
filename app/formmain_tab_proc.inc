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
  if F.CanFocus and F.CanSetFocus then
    F.SetFocus;

  //load lexer-specific config+keymap
  DoOps_LoadOptionsLexerSpecific(F);
  DoOps_LoadKeymap;

  UpdateStatus;
  DoPyEvent(CurrentEditor, cEventOnTabChange, []);
end;

procedure TfmMain.UpdateTabsActiveColor(F: TEditorFrame);
var
  Gr: TATGroups;
  Pages: TATPages;
  NColorAct, NColorOther: TColor;
  NLocalGroups, NGlobalGroup, NTab, i: integer;
begin
  NColorAct:= GetAppColor('TabActive');
  NColorOther:= GetAppColor('TabActiveOthers');

  GetFrameLocation(F, Gr, Pages, NLocalGroups, NGlobalGroup, NTab);

  for i:= Low(TATGroupsNums) to High(TATGroupsNums) do
    with Gr.Pages[i] do
    begin
      Tabs.ColorTabActive:= IfThen(i=NLocalGroups, NColorAct, NColorOther);
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

procedure TfmMain.DoOnTabPopup(Sender: TObject; APages: TATPages; ATabIndex: integer);
begin
  if Assigned(APages) and (ATabIndex>=0) then
    PopupTab.Popup;
end;

procedure TfmMain.mnuTabCloseAllAllClick(Sender: TObject);
begin
  GroupsCtx.CloseTabs(tabCloseAll, false);
end;

procedure TfmMain.mnuTabCloseAllSameClick(Sender: TObject);
begin
  GroupsCtx.CloseTabs(tabCloseAllThisPage, true);
end;

procedure TfmMain.mnuTabCloseLeftClick(Sender: TObject);
begin
  GroupsCtx.CloseTabs(tabCloseLefterThisPage, true);
end;

procedure TfmMain.mnuTabCloseOtherAllClick(Sender: TObject);
begin
  GroupsCtx.CloseTabs(tabCloseOthersAllPages, true);
end;

procedure TfmMain.mnuTabCloseOtherSameClick(Sender: TObject);
begin
  GroupsCtx.CloseTabs(tabCloseOthersThisPage, true);
end;

procedure TfmMain.mnuTabCloseRightClick(Sender: TObject);
begin
  GroupsCtx.CloseTabs(tabCloseRighterThisPage, true);
end;

procedure TfmMain.mnuTabCloseThisClick(Sender: TObject);
begin
  GroupsCtx.PopupPages.Tabs.DeleteTab(GroupsCtx.PopupTabIndex, true, true);
end;

procedure TfmMain.DoMoveTabToGroup(AGroupIndex: Integer);
var
  Pages, PagesTo: TATPages;
  NTabIndex: integer;
begin
  Pages:= GroupsCtx.PopupPages;
  NTabIndex:= GroupsCtx.PopupTabIndex;

  //force 2 groups if only one
  if (AGroupIndex=1) and (Groups.Mode=gmOne) then
  begin
    Groups.Mode:= gm2Horz;
    Groups.Update; //maybe no need
  end;

  case AGroupIndex of
    0..High(TATGroupsNums):
      PagesTo:= Groups.Pages[AGroupIndex];
    6:
      begin
        ShowFloatGroup1:= true;
        PagesTo:= GroupsF1.Pages1;
      end;
    7:
      begin
        ShowFloatGroup2:= true;
        PagesTo:= GroupsF2.Pages1;
      end;
    8:
      begin
        ShowFloatGroup3:= true;
        PagesTo:= GroupsF3.Pages1;
      end;
  end;

  GroupsCtx.MoveTab(Pages, NTabIndex, PagesTo, -1, false);
end;

procedure TfmMain.mnuTabMove1Click(Sender: TObject);
begin
  DoMoveTabToGroup(0);
end;

procedure TfmMain.mnuTabMove2Click(Sender: TObject);
begin
  DoMoveTabToGroup(1);
end;

procedure TfmMain.mnuTabMove3Click(Sender: TObject);
begin
  DoMoveTabToGroup(2);
end;

procedure TfmMain.mnuTabMove4Click(Sender: TObject);
begin
  DoMoveTabToGroup(3);
end;

procedure TfmMain.mnuTabMove5Click(Sender: TObject);
begin
  DoMoveTabToGroup(4);
end;

procedure TfmMain.mnuTabMove6Click(Sender: TObject);
begin
  DoMoveTabToGroup(5);
end;

procedure TfmMain.mnuTabMoveF1Click(Sender: TObject);
begin
  DoMoveTabToGroup(6);
end;

procedure TfmMain.mnuTabMoveF2Click(Sender: TObject);
begin
  DoMoveTabToGroup(7);
end;

procedure TfmMain.mnuTabMoveF3Click(Sender: TObject);
begin
  DoMoveTabToGroup(8);
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
  cMaxLevels = 5;
var
  Frame: TEditorFrame;
  L_fn, L_cap: TStringList;
  L_found: TList;
  NLevel, i, j: integer;
begin
  L_fn:= TStringList.Create;
  L_cap:= TStringList.Create;
  L_found:= TList.Create;

  try
    for i:= 0 to FrameCount-1 do
    begin
      Frame:= Frames[i];
      L_fn.Add(Frame.FileName);
      if Frame.FileName<>'' then
        L_cap.Add(ExtractFileName(Frame.FileName))
      else
        L_cap.Add(Frame.TabCaption);
    end;

    for NLevel:= 0 to cMaxLevels do
    begin
      L_found.Clear;

      for i:= 0 to L_cap.Count-2 do
        if PtrInt(L_cap.Objects[i]) = NLevel then
          for j:= i+1 to L_cap.Count-1 do
            if L_cap[i]=L_cap[j] then
            begin
              if L_found.IndexOf(Pointer(i))<0 then
                L_found.Add(Pointer(i));
              if L_found.IndexOf(Pointer(j))<0 then
                L_found.Add(Pointer(j));
            end;

      if L_found.Count=0 then Break;
      for i:= 0 to L_found.Count-1 do
      begin
        j:= PtrInt(L_found[i]);
        L_cap[j]:= _LeveledTabCaption(L_fn[j], NLevel+1);
        L_cap.Objects[j]:= TObject(NLevel+1);
      end;
    end;

    for i:= 0 to L_fn.Count-1 do
      if L_fn[i]<>'' then
      begin
        Frame:= Frames[i];
        if Frame.TabCaption<>L_cap[i] then
          Frame.TabCaption:= L_cap[i];
      end;
  finally
    FreeAndNil(L_found);
    FreeAndNil(L_cap);
    FreeAndNil(L_fn);
  end;
end;

