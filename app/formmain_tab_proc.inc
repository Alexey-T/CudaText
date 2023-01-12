(*
This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at http://mozilla.org/MPL/2.0/.

Copyright (c) Alexey Torgashin
*)
{$ifdef nn}begin end;{$endif}

procedure TfmMain.InitFrameEvents(F: TEditorFrame);
begin
  F.OnFocusEditor:= @FrameOnEditorFocus;
  F.OnChangeCaption:= @FrameOnChangeCaption;
  F.OnChangeSlow:= @FrameOnChangeSlow;
  F.OnUpdateStatusbar:= @FrameOnUpdateStatusbar;
  F.OnUpdateState:= @FrameOnUpdateState;
  F.OnUpdateZoom:= @FrameOnUpdateZoom;
  F.OnEditorCommand:= @FrameOnEditorCommand;
  F.OnEditorChangeCaretPos:= @FrameOnEditorChangeCaretPos;
  F.OnEditorScroll:= @FrameOnEditorScroll;
  F.OnSaveFile:= @FrameOnSaveFile;
  F.OnAddRecent:= @FrameAddRecent;
  F.OnPyEvent:= @DoPyEvent;
  F.OnCheckFilenameOpened:= @DoCheckFilenameOpened;
  F.OnMsgStatus:= @FrameOnMsgStatus;
  F.OnProgress:= @FinderOnProgress;
  F.OnInitAdapter:= @FrameOnInitAdapter;
  F.OnLexerChange:= @FrameLexerChange;
  F.OnAppClickLink:= @FrameConfirmLink;
  F.LexerChooseFunc:= @DoDialogMenuLexerChoose;
  F.OnGetSaveDialog:= @DoGetSaveDialog;
  F.Splitter.OnPaint:= @SplitterOnPaintDummy;

  F.Groups:= Groups;
  F.OnInitAdapter(F.Adapter[F.Ed1]);

  F.Ed1.PopupText:= PopupText;
  F.Ed2.PopupText:= PopupText;

  F.NotifEnabled:= false;
end;

function TfmMain.CreateTab(APages: TATPages; const ACaption: string;
  AndActivate: boolean; AAllowNearCurrent: TApp3States): TATTabData;
var
  F: TEditorFrame;
  Data: TATTabData;
  NToIndex: integer;
  bAllowNear: boolean;
begin
  if (Groups.Pages1.Tabs.TabCount>0) and UiOps.TabsDisabled then
    exit(nil);

  F:= TEditorFrame.Create(Self, Groups.Mode=gmOne);
  F.Name:= '';
  F.Visible:= false;
  F.TabCaption:= ACaption;

  if ACaption='' then
  begin
    F.TabCaptionUntitled:= GetUntitledNumberedCaption;
    F.TabCaption:= F.TabCaptionUntitled;
  end;

  InitFrameEvents(F);

  case AAllowNearCurrent of
    a3sOff:
      bAllowNear:= false;
    a3sOn:
      bAllowNear:= true;
    a3sPassive:
      bAllowNear:= UiOps.TabNewNearCurrent;
  end;

  NToIndex:= -1;
  if bAllowNear then
    if APages.Tabs.TabIndex<APages.Tabs.TabCount-1 then
      NToIndex:= APages.Tabs.TabIndex+1;

  Data:= TATTabData.Create(nil);
  try
    Data.TabObject:= F;
    Data.TabCaption:= F.TabCaption;
    Data.TabCaptionAddon:= F.TabCaptionAddon;
    NToIndex:= APages.AddTab(NToIndex, Data, AndActivate);
  finally
    Data.Free;
  end;

  if AndActivate then
    APages.Tabs.MakeVisible(NToIndex);

  AppFrameList1.Add(F);

  Result:= APages.Tabs.GetTabData(NToIndex);
end;

function TfmMain.IsTooManyTabsOpened: boolean;
begin
  Result:= FrameCount>(cPyEditorHandleMax-cPyEditorHandleMin);
  if Result then
    MsgLogConsole(msgErrorTooManyFileTabs);
end;

procedure TfmMain.DoOnTabAdd(Sender: TObject);
var
  Pages: TATPages;
  Data: TATTabData;
begin
  DoTooltipHide;

  if IsTooManyTabsOpened then exit;

  Pages:= (Sender as TATTabs).Parent as TATPages;
  Data:= CreateTab(Pages, '');

  if Assigned(Data) then
    DoApplyNewdocLexer(Data.TabObject as TEditorFrame);
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

  DoTooltipHide;

  F:= D.TabObject as TEditorFrame;

  //avoid duplicate calls when session is loaded (and TabIndex is set there 2 times)
  if AppSessionIsLoading then
  begin
    if F=FLastFocusedFrame then exit;
    FLastFocusedFrame:= F;
  end;

  if not F.IsEditorFocused then
  begin
    //prevent ListIndexError on file_open(..)
    if not FDisableTreeClearing then
      if F.Editor<>AppCodetreeState.Editor then
        DoCodetree_Clear;
  end;

  //update toolbar pressed-states
  UpdateToolbarButtons(F);

  //if we fire 'on_tab_change' during session loading, Auto Save plugin may activate
  //and overwrite the session (it saves the session)
  if not (AppSessionIsLoading or AppSessionIsClosing) then
    DoOnTabFocusFinalization(F, true);
end;

procedure TfmMain.DoOnTabFocusFinalization(F: TEditorFrame; AAllowEventOnTabChange: boolean);
var
  SFilterText: UnicodeString;
begin
  if F=nil then exit;
  if not F.IsEditorFocused then
    if F.CanFocus and F.CanSetFocus then
      F.SetFocus;

  SFilterText:= UTF8Decode(F.CodetreeFilter);
  if CodeTreeFilterInput.Text<>SFilterText then
  begin
    CodeTreeFilterInput.Text:= SFilterText;
    CodeTreeFilter_OnChange(nil);
    CodeTreeFilterInput.Items.Assign(F.CodetreeFilterHistory);
  end;

  //load lexer-specific config+keymap
  DoOps_LoadOptionsLexerSpecific(F, F.Ed1);
  if not F.EditorsLinked then
    DoOps_LoadOptionsLexerSpecific(F, F.Ed2);

  UpdateStatusbar;

  if AAllowEventOnTabChange then
    DoPyEvent(F.Editor, cEventOnTabChange, []);

  UpdateFindDialogOnTabFocusing(F);
end;

procedure TfmMain.UpdateTabsActiveColor(F: TEditorFrame);
var
  Gr: TATGroups;
  Pages: TATPages;
  NColorTabActive, NColorTabOther: TColor;
  NColorMarkActive, NColorMarkOther: TColor;
  NLocalGroups, NGlobalGroup, NTab, i: integer;
  bGroupActive: boolean;
begin
  NColorTabActive:= GetAppColor(apclTabActive);
  NColorTabOther:= GetAppColor(apclTabActiveOthers);

  NColorMarkActive:= GetAppColor(apclTabActiveMark);
  NColorMarkOther:= ColorBlendHalf(NColorMarkActive, NColorTabOther);

  GetFrameLocation(F, Gr, Pages, NLocalGroups, NGlobalGroup, NTab);

  for i:= 0 to cAppMaxGroup do
  begin
    Pages:= TGroupsHelper.GetPagesOfGroupIndex(i);
    if Pages=nil then Continue;
    with Pages do
    begin
      bGroupActive:= i=NLocalGroups;
      Tabs.ColorTabActive:= IfThen(bGroupActive, NColorTabActive, NColorTabOther);
      Tabs.ColorActiveMark:= IfThen(bGroupActive, NColorMarkActive, NColorMarkOther);
      Tabs.Invalidate;
    end;
  end;
end;

procedure TfmMain.DoClearSingleFirstTab;
var
  D: TATTabData;
  Frame: TEditorFrame;
begin
  D:= Groups.Pages1.Tabs.GetTabData(0);
  if not Assigned(D) then
    raise Exception.Create('Cannot get first tab');

  Frame:= (D.TabObject as TEditorFrame);
  if Frame.Editor.Modified then
    case MsgBox(
         Format(msgConfirmSaveModifiedTab, [Frame.TabCaption]),
         MB_YESNOCANCEL or MB_ICONQUESTION
         ) of
      ID_YES:
        begin
          if Frame.DoFileSave(false, true) then
            Frame.DoFileClose;
        end;
      ID_NO:
        begin
          Frame.DoFileClose;
        end;
      ID_CANCEL:
        exit;
    end;
end;


procedure TfmMain.DoOnTabClose(Sender: TObject; ATabIndex: Integer;
  var ACanClose, ACanContinue: boolean);
  //
  function NiceTabTitle(D: TATTabData): string;
  begin
    Result:= SWrapLongString(D.TabCaptionFull, 80, #10);
  end;
  //
var
  D: TATTabData;
  Frame: TEditorFrame;
  Btn, Res: Integer;
  Msg: string;
  bNeedTreeUpd, bSavedOk: boolean;
  bModified: boolean;
  Adapter: TATAdapterEControl;
begin
  D:= (Sender as TATTabs).GetTabData(ATabIndex);
  if D=nil then exit;
  Frame:= (D.TabObject as TEditorFrame);
  bNeedTreeUpd:= Frame.Visible;
  bSavedOk:= true;

  DoTooltipHide;

  if Frame.Editor.IsLocked then
  begin
    ACanClose:= false;
    Exit
  end;

  while Frame.IsParsingBusy do
  begin
    Sleep(50);
    Application.ProcessMessages;
  end;
  {
  if Frame.IsParsingBusy then
  begin
    ACanClose:= false;
    Exit
  end;
  }

  if Frame.IsEmpty then
    if FrameCount=1 then
    begin
      ACanClose:= false;
      Exit
    end;

  if not Application.Terminated then
  begin
    if DoPyEvent(Frame.Editor, cEventOnCloseBefore, []).Val = evrFalse then
    begin
      ACanClose:= false;
      Exit
    end;
  end;

  bModified:= Frame.Modified;

  if Application.Terminated then
    Res:= ID_OK
  else
  if Frame.TabPinned and not AppSessionIsClosing then
  begin
    //need to activate tab, before msgbox
    (Sender as TATTabs).TabIndex:= ATabIndex;

    Btn:= MB_OKCANCEL;
    Msg:= Format(msgConfirmClosePinnedTab, [NiceTabTitle(D)]);
    Res:= MsgBox(Msg, Btn or MB_ICONWARNING);

    //second confirm for pinned+modified tab
    if Res=ID_OK then
      if bModified then
      begin
        Btn:= MB_OKCANCEL;
        Msg:= Format(msgConfirmSaveModifiedTab, [NiceTabTitle(D)]);
        if MsgBox(Msg, Btn or MB_ICONQUESTION)=ID_OK then
          bSavedOk:= Frame.DoFileSave(false, true);
      end;
  end
  else
  if bModified then
  begin
    //need to activate tab, before msgbox
    (Sender as TATTabs).TabIndex:= ATabIndex;

    if ACanContinue then
      Btn:= MB_YESNOCANCEL
    else
      Btn:= MB_YESNO;
    Msg:= Format(msgConfirmSaveModifiedTab, [NiceTabTitle(D)]);
    Res:= MsgBox(Msg, Btn or MB_ICONQUESTION);
    if (Res=ID_OK) or (Res=ID_YES) then
      bSavedOk:= Frame.DoFileSave(false, true);
  end
  else
    Res:= ID_OK;

  ACanClose:= (Res<>ID_CANCEL) and bSavedOk;
  ACanContinue:= (Res<>ID_CANCEL);

  if ACanClose then
  begin
    Frame.Hide;
    AppFrameList1.Remove(Frame);
    AppFrameListDeleting.Add(Frame);

    UpdateMenuRecent(Frame.Ed1);
    if not Frame.EditorsLinked then
      UpdateMenuRecent(Frame.Ed2);

    Frame.Ed1.AdapterForHilite:= nil;
    Frame.Ed2.AdapterForHilite:= nil;

    Adapter:= Frame.Adapter[Frame.Ed1];
    Adapter.StopTreeUpdate;
    Adapter.Stop;

    if not Frame.EditorsLinked then
    begin
      Adapter:= Frame.Adapter[Frame.Ed2];
      if Assigned(Adapter) then
      begin
        Adapter.StopTreeUpdate;
        Adapter.Stop;
      end;
    end;

    if not Application.Terminated then
      if bNeedTreeUpd then
        DoCodetree_Clear;

    FNeedUpdateStatuses:= true;
  end;
end;

procedure TfmMain.DoOnTabPopup(Sender: TObject; APages: TATPages; ATabIndex: integer);
var
  Data: TATTabData;
  Frame: TEditorFrame;
  i: integer;
begin
  if Assigned(APages) and (ATabIndex>=0) then
  begin
    //fix for #4708
    //plugin Differ expects updated tab-titles in on_tab_menu, and titles are made
    //by plugin 'Suggest Untitled Filenames' in on_change_slow
    for i:= 0 to FrameCount-1 do
    begin
      Frame:= Frames[i];
      if (Frame.Ed1.FileName='') and Frame.Ed1.Modified then
        DoPyEvent(Frame.Ed1, cEventOnChangeSlow, []);
    end;

    InitPopupTab;

    Data:= APages.Tabs.GetTabData(ATabIndex);
    if Assigned(Data) and Assigned(Data.TabObject) then
    begin
      Frame:= Data.TabObject as TEditorFrame;
      if DoPyEvent(Frame.Ed1, cEventOnTabMenu, []).Val=evrFalse then
        exit;
    end;

    PopupTab.Popup;
  end;
end;

procedure TfmMain.mnuTabCloseAllAllClick(Sender: TObject);
begin
  //GroupsCtx.CloseTabs(tabCloseAll, false); //old way in CudaText 1.178
  DoCloseAllTabs(false);
end;

procedure TfmMain.mnuTabCloseAllSameClick(Sender: TObject);
begin
  AppClosingTabs:= true;
  GroupsCtx.CloseTabs(tabCloseAllThisPage, true, false);
  AppClosingTabs:= false;
end;

procedure TfmMain.mnuTabCloseLeftClick(Sender: TObject);
begin
  AppClosingTabs:= true;
  GroupsCtx.CloseTabs(tabCloseLefterThisPage, true, false);
  AppClosingTabs:= false;
end;

procedure TfmMain.mnuTabCloseOtherAllClick(Sender: TObject);
begin
  AppClosingTabs:= true;
  GroupsCtx.CloseTabs(tabCloseOthersAllPages, true, false);
  AppClosingTabs:= false;
end;

procedure TfmMain.mnuTabCloseOtherSameClick(Sender: TObject);
begin
  AppClosingTabs:= true;
  GroupsCtx.CloseTabs(tabCloseOthersThisPage, true, false);
  AppClosingTabs:= false;
end;

procedure TfmMain.mnuTabCloseRightClick(Sender: TObject);
begin
  AppClosingTabs:= true;
  GroupsCtx.CloseTabs(tabCloseRighterThisPage, true, false);
  AppClosingTabs:= false;
end;

procedure TfmMain.mnuTabCloseThisClick(Sender: TObject);
begin
  GroupsCtx.PopupPages.Tabs.DeleteTab(GroupsCtx.PopupTabIndex, true, true);
end;

procedure TfmMain.DoMoveTabToGroup(AGroupIndex: Integer;
  AFromCommandPalette: boolean);
var
  Pages, PagesTo: TATPages;
  NTabIndex: integer;
  //Form: TForm;
begin
  if Assigned(GroupsCtx) and not AFromCommandPalette then
  begin
    Pages:= GroupsCtx.PopupPages;
    NTabIndex:= GroupsCtx.PopupTabIndex;
  end
  else
  begin
    Pages:= CurrentGroups.PagesCurrent;
    NTabIndex:= Pages.Tabs.TabIndex;
  end;

  //force 2 groups if only one
  if (AGroupIndex=1) and (Groups.Mode=gmOne) then
  begin
    UpdateGroupsMode(gm2v);
    Groups.Update; //maybe no need
  end;

  case AGroupIndex of
    0..High(TATGroupsNums):
      begin
        //Form:= nil;
        PagesTo:= Groups.Pages[AGroupIndex];
      end;
    6:
      begin
        //Form:= FFormFloatGroups1;
        ShowFloatGroup1:= true;
        PagesTo:= GroupsF1.Pages1;
      end;
    7:
      begin
        //Form:= FFormFloatGroups2;
        ShowFloatGroup2:= true;
        PagesTo:= GroupsF2.Pages1;
      end;
    8:
      begin
        //Form:= FFormFloatGroups3;
        ShowFloatGroup3:= true;
        PagesTo:= GroupsF3.Pages1;
      end;
    else
      exit;
  end;

  GroupsCtx.MoveTab(Pages, NTabIndex, PagesTo, -1, true);
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
  F.DoFileSave(true, false);
end;

procedure TfmMain.mnuTabSaveClick(Sender: TObject);
var
  F: TEditorFrame;
begin
  F:= FrameOfPopup;
  if F=nil then exit;
  F.DoFileSave(false, false);
end;

procedure TfmMain.mnuTabsizeSpaceClick(Sender: TObject);
begin
  UpdateEditorTabsize(-3);
end;

(*
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
*)

procedure TfmMain.DoOnTabMove(Sender: TObject; NFrom, NTo: Integer);
begin
  //tab closed: set flag
  if NTo=-1 then
    PyEditorMaybeDeleted:= true;

  DoPyEvent(CurrentEditor, cEventOnTabMove, []);
end;


type
  { TAppFrameX }

  TAppFrameX = class
  public
    Frame: TEditorFrame;
    FileName: string;
    Level: integer;
    function GetKey: string;
  end;

{ TAppFramePropX }

function TAppFrameX.GetKey: string;
begin
  Result:= AppGetLeveledPath(FileName, Level);
end;

procedure TfmMain.UpdateTabCaptionsFromFolders;
var
  PagesArray: array[0..9] of TATPages; //6 main groups + 3 floating groups
  PagesCount: integer;
  //
  procedure AddPagesToArray(Obj: TATPages);
  var
    i: integer;
  begin
    for i:= 0 to PagesCount-1 do
      if PagesArray[i]=Obj then exit;
    if PagesCount<Length(PagesArray) then
    begin
      Inc(PagesCount);
      PagesArray[PagesCount-1]:= Obj;
    end;
  end;
  //
var
  SName: string;
  Frame: TEditorFrame;
  L: TFPList;
  Prop, Prop2: TAppFrameX;
  bFoundPair: boolean;
  iLevel, i, j: integer;
begin
  if not UiOps.TabsShowFoldersSuffix then exit;

  FillChar(PagesArray, SizeOf(PagesArray), 0);
  PagesCount:= 0;

  L:= TFPList.Create;
  try
    for i:= 0 to FrameCount-1 do
    begin
      Frame:= Frames[i];
      if Frame.TabCaptionReason=tcrFromPlugin then Continue; //solve issue #3360
      SName:= Frame.FileName;
      if (SName<>'') and Frame.EditorsLinked then
      begin
        Prop:= TAppFrameX.Create;
        Prop.Frame:= Frame;
        Prop.FileName:= SName;
        L.Add(Prop);
      end;
    end;

    for iLevel:= 0 to UiOps.TabsShowFoldersMaxLevels do
      for i:= 0 to L.Count-2 do
      begin
        Prop:= TAppFrameX(L[i]);
        bFoundPair:= false;
        if Prop.Level = iLevel then
          for j:= i+1 to L.Count-1 do
          begin
            Prop2:= TAppFrameX(L[j]);
            if (Prop.Level=Prop2.Level) and SameFileName(Prop.GetKey, Prop2.GetKey) then
            begin
              bFoundPair:= true;
              Inc(Prop2.Level);
            end;
          end;
        if bFoundPair then
          Inc(Prop.Level);
      end;

    for i:= 0 to L.Count-1 do
    begin
      Prop:= TAppFrameX(L[i]);
      if Prop.Level=0 then
        SName:= ''
      else
        SName:= ExtractFileDir(Prop.GetKey);
      if Prop.Frame.TabCaptionAddon<>SName then
      begin
        Prop.Frame.TabCaptionAddon:= SName;
        AddPagesToArray(Prop.Frame.GetTabPages);
      end;
    end;
  finally
    for i:= L.Count-1 downto 0 do
      TObject(L[i]).Free;
    FreeAndNil(L);
  end;

  for i:= 0 to PagesCount-1 do
    PagesArray[i].Invalidate;

  //do MakeVisible for tabs, because appending folder suffixes make titles wider
  //(and current righter tab may shift away).
  // https://github.com/Alexey-T/CudaText/issues/4653#issuecomment-1344280566
  Application.ProcessMessages;
  for i:= 0 to High(Groups.Pages) do
    if Groups.Pages[i].Tabs.TabCount>0 then
      Groups.Pages[i].Tabs.MakeVisible(Groups.Pages[i].Tabs.TabIndex);
end;

