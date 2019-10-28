(*
This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at http://mozilla.org/MPL/2.0/.

Copyright (c) Alexey Torgashin
*)
{$ifdef nn}begin end;{$endif}

procedure TfmMain.DoSidebar_ListboxDrawItem(Sender: TObject; C: TCanvas;
  AIndex: integer; const ARect: TRect);
const
  cDx=4; cDy=1;
var
  Listbox: ATListbox.TATListbox;
begin
  if AIndex<0 then exit;
  Listbox:= Sender as ATListbox.TATListbox;

  C.Font.Color:= GetAppColor('ListFont');
  C.Brush.Color:= GetAppColor('ListBg');

  if AIndex=Listbox.ItemIndex then
  begin
    C.Font.Color:= GetAppColor('ListSelFont');
    C.Brush.Color:= GetAppColor('ListSelBg');
    C.FillRect(ARect);
  end;

  C.TextOut(ARect.Left+cDx, ARect.Top+cDy, Listbox.Items[AIndex]);
end;

procedure TfmMain.DoSidebar_MainMenuClick(Sender: TObject);
var
  Pnt: TPoint;
begin
  if not Assigned(FMenuCopy) then
    FMenuCopy:= TPopupMenu.Create(Self)
  else
    FMenuCopy.Items.Clear;

  Menu_Copy(MainMenu, FMenuCopy);
  Pnt:= ClientToScreen(Point(0, 0));
  FMenuCopy.Popup(Pnt.X, Pnt.Y);
end;


procedure TfmMain.DoSidebar_OnTabClick(Sender: TObject);
var
  Btn: TATButton;
  SCaption, S, SModule, SMethod: string;
  i: integer;
begin
  //toggle side panel?
  Btn:= (Sender as TATButton);
  if Btn.Checked then
  begin
    Btn.Checked:= false;
    ShowSidePanel:= false;
    Exit
  end;

  SCaption:= '';
  for i:= 0 to ToolbarSideTop.ButtonCount-1 do
  begin
    Btn:= ToolbarSideTop.Buttons[i];
    Btn.Checked:= Btn=Sender;
    if Btn.Checked then
    begin
      SCaption:= Btn.Caption;
      //button has no form? detect by DataString.
      if Btn.DataString<>'' then
      begin
        S:= Btn.DataString;
        SModule:= SGetItem(S, '.');
        SMethod:= S;
        //MsgLogConsole(Format('sidebar click: caption "%s", py %s.%s',
        //  [SCaption, SModule, SMethod]));
        DoPyCommand(SModule, SMethod, []);
      end;
    end;
  end;

  if SCaption<>'' then
  begin
    //MsgLogConsole(Format('sidebar click: caption "%s"', [SCaption]));
    UpdateSidebarPanels(SCaption, true);
  end
  else
    ShowSidePanel:= false;
end;


function TfmMain.DoSidebar_TranslatedCaption(const ACaption: string): string;
begin
  case ACaption of
    msgPanelTree_Init:
      Result:= msgPanelTree;
    msgPanelProject_Init:
      Result:= msgPanelProject;
    msgPanelTabs_Init:
      Result:= msgPanelTabs;
    else
      Result:= ACaption;
  end;
end;

procedure TfmMain.UpdateSidebarPanels(const ACaption: string; AndFocus: boolean);
var
  bFound: boolean;
  i: integer;
begin
  FLastSidebarPanel:= ACaption;
  ShowSidePanel:= true;
  LabelSideTitle.Caption:= DoSidebar_TranslatedCaption(ACaption);
  if FloatSide then
    FFormFloatSide.Caption:= LabelSideTitle.Caption + ' - ' + msgTitle;

  for i:= 0 to AppSidePanels.Count-1 do
    with TAppSidePanel(AppSidePanels[i]) do
    begin
      if ItemCaption='' then Break;
      bFound:= SameText(ItemCaption, ACaption);
      if Assigned(ItemControl) then
      begin
        ItemControl.Visible:= bFound;
        if AndFocus then
          if PanelLeft.Visible then
            if ItemControl.Visible then
              if ItemControl.CanFocus then
                ItemControl.SetFocus;
      end
      else
      if bFound and (ItemModule<>'') and (ItemMethod<>'') then
        DoPyCommand(ItemModule, ItemMethod, []);
    end;

  //codetree filter has different parent, so adjust visible too
  PanelCodeTreeTop.Visible:= CodeTree.Visible and (UiOps.TreeFilterLayout>0);
end;

function TfmMain.DoSidebar_CaptionToPanelsIndex(const ACaption: string): integer;
var
  i: integer;
begin
  Result:= -1;
  for i:= 0 to AppSidePanels.Count-1 do
    with TAppSidePanel(AppSidePanels[i]) do
      if ItemCaption=ACaption then
        exit(i);
end;

function TfmMain.DoSidebar_CaptionToControlHandle(const ACaption: string): PtrInt;
var
  Num: integer;
begin
  Result:= 0;
  Num:= DoSidebar_CaptionToPanelsIndex(ACaption);
  if Num<0 then exit;

  with TAppSidePanel(AppSidePanels[Num]) do
    if Assigned(ItemControl) then
      Result:= PtrInt(ItemControl);
end;

function TfmMain.DoSidebar_CaptionToTabIndex(const Str: string): integer;
var
  i: integer;
begin
  Result:= -1;
  for i:= 0 to ToolbarSideTop.ButtonCount-1 do
    if SameText(ToolbarSideTop.Buttons[i].Caption, Str) then
      Exit(i);
end;


procedure TfmMain.DoSidebar_InitPanelForm(AItem: TAppSidePanel;
  const ACaption: string; AForm: TCustomForm; AParent: TWinControl);
begin
  AItem.ItemCaption:= ACaption;
  AItem.ItemControl:= AForm;
  AItem.ItemModule:= '';
  AItem.ItemMethod:= '';

  AForm.BorderStyle:= bsNone;
  AForm.Parent:= AParent;
  AForm.Align:= alClient;
end;


function TfmMain.DoSidebar_FilenameToImageIndex(ATabCaption, AFilename: string): integer;
begin
  if AFilename='' then
    AFilename:= LowerCase(ATabCaption)+'.png';
  if ExtractFileDir(AFilename)='' then
    AFilename:= AppDir_DataSidebarIcons+DirectorySeparator+UiOps.SidebarTheme+DirectorySeparator+AFilename;

  Result:= UpdateImagelistWithIconFromFile(ImageListSide, AFilename);
end;


function TfmMain.DoSidebar_AddTab(const ACaption: string; AImageIndex: integer;
  AHandle: PtrInt): boolean;
var
  Panel: TAppSidePanel;
  Num: integer;
  bExist: boolean;
begin
  Num:= DoSidebar_CaptionToPanelsIndex(ACaption);
  bExist:= Num>=0;

  if bExist then
    Panel:= TAppSidePanel(AppSidePanels[Num])
  else
  begin
    Panel:= TAppSidePanel.Create;
    AppSidePanels.Add(Panel);
  end;

  if AHandle<>0 then
    DoSidebar_InitPanelForm(Panel, ACaption, TCustomForm(AHandle), PanelLeft);

  if not bExist then
  begin
    ToolbarSideTop.AddButton(AImageIndex, @DoSidebar_OnTabClick, ACaption, ACaption, '', UiOps.ShowSidebarCaptions);
    ToolbarSideTop.UpdateControls;
  end;

  Result:= true;
end;

function TfmMain.DoSidebar_AddTab_Empty(const ACaption: string; AImageIndex: integer;
  const AModule, AMethod: string): boolean;
var
  Panel: TAppSidePanel;
begin
  if DoSidebar_CaptionToPanelsIndex(ACaption)>=0 then exit(false);

  Panel:= TAppSidePanel.Create;
  Panel.ItemCaption:= ACaption;
  Panel.ItemControl:= nil;
  Panel.ItemModule:= AModule;
  Panel.ItemMethod:= AMethod;
  AppSidePanels.Add(Panel);

  //save module/method to Btn.DataString
  ToolbarSideTop.AddButton(AImageIndex, @DoSidebar_OnTabClick, ACaption, ACaption,
    AModule+'.'+AMethod,
    UiOps.ShowSidebarCaptions);
  ToolbarSideTop.UpdateControls;

  Result:= true;
end;


function TfmMain.DoSidebar_RemoveTab(const ACaption: string): boolean;
var
  Num, i: integer;
begin
  Num:= DoSidebar_CaptionToTabIndex(ACaption);
  Result:= Num>=0;
  if Result then
  begin
    ToolbarSideTop.Buttons[Num].Free;
    ToolbarSideTop.UpdateControls;

    //hard to remove item, so hide it by "?"
    for i:= 0 to AppSidePanels.Count-1 do
      with TAppSidePanel(AppSidePanels[i]) do
        if ItemCaption=ACaption then
        begin
          ItemCaption:= '?';
          Break;
        end;
  end;
end;

function TfmMain.DoSidebar_ActivateTab(const ACaption: string; AndFocus: boolean): boolean;
begin
  Result:= DoSidebar_CaptionToTabIndex(ACaption)>=0;
  if Result then
    UpdateSidebarPanels(ACaption, AndFocus);
end;

procedure TfmMain.UpdateSidebarButtons;
var
  Btn: TATButton;
  i: integer;
begin
  for i:= 0 to ToolbarSideTop.ButtonCount-1 do
  begin
    Btn:= ToolbarSideTop.Buttons[i];
    Btn.Checked:= SameText(Btn.Caption, FLastSidebarPanel) and ShowSidePanel;
  end;
end;


procedure TfmMain.DoSidebar_FocusCodetreeFilter;
begin
  DoSidebar_ActivateTab(msgPanelTree_Init, false);
  if CodeTree.Visible and
    CodeTreeFilterInput.Visible and
    CodeTreeFilterInput.Enabled then
  begin
    if FloatSide then
    begin
      if FFormFloatSide.Visible then
      begin
        FFormFloatSide.SetFocus;
        FFormFloatSide.ActiveControl:= CodeTreeFilterInput;
      end;
    end
    else
      ActiveControl:= CodeTreeFilterInput;
  end;
end;

procedure TfmMain.DoSidebar_FocusCodetree;
begin
  DoSidebar_ActivateTab(msgPanelTree_Init, false);
  if CodeTree.Visible and
    CodeTree.Enabled then
  begin
    if FloatSide then
    begin
      if FFormFloatSide.Visible then
      begin
        FFormFloatSide.SetFocus;
        FFormFloatSide.ActiveControl:= CodeTree.Tree;
      end;
    end
    else
      ActiveControl:= CodeTree.Tree;
  end;
end;


procedure TfmMain.CodeTreeFilter_OnCommand(Sender: TObject; ACmd: integer;
  const AText: string; var AHandled: boolean);
var
  Ed: TATComboEdit;
  F: TEditorFrame;
begin
  Ed:= CodeTreeFilterInput;

  if ACmd=cCommand_KeyEnter then
  begin
    Ed.DoAddLineToHistory(Ed.Text, UiOps.MaxHistoryEdits);

    F:= CurrentFrame;
    if Assigned(F) then
      F.CodetreeFilterHistory.Assign(Ed.Items);

    AHandled:= true;
  end;
end;

