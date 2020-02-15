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
  if not Assigned(PopupSidebarClone) then
    PopupSidebarClone:= TPopupMenu.Create(Self)
  else
    PopupSidebarClone.Items.Clear;

  //fill submenu "Recent files"
  MenuRecentsPopup(nil);

  Menu_Copy(MainMenu, PopupSidebarClone);
  Pnt:= ClientToScreen(Point(0, 0));
  PopupSidebarClone.Popup(Pnt.X, Pnt.Y);
end;


procedure TfmMain.DoSidebar_OnTabClick(Sender: TObject);
var
  Btn: TATButton;
  SCaption, SModule, SMethod: string;
  NPanelIndex: integer;
begin
  Btn:= Sender as TATButton;
  SCaption:= Btn.Caption;

  if Btn.Checked or (SCaption='') then
  begin
    Btn.Checked:= false;
    ShowSidePanel:= false;
    Exit
  end;

  //avoid plugin call if panel already inited
  NPanelIndex:= DoSidebar_CaptionToPanelsIndex(cSideLeft, SCaption);
  if (NPanelIndex>=0) and (TAppSidePanel(AppPanels[cSideLeft].Panels[NPanelIndex]).ItemControl=nil) then
    if Btn.DataString<>'' then
    begin
      SSplitByChar(Btn.DataString, '.', SModule, SMethod);
      //MsgLogConsole(Format('sidebar click: caption "%s", py %s.%s',
      //  [SCaption, SModule, SMethod]));
      DoPyCommand(SModule, SMethod, []);
    end;

  UpdateSidebarPanels(SCaption, true);
  Btn.Checked:= true;
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
    msgPanelSnippet_Init:
      Result:= msgPanelSnippet;
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

  for i:= 0 to AppPanels[cSideLeft].Panels.Count-1 do
    with TAppSidePanel(AppPanels[cSideLeft].Panels[i]) do
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

function TfmMain.DoSidebar_CaptionToPanelsIndex(ASide: TAppSideId; const ACaption: string): integer;
var
  i: integer;
begin
  Result:= -1;
  for i:= 0 to AppPanels[ASide].Panels.Count-1 do
    with TAppSidePanel(AppPanels[ASide].Panels[i]) do
      if ItemCaption=ACaption then
        exit(i);
end;

function TfmMain.DoSidebar_CaptionToControlHandle(const ACaption: string): PtrInt;
var
  Num: integer;
begin
  Result:= 0;
  Num:= DoSidebar_CaptionToPanelsIndex(cSideLeft, ACaption);
  if Num<0 then exit;

  with TAppSidePanel(AppPanels[cSideLeft].Panels[Num]) do
    if Assigned(ItemControl) then
      Result:= PtrInt(ItemControl);
end;

function TfmMain.DoSidebar_CaptionToTabIndex(ASide: TAppSideId; const ACaption: string): integer;
var
  Bar: TATFlatToolbar;
  i: integer;
begin
  Result:= -1;
  Bar:= AppPanels[ASide].Toolbar;
  for i:= 0 to Bar.ButtonCount-1 do
    if SameText(Bar.Buttons[i].Caption, ACaption) then
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
  Num:= DoSidebar_CaptionToPanelsIndex(cSideLeft, ACaption);
  bExist:= Num>=0;

  if bExist then
    Panel:= TAppSidePanel(AppPanels[cSideLeft].Panels[Num])
  else
  begin
    Panel:= TAppSidePanel.Create;
    AppPanels[cSideLeft].Panels.Add(Panel);
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
  if DoSidebar_CaptionToPanelsIndex(cSideLeft, ACaption)>=0 then exit(false);

  Panel:= TAppSidePanel.Create;
  Panel.ItemCaption:= ACaption;
  Panel.ItemControl:= nil;
  Panel.ItemModule:= AModule;
  Panel.ItemMethod:= AMethod;
  AppPanels[cSideLeft].Panels.Add(Panel);

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
  Num:= DoSidebar_CaptionToTabIndex(cSideLeft, ACaption);
  Result:= Num>=0;
  if Result then
  begin
    ToolbarSideTop.Buttons[Num].Free;
    ToolbarSideTop.UpdateControls;

    //hard to remove item, so hide it by "?"
    for i:= 0 to AppPanels[cSideLeft].Panels.Count-1 do
      with TAppSidePanel(AppPanels[cSideLeft].Panels[i]) do
        if ItemCaption=ACaption then
        begin
          ItemCaption:= '?';
          Break;
        end;
  end;
end;

function TfmMain.DoSidebar_ActivateTab(const ACaption: string; AndFocus: boolean): boolean;
begin
  Result:= DoSidebar_CaptionToTabIndex(cSideLeft, ACaption)>=0;
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

