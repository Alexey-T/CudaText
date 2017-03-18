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
  Listbox: TATListbox;
begin
  if AIndex<0 then exit;
  Listbox:= Sender as TATListbox;

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


procedure TfmMain.DoSidebar_OnTabClick(Sender: TObject);
var
  Btn: TATButton;
  SCaption: string;
  i: integer;
begin
  //toggle side panel?
  Btn:= (Sender as TATButton);
  if Btn.Checked then
  begin
    Btn.Checked:= false;
    CurrentEditor.DoCommand(cmd_HideSidePanel);
    Exit
  end;

  SCaption:= '';
  for i:= 0 to ToolbarSide.ButtonCount-1 do
  begin
    Btn:= ToolbarSide.Buttons[i];
    Btn.Checked:= Btn=Sender;
    if Btn.Checked then
    begin
      SCaption:= Btn.Caption;
    end;
  end;

  if SCaption<>'' then
    UpdateSidebarPanels(SCaption)
  else
    CurrentEditor.DoCommand(cmd_HideSidePanel);
end;

procedure TfmMain.UpdateSidebarPanels(const ACaption: string);
var
  i: integer;
begin
  FLastSidebarPanel:= ACaption;
  PanelLeftTitle.Caption:= ACaption;
  CurrentEditor.DoCommand(cmd_ShowSidePanelAsIs);

  for i:= Low(FAppSidePanels) to High(FAppSidePanels) do
    with FAppSidePanels[i] do
    begin
      if ItemCaption='' then Break;
      if Assigned(ItemTreeview) then
      begin
        ItemTreeview.Visible:= ItemCaption=ACaption;
        if PanelLeft.Visible then
          if ItemTreeview.Visible then
            if ItemTreeview.CanFocus then
              ItemTreeview.SetFocus;
      end
      else
      if Assigned(ItemListbox) then
      begin
        ItemListbox.Visible:= ItemCaption=ACaption;
        if PanelLeft.Visible then
          if ItemListbox.Visible then
            if ItemListbox.CanFocus then
              ItemListbox.SetFocus;
      end;
    end;
end;

function TfmMain.DoSidebar_CaptionToPanelsIndex(const Str: string): integer;
var
  i: integer;
begin
  Result:= -1;
  for i:= Low(FAppSidePanels) to High(FAppSidePanels) do
    with FAppSidePanels[i] do
    begin
      if ItemCaption='' then break;
      if ItemCaption=Str then exit(i);
    end;
end;

function TfmMain.DoSidebar_CaptionToControlHandle(const ACaption: string): PtrInt;
var
  Num: integer;
begin
  Result:= 0;
  Num:= DoSidebar_CaptionToPanelsIndex(ACaption);
  if Num<0 then exit;

  with FAppSidePanels[Num] do
  begin
    if Assigned(ItemTreeview) then
      Result:= PtrInt(ItemTreeview)
    else
    if Assigned(ItemListbox) then
      Result:= PtrInt(ItemListbox);
  end;
end;

function TfmMain.DoSidebar_CaptionToTabIndex(const Str: string): integer;
var
  i: integer;
begin
  Result:= -1;
  for i:= 0 to ToolbarSide.ButtonCount-1 do
    if SameText(ToolbarSide.Buttons[i].Caption, Str) then
      Exit(i);
end;

procedure TfmMain.DoSidebar_InitPanelTreeview(var AItem: TAppSidePanel;
  const ACaption: string; AParent: TWinControl);
begin
  with AItem do
  begin
    ItemCaption:= ACaption;
    ItemMenu:= TPopupMenu.Create(Self);
    ItemImagelist:= TImageList.Create(Self);
    ItemTreeview:= TTreeView.Create(Self);

    ItemTreeview.Hide;
    ItemTreeview.Parent:= AParent;
    ItemTreeview.Align:= alClient;
    ItemTreeview.Images:= ItemImagelist;
    ItemTreeview.PopupMenu:= ItemMenu;
    ItemTreeview.OnClick:= @DoPanel_OnClick;
    ItemTreeview.OnDblClick:= @DoPanel_OnDblClick;
    ItemTreeview.OnSelectionChanged:= @DoPanel_OnSelChanged;
    ItemTreeview.OnContextPopup:=@DoPanel_OnContextPopup;
    ItemTreeview.OnKeyDown:= @TreeKeyDown;

    DoApplyThemeToTreeview(ItemTreeview);
    //just to see it ok
    ItemTreeview.Items.Add(nil, 'panel '+ACaption);
  end;
end;


procedure TfmMain.DoSidebar_InitPanelListbox(var AItem: TAppSidePanel;
  const ACaption: string; AParent: TWinControl);
begin
  with AItem do
  begin
    ItemCaption:= ACaption;
    ItemMenu:= TPopupMenu.Create(Self);
    ItemImagelist:= nil;
    ItemTreeview:= nil;
    ItemListbox:= TATListbox.Create(Self);

    ItemListbox.Hide;
    ItemListbox.Parent:= AParent;
    ItemListbox.Align:= alClient;
    ItemListbox.PopupMenu:= ItemMenu;

    ItemListbox.OnDrawItem:= @DoSidebar_ListboxDrawItem;
    ItemListbox.ItemHeight:= ListboxOut.ItemHeight;
    ItemListbox.CanGetFocus:= true;

    ItemListbox.OnClick:= @DoPanel_OnClick;
    ItemListbox.OnDblClick:= @DoPanel_OnDblClick;
    ItemListbox.OnChangedSel:= @DoPanel_OnSelChanged;
    ItemListbox.OnContextPopup:=@DoPanel_OnContextPopup;

    DoApplyThemeToListbox(ItemListbox);
  end;
end;


function TfmMain.DoSidebar_AddTab(const ACaption, AControlType: string;
  ATabIndex, AImageIndex: integer): boolean;
var
  Num: integer;
begin
  if DoSidebar_CaptionToPanelsIndex(ACaption)>=0 then exit(false);

  Num:= 0;
  while FAppSidePanels[Num].ItemCaption<>'' do Inc(Num);
  if Num>High(FAppSidePanels) then exit(false);

  if AControlType=PyPanelControl_Tree then
    DoSidebar_InitPanelTreeview(FAppSidePanels[Num], ACaption, PanelLeft)
  else
  if AControlType=PyPanelControl_Listbox then
    DoSidebar_InitPanelListbox(FAppSidePanels[Num], ACaption, PanelLeft)
  else
    exit(false);

  ToolbarSide.AddButton(AImageIndex, @DoSidebar_OnTabClick, ACaption, ACaption, '', true);
  ToolbarSide.UpdateControls;
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
    ToolbarSide.Buttons[Num].Free;
    ToolbarSide.UpdateControls;

    //hard to remov array item, so hide it by "?"
    for i:= Low(FAppSidePanels) to High(FAppSidePanels) do
      if FAppSidePanels[i].ItemCaption=ACaption then
      begin
        FAppSidePanels[i].ItemCaption:= '?';
        Break;
      end;
  end;
end;

function TfmMain.DoSidebar_ActivateTab(const ACaption: string): boolean;
begin
  Result:= DoSidebar_CaptionToTabIndex(ACaption)>=0;
  if Result then
    UpdateSidebarPanels(ACaption);
end;

procedure TfmMain.UpdateSidebarButtons;
var
  Btn: TATButton;
  i: integer;
begin
  for i:= 0 to ToolbarSide.ButtonCount-1 do
  begin
    Btn:= ToolbarSide.Buttons[i];
    Btn.Checked:= SameText(Btn.Caption, FLastSidebarPanel) and PanelLeft.Visible;
  end;
end;


