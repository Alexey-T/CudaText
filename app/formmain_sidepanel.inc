{$ifdef nn}begin end;{$endif}

procedure TfmMain.DoSidebar_ListboxDrawItem(Sender: TObject; C: TCanvas;
  AIndex: integer; const ARect: TRect);
const
  cDx=4; cDy=1;
var
  Listbox: TATListboxMy;
begin
  if AIndex<0 then exit;
  Listbox:= Sender as TATListboxMy;

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
  NTab, i: integer;
  SCaption: string;
begin
  NTab:= TabsLeft.TabIndex;
  if NTab<0 then exit;
  SCaption:= TabsLeft.GetTabData(NTab).TabCaption;
  for i:= Low(FAppSidePanels) to High(FAppSidePanels) do
    with FAppSidePanels[i] do
    begin
      if ItemCaption='' then Break;
      if Assigned(ItemTreeview) then
      begin
        ItemTreeview.Visible:= ItemCaption=SCaption;
        if PanelLeft.Visible then
          if ItemTreeview.Visible then
            if ItemTreeview.CanFocus then
              ItemTreeview.SetFocus;
      end
      else
      if Assigned(ItemListbox) then
      begin
        ItemListbox.Visible:= ItemCaption=SCaption;
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
  Data: TATTabData;
  i: integer;
begin
  Result:= -1;
  for i:= 0 to TabsLeft.TabCount-1 do
  begin
    Data:= TabsLeft.GetTabData(i);
    if Assigned(Data) then
      if Data.TabCaption=Str then exit(i)
  end;
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
    ItemListbox:= TATListboxMy.Create(Self);

    ItemListbox.Hide;
    ItemListbox.Parent:= AParent;
    ItemListbox.Align:= alClient;
    ItemListbox.PopupMenu:= ItemMenu;

    ItemListbox.OnDrawItem:= @DoSidebar_ListboxDrawItem;
    ItemListbox.Color:= GetAppColor('ListBg');
    ItemListbox.ItemHeight:= ListboxOut.ItemHeight;
    ItemListbox.CanGetFocus:= true;

    {
    ItemListbox.OnClick:= @DoPanel_OnClick;
    ItemListbox.OnDblClick:= @DoPanel_OnDblClick;
    ItemListbox.OnSelectionChanged:= @DoPanel_OnSelChanged;
    ItemListbox.OnContextPopup:=@DoPanel_OnContextPopup;
    ItemListbox.OnKeyDown:= @TreeKeyDown;
    }
  end;
end;


function TfmMain.DoSidebar_AddTab(const ACaption, AControlType: string; ATabIndex: integer): boolean;
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

  TabsLeft.AddTab(ATabIndex, ACaption);
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
    fmMain.TabsLeft.DeleteTab(Num, false, false);

    //hard to remov array item, so hide it by "?"
    for i:= Low(FAppSidePanels) to High(FAppSidePanels) do
      if FAppSidePanels[i].ItemCaption=ACaption then
      begin
        FAppSidePanels[i].ItemCaption:= '?';
        Break;
      end;

    if (Num>0) and (fmMain.TabsLeft.TabIndex=Num) then
      fmMain.TabsLeft.TabIndex:= Num-1;
  end;
end;

function TfmMain.DoSidebar_ActivateTab(const ACaption: string): boolean;
var
  Num: integer;
begin
  Num:= DoSidebar_CaptionToTabIndex(ACaption);
  Result:= Num>=0;
  if Result then
    fmMain.TabsLeft.TabIndex:= Num;
end;


