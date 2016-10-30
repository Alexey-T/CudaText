{$ifdef nn}begin end;{$endif}

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
      if ItemCaption='' then break;
      if Assigned(ItemTreeview) then
      begin
        ItemTreeview.Visible:= ItemCaption=SCaption;
        if PanelLeft.Visible and ItemTreeview.Visible then
          if ItemTreeview.CanFocus then
            ItemTreeview.SetFocus;
      end;
      if Assigned(ItemListbox) then
      begin
        ItemListbox.Visible:= ItemCaption=SCaption;
        if ItemListbox.Visible then ItemListbox.SetFocus;
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
  Num:= DoSidebar_CaptionToPanelsIndex(ACaption);
  if Num<0 then
    Result:= 0
  else
    Result:= PtrInt(FAppSidePanels[Num].ItemTreeview)
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

function TfmMain.DoSidebar_AddTab(const ACaption, AControlType: string; ATabIndex: integer): boolean;
const
  cTypeTree = 'tree';
var
  Num: integer;
begin
  if DoSidebar_CaptionToPanelsIndex(ACaption)>=0 then exit(false);
  if (AControlType<>cTypeTree) then exit(false);

  Num:= 0;
  while FAppSidePanels[Num].ItemCaption<>'' do Inc(Num);
  if Num>High(FAppSidePanels) then exit(false);

  DoSidebar_InitPanelTreeview(FAppSidePanels[Num], ACaption, PanelLeft);
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


