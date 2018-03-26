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
        DoPyCommand(SModule, SMethod, []);
        Exit
      end;
    end;
  end;

  if SCaption<>'' then
    UpdateSidebarPanels(SCaption, true)
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

  for i:= Low(AppSidePanels) to High(AppSidePanels) do
    with AppSidePanels[i] do
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
end;

function TfmMain.DoSidebar_CaptionToPanelsIndex(const Str: string): integer;
var
  i: integer;
begin
  Result:= -1;
  for i:= Low(AppSidePanels) to High(AppSidePanels) do
    with AppSidePanels[i] do
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

  with AppSidePanels[Num] do
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


procedure TfmMain.DoSidebar_InitPanelForm(var AItem: TAppSidePanel;
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
    AFilename:= GetAppPath(cDirDataSideIcons)+DirectorySeparator+AFilename;

  Result:= UpdateImagelistWithIconFromFile(ImageListSide, AFilename);
end;


function TfmMain.DoSidebar_AddTab(const ACaption: string; AImageIndex: integer;
  AHandle: PtrInt): boolean;
var
  Num: integer;
  bExist: boolean;
begin
  Num:= DoSidebar_CaptionToPanelsIndex(ACaption);
  bExist:= Num>=0;

  if not bExist then
  begin
    //find free index for new button
    Num:= 0;
    while AppSidePanels[Num].ItemCaption<>'' do Inc(Num);
    if Num>High(AppSidePanels) then exit(false);
  end;

  if AHandle<>0 then
    DoSidebar_InitPanelForm(AppSidePanels[Num], ACaption, TCustomForm(AHandle), PanelLeft);

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
  Num: integer;
begin
  if DoSidebar_CaptionToPanelsIndex(ACaption)>=0 then exit(false);

  //find free index for button
  Num:= 0;
  while AppSidePanels[Num].ItemCaption<>'' do Inc(Num);
  if Num>High(AppSidePanels) then exit(false);

  AppSidePanels[Num].ItemCaption:= ACaption;
  AppSidePanels[Num].ItemControl:= nil;
  AppSidePanels[Num].ItemModule:= AModule;
  AppSidePanels[Num].ItemMethod:= AMethod;

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

    //hard to remov array item, so hide it by "?"
    for i:= Low(AppSidePanels) to High(AppSidePanels) do
      if AppSidePanels[i].ItemCaption=ACaption then
      begin
        AppSidePanels[i].ItemCaption:= '?';
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


