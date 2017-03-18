(*
This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at http://mozilla.org/MPL/2.0/.

Copyright (c) Alexey Torgashin
*)
{$ifdef nn}begin end;{$endif}

function TfmMain.IsFocusedBottom: boolean;
begin
  Result:=
    fmConsole.ed.Focused or
    fmConsole.memo.Focused or
    ListboxOut.Focused or
    ListboxVal.Focused;
end;

procedure TfmMain.DoBottom_OnTabClick(Sender: TObject);
var
  Btn: TATButton;
  SCaption: string;
  i: integer;
begin
  Btn:= Sender as TATButton;
  if Btn.Checked then
  begin
    Btn.Checked:= false;
    ShowBottom:= false;
    Exit
  end;

  SCaption:= '';
  for i:= 0 to ToolbarBtm.ButtonCount-1 do
  begin
    Btn:= ToolbarBtm.Buttons[i];
    Btn.Checked:= Btn=Sender;
    if Btn.Checked then
    begin
      SCaption:= Btn.Caption;
    end;
  end;

  if SCaption<>'' then
    UpdateBottomPanels(SCaption)
  else
    ShowBottom:= false;
end;


procedure TfmMain.UpdateBottomPanels(const ACaption: string);
var
  Ctl: TWinControl;
  i: integer;
begin
  FLastBottomPanel:= ACaption;
  ShowBottom:= true;

  //hide std panels
  fmConsole.Hide;
  ListboxOut.Hide;
  ListboxVal.Hide;

  //hide panels from log_panel_add
  for i:= 0 to FAppBottomPanelsListbox.Count-1 do
  begin
    Ctl:= (FAppBottomPanelsListbox.Objects[i] as TAppPanelPropsClass).Data.Listbox;
    if Assigned(Ctl) then
      Ctl.Hide;
  end;

  //hide panels from proc_bottompanel_add
  for i:= Low(FAppBottomPanels) to High(FAppBottomPanels) do
  begin
    Ctl:= FAppBottomPanels[i].ItemTreeview;
    if Assigned(Ctl) then
      Ctl.Hide;
  end;

  if SameText(ACaption, 'console') then
  begin
    fmConsole.Show;
    fmConsole.Ed.SetFocus;
  end
  else
  if SameText(ACaption, 'output') then
  begin
    ListboxOut.Show;
    ListboxOut.SetFocus;
  end
  else
  if SameText(ACaption, 'validate') then
  begin
    ListboxVal.Show;
    ListboxVal.SetFocus;
  end
  else
  begin
    //tabs for api log_panel_add
    i:= FAppBottomPanelsListbox.IndexOf(ACaption);
    if i>=0 then
    begin
      Ctl:= (FAppBottomPanelsListbox.Objects[i] as TAppPanelPropsClass).Data.Listbox;
      Ctl.Show;
      if PanelBottom.Visible then
        if Ctl.CanFocus then
          Ctl.SetFocus;
    end;

    //tabs for api proc_bottom_panel
    for i:= Low(FAppBottomPanels) to High(FAppBottomPanels) do
      with FAppBottomPanels[i] do
      begin
        if ItemCaption='' then break;
        if Assigned(ItemTreeview) then
        begin
          ItemTreeview.Visible:= SameText(ItemCaption, ACaption);
          if PanelBottom.Visible then
            if ItemTreeview.Visible then
              if ItemTreeview.CanFocus then
                ItemTreeview.SetFocus;
        end
        else
        if Assigned(ItemListbox) then
        begin
          ItemListbox.Visible:= SameText(ItemCaption, ACaption);
          if PanelBottom.Visible then
            if ItemListbox.Visible then
              if ItemListbox.CanFocus then
                ItemListbox.SetFocus;
        end;
      end;
  end;
end;

function TfmMain.DoBottom_CaptionToPanelsIndex(const Str: string): integer;
var
  i: integer;
begin
  Result:= -1;
  for i:= Low(FAppBottomPanels) to High(FAppBottomPanels) do
    with FAppBottomPanels[i] do
    begin
      if ItemCaption='' then break;
      if ItemCaption=Str then exit(i);
    end;
end;

function TfmMain.DoBottom_CaptionToControlHandle(const ACaption: string): PtrInt;
var
  Num: integer;
begin
  Result:= 0;
  Num:= DoBottom_CaptionToPanelsIndex(ACaption);
  if Num<0 then exit;

  with FAppBottomPanels[Num] do
  begin
    if Assigned(ItemTreeview) then
      Result:= PtrInt(ItemTreeview)
    else
    if Assigned(ItemListbox) then
      Result:= PtrInt(ItemListbox);
  end;
end;

function TfmMain.DoBottom_CaptionToTabIndex(const ACaption: string): integer;
var
  Btn: TATButton;
  i: integer;
begin
  Result:= -1;
  for i:= 0 to ToolbarBtm.ButtonCount-1 do
  begin
    Btn:= ToolbarBtm.Buttons[i];
    if SameText(Btn.Caption, ACaption) then exit(i);
  end;
end;

function TfmMain.DoBottom_AddTab(const ACaption, AControlType: string; ATabIndex: integer): boolean;
var
  Num: integer;
begin
  if DoBottom_CaptionToPanelsIndex(ACaption)>=0 then exit(false);

  Num:= 0;
  while FAppBottomPanels[Num].ItemCaption<>'' do Inc(Num);
  if Num>High(FAppBottomPanels) then exit(false);

  if AControlType=PyPanelControl_Tree then
    DoSidebar_InitPanelTreeview(FAppBottomPanels[Num], ACaption, PanelBottom)
  else
  if AControlType=PyPanelControl_Listbox then
    DoSidebar_InitPanelListbox(FAppBottomPanels[Num], ACaption, PanelBottom)
  else
    exit(false);

  ToolbarBtm.AddButton(-1, @DoBottom_OnTabClick, ACaption, ACaption, '', UiOps.ShowSidebarCaptions);
  Result:= true;
end;

function TfmMain.DoBottom_RemoveTab(const ACaption: string): boolean;
var
  Num, i: integer;
begin
  Num:= DoBottom_CaptionToTabIndex(ACaption);
  Result:= Num>=0;
  if Result then
  begin
    ToolbarBtm.Buttons[Num].Free;
    ToolbarBtm.UpdateControls;

    //hard to remov array item, so hide it by "?"
    for i:= Low(FAppBottomPanels) to High(FAppBottomPanels) do
      if FAppBottomPanels[i].ItemCaption=ACaption then
      begin
        FAppBottomPanels[i].ItemCaption:= '?';
        Break;
      end;
  end;
end;

function TfmMain.DoBottom_ActivateTab(const ACaption: string): boolean;
begin
  Result:= DoBottom_CaptionToTabIndex(ACaption)>=0;
  if Result then
    UpdateBottomPanels(ACaption);
end;


procedure TfmMain.UpdateBottomButtons;
var
  Btn: TATButton;
  i: integer;
begin
  for i:= 0 to ToolbarBtm.ButtonCount-1 do
  begin
    Btn:= ToolbarBtm.Buttons[i];
    Btn.Checked:= SameText(Btn.Caption, FLastBottomPanel) and ShowBottom;
  end;
end;


