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
  SCaption: string;
  Ctl: TWinControl;
  i: integer;
begin
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

  case TabsBottom.TabIndex of
    0:
      begin
        fmConsole.Show;
        fmConsole.Ed.SetFocus;
      end;
    1:
      begin
        ListboxOut.Show;
        ListboxOut.SetFocus;
      end;
    2:
      begin
        ListboxVal.Show;
        ListboxVal.SetFocus;
      end;
    else
      begin
        SCaption:= TabsBottom.GetTabData(TabsBottom.TabIndex).TabCaption;

        //tabs for api log_panel_add
        i:= FAppBottomPanelsListbox.IndexOf(SCaption);
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
              ItemTreeview.Visible:= ItemCaption=SCaption;
              if PanelBottom.Visible then
                if ItemTreeview.Visible then
                  if ItemTreeview.CanFocus then
                    ItemTreeview.SetFocus;
            end
            else
            if Assigned(ItemListbox) then
            begin
              ItemListbox.Visible:= ItemCaption=SCaption;
              if PanelBottom.Visible then
                if ItemListbox.Visible then
                  if ItemListbox.CanFocus then
                    ItemListbox.SetFocus;
            end;
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

function TfmMain.DoBottom_CaptionToTabIndex(const Str: string): integer;
var
  Data: TATTabData;
  i: integer;
begin
  Result:= -1;
  for i:= 0 to TabsBottom.TabCount-1 do
  begin
    Data:= TabsBottom.GetTabData(i);
    if Assigned(Data) then
      if Data.TabCaption=Str then exit(i)
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

  TabsBottom.AddTab(ATabIndex, ACaption);
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
    fmMain.TabsBottom.DeleteTab(Num, false, false);

    //hard to remov array item, so hide it by "?"
    for i:= Low(FAppBottomPanels) to High(FAppBottomPanels) do
      if FAppBottomPanels[i].ItemCaption=ACaption then
      begin
        FAppBottomPanels[i].ItemCaption:= '?';
        Break;
      end;

    if (Num>0) and (fmMain.TabsBottom.TabIndex=Num) then
      fmMain.TabsBottom.TabIndex:= Num-1;
  end;
end;

function TfmMain.DoBottom_ActivateTab(const ACaption: string): boolean;
var
  Num: integer;
begin
  Num:= DoBottom_CaptionToTabIndex(ACaption);
  Result:= Num>=0;
  if Result then
    fmMain.TabsBottom.TabIndex:= Num;
end;

