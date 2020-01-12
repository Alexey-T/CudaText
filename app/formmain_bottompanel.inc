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
  for i:= 0 to ToolbarSideLow.ButtonCount-1 do
  begin
    Btn:= ToolbarSideLow.Buttons[i];
    Btn.Checked:= Btn=Sender;
    if Btn.Checked then
    begin
      SCaption:= Btn.Caption;
    end;
  end;

  if SCaption<>'' then
    UpdateBottomPanels(SCaption, true)
  else
    ShowBottom:= false;
end;


procedure TfmMain.DoBottom_AddonsClick(Sender: TObject);
begin
  DoPyCommand('cuda_addonman', 'do_install_addon', []);
end;

procedure TfmMain.DoBottom_FindClick(Sender: TObject);
var
  bShown: boolean;
begin
  bShown:= Assigned(fmFind) and fmFind.Visible;
  if bShown then
    DoDialogFind_Hide
  else
    DoDialogFind(false);
  UpdateStatus;
end;

procedure TfmMain.UpdateBottomPanels(const ACaption: string; AndFocus: boolean);
var
  bConsole, bListboxOut, bListboxVal: boolean;
  Ctl: TWinControl;
  i: integer;
begin
  FLastBottomPanel:= ACaption;

  ShowBottom:= true;
  if FloatBottom then
    FFormFloatBottom.Caption:= ACaption + ' - ' + msgTitle;

  //hide panels from proc_bottompanel_add
  for i:= 0 to AppBottomPanels.Count-1 do
  begin
    Ctl:= TAppSidePanel(AppBottomPanels[i]).ItemControl;
    if Assigned(Ctl) then
      Ctl.Hide;
  end;

  bConsole:= SameText(ACaption, msgPanelConsole_Init);
  bListboxOut:= SameText(ACaption, msgPanelOutput_Init);
  bListboxVal:= SameText(ACaption, msgPanelValidate_Init);

  fmConsole.Visible:= bConsole;
  ListboxOut.Visible:= bListboxOut;
  ListboxVal.Visible:= bListboxVal;

  if bConsole then
  begin
    if AndFocus then
      fmConsole.Ed.SetFocus;
  end
  else
  if bListboxOut then
  begin
    if AndFocus then
      ListboxOut.SetFocus;
  end
  else
  if bListboxVal then
  begin
    if AndFocus then
      ListboxVal.SetFocus;
  end
  else
  begin
    //tabs for api proc_bottompanel
    for i:= 0 to AppBottomPanels.Count-1 do
      with TAppSidePanel(AppBottomPanels[i]) do
      begin
        if Assigned(ItemControl) then
        begin
          ItemControl.Visible:= SameText(ItemCaption, ACaption);
          if AndFocus then
            if PanelBottom.Visible then
              if ItemControl.Visible then
                if ItemControl.CanFocus then
                  ItemControl.SetFocus;
        end
      end;
  end;
end;

function TfmMain.DoBottom_CaptionToPanelsIndex(const ACaption: string): integer;
var
  i: integer;
begin
  Result:= -1;
  for i:= 0 to AppBottomPanels.Count-1 do
    with TAppSidePanel(AppBottomPanels[i]) do
      if ItemCaption=ACaption then
        exit(i);
end;

function TfmMain.DoBottom_CaptionToControlHandle(const ACaption: string): PtrInt;
var
  Num: integer;
begin
  Result:= 0;
  Num:= DoBottom_CaptionToPanelsIndex(ACaption);
  if Num<0 then exit;

  with TAppSidePanel(AppBottomPanels[Num]) do
    if Assigned(ItemControl) then
      Result:= PtrInt(ItemControl);
end;

function TfmMain.DoBottom_CaptionToTabIndex(const ACaption: string): integer;
var
  Btn: TATButton;
  i: integer;
begin
  Result:= -1;
  for i:= 0 to ToolbarSideLow.ButtonCount-1 do
  begin
    Btn:= ToolbarSideLow.Buttons[i];
    if SameText(Btn.Caption, ACaption) then exit(i);
  end;
end;

function TfmMain.DoBottom_AddTab(const ACaption: string; AImageIndex: integer;
  AHandle: PtrInt): boolean;
var
  Panel: TAppSidePanel;
begin
  if DoBottom_CaptionToPanelsIndex(ACaption)>=0 then exit(false);

  Panel:= TAppSidePanel.Create;
  AppBottomPanels.Add(Panel);

  DoSidebar_InitPanelForm(Panel, ACaption, TCustomForm(AHandle), PanelBottom);

  ToolbarSideLow.AddButton(AImageIndex, @DoBottom_OnTabClick, ACaption, ACaption, '', UiOps.ShowSidebarCaptions);
  ToolbarSideLow.UpdateControls;
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
    ToolbarSideLow.Buttons[Num].Free;
    ToolbarSideLow.UpdateControls;

    //hard to remove item, so hide it by "?"
    for i:= 0 to AppBottomPanels.Count-1 do
      with TAppSidePanel(AppBottomPanels[i]) do
        if ItemCaption=ACaption then
        begin
          ItemCaption:= '?';
          Break;
        end;
  end;
end;

function TfmMain.DoBottom_ActivateTab(const ACaption: string; AndFocus: boolean): boolean;
begin
  Result:= DoBottom_CaptionToTabIndex(ACaption)>=0;
  if Result then
    UpdateBottomPanels(ACaption, AndFocus);
end;


procedure TfmMain.UpdateBottomButtons;
var
  Btn: TATButton;
  i: integer;
begin
  for i:= 0 to ToolbarSideLow.ButtonCount-1 do
  begin
    Btn:= ToolbarSideLow.Buttons[i];
    Btn.Checked:= SameText(Btn.Caption, FLastBottomPanel) and ShowBottom;
  end;
end;


