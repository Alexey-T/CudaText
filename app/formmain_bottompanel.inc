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
    UpdateBottomPanels(SCaption)
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

procedure TfmMain.UpdateBottomPanels(const ACaption: string);
var
  Ctl: TWinControl;
  i: integer;
begin
  FLastBottomPanel:= ACaption;
  ShowBottom:= true;
  if FloatBottom then
    FFormFloatBottom.Caption:= ACaption + ' - ' + msgTitle;

  //hide std panels
  fmConsole.Hide;
  ListboxOut.Hide;
  ListboxVal.Hide;

  //hide panels from proc_bottompanel_add
  for i:= Low(AppBottomPanels) to High(AppBottomPanels) do
  begin
    Ctl:= AppBottomPanels[i].ItemControl;
    if Assigned(Ctl) then
      Ctl.Hide;
  end;

  if SameText(ACaption, msgPanelConsole_Init) then
  begin
    fmConsole.Show;
    fmConsole.Ed.SetFocus;
  end
  else
  if SameText(ACaption, msgPanelOutput_Init) then
  begin
    ListboxOut.Show;
    ListboxOut.SetFocus;
  end
  else
  if SameText(ACaption, msgPanelValidate_Init) then
  begin
    ListboxVal.Show;
    ListboxVal.SetFocus;
  end
  else
  begin
    //tabs for api proc_bottompanel
    for i:= Low(AppBottomPanels) to High(AppBottomPanels) do
      with AppBottomPanels[i] do
      begin
        if ItemCaption='' then break;
        if Assigned(ItemControl) then
        begin
          ItemControl.Visible:= SameText(ItemCaption, ACaption);
          if PanelBottom.Visible then
            if ItemControl.Visible then
              if ItemControl.CanFocus then
                ItemControl.SetFocus;
        end
      end;
  end;
end;

function TfmMain.DoBottom_CaptionToPanelsIndex(const Str: string): integer;
var
  i: integer;
begin
  Result:= -1;
  for i:= Low(AppBottomPanels) to High(AppBottomPanels) do
    with AppBottomPanels[i] do
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

  with AppBottomPanels[Num] do
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
  Num: integer;
begin
  if DoBottom_CaptionToPanelsIndex(ACaption)>=0 then exit(false);

  Num:= 0;
  while AppBottomPanels[Num].ItemCaption<>'' do Inc(Num);
  if Num>High(AppBottomPanels) then exit(false);

  DoSidebar_InitPanelForm(AppBottomPanels[Num], ACaption, TCustomForm(AHandle), PanelBottom);

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

    //hard to remov array item, so hide it by "?"
    for i:= Low(AppBottomPanels) to High(AppBottomPanels) do
      if AppBottomPanels[i].ItemCaption=ACaption then
      begin
        AppBottomPanels[i].ItemCaption:= '?';
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
  for i:= 0 to ToolbarSideLow.ButtonCount-1 do
  begin
    Btn:= ToolbarSideLow.Buttons[i];
    Btn.Checked:= SameText(Btn.Caption, FLastBottomPanel) and ShowBottom;
  end;
end;


