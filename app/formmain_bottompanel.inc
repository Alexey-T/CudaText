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
    fmConsole.EdInput.Focused or
    fmConsole.EdMemo.Focused or
    ListboxOut.Focused or
    ListboxVal.Focused;
end;


procedure TfmMain.DoBottom_OnTabClick(Sender: TObject);
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
    ShowBottom:= false;
    Exit
  end;

  //avoid plugin call if panel already inited
  NPanelIndex:= DoBottom_CaptionToPanelsIndex(SCaption);
  if (NPanelIndex>=0) and (TAppSidePanel(AppPanels[cSideBottom].Panels[NPanelIndex]).ItemControl=nil) then
    if Btn.DataString<>'' then
    begin
      SSplitByChar(Btn.DataString, '.', SModule, SMethod);
      //MsgLogConsole(Format('bottombar click: caption "%s", py %s.%s',
      //  [SCaption, SModule, SMethod]));
      DoPyCommand(SModule, SMethod, []);
    end;

  UpdateBottomPanels(SCaption, true);
  Btn.Checked:= true;
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
    FFormFloatBottom.Caption:= DoBottom_TranslatedCaption(ACaption) + ' - ' + msgTitle;

  //hide panels from proc_bottompanel_add
  for i:= 0 to AppPanels[cSideBottom].Panels.Count-1 do
  begin
    Ctl:= TAppSidePanel(AppPanels[cSideBottom].Panels[i]).ItemControl;
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
      fmConsole.EdInput.SetFocus;
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
    for i:= 0 to AppPanels[cSideBottom].Panels.Count-1 do
      with TAppSidePanel(AppPanels[cSideBottom].Panels[i]) do
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
  for i:= 0 to AppPanels[cSideBottom].Panels.Count-1 do
    with TAppSidePanel(AppPanels[cSideBottom].Panels[i]) do
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

  with TAppSidePanel(AppPanels[cSideBottom].Panels[Num]) do
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
  AppPanels[cSideBottom].Panels.Add(Panel);

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
    for i:= 0 to AppPanels[cSideBottom].Panels.Count-1 do
      with TAppSidePanel(AppPanels[cSideBottom].Panels[i]) do
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

function TfmMain.DoBottom_TranslatedCaption(const ACaption: string): string;
begin
  case ACaption of
    msgPanelConsole_Init:
      Result:= msgPanelConsole;
    msgPanelOutput_Init:
      Result:= msgPanelOutput;
    msgPanelValidate_Init:
      Result:= msgPanelValidate;
    else
      Result:= ACaption;
  end;
end;

