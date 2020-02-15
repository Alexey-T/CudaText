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
  NPanelIndex:= AppPanels[cSideBottom].CaptionToPanelIndex(SCaption);
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
  AppPanels[cSideBottom].LastActivePanel:= ACaption;

  ShowBottom:= true;
  if FloatBottom then
    AppPanels[cSideBottom].FormFloat.Caption:= DoSidebar_TranslatedCaption(ACaption) + ' - ' + msgTitle;

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

function TfmMain.DoBottom_ActivateTab(const ACaption: string; AndFocus: boolean): boolean;
begin
  Result:= AppPanels[cSideBottom].CaptionToTabIndex(ACaption)>=0;
  if Result then
    UpdateBottomPanels(ACaption, AndFocus);
end;


