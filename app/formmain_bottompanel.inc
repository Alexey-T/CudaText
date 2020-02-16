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
  SCaption: string;
  NPanelIndex: integer;
begin
  Btn:= Sender as TATButton;
  SCaption:= Btn.Caption;

  if Btn.Checked or (SCaption='') then
  begin
    Btn.Checked:= false;
    AppPanels[cSideBottom].Visible:= false;
    Exit
  end;

  //avoid plugin call if panel already inited
  NPanelIndex:= AppPanels[cSideBottom].CaptionToPanelIndex(SCaption);
  if (NPanelIndex>=0) and (TAppPanelItem(AppPanels[cSideBottom].Panels[NPanelIndex]).ItemControl=nil) then
    AppPanels[cSideBottom].OnPythonCall(Btn.DataString);

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
  Ctl: TWinControl;
  i: integer;
begin
  AppPanels[cSideBottom].LastActivePanel:= ACaption;
  AppPanels[cSideBottom].Visible:= true;
  AppPanels[cSideBottom].UpdateButtons;
  AppPanels[cSideBottom].OnChange(nil);

  //hide panels from proc_bottompanel_add
  for i:= 0 to AppPanels[cSideBottom].Panels.Count-1 do
  begin
    Ctl:= TAppPanelItem(AppPanels[cSideBottom].Panels[i]).ItemControl;
    if Assigned(Ctl) then
      Ctl.Hide;
  end;

  //tabs for api proc_bottompanel
  for i:= 0 to AppPanels[cSideBottom].Panels.Count-1 do
    with TAppPanelItem(AppPanels[cSideBottom].Panels[i]) do
      if Assigned(ItemControl) then
      begin
        ItemControl.Visible:= SameText(ItemCaption, ACaption);
        if AndFocus then
          if PanelBottom.Visible then
            if ItemControl.Visible then
              if ItemControl.CanFocus then
                ItemControl.SetFocus;
      end;
end;

function TfmMain.DoBottom_ActivateTab(const ACaption: string; AndFocus: boolean): boolean;
begin
  Result:= AppPanels[cSideBottom].CaptionToButtonIndex(ACaption)>=0;
  if Result then
    UpdateBottomPanels(ACaption, AndFocus);
end;

procedure TfmMain.DoBottom_OnTabChange(Sender: TObject);
begin
  with AppPanels[cSideBottom] do
    if Floating then
      FormFloat.Caption:= msgTranslatedPanelCaption(LastActivePanel) + ' - ' + msgTitle;
end;

procedure TfmMain.DoBottom_OnHide(Sender: TObject);
var
  Frame: TEditorFrame;
begin
  Frame:= CurrentFrame;
  if Assigned(Frame) then
    Frame.SetFocus;
end;

