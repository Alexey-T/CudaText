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

