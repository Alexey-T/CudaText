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
    Assigned(fmConsole) and
    (fmConsole.EdInput.Focused or
    fmConsole.EdMemo.Focused or
    fmOutput.Ed.Focused or
    fmValidate.Ed.Focused);
end;


procedure TfmMain.DoBottom_FindClick(Sender: TObject);
begin
  if not Assigned(fmFind) then
    InitFormFind;

  if fmFind.Visible then
  begin
    if fmFind.IsReplace then
      DoDialogFind_Hide
    else
      fmFind.IsReplace:= true;
  end
  else
    DoDialogFind(false);
end;

procedure TfmMain.DoBottom_OnBeforeToggle(Sender: TObject);
var
  Frame: TEditorFrame;
begin
  Frame:= CurrentFrame;
  if Assigned(Frame) then
    Frame.IsEditorCaretInView:= Frame.Editor.IsCaretOnVisibleRect;
end;

procedure TfmMain.DoBottom_OnAfterToggle(Sender: TObject);
var
  Frame: TEditorFrame;
begin
  Frame:= CurrentFrame;
  if Frame=nil then exit;

  if not AppPanels[cPaneOut].Visible then
    Frame.SetFocus;

  if Frame.IsEditorCaretInView then
    UpdateEditorShowCaret;
end;

procedure TfmMain.DoBottom_OnCloseFloatForm(Sender: TObject; var CloseAction: TCloseAction);
begin
  UpdateMenuItemChecked(mnuViewBottom, mnuViewBottom_Alt, false);
end;


