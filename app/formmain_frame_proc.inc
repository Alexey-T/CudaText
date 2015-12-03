(*
This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at http://mozilla.org/MPL/2.0/.

Copyright (c) Alexey Torgashin
*)
{$ifdef nn}begin end;{$endif}

procedure TfmMain.FrameOnChangeCaption(Sender: TObject);
var
  NPages, NTab: integer;
  Pages: TATPages;
  Frame: TEditorFrame;
  D: TATTabData;
begin
  Frame:= Sender as TEditorFrame;
  NPages:= -1;
  NTab:= -1;
  Groups.PagesAndTabIndexOfControl(Frame, NPages, NTab);
  Pages:= Groups.Pages[NPages];
  D:= Pages.Tabs.GetTabData(NTab);
  if D=nil then Exit;
  D.TabCaption:= Frame.TabCaption;
  D.TabModified:= Frame.Modified;
  Pages.Invalidate;

  UpdateCaption;
end;

procedure TfmMain.FrameOnUpdateStatus(Sender: TObject);
begin
  UpdateStatus;
end;

procedure TfmMain.FrameOnFocus(Sender: TObject);
var
  F: TEditorFrame;
begin
  F:= (Sender as TWinControl).Parent as TEditorFrame;
  Groups.PagesCurrent:= F.Parent as TATPages;
  UpdateCaption;
  UpdateTabsActiveColor(F);

  if FNewClickedEditor<>CurrentEditor then
  begin
    FNewClickedEditor:= CurrentEditor;
    UpdateTree(true);
  end;
end;

function TfmMain.GetFrame(N: integer): TEditorFrame;
var
  D: TATTabData;
begin
  D:= Groups.GetTabDataOfTotalIndex(N);
  if D=nil then
    Result:= nil
  else
    Result:= D.TabObject as TEditorFrame;
end;

function TfmMain.FrameCount: integer;
begin
  Result:= Groups.GetTabTotalCount;
end;

function TfmMain.CurrentFrame: TEditorFrame;
var
  Pages: TATPages;
  D: TATTabData;
begin
  Result:= nil;
  Pages:= Groups.PagesCurrent;
  if Pages.Tabs.TabCount=0 then
    Pages:= Groups.Pages1;
  if Pages.Tabs.TabIndex<0 then
    Pages.Tabs.TabIndex:= 0;

  D:= Pages.Tabs.GetTabData(Pages.Tabs.TabIndex);
  if D=nil then Exit;
  Result:= D.TabObject as TEditorFrame;
end;

function TfmMain.CurrentEditor: TATSynEdit;
var
  F: TEditorFrame;
begin
  Result:= nil;
  F:= CurrentFrame;
  if F<>nil then
    Result:= F.Editor;
end;

function TfmMain.GetEditorFrame(Ed: TATSynEdit): TEditorFrame;
begin
  Result:= Ed.Parent as TEditorFrame;
end;

function TfmMain.GetEditorBrother(Ed: TATSynEdit): TATSynEdit;
var
  F: TEditorFrame;
begin
  F:= GetEditorFrame(Ed);
  if Ed=F.Editor then
    Result:= F.Editor2
  else
    Result:= F.Editor;
end;

procedure TfmMain.SetFrame(Frame: TEditorFrame);
var
  NPages, NTab: integer;
begin
  NPages:= -1;
  NTab:= -1;
  Groups.PagesAndTabIndexOfControl(Frame, NPages, NTab);
  Groups.SetPagesAndTabIndex(NPages, NTab);
end;

procedure TfmMain.FrameOnSaveFile(Sender: TObject);
var
  F: TEditorFrame;
  fn: string;
begin
  F:= Sender as TEditorFrame;
  fn:= F.Filename;
  MsgStatus(msgStatusSavedFile+' '+ExtractFileName(fn)+' ('+ExtractFileDir(fn)+')');

  if fn=GetAppPath(cFileOptUser) then
    //must use timer
    CurrentEditor.DoCommand(cmd_OpsReloadAndApply);
end;

procedure TfmMain.FrameOnSetLexer(Sender: TObject);
begin
  DoOps_LoadOptionsOverride(Sender as TEditorFrame);
end;

procedure TfmMain.FrameParseBegin(Sender: TObject);
begin
  Tree.Items.Clear;
end;

procedure TfmMain.FrameParseDone(Sender: TObject);
var
  Frame: TEditorFrame;
begin
  TimerTreeFill.Enabled:= false;
  TimerTreeFill.Enabled:= true;

  Frame:= (Sender as TATAdapterEControl).Owner as TEditorFrame;
  Frame.DoRestoreFolding;
end;

function TfmMain.FrameOfPopup: TEditorFrame;
var
  Pages: TATPages;
  TabN: integer;
  D: TATTabData;
begin
  Result:= nil;
  Pages:= Groups.PopupPages; if Pages=nil then exit;
  TabN:= Groups.PopupTabIndex; if TabN<0 then exit;
  D:= Pages.Tabs.GetTabData(TabN); if D=nil then exit;
  Result:= D.TabObject as TEditorFrame;
end;


function ConvertTwoPointsToDiffPoint(APrevPnt, ANewPnt: TPoint): TPoint;
begin
  if APrevPnt.Y=ANewPnt.Y then
  begin
    Result.Y:= 0;
    Result.X:= ANewPnt.X-APrevPnt.X;
  end
  else
  begin
    Result.Y:= ANewPnt.Y-APrevPnt.Y;
    Result.X:= ANewPnt.X;
  end;
end;

procedure TfmMain.FrameOnEditorClickMoveCaret(Sender: TObject; APrevPnt,
  ANewPnt: TPoint);
var
  F: TEditorFrame;
  Pnt: TPoint;
begin
  F:= Sender as TEditorFrame;
  if F.MacroRecord then
  begin
    Pnt:= ConvertTwoPointsToDiffPoint(APrevPnt, ANewPnt);
    F.MacroString:= F.MacroString+(Format('%d,%d,%d', [cmd_MouseClick, Pnt.X, Pnt.Y])+#10);
  end;
end;

procedure TfmMain.FrameOnEditorClickEndSelect(Sender: TObject; APrevPnt,
  ANewPnt: TPoint);
var
  F: TEditorFrame;
  Pnt: TPoint;
begin
  F:= Sender as TEditorFrame;
  if F.MacroRecord then
  begin
    Pnt:= ConvertTwoPointsToDiffPoint(APrevPnt, ANewPnt);
    F.MacroString:= F.MacroString+(Format('%d,%d,%d', [cmd_MouseSelect, Pnt.X, Pnt.Y])+#10);
  end;
end;

