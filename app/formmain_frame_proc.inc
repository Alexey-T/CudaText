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
begin
  Groups.PagesCurrent:= (Sender as TWinControl).Parent.Parent as TATPages;
  UpdateCaption;

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
begin
  TimerTreeFill.Enabled:= false;
  TimerTreeFill.Enabled:= true;
end;

procedure TfmMain.mnuHelpWikiClick(Sender: TObject);
begin
  OpenURL('http://wiki.freepascal.org/CudaText');
end;

procedure TfmMain.MenuThemesClick(Sender: TObject);
var
  fn: string;
begin
  fn:= FListThemes[(Sender as TComponent).Tag];
  FThemeName:= ExtractFileNameOnly(fn);
  DoLoadTheme(fn, Theme);
  DoApplyTheme;
end;

procedure TfmMain.mnuHelpLexInstClick(Sender: TObject);
begin
  DoFileOpen(GetAppPath(cFileReadmeLexerInst));
end;

procedure TfmMain.mnuOpKeysClick(Sender: TObject);
begin
  MsgBox('To customize hotkeys, call Commands dialog, focus any command, and press F9, you''ll see additional dialog', mb_ok);
end;

procedure TfmMain.mnuTabColorClick(Sender: TObject);
var
  F: TEditorFrame;
begin
  F:= FrameOfPopup;
  if F=nil then exit;

  with TfmPalette.Create(Self) do
  try
    ResColor:= F.TabColor;
    case ShowModal of
      mrOk: F.TabColor:= ResColor;
      mrNo: F.TabColor:= clNone;
    end;
  finally
    Free
  end;
end;

procedure TfmMain.MenuThemeDefClick(Sender: TObject);
begin
  FThemeName:= '';
  DoInitTheme(Theme);
  DoApplyTheme;
end;

procedure TfmMain.mnuTabsize1Click(Sender: TObject);
begin
  UpdateEditorTabsize(1);
end;

procedure TfmMain.mnuTabsize2Click(Sender: TObject);
begin
  UpdateEditorTabsize(2);
end;

procedure TfmMain.mnuTabsize4Click(Sender: TObject);
begin
  UpdateEditorTabsize(4);
end;

procedure TfmMain.mnuTabsize8Click(Sender: TObject);
begin
  UpdateEditorTabsize(8);
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


