(*
This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at http://mozilla.org/MPL/2.0/.

Copyright (c) Alexey Torgashin
*)
{$ifdef nn}begin end;{$endif}

procedure TfmMain.DoSidebar_ListboxDrawItem(Sender: TObject; C: TCanvas;
  AIndex: integer; const ARect: TRect);
const
  cDx=4;
  cDy=1;
var
  Listbox: TATListbox;
begin
  Listbox:= Sender as TATListbox;
  if not Listbox.IsIndexValid(AIndex) then exit;

  C.Font.Color:= GetAppColor(apclListFont);
  C.Brush.Color:= GetAppColor(apclListBg);

  if AIndex=Listbox.ItemIndex then
  begin
    C.Font.Color:= GetAppColor(apclListSelFont);
    C.Brush.Color:= GetAppColor(apclListSelBg);
    C.FillRect(ARect);
  end;

  C.TextOut(ARect.Left+cDx, ARect.Top+cDy, Listbox.Items[AIndex]);
end;

procedure TfmMain.DoSidebar_MainMenuClick(Sender: TObject);
var
  Pnt: TPoint;
begin
  if not Assigned(PopupSidebarClone) then
    PopupSidebarClone:= TPopupMenu.Create(Self)
  else
    PopupSidebarClone.Items.Clear;

  //fill submenu "Recent files"
  MenuRecentsPopup(nil);

  Menu_Copy(MainMenu, PopupSidebarClone);

  //better show menu from top-left corner, coz otherwise,
  //when sidebar is on the right, menu fly-outs overlap each other
  Pnt:= PanelAll.ClientToScreen(Point(0, 0));

  UpdateMenuTheming(PopupSidebarClone);
  PopupSidebarClone.Popup(Pnt.X, Pnt.Y);
end;


function TfmMain.DoSidebar_FilenameToImageIndex(ATabCaption, AFilename: string): integer;
begin
  if AFilename='' then
    AFilename:= LowerCase(ATabCaption)+'.png';
  if ExtractFileDir(AFilename)='' then
    AFilename:= AppDir_DataSidebarIcons+DirectorySeparator+UiOps.SidebarTheme+DirectorySeparator+AFilename;

  Result:= UpdateImagelistWithIconFromFile(ImageListSide, AFilename, 'DoSidebar_FilenameToImageIndex');
end;


procedure TfmMain.DoSidebar_FocusCodetreeFilter;
var
  ok: boolean;
begin
  ok:= false;
  AppPanels[cPaneSide].UpdatePanels(msgPanelTree_Init, false, true);
  if CodeTree.Visible and
    CodeTreeFilterInput.CanFocus then
  begin
    if AppPanels[cPaneSide].Floating then
      ok:= AppPanels[cPaneSide].FormFloat.Visible
    else
      ok:= true
  end;
  if ok then
  begin
    EditorFocus(CodeTreeFilterInput);

    {$ifndef windows}
    //it helped on Linux gtk2, issue #3755
    Application.ProcessMessages;
    CodeTreeFilterInput.SetFocus;
    {$endif}
  end;
end;

procedure TfmMain.DoSidebar_FocusCodetree;
begin
  AppPanels[cPaneSide].UpdatePanels(msgPanelTree_Init, false, true);
  if CodeTree.Visible and
    CodeTree.Enabled then
  begin
    if AppPanels[cPaneSide].Floating then
    begin
      if AppPanels[cPaneSide].FormFloat.Visible then
      begin
        AppPanels[cPaneSide].FormFloat.SetFocus;
        AppPanels[cPaneSide].FormFloat.ActiveControl:= CodeTree.Tree;
      end;
    end
    else
      ActiveControl:= CodeTree.Tree;
  end;
end;


procedure TfmMain.CodeTreeFilter_OnCommand(Sender: TObject; ACmd: integer;
  AInvoke: TATEditorCommandInvoke; const AText: string; var AHandled: boolean);
var
  Ed: TATComboEdit;
  F: TEditorFrame;
begin
  Ed:= CodeTreeFilterInput;

  if ACmd=cCommand_KeyEnter then
  begin
    Ed.DoAddLineToHistory(Ed.Text, UiOps.MaxHistoryEdits);

    F:= CurrentFrame;
    if Assigned(F) then
      F.CodetreeFilterHistory.Assign(Ed.Items);

    AHandled:= true;
  end;
end;

procedure TfmMain.DoSidebar_OnShowCodeTree(Sender: TObject);
begin
  PanelCodeTreeAll.Show;
  UpdateTreeByTimer;
end;

procedure TfmMain.DoSidebar_OnPythonCall(const ACallback: string);
var
  SModule, SMethod: string;
begin
  if ACallback<>'' then
  begin
    SSplitByChar(ACallback, '.', SModule, SMethod);
    DoPyCommand(SModule, SMethod, [], TATEditorCommandInvoke.AppSidebar);
  end;
end;

function TfmMain.DoSidebar_GetFormTitle(const ACaption: string): string;
begin
  Result:= msgTranslatedPanelCaption(ACaption);
end;

procedure TfmMain.DoSidebar_OnCloseFloatForm(Sender: TObject; var CloseAction: TCloseAction);
begin
  UpdateMenuItemChecked(mnuViewSide, mnuViewSide_Alt, false);
end;

procedure TfmMain.DoSidebar_OnShowHide(Sender: TObject);
begin
  UpdateEditorShowCaret;
end;

procedure TfmMain.DoSidebar_OnContextPopup(const ACaption: string);
begin
  DoPyEvent(nil, cEventOnSidebarPopup, [
    AppVariant(ACaption)
    ]);
end;

