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
  if AIndex<0 then exit;
  Listbox:= Sender as TATListbox;

  C.Font.Color:= GetAppColor('ListFont');
  C.Brush.Color:= GetAppColor('ListBg');

  if AIndex=Listbox.ItemIndex then
  begin
    C.Font.Color:= GetAppColor('ListSelFont');
    C.Brush.Color:= GetAppColor('ListSelBg');
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
  Pnt:= ClientToScreen(Point(0, 0));
  PopupSidebarClone.Popup(Pnt.X, Pnt.Y);
end;


function TfmMain.DoSidebar_FilenameToImageIndex(ATabCaption, AFilename: string): integer;
begin
  if AFilename='' then
    AFilename:= LowerCase(ATabCaption)+'.png';
  if ExtractFileDir(AFilename)='' then
    AFilename:= AppDir_DataSidebarIcons+DirectorySeparator+UiOps.SidebarTheme+DirectorySeparator+AFilename;

  Result:= UpdateImagelistWithIconFromFile(ImageListSide, AFilename);
end;


procedure TfmMain.DoSidebar_FocusCodetreeFilter;
begin
  AppPanels[cSideLeft].UpdatePanels(msgPanelTree_Init, false, true);
  if CodeTree.Visible and
    CodeTreeFilterInput.Visible and
    CodeTreeFilterInput.Enabled then
  begin
    if AppPanels[cSideLeft].Floating then
    begin
      if AppPanels[cSideLeft].FormFloat.Visible then
      begin
        AppPanels[cSideLeft].FormFloat.SetFocus;
        AppPanels[cSideLeft].FormFloat.ActiveControl:= CodeTreeFilterInput;
      end;
    end
    else
      ActiveControl:= CodeTreeFilterInput;
  end;
end;

procedure TfmMain.DoSidebar_FocusCodetree;
begin
  AppPanels[cSideLeft].UpdatePanels(msgPanelTree_Init, false, true);
  if CodeTree.Visible and
    CodeTree.Enabled then
  begin
    if AppPanels[cSideLeft].Floating then
    begin
      if AppPanels[cSideLeft].FormFloat.Visible then
      begin
        AppPanels[cSideLeft].FormFloat.SetFocus;
        AppPanels[cSideLeft].FormFloat.ActiveControl:= CodeTree.Tree;
      end;
    end
    else
      ActiveControl:= CodeTree.Tree;
  end;
end;


procedure TfmMain.CodeTreeFilter_OnCommand(Sender: TObject; ACmd: integer;
  const AText: string; var AHandled: boolean);
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

procedure TfmMain.DoSidebar_OnTabChange(Sender: TObject);
var
  SCaption: string;
begin
  with AppPanels[cSideLeft] do
  begin
    SCaption:= msgTranslatedPanelCaption(LastActivePanel);
    LabelSideTitle.Caption:= SCaption;
    if Floating then
      FormFloat.Caption:= SCaption + ' - ' + msgTitle;
  end;
end;

procedure TfmMain.DoSidebar_OnShowCodeTree(Sender: TObject);
begin
  UpdateTreeContents;
end;

procedure TfmMain.DoSidebar_OnPythonCall(const ACallback: string);
var
  SModule, SMethod: string;
begin
  if ACallback<>'' then
  begin
    SSplitByChar(ACallback, '.', SModule, SMethod);
    DoPyCommand(SModule, SMethod, []);
  end;
end;

function TfmMain.DoSidebar_GetFormTitle(const ACaption: string): string;
begin
  Result:= msgTranslatedPanelCaption(ACaption) + ' - ' + msgTitle;
end;

