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
  cDx=4; cDy=1;
var
  Listbox: ATListbox.TATListbox;
begin
  if AIndex<0 then exit;
  Listbox:= Sender as ATListbox.TATListbox;

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


procedure TfmMain.DoSidebar_OnTabClick(Sender: TObject);
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
    AppPanels[cSideLeft].Visible:= false;
    Exit
  end;

  //avoid plugin call if panel already inited
  NPanelIndex:= AppPanels[cSideLeft].CaptionToPanelIndex(SCaption);
  if (NPanelIndex>=0) and (TAppPanelItem(AppPanels[cSideLeft].Panels[NPanelIndex]).ItemControl=nil) then
    if Btn.DataString<>'' then
    begin
      SSplitByChar(Btn.DataString, '.', SModule, SMethod);
      //MsgLogConsole(Format('sidebar click: caption "%s", py %s.%s',
      //  [SCaption, SModule, SMethod]));
      DoPyCommand(SModule, SMethod, []);
    end;

  UpdateSidebarPanels(SCaption, true);
  Btn.Checked:= true;
end;


procedure TfmMain.UpdateSidebarPanels(const ACaption: string; AndFocus: boolean);
var
  bFound: boolean;
  i: integer;
begin
  AppPanels[cSideLeft].LastActivePanel:= ACaption;
  AppPanels[cSideLeft].Visible:= true;
  AppPanels[cSideLeft].UpdateButtons;
  AppPanels[cSideLeft].OnChange(nil);

  for i:= 0 to AppPanels[cSideLeft].Panels.Count-1 do
    with TAppPanelItem(AppPanels[cSideLeft].Panels[i]) do
    begin
      if ItemCaption='' then Break;
      bFound:= SameText(ItemCaption, ACaption);
      if Assigned(ItemControl) then
      begin
        ItemControl.Visible:= bFound;
        if AndFocus then
          if PanelLeft.Visible then
            if ItemControl.Visible then
              if ItemControl.CanFocus then
                ItemControl.SetFocus;
      end
      else
      if bFound and (ItemModule<>'') and (ItemMethod<>'') then
        DoPyCommand(ItemModule, ItemMethod, []);
    end;
end;


function TfmMain.DoSidebar_FilenameToImageIndex(ATabCaption, AFilename: string): integer;
begin
  if AFilename='' then
    AFilename:= LowerCase(ATabCaption)+'.png';
  if ExtractFileDir(AFilename)='' then
    AFilename:= AppDir_DataSidebarIcons+DirectorySeparator+UiOps.SidebarTheme+DirectorySeparator+AFilename;

  Result:= UpdateImagelistWithIconFromFile(ImageListSide, AFilename);
end;


function TfmMain.DoSidebar_ActivateTab(const ACaption: string; AndFocus: boolean): boolean;
begin
  Result:= AppPanels[cSideLeft].CaptionToButtonIndex(ACaption)>=0;
  if Result then
    UpdateSidebarPanels(ACaption, AndFocus);
end;


procedure TfmMain.DoSidebar_FocusCodetreeFilter;
begin
  DoSidebar_ActivateTab(msgPanelTree_Init, false);
  if CodeTree.Visible and
    CodeTreeFilterInput.Visible and
    CodeTreeFilterInput.Enabled then
  begin
    if FloatSide then
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
  DoSidebar_ActivateTab(msgPanelTree_Init, false);
  if CodeTree.Visible and
    CodeTree.Enabled then
  begin
    if FloatSide then
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

procedure TfmMain.DoSidebar_OnPythonCall(const AStr: string);
var
  SModule, SMethod: string;
begin
  SSplitByChar(AStr, '.', SModule, SMethod);
  DoPyCommand(SModule, SMethod, []);
end;

