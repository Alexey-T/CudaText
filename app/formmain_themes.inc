{$ifdef nnnn}begin end;{$endif}

procedure TfmMain.DoApplyTheme;
var
  F: TEditorFrame;
  PropObject: TAppPanelPropsClass;
  i: integer;
begin
  UpdateMenuThemes(true);
  UpdateMenuThemes(false);

  ATScrollbarTheme.ColorBG:= GetAppColor('ScrollBack');
  ATScrollbarTheme.ColorThumbBorder:= GetAppColor('ScrollRect');
  ATScrollbarTheme.ColorThumbFill:= GetAppColor('ScrollFill');
  ATScrollbarTheme.ColorArrowBorder:= ATScrollbarTheme.ColorBG;
  ATScrollbarTheme.ColorArrowFill:= ATScrollbarTheme.ColorBG;
  ATScrollbarTheme.ColorArrowSign:= GetAppColor('ScrollArrow');
  ATScrollbarTheme.ColorScrolled:= GetAppColor('ScrollScrolled');

  ListboxOut.Color:= GetAppColor('ListBg');
  ListboxOut.Invalidate;
  ListboxVal.Color:= ListboxOut.Color;
  ListboxVal.Invalidate;

  for i:= 0 to 9 do
    AppBookmarkSetup[240+i].Color:= GetAppColor('EdBookmarkBg');

  EditorApplyTheme(fmConsole.ed);
  EditorApplyTheme(fmConsole.memo);

  Toolbar.Color:= GetAppColor('TabBg');
  Self.Color:= Toolbar.Color;

  PanelLeftTitle.Color:= Toolbar.Color;
  ToolbarSide.Color:= GetAppColor('SideBg');
  ToolbarBtm.Color:= ToolbarSide.Color;
  ToolbarAlt.Color:= ToolbarSide.Color;

  Status.Font.Color:= GetAppColor('StatusFont');
  Status.Color:= GetAppColor('StatusBg');
  Status.ColorBorderTop:= GetAppColor('StatusLines');
  Status.ColorBorderR:= GetAppColor('StatusLines');

  StatusAlt.Font.Color:= GetAppColor('StatusAltFont');
  StatusAlt.Color:= GetAppColor('StatusAltBg');
  StatusAlt.ColorBorderTop:= Status.ColorBorderTop;
  StatusAlt.ColorBorderR:= Status.ColorBorderR;

  Gauge.Font.Color:= Status.Font.Color;
  Gauge.ForeColor:= GetAppColor('GaugeFill');
  Gauge.BackColor:= GetAppColor('GaugeBg');
  Gauge.BorderColor:= GetAppColor('ButtonBorderPassive');

  ATButtonTheme.ColorFont:= GetAppColor('ButtonFont');
  ATButtonTheme.ColorFontDisabled:= GetAppColor('ButtonFontDisabled');
  ATButtonTheme.ColorBgPassive:= GetAppColor('ButtonBgPassive');
  ATButtonTheme.ColorBgOver:= GetAppColor('ButtonBgOver');
  ATButtonTheme.ColorBgChecked:= GetAppColor('ButtonBgChecked');
  ATButtonTheme.ColorBgDisabled:= GetAppColor('ButtonBgDisabled');
  ATButtonTheme.ColorBorderPassive:= GetAppColor('ButtonBorderPassive');
  ATButtonTheme.ColorArrows:= GetAppColor('TabArrow');
  ATButtonTheme.ColorBorderOver:= GetAppColor('ButtonBorderOver');
  ATButtonTheme.ColorBorderFocused:= GetAppColor('ButtonBorderFocused');

  Groups.SetTabOption(tabColorText, GetAppColor('TabFont'));
  Groups.SetTabOption(tabColorTextModified, GetAppColor('TabFontMod'));
  Groups.SetTabOption(tabColorBg, GetAppColor('TabBg'));
  Groups.SetTabOption(tabColorBgActive, GetAppColor('TabActive'));
  Groups.SetTabOption(tabColorBgPassive, GetAppColor('TabPassive'));
  Groups.SetTabOption(tabColorBgPassiveOver, GetAppColor('TabOver'));
  Groups.SetTabOption(tabColorBorderActive, GetAppColor('TabBorderActive'));
  Groups.SetTabOption(tabColorBorderPassive, GetAppColor('TabBorderPassive'));
  Groups.SetTabOption(tabColorArrow, GetAppColor('TabArrow'));
  Groups.SetTabOption(tabColorArrowOver, GetAppColor('TabArrowOver'));
  Groups.SetTabOption(tabColorCloseBg, GetAppColor('TabCloseBg'));
  Groups.SetTabOption(tabColorCloseBgOver, GetAppColor('TabCloseBgOver'));
  Groups.SetTabOption(tabColorCloseBorderOver, GetAppColor('TabCloseBorderOver'));
  Groups.SetTabOption(tabColorCloseX, GetAppColor('TabCloseX'));
  Groups.SetTabOption(tabColorCloseXOver, GetAppColor('TabCloseXOver'));
  Groups.Invalidate;

  PanelLeftTitle.Font.Color:= GetAppColor('TabFont');

  if assigned(fmGoto) then
  begin
    fmGoto.Color:= GetAppColor('TabBg');
    fmGoto.LabelGoto.Font.Color:= GetAppColor('TabFont');
    EditorApplyTheme(fmGoto.edInput);
    fmGoto.Invalidate;
  end;

  if assigned(fmFind) then
  begin
    fmFind.Color:= GetAppColor('TabBg');
    fmFind.LabelFind.Font.Color:= GetAppColor('ListFont');
    EditorApplyTheme(fmFind.edFind);
    EditorApplyTheme(fmFind.edRep);
    fmFind.Invalidate;
  end;

  DoApplyThemeToTreeview(Tree);

  //side: proc_sidepanel_add
  for i:= Low(FAppSidePanels) to High(FAppSidePanels) do
    with FAppSidePanels[i] do
    begin
      if ItemCaption='' then break;
      if Assigned(ItemTreeview) then
        DoApplyThemeToTreeview(ItemTreeview);
      if Assigned(ItemListbox) then
        DoApplyThemeToListbox(ItemListbox);
    end;

  //bottom: proc_bottompanel_add
  for i:= Low(FAppBottomPanels) to High(FAppBottomPanels) do
    with FAppBottomPanels[i] do
    begin
      if ItemCaption='' then break;
      if Assigned(ItemTreeview) then
        DoApplyThemeToTreeview(ItemTreeview);
      if Assigned(ItemListbox) then
        DoApplyThemeToListbox(ItemListbox);
    end;

  //bottom: log_panel_add
  for i:= 0 to FAppBottomPanelsListbox.Count-1 do
  begin
    PropObject:= FAppBottomPanelsListbox.Objects[i] as TAppPanelPropsClass;
    DoApplyThemeToListbox(PropObject.Data.Listbox);
  end;

  //apply AppTheme to all frames
  for i:= 0 to FrameCount-1 do
  begin
    F:= Frames[i];
    F.Lexer:= F.Lexer; //apply lexer style map
    EditorApplyTheme(F.Editor);
    EditorApplyTheme(F.Editor2);
    F.Editor.Update;
    F.Editor2.Update;
  end;

  SplitterHorz.Invalidate;
  SplitterVert.Invalidate;

  CompletionOptions.ColorBg:= GetAppColor('ListBg');
  CompletionOptions.ColorSelBg:= GetAppColor('ListSelBg');
  CompletionOptions.ColorFont[0]:= GetAppColor('ListCompletePrefix');
  CompletionOptions.ColorFont[1]:= GetAppColor('ListFont');
  CompletionOptions.ColorFont[2]:= GetAppColor('ListCompleteParams');
end;

procedure TfmMain.DoApplyThemeToTreeview(C: TTreeview);
begin
  C.Font.Color:= GetAppColor('TreeFont');
  C.BackgroundColor:= GetAppColor('TreeBg');
  C.SelectionFontColor:= GetAppColor('TreeSelFont'); //lew Laz
  C.SelectionFontColorUsed:= true; //new Laz
  C.SelectionColor:= GetAppColor('TreeSelBg');
  C.TreeLineColor:= GetAppColor('TreeLines');
  C.TreeLinePenStyle:= psSolid;
  C.ExpandSignColor:= GetAppColor('TreeSign');

  C.BorderStyle:= bsNone;
  C.ExpandSignType:= tvestArrowFill;
  C.HideSelection:= false;
  C.Options:= [
    tvoAutoItemHeight,
    tvoKeepCollapsedNodes,
    tvoShowButtons,
    tvoShowLines,
    tvoShowRoot,
    tvoToolTips
    ];
  C.ShowLines:= UiOps.TreeShowLines;
  C.ScrollBars:= ssNone; //ssVertical;
  C.RowSelect:= true;
  C.RightClickSelect:= true;
  C.ReadOnly:= true;

  C.Invalidate;
end;

procedure TfmMain.DoApplyThemeToListbox(C: proc_globdata.TATListbox);
begin
  C.Color:= GetAppColor('ListBg');
  C.Invalidate;
end;

procedure TfmMain.UpdateMenuThemes(AThemeUI: boolean);
var
  mi: TMenuItem;
  cap, FileMask: string;
  ListThemes: TStringList;
  sub: TMenuItem;
  i: integer;
begin
  if AThemeUI then
    sub:= mnuThemesUI
  else
    sub:= mnuThemesSyntax;

  if sub=nil then exit;
  sub.Clear;

  mi:= TMenuItem.Create(Self);
  mi.Caption:= msgThemeDefault;
  mi.OnClick:= @MenuThemeDefClick;
  if AThemeUI then
  begin
    mi.Checked:= FThemeUi='';
    ListThemes:= FListThemesUI;
    FileMask:= '*'+AppExtensionThemeUi;
  end
  else
  begin
    mi.Checked:= FThemeSyntax='';
    ListThemes:= FListThemesSyntax;
    FileMask:= '*'+AppExtensionThemeSyntax;
  end;
  sub.Add(mi);

  FFindFilesInDir(GetAppPath(cDirDataThemes), FileMask, ListThemes);
  if ListThemes.Count=0 then exit;
  ListThemes.Sort;

  for i:= 0 to ListThemes.Count-1 do
  begin
    cap:= LowerCase(ExtractFileNameOnly(ListThemes[i]));
    if sub.IndexOfCaption(cap)>=0 then Continue; //for Qt
    mi:= TMenuItem.Create(Self);
    mi.Caption:= cap;
    mi.Tag:= i;

    if AThemeUI then
    begin
      mi.OnClick:= @MenuThemesUiClick;
      mi.Checked:= LowerCase(FThemeUi)=LowerCase(mi.Caption);
    end
    else
    begin
      mi.OnClick:= @MenuThemesSyntaxClick;
      mi.Checked:= LowerCase(FThemeSyntax)=LowerCase(mi.Caption);
    end;

    sub.Add(mi);
  end;
end;

procedure TfmMain.SetThemeUi(const AValue: string);
begin
  if FThemeUi=AValue then Exit;
  FThemeUi:= AValue;

  if FThemeUi='' then
  begin
    //both themes reset
    FThemeSyntax:= '';
    DoInitTheme(AppTheme);
  end
  else
    DoLoadTheme(GetAppPath(cDirDataThemes)+DirectorySeparator+FThemeUi+AppExtensionThemeUi, AppTheme, true);

  DoClearLexersAskedList;
  DoApplyTheme;
end;

procedure TfmMain.SetThemeSyntax(const AValue: string);
begin
  if FThemeSyntax=AValue then Exit;
  FThemeSyntax:= AValue;

  if FThemeSyntax='' then
  begin
    //both themes reset
    FThemeUi:= '';
    DoInitTheme(AppTheme);
  end
  else
    DoLoadTheme(GetAppPath(cDirDataThemes)+DirectorySeparator+FThemeSyntax+AppExtensionThemeSyntax, AppTheme, false);

  DoClearLexersAskedList;
  DoApplyTheme;
end;

procedure TfmMain.MenuThemeDefClick(Sender: TObject);
begin
  //this resets both themes
  ThemeUi:= '';
end;

function TfmMain.IsThemeNameExist(const AName: string; AThemeUI: boolean): boolean;
var
  List: TStringList;
  StrName: string;
  i: integer;
begin
  Result:= false;

  if AThemeUI then
    List:= FListThemesUI
  else
    List:= FListThemesSyntax;

  for i:= 0 to List.Count-1 do
  begin
    StrName:= ExtractFileNameOnly(List[i]);
    if StrName=AName then exit(true);
  end;
end;

procedure TfmMain.MenuThemesUiClick(Sender: TObject);
var
  fn: string;
begin
  fn:= FListThemesUI[(Sender as TComponent).Tag];
  ThemeUi:= ExtractFileNameOnly(fn);

  //find syntax-theme with the same name
  if IsThemeNameExist(ThemeUi, true) then
    if MsgBox(msgConfirmSyntaxThemeSameName, MB_OKCANCEL+MB_ICONQUESTION)=ID_OK then
      ThemeSyntax:= ThemeUi;
end;

procedure TfmMain.MenuThemesSyntaxClick(Sender: TObject);
var
  fn: string;
begin
  fn:= FListThemesSyntax[(Sender as TComponent).Tag];
  ThemeSyntax:= ExtractFileNameOnly(fn);
end;

procedure TfmMain.DoDialogTheme(AThemeUI: boolean);
var
  Str, StrName, StrExt: string;
begin
  if AThemeUI then
  begin
    StrName:= ThemeUi;
    StrExt:= AppExtensionThemeUi;
  end
  else
  begin
    StrName:= ThemeSyntax;
    StrExt:= AppExtensionThemeSyntax;
  end;

  if DoDialogConfigTheme(AppTheme, AThemeUI) then
  begin
    DoApplyTheme;
    if Msgbox(msgConfirmSaveColorsToFile, MB_OKCANCEL or MB_ICONQUESTION)=ID_OK then
    begin
      Str:= Trim(InputBox(msgTitle, msgThemeName, StrName));
      if Str='' then exit;
      Str:= GetAppPath(cDirDataThemes)+DirectorySeparator+Str+StrExt;

      DoSaveTheme(Str, AppTheme, AThemeUI);
      UpdateMenuThemes(AThemeUI);
    end;
  end;
end;

function TfmMain.DoDialogConfigTheme(var AData: TAppTheme; AThemeUI: boolean
  ): boolean;
var
  Form: TfmColorSetup;
  i: integer;
begin
  Form:= TfmColorSetup.Create(nil);
  try
    Form.PanelUi.Visible:= AThemeUI;
    Form.PanelSyntax.Visible:= not AThemeUI;

    DoLocalize_FormColorSetup(Form);
    Form.OnApply:= @FormColorsApply;
    Form.Data:= AData;
    Result:= Form.ShowModal=mrOk;

    if Result then
    begin
      AData:= Form.Data;
      for i:= 0 to FrameCount-1 do
        with Frames[i] do Lexer:= Lexer;
      UpdateFrame;
    end;
  finally
    FreeAndNil(Form);
  end;
end;


