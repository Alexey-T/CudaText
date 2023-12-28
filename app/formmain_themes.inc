{$ifdef nnnn}begin end;{$endif}

procedure DoApplyThemeToCustomDialog(F: TFormDummy);
var
  C: TComponent;
  i: integer;
begin
  for i:= 0 to F.ComponentCount-1 do
  begin
    C:= F.Components[i];

    {
    if C is TATListbox then
      if (C as TATListbox).ThemedColors then
        DoApplyThemeToListbox(C as ATListbox.TATListbox);
        }

    if C is TATSynEdit then
      if TATSynEdit(C).OptThemed then
        EditorApplyTheme(TATSynEdit(C));

    if C is TAppTreeContainer then
      if TAppTreeContainer(C).Themed then
        ApplyThemeToTreeview(TAppTreeContainer(C).Tree, true, false);

    if C is TATFlatToolbar then
      if TATFlatToolbar(C).Themed then
        ApplyThemeToToolbar(TATFlatToolbar(C));

    if C is TATStatus then
      TATStatus(C).AutoSize;
  end;
end;


procedure TfmMain.DoApplyThemeToGroups(G: TATGroups);
begin
  G.SetTabOption(tabColorText, GetAppColor(TAppThemeColor.TabFont));
  G.SetTabOption(tabColorTextActive, GetAppColor(TAppThemeColor.TabFontActive));
  G.SetTabOption(tabColorTextModified, GetAppColor(TAppThemeColor.TabFontMod));
  G.SetTabOption(tabColorBg, GetAppColor(TAppThemeColor.TabBg));
  G.SetTabOption(tabColorBgActive, GetAppColor(TAppThemeColor.TabActive));
  G.SetTabOption(tabColorBgPassive, GetAppColor(TAppThemeColor.TabPassive));
  G.SetTabOption(tabColorBgPassiveOver, GetAppColor(TAppThemeColor.TabOver));
  G.SetTabOption(tabColorBorderActive, GetAppColor(TAppThemeColor.TabBorderActive));
  G.SetTabOption(tabColorBorderPassive, GetAppColor(TAppThemeColor.TabBorderPassive));
  G.SetTabOption(tabColorSeparator, GetAppColor(TAppThemeColor.TabBorderActive));
  G.SetTabOption(tabColorArrow, GetAppColor(TAppThemeColor.TabArrow));
  G.SetTabOption(tabColorArrowOver, GetAppColor(TAppThemeColor.TabArrowOver));
  G.SetTabOption(tabColorCloseBg, GetAppColor(TAppThemeColor.TabCloseBg));
  G.SetTabOption(tabColorCloseBgOver, GetAppColor(TAppThemeColor.TabCloseBgOver));
  G.SetTabOption(tabColorCloseBorderOver, GetAppColor(TAppThemeColor.TabCloseBorderOver));
  G.SetTabOption(tabColorCloseX, GetAppColor(TAppThemeColor.TabCloseX));
  G.SetTabOption(tabColorCloseXOver, GetAppColor(TAppThemeColor.TabCloseXOver));
  G.SetTabOption(tabColorActiveMark, GetAppColor(TAppThemeColor.TabActiveMark));
  G.SetTabOption(tabColorSpecMarks, GetAppColor(TAppThemeColor.TabMarks));
  G.Invalidate;
end;

procedure TfmMain.DoApplyTheme;
var
  id: TAppPanelId;
  NColor: TColor;
  i: integer;
begin
  with ATScrollbarTheme do
  begin
    ColorBG:= GetAppColor(TAppThemeColor.ScrollBack);
    ColorBorder:= ColorBG;
    ColorThumbBorder:= GetAppColor(TAppThemeColor.ScrollRect);
    ColorThumbFill:= GetAppColor(TAppThemeColor.ScrollFill);
    ColorThumbFillOver:= ColorThumbFill;
    ColorThumbFillPressed:= ColorThumbFill;
    ColorThumbDecor:= ColorThumbBorder;
    ColorArrowBorder:= ColorBG;
    ColorArrowFill:= ColorBG;
    ColorArrowFillOver:= ColorArrowFill;
    ColorArrowFillPressed:= ColorArrowFill;
    ColorArrowSign:= GetAppColor(TAppThemeColor.ScrollArrow);
    ColorArrowSignOver:= GetAppColor(TAppThemeColor.TabArrowOver);
    ColorArrowSignPressed:= ColorArrowSignOver;
    ColorScrolled:= GetAppColor(TAppThemeColor.ScrollScrolled);
  end;

  EditorApplyTheme(fmOutput.Ed);
  EditorApplyTheme(fmValidate.Ed);

  EditorApplyTheme(CodeTreeFilterInput);

  ToolbarMain.Color:= GetAppColor(TAppThemeColor.TabBg);
  Self.Color:= ToolbarMain.Color;

  PanelCodeTreeTop.Color:= ToolbarMain.Color;
  ToolbarSideTop.Color:= GetAppColor(TAppThemeColor.SideBg);
  ToolbarSideLow.Color:= ToolbarSideTop.Color;
  ToolbarSideMid.Color:= ToolbarSideTop.Color;

  //floating side-panel and bottom-panel
  for id in TAppPanelId do
    if id<>TAppPanelId.None then
      with AppPanels[id] do
      begin
        PanelTitle.Color:= ToolbarMain.Color;
        PanelTitle.Font.Color:= GetAppColor(TAppThemeColor.TabFont);
        Splitter.Color:= GetAppColor(TAppThemeColor.SplitMain);
        Splitter.Invalidate;
        if Assigned(FormFloat) then
          FormFloat.Invalidate;
      end;

  NColor:= GetAppColor(TAppThemeColor.StatusBg);
  if NColor=clNone then
    NColor:= GetAppColor(TAppThemeColor.TabBg);
  Status.Color:= NColor;

  Status.ColorBorderTop:= GetAppColor(TAppThemeColor.ButtonBorderPassive);
  Status.ColorBorderR:= GetAppColor(TAppThemeColor.ButtonBorderPassive);

  ATFlatTheme.ColorFont:= GetAppColor(TAppThemeColor.ButtonFont);
  ATFlatTheme.ColorFontDisabled:= GetAppColor(TAppThemeColor.ButtonFontDisabled);
  ATFlatTheme.ColorFontOverlay:= GetAppColor(TAppThemeColor.SideBadgeFont);
  ATFlatTheme.ColorBgPassive:= GetAppColor(TAppThemeColor.ButtonBgPassive);
  ATFlatTheme.ColorBgOver:= GetAppColor(TAppThemeColor.ButtonBgOver);
  ATFlatTheme.ColorBgChecked:= GetAppColor(TAppThemeColor.ButtonBgChecked);
  ATFlatTheme.ColorBgDisabled:= GetAppColor(TAppThemeColor.ButtonBgDisabled);
  ATFlatTheme.ColorBorderPassive:= GetAppColor(TAppThemeColor.ButtonBorderPassive);
  ATFlatTheme.ColorArrows:= GetAppColor(TAppThemeColor.TabArrow);
  ATFlatTheme.ColorArrowsOver:= GetAppColor(TAppThemeColor.TabArrowOver);
  ATFlatTheme.ColorSeparators:= GetAppColor(TAppThemeColor.TabBorderActive);
  ATFlatTheme.ColorBorderOver:= GetAppColor(TAppThemeColor.ButtonBorderOver);
  ATFlatTheme.ColorBorderFocused:= GetAppColor(TAppThemeColor.ButtonBorderFocused);
  ATFlatTheme.ColorFontListbox:= GetAppColor(TAppThemeColor.ListFont);
  ATFlatTheme.ColorFontListboxSel:= GetAppColor(TAppThemeColor.ListSelFont);
  ATFlatTheme.ColorFontListboxHeader:= GetAppColor(TAppThemeColor.ListFont);
  ATFlatTheme.ColorBgListbox:= GetAppColor(TAppThemeColor.ListBg);
  ATFlatTheme.ColorBgListboxSel:= GetAppColor(TAppThemeColor.ListSelBg);
  ATFlatTheme.ColorBgListboxHottrack:= ColorBlendHalf(ATFlatTheme.ColorBgListbox, ATFlatTheme.ColorBgListboxSel);
  ATFlatTheme.ColorBgListboxHeader:= GetAppColor(TAppThemeColor.ButtonBgDisabled);
  ATFlatTheme.ColorBgOverlay:= GetAppColor(TAppThemeColor.SideBadgeBg);
  //maybe use border colors from editor, not from buttons?
  ATFlatTheme.ColorListboxBorderPassive:= GetAppColor(TAppThemeColor.EdBorder);
  ATFlatTheme.ColorListboxBorderFocused:= GetAppColor(TAppThemeColor.EdBorderFocused);
  //ATFlatTheme.ColorListboxBorderPassive:= GetAppColor(apclButtonBorderPassive);
  //ATFlatTheme.ColorListboxBorderFocused:= GetAppColor(apclButtonBorderFocused);

  ToolbarMain.Invalidate;
  ToolbarSideTop.Invalidate;
  ToolbarSideMid.Invalidate;
  ToolbarSideLow.Invalidate;

  if Assigned(fmConsole) then
    fmConsole.ApplyTheme;

  if Assigned(fmFind) then
    fmFind.ApplyTheme;

  UpdateThemeStatusbar;

  AppApiFlatTheme:= ATFlatTheme;
  AppApiFlatTheme.ColorBgListbox:= GetAppColor(TAppThemeColor.TreeBg);
  AppApiFlatTheme.ColorBgListboxSel:= GetAppColor(TAppThemeColor.TreeSelBg);
  AppApiFlatTheme.ColorFontListbox:= GetAppColor(TAppThemeColor.TreeFont);
  AppApiFlatTheme.ColorFontListboxSel:= GetAppColor(TAppThemeColor.TreeSelFont);

  DoApplyThemeToGroups(Groups);
  if FloatGroups then
  begin
    DoApplyThemeToGroups(GroupsF1);
    DoApplyThemeToGroups(GroupsF2);
    DoApplyThemeToGroups(GroupsF3);
  end;

  ApplyThemeToTreeview(CodeTree.Tree, true, true);
  CodeTree.Invalidate;

  //proc_sidepanel_add_dialog
  for i:= 0 to AppCustomDialogs.Count-1 do
    DoApplyThemeToCustomDialog(TFormDummy(AppCustomDialogs[i]));

  //apply AppTheme to all frames
  DoApplyLexerStylesMapsToFrames(true);

  Groups.SplitterColor:= GetAppColor(TAppThemeColor.SplitGroups);

  CompletionOps.ColorFontPrefix:= GetAppColor(TAppThemeColor.ListCompletePrefix);
  CompletionOps.ColorFontParams:= GetAppColor(TAppThemeColor.ListCompleteParams);

  {$ifdef windows}
  ApplyFormDarkTitle(Self, IsColorDark(GetAppColor(TAppThemeColor.TabBg)), false);
  {$endif}

  DoApplyTheme_ThemedMainMenu;
end;


procedure TfmMain.DoApplyTheme_ThemedMainMenu;
{$ifdef windows}
var
  NColor: TColor;
  NPadding: integer;
  SPadding: string;
  i: integer;
{$endif}
begin
  {$ifdef windows}
  if (UiOps.ThemedMainMenuFontName<>'default') or
    (UiOps.ThemedMainMenuFontSize<>9) then
  begin
    MenuStylerTheme.FontName:= UiOps.ThemedMainMenuFontName;
    MenuStylerTheme.FontSize:= UiOps.ThemedMainMenuFontSize;

    if UiOps.ThemedMainMenuFontSize>9 then
    begin
      //padding is to make MainMenu root items wider,
      //we can also do it by calling RecreateWnd(Self) but it's slow, about 30ms on Intel Core i3
      NPadding:= UiOps.ThemedMainMenuFontSize div 6;
      SPadding:= StringOfChar(' ', NPadding);
      for i:= 0 to MainMenu.Items.Count-1 do
        with MainMenu.Items[i] do
          Caption:= Trim(Caption)+SPadding;
    end;
  end;

  NColor:= GetAppColor(TAppThemeColor.MenuBg);
  if NColor=clNone then
    NColor:= GetAppColor(TAppThemeColor.TabBg);
  MenuStylerTheme.ColorBk:= NColor;

  NColor:= GetAppColor(TAppThemeColor.MenuSelBg);
  if NColor=clNone then
    NColor:= GetAppColor(TAppThemeColor.TabOver);
  MenuStylerTheme.ColorBkSelected:= NColor;

  NColor:= GetAppColor(TAppThemeColor.MenuFont);
  if NColor=clNone then
    NColor:= GetAppColor(TAppThemeColor.TabFont);
  MenuStylerTheme.ColorFont:= NColor;

  NColor:= GetAppColor(TAppThemeColor.MenuFontDisabled);
  if NColor=clNone then
    NColor:= GetAppColor(TAppThemeColor.TabBorderPassive);
  MenuStylerTheme.ColorFontDisabled:= NColor;

  NColor:= GetAppColor(TAppThemeColor.MenuFontHotkey);
  if NColor=clNone then
    NColor:= GetAppColor(TAppThemeColor.MenuFont);
  if NColor=clNone then
    NColor:= GetAppColor(TAppThemeColor.TabFontMod);
  MenuStylerTheme.ColorFontShortcut:= NColor;

  MenuStylerTheme.CharSubmenu:= '>'; //old Win don't have triangle char in default font
  UpdateMenuTheming_MainMenu;
  {$endif}
end;

procedure TfmMain.DoDialogMenuThemes_ThemeSetter(const AThemeUi, AThemeSyntax: string);
begin
  UpdateThemes(
    IfThen(UiOps.ThemeUi<>AThemeUi, AThemeUi, '?'),
    IfThen(UiOps.ThemeSyntax<>AThemeSyntax, AThemeSyntax, '?')
    );
end;

procedure TfmMain.DoDialogMenuThemes;
var
  Form: TfmChooseTheme;
  OldThemeUi, OldThemeSyntax: string;
begin
  Form:= TfmChooseTheme.Create(Self);
  try
    OldThemeUi:= UiOps.ThemeUi;
    OldThemeSyntax:= UiOps.ThemeSyntax;
    Form.ThemeSetter:= @DoDialogMenuThemes_ThemeSetter;

    if Form.ShowModal=mrOK then
      DoOps_SaveThemes
    else
    begin
      UpdateThemes(OldThemeUi, OldThemeSyntax);
    end;
  finally
    Form.Free;
  end;
end;

procedure TfmMain.UpdateThemes(const AThemeUi, AThemeSyntax: string);
var
  bChangeUi,
  bChangeSyntax: boolean;
begin
  bChangeUi:= AThemeUi<>'?';
  bChangeSyntax:= AThemeSyntax<>'?';

  if AThemeUi='' then
    AppThemeInit_UI(AppTheme);
  if AThemeSyntax='' then
    AppThemeInit_Syntax(AppTheme);

  if bChangeUi then
  begin
    UiOps.ThemeUi:= AThemeUi;
    if UiOps.ThemeUi='' then
    begin
    end
    else
      AppThemeLoadFromFile(
        AppDir_DataThemes+DirectorySeparator+UiOps.ThemeUi+AppExtensionThemeUi,
        AppTheme,
        true
        );
  end;

  if bChangeSyntax then
  begin
    UiOps.ThemeSyntax:= AThemeSyntax;
    if UiOps.ThemeSyntax='' then
    begin
    end
    else
      AppThemeLoadFromFile(
        AppDir_DataThemes+DirectorySeparator+UiOps.ThemeSyntax+AppExtensionThemeSyntax,
        AppTheme,
        false
        );
  end;

  if bChangeUi or bChangeSyntax then
  begin
    DoClearLexersAskedList;
    DoApplyTheme;

    if bChangeUi then
      DoPyEvent_AppState(APPSTATE_THEME_UI);

    if bChangeSyntax then
      DoPyEvent_AppState(APPSTATE_THEME_SYNTAX);
  end;
end;


function TfmMain.IsThemeNameExist(const AName: string; AThemeUI: boolean): boolean;
var
  Ext: string;
begin
  if AThemeUI then
    Ext:= AppExtensionThemeUi
  else
    Ext:= AppExtensionThemeSyntax;

  Result:= FileExists(AppDir_DataThemes+DirectorySeparator+AName+Ext);
end;

procedure TfmMain.DoDialogTheme(AThemeUI: boolean);
var
  Str, StrName, StrExt: string;
begin
  if AThemeUI then
  begin
    StrName:= UiOps.ThemeUi;
    StrExt:= AppExtensionThemeUi;
  end
  else
  begin
    if not UiOps.LexerThemes then
      MsgBox(msgStatusSyntaxThemesOff, MB_OK or MB_ICONWARNING);

    StrName:= UiOps.ThemeSyntax;
    StrExt:= AppExtensionThemeSyntax;
  end;

  if DoDialogConfigTheme(AThemeUI) then
  begin
    DoApplyTheme;

    if Msgbox(msgConfirmSaveColorsToFile, MB_OKCANCEL or MB_ICONQUESTION)=ID_OK then
    begin
      Str:= Trim(InputBox(msgTitle, msgThemeName, StrName));
      if Str='' then exit;
      Str:= AppDir_DataThemes+DirectorySeparator+Str+StrExt;

      AppThemeSaveToFile(Str, AppTheme, AThemeUI);
    end;

    if AThemeUI then
      DoPyEvent_AppState(APPSTATE_THEME_UI)
    else
      DoPyEvent_AppState(APPSTATE_THEME_SYNTAX)
  end;
end;

function TfmMain.DoDialogConfigTheme(AThemeUI: boolean): boolean;
var
  Form: TfmColorSetup;
begin
  Form:= TfmColorSetup.Create(nil);
  try
    Form.ThemeUI:= AThemeUI;
    Form.OnApply:= @FormColorsApply;
    Form.Data:= AppTheme;
    Result:= Form.ShowModal=mrOk;
    if Result then
      Form.OnApply(Form.Data, AThemeUI);
  finally
    FreeAndNil(Form);
  end;
end;


