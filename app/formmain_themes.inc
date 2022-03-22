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
        DoApplyThemeToTreeview(TAppTreeContainer(C).Tree, true, false);

    if C is TATFlatToolbar then
      if TATFlatToolbar(C).Themed then
        DoApplyThemeToToolbar(TATFlatToolbar(C));
  end;
end;


procedure TfmMain.DoApplyThemeToGroups(G: TATGroups);
begin
  G.SetTabOption(tabColorText, GetAppColor(apclTabFont));
  G.SetTabOption(tabColorTextActive, GetAppColor(apclTabFontActive));
  G.SetTabOption(tabColorTextModified, GetAppColor(apclTabFontMod));
  G.SetTabOption(tabColorBg, GetAppColor(apclTabBg));
  G.SetTabOption(tabColorBgActive, GetAppColor(apclTabActive));
  G.SetTabOption(tabColorBgPassive, GetAppColor(apclTabPassive));
  G.SetTabOption(tabColorBgPassiveOver, GetAppColor(apclTabOver));
  G.SetTabOption(tabColorBorderActive, GetAppColor(apclTabBorderActive));
  G.SetTabOption(tabColorBorderPassive, GetAppColor(apclTabBorderPassive));
  G.SetTabOption(tabColorSeparator, GetAppColor(apclTabBorderActive));
  G.SetTabOption(tabColorArrow, GetAppColor(apclTabArrow));
  G.SetTabOption(tabColorArrowOver, GetAppColor(apclTabArrowOver));
  G.SetTabOption(tabColorCloseBg, GetAppColor(apclTabCloseBg));
  G.SetTabOption(tabColorCloseBgOver, GetAppColor(apclTabCloseBgOver));
  G.SetTabOption(tabColorCloseBorderOver, GetAppColor(apclTabCloseBorderOver));
  G.SetTabOption(tabColorCloseX, GetAppColor(apclTabCloseX));
  G.SetTabOption(tabColorCloseXOver, GetAppColor(apclTabCloseXOver));
  G.SetTabOption(tabColorActiveMark, GetAppColor(apclTabActiveMark));
  G.SetTabOption(tabColorSpecMarks, GetAppColor(apclTabMarks));
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
    ColorBG:= GetAppColor(apclScrollBack);
    ColorBorder:= ColorBG;
    ColorThumbBorder:= GetAppColor(apclScrollRect);
    ColorThumbFill:= GetAppColor(apclScrollFill);
    ColorThumbFillOver:= ColorThumbFill;
    ColorThumbFillPressed:= ColorThumbFill;
    ColorThumbDecor:= ColorThumbBorder;
    ColorArrowBorder:= ColorBG;
    ColorArrowFill:= ColorBG;
    ColorArrowFillOver:= ColorArrowFill;
    ColorArrowFillPressed:= ColorArrowFill;
    ColorArrowSign:= GetAppColor(apclScrollArrow);
    ColorScrolled:= GetAppColor(apclScrollScrolled);
  end;

  EditorApplyTheme(fmOutput.Ed);
  EditorApplyTheme(fmValidate.Ed);

  EditorApplyTheme(CodeTreeFilterInput);
  if Assigned(fmConsole) then
  begin
    EditorApplyTheme(fmConsole.EdInput);
    EditorApplyTheme(fmConsole.EdMemo);
    fmConsole.Invalidate;
  end;

  ToolbarMain.Color:= GetAppColor(apclTabBg);
  Self.Color:= ToolbarMain.Color;

  PanelCodeTreeTop.Color:= ToolbarMain.Color;
  ToolbarSideTop.Color:= GetAppColor(apclSideBg);
  ToolbarSideLow.Color:= ToolbarSideTop.Color;
  ToolbarSideMid.Color:= ToolbarSideTop.Color;

  //floating side-panel and bottom-panel
  for id in TAppPanelId do
    if id<>cPaneNone then
      with AppPanels[id] do
      begin
        PanelTitle.Color:= ToolbarMain.Color;
        PanelTitle.Font.Color:= GetAppColor(apclTabFont);
        Splitter.Color:= GetAppColor(apclSplitMain);
        Splitter.Invalidate;
        if Assigned(FormFloat) then
          FormFloat.Invalidate;
      end;

  NColor:= GetAppColor(apclStatusBg);
  if NColor=clNone then
    NColor:= GetAppColor(apclTabBg);
  Status.Color:= NColor;

  Status.ColorBorderTop:= GetAppColor(apclButtonBorderPassive);
  Status.ColorBorderR:= GetAppColor(apclButtonBorderPassive);

  ATFlatTheme.ColorFont:= GetAppColor(apclButtonFont);
  ATFlatTheme.ColorFontDisabled:= GetAppColor(apclButtonFontDisabled);
  ATFlatTheme.ColorFontOverlay:= GetAppColor(apclSideBadgeFont);
  ATFlatTheme.ColorBgPassive:= GetAppColor(apclButtonBgPassive);
  ATFlatTheme.ColorBgOver:= GetAppColor(apclButtonBgOver);
  ATFlatTheme.ColorBgChecked:= GetAppColor(apclButtonBgChecked);
  ATFlatTheme.ColorBgDisabled:= GetAppColor(apclButtonBgDisabled);
  ATFlatTheme.ColorBorderPassive:= GetAppColor(apclButtonBorderPassive);
  ATFlatTheme.ColorArrows:= GetAppColor(apclTabArrow);
  ATFlatTheme.ColorArrowsOver:= GetAppColor(apclTabArrowOver);
  ATFlatTheme.ColorSeparators:= GetAppColor(apclTabBorderActive);
  ATFlatTheme.ColorBorderOver:= GetAppColor(apclButtonBorderOver);
  ATFlatTheme.ColorBorderFocused:= GetAppColor(apclButtonBorderFocused);
  ATFlatTheme.ColorFontListbox:= GetAppColor(apclListFont);
  ATFlatTheme.ColorFontListboxSel:= GetAppColor(apclListSelFont);
  ATFlatTheme.ColorFontListboxHeader:= GetAppColor(apclListFont);
  ATFlatTheme.ColorBgListbox:= GetAppColor(apclListBg);
  ATFlatTheme.ColorBgListboxSel:= GetAppColor(apclListSelBg);
  ATFlatTheme.ColorBgListboxHottrack:= ColorBlendHalf(ATFlatTheme.ColorBgListbox, ATFlatTheme.ColorBgListboxSel);
  ATFlatTheme.ColorBgListboxHeader:= GetAppColor(apclButtonBgDisabled);
  ATFlatTheme.ColorBgOverlay:= GetAppColor(apclSideBadgeBg);
  //maybe use border colors from editor, not from buttons?
  ATFlatTheme.ColorListboxBorderPassive:= GetAppColor(apclEdBorder);
  ATFlatTheme.ColorListboxBorderFocused:= GetAppColor(apclEdBorderFocused);
  //ATFlatTheme.ColorListboxBorderPassive:= GetAppColor(apclButtonBorderPassive);
  //ATFlatTheme.ColorListboxBorderFocused:= GetAppColor(apclButtonBorderFocused);

  UpdateThemeStatusbar;

  AppApiFlatTheme:= ATFlatTheme;
  AppApiFlatTheme.ColorBgListbox:= GetAppColor(apclTreeBg);
  AppApiFlatTheme.ColorBgListboxSel:= GetAppColor(apclTreeSelBg);
  AppApiFlatTheme.ColorFontListbox:= GetAppColor(apclTreeFont);
  AppApiFlatTheme.ColorFontListboxSel:= GetAppColor(apclTreeSelFont);

  DoApplyThemeToGroups(Groups);
  if FloatGroups then
  begin
    DoApplyThemeToGroups(GroupsF1);
    DoApplyThemeToGroups(GroupsF2);
    DoApplyThemeToGroups(GroupsF3);
  end;

  if Assigned(fmFind) then
  begin
    fmFind.Color:= GetAppColor(apclTabBg);
    EditorApplyTheme(fmFind.edFind);
    EditorApplyTheme(fmFind.edRep);
    fmFind.Invalidate;
  end;

  DoApplyThemeToTreeview(CodeTree.Tree, true, true);
  CodeTree.Invalidate;

  //proc_sidepanel_add_dialog
  for i:= 0 to AppCustomDialogs.Count-1 do
    DoApplyThemeToCustomDialog(TFormDummy(AppCustomDialogs[i]));

  //apply AppTheme to all frames
  DoApplyLexerStylesMapsToFrames(true);

  Groups.SplitterColor:= GetAppColor(apclSplitGroups);

  CompletionOps.ColorFontPrefix:= GetAppColor(apclListCompletePrefix);
  CompletionOps.ColorFontParams:= GetAppColor(apclListCompleteParams);

  DoApplyTheme_ThemedMainMenu;
end;


procedure TfmMain.DoApplyTheme_ThemedMainMenu;
{$ifdef windows}
var
  NColor: TColor;
{$endif}
begin
  {$ifdef windows}
  MenuStylerTheme.FontName:= UiOps.ThemedMainMenuFontName;
  MenuStylerTheme.FontSize:= UiOps.ThemedMainMenuFontSize;

  NColor:= GetAppColor(apclMenuBg);
  if NColor=clNone then
    NColor:= GetAppColor(apclTabBg);
  MenuStylerTheme.ColorBk:= NColor;

  NColor:= GetAppColor(apclMenuSelBg);
  if NColor=clNone then
    NColor:= GetAppColor(apclTabOver);
  MenuStylerTheme.ColorBkSelected:= NColor;

  NColor:= GetAppColor(apclMenuFont);
  if NColor=clNone then
    NColor:= GetAppColor(apclTabFont);
  MenuStylerTheme.ColorFont:= NColor;

  NColor:= GetAppColor(apclMenuFontDisabled);
  if NColor=clNone then
    NColor:= GetAppColor(apclTabBorderPassive);
  MenuStylerTheme.ColorFontDisabled:= NColor;

  NColor:= GetAppColor(apclMenuFontHotkey);
  if NColor=clNone then
    NColor:= GetAppColor(apclMenuFont);
  if NColor=clNone then
    NColor:= GetAppColor(apclTabFontMod);
  MenuStylerTheme.ColorFontShortcut:= NColor;

  MenuStylerTheme.CharSubmenu:= '>'; //old Win don't have triangle char in default font
  UpdateMenuTheming_MainMenu(true);
  {$endif}
end;

procedure TfmMain.DoDialogMenuThemes_ThemeUiSelect(const AStr: string);
begin
  ThemeUi:= AStr;
end;

procedure TfmMain.DoDialogMenuThemes_ThemeSyntaxSelect(const AStr: string);
begin
  ThemeSyntax:= AStr;
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
    Form.ThemeUiSetter:= @DoDialogMenuThemes_ThemeUiSelect;
    Form.ThemeSyntaxSetter:= @DoDialogMenuThemes_ThemeSyntaxSelect;

    if Form.ShowModal=mrOK then
      DoOps_SaveThemes
    else
    begin
      ThemeUi:= OldThemeUi;
      ThemeSyntax:= OldThemeSyntax;
    end;
  finally
    Form.Free;
  end;
end;

procedure TfmMain.SetThemeUi(const AValue: string);
var
  SName: string;
begin
  UiOps.ThemeUi:= AValue;

  if UiOps.ThemeUi='' then
  begin
    SName:= UiOps.ThemeSyntax;
    AppThemeInit(AppTheme); //resets UI+Syntax
    if SName<>'' then
      SetThemeSyntax(SName);
  end
  else
    AppThemeLoadFromFile(
      AppDir_DataThemes+DirectorySeparator+UiOps.ThemeUi+AppExtensionThemeUi,
      AppTheme,
      true
      );

  DoClearLexersAskedList;
  DoApplyTheme;

  DoPyEvent_AppState(APPSTATE_THEME_UI);
end;

procedure TfmMain.SetThemeSyntax(const AValue: string);
var
  SName: string;
begin
  UiOps.ThemeSyntax:= AValue;

  if UiOps.ThemeSyntax='' then
  begin
    SName:= UiOps.ThemeUi;
    AppThemeInit(AppTheme); //resets UI+Syntax
    if SName<>'' then
      SetThemeUi(SName);
  end
  else
    AppThemeLoadFromFile(
      AppDir_DataThemes+DirectorySeparator+UiOps.ThemeSyntax+AppExtensionThemeSyntax,
      AppTheme,
      false
      );

  DoClearLexersAskedList;
  DoApplyTheme;

  DoPyEvent_AppState(APPSTATE_THEME_SYNTAX);
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

procedure TfmMain.mnuEditClick(Sender: TObject);
var
  Ed: TATSynEdit;
  bSel: boolean;
begin
  Ed:= CurrentEditor;
  if Ed=nil then exit;

  bSel:= Ed.Carets.IsSelection;

  if Assigned(mnuEditUndo) then
    mnuEditUndo.Enabled:= not Ed.Strings.UndoEmpty;

  if Assigned(mnuEditRedo) then
    mnuEditRedo.Enabled:= not Ed.Strings.RedoEmpty;

  if Assigned(mnuEditPaste) then
  begin
    mnuEditPaste.Enabled:= Clipboard.HasFormat(CF_Text);
    if Assigned(mnuEditPasteIndent) then
      mnuEditPasteIndent.Enabled:= mnuEditPaste.Enabled;
    if Assigned(mnuEditPasteHist) then
      mnuEditPasteHist.Enabled:= Assigned(ATEditorClipboardRecents) and (ATEditorClipboardRecents.Count>0);
  end;

  if Assigned(mnuEditCopy) then
    mnuEditCopy.Enabled:= Ed.OptCopyLinesIfNoSel or bSel;

  if Assigned(mnuEditCut) then
    mnuEditCut.Enabled:= Ed.OptCutLinesIfNoSel or bSel;

  if Assigned(mnuEditCopyAppend) then
    mnuEditCopyAppend.Enabled:= bSel;
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

  if DoDialogConfigTheme(AppTheme, AThemeUI) then
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

function TfmMain.DoDialogConfigTheme(var AData: TAppTheme; AThemeUI: boolean): boolean;
var
  Form: TfmColorSetup;
begin
  Form:= TfmColorSetup.Create(nil);
  try
    Form.PanelUi.Visible:= AThemeUI;
    Form.PanelSyntax.Visible:= not AThemeUI;

    Form.OnApply:= @FormColorsApply;
    Form.Data:= AData;
    Result:= Form.ShowModal=mrOk;

    if Result then
    begin
      AData:= Form.Data;
      DoApplyLexerStylesMapsToFrames(false);
    end;
  finally
    FreeAndNil(Form);
  end;
end;


