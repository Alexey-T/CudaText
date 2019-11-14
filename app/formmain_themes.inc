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

    if C is TAppTreeContainer then
      if (C as TAppTreeContainer).Themed then
        DoApplyThemeToTreeview((C as TAppTreeContainer).Tree, true, false);

    if C is TATFlatToolbar then
      if (C as TATFlatToolbar).Themed then
        DoApplyThemeToToolbar(C as TATFlatToolbar);
  end;
end;


procedure TfmMain.DoApplyThemeToGroups(G: TATGroups);
begin
  G.SetTabOption(tabColorText, GetAppColor('TabFont'));
  G.SetTabOption(tabColorTextModified, GetAppColor('TabFontMod'));
  G.SetTabOption(tabColorBg, GetAppColor('TabBg'));
  G.SetTabOption(tabColorBgActive, GetAppColor('TabActive'));
  G.SetTabOption(tabColorBgPassive, GetAppColor('TabPassive'));
  G.SetTabOption(tabColorBgPassiveOver, GetAppColor('TabOver'));
  G.SetTabOption(tabColorBorderActive, GetAppColor('TabBorderActive'));
  G.SetTabOption(tabColorBorderPassive, GetAppColor('TabBorderPassive'));
  G.SetTabOption(tabColorSeparator, GetAppColor('TabBorderActive'));
  G.SetTabOption(tabColorArrow, GetAppColor('TabArrow'));
  G.SetTabOption(tabColorArrowOver, GetAppColor('TabArrowOver'));
  G.SetTabOption(tabColorCloseBg, GetAppColor('TabCloseBg'));
  G.SetTabOption(tabColorCloseBgOver, GetAppColor('TabCloseBgOver'));
  G.SetTabOption(tabColorCloseBorderOver, GetAppColor('TabCloseBorderOver'));
  G.SetTabOption(tabColorCloseX, GetAppColor('TabCloseX'));
  G.SetTabOption(tabColorCloseXOver, GetAppColor('TabCloseXOver'));
  G.SetTabOption(tabColorActiveMark, GetAppColor('TabActiveMark'));
  G.SetTabOption(tabColorSpecMarks, GetAppColor('TabMarks'));
  G.Invalidate;
end;

procedure TfmMain.DoApplyTheme;
var
  i: integer;
begin
  with ATScrollbarTheme do
  begin
    ColorBG:= GetAppColor('ScrollBack');
    ColorThumbBorder:= GetAppColor('ScrollRect');
    ColorThumbFill:= GetAppColor('ScrollFill');
    ColorThumbFillOver:= ColorThumbFill;
    ColorThumbFillPressed:= ColorThumbFill;
    ColorThumbDecor:= ColorThumbBorder;
    ColorArrowBorder:= ColorBG;
    ColorArrowFill:= ColorBG;
    ColorArrowFillOver:= ColorArrowFill;
    ColorArrowFillPressed:= ColorArrowFill;
    ColorArrowSign:= GetAppColor('ScrollArrow');
    ColorScrolled:= GetAppColor('ScrollScrolled');
  end;

  ListboxOut.Color:= GetAppColor('ListBg');
  ListboxOut.Invalidate;
  ListboxVal.Color:= ListboxOut.Color;
  ListboxVal.Invalidate;

  EditorApplyTheme(CodeTreeFilterInput);
  EditorApplyTheme(fmConsole.ed);
  EditorApplyTheme(fmConsole.memo);
  if fmConsole.ShowError then
  begin
    fmConsole.memo.Colors.TextFont:= clRed;
    fmConsole.memo.Update;
  end;

  ToolbarMain.Color:= GetAppColor('TabBg');
  Self.Color:= ToolbarMain.Color;

  PanelCodeTreeTop.Color:= ToolbarMain.Color;
  PanelLeftTitle.Color:= ToolbarMain.Color;
  ToolbarSideTop.Color:= GetAppColor('SideBg');
  ToolbarSideLow.Color:= ToolbarSideTop.Color;
  ToolbarSideMid.Color:= ToolbarSideTop.Color;

  Status.Color:= GetAppColor('TabBg');
  Status.ColorBorderTop:= GetAppColor('ButtonBorderPassive');
  Status.ColorBorderR:= GetAppColor('ButtonBorderPassive');

  ATFlatTheme.ColorFont:= GetAppColor('ButtonFont');
  ATFlatTheme.ColorFontDisabled:= GetAppColor('ButtonFontDisabled');
  ATFlatTheme.ColorFontOverlay:= GetAppColor('SideBadgeFont');
  ATFlatTheme.ColorBgPassive:= GetAppColor('ButtonBgPassive');
  ATFlatTheme.ColorBgOver:= GetAppColor('ButtonBgOver');
  ATFlatTheme.ColorBgChecked:= GetAppColor('ButtonBgChecked');
  ATFlatTheme.ColorBgDisabled:= GetAppColor('ButtonBgDisabled');
  ATFlatTheme.ColorBorderPassive:= GetAppColor('ButtonBorderPassive');
  ATFlatTheme.ColorArrows:= GetAppColor('TabArrow');
  ATFlatTheme.ColorArrowsOver:= GetAppColor('TabArrowOver');
  ATFlatTheme.ColorSeparators:= GetAppColor('TabBorderActive');
  ATFlatTheme.ColorBorderOver:= GetAppColor('ButtonBorderOver');
  ATFlatTheme.ColorBorderFocused:= GetAppColor('ButtonBorderFocused');
  ATFlatTheme.ColorFontListbox:= GetAppColor('ListFont');
  ATFlatTheme.ColorFontListboxSel:= GetAppColor('ListSelFont');
  ATFlatTheme.ColorBgListbox:= GetAppColor('ListBg');
  ATFlatTheme.ColorBgListboxSel:= GetAppColor('ListSelBg');
  ATFlatTheme.ColorBgListboxHottrack:= ColorBlendHalf(ATFlatTheme.ColorBgListbox, ATFlatTheme.ColorBgListboxSel);
  ATFlatTheme.ColorBgOverlay:= GetAppColor('SideBadgeBg');

  AppApiFlatTheme:= ATFlatTheme;
  AppApiFlatTheme.ColorBgListbox:= GetAppColor('TreeBg');
  AppApiFlatTheme.ColorBgListboxSel:= GetAppColor('TreeSelBg');
  AppApiFlatTheme.ColorFontListbox:= GetAppColor('TreeFont');
  AppApiFlatTheme.ColorFontListboxSel:= GetAppColor('TreeSelFont');

  DoApplyThemeToGroups(Groups);
  if FloatGroups then
  begin
    DoApplyThemeToGroups(GroupsF1);
    DoApplyThemeToGroups(GroupsF2);
    DoApplyThemeToGroups(GroupsF3);
  end;

  PanelLeftTitle.Font.Color:= GetAppColor('TabFont');

  if assigned(fmFind) then
  begin
    fmFind.Color:= GetAppColor('TabBg');
    fmFind.LabelFind.Font.Color:= GetAppColor('ListFont');
    EditorApplyTheme(fmFind.edFind);
    EditorApplyTheme(fmFind.edRep);
    fmFind.Invalidate;
  end;

  DoApplyThemeToTreeview(CodeTree.Tree, true, true);

  //proc_sidepanel_add_dialog
  for i:= 0 to CustomDialogs.Count-1 do
    DoApplyThemeToCustomDialog(TFormDummy(CustomDialogs[i]));

  //apply AppTheme to all frames
  DoApplyLexerStyleMaps(true);

  SplitterVert.Color:= GetAppColor('SplitMain');
  SplitterHorz.Color:= SplitterVert.Color;
  SplitterHorz.Invalidate;
  SplitterVert.Invalidate;

  Groups.Splitter1.Color:= GetAppColor('SplitGroups');
  Groups.Splitter2.Color:= Groups.Splitter1.Color;
  Groups.Splitter3.Color:= Groups.Splitter1.Color;
  Groups.Splitter4.Color:= Groups.Splitter1.Color;
  Groups.Splitter5.Color:= Groups.Splitter1.Color;
  Groups.Splitter1.Invalidate;
  Groups.Splitter2.Invalidate;
  Groups.Splitter3.Invalidate;
  Groups.Splitter4.Invalidate;
  Groups.Splitter5.Invalidate;

  CompletionOps.ColorFontPrefix:= GetAppColor('ListCompletePrefix');
  CompletionOps.ColorFontParams:= GetAppColor('ListCompleteParams');
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
    DoInitTheme(AppTheme); //resets UI+Syntax
    if SName<>'' then
      SetThemeSyntax(SName);
  end
  else
    DoLoadTheme(
      AppDir_DataThemes+DirectorySeparator+UiOps.ThemeUi+AppExtensionThemeUi,
      AppTheme,
      true,
      @MsgLogConsole
      );

  DoClearLexersAskedList;
  DoApplyTheme;

  DoPyEvent(CurrentEditor, cEventOnState, [IntToStr(APPSTATE_THEME_UI)]);
end;

procedure TfmMain.SetThemeSyntax(const AValue: string);
var
  SName: string;
begin
  UiOps.ThemeSyntax:= AValue;

  if UiOps.ThemeSyntax='' then
  begin
    SName:= UiOps.ThemeUi;
    DoInitTheme(AppTheme); //resets UI+Syntax
    if SName<>'' then
      SetThemeUi(SName);
  end
  else
    DoLoadTheme(
      AppDir_DataThemes+DirectorySeparator+UiOps.ThemeSyntax+AppExtensionThemeSyntax,
      AppTheme,
      false,
      @MsgLogConsole
      );

  DoClearLexersAskedList;
  DoApplyTheme;

  DoPyEvent(CurrentEditor, cEventOnState, [IntToStr(APPSTATE_THEME_SYNTAX)]);
end;


function TfmMain.IsThemeNameExist(const AName: string; AThemeUI: boolean): boolean;
var
  Ext: string;
begin
  if AThemeUI then
    Ext:= AppExtensionThemeUi
  else
    Ext:= AppExtensionThemeSyntax;

  Result:= FileExistsUTF8(AppDir_DataThemes+DirectorySeparator+AName+Ext);
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
    mnuEditPaste.Enabled:= Clipboard.HasFormat(CF_Text);

  if Assigned(mnuEditCopy) then
    mnuEditCopy.Enabled:= Ed.OptCopyLinesIfNoSel or bSel;

  if Assigned(mnuEditCut) then
    mnuEditCut.Enabled:= Ed.OptCutLinesIfNoSel or bSel;

  if Assigned(mnuEditDel) then
    mnuEditDel.Enabled:= bSel;

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

      DoSaveTheme(Str, AppTheme, AThemeUI);
    end;
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
      DoApplyLexerStyleMaps(false);
      UpdateCurrentFrame;
    end;
  finally
    FreeAndNil(Form);
  end;
end;


