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
  //PropObject: TAppPanelPropsClass;
  i: integer;
begin
  with ATScrollbarTheme do
  begin
    ColorBG:= GetAppColor('ScrollBack');
    ColorThumbBorder:= GetAppColor('ScrollRect');
    ColorThumbFill:= GetAppColor('ScrollFill');
    ColorArrowBorder:= ATScrollbarTheme.ColorBG;
    ColorArrowFill:= ATScrollbarTheme.ColorBG;
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

  StatusAlt.Color:= GetAppColor('StatusAltBg');
  StatusAlt.ColorBorderTop:= Status.ColorBorderTop;
  StatusAlt.ColorBorderR:= Status.ColorBorderR;

  ATFlatTheme.ColorFont:= GetAppColor('ButtonFont');
  ATFlatTheme.ColorFontDisabled:= GetAppColor('ButtonFontDisabled');
  ATFlatTheme.ColorFontOverlay:= GetAppColor('SideBadgeFont');
  ATFlatTheme.ColorBgPassive:= GetAppColor('ButtonBgPassive');
  ATFlatTheme.ColorBgOver:= GetAppColor('ButtonBgOver');
  ATFlatTheme.ColorBgChecked:= GetAppColor('ButtonBgChecked');
  ATFlatTheme.ColorBgDisabled:= GetAppColor('ButtonBgDisabled');
  ATFlatTheme.ColorBorderPassive:= GetAppColor('ButtonBorderPassive');
  ATFlatTheme.ColorArrows:= GetAppColor('TabArrow');
  ATFlatTheme.ColorSeparators:= GetAppColor('TabBorderActive');
  ATFlatTheme.ColorBorderOver:= GetAppColor('ButtonBorderOver');
  ATFlatTheme.ColorBorderFocused:= GetAppColor('ButtonBorderFocused');
  ATFlatTheme.ColorFontListbox:= GetAppColor('ListFont');
  ATFlatTheme.ColorFontListboxSel:= GetAppColor('ListSelFont');
  ATFlatTheme.ColorBgListbox:= GetAppColor('ListBg');
  ATFlatTheme.ColorBgListboxSel:= GetAppColor('ListSelBg');
  ATFlatTheme.ColorBgOverlay:= GetAppColor('SideBadgeBg');

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


procedure TfmMain.DoDialogMenuThemes(AThemeUI: boolean);
var
  SMask, SCaption: string;
  ListFiles, ListItems: TStringList;
  NResult, i: integer;
begin
  if AThemeUI then
  begin
    SMask:= '*'+AppExtensionThemeUi;
    SCaption:= msgMenuThemesUI;
  end
  else
  begin
    SMask:= '*'+AppExtensionThemeSyntax;
    SCaption:= msgMenuThemesSyntax;
  end;

  ListItems:= TStringList.Create;
  ListFiles:= TStringList.Create;
  try
    FFindFilesInDir(GetAppPath(cDirDataThemes), SMask, ListFiles);
    if ListFiles.Count=0 then exit;
    ListFiles.Sort;

    ListItems.Add(msgThemeDefault);
    for i:= 0 to ListFiles.Count-1 do
      ListItems.Add(LowerCase(ExtractFileNameOnly(ListFiles[i])));

    NResult:= DoDialogMenuList(SCaption, ListItems);
    if NResult<0 then exit;

    if AThemeUI then
    begin
      if NResult=0 then
      begin
        ThemeUi:= '';
        if MsgBox(msgConfirmSyntaxThemeSameName, MB_OKCANCEL+MB_ICONQUESTION)=ID_OK then
          ThemeSyntax:= '';
      end
      else
      begin
        ThemeUi:= ListItems[NResult];
        //find syntax-theme with the same name
        if IsThemeNameExist(ThemeUi, false) then
          if MsgBox(msgConfirmSyntaxThemeSameName, MB_OKCANCEL+MB_ICONQUESTION)=ID_OK then
            ThemeSyntax:= ThemeUi;
      end;
    end
    else
    begin
      if NResult=0 then
      begin
        ThemeSyntax:= '';
      end
      else
      begin
        ThemeSyntax:= ListItems[NResult];
      end;
    end;
  finally
    FreeAndNil(ListItems);
    FreeAndNil(ListFiles);
  end;
end;


procedure TfmMain.SetThemeUi(const AValue: string);
var
  SName: string;
begin
  FThemeUi:= AValue;

  if FThemeUi='' then
  begin
    SName:= FThemeSyntax;
    DoInitTheme(AppTheme); //resets UI+Syntax
    if SName<>'' then
      SetThemeSyntax(SName);
  end
  else
    DoLoadTheme(
      GetAppPath(cDirDataThemes)+DirectorySeparator+FThemeUi+AppExtensionThemeUi,
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
  FThemeSyntax:= AValue;

  if FThemeSyntax='' then
  begin
    SName:= FThemeUi;
    DoInitTheme(AppTheme); //resets UI+Syntax
    if SName<>'' then
      SetThemeUi(SName);
  end
  else
    DoLoadTheme(
      GetAppPath(cDirDataThemes)+DirectorySeparator+FThemeSyntax+AppExtensionThemeSyntax,
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

  Result:= FileExistsUTF8(GetAppPath(cDirDataThemes)+DirectorySeparator+AName+Ext);
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
    StrName:= ThemeUi;
    StrExt:= AppExtensionThemeUi;
  end
  else
  begin
    if not UiOps.LexerThemes then
      MsgBox(msgStatusSyntaxThemesOff, MB_OK or MB_ICONWARNING);

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

    DoLocalize_FormColorSetup(Form);
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


