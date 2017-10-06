{$ifdef nnnn}begin end;{$endif}

procedure DoApplyThemeToCustomDialog(F: TFormDummy);
var
  C: TControl;
  i: integer;
begin
  for i:= 0 to F.ControlCount-1 do
  begin
    C:= F.Controls[i];

    if C is TATListbox then
      if (C as TATListbox).ThemedColors then
        DoApplyThemeToListbox(C as ATListbox.TATListbox);

    if C is TTreeViewMy then
      if (C as TTreeViewMy).Themed then
        DoApplyThemeToTreeview(C as TTreeview, true, false);

    if C is TATButtonsToolbar then
      if (C as TATButtonsToolbar).Themed then
        DoApplyThemeToToolbar(C as TATButtonsToolbar);
  end;
end;

procedure TfmMain.DoApplyTheme;
var
  F: TEditorFrame;
  //PropObject: TAppPanelPropsClass;
  i: integer;
begin
  UpdateMenuThemes(true);
  UpdateMenuThemes(false);

  //we have 2 copies of ATScrollbar unit, need to update same 2 records
  //unit#1:
  with ATSynEdit_ScrollBar.ATScrollbarTheme do
  begin
    ColorBG:= GetAppColor('ScrollBack');
    ColorThumbBorder:= GetAppColor('ScrollRect');
    ColorThumbFill:= GetAppColor('ScrollFill');
    ColorArrowBorder:= ATScrollbarTheme.ColorBG;
    ColorArrowFill:= ATScrollbarTheme.ColorBG;
    ColorArrowSign:= GetAppColor('ScrollArrow');
    ColorScrolled:= GetAppColor('ScrollScrolled');
  end;

  //unit#2:
  Assert(
    SizeOf(ATSynEdit_Scrollbar.ATScrollbarTheme) =
    SizeOf(ATScrollbar.ATScrollbarTheme), 'Not equal ATScrollbarTheme vars in 2 units');
  Move(
    ATSynEdit_Scrollbar.ATScrollbarTheme,
    ATScrollbar.ATScrollbarTheme,
    SizeOf(ATScrollbar.ATScrollbarTheme));

  ListboxOut.Color:= GetAppColor('ListBg');
  ListboxOut.Invalidate;
  ListboxVal.Color:= ListboxOut.Color;
  ListboxVal.Invalidate;

  for i:= 0 to 9 do
    AppBookmarkSetup[240+i].Color:= GetAppColor('EdBookmarkBg');

  EditorApplyTheme(fmConsole.ed);
  EditorApplyTheme(fmConsole.memo);

  ToolbarMain.Color:= GetAppColor('TabBg');
  Self.Color:= ToolbarMain.Color;

  PanelLeftTitle.Color:= ToolbarMain.Color;
  ToolbarSideTop.Color:= GetAppColor('SideBg');
  ToolbarSideLow.Color:= ToolbarSideTop.Color;
  ToolbarSideMid.Color:= ToolbarSideTop.Color;

  Status.Font.Color:= GetAppColor('StatusFont');
  Status.Color:= GetAppColor('StatusBg');
  Status.ColorBorderTop:= GetAppColor('StatusLines');
  Status.ColorBorderR:= GetAppColor('StatusLines');

  StatusAlt.Font.Color:= GetAppColor('StatusAltFont');
  StatusAlt.Color:= GetAppColor('StatusAltBg');
  StatusAlt.ColorBorderTop:= Status.ColorBorderTop;
  StatusAlt.ColorBorderR:= Status.ColorBorderR;

  StatusProgress.Font.Color:= Status.Font.Color;
  StatusProgress.ForeColor:= GetAppColor('GaugeFill');
  StatusProgress.BackColor:= GetAppColor('GaugeBg');
  StatusProgress.BorderColor:= GetAppColor('ButtonBorderPassive');

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
  Groups.SetTabOption(tabColorActiveMark, GetAppColor('TabActiveMark'));
  Groups.SetTabOption(tabColorSpecMarks, GetAppColor('TabMarks'));
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

  DoApplyThemeToTreeview(Tree, true, true);

  //proc_sidepanel_add_dialog
  for i:= 0 to CustomDialogs.Count-1 do
    DoApplyThemeToCustomDialog(TFormDummy(CustomDialogs[i]));

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

  CompletionOps.ColorBg:= GetAppColor('ListBg');
  CompletionOps.ColorSelBg:= GetAppColor('ListSelBg');
  CompletionOps.ColorFont[0]:= GetAppColor('ListCompletePrefix');
  CompletionOps.ColorFont[1]:= GetAppColor('ListFont');
  CompletionOps.ColorFont[2]:= GetAppColor('ListCompleteParams');
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
  if AThemeUI then
  begin
    mi.OnClick:= @MenuThemeDefaultUiClick;
    mi.Checked:= FThemeUi='';
    ListThemes:= FListThemesUI;
    FileMask:= '*'+AppExtensionThemeUi;
  end
  else
  begin
    mi.OnClick:= @MenuThemeDefaultSyntaxClick;
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
    DoLoadTheme(GetAppPath(cDirDataThemes)+DirectorySeparator+FThemeUi+AppExtensionThemeUi, AppTheme, true);

  DoClearLexersAskedList;
  DoApplyTheme;
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
    DoLoadTheme(GetAppPath(cDirDataThemes)+DirectorySeparator+FThemeSyntax+AppExtensionThemeSyntax, AppTheme, false);

  DoClearLexersAskedList;
  DoApplyTheme;
end;

procedure TfmMain.MenuThemeDefaultUiClick(Sender: TObject);
begin
  ThemeUi:= '';

  if MsgBox(msgConfirmSyntaxThemeSameName, MB_OKCANCEL+MB_ICONQUESTION)=ID_OK then
    ThemeSyntax:= '';
end;

procedure TfmMain.MenuThemeDefaultSyntaxClick(Sender: TObject);
begin
  ThemeSyntax:= '';
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
  if IsThemeNameExist(ThemeUi, false) then
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


