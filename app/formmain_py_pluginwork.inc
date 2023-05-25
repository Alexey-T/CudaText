{$ifdef nn}begin end;{$endif}

function IsPluginBad(const ADirName: string; out AShowWarning: boolean): boolean;
begin
  AShowWarning:= false;

  case ADirName of
    'cuda_tree', //replaced with built-in tree-helper engine
    'cuda_tree_markdown', //rewritten in Pascal
    'cuda_tree_mediawiki', //rewritten in Pascal
    'cuda_tree_rest', //rewritten in Pascal
    'cuda_auto_copy', //replaced with built-in code
    'cuda_brackets_hilite': //replaced with built-in code
    begin
      Result:= true;
      AShowWarning:= true;
    end
    else
      Result:= false;
  end;

  if not Result then
    if SBeginsWith(ADirName, 'cuda_lint_') or //sub-plugins of CudaLinter
      SBeginsWith(ADirName, 'cuda_fmt_') //sub-plugins of CudaFormatter
      then
        Result:= true;
end;


procedure TfmMain.DoOps_LoadPlugins(AKeepHotkeys: boolean);
var
  ListFiles: TStringlist;
  SItem, SFilenameInf, SFilenameDisabled, DirName: string;
  IniPlugins: TMemIniFile;
  BackupMain,
  BackupTemp: TAppHotkeyBackup;
  BackupLexers: TFPList;
  bShowIgnoredWarning: boolean;
  i: integer;
begin
  if not AppPython.Inited then exit;

  if not DirectoryExists(AppDir_Py) then
  begin
    MsgLogConsole(Format(msgCannotFindData, [AppDir_Py]));
    MsgStdout(Format(msgCannotFindData, [AppDir_Py]));
    exit
  end;

  BackupMain:= nil;
  BackupTemp:= nil;
  BackupLexers:= nil;

  if AKeepHotkeys then
  begin
    BackupMain:= TAppHotkeyBackup.Create;
    BackupLexers:= TFPList.Create;

    //save hotkey-backup of main keymap
    TKeymapHelperMain.DeleteCategoryWithHotkeyBackup(AppKeymapMain, BackupMain, categ_Plugin);

    //save hotkey-backup of lexers keymaps
    for i:= 0 to AppKeymapLexers.Count-1 do
    begin
      BackupTemp:= TAppHotkeyBackup.Create;
      BackupLexers.Add(BackupTemp);
      TKeymapHelperMain.DeleteCategoryWithHotkeyBackup(AppKeymapLexers.Objects[i] as TATKeymap, BackupTemp, categ_Plugin);
    end;
  end;

  //commands: don't clear all, leave items from API
  TPluginHelper.CommandsClearButKeepApiItems;

  //events: clear all
  AppEventList.Clear;

  ListFiles:= TStringlist.Create;

  if FileExists(AppFile_PluginsIni) then
    IniPlugins:= TMemIniFile.Create(AppFile_PluginsIni)
  else
    IniPlugins:= nil;

  try
    FindAllDirectories(ListFiles, AppDir_Py, false);
    ListFiles.Sort;
    for SItem in ListFiles do
    begin
      DirName:= ExtractFileName(SItem);
      if IsPluginBad(DirName, bShowIgnoredWarning) then
      begin
        if bShowIgnoredWarning then
          MsgLogConsole(Format(msgErrorPluginIgnored, [DirName]));
        Continue;
      end;

      SFilenameInf:= SItem+DirectorySeparator+'install.inf';
      SFilenameDisabled:= SItem+DirectorySeparator+'plugin_disabled';
      if FileExists(SFilenameInf) and not FileExists(SFilenameDisabled) then
        DoOps_LoadPluginFromInf(SFilenameInf, IniPlugins);
    end;

    if AKeepHotkeys then
    begin
      //restore hotkey-backup of main keymap
      TKeymapHelperMain.AddPluginsWithHotkeyBackup(AppKeymapMain, BackupMain, categ_Plugin);

      //restore hotkey-backup of lexers keymaps
      for i:= 0 to AppKeymapLexers.Count-1 do
      begin
        BackupTemp:= TAppHotkeyBackup(BackupLexers[i]);
        TKeymapHelperMain.AddPluginsWithHotkeyBackup(AppKeymapLexers.Objects[i] as TATKeymap, BackupTemp, categ_Plugin);
      end;
    end;

  finally
    FreeAndNil(IniPlugins);
    FreeAndNil(ListFiles);

    if Assigned(BackupLexers) then
    begin
      for i:= 0 to BackupLexers.Count-1 do
        TObject(BackupLexers[i]).Free;
      FreeAndNil(BackupLexers);
    end;
    if Assigned(BackupMain) then
      FreeAndNil(BackupMain);
  end;

  TPluginHelper.EventsMaxPrioritiesUpdate;
  TKeymapHelperMain.UpdateDynamic(categ_Plugin);
  TKeymapHelperMain.UpdateDynamic(categ_PluginSub);
end;


procedure TfmMain.DoOps_LoadPluginFromInf(const fn_inf: string;
  IniPlugins: TMemIniFile);
type
  TAppInfSection = (
    sidNone,
    sidItem,
    sidTreeHelper,
    sidSidebar,
    sidBottombar
    );
var
  ini: TMemIniFile;
  sections: TStringList;
  sectionId: TAppInfSection;
  ini_section, dir_inf: string;
  s_itemkind, s_caption, s_module, s_method, s_lexers,
  s_events, s_keys, s_inmenu, s_icon, s_os: string;
  CmdItem: TAppCommandInfo;
  EventItem: TAppEventInfo;
  TreeHelper: TAppTreeHelper;
  PanelId: TAppPanelId;
begin
  dir_inf:= ExtractFileDir(fn_inf);
  s_module:= ExtractFileName(dir_inf);

  ini:= TMemIniFile.Create(fn_inf);
  sections:= TStringList.Create;
  try
    //ignore plugin for unsupported OS
    s_os:= ini.ReadString('info', 'os', '');
    if not CheckValue_OS(s_os) then exit;

    ini.ReadSections(sections);

    for ini_section in sections do
    begin
      if SBeginsWith(ini_section, 'item') then
        sectionId:= sidItem
      else
      if SBeginsWith(ini_section, 'treehelper') then
        sectionId:= sidTreeHelper
      else
      if SBeginsWith(ini_section, 'sidebar') then
        sectionId:= sidSidebar
      else
      if SBeginsWith(ini_section, 'bottombar') then
        sectionId:= sidBottombar
      else
        sectionId:= sidNone;

      if (sectionId=sidSidebar) or
        (sectionId=sidBottombar) then
      begin
        s_caption:= ini.ReadString(ini_section, 'hint', '');
        if s_caption='' then Continue;

        s_icon:= ini.ReadString(ini_section, 'icon', '');
        if s_icon='' then Continue;
        s_icon:= StringReplace(s_icon, '{dir}', dir_inf, []);
        {$ifdef windows}
        s_icon:= StringReplace(s_icon, '/', '\', [rfReplaceAll]);
        {$endif}

        s_method:= ini.ReadString(ini_section, 'method', '');
        if s_method='' then Continue;

        case sectionId of
          sidSidebar:
            PanelId:= cPaneSide;
          sidBottombar:
            PanelId:= cPaneOut;
          else
            Continue;
        end;

        AppPanels[PanelId].AddEmpty(
          s_caption,
          DoSidebar_FilenameToImageIndex(s_caption, s_icon),
          s_module,
          s_method
          );
      end
      else
      if sectionId=sidTreeHelper then
      begin
        s_method:= ini.ReadString(ini_section, 'method', '');
        s_lexers:= ini.ReadString(ini_section, 'lexers', '');
        TreeHelper:= TAppTreeHelper.Create;
        TreeHelper.ItemModule:= s_module;
        TreeHelper.ItemProc:= s_method;
        TreeHelper.ItemLexers:= s_lexers;
        AppTreeHelpers.Add(TreeHelper);
      end
      else
      if sectionId=sidItem then
      begin
        s_itemkind:= ini.ReadString(ini_section, 'section', '');
        s_caption:= ini.ReadString(ini_section, 'caption', '');
        s_method:= ini.ReadString(ini_section, 'method', '');
        //s_hotkey:= ini.ReadString(ini_section, 'hotkey', '');

        s_lexers:= ini.ReadString(ini_section, 'lexers', '');
        if SBeginsWith(s_lexers, '$') then //var $name defined in [info]
          s_lexers:= ini.ReadString('info', s_lexers, '');

        s_events:= ini.ReadString(ini_section, 'events', '');
        s_keys:= ini.ReadString(ini_section, 'keys', '');
        s_inmenu:= ini.ReadString(ini_section, 'menu', '');

        case s_itemkind of
          'commands':
            begin
              if s_caption='' then Continue;
              if s_method='' then Continue;

              CmdItem:= TAppCommandInfo.Create;
              CmdItem.ItemModule:= s_module;
              CmdItem.ItemProc:= s_method;
              CmdItem.ItemProcParam:= '';
              CmdItem.ItemCaption:= s_caption;
              CmdItem.ItemLexers:= s_lexers;
              CmdItem.ItemInMenu:= s_inmenu;

              if AppCommandList.Count>(cmdLastPluginCommand-cmdFirstPluginCommand) then
                MsgLogConsole('ERROR: Too many plugin commands');
              AppCommandList.Add(CmdItem);
            end;
          'events':
            begin
              if s_events='' then Continue;

              EventItem:= TAppEventInfo.Create;
              EventItem.ItemModule:= s_module;
              TPluginHelper.EventStringToEventData(s_events,
                EventItem.ItemEvents,
                EventItem.ItemEventsPrior,
                EventItem.ItemEventsLazy
                );
              EventItem.ItemLexers:= s_lexers;
              EventItem.ItemKeys:= s_keys;
              AppEventList.Add(EventItem);
            end;
        end;
      end;
    end;
  finally
    FreeAndNil(sections);
    FreeAndNil(ini);
  end;

  //add events from plugins.ini
  if Assigned(IniPlugins) then
  begin
    s_events:= IniPlugins.ReadString('events', s_module, '');
    if s_events<>'' then
    begin
      EventItem:= TAppEventInfo.Create;
      EventItem.ItemModule:= s_module;
      TPluginHelper.EventStringToEventData(s_events,
        EventItem.ItemEvents,
        EventItem.ItemEventsPrior,
        EventItem.ItemEventsLazy
        );
      AppEventList.Add(EventItem);
    end;
  end;
end;


procedure TfmMain.DoOps_ShowEventPlugins;
var
  ev: TAppPyEvent;
  s, s2: string;
  i: integer;
begin
  exit; //this is debug procedure

  s:= '';
  for i:= 0 to Min(AppEventList.Count-1, 20) do
    with TAppEventInfo(AppEventList[i]) do
    begin
      s2:= '';
      for ev in TAppPyEvent do
        if ev in ItemEvents then
          s2:= s2+'/'+cAppPyEvent[ev];

      s:= s+#13+
        'module: '+ItemModule+#13+
        'lexers: "'+ItemLexers+'"'#13+
        'keys: "'+ItemKeys+'"'#13+
        'events: '+s2+#13;
    end;
  ShowMessage(s);
end;


procedure TfmMain.DoOps_AddPluginMenuItem(const ACaption: string;
  ASubMenu: TMenuItem; ALangFile: TIniFile; ATag: integer);
var
  mi: TMenuItem;
  Sep: TATStringSeparator;
  strItem, strLocal: string;
  idx, NInsert: integer;
  bRoot: boolean;
begin
  mi:= nil;
  bRoot:= true;

  //need to parse caption and create subitems, separated with '\'
  Sep.Init(ACaption, '\');
  repeat
    if not Sep.GetItemStr(strItem) then Break;

    //translate items using files data/langmenu/cuda_nnnnn/ru_RU.ini
    if strItem<>'-' then
      if Assigned(ALangFile) then
      begin
        strLocal:= ALangFile.ReadString('menu', strItem, '');
        if strLocal<>'' then
          strItem:= strLocal;
      end;

    if strItem='-' then
      idx:= -1
    else
      idx:= ASubMenu.IndexOfCaption(strItem);
    if idx<0 then
    begin
      mi:= TMenuItem.Create(Self);
      mi.Caption:= strItem;

      if bRoot then
      begin
        NInsert:= Menu_GetIndexToInsert(ASubMenu, strItem);
        if NInsert>=0 then
          ASubMenu.Insert(NInsert, mi)
        else
          ASubMenu.Add(mi);
      end
      else
        ASubMenu.Add(mi);

      ASubMenu:= mi;
    end
    else
    begin
      ASubMenu:= ASubMenu.Items[idx];
      bRoot:= false;
    end;
  until false;

  if Assigned(mi) then
  begin
    mi.OnClick:= @MenuPluginClick;
    mi.Tag:= ATag;
  end;
end;

procedure TfmMain.UpdateMenuPlugins;
  //
  function GroupValue(const AValue: string): string;
  var
    SName, SAll, SRes: string;
  begin
    Result:= '';
    SSplitByChar(AValue, '\', SName, SAll);

    //strip '&' for accelerators in caption
    SName:= StringReplace(SName, '&', '', []);

    SRes:= AppConfig_PGroups.GetValueByRegex(SName, false);
    if SRes<>'' then
      Result:= SRes+'\';
  end;
  //
var
  SCaption, SInMenu: string;
  LangFN, LastModule: string;
  CmdItem: TAppCommandInfo;
  LangFile: TIniFile;
  i: integer;
begin
  if not AppPython.Inited then exit;
  if mnuPlugins=nil then exit;

  LastModule:= '';
  LangFile:= nil;

  mnuPlugins.Clear;
  if Assigned(mnuOpPlugins) then
    mnuOpPlugins.Clear;

  for i:= 0 to AppCommandList.Count-1 do
  begin
    CmdItem:= TAppCommandInfo(AppCommandList[i]);
    if CmdItem.ItemModule='' then Break;
    if CmdItem.ItemFromApi then Continue;

    SCaption:= CmdItem.ItemCaption;
    SInMenu:= CmdItem.ItemInMenu;
    if SInMenu='0' then Continue;

    //translate menu items using files data/langmenu/cuda_nnnnn/ru_RU.ini
    if LastModule<>CmdItem.ItemModule then
    begin
      LastModule:= CmdItem.ItemModule;

      LangFN:= '';
      if UiOps.LangName<>'' then
        LangFN:= AppDir_Data+DirectorySeparator+
                 'langmenu'+DirectorySeparator+
                 CmdItem.ItemModule+DirectorySeparator+
                 UiOps.LangName+'.ini';

      if Assigned(LangFile) then
        FreeAndNil(LangFile);

      if (LangFN<>'') and FileExists(LangFN) then
      begin
        //ShowMessage('create ini: '+LangFN);
        LangFile:= TMemIniFile.Create(LangFN
          {$if FPC_FULLVERSION>=30200}, TEncoding.UTF8 {$endif});
      end;
    end;

    //add to Plugins
    if (SInMenu='') or (Pos('p', SInMenu)>0) then
      DoOps_AddPluginMenuItem(GroupValue(SCaption)+SCaption, mnuPlugins, LangFile, i);

    //add to Settings-plugins
    if Assigned(mnuOpPlugins) then
    if Pos('o', SInMenu)>0 then
      DoOps_AddPluginMenuItem(SCaption, mnuOpPlugins, LangFile, i);
  end;

  UpdateMenuTheming_MainMenu;
end;

procedure TfmMain.UpdateMenuPlugins_Shortcuts(AForceUpdate: boolean = false);
begin
  //real work will run in OnIdleTimer (it's rather slow)
  FInvalidateShortcuts:= true;
  FInvalidateShortcutsForce:= AForceUpdate;
end;

procedure TfmMain.UpdateMenuPlugins_Shortcuts_Work(AForceUpdate: boolean);
  //
  procedure UpdMenu(AMenu: TMenuItem; AKeymap: TATKeymap);
  var
    miSub: TMenuItem;
    i: integer;
  begin
    for i:= 0 to AMenu.Count-1 do
    begin
      miSub:= AMenu.Items[i];
      if miSub.Count>0 then
        UpdMenu(miSub, AKeymap)
      else
        if miSub.Tag>0 then
          miSub.ShortCut:= AKeymap.GetShortcutFromCommand(cmdFirstPluginCommand+miSub.Tag);
    end;
  end;
  //
var
  F: TEditorFrame;
  Ed: TATSynEdit;
  NewLexer: string;
begin
  if Application.Terminated then exit;

  F:= CurrentFrame;
  if F=nil then exit;
  Ed:= F.Editor;

  if not AForceUpdate then
  begin
    NewLexer:= F.LexerName[Ed];
    if FLastLexerForPluginsMenu=NewLexer then exit;
    FLastLexerForPluginsMenu:= NewLexer;
  end;

  UpdMenu(mnuPlugins, Ed.Keymap);
end;


procedure TfmMain.MenuPluginClick(Sender: TObject);
begin
  DoPyCommand_ByPluginIndex((Sender as TComponent).Tag, TATEditorCommandInvoke.MenuMain);
end;


