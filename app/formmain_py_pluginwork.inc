{$ifdef nn}begin end;{$endif}

procedure TfmMain.DoPyStringToEvents(const AEventStr: string;
  out AEvents: TAppPyEvents;
  out AEventsPrior: TAppPyEventsPrior;
  out AEventsLazy: TAppPyEventsLazy);
var
  str, strItem: string;
  event: TAppPyEvent;
  nPrior: byte;
  bLazy: boolean;
begin
  AEvents:= [];
  FillChar(AEventsPrior, SizeOf(AEventsPrior), 0);
  FillChar(AEventsLazy, SizeOf(AEventsLazy), 0);
  str:= AEventStr;
  repeat
    strItem:= SGetItem(str);
    if strItem='' then Break;

    nPrior:= 0;
    while SEndsWith(strItem, '+') do
    begin
      Inc(nPrior);
      SetLength(strItem, Length(strItem)-1);
    end;

    bLazy:= false;
    if SEndsWith(strItem, '~') then
    begin
      bLazy:= true;
      SetLength(strItem, Length(strItem)-1);
    end;

    for event in TAppPyEvent do
      if strItem=cAppPyEvent[event] then
      begin
        Include(AEvents, event);
        AEventsPrior[event]:= nPrior;
        AEventsLazy[event]:= bLazy;
        Break
      end;
  until false;
end;


procedure TfmMain.DoOps_LoadPlugins;
var
  list: TStringlist;
  SItem, fn: string;
  i: integer;
begin
  //commands: don't clear all, leave items from Api
  for i:= AppCommandList.Count-1 downto 0 do
    with TAppCommand(AppCommandList[i]) do
      if (ItemModule<>'') and (not ItemFromApi) then
        AppCommandList.Delete(i);

  //events: clear all
  AppEventList.Clear;

  list:= TStringlist.Create;
  try
    FindAllDirectories(list, GetAppPath(cDirPy), false);
    list.Sort;
    for SItem in list do
    begin
      fn:= SItem+DirectorySeparator+'install.inf';
      if FileExists(fn) then
        DoOps_LoadPluginFromInf(fn);
    end;
  finally
    FreeAndNil(list);
  end;

  UpdateKeymapDynamicItems;
end;


procedure TfmMain.DoOps_LoadPluginFromInf(const fn_inf: string);
var
  ini: TIniFile;
  sections: TStringList;
  ini_section,
  dir_inf, s_section, s_caption, s_module, s_method, s_lexers,
  s_events, s_keys, s_inmenu, s_icon: string;
  CmdItem: TAppCommand;
  EventItem: TAppEvent;
  TreeHelper: TAppTreeHelper;
begin
  ini:= TIniFile.Create(fn_inf);
  sections:= TStringList.Create;

  try
    dir_inf:= ExtractFileDir(fn_inf);
    s_module:= ExtractFileName(dir_inf);
    ini.ReadSections(sections);

    for s_section in sections do
    begin
      if not SRegexMatchesString(s_section, 'sidebar\d+', true) then Continue;

      s_caption:= ini.ReadString(s_section, 'hint', '');
      if s_caption='' then Continue;

      s_icon:= ini.ReadString(s_section, 'icon', '');
      if s_icon='' then Continue;
      s_icon:= StringReplace(s_icon, '{dir}', dir_inf, []);
      {$ifdef windows}
      s_icon:= StringReplace(s_icon, '/', '\', [rfReplaceAll]);
      {$endif}

      s_method:= ini.ReadString(s_section, 'method', '');
      if s_method='' then Continue;

      DoSidebar_AddTab_Empty(
        s_caption,
        DoSidebar_FilenameToImageIndex(s_caption, s_icon),
        s_module,
        s_method
        );
    end;

    for ini_section in sections do
    begin
      //add tree helpers
      if SRegexMatchesString(ini_section, 'treehelper\d+', true) then
      begin
        s_method:= ini.ReadString(ini_section, 'method', '');
        s_lexers:= ini.ReadString(ini_section, 'lexers', '');
        TreeHelper:= TAppTreeHelper.Create;
        TreeHelper.ItemModule:= s_module;
        TreeHelper.ItemProc:= s_method;
        TreeHelper.ItemLexers:= s_lexers;
        AppTreeHelpers.Add(TreeHelper);
        Continue;
      end;

      if not SRegexMatchesString(ini_section, 'item\d+', true) then Continue;

      s_section:= ini.ReadString(ini_section, 'section', '');
      s_caption:= ini.ReadString(ini_section, 'caption', '');
      s_method:= ini.ReadString(ini_section, 'method', '');
      //s_hotkey:= ini.ReadString(ini_section, 'hotkey', '');

      s_lexers:= ini.ReadString(ini_section, 'lexers', '');
      if SBeginsWith(s_lexers, '$') then //var $name defined in [info]
        s_lexers:= ini.ReadString('info', s_lexers, '');

      s_events:= ini.ReadString(ini_section, 'events', '');
      s_keys:= ini.ReadString(ini_section, 'keys', '');
      s_inmenu:= ini.ReadString(ini_section, 'menu', '');

      //add commands
      if s_section='commands' then
      begin
        if s_caption='' then Continue;
        if s_method='' then Continue;

        CmdItem:= TAppCommand.Create;
        CmdItem.ItemModule:= s_module;
        CmdItem.ItemProc:= s_method;
        CmdItem.ItemProcParam:= '';
        CmdItem.ItemCaption:= s_caption;
        CmdItem.ItemLexers:= s_lexers;
        CmdItem.ItemInMenu:= s_inmenu;
        AppCommandList.Add(CmdItem);
        Continue;
      end;

      //add events
      if s_section='events' then
      begin
        if s_events='' then Continue;

        EventItem:= TAppEvent.Create;
        EventItem.ItemModule:= s_module;
        DoPyStringToEvents(s_events,
          EventItem.ItemEvents,
          EventItem.ItemEventsPrior,
          EventItem.ItemEventsLazy
          );
        EventItem.ItemLexers:= s_lexers;
        EventItem.ItemKeys:= s_keys;
        AppEventList.Add(EventItem);
        Continue;
      end;
    end;
  finally
    FreeAndNil(sections);
    FreeAndNil(ini);
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
    with TAppEvent(AppEventList[i]) do
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


procedure TfmMain.DoOps_AddPluginMenuItem(ACaption: string; ASubMenu: TMenuItem; ATag: integer);
var
  mi: TMenuItem;
  strItem: string;
  idx, NInsert: integer;
  bRoot: boolean;
begin
  mi:= nil;
  bRoot:= true;

  //need to parse caption and create subitems, separated with '\'
  repeat
    strItem:= SGetItem(ACaption, '\');
    if strItem='' then Break;
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
  function GroupValue(AName: string): string;
  var
    Item: TAppKeyValue;
    i: integer;
  begin
    Result:= '';
    AName:= SGetItem(AName, '\');
    for i:= 0 to AppConfig_PGroups.Count-1 do
    begin
      Item:= TAppKeyValue(AppConfig_PGroups[i]);
      if SRegexMatchesString(AName, Item.Key, false) then
        exit(Item.Value+'\');
    end;
  end;
  //
var
  SCaption, SInMenu: string;
  CmdItem: TAppCommand;
  i: integer;
begin
  if not PythonOK then exit;
  if mnuPlugins=nil then exit;

  mnuPlugins.Clear;
  if Assigned(mnuOpPlugins) then
    mnuOpPlugins.Clear;

    for i:= 0 to AppCommandList.Count-1 do
    begin
      CmdItem:= TAppCommand(AppCommandList[i]);
      if CmdItem.ItemModule='' then Break;
      if CmdItem.ItemFromApi then Continue;

      SCaption:= CmdItem.ItemCaption;
      SInMenu:= CmdItem.ItemInMenu;
      if SInMenu='0' then Continue;

      //add to Plugins
      if (SInMenu='') or (Pos('p', SInMenu)>0) then
        DoOps_AddPluginMenuItem(GroupValue(SCaption)+SCaption, mnuPlugins, i);

      //add to Settings-plugins
      if Assigned(mnuOpPlugins) then
      if Pos('o', SInMenu)>0 then
        DoOps_AddPluginMenuItem(SCaption, mnuOpPlugins, i);
    end;
end;

procedure TfmMain.UpdateMenuPlugins_Shortcuts(AForceUpdate: boolean = false);
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
  DoPyCommand_ByPluginIndex((Sender as TComponent).Tag);
end;


