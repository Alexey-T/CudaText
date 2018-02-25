{$ifdef nn}begin end;{$endif}

procedure TfmMain.DoPyStringToEvents(const AEventStr: string;
  out AEvents: TAppPyEvents;
  out AEventsPrior: TAppPyEventsPrior);
var
  str, strItem: string;
  event: TAppPyEvent;
  nPrior: byte;
begin
  AEvents:= [];
  FillChar(AEventsPrior, SizeOf(AEventsPrior), 0);
  str:= AEventStr;
  repeat
    strItem:= SGetItem(str);
    if strItem='' then Break;

    nPrior:= 0;
    while SEndsWith(strItem, '+') do
    begin
      Inc(nPrior);
      Delete(strItem, Length(strItem), 1);
    end;

    for event in TAppPyEvent do
      if strItem=cAppPyEvent[event] then
      begin
        Include(AEvents, event);
        AEventsPrior[event]:= nPrior;
      end;
  until false;
end;


procedure TfmMain.DoOps_LoadPlugins;
var
  list: TStringlist;
  fn: string;
  i: integer;
begin
  //commands: don't clear all, leave items from Api
  for i:= High(AppPluginsCommand) downto Low(AppPluginsCommand) do
    if (AppPluginsCommand[i].ItemModule<>'') and (not AppPluginsCommand[i].ItemFromApi) then
      CommandPlugins_DeleteItem(i);
  //events: clear all
  FillChar(AppPluginsEvent, SizeOf(AppPluginsEvent), 0);

  list:= TStringlist.Create;
  try
    FindAllDirectories(list, GetAppPath(cDirPy), false);
    list.Sort;
    for i:= 0 to list.Count-1 do
    begin
      fn:= list[i]+DirectorySeparator+'install.inf';
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
  dir_inf, s_section, s_caption, s_module, s_method, s_lexers,
  s_events, s_keys, s_inmenu, s_icon: string;
  i, cnt: integer;
begin
  ini:= TIniFile.Create(fn_inf);
  try
    dir_inf:= ExtractFileDir(fn_inf);
    s_module:= ExtractFileName(dir_inf);

    for i:= 1 to cMaxSidebarItemsInInstallInf do
    begin
      s_section:= 'sidebar'+Inttostr(i);
      s_caption:= ini.ReadString(s_section, 'hint', '');
      if s_caption='' then Continue;

      s_icon:= ini.ReadString(s_section, 'icon', '');
      if s_icon='' then Continue;
      s_icon:= StringReplace(s_icon, '{dir}', dir_inf, []);

      s_method:= ini.ReadString(s_section, 'method', '');
      if s_method='' then Continue;

      DoSidebar_AddTab_Empty(
        s_caption,
        DoSidebar_FilenameToImageIndex(s_caption, s_icon),
        s_module,
        s_method
        );
    end;

    for i:= 1 to cMaxItemsInInstallInf do
    begin
      s_section:= ini.ReadString('item'+Inttostr(i), 'section', '');
      s_caption:= ini.ReadString('item'+Inttostr(i), 'caption', '');
      s_method:= ini.ReadString('item'+Inttostr(i), 'method', '');
      //s_hotkey:= ini.ReadString('item'+Inttostr(i), 'hotkey', '');

      s_lexers:= ini.ReadString('item'+Inttostr(i), 'lexers', '');
      if SBeginsWith(s_lexers, '$') then //var $name defined in [info]
        s_lexers:= ini.ReadString('info', s_lexers, '');

      s_events:= ini.ReadString('item'+Inttostr(i), 'events', '');
      s_keys:= ini.ReadString('item'+Inttostr(i), 'keys', '');
      s_inmenu:= ini.ReadString('item'+Inttostr(i), 'menu', '');

      if s_section='commands' then
      begin
        if s_caption='' then Continue;
        if s_method='' then Continue;

        cnt:= 0;
        while AppPluginsCommand[cnt].ItemModule<>'' do Inc(cnt);
        if cnt>=High(AppPluginsCommand) then Break;

        AppPluginsCommand[cnt].ItemModule:= s_module;
        AppPluginsCommand[cnt].ItemProc:= s_method;
        AppPluginsCommand[cnt].ItemProcParam:= '';
        AppPluginsCommand[cnt].ItemCaption:= s_caption;
        AppPluginsCommand[cnt].ItemLexers:= s_lexers;
        AppPluginsCommand[cnt].ItemInMenu:= s_inmenu;
      end;

      if s_section='events' then
      begin
        if s_events='' then Continue;

        cnt:= 0;
        while AppPluginsEvent[cnt].ItemModule<>'' do Inc(cnt);
        if cnt>=High(AppPluginsEvent) then Break;

        AppPluginsEvent[cnt].ItemModule:= s_module;
        DoPyStringToEvents(s_events,
          AppPluginsEvent[cnt].ItemEvents,
          AppPluginsEvent[cnt].ItemEventsPrior);
        AppPluginsEvent[cnt].ItemLexers:= s_lexers;
        AppPluginsEvent[cnt].ItemKeys:= s_keys;
      end;
    end;
  finally
    FreeAndNil(ini);
  end;
end;


procedure TfmMain.DoOps_ShowEventPlugins;
var
  i: integer;
  s, s2: string;
  ev: TAppPyEvent;
begin
  exit;////////

  s:= '';
  for i:= 0 to 100 do
    with AppPluginsEvent[i] do
    begin
      if ItemModule='' then Break;
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
  Showmessage(s);
end;


procedure TfmMain.DoOps_AddPluginMenuItem(ACaption: string; ASubMenu: TMenuItem; ATag: integer);
var
  mi: TMenuItem;
  strItem: string;
  idx: integer;
begin
  mi:= nil;

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
      ASubMenu.Add(mi);
      ASubMenu:= mi;
    end
    else
      ASubMenu:= ASubMenu.Items[idx];
  until false;

  if Assigned(mi) then
  begin
    mi.OnClick:= @MenuPluginClick;
    mi.Tag:= ATag;
  end;
end;

procedure TfmMain.UpdateMenuPlugins;
var
  SCaption, SInMenu: string;
  i: integer;
begin
  if not PythonOK then exit;
  if mnuPlugins=nil then exit;

  mnuPlugins.Clear;
  if Assigned(mnuOpPlugins) then
    mnuOpPlugins.Clear;

  for i:= 0 to High(AppPluginsCommand) do
  begin
    if AppPluginsCommand[i].ItemModule='' then Break;
    if AppPluginsCommand[i].ItemFromApi then Continue;

    SCaption:= AppPluginsCommand[i].ItemCaption;
    SInMenu:= AppPluginsCommand[i].ItemInMenu;
    if SInMenu='0' then Continue;

    //add to Plugins
    if (SInMenu='') or (Pos('p', SInMenu)>0) then
      DoOps_AddPluginMenuItem(SCaption, mnuPlugins, i);

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
  NewLexer: string;
  Ed: TATSynEdit;
begin
  if Application.Terminated then exit;
  if not AForceUpdate then
  begin
    NewLexer:= CurrentFrame.LexerName;
    if FLastLexerForPluginsMenu=NewLexer then exit;
    FLastLexerForPluginsMenu:= NewLexer;
  end;

  Ed:= CurrentEditor;
  if Assigned(Ed) then
    UpdMenu(mnuPlugins, Ed.Keymap);
end;


procedure TfmMain.MenuPluginClick(Sender: TObject);
begin
  DoPyCommand_ByPluginIndex((Sender as TComponent).Tag);
end;


