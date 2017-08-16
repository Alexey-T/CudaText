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
  FillChar(FPluginsEvents, SizeOf(FPluginsEvents), 0);

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
  s_section, s_caption, s_module, s_method, s_lexers, //s_hotkey,
  s_events, s_keys: string;
  s_inmenu: boolean;
  i, cnt: integer;
begin
  ini:= TIniFile.Create(fn_inf);
  try
    s_module:= ExtractFileName(ExtractFileDir(fn_inf));

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
      s_inmenu:= ini.ReadBool('item'+Inttostr(i), 'menu', true);

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
        while FPluginsEvents[cnt].ItemModule<>'' do Inc(cnt);
        if cnt>=High(FPluginsEvents) then Break;

        FPluginsEvents[cnt].ItemModule:= s_module;
        DoPyStringToEvents(s_events,
          FPluginsEvents[cnt].ItemEvents,
          FPluginsEvents[cnt].ItemEventsPrior);
        FPluginsEvents[cnt].ItemLexers:= s_lexers;
        FPluginsEvents[cnt].ItemKeys:= s_keys;
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
    with FPluginsEvents[i] do
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

procedure TfmMain.UpdateMenuPlugins;
var
  mi, miSub: TMenuItem;
  str, strItem: string;
  i, idx: integer;
begin
  if not PythonOK then exit;
  if mnuPlug=nil then exit;
  mnuPlug.Clear;

  //create menu items for Command plugins
  for i:= 0 to High(AppPluginsCommand) do
  begin
    if AppPluginsCommand[i].ItemModule='' then Break;
    if not AppPluginsCommand[i].ItemInMenu then Continue;

    mi:= nil;
    miSub:= mnuPlug;
    str:= AppPluginsCommand[i].ItemCaption;

    //need parse caption and create subitems, separated with '\'
    repeat
      strItem:= SGetItem(str, '\');
      if strItem='' then Break;
      if strItem='-' then
        idx:= -1
      else
        idx:= miSub.IndexOfCaption(strItem);
      if idx<0 then
      begin
        mi:= TMenuItem.Create(Self);
        mi.Caption:= strItem;
        miSub.Add(mi);
        miSub:= mi;
      end
      else
        miSub:= miSub.Items[idx];
    until false;

    if Assigned(mi) then
    begin
      mi.OnClick:= @MenuPluginClick;
      mi.Tag:= i;
    end;
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
    UpdMenu(mnuPlug, Ed.Keymap);
end;


procedure TfmMain.MenuPluginClick(Sender: TObject);
begin
  DoPyCommand_ByPluginIndex((Sender as TComponent).Tag);
end;


