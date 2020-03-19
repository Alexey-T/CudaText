{$ifdef nn}begin end;{$endif}




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
    Item: TAppKeyValue;
    SName, SAll: string;
    i: integer;
  begin
    Result:= '';
    SSplitByChar(AValue, '\', SName, SAll);
    for i:= 0 to AppConfig_PGroups.Count-1 do
    begin
      Item:= TAppKeyValue(AppConfig_PGroups[i]);
      if SRegexMatchesString(SName, Item.Key, false) then
        exit(Item.Value+'\');
    end;
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


