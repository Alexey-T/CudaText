(*
This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at http://mozilla.org/MPL/2.0/.

Copyright (c) Alexey Torgashin
*)
{$ifdef nn}begin end;{$endif}

procedure TfmMain.DoPyStringToEvents(const AEventStr: string; var AEvents: TAppPyEvents);
var
  str, strItem: string;
  i_event: TAppPyEvent;
begin
  AEvents:= [];
  str:= AEventStr;
  repeat
    strItem:= SGetItem(str);
    if strItem='' then Break;
    for i_event in TAppPyEvent do
      if strItem=cAppPyEvent[i_event] then
        Include(AEvents, i_event);
  until false;
end;


procedure TfmMain.DoOps_LoadPlugins;
var
  fn_plugins: string;
  cfg: TJsonConfig;
  list, list2: TStringlist;
  str, strItem, strPath: string;
  i, j, cnt: integer;
  i_event: TAppPyEvent;
begin
  fn_plugins:= GetAppPath(cFileOptPlugins);
  if not FileExists(fn_plugins) then exit;

  cfg:= TJSONConfig.Create(nil);
  list:= TStringlist.Create;
  list2:= TStringlist.Create;
  try
    try
      cfg.Filename:= fn_plugins;
    except
      MsgBox(msgCannotReadConf+#13+fn_plugins, mb_ok or mb_iconerror);
      exit
    end;

    //read command plugins
    cnt:= 0;
    list.Clear;
    cfg.EnumSubKeys('/commands', list);
    list.Sort;

    for i:= 0 to list.Count-1 do
    begin
      list2.Clear;
      cfg.EnumSubKeys('/commands/'+list[i], list2);
      list2.Sort;

      for j:= 0 to list2.Count-1 do
      begin
        FPluginsCmd[cnt].ItemModule:= list[i];
        strPath:= '/commands/'+list[i]+'/'+list2[j]+'/';
        FPluginsCmd[cnt].ItemProc:= cfg.GetValue(strPath+'proc', '');
        FPluginsCmd[cnt].ItemCaption:= cfg.GetValue(strPath+'caption', '');
        FPluginsCmd[cnt].ItemLexers:= cfg.GetValue(strPath+'lexers', '');
        FPluginsCmd[cnt].ItemHotkey:= TextToShortCut(cfg.GetValue(strPath+'hotkey', ''));

        Inc(cnt);
        if cnt>=High(FPluginsCmd) then Break;
      end;
    end;

    //read event plugins
    cnt:= 0;
    list.Clear;
    cfg.EnumSubKeys('/events', list);
    list.Sort;

    for cnt:= 0 to list.Count-1 do
    begin
      FPluginsEvents[cnt].ItemModule:= list[cnt];
      strPath:= '/events/'+list[cnt]+'/';
      FPluginsEvents[cnt].ItemLexers:= cfg.GetValue(strPath+'lexers', '');
      FPluginsEvents[cnt].ItemKeys:= cfg.GetValue(strPath+'keys', '');

      //parse "events"
      str:= cfg.GetValue(strPath+'events', '');
      DoPyStringToEvents(str, FPluginsEvents[cnt].ItemEvents);

      if cnt>=High(FPluginsEvents) then Break;
    end;
    DoOps_ShowEventPlugins;

  finally
    FreeAndNil(list2);
    FreeAndNil(list);
    FreeAndNil(cfg);
  end;

  UpdateMenuPlugins;
  UpdateKeymapDynamicItems;
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
  if mnuPlug=nil then exit;

  //create menu items for Command plugins
  for i:= 0 to High(FPluginsCmd) do
  begin
    if FPluginsCmd[i].ItemModule='' then Break;
    mi:= nil;
    miSub:= mnuPlug;
    str:= FPluginsCmd[i].ItemCaption;

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
      mi.ShortCut:= FPluginsCmd[i].ItemHotkey;
      mi.OnClick:= @MenuPluginClick;
      mi.Tag:= i;
    end;
  end;
end;

function TfmMain.IsLexerMatches(const ANameList: string): boolean;
begin
  Result:= IsLexerListed(CurrentFrame.LexerName, ANameList);
end;

procedure TfmMain.MenuPluginClick(Sender: TObject);
begin
  Py_RunPlugin_Index((Sender as TComponent).Tag);
end;

procedure TfmMain.Py_RunPlugin_Index(Num: integer);
begin
  if not ((Num>=Low(FPluginsCmd)) and (Num<=High(FPluginsCmd))) then exit;

  if not IsLexerMatches(FPluginsCmd[Num].ItemLexers) then
  begin
    MsgBox('Command is only for lexers: '+FPluginsCmd[Num].ItemLexers, MB_OK+MB_ICONWARNING);
    Exit
  end;

  Py_RunPlugin_Command(
    FPluginsCmd[Num].ItemModule,
    FPluginsCmd[Num].ItemProc
    );
end;

procedure TfmMain.DoOps_PreinstallPlugins;
var
  cfg: TJSONConfig;
  //
  procedure DoAdd(const SModule, SMethod, SCaption: string; AIndex: integer);
  var
    path_method,
    path_caption: string;
  begin
    path_method:= '/commands/'+SModule+'/'+IntToStr(AIndex)+'/proc';
    path_caption:= '/commands/'+SModule+'/'+IntToStr(AIndex)+'/caption';
    if cfg.GetValue(path_caption, '')<>SCaption then
    begin
      cfg.SetValue(path_method, SMethod);
      cfg.SetValue(path_caption, SCaption);
    end;
  end;
  //
const
  cModuleAddonman = 'cuda_addonman';
  cCaptionAddonman = 'Addons Manager';
  cModuleMake = 'cuda_make_plugin';
  cCaptionMake = 'Make Plugin';
  cModuleInsTime = 'cuda_insert_time';
  cCaptionInsTime = 'Insert Time';
var
  fn: string;
begin
  fn:= GetAppPath(cFileOptPlugins);
  cfg:= TJSONConfig.Create(nil);
  try
    try
      cfg.Filename:= fn;
      cfg.Formatted:= true;
    except
      MsgBox(msgCannotReadConf+#13+fn, mb_ok or mb_iconerror);
      exit
    end;

    DoAdd(cModuleAddonman, 'do_install', cCaptionAddonman+'\Install lexer...', 0);
    DoAdd(cModuleAddonman, 'do_edit', cCaptionAddonman+'\Edit plugin...', 1);
    DoAdd(cModuleAddonman, 'do_remove', cCaptionAddonman+'\Remove plugin...', 2);

    DoAdd(cModuleMake, 'run', cCaptionMake, 0);
    DoAdd(cModuleInsTime, 'run', cCaptionInsTime, 0);

  finally
    FreeAndNil(cfg);
  end;
end;


function TfmMain.DoPyEvent(AEd: TATSynEdit; AEvent: TAppPyEvent;
  const AParams: array of string): string;
const
  cTrueResultStopsThese = [cEventOnComplete, cEventOnFuncHint];
var
  SCurLexer: string;
  i: integer;
begin
  //empty string result means "no handlers for event"
  Result:= '';
  SCurLexer:= CurrentFrame.LexerName;

  for i:= Low(FPluginsEvents) to High(FPluginsEvents) do
    with FPluginsEvents[i] do
    begin
      if (ItemModule='') then Break;
      if (AEvent in ItemEvents) then
        if (ItemLexers='') or IsLexerListed(SCurLexer, ItemLexers) then
        begin
          //check that OnKey event is called for supported keys
          if (AEvent=cEventOnKey) then
            if Length(AParams)>0 then
              if (ItemKeys<>'') and not IsLexerListed(AParams[0], ItemKeys) then
                Continue;

          //call Python
          Result:= Py_RunPlugin_Event(ItemModule, cAppPyEvent[AEvent], AEd, AParams);

          //True for some events means "stop"
          if Result=cPyTrue then
            if AEvent in cTrueResultStopsThese then Exit;

          //False means "stop", other results ignored
          if Result=cPyFalse then Exit;
        end;
    end;
end;

