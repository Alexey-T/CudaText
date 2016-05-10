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
  list: TStringlist;
  fn: string;
  i: integer;
begin
  //commands: don't clear all, leave items from Api
  for i:= High(FPluginsCmd) downto Low(FPluginsCmd) do
    if (FPluginsCmd[i].ItemModule<>'') and (not FPluginsCmd[i].ItemFromApi) then
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
      s_events:= ini.ReadString('item'+Inttostr(i), 'events', '');
      s_keys:= ini.ReadString('item'+Inttostr(i), 'keys', '');
      s_inmenu:= ini.ReadBool('item'+Inttostr(i), 'menu', true);

      if s_section='commands' then
      begin
        if s_caption='' then Continue;
        if s_method='' then Continue;

        cnt:= 0;
        while FPluginsCmd[cnt].ItemModule<>'' do Inc(cnt);
        if cnt>=High(FPluginsCmd) then Break;

        FPluginsCmd[cnt].ItemModule:= s_module;
        FPluginsCmd[cnt].ItemProc:= s_method;
        FPluginsCmd[cnt].ItemProcParam:= '';
        FPluginsCmd[cnt].ItemCaption:= s_caption;
        FPluginsCmd[cnt].ItemLexers:= s_lexers;
        FPluginsCmd[cnt].ItemInMenu:= s_inmenu;
      end;

      if s_section='events' then
      begin
        if s_events='' then Continue;

        cnt:= 0;
        while FPluginsEvents[cnt].ItemModule<>'' do Inc(cnt);
        if cnt>=High(FPluginsEvents) then Break;

        FPluginsEvents[cnt].ItemModule:= s_module;
        DoPyStringToEvents(s_events, FPluginsEvents[cnt].ItemEvents);
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
  if mnuPlug=nil then exit;
  mnuPlug.Clear;

  //create menu items for Command plugins
  for i:= 0 to High(FPluginsCmd) do
  begin
    if FPluginsCmd[i].ItemModule='' then Break;
    if not FPluginsCmd[i].ItemInMenu then Continue;

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
      mi.OnClick:= @MenuPluginClick;
      mi.Tag:= i;

      //disabled yet: this gives side problem: after chg of hotkey in Commands,
      //need call UpdateMenuPlugins(not done)
      //UpKey(mi, cmdFirstPluginCommand+i);
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
    MsgStatus(msgStatusCommandOnlyForLexers+' '+FPluginsCmd[Num].ItemLexers);
    Exit
  end;

  DoPyCommand(
    FPluginsCmd[Num].ItemModule,
    FPluginsCmd[Num].ItemProc,
    FPluginsCmd[Num].ItemProcParam
    );
end;


function TfmMain.DoPyEvent(AEd: TATSynEdit; AEvent: TAppPyEvent;
  const AParams: array of string): string;
const
  cTrueResultStopsThese = [cEventOnComplete{, cEventOnFuncHint}];
var
  SCurLexer: string;
  bNeedGroup: boolean;
  Frame: TEditorFrame;
  i: integer;
begin
  //empty string result means "no handlers for event"
  Result:= '';
  Frame:= CurrentFrame;
  if Frame=nil then exit;
  SCurLexer:= Frame.LexerName;
  bNeedGroup:= not (AEvent in [cEventOnCaret, cEventOnChange, cEventOnChangeSlow]);

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
          if bNeedGroup then
            AEd.Strings.BeginUndoGroup;
          PyCommandRunning:= true;
          try
            Result:= Py_RunPlugin_Event(ItemModule, cAppPyEvent[AEvent], AEd, AParams);
          finally
            PyCommandRunning:= false;
            if bNeedGroup then
              AEd.Strings.EndUndoGroup;
          end;

          //True for some events means "stop"
          if Result=cPyTrue then
            if AEvent in cTrueResultStopsThese then Exit;

          //False means "stop", other results ignored
          if Result=cPyFalse then Exit;
        end;
    end;
end;


procedure TfmMain.DoPyResetPlugins;
var
  fn, Cmd: string;
  L: TStringList;
begin
  fn:= GetAppPath(cDirPy)+DirectorySeparator+'cudatext_reset_plugins.py';
  if not FileExists(fn) then
  begin
    MsgBox(msgCannotFindFile+#13+fn, MB_OK or MB_ICONERROR);
    Exit
  end;

  L:= TStringList.Create;
  try
    L.LoadFromFile(fn);
    GetPythonEngine.ExecStrings(L);
    Cmd:= Format('_reset_plugins(r"%s")', [GetAppPath(cDirPy)]);
    GetPythonEngine.ExecString(Cmd);
  finally
    FreeAndNil(L)
  end;
end;

procedure TfmMain.DoPyRescanPlugins;
begin
  DoOps_LoadPlugins;
  UpdateMenuPlugins;
  DoOps_LoadKeymap;
  MsgStatus('Rescanned plugins');
end;

procedure TfmMain.DoPyRunLastPlugin;
begin
  if PyLastCommandModule<>'' then
    DoPyCommand(PyLastCommandModule, PyLastCommandMethod, PyLastCommandParam);
end;

procedure TfmMain.DoPanel_Event(AControl: TControl; const AEvent: string);
begin
  DoPyEvent(CurrentEditor, cEventOnPanel,
    [
      IntToStr(PtrInt(AControl)),
      '"'+AEvent+'"'
    ]);
end;

procedure TfmMain.DoPanel_OnContextPopup(Sender: TObject; MousePos: TPoint;
  var Handled: Boolean);
begin
  DoPanel_Event(Sender as TControl, 'on_menu');
end;

procedure TfmMain.DoPanel_OnClick(Sender: TObject);
begin
  DoPanel_Event(Sender as TControl, 'on_click');
end;

procedure TfmMain.DoPanel_OnDblClick(Sender: TObject);
begin
  DoPanel_Event(Sender as TControl, 'on_dbl_click');
end;

procedure TfmMain.DoPanel_OnSelChanged(Sender: TObject);
begin
  DoPanel_Event(Sender as TControl, 'on_sel');
end;

