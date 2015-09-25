{$ifdef nn}begin end;{$endif}

procedure TfmMain.DoOps_LoadPlugins;
var
  fn_plugins: string;
  cfg: TJsonConfig;
  list, list2: TStringlist;
  mi: TMenuItem;
  i, j, cnt: integer;
begin
  fn_plugins:= GetAppPath(cFileOptPlugins);
  if not FileExists(fn_plugins) then exit;

  cfg:= TJSONConfig.Create(Self);
  list:= TStringlist.Create;
  list2:= TStringlist.Create;
  try
    try
      cfg.Filename:= fn_plugins;
    except
      MsgBox(msgCannotReadConf+#13+fn_plugins, mb_ok or mb_iconerror);
      exit
    end;

    cnt:= 0;
    list.Clear;
    cfg.EnumSubKeys('/commands', list);
    for i:= 0 to list.Count-1 do
    begin
      list2.Clear;
      cfg.EnumSubKeys('/commands/'+list[i], list2);
      for j:= 0 to list2.Count-1 do
      begin
        FPluginsCmd[cnt].ItemModule:= list[i];
        FPluginsCmd[cnt].ItemProc:= list2[j];
        FPluginsCmd[cnt].ItemCaption:= cfg.GetValue('/commands/'+list[i]+'/'+list2[j]+'/caption', list2[j]);

        Inc(cnt);
        if cnt>=High(FPluginsCmd) then Break;
      end;
    end;
  finally
    FreeAndNil(list2);
    FreeAndNil(list);
    FreeAndNil(cfg);
  end;

  for i:= 0 to High(FPluginsCmd) do
  begin
    if FPluginsCmd[i].ItemModule='' then Break;
    mi:= TMenuItem.Create(Self);
    mi.Caption:= FPluginsCmd[i].ItemCaption;
    mi.OnClick:= @MenuPluginClick;
    mi.Tag:= i;
    mnuPlug.Add(mi);
    mnuPlugNone.Visible:= false;
  end;
end;


procedure TfmMain.MenuPluginClick(Sender: TObject);
var
  Num: integer;
begin
  Num:= (Sender as TComponent).Tag;
  Py_RunPlugin_Command(
    FPluginsCmd[Num].ItemModule,
    FPluginsCmd[Num].ItemProc
    );
end;

