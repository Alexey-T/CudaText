{$ifdef nn}begin end;{$endif}

procedure TfmMain.DoOps_LoadPlugins;
var
  fn_plugins: string;
  cfg: TJsonConfig;
  list: TStringlist;
  mi: TMenuItem;
  i: integer;
begin
  fn_plugins:= GetAppPath(cFileOptPlugins);
  if not FileExists(fn_plugins) then exit;

  cfg:= TJSONConfig.Create(Self);
  try
    try
      cfg.Filename:= fn_plugins;
    except
      MsgBox(msgCannotReadConf+#13+fn_plugins, mb_ok or mb_iconerror);
      exit
    end;

    list:= TStringlist.create;
    try
      cfg.EnumSubKeys('/commands', list);
      for i:= 0 to list.Count-1 do
      begin
        FPluginsCmd[i].ItemDir:= list[i];
        FPluginsCmd[i].ItemProc:= cfg.GetValue('/commands/'+list[i]+'/def', '');
        FPluginsCmd[i].ItemCaption:= cfg.GetValue('/commands/'+list[i]+'/title', '');
      end;
    finally
      FreeAndNil(list);
    end;
  finally
    FreeAndNil(cfg);
  end;

  for i:= 0 to High(FPluginsCmd) do
  begin
    if FPluginsCmd[i].ItemDir='' then break;
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
  Showmessage(FPluginsCmd[Num].ItemProc);
end;

