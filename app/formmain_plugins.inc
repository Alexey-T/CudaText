(*
This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at http://mozilla.org/MPL/2.0/.

Copyright (c) Alexey Torgashin
*)
{$ifdef nn}begin end;{$endif}

procedure TfmMain.DoOps_LoadPlugins;
var
  fn_plugins: string;
  cfg: TJsonConfig;
  list, list2: TStringlist;
  mi, miSub: TMenuItem;
  str, strItem, strPath: string;
  i, j, cnt, idx: integer;
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
  finally
    FreeAndNil(list2);
    FreeAndNil(list);
    FreeAndNil(cfg);
  end;

  for i:= 0 to High(FPluginsCmd) do
  begin
    if FPluginsCmd[i].ItemModule='' then Break;
    mnuPlugNone.Visible:= false;
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

  UpdateKeymapDynamicItems;
end;

function TfmMain.IsLexerMatches(const ANameList: string): boolean;
var
  F: TEditorFrame;
  SLexer: string;
begin
  if ANameList='' then exit(true);
  F:= CurrentFrame;
  if F.Lexer=nil then exit(false);
  SLexer:= F.Lexer.LexerName;
  if SLexer='' then exit(false);
  Result:= Pos(
    ','+LowerCase(SLexer)+',',
    ','+LowerCase(ANameList)+',' )>0;
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

