{$ifdef nn}begin end;{$endif}


procedure TfmMain.DoPyCommand(const AModule, AMethod: string;
  const AParams: TAppVariantArray; AInvoke: TATCommandInvoke);
var
  Frame: TEditorFrame;
  Ed: TATSynEdit;
  SParam, SPyText: string;
begin
  if not AppPython.Inited then exit;
  PyEditorMaybeDeleted:= false;

  Frame:= CurrentFrame;
  if Frame=nil then exit;
  Ed:= Frame.Editor;
  if Ed=nil then exit;

  SParam:= '';
  if Length(AParams)>0 then
    SParam:= AppVariantToString(AParams[0], false{AndQuote});
    //AndQuote=false to fix CudaText issue #3929

  SPyText:= 'py:'+AModule+','+AMethod+','+SParam;

  if Frame.MacroRecord then
    Frame.MacroStrings.Add(SPyText);

  Ed.CommandLog.Add(cmd_PluginRun, AInvoke, SPyText);

  Ed.Strings.BeginUndoGroup;
  try
    AppPython.RunCommand(AModule, AMethod, AParams);
  finally
    if not PyEditorMaybeDeleted then
      Ed.Strings.EndUndoGroup;
  end;

  //if command "close all tabs" was run,
  //or Session Manager command "Forget session and close all files" was run,
  //Ed may be destroyed, so here is additional check
  if Ed<>CurrentEditor then exit;

  Ed.CommandLog.Add(cmd_PluginEnd, AInvoke, SPyText);
end;


procedure TfmMain.DoPyCommand_ByPluginIndex(AIndex: integer; AInvoke: TATCommandInvoke);
var
  F: TEditorFrame;
  CurLexer: string;
  CmdItem: TAppCommandInfo;
  Params: TAppVariantArray;
begin
  if not ((AIndex>=0) and (AIndex<AppCommandList.Count)) then exit;
  CmdItem:= TAppCommandInfo(AppCommandList[AIndex]);

  F:= CurrentFrame;
  if F=nil then exit;
  CurLexer:= F.LexerName[F.Editor];

  if not IsLexerListed(CurLexer, CmdItem.ItemLexers) then
  begin
    MsgStatus(msgStatusCommandOnlyForLexers+' '+CmdItem.ItemLexers);
    Exit
  end;

  if CmdItem.ItemProcParam<>'' then
    Params:= [AppVariant(CmdItem.ItemProcParam)]
  else
    Params:= [];

  DoPyCommand(CmdItem.ItemModule, CmdItem.ItemProc, Params, AInvoke);
end;


function TfmMain.DoPyEvent(AEd: TATSynEdit; AEvent: TAppPyEvent;
  const AParams: TAppVariantArray): TAppPyEventResult;
const
  cTheseEventsStoppedByTrue = [cEventOnComplete, cEventOnConsoleComplete, cEventOnFuncHint{, cEventOnTabSwitch}];
  cTheseEventsStoppedByNonEmptyString = [cEventOnFuncHint];
  cTheseEventsNeedGroupingUndo = [cEventOnComplete, cEventOnSnippet];
  cTheseEventsDontRequireFrame = [cEventOnSnippet, cEventOnStart, cEventOnStart2, cEventOnExit, cEventOnDeleteFile, cEventOnSidebarPopup];
  cTheseEventsAlwaysLazy = [cEventOnExit];
  cTheseEventsIgnoreCurrentLexer = [cEventOnLexer, cEventOnStart, cEventOnStart2, cEventOnExit, cEventOnDeleteFile, cEventOnSidebarPopup];
var
  SCurLexer, KeyForFilter: string;
  bNeedGroup: boolean;
  Frame: TEditorFrame;
  MaxPriority, NPlugin, NPriority: integer;
  Plugin: TAppEventInfo;
  bCheckLexer: boolean;
  bLazy: boolean;
begin
  Result.Val:= evrOther;
  Result.Str:= '';
  if not AppPython.Inited then exit;

  //block all events fired in FormCreate
  if not FHandledOnShowPartly then exit;
  //block on_focus/on_lexer fired too early
  if (AEvent in [cEventOnFocus, cEventOnLexer]) and not FHandledUntilFirstFocus then exit;

  //-1 if no such plugins
  MaxPriority:= AppEventsMaxPriorities[AEvent];
  if MaxPriority<0 then exit;

  Frame:= nil;
  SCurLexer:= '';
  bNeedGroup:= false;
  bCheckLexer:= Assigned(AEd) and not (AEvent in cTheseEventsIgnoreCurrentLexer);

  if bCheckLexer then
  begin
    Frame:= TGroupsHelper.GetEditorFrame(AEd);
    if Frame=nil then
    begin
      bCheckLexer:= false;
      if not (AEvent in cTheseEventsDontRequireFrame) then
        exit;
    end
    else
    begin
      SCurLexer:= Frame.LexerName[AEd];
      bNeedGroup:= AEvent in cTheseEventsNeedGroupingUndo;
    end;
  end;

  KeyForFilter:= '';
  case AEvent of
    cEventOnKey:
      begin
        //key is 1st int parameter
        KeyForFilter:= IntToStr(AParams[0].Int);
      end;
    cEventOnOpen:
      begin
        //key is file extension without leading dot
        KeyForFilter:= Copy(ExtractFileExt(AEd.FileName), 2);
      end;
    cEventOnOpenBefore:
      begin
        //key is filename, it is 1st parameter
        KeyForFilter:= Copy(ExtractFileExt(AParams[0].Str), 2);
      end;
  end;

  //see all items with priority=MaxPriority..0
  for NPriority:= MaxPriority downto 0 do
    for NPlugin:= 0 to AppEventList.Count-1 do
      begin
        Plugin:= TAppEventInfo(AppEventList[NPlugin]);
        if not (AEvent in Plugin.ItemEvents) then Continue;
        if (NPriority<>Plugin.ItemEventsPrior[AEvent]) then Continue;

        if bCheckLexer then
          if not ((Plugin.ItemLexers='') or IsLexerListed(SCurLexer, Plugin.ItemLexers)) then Continue;

        //check that OnKey event is called for supported keys
        //don't allow empty KeyForFilter, it is empty for on_open_pre for filename w/o extension
        if (Plugin.ItemKeys<>'') and not IsLexerListed(KeyForFilter, Plugin.ItemKeys) then
          Continue;

        //call Python
        if bNeedGroup then
          AEd.Strings.BeginUndoGroup;

        try
          bLazy:= (AEvent in cTheseEventsAlwaysLazy) or Plugin.ItemEventsLazy[AEvent]; //on_exit must be lazy
          Result:= AppPython.RunEvent(
              Plugin.ItemModule,
              cAppPyEvent[AEvent],
              AEd,
              AParams,
              bLazy
              );
        finally
          if bNeedGroup then
            AEd.Strings.EndUndoGroup;
        end;

        //True for _some_ events means "stop processing this event"
        if Result.Val=evrTrue then
          if AEvent in cTheseEventsStoppedByTrue then Exit;

        //non-empty string for some events means "stop processing this event"
        if AEvent in cTheseEventsStoppedByNonEmptyString then
          if (Result.Val=evrString) and (Result.Str<>'') then Exit;

        //False means "stop processing this event".
        //It's used e.g. for on_complete, on_key - first plugin which returned False,
        //stops event processing in other plugins.
        //Other results (e.g. None, 0, 1, list, dict) are ignored.
        if Result.Val=evrFalse then Exit;
      end;
end;


function EditorTextToPyObject(Ed: TATSynEdit): PPyObject;
var
  Count, i: integer;
  Strs: TATStrings;
begin
  Strs:= Ed.Strings;
  Count:= Strs.Count;
  with AppPython.Engine do
  begin
    Result:= PyList_New(Count);
    for i:= 0 to Count-1 do
    begin
      PyList_SetItem(Result, i, PyUnicodeFromString(Strs.Lines[i]));
    end;
  end;
end;


function TfmMain.RunTreeHelper(Frame: TEditorFrame; ATree: TTreeView;
  AllowPascalHelpers, AllowPythonHelpers: boolean): boolean;
var
  Ed, Ed2: TATSynEdit;
  CurLexer, CurFilename: string;
  TreeData: PPyObject;
  ParamFilename, ParamText: PPyObject;
  NHelper: integer;
  Helper: TAppTreeHelper;
begin
  Result:= false;

  Ed:= Frame.Editor;
  if Frame.EditorsLinked and Frame.Splitted then //Ed2 should be Nil when tab is not splitted
    Ed2:= Frame.EditorBrother
  else
    Ed2:= nil;

  CurLexer:= Frame.LexerName[Ed];
  if CurLexer='' then exit;

  //don't run helper on huge files
  if Ed.Strings.Count > EControlOptions.MaxLinesWhenParserEnablesFolding then exit;

  //Pascal helpers
  if AllowPascalHelpers then
  begin
    Result:= DoCodetree_ApplyTreeHelperInPascal(Ed, Ed2, ATree, CurLexer);
    if Result then exit;
  end;

  //Python helpers
  if AllowPythonHelpers and AppPython.Inited and Assigned(ATree) then
  with AppPython.Engine do
    for NHelper:= 0 to AppTreeHelpers.Count-1 do
    begin
      Helper:= TAppTreeHelper(AppTreeHelpers[NHelper]);
      if not IsLexerListed(CurLexer, Helper.ItemLexers) then Continue;

      CurFilename:= Frame.GetFileName(Ed);
      ParamText:= EditorTextToPyObject(Ed);
      ParamFilename:= PyUnicodeFromString(CurFilename);

      try
        TreeData:= AppPython.RunModuleFunction(Helper.ItemModule, Helper.ItemProc, [ParamFilename, ParamText]);
        try
          //refresh CodeTree only if TreeData is list
          if PyObject_TypeCheck(TreeData, PyList_Type) then
            DoCodetree_ApplyTreeHelperResults(ATree, TreeData);
        finally
          Py_DECREF(TreeData);
        end;
      except
      end;

      //Py_DECREF(ParamText);
      //Py_DECREF(ParamFilename);
      exit(true);
    end;
end;


function ParsePythonCallback_DotCommand(const AStr: string; out APart1, APart2: string): boolean;
var
  NLen, NPosDot, i: integer;
begin
  Result:= false;
  APart1:= '';
  APart2:= '';

  NLen:= Length(AStr);
  if NLen<3 then exit;

  i:= 1;
  while (i<=NLen) and IsCharWordA(AStr[i]) do Inc(i);
  if (i>=NLen) or (AStr[i]<>'.') then exit;

  NPosDot:= i;
  Inc(i);
  while (i<=NLen) and IsCharWordA(AStr[i]) do Inc(i);
  if i<=NLen then exit;

  APart1:= Copy(AStr, 1, NPosDot-1);
  APart2:= Copy(AStr, NPosDot+1, MaxInt);
  Result:= true;
end;


function ParsePythonCallback_CommaCommand(const AStr: string; out APart1, APart2, APart3: string): boolean;
// regex '(\w+),(\w+)(,(.+))?$'
var
  NLen, NSep, i: integer;
begin
  Result:= false;
  APart1:= '';
  APart2:= '';
  APart3:= '';

  NLen:= Length(AStr);
  if NLen<3 then exit;

  i:= 1;
  while (i<=NLen) and IsCharWordA(AStr[i]) do Inc(i);
  if (i>=NLen) or (AStr[i]<>',') then exit;

  NSep:= i;
  Inc(i);
  while (i<=NLen) and IsCharWordA(AStr[i]) do Inc(i);
  if i>NLen then
  begin
    APart1:= Copy(AStr, 1, NSep-1);
    APart2:= Copy(AStr, NSep+1, MaxInt);
  end
  else
  begin
    if AStr[i]<>',' then exit;
    APart1:= Copy(AStr, 1, NSep-1);
    APart2:= Copy(AStr, NSep+1, i-NSep-1);
    APart3:= Copy(AStr, i+1, MaxInt);
  end;

  Result:= true;
end;


function ParsePythonCallback_3Prefixes(const AStr, APrefix1, APrefix2, APrefix3: string;
  out APart1, APart2, APart3: string): boolean;
//   cRegex_SignCommand = 'module=(.+?);cmd=(.+?);(info=(.+?);)?$';
var
  NLen, NSep1, NSep2: integer;
  LenPrefix1, LenPrefix2, LenPrefix3: integer;
begin
  Result:= false;
  APart1:= '';
  APart2:= '';
  APart3:= '';

  NLen:= Length(AStr);
  LenPrefix1:= Length(APrefix1);
  LenPrefix2:= Length(APrefix2);
  LenPrefix3:= Length(APrefix3);

  if NLen<LenPrefix1+LenPrefix2+LenPrefix3+4 then exit;
  if not EndsStr(';', AStr) then exit;
  if not StartsStr(APrefix1, AStr) then exit;

  NSep1:= PosEx(';'+APrefix2, AStr, LenPrefix1);
  if NSep1=0 then exit;
  APart1:= Copy(AStr, LenPrefix1+1, NSep1-LenPrefix1-1);
  if APart1='' then exit;

  NSep2:= PosEx(';'+APrefix3, AStr, NSep1+LenPrefix2);
  if NSep2>0 then
  begin
    APart2:= Copy(AStr, NSep1+1+LenPrefix2, NSep2-NSep1-LenPrefix2-1);
    APart3:= Copy(AStr, NSep2+1+LenPrefix3, NLen-NSep2-LenPrefix3-1);
  end
  else
  begin
    NSep2:= PosEx(';', AStr, NSep1+LenPrefix2);
    if NSep2=0 then exit;
    APart2:= Copy(AStr, NSep1+1+LenPrefix2, NSep2-NSep1-LenPrefix2-1);
  end;
  if APart2='' then exit;

  Result:= true;
end;


function DoPyCallbackFromAPI_2(const ACallback: string;
  AEd: TATSynEdit;
  const AParams: TAppVariantArray): TAppPyEventResult;
var
  SModule, SFunc: string;
begin
  Result:= Default(TAppPyEventResult);

  if ParsePythonCallback_DotCommand(ACallback, SModule, SFunc) then
  begin
    Result:= AppPython.RunEvent(SModule, SFunc, AEd, AParams, false);
    exit;
  end;

  MsgLogConsole(Format(msgCallbackBad, [ACallback]));
end;


function DoPyCallbackFromAPI(const ACallback: string;
  const AParamVars: TAppVariantArray;
  const AParamNames: array of string): boolean;
const
  cPrefixExec = 'exec=';
var
  FParamObjs: array of PPyObject;
  FParamNames: array of string;
  FParamVars: TAppVariantArray;
  SModule, SFunc, SParam, SInfo: string;
  Obj: PPyObject;
  i: integer;
begin
  Result:= true;
  if not AppPython.Inited then exit;

  //avoid log error on standard ids
  if SBeginsWith(ACallback, 'top-') then exit;

  //-----------------------------------------------------
  if SBeginsWith(ACallback, cPrefixExec) then
  begin
    SParam:= Copy(ACallback, Length(cPrefixExec)+1, MaxInt);
    fmMain.DoLoadCommandLine_FromString(SParam);
    exit;
  end;

  //-----------------------------------------------------
  if ParsePythonCallback_DotCommand(ACallback, SModule, SFunc) then
  begin
    Result:= AppPython.RunCommand(SModule, SFunc, AParamVars);
    exit;
  end;

  //-----------------------------------------------------
  if ParsePythonCallback_3Prefixes(ACallback, 'module=', 'cmd=', 'info=', SModule, SFunc, SInfo) then
  begin
    if SInfo<>'' then
    begin
      FParamVars:= nil;
      SetLength(FParamVars, Length(AParamVars)+1);
      for i:= 0 to Length(AParamVars)-1 do
        FParamVars[i]:= AParamVars[i];
      FParamVars[Length(FParamVars)-1]:= AppVariant(SInfo);

      Result:= AppPython.RunCommand(SModule, SFunc, FParamVars);
    end
    else
      Result:= AppPython.RunCommand(SModule, SFunc, AParamVars);

    exit;
  end;

  //-----------------------------------------------------
  if ParsePythonCallback_3Prefixes(ACallback, 'module=', 'func=', 'info=', SModule, SFunc, SInfo) then
  begin
    FParamObjs:= nil;
    SetLength(FParamObjs, Length(AParamVars));
    for i:= 0 to Length(AParamVars)-1 do
      FParamObjs[i]:= AppVariantToPyObject(AParamVars[i]);

    FParamNames:= nil;
    SetLength(FParamNames, Length(AParamNames));
    for i:= 0 to Length(AParamNames)-1 do
      FParamNames[i]:= AParamNames[i];

    if SInfo<>'' then
    begin
      // SInfo may have simple type values: string, int, bool...
      SetLength(FParamObjs, Length(FParamObjs)+1);
      FParamObjs[Length(FParamObjs)-1]:= AppPython.ValueFromString(SInfo);
      SetLength(FParamNames, Length(FParamNames)+1);
      FParamNames[Length(FParamNames)-1]:= 'info';
    end;

    Obj:= AppPython.RunModuleFunction(SModule, SFunc, FParamObjs, FParamNames);
    if Assigned(Obj) then
      with AppPython.Engine do
      begin
        //only check for False
        Result:= Pointer(Obj)<>Pointer(Py_False);
        Py_DECREF(Obj);
      end;

    exit;
  end;

  //-----------------------------------------------------
  if ParsePythonCallback_CommaCommand(ACallback, SModule, SFunc, SParam) then
  begin
    if SParam<>'' then
      FParamVars:= [AppVariant(SParam)]
    else
      FParamVars:= nil;

    Result:= AppPython.RunCommand(SModule, SFunc, FParamVars);
    MsgLogConsole(Format(msgCallbackDeprecated, [ACallback]));
    exit;
  end;

  MsgLogConsole(Format(msgCallbackBad, [ACallback]));
end;


procedure TfmMain.DoPyResetPlugins;
var
  fn, Cmd: string;
  L: TStringList;
begin
  if not AppPython.Inited then exit;

  fn:= AppDir_Py+DirectorySeparator+'cudatext_reset_plugins.py';
  if not FileExists(fn) then
  begin
    MsgBox(msgCannotFindFile+#10+AppCollapseHomeDirInFilename(fn), MB_OK or MB_ICONERROR);
    Exit
  end;

  L:= TStringList.Create;
  try
    Cmd:= Format('_reset_plugins(r"%s")', [AppDir_Py]);
    L.LoadFromFile(fn);
    L.Add(Cmd);
    AppPython.Exec(L.Text);
  finally
    FreeAndNil(L)
  end;

  AppPython.ClearCache;

  //support on_start, issue #1253
  DoPyEvent(nil, cEventOnStart, []);
end;

procedure TfmMain.DoPyRescanPlugins;
begin
  if not AppPython.Inited then exit;
  DoOps_LoadPlugins(true);
  UpdateMenuPlugins;
  MsgStatus(msgRescannedAllPlugins);
end;

procedure TfmMain.DoPyRunLastPlugin(AInvoke: TATCommandInvoke);
var
  Params: TAppVariantArray;
begin
  if not AppPython.Inited then exit;
  if AppPython.LastCommandModule='' then exit;
  if AppPython.LastCommandParam<>'' then
    Params:= [AppVariant(AppPython.LastCommandParam)]
  else
    Params:= [];

  DoPyCommand(AppPython.LastCommandModule, AppPython.LastCommandMethod, Params, AInvoke);
end;


procedure TfmMain.DoPyTimerTick(Sender: TObject);
var
  Timer: TTimer;
  SCallback, STag: string;
  NIndex: integer;
begin
  Timer:= Sender as TTimer;
  NIndex:= AppListTimers.IndexOfObject(Timer);
  if NIndex<0 then exit;

  //Timer.Tag=1 for TIMER_START_ONE action
  if Timer.Tag=1 then
    Timer.Enabled:= false;

  SSplitByChar(AppListTimers[NIndex], '|', SCallback, STag);

  //timer_proc callback must pass 'tag' as str!
  DoPyCallbackFromAPI(
    SCallback,
    [AppVariant(STag)],
    ['tag']
    );
end;

procedure TfmMain.DoPyCommand_CommandLineParam(const AModuleAndMethod: string);
var
  SModule, SMethod: string;
begin
  SSplitByChar(AModuleAndMethod, ',', SModule, SMethod);
  if (SModule<>'') and (SMethod<>'') then
    DoPyCommand(SModule, SMethod, [], TATCommandInvoke.AppInternal);
end;

