{$ifdef nn}begin end;{$endif}


procedure TfmMain.DoPyCommand(const AModule, AMethod: string;
  const AParams: array of string);
var
  Frame: TEditorFrame;
  Ed: TATSynEdit;
begin
  if not PythonOK then exit;
  PyLastCommandModule:= AModule;
  PyLastCommandMethod:= AMethod;
  PyLastCommandParam:= '';
  if Length(AParams)>0 then
    PyLastCommandParam:= AParams[0];
  PyEditorMaybeDeleted:= false;

  Frame:= CurrentFrame;
  if Frame=nil then exit;
  Ed:= CurrentEditor;
  if Ed=nil then exit;

  if Frame.MacroRecord then
    Frame.MacroString:= Frame.MacroString+ ('py:'+AModule+','+AMethod+','+PyLastCommandParam+#10);

  Ed.Strings.BeginUndoGroup;
  PyCommandRunning:= true;
  try
    Py_RunPlugin_Command(AModule, AMethod, AParams);
  finally
    PyCommandRunning:= false;
    if not PyEditorMaybeDeleted then
      Ed.Strings.EndUndoGroup;
  end;
end;


procedure TfmMain.DoPyCommand_ByPluginIndex(AIndex: integer);
var
  F: TEditorFrame;
  CurLexer: string;
  CmdItem: TAppCommand;
begin
  if not ((AIndex>=0) and (AIndex<AppCommandList.Count)) then exit;
  CmdItem:= TAppCommand(AppCommandList[AIndex]);

  F:= CurrentFrame;
  if F=nil then exit;
  CurLexer:= F.LexerName[F.Editor];

  if not IsLexerListed(CurLexer, CmdItem.ItemLexers) then
  begin
    MsgStatus(msgStatusCommandOnlyForLexers+' '+CmdItem.ItemLexers);
    Exit
  end;

  DoPyCommand(
    CmdItem.ItemModule,
    CmdItem.ItemProc,
    CmdItem.ItemProcParam
    );
end;


function TfmMain.DoPyEvent(AEd: TATSynEdit; AEvent: TAppPyEvent;
  const AParams: array of string): string;
const
  cTheseEventsStoppedByTrue = [cEventOnComplete{, cEventOnTabSwitch}];
  cTheseEventsNeedGroupingUndo = [cEventOnComplete, cEventOnSnippet];
var
  SCurLexer: string;
  bNeedGroup: boolean;
  Frame: TEditorFrame;
  MaxPrior, i_plugin, i_prior: integer;
begin
  //empty string result means "no handlers for event"
  Result:= '';
  if not PythonOK then exit;

  if (AEvent=cEventOnFocus) and not FAllowOnFocus then exit;

  Frame:= GetEditorFrame(AEd);
  if Frame=nil then exit;

  SCurLexer:= Frame.LexerName[AEd];
  bNeedGroup:= AEvent in cTheseEventsNeedGroupingUndo;
  MaxPrior:= AppEventMaxPriority(AEvent);

  //see all items for prior=MaxPrior..0
  for i_prior:= MaxPrior downto 0 do
    for i_plugin:= 0 to AppEventList.Count-1 do
      with TAppEvent(AppEventList[i_plugin]) do
      begin
        if not (AEvent in ItemEvents) then Continue;
        if (i_prior<>ItemEventsPrior[AEvent]) then Continue;
        if not ((ItemLexers='') or IsLexerListed(SCurLexer, ItemLexers)) then Continue;

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
          Result:= Py_RunPlugin_Event(
              ItemModule,
              cAppPyEvent[AEvent],
              AEd,
              AParams,
              (AEvent=cEventOnExit) or ItemEventsLazy[AEvent] //on_exit must be lazy
              );
        finally
          PyCommandRunning:= false;
          if bNeedGroup then
            AEd.Strings.EndUndoGroup;
        end;

        //True for some events means "stop"
        if Result=cPyTrue then
          if AEvent in cTheseEventsStoppedByTrue then Exit;

        //False means "stop", other results ignored
        if Result=cPyFalse then Exit;
      end;
end;


function EditorTextToPyObject(Ed: TATSynEdit): PPyObject;
var
  Count, i: integer;
  Strs: TATStrings;
  S: string;
begin
  Strs:= Ed.Strings;
  Count:= Strs.Count;
  with GetPythonEngine do
  begin
    Result:= PyList_New(Count);
    for i:= 0 to Count-1 do
    begin
      S:= Strs.LinesUTF8[i];
      PyList_SetItem(Result, i, PyString_FromString(PChar(S)));
    end;
  end;
end;


function TfmMain.DoPyTreeHelper(Frame: TEditorFrame): boolean;
var
  Ed: TATSynEdit;
  CurLexer, CurFilename: string;
  TreeData: PPyObject;
  ParamFilename, ParamText: PPyObject;
  NHelper: integer;
  Helper: TAppTreeHelper;
begin
  Result:= false;
  if not PythonOK then exit;

  Ed:= Frame.Editor;
  CurLexer:= Frame.LexerName[Ed];
  if CurLexer='' then exit;
  CurFilename:= Frame.GetFileName(Ed);

  with GetPythonEngine do
    for NHelper:= 0 to AppTreeHelpers.Count-1 do
    begin
      Helper:= TAppTreeHelper(AppTreeHelpers[NHelper]);
      if not IsLexerListed(CurLexer, Helper.ItemLexers) then Continue;

      ParamText:= EditorTextToPyObject(Ed);
      ParamFilename:= PyString_FromString(PChar(CurFilename));

      try
        TreeData:= Py_RunModuleFunction(Helper.ItemModule, Helper.ItemProc, [ParamFilename, ParamText]);
        try
          //refresh CodeTree only if TreeData is list
          if PyObject_TypeCheck(TreeData, PyList_Type) then
            DoCodetree_ApplyTreeHelperResults(TreeData);
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


function DoPyCallbackFromAPI(const ACallback: string;
  const AParams: array of PPyObject;
  const AParamNames: array of string): boolean;
const
  cRegex_DotCommand = '([a-z_]\w*)\.([a-z_]\w*)$';
  cRegex_SignCommand = 'module=(.+?);cmd=(.+?);(info=(.+?);)?$';
  cRegex_SignFunc = 'module=(.+?);func=(.+?);(info=(.+?);)?$';
  cRegex_CommaCommand = '([a-z_]\w*),([a-z_]\w*)(,(.+))?$'; //compatible with menu_proc, with 3rd param
var
  FParams: array of PPyObject;
  FParamsStr: array of string;
  FParamNames: array of string;
  SModule, SFunc, SParam, SInfo: string;
  Parts: TRegexParts;
  Obj: PPyObject;
  i: integer;
begin
  Result:= true;

  //avoid log error on standard ids
  if SBeginsWith(ACallback, 'top-') then exit;

  SetLength(FParams, Length(AParams));
  SetLength(FParamsStr, Length(AParams));
  for i:= 0 to Length(AParams)-1 do
  begin
    FParams[i]:= AParams[i];
    FParamsStr[i]:= Py_SimpleValueToString(AParams[i], true);
  end;
  SetLength(FParamNames, Length(AParamNames));
  for i:= 0 to Length(AParamNames)-1 do
    FParamNames[i]:= AParamNames[i];

  if SRegexFindParts(cRegex_DotCommand, ACallback, Parts) then
  begin
    SModule:= Parts[1].Str;
    SFunc:= Parts[2].Str;
    Result:= Py_RunPlugin_Command(SModule, SFunc, FParamsStr);
    exit;
  end;

  if SRegexFindParts(cRegex_SignCommand, ACallback, Parts) then
  begin
    SModule:= Parts[1].Str;
    SFunc:= Parts[2].Str;
    SInfo:= Parts[4].Str;

    if SInfo<>'' then
    begin
      SetLength(FParamsStr, Length(FParamsStr)+1);
      FParamsStr[Length(FParamsStr)-1]:= SInfo;
      SetLength(FParamNames, Length(FParamNames)+1);
      FParamNames[Length(FParamNames)-1]:= 'info';
    end;

    Result:= Py_RunPlugin_Command(SModule, SFunc, FParamsStr);
    exit;
  end;

  if SRegexFindParts(cRegex_SignFunc, ACallback, Parts) then
  begin
    SModule:= Parts[1].Str;
    SFunc:= Parts[2].Str;
    SInfo:= Parts[4].Str;

    if SInfo<>'' then
    begin
      // SInfo may have simple type values: string, int, bool...
      SetLength(FParams, Length(FParams)+1);
      FParams[Length(FParams)-1]:= Py_SimpleValueFromString(SInfo);
      SetLength(FParamNames, Length(FParamNames)+1);
      FParamNames[Length(FParamNames)-1]:= 'info';
    end;

    Obj:= Py_RunModuleFunction(SModule, SFunc, FParams, FParamNames);
    if Assigned(Obj) then
      with GetPythonEngine do
      begin
        Result:= not PyBool_Check(Obj) or (PyObject_IsTrue(Obj)=1);
        Py_DECREF(Obj);
      end;

    exit;
  end;

  if SRegexFindParts(cRegex_CommaCommand, ACallback, Parts) then
  begin
    SModule:= Parts[1].Str;
    SFunc:= Parts[2].Str;
    SParam:= Parts[4].Str;
    Result:= Py_RunPlugin_Command(SModule, SFunc, [SParam]);
    MsgLogConsole(Format(msgCallbackDeprecated, [ACallback]));
    exit;
  end;

  MsgLogConsole(Format(msgCallbackBad, [ACallback]));
end;


procedure TfmMain.DoPyUpdateEvents(const AModuleName, AEventStr, ALexerStr, AKeyStr: string);
var
  EventItem: TAppEvent;
  i: integer;
begin
  //find index of plugin (get first empty index if not listed)
  EventItem:= nil;
  for i:= 0 to AppEventList.Count-1 do
    with TAppEvent(AppEventList[i]) do
      if (ItemModule=AModuleName) then
      begin
        EventItem:= TAppEvent(AppEventList[i]);
        Break
      end;

  if EventItem=nil then
  begin
    EventItem:= TAppEvent.Create;
    AppEventList.Add(EventItem);
  end;

  //update item
  with EventItem do
  begin
    if ItemModule='' then
      ItemModule:= AModuleName;
    DoPyStringToEvents(AEventStr, ItemEvents, ItemEventsPrior, ItemEventsLazy);
    ItemLexers:= ALexerStr;
    ItemKeys:= AKeyStr;
  end;
end;


procedure TfmMain.DoPyResetPlugins;
var
  fn, Cmd: string;
  L: TStringList;
begin
  fn:= AppDir_Py+DirectorySeparator+'cudatext_reset_plugins.py';
  if not FileExists(fn) then
  begin
    MsgBox(msgCannotFindFile+#13+fn, MB_OK or MB_ICONERROR);
    Exit
  end;

  L:= TStringList.Create;
  try
    L.LoadFromFile(fn);
    GetPythonEngine.ExecStrings(L);
    Cmd:= Format('_reset_plugins(r"%s")', [AppDir_Py]);
    GetPythonEngine.ExecString(Cmd);
  finally
    FreeAndNil(L)
  end;

  PyClearLoadedModuleLists;

  //https://github.com/Alexey-T/CudaText/issues/1253
  DoPyEvent(nil, cEventOnStart, []);
end;

procedure TfmMain.DoPyRescanPlugins;
begin
  DoOps_LoadPlugins;
  UpdateMenuPlugins;
  DoOps_LoadKeymap(false);
  MsgStatus(msgRescannedAllPlugins);
end;

procedure TfmMain.DoPyRunLastPlugin;
begin
  if PyLastCommandModule<>'' then
    DoPyCommand(PyLastCommandModule, PyLastCommandMethod, PyLastCommandParam);
end;


procedure TfmMain.DoPyTimerTick(Sender: TObject);
var
  Timer: TTimer;
  SCallback, STag: string;
  Params: array[0..0] of PPyObject;
  ParamNames: array[0..0] of string;
  N: integer;
begin
  Timer:= Sender as TTimer;
  N:= FListTimers.IndexOfObject(Timer);
  if N<0 then exit;

  //if timer_start_one
  if Timer.Tag=1 then
    Timer.Enabled:= false;

  SSplitByChar(FListTimers[N], '|', SCallback, STag);

  with GetPythonEngine do
    Params[0]:= PyLong_FromLongLong(StrToInt64Def(STag, 0));
  ParamNames[0]:= 'tag';

  DoPyCallbackFromAPI(SCallback, Params, ParamNames);
end;


