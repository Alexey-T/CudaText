(*
This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at http://mozilla.org/MPL/2.0/.

Copyright (c) Alexey Torgashin
*)
{$ifdef nn}begin end;{$endif}

const
  cPyEditorHandleMin = 10;
  cPyEditorHandleMax = 10000;
  cPyTokenKind: array[TATTokenKind] of string = ('a', 'c', 's');

var
  PyEscapeFlag: boolean = false;
  PyEditorMaybeDeleted: boolean = false;
  PyCurrentFolder: string = '';

var
  AppEmmet: TEmmet = nil;

function Py_ObjectHandle(Obj: TObject): PPyObject; inline;
begin
  with AppPython.Engine do
    Result:= PyLong_FromLongLong(Int64(PtrInt(Obj)))
end;

procedure MsgOldApi(const s: string);
begin
  MsgLogConsole(Format(msgApiDeprecated, [s]));
end;

function AppSendMessageFromString(const AText: string): integer;
var
  H: Int64;
  Msg: DWord;
  Param1, Param2: integer;
  Sep: TATStringSeparator;
begin
  Sep.Init(AText);
  Sep.GetItemInt64(H, 0);
  if H=0 then exit(0);
  Sep.GetItemDWord(Msg, 0);
  if Msg=0 then exit(0);
  Sep.GetItemInt(Param1, 0);
  Sep.GetItemInt(Param2, 0);
  Result:= SendMessage(HWND(H), Cardinal(Msg), Param1, Param2);
end;

//func at begin
function PyEditor(Handle: Int64): TATSynEdit;
var
  nTab: Integer;
begin
  Result:= nil;
  if not Assigned(fmMain) then exit;

  if (Handle<0) or (Handle>cPyEditorHandleMax) then
    exit(TATSynEdit(PtrInt(Handle))); //Access Violation for incorrect handle

  case Handle of
    0: //"ed"
      Result:= fmMain.CurrentEditor;
    {
    1: //"ed_bro"
      Result:= GetEditorBrother(fmMain.CurrentEditor);

    2: //"ed_1st"
      Result:= GetEditorFirstSecond(fmMain.CurrentEditor, true);
    3: //"ed_2nd"
      Result:= GetEditorFirstSecond(fmMain.CurrentEditor, false);
    }

    7: //"ed_con_log"
      Result:= fmConsole.EdMemo;
    8: //"ed_con_in"
      Result:= fmConsole.EdInput;

    cPyEditorHandleMin..cPyEditorHandleMax:
      begin
        nTab:= Handle-cPyEditorHandleMin;
        if (nTab>=0) and (nTab<fmMain.FrameCount) then
          Result:= fmMain.Frames[nTab].Ed1;
      end;
  end;
end;

function Py_rect(const R: TRect): PPyObject; cdecl;
begin
  with AppPython.Engine do
    Result:= Py_BuildValue('(iiii)', R.Left, R.Top, R.Right, R.Bottom);
end;

function Py_rect_monitor(N: Integer): PPyObject; cdecl;
begin
  if (N>=0) and (N<Screen.MonitorCount) then
    Result:= Py_rect(Screen.Monitors[N].BoundsRect)
  else
    Result:= AppPython.Engine.ReturnNone;
end;

function Py_rect_control(C: TControl): PPyObject; cdecl;
var
  Pnt: TPoint;
  R: TRect;
begin
  Pnt:= C.ClientToScreen(Point(0, 0));
  R.Left:= Pnt.X;
  R.Top:= Pnt.Y;
  R.Right:= Pnt.X + C.Width;
  R.Bottom:= Pnt.Y + C.Height;
  Result:= Py_rect(R);
end;

function Py_GetCaretProps(Props: TATCaretProps): PPyObject;
begin
  with AppPython.Engine do
    Result:= Py_BuildValue('(iiO)',
      Props.Width,
      Props.Height,
      PyBool_FromLong(Ord(Props.EmptyInside))
      );
end;

function Py_GetFontProps(F: TFont): PPyObject;
begin
  with AppPython.Engine do
    Result:= Py_BuildValue('(si)', PChar(F.Name), F.Size);
end;

function Py_GetThemeDict_UI: PPyObject;
var
  iColor: TAppThemeColorId;
  ColorItem: TAppThemeColor;
begin
  with AppPython.Engine do
  begin
    Result:= PyDict_New();
    if not Assigned(Result) then
      raise EPythonError.Create(msgPythonListError);
    for iColor:= Low(iColor) to High(iColor) do
    begin
      ColorItem:= AppTheme.Colors[iColor];
      PyDict_SetItemString(Result,
        PChar(ColorItem.name),
        Py_BuildValue('{sssi}',
          'desc',
          PChar(ColorItem.desc),
          'color',
          ColorItem.color
          ));
    end;
  end;
end;


function Py_GetLineStateList(Ed: TATSynEdit): PPyObject;
var
  Strs: TATStrings;
  NLen, i: integer;
begin
  with AppPython.Engine do
  begin
    Strs:= Ed.Strings;
    NLen:= Strs.Count;
    Result:= PyList_New(NLen);
    if not Assigned(Result) then
      raise EPythonError.Create(msgPythonListError);
    for i:= 0 to NLen-1 do
    begin
      PyList_SetItem(Result, i, PyInt_FromLong(Ord(Strs.LinesState[i])));
    end;
  end;
end;

function Py_GetLineUpdatedList(Ed: TATSynEdit): PPyObject;
var
  Strs: TATStrings;
  NLen, i: integer;
begin
  with AppPython.Engine do
  begin
    Strs:= Ed.Strings;
    NLen:= Strs.Count;
    Result:= PyList_New(NLen);
    if not Assigned(Result) then
      raise EPythonError.Create(msgPythonListError);
    for i:= 0 to NLen-1 do
    begin
      PyList_SetItem(Result, i, PyBool_FromLong(Ord(Strs.LinesUpdated[i])));
    end;
  end;
end;


function AppEmmetOptions(const ASyntax: string): TExpandOptions;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result.AlwaysAddNewLine:= ASyntax = 'xml';
  Result.AddSlashToEmptyTags:= UiOps.Emmet_AddSlashToEmptyTags;
  Result.CommentTags:= UiOps.Emmet_CommentTags;
  Result.IndentChilds:= UiOps.Emmet_IndentNested;
  Result.SingleLine:= UiOps.Emmet_SingleLine;
  Result.TabSize:= EditorOps.OpTabSize;
  Result.TrimLineMarkers:= UiOps.Emmet_TrimLineMarkers;
  Result.Wordwrap:= UiOps.Emmet_WordWrap;
  Result.WordwrapAt:= EditorOps.OpMarginFixed;
  Result.AlwaysReturnIndexedTabStops:= true;
end;

function Py_emmet(Self, Args: PPyObject): PPyObject; cdecl;
var
  Id: integer;
  PtrText, Ptr1, Ptr2: PChar;
  StrText, Str1, Str2: string;
var
  dir, filenameSnips, filenameLorem,
  SSection: string;
  SAbbreb: UnicodeString;
  bMulLine: boolean;
  NPos, NRes: integer;
begin
  with AppPython.Engine do
  if Bool(PyArg_ParseTuple(Args, 'isss:emmet', @Id, @PtrText, @Ptr1, @Ptr2)) then
  begin
    StrText:= string(PtrText);
    Str1:= string(Ptr1);
    Str2:= string(Ptr2);

    if AppEmmet=nil then
    begin
      dir:= AppDir_DataAutocompleteSpec;
      filenameSnips:= dir+DirectorySeparator+'emmet_snippets.ini';
      filenameLorem:= dir+DirectorySeparator+'emmet_lorem.txt';
      if not FileExistsUTF8(filenameSnips) then exit(ReturnNone);
      if not FileExistsUTF8(filenameLorem) then exit(ReturnNone);
      AppEmmet:= TEmmet.Create(filenameSnips, filenameLorem);
    end;

    case Id of
      EMMET_EXPAND:
        begin
          StrText:= AppEmmet.ExpandAbbreviation(
            StrText,
            Str1,
            '',
            SSection,
            bMulLine,
            AppEmmetOptions(Str1)
            );
          StrText:= StringReplace(StrText, #13#10, #10, [rfReplaceAll]);
          Result:= Py_BuildValue('(sO)',
            PChar(StrText),
            PyBool_FromLong(Ord(bMulLine))
            );
        end;

      EMMET_WRAP:
        begin
          Str2:= StringReplace(Str2, #10, #13#10, [rfReplaceAll]);
          StrText:= AppEmmet.ExpandAbbreviation(
            StrText,
            Str1,
            Str2,
            SSection,
            bMulLine,
            AppEmmetOptions(Str1)
            );
          StrText:= StringReplace(StrText, #13#10, #10, [rfReplaceAll]);
          Result:= PyString_FromString(PChar(StrText));
        end;

      EMMET_GET_POS:
        begin
          NPos:= StrToIntDef(Str1, -1);
          if StrText='' then
            exit(ReturnNone);
          if NPos<=0 then
            exit(ReturnNone);
          EmmetHelper.EmmetFindAbbrev(StrText, NPos, NRes, SAbbreb);
          Result:= PyInt_FromLong(NRes);
        end;

      else
        Result:= ReturnNone;
    end;
  end
  else
    Result:= ReturnNone;
end;


function Py_GetThemeDict_Syntax: PPyObject;
var
  iStyle: TAppThemeStyleId;
  St: TecSyntaxFormat;
begin
  with AppPython.Engine do
  begin
    Result:= PyDict_New();
    if not Assigned(Result) then
      raise EPythonError.Create(msgPythonListError);
    for iStyle:= Low(iStyle) to High(iStyle) do
    begin
      St:= AppTheme.Styles[iStyle];
      PyDict_SetItemString(Result,
        PChar(St.DisplayName),
        Py_BuildValue('{sisisisisisisisiss}',
          'type',
          Ord(St.FormatType),
          'color_font',
          St.Font.Color,
          'color_back',
          St.BgColor,
          'color_border',
          St.BorderColorBottom,
          'border_left',
          Ord(St.BorderTypeLeft),
          'border_right',
          Ord(St.BorderTypeRight),
          'border_top',
          Ord(St.BorderTypeTop),
          'border_bottom',
          Ord(St.BorderTypeBottom),
          'styles',
          PChar(FontStylesToString(St.Font.Style))
          ));
    end;
  end;
end;

function Py_ImagelistFromId(const Str: string): TImageList;
var
  Obj: TObject;
  N: PtrInt;
begin
  Result:= nil;
  N:= StrToInt64Def(Str, 0);
  if N<>0 then
  begin
    Obj:= TObject(N);
    if Obj is TImageList then
      exit(TImageList(Obj));
  end;
end;

function Py_MenuFromId(const Str: string): TMenu;
var
  Obj: TObject;
  N: PtrInt;
begin
  Result:= nil;
  N:= StrToInt64Def(Str, 0);
  if N<>0 then
  begin
    Obj:= TObject(N);
    if Obj is TMenu then
      exit(TMenu(Obj));
  end;
end;

function Py_MenuItemFromId(Str: string): TMenuItem;
const
  cPrefixToolmenu = 'toolmenu:';
var
  Obj: TObject;
  N: PtrInt;
begin
  Result:= nil;

  N:= StrToInt64Def(Str, 0);
  if N<>0 then
  begin
    Obj:= TObject(N);
    if Obj is TMenuItem then
      exit(TMenuItem(Obj))
    else
      exit;
  end;

  if Str=PyMenuId_Top then exit(fmMain.MainMenu.Items);
  if Str=PyMenuId_TopEdit then exit(fmMain.mnuEdit);
  if Str=PyMenuId_TopSel then exit(fmMain.mnuSel);
  if Str=PyMenuId_TopSearch then exit(fmMain.mnuSr);
  if Str=PyMenuId_TopFile then exit(fmMain.mnuFile);
  if Str=PyMenuId_TopView then exit(fmMain.mnuView);
  if Str=PyMenuId_TopOptions then exit(fmMain.mnuOp);
  if Str=PyMenuId_TopHelp then exit(fmMain.mnuHelp);
  if Str=PyMenuId_TopGroups then exit(fmMain.mnuGroups);

  if Str=PyMenuId_Text then exit(fmMain.PopupText.Items);
  if Str=PyMenuId_Tab then
  begin
    fmMain.InitPopupTab;
    exit(fmMain.PopupTab.Items);
  end;

  if SBeginsWith(Str, cPrefixToolmenu) then
  begin
    for N:= 0 to fmMain.ToolbarMain.ButtonCount-1 do
      if fmMain.ToolbarMain.Buttons[N].DataString=Str then
        exit(fmMain.ToolbarMain.Buttons[N].PopupMenu.Items);
  end;
end;

procedure TfmMain.PythonIOSendUniData(Sender: TObject; const Data: UnicodeString);
var
  Sep: TATStringSeparator;
  S: UnicodeString;
begin
  if Pos(#10, Data)=0 then
    AppConsoleQueue.Push(Data)
  else
  begin
    Sep.Init(Data, #10);
    while Sep.GetItemStr(S) do
      AppConsoleQueue.Push(S);
  end;
end;


function Py_app_exe_version(Self, Args : PPyObject): PPyObject; cdecl;
begin
  with AppPython.Engine do
    Result:= PyString_FromString(cAppExeVersion);
end;

function Py_app_ver(Self, Args : PPyObject): PPyObject; cdecl;
begin
  with AppPython.Engine do
    Result:= Py_BuildValue('(is)',
      cAppApiVersion,
      cAppExeVersion
      );
end;

function Py_app_path(Self, Args : PPyObject): PPyObject; cdecl;
var
  Id: integer;
begin
  with AppPython.Engine do
    if Bool(PyArg_ParseTuple(Args, 'i:app_path', @Id)) then
      case Id of
        APP_DIR_EXE:
          Result:= PyString_FromString(PChar(ExtractFileDir(Application.ExeName)));
        APP_DIR_SETTINGS:
          Result:= PyString_FromString(PChar(AppDir_Settings));
        APP_DIR_DATA:
          Result:= PyString_FromString(PChar(AppDir_Data));
        APP_DIR_PY:
          Result:= PyString_FromString(PChar(AppDir_Py));
        APP_FILE_SESSION:
          Result:= PyString_FromString(PChar(fmMain.GetSessionFilename));
        APP_DIR_INSTALLED_ADDON:
          Result:= PyString_FromString(PChar(AppDir_LastInstalledAddon));
        APP_FILE_RECENTS:
          Result:= PyString_FromString(PChar(AppListRecents.Text));

        else
          Result:= ReturnNone;
      end
  else
    Result:= ReturnNone;
end;


function Py_app_idle(Self, Args: PPyObject): PPyObject; cdecl;
var
  Flag: LongBool;
begin
  with AppPython.Engine do
    if Bool(PyArg_ParseTuple(Args, 'i:app_idle', @Flag)) then
    begin
      Application.ProcessMessages;
      Application.Idle(Flag);
      Result:= AppPython.Engine.ReturnNone;
    end
  else
    Result:= ReturnNone;
end;


function Py_dlg_input(Self, Args : PPyObject): PPyObject; cdecl;
var
  P1, P2: PChar;
  StrCaption, StrVal: string;
begin
  with AppPython.Engine do
    if Bool(PyArg_ParseTuple(Args, 'ss:dlg_input', @P1, @P2)) then
    begin
      StrCaption:= string(P1);
      StrVal:= string(P2);
      if InputQuery(msgTitle, StrCaption, StrVal) then
        Result:= PyString_FromString(PChar(StrVal))
      else
        Result:= ReturnNone;
    end
  else
    Result:= ReturnNone;
end;

function Py_dlg_input_ex(Self, Args : PPyObject): PPyObject; cdecl;
var
  Number, i: integer;
  PCaption: PChar;
  PLabel, PText: array[1..10] of PChar;
  SCaption: string;
  Labels, Values: array of string;
  List: TStringList;
begin
  with AppPython.Engine do
    if Bool(PyArg_ParseTuple(Args, 'isssssssssssssssssssss:dlg_input_ex',
      @Number, @PCaption,
      @PLabel[1], @PText[1], @PLabel[2], @PText[2], @PLabel[3], @PText[3], @PLabel[4], @PText[4],
      @PLabel[5], @PText[5], @PLabel[6], @PText[6], @PLabel[7], @PText[7], @PLabel[8], @PText[8],
      @PLabel[9], @PText[9], @PLabel[10], @PText[10])) then
    begin
      Number:= Max(1, Min(10, Number));
      SCaption:= string(PCaption);
      SetLength(Labels, Number);
      SetLength(Values, Number);
      for i:= 1 to Number do
      begin
        Labels[i-1]:= string(PLabel[i]);
        Values[i-1]:= string(PText[i]);
      end;

      if InputQuery(SCaption, Labels, Values) then
      begin
        List:= TStringList.Create;
        try
          for i:= 0 to Number-1 do
            List.Add(Values[i]);
          Result:= StringsToPyList(List);
        finally
          FreeAndNil(List);
        end;
      end
      else
        Result:= ReturnNone;
    end;
end;


function Py_dlg_charmap(Self, Args : PPyObject): PPyObject; cdecl;
var
  str: string;
begin
  with AppPython.Engine do
    if Bool(PyArg_ParseTuple(Args, ':dlg_charmap')) then
    begin
      str:= DoDialogCharmapModal;
      if str<>'' then
        Result:= PyString_FromString(PChar(str))
      else
        Result:= ReturnNone;
    end
  else
    Result:= ReturnNone;
end;

function Py_msg_status(Self, Args: PPyObject): PPyObject; cdecl;
var
  P: PChar;
  Str: string;
  Flag: integer;
begin
  with AppPython.Engine do
  begin
    if Bool(PyArg_ParseTuple(Args, 'si:msg_status', @P, @Flag)) then
    begin
      Str:= string(P);
      fmMain.MsgStatus(Str);
      if Bool(Flag) then
        Application.ProcessMessages; //PluginManager calls loop with msg_status
    end;
    Result:= ReturnNone;
  end;
end;

function Py_msg_status_alt(Self, Args: PPyObject): PPyObject; cdecl;
var
  Ptr: PChar;
  Str: string;
  Num: integer;
begin
  with AppPython.Engine do
  begin
    if Bool(PyArg_ParseTuple(Args, 'si:msg_status_alt', @Ptr, @Num)) then
    begin
      Str:= string(Ptr);
      fmMain.DoTooltipShow(Str, Num, atpEditorCaret, false);
      Application.ProcessMessages; //PluginManager calls loop with msg_status
    end;
    Result:= ReturnNone;
  end;
end;


function Py_msg_box(Self, Args: PPyObject): PPyObject; cdecl;
var
  Ptr: PChar;
  Str: string;
  Flags: integer;
begin
  with AppPython.Engine do
    if Bool(PyArg_ParseTuple(Args, 'si:msg_box', @Ptr, @Flags)) then
    begin
      Str:= string(Ptr);
      Result:= PyInt_FromLong(MsgBox(Str, Flags));
    end
  else
    Result:= ReturnNone;
end;


function ConvertButtonIcon_MB_to_ID(N: integer): integer;
begin
  case N of
    MB_ICONWARNING:
      Result:= idDialogWarning;
    MB_ICONERROR:
      Result:= idDialogError;
    MB_ICONQUESTION:
      Result:= idDialogConfirm;
    else
      Result:= idDialogInfo;
  end;
end;

function Py_msg_box_ex(Self, Args: PPyObject): PPyObject; cdecl;
const
  cResultDelta = 100;
var
  PtrCaption, PtrText, PtrButtons: PChar;
  StrCaption, StrText, StrButtons, StrBtn: string;
  NIcon, NFocused: Longint;
  Buttons: TDialogButtons;
  Res: Longint;
  Sep: TATStringSeparator;
begin
  with AppPython.Engine do
    if Bool(PyArg_ParseTuple(Args, 'sssii:msg_box_ex', @PtrCaption, @PtrText, @PtrButtons, @NIcon, @NFocused)) then
    begin
      StrCaption:= string(PtrCaption);
      StrText:= string(PtrText);
      StrButtons:= string(PtrButtons);

      if StrButtons='' then
        exit(ReturnNone);

      Buttons:= TDialogButtons.Create(TDialogButton);

      Sep.Init(StrButtons, #1);
      repeat
        if not Sep.GetItemStr(StrBtn) then Break;
        with Buttons.Add do
        begin
          Caption:= StrBtn;
          ModalResult:= cResultDelta+Buttons.Count-1;
        end;
      until false;

      if (NFocused>=0) and (NFocused<Buttons.Count) then
        Buttons.DefaultButton:= Buttons[NFocused];

      Res:= AskUser(
        StrCaption,
        StrText,
        ConvertButtonIcon_MB_to_ID(NIcon),
        Buttons,
        0);
      FreeAndNil(Buttons);

      if Res=mrCancel then
        Result:= ReturnNone
      else
        Result:= PyInt_FromLong(Res-cResultDelta);
    end
    else
      Result:= ReturnNone;
end;


function Py_file_open(Self, Args: PPyObject): PPyObject; cdecl;
var
  PtrFile, PtrFile2, PtrArgs: PChar;
  StrFile, StrFile2, StrArgs: string;
  Num: integer;
  Pages: TATPages;
  Ok: boolean;
begin
  with AppPython.Engine do
    if Bool(PyArg_ParseTuple(Args, 'ssis:file_open', @PtrFile, @PtrFile2, @Num, @PtrArgs)) then
    begin
      StrFile:= string(PtrFile);
      StrFile2:= string(PtrFile2);
      StrArgs:= string(PtrArgs);

      Pages:= nil;
      if (Num>=Low(TATGroupsNums)) and (Num<=High(TATGroupsNums)) then
        Pages:= fmMain.Groups.Pages[Num];

      Ok:= fmMain.DoFileOpen(StrFile, StrFile2, Pages, StrArgs)<>nil;
      Result:= PyBool_FromLong(Ord(Ok));
    end
    else
      Result:= ReturnNone;
end;


function Py_ed_save(Self, Args: PPyObject): PPyObject; cdecl;
var
  H: Int64;
  Ed: TATSynEdit;
  Frame: TEditorFrame;
  PtrName: PChar;
  StrName: string;
  Ok: boolean;
begin
  with AppPython.Engine do
    if Bool(PyArg_ParseTuple(Args, 'Ls:save', @H, @PtrName)) then
    begin
      Ed:= PyEditor(H);
      Frame:= GetEditorFrame(Ed);
      if Frame=nil then exit(ReturnNone);
      StrName:= string(PtrName);

      if StrName<>'' then
      begin
        Frame.SetFileName(Ed, StrName);
        Frame.DoLexerFromFilename(Ed, StrName);
      end;
      Ok:= Frame.DoFileSave_Ex(Ed, false);

      Result:= PyBool_FromLong(Ord(Ok));
    end
  else
    Result:= ReturnNone;
end;


function Py_ed_get_text_sel(Self, Args: PPyObject): PPyObject; cdecl;
var
  H: Int64;
begin
  with AppPython.Engine do
    if Bool(PyArg_ParseTuple(Args, 'L:get_text_sel', @H)) then
      Result:= PyUnicode_FromWideString(PyEditor(H).TextSelected)
  else
    Result:= ReturnNone;
end;

function Py_ed_get_text_line(Self, Args: PPyObject): PPyObject; cdecl;
var
  H: Int64;
  Index, MaxLen: Integer;
  Ed: TATSynEdit;
  Strs: TATStrings;
begin
  with AppPython.Engine do
    if Bool(PyArg_ParseTuple(Args, 'Lii:get_text_line', @H, @Index, @MaxLen)) then
    begin
      Ed:= PyEditor(H);
      Strs:= Ed.Strings;
      if Strs.IsIndexValid(Index) then
      begin
        if (MaxLen=0) or (Strs.LinesLen[Index]<=MaxLen) then
          Result:= PyUnicode_FromWideString(Strs.Lines[Index])
        else
          Result:= PyString_FromString('');
      end
      else
        Result:= ReturnNone;
    end
  else
    Result:= ReturnNone;
end;

function Py_ed_get_text_all(Self, Args: PPyObject): PPyObject; cdecl;
var
  H: Int64;
  Ed: TATSynEdit;
begin
  with AppPython.Engine do
    if Bool(PyArg_ParseTuple(Args, 'L:get_text_all', @H)) then
    begin
      Ed:= PyEditor(H);
      Result:= PyUnicode_FromWideString(Ed.Strings.TextString_Unicode);
    end
  else
    Result:= ReturnNone;
end;


function Py_ed_get_text_substr(Self, Args: PPyObject): PPyObject; cdecl;
var
  H: Int64;
  X1, Y1, X2, Y2: Integer;
  Str: atString;
  Ed: TATSynEdit;
begin
  with AppPython.Engine do
    if Bool(PyArg_ParseTuple(Args, 'Liiii:get_text_substr', @H, @X1, @Y1, @X2, @Y2)) then
    begin
      Ed:= PyEditor(H);
      Str:= Ed.Strings.TextSubstring(X1, Y1, X2, Y2);
      Result:= PyUnicode_FromWideString(Str);
    end
  else
    Result:= ReturnNone;
end;

function Py_ed_set_caret(Self, Args: PPyObject): PPyObject; cdecl;
var
  H: Int64;
  Id, NCaret, NOptions: Integer;
  X1, Y1, X2, Y2: Integer;
  Ed: TATSynEdit;
begin
  with AppPython.Engine do
    if Bool(PyArg_ParseTuple(Args, 'Liiiiii:set_caret', @H, @X1, @Y1, @X2, @Y2, @Id, @NOptions)) then
    begin
      Ed:= PyEditor(H);

      case Id of
        CARET_DELETE_ALL:
          begin
            Ed.Carets.Clear;
            Result:= ReturnNone;
          end;

        CARET_SET_ONE:
          begin
            Ed.DoCaretSingle(X1, Y1, X2, Y2);

            if (NOptions and CARET_OPTION_NO_SCROLL)=0 then
              Ed.DoGotoPos(
                Point(X1, Y1),
                Point(X2, Y2),
                UiOps.FindIndentHorz,
                UiOps.FindIndentVert,
                false,
                (NOptions and CARET_OPTION_UNFOLD)<>0
                );

            Result:= ReturnNone;
          end;

        CARET_ADD:
          begin
            Ed.Carets.Add(X1, Y1, X2, Y2);
            Ed.Carets.Sort;
            Result:= PyInt_FromLong(Ed.Carets.Count);
          end;

        CARET_SET_INDEX..CARET_DELETE_INDEX-1:
          begin
            NCaret:= Id-CARET_SET_INDEX;
            if Ed.Carets.IsIndexValid(NCaret) then
              with Ed.Carets[NCaret] do
              begin
                PosX:= X1;
                PosY:= Y1;
                EndX:= X2;
                EndY:= Y2;
                Ed.Carets.Sort;
              end;
            Result:= ReturnNone;
          end;

        CARET_DELETE_INDEX..MaxLongint:
          begin
            NCaret:= Id-CARET_DELETE_INDEX;
            if Ed.Carets.IsIndexValid(NCaret) then
            begin
              Ed.Carets.Delete(NCaret);
              Ed.Carets.Sort;
            end;
            Result:= ReturnNone;
          end

        else
          Result:= ReturnNone;
      end;

      if Ed.Carets.Count>0 then //don't fire when no carets, plugin will add caret
        Ed.DoEventCarets;
    end
  else
    Result:= ReturnNone;
end;


function Py_ed_get_carets(Self, Args: PPyObject): PPyObject; cdecl;
var
  H: Int64;
  Ed: TATSynEdit;
  Caret: TATCaretItem;
  NLen, i: Integer;
begin
  with AppPython.Engine do
    if Bool(PyArg_ParseTuple(Args, 'L:get_carets', @H)) then
    begin
      Ed:= PyEditor(H);
      NLen:= Ed.Carets.Count;
      if NLen>0 then
      begin
        Result:= PyList_New(NLen);
        if not Assigned(Result) then
          raise EPythonError.Create(msgPythonListError);
        for i:= 0 to NLen-1 do
        begin
          Caret:= Ed.Carets[i];
          PyList_SetItem(Result, i,
            Py_BuildValue('(iiii)', Caret.PosX, Caret.PosY, Caret.EndX, Caret.EndY));
        end;
      end
      else
        Result:= ReturnNone;
    end
  else
    Result:= ReturnNone;
end;


function Py_ed_markers(Self, Args: PPyObject): PPyObject; cdecl;
var
  H: Int64;
  Id: Integer;
  Ed: TATSynEdit;
  Mark: TATMarkerItem;
  X, Y, NTag, NLenX, NLenY, i: integer;
begin
  with AppPython.Engine do
    if Bool(PyArg_ParseTuple(Args, 'Liiiiii:markers', @H, @Id, @X, @Y, @NTag, @NLenX, @NLenY)) then
    begin
      Ed:= PyEditor(H);

      case Id of
        MARKERS_GET:
          begin
            NLenX:= Ed.Markers.Count;
            if NLenX>0 then
            begin
              Result:= PyList_New(NLenX);
              if not Assigned(Result) then
                raise EPythonError.Create(msgPythonListError);
              for i:= 0 to NLenX-1 do
              begin
                Mark:= Ed.Markers[i];
                PyList_SetItem(Result, i,
                  Py_BuildValue('(iiiiL)',
                    Mark.PosX,
                    Mark.PosY,
                    Mark.LenX,
                    Mark.LenY,
                    Mark.Tag
                    ));
              end;
            end
            else
              Result:= ReturnNone;
          end;

        MARKERS_ADD:
          begin
            Ed.Markers.Add(X, Y, NTag, NLenX, NLenY);
            Ed.Update;
            Result:= PyInt_FromLong(Ed.Markers.Count);
          end;

        MARKERS_DELETE_ALL:
          begin
            if Ed.Markers.Count>0 then
            begin
              Ed.Markers.Clear;
              Ed.Update;
            end;
            Result:= ReturnNone;
          end;

        MARKERS_DELETE_LAST:
          begin
            if Ed.Markers.Count>0 then
            begin
              Ed.Markers.Delete(Ed.Markers.Count-1);
              Ed.Update;
            end;
            Result:= ReturnNone;
          end;

        MARKERS_DELETE_BY_TAG:
          begin
            if Ed.Markers.Count>0 then
            begin
              Ed.Markers.DeleteWithTag(NTag);
              Ed.Update;
            end;
            Result:= ReturnNone;
          end;

        MARKERS_DELETE_BY_INDEX:
          begin
            if Ed.Markers.IsIndexValid(NTag) then
            begin
              Ed.Markers.Delete(NTag);
              Ed.Update;
            end;
            Result:= ReturnNone;
          end;

        else
          Result:= ReturnNone;
      end;
    end
  else
    Result:= ReturnNone;
end;


function Py_ed_attr(Self, Args: PPyObject): PPyObject; cdecl;
var
  H: Int64;
  Ed: TATSynEdit;
  Mark: TATMarkerItem;
  PtrX, PtrY, PtrLen: PChar;
  StrX, StrY, StrLen: string;
  Id, X, Y, NTag, NLen, i: integer;
  NColorFont, NColorBg, NColorBorder: integer;
  NBorderL, NBorderR, NBorderD, NBorderU: integer;
  NFontBold, NFontItalic, NFontStrike, NColumn, NMapOnly: integer;
  Obj: TATLinePartClass;
  SepX, SepY, SepLen: TATStringSeparator;
  NStyles: byte;
begin
  with AppPython.Engine do
    if Bool(PyArg_ParseTuple(Args, 'Liisssiiiiiiiiiiii:attr',
      @H, @Id, @NTag, @PtrX, @PtrY, @PtrLen,
      @NColorFont, @NColorBg, @NColorBorder,
      @NFontBold, @NFontItalic, @NFontStrike,
      @NBorderL, @NBorderR, @NBorderD, @NBorderU,
      @NColumn, @NMapOnly
      )) then
    begin
      Ed:= PyEditor(H);
      StrX:= string(PtrX);
      StrY:= string(PtrY);
      StrLen:= string(PtrLen);

      case Id of
        MARKERS_GET:
          begin
            NLen:= Ed.Attribs.Count;
            if NLen>0 then
            begin
              Result:= PyList_New(NLen);
              if not Assigned(Result) then
                raise EPythonError.Create(msgPythonListError);
              for i:= 0 to NLen-1 do
              begin
                Mark:= Ed.Attribs[i];
                Obj:= TATLinePartClass(Mark.Ptr);
                PyList_SetItem(Result, i,
                  Py_BuildValue('(Liiiiiiiiiiiiiii)',
                    Mark.Tag,
                    Mark.PosX,
                    Mark.PosY,
                    Mark.LenX,
                    Obj.Data.ColorFont,
                    Obj.Data.ColorBG,
                    Obj.Data.ColorBorder,
                    Ord((Obj.Data.FontStyles and afsFontBold)<>0),
                    Ord((Obj.Data.FontStyles and afsFontItalic)<>0),
                    Ord((Obj.Data.FontStyles and afsFontCrossed)<>0),
                    Ord(Obj.Data.BorderLeft),
                    Ord(Obj.Data.BorderRight),
                    Ord(Obj.Data.BorderDown),
                    Ord(Obj.Data.BorderUp),
                    Ord(Obj.ColumnTag),
                    Ord(Mark.MicromapOnly)
                    ));
              end;
            end
            else
              Result:= ReturnNone;
          end;

        MARKERS_GET_DICT:
          begin
            NLen:= Ed.Attribs.Count;
            Result:= PyList_New(NLen);
            if not Assigned(Result) then
              raise EPythonError.Create(msgPythonListError);
            for i:= 0 to NLen-1 do
            begin
              Mark:= Ed.Attribs[i];
              Obj:= TATLinePartClass(Mark.Ptr);
              PyList_SetItem(Result, i,
                Py_BuildValue('{sLsisisisisisisOsOsOsisisisisisO}',
                  'tag',
                  Mark.Tag,
                  PChar(string('x')),
                  Mark.PosX,
                  PChar(string('y')),
                  Mark.PosY,
                  'len',
                  Mark.LenX,
                  'color_font',
                  Obj.Data.ColorFont,
                  'color_bg',
                  Obj.Data.ColorBG,
                  'color_border',
                  Obj.Data.ColorBorder,
                  'font_bold',
                  PyBool_FromLong(Ord((Obj.Data.FontStyles and afsFontBold)<>0)),
                  'font_italic',
                  PyBool_FromLong(Ord((Obj.Data.FontStyles and afsFontItalic)<>0)),
                  'font_strikeout',
                  PyBool_FromLong(Ord((Obj.Data.FontStyles and afsFontCrossed)<>0)),
                  'border_left',
                  Ord(Obj.Data.BorderLeft),
                  'border_right',
                  Ord(Obj.Data.BorderRight),
                  'border_down',
                  Ord(Obj.Data.BorderDown),
                  'border_up',
                  Ord(Obj.Data.BorderUp),
                  'show_on_map',
                  Ord(Obj.ColumnTag),
                  'map_only',
                  PyBool_FromLong(Ord(Mark.MicromapOnly))
                  ));
            end;
          end;

        MARKERS_ADD:
          begin
            Obj:= TATLinePartClass.Create;
            Obj.Data.ColorFont:= NColorFont;
            Obj.Data.ColorBG:= NColorBg;
            if NColorBorder<>clNone then
              Obj.Data.ColorBorder:= NColorBorder
            else
              Obj.Data.ColorBorder:= Ed.Colors.TextFont;

            NStyles:= 0;
            if Bool(NFontBold) then
              NStyles:= NStyles or afsFontBold;
            if Bool(NFontItalic) then
              NStyles:= NStyles or afsFontItalic;
            if Bool(NFontStrike) then
              NStyles:= NStyles or afsFontCrossed;
            Obj.Data.FontStyles:= NStyles;

            Obj.Data.BorderLeft:= TATLineStyle(NBorderL);
            Obj.Data.BorderRight:= TATLineStyle(NBorderR);
            Obj.Data.BorderDown:= TATLineStyle(NBorderD);
            Obj.Data.BorderUp:= TATLineStyle(NBorderU);
            Obj.ColumnTag:= NColumn;

            X:= StrToIntDef(StrX, 0);
            Y:= StrToIntDef(StrY, 0);
            NLen:= StrToIntDef(StrLen, 1);
            Ed.Attribs.Add(X, Y, NTag, NLen, 0, Obj, 0, Bool(NMapOnly));
            Ed.Update;
            Result:= PyInt_FromLong(Ed.Attribs.Count);
          end;

        MARKERS_ADD_MANY:
          begin
            SepX.Init(StrX);
            SepY.Init(StrY);
            SepLen.Init(StrLen);

            repeat
              if not SepX.GetItemInt(X, -1) then Break;
              if not SepY.GetItemInt(Y, -1) then Break;
              if not SepLen.GetItemInt(NLen, 0) then Break;
              if X<0 then Break;
              if Y<0 then Break;
              if NLen=0 then Break;

              Obj:= TATLinePartClass.Create;
              Obj.Data.ColorFont:= NColorFont;
              Obj.Data.ColorBG:= NColorBg;
              if NColorBorder<>clNone then
                Obj.Data.ColorBorder:= NColorBorder
              else
                Obj.Data.ColorBorder:= Ed.Colors.TextFont;

              NStyles:= 0;
              if Bool(NFontBold) then
                NStyles:= NStyles or afsFontBold;
              if Bool(NFontItalic) then
                NStyles:= NStyles or afsFontItalic;
              if Bool(NFontStrike) then
                NStyles:= NStyles or afsFontCrossed;
              Obj.Data.FontStyles:= NStyles;

              Obj.Data.BorderLeft:= TATLineStyle(NBorderL);
              Obj.Data.BorderRight:= TATLineStyle(NBorderR);
              Obj.Data.BorderDown:= TATLineStyle(NBorderD);
              Obj.Data.BorderUp:= TATLineStyle(NBorderU);
              Obj.ColumnTag:= NColumn;

              Ed.Attribs.Add(X, Y, NTag, NLen, 0, Obj, 0, Bool(NMapOnly));
            until false;

            Ed.Update;
            Result:= PyInt_FromLong(Ed.Attribs.Count);
          end;

        MARKERS_DELETE_ALL:
          begin
            //Ed.InvalidateHilitingCache;
            if Ed.Attribs.Count>0 then
            begin
              Ed.Attribs.Clear;
              Ed.Update;
            end;
            Result:= ReturnNone;
          end;

        MARKERS_DELETE_LAST:
          begin
            //Ed.InvalidateHilitingCache;
            if Ed.Attribs.Count>0 then
            begin
              Ed.Attribs.Delete(Ed.Attribs.Count-1);
              Ed.Update;
            end;
            Result:= ReturnNone;
          end;

        MARKERS_DELETE_BY_TAG:
          begin
            //Ed.InvalidateHilitingCache;
            if Ed.Attribs.Count>0 then
            begin
              Ed.Attribs.DeleteWithTag(NTag);
              Ed.Update;
            end;
            Result:= ReturnNone;
          end;

        MARKERS_DELETE_BY_INDEX:
          begin
            if Ed.Attribs.IsIndexValid(NTag) then
            begin
              Ed.Attribs.Delete(NTag);
              Ed.Update;
            end;
            Result:= ReturnNone;
          end;

        else
          Result:= ReturnNone;
      end;
    end
  else
    Result:= ReturnNone;
end;


function Py_ed_hotspots(Self, Args: PPyObject): PPyObject; cdecl;
var
  H: Int64;
  Ed: TATSynEdit;
  Id: integer;
  NTag: Int64;
  PtrTag, PtrPos: PChar;
  StrTag, StrPos: string;
  NLen, i: integer;
  FItem: TATHotspotItem;
  Sep: TATStringSeparator;
begin
  with AppPython.Engine do
    if Bool(PyArg_ParseTuple(Args, 'LiLss:hotspots', @H, @Id, @NTag, @PtrTag, @PtrPos)) then
    begin
      Ed:= PyEditor(H);
      StrTag:= string(PtrTag);
      StrPos:= string(PtrPos);

      case Id of
        HOTSPOT_DELETE_ALL:
          begin
            Ed.Hotspots.Clear;
            Result:= ReturnNone;
          end;

        HOTSPOT_DELETE_LAST:
          begin
            if Ed.Hotspots.Count>0 then
              Ed.Hotspots.Delete(Ed.Hotspots.Count-1);
            Result:= ReturnNone;
          end;

        HOTSPOT_DELETE_BY_TAG:
          begin
            Ed.Hotspots.DeleteByTag(NTag);
            Result:= ReturnNone;
          end;

        HOTSPOT_ADD:
          begin
            Sep.Init(StrPos);
            Sep.GetItemInt(FItem.PosX, -1);
            Sep.GetItemInt(FItem.PosY, -1);
            Sep.GetItemInt(FItem.EndX, -1);
            Sep.GetItemInt(FItem.EndY, -1);
            if (FItem.PosX<0) or
               (FItem.PosY<0) or
               (FItem.EndX<0) or
               (FItem.EndY<0) then exit(PyBool_FromLong(0));
            FItem.Tag:= NTag;
            FItem.TagString:= StrTag;
            Ed.Hotspots.Add(FItem);
            Result:= PyBool_FromLong(1);
          end;

        HOTSPOT_GET_LIST:
          begin
            NLen:= Ed.Hotspots.Count;
            if NLen>0 then
            begin
              Result:= PyList_New(NLen);
              if not Assigned(Result) then
                raise EPythonError.Create(msgPythonListError);
              for i:= 0 to NLen-1 do
              begin
                FItem:= Ed.Hotspots[i];
                PyList_SetItem(Result, i,
                  Py_BuildValue('{s(iiii)sLss}',
                    'pos',
                    FItem.PosX,
                    FItem.PosY,
                    FItem.EndX,
                    FItem.EndY,
                    'tag',
                    FItem.Tag,
                    'tag_str',
                    PChar(FItem.TagString)
                    ));
              end;
            end
            else
              Result:= ReturnNone;
          end;

        else
          Result:= ReturnNone;
      end;
    end
  else
    Result:= ReturnNone;
end;


function Py_ed_get_sel_mode(Self, Args: PPyObject): PPyObject; cdecl;
var
  H: Int64;
  N: Integer;
  Ed: TATSynEdit;
begin
  with AppPython.Engine do
    if Bool(PyArg_ParseTuple(Args, 'L:get_sel_mode', @H)) then
    begin
      Ed:= PyEditor(H);
      if Ed.IsSelRectEmpty then
        N:= SEL_NORMAL
      else
        N:= SEL_COLUMN;
      Result:= PyInt_FromLong(N);
    end
  else
    Result:= ReturnNone;
end;

function Py_ed_get_sel_rect(Self, Args: PPyObject): PPyObject; cdecl;
var
  H: Int64;
  R: TRect;
begin
  with AppPython.Engine do
    if Bool(PyArg_ParseTuple(Args, 'L:get_sel_rect', @H)) then
    begin
      R:= PyEditor(H).SelRect;
      Result:= Py_BuildValue('(iiii)', R.Left, R.Top, R.Right, R.Bottom);
    end
  else
    Result:= ReturnNone;
end;

function Py_ed_get_sel_lines(Self, Args: PPyObject): PPyObject; cdecl;
var
  H: Int64;
  N1, N2: Integer;
  Ed: TATSynEdit;
  Caret: TATCaretItem;
begin
  with AppPython.Engine do
    if Bool(PyArg_ParseTuple(Args, 'L:get_sel_lines', @H)) then
    begin
      Ed:= PyEditor(H);
      Caret:= Ed.Carets[0];
      Caret.GetSelLines(N1, N2, false);
      Result:= Py_BuildValue('(ii)', N1, N2);
    end
  else
    Result:= ReturnNone;
end;

function Py_ed_set_sel_rect(Self, Args: PPyObject): PPyObject; cdecl;
var
  H: Int64;
  X1, Y1, X2, Y2: Integer;
  Ed: TATSynEdit;
begin
  with AppPython.Engine do
    if Bool(PyArg_ParseTuple(Args, 'Liiii:set_sel_rect', @H, @X1, @Y1, @X2, @Y2)) then
    begin
      Ed:= PyEditor(H);

      Ed.DoSelect_None;
      Ed.DoSelect_ColumnBlock_FromPoints(Point(X1, Y1), Point(X2, Y2));
      Ed.DoEventCarets;
      Ed.Update;

      Result:= ReturnNone;
    end
  else
    Result:= ReturnNone;
end;


function Py_ed_set_text_all(Self, Args: PPyObject): PPyObject; cdecl;
var
  H: Int64;
  Ed: TATSynEdit;
  PtrText: PChar;
  StrW: atString;
begin
  with AppPython.Engine do
    if Bool(PyArg_ParseTuple(Args, 'Ls:set_text_all', @H, @PtrText)) then
    begin
      Ed:= PyEditor(H);
      if Ed.ModeReadOnly then
        exit(ReturnNone);

      StrW:= UTF8Decode(string(PtrText));

      //don't keep Undo, not needed in set_text_all
      Ed.DoCaretSingle(0, 0);
      Ed.Strings.LoadFromString(StrW);

      Ed.DoCaretsFixIncorrectPos(false);
      Ed.DoEventChange;
      Ed.Strings.DoSaveLastEditPos(0, 0);
      Ed.Update(true);

      Result:= ReturnNone;
    end;
end;

function Py_ed_set_text_line(Self, Args: PPyObject): PPyObject; cdecl;
var
  H: Int64;
  N, i: Integer;
  Ptr: PChar;
  Ed: TATSynEdit;
  Str: atString;
  Strs: TATStrings;
begin
  with AppPython.Engine do
    if Bool(PyArg_ParseTuple(Args, 'Lis:set_text_line', @H, @N, @Ptr)) then
    begin
      Ed:= PyEditor(H);
      Str:= UTF8Decode(string(Ptr));
      Strs:= Ed.Strings;
      if Ed.ModeReadOnly then
        exit(ReturnNone);

      //replace \n \r to "_"
      for i:= 1 to Length(Str) do
        if (Str[i]=#10) or (Str[i]=#13) then Str[i]:= '_';

      if N=-1 then
        Strs.LineAdd(Str)
      else
      if Strs.IsIndexValid(N) then
      begin
        Strs.Lines[N]:= Str;
        if Strs.LinesEnds[N]=cEndNone then
          Strs.LinesEnds[N]:= Strs.Endings;
      end;

      Ed.DoEventChange;
      Strs.DoSaveLastEditPos(0, N);
      Ed.Update(true);

      Result:= ReturnNone;
    end
  else
    Result:= ReturnNone;
end;


function Py_ed_delete(Self, Args: PPyObject): PPyObject; cdecl;
var
  H: Int64;
  X1, Y1, X2, Y2: Integer;
  Ed: TATSynEdit;
  Shift, PosAfter: TPoint;
begin
  with AppPython.Engine do
    if Bool(PyArg_ParseTuple(Args, 'Liiii:delete', @H, @X1, @Y1, @X2, @Y2)) then
    begin
      Ed:= PyEditor(H);
      if Ed.ModeReadOnly then
        exit(ReturnNone);

      Ed.Strings.TextDeleteRange(X1, Y1, X2, Y2, Shift, PosAfter);
      Ed.DoEventChange;
      Ed.Update(true);

      Result:= ReturnNone;
    end
  else
    Result:= ReturnNone;
end;

function Py_ed_insert(Self, Args: PPyObject): PPyObject; cdecl;
var
  H: Int64;
  X1, Y1: Integer;
  Ptr: PChar;
  Str: atString;
  Ed: TATSynEdit;
  Strs: TATStrings;
  Shift, PosAfter: TPoint;
begin
  with AppPython.Engine do
    if Bool(PyArg_ParseTuple(Args, 'Liis:insert', @H, @X1, @Y1, @Ptr)) then
    begin
      Ed:= PyEditor(H);
      Str:= UTF8Decode(string(Ptr));
      Strs:= Ed.Strings;
      if Ed.ModeReadOnly then
        Exit(ReturnNone);

      if Y1<0 then
        Exit(ReturnNone);

      //too big index- do append
      if Y1>=Strs.Count then
        Strs.TextAppend(Str, Shift, PosAfter)
      else
        Strs.TextInsert(X1, Y1, Str, false, Shift, PosAfter);

      Ed.DoEventChange;
      Ed.Update(true);

      Result:= Py_BuildValue('(ii)', PosAfter.X, PosAfter.Y);
    end
  else
    Result:= ReturnNone;
end;


function Py_ed_replace(Self, Args: PPyObject): PPyObject; cdecl;
var
  H: Int64;
  X1, Y1, X2, Y2: Integer;
  Ed: TATSynEdit;
  Shift, PosAfter: TPoint;
  PtrText: PChar;
  StrText: string;
begin
  with AppPython.Engine do
    if Bool(PyArg_ParseTuple(Args, 'Liiiis:replace', @H, @X1, @Y1, @X2, @Y2, @PtrText)) then
    begin
      Ed:= PyEditor(H);
      StrText:= string(PtrText);
      if Ed.ModeReadOnly then
        exit(ReturnNone);

      Ed.Strings.TextReplaceRange(X1, Y1, X2, Y2,
        UTF8Decode(StrText), Shift, PosAfter, true);

      Ed.DoEventChange;
      Ed.Update(true);

      Result:= Py_BuildValue('(ii)', PosAfter.X, PosAfter.Y);
    end
  else
    Result:= ReturnNone;
end;


function Py_ed_replace_lines(Self, Args: PPyObject): PPyObject; cdecl;
var
  H: Int64;
  Y1, Y2: Integer;
  Ed: TATSynEdit;
  Lines: TStringList;
  ListPy: PPyObject;
  Res: boolean;
begin
  with AppPython.Engine do
    if Bool(PyArg_ParseTuple(Args, 'LiiO:replace_lines', @H, @Y1, @Y2, @ListPy)) then
    begin
      Ed:= PyEditor(H);
      if Ed.ModeReadOnly then
        exit(ReturnNone);

      Lines:= TStringList.Create;
      try
        PyListToStrings(ListPy, Lines);
        Res:= Ed.Strings.TextReplaceLines_UTF8(Y1, Y2, Lines);

        Ed.DoEventChange;
      finally
        FreeAndNil(Lines);
      end;

      Ed.Update(true);

      Result:= PyBool_FromLong(Ord(Res));
    end;
end;


function Py_ed_get_line_count(Self, Args: PPyObject): PPyObject; cdecl;
var
  H: Int64;
begin
  with AppPython.Engine do
    if Bool(PyArg_ParseTuple(Args, 'L:get_line_count', @H)) then
    begin
      Result:= PyInt_FromLong(PyEditor(H).Strings.Count);
    end
  else
    Result:= ReturnNone;
end;


function Py_ed_get_filename(Self, Args: PPyObject): PPyObject; cdecl;
var
  H: Int64;
  Ed: TATSynEdit;
  PtrOps: PChar;
  StrOps: string;
  F: TEditorFrame;
  Str: string;
begin
  with AppPython.Engine do
    if Bool(PyArg_ParseTuple(Args, 'Ls:get_filename', @H, @PtrOps)) then
    begin
      Ed:= PyEditor(H);
      F:= GetEditorFrame(Ed);
      StrOps:= string(PtrOps);

      if F=nil then
        exit(ReturnNone);
      if F.IsText or (Pos('*', StrOps)>0) then
        Str:= F.GetFileName(Ed)
      else
        Str:= '?';

      Result:= PyString_FromString(PChar(Str));
    end
  else
    Result:= ReturnNone;
end;


function Py_ed_cmd(Self, Args: PPyObject): PPyObject; cdecl;
var
  H: Int64;
  Ed: TATSynEdit;
  NCode: integer;
  Ptr: PChar;
  Str: string;
begin
  with AppPython.Engine do
    if Bool(PyArg_ParseTuple(Args, 'Lis:cmd', @H, @NCode, @Ptr)) then
    begin
      Ed:= PyEditor(H);
      Str:= string(Ptr);

      Ed.DoCommand(NCode, Str);

      //many commands use timer, so immediate quit is not ok, app didn't finish command yet.
      //example of bug: plugin Differ uses ed.cmd(cmd_FileClose) and immediately after that it calls
      //file_open for file-pair with the same file, it failed
      if IsCommandNeedTimer(NCode) then
      begin
        Sleep(fmMain.TimerCmd.Interval+100);
        Application.ProcessMessages;
      end;

      Result:= ReturnNone;
    end
  else
    Result:= ReturnNone;
end;

function Py_ed_lock(Self, Args: PPyObject): PPyObject; cdecl;
var
  H: Int64;
  Ed: TATSynEdit;
begin
  with AppPython.Engine do
    if Bool(PyArg_ParseTuple(Args, 'L:lock', @H)) then
    begin
      Ed:= PyEditor(H);
      Ed.BeginUpdate;
      Result:= ReturnNone;
    end
  else
    Result:= ReturnNone;
end;

function Py_ed_unlock(Self, Args: PPyObject): PPyObject; cdecl;
var
  H: Int64;
  Ed: TATSynEdit;
begin
  with AppPython.Engine do
    if Bool(PyArg_ParseTuple(Args, 'L:unlock', @H)) then
    begin
      Ed:= PyEditor(H);
      Ed.EndUpdate;
      Result:= ReturnNone;
    end
  else
    Result:= ReturnNone;
end;

function Py_ed_focus(Self, Args: PPyObject): PPyObject; cdecl;
var
  H: Int64;
  Ed: TATSynEdit;
  F: TEditorFrame;
begin
  with AppPython.Engine do
    if Bool(PyArg_ParseTuple(Args, 'L:focus', @H)) then
    begin
      Ed:= PyEditor(H);
      F:= GetEditorFrame(Ed);
      if Assigned(F) then
      begin
        fmMain.SetFrame(F);
        F.SetFocus;
        fmMain.UpdateFrameEx(F, false);
        fmMain.UpdateStatus;
      end;

      Result:= ReturnNone;
    end
  else
    Result:= ReturnNone;
end;


function Py_ed_bookmark_getitem(BmItem: TATBookmarkItem): PPyObject;
begin
  with AppPython.Engine do
    Result:= Py_BuildValue('{sisLsisssOsO}',
      'line',
      BmItem.Data.LineNum,
      'tag',
      BmItem.Data.Tag,
      'kind',
      Integer(BmItem.Data.Kind),
      'hint',
      PChar(string(BmItem.Data.Hint)),
      'auto_del',
      PyBool_FromLong(Ord(BmItem.Data.DeleteOnDelLine)),
      'show_in_list',
      PyBool_FromLong(Ord(BmItem.Data.ShowInBookmarkList))
      );
end;

function Py_ed_bookmark_getall(ABookmarks: TATBookmarks): PPyObject;
var
  NCount, i: integer;
  BmItem: TATBookmarkItem;
begin
  with AppPython.Engine do
  begin
    NCount:= ABookmarks.Count;
    if NCount>0 then
    begin
      Result:= PyList_New(NCount);
      if not Assigned(Result) then
        raise EPythonError.Create(msgPythonListError);
      for i:= 0 to NCount-1 do
      begin
        BmItem:= ABookmarks[i];
        PyList_SetItem(Result, i, Py_ed_bookmark_getitem(BmItem));
      end;
    end
    else
      Result:= ReturnNone;
  end;
end;

function Py_ed_bookmark(Self, Args: PPyObject): PPyObject; cdecl;
var
  H: Int64;
  PtrText: PChar;
  StrText: string;
  Ed: TATSynEdit;
  Strs: TATStrings;
  NId, NLine, NKind, NColor, NCount,
  NDeleteIfDelLine, NShowInList: integer;
  NTag: Int64;
  BmItem: TATBookmarkItem;
  i: integer;
begin
  with AppPython.Engine do
    if Bool(PyArg_ParseTuple(Args, 'LiiiisiiL:bookmark',
            @H, @NId, @NLine, @NKind, @NColor, @PtrText,
            @NDeleteIfDelLine, @NShowInList, @NTag)) then
    begin
      Ed:= PyEditor(H);
      Strs:= Ed.Strings;
      StrText:= string(PtrText);

      //check range of NKind
      if NKind<Low(AppBookmarkSetup) then
        NKind:= Low(AppBookmarkSetup);
      if NKind>High(AppBookmarkSetup) then
        NKind:= High(AppBookmarkSetup);

      case NId of
        BOOKMARK_GET_PROP:
          begin
            i:= Strs.Bookmarks.Find(NLine);
            if i>=0 then
            begin
              BmItem:= Strs.Bookmarks[i];
              Result:= Py_ed_bookmark_getitem(BmItem);
            end
            else
              Result:= ReturnNone;
          end;
        BOOKMARK2_GET_PROP:
          begin
            i:= Strs.Bookmarks2.Find(NLine);
            if i>=0 then
            begin
              BmItem:= Strs.Bookmarks2[i];
              Result:= Py_ed_bookmark_getitem(BmItem);
            end
            else
              Result:= ReturnNone;
          end;

        BOOKMARK_SET:
          begin
            ed.BookmarkSetForLine(NLine, NKind, StrText,
                Bool(NDeleteIfDelLine),
                Bool(NShowInList),
                NTag
                );
            Result:= ReturnNone;
          end;
        BOOKMARK2_SET:
          begin
            ed.BookmarkSetForLine_2(NLine, NKind, StrText,
                Bool(NDeleteIfDelLine),
                Bool(NShowInList),
                NTag
                );
            Result:= ReturnNone;
          end;

        BOOKMARK_CLEAR:
          begin
            ed.BookmarkDeleteForLine(NLine);
            Result:= ReturnNone;
          end;
        BOOKMARK2_CLEAR:
          begin
            ed.BookmarkDeleteForLine_2(NLine);
            Result:= ReturnNone;
          end;

        BOOKMARK_CLEAR_ALL:
          begin
            ed.BookmarkDeleteAll;
            Result:= ReturnNone;
          end;
        BOOKMARK2_CLEAR_ALL:
          begin
            ed.BookmarkDeleteAll_2;
            Result:= ReturnNone;
          end;

        BOOKMARK_SETUP:
          begin
            AppBookmarkSetup[NKind].ImageIndex:=
              UpdateImagelistWithIconFromFile(AppBookmarkImagelist, StrText);
            AppBookmarkSetup[NKind].Color:= NColor;
            Result:= ReturnNone;
          end;

        BOOKMARK_GET_LIST:
          begin
            NCount:= Strs.Bookmarks.Count;
            if NCount>0 then
            begin
              Result:= PyList_New(NCount);
              if not Assigned(Result) then
                raise EPythonError.Create(msgPythonListError);
              for i:= 0 to NCount-1 do
                PyList_SetItem(Result, i,
                  PyInt_FromLong(Strs.Bookmarks[i].Data.LineNum)
                  );
            end
            else
              Result:= ReturnNone;
          end;

        BOOKMARK_GET_ALL:
          begin
            Result:= Py_ed_bookmark_getall(Strs.Bookmarks);
          end;
        BOOKMARK2_GET_ALL:
          begin
            Result:= Py_ed_bookmark_getall(Strs.Bookmarks2);
          end;

        BOOKMARK_DELETE_BY_TAG:
          begin
            Result:= PyBool_FromLong(Ord(ed.BookmarkDeleteByTag(NTag)));
          end;
        BOOKMARK2_DELETE_BY_TAG:
          begin
            Result:= PyBool_FromLong(Ord(ed.BookmarkDeleteByTag_2(NTag)));
          end;

        else
          Result:= ReturnNone;
      end;

      Ed.Update;
    end
  else
    Result:= ReturnNone;
end;


function Py_ed_decor_getitem(Item: TATGutterDecorItem): PPyObject;
begin
  with AppPython.Engine do
    Result:= Py_BuildValue('{sisLsssOsOsisisO}',
      'line',
      Item.Data.LineNum,
      'tag',
      Item.Data.Tag,
      'text',
      PChar(string(Item.Data.Text)),
      'bold',
      PyBool_FromLong(Ord(Item.Data.TextBold)),
      'italic',
      PyBool_FromLong(Ord(Item.Data.TextItalic)),
      'color',
      Item.Data.TextColor,
      'image',
      Item.Data.ImageIndex,
      'auto_del',
      PyBool_FromLong(Ord(Item.Data.DeleteOnDelLine))
      );
end;

function Py_ed_decor(Self, Args: PPyObject): PPyObject; cdecl;
var
  H: Int64;
  PtrText: PChar;
  Ed: TATSynEdit;
  NId, NLine, NImageIndex, NColor: integer;
  NFontBold, NFontItalic, NAutoDelete: integer;
  NTag: Int64;
  Data: TATGutterDecorData;
  NCount, i: integer;
begin
  with AppPython.Engine do
    if Bool(PyArg_ParseTuple(Args, 'LiiLsiiiii:decor',
            @H, @NId, @NLine, @NTag, @PtrText, @NColor, @NFontBold, @NFontItalic,
            @NImageIndex, @NAutoDelete)) then
    begin
      Ed:= PyEditor(H);

      case NId of
        DECOR_GET_ALL:
          begin
            NCount:= Ed.GutterDecor.Count;
            if NCount>0 then
            begin
              Result:= PyList_New(NCount);
              if not Assigned(Result) then
                raise EPythonError.Create(msgPythonListError);
              for i:= 0 to NCount-1 do
                PyList_SetItem(Result, i,
                  Py_ed_decor_getitem(Ed.GutterDecor[i])
                  );
            end
            else
              Result:= ReturnNone;
          end;

        DECOR_GET:
          begin
            i:= Ed.GutterDecor.Find(NLine);
            if i>=0 then
              Result:= Py_ed_decor_getitem(Ed.GutterDecor[i])
            else
              Result:= ReturnNone;
          end;

        DECOR_SET:
          begin
            if not Ed.Strings.IsIndexValid(NLine) then
              exit(PyBool_FromLong(0));

            FillChar(Data, SizeOf(Data), 0);
            Data.LineNum:= NLine;
            Data.Tag:= NTag;
            Data.DeleteOnDelLine:= Bool(NAutoDelete);
            Data.ImageIndex:= NImageIndex;
            Data.Text:= string(PtrText);
            Data.TextBold:= Bool(NFontBold);
            Data.TextItalic:= Bool(NFontItalic);
            data.TextColor:= NColor;

            Ed.GutterDecor.Add(Data);
            Ed.Update;
            Result:= PyBool_FromLong(1);
          end;

        DECOR_DELETE_ALL:
          begin
            if Ed.GutterDecor.Count>0 then
            begin
              Ed.GutterDecor.Clear;
              Ed.Update;
            end;
            Result:= ReturnNone;
          end;

        DECOR_DELETE_BY_LINE:
          begin
            i:= Ed.GutterDecor.Find(NLine);
            if i>=0 then
            begin
              Ed.GutterDecor.Delete(i);
              Ed.Update;
            end;
            Result:= ReturnNone;
          end;

        DECOR_DELETE_BY_TAG:
          begin
            if Ed.GutterDecor.DeleteByTag(NTag) then
              Ed.Update;
            Result:= ReturnNone;
          end;

        DECOR_GET_IMAGELIST:
          begin
            if Ed.ImagesGutterDecor=nil then
              Ed.ImagesGutterDecor:= TImageList.Create(Ed);
            Result:= Py_ObjectHandle(Ed.ImagesGutterDecor);
          end;

        else
          Result:= ReturnNone;
      end;
    end
  else
    Result:= ReturnNone;
end;


function Py_ed_handles(Self, Args : PPyObject): PPyObject; cdecl;
var
  nMin, nMax: integer;
begin
  nMin:= cPyEditorHandleMin;
  nMax:= Min(cPyEditorHandleMin + fmMain.FrameCount - 1, cPyEditorHandleMax);

  with AppPython.Engine do
    Result:= Py_BuildValue('(ii)', nMin, nMax);
end;

function Py_ed_group(Self, Args: PPyObject): PPyObject; cdecl;
var
  Index: Longint;
  Ed: TATSynEdit;
begin
  with AppPython.Engine do
    if Bool(PyArg_ParseTuple(Args, 'i:ed_group', @Index)) then
    begin
      Ed:= GetEditorActiveInGroup(Index);
      if Ed=nil then
        Result:= ReturnNone
      else
        Result:= Py_ObjectHandle(Ed);
    end
  else
    Result:= ReturnNone;
end;


function Py_dlg_file(Self, Args: PPyObject): PPyObject; cdecl;
var
  PtrFilename, PtrFolder, PtrFilter, PtrCaption: PChar;
  StrFilename, StrFolder, StrFilter, StrCaption: string;
  IsOpen: LongBool;
  IsAllowAny: boolean;
  Dlg: TOpenDialog;
begin
  with AppPython.Engine do
    if Bool(PyArg_ParseTuple(Args, 'issss:dlg_file',
      @IsOpen, @PtrFilename, @PtrFolder, @PtrFilter, @PtrCaption)) then
    begin
      StrFilename:= string(PtrFilename);
      StrFolder:= string(PtrFolder);
      StrFilter:= string(PtrFilter);
      StrCaption:= string(PtrCaption);

      IsAllowAny:= SBeginsWith(StrFilename, '!');
      if IsAllowAny then Delete(StrFilename, 1, 1);

      if IsOpen then
      begin
        Dlg:= TOpenDialog.Create(nil);
        Dlg.Title:= msgDialogTitleOpen;
        Dlg.Options:= Dlg.Options+[ofHideReadOnly, ofPathMustExist];
        if not IsAllowAny then
          Dlg.Options:= Dlg.Options+[ofFileMustExist];
      end
      else
      begin
        Dlg:= TSaveDialog.Create(nil);
        Dlg.Title:= msgDialogTitleSaveAs;
        Dlg.Options:= Dlg.Options+[ofHideReadOnly, ofPathMustExist, ofOverwritePrompt];
      end;

      try
        if StrFilename='*' then
        begin
          StrFilename:= '';
          Dlg.Options:= Dlg.Options+[ofAllowMultiSelect];
        end;

        Dlg.Title:= StrCaption;
        Dlg.FileName:= StrFilename;
        Dlg.InitialDir:= StrFolder;
        Dlg.Filter:= StrFilter;

        if Dlg.Execute then
        begin
          if (ofAllowMultiSelect in Dlg.Options) and (Dlg.Files.Count>1) then
            Result:= StringsToPyList(Dlg.Files)
          else
            Result:= PyString_FromString(PChar(Dlg.FileName));
        end
        else
          Result:= ReturnNone;
      finally
        FreeAndNil(Dlg);
      end;
    end;
end;


function Py_dlg_dir(Self, Args: PPyObject): PPyObject; cdecl;
var
  PtrFolder, PtrCaption: PChar;
  StrFolder, StrCaption: string;
  Dlg: TSelectDirectoryDialog;
begin
  with AppPython.Engine do
    if Bool(PyArg_ParseTuple(Args, 'ss:dlg_dir', @PtrFolder, @PtrCaption)) then
    begin
      StrFolder:= string(PtrFolder);
      StrCaption:= string(PtrCaption);
      Dlg:= TSelectDirectoryDialog.Create(nil);
      try
        if StrCaption<>'' then
          Dlg.Title:= StrCaption
        else
          Dlg.Title:= msgDialogTitleSelFolder;
        Dlg.InitialDir:= StrFolder;
        if Dlg.Execute then
          Result:= PyString_FromString(PChar(Dlg.FileName))
        else
          Result:= ReturnNone;
      finally
        FreeAndNil(Dlg);
      end;
    end;
end;

procedure TfmMain.PyCompletionOnGetProp(Sender: TObject; out AText: string; out
  ACharsLeft, ACharsRight: integer);
var
  Ed: TATSynEdit;
  Caret: TATCaretItem;
  PrevCharsLeft: integer;
  Params: TAppVariantArray;
begin
  AText:= '';
  ACharsLeft:= 0;
  ACharsRight:= 0;

  Ed:= FPyComplete_Editor; //can be not CurrentEditor
  Caret:= Ed.Carets[0];

  //if caret moved by 1 char: repeat auto-complete
  if (Abs(Caret.PosX-FPyComplete_CaretPos.X)=1) and
     (Caret.PosY=FPyComplete_CaretPos.Y) then
  begin
    PrevCharsLeft:= FPyComplete_CharsLeft;
    SetLength(Params, 0);
    DoPyEvent(Ed, cEventOnComplete, Params);

    //this check- to close listbox when no more matches after typing too much
    //MsgStatus(Format('prev %d now %d', [PrevCharsLeft, FPyComplete_CharsLeft]));
    if Abs(PrevCharsLeft-FPyComplete_CharsLeft)<>1 then exit;

    AText:= FPyComplete_Text;
    ACharsLeft:= FPyComplete_CharsLeft;
    ACharsRight:= FPyComplete_CharsRight;
    exit
  end;

  //stop doing, if caret moved
  if (Caret.PosX<>FPyComplete_CaretPos.X) or
     (Caret.PosY<>FPyComplete_CaretPos.Y) then
    exit;

  AText:= FPyComplete_Text;
  ACharsLeft:= FPyComplete_CharsLeft;
  ACharsRight:= FPyComplete_CharsRight;
end;

procedure TfmMain.PyCompletionOnResult(Sender: TObject;
  const ASnippetId: string; ASnippetIndex: integer);
var
  Sep: TATStringSeparator;
  StrLine, StrSnippet: string;
  Params: TAppVariantArray;
  i: integer;
begin
  //find index-th line
  Sep.Init(FPyComplete_Text, #10);
  for i:= 0 to ASnippetIndex do
    Sep.GetItemStr(StrLine);

  //in line: snippet is 3rd column, skip 2 columns
  Sep.Init(StrLine, #9);
  for i:= 1 to 3 do
    Sep.GetItemStr(StrSnippet);

  //ShowMessage('id: '+ASnippetId+#10+'text:'#10+StrSnippet);

  SetLength(Params, 2);
  Params[0]:= AppVariant(ASnippetId);
  Params[1]:= AppVariant(StrSnippet);

  DoPyEvent(FPyComplete_Editor, cEventOnSnippet, Params);
end;


procedure DoApplyCompletionListboxStyle(Alt: boolean);
begin
  if Alt then
  begin
    //id|prefix|desc
    CompletionOps.IndexOfText:= 0;
  end
  else
  begin
    //prefix|id|desc
    CompletionOps.IndexOfText:= 1;
  end;
end;


function Py_ed_complete(Self, Args: PPyObject): PPyObject; cdecl;
var
  H: Int64;
  Ptr: PChar;
  Ed: TATSynEdit;
  Str: string;
  NChars1, NChars2, NSelIndex, NAltOrder: integer;
begin
  with AppPython.Engine do
    if Bool(PyArg_ParseTuple(Args, 'Lsiiii:complete', @H, @Ptr, @NChars1, @NChars2, @NSelIndex, @NAltOrder)) then
    begin
      Ed:= PyEditor(H);
      Str:= string(Ptr);

      fmMain.FPyComplete_Editor:= Ed;
      fmMain.FPyComplete_Text:= Str;
      fmMain.FPyComplete_CharsLeft:= NChars1;
      fmMain.FPyComplete_CharsRight:= NChars2;
      with Ed.Carets[0] do
        fmMain.FPyComplete_CaretPos:= Point(PosX, PosY);

      DoApplyCompletionListboxStyle(Bool(NAltOrder));
      DoEditorCompletionListbox(Ed, @fmMain.PyCompletionOnGetProp, nil, '_', NSelIndex);

      Result:= ReturnNone;
    end
  else
    Result:= ReturnNone;
end;

function Py_ed_complete_alt(Self, Args: PPyObject): PPyObject; cdecl;
var
  H: Int64;
  PtrText, PtrId: PChar;
  Ed: TATSynEdit;
  StrText, StrId: string;
  NChars, NSelIndex: integer;
begin
  with AppPython.Engine do
    if Bool(PyArg_ParseTuple(Args, 'Lssii:complete_alt', @H, @PtrText, @PtrId, @NChars, @NSelIndex)) then
    begin
      Ed:= PyEditor(H);
      StrText:= string(PtrText);
      StrId:= string(PtrId);

      fmMain.FPyComplete_Editor:= Ed;
      fmMain.FPyComplete_Text:= StrText;
      fmMain.FPyComplete_CharsLeft:= NChars;
      fmMain.FPyComplete_CharsRight:= 0;
      with Ed.Carets[0] do
        fmMain.FPyComplete_CaretPos:= Point(PosX, PosY);

      DoApplyCompletionListboxStyle(false);
      DoEditorCompletionListbox(Ed, @fmMain.PyCompletionOnGetProp, @fmMain.PyCompletionOnResult, StrId, NSelIndex);

      Result:= ReturnNone;
    end
  else
    Result:= ReturnNone;
end;


function Py_ini_readwrite(Self, Args: PPyObject; AWrite: boolean): PPyObject; cdecl;
var
  P1, P2, P3, P4: PChar;
  StrFN, StrSess, StrKey, StrVal: string;
  ini: TIniFile;
  fn: string;
begin
  with AppPython.Engine do
  begin
    if Bool(PyArg_ParseTuple(Args, 'ssss:ini_readwrite', @P1, @P2, @P3, @P4)) then
    begin
      StrFN:= string(P1);
      StrSess:= string(P2);
      StrKey:= string(P3);
      StrVal:= string(P4);

      fn:= StrFN;
      if ExtractFileDir(fn)='' then
        fn:= AppDir_Settings+DirectorySeparator+fn;

      ini:= TIniFile.Create(fn);
      //FreePascal 3.0.4 sets StripQuotes:=True in TIniFile.Create, but plugins need to keep quotes
      ini.Options:= ini.Options-[ifoStripComments, ifoStripQuotes];

      try
        if AWrite then
        begin
          ini.WriteString(StrSess, StrKey, StrVal);
          Result:= ReturnNone;
        end
        else
        begin
          StrVal:= ini.ReadString(StrSess, StrKey, StrVal);
          Result:= PyString_FromString(PChar(StrVal));
        end;
      finally
        FreeAndNil(ini);
      end;
    end
    else
      Result:= ReturnNone;
  end;
end;

function Py_ini_read(Self, Args: PPyObject): PPyObject; cdecl;
begin
  Result:= Py_ini_readwrite(Self, Args, false);
end;

function Py_ini_write(Self, Args: PPyObject): PPyObject; cdecl;
begin
  Result:= Py_ini_readwrite(Self, Args, true);
end;

function Py_ini_proc(Self, Args: PPyObject): PPyObject; cdecl;
var
  Id: Longint;
  PtrFN, PtrSection, PtrKey: PChar;
  StrFN, StrSection, StrKey: string;
  L: TStringList;
  Ini: TIniFile;
begin
  with AppPython.Engine do
  begin
    if Bool(PyArg_ParseTuple(Args, 'isss:ini_proc', @Id, @PtrFN, @PtrSection, @PtrKey)) then
    begin
      StrFN:= string(PtrFN);
      StrSection:= string(PtrSection);
      StrKey:= string(PtrKey);

      if ExtractFileDir(StrFN)='' then
        StrFN:= AppDir_Settings+DirectorySeparator+StrFN;

      Ini:= TIniFile.Create(StrFN);
      try
        case Id of
          INI_GET_SECTIONS:
            begin
              L:= TStringList.Create;
              try
                Ini.ReadSections(L);
                Result:= StringsToPyList(L);
              finally
                FreeAndNil(L);
              end;
            end;

          INI_GET_KEYS:
            begin
              L:= TStringList.Create;
              try
                Ini.ReadSection(StrSection, L);
                Result:= StringsToPyList(L);
              finally
                FreeAndNil(L);
              end;
            end;

          INI_DELETE_KEY:
            begin
              Ini.DeleteKey(StrSection, StrKey);
              Result:= ReturnNone;
            end;

          INI_DELETE_SECTION:
            begin
              Ini.EraseSection(StrSection);
              Result:= ReturnNone;
            end;

          else
            Result:= ReturnNone;
        end;

      finally
        FreeAndNil(Ini);
      end;
    end;
  end;
end;


function Py_dlg_color(Self, Args : PPyObject): PPyObject; cdecl;
var
  NCode: Longint;
begin
  with AppPython.Engine do
    if Bool(PyArg_ParseTuple(Args, 'i:dlg_color', @NCode)) then
    begin
      if not Assigned(fmMain.FColorDialog) then
        fmMain.FColorDialog:= TColorDialog.Create(fmMain);
      fmMain.FColorDialog.Color:= NCode;
      if fmMain.FColorDialog.Execute then
        Result:= PyInt_FromLong(fmMain.FColorDialog.Color)
      else
        Result:= ReturnNone;
    end
  else
    Result:= ReturnNone;
end;

function Py_dlg_menu(Self, Args: PPyObject): PPyObject; cdecl;
var
  PtrItems, PtrCaption: PChar;
  StrItems, StrCaption: string;
  NId, NIndex: integer;
  NCollapse, NSizeW, NSizeH: integer;
  Props: TDlgMenuProps;
begin
  with AppPython.Engine do
    if Bool(PyArg_ParseTuple(Args, 'isisiii:dlg_menu', @NId, @PtrItems, @NIndex, @PtrCaption,
                             @NCollapse, @NSizeW, @NSizeH)) then
    begin
      StrItems:= string(PtrItems);
      StrCaption:= string(PtrCaption);

      case (NId and 1) of
        MENU_LIST,
        MENU_LIST_ALT:
          begin
            FillChar(Props, SizeOf(Props), 0);
            Props.ItemsText:= StrItems;
            Props.Caption:= StrCaption;
            Props.InitialIndex:= NIndex;
            Props.Multiline:= (NId and 1)=MENU_LIST_ALT;
            Props.NoFuzzy:= (NId and MENU_NO_FUZZY)<>0;
            Props.NoFullFilter:= (NId and MENU_NO_FULLFILTER)<>0;
            Props.ShowCentered:= (NId and MENU_CENTERED)<>0;
            Props.UseEditorFont:= (NId and MENU_EDITORFONT)<>0;
            Props.Collapse:= TATCollapseStringMode(NCollapse);
            Props.W:= NSizeW;
            Props.H:= NSizeH;

            NIndex:= fmMain.DoDialogMenuApi(Props);
            Application.ProcessMessages; //force hide dialog
            if NIndex>=0 then
              Result:= PyInt_FromLong(NIndex)
            else
              Result:= ReturnNone;
          end
        else
          Result:= ReturnNone;
      end;
    end
  else
    Result:= ReturnNone;
end;

var
  AppPanelProp_Out: TAppPanelProps;
  AppPanelProp_Val: TAppPanelProps;
  AppPanelActive: string = '0';


function GetAppPanelProps_ByCaption(const ACaption: string): PAppPanelProps;
begin
  Result:= nil;
  if ACaption='0' then
    Result:= @AppPanelProp_Out
  else
  if ACaption='1' then
    Result:= @AppPanelProp_Val;
end;

function GetAppPanelProps_ByListbox(AListbox: TATListbox): PAppPanelProps;
begin
  Result:= nil;
  if AListbox=fmMain.ListboxOut then
    exit(@AppPanelProp_Out);
  if AListbox=fmMain.ListboxVal then
    exit(@AppPanelProp_Val);
end;


function Py_app_log(Self, Args: PPyObject): PPyObject; cdecl;
var
  PtrText, PtrPanel: PChar;
  StrText, StrPanel: string;
  Id, NLen, i: integer;
  NTag: Int64;
  Prop: PAppPanelProps;
  List: TStringList;
  Obj: TObject;
begin
  with AppPython.Engine do
    if Bool(PyArg_ParseTuple(Args, 'isLs:app_log', @Id, @PtrText, @NTag, @PtrPanel)) then
    begin
      StrText:= string(PtrText);
      StrPanel:= string(PtrPanel);

      if StrPanel<>'' then
        Prop:= GetAppPanelProps_ByCaption(StrPanel)
      else
        Prop:= GetAppPanelProps_ByCaption(AppPanelActive);
      if Prop=nil then
        exit(ReturnNone);

      case Id of
        LOG_CLEAR:
          begin
            for i:= Prop^.Listbox.ItemCount-1 downto 0 do
            begin
              Obj:= Prop^.Listbox.Items.Objects[i];
              if Assigned(Obj) then
                Obj.Free;
            end;
            Prop^.Listbox.Items.Clear;
            Prop^.Listbox.Invalidate;
            Result:= ReturnNone;
          end;

        LOG_ADD:
          begin
            Prop^.Listbox.Items.AddObject(StrText, TATListboxItemProp.Create(NTag, false, ''));
            Prop^.Listbox.Invalidate;
            Result:= ReturnNone;
          end;

        LOG_SET_REGEX:
          begin
            Prop^.RegexStr:= StrText;
            Result:= ReturnNone;
          end;

        LOG_SET_LINE_ID:
          begin
            Prop^.RegexIdLine:= StrToIntDef(StrText, 0);
            Result:= ReturnNone;
          end;

        LOG_SET_COL_ID:
          begin
            Prop^.RegexIdCol:= StrToIntDef(StrText, 0);
            Result:= ReturnNone;
          end;

        LOG_SET_NAME_ID:
          begin
            Prop^.RegexIdName:= StrToIntDef(StrText, 0);
            Result:= ReturnNone;
          end;

        LOG_SET_FILENAME:
          begin
            Prop^.DefFilename:= StrText;
            Result:= ReturnNone;
          end;

        LOG_SET_ZEROBASE:
          begin
            Prop^.ZeroBase:= Bool(StrToIntDef(StrText, 0));
            Result:= ReturnNone;
          end;

        LOG_GET_LINES_LIST:
          begin
            NLen:= Prop^.Listbox.Items.Count;
            Result:= PyList_New(NLen);
            if not Assigned(Result) then
              raise EPythonError.Create(msgPythonListError);
            for i:= 0 to NLen-1 do
            begin
              Obj:= Prop^.Listbox.Items.Objects[i];
              PyList_SetItem(Result, i, Py_BuildValue('(sL)',
                  PChar(Prop^.Listbox.Items[i]),
                  Int64((Obj as TATListboxItemProp).Tag)
                  ));
            end;
          end;

        LOG_GET_LINEINDEX:
          begin
            Result:= PyInt_FromLong(Prop^.Listbox.ItemIndex);
          end;

        LOG_SET_LINEINDEX:
          begin
            Prop^.Listbox.ItemIndex:= StrToIntDef(StrText, 0);
            Result:= ReturnNone;
          end;

        LOG_CONSOLE_CLEAR:
          begin
            if (StrText='') or (Pos('m', StrText)>0) then
              fmConsole.DoClearMemo(nil);

            if (StrText='') or (Pos('e', StrText)>0) then
              fmConsole.DoClearInput(nil);

            if (StrText='') or (Pos('h', StrText)>0) then
              fmConsole.DoClearHistory;

            Result:= ReturnNone;
          end;

        LOG_CONSOLE_ADD:
          with fmMain do
          begin
            MsgLogConsole(cConsolePrompt+StrText);
            fmConsole.EdInput.DoAddLineToHistory(StrText, cConsoleMaxComboboxItems);
            fmConsole.EdInput.Text:= '';
            fmConsole.EdInput.Update(true);
            Result:= ReturnNone;
          end;

        LOG_CONSOLE_GET_COMBO_LINES:
          begin
            Result:= StringsToPyList(fmConsole.EdInput.Items);
          end;

        LOG_CONSOLE_GET_MEMO_LINES:
          begin
            List:= TStringList.Create;
            try
              List.TextLineBreakStyle:= tlbsLF;
              List.Text:= UTF8Encode(fmConsole.EdMemo.Text);
              Result:= StringsToPyList(List);
            finally
              FreeAndNil(List);
            end;
          end;

        else
          Result:= ReturnNone;
      end;

      fmMain.UpdateSidebarButtonOverlay;
    end;
end;


function Py_dlg_hotkey(Self, Args: PPyObject): PPyObject; cdecl;
var
  Ptr: PChar;
  S: string;
begin
  with AppPython.Engine do
    if Bool(PyArg_ParseTuple(Args, 's:dlg_hotkey', @Ptr)) then
    begin
      S:= string(Ptr);
      S:= DoDialogHotkeyInput(S);
      if S<>'' then
        Result:= PyString_FromString(PChar(S))
      else
        Result:= ReturnNone;
    end
  else
    Result:= ReturnNone;
end;

function Py_dlg_hotkeys(Self, Args : PPyObject): PPyObject; cdecl;
var
  PtrCommand, PtrLexer: PChar;
  StrCommand, StrLexer: string;
  NCommand: integer;
  Ok: boolean;
begin
  with AppPython.Engine do
    if Bool(PyArg_ParseTuple(Args, 'ss:dlg_hotkeys', @PtrCommand, @PtrLexer)) then
    begin
      StrCommand:= string(PtrCommand);
      StrLexer:= string(PtrLexer);

      NCommand:= StrToIntDef(StrCommand, 0);
      if NCommand>0 then
        Ok:= DoDialogHotkeys(AppKeymapMain, NCommand, StrLexer)
      else
        Ok:= DoDialogHotkeys_Plugin(AppKeymapMain, StrCommand, StrLexer);

      Result:= PyBool_FromLong(Ord(Ok));
    end
  else
    Result:= ReturnNone;
end;


function Py_dlg_commands(Self, Args : PPyObject): PPyObject; cdecl;
var
  NOps: integer;
  PtrTitle: PChar;
  StrTitle, Str: string;
  W, H: integer;
  Props: TDlgCommandsProps;
begin
  with AppPython.Engine do
    if Bool(PyArg_ParseTuple(Args, 'isii:dlg_commands', @NOps, @PtrTitle, @W, @H)) then
    begin
      StrTitle:= string(PtrTitle);

      Keymap_UpdateDynamic(categ_Lexer);
      Keymap_UpdateDynamic(categ_OpenedFile);
      Keymap_UpdateDynamic(categ_RecentFile);

      FillChar(Props, SizeOf(Props), 0);
      Props.Caption:= StrTitle;
      Props.ShowUsual:= (NOps and COMMANDS_USUAL)<>0;
      Props.ShowPlugins:= (NOps and COMMANDS_PLUGINS)<>0;
      Props.ShowLexers:= (NOps and COMMANDS_LEXERS)<>0;
      Props.ShowFiles:= (NOps and COMMANDS_FILES)<>0;
      Props.ShowRecents:= (NOps and COMMANDS_RECENTS)<>0;
      Props.AllowConfig:= (NOps and COMMANDS_CONFIG)<>0;
      Props.AllowConfigForLexer:= (NOps and COMMANDS_CONFIG_LEXER)<>0;
      Props.ShowCentered:= (NOps and COMMANDS_CENTERED)<>0;
      Props.W:= W;
      Props.H:= H;

      Str:= fmMain.DoDialogCommands_Py(Props);

      if Str='' then
        Result:= ReturnNone
      else
        Result:= PyString_FromString(PChar(Str));
    end
  else
    Result:= ReturnNone;
end;


(*
procedure DoLogDialog(ASizeX, ASizeY: integer; const AText: string);
var
  STime, SLog: string;
begin
  if AText='' then exit;
  STime:= FormatDateTime('yyyy-mm-dd_hh-mm-ss', Now);
  SLog:= Format(GetTempDir+'dlg_'+STime+'_%dx%d.txt', [ASizeX, ASizeY]);

  MsgLogConsole('Dialog logged: '+SLog);

  with TFileStream.Create(SLog, fmCreate) do
  try
    WriteBuffer(AText[1], Length(AText));
  finally
    Free;
  end;
end;
*)


function Py_dlg_custom(Self, Args : PPyObject): PPyObject; cdecl;
var
  PtrTitle, PtrText: PChar;
  StrTitle, StrText, StrResult: string;
  NSizeX, NSizeY, NFocused, NButton: integer;
begin
  with AppPython.Engine do
    if Bool(PyArg_ParseTuple(Args, 'siisi:dlg_custom', @PtrTitle, @NSizeX, @NSizeY, @PtrText, @NFocused)) then
    begin
      StrTitle:= string(PtrTitle);
      StrText:= string(PtrText);
      //if UiOps.LogCustomDialogs then
      //  DoLogDialog(NSizeX, NSizeY, StrText);

      DoDialogCustom(StrTitle, NSizeX, NSizeY, StrText, NFocused, NButton, StrResult);
      if NButton<0 then
        Result:= ReturnNone
      else
        Result:= Py_BuildValue('(is)', NButton, PChar(StrResult));
    end
  else
    Result:= ReturnNone;
end;


function Py_ed_get_wrapinfo(Self, Args: PPyObject): PPyObject; cdecl;
var
  H: Int64;
  Ed: TATSynEdit;
  Data: TATWrapItem;
  NLen, i: Integer;
begin
  with AppPython.Engine do
    if Bool(PyArg_ParseTuple(Args, 'L:get_wrapinfo', @H)) then
    begin
      Ed:= PyEditor(H);
      NLen:= Ed.WrapInfo.Count;
      if NLen>0 then
      begin
        Result:= PyList_New(NLen);
        if not Assigned(Result) then
          raise EPythonError.Create(msgPythonListError);
        for i:= 0 to NLen-1 do
        begin
          Data:= Ed.WrapInfo[i];
          PyList_SetItem(Result, i,
            Py_BuildValue('{sisisisisisO}',
              'line', Data.NLineIndex,
              'char', Data.NCharIndex,
              'len', Data.NLength,
              'indent', Data.NIndent,
              'final', Ord(Data.NFinal),
              'initial', PyBool_FromLong(Ord(Data.bInitial))
              ));
        end;
      end
      else
        Result:= ReturnNone;
    end
  else
    Result:= ReturnNone;
end;


function Py_GetScrollInfo(const AInfo: TATSynScrollInfo): PPyObject;
begin
  with AppPython.Engine do
  begin
    Result:= Py_BuildValue('{sisisisisisisisisisi}',
      'pos', AInfo.NPos,
      'pos_last', AInfo.NPosLast,
      'max', AInfo.NMax,
      'page', AInfo.NPage,
      'pixels', AInfo.NPixelOffset,
      'char_size', AInfo.SmoothCharSize,
      'smooth_pos', AInfo.SmoothPos,
      'smooth_pos_last', AInfo.SmoothPosLast,
      'smooth_max', AInfo.SmoothMax,
      'smooth_page', AInfo.SmoothPage
      );
  end;
end;

function Py_ed_get_prop(Self, Args: PPyObject): PPyObject; cdecl;
var
  H: Int64;
  Id, Num1, Num2: integer;
  Ptr: PChar;
  Str: string;
  Ed: TATSynEdit;
  F: TEditorFrame;
  Pnt: TPoint;
  Ada: TATAdapterEControl;
  Lex: TecSyntAnalyzer;
  Sep: TATStringSeparator;
begin
  with AppPython.Engine do
    if Bool(PyArg_ParseTuple(Args, 'Lis:get_prop', @H, @Id, @Ptr)) then
    begin
      Ed:= PyEditor(H);
      Str:= string(Ptr);
      F:= GetEditorFrame(Ed);

      case Id of
        PROP_GUTTER_ALL:
          Result:= PyBool_FromLong(Ord(Ed.OptGutterVisible));

        PROP_GUTTER_STATES:
          Result:= PyBool_FromLong(Ord(Ed.Gutter[Ed.GutterBandStates].Visible));

        PROP_GUTTER_NUM:
          Result:= PyBool_FromLong(Ord(Ed.Gutter[Ed.GutterBandNumbers].Visible));

        PROP_GUTTER_FOLD:
          Result:= PyBool_FromLong(Ord(Ed.Gutter[Ed.GutterBandFolding].Visible));

        PROP_GUTTER_BM:
          Result:= PyBool_FromLong(Ord(Ed.Gutter[Ed.GutterBandBookmarks].Visible));

        PROP_WRAP:
          Result:= PyInt_FromLong(Ord(Ed.OptWrapMode));

        PROP_RO:
          Result:= PyBool_FromLong(Ord(Ed.ModeReadOnly));

        PROP_TAB_SPACES:
          Result:= PyBool_FromLong(Ord(Ed.OptTabSpaces));

        PROP_TAB_SIZE:
          Result:= PyInt_FromLong(Ed.OptTabSize);

        PROP_MARGIN:
          Result:= PyInt_FromLong(Ed.OptMarginRight);

        PROP_MARGIN_STRING:
          Result:= PyString_FromString(PChar(Ed.OptMarginString));

        PROP_INSERT:
          Result:= PyBool_FromLong(Ord(not Ed.ModeOverwrite));

        PROP_MODIFIED:
          Result:= PyBool_FromLong(Ord(Ed.Modified));

        PROP_MODIFIED_VERSION:
          Result:= PyLong_FromLongLong(Ed.Strings.ModifiedVersion);

        PROP_RULER:
          Result:= PyBool_FromLong(Ord(Ed.OptRulerVisible));

        PROP_LINE_STATE:
          begin
            Num1:= StrToIntDef(Str, -1);
            if Ed.Strings.IsIndexValid(Num1) then
              Result:= PyInt_FromLong(Ord(Ed.Strings.LinesState[Num1]))
            else
              Result:= ReturnNone;
          end;

        PROP_LINE_STATES:
          Result:= Py_GetLineStateList(Ed);

        PROP_LINE_STATES_UPDATED:
          Result:= Py_GetLineUpdatedList(Ed);

        PROP_ZEBRA:
          begin
            if Ed.OptZebraActive then
              Num1:= Ed.OptZebraAlphaBlend
            else
              Num1:= 0;
            Result:= PyInt_FromLong(Num1);
          end;

        PROP_ZEBRA_STEP:
          Result:= PyInt_FromLong(Ed.OptZebraStep);

        PROP_COLOR:
          begin
            Num1:= EditorGetColorById(Ed, Str);
            if Num1<>-1 then
              Result:= PyInt_FromLong(Num1)
            else
              Result:= ReturnNone;
          end;

        PROP_LINE_TOP:
          Result:= PyInt_FromLong(Ed.LineTop);

        PROP_LINE_NUMBERS:
          Result:= PyInt_FromLong(Ord(Ed.OptNumbersStyle));

        PROP_LEXER_FILE:
          begin
            //if editor in frame, use frame's prop
            if Assigned(F) then
            begin
              Str:= F.LexerName[Ed];
              Result:= PyString_FromString(PChar(Str));
            end
            else
            if Ed.AdapterForHilite is TATAdapterEControl then
            begin
              Ada:= Ed.AdapterForHilite as TATAdapterEControl;
              Lex:= Ada.Lexer;
              if Assigned(Lex) then
                Result:= PyString_FromString(PChar(Lex.LexerName))
              else
                Result:= PyString_FromString('');
            end
            else
              Result:= PyString_FromString('');
          end;

        PROP_LEXER_POS:
          begin
            Sep.Init(Str, ',');
            Sep.GetItemInt(Pnt.X, 0);
            Sep.GetItemInt(Pnt.Y, 0);
            //if editor in frame, use frame's prop
            if Assigned(F) then
              Result:= PyString_FromString(PChar(F.LexerNameAtPos(Ed, Pnt)))
            else
            if Ed.AdapterForHilite is TATAdapterEControl then
            begin
              Ada:= Ed.AdapterForHilite as TATAdapterEControl;
              Lex:= Ada.LexerAtPos(Pnt);
              if Assigned(Lex) then
                Result:= PyString_FromString(PChar(Lex.LexerName))
              else
                Result:= PyString_FromString('');
            end
            else
              Result:= PyString_FromString('');
          end;

        PROP_LEXER_CARET:
          begin
            with Ed.Carets[0] do
            begin
              Pnt.X:= PosX;
              Pnt.Y:= PosY;
            end;
            //if editor in frame, use frame's prop
            if Assigned(F) then
              Result:= PyString_FromString(PChar(F.LexerNameAtPos(Ed, Pnt)))
            else
            if Ed.AdapterForHilite is TATAdapterEControl then
            begin
              Ada:= Ed.AdapterForHilite as TATAdapterEControl;
              Lex:= Ada.LexerAtPos(Pnt);
              if Assigned(Lex) then
                Result:= PyString_FromString(PChar(Lex.LexerName))
              else
                Result:= PyString_FromString('');
            end
            else
              Result:= PyString_FromString('');
          end;

        PROP_TAB_COLOR:
          begin
            if Assigned(F) then
              Result:= PyInt_FromLong(F.TabColor)
            else
              Result:= ReturnNone;
          end;

        PROP_TAB_TITLE:
          begin
            if Assigned(F) then
              Result:= PyString_FromString(PChar(F.TabCaption))
            else
              Result:= ReturnNone;
          end;

        PROP_TAB_ID:
          begin
            if Assigned(F) then
              Result:= PyLong_FromLong(F.TabId)
            else
              Result:= ReturnNone;
          end;

        PROP_ENC:
          begin
            Str:= AppEncodingFullnameToShortname(Ed.EncodingName);
            Result:= PyString_FromString(PChar(Str));
          end;

        PROP_IN_SESSION:
          begin
            if Assigned(F) then
              Result:= PyBool_FromLong(Ord(F.InSession))
            else
              Result:= PyBool_FromLong(0);
          end;

        PROP_IN_HISTORY:
          begin
            if Assigned(F) then
              Result:= PyBool_FromLong(Ord(F.InHistory))
            else
              Result:= PyBool_FromLong(0);
          end;

        PROP_NEWLINE:
          begin
            case Ed.Strings.Endings of
              cEndWin:
                Str:= 'crlf';
              cEndMac:
                Str:= 'cr';
              else
                Str:= 'lf';
            end;
            Result:= PyString_FromString(PChar(Str));
          end;

        PROP_INDEX_GROUP:
          begin
            fmMain.GetEditorIndexes(Ed, Num1, Num2);
            Result:= PyLong_FromLongLong(Num1);
          end;

        PROP_INDEX_TAB:
          begin
            fmMain.GetEditorIndexes(Ed, Num1, Num2);
            Result:= PyLong_FromLongLong(Num2);
          end;

        PROP_KIND:
          begin
            if not Assigned(F) then exit(ReturnNone);
            if F.IsBinary then
              Str:= 'bin'
            else
            if F.IsPicture then
              Str:= 'pic'
            else
              Str:= 'text';
            Result:= PyString_FromString(PChar(Str));
          end;

        PROP_TAG:
          begin
            Result:= PyString_FromString(PChar(Ed.TagString));
          end;

        PROP_UNPRINTED_SHOW:
          Result:= PyBool_FromLong(Ord(Ed.OptUnprintedVisible));

        PROP_UNPRINTED_SPACES:
          Result:= PyBool_FromLong(Ord(Ed.OptUnprintedSpaces));

        PROP_UNPRINTED_SPACES_TRAILING:
          Result:= PyBool_FromLong(Ord(Ed.OptUnprintedSpacesTrailing));

        PROP_UNPRINTED_ENDS:
          Result:= PyBool_FromLong(Ord(Ed.OptUnprintedEnds));

        PROP_UNPRINTED_END_DETAILS:
          Result:= PyBool_FromLong(Ord(Ed.OptUnprintedEndsDetails));

        PROP_TAB_ICON:
          begin
            if Assigned(F) then
              Result:= PyLong_FromLongLong(F.TabImageIndex)
            else
              Result:= ReturnNone;
          end;

        PROP_TAB_COLLECT_MARKERS:
          begin
            if Assigned(F) then
              Result:= PyBool_FromLong(Ord(F.TabKeyCollectMarkers))
            else
              Result:= ReturnNone;
          end;

        PROP_MACRO_REC:
          begin
            if Assigned(F) then
              Result:= PyBool_FromLong(Ord(F.MacroRecord))
            else
              Result:= ReturnNone;
          end;

        PROP_MARKED_RANGE:
          begin
            Ed.DoGetMarkedLines(Num1, Num2);
            Result:= Py_BuildValue('(ii)', Num1, Num2);
          end;

        PROP_CARET_VIRTUAL:
          Result:= PyBool_FromLong(Ord(Ed.OptCaretVirtual));

        PROP_CARET_STOP_UNFOCUSED:
          Result:= PyBool_FromLong(Ord(Ed.OptCaretStopUnfocused));

        PROP_CARET_VIEW:
          Result:= Py_GetCaretProps(Ed.CaretPropsNormal);

        PROP_CARET_VIEW_OVR:
          Result:= Py_GetCaretProps(Ed.CaretPropsOverwrite);

        PROP_CARET_VIEW_RO:
          Result:= Py_GetCaretProps(Ed.CaretPropsReadonly);

        PROP_MINIMAP:
          Result:= PyBool_FromLong(Ord(Ed.OptMinimapVisible));

        PROP_MICROMAP:
          Result:= PyBool_FromLong(Ord(Ed.OptMicromapVisible));

        PROP_VISIBLE_LINES:
          Result:= PyInt_FromLong(Ed.GetVisibleLines);

        PROP_VISIBLE_COLUMNS:
          Result:= PyInt_FromLong(Ed.GetVisibleColumns);

        PROP_LINE_BOTTOM:
          Result:= PyInt_FromLong(Ed.LineBottom);

        PROP_SCROLL_VERT:
          Result:= PyInt_FromLong(Ed.ScrollVert.NPos);

        PROP_SCROLL_HORZ:
          Result:= PyInt_FromLong(Ed.ScrollHorz.NPos);

        PROP_SCROLL_VERT_SMOOTH:
          Result:= PyInt_FromLong(Ed.ScrollVert.SmoothPos);

        PROP_SCROLL_HORZ_SMOOTH:
          Result:= PyInt_FromLong(Ed.ScrollHorz.SmoothPos);

        PROP_SCROLL_VERT_INFO:
          Result:= Py_GetScrollInfo(Ed.ScrollVert);

        PROP_SCROLL_HORZ_INFO:
          Result:= Py_GetScrollInfo(Ed.ScrollHorz);

        PROP_SCALE_FONT:
          Result:= PyInt_FromLong(Ed.OptScaleFont);

        PROP_LAST_LINE_ON_TOP:
          Result:= PyBool_FromLong(Ord(Ed.OptLastLineOnTop));

        PROP_HILITE_CUR_COL:
          Result:= PyBool_FromLong(Ord(Ed.OptShowCurColumn));

        PROP_HILITE_CUR_LINE:
          Result:= PyBool_FromLong(Ord(Ed.OptShowCurLine));

        PROP_HILITE_CUR_LINE_MINIMAL:
          Result:= PyBool_FromLong(Ord(Ed.OptShowCurLineMinimal));

        PROP_HILITE_CUR_LINE_IF_FOCUS:
          Result:= PyBool_FromLong(Ord(Ed.OptShowCurLineOnlyFocused));

        PROP_CODETREE:
          begin
            if Assigned(F) then
              Result:= PyBool_FromLong(Ord(F.EnabledCodeTree[Ed]))
            else
              Result:= PyBool_FromLong(0);
          end;

        PROP_EDITORS_LINKED:
          begin
            if Assigned(F) then
              Result:= PyBool_FromLong(Ord(F.EditorsLinked))
            else
              Result:= PyBool_FromLong(0);
          end;

        PROP_ACTIVATION_TIME:
          begin
            if Assigned(F) then
              Result:= PyLong_FromLongLong(Int64(F.ActivationTime))
            else
              Result:= ReturnNone;
          end;

        PROP_LINK_AT_POS:
          begin
            Sep.Init(Str);
            Sep.GetItemInt(Pnt.X, -1);
            Sep.GetItemInt(Pnt.Y, -1);
            Str:= Ed.DoGetLinkAtPos(Pnt.X, Pnt.Y);
            Result:= PyString_FromString(PChar(Str));
          end;

        PROP_PICTURE:
          begin
            if (F=nil) or (F.IsText) then
              Result:= ReturnNone
            else
            begin
              Pnt:= F.PictureSizes;
              Result:= Py_BuildValue('(sii)', PChar(F.FileName), Pnt.X, Pnt.Y);
            end;
          end;

        PROP_COORDS:
          begin
            Result:= Py_rect_control(Ed);
          end;

        PROP_ONE_LINE:
          begin
            Result:= PyBool_FromLong(Ord(Ed.ModeOneLine));
          end;

        PROP_INDENT_SIZE:
          begin
            Result:= PyInt_FromLong(Ed.OptIndentSize);
          end;

        PROP_INDENT_KEEP_ALIGN:
          begin
            Result:= PyBool_FromLong(Ord(Ed.OptIndentKeepsAlign));
          end;

        PROP_INDENT_AUTO:
          begin
            Result:= PyBool_FromLong(Ord(Ed.OptAutoIndent));
          end;

        PROP_INDENT_KIND:
          begin
            Result:= PyInt_FromLong(Ord(Ed.OptAutoIndentKind));
          end;

        PROP_CELL_SIZE:
          begin
            Result:= Py_BuildValue('(ii)', Ed.TextCharSize.X, Ed.TextCharSize.Y);
          end;

        PROP_MODERN_SCROLLBAR:
          begin
            Result:= PyBool_FromLong(Ord(Ed.OptScrollbarsNew));
          end;

        PROP_SAVE_HISTORY:
          begin
            if Assigned(F) then
              Result:= PyBool_FromLong(Ord(F.SaveHistory))
            else
              Result:= PyBool_FromLong(0);
          end;

        PROP_PREVIEW:
          begin
            if Assigned(F) then
              Result:= PyBool_FromLong(Ord(F.TabIsPreview))
            else
              Result:= PyBool_FromLong(0);
          end;

        PROP_UNDO_GROUPED:
          Result:= PyBool_FromLong(Ord(Ed.OptUndoGrouped));

        PROP_UNDO_LIMIT:
          Result:= PyInt_FromLong(Ed.OptUndoLimit);

        PROP_UNDO_DATA:
          Result:= PyString_FromString(PChar(Ed.UndoAsString));

        PROP_REDO_DATA:
          Result:= PyString_FromString(PChar(Ed.RedoAsString));

        PROP_SPLIT:
          begin
            if Assigned(F) then
            begin
              if not F.Splitted then
                Str:= '-'
              else
              if F.SplitHorz then
                Str:= 'h'
              else
                Str:= 'v';
              Result:= Py_BuildValue('(si)', PChar(Str), Round(F.SplitPos*1000));
            end
            else
              Result:= ReturnNone;
          end;

        PROP_SAVING_FORCE_FINAL_EOL:
          Result:= PyBool_FromLong(Ord(Ed.OptSavingForceFinalEol));

        PROP_SAVING_TRIM_SPACES:
          Result:= PyBool_FromLong(Ord(Ed.OptSavingTrimSpaces));

        PROP_SAVING_TRIM_FINAL_EMPTY_LINES:
          Result:= PyBool_FromLong(Ord(Ed.OptSavingTrimFinalEmptyLines));

        PROP_FOLD_TOOLTIP_SHOW:
          begin
            Result:= PyBool_FromLong(Ord(Ed.OptFoldTooltipVisible));
          end;

        PROP_FOLD_ALWAYS:
          begin
            Result:= PyBool_FromLong(Ord(Ed.OptGutterShowFoldAlways));
          end;

        PROP_FOLD_ICONS:
          begin
            Result:= PyBool_FromLong(Ord(Ed.OptGutterIcons));
          end;

        PROP_HANDLE_SELF:
          begin
            Result:= Py_ObjectHandle(Ed);
          end;

        PROP_HANDLE_PRIMARY:
          begin
            if Assigned(F) then
              Result:= Py_ObjectHandle(F.Ed1)
            else
              Result:= ReturnNone;
          end;
        PROP_HANDLE_SECONDARY:
          begin
            if Assigned(F) then
              Result:= Py_ObjectHandle(F.Ed2)
            else
              Result:= ReturnNone;
          end;

        PROP_HANDLE_PARENT:
          begin
            Result:= Py_ObjectHandle(Ed.Parent);
          end;

        PROP_SCROLLSTYLE_HORZ:
          begin
            Result:= PyLong_FromLong(Ord(Ed.OptScrollStyleHorz));
          end;

        PROP_SCROLLSTYLE_VERT:
          begin
            Result:= PyLong_FromLong(Ord(Ed.OptScrollStyleVert));
          end;

        PROP_LINKS_SHOW:
          Result:= PyBool_FromLong(Ord(Ed.OptShowURLs));

        PROP_LINKS_REGEX:
          Result:= PyString_FromString(PChar(Ed.OptShowURLsRegex));

        PROP_LINKS_CLICKS:
          begin
            Num1:= 0;
            if Ed.OptMouseClickOpensURL then Num1:= 1
            else
            if Ed.OptMouse2ClickOpensURL then Num1:= 2;
            Result:= PyInt_FromLong(Num1);
          end;

        PROP_RECT_CLIENT:
          begin
            Result:= Py_rect(Ed.ClientRect);
          end;
        PROP_RECT_TEXT:
          begin
            Result:= Py_rect(Ed.RectMain);
          end;

        PROP_COMBO_ITEMS:
          begin
            if Ed is TATComboEdit then
              Result:= StringsToPyList(TATComboEdit(Ed).Items)
            else
              Result:= ReturnNone;
          end;

        PROP_CODETREE_MODIFIED_VERSION:
          Result:= PyInt_FromLong(fmMain.FCodetreeModifiedVersion);

        PROP_CODETREE_SUBLEXER:
          begin
            if Ed.AdapterForHilite is TATAdapterEControl then
              Result:= PyBool_FromLong(Ord((Ed.AdapterForHilite as TATAdapterEControl).EnabledSublexerTreeNodes))
            else
              Result:= PyBool_FromLong(0);
          end;

        PROP_FONT:
          Result:= Py_GetFontProps(Ed.Font);
        PROP_FONT_B:
          Result:= Py_GetFontProps(Ed.FontBold);
        PROP_FONT_I:
          Result:= Py_GetFontProps(Ed.FontItalic);
        PROP_FONT_BI:
          Result:= Py_GetFontProps(Ed.FontBoldItalic);

        PROP_V_MODE:
          begin
            if Assigned(F) and F.IsBinary then
              Num1:= Ord(F.Binary.Mode)
            else
              Num1:= VMODE_NONE;
            Result:= PyInt_FromLong(Num1);
          end;

        PROP_V_POS:
          begin
            if not Assigned(F) then exit(ReturnNone);
            if F.IsBinary then
              Result:= PyLong_FromLongLong(F.Binary.PosOffset)
            else
              Result:= ReturnNone;
          end;

        PROP_V_SEL_START:
          begin
            if not Assigned(F) then exit(ReturnNone);
            if F.IsBinary then
              Result:= PyLong_FromLongLong(F.Binary.SelStart)
            else
              Result:= ReturnNone;
          end;

        PROP_V_SEL_LEN:
          begin
            if not Assigned(F) then exit(ReturnNone);
            if F.IsBinary then
              Result:= PyLong_FromLongLong(F.Binary.SelLength)
            else
              Result:= ReturnNone;
          end;

        PROP_V_WIDTH:
          begin
            if not Assigned(F) then exit(ReturnNone);
            if F.IsBinary then
              Result:= PyLong_FromLong(F.Binary.TextWidth)
            else
              Result:= ReturnNone;
          end;

        PROP_V_WIDTH_HEX:
          begin
            if not Assigned(F) then exit(ReturnNone);
            if F.IsBinary then
              Result:= PyLong_FromLong(F.Binary.TextWidthHex)
            else
              Result:= ReturnNone;
          end;

        PROP_V_WIDTH_UHEX:
          begin
            if not Assigned(F) then exit(ReturnNone);
            if F.IsBinary then
              Result:= PyLong_FromLong(F.Binary.TextWidthUHex)
            else
              Result:= ReturnNone;
          end;

        else
          Result:= ReturnNone;
      end;
    end
  else
    Result:= ReturnNone;
end;

function Py_ed_set_prop(Self, Args: PPyObject): PPyObject; cdecl;
var
  H: Int64;
  Id: integer;
  Ptr: PChar;
  Str, Str1, Str2: string;
  Ed: TATSynEdit;
  F: TEditorFrame;
  An, AnOut: TecSyntAnalyzer;
  ValueBool: boolean;
  ValueInt: Int64;
  ScrollInfo: TATSynScrollInfo;
  Gr: TATGroups;
  Pages, PagesTo: TATPages;
  NLocalGroup, NGlobalGroup, NTab, NIndexNew, N1, N2: integer;
  LineEnd: TATLineEnds;
  Params: TAppVariantArray;
begin
  with AppPython.Engine do
    if Bool(PyArg_ParseTuple(Args, 'Lis:set_prop', @H, @Id, @Ptr)) then
    begin
      Ed:= PyEditor(H);
      F:= GetEditorFrame(Ed);
      Str:= string(Ptr);

      ValueBool:= AppStrToBool(Str);
      ValueInt:= StrToInt64Def(Str, 0);

      case Id of
        PROP_GUTTER_ALL:
          Ed.OptGutterVisible:= ValueBool;

        PROP_GUTTER_STATES:
          Ed.Gutter[Ed.GutterBandStates].Visible:= ValueBool;

        PROP_GUTTER_NUM:
          Ed.Gutter[Ed.GutterBandNumbers].Visible:= ValueBool;

        PROP_GUTTER_FOLD:
          Ed.Gutter[Ed.GutterBandFolding].Visible:= ValueBool;

        PROP_GUTTER_BM:
          Ed.Gutter[Ed.GutterBandBookmarks].Visible:= ValueBool;

        PROP_WRAP:
          Ed.OptWrapMode:= TATSynWrapMode(ValueInt);

        PROP_RO:
          Ed.ModeReadOnly:= ValueBool;

        PROP_TAB_SPACES:
          Ed.OptTabSpaces:= ValueBool;

        PROP_TAB_SIZE:
          Ed.OptTabSize:= ValueInt;

        PROP_MARGIN:
          Ed.OptMarginRight:= ValueInt;

        PROP_MARGIN_STRING:
          Ed.OptMarginString:= Str;

        PROP_INSERT:
          Ed.ModeOverwrite:= not ValueBool;

        PROP_MODIFIED:
          begin
            Ed.Modified:= ValueBool;
            if Assigned(F) then
              F.UpdateModified(Ed);
          end;

        PROP_RULER:
          Ed.OptRulerVisible:= ValueBool;

        PROP_LEXER_FILE:
          begin
            //for editors inside TEditorFrame
            if Assigned(F) then
            begin
              F.LexerName[Ed]:= Str;
              fmMain.UpdateStatus;
            end
            else
            //for other editors
            if Assigned(Ed.AdapterForHilite) then
            begin
              An:= AppManager.FindLexerByName(Str);
              //apply syntax theme
              if Assigned(An) then
                DoApplyLexerStylesMap(An, AnOut);
              (Ed.AdapterForHilite as TATAdapterEControl).Lexer:= An;
            end;
          end;

        PROP_COLOR:
          begin
            SSplitByChar(Str, ',', Str1, Str2);
            N1:= StrToIntDef(Str2, -1);
            if N1<>-1 then
              EditorSetColorById(Ed, Str1, N1);
          end;

        PROP_LINE_TOP:
          begin
            if Assigned(F) then
              if F.IsParsingBusy then
                F.TopLineTodo:= ValueInt;
            Ed.LineTop:= ValueInt;
          end;

        PROP_LINE_NUMBERS:
          begin
            Ed.OptNumbersStyle:= TATSynNumbersStyle(ValueInt);
          end;

        PROP_LINE_STATE:
          begin
            SSplitByChar(Str, ',', Str1, Str2);
            N1:= StrToIntDef(Str1, -1);
            N2:= StrToIntDef(Str2, -1);
            if Ed.Strings.IsIndexValid(N1) and (N2>=0) then
              Ed.Strings.LinesState[N1]:= TATLineState(N2);
          end;

        PROP_LINE_STATES_UPDATED:
          begin
            if Str='' then
              Ed.Strings.DoClearLineStatesUpdated;
          end;

        PROP_ZEBRA:
          begin
            N1:= StrToIntDef(Str, -1);
            if N1=0 then
              Ed.OptZebraActive:= false
            else
            if N1>0 then
            begin
              Ed.OptZebraActive:= true;
              Ed.OptZebraAlphaBlend:= N1;
            end;
          end;

        PROP_ZEBRA_STEP:
          Ed.OptZebraStep:= Max(2, ValueInt);

        PROP_TAB_COLOR:
          begin
            if Assigned(F) then
              F.TabColor:= ValueInt;
          end;

        PROP_TAB_TITLE:
          begin
            if Assigned(F) then
            begin
              F.TabCaptionFromApi:= true;
              F.TabCaption:= Str;
            end;
          end;

        PROP_ENC:
          begin
            if Assigned(F) then
              fmMain.SetFrameEncoding(Ed, AppEncodingShortnameToFullname(Str), false)
            else
              Ed.EncodingName:= AppEncodingShortnameToFullname(Str);
          end;

        PROP_ENC_RELOAD:
          begin
            if Assigned(F) then
              fmMain.SetFrameEncoding(Ed, AppEncodingShortnameToFullname(Str), true);
          end;

        PROP_NEWLINE:
          begin
            if Str='lf' then
              LineEnd:= cEndUnix
            else
            if Str='crlf' then
              LineEnd:= cEndWin
            else
            if Str='cr' then
              LineEnd:= cEndMac
            else
              LineEnd:= cEndNone;

            if LineEnd<>cEndNone then
            begin
              if Assigned(F) then
                fmMain.UpdateFrameLineEnds(F, LineEnd)
              else
              begin
                Ed.Strings.Endings:= LineEnd;
                Ed.Update;
              end;
            end;
          end;

        PROP_TAG:
          Ed.TagString:= Str;

        PROP_UNPRINTED_SHOW:
          Ed.OptUnprintedVisible:= ValueBool;

        PROP_UNPRINTED_SPACES:
          Ed.OptUnprintedSpaces:= ValueBool;

        PROP_UNPRINTED_SPACES_TRAILING:
          Ed.OptUnprintedSpacesTrailing:= ValueBool;

        PROP_UNPRINTED_ENDS:
          Ed.OptUnprintedEnds:= ValueBool;

        PROP_UNPRINTED_END_DETAILS:
          Ed.OptUnprintedEndsDetails:= ValueBool;

        PROP_TAB_ICON:
          begin
            if Assigned(F) then
              F.TabImageIndex:= ValueInt;
          end;

        PROP_TAB_COLLECT_MARKERS:
          begin
            if Assigned(F) then
              F.TabKeyCollectMarkers:= ValueBool;
          end;

        PROP_INDEX_GROUP:
          begin
            NIndexNew:= ValueInt;
            if Assigned(F) then
            begin
             GetFrameLocation(F, Gr, Pages, NLocalGroup, NGlobalGroup, NTab);
             if NTab>=0 then
              if NGlobalGroup<>NIndexNew then
              begin
                case NIndexNew of
                  0..5:
                    PagesTo:= fmMain.Groups.Pages[NIndexNew];
                  6:
                    begin
                      fmMain.ShowFloatGroup1:= true;
                      PagesTo:= fmMain.GroupsF1.Pages1;
                    end;
                  7:
                    begin
                      fmMain.ShowFloatGroup2:= true;
                      PagesTo:= fmMain.GroupsF2.Pages1;
                    end;
                  8:
                    begin
                      fmMain.ShowFloatGroup3:= true;
                      PagesTo:= fmMain.GroupsF3.Pages1;
                    end;
                  else
                    PagesTo:= nil;
                end;

                if Assigned(PagesTo) then
                  Gr.MoveTab(Pages, NTab, PagesTo, -1, true);
              end;
            end;
          end;

        PROP_INDEX_TAB:
          begin
            if Assigned(F) then
            begin
              NIndexNew:= ValueInt;
              GetFrameLocation(F, Gr, Pages, NLocalGroup, NGlobalGroup, NTab);
              if NTab>=0 then
              begin
                Pages.Tabs.MoveTab(NTab, NIndexNew, true);
                SetLength(Params, 0);
                fmMain.DoPyEvent(Ed, cEventOnTabMove, Params);
              end;
            end;
          end;

        PROP_CARET_VIEW:
          EditorCaretPropsFromPyTuple(Ed.CaretPropsNormal, Str);

        PROP_CARET_VIEW_OVR:
          EditorCaretPropsFromPyTuple(Ed.CaretPropsOverwrite, Str);

        PROP_CARET_VIEW_RO:
          EditorCaretPropsFromPyTuple(Ed.CaretPropsReadonly, Str);

        PROP_CARET_VIRTUAL:
          Ed.OptCaretVirtual:= ValueBool;

        PROP_CARET_STOP_UNFOCUSED:
          Ed.OptCaretStopUnfocused:= ValueBool;

        PROP_MINIMAP:
          Ed.OptMinimapVisible:= ValueBool;

        PROP_MICROMAP:
          Ed.OptMicromapVisible:= ValueBool;

        PROP_SCROLL_VERT:
          begin
            ScrollInfo:= Ed.ScrollVert;
            ScrollInfo.NPos:= ValueInt;
            Ed.ScrollVert:= ScrollInfo;
            Ed.UpdateScrollbars(true);
          end;

        PROP_SCROLL_HORZ:
          begin
            ScrollInfo:= Ed.ScrollHorz;
            ScrollInfo.NPos:= ValueInt;
            Ed.ScrollHorz:= ScrollInfo;
            Ed.UpdateScrollbars(true);
          end;

        PROP_SCROLL_VERT_SMOOTH:
          begin
            ScrollInfo:= Ed.ScrollVert;
            Ed.UpdateScrollInfoFromSmoothPos(ScrollInfo, ValueInt);
            Ed.ScrollVert:= ScrollInfo;
            Ed.UpdateScrollbars(false);
          end;

        PROP_SCROLL_HORZ_SMOOTH:
          begin
            ScrollInfo:= Ed.ScrollHorz;
            Ed.UpdateScrollInfoFromSmoothPos(ScrollInfo, ValueInt);
            Ed.ScrollHorz:= ScrollInfo;
            Ed.UpdateScrollbars(false);
          end;

        PROP_SCALE_FONT:
          Ed.OptScaleFont:= ValueInt;

        PROP_LAST_LINE_ON_TOP:
          Ed.OptLastLineOnTop:= ValueBool;

        PROP_HILITE_CUR_COL:
          Ed.OptShowCurColumn:= ValueBool;

        PROP_HILITE_CUR_LINE:
          Ed.OptShowCurLine:= ValueBool;

        PROP_HILITE_CUR_LINE_MINIMAL:
          Ed.OptShowCurLineMinimal:= ValueBool;

        PROP_HILITE_CUR_LINE_IF_FOCUS:
          Ed.OptShowCurLineOnlyFocused:= ValueBool;

        PROP_CODETREE:
          begin
            if Assigned(F) then
              F.EnabledCodeTree[Ed]:= ValueBool;
          end;

        PROP_EDITORS_LINKED:
          begin
            if Assigned(F) then
              F.EditorsLinked:= ValueBool;
          end;

        PROP_ONE_LINE:
          Ed.ModeOneLine:= ValueBool;

        PROP_INDENT_SIZE:
          Ed.OptIndentSize:= ValueInt;

        PROP_INDENT_KEEP_ALIGN:
          Ed.OptIndentKeepsAlign:= ValueBool;

        PROP_INDENT_AUTO:
          Ed.OptAutoIndent:= ValueBool;

        PROP_INDENT_KIND:
          Ed.OptAutoIndentKind:= TATAutoIndentKind(ValueInt);

        PROP_MARKED_RANGE:
          begin
            SSplitByChar(Str, ',', Str1, Str2);
            N1:= StrToIntDef(Str1, -1);
            N2:= StrToIntDef(Str2, -1);
            Ed.DoSetMarkedLines(N1, N2);
          end;

        PROP_MODERN_SCROLLBAR:
          Ed.OptScrollbarsNew:= ValueBool;

        PROP_SAVE_HISTORY:
          begin
            if Assigned(F) then
              F.SaveHistory:= ValueBool;
          end;

        PROP_PREVIEW:
          begin
            if Assigned(F) then
              F.TabIsPreview:= ValueBool;
          end;

        PROP_UNDO_GROUPED:
          Ed.OptUndoGrouped:= ValueBool;

        PROP_UNDO_LIMIT:
          Ed.OptUndoLimit:= ValueInt;

        PROP_UNDO_DATA:
          Ed.UndoAsString:= Str;

        PROP_REDO_DATA:
          Ed.RedoAsString:= Str;

        PROP_SPLIT:
          begin
            if Assigned(F) then
            begin
              SSplitByChar(Str, ',', Str1, Str2);
              F.Splitted:= Str1<>'-';
              F.SplitHorz:= Str1='h';
              F.SplitPos:= StrToIntDef(Str2, 500)/1000;
            end;
          end;

        PROP_SAVING_FORCE_FINAL_EOL:
          Ed.OptSavingForceFinalEol:= ValueBool;

        PROP_SAVING_TRIM_SPACES:
          Ed.OptSavingTrimSpaces:= ValueBool;

        PROP_SAVING_TRIM_FINAL_EMPTY_LINES:
          Ed.OptSavingTrimFinalEmptyLines:= ValueBool;

        PROP_FOLD_TOOLTIP_SHOW:
          Ed.OptFoldTooltipVisible:= ValueBool;

        PROP_FOLD_ALWAYS:
          Ed.OptGutterShowFoldAlways:= ValueBool;

        PROP_FOLD_ICONS:
          Ed.OptGutterIcons:= TATGutterIconsKind(ValueInt);

        PROP_SCROLLSTYLE_HORZ:
          Ed.OptScrollStyleHorz:= TATSynEditScrollStyle(ValueInt);

        PROP_SCROLLSTYLE_VERT:
          Ed.OptScrollStyleVert:= TATSynEditScrollStyle(ValueInt);

        PROP_LINKS_SHOW:
          Ed.OptShowURLs:= ValueBool;

        PROP_LINKS_REGEX:
          Ed.OptShowURLsRegex:= Str;

        PROP_LINKS_CLICKS:
          begin
            Ed.OptMouseClickOpensURL:= ValueInt=1;
            Ed.OptMouse2ClickOpensURL:= ValueInt=2;
          end;

        PROP_COMBO_ITEMS:
          begin
            if Ed is TATComboEdit then
              TATComboEdit(Ed).Items.Text:= Str;
          end;

        PROP_CODETREE_SUBLEXER:
          begin
            if Ed.AdapterForHilite is TATAdapterEControl then
              (Ed.AdapterForHilite as TATAdapterEControl).EnabledSublexerTreeNodes:= ValueBool;
          end;

        PROP_FONT:
          EditorSetFont(Ed.Font, Str);
        PROP_FONT_B:
          EditorSetFont(Ed.FontBold, Str);
        PROP_FONT_I:
          EditorSetFont(Ed.FontItalic, Str);
        PROP_FONT_BI:
          EditorSetFont(Ed.FontBoldItalic, Str);

        PROP_V_MODE:
          begin
            if not Assigned(F) then exit(ReturnNone);
            if ValueInt=VMODE_NONE then
            begin
              if F.IsBinary then
              begin
                Str:= F.FileName;
                F.DoFileClose;
                F.DoFileOpen(Str, '', false, true, true, cOpenModeEditor);
              end;
            end
            else
            if (ValueInt>=0) and (ValueInt<=Ord(High(TATBinHexMode))) then
            begin
              if F.IsBinary then
                F.Binary.Mode:= TATBinHexMode(ValueInt)
              else
              begin
                if F.EditorsLinked and (F.FileName<>'') then
                begin
                  Str:= F.FileName;
                  F.DoFileClose;
                  F.DoFileOpen_AsBinary(Str, TATBinHexMode(ValueInt));
                end;
              end;
            end;
          end;

        PROP_V_POS:
          begin
            if not Assigned(F) then exit(ReturnNone);
            if F.IsBinary then
              F.Binary.PosOffset:= ValueInt;
          end;

        PROP_V_SEL_START:
          begin
            if not Assigned(F) then exit(ReturnNone);
            if F.IsBinary then
              F.Binary.SetSelection(
                ValueInt,
                F.Binary.SelLength,
                false);
          end;

        PROP_V_SEL_LEN:
          begin
            if not Assigned(F) then exit(ReturnNone);
            if F.IsBinary then
              F.Binary.SetSelection(
                F.Binary.SelStart,
                ValueInt,
                false);
          end;

        PROP_V_WIDTH:
          begin
            if not Assigned(F) then exit(ReturnNone);
            if F.IsBinary then
            begin
              F.Binary.TextWidth:= ValueInt;
              F.Binary.Redraw();
            end;
          end;

        PROP_V_WIDTH_HEX:
          begin
            if not Assigned(F) then exit(ReturnNone);
            if F.IsBinary then
            begin
              F.Binary.TextWidthHex:= ValueInt;
              F.Binary.Redraw();
            end;
          end;

        PROP_V_WIDTH_UHEX:
          begin
            if not Assigned(F) then exit(ReturnNone);
            if F.IsBinary then
            begin
              F.Binary.TextWidthUHex:= ValueInt;
              F.Binary.Redraw();
            end;
          end;
      end;

      Ed.Update;
      {
      if Assigned(F) then
      begin
        fmMain.UpdateFrameEx(F, false);
        fmMain.UpdateStatus;
      end;
      }

      Result:= ReturnNone;
    end
  else
    Result:= ReturnNone;
end;


function Py_CommandList: PPyObject; cdecl;
var
  Item, ItemInitial: TATKeymapItem;
  ItemPlugin: TAppCommandInfo;
  NLen, i: Integer;
  SKeyInit1, SKeyInit2: string;
begin
  //AppKeymapInitial must be inited the _same way_ as AppKeymapMain
  if not Assigned(AppKeymapInitial) then
  begin
    AppKeymapInitial:= TATKeymap.Create;
    InitKeymapFull(AppKeymapInitial);
    Keymap_AddCudatextItems(AppKeymapInitial);
  end;

  with AppPython.Engine do
  begin
    NLen:= AppKeymapMain.Count;
    if NLen>0 then
    begin
      Result:= PyList_New(NLen);
      if not Assigned(Result) then
        raise EPythonError.Create(msgPythonListError);

      for i:= 0 to NLen-1 do
      begin
        Item:= AppKeymapMain[i];
        case AppCommandCategory(Item.Command) of
          categ_Lexer:
            begin
              PyList_SetItem(Result, i,
                Py_BuildValue('{sssissssssssss}',
                  'type',
                  'lexer',
                  'cmd',
                  Item.Command,
                  'name',
                  PChar(Item.Name),
                  'key1',
                  '',
                  'key2',
                  '',
                  'key1_init',
                  '',
                  'key2_init',
                  ''
                  ));
            end;

          categ_Plugin,
          categ_PluginSub:
            begin
              ItemPlugin:= TAppCommandInfo(AppCommandList[Item.Command-cmdFirstPluginCommand]);
              PyList_SetItem(Result, i,
                Py_BuildValue('{sssisssssssssssssssOssss}',
                  'type',
                  'plugin',
                  'cmd',
                  Item.Command,
                  'name',
                  PChar(Item.Name),
                  'key1',
                  PChar(Item.Keys1.ToString),
                  'key2',
                  PChar(Item.Keys2.ToString),
                  'p_module',
                  PChar(ItemPlugin.ItemModule),
                  'p_method',
                  PChar(ItemPlugin.ItemProc),
                  'p_method_params',
                  PChar(ItemPlugin.ItemProcParam),
                  'p_lexers',
                  PChar(ItemPlugin.ItemLexers),
                  'p_from_api',
                  PyBool_FromLong(Ord(ItemPlugin.ItemFromApi)),
                  'p_caption',
                  PChar(ItemPlugin.ItemCaption),
                  'p_in_menu',
                  PChar(ItemPlugin.ItemInMenu)
                  ));
            end;

          categ_Normal:
            begin
              SKeyInit1:= '';
              SKeyInit2:= '';
              if AppKeymapInitial.IsIndexValid(i) then
              begin
                ItemInitial:= AppKeymapInitial[i];
                SKeyInit1:= ItemInitial.Keys1.ToString;
                SKeyInit2:= ItemInitial.Keys2.ToString;
              end;

              PyList_SetItem(Result, i,
                Py_BuildValue('{sssissssssssss}',
                  'type',
                  'cmd',
                  'cmd',
                  Item.Command,
                  'name',
                  PChar(Item.Name),
                  'key1',
                  PChar(Item.Keys1.ToString),
                  'key2',
                  PChar(Item.Keys2.ToString),
                  'key1_init',
                  PChar(SKeyInit1),
                  'key2_init',
                  PChar(SKeyInit2)
                  ));
            end;

          categ_OpenedFile:
            begin
              PyList_SetItem(Result, i,
                Py_BuildValue('{sssissssssssss}',
                  'type',
                  'openedfile',
                  'cmd',
                  Item.Command,
                  'name',
                  PChar(Item.Name),
                  'key1',
                  '',
                  'key2',
                  '',
                  'key1_init',
                  '',
                  'key2_init',
                  ''
                  ));
            end;

          categ_RecentFile:
            begin
              PyList_SetItem(Result, i,
                Py_BuildValue('{sssissssssssss}',
                  'type',
                  'recentfile',
                  'cmd',
                  Item.Command,
                  'name',
                  PChar(Item.Name),
                  'key1',
                  '',
                  'key2',
                  '',
                  'key1_init',
                  '',
                  'key2_init',
                  ''
                  ));
            end;
        end;
    end
    end
    else
      Result:= ReturnNone;
  end;
end;


function Py_app_proc(Self, Args: PPyObject): PPyObject; cdecl;
var
  Id, i: integer;
  Ptr: PChar;
  Str, Str1, Str2, Str3: string;
  StrCaption, StrId, StrFilename, StrCmd, StrIndex: string;
  SplitVert, SplitVisible, ok: boolean;
  SplitPos, SplitTotal: integer;
  ParamBool: boolean;
  ParamRect: TRect;
  Params: TAppVariantArray;
  Num64: Int64;
  Pnt: TPoint;
  Sep: TATStringSeparator;
begin
  with AppPython.Engine do
    if Bool(PyArg_ParseTuple(Args, 'is:app_proc', @Id, @Ptr)) then
    begin
      Str:= string(Ptr);
      ParamBool:= AppStrToBool(Str);

      case Id of
        APP_PROC_GET_CLIP:
          begin
            Str:= Clipboard.AsText;
            Result:= PyString_FromString(PChar(Str));
          end;
        APP_PROC_SET_CLIP:
          begin
            SClipboardCopy(Str);
            Result:= ReturnNone;
          end;
        APP_PROC_SET_CLIP_ALT:
          begin
            SClipboardCopy(Str, PrimarySelection); //for Linux
            Result:= ReturnNone;
          end;

        APP_PROC_GET_COMMANDS:
          begin
            Keymap_UpdateDynamic(categ_Lexer);
            Keymap_UpdateDynamic(categ_OpenedFile);
            Keymap_UpdateDynamic(categ_RecentFile);
            Result:= Py_CommandList;
          end;

        APP_PROC_SAVE_SESSION:
          begin
            ParamBool:= fmMain.DoOps_SaveSession(Str);
            Result:= PyBool_FromLong(Ord(ParamBool));
          end;

        APP_PROC_LOAD_SESSION:
          begin
            ParamBool:= fmMain.DoOps_LoadSession(Str, true);
            Result:= PyBool_FromLong(Ord(ParamBool));
          end;

        APP_PROC_SET_SESSION:
          begin
            AppSessionName:= Str;
            fmMain.UpdateCaption;
            Result:= ReturnNone;
          end;

        APP_PROC_SET_FOLDER:
          begin
            PyCurrentFolder:= Str;
            Result:= ReturnNone;
          end;

        APP_PROC_SET_EVENTS:
          begin
            Sep.Init(Str, ';');
            Sep.GetItemStr(StrId); //module
            Sep.GetItemStr(StrCmd); //events
            Sep.GetItemStr(StrCaption); //lexers
            Sep.GetItemStr(StrIndex); //keys
            AppEventsUpdate(StrId, StrCmd, StrCaption, StrIndex);
            Result:= ReturnNone;
          end;

        APP_PROC_SET_SUBCOMMANDS:
          begin
            CommandPlugins_UpdateSubcommands(Str);
            Keymap_UpdateDynamic(categ_PluginSub);
            Result:= ReturnNone;
          end;

        APP_PROC_GET_LAST_PLUGIN:
          begin
            Str:= AppPython.LastCommandModule+','+AppPython.LastCommandMethod;
            if AppPython.LastCommandParam<>'' then
              Str+= ','+AppPython.LastCommandParam;
            Result:= PyString_FromString(PChar(Str));
          end;

        APP_PROC_GET_GROUPING:
          begin
            Result:= PyInt_FromLong(Ord(fmMain.Groups.Mode));
          end;
        APP_PROC_SET_GROUPING:
          begin
            fmMain.Groups.Mode:= TATGroupsMode(StrToIntDef(Str, 1));
            fmMain.UpdateStatus;
            Result:= ReturnNone;
          end;


        APP_PROC_EXEC_PYTHON:
          begin
            AppPython.Exec(Str);
            Result:= ReturnNone;
          end;

        APP_PROC_EXEC_PLUGIN:
          begin
            Sep.Init(Str);
            Sep.GetItemStr(Str1);
            Sep.GetItemStr(Str2);
            Sep.GetItemStr(Str3);
            if Str3<>'' then
            begin
              SetLength(Params, 1);
              Params[0]:= AppVariant(Str3);
            end
            else
              SetLength(Params, 0);

            fmMain.DoPyCommand(Str1, Str2, Params);
            Result:= ReturnNone;
          end;

        APP_PROC_GET_ESCAPE:
          begin
            Result:= PyBool_FromLong(Ord(PyEscapeFlag or Application.Terminated));
          end;
        APP_PROC_SET_ESCAPE:
          begin
            PyEscapeFlag:= ParamBool;
            Result:= ReturnNone;
          end;

        APP_PROC_GET_FINDER_PROP:
          begin
            Result:= fmMain.DoFindOptions_GetDict;
          end;

        APP_PROC_SET_FINDER_PROP:
          begin
            fmMain.DoFindOptions_ApplyDict(Str);
            Result:= ReturnNone;
          end;

        APP_PROC_GET_GUI_HEIGHT:
          begin
            i:= DoControl_GetAutoHeight(Str);
            if i>0 then
              Result:= PyInt_FromLong(i)
            else
              Result:= ReturnNone;
          end;

        APP_PROC_GET_SYSTEM_PPI:
          begin
            Result:= PyInt_FromLong(Screen.PixelsPerInch);
          end;

        APP_PROC_PROGRESSBAR:
          begin
            Num64:= StrToIntDef(Str, -1);
            if Num64<0 then
              fmMain.StatusProgress.Hide
            else
            begin
              fmMain.StatusProgress.Show;
              fmMain.StatusProgress.MinValue:= 0;
              fmMain.StatusProgress.MaxValue:= 100;
              fmMain.StatusProgress.Progress:= Num64;
            end;
            Result:= ReturnNone;
          end;

        APP_PROC_GET_TAB_IMAGELIST:
          begin
            Result:= Py_ObjectHandle(fmMain.ImageListTabs);
          end;

        APP_PROC_GET_CODETREE:
          begin
            Result:= Py_ObjectHandle(fmMain.CodeTree);
          end;

        APP_PROC_WINDOW_TOPMOST_GET:
          begin
            Result:= PyBool_FromLong(Ord(fmMain.FormStyle=fsSystemStayOnTop));
          end;
        APP_PROC_WINDOW_TOPMOST_SET:
          begin
            if ParamBool then
              fmMain.FormStyle:= fsSystemStayOnTop
            else
              fmMain.FormStyle:= fsNormal;
            Result:= ReturnNone;
          end;

        //-------------------
        APP_PROC_SIDEPANEL_ADD_DIALOG:
          begin
            Sep.Init(Str);
            Sep.GetItemStr(StrCaption); //caption
            Sep.GetItemStr(StrIndex); //handle
            Sep.GetItemStr(StrFilename); //icon filename

            ok:= AppPanels[cPaneSide].Add(
              StrCaption,
              fmMain.DoSidebar_FilenameToImageIndex(StrCaption, StrFilename),
              StrToInt64Def(StrIndex, 0),
              nil
              );

            Result:= PyBool_FromLong(Ord(ok));
          end;

        APP_PROC_SIDEPANEL_REMOVE:
          begin
            StrCaption:= Str;
            ok:= AppPanels[cPaneSide].Delete(StrCaption);
            Result:= PyBool_FromLong(Ord(ok));
          end;

        APP_PROC_SIDEPANEL_ACTIVATE:
          begin
            SSplitByChar(Str, ',', Str1, Str2);
            ok:= AppPanels[cPaneSide].UpdatePanels(Str1, AppStrToBool(Str2), true);
            Result:= PyBool_FromLong(Ord(ok));
          end;

        APP_PROC_SIDEPANEL_ENUM:
          begin
            Str:= '';
            for i:= 0 to fmMain.ToolbarSideTop.ButtonCount-1 do
              Str:= Str + IfThen(i>0, #10) + fmMain.ToolbarSideTop.Buttons[i].Caption;
            Result:= PyString_FromString(PChar(Str));
          end;

        APP_PROC_SIDEPANEL_GET_CONTROL:
          begin
            Num64:= AppPanels[cPaneSide].CaptionToControlHandle(Str);
            if Num64<>0 then
              Result:= PyLong_FromLongLong(Num64)
            else
              Result:= ReturnNone;
          end;

        APP_PROC_SIDEPANEL_GET:
          begin
            Str:= AppPanels[cPaneSide].LastActivePanel;
            Result:= PyString_FromString(PChar(Str));
          end;

        //-------------------
        APP_PROC_BOTTOMPANEL_GET_CONTROL:
          begin
            Num64:= AppPanels[cPaneOut].CaptionToControlHandle(Str);
            if Num64<>0 then
              Result:= PyLong_FromLongLong(Num64)
            else
              Result:= ReturnNone;
          end;

        APP_PROC_BOTTOMPANEL_ADD_DIALOG:
          begin
            Sep.Init(Str);
            Sep.GetItemStr(StrCaption); //caption
            Sep.GetItemStr(StrIndex); //handle
            Sep.GetItemStr(StrFilename); //icon filename

            ok:= AppPanels[cPaneOut].Add(
              StrCaption,
              fmMain.DoSidebar_FilenameToImageIndex(StrCaption, StrFilename),
              StrToInt64Def(StrIndex, 0),
              nil
              );

            Result:= PyBool_FromLong(Ord(ok));
          end;

        APP_PROC_BOTTOMPANEL_REMOVE:
          begin
            StrCaption:= Str;
            ok:= AppPanels[cPaneOut].Delete(StrCaption);
            Result:= PyBool_FromLong(Ord(ok));
          end;

        APP_PROC_BOTTOMPANEL_ACTIVATE:
          begin
            SSplitByChar(Str, ',', Str1, Str2);
            ok:= AppPanels[cPaneOut].UpdatePanels(Str1, AppStrToBool(Str2), true);
            Result:= PyBool_FromLong(Ord(ok));
          end;

        APP_PROC_BOTTOMPANEL_ENUM:
          begin
            Str:= '';
            for i:= 0 to fmMain.ToolbarSideLow.ButtonCount-1 do
              Str:= Str + IfThen(i>0, #10) + fmMain.ToolbarSideLow.Buttons[i].Caption;
            Result:= PyString_FromString(PChar(Str));
          end;

        APP_PROC_BOTTOMPANEL_GET:
          begin
            Str:= AppPanels[cPaneOut].LastActivePanel;
            Result:= PyString_FromString(PChar(Str));
          end;

        //-------------------
        APP_PROC_GET_LANG:
          begin
            Result:= PyString_FromString(PChar(UiOps.LangName));
          end;

        APP_PROC_GET_HOTKEY:
          begin
            Result:= PyString_FromString(PChar(Keymap_GetHotkey(AppKeymapMain, Str)));
          end;
        APP_PROC_SET_HOTKEY:
          begin
            Result:= PyBool_FromLong(Ord(Keymap_SetHotkey(AppKeymapMain, Str, true)));
          end;

        APP_PROC_GET_KEYSTATE:
          begin
            Str:= ConvertShiftStateToString(KeyboardStateToShiftState);
            Result:= PyString_FromString(PChar(Str));
          end;

        APP_PROC_THEME_UI_GET:
          begin
            Result:= PyString_FromString(PChar(UiOps.ThemeUi));
          end;
        APP_PROC_THEME_UI_SET:
          begin
            fmMain.ThemeUi:= Str;
            Result:= ReturnNone;
          end;
        APP_PROC_THEME_SYNTAX_GET:
          begin
            Result:= PyString_FromString(PChar(UiOps.ThemeSyntax));
          end;
        APP_PROC_THEME_SYNTAX_SET:
          begin
            fmMain.ThemeSyntax:= Str;
            Result:= ReturnNone;
          end;

        APP_PROC_HOTKEY_INT_TO_STR:
          begin
            Str:= ShortCutToText(StrToIntDef(Str, 0));
            Result:= PyString_FromString(PChar(Str));
          end;
        APP_PROC_HOTKEY_STR_TO_INT:
          begin
            Str:= IntToStr(TextToShortCut(Str));
            Result:= PyString_FromString(PChar(Str));
          end;

        APP_PROC_GET_MOUSE_POS:
          begin
            Pnt:= Mouse.CursorPos;
            Result:= Py_BuildValue('(ii)', Pnt.X, Pnt.Y);
          end;

        APP_PROC_SHOW_STATUSBAR_SET:
          begin
            fmMain.ShowStatus:= ParamBool;
            Result:= ReturnNone;
          end;
        APP_PROC_SHOW_STATUSBAR_GET:
          begin
            Result:= PyBool_FromLong(Ord(fmMain.ShowStatus));
          end;

        APP_PROC_SHOW_TOOLBAR_SET:
          begin
            fmMain.ShowToolbar:= ParamBool;
            Result:= ReturnNone;
          end;
        APP_PROC_SHOW_TOOLBAR_GET:
          begin
            Result:= PyBool_FromLong(Ord(fmMain.ShowToolbar));
          end;

        APP_PROC_SHOW_SIDEPANEL_SET:
          begin
            AppPanels[cPaneSide].Visible:= ParamBool;
            Result:= ReturnNone;
          end;
        APP_PROC_SHOW_SIDEPANEL_GET:
          begin
            Result:= PyBool_FromLong(Ord(AppPanels[cPaneSide].Visible));
          end;

        APP_PROC_SHOW_BOTTOMPANEL_SET:
          begin
            AppPanels[cPaneOut].Visible:= ParamBool;
            Result:= ReturnNone;
          end;
        APP_PROC_SHOW_BOTTOMPANEL_GET:
          begin
            Result:= PyBool_FromLong(Ord(AppPanels[cPaneOut].Visible));
          end;

        APP_PROC_SHOW_TABS_SET:
          begin
            fmMain.ShowTabsMain:= ParamBool;
            Result:= ReturnNone;
          end;
        APP_PROC_SHOW_TABS_GET:
          begin
            Result:= PyBool_FromLong(Ord(fmMain.ShowTabsMain));
          end;

        APP_PROC_SHOW_SIDEBAR_SET:
          begin
            fmMain.ShowSideBar:= ParamBool;
            Result:= ReturnNone;
          end;
        APP_PROC_SHOW_SIDEBAR_GET:
          begin
            Result:= PyBool_FromLong(Ord(fmMain.ShowSideBar));
          end;

        APP_PROC_SHOW_TREEFILTER_SET:
          begin
            fmMain.PanelCodeTreeTop.Visible:= ParamBool;
            Result:= ReturnNone;
          end;
        APP_PROC_SHOW_TREEFILTER_GET:
          begin
            Result:= PyBool_FromLong(Ord(fmMain.PanelCodeTreeTop.Visible));
          end;

        APP_PROC_SHOW_FLOATGROUP1_SET:
          begin
            fmMain.ShowFloatGroup1:= ParamBool;
            Result:= ReturnNone;
          end;
        APP_PROC_SHOW_FLOATGROUP1_GET:
          begin
            Result:= PyBool_FromLong(Ord(fmMain.ShowFloatGroup1));
          end;

        APP_PROC_SHOW_FLOATGROUP2_SET:
          begin
            fmMain.ShowFloatGroup2:= ParamBool;
            Result:= ReturnNone;
          end;
        APP_PROC_SHOW_FLOATGROUP2_GET:
          begin
            Result:= PyBool_FromLong(Ord(fmMain.ShowFloatGroup2));
          end;

        APP_PROC_SHOW_FLOATGROUP3_SET:
          begin
            fmMain.ShowFloatGroup3:= ParamBool;
            Result:= ReturnNone;
          end;
        APP_PROC_SHOW_FLOATGROUP3_GET:
          begin
            Result:= PyBool_FromLong(Ord(fmMain.ShowFloatGroup3));
          end;

        APP_PROC_FLOAT_SIDE_SET:
          begin
            AppPanels[cPaneSide].Floating:= ParamBool;
            Result:= ReturnNone;
          end;
        APP_PROC_FLOAT_SIDE_GET:
          begin
            Result:= PyBool_FromLong(Ord(AppPanels[cPaneSide].Floating));
          end;

        APP_PROC_FLOAT_BOTTOM_SET:
          begin
            AppPanels[cPaneOut].Floating:= ParamBool;
            Result:= ReturnNone;
          end;
        APP_PROC_FLOAT_BOTTOM_GET:
          begin
            Result:= PyBool_FromLong(Ord(AppPanels[cPaneOut].Floating));
          end;

        APP_PROC_COORD_WINDOW_GET:
          begin
            Result:= Py_rect(Application.MainForm.BoundsRect);
          end;
        APP_PROC_COORD_WINDOW_SET:
          begin
            Sep.Init(Str);
            Sep.GetItemInt(ParamRect.Left, 0);
            Sep.GetItemInt(ParamRect.Top, 0);
            Sep.GetItemInt(ParamRect.Right, 0);
            Sep.GetItemInt(ParamRect.Bottom, 0);
            Application.MainForm.BoundsRect:= ParamRect;
            Result:= ReturnNone;
          end;
        APP_PROC_COORD_DESKTOP:
          Result:= Py_rect(Screen.DesktopRect);
        APP_PROC_COORD_MONITOR:
          Result:= Py_rect(fmMain.Monitor.BoundsRect);
        APP_PROC_COORD_MONITOR0:
          Result:= Py_rect_monitor(0);
        APP_PROC_COORD_MONITOR1:
          Result:= Py_rect_monitor(1);
        APP_PROC_COORD_MONITOR2:
          Result:= Py_rect_monitor(2);
        APP_PROC_COORD_MONITOR3:
          Result:= Py_rect_monitor(3);

        APP_PROC_CONFIG_READ:
          begin
            fmMain.DoOps_LoadOptionsFromString(Str);
            Result:= ReturnNone;
          end;

        APP_PROC_ENUM_FONTS:
          Result:= StringsToPyList(Screen.Fonts);

        APP_PROC_SEND_MESSAGE:
          Result:= PyInt_FromLong(AppSendMessageFromString(Str));

        APP_PROC_SPLITTER_GET:
          begin
            fmMain.DoSplitter_GetInfo(
              StrToIntDef(Str, -1),
              SplitVert,
              SplitVisible,
              SplitPos,
              SplitTotal);
            Result:= Py_BuildValue('(OOii)',
              PyBool_FromLong(Ord(SplitVert)),
              PyBool_FromLong(Ord(SplitVisible)),
              SplitPos,
              SplitTotal
              );
          end;

        APP_PROC_SPLITTER_SET:
          begin
            SSplitByChar(Str, ',', Str1, Str2);
            fmMain.DoSplitter_SetInfo(
              StrToIntDef(Str1, -1),
              StrToIntDef(Str2, -1)
              );
            Result:= ReturnNone;
          end;

        APP_PROC_THEME_UI_DICT_GET:
          begin
            Result:= Py_GetThemeDict_UI;
          end;

        APP_PROC_THEME_SYNTAX_DICT_GET:
          begin
            Result:= Py_GetThemeDict_Syntax;
          end;

        APP_PROC_CONFIG_NEWDOC_EOL_GET:
          begin
            Result:= PyInt_FromLong(Ord(UiOps.NewdocEnds));
          end;
        APP_PROC_CONFIG_NEWDOC_EOL_SET:
          begin
            UiOps.NewdocEnds:= StrToIntDef(Str, 0);
            Result:= ReturnNone;
          end;

        APP_PROC_CONFIG_NEWDOC_ENC_GET:
          begin
            Result:= PyString_FromString(PChar(UiOps.NewdocEnc));
          end;

        APP_PROC_CONFIG_NEWDOC_ENC_SET:
          begin
            UiOps.NewdocEnc:= Str;
            Result:= ReturnNone;
          end;

        APP_PROC_CONFIG_SCALE_GET:
          begin
            Result:= Py_BuildValue('(ii)', UiOps.Scale, UiOps.ScaleFont);
          end;

        APP_PROC_CONFIG_SCALE_SET:
          begin
            Sep.Init(Str);
            Sep.GetItemInt(UiOps.Scale, 100, 80, 500);
            Sep.GetItemInt(UiOps.ScaleFont, 100, 80, 500);
            fmMain.DoApplyUiOps;
          end;

        else
          Result:= ReturnNone;
      end;
    end
  else
    Result:= ReturnNone;
end;


function Py_GetLexerProps(An: TecSyntAnalyzer): PPyObject;
var
  Str1, Str2: string;
  List: TStringList;
  ObjTypes,
  ObjTokenKinds,
  ObjSublex,
  ObjCommentStr,
  ObjCommentLined,
  ObjStyles,
  ObjStylesCmt,
  ObjStylesStr: PPyObject;
begin
  with AppPython.Engine do
  begin
    Str1:= An.CommentRangeBegin;
    Str2:= An.CommentRangeEnd;
    if (Str1<>'') and (Str2<>'') then
      ObjCommentStr:= Py_BuildValue('(ss)', PChar(Str1), PChar(Str2))
    else
      ObjCommentStr:= ReturnNone;

    Str1:= An.CommentFullLinesBegin;
    Str2:= An.CommentFullLinesEnd;
    if (Str1<>'') and (Str2<>'') then
      ObjCommentLined:= Py_BuildValue('(ss)', PChar(Str1), PChar(Str2))
    else
      ObjCommentLined:= ReturnNone;

    List:= TStringList.Create;
    try
      List.LineBreak:= ' ';
      List.Text:= An.Extentions;
      ObjTypes:= StringsToPyList(List);

      List.LineBreak:= ',';
      List.Text:= An.StylesOfComments;
      ObjStylesCmt:= StringsToPyList(List);

      List.Text:= An.StylesOfStrings;
      ObjStylesStr:= StringsToPyList(List);

      List.LineBreak:= #10;

      ObjTokenKinds:= StringsToPyList(An.TokenTypeNames);

      LexerEnumSublexers(An, List);
      ObjSublex:= StringsToPyList(List);

      LexerEnumStyles(An, List);
      ObjStyles:= StringsToPyList(List);
    finally
      FreeAndNil(List);
    end;

    Result:= Py_BuildValue('{sOsOsssOsOsOsOsOsOsO}',
      'typ',
      ObjTypes,
      'en',
      PyBool_FromLong(Ord(not An.Internal)),
      'c_line',
      PChar(string(An.LineComment)),
      'c_str',
      ObjCommentStr,
      'c_lined',
      ObjCommentLined,
      'st',
      ObjStyles,
      'st_c',
      ObjStylesCmt,
      'st_s',
      ObjStylesStr,
      'kinds',
      ObjTokenKinds,
      'sub',
      ObjSublex
      );
  end;
end;

function Py_GetLiteLexerProps(An: TATLiteLexer): PPyObject;
var
  Str1, Str2: string;
  List: TStringList;
  ObjTypes,
  ObjTokenKinds,
  ObjSublex,
  ObjCommentStr,
  ObjCommentLined,
  ObjStyles,
  ObjStylesCmt,
  ObjStylesStr: PPyObject;
begin
  with AppPython.Engine do
  begin
    Str1:= An.CommentBlockBegin;
    Str2:= An.CommentBlockEnd;
    if (Str1<>'') and (Str2<>'') then
      ObjCommentStr:= Py_BuildValue('(ss)', PChar(Str1), PChar(Str2))
    else
      ObjCommentStr:= ReturnNone;

    ObjCommentLined:= ReturnNone;

    List:= TStringList.Create;
    try
      List.LineBreak:= ';';
      List.Text:= An.FileTypes;
      ObjTypes:= StringsToPyList(List);

      ObjTokenKinds:= ReturnNone;
      ObjStylesCmt:= ReturnNone;
      ObjStylesStr:= ReturnNone;
      ObjSublex:= ReturnNone;
      ObjStyles:= ReturnNone;
    finally
      FreeAndNil(List);
    end;

    Result:= Py_BuildValue('{sOsOsssOsOsOsOsOsOsO}',
      'typ',
      ObjTypes,
      'en',
      PyBool_FromLong(1),
      'c_line',
      PChar(An.CommentLine),
      'c_str',
      ObjCommentStr,
      'c_lined',
      ObjCommentLined,
      'st',
      ObjStyles,
      'st_c',
      ObjStylesCmt,
      'st_s',
      ObjStylesStr,
      'kinds',
      ObjTokenKinds,
      'sub',
      ObjSublex
      );
  end;
end;


function Py_GetLexerStylesProps(An: TecSyntAnalyzer): PPyObject;
var
  Fmt: TecSyntaxFormat;
  i: integer;
begin
  with AppPython.Engine do
  begin
    Result:= PyDict_New();
    for i:= 0 to An.Formats.Count-1 do
    begin
      Fmt:= An.Formats[i];
      PyDict_SetItemString(Result,
        PChar(Fmt.DisplayName),
        Py_BuildValue('{sisisisisisisisissss}',
          'type',
          Ord(Fmt.FormatType),
          'color_font',
          Fmt.Font.Color,
          'color_back',
          Fmt.BgColor,
          'color_border',
          Fmt.BorderColorBottom,
          'border_left',
          Ord(Fmt.BorderTypeLeft),
          'border_right',
          Ord(Fmt.BorderTypeRight),
          'border_top',
          Ord(Fmt.BorderTypeTop),
          'border_bottom',
          Ord(Fmt.BorderTypeBottom),
          'tkind',
          PChar(cPyTokenKind[TATTokenKind(Fmt.TokenKind)]),
          'styles',
          PChar(FontStylesToString(Fmt.Font.Style))
          ));
    end;
  end;
end;

function Py_lexer_proc(Self, Args : PPyObject): PPyObject; cdecl;
var
  Id: integer;
  PtrValue: PChar;
  StrValue, SName: string;
  List: TStringList;
  Lexer: TecSyntAnalyzer;
  LexerLite: TATLiteLexer;
  Sep: TATStringSeparator;
  PropName, PropTypes, PropCmtLine, PropCmtBlock1, PropCmtBlock2: string;
  ok: boolean;
begin
  with AppPython.Engine do
    if Bool(PyArg_ParseTuple(Args, 'is:lexer_proc', @Id, @PtrValue)) then
    begin
      StrValue:= string(PtrValue);

      case Id of
        LEXER_GET_PROP:
          begin
            if SEndsWith(StrValue, msgLiteLexerSuffix) then
            begin
              SetLength(StrValue, Length(StrValue)-Length(msgLiteLexerSuffix));
              LexerLite:= AppManagerLite.FindLexerByName(StrValue);
              if Assigned(LexerLite) then
                Result:= Py_GetLiteLexerProps(LexerLite)
              else
                Result:= ReturnNone;
            end
            else
            begin
              Lexer:= AppManager.FindLexerByName(StrValue);
              if Assigned(Lexer) then
                Result:= Py_GetLexerProps(Lexer)
              else
                Result:= ReturnNone;
            end;
          end;

        LEXER_GET_STYLES:
          begin
            if SEndsWith(StrValue, msgLiteLexerSuffix) then
              Result:= ReturnNone
            else
            begin
              Lexer:= AppManager.FindLexerByName(StrValue);
              if Assigned(Lexer) then
                Result:= Py_GetLexerStylesProps(Lexer)
              else
                Result:= ReturnNone;
            end;
          end;

        LEXER_DETECT:
          begin
            if Assigned(fmMain.LexersDetected) then
              fmMain.LexersDetected.Clear
            else
              fmMain.LexersDetected:= TStringList.Create;

            DoLexerDetect(StrValue, Lexer, LexerLite, SName, @fmMain.DoPyLexerDetection);

            if fmMain.LexersDetected.Count>0 then
              Result:= StringsToPyTuple(fmMain.LexersDetected)
            else
            if SName<>'' then
              Result:= PyString_FromString(PChar(SName))
            else
              Result:= ReturnNone;
          end;

        LEXER_GET_LEXERS:
          begin
            List:= TStringList.Create;
            List.Sorted:= true;
            try
              DoLexerEnum(List, AppStrToBool(StrValue));
              Result:= StringsToPyList(List);
            finally
              FreeAndNil(List);
            end;
          end;

        LEXER_REREAD_LIB:
          begin
            fmMain.DoOps_LoadLexerLib(false);
            Result:= ReturnNone;
          end;

        LEXER_ADD_VIRTUAL:
          begin
            Sep.Init(StrValue, ',');
            Sep.GetItemStr(PropName);
            Sep.GetItemStr(PropTypes);
            Sep.GetItemStr(PropCmtLine);
            Sep.GetItemStr(PropCmtBlock1);
            Sep.GetItemStr(PropCmtBlock2);
            ok:= AppManagerLite.Add(
              PropName,
              PropTypes,
              PropCmtLine,
              PropCmtBlock1,
              PropCmtBlock2
              );
            Result:= PyBool_FromLong(Ord(ok));;
          end;

        else
          Result:= ReturnNone;
      end;
    end;
end;

function Py_ed_convert(Self, Args: PPyObject): PPyObject; cdecl;
var
  H: Int64;
  Ed: TATSynEdit;
  Strs: TATStrings;
  Id, X, Y: integer;
  PtrValue: PChar;
  StrValue: string;
  StrTemp: UnicodeString;
  Pnt: TPoint;
  Details: TATPosDetails;
begin
  with AppPython.Engine do
    if Bool(PyArg_ParseTuple(Args, 'Liiis:convert', @H, @Id, @X, @Y, @PtrValue)) then
    begin
      Ed:= PyEditor(H);
      Strs:= Ed.Strings;
      StrValue:= string(PtrValue);

      case Id of
        CONVERT_CHAR_TO_COL:
          begin
            if (X<0) or (not Strs.IsIndexValid(Y)) then
              exit(ReturnNone);

            X:= Strs.CharPosToColumnPos(Y, X, Ed.TabHelper);
            Result:= Py_BuildValue('(ii)', X, Y);
          end;

        CONVERT_COL_TO_CHAR:
          begin
            if (X<0) or (not Strs.IsIndexValid(Y)) then
              exit(ReturnNone);

            X:= Strs.ColumnPosToCharPos(Y, X, Ed.TabHelper);
            Result:= Py_BuildValue('(ii)', X, Y);
          end;

        CONVERT_LINE_TABS_TO_SPACES:
          begin
            StrTemp:= Ed.TabHelper.TabsToSpaces(-1, UTF8Decode(StrValue));
            Result:= PyUnicode_FromWideString(StrTemp);
          end;

        CONVERT_SCREEN_TO_LOCAL:
          begin
            Pnt:= Ed.ScreenToClient(Point(X, Y));
            Result:= Py_BuildValue('(ii)', Pnt.X, Pnt.Y);
          end;

        CONVERT_LOCAL_TO_SCREEN:
          begin
            Pnt:= Ed.ClientToScreen(Point(X, Y));
            Result:= Py_BuildValue('(ii)', Pnt.X, Pnt.Y);
          end;

        CONVERT_PIXELS_TO_CARET:
          begin
            Pnt:= Ed.ClientPosToCaretPos(Point(X, Y), Details);
            if Pnt.Y<0 then
              exit(ReturnNone);

            Result:= Py_BuildValue('(ii)', Pnt.X, Pnt.Y);
          end;

        CONVERT_CARET_TO_PIXELS:
          begin
            if (X<0) or (not Strs.IsIndexValid(Y)) then
              exit(ReturnNone);

            Pnt:= Ed.CaretPosToClientPos(Point(X, Y));
            if Pnt.Y<0 then
              exit(ReturnNone);

            Result:= Py_BuildValue('(ii)', Pnt.X, Pnt.Y);
          end;

        else
          Result:= ReturnNone;
      end;
    end
  else
    Result:= ReturnNone;
end;

procedure Py_ed_folding_SetItem(AObj: PPyObject; AListIndex: integer; AItem: PATSynRange);
begin
  with AppPython.Engine do
    PyList_SetItem(AObj, AListIndex,
      Py_BuildValue('(iiiOO)',
        AItem^.Y,
        AItem^.Y2,
        AItem^.X-1,
        PyBool_FromLong(Ord(AItem^.Staple)),
        PyBool_FromLong(Ord(AItem^.Folded))
        ));
end;

function Py_ed_folding_GetAll(Ed: TATSynEdit): PPyObject; cdecl;
var
  Item: PATSynRange;
  NLen, i: integer;
begin
  with AppPython.Engine do
  begin
    NLen:= Ed.Fold.Count;
    if NLen>0 then
    begin
      Result:= PyList_New(NLen);
      if not Assigned(Result) then
        raise EPythonError.Create(msgPythonListError);
      for i:= 0 to NLen-1 do
      begin
        Item:= Ed.Fold.ItemPtr(i);
        Py_ed_folding_SetItem(Result, i, Item);
      end;
    end
    else
      Result:= ReturnNone;
  end;
end;

function Py_ed_folding_GetFiltered(Ed: TATSynEdit; NeedY, NeedY2: integer): PPyObject; cdecl;
var
  L: TFPList;
  Item: PATSynRange;
  NLen, i: integer;
begin
  if Ed.Fold.Count=0 then
    exit(AppPython.Engine.ReturnNone);

  L:= TFPList.Create;
  try
    for i:= 0 to Ed.Fold.Count-1 do
    begin
      Item:= Ed.Fold.ItemPtr(i);
      if (Item^.Y<=NeedY) and (Item^.Y2>=NeedY2) then
        L.Add(Item);
    end;

    with AppPython.Engine do
    begin
      NLen:= L.Count;
      if NLen>0 then
      begin
        Result:= PyList_New(NLen);
        if not Assigned(Result) then
          raise EPythonError.Create(msgPythonListError);
        for i:= 0 to NLen-1 do
        begin
          Item:= PATSynRange(L[i]);
          Py_ed_folding_SetItem(Result, i, Item);
        end;
      end
      else
        Result:= ReturnNone;
    end;
  finally
    FreeAndNil(L);
  end;
end;


function Py_ed_get_sublexer_ranges(Self, Args: PPyObject): PPyObject; cdecl;
var
  H: Int64;
  Ed: TATSynEdit;
  NLen, i: Integer;
  PntStart, PntEnd: TPoint;
  LexName, Str: string;
  Adapter: TATAdapterEControl;
begin
  with AppPython.Engine do
    if Bool(PyArg_ParseTuple(Args, 'L:get_sublexer_ranges', @H)) then
    begin
      Ed:= PyEditor(H);

      if not (Ed.AdapterForHilite is TATAdapterEControl) then
        exit(ReturnNone);
      Adapter:= (Ed.AdapterForHilite as TATAdapterEControl);

      NLen:= Adapter.SublexerRangeCount;
      if NLen>0 then
      begin
        Str:= '';
        for i:= 0 to NLen-1 do
          if Adapter.SublexerRangeProps(i, PntStart, PntEnd, LexName) then
            Str:= Str + Format('%d,%d,%d,%d,%s;',
              [ PntStart.X, PntStart.Y,
                PntEnd.X, PntEnd.Y,
                LexName ]);
        Result:= PyString_FromString(PChar(Str));
      end
      else
        Result:= ReturnNone;
    end
  else
    Result:= ReturnNone;
end;


//menu_proc(id_menu, id_action, command, caption, index, hotkey, tag)
function Py_menu_proc(Self, Args: PPyObject): PPyObject; cdecl;
var
  PtrMenu, PtrCaption, PtrCmd, PtrHotkey, PtrTag: PChar;
  StrMenu, StrCaption, StrCmd, StrHotkey, StrTag, StrResult: string;
  NAction, NIndex: integer;
  Popup: TPopupMenu;
  PopupItem: TMenuItem;
  NX, NY: integer;
  ImgList: TImageList;
  Sep: TATStringSeparator;
begin
  with AppPython.Engine do
    if Bool(PyArg_ParseTuple(Args, 'sississ:menu_proc',
       @PtrMenu, @NAction, @PtrCmd, @PtrCaption, @NIndex, @PtrHotkey, @PtrTag)) then
    begin
      StrMenu:= string(PtrMenu);
      StrCaption:= string(PtrCaption);
      StrCmd:= string(PtrCmd);
      StrHotkey:= string(PtrHotkey);
      StrTag:= string(PtrTag);

      case NAction of
        MENU_CLEAR:
          begin
            fmMain.DoMenuClear(StrMenu);
            Result:= ReturnNone;
          end;

        MENU_ENUM:
          begin
            Result:= fmMain.DoMenu_PyEnum(StrMenu);
          end;

        MENU_ADD:
          begin
            StrResult:= fmMain.DoMenuAdd_Params(StrMenu, StrCmd, StrCaption, StrHotkey, StrTag, NIndex);
            Result:= PyString_FromString(PChar(StrResult));
          end;

        MENU_REMOVE:
          begin
            fmMain.DoMenu_Remove(StrMenu);
            Result:= ReturnNone;
          end;

        MENU_CREATE:
          begin
            Popup:= TPopupMenu.Create(fmMain);
            StrResult:= IntToStr(PtrInt(Popup.Items));
            Result:= PyString_FromString(PChar(StrResult));
          end;

        MENU_SET_CAPTION:
          begin
            PopupItem:= Py_MenuItemFromId(StrMenu);
            if Assigned(PopupItem) then
              PopupItem.Caption:= StrCmd;
            Result:= ReturnNone;
          end;

        MENU_SET_VISIBLE:
          begin
            PopupItem:= Py_MenuItemFromId(StrMenu);
            if Assigned(PopupItem) then
              PopupItem.Visible:= AppStrToBool(StrCmd);
            Result:= ReturnNone;
          end;

        MENU_SET_ENABLED:
          begin
            PopupItem:= Py_MenuItemFromId(StrMenu);
            if Assigned(PopupItem) then
              PopupItem.Enabled:= AppStrToBool(StrCmd);
            Result:= ReturnNone;
          end;

        MENU_SET_CHECKED:
          begin
            PopupItem:= Py_MenuItemFromId(StrMenu);
            if Assigned(PopupItem) then
              PopupItem.Checked:= AppStrToBool(StrCmd);
            Result:= ReturnNone;
          end;

        MENU_SET_RADIOITEM:
          begin
            PopupItem:= Py_MenuItemFromId(StrMenu);
            if Assigned(PopupItem) then
              PopupItem.RadioItem:= AppStrToBool(StrCmd);
            Result:= ReturnNone;
          end;

        MENU_SET_HOTKEY:
          begin
            PopupItem:= Py_MenuItemFromId(StrMenu);
            if Assigned(PopupItem) then
              PopupItem.ShortCut:= TextToShortCut(StrCmd);
            Result:= ReturnNone;
          end;

        MENU_SHOW:
          begin
            PopupItem:= Py_MenuItemFromId(StrMenu);
            if not Assigned(PopupItem) then
              exit(ReturnNone);
            Popup:= PopupItem.Owner as TPopupMenu;
            if StrCmd='' then
              Popup.PopUp //at cursor position
            else
            begin
              Sep.Init(StrCmd);
              Sep.GetItemInt(NX, 0);
              Sep.GetItemInt(NY, 0);
              Popup.PopUp(NX, NY);
            end;
            Result:= ReturnNone;
          end;

        MENU_GET_PROP:
          begin
            PopupItem:= Py_MenuItemFromId(StrMenu);
            if Assigned(PopupItem) then
              Result:= fmMain.DoMenu_GetPyProps(PopupItem)
            else
              Result:= ReturnNone;
          end;

        MENU_SET_IMAGELIST:
          begin
            ImgList:= Py_ImagelistFromId(StrCmd);
            PopupItem:= Py_MenuItemFromId(StrMenu);
            if PopupItem=nil then
              exit(ReturnNone);

            //TPopupMenu - then menuitem.owner is set to popupmenu by Cud,
            //TMainMenu - then menuitem.owner is fmMain
            if PopupItem.Owner is TMenu then
            begin
              (PopupItem.Owner as TMenu).Images:= ImgList;
            end
            else
            if PopupItem.Owner is TForm then
            begin
              (PopupItem.Owner as TForm).Menu.Images:= ImgList;
            end;

            Result:= ReturnNone;
          end;

        MENU_SET_IMAGEINDEX:
          begin
            PopupItem:= Py_MenuItemFromId(StrMenu);
            if Assigned(PopupItem) then
              PopupItem.ImageIndex:= NIndex;
            Result:= ReturnNone;
          end;

        else
          Result:= ReturnNone;
      end;
    end
  else
    Result:= ReturnNone;
end;


procedure ListboxItem_SetFromString(AItem: TATListboxItemProp; const AText: string);
var
  Sep: TATStringSeparator;
  SItem, SKey, SValue: string;
begin
  //text is '{key1:value1;key2:value2}' from to_str()
  Sep.Init(SDeleteCurlyBrackets(AText), #1);
  repeat
    if not Sep.GetItemStr(SItem) then Break;
    SSplitByChar(SItem, ':', SKey, SValue);
    SValue:= StringReplace(SValue, #2, ',', [rfReplaceAll]);

    if SKey='datatext' then
      AItem.DataText:= SValue
    else
    if SKey='tag' then
      AItem.Tag:= StrToInt64Def(SValue, 0)
    else
    if SKey='modified' then
      AItem.Modified:= SValue='1';
  until false;
end;


//listbox_proc(id_listbox, id_action, index, text)
function Py_listbox_proc(Self, Args: PPyObject): PPyObject; cdecl;
var
  IdList, NTag: Int64;
  IdAction, NIndex, i: integer;
  PtrText, PtrTag: PChar;
  StrText, StrTag: string;
  Listbox: TATListbox;
  IndexOk: boolean;
  Obj: TObject;
begin
  with AppPython.Engine do
    if Bool(PyArg_ParseTuple(Args, 'Liiss:listbox_proc', @IdList, @IdAction, @NIndex, @PtrText, @PtrTag)) then
    begin
      if IdList=0 then exit(ReturnNone);
      Listbox:= TATListbox(PtrInt(IdList));
      StrText:= string(PtrText);
      StrTag:= string(PtrTag);
      NTag:= StrToInt64Def(StrTag, 0);
      IndexOk:= Listbox.IsIndexValid(NIndex);

      case IdAction of
        LISTBOX_GET_COUNT:
          begin
            Result:= PyInt_FromLong(Listbox.Items.Count);
          end;

        LISTBOX_ADD:
          begin
            if NIndex=-1 then
              Listbox.Items.AddObject(StrText, TATListboxItemProp.Create(NTag, false, ''))
            else
            if IndexOk then
              Listbox.Items.InsertObject(NIndex, StrText, TATListboxItemProp.Create(NTag, false, ''));
            Listbox.Invalidate;
            Result:= PyInt_FromLong(Listbox.Items.Count);
          end;

        LISTBOX_DELETE:
          begin
            if IndexOk then
            begin
              Obj:= Listbox.Items.Objects[NIndex];
              if Assigned(Obj) then
                Obj.Free;
              Listbox.Items.Delete(NIndex);
              if Listbox.ItemIndex>=Listbox.ItemCount then
                Listbox.ItemIndex:= Listbox.ItemCount-1;
              Listbox.Invalidate;
            end;
            Result:= ReturnNone;
          end;

        LISTBOX_DELETE_ALL:
          begin
            for i:= Listbox.Items.Count-1 downto 0 do
            begin
              Obj:= Listbox.Items.Objects[i];
              if Assigned(Obj) then
                Obj.Free;
            end;
            Listbox.Items.Clear;
            Listbox.ItemIndex:= -1;
            Listbox.ItemTop:= 0;
            Listbox.Invalidate;
            Result:= ReturnNone;
          end;

        LISTBOX_GET_ITEM:
          begin
            if IndexOk then
            begin
              Obj:= Listbox.Items.Objects[NIndex];
              if Assigned(Obj) then
                Result:= Py_BuildValue('(sL)',
                  PChar(Listbox.Items[NIndex]),
                  PtrInt((Obj as TATListboxItemProp).Tag) )
              else
                Result:= ReturnNone;
            end
            else
              Result:= ReturnNone;
          end;

        LISTBOX_SET_ITEM:
          begin
            if IndexOk then
            begin
              Listbox.Items[NIndex]:= StrText;
              Obj:= Listbox.Items.Objects[NIndex];
              if Assigned(Obj) then
                (Obj as TATListboxItemProp).Tag:= NTag;
              Listbox.Invalidate;
            end;
            Result:= ReturnNone;
          end;

        LISTBOX_GET_SEL:
          begin
            Result:= PyInt_FromLong(Listbox.ItemIndex);
          end;

        LISTBOX_SET_SEL:
          begin
            if IndexOk then
            begin
              Listbox.ItemIndex:= NIndex;
            end;
            Result:= ReturnNone;
          end;

        LISTBOX_GET_ITEM_PROP:
          begin
            if IndexOk then
            begin
              Obj:= Listbox.Items.Objects[NIndex];
              if Assigned(Obj) then
                Result:= Py_BuildValue('{sssOsOss}',
                  'text',
                  PChar(Listbox.Items[NIndex]),
                  'tag',
                  PyLong_FromLongLong(TATListboxItemProp(Obj).Tag),
                  'modified',
                  PyBool_FromLong(Ord(TATListboxItemProp(Obj).Modified)),
                  'datatext',
                  PChar(TATListboxItemProp(Obj).DataText)
                  )
              else
                Result:= ReturnNone;
            end
            else
              Result:= ReturnNone;
          end;

        LISTBOX_SET_ITEM_PROP:
          begin
            if IndexOk then
            begin
              Listbox.Items[NIndex]:= StrText;
              Obj:= Listbox.Items.Objects[NIndex];
              ListboxItem_SetFromString(Obj as TATListboxItemProp, StrTag);
              Listbox.Invalidate;
            end;
            Result:= ReturnNone;
          end;

        LISTBOX_ADD_PROP:
          begin
            if (NIndex=-1) or IndexOk then
            begin
              Obj:= TATListboxItemProp.Create(0, false, '');
              ListboxItem_SetFromString(Obj as TATListboxItemProp, StrTag);
              if NIndex=-1 then
                Listbox.Items.AddObject(StrText, Obj)
              else
              if IndexOk then
                Listbox.Items.InsertObject(NIndex, StrText, Obj);
              Listbox.Invalidate;
            end;
            Result:= PyInt_FromLong(Listbox.Items.Count);
          end;

        LISTBOX_GET_TOP:
          begin
            Result:= PyInt_FromLong(Listbox.ItemTop);
          end;

        LISTBOX_SET_TOP:
          begin
            if IndexOk then
            begin
              Listbox.ItemTop:= NIndex;
              Listbox.Invalidate;
            end;
            Result:= ReturnNone;
          end;

        LISTBOX_GET_ITEM_H:
          begin
            Result:= PyInt_FromLong(Listbox.ItemHeight);
          end;

        LISTBOX_SET_ITEM_H:
          begin
            Listbox.ItemHeight:= NIndex;
            Listbox.Invalidate;
            Result:= ReturnNone;
          end;

        LISTBOX_GET_HOTTRACK:
          begin
            Result:= PyBool_FromLong(Ord(Listbox.HotTrack));
          end;

        LISTBOX_SET_HOTTRACK:
          begin
            Listbox.HotTrack:= (NIndex=1);
            Listbox.Invalidate;
            Result:= ReturnNone;
          end;

        LISTBOX_GET_DRAWN:
          begin
            Result:= PyBool_FromLong(Ord(Listbox.OwnerDrawn));
          end;

        LISTBOX_SET_DRAWN:
          begin
            Listbox.OwnerDrawn:= (NIndex=1);
            Listbox.Invalidate;
            Result:= ReturnNone;
          end;

        LISTBOX_GET_COLUMN_SEP:
          begin
            Result:= PyString_FromString(PChar(string(Listbox.ColumnSeparator)));
          end;

        LISTBOX_SET_COLUMN_SEP:
          begin
            if StrText<>'' then
            begin
              Listbox.ColumnSeparator:= StrText[1];
              Listbox.Invalidate;
            end;
            Result:= ReturnNone;
          end;

        LISTBOX_GET_COLUMNS:
          begin
            Result:= PyList_New(Length(Listbox.ColumnSizes));
            if not Assigned(Result) then
              raise EPythonError.Create(msgPythonListError);
            for i:= 0 to Length(Listbox.ColumnSizes)-1 do
              PyList_SetItem(Result, i, PyLong_FromLongLong(Int64(Listbox.ColumnSizes[i])));
          end;

        LISTBOX_SET_COLUMNS:
          begin
            Listbox.ColumnSizes:= StringToIntArray(StrText);
            Listbox.Invalidate;
            Result:= ReturnNone;
          end;

        LISTBOX_GET_SHOW_X:
          begin
            Result:= PyInt_FromLong(Ord(Listbox.ShowXMark));
          end;

        LISTBOX_SET_SHOW_X:
          begin
            Listbox.ShowXMark:= TATListboxShowX(NIndex);
            Listbox.Invalidate;
            Result:= ReturnNone;
          end;

        LISTBOX_GET_SCROLLSTYLE_HORZ:
          begin
            Result:= PyBool_FromLong(Ord(Listbox.ScrollStyleHorz));
          end;

        LISTBOX_GET_SCROLLSTYLE_VERT:
          begin
            Result:= PyBool_FromLong(Ord(Listbox.ScrollStyleVert));
          end;

        LISTBOX_SET_SCROLLSTYLE_HORZ:
          begin
            Listbox.ScrollStyleHorz:= TATListboxScrollStyle(NIndex);
            Listbox.Invalidate;
            Result:= ReturnNone;
          end;

        LISTBOX_SET_SCROLLSTYLE_VERT:
          begin
            Listbox.ScrollStyleVert:= TATListboxScrollStyle(NIndex);
            Listbox.Invalidate;
            Result:= ReturnNone;
          end;

        LISTBOX_GET_SCROLLPOS_HORZ:
          begin
            Result:= PyInt_FromLong(Listbox.ScrollHorz);
          end;

        LISTBOX_SET_SCROLLPOS_HORZ:
          begin
            Listbox.ScrollHorz:= NIndex;
            Result:= ReturnNone;
          end;

        else
          Result:= ReturnNone;
      end;
    end
  else
    Result:= ReturnNone;
end;


function Py_button_proc(Self, Args: PPyObject): PPyObject; cdecl;
var
  IdBtn: Int64;
  IdAction: integer;
  PtrValue: PChar;
  StrValue: string;
  Btn: TATButton;
  Mnu: TMenu;
begin
  with AppPython.Engine do
    if Bool(PyArg_ParseTuple(Args, 'Lis:button_proc', @IdBtn, @IdAction, @PtrValue)) then
    begin
      StrValue:= string(PtrValue);
      Btn:= TATButton(PtrInt(IdBtn));

      case IdAction of
        BTN_UPDATE:
          begin
            Btn.Invalidate;
            Result:= ReturnNone;
          end;

        BTN_GET_CHECKED:
          begin
            Result:= PyBool_FromLong(Ord(Btn.Checked));
          end;

        BTN_SET_CHECKED:
          begin
            Btn.Checked:= AppStrToBool(StrValue);
            Result:= ReturnNone;
          end;

        BTN_GET_IMAGELIST:
          begin
            Result:= Py_ObjectHandle(Btn.Images);
          end;

        BTN_SET_IMAGELIST:
          begin
            Btn.Images:= Py_ImagelistFromId(StrValue);
            Result:= ReturnNone;
          end;

        BTN_GET_IMAGEINDEX:
          begin
            Result:= PyLong_FromLongLong(Btn.ImageIndex);
          end;

        BTN_SET_IMAGEINDEX:
          begin
            Btn.ImageIndex:= StrToIntDef(StrValue, -1);
            Result:= ReturnNone;
          end;

        BTN_GET_KIND:
          begin
            Result:= PyInt_FromLong(Ord(Btn.Kind));
          end;

        BTN_SET_KIND:
          begin
            Btn.Kind:= TATButtonKind(StrToIntDef(StrValue, 0));
            Result:= ReturnNone;
          end;

        BTN_GET_BOLD:
          begin
            Result:= PyBool_FromLong(Ord(Btn.BoldFont));
          end;

        BTN_SET_BOLD:
          begin
            Btn.BoldFont:= AppStrToBool(StrValue);
            Result:= ReturnNone;
          end;

        BTN_GET_ENABLED:
          begin
            Result:= PyBool_FromLong(Ord(Btn.Enabled));
          end;

        BTN_SET_ENABLED:
          begin
            Btn.Enabled:= AppStrToBool(StrValue);
            Result:= ReturnNone;
          end;

        BTN_GET_VISIBLE:
          begin
            Result:= PyBool_FromLong(Ord(Btn.Visible));
          end;

        BTN_SET_VISIBLE:
          begin
            Btn.Visible:= AppStrToBool(StrValue);
            Result:= ReturnNone;
          end;

        BTN_GET_TEXT:
          begin
            Result:= PyString_FromString(PChar(Btn.Caption));
          end;

        BTN_SET_TEXT:
          begin
            Btn.Caption:= StrValue;
            Result:= ReturnNone;
          end;

        BTN_GET_HINT:
          begin
            Result:= PyString_FromString(PChar(Btn.Hint));
          end;

        BTN_SET_HINT:
          begin
            Btn.Hint:= StrValue;
            Result:= ReturnNone;
          end;

        BTN_GET_MENU:
          begin
            Result:= Py_ObjectHandle(Btn.PopupMenu.Items);
          end;

        BTN_SET_MENU:
          begin
            Mnu:= Py_MenuFromId(StrValue);
            if Assigned(Mnu) and Assigned(Mnu.Owner) then
              Btn.PopupMenu:= Mnu.Owner as TPopupMenu;
            Result:= ReturnNone;
          end;

        BTN_GET_DATA1:
          begin
            Result:= PyString_FromString(PChar(Btn.DataString));
          end;

        BTN_SET_DATA1:
          begin
            Btn.DataString:= StrValue;
            Result:= ReturnNone;
          end;

        BTN_GET_DATA2:
          begin
            Result:= PyString_FromString(PChar(Btn.DataString2));
          end;

        BTN_SET_DATA2:
          begin
            Btn.DataString2:= StrValue;
            Result:= ReturnNone;
          end;

        BTN_GET_ARROW:
          begin
            Result:= PyBool_FromLong(Ord(Btn.Arrow));
          end;

        BTN_SET_ARROW:
          begin
            Btn.Arrow:= AppStrToBool(StrValue);
            Result:= ReturnNone;
          end;

        BTN_GET_ARROW_ALIGN:
          begin
            Result:= PyString_FromString(PChar(AppAlignmentToString(Btn.ArrowAlign)));
          end;

        BTN_SET_ARROW_ALIGN:
          begin
            Btn.ArrowAlign:= AppStringToAlignment(StrValue);
            Result:= ReturnNone;
          end;

        BTN_GET_FOCUSABLE:
          begin
            Result:= PyBool_FromLong(Ord(Btn.Focusable));
          end;

        BTN_SET_FOCUSABLE:
          begin
            Btn.Focusable:= AppStrToBool(StrValue);
            Result:= ReturnNone;
          end;

        BTN_GET_FLAT:
          begin
            Result:= PyBool_FromLong(Ord(Btn.Flat));
          end;

        BTN_SET_FLAT:
          begin
            Btn.Flat:= AppStrToBool(StrValue);
            Result:= ReturnNone;
          end;

        BTN_GET_WIDTH:
          begin
            Result:= PyInt_FromLong(Btn.Width);
          end;

        BTN_SET_WIDTH:
          begin
            //must set Btn.WidthInitial because it's used in ATFlatToolbar.Update
            //for Btn.Kind=TextChoice
            Btn.WidthInitial:= StrToIntDef(StrValue, Btn.WidthInitial);
            Btn.Width:= Btn.WidthInitial;
            Result:= ReturnNone;
          end;

        BTN_GET_ITEMS:
          begin
            Result:= PyString_FromString(PChar(Btn.Items.Text));
          end;

        BTN_SET_ITEMS:
          begin
            Btn.Items.Text:= StrValue;
            Result:= ReturnNone;
          end;

        BTN_GET_ITEMINDEX:
          begin
            Result:= PyInt_FromLong(Btn.ItemIndex);
          end;

        BTN_SET_ITEMINDEX:
          begin
            Btn.ItemIndex:= StrToIntDef(StrValue, -1);
            Result:= ReturnNone;
          end;

        else
          Result:= ReturnNone;
      end;
    end
  else
    Result:= ReturnNone;
end;


function Py_CanvasId_to_CanvasObject(const IdCanvas: Int64): TCanvas;
begin
  if IdCanvas=0 then //support PaintBox for plugins debugging
  begin
    fmMain.InitPaintTest;
    fmMain.PaintTest.Show;
    Application.ProcessMessages;
    Result:= fmMain.PaintTest.Canvas;
  end
  else
    Result:= TCanvas(PtrInt(IdCanvas));
end;


function Py_imagelist_proc(Self, Args: PPyObject): PPyObject; cdecl;
var
  IdList, Num64: Int64;
  IdAction: integer;
  PtrValue: PChar;
  StrValue: string;
  Obj: TComponent;
  List: TImageList;
  NX, NY, NIndex: integer;
  Canvas: TCanvas;
  Sep: TATStringSeparator;
begin
  with AppPython.Engine do
    if Bool(PyArg_ParseTuple(Args, 'Lis:imagelist_proc', @IdList, @IdAction, @PtrValue)) then
    begin
      StrValue:= string(PtrValue);

      if IdAction=IMAGELIST_CREATE then
      begin
        Num64:= StrToInt64Def(StrValue, 0);
        if Num64=0 then
          Obj:= fmMain
        else
          Obj:= TComponent(PtrInt(Num64));
        List:= TImageList.Create(Obj);
        exit(Py_ObjectHandle(List));
      end;

      //other actions require id_list param
      List:= TImageList(PtrInt(IdList));
      if List=nil then exit(ReturnNone);

      case IdAction of
        IMAGELIST_COUNT:
          begin
            Result:= PyInt_FromLong(List.Count);
          end;

        IMAGELIST_GET_SIZE:
          begin
            Result:= Py_BuildValue('(ii)', List.Width, List.Height);
          end;

        IMAGELIST_SET_SIZE:
          begin
            Sep.Init(StrValue);
            Sep.GetItemInt(NX, 0);
            Sep.GetItemInt(NY, 0);
            List.Width:= Min(500, Max(4, NX));
            List.Height:= Min(500, Max(4, NY));
            Result:= Py_BuildValue('(ii)', List.Width, List.Height);
          end;

        IMAGELIST_DELETE:
          begin
            NIndex:= StrToIntDef(StrValue, -1);
            if (NIndex>=0) and (NIndex<List.Count) then
              List.Delete(NIndex);
            Result:= ReturnNone;
          end;

        IMAGELIST_DELETE_ALL:
          begin
            List.Clear;
            Result:= ReturnNone;
          end;

        IMAGELIST_ADD:
          begin
            NX:= UpdateImagelistWithIconFromFile(List, StrValue);
            if NX>=0 then
              Result:= PyInt_FromLong(NX)
            else
              Result:= ReturnNone;
          end;

        IMAGELIST_PAINT:
          begin
            Sep.Init(StrValue);
            Sep.GetItemInt64(Num64, 0);
            Canvas:= Py_CanvasId_to_CanvasObject(Num64);
            Sep.GetItemInt(NX, 0);
            Sep.GetItemInt(NY, 0);
            Sep.GetItemInt(NIndex, 0);
            if (NIndex>=0) and (NIndex<List.Count) then
              List.Draw(Canvas, NX, NY, NIndex);
            Result:= ReturnNone;
          end;

        else
          Result:= ReturnNone;
      end;
    end
  else
    Result:= ReturnNone;
end;


type
  TTreeviewAccess = class(TAppTreeView);

function Py_tree_proc(Self, Args: PPyObject): PPyObject; cdecl;
var
  IdTree, IdItem: Int64;
  IdAction, NIndex, NImageIndex: integer;
  Pnt1, Pnt2: TPoint;
  PtrText: PChar;
  StrText: string;
  TreeCont: TAppTreeContainer;
  TreeNode, Node1: TTreeNode;
  ItemList: TFPList;
  Sep: TATStringSeparator;
  i: integer;
begin
  with AppPython.Engine do
    if Bool(PyArg_ParseTuple(Args, 'LiLisi:tree_proc', @IdTree, @IdAction, @IdItem, @NIndex, @PtrText, @NImageIndex)) then
    begin
      StrText:= string(PtrText);
      TreeCont:= TAppTreeContainer(PtrInt(IdTree));
      TreeNode:= TTreeNode(PtrInt(IdItem));

      case IdAction of
        TREE_ITEM_ENUM:
          begin
            if Assigned(TreeNode) then
              Node1:= TreeNode.GetFirstChild
            else
              Node1:= TreeCont.Tree.Items.GetFirstNode;
            if Node1=nil then exit(ReturnNone);

            ItemList:= TFPList.Create;
            try
              repeat
                ItemList.Add(Node1);
                Node1:= TreeNode.GetNextChild(Node1);
              until Node1=nil;

              Result:= PyList_New(ItemList.Count);
              if not Assigned(Result) then
                raise EPythonError.Create(msgPythonListError);
              for i:= 0 to ItemList.Count-1 do
                PyList_SetItem(Result, i,
                  Py_BuildValue('(Ls)',
                    Int64(PtrInt(ItemList[i])),
                    PChar(TTreeNode(ItemList[i]).Text) ));
            finally
              FreeAndNil(ItemList);
            end;
          end;

        TREE_ITEM_ADD:
          begin
            Node1:= TreeCont.Tree.Items.AddChild(TreeNode, StrText);
            Node1.Text:= StrText;
            Node1.ImageIndex:= NImageIndex;
            Node1.SelectedIndex:= NImageIndex;
            if NIndex>=0 then
              Node1.Index:= NIndex;

            TreeCont.Tree.Invalidate;
            Result:= Py_ObjectHandle(Node1);
          end;

        TREE_ITEM_DELETE:
          begin
            if Assigned(TreeNode) then
              TreeNode.Free
            else
              ClearTreeviewWithData(TreeCont.Tree);

            TreeCont.Tree.Invalidate;
            Result:= ReturnNone;
          end;

        TREE_ITEM_SET_TEXT:
          begin
            if Assigned(TreeNode) then
              TreeNode.Text:= StrText;
            TreeCont.Tree.Invalidate;
            Result:= ReturnNone;
          end;

        TREE_ITEM_SET_ICON:
          begin
            if Assigned(TreeNode) then
            begin
              TreeNode.ImageIndex:= NImageIndex;
              TreeNode.SelectedIndex:= NImageIndex;
            end;
            TreeCont.Tree.Invalidate;
            Result:= ReturnNone;
          end;

        TREE_ITEM_SELECT:
          begin
            if Assigned(TreeNode) then
            begin
              TreeNode.Selected:= true;
              TreeNode.MakeVisible;
              TreeCont.Tree.Invalidate;
            end;
            Result:= ReturnNone;
          end;

        TREE_ITEM_GET_SELECTED:
          begin
            if Assigned(TreeCont.Tree.Selected) then
              Result:= Py_ObjectHandle(TreeCont.Tree.Selected)
            else
              Result:= ReturnNone;
          end;

        TREE_ITEM_GET_PROPS:
          begin
            if Assigned(TreeNode) then
              Result:= Py_BuildValue('{sssisisisisLsOsOsO}',
                       'text',
                       PChar(TreeNode.Text),
                       'index',
                       TreeNode.Index,
                       'index_abs',
                       TreeNode.AbsoluteIndex,
                       'icon',
                       TreeNode.ImageIndex,
                       'level',
                       TreeNode.Level,
                       'parent',
                       Int64(PtrInt(TreeNode.Parent)),
                       'folded',
                       PyBool_FromLong(Ord(not TreeNode.Expanded)),
                       'selected',
                       PyBool_FromLong(Ord(TreeNode.Selected)),
                       'sub_items',
                       PyBool_FromLong(Ord(TreeNode.HasChildren))
                       )
            else
              Result:= ReturnNone;
          end;

        TREE_ITEM_FOLD:
          begin
            if Assigned(TreeNode) then
              TreeNode.Collapse(false);
            TreeCont.Tree.Invalidate;
            Result:= ReturnNone;
          end;

        TREE_ITEM_FOLD_DEEP:
          begin
            if Assigned(TreeNode) then
              TreeNode.Collapse(true)
            else
              TreeCont.Tree.FullCollapse;
            TreeCont.Tree.Invalidate;
            Result:= ReturnNone;
          end;

        TREE_ITEM_FOLD_LEVEL:
          begin
            DoTreeviewFoldLevel(TreeCont.Tree, NIndex);
            Result:= ReturnNone;
          end;

        TREE_ITEM_UNFOLD:
          begin
            //if TreeCont.Owner is TFormDummy then
            //  (TreeCont.Owner as TFormDummy).BlockedOnUnfold:= true;

            if Assigned(TreeNode) then
              TreeNode.Expand(false);

            //if TreeCont.Owner is TFormDummy then
            //  (TreeCont.Owner as TFormDummy).BlockedOnUnfold:= false;

            TreeCont.Tree.Invalidate;
            Result:= ReturnNone;
          end;

        TREE_ITEM_UNFOLD_DEEP:
          begin
            //if TreeCont.Owner is TFormDummy then
            //  (TreeCont.Owner as TFormDummy).BlockedOnUnfold:= true;

            if Assigned(TreeNode) then
              TreeNode.Expand(true)
            else
              TreeCont.Tree.FullExpand;

            //if TreeCont.Owner is TFormDummy then
            //  (TreeCont.Owner as TFormDummy).BlockedOnUnfold:= false;

            TreeCont.Tree.Invalidate;
            Result:= ReturnNone;
          end;

        TREE_ITEM_GET_RANGE:
          begin
            fmMain.DoCodetree_GetSyntaxRange(TreeNode, Pnt1, Pnt2);
            Result:= Py_BuildValue('(iiii)', Pnt1.X, Pnt1.Y, Pnt2.X, Pnt2.Y);
          end;

        TREE_ITEM_SET_RANGE:
          begin
            Sep.Init(StrText);
            Sep.GetItemInt(Pnt1.X, 0);
            Sep.GetItemInt(Pnt1.Y, 0);
            Sep.GetItemInt(Pnt2.X, 0);
            Sep.GetItemInt(Pnt2.Y, 0);
            fmMain.DoCodetree_SetSyntaxRange(TreeNode, Pnt1, Pnt2);
            Result:= ReturnNone;
          end;

        TREE_ITEM_SHOW:
          begin
            TreeNode.MakeVisible;
            Result:= ReturnNone;
          end;

        TREE_GET_IMAGELIST:
          begin
            Result:= Py_ObjectHandle(TreeCont.Tree.Images);
          end;

        TREE_LOCK:
          begin
            TreeCont.Tree.Items.BeginUpdate;
            Result:= ReturnNone;
          end;

        TREE_UNLOCK:
          begin
            TreeCont.Tree.Items.EndUpdate;
            TTreeviewAccess(TreeCont.Tree).DoSelectionChanged; //force update new scrollbars
            Result:= ReturnNone;
          end;

        TREE_PROP_SHOW_ROOT:
          begin
            TreeCont.Tree.ShowRoot:= AppStrToBool(StrText);
            TreeCont.Tree.Invalidate;
            Result:= ReturnNone;
          end;

        TREE_THEME:
          begin
            TreeCont.Themed:= true;
            DoApplyThemeToTreeview(TreeCont.Tree, true, false);
            Result:= ReturnNone;
          end;

        else
          Result:= ReturnNone;
      end;
    end;
end;

function Py_ed_dim(Self, Args: PPyObject): PPyObject; cdecl;
var
  H: Int64;
  Ed: TATSynEdit;
  Id, NIndex1, NIndex2, NValue, NLen, i: integer;
  Item: TATMarkerItem;
begin
  with AppPython.Engine do
    if Bool(PyArg_ParseTuple(Args, 'Liiii:dim', @H, @Id, @NIndex1, @NIndex2, @NValue)) then
    begin
      Ed:= PyEditor(H);

      case Id of
        DIM_ADD:
          begin
            Ed.DimRanges.Add(NIndex1, NIndex2, NValue);
            Ed.Update;
            Result:= ReturnNone;
          end;

        DIM_DELETE:
          begin
            if Ed.DimRanges.IsIndexValid(NIndex1) then
            begin
              Ed.DimRanges.Delete(NIndex1);
              Ed.Update;
            end;
            Result:= ReturnNone;
          end;

        DIM_DELETE_ALL:
          begin
            if Ed.DimRanges.Count>0 then
            begin
              Ed.DimRanges.Clear;
              Ed.Update;
            end;
            Result:= ReturnNone;
          end;

        DIM_ENUM:
          begin
            NLen:= Ed.DimRanges.Count;
            if NLen>0 then
            begin
              Result:= PyList_New(NLen);
              if not Assigned(Result) then
                raise EPythonError.Create(msgPythonListError);
              for i:= 0 to NLen-1 do
              begin
                Item:= Ed.DimRanges[i];
                PyList_SetItem(Result, i,
                  Py_BuildValue('(iii)',
                    Item.PosY,
                    Item.PosY+Item.LenY-1,
                    Item.Value
                    ));
              end;
            end
            else
              Result:= ReturnNone;
          end;

        else
          Result:= ReturnNone;
      end;
    end
  else
    Result:= ReturnNone;
end;

procedure _FindTokenRange(An: TecClientSyntAnalyzer; ALineFrom, ALineTo: integer;
  out ResFrom, ResTo: integer);
var
  NMaxLine: integer;
begin
  ResFrom:= -1;
  ResTo:= -1;

  if ALineFrom<0 then exit;
  if ALineFrom>ALineTo then exit;

  NMaxLine:= High(An.TokenIndexer);
  if ALineFrom>NMaxLine then exit;
  if ALineTo>NMaxLine then
    ALineTo:= NMaxLine;

  while (ALineFrom<=NMaxLine) and (An.TokenIndexer[ALineFrom]<0) do
    Inc(ALineFrom);
  if ALineFrom<=NMaxLine then
    ResFrom:= An.TokenIndexer[ALineFrom]
  else
    exit;

  Inc(ALineTo);
  while (ALineTo<=NMaxLine) and (An.TokenIndexer[ALineTo]<0) do
    Inc(ALineTo);
  if ALineTo<=NMaxLine then
    ResTo:= An.TokenIndexer[ALineTo]-1
  else
    ResTo:= An.TagCount-1;
end;

function Py_GetTokenList(Adapter: TATAdapterEControl; ALineFrom, ALineTo: integer): PPyObject;
var
  An: TecClientSyntAnalyzer;
  Token: PecSyntToken;
  TokenStr, TokenStyle: string;
  TokenKind: TATTokenKind;
  NIndexFrom, NIndexTo, NCount: integer;
  iToken: integer;
begin
  with AppPython.Engine do
  begin
    An:= Adapter.AnClient;
    if An=nil then exit(ReturnNone);

    _FindTokenRange(An, ALineFrom, ALineTo, NIndexFrom, NIndexTo);
    if NIndexFrom<0 then
      exit(PyList_New(0));

    NCount:= NIndexTo-NIndexFrom+1;
    if NCount<=0 then
      exit(PyList_New(0));

    Result:= PyList_New(NCount);
    if not Assigned(Result) then
      raise EPythonError.Create(msgPythonListError);

    for iToken:= NIndexFrom to NIndexTo do
    begin
      Token:= An.Tags[iToken];
      TokenStr:= Adapter.GetTokenString(Token);
      TokenKind:= atkOther;

      if Assigned(Token^.Style) then
      begin
        TokenStyle:= Token^.Style.DisplayName;
        TokenKind:= TATTokenKind(Token^.Style.TokenKind);
      end
      else
        TokenStyle:= '';

      PyList_SetItem(Result, iToken-NIndexFrom,
        Py_BuildValue('{sssssisisisisiss}',
          'str',
          PChar(TokenStr),
          'style',
          PChar(TokenStyle),
          'x1',
          Token^.Range.PointStart.X,
          'y1',
          Token^.Range.PointStart.Y,
          'x2',
          Token^.Range.PointEnd.X,
          'y2',
          Token^.Range.PointEnd.Y,
          'ki',
          Token^.TokenType,
          'ks',
          PChar(cPyTokenKind[TokenKind])
          ));
    end;
  end;
end;

function Py_ed_get_token(Self, Args: PPyObject): PPyObject; cdecl;
var
  H: Int64;
  Ed: TATSynEdit;
  Id, NIndex1, NIndex2: integer;
  Adapter: TATAdapterEControl;
  TokenKind: TATTokenKind;
begin
  with AppPython.Engine do
    if Bool(PyArg_ParseTuple(Args, 'Liii:get_token', @H, @Id, @NIndex1, @NIndex2)) then
    begin
      Ed:= PyEditor(H);
      if not (Ed.AdapterForHilite is TATAdapterEControl) then
        exit(ReturnNone);
      Adapter:= Ed.AdapterForHilite as TATAdapterEControl;

      case Id of
        TOKEN_GET_KIND:
          begin
            TokenKind:= EditorGetTokenKind(Ed, NIndex1, NIndex2);
            Result:= PyString_FromString(PChar(cPyTokenKind[TokenKind]));
          end;

        TOKEN_LIST:
          begin
            Result:= Py_GetTokenList(Adapter, 0, MaxInt)
          end;

        TOKEN_LIST_SUB:
          begin
            Result:= Py_GetTokenList(Adapter, NIndex1, NIndex2)
          end;

        else
          Result:= ReturnNone;
      end;
    end
  else
    Result:= ReturnNone;
end;


function Py_canvas_proc(Self, Args: PPyObject): PPyObject; cdecl;
var
  C: TCanvas;
  IdCanvas: Int64;
  IdAction, NColor, NSize, NStyle, NX, NY, NX2, NY2, NP1, NP2: integer;
  PtrText: PChar;
  StrText: string;
begin
  with AppPython.Engine do
    if Bool(PyArg_ParseTuple(Args, 'Lisiiiiiiiii:canvas_proc',
              @IdCanvas, @IdAction, @PtrText, @NColor, @NSize,
              @NX, @NY, @NX2, @NY2, @NStyle, @NP1, @NP2)) then
    begin
      C:= Py_CanvasId_to_CanvasObject(IdCanvas);
      StrText:= string(PtrText);

      case IdAction of
        CANVAS_SET_FONT:
          begin
            if StrText<>'' then C.Font.Name:= StrText;
            if NSize>0 then C.Font.Size:= NSize;
            if NColor>=0 then C.Font.Color:= NColor;
            if NStyle>=0 then C.Font.Style:= Canvas_NumberToFontStyles(NStyle);
            Result:= ReturnNone;
          end;

        CANVAS_SET_PEN:
          begin
            if NSize>0 then C.Pen.Width:= NSize;
            if NColor>=0 then C.Pen.Color:= NColor;
            if NStyle>=0 then C.Pen.Style:= TPenStyle(NStyle);
            if NP1>=0 then C.Pen.EndCap:= TPenEndCap(NP1);
            if NP2>=0 then C.Pen.JoinStyle:= TPenJoinStyle(NP2);
            Result:= ReturnNone;
          end;

        CANVAS_SET_BRUSH:
          begin
            if NColor>=0 then C.Brush.Color:= NColor;
            if NStyle>=0 then C.Brush.Style:= TBrushStyle(NStyle);
            Result:= ReturnNone;
          end;

        CANVAS_SET_ANTIALIAS:
          begin
            if NStyle>=0 then C.AntialiasingMode:= TAntialiasingMode(NStyle);
            Result:= ReturnNone;
          end;

        CANVAS_TEXT:
          begin
            C.TextOut(NX, NY, StrText);
            Result:= ReturnNone;
          end;

        CANVAS_GET_TEXT_SIZE:
          begin
            C.GetTextSize(StrText, NX, NY);
            Result:= Py_BuildValue('(ii)', NX, NY);
          end;

        CANVAS_LINE:
          begin
            C.Line(NX, NY, NX2, NY2);
            Result:= ReturnNone;
          end;

        CANVAS_PIXEL:
          begin
            C.Pixels[NX, NY]:= NColor;
            Result:= ReturnNone;
          end;

        CANVAS_RECT:
          begin
            C.Rectangle(NX, NY, NX2, NY2);
            Result:= ReturnNone;
          end;
        CANVAS_RECT_FRAME:
          begin
            C.FrameRect(NX, NY, NX2, NY2);
            Result:= ReturnNone;
          end;
        CANVAS_RECT_FILL:
          begin
            C.FillRect(NX, NY, NX2, NY2);
            Result:= ReturnNone;
          end;
        CANVAS_RECT_ROUND:
          begin
            C.RoundRect(NX, NY, NX2, NY2, NStyle, NStyle{Rx=Ry});
            Result:= ReturnNone;
          end;

        CANVAS_ELLIPSE:
          begin
            C.Ellipse(NX, NY, NX2, NY2);
            Result:= ReturnNone;
          end;

        CANVAS_POLYGON:
          begin
            Canvas_PaintPolygonFromSting(C, StrText);
            Result:= ReturnNone;
          end;

        CANVAS_SET_TESTPANEL:
          begin
            fmMain.InitPaintTest;
            if NSize<20 then
              fmMain.PaintTest.Hide
            else
            begin
              fmMain.PaintTest.Show;
              fmMain.PaintTest.Height:= Min(NSize, 300);
            end;
            with fmMain.PaintTest do
              Result:= Py_BuildValue('(ii)', Width, Height);
          end

        else
          Result:= ReturnNone;
      end;
    end
  else
    Result:= ReturnNone;
end;


function Py_image_proc(Self, Args: PPyObject): PPyObject; cdecl;
var
  IdImage, Num64: Int64;
  IdAction: integer;
  PtrValue: PChar;
  StrValue: string;
  Obj: TComponent;
  Image: TImage;
  Gr: TGraphic;
  NX, NY, NX2, NY2: integer;
  Canvas: TCanvas;
  Sep: TATStringSeparator;
begin
  with AppPython.Engine do
    if Bool(PyArg_ParseTuple(Args, 'Lis:image_proc', @IdImage, @IdAction, @PtrValue)) then
    begin
      StrValue:= string(PtrValue);

      if IdAction=APP_IMAGE_CREATE then
      begin
        Num64:= StrToInt64Def(StrValue, 0);
        if Num64=0 then
          Obj:= fmMain
        else
          Obj:= TComponent(PtrInt(Num64));
        Image:= TImage.Create(Obj);
        exit(Py_ObjectHandle(Image));
      end;

      //other actions require id_image param
      Image:= TImage(PtrInt(IdImage));
      if Image=nil then exit(ReturnNone);

      case IdAction of
        APP_IMAGE_GET_SIZE:
          begin
            Result:= Py_BuildValue('(ii)',
              Image.Picture.Width,
              Image.Picture.Height);
          end;

        APP_IMAGE_LOAD:
          begin
            Gr:= DoPictureLoadFromFile(StrValue);
            Image.Picture.Assign(Gr);
            Result:= PyBool_FromLong(Ord(Assigned(Gr)));
          end;

        APP_IMAGE_PAINT:
          begin
            Sep.Init(StrValue);
            Sep.GetItemInt64(Num64, 0);
            Canvas:= Py_CanvasId_to_CanvasObject(Num64);
            Sep.GetItemInt(NX, 0);
            Sep.GetItemInt(NY, 0);
            Canvas.Draw(NX, NY, Image.Picture.Graphic);
            Result:= ReturnNone;
          end;

        APP_IMAGE_PAINT_SIZED:
          begin
            Sep.Init(StrValue);
            Sep.GetItemInt64(Num64, 0);
            Canvas:= Py_CanvasId_to_CanvasObject(Num64);
            Sep.GetItemInt(NX, 0);
            Sep.GetItemInt(NY, 0);
            Sep.GetItemInt(NX2, 0);
            Sep.GetItemInt(NY2, 0);
            Canvas_PaintImageInRect(Canvas,
              Image.Picture.Graphic,
              Rect(NX, NY, NX2, NY2));
            Result:= ReturnNone;
          end;

        else
          Result:= ReturnNone;
      end;
    end
  else
    Result:= ReturnNone;
end;


function Py_ed_gap_GetDict(Ed: TATSynEdit): PPyObject; cdecl;
var
  Item: TATGapItem;
  NLen, NSizeX, NSizeY, i: Integer;
begin
  with AppPython.Engine do
  begin
    NLen:= Ed.Gaps.Count;
    if NLen>0 then
    begin
      Result:= PyList_New(NLen);
      if not Assigned(Result) then
        raise EPythonError.Create(msgPythonListError);
      for i:= 0 to NLen-1 do
      begin
        Item:= Ed.Gaps[i];

        if Assigned(Item.Bitmap) then
        begin
          NSizeX:= Item.Bitmap.Width;
          NSizeY:= Item.Bitmap.Height;
        end
        else
        begin
          NSizeX:= 0;
          NSizeY:= 0;
        end;

        PyList_SetItem(Result, i,
          Py_BuildValue('{sisLsis(ii)si}',
            'line',
            Item.LineIndex,
            'tag',
            Int64(Item.Tag),
            'size',
            Item.Size,
            'bitmap',
            NSizeX,
            NSizeY,
            'color',
            Item.Color
            ));
      end;
    end
    else
      Result:= ReturnNone;
  end;
end;


function Py_ed_gap(Self, Args: PPyObject): PPyObject; cdecl;
var
  H, Num1, Num2, NTag: Int64;
  Id, NColor, NSize: integer;
  Ed: TATSynEdit;
  Bmp: TBitmap;
  ScrollInfo: TATSynScrollInfo;
begin
  with AppPython.Engine do
    if Bool(PyArg_ParseTuple(Args, 'LiLLLii:gap', @H, @Id, @Num1, @Num2, @NTag, @NSize, @NColor)) then
    begin
      Ed:= PyEditor(H);

      case Id of
        GAP_GET_ALL:
          begin
            Result:= Py_ed_gap_GetDict(Ed);
          end;

        GAP_MAKE_BITMAP:
          begin
            Bmp:= TBitmap.Create;
            Bmp.PixelFormat:= pf24bit;
            Bmp.SetSize(Num1, Num2);
            Bmp.Canvas.Brush.Color:= clMoneyGreen;
            Bmp.Canvas.FillRect(0, 0, Bmp.Width, Bmp.Height);
            Result:= Py_BuildValue('(LL)', Int64(PtrInt(Bmp)), Int64(PtrInt(Bmp.Canvas)));
          end;

        GAP_ADD:
          begin
            if Ed.Strings.IsIndexValid(Num1) or (Num1=-1) then
            begin
              if Num2<>0 then
              begin
                Bmp:= TBitmap(PtrInt(Num2));
                if NSize<=0 then
                  NSize:= Bmp.Height+2{borders};
              end
              else
                Bmp:= nil;

              Ed.Gaps.DeleteForLineRange(Num1, Num1);
              if Ed.Gaps.Add(Num1, NSize, Bmp, NTag, NColor) then
              begin
                ////better to do always, to fix [v-scroll cannot reach end for many gaps at end]
                //if Num1=Ed.Strings.Count-1 then
                  Ed.OptLastLineOnTop:= true;

                //if added gap before 1st line, scroll editor upper
                if Num1=-1 then
                  if Ed.ScrollVert.SmoothPos=0 then
                  begin
                    ScrollInfo:= Ed.ScrollVert;
                    ScrollInfo.NPos:= -1;
                    Ed.ScrollVert:= ScrollInfo;
                  end;

                Ed.Update;
                Result:= PyBool_FromLong(1);
              end
              else
                Result:= PyBool_FromLong(0);
            end
            else
              Result:= PyBool_FromLong(0);
          end;

        GAP_DELETE:
          begin
            if Num2<Num1 then Num2:= Num1; //allow value Num2=-1
            if Ed.Gaps.DeleteForLineRange(Num1, Num2) then
              Ed.Update;
            Result:= ReturnNone;
          end;

        GAP_DELETE_ALL:
          begin
            Ed.Gaps.Clear;
            Ed.Update;
            Result:= ReturnNone;
          end;

        else
          Result:= ReturnNone;
      end;
    end
  else
    Result:= ReturnNone;
end;


function Py_ed_action_ExportHtml(Ed: TATSynEdit; const SParams: string): boolean;
var
  SItem, SKey, SValue: string;
  SFileName, STitle, SFontName: string;
  NFontSize: integer;
  bWithNums: boolean;
  NColorBg, NColorNums: TColor;
  Sep: TATStringSeparator;
begin
  SFileName:= '';
  STitle:= '';
  SFontName:= '';
  NFontSize:= 10;
  bWithNums:= true;
  NColorBg:= clWhite;
  NColorNums:= clGray;

  //text is '{key1:value1;key2:value2}' from to_str()
  Sep.Init(SDeleteCurlyBrackets(SParams), #1);
  repeat
    if not Sep.GetItemStr(SItem) then Break;
    SSplitByChar(SItem, ':', SKey, SValue);
    SValue:= StringReplace(SValue, #2, ',', [rfReplaceAll]);

    case SKey of
      'file_name': SFileName:= SValue;
      'title': STitle:= SValue;
      'font_name': SFontName:= SValue;
      'font_size': NFontSize:= StrToIntDef(SValue, NFontSize);
      'with_numbers': bWithNums:= AppStrToBool(SValue);
      'color_back': NColorBg:= StrToIntDef(SValue, NColorBg);
      'color_numbers': NColorNums:= StrToIntDef(SValue, NColorNums);
    end;
  until false;

  if SFileName='' then exit(false);
  DeleteFileUTF8(SFileName);

  DoEditorExportToHTML(Ed,
    SFileName,
    STitle,
    SFontName,
    NFontSize,
    bWithNums,
    NColorBg,
    NColorNums
    );

  Result:= FileExistsUTF8(SFileName);
end;


function Py_FindOrReplaceOne(Ed: TATSynEdit; const ATextFrom, ATextTo, AOptions: string; AReplace, AUpdateCaret: boolean): PPyObject;
var
  Finder: TATEditorFinder;
  bChange: boolean;
begin
  Finder:= TATEditorFinder.Create;
  try
    Finder.Editor:= Ed;
    Finder.StrFind:= ATextFrom;
    Finder.StrReplace:= ATextTo;
    FinderOptionsFromString(Finder, AOptions);
    Finder.OnGetToken:= @fmMain.FinderOnGetToken;

    with AppPython.Engine do
      if Finder.DoAction_FindOrReplace(AReplace, false, bChange, AUpdateCaret) then
      begin
        Result:= Py_BuildValue('(iiii)',
          Finder.MatchEdPos.X,
          Finder.MatchEdPos.Y,
          Finder.MatchEdEnd.X,
          Finder.MatchEdEnd.Y);
        if bChange then
        begin
          Ed.Update(true);
          Ed.DoEventChange;
        end
        else
        if AUpdateCaret then
          Ed.Update;
      end
      else
      begin
        if Finder.IsRegexBad then
          Result:= PyBool_FromLong(0)
        else
          Result:= ReturnNone;
      end;
  finally
    FreeAndNil(Finder);
  end;
end;

function Py_ReplaceAll(Ed: TATSynEdit; const ATextFrom, ATextTo, AOptions: string): PPyObject;
var
  Finder: TATEditorFinder;
  NCount: integer;
begin
  Finder:= TATEditorFinder.Create;
  try
    Finder.Editor:= Ed;
    Finder.StrFind:= ATextFrom;
    Finder.StrReplace:= ATextTo;
    FinderOptionsFromString(Finder, AOptions);
    Finder.OnGetToken:= @fmMain.FinderOnGetToken;

    with AppPython.Engine do
    begin
      NCount:= Finder.DoAction_ReplaceAll;
      if NCount>0 then
        Ed.Update(true);

      if NCount=0 then
        if Finder.IsRegexBad then
          exit(PyBool_FromLong(0));

      Result:= PyInt_FromLong(NCount);
    end;
  finally
    FreeAndNil(Finder);
  end;
end;



function Py_FindAllMatches(Ed: TATSynEdit; const AText, AOptions: string; AMaxLineLen: integer): PPyObject;
var
  Finder: TATEditorFinder;
  ResList: TATFinderResults;
  Res: TATFinderResult;
  i: integer;
begin
  Finder:= TATEditorFinder.Create;
  ResList:= TATFinderResults.Create;
  try
    Finder.Editor:= Ed;
    Finder.StrFind:= UTF8Decode(AText);
    FinderOptionsFromString(Finder, AOptions);
    Finder.OnGetToken:= @fmMain.FinderOnGetToken;
    Finder.MaxLineLen:= AMaxLineLen;

    Finder.DoAction_FindAll(ResList, false);

    if ResList.Count=0 then
      if Finder.IsRegexBad then
        exit(AppPython.Engine.PyBool_FromLong(0));

    with AppPython.Engine do
    begin
      Result:= PyList_New(ResList.Count);
      if not Assigned(Result) then
        raise EPythonError.Create(msgPythonListError);
      for i:= 0 to ResList.Count-1 do
      begin
        Res:= ResList[i];
        PyList_SetItem(Result, i,
          Py_BuildValue('(iiii)', Res.FPos.X, Res.FPos.Y, Res.FEnd.X, Res.FEnd.Y));
      end;
    end;
  finally
    FreeAndNil(ResList);
    FreeAndNil(Finder);
  end;
end;


function Py_ed_action(Self, Args: PPyObject): PPyObject; cdecl;
var
  HEditor: Int64;
  IdAction: integer;
  PtrText1, PtrText2, PtrText3: PChar;
  StrText1, StrText2, StrText3: string;
  Ed: TATSynEdit;
  Frame: TEditorFrame;
  Ada: TATAdapterEControl;
  NValue: Int64;
  TreeContainer: TAppTreeContainer;
  Sep: TATStringSeparator;
  CharFrom, CharTo: atChar;
  Kind: TATEditorBracketKind;
  AllowedSymbols: string;
  Distance: integer;
  Ok: boolean;
  NX, NY, N1, N2: integer;
begin
  with AppPython.Engine do
    if Bool(PyArg_ParseTuple(Args, 'Lisss:action', @HEditor, @IdAction, @PtrText1, @PtrText2, @PtrText3)) then
    begin
      Ed:= PyEditor(HEditor);
      StrText1:= string(PtrText1);
      StrText2:= string(PtrText2);
      StrText3:= string(PtrText3);
      NValue:= StrToInt64Def(StrText1, 0);

      case IdAction of
        EDACTION_UPDATE:
          begin
            Ed.Update(AppStrToBool(StrText1));
            Result:= ReturnNone;
          end;

        EDACTION_UNDOGROUP_BEGIN:
          begin
            Ed.Strings.BeginUndoGroup;
            Result:= ReturnNone;
          end;

        EDACTION_UNDOGROUP_END:
          begin
            Ed.Strings.EndUndoGroup;
            Result:= ReturnNone;
          end;

        EDACTION_FIND_ONE:
          begin
            Result:= Py_FindOrReplaceOne(Ed, StrText1, '', StrText2, false{Replace}, false);
          end;

        EDACTION_REPLACE_ONE:
          begin
            Result:= Py_FindOrReplaceOne(Ed, StrText1, StrText2, StrText3, true{Replace}, false);
          end;

        EDACTION_FIND_ALL:
          begin
            Result:= Py_FindAllMatches(Ed, StrText1, StrText2, StrToIntDef(StrText3, MaxInt));
          end;

        EDACTION_REPLACE_ALL:
          begin
            Result:= Py_ReplaceAll(Ed, StrText1, StrText2, StrText3);
          end;

        EDACTION_FIND_BRACKETS:
          begin
            Sep.Init(StrText1);
            Sep.GetItemInt(NX, -1);
            Sep.GetItemInt(NY, -1);
            AllowedSymbols:= StrText2;
            Distance:= StrToIntDef(StrText3, MaxInt);
            EditorBracket_FindBoth(Ed,
              NX, NY,
              AllowedSymbols,
              Distance,
              N1, N2,
              CharFrom, CharTo, Kind);
            Result:= Py_BuildValue('(iiii)', NX, NY, N1, N2);
          end;

        EDACTION_SHOW_POS:
          begin
            Sep.Init(StrText1);
            Sep.GetItemInt(NX, -1);
            Sep.GetItemInt(NY, -1);
            Sep.Init(StrText2);
            Sep.GetItemInt(N1, 0);
            Sep.GetItemInt(N2, 0);
            if (NX>=0) and Ed.Strings.IsIndexValid(NY) then
            begin
              ok:= Ed.DoShowPos(Point(NX, NY), N1, N2, true);
              Result:= PyBool_FromLong(Ord(ok));
            end
            else
              Result:= ReturnNone;
          end;

        EDACTION_CODETREE_FILL:
          begin
            if NValue=0 then
              exit(PyBool_FromLong(0));
            TreeContainer:= TAppTreeContainer(PtrInt(NValue));
            if Ed.AdapterForHilite is TATAdapterEControl then
            begin
              TATAdapterEControl(Ed.AdapterForHilite).TreeFill(TreeContainer.Tree);
              Result:= PyBool_FromLong(1);
            end
            else
              Result:= PyBool_FromLong(0);
          end;

        EDACTION_LEXER_SCAN:
          begin
            if Ed.Strings.IsIndexValid(NValue) then
            begin
              Ada:= nil;
              if Ed.AdapterForHilite is TATAdapterEControl then
                Ada:= TATAdapterEControl(Ed.AdapterForHilite);

              if Ada=nil then
              begin
                Frame:= GetEditorFrame(Ed);
                //maybe frame was not yet shown-> its FInitialLexer is not yet used-> force using it
                if Assigned(Frame) and not Frame.WasVisible then
                begin
                  Ada:= Frame.Adapter[Ed];
                  if Ada=nil then
                  begin
                    AllowFrameParsing:= true;
                    Frame.DoShow;
                    Ada:= Frame.Adapter[Ed];
                  end;
                end
              end;

              if Assigned(Ada) then
                Ada.DoAnalyzeFromLine(NValue, true{Wait});
            end;
            Result:= ReturnNone;
          end;

        EDACTION_EXPORT_HTML:
          begin
            Ok:= Py_ed_action_ExportHtml(Ed, StrText1);
            Result:= PyBool_FromLong(Ord(Ok));
          end;

        else
          Result:= ReturnNone;
      end;
    end
    else
      Result:= ReturnNone;
end;


function Py_ed_micromap(Self, Args: PPyObject): PPyObject; cdecl;
var
  HEditor: Int64;
  IdAction: integer;
  Num1, Num2, Num3: Int64;
  Ed: TATSynEdit;
  Ok: boolean;
  NCount, i: integer;
begin
  with AppPython.Engine do
    if Bool(PyArg_ParseTuple(Args, 'LiLLL:micromap', @HEditor, @IdAction, @Num1, @Num2, @Num3)) then
    begin
      Ed:= PyEditor(HEditor);

      case IdAction of
        MICROMAP_GET:
          begin
            NCount:= Length(Ed.Micromap.Columns);
            Result:= PyList_New(NCount);
            if not Assigned(Result) then
              raise EPythonError.Create(msgPythonListError);
            for i:= 0 to NCount-1 do
              with Ed.Micromap.Columns[i] do
                PyList_SetItem(Result, i,
                  Py_BuildValue('{sisisLsi}',
                    'size_perc',
                    NWidthPercents,
                    'size_px',
                    NWidthPixels,
                    'tag',
                    NTag,
                    'color',
                    NColor
                    ));
          end;

        MICROMAP_ADD:
          begin
            Ok:= Ed.Micromap.ColumnAdd(Num1, Num2, Num3);
            if Ok then
              Ed.Update;
            Result:= PyBool_FromLong(Ord(Ok));
          end;

        MICROMAP_DELETE:
          begin
            Ok:= Ed.Micromap.ColumnDelete(Num1);
            if Ok then
              Ed.Update;
            Result:= PyBool_FromLong(Ord(Ok));
          end;

        else
          Result:= ReturnNone;
      end;
    end;
end;

function Py_ed_folding(Self, Args: PPyObject): PPyObject; cdecl;
var
  H: Int64;
  Id, NItem, NX, NY, NY2, NStaple: integer;
  PtrText: PChar;
  StrText: string;
  Ed: TATSynEdit;
  Ok: boolean;
begin
  with AppPython.Engine do
    if Bool(PyArg_ParseTuple(Args, 'Liiiiiis:folding', @H, @Id, @NItem, @NX, @NY, @NY2, @NStaple, @PtrText)) then
    begin
      Ed:= PyEditor(H);
      StrText:= string(PtrText);

      case Id of
        FOLDING_GET_LIST:
          begin
            Result:= Py_ed_folding_GetAll(Ed);
          end;

        FOLDING_GET_LIST_FILTERED:
          begin
            if NY<0 then
              Result:= Py_ed_folding_GetAll(Ed)
            else
              Result:= Py_ed_folding_GetFiltered(Ed, NY, NY2);
          end;

        FOLDING_FOLD:
          begin
            if Ed.Fold.IsIndexValid(NItem) then
            begin
              Ed.DoRangeFold(NItem);
              Ed.Update;
            end;
            Result:= ReturnNone;
          end;

        FOLDING_UNFOLD:
          begin
            if Ed.Fold.IsIndexValid(NItem) then
            begin
              Ed.DoRangeUnfold(NItem);
              Ed.Update;
            end;
            Result:= ReturnNone;
          end;

        FOLDING_UNFOLD_LINE:
          begin
            if Ed.Strings.IsIndexValid(NItem) then
              if Ed.IsLineFolded(NItem, true) then
              begin
                Ed.DoUnfoldLine(NItem);
                Ed.Update;
              end;
            Result:= ReturnNone;
          end;

        FOLDING_ADD:
          begin
            if Ed.Strings.IsIndexValid(NY) and Ed.Strings.IsIndexValid(NY2) and (NX>=0) then
            begin
              if Ed.Fold.IsIndexValid(NItem) then
                Ed.Fold.Insert(NItem, NX+1, NY, NY2, Bool(NStaple), StrText)
              else
                Ed.Fold.Add(NX+1, NY, NY2, Bool(NStaple), StrText);

              Ed.Update(true);
              Result:= PyBool_FromLong(1);
            end
            else
              Result:= PyBool_FromLong(0);
          end;

        FOLDING_DELETE:
          begin
            if Ed.Fold.IsIndexValid(NItem) then
            begin
              Ed.Fold.Delete(NItem);
              Ed.Update(true);
            end;
            Result:= ReturnNone;
          end;

        FOLDING_DELETE_ALL:
          begin
            Ed.Fold.Clear;
            Ed.Update(true);
            Result:= ReturnNone;
          end;

        FOLDING_FIND:
          begin
            if Ed.Strings.IsIndexValid(NY) then
              NX:= Ed.Fold.FindRangeWithPlusAtLine(NY);
            if NX>=0 then
              Result:= PyInt_FromLong(NX)
            else
              Result:= ReturnNone;
          end;

        FOLDING_FOLD_ALL:
          begin
            Ed.DoCommand(cCommand_FoldAll);
            Result:= ReturnNone;
          end;

        FOLDING_UNFOLD_ALL:
          begin
            Ed.DoCommand(cCommand_UnfoldAll);
            Result:= ReturnNone;
          end;

        FOLDING_FOLD_LEVEL:
          begin
            Ed.DoFoldForLevel(NItem);
            Result:= ReturnNone;
          end;

        FOLDING_CHECK_RANGE_INSIDE:
          begin
            Ok:= Ed.Fold.IsIndexValid(NItem) and
                 Ed.Fold.IsIndexValid(NX) and
                 Ed.Fold.IsRangeInsideOther(Ed.Fold.ItemPtr(NItem), Ed.Fold.ItemPtr(NX));
            Result:= PyBool_FromLong(Ord(Ok));
          end;

        FOLDING_CHECK_RANGES_SAME:
          begin
            Ok:= Ed.Fold.IsIndexValid(NItem) and
                 Ed.Fold.IsIndexValid(NX) and
                 Ed.Fold.IsRangesSame(Ed.Fold.ItemPtr(NItem), Ed.Fold.ItemPtr(NX));
            Result:= PyBool_FromLong(Ord(Ok));
          end;

        else
          Result:= ReturnNone;
      end
    end
  else
    Result:= ReturnNone;
end;


function Py_timer_proc(Self, Args: PPyObject): PPyObject; cdecl;
const
  cMinInterval = 150;
var
  NIdAction, NInterval, N: integer;
  PtrCallback, PtrTag: PChar;
  StrCallback, StrTag: string;
  Timer: TTimer;
  SAll, SItem: string;
begin
  with AppPython.Engine do
  begin
    if Bool(PyArg_ParseTuple(Args, 'isis:timer_proc',
            @NIdAction, @PtrCallback, @NInterval, @PtrTag)) then
    begin
      StrCallback:= string(PtrCallback);
      StrTag:= string(PtrTag);

      //find timer by StrCallback
      Timer:= nil;
      for N:= 0 to fmMain.FListTimers.Count-1 do
      begin
        SSplitByChar(fmMain.FListTimers[N], '|', SItem, SAll);
        if SItem=StrCallback then
        begin
          Timer:= TTimer(fmMain.FListTimers.Objects[N]);
          //timer already exists? update its tag to new value StrTag
          fmMain.FListTimers[N]:= StrCallback+'|'+StrTag;
          Break
        end;
      end;

      case NIdAction of
        TIMER_START,
        TIMER_START_ONE:
          begin
            if NInterval<cMinInterval then
              Exit(PyBool_FromLong(0));

            if Timer=nil then
            begin
              Timer:= TTimer.Create(fmMain);
              Timer.OnTimer:= @fmMain.DoPyTimerTick;
              fmMain.FListTimers.AddObject(StrCallback+'|'+StrTag, Timer);
            end;

            if NIdAction=TIMER_START_ONE then
              Timer.Tag:= 1
            else
              Timer.Tag:= 0;

            Timer.Enabled:= false;
            Timer.Interval:= NInterval;
            Timer.Enabled:= true;
            Exit(PyBool_FromLong(1));
          end;

        TIMER_STOP,
        TIMER_DELETE:
          begin
            if Timer=nil then
              Exit(PyBool_FromLong(0));
            Timer.Enabled:= false;

            if NIdAction=TIMER_DELETE then
            begin
              Timer.Free;
              N:= fmMain.FListTimers.IndexOfObject(Timer);
              if N>=0 then
                fmMain.FListTimers.Delete(N);
            end;

            Exit(PyBool_FromLong(1));
          end;

        else
          Result:= ReturnNone;
      end;
    end
    else
      Result:= ReturnNone;
  end;
end;


//toolbar_proc(id_toolbar, id_action, text="", text2="", command="", index=-1, index2=-1)
function Py_toolbar_proc(Self, Args: PPyObject): PPyObject; cdecl;
var
  PtrToolbar, PtrText, PtrText2, PtrCmd: PChar;
  StrToolbar: string;
  FToolbar: TATFlatToolbar;
  NAction: integer;
  NIndex1, NIndex2: integer;
begin
  with AppPython.Engine do
  begin
    if Bool(PyArg_ParseTuple(Args, 'sisssii:toolbar_proc',
      @PtrToolbar, @NAction, @PtrText, @PtrText2, @PtrCmd, @NIndex1, @NIndex2)) then
    begin
      StrToolbar:= string(PtrToolbar);
      //StrText:= string(PtrText);
      //StrText2:= string(PtrText2);
      //StrCmd:= string(PtrCmd);

      FToolbar:= DoToolbar_GetToolbarFromId(StrToolbar);
      if FToolbar=nil then
        exit(ReturnNone);

      case NAction of
        TOOLBAR_UPDATE:
          begin
            FToolbar.UpdateControls;
            Result:= ReturnNone;
          end;

        TOOLBAR_GET_BUTTON_HANDLE:
          begin
            if FToolbar.IsIndexOk(NIndex1) then
              Result:= Py_ObjectHandle(FToolbar.Buttons[NIndex1])
            else
              Result:= ReturnNone;
          end;

        TOOLBAR_GET_BUTTON_HANDLES:
          begin
            Result:= PyList_New(FToolbar.ButtonCount);
            if not Assigned(Result) then
              raise EPythonError.Create(msgPythonListError);
            for NIndex1:= 0 to FToolbar.ButtonCount-1 do
              PyList_SetItem(Result, NIndex1,
                Py_ObjectHandle(FToolbar.Buttons[NIndex1]));
          end;

        TOOLBAR_GET_COUNT:
          begin
            Result:= PyInt_FromLong(FToolbar.ButtonCount);
          end;

        TOOLBAR_DELETE_ALL:
          begin
            DoToolbar_Clear(FToolbar);
            Result:= ReturnNone;
          end;

        TOOLBAR_DELETE_BUTTON:
          begin
            DoToolbar_DeleteButton(FToolbar, NIndex1);
            Result:= ReturnNone;
          end;

        TOOLBAR_ADD_ITEM:
          begin
            Result:= Py_ObjectHandle(
              DoToolbar_AddButton(FToolbar, '', '', '', NIndex1, -1));
          end;

        TOOLBAR_ADD_MENU:
          begin
            Result:= Py_ObjectHandle(
              DoToolbar_AddMenu(FToolbar, '', '', '', NIndex1, -1));
          end;

        TOOLBAR_GET_VERTICAL:
          begin
            Result:= PyBool_FromLong(Ord(FToolbar.Vertical));
          end;

        TOOLBAR_SET_VERTICAL:
          begin
            FToolbar.Vertical:= Bool(NIndex1);
            Result:= ReturnNone;
          end;

        TOOLBAR_GET_WRAP:
          begin
            Result:= PyBool_FromLong(Ord(FToolbar.Wrapable));
          end;

        TOOLBAR_SET_WRAP:
          begin
            FToolbar.Wrapable:= Bool(NIndex1);
            Result:= ReturnNone;
          end;

        TOOLBAR_GET_IMAGELIST:
          begin
            Result:= Py_ObjectHandle(FToolbar.Images);
          end;

        TOOLBAR_THEME:
          begin
            DoApplyThemeToToolbar(FToolbar);
            FToolbar.Themed:= true;
            Result:= ReturnNone;
          end;

        else
          Result:= ReturnNone;
      end;
    end
    else
      Result:= ReturnNone;
  end;
end;


//statusbar_proc(id_statusbar, id_action, index=-1, tag=0, value="")
function Py_statusbar_proc(Self, Args: PPyObject): PPyObject; cdecl;
var
  PtrBar, PtrValue: PChar;
  StrBar, StrValue: string;
  FBar: TATStatus;
  NAction: integer;
  NIndex: integer;
  NTag: Int64;
  ImgList: TImageList;
  Data: TATStatusData;
begin
  with AppPython.Engine do
  begin
    if Bool(PyArg_ParseTuple(Args, 'siiLs:statusbar_proc',
      @PtrBar, @NAction, @NIndex, @NTag, @PtrValue)) then
    begin
      StrBar:= string(PtrBar);
      StrValue:= string(PtrValue);

      FBar:= DoStatusbar_GetStatusbarFromId(StrBar);
      if FBar=nil then
        exit(ReturnNone);

      if NAction<>STATUSBAR_ADD_CELL then
        if NTag<>0 then
          NIndex:= FBar.FindPanel(NTag);

      case NAction of
        STATUSBAR_GET_COUNT:
          Result:= PyInt_FromLong(FBar.PanelCount);

        STATUSBAR_DELETE_ALL:
          begin
            FBar.DeletePanels;
            Result:= ReturnNone;
          end;

        STATUSBAR_DELETE_CELL:
          begin
            FBar.DeletePanel(NIndex);
            Result:= ReturnNone;
          end;

        STATUSBAR_ADD_CELL:
          begin
            //check that tag is not busy
            if NTag<>0 then
              if FBar.FindPanel(NTag)>=0 then
                exit(ReturnNone);

            FBar.AddPanel(NIndex, 100, taLeftJustify, '', -1, NTag);
            if NIndex<0 then
              Result:= PyInt_FromLong(FBar.PanelCount-1)
            else
              Result:= PyInt_FromLong(NIndex);
          end;

        STATUSBAR_FIND_CELL:
          begin
            NTag:= StrToIntDef(StrValue, -1);
            NIndex:= FBar.FindPanel(NTag);
            if NIndex>=0 then
              Result:= PyInt_FromLong(NIndex)
            else
              Result:= ReturnNone;
          end;

        STATUSBAR_SET_IMAGELIST:
          begin
            ImgList:= Py_ImagelistFromId(StrValue);
            FBar.Images:= ImgList;
            Result:= ReturnNone;
          end;

        STATUSBAR_GET_IMAGELIST:
          begin
            ImgList:= FBar.Images;
            Result:= Py_ObjectHandle(ImgList);
          end;

        STATUSBAR_SET_CELL_SIZE:
          begin
            Data:= FBar.GetPanelData(NIndex);
            if Assigned(Data) then
            begin
              Data.Width:= StrToIntDef(StrValue, 100);
              FBar.Invalidate;
            end;
            Result:= ReturnNone;
          end;

        STATUSBAR_SET_CELL_ALIGN:
          begin
            Data:= FBar.GetPanelData(NIndex);
            if Assigned(Data) then
            begin
              Data.Align:= AppStringToAlignment(StrValue);
              FBar.Invalidate;
            end;
            Result:= ReturnNone;
          end;

        STATUSBAR_SET_CELL_TEXT:
          begin
            Data:= FBar.GetPanelData(NIndex);
            if Assigned(Data) then
            begin
              Data.Caption:= StrValue;
              FBar.Invalidate;
            end;
            Result:= ReturnNone;
          end;

        STATUSBAR_SET_CELL_HINT:
          begin
            Data:= FBar.GetPanelData(NIndex);
            if Assigned(Data) then
            begin
              Data.Hint:= StrValue;
            end;
            Result:= ReturnNone;
          end;

        STATUSBAR_SET_CELL_IMAGEINDEX:
          begin
            Data:= FBar.GetPanelData(NIndex);
            if Assigned(Data) then
            begin
              Data.ImageIndex:= StrToIntDef(StrValue, -1);
              FBar.Invalidate;
            end;
            Result:= ReturnNone;
          end;

        STATUSBAR_SET_CELL_COLOR_BACK:
          begin
            Data:= FBar.GetPanelData(NIndex);
            if Assigned(Data) then
            begin
              Data.ColorBack:= StrToIntDef(StrValue, clWhite);
              FBar.Invalidate;
            end;
            Result:= ReturnNone;
          end;

        STATUSBAR_SET_CELL_COLOR_FONT:
          begin
            Data:= FBar.GetPanelData(NIndex);
            if Assigned(Data) then
            begin
              Data.ColorFont:= StrToIntDef(StrValue, clBlack);
              FBar.Invalidate;
            end;
            Result:= ReturnNone;
          end;

        STATUSBAR_SET_CELL_TAG:
          begin
            Data:= FBar.GetPanelData(NIndex);
            if Assigned(Data) then
            begin
              Data.Tag:= StrToInt64Def(StrValue, 0);
              FBar.Invalidate;
            end;
            Result:= ReturnNone;
          end;

        STATUSBAR_SET_CELL_AUTOSIZE:
          begin
            Data:= FBar.GetPanelData(NIndex);
            if Assigned(Data) then
            begin
              Data.AutoSize:= AppStrToBool(StrValue);
              FBar.Invalidate;
            end;
            Result:= ReturnNone;
          end;

        STATUSBAR_SET_CELL_AUTOSTRETCH:
          begin
            Data:= FBar.GetPanelData(NIndex);
            if Assigned(Data) then
            begin
              Data.AutoStretch:= AppStrToBool(StrValue);
              FBar.Invalidate;
            end;
            Result:= ReturnNone;
          end;

        STATUSBAR_SET_CELL_FONT_NAME:
          begin
            Data:= FBar.GetPanelData(NIndex);
            if Assigned(Data) then
            begin
              Data.FontName:= StrValue;
              FBar.Invalidate;
            end;
            Result:= ReturnNone;
          end;

        STATUSBAR_SET_CELL_FONT_SIZE:
          begin
            Data:= FBar.GetPanelData(NIndex);
            if Assigned(Data) then
            begin
              Data.FontSize:= StrToIntDef(StrValue, 0);
              FBar.Invalidate;
            end;
            Result:= ReturnNone;
          end;


        STATUSBAR_GET_CELL_SIZE:
          begin
            Data:= FBar.GetPanelData(NIndex);
            if Assigned(Data) then
              Result:= PyInt_FromLong(Data.Width)
            else
              Result:= ReturnNone;
          end;

        STATUSBAR_GET_CELL_ALIGN:
          begin
            Data:= FBar.GetPanelData(NIndex);
            if Assigned(Data) then
              Result:= PyString_FromString(PChar(AppAlignmentToString(Data.Align)))
            else
              Result:= ReturnNone;
          end;

        STATUSBAR_GET_CELL_TEXT:
          begin
            Data:= FBar.GetPanelData(NIndex);
            if Assigned(Data) then
              Result:= PyString_FromString(PChar(Data.Caption))
            else
              Result:= ReturnNone;
          end;

        STATUSBAR_GET_CELL_HINT:
          begin
            Data:= FBar.GetPanelData(NIndex);
            if Assigned(Data) then
              Result:= PyString_FromString(PChar(Data.Hint))
            else
              Result:= ReturnNone;
          end;

        STATUSBAR_GET_CELL_IMAGEINDEX:
          begin
            Data:= FBar.GetPanelData(NIndex);
            if Assigned(Data) then
              Result:= PyInt_FromLong(Data.ImageIndex)
            else
              Result:= ReturnNone;
          end;

        STATUSBAR_GET_CELL_COLOR_BACK:
          begin
            Data:= FBar.GetPanelData(NIndex);
            if Assigned(Data) then
              Result:= PyInt_FromLong(Data.ColorBack)
            else
              Result:= ReturnNone;
          end;

        STATUSBAR_GET_CELL_COLOR_FONT:
          begin
            Data:= FBar.GetPanelData(NIndex);
            if Assigned(Data) then
              Result:= PyInt_FromLong(Data.ColorFont)
            else
              Result:= ReturnNone;
          end;

        STATUSBAR_GET_CELL_TAG:
          begin
            Data:= FBar.GetPanelData(NIndex);
            if Assigned(Data) then
              Result:= PyLong_FromLongLong(Data.Tag)
            else
              Result:= ReturnNone;
          end;

        STATUSBAR_GET_CELL_AUTOSIZE:
          begin
            Data:= FBar.GetPanelData(NIndex);
            if Assigned(Data) then
              Result:= PyBool_FromLong(Ord(Data.AutoSize))
            else
              Result:= ReturnNone;
          end;

        STATUSBAR_GET_CELL_AUTOSTRETCH:
          begin
            Data:= FBar.GetPanelData(NIndex);
            if Assigned(Data) then
              Result:= PyBool_FromLong(Ord(Data.AutoStretch))
            else
              Result:= ReturnNone;
          end;

        STATUSBAR_GET_CELL_FONT_NAME:
          begin
            Data:= FBar.GetPanelData(NIndex);
            if Assigned(Data) then
              Result:= PyString_FromString(PChar(Data.FontName))
            else
              Result:= ReturnNone;
          end;

        STATUSBAR_GET_CELL_FONT_SIZE:
          begin
            Data:= FBar.GetPanelData(NIndex);
            if Assigned(Data) then
              Result:= PyInt_FromLong(Data.FontSize)
            else
              Result:= ReturnNone;
          end;


        //get color
        STATUSBAR_GET_COLOR_BACK:
          begin
            Result:= PyInt_FromLong(FBar.Color);
          end;

        STATUSBAR_GET_COLOR_BORDER_TOP:
          begin
            Result:= PyInt_FromLong(FBar.ColorBorderTop);
          end;

        STATUSBAR_GET_COLOR_BORDER_L:
          begin
            Result:= PyInt_FromLong(FBar.ColorBorderL);
          end;

        STATUSBAR_GET_COLOR_BORDER_R:
          begin
            Result:= PyInt_FromLong(FBar.ColorBorderR);
          end;

        STATUSBAR_GET_COLOR_BORDER_U:
          begin
            Result:= PyInt_FromLong(FBar.ColorBorderU);
          end;

        STATUSBAR_GET_COLOR_BORDER_D:
          begin
            Result:= PyInt_FromLong(FBar.ColorBorderD);
          end;

        //set color
        STATUSBAR_SET_COLOR_BACK:
          begin
            FBar.Color:= StrToIntDef(StrValue, FBar.Color);
            FBar.Invalidate;
            Result:= ReturnNone;
          end;

        STATUSBAR_SET_COLOR_BORDER_TOP:
          begin
            FBar.ColorBorderTop:= StrToIntDef(StrValue, FBar.ColorBorderTop);
            FBar.Invalidate;
            Result:= ReturnNone;
          end;

        STATUSBAR_SET_COLOR_BORDER_L:
          begin
            FBar.ColorBorderL:= StrToIntDef(StrValue, FBar.ColorBorderL);
            FBar.Invalidate;
            Result:= ReturnNone;
          end;

        STATUSBAR_SET_COLOR_BORDER_R:
          begin
            FBar.ColorBorderR:= StrToIntDef(StrValue, FBar.ColorBorderR);
            FBar.Invalidate;
            Result:= ReturnNone;
          end;

        STATUSBAR_SET_COLOR_BORDER_U:
          begin
            FBar.ColorBorderU:= StrToIntDef(StrValue, FBar.ColorBorderU);
            FBar.Invalidate;
            Result:= ReturnNone;
          end;

        STATUSBAR_SET_COLOR_BORDER_D:
          begin
            FBar.ColorBorderD:= StrToIntDef(StrValue, FBar.ColorBorderD);
            FBar.Invalidate;
            Result:= ReturnNone;
          end;

        else
          Result:= ReturnNone;
      end;
    end
    else
      Result:= ReturnNone;
  end;
end;


type
  TFormHack = class(TFormDummy);

//dlg_proc(id_dialog, id_action, prop='', index=-1, index2=-1, name='')
function Py_dlg_proc(Self, Args: PPyObject): PPyObject; cdecl;
var
  Form: TFormDummy;
  PtrProp, PtrName: PChar;
  StrProp, StrName: string;
  NAction: integer;
  NIndex, NIndex2: Int64;
  Ctl: TControl;
  Pnt: TPoint;
  Num64: Int64;
  i: integer;
begin
  with AppPython.Engine do
  begin
    if Bool(PyArg_ParseTuple(Args, 'LisLLs:dlg_proc',
      @Form, @NAction, @PtrProp, @NIndex, @NIndex2, @PtrName)) then
    begin
      StrProp:= string(PtrProp);
      StrName:= string(PtrName);

      if NAction<>DLG_CREATE then
        if Form=nil then
          exit(ReturnNone);

      //find control by name, then by index
      Ctl:= nil;
      case NAction of
        DLG_CTL_PROP_GET,
        DLG_CTL_PROP_SET,
        DLG_CTL_DELETE,
        DLG_CTL_FOCUS,
        DLG_CTL_HANDLE:
          begin
            if StrName<>'' then
              Ctl:= Form.FindControlByOurName(StrName)
            else
              Ctl:= Form.FindControlByIndex(NIndex);
          end;
      end;

      case NAction of
        DLG_CREATE:
          begin
            Form:= TFormDummy.Create(fmMain);
            Result:= Py_ObjectHandle(Form);
          end;

        DLG_FREE:
          begin
            if Form.Visible then
              Form.Hide;
            Form.Free;
            Result:= ReturnNone;
          end;

        DLG_SHOW_MODAL:
          begin
            //don't do ShowModal for docked forms, this hangs the app
            if Form.Parent=nil then
              Form.DoEmulatedModalShow
            else
              MsgLogConsole('Error: DLG_SHOW_MODAL was called for docked form');
            Result:= ReturnNone;
          end;

        DLG_SHOW_NONMODAL:
          begin
            Form.Show;
            Result:= ReturnNone;
          end;

        DLG_HIDE:
          begin
            //DoForm_CloseDockedForms(Form); //not needed
            Form.Close;
            Result:= ReturnNone;
          end;

        DLG_LOCK:
          begin
            Form.DisableAutoSizing;
            Result:= ReturnNone;
          end;

        DLG_UNLOCK:
          begin
            if TFormHack(Form).AutoSizingLockCount>0 then //check to avoid exception if Unlock was called more times
              Form.EnableAutoSizing;
            Result:= ReturnNone;
          end;

        DLG_PROP_GET:
          begin
            Result:= DoForm_GetPropsAsStringDict(Form);
          end;

        DLG_PROP_SET:
          begin
            DoForm_SetPropsFromStringDict(Form, StrProp);
            Result:= ReturnNone;
          end;

        DLG_CTL_COUNT:
          begin
            Result:= PyInt_FromLong(Form.ComponentCount);
          end;

        DLG_CTL_ADD:
          begin
            DoControl_CreateNew(StrProp, Form, Ctl);
            if Assigned(Ctl) then
            begin
              Ctl.Parent:= Form;
              DoForm_AdjustLabelForNewControl(Form, Ctl);
              Result:= PyInt_FromLong(Form.ComponentCount-1);
            end
            else
              Result:= ReturnNone;
          end;

        DLG_CTL_PROP_GET:
          begin
            if Assigned(Ctl) then
              Result:= DoControl_GetPropsAsStringDict(Ctl)
            else
              Result:= ReturnNone;
          end;

        DLG_CTL_PROP_SET:
          begin
            if Assigned(Ctl) then
            begin
              Form.BlockedOnChange:= true;
              Form.BlockedOnSelect_Listview:= true;
              Form.BlockedOnSelect_Treeview:= true;
              try
                DoControl_SetPropsFromStringDict(Ctl, StrProp);
              finally
                Form.BlockedOnChange:= false;
                Form.BlockedOnSelect_Listview:= false;
                Form.BlockedOnSelect_Treeview:= false;
              end;
            end;
            Result:= ReturnNone;
          end;

        DLG_CTL_DELETE:
          begin
            if Assigned(Ctl) then
              Ctl.Free;
            Result:= ReturnNone;
          end;

        DLG_CTL_DELETE_ALL:
          begin
            for i:= Form.ComponentCount-1 downto 0 do
              Form.Components[i].Free;
            Result:= ReturnNone;
          end;

        DLG_FOCUS:
          begin
            if Form.Visible and Form.Enabled then
              Form.SetFocus;
            Result:= ReturnNone;
          end;

        DLG_SCALE:
          begin
            DoForm_ScaleAuto(Form);
            Result:= ReturnNone;
          end;

        DLG_DOCK:
          begin
            Form.PrevBorderStyle:= Form.BorderStyle;
            Form.BorderStyle:= bsNone;

            if NIndex=0 then
              Form.Parent:= fmMain
            else
              Form.Parent:= TForm(PtrInt(NIndex));

            if StrProp='L' then Form.Align:= alLeft else
             if StrProp='R' then Form.Align:= alRight else
              if StrProp='T' then Form.Align:= alTop else
               Form.Align:= alBottom;
            Result:= ReturnNone;
          end;

        DLG_UNDOCK:
          begin
            Form.Parent:= nil;
            Form.BorderStyle:= Form.PrevBorderStyle;
            Result:= ReturnNone;
          end;

        DLG_CTL_FOCUS:
          begin
            if Assigned(Ctl) then
              DoForm_FocusControl(Form, Ctl);
            Result:= ReturnNone;
          end;

        DLG_CTL_FIND:
          begin
            Result:= PyInt_FromLong(Form.FindControlIndexByOurName(StrProp));
          end;

        DLG_CTL_HANDLE:
          begin
            if Assigned(Ctl) then
            begin
              if Ctl is TPaintBox then
                Num64:= PtrInt(TPaintBox(Ctl).Canvas)
              else
                Num64:= PtrInt(Ctl);
              Result:= PyLong_FromLongLong(Num64);
            end
            else
              Result:= ReturnNone;
          end;

        DLG_COORD_LOCAL_TO_SCREEN:
          begin
            Pnt:= Point(NIndex, NIndex2);
            Pnt:= Form.ClientToScreen(Pnt);
            Result:= Py_BuildValue('(ii)', Pnt.X, Pnt.Y);
          end;

        DLG_COORD_SCREEN_TO_LOCAL:
          begin
            Pnt:= Point(NIndex, NIndex2);
            Pnt:= Form.ScreenToClient(Pnt);
            Result:= Py_BuildValue('(ii)', Pnt.X, Pnt.Y);
          end;

        DLG_POS_GET_STR:
          begin
            Result:= PyString_FromString(PChar(FormPosGetAsString(Form, false)));
          end;

        DLG_POS_SET_FROM_STR:
          begin
            FormPosSetFromString(Form, StrProp, false);
            Result:= ReturnNone;
          end;

        else
          Result:= ReturnNone;
      end;
    end;
  end;
end;


//finder_proc(id_finder, id_action, value="", setcaret=True)
function Py_finder_proc(Self, Args: PPyObject): PPyObject; cdecl;
const
  cDefIndent=2;
var
  IdFinder: Int64;
  IdAction: integer;
  Finder: TATEditorFinder;
  PtrValue: PChar;
  StrValue: string;
  NSetCaret: integer;
  NValue: Int64;
  bSetCaret: boolean;
  bResult, bChanged: boolean;
  Pnt1, Pnt2, PntAfter: TPoint;
  ResList: TATFinderResults;
  ResItem: TATFinderResult;
  ResList2: TATFinderResults2;
  ResItem2: TATFinderResult2;
  Sep: TATStringSeparator;
  Caret: TATCaretItem;
  OldX, OldY: integer;
  OldFromCaret, OldBack: boolean;
  i: integer;
begin
  with AppPython.Engine do
    if Bool(PyArg_ParseTuple(Args, 'Lisi:finder_proc', @IdFinder, @IdAction, @PtrValue, @NSetCaret)) then
    begin
      StrValue:= string(PtrValue);
      bSetCaret:= Bool(NSetCaret);

      if (IdFinder=0) and (IdAction<>FINDER_CREATE) then
        exit(ReturnNone);
      Finder:= TATEditorFinder(PtrInt(IdFinder));

      if IdAction in [
         FINDER_FIND,
         FINDER_FIND_ALL,
         FINDER_FIND_REP,
         FINDER_REP_ALL,
         FINDER_REP_ALL_EX,
         FINDER_COUNT
         ] then
      begin
        if Finder.Editor=nil then
          exit(ReturnNone);
        if Finder.Editor.Carets.Count=0 then
          exit(ReturnNone);
        if Finder.StrFind='' then
          exit(ReturnNone);
      end;

      case IdAction of
        FINDER_CREATE:
          begin
            Finder:= TATEditorFinder.Create;
            Finder.OnConfirmReplace:= @fmMain.FinderOnConfirmReplace_API;
            Result:= Py_ObjectHandle(Finder);
          end;

        FINDER_FREE:
          begin
            FreeAndNil(Finder);
            Result:= ReturnNone;
          end;

        FINDER_CLEAR:
          begin
            Finder.Clear;
            Result:= ReturnNone;
          end;

        FINDER_SET_FINDTEXT:
          begin
            Finder.StrFind:= StrValue;
            Result:= ReturnNone;
          end;

        FINDER_GET_FINDTEXT:
          begin
            Result:= PyUnicode_FromWideString(Finder.StrFind);
          end;

        FINDER_SET_REPTEXT:
          begin
            Finder.StrReplace:= StrValue;
            Result:= ReturnNone;
          end;

        FINDER_GET_REPTEXT:
          begin
            Result:= PyUnicode_FromWideString(Finder.StrReplace);
          end;

        FINDER_SET_ED:
          begin
            NValue:= StrToInt64Def(StrValue, -1); //0 is allowed, ed's handle
            if NValue=-1 then
              exit(ReturnNone);
            Finder.Editor:= PyEditor(NValue);
            Result:= ReturnNone;
          end;

        FINDER_GET_ED:
          begin
            Result:= Py_ObjectHandle(Finder.Editor);
          end;

        FINDER_SET_OPT:
          begin
            FinderOptionsFromString(Finder, StrValue);
            Result:= ReturnNone;
          end;

        FINDER_GET_OPT:
          begin
            Result:= PyString_FromString(PChar(FinderOptionsToString(Finder)));
          end;

        FINDER_SET_MAXLEN:
          begin
            NValue:= StrToIntDef(StrValue, 0);
            if NValue>0 then
              Finder.MaxLineLen:= NValue;
            Result:= ReturnNone;
          end;

        FINDER_GET_MAXLEN:
          begin
            Result:= PyInt_FromLong(Finder.MaxLineLen);
          end;

        FINDER_SET_CARETS:
          begin
            Finder.VirtualCaretsAsString:= StrValue;
            Result:= ReturnNone;
          end;

        FINDER_GET_CARETS:
          begin
            Result:= PyString_FromString(PChar(Finder.VirtualCaretsAsString));
          end;

        FINDER_SET_INDENTS:
          begin
            Sep.Init(StrValue);
            Sep.GetItemInt(i, cDefIndent);
            Finder.IndentHorz:= i;
            Sep.GetItemInt(i, cDefIndent);
            Finder.IndentVert:= i;
            Result:= ReturnNone;
          end;

        FINDER_GET_INDENTS:
          begin
            Result:= Py_BuildValue('(ii)', Finder.IndentHorz, Finder.IndentVert);
          end;

        FINDER_SET_TAG:
          begin
            Finder.DataString:= StrValue;
            Result:= ReturnNone;
          end;

        FINDER_GET_TAG:
          begin
            Result:= PyString_FromString(PChar(Finder.DataString));
          end;

        FINDER_SET_ON_REPLACE:
          begin
            Finder.CallbackString:= StrValue;
            Result:= ReturnNone;
          end;

        FINDER_GET_ON_REPLACE:
          begin
            Result:= PyString_FromString(PChar(Finder.CallbackString));
          end;

        FINDER_FIND:
          begin
            bResult:= Finder.DoAction_FindOrReplace(false, false, bChanged, bSetCaret);
            if bResult then
            begin
              Pnt1:= Finder.MatchEdPos;
              Pnt2:= Finder.MatchEdEnd;

              if bSetCaret then
              begin
                Finder.Editor.DoShowPos(Pnt1, Finder.IndentHorz, Finder.IndentVert, false);
                Finder.Editor.Update;
              end;

              Result:= Py_BuildValue('(iiii)', Pnt1.X, Pnt1.Y, Pnt2.X, Pnt2.Y);
            end
            else
            if Finder.IsRegexBad then
              Result:= PyBool_FromLong(0)
            else
              Result:= ReturnNone;
          end;

        FINDER_FIND_REP:
          begin
            bResult:= Finder.DoAction_FindOrReplace(true, false, bChanged, bSetCaret);
            if bResult then
            begin
              Pnt1:= Finder.MatchEdPos;
              Pnt2:= Finder.MatchEdEnd;

              if bChanged then
                PntAfter:= Finder.MatchEdPosAfterRep
              else
                PntAfter:= Point(-1, -1);

              if bSetCaret then
                Finder.Editor.DoShowPos(Pnt1, Finder.IndentHorz, Finder.IndentVert, false);

              if bSetCaret or bChanged then
                Finder.Editor.Update(bChanged);

              Result:= Py_BuildValue('(iiiiii)', Pnt1.X, Pnt1.Y, Pnt2.X, Pnt2.Y, PntAfter.X, PntAfter.Y);
            end
            else
            if Finder.IsRegexBad then
              Result:= PyBool_FromLong(0)
            else
              Result:= ReturnNone;
          end;

        FINDER_FIND_ALL:
          begin
            ResList:= TATFinderResults.Create;
            try
              Finder.DoAction_FindAll(ResList, true);
              if (ResList.Count=0) and Finder.IsRegexBad then
                Result:= PyBool_FromLong(0)
              else
              begin
                Result:= PyList_New(ResList.Count);
                if not Assigned(Result) then
                  raise EPythonError.Create(msgPythonListError);
                for i:= 0 to ResList.Count-1 do
                begin
                  ResItem:= ResList[i];
                  PyList_SetItem(Result, i,
                    Py_BuildValue('(iiii)',
                      ResItem.FPos.X,
                      ResItem.FPos.Y,
                      ResItem.FEnd.X,
                      ResItem.FEnd.Y
                      ));
                end;
              end;
            finally
              FreeAndNil(ResList);
            end;
          end;

        FINDER_COUNT:
          begin
            i:= Finder.DoAction_CountAll(false);
            if (i=0) and Finder.IsRegexBad then
              Result:= PyBool_FromLong(0)
            else
              Result:= PyInt_FromLong(i);
          end;

        FINDER_REP_ALL:
          begin
            i:= Finder.DoAction_ReplaceAll;
            if (i=0) and Finder.IsRegexBad then
              Result:= PyBool_FromLong(0)
            else
            begin
              Finder.Editor.Update(i>0);
              Result:= PyInt_FromLong(i);
            end;
          end;

        FINDER_REP_ALL_EX:
          begin
            Caret:= Finder.Editor.Carets[0];
            OldX:= Caret.PosX;
            OldY:= Caret.PosY;
            OldFromCaret:= Finder.OptFromCaret;
            OldBack:= Finder.OptBack;
            Finder.OptFromCaret:= true;
            Finder.OptBack:= false;

            ResList2:= TATFinderResults2.Create;
            try
              bResult:= Finder.DoAction_FindOrReplace(true, false, bChanged, true);
              if not bResult then
              begin
                if Finder.IsRegexBad then
                  exit(PyBool_FromLong(0))
                else
                  exit(PyList_New(0));
              end;

              repeat
                ResItem2.Pos:= Finder.MatchEdPos;
                ResItem2.PosEnd:= Finder.MatchEdEnd;
                if bChanged then
                  ResItem2.PosAfter:= Finder.MatchEdPosAfterRep
                else
                  ResItem2.PosAfter:= Point(-1, -1);
                ResList2.Add(ResItem2);

                bResult:= Finder.DoAction_FindOrReplace(true, false, bChanged, true);
              until not bResult;

              Result:= PyList_New(ResList2.Count);
              if not Assigned(Result) then
                raise EPythonError.Create(msgPythonListError);
              for i:= 0 to ResList2.Count-1 do
              begin
                ResItem2:= ResList2[i];
                PyList_SetItem(Result, i,
                  Py_BuildValue('(iiiiii)',
                    ResItem2.Pos.X,
                    ResItem2.Pos.Y,
                    ResItem2.PosEnd.X,
                    ResItem2.PosEnd.Y,
                    ResItem2.PosAfter.X,
                    ResItem2.PosAfter.Y
                    ));
              end;
            finally
              FreeAndNil(ResList2);
            end;

            Finder.Editor.DoCaretSingle(OldX, OldY);
            Finder.Editor.Update(true);
            Finder.OptFromCaret:= OldFromCaret;
            Finder.OptBack:= OldBack;
          end;

        else
          Result:= ReturnNone;
      end;
    end
    else
      Result:= ReturnNone;
end;


//func at end (uses funcs above)
procedure TfmMain.PythonModuleInitialization(Sender: TObject);
begin
  with Sender as TPythonModule do
  begin
    AddMethod('app_ver', @Py_app_ver, '');
    AddMethod('app_path', @Py_app_path, '');
    AddMethod('app_proc', @Py_app_proc, '');
    AddMethod('app_log', @Py_app_log, '');
    AddMethod('app_idle', @Py_app_idle, '');
    AddMethod('emmet', @Py_emmet, '');

    AddMethod('msg_status', @Py_msg_status, '');
    AddMethod('msg_status_alt', @Py_msg_status_alt, '');
    AddMethod('msg_box', @Py_msg_box, '');
    AddMethod('msg_box_ex', @Py_msg_box_ex, '');
    AddMethod('dlg_input', @Py_dlg_input, '');
    AddMethod('dlg_input_ex', @Py_dlg_input_ex, '');
    AddMethod('dlg_file', @Py_dlg_file, '');
    AddMethod('dlg_dir', @Py_dlg_dir, '');
    AddMethod('dlg_menu', @Py_dlg_menu, '');
    AddMethod('dlg_color', @Py_dlg_color, '');
    AddMethod('dlg_hotkey', @Py_dlg_hotkey, '');
    AddMethod('dlg_hotkeys', @Py_dlg_hotkeys, '');
    AddMethod('dlg_custom', @Py_dlg_custom, '');
    AddMethod('dlg_commands', @Py_dlg_commands, '');

    AddMethod('ed_get_carets', @Py_ed_get_carets, '');
    AddMethod('ed_set_caret', @Py_ed_set_caret, '');

    AddMethod('ed_get_sel_mode', @Py_ed_get_sel_mode, '');
    AddMethod('ed_get_sel_rect', @Py_ed_get_sel_rect, '');
    AddMethod('ed_get_sel_lines', @Py_ed_get_sel_lines, '');
    AddMethod('ed_set_sel_rect', @Py_ed_set_sel_rect, '');

    AddMethod('ed_get_text_all', @Py_ed_get_text_all, '');
    AddMethod('ed_set_text_all', @Py_ed_set_text_all, '');
    AddMethod('ed_get_text_sel', @Py_ed_get_text_sel, '');
    Addmethod('ed_get_text_line', @Py_ed_get_text_line, '');
    Addmethod('ed_set_text_line', @Py_ed_set_text_line, '');
    Addmethod('ed_get_text_substr', @Py_ed_get_text_substr, '');
    Addmethod('ed_get_line_count', @Py_ed_get_line_count, '');
    AddMethod('ed_delete', @Py_ed_delete, '');
    AddMethod('ed_insert', @Py_ed_insert, '');
    AddMethod('ed_replace', @Py_ed_replace, '');
    AddMethod('ed_replace_lines', @Py_ed_replace_lines, '');

    Addmethod('ed_get_filename', @Py_ed_get_filename, '');
    AddMethod('ed_get_prop', @Py_ed_get_prop, '');
    AddMethod('ed_set_prop', @Py_ed_set_prop, '');
    AddMethod('ed_folding', @Py_ed_folding, '');
    AddMethod('ed_get_sublexer_ranges', @Py_ed_get_sublexer_ranges, '');
    AddMethod('ed_get_token', @Py_ed_get_token, '');
    AddMethod('ed_get_wrapinfo', @Py_ed_get_wrapinfo, '');

    AddMethod('ed_save', @Py_ed_save, '');
    AddMethod('ed_cmd', @Py_ed_cmd, '');
    AddMethod('ed_lock', @Py_ed_lock, '');
    AddMethod('ed_unlock', @Py_ed_unlock, '');
    AddMethod('ed_bookmark', @Py_ed_bookmark, '');
    AddMethod('ed_decor', @Py_ed_decor, '');
    AddMethod('ed_focus', @Py_ed_focus, '');
    AddMethod('ed_complete', @Py_ed_complete, '');
    AddMethod('ed_complete_alt', @Py_ed_complete_alt, '');
    AddMethod('ed_convert', @Py_ed_convert, '');
    AddMethod('ed_markers', @Py_ed_markers, '');
    AddMethod('ed_attr', @Py_ed_attr, '');
    AddMethod('ed_gap', @Py_ed_gap, '');
    AddMethod('ed_dim', @Py_ed_dim, '');
    AddMethod('ed_hotspots', @Py_ed_hotspots, '');
    AddMethod('ed_action', @Py_ed_action, '');
    AddMethod('ed_micromap', @Py_ed_micromap, '');

    AddMethod('file_open', @Py_file_open, '');
    AddMethod('ed_handles', @Py_ed_handles, '');
    AddMethod('ed_group', @Py_ed_group, '');
    AddMethod('ini_read', @Py_ini_read, '');
    AddMethod('ini_write', @Py_ini_write, '');
    AddMethod('ini_proc', @Py_ini_proc, '');

    AddMethod('lexer_proc', @Py_lexer_proc, '');
    AddMethod('tree_proc', @Py_tree_proc, '');
    AddMethod('menu_proc', @Py_menu_proc, '');
    AddMethod('toolbar_proc', @Py_toolbar_proc, '');
    AddMethod('statusbar_proc', @Py_statusbar_proc, '');
    AddMethod('listbox_proc', @Py_listbox_proc, '');
    AddMethod('canvas_proc', @Py_canvas_proc, '');
    AddMethod('timer_proc', @Py_timer_proc, '');
    AddMethod('dlg_proc', @Py_dlg_proc, '');
    AddMethod('imagelist_proc', @Py_imagelist_proc, '');
    AddMethod('image_proc', @Py_image_proc, '');
    AddMethod('button_proc', @Py_button_proc, '');
    AddMethod('finder_proc', @Py_finder_proc, '');
  end;
end;


function Py_DialogColorPicker(AColor: integer): integer;
var
  SCaption: string;
  Par1, Par2, Obj: PPyObject;
begin
  Result:= -1;
  if not AppPython.Inited then exit;

  with TIniFile.Create(GetAppLangFilename) do
  try
    SCaption:= ReadString('d_tab_color', '_', 'Colors');
  finally
    Free
  end;

  with AppPython.Engine do
  begin
    Par1:= PyString_FromString(PChar(SCaption));
    Par2:= PyInt_FromLong(AColor);

    Obj:= AppPython.RunModuleFunction('cuda_palette', 'dlg_color_palette', [Par1, Par2]);
    if Assigned(Obj) then
    try
      if PyInt_Check(Obj) then
        Result:= PyInt_AsLong(Obj);
    finally
      Py_DECREF(Obj);
    end;
  end;
end;

