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

var
  PyLastCommandModule: string = '';
  PyLastCommandMethod: string = '';
  PyLastCommandParam: string = '';
  PyCommandRunning: boolean = false;
  PyEscapeFlag: boolean = false;
  PyEditorMaybeDeleted: boolean = false;

//func at begin
function PyEditor(Handle: Int64): TATSynEdit;
var
  nTab: Integer;
begin
  Result:= nil;
  if Assigned(fmMain) then
  begin
    case Handle of
      0:
        Result:= fmMain.CurrentEditor;
      1:
        Result:= fmMain.GetEditorBrother(fmMain.CurrentEditor);
      {
      2:
        Result:= fmMain.OppositeFrame.EditorMaster;
      3:
        Result:= fmMain.OppositeFrame.EditorSlave;
        }
      cPyEditorHandleMin..
      cPyEditorHandleMax:
        begin
          nTab:= Handle-cPyEditorHandleMin;
          if (nTab>=0) and (nTab<fmMain.FrameCount) then
            Result:= fmMain.Frames[nTab].Editor
          else
            Result:= nil;
        end;
      else
        Result:= TATSynEdit(PtrInt(Handle));
    end;
  end;
end;


function Py_MenuItemFromId(Str: string): TMenuItem;
const
  cPrefixSidebar = 'side:';
  cPrefixToolmenu = 'toolmenu:';
var
  N: PtrInt;
begin
  Result:= nil;

  N:= StrToInt64Def(Str, 0);
  if N>0 then exit(TMenuItem(N));

  if Str=PyMenuId_Top then exit(fmMain.MainMenu.Items);
  if Str=PyMenuId_TopEdit then exit(fmMain.mnuEdit);
  if Str=PyMenuId_TopSel then exit(fmMain.mnuSel);
  if Str=PyMenuId_TopSearch then exit(fmMain.mnuSr);
  if Str=PyMenuId_TopFile then exit(fmMain.mnuFile);
  if Str=PyMenuId_TopView then exit(fmMain.mnuView);
  if Str=PyMenuId_TopOptions then exit(fmMain.mnuOp);
  if Str=PyMenuId_TopHelp then exit(fmMain.mnuHelp);

  if Str=PyMenuId_Text then exit(fmMain.PopupText.Items);

  if SBeginsWith(Str, cPrefixSidebar) then
  begin
    Delete(Str, 1, Length(cPrefixSidebar));
    N:= fmMain.DoSidebar_CaptionToPanelsIndex(Str);
    if N>=0 then
      exit(FAppSidePanels[N].ItemMenu.Items);
  end;

  if SBeginsWith(Str, cPrefixToolmenu) then
  begin
    for N:= 0 to fmMain.Toolbar.ButtonCount-1 do
      if fmMain.Toolbar.Buttons[N].Caption=Str then
        exit(fmMain.Toolbar.Buttons[N].PopupMenu.Items);
  end;
end;

procedure TfmMain.PythonIOSendData(Sender: TObject; const Data: AnsiString);
begin
  if Assigned(fmConsole) then
    fmConsole.DoLogConsoleLine(Data);
end;

procedure TfmMain.PythonIOSendUniData(Sender: TObject;
  const Data: UnicodeString);
begin
  if Assigned(fmConsole) then
    fmConsole.DoLogConsoleLine(Utf8Encode(Data));
end;


function Py_app_exe_version(Self, Args : PPyObject): PPyObject; cdecl;
begin
  with GetPythonEngine do
    Result:= PyString_FromString(cAppExeVersion);
end;

function Py_app_api_version(Self, Args : PPyObject): PPyObject; cdecl;
begin
  with GetPythonEngine do
    Result:= PyString_FromString(cAppApiVersion);
end;

function Py_app_path(Self, Args : PPyObject): PPyObject; cdecl;
var
  Id: integer;
begin
  with GetPythonEngine do
    if Bool(PyArg_ParseTuple(Args, 'i:app_path', @Id)) then
      case Id of
        APP_DIR_EXE:
          Result:= PyString_FromString(PChar(ExtractFileDir(Application.ExeName)));
        APP_DIR_SETTINGS:
          Result:= PyString_FromString(PChar(GetAppPath(cDirSettings)));
        APP_DIR_DATA:
          Result:= PyString_FromString(PChar(GetAppPath(cDirData)));
        APP_DIR_PY:
          Result:= PyString_FromString(PChar(GetAppPath(cDirPy)));
        APP_FILE_SESSION:
          Result:= PyString_FromString(PChar(fmMain.GetSessionFilename));
        APP_DIR_INSTALLED_ADDON:
          Result:= PyString_FromString(PChar(GetAppPath(cDirLastInstalledAddon)));
        APP_FILE_RECENTS:
          Result:= PyString_FromString(PChar(fmMain.FListRecents.Text));

        else
          Result:= ReturnNone;
      end;
end;


function Py_app_idle(Self, Args: PPyObject): PPyObject; cdecl;
var
  Flag: LongBool;
begin
  with GetPythonEngine do
    if Bool(PyArg_ParseTuple(Args, 'i:app_idle', @Flag)) then
    begin
      Application.ProcessMessages;
      Application.Idle(Flag);
      Result:= GetPythonEngine.ReturnNone;
    end;
end;


function Py_dlg_input(Self, Args : PPyObject): PPyObject; cdecl;
var
  P1, P2: PChar;
  StrCaption, StrVal: string;
begin
  with GetPythonEngine do
    if Bool(PyArg_ParseTuple(Args, 'ss:dlg_input', @P1, @P2)) then
    begin
      StrCaption:= string(P1);
      StrVal:= string(P2);
      if InputQuery(msgTitle, StrCaption, StrVal) then
        Result:= PyString_FromString(PChar(StrVal))
      else
        Result:= ReturnNone;
    end;
end;

function Py_dlg_input_ex(Self, Args : PPyObject): PPyObject; cdecl;
var
  Number, i: integer;
  PCaption: PChar;
  PLabel, PText: array[1..10] of PChar;
  SCaption: string;
  Labels, Values: array of string;
begin
  with GetPythonEngine do
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
        SCaption:= '';
        for i:= 0 to Number-1 do
          SCaption:= SCaption+Values[i]+#10;
        Result:= PyString_FromString(PChar(SCaption))
      end
      else
        Result:= ReturnNone;
    end;
end;


function Py_dlg_charmap(Self, Args : PPyObject): PPyObject; cdecl;
var
  str: string;
begin
  with GetPythonEngine do
    if Bool(PyArg_ParseTuple(Args, ':dlg_charmap')) then
    begin
      str:= DoDialogCharmapModal(GetAppLangFilename);
      if str<>'' then
        Result:= PyString_FromString(PChar(str))
      else
        Result:= ReturnNone;
    end;
end;

function Py_msg_status(Self, Args: PPyObject): PPyObject; cdecl;
var
  P: PChar;
  Str: string;
  Flag: integer;
begin
  with GetPythonEngine do
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
  with GetPythonEngine do
  begin
    if Bool(PyArg_ParseTuple(Args, 'si:msg_status_alt', @Ptr, @Num)) then
    begin
      Str:= string(Ptr);
      fmMain.MsgStatusAlt(Str, Num);
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
  with GetPythonEngine do
  begin
    if Bool(PyArg_ParseTuple(Args, 'si:msg_box', @Ptr, @Flags)) then
    begin
      Str:= string(Ptr);
      Result:= PyInt_FromLong(MsgBox(Str, Flags));
    end;
  end;
end;


function Py_file_open(Self, Args: PPyObject): PPyObject; cdecl;
var
  Ptr: PChar;
  Str: string;
  Num: integer;
  Ok: boolean;
  Pages: TATPages;
begin
  with GetPythonEngine do
  begin
    if Bool(PyArg_ParseTuple(Args, 'si:file_open', @Ptr, @Num)) then
    begin
      Str:= string(Ptr);

      Pages:= nil;
      Inc(Num, Low(TATGroupsNums)); //was 0-based, need 1-based
      if (Num>=Low(TATGroupsNums)) and (Num<=High(TATGroupsNums)) then
        Pages:= fmMain.Groups.Pages[Num];

      Ok:= fmMain.DoFileOpen(Str, Pages)<>nil;
      Result:= PyBool_FromLong(Ord(Ok));
    end;
  end;
end;

function Py_file_save(Self, Args: PPyObject): PPyObject; cdecl;
var
  Ok: boolean;
  PtrName: PChar;
  StrName: string;
  Frame: TEditorFrame;
begin
  with GetPythonEngine do
    if Bool(PyArg_ParseTuple(Args, 's:file_save', @PtrName)) then
    begin
      StrName:= string(PtrName);
      Frame:= fmMain.CurrentFrame;
      if StrName<>'' then
        Frame.FileName:= StrName;

      Ok:= Frame.DoFileSave(false, fmMain.SaveDlg, nil);
      Result:= PyBool_FromLong(Ord(Ok));
    end;
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
  with GetPythonEngine do
    if Bool(PyArg_ParseTuple(Args, 'Ls:save', @H, @PtrName)) then
    begin
      Ed:= PyEditor(H);
      Frame:= fmMain.GetEditorFrame(Ed);
      StrName:= string(PtrName);

      if StrName<>'' then
      begin
        Frame.FileName:= StrName;
        Frame.Lexer:= DoLexerFindByFilename(Frame.FileName);
      end;

      Ok:= false;
      if Assigned(Frame) then
        Ok:= Frame.DoFileSave(false, fmMain.SaveDlg, nil);
      Result:= PyBool_FromLong(Ord(Ok));
    end;
end;


function Py_ed_get_text_sel(Self, Args: PPyObject): PPyObject; cdecl;
var
  H: Int64;
begin
  with GetPythonEngine do
    if Bool(PyArg_ParseTuple(Args, 'L:get_text_sel', @H)) then
      Result:= PyUnicode_FromWideString(PyEditor(H).TextSelected);
end;

function Py_ed_get_text_line(Self, Args: PPyObject): PPyObject; cdecl;
var
  H: Int64;
  N: Integer;
  Ed: TATSynEdit;
begin
  with GetPythonEngine do
    if Bool(PyArg_ParseTuple(Args, 'Li:get_text_line', @H, @N)) then
    begin
      Ed:= PyEditor(H);
      if Ed.Strings.IsIndexValid(N) then
        Result:= PyUnicode_FromWideString(Ed.Strings.Lines[N])
      else
        Result:= ReturnNone;
    end;
end;


function Py_ed_get_text_substr(Self, Args: PPyObject): PPyObject; cdecl;
var
  H: Int64;
  X1, Y1, X2, Y2: Integer;
  Str: atString;
  Ed: TATSynEdit;
begin
  with GetPythonEngine do
    if Bool(PyArg_ParseTuple(Args, 'Liiii:get_text_substr', @H, @X1, @Y1, @X2, @Y2)) then
    begin
      Ed:= PyEditor(H);
      Str:= Ed.Strings.TextSubstring(X1, Y1, X2, Y2);
      Result:= PyUnicode_FromWideString(Str);
    end;
end;

function Py_ed_set_caret(Self, Args: PPyObject): PPyObject; cdecl;
var
  H: Int64;
  Id, NCaret: Integer;
  X1, Y1, X2, Y2: Integer;
  Ed: TATSynEdit;
begin
  with GetPythonEngine do
    if Bool(PyArg_ParseTuple(Args, 'Liiiii:set_caret', @H, @X1, @Y1, @X2, @Y2, @Id)) then
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
            Ed.DoSelect_None;
            Ed.DoCaretSingle(X1, Y1, X2, Y2);
            Ed.DoGotoPos(Point(X1, Y1),
              UiOps.FindIndentHorz,
              UiOps.FindIndentVert);
            Result:= ReturnNone;
          end;

        CARET_ADD:
          begin
            Ed.Carets.Add(X1, Y1, X2, Y2);
            Ed.Carets.Sort;
            Result:= PyInt_FromLong(Ed.Carets.Count);
          end;

        CARET_SET_INDEX..MaxInt:
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

        else
          Result:= ReturnNone;
      end;

      fmMain.UpdateFrame;
      fmMain.UpdateStatus;
    end;
end;


function Py_ed_get_carets(Self, Args: PPyObject): PPyObject; cdecl;
var
  H: Int64;
  Ed: TATSynEdit;
  Caret: TATCaretItem;
  ComArray: Variant;
  NLen, i: Integer;
begin
  with GetPythonEngine do
    if Bool(PyArg_ParseTuple(Args, 'L:get_carets', @H)) then
    begin
      Ed:= PyEditor(H);
      NLen:= Ed.Carets.Count;
      if NLen>0 then
      begin
        ComArray:= VarArrayCreate([0, NLen-1, 0, 3], varInteger);
        for i:= 0 to NLen-1 do
        begin
          Caret:= Ed.Carets[i];
          ComArray[i, 0]:= Caret.PosX;
          ComArray[i, 1]:= Caret.PosY;
          ComArray[i, 2]:= Caret.EndX; //todo: fix, gets maxdword, not -1
          ComArray[i, 3]:= Caret.EndY;
        end;
        Result:= VariantAsPyObject(ComArray);
      end
      else
        Result:= ReturnNone;
    end;
end;


function Py_ed_markers(Self, Args: PPyObject): PPyObject; cdecl;
var
  H: Int64;
  Id: Integer;
  Ed: TATSynEdit;
  ComArray: Variant;
  Mark: TATMarkerItem;
  X, Y, NTag, NLenX, NLenY, i: integer;
begin
  with GetPythonEngine do
    if Bool(PyArg_ParseTuple(Args, 'Liiiiii:markers', @H, @Id, @X, @Y, @NTag, @NLenX, @NLenY)) then
    begin
      Ed:= PyEditor(H);

      case Id of
        MARKERS_GET:
          begin
            NLenX:= Ed.Markers.Count;
            if NLenX>0 then
            begin
              ComArray:= VarArrayCreate([0, NLenX-1, 0, 4], varInteger);
              for i:= 0 to NLenX-1 do
              begin
                Mark:= Ed.Markers[i];
                ComArray[i, 0]:= Mark.PosX;
                ComArray[i, 1]:= Mark.PosY;
                ComArray[i, 2]:= Mark.LenX;
                ComArray[i, 3]:= Mark.LenY;
                ComArray[i, 4]:= Mark.Tag;
              end;
              Result:= VariantAsPyObject(ComArray);
            end
            else
              Result:= ReturnNone;
          end;

        MARKERS_ADD:
          begin
            Ed.Markers.Add(X, Y, NTag, NLenX, NLenY);
            Ed.Update;
            fmMain.UpdateStatus;
            Result:= PyInt_FromLong(Ed.Markers.Count);
          end;

        MARKERS_DELETE_ALL:
          begin
            Ed.Markers.Clear;
            Ed.Update;
            fmMain.UpdateStatus;
            Result:= ReturnNone;
          end;

        MARKERS_DELETE_LAST:
          begin
            if Ed.Markers.Count>0 then
              Ed.Markers.Delete(Ed.Markers.Count-1);
            Ed.Update;
            fmMain.UpdateStatus;
            Result:= ReturnNone;
          end;

        else
          Result:= ReturnNone;
      end;
    end;
end;


function Py_ed_attr(Self, Args: PPyObject): PPyObject; cdecl;
var
  H: Int64;
  Ed: TATSynEdit;
  ComArray: Variant;
  Mark: TATMarkerItem;
  Id, X, Y, NTag, NLen, i: integer;
  NColorFont, NColorBg, NColorBorder: integer;
  NBorderL, NBorderR, NBorderD, NBorderU: integer;
  NFontBold, NFontItalic, NFontStrike: integer;
  Obj: TATLinePartClass;
begin
  with GetPythonEngine do
    if Bool(PyArg_ParseTuple(Args, 'Liiiiiiiiiiiiiii:attr',
      @H, @Id, @NTag, @X, @Y, @NLen,
      @NColorFont, @NColorBg, @NColorBorder,
      @NFontBold, @NFontItalic, @NFontStrike,
      @NBorderL, @NBorderR, @NBorderD, @NBorderU
      )) then
    begin
      Ed:= PyEditor(H);

      case Id of
        MARKERS_GET:
          begin
            NLen:= Ed.Attribs.Count;
            if NLen>0 then
            begin
              ComArray:= VarArrayCreate([0, NLen-1, 0, 13], varInteger);
              for i:= 0 to NLen-1 do
              begin
                Mark:= Ed.Attribs[i];
                Obj:= TATLinePartClass(Mark.Ptr);
                ComArray[i, 0]:= Mark.Tag;
                ComArray[i, 1]:= Mark.PosX;
                ComArray[i, 2]:= Mark.PosY;
                ComArray[i, 3]:= Mark.LenX;
                ComArray[i, 4]:= Obj.Data.ColorFont;
                ComArray[i, 5]:= Obj.Data.ColorBG;
                ComArray[i, 6]:= Obj.Data.ColorBorder;
                ComArray[i, 7]:= Ord(Obj.Data.FontBold);
                ComArray[i, 8]:= Ord(Obj.Data.FontItalic);
                ComArray[i, 9]:= Ord(Obj.Data.FontStrikeOut);
                ComArray[i, 10]:= Ord(Obj.Data.BorderLeft);
                ComArray[i, 11]:= Ord(Obj.Data.BorderRight);
                ComArray[i, 12]:= Ord(Obj.Data.BorderDown);
                ComArray[i, 13]:= Ord(Obj.Data.BorderUp);
              end;
              Result:= VariantAsPyObject(ComArray);
            end
            else
              Result:= ReturnNone;
          end;

        MARKERS_ADD:
          begin
            Obj:= TATLinePartClass.Create;
            Obj.Data.ColorFont:= NColorFont;
            Obj.Data.ColorBG:= NColorBg;
            Obj.Data.ColorBorder:= NColorBorder;
            Obj.Data.FontBold:= Bool(NFontBold);
            Obj.Data.FontItalic:= Bool(NFontItalic);
            Obj.Data.FontStrikeOut:= Bool(NFontStrike);
            Obj.Data.BorderLeft:= TATLineStyle(NBorderL);
            Obj.Data.BorderRight:= TATLineStyle(NBorderR);
            Obj.Data.BorderDown:= TATLineStyle(NBorderD);
            Obj.Data.BorderUp:= TATLineStyle(NBorderU);

            Ed.Attribs.Add(X, Y, NTag, NLen, 0, Obj);
            Ed.Update;
            fmMain.UpdateStatus;
            Result:= PyInt_FromLong(Ed.Attribs.Count);
          end;

        MARKERS_DELETE_ALL:
          begin
            Ed.Attribs.Clear;
            Ed.Update;
            fmMain.UpdateStatus;
            Result:= ReturnNone;
          end;

        MARKERS_DELETE_LAST:
          begin
            if Ed.Attribs.Count>0 then
              Ed.Attribs.Delete(Ed.Attribs.Count-1);
            Ed.Update;
            fmMain.UpdateStatus;
            Result:= ReturnNone;
          end;

        MARKERS_DELETE_BY_TAG:
          begin
            Ed.Attribs.DeleteWithTag(NTag);
            Ed.Update;
            fmMain.UpdateStatus;
            Result:= ReturnNone;
          end;

        else
          Result:= ReturnNone;
      end;
    end;
end;


function Py_ed_get_sel_mode(Self, Args: PPyObject): PPyObject; cdecl;
var
  H: Int64;
  N: Integer;
  Ed: TATSynEdit;
begin
  with GetPythonEngine do
    if Bool(PyArg_ParseTuple(Args, 'L:get_sel_mode', @H)) then
    begin
      Ed:= PyEditor(H);
      if Ed.IsSelRectEmpty then
        N:= SEL_NORMAL
      else
        N:= SEL_COLUMN;
      Result:= PyInt_FromLong(N);
    end;
end;

function Py_ed_get_sel_rect(Self, Args: PPyObject): PPyObject; cdecl;
var
  H: Int64;
  R: TRect;
begin
  with GetPythonEngine do
    if Bool(PyArg_ParseTuple(Args, 'L:get_sel_rect', @H)) then
    begin
      R:= PyEditor(H).SelRect;
      Result:= Py_BuildValue('(iiii)', R.Left, R.Top, R.Right, R.Bottom);
    end;
end;

function Py_ed_get_sel_lines(Self, Args: PPyObject): PPyObject; cdecl;
var
  H: Int64;
  N1, N2: Integer;
  Ed: TATSynEdit;
  Caret: TATCaretItem;
begin
  with GetPythonEngine do
    if Bool(PyArg_ParseTuple(Args, 'L:get_sel_lines', @H)) then
    begin
      Ed:= PyEditor(H);
      Caret:= Ed.Carets[0];
      Caret.GetSelLines(N1, N2, false);
      Result:= Py_BuildValue('(ii)', N1, N2);
    end;
end;

function Py_ed_set_sel_rect(Self, Args: PPyObject): PPyObject; cdecl;
var
  H: Int64;
  X1, Y1, X2, Y2: Integer;
  Ed: TATSynEdit;
begin
  with GetPythonEngine do
    if Bool(PyArg_ParseTuple(Args, 'Liiii:set_sel_rect', @H, @X1, @Y1, @X2, @Y2)) then
    begin
      Ed:= PyEditor(H);
      Ed.DoSelect_None;
      Ed.DoSelect_ColumnBlock(Point(X1, Y1), Point(X2, Y2));
      fmMain.UpdateFrame;
      fmMain.UpdateStatus;
      Result:= ReturnNone;
    end;
end;


function Py_ed_set_text_all(Self, Args: PPyObject): PPyObject; cdecl;
var
  H: Int64;
  Ptr: PChar;
  Ed: TATSynEdit;
  StrW: atString;
  Shift, PosAfter: TPoint;
  i: integer;
begin
  with GetPythonEngine do
    if Bool(PyArg_ParseTuple(Args, 'Ls:set_text_all', @H, @Ptr)) then
    begin
      Ed:= PyEditor(H);

      StrW:= UTF8Decode(string(Ptr));
      Ed.Strings.BeginUndoGroup;
      try
        Ed.DoCaretSingle(0, 0);
        for i:= Ed.Strings.Count-1 downto 0 do
          Ed.Strings.TextDeleteLine(0, i, Shift, PosAfter);
        Ed.Strings.TextInsert(0, 0, StrW, false, Shift, PosAfter);
      finally
        Ed.Strings.EndUndoGroup;
        Ed.DoEventChange;
      end;

      fmMain.UpdateFrame(true);
      fmMain.UpdateStatus;
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
begin
  with GetPythonEngine do
    if Bool(PyArg_ParseTuple(Args, 'Lis:set_text_line', @H, @N, @Ptr)) then
    begin
      Ed:= PyEditor(H);
      Str:= UTF8Decode(string(Ptr));

      //replace \n \r to "_"
      for i:= 1 to Length(Str) do
        if (Str[i]=#10) or (Str[i]=#13) then Str[i]:= '_';

      if N=-1 then
        Ed.Strings.LineAdd(Str)
      else
      if Ed.Strings.IsIndexValid(N) then
      begin
        Ed.Strings.Lines[N]:= Str;
        if Ed.Strings.LinesEnds[N]=cEndNone then
          Ed.Strings.LinesEnds[N]:= Ed.Strings.Endings;
      end;

      Ed.DoEventChange;

      fmMain.UpdateFrame(true);
      fmMain.UpdateStatus;
      Result:= ReturnNone;
    end;
end;


function Py_ed_delete(Self, Args: PPyObject): PPyObject; cdecl;
var
  H: Int64;
  X1, Y1, X2, Y2: Integer;
  Ed: TATSynEdit;
  Shift, PosAfter: TPoint;
begin
  with GetPythonEngine do
    if Bool(PyArg_ParseTuple(Args, 'Liiii:delete', @H, @X1, @Y1, @X2, @Y2)) then
    begin
      Ed:= PyEditor(H);
      Ed.Strings.TextDeleteRange(X1, Y1, X2, Y2, Shift, PosAfter);
      Ed.DoEventChange;

      fmMain.UpdateFrame(true);
      fmMain.UpdateStatus;
      Result:= ReturnNone;
    end;
end;

function Py_ed_insert(Self, Args: PPyObject): PPyObject; cdecl;
var
  H: Int64;
  X1, Y1: Integer;
  Ptr: PChar;
  Str: atString;
  Ed: TATSynEdit;
  Shift, PosAfter: TPoint;
begin
  with GetPythonEngine do
    if Bool(PyArg_ParseTuple(Args, 'Liis:insert', @H, @X1, @Y1, @Ptr)) then
    begin
      Ed:= PyEditor(H);
      Str:= UTF8Decode(string(Ptr));
      Ed.Strings.TextInsert(X1, Y1, Str, false, Shift, PosAfter);
      Ed.DoEventChange;

      fmMain.UpdateFrame(true);
      fmMain.UpdateStatus;
      Result:= Py_BuildValue('(ii)', PosAfter.X, PosAfter.Y);
    end;
end;


function Py_ed_get_line_count(Self, Args: PPyObject): PPyObject; cdecl;
var
  H: Int64;
begin
  with GetPythonEngine do
    if Bool(PyArg_ParseTuple(Args, 'L:get_line_count', @H)) then
    begin
      Result:= PyInt_FromLong(PyEditor(H).Strings.Count);
    end;
end;


function Py_ed_get_filename(Self, Args: PPyObject): PPyObject; cdecl;
var
  H: Int64;
  Ed: TATSynEdit;
  F: TEditorFrame;
begin
  with GetPythonEngine do
    if Bool(PyArg_ParseTuple(Args, 'L:get_filename', @H)) then
    begin
      Ed:= PyEditor(H);
      F:= fmMain.GetEditorFrame(Ed);
      Result:= PyUnicode_FromWideString(Utf8Decode(F.FileName));
    end;
end;


function Py_ed_cmd(Self, Args: PPyObject): PPyObject; cdecl;
var
  H: Int64;
  Ed: TATSynEdit;
  NCode: integer;
  Ptr: PChar;
  Str: string;
begin
  with GetPythonEngine do
    if Bool(PyArg_ParseTuple(Args, 'Lis:cmd', @H, @NCode, @Ptr)) then
    begin
      Ed:= PyEditor(H);
      Str:= string(Ptr);

      if IsCommandNeedTimer(NCode) then
      begin
        fmMain.TimerCmd.Tag:= NCode;
        fmMain.TimerCmdTimer(nil);
      end
      else
      begin
        Ed.DoCommand(NCode, Utf8Decode(Str));
        Ed.Update();
      end;

      fmMain.UpdateStatus;
      Result:= ReturnNone;
    end;
end;

function Py_ed_lock(Self, Args: PPyObject): PPyObject; cdecl;
var
  H: Int64;
  Ed: TATSynEdit;
begin
  with GetPythonEngine do
    if Bool(PyArg_ParseTuple(Args, 'L:lock', @H)) then
    begin
      Ed:= PyEditor(H);
      Ed.BeginUpdate;
      Result:= ReturnNone;
    end;
end;

function Py_ed_unlock(Self, Args: PPyObject): PPyObject; cdecl;
var
  H: Int64;
  Ed: TATSynEdit;
begin
  with GetPythonEngine do
    if Bool(PyArg_ParseTuple(Args, 'L:unlock', @H)) then
    begin
      Ed:= PyEditor(H);
      Ed.EndUpdate;
      fmMain.UpdateFrame();
      fmMain.UpdateStatus;
      Result:= ReturnNone;
    end;
end;

function Py_ed_focus(Self, Args: PPyObject): PPyObject; cdecl;
var
  H: Int64;
  Ed: TATSynEdit;
  F: TEditorFrame;
begin
  with GetPythonEngine do
    if Bool(PyArg_ParseTuple(Args, 'L:focus', @H)) then
    begin
      Ed:= PyEditor(H);
      F:= fmMain.GetEditorFrame(Ed);
      fmMain.SetFrame(F);
      EditorFocus(Ed);

      fmMain.UpdateFrame();
      fmMain.UpdateStatus;
      Result:= ReturnNone;
    end;
end;


function Py_ed_bookmark(Self, Args: PPyObject): PPyObject; cdecl;
var
  H: Int64;
  PtrText: PChar;
  StrText: string;
  Ed: TATSynEdit;
  NId, NLine, NKind, NColor: integer;
  ComArray: Variant;
  List: TList;
  i: Integer;
begin
  with GetPythonEngine do
    if Bool(PyArg_ParseTuple(Args, 'Liiiis:bookmark', @H, @NId, @NLine, @NKind, @NColor, @PtrText)) then
    begin
      Ed:= PyEditor(H);
      StrText:= string(PtrText);
      //limit NKind
      NKind:= Max(Low(AppBookmarkSetup), Min(High(AppBookmarkSetup), NKind));

      case NId of
        BOOKMARK_GET:
          begin
            Result:= PyInt_FromLong(Ed.Strings.LinesBm[NLine]);
          end;

        BOOKMARK_SET:
          begin
            EditorBookmarkSet(Ed, NLine, NKind, bmOpSet, StrText);
            Result:= ReturnNone;
          end;

        BOOKMARK_CLEAR:
          begin
            EditorBookmarkSet(Ed, NLine, NKind, bmOpClear, '');
            Result:= ReturnNone;
          end;

        BOOKMARK_CLEAR_ALL:
          begin
            EditorBookmarkClearAll(Ed);
            Result:= ReturnNone;
          end;

        BOOKMARK_SETUP:
          begin
            if (StrText<>'') and UpdateImagelistWithIconFromFile(AppBookmarkImagelist, StrText) then
              AppBookmarkSetup[NKind].ImageIndex:= AppBookmarkImagelist.Count-1
            else
              AppBookmarkSetup[NKind].ImageIndex:= -1;
            AppBookmarkSetup[NKind].Color:= NColor;
            Result:= ReturnNone;
          end;

        BOOKMARK_GET_LIST:
          begin
            List:= TList.Create;
            try
              for i:= 0 to Ed.Strings.Count-1 do
              begin
                if Ed.Strings.LinesBm[i]>0 then
                  List.Add(Pointer(PtrInt(i)));
              end;

              if List.Count>0 then
              begin
                ComArray:= VarArrayCreate([0, List.Count-1], varInteger);
                for i:= 0 to List.Count-1 do
                  ComArray[i]:= PtrInt(List[i]);
                Result:= VariantAsPyObject(ComArray);
              end
              else
                Result:= ReturnNone;
            finally
              FreeAndNil(List);
            end;
          end;


        BOOKMARK_CLEAR_HINTS:
          begin
            Ed.Strings.ClearHints;
            Result:= ReturnNone;
          end;

        else
          Result:= ReturnNone;
      end;

      fmMain.UpdateFrame(true);
      fmMain.UpdateStatus;
    end;
end;


function Py_ed_get_split(Self, Args: PPyObject): PPyObject; cdecl;
var
  H: Int64;
  NState: integer;
  NValue: double;
  Ed: TATSynEdit;
  F: TEditorFrame;
begin
  with GetPythonEngine do
    if Bool(PyArg_ParseTuple(Args, 'L:get_split', @H)) then
    begin
      Ed:= PyEditor(H);
      F:= fmMain.GetEditorFrame(Ed);

      NValue:= F.SplitPos;
      if not F.Splitted then NState:= TAB_SPLIT_NO else
        if F.SplitHorz then NState:= TAB_SPLIT_HORZ else
          NState:= TAB_SPLIT_VERT;

      Result:= Py_BuildValue('(id)', NState, NValue);
    end;
end;


function Py_ed_set_split(Self, Args: PPyObject): PPyObject; cdecl;
var
  H: Int64;
  NState: integer;
  NValue: double;
  Ed: TATSynEdit;
  F: TEditorFrame;
begin
  with GetPythonEngine do
    if Bool(PyArg_ParseTuple(Args, 'Lid:set_split', @H, @NState, @NValue)) then
    begin
      Ed:= PyEditor(H);
      F:= fmMain.GetEditorFrame(Ed);
      case NState of
        TAB_SPLIT_NO: begin F.Splitted:= false; end;
        TAB_SPLIT_HORZ: begin F.Splitted:= true; F.SplitHorz:= true; end;
        TAB_SPLIT_VERT: begin F.Splitted:= true; F.SplitHorz:= false; end;
      end;
      F.SplitPos:= NValue;
      Result:= ReturnNone;
    end;
end;

function Py_ed_handles(Self, Args : PPyObject): PPyObject; cdecl;
var
  nMin, nMax: integer;
begin
  nMin:= cPyEditorHandleMin;
  nMax:= Min(cPyEditorHandleMin + fmMain.FrameCount - 1, cPyEditorHandleMax);

  with GetPythonEngine do
    Result:= Py_BuildValue('(ii)', nMin, nMax);
end;

function Py_ed_group(Self, Args: PPyObject): PPyObject; cdecl;
var
  Index: Longint;
  Pages: TATPages;
  Data: TATTabData;
  Ed: TATSynEdit;
begin
  with GetPythonEngine do
    if Bool(PyArg_ParseTuple(Args, 'i:ed_group', @Index)) then
    begin
      //need 1-based index
      Inc(Index, Low(TATGroupsNums));
      if not (Index in [Low(TATGroupsNums)..High(TATGroupsNums)]) then
        exit(ReturnNone);

      Pages:= fmMain.Groups.Pages[Index];
      Data:= Pages.Tabs.GetTabData(Pages.Tabs.TabIndex);
      if Data=nil then
        exit(ReturnNone);

      Ed:= (Data.TabObject as TEditorFrame).Editor;
      Result:= PyLong_FromLongLong(PtrInt(Ed));
    end;
end;


function Py_dlg_file(Self, Args: PPyObject): PPyObject; cdecl;
var
  PtrFilename, PtrFolder, PtrFilter: PChar;
  StrFilename, StrFolder, StrFilter: string;
  IsOpen: LongBool;
  IsAllowAny: boolean;
  Dlg: TOpenDialog;
begin
  with GetPythonEngine do
    if Bool(PyArg_ParseTuple(Args, 'isss:dlg_file',
      @IsOpen, @PtrFilename, @PtrFolder, @PtrFilter)) then
    begin
      StrFilename:= string(PtrFilename);
      StrFolder:= string(PtrFolder);
      StrFilter:= string(PtrFilter);

      IsAllowAny:= SBeginsWith(StrFilename, '!');
      if IsAllowAny then Delete(StrFilename, 1, 1);

      if IsOpen then
      begin
        Dlg:= TOpenDialog.Create(nil);
        Dlg.Options:= Dlg.Options+[ofHideReadOnly, ofPathMustExist];
        if not IsAllowAny then
          Dlg.Options:= Dlg.Options+[ofFileMustExist];
      end
      else
      begin
        Dlg:= TSaveDialog.Create(nil);
        Dlg.Options:= Dlg.Options+[ofHideReadOnly, ofPathMustExist, ofOverwritePrompt];
      end;

      try
        if StrFilename='*' then
        begin
          StrFilename:= '';
          Dlg.Options:= Dlg.Options+[ofAllowMultiSelect];
        end;

        Dlg.FileName:= StrFilename;
        Dlg.InitialDir:= StrFolder;
        Dlg.Filter:= StrFilter;

        if Dlg.Execute then
        begin
          if ofAllowMultiSelect in Dlg.Options then
            StrFilename:= Dlg.Files.Text
          else
            StrFilename:= Dlg.FileName;
          Result:= PyUnicode_FromWideString(Utf8Decode(StrFilename));
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
  PtrFolder: PChar;
  StrFolder: string;
  Dlg: TSelectDirectoryDialog;
begin
  with GetPythonEngine do
    if Bool(PyArg_ParseTuple(Args, 's:dlg_dir', @PtrFolder)) then
    begin
      StrFolder:= string(PtrFolder);
      Dlg:= TSelectDirectoryDialog.Create(nil);
      try
        Dlg.InitialDir:= StrFolder;
        Dlg.Options:= Dlg.Options
          -[ofViewDetail]
          +[ofCreatePrompt, ofPathMustExist];
          //Win32: hide editbox, show "create dir" btn
        if Dlg.Execute then
          Result:= PyUnicode_FromWideString(Utf8Decode(Dlg.FileName))
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
  Caret: TATCaretItem;
  PrevCharsLeft: integer;
begin
  AText:= '';
  ACharsLeft:= 0;
  ACharsRight:= 0;

  Caret:= CurrentEditor.Carets[0];

  //if caret moved by 1 char: repeat auto-complete
  if (Abs(Caret.PosX-FPyComplete_CaretPos.X)=1) and
     (Caret.PosY=FPyComplete_CaretPos.Y) then
  begin
    PrevCharsLeft:= FPyComplete_CharsLeft;
    DoPyEvent(CurrentEditor, cEventOnComplete, []);

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
  i: integer;
  Str0, Str1, StrSnippet: string;
begin
  //find index-th line
  Str0:= FPyComplete_Text;
  for i:= 0 to ASnippetIndex do
    Str1:= SGetItem(Str0, #10);
  //in line snippet is 3rd column, skip 2 columns
  Str0:= Str1;
  Str1:= SGetItem(Str0, #9);
  Str1:= SGetItem(Str0, #9);
  StrSnippet:= Str0;

  //ShowMessage('id: '+ASnippetId+#10+'text:'#10+StrSnippet);
  DoPyEvent(CurrentEditor, cEventOnSnippet, [
    SStringToPythonString(ASnippetId),
    SStringToPythonString(StrSnippet)
    ]);
end;

function Py_ed_complete(Self, Args: PPyObject): PPyObject; cdecl;
var
  H: Int64;
  Ptr: PChar;
  Ed: TATSynEdit;
  Str: string;
  NChars1, NChars2, NSelIndex: integer;
begin
  with GetPythonEngine do
    if Bool(PyArg_ParseTuple(Args, 'Lsiii:complete', @H, @Ptr, @NChars1, @NChars2, @NSelIndex)) then
    begin
      Ed:= PyEditor(H);
      Str:= string(Ptr);

      fmMain.FPyComplete_Text:= Str;
      fmMain.FPyComplete_CharsLeft:= NChars1;
      fmMain.FPyComplete_CharsRight:= NChars2;
      with Ed.Carets[0] do
        fmMain.FPyComplete_CaretPos:= Point(PosX, PosY);
      DoEditorCompletionListbox(Ed, @fmMain.PyCompletionOnGetProp, nil, '_', NSelIndex);

      Result:= ReturnNone;
    end;
end;

function Py_ed_complete_alt(Self, Args: PPyObject): PPyObject; cdecl;
var
  H: Int64;
  PtrText, PtrId: PChar;
  Ed: TATSynEdit;
  StrText, StrId: string;
  NChars, NSelIndex: integer;
begin
  with GetPythonEngine do
    if Bool(PyArg_ParseTuple(Args, 'Lssii:complete_alt', @H, @PtrText, @PtrId, @NChars, @NSelIndex)) then
    begin
      Ed:= PyEditor(H);
      StrText:= string(PtrText);
      StrId:= string(PtrId);

      fmMain.FPyComplete_Text:= StrText;
      fmMain.FPyComplete_CharsLeft:= NChars;
      fmMain.FPyComplete_CharsRight:= 0;
      with Ed.Carets[0] do
        fmMain.FPyComplete_CaretPos:= Point(PosX, PosY);

      DoEditorCompletionListbox(Ed, @fmMain.PyCompletionOnGetProp, @fmMain.PyCompletionOnResult, StrId, NSelIndex);

      Result:= ReturnNone;
    end;
end;


function Py_ini_readwrite(Self, Args: PPyObject; AWrite: boolean): PPyObject; cdecl;
var
  P1, P2, P3, P4: PChar;
  StrFN, StrSess, StrKey, StrVal: string;
  fn: string;
begin
  with GetPythonEngine do
  begin
    if Bool(PyArg_ParseTuple(Args, 'ssss:ini_readwrite', @P1, @P2, @P3, @P4)) then
    begin
      StrFN:= string(P1);
      StrSess:= string(P2);
      StrKey:= string(P3);
      StrVal:= string(P4);

      fn:= StrFN;
      if ExtractFileDir(fn)='' then
        fn:= GetAppPath(cDirSettings)+DirectorySeparator+fn;

      with TIniFile.Create(fn) do
      try
        if AWrite then
        begin
          WriteString(StrSess, StrKey, StrVal);
          Result:= ReturnNone;
        end
        else
        begin
          StrVal:= ReadString(StrSess, StrKey, StrVal);
          Result:= PyString_FromString(PChar(StrVal));
        end;
      finally
        Free
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

function Py_dlg_color(Self, Args : PPyObject): PPyObject; cdecl;
var
  NCode: Longint;
begin
  with GetPythonEngine do
    if Bool(PyArg_ParseTuple(Args, 'i:dlg_color', @NCode)) then
    begin
      if not Assigned(fmMain.FColorDialog) then
        fmMain.FColorDialog:= TColorDialog.Create(fmMain);
      fmMain.FColorDialog.Color:= NCode;
      if fmMain.FColorDialog.Execute then
        Result:= PyInt_FromLong(fmMain.FColorDialog.Color)
      else
        Result:= ReturnNone;
    end;
end;

function Py_dlg_menu(Self, Args: PPyObject): PPyObject; cdecl;
var
  Ptr: PChar;
  Str: string;
  Id, NIndex: integer;
begin
  with GetPythonEngine do
    if Bool(PyArg_ParseTuple(Args, 'isi:dlg_menu', @Id, @Ptr, @NIndex)) then
    begin
      Str:= string(Ptr);
      case Id of
        MENU_LIST,
        MENU_LIST_ALT:
          begin
            Id:= fmMain.DoDialogMenuApi(Str, Id=MENU_LIST_ALT, NIndex);
            if Id>=0 then
              Result:= PyInt_FromLong(Id)
            else
              Result:= ReturnNone;
          end
        else
          Result:= ReturnNone;
      end;
    end;
end;

type
  PAppPanelProps = ^TAppPanelProps;
  TAppPanelProps = record
    Listbox: TATListbox;
    RegexStr: string;
    RegexIdLine,
    RegexIdCol,
    RegexIdName: integer;
    DefFilename: string;
    ZeroBase: boolean;
    Encoding: string;
  end;

type
  TAppPanelPropsClass = class
    Data: TAppPanelProps;
  end;

var
  AppPanelProp_Out: TAppPanelProps;
  AppPanelProp_Val: TAppPanelProps;
  AppPanelActive: string = '0';


function GetAppPanelProps_ByCaption(const ACaption: string): PAppPanelProps;
var
  N: integer;
  PropObject: TAppPanelPropsClass;
begin
  Result:= nil;
  if ACaption='0' then
    Result:= @AppPanelProp_Out
  else
  if ACaption='1' then
    Result:= @AppPanelProp_Val
  else
  begin
    N:= fmMain.FPanelCaptions.IndexOf(ACaption);
    if N<0 then exit;
    PropObject:= fmMain.FPanelCaptions.Objects[N] as TAppPanelPropsClass;
    Result:= @PropObject.Data;
  end;
end;

function GetAppPanelProps_ByListbox(AListbox: TATListbox): PAppPanelProps;
var
  PropObject: TAppPanelPropsClass;
  i: integer;
begin
  Result:= nil;

  if AListbox=fmMain.ListboxOut then
    exit(@AppPanelProp_Out);
  if AListbox=fmMain.ListboxVal then
    exit(@AppPanelProp_Val);

  for i:= 0 to fmMain.FPanelCaptions.Count-1 do
  begin
    PropObject:= fmMain.FPanelCaptions.Objects[i] as TAppPanelPropsClass;
    if PropObject.Data.Listbox=AListbox then
      exit(@PropObject.Data);
  end;
end;


function Py_app_log(Self, Args: PPyObject): PPyObject; cdecl;
var
  Ptr: PChar;
  Str: string;
  Id, NTag, i: integer;
  Prop: PAppPanelProps;
begin
  with GetPythonEngine do
    if Bool(PyArg_ParseTuple(Args, 'isi:app_log', @Id, @Ptr, @NTag)) then
    begin
      Str:= string(Ptr);

      //this action don't need Prop
      if Id=LOG_SET_PANEL then
      begin
        AppPanelActive:= Str;
        Result:= ReturnNone;
        exit
      end;

      Prop:= GetAppPanelProps_ByCaption(AppPanelActive);
      if Prop=nil then
      begin
        Result:= ReturnNone;
        exit
      end;

      case Id of
        LOG_CLEAR:
          begin
            Prop^.Listbox.Items.Clear;
            Prop^.Listbox.ItemCount:= 0;
            Prop^.Listbox.Invalidate;
          end;
        LOG_ADD:
          begin
            Prop^.Listbox.Items.AddObject(Str, TObject(PtrInt(NTag)));
            Prop^.Listbox.ItemCount:= Prop^.Listbox.Items.Count;
            Prop^.Listbox.Invalidate;
          end;
        LOG_SET_REGEX:
          Prop^.RegexStr:= Str;
        LOG_SET_LINE_ID:
          Prop^.RegexIdLine:= StrToIntDef(Str, 0);
        LOG_SET_COL_ID:
          Prop^.RegexIdCol:= StrToIntDef(Str, 0);
        LOG_SET_NAME_ID:
          Prop^.RegexIdName:= StrToIntDef(Str, 0);
        LOG_SET_FILENAME:
          Prop^.DefFilename:= Str;
        LOG_SET_ZEROBASE:
          Prop^.ZeroBase:= Bool(StrToIntDef(Str, 0));

        LOG_GET_LINES:
          begin
            Str:= '';
            for i:= 0 to Prop^.Listbox.Items.Count-1 do
              Str:= Str + Prop^.Listbox.Items[i] + #13 + IntToStr(PtrInt(Prop^.Listbox.Items.Objects[i]))
                    + IfThen(i<Prop^.Listbox.Items.Count-1, #10);
            Result:= PyString_FromString(PChar(Str));
            exit; //skip ReturnNone
          end;

        LOG_GET_LINEINDEX:
          begin
            Result:= PyInt_FromLong(Prop^.Listbox.ItemIndex);
            exit; //skip ReturnNone
          end;
        LOG_SET_LINEINDEX:
          begin
            Prop^.Listbox.ItemIndex:= StrToIntDef(Str, 0);
          end;

        LOG_PANEL_ADD:
          begin
            Result:= PyBool_FromLong(Ord(fmMain.DoPyPanelAdd(Str)));
            Exit; //skip ReturnNone
          end;
        LOG_PANEL_DELETE:
          begin
            Result:= PyBool_FromLong(Ord(fmMain.DoPyPanelDelete(Str)));
            Exit; //skip ReturnNone
          end;
        LOG_PANEL_FOCUS:
          begin
            Result:= PyBool_FromLong(Ord(fmMain.DoPyPanelFocus(Str)));
            Exit; //skip ReturnNone
          end;

        LOG_CONSOLE_CLEAR:
          with fmMain do
          begin
            if (Str='') or (Pos('m', Str)>0) then
            begin
              fmConsole.memo.ModeReadOnly:= false;
              fmConsole.memo.DoCommand(cCommand_GotoTextBegin);
              fmConsole.memo.DoCommand(cCommand_TextDeleteToTextEnd);
              fmConsole.memo.ModeReadOnly:= true;
            end;
            if (Str='') or (Pos('e', Str)>0) then
            begin
              fmConsole.ed.DoCommand(cCommand_GotoTextBegin);
              fmConsole.ed.DoCommand(cCommand_TextDeleteToTextEnd);
            end;
            if (Str='') or (Pos('h', Str)>0) then
            begin
              fmConsole.ed.Items.Clear;
            end;
          end;

        LOG_CONSOLE_ADD:
          with fmMain do
          begin
            fmConsole.DoLogConsoleLine(cPyConsolePrompt+Str);
            fmConsole.ed.DoAddLineToHistory(Utf8Decode(Str), cPyConsoleMaxComboItems);
            fmConsole.ed.Text:= '';
            fmConsole.ed.Update(true);
          end;

        LOG_CONSOLE_GET:
          begin
            Result:= PyString_FromString(PChar(fmConsole.ed.Items.Text));
            Exit; //skip ReturnNone
          end;
        LOG_CONSOLE_GET_LOG:
          begin
            Result:= PyUnicode_FromWideString(fmConsole.memo.Text);
            Exit; //skip ReturnNone
          end;
      end;

      Result:= ReturnNone;
    end;
end;


function Py_dlg_hotkey(Self, Args: PPyObject): PPyObject; cdecl;
var
  Ptr: PChar;
  S: string;
begin
  with GetPythonEngine do
    if Bool(PyArg_ParseTuple(Args, 's:dlg_hotkey', @Ptr)) then
    begin
      S:= string(Ptr);
      S:= DoDialogHotkeyInput(S);
      if S<>'' then
        Result:= PyString_FromString(PChar(S))
      else
        Result:= ReturnNone;
    end;
end;

function Py_dlg_hotkeys(Self, Args : PPyObject): PPyObject; cdecl;
var
  PtrCaption, PtrLexer: PChar;
  StrCaption, StrLexer: string;
  Cmd: integer;
  Ok: boolean;
begin
  with GetPythonEngine do
    if Bool(PyArg_ParseTuple(Args, 'ss:dlg_hotkeys', @PtrCaption, @PtrLexer)) then
    begin
      StrCaption:= string(PtrCaption);
      StrLexer:= string(PtrLexer);

      Cmd:= StrToIntDef(StrCaption, 0);
      if Cmd>0 then
        Ok:= DoDialogHotkeys(Cmd, StrLexer)
      else
        Ok:= DoDialogHotkeys(StrCaption, StrLexer);
      Result:= PyBool_FromLong(Ord(Ok));
    end;
end;


function Py_dlg_custom(Self, Args : PPyObject): PPyObject; cdecl;
var
  PtrTitle, PtrText: PChar;
  StrTitle, StrText, StrStateText: string;
  NSizeX, NSizeY, NFocused, NButton: integer;
begin
  with GetPythonEngine do
    if Bool(PyArg_ParseTuple(Args, 'siisi:dlg_custom', @PtrTitle, @NSizeX, @NSizeY, @PtrText, @NFocused)) then
    begin
      StrTitle:= string(PtrTitle);
      StrText:= string(PtrText);
      DoDialogCustom(StrTitle, NSizeX, NSizeY, StrText, NFocused, NButton, StrStateText);
      if NButton<0 then
        Result:= ReturnNone
      else
        Result:= Py_BuildValue('(is)', NButton, PChar(StrStateText));
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
begin
  with GetPythonEngine do
    if Bool(PyArg_ParseTuple(Args, 'Lis:get_prop', @H, @Id, @Ptr)) then
    begin
      Ed:= PyEditor(H);
      Str:= string(Ptr);
      F:= fmMain.GetEditorFrame(Ed);

      case Id of
        PROP_GUTTER_NUM:
          Result:= PyBool_FromLong(Ord(Ed.Gutter[Ed.GutterBandNum].Visible));
        PROP_GUTTER_FOLD:
          Result:= PyBool_FromLong(Ord(Ed.Gutter[Ed.GutterBandFold].Visible));
        PROP_GUTTER_BM:
          Result:= PyBool_FromLong(Ord(Ed.Gutter[Ed.GutterBandBm].Visible));
        PROP_EOL:
          Result:= PyString_FromString(#10);
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

        PROP_LEXER_FILE:
          Result:= PyString_FromString(PChar(F.LexerName));
        PROP_LEXER_POS:
          begin
            Pnt.X:= StrToIntDef(SGetItem(Str, ','), 0);
            Pnt.Y:= StrToIntDef(SGetItem(Str, ','), 0);
            Result:= PyString_FromString(PChar(F.LexerNameAtPos(Pnt)));
          end;
        PROP_LEXER_CARET:
          begin
            Pnt.X:= Ed.Carets[0].PosX;
            Pnt.Y:= Ed.Carets[0].PosY;
            Result:= PyString_FromString(PChar(F.LexerNameAtPos(Pnt)));
          end;

        PROP_TAB_COLOR:
          Result:= PyInt_FromLong(F.TabColor);
        PROP_TAB_TITLE:
          Result:= PyUnicode_FromWideString(Utf8Decode(F.TabCaption));
        PROP_TAB_ID:
          Result:= PyLong_FromLong(F.TabId);

        PROP_ENC:
          Result:= PyUnicode_FromWideString(Utf8Decode(F.EncodingName));

        PROP_INDEX_GROUP:
          begin
            fmMain.GetEditorIndexes(Ed, Num1, Num2);
            Result:= PyInt_FromLong(Num1);
          end;
        PROP_INDEX_TAB:
          begin
            fmMain.GetEditorIndexes(Ed, Num1, Num2);
            Result:= PyInt_FromLong(Num2);
          end;

        PROP_TAG:
          Result:= PyString_FromString(PChar(F.TagString));

        PROP_UNPRINTED_SHOW:
          Result:= PyBool_FromLong(Ord(Ed.OptUnprintedVisible));
        PROP_UNPRINTED_SPACES:
          Result:= PyBool_FromLong(Ord(Ed.OptUnprintedSpaces));
        PROP_UNPRINTED_ENDS:
          Result:= PyBool_FromLong(Ord(Ed.OptUnprintedEnds));
        PROP_UNPRINTED_END_DETAILS:
          Result:= PyBool_FromLong(Ord(Ed.OptUnprintedEndsDetails));

        PROP_TAB_COLLECT_MARKERS:
          Result:= PyBool_FromLong(Ord(F.TabKeyCollectMarkers));

        PROP_MACRO_REC:
          Result:= PyBool_FromLong(Ord(F.MacroRecord));

        PROP_MARKED_RANGE:
          begin
            Ed.DoGetMarkedLines(Num1, Num2);
            Result:= Py_BuildValue('(ii)', Num1, Num2);
          end;

        PROP_CARET_SHAPE:
          Result:= PyInt_FromLong(Ord(Ed.OptCaretShape));
        PROP_CARET_SHAPE_OVR:
          Result:= PyInt_FromLong(Ord(Ed.OptCaretShapeOvr));
        PROP_CARET_SHAPE_RO:
          Result:= PyInt_FromLong(Ord(Ed.OptCaretShapeRO));

        PROP_CARET_VIRTUAL:
          Result:= PyBool_FromLong(Ord(Ed.OptCaretVirtual));

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
        PROP_COLUMN_LEFT:
          Result:= PyInt_FromLong(Ed.ColumnLeft);

        PROP_LINK_AT_POS:
          begin
            Pnt.X:= StrToIntDef(SGetItem(Str), -1);
            Pnt.Y:= StrToIntDef(SGetItem(Str), -1);
            Str:= Ed.DoGetLinkAtPos(Pnt.X, Pnt.Y);
            Result:= PyString_FromString(PChar(Str));
          end;

        PROP_PICTURE:
          begin
            if F.IsText then
              Result:= ReturnNone
            else
            begin
              Pnt:= F.PictureSizes;
              Result:= Py_BuildValue('(sii)', PChar(F.PictureFileName), Pnt.X, Pnt.Y);
            end;
          end;

        else
          Result:= ReturnNone;
      end;
    end;
end;

function Py_ed_set_prop(Self, Args: PPyObject): PPyObject; cdecl;
var
  H: Int64;
  Id: integer;
  Ptr: PChar;
  Str, Str1, Str2: string;
  Ed: TATSynEdit;
  F: TEditorFrame;
  ValueBool: boolean;
  ValueInt, NGroup, NTab, NIndexNew, N1, N2: integer;
begin
  with GetPythonEngine do
    if Bool(PyArg_ParseTuple(Args, 'Lis:set_prop', @H, @Id, @Ptr)) then
    begin
      Ed:= PyEditor(H);
      F:= fmMain.GetEditorFrame(Ed);
      Str:= string(Ptr);

      ValueBool:= Str<>'0';
      ValueInt:= StrToIntDef(Str, 0);

      case Id of
        PROP_GUTTER_NUM:
          Ed.Gutter[Ed.GutterBandNum].Visible:= ValueBool;
        PROP_GUTTER_FOLD:
          Ed.Gutter[Ed.GutterBandFold].Visible:= ValueBool;
        PROP_GUTTER_BM:
          Ed.Gutter[Ed.GutterBandBm].Visible:= ValueBool;
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
            F.UpdateModifiedState;
          end;
        PROP_RULER:
          Ed.OptRulerVisible:= ValueBool;
        PROP_LEXER_FILE:
          F.Lexer:= AppManager.FindAnalyzer(Str);

        PROP_COLOR:
          begin
            Str1:= SGetItem(Str);
            Str2:= SGetItem(Str);
            N1:= StrToIntDef(Str2, -1);
            if N1<>-1 then
              EditorSetColorById(Ed, Str1, N1);
          end;

        PROP_LINE_TOP:
          begin
            F.TopLineTodo:= ValueInt;
            Ed.LineTop:= ValueInt;
          end;
        PROP_COLUMN_LEFT:
          begin
            Ed.ColumnLeft:= ValueInt;
          end;

        PROP_TAB_COLOR:
          F.TabColor:= ValueInt;
        PROP_TAB_TITLE:
          begin
            F.TabCaption:= Str;
            F.TabCaptionFromApi:= true;
          end;

        PROP_ENC:
          F.EncodingName:= Str;

        PROP_TAG:
          F.TagString:= Str;

        PROP_UNPRINTED_SHOW:
          Ed.OptUnprintedVisible:= ValueBool;
        PROP_UNPRINTED_SPACES:
          Ed.OptUnprintedSpaces:= ValueBool;
        PROP_UNPRINTED_ENDS:
          Ed.OptUnprintedEnds:= ValueBool;
        PROP_UNPRINTED_END_DETAILS:
          Ed.OptUnprintedEndsDetails:= ValueBool;

        PROP_TAB_COLLECT_MARKERS:
          F.TabKeyCollectMarkers:= ValueBool;

        PROP_INDEX_GROUP:
          begin
            NIndexNew:= ValueInt+1;
            fmMain.Groups.PagesAndTabIndexOfControl(F, NGroup, NTab);
            if NTab>=0 then
              if NGroup<>NIndexNew then
                if (NIndexNew>=Low(TATGroupsNums)) and (NIndexNew<=High(TATGroupsNums)) then
                  fmMain.Groups.MoveTab(
                    fmMain.Groups.Pages[NGroup], NTab,
                    fmMain.Groups.Pages[NIndexNew], -1, true);
          end;

        PROP_INDEX_TAB:
          begin
            NIndexNew:= ValueInt;
            fmMain.Groups.PagesAndTabIndexOfControl(F, NGroup, NTab);
            if NTab>=0 then
              fmMain.Groups.Pages[NGroup].Tabs.MoveTab(NTab, NIndexNew, true);
          end;

        PROP_CARET_SHAPE:
          Ed.OptCaretShape:= TATSynCaretShape(StrToIntDef(Str, 0));
        PROP_CARET_SHAPE_OVR:
          Ed.OptCaretShapeOvr:= TATSynCaretShape(StrToIntDef(Str, 0));
        PROP_CARET_SHAPE_RO:
          Ed.OptCaretShapeRO:= TATSynCaretShape(StrToIntDef(Str, 0));

        PROP_CARET_VIRTUAL:
          Ed.OptCaretVirtual:= ValueBool;

        PROP_MINIMAP:
          Ed.OptMinimapVisible:= ValueBool;
        PROP_MICROMAP:
          Ed.OptMicromapVisible:= ValueBool;

        PROP_EXPORT_HTML:
          begin
            DoEditorExportToHTML_WithParams(Ed, Str);
          end;

        PROP_MARKED_RANGE:
          begin
            N1:= StrToIntDef(SGetItem(Str), -1);
            N2:= StrToIntDef(SGetItem(Str), -1);
            Ed.DoSetMarkedLines(N1, N2);
          end;
      end;

      fmMain.UpdateFrame;
      fmMain.UpdateStatus;
      Result:= ReturnNone;
    end;
end;

function Py_app_proc(Self, Args: PPyObject): PPyObject; cdecl;
var
  Id, i: integer;
  Ptr: PChar;
  Str, Str1, Str2, Str3: string;
  StrCaption, StrId, StrCmd, StrIndex: string;
  SplitVert, SplitVisible, ok: boolean;
  SplitPos, SplitTotal: integer;
  ParamInt: integer;
  ParamBool: boolean;
  ParamRect: TRect;
  Num64: Int64;
begin
  with GetPythonEngine do
    if Bool(PyArg_ParseTuple(Args, 'is:app_proc', @Id, @Ptr)) then
    begin
      Str:= string(Ptr);
      ParamInt:= StrToIntDef(Str, -1);
      ParamBool:= (Str='1');

      case Id of
        APP_PROC_GET_CLIP:
          begin
            Str:= Clipboard.AsText;
            Result:= PyString_FromString(PChar(Str));
          end;
        APP_PROC_SET_CLIP:
          begin
            Clipboard.AsText:= Str;
            Result:= ReturnNone;
          end;

        APP_PROC_GET_COMMAND:
          begin
            if (AppKeymap.IsIndexValid(ParamInt)) and
              not ((AppKeymap[ParamInt].Command>=cmdFirstPluginCommand) and
                   (AppKeymap[ParamInt].Command<cmdLastPluginCommand)) and
              not ((AppKeymap[ParamInt].Command>=cmdFirstLexerCommand) and
                   (AppKeymap[ParamInt].Command<cmdLastLexerCommand)) then
              with AppKeymap[ParamInt] do
                Result:= Py_BuildValue('(isss)',
                  Command,
                  PChar(Name),
                  PChar(KeyArrayToString(Keys1)),
                  PChar(KeyArrayToString(Keys2))
                  )
            else
              Result:= ReturnNone;
          end;

        APP_PROC_GET_COMMAND_INITIAL:
          begin
            //AppKeymapInitial must be inited with _same_ code
            //as AppKeymap in proc_global
            if not Assigned(AppKeymapInitial) then
            begin
              AppKeymapInitial:= TATKeymap.Create;
              InitKeymapFull(AppKeymapInitial);
              InitKeymapForApplication(AppKeymapInitial);
            end;

            if AppKeymapInitial.IsIndexValid(ParamInt) then
              with AppKeymapInitial[ParamInt] do
                Result:= Py_BuildValue('(isss)',
                  Command,
                  PChar(Name),
                  PChar(KeyArrayToString(Keys1)),
                  PChar(KeyArrayToString(Keys2))
                  )
            else
              Result:= ReturnNone;
          end;

        APP_PROC_GET_COMMAND_PLUGIN:
          begin
            if (ParamInt>=Low(FPluginsCmd)) and
               (ParamInt<=High(FPluginsCmd)) and
               (FPluginsCmd[ParamInt].ItemModule<>'') then
              Result:= Py_BuildValue('(sssss)',
                PChar(FPluginsCmd[ParamInt].ItemCaption),
                PChar(FPluginsCmd[ParamInt].ItemModule),
                PChar(FPluginsCmd[ParamInt].ItemProc),
                PChar(FPluginsCmd[ParamInt].ItemProcParam),
                PChar(FPluginsCmd[ParamInt].ItemLexers)
                )
            else
               Result:= ReturnNone;
          end;

        APP_PROC_SAVE_SESSION:
          begin
            fmMain.DoOps_SaveSession(Str);
            Result:= ReturnNone;
          end;
        APP_PROC_LOAD_SESSION:
          begin
            fmMain.DoOps_LoadSession(Str);
            Result:= ReturnNone;
          end;
        APP_PROC_SET_SESSION:
          begin
            fmMain.FSessionName:= Str;
            fmMain.UpdateCaption;
            Result:= ReturnNone;
          end;

        APP_PROC_MENU_ENUM:
          begin
            Str:= fmMain.DoMenuEnum(Str);
            if Str<>'' then
              Result:= PyString_FromString(PChar(Str))
            else
              Result:= ReturnNone;
          end;

        APP_PROC_MENU_CLEAR:
          begin
            fmMain.DoMenuClear(Str);
            Result:= ReturnNone;
          end;

        APP_PROC_MENU_ADD:
          begin
            Str:= fmMain.DoMenuAdd(Str);
            if Str<>'' then
              Result:= PyString_FromString(PChar(Str))
            else
              Result:= ReturnNone;
          end;

        APP_PROC_SET_EVENTS:
          begin
            StrId:= SGetItem(Str, ';'); //module
            StrCmd:= SGetItem(Str, ';'); //events
            StrCaption:= SGetItem(Str, ';'); //lexers
            StrIndex:= SGetItem(Str, ';'); //keys
            fmMain.DoPyUpdateEvents(StrId, StrCmd, StrCaption, StrIndex);
            Result:= ReturnNone;
          end;

        APP_PROC_SET_SUBCOMMANDS:
          begin
            CommandPlugins_UpdateSubcommands(Str);
            fmMain.UpdateKeymapDynamicItems;
            fmMain.DoOps_LoadKeymap;
            Result:= ReturnNone;
          end;

        APP_PROC_GET_LAST_PLUGIN:
          begin
            Result:= PyString_FromString(PChar(
              PyLastCommandModule+','+PyLastCommandMethod+
                IfThen(PyLastCommandParam<>'', ','+PyLastCommandParam)
              ));
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
            GetPythonEngine.ExecString(Str);
            Result:= ReturnNone;
          end;

        APP_PROC_EXEC_PLUGIN:
          begin
            Str1:= SGetItem(Str);
            Str2:= SGetItem(Str);
            Str3:= Str;
            fmMain.DoPyCommand(Str1, Str2, Str3);
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

        APP_PROC_GET_SPLIT:
          begin
            fmMain.DoGetSplitInfo(Str, SplitVert, SplitVisible, SplitPos, SplitTotal);
            Result:= Py_BuildValue('(iiii)', Ord(SplitVert), Ord(SplitVisible), SplitPos, SplitTotal);
          end;
        APP_PROC_SET_SPLIT:
          begin
            Str1:= SGetItem(Str, ';');
            Str2:= SGetItem(Str, ';');
            fmMain.DoSetSplitInfo(Str1, StrToIntDef(Str2, -1));
            Result:= ReturnNone;
          end;

        APP_PROC_GET_FIND_OPTIONS:
          begin
            Result:= PyString_FromString(PChar(fmMain.DoFindOptionsToString));
          end;
        APP_PROC_SET_FIND_OPTIONS:
          begin
            fmMain.DoFindOptionsFromString(Str);
            Result:= ReturnNone;
          end;
        APP_PROC_GET_FIND_STRINGS:
          begin
            fmMain.DoFindOptions_GetStrings(Str1, Str2);
            Result:= Py_BuildValue('(ss)', PChar(Str1), PChar(Str2));
          end;

        APP_PROC_SIDEPANEL_ADD:
          begin
            StrCaption:= SGetItem(Str); //caption
            StrIndex:= SGetItem(Str); //tab index
            StrId:= SGetItem(Str); //type of control
            ok:= fmMain.DoSidebar_AddTab(StrCaption, StrId, StrToIntDef(StrIndex, -1));
            Result:= PyBool_FromLong(Ord(ok));
          end;

        APP_PROC_SIDEPANEL_REMOVE:
          begin
            StrCaption:= Str;
            ok:= fmMain.DoSidebar_RemoveTab(StrCaption);
            Result:= PyBool_FromLong(Ord(ok));
          end;

        APP_PROC_SIDEPANEL_ACTIVATE:
          begin
            ok:= fmMain.DoSidebar_ActivateTab(Str);
            Result:= PyBool_FromLong(Ord(ok));
          end;

        APP_PROC_SIDEPANEL_ENUM:
          begin
            Str:= '';
            for i:= 0 to fmMain.TabsLeft.TabCount-1 do
              Str:= Str + IfThen(i>0, #10) + fmMain.TabsLeft.GetTabData(i).TabCaption;
            Result:= PyString_FromString(PChar(Str));
          end;

        APP_PROC_SIDEPANEL_GET_CONTROL:
          begin
            Num64:= fmMain.DoSidebar_CaptionToControlHandle(Str);
            if Num64<>0 then
              Result:= PyLong_FromLongLong(Num64)
            else
              Result:= ReturnNone;
          end;

        APP_PROC_BOTTOMPANEL_GET_CONTROL:
          begin
            Num64:= fmMain.DoBottom_CaptionToControlHandle(Str);
            if Num64<>0 then
              Result:= PyLong_FromLongLong(Num64)
            else
              Result:= ReturnNone;
          end;

        APP_PROC_TOOLBAR_ENUM:
          begin
            Str:= '';
            for i:= 0 to fmMain.Toolbar.ButtonCount-1 do
              with fmMain.Toolbar do
                Str:= Str+Format('%s;%s'#10, [Buttons[i].Hint, Buttons[i].Caption]);
            Result:= PyString_FromString(PChar(Str));
          end;

        APP_PROC_TOOLBAR_ADD:
          begin
            fmMain.DoToolbarAddButton(Str);
            fmMain.UpdateToolbar;
            Result:= ReturnNone;
          end;

        APP_PROC_TOOLBAR_DELETE:
          begin
            if (ParamInt>=0) and (ParamInt<fmMain.Toolbar.ButtonCount) then
              fmMain.Toolbar.Buttons[ParamInt].Free;
            fmMain.UpdateToolbar;
            Result:= ReturnNone;
          end;

        APP_PROC_TOOLBAR_DELETE_ALL:
          begin
            for i:= fmMain.Toolbar.ButtonCount-1 downto 0 do
              fmMain.Toolbar.Buttons[i].Free;
            fmMain.UpdateToolbar;
            Result:= ReturnNone;
          end;

        APP_PROC_TOOLBAR_ICON_ADD:
          begin
            if UpdateImagelistWithIconFromFile(fmMain.ImageListBar, Str) then
              Result:= PyInt_FromLong(fmMain.ImageListBar.Count-1)
            else
              Result:= ReturnNone;
          end;

        APP_PROC_TOOLBAR_ICON_SET:
          begin
            Str1:= SGetItem(Str);
            Str2:= SGetItem(Str);
            ParamInt:= StrToIntDef(Str1, -1);
            if (ParamInt>=0) and (ParamInt<fmMain.Toolbar.ButtonCount) then
              fmMain.Toolbar.Buttons[ParamInt].ImageIndex:= StrToIntDef(Str2, -1);
            fmMain.UpdateToolbar;
            Result:= ReturnNone;
          end;

        APP_PROC_TOOLBAR_ICON_GET_SIZE:
          begin
            with fmMain.Toolbar do
              Result:= Py_BuildValue('(ii)', Images.Width, Images.Height);
          end;

        APP_PROC_TOOLBAR_ICON_SET_SIZE:
          begin
            Str1:= SGetItem(Str);
            Str2:= SGetItem(Str);
            with fmMain.Toolbar do
            begin
              Images.Width:= StrToIntDef(Str1, 16);
              Images.Height:= StrToIntDef(Str2, 16);
              SizeIncrementToIcon:= IfThen(Images.Width<=20, 8, 4);
            end;
            fmMain.UpdateToolbar;
            Result:= ReturnNone;
          end;

        APP_PROC_GET_LANG:
          begin
            Result:= PyString_FromString(PChar(AppLangName));
          end;

        APP_PROC_GET_HOTKEY:
          begin
            Result:= PyString_FromString(PChar(GetAppKeymapHotkey(Str)));
          end;
        APP_PROC_SET_HOTKEY:
          begin
            Result:= PyBool_FromLong(Ord(SetAppKeymapHotkey(Str)));
          end;

        APP_PROC_GET_KEYSTATE:
          begin
            Str:= ConvertShiftStateToString(KeyboardStateToShiftState);
            Result:= PyString_FromString(PChar(Str));
          end;

        APP_PROC_THEME_UI_GET:
          begin
            Result:= PyString_FromString(PChar(fmMain.ThemeUi));
          end;
        APP_PROC_THEME_UI_SET:
          begin
            fmMain.ThemeUi:= Str;
            Result:= ReturnNone;
          end;
        APP_PROC_THEME_SYNTAX_GET:
          begin
            Result:= PyString_FromString(PChar(fmMain.ThemeSyntax));
          end;
        APP_PROC_THEME_SYNTAX_SET:
          begin
            fmMain.ThemeSyntax:= Str;
            Result:= ReturnNone;
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
            fmMain.ShowSidePanel:= ParamBool;
            Result:= ReturnNone;
          end;
        APP_PROC_SHOW_SIDEPANEL_GET:
          begin
            Result:= PyBool_FromLong(Ord(fmMain.ShowSidePanel));
          end;

        APP_PROC_SHOW_BOTTOMPANEL_SET:
          begin
            fmMain.ShowBottom:= ParamBool;
            Result:= ReturnNone;
          end;
        APP_PROC_SHOW_BOTTOMPANEL_GET:
          begin
            Result:= PyBool_FromLong(Ord(fmMain.ShowBottom));
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

        APP_PROC_COORD_WINDOW_GET:
          begin
            Result:= Py_rect(Application.MainForm.BoundsRect);
          end;
        APP_PROC_COORD_WINDOW_SET:
          begin
            ParamRect.Left:= StrToIntDef(SGetItem(Str), 0);
            ParamRect.Top:= StrToIntDef(SGetItem(Str), 0);
            ParamRect.Right:= StrToIntDef(SGetItem(Str), 0);
            ParamRect.Bottom:= StrToIntDef(SGetItem(Str), 0);
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

        else
          Result:= ReturnNone;
      end;
    end;
end;


function Py_lexer_proc(Self, Args : PPyObject): PPyObject; cdecl;
var
  Id: integer;
  Ptr: PChar;
  Str, Str1, Str2: string;
  An: TecSyntAnalyzer;
  List: TStringlist;
begin
  with GetPythonEngine do
    if Bool(PyArg_ParseTuple(Args, 'is:lexer_proc', @Id, @Ptr)) then
    begin
      Str:= string(Ptr);
      Str1:= SGetItem(Str, ';');
      Str2:= SGetItem(Str, ';');

      case Id of
        LEXER_GET_ENABLED:
          begin
            An:= AppManager.FindAnalyzer(Str1);
            if Assigned(An) then
              Result:= PyBool_FromLong(Ord(not An.Internal))
            else
              Result:= ReturnNone;
          end;

        LEXER_GET_EXT:
          begin
            An:= AppManager.FindAnalyzer(Str1);
            if Assigned(An) then
              Result:= PyString_FromString(PChar(string(An.Extentions)))
            else
              Result:= ReturnNone;
          end;

        LEXER_GET_COMMENT:
          begin
            An:= AppManager.FindAnalyzer(Str1);
            if Assigned(An) then
              Result:= PyString_FromString(PChar(string(An.LineComment)))
            else
              Result:= ReturnNone;
          end;

        LEXER_GET_COMMENT_STREAM:
          begin
            An:= AppManager.FindAnalyzer(Str1);
            if Assigned(An) then
            begin
              Str1:= GetAppLexerPropInCommentsSection(An.LexerName, 'str1');
              Str2:= GetAppLexerPropInCommentsSection(An.LexerName, 'str2');
              if (Str1<>'') and (Str2<>'') then
                Result:= Py_BuildValue('(ss)', PChar(Str1), PChar(Str2))
              else
                Result:= ReturnNone;
            end
            else
              Result:= ReturnNone;
          end;

        LEXER_GET_COMMENT_LINED:
          begin
            An:= AppManager.FindAnalyzer(Str1);
            if Assigned(An) then
            begin
              Str1:= GetAppLexerPropInCommentsSection(An.LexerName, 'full1');
              Str2:= GetAppLexerPropInCommentsSection(An.LexerName, 'full2');
              if (Str1<>'') and (Str2<>'') then
                Result:= Py_BuildValue('(ss)', PChar(Str1), PChar(Str2))
              else
                Result:= ReturnNone;
            end
            else
              Result:= ReturnNone;
          end;

        LEXER_GET_STYLES_COMMENTS:
          begin
            An:= AppManager.FindAnalyzer(Str1);
            if Assigned(An) then
            begin
              Str1:= GetAppLexerPropInCommentsSection(An.LexerName, 'styles_cmt');
              Result:= PyString_FromString(PChar(Str1));
            end
            else
              Result:= ReturnNone;
          end;

        LEXER_GET_STYLES_STRINGS:
          begin
            An:= AppManager.FindAnalyzer(Str1);
            if Assigned(An) then
            begin
              Str1:= GetAppLexerPropInCommentsSection(An.LexerName, 'styles_str');
              Result:= PyString_FromString(PChar(Str1));
            end
            else
              Result:= ReturnNone;
          end;

        LEXER_SET_ENABLED:
          begin
            An:= AppManager.FindAnalyzer(Str1);
            if Assigned(An) then
              An.Internal:= Str2='0';
            Result:= ReturnNone;

            AppManager.Modified:= true;
            fmMain.UpdateMenuLexers;
          end;

        LEXER_SET_NAME:
          begin
            An:= AppManager.FindAnalyzer(Str1);
            if Assigned(An) then
              An.LexerName:= Str2;
            Result:= ReturnNone;

            AppManager.Modified:= true;
            fmMain.UpdateMenuLexers;
          end;

        LEXER_SET_EXT:
          begin
            An:= AppManager.FindAnalyzer(Str1);
            if Assigned(An) then
              An.Extentions:= Str2;
            Result:= ReturnNone;

            AppManager.Modified:= true;
          end;

        LEXER_DELETE:
          begin
            An:= AppManager.FindAnalyzer(Str1);
            if Assigned(An) then
              An.Free;
            Result:= ReturnNone;

            AppManager.Modified:= true;
            fmMain.UpdateMenuLexers;
          end;

        LEXER_IMPORT:
          begin
            if FileExists(Str1) then
            begin
              An:= AppManager.AddAnalyzer;
              An.LoadFromFile(Str1);
              Result:= PyString_FromString(PChar(string(An.LexerName)));

              AppManager.Modified:= true;
              fmMain.UpdateMenuLexers;
            end
            else
              Result:= ReturnNone;
          end;

        LEXER_GET_LIST:
          begin
            List:= TStringList.Create;
            List.Sorted:= true;
            try
              DoLexerEnum(List, true);
              Result:= PyString_FromString(PChar(List.Text));
            finally
              FreeAndNil(List);
            end;
          end;

        LEXER_GET_LINKS:
          begin
            An:= AppManager.FindAnalyzer(Str1);
            if Assigned(An) then
            begin
              List:= TStringList.Create;
              try
                LexerEnumSublexers(An, List);
                Result:= PyString_FromString(PChar(List.Text));
              finally
                FreeAndNil(List);
              end;
            end
            else
              Result:= ReturnNone;
          end;

        LEXER_GET_STYLES:
          begin
            An:= AppManager.FindAnalyzer(Str1);
            if Assigned(An) then
            begin
              List:= TStringList.Create;
              try
                LexerEnumStyles(An, List);
                Result:= PyString_FromString(PChar(List.Text));
              finally
                FreeAndNil(List);
              end;
            end
            else
              Result:= ReturnNone;
          end;

        LEXER_SET_LINKS:
          begin
            An:= AppManager.FindAnalyzer(Str1);
            if Assigned(An) then
              LexerSetSublexers(AppManager, An, Str2);
            Result:= ReturnNone;
            AppManager.Modified:= true;
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
  Id, X, Y: integer;
  Ptr: PChar;
  Str: atString;
begin
  with GetPythonEngine do
    if Bool(PyArg_ParseTuple(Args, 'Liiis:convert', @H, @Id, @X, @Y, @Ptr)) then
    begin
      Ed:= PyEditor(H);
      Str:= string(Ptr);

      if (X<0) or (not Ed.Strings.IsIndexValid(Y)) then
      begin
        Result:= ReturnNone;
        exit
      end;

      case Id of
        CONVERT_CHAR_TO_COL:
          begin
            Str:= Ed.Strings.Lines[Y];
            X:= SCharPosToColumnPos(Str, X, Ed.OptTabSize);
            Result:= Py_BuildValue('(ii)', X, Y);
          end;

        CONVERT_COL_TO_CHAR:
          begin
            Str:= Ed.Strings.Lines[Y];
            X:= SColumnPosToCharPos(Str, X, Ed.OptTabSize);
            Result:= Py_BuildValue('(ii)', X, Y);
          end;

        CONVERT_LINE_TABS_TO_SPACES:
          begin
            Str:= STabsToSpaces(Str, Ed.OptTabSize);
            Result:= PyUnicode_FromWideString(Str);
          end;

        else
          Result:= ReturnNone;
      end;
    end;
end;


function Py_ed_get_ranges(Self, Args: PPyObject): PPyObject; cdecl;
var
  H: Int64;
  Ed: TATSynEdit;
  ComArray: Variant;
  NLen, i: Integer;
  Range: TATSynRange;
begin
  with GetPythonEngine do
    if Bool(PyArg_ParseTuple(Args, 'L:get_ranges', @H)) then
    begin
      Ed:= PyEditor(H);
      NLen:= Ed.Fold.Count;
      if NLen>0 then
      begin
        ComArray:= VarArrayCreate([0, NLen-1, 0, 4], varInteger);
        for i:= 0 to NLen-1 do
        begin
          Range:= Ed.Fold[i];
          ComArray[i, 0]:= Range.Y;
          ComArray[i, 1]:= Range.Y2;
          ComArray[i, 2]:= Range.X-1;
          ComArray[i, 3]:= Ord(Range.Staple);
          ComArray[i, 4]:= Ord(Range.Folded);
        end;
        Result:= VariantAsPyObject(ComArray);
      end
      else
        Result:= ReturnNone;
    end;
end;


function Py_ed_get_sublexer_ranges(Self, Args: PPyObject): PPyObject; cdecl;
var
  H: Int64;
  Ed: TATSynEdit;
  Frame: TEditorFrame;
  NLen, i: Integer;
  PntStart, PntEnd: TPoint;
  LexName, Str: string;
begin
  with GetPythonEngine do
    if Bool(PyArg_ParseTuple(Args, 'L:get_sublexer_ranges', @H)) then
    begin
      Ed:= PyEditor(H);
      Frame:= fmMain.GetEditorFrame(Ed);

      NLen:= Frame.Adapter.SublexerRangeCount;
      if NLen>0 then
      begin
        Str:= '';
        for i:= 0 to NLen-1 do
          if Frame.Adapter.SublexerRangeProps(i, PntStart, PntEnd, LexName) then
            Str:= Str + Format('%d,%d,%d,%d,%s;',
              [ PntStart.X, PntStart.Y,
                PntEnd.X, PntEnd.Y,
                LexName ]);
        Result:= PyString_FromString(PChar(Str));
      end
      else
        Result:= ReturnNone;
    end;
end;


//tree_proc(id_tree, id_action, id_item, index, text, icon)
function Py_tree_proc(Self, Args: PPyObject): PPyObject; cdecl;
var
  IdTree, IdItem: Int64;
  IdAction, NIndex, NImageIndex: integer;
  Pnt1, Pnt2: TPoint;
  PtrText: PChar;
  StrText, Str: string;
  Tree: TTreeView;
  TreeNode, Node1: TTreeNode;
  ok: boolean;
begin
  with GetPythonEngine do
    if Bool(PyArg_ParseTuple(Args, 'LiLisi:tree_proc', @IdTree, @IdAction, @IdItem, @NIndex, @PtrText, @NImageIndex)) then
    begin
      StrText:= string(PtrText);
      Tree:= TTreeView(PtrInt(IdTree));
      TreeNode:= TTreeNode(PtrInt(IdItem));

      case IdAction of
        TREE_ITEM_ENUM:
          begin
            if Assigned(TreeNode) then
              Node1:= TreeNode.GetFirstChild
            else
              Node1:= Tree.Items.GetFirstNode;
            if Node1=nil then exit(ReturnNone);

            Str:= '';
            repeat
              Str:= Str+(IntToStr(PtrInt(Node1))+'='+Node1.Text+#10);
              Node1:= TreeNode.GetNextChild(Node1);
            until Node1=nil;

            Result:= PyString_FromString(PChar(Str));
          end;

        TREE_ITEM_ADD:
          begin
            Node1:= Tree.Items.AddChild(TreeNode, StrText);
            Node1.Text:= StrText;
            Node1.ImageIndex:= NImageIndex;
            Node1.SelectedIndex:= NImageIndex;
            if NIndex>=0 then
              Node1.Index:= NIndex;

            Tree.Invalidate;
            Result:= PyLong_FromLongLong(PtrInt(Node1));
          end;

        TREE_ITEM_DELETE:
          begin
            if Assigned(TreeNode) then
              TreeNode.Free
            else
              Tree.Items.Clear;

            Tree.Invalidate;
            Result:= ReturnNone;
          end;

        TREE_ITEM_SET_TEXT:
          begin
            if Assigned(TreeNode) then
              TreeNode.Text:= StrText;
            Tree.Invalidate;
            Result:= ReturnNone;
          end;

        TREE_ITEM_SET_ICON:
          begin
            if Assigned(TreeNode) then
            begin
              TreeNode.ImageIndex:= NImageIndex;
              TreeNode.SelectedIndex:= NImageIndex;
            end;
            Tree.Invalidate;
            Result:= ReturnNone;
          end;

        TREE_ITEM_SELECT:
          begin
            if Assigned(TreeNode) then
            begin
              TreeNode.Selected:= true;
              TreeNode.MakeVisible;
              Tree.Invalidate;
            end;
            Result:= ReturnNone;
          end;

        TREE_ITEM_GET_SELECTED:
          begin
            if Assigned(Tree.Selected) then
              Result:= PyLong_FromLongLong(PtrInt(Tree.Selected))
            else
              Result:= ReturnNone;
          end;

        TREE_ITEM_GET_PROP:
          begin
            if Assigned(TreeNode) then
              Result:= Py_BuildValue('(siii)',
                       PChar(TreeNode.Text),
                       TreeNode.Index,
                       TreeNode.ImageIndex,
                       TreeNode.Level)
            else
              Result:= ReturnNone;
          end;

        TREE_ITEM_GET_PARENT:
          begin
            if Assigned(TreeNode) then
              Result:= PyLong_FromLongLong(PtrInt(TreeNode.Parent))
            else
              Result:= ReturnNone;
          end;

        TREE_ITEM_FOLD:
          begin
            if Assigned(TreeNode) then
              TreeNode.Collapse(false);
            Tree.Invalidate;
            Result:= ReturnNone;
          end;
        TREE_ITEM_FOLD_DEEP:
          begin
            if Assigned(TreeNode) then
              TreeNode.Collapse(true)
            else
              Tree.FullCollapse;
            Tree.Invalidate;
            Result:= ReturnNone;
          end;

        TREE_ITEM_UNFOLD:
          begin
            if Assigned(TreeNode) then
              TreeNode.Expand(false);
            Tree.Invalidate;
            Result:= ReturnNone;
          end;
        TREE_ITEM_UNFOLD_DEEP:
          begin
            if Assigned(TreeNode) then
              TreeNode.Expand(true)
            else
              Tree.FullExpand;
            Tree.Invalidate;
            Result:= ReturnNone;
          end;

        TREE_ITEM_GET_SYNTAX_RANGE:
          begin
            fmMain.TreeGetSyntaxRange(TreeNode, Pnt1, Pnt2);
            Result:= Py_BuildValue('(iiii)', Pnt1.X, Pnt1.Y, Pnt2.X, Pnt2.Y);
          end;

        TREE_ICON_ADD:
          begin
            ok:= UpdateImagelistWithIconFromFile(Tree.Images, StrText);
            if ok then
              Result:= PyInt_FromLong(Tree.Images.Count-1)
            else
              Result:= PyInt_FromLong(-1);
          end;

        TREE_ICON_DELETE:
          begin
            ok:= Assigned(Tree.Images) and (NImageIndex>=0) and (NImageIndex<Tree.Images.Count);
            if ok then
            begin
              Tree.Images.Delete(NImageIndex);
              Tree.Invalidate;
            end;
            Result:= PyBool_FromLong(Ord(ok));
          end;

        TREE_LOCK:
          begin
            Tree.Items.BeginUpdate;
            Result:= ReturnNone;
          end;
        TREE_UNLOCK:
          begin
            Tree.Items.EndUpdate;
            Result:= ReturnNone;
          end;

        TREE_PROP_SHOW_ROOT:
          begin
            Tree.ShowRoot:= StrText<>'0';
            Tree.Invalidate;
            Result:= ReturnNone;
          end;

        else
          Result:= ReturnNone;
      end;
    end;
end;

function Py_ed_get_token_result(const PntFrom, PntTo: TPoint; const StrToken, StrStyle: string): PPyObject;
begin
  with GetPythonEngine do
    if PntFrom.X<0 then
      Result:= ReturnNone
    else
      Result:= Py_BuildValue('((ii)(ii)ss)',
        PntFrom.X, PntFrom.Y,
        PntTo.X, PntTo.Y,
        PChar(StrToken), PChar(StrStyle));
end;

function Py_ed_get_token(Self, Args: PPyObject): PPyObject; cdecl;
var
  H: Int64;
  Ed: TATSynEdit;
  F: TEditorFrame;
  Id, PosX, PosY: integer;
  PntFrom, PntTo: TPoint;
  StrToken, StrStyle: string;
begin
  with GetPythonEngine do
    if Bool(PyArg_ParseTuple(Args, 'Liii:get_token', @H, @Id, @PosX, @PosY)) then
    begin
      Ed:= PyEditor(H);
      F:= fmMain.GetEditorFrame(Ed);

      case Id of
        TOKEN_AT_POS:
          begin
            F.Adapter.GetTokenAtPos(Point(PosX, PosY), PntFrom, PntTo, StrToken, StrStyle);
            Result:= Py_ed_get_token_result(PntFrom, PntTo, StrToken, StrStyle);
          end;
        TOKEN_INDEX:
          begin
            F.Adapter.GetTokenWithIndex(PosX, PntFrom, PntTo, StrToken, StrStyle);
            Result:= Py_ed_get_token_result(PntFrom, PntTo, StrToken, StrStyle);
          end;
        else
          Result:= ReturnNone;
      end;
    end;
end;

//func at end (uses funcs above)
procedure TfmMain.PythonModInitialization(Sender: TObject);
begin
  with Sender as TPythonModule do
  begin
    AddMethod('app_exe_version', @Py_app_exe_version, '');
    AddMethod('app_api_version', @Py_app_api_version, '');
    AddMethod('app_path', @Py_app_path, '');
    AddMethod('app_proc', @Py_app_proc, '');
    AddMethod('app_log', @Py_app_log, '');
    AddMethod('app_idle', @Py_app_idle, '');

    AddMethod('msg_status', @Py_msg_status, '');
    AddMethod('msg_status_alt', @Py_msg_status_alt, '');
    AddMethod('msg_box', @Py_msg_box, '');
    AddMethod('dlg_input', @Py_dlg_input, '');
    AddMethod('dlg_input_ex', @Py_dlg_input_ex, '');
    AddMethod('dlg_file', @Py_dlg_file, '');
    AddMethod('dlg_dir', @Py_dlg_dir, '');
    AddMethod('dlg_menu', @Py_dlg_menu, '');
    AddMethod('dlg_color', @Py_dlg_color, '');
    AddMethod('dlg_hotkey', @Py_dlg_hotkey, '');
    AddMethod('dlg_hotkeys', @Py_dlg_hotkeys, '');
    AddMethod('dlg_custom', @Py_dlg_custom, '');

    AddMethod('ed_get_carets', @Py_ed_get_carets, '');
    AddMethod('ed_set_caret', @Py_ed_set_caret, '');

    AddMethod('ed_get_sel_mode', @Py_ed_get_sel_mode, '');
    AddMethod('ed_get_sel_rect', @Py_ed_get_sel_rect, '');
    AddMethod('ed_get_sel_lines', @Py_ed_get_sel_lines, '');
    AddMethod('ed_set_sel_rect', @Py_ed_set_sel_rect, '');

    AddMethod('ed_set_text_all', @Py_ed_set_text_all, '');
    AddMethod('ed_get_text_sel', @Py_ed_get_text_sel, '');
    Addmethod('ed_get_text_line', @Py_ed_get_text_line, '');
    Addmethod('ed_set_text_line', @Py_ed_set_text_line, '');
    Addmethod('ed_get_text_substr', @Py_ed_get_text_substr, '');
    Addmethod('ed_get_line_count', @Py_ed_get_line_count, '');
    AddMethod('ed_delete', @Py_ed_delete, '');
    AddMethod('ed_insert', @Py_ed_insert, '');

    Addmethod('ed_get_filename', @Py_ed_get_filename, '');
    AddMethod('ed_get_split', @Py_ed_get_split, '');
    AddMethod('ed_set_split', @Py_ed_set_split, '');
    AddMethod('ed_get_prop', @Py_ed_get_prop, '');
    AddMethod('ed_set_prop', @Py_ed_set_prop, '');
    AddMethod('ed_get_ranges', @Py_ed_get_ranges, '');
    AddMethod('ed_get_sublexer_ranges', @Py_ed_get_sublexer_ranges, '');
    AddMethod('ed_get_token', @Py_ed_get_token, '');

    AddMethod('ed_save', @Py_ed_save, '');
    AddMethod('ed_cmd', @Py_ed_cmd, '');
    AddMethod('ed_lock', @Py_ed_lock, '');
    AddMethod('ed_unlock', @Py_ed_unlock, '');
    AddMethod('ed_bookmark', @Py_ed_bookmark, '');
    AddMethod('ed_focus', @Py_ed_focus, '');
    AddMethod('ed_complete', @Py_ed_complete, '');
    AddMethod('ed_complete_alt', @Py_ed_complete_alt, '');
    AddMethod('ed_convert', @Py_ed_convert, '');
    AddMethod('ed_markers', @Py_ed_markers, '');
    AddMethod('ed_attr', @Py_ed_attr, '');

    AddMethod('file_open', @Py_file_open, '');
    AddMethod('file_save', @Py_file_save, '');
    AddMethod('ed_handles', @Py_ed_handles, '');
    AddMethod('ed_group', @Py_ed_group, '');
    AddMethod('ini_read', @Py_ini_read, '');
    AddMethod('ini_write', @Py_ini_write, '');
    AddMethod('lexer_proc', @Py_lexer_proc, '');
    AddMethod('tree_proc', @Py_tree_proc, '');
  end;
end;


