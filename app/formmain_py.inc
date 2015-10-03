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

//func at begin
function PyEditor(Handle: PtrInt): TATSynEdit;
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
        Result:= TATSynEdit(Handle);
    end;
  end;
end;

procedure TfmMain.PythonIOSendData(Sender: TObject;
  const Data: AnsiString);
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


function Py_app_version(Self, Args : PPyObject): PPyObject; cdecl;
begin
  with GetPythonEngine do
    Result:= PyString_FromString(cAppVersion);
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
        else
          Result:= ReturnNone;
      end;
end;


function Py_dlg_input(Self, Args : PPyObject): PPyObject; cdecl;
var
  P1, P2: PAnsiChar;
  StrCaption, StrVal: string;
begin
  with GetPythonEngine do
    if Bool(PyArg_ParseTuple(Args, 'ss:dlg_input', @P1, @P2)) then
    begin
      StrCaption:= AnsiString(P1);
      StrVal:= AnsiString(P2);
      if InputQuery(msgTitle, StrCaption, StrVal) then
        Result:= PyString_FromString(PChar(StrVal))
      else
        Result:= ReturnNone;
    end;
end;

function Py_dlg_input_ex(Self, Args : PPyObject): PPyObject; cdecl;
var
  Number, i: integer;
  PCaption: PAnsiChar;
  PLabel, PText: array[1..10] of PAnsiChar;
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
      SCaption:= AnsiString(PCaption);
      SetLength(Labels, Number);
      SetLength(Values, Number);
      for i:= 1 to Number do
      begin
        Labels[i-1]:= AnsiString(PLabel[i]);
        Values[i-1]:= AnsiString(PText[i]);
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


function Py_msg_status(Self, Args: PPyObject): PPyObject; cdecl;
var
  P: PAnsiChar;
  Str: string;
begin
  with GetPythonEngine do
  begin
    if Bool(PyArg_ParseTuple(Args, 's:msg_status', @P)) then
    begin
      Str:= AnsiString(P);
      fmMain.MsgStatus(Str);
      Application.ProcessMessages; //PluginManager calls loop with msg_status
    end;
    Result:= ReturnNone;
  end;
end;

function Py_msg_box(Self, Args: PPyObject): PPyObject; cdecl;
var
  Ptr: PAnsiChar;
  Str: string;
  Flags: integer;
begin
  with GetPythonEngine do
  begin
    if Bool(PyArg_ParseTuple(Args, 'si:msg_box', @Ptr, @Flags)) then
    begin
      Str:= AnsiString(Ptr);
      Result:= PyInt_FromLong(MsgBox(Str, Flags));
    end;
  end;
end;


function Py_file_open(Self, Args: PPyObject): PPyObject; cdecl;
var
  P: PAnsiChar;
  Str: string;
  Ok: boolean;
begin
  with GetPythonEngine do
  begin
    if Bool(PyArg_ParseTuple(Args, 's:file_open', @P)) then
    begin
      Str:= AnsiString(P);
      Ok:= fmMain.DoFileOpen(Str)<>nil;
      Result:= PyBool_FromLong(Ord(Ok));
    end;
  end;
end;

function Py_file_save(Self, Args: PPyObject): PPyObject; cdecl;
begin
  with GetPythonEngine do
  begin
    fmMain.CurrentFrame.DoFileSave(false, fmMain.SaveDlg);
    Result:= ReturnNone;
  end;
end;

function Py_ed_get_text_all(Self, Args: PPyObject): PPyObject; cdecl;
var
  H: PtrInt;
begin
  with GetPythonEngine do
    if Bool(PyArg_ParseTuple(Args, 'L:get_text_all', @H)) then
      Result:= PyUnicode_FromWideString(PyEditor(H).Text);
end;

function Py_ed_get_text_sel(Self, Args: PPyObject): PPyObject; cdecl;
var
  H: PtrInt;
begin
  with GetPythonEngine do
    if Bool(PyArg_ParseTuple(Args, 'L:get_text_sel', @H)) then
      Result:= PyUnicode_FromWideString(PyEditor(H).TextSelected);
end;

function Py_ed_get_text_line(Self, Args: PPyObject): PPyObject; cdecl;
var
  H: PtrInt;
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
  H: PtrInt;
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
  H: PtrInt;
  X1, Y1, X2, Y2: Integer;
  Ed: TATSynEdit;
begin
  with GetPythonEngine do
    if Bool(PyArg_ParseTuple(Args, 'Liiii:set_caret', @H, @X1, @Y1, @X2, @Y2)) then
    begin
      Ed:= PyEditor(H);

      Ed.DoSelect_None;
      Ed.Carets.Clear;
      Ed.Carets.Add(X2, Y2, X1, Y1);
      Ed.DoGotoCaret(cEdgeTop);

      fmMain.UpdateFrame;
      fmMain.UpdateStatus;
      Result:= ReturnNone;
    end;
end;

function Py_ed_add_caret(Self, Args: PPyObject): PPyObject; cdecl;
var
  H: PtrInt;
  X1, Y1, X2, Y2: Integer;
  Ed: TATSynEdit;
begin
  with GetPythonEngine do
    if Bool(PyArg_ParseTuple(Args, 'Liiii:add_caret', @H, @X1, @Y1, @X2, @Y2)) then
    begin
      Ed:= PyEditor(H);
      if X1<0 then
        Ed.Carets.Clear
      else
      begin
        Ed.Carets.Add(X2, Y2, X1, Y1);
        Ed.Carets.Sort;
      end;

      fmMain.UpdateFrame;
      fmMain.UpdateStatus;
      Result:= ReturnNone;
    end;
end;


function Py_ed_get_carets(Self, Args: PPyObject): PPyObject; cdecl;
var
  H: PtrInt;
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


function Py_ed_get_sel_mode(Self, Args: PPyObject): PPyObject; cdecl;
var
  H: PtrInt;
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
  H: PtrInt;
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
  H: PtrInt;
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
  H: PtrInt;
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
  H: PtrInt;
  P: PAnsiChar;
  Ed: TATSynEdit;
  StrW: atString;
begin
  with GetPythonEngine do
    if Bool(PyArg_ParseTuple(Args, 'Ls:set_text_all', @H, @P)) then
    begin
      Ed:= PyEditor(H);
      Ed.DoSelect_None;
      Ed.Carets.Clear;
      Ed.Carets.Add(0, 0);

      StrW:= UTF8Decode(AnsiString(P));
      Ed.Strings.LoadFromString(StrW);

      fmMain.UpdateFrame(true);
      fmMain.UpdateStatus;
      Result:= ReturnNone;
    end;
end;

function Py_ed_set_text_line(Self, Args: PPyObject): PPyObject; cdecl;
var
  H: PtrInt;
  N: Integer;
  Ptr: PAnsiChar;
  Ed: TATSynEdit;
  Str: atString;
begin
  with GetPythonEngine do
    if Bool(PyArg_ParseTuple(Args, 'Lis:set_text_line', @H, @N, @Ptr)) then
    begin
      Ed:= PyEditor(H);
      Str:= UTF8Decode(AnsiString(Ptr));
      if N=-1 then
        Ed.Strings.LineAdd(Str)
      else
      if Ed.Strings.IsIndexValid(N) then
        Ed.Strings.Lines[N]:= Str;

      fmMain.UpdateFrame(true);
      fmMain.UpdateStatus;
      Result:= ReturnNone;
    end;
end;


function Py_ed_delete(Self, Args: PPyObject): PPyObject; cdecl;
var
  H: PtrInt;
  X1, Y1, X2, Y2: Integer;
  Ed: TATSynEdit;
  Shift, PosAfter: TPoint;
begin
  with GetPythonEngine do
    if Bool(PyArg_ParseTuple(Args, 'Liiii:delete', @H, @X1, @Y1, @X2, @Y2)) then
    begin
      Ed:= PyEditor(H);
      Ed.Strings.TextDeleteRange(X1, Y1, X2, Y2, Shift, PosAfter);
      fmMain.UpdateFrame(true);
      fmMain.UpdateStatus;
      Result:= ReturnNone;
    end;
end;

function Py_ed_insert(Self, Args: PPyObject): PPyObject; cdecl;
var
  H: PtrInt;
  X1, Y1: Integer;
  Ptr: PAnsiChar;
  Str: atString;
  Ed: TATSynEdit;
  Shift, PosAfter: TPoint;
begin
  with GetPythonEngine do
    if Bool(PyArg_ParseTuple(Args, 'Liis:insert', @H, @X1, @Y1, @Ptr)) then
    begin
      Ed:= PyEditor(H);
      Str:= UTF8Decode(AnsiString(Ptr));
      Ed.Strings.TextInsert(X1, Y1, Str, false, Shift, PosAfter);

      fmMain.UpdateFrame(true);
      fmMain.UpdateStatus;
      Result:= Py_BuildValue('(ii)', PosAfter.X, PosAfter.Y);
    end;
end;


function Py_ed_get_line_count(Self, Args: PPyObject): PPyObject; cdecl;
var
  H: PtrInt;
begin
  with GetPythonEngine do
    if Bool(PyArg_ParseTuple(Args, 'L:get_line_count', @H)) then
    begin
      Result:= PyInt_FromLong(PyEditor(H).Strings.Count);
    end;
end;

function Py_ed_get_line_state(Self, Args: PPyObject): PPyObject; cdecl;
var
  H: PtrInt;
  N: Integer;
  Ed: TATSynEdit;
begin
  with GetPythonEngine do
    if Bool(PyArg_ParseTuple(Args, 'Li:get_line_state', @H, @N)) then
    begin
      Ed:= PyEditor(H);
      if Ed.Strings.IsIndexValid(N) then
        Result:= PyInt_FromLong(Ord(Ed.Strings.LinesState[N]))
      else
        Result:= ReturnNone;
    end;
end;


function Py_ed_get_filename(Self, Args: PPyObject): PPyObject; cdecl;
var
  H: PtrInt;
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

function Py_ed_get_tabcolor(Self, Args: PPyObject): PPyObject; cdecl;
var
  H: PtrInt;
  Ed: TATSynEdit;
  F: TEditorFrame;
begin
  with GetPythonEngine do
    if Bool(PyArg_ParseTuple(Args, 'L:get_tabcolor', @H)) then
    begin
      Ed:= PyEditor(H);
      F:= fmMain.GetEditorFrame(Ed);
      Result:= PyInt_FromLong(F.TabColor);
    end;
end;

function Py_ed_set_tabcolor(Self, Args: PPyObject): PPyObject; cdecl;
var
  H: PtrInt;
  Val: Integer;
  Ed: TATSynEdit;
  F: TEditorFrame;
begin
  with GetPythonEngine do
    if Bool(PyArg_ParseTuple(Args, 'Li:set_tabcolor', @H, @Val)) then
    begin
      Ed:= PyEditor(H);
      F:= fmMain.GetEditorFrame(Ed);
      F.TabColor:= Val;
      Result:= ReturnNone;
    end;
end;


function Py_ed_get_enc(Self, Args: PPyObject): PPyObject; cdecl;
var
  H: PtrInt;
  Ed: TATSynEdit;
  F: TEditorFrame;
begin
  with GetPythonEngine do
    if Bool(PyArg_ParseTuple(Args, 'L:get_enc', @H)) then
    begin
      Ed:= PyEditor(H);
      F:= fmMain.GetEditorFrame(Ed);
      Result:= PyUnicode_FromWideString(Utf8Decode(F.EncodingName));
    end;
end;

function Py_ed_set_enc(Self, Args: PPyObject): PPyObject; cdecl;
var
  H: PtrInt;
  Ptr: PAnsiChar;
  Ed: TATSynEdit;
  F: TEditorFrame;
  Str: string;
begin
  with GetPythonEngine do
    if Bool(PyArg_ParseTuple(Args, 'Ls:set_enc', @H, @Ptr)) then
    begin
      Ed:= PyEditor(H);
      Str:= AnsiString(Ptr);
      F:= fmMain.GetEditorFrame(Ed);

      F.EncodingName:= Str;

      fmMain.UpdateFrame();
      fmMain.UpdateStatus;
      Result:= ReturnNone;
    end;
end;

function Py_ed_get_top(Self, Args: PPyObject): PPyObject; cdecl;
var
  H: PtrInt;
  Ed: TATSynEdit;
begin
  with GetPythonEngine do
    if Bool(PyArg_ParseTuple(Args, 'L:get_top', @H)) then
    begin
      Ed:= PyEditor(H);
      Result:= PyInt_FromLong(Ed.LineTop);
    end;
end;

function Py_ed_set_top(Self, Args: PPyObject): PPyObject; cdecl;
var
  H: PtrInt;
  Val: integer;
  Ed: TATSynEdit;
begin
  with GetPythonEngine do
    if Bool(PyArg_ParseTuple(Args, 'Li:set_top', @H, @Val)) then
    begin
      Ed:= PyEditor(H);
      Ed.LineTop:= Val;
      fmMain.UpdateFrame();
      fmMain.UpdateStatus;
      Result:= ReturnNone;
    end;
end;

function Py_ed_cmd(Self, Args: PPyObject): PPyObject; cdecl;
var
  H: PtrInt;
  Val: integer;
  Ed: TATSynEdit;
begin
  with GetPythonEngine do
    if Bool(PyArg_ParseTuple(Args, 'Li:cmd', @H, @Val)) then
    begin
      Ed:= PyEditor(H);
      Ed.DoCommand(Val);
      fmMain.UpdateStatus;
      Result:= ReturnNone;
    end;
end;

function Py_ed_lock(Self, Args: PPyObject): PPyObject; cdecl;
var
  H: PtrInt;
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
  H: PtrInt;
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
  H: PtrInt;
  Ed: TATSynEdit;
  F: TEditorFrame;
begin
  with GetPythonEngine do
    if Bool(PyArg_ParseTuple(Args, 'L:focus', @H)) then
    begin
      Ed:= PyEditor(H);
      F:= fmMain.GetEditorFrame(Ed);
      fmMain.SetFrame(F);
      Ed.SetFocus;

      fmMain.UpdateFrame();
      fmMain.UpdateStatus;
      Result:= ReturnNone;
    end;
end;


function Py_ed_bookmark(Self, Args: PPyObject): PPyObject; cdecl;
var
  H: PtrInt;
  Ptr: PAnsiChar;
  Str: string;
  Ed: TATSynEdit;
  NId, NLine, NKind, NColor: integer;
  Bmp: TBitmap;
  ComArray: Variant;
  List: TList;
  i: Integer;
begin
  with GetPythonEngine do
    if Bool(PyArg_ParseTuple(Args, 'Liiiis:bookmark', @H, @NId, @NLine, @NKind, @NColor, @Ptr)) then
    begin
      Ed:= PyEditor(H);
      Str:= AnsiString(Ptr);
      //limit NKind
      NKind:= Max(Low(AppBookmarkSetup), Min(High(AppBookmarkSetup), NKind));

      case NId of
        BOOKMARK_GET:
          begin
            Result:= PyInt_FromLong(Ed.Strings.LinesBm[NLine]);
          end;

        BOOKMARK_SET:
          begin
            EditorBmSet(Ed, NLine, NKind, bmOpSet);
            Result:= ReturnNone;
          end;

        BOOKMARK_CLEAR:
          begin
            EditorBmSet(Ed, NLine, NKind, bmOpClear);
            Result:= ReturnNone;
          end;

        BOOKMARK_CLEAR_ALL:
          begin
            Ed.DoCommand(cmd_BookmarkClearAll);
            Result:= ReturnNone;
          end;

        BOOKMARK_SETUP:
          begin
            Bmp:= TBitmap.Create;
            try
              Bmp.Transparent:= true;
              Bmp.LoadFromFile(Str);
              AppBookmarkImagelist.Add(Bmp, nil);
              AppBookmarkSetup[NKind].ImageIndex:= AppBookmarkImagelist.Count-1;
              AppBookmarkSetup[NKind].Color:= NColor;
            finally
              FreeAndNil(Bmp);
            end;
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

        else
          Result:= ReturnNone;
      end;

      fmMain.UpdateFrame(true);
      fmMain.UpdateStatus;
    end;
end;


function Py_ed_get_split(Self, Args: PPyObject): PPyObject; cdecl;
var
  H: PtrInt;
  NState: integer;
  NValue: double;
  Ed: TATSynEdit;
  F: TEditorFrame;
begin
  with GetPythonEngine do
    if Bool(PyArg_ParseTuple(Args, 'L', @H)) then
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
  H: PtrInt;
  NState: integer;
  NValue: double;
  Ed: TATSynEdit;
  F: TEditorFrame;
begin
  with GetPythonEngine do
    if Bool(PyArg_ParseTuple(Args, 'Lid', @H, @NState, @NValue)) then
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


function Py_dlg_file(Self, Args: PPyObject): PPyObject; cdecl;
var
  PtrFilename, PtrFolder, PtrFilter: PAnsiChar;
  StrFilename, StrFolder, StrFilter: string;
  IsOpen: LongBool;
  Dlg: TOpenDialog;
begin
  with GetPythonEngine do
    if Bool(PyArg_ParseTuple(Args, 'isss:dlg_file',
      @IsOpen, @PtrFilename, @PtrFolder, @PtrFilter)) then
    begin
      StrFilename:= AnsiString(PtrFilename);
      StrFolder:= AnsiString(PtrFolder);
      StrFilter:= AnsiString(PtrFilter);

      if IsOpen then
      begin
        Dlg:= TOpenDialog.Create(nil);
        Dlg.Options:= Dlg.Options+[ofFileMustExist];
      end
      else
      begin
        Dlg:= TSaveDialog.Create(nil);
        Dlg.Options:= Dlg.Options+[ofOverwritePrompt];
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
            Result:= PyString_FromString(PChar(Dlg.Files.Text))
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


procedure TfmMain.PyCompletionOnGetProp(Sender: TObject;
  out AText, ASuffix: string; out ACharsLeft, ACharsRight: integer);
begin
  AText:= '';
  ASuffix:= '';
  ACharsLeft:= 0;
  ACharsRight:= 0;

  //stop doing, if caret moved
  //(cannot recalc AText here)
  with CurrentEditor.Carets[0] do
    if (PosX<>FPyComplete_CaretPos.X) or (PosY<>FPyComplete_CaretPos.Y) then
      exit;

  AText:= FPyComplete_Text;
  ACharsLeft:= FPyComplete_CharsLeft;
  ACharsRight:= FPyComplete_CharsRight;
end;

function Py_ed_complete(Self, Args: PPyObject): PPyObject; cdecl;
var
  H: PtrInt;
  Ptr: PAnsiChar;
  Ed: TATSynEdit;
  Str: string;
  NChars1, NChars2: integer;
begin
  with GetPythonEngine do
    if Bool(PyArg_ParseTuple(Args, 'Lsii:complete', @H, @Ptr, @NChars1, @NChars2)) then
    begin
      Ed:= PyEditor(H);
      Str:= AnsiString(Ptr);

      fmMain.FPyComplete_Text:= Str;
      fmMain.FPyComplete_CharsLeft:= NChars1;
      fmMain.FPyComplete_CharsRight:= NChars2;
      with Ed.Carets[0] do
        fmMain.FPyComplete_CaretPos:= Point(PosX, PosY);
      DoEditorCompletionListbox(Ed, @fmMain.PyCompletionOnGetProp);

      Result:= ReturnNone;
    end;
end;


function Py_ini_readwrite(Self, Args: PPyObject; AWrite: boolean): PPyObject; cdecl;
var
  P1, P2, P3, P4: PAnsiChar;
  StrFN, StrSess, StrKey, StrVal: string;
  fn: string;
begin
  with GetPythonEngine do
  begin
    if Bool(PyArg_ParseTuple(Args, 'ssss:ini_readwrite', @P1, @P2, @P3, @P4)) then
    begin
      StrFN:= AnsiString(P1);
      StrSess:= AnsiString(P2);
      StrKey:= AnsiString(P3);
      StrVal:= AnsiString(P4);

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
  Ptr: PAnsiChar;
  Str: string;
  Id: integer;
begin
  with GetPythonEngine do
    if Bool(PyArg_ParseTuple(Args, 'is:dlg_menu', @Id, @Ptr)) then
    begin
      Str:= AnsiString(Ptr);
      case Id of
        MENU_LIST,
        MENU_LIST_ALT:
          begin
            Id:= fmMain.DoDialogMenuApi(Str, Id=MENU_LIST_ALT);
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

function Py_ed_get_indexes(Self, Args: PPyObject): PPyObject; cdecl;
var
  H: PtrInt;
  Ed: TATSynEdit;
  Num1, Num2: integer;
begin
  with GetPythonEngine do
    if Bool(PyArg_ParseTuple(Args, 'L:get_indexes', @H)) then
    begin
      Ed:= PyEditor(H);
      fmMain.GetEditorIndexes(Ed, Num1, Num2);
      Result:= Py_BuildValue('(ii)', Num1, Num2);
    end;
end;


//func at end (uses funcs above)
procedure TfmMain.PythonModInitialization(Sender: TObject);
begin
  with Sender as TPythonModule do
  begin
    AddMethod('app_version', @Py_app_version, '');
    AddMethod('app_api_version', @Py_app_api_version, '');
    AddMethod('app_path', @Py_app_path, '');

    AddMethod('msg_status', @Py_msg_status, '');
    AddMethod('msg_box', @Py_msg_box, '');
    AddMethod('dlg_input', @Py_dlg_input, '');
    AddMethod('dlg_input_ex', @Py_dlg_input_ex, '');
    AddMethod('dlg_file', @Py_dlg_file, '');
    AddMethod('dlg_menu', @Py_dlg_menu, '');
    AddMethod('dlg_color', @Py_dlg_color, '');

    AddMethod('ed_get_carets', @Py_ed_get_carets, '');
    AddMethod('ed_set_caret', @Py_ed_set_caret, '');
    AddMethod('ed_add_caret', @Py_ed_add_caret, '');

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
    Addmethod('ed_get_line_state', @Py_ed_get_line_state, '');

    AddMethod('ed_delete', @Py_ed_delete, '');
    AddMethod('ed_insert', @Py_ed_insert, '');

    Addmethod('ed_get_filename', @Py_ed_get_filename, '');
    AddMethod('ed_get_tabcolor', @Py_ed_get_tabcolor, '');
    AddMethod('ed_set_tabcolor', @Py_ed_set_tabcolor, '');
    AddMethod('ed_get_enc', @Py_ed_get_enc, '');
    AddMethod('ed_set_enc', @Py_ed_set_enc, '');
    AddMethod('ed_get_top', @Py_ed_get_top, '');
    AddMethod('ed_set_top', @Py_ed_set_top, '');
    AddMethod('ed_get_split', @Py_ed_get_split, '');
    AddMethod('ed_set_split', @Py_ed_set_split, '');
    AddMethod('ed_get_indexes', @Py_ed_get_indexes, '');

    AddMethod('ed_cmd', @Py_ed_cmd, '');
    AddMethod('ed_lock', @Py_ed_lock, '');
    AddMethod('ed_unlock', @Py_ed_unlock, '');
    AddMethod('ed_bookmark', @Py_ed_bookmark, '');
    AddMethod('ed_focus', @Py_ed_focus, '');
    AddMethod('ed_complete', @Py_ed_complete, '');

    AddMethod('file_open', @Py_file_open, '');
    AddMethod('file_save', @Py_file_save, '');
    AddMethod('ed_handles', @Py_ed_handles, '');
    AddMethod('ini_read', @Py_ini_read, '');
    AddMethod('ini_write', @Py_ini_write, '');
end;
end;


