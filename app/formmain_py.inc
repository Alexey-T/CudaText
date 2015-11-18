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


function Py_MenuItemFromId(const Str: string): TMenuItem;
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
          Result:= PyString_FromString(PChar(fmMain.FSessionFilename));
        else
          Result:= ReturnNone;
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


function Py_msg_status(Self, Args: PPyObject): PPyObject; cdecl;
var
  P: PChar;
  Str: string;
begin
  with GetPythonEngine do
  begin
    if Bool(PyArg_ParseTuple(Args, 's:msg_status', @P)) then
    begin
      Str:= string(P);
      fmMain.MsgStatus(Str);
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
  P: PChar;
  Str: string;
  Num: integer;
  Ok: boolean;
begin
  with GetPythonEngine do
  begin
    if Bool(PyArg_ParseTuple(Args, 'si:file_open', @P, @Num)) then
    begin
      Str:= string(P);

      Inc(Num, Low(TATGroupsNums));
      if Num in [Low(TATGroupsNums)..High(TATGroupsNums)] then
        fmMain.Groups.PagesCurrent:= fmMain.Groups.Pages[Num];

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
            Ed.DoCaretSingle(X1, Y1, X2, Y2, true);
            Ed.DoGotoCaret(cEdgeTop);
            Result:= ReturnNone;
          end;

        CARET_ADD:
          begin
            Ed.Carets.Add(X2, Y2, X1, Y1);
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
  Ptr: PChar;
  Ed: TATSynEdit;
  F: TEditorFrame;
  StrW: atString;
  Enc: string;
begin
  with GetPythonEngine do
    if Bool(PyArg_ParseTuple(Args, 'Ls:set_text_all', @H, @Ptr)) then
    begin
      Ed:= PyEditor(H);
      F:= fmMain.GetEditorFrame(Ed);

      StrW:= UTF8Decode(string(Ptr));
      Enc:= F.EncodingName;
      Ed.DoCaretSingle(0, 0);
      Ed.Strings.LoadFromString(StrW); //resets enc to utf16
      Ed.DoEventChange;
      F.EncodingName:= Enc;

      fmMain.UpdateFrame(true);
      fmMain.UpdateStatus;
      Result:= ReturnNone;
    end;
end;

function Py_ed_set_text_line(Self, Args: PPyObject): PPyObject; cdecl;
var
  H: PtrInt;
  N: Integer;
  Ptr: PChar;
  Ed: TATSynEdit;
  Str: atString;
begin
  with GetPythonEngine do
    if Bool(PyArg_ParseTuple(Args, 'Lis:set_text_line', @H, @N, @Ptr)) then
    begin
      Ed:= PyEditor(H);
      Str:= UTF8Decode(string(Ptr));

      if N=-1 then
        Ed.Strings.LineAdd(Str)
      else
      if Ed.Strings.IsIndexValid(N) then
        Ed.Strings.Lines[N]:= Str;
      Ed.DoEventChange;

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
      Ed.DoEventChange;

      fmMain.UpdateFrame(true);
      fmMain.UpdateStatus;
      Result:= ReturnNone;
    end;
end;

function Py_ed_insert(Self, Args: PPyObject): PPyObject; cdecl;
var
  H: PtrInt;
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
  H: PtrInt;
begin
  with GetPythonEngine do
    if Bool(PyArg_ParseTuple(Args, 'L:get_line_count', @H)) then
    begin
      Result:= PyInt_FromLong(PyEditor(H).Strings.Count);
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
  Ptr: PChar;
  Ed: TATSynEdit;
  F: TEditorFrame;
  Str: string;
begin
  with GetPythonEngine do
    if Bool(PyArg_ParseTuple(Args, 'Ls:set_enc', @H, @Ptr)) then
    begin
      Ed:= PyEditor(H);
      Str:= string(Ptr);
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
  Ptr: PChar;
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
      Str:= string(Ptr);
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
  Ptr: PChar;
  Ed: TATSynEdit;
  Str: string;
  NChars1, NChars2: integer;
begin
  with GetPythonEngine do
    if Bool(PyArg_ParseTuple(Args, 'Lsii:complete', @H, @Ptr, @NChars1, @NChars2)) then
    begin
      Ed:= PyEditor(H);
      Str:= string(Ptr);

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
  Id: integer;
begin
  with GetPythonEngine do
    if Bool(PyArg_ParseTuple(Args, 'is:dlg_menu', @Id, @Ptr)) then
    begin
      Str:= string(Ptr);
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

type
  TAppPanelProps = record
    Listbox: TATListbox;
    Items: TStringList;
    RegexStr: string;
    RegexIdLine,
    RegexIdCol,
    RegexIdName: integer;
    DefFilename: string;
    ZeroBase: boolean;
    Encoding: string;
  end;

var
  AppPanelProp_Out: TAppPanelProps;
  AppPanelProp_Val: TAppPanelProps;
  AppPanelActive: string = '0';

function Py_app_log(Self, Args: PPyObject): PPyObject; cdecl;
var
  Ptr: PChar;
  Str: string;
  Id: integer;
  Prop: ^TAppPanelProps;
begin
  with GetPythonEngine do
    if Bool(PyArg_ParseTuple(Args, 'is:app_log', @Id, @Ptr)) then
    begin
      Str:= string(Ptr);

      if AppPanelActive='0' then
        Prop:= @AppPanelProp_Out
      else
        Prop:= @AppPanelProp_Val;

      case Id of
        LOG_CLEAR:
          begin
            Prop^.Items.Clear;
            Prop^.Listbox.ItemCount:= 0;
            Prop^.Listbox.Invalidate;
          end;
        LOG_ADD:
          begin
            Prop^.Items.Add(Str);
            Prop^.Listbox.ItemCount:= Prop^.Items.Count;
            Prop^.Listbox.Invalidate;
          end;
        LOG_SET_PANEL:
          AppPanelActive:= Str;
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

        LOG_CONSOLE_CLEAR:
          with fmMain do
          begin
            fmConsole.memo.Strings.Clear;
            fmConsole.memo.Update(true);
            fmConsole.ed.Text:= '';
            fmConsole.ed.Items.Clear;
            fmConsole.ed.Update(true);
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
      end;

      Result:= ReturnNone;
    end;
end;


function Py_dlg_checklist(Self, Args: PPyObject): PPyObject; cdecl;
var
  PtrCaption, PtrColumns, PtrItems: PChar;
  StrCaption, StrColumns, StrItems: string;
  NSizeX, NSizeY: integer;
begin
  with GetPythonEngine do
    if Bool(PyArg_ParseTuple(Args, 'sssii:dlg_checklist',
      @PtrCaption, @PtrColumns, @PtrItems, @NSizeX, @NSizeY)) then
    begin
      StrCaption:= string(PtrCaption);
      StrColumns:= string(PtrColumns);
      StrItems:= string(PtrItems);

      StrItems:= DoInputCheckList(StrCaption, StrColumns, StrItems, NSizeX, NSizeY);
      Result:= PyString_FromString(PChar(StrItems));
    end;
end;


function Py_ed_get_prop(Self, Args: PPyObject): PPyObject; cdecl;
var
  H: PtrInt;
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

        PROP_UNPRINTED_SHOW:
          Result:= PyBool_FromLong(Ord(Ed.OptUnprintedVisible));
        PROP_UNPRINTED_SPACES:
          Result:= PyBool_FromLong(Ord(Ed.OptUnprintedSpaces));
        PROP_UNPRINTED_ENDS:
          Result:= PyBool_FromLong(Ord(Ed.OptUnprintedEnds));
        PROP_UNPRINTED_END_DETAILS:
          Result:= PyBool_FromLong(Ord(Ed.OptUnprintedEndsDetails));

        else
          Result:= ReturnNone;
      end;
    end;
end;

function Py_ed_set_prop(Self, Args: PPyObject): PPyObject; cdecl;
var
  H: PtrInt;
  Id: integer;
  Ptr: PChar;
  Str: string;
  Ed: TATSynEdit;
  F: TEditorFrame;
  ValueBool: boolean;
  ValueInt: integer;
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
        PROP_RULER:
          Ed.OptRulerVisible:= ValueBool;
        PROP_LEXER_FILE:
          F.Lexer:= Manager.FindAnalyzer(Str);

        PROP_UNPRINTED_SHOW:
          Ed.OptUnprintedVisible:= ValueBool;
        PROP_UNPRINTED_SPACES:
          Ed.OptUnprintedSpaces:= ValueBool;
        PROP_UNPRINTED_ENDS:
          Ed.OptUnprintedEnds:= ValueBool;
        PROP_UNPRINTED_END_DETAILS:
          Ed.OptUnprintedEndsDetails:= ValueBool;
      end;

      fmMain.UpdateFrame;
      fmMain.UpdateStatus;
      Result:= ReturnNone;
    end;
end;

function Py_app_proc(Self, Args: PPyObject): PPyObject; cdecl;
var
  Id, Num: integer;
  Ptr: PChar;
  Str: string;
  StrCaption, StrId, StrCmd, StrIndex: string;
  mi, miMain: TMenuItem;
begin
  with GetPythonEngine do
    if Bool(PyArg_ParseTuple(Args, 'is:app_proc', @Id, @Ptr)) then
    begin
      Str:= string(Ptr);

      case Id of
        APP_PROC_GET_CLIP:
          begin
            Result:= PyString_FromString(PChar(Clipboard.AsText));
          end;
        APP_PROC_SET_CLIP:
          begin
            Clipboard.AsText:= Str;
            Result:= ReturnNone;
          end;

        APP_PROC_GET_COMMAND:
          begin
            Num:= StrToIntDef(Str, -1);
            if Keymap.IsIndexValid(Num) then
              with Keymap[Num] do
                Result:= Py_BuildValue('(isss)',
                  Command,
                  PChar(Name),
                  PChar(KeyArrayToString(Keys1)),
                  PChar(KeyArrayToString(Keys2))
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
            fmMain.FSessionFilename:= Str;
            fmMain.UpdateCaption;
            Result:= ReturnNone;
          end;

        APP_PROC_MENU_ENUM:
          begin
            //this updates PopupText items tags
            fmMain.PopupText.OnPopup(nil);

            mi:= Py_MenuItemFromId(Str);
            if Assigned(mi) then
            begin
              Str:= '';
              for Num:= 0 to mi.Count-1 do
                Str:= Str+
                  mi.Items[Num].Caption+'|'+
                  IfThen(mi.Items[Num].Tag>0,
                         IntToStr(mi.Items[Num].Tag),
                         mi.Items[Num].Hint)
                         +#10;
              Result:= PyString_FromString(PChar(Str));
            end
            else
              Result:= ReturnNone;
          end;

        APP_PROC_MENU_CLEAR:
          begin
            mi:= Py_MenuItemFromId(Str);
            if Assigned(mi) then
            begin
              mi.Clear;
              if Str=PyMenuId_Top then
              begin
                fmMain.mnuFileOpenSub:= nil;
                fmMain.mnuThemes:= nil;
                fmMain.mnuPlug:= nil;
              end;
              if Str=PyMenuId_TopFile then
              begin
                fmMain.mnuFileOpenSub:= nil;
              end;
              if Str=PyMenuId_Text then
              begin
                fmMain.mnuTextCopy:= nil;
                fmMain.mnuTextCut:= nil;
                fmMain.mnuTextDelete:= nil;
                fmMain.mnuTextPaste:= nil;
                fmMain.mnuTextUndo:= nil;
                fmMain.mnuTextRedo:= nil;
                fmMain.mnuTextSel:= nil;
                fmMain.mnuTextGotoDef:= nil;
              end;
            end;
            Result:= ReturnNone;
          end;

        APP_PROC_MENU_ADD:
          begin
            StrId:= SGetItem(Str, ';');
            StrCmd:= SGetItem(Str, ';');
            StrCaption:= SGetItem(Str, ';');
            StrIndex:= SGetItem(Str, ';');

            miMain:= Py_MenuItemFromId(StrId);
            if Assigned(miMain) and (StrCaption<>'') then
            begin
              mi:= TMenuItem.Create(fmMain);
              mi.Caption:= StrCaption;

              Num:= StrToIntDef(StrCmd, 0);
              if Num>0 then
                fmMain.UpKey(mi, Num)
              else
              if StrCmd=PyMenuCmd_Recents then
              begin
                fmMain.mnuFileOpenSub:= mi;
                fmMain.UpdateMenuRecent(nil);
              end
              else
              if StrCmd=PyMenuCmd_Themes then
              begin
                fmMain.mnuThemes:= mi;
                fmMain.UpdateMenuThemes(mi);
              end
              else
              if StrCmd=PyMenuCmd_Plugins then
              begin
                fmMain.mnuPlug:= mi;
                fmMain.UpdateMenuPlugins;
              end
              else
              begin
                mi.Tag:= -1;
                mi.Hint:= StrCmd;
                if StrCmd<>'0' then
                  mi.OnClick:= @fmMain.MenuMainClick;
              end;

              Num:= StrToIntDef(StrIndex, -1);
              if Num>=0 then
                miMain.Insert(Num, mi)
              else
                miMain.Add(mi);

              Result:= PyString_FromString(PChar(IntToStr(PtrInt(mi))));
            end
            else
              Result:= ReturnNone;
          end;

        APP_PROC_SET_EVENTS:
          begin
            StrId:= SGetItem(Str, ';'); //module
            StrCmd:= SGetItem(Str, ';'); //events
            StrCaption:= SGetItem(Str, ';'); //lexers
            fmMain.DoPyUpdateEvents(StrId, StrCmd, StrCaption);
            Result:= ReturnNone;
          end;

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
        LEXER_GET_COMMENT:
          begin
            An:= Manager.FindAnalyzer(Str1);
            if Assigned(An) then
              Result:= PyString_FromString(PChar(string(An.LineComment)))
            else
              Result:= ReturnNone;
          end;

        LEXER_GET_ENABLED:
          begin
            An:= Manager.FindAnalyzer(Str1);
            if Assigned(An) then
              Result:= PyBool_FromLong(Ord(not An.Internal))
            else
              Result:= ReturnNone;
          end;

        LEXER_GET_EXT:
          begin
            An:= Manager.FindAnalyzer(Str1);
            if Assigned(An) then
              Result:= PyString_FromString(PChar(string(An.Extentions)))
            else
              Result:= ReturnNone;
          end;

        LEXER_SET_ENABLED:
          begin
            An:= Manager.FindAnalyzer(Str1);
            if Assigned(An) then
              An.Internal:= Str2='0';
            Result:= ReturnNone;

            Manager.Modified:= true;
            fmMain.UpdateMenuLexers;
          end;

        LEXER_SET_NAME:
          begin
            An:= Manager.FindAnalyzer(Str1);
            if Assigned(An) then
              An.LexerName:= Str2;
            Result:= ReturnNone;

            Manager.Modified:= true;
            fmMain.UpdateMenuLexers;
          end;

        LEXER_SET_EXT:
          begin
            An:= Manager.FindAnalyzer(Str1);
            if Assigned(An) then
              An.Extentions:= Str2;
            Result:= ReturnNone;

            Manager.Modified:= true;
          end;

        LEXER_GET_MODIFIED:
          begin
            Result:= PyBool_FromLong(Ord(Manager.Modified));
          end;

        LEXER_SAVE_LIB:
          begin
            with Manager do
              SaveToFile(FileName);
            Result:= ReturnNone;
          end;

        LEXER_DELETE:
          begin
            An:= Manager.FindAnalyzer(Str1);
            if Assigned(An) then
              An.Free;
            Result:= ReturnNone;

            Manager.Modified:= true;
            fmMain.UpdateMenuLexers;
          end;

        LEXER_IMPORT:
          begin
            if FileExists(Str1) then
            begin
              An:= Manager.AddAnalyzer;
              An.LoadFromFile(Str1);
              Result:= PyString_FromString(PChar(string(An.LexerName)));

              Manager.Modified:= true;
              fmMain.UpdateMenuLexers;
            end
            else
              Result:= ReturnNone;
          end;

        LEXER_EXPORT:
          begin
            An:= Manager.FindAnalyzer(Str1);
            if Assigned(An) then
            begin
              An.SaveToFile(Str2);
              Result:= PyBool_FromLong(1);
            end
            else
              Result:= PyBool_FromLong(0);
          end;

        LEXER_GET_LIST:
          begin
            List:= TStringList.Create;
            List.Sorted:= true;
            try
              DoEnumLexers(List, true);
              Result:= PyString_FromString(PChar(List.Text));
            finally
              FreeAndNil(List);
            end;
          end;

        LEXER_GET_LINKS:
          begin
            An:= Manager.FindAnalyzer(Str1);
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
            An:= Manager.FindAnalyzer(Str1);
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
            An:= Manager.FindAnalyzer(Str1);
            if Assigned(An) then
              LexerSetSublexers(Manager, An, Str2);
            Result:= ReturnNone;
            Manager.Modified:= true;
          end;

        else
          Result:= ReturnNone;
      end;
    end;
end;

function Py_ed_convert(Self, Args: PPyObject): PPyObject; cdecl;
var
  H: PtrInt;
  Ed: TATSynEdit;
  Id, X, Y: integer;
  Str: atString;
begin
  with GetPythonEngine do
    if Bool(PyArg_ParseTuple(Args, 'Liii:convert', @H, @Id, @X, @Y)) then
    begin
      Ed:= PyEditor(H);

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
        else
          Result:= ReturnNone;
      end;
    end;
end;


function Py_ed_get_ranges(Self, Args: PPyObject): PPyObject; cdecl;
var
  H: PtrInt;
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

    AddMethod('msg_status', @Py_msg_status, '');
    AddMethod('msg_box', @Py_msg_box, '');
    AddMethod('dlg_input', @Py_dlg_input, '');
    AddMethod('dlg_input_ex', @Py_dlg_input_ex, '');
    AddMethod('dlg_file', @Py_dlg_file, '');
    AddMethod('dlg_menu', @Py_dlg_menu, '');
    AddMethod('dlg_color', @Py_dlg_color, '');
    AddMethod('dlg_checklist', @Py_dlg_checklist, '');

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
    AddMethod('ed_get_tabcolor', @Py_ed_get_tabcolor, '');
    AddMethod('ed_set_tabcolor', @Py_ed_set_tabcolor, '');
    AddMethod('ed_get_enc', @Py_ed_get_enc, '');
    AddMethod('ed_set_enc', @Py_ed_set_enc, '');
    AddMethod('ed_get_top', @Py_ed_get_top, '');
    AddMethod('ed_set_top', @Py_ed_set_top, '');
    AddMethod('ed_get_split', @Py_ed_get_split, '');
    AddMethod('ed_set_split', @Py_ed_set_split, '');
    AddMethod('ed_get_prop', @Py_ed_get_prop, '');
    AddMethod('ed_set_prop', @Py_ed_set_prop, '');
    AddMethod('ed_get_ranges', @Py_ed_get_ranges, '');

    AddMethod('ed_cmd', @Py_ed_cmd, '');
    AddMethod('ed_lock', @Py_ed_lock, '');
    AddMethod('ed_unlock', @Py_ed_unlock, '');
    AddMethod('ed_bookmark', @Py_ed_bookmark, '');
    AddMethod('ed_focus', @Py_ed_focus, '');
    AddMethod('ed_complete', @Py_ed_complete, '');
    AddMethod('ed_convert', @Py_ed_convert, '');

    AddMethod('file_open', @Py_file_open, '');
    AddMethod('file_save', @Py_file_save, '');
    AddMethod('ed_handles', @Py_ed_handles, '');
    AddMethod('ini_read', @Py_ini_read, '');
    AddMethod('ini_write', @Py_ini_write, '');
    AddMethod('lexer_proc', @Py_lexer_proc, '');
  end;
end;


