(*
This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at http://mozilla.org/MPL/2.0/.

Copyright (c) Alexey Torgashin
*)
unit form_console;

{$mode objfpc}{$H+}
{$ScopedEnums on}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StrUtils, Menus, LclType, Math, IniFiles,
  PythonEngine,
  ATStrings,
  ATSynEdit,
  ATSynEdit_Globals,
  ATSynEdit_Edits,
  ATSynEdit_Commands,
  ATSynEdit_Adapter_Simple,
  ATSynEdit_Cmp_Form,
  ATStringProc,
  ec_SyntAnal,
  ec_syntax_format,
  proc_py,
  proc_str,
  proc_colors,
  proc_globdata,
  proc_customdialog,
  proc_customdialog_dummy,
  proc_editor,
  proc_cmd,
  proc_msg;

type
  TAppStrEvent = procedure(const Str: string) of object;
  TAppConsoleEvent = function(const Str: string): boolean of object;
  TAppConsoleCommandEvent = procedure(ACommand: integer; const AText: string; var AHandled: boolean) of object;
  TAppConsoleGetEditor = procedure(out AEditor: TATSynEdit) of object;

  TAppConsoleLineKind = (
    Usual,
    Prompt,
    Note,
    Error
    );

type
  { TfmConsole }

  //inherit from TFormDummy to support dlg_proc API for this form
  TfmConsole = class(TFormDummy)
  private
    { private declarations }
    FFormMain: TCustomForm;
    FAdapter: TATAdapterSimple;
    FOnNavigate: TAppConsoleEvent;
    FOnComplete: TNotifyEvent;
    FOnNumberChange: TNotifyEvent;
    FOnGetMainEditor: TAppConsoleGetEditor;
    FCudatextImported: boolean;
    mnuTextClear: TMenuItem;
    mnuTextNav: TMenuItem;
    mnuTextWrap: TMenuItem;
    procedure InputOnClick(Sender: TObject);
    procedure InputOnCommand(Sender: TObject; ACommand: integer; AInvoke: TATCommandInvoke; const AText: string; var AHandled: boolean);
    procedure DoGetLineColor(Ed: TATSynEdit; ALineIndex: integer; var AColorFont, AColorBg: TColor);
    procedure MemoOnClick(Sender: TObject);
    procedure MemoOnClickDbl(Sender: TObject; var AHandled: boolean);
    procedure MemoCommand(Sender: TObject; ACommand: integer; AInvoke: TATCommandInvoke; const AText: string; var AHandled: boolean);
    procedure MemoContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
    procedure DoNavigate(Sender: TObject);
    procedure DoToggleWrap(Sender: TObject);
    function ParseLine(const S: string): TAppConsoleLineKind;
    procedure SetIsDoubleBuffered(AValue: boolean);
    function GetWordWrap: boolean;
    procedure SetWordWrap(AValue: boolean);
    procedure DoRunLine(Str: string);
  protected
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
  public
    { public declarations }
    EdInput: TATComboEdit;
    EdMemo: TATSynEdit;
    ErrorCounter: integer;
    ShortCutForAutoCompletion: TShortCut;
    constructor Create(AOwner: TComponent); override;
    property OnConsoleNav: TAppConsoleEvent read FOnNavigate write FOnNavigate;
    property OnConsoleComplete: TNotifyEvent read FOnComplete write FOnComplete;
    property OnNumberChange: TNotifyEvent read FOnNumberChange write FOnNumberChange;
    property OnGetMainEditor: TAppConsoleGetEditor read FOnGetMainEditor write FOnGetMainEditor;
    procedure DoAddLine(const AText: UnicodeString);
    procedure DoClearMemo(Sender: TObject);
    procedure DoClearInput(Sender: TObject);
    procedure DoClearHistory;
    procedure DoUpdateMemo;
    property IsDoubleBuffered: boolean write SetIsDoubleBuffered;
    property MemoWordWrap: boolean read GetWordWrap write SetWordWrap;
    procedure SetFocus; override;
    procedure ApplyTheme;
    procedure ApplyCaretView;
    procedure FlushConsole;
  end;

var
  fmConsole: TfmConsole = nil;

procedure InitConsole(AFormMain: TCustomForm; AOnGetMainEditor: TAppConsoleGetEditor);

const
  cConsoleMaxLines = 1000;
  cConsoleMaxComboboxItems: integer = 20;
  cConsolePrompt = '>>> ';
  cConsolePrintPrefix = '=';

implementation

//{$R *.lfm} //not needed for console

function IsConsoleErrorLine(const S: string): boolean;
// checks for Python exception string like:
// ZeroDivisionError: division by zero
const
  cMinLen = 10;
  cMaxLen = 150;
var
  N, i: SizeInt;
begin
  Result:= false;
  if Length(S)<cMinLen then exit;
  if Length(S)>cMaxLen then exit;
  if S[1]='.' then exit;

  N:= Pos('Error: ', S, 2);
  if N=0 then
  begin
    N:= Pos(': SyntaxWarning: ', S, 2);
    if N>0 then
      Result:= true;
    exit;
  end;

  for i:= 1 to N-1 do
    if not (S[i] in ['.', 'a'..'z', 'A'..'Z', '_']) then
      exit;
  Result:= true;
end;

{ TfmConsole }

function TfmConsole.ParseLine(const S: string): TAppConsoleLineKind;
begin
  if StartsStr(cConsolePrompt, S) then
    exit(TAppConsoleLineKind.Prompt);

  if StartsText('NOTE:', S) then
    exit(TAppConsoleLineKind.Note);

  if StartsText('ERROR:', S) then
    exit(TAppConsoleLineKind.Error);

  //EndsText is better than compare, to find FindInFiles4 log string added to 'traceback'
  if EndsText('Traceback (most recent call last):', S) then
    exit(TAppConsoleLineKind.Error);

  if IsConsoleErrorLine(S) then
    exit(TAppConsoleLineKind.Error);

  Result:= TAppConsoleLineKind.Usual;
end;

procedure TfmConsole.DoGetLineColor(Ed: TATSynEdit; ALineIndex: integer;
  var AColorFont, AColorBg: TColor);
var
  Str: UnicodeString;
  fmt: TecSyntaxFormat;
begin
  Str:= Ed.Strings.Lines[ALineIndex];
  case ParseLine(Str) of
    TAppConsoleLineKind.Prompt:
      begin
        fmt:= GetAppStyle(TAppThemeStyle.Id2);
        AColorFont:= fmt.Font.Color;
        exit;
      end;
    TAppConsoleLineKind.Note:
      begin
        fmt:= GetAppStyle(TAppThemeStyle.LightBG2);
        AColorBg:= fmt.BgColor;
        fmt:= GetAppStyle(TAppThemeStyle.Id);
        AColorFont:= fmt.Font.Color;
        exit
      end;
    TAppConsoleLineKind.Error:
      begin
        fmt:= GetAppStyle(TAppThemeStyle.LightBG1);
        AColorBg:= fmt.BgColor;
        fmt:= GetAppStyle(TAppThemeStyle.Id);
        AColorFont:= fmt.Font.Color;
        exit
      end;
  end;
end;

procedure TfmConsole.DoAddLine(const AText: UnicodeString);
var
  Strs: TATStrings;
  i: integer;
begin
  with EdMemo do
  begin
    ModeReadOnly:= false;
    Strs:= Strings;

    //this is to remove 1st empty line
    if (Strs.Count=1) and (Strs.LinesLen[0]=0) then
      Strs.Lines[0]:= AText
    else
    begin
      Strs.LineAddRaw_NoUndo(AText, TATLineEnds.Unix);
      for i:= 1 to Strs.Count-cConsoleMaxLines do
        Strs.LineDelete(0, false, false, false);
    end;

    ModeReadOnly:= true;

    if ParseLine(AText) in [TAppConsoleLineKind.Error, TAppConsoleLineKind.Note] then
    begin
      Inc(ErrorCounter);
      if Assigned(FOnNumberChange) then
        FOnNumberChange(Self);
    end;
  end;
end;

procedure TfmConsole.DoUpdateMemo;
//This must be called after some DoAddLine calls
//
//Note: don't call Application.ProcessMessages here!
// https://github.com/Alexey-T/CudaText/issues/2326
begin
  with EdMemo do
  begin
    //we added some lines directly to EdMemo.Strings, so update WrapInfo
    UpdateWrapInfo(true, false{important});
    DoCommand(cCommand_GotoTextEnd, TATCommandInvoke.AppInternal);
    ColumnLeft:= 0;

    //extra params of Update() are not needed
    Update;
  end;
end;

procedure TfmConsole.SetFocus;
begin
  inherited;
  EdInput.SetFocus;
end;

procedure TfmConsole.DoRunLine(Str: string);
var
  Obj: PPyObject;
  bNoLog: boolean;
  bExpr: boolean;
begin
  bNoLog:= EndsStr(';', Str);
  if bNoLog then
    Delete(Str, Length(Str), 1)
  else
    EdInput.DoAddLineToHistory(Utf8Decode(Str), cConsoleMaxComboboxItems);

  DoAddLine(cConsolePrompt+Str);
  DoUpdateMemo;

  try
    if StartsStr(cConsolePrintPrefix, Str) then
    begin
      Str:= 'print('+Copy(Str, 2, MaxInt) + ')';
      bExpr:= false;
    end
    else
      bExpr:= IsPythonExpression(Str);

    if not FCudatextImported then
    begin
      FCudatextImported:= true;
      AppPython.Exec('from cudatext import *');
    end;

    Obj:= AppPython.Eval(Str, not bExpr);
    //if 2nd param is True, this won't return PyObject

    if Assigned(Obj) then
      with AppPython.Engine do
      try
        if Pointer(Obj)<>Pointer(Py_None) then
          MsgLogConsole(PyObjectAsString(Obj));
      finally
        Py_DECREF(Obj);
      end;

    FlushConsole;
  except
  end;
end;

procedure TfmConsole.KeyDown(var Key: Word; Shift: TShiftState);
var
  NShortCut: TShortCut;
begin
  NShortCut:= ShortCut(Key, Shift);
  if NShortCut=0 then exit;

  if (Key=VK_ESCAPE) and (Shift=[]) then
    if Assigned(FFormMain.OnKeyDown) then
    begin
      FFormMain.OnKeyDown(nil, Key, Shift);
      Key:= 0;
      exit;
    end;

  //avoid handling of Shift+Tab in the editor (it runs "Unindent block")
  if (Key=VK_TAB) and (Shift*[ssCtrl, ssAlt]=[]) then
  begin
    //SelectNext() LCL method works worse
    if EdInput.Focused then
      EdMemo.SetFocus
    else
    if EdMemo.Focused then
      EdInput.SetFocus;
    Key:= 0;
    exit;
  end;

  if NShortCut=ShortCutForAutoCompletion then
  begin
    if Assigned(FOnComplete) then
      FOnComplete(Self);
    Key:= 0;
    exit;
  end;

  inherited KeyDown(Key, Shift);
end;

constructor TfmConsole.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  ShowInTaskBar:= stNever;
  BorderStyle:= bsNone;
  IsDlgCounterIgnored:= true;

  FAdapter:= TATAdapterSimple.Create(Self);
  FAdapter.OnGetLineColor:= @DoGetLineColor;

  EdInput:= TATComboEdit.Create(Self);
  EdInput.Name:= 'input';
  EdInput.Parent:= Self;
  EdInput.Align:= alBottom;
  EdInput.Keymap:= AppKeymapMain;
  EdInput.WantTabs:= false;
  EdInput.TabStop:= true;
  EdInput.OnClick:= @InputOnClick;
  EdInput.OnCommand:= @InputOnCommand;

  EdInput.OptTabSize:= 4;
  EdInput.OptBorderWidth:= 1;
  EdInput.OptBorderWidthFocused:= 1;

  EdMemo:= TATSynEdit.Create(Self);
  EdMemo.Name:= 'memo';
  EdMemo.Parent:= Self;
  EdMemo.Align:= alClient;
  EdMemo.BorderStyle:= bsNone;

  EdMemo.WantTabs:= false;
  EdMemo.TabStop:= true;
  EdMemo.AdapterForHilite:= FAdapter;

  IsDoubleBuffered:= UiOps.DoubleBuffered;

  EdMemo.OptWrapMode:= TATEditorWrapMode.ModeOn;
  EdMemo.OptScrollbarsNew:= EditorOps.OpScrollbarsNew;
  EdMemo.OptUndoLimit:= 0;

  EdMemo.OptTabSize:= 4;
  EdMemo.OptBorderFocusedActive:= EditorOps.OpActiveBorderInControls;
  EdMemo.OptBorderWidthFocused:= ATEditorScale(EditorOps.OpActiveBorderWidth);
  EdMemo.OptBorderWidth:= 0;
  EdMemo.OptShowURLs:= false;
  EdMemo.OptMouse2ClickOpensURL:= false;
  EdMemo.OptCaretVirtual:= false;
  EdMemo.OptCaretManyAllowed:= false;
  EdMemo.OptGutterVisible:= true;
  EdMemo.Gutter[EdMemo.Gutter.FindIndexByTag(ATEditorOptions.GutterTagNumbers)].Visible:= false;
  EdMemo.Gutter[EdMemo.Gutter.FindIndexByTag(ATEditorOptions.GutterTagFolding)].Visible:= false;
  EdMemo.Gutter[EdMemo.Gutter.FindIndexByTag(ATEditorOptions.GutterTagLineStates)].Visible:= false;
  EdMemo.Gutter[EdMemo.Gutter.FindIndexByTag(ATEditorOptions.GutterTagBookmarks)].Visible:= false;
  EdMemo.OptGutterWidthEmpty:= 3;
  EdMemo.OptRulerVisible:= false;
  EdMemo.OptUnprintedVisible:= false;
  EdMemo.OptMarginRight:= 2000;
  EdMemo.ModeReadOnly:= true;
  EdMemo.OptMouseRightClickMovesCaret:= true;
  EdMemo.OptMouseWheelZooms:= false;
  EdMemo.OptShowMouseSelFrame:= false;

  //support dlg_proc API, it needs PropsObject
  DoControl_InitPropsObject(EdInput, Self, 'editor_edit');
  DoControl_InitPropsObject(EdMemo, Self, 'editor');

  EdMemo.OnClick:= @MemoOnClick;
  EdMemo.OnClickDouble:= @MemoOnClickDbl;
  EdMemo.OnCommand:= @MemoCommand;
  //after DoControl_InitPropsObject, because it did set custom OnContextMenu
  EdMemo.OnContextPopup:= @MemoContextPopup;
end;


procedure InitConsole(AFormMain: TCustomForm; AOnGetMainEditor: TAppConsoleGetEditor);
begin
  if fmConsole=nil then
  begin
    fmConsole:= TfmConsole.Create(nil);
    fmConsole.FFormMain:= AFormMain;
    fmConsole.OnGetMainEditor:= AOnGetMainEditor;
  end;
end;

procedure TfmConsole.InputOnCommand(Sender: TObject; ACommand: integer;
  AInvoke: TATCommandInvoke; const AText: string; var AHandled: boolean);
var
  Ed: TATSynEdit;
  s: string;
begin
  if ACommand=cCommand_KeyEnter then
  begin
    s:= UTF8Encode(EdInput.Text);
    DoRunLine(s);

    EdInput.Text:= '';
    EdInput.DoCaretSingle(0, 0);

    AHandled:= true;
    exit
  end;

  if (ACommand>=cmdFirstAppCommand) and (ACommand<=cmdLastAppCommand) then
  begin
    FOnGetMainEditor(Ed);
    Ed.DoCommand(ACommand, TATCommandInvoke.Hotkey, '');
    AHandled:= true;
    exit;
  end;
end;

function TfmConsole.GetWordWrap: boolean;
begin
  Result:= EdMemo.OptWrapMode=TATEditorWrapMode.ModeOn;
end;

procedure TfmConsole.DoClearMemo(Sender: TObject);
begin
  EdMemo.ModeReadOnly:= false;
  EdMemo.Text:= '';
  EdMemo.ModeReadOnly:= true;

  ErrorCounter:= 0;
  if Assigned(FOnNumberChange) then
    FOnNumberChange(Self);
end;

procedure TfmConsole.DoClearInput(Sender: TObject);
begin
  EdInput.DoCommand(cCommand_GotoTextBegin, TATCommandInvoke.AppInternal);
  EdInput.DoCommand(cCommand_TextDeleteToTextEnd, TATCommandInvoke.AppInternal);
  EdInput.DoCaretSingle(0, 0);
end;

procedure TfmConsole.DoClearHistory;
begin
  EdInput.Items.Clear;
end;

procedure TfmConsole.DoNavigate(Sender: TObject);
var
  S: string;
  N: integer;
begin
  if Assigned(FOnNavigate) then
  begin
    N:= EdMemo.Carets[0].PosY;
    if not EdMemo.Strings.IsIndexValid(N) then exit;
    S:= EdMemo.Strings.Lines[N];
    FOnNavigate(S);
  end;
end;

procedure TfmConsole.DoToggleWrap(Sender: TObject);
begin
  MemoWordWrap:= not MemoWordWrap;
end;

procedure TfmConsole.MemoContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
var
  msgConsoleClear: string;
  msgConsoleToggleWrap: string;
  msgConsoleNavigate: string;
begin
  if mnuTextClear=nil then
  begin
    mnuTextClear:= TMenuItem.Create(Self);
    mnuTextClear.OnClick:= @DoClearMemo;
    EdMemo.PopupTextDefault.Items.Add(mnuTextClear);

    mnuTextWrap:= TMenuItem.Create(Self);
    mnuTextWrap.OnClick:= @DoToggleWrap;
    EdMemo.PopupTextDefault.Items.Add(mnuTextWrap);

    mnuTextNav:= TMenuItem.Create(Self);
    mnuTextNav.OnClick:= @DoNavigate;
    EdMemo.PopupTextDefault.Items.Add(mnuTextNav);
  end;

  with TIniFile.Create(AppFile_Language) do
  try
    msgConsoleClear:= ReadString('ct', 'clr', 'Clear');
    msgConsoleNavigate:= ReadString('ct', 'nav', 'Navigate');
    msgConsoleToggleWrap:= ReadString('ct', 'wr', 'Toggle word wrap');
  finally
    Free;
  end;

  mnuTextClear.Caption:= msgConsoleClear;
  mnuTextWrap.Caption:= msgConsoleToggleWrap;
  mnuTextNav.Caption:= msgConsoleNavigate;
  mnuTextWrap.Checked:= MemoWordWrap;

  Handled:= false;
end;

procedure TfmConsole.SetIsDoubleBuffered(AValue: boolean);
begin
  EdInput.DoubleBuffered:= AValue;
  EdMemo.DoubleBuffered:= AValue;
end;

procedure TfmConsole.SetWordWrap(AValue: boolean);
begin
  if AValue then
    EdMemo.OptWrapMode:= TATEditorWrapMode.ModeOn
  else
    EdMemo.OptWrapMode:= TATEditorWrapMode.ModeOff;
  //EdMemo.OptAllowScrollbarHorz:= not AValue;
  EdMemo.Update;
end;

procedure TfmConsole.MemoCommand(Sender: TObject; ACommand: integer;
  AInvoke: TATCommandInvoke; const AText: string; var AHandled: boolean);
var
  Ed: TATSynEdit;
begin
  if ACommand=cCommand_KeyEnter then
  begin
    MemoOnClickDbl(nil, AHandled);
    AHandled:= true;
    exit;
  end;

  if (ACommand>=cmdFirstAppCommand) and (ACommand<=cmdLastAppCommand) then
  begin
    FOnGetMainEditor(Ed);
    Ed.DoCommand(ACommand, TATCommandInvoke.Hotkey, '');
    AHandled:= true;
    exit;
  end;
end;


procedure TfmConsole.InputOnClick(Sender: TObject);
begin
  CloseFormAutoCompletion;
end;

procedure TfmConsole.MemoOnClick(Sender: TObject);
begin
  CloseFormAutoCompletion;
end;

procedure TfmConsole.MemoOnClickDbl(Sender: TObject; var AHandled: boolean);
var
  s: string;
  n: integer;
begin
  n:= EdMemo.Carets[0].PosY;
  if EdMemo.Strings.IsIndexValid(n) then
  begin
    s:= EdMemo.Strings.Lines[n];
    if StartsStr(cConsolePrompt, s) then
    begin
      Delete(s, 1, Length(cConsolePrompt));
      DoRunLine(s);
    end
    else
      DoNavigate(Self);
  end;
  AHandled:= true;
end;

procedure TfmConsole.ApplyTheme;
begin
  EditorApplyTheme(EdInput);
  EditorApplyTheme(EdMemo);
  Invalidate;
end;

procedure TfmConsole.ApplyCaretView;
begin
  EditorCaretShapeFromString(EdInput.CaretShapeNormal, EditorOps.OpCaretViewNormal);
  EditorCaretShapeFromString(EdInput.CaretShapeOverwrite, EditorOps.OpCaretViewOverwrite);

  EditorCaretShapeFromString(EdMemo.CaretShapeNormal, EditorOps.OpCaretViewNormal);
  EditorCaretShapeFromString(EdMemo.CaretShapeOverwrite, EditorOps.OpCaretViewOverwrite);
  EditorCaretShapeFromString(EdMemo.CaretShapeReadonly, EditorOps.OpCaretViewReadonly);
end;

procedure TfmConsole.FlushConsole;
var
  S: UnicodeString;
  NCnt, i: integer;
begin
  if not AppConsoleQueue.IsEmpty() then
  begin
    //avoid output of huge items count at once
    NCnt:= Min(AppConsoleQueue.Size, 300);
    for i:= 1 to NCnt do
    begin
      S:= AppConsoleQueue.Front();
      AppConsoleQueue.Pop();
      DoAddLine(S);
      if UiOps.LogConsole then
        MsgLogToFilename(S, AppFile_LogConsole, false);
    end;

    DoUpdateMemo;
  end;
end;

finalization
  if Assigned(fmConsole) then
    FreeAndNil(fmConsole);

end.
