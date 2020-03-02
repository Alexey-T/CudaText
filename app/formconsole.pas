(*
This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at http://mozilla.org/MPL/2.0/.

Copyright (c) Alexey Torgashin
*)
unit formconsole;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Menus, LclType,
  PythonEngine,
  ATStrings,
  ATSynEdit,
  ATSynEdit_Edits,
  ATSynEdit_Commands,
  ATSynEdit_Adapter_Simple,
  ATStringProc,
  ec_SyntAnal,
  ec_syntax_format,
  proc_py,
  proc_str,
  proc_colors,
  proc_globdata,
  proc_msg;

type
  TAppStrEvent = procedure(const Str: string) of object;
  TAppConsoleEvent = function(const Str: string): boolean of object;
  TAppConsoleCommandEvent = procedure(ACommand: integer; const AText: string; var AHandled: boolean) of object;

type
  { TfmConsole }
  TfmConsole = class(TForm)
    panelConsole: TPanel;
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
    FAdapter: TATAdapterSimple;
    FOnNavigate: TAppConsoleEvent;
    mnuTextClear: TMenuItem;
    mnuTextNav: TMenuItem;
    mnuTextWrap: TMenuItem;
    procedure ComboCommand(Sender: TObject; ACmd: integer; const AText: string; var AHandled: boolean);
    procedure DoGetLineColor(Ed: TATSynEdit; ALineIndex: integer; var AColorFont, AColorBg: TColor);
    procedure MemoClickDbl(Sender: TObject; var AHandled: boolean);
    procedure MemoCommand(Sender: TObject; ACmd: integer; const AText: string; var AHandled: boolean);
    procedure MemoContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
    procedure DoClearMemo(Sender: TObject);
    procedure DoNavigate(Sender: TObject);
    procedure DoToggleWrap(Sender: TObject);
    procedure SetIsDoubleBuffered(AValue: boolean);
    function GetWordWrap: boolean;
    procedure SetWordWrap(AValue: boolean);
    procedure DoRunLine(Str: string);
  public
    { public declarations }
    EdInput: TATComboEdit;
    EdMemo: TATSynEdit;
    property OnConsoleNav: TAppConsoleEvent read FOnNavigate write FOnNavigate;
    procedure DoAddLine(const AText: UnicodeString);
    procedure DoUpdateMemo;
    property IsDoubleBuffered: boolean write SetIsDoubleBuffered;
    property MemoWordWrap: boolean read GetWordWrap write SetWordWrap;
    procedure SetFocus; override;
  end;

var
  fmConsole: TfmConsole;

const
  cConsoleMaxLines = 1000;
  cConsoleMaxComboboxItems: integer = 20;
  cConsolePrompt = '>>> ';
  cConsolePrintPrefix = '=';

implementation

{$R *.lfm}

{ TfmConsole }

procedure TfmConsole.DoGetLineColor(Ed: TATSynEdit; ALineIndex: integer;
  var AColorFont, AColorBg: TColor);
var
  Str: atString;
  fmt: TecSyntaxFormat;
begin
  Str:= Ed.Strings.Lines[ALineIndex];
  if SBeginsWith(Str, cConsolePrompt) then
  begin
    fmt:= GetAppStyle(apstId2);
    AColorFont:= fmt.Font.Color
  end
  else
  if SBeginsWith(Str, 'NOTE: ') or
    (Str='Traceback (most recent call last):') or
    SRegexMatchesString(Str, '^[a-zA-Z][\w\.]*Error: .+', true) then
  begin
    fmt:= GetAppStyle(apstLightBG1);
    AColorFont:= fmt.Font.Color;
    AColorBg:= fmt.BgColor;
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
      Strs.LineAddRaw_NoUndo(AText, cEndUnix);
      for i:= 1 to Strs.Count-cConsoleMaxLines do
        Strs.LineDelete(0, false, false, false);
    end;

    ModeReadOnly:= true;
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
    UpdateWrapInfo(true);
    DoCommand(cCommand_GotoTextEnd);
    ColumnLeft:= 0;
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
  bNoLog:= SEndsWith(Str, ';');
  if bNoLog then
    Delete(Str, Length(Str), 1)
  else
    EdInput.DoAddLineToHistory(Utf8Decode(Str), cConsoleMaxComboboxItems);

  DoAddLine(cConsolePrompt+Str);
  DoUpdateMemo;

  try
    if SBeginsWith(Str, cConsolePrintPrefix) then
    begin
      Str:= 'print('+Copy(Str, 2, MaxInt) + ')';
      bExpr:= false;
    end
    else
      bExpr:= IsPythonExpression(Str);

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
  except
  end;
end;


procedure TfmConsole.FormCreate(Sender: TObject);
begin
  FAdapter:= TATAdapterSimple.Create(Self);
  FAdapter.OnGetLineColor:= @DoGetLineColor;

  EdInput:= TATComboEdit.Create(Self);
  EdInput.Parent:= Self;
  EdInput.Align:= alBottom;
  EdInput.WantTabs:= false;
  EdInput.TabStop:= true;
  EdInput.OnCommand:= @ComboCommand;

  EdInput.OptTabSize:= 4;
  EdInput.OptBorderWidth:= 1;
  EdInput.OptBorderWidthFocused:= 1;

  EdMemo:= TATSynEdit.Create(Self);
  EdMemo.Parent:= Self;
  EdMemo.Align:= alClient;
  EdMemo.BorderStyle:= bsNone;

  EdMemo.WantTabs:= false;
  EdMemo.TabStop:= true;
  EdMemo.AdapterForHilite:= FAdapter;

  IsDoubleBuffered:= UiOps.DoubleBuffered;

  EdMemo.OptWrapMode:= cWrapOn;
  EdMemo.OptScrollbarsNew:= true;
  EdMemo.OptUndoLimit:= 0;

  EdMemo.OptTabSize:= 4;
  EdMemo.OptBorderWidth:= 0;
  EdMemo.OptBorderWidthFocused:= 1;
  EdMemo.OptBorderFocusedActive:= UiOps.ShowActiveBorder;
  EdMemo.OptShowURLs:= false;
  EdMemo.OptCaretManyAllowed:= false;
  EdMemo.OptGutterVisible:= false;
  EdMemo.OptRulerVisible:= false;
  EdMemo.OptUnprintedVisible:= false;
  EdMemo.OptMarginRight:= 2000;
  EdMemo.OptCaretVirtual:= false;
  EdMemo.ModeReadOnly:= true;
  EdMemo.OptMouseRightClickMovesCaret:= true;
  EdMemo.OptMouseWheelZooms:= false;
  EdMemo.OptShowMouseSelFrame:= false;

  EdMemo.OnClickDouble:= @MemoClickDbl;
  EdMemo.OnCommand:= @MemoCommand;
  EdMemo.OnContextPopup:= @MemoContextPopup;
end;

procedure TfmConsole.ComboCommand(Sender: TObject; ACmd: integer;
  const AText: string; var AHandled: boolean);
var
  s: string;
begin
  if ACmd=cCommand_KeyEnter then
  begin
    s:= UTF8Encode(EdInput.Text);
    DoRunLine(s);

    EdInput.Text:= '';
    EdInput.DoCaretSingle(0, 0);

    AHandled:= true;
    Exit
  end;

  //if Assigned(FOnEditCommand) then
  //  FOnEditCommand(ACmd, AText, AHandled);
end;

function TfmConsole.GetWordWrap: boolean;
begin
  Result:= EdMemo.OptWrapMode=cWrapOn;
end;

procedure TfmConsole.DoClearMemo(Sender: TObject);
begin
  EdMemo.ModeReadOnly:= false;
  EdMemo.Text:= '';
  EdMemo.DoCaretSingle(0, 0);
  EdMemo.ModeReadOnly:= true;
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
    fmConsole.EdMemo.OptWrapMode:= cWrapOn
  else
    fmConsole.EdMemo.OptWrapMode:= cWrapOff;
  //fmConsole.EdMemo.OptAllowScrollbarHorz:= not AValue;
  fmConsole.EdMemo.Update;
end;

procedure TfmConsole.MemoCommand(Sender: TObject; ACmd: integer;
  const AText: string; var AHandled: boolean);
begin
  if ACmd=cCommand_KeyEnter then
  begin
    MemoClickDbl(nil, AHandled);
    AHandled:= true;
  end;
end;


procedure TfmConsole.MemoClickDbl(Sender: TObject; var AHandled: boolean);
var
  s: string;
  n: integer;
begin
  n:= EdMemo.Carets[0].PosY;
  if EdMemo.Strings.IsIndexValid(n) then
  begin
    s:= EdMemo.Strings.Lines[n];
    if SBeginsWith(s, cConsolePrompt) then
    begin
      Delete(s, 1, Length(cConsolePrompt));
      DoRunLine(s);
    end
    else
      DoNavigate(Self);
  end;
  AHandled:= true;
end;

end.

