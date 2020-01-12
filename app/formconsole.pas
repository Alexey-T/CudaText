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
  ATSynEdit,
  ATSynEdit_Edits,
  ATSynEdit_Commands,
  ATSynEdit_Adapter_Simple,
  ATStringProc,
  ec_SyntAnal,
  ec_syntax_format,
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
    ed: TATComboEdit;
    memo: TATSynEdit;
    property OnConsoleNav: TAppConsoleEvent read FOnNavigate write FOnNavigate;
    procedure DoAddLine(const Str: string);
    procedure DoUpdate;
    procedure DoScrollToEnd(AllowProcessMsg: boolean);
    property IsDoubleBuffered: boolean write SetIsDoubleBuffered;
    property MemoWordWrap: boolean read GetWordWrap write SetWordWrap;
  end;

var
  fmConsole: TfmConsole;

const
  cPyConsoleMaxLines = 1000;
  cPyConsoleMaxComboItems: integer = 20;
  cPyConsolePrompt = '>>> ';
  cPyCharPrint = '=';

implementation

{$R *.lfm}

{ TfmConsole }

procedure TfmConsole.DoGetLineColor(Ed: TATSynEdit; ALineIndex: integer;
  var AColorFont, AColorBg: TColor);
var
  Str: string;
  fmt: TecSyntaxFormat;
begin
  Str:= Ed.Strings.LinesUTF8[ALineIndex];
  if SBeginsWith(Str, cPyConsolePrompt) then
  begin
    fmt:= AppStyleId2;
    AColorFont:= fmt.Font.Color
  end
  else
  if SBeginsWith(Str, 'NOTE: ') or
    (Str='Traceback (most recent call last):') or
    SRegexMatchesString(Str, '^[a-zA-Z][\w\.]*Error: .+', true) then
  begin
    fmt:= AppStyleError;
    AColorFont:= fmt.Font.Color;
    AColorBg:= fmt.BgColor;
  end;
end;

procedure TfmConsole.DoAddLine(const Str: string);
begin
  with memo do
  begin
    ModeReadOnly:= false;

    //this is to remove 1st empty line
    if (Strings.Count=1) and (Strings.LinesUTF8[0]='') then
      Strings.LinesUTF8[0]:= Str
    else
      Strings.LineAddRaw_UTF8_NoUndo(Str, cEndUnix);

    ModeReadOnly:= true;
  end;
end;

procedure TfmConsole.DoUpdate;
//DoUpdate must be called after 1+ DoAddLine calls
//It's called after N calls in main form
//
//Note: don't call Application.ProcessMessages here!
// https://github.com/Alexey-T/CudaText/issues/2326
begin
  with memo do
  begin
    if Strings.Count>cPyConsoleMaxLines then
    begin
      ModeReadOnly:= false;
      while Strings.Count>cPyConsoleMaxLines do
        Strings.LineDelete(0);
      //if Strings.LinesUTF8[0]='' then
      //  Strings.LineDelete(0);
      ModeReadOnly:= true;
    end;

    DoCommand(cCommand_GotoTextEnd);
    ColumnLeft:= 0;
    Update(true);
  end;
end;

procedure TfmConsole.DoScrollToEnd(AllowProcessMsg: boolean);
begin
  if AllowProcessMsg then
    Application.ProcessMessages;
  memo.DoScrollToBeginOrEnd(false);
  memo.Update;
end;

{$define py_always_eval}
procedure TfmConsole.DoRunLine(Str: string);
var
  bNoLog: boolean;
begin
  bNoLog:= SEndsWith(Str, ';');
  if bNoLog then
    Delete(Str, Length(Str), 1)
  else
    ed.DoAddLineToHistory(Utf8Decode(Str), cPyConsoleMaxComboItems);

  DoAddLine(cPyConsolePrompt+Str);
  DoUpdate;
  DoScrollToEnd(true);

  try
    {$ifdef PY_ALWAYS_EVAL}
    if SBeginsWith(Str, cPyCharPrint) then
      GetPythonEngine.ExecString('print('+Copy(Str, 2, MaxInt) + ')')
    else
    if not IsPythonExpression(Str) then
      GetPythonEngine.ExecString(Str)
    else
      GetPythonEngine.Run_CommandAsString('print('+Str+')', file_input);

    {$else}
    if SBeginsWith(Str, cPyCharPrint) then
      Str:= 'print('+Copy(Str, 2, MaxInt) + ')';
    GetPythonEngine.ExecString(Str);
    {$endif}
  except
  end;
end;


procedure TfmConsole.FormCreate(Sender: TObject);
begin
  FAdapter:= TATAdapterSimple.Create(Self);
  FAdapter.OnGetLineColor:= @DoGetLineColor;

  ed:= TATComboEdit.Create(Self);
  ed.Parent:= Self;
  ed.Align:= alBottom;
  ed.WantTabs:= false;
  ed.TabStop:= true;
  ed.OnCommand:= @ComboCommand;

  ed.OptTabSize:= 4;
  ed.OptBorderWidth:= 1;
  ed.OptBorderWidthFocused:= 1;

  memo:= TATSynEdit.Create(Self);
  memo.Parent:= Self;
  memo.Align:= alClient;
  memo.BorderStyle:= bsNone;

  memo.WantTabs:= false;
  memo.TabStop:= true;
  memo.AdapterForHilite:= FAdapter;

  IsDoubleBuffered:= UiOps.DoubleBuffered;

  memo.OptWrapMode:= cWrapOn;
  memo.OptScrollbarsNew:= true;
  memo.OptUndoLimit:= 0;

  memo.OptTabSize:= 4;
  memo.OptBorderWidth:= 0;
  memo.OptBorderWidthFocused:= 1;
  memo.OptBorderFocusedActive:= UiOps.ShowActiveBorder;
  memo.OptShowURLs:= false;
  memo.OptCaretManyAllowed:= false;
  memo.OptGutterVisible:= false;
  memo.OptRulerVisible:= false;
  memo.OptUnprintedVisible:= false;
  memo.OptMarginRight:= 2000;
  memo.OptCaretVirtual:= false;
  memo.ModeReadOnly:= true;
  memo.OptMouseRightClickMovesCaret:= true;
  memo.OptMouseWheelZooms:= false;
  memo.OptShowMouseSelFrame:= false;

  memo.OnClickDouble:= @MemoClickDbl;
  memo.OnCommand:= @MemoCommand;
  memo.OnContextPopup:= @MemoContextPopup;
end;

procedure TfmConsole.ComboCommand(Sender: TObject; ACmd: integer;
  const AText: string; var AHandled: boolean);
var
  s: string;
begin
  if ACmd=cCommand_KeyEnter then
  begin
    s:= UTF8Encode(ed.Text);
    DoRunLine(s);

    ed.Text:= '';
    ed.DoCaretSingle(0, 0);

    AHandled:= true;
    Exit
  end;

  //if Assigned(FOnEditCommand) then
  //  FOnEditCommand(ACmd, AText, AHandled);
end;

function TfmConsole.GetWordWrap: boolean;
begin
  Result:= memo.OptWrapMode=cWrapOn;
end;

procedure TfmConsole.DoClearMemo(Sender: TObject);
begin
  memo.ModeReadOnly:= false;
  memo.Text:= '';
  memo.DoCaretSingle(0, 0);
  memo.ModeReadOnly:= true;
end;

procedure TfmConsole.DoNavigate(Sender: TObject);
var
  S: string;
  N: integer;
begin
  if Assigned(FOnNavigate) then
  begin
    N:= Memo.Carets[0].PosY;
    if not Memo.Strings.IsIndexValid(N) then exit;
    S:= Memo.Strings.LinesUTF8[N];
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
    memo.PopupTextDefault.Items.Add(mnuTextClear);

    mnuTextWrap:= TMenuItem.Create(Self);
    mnuTextWrap.OnClick:= @DoToggleWrap;
    memo.PopupTextDefault.Items.Add(mnuTextWrap);

    mnuTextNav:= TMenuItem.Create(Self);
    mnuTextNav.OnClick:= @DoNavigate;
    memo.PopupTextDefault.Items.Add(mnuTextNav);
  end;

  mnuTextClear.Caption:= msgConsoleClear;
  mnuTextWrap.Caption:= msgConsoleToggleWrap;
  mnuTextNav.Caption:= msgConsoleNavigate;
  mnuTextWrap.Checked:= MemoWordWrap;

  Handled:= false;
end;

procedure TfmConsole.SetIsDoubleBuffered(AValue: boolean);
begin
  ed.DoubleBuffered:= AValue;
  memo.DoubleBuffered:= AValue;
end;

procedure TfmConsole.SetWordWrap(AValue: boolean);
begin
  if AValue then
    fmConsole.memo.OptWrapMode:= cWrapOn
  else
    fmConsole.memo.OptWrapMode:= cWrapOff;
  //fmConsole.memo.OptAllowScrollbarHorz:= not AValue;
  fmConsole.memo.Update;
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
  n:= Memo.Carets[0].PosY;
  if Memo.Strings.IsIndexValid(n) then
  begin
    s:= Memo.Strings.LinesUTF8[n];
    if SBeginsWith(s, cPyConsolePrompt) then
    begin
      Delete(s, 1, Length(cPyConsolePrompt));
      DoRunLine(s);
    end
    else
      DoNavigate(Self);
  end;
  AHandled:= true;
end;

end.

