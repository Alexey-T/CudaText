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
  ATStringProc,
  proc_globdata,
  proc_colors;

type
  TAppStrEvent = procedure(const Str: string) of object;
  TAppConsoleEvent = function(const Str: string): boolean of object;
  TAppConsoleCommandEvent = procedure(ACommand: integer; const AText: string; var AHandled: boolean) of object;

type
  { TfmConsole }
  TfmConsole = class(TForm)
    panelConsole: TPanel;
    PopupMenu1: TPopupMenu;
    procedure FormCreate(Sender: TObject);
    procedure MemoClickDbl(Sender: TObject; var AHandled: boolean);
  private
    { private declarations }
    FOnConsoleInput: TAppConsoleEvent;
    FOnConsolePrint: TAppStrEvent;
    FOnNavigate: TAppConsoleEvent;
    procedure ComboCommand(Sender: TObject; ACmd: integer; const AText: string; var AHandled: boolean);
    function GetWrap: boolean;
    procedure MemoCommand(Sender: TObject; ACmd: integer; const AText: string; var AHandled: boolean);
    procedure DoClearMemo(Sender: TObject);
    procedure DoNavigate(Sender: TObject);
    procedure DoToggleWrap(Sender: TObject);
    procedure SetIsDoubleBuffered(AValue: boolean);
    procedure SetWrap(AValue: boolean);
  public
    { public declarations }
    ed: TATComboEdit;
    memo: TATSynEdit;
    mnuTextClear: TMenuItem;
    mnuTextNav: TMenuItem;
    mnuTextWrap: TMenuItem;
    property OnConsoleInput: TAppConsoleEvent read FOnConsoleInput write FOnConsoleInput;
    property OnConsolePrint: TAppStrEvent read FOnConsolePrint write FOnConsolePrint;
    property OnConsoleNav: TAppConsoleEvent read FOnNavigate write FOnNavigate;
    procedure DoLogConsoleLine(const Str: string);
    procedure DoExecuteConsoleLine(Str: string);
    property IsDoubleBuffered: boolean write SetIsDoubleBuffered;
    property Wordwrap: boolean read GetWrap write SetWrap;
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

procedure TfmConsole.DoLogConsoleLine(const Str: string);
begin
  with memo do
  begin
    ModeReadOnly:= false;
    while Strings.Count>cPyConsoleMaxLines do
      Strings.LineDelete(0);
    Strings.LineAdd(Utf8Decode(Str));
    ModeReadOnly:= true;

    Update(true);
    Application.ProcessMessages;

    DoCommand(cCommand_GotoTextEnd);
    ColumnLeft:= 0;
    Update;
  end;
end;

procedure TfmConsole.DoExecuteConsoleLine(Str: string);
var
  bNoLog: boolean;
begin
  bNoLog:= SEndsWith(Str, ';');
  if bNoLog then
    Delete(Str, Length(Str), 1);

  if Assigned(FOnConsolePrint) then
    FOnConsolePrint(cPyConsolePrompt+Str);

  if not bNoLog then
  begin
    ed.DoAddLineToHistory(Utf8Decode(Str), cPyConsoleMaxComboItems);
  end;

  if Assigned(FOnConsoleInput) then
    if not FOnConsoleInput(Str) then exit;

  if SBeginsWith(Str, cPyCharPrint) then
    Str:= 'print('+Copy(Str, 2, MaxInt) + ')';

  try
    GetPythonEngine.ExecString(Str);
  except
  end;
end;


procedure TfmConsole.FormCreate(Sender: TObject);
begin
  ed:= TATComboEdit.Create(Self);
  ed.Parent:= Self;
  ed.Align:= alBottom;

  ed.OnCommand:= @ComboCommand;

  ed.WantTabs:= false;
  ed.TabStop:= true;
  ed.OptTabSize:= 4;
  ed.OptBorderWidth:= 1;
  ed.OptBorderWidthFocused:= 1;
  ed.OptBorderFocusedActive:= UiOps.ShowActiveBorder;

  memo:= TATSynEdit.Create(Self);
  memo.Parent:= Self;
  memo.Align:= alClient;
  memo.BorderStyle:= bsNone;

  memo.WantTabs:= false;
  memo.TabStop:= true;

  IsDoubleBuffered:= UiOps.DoubleBuffered;

  //Linux h-scroll paints bad (some gtk2 bug) so i disabled it
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

  memo.OnClickDouble:= @MemoClickDbl;
  memo.OnCommand:= @MemoCommand;

  //menu items
  mnuTextClear:= TMenuItem.Create(Self);
  mnuTextClear.Caption:= 'Clear';
  mnuTextClear.OnClick:= @DoClearMemo;
  memo.PopupTextDefault.Items.Add(mnuTextClear);

  mnuTextWrap:= TMenuItem.Create(Self);
  mnuTextWrap.Caption:= 'Toggle word wrap';
  mnuTextWrap.OnClick:= @DoToggleWrap;
  memo.PopupTextDefault.Items.Add(mnuTextWrap);

  mnuTextNav:= TMenuItem.Create(Self);
  mnuTextNav.Caption:= 'Navigate';
  mnuTextNav.OnClick:= @DoNavigate;
  memo.PopupTextDefault.Items.Add(mnuTextNav);

  AutoAdjustLayout(lapAutoAdjustForDPI, 96, Screen.PixelsPerInch, 100, 100);
end;

procedure TfmConsole.ComboCommand(Sender: TObject; ACmd: integer;
  const AText: string; var AHandled: boolean);
var
  s: string;
begin
  if ACmd=cCommand_KeyEnter then
  begin
    s:= UTF8Encode(ed.Text);
    DoExecuteConsoleLine(s);

    ed.Text:= '';
    ed.DoCaretSingle(0, 0);

    AHandled:= true;
    Exit
  end;

  //if Assigned(FOnEditCommand) then
  //  FOnEditCommand(ACmd, AText, AHandled);
end;

function TfmConsole.GetWrap: boolean;
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
  S: atString;
  N: integer;
begin
  if Assigned(FOnNavigate) then
  begin
    N:= Memo.Carets[0].PosY;
    if not Memo.Strings.IsIndexValid(N) then exit;
    S:= Memo.Strings.Lines[N];
    FOnNavigate(Utf8Encode(S));
  end;
end;

procedure TfmConsole.DoToggleWrap(Sender: TObject);
begin
  WordWrap:= not WordWrap;
end;

procedure TfmConsole.SetIsDoubleBuffered(AValue: boolean);
begin
  ed.DoubleBuffered:= AValue;
  memo.DoubleBuffered:= AValue;
end;

procedure TfmConsole.SetWrap(AValue: boolean);
begin
  if AValue then
    fmConsole.memo.OptWrapMode:= cWrapOn
  else
    fmConsole.memo.OptWrapMode:= cWrapOff;
  fmConsole.memo.OptAllowScrollbarHorz:= AValue;
  fmConsole.memo.Update;

  mnuTextWrap.Checked:= AValue;
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
  s: atString;
  n: integer;
begin
  n:= Memo.Carets[0].PosY;
  if Memo.Strings.IsIndexValid(n) then
  begin
    s:= Memo.Strings.Lines[n];
    if SBeginsWith(s, cPyConsolePrompt) then
    begin
      Delete(s, 1, Length(cPyConsolePrompt));
      DoExecuteConsoleLine(Utf8Encode(s));
    end
    else
      DoNavigate(Self);
  end;
  AHandled:= true;
end;

end.

