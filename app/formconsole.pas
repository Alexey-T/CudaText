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
  TAppConsoleEvent = function(const Str: string): boolean of object;

type
  { TfmConsole }
  TfmConsole = class(TForm)
    panelConsole: TPanel;
    PopupMenu1: TPopupMenu;
    procedure FormCreate(Sender: TObject);
    procedure MemoClickDbl(Sender: TObject; var AHandled: boolean);
  private
    { private declarations }
    FOnConsole: TAppConsoleEvent;
    procedure ComboCommand(Snd: TObject; ACmd: integer; var AHandled: boolean);
    procedure DoClearMemo(Sender: TObject);
    procedure MemoCommand(Snd: TObject; ACmd: integer; var AHandled: boolean);
  public
    { public declarations }
    ed: TATComboEdit;
    memo: TATSynEdit;
    property OnConsole: TAppConsoleEvent read FOnConsole write FOnConsole;
    procedure DoLogConsoleLine(const Str: string);
    procedure DoExecuteConsoleLine(Str: string);
  end;

var
  fmConsole: TfmConsole;

const
  cPyConsoleMaxLines = 1000;
  cPyConsoleMaxComboItems: integer = 20;
  cPyConsolePrompt = '>>> ';

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
    Update;
  end;
end;

procedure TfmConsole.DoExecuteConsoleLine(Str: string);
begin
  DoLogConsoleLine(cPyConsolePrompt+Str);

  if Assigned(FOnConsole) then
    if not FOnConsole(Str) then exit;

  if (Str<>'') and (Str[1]='=') then
    Str:= 'print('+Copy(Str, 2, MaxInt) + ')';

  try
    GetPythonEngine.ExecString(Str);
  except
  end;
end;


procedure TfmConsole.FormCreate(Sender: TObject);
var
  mi: TMenuItem;
begin
  ed:= TATComboEdit.Create(Self);
  ed.Parent:= Self;
  ed.Align:= alBottom;
  ed.BorderStyle:= bsSingle;

  memo:= TATSynEdit.Create(Self);
  memo.Parent:= Self;
  memo.Align:= alClient;
  memo.BorderStyle:= bsNone;

  memo.OptCaretManyAllowed:= false;
  memo.OptGutterVisible:= false;
  memo.OptRulerVisible:= false;
  memo.OptUnprintedVisible:= false;
  memo.OptMarginRight:= 2000;
  memo.OptCaretVirtual:= false;
  memo.ModeReadOnly:= true;
  memo.OptAllowScrollbarHorz:= false;

  ed.OnCommand:= @ComboCommand;
  memo.OnClickDouble:= @MemoClickDbl;
  memo.OnCommand:=@MemoCommand;

  ed.WantTabs:= false;
  ed.TabStop:= true;
  memo.WantTabs:= false;
  memo.TabStop:= true;

  ed.OptTabSize:= 4;
  memo.OptTabSize:= 4;

  mi:= TMenuItem.Create(Self);
  mi.Caption:= 'Clear';
  mi.OnClick:=@DoClearMemo;
  memo.PopupTextDefault.Items.Add(mi);
end;

procedure TfmConsole.ComboCommand(Snd: TObject; ACmd: integer;
  var AHandled: boolean);
var
  s: string;
begin
  if ACmd=cCommand_KeyEnter then
  begin
    s:= UTF8Encode(ed.Text);
    DoExecuteConsoleLine(s);

    ed.DoAddLineToHistory(Utf8Decode(s), cPyConsoleMaxComboItems);
    ed.Text:= '';
    ed.DoCaretSingle(0, 0);

    AHandled:= true;
  end;
end;

procedure TfmConsole.DoClearMemo(Sender: TObject);
begin
  memo.ModeReadOnly:= false;
  memo.Text:= '';
  memo.DoCaretSingle(0, 0);
  memo.ModeReadOnly:= true;
end;

procedure TfmConsole.MemoCommand(Snd: TObject; ACmd: integer;
  var AHandled: boolean);
begin
  if ACmd=cCommand_KeyEnter then
  begin
    MemoClickDbl(nil, AHandled);
    AHandled:= true;
  end;
end;


procedure TfmConsole.MemoClickDbl(Sender: TObject; var AHandled: boolean);
var
  n: Integer;
  s: atString;
begin
  with memo do
  begin
    n:= Carets[0].PosY;
    if (n>=0) and (n<Strings.Count) then
    begin
      s:= Strings.Lines[n];
      if SBeginsWith(s, cPyConsolePrompt) then
      begin
        Delete(s, 1, Length(cPyConsolePrompt));
        DoExecuteConsoleLine(s);
      end;
    end;
  end;
  AHandled:= true;
end;

end.

