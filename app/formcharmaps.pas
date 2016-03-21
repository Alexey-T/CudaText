(*
This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at http://mozilla.org/MPL/2.0/.

Copyright (c) Alexey Torgashin
*)

unit formcharmaps;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Grids, IniFiles,
  LclType, LclProc, LCLUnicodeData, LConvEncoding,
  LazUTF8, LazFileUtils,
  {$ifdef windows} Windows, {$endif}
  proc_msg,
  proc_globdata;

type
  TCharmapInsertEvent = procedure(const Str: string) of object;

type
  { TfmCharmaps }
  TfmCharmaps = class(TForm)
    btnAnsi: TButton;
    btnClose: TButton;
    btnUnicode: TButton;
    comboAnsi: TComboBox;
    comboUnicode: TComboBox;
    LabelInfo: TLabel;
    PanelInfo: TPanel;
    PanelBtm: TPanel;
    Grid: TStringGrid;
    procedure btnAnsiClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure btnUnicodeClick(Sender: TObject);
    procedure comboAnsiChange(Sender: TObject);
    procedure comboUnicodeChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure GridMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure GridMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure GridSelectCell(Sender: TObject; aCol, aRow: Integer; var CanSelect: Boolean);
  private
    FOnInsert: TCharmapInsertEvent;
    FUnicode: boolean;
    FUnicodeBegin: integer;
    function CodeToString(code: integer): string;
    function DoGetCode(aCol, aRow: integer): integer;
    procedure DoShowAnsi;
    procedure DoShowUnicode;
    procedure DoFormAutosize;
    procedure DoInsert(aCol, aRow: integer);
    procedure DoShowStatus(aCol, aRow: integer);
    function GetCodepage: string;
    { private declarations }
  public
    { public declarations }
    AllowUnicodeAfterFFFF: boolean;
    MsgStatusAnsi: string;
    MsgStatusUnicode: string;
    InitialStr: string;
    property OnInsert: TCharmapInsertEvent read FOnInsert write FOnInsert;
  end;

var
  fmCharmaps: TfmCharmaps;

function DoDialogCharmapModal(const ALangFilename: string): string;
procedure DoLocalize_FormCharmap(F: TfmCharmaps);


implementation

{$R *.lfm}

procedure DoLocalize_FormCharmap(F: TfmCharmaps);
const
  section = 'd_charmap';
var
  ini: TIniFile;
  fn: string;
begin
  fn:= GetAppLangFilename;
  if not FileExists(fn) then exit;
  ini:= TIniFile.Create(fn);
  try
    with F do Caption:= ini.ReadString(section, '_', Caption);
    with F.btnClose do Caption:= msgButtonClose;
    with F.btnAnsi do Caption:= ini.ReadString(section, 'mod1', Caption);
    with F.btnUnicode do Caption:= ini.ReadString(section, 'mod2', Caption);
    F.MsgStatusAnsi:= ini.ReadString(section, 'stat1', F.MsgStatusAnsi);
    F.MsgStatusUnicode:= ini.ReadString(section, 'stat2', F.MsgStatusUnicode);
  finally
    FreeAndNil(ini);
  end;
end;


type
  TDummy = class
  public
    StrVal: string;
    Form: TForm;
    procedure OnInsert(const S: string);
  end;

procedure TDummy.OnInsert(const S: string);
begin
  StrVal:= S;
  if Assigned(Form) then Form.Close;
end;

function DoDialogCharmapModal(const ALangFilename: string): string;
var
  F: TfmCharmaps;
  Dummy: TDummy;
begin
  F:= TfmCharmaps.Create(nil);
  Dummy:= TDummy.Create;
  Dummy.Form:= F;

  try
    DoLocalize_FormCharmap(F);
    F.OnInsert:= @Dummy.OnInsert;
    F.ShowModal;
    Result:= Dummy.StrVal;
  finally
    FreeAndNil(Dummy);
    FreeAndNil(F);
  end;
end;

{ TfmCharmaps }

procedure TfmCharmaps.FormShow(Sender: TObject);
var
  str: string;
  cp, i, j: integer;
begin
  {$ifdef windows}
  //select comboAnsi item for system codepage
  cp:= Windows.GetACP;
  case cp of
    437..1258: str:= 'cp'+IntToStr(cp)+' ';
    else str := '?';
  end;
  for i:= 0 to comboAnsi.Items.Count-1 do
    if Pos(str, comboAnsi.Items[i])=1 then
    begin
      comboAnsi.ItemIndex:= i;
      comboAnsiChange(nil);
      break;
    end;
  {$endif}

  if not FUnicode then
    if InitialStr<>'' then
      for i:= 1 to 16 do
        for j:= 1 to 16 do
          if CodeToString(DoGetCode(i, j))=InitialStr then
          begin
            Grid.Col:= i;
            Grid.Row:= j;
            Break
          end;
end;

procedure TfmCharmaps.GridMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  i, j: integer;
begin
  if Grid.MouseToGridZone(X, Y)<>gzNormal then exit;
  if Button=mbLeft then
  begin
    Grid.MouseToCell(X, Y, i, j);
    DoInsert(i, j);
  end;
end;

procedure TfmCharmaps.GridMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  i, j: integer;
begin
  if Grid.MouseToGridZone(X, Y)<>gzNormal then exit;
  Grid.MouseToCell(X, Y, i, j);
  DoShowStatus(i, j);
end;

function TfmCharmaps.DoGetCode(aCol, aRow: integer): integer;
begin
  if not FUnicode then
    Result:= aCol-1 + (aRow-1)*16
  else
    Result:= aCol + aRow*16 + FUnicodeBegin;
end;

function TfmCharmaps.CodeToString(code: integer): string;
begin
  if FUnicode then
    Result:= UnicodeToUTF8(code)
  else
  if code>=0 then
    Result:= ConvertEncoding(Chr(code), GetCodepage, 'utf8')
  else
    Result:= '';
end;

procedure TfmCharmaps.DoShowStatus(aCol, aRow: integer);
var
  code: integer;
  str: string;
begin
  code:= DoGetCode(aCol, aRow);
  str:= CodeToString(code);
  if code=0 then str:= '';

  if not FUnicode then
    LabelInfo.Caption:= Format(MsgStatusAnsi, [code, IntToHex(code, 2), str])
  else
    LabelInfo.Caption:= Format(MsgStatusUnicode, [IntToHex(code, 4), str]);
end;

procedure TfmCharmaps.GridSelectCell(Sender: TObject; aCol, aRow: Integer;
  var CanSelect: Boolean);
begin
  DoShowStatus(aCol, aRow);
end;

function TfmCharmaps.GetCodepage: string;
var
  n: integer;
begin
  Result:= comboAnsi.Items[comboAnsi.ItemIndex];
  n:= Pos(' ', Result);
  if n>0 then SetLength(Result, n-1);
end;

procedure TfmCharmaps.DoShowAnsi;
var
  i, j, code: integer;
begin
  FUnicode:= false;
  comboAnsi.Visible:= not FUnicode;
  comboUnicode.Visible:= FUnicode;

  Grid.Clear;
  Grid.RowCount:= 17;
  Grid.ColCount:= 17;
  Grid.FixedCols:= 1;
  Grid.FixedRows:= 1;

  for i:= 1 to 16 do
  begin
    Grid.Cells[i, 0]:= Format('%2.2d', [Pred(i)]);
    Grid.Cells[0, i]:= Format('%3.3d+', [Pred(i)*16]);
  end;

  for i:= 1 to 16 do
    for j:= 1 to 16 do
    begin
      code:= i-1 + (j-1)*16;
      if (code=0) or (code=8) or (code=9) then Continue;
      Grid.Cells[i, j]:= CodeToString(code);
    end;

  DoShowStatus(Grid.Col, Grid.Row);
  DoFormAutosize;
end;

procedure TfmCharmaps.DoShowUnicode;
const
  cSize=16;
var
  nBegin, nEnd, i: integer;
begin
  FUnicode:= true;
  comboAnsi.Visible:= not FUnicode;
  comboUnicode.Visible:= FUnicode;

  PanelInfo.Top:= 10;
  Grid.Clear;

  if comboUnicode.ItemIndex<0 then exit;
  nBegin:= UnicodeBlocks[comboUnicode.ItemIndex].S;
  nEnd:= UnicodeBlocks[comboUnicode.ItemIndex].E;
  FUnicodeBegin:= nBegin;

  Grid.ColCount:= cSize;
  Grid.RowCount:= (nEnd-nBegin) div cSize +1;
  Grid.FixedCols:= 0;
  Grid.FixedRows:= 0;

  for i:= nBegin to nEnd do
    Grid.Cells[(i-nBegin) mod cSize, (i-nBegin) div cSize]:= CodeToString(i);

  Grid.Row:= 0;
  Grid.Col:= 0;
  DoShowStatus(0, 0);
  DoFormAutosize;
end;

procedure TfmCharmaps.FormCreate(Sender: TObject);
var
  i: integer;
begin
  MsgStatusAnsi:= 'Decimal %d, Hex %s, Char "%s"';
  MsgStatusUnicode:= 'U+%s, Char "%s"';

  comboUnicode.Items.Clear;
  for i:= Low(UnicodeBlocks) to High(UnicodeBlocks) do
    if AllowUnicodeAfterFFFF or (UnicodeBlocks[i].E<=$FFFF) then
      comboUnicode.Items.Add(UnicodeBlocks[i].PG);
  comboUnicode.ItemIndex:= 0;

  DoShowAnsi;
end;

procedure TfmCharmaps.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key=VK_RETURN then
    if Grid.Focused then
    begin
      DoInsert(Grid.Col, Grid.Row);
      Key:= 0;
      exit;
    end;

  if Key=VK_ESCAPE then
  begin
    Close;
    Key:= 0;
    exit
  end;

  if Key=Ord('1') then
  begin
    DoShowAnsi;
    Key:= 0;
    exit
  end;

  if Key=Ord('2') then
  begin
    DoShowUnicode;
    Key:= 0;
    exit
  end;
end;

procedure TfmCharmaps.DoFormAutosize;
begin
  Grid.AutoSizeColumns;
  //ClientHeight:= 17{fixed}*(Grid.RowHeights[1]+1) + 6 + PanelBtm.Height;
end;

procedure TfmCharmaps.btnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TfmCharmaps.btnUnicodeClick(Sender: TObject);
begin
  DoShowUnicode;
end;

procedure TfmCharmaps.comboAnsiChange(Sender: TObject);
begin
  if not FUnicode then
    DoShowAnsi;
end;

procedure TfmCharmaps.comboUnicodeChange(Sender: TObject);
begin
  DoShowUnicode;
end;

procedure TfmCharmaps.btnAnsiClick(Sender: TObject);
begin
  DoShowAnsi;
end;

procedure TfmCharmaps.DoInsert(aCol, aRow: integer);
var
  code: integer;
  str: string;
begin
  code:= DoGetCode(aCol, aRow);
  str:= CodeToString(code);

  if Assigned(OnInsert) then
    OnInsert(str);
end;

end.

