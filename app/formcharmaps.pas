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
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Grids, LclType, LclProc, LCLUnicodeData;

type
  TCharmapInsertEvent = procedure(const Str: string) of object;

type
  { TfmCharmaps }
  TfmCharmaps = class(TForm)
    btnAnsi: TButton;
    btnClose: TButton;
    btnUnicode: TButton;
    comboUnicode: TComboBox;
    LabelInfo: TLabel;
    PanelInfo: TPanel;
    PanelBtm: TPanel;
    Grid: TStringGrid;
    procedure btnAnsiClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure btnUnicodeClick(Sender: TObject);
    procedure comboUnicodeChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure GridKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure GridMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure GridMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure GridSelectCell(Sender: TObject; aCol, aRow: Integer; var CanSelect: Boolean);
  private
    FOnInsert: TCharmapInsertEvent;
    FUnicode: boolean;
    FUnicodeBegin: integer;
    function DoGetCode(aCol, aRow: integer): integer;
    procedure DoShowAnsi;
    procedure DoShowUnicode;
    procedure DoFormAutosize;
    procedure DoInsert(aCol, aRow: integer);
    procedure DoShowStatus(aCol, aRow: integer);
    { private declarations }
  public
    { public declarations }
    AllowUnicodeAfterFFFF: boolean;
    InitialAsciiCode: byte;
    property OnInsert: TCharmapInsertEvent read FOnInsert write FOnInsert;
  end;

var
  fmCharmaps: TfmCharmaps;

function DoDialogCharmapModal: string;

implementation

{$R *.lfm}

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

function DoDialogCharmapModal: string;
var
  F: TfmCharmaps;
  Dummy: TDummy;
begin
  F:= TfmCharmaps.Create(nil);
  Dummy:= TDummy.Create;
  Dummy.Form:= F;

  try
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
begin
  if not FUnicode then
    if InitialAsciiCode>0 then
    begin
      Grid.Col:= InitialAsciiCode mod 16 +1;
      Grid.Row:= InitialAsciiCode div 16 +1;
    end;
end;

procedure TfmCharmaps.GridKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key=VK_RETURN then
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

function CodeToString(code: integer): string;
begin
  if code>=0 then
    Result:= UnicodeToUTF8(code)
  else
    Result:= '';
end;

procedure TfmCharmaps.DoShowStatus(aCol, aRow: integer);
var
  code: integer;
  str: string;
begin
  code:= DoGetCode(aCol, aRow);
  if not FUnicode then
    LabelInfo.Caption:= Format('Decimal %d, Hex %s', [code, IntToHex(code, 2)])
  else
    LabelInfo.Caption:= Format('U+%s', [IntToHex(code, 4)]);

  str:= CodeToString(code);
  LabelInfo.Caption:= LabelInfo.Caption+Format(', Char "%s"', [str]);
end;

procedure TfmCharmaps.GridSelectCell(Sender: TObject; aCol, aRow: Integer;
  var CanSelect: Boolean);
begin
  DoShowStatus(aCol, aRow);
end;

procedure TfmCharmaps.DoShowAnsi;
var
  i, j, code: integer;
begin
  FUnicode:= false;
  PanelInfo.Hide;

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
      Grid.Cells[i, j]:= AnsiToUtf8(Chr(code));
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
  PanelInfo.Show;
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
  comboUnicode.Items.Clear;
  for i:= Low(UnicodeBlocks) to High(UnicodeBlocks) do
    if AllowUnicodeAfterFFFF or (UnicodeBlocks[i].E<=$FFFF) then
      comboUnicode.Items.Add(UnicodeBlocks[i].PG);
  comboUnicode.ItemIndex:= 0;

  DoShowAnsi;
end;

procedure TfmCharmaps.DoFormAutosize;
begin
  Grid.AutoSizeColumns;
  ClientHeight:= 17{fixed}*(Grid.RowHeights[1]+1) + 6 + PanelBtm.Height;
end;

procedure TfmCharmaps.btnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TfmCharmaps.btnUnicodeClick(Sender: TObject);
begin
  DoShowUnicode;
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

