unit formcharmaps;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Grids, LclType, LclProc, LCLUnicodeData;

type
  { TfmCharmaps }
  TCharmapInsertEvent = procedure(const Str: string) of object;

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
    FModeUnicode: boolean;
    FModeUnicodeBegin: integer;
    function DoGetCode(aCol, aRow: integer): integer;
    procedure DoShowAnsi;
    procedure DoShowUnicode;
    procedure DoAutosize;
    procedure DoInsert(aCol, aRow: integer);
    procedure DoStatus(aCol, aRow: integer);
    { private declarations }
  public
    { public declarations }
    property OnInsert: TCharmapInsertEvent read FOnInsert write FOnInsert;
  end;

var
  fmCharmaps: TfmCharmaps;

implementation

{$R *.lfm}

{ TfmCharmaps }

procedure TfmCharmaps.FormShow(Sender: TObject);
begin
  DoShowAnsi;
end;

procedure TfmCharmaps.GridKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key=VK_RETURN then
  begin
    DoInsert(Grid.Selection.Left, Grid.Selection.Top);
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
  DoStatus(i, j);
end;

function TfmCharmaps.DoGetCode(aCol, aRow: integer): integer;
begin
  if not FModeUnicode then
    result:= aCol-1 + (aRow-1)*16
  else
    result:= aCol + aRow*16 + FModeUnicodeBegin;
end;

procedure TfmCharmaps.DoStatus(aCol, aRow: integer);
var
  code: integer;
  str: string;
begin
  code:= DoGetCode(aCol, aRow);
  if not FModeUnicode then
    LabelInfo.Caption:= Format('Decimal %d, Hex %s', [code, IntToHex(code, 2)])
  else
    LabelInfo.Caption:= Format('U+%s', [IntToHex(code, 4)]);

  str:= Utf8Encode(WideChar(code));
  LabelInfo.Caption:= LabelInfo.Caption+Format(', Char "%s"', [str]);
end;

procedure TfmCharmaps.GridSelectCell(Sender: TObject; aCol, aRow: Integer;
  var CanSelect: Boolean);
begin
  DoStatus(aCol, aRow);
end;

procedure TfmCharmaps.DoShowAnsi;
var
  i, j, code: integer;
begin
  FModeUnicode:= false;
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

  DoStatus(Grid.Selection.Left, grid.Selection.Top);
  DoAutosize;
end;

procedure TfmCharmaps.DoShowUnicode;
const
  cSize=16;
var
  nBegin, nEnd,
  i, j: integer;
  str: string;
  sel: TGridRect;
begin
  FModeUnicode:= true;
  PanelInfo.Show;
  Grid.Clear;

  if comboUnicode.ItemIndex<0 then exit;
  nBegin:= UnicodeBlocks[comboUnicode.ItemIndex].S;
  nEnd:= UnicodeBlocks[comboUnicode.ItemIndex].E;
  FModeUnicodeBegin:= nBegin;

  Grid.ColCount:= cSize;
  Grid.RowCount:= (nEnd-nBegin) div cSize +1;
  Grid.FixedCols:= 0;
  Grid.FixedRows:= 0;

  for i:= nBegin to nEnd do
    Grid.Cells[(i-nBegin) mod cSize, (i-nBegin) div cSize]:=
      Utf8Encode(WideChar(i));

  FillChar(sel, Sizeof(sel), 0);
  Grid.Selection:= sel;
  DoStatus(Grid.Selection.Left, Grid.Selection.Top);
  DoAutosize;
end;

procedure TfmCharmaps.FormCreate(Sender: TObject);
var
  i: integer;
begin
  comboUnicode.Items.Clear;
  for i:= Low(UnicodeBlocks) to High(UnicodeBlocks) do
    comboUnicode.Items.Add(UnicodeBlocks[i].PG);
  comboUnicode.ItemIndex:= 0;
end;

procedure TfmCharmaps.DoAutosize;
begin
  Grid.AutoSizeColumns;
  ClientHeight:= 17{fixed}*(Grid.RowHeights[1]+1)+6 + PanelBtm.Height;
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
  str:= Utf8Encode(WideChar(code));

  if Assigned(OnInsert) then
    OnInsert(str);
end;

end.

