unit formcharmaps;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Grids, LclType, LclProc;

type
  { TfmCharmaps }
  TCharmapInsertEvent = procedure(const Str: string) of object;

  TfmCharmaps = class(TForm)
    btnClose: TButton;
    ComboBox1: TComboBox;
    LabelInfo: TLabel;
    PanelInfo: TPanel;
    PanelBtm: TPanel;
    Grid: TStringGrid;
    procedure btnCloseClick(Sender: TObject);
    procedure DoShowAnsi;
    procedure FormShow(Sender: TObject);
    procedure GridKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure GridMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure GridMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure GridSelectCell(Sender: TObject; aCol, aRow: Integer; var CanSelect: Boolean);
  private
    FOnInsert: TCharmapInsertEvent;
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
end;

procedure TfmCharmaps.GridMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  i, j: integer;
begin
  if Button=mbLeft then
  begin
    Grid.MouseToCell(X, Y, i, j);
    if i<1 then exit;
    if j<1 then exit;
    DoInsert(i, j);
  end;
end;

procedure TfmCharmaps.GridMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  i, j: integer;
begin
  Grid.MouseToCell(X, Y, i, j);
  if i<1 then exit;
  if j<1 then exit;
  DoStatus(i, j);
end;

procedure TfmCharmaps.DoStatus(aCol, aRow: integer);
var
  code: integer;
begin
  code:= aCol-1 + (aRow-1)*16;
  LabelInfo.Caption:= Format('Decimal %d, Hex %s', [code, IntToHex(code, 2)]);
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
  Grid.RowCount:= 17;
  Grid.ColCount:= 17;

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

  DoAutosize;
end;

procedure TfmCharmaps.DoAutosize;
begin
  Grid.AutoSizeColumns;
  ClientHeight:= Grid.RowCount*(Grid.RowHeights[1]+1)+6 + PanelBtm.Height;
end;

procedure TfmCharmaps.btnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TfmCharmaps.DoInsert(aCol, aRow: integer);
var
  code: integer;
  str: string;
begin
  code:= aCol-1 + (aRow-1)*16;
  str:= AnsiToUtf8(Chr(code));

  if Assigned(OnInsert) then
    OnInsert(str);
end;

end.

