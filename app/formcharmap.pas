{
 ***************************************************************************
 *                                                                         *
 *   This source is free software; you can redistribute it and/or modify   *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This code is distributed in the hope that it will be useful, but      *
 *   WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   General Public License for more details.                              *
 *                                                                         *
 *   A copy of the GNU General Public License is available on the World    *
 *   Wide Web at <http://www.gnu.org/copyleft/gpl.html>. You can also      *
 *   obtain it by writing to the Free Software Foundation,                 *
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
 *                                                                         *
 ***************************************************************************

  Author: Mattias Gaertner
  
  Abstract:
    Dialog for character map.
}

unit FormCharMap;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Buttons,
  StdCtrls, Grids, ButtonPanel, ComCtrls,
  LCLType, LCLUnicodeData, GraphType, LazUTF8,
  proc_globdata;

type
  TOnInsertCharacterEvent = procedure (const C: TUTF8Char) of object;

  { TfmCharmap }

  TfmCharmap = class(TForm)
    ButtonPanel: TButtonPanel;
    CharInfoLabel: TLabel;
    cbUniRange: TComboBox;
    UnicodeCharInfoLabel: TLabel;
    PageControl1: TPageControl;
    StringGrid1: TStringGrid;
    StringGrid2: TStringGrid;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    procedure cbUniRangeSelect(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure StringGridKeyPress(Sender: TObject; var Key: char);
    procedure StringGridMouseDown(Sender: TObject; Button: TMouseButton;
      {%H-}Shift: TShiftState; X, Y: Integer);
    procedure StringGrid1MouseMove(Sender: TObject; {%H-}Shift: TShiftState; X,
      Y: Integer);
    procedure StringGrid2MouseMove(Sender: TObject; {%H-}Shift: TShiftState; X,
      Y: Integer);
  private
    FOnInsertCharacter: TOnInsertCharacterEvent;
    procedure FillCharMap;
  public
    property OnInsertCharacter: TOnInsertCharacterEvent read FOnInsertCharacter
                                                        write FOnInsertCharacter;
  end;

procedure ShowCharacterMap(AOnInsertChar: TOnInsertCharacterEvent);

var
  fmCharmap: TfmCharmap = nil;

implementation

{$R *.lfm}

procedure ShowCharacterMap(AOnInsertChar: TOnInsertCharacterEvent);
begin
  if fmCharmap = nil then
    Application.CreateForm(TfmCharmap, fmCharmap);
    
  fmCharmap.OnInsertCharacter := AOnInsertChar;
  fmCharmap.Show;
end;

{ TfmCharmap }

procedure TfmCharmap.FormCreate(Sender: TObject);
begin
  CharInfoLabel.Caption := '-';
  UnicodeCharInfoLabel.Caption := '-';
  FillCharMap;
end;

procedure TfmCharmap.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if key=VK_ESCAPE then
  begin
    Close;
    key:= 0;
    exit
  end;
end;

function RoundUp(Value, Divi:integer):integer;
begin
  if Value mod Divi=0 then
   Result:=Value div Divi else
   Result:=(Value div Divi)+1;
end;

procedure TfmCharmap.cbUniRangeSelect(Sender: TObject);
var cnt, x, y :integer;
S,E:Integer;
begin
 S:=UnicodeBlocks[cbUniRange.ItemIndex].S;
 E:=UnicodeBlocks[cbUniRange.ItemIndex].E;
 StringGrid2.Clear;
 StringGrid2.ColCount:=16;
 StringGrid2.RowCount:=RoundUp(E-S,16);
 cnt:=0;
 for y:=0 to StringGrid2.RowCount-1 do
  for x:=0 to StringGrid2.ColCount-1 do
  begin
   if S+Cnt<=E then
     StringGrid2.Cells[x,y]:=UnicodeToUTF8(S+Cnt);
   inc(cnt);
  end;
  StringGrid2.AutoSizeColumns;
end;

procedure TfmCharmap.FormShow(Sender: TObject);
var i:integer;
begin
  StringGrid1.Font.Name:= EditorOps.OpFontName;
  StringGrid2.Font.Name:= EditorOps.OpFontName;
  StringGrid1.Font.Size:= EditorOps.OpFontSize;
  StringGrid2.Font.Size:= EditorOps.OpFontSize;

  StringGrid1.AutoSizeColumns;

  cbUniRange.Items.Clear;
  for i:=0 to MaxUnicodeBlocks do cbUniRange.Items.Add(UnicodeBlocks[i].PG);
  cbUniRange.ItemIndex:=0;
  cbUniRangeSelect(nil);
end;

procedure TfmCharmap.StringGridKeyPress(Sender: TObject; var Key: char);
var
  sg: TStringGrid;
  s: string;
begin
  if Key = #13 then begin
    sg := Sender as TStringGrid;
    s := sg.Cells[sg.Col, sg.Row];
    if (s <> '') and (Assigned(OnInsertCharacter)) then
      OnInsertCharacter(s);
  end;
end;

procedure TfmCharmap.StringGridMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Row, Col: Integer;
  sg: TStringGrid;
begin
  sg := Sender as TStringGrid;
  if (Button = mbLeft) and (sg.MouseToGridZone(X, Y) = gzNormal) then
  begin
    Col:=0; Row:=0;
    sg.MouseToCell(X, Y, Col, Row);
    if (sg.Cells[Col, Row] <> '') and (Assigned(OnInsertCharacter)) then
      OnInsertCharacter(sg.Cells[Col, Row]);
  end;
end;

procedure TfmCharmap.StringGrid1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  CharOrd: Byte;
  Row, Col: Integer;
begin
  if StringGrid1.MouseToGridZone(X, Y) = gzNormal then
  begin
    Col:=0; Row:=0;
    StringGrid1.MouseToCell(X, Y, Col, Row);
    if StringGrid1.Cells[Col, Row] <> '' then
    begin
      CharOrd := Ord(UTF8ToAnsi(StringGrid1.Cells[Col, Row])[1]);
      CharInfoLabel.Caption := 'Decimal = ' + IntToStr(CharOrd) +
                               ', Hex = $'  + HexStr(CharOrd, 2);
    end 
    else
      CharInfoLabel.Caption := '-';
  end
  else
  begin
    CharInfoLabel.Caption := '-';
  end;
end;

procedure TfmCharmap.StringGrid2MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var  Row, Col, i: Integer;
     S:Cardinal;
     tmp,tmp2:String;
begin
  if StringGrid2.MouseToGridZone(X, Y) = gzNormal then
  begin
    Col:=0; Row:=0;
    StringGrid2.MouseToCell(X, Y, Col, Row);
    S:=UnicodeBlocks[cbUniRange.ItemIndex].S+(Col)+(Row*16);
    tmp:=UnicodeToUTF8(S);
    tmp2:='';
    for i:=1 to Length(tmp) do
      tmp2:=tmp2+'$'+IntToHex(Ord(tmp[i]),2);
    UnicodeCharInfoLabel.Caption:='U+'+inttohex(S,4)+', UTF-8 = '+tmp2;
  end
  else
  begin
    CharInfoLabel.Caption := '-';
  end;
end;

procedure TfmCharmap.FillCharMap;
var
  R, C: Integer;
begin
  for R := 0 to Pred(StringGrid1.RowCount) do
  begin
    if R <> 0 then  StringGrid1.Cells[0, R] := Format('%.3d +', [Succ(R) * 16]);
    for C := 1 to Pred(StringGrid1.ColCount) do
    begin
      if R = 0 then StringGrid1.Cells[C, R] := Format('%.2d', [Pred(C)])
      else
        StringGrid1.Cells[C, R] := AnsiToUTF8(Chr(Succ(R) * 16 + Pred(C)));
    end;
  end;
end;

end.

