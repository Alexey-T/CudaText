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
  LclType, LclProc, LCLUnicodeData,
  LazUTF8, LazFileUtils,
  {$ifdef windows} Windows, {$endif}
  EncConv,
  ATPanelSimple,
  ATSynEdit_Globals,
  proc_msg,
  proc_miscutils,
  proc_globdata;

type
  TCharmapInsertEvent = procedure(const Str: string) of object;

type
  { TfmCharmaps }
  TfmCharmaps = class(TForm)
    btnAnsi: TButton;
    btnClose: TButton;
    btnUnicode: TButton;
    chkHexTitle: TCheckBox;
    comboAnsi: TComboBox;
    comboUnicode: TComboBox;
    LabelInfo: TLabel;
    PanelBtm: TATPanelSimple;
    Grid: TStringGrid;
    PanelInfo: TATPanelSimple;
    procedure btnAnsiClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure btnUnicodeClick(Sender: TObject);
    procedure chkHexTitleChange(Sender: TObject);
    procedure comboAnsiChange(Sender: TObject);
    procedure comboUnicodeChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure GridMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure GridMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure GridSelectCell(Sender: TObject; aCol, aRow: Integer; var CanSelect: Boolean);
  private
    { private declarations }
    FOnInsert: TCharmapInsertEvent;
    FUnicodeMode: boolean;
    FUnicodeBegin: integer;
    FHexTitles: boolean;
    FEncodingId: TEncConvId;
    function CodeToString(code: integer): string;
    function DoGetCode(aCol, aRow: integer): integer;
    procedure DoShowAnsi;
    procedure DoShowUnicode;
    procedure DoFormAutosize;
    procedure DoInsert(aCol, aRow: integer);
    procedure DoShowStatus(aCol, aRow: integer);
    function GetCodepage: string;
    procedure SetHexTitles(Value: boolean);
    procedure UpdateTitles;
  public
    { public declarations }
    AllowUnicodeAfterFFFF: boolean;
    MsgStatusAnsi: string;
    MsgStatusUnicode: string;
    InitialStr: string;
    procedure Localize;
    property OnInsert: TCharmapInsertEvent read FOnInsert write FOnInsert;
    property HexTitles: boolean read FHexTitles write SetHexTitles;
  end;

var
  fmCharmaps: TfmCharmaps = nil;

function DoDialogCharmapModal: string;


implementation

{$R *.lfm}

procedure TfmCharmaps.Localize;
const
  section = 'd_charmap';
var
  ini: TIniFile;
  fn: string;
begin
  fn:= AppFile_Language;
  if not FileExists(fn) then exit;
  ini:= TIniFile.Create(fn);
  try
    Caption:= ini.ReadString(section, '_', Caption);
    with btnClose do Caption:= msgButtonClose;
    with btnAnsi do Caption:= ini.ReadString(section, 'mod1', Caption);
    with btnUnicode do Caption:= ini.ReadString(section, 'mod2', Caption);
    with chkHexTitle do Caption:= ini.ReadString(section, 'hex', Caption);
    MsgStatusAnsi:= ini.ReadString(section, 'stat1', MsgStatusAnsi);
    MsgStatusUnicode:= ini.ReadString(section, 'stat2', MsgStatusUnicode);
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

function DoDialogCharmapModal: string;
var
  F: TfmCharmaps;
  Dummy: TDummy;
begin
  F:= TfmCharmaps.Create(nil);
  Dummy:= TDummy.Create;
  Dummy.Form:= F;

  try
    F.Localize;
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
  {$ifdef windows}
  str: string;
  cp: integer;
  {$endif}
  i, j: integer;
begin
  UpdateFormOnTop(Self);

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

  if not FUnicodeMode then
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
  NRow, NCol: integer;
begin
  if Grid.MouseToGridZone(X, Y)<>gzNormal then exit;
  if Button=mbLeft then
  begin
    NRow:= -1;
    NCol:= -1;
    Grid.MouseToCell(X, Y, NRow, NCol);
    if (NRow>=0) and (NCol>=0) then
      DoInsert(NRow, NCol);
  end;
end;

procedure TfmCharmaps.GridMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  NRow, NCol: integer;
begin
  if Grid.MouseToGridZone(X, Y)<>gzNormal then exit;
  NRow:= -1;
  NCol:= -1;
  Grid.MouseToCell(X, Y, NRow, NCol);
  if (NRow>=0) and (NCol>=0) then
    DoShowStatus(NRow, NCol);
end;

function TfmCharmaps.DoGetCode(aCol, aRow: integer): integer;
begin
  if not FUnicodeMode then
    Result:= aCol-1 + (aRow-1)*16
  else
    Result:= aCol + aRow*16 + FUnicodeBegin;
end;

function TfmCharmaps.CodeToString(code: integer): string;
begin
  if FUnicodeMode then
    Result:= UnicodeToUTF8(code)
  else
  if code>=0 then
    Result:= EncConvertToUTF8(Chr(code), FEncodingId)
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

  if not FUnicodeMode then
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

procedure TfmCharmaps.SetHexTitles(Value: boolean);
begin
  FHexTitles:= Value;
  UpdateTitles;
end;

procedure TfmCharmaps.UpdateTitles;
var
  i: integer;
begin
  if not FUnicodeMode then
  for i:= 1 to 16 do
    if HexTitles then
    begin
      Grid.Cells[i, 0]:= Format('%2.2x', [Pred(i)]);
      Grid.Cells[0, i]:= Format('%2.2x+', [Pred(i)*16]);
    end
    else
    begin
      Grid.Cells[i, 0]:= Format('%2.2d', [Pred(i)]);
      Grid.Cells[0, i]:= Format('%3.3d+', [Pred(i)*16]);
    end;
end;

procedure TfmCharmaps.DoShowAnsi;
var
  Str: string;
  i, j, code: integer;
begin
  FUnicodeMode:= false;
  FEncodingId:= EncConvFindEncoding(GetCodepage, eidCP1252);
  comboAnsi.Visible:= not FUnicodeMode;
  comboUnicode.Visible:= FUnicodeMode;
  chkHexTitle.Enabled:= true;

  Grid.Clear;
  Grid.RowCount:= 17;
  Grid.ColCount:= 17;
  Grid.FixedCols:= 1;
  Grid.FixedRows:= 1;

  for i:= 1 to 16 do
    for j:= 1 to 16 do
    begin
      code:= i-1 + (j-1)*16;
      if (code<16) then Continue;
      Str:= CodeToString(code);
      Grid.Cells[i, j]:= Str;
    end;

  UpdateTitles;

  DoShowStatus(Grid.Col, Grid.Row);
  DoFormAutosize;
end;

procedure TfmCharmaps.DoShowUnicode;
const
  cSize=16;
var
  nBegin, nEnd, i: integer;
begin
  FUnicodeMode:= true;
  comboAnsi.Visible:= not FUnicodeMode;
  comboUnicode.Visible:= FUnicodeMode;
  chkHexTitle.Enabled:= false;

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

  //for Hi-DPI
  Grid.DefaultRowHeight:= ATEditorScale(22);

  comboUnicode.Items.Clear;
  for i:= Low(UnicodeBlocks) to High(UnicodeBlocks) do
    if AllowUnicodeAfterFFFF or (UnicodeBlocks[i].E<=$FFFF) then
      comboUnicode.Items.Add(UnicodeBlocks[i].PG);
  comboUnicode.ItemIndex:= 0;

  DoShowAnsi;

  chkHexTitle.Checked:= true; //'hex titles' is better as On
  FormHistoryLoad(Self, '/pos/charmap', false);
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

  if (Key=VK_ESCAPE) and (Shift=[]) then
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

  if (Key=VK_DOWN) and (Shift=[ssCtrl]) then
  begin
    if FUnicodeMode then
    begin
      with comboUnicode do
        if ItemIndex<Items.Count-1 then
          ItemIndex:= ItemIndex+1;
      DoShowUnicode;
    end
    else
    begin
      with comboAnsi do
        if ItemIndex<Items.Count-1 then
          ItemIndex:= ItemIndex+1;
      DoShowAnsi;
    end;
    Key:= 0;
    exit
  end;

  if (Key=VK_UP) and (Shift=[ssCtrl]) then
  begin
    if FUnicodeMode then
    begin
      with comboUnicode do
        if ItemIndex>0 then
          ItemIndex:= ItemIndex-1;
      DoShowUnicode;
    end
    else
    begin
      with comboAnsi do
        if ItemIndex>0 then
          ItemIndex:= ItemIndex-1;
      DoShowAnsi;
    end;
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

procedure TfmCharmaps.chkHexTitleChange(Sender: TObject);
begin
  HexTitles:= chkHexTitle.Checked;
end;

procedure TfmCharmaps.comboAnsiChange(Sender: TObject);
begin
  if not FUnicodeMode then
    DoShowAnsi;
end;

procedure TfmCharmaps.comboUnicodeChange(Sender: TObject);
begin
  DoShowUnicode;
end;

procedure TfmCharmaps.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  FormHistorySave(Self, '/pos/charmap', false);
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

