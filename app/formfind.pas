(*
This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at http://mozilla.org/MPL/2.0/.

Copyright (c) Alexey Torgashin
*)
unit formfind;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls,
  LclType, LclProc, Math,
  ATButtons,
  ATStringProc,
  ATSynEdit,
  ATSynEdit_Edits,
  ATSynEdit_Commands,
  proc_globdata,
  proc_colors,
  proc_editor;

const
  cOpFindFirst='findfirst';
  cOpFindNext='findnext';
  cOpFindPrev='findprev';
  cOpFindRep='rep';
  cOpFindRepAndStop='repstop';
  cOpFindRepAll='repall';
  cOpFindCount='findcnt';
  cOpFindSelectAll='findsel';
  cOpFindMarkAll='findmark';
  cOpFindClose='x';

type
  { TfmFind }

  TfmFind = class(TForm)
    bCancel: TATButton;
    bCount: TATButton;
    bFindFirst: TATButton;
    bFindNext: TATButton;
    bFindPrev: TATButton;
    bMarkAll: TATButton;
    bSelectAll: TATButton;
    bRep: TATButton;
    bRepAll: TATButton;
    chkCase: TATButton;
    chkConfirm: TATButton;
    chkMulLine: TATButton;
    chkRegex: TATButton;
    chkWords: TATButton;
    chkWrap: TATButton;
    edFind: TATComboEdit;
    edRep: TATComboEdit;
    LabelFind: TLabel;
    LabelRep: TLabel;
    PanelTopOps: TPanel;
    PanelTop: TPanel;
    PanelBtn: TPanel;
    PanelBtnRep: TPanel;
    PanelOps1: TPanel;
    PanelOps2: TPanel;
    PanelTopBtn: TPanel;
    PanelX: TPanel;
    PanelOps: TPanel;
    PanelLabels: TPanel;
    PanelAll: TPanel;
    procedure bFindNextClick(Sender: TObject);
    procedure bFindPrevClick(Sender: TObject);
    procedure bMarkAllClick(Sender: TObject);
    procedure bRepClick(Sender: TObject);
    procedure bRepAllClick(Sender: TObject);
    procedure bCountClick(Sender: TObject);
    procedure bCancelClick(Sender: TObject);
    procedure bSelectAllClick(Sender: TObject);
    procedure chkMulLineClick(Sender: TObject);
    procedure chkRegexChange(Sender: TObject);
    procedure chkRepChange(Sender: TObject);
    procedure bFindFirstClick(Sender: TObject);
    procedure chkRepClick(Sender: TObject);
    procedure edFindChange(Sender: TObject);
    procedure edFindEnter(Sender: TObject);
    procedure edFindKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure edRepEnter(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
    FOnDone: TStrEvent;
    FReplace: boolean;
    FMultiLine: boolean;
    FNarrow: boolean;
    procedure DoDone(const Str: string);
    procedure SetMultiLine(Value: boolean);
    procedure SetNarrow(AValue: boolean);
    procedure SetReplace(AValue: boolean);
  public
    { public declarations }
    FCaptionFind,
    FCaptionReplace: string;
    FHotkeyFind,
    FHotkeyRep: TShortCut;
    procedure UpdateSize;
    procedure UpdateState;
    procedure UpdateFonts;
    property OnDone: TStrEvent read FOnDone write FOnDone;
    property IsReplace: boolean read FReplace write SetReplace;
    property IsMultiLine: boolean read FMultiLine write SetMultiLine;
    property IsNarrow: boolean read FNarrow write SetNarrow;
  end;

var
  fmFind: TfmFind;

implementation

{$R *.lfm}

{ TfmFind }

procedure TfmFind.chkRegexChange(Sender: TObject);
begin
  UpdateState;
end;

procedure TfmFind.bRepClick(Sender: TObject);
begin
  DoDone(cOpFindRep);
end;

procedure TfmFind.bFindNextClick(Sender: TObject);
begin
  DoDone(cOpFindNext);
end;

procedure TfmFind.bFindPrevClick(Sender: TObject);
begin
  DoDone(cOpFindPrev);
end;

procedure TfmFind.bMarkAllClick(Sender: TObject);
begin
  DoDone(cOpFindMarkAll);
end;

procedure TfmFind.bRepAllClick(Sender: TObject);
begin
  DoDone(cOpFindRepAll);
end;

procedure TfmFind.bCountClick(Sender: TObject);
begin
  DoDOne(cOpFindCount);
end;

procedure TfmFind.bCancelClick(Sender: TObject);
begin
  DoDone(cOpFindClose);
end;

procedure TfmFind.bSelectAllClick(Sender: TObject);
begin
  DoDone(cOpFindSelectAll);
end;

procedure TfmFind.chkMulLineClick(Sender: TObject);
begin
  IsMultiLine:= not IsMultiLine;
end;

procedure TfmFind.chkRepChange(Sender: TObject);
begin
  UpdateState;
end;

procedure TfmFind.bFindFirstClick(Sender: TObject);
begin
  DoDone(cOpFindFirst);
end;

procedure TfmFind.chkRepClick(Sender: TObject);
begin
  UpdateState;
end;

procedure TfmFind.edFindChange(Sender: TObject);
begin
  UpdateState;
end;

procedure TfmFind.edFindEnter(Sender: TObject);
begin
  edFind.DoCommand(cCommand_SelectAll);
end;

procedure TfmFind.edFindKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  //Ctrl+Down: copy Find to IsReplace
  if (Key=VK_DOWN) and (Shift=[ssCtrl]) then
  begin
    edRep.Text:= edFind.Text;
    edRep.Update(true);
    key:= 0;
    exit
  end;
end;

procedure TfmFind.edRepEnter(Sender: TObject);
begin
  edRep.DoCommand(cCommand_SelectAll);
end;

procedure TfmFind.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction:= caHide;
end;

procedure TfmFind.FormCreate(Sender: TObject);
begin
  FCaptionFind:= 'Find';
  FCaptionReplace:= 'Replace';

  edFind.OptTabSize:= 4;
  edRep.OptTabSize:= 4;

  edFind.Strings.Endings:= cEndUnix;
  edRep.Strings.Endings:= cEndUnix;

  edFind.OptUnprintedSpaces:= false;
  edRep.OptUnprintedSpaces:= false;
  edFind.OptUnprintedEndsDetails:= false;
  edRep.OptUnprintedEndsDetails:= false;

  bCancel.Caption:= '';
end;

procedure TfmFind.UpdateFonts;
begin
  with LabelFind do
  begin
    Font.Name:= UiOps.VarFontName;
    Font.Size:= UiOps.VarFontSize;
    Font.Color:= GetAppColor('TabFont');
  end;
  LabelRep.Font.Assign(LabelFind.Font);

  with edFind do
  begin
    Font.Name:= EditorOps.OpFontName;
    Font.Size:= EditorOps.OpFontSize;
    Font.Quality:= EditorOps.OpFontQuality;
    EditorApplyTheme(edFind);
    Update;
  end;

  with edRep do
  begin
    Font.Name:= EditorOps.OpFontName;
    Font.Size:= EditorOps.OpFontSize;
    Font.Quality:= EditorOps.OpFontQuality;
    EditorApplyTheme(edRep);
    Update;
  end;

  bCancel.Font.Assign(LabelFind.Font);
end;

procedure TfmFind.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if key=VK_RETURN then
  begin
    //Enter: find next
    if Shift=[] then DoDone(cOpFindNext);
    //Shift+Enter: find prev
    if Shift=[ssShift] then DoDone(cOpFindPrev);
    //Ctrl+Enter: dont catch here, combobox must handle it as new-line
    if Shift=[ssCtrl] then exit;

    if IsReplace then
    begin
      //Alt+Enter: IsReplace
      if Shift=[ssAlt] then DoDone(cOpFindRep);
      //Ctrl+Alt+Enter: IsReplace and dont find next
      if Shift=[ssAlt, ssCtrl] then DoDone(cOpFindRepAndStop);
    end;

    key:= 0;
    exit
  end;

  if key=VK_ESCAPE then
  begin
    DoDone(cOpFindClose);
    key:= 0;
    exit;
  end;

  //handle Tab/ShiftTab: needed coz Mainmenu item handles ShiftTab (unindent)
  if key=VK_TAB then
  begin
    SelectNext(GetActiveControl(Self), not (ssShift in Shift), true);
    key:= 0;
    exit
  end;

  if (FHotkeyFind<>0) and (FHotkeyFind=KeyToShortCut(Key, Shift)) then
  begin
    FReplace:= false;
    UpdateState;
    key:= 0;
    exit;
  end;

  if (FHotkeyRep<>0) and (FHotkeyRep=KeyToShortCut(Key, Shift)) then
  begin
    FReplace:= true;
    UpdateState;
    key:= 0;
    exit;
  end;

  if (key=VK_R) and (Shift=[ssAlt]) then
    begin with chkRegex do checked:= not checked; chkRegexChange(Self); key:= 0; exit end;
  if (key=VK_C) and (Shift=[ssAlt]) then
    begin with chkCase do checked:= not checked; key:= 0; exit end;
  if (key=VK_W) and (Shift=[ssAlt]) then
    begin with chkWords do checked:= not checked; key:= 0; exit end;
  if (key=VK_Y) and (Shift=[ssAlt]) then
    begin with chkConfirm do checked:= not checked; key:= 0; exit end;
  if (key=VK_N) and (Shift=[ssAlt]) then
    begin with chkWrap do checked:= not checked; key:= 0; exit end;
  if (key=VK_M) and (Shift=[ssAlt]) then
    begin chkMulLineClick(Self); key:= 0; exit end;

  if (key=VK_A) and (Shift=[ssAlt]) then
    begin bRepAllClick(Self); key:= 0; exit end;
  if (key=VK_5) and (Shift=[ssAlt]) then
    begin bCountClick(Self); key:= 0; exit end;
  if (key=VK_6) and (Shift=[ssAlt]) then
    begin bSelectAllClick(Self); key:= 0; exit end;
  if (key=VK_7) and (Shift=[ssAlt]) then
    begin bMarkAllClick(Self); key:= 0; exit end;
end;

procedure TfmFind.FormShow(Sender: TObject);
begin
  UpdateSize;
  UpdateFonts;

  //fit form to screen
  if Left>=Screen.DesktopWidth-50 then Left:= Screen.DesktopWidth-Width;
  if Top>=Screen.DesktopHeight-50 then Top:= Screen.DesktopHeight-200;
end;

procedure TfmFind.DoDone(const Str: string);
begin
  if Str=cOpFindPrev then
    if chkRegex.Checked then exit;

  if edFind.Text='' then
    if Str<>cOpFindClose then exit;

  if Assigned(FOnDone) then
    FOnDone(Self, Str);

  if Str<>cOpFindClose then
  begin
    edFind.DoAddLineToHistory(edFind.Text, UiOps.MaxHistoryEdits);
    edRep.DoAddLineToHistory(edRep.Text, UiOps.MaxHistoryEdits);
  end;
end;

procedure TfmFind.SetMultiLine(Value: boolean);
var
  NSizeY, NSmall: integer;
begin
  FMultiLine:= Value;

  edFind.ModeOneLine:= not FMultiLine;
  edRep.ModeOneLine:= not FMultiLine;

  edFind.OptUnprintedVisible:= FMultiLine;
  edRep.OptUnprintedVisible:= FMultiLine;

  NSmall:= 4;
  NSizeY:= bFindFirst.Height;
  if FMultiLine then NSizeY:= Trunc(NSizeY*UiOps.FindMultiLineScale);

  edFind.Height:= NSizeY;
  edRep.Height:= NSizeY;

  edRep.Top:= edFind.Top+edFind.Height+NSmall;
  PanelBtnRep.Top:= edRep.Top;
  LabelRep.Top:= edRep.Top+NSmall;

  UpdateState;
end;

procedure TfmFind.SetNarrow(AValue: boolean);
begin
  if FNarrow=AValue then Exit;
  FNarrow:= AValue;

  if FNarrow then
  begin
    chkRegex.Parent:= PanelTopOps;
    chkCase.Parent:= PanelTopOps;
    chkWords.Parent:= PanelTopOps;
    chkWrap.Parent:= PanelTopOps;
    chkMulLine.Parent:= PanelTopOps;
    chkConfirm.Parent:= PanelTopOps;
    chkConfirm.Left:= 400; //to right

    bCount.Parent:= PanelTopBtn;
    bSelectAll.Parent:= PanelTopBtn;
    bMarkAll.Parent:= PanelTopBtn;
  end;

  PanelTopOps.Left:= PanelLabels.Width + edFind.Left;
  PanelTopBtn.Left:= PanelLabels.Width + PanelBtn.Left;
end;

procedure TfmFind.SetReplace(AValue: boolean);
begin
  if FReplace=AValue then Exit;
  FReplace:= AValue;
  UpdateState;
end;

procedure TfmFind.UpdateSize;
  //
  function MaxX(C: TControl): integer;
  var
    P: TPoint;
  begin
    if not C.Visible then exit(0);
    P:= Point(C.Width, 0);
    P:= C.ClientToScreen(P);
    P:= Self.ScreenToClient(P);
    Result:= P.X;
  end;
  //
  function MaxY(C: TControl): integer;
  var
    P: TPoint;
  begin
    if not C.Visible then exit(0);
    P:= Point(0, C.Height);
    P:= C.ClientToScreen(P);
    P:= Self.ScreenToClient(P);
    Result:= P.Y;
  end;
  //
begin
  if IsNarrow then
    ClientWidth:= MaxX(bMarkAll) + 8;

  ClientHeight:=
    IfThen(IsReplace, MaxY(edRep), MaxY(edFind)) +
    + 4;
end;

procedure TfmFind.UpdateState;
begin
  if IsReplace then
    Caption:= FCaptionReplace
  else
    Caption:= FCaptionFind;

  PanelTop.Visible:= IsNarrow;
  PanelOps.Visible:= not IsNarrow;
  PanelX.Visible:= not IsNarrow;
  chkMulLine.Checked:= IsMultiLine;
  chkWords.Enabled:= not chkRegex.Checked;
  chkConfirm.Visible:= IsReplace or IsNarrow;
  edRep.Visible:= IsReplace;
  PanelLabels.Visible:= IsReplace or IsNarrow;
  LabelRep.Visible:= IsReplace;
  PanelBtnRep.Visible:= IsReplace;

  chkConfirm.Enabled:= IsReplace;
  //bCount.Enabled:= not IsReplace;
  //bSelectAll.Enabled:= not IsReplace;
  //bMarkAll.Enabled:= not IsReplace;

  bFindFirst.Enabled:= true;
  bFindNext.Enabled:= true;
  bFindPrev.Enabled:= not chkRegex.Checked;
  edRep.Enabled:= IsReplace;
  bRep.Enabled:= IsReplace;
  bRepAll.Enabled:= IsReplace;

  UpdateSize;
end;

end.

