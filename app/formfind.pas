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
  ExtCtrls, Menus,
  LclType, LclProc, IniFiles, Math,
  ATButtons,
  ATPanelSimple,
  ATStringProc,
  ATCanvasPrimitives,
  ATSynEdit,
  ATSynEdit_Globals,
  ATSynEdit_Carets,
  ATSynEdit_Edits,
  ATSynEdit_Commands,
  ATSynEdit_Finder,
  ATSynEdit_Adapter_EControl,
  proc_msg,
  proc_globdata,
  proc_miscutils,
  proc_colors,
  proc_editor,
  proc_cmd,
  ec_SyntAnal,
  formlexerstylemap;

type
  TAppFinderOperation = (
    afoNone,
    afoCloseDlg,
    afoFindFirst,
    afoFindNext,
    afoFindPrev,
    afoCountAll,
    afoExtractAll,
    afoFindSelectAll,
    afoFindMarkAll,
    afoReplace,
    afoReplaceStop,
    afoReplaceAll,
    afoReplaceGlobal
    );

const
  cAppFinderOperationString: array[TAppFinderOperation] of string = (
    '-',
    'x',
    'findfirst',
    'findnext',
    'findprev',
    'findcnt',
    'findget',
    'findsel',
    'findmark',
    'rep',
    'repstop',
    'repall',
    'repglobal'
    );

type
  TAppFinderOperationCategory = (
    afcNone,
    afcCloseDlg,
    afcFind,
    afcReplaceOne,
    afcReplaceGlobal
    );

const
  cAppFinderOperationCategory: array[TAppFinderOperation] of TAppFinderOperationCategory = (
    afcNone,
    afcCloseDlg,
    afcFind,
    afcFind,
    afcFind,
    afcFind,
    afcFind,
    afcFind,
    afcFind,
    afcReplaceOne,
    afcReplaceOne,
    afcReplaceOne,
    afcReplaceGlobal
    );

type
  TAppFinderOperationEvent = procedure(Sender: TObject; Op: TAppFinderOperation) of object;
  TAppFinderGetEditor = procedure(out AEditor: TATSynEdit) of object;
  TAppFinderShowMatchesCount = procedure(AMatchCount, ATime: integer) of object;
  TAppFinderKeyDownEvent = function(AKey: word; AShiftState: TShiftState): boolean of object;

function AppFinderOperationFromString(const Str: string): TAppFinderOperation;

type
  { TfmFind }

  TfmFind = class(TForm)
    bFindFirst: TATButton;
    bFindNext: TATButton;
    bFindPrev: TATButton;
    bMore: TATButton;
    bRep: TATButton;
    bRepAll: TATButton;
    bRepGlobal: TATButton;
    chkCase: TATButton;
    chkConfirm: TATButton;
    chkRegexSubst: TATButton;
    chkInSel: TATButton;
    chkHiAll: TATButton;
    chkMulLine: TATButton;
    bTokens: TATButton;
    chkRegex: TATButton;
    chkPreserveCase: TATButton;
    chkWords: TATButton;
    chkWrap: TATButton;
    edFind: TATComboEdit;
    edRep: TATComboEdit;
    PanelAll: TATPanelSimple;
    PanelBtn: TATPanelSimple;
    PanelBtnRep: TATPanelSimple;
    PanelOps: TATPanelSimple;
    PanelOps1: TATPanelSimple;
    PanelOps2: TATPanelSimple;
    PanelTop: TATPanelSimple;
    PanelTopOps: TATPanelSimple;
    procedure bExtractClick(Sender: TObject);
    procedure bFindNextClick(Sender: TObject);
    procedure bFindPrevClick(Sender: TObject);
    procedure bMarkAllClick(Sender: TObject);
    procedure bMoreClick(Sender: TObject);
    procedure bRepClick(Sender: TObject);
    procedure bRepAllClick(Sender: TObject);
    procedure bCountClick(Sender: TObject);
    procedure bCancelClick(Sender: TObject);
    procedure bRepGlobalClick(Sender: TObject);
    procedure bSelectAllClick(Sender: TObject);
    procedure bTokensClick(Sender: TObject);
    procedure chkCaseClick(Sender: TObject);
    procedure chkConfirmClick(Sender: TObject);
    procedure chkHiAllClick(Sender: TObject);
    procedure chkInSelClick(Sender: TObject);
    procedure chkMulLineClick(Sender: TObject);
    procedure chkPreserveCaseClick(Sender: TObject);
    procedure chkRegexClick(Sender: TObject);
    procedure chkRegexSubstClick(Sender: TObject);
    procedure chkRepChange(Sender: TObject);
    procedure bFindFirstClick(Sender: TObject);
    procedure chkRepClick(Sender: TObject);
    procedure chkWordsClick(Sender: TObject);
    procedure chkWrapClick(Sender: TObject);
    procedure edFindChange(Sender: TObject);
    procedure edFindCommand(Sender: TObject; ACommand: integer; AInvoke: TATCommandInvoke;
      const AText: string; var AHandled: boolean);
    procedure edFindCommandAfter(Sender: TObject; ACommand: integer;
      const AText: string);
    procedure edFindEnter(Sender: TObject);
    procedure edFindKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure edRepEnter(Sender: TObject);
    procedure edRepExit(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
    FTimerShow: TTimer;
    FPopupMore: TPopupMenu;
    FMenuitemOptRegex: TMenuItem;
    FMenuitemOptCase: TMenuItem;
    FMenuitemOptWords: TMenuItem;
    FMenuitemOptWrapped: TMenuItem;
    FMenuitemOptInSel: TMenuItem;
    FMenuitemOptMulti: TMenuItem;
    FMenuitemOptTokens: TMenuItem;
    FMenuitemOptTokensSub: array[TATFinderTokensAllowed] of TMenuItem;
    FMenuitemOptHiAll: TMenuItem;
    FMenuitemOptRegexSubst: TMenuItem;
    FMenuitemOptPreserveCase: TMenuItem;
    FMenuitemFindFirst: TMenuItem;
    FMenuitemFindPrev: TMenuItem;
    FMenuitemFindNext: TMenuItem;
    FMenuitemCount: TMenuItem;
    FMenuitemExtract: TMenuItem;
    FMenuitemSelectAll: TMenuItem;
    FMenuitemMarkAll: TMenuItem;
    FMenuitemRep: TMenuItem;
    FMenuitemRepStop: TMenuItem;
    FMenuitemRepAll: TMenuItem;
    FMenuitemRepGlobal: TMenuItem;
    FReplace: boolean;
    FMultiLine: boolean;
    FMultiLineJustActivated: boolean;
    FNarrow: boolean;
    FOnResult: TAppFinderOperationEvent;
    FOnChangeVisible: TNotifyEvent;
    FOnChangeOptions: TNotifyEvent;
    FOnFocusEditor: TNotifyEvent;
    FOnGetMainEditor: TAppFinderGetEditor;
    FOnGetToken: TATFinderGetToken;
    FOnShowMatchesCount: TAppFinderShowMatchesCount;
    FOnHandleKeyDown: TAppFinderKeyDownEvent;
    FLexerRegexThemed: boolean;
    Adapter: TATAdapterEControl;
    AdapterActive: boolean;
    procedure bRepStopClick(Sender: TObject);
    procedure ControlAutosizeOptionsByWidth;
    procedure CopyFieldFindToReplace;
    procedure DoFocusEditor;
    procedure DoResult(Op: TAppFinderOperation);
    function GetHiAll: boolean;
    procedure InitPopupMore;
    procedure MenuitemTokensClick(Sender: TObject);
    procedure SetHiAll(AValue: boolean);
    procedure SetIsDoubleBuffered(AValue: boolean);
    procedure SetMultiLine(AValue: boolean);
    procedure SetNarrow(AValue: boolean);
    procedure SetReplace(AValue: boolean);
    procedure TimerShowTick(Sender: TObject);
    procedure UpdateButtonBold;
    procedure UpdateRegexHighlight;
  public
    { public declarations }
    FCaptionFind: string;
    FCaptionReplace: string;
    FInitialCaretPos: TPoint;
    FForViewer: boolean;
    procedure Localize;
    procedure DoOnChange;
    procedure UpdateFormHeight;
    procedure UpdateInitialCaretPos;
    procedure UpdateState(AEnableFindNextForHiOption: boolean);
    procedure UpdateFonts;
    procedure UpdateFocus(AFindMode: boolean);
    procedure UpdateInputFind(const AText: UnicodeString);
    procedure UpdateInputReplace(const AText: UnicodeString);
    procedure UpdateCaption(const AText: string);
    procedure UpdateHiAll(AEnableFindNext: boolean);
    procedure ClearHiAll;
    procedure ApplyTheme;
    function CurrentCaption: string;
    property OnResult: TAppFinderOperationEvent read FOnResult write FOnResult;
    property OnChangeOptions: TNotifyEvent read FOnChangeOptions write FOnChangeOptions;
    property OnChangeVisible: TNotifyEvent read FOnChangeVisible write FOnChangeVisible;
    property OnFocusEditor: TNotifyEvent read FOnFocusEditor write FOnFocusEditor;
    property OnGetMainEditor: TAppFinderGetEditor read FOnGetMainEditor write FOnGetMainEditor;
    property OnGetToken: TATFinderGetToken read FOnGetToken write FOnGetToken;
    property OnShowMatchesCount: TAppFinderShowMatchesCount read FOnShowMatchesCount write FOnShowMatchesCount;
    property OnHandleKeyDown: TAppFinderKeyDownEvent read FOnHandleKeyDown write FOnHandleKeyDown;
    property IsReplace: boolean read FReplace write SetReplace;
    property IsMultiLine: boolean read FMultiLine write SetMultiLine;
    property IsNarrow: boolean read FNarrow write SetNarrow;
    property IsHiAll: boolean read GetHiAll write SetHiAll;
    property IsDoubleBuffered: boolean write SetIsDoubleBuffered;
  end;

var
  fmFind: TfmFind = nil;

implementation

{$R *.lfm}

var
  cPadding: integer = 4;

const
  cTokensDesc: array[TATFinderTokensAllowed] of string = (
    'Any',
    'Only comments',
    'Only strings',
    'Only comments/strings',
    'Except comments',
    'Except strings',
    'Except comments/strings'
    );

  cTokensShorts: array[TATFinderTokensAllowed] of string = (
    '*',
    '+c',
    '+s',
    '+cs',
    '-c',
    '-s',
    '-cs'
    );

function AppFinderOperationFromString(const Str: string): TAppFinderOperation;
var
  op: TAppFinderOperation;
begin
  for op:= Low(TAppFinderOperation) to High(TAppFinderOperation) do
    if Str=cAppFinderOperationString[op] then
      exit(op);
  Result:= afoNone;
end;

function _MakeHint(const AText, AHotkey: string): string;
begin
  Result:= AText;
  if AHotkey<>'' then
    Result+= ' ['+AHotkey+']';
end;

{ TfmFind }

procedure TfmFind.chkRegexClick(Sender: TObject);
begin
  with chkRegex do
    Checked:= not Checked;
  UpdateState(true);
  DoOnChange;
end;

procedure TfmFind.chkRegexSubstClick(Sender: TObject);
begin
  with chkRegexSubst do
    Checked:= not Checked;
  UpdateState(false);
  DoOnChange;
end;

procedure TfmFind.bRepClick(Sender: TObject);
begin
  if IsReplace then
    DoResult(afoReplace);
end;

procedure TfmFind.bRepStopClick(Sender: TObject);
begin
  if IsReplace then
    DoResult(afoReplaceStop);
end;

procedure TfmFind.bFindNextClick(Sender: TObject);
begin
  DoResult(afoFindNext);
end;

procedure TfmFind.bExtractClick(Sender: TObject);
begin
  DoResult(afoExtractAll);
end;

procedure TfmFind.bFindPrevClick(Sender: TObject);
begin
  DoResult(afoFindPrev);
end;

procedure TfmFind.bMarkAllClick(Sender: TObject);
begin
  DoResult(afoFindMarkAll);
end;

procedure TfmFind.InitPopupMore;
const
  section = 'd_f';
var
  fn: string;
  ini: TIniFile;
  SCaptionFindFirst,
  SCaptionFindPrev,
  SCaptionFindNext,
  SCaptionCount,
  SCaptionExtract,
  SCaptionSelect,
  SCaptionMark,
  SCaptionRep,
  SCaptionRepStop,
  SCaptionRepAll,
  SCaptionRepGlobal: string;
  Sep1: TMenuItem;
  Sep2: TMenuItem;
  Sep3: TMenuItem;
  kind: TATFinderTokensAllowed;
begin
  SCaptionFindFirst:= 'Find first';
  SCaptionFindPrev:= 'Find previous';
  SCaptionFindNext:= 'Find next';
  SCaptionCount:= 'Count all';
  SCaptionExtract:= 'Extract RegEx matches';
  SCaptionSelect:= 'Select all';
  SCaptionMark:= 'Mark all';
  SCaptionRep:= 'Replace next, find next';
  SCaptionRepStop:= 'Replace next, don''t find next';
  SCaptionRepAll:= 'Replace all';
  SCaptionRepGlobal:= 'Replace global';

  fn:= AppFile_Language;
  if FileExists(fn) then
  begin
    ini:= TIniFile.Create(fn);
    try
      SCaptionFindFirst:= ini.ReadString(section, 'h_f1', SCaptionFindFirst);
      SCaptionFindPrev:= ini.ReadString(section, 'h_fp', SCaptionFindPrev);
      SCaptionFindNext:= ini.ReadString(section, 'h_fn', SCaptionFindNext);
      SCaptionCount:= ini.ReadString(section, 'cnt', SCaptionCount);
      SCaptionExtract:= ini.ReadString(section, 'get', SCaptionExtract);
      SCaptionSelect:= ini.ReadString(section, 'sel', SCaptionSelect);
      SCaptionMark:= ini.ReadString(section, 'mk', SCaptionMark);
      SCaptionRep:= ini.ReadString(section, 'h_r', SCaptionRep);
      SCaptionRepStop:= ini.ReadString(section, 'h_r2', SCaptionRepStop);
      SCaptionRepAll:= ini.ReadString(section, 'h_r_a', SCaptionRepAll);
      SCaptionRepGlobal:= ini.ReadString(section, 'h_r_g', SCaptionRepGlobal);
    finally
      FreeAndNil(ini);
    end;
  end;

  if not Assigned(FPopupMore) then
  begin
    FPopupMore:= TPopupMenu.Create(Self);

    FMenuitemOptRegex:= TMenuItem.Create(Self);
    FMenuitemOptRegex.OnClick:= @chkRegexClick;

    FMenuitemOptCase:= TMenuItem.Create(Self);
    FMenuitemOptCase.OnClick:= @chkCaseClick;

    FMenuitemOptWords:= TMenuItem.Create(Self);
    FMenuitemOptWords.OnClick:= @chkWordsClick;

    FMenuitemOptWrapped:= TMenuItem.Create(Self);
    FMenuitemOptWrapped.OnClick:= @chkWrapClick;

    FMenuitemOptInSel:= TMenuItem.Create(Self);
    FMenuitemOptInSel.OnClick:= @chkInSelClick;

    FMenuitemOptMulti:= TMenuItem.Create(Self);
    FMenuitemOptMulti.OnClick:= @chkMulLineClick;

    FMenuitemOptTokens:= TMenuItem.Create(Self);
    for kind in TATFinderTokensAllowed do
    begin
      FMenuitemOptTokensSub[kind]:= TMenuItem.Create(Self);
      FMenuitemOptTokensSub[kind].Tag:= Ord(kind);
      FMenuitemOptTokensSub[kind].OnClick:= @MenuitemTokensClick;
      FMenuitemOptTokensSub[kind].RadioItem:= true;
      FMenuitemOptTokens.Add(FMenuitemOptTokensSub[kind]);
    end;

    FMenuitemOptHiAll:= TMenuItem.Create(Self);
    FMenuitemOptHiAll.OnClick:= @chkHiAllClick;

    FMenuitemOptRegexSubst:= TMenuItem.Create(Self);
    FMenuitemOptRegexSubst.OnClick:= @chkRegexSubstClick;

    FMenuitemOptPreserveCase:= TMenuItem.Create(Self);
    FMenuitemOptPreserveCase.OnClick:= @chkPreserveCaseClick;

    FMenuitemFindFirst:= TMenuItem.Create(Self);
    FMenuitemFindFirst.OnClick:= @bFindFirstClick;

    FMenuitemFindPrev:= TMenuItem.Create(Self);
    FMenuitemFindPrev.OnClick:= @bFindPrevClick;

    FMenuitemFindNext:= TMenuItem.Create(Self);
    FMenuitemFindNext.OnClick:= @bFindNextClick;

    Sep1:= TMenuItem.Create(Self);
    Sep1.Caption:= '-';

    Sep2:= TMenuItem.Create(Self);
    Sep2.Caption:= '-';

    Sep3:= TMenuItem.Create(Self);
    Sep3.Caption:= '-';

    FMenuitemCount:= TMenuItem.Create(Self);
    FMenuitemCount.OnClick:= @bCountClick;

    FMenuitemExtract:= TMenuItem.Create(Self);
    FMenuitemExtract.OnClick:= @bExtractClick;

    FMenuitemSelectAll:= TMenuItem.Create(Self);
    FMenuitemSelectAll.OnClick:= @bSelectAllClick;

    FMenuitemMarkAll:= TMenuItem.Create(Self);
    FMenuitemMarkAll.OnClick:= @bMarkAllClick;

    FMenuitemRep:= TMenuItem.Create(Self);
    FMenuitemRep.OnClick:= @bRepClick;

    FMenuitemRepStop:= TMenuItem.Create(Self);
    FMenuitemRepStop.OnClick:= @bRepStopClick;

    FMenuitemRepAll:= TMenuItem.Create(Self);
    FMenuitemRepAll.OnClick:= @bRepAllClick;

    FMenuitemRepGlobal:= TMenuItem.Create(Self);
    FMenuitemRepGlobal.OnClick:= @bRepGlobalClick;

    FPopupMore.Items.Add(FMenuitemOptRegex);
    FPopupMore.Items.Add(FMenuitemOptCase);
    FPopupMore.Items.Add(FMenuitemOptWords);
    FPopupMore.Items.Add(FMenuitemOptWrapped);
    FPopupMore.Items.Add(FMenuitemOptInSel);
    FPopupMore.Items.Add(FMenuitemOptMulti);
    FPopupMore.Items.Add(FMenuitemOptTokens);
    FPopupMore.Items.Add(FMenuitemOptHiAll);
    FPopupMore.Items.Add(FMenuitemOptRegexSubst);
    FPopupMore.Items.Add(FMenuitemOptPreserveCase);
    FPopupMore.Items.Add(Sep1);
    FPopupMore.Items.Add(FMenuitemFindFirst);
    FPopupMore.Items.Add(FMenuitemFindPrev);
    FPopupMore.Items.Add(FMenuitemFindNext);
    FPopupMore.Items.Add(Sep2);
    FPopupMore.Items.Add(FMenuitemCount);
    FPopupMore.Items.Add(FMenuitemExtract);
    FPopupMore.Items.Add(FMenuitemSelectAll);
    FPopupMore.Items.Add(FMenuitemMarkAll);
    FPopupMore.Items.Add(Sep3);
    FPopupMore.Items.Add(FMenuitemRep);
    FPopupMore.Items.Add(FMenuitemRepStop);
    FPopupMore.Items.Add(FMenuitemRepAll);
    FPopupMore.Items.Add(FMenuitemRepGlobal);
  end;

  FMenuitemOptRegex.Caption:= msgFindHint_Regex;
  FMenuitemOptRegex.Checked:= chkRegex.Checked;
  FMenuitemOptRegex.ShortCut:= TextToShortCut(UiOps.HotkeyToggleRegex);
  FMenuitemOptCase.Caption:= msgFindHint_Case;
  FMenuitemOptCase.Checked:= chkCase.Checked;
  FMenuitemOptCase.ShortCut:= TextToShortCut(UiOps.HotkeyToggleCaseSens);
  FMenuitemOptWords.Caption:= msgFindHint_Words;
  FMenuitemOptWords.Checked:= chkWords.Checked;
  FMenuitemOptWords.ShortCut:= TextToShortCut(UiOps.HotkeyToggleWords);
  FMenuitemOptWrapped.Caption:= msgFindHint_Wrapped;
  FMenuitemOptWrapped.Checked:= chkWrap.Checked;
  FMenuitemOptWrapped.ShortCut:= TextToShortCut(UiOps.HotkeyToggleWrapped);
  FMenuitemOptInSel.Caption:= msgFindHint_InSelect;
  FMenuitemOptInSel.Checked:= chkInSel.Checked;
  FMenuitemOptInSel.ShortCut:= TextToShortCut(UiOps.HotkeyToggleInSelect);
  FMenuitemOptMulti.Caption:= msgFindHint_MultiLine;
  FMenuitemOptMulti.Checked:= chkMulLine.Checked;
  FMenuitemOptMulti.ShortCut:= TextToShortCut(UiOps.HotkeyToggleMultiline);
  FMenuitemOptTokens.Caption:= msgFindHint_Tokens;
  FMenuitemOptTokens.ShortCut:= TextToShortCut(UiOps.HotkeyToggleTokens);
  for kind in TATFinderTokensAllowed do
  begin
    FMenuitemOptTokensSub[kind].Caption:= bTokens.Items[Ord(kind)];
    FMenuitemOptTokensSub[kind].Checked:= bTokens.ItemIndex=Ord(kind);
  end;

  FMenuitemOptHiAll.Caption:= msgFindHint_HiAll;
  FMenuitemOptHiAll.Checked:= chkHiAll.Checked;
  FMenuitemOptHiAll.ShortCut:= TextToShortCut(UiOps.HotkeyToggleHiAll);
  FMenuitemOptHiAll.Enabled:= chkHiAll.Enabled;
  if not FMenuitemOptHiAll.Enabled then
    FMenuitemOptHiAll.Caption:= FMenuitemOptHiAll.Caption+' '+
      Format('("find_hi_max_lines": %d)', [UiOps.FindHiAll_MaxLines]);

  FMenuitemOptRegexSubst.Caption:= msgFindHint_RegexSubst;
  FMenuitemOptRegexSubst.Checked:= chkRegexSubst.Checked;
  FMenuitemOptRegexSubst.Enabled:= IsReplace and chkRegex.Checked;

  FMenuitemOptPreserveCase.Caption:= msgFindHint_PresCase;
  FMenuitemOptPreserveCase.Checked:= chkPreserveCase.Checked;
  FMenuitemOptPreserveCase.Enabled:= IsReplace and not chkRegex.Checked;
  FMenuitemOptPreserveCase.ShortCut:= TextToShortCut(UiOps.HotkeyTogglePresCase);

  FMenuitemFindFirst.Caption:= SCaptionFindFirst;
  FMenuitemFindFirst.ShortCut:= TextToShortCut(UiOps.HotkeyFindFirst);
  FMenuitemFindPrev.Caption:= SCaptionFindPrev;
  FMenuitemFindPrev.Shortcut:= TextToShortcut(UiOps.HotkeyFindPrev);
  FMenuitemFindNext.Caption:= SCaptionFindNext;
  FMenuitemFindNext.Shortcut:= TextToShortcut(UiOps.HotkeyFindNext);
  FMenuitemCount.Caption:= SCaptionCount;
  FMenuitemCount.Shortcut:= TextToShortcut(UiOps.HotkeyCountAll);
  FMenuitemExtract.Caption:= SCaptionExtract;
  FMenuitemExtract.Shortcut:= TextToShortcut(UiOps.HotkeyExtractAll);
  FMenuitemSelectAll.Caption:= SCaptionSelect;
  FMenuitemSelectAll.Shortcut:= TextToShortcut(UiOps.HotkeySelectAll);
  FMenuitemMarkAll.Caption:= SCaptionMark;
  FMenuitemMarkAll.Shortcut:= TextToShortcut(UiOps.HotkeyMarkAll);
  FMenuitemRep.Caption:= SCaptionRep;
  FMenuitemRep.Shortcut:= TextToShortcut(UiOps.HotkeyReplaceAndFindNext);
  FMenuitemRep.Enabled:= IsReplace;
  FMenuitemRepStop.Caption:= SCaptionRepStop;
  FMenuitemRepStop.Shortcut:= TextToShortcut(UiOps.HotkeyReplaceNoFindNext);
  FMenuitemRepStop.Enabled:= IsReplace;
  FMenuitemRepAll.Caption:= SCaptionRepAll;
  FMenuitemRepAll.Shortcut:= TextToShortcut(UiOps.HotkeyReplaceAll);
  FMenuitemRepAll.Enabled:= IsReplace;
  FMenuitemRepGlobal.Caption:= SCaptionRepGlobal;
  FMenuitemRepGlobal.Shortcut:= TextToShortcut(UiOps.HotkeyReplaceGlobal);
  FMenuitemRepGlobal.Enabled:= IsReplace;
end;

procedure TfmFind.MenuitemTokensClick(Sender: TObject);
begin
  bTokens.ItemIndex:= (Sender as TMenuItem).Tag;
  bTokens.Invalidate;
end;

procedure TfmFind.bMoreClick(Sender: TObject);
var
  P: TPoint;
begin
  InitPopupMore;
  FMenuitemExtract.Enabled:= chkRegex.Checked;

  P:= bMore.ClientToScreen(Point(0, 0));
  FPopupMore.Popup(P.X, P.Y);
end;

procedure TfmFind.bRepAllClick(Sender: TObject);
begin
  if IsReplace then
    DoResult(afoReplaceAll);
end;

procedure TfmFind.bCountClick(Sender: TObject);
begin
  DoResult(afoCountAll);
end;

procedure TfmFind.bCancelClick(Sender: TObject);
begin
  DoResult(afoCloseDlg);
end;

procedure TfmFind.bRepGlobalClick(Sender: TObject);
begin
  if IsReplace then
    if MsgBox(msgConfirmReplaceGlobal, MB_OKCANCEL or MB_ICONWARNING)=ID_OK then
      DoResult(afoReplaceGlobal);
end;

procedure TfmFind.bSelectAllClick(Sender: TObject);
begin
  DoResult(afoFindSelectAll);
end;

procedure TfmFind.bTokensClick(Sender: TObject);
begin
  UpdateState(true);
  DoOnChange;
end;

procedure TfmFind.chkCaseClick(Sender: TObject);
begin
  with chkCase do
    Checked:= not Checked;
  UpdateState(true);
  DoOnChange;
end;

procedure TfmFind.chkConfirmClick(Sender: TObject);
begin
  with chkConfirm do
    Checked:= not Checked;
  UpdateState(false);
  DoOnChange;
end;

procedure TfmFind.chkHiAllClick(Sender: TObject);
begin
  with chkHiAll do
    Checked:= not Checked;
  UpdateState(true);
  DoOnChange;
end;

procedure TfmFind.chkInSelClick(Sender: TObject);
begin
  with chkInSel do
    Checked:= not Checked;
  UpdateState(true);
  DoOnChange;
end;

procedure TfmFind.chkMulLineClick(Sender: TObject);
begin
  IsMultiLine:= not IsMultiLine;
  DoOnChange;
end;

procedure TfmFind.chkPreserveCaseClick(Sender: TObject);
begin
  with chkPreserveCase do
    Checked:= not Checked;
  UpdateState(false);
  DoOnChange;
end;

procedure TfmFind.chkRepChange(Sender: TObject);
begin
  UpdateState(false);
end;

procedure TfmFind.bFindFirstClick(Sender: TObject);
begin
  DoResult(afoFindFirst);
end;

procedure TfmFind.chkRepClick(Sender: TObject);
begin
  UpdateState(false);
end;

procedure TfmFind.chkWordsClick(Sender: TObject);
begin
  with chkWords do
    Checked:= not Checked;
  UpdateState(true);
  DoOnChange;
end;

procedure TfmFind.chkWrapClick(Sender: TObject);
begin
  with chkWrap do
    Checked:= not Checked;
  UpdateState(false);
  DoOnChange;
end;

procedure TfmFind.edFindChange(Sender: TObject);
begin
  UpdateState(true);
  if AdapterActive then
    EditorHighlightBadRegexBrackets(edFind, false);
end;

procedure TfmFind.edFindCommand(Sender: TObject; ACommand: integer;
  AInvoke: TATCommandInvoke; const AText: string; var AHandled: boolean);
var
  Ed: TATSynEdit;
begin
  //auto turn on multi-line
  if ACommand=cCommand_KeyEnter then
  begin
    FMultiLineJustActivated:= not IsMultiLine;
    IsMultiLine:= true;
    AHandled:= false;
    exit
  end;

  if (ACommand>=cmdFirstAppCommand) and (ACommand<=cmdLastAppCommand) then
  begin
    FOnGetMainEditor(Ed);
    Ed.DoCommand(ACommand, TATCommandInvoke.Hotkey, '');
    AHandled:= true;
    exit;
  end;
end;

procedure TfmFind.edFindCommandAfter(Sender: TObject; ACommand: integer; const AText: string);
begin
  if ACommand=cCommand_KeyEnter then
  begin
    //fix bad scrollpos=1, after Ctrl+Enter activated multi-line mode
    if FMultiLineJustActivated then
    begin
      edFind.ScrollVert.SetZero;
      edRep.ScrollVert.SetZero;
      edFind.Update;
      edRep.Update;
    end;
    exit;
  end;
end;

procedure TfmFind.edFindEnter(Sender: TObject);
begin
  edFind.DoSelect_All;
  UpdateButtonBold;
end;

procedure TfmFind.CopyFieldFindToReplace;
begin
  edRep.Text:= edFind.Text;
  edRep.DoCaretSingle(0, 0);
  edRep.DoScrollToBeginOrEnd(true);
  edRep.Update(true);
end;

procedure TfmFind.edFindKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  //Ctrl+Down: copy find_edit to replace_edit
  if (Key=VK_DOWN) and (Shift=[ssCtrl]) then
  begin
    CopyFieldFindToReplace;
    Key:= 0;
    exit
  end;

  //Ctrl+Enter: add line-break
  if (Key=VK_RETURN) and (Shift=[ssCtrl]) then
  begin
    (Sender as TATSynEdit).DoCommand(cCommand_KeyEnter, TATCommandInvoke.AppInternal);
    Key:= 0;
    exit;
  end;
end;

procedure TfmFind.edRepEnter(Sender: TObject);
begin
  edRep.DoSelect_All;
  UpdateButtonBold;
end;

procedure TfmFind.edRepExit(Sender: TObject);
begin
  UpdateButtonBold;
end;

procedure TfmFind.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction:= caHide;
  ClearHiAll;
end;

procedure TfmFind.ClearHiAll;
var
  Ed: TATSynEdit;
begin
  FOnGetMainEditor(Ed);
  if Assigned(Ed) then
    EditorClearHiAllMarkers(Ed);
end;

procedure TfmFind.FormCreate(Sender: TObject);
var
  kind: TATFinderTokensAllowed;
begin
  FCaptionFind:= 'Find';
  FCaptionReplace:= 'Replace';

  edFind.OptTabSize:= 4;
  edRep.OptTabSize:= 4;

  edFind.Keymap:= AppKeymapMain;
  edRep.Keymap:= AppKeymapMain;

  edFind.Strings.Endings:= TATLineEnds.Unix;
  edRep.Strings.Endings:= TATLineEnds.Unix;

  edFind.OptUnprintedSpaces:= false;
  edRep.OptUnprintedSpaces:= false;
  edFind.OptUnprintedEndsDetails:= false;
  edRep.OptUnprintedEndsDetails:= false;

  edFind.OptPasteAtEndMakesFinalEmptyLine:= false;
  edRep.OptPasteAtEndMakesFinalEmptyLine:= false;

  IsDoubleBuffered:= UiOps.DoubleBuffered;
  AppScalePanelControls(Self);

  bFindFirst.Hint:= UiOps.HotkeyFindFirst;
  bFindNext.Hint:= UiOps.HotkeyFindNext;
  bFindPrev.Hint:= UiOps.HotkeyFindPrev;
  bRep.Hint:= UiOps.HotkeyReplaceAndFindNext;
  bRepAll.Hint:= UiOps.HotkeyReplaceAll;
  bRepGlobal.Hint:= UiOps.HotkeyReplaceGlobal;

  for kind in TATFinderTokensAllowed do
  begin
    bTokens.Items.Add(cTokensDesc[kind]);
    bTokens.ItemsShort.Add(cTokensShorts[kind]);
  end;

  //bTokens.Arrow:= true;
  bTokens.ShowShortItems:= true;
  bTokens.TextAlign:= taCenter;
  bTokens.ItemIndex:= 0;
  bTokens.Checkable:= true;

  chkRegexSubst.Checked:= true;

  Adapter:= TATAdapterEControl.Create(Self);
  Adapter.Lexer:= AppManager.FindLexerByName('RegEx');

  FTimerShow:= TTimer.Create(Self);
  FTimerShow.Enabled:= false;
  FTimerShow.Interval:= 100;
  FTimerShow.OnTimer:= @TimerShowTick;
end;

procedure TfmFind.FormHide(Sender: TObject);
begin
  if Assigned(FOnChangeVisible) then
    FOnChangeVisible(Self);
end;


procedure TfmFind.UpdateFonts;
  //
  procedure UpdateEdFont(Ed: TATSynEdit);
  begin
    Ed.Font.Name:= EditorOps.OpFontName;
    Ed.Font.Size:= EditorOps.OpFontSize;
    Ed.Font.Quality:= EditorOps.OpFontQuality;
    Ed.OptBorderFocusedActive:= EditorOps.OpActiveBorderInControls;
    Ed.OptBorderWidthFocused:= ATEditorScale(EditorOps.OpActiveBorderWidth);
    EditorApplyTheme(Ed);
    Ed.Update;
  end;
  //
begin
  UpdateEdFont(edFind);
  UpdateEdFont(edRep);
  //bCancel.Font.Assign(LabelFind.Font);
end;

procedure TfmFind.UpdateFocus(AFindMode: boolean);
begin
  if (not Visible) or (not Enabled) then exit;

  if AFindMode then
  begin
    if edFind.Visible and edFind.Enabled and edFind.CanFocus then
      edFind.SetFocus;
  end
  else
  if edRep.Visible and edRep.Enabled and edRep.CanFocus then
    edRep.SetFocus;
end;

procedure TfmFind.UpdateInputFind(const AText: UnicodeString);
var
  bMultiLineText: boolean;
begin
  edFind.Text:= AText;
  edFind.DoEventChange(0); //for lexer RegEx

  bMultiLineText:= Pos(#10, AText)>0;
  IsMultiLine:= bMultiLineText;
end;

procedure TfmFind.UpdateInputReplace(const AText: UnicodeString);
begin
  edRep.Text:= AText;
end;

procedure TfmFind.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  Str: string;
begin
  Str:= ShortcutToText(KeyToShortCut(Key, Shift));
  if Str='' then exit; //only Shift is pressed

  if (Key=VK_ESCAPE) and (Shift=[]) then
  begin
    if UiOps.EscapeCloseFinder then
      DoResult(afoCloseDlg)
    else
      DoFocusEditor;
    key:= 0;
    exit;
  end;

  if Str=UiOps.HotkeyFindFirst then
  begin
    DoResult(afoFindFirst);
    key:= 0;
    exit
  end;

  if (Str=UiOps.HotkeyFindNext) and (Str<>'Enter') then
  begin
    DoResult(afoFindNext);
    key:= 0;
    exit
  end;

  if Str='Enter' then
  begin
    //Enter: action depends on focus
    if IsReplace and edRep.Focused then
      DoResult(afoReplace)
    else
      DoResult(afoFindNext);
    key:= 0;
    exit
  end;

  if Str=UiOps.HotkeyFindPrev then
  begin
    DoResult(afoFindPrev);
    key:= 0;
    exit
  end;

  if (Str=UiOps.HotkeyReplaceAndFindNext) and IsReplace then
  begin
    if IsReplace then
      DoResult(afoReplace);
    key:= 0;
    exit
  end;

  if (Str=UiOps.HotkeyReplaceNoFindNext) and IsReplace then
  begin
    if IsReplace then
      DoResult(afoReplaceStop);
    key:= 0;
    exit
  end;

  if Str=UiOps.HotkeyFindDialog then
  begin
    //ST4 and VSCode both stay in the Find dlg on pressing Ctrl+F
    if IsReplace then
    begin
      IsReplace:= false;
      UpdateState(true);
    end;
    //feature: Ctrl+F from inside the Find dlg, does select-all
    edFind.DoSelect_All;
    edFind.Update;
    key:= 0;
    exit;
  end;

  if Str=UiOps.HotkeyReplaceDialog then
  begin
    //ST4 and VSCode both stay in the Replace dlg on pressing Ctrl+H
    if not IsReplace then
    begin
      IsReplace:= true;
      UpdateState(true);
    end;
    key:= 0;
    exit;
  end;

  if Str=UiOps.HotkeyToggleRegex then
  begin
    chkRegexClick(Self);
    UpdateState(true);
    key:= 0;
    exit
  end;

  if Str=UiOps.HotkeyToggleCaseSens then
  begin
    chkCaseClick(Self);
    UpdateState(true);
    key:= 0;
    exit
  end;

  if Str=UiOps.HotkeyToggleWords then
  begin
    chkWordsClick(Self);
    UpdateState(true);
    key:= 0;
    exit
  end;

  if Str=UiOps.HotkeyToggleConfirmRep then
  begin
    chkConfirmClick(Self);
    UpdateState(false);
    key:= 0;
    exit
  end;

  if Str=UiOps.HotkeyToggleWrapped then
  begin
    chkWrapClick(Self);
    UpdateState(false);
    key:= 0;
    exit
  end;

  if Str=UiOps.HotkeyToggleInSelect then
  begin
    chkInSelClick(Self);
    UpdateState(true);
    key:= 0;
    exit
  end;

  if Str=UiOps.HotkeyToggleTokens then
  begin
    bTokens.Click;
    UpdateState(true);
    key:= 0;
    exit
  end;

  if Str=UiOps.HotkeyToggleHiAll then
  begin
    chkHiAll.Click;
    UpdateState(true);
    key:= 0;
    exit
  end;

  if Str=UiOps.HotkeyToggleMultiline then
  begin
    chkMulLineClick(Self);
    UpdateState(false);
    key:= 0;
    exit
  end;

  if (Str=UiOps.HotkeyTogglePresCase) and IsReplace then
  begin
    chkPreserveCaseClick(Self);
    UpdateState(false);
    key:= 0;
    exit
  end;

  if (Str=UiOps.HotkeyReplaceAll) and IsReplace then
  begin
    bRepAllClick(Self);
    UpdateState(false);
    key:= 0;
    exit
  end;

  if (Str=UiOps.HotkeyReplaceGlobal) and IsReplace then
  begin
    bRepGlobalClick(Self);
    UpdateState(false);
    key:= 0;
    exit
  end;

  if Str=UiOps.HotkeyCountAll then
  begin
    bCountClick(Self);
    UpdateState(false);
    key:= 0;
    exit
  end;

  if (Str=UiOps.HotkeyExtractAll) and chkRegex.Checked then
  begin
    bExtractClick(Self);
    UpdateState(false);
    key:= 0;
    exit
  end;

  if Str=UiOps.HotkeySelectAll then
  begin
    bSelectAllClick(Self);
    UpdateState(false);
    key:= 0;
    exit
  end;

  if Str=UiOps.HotkeyMarkAll then
  begin
    bMarkAllClick(Self);
    UpdateState(false);
    key:= 0;
    exit
  end;

  //avoid handling of Shift+Tab in the editor (it runs "Unindent block")
  if (Key=VK_TAB) and (Shift*[ssCtrl, ssAlt]=[]) then
  begin
    //SelectNext() LCL method works worse
    if IsReplace then
    begin
      if edFind.Focused then
        edRep.SetFocus
      else
      if edRep.Focused then
        edFind.SetFocus;
    end;
    key:= 0;
    exit;
  end;

  if Assigned(FOnHandleKeyDown) then
    if FOnHandleKeyDown(Key, Shift) then
      Key:= 0;
end;

function BtnSize(C: TControl): integer;
begin
  if C.Visible then
    Result:= C.Width
  else
    Result:= 0;
end;

procedure TfmFind.FormResize(Sender: TObject);
var
  Size1, Size2: integer;
begin
  Size1:=
    BtnSize(bFindFirst)+
    BtnSize(bFindNext)+
    BtnSize(bFindPrev)+
    BtnSize(bMore);

  Size2:=
    BtnSize(bRep)+
    BtnSize(bRepAll)+
    BtnSize(bRepGlobal);
  if not IsReplace then
    Size2:= 0;

  edFind.Left:= cPadding;
  PanelBtn.Width:= Max(Size1, Size2);
  PanelTopOps.Left:= edFind.Left;
end;


procedure TfmFind.FormShow(Sender: TObject);
begin
  edFind.OptCaretBlinkEnabled:= EditorOps.OpCaretBlinkEn;
  edRep.OptCaretBlinkEnabled:= EditorOps.OpCaretBlinkEn;
  edFind.OptCaretBlinkTime:= EditorOps.OpCaretBlinkTime;
  edRep.OptCaretBlinkTime:= EditorOps.OpCaretBlinkTime;

  edFind.OptMouseMiddleClickAction:= TATEditorMiddleClickAction(EditorOps.OpMouseMiddleClickAction);
  edRep.OptMouseMiddleClickAction:= TATEditorMiddleClickAction(EditorOps.OpMouseMiddleClickAction);

  edFind.OptPasteWithEolAtLineStart:= EditorOps.OpPasteWithEolAtLineStart;
  edRep.OptPasteWithEolAtLineStart:= EditorOps.OpPasteWithEolAtLineStart;

  EditorCaretShapeFromString(edFind.CaretShapeNormal, EditorOps.OpCaretViewNormal);
  EditorCaretShapeFromString(edFind.CaretShapeOverwrite, EditorOps.OpCaretViewOverwrite);
  EditorCaretShapeFromString(edRep.CaretShapeNormal, EditorOps.OpCaretViewNormal);
  EditorCaretShapeFromString(edRep.CaretShapeOverwrite, EditorOps.OpCaretViewOverwrite);

  UpdateFormHeight;
  UpdateFonts;
  FixFormPositionToDesktop(Self);
  OnResize(Self);

  if Assigned(FOnChangeVisible) then
    FOnChangeVisible(Self);

  FTimerShow.Enabled:= true;
end;

procedure TfmFind.UpdateInitialCaretPos;
//should be called on dlg creation + on each dialog activation,
//like in Sublime
var
  Ed: TATSynEdit;
begin
  OnGetMainEditor(Ed);
  if Ed.Carets.Count>0 then
    FInitialCaretPos:= Ed.Carets[0].GetLeftEdge;
end;

procedure TfmFind.DoResult(Op: TAppFinderOperation);
var
  bUpdateState: boolean;
begin
  if edFind.Text='' then
    if Op<>afoCloseDlg then exit;

  if Assigned(FOnResult) then
    FOnResult(Self, Op);

  bUpdateState:= not (Op in [
    afoNone,
    afoCloseDlg,
    afoCountAll,
    afoExtractAll,
    afoFindMarkAll,
    afoFindSelectAll
    ]);

  if Op<>afoCloseDlg then
  begin
    edFind.DoAddLineToHistory(edFind.Text, UiOps.MaxHistoryEdits);
    edRep.DoAddLineToHistory(edRep.Text, UiOps.MaxHistoryEdits);
    if bUpdateState then
      UpdateState(false);
  end;
end;

function TfmFind.GetHiAll: boolean;
begin
  Result:= chkHiAll.Checked;
end;

procedure TfmFind.SetHiAll(AValue: boolean);
begin
  if chkHiAll.Checked<>AValue then
  begin
    chkHiAll.Checked:= AValue;
    UpdateState(true);
  end;
end;

procedure TfmFind.SetIsDoubleBuffered(AValue: boolean);
begin
  edFind.DoubleBuffered:= AValue;
  edRep.DoubleBuffered:= AValue;

  {
  //no need
  chkRegex.DoubleBuffered:= AValue;
  chkCase.DoubleBuffered:= AValue;
  chkWords.DoubleBuffered:= AValue;
  chkWrap.DoubleBuffered:= AValue;
  chkMulLine.DoubleBuffered:= AValue;
  chkConfirm.DoubleBuffered:= AValue;

  bFindFirst.DoubleBuffered:= AValue;
  bFindNext.DoubleBuffered:= AValue;
  bFindPrev.DoubleBuffered:= AValue;
  bCount.DoubleBuffered:= AValue;
  bSelectAll.DoubleBuffered:= AValue;
  bMarkAll.DoubleBuffered:= AValue;
  bRep.DoubleBuffered:= AValue;
  bRepAll.DoubleBuffered:= AValue;
  bCancel.DoubleBuffered:= AValue;
  }
end;

procedure TfmFind.SetMultiLine(AValue: boolean);
var
  NSizeY: integer;
begin
  FMultiLine:= AValue;
  chkMulLine.Checked:= AValue;

  edFind.ModeOneLine:= not FMultiLine;
  edRep.ModeOneLine:= not FMultiLine;

  edFind.OptUnprintedVisible:= FMultiLine;
  edRep.OptUnprintedVisible:= FMultiLine;

  NSizeY:= bFindFirst.Height;
  if FMultiLine then NSizeY:= Trunc(NSizeY*UiOps.FindMultiLineScale);

  edFind.Height:= NSizeY;
  edRep.Height:= NSizeY;

  UpdateState(false);
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
    chkInSel.Parent:= PanelTopOps;
    chkMulLine.Parent:= PanelTopOps;
    bTokens.Parent:= PanelTopOps;
    chkHiAll.Parent:= PanelTopOps;
    chkConfirm.Parent:= PanelTopOps;
    chkConfirm.Left:= 400; //to right
    chkRegexSubst.Parent:= PanelTopOps;
    chkPreserveCase.Parent:= PanelTopOps;
    chkRegexSubst.Left:= chkConfirm.Left+chkConfirm.Width; //to right
    chkPreserveCase.Left:= chkRegexSubst.Left+chkRegexSubst.Width; //to right
  end;

  PanelTopOps.Left:= edFind.Left;
end;

procedure TfmFind.SetReplace(AValue: boolean);
begin
  if FReplace=AValue then Exit;
  FReplace:= AValue;

  //in find mode: focus input, because old focused control maybe hidden now
  if not FReplace then
    if edFind.CanFocus then
      edFind.SetFocus;

  Caption:= CurrentCaption;
  UpdateState(false);
end;

procedure TfmFind.TimerShowTick(Sender: TObject);
begin
  FTimerShow.Enabled:= false;
  //fixing caret in the middle of field, #4670
  edFind.Update;
  edRep.Update;
end;

procedure TfmFind.UpdateFormHeight;
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
    //Lazarus sometimes gets big negative p.x/p.y
    if Result<0 then
      Result:= 0;
  end;
  //
const
  cMinHeight = 30;
  cHeightIncrease = 4;
var
  N: integer;
begin
  if IsReplace then
    N:= MaxY(edRep)
  else
    N:= MaxY(edFind);
  N:= Max(cMinHeight, N+cHeightIncrease);
  Constraints.MinHeight:= N;
  Constraints.MaxHeight:= N;
  Height:= N;
end;

procedure TfmFind.ControlAutosizeOptionsByWidth;
var
  Ar: array of TATButton;
  N, i: integer;
begin
  Ar:= [
    chkRegex,
    chkCase,
    chkWords,
    chkWrap,
    chkInSel,
    chkMulLine,
    bTokens,
    chkHiAll
    ];

  N:= 10; //indents left/right
  for i:= 0 to High(Ar) do
    if Ar[i].Visible then
      Inc(N, Ar[i].Width);
  PanelOps.Width:= N;
end;

function TfmFind.CurrentCaption: string;
begin
  if IsReplace then
    Result:= FCaptionReplace
  else
    Result:= FCaptionFind;
end;

procedure TfmFind.UpdateState(AEnableFindNextForHiOption: boolean);
var
  Ed: TATSynEdit;
  bEnabled: boolean;
begin
  cPadding:= ATEditorScale(4);
  bEnabled:= Self.Enabled;

  PanelTop.Visible:= IsNarrow;
  PanelOps.Visible:= not IsNarrow;
  edRep.Visible:= IsReplace;
  PanelBtnRep.Visible:= IsReplace;

  edFind.Enabled:= bEnabled;
  edRep.Enabled:= bEnabled and IsReplace;
  bFindFirst.Enabled:= bEnabled;
  bFindNext.Enabled:= bEnabled;
  bFindPrev.Enabled:= bEnabled;
  bRep.Enabled:= bEnabled and IsReplace and not FForViewer;
  bRepAll.Enabled:= bEnabled and IsReplace and not FForViewer;
  bRepGlobal.Enabled:= bEnabled and IsReplace and not FForViewer;
  bMore.Enabled:= bEnabled and not FForViewer;

  FOnGetMainEditor(Ed);
  //sometimes, Ed=Nil here (after changing groups 2->1)
  chkHiAll.Enabled:= bEnabled and Assigned(Ed) and (Ed.Strings.Count<UiOps.FindHiAll_MaxLines);

  chkCase.Enabled:= bEnabled;
  chkWords.Enabled:= bEnabled and not chkRegex.Checked and (edFind.Strings.Count<2); //disable "w" for multi-line input
  chkRegex.Enabled:= bEnabled and not FForViewer;
  chkWrap.Enabled:= bEnabled and not FForViewer;
  chkInSel.Enabled:= bEnabled and not FForViewer;
  chkMulLine.Enabled:= bEnabled;
  chkConfirm.Enabled:= bEnabled and IsReplace and not FForViewer;
  chkRegexSubst.Enabled:= bEnabled and IsReplace and not FForViewer and chkRegex.Checked;
  chkPreserveCase.Enabled:= bEnabled and IsReplace and not FForViewer and not chkRegex.Checked;
  bTokens.Enabled:= bEnabled and not FForViewer;

  bFindFirst.Visible:= UiOps.FindShow_FindFirst;
  bFindNext.Visible:= UiOps.FindShow_FindNext;
  bFindPrev.Visible:= UiOps.FindShow_FindPrev;
  bRepAll.Visible:= UiOps.FindShow_ReplaceAll;
  bRepGlobal.Visible:= UiOps.FindShow_ReplaceGlobal;

  chkRegex.Visible:= UiOps.FindShow_RegEx;
  chkCase.Visible:= UiOps.FindShow_CaseSens;
  chkWords.Visible:= UiOps.FindShow_WholeWords;
  chkWrap.Visible:= UiOps.FindShow_Wrapped;
  chkInSel.Visible:= UiOps.FindShow_InSel;
  chkMulLine.Visible:= UiOps.FindShow_MultiLine;
  bTokens.Visible:= UiOps.FindShow_SyntaxElements;
  chkHiAll.Visible:= UiOps.FindShow_HiAll;
  chkConfirm.Visible:= (IsReplace or IsNarrow) and UiOps.FindShow_ConfirmRep;
  chkRegexSubst.Visible:= (IsReplace or IsNarrow) and UiOps.FindShow_RegexSubst;
  chkPreserveCase.Visible:= (IsReplace or IsNarrow) and UiOps.FindShow_PreserveCase;
  ControlAutosizeOptionsByWidth;

  edFind.Left:= cPadding;
  edRep.Left:= cPadding;
  edFind.OptTextHint:= msgFindHint_InputFind;
  edRep.OptTextHint:= msgFindHint_InputRep;

  //form can be already closed ('select all' closes it);
  //and we must avoid UpdateHiAll etc.
  if not Visible then exit;

  UpdateButtonBold;
  UpdateFormHeight;
  FormResize(nil);

  UpdateRegexHighlight;

  UpdateHiAll(AEnableFindNextForHiOption);
end;

procedure TfmFind.UpdateRegexHighlight;
var
  bActive: boolean;
  TempLexer: TecSyntAnalyzer;
begin
  bActive:= chkRegex.Checked;
  if AdapterActive<>bActive then
  begin
    AdapterActive:= bActive;
    if AdapterActive then
    begin
      if not FLexerRegexThemed then
      begin
        FLexerRegexThemed:= true;
        if Assigned(Adapter.Lexer) then
          DoApplyLexerStylesMap(Adapter.Lexer, TempLexer);
      end;

      Adapter.AddEditor(edFind);
      edFind.DoEventChange(0);
      //Adapter.ParseFromLine(0, true); //seems not needed with parser thread
    end
    else
    begin
      edFind.AdapterForHilite:= nil;
      Adapter.AddEditor(nil);
      EditorHighlightBadRegexBrackets(edFind, true);
    end;
    edFind.Update;
  end;
end;

procedure TfmFind.UpdateButtonBold;
begin
  bFindNext.BoldFont:= not edRep.Focused;
  bRep.BoldFont:= edRep.Focused;
end;

procedure TfmFind.DoOnChange;
begin
  if Assigned(FOnChangeOptions) then
    FOnChangeOptions(nil);
end;

procedure TfmFind.Localize;
const
  section = 'd_f';
var
  fn: string;
  ini: TIniFile;
begin
  fn:= AppFile_Language;
  if FileExists(fn) then
  begin
    ini:= TIniFile.Create(fn);
    try
      FCaptionFind:= ini.ReadString(section, '_f', FCaptionFind);
      FCaptionReplace:= ini.ReadString(section, '_r', FCaptionReplace);
      with bRep do Caption:= ini.ReadString(section, 'r_c', Caption);
      with bRepAll do Caption:= ini.ReadString(section, 'r_a', Caption);
      with bRepGlobal do Caption:= ini.ReadString(section, 'r_gl', Caption);

      msgFindHint_InputFind:= ini.ReadString(section, 'h_xf', msgFindHint_InputFind);
      msgFindHint_InputRep:= ini.ReadString(section, 'h_xr', msgFindHint_InputRep);
      msgFindHint_FindFirst:= ini.ReadString(section, 'h_f1', msgFindHint_FindFirst);
      msgFindHint_FindNext:= ini.ReadString(section, 'h_fn', msgFindHint_FindNext);
      msgFindHint_FindPrev:= ini.ReadString(section, 'h_fp', msgFindHint_FindPrev);
      msgFindHint_Rep:= ini.ReadString(section, 'h_r', msgFindHint_Rep);
      msgFindHint_RepAll:= ini.ReadString(section, 'h_r_a', msgFindHint_RepAll);
      msgFindHint_RepGlobal:= ini.ReadString(section, 'h_r_g', msgFindHint_RepGlobal);
      msgFindHint_Regex:= ini.ReadString(section, 'h_re', msgFindHint_Regex);
      msgFindHint_RegexSubst:= ini.ReadString(section, 'h_subs', msgFindHint_RegexSubst);
      msgFindHint_Case:= ini.ReadString(section, 'h_ca', msgFindHint_Case);
      msgFindHint_Words:= ini.ReadString(section, 'h_wo', msgFindHint_Words);
      msgFindHint_Wrapped:= ini.ReadString(section, 'h_wr', msgFindHint_Wrapped);
      msgFindHint_ConfirmRep:= ini.ReadString(section, 'h_cf', msgFindHint_ConfirmRep);
      msgFindHint_InSelect:= ini.ReadString(section, 'h_sel', msgFindHint_InSelect);
      msgFindHint_MultiLine:= ini.ReadString(section, 'h_mul', msgFindHint_MultiLine);
      msgFindHint_Tokens:= ini.ReadString(section, 'h_tok', msgFindHint_Tokens);
      msgFindHint_HiAll:= ini.ReadString(section, 'h_hi', msgFindHint_HiAll);
      msgFindHint_PresCase:= ini.ReadString(section, 'h_pres', msgFindHint_PresCase);

      with bTokens do
      begin
        Items[0]:= ini.ReadString(section, 'tk_a', Items[0]);
        Items[1]:= ini.ReadString(section, 'tk_y_c', Items[1]);
        Items[2]:= ini.ReadString(section, 'tk_y_s', Items[2]);
        Items[3]:= ini.ReadString(section, 'tk_y_cs', Items[3]);
        Items[4]:= ini.ReadString(section, 'tk_n_c', Items[4]);
        Items[5]:= ini.ReadString(section, 'tk_n_s', Items[5]);
        Items[6]:= ini.ReadString(section, 'tk_n_cs', Items[6]);
      end;
    finally
      FreeAndNil(ini);
    end;
  end;

  bFindFirst.Hint:= _MakeHint(msgFindHint_FindFirst, UiOps.HotkeyFindFirst);
  bFindNext.Hint:= _MakeHint(msgFindHint_FindNext, UiOps.HotkeyFindNext);
  bFindPrev.Hint:= _MakeHint(msgFindHint_FindPrev, UiOps.HotkeyFindPrev);

  bRep.Hint:= _MakeHint(msgFindHint_Rep, UiOps.HotkeyReplaceAndFindNext);
  bRepAll.Hint:= _MakeHint(msgFindHint_RepAll, UiOps.HotkeyReplaceAll);
  bRepGlobal.Hint:= _MakeHint(msgFindHint_RepGlobal, UiOps.HotkeyReplaceGlobal);

  chkRegex.Hint:= _MakeHint(msgFindHint_Regex, UiOps.HotkeyToggleRegex);
  chkRegexSubst.Hint:= _MakeHint(msgFindHint_RegexSubst, '');
  chkCase.Hint:= _MakeHint(msgFindHint_Case, UiOps.HotkeyToggleCaseSens);
  chkWords.Hint:= _MakeHint(msgFindHint_Words, UiOps.HotkeyToggleWords);
  chkWrap.Hint:= _MakeHint(msgFindHint_Wrapped, UiOps.HotkeyToggleWrapped);
  chkConfirm.Hint:= _MakeHint(msgFindHint_ConfirmRep, UiOps.HotkeyToggleConfirmRep);
  chkInSel.Hint:= _MakeHint(msgFindHint_InSelect, UiOps.HotkeyToggleInSelect);
  chkMulLine.Hint:= _MakeHint(msgFindHint_MultiLine, UiOps.HotkeyToggleMultiline);
  bTokens.Hint:= _MakeHint(msgFindHint_Tokens, UiOps.HotkeyToggleTokens);
  chkHiAll.Hint:= _MakeHint(msgFindHint_HiAll, UiOps.HotkeyToggleHiAll);
  chkPreserveCase.Hint:= _MakeHint(msgFindHint_PresCase, UiOps.HotkeyTogglePresCase);

  bFindFirst.AutoSize:= true;
  bFindNext.AutoSize:= true;
  bFindPrev.AutoSize:= true;
  bMore.AutoSize:= true;
  bRep.AutoSize:= true;
  bRepAll.AutoSize:= true;
  bRepGlobal.AutoSize:= true;
end;

procedure TfmFind.DoFocusEditor;
begin
  if Assigned(FOnFocusEditor) then
    FOnFocusEditor(nil);
end;

procedure TfmFind.UpdateHiAll(AEnableFindNext: boolean);
var
  Finder: TATEditorFinder;
  NMatches: integer;
  NColorBG: TColor;
  NTick: QWord;
begin
  if FForViewer then
  begin
    NColorBG:= GetAppColor(apclEdTextBg);
    edFind.Colors.TextBG:= NColorBG;
    edFind.Update;
    exit;
  end;

  ClearHiAll;

  if UiOps.FindShowNoResultsByInputBgColor then
  begin
    NColorBG:= GetAppColor(apclEdTextBg);
    edFind.Colors.TextBG:= NColorBG;
    edFind.Update;
  end;

  if edFind.Text='' then exit;
  if not chkHiAll.Enabled then exit;

  if IsHiAll then
  begin
    Finder:= TATEditorFinder.Create;
    try
      FOnGetMainEditor(Finder.Editor);
      Finder.StrFind:= edFind.Text;
      Finder.OptConfirmReplace:= false;
      Finder.OptFromCaret:= false;
      Finder.OptInSelection:= chkInSel.Checked;
      Finder.OptPutBackwardSelection:= false;
      Finder.OptBack:= false;
      Finder.OptCase:= chkCase.Checked;
      Finder.OptWords:= chkWords.Checked;
      Finder.OptRegex:= chkRegex.Checked;
      Finder.OptTokens:= TATFinderTokensAllowed(bTokens.ItemIndex);
      Finder.OptWrapped:= chkWrap.Checked;
      Finder.OptPreserveCase:= chkPreserveCase.Checked;
      Finder.OnGetToken:= FOnGetToken;

      NTick:= GetTickCount64;
      EditorHighlightAllMatches(Finder, AEnableFindNext, NMatches, FInitialCaretPos);
      NTick:= GetTickCount64-NTick;

      if UiOps.FindShowNoResultsByInputBgColor then
      begin
        if NMatches=0 then
          NColorBG:= ColorBlendHalf(
                       GetAppColor(apclEdTextBg),
                       GetAppColor(apclButtonBgDisabled))
        else
          NColorBG:= GetAppColor(apclEdTextBg);

        edFind.Colors.TextBG:= NColorBG;
        edFind.Update;
      end;

      if NMatches=0 then //fixing #4775
        if Assigned(FOnShowMatchesCount) then
          FOnShowMatchesCount(NMatches, NTick);
    finally
      FreeAndNil(Finder);
    end;
  end;
end;

procedure TfmFind.UpdateCaption(const AText: string);
begin
  if not Assigned(Parent) then //show text only when dlg is not docked
    Caption:= CurrentCaption+': '+AText;
end;

procedure TfmFind.ApplyTheme;
var
  TempLexer: TecSyntAnalyzer;
begin
  Color:= GetAppColor(apclTabBg);

  EditorApplyTheme(edFind);
  EditorApplyTheme(edRep);

  if Assigned(Adapter.Lexer) then
    DoApplyLexerStylesMap(Adapter.Lexer, TempLexer);
  FLexerRegexThemed:= true;

  Invalidate;
end;

end.
