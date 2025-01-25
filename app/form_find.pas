(*
This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at http://mozilla.org/MPL/2.0/.

Copyright (c) Alexey Torgashin
*)
unit form_find;

{$mode objfpc}{$H+}
{$ScopedEnums on}

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
  ATSynEdit_Regexpr,
  proc_msg,
  proc_globdata,
  proc_miscutils,
  proc_colors,
  proc_editor,
  proc_cmd,
  ec_SyntAnal,
  form_lexer_stylemap;

type
  {$ScopedEnums on}
  TAppFinderOperation = (
    None,
    CloseDlg,
    FindFirst,
    FindNext,
    FindPrev,
    CountAll,
    ExtractAll,
    FindSelectAll,
    FindMarkAll,
    Replace,
    ReplaceStop,
    ReplaceAll,
    ReplaceGlobal
    );
  {$ScopedEnums off}

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
    None,
    CloseDlg,
    Find,
    ReplaceOne,
    ReplaceGlobal
    );

const
  cAppFinderOperationCategory: array[TAppFinderOperation] of TAppFinderOperationCategory = (
    TAppFinderOperationCategory.None,
    TAppFinderOperationCategory.CloseDlg,
    TAppFinderOperationCategory.Find,
    TAppFinderOperationCategory.Find,
    TAppFinderOperationCategory.Find,
    TAppFinderOperationCategory.Find,
    TAppFinderOperationCategory.Find,
    TAppFinderOperationCategory.Find,
    TAppFinderOperationCategory.Find,
    TAppFinderOperationCategory.ReplaceOne,
    TAppFinderOperationCategory.ReplaceOne,
    TAppFinderOperationCategory.ReplaceOne,
    TAppFinderOperationCategory.ReplaceGlobal
    );

type
  TAppFinderOperationEvent = procedure(Sender: TObject; Op: TAppFinderOperation;
    AUpdateEnabledAll, ADocumentIsSmall: boolean) of object;
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
    chkImmediate: TATButton;
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
    TimerIdle: TTimer;
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
    procedure chkImmediateClick(Sender: TObject);
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
    procedure FormDestroy(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure TimerIdleTimer(Sender: TObject);
  private
    { private declarations }
    FRegexObj: TRegExpr;
    FTimerShow: TTimer;
    FTimerWrapped: TTimer;
    FTimerHiAll: TTimer;
    FPopupMore: TPopupMenu;
    FPrevColorOfInput: TColor;
    FMenuitemOptRegex: TMenuItem;
    FMenuitemOptCase: TMenuItem;
    FMenuitemOptWords: TMenuItem;
    FMenuitemOptWrapped: TMenuItem;
    FMenuitemOptInSel: TMenuItem;
    FMenuitemOptMulti: TMenuItem;
    FMenuitemOptTokens: TMenuItem;
    FMenuitemOptTokensSub: array[TATFinderTokensAllowed] of TMenuItem;
    FMenuitemOptHiAll: TMenuItem;
    FMenuitemOptIm: TMenuItem;
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
    FInputChanged: boolean;
    FInputColored: boolean;
    FForViewer: boolean;
    FDocumentIsSmall: boolean;
    FOnResult: TAppFinderOperationEvent;
    FOnChangeVisible: TNotifyEvent;
    FOnChangeOptions: TNotifyEvent;
    FOnFocusEditor: TNotifyEvent;
    FOnGetMainEditor: TAppFinderGetEditor;
    FOnResetSearchString: TNotifyEvent;
    FOnGetToken: TATFinderGetToken;
    FOnShowMatchesCount: TAppFinderShowMatchesCount;
    FOnHandleKeyDown: TAppFinderKeyDownEvent;
    FCaptionFind: string;
    FCaptionReplace: string;
    FLexerRegexThemed: boolean;
    FHiAllEnableFindNext: boolean;
    FInitialCaretPos: TPoint;
    Adapter: TATAdapterEControl;
    AdapterActive: boolean;
    procedure bRepStopClick(Sender: TObject);
    procedure ControlAutosizeOptionsByWidth;
    procedure CopyFieldFindToReplace;
    procedure DoFocusEditor;
    procedure DoResult(Op: TAppFinderOperation; AUpdateEnabledAll: boolean=true);
    function GetHiAll: boolean;
    function GetImmediate: boolean;
    procedure InitPopupMore;
    function IsRegexInputOk: boolean;
    procedure MenuitemTokensClick(Sender: TObject);
    procedure SetHiAll(AValue: boolean);
    procedure SetImmediate(AValue: boolean);
    procedure SetInputColored(AValue: boolean);
    procedure SetIsDoubleBuffered(AValue: boolean);
    procedure SetMultiLine(AValue: boolean);
    procedure SetNarrow(AValue: boolean);
    procedure SetReplace(AValue: boolean);
    procedure TimerShowTick(Sender: TObject);
    procedure TimerWrappedTick(Sender: TObject);
    procedure TimerHiAllTick(Sender: TObject);
    procedure UpdateButtonBold;
    procedure UpdateRegexHighlight;
    function CurrentCaption: string;
  public
    { public declarations }
    property ForViewer: boolean read FForViewer write FForViewer;
    procedure Localize;
    procedure DoOnChange;
    procedure UpdateFormHeight;
    procedure UpdateCaretPosVar;
    procedure UpdateState(AEnableFindNextForHiOption: boolean);
    procedure UpdateFocus(AFindMode: boolean);
    procedure UpdateInputFieldsProps;
    procedure UpdateInputFind(const AText: UnicodeString);
    procedure UpdateInputReplace(const AText: UnicodeString);
    procedure UpdateCaption(const AText: string);
    procedure UpdateHiAll(AEnableFindNext: boolean);
    procedure UpdateInputReddishIndicator(AFound: boolean);
    procedure ClearHiAll;
    procedure ApplyTheme;
    property OnResult: TAppFinderOperationEvent read FOnResult write FOnResult;
    property OnChangeOptions: TNotifyEvent read FOnChangeOptions write FOnChangeOptions;
    property OnChangeVisible: TNotifyEvent read FOnChangeVisible write FOnChangeVisible;
    property OnFocusEditor: TNotifyEvent read FOnFocusEditor write FOnFocusEditor;
    property OnGetMainEditor: TAppFinderGetEditor read FOnGetMainEditor write FOnGetMainEditor;
    property OnResetSearchString: TNotifyEvent read FOnResetSearchString write FOnResetSearchString;
    property OnGetToken: TATFinderGetToken read FOnGetToken write FOnGetToken;
    property OnShowMatchesCount: TAppFinderShowMatchesCount read FOnShowMatchesCount write FOnShowMatchesCount;
    property OnHandleKeyDown: TAppFinderKeyDownEvent read FOnHandleKeyDown write FOnHandleKeyDown;
    property IsReplace: boolean read FReplace write SetReplace;
    property IsMultiLine: boolean read FMultiLine write SetMultiLine;
    property IsNarrow: boolean read FNarrow write SetNarrow;
    property IsHiAll: boolean read GetHiAll write SetHiAll;
    property IsImmediate: boolean read GetImmediate write SetImmediate;
    property IsDoubleBuffered: boolean write SetIsDoubleBuffered;
    property IsInputColored: boolean read FInputColored write SetInputColored;
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
  Result:= TAppFinderOperation.None;
end;

function _MakeHint(const AText, AHotkey: string): string;
begin
  Result:= AText;
  if Result<>'' then
    Result+= ' ';
  if AHotkey<>'' then
    Result+= '['+AHotkey+']';
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
var
  Ed: TATSynEdit;
  bEditorFocused: boolean;
begin
  OnGetMainEditor(Ed);
  bEditorFocused:= Assigned(Ed) and Ed.Focused;

  if IsReplace then
    DoResult(TAppFinderOperation.Replace);

  if bEditorFocused then
    EditorFocus(Ed);
end;

procedure TfmFind.bRepStopClick(Sender: TObject);
begin
  if IsReplace then
    DoResult(TAppFinderOperation.ReplaceStop);
end;

procedure TfmFind.bFindNextClick(Sender: TObject);
begin
  DoResult(TAppFinderOperation.FindNext);
end;

procedure TfmFind.bExtractClick(Sender: TObject);
begin
  DoResult(TAppFinderOperation.ExtractAll);
end;

procedure TfmFind.bFindPrevClick(Sender: TObject);
begin
  DoResult(TAppFinderOperation.FindPrev);
end;

procedure TfmFind.bMarkAllClick(Sender: TObject);
begin
  DoResult(TAppFinderOperation.FindMarkAll);
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

    FMenuitemOptIm:= TMenuItem.Create(Self);
    FMenuitemOptIm.OnClick:= @chkImmediateClick;

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
    FPopupMore.Items.Add(FMenuitemOptIm);
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

  FMenuitemOptRegex.Enabled:= chkRegex.Enabled;
  FMenuitemOptRegex.Caption:= msgFindHint_Regex;
  FMenuitemOptRegex.Checked:= chkRegex.Checked;
  FMenuitemOptRegex.ShortCut:= TextToShortCutRaw(UiOps.HotkeyToggleRegex);
  FMenuitemOptCase.Caption:= msgFindHint_Case;
  FMenuitemOptCase.Checked:= chkCase.Checked;
  FMenuitemOptCase.ShortCut:= TextToShortCutRaw(UiOps.HotkeyToggleCaseSens);
  FMenuitemOptWords.Caption:= msgFindHint_Words;
  FMenuitemOptWords.Checked:= chkWords.Checked;
  FMenuitemOptWords.ShortCut:= TextToShortCutRaw(UiOps.HotkeyToggleWords);
  FMenuitemOptWrapped.Enabled:= chkWrap.Enabled;
  FMenuitemOptWrapped.Caption:= msgFindHint_Wrapped;
  FMenuitemOptWrapped.Checked:= chkWrap.Checked;
  FMenuitemOptWrapped.ShortCut:= TextToShortCutRaw(UiOps.HotkeyToggleWrapped);
  FMenuitemOptInSel.Caption:= msgFindHint_InSelect;
  FMenuitemOptInSel.Checked:= chkInSel.Checked;
  FMenuitemOptInSel.ShortCut:= TextToShortCutRaw(UiOps.HotkeyToggleInSelect);
  FMenuitemOptMulti.Caption:= msgFindHint_MultiLine;
  FMenuitemOptMulti.Checked:= chkMulLine.Checked;
  FMenuitemOptMulti.ShortCut:= TextToShortCutRaw(UiOps.HotkeyToggleMultiline);
  FMenuitemOptTokens.Enabled:= bTokens.Enabled;
  FMenuitemOptTokens.Caption:= msgFindHint_Tokens;
  FMenuitemOptTokens.ShortCut:= TextToShortCutRaw(UiOps.HotkeyToggleTokens);
  for kind in TATFinderTokensAllowed do
  begin
    FMenuitemOptTokensSub[kind].Caption:= bTokens.Items[Ord(kind)];
    FMenuitemOptTokensSub[kind].Checked:= bTokens.ItemIndex=Ord(kind);
  end;

  FMenuitemOptHiAll.Caption:= msgFindHint_HiAll;
  FMenuitemOptHiAll.Checked:= chkHiAll.Checked;
  FMenuitemOptHiAll.ShortCut:= TextToShortCutRaw(UiOps.HotkeyToggleHiAll);
  FMenuitemOptHiAll.Enabled:= chkHiAll.Enabled;
  if not FMenuitemOptHiAll.Enabled then
    FMenuitemOptHiAll.Caption:= FMenuitemOptHiAll.Caption+' '+
      Format('("find_hi_max_lines": %d)', [UiOps.FindHiAll_MaxLines]);

  FMenuitemOptIm.Caption:= msgFindHint_Immediate;
  FMenuitemOptIm.Checked:= chkImmediate.Checked;
  FMenuitemOptIm.ShortCut:= TextToShortCutRaw(UiOps.HotkeyToggleImmediate);

  FMenuitemOptRegexSubst.Caption:= msgFindHint_RegexSubst;
  FMenuitemOptRegexSubst.Checked:= chkRegexSubst.Checked;
  FMenuitemOptRegexSubst.Enabled:= IsReplace and chkRegex.Checked;

  FMenuitemOptPreserveCase.Caption:= msgFindHint_PresCase;
  FMenuitemOptPreserveCase.Checked:= chkPreserveCase.Checked;
  FMenuitemOptPreserveCase.Enabled:= IsReplace and not chkRegex.Checked;
  FMenuitemOptPreserveCase.ShortCut:= TextToShortCutRaw(UiOps.HotkeyTogglePresCase);

  FMenuitemFindFirst.Caption:= SCaptionFindFirst;
  FMenuitemFindFirst.ShortCut:= TextToShortCutRaw(UiOps.HotkeyFindFirst);
  FMenuitemFindPrev.Caption:= SCaptionFindPrev;
  FMenuitemFindPrev.Shortcut:= TextToShortCutRaw(UiOps.HotkeyFindPrev);
  FMenuitemFindNext.Caption:= SCaptionFindNext;
  FMenuitemFindNext.Shortcut:= TextToShortCutRaw(UiOps.HotkeyFindNext);
  FMenuitemCount.Enabled:= not FForViewer;
  FMenuitemCount.Caption:= SCaptionCount;
  FMenuitemCount.Shortcut:= TextToShortCutRaw(UiOps.HotkeyCountAll);
  FMenuitemExtract.Enabled:= not FForViewer;
  FMenuitemExtract.Caption:= SCaptionExtract;
  FMenuitemExtract.Shortcut:= TextToShortCutRaw(UiOps.HotkeyExtractAll);
  FMenuitemSelectAll.Enabled:= not FForViewer;
  FMenuitemSelectAll.Caption:= SCaptionSelect;
  FMenuitemSelectAll.Shortcut:= TextToShortCutRaw(UiOps.HotkeySelectAll);
  FMenuitemMarkAll.Enabled:= not FForViewer;
  FMenuitemMarkAll.Caption:= SCaptionMark;
  FMenuitemMarkAll.Shortcut:= TextToShortCutRaw(UiOps.HotkeyMarkAll);
  FMenuitemRep.Caption:= SCaptionRep;
  FMenuitemRep.Shortcut:= TextToShortCutRaw(UiOps.HotkeyReplaceAndFindNext);
  FMenuitemRep.Enabled:= IsReplace;
  FMenuitemRepStop.Caption:= SCaptionRepStop;
  FMenuitemRepStop.Shortcut:= TextToShortCutRaw(UiOps.HotkeyReplaceNoFindNext);
  FMenuitemRepStop.Enabled:= IsReplace;
  FMenuitemRepAll.Caption:= SCaptionRepAll;
  FMenuitemRepAll.Shortcut:= TextToShortCutRaw(UiOps.HotkeyReplaceAll);
  FMenuitemRepAll.Enabled:= IsReplace;
  FMenuitemRepGlobal.Caption:= SCaptionRepGlobal;
  FMenuitemRepGlobal.Shortcut:= TextToShortCutRaw(UiOps.HotkeyReplaceGlobal);
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
  P:= bMore.ClientToScreen(Point(0, 0));
  FPopupMore.Popup(P.X, P.Y);
end;

procedure TfmFind.bRepAllClick(Sender: TObject);
var
  Ed: TATSynEdit;
  bEditorFocused: boolean;
begin
  OnGetMainEditor(Ed);
  bEditorFocused:= Assigned(Ed) and Ed.Focused;

  if IsReplace then
    DoResult(TAppFinderOperation.ReplaceAll);

  if bEditorFocused then
    EditorFocus(Ed);
end;

procedure TfmFind.bCountClick(Sender: TObject);
begin
  DoResult(TAppFinderOperation.CountAll);
end;

procedure TfmFind.bCancelClick(Sender: TObject);
begin
  DoResult(TAppFinderOperation.CloseDlg);
end;

procedure TfmFind.bRepGlobalClick(Sender: TObject);
begin
  if IsReplace then
    if MsgBox(msgConfirmReplaceGlobal, MB_OKCANCEL or MB_ICONWARNING)=ID_OK then
      DoResult(TAppFinderOperation.ReplaceGlobal);
end;

procedure TfmFind.bSelectAllClick(Sender: TObject);
begin
  DoResult(TAppFinderOperation.FindSelectAll);
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

procedure TfmFind.chkImmediateClick(Sender: TObject);
begin
  with chkImmediate do
    Checked:= not Checked;
  if chkImmediate.Checked then
    edFind.OnChange(nil);
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
  DoResult(TAppFinderOperation.FindFirst);
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

function EditorSizeIsSmall(Ed: TATSynEdit): boolean;
var
  NMaxDocumentSize, NCharCount: Int64;
const
  cMaxCalcTime = 40;
  cAverageLineLen = 50;
begin
  NMaxDocumentSize:= UiOps.FindHiAll_MaxLines*cAverageLineLen;
  NCharCount:= EditorGetCharCount(Ed, NMaxDocumentSize, cMaxCalcTime);
  {
  -1: AMaxChars is reached
  -2: AMaxTime is reached
  }
  Result:= (NCharCount>=0) and (NCharCount<=NMaxDocumentSize);
end;

procedure TfmFind.edFindChange(Sender: TObject);
var
  Ed: TATSynEdit;
begin
  FDocumentIsSmall:= false;

  if IsImmediate then
  begin
    FInputChanged:= true;
    if Assigned(FOnResetSearchString) then
      FOnResetSearchString(nil);
  end;

  {
  Look at how your browser works (Firefox). Cuda should behave the same.

  If find input is empty, when you start typing it finds from the start of the
  document. But once there's at least one char in find input, further find
  commands (find next, find previous or typing more chars in the input) should
  always find starting from caret position (or focus position in case of browser).

  https://github.com/Alexey-T/CudaText/issues/5466
  }
  if IsImmediate then
    if not chkRegex.Checked or IsRegexInputOk then
    begin
      OnGetMainEditor(Ed);
      if Ed=nil then exit;
      FDocumentIsSmall:= EditorSizeIsSmall(Ed);
      if FDocumentIsSmall then
      begin
        edFind.Update(true, true); //forced repaint the input field
        TimerHiAllTick(Self);
      end
      else
      begin
        FTimerHiAll.Enabled:= false;
        FTimerHiAll.Enabled:= true;
      end;
    end;

  if edFind.IsEmpty then
    UpdateInputReddishIndicator(true);

  UpdateRegexHighlight;

  if AdapterActive then
    EditorHighlightBadRegexBrackets(edFind, false);
end;

procedure TfmFind.edFindCommand(Sender: TObject; ACommand: integer;
  AInvoke: TATCommandInvoke; const AText: string; var AHandled: boolean);
var
  Ed: TATSynEdit;
begin
  //auto turn on multi-line
  if (ACommand=cCommand_KeyEnter) then
  begin
    if not IsMultiline then
    begin
      FMultiLineJustActivated:= true;
      IsMultiLine:= true;
    end;
    AHandled:= false;
    exit
  end;

  if (ACommand>=cmdFirstAppCommand) and (ACommand<=cmdLastAppCommand) then
  begin
    OnGetMainEditor(Ed);
    if Assigned(Ed) then
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
      FMultiLineJustActivated:= false;
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
  if edFind.Focused then exit; //must have for Qt5 build
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
    if IsMultiLine or UiOps.FindEnableCtrlEnterInSinleLineMode then
      (Sender as TATSynEdit).DoCommand(cCommand_KeyEnter, TATCommandInvoke.AppInternal);
    Key:= 0;
    exit;
  end;
end;

procedure TfmFind.edRepEnter(Sender: TObject);
begin
  if edRep.Focused then exit; //must have for Qt5
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
  OnGetMainEditor(Ed);
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

  FRegexObj:= TRegExpr.Create;

  IsDoubleBuffered:= UiOps.DoubleBuffered;
  AppScalePanelControls(Self);

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

  FTimerHiAll:= TTimer.Create(Self);
  FTimerHiAll.Enabled:= false;
  FTimerHiAll.Interval:= UiOps.FindHiAll_TimerInterval;
  FTimerHiAll.OnTimer:= @TimerHiAllTick;
end;

procedure TfmFind.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FRegexObj);
end;

procedure TfmFind.FormHide(Sender: TObject);
begin
  if Assigned(FOnChangeVisible) then
    FOnChangeVisible(Self);
end;


procedure TfmFind.UpdateInputFieldsProps;
  //
  procedure UpdateEdProps(Ed: TATSynEdit);
  begin
    Ed.Font.Name:= EditorOps.OpFontName;
    Ed.Font.Size:= EditorOps.OpFontSize;
    Ed.Font.Quality:= EditorOps.OpFontQuality;
    Ed.OptBorderFocusedActive:= EditorOps.OpActiveBorderInControls;
    Ed.OptBorderWidthFocused:= ATEditorScale(EditorOps.OpActiveBorderWidth);
    EditorApplyTheme(Ed);

    Ed.Colors.TextFont:= GetAppColor(TAppThemeColor.OtherTextFont, TAppThemeColor.EdTextFont);
    Ed.Colors.TextBG:= GetAppColor(TAppThemeColor.OtherTextBg, TAppThemeColor.EdTextBg);
    Ed.Update;
  end;
  //
begin
  UpdateEdProps(edFind);
  UpdateEdProps(edRep);
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
  Str:= ShortCutToTextRaw(KeyToShortCut(Key, Shift));
  if Str='' then exit; //only Shift is pressed

  if (Key=VK_ESCAPE) and (Shift=[]) then
  begin
    if UiOps.EscapeCloseFinder then
      DoResult(TAppFinderOperation.CloseDlg)
    else
      DoFocusEditor;
    if FTimerHiAll.Enabled then
      TimerHiAllTick(nil); //disarm timer, requested at #5353
    key:= 0;
    exit;
  end;

  if Str=UiOps.HotkeyFindFirst then
  begin
    DoResult(TAppFinderOperation.FindFirst);
    key:= 0;
    exit
  end;

  if (Str=UiOps.HotkeyFindNext) and (Str<>'Enter') then
  begin
    DoResult(TAppFinderOperation.FindNext);
    if FTimerHiAll.Enabled then
      TimerHiAllTick(nil); //disarm timer, requested at #5353
    key:= 0;
    exit
  end;

  if Str='Enter' then
  begin
    //Enter: action depends on focus
    if IsReplace and edRep.Focused then
      DoResult(TAppFinderOperation.Replace)
    else
      DoResult(TAppFinderOperation.FindNext);
    if FTimerHiAll.Enabled then
      TimerHiAllTick(nil); //disarm timer, requested at #5353
    key:= 0;
    exit
  end;

  if Str=UiOps.HotkeyFindPrev then
  begin
    DoResult(TAppFinderOperation.FindPrev);
    if FTimerHiAll.Enabled then
      TimerHiAllTick(nil); //disarm timer, requested at #5353
    key:= 0;
    exit
  end;

  if (Str=UiOps.HotkeyReplaceAndFindNext) and IsReplace then
  begin
    if IsReplace then
      DoResult(TAppFinderOperation.Replace);
    key:= 0;
    exit
  end;

  if (Str=UiOps.HotkeyReplaceNoFindNext) and IsReplace then
  begin
    if IsReplace then
      DoResult(TAppFinderOperation.ReplaceStop);
    key:= 0;
    exit
  end;

  if Str=ShortcutToTextRaw(AppKeymapMain.GetShortcutFromCommand(cmd_DialogFind)) then
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

  if Str=ShortcutToTextRaw(AppKeymapMain.GetShortcutFromCommand(cmd_DialogReplace)) then
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

  if Str=UiOps.HotkeyToggleImmediate then
  begin
    chkImmediate.Click;
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

  if Str=UiOps.HotkeyFindMenu then
  begin
    bMoreClick(nil);
    key:= 0;
    exit;
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
  Localize;

  edFind.OptCaretBlinkEnabled:= EditorOps.OpCaretBlinkEn;
  edRep.OptCaretBlinkEnabled:= EditorOps.OpCaretBlinkEn;
  edFind.OptCaretBlinkTime:= EditorOps.OpCaretBlinkTime;
  edRep.OptCaretBlinkTime:= EditorOps.OpCaretBlinkTime;

  edFind.OptMouseMiddleClickAction:= TATEditorMiddleClickAction(EditorOps.OpMouseMiddleClickAction);
  edRep.OptMouseMiddleClickAction:= TATEditorMiddleClickAction(EditorOps.OpMouseMiddleClickAction);

  edFind.OptPasteWithEolAtLineStart:= EditorOps.OpPasteWithEolAtLineStart;
  edRep.OptPasteWithEolAtLineStart:= EditorOps.OpPasteWithEolAtLineStart;

  edFind.OptScrollbarsNew:= EditorOps.OpScrollbarsNew;
  edRep.OptScrollbarsNew:= EditorOps.OpScrollbarsNew;

  EditorCaretShapeFromString(edFind.CaretShapeNormal, EditorOps.OpCaretViewNormal);
  EditorCaretShapeFromString(edFind.CaretShapeOverwrite, EditorOps.OpCaretViewOverwrite);
  EditorCaretShapeFromString(edRep.CaretShapeNormal, EditorOps.OpCaretViewNormal);
  EditorCaretShapeFromString(edRep.CaretShapeOverwrite, EditorOps.OpCaretViewOverwrite);

  UpdateFormHeight;
  UpdateInputFieldsProps;
  FixFormPositionToDesktop(Self);
  OnResize(Self);

  if Assigned(FOnChangeVisible) then
    FOnChangeVisible(Self);

  FTimerShow.Enabled:= true;
  UpdateState(false);
end;

procedure TfmFind.UpdateCaretPosVar;
//should be called on dlg creation + on each dialog activation,
//like in Sublime
var
  Ed: TATSynEdit;
begin
  OnGetMainEditor(Ed);
  if Assigned(Ed) and (Ed.Carets.Count>0) then
    FInitialCaretPos:= Ed.Carets[0].GetLeftEdge;
end;

procedure TfmFind.DoResult(Op: TAppFinderOperation; AUpdateEnabledAll: boolean=true);
var
  bUpdateState: boolean;
begin
  if edFind.IsEmpty then
    if Op<>TAppFinderOperation.CloseDlg then exit;

  bUpdateState:= Op in [
    TAppFinderOperation.FindFirst,
    TAppFinderOperation.FindNext,
    TAppFinderOperation.FindPrev,
    TAppFinderOperation.Replace,
    TAppFinderOperation.ReplaceStop,
    TAppFinderOperation.ReplaceAll,
    TAppFinderOperation.ReplaceGlobal
    ];
  if bUpdateState then
    UpdateState(false);

  if Assigned(FOnResult) then
    FOnResult(Self, Op, AUpdateEnabledAll, FDocumentIsSmall);

  if Op<>TAppFinderOperation.CloseDlg then
  begin
    TimerIdle.Enabled:= false;
    TimerIdle.Enabled:= true;
  end;
end;

function TfmFind.GetHiAll: boolean;
begin
  Result:= chkHiAll.Checked;
end;

function TfmFind.GetImmediate: boolean;
begin
  Result:= chkImmediate.Checked;
end;

procedure TfmFind.SetHiAll(AValue: boolean);
begin
  if chkHiAll.Checked<>AValue then
  begin
    chkHiAll.Checked:= AValue;
    UpdateState(true);
  end;
end;

procedure TfmFind.SetImmediate(AValue: boolean);
begin
  if chkImmediate.Checked<>AValue then
  begin
    chkImmediate.Checked:= AValue;
    UpdateState(true);
  end;
end;

procedure TfmFind.SetInputColored(AValue: boolean);
var
  NewColor: TColor;
begin
  if FInputColored=AValue then Exit;
  FInputColored:= AValue;

  if AValue then
  begin
    FPrevColorOfInput:= edFind.Colors.TextBG;
    NewColor:= FindAppColorByName(UiOps.FindWrapAtEdge_ThemeItem, clNone);
    if NewColor=clNone then exit;
    edFind.Colors.TextBG:= NewColor;
    edFind.Update;

    if not FDocumentIsSmall then
      Application.ProcessMessages;

    if FTimerWrapped=nil then
    begin
      FTimerWrapped:= TTimer.Create(Self);
      FTimerWrapped.Enabled:= false;
      FTimerWrapped.Interval:= UiOps.FindWrapAtEdge_Delay;
      FTimerWrapped.OnTimer:= @TimerWrappedTick;
    end;
    FTimerWrapped.Enabled:= false;
    FTimerWrapped.Enabled:= true;
  end
  else
  begin
    edFind.Colors.TextBG:= FPrevColorOfInput;
    edFind.Update;
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
  if FMultiLine then
    NSizeY:= NSizeY*UiOps.FindMultilineHeight div 100;

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
    chkImmediate.Parent:= PanelTopOps;
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

procedure TfmFind.TimerWrappedTick(Sender: TObject);
begin
  FTimerWrapped.Enabled:= false;
  IsInputColored:= false;
end;

procedure TfmFind.UpdateFormHeight;
var
  N, NPadding: integer;
  Ctl: TControl;
begin
  NPadding:= edFind.Top;

  if IsReplace then
    Ctl:= edRep
  else
    Ctl:= edFind;
  if not Ctl.Visible then
    Ctl:= edFind;

  N:= Ctl.Top+Ctl.Height;
  Inc(N, NPadding);
  if IsNarrow then
    Inc(N, PanelTopOps.Height+NPadding);

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
    chkHiAll,
    chkImmediate
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
  bMore.Enabled:= bEnabled;

  chkCase.Enabled:= bEnabled;
  chkWords.Enabled:= bEnabled and (FForViewer or not chkRegex.Checked) and (edFind.Strings.Count<2); //disable "w" for multi-line input
  chkRegex.Enabled:= bEnabled and not FForViewer;
  chkWrap.Enabled:= bEnabled and not FForViewer;
  chkInSel.Enabled:= bEnabled;
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
  chkImmediate.Visible:= UiOps.FindShow_Immediate;
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
      msgFindHint_Immediate:= ini.ReadString(section, 'h_im', msgFindHint_Immediate);
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

  UiOps.HotkeyFindNext:= ShortcutToTextRaw(AppKeymapMain.GetShortcutFromCommand(cmd_FindNext));
  UiOps.HotkeyReplaceAndFindNext:= ShortcutToTextRaw(AppKeymapMain.GetShortcutFromCommand(cmd_FindNextAndReplace));

  bFindFirst.Hint:= _MakeHint(msgFindHint_FindFirst, UiOps.HotkeyFindFirst);
  bFindNext.Hint:= _MakeHint(msgFindHint_FindNext, UiOps.HotkeyFindNext);
  bFindPrev.Hint:= _MakeHint(msgFindHint_FindPrev, UiOps.HotkeyFindPrev);

  bRep.Hint:= _MakeHint(msgFindHint_Rep, UiOps.HotkeyReplaceAndFindNext);
  bRepAll.Hint:= _MakeHint(msgFindHint_RepAll, UiOps.HotkeyReplaceAll);
  bRepGlobal.Hint:= _MakeHint(msgFindHint_RepGlobal, UiOps.HotkeyReplaceGlobal);
  bMore.Hint:= _MakeHint('', UiOps.HotkeyFindMenu);

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
  chkImmediate.Hint:= _MakeHint(msgFindHint_Immediate, UiOps.HotkeyToggleImmediate);
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
  Ed: TATSynEdit;
begin
  FTimerHiAll.Enabled:= false;

  if FForViewer then
  begin
    UpdateInputReddishIndicator(true);
    exit;
  end;

  if edFind.IsEmpty then
  begin
    ClearHiAll;
    exit;
  end;
  if not chkHiAll.Enabled then
  begin
    ClearHiAll;
    exit;
  end;

  FHiAllEnableFindNext:= AEnableFindNext;
  OnGetMainEditor(Ed);
  if Assigned(Ed) and (Ed.Strings.Count>UiOps.FindHiAll_TimerLines) then
  begin
    FTimerHiAll.Interval:= UiOps.FindHiAll_TimerInterval;
    FTimerHiAll.Enabled:= true;
  end
  else
    TimerHiAllTick(nil);
end;


procedure TfmFind.TimerHiAllTick(Sender: TObject);
var
  Ed: TATSynEdit;
  Caret: TATCaretItem;
  Finder: TATEditorFinder;
  Pnt: TPoint;
  NMatches: integer;
  NTick: QWord;
begin
  FTimerHiAll.Enabled:= false;

  OnGetMainEditor(Ed);
  if Ed=nil then exit;

  if FInputChanged then
  begin
    FInputChanged:= false;
    if not edFind.IsEmpty then
    begin
      if Ed.Carets.Count=0 then
        Ed.DoCaretSingle(0, 0);
      Caret:= Ed.Carets[0];

      Pnt:= Caret.GetLeftEdge;
      Ed.DoCaretSingle(Pnt.X, Pnt.Y);
      //Ed.Update;

      DoResult(TAppFinderOperation.FindNext, false);
      {
      it's important to pass param AUpdateEnableAll=False, to fix issue #5471.
      without False, DoResult will toggle fmFind.Enabled, so _fast_ key input
      to edFind will be partially lost.
      }
      {
      why always 'find next'? this is more like in other editors.
      from issue #5466:
      is Cuda the only editor with "find first"? Others I tested now don't have (Kate, Sublime, VSCode, Notepad++).
      Interesting. It's an useful command. With find first + always 'b', Cuda users have more control
      over find than users of other editors.
      }
    end;
  end;

  ClearHiAll;
  if IsHiAll then
  begin
    Finder:= TATEditorFinder.Create;
    try
      Finder.Editor:= Ed;
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
      Finder.MaxLineLen:= UiOps.FindHiAll_MaxLineLen;
      Finder.OnGetToken:= FOnGetToken;

      NTick:= GetTickCount64;
      EditorHighlightAllMatches(Finder, FHiAllEnableFindNext, NMatches, FInitialCaretPos);
      NTick:= GetTickCount64-NTick;

      {
      //removed, to avoid reddish indicator, when 'Hi' cannot find anything,
      //but Im' could find the match before 'Hi'
      if NMatches=0 then //fixing #4775
        if Assigned(FOnShowMatchesCount) then
          FOnShowMatchesCount(NMatches, NTick);
          }
    finally
      FreeAndNil(Finder);
    end;
  end;
end;

procedure TfmFind.UpdateInputReddishIndicator(AFound: boolean);
var
  NColorBG: TColor;
begin
  if UiOps.FindUseReddishIndicator then
  begin
    NColorBG:= GetAppColor(TAppThemeColor.OtherTextBg, TAppThemeColor.EdTextBg);

    if not AFound then
      NColorBG:= ColorBlendHalf(
                   NColorBG,
                   GetAppColor(TAppThemeColor.ButtonBgDisabled));

    edFind.Colors.TextBG:= NColorBG;
    edFind.Update;

    //fixing disappeared reddish indicator after SetInputColored(true) call
    FPrevColorOfInput:= NColorBG;
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
  NColor: TColor;
begin
  Color:= GetAppColor(TAppThemeColor.TabBg);

  EditorApplyTheme(edFind);
  EditorApplyTheme(edRep);

  NColor:= GetAppColor(TAppThemeColor.OtherTextFont, TAppThemeColor.EdTextFont);
  edFind.Colors.TextFont:= NColor;
  edRep.Colors.TextFont:= NColor;
  NColor:= GetAppColor(TAppThemeColor.OtherTextBg, TAppThemeColor.EdTextBg);
  edFind.Colors.TextBG:= NColor;
  edRep.Colors.TextBG:= NColor;

  edFind.Update;
  edRep.Update;

  if Assigned(Adapter.Lexer) then
    DoApplyLexerStylesMap(Adapter.Lexer, TempLexer);
  FLexerRegexThemed:= true;

  Invalidate;
end;

function TfmFind.IsRegexInputOk: boolean;
begin
  Result:= true;
  if not edFind.IsEmpty and chkRegex.Checked then
  try
    try
      FRegexObj.Expression:= edFind.Text;
      FRegexObj.Compile;
    except
      Result:= false;
    end;
  finally
    FRegexObj.Expression:= '';
  end;
end;

procedure TfmFind.TimerIdleTimer(Sender: TObject);
begin
  TimerIdle.Enabled:= false;

  if edFind.Text<>'' then
    edFind.DoAddLineToHistory(edFind.Text, UiOps.MaxHistoryEdits);

  if edRep.Text<>'' then
    edRep.DoAddLineToHistory(edRep.Text, UiOps.MaxHistoryEdits);
end;


end.
