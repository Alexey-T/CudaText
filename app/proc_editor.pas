(*
This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at http://mozilla.org/MPL/2.0/.

Copyright (c) Alexey Torgashin
*)
unit proc_editor;

{$mode objfpc}{$H+}
{$ModeSwitch advancedrecords}

interface

uses
  SysUtils,
  Classes, Graphics, Controls,
  ATSynEdit,
  ATSynEdit_Finder,
  ATStringProc,
  proc_globdata;

procedure EditorStartParse(Ed: TATSynEdit);
procedure EditorAdjustForBigFile(Ed: TATSynEdit);
function EditorIsEmpty(Ed: TATSynEdit): boolean;
function EditorIsModifiedEx(Ed: TATSynEdit): boolean;
procedure EditorFocus(C: TWinControl);
procedure EditorMouseClick_AtCursor(Ed: TATSynEdit; AAndSelect: boolean);
procedure EditorMouseClick_NearCaret(Ed: TATSynEdit; const AParams: string; AAndSelect: boolean);
procedure EditorSetFont(F: TFont; const AParams: string);
function EditorBookmarksToString(Ed: TATSynEdit): string;
procedure EditorStringToBookmarks(Ed: TATSynEdit; const AValue: string);

procedure EditorClear(Ed: TATSynEdit);
function EditorGetCurrentChar(Ed: TATSynEdit): Widechar;
procedure EditorApplyOps(Ed: TATSynEdit; const Op: TEditorOps;
  AApplyUnprintedAndWrap, AApplyTabSize, AApplyCentering: boolean);
procedure EditorApplyOpsCommon(Ed: TATSynEdit);

function EditorGetLinkAtScreenCoord(Ed: TATSynEdit; P: TPoint): atString;
function EditorGetLinkAtCaret(Ed: TATSynEdit): atString;
function EditorLexerNameAtPos(Ed: TATSynEdit; APos: TPoint): string;

type
  TEditorSelectionKind = (
    selkindNone,
    selkindSmallSel,
    selkindStreamSel,
    selkindColumnSel,
    selkindCarets
    );
  TEditorSimpleEvent = procedure(Ed: TATSynEdit) of object;
  TEditorBooleanEvent = procedure(Ed: TATSynEdit; AValue: boolean) of object;

function EditorGetSelectionKind(Ed: TATSynEdit): TEditorSelectionKind;
function EditorFormatStatus(ed: TATSynEdit; const MacroText: string): string;
procedure EditorDeleteNewColorAttribs(ed: TATSynEdit);
procedure EditorGotoLastEditingPos(Ed: TATSynEdit; AIndentHorz, AIndentVert: integer);
function EditorGotoFromString(Ed: TATSynEdit; SInput: string): boolean;

procedure EditorApplyTheme(Ed: TATSynedit);
procedure EditorSetColorById(Ed: TATSynEdit; const Id: string; AColor: TColor);
function EditorGetColorById(Ed: TATSynEdit; const Id: string): TColor;

function EditorIsAutocompleteCssPosition(Ed: TATSynEdit; AX, AY: integer): boolean;
function EditorAutoSkipClosingBracket(Ed: TATSynEdit; CharClosing: char): boolean;
function EditorAutoDeleteClosingBracket(Ed: TATSynEdit): boolean;
function EditorAutoPairChar(Ed: TATSynEdit; CharBegin: atChar): boolean;
procedure EditorCopySelToPrimarySelection(Ed: TATSynEdit; AMaxLineCount: integer);
procedure EditorCopyLine(Ed: TATSynEdit);
procedure EditorCopyAsHTML(Ed: TATSynEdit);

procedure EditorSetLine(Ed: TATSynEdit; AIndex: integer; AStr: UnicodeString);
procedure EditorSetAllText(Ed: TATSynEdit; const AStr: string);
procedure EditorDeleteRange(Ed: TATSynEdit; X1, Y1, X2, Y2: integer);
function EditorInsert(Ed: TATSynEdit; AX, AY: integer; const AStr: UnicodeString; out APosAfter: TPoint): boolean;
procedure EditorHighlightBadRegexBrackets(Ed: TATSynEdit; AOnlyClear: boolean);

procedure EditorConvertTabsToSpaces(Ed: TATSynEdit);
procedure EditorConvertIndentation(Ed: TATSynEdit; ASpacesToTabs: boolean);

procedure EditorCaretShapeFromString(Props: TATCaretShape; const AText: string);
procedure EditorCaretShapeFromPyTuple(Props: TATCaretShape; const AText: string);
function EditorCaretIsOnStart(Ed: TATSynEdit): boolean;

type
  TATEditorBracketKind = (
    bracketUnknown,
    bracketOpening,
    bracketClosing
    );

  TATEditorBracketAction = (
    bracketActionHilite,
    bracketActionJump,
    bracketActionSelect,
    bracketActionSelectInside
    );

const
  cEditorTagForBracket = 1;

type
  TATEditorGetTokenKind = function(Ed: TATSynEdit; AX, AY: integer): TATTokenKind of object;

function EditorBracket_GetPairForOpeningBracketOrQuote(ch: char): char;
function EditorBracket_GetPairForClosingBracketOrQuote(ch: char): char;

function EditorBracket_ClearHilite(Ed: TATSynEdit): boolean;
procedure EditorBracket_FindBoth(Ed: TATSynEdit;
  var PosX, PosY: integer;
  const AllowedSymbols: string;
  MaxDistance: integer;
  out FoundX, FoundY: integer;
  out CharFrom, CharTo: atChar;
  out Kind: TATEditorBracketKind);
procedure EditorBracket_Action(Ed: TATSynEdit;
  Action: TATEditorBracketAction;
  const AllowedSymbols: string;
  MaxDistance: integer);
procedure EditorBracket_FindOpeningBracketBackward(Ed: TATSynEdit;
  PosX, PosY: integer;
  const AllowedSymbols: string;
  MaxDistance: integer;
  out FoundX, FoundY: integer);

type
  TATEditorFinderCallback = procedure(AFound: boolean; AFinder: TATEditorFinder) of object;

function EditorGetTokenKind(Ed: TATSynEdit; AX, AY: integer;
  ADocCommentIsAlsoComment: boolean=true): TATTokenKind;
function EditorExpandSelectionToWord(Ed: TATSynEdit;
  AFinderResultCallback: TATEditorFinderCallback;
  AAddOrSkip, AWholeWords: boolean): boolean;
function EditorFindCurrentWordOrSel(Ed: TATSynEdit;
  ANext, AWordOrSel, AOptCase, AOptWrapped: boolean;
  out Str: UnicodeString): boolean;
procedure EditorHighlightAllMatches(AFinder: TATEditorFinder;
  AEnableFindNext: boolean; out AMatchesCount: integer; ACaretPos: TPoint);

function EditorAutoCompletionAfterTypingChar(Ed: TATSynEdit;
  const AText: string; var ACharsTyped: integer): boolean;
function EditorGetLefterHtmlTag(Ed: TATSynEdit; AX, AY: integer): UnicodeString;
function EditorGetLefterWordChars(Ed: TATSynEdit; AX, AY: integer): integer;
procedure EditorAutoCloseOpeningHtmlTag(Ed: TATSynEdit; AX, AY: integer);
procedure EditorAutoCloseClosingHtmlTag(Ed: TATSynEdit; AX, AY: integer);
procedure EditorChangeLineEndsForSelection(Ed: TATSynEdit; AValue: TATLineEnds);
procedure EditorClearHiAllMarkers(Ed: TATSynEdit);
procedure EditorForceUpdateIfWrapped(Ed: TATSynEdit);
procedure EditorScrollToCaret(Ed: TATSynEdit; ANeedWrapOff, AllowProcessMsg: boolean);
procedure EditorCaretToView(Ed: TATSynEdit; ANeedWrapOff, AllowProcessMsg: boolean);
procedure EditorCalcOffsetsForStatusbar(Ed: TATSynEdit; out AOffsetMax, AOffsetCaret: integer);

type
  TATEditorTempOptions = record
    FontSize: integer;
    TabSize: integer;
    TabSpaces: boolean;
    WrapMode: TATEditorWrapMode;
    ShowMinimap: boolean;
    ShowMicromap: boolean;
    ShowRuler: boolean;
    ShowNumbers: boolean;
    ShowFolding: boolean;
    ShowUnprinted: boolean;
    UnprintedSpaces: boolean;
    UnprintedSpacesTrail: boolean;
    UnprintedSpacesInSel: boolean;
    UnprintedEnds: boolean;
    UnprintedEndsDetails: boolean;
    UnprintedForceTabs: boolean;
  end;

procedure EditorSaveTempOptions(Ed: TATSynEdit; out Ops: TATEditorTempOptions);
procedure EditorRestoreTempOptions(Ed: TATSynEdit; const ANew, AOld: TATEditorTempOptions);

implementation

uses
  StrUtils,
  Dialogs, Forms, Clipbrd,
  LCLType, LCLIntf, LazUTF8,
  ATSynEdit_Globals,
  ATSynEdit_LineParts,
  ATSynEdit_Carets,
  ATSynEdit_Markers,
  ATSynEdit_Commands,
  ATSynEdit_CharSizer,
  ATSynEdit_Gutter_Decor,
  ATSynEdit_Adapters,
  ATSynEdit_Adapter_EControl,
  ATSynEdit_Adapter_LiteLexer,
  ATSynEdit_Bookmarks,
  ATSynEdit_Cmp_HTML,
  ATSynEdit_Cmp_Form,
  ATSynEdit_RegExpr,
  ATSynEdit_Export_HTML,
  ATSynEdit_FGL,
  ATStrings,
  ATStringProc_Separator,
  ATStringProc_TextBuffer,
  proc_colors,
  proc_msg,
  proc_str,
  ec_SyntAnal,
  ec_syntax_format,
  math;

procedure EditorApplyOps(Ed: TATSynEdit; const Op: TEditorOps;
  AApplyUnprintedAndWrap, AApplyTabSize, AApplyCentering: boolean);
var
  Sep: TATStringSeparator;
  MouseActions: TATEditorMouseActions;
  N: integer;
begin
  Ed.Font.Name:= Op.OpFontName;
  Ed.FontItalic.Name:= Op.OpFontName_i;
  Ed.FontBold.Name:= Op.OpFontName_b;
  Ed.FontBoldItalic.Name:= Op.OpFontName_bi;

  Ed.Font.Size:= Op.OpFontSize;
  Ed.FontItalic.Size:= Op.OpFontSize_i;
  Ed.FontBold.Size:= Op.OpFontSize_b;
  Ed.FontBoldItalic.Size:= Op.OpFontSize_bi;

  Ed.Font.Quality:= Op.OpFontQuality;
  Ed.OptShowFontLigatures:= Op.OpFontLigatures;
  Ed.OptFlickerReducingPause:= Op.OpFlickerReducingPause;

  Ed.OptScrollbarsNew:= Op.OpScrollbarsNew;
  Ed.OptSpacingY:= Op.OpSpacingY;

  if AApplyTabSize then
  begin
    Ed.OptTabSize:= Op.OpTabSize;
    Ed.OptTabSpaces:= Op.OpTabSpaces;
  end;
  Ed.OptTabSmart:= Op.OpTabSmart;

  Ed.OptBorderFocusedActive:= Op.OpActiveBorderInEditor;
  Ed.OptBorderWidthFocused:= ATEditorScale(Op.OpActiveBorderWidth);

  Ed.OptOverwriteSel:= Op.OpOverwriteSel;
  Ed.OptOverwriteAllowedOnPaste:= Op.OpOverwriteOnPaste;
  Ed.OptPasteWithEolAtLineStart:= Op.OpPasteWithEolAtLineStart;

  Ed.OptAutoPairForMultiCarets:= Op.OpAutoCloseBracketsMultiCarets;
  Ed.OptAutoPairChars:= Op.OpAutoCloseBrackets;
  Ed.OptAutocompleteAutoshowCharCount:= Op.OpAutocompleteAutoshowCharCount;
  Ed.OptAutocompleteTriggerChars:= Op.OpAutocompleteTriggerChars;
  Ed.OptAutocompleteCommitChars:= Op.OpAutocompleteCommitChars;
  Ed.OptAutocompleteCloseChars:= Op.OpAutocompleteCloseChars;
  Ed.OptAutocompleteAddOpeningBracket:= Op.OpAutocompleteAddOpeningBracket;
  Ed.OptAutocompleteUpDownAtEdge:= Op.OpAutocompleteUpDownAtEdge;
  Ed.OptAutocompleteCommitIfSingleItem:= Op.OpAutocompleteCommitIfSingleItem;

  if not Ed.ModeOneLine then
  begin
    Ed.OptGutterVisible:= Op.OpGutterShow;
    Ed.OptGutterShowFoldAlways:= Op.OpGutterFoldAlways;
    Ed.OptGutterIcons:= TATEditorGutterIcons(Op.OpGutterFoldIcons);
    if not (TATEditorModifiedOption.GutterBookmarks  in Ed.ModifiedOptions) then
      Ed.Gutter[Ed.Gutter.FindIndexByTag(ATEditorOptions.GutterTagBookmarks)].Visible:= Op.OpGutterBookmarks;
    if not (TATEditorModifiedOption.GutterFolding in Ed.ModifiedOptions) then
      Ed.Gutter[Ed.Gutter.FindIndexByTag(ATEditorOptions.GutterTagFolding)].Visible:= Op.OpGutterFold;
    if not (TATEditorModifiedOption.GutterNumbers in Ed.ModifiedOptions) then
      Ed.Gutter[Ed.Gutter.FindIndexByTag(ATEditorOptions.GutterTagNumbers)].Visible:= Op.OpNumbersShow;
    //if not Ed.IsModifiedGutterLineStatesVisible then
      Ed.Gutter[Ed.Gutter.FindIndexByTag(ATEditorOptions.GutterTagLineStates)].Visible:= Op.OpGutterLineStates;
    Ed.OptGutterPlusSize:= Op.OpGutterIconSize;
    Ed.Gutter.Update;

    if (Op.OpNumbersStyle>=0) and (Op.OpNumbersStyle<=Ord(High(TATEditorNumbersStyle))) then
      Ed.OptNumbersStyle:= TATEditorNumbersStyle(Op.OpNumbersStyle);
    Ed.OptNumbersShowCarets:= Op.OpNumbersForCarets;
    if Op.OpNumbersCenter then
      Ed.OptNumbersAlignment:= taCenter
    else
      Ed.OptNumbersAlignment:= taRightJustify;

    if not (TATEditorModifiedOption.RulerVisible in Ed.ModifiedOptions) then
      Ed.OptRulerVisible:= Op.OpRulerShow;
    Ed.OptRulerNumeration:= TATEditorRulerNumeration(Op.OpRulerNumeration);
    Ed.OptRulerMarkSizeCaret:= Op.OpRulerMarkCaret;
    Ed.OptRulerHeightPercents:= Op.OpRulerHeight;

    if not (TATEditorModifiedOption.MinimapVisible in Ed.ModifiedOptions) then
      Ed.OptMinimapVisible:= Op.OpMinimapShow;
    Ed.OptMinimapShowSelAlways:= Op.OpMinimapShowSelAlways;
    Ed.OptMinimapShowSelBorder:= Op.OpMinimapShowSelBorder;
    Ed.OptMinimapCharWidth:= Op.OpMinimapCharWidth;
    Ed.OptMinimapAtLeft:= Op.OpMinimapAtLeft;
    Ed.OptMinimapCustomScale:= Op.OpMinimapScale;
    Ed.OptMinimapTooltipVisible:= Op.OpMinimapTooltipShow;
    Ed.OptMinimapTooltipHeight:= Op.OpMinimapTooltipHeight;
    Ed.OptMinimapTooltipWidthPercents:= Op.OpMinimapTooltipWidth;
    Ed.OptMinimapTooltipFontSize:= Op.OpMinimapTooltipFontSize;
    Ed.OptMinimapDragImmediately:= Op.OpMinimapDragImmediately;

    if not (TATEditorModifiedOption.MicromapVisible in Ed.ModifiedOptions) then
      Ed.OptMicromapVisible:= Op.OpMicromapShow;
    Ed.OptMicromapOnScrollbar:= Op.OpMicromapOnScrollbar;
    Ed.OptMicromapLineStates:= Op.OpMicromapLineStates;
    Ed.OptMicromapSelections:= Op.OpMicromapSelections;
    Ed.OptMicromapBookmarks:= Op.OpMicromapBookmarks;

    Ed.OptMarginRight:= Op.OpMarginFixed;
    Ed.OptMarginString:= Op.OpMarginString;

    Ed.OptShowURLs:= Op.OpLinks;
    Ed.OptShowURLsRegex:= Op.OpLinksRegex;
  end;

  if AApplyUnprintedAndWrap then
  begin
    if not (TATEditorModifiedOption.UnprintedVisible in Ed.ModifiedOptions) then
      Ed.OptUnprintedVisible:= Op.OpUnprintedShow;

    if not (TATEditorModifiedOption.UnprintedSpaces in Ed.ModifiedOptions) then
      Ed.OptUnprintedSpaces:= Pos('s', Op.OpUnprintedContent)>0;

    if not (TATEditorModifiedOption.UnprintedEnds in Ed.ModifiedOptions) then
      Ed.OptUnprintedEnds:= Pos('e', Op.OpUnprintedContent)>0;

    if not (TATEditorModifiedOption.UnprintedEndDetails in Ed.ModifiedOptions) then
      Ed.OptUnprintedEndsDetails:= Pos('d', Op.OpUnprintedContent)>0;

    Ed.OptUnprintedSpacesTrailing:= Pos('t', Op.OpUnprintedContent)>0;
    Ed.OptUnprintedSpacesBothEnds:= Pos('l', Op.OpUnprintedContent)>0;
    Ed.OptUnprintedSpacesOnlyInSelection:= Pos('x', Op.OpUnprintedContent)>0;
    Ed.OptUnprintedSpacesAlsoInSelection:= Pos('X', Op.OpUnprintedContent)>0;
    Ed.OptUnprintedForceTabs:= Pos('T', Op.OpUnprintedContent)>0;
  end;

  if Pos('.', Op.OpUnprintedContent)>0 then
    ATEditorOptions.UnprintedEndSymbol:= TATEditorUnptintedEolSymbol.Dot
  else
  if Pos('p', Op.OpUnprintedContent)>0 then
    ATEditorOptions.UnprintedEndSymbol:= TATEditorUnptintedEolSymbol.Pilcrow
  else
    ATEditorOptions.UnprintedEndSymbol:= TATEditorUnptintedEolSymbol.ArrowDown;

  if AApplyUnprintedAndWrap then
    if not (TATEditorModifiedOption.WordWrap in Ed.ModifiedOptions) then
      if (Op.OpWrapMode>=0) and (Op.OpWrapMode<=Ord(High(TATEditorWrapMode))) then
        Ed.OptWrapMode:= TATEditorWrapMode(Op.OpWrapMode);

  Ed.OptWrapIndented:= Op.OpWrapIndented;
  Ed.OptWrapEnabledForMaxLines:= Op.OpWrapEnabledMaxLines;

  Ed.OptUndoLimit:= Op.OpUndoLimit;
  Ed.OptUndoGrouped:= Op.OpUndoGrouped;
  Ed.OptUndoAfterSave:= Op.OpUndoAfterSave;
  Ed.OptUndoMaxCarets:= Op.OpUndoMaxCarets;
  Ed.OptUndoIndentVert:= Op.OpUndoIndentVert;
  Ed.OptUndoIndentHorz:= Op.OpUndoIndentHorz;
  Ed.OptUndoPause:= Op.OpUndoPause;
  Ed.OptUndoForCaretJump:= Op.OpUndoMouseClicks;

  Ed.OptCaretBlinkTime:= Op.OpCaretBlinkTime;
  Ed.OptCaretBlinkEnabled:= Op.OpCaretBlinkEn;

  if not Ed.ModeOneLine then
  begin
    if not (TATEditorModifiedOption.CaretShapeNormal in Ed.ModifiedOptions) then
      EditorCaretShapeFromString(Ed.CaretShapeNormal, Op.OpCaretViewNormal);
    if not (TATEditorModifiedOption.CaretShapeOverwrite in Ed.ModifiedOptions) then
      EditorCaretShapeFromString(Ed.CaretShapeOverwrite, Op.OpCaretViewOverwrite);
    if not (TATEditorModifiedOption.CaretShapeReadonly in Ed.ModifiedOptions) then
      EditorCaretShapeFromString(Ed.CaretShapeReadonly, Op.OpCaretViewReadonly);

    if (Op.OpCaretAfterPasteColumn>=0) and (Op.OpCaretAfterPasteColumn<=Ord(High(TATEditorPasteCaret))) then
      Ed.OptCaretPosAfterPasteColumn:= TATEditorPasteCaret(Op.OpCaretAfterPasteColumn);

    Ed.OptCaretVirtual:= Op.OpCaretVirtual;
    Ed.OptCaretManyAllowed:= Op.OpCaretMulti;
    Ed.OptCaretsAddedToColumnSelection:= Op.OpCaretsAddedToColumnSel;
    Ed.OptCaretsPrimitiveColumnSelection:= Op.OpCaretsPrimitiveColumnSel;
    Ed.OptCaretProximityVert:= Op.OpCaretProximityVert;
    Ed.OptScrollLineCommandsKeepCaretOnScreen:= Op.OpCaretKeepVisibleOnScroll;

    Ed.OptShowCurLine:= Op.OpShowCurLine;
    Ed.OptShowCurLineMinimal:= Op.OpShowCurLineMinimal;
    Ed.OptShowCurLineOnlyFocused:= Op.OpShowCurLineOnlyFocused;
    Ed.OptShowCurColumn:= Op.OpShowCurCol;

    if not (TATEditorModifiedOption.LastLineOnTop in Ed.ModifiedOptions) then
      Ed.OptLastLineOnTop:= Op.OpShowLastLineOnTop;
  end;

  Ed.OptShowFullWidthForSelection:= Op.OpShowFullBackgroundSel;
  Ed.OptShowFullWidthForSyntaxHilite:= Op.OpShowFullBackgroundSyntax;
  Ed.OptShowMouseSelFrame:= Op.OpShowMouseSelFrame;
  Ed.OptShowIndentLines:= Op.OpShowIndentLines;
  Ed.OptCopyLinesIfNoSel:= Op.OpCopyLineIfNoSel;
  Ed.OptCutLinesIfNoSel:= Op.OpCutLineIfNoSel;
  Ed.OptCopyColumnBlockAlignedBySpaces:= Op.OpCopyColumnAlignedBySpaces;

  if not (TATEditorModifiedOption.SavingTrimSpaces in Ed.ModifiedOptions) then
    Ed.OptSavingTrimSpaces:= Op.OpSavingTrimSpaces;
  if not (TATEditorModifiedOption.SavingTrimFinalEmptyLines in Ed.ModifiedOptions) then
    Ed.OptSavingTrimFinalEmptyLines:= Op.OpSavingTrimFinalEmptyLines;
  if not (TATEditorModifiedOption.SavingForceFinalEol in Ed.ModifiedOptions) then
    Ed.OptSavingForceFinalEol:= Op.OpSavingForceFinalEol;

  Ed.OptShowScrollHint:= Op.OpShowHintOnVertScroll;
  Ed.OptScrollSmooth:= Op.OpSmoothScroll;
  Ed.OptScrollStyleHorz:= TATEditorScrollbarStyle(Op.OpScrollStyleHorz);
  Ed.OptNonWordChars:= Op.OpNonWordChars;

  if not Ed.ModeOneLine then
  begin
    Ed.OptFoldStyle:= TATEditorFoldStyle(Op.OpFoldStyle);
    Ed.OptFoldTooltipVisible:= Op.OpFoldTooltipShow;
    Ed.OptFoldIconForMinimalRangeHeight:= Op.OpFoldIconForMinimalRangeHeight;

    Ed.OptMarkersSize:= Op.OpMarkerSize;
    Ed.OptStapleStyle:= TATLineStyle(Op.OpStaplesStyle);
    Ed.OptStapleIndentConsidersEnd:= Op.OpStapleIndentConsidersEnd;

    Sep.Init(Op.OpStaplesProps);
    Sep.GetItemInt(N, 0);
    Ed.OptStapleIndent:= N;
    Sep.GetItemInt(N, 40);
    Ed.OptStapleWidthPercent:= N;
    Sep.GetItemInt(N, 1);
    Ed.OptStapleEdge1:= TATEditorStapleEdge(N);
    Sep.GetItemInt(N, 1);
    Ed.OptStapleEdge2:= TATEditorStapleEdge(N);

    Ed.OptAutoIndent:= Op.OpIndentAuto;
    if (Op.OpIndentAutoKind>=0) and (Op.OpIndentAutoKind<=Ord(High(TATEditorAutoIndentKind))) then
      Ed.OptAutoIndentKind:= TATEditorAutoIndentKind(Op.OpIndentAutoKind);
    Ed.OptAutoIndentBetterBracketsCurly:= Op.OpIndentAuto; //no separate option
    Ed.OptAutoIndentRegexRule:= Op.OpIndentAutoRule;

    Ed.OptZebraActive:= Op.OpZebra>0;
    if Ed.OptZebraActive then
      Ed.OptZebraAlphaBlend:= Op.OpZebra;
    Ed.OptZebraStep:= Op.OpZebraStep;
    Ed.OptDimUnfocusedBack:= Op.OpDimUnfocused;

    Ed.OptIndentSize:= Op.OpIndentSize;
    Ed.OptIndentKeepsAlign:= Op.OpUnIndentKeepsAlign;
    Ed.OptIndentMakesWholeLinesSelection:= Op.OpIndentMakesWholeLineSel;
  end;

  //change Ctrl+click to 'goto definition' and Ctrl+Wheel to 'add caret'
  InitEditorMouseActions(MouseActions, Op.OpMouseGotoDefinition='c');
  Ed.MouseActions:= MouseActions;

  Ed.OptMouse2ClickDragSelectsWords:= Op.OpMouse2ClickDragSelectsWords;
  Ed.OptMouseDragDrop:= Op.OpMouseDragDrop;
  Ed.OptMouseMiddleClickAction:= TATEditorMiddleClickAction(Op.OpMouseMiddleClickAction);
  Ed.OptMouseRightClickMovesCaret:= Op.OpMouseRightClickMovesCaret;
  Ed.OptMouseEnableColumnSelection:= Op.OpMouseEnableColumnSelection;
  Ed.OptMouseHideCursorOnType:= Op.OpMouseHideCursorOnType;
  Ed.OptMouseClickNumberSelectsLine:= Op.OpMouseGutterClickSelectedLine;
  Ed.OptMouseWheelZooms:= Op.OpMouseWheelZoom;
  Ed.OptMouseWheelScrollVertSpeed:= Op.OpMouseWheelSpeedVert;
  Ed.OptMouseWheelScrollHorzSpeed:= Op.OpMouseWheelSpeedHorz;
  Ed.OptMouseClickNumberSelectsLineWithEOL:= Op.OpMouseClickNumberSelectsEol;

  Ed.OptMouseClickOpensURL:= Op.OpMouseClickLinks=1;
  Ed.OptMouse2ClickOpensURL:= Op.OpMouseClickLinks=2;

  Ed.OptKeyBackspaceUnindent:= Op.OpKeyBackspaceUnindent;
  Ed.OptKeyBackspaceGoesToPrevLine:= Op.OpKeyBackspaceWrap;
  Ed.OptKeyTabIndents:= Op.OpKeyTabIndents;
  Ed.OptKeyHomeToNonSpace:= Op.OpKeyHomeToNonSpace;
  Ed.OptKeyHomeEndNavigateWrapped:= Op.OpKeyHomeEndNavigateWrapped;
  Ed.OptKeyEndToNonSpace:= Op.OpKeyEndToNonSpace;
  Ed.OptKeyPageKeepsRelativePos:= Op.OpKeyPageKeepsRelativePos;
  if (Op.OpKeyPageUpDownSize>=0) and (Op.OpKeyPageUpDownSize<=Ord(High(TATEditorPageDownSize))) then
    Ed.OptKeyPageUpDownSize:= TATEditorPageDownSize(Op.OpKeyPageUpDownSize);
  Ed.OptKeyUpDownKeepColumn:= Op.OpKeyUpDownKeepColumn;
  Ed.OptKeyUpDownNavigateWrapped:= Op.OpKeyUpDownNavigateWrapped;
  Ed.OptKeyUpDownAllowToEdge:= Op.OpKeyUpDownAllowToEdge;
  Ed.OptKeyLeftRightGoToNextLineWithCarets:= Op.OpKeyLeftRightGoToNextLineWithCarets;
  Ed.OptKeyLeftRightSwapSel:= Op.OpKeyLeftRightSwapSel;
  Ed.OptKeyLeftRightSwapSelAndSelect:= Op.OpKeyLeftRightSwapSelAndSelect;
end;

procedure EditorApplyOpsCommon(Ed: TATSynEdit);
begin
  Ed.OptBorderFocusedActive:= EditorOps.OpActiveBorderInControls;
  Ed.OptBorderWidthFocused:= ATEditorScale(EditorOps.OpActiveBorderWidth);
  Ed.OptCaretBlinkEnabled:= EditorOps.OpCaretBlinkEn;
  Ed.OptCaretBlinkTime:= EditorOps.OpCaretBlinkTime;
  Ed.OptScrollbarsNew:= EditorOps.OpScrollbarsNew;
  Ed.DoubleBuffered:= UiOps.DoubleBuffered;
  Ed.Font.Size:= EditorOps.OpFontSize;
end;

function EditorGetSelLinesCount(ed: TATSynEdit): integer;
var
  n1, n2, i: integer;
begin
  result:= 0;

  if not ed.IsSelRectEmpty then
  begin
    result:= ed.SelRect.Bottom-ed.SelRect.Top+1;
    exit
  end;

  for i:= 0 to ed.carets.count-1 do
  begin
    ed.carets[i].GetSelLines(n1, n2);
    if n1<0 then Continue;
    inc(result, n2-n1+1);
  end;
end;

function EditorGetSelCharsCount(ed: TATSynEdit): integer;
var
  Caret: TATCaretItem;
  x1, x2, y1, y2, i: integer;
  bSel: boolean;
begin
  result:= 0;

  if not ed.IsSelRectEmpty then
  begin
    x1:= ed.SelRect.Left;
    x2:= ed.SelRect.Right;
    for i:= ed.SelRect.Top to ed.SelRect.Bottom do
      Inc(result, ed.Strings.TextSubstringLength(x1, i, x2, i));
    exit;
  end;

  for i:= 0 to ed.Carets.Count-1 do
  begin
    Caret:= ed.Carets[i];
    Caret.GetRange(x1, y1, x2, y2, bSel);
    if bSel then
      Inc(result, ed.Strings.TextSubstringLength(x1, y1, x2, y2));
  end;
end;

function EditorFormatStatus(ed: TATSynEdit; const MacroText: string): string;
var
  St: TATStrings;
  Caret: TATCaretItem;
  nColumns, xBegin, yBegin, xEnd, yEnd: integer;
  bSel: boolean;
  nCodepoint: cardinal;
  nCodepointLen: integer;
  nOffsetMax, nOffsetCaret: integer;
  ch, ch2: WideChar;
  s, sOneChar: string;
begin
  result:= '';
  St:= ed.Strings;
  if ed.Carets.Count=0 then exit;
  Caret:= ed.Carets[0];

  Caret.GetRange(xBegin, yBegin, xEnd, yEnd, bSel);

  //make {cols} work for column-selection and small-selection
  nColumns:= 0;
  //column-selection?
  if not ed.IsSelRectEmpty then
    nColumns:= ed.SelRect.Right-ed.SelRect.Left
  else
  //small-selection?
  if (ed.Carets.Count=1) and (Caret.PosY=Caret.EndY) then
    nColumns:= Abs(Caret.PosX-Caret.EndX);

  result:= MacroText;
  result:= StringReplace(result, '{x}', IntToStr(Caret.PosX+1), []);
  result:= StringReplace(result, '{y}', IntToStr(Caret.PosY+1), []);
  result:= StringReplace(result, '{y2}', IntToStr(ed.Carets[ed.Carets.Count-1].PosY+1), []);
  result:= StringReplace(result, '{yb}', IntToStr(yBegin+1), []);
  result:= StringReplace(result, '{ye}', IntToStr(yEnd+1), []);
  result:= StringReplace(result, '{count}', IntToStr(St.Count), []);
  result:= StringReplace(result, '{carets}', IntToStr(ed.Carets.Count), []);
  result:= StringReplace(result, '{cols}', IntToStr(nColumns), []);

  result:= StringReplace(result, '{_ln}', msgStatusbarTextLine, []);
  result:= StringReplace(result, '{_col}', msgStatusbarTextCol, []);
  result:= StringReplace(result, '{_sel}', msgStatusbarTextSel, []);
  result:= StringReplace(result, '{_linesel}', msgStatusbarTextLinesSel, []);
  result:= StringReplace(result, '{_carets}', msgStatusbarTextCarets, []);

  if Pos('{sel}', result)>0 then
    result:= StringReplace(result, '{sel}', IntToStr(EditorGetSelLinesCount(ed)), []);

  if Pos('{selchars}', result)>0 then
    result:= StringReplace(result, '{selchars}', IntToStr(EditorGetSelCharsCount(ed)), []);

  if Pos('{xx}', result)>0 then
    if St.IsIndexValid(Caret.PosY) then
    begin
      //optimized for huge lines
      nColumns:= St.CharPosToColumnPos(Caret.PosY, Caret.PosX, ed.TabHelper)+1;
      result:= StringReplace(result, '{xx}', IntToStr(nColumns), []);
    end;

  if Pos('{offset_', result)>0 then
  begin
    EditorCalcOffsetsForStatusbar(ed, nOffsetMax, nOffsetCaret);
    if nOffsetMax>=0 then
      s:= IntToStr(nOffsetMax)
    else
      s:= '?';
    result:= StringReplace(result, '{offset_max}', s, []);
    if nOffsetCaret>=0 then
      s:= IntToStr(nOffsetCaret)
    else
      s:= '?';
    result:= StringReplace(result, '{offset_caret}', s, []);
  end;

  if Pos('{char', result)>0 then
  begin
    sOneChar:= '';
    if St.IsIndexValid(yBegin) then
      if (xBegin>=0) and (xBegin<St.LinesLen[yBegin]) then
      begin
        ch:= St.LineCharAt(yBegin, xBegin+1);
        if IsCharSurrogateHigh(ch) then
          ch2:= St.LineCharAt(yBegin, xBegin+2)
        else
          ch2:= #0;
        if ch<>#0 then
        begin
          if ch2=#0 then
            sOneChar:= UTF8Encode(UnicodeString(ch))
          else
            sOneChar:= UTF8Encode(UnicodeString(ch)+UnicodeString(ch2));
        end;
      end;

    result:= StringReplace(result, '{char}', sOneChar, []);

    if Pos('{char_', result)>0 then
    begin
      if sOneChar<>'' then
        nCodepoint:= UTF8CodepointToUnicode(PChar(sOneChar), nCodepointLen)
      else
        nCodepoint:= 0;

      if nCodepoint>0 then
        s:= IntToStr(nCodepoint)
      else
        s:= '';
      result:= StringReplace(result, '{char_dec}', s, []);

      if nCodepoint>0 then
        s:= IntToHex(nCodepoint, 2)
      else
        s:= '';
      result:= StringReplace(result, '{char_hex}', s, []);

      if nCodepoint>0 then
        s:= IntToHex(nCodepoint, 4)
      else
        s:= '';
      result:= StringReplace(result, '{char_hex4}', s, []);
    end;
  end;
end;

procedure EditorDeleteNewColorAttribs(ed: TATSynEdit);
begin
  ed.Attribs.Clear;
  ed.Update;
end;

function EditorGetSelectionKind(Ed: TATSynEdit): TEditorSelectionKind;
var
  NFrom, NTo: integer;
begin
  if not Ed.IsSelRectEmpty then
    Result:= selkindColumnSel
  else
  if Ed.Carets.Count>1 then
    Result:= selkindCarets
  else
  if Ed.Carets.IsSelection then
  begin
    Ed.Carets[0].GetSelLines(NFrom, NTo);
    if NTo>NFrom then
      Result:= selkindStreamSel
    else
      Result:= selkindSmallSel;
  end
  else
    Result:= selkindNone;
end;


procedure EditorApplyTheme(Ed: TATSynedit);
begin
  Ed.Colors.TextFont:= GetAppColor(apclEdTextFont);
  Ed.Colors.TextBG:= GetAppColor(apclEdTextBg);
  Ed.Colors.TextSelFont:= GetAppColor(apclEdSelFont);
  Ed.Colors.TextSelBG:= GetAppColor(apclEdSelBg);

  Ed.Colors.TextDisabledFont:= GetAppColor(apclEdDisableFont);
  Ed.Colors.TextDisabledBG:= GetAppColor(apclEdDisableBg);
  Ed.Colors.Caret:= GetAppColor(apclEdCaret);
  Ed.Colors.Markers:= GetAppColor(apclEdMarkers);
  Ed.Colors.DragDropMarker:= Ed.Colors.Markers;
  Ed.Colors.GitMarkerBG:= Ed.Colors.Markers;
  Ed.Colors.CurrentLineBG:= GetAppColor(apclEdCurLineBg);
  Ed.Colors.IndentVertLines:= GetAppColor(apclEdIndentVLine);
  Ed.Colors.UnprintedFont:= GetAppColor(apclEdUnprintFont);
  Ed.Colors.UnprintedBG:= GetAppColor(apclEdUnprintBg);
  Ed.Colors.UnprintedHexFont:= GetAppColor(apclEdUnprintHexFont);
  Ed.Colors.MinimapBorder:= GetAppColor(apclEdMinimapBorder);
  Ed.Colors.MinimapTooltipBG:= GetAppColor(apclEdMinimapTooltipBg);
  Ed.Colors.MinimapTooltipBorder:= GetAppColor(apclEdMinimapTooltipBorder);
  Ed.Colors.StateChanged:= GetAppColor(apclEdStateChanged);
  Ed.Colors.StateAdded:= GetAppColor(apclEdStateAdded);
  Ed.Colors.StateSaved:= GetAppColor(apclEdStateSaved);
  Ed.Colors.BlockStaple:= GetAppColor(apclEdBlockStaple);
  Ed.Colors.BlockStapleForCaret:= GetAppColor(apclEdBlockStapleActive);
  Ed.Colors.BlockSepLine:= GetAppColor(apclEdBlockSepLine);
  Ed.Colors.Links:= GetAppColor(apclEdLinks);
  Ed.Colors.LockedBG:= GetAppColor(apclEdLockedBg);
  Ed.Colors.ComboboxArrow:= GetAppColor(apclEdComboArrow);
  Ed.Colors.ComboboxArrowBG:= GetAppColor(apclEdComboArrowBg);
  Ed.Colors.CollapseLine:= GetAppColor(apclEdFoldMarkLine);
  Ed.Colors.CollapseMarkFont:= GetAppColor(apclEdFoldMarkFont);
  Ed.Colors.CollapseMarkBorder:= GetAppColor(apclEdFoldMarkBorder);
  Ed.Colors.CollapseMarkBG:= GetAppColor(apclEdFoldMarkBg);

  Ed.Colors.GutterFont:= GetAppColor(apclEdGutterFont);
  Ed.Colors.GutterBG:= GetAppColor(apclEdGutterBg);
  Ed.Colors.GutterCaretFont:= GetAppColor(apclEdGutterCaretFont);
  Ed.Colors.GutterCaretBG:= GetAppColor(apclEdGutterCaretBg);

  Ed.Colors.BookmarkBG:= GetAppColor(apclEdBookmarkBg);
  Ed.Colors.RulerFont:= GetAppColor(apclEdRulerFont);
  Ed.Colors.RulerBG:= GetAppColor(apclEdRulerBg);

  Ed.Colors.GutterFoldLine:= GetAppColor(apclEdFoldLine);
  Ed.Colors.GutterFoldLine2:= GetAppColor(apclEdFoldLine2);
  Ed.Colors.GutterFoldBG:= GetAppColor(apclEdFoldBg);

  Ed.Colors.MarginRight:= GetAppColor(apclEdMarginFixed);
  Ed.Colors.MarginCaret:= GetAppColor(apclEdMarginCaret);
  Ed.Colors.MarginUser:= GetAppColor(apclEdMarginUser);

  Ed.Colors.MarkedLinesBG:= GetAppColor(apclEdMarkedRangeBg);
  Ed.Colors.BorderLine:= GetAppColor(apclEdBorder);
  Ed.Colors.BorderLineFocused:= GetAppColor(apclEdBorderFocused);

  Ed.Update;
end;


procedure EditorSetColorById(Ed: TATSynEdit; const Id: string; AColor: TColor);
begin
  case Id of
    'EdTextFont'            : Ed.Colors.TextFont:= AColor;
    'EdTextBg'              : Ed.Colors.TextBG:= AColor;
    'EdSelFont'             : Ed.Colors.TextSelFont:= AColor;
    'EdSelBg'               : Ed.Colors.TextSelBG:= AColor;
    'EdDisableFont'         : Ed.Colors.TextDisabledFont:= AColor;
    'EdDisableBg'           : Ed.Colors.TextDisabledBG:= AColor;
    'EdCaret'               : Ed.Colors.Caret:= AColor;
    'EdMarkers'             : Ed.Colors.Markers:= AColor;
    'EdCurLineBg'           : Ed.Colors.CurrentLineBG:= AColor;
    'EdIndentVLine'         : Ed.Colors.IndentVertLines:= AColor;
    'EdUnprintFont'         : Ed.Colors.UnprintedFont:= AColor;
    'EdUnprintBg'           : Ed.Colors.UnprintedBG:= AColor;
    'EdUnprintHexFont'      : Ed.Colors.UnprintedHexFont:= AColor;
    'EdMinimapBorder'       : Ed.Colors.MinimapBorder:= AColor;
    'EdMinimapTooltipBg'    : Ed.Colors.MinimapTooltipBG:= AColor;
    'EdMinimapTooltipBorder': Ed.Colors.MinimapTooltipBorder:= AColor;
    'EdStateChanged'        : Ed.Colors.StateChanged:= AColor;
    'EdStateAdded'          : Ed.Colors.StateAdded:= AColor;
    'EdStateSaved'          : Ed.Colors.StateSaved:= AColor;
    'EdBlockStaple'         : Ed.Colors.BlockStaple:= AColor;
    'EdBlockStapleActive'   : Ed.Colors.BlockStapleForCaret:= AColor;
    'EdBlockSepLine'        : Ed.Colors.BlockSepLine:= AColor;
    'EdLinks'               : Ed.Colors.Links:= AColor;
    'EdLockedBg'            : Ed.Colors.LockedBG:= AColor;
    'EdComboArrow'          : Ed.Colors.ComboboxArrow:= AColor;
    'EdComboArrowBg'        : Ed.Colors.ComboboxArrowBG:= AColor;
    'EdFoldMarkLine'        : Ed.Colors.CollapseLine:= AColor;
    'EdFoldMarkFont'        : Ed.Colors.CollapseMarkFont:= AColor;
    'EdFoldMarkBorder'      : Ed.Colors.CollapseMarkBorder:= AColor;
    'EdFoldMarkBg'          : Ed.Colors.CollapseMarkBG:= AColor;
    'EdGutterFont'          : Ed.Colors.GutterFont:= AColor;
    'EdGutterBg'            : Ed.Colors.GutterBG:= AColor;
    'EdGutterCaretFont'     : Ed.Colors.GutterCaretFont:= AColor;
    'EdGutterCaretBg'       : Ed.Colors.GutterCaretBG:= AColor;
    'EdBookmarkBg'          : Ed.Colors.BookmarkBG:= AColor;
    'EdRulerFont'           : Ed.Colors.RulerFont:= AColor;
    'EdRulerBg'             : Ed.Colors.RulerBG:= AColor;
    'EdFoldLine'            : Ed.Colors.GutterFoldLine:= AColor;
    'EdFoldBg'              : Ed.Colors.GutterFoldBG:= AColor;
    'EdMarginFixed'         : Ed.Colors.MarginRight:= AColor;
    'EdMarginCaret'         : Ed.Colors.MarginCaret:= AColor;
    'EdMarginUser'          : Ed.Colors.MarginUser:= AColor;
    'EdMarkedRangeBg'       : Ed.Colors.MarkedLinesBG:= AColor;
    'EdBorder'              : Ed.Colors.BorderLine:= AColor;
    'EdBorderFocused'       : Ed.Colors.BorderLineFocused:= AColor;
  end;
end;


function EditorGetColorById(Ed: TATSynEdit; const Id: string): TColor;
begin
  Result:= -1;
  case Id of
    'EdTextFont'            : Result:= Ed.Colors.TextFont;
    'EdTextBg'              : Result:= Ed.Colors.TextBG;
    'EdSelFont'             : Result:= Ed.Colors.TextSelFont;
    'EdSelBg'               : Result:= Ed.Colors.TextSelBG;
    'EdDisableFont'         : Result:= Ed.Colors.TextDisabledFont;
    'EdDisableBg'           : Result:= Ed.Colors.TextDisabledBG;
    'EdCaret'               : Result:= Ed.Colors.Caret;
    'EdMarkers'             : Result:= Ed.Colors.Markers;
    'EdCurLineBg'           : Result:= Ed.Colors.CurrentLineBG;
    'EdIndentVLine'         : Result:= Ed.Colors.IndentVertLines;
    'EdUnprintFont'         : Result:= Ed.Colors.UnprintedFont;
    'EdUnprintBg'           : Result:= Ed.Colors.UnprintedBG;
    'EdUnprintHexFont'      : Result:= Ed.Colors.UnprintedHexFont;
    'EdMinimapBorder'       : Result:= Ed.Colors.MinimapBorder;
    'EdMinimapTooltipBg'    : Result:= Ed.Colors.MinimapTooltipBG;
    'EdMinimapTooltipBorder': Result:= Ed.Colors.MinimapTooltipBorder;
    'EdStateChanged'        : Result:= Ed.Colors.StateChanged;
    'EdStateAdded'          : Result:= Ed.Colors.StateAdded;
    'EdStateSaved'          : Result:= Ed.Colors.StateSaved;
    'EdBlockStaple'         : Result:= Ed.Colors.BlockStaple;
    'EdBlockStapleActive'   : Result:= Ed.Colors.BlockStapleForCaret;
    'EdBlockSepLine'        : Result:= Ed.Colors.BlockSepLine;
    'EdLinks'               : Result:= Ed.Colors.Links;
    'EdLockedBg'            : Result:= Ed.Colors.LockedBG;
    'EdComboArrow'          : Result:= Ed.Colors.ComboboxArrow;
    'EdComboArrowBg'        : Result:= Ed.Colors.ComboboxArrowBG;
    'EdFoldMarkLine'        : Result:= Ed.Colors.CollapseLine;
    'EdFoldMarkFont'        : Result:= Ed.Colors.CollapseMarkFont;
    'EdFoldMarkBorder'      : Result:= Ed.Colors.CollapseMarkBorder;
    'EdFoldMarkBg'          : Result:= Ed.Colors.CollapseMarkBG;
    'EdGutterFont'          : Result:= Ed.Colors.GutterFont;
    'EdGutterBg'            : Result:= Ed.Colors.GutterBG;
    'EdGutterCaretFont'     : Result:= Ed.Colors.GutterCaretFont;
    'EdGutterCaretBg'       : Result:= Ed.Colors.GutterCaretBG;
    'EdBookmarkBg'          : Result:= Ed.Colors.BookmarkBG;
    'EdRulerFont'           : Result:= Ed.Colors.RulerFont;
    'EdRulerBg'             : Result:= Ed.Colors.RulerBG;
    'EdFoldLine'            : Result:= Ed.Colors.GutterFoldLine;
    'EdFoldBg'              : Result:= Ed.Colors.GutterFoldBG;
    'EdMarginFixed'         : Result:= Ed.Colors.MarginRight;
    'EdMarginCaret'         : Result:= Ed.Colors.MarginCaret;
    'EdMarginUser'          : Result:= Ed.Colors.MarginUser;
    'EdMarkedRangeBg'       : Result:= Ed.Colors.MarkedLinesBG;
    'EdBorder'              : Result:= Ed.Colors.BorderLine;
    'EdBorderFocused'       : Result:= Ed.Colors.BorderLineFocused;
  end;
end;

procedure EditorClear(Ed: TATSynEdit);
begin
  Ed.Strings.Clear;
  Ed.Strings.ActionAddFakeLineIfNeeded;
  Ed.DoCaretSingle(0, 0);
  Ed.Update(true);
  Ed.Modified:= false;
end;

function EditorGetCurrentChar(Ed: TATSynEdit): Widechar;
var
  Caret: TATCaretItem;
begin
  Result:= #0;
  if Ed.Carets.Count<>1 then exit;
  Caret:= Ed.Carets[0];
  if not Ed.Strings.IsIndexValid(Caret.PosY) then exit;
  if (Caret.PosX<0) then exit;
  Result:= Ed.Strings.LineCharAt(Caret.PosY, Caret.PosX+1);
end;


procedure EditorMouseClick_AtCursor(Ed: TATSynEdit; AAndSelect: boolean);
var
  PntText: TPoint;
  PntCoord: TATPoint;
  P0: TPoint;
  Details: TATEditorPosDetails;
  Caret: TATCaretItem;
begin
  if Ed.Carets.Count=0 then exit;
  Caret:= Ed.Carets[0];

  P0:= Ed.ScreenToClient(Mouse.CursorPos);
  PntCoord:= ATPoint(P0.X, P0.Y);
  PntText:= Ed.ClientPosToCaretPos(PntCoord, Details);

  Ed.DoCaretSingle(
    PntText.X,
    PntText.Y,
    IfThen(AAndSelect, Caret.PosX, -1),
    IfThen(AAndSelect, Caret.PosY, -1)
    );
  Ed.Update;
end;

procedure EditorMouseClick_NearCaret(Ed: TATSynEdit; const AParams: string; AAndSelect: boolean);
var
  X, Y: integer;
  Caret: TATCaretItem;
  Sep: TATStringSeparator;
begin
  Sep.Init(AParams);
  Sep.GetItemInt(X, MaxInt);
  Sep.GetItemInt(Y, MaxInt);
  if X=MaxInt then exit;
  if Y=MaxInt then exit;

  if Ed.Carets.Count=0 then exit;
  Caret:= Ed.Carets[0];

  if Y=0 then
    Ed.DoCaretSingle(
      Caret.PosX+X,
      Caret.PosY,
      IfThen(AAndSelect, Caret.PosX, -1),
      IfThen(AAndSelect, Caret.PosY, -1)
      )
  else
    Ed.DoCaretSingle(
      X,
      Caret.PosY+Y,
      IfThen(AAndSelect, Caret.PosX, -1),
      IfThen(AAndSelect, Caret.PosY, -1)
      );

  Ed.Update;
end;

procedure EditorSetFont(F: TFont; const AParams: string);
var
  Sep: TATStringSeparator;
  S: string;
  N: integer;
begin
  Sep.Init(AParams);

  Sep.GetItemStr(S);
  F.Name:= S;

  Sep.GetItemInt(N, 0);
  if N>0 then
    F.Size:= N;
end;


function EditorGetLinkAtScreenCoord(Ed: TATSynEdit; P: TPoint): atString;
var
  PntCoord: TATPoint;
  Details: TATEditorPosDetails;
begin
  Result:= '';
  P:= Ed.ScreenToClient(P);
  PntCoord:= ATPoint(P.X, P.Y);
  P:= Ed.ClientPosToCaretPos(PntCoord, Details);
  Result:= Ed.DoGetLinkAtPos(P.X, P.Y);
  if SBeginsWith(Result, 'www') then
    Result:= 'https://'+Result;
end;

function EditorGetLinkAtCaret(Ed: TATSynEdit): atString;
begin
  Result:= '';
  if Ed.Carets.Count=0 then exit;
  Result:= Ed.DoGetLinkAtPos(Ed.Carets[0].PosX, Ed.Carets[0].PosY);
end;

function EditorIsAutocompleteCssPosition(Ed: TATSynEdit; AX, AY: integer): boolean;
//function finds 1st nonspace char before AX:AY and if it's ";" or "{" then it's OK position
  //
  function IsSepChar(ch: Widechar): boolean;
  begin
    Result:= (ch=';') or (ch='{');
  end;
  function IsSpaceChar(ch: Widechar): boolean;
  begin
    Result:= (ch=' ') or (ch=#9);
  end;
  //
var
  str: atString;
  ch: Widechar;
  i: integer;
begin
  Result:= false;
  if not Ed.Strings.IsIndexValid(AY) then exit;

  //find char in line AY before AX
  str:= Ed.Strings.Lines[AY];
  for i:= AX downto 1 do
  begin
    ch:= str[i];
    if IsSpaceChar(ch) then Continue;
    exit(IsSepChar(ch));
  end;

  //find char in line AY-1 from end
  if AY=0 then exit;
  str:= Ed.Strings.Lines[AY-1];
  for i:= Length(str) downto 1 do
  begin
    ch:= str[i];
    if IsSpaceChar(ch) then Continue;
    exit(IsSepChar(ch));
  end;
end;


function Editor_NextCharAllowed_AutoCloseBracket(Ed: TATSynEdit; ch: widechar): boolean;
var
  S: UnicodeString;
begin
  S:= Ed.OptNonWordChars+' '#9;
  S:= UnicodeStringReplace(S, '"', '', [rfReplaceAll]);
  S:= UnicodeStringReplace(S, '''', '', [rfReplaceAll]);
  S:= UnicodeStringReplace(S, '(', '', [rfReplaceAll]);
  S:= UnicodeStringReplace(S, '[', '', [rfReplaceAll]);
  S:= UnicodeStringReplace(S, '{', '', [rfReplaceAll]);
  Result:= Pos(ch, S)>0;
end;


function EditorAutoSkipClosingBracket(Ed: TATSynEdit; CharClosing: char): boolean;
var
  Caret: TATCaretItem;
  CharOpening: char;
  St: TATStrings;
  Str: UnicodeString;
  iCaret: integer;
begin
  Result:= false;
  St:= Ed.Strings;

  CharOpening:= EditorBracket_GetPairForClosingBracketOrQuote(CharClosing);
  if CharOpening=#0 then exit;
  if Pos(CharOpening, Ed.OptAutoPairChars)=0 then exit;

  for iCaret:= Ed.Carets.Count-1 downto 0 do
  begin
    Caret:= Ed.Carets[iCaret];
    //improve auto-closing brackets, avoid duplicate )]}
    //when closing bracket )]} is typed over itself,
    //and previous char is opening bracket ([{
    if St.IsIndexValid(Caret.PosY) then
    begin
      Str:= St.Lines[Caret.PosY];
      if (Caret.PosX<Length(Str)) then
       if Str[Caret.PosX+1]=CharClosing then
        //if (Caret.PosX>0) and (Str[Caret.PosX]=CharOpening) then //only if previous is ([{ ? no, always, like Sublime Text
        begin
          Caret.Change(Caret.PosX+1, Caret.PosY, -1, -1);
          Result:= true;
        end;
    end;
  end;
end;


function EditorAutoDeleteClosingBracket(Ed: TATSynEdit): boolean;
var
  St: TATStrings;
  Str: UnicodeString;
  Caret: TATCaretItem;
  Props: array of record NLine, XOpen, XClose: integer; end;
  NLine, XOpen, XClose: integer;
  CharPrev, CharNext: WideChar;
  iCaret, X: integer;
begin
  Result:= false;
  St:= Ed.Strings;

  if Ed.Carets.IsSelection then exit;

  Props:= nil;
  SetLength(Props, Ed.Carets.Count);

  //check each caret has the situation "(|)" or "(|  )"
  for iCaret:= Ed.Carets.Count-1 downto 0 do
  begin
    Props[iCaret].NLine:= -1;
    Props[iCaret].XOpen:= -1;
    Props[iCaret].XClose:= -1;

    Caret:= Ed.Carets[iCaret];
    if St.IsIndexValid(Caret.PosY) then
    begin
      Str:= St.Lines[Caret.PosY];
      if (Caret.PosX>0) and (Caret.PosX<Length(Str)) then
      begin
        CharPrev:= Str[Caret.PosX];
        if Pos(CharPrev, Ed.OptAutoPairChars)=0 then
          Break;

        CharNext:= EditorBracket_GetPairForOpeningBracketOrQuote(CharPrev);
        if CharNext=#0 then
          Break;

        X:= Caret.PosX+1;
        while (X<=Length(Str)) and IsCharSpace(Str[X]) do
          Inc(X);
        if (X<=Length(Str)) and (Str[X]=CharNext) then
        begin
          Props[iCaret].NLine:= Caret.PosY;
          Props[iCaret].XOpen:= Caret.PosX;
          Props[iCaret].XClose:= X;
        end;
      end;
    end;
  end;

  for iCaret:= Ed.Carets.Count-1 downto 0 do
  begin
    if Props[iCaret].NLine<0 then exit;
  end;

  //all carets have 'good situation', delete pair brackets
  for iCaret:= Ed.Carets.Count-1 downto 0 do
  begin
    NLine:= Props[iCaret].NLine;
    XOpen:= Props[iCaret].XOpen;
    XClose:= Props[iCaret].XClose;

    Str:= St.Lines[NLine];
    Delete(Str, XClose, 1);
    Delete(Str, XOpen, 1);
    St.Lines[NLine]:= Str;

    Caret:= Ed.Carets[iCaret];
    Caret.Change(XOpen-1, NLine, -1, -1);

    Ed.UpdateCaretsAndMarkersOnEditing(
      iCaret+1,
      Point(XOpen, NLine),
      Point(-1, -1),
      Point(-2, 0),
      Point(XOpen-1, NLine)
      );
  end;

  Ed.Update;
  Result:= true;
end;


function EditorAutoCloseBracket_NeedPair(Ed: TATSynEdit; Caret: TATCaretItem; AQuoteChar: boolean): boolean;
var
  NPos: integer;
  Str: atString;
begin
  Result:= true;
  NPos:= Caret.PosX;
  Str:= Ed.Strings.Lines[Caret.PosY];

  if (NPos>=1) and (NPos<=Length(Str)) then
  begin
    //bad context: caret after a backslash
    if (Str[NPos]='\') then
      exit(false);

    //bad context: quote-char is typed after a word-char. issue #3331
    if AQuoteChar and IsCharWord(Str[NPos], Ed.OptNonWordChars) then
      exit(false);
  end;

  //bad context: caret is before a not-allowed symbol char
  if (NPos<Length(Str)) and
    not Editor_NextCharAllowed_AutoCloseBracket(Ed, Str[NPos+1]) then
      exit(false);
end;

function EditorAutoPairChar(Ed: TATSynEdit; CharBegin: atChar): boolean;
var
  Caret: TATCaretItem;
  St: TATStrings;
  X1, Y1, X2, Y2: integer;
  NCaret, NLineChanged: integer;
  bSel, bBackwardSel: boolean;
  bQuoteChar: boolean;
  CharEnd: atChar;
  Shift, PosAfter: TPoint;
begin
  Result:= false;

  //makes no sense to auto-close brackets in overwrite mode
  if Ed.ModeOverwrite then exit;

  CharEnd:= EditorBracket_GetPairForOpeningBracketOrQuote(CharBegin);
  if CharEnd=#0 then exit;

  case CharBegin of
    '"', '''', '`':
      bQuoteChar:= true;
    else
      bQuoteChar:= false;
  end;

  St:= Ed.Strings;
  NLineChanged:= St.Count-1;

  //make additional loop, to detect that ALL carets need pairing.
  //if at least one caret doesn't need, stop.
  //it's needed to make pair for all or nothing. issue #3219.
  for NCaret:= Ed.Carets.Count-1 downto 0 do
  begin
    Caret:= Ed.Carets[NCaret];
    if not St.IsIndexValid(Caret.PosY) then Continue;
    if Caret.EndY<0 then
      if not EditorAutoCloseBracket_NeedPair(Ed, Caret, bQuoteChar) then
        exit;
  end;

  //cancel vertical selection
  Ed.DoSelect_ClearColumnBlock;

  St.BeginUndoGroup;
  for NCaret:= Ed.Carets.Count-1 downto 0 do
  begin
    Caret:= Ed.Carets[NCaret];
    if not St.IsIndexValid(Caret.PosY) then Continue;
    Caret.GetRange(X1, Y1, X2, Y2, bSel);
    bBackwardSel:= not Caret.IsForwardSelection;
    NLineChanged:= Min(NLineChanged, Y1);

    if not bSel then
      if not EditorAutoCloseBracket_NeedPair(Ed, Caret, bQuoteChar) then Continue;

    if not bSel then
    begin
      St.TextInsert(X1, Y1, CharBegin+CharEnd, false, Shift, PosAfter);
      Ed.UpdateCaretsAndMarkersOnEditing(NCaret+1,
        Point(X1, Y1),
        Point(X1, Y1),
        Shift,
        PosAfter);

      Caret.PosX:= Caret.PosX+1;
      Caret.EndX:= -1;
      Caret.EndY:= -1;
    end
    else
    begin
      St.TextInsert(X2, Y2, CharEnd, false, Shift, PosAfter);
      Ed.UpdateCaretsAndMarkersOnEditing(NCaret+1,
        Point(X2, Y2),
        Point(X2, Y2),
        Shift,
        PosAfter);

      St.TextInsert(X1, Y1, CharBegin, false, Shift, PosAfter);
      Ed.UpdateCaretsAndMarkersOnEditing(NCaret+1,
        Point(X1, Y1),
        Point(X1, Y1),
        Shift,
        PosAfter);

      Caret.EndX:= X1+1;
      Caret.EndY:= Y1;
      Caret.PosX:= X2+IfThen(Y1=Y2, 1);
      Caret.PosY:= Y2;

      if bBackwardSel then
        Caret.SwapSelection;
    end;

    Result:= true;
  end;
  St.EndUndoGroup;

  if Result then
  begin
    Ed.Modified:= true;
    Ed.DoEventChange(NLineChanged);
    Ed.Update(true);
  end;
end;


procedure EditorFocus(C: TWinControl);
var
  Form: TCustomForm;
begin
  try
    Form:= GetParentForm(C);
    if Form=nil then exit;
    if not Form.Focused then
      if Form.CanFocus then
        Form.SetFocus;

    if Form.Visible and Form.Enabled then
    begin
      Form.ActiveControl:= C;
      if C.CanFocus then
        C.SetFocus;
    end;
  except
  end;
end;


procedure EditorGotoLastEditingPos(Ed: TATSynEdit;
  AIndentHorz, AIndentVert: integer);
var
  Caret: TATCaretItem;
begin
  Ed.Strings.ActionGotoLastEditionPos;
  if Ed.Carets.Count>0 then
  begin
    Caret:= Ed.Carets[0];
    Ed.DoGotoPos(
      Point(Caret.PosX, Caret.PosY),
      Point(-1, -1),
      AIndentHorz,
      AIndentVert,
      true,
      true
      );
  end;
end;


function EditorGotoFromString(Ed: TATSynEdit; SInput: string): boolean;
var
  NumCount, NumLine, NumCol, NumOffset: integer;
  Pnt: TPoint;
  bExtend: boolean;
  Caret: TATCaretItem;
  Sep: TATStringSeparator;
begin
  NumCount:= Ed.Strings.Count;
  if NumCount=0 then exit(false);

  bExtend:= SEndsWith(SInput, '+');
  if bExtend then
    SetLength(SInput, Length(SInput)-1)
  else
  begin
    bExtend:= SBeginsWith(SInput, '+');
    if bExtend then
      Delete(SInput, 1, 1);
  end;

  if SEndsWith(SInput, '%') then
  begin
    NumLine:= StrToIntDef(Copy(SInput, 1, Length(SInput)-1), 0);
    if NumLine<0 then
      NumLine:= NumCount-1 + (NumCount * NumLine div 100)
    else
      NumLine:= NumCount * NumLine div 100;
    NumCol:= 0;
  end
  else
  if SBeginsWith(SInput, 'd') then
  begin
    NumOffset:= StrToIntDef(Copy(SInput, 2, MaxInt), -1);
    if NumOffset>=0 then
    begin
      Pnt:= Ed.OffsetToCaretPos(NumOffset);
      NumLine:= Pnt.Y;
      NumCol:= Pnt.X;
    end
    else
    begin
      NumLine:= -1;
      NumCol:= -1;
    end;
  end
  else
  if SBeginsWith(SInput, 'x') then
  begin
    NumOffset:= StrToIntDef('$'+Copy(SInput, 2, MaxInt), -1);
    if NumOffset>=0 then
    begin
      Pnt:= Ed.OffsetToCaretPos(NumOffset);
      NumLine:= Pnt.Y;
      NumCol:= Pnt.X;
    end
    else
    begin
      NumLine:= -1;
      NumCol:= -1;
    end;
  end
  else
  begin
    Sep.Init(SInput, ':');
    Sep.GetItemInt(NumLine, 0);
    Sep.GetItemInt(NumCol, 0);
    if NumLine<0 then
      NumLine:= NumCount+NumLine
    else
      Dec(NumLine);
    Dec(NumCol);
  end;

  Result:= NumLine>=0;
  if not Result then exit;

  NumLine:= Min(NumLine, NumCount-1);
  NumCol:= Max(0, NumCol);

  Pnt:= Point(-1, -1);
  if bExtend then
  begin
    if Ed.Carets.Count=0 then exit;
    Caret:= Ed.Carets[0];
    //set end of selection to previous caret pos
    Pnt:= Point(Caret.PosX, Caret.PosY);
    //make it like SynWrite: jump extends previous selection (below and above)
    if Caret.EndY>=0 then
      if IsPosSorted(Caret.PosX, Caret.PosY, NumCol, NumLine, true) then
      begin
        //jump below
        if Caret.IsForwardSelection then
          Pnt:= Point(Caret.EndX, Caret.EndY);
      end
      else
      begin
        //jump above
        if not Caret.IsForwardSelection then
          Pnt:= Point(Caret.EndX, Caret.EndY);
      end;
  end;

  Ed.DoGotoPos(
    Point(NumCol, NumLine),
    Pnt,
    UiOps.FindIndentHorz,
    UiOps.FindIndentVert,
    true,
    true
    );
  Ed.Update;
end;


procedure EditorCaretShapeFromString(Props: TATCaretShape; const AText: string);
var
  Sep: TATStringSeparator;
  S: string;
begin
  Sep.Init(AText);
  Sep.GetItemInt(Props.Width, 1);
  Sep.GetItemInt(Props.Height, -100);
  Sep.GetItemStr(S);
  Props.EmptyInside:= Pos('_', S)>0;
end;


procedure EditorCaretShapeFromPyTuple(Props: TATCaretShape; const AText: string);
var
  Sep: TATStringSeparator;
  S: string;
begin
  Sep.Init(AText);
  Sep.GetItemInt(Props.Width, 1);
  Sep.GetItemInt(Props.Height, -100);
  Sep.GetItemStr(S);
  Props.EmptyInside:= S='1';
end;

function EditorBracket_GetPairForOpeningBracketOrQuote(ch: char): char;
begin
  case ch of
    '(': Result:= ')';
    '[': Result:= ']';
    '{': Result:= '}';
    '<': Result:= '>';
    '"': Result:= '"';
    '''': Result:= '''';
    '`': Result:= '`';
    '~': Result:= '~';
    '*': Result:= '*';
    '#': Result:= '#';
    else Result:= #0;
  end;
end;

function EditorBracket_GetPairForClosingBracketOrQuote(ch: char): char;
begin
  case ch of
    ')': Result:= '(';
    ']': Result:= '[';
    '}': Result:= '{';
    '"': Result:= '"';
    '''': Result:= '''';
    '`': Result:= '`';
    else Result:= #0;
  end;
end;

procedure EditorBracket_GetCharKind(ch: atChar; out Kind: TATEditorBracketKind; out PairChar: atChar);
begin
  case ch of
    '(': begin Kind:= bracketOpening; PairChar:= ')'; end;
    '[': begin Kind:= bracketOpening; PairChar:= ']'; end;
    '{': begin Kind:= bracketOpening; PairChar:= '}'; end;
    '<': begin Kind:= bracketOpening; PairChar:= '>'; end;
    ')': begin Kind:= bracketClosing; PairChar:= '('; end;
    ']': begin Kind:= bracketClosing; PairChar:= '['; end;
    '}': begin Kind:= bracketClosing; PairChar:= '{'; end;
    '>': begin Kind:= bracketClosing; PairChar:= '<'; end;
    else begin Kind:= bracketUnknown; PairChar:= #0; end;
  end;
end;

procedure EditorBracket_FindOpeningBracketBackward(Ed: TATSynEdit;
  PosX, PosY: integer;
  const AllowedSymbols: string;
  MaxDistance: integer;
  out FoundX, FoundY: integer);
var
  Level: integer;
  Kind: TATEditorBracketKind;
  ch, ch2: atChar;
  iLine, iChar, nChar: integer;
  S: atString;
begin
  FoundX:= -1;
  FoundY:= -1;
  Level:= 0;

  for iLine:= PosY downto Max(0, PosY-MaxDistance) do
  begin
    if Ed.Strings.LinesLen[iLine]>EditorOps.OpMaxLineLenForBracketFinder then
      Continue;

    S:= Ed.Strings.Lines[iLine];
    if S='' then Continue;
    if iLine=PosY then
      nChar:= Min(PosX-1, Length(S)-1)
    else
      nChar:= Length(S)-1;
    for iChar:= nChar downto 0 do
    begin
      ch:= S[iChar+1];
      if Pos(ch, AllowedSymbols)=0 then Continue;
      EditorBracket_GetCharKind(ch, Kind, ch2);
      if Kind=bracketUnknown then Continue;

      //ignore brackets in comments/strings, because of constants '{', '(' etc
      if EditorGetTokenKind(Ed, iChar, iLine)<>TATTokenKind.Other then Continue;

      if Kind=bracketClosing then
      begin
        Dec(Level);
      end
      else
      if Kind=bracketOpening then
      begin
        Inc(Level);
        if Level>0 then
        begin
          FoundX:= iChar;
          FoundY:= iLine;
          exit;
        end;
      end;
    end;
  end;
end;

procedure EditorBracket_FindPair(
  Ed: TATSynEdit;
  CharFrom, CharTo: atChar;
  Kind: TATEditorBracketKind;
  MaxDistance: integer;
  FromX, FromY: integer;
  out FoundX, FoundY: integer);
var
  St: TATStrings;
  S: atString;
  IndexX, IndexY, IndexXBegin, IndexXEnd: integer;
  Level: integer;
  ch: atChar;
begin
  FoundX:= -1;
  FoundY:= -1;
  Level:= 0;
  St:= Ed.Strings;

  if Kind=bracketOpening then
  begin
    for IndexY:= FromY to Min(Int64(St.Count-1), Int64(FromY)+MaxDistance) do
    begin
      if St.LinesLen[IndexY]>EditorOps.OpMaxLineLenForBracketFinder then
        Continue;

      S:= St.Lines[IndexY];
      if S='' then Continue;
      if IndexY=FromY then
        IndexXBegin:= FromX+1
      else
        IndexXBegin:= 0;
      IndexXEnd:= Length(S)-1;
      for IndexX:= IndexXBegin to IndexXEnd do
      begin
        ch:= S[IndexX+1];
        if (ch=CharFrom) and (EditorGetTokenKind(Ed, IndexX, IndexY)=TATTokenKind.Other) then
          Inc(Level)
        else
        if (ch=CharTo) and (EditorGetTokenKind(Ed, IndexX, IndexY)=TATTokenKind.Other) then
        begin
          if Level>0 then
            Dec(Level)
          else
          begin
            FoundX:= IndexX;
            FoundY:= IndexY;
            Exit
          end;
        end;
      end;
    end;
  end
  else
  begin
    for IndexY:= FromY downto Max(0, Int64(FromY)-MaxDistance) do
    begin
      if St.LinesLen[IndexY]>EditorOps.OpMaxLineLenForBracketFinder then
        Continue;

      S:= St.Lines[IndexY];
      if S='' then Continue;
      if IndexY=FromY then
        IndexXEnd:= FromX-1
      else
        IndexXEnd:= Length(S)-1;
      IndexXBegin:= 0;
      for IndexX:= IndexXEnd downto IndexXBegin do
      begin
        ch:= S[IndexX+1];
        if (ch=CharFrom) and (EditorGetTokenKind(Ed, IndexX, IndexY)=TATTokenKind.Other) then
          Inc(Level)
        else
        if (ch=CharTo) and (EditorGetTokenKind(Ed, IndexX, IndexY)=TATTokenKind.Other) then
        begin
          if Level>0 then
            Dec(Level)
          else
          begin
            FoundX:= IndexX;
            FoundY:= IndexY;
            Exit
          end;
        end;
      end;
    end;
  end;
end;

function EditorBracket_ClearHilite(Ed: TATSynEdit): boolean;
var
  bChange1, bChange2: boolean;
begin
  bChange1:= Ed.Attribs.DeleteWithTag(cEditorTagForBracket);
  bChange2:= Ed.GutterDecor.DeleteByTag(cEditorTagForBracket);
  Result:= bChange1 or bChange2;
  if Result then
    Ed.Update;
end;

procedure EditorBracket_FindBoth(Ed: TATSynEdit;
  var PosX, PosY: integer;
  const AllowedSymbols: string;
  MaxDistance: integer;
  out FoundX, FoundY: integer;
  out CharFrom, CharTo: atChar;
  out Kind: TATEditorBracketKind);
var
  St: TATStrings;
  S: atString;
begin
  FoundX:= -1;
  FoundY:= -1;
  CharFrom:= #0;
  CharTo:= #0;
  Kind:= bracketUnknown;
  St:= Ed.Strings;

  if PosX<0 then exit;
  if not St.IsIndexValid(PosY) then exit;
  if St.LinesLen[PosY]>EditorOps.OpMaxLineLenForBracketFinder then exit;

  S:= St.Lines[PosY];
  if (PosX=Length(S)) and (PosX>0) then
    Dec(PosX);

  if PosX<Length(S) then
  begin
    CharFrom:= S[PosX+1];
    if Pos(CharFrom, AllowedSymbols)>0 then
      if EditorGetTokenKind(Ed, PosX, PosY)=TATTokenKind.Other then
        EditorBracket_GetCharKind(CharFrom, Kind, CharTo);
  end;

  if Kind=bracketUnknown then
  begin
    //test char before caret
    if (PosX>0) and (PosX<Length(S)) then
    begin
      Dec(PosX);
      CharFrom:= S[PosX+1];
      if Pos(CharFrom, AllowedSymbols)>0 then
      begin
        if EditorGetTokenKind(Ed, PosX, PosY)=TATTokenKind.Other then
          EditorBracket_GetCharKind(CharFrom, Kind, CharTo);
      end
      else
        Kind:= bracketUnknown;
    end;

    //find opening bracket backwards
    if Kind=bracketUnknown then
    begin
      EditorBracket_FindOpeningBracketBackward(Ed,
        PosX, PosY,
        AllowedSymbols,
        MaxDistance,
        FoundX, FoundY);
      if FoundY<0 then exit;
      PosX:= FoundX;
      PosY:= FoundY;
      S:= St.Lines[PosY];
      CharFrom:= S[PosX+1];
      EditorBracket_GetCharKind(CharFrom, Kind, CharTo);
    end;

    if Kind=bracketUnknown then exit;
  end;

  EditorBracket_FindPair(Ed, CharFrom, CharTo, Kind,
    MaxDistance, PosX, PosY, FoundX, FoundY);
end;


procedure EditorBracket_Action(Ed: TATSynEdit;
  Action: TATEditorBracketAction;
  const AllowedSymbols: string;
  MaxDistance: integer);
var
  Caret: TATCaretItem;
  CharFrom, CharTo: atChar;
  Kind: TATEditorBracketKind;
  LinePart: TATLinePart;
  Decor: TATGutterDecorData;
  PosX, PosY, FoundX, FoundY: integer;
  Pnt1, Pnt2: TPoint;
  StyleSymbol: TecSyntaxFormat;
begin
  EditorBracket_ClearHilite(Ed);

  if Ed.Carets.Count<>1 then exit;
  Caret:= Ed.Carets[0];
  PosX:= Caret.PosX;
  PosY:= Caret.PosY;

  //don't work if selection
  if Caret.EndY>=0 then exit;
  if not Ed.Strings.IsIndexValid(PosY) then exit;

  //don't work on huge lines
  if Ed.Strings.LinesLen[PosY]>EditorOps.OpMaxLineLenForBracketFinder then
    exit;

  EditorBracket_FindBoth(Ed,
    PosX, PosY,
    AllowedSymbols,
    MaxDistance,
    FoundX, FoundY,
    CharFrom, CharTo,
    Kind);
  if FoundY<0 then exit;

  case Action of
    bracketActionHilite:
      begin
        InitLinePart(LinePart);
        ApplyPartStyleFromEcontrolStyle(LinePart, GetAppStyle(apstBracketBG));
        Ed.Attribs.Add(
          Point(PosX, PosY),
          Point(1, 0),
          TATMarkerTags.Init(cEditorTagForBracket, 0),
          @LinePart
          );

        ApplyPartStyleFromEcontrolStyle(LinePart, GetAppStyle(apstBracketBG));
        Ed.Attribs.Add(
          Point(FoundX, FoundY),
          Point(1, 0),
          TATMarkerTags.Init(cEditorTagForBracket, 0),
          @LinePart
          );

        Decor:= Default(TATGutterDecorData);
        StyleSymbol:= GetAppStyle(apstSymbol);
        Decor.DeleteOnDelLine:= true;
        Decor.ImageIndex:= -1;
        Decor.Tag:= cEditorTagForBracket;
        Decor.TextBold:= fsBold in StyleSymbol.Font.Style;
        Decor.TextItalic:= fsItalic in StyleSymbol.Font.Style;
        Decor.TextColor:= StyleSymbol.Font.Color;

        if PosY<>FoundY then
        begin
          Decor.LineNum:= PosY;
          Decor.TextBuffer:= nil; //not important
          Decor.TextAll:= CharFrom;
          Ed.GutterDecor.Add(Decor);

          Decor.LineNum:= FoundY;
          Decor.TextBuffer:= nil; //important
          Decor.TextAll:= CharTo;
          Ed.GutterDecor.Add(Decor);
        end
        else
        begin
          Decor.LineNum:= PosY;
          Decor.TextBuffer:= nil;
          if Kind=bracketOpening then
            Decor.TextAll:= CharFrom+CharTo
          else
            Decor.TextAll:= CharTo+CharFrom;
          Ed.GutterDecor.Add(Decor);
        end;

        Ed.Update;
      end;

    bracketActionJump:
      begin
        Ed.DoGotoPos(
          Point(FoundX, FoundY),
          Point(-1, -1),
          UiOps.FindIndentHorz,
          UiOps.FindIndentVert,
          true,
          true
          );
      end;

    bracketActionSelect:
      begin
        if IsPosSorted(PosX, PosY, FoundX, FoundY, true) then
        begin
          Pnt1:= Point(FoundX+1, FoundY);
          Pnt2:= Point(PosX, PosY);
        end
        else
        begin
          Pnt1:= Point(FoundX, FoundY);
          Pnt2:= Point(PosX+1, PosY);
        end;
        if Pnt1<>Pnt2 then
          Ed.DoGotoPos(
            Pnt1,
            Pnt2,
            UiOps.FindIndentHorz,
            UiOps.FindIndentVert,
            true,
            true
            )
      end;

    bracketActionSelectInside:
      begin
        if IsPosSorted(PosX, PosY, FoundX, FoundY, true) then
        begin
          Pnt1:= Point(FoundX, FoundY);
          Pnt2:= Point(PosX+1, PosY);
        end
        else
        begin
          Pnt1:= Point(FoundX+1, FoundY);
          Pnt2:= Point(PosX, PosY);
        end;
        if Pnt1<>Pnt2 then
          Ed.DoGotoPos(
            Pnt1,
            Pnt2,
            UiOps.FindIndentHorz,
            UiOps.FindIndentVert,
            true,
            true
            )
      end;
  end;
end;

function EditorIsModifiedEx(Ed: TATSynEdit): boolean;
begin
  if (Ed.FileName='') and (not UiOps.ConfirmSaveEmptyUntitledTab) then
    Result:= Ed.Modified and not EditorIsEmpty(Ed)
  else
    Result:= Ed.Modified;
end;

procedure EditorSaveTempOptions(Ed: TATSynEdit; out Ops: TATEditorTempOptions);
begin
  Ops:= Default(TATEditorTempOptions);

  Ops.FontSize:= Ed.Font.Size;
  Ops.TabSize:= Ed.OptTabSize;
  Ops.TabSpaces:= Ed.OptTabSpaces;
  Ops.WrapMode:= Ed.OptWrapMode;
  Ops.ShowMinimap:= Ed.OptMinimapVisible;
  Ops.ShowMicromap:= Ed.OptMicromapVisible;
  Ops.ShowRuler:= Ed.OptRulerVisible;
  Ops.ShowNumbers:= Ed.Gutter.Items[Ed.Gutter.FindIndexByTag(ATEditorOptions.GutterTagNumbers)].Visible;
  Ops.ShowFolding:= Ed.Gutter.Items[Ed.Gutter.FindIndexByTag(ATEditorOptions.GutterTagFolding)].Visible;
  Ops.ShowUnprinted:= Ed.OptUnprintedVisible;

  Ops.UnprintedSpaces:= Ed.OptUnprintedSpaces;
  Ops.UnprintedSpacesTrail:= Ed.OptUnprintedSpacesTrailing;
  Ops.UnprintedSpacesInSel:= Ed.OptUnprintedSpacesOnlyInSelection;
  Ops.UnprintedEnds:= Ed.OptUnprintedEnds;
  Ops.UnprintedEndsDetails:= Ed.OptUnprintedEndsDetails;
  Ops.UnprintedForceTabs:= Ed.OptUnprintedForceTabs;
end;

procedure EditorRestoreTempOptions(Ed: TATSynEdit; const ANew, AOld: TATEditorTempOptions);
begin
  if AOld.FontSize<>ANew.FontSize then
    Ed.Font.Size:= ANew.FontSize;
  if AOld.TabSize<>ANew.TabSize then
    Ed.OptTabSize:= ANew.TabSize;
  if AOld.TabSpaces<>ANew.TabSpaces then
    Ed.OptTabSpaces:= ANew.TabSpaces;
  if AOld.WrapMode<>ANew.WrapMode then
    Ed.OptWrapMode:= ANew.WrapMode;
  if AOld.ShowMinimap<>ANew.ShowMinimap then
    Ed.OptMinimapVisible:= ANew.ShowMinimap;
  if AOld.ShowMicromap<>ANew.ShowMicromap then
    Ed.OptMicromapVisible:= ANew.ShowMicromap;
  if AOld.ShowRuler<>ANew.ShowRuler then
    Ed.OptRulerVisible:= ANew.ShowRuler;
  if AOld.ShowNumbers<>ANew.ShowNumbers then
    Ed.Gutter.Items[Ed.Gutter.FindIndexByTag(ATEditorOptions.GutterTagNumbers)].Visible:= ANew.ShowNumbers;
  if AOld.ShowFolding<>ANew.ShowFolding then
    Ed.Gutter.Items[Ed.Gutter.FindIndexByTag(ATEditorOptions.GutterTagFolding)].Visible:= ANew.ShowFolding;

  if AOld.ShowUnprinted<>ANew.ShowUnprinted then
    Ed.OptUnprintedVisible:= ANew.ShowUnprinted;

  //let's not compare AOld.*<>ANew.*, to fix issue #2908

  //if AOld.UnprintedSpaces<>ANew.UnprintedSpaces then
    Ed.OptUnprintedSpaces:= ANew.UnprintedSpaces;
  //if AOld.UnprintedSpacesTrail<>ANew.UnprintedSpacesTrail then
    Ed.OptUnprintedSpacesTrailing:= ANew.UnprintedSpacesTrail;
  //if AOld.UnprintedSpacesInSel<>ANew.UnprintedSpacesInSel then
    Ed.OptUnprintedSpacesOnlyInSelection:= ANew.UnprintedSpacesInSel;
  //if AOld.UnprintedEnds<>ANew.UnprintedEnds then
    Ed.OptUnprintedEnds:= ANew.UnprintedEnds;
  //if AOld.UnprintedEndsDetails<>ANew.UnprintedEndsDetails then
    Ed.OptUnprintedEndsDetails:= ANew.UnprintedEndsDetails;
  Ed.OptUnprintedForceTabs:= ANew.UnprintedForceTabs;
end;

procedure EditorCopySelToPrimarySelection(Ed: TATSynEdit; AMaxLineCount: integer);
var
  Caret: TATCaretItem;
  NFrom, NTo: integer;
begin
  if Ed.Carets.Count<>1 then exit;
  Caret:= Ed.Carets[0];
  if Caret.EndY<0 then exit;
  Caret.GetSelLines(NFrom, NTo, false);
  if NTo-NFrom<=AMaxLineCount then
    SClipboardCopy(Ed.TextSelected, PrimarySelection);
end;

function EditorGetTokenKind(Ed: TATSynEdit; AX, AY: integer;
  ADocCommentIsAlsoComment: boolean): TATTokenKind;
var
  NLen: integer;
begin
  Result:= TATTokenKind.Other;
  if Ed.AdapterForHilite is TATAdapterEControl then
  begin
    if not Ed.Strings.IsIndexValid(AY) then
      exit;
    NLen:= Ed.Strings.LinesLen[AY];
    if NLen=0 then
      exit;
    if AX>NLen then
      exit;
    if AX=NLen then //caret at line end: decrement X
      Dec(AX);
    Result:= TATAdapterEControl(Ed.AdapterForHilite).GetTokenKindAtPos(Point(AX, AY), ADocCommentIsAlsoComment)
  end;
end;


procedure EditorCopyLine(Ed: TATSynEdit);
var
  N: integer;
begin
  N:= Ed.Carets[0].PosY;
  if Ed.Strings.IsIndexValid(N) then
  begin
    SClipboardCopy(UTF8Encode(Ed.Strings.Lines[N]));
  end;
end;

procedure EditorCopyAsHTML(Ed: TATSynEdit);
var
  List: TStringList;
  SText: string;
  Caret: TATCaretItem;
  SaveCarets: TATCarets;
  PosBegin, PosEnd: TPoint;
  X1, Y1, X2, Y2: integer;
  bSel: boolean;
begin
  if Ed.Carets.Count=0 then exit;

  Caret:= Ed.Carets[0];
  Caret.GetRange(X1, Y1, X2, Y2, bSel);
  if bSel then
  begin
    PosBegin:= Point(X1, Y1);
    PosEnd:= Point(X2, Y2);
  end
  else
  if Ed.OptCopyLinesIfNoSel and Ed.Strings.IsIndexValid(Y1) then
  begin
    PosBegin:= Point(0, Y1);
    PosEnd:= Point(Ed.Strings.LinesLen[Y1], Y1);
  end
  else
    exit;

  //copy in text format first
  SText:= Ed.Strings.TextSubstring(PosBegin.X, PosBegin.Y, PosEnd.X, PosEnd.Y);
  Clipboard.AsText:= SText;

  //hide caret
  SaveCarets:= TATCarets.Create;
  SaveCarets.Assign(Ed.Carets);

  Ed.DoCaretSingle(-1, -1);
  Ed.DoEventCarets;
  Ed.Update;

  List:= TStringList.Create;
  try
    EditorExportToHTML(Ed,
      List,
      PosBegin,
      PosEnd,
      '',
      UiOps.ExportHtmlFontName,
      UiOps.ExportHtmlFontSize,
      false,
      GetAppColor(apclExportHtmlBg),
      clBlack
      );

    List.TrailingLineBreak:= false;
    SText:= List.Text;
  finally
    FreeAndNil(List);
  end;

  Ed.Carets.Assign(SaveCarets);
  Ed.DoEventCarets;
  Ed.Update;
  FreeAndNil(SaveCarets);

  Clipboard.SetAsHtml(SText);
end;


function EditorExpandSelectionToWord(Ed: TATSynEdit;
  AFinderResultCallback: TATEditorFinderCallback;
  AAddOrSkip, AWholeWords: boolean): boolean;
var
  Caret: TATCaretItem;
  Finder: TATEditorFinder;
  MarkerPtr: PATMarkerItem;
  X1, Y1, X2, Y2, NSelLen, NCaret: integer;
  bSel, bForwardSel: boolean;
  bWholeWord: boolean;
  sText: UnicodeString;
  iMarker: integer;
begin
  Result:= true;
  bWholeWord:= false;
  sText:= '';

  //issue #2828: detect WholeWord by first caret, not last
  Caret:= Ed.Carets[0];
  Caret.GetRange(X1, Y1, X2, Y2, bSel);
  if bSel then
    if Ed.Strings.IsIndexValid(Y1) then
    begin
      bWholeWord:=
        IsFinderWholeWordRange(Ed.Strings.Lines[Y1], X1+1, X2+1);
        //STextWholeWordSelection(Ed.Strings.Lines[Y1], X1, X2, Ed.OptNonWordChars);
      sText:= Ed.Strings.TextSubstring(X1, Y1, X2, Y2);
    end;

  //now we need coords of last caret
  Caret:= Ed.Carets[Ed.Carets.Count-1];
  Caret.GetRange(X1, Y1, X2, Y2, bSel);
  //pos are sorted: (X1,Y1) <= (X2,Y2)

  //consider position of marker with special tag
  for iMarker:= 0 to Ed.Markers.Count-1 do
  begin
    MarkerPtr:= Ed.Markers.ItemPtr(iMarker);
    if MarkerPtr^.Tag=UiOps.FindOccur_TagValue then
    begin
      X1:= MarkerPtr^.PosX;
      Y1:= MarkerPtr^.PosY;
      X2:= X1;
      Y2:= Y1;
      Ed.Markers.Delete(iMarker);
      Break
    end;
  end;

  //skip last (last made, not last in text) selection
  if not AAddOrSkip then
  begin
    NCaret:= Ed.Carets.FindCaretBeforePos(X1, Y1, true);
    if NCaret>=0 then
      Ed.Carets.Delete(NCaret);
  end;

  if not bSel then
  begin
    bForwardSel:= true;
    Ed.DoSelect_CharGroupAtPos(Point(X1, Y1), false, true);
  end
  else
  begin
    Finder:= TATEditorFinder.Create;
    try
      bForwardSel:= Caret.IsForwardSelection;

      Finder.StrFind:= sText;
      Finder.StrReplace:= '';
      Finder.Editor:= Ed;

      Finder.OptWords:= bWholeWord and AWholeWords;
      Finder.OptRegex:= false;
      Finder.OptCase:= true;
      Finder.OptBack:= false;
      Finder.OptFromCaret:= false;
      Finder.OptWrapped:= true; //wrapped search, like in Sublime
      Finder.OptInSelection:= false;
      Finder.OptTokens:= cTokensAll;
      Finder.OptPreserveCase:= false;

      Result:= Finder.DoAction_FindSimple(Point(X2, Y2));
      AFinderResultCallback(Result, Finder);

      if Result then
      begin
        if bForwardSel then
          Ed.Carets.Add(
            Finder.MatchEdEnd.X,
            Finder.MatchEdEnd.Y,
            Finder.MatchEdPos.X,
            Finder.MatchEdPos.Y
            )
        else
          Ed.Carets.Add(
            Finder.MatchEdPos.X,
            Finder.MatchEdPos.Y,
            Finder.MatchEdEnd.X,
            Finder.MatchEdEnd.Y
            );
        //sort carets: maybe we added caret in the middle
        Ed.Carets.Sort;

        if Finder.MatchEdEnd.Y=Finder.MatchEdPos.Y then
          NSelLen:= Finder.MatchEdEnd.X-Finder.MatchEdPos.X
        else
          NSelLen:= 0;

        //place marker which will be useful after find-next will wrap, so
        //last-occurrence-pos will be irrelevant, but marker will be used
        Ed.Markers.Add(
          Point(Finder.MatchEdEnd.X, Finder.MatchEdEnd.Y),
          Point(0, 0),
          TATMarkerTags.Init(UiOps.FindOccur_TagValue, 0),
          nil,
          mmmShowInTextOnly,
          -NSelLen //marker with underline looks good
          );

        Ed.DoShowPos(
          Point(Finder.MatchEdPos.X, Finder.MatchEdPos.Y),
          UiOps.FindIndentHorz,
          UiOps.FindIndentVert,
          true,
          true,
          true);
      end;
    finally
      FreeAndNil(Finder);
    end;
  end;

  Ed.Update;
end;


function EditorFindCurrentWordOrSel(Ed: TATSynEdit;
  ANext, AWordOrSel, AOptCase, AOptWrapped: boolean;
  out Str: UnicodeString): boolean;
var
  Finder: TATEditorFinder;
  bFlag: boolean;
begin
  Str:= '';
  if Ed.Carets.Count<>1 then exit;

  if AWordOrSel then
  begin
    Str:= Ed.TextCurrentWord;
    Ed.DoCommand(cCommand_SelectWords, TATCommandInvoke.AppInternal);
  end
  else
  begin
    Str:= Ed.TextSelected;
    if Str='' then
    begin
      Str:= Ed.TextCurrentWord;
      Ed.DoCommand(cCommand_SelectWords, TATCommandInvoke.AppInternal);
    end;
  end;
  if Str='' then exit;

  Finder:= TATEditorFinder.Create;
  try
    Finder.StrFind:= Str;
    Finder.StrReplace:= '';
    Finder.Editor:= Ed;

    Finder.OptRegex:= false;
    Finder.OptCase:= AOptCase;
    Finder.OptWords:= AWordOrSel;
    Finder.OptWrapped:= AOptWrapped;
    Finder.OptFromCaret:= true;
    Finder.OptBack:= not ANext;
    Finder.OptInSelection:= false;
    Finder.OptTokens:= cTokensAll;

    Result:= Finder.DoAction_FindOrReplace(false, false, bFlag, false{UpdateCaret});
    if Result then
      Ed.DoGotoPos(
        Finder.MatchEdEnd,
        Finder.MatchEdPos,
        UiOps.FindIndentHorz,
        UiOps.FindIndentVert,
        true,
        true
        );
  finally
    FreeAndNil(Finder);
  end;
end;


procedure EditorHighlightAllMatches(AFinder: TATEditorFinder;
  AEnableFindNext: boolean; out AMatchesCount: integer; ACaretPos: TPoint);
var
  ColorBorder: TColor;
  StyleBorder: TATLineStyle;
  SavedCarets: TATCarets;
  bChanged: boolean;
  bSaveCarets: boolean;
  bSavedWrappedConfirm: boolean;
  NLineCount: integer;
begin
  ColorBorder:= GetAppStyle(AppHiAll_ThemeStyleId).BgColor;

  if EditorOps.OpActiveBorderWidth>1 then
    StyleBorder:= cLineStyleSolid2px
  else
    StyleBorder:= cLineStyleRounded;

  //stage-1: highlight all matches
  AMatchesCount:= AFinder.DoAction_HighlightAllEditorMatches(
    ColorBorder,
    StyleBorder,
    UiOps.FindHiAll_TagValue,
    UiOps.FindHiAll_MaxLines
    );

  //stage-2: perform find-next from ACaretPos
  ////if UiOps.FindHiAll_MoveCaret then
  if AEnableFindNext then
  begin
    //CudaText issue #3950.
    //we save selections before running HighlightAll, later we restore them.
    bSaveCarets:= AFinder.OptInSelection and AFinder.Editor.Carets.IsSelection;
    if bSaveCarets then
      SavedCarets:= TATCarets.Create;

    try
      if bSaveCarets then
        SavedCarets.Assign(AFinder.Editor.Carets);

      //we found and highlighted all matches,
      //now we need to do 'find next from caret' like Sublime does
      NLineCount:= AFinder.Editor.Strings.Count;
      if ACaretPos.Y>=NLineCount then exit;

      bSavedWrappedConfirm:= AFinder.OptWrappedConfirm;
      AFinder.OptWrappedConfirm:= false;
      AFinder.OptFromCaret:= true;
      AFinder.Editor.DoCaretSingle(ACaretPos.X, ACaretPos.Y);

      if AFinder.DoAction_FindOrReplace(
        false{AReplace},
        false,
        bChanged,
        false{AUpdateCaret}
        ) then
        AFinder.Editor.DoGotoPos(
          AFinder.MatchEdPos,
          AFinder.MatchEdEnd,
          AFinder.IndentHorz,
          100{big value to center vertically},
          true{APlaceCaret},
          true{ADoUnfold}
          );

      if bSaveCarets then
        AFinder.Editor.Carets.Assign(SavedCarets);

      AFinder.OptWrappedConfirm:= bSavedWrappedConfirm;
    finally
      if bSaveCarets then
        FreeAndNil(SavedCarets);
    end;
  end;
end;


function EditorIsEmpty(Ed: TATSynEdit): boolean;
var
  Str: TATStrings;
begin
  //dont check Modified here
  Str:= Ed.Strings;
  Result:=
    (Str.Count=0) or ((Str.Count=1) and (Str.LinesLen[0]=0));
end;

procedure DeleteArrayItem(var Ar: TATIntArray; Val: integer);
var
  i, j: integer;
begin
  for i:= High(Ar) downto Low(Ar) do
    if Ar[i]=Val then
    begin
      for j:= i to High(Ar)-1 do
        Ar[j]:= Ar[j+1];
      SetLength(Ar, Length(Ar)-1);
      Break;
    end;
end;

procedure DeleteArrayLastItem(var Ar: TATIntArray);
begin
  if Length(Ar)>0 then
    SetLength(Ar, Length(Ar)-1);
end;

procedure AddArrayItem(var Ar: TATIntArray; Val: integer);
begin
  SetLength(Ar, Length(Ar)+1);
  Ar[High(Ar)]:= Val;
end;

procedure EditorHighlightCharsInLine(Ed: TATSynEdit; AY: integer;
  const AX: TATIntArray; AStyle: TAppThemeStyleId; ATag: integer);
var
  LinePart: TATLinePart;
  i: integer;
begin
  if Length(AX)=0 then exit;

  InitLinePart(LinePart);
  ApplyPartStyleFromEcontrolStyle(LinePart, GetAppStyle(AStyle));
  LinePart.ColorBG:= clNone;

  for i:= 0 to High(AX) do
    Ed.Attribs.Add(
      Point(AX[i], AY),
      Point(1, 0),
      TATMarkerTags.Init(ATag, 0),
      @LinePart
      );

  Ed.Invalidate;
end;

procedure EditorSetLine(Ed: TATSynEdit; AIndex: integer; AStr: UnicodeString);
var
  Strs: TATStrings;
  NLastIndex, i: integer;
begin
  Strs:= Ed.Strings;
  Strs.SetNewCommandMark;

  //replace \n \r to "_"
  for i:= 1 to Length(AStr) do
    if (AStr[i]=#10) or (AStr[i]=#13) then
      AStr[i]:= '_';

  if (AIndex=-1) or (AIndex=-2) then
  begin
    NLastIndex:= Strs.Count-1;
    Strs.LineAdd(AStr);
    //fix old last line not having the EOL mark
    if NLastIndex>=0 then
    begin
      if Strs.LinesEnds[NLastIndex]=TATLineEnds.None then
        Strs.LinesEnds[NLastIndex]:= Strs.Endings;
    end;
    if (AIndex=-2) and (AStr<>'') then
      Strs.ActionDeleteFakeLineAndFinalEol;
  end
  else
  if Strs.IsIndexValid(AIndex) then
  begin
    Strs.Lines[AIndex]:= AStr;
    if AIndex<Strs.Count-1 then
      if Strs.LinesEnds[AIndex]=TATLineEnds.None then
        Strs.LinesEnds[AIndex]:= Strs.Endings;
  end;

  Ed.DoEventChange(AIndex);
  Strs.ActionSaveLastEditionPos(0, AIndex);
  Ed.UpdateWrapInfo(true); //fixing #4173
  Ed.Update(true);
end;

procedure EditorSetAllText(Ed: TATSynEdit; const AStr: string);
var
  Strs: TATStrings;
begin
  Strs:= Ed.Strings;
  Strs.SetNewCommandMark;

  //don't keep Undo, not needed in set_text_all
  Ed.DoCaretSingle(0, 0);
  Ed.Markers.Clear;
  Ed.Attribs.Clear;
  Strs.LoadFromString(AStr);

  Ed.DoCaretsFixIncorrectPos(false);
  Ed.DoEventChange(0);
  Strs.ActionSaveLastEditionPos(0, 0);

  Strs.EnableCachedWrapinfoUpdate:= false;
  Strs.IndexesOfEditedLines.Clear; //don't keep Undo -> list should be cleared
  Ed.UpdateWrapInfo(true); //fix 2nd+3rd parts of CudaText #4172, Ed.Update(true) is not enough
  Ed.Update(true); //with True, to fix CudaText #4174
end;

procedure EditorDeleteRange(Ed: TATSynEdit; X1, Y1, X2, Y2: integer);
var
  Strs: TATStrings;
  Shift, PosAfter: TPoint;
begin
  Strs:= Ed.Strings;
  Strs.SetNewCommandMark;

  Strs.TextDeleteRange(X1, Y1, X2, Y2, Shift, PosAfter);
  Ed.UpdateMarkersOnDeleting(X1, Y1, X2, Y2);
  Ed.UpdateCaretsAndMarkersOnEditing(0,
    Point(X1, Y1),
    Point(X2, Y2),
    Shift,
    PosAfter);

  Ed.DoEventChange(Y1);
  Ed.Update(true);
end;

function EditorInsert(Ed: TATSynEdit; AX, AY: integer; const AStr: UnicodeString; out APosAfter: TPoint): boolean;
var
  Strs: TATStrings;
  Shift: TPoint;
begin
  Result:= true;
  Strs:= Ed.Strings;
  Strs.SetNewCommandMark;
  APosAfter:= Point(AX, AY);
  if AY<0 then
    exit(false);

  //too big index: do append
  if AY>=Strs.Count then
    Strs.TextAppend(AStr, Shift, APosAfter)
  else
  begin
    Strs.TextInsert(AX, AY, AStr, false, Shift, APosAfter);
    Ed.UpdateCaretsAndMarkersOnEditing(0,
      Point(AX, AY),
      Point(AX, AY),
      Shift,
      APosAfter);
  end;

  Ed.DoEventChange(AY);
  Ed.UpdateWrapInfo(true); //fixing #4173
  Ed.Update(true);
end;

procedure EditorHighlightBadRegexBrackets(Ed: TATSynEdit; AOnlyClear: boolean);
const
  cBadRegexTag = 10;
var
  Bads: TATIntArray;
  OpenedRound: TATIntArray;
  OpenedSquare: TATIntArray;
  LevelRound, LevelSquare: integer;
  PosSquareOpen: integer;
  S: UnicodeString;
  ch: WideChar;
  i: integer;
begin
  Ed.Attribs.DeleteWithTag(cBadRegexTag);
  if AOnlyClear then exit;

  S:= Ed.Text;
  Bads:= nil;
  OpenedRound:= nil;
  OpenedSquare:= nil;
  LevelRound:= 0;
  LevelSquare:= 0;
  PosSquareOpen:= 0;
  i:= 0;

  while i<Length(S) do
  begin
    Inc(i);
    if S[i]='\' then
    begin
      Inc(i);
      Continue;
    end;
    ch:= S[i];

    //regex allows any () inside char-class []
    if LevelSquare<1 then
    begin
      if ch='(' then
      begin
        AddArrayItem(OpenedRound, i);
        Inc(LevelRound);
        Continue;
      end;

      if ch=')' then
      begin
        if LevelRound<1 then
          AddArrayItem(Bads, i);
        if LevelRound>0 then
          Dec(LevelRound);
        DeleteArrayLastItem(OpenedRound);
        Continue;
      end;
    end;

    //no pairs of [], only one level is allowed
    if LevelSquare=0 then
      if ch='[' then
      begin
        AddArrayItem(OpenedSquare, i);
        Inc(LevelSquare);

        PosSquareOpen:= i;
        if (i<Length(S)) and (S[i+1]='^') then
          Inc(PosSquareOpen);

        Continue;
      end;

    if ch=']' then
    begin
      if LevelSquare<1 then
        AddArrayItem(Bads, i);
      //don't close by ']' immediately after '[' or '[^'
      if (LevelSquare>0) and (PosSquareOpen>0) and (i-PosSquareOpen>1) then
      begin
        Dec(LevelSquare);
        PosSquareOpen:= 0;
        DeleteArrayLastItem(OpenedSquare);
      end;
      Continue;
    end;
  end;

  for i:= 0 to High(OpenedRound) do
    AddArrayItem(Bads, OpenedRound[i]);
  for i:= 0 to High(OpenedSquare) do
    AddArrayItem(Bads, OpenedSquare[i]);

  if Length(Bads)>0 then
  begin
    for i:= 0 to High(Bads) do
      Dec(Bads[i]);
    EditorHighlightCharsInLine(Ed, 0, Bads, apstSymbolBad, cBadRegexTag);
  end;
end;

procedure EditorAdjustForBigFile(Ed: TATSynEdit);
begin
  Ed.OptWrapMode:= TATEditorWrapMode.ModeOff;
  Ed.OptMicromapVisible:= false;
end;


procedure EditorStartParse(Ed: TATSynEdit);
var
  Ada: TATAdapterEControl;
begin
  if Assigned(Ed.AdapterForHilite) then
    if Ed.AdapterForHilite is TATAdapterEControl then
    begin
      Ada:= TATAdapterEControl(Ed.AdapterForHilite);
      if Assigned(Ada.AnClient) then
        Ada.AnClient.EventParseNeeded.SetEvent;
    end;
end;

function EditorCaretIsOnStart(Ed: TATSynEdit): boolean;
var
  Caret: TATCaretItem;
begin
  if Ed.Carets.Count<>1 then exit(false);
  Caret:= Ed.Carets[0];
  Result:= (Caret.PosX=0) and (Caret.PosY=0) and (Caret.EndY=-1);
end;

function EditorLexerNameAtPos(Ed: TATSynEdit; APos: TPoint): string;
// APos.Y=-1 means 'get lexer for entire document'
var
  CurAdapter: TATAdapterHilite;
  an: TecSyntAnalyzer;
begin
  Result:= '';
  CurAdapter:= Ed.AdapterForHilite;
  if CurAdapter=nil then exit;

  if CurAdapter is TATAdapterEControl then
  begin
    if APos.Y<0 then
      an:= TATAdapterEControl(CurAdapter).Lexer
    else
      an:= TATAdapterEControl(CurAdapter).LexerAtPos(APos);
    if Assigned(an) then
      Result:= an.LexerName;
  end
  else
  if CurAdapter is TATLiteLexer then
    Result:= TATLiteLexer(CurAdapter).LexerName+msgLiteLexerSuffix;
end;

function EditorAutoCompletionAfterTypingChar(Ed: TATSynEdit;
  const AText: string; var ACharsTyped: integer): boolean;
var
  Caret: TATCaretItem;
  STextW: UnicodeString;
  bWordChar, bIdentChar: boolean;
  //SLexerName: string;
  //bLexerHTML: boolean;
begin
  Result:= true;
  if Ed.Carets.Count=0 then exit;
  Caret:= Ed.Carets[0];
  bWordChar:= false;
  bIdentChar:= false;

  //avoid double firing on_complete API event, when user types chars with listbox visible; issue #4323
  if Assigned(FormAutoCompletion) and FormAutoCompletion.Visible then exit;

  //SLexerName:= EditorLexerNameAtPos(Ed, Point(Caret.PosX, Caret.PosY));
  //bLexerHTML:= Pos('HTML', SLexerName)>0;

  //autoshow by trigger chars
  if (Ed.OptAutocompleteTriggerChars<>'') and
    (Pos(AText[1], Ed.OptAutocompleteTriggerChars)>0) then
  begin
    //check that we are not inside comment (strings are OK)
    if EditorGetTokenKind(Ed, Caret.PosX, Caret.PosY)=TATTokenKind.Comment then exit;

    ACharsTyped:= 0;
    AppRunAutocomplete(Ed, true);
    exit;
  end;

  //other conditions need word-char
  STextW:= UTF8Decode(AText);
  if Length(STextW)=1 then
  begin
    bWordChar:= IsCharWord(STextW[1], Ed.OptNonWordChars);
    if not bWordChar then
    begin
      ACharsTyped:= 0;
      AppRunAutocomplete(Ed, false);
      exit;
    end;
  end
  else
    AppRunAutocomplete(Ed, false);

  //autoshow for all, when typed N chars
  if (Ed.OptAutocompleteAutoshowCharCount>0) then
  begin
    //ignore if number typed
    bIdentChar:= bWordChar and not IsCharDigit(AText[1]);
    if (ACharsTyped=0) and (not bIdentChar) then exit;

    //check that we are not inside comment,
    //but allow autocomplete in HTML strings like in <a target="_bl|"
    if EditorGetTokenKind(Ed, Caret.PosX, Caret.PosY)=TATTokenKind.Comment then exit;

    Inc(ACharsTyped);
    if ACharsTyped>=Ed.OptAutocompleteAutoshowCharCount then
    begin
      //ACharsTyped:= 0; //don't reset, fix #4479
      Application.ProcessMessages; //give time to lexer-parser, to fix https://github.com/CudaText-addons/cuda_lsp/issues/137
      AppRunAutocomplete(Ed, true);
      exit;
    end;
  end
  else
  begin
    ACharsTyped:= 0;
    AppRunAutocomplete(Ed, false);
  end;

  (*
  //autoshow for HTML
  //block is commented, since HTML is handled good by AutoshowCharCount
  if UiOps.AutocompleteHtml and bLexerHTML then
  begin
    if Ed.Strings.LineCharAt(Caret.PosY, Caret.PosX-1)='<' then
    begin
      AppRunAutocomplete(Ed, true);
    end;
    exit;
  end;
  *)

  (*
  //autoshow for CSS
  //block is commented, since CSS must work like other lexers with AutoshowCharCount
  if UiOps.AutocompleteCss and (SLexerName='CSS') then
  begin
    if EditorIsAutocompleteCssPosition(Ed, Caret.PosX-1, Caret.PosY) then
      AOnAutoCompletion(Ed);
    exit;
  end;
  *)

  Result:= false;
end;

function IsValidHtmlTagChar(ch: WideChar): boolean;
begin
  case ch of
    'a'..'z',
    'A'..'Z',
    '0'..'9',
    ':', '_':
      Result:= true;
    else
      Result:= false;
  end;
end;

function EditorGetLefterHtmlTag(Ed: TATSynEdit; AX, AY: integer): UnicodeString;
var
  St: TATStrings;
  NLen, NStart, i: integer;
  SLine: UnicodeString;
begin
  Result:= '';
  St:= Ed.Strings;
  if not St.IsIndexValid(AY) then exit;
  SLine:= St.LineSub(AY, 1, AX+1);
  NLen:= Length(SLine);
  if AX<3 then exit; //need at least <h>
  if AX>NLen then exit;

  //don't work if next text is some tag
  if (AX<NLen) and (SLine[AX+1]='<') then exit;

  NStart:= RPosEX('<', SLine, AX);
  if NStart=0 then exit;
  SLine:= Copy(SLine, NStart+1, AX-NStart-1);
  if SLine='' then exit;

  if not IsCharWordA(SLine[1]) then exit;
  if not IsCharWordA(SLine[Length(SLine)]) then exit;
  for i:= 2 to Length(SLine)-1 do
    if not IsValidHtmlTagChar(SLine[i]) then exit;

  Result:= SLine;
end;

function EditorGetLefterWordChars(Ed: TATSynEdit; AX, AY: integer): integer;
var
  St: TATStrings;
  i: integer;
  SLine: UnicodeString;
begin
  Result:= 0;
  St:= Ed.Strings;
  if not St.IsIndexValid(AY) then exit;
  if AX>St.LinesLen[AY] then exit;
  SLine:= St.LineSub(AY, 1, AX);
  for i:= AX-1 downto 0 do
  begin
    if not IsCharWord(SLine[i+1], Ed.OptNonWordChars) then
      Break;
    Inc(Result);
  end;
end;

procedure EditorAutoCloseOpeningHtmlTag(Ed: TATSynEdit; AX, AY: integer);
var
  SValue: UnicodeString;
  SLexer: string;
begin
  if not (UiOps.AutocompleteHtml and UiOps.AutocompleteHtml_AutoClose) then exit;
  if Ed.AdapterForHilite=nil then exit;
  SLexer:= Ed.AdapterForHilite.GetLexerName;
  if SLexer='' then exit;

  SValue:= EditorGetLefterHtmlTag(Ed, AX, AY);
  if SValue='' then exit;
  if not IsTagNeedsClosingTag(LowerCase(SValue)) then exit;

  if SRegexMatchesString(SLexer, UiOps.AutocompleteHtml_Lexers, false) then
  begin
    Ed.TextInsertAtCarets('</'+SValue+'>', true{AKeepCaret}, false, false);
    Ed.DoEventChange(AY);
  end;
end;

function EditorBookmarksToString(Ed: TATSynEdit): string;
var
  List: TStringList;
  Bm: PATBookmarkItem;
  i: integer;
begin
  List:= TStringList.Create;
  List.Delimiter:= ' ';
  for i:= 0 to Ed.Strings.Bookmarks.Count-1 do
  begin
    Bm:= Ed.Strings.Bookmarks[i];
    //save usual bookmarks and numbered bookmarks (kind=1..10)
    if Bm^.Data.Kind>10 then Continue;
    List.Add(IntToStr(Bm^.Data.LineNum)+','+IntToStr(Bm^.Data.Kind));
  end;
  Result:= List.DelimitedText;
end;

procedure EditorStringToBookmarks(Ed: TATSynEdit; const AValue: string);
var
  Sep, Sep2: TATStringSeparator;
  StrItem: string;
  nLine, nKind: integer;
  Bm: TATBookmarkData;
begin
  Bm:= Default(TATBookmarkData);
  Bm.ShowInBookmarkList:= true;

  Sep.Init(AValue, ' ');
  while Sep.GetItemStr(StrItem) do
  begin
    Sep2.Init(StrItem, ',');
    if not Sep2.GetItemInt(nLine, -1) then Continue;
    if not Sep2.GetItemInt(nKind, 0, 0, 10) then Continue;
    if Ed.Strings.IsIndexValid(nLine) then
    begin
      Bm.LineNum:= nLine;
      Bm.Kind:= nKind;
      Bm.AutoDelete:= TATBookmarkAutoDelete.ByOption;
      Ed.Strings.Bookmarks.Add(Bm);
    end;
  end;
end;

type
  { TEditorHtmlTagRecord }

  PEditorHtmlTagRecord = ^TEditorHtmlTagRecord;
  TEditorHtmlTagRecord = record
    bClosing: boolean;
    sTagName: string[30];
    class operator =(const a, b: TEditorHtmlTagRecord): boolean;
  end;

  { TEditorHtmlTagList }

  TEditorHtmlTagList = class(specialize TFPGList<TEditorHtmlTagRecord>)
  public
    function ItemPtr(AIndex: integer): PEditorHtmlTagRecord;
  end;


procedure EditorFindHtmlTagsInText(var AText: UnicodeString;
  AList: TEditorHtmlTagList; AllowSingletonTags: boolean);
const
  cRegexComment = '(?s)<!--.*?-->';
  cRegexScript = '(?s)<script\b.+?</script>';
  cRegexTags = '(?s)<(/?)([a-z][\w:]*).*?>';
var
  obj: TRegExpr;
  TagRecord: TEditorHtmlTagRecord;
begin
  AList.Clear;

  //remove HTML comments
  obj:= TRegExpr.Create(cRegexComment);
  try
    obj.Compile;
    AText:= obj.Replace(AText, ' ');
  finally
    FreeAndNil(obj);
  end;

  //remove JS/CSS blocks
  obj:= TRegExpr.Create(cRegexScript);
  try
    obj.Compile;
    AText:= obj.Replace(AText, ' ');
  finally
    FreeAndNil(obj);
  end;

  //find tags
  obj:= TRegExpr.Create(cRegexTags);
  try
    obj.Compile;
    obj.InputString:= AText;
    if obj.ExecPos(1) then
    repeat
      TagRecord.bClosing:= obj.Match[1]<>'';
      TagRecord.sTagName:= obj.Match[2];

      if not AllowSingletonTags then
        if not IsTagNeedsClosingTag(TagRecord.sTagName) then Continue;

      AList.Add(TagRecord);
    until not obj.ExecNext;
  finally
    FreeAndNil(obj);
  end;
end;


function EditorFindHtmlLastOpenedTagInText(var AText: UnicodeString): string;
var
  tags: TEditorHtmlTagList;
  tagPtr, tagPtr2: PEditorHtmlTagRecord;
  i, j: integer;
begin
  Result:= '';
  tags:= TEditorHtmlTagList.Create;
  try
    EditorFindHtmlTagsInText(AText, tags, false);

    //delete pairs <tag> - </tag>
    for i:= tags.Count-1 downto 0 do
    begin
      tagPtr:= tags.ItemPtr(i);
      if not tagPtr^.bClosing then
        for j:= i+1 to tags.Count-1 do
        begin
          tagPtr2:= tags.ItemPtr(j);
          if tagPtr2^.bClosing and SameText(tagPtr^.sTagName, tagPtr2^.sTagName) then
          begin
            tags.Delete(j);
            tags.Delete(i);
            Break;
          end;
        end;
    end;

    //take last opened tag
    for i:= tags.Count-1 downto 0 do
    begin
      tagPtr:= tags.ItemPtr(i);
      if not tagPtr^.bClosing then
      begin
        Result:= tagPtr^.sTagName;
        Break
      end;
    end;
  finally
    FreeAndNil(tags);
  end;
end;


procedure EditorAutoCloseClosingHtmlTag(Ed: TATSynEdit; AX, AY: integer);
var
  SText: UnicodeString;
  STag: string;
  SLexer: string;
  St: TATStrings;
  ch: WideChar;
  NLen: integer;
begin
  if Ed.Carets.Count<>1 then exit; //don't support multi-carets
  if not (UiOps.AutocompleteHtml and UiOps.AutocompleteHtml_AutoClose) then exit;
  if Ed.AdapterForHilite=nil then exit;
  SLexer:= Ed.AdapterForHilite.GetLexerName;
  if SLexer='' then exit;
  St:= Ed.Strings;

  if not St.IsIndexValid(AY) then exit;
  NLen:= St.LinesLen[AY];
  if AX<2 then exit;
  if AX>NLen then exit;
  if AX<NLen then
  begin
    ch:= St.LineCharAt(AY, AX+1);
    if IsCharWord(ch, Ed.OptNonWordChars) then exit;
  end;
  ch:= St.LineCharAt(AY, AX);
  if ch<>'/' then exit;
  ch:= St.LineCharAt(AY, AX-1);
  if ch<>'<' then exit;

  if not SRegexMatchesString(SLexer, UiOps.AutocompleteHtml_Lexers, false) then exit;

  SText:= St.TextSubstring(0, 0, AX-2, AY);
  STag:= EditorFindHtmlLastOpenedTagInText(SText);
  if STag='' then exit;

  Ed.TextInsertAtCarets(STag+'>', false{AKeepCaret}, false, false);
  Ed.DoEventChange(AY);
end;

procedure EditorChangeLineEndsForSelection(Ed: TATSynEdit; AValue: TATLineEnds);
var
  Caret: TATCaretItem;
  Y1, Y2: integer;
  iCaret, iLine: integer;
begin
  for iCaret:= 0 to Ed.Carets.Count-1 do
  begin
    Caret:= Ed.Carets[iCaret];
    Caret.GetSelLines(Y1, Y2, true);
    for iLine:= Y1 to Y2 do
      Ed.Strings.LinesEnds[iLine]:= AValue;
  end;
  Ed.Modified:= true;
end;

procedure EditorClearHiAllMarkers(Ed: TATSynEdit);
begin
  if Ed.Attribs.Count>0 then
    if Ed.Attribs.DeleteWithTag(UiOps.FindHiAll_TagValue) then
      Ed.Update;
end;

procedure EditorForceUpdateIfWrapped(Ed: TATSynEdit);
begin
  if Ed.OptWrapMode<>TATEditorWrapMode.ModeOff then
  begin
    Ed.UpdateWrapInfo;
    Ed.Update;
  end;
end;

procedure EditorScrollToCaret(Ed: TATSynEdit; ANeedWrapOff, AllowProcessMsg: boolean);
begin
  if Ed.Carets.Count=0 then exit;
  if ANeedWrapOff then
    if Ed.OptWrapMode<>TATEditorWrapMode.ModeOff then exit;
  if AllowProcessMsg then
    Application.ProcessMessages;
  if Ed.IsCaretOnVisibleRect then exit; //don't work on GTK2 without App.ProcessMessagess

  Ed.DoCommand(cCommand_ScrollToCaretTop, TATCommandInvoke.AppInternal);
end;

procedure EditorCaretToView(Ed: TATSynEdit; ANeedWrapOff, AllowProcessMsg: boolean);
begin
  if Ed.Carets.Count=0 then exit;
  if ANeedWrapOff then
    if Ed.OptWrapMode<>TATEditorWrapMode.ModeOff then exit;
  if AllowProcessMsg then
    Application.ProcessMessages;
  if Ed.IsCaretOnVisibleRect then exit; //don't work on GTK2 without App.ProcessMessagess

  Ed.Carets[0].PosX:= 0;
  Ed.DoEventCarets;
  Ed.Update;
end;

procedure EditorCalcOffsetsForStatusbar(Ed: TATSynEdit; out AOffsetMax, AOffsetCaret: integer);
var
  St: TATStrings;
  Buffer: TATStringBuffer;
  LineLens: array of integer;
  Caret: TATCaretItem;
  i: integer;
begin
  AOffsetMax:= -1;
  AOffsetCaret:= -1;

  St:= Ed.Strings;
  if St.Count>ATEditorOptions.MaxLinesForStatusbarOffsetsCalc then exit;

  if St.StringBufferObject=nil then
    St.StringBufferObject:= TATStringBuffer.Create;
  Buffer:= St.StringBufferObject as TATStringBuffer;

  if Buffer.Version<>St.ModifiedVersion then
  begin
    LineLens:= nil;
    SetLength(LineLens, St.Count);
    for i:= 0 to St.Count-1 do
      LineLens[i]:= St.LinesLen[i];
    Buffer.Setup('', LineLens);
    LineLens:= nil;
    Buffer.Version:= St.ModifiedVersion;
  end;

  AOffsetMax:= Buffer.TextLength;

  if Ed.Carets.Count>0 then
  begin
    Caret:= Ed.Carets[0];
    AOffsetCaret:= Buffer.CaretToStr(Point(Caret.PosX, Caret.PosY));
  end;
end;


procedure EditorConvertTabsToSpaces(Ed: TATSynEdit);
var
  St: TATStrings;
  S1, S2: atString;
  bChanged: boolean;
  i: integer;
begin
  bChanged:= false;
  St:= Ed.Strings;
  St.BeginUndoGroup;
  try
    for i:= 0 to St.Count-1 do
    begin
      S1:= St.Lines[i];
      if not SStringHasTab(S1) then Continue;

      S2:= Ed.TabHelper.TabsToSpaces(i, S1);
      if S1<>S2 then
      begin
        St.Lines[i]:= S2;
        bChanged:= true;
      end;
    end;
  finally
    St.EndUndoGroup;
    if bChanged then
    begin
      Ed.Update(true);
      Ed.DoEventChange;
    end;
  end;
end;


procedure EditorConvertIndentation(Ed: TATSynEdit; ASpacesToTabs: boolean);
var
  St: TATStrings;
  S1, SBegin, SBeginNew, SEnd: atString;
  bChanged: boolean;
  N, i: integer;
begin
  bChanged:= false;
  St:= Ed.Strings;
  St.BeginUndoGroup;
  try
    for i:= 0 to St.Count-1 do
    begin
      S1:= St.Lines[i];

      N:= SGetIndentChars(S1);
      if N=0 then Continue;
      SBegin:= Copy(S1, 1, N);
      SEnd:= Copy(S1, N+1, MaxInt);

      if ASpacesToTabs then
        SBeginNew:= Ed.TabHelper.SpacesToTabs(i, SBegin)
      else
        SBeginNew:= Ed.TabHelper.TabsToSpaces(i, SBegin);

      if SBeginNew<>SBegin then
      begin
        St.Lines[i]:= SBeginNew+SEnd;
        bChanged:= true;
      end;
    end;
  finally
    St.EndUndoGroup;
    if bChanged then
    begin
      Ed.Update(true);
      Ed.DoEventChange;
    end;
  end;
end;


{ TEditorHtmlTagList }

function TEditorHtmlTagList.ItemPtr(AIndex: integer): PEditorHtmlTagRecord;
begin
  Result:= PEditorHtmlTagRecord(InternalGet(AIndex));
end;

{ TEditorHtmlTagRecord }

class operator TEditorHtmlTagRecord.=(const a, b: TEditorHtmlTagRecord): boolean;
begin
  Result:= false;
end;

end.
