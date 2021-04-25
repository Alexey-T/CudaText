(*
This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at http://mozilla.org/MPL/2.0/.

Copyright (c) Alexey Torgashin
*)
unit proc_editor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, StrUtils,
  Controls, LCLType,
  Dialogs, Forms,
  Clipbrd,
  ATSynEdit,
  ATSynEdit_LineParts,
  ATSynEdit_CanvasProc,
  ATSynEdit_Carets,
  ATSynEdit_Markers,
  ATSynEdit_Commands,
  ATSynEdit_CharSizer,
  ATSynEdit_Gutter_Decor,
  ATSynEdit_Adapter_EControl,
  ATSynEdit_Finder,
  ATStrings,
  ATStringProc,
  ATStringProc_Separator,
  proc_globdata,
  proc_colors,
  proc_msg,
  proc_files,
  ec_SyntAnal,
  ec_syntax_format,
  math;

procedure EditorStartParse(Ed: TATSynEdit);
procedure EditorAdjustForBigFile(Ed: TATSynEdit);
function EditorSaveFileAs(Ed: TATSynEdit; const AFileName: string): boolean;
function EditorIsEmpty(Ed: TATSynEdit): boolean;
function EditorIsModifiedEx(Ed: TATSynEdit): boolean;
procedure EditorSaveTempOptions(Ed: TATSynEdit; out Ops: TATEditorTempOptions);
procedure EditorRestoreTempOptions(Ed: TATSynEdit; const ANew, AOld: TATEditorTempOptions);
procedure EditorFocus(C: TWinControl);
procedure EditorMouseClick_AtCursor(Ed: TATSynEdit; AAndSelect: boolean);
procedure EditorMouseClick_NearCaret(Ed: TATSynEdit; const AParams: string; AAndSelect: boolean);
procedure EditorSetFont(F: TFont; const AParams: string);

procedure EditorClear(Ed: TATSynEdit);
function EditorGetCurrentChar(Ed: TATSynEdit): Widechar;
procedure EditorApplyOps(Ed: TATSynEdit; const Op: TEditorOps;
  AApplyUnprintedAndWrap, AApplyTabSize, AApplyCentering, AOneLiner: boolean);
procedure EditorApplyOpsCommon(Ed: TATSynEdit);

function EditorGetLinkAtScreenCoord(Ed: TATSynEdit; P: TPoint): atString;
function EditorGetLinkAtCaret(Ed: TATSynEdit): atString;

type
  TEdSelType = (selNo, selSmall, selStream, selCol, selCarets);

function EditorGetStatusType(ed: TATSynEdit): TEdSelType;
function EditorFormatStatus(ed: TATSynEdit; const str: string): string;
procedure EditorDeleteNewColorAttribs(ed: TATSynEdit);
procedure EditorGotoLastEditingPos(Ed: TATSynEdit; AIndentHorz, AIndentVert: integer);
function EditorGotoFromString(Ed: TATSynEdit; SInput: string): boolean;

procedure EditorApplyTheme(Ed: TATSynedit);
procedure EditorSetColorById(Ed: TATSynEdit; const Id: string; AColor: TColor);
function EditorGetColorById(Ed: TATSynEdit; const Id: string): TColor;

function EditorIsAutocompleteCssPosition(Ed: TATSynEdit; AX, AY: integer): boolean;
function EditorAutoSkipClosingBracket(Ed: TATSynEdit; CharClosing: char): boolean;
function EditorAutoPairChar(Ed: TATSynEdit; CharBegin: atChar): boolean;
procedure EditorCopySelToPrimarySelection(Ed: TATSynEdit; AMaxLineCount: integer);
procedure EditorCopyLine(Ed: TATSynEdit);
procedure EditorHighlightBadRegexBrackets(Ed: TATSynEdit; AOnlyClear: boolean);

procedure EditorCaretShapeFromString(Props: TATCaretShape; const AText: string);
procedure EditorCaretShapeFromPyTuple(Props: TATCaretShape; const AText: string);

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

procedure EditorBracket_ClearHilite(Ed: TATSynEdit);
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

function EditorGetTokenKind(Ed: TATSynEdit; AX, AY: integer): TATTokenKind;
function EditorExpandSelectionToWord(Ed: TATSynEdit): boolean;
function EditorFindCurrentWordOrSel(Ed: TATSynEdit;
  ANext, AWordOrSel, AOptCase, AOptWrapped: boolean;
  out Str: UnicodeString): boolean;
procedure EditorHighlightAllMatches(AFinder: TATEditorFinder;
  AScroll: boolean; out AMatchesCount: integer);


implementation

procedure EditorApplyOps(Ed: TATSynEdit; const Op: TEditorOps;
  AApplyUnprintedAndWrap, AApplyTabSize, AApplyCentering, AOneLiner: boolean);
var
  Sep: TATStringSeparator;
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

  Ed.OptScrollAnimationSteps:= Op.OpScrollAnimationSteps;
  Ed.OptScrollAnimationSleep:= Op.OpScrollAnimationSleep;

  Ed.OptScrollbarsNew:= Op.OpScrollbarsNew;
  Ed.OptCharSpacingY:= Op.OpSpacingY;

  if AApplyTabSize then
  begin
    Ed.OptTabSize:= Op.OpTabSize;
    Ed.OptTabSpaces:= Op.OpTabSpaces;
  end;

  Ed.OptBorderFocusedActive:= Op.OpActiveBorderInEditor;
  Ed.OptBorderWidthFocused:= AppScale(Op.OpActiveBorderWidth);

  Ed.OptOverwriteSel:= Op.OpOverwriteSel;
  Ed.OptOverwriteAllowedOnPaste:= Op.OpOverwriteOnPaste;

  ec_SyntAnal.AutoFoldComments:= Op.OpAutoFoldComments;
  Ed.OptAutoPairForMultiCarets:= Op.OpAutoCloseBracketsMultiCarets;
  Ed.OptAutoPairChars:= Op.OpAutoCloseBrackets;
  Ed.OptAutocompleteAutoshowCharCount:= Op.OpAutocompleteAutoshowCharCount;
  Ed.OptAutocompleteTriggerChars:= Op.OpAutocompleteTriggerChars;
  Ed.OptAutocompleteCommitChars:= Op.OpAutocompleteCommitChars;
  Ed.OptAutocompleteCloseChars:= Op.OpAutocompleteCloseChars;
  Ed.OptAutocompleteAddOpeningBracket:= Op.OpAutocompleteAddOpeningBracket;
  Ed.OptAutocompleteUpDownAtEdge:= Op.OpAutocompleteUpDownAtEdge;
  Ed.OptAutocompleteCommitIfSingleItem:= Op.OpAutocompleteCommitIfSingleItem;

  if not AOneLiner then
  begin
    Ed.OptGutterVisible:= Op.OpGutterShow;
    Ed.OptGutterShowFoldAlways:= Op.OpGutterFoldAlways;
    Ed.OptGutterIcons:= TATEditorGutterIcons(Op.OpGutterFoldIcons);
    if not Ed.IsModifiedGutterBookmarksVisible then
      Ed.Gutter[Ed.GutterBandBookmarks].Visible:= Op.OpGutterBookmarks;
    if not Ed.IsModifiedGutterFoldingVisible then
      Ed.Gutter[Ed.GutterBandFolding].Visible:= Op.OpGutterFold;
    if not Ed.IsModifiedGutterNumbersVisible then
      Ed.Gutter[Ed.GutterBandNumbers].Visible:= Op.OpNumbersShow;
    Ed.Gutter.Update;

    if Op.OpNumbersStyle<=Ord(High(TATEditorNumbersStyle)) then
      Ed.OptNumbersStyle:= TATEditorNumbersStyle(Op.OpNumbersStyle);
    Ed.OptNumbersShowCarets:= Op.OpNumbersForCarets;
    if Op.OpNumbersCenter then
      Ed.OptNumbersAlignment:= taCenter
    else
      Ed.OptNumbersAlignment:= taRightJustify;

    if not Ed.IsModifiedRulerVisible then
      Ed.OptRulerVisible:= Op.OpRulerShow;
    Ed.OptRulerNumeration:= TATEditorRulerNumeration(Op.OpRulerNumeration);
    Ed.OptRulerMarkSizeCaret:= Op.OpRulerMarkCaret;

    if not Ed.IsModifiedMinimapVisible then
      Ed.OptMinimapVisible:= Op.OpMinimapShow;
    Ed.OptMinimapShowSelAlways:= Op.OpMinimapShowSelAlways;
    Ed.OptMinimapShowSelBorder:= Op.OpMinimapShowSelBorder;
    Ed.OptMinimapCharWidth:= Op.OpMinimapCharWidth;
    Ed.OptMinimapAtLeft:= Op.OpMinimapAtLeft;
    Ed.OptMinimapCustomScale:= Op.OpMinimapScale;
    Ed.OptMinimapTooltipVisible:= Op.OpMinimapTooltipShow;
    Ed.OptMinimapTooltipLinesCount:= Op.OpMinimapTooltipLineCount;
    Ed.OptMinimapTooltipWidthPercents:= Op.OpMinimapTooltipWidth;

    if not Ed.IsModifiedMicromapVisible then
      Ed.OptMicromapVisible:= Op.OpMicromapShow;

    Ed.OptMarginRight:= Op.OpMarginFixed;
    Ed.OptMarginString:= Op.OpMarginString;

    Ed.OptShowURLs:= Op.OpLinks;
    Ed.OptShowURLsRegex:= Op.OpLinksRegex;
  end;

  if AApplyUnprintedAndWrap then
  begin
    if not Ed.IsModifiedUnprintedVisible then
      Ed.OptUnprintedVisible:= Op.OpUnprintedShow;

    if not Ed.IsModifiedUnprintedSpaces then
      Ed.OptUnprintedSpaces:= Pos('s', Op.OpUnprintedContent)>0;

    if not Ed.IsModifiedUnprintedEnds then
      Ed.OptUnprintedEnds:= Pos('e', Op.OpUnprintedContent)>0;

    if not Ed.IsModifiedUnprintedEndDetails then
      Ed.OptUnprintedEndsDetails:= Pos('d', Op.OpUnprintedContent)>0;

    Ed.OptUnprintedSpacesTrailing:= Pos('t', Op.OpUnprintedContent)>0;
    Ed.OptUnprintedSpacesBothEnds:= Pos('l', Op.OpUnprintedContent)>0;
    Ed.OptUnprintedSpacesOnlyInSelection:= Pos('x', Op.OpUnprintedContent)>0;
  end;

  //global options
  OptMaxTabPositionToExpand:= Op.OpTabMaxPosExpanded;
  OptMaxLineLenForAccurateCharWidths:= Op.OpMaxLineLenForAccurateCharWidths;

  Ed.OptMaxLineLenToTokenize:= Op.OpMaxLineLenToTokenize;

  OptUnprintedEndArrowOrDot:= Pos('.', Op.OpUnprintedContent)=0;
  OptUnprintedTabCharLength:= Op.OpUnprintedTabArrowLen;
  OptUnprintedSpaceDotScale:= Op.OpUnprintedSpaceDotScale;
  OptUnprintedEndDotScale:= Op.OpUnprintedEndDotScale;
  OptUnprintedEndFontScale:= Op.OpUnprintedEndFontScale * 6 div 10;
  OptUnprintedTabPointerScale:= Op.OpUnprintedTabPointerScale;
  OptUnprintedReplaceSpec:= Op.OpUnprintedReplaceSpec;
  OptUnprintedReplaceSpecToCode:= StrToInt('$'+Op.OpUnprintedReplaceToCode);

  if AApplyUnprintedAndWrap then
    if not Ed.IsModifiedWrapMode then
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

  if not AOneLiner then
  begin
    if not Ed.IsCaretShapeChangedFromAPI then
    begin
      EditorCaretShapeFromString(Ed.CaretShapeNormal, Op.OpCaretViewNormal);
      EditorCaretShapeFromString(Ed.CaretShapeOverwrite, Op.OpCaretViewOverwrite);
      EditorCaretShapeFromString(Ed.CaretShapeReadonly, Op.OpCaretViewReadonly);
    end;

    if Op.OpCaretAfterPasteColumn<=Ord(High(TATEditorPasteCaret)) then
      Ed.OptCaretPosAfterPasteColumn:= TATEditorPasteCaret(Op.OpCaretAfterPasteColumn);

    Ed.OptCaretVirtual:= Op.OpCaretVirtual;
    Ed.OptCaretManyAllowed:= Op.OpCaretMulti;
    Ed.OptCaretsAddedToColumnSelection:= Op.OpCaretsAddedToColumnSel;
    Ed.OptCaretsPrimitiveColumnSelection:= Op.OpCaretsPrimitiveColumnSel;
    Ed.OptScrollLineCommandsKeepCaretOnScreen:= Op.OpCaretKeepVisibleOnScroll;

    Ed.OptShowCurLine:= Op.OpShowCurLine;
    Ed.OptShowCurLineMinimal:= Op.OpShowCurLineMinimal;
    Ed.OptShowCurLineOnlyFocused:= Op.OpShowCurLineOnlyFocused;
    Ed.OptShowCurColumn:= Op.OpShowCurCol;
    Ed.OptLastLineOnTop:= Op.OpShowLastLineOnTop;
  end;

  Ed.OptShowFullWidthForSelection:= Op.OpShowFullBackgroundSel;
  Ed.OptShowFullWidthForSyntaxHilite:= Op.OpShowFullBackgroundSyntax;
  Ed.OptShowMouseSelFrame:= Op.OpShowMouseSelFrame;
  Ed.OptCopyLinesIfNoSel:= Op.OpCopyLineIfNoSel;
  Ed.OptCutLinesIfNoSel:= Op.OpCutLineIfNoSel;
  Ed.OptCopyColumnBlockAlignedBySpaces:= Op.OpCopyColumnAlignedBySpaces;
  Ed.OptSavingTrimSpaces:= Op.OpSavingTrimSpaces;
  Ed.OptSavingTrimFinalEmptyLines:= Op.OpSavingTrimFinalEmptyLines;
  Ed.OptSavingForceFinalEol:= Op.OpSavingForceFinalEol;
  Ed.OptShowScrollHint:= Op.OpShowHintOnVertScroll;
  Ed.OptScrollSmooth:= Op.OpSmoothScroll;
  Ed.OptScrollStyleHorz:= TATEditorScrollbarStyle(Op.OpScrollStyleHorz);
  Ed.OptTextCenteringCharWidth:= IfThen(AApplyCentering, Op.OpCenteringWidth, 0);
  Ed.OptNonWordChars:= Op.OpNonWordChars;

  if not AOneLiner then
  begin
    Ed.OptFoldStyle:= TATEditorFoldStyle(Op.OpFoldStyle);
    Ed.OptFoldTooltipVisible:= Op.OpFoldTooltipShow;

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
    if Op.OpIndentAutoKind<=Ord(High(TATEditorAutoIndentKind)) then
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

  Ed.OptMouse2ClickDragSelectsWords:= Op.OpMouse2ClickDragSelectsWords;
  Ed.OptMouseDragDrop:= Op.OpMouseDragDrop;
  ATSynEdit.OptMouseDragDropFocusesTargetEditor:= Op.OpMouseDragDropFocusTarget;
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
  if Op.OpKeyPageUpDownSize<=Ord(High(TATEditorPageDownSize)) then
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
  Ed.OptBorderWidthFocused:= AppScale(EditorOps.OpActiveBorderWidth);
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

function EditorFormatStatus(ed: TATSynEdit; const str: string): string;
var
  caret: TATCaretItem;
  cols, n, x_b, y_b, x_e, y_e: integer;
  bSel: boolean;
  char_str, temp_str: UnicodeString;
  char_code: integer;
  ch: WideChar;
begin
  result:= '';
  if ed.Carets.Count=0 then exit;
  caret:= ed.Carets[0];

  caret.GetRange(x_b, y_b, x_e, y_e, bSel);

  //make {cols} work for column-sel and small-sel
  cols:= 0;
  //column-sel?
  if not ed.IsSelRectEmpty then
    cols:= ed.SelRect.Right-ed.SelRect.Left
  else
  //small-sel?
  if (ed.Carets.Count=1) and (caret.PosY=caret.EndY) then
    cols:= Abs(caret.PosX-caret.EndX);

  result:= str;
  result:= StringReplace(result, '{x}', inttostr(caret.PosX+1), []);
  result:= StringReplace(result, '{y}', inttostr(caret.PosY+1), []);
  result:= StringReplace(result, '{y2}', inttostr(ed.carets[ed.carets.count-1].PosY+1), []);
  result:= StringReplace(result, '{yb}', inttostr(y_b+1), []);
  result:= StringReplace(result, '{ye}', inttostr(y_e+1), []);
  result:= StringReplace(result, '{count}', inttostr(ed.strings.count), []);
  result:= StringReplace(result, '{carets}', inttostr(ed.carets.count), []);
  result:= StringReplace(result, '{cols}', inttostr(cols), []);

  result:= StringReplace(result, '{_ln}', msgStatusbarTextLine, []);
  result:= StringReplace(result, '{_col}', msgStatusbarTextCol, []);
  result:= StringReplace(result, '{_sel}', msgStatusbarTextSel, []);
  result:= StringReplace(result, '{_linesel}', msgStatusbarTextLinesSel, []);
  result:= StringReplace(result, '{_carets}', msgStatusbarTextCarets, []);

  if pos('{sel}', result)>0 then
    result:= StringReplace(result, '{sel}', inttostr(EditorGetSelLinesCount(ed)), []);

  if pos('{selchars}', result)>0 then
    result:= StringReplace(result, '{selchars}', inttostr(EditorGetSelCharsCount(ed)), []);

  if pos('{xx}', result)>0 then
    if ed.Strings.IsIndexValid(caret.PosY) then
    begin
      //optimized for huge lines
      n:= ed.Strings.CharPosToColumnPos(caret.PosY, caret.PosX, ed.TabHelper)+1;
      result:= StringReplace(result, '{xx}', inttostr(n), []);
    end;

  if pos('{char', result)>0 then
  begin
    char_str:= '';
    char_code:= -1;

    if ed.Strings.IsIndexValid(y_b) then
      if (x_b>=0) and (x_b<ed.Strings.LinesLen[y_b]) then
      begin
        ch:= ed.Strings.LineCharAt(y_b, x_b+1);
        if ch<>#0 then
        begin
          char_str:= ch;
          char_code:= Ord(ch);
        end;
      end;

    result:= StringReplace(result, '{char}', char_str, []);

    if char_code>=0 then
      temp_str:= IntToStr(char_code)
    else
      temp_str:= '';
    result:= StringReplace(result, '{char_dec}', temp_str, []);

    if char_code>=0 then
      temp_str:= IntToHex(char_code, 2)
    else
      temp_str:= '';
    result:= StringReplace(result, '{char_hex}', temp_str, []);

    if char_code>=0 then
      temp_str:= IntToHex(char_code, 4)
    else
      temp_str:= '';
    result:= StringReplace(result, '{char_hex4}', temp_str, []);
  end;
end;

procedure EditorDeleteNewColorAttribs(ed: TATSynEdit);
begin
  ed.Attribs.Clear;
  ed.Update;
end;

function EditorGetStatusType(ed: TATSynEdit): TEdSelType;
var
  NFrom, NTo: integer;
begin
  if not Ed.IsSelRectEmpty then
    result:= selCol
  else
  if Ed.Carets.Count>1 then
    result:= selCarets
  else
  if Ed.Carets.IsSelection then
  begin
    Ed.Carets[0].GetSelLines(NFrom, NTo);
    if NTo>NFrom then
      result:= selStream
    else
      result:= selSmall;
  end
  else
    result:= selNo;
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
  Ed.Colors.CurrentLineBG:= GetAppColor(apclEdCurLineBg);
  Ed.Colors.IndentVertLines:= GetAppColor(apclEdIndentVLine);
  Ed.Colors.UnprintedFont:= GetAppColor(apclEdUnprintFont);
  Ed.Colors.UnprintedBG:= GetAppColor(apclEdUnprintBg);
  Ed.Colors.UnprintedHexFont:= GetAppColor(apclEdUnprintHexFont);
  Ed.Colors.MinimapBorder:= GetAppColor(apclEdMinimapBorder);
  Ed.Colors.MinimapSelBG:= GetAppColor(apclEdMinimapSelBg);
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
  if Id='EdTextFont' then Ed.Colors.TextFont:= AColor else
  if Id='EdTextBg' then Ed.Colors.TextBG:= AColor else
  if Id='EdSelFont' then Ed.Colors.TextSelFont:= AColor else
  if Id='EdSelBg' then Ed.Colors.TextSelBG:= AColor else
  if Id='EdDisableFont' then Ed.Colors.TextDisabledFont:= AColor else
  if Id='EdDisableBg' then Ed.Colors.TextDisabledBG:= AColor else
  if Id='EdCaret' then Ed.Colors.Caret:= AColor else
  if Id='EdMarkers' then Ed.Colors.Markers:= AColor else
  if Id='EdCurLineBg' then Ed.Colors.CurrentLineBG:= AColor else
  if Id='EdIndentVLine' then Ed.Colors.IndentVertLines:= AColor else
  if Id='EdUnprintFont' then Ed.Colors.UnprintedFont:= AColor else
  if Id='EdUnprintBg' then Ed.Colors.UnprintedBG:= AColor else
  if Id='EdUnprintHexFont' then Ed.Colors.UnprintedHexFont:= AColor else
  if Id='EdMinimapBorder' then Ed.Colors.MinimapBorder:= AColor else
  if Id='EdMinimapSelBg' then Ed.Colors.MinimapSelBG:= AColor else
  if Id='EdMinimapTooltipBg' then Ed.Colors.MinimapTooltipBG:= AColor else
  if Id='EdMinimapTooltipBorder' then Ed.Colors.MinimapTooltipBorder:= AColor else
  if Id='EdStateChanged' then Ed.Colors.StateChanged:= AColor else
  if Id='EdStateAdded' then Ed.Colors.StateAdded:= AColor else
  if Id='EdStateSaved' then Ed.Colors.StateSaved:= AColor else
  if Id='EdBlockStaple' then Ed.Colors.BlockStaple:= AColor else
  if Id='EdBlockStapleActive' then Ed.Colors.BlockStapleForCaret:= AColor else
  if Id='EdBlockSepLine' then Ed.Colors.BlockSepLine:= AColor else
  if Id='EdLinks' then Ed.Colors.Links:= AColor else
  if Id='EdLockedBg' then Ed.Colors.LockedBG:= AColor else
  if Id='EdComboArrow' then Ed.Colors.ComboboxArrow:= AColor else
  if Id='EdComboArrowBg' then Ed.Colors.ComboboxArrowBG:= AColor else
  if Id='EdFoldMarkLine' then Ed.Colors.CollapseLine:= AColor else
  if Id='EdFoldMarkFont' then Ed.Colors.CollapseMarkFont:= AColor else
  if Id='EdFoldMarkBorder' then Ed.Colors.CollapseMarkBorder:= AColor else
  if Id='EdFoldMarkBg' then Ed.Colors.CollapseMarkBG:= AColor else
  if Id='EdGutterFont' then Ed.Colors.GutterFont:= AColor else
  if Id='EdGutterBg' then Ed.Colors.GutterBG:= AColor else
  if Id='EdGutterCaretFont' then Ed.Colors.GutterCaretFont:= AColor else
  if Id='EdGutterCaretBg' then Ed.Colors.GutterCaretBG:= AColor else
  if Id='EdBookmarkBg' then Ed.Colors.BookmarkBG:= AColor else
  if Id='EdRulerFont' then Ed.Colors.RulerFont:= AColor else
  if Id='EdRulerBg' then Ed.Colors.RulerBG:= AColor else
  if Id='EdFoldLine' then Ed.Colors.GutterFoldLine:= AColor else
  if Id='EdFoldBg' then Ed.Colors.GutterFoldBG:= AColor else
  if Id='EdMarginFixed' then Ed.Colors.MarginRight:= AColor else
  if Id='EdMarginCaret' then Ed.Colors.MarginCaret:= AColor else
  if Id='EdMarginUser' then Ed.Colors.MarginUser:= AColor else
  if Id='EdMarkedRangeBg' then Ed.Colors.MarkedLinesBG:= AColor else
  if Id='EdBorder' then Ed.Colors.BorderLine:= AColor else
  if Id='EdBorderFocused' then Ed.Colors.BorderLineFocused:= AColor else
  ;
end;


function EditorGetColorById(Ed: TATSynEdit; const Id: string): TColor;
begin
  Result:= -1;
  if Id='EdTextFont' then exit(Ed.Colors.TextFont);
  if Id='EdTextBg' then exit(Ed.Colors.TextBG);
  if Id='EdSelFont' then exit(Ed.Colors.TextSelFont);
  if Id='EdSelBg' then exit(Ed.Colors.TextSelBG);
  if Id='EdDisableFont' then exit(Ed.Colors.TextDisabledFont);
  if Id='EdDisableBg' then exit(Ed.Colors.TextDisabledBG);
  if Id='EdCaret' then exit(Ed.Colors.Caret);
  if Id='EdMarkers' then exit(Ed.Colors.Markers);
  if Id='EdCurLineBg' then exit(Ed.Colors.CurrentLineBG);
  if Id='EdIndentVLine' then exit(Ed.Colors.IndentVertLines);
  if Id='EdUnprintFont' then exit(Ed.Colors.UnprintedFont);
  if Id='EdUnprintBg' then exit(Ed.Colors.UnprintedBG);
  if Id='EdUnprintHexFont' then exit(Ed.Colors.UnprintedHexFont);
  if Id='EdMinimapBorder' then exit(Ed.Colors.MinimapBorder);
  if Id='EdMinimapSelBg' then exit(Ed.Colors.MinimapSelBG);
  if Id='EdMinimapTooltipBg' then exit(Ed.Colors.MinimapTooltipBG);
  if Id='EdMinimapTooltipBorder' then exit(Ed.Colors.MinimapTooltipBorder);
  if Id='EdStateChanged' then exit(Ed.Colors.StateChanged);
  if Id='EdStateAdded' then exit(Ed.Colors.StateAdded);
  if Id='EdStateSaved' then exit(Ed.Colors.StateSaved);
  if Id='EdBlockStaple' then exit(Ed.Colors.BlockStaple);
  if Id='EdBlockStapleActive' then exit(Ed.Colors.BlockStapleForCaret);
  if Id='EdBlockSepLine' then exit(Ed.Colors.BlockSepLine);
  if Id='EdLinks' then exit(Ed.Colors.Links);
  if Id='EdLockedBg' then exit(Ed.Colors.LockedBG);
  if Id='EdComboArrow' then exit(Ed.Colors.ComboboxArrow);
  if Id='EdComboArrowBg' then exit(Ed.Colors.ComboboxArrowBG);
  if Id='EdFoldMarkLine' then exit(Ed.Colors.CollapseLine);
  if Id='EdFoldMarkFont' then exit(Ed.Colors.CollapseMarkFont);
  if Id='EdFoldMarkBorder' then exit(Ed.Colors.CollapseMarkBorder);
  if Id='EdFoldMarkBg' then exit(Ed.Colors.CollapseMarkBG);
  if Id='EdGutterFont' then exit(Ed.Colors.GutterFont);
  if Id='EdGutterBg' then exit(Ed.Colors.GutterBG);
  if Id='EdGutterCaretFont' then exit(Ed.Colors.GutterCaretFont);
  if Id='EdGutterCaretBg' then exit(Ed.Colors.GutterCaretBG);
  if Id='EdBookmarkBg' then exit(Ed.Colors.BookmarkBG);
  if Id='EdRulerFont' then exit(Ed.Colors.RulerFont);
  if Id='EdRulerBg' then exit(Ed.Colors.RulerBG);
  if Id='EdFoldLine' then exit(Ed.Colors.GutterFoldLine);
  if Id='EdFoldBg' then exit(Ed.Colors.GutterFoldBG);
  if Id='EdMarginFixed' then exit(Ed.Colors.MarginRight);
  if Id='EdMarginCaret' then exit(Ed.Colors.MarginCaret);
  if Id='EdMarginUser' then exit(Ed.Colors.MarginUser);
  if Id='EdMarkedRangeBg' then exit(Ed.Colors.MarkedLinesBG);
  if Id='EdBorder' then exit(Ed.Colors.BorderLine);
  if Id='EdBorderFocused' then exit(Ed.Colors.BorderLineFocused);
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
  Pnt: TPoint;
  Details: TATEditorPosDetails;
  Caret: TATCaretItem;
begin
  if Ed.Carets.Count=0 then exit;
  Caret:= Ed.Carets[0];

  Pnt:= Mouse.CursorPos;
  Pnt:= Ed.ScreenToClient(Pnt);
  Pnt:= Ed.ClientPosToCaretPos(Pnt, Details);

  Ed.DoCaretSingle(
    Pnt.X,
    Pnt.Y,
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
  Details: TATEditorPosDetails;
begin
  Result:= '';
  P:= Ed.ScreenToClient(P);
  P:= Ed.ClientPosToCaretPos(P, Details);
  Result:= Ed.DoGetLinkAtPos(P.X, P.Y);
  if SBeginsWith(Result, 'www') then
    Result:= 'http://'+Result;
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


function Editor_NextCharAllowed_AutoCloseBracket(ch: char): boolean;
begin
  Result:= Pos(ch, ' ])};:.,=>'#9)>0;
end;


function EditorAutoSkipClosingBracket(Ed: TATSynEdit; CharClosing: char): boolean;
var
  Caret: TATCaretItem;
  CharOpening: char;
  Str: UnicodeString;
  iCaret: integer;
begin
  Result:= false;

  CharOpening:= EditorBracket_GetPairForClosingBracketOrQuote(CharClosing);
  if CharOpening=#0 then exit;
  if Pos(CharOpening, Ed.OptAutoPairChars)=0 then exit;

  for iCaret:= Ed.Carets.Count-1 downto 0 do
  begin
    Caret:= Ed.Carets[iCaret];
    //improve auto-closing brackets, avoid duplicate )]}
    //when closing bracket )]} is typed over itself,
    //and previous char is opening bracket ([{
    if Ed.Strings.IsIndexValid(Caret.PosY) then
    begin
      Str:= Ed.Strings.Lines[Caret.PosY];
      if (Caret.PosX<Length(Str)) then
       if Str[Caret.PosX+1]=CharClosing then
        if (Caret.PosX>0) and (Str[Caret.PosX]=CharOpening) then //only if previous is ([{
        begin
          Caret.Change(Caret.PosX+1, Caret.PosY, -1, -1);
          Result:= true;
        end;
    end;
  end;
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

  //bad context: caret is just before a word-char
  if (NPos<Length(Str)) and
    not Editor_NextCharAllowed_AutoCloseBracket(Str[NPos+1]) then
      exit(false);
end;

function EditorAutoPairChar(Ed: TATSynEdit; CharBegin: atChar): boolean;
var
  Caret: TATCaretItem;
  X1, Y1, X2, Y2: integer;
  NCaret: integer;
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

  //make additional loop, to detect that ALL carets need pairing.
  //if at least one caret doesn't need, stop.
  //it's needed to make pair for all or nothing. issue #3219.
  for NCaret:= Ed.Carets.Count-1 downto 0 do
  begin
    Caret:= Ed.Carets[NCaret];
    if not Ed.Strings.IsIndexValid(Caret.PosY) then Continue;
    if Caret.EndY<0 then
      if not EditorAutoCloseBracket_NeedPair(Ed, Caret, bQuoteChar) then
        exit;
  end;

  //cancel vertical selection
  Ed.DoSelect_ClearColumnBlock;

  Ed.Strings.BeginUndoGroup;
  for NCaret:= Ed.Carets.Count-1 downto 0 do
  begin
    Caret:= Ed.Carets[NCaret];
    if not Ed.Strings.IsIndexValid(Caret.PosY) then Continue;
    Caret.GetRange(X1, Y1, X2, Y2, bSel);
    bBackwardSel:= not Caret.IsForwardSelection;

    if not bSel then
      if not EditorAutoCloseBracket_NeedPair(Ed, Caret, bQuoteChar) then Continue;

    if not bSel then
    begin
      Ed.Strings.TextInsert(X1, Y1, CharBegin+CharEnd, false, Shift, PosAfter);
      Ed.DoCaretsShift(NCaret, X1, Y1, Shift.X, Shift.Y, PosAfter);

      Caret.PosX:= Caret.PosX+1;
      Caret.EndX:= -1;
      Caret.EndY:= -1;
    end
    else
    begin
      Ed.Strings.TextInsert(X2, Y2, CharEnd, false, Shift, PosAfter);
      Ed.DoCaretsShift(NCaret, X2, Y2, Shift.X, Shift.Y, PosAfter);

      Ed.Strings.TextInsert(X1, Y1, CharBegin, false, Shift, PosAfter);
      Ed.DoCaretsShift(NCaret, X1, Y1, Shift.X, Shift.Y, PosAfter);

      Caret.EndX:= X1+1;
      Caret.EndY:= Y1;
      Caret.PosX:= X2+IfThen(Y1=Y2, 1);
      Caret.PosY:= Y2;

      if bBackwardSel then
        Caret.SwapSelection;
    end;

    Result:= true;
  end;
  Ed.Strings.EndUndoGroup;

  if Result then
  begin
    Ed.Modified:= true;
    Ed.Update(true);
  end;
end;


procedure EditorFocus(C: TWinControl);
var
  Form: TCustomForm;
begin
  try
    Form:= GetParentForm(C);
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
  NumCount, NumLine, NumCol: integer;
  Pnt: TPoint;
  bExtend: boolean;
  Caret: TATCaretItem;
  Sep: TATStringSeparator;
begin
  NumCount:= Ed.Strings.Count;
  if NumCount<2 then exit(false);

  bExtend:= SEndsWith(SInput, '+');
  if bExtend then
    SetLength(SInput, Length(SInput)-1);

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
    Pnt:= Ed.Strings.OffsetToPosition(
      StrToIntDef(Copy(SInput, 2, MaxInt), -1));
    NumLine:= Pnt.Y;
    NumCol:= Pnt.X;
  end
  else
  if SBeginsWith(SInput, 'x') then
  begin
    Pnt:= Ed.Strings.OffsetToPosition(
      StrToIntDef('$'+Copy(SInput, 2, MaxInt), -1));
    NumLine:= Pnt.Y;
    NumCol:= Pnt.X;
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
  Props.EmptyInside:= S='_';
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
      if EditorGetTokenKind(Ed, iChar, iLine)<>atkOther then Continue;

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
        if (ch=CharFrom) and (EditorGetTokenKind(Ed, IndexX, IndexY)=atkOther) then
          Inc(Level)
        else
        if (ch=CharTo) and (EditorGetTokenKind(Ed, IndexX, IndexY)=atkOther) then
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
        if (ch=CharFrom) and (EditorGetTokenKind(Ed, IndexX, IndexY)=atkOther) then
          Inc(Level)
        else
        if (ch=CharTo) and (EditorGetTokenKind(Ed, IndexX, IndexY)=atkOther) then
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

procedure EditorBracket_ClearHilite(Ed: TATSynEdit);
begin
  Ed.Attribs.DeleteWithTag(cEditorTagForBracket);
  Ed.GutterDecor.DeleteByTag(cEditorTagForBracket);
end;

procedure EditorBracket_FindBoth(Ed: TATSynEdit;
  var PosX, PosY: integer;
  const AllowedSymbols: string;
  MaxDistance: integer;
  out FoundX, FoundY: integer;
  out CharFrom, CharTo: atChar;
  out Kind: TATEditorBracketKind);
var
  S: atString;
begin
  FoundX:= -1;
  FoundY:= -1;
  CharFrom:= #0;
  CharTo:= #0;
  Kind:= bracketUnknown;

  if PosX<0 then exit;
  if not Ed.Strings.IsIndexValid(PosY) then exit;

  S:= Ed.Strings.Lines[PosY];
  if (PosX=Length(S)) and (PosX>0) then
    Dec(PosX);

  if PosX<Length(S) then
  begin
    CharFrom:= S[PosX+1];
    if Pos(CharFrom, AllowedSymbols)>0 then
      if EditorGetTokenKind(Ed, PosX, PosY)=atkOther then
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
        if EditorGetTokenKind(Ed, PosX, PosY)=atkOther then
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
      S:= Ed.Strings.Lines[PosY];
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
  PartObj: TATLinePartClass;
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
        PartObj:= TATLinePartClass.Create;
        ApplyPartStyleFromEcontrolStyle(PartObj.Data, GetAppStyle(apstBracketBG));
        Ed.Attribs.Add(PosX, PosY, cEditorTagForBracket, 1, 0, PartObj);

        PartObj:= TATLinePartClass.Create;
        ApplyPartStyleFromEcontrolStyle(PartObj.Data, GetAppStyle(apstBracketBG));
        Ed.Attribs.Add(FoundX, FoundY, cEditorTagForBracket, 1, 0, PartObj);

        FillChar(Decor, SizeOf(Decor), 0);
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
          Decor.Text:= CharFrom;
          Ed.GutterDecor.Add(Decor);

          Decor.LineNum:= FoundY;
          Decor.Text:= CharTo;
          Ed.GutterDecor.Add(Decor);
        end
        else
        begin
          Decor.LineNum:= PosY;
          if Kind=bracketOpening then
            Decor.Text:= CharFrom+CharTo
          else
            Decor.Text:= CharTo+CharFrom;
          Ed.GutterDecor.Add(Decor);
        end;
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
  Ops.FontSize:= Ed.Font.Size;
  Ops.WrapMode:= Ed.OptWrapMode;
  Ops.ShowMinimap:= Ed.OptMinimapVisible;
  Ops.ShowMicromap:= Ed.OptMicromapVisible;
  Ops.ShowRuler:= Ed.OptRulerVisible;
  Ops.ShowNumbers:= Ed.Gutter.Items[Ed.GutterBandNumbers].Visible;
  Ops.ShowFolding:= Ed.Gutter.Items[Ed.GutterBandFolding].Visible;
  Ops.ShowUnprinted:= Ed.OptUnprintedVisible;

  Ops.UnprintedSpaces:= Ed.OptUnprintedSpaces;
  Ops.UnprintedSpacesTrail:= Ed.OptUnprintedSpacesTrailing;
  Ops.UnprintedSpacesInSel:= Ed.OptUnprintedSpacesOnlyInSelection;
  Ops.UnprintedEnds:= Ed.OptUnprintedEnds;
  Ops.UnprintedEndsDetails:= Ed.OptUnprintedEndsDetails;
end;

procedure EditorRestoreTempOptions(Ed: TATSynEdit; const ANew, AOld: TATEditorTempOptions);
begin
  if AOld.FontSize<>ANew.FontSize then
    Ed.Font.Size:= ANew.FontSize;
  if AOld.WrapMode<>ANew.WrapMode then
    Ed.OptWrapMode:= ANew.WrapMode;
  if AOld.ShowMinimap<>ANew.ShowMinimap then
    Ed.OptMinimapVisible:= ANew.ShowMinimap;
  if AOld.ShowMicromap<>ANew.ShowMicromap then
    Ed.OptMicromapVisible:= ANew.ShowMicromap;
  if AOld.ShowRuler<>ANew.ShowRuler then
    Ed.OptRulerVisible:= ANew.ShowRuler;
  if AOld.ShowNumbers<>ANew.ShowNumbers then
    Ed.Gutter.Items[Ed.GutterBandNumbers].Visible:= ANew.ShowNumbers;
  if AOld.ShowFolding<>ANew.ShowFolding then
    Ed.Gutter.Items[Ed.GutterBandFolding].Visible:= ANew.ShowFolding;
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

function EditorGetTokenKind(Ed: TATSynEdit; AX, AY: integer): TATTokenKind;
var
  NLen: integer;
begin
  Result:= atkOther;
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
    Result:= TATAdapterEControl(Ed.AdapterForHilite).GetTokenKindAtPos(Point(AX, AY))
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

function EditorExpandSelectionToWord(Ed: TATSynEdit): boolean;
var
  Caret: TATCaretItem;
  Finder: TATEditorFinder;
  X1, Y1, X2, Y2: integer;
  bSel, bForwardSel: boolean;
  bWholeWord: boolean;
  sText: UnicodeString;
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

      Finder.OptWords:= bWholeWord;
      Finder.OptRegex:= false;
      Finder.OptCase:= true;
      Finder.OptBack:= false;
      Finder.OptFromCaret:= false;
      Finder.OptInSelection:= false;
      Finder.OptTokens:= cTokensAll;

      Result:= Finder.DoAction_FindSimple(Point(X2, Y2));

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
        Ed.DoCommand(cCommand_ScrollToCaretBottom);
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
    Ed.DoCommand(cCommand_SelectWords);
  end
  else
  begin
    Str:= Ed.TextSelected;
    if Str='' then
    begin
      Str:= Ed.TextCurrentWord;
      Ed.DoCommand(cCommand_SelectWords);
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
  AScroll: boolean; out AMatchesCount: integer);
var
  ColorBorder: TColor;
  StyleBorder: TATLineStyle;
  bMoveCaret: boolean;
begin
  ColorBorder:= GetAppStyle(AppHiAll_ThemeStyleId).BgColor;

  if EditorOps.OpActiveBorderWidth>1 then
    StyleBorder:= cLineStyleSolid2px
  else
    StyleBorder:= cLineStyleRounded;

  bMoveCaret:= UiOps.FindHiAll_MoveCaret and not AFinder.Editor.Carets.IsSelection;

  AMatchesCount:= AFinder.DoAction_HighlightAllEditorMatches(
    ColorBorder,
    StyleBorder,
    UiOps.FindHiAll_TagValue,
    UiOps.FindHiAll_MaxLines,
    AScroll,
    bMoveCaret
    );
end;


function EditorIsEmpty(Ed: TATSynEdit): boolean;
var
  Str: TATStrings;
begin
  //dont check Modified here
  Str:= Ed.Strings;
  Result:=
    (Str.Count=0) or ((Str.Count=1) and (Str.Lines[0]=''));
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
  PartObj: TATLinePartClass;
  Part: TATLinePart;
  i: integer;
begin
  if Length(AX)=0 then exit;

  FillChar(Part, SizeOf(Part), 0);
  ApplyPartStyleFromEcontrolStyle(Part, GetAppStyle(AStyle));
  Part.ColorBG:= clNone;

  for i:= 0 to High(AX) do
  begin
    PartObj:= TATLinePartClass.Create;
    PartObj.Data:= Part;
    Ed.Attribs.Add(AX[i], AY, ATag, 1, 0, PartObj);
  end;

  Ed.Invalidate;
end;

procedure EditorHighlightBadRegexBrackets(Ed: TATSynEdit; AOnlyClear: boolean);
const
  cBadRegexTag = 10;
var
  Bads: TATIntArray;
  OpenedRound: TATIntArray;
  OpenedSquare: TATIntArray;
  LevelRound, LevelSquare: integer;
  S: UnicodeString;
  ch: WideChar;
  i: integer;
begin
  Ed.Attribs.DeleteWithTag(cBadRegexTag);
  if AOnlyClear then exit;

  S:= Ed.Text;
  SetLength(Bads, 0);
  SetLength(OpenedRound, 0);
  SetLength(OpenedSquare, 0);
  LevelRound:= 0;
  LevelSquare:= 0;
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
        Continue;
      end;

    if ch=']' then
    begin
      if LevelSquare<1 then
        AddArrayItem(Bads, i);
      if LevelSquare>0 then
        Dec(LevelSquare);
      DeleteArrayLastItem(OpenedSquare);
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
  Ed.OptWrapMode:= cWrapOff;
  Ed.OptMicromapVisible:= false;
  Ed.OptMinimapVisible:= false;
end;

function EditorSaveFileAs(Ed: TATSynEdit; const AFileName: string): boolean;
  //
  procedure DoSave;
  begin
    {
    //atomic file saving is NOT a good thing, people write they loose file properties,
    //and even loose data
    if UiOps.AtomicFileSave then
      ....
    else
    }
      Ed.SaveToFile(AFileName);
  end;
  //
var
  OldEncoding: string;
  OldAttr: Longint;
begin
  Result:= true;
  while true do
  try
    AppFileAttrPrepare(AFileName, OldAttr);
    Ed.BeginUpdate;
    try
      try
        DoSave;
      except
        on E: EConvertError do
          begin
            OldEncoding:= Ed.EncodingName;
            Ed.EncodingName:= cEncNameUtf8_NoBom;
            DoSave;
            MsgBox(Format(msgCannotSaveFileWithEnc, [OldEncoding]), MB_OK or MB_ICONWARNING);
          end
        else
          raise;
      end;
    finally
      Ed.EndUpdate;
    end;
    AppFileAttrRestore(AFileName, OldAttr);
    Break;
  except
    if MsgBox(msgCannotSaveFile+#10+AFileName,
      MB_RETRYCANCEL or MB_ICONERROR) = IDCANCEL then
    begin
      Result:= false;
      Break;
    end;
  end;
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

end.

