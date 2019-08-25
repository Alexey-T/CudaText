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
  Controls,
  Dialogs, Forms,
  Clipbrd,
  ATSynEdit,
  ATSynEdit_CanvasProc,
  ATSynEdit_Carets,
  ATSynEdit_Markers,
  ATSynEdit_Ranges,
  ATSynEdit_Commands,
  ATSynEdit_CharSizer,
  ATSynEdit_Edits,
  ATStrings,
  ATStringProc,
  proc_globdata,
  proc_colors,
  proc_msg,
  math;

type
  TATEditorTempOps = record
    FontSize: integer;
    WrapMode: TATSynWrapMode;
    ShowMinimap: boolean;
    ShowMicromap: boolean;
    ShowRuler: boolean;
    ShowNumbers: boolean;
    ShowUnprinted: boolean;
  end;

procedure EditorSaveTempOptions(Ed: TATSynEdit; var Ops: TATEditorTempOps);
procedure EditorRestoreTempOptions(Ed: TATSynEdit; const Ops: TATEditorTempOps);
procedure EditorFocus(C: TWinControl);
procedure EditorMouseClick_AtCursor(Ed: TATSynEdit; AAndSelect: boolean);
procedure EditorMouseClick_NearCaret(Ed: TATSynEdit; AParams: string; AAndSelect: boolean);

procedure EditorClear(Ed: TATSynEdit);
function EditorGetCurrentChar(Ed: TATSynEdit): Widechar;
procedure EditorApplyOps(Ed: TATSynEdit; const Op: TEditorOps;
  AApplyUnprintedAndWrap, AApplyTabSize, AApplyCentering: boolean);

function EditorGetFoldString(Ed: TATSynEdit): string;
procedure EditorSetFoldString(Ed: TATSynEdit; S: string);

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
function EditorAutoCloseBracket(Ed: TATSynEdit; CharBegin: atChar): boolean;
function EditorGetPairForCloseBracket(ch: char): char;

procedure EditorCaretPropsFromString(Props: TATCaretProps; S: string);
procedure EditorCaretPropsFromPyTuple(Props: TATCaretProps; S: string);


implementation

procedure EditorApplyOps(Ed: TATSynEdit; const Op: TEditorOps;
  AApplyUnprintedAndWrap, AApplyTabSize, AApplyCentering: boolean);
var
  S: string;
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

  Ed.OptCharSpacingY:= Op.OpSpacingY;

  if AApplyTabSize then
  begin
    Ed.OptTabSize:= Op.OpTabSize;
    Ed.OptTabSpaces:= Op.OpTabSpaces;
  end;

  Ed.OptOverwriteSel:= Op.OpOverwriteSel;
  Ed.OptOverwriteAllowedOnPaste:= Op.OpOverwriteOnPaste;

  Ed.OptGutterVisible:= Op.OpGutterShow;
  Ed.OptGutterShowFoldAlways:= Op.OpGutterFoldAlways;
  Ed.OptGutterIcons:= TATGutterIconsKind(Op.OpGutterFoldIcons);
  Ed.Gutter[Ed.GutterBandBookmarks].Visible:= Op.OpGutterBookmarks;
  Ed.Gutter[Ed.GutterBandFolding].Visible:= Op.OpGutterFold;
  Ed.Gutter[Ed.GutterBandNumbers].Visible:= Op.OpNumbersShow;
  Ed.Gutter.Update;

  if Op.OpNumbersStyle<=Ord(High(TATSynNumbersStyle)) then
    Ed.OptNumbersStyle:= TATSynNumbersStyle(Op.OpNumbersStyle);
  Ed.OptNumbersShowCarets:= Op.OpNumbersForCarets;
  if Op.OpNumbersCenter then
    Ed.OptNumbersAlignment:= taCenter
  else
    Ed.OptNumbersAlignment:= taRightJustify;

  Ed.OptRulerVisible:= Op.OpRulerShow;
  Ed.OptRulerNumeration:= TATRulerNumeration(Op.OpRulerNumeration);
  Ed.OptRulerMarkSizeCaret:= Op.OpRulerMarkCaret;

  Ed.OptMinimapVisible:= Op.OpMinimapShow;
  Ed.OptMinimapShowSelAlways:= Op.OpMinimapShowSelAlways;
  Ed.OptMinimapShowSelBorder:= Op.OpMinimapShowSelBorder;
  Ed.OptMinimapCharWidth:= Op.OpMinimapCharWidth;
  Ed.OptMinimapAtLeft:= Op.OpMinimapAtLeft;
  Ed.OptMinimapTooltipVisible:= Op.OpMinimapTooltipShow;
  Ed.OptMinimapTooltipLinesCount:= Op.OpMinimapTooltipLineCount;
  Ed.OptMinimapTooltipWidthPercents:= Op.OpMinimapTooltipWidth;

  Ed.OptMicromapVisible:= Op.OpMicromapShow;

  Ed.OptMarginRight:= Op.OpMarginFixed;
  Ed.OptMarginString:= Op.OpMarginString;

  Ed.OptShowURLs:= Op.OpLinks;
  Ed.OptShowURLsRegex:= Op.OpLinksRegex;

  if AApplyUnprintedAndWrap then
  begin
    Ed.OptUnprintedVisible:= Op.OpUnprintedShow;
    Ed.OptUnprintedSpaces:=         Pos('s', Op.OpUnprintedContent)>0;
    Ed.OptUnprintedSpacesTrailing:= Pos('t', Op.OpUnprintedContent)>0;
    Ed.OptUnprintedSpacesBothEnds:= Pos('l', Op.OpUnprintedContent)>0;
    Ed.OptUnprintedEnds:=           Pos('e', Op.OpUnprintedContent)>0;
    Ed.OptUnprintedEndsDetails:=    Pos('d', Op.OpUnprintedContent)>0;
  end;

  //global options
  OptMaxTabPositionToExpand:= Op.OpTabMaxPosExpanded;
  OptHexChars:= OptHexCharsDefault + Op.OpHexChars;

  OptUnprintedEndArrowOrDot:= Pos('.', Op.OpUnprintedContent)=0;
  OptUnprintedTabCharLength:= Op.OpUnprintedTabArrowLen;
  OptUnprintedSpaceDotScale:= Op.OpUnprintedSpaceDotScale;
  OptUnprintedEndDotScale:= Op.OpUnprintedEndDotScale;
  OptUnprintedEndFontScale:= Op.OpUnprintedEndFontScale;
  OptUnprintedTabPointerScale:= Op.OpUnprintedTabPointerScale;
  OptUnprintedReplaceSpec:= Op.OpUnprintedReplaceSpec;
  OptUnprintedReplaceSpecToCode:= StrToInt('$'+Op.OpUnprintedReplaceToCode);

  if AApplyUnprintedAndWrap then
  begin
    if Op.OpWrapMode<=Ord(High(TATSynWrapMode)) then
      Ed.OptWrapMode:= TATSynWrapMode(Op.OpWrapMode);
  end;
  Ed.OptWrapIndented:= Op.OpWrapIndented;
  Ed.OptWrapEnabledForMaxLines:= Op.OpWrapEnabledMaxLines;

  Ed.OptUndoLimit:= Op.OpUndoLimit;
  Ed.OptUndoGrouped:= Op.OpUndoGrouped;
  Ed.OptUndoAfterSave:= Op.OpUndoAfterSave;

  Ed.OptCaretBlinkTime:= Op.OpCaretBlinkTime;
  Ed.OptCaretBlinkEnabled:= Op.OpCaretBlinkEn;

  EditorCaretPropsFromString(Ed.CaretPropsNormal, Op.OpCaretViewNormal);
  EditorCaretPropsFromString(Ed.CaretPropsOverwrite, Op.OpCaretViewOverwrite);
  EditorCaretPropsFromString(Ed.CaretPropsReadonly, Op.OpCaretViewReadonly);

  if Op.OpCaretAfterPasteColumn<=Ord(High(TATPasteCaret)) then
    Ed.OptCaretPosAfterPasteColumn:= TATPasteCaret(Op.OpCaretAfterPasteColumn);

  Ed.OptCaretVirtual:= Op.OpCaretVirtual;
  Ed.OptCaretManyAllowed:= Op.OpCaretMulti;
  Ed.OptCaretsAddedToColumnSelection:= Op.OpCaretsAddedToColumnSel;

  Ed.OptShowCurLine:= Op.OpShowCurLine;
  Ed.OptShowCurLineMinimal:= Op.OpShowCurLineMinimal;
  Ed.OptShowCurLineOnlyFocused:= Op.OpShowCurLineOnlyFocused;
  Ed.OptShowCurColumn:= Op.OpShowCurCol;
  Ed.OptLastLineOnTop:= Op.OpShowLastLineOnTop;
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
  Ed.OptScrollbarHorizontalHidden:= Op.OpHideHorizScrollbar;
  Ed.OptTextCenteringCharWidth:= IfThen(AApplyCentering, Op.OpCenteringWidth, 0);
  Ed.OptWordChars:= Op.OpWordChars;
  Ed.OptFoldStyle:= TATFoldStyle(Op.OpFoldStyle);
  Ed.OptFoldTooltipVisible:= Op.OpFoldTooltipShow;

  Ed.OptStapleStyle:= TATLineStyle(Op.OpStaplesStyle);
  S:= Op.OpStaplesProps;
  Ed.OptStapleIndent:= StrToIntDef(SGetItem(S), 0);
  Ed.OptStapleWidthPercent:= StrToIntDef(SGetItem(S), 40);
  Ed.OptStapleEdge1:= TATStapleEdge(StrToIntDef(SGetItem(S), 1));
  Ed.OptStapleEdge2:= TATStapleEdge(StrToIntDef(SGetItem(S), 1));

  Ed.OptAutoIndent:= Op.OpIndentAuto;
  if Op.OpIndentAutoKind<=Ord(High(TATAutoIndentKind)) then
    Ed.OptAutoIndentKind:= TATAutoIndentKind(Op.OpIndentAutoKind);
  Ed.OptAutoIndentBetterBracketsCurly:= Op.OpIndentAuto; //no separate option
  Ed.OptAutoIndentRegexRule:= Op.OpIndentAutoRule;

  Ed.OptZebraActive:= Op.OpZebra>0;
  if Ed.OptZebraActive then
    Ed.OptZebraAlphaBlend:= Op.OpZebra;

  Ed.OptIndentSize:= Op.OpIndentSize;
  Ed.OptIndentKeepsAlign:= Op.OpUnIndentKeepsAlign;
  Ed.OptIndentMakesWholeLinesSelection:= Op.OpIndentMakesWholeLineSel;

  Ed.OptMouse2ClickDragSelectsWords:= Op.OpMouse2ClickDragSelectsWords;
  Ed.OptMouseDragDrop:= Op.OpMouseDragDrop;
  ATSynEdit.OptMouseDragDropFocusesTargetEditor:= Op.OpMouseDragDropFocusTarget;
  Ed.OptMouseNiceScroll:= Op.OpMouseMiddleClickNiceScroll;
  Ed.OptMouseRightClickMovesCaret:= Op.OpMouseRightClickMovesCaret;
  Ed.OptMouseEnableColumnSelection:= Op.OpMouseEnableColumnSelection;
  Ed.OptMouseHideCursorOnType:= Op.OpMouseHideCursorOnType;
  Ed.OptMouseClickNumberSelectsLine:= Op.OpMouseGutterClickSelectedLine;
  Ed.OptMouseWheelZooms:= Op.OpMouseWheelZoom;
  Ed.OptMouseWheelScrollVertSpeed:= Op.OpMouseWheelSpeedVert;
  Ed.OptMouseWheelScrollHorzSpeed:= Op.OpMouseWheelSpeedHorz;
  Ed.OptMouseClickNumberSelectsLineWithEOL:= Op.OpMouseClickNumberSelectsEol;

  Ed.OptKeyBackspaceUnindent:= Op.OpKeyBackspaceUnindent;
  Ed.OptKeyTabIndents:= Op.OpKeyTabIndents;
  Ed.OptKeyHomeToNonSpace:= Op.OpKeyHomeToNonSpace;
  Ed.OptKeyHomeEndNavigateWrapped:= Op.OpKeyHomeEndNavigateWrapped;
  Ed.OptKeyEndToNonSpace:= Op.OpKeyEndToNonSpace;
  Ed.OptKeyPageKeepsRelativePos:= Op.OpKeyPageKeepsRelativePos;
  if Op.OpKeyPageUpDownSize<=Ord(High(TATPageUpDownSize)) then
    Ed.OptKeyPageUpDownSize:= TATPageUpDownSize(Op.OpKeyPageUpDownSize);
  Ed.OptKeyUpDownKeepColumn:= Op.OpKeyUpDownKeepColumn;
  Ed.OptKeyUpDownNavigateWrapped:= Op.OpKeyUpDownNavigateWrapped;
  Ed.OptKeyLeftRightSwapSel:= Op.OpKeyLeftRightSwapSel;
  Ed.OptKeyLeftRightSwapSelAndSelect:= Op.OpKeyLeftRightSwapSelAndSelect;
end;

function EditorGetSelLines(ed: TATSynEdit): integer;
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

function EditorFormatStatus(ed: TATSynEdit; const str: string): string;
var
  caret: TATCaretItem;
  cols, n, x_b, y_b, x_e, y_e: integer;
  bSel: boolean;
  char_str, temp_str: UnicodeString;
  char_code: integer;
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
  result:= stringreplace(result, '{x}', inttostr(caret.PosX+1), []);
  result:= stringreplace(result, '{y}', inttostr(caret.PosY+1), []);
  result:= stringreplace(result, '{y2}', inttostr(ed.carets[ed.carets.count-1].PosY+1), []);
  result:= stringreplace(result, '{yb}', inttostr(y_b+1), []);
  result:= stringreplace(result, '{ye}', inttostr(y_e+1), []);
  result:= stringreplace(result, '{count}', inttostr(ed.strings.count), []);
  result:= stringreplace(result, '{carets}', inttostr(ed.carets.count), []);
  result:= stringreplace(result, '{cols}', inttostr(cols), []);

  result:= stringreplace(result, '{_ln}', msgStatusbarTextLine, []);
  result:= stringreplace(result, '{_col}', msgStatusbarTextCol, []);
  result:= stringreplace(result, '{_sel}', msgStatusbarTextSel, []);
  result:= stringreplace(result, '{_linesel}', msgStatusbarTextLinesSel, []);
  result:= stringreplace(result, '{_carets}', msgStatusbarTextCarets, []);

  if pos('{sel}', result)>0 then
    result:= stringreplace(result, '{sel}', inttostr(EditorGetSelLines(ed)), []);

  if pos('{xx}', result)>0 then
    if ed.Strings.IsIndexValid(caret.PosY) then
    begin
      //optimized for huge lines
      n:= ed.Strings.CharPosToColumnPos(caret.PosY, caret.PosX, ed.TabHelper)+1;
      result:= stringreplace(result, '{xx}', inttostr(n), []);
    end;

  if pos('{char', result)>0 then
  begin
    char_str:= '';
    char_code:= -1;

    if ed.Strings.IsIndexValid(y_b) then
      if (x_b>=0) and (x_b<ed.Strings.LinesLen[y_b]) then
      begin
        char_str:= ed.Strings.LineSub(y_b, x_b+1, 1);
        if char_str<>'' then
          char_code:= Ord(char_str[1]);
      end;

    result:= stringreplace(result, '{char}', char_str, []);

    if char_code>=0 then
      temp_str:= IntToStr(char_code)
    else
      temp_str:= '';
    result:= stringreplace(result, '{char_dec}', temp_str, []);

    if char_code>=0 then
      temp_str:= IntToHex(char_code, 2)
    else
      temp_str:= '';
    result:= stringreplace(result, '{char_hex}', temp_str, []);

    if char_code>=0 then
      temp_str:= IntToHex(char_code, 4)
    else
      temp_str:= '';
    result:= stringreplace(result, '{char_hex4}', temp_str, []);
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
  Ed.Colors.TextFont:= GetAppColor('EdTextFont');
  Ed.Colors.TextBG:= GetAppColor('EdTextBg');
  Ed.Colors.TextSelFont:= GetAppColor('EdSelFont');
  Ed.Colors.TextSelBG:= GetAppColor('EdSelBg');

  Ed.Colors.TextDisabledFont:= GetAppColor('EdDisableFont');
  Ed.Colors.TextDisabledBG:= GetAppColor('EdDisableBg');
  Ed.Colors.Caret:= GetAppColor('EdCaret');
  Ed.Colors.Markers:= GetAppColor('EdMarkers');
  Ed.Colors.CurrentLineBG:= GetAppColor('EdCurLineBg');
  Ed.Colors.IndentVertLines:= GetAppColor('EdIndentVLine');
  Ed.Colors.UnprintedFont:= GetAppColor('EdUnprintFont');
  Ed.Colors.UnprintedBG:= GetAppColor('EdUnprintBg');
  Ed.Colors.UnprintedHexFont:= GetAppColor('EdUnprintHexFont');
  Ed.Colors.MinimapBorder:= GetAppColor('EdMinimapBorder');
  Ed.Colors.MinimapSelBG:= GetAppColor('EdMinimapSelBg');
  Ed.Colors.MinimapTooltipBG:= GetAppColor('EdMinimapTooltipBg');
  Ed.Colors.MinimapTooltipBorder:= GetAppColor('EdMinimapTooltipBorder');
  Ed.Colors.StateChanged:= GetAppColor('EdStateChanged');
  Ed.Colors.StateAdded:= GetAppColor('EdStateAdded');
  Ed.Colors.StateSaved:= GetAppColor('EdStateSaved');
  Ed.Colors.BlockStaple:= GetAppColor('EdBlockStaple');
  Ed.Colors.BlockStapleForCaret:= GetAppColor('EdBlockStapleActive');
  Ed.Colors.BlockSepLine:= GetAppColor('EdBlockSepLine');
  Ed.Colors.Links:= GetAppColor('EdLinks');
  Ed.Colors.LockedBG:= GetAppColor('EdLockedBg');
  Ed.Colors.ComboboxArrow:= GetAppColor('EdComboArrow');
  Ed.Colors.ComboboxArrowBG:= GetAppColor('EdComboArrowBg');
  Ed.Colors.CollapseLine:= GetAppColor('EdFoldMarkLine');
  Ed.Colors.CollapseMarkFont:= GetAppColor('EdFoldMarkFont');
  Ed.Colors.CollapseMarkBorder:= GetAppColor('EdFoldMarkBorder');
  Ed.Colors.CollapseMarkBG:= GetAppColor('EdFoldMarkBg');

  Ed.Colors.GutterFont:= GetAppColor('EdGutterFont');
  Ed.Colors.GutterBG:= GetAppColor('EdGutterBg');
  Ed.Colors.GutterCaretFont:= GetAppColor('EdGutterCaretFont');
  Ed.Colors.GutterCaretBG:= GetAppColor('EdGutterCaretBg');

  Ed.Colors.BookmarkBG:= GetAppColor('EdBookmarkBg');
  Ed.Colors.RulerFont:= GetAppColor('EdRulerFont');
  Ed.Colors.RulerBG:= GetAppColor('EdRulerBg');

  Ed.Colors.GutterFoldLine:= GetAppColor('EdFoldLine');
  Ed.Colors.GutterFoldBG:= GetAppColor('EdFoldBg');
  Ed.Colors.GutterPlusBorder:= GetAppColor('EdFoldPlusLine');
  Ed.Colors.GutterPlusBG:= GetAppColor('EdFoldPlusBg');

  Ed.Colors.MarginRight:= GetAppColor('EdMarginFixed');
  Ed.Colors.MarginCaret:= GetAppColor('EdMarginCaret');
  Ed.Colors.MarginUser:= GetAppColor('EdMarginUser');

  Ed.Colors.MarkedLinesBG:= GetAppColor('EdMarkedRangeBg');
  Ed.Colors.BorderLine:= GetAppColor('EdBorder');
  Ed.Colors.BorderLineFocused:= GetAppColor('EdBorderFocused');

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
  if Id='EdFoldPlusLine' then Ed.Colors.GutterPlusBorder:= AColor else
  if Id='EdFoldPlusBg' then Ed.Colors.GutterPlusBG:= AColor else
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
  if Id='EdFoldPlusLine' then exit(Ed.Colors.GutterPlusBorder);
  if Id='EdFoldPlusBg' then exit(Ed.Colors.GutterPlusBG);
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
  Str: atString;
begin
  Result:= #0;
  if Ed.Carets.Count<>1 then exit;
  Caret:= Ed.Carets[0];
  if not Ed.Strings.IsIndexValid(Caret.PosY) then exit;
  if (Caret.PosX<0) then exit;
  Str:= Ed.Strings.LineSub(Caret.PosY, Caret.PosX+1, 1);
  if Str<>'' then
    Result:= Str[1];
end;


function EditorGetFoldString(Ed: TATSynEdit): string;
var
  i: integer;
  R: TATSynRange;
begin
  Result:= '';
  for i:= 0 to Ed.Fold.Count-1 do
  begin
    R:= Ed.Fold[i];
    if R.Folded then
      Result:= Result+(IntToStr(R.Y)+',');
  end;
end;

procedure EditorSetFoldString(Ed: TATSynEdit; S: string);
var
  SItem: string;
  ScrollInfo: TATSynScrollInfo;
  n: integer;
begin
  Ed.DoCommand(cCommand_UnfoldAll);

  repeat
    SItem:= SGetItem(S);
    if SItem='' then Break;

    n:= StrToIntDef(SItem, -1);
    if not Ed.Strings.IsIndexValid(n) then Continue;

    n:= Ed.Fold.FindRangeWithPlusAtLine(n);
    if n<0 then Continue;

    Ed.DoRangeFold(n);
  until false;

  //fix changed horz scroll, https://github.com/Alexey-T/CudaText/issues/1439
  ScrollInfo:= Ed.ScrollHorz;
  ScrollInfo.NPos:= 0;
  Ed.ScrollHorz:= ScrollInfo;

  Ed.Update;
end;


procedure EditorMouseClick_AtCursor(Ed: TATSynEdit; AAndSelect: boolean);
var
  Pnt: TPoint;
  Details: TATPosDetails;
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

procedure EditorMouseClick_NearCaret(Ed: TATSynEdit; AParams: string; AAndSelect: boolean);
var
  X, Y: integer;
  Caret: TATCaretItem;
begin
  X:= StrToIntDef(SGetItem(AParams), MaxInt);
  Y:= StrToIntDef(SGetItem(AParams), MaxInt);
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


function EditorGetLinkAtScreenCoord(Ed: TATSynEdit; P: TPoint): atString;
var
  Details: TATPosDetails;
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


function EditorAutoCloseBracket(Ed: TATSynEdit; CharBegin: atChar): boolean;
var
  Caret: TATCaretItem;
  X1, Y1, X2, Y2: integer;
  NPos, NCaret: integer;
  bSel: boolean;
  CharEnd: atChar;
  Str: atString;
  Shift, PosAfter: TPoint;
begin
  Result:= false;

  //makes no sense to auto-close brackets in overwrite mode
  if Ed.ModeOverwrite then exit;

  if CharBegin='(' then CharEnd:= ')' else
   if CharBegin='[' then CharEnd:= ']' else
    if CharBegin='{' then CharEnd:= '}' else
     if CharBegin='"' then CharEnd:= '"' else
      if CharBegin='''' then CharEnd:= '''' else
       if CharBegin='`' then CharEnd:= '`' else
        exit;

  //cancel vertical selection
  Ed.DoSelect_ClearColumnBlock;

  Ed.Strings.BeginUndoGroup;
  for NCaret:= Ed.Carets.Count-1 downto 0 do
  begin
    Caret:= Ed.Carets[NCaret];
    if not Ed.Strings.IsIndexValid(Caret.PosY) then Continue;
    Caret.GetRange(X1, Y1, X2, Y2, bSel);

    if not bSel then
    begin
      NPos:= Caret.PosX;
      Str:= Ed.Strings.Lines[Caret.PosY];
      //don't do, if before caret is \
      if (NPos>=1) and (NPos<=Length(Str)) and (Str[NPos]='\') then Continue;
      //don't do, if caret before text
      if (NPos<Length(Str)) and
        not Editor_NextCharAllowed_AutoCloseBracket(Str[NPos+1]) then Continue;
    end;

    if not bSel then
    begin
      Ed.Strings.TextInsert(X1, Y1, CharBegin+CharEnd, false, Shift, PosAfter);
      Ed.DoCaretsShift(X1, Y1, Shift.X, Shift.Y, PosAfter);

      Caret.PosX:= Caret.PosX+1;
    end
    else
    begin
      Ed.Strings.TextInsert(X2, Y2, CharEnd, false, Shift, PosAfter);
      Ed.DoCaretsShift(X2, Y2, Shift.X, Shift.Y, PosAfter);

      Ed.Strings.TextInsert(X1, Y1, CharBegin, false, Shift, PosAfter);
      Ed.DoCaretsShift(X1, Y1, Shift.X, Shift.Y, PosAfter);

      Caret.EndX:= X1+1;
      Caret.PosX:= X2+IfThen(Y1=Y2, 1);
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
  CParent: TControl;
begin
  if C.CanSetFocus then
  begin
    C.SetFocus;

    //this fixes Linux gtk2 issue, if added to handling of cmd_FileClose,
    //(focus goes to console, after closing tab),
    //so added here too
    CParent:= C.GetTopParent;
    if CParent is TForm then
      (CParent as TForm).ActiveControl:= C;
  end;
end;


procedure EditorGotoLastEditingPos(Ed: TATSynEdit;
  AIndentHorz, AIndentVert: integer);
var
  Caret: TATCaretItem;
begin
  Ed.Strings.DoGotoLastEditPos;
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
  NumLine, NumCol: integer;
  Pnt: TPoint;
  bExtend: boolean;
  Caret: TATCaretItem;
begin
  bExtend:= SEndsWith(SInput, '+');
  if bExtend then
    SetLength(SInput, Length(SInput)-1);

  if SEndsWith(SInput, '%') then
  begin
    NumLine:= StrToIntDef(Copy(SInput, 1, Length(SInput)-1), -1);
    NumLine:= Ed.Strings.Count * NumLine div 100 - 1;
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
    NumLine:= StrToIntDef(SGetItem(SInput, ':'), -1) - 1;
    NumCol:= StrToIntDef(SInput, 0) - 1;
  end;

  Result:= NumLine>=0;
  if not Result then exit;

  NumLine:= Min(NumLine, Ed.Strings.Count-1);
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
        if IsPosSorted(Caret.EndX, Caret.EndY, Caret.PosX, Caret.PosY, false) then
          Pnt:= Point(Caret.EndX, Caret.EndY);
      end
      else
      begin
        //jump above
        if not IsPosSorted(Caret.EndX, Caret.EndY, Caret.PosX, Caret.PosY, false) then
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


procedure EditorCaretPropsFromString(Props: TATCaretProps; S: string);
begin
  Props.Width:= StrToIntDef(SGetItem(S), 0);
  Props.Height:= StrToIntDef(SGetItem(S), 0);
  Props.EmptyInside:= SGetItem(S)='_';
end;


procedure EditorCaretPropsFromPyTuple(Props: TATCaretProps; S: string);
begin
  Props.Width:= StrToIntDef(SGetItem(S), 0);
  Props.Height:= StrToIntDef(SGetItem(S), 0);
  Props.EmptyInside:= SGetItem(S)='1';
end;

function EditorGetPairForCloseBracket(ch: char): char;
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


function _StringToPython(const S: string): string; inline;
begin
  Result:= StringReplace(S, '\', '\\', [rfReplaceAll]);
  Result:= StringReplace(Result, '"', '\"', [rfReplaceAll]);
  Result:= '"'+Result+'"';
end;

procedure EditorSaveTempOptions(Ed: TATSynEdit; var Ops: TATEditorTempOps);
begin
  Ops.FontSize:= Ed.Font.Size;
  Ops.WrapMode:= Ed.OptWrapMode;
  Ops.ShowMinimap:= Ed.OptMinimapVisible;
  Ops.ShowMicromap:= Ed.OptMicromapVisible;
  Ops.ShowRuler:= Ed.OptRulerVisible;
  Ops.ShowNumbers:= Ed.Gutter.Items[Ed.GutterBandNumbers].Visible;
  Ops.ShowUnprinted:= Ed.OptUnprintedVisible;
end;

procedure EditorRestoreTempOptions(Ed: TATSynEdit; const Ops: TATEditorTempOps);
begin
  Ed.Font.Size:= Ops.FontSize;
  Ed.OptWrapMode:= Ops.WrapMode;
  Ed.OptMinimapVisible:= Ops.ShowMinimap;
  Ed.OptMicromapVisible:= Ops.ShowMicromap;
  Ed.OptRulerVisible:= Ops.ShowRuler;
  Ed.Gutter.Items[Ed.GutterBandNumbers].Visible:= Ops.ShowNumbers;
  Ed.OptUnprintedVisible:= Ops.ShowUnprinted;
end;

end.

