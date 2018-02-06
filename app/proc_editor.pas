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
  ATStringProc,
  proc_globdata,
  proc_colors,
  math;

procedure EditorMarkerDrop(Ed: TATSynEdit);
procedure EditorMarkerGotoLast(Ed: TATSynEdit; AndDelete: boolean);
procedure EditorMarkerClearAll(Ed: TATSynEdit);
procedure EditorMarkerSwap(Ed: TATSynEdit);

type TAppBookmarkOp = (bmOpClear, bmOpSet, bmOpToggle);
procedure EditorBookmarkSet(ed: TATSynEdit; ALine, ABmKind: integer; AOp: TAppBookmarkOp; const AHint: string);
procedure EditorBookmarkInvertAll(ed: TATSynEdit);
procedure EditorBookmarkClearAll(ed: TATSynEdit);
procedure EditorBookmarkGotoNext(ed: TATSynEdit; ANext: boolean);
procedure EditorBookmarkPlaceCaretsOnBookmarks(ed: TATSynEdit);
procedure EditorBookmarkPlaceBookmarksOnCarets(ed: TATSynEdit);
procedure EditorBookmarkCopyMarkedLines(ed: TATSynEdit);
procedure EditorBookmarkDeleteMarkedLines(ed: TATSynEdit);

procedure EditorConvertTabsToSpaces(ed: TATSynEdit);
procedure EditorConvertSpacesToTabsLeading(Ed: TATSynEdit);

procedure EditorFocus(C: TWinControl);
procedure EditorMouseClick_AtCursor(Ed: TATSynEdit; AAndSelect: boolean);
procedure EditorMouseClick_NearCaret(Ed: TATSynEdit; AParams: string; AAndSelect: boolean);

function EditorGetCurrentChar(Ed: TATSynEdit): Widechar;
procedure EditorApplyOps(Ed: TATSynEdit; const Op: TEditorOps; AApplyUnprintedAndWrap, AApplyTabSize: boolean);

type
  TEditorFoldOp = (cEditorFold, cEditorUnfold, cEditorFoldUnfold);

procedure EditorFoldUnfoldRangeAtCurLine(Ed: TATSynEdit; AOp: TEditorFoldOp);
function EditorGetFoldString(Ed: TATSynEdit): string;
procedure EditorSetFoldString(Ed: TATSynEdit; S: string);

function EditorGetLinkAtScreenCoord(Ed: TATSynEdit; P: TPoint): atString;
function EditorGetLinkAtCaret(Ed: TATSynEdit): atString;

type
  TEdSelType = (selNo, selSmall, selStream, selCol, selCarets);

function EditorGetStatusType(ed: TATSynEdit): TEdSelType;
function EditorFormatStatus(ed: TATSynEdit; const str: string): string;
function EditorFormatTabsize(ed: TATSynEdit; const str: string): string;
procedure EditorDeleteNewColorAttribs(ed: TATSynEdit);
procedure EditorGotoLastEditingPos(Ed: TATSynEdit; AIndentHorz, AIndentVert: integer);
function EditorGotoFromString(Ed: TATSynEdit; SInput: string): boolean;

procedure EditorApplyTheme(Ed: TATSynedit);
procedure EditorSetColorById(Ed: TATSynEdit; const Id: string; AColor: TColor);
function EditorGetColorById(Ed: TATSynEdit; const Id: string): TColor;

function EditorIsAutocompleteCssPosition(Ed: TATSynEdit; AX, AY: integer): boolean;
function EditorAutoCloseBracket(Ed: TATSynEdit; SBegin: char): boolean;

implementation

procedure EditorBookmarkSet(ed: TATSynEdit; ALine, ABmKind: integer;
  AOp: TAppBookmarkOp; const AHint: string);
var
  NIndex: integer;
begin
  if ALine<0 then
    ALine:= ed.Carets[0].PosY;
  if not ed.Strings.IsIndexValid(ALine) then exit;

  case AOp of
    bmOpSet:
      begin
        ed.Strings.Bookmarks.Add(ALine, ABmKind, AHint);
      end;
    bmOpClear:
      begin
        ed.Strings.Bookmarks.DeleteForLine(ALine);
      end;
    bmOpToggle:
      begin
        NIndex:= ed.Strings.Bookmarks.Find(ALine);
        if NIndex>=0 then
          ed.Strings.Bookmarks.Delete(NIndex)
        else
          ed.Strings.Bookmarks.Add(ALine, ABmKind, AHint);
      end;
  end;

  ed.Update;
end;

procedure EditorBookmarkInvertAll(ed: TATSynEdit);
var
  NIndex, i: integer;
begin
  for i:= 0 to ed.Strings.Count-1 do
  begin
    NIndex:= ed.Strings.Bookmarks.Find(i);
    if NIndex>=0 then
      ed.Strings.Bookmarks.Delete(NIndex)
    else
      ed.Strings.Bookmarks.Add(i, 1, '');
  end;
  ed.Update;
end;

procedure EditorBookmarkClearAll(ed: TATSynEdit);
begin
  ed.Strings.Bookmarks.Clear;
  ed.Update;
end;

procedure EditorBookmarkGotoNext(ed: TATSynEdit; ANext: boolean);
var
  n, nFrom: integer;
begin
  n:= ed.Carets[0].PosY;
  nFrom:= n;
  repeat
    if ANext then Inc(n) else Dec(n);
    if n=nFrom then exit;

    if n>=ed.Strings.Count then n:= 0;
    if n<0 then n:= ed.Strings.Count-1;

    if ed.Strings.Bookmarks.Find(n)>=0 then
    begin
      ed.DoGotoPos(
        Point(0, n),
        Point(-1, -1),
        UiOps.FindIndentHorz,
        UiOps.FindIndentVert,
        true,
        true
        );
      exit;
    end;
  until false;
end;

procedure EditorApplyOps(Ed: TATSynEdit; const Op: TEditorOps;
  AApplyUnprintedAndWrap, AApplyTabSize: boolean);
begin
  Ed.Font.Name:= Op.OpFontName;
  Ed.Font.Size:= Op.OpFontSize;
  Ed.Font.Quality:= Op.OpFontQuality;
  Ed.OptShowFontLigatures:= Op.OpFontLigatures;

  {$ifdef windows}
  //no effect on gtk2, not tested on Mac
  Ed.OptCharSpacingX:= Op.OpSpacingX;
  {$endif}
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
  Ed.Gutter[Ed.GutterBandBm].Visible:= Op.OpGutterBookmarks;
  Ed.Gutter[Ed.GutterBandFold].Visible:= Op.OpGutterFold;
  Ed.Gutter[Ed.GutterBandNum].Visible:= Op.OpNumbersShow;
  Ed.Gutter.Update;

  Ed.OptNumbersFontSize:= Op.OpNumbersFontSize;
  if Op.OpNumbersStyle<=Ord(High(TATSynNumbersStyle)) then
    Ed.OptNumbersStyle:= TATSynNumbersStyle(Op.OpNumbersStyle);
  Ed.OptNumbersShowCarets:= Op.OpNumbersForCarets;
  if Op.OpNumbersCenter then
    Ed.OptNumbersAlignment:= taCenter
  else
    Ed.OptNumbersAlignment:= taRightJustify;

  Ed.OptRulerVisible:= Op.OpRulerShow;
  Ed.OptRulerNumeration:= TATRulerNumeration(Op.OpRulerNumeration);
  Ed.OptRulerFontSize:= Op.OpRulerFontSize;
  Ed.OptRulerSize:= Op.OpRulerSize;
  Ed.OptRulerTextIndent:= Op.OpRulerTextIndent;

  Ed.OptMinimapVisible:= Op.OpMinimapShow;
  Ed.OptMinimapShowSelAlways:= Op.OpMinimapShowSelAlways;
  Ed.OptMinimapShowSelBorder:= Op.OpMinimapShowSelBorder;
  Ed.OptMinimapCharWidth:= Op.OpMinimapCharWidth;
  Ed.OptMinimapAtLeft:= Op.OpMinimapAtLeft;
  Ed.OptMicromapVisible:= Op.OpMicromapShow;
  Ed.OptMicromapWidth:= Op.OpMicromapWidth;

  Ed.OptMarginRight:= Op.OpMarginFixed;
  Ed.OptMarginString:= Op.OpMarginString;

  Ed.OptShowURLs:= Op.OpLinks;
  Ed.OptShowURLsRegex:= Op.OpLinksRegex;

  if AApplyUnprintedAndWrap then
  begin
    Ed.OptUnprintedVisible:= Op.OpUnprintedShow;
    Ed.OptUnprintedSpaces:= Op.OpUnprintedSpaces;
    Ed.OptUnprintedSpacesTrailing:= Op.OpUnprintedSpacesTrailing;
    Ed.OptUnprintedEnds:= Op.OpUnprintedEnds;
    Ed.OptUnprintedEndsDetails:= Op.OpUnprintedEndDetails;
  end;

  //global options
  OptMaxTabPositionToExpand:= Op.OpTabMaxPosExpanded;
  OptAllowSpecialWidthChars:= Op.OpAllowWideChars;

  OptUnprintedEndArrowOrDot:= Op.OpUnprintedEndArrow;
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

  if Op.OpCaretShapeNorm<=Ord(High(TATSynCaretShape)) then
    Ed.OptCaretShape:= TATSynCaretShape(Op.OpCaretShapeNorm);
  if Op.OpCaretShapeOvr<=Ord(High(TATSynCaretShape)) then
    Ed.OptCaretShapeOvr:= TATSynCaretShape(Op.OpCaretShapeOvr);
  if Op.OpCaretShapeRO<=Ord(High(TATSynCaretShape)) then
    Ed.OptCaretShapeRO:= TATSynCaretShape(Op.OpCaretShapeRO);

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
  Ed.OptCopyLinesIfNoSel:= Op.OpCopyLineIfNoSel;
  Ed.OptCutLinesIfNoSel:= Op.OpCutLineIfNoSel;
  Ed.OptCopyColumnBlockAlignedBySpaces:= Op.OpCopyColumnAlignedBySpaces;
  Ed.OptSavingTrimSpaces:= Op.OpSavingTrimSpaces;
  Ed.OptSavingForceFinalEol:= Op.OpSavingForceFinalEol;
  Ed.OptShowScrollHint:= Op.OpShowHintOnVertScroll;
  Ed.OptTextCenteringCharWidth:= Op.OpCenteringWidth;
  Ed.OptWordChars:= Op.OpWordChars;
  Ed.OptFoldStyle:= TATFoldStyle(Op.OpFoldStyle);
  Ed.OptShowStapleStyle:= TATLineStyle(Op.OpStaplesStyle);
  OptHexChars:= OptHexCharsDefault + Op.OpHexChars;

  Ed.OptAutoIndent:= Op.OpIndentAuto;
  if Op.OpIndentAutoKind<=Ord(High(TATAutoIndentKind)) then
    Ed.OptAutoIndentKind:= TATAutoIndentKind(Op.OpIndentAutoKind);
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
  StrTemp: atString;
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

  if pos('{sel}', result)>0 then
    result:= stringreplace(result, '{sel}', inttostr(EditorGetSelLines(ed)), []);

  if pos('{xx}', result)>0 then
    if ed.Strings.IsIndexValid(caret.PosY) then
    begin
      //optimized for huge lines
      StrTemp:= ed.Strings.LineSub(caret.PosY, 1, caret.posX);
      n:= SCharPosToColumnPos(StrTemp, caret.PosX, ed.OptTabSize)+1;
      result:= stringreplace(result, '{xx}', inttostr(n), []);
    end;
end;

function EditorFormatTabsize(ed: TATSynEdit; const str: string): string;
begin
  Result:= str;
  SReplaceAll(Result, '{tab}', IntToStr(Ed.OptTabSize));
  SReplaceAll(Result, '{_}', IfThen(Ed.OptTabSpaces, '_'));
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
  Ed.Colors.StateChanged:= GetAppColor('EdStateChanged');
  Ed.Colors.StateAdded:= GetAppColor('EdStateAdded');
  Ed.Colors.StateSaved:= GetAppColor('EdStateSaved');
  Ed.Colors.BlockStaple:= GetAppColor('EdBlockStaple');
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
  if Id='EdStateChanged' then Ed.Colors.StateChanged:= AColor else
  if Id='EdStateAdded' then Ed.Colors.StateAdded:= AColor else
  if Id='EdStateSaved' then Ed.Colors.StateSaved:= AColor else
  if Id='EdBlockStaple' then Ed.Colors.BlockStaple:= AColor else
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
  if Id='EdStateChanged' then exit(Ed.Colors.StateChanged);
  if Id='EdStateAdded' then exit(Ed.Colors.StateAdded);
  if Id='EdStateSaved' then exit(Ed.Colors.StateSaved);
  if Id='EdBlockStaple' then exit(Ed.Colors.BlockStaple);
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


procedure EditorFoldUnfoldRangeAtCurLine(Ed: TATSynEdit; AOp: TEditorFoldOp);
var
  NLine: integer;
  R: TATSynRange;
begin
  if Ed.Carets.Count<>1 then exit;
  NLine:= Ed.Carets[0].PosY;
  if not Ed.Strings.IsIndexValid(NLine) then exit;

  R:= Ed.Fold.FindRangeWithPlusAtLine(NLine);
  if R=nil then exit;

  case AOp of
    cEditorFold:
      begin
        if not R.Folded then
        begin
          Ed.DoRangeFold(R);
          Ed.Update;
        end;
      end;
    cEditorUnfold:
      begin
        if R.Folded then
        begin
          Ed.DoRangeUnfold(R);
          Ed.Update;
        end;
      end;
    cEditorFoldUnfold:
      begin
        if R.Folded then
          Ed.DoRangeUnfold(R)
        else
          Ed.DoRangeFold(R);
        Ed.Update;
      end;
  end;
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
  R: TATSynRange;
  n: integer;
begin
  Ed.DoCommand(cCommand_UnfoldAll);

  repeat
    SItem:= SGetItem(S);
    if SItem='' then Break;

    n:= StrToIntDef(SItem, -1);
    if not Ed.Strings.IsIndexValid(n) then Continue;

    R:= Ed.Fold.FindRangeWithPlusAtLine(n);
    if R=nil then Continue;

    Ed.DoRangeFold(R);
  until false;

  Ed.Update;
end;


procedure EditorMarkerDrop(Ed: TATSynEdit);
var
  Caret: TATCaretItem;
begin
  if Ed.Carets.Count<>1 then exit;
  Caret:= Ed.Carets[0];
  Ed.Markers.Add(Caret.PosX, Caret.PosY);
  Ed.Update;
end;

procedure EditorMarkerGotoLast(Ed: TATSynEdit; AndDelete: boolean);
var
  Mark: TATMarkerItem;
  X1, Y1, X2, Y2: integer;
  NTag, i: integer;
begin
  if Ed.Markers.Count=0 then exit;
  Mark:= Ed.Markers[Ed.Markers.Count-1];

  X1:= Mark.PosX;
  Y1:= Mark.PosY;
  X2:= -1;
  Y2:= -1;

  if Mark.LenY<=0 then
  begin
    //LenX is selection len (1-line)
    if Mark.LenX>0 then
    begin
      X2:= X1+Mark.LenX;
      Y2:= Y1;
    end;
  end
  else
  begin
    //LenX is selection end X-pos;
    //LenY is count of sel lines
    X2:= Mark.LenX;
    Y2:= Y1+Mark.LenY;
  end;

  Ed.DoGotoPos(
    Point(X1, Y1),
    Point(X2, Y2),
    UiOps.FindIndentHorz,
    UiOps.FindIndentVert,
    true,
    true
    );

  if AndDelete then
  begin
    NTag:= Ed.Markers[Ed.Markers.Count-1].Tag;
    Ed.Markers.Delete(Ed.Markers.Count-1);

    //Tag>0: delete also same tag marks
    //and place mul-carets
    if NTag>0 then
      for i:= Ed.Markers.Count-1 downto 0 do
      begin
        Mark:= Ed.Markers[i];
        if Mark.Tag=NTag then
        begin
          if Mark.LenY>0 then
            Ed.Carets.Add(Mark.LenX, Mark.PosY+Mark.LenY, Mark.PosX, Mark.PosY)
          else
          if Mark.LenX<=0 then
            Ed.Carets.Add(Mark.PosX, Mark.PosY)
          else
            Ed.Carets.Add(Mark.PosX+Mark.LenX, Mark.PosY, Mark.PosX, Mark.PosY);
          Ed.Markers.Delete(i);
        end;
      end;
  end;

  Ed.Update;
end;

procedure EditorMarkerClearAll(Ed: TATSynEdit);
begin
  Ed.Markers.Clear;
  Ed.Update;
end;

procedure EditorMarkerSwap(Ed: TATSynEdit);
var
  Caret: TATCaretItem;
  Mark: TATMarkerItem;
  PX, PY: integer;
begin
  if Ed.Carets.Count<>1 then exit;
  if Ed.Markers.Count=0 then exit;
  Caret:= Ed.Carets[0];
  Mark:= Ed.Markers[Ed.Markers.Count-1];

  PX:= Caret.PosX;
  PY:= Caret.PosY;
  Caret.PosX:= Mark.PosX;
  Caret.PosY:= Mark.PosY;
  Mark.PosX:= PX;
  Mark.PosY:= PY;

  Ed.DoGotoCaret(cEdgeTop);
  Ed.Update;
end;


procedure EditorConvertTabsToSpaces(ed: TATSynEdit);
var
  S1, S2: atString;
  i: integer;
begin
  Ed.Strings.BeginUndoGroup;
  try
    for i:= 0 to Ed.Strings.Count-1 do
    begin
      S1:= Ed.Strings.Lines[i];
      if Pos(#9, S1)=0 then Continue;

      S2:= STabsToSpaces(S1, Ed.OptTabSize);
      if S1<>S2 then
        Ed.Strings.Lines[i]:= S2;
    end;
  finally
    Ed.Strings.EndUndoGroup;
    Ed.Update(true);
    Ed.DoEventChange;
  end;
end;

procedure EditorConvertSpacesToTabsLeading(Ed: TATSynEdit);
var
  S1, SBegin, SBegin2, SEnd: atString;
  N, i: integer;
begin
  Ed.Strings.BeginUndoGroup;
  try
    for i:= 0 to Ed.Strings.Count-1 do
    begin
      S1:= Ed.Strings.Lines[i];
      if Pos(' ', S1)=0 then Continue;

      N:= SGetIndentChars(S1);
      if N=0 then Continue;
      SBegin:= Copy(S1, 1, N);
      SEnd:= Copy(S1, N+1, MaxInt);

      SBegin2:= SSpacesToTabs(SBegin, Ed.OptTabSize);
      if SBegin2<>SBegin then
        Ed.Strings.Lines[i]:= SBegin2+SEnd;
    end;
  finally
    Ed.Strings.EndUndoGroup;
    Ed.Update(true);
    Ed.DoEventChange;
  end;
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
  Result:= Pos(ch, ' ])}'#9)>0;
end;


function EditorAutoCloseBracket(Ed: TATSynEdit; SBegin: char): boolean;
var
  Caret: TATCaretItem;
  X1, Y1, X2, Y2: integer;
  NPos: integer;
  bSel: boolean;
  SEnd, SSel: atString;
begin
  Result:= false;
  Caret:= Ed.Carets[0];
  if not Ed.Strings.IsIndexValid(Caret.PosY) then exit;
  Caret.GetRange(X1, Y1, X2, Y2, bSel);

  if not bSel then
  begin
    NPos:= Caret.PosX;
    SEnd:= Ed.Strings.Lines[Caret.PosY];
    //don't do, if before caret is \
    if (NPos>=1) and (NPos<=Length(SEnd)) and (SEnd[NPos]='\') then exit;
    //don't do, if caret before text
    if (NPos<Length(SEnd)) and
      not Editor_NextCharAllowed_AutoCloseBracket(SEnd[NPos+1]) then exit;
  end;

  if SBegin='(' then SEnd:= ')' else
   if SBegin='[' then SEnd:= ']' else
    if SBegin='{' then SEnd:= '}' else
     if SBegin='"' then SEnd:= '"' else
      if SBegin='''' then SEnd:= '''' else
       if SBegin='`' then SEnd:= '`' else
        exit;

  SSel:= '';
  if Ed.Carets.Count=1 then
    SSel:= Ed.TextSelected;

  Ed.DoCommand(cCommand_TextInsert, atString(SBegin)+SSel+SEnd);
  if SSel='' then
    Ed.DoCommand(cCommand_KeyLeft)
  else
    Ed.DoCaretSingle(X2+IfThen(Y1=Y2, 1), Y2, X1+1, Y1);

  Result:= true;
end;

procedure EditorBookmarkPlaceCaretsOnBookmarks(ed: TATSynEdit);
var
  X1, Y1, X2, Y2: integer;
  NLine, i: integer;
begin
  if ed.Carets.Count=0 then exit;
  with ed.Carets[0] do
  begin
    X1:= PosX;
    Y1:= PosY;
    X2:= EndX;
    Y2:= EndY;
  end;

  ed.Carets.Clear;
  for i:= 0 to ed.Strings.Bookmarks.Count-1 do
  begin
    NLine:= ed.Strings.Bookmarks[i].LineNum;
    ed.Carets.Add(0, NLine);
  end;

  if ed.Carets.Count=0 then
    ed.DoCaretSingle(X1, Y1, X2, Y2);
end;

procedure EditorBookmarkPlaceBookmarksOnCarets(ed: TATSynEdit);
var
  Caret: TATCaretItem;
  i: integer;
begin
  EditorBookmarkClearAll(ed);
  for i:= 0 to ed.Carets.Count-1 do
  begin
    Caret:= ed.Carets[i];
    if ed.Strings.IsIndexValid(Caret.PosY) then
      ed.Strings.Bookmarks.Add(Caret.PosY, 1, '');
  end;
end;


procedure EditorBookmarkCopyMarkedLines(ed: TATSynEdit);
var
  List: TStringList;
  NLine, i: integer;
begin
  List:= TStringList.Create;
  try
    for i:= 0 to Ed.Strings.Bookmarks.Count-1 do
    begin
      NLine:= Ed.Strings.Bookmarks[i].LineNum;
      if Ed.Strings.IsIndexValid(NLine) then
        List.Add(Ed.Strings.LinesUTF8[NLine]);
    end;
    SClipboardCopy(List.Text);
  finally
    FreeAndNil(List);
  end;
end;

procedure EditorBookmarkDeleteMarkedLines(ed: TATSynEdit);
var
  NCount, NLine, i: integer;
begin
  NCount:= Ed.Strings.Bookmarks.Count;
  if NCount=0 then exit;

  for i:= NCount-1 downto 0 do
  begin
    NLine:= Ed.Strings.Bookmarks[i].LineNum;
    Ed.Strings.LineDelete(NLine);
  end;

  Ed.UpdateIncorrectCaretPositions;
  Ed.Update(true);
  Ed.DoEventChange;
end;


procedure EditorFocus(C: TWinControl);
begin
  if C.CanSetFocus then
  begin
    C.SetFocus;

    //this fixes Linux gtk2 issue, if added to handling of cmd_FileClose,
    //(focus goes to console, after closing tab),
    //so added here too
    Application.MainForm.ActiveControl:= C;

    //if needed, try this, almost same as ActiveControl:=
    //Application.MainForm.FocusControl(Editor);
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
begin
  if SEndsWith(SInput, '%') then
  begin
    NumLine:= StrToIntDef(Copy(SInput, 1, Length(SInput)-1), -1);
    NumLine:= Ed.Strings.Count * NumLine div 100 - 1;
    NumCol:= 0;
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

  Ed.DoGotoPos(
    Point(NumCol, NumLine),
    Point(-1, -1),
    UiOps.FindIndentHorz,
    UiOps.FindIndentVert,
    true,
    true
    );
  Ed.Update;
end;


end.

