(*
This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at http://mozilla.org/MPL/2.0/.

Copyright (c) Alexey Torgashin
*)
unit proc_editor_micromap;

{$mode objfpc}{$H+}
{$ScopedEnums on}

interface

uses
  Graphics, Types,
  BGRABitmap,
  ATSynEdit;

procedure EditorPaintMicromap(Ed: TATSynEdit; ACanvas: TCanvas; const ARect: TRect; ABitmap: TBGRABitmap);

implementation

uses
  SysUtils, Math,
  BGRABitmapTypes,
  ATStrings,
  ATStringProc,
  ATSynEdit_WrapInfo,
  ATSynEdit_Carets,
  ATSynEdit_Markers,
  ATSynEdit_Bookmarks,
  proc_globdata,
  proc_colors;

function EditorRectMicromapMark(Ed: TATSynEdit; AColumn, AIndexFrom, AIndexTo: integer;
  AMapHeight, AMinMarkHeight, AScaleDiv: integer): TRect;
//to make things safe, don't pass the ARect, but only its height
//no need to check Ed.Micromap.IsIndexValid(AColumn), checked already
begin
    if AIndexFrom>=0 then
      Result.Top:= Int64(AIndexFrom) * AMapHeight div AScaleDiv
    else
      Result.Top:= 0;

    if AIndexTo>=0 then
      Result.Bottom:= Max(Result.Top + AMinMarkHeight,
                          Int64(AIndexTo+1) * AMapHeight div AScaleDiv)
    else
      Result.Bottom:= AMapHeight;

    with Ed.Micromap.Columns[AColumn] do
    begin
      Result.Left:= NLeft;
      Result.Right:= NRight;
    end;
end;


type
  TEditorMicromapMark = (Column, Full, Right);

//these are variables of EditorPaintMicromap,
//moved here to avoid big stack usage
var
  BoolArray: packed array of boolean;
  PropArray: packed array of record
    Column: integer;
    XColor: TBGRAPixel;
    Inited: boolean;
    MarkPos: TEditorMicromapMark;
  end;

procedure EditorPaintMicromap(Ed: TATSynEdit; ACanvas: TCanvas; const ARect: TRect; ABitmap: TBGRABitmap);
{
  micromap has columns:
    column_0: width 50% of char cell, it's used for line states
    column_1: width 50% of char cell, it's used for boomkarks + plugins marks
    right edge column: width 50% of char cell, it's used for selections
  so, for different micromap rect widths, some columns may overlap, e.g. right_edge and column_1
}
{
1.180.0 (2022/12)
+ add: reworked how micromap is painted in word-wrapped mode (both on vert scrollbar and not);
now it paints all WrapInfo items, so e.g. long wrapped line gives several cells on micromap
}
const
  cTagOccurrences = 101; //see plugin 'Highlight Occurrences'
  cTagSpellChecker = 105; //see plugin 'Spell Checker'
  cTagColumnFullsized = -2;
var
  NWidthSmall: integer;
  NScaleDiv: integer;
//
  function GetWrapItemRect(AColumn, AIndexFrom, AIndexTo: integer; AMarkPos: TEditorMicromapMark): TRect;
  begin
    Result:= EditorRectMicromapMark(Ed, AColumn, AIndexFrom, AIndexTo, ARect.Height, EditorOps.OpMicromapMinMarkHeight, NScaleDiv);
    case AMarkPos of
      TEditorMicromapMark.Right:
        begin
          Result.Right:= ARect.Width;
          Result.Left:= Result.Right - NWidthSmall;
        end;
      TEditorMicromapMark.Full:
        begin
          Result.Left:= 0;
          Result.Right:= ARect.Width;
        end;
    end;
  end;
//
var
  St: TATStrings;
  Wr: TATWrapInfo;
  Caret: TATCaretItem;
  LineState: TATLineState;
  Marker: TATMarkerItem;
  BookmarkPtr: PATBookmarkItem;
  XColor, XColorBkmk, XColorSelected, XColorOccur, XColorSpell: TBGRAPixel;
  NColor: TColor;
  RectMark: TRect;
  NLine1, NIndex, NIndex1, NIndex2, NColumnIndex, NMaxLineIndex, NColumnCount: integer;
  CaretX1, CaretY1, CaretX2, CaretY2: integer;
  bSel: boolean;
  i: integer;
begin
  St:= Ed.Strings;
  if St.Count=0 then exit;
  Wr:= Ed.WrapInfo;
  if Wr.Count=0 then exit;

  NMaxLineIndex:= St.Count-1;
  NColumnCount:= Length(Ed.Micromap.Columns);
  if NColumnCount<2 then exit;

  NWidthSmall:= Ed.TextCharSize.XScaled * EditorOps.OpMicromapSmallMarkSizePercents div 100 div ATEditorCharXScale;

  NScaleDiv:= Max(1, Wr.Count);
  if Ed.OptLastLineOnTop then
    NScaleDiv:= Max(1, NScaleDiv+Ed.GetVisibleLines-1);

  ABitmap.SetSize(ARect.Width, ARect.Height);

  XColor.FromColor(GetAppColor(TAppThemeColor.EdMicromapBg));
  ABitmap.Fill(XColor);

  //paint full-width area of current visible area
  NIndex1:= Ed.ScrollVert.NPos;
  NIndex2:= NIndex1+Ed.GetVisibleLines; //note: limiting this by Ed.WrapInfo.Count-1 causes issue #4718
  RectMark:= GetWrapItemRect(0, NIndex1, NIndex2, TEditorMicromapMark.Full);
  XColor.FromColor(GetAppColor(TAppThemeColor.EdMicromapViewBg));
  ABitmap.FillRect(RectMark, XColor);

  XColorSelected.FromColor(Ed.Colors.TextSelBG);
  XColorBkmk.FromColor(Ed.Colors.StateAdded); //not sure what color to use
  XColorOccur.FromColor(GetAppColor(TAppThemeColor.EdMicromapOccur));
  XColorSpell.FromColor(GetAppColor(TAppThemeColor.EdMicromapSpell));

  //paint line states
  if Ed.OptMicromapLineStates and (Wr.Count>=Ed.OptMicromapShowForMinCount) then
    for i:= 0 to Wr.Count-1 do
    begin
      NIndex:= Wr.Data[i].NLineIndex;
      if NIndex>NMaxLineIndex then Break;
      LineState:= St.LinesState[NIndex];
      case LineState of
        TATLineState.None: Continue;
        TATLineState.Added: XColor.FromColor(Ed.Colors.StateAdded);
        TATLineState.Changed: XColor.FromColor(Ed.Colors.StateChanged);
        TATLineState.Saved: XColor.FromColor(Ed.Colors.StateSaved);
        else Continue;
      end;
      RectMark:= GetWrapItemRect(0{column_0}, i, i, TEditorMicromapMark.Column);
      ABitmap.FillRect(RectMark, XColor);
    end;

  //paint selections
  if Ed.OptMicromapSelections then
    for i:= 0 to Ed.Carets.Count-1 do
    begin
      Caret:= Ed.Carets[i];
      Caret.GetRange(CaretX1, CaretY1, CaretX2, CaretY2, bSel);
      if not bSel then Continue;

      NIndex1:= Wr.FindIndexOfCaretPos(Point(CaretX1, CaretY1));
      if (CaretY1=CaretY2) and Wr.IsIndexUniqueForLine(NIndex1) then
        NIndex2:= NIndex1
      else
        NIndex2:= Wr.FindIndexOfCaretPos(Point(CaretX2, CaretY2));

      RectMark:= GetWrapItemRect(0, NIndex1, NIndex2, TEditorMicromapMark.Right);
      ABitmap.FillRect(RectMark, XColorSelected);
    end;

  //paint background of columns added from Py API
  for i:= 2{after default columns} to NColumnCount-1 do
  begin
    NColor:= Ed.Micromap.Columns[i].NColor;
    if NColor<>clNone then
    begin
      XColor.FromColor(NColor);
      RectMark:= EditorRectMicromapMark(Ed, i, -1, -1, ARect.Height, EditorOps.OpMicromapMinMarkHeight, NScaleDiv);
      ABitmap.FillRect(RectMark, XColor);
    end;
  end;

  //paint bookmarks
  //it can be done w/o BoolArray but it will be 2x slower, with 50k bookmarks
  if Ed.OptMicromapBookmarks then
  begin
    BoolArray:= nil;
    SetLength(BoolArray, St.Count);
    for i:= 0 to St.Bookmarks.Count-1 do
    begin
      BookmarkPtr:= St.Bookmarks.ItemPtr[i];
      NIndex:= BookmarkPtr^.Data.LineNum;
      if (NIndex>=0) and (NIndex<=High(BoolArray)) then
        BoolArray[NIndex]:= true;
    end;
    for i:= 0 to Wr.Count-1 do
    begin
      NIndex:= Wr.Data[i].NLineIndex;
      if (NIndex>=0) and (NIndex<=High(BoolArray)) then
        if BoolArray[NIndex] then
        begin
          RectMark:= EditorRectMicromapMark(Ed, 1{column}, i, i, ARect.Height, EditorOps.OpMicromapMinMarkHeight, NScaleDiv);
          ABitmap.FillRect(RectMark, XColorBkmk);
        end;
    end;
    BoolArray:= nil;
  end;

  //paint marks for plugins
  PropArray:= nil;
  SetLength(PropArray, St.Count);
  for i:= 0 to Ed.Attribs.Count-1 do
  begin
    Marker:= Ed.Attribs[i];
    NLine1:= Marker.PosY;
    {
    NLine2:= NLine1;
    //negative LenX means we need multiline Marker, its height is abs(LenX)
    if Marker.SelX<0 then
      Inc(NLine2, -Marker.SelX-1);
    }

    if (NLine1<0) or (NLine1>High(PropArray)) then Continue; //fix issue #4821

    case Marker.Tag of
      cTagSpellChecker:
        begin
          PropArray[NLine1].Inited:= true;
          PropArray[NLine1].Column:= 1;
          PropArray[NLine1].MarkPos:= TEditorMicromapMark.Column;
          PropArray[NLine1].XColor:= XColorSpell;
        end;
      cTagOccurrences:
        begin
          PropArray[NLine1].Inited:= true;
          PropArray[NLine1].Column:= 1;
          PropArray[NLine1].MarkPos:= TEditorMicromapMark.Column;
          PropArray[NLine1].XColor:= XColorOccur;
        end
      else
      begin
        if Marker.TagEx>0 then
        begin
          NColumnIndex:= Ed.Micromap.ColumnFromTag(Marker.TagEx);
          if NColumnIndex>=0 then
          begin
            //if ColorBG=none, it may be find-all-matches with custom border color, use border color
            if Marker.LinePart.ColorBG<>clNone then
              XColor.FromColor(Marker.LinePart.ColorBG)
            else
              XColor.FromColor(Marker.LinePart.ColorBorder);
            PropArray[NLine1].Inited:= true;
            PropArray[NLine1].Column:= NColumnIndex;
            PropArray[NLine1].MarkPos:= TEditorMicromapMark.Column;
            PropArray[NLine1].XColor:= XColor;
          end;
        end
        else
        if Marker.TagEx=cTagColumnFullsized then
        begin
          PropArray[NLine1].Inited:= true;
          PropArray[NLine1].Column:= 0;
          PropArray[NLine1].MarkPos:= TEditorMicromapMark.Full;
          PropArray[NLine1].XColor.FromColor(Marker.LinePart.ColorBG);
        end;
      end;
    end;
  end;

  for i:= 0 to Wr.Count-1 do
  begin
    NIndex:= Wr.Data[i].NLineIndex;
    if (NIndex<0) or (NIndex>High(PropArray)) then Continue;
    if PropArray[NIndex].Inited then
    begin
      RectMark:= GetWrapItemRect(PropArray[NIndex].Column, i, i, PropArray[NIndex].MarkPos);
      if PropArray[NIndex].MarkPos<>TEditorMicromapMark.Full then
        ABitmap.FillRect(RectMark, PropArray[NIndex].XColor)
      else
        //todo: not tested with BGRABitmap - it must give inverted colors
        ABitmap.FillRect(RectMark, PropArray[NIndex].XColor, dmDrawWithTransparency, $8000);
    end;
  end;
  PropArray:= nil;

  //all done
  ABitmap.Draw(ACanvas, ARect.Left, ARect.Top);
end;

end.

