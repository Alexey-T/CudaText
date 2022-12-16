(*
This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at http://mozilla.org/MPL/2.0/.

Copyright (c) Alexey Torgashin
*)
unit proc_editor_micromap;

{$mode objfpc}{$H+}

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
begin
  if Ed.Micromap.IsIndexValid(AColumn) then
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
  end
  else
    Result:= cRectEmpty;
end;


procedure EditorPaintMicromap(Ed: TATSynEdit; ACanvas: TCanvas; const ARect: TRect; ABitmap: TBGRABitmap);
{
  micromap has columns:
    column_0: width 50% of char cell, it's used for line states
    column_1: width 50% of char cell, it's used for boomkarks + plugins marks
    right edge column: width 50% of char cell, it's used for selections
  so, for different micromap rect widths, some columns may overlap, e.g. right_edge and column_1
}
type
  TAppMicromapMarkPos = (markColumn, markFull, markRight);
const
  cTagOccurrences = 101; //see plugin 'Highlight Occurrences'
  cTagSpellChecker = 105; //see plugin 'Spell Checker'
  cTagColumnFullsized = -2;
var
  NWidthSmall: integer;
  NScaleDiv: integer;
//
  function GetWrapItemRect(AColumn, AIndexFrom, AIndexTo: integer; AMarkPos: TAppMicromapMarkPos): TRect;
  begin
    Result:= EditorRectMicromapMark(Ed, AColumn, AIndexFrom, AIndexTo, ARect.Height, EditorOps.OpMicromapMinMarkHeight, NScaleDiv);
    case AMarkPos of
      markRight:
        begin
          Result.Right:= ARect.Width;
          Result.Left:= Result.Right - NWidthSmall;
        end;
      markFull:
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
  Bookmarks: TATBookmarks;
  BookmarkPtr: PATBookmarkItem;
  BoolArray: packed array of boolean;
  PropArray: packed array of record
    Column: integer;
    XColor: TBGRAPixel;
    Inited: boolean;
    MarkPos: TAppMicromapMarkPos;
  end;
  XColor, XColorBkmk, XColorSelected, XColorOccur, XColorSpell: TBGRAPixel;
  NColor: TColor;
  RectMark: TRect;
  NLine1, NLine2, NIndex, NIndex1, NIndex2, NColumnIndex, i: integer;
  CaretX1, CaretY1, CaretX2, CaretY2: integer;
  bSel: boolean;
begin
  St:= Ed.Strings;
  if St.Count=0 then exit;
  Wr:= Ed.WrapInfo;
  if Wr.Count=0 then exit;

  NWidthSmall:= Ed.TextCharSize.XScaled * EditorOps.OpMicromapSmallMarkSizePercents div 100 div ATEditorCharXScale;

  NScaleDiv:= Max(1, Wr.Count);
  if Ed.OptLastLineOnTop then
    NScaleDiv:= Max(1, NScaleDiv+Ed.GetVisibleLines-1);

  ABitmap.SetSize(ARect.Width, ARect.Height);

  XColor.FromColor(GetAppColor(apclEdMicromapBg));
  ABitmap.Fill(XColor);

  //paint full-width area of current visible area
  NIndex1:= Ed.ScrollVert.NPos;
  NIndex2:= Min(NIndex1+Ed.GetVisibleLines, Ed.WrapInfo.Count-1);
  RectMark:= GetWrapItemRect(0, NIndex1, NIndex2, markFull);
  XColor.FromColor(GetAppColor(apclEdMicromapViewBg));
  ABitmap.FillRect(RectMark, XColor);

  XColorSelected.FromColor(Ed.Colors.TextSelBG);
  XColorBkmk.FromColor(Ed.Colors.StateAdded); //not sure what color to use
  XColorOccur.FromColor(GetAppColor(apclEdMicromapOccur));
  XColorSpell.FromColor(GetAppColor(apclEdMicromapSpell));

  //paint line states
  if Ed.OptMicromapLineStates and (Wr.Count>=Ed.OptMicromapShowForMinCount) then
    for i:= 0 to Wr.Count-1 do
    begin
      NIndex:= Wr.Data[i].NLineIndex;
      if not St.IsIndexValid(NIndex) then Continue;
      LineState:= St.LinesState[NIndex];
      case LineState of
        cLineStateNone: Continue;
        cLineStateAdded: XColor.FromColor(Ed.Colors.StateAdded);
        cLineStateChanged: XColor.FromColor(Ed.Colors.StateChanged);
        cLineStateSaved: XColor.FromColor(Ed.Colors.StateSaved);
        else Continue;
      end;
      RectMark:= GetWrapItemRect(0{column_0}, i, i, markColumn);
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
      if Wr.IsIndexUniqueForLine(NIndex1) and (CaretY1=CaretY2) then
        NIndex2:= NIndex1
      else
        NIndex2:= Wr.FindIndexOfCaretPos(Point(CaretX2, CaretY2));

      RectMark:= GetWrapItemRect(0, NIndex1, NIndex2, markRight);
      ABitmap.FillRect(RectMark, XColorSelected);
    end;

  //paint background of columns added from Py API
  for i:= 2{after default columns} to Length(Ed.Micromap.Columns)-1 do
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
  if Ed.OptMicromapBookmarks then
  begin
    SetLength(BoolArray, St.Count);
    Bookmarks:= Ed.Strings.Bookmarks;
    for i:= 0 to Bookmarks.Count-1 do
    begin
      BookmarkPtr:= Bookmarks.ItemPtr[i];
      NIndex:= BookmarkPtr^.Data.LineNum;
      BoolArray[NIndex]:= true;
    end;
    for i:= 0 to Wr.Count-1 do
    begin
      NIndex:= Wr.Data[i].NLineIndex;
      if BoolArray[NIndex] then
      begin
        RectMark:= EditorRectMicromapMark(Ed, 1{column}, i, i, ARect.Height, EditorOps.OpMicromapMinMarkHeight, NScaleDiv);
        ABitmap.FillRect(RectMark, XColorBkmk);
      end;
    end;
    SetLength(BoolArray, 0);
  end;

  //paint marks for plugins
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
    case Marker.Tag of
      cTagSpellChecker:
        begin
          PropArray[NLine1].Inited:= true;
          PropArray[NLine1].Column:= 1;
          PropArray[NLine1].MarkPos:= markColumn;
          PropArray[NLine1].XColor:= XColorSpell;
        end;
      cTagOccurrences:
        begin
          PropArray[NLine1].Inited:= true;
          PropArray[NLine1].Column:= 1;
          PropArray[NLine1].MarkPos:= markColumn;
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
            PropArray[NLine1].MarkPos:= markColumn;
            PropArray[NLine1].XColor:= XColor;
          end;
        end
        else
        if Marker.TagEx=cTagColumnFullsized then
        begin
          PropArray[NLine1].Inited:= true;
          PropArray[NLine1].Column:= 0;
          PropArray[NLine1].MarkPos:= markFull;
          PropArray[NLine1].XColor.FromColor(Marker.LinePart.ColorBG);
        end;
      end;
    end;
  end;
  for i:= 0 to Wr.Count-1 do
  begin
    NIndex:= Wr.Data[i].NLineIndex;
    if PropArray[NIndex].Inited then
    begin
      RectMark:= GetWrapItemRect(PropArray[NIndex].Column, i, i, PropArray[NIndex].MarkPos);
      if PropArray[NIndex].MarkPos<>markFull then
        ABitmap.FillRect(RectMark, PropArray[NIndex].XColor)
      else
        //todo: not tested with BGRABitmap - it must give inverted colors
        ABitmap.FillRect(RectMark, PropArray[NIndex].XColor, dmDrawWithTransparency, $8000);
    end;
  end;
  SetLength(PropArray, 0);

  //all done
  ABitmap.Draw(ACanvas, ARect.Left, ARect.Top);
end;

end.

