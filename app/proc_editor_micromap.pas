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

procedure EditorPaintMicromap(Ed: TATSynEdit; ACanvas: TCanvas; const ARect: TRect; var ABitmap: TBGRABitmap);

implementation

uses
  SysUtils, Math,
  BGRABitmapTypes,
  ATStrings,
  ATStringProc,
  ATSynEdit_Carets,
  ATSynEdit_Markers,
  ATSynEdit_Bookmarks,
  proc_globdata,
  proc_colors;

function EditorRectMicromapMark(Ed: TATSynEdit; AColumn, ALineFrom, ALineTo: integer;
  AMapHeight, AMinMarkHeight, AScaleDiv: integer): TRect;
//to make things safe, don't pass the ARect, but only its height
begin
  if Ed.Micromap.IsIndexValid(AColumn) then
  begin
    if ALineFrom>=0 then
      Result.Top:= Int64(ALineFrom) * AMapHeight div AScaleDiv
    else
      Result.Top:= 0;

    if ALineTo>=0 then
      Result.Bottom:= Max(Result.Top + AMinMarkHeight,
                          Int64(ALineTo+1) * AMapHeight div AScaleDiv)
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


procedure EditorPaintMicromap(Ed: TATSynEdit; ACanvas: TCanvas; const ARect: TRect; var ABitmap: TBGRABitmap);
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
  function GetItemRect(AColumn, NLine1, NLine2: integer; AMarkPos: TAppMicromapMarkPos): TRect;
  begin
    Result:= EditorRectMicromapMark(Ed, AColumn, NLine1, NLine2, ARect.Height, EditorOps.OpMicromapMinMarkHeight, NScaleDiv);
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
  Caret: TATCaretItem;
  LineState: TATLineState;
  Marker: TATMarkerItem;
  Bookmarks: TATBookmarks;
  BookmarkPtr: PATBookmarkItem;
  XColor, XColorSelected, XColorOccur, XColorSpell: TBGRAPixel;
  NColor: TColor;
  RectMark: TRect;
  NLine1, NLine2, NIndex, i: integer;
begin
  St:= Ed.Strings;
  if St.Count=0 then exit;
  NWidthSmall:= Ed.TextCharSize.XScaled * EditorOps.OpMicromapSmallMarkSizePercents div 100 div ATEditorCharXScale;

  NScaleDiv:= Max(1, St.Count);
  if Ed.OptLastLineOnTop then
    NScaleDiv:= Max(1, NScaleDiv+Ed.GetVisibleLines-1);

  if ABitmap=nil then
    ABitmap:= TBGRABitmap.Create;
  ABitmap.SetSize(ARect.Width, ARect.Height);

  XColor.FromColor(GetAppColor(apclEdMicromapBg));
  ABitmap.Fill(XColor);

  //paint full-width area of current visible area
  RectMark:= GetItemRect(0, Ed.LineTop, Ed.LineBottom, markFull);
  XColor.FromColor(GetAppColor(apclEdMicromapViewBg));
  ABitmap.FillRect(RectMark, XColor);

  XColorSelected.FromColor(Ed.Colors.TextSelBG);
  XColorOccur.FromColor(GetAppColor(apclEdMicromapOccur));
  XColorSpell.FromColor(GetAppColor(apclEdMicromapSpell));

  //paint line states
  if Ed.OptMicromapLineStates and (St.Count>=Ed.OptMicromapShowForMinCount) then
    for i:= 0 to St.Count-1 do
    begin
      //if Ed.IsLineFolded(i) then
      //  Continue;
      LineState:= St.LinesState[i];
      case LineState of
        cLineStateNone: Continue;
        cLineStateAdded: XColor.FromColor(Ed.Colors.StateAdded);
        cLineStateChanged: XColor.FromColor(Ed.Colors.StateChanged);
        cLineStateSaved: XColor.FromColor(Ed.Colors.StateSaved);
        else Continue;
      end;
      RectMark:= GetItemRect(0{column_0}, i, i, markColumn);
      ABitmap.FillRect(RectMark, XColor);
    end;

  //paint selections
  if Ed.OptMicromapSelections then
    for i:= 0 to Ed.Carets.Count-1 do
    begin
      Caret:= Ed.Carets[i];
      Caret.GetSelLines(NLine1, NLine2, false);
      if NLine1<0 then Continue;
      RectMark:= GetItemRect(0, NLine1, NLine2, markRight);
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
    Bookmarks:= Ed.Strings.Bookmarks;
    XColor.FromColor(Ed.Colors.StateAdded); //not sure what color to take
    for i:= 0 to Bookmarks.Count-1 do
    begin
      BookmarkPtr:= Bookmarks.ItemPtr[i];
      NIndex:= BookmarkPtr^.Data.LineNum;
      //if Ed.IsLineFolded(NIndex) then
      //  Continue;
      RectMark:= EditorRectMicromapMark(Ed, 1{column}, NIndex, NIndex, ARect.Height, EditorOps.OpMicromapMinMarkHeight, NScaleDiv);
      ABitmap.FillRect(RectMark, XColor);
    end;
  end;

  //paint marks for plugins
  for i:= 0 to Ed.Attribs.Count-1 do
  begin
    Marker:= Ed.Attribs[i];

    NLine1:= Marker.PosY;
    NLine2:= NLine1;
    //negative LenX means we need multiline Marker, its height is abs(LenX)
    if Marker.SelX<0 then
      Inc(NLine2, -Marker.SelX-1);

    case Marker.Tag of
      cTagSpellChecker:
        begin
          RectMark:= GetItemRect(1{column_1}, NLine1, NLine2, markColumn);
          ABitmap.FillRect(RectMark, XColorSpell);
        end;
      cTagOccurrences:
        begin
          RectMark:= GetItemRect(1{column_1}, NLine1, NLine2, markColumn);
          ABitmap.FillRect(RectMark, XColorOccur);
        end;
      else
        begin
          if Marker.TagEx>0 then
          begin
            NIndex:= Ed.Micromap.ColumnFromTag(Marker.TagEx);
            if NIndex>=0 then
            begin
              //if ColorBG=none, it may be find-all-matches with custom border color, use border color
              if Marker.LinePart.ColorBG<>clNone then
                XColor.FromColor(Marker.LinePart.ColorBG)
              else
                XColor.FromColor(Marker.LinePart.ColorBorder);
              RectMark:= GetItemRect(NIndex, NLine1, NLine2, markColumn);
              ABitmap.FillRect(RectMark, XColor);
            end;
          end
          else
          if Marker.TagEx=cTagColumnFullsized then
          begin
            RectMark:= GetItemRect(0, NLine1, NLine2, markFull);
            //todo: not tested with BGRABitmap - it must give inverted colors
            XColor.FromColor(Marker.LinePart.ColorBG);
            ABitmap.FillRect(RectMark, XColor, dmDrawWithTransparency, $8000);
          end;
        end;
      end;
  end;

  ABitmap.Draw(ACanvas, ARect.Left, ARect.Top);
end;

end.

