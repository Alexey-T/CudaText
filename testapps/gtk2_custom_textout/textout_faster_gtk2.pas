unit textout_faster_gtk2;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  Types, LCLType,
  Gtk2Int;

procedure TextOutFaster(DC: HDC; X, Y: Longint; Str: string; Dx: PInteger);

implementation

uses
  LazUTF8,
  Gtk2Def, Gtk2Proc, Gdk2, pango;

procedure TextOutFaster(DC: HDC; X, Y: Longint; Str: string; Dx: PInteger);
var
  DevCtx: TGtkDeviceContext absolute DC;
  DCOrigin: TPoint;
  Foreground, BackgroundColor: PGDKColor;

  procedure DoTextOut(X, Y: Integer; Str: Pchar; CurCount: Integer);
  var
    CurStr: PChar;
    CurDx: PInteger;
    CurScreenX: LongInt;
    CharLen: LongInt;
    AFont: PGDIObject;
  begin
    AFont := DevCtx.GetFont;
    if (Dx <> nil) then
    begin
      CurStr := Str;
      CurDx := Dx;
      CurScreenX := X;
      while CurCount > 0 do
      begin
        CharLen := UTF8CodepointSize(CurStr);
        DevCtx.DrawTextWithColors(CurStr, CharLen, CurScreenX, Y, Foreground, BackgroundColor);
        inc(CurScreenX, CurDx^);
        inc(CurDx);
        inc(CurStr, CharLen);
        dec(CurCount, CharLen);
      end;
    end
    else
    begin
      DevCtx.DrawTextWithColors(Str, CurCount, X, Y, Foreground, BackgroundColor);
    end;
  end;

begin
  if not Gtk2WidgetSet.IsValidDC(DC) then Exit;
  if DevCtx.GC <> nil then; // create GC
  BackgroundColor := nil;
  DCOrigin := DevCtx.Offset;

  Gtk2Widgetset.UpdateDCTextMetric(DevCtx);
  DevCtx.SelectedColors := dcscCustom;

  if (DevCtx.BkMode = OPAQUE) then
  begin
    AllocGDIColor(DC, @DevCtx.CurrentBackColor);
    BackGroundColor := @DevCtx.CurrentBackColor.Color;
  end;

  EnsureGCColor(DC, dccCurrentTextColor, True, False);
  Foreground := nil; //StyleForegroundColor(CurrentTextColor.ColorRef, nil);

  DoTextOut(
    X + DCOrigin.X,
    Y + DCOrigin.Y,
    PChar(Str),
    Length(Str)
    );
end;

end.

