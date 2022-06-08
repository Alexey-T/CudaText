{
Copyright (C) Alexey Torgashin, uvviewsoft.com
License: MPL 2.0 or LGPL
}
unit win32menustyler;

{$mode objfpc}{$H+}

interface

uses
  Windows, SysUtils, Classes, Graphics, Menus, Forms,
  Types, LCLType, LCLProc, Registry,
  ImgList;

type
  TWin32MenuStylerTheme = record
    ColorBk: TColor;
    ColorBkSelected: TColor;
    ColorBkSelectedDisabled: TColor; //used only if <>clNone
    ColorSelBorder: TColor; //used only if <>clNone
    ColorFont: TColor;
    ColorFontSelected: TColor; //used only if <>clNone
    ColorFontDisabled: TColor;
    ColorFontShortcut: TColor;
    CharCheckmark: WideChar;
    CharRadiomark: WideChar;
    CharSubmenu: WideChar;
    FontName: string;
    FontSize: integer;
    //indents in percents of average char width
    IndentMinPercents: integer; //indent from edges to separator line
    IndentBigPercents: integer; //indent from left edge to caption
    IndentIconPercents: integer; //indents around the icon
    IndentRightPercents: integer; //indent from right edge to end of shortcut text
    IndentSubmenuArrowPercents: integer; //indent from right edge to submenu '>' char
  end;

type
  { TWin32MenuStyler }

  TWin32MenuStyler = class
  private
    procedure ApplyBackColor(h: HMENU; AReset: boolean);
    procedure HandleMenuDrawItem(Sender: TObject; ACanvas: TCanvas;
      ARect: TRect; AState: TOwnerDrawState);
    procedure HandleMenuMeasureItem(Sender: TObject; ACanvas: TCanvas;
      var AWidth, AHeight: Integer);
    procedure HandleMenuPopup(Sender: TObject);
  public
    procedure ApplyToMenu(AMenu: TMenu);
    procedure ApplyToForm(AForm: TForm; ARepaintEntireForm: boolean);
    procedure ResetMenu(AMenu: TMenu);
    procedure ResetForm(AForm: TForm; ARepaintEntireForm: boolean);
  end;

var
  MenuStylerTheme: TWin32MenuStylerTheme;
  MenuStyler: TWin32MenuStyler = nil;

  MenuStylerAlwaysShowAccelerators: boolean = false;

implementation

procedure TWin32MenuStyler.ApplyBackColor(h: HMENU; AReset: boolean);
var
  mi: TMENUINFO;
begin
  FillChar(mi{%H-}, sizeof(mi), 0);
  mi.cbSize:= sizeof(mi);
  mi.fMask:= MIM_BACKGROUND or MIM_APPLYTOSUBMENUS;
  if AReset then
    mi.hbrBack:= 0
  else
    mi.hbrBack:= CreateSolidBrush(MenuStylerTheme.ColorBk);
  SetMenuInfo(h, @mi);
end;

procedure TWin32MenuStyler.ApplyToMenu(AMenu: TMenu);
begin
  AMenu.OwnerDraw:= true;
  AMenu.OnDrawItem:= @HandleMenuDrawItem;
  AMenu.OnMeasureItem:= @HandleMenuMeasureItem;

  //it don't work!
  {
  if AMenu is TPopupMenu then
    with (AMenu as TPopupMenu) do
      if not Assigned(OnPopup) then
        OnPopup:= @HandleMenuPopup;
        }

  //it dont work!
  //ApplyBackColor(AMenu.Handle, false);
end;

procedure TWin32MenuStyler.ApplyToForm(AForm: TForm; ARepaintEntireForm: boolean);
var
  menu: TMainMenu;
begin
  menu:= AForm.Menu;
  if menu=nil then exit;

  ApplyToMenu(menu);

  //theme 2-3 pixel frame around menu
  ApplyBackColor(GetMenu(AForm.Handle), false);

  //repaint the menu bar
  if ARepaintEntireForm and (AForm.WindowState=wsNormal) then
    with AForm do
    begin
      Width:= Width+1;
      Width:= Width-1;
    end;
end;

procedure TWin32MenuStyler.ResetMenu(AMenu: TMenu);
begin
  AMenu.OwnerDraw:= false;
  AMenu.OnDrawItem:= nil;
  AMenu.OnMeasureItem:= nil;
end;

procedure TWin32MenuStyler.ResetForm(AForm: TForm; ARepaintEntireForm: boolean);
var
  menu: TMenu;
begin
  menu:= AForm.Menu;
  if menu=nil then exit;

  ResetMenu(menu);
  ApplyBackColor(GetMenu(AForm.Handle), true);

  //repaint the menu bar
  if ARepaintEntireForm then
    with AForm do
    begin
      Width:= Width+1;
      Width:= Width-1;
    end;
end;

procedure TWin32MenuStyler.HandleMenuDrawItem(Sender: TObject; ACanvas: TCanvas;
  ARect: TRect; AState: TOwnerDrawState);
const
  cSampleShort = '0';
  cSampleTall = 'Wj';
var
  mi: TMenuItem;
  Images: TCustomImageList;
  IconW: integer;
  dx, dxCell, dxMin, dxBig, Y: integer;
  ExtCell, ExtTall, Ext2: Types.TSize;
  NDrawFlags: UINT;
  bSel, bDisabled, bInBar, bHasSubmenu: boolean;
  BufW: UnicodeString;
  mark: WideChar;
  R: TRect;
begin
  mi:= Sender as TMenuItem;
  bSel:= odSelected in AState;
  bDisabled:= odDisabled in AState;
  bInBar:= mi.IsInMenuBar;
  bHasSubmenu:= (not bInBar) and (mi.Count>0);
  IconW:= 0;

  if bSel then
  begin
    if bDisabled and (MenuStylerTheme.ColorBkSelectedDisabled<>clNone) then
      ACanvas.Brush.Color:= MenuStylerTheme.ColorBkSelectedDisabled
    else
      ACanvas.Brush.Color:= MenuStylerTheme.ColorBkSelected;
    if MenuStylerTheme.ColorSelBorder<>clNone then
      ACanvas.Pen.Color:= MenuStylerTheme.ColorSelBorder
    else
      ACanvas.Pen.Color:= ACanvas.Brush.Color;
    ACanvas.Rectangle(ARect);
  end
  else
  begin
    ACanvas.Brush.Color:= MenuStylerTheme.ColorBk;
    ACanvas.FillRect(ARect);
  end;

  ACanvas.Font.Name:= MenuStylerTheme.FontName;
  ACanvas.Font.Size:= MenuStylerTheme.FontSize;
  ACanvas.Font.Style:= [];

  Windows.GetTextExtentPoint(ACanvas.Handle, PChar(cSampleShort), Length(cSampleShort), ExtCell);
  dxCell:= ExtCell.cx;
  dxMin:= dxCell * MenuStylerTheme.IndentMinPercents div 100;
  dxBig:= dxCell * MenuStylerTheme.IndentBigPercents div 100;

  if Assigned(mi.Bitmap) then
    IconW:= mi.Bitmap.Width;

  Images:= mi.GetParentMenu.Images;
  if Assigned(Images) then
    IconW:= Max(IconW, Images.Width);

  if IconW>0 then
    dxBig:= Max(dxBig, IconW + dxCell * MenuStylerTheme.IndentIconPercents * 2 div 100);

  if mi.IsLine then
  begin
    ACanvas.Pen.Color:= MenuStylerTheme.ColorFontDisabled;
    Y:= (ARect.Top+ARect.Bottom) div 2;
    ACanvas.Line(ARect.Left+dxMin, Y, ARect.Right-dxMin, Y);
    exit;
  end;

  if bDisabled then
    ACanvas.Font.Color:= MenuStylerTheme.ColorFontDisabled
  else
  if bSel and (MenuStylerTheme.ColorFontSelected<>clNone) then
    ACanvas.Font.Color:= MenuStylerTheme.ColorFontSelected
  else
    ACanvas.Font.Color:= MenuStylerTheme.ColorFont;

  Windows.GetTextExtentPoint(ACanvas.Handle, PChar(cSampleTall), Length(cSampleTall), ExtTall);

  if bInBar then
    dx:= dxCell
  else
    dx:= dxBig;

  Y:= (ARect.Top+ARect.Bottom-ExtTall.cy) div 2;

  if MenuStylerAlwaysShowAccelerators then
    NDrawFlags:= 0
  else
  if odNoAccel in AState then
    NDrawFlags:= DT_HIDEPREFIX
  else
    NDrawFlags:= 0;

  BufW:= UTF8Decode(mi.Caption);
  R.Left:= ARect.Left+dx;
  R.Top:= Y;
  R.Right:= ARect.Right;
  R.Bottom:= ARect.Bottom;
  Windows.DrawTextW(ACanvas.Handle, PWideChar(BufW), Length(BufW), R, NDrawFlags);

  if (not bInBar) and Assigned(mi.Bitmap) and (mi.Bitmap.Width>0) then
  begin
    ACanvas.Draw(
      ARect.Left + (dx-mi.Bitmap.Width) div 2,
      (ARect.Top+ARect.Bottom-mi.Bitmap.Height) div 2,
      mi.Bitmap);
  end
  else
  if (not bInBar) and Assigned(Images) and (mi.ImageIndex>=0) then
  begin
    Images.Draw(ACanvas,
      ARect.Left + (dx-Images.Width) div 2,
      (ARect.Top+ARect.Bottom-Images.Height) div 2,
      mi.ImageIndex, not bDisabled);
  end
  else
  if mi.Checked then
  begin
    if mi.RadioItem then
      mark:= MenuStylerTheme.CharRadiomark
    else
      mark:= MenuStylerTheme.CharCheckmark;
    Windows.TextOutW(ACanvas.Handle,
      ARect.Left+ (dx-dxCell) div 2,
      Y, @mark, 1);
  end;

  if mi.ShortCut<>0 then
  begin
    if bDisabled then
      ACanvas.Font.Color:= MenuStylerTheme.ColorFontDisabled
    else
      ACanvas.Font.Color:= MenuStylerTheme.ColorFontShortcut;
    BufW:= UTF8Decode(ShortCutToText(mi.Shortcut));
    Windows.GetTextExtentPointW(ACanvas.Handle, PWideChar(BufW), Length(BufW), Ext2);
    Windows.TextOutW(ACanvas.Handle,
      ARect.Right - Ext2.cx - dxCell*MenuStylerTheme.IndentRightPercents div 100,
      Y, PWideChar(BufW), Length(BufW));
  end;

  if bHasSubmenu then
  begin
    if bDisabled then
      ACanvas.Font.Color:= MenuStylerTheme.ColorFontDisabled
    else
      ACanvas.Font.Color:= MenuStylerTheme.ColorFont;

    Windows.TextOutW(ACanvas.Handle,
      ARect.Right - dxCell*MenuStylerTheme.IndentSubmenuArrowPercents div 100 - 2,
      Y,
      @MenuStylerTheme.CharSubmenu,
      1);

    //block OS drawing of submenu arrow
    Windows.ExcludeClipRect(ACanvas.Handle,
      ARect.Right - dxCell*MenuStylerTheme.IndentRightPercents div 100,
      ARect.Top,
      ARect.Right,
      ARect.Bottom);
  end;
end;

procedure TWin32MenuStyler.HandleMenuMeasureItem(Sender: TObject;
  ACanvas: TCanvas; var AWidth, AHeight: Integer);
var
  Size: TSize;
  mi: TMenuItem;
  S: string;
begin
  if MenuStylerTheme.FontSize<=9 then exit;

  mi:= Sender as TMenuItem;
  S:= mi.Caption;
  if S='-' then exit;

  ACanvas.Font.Name:= MenuStylerTheme.FontName;
  ACanvas.Font.Size:= MenuStylerTheme.FontSize;
  ACanvas.Font.Style:= [];

  if not mi.IsInMenuBar then
  begin
    S:= '     '+S;
    if mi.ShortCut<>0 then
      S+= '  '+ShortCutToText(mi.ShortCut);
    if mi.Count>0 then;
      S+= ' >';
  end;

  Size:= ACanvas.TextExtent(S);
  AWidth:= Max(AWidth, Size.cx);
  AHeight:= Max(AHeight, Size.cy);
end;

procedure TWin32MenuStyler.HandleMenuPopup(Sender: TObject);
begin
  //it dont work!
  {
  if Sender is TPopupMenu then
    with (Sender as TPopupMenu) do
      ApplyBackColor(Handle, false);
  }
end;

initialization

  MenuStyler:= TWin32MenuStyler.Create;

  with MenuStylerTheme do
  begin
    ColorBk:= clDkGray;
    ColorBkSelected:= clNavy;
    ColorBkSelectedDisabled:= clNone;
    ColorSelBorder:= clNone;
    ColorFont:= clWhite;
    ColorFontSelected:= clNone;
    ColorFontDisabled:= clMedGray;
    ColorFontShortcut:= clYellow;
    CharCheckmark:= #$2713;
    CharRadiomark:= #$25CF;
    CharSubmenu:= #$2BC8;
    FontName:= 'default';
    FontSize:= 9;
    IndentMinPercents:= 50;
    IndentBigPercents:= 300;
    IndentIconPercents:= 40;
    IndentRightPercents:= 250;
    IndentSubmenuArrowPercents:= 150;
  end;

  with TRegistry.Create do
  try
    RootKey:= HKEY_CURRENT_USER;
    if OpenKey('Control Panel\Accessibility\Keyboard Preference', false) then
      MenuStylerAlwaysShowAccelerators:= ReadString('On')='1';
  finally
    Free;
  end;;


finalization

  FreeAndNil(MenuStyler);

end.
