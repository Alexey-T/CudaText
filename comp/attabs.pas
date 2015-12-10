{
ATTabs component for Delphi/Lazarus
Copyright (c) Alexey Torgashin (UVViewSoft)
License: MPL 2.0 or LGPL
}

unit ATTabs;

{$ifdef FPC}
  {$mode delphi}
{$else}
  {$define windows}
  {$ifdef VER150} //Delphi 7
    {$define WIDE}
    {$define TNT} //Tnt controls
  {$endif}
{$endif}

interface

uses
  {$ifdef windows}
  Windows,
  {$endif}
  {$ifdef FPC}
  LCLIntf,
  LclType,
  LclProc,
  {$endif}
  Messages,
  Classes, Types, Graphics,
  Controls,
  {$ifdef TNT}
  TntMenus,
  {$endif}
  ExtCtrls,
  Menus;

type
  atString = {$ifdef WIDE} UnicodeString {$else} string {$endif};
  TatPopupMenu = {$ifdef TNT} TTntPopupMenu {$else} TPopupMenu {$endif};
  TatMenuItem = {$ifdef TNT} TTntMenuItem {$else} TMenuItem {$endif};

type
  TATTabData = class
  public
    TabCaption: atString;
    TabObject: TObject;
    TabColor: TColor;
    TabModified: boolean;
  end;

type
  TATTabElemType = (
    aeBackground,
    aeTabActive,
    aeTabPassive,
    aeTabPassiveOver,
    aeTabPlus,
    aeTabPlusOver,
    aeXButton,
    aeXButtonOver
    );

type
  TATTabOverEvent = procedure (Sender: TObject; ATabIndex: Integer) of object;
  TATTabCloseEvent = procedure (Sender: TObject; ATabIndex: Integer;
    var ACanClose, ACanContinue: boolean) of object;
  TATTabMenuEvent = procedure (Sender: TObject; var ACanShow: boolean) of object;
  TATTabDrawEvent = procedure (Sender: TObject;
    AElemType: TATTabElemType; ATabIndex: Integer;
    ACanvas: TCanvas; const ARect: TRect; var ACanDraw: boolean) of object;
  TATTabMoveEvent = procedure (Sender: TObject; NFrom, NTo: Integer) of object;  

type
  TATTriType = (triDown, triLeft, triRight);
  TATTabShowClose = (tbShowNone, tbShowAll, tbShowActive);

//int constants for GetTabAt
const
  cAtTabNone = -1; //none tab
  cAtTabPlus = -2;
  cAtArrowDown = -3;

type

  { TATTabs }

  TATTabs = class(TPanel)
  private
    //drag-drop
    FMouseDown: boolean;
    FMouseDownPnt: TPoint;
    FMouseDownDbl: boolean;
    FMouseDrag: boolean;

    //colors
    FColorBg: TColor; //color of background (visible at top and between tabs)
    FColorDrop: TColor;
    FColorBorderActive: TColor; //color of 1px border of active tab
    FColorBorderPassive: TColor; //color of 1px border of inactive tabs
    FColorTabActive: TColor; //color of active tab
    FColorTabPassive: TColor; //color of inactive tabs
    FColorTabOver: TColor; //color of inactive tabs, mouse-over
    FColorFontModified: TColor;
    FColorCloseBg: TColor; //color of small square with "x" mark, inactive
    FColorCloseBgOver: TColor; //color of small square with "x" mark, mouse-over
    FColorCloseBorderOver: TColor; //color of 1px border of "x" mark, mouse-over
    FColorCloseX: TColor; //color of "x" mark
    FColorCloseXOver: TColor; //"color of "x" mark, mouseover
    FColorArrow: TColor; //color of "down" arrow (tab menu), inactive
    FColorArrowOver: TColor; //color of "down" arrow, mouse-over

    //spaces
    FTabNumPrefix: atString;
    FTabBottom: boolean; 
    FTabAngle: Integer; //angle of tab border: from 0 (vertcal border) to any size
    FTabHeight: Integer;
    FTabWidthMin: Integer; //tab minimal width (used when lot of tabs)
    FTabWidthMax: Integer; //tab maximal width (used when only few tabs)
    FTabWidthHideX: Integer; //tab minimal width, after which "x" mark hides for inactive tabs
    FTabIndentDropI: Integer;
    FTabIndentInter: Integer; //space between nearest tabs (no need for angled tabs)
    FTabIndentInit: Integer; //space between first tab and left control edge
    FTabIndentLeft: Integer; //space between text and tab left edge
    FTabIndentText: Integer; //space between text and tab top edge
    FTabIndentTop: Integer; //height of top empty space (colored with bg)
    FTabIndentXRight: Integer; //space from "x" btn to right tab edge
    FTabIndentXInner: Integer; //space from "x" square edge to "x" mark
    FTabIndentXSize: Integer; //size of "x" mark
    FTabIndentColor: Integer; //height of "misc color" line
    FTabIndentArrowSize: Integer; //half-size of "arrow" mark
    FTabIndentArrowLeft: Integer; //space from scroll-arrows to left control edge
    FTabIndentArrowRight: Integer; //width of down-arrow area at right

    //show
    FTabShowClose: TATTabShowClose; //show mode for "x" buttons
    FTabShowPlus: boolean; //show "plus" tab
    FTabShowPlusText: atString; //text of "plus" tab
    FTabShowModifiedText: atString;
    FTabShowMenu: boolean; //show down arrow (menu of tabs)
    FTabShowBorderActiveLow: boolean; //show border line below active tab (like Firefox)
    FTabShowEntireColor: boolean;
    FTabMiddleClickClose: boolean; //enable close tab by middle-click
    FTabDoubleClickClose: boolean;
    FTabDoubleClickPlus: boolean; //enable call "+" tab with dbl-click on empty area
    FTabDragEnabled: boolean; //enable drag-drop
    FTabDragOutEnabled: boolean; //also enable drag-drop to another controls
    FTabDragCursor: TCursor;

    //otherrs
    FTabWidth: Integer;
    FTabIndex: Integer;
    FTabIndexOver: Integer;
    FTabIndexDrop: Integer;
    FTabList: TList;
    FTabMenu: TatPopupMenu;
    FTimerDrag: TTimer;

    FBitmap: TBitmap;
    FBitmapText: TBitmap;
    FOnTabClick: TNotifyEvent;
    FOnTabPlusClick: TNotifyEvent;
    FOnTabClose: TATTabCloseEvent;
    FOnTabMenu: TATTabMenuEvent;
    FOnTabDrawBefore: TATTabDrawEvent;
    FOnTabDrawAfter: TATTabDrawEvent;
    FOnTabEmpty: TNotifyEvent;
    FOnTabOver: TATTabOverEvent;
    FOnTabMove: TATTabMoveEvent;

    procedure DoPaintTo(C: TCanvas);
    procedure DoPaintBgTo(C: TCanvas; const ARect: TRect);
    procedure DoPaintTabTo(C: TCanvas; ARect: TRect; const ACaption: atString;
      ATabBg, ATabBorder, ATabBorderLow, ATabHilite, ATabCloseBg,
  ATabCloseBorder, ATabCloseXMark: TColor; ACloseBtn, AModified: boolean);
    procedure DoPaintArrowTo(C: TCanvas; ATyp: TATTriType; ARect: TRect;
      AColorArr, AColorBg: TColor);
    procedure DoPaintXTo(C: TCanvas; const R: TRect; ATabBg, ATabCloseBg,
      ATabCloseBorder, ATabCloseXMark: TColor);
    procedure DoPaintDropMark(C: TCanvas);
    function IsMousePressed: boolean;
    procedure SetTabIndex(AIndex: Integer);
    procedure GetTabCloseColor(AIndex: Integer; const ARect: TRect; var AColorXBg,
      AColorXBorder, AColorXMark: TColor);
    function IsIndexOk(AIndex: Integer): boolean;
    function IsShowX(AIndex: Integer): boolean;
    function IsPaintNeeded(AElemType: TATTabElemType;
      AIndex: Integer; ACanvas: TCanvas; const ARect: TRect): boolean;
    function DoPaintAfter(AElemType: TATTabElemType;
      AIndex: Integer; ACanvas: TCanvas; const ARect: TRect): boolean;
    procedure TabMenuClick(Sender: TObject);
    function GetTabWidth_Plus_Raw: Integer;
    procedure DoUpdateTabWidths;
    procedure DoTabDrop;
    procedure DoTabDropToOtherControl(ATarget: TControl; const APnt: TPoint);
    procedure TimerDragTimer(Sender: TObject);
  public
    constructor Create(AOnwer: TComponent); override;
    destructor Destroy; override;
    function GetTabRectWidth(APlusBtn: boolean): Integer;
    function GetTabRect(AIndex: Integer): TRect;
    function GetTabRect_Plus: TRect;
    function GetTabRect_X(const ARect: TRect): TRect;
    procedure GetArrowRect(var RDown: TRect);
    function GetTabAt(X, Y: Integer): Integer;
    function GetTabData(AIndex: Integer): TATTabData;
    function TabCount: Integer;
    property TabIndex: Integer read FTabIndex write SetTabIndex;
    procedure AddTab(
      AIndex: Integer;
      const ACaption: atString;
      AObject: TObject = nil;
      AModified: boolean = false;
      AColor: TColor = clNone);
    function DeleteTab(AIndex: Integer; AAllowEvent, AWithCancelBtn: boolean): boolean;
    procedure ShowTabMenu;
    procedure SwitchTab(ANext: boolean);
    procedure MoveTab(AFrom, ATo: integer; AActivateThen: boolean);
  protected
    procedure Paint; override;
    procedure Resize; override;
    procedure DblClick; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure CMMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
    {$ifdef windows}
    procedure WMEraseBkgnd(var Message: TMessage); message WM_ERASEBKGND;
    {$endif}
  published
    //colors
    property ColorBg: TColor read FColorBg write FColorBg;
    property ColorDrop: TColor read FColorDrop write FColorDrop;
    property ColorBorderActive: TColor read FColorBorderActive write FColorBorderActive;
    property ColorBorderPassive: TColor read FColorBorderPassive write FColorBorderPassive;
    property ColorTabActive: TColor read FColorTabActive write FColorTabActive;
    property ColorTabPassive: TColor read FColorTabPassive write FColorTabPassive;
    property ColorTabOver: TColor read FColorTabOver write FColorTabOver;
    property ColorFontModified: TColor read FColorFontModified write FColorFontModified;
    property ColorCloseBg: TColor read FColorCloseBg write FColorCloseBg;
    property ColorCloseBgOver: TColor read FColorCloseBgOver write FColorCloseBgOver;
    property ColorCloseBorderOver: TColor read FColorCloseBorderOver write FColorCloseBorderOver;
    property ColorCloseX: TColor read FColorCloseX write FColorCloseX;
    property ColorCloseXOver: TColor read FColorCloseXOver write FColorCloseXOver;
    property ColorArrow: TColor read FColorArrow write FColorArrow;
    property ColorArrowOver: TColor read FColorArrowOver write FColorArrowOver;
    //spaces
    property TabBottom: boolean read FTabBottom write FTabBottom;
    property TabAngle: Integer read FTabAngle write FTabAngle;
    property TabHeight: Integer read FTabHeight write FTabHeight;
    property TabWidthMin: Integer read FTabWidthMin write FTabWidthMin;
    property TabWidthMax: Integer read FTabWidthMax write FTabWidthMax;
    property TabNumPrefix: atString read FTabNumPrefix write FTabNumPrefix;
    property TabIndentDropI: Integer read FTabIndentDropI write FTabIndentDropI;
    property TabIndentInter: Integer read FTabIndentInter write FTabIndentInter;
    property TabIndentInit: Integer read FTabIndentInit write FTabIndentInit;
    property TabIndentLeft: Integer read FTabIndentLeft write FTabIndentLeft;
    property TabIndentText: Integer read FTabIndentText write FTabIndentText;
    property TabIndentTop: Integer read FTabIndentTop write FTabIndentTop;
    property TabIndentXRight: Integer read FTabIndentXRight write FTabIndentXRight;
    property TabIndentXInner: Integer read FTabIndentXInner write FTabIndentXInner;
    property TabIndentXSize: Integer read FTabIndentXSize write FTabIndentXSize;
    property TabIndentColor: Integer read FTabIndentColor write FTabIndentColor;
    property TabIndentArrowSize: Integer read FTabIndentArrowSize write FTabIndentArrowSize;
    property TabIndentArrowLeft: Integer read FTabIndentArrowLeft write FTabIndentArrowLeft;
    property TabIndentArrowRight: Integer read FTabIndentArrowRight write FTabIndentArrowRight;

    property TabShowClose: TATTabShowClose read FTabShowClose write FTabShowClose;
    property TabShowPlus: boolean read FTabShowPlus write FTabShowPlus;
    property TabShowPlusText: atString read FTabShowPlusText write FTabShowPlusText;
    property TabShowModifiedText: atString read FTabShowModifiedText write FTabShowModifiedText;
    property TabShowMenu: boolean read FTabShowMenu write FTabShowMenu;
    property TabShowBorderActiveLow: boolean read FTabShowBorderActiveLow write FTabShowBorderActiveLow;
    property TabShowEntireColor: boolean read FTabShowEntireColor write FTabShowEntireColor;
    property TabMiddleClickClose: boolean read FTabMiddleClickClose write FTabMiddleClickClose;
    property TabDoubleClickClose: boolean read FTabDoubleClickClose write FTabDoubleClickClose;
    property TabDoubleClickPlus: boolean read FTabDoubleClickPlus write FTabDoubleClickPlus;
    property TabDragEnabled: boolean read FTabDragEnabled write FTabDragEnabled;
    property TabDragOutEnabled: boolean read FTabDragOutEnabled write FTabDragOutEnabled;
    property TabDragCursor: TCursor read FTabDragCursor write FTabDragCursor;

    //events
    property OnTabClick: TNotifyEvent read FOnTabClick write FOnTabClick;
    property OnTabPlusClick: TNotifyEvent read FOnTabPlusClick write FOnTabPlusClick;
    property OnTabClose: TATTabCloseEvent read FOnTabClose write FOnTabClose;
    property OnTabMenu: TATTabMenuEvent read FOnTabMenu write FOnTabMenu;
    property OnTabDrawBefore: TATTabDrawEvent read FOnTabDrawBefore write FOnTabDrawBefore;
    property OnTabDrawAfter: TATTabDrawEvent read FOnTabDrawAfter write FOnTabDrawAfter;
    property OnTabEmpty: TNotifyEvent read FOnTabEmpty write FOnTabEmpty;
    property OnTabOver: TATTabOverEvent read FOnTabOver write FOnTabOver;
    property OnTabMove: TATTabMoveEvent read FOnTabMove write FOnTabMove;
  end;

implementation

uses
  SysUtils,
  StrUtils,
  Dialogs,
  Forms,
  Math;

function _FindControl(Pnt: TPoint): TControl;
begin
  {$ifdef FPC}
  Result:= FindControlAtPosition(Pnt, false);
  {$else}
  Result:= FindVCLWindow(Pnt);
  {$endif}
end;

function PtInControl(Control: TControl; const ScreenPnt: TPoint): boolean;
begin
  Result:= PtInRect(Control.ClientRect, Control.ScreenToClient(ScreenPnt));
end;

procedure DrawAntialisedLine(Canvas: TCanvas; const AX1, AY1, AX2, AY2: {real}Integer; const LineColor: TColor);
// http://stackoverflow.com/a/3613953/1789574
var
  swapped: boolean;

  procedure plot(const x, y, c: real);
  var
    resclr: TColor;
  begin
    if swapped then
      resclr := Canvas.Pixels[round(y), round(x)]
    else
      resclr := Canvas.Pixels[round(x), round(y)];
    resclr := RGB(round(GetRValue(resclr) * (1-c) + GetRValue(LineColor) * c),
                  round(GetGValue(resclr) * (1-c) + GetGValue(LineColor) * c),
                  round(GetBValue(resclr) * (1-c) + GetBValue(LineColor) * c));
    if swapped then
      Canvas.Pixels[round(y), round(x)] := resclr
    else
      Canvas.Pixels[round(x), round(y)] := resclr;
  end;

  function rfrac(const x: real): real;
  begin
    rfrac := 1 - frac(x);
  end;

  procedure swap(var a, b: real);
  var
    tmp: real;
  begin
    tmp := a;
    a := b;
    b := tmp;
  end;

var
  x1, x2, y1, y2, dx, dy, gradient, xend, yend, xgap, xpxl1, ypxl1,
  xpxl2, ypxl2, intery: real;
  x: integer;

begin
  //speed up drawing (AT)
  if (AX1 = AX2) or (AY1 = AY2) then
  begin
    Canvas.Pen.Width:= 1;
    Canvas.Pen.Color:= LineColor;
    if (AX1 = AX2) then
    begin
      Canvas.MoveTo(AX1, AY1);
      Canvas.LineTo(AX2, AY2+1);
    end
    else
    begin
      Canvas.MoveTo(AX1, AY1);
      Canvas.LineTo(AX2+1, AY2);
    end;
    Exit
  end;

  x1 := AX1;
  x2 := AX2;
  y1 := AY1;
  y2 := AY2;

  dx := x2 - x1;
  dy := y2 - y1;

  swapped := abs(dx) < abs(dy);
  if swapped then
  begin
    swap(x1, y1);
    swap(x2, y2);
    swap(dx, dy);
  end;
  if x2 < x1 then
  begin
    swap(x1, x2);
    swap(y1, y2);
  end;

  gradient := dy / dx;

  xend := round(x1);
  yend := y1 + gradient * (xend - x1);
  xgap := rfrac(x1 + 0.5);
  xpxl1 := xend;
  ypxl1 := floor(yend);
  plot(xpxl1, ypxl1, rfrac(yend) * xgap);
  plot(xpxl1, ypxl1 + 1, frac(yend) * xgap);
  intery := yend + gradient;

  xend := round(x2);
  yend := y2 + gradient * (xend - x2);
  xgap := frac(x2 + 0.5);
  xpxl2 := xend;
  ypxl2 := floor(yend);
  plot(xpxl2, ypxl2, rfrac(yend) * xgap);
  plot(xpxl2, ypxl2 + 1, frac(yend) * xgap);

  for x := round(xpxl1) + 1 to round(xpxl2) - 1 do
  begin
    plot(x, floor(intery), rfrac(intery));
    plot(x, floor(intery) + 1, frac(intery));
    intery := intery + gradient;
  end;

end;

procedure DrawTriangleRaw(C: TCanvas; const P1, P2, P3: TPoint; Color: TColor);
//optimize later, make antialiased draw
begin
  C.Brush.Color:= Color;
  C.Pen.Color:= Color;
  C.Polygon([P1, P2, P3]);
end;

procedure DrawTriangleType(C: TCanvas; Typ: TATTriType; const R: TRect; Color: TColor);
var
  P1, P2, P3: TPoint;
begin
  //P1/P2: points of vert/horz line
  //P3: end point at arrow direction
  case Typ of
    triDown:
    begin
      P1:= Point(R.Left, R.Top);
      P2:= Point(R.Right, R.Top);
      P3:= Point((R.Left+R.Right) div 2, R.Bottom);
    end;
    triRight:
    begin
      P1:= Point(R.Left, R.Top);
      P2:= Point(R.Left, R.Bottom);
      P3:= Point(R.Right, (R.Top+R.Bottom) div 2);
    end;
    triLeft:
    begin
      P1:= Point(R.Right, R.Top);
      P2:= Point(R.Right, R.Bottom);
      P3:= Point(R.Left, (R.Top+R.Bottom) div 2);
    end;
  end;

  DrawTriangleRaw(C, P1, P2, P3, Color);
end;

{ TATTabs }

function TATTabs.IsIndexOk(AIndex: Integer): boolean;
begin
  Result:= (AIndex>=0) and (AIndex<FTabList.Count);
end;

function TATTabs.TabCount: Integer;
begin
  Result:= FTabList.Count;
end;

constructor TATTabs.Create(AOnwer: TComponent);
begin
  inherited;

  Caption:= '';
  BorderStyle:= bsNone;
  ControlStyle:= ControlStyle+[csOpaque];

  Width:= 400;
  Height:= 35;

  FMouseDown:= false;
  FMouseDownPnt:= Point(0, 0);
  FMouseDownDbl:= false;
  FMouseDrag:= false;

  FColorBg:= clBlack;
  FColorDrop:= $6060E0;
  FColorTabActive:= $808080;
  FColorTabPassive:= $786868;
  FColorTabOver:= $A08080;
  FColorFontModified:= $A00000;
  FColorBorderActive:= $A0A0A0;
  FColorBorderPassive:= $A07070;
  FColorCloseBg:= clNone;
  FColorCloseBgOver:= $6060E0;
  FColorCloseBorderOver:= FColorCloseBgOver;
  FColorCloseX:= clLtGray;
  FColorCloseXOver:= clWhite;
  FColorArrow:= $999999;
  FColorArrowOver:= $E0E0E0;

  FTabBottom:= false;
  FTabAngle:= 5;
  FTabHeight:= 24;
  FTabWidthMin:= 20;
  FTabWidthMax:= 130;
  FTabWidthHideX:= 55;
  FTabNumPrefix:= '';
  FTabDragCursor:= crDrag;
  FTabIndentLeft:= 6;
  FTabIndentDropI:= 4;
  FTabIndentInter:= 0;
  FTabIndentInit:= 4;
  FTabIndentTop:= 5;
  FTabIndentText:= 6;
  FTabIndentXRight:= 10;
  FTabIndentXInner:= 3;
  FTabIndentXSize:= 12;
  FTabIndentArrowSize:= 4;
  FTabIndentArrowLeft:= 4;
  FTabIndentArrowRight:= 20;
  FTabIndentColor:= 3;
  
  FTabShowClose:= tbShowAll;
  FTabShowPlus:= true;
  FTabShowPlusText:= ' + ';
  FTabShowModifiedText:= '*';
  FTabShowMenu:= true;
  FTabShowBorderActiveLow:= false;
  FTabShowEntireColor:= false;
  FTabMiddleClickClose:= false;
  FTabDoubleClickClose:= true;
  FTabDoubleClickPlus:= false;
  FTabDragEnabled:= true;
  FTabDragOutEnabled:= true;

  FBitmap:= TBitmap.Create;
  FBitmap.PixelFormat:= pf24bit;
  FBitmap.Width:= 1600;
  FBitmap.Height:= 60;

  FBitmapText:= TBitmap.Create;
  FBitmapText.PixelFormat:= pf24bit;
  FBitmapText.Width:= 600;
  FBitmapText.Height:= 60;

  Font.Name:= 'Tahoma';
  Font.Color:= $E0E0E0;
  Font.Size:= 8;

  FTabIndex:= 0;
  FTabIndexOver:= -1;
  FTabList:= TList.Create;
  FTabMenu:= nil;

  FTimerDrag:= TTimer.Create(Self);
  FTimerDrag.Enabled:= false;
  FTimerDrag.Interval:= 200;
  FTimerDrag.OnTimer:= TimerDragTimer;

  FOnTabClick:= nil;
  FOnTabPlusClick:= nil;
  FOnTabClose:= nil;
  FOnTabMenu:= nil;
  FOnTabDrawBefore:= nil;
  FOnTabDrawAfter:= nil;
end;

destructor TATTabs.Destroy;
var
  i: Integer;
begin
  for i:= TabCount-1 downto 0 do
  begin
    TObject(FTabList[i]).Free;
    FTabList[i]:= nil;
  end;
  FreeAndNil(FTabList);

  FreeAndNil(FBitmapText);
  FreeAndNil(FBitmap);
  inherited;
end;

procedure TATTabs.Paint;
begin
  if Assigned(FBitmap) then
  begin
    DoPaintTo(FBitmap.Canvas);
    Canvas.CopyRect(ClientRect, FBitmap.Canvas, ClientRect);
  end;
end;

procedure TATTabs.DoPaintTabTo(
  C: TCanvas; ARect: TRect; const ACaption: atString;
  ATabBg, ATabBorder, ATabBorderLow, ATabHilite, ATabCloseBg, ATabCloseBorder, ATabCloseXMark: TColor;
  ACloseBtn, AModified: boolean);
var
  PL1, PL2, PR1, PR2: TPoint;
  RText: TRect;
  NIndentL, NIndentR: Integer;
  AType: TATTabElemType;
  AInvert: Integer;
  TempCaption: atString;
begin
  if FTabShowEntireColor and (ATabHilite<>clNone) then
    ATabBg:= ATabHilite;

  C.Pen.Color:= ATabBg;
  C.Brush.Color:= ATabBg;

  if FTabBottom then
    AInvert:= -1
  else
    AInvert:= 1;

  NIndentL:= FTabAngle+FTabIndentLeft;
  NIndentR:= NIndentL+IfThen(ACloseBtn, FTabIndentXRight);
  RText:= Rect(ARect.Left+FTabAngle, ARect.Top, ARect.Right-FTabAngle, ARect.Bottom);
  C.FillRect(RText);
  RText:= Rect(ARect.Left+NIndentL, ARect.Top, ARect.Right-NIndentR, ARect.Bottom);

  //left triangle
  PL1:= Point(ARect.Left+FTabAngle*AInvert, ARect.Top);
  PL2:= Point(ARect.Left-FTabAngle*AInvert, ARect.Bottom-1);
  if FTabAngle>0 then
  begin
    //DrawTriangleRaw(C, PL1, PL2, Point(PL1.X, PL2.Y), ATabBg);
    //draw little shifted line- bottom-left point x+=1
    if FTabBottom then
      DrawTriangleRaw(C, PL1, Point(PL2.X+1, PL2.Y), Point(PL2.X, PL1.Y), ATabBg)
    else
      DrawTriangleRaw(C, PL1, Point(PL2.X+1, PL2.Y), Point(PL1.X, PL2.Y), ATabBg);
  end;

  //right triangle
  PR1:= Point(ARect.Right-FTabAngle*AInvert-1, ARect.Top);
  PR2:= Point(ARect.Right+FTabAngle*AInvert-1, ARect.Bottom-1);
  if FTabAngle>0 then
  begin
    //DrawTriangleRaw(C, PR1, PR2, Point(PR1.X, PR2.Y), ATabBg);
    //draw little shifted line- bottom-right point x-=1
    if FTabBottom then
      DrawTriangleRaw(C, PR1, Point(PR2.X-1, PR2.Y), Point(PR2.X, PR1.Y), ATabBg)
    else
      DrawTriangleRaw(C, PR1, Point(PR2.X-1, PR2.Y), Point(PR1.X, PR2.Y), ATabBg);
  end;

  //caption
  FBitmapText.Canvas.Brush.Color:= ATabBg;
  FBitmapText.Canvas.FillRect(Rect(0, 0, FBitmapText.Width, FBitmapText.Height));
  FBitmapText.Canvas.Font.Assign(Self.Font);

  if AModified then
    FBitmapText.Canvas.Font.Color:= FColorFontModified;
  TempCaption:= IfThen(AModified, FTabShowModifiedText) + ACaption;

  {$ifdef WIDE}
  Windows.TextOutW(FBitmapText.Canvas.Handle, 0, FTabIndentText, PWideChar(TempCaption), Length(TempCaption));
  {$else}
  FBitmapText.Canvas.TextOut(0, FTabIndentText, TempCaption);
  {$endif}

  C.CopyRect(RText, FBitmapText.Canvas,
    Rect(0, 0, RText.Right-RText.Left, RText.Bottom-RText.Top));

  //borders
  if FTabBottom then
  begin
    DrawAntialisedLine(C, PL1.X, PL1.Y, PL2.X, PL2.Y+1, ATabBorder);
    DrawAntialisedLine(C, PR1.X, PR1.Y, PR2.X, PR2.Y+1, ATabBorder);
    DrawAntialisedLine(C, PL2.X, PL2.Y+1, PR2.X, PL2.Y+1, ATabBorder);
    if ATabBorderLow<>clNone then
      DrawAntialisedLine(C, PL1.X, ARect.Top, PR1.X, ARect.Top, ATabBorderLow)
  end
  else
  begin
    DrawAntialisedLine(C, PL1.X, PL1.Y, PL2.X, PL2.Y+1, ATabBorder);
    DrawAntialisedLine(C, PR1.X, PR1.Y, PR2.X, PR2.Y+1, ATabBorder);
    DrawAntialisedLine(C, PL1.X, PL1.Y, PR1.X, PL1.Y, ATabBorder);
    if ATabBorderLow<>clNone then
      DrawAntialisedLine(C, PL2.X, ARect.Bottom, PR2.X, ARect.Bottom, ATabBorderLow)
    else
      DrawAntialisedLine(C, PL2.X+1, ARect.Bottom, PR2.X-1, ARect.Bottom, ATabBg);
  end;

  //color mark
  if ATabHilite<>clNone then
  begin
    C.Brush.Color:= ATabHilite;
    if FTabBottom then
      C.FillRect(Rect(PL2.X+1, PL2.Y-2, PR2.X, PR2.Y-2+FTabIndentColor))
    else
      C.FillRect(Rect(PL1.X+1, PL1.Y+1, PR1.X, PR1.Y+1+FTabIndentColor));
    C.Brush.Color:= ATabBg;
  end;

  //"close" button
  if ACloseBtn then
  begin
    if ATabCloseBg<>clNone then
      AType:= aeXButtonOver
    else
      AType:= aeXButton;
    RText:= GetTabRect_X(ARect);
    if IsPaintNeeded(AType, -1, C, RText) then
    begin
      DoPaintXTo(C, RText, ATabBg, ATabCloseBg, ATabCloseBorder, ATabCloseXMark);
      DoPaintAfter(AType, -1, C, RText);
    end;
  end;
end;

procedure TATTabs.DoPaintXTo(C: TCanvas; const R: TRect;
  ATabBg, ATabCloseBg, ATabCloseBorder, ATabCloseXMark: TColor);
var
  PX1, PX2, PX3, PX4, PXX1, PXX2: TPoint;
begin
  C.Brush.Color:= IfThen(ATabCloseBg<>clNone, ATabCloseBg, ATabBg);
  C.FillRect(R);
  C.Pen.Color:= IfThen(ATabCloseBorder<>clNone, ATabCloseBorder, ATabBg);
  C.Rectangle(R);
  C.Brush.Color:= ATabBg;

  //paint cross by 2 polygons, each has 6 points (3 points at line edge)
  C.Brush.Color:= ATabCloseXMark;
  C.Pen.Color:= ATabCloseXMark;

  PXX1:= Point(R.Left+FTabIndentXInner, R.Top+FTabIndentXInner);
  PXX2:= Point(R.Right-FTabIndentXInner-1, R.Bottom-FTabIndentXInner-1);
  PX1:= Point(PXX1.X+1, PXX1.Y);
  PX2:= Point(PXX1.X, PXX1.Y+1);
  PX3:= Point(PXX2.X-1, PXX2.Y);
  PX4:= Point(PXX2.X, PXX2.Y-1);
  C.Polygon([PX1, PXX1, PX2, PX3, PXX2, PX4]);

  PXX1:= Point(R.Right-FTabIndentXInner-1, R.Top+FTabIndentXInner);
  PXX2:= Point(R.Left+FTabIndentXInner, R.Bottom-FTabIndentXInner-1);
  PX1:= Point(PXX1.X-1, PXX1.Y);
  PX2:= Point(PXX1.X, PXX1.Y+1);
  PX3:= Point(PXX2.X+1, PXX2.Y);
  PX4:= Point(PXX2.X, PXX2.Y-1);
  C.Polygon([PX1, PXX1, PX2, PX3, PXX2, PX4]);

  C.Brush.Color:= ATabBg;
end;

function TATTabs.GetTabWidth_Plus_Raw: Integer;
begin
  Canvas.Font.Assign(Self.Font);
  Result:= Canvas.TextWidth(FTabShowPlusText);
end;

function TATTabs.GetTabRectWidth(APlusBtn: boolean): Integer;
begin
  if APlusBtn then
    Result:= GetTabWidth_Plus_Raw
  else
    Result:= FTabWidthMax;

  Inc(Result, 2*(FTabAngle + FTabIndentLeft));
end;


function TATTabs.GetTabRect(AIndex: Integer): TRect;
var
  i: Integer;
begin
  Result.Left:= FTabIndentInit+FTabAngle;
  Result.Right:= Result.Left;
  Result.Top:= FTabIndentTop;
  Result.Bottom:= Result.Top+FTabHeight;

  if IsIndexOk(AIndex) then
    for i:= 0 to TabCount-1 do
    begin
      Result.Left:= Result.Right + FTabIndentInter;
      Result.Right:= Result.Left + FTabWidth;
      if AIndex=i then Exit;
    end;
end;

function TATTabs.GetTabRect_Plus: TRect;
begin
  Result:= GetTabRect(TabCount-1);
  Result.Left:= Result.Right + FTabIndentInter;
  Result.Right:= Result.Left + GetTabRectWidth(true);
end;

function TATTabs.GetTabRect_X(const ARect: TRect): TRect;
var
  P: TPoint;
begin
  P:= Point(
    ARect.Right-FTabAngle-FTabIndentXRight,
    (ARect.Top+ARect.Bottom) div 2 + 1);
  Dec(P.X, FTabIndentXSize div 2);
  Dec(P.Y, FTabIndentXSize div 2);
  Result:= Rect(
    P.X,
    P.Y,
    P.X+FTabIndentXSize,
    P.Y+FTabIndentXSize);
end;

procedure TATTabs.GetTabCloseColor(AIndex: Integer; const ARect: TRect;
  var AColorXBg, AColorXBorder, AColorXMark: TColor);
var
  P: TPoint;
begin
  AColorXBg:= FColorCloseBg;
  AColorXBorder:= FColorCloseBg;
  AColorXMark:= FColorCloseX;

  if FMouseDrag then Exit;

  if IsShowX(AIndex) then
    if AIndex=FTabIndexOver then
    begin
      P:= Mouse.CursorPos;
      P:= ScreenToClient(P);
      if PtInRect(GetTabRect_X(ARect), P) then
      begin
        AColorXBg:= FColorCloseBgOver;
        AColorXBorder:= FColorCloseBorderOver;
        AColorXMark:= FColorCloseXOver;
      end;
    end;
end;

function TATTabs.IsPaintNeeded(AElemType: TATTabElemType;
  AIndex: Integer; ACanvas: TCanvas; const ARect: TRect): boolean;
begin
  Result:= true;
  if Assigned(FOnTabDrawBefore) then
    FOnTabDrawBefore(Self, AElemType, AIndex, ACanvas, ARect, Result);
end;

function TATTabs.DoPaintAfter(AElemType: TATTabElemType;
  AIndex: Integer; ACanvas: TCanvas; const ARect: TRect): boolean;
begin
  Result:= true;
  if Assigned(FOnTabDrawAfter) then
    FOnTabDrawAfter(Self, AElemType, AIndex, ACanvas, ARect, Result);
end;

procedure TATTabs.DoPaintBgTo(C: TCanvas; const ARect: TRect);
begin
  C.Brush.Color:= FColorBg;
  C.FillRect(ARect);
end;

procedure TATTabs.DoPaintTo(C: TCanvas);
var
  i: Integer;
  RBottom: TRect;
  AColorXBg, AColorXBorder, AColorXMark: TColor;
  ARect, ARectDown: TRect;
  AType: TATTabElemType;
begin
  AType:= aeBackground;
  ARect:= ClientRect;

  //painting of BG is little different then other elements:
  //paint fillrect anyway, then maybe paint ownerdraw
  DoPaintBgTo(C, ARect);
  if IsPaintNeeded(AType, -1, C, ARect) then
  begin
    DoPaintAfter(AType, -1, C, ARect);
  end;

  DoUpdateTabWidths;

  //paint bottom rect
  if not FTabBottom then
  begin
    RBottom:= Rect(0, FTabIndentTop+FTabHeight, ClientWidth, ClientHeight);
    C.Brush.Color:= FColorTabActive;
    C.FillRect(RBottom);
    DrawAntialisedLine(C, 0, RBottom.Top, ClientWidth, RBottom.Top, FColorBorderActive);
  end
  else
  begin
    RBottom:= Rect(0, 0, ClientWidth, FTabIndentTop);
    C.Brush.Color:= FColorTabActive;
    C.FillRect(RBottom);
    DrawAntialisedLine(C, 0, RBottom.Bottom, ClientWidth, RBottom.Bottom, FColorBorderActive);
  end;

  //paint "plus" tab
  if FTabShowPlus then
  begin
    ARect:= GetTabRect_Plus;
    AColorXBg:= clNone;
    AColorXBorder:= clNone;
    if FTabIndexOver=cAtTabPlus then
      AType:= aeTabPlusOver
    else
      AType:= aeTabPlus;
    if IsPaintNeeded(AType, -1, C, ARect) then
    begin
      DoPaintTabTo(C, ARect,
        FTabShowPlusText,
        IfThen((FTabIndexOver=cAtTabPlus) and not FMouseDrag, FColorTabOver, FColorTabPassive),
        FColorBorderPassive,
        FColorBorderActive,
        clNone,
        AColorXBg,
        AColorXBorder,
        AColorXMark,
        false,
        false
        );
      DoPaintAfter(AType, -1, C, ARect);
    end;    
  end;

  //paint passive tabs
  for i:= TabCount-1 downto 0 do
    if i<>FTabIndex then
    begin
      ARect:= GetTabRect(i);
      GetTabCloseColor(i, ARect, AColorXBg, AColorXBorder, AColorXMark);
      if i=FTabIndexOver then
        AType:= aeTabPassiveOver
      else
        AType:= aeTabPassive;
      if IsPaintNeeded(AType, i, C, ARect) then
      begin
        DoPaintTabTo(C, ARect,
          Format(FTabNumPrefix, [i+1]) +
            TATTabData(FTabList[i]).TabCaption,
          IfThen((i=FTabIndexOver) and not FMouseDrag, FColorTabOver, FColorTabPassive),
          FColorBorderPassive,
          FColorBorderActive,
          TATTabData(FTabList[i]).TabColor,
          AColorXBg,
          AColorXBorder,
          AColorXMark,
          IsShowX(i),
          TATTabData(FTabList[i]).TabModified
          );
        DoPaintAfter(AType, i, C, ARect);
      end;
    end;

  //paint active tab
  i:= FTabIndex;
  if IsIndexOk(i) then
  begin
    ARect:= GetTabRect(i);
    GetTabCloseColor(i, ARect, AColorXBg, AColorXBorder, AColorXMark);
    if IsPaintNeeded(aeTabActive, i, C, ARect) then
    begin
      DoPaintTabTo(C, ARect,
        Format(FTabNumPrefix, [i+1]) +
          TATTabData(FTabList[i]).TabCaption,
        FColorTabActive,
        FColorBorderActive,
        IfThen(FTabShowBorderActiveLow, FColorBorderActive, clNone),
        TATTabData(FTabList[i]).TabColor,
        AColorXBg,
        AColorXBorder,
        AColorXMark,
        IsShowX(i),
        TATTabData(FTabList[i]).TabModified
        );
      DoPaintAfter(aeTabActive, i, C, ARect);
    end;  
  end;

  //paint arrows
  GetArrowRect(ARectDown);

  if FTabShowMenu then
  begin
    DoPaintArrowTo(C, triDown, ARectDown,
      IfThen((FTabIndexOver=cAtArrowDown) and not FMouseDrag, FColorArrowOver, FColorArrow), FColorBg);
  end;

  //paint drop mark
  if FMouseDrag then
  begin
    if PtInControl(Self, Mouse.CursorPos) then
      DoPaintDropMark(C);
  end;
end;

procedure TATTabs.DoPaintDropMark(C: TCanvas);
var
  i: Integer;
  R: TRect;
begin
  i:= FTabIndexDrop;
  if i<0 then i:= TabCount-1;
  if i<>FTabIndex then
  begin
    R:= GetTabRect(i);
    R.Left:= IfThen(i<=FTabIndex, R.Left, R.Right);
    R.Left:= R.Left - FTabIndentDropI div 2;
    R.Right:= R.Left + FTabIndentDropI - FTabIndentDropI div 2 + 1;
    C.Brush.Color:= FColorDrop;
    C.FillRect(R);
  end;
end;


function TATTabs.GetTabAt(X, Y: Integer): Integer;
var
  i: Integer;
  Pnt: TPoint;
  RDown: TRect;
begin
  Result:= -1;
  Pnt:= Point(X, Y);

  //arrows?
  GetArrowRect(RDown);

  {
  if FTabShowScroll then
    if PtInRect(RLeft, Pnt) then
    begin
      Result:= cAtArrowLeft;
      Exit
    end;

  if FTabShowScroll then
    if PtInRect(RRight, Pnt) then
    begin
      Result:= cAtArrowRight;
      Exit
    end;
  }  

  if FTabShowMenu then
    if PtInRect(RDown, Pnt) then
    begin
      Result:= cAtArrowDown;
      Exit
    end;

  //normal tab?
  for i:= 0 to TabCount-1 do
    if PtInRect(GetTabRect(i), Pnt) then
    begin
      Result:= i;
      Exit;
    end;

  //plus tab?
  if FTabShowPlus then
    if PtInRect(GetTabRect_Plus, Pnt) then
    begin
      Result:= cAtTabPlus;
      Exit
    end;
end;

procedure TATTabs.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FMouseDown:= false;
  FMouseDownPnt:= Point(0, 0);
  FTimerDrag.Enabled:= false;
  Cursor:= crDefault;
  Screen.Cursor:= crDefault;

  //do drop?
  if FMouseDrag then
  begin
    FMouseDrag:= false;
    if (FTabIndexDrop>=0) then
    begin
      DoTabDrop;
      Invalidate;
      Exit
    end;
  end;

  //dbl click?
  if FMouseDownDbl then
  begin
    FMouseDownDbl:= false;

    if FTabDoubleClickClose and (FTabIndexOver>=0) then
      DeleteTab(FTabIndexOver, true, true)
    else
    if FTabDoubleClickPlus and (FTabIndexOver=-1) then
      if Assigned(FOnTabPlusClick) then
        FOnTabPlusClick(Self);
    Exit
  end;
end;


procedure TATTabs.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  R: TRect;
begin
  FMouseDown:= true;
  FMouseDownPnt:= Point(X, Y);
  FTabIndexOver:= GetTabAt(X, Y);

  if Button=mbMiddle then
  begin
    if FTabMiddleClickClose then
      if FTabIndexOver>=0 then
        DeleteTab(FTabIndexOver, true, true);
    Exit;
  end;

  if Button=mbLeft then
  begin
    case FTabIndexOver of
      cAtArrowDown:
        begin
          FMouseDown:= false;
          FTabIndexOver:= -1;
          Invalidate;
          ShowTabMenu;
          Exit
        end;

      cAtTabPlus:
        begin
          FMouseDown:= false;
          FTabIndexOver:= -1;
          if Assigned(FOnTabPlusClick) then
            FOnTabPlusClick(Self);
          Exit;
        end;

      else
        begin
          if IsShowX(FTabIndexOver) then
          begin
            R:= GetTabRect(FTabIndexOver);
            R:= GetTabRect_X(R);
            if PtInRect(R, Point(X, Y)) then
            begin
              DeleteTab(FTabIndexOver, true, true);
              Exit
            end;
          end;
          SetTabIndex(FTabIndexOver);

          if FTabDragEnabled then
            FTimerDrag.Enabled:= true;
        end;
    end;
  end;
end;

type
  TControl2 = class(TControl);

procedure TATTabs.MouseMove(Shift: TShiftState; X, Y: Integer);
const
  cDragMin = 10; //mouse must move by NN pixels to start drag
begin
  inherited;
  FTabIndexOver:= GetTabAt(X, Y);
  FTabIndexDrop:= FTabIndexOver;

  if Assigned(FOnTabOver) then
    FOnTabOver(Self, FTabIndexOver);

  if FMouseDown and FTabDragEnabled and (TabCount>0) then
  begin
    if (Abs(X-FMouseDownPnt.X)>cDragMin) or
       (Abs(Y-FMouseDownPnt.Y)>cDragMin) then
    begin
      FMouseDrag:= true;
      FTimerDrag.Enabled:= true;
    end;
  end;

  Invalidate;
end;

procedure TATTabs.Resize;
begin
  inherited;
  if Assigned(FBitmap) then
  begin
    FBitmap.Width:= Max(FBitmap.Width, Width);
    FBitmap.Height:= Max(FBitmap.Height, Height);
  end;
end;


procedure TATTabs.AddTab(
  AIndex: Integer;
  const ACaption: atString;
  AObject: TObject = nil;
  AModified: boolean = false;
  AColor: TColor = clNone);
var
  Data: TATTabData;
begin
  Data:= TATTabData.Create;
  Data.TabCaption:= ACaption;
  Data.TabObject:= AObject;
  Data.TabModified:= AModified;
  Data.TabColor:= AColor;

  if IsIndexOk(AIndex) then
    FTabList.Insert(AIndex, Data)
  else
  begin
    FTabList.Add(Data);
    AIndex:= TabCount-1;
  end;

  Invalidate;

  if Assigned(FOnTabMove) then
    FOnTabMove(Self, -1, AIndex);
end;

function TATTabs.DeleteTab(AIndex: Integer; AAllowEvent, AWithCancelBtn: boolean): boolean;
var
  CanClose, CanContinue: boolean;
begin
  FMouseDown:= false;

  if AAllowEvent then
  begin
    CanClose:= true;
    CanContinue:= AWithCancelBtn;

    if Assigned(FOnTabClose) then
      FOnTabClose(Self, AIndex, CanClose, CanContinue);

    if AWithCancelBtn and not CanContinue then
      begin Result:= false; Exit end;
    if not CanClose then
      begin Result:= true; Exit end;
  end;

  if IsIndexOk(AIndex) then
  begin
    TObject(FTabList[AIndex]).Free;
    FTabList.Delete(AIndex);

    //need to call OnTabClick
    if FTabIndex>AIndex then
      SetTabIndex(FTabIndex-1)
    else
    if (FTabIndex=AIndex) and (FTabIndex>0) and (FTabIndex>=TabCount) then
      SetTabIndex(FTabIndex-1)
    else
    if FTabIndex=AIndex then
      SetTabIndex(FTabIndex);

    Invalidate;

    if (TabCount=0) then
      if Assigned(FOnTabEmpty) then
        FOnTabEmpty(Self);

    if Assigned(FOnTabMove) then
      FOnTabMove(Self, AIndex, -1);    
  end;

  Result:= true;
end;

procedure TATTabs.SetTabIndex(AIndex: Integer);
begin
  if IsIndexOk(AIndex) then
  begin
    FTabIndex:= AIndex;
    Invalidate;
    if Assigned(FOnTabClick) then
      FOnTabClick(Self);
  end;
end;


function TATTabs.GetTabData(AIndex: Integer): TATTabData;
begin
  if IsIndexOk(AIndex) then
    Result:= TATTabData(FTabList[AIndex])
  else
    Result:= nil;
end;

{$ifdef windows}
//needed to remove flickering on resize and mouse-over
procedure TATTabs.WMEraseBkgnd(var Message: TMessage);
begin
  Message.Result:= 1;
end;
{$endif}

procedure TATTabs.DoPaintArrowTo(C: TCanvas; ATyp: TATTriType; ARect: TRect;
  AColorArr, AColorBg: TColor);
var
  P: TPoint;
  R: TRect;
  N, SizeX, SizeY: Integer;
begin
  N:= FTabIndentArrowSize;
  case ATyp of
    triLeft,
    triRight:
      begin
        SizeY:= N;
        SizeX:= N div 2;
      end;
    else
      begin
        SizeX:= N;
        SizeY:= N div 2;
      end;
  end;

  P:= CenterPoint(ARect);
  R:= Rect(P.X-SizeX, P.Y-SizeY, P.X+SizeX, P.Y+SizeY);
  DrawTriangleType(C, ATyp, R, AColorArr);
end;


procedure TATTabs.GetArrowRect(var RDown: TRect);
begin
  RDown.Top:= FTabIndentTop;
  RDown.Bottom:= RDown.Top+FTabHeight;

  RDown.Right:= ClientWidth;
  RDown.Left:= RDown.Right-FTabIndentArrowRight;
end;

procedure TATTabs.ShowTabMenu;
var
  i: Integer;
  mi: TatMenuItem;
  RDown: TRect;
  P: TPoint;
  bShow: boolean;
begin
  if TabCount=0 then Exit;

  bShow:= true;
  if Assigned(FOnTabMenu) then
    FOnTabMenu(Self, bShow);
  if not bShow then Exit;

  if not Assigned(FTabMenu) then
    FTabMenu:= TatPopupMenu.Create(Self);
  FTabMenu.Items.Clear;

  for i:= 0 to TabCount-1 do
  begin
    mi:= TatMenuItem.Create(Self);
    mi.Tag:= i;
    mi.Caption:= TATTabData(FTabList[i]).TabCaption;
    mi.OnClick:= TabMenuClick;
    //mi.RadioItem:= true; //bug in Lazarus/gtk2
    mi.Checked:= i=FTabIndex;
    FTabMenu.Items.Add(mi);
  end;

  GetArrowRect(RDown);
  P:= Point(RDown.Left, RDown.Bottom);
  P:= ClientToScreen(P);
  FTabMenu.Popup(P.X, P.Y);
end;

procedure TATTabs.TabMenuClick(Sender: TObject);
begin
  SetTabIndex((Sender as TComponent).Tag);
end;

procedure TATTabs.DoUpdateTabWidths;
var
  Value, Count: Integer;
begin
  Count:= TabCount;
  if Count=0 then Exit;

  //tricky formula: calculate auto-width
  Value:= (ClientWidth
    - IfThen(FTabShowPlus, GetTabWidth_Plus_Raw + 2*FTabIndentLeft + 1*FTabAngle)
    - FTabAngle*2
    - FTabIndentInter
    - FTabIndentInit
    - IfThen(FTabShowMenu, FTabIndentArrowRight)) div Count
      - FTabIndentInter;

  if Value<FTabWidthMin then
    Value:= FTabWidthMin
  else
  if Value>FTabWidthMax then
    Value:= FTabWidthMax;

  FTabWidth:= Value;
end;

function TATTabs.IsShowX(AIndex: Integer): boolean;
begin
  case FTabShowClose of
    tbShowNone: Result:= false;
    tbShowAll: Result:= true;
    tbShowActive: Result:= AIndex=FTabIndex;
    else Result:= false;
  end;

  //if (AIndex<>FTabIndex) then //let's hide x for all tabs
    if (FTabWidth<FTabWidthHideX) then
      Result:= false;
end;

procedure TATTabs.DoTabDrop;
var
  NFrom, NTo: Integer;
begin
  NFrom:= FTabIndex;
  if NFrom<0 then Exit;
  NTo:= FTabIndexDrop;
  if NTo<0 then NTo:= TabCount-1;
  if NFrom=NTo then Exit;  

  FTabList.Move(NFrom, NTo);
  SetTabIndex(NTo);

  if Assigned(FOnTabMove) then
    FOnTabMove(Self, NFrom, NTo);
end;

procedure TATTabs.MoveTab(AFrom, ATo: integer; AActivateThen: boolean);
begin
  if not IsIndexOk(AFrom) then exit;
  if not IsIndexOk(ATo) then exit;
  if AFrom=ATo then exit;

  FTabList.Move(AFrom, ATo);
  if AActivateThen then
    SetTabIndex(ATo);
end;

procedure TATTabs.DoTabDropToOtherControl(ATarget: TControl; const APnt: TPoint);
var
  ATabs: TATTabs;
  NTab, NTabTo: Integer;
  Data: TATTabData;
  P: TPoint;
begin
  if not (ATarget is TATTabs) then
  begin
    if Assigned(TControl2(ATarget).OnDragDrop) then
    begin
      P:= APnt;
      Data:= GetTabData(FTabIndex);
      if Data<>nil then
        TControl2(ATarget).OnDragDrop(ATarget, Data.TabObject, P.X, P.Y);
    end;
    Exit;
  end;  

  ATabs:= ATarget as TATTabs;
  if not ATabs.TabDragEnabled then Exit;

  NTab:= FTabIndex;
  NTabTo:= ATabs.GetTabAt(APnt.X, APnt.Y); //-1 is allowed

  Data:= GetTabData(NTab);
  if Data=nil then Exit;

  ATabs.AddTab(NTabTo, Data.TabCaption, Data.TabObject, Data.TabModified, Data.TabColor);

  //correct TabObject parent
  if Data.TabObject is TWinControl then
    if (Data.TabObject as TWinControl).Parent = Self.Parent then
      (Data.TabObject as TWinControl).Parent:= ATabs.Parent;

  //activate dropped tab
  if NTabTo<0 then
    ATabs.TabIndex:= ATabs.TabCount-1
  else
    ATabs.TabIndex:= NTabTo;  

  //delete old tab (don't call OnTabClose)
  DeleteTab(NTab, false{AllowEvent}, false);
end;

procedure TATTabs.CMMouseLeave(var Msg: TMessage);
begin
  inherited;
  FTabIndexOver:= -1;
  Invalidate;
end;

procedure TATTabs.SwitchTab(ANext: boolean);
begin
  if TabCount>1 then
    if ANext then
    begin
      if TabIndex=TabCount-1 then
        TabIndex:= 0
      else
        TabIndex:= TabIndex+1;
    end
    else
    begin
      if TabIndex=0 then
        TabIndex:= TabCount-1
      else
        TabIndex:= TabIndex-1;
    end;
end;

procedure TATTabs.DblClick;
begin
  FMouseDownDbl:= true;
end;

function TATTabs.IsMousePressed: boolean;
begin
  Result:= GetKeyState(vk_lbutton)<0;
end;

procedure TATTabs.TimerDragTimer(Sender: TObject);
var
  Pnt: TPoint;
  Ctl: TControl;
begin
  Pnt:= Mouse.CursorPos;

  //mouse not pressed: stop timer, do drop
  if not IsMousePressed then
  begin
    FTimerDrag.Enabled:= false;
    FMouseDown:= false;
    FMouseDrag:= false;
    Cursor:= crDefault;
    Screen.Cursor:= crDefault;

    //find drag-drop target: is it Self or other ATTabs?
    if not PtInControl(Self, Pnt) then
    if FTabDragOutEnabled then
    begin
      Ctl:= _FindControl(Pnt);
      if Ctl<>nil then
        DoTabDropToOtherControl(Ctl, Ctl.ScreenToClient(Pnt));
    end;

    Invalidate;
    Exit
  end;

  //update mouse cursor: indicate drop allowed
  Screen.Cursor:= crNoDrop;
  if PtInControl(Self, Pnt) then
    Screen.Cursor:= FTabDragCursor
  else
  if FTabDragOutEnabled then
  begin
    Ctl:= _FindControl(Pnt);
    if Ctl<>nil then
    begin
      if (Ctl is TATTabs) and (Ctl as TATTabs).TabDragEnabled then
        Screen.Cursor:= FTabDragCursor
      else
      if Assigned(TControl2(Ctl).OnDragDrop) then
        Screen.Cursor:= FTabDragCursor;
    end;
  end;
end;

end.

