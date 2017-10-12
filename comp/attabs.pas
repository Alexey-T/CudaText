{
ATTabs component for Delphi/Lazarus
Copyright (c) Alexey Torgashin (UVviewsoft.com)
License: MPL 2.0 or LGPL
}

unit ATTabs;

{$ifdef FPC}
  {$mode delphi}
{$else}
  {$define windows}
  {$ifdef VER150} //Delphi 7
    {$define WIDE}
    //{$define TNT} //Tnt controls
  {$endif}
{$endif}

interface

uses
  {$ifdef windows}
  Windows,
  {$endif}
  Classes, Types, Graphics,
  Controls, Messages,
  {$ifdef FPC}
  InterfaceBase,
  LCLIntf,
  LCLType,
  LCLProc,
  {$endif}
  {$ifdef TNT}
  TntMenus,
  {$endif}
  Menus;

type
  TATTabString = {$ifdef WIDE} WideString {$else} string {$endif};
  TATTabPopupMenu = {$ifdef TNT} TTntPopupMenu {$else} TPopupMenu {$endif};
  TATTabMenuItem = {$ifdef TNT} TTntMenuItem {$else} TMenuItem {$endif};

type
  { TATTabData }

  TATTabData = class
  public
    TabCaption: TATTabString;
    TabObject: TObject;
    TabColor: TColor;
    TabModified: boolean;
    TabRect: TRect;
    TabImageIndex: integer;
    TabPopupMenu: TPopupMenu;
    constructor Create; virtual;
  end;

type
  TATTabElemType = (
    aeBackground,
    aeTabActive,
    aeTabPassive,
    aeTabPassiveOver,
    aeTabPlus,
    aeTabPlusOver,
    aeTabIconX,
    aeTabIconXOver,
    aeArrowDropdown,
    aeArrowDropdownOver,
    aeArrowScrollLeft,
    aeArrowScrollLeftOver,
    aeArrowScrollRight,
    aeArrowScrollRightOver,
    aeButtonPlus,
    aeButtonPlusOver,
    aeButtonClose,
    aeButtonCloseOver,
    aeButtonUser,
    aeButtonUserOver
    );

type
  TATTabButton = (
    atbNone,
    atbPlus,
    atbClose,
    atbScrollLeft,
    atbScrollRight,
    atbDropdownMenu,
    atbUser0,
    atbUser1,
    atbUser2,
    atbUser3,
    atbUser4
    );

  TATTabButtons = array[0..20] of TATTabButton;

type
  TATTabPosition = (
    atpTop,
    atpBottom,
    atpLeft,
    atpRight
    );

type
  TATTabOverEvent = procedure (Sender: TObject; ATabIndex: integer) of object;
  TATTabCloseEvent = procedure (Sender: TObject; ATabIndex: integer;
    var ACanClose, ACanContinue: boolean) of object;
  TATTabMenuEvent = procedure (Sender: TObject; var ACanShow: boolean) of object;
  TATTabDrawEvent = procedure (Sender: TObject;
    AElemType: TATTabElemType; ATabIndex: integer;
    ACanvas: TCanvas; const ARect: TRect; var ACanDraw: boolean) of object;
  TATTabMoveEvent = procedure (Sender: TObject; NFrom, NTo: integer) of object;  
  TATTabChangeQueryEvent = procedure (Sender: TObject; ANewTabIndex: integer;
    var ACanChange: boolean) of object;
  TATTabClickUserButton = procedure (Sender: TObject; AIndex: integer) of object;

type
  TATTabTriangle = (
    atriDown,
    atriLeft,
    atriRight
    );

  TATTabShowClose = (
    atbxShowNone,
    atbxShowAll,
    atbxShowActive,
    atbxShowMouseOver
    );

//int constants for GetTabAt
const
  TabIndexNone = -1; //none tab
  TabIndexPlus = -2;
  TabIndexPlusBtn = -3;
  TabIndexCloseBtn = -4;
  TabIndexArrowMenu = -5;
  TabIndexArrowScrollLeft = -6;
  TabIndexArrowScrollRight = -7;
  TabIndexUser0 = -10;
  TabIndexUser1 = -11;
  TabIndexUser2 = -12;
  TabIndexUser3 = -13;
  TabIndexUser4 = -14;

const
  _InitTabColorBg = $585858;
  _InitTabColorTabActive = $808080;
  _InitTabColorTabPassive = $786868;
  _InitTabColorTabOver = $A08080;
  _InitTabColorActiveMark = $C04040;
  _InitTabColorFontModified = $A00000;
  _InitTabColorBorderActive = $A0A0A0;
  _InitTabColorBorderPassive = $A07070;
  _InitTabColorCloseBg = clNone;
  _InitTabColorCloseBgOver = $6060E0;
  _InitTabColorCloseBorderOver = _InitTabColorCloseBgOver;
  _InitTabColorCloseX = clLtGray;
  _InitTabColorCloseXOver = clWhite;
  _InitTabColorArrow = $999999;
  _InitTabColorArrowOver = $E0E0E0;
  _InitTabColorDropMark = $6060E0;
  _InitTabColorScrollMark = _InitTabColorDropMark;

const
  _InitOptButtonLayout = '<>,v';
  _InitOptButtonSize = 16;
  _InitOptTabAngle = 4;
  _InitOptUseAngleForMaxTabs = 10;
  _InitOptTabHeight = 24;
  _InitOptTabWidthMinimal = 40;
  _InitOptTabWidthNormal = 130;
  _InitOptTabWidthMinimalHidesX = 55;
  _InitOptSpaceInitial = 5;
  _InitOptSpaceBeforeText = 6;
  _InitOptSpaceBetweenTabs = 0;
  _InitOptSpacer = 4;
  _InitOptSpacer2 = 15;
  _InitOptSpaceXRight = 10;
  _InitOptSpaceXInner = 3;
  _InitOptSpaceXSize = 12;
  _InitOptArrowSize = 4;
  _InitOptArrowSpaceLeft = 4;
  _InitOptColoredBandSize = 4;
  _InitOptActiveMarkSize = 4;
  _InitOptScrollMarkSizeX = 20;
  _InitOptScrollMarkSizeY = 3;
  _InitOptDropMarkSize = 6;

  _InitOptShowFlat = false;
  _InitOptPosition = atpTop;
  _InitOptShowNumberPrefix = '';
  _InitOptShowScrollMark = true;
  _InitOptShowDropMark = true;
  _InitOptShowArrowsNear = true;
  _InitOptShowXButtons = atbxShowAll;
  _InitOptShowPlusTab = true;
  _InitOptShowModifiedText = '*';
  _InitOptShowBorderActiveLow = false;
  _InitOptShowEntireColor = false;
  _InitOptMouseMiddleClickClose = true;
  _InitOptMouseDoubleClickClose = true;
  _InitOptMouseDoubleClickPlus = false;
  _InitOptMouseDragEnabled = true;
  _InitOptMouseDragOutEnabled = true;

type
  { TATTabs }

  TATTabs = class(TCustomControl)
  private
    //drag-drop
    FMouseDown: boolean;
    FMouseDownPnt: TPoint;
    FMouseDownDbl: boolean;
    FMouseDownButton: TMouseButton;
    FMouseDownShift: TShiftState;
    FMouseDownRightBtn: boolean;

    //colors
    FColorBg: TColor; //color of background (visible at top and between tabs)
    FColorBorderActive: TColor; //color of 1px border of active tab
    FColorBorderPassive: TColor; //color of 1px border of inactive tabs
    FColorTabActive: TColor; //color of active tab
    FColorTabPassive: TColor; //color of inactive tabs
    FColorTabOver: TColor; //color of inactive tabs, mouse-over
    FColorActiveMark: TColor;
    FColorFontModified: TColor;
    FColorCloseBg: TColor; //color of small square with "x" mark, inactive
    FColorCloseBgOver: TColor; //color of small square with "x" mark, mouse-over
    FColorCloseBorderOver: TColor; //color of 1px border of "x" mark, mouse-over
    FColorCloseX: TColor; //color of "x" mark
    FColorCloseXOver: TColor; //"color of "x" mark, mouseover
    FColorArrow: TColor; //color of "down" arrow (tab menu), inactive
    FColorArrowOver: TColor; //color of "down" arrow, mouse-over
    FColorDropMark: TColor;
    FColorScrollMark: TColor;

    //opts
    FButtonsLeft: TATTabButtons;
    FButtonsRight: TATTabButtons;
    FOptButtonSize: integer;
    FOptButtonLayout: string;

    FOptTabAngle: integer; //angle of tab border: from 0 (vertcal border) to any size
    FOptUseAngleForMaxTabs: integer; //maximal tab count, for which TabAngle is used (else used 0)
    FOptTabHeight: integer;
    FOptTabWidthMinimal: integer; //tab minimal width (used when lot of tabs)
    FOptTabWidthNormal: integer; //tab maximal width (used when only few tabs)
    FOptTabWidthMinimalHidesX: integer; //tab minimal width, after which "x" mark hides for inactive tabs
    FOptSpaceBetweenTabs: integer; //space between nearest tabs (no need for angled tabs)
    FOptSpaceInitial: integer; //space between first tab and left control edge
    FOptSpaceBeforeText: integer; //space between text and tab left edge
    FOptSpacer: integer; //height of top empty space (colored with bg)
    FOptSpacer2: integer;
    FOptSpaceXRight: integer; //space from "x" btn to right tab edge
    FOptSpaceXInner: integer; //space from "x" square edge to "x" mark
    FOptSpaceXSize: integer; //size of "x" mark
    FOptColoredBandSize: integer; //height of "misc color" line
    FOptColoredBandForTop: TATTabPosition;
    FOptColoredBandForBottom: TATTabPosition;
    FOptColoredBandForLeft: TATTabPosition;
    FOptColoredBandForRight: TATTabPosition;
    FOptActiveMarkSize: integer;
    FOptArrowSize: integer; //half-size of "arrow" mark
    FOptDropMarkSize: integer;
    FOptScrollMarkSizeX: integer;
    FOptScrollMarkSizeY: integer;

    FOptShowFlat: boolean;
    FOptPosition: TATTabPosition;
    FOptShowXButtons: TATTabShowClose; //show mode for "x" buttons
    FOptShowArrowsNear: boolean;
    FOptShowPlusTab: boolean; //show "plus" tab
    FOptShowModifiedText: TATTabString;
    FOptShowBorderActiveLow: boolean; //show border line below active tab (like Firefox)
    FOptShowEntireColor: boolean;
    FOptShowNumberPrefix: TATTabString;
    FOptShowScrollMark: boolean;
    FOptShowDropMark: boolean;

    FOptMouseMiddleClickClose: boolean; //enable close tab by middle-click
    FOptMouseDoubleClickClose: boolean;
    FOptMouseDoubleClickPlus: boolean; //enable call "+" tab with dbl-click on empty area
    FOptMouseDragEnabled: boolean; //enable drag-drop
    FOptMouseDragOutEnabled: boolean; //also enable drag-drop to another controls

    //others
    FTabWidth: integer;
    FTabIndex: integer;
    FTabIndexLoaded: integer;
    FTabIndexOver: integer;
    FTabIndexDrop: integer;
    FTabList: TList;
    FTabCaptions: TStrings;
    FTabMenu: TATTabPopupMenu;

    FRealIndentLeft: integer;
    FRealIndentRight: integer;

    FScrollPos: integer;
    FImages: TImageList;
    FBitmap: TBitmap;

    FRectArrowDown: TRect;
    FRectArrowLeft: TRect;
    FRectArrowRight: TRect;
    FRectButtonPlus: TRect;
    FRectButtonClose: TRect;
    FRectButtonUser0: TRect;
    FRectButtonUser1: TRect;
    FRectButtonUser2: TRect;
    FRectButtonUser3: TRect;
    FRectButtonUser4: TRect;

    //events    
    FOnTabClick: TNotifyEvent;
    FOnTabPlusClick: TNotifyEvent;
    FOnTabClickUserButton: TATTabClickUserButton;
    FOnTabClose: TATTabCloseEvent;
    FOnTabMenu: TATTabMenuEvent;
    FOnTabDrawBefore: TATTabDrawEvent;
    FOnTabDrawAfter: TATTabDrawEvent;
    FOnTabEmpty: TNotifyEvent;
    FOnTabOver: TATTabOverEvent;
    FOnTabMove: TATTabMoveEvent;
    FOnTabChangeQuery: TATTabChangeQueryEvent;

    procedure ApplyButtonLayout;
    procedure DoClickUser(AIndex: integer);
    procedure DoHandleClick;
    procedure DoHandleRightClick;
    procedure DoPaintArrowDown(C: TCanvas);
    procedure DoPaintArrowLeft(C: TCanvas);
    procedure DoPaintArrowRight(C: TCanvas);
    procedure DoPaintButtonClose(C: TCanvas);
    procedure DoPaintButtonPlus(C: TCanvas);
    procedure DoPaintColoredBand(C: TCanvas; PL1, PL2, PR1, PR2: TPoint;
      AColor: TColor);
    procedure DoPaintTo(C: TCanvas);
    procedure DoPaintBgTo(C: TCanvas; const ARect: TRect);
    procedure DoPaintTabTo(C: TCanvas; ARect: TRect; const ACaption: TATTabString;
      ATabBg, ATabBorder, ATabBorderLow, ATabHilite, ATabCloseBg,
      ATabCloseBorder, ATabCloseXMark: TColor; ACloseBtn, AModified: boolean;
      AImageIndex: integer);
    procedure DoPaintArrowTo(C: TCanvas; ATyp: TATTabTriangle; ARect: TRect;
      AColorArr, AColorBg: TColor);
    procedure DoPaintUserButtons(C: TCanvas);
    procedure DoPaintXTo(C: TCanvas; const R: TRect; ATabBg, ATabCloseBg,
      ATabCloseBorder, ATabCloseXMark: TColor);
    procedure DoPaintDropMark(C: TCanvas);
    procedure DoPaintScrollMark(C: TCanvas);
    procedure DoScrollAnimation(APosTo: integer);
    function GetIndexOfButton(AData: TATTabButtons; ABtn: TATTabButton): integer;
    function GetInitialVerticalIndent: integer;
    function GetTabs: TStrings;
    function IsScrollMarkNeeded: boolean;
    procedure UpdateTabCaptions;
    function GetMaxScrollPos: integer;
    function GetRectOfButton(AButton: TATTabButton): TRect;
    function GetRectOfButtonIndex(AIndex: integer; AtLeft: boolean): TRect;
    function GetScrollPageSize: integer;
    function RealTabAngle: integer;
    procedure SetTabs(AValue: TStrings);
    procedure SetOptButtonLayout(const AValue: string);
    procedure SetTabIndex(AIndex: integer);
    procedure GetTabCloseColor(AIndex: integer; const ARect: TRect; var AColorXBg,
      AColorXBorder, AColorXMark: TColor);
    function IsIndexOk(AIndex: integer): boolean;
    function IsShowX(AIndex: integer): boolean;
    function IsPaintNeeded(AElemType: TATTabElemType;
      AIndex: integer; ACanvas: TCanvas; const ARect: TRect): boolean;
    function DoPaintAfter(AElemType: TATTabElemType;
      AIndex: integer; ACanvas: TCanvas; const ARect: TRect): boolean;
    procedure TabMenuClick(Sender: TObject);
    function GetTabWidth_Plus_Raw: integer;
    procedure DoUpdateTabWidths;
    procedure DoTabDrop;
    procedure DoTabDropToOtherControl(ATarget: TControl; const APnt: TPoint);
    procedure DoUpdateTabRects;

  public
    constructor Create(AOnwer: TComponent); override;
    function CanFocus: boolean; override;
    destructor Destroy; override;
    procedure DragDrop(Source: TObject; X, Y: integer); override;

    function GetTabRectWidth(APlusBtn: boolean): integer;
    function GetTabRect(AIndex: integer): TRect;
    function GetTabRect_Plus: TRect;
    function GetTabRect_X(const ARect: TRect): TRect;
    function GetTabAt(X, Y: integer): integer;
    function GetTabData(AIndex: integer): TATTabData;
    function TabCount: integer;
    procedure AddTab(
      AIndex: integer;
      const ACaption: TATTabString;
      AObject: TObject = nil;
      AModified: boolean = false;
      AColor: TColor = clNone;
      AImageIndex: integer = -1;
      APopupMenu: TPopupMenu = nil);
    procedure Clear;
    function DeleteTab(AIndex: integer; AAllowEvent, AWithCancelBtn: boolean): boolean;
    procedure ShowTabMenu;
    procedure SwitchTab(ANext: boolean);
    procedure MoveTab(AFrom, ATo: integer; AActivateThen: boolean);
    procedure DoScrollLeft;
    procedure DoScrollRight;

  protected
    procedure Paint; override;
    procedure Resize; override;
    procedure DblClick; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: integer); override;
    procedure CMMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
    {$ifdef windows}
    procedure WMEraseBkgnd(var Message: TMessage); message WM_ERASEBKGND;
    {$endif}
    procedure DragOver(Source: TObject; X, Y: integer; State: TDragState; var Accept: Boolean); override;
    procedure Loaded; override;

  published
    //inherited
    property Align;
    property Anchors;
    {$ifdef fpc}
    property BorderSpacing;
    {$endif}
    property ClientHeight;
    property ClientWidth;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Visible;

    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnContextPopup;
    {$ifdef fpc}
    property OnMouseEnter;
    property OnMouseLeave;
    {$endif}
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
    property OnStartDrag;

    //new
    property DoubleBuffered;
    property Images: TImageList read FImages write FImages;
    property Tabs: TStrings read GetTabs write SetTabs;
    property TabIndex: integer read FTabIndex write SetTabIndex default 0;

    //colors
    property ColorBg: TColor read FColorBg write FColorBg default _InitTabColorBg;
    property ColorBorderActive: TColor read FColorBorderActive write FColorBorderActive default _InitTabColorBorderActive;
    property ColorBorderPassive: TColor read FColorBorderPassive write FColorBorderPassive default _InitTabColorBorderPassive;
    property ColorTabActive: TColor read FColorTabActive write FColorTabActive default _InitTabColorTabActive;
    property ColorTabPassive: TColor read FColorTabPassive write FColorTabPassive default _InitTabColorTabPassive;
    property ColorTabOver: TColor read FColorTabOver write FColorTabOver default _InitTabColorTabOver;
    property ColorActiveMark: TColor read FColorActiveMark write FColorActiveMark default _InitTabColorActiveMark;
    property ColorFontModified: TColor read FColorFontModified write FColorFontModified default _InitTabColorFontModified;
    property ColorCloseBg: TColor read FColorCloseBg write FColorCloseBg default _InitTabColorCloseBg;
    property ColorCloseBgOver: TColor read FColorCloseBgOver write FColorCloseBgOver default _InitTabColorCloseBgOver;
    property ColorCloseBorderOver: TColor read FColorCloseBorderOver write FColorCloseBorderOver default _InitTabColorCloseBorderOver;
    property ColorCloseX: TColor read FColorCloseX write FColorCloseX default _InitTabColorCloseX;
    property ColorCloseXOver: TColor read FColorCloseXOver write FColorCloseXOver default _InitTabColorCloseXOver;
    property ColorArrow: TColor read FColorArrow write FColorArrow default _InitTabColorArrow;
    property ColorArrowOver: TColor read FColorArrowOver write FColorArrowOver default _InitTabColorArrowOver;
    property ColorDropMark: TColor read FColorDropMark write FColorDropMark default _InitTabColorDropMark;
    property ColorScrollMark: TColor read FColorScrollMark write FColorScrollMark default _InitTabColorScrollMark;

    //options
    property OptButtonLayout: string read FOptButtonLayout write SetOptButtonLayout;
    property OptButtonSize: integer read FOptButtonSize write FOptButtonSize default _InitOptButtonSize;
    property OptTabHeight: integer read FOptTabHeight write FOptTabHeight default _InitOptTabHeight;
    property OptTabWidthNormal: integer read FOptTabWidthNormal write FOptTabWidthNormal default _InitOptTabWidthNormal;
    property OptTabWidthMinimal: integer read FOptTabWidthMinimal write FOptTabWidthMinimal default _InitOptTabWidthMinimal;
    property OptTabWidthMinimalHidesX: integer read FOptTabWidthMinimalHidesX write FOptTabWidthMinimalHidesX default _InitOptTabWidthMinimalHidesX;
    property OptTabAngle: integer read FOptTabAngle write FOptTabAngle default _InitOptTabAngle;
    property OptUseAngleForMaxTabs: integer read FOptUseAngleForMaxTabs write FOptUseAngleForMaxTabs default _InitOptUseAngleForMaxTabs;
    property OptSpaceBetweenTabs: integer read FOptSpaceBetweenTabs write FOptSpaceBetweenTabs default _InitOptSpaceBetweenTabs;
    property OptSpaceInitial: integer read FOptSpaceInitial write FOptSpaceInitial default _InitOptSpaceInitial;
    property OptSpaceBeforeText: integer read FOptSpaceBeforeText write FOptSpaceBeforeText default _InitOptSpaceBeforeText;
    property OptSpacer: integer read FOptSpacer write FOptSpacer default _InitOptSpacer;
    property OptSpacer2: integer read FOptSpacer2 write FOptSpacer2 default _InitOptSpacer2;
    property OptSpaceXRight: integer read FOptSpaceXRight write FOptSpaceXRight default _InitOptSpaceXRight;
    property OptSpaceXInner: integer read FOptSpaceXInner write FOptSpaceXInner default _InitOptSpaceXInner;
    property OptSpaceXSize: integer read FOptSpaceXSize write FOptSpaceXSize default _InitOptSpaceXSize;
    property OptColoredBandSize: integer read FOptColoredBandSize write FOptColoredBandSize default _InitOptColoredBandSize;
    property OptColoredBandForTop: TATTabPosition read FOptColoredBandForTop write FOptColoredBandForTop default atpTop;
    property OptColoredBandForBottom: TATTabPosition read FOptColoredBandForBottom write FOptColoredBandForBottom default atpBottom;
    property OptColoredBandForLeft: TATTabPosition read FOptColoredBandForLeft write FOptColoredBandForLeft default atpLeft;
    property OptColoredBandForRight: TATTabPosition read FOptColoredBandForRight write FOptColoredBandForRight default atpRight;
    property OptActiveMarkSize: integer read FOptActiveMarkSize write FOptActiveMarkSize default _InitOptActiveMarkSize;
    property OptArrowSize: integer read FOptArrowSize write FOptArrowSize default _InitOptArrowSize;
    property OptScrollMarkSizeX: integer read FOptScrollMarkSizeX write FOptScrollMarkSizeX default _InitOptScrollMarkSizeX;
    property OptScrollMarkSizeY: integer read FOptScrollMarkSizeY write FOptScrollMarkSizeY default _InitOptScrollMarkSizeY;
    property OptDropMarkSize: integer read FOptDropMarkSize write FOptDropMarkSize default _InitOptDropMarkSize;

    property OptShowFlat: boolean read FOptShowFlat write FOptShowFlat default _InitOptShowFlat;
    property OptPosition: TATTabPosition read FOptPosition write FOptPosition default _InitOptPosition;
    property OptShowScrollMark: boolean read FOptShowScrollMark write FOptShowScrollMark default _InitOptShowScrollMark;
    property OptShowDropMark: boolean read FOptShowDropMark write FOptShowDropMark default _InitOptShowDropMark;
    property OptShowXButtons: TATTabShowClose read FOptShowXButtons write FOptShowXButtons default _InitOptShowXButtons;
    property OptShowPlusTab: boolean read FOptShowPlusTab write FOptShowPlusTab default _InitOptShowPlusTab;
    property OptShowArrowsNear: boolean read FOptShowArrowsNear write FOptShowArrowsNear default _InitOptShowArrowsNear;
    property OptShowModifiedText: TATTabString read FOptShowModifiedText write FOptShowModifiedText;
    property OptShowBorderActiveLow: boolean read FOptShowBorderActiveLow write FOptShowBorderActiveLow default _InitOptShowBorderActiveLow;
    property OptShowEntireColor: boolean read FOptShowEntireColor write FOptShowEntireColor default _InitOptShowEntireColor;
    property OptShowNumberPrefix: TATTabString read FOptShowNumberPrefix write FOptShowNumberPrefix;
    property OptMouseMiddleClickClose: boolean read FOptMouseMiddleClickClose write FOptMouseMiddleClickClose default _InitOptMouseMiddleClickClose;
    property OptMouseDoubleClickClose: boolean read FOptMouseDoubleClickClose write FOptMouseDoubleClickClose default _InitOptMouseDoubleClickClose;
    property OptMouseDoubleClickPlus: boolean read FOptMouseDoubleClickPlus write FOptMouseDoubleClickPlus default _InitOptMouseDoubleClickPlus;
    property OptMouseDragEnabled: boolean read FOptMouseDragEnabled write FOptMouseDragEnabled default _InitOptMouseDragEnabled;
    property OptMouseDragOutEnabled: boolean read FOptMouseDragOutEnabled write FOptMouseDragOutEnabled default _InitOptMouseDragOutEnabled;

    //events
    property OnTabClick: TNotifyEvent read FOnTabClick write FOnTabClick;
    property OnTabPlusClick: TNotifyEvent read FOnTabPlusClick write FOnTabPlusClick;
    property OnTabClickUserButton: TATTabClickUserButton read FOnTabClickUserButton write FOnTabClickUserButton;
    property OnTabClose: TATTabCloseEvent read FOnTabClose write FOnTabClose;
    property OnTabMenu: TATTabMenuEvent read FOnTabMenu write FOnTabMenu;
    property OnTabDrawBefore: TATTabDrawEvent read FOnTabDrawBefore write FOnTabDrawBefore;
    property OnTabDrawAfter: TATTabDrawEvent read FOnTabDrawAfter write FOnTabDrawAfter;
    property OnTabEmpty: TNotifyEvent read FOnTabEmpty write FOnTabEmpty;
    property OnTabOver: TATTabOverEvent read FOnTabOver write FOnTabOver;
    property OnTabMove: TATTabMoveEvent read FOnTabMove write FOnTabMove;
    property OnTabChangeQuery: TATTabChangeQueryEvent read FOnTabChangeQuery write FOnTabChangeQuery;
  end;

var
  cTabsMouseMinDistanceToDrag: integer = 10; //mouse must move >=N pixels to start drag-drop
  cTabsMouseMaxDistanceToClick: integer = 4; //if mouse moves during mouse-down >=N pixels, dont click


implementation

uses
  SysUtils,
  StrUtils,
  Dialogs,
  Forms,
  Math;

function IsDoubleBufferedNeeded: boolean;
begin
  {$ifdef FPC}
  Result:= WidgetSet.GetLCLCapability(lcCanDrawOutsideOnPaint) = LCL_CAPABILITY_YES;
  {$else}
  Result:= true;
  {$endif}
end;

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

procedure DrawAntialisedLine(Canvas: TCanvas; const AX1, AY1, AX2, AY2: {real}integer; const LineColor: TColor);
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
    if resclr<0 then exit; //prevent except in GetRValue
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
  if AX1<0 then exit;
  if AX2<0 then exit;

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

procedure DrawTriangleType(C: TCanvas; Typ: TATTabTriangle; const R: TRect; Color: TColor);
var
  P1, P2, P3: TPoint;
begin
  //P1/P2: points of vert/horz line
  //P3: end point at arrow direction
  case Typ of
    atriDown:
    begin
      P1:= Point(R.Left, R.Top);
      P2:= Point(R.Right, R.Top);
      P3:= Point((R.Left+R.Right) div 2, R.Bottom);
    end;
    atriRight:
    begin
      P1:= Point(R.Left, R.Top);
      P2:= Point(R.Left, R.Bottom);
      P3:= Point(R.Right, (R.Top+R.Bottom) div 2);
    end;
    atriLeft:
    begin
      P1:= Point(R.Right, R.Top);
      P2:= Point(R.Right, R.Bottom);
      P3:= Point(R.Left, (R.Top+R.Bottom) div 2);
    end;
  end;

  DrawTriangleRaw(C, P1, P2, P3, Color);
end;


procedure DrawPlusSign(C: TCanvas; const R: TRect; ASize: integer; AColor: TColor);
var
  CX, CY: integer;
begin
  C.Pen.Color:= AColor;
  CX:= (R.Left+R.Right) div 2;
  CY:= (R.Top+R.Bottom) div 2;

  C.MoveTo(CX - ASize, CY);
  C.LineTo(CX + ASize+1, CY);

  C.MoveTo(CX, CY - ASize);
  C.LineTo(CX, CY + ASize+1);
end;


procedure DrawCrossSign(C: TCanvas; const R: TRect; ASize: integer; AColor: TColor);
var
  CX, CY: integer;
begin
  C.Pen.Color:= AColor;
  CX:= (R.Left+R.Right) div 2;
  CY:= (R.Top+R.Bottom) div 2;

  C.MoveTo(CX - ASize+1, CY - ASize+1);
  C.LineTo(CX + ASize+1, CY + ASize+1);

  C.MoveTo(CX + ASize, CY - ASize+1);
  C.LineTo(CX - ASize, CY + ASize+1);
end;


{ TATTabData }

constructor TATTabData.Create;
begin
  inherited;
  TabColor:= clNone;
  TabImageIndex:= -1;
  TabPopupMenu:= nil;
end;

{ TATTabs }

function TATTabs.IsIndexOk(AIndex: integer): boolean;
begin
  Result:= (AIndex>=0) and (AIndex<FTabList.Count);
end;

function TATTabs.TabCount: integer;
begin
  Result:= FTabList.Count;
end;

constructor TATTabs.Create(AOnwer: TComponent);
begin
  inherited;

  Caption:= '';
  ControlStyle:= ControlStyle+[csOpaque];
  DoubleBuffered:= IsDoubleBufferedNeeded;

  Width:= 400;
  Height:= 35;

  FMouseDown:= false;
  FMouseDownPnt:= Point(0, 0);
  FMouseDownDbl:= false;
  FMouseDownRightBtn:=false;

  FColorBg:= _InitTabColorBg;
  FColorTabActive:= _InitTabColorTabActive;
  FColorTabPassive:= _InitTabColorTabPassive;
  FColorTabOver:= _InitTabColorTabOver;
  FColorActiveMark:= _InitTabColorActiveMark;
  FColorFontModified:= _InitTabColorFontModified;
  FColorBorderActive:= _InitTabColorBorderActive;
  FColorBorderPassive:= _InitTabColorBorderPassive;
  FColorCloseBg:= _InitTabColorCloseBg;
  FColorCloseBgOver:= _InitTabColorCloseBgOver;
  FColorCloseBorderOver:= _InitTabColorCloseBorderOver;
  FColorCloseX:= _InitTabColorCloseX;
  FColorCloseXOver:= _InitTabColorCloseXOver;
  FColorArrow:= _InitTabColorArrow;
  FColorArrowOver:= _InitTabColorArrowOver;
  FColorDropMark:= _InitTabColorDropMark;
  FColorScrollMark:= _InitTabColorScrollMark;

  FOptButtonLayout:= _InitOptButtonLayout;
  ApplyButtonLayout;
  FOptButtonSize:= _InitOptButtonSize;
  FOptTabAngle:= _InitOptTabAngle;
  FOptUseAngleForMaxTabs:= _InitOptUseAngleForMaxTabs;
  FOptTabHeight:= _InitOptTabHeight;
  FOptTabWidthMinimal:= _InitOptTabWidthMinimal;
  FOptTabWidthNormal:= _InitOptTabWidthNormal;
  FOptTabWidthMinimalHidesX:= _InitOptTabWidthMinimalHidesX;
  FOptSpaceInitial:= _InitOptSpaceInitial;
  FOptSpaceBeforeText:= _InitOptSpaceBeforeText;
  FOptSpaceBetweenTabs:= _InitOptSpaceBetweenTabs;
  FOptSpacer:= _InitOptSpacer;
  FOptSpacer2:= _InitOptSpacer2;
  FOptSpaceXRight:= _InitOptSpaceXRight;
  FOptSpaceXInner:= _InitOptSpaceXInner;
  FOptSpaceXSize:= _InitOptSpaceXSize;
  FOptArrowSize:= _InitOptArrowSize;
  FOptColoredBandSize:= _InitOptColoredBandSize;
  FOptColoredBandForTop:= atpTop;
  FOptColoredBandForBottom:= atpBottom;
  FOptColoredBandForLeft:= atpLeft;
  FOptColoredBandForRight:= atpRight;
  FOptActiveMarkSize:= _InitOptActiveMarkSize;
  FOptScrollMarkSizeX:= _InitOptScrollMarkSizeX;
  FOptScrollMarkSizeY:= _InitOptScrollMarkSizeY;
  FOptDropMarkSize:= _InitOptDropMarkSize;

  FOptShowFlat:= _InitOptShowFlat;
  FOptPosition:= _InitOptPosition;
  FOptShowNumberPrefix:= _InitOptShowNumberPrefix;
  FOptShowScrollMark:= _InitOptShowScrollMark;
  FOptShowDropMark:= _InitOptShowDropMark;
  FOptShowXButtons:= _InitOptShowXButtons;
  FOptShowPlusTab:= _InitOptShowPlusTab;
  FOptShowArrowsNear:= _InitOptShowArrowsNear;
  FOptShowModifiedText:= _InitOptShowModifiedText;
  FOptShowBorderActiveLow:= _InitOptShowBorderActiveLow;
  FOptShowEntireColor:= _InitOptShowEntireColor;
  FOptMouseMiddleClickClose:= _InitOptMouseMiddleClickClose;
  FOptMouseDoubleClickClose:= _InitOptMouseDoubleClickClose;
  FOptMouseDoubleClickPlus:= _InitOptMouseDoubleClickPlus;
  FOptMouseDragEnabled:= _InitOptMouseDragEnabled;
  FOptMouseDragOutEnabled:= _InitOptMouseDragOutEnabled;

  FBitmap:= TBitmap.Create;
  FBitmap.PixelFormat:= pf24bit;
  FBitmap.Width:= 1600;
  FBitmap.Height:= 60;

  FTabIndex:= 0;
  FTabIndexOver:= -1;
  FTabList:= TList.Create;
  FTabCaptions:= TStringList.Create;
  FTabMenu:= nil;
  FScrollPos:= 0;

  FOnTabClick:= nil;
  FOnTabPlusClick:= nil;
  FOnTabClose:= nil;
  FOnTabMenu:= nil;
  FOnTabDrawBefore:= nil;
  FOnTabDrawAfter:= nil;
  FOnTabChangeQuery:= nil;
end;

function TATTabs.CanFocus: boolean;
begin
  Result:= false;
end;

procedure TATTabs.Clear;
var
  i: integer;
begin
  for i:= TabCount-1 downto 0 do
  begin
    TObject(FTabList[i]).Free;
    FTabList[i]:= nil;
  end;
  FTabList.Clear;
  FTabIndex:= 0;
end;

destructor TATTabs.Destroy;
begin
  Clear;
  FreeAndNil(FTabCaptions);
  FreeAndNil(FTabList);
  FreeAndNil(FBitmap);
  inherited;
end;

procedure TATTabs.Paint;
begin
  if DoubleBuffered then
  begin
    if Assigned(FBitmap) then
    begin
      DoPaintTo(FBitmap.Canvas);
      Canvas.CopyRect(ClientRect, FBitmap.Canvas, ClientRect);
    end;
  end
  else
    DoPaintTo(Canvas);
end;

procedure TATTabs.DoPaintTabTo(
  C: TCanvas; ARect: TRect; const ACaption: TATTabString;
  ATabBg, ATabBorder, ATabBorderLow, ATabHilite, ATabCloseBg, ATabCloseBorder, ATabCloseXMark: TColor;
  ACloseBtn, AModified: boolean;
  AImageIndex: integer);
var
  PL1, PL2, PR1, PR2: TPoint;
  RectText: TRect;
  NIndentL, NIndentR, NIndentTop: integer;
  ElemType: TATTabElemType;
  AInvert, NAngle: integer;
  TempCaption: TATTabString;
  bActive, bNeedMoreSpace: boolean;
begin
  //optimize for 200 tabs
  if ARect.Left>=ClientWidth then exit;
  //skip tabs scrolled lefter
  if ARect.Right<=0 then exit;

  bActive:= ATabBg=ColorTabActive;
  if FOptShowFlat then
    ATabBg:= ColorBg;

  if FOptShowEntireColor and (ATabHilite<>clNone) then
    ATabBg:= ATabHilite;

  C.Pen.Color:= ATabBg;
  C.Brush.Color:= ATabBg;

  if FOptPosition=atpBottom then
    AInvert:= -1
  else
    AInvert:= 1;

  NAngle:= RealTabAngle;
  RectText:= Rect(ARect.Left+NAngle, ARect.Top, ARect.Right-NAngle, ARect.Bottom);
  bNeedMoreSpace:= (RectText.Right-RectText.Left<=30) and (ACaption<>'');
  NIndentL:= IfThen(not bNeedMoreSpace, NAngle+FOptSpaceBeforeText, 2);
  NIndentR:= NIndentL+IfThen(ACloseBtn, FOptSpaceXRight);
  C.FillRect(RectText);
  RectText:= Rect(ARect.Left+NIndentL, ARect.Top, ARect.Right-NIndentR, ARect.Bottom);

  //imagelist
  if Assigned(FImages) then
    if (AImageIndex>=0) and (AImageIndex<FImages.Count) then
    begin
      FImages.Draw(C,
        RectText.Left-2,
        (RectText.Top + RectText.Bottom - FImages.Height) div 2,
        AImageIndex);
      Inc(RectText.Left, FImages.Width);
    end;

  //left triangle
  PL1:= Point(ARect.Left+NAngle*AInvert, ARect.Top);
  PL2:= Point(ARect.Left-NAngle*AInvert, ARect.Bottom-1);
  if NAngle>0 then
  begin
    //DrawTriangleRaw(C, PL1, PL2, Point(PL1.X, PL2.Y), ATabBg);
    //draw little shifted line- bottom-left point x+=1
    if FOptPosition=atpBottom then
      DrawTriangleRaw(C, PL1, Point(PL2.X+1, PL2.Y), Point(PL2.X, PL1.Y), ATabBg)
    else
      DrawTriangleRaw(C, PL1, Point(PL2.X+1, PL2.Y), Point(PL1.X, PL2.Y), ATabBg);
  end;

  //right triangle
  PR1:= Point(ARect.Right-NAngle*AInvert-1, ARect.Top);
  PR2:= Point(ARect.Right+NAngle*AInvert-1, ARect.Bottom-1);
  if NAngle>0 then
  begin
    //DrawTriangleRaw(C, PR1, PR2, Point(PR1.X, PR2.Y), ATabBg);
    //draw little shifted line- bottom-right point x-=1
    if FOptPosition=atpBottom then
      DrawTriangleRaw(C, PR1, Point(PR2.X-1, PR2.Y), Point(PR2.X, PR1.Y), ATabBg)
    else
      DrawTriangleRaw(C, PR1, Point(PR2.X-1, PR2.Y), Point(PR1.X, PR2.Y), ATabBg);
  end;

  //caption
  if RectText.Right-RectText.Left>=8 then
  begin
    C.Font.Assign(Self.Font);
    if AModified then
      C.Font.Color:= FColorFontModified;

    TempCaption:= IfThen(AModified, FOptShowModifiedText) + ACaption;

    NIndentTop:= (FOptTabHeight - C.TextHeight('Wj')) div 2 + 1;

    {$ifdef WIDE}
    ExtTextOutW(C.Handle,
      RectText.Left,
      RectText.Top+NIndentTop,
      ETO_CLIPPED{+ETO_OPAQUE},
      @RectText,
      PWChar(TempCaption),
      Length(TempCaption),
      nil);
    {$else}
    ExtTextOut(C.Handle,
      RectText.Left,
      RectText.Top+NIndentTop,
      ETO_CLIPPED{+ETO_OPAQUE},
      @RectText,
      PChar(TempCaption),
      Length(TempCaption),
      nil);
    {$endif}
  end;

  //borders
  if FOptShowFlat then
  begin
    if bActive then
    begin
      C.Brush.Color:= ColorActiveMark;
      case FOptPosition of
        atpTop:
          C.FillRect(Rect(PL2.X, ClientHeight-FOptActiveMarkSize, PR2.X, ClientHeight));
        atpBottom:
          C.FillRect(Rect(PL1.X, 0, PR1.X, FOptActiveMarkSize));
        atpLeft:
          C.FillRect(Rect(ClientWidth-FOptActiveMarkSize, PR1.Y, ClientWidth, PR2.Y));
        atpRight:
          C.FillRect(Rect(0, PL1.Y, FOptActiveMarkSize, PL2.Y))
       end;
    end;
  end
  else
  case FOptPosition of
    atpTop:
      begin
        DrawAntialisedLine(C, PL1.X, PL1.Y, PL2.X, PL2.Y+1, ATabBorder);
        DrawAntialisedLine(C, PR1.X, PR1.Y, PR2.X, PR2.Y+1, ATabBorder);
        DrawAntialisedLine(C, PL1.X, PL1.Y, PR1.X, PL1.Y, ATabBorder);
        if ATabBorderLow<>clNone then
          DrawAntialisedLine(C, PL2.X, ARect.Bottom, PR2.X, ARect.Bottom, ATabBorderLow)
        else
          DrawAntialisedLine(C, PL2.X+1, ARect.Bottom, PR2.X-1, ARect.Bottom, ATabBg);
      end;
    atpBottom:
      begin
        DrawAntialisedLine(C, PL1.X, PL1.Y, PL2.X, PL2.Y+1, ATabBorder);
        DrawAntialisedLine(C, PR1.X, PR1.Y, PR2.X, PR2.Y+1, ATabBorder);
        DrawAntialisedLine(C, PL2.X, PL2.Y+1, PR2.X, PL2.Y+1, ATabBorder);
        if ATabBorderLow<>clNone then
          DrawAntialisedLine(C, PL1.X, ARect.Top, PR1.X, ARect.Top, ATabBorderLow)
      end;
    atpLeft:
      begin
        DrawAntialisedLine(C, PL1.X, PL1.Y, PR1.X, PR1.Y, ATabBorder);
        DrawAntialisedLine(C, PL2.X, PL2.Y, PR2.X, PR2.Y, ATabBorder);
        DrawAntialisedLine(C, PL1.X, PL1.Y, PL2.X, PL2.Y, ATabBorder);
        DrawAntialisedLine(C, PR1.X+1, PR1.Y+1, PR1.X+1, PR2.Y-1, IfThen(ATabBorderLow<>clNone, ATabBorderLow, ATabBg));
      end;
    atpRight:
      begin
        DrawAntialisedLine(C, PL1.X, PL1.Y, PR1.X, PR1.Y, ATabBorder);
        DrawAntialisedLine(C, PL2.X, PL2.Y, PR2.X, PR2.Y, ATabBorder);
        DrawAntialisedLine(C, PL1.X-1, PL1.Y+1, PL1.X-1, PL2.Y-1, IfThen(ATabBorderLow<>clNone, ATabBorderLow, ATabBg));
        DrawAntialisedLine(C, PR1.X, PR1.Y, PR2.X, PR2.Y, ATabBorder);
      end;
  end;

  //colored band
  if not FOptShowEntireColor then
    if ATabHilite<>clNone then
      DoPaintColoredBand(C, PL1, PL2, PR1, PR2, ATabHilite);

  //"close" button
  if ACloseBtn then
  begin
    if ATabCloseBg<>clNone then
      ElemType:= aeTabIconXOver
    else
      ElemType:= aeTabIconX;
    RectText:= GetTabRect_X(ARect);
    if IsPaintNeeded(ElemType, -1, C, RectText) then
    begin
      DoPaintXTo(C, RectText, ATabBg, ATabCloseBg, ATabCloseBorder, ATabCloseXMark);
      DoPaintAfter(ElemType, -1, C, RectText);
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

  PXX1:= Point(R.Left+FOptSpaceXInner, R.Top+FOptSpaceXInner);
  PXX2:= Point(R.Right-FOptSpaceXInner-1, R.Bottom-FOptSpaceXInner-1);
  PX1:= Point(PXX1.X+1, PXX1.Y);
  PX2:= Point(PXX1.X, PXX1.Y+1);
  PX3:= Point(PXX2.X-1, PXX2.Y);
  PX4:= Point(PXX2.X, PXX2.Y-1);
  C.Polygon([PX1, PXX1, PX2, PX3, PXX2, PX4]);

  PXX1:= Point(R.Right-FOptSpaceXInner-1, R.Top+FOptSpaceXInner);
  PXX2:= Point(R.Left+FOptSpaceXInner, R.Bottom-FOptSpaceXInner-1);
  PX1:= Point(PXX1.X-1, PXX1.Y);
  PX2:= Point(PXX1.X, PXX1.Y+1);
  PX3:= Point(PXX2.X+1, PXX2.Y);
  PX4:= Point(PXX2.X, PXX2.Y-1);
  C.Polygon([PX1, PXX1, PX2, PX3, PXX2, PX4]);

  C.Brush.Color:= ATabBg;
end;

function TATTabs.GetTabWidth_Plus_Raw: integer;
begin
  Result:= FOptArrowSize*4;
end;

function TATTabs.GetTabRectWidth(APlusBtn: boolean): integer;
begin
  case FOptPosition of
    atpLeft,
    atpRight:
      begin
        Result:= ClientWidth-FOptSpacer;
      end;
    else
      begin
        if APlusBtn then
          Result:= GetTabWidth_Plus_Raw
        else
          Result:= FOptTabWidthNormal;
        Inc(Result, 2*(RealTabAngle + FOptSpaceBeforeText));
      end;
  end;
end;


function TATTabs.GetTabRect(AIndex: integer): TRect;
var
  Data: TATTabData;
begin
  Data:= GetTabData(AIndex);
  if Assigned(Data) then
    Result:= Data.TabRect
  else
    Result:= Rect(0, 0, 10, 10);

  case FOptPosition of
    atpTop,
    atpBottom:
      begin
        Dec(Result.Left, FScrollPos);
        Dec(Result.Right, FScrollPos);
      end;
    else
      begin
        Dec(Result.Top, FScrollPos);
        Dec(Result.Bottom, FScrollPos);
      end;
  end;
end;

procedure TATTabs.DoUpdateTabRects;
var
  i: integer;
  Data: TATTabData;
  R: TRect;
begin
  if FOptPosition in [atpLeft, atpRight] then
  begin
    R.Left:= IfThen(FOptPosition=atpLeft, FOptSpacer, FOptSpacer2+1);
    R.Right:= IfThen(FOptPosition=atpLeft, ClientWidth-FOptSpacer2, ClientWidth-FOptSpacer);
    R.Bottom:= GetInitialVerticalIndent;
    R.Top:= R.Bottom;

    for i:= 0 to TabCount-1 do
    begin
      R.Top:= R.Bottom + FOptSpaceBetweenTabs;
      R.Bottom:= R.Top + FOptTabHeight;
      Data:= GetTabData(i);
      if Assigned(Data) then
        Data.TabRect:= R;
    end;

    exit;
  end;

  R.Left:= FRealIndentLeft+RealTabAngle;
  R.Right:= R.Left;
  R.Top:= FOptSpacer;
  R.Bottom:= R.Top+FOptTabHeight;

  for i:= 0 to TabCount-1 do
  begin
    R.Left:= R.Right + FOptSpaceBetweenTabs;
    R.Right:= R.Left + FTabWidth;
    Data:= GetTabData(i);
    if Assigned(Data) then
      Data.TabRect:= R;
  end;
end;

function TATTabs.GetTabRect_Plus: TRect;
begin
  case FOptPosition of
    atpTop,
    atpBottom:
      begin
        if TabCount>0 then
        begin
          Result:= GetTabRect(TabCount-1);
          Result.Left:= Result.Right + FOptSpaceBetweenTabs;
          Result.Right:= Result.Left + GetTabRectWidth(true);
        end
        else
        begin
          Result.Top:= FOptSpacer;
          Result.Bottom:= Result.Top + FOptTabHeight;
          Result.Left:= FRealIndentLeft + RealTabAngle;
          Result.Right:= Result.Left + GetTabRectWidth(true);
        end;
      end;
    else
      begin
        if TabCount>0 then
        begin
          Result:= GetTabRect(TabCount-1);
          Result.Top:= Result.Bottom + FOptSpaceBetweenTabs;
          Result.Bottom:= Result.Top + FOptTabHeight;
        end
        else
        begin
          Result.Left:= IfThen(FOptPosition=atpLeft, FOptSpacer, FOptSpacer2);
          Result.Right:= IfThen(FOptPosition=atpLeft, ClientWidth-FOptSpacer2, ClientWidth-FOptSpacer);
          Result.Top:= GetInitialVerticalIndent;
          Result.Bottom:= Result.Top + FOptTabHeight;
        end;
      end;
  end;
end;

function TATTabs.GetTabRect_X(const ARect: TRect): TRect;
var
  P: TPoint;
begin
  P:= Point(
    ARect.Right-RealTabAngle-FOptSpaceXRight,
    (ARect.Top+ARect.Bottom) div 2 + 1);
  Dec(P.X, FOptSpaceXSize div 2);
  Dec(P.Y, FOptSpaceXSize div 2);
  Result:= Rect(
    P.X,
    P.Y,
    P.X+FOptSpaceXSize,
    P.Y+FOptSpaceXSize);
end;

function _IsDrag: boolean;
begin
  Result:= Mouse.IsDragging;
end;

procedure TATTabs.GetTabCloseColor(AIndex: integer; const ARect: TRect;
  var AColorXBg, AColorXBorder, AColorXMark: TColor);
var
  P: TPoint;
begin
  AColorXBg:= FColorCloseBg;
  AColorXBorder:= FColorCloseBg;
  AColorXMark:= FColorCloseX;

  if _IsDrag then Exit;

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
  AIndex: integer; ACanvas: TCanvas; const ARect: TRect): boolean;
begin
  Result:= true;
  if Assigned(FOnTabDrawBefore) then
    FOnTabDrawBefore(Self, AElemType, AIndex, ACanvas, ARect, Result);
end;

function TATTabs.DoPaintAfter(AElemType: TATTabElemType;
  AIndex: integer; ACanvas: TCanvas; const ARect: TRect): boolean;
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
  RRect, RBottom: TRect;
  NColorXBg, NColorXBorder, NColorXMark: TColor;
  ElemType: TATTabElemType;
  Data: TATTabData;
  i: integer;
begin
  ElemType:= aeBackground;
  RRect:= ClientRect;

  FRealIndentLeft:= FOptSpaceInitial;
  FRealIndentRight:= 0;
  for i:= 0 to High(TATTabButtons) do
    if FButtonsLeft[i]<>atbNone then
      Inc(FRealIndentLeft, FOptButtonSize);
  for i:= 0 to High(TATTabButtons) do
    if FButtonsRight[i]<>atbNone then
      Inc(FRealIndentRight, FOptButtonSize);

  FRectArrowLeft:= GetRectOfButton(atbScrollLeft);
  FRectArrowRight:= GetRectOfButton(atbScrollRight);
  FRectArrowDown:= GetRectOfButton(atbDropdownMenu);
  FRectButtonPlus:= GetRectOfButton(atbPlus);
  FRectButtonClose:= GetRectOfButton(atbClose);
  FRectButtonUser0:= GetRectOfButton(atbUser0);
  FRectButtonUser1:= GetRectOfButton(atbUser1);
  FRectButtonUser2:= GetRectOfButton(atbUser2);
  FRectButtonUser3:= GetRectOfButton(atbUser3);
  FRectButtonUser4:= GetRectOfButton(atbUser4);

  //painting of BG is little different then other elements:
  //paint fillrect anyway, then maybe paint ownerdraw
  DoPaintBgTo(C, RRect);
  if IsPaintNeeded(ElemType, -1, C, RRect) then
  begin
    DoPaintAfter(ElemType, -1, C, RRect);
  end;

  DoUpdateTabWidths;
  DoUpdateTabRects;

  //paint spacer rect
  if not FOptShowFlat then
    case FOptPosition of
      atpTop:
        begin
          RBottom:= Rect(0, FOptSpacer+FOptTabHeight, ClientWidth, ClientHeight);
          C.Brush.Color:= FColorTabActive;
          C.FillRect(RBottom);
          DrawAntialisedLine(C, RBottom.Left, RBottom.Top, RBottom.Right, RBottom.Top, FColorBorderActive);
        end;
      atpBottom:
        begin
          RBottom:= Rect(0, 0, ClientWidth, FOptSpacer);
          C.Brush.Color:= FColorTabActive;
          C.FillRect(RBottom);
          DrawAntialisedLine(C, RBottom.Left, RBottom.Bottom, RBottom.Right, RBottom.Bottom, FColorBorderActive);
        end;
      atpLeft:
        begin
          RBottom:= Rect(ClientWidth-FOptSpacer2, 0, ClientWidth, ClientHeight);
          C.Brush.Color:= FColorTabActive;
          C.FillRect(RBottom);
          DrawAntialisedLine(C, RBottom.Left, RBottom.Top, RBottom.Left, RBottom.Bottom, FColorBorderActive);
        end;
      atpRight:
        begin
          RBottom:= Rect(0, 0, FOptSpacer2, ClientHeight);
          C.Brush.Color:= FColorTabActive;
          C.FillRect(RBottom);
          DrawAntialisedLine(C, RBottom.Right, RBottom.Top, RBottom.Right, RBottom.Bottom, FColorBorderActive);
        end;
    end;

  //paint "plus" tab
  if FOptShowPlusTab then
  begin
    RRect:= GetTabRect_Plus;
    NColorXBg:= clNone;
    NColorXBorder:= clNone;
    NColorXMark:= clWhite;
    if FTabIndexOver=TabIndexPlus then
      ElemType:= aeTabPlusOver
    else
      ElemType:= aeTabPlus;
    if IsPaintNeeded(ElemType, -1, C, RRect) then
    begin
      DoPaintTabTo(C, RRect,
        '',
        IfThen((FTabIndexOver=TabIndexPlus) and not _IsDrag, FColorTabOver, FColorTabPassive),
        FColorBorderPassive,
        FColorBorderActive,
        clNone,
        NColorXBg,
        NColorXBorder,
        NColorXMark,
        false,
        false,
        -1 //no icon
        );
      DrawPlusSign(C, RRect, FOptArrowSize, Font.Color);
      DoPaintAfter(ElemType, -1, C, RRect);
    end;    
  end;

  //paint passive tabs
  for i:= TabCount-1 downto 0 do
    if i<>FTabIndex then
    begin
      RRect:= GetTabRect(i);
      GetTabCloseColor(i, RRect, NColorXBg, NColorXBorder, NColorXMark);
      if i=FTabIndexOver then
        ElemType:= aeTabPassiveOver
      else
        ElemType:= aeTabPassive;
      if IsPaintNeeded(ElemType, i, C, RRect) then
      begin
        Data:= TATTabData(FTabList[i]);
        DoPaintTabTo(C, RRect,
          Format(FOptShowNumberPrefix, [i+1]) + Data.TabCaption,
          IfThen((i=FTabIndexOver) and not _IsDrag, FColorTabOver, FColorTabPassive),
          FColorBorderPassive,
          FColorBorderActive,
          Data.TabColor,
          NColorXBg,
          NColorXBorder,
          NColorXMark,
          IsShowX(i),
          Data.TabModified,
          Data.TabImageIndex
          );
        DoPaintAfter(ElemType, i, C, RRect);
      end;
    end;

  //paint active tab
  i:= FTabIndex;
  if IsIndexOk(i) then
  begin
    RRect:= GetTabRect(i);
    GetTabCloseColor(i, RRect, NColorXBg, NColorXBorder, NColorXMark);
    if IsPaintNeeded(aeTabActive, i, C, RRect) then
    begin
      Data:= TATTabData(FTabList[i]);
      DoPaintTabTo(C, RRect,
        Format(FOptShowNumberPrefix, [i+1]) + Data.TabCaption,
        FColorTabActive,
        FColorBorderActive,
        IfThen(FOptShowBorderActiveLow, FColorBorderActive, clNone),
        Data.TabColor,
        NColorXBg,
        NColorXBorder,
        NColorXMark,
        IsShowX(i),
        Data.TabModified,
        Data.TabImageIndex
        );
      DoPaintAfter(aeTabActive, i, C, RRect);
    end;  
  end;

  DoPaintArrowLeft(C);
  DoPaintArrowRight(C);
  DoPaintArrowDown(C);
  DoPaintButtonPlus(C);
  DoPaintButtonClose(C);
  DoPaintUserButtons(C);

  if FOptShowDropMark then
    if _IsDrag then
      if PtInControl(Self, Mouse.CursorPos) then
        DoPaintDropMark(C);

  if FOptShowScrollMark then
    DoPaintScrollMark(C);
end;

procedure TATTabs.DoPaintDropMark(C: TCanvas);
var
  i: integer;
  R: TRect;
begin
  i:= FTabIndexDrop;
  if i<0 then i:= TabCount-1;
  if i<>FTabIndex then
  begin
    R:= GetTabRect(i);

    case FOptPosition of
      atpTop,
      atpBottom:
        begin
          R.Left:= IfThen(i<=FTabIndex, R.Left, R.Right);
          R.Left:= R.Left - FOptDropMarkSize div 2;
          R.Right:= R.Left + FOptDropMarkSize;
        end;
      else
        begin
          R.Top:= IfThen(i<=FTabIndex, R.Top, R.Bottom);
          R.Top:= R.Top  - FOptDropMarkSize div 2;
          R.Bottom:= R.Top + FOptDropMarkSize;
        end;
    end;

    C.Brush.Color:= FColorDropMark;
    C.FillRect(R);
  end;
end;


function TATTabs.IsScrollMarkNeeded: boolean;
var
  R: TRect;
begin
  if TabCount=0 then
    Result:= false
  else
  if FScrollPos>0 then
    Result:= true
  else
  case FOptPosition of
    atpTop,
    atpBottom:
      begin
        Result:= FTabWidth<=FOptTabWidthMinimal;
      end;
    else
      begin
        R:= GetTabRect(TabCount-1);
        if FOptShowPlusTab then
          Inc(R.Bottom, FOptTabHeight);
        Result:= R.Bottom>=ClientHeight;
      end;
  end;
end;

procedure TATTabs.DoPaintScrollMark(C: TCanvas);
var
  NPos, NSize, NIndent: integer;
  R: TRect;
begin
  if not IsScrollMarkNeeded then exit;

  case FOptPosition of
    atpTop,
    atpBottom:
      begin
        NPos:= GetMaxScrollPos;
        NSize:= ClientWidth - FRealIndentLeft - FRealIndentRight;

        if NPos>0 then
        begin
          R.Top:= IfThen(FOptPosition=atpBottom, FOptTabHeight + FOptSpacer, 0);
          R.Bottom:= R.Top + FOptScrollMarkSizeY;

          R.Left:= FRealIndentLeft +
            Max(0, Min(
              NSize-FOptScrollMarkSizeX,
              Int64(FScrollPos) * (NSize-FOptScrollMarkSizeX) div NPos
            ));
          R.Right:= R.Left + FOptScrollMarkSizeX;

          C.Brush.Color:= FColorScrollMark;
          C.FillRect(R);
        end;
      end;
    else
      begin
        NIndent:= GetInitialVerticalIndent;
        NPos:= GetMaxScrollPos;
        NSize:= ClientHeight-NIndent;

        if NPos>0 then
        begin
          R.Top:= NIndent +
            Max(0, Min(
              NSize - FOptScrollMarkSizeX,
              Int64(FScrollPos) * (NSize-FOptScrollMarkSizeX) div NPos
              ));
          R.Bottom:= R.Top + FOptScrollMarkSizeX;

          if FOptPosition=atpLeft then
          begin
            R.Left:= 0;
            R.Right:= R.Left + FOptScrollMarkSizeY;
          end
          else
          begin
            R.Right:= ClientWidth;
            R.Left:= R.Right - FOptScrollMarkSizeY;
          end;

          C.Brush.Color:= FColorScrollMark;
          C.FillRect(R);
        end;
      end;
  end;
end;

function TATTabs.RealTabAngle: integer;
begin
  {$ifdef darwin}
  //macOS paints angled tab bad
  exit(0);
  {$endif}

  if FOptPosition in [atpLeft, atpRight] then
    Result:= 0
  else
  if FTabList.Count>FOptUseAngleForMaxTabs then
    Result:= 0
  else
    Result:= FOptTabAngle;
end;

procedure TATTabs.SetTabs(AValue: TStrings);
var
  i: integer;
begin
  Clear;
  for i:= 0 to AValue.Count-1 do
    AddTab(-1, AValue[i]);
  Invalidate;
end;

procedure TATTabs.SetOptButtonLayout(const AValue: string);
begin
  if FOptButtonLayout=AValue then Exit;
  FOptButtonLayout:= AValue;
  ApplyButtonLayout;
end;


function TATTabs.GetTabAt(X, Y: integer): integer;
var
  i: integer;
  Pnt: TPoint;
  R1: TRect;
begin
  Result:= -1;
  Pnt:= Point(X, Y);

  if PtInRect(FRectArrowLeft, Pnt) then
  begin
    Result:= TabIndexArrowScrollLeft;
    Exit
  end;

  if PtInRect(FRectArrowRight, Pnt) then
  begin
    Result:= TabIndexArrowScrollRight;
    Exit
  end;

  if PtInRect(FRectArrowDown, Pnt) then
  begin
    Result:= TabIndexArrowMenu;
    Exit
  end;

  if PtInRect(FRectButtonPlus, Pnt) then
  begin
    Result:= TabIndexPlusBtn;
    Exit
  end;

  if PtInRect(FRectButtonClose, Pnt) then
  begin
    Result:= TabIndexCloseBtn;
    Exit
  end;

  if PtInRect(FRectButtonUser0, Pnt) then
  begin
    Result:= TabIndexUser0;
    Exit
  end;

  if PtInRect(FRectButtonUser1, Pnt) then
  begin
    Result:= TabIndexUser1;
    Exit
  end;

  if PtInRect(FRectButtonUser2, Pnt) then
  begin
    Result:= TabIndexUser2;
    Exit
  end;

  if PtInRect(FRectButtonUser3, Pnt) then
  begin
    Result:= TabIndexUser3;
    Exit
  end;

  if PtInRect(FRectButtonUser4, Pnt) then
  begin
    Result:= TabIndexUser4;
    Exit
  end;

  //normal tab?
  for i:= 0 to TabCount-1 do
  begin
    R1:= GetTabRect(i);
    if R1.Left>Pnt.X then exit;
    if PtInRect(R1, Pnt) then
    begin
      Result:= i;
      Exit;
    end;
  end;

  //plus tab?
  if FOptShowPlusTab then
    if PtInRect(GetTabRect_Plus, Pnt) then
    begin
      Result:= TabIndexPlus;
      Exit
    end;
end;

procedure TATTabs.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: integer);
var
  IsClick, IsDblClick, IsRightClick: boolean;
begin
  IsClick:= FMouseDown and
    (Abs(X-FMouseDownPnt.X) < cTabsMouseMaxDistanceToClick) and
    (Abs(Y-FMouseDownPnt.Y) < cTabsMouseMaxDistanceToClick);
  IsDblClick:= IsClick and FMouseDownDbl;
  IsRightClick:= FMouseDownRightBtn and
    (Abs(X-FMouseDownPnt.X) < cTabsMouseMaxDistanceToClick) and
    (Abs(Y-FMouseDownPnt.Y) < cTabsMouseMaxDistanceToClick);
       
  FMouseDown:= false;
  FMouseDownDbl:= false;
  FMouseDownRightBtn:= false;
  Cursor:= crDefault;
  Screen.Cursor:= crDefault;
  
  if IsDblClick then
  begin
    if FOptMouseDoubleClickClose and (FTabIndexOver>=0) then
      DeleteTab(FTabIndexOver, true, true)
    else
    if FOptMouseDoubleClickPlus and (FTabIndexOver=-1) then
      if Assigned(FOnTabPlusClick) then
        FOnTabPlusClick(Self);
    Exit
  end;

  if IsClick then     
  begin
    DoHandleClick;
    Invalidate;
    Exit
  end;

  if IsRightClick then
  begin
    DoHandleRightClick;
    Exit;
  end;
end;

procedure TATTabs.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: integer);
begin
  FMouseDown:= Button in [mbLeft, mbMiddle]; //but not mbRight
  FMouseDownRightBtn:= (Button = mbRight);
  FMouseDownPnt:= Point(X, Y);
  FMouseDownButton:= Button;
  FMouseDownShift:= Shift;

  FTabIndexOver:= GetTabAt(X, Y);
  SetTabIndex(FTabIndexOver);

  Invalidate;
end;


procedure TATTabs.DoHandleClick;
var
  R: TRect;
begin
  if FMouseDownButton=mbMiddle then
  begin
    if FOptMouseMiddleClickClose then
      if FTabIndexOver>=0 then
        DeleteTab(FTabIndexOver, true, true);
    Exit;
  end;

  if FMouseDownButton=mbLeft then
  begin
    case FTabIndexOver of
      TabIndexArrowMenu:
        begin
          EndDrag(false);
          FTabIndexOver:= -1;
          Invalidate;
          ShowTabMenu;
        end;

      TabIndexArrowScrollLeft:
        DoScrollLeft;

      TabIndexArrowScrollRight:
        DoScrollRight;

      TabIndexUser0:
        DoClickUser(0);
      TabIndexUser1:
        DoClickUser(1);
      TabIndexUser2:
        DoClickUser(2);
      TabIndexUser3:
        DoClickUser(3);
      TabIndexUser4:
        DoClickUser(4);

      TabIndexPlus,
      TabIndexPlusBtn:
        begin
          EndDrag(false);
          FTabIndexOver:= -1;
          if Assigned(FOnTabPlusClick) then
            FOnTabPlusClick(Self);
        end;

      TabIndexCloseBtn:
        begin
          DeleteTab(FTabIndex, true, true);
        end

      else
        begin
          if IsShowX(FTabIndexOver) then
          begin
            R:= GetTabRect(FTabIndexOver);
            R:= GetTabRect_X(R);
            if PtInRect(R, FMouseDownPnt) then
            begin
              EndDrag(false);
              DeleteTab(FTabIndexOver, true, true);
            end;
          end;
        end;
    end;
  end;
end;

procedure TATTabs.DoHandleRightClick;
var
  P: TPoint;
  D: TATTabData;
begin
  if (FTabIndex=FTabIndexOver) then // to check if click was processed as a valid click on a tab
  begin
    D:= GetTabData(FTabIndex);
    if Assigned(D) and Assigned(D.TabPopupMenu) then
    begin
      P:= ClientToScreen(FMouseDownPnt);
      D.TabPopupMenu.PopUp(P.X, P.Y);
    end;
  end;
end;

type
  TControl2 = class(TControl);

procedure TATTabs.MouseMove(Shift: TShiftState; X, Y: integer);
begin
  inherited;
  FTabIndexOver:= GetTabAt(X, Y);
  FTabIndexDrop:= FTabIndexOver;

  if Assigned(FOnTabOver) then
    FOnTabOver(Self, FTabIndexOver);

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
  Invalidate;
end;


procedure TATTabs.AddTab(
  AIndex: integer;
  const ACaption: TATTabString;
  AObject: TObject = nil;
  AModified: boolean = false;
  AColor: TColor = clNone;
  AImageIndex: integer = -1;
  APopupMenu: TPopupMenu = nil);
var
  Data: TATTabData;
begin
  Data:= TATTabData.Create;
  Data.TabCaption:= ACaption;
  Data.TabObject:= AObject;
  Data.TabModified:= AModified;
  Data.TabColor:= AColor;
  Data.TabImageIndex:= AImageIndex;
  Data.TabPopupMenu:= APopupMenu;

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

function TATTabs.DeleteTab(AIndex: integer; AAllowEvent, AWithCancelBtn: boolean): boolean;
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

procedure TATTabs.SetTabIndex(AIndex: integer);
var
  CanChange: boolean;
begin
  if csLoading in ComponentState then
    FTabIndexLoaded:= AIndex;

  if IsIndexOk(AIndex) then
  begin
    CanChange:= true;
    if Assigned(FOnTabChangeQuery) then
    begin
      FOnTabChangeQuery(Self, AIndex, CanChange);
      if not CanChange then Exit;
    end;

    FTabIndex:= AIndex;
    Invalidate;
    if Assigned(FOnTabClick) then
      FOnTabClick(Self);
  end;
end;


function TATTabs.GetTabData(AIndex: integer): TATTabData;
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

procedure TATTabs.DoPaintArrowTo(C: TCanvas; ATyp: TATTabTriangle; ARect: TRect;
  AColorArr, AColorBg: TColor);
var
  P: TPoint;
  R: TRect;
  N, SizeX, SizeY: integer;
begin
  N:= FOptArrowSize;
  case ATyp of
    atriLeft,
    atriRight:
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


function TATTabs.GetIndexOfButton(AData: TATTabButtons; ABtn: TATTabButton): integer;
var
  i: integer;
begin
  Result:= -1;
  for i:= 0 to High(AData) do
    if AData[i]=ABtn then
      begin Result:= i; exit; end;
end;

function TATTabs.GetTabs: TStrings;
begin
  Result:= FTabCaptions;
  UpdateTabCaptions;
end;

procedure TATTabs.UpdateTabCaptions;
var
  D: TATTabData;
  S: string;
  i: integer;
begin
  FTabCaptions.Clear;
  for i:= 0 to TabCount-1 do
  begin
    D:= GetTabData(i);
    if Assigned(D) then
      S:= D.TabCaption
    else
      S:= '?';
    FTabCaptions.Add(S);
  end;
end;

function TATTabs.GetRectOfButtonIndex(AIndex: integer; AtLeft: boolean): TRect;
begin
  if AtLeft then
  begin
    Result.Left:= AIndex*FOptButtonSize
      +IfThen(FOptPosition=atpLeft, FOptSpacer)
      +IfThen(FOptPosition=atpRight, FOptSpacer2)
      +1;
    Result.Right:= Result.Left+FOptButtonSize;
  end
  else
  begin
    Result.Right:= ClientWidth-AIndex*FOptButtonSize
      -IfThen(FOptPosition=atpLeft, FOptSpacer2)
      -IfThen(FOptPosition=atpRight, FOptSpacer)
      -1;
    Result.Left:= Result.Right-FOptButtonSize;
  end;

  Result.Top:= IfThen(
    FOptPosition in [atpTop, atpBottom],
    FOptSpacer,
    0);
  Result.Bottom:= Result.Top+FOptTabHeight;

  if FOptPosition=atpBottom then Inc(Result.Top);
end;

function TATTabs.GetRectOfButton(AButton: TATTabButton): TRect;
var
  N: integer;
begin
  Result:= Rect(0, 0, 0, 0);

  N:= GetIndexOfButton(FButtonsLeft, AButton);
  if N>=0 then
    Result:= GetRectOfButtonIndex(N, true)
  else
  begin
    N:= GetIndexOfButton(FButtonsRight, AButton);
    if N>=0 then
      Result:= GetRectOfButtonIndex(N, false);
  end;
end;


procedure TATTabs.ShowTabMenu;
var
  i: integer;
  mi: TATTabMenuItem;
  P: TPoint;
  bShow: boolean;
begin
  if TabCount=0 then Exit;

  bShow:= true;
  if Assigned(FOnTabMenu) then
    FOnTabMenu(Self, bShow);
  if not bShow then Exit;

  if not Assigned(FTabMenu) then
    FTabMenu:= TATTabPopupMenu.Create(Self);
  FTabMenu.Items.Clear;

  for i:= 0 to TabCount-1 do
  begin
    mi:= TATTabMenuItem.Create(Self);
    mi.Tag:= i;
    mi.Caption:= TATTabData(FTabList[i]).TabCaption;
    mi.OnClick:= TabMenuClick;
    //mi.RadioItem:= true; //bug in Lazarus/gtk2
    mi.Checked:= i=FTabIndex;
    FTabMenu.Items.Add(mi);
  end;

  P:= Point(FRectArrowDown.Left, FRectArrowDown.Bottom);
  P:= ClientToScreen(P);
  FTabMenu.Popup(P.X, P.Y);
end;

procedure TATTabs.TabMenuClick(Sender: TObject);
begin
  SetTabIndex((Sender as TComponent).Tag);
end;

procedure TATTabs.DoUpdateTabWidths;
var
  Value, Count, NAngle: integer;
begin
  Count:= TabCount;
  if Count=0 then Exit;

  if FOptPosition in [atpLeft, atpRight] then
  begin
    FTabWidth:= ClientWidth-FOptSpacer;
    exit
  end;

  //tricky formula: calculate auto-width
  NAngle:= RealTabAngle;
  Value:= (ClientWidth
    - IfThen(FOptShowPlusTab, GetTabWidth_Plus_Raw + 2*FOptSpaceBeforeText + 1*NAngle)
    - NAngle*2
    - FOptSpaceBetweenTabs
    - FRealIndentLeft
    - FRealIndentRight) div Count
      - FOptSpaceBetweenTabs;

  if Value<FOptTabWidthMinimal then
    Value:= FOptTabWidthMinimal
  else
  if Value>FOptTabWidthNormal then
    Value:= FOptTabWidthNormal;

  FTabWidth:= Value;
end;

function TATTabs.IsShowX(AIndex: integer): boolean;
begin
  case FOptShowXButtons of
    atbxShowNone: Result:= false;
    atbxShowAll: Result:= true;
    atbxShowActive: Result:= AIndex=FTabIndex;
    atbxShowMouseOver: Result:= AIndex=FTabIndexOver;
    else Result:= false;
  end;

  if FOptPosition in [atpTop, atpBottom] then
    if FTabWidth<FOptTabWidthMinimalHidesX then
      Result:= false;
end;

procedure TATTabs.DoTabDrop;
var
  NFrom, NTo: integer;
begin
  NFrom:= FTabIndex;
  if not IsIndexOk(NFrom) then Exit;
  NTo:= FTabIndexDrop;
  if not IsIndexOk(NTo) then
    NTo:= TabCount-1;
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
  NTab, NTabTo: integer;
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
  if not ATabs.OptMouseDragEnabled then Exit;

  NTab:= FTabIndex;
  NTabTo:= ATabs.GetTabAt(APnt.X, APnt.Y); //-1 is allowed

  Data:= GetTabData(NTab);
  if Data=nil then Exit;

  ATabs.AddTab(NTabTo, Data.TabCaption, Data.TabObject,
    Data.TabModified, Data.TabColor, Data.TabImageIndex);

  //correct TabObject parent
  if Data.TabObject is TWinControl then
    if (Data.TabObject as TWinControl).Parent = Self.Parent then
      (Data.TabObject as TWinControl).Parent:= ATabs.Parent;

  //delete old tab (don't call OnTabClose)
  DeleteTab(NTab, false{AllowEvent}, false);

  //activate dropped tab
  if NTabTo<0 then
    ATabs.TabIndex:= ATabs.TabCount-1
  else
    ATabs.TabIndex:= NTabTo;
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

procedure TATTabs.DragOver(Source: TObject; X, Y: integer; State: TDragState;
  var Accept: Boolean);
begin
  Accept:=
    (Source is TATTabs) and
    FOptMouseDragEnabled and
    FOptMouseDragOutEnabled;
end;

procedure TATTabs.DragDrop(Source: TObject; X, Y: integer);
begin
  if not (Source is TATTabs) then exit;

  if (Source=Self) then
  begin
    //drop to itself
    if (FTabIndexDrop>=0) then
    begin
      DoTabDrop;
      Invalidate;
    end;
  end
  else
  begin
    //drop to anoter control
    (Source as TATTabs).DoTabDropToOtherControl(Self, Point(X, Y));
  end;
end;


function TATTabs.GetScrollPageSize: integer;
const
  cPercents = 80;
begin
  case FOptPosition of
    atpTop,
    atpBottom:
      Result:= ClientWidth * cPercents div 100;
    else
      Result:= ClientHeight * cPercents div 100;
  end;
end;

function TATTabs.GetMaxScrollPos: integer;
var
  D: TATTabData;
begin
  Result:= 0;
  if TabCount=0 then exit;
  D:= GetTabData(TabCount-1);

  case FOptPosition of
    atpTop,
    atpBottom:
      begin
        Result:= Max(0,
          D.TabRect.Right - ClientWidth + FRealIndentRight +
          IfThen(FOptShowPlusTab, GetTabRectWidth(true))
          );
      end;
    else
      begin
        Result:= Max(0,
          D.TabRect.Bottom - ClientHeight +
          IfThen(FOptShowPlusTab, FOptTabHeight)
          );
      end;
  end;
end;

procedure TATTabs.DoScrollAnimation(APosTo: integer);
const
  cStep = 70; //pixels
  cSleepTime = 20; //msec
begin
  Enabled:= false;
  try
    if APosTo>FScrollPos then
      repeat
        FScrollPos:= Min(APosTo, FScrollPos+cStep);
        Invalidate;
        Application.ProcessMessages;
        Sleep(cSleepTime);
      until FScrollPos=APosTo
    else
      repeat
        FScrollPos:= Max(APosTo, FScrollPos-cStep);
        Invalidate;
        Application.ProcessMessages;
        Sleep(cSleepTime);
      until FScrollPos=APosTo;
  finally
    Enabled:= true;
    Invalidate;
  end;
end;

procedure TATTabs.DoScrollLeft;
var
  NPos: integer;
begin
  NPos:= Max(0, FScrollPos-GetScrollPageSize);
  if NPos<>FScrollPos then
    DoScrollAnimation(NPos);
end;

procedure TATTabs.DoScrollRight;
var
  NPos: integer;
begin
  NPos:= GetMaxScrollPos;
  NPos:= Min(NPos, FScrollPos+GetScrollPageSize);
  if NPos<>FScrollPos then
    DoScrollAnimation(NPos);
end;

procedure TATTabs.DoPaintButtonPlus(C: TCanvas);
var
  bOver: boolean;
  ElemType: TATTabElemType;
  R: TRect;
  NColor: TColor;
begin
  bOver:= FTabIndexOver=TabIndexPlusBtn;
  if bOver then
    ElemType:= aeButtonPlusOver
  else
    ElemType:= aeButtonPlus;

  R:= FRectButtonPlus;
  if R.Right>0 then
    if IsPaintNeeded(ElemType, -1, C, R) then
    begin
      NColor:= IfThen(
        bOver and not _IsDrag,
        FColorArrowOver,
        FColorArrow);

      DoPaintBgTo(C, R);
      DrawPlusSign(C, R, FOptArrowSize, NColor);
      DoPaintAfter(ElemType, -1, C, R);
    end;
end;

procedure TATTabs.DoPaintButtonClose(C: TCanvas);
var
  bOver: boolean;
  ElemType: TATTabElemType;
  R: TRect;
  NColor: TColor;
begin
  bOver:= FTabIndexOver=TabIndexCloseBtn;
  if bOver then
    ElemType:= aeButtonCloseOver
  else
    ElemType:= aeButtonClose;

  R:= FRectButtonClose;
  if R.Right>0 then
    if IsPaintNeeded(ElemType, -1, C, R) then
    begin
      NColor:= IfThen(
        bOver and not _IsDrag,
        FColorArrowOver,
        FColorArrow);

      DoPaintBgTo(C, R);
      DrawCrossSign(C, R, FOptArrowSize, NColor);
      DoPaintAfter(ElemType, -1, C, R);
    end;
end;


procedure TATTabs.DoPaintArrowDown(C: TCanvas);
var
  bOver: boolean;
  ElemType: TATTabElemType;
begin
  bOver:= FTabIndexOver=TabIndexArrowMenu;
  if bOver then
    ElemType:= aeArrowDropdownOver
  else
    ElemType:= aeArrowDropdown;

  if FRectArrowDown.Right>0 then
    if IsPaintNeeded(ElemType, -1, C, FRectArrowDown) then
    begin
      DoPaintBgTo(C, FRectArrowDown);
      DoPaintArrowTo(C,
        atriDown,
        FRectArrowDown,
        IfThen(bOver and not _IsDrag,
          FColorArrowOver,
          FColorArrow),
        FColorBg);
      DoPaintAfter(ElemType, -1, C, FRectArrowDown);
    end;
end;

procedure TATTabs.DoPaintArrowLeft(C: TCanvas);
var
  bOver: boolean;
  ElemType: TATTabElemType;
  R: TRect;
begin
  bOver:= FTabIndexOver=TabIndexArrowScrollLeft;
  if bOver then
    ElemType:= aeArrowScrollLeftOver
  else
    ElemType:= aeArrowScrollLeft;

  if FRectArrowLeft.Right>0 then
    if IsPaintNeeded(ElemType, -1, C, FRectArrowLeft) then
    begin
      R:= FRectArrowLeft;
      if FOptShowArrowsNear then
        R.Left:= (R.Left+R.Right) div 2;

      DoPaintBgTo(C, FRectArrowLeft);
      DoPaintArrowTo(C,
        atriLeft,
        R,
        IfThen(bOver, FColorArrowOver, FColorArrow),
        FColorBg);
      DoPaintAfter(ElemType, -1, C, FRectArrowLeft);
    end;
end;

procedure TATTabs.DoPaintArrowRight(C: TCanvas);
var
  bOver: boolean;
  ElemType: TATTabElemType;
  R: TRect;
begin
  bOver:= FTabIndexOver=TabIndexArrowScrollRight;
  if bOver then
    ElemType:= aeArrowScrollRightOver
  else
    ElemType:= aeArrowScrollRight;

  if FRectArrowRight.Right<>0 then
    if IsPaintNeeded(ElemType, -1, C, FRectArrowRight) then
    begin
      R:= FRectArrowRight;
      if FOptShowArrowsNear then
        R.Right:= (R.Left+R.Right) div 2;

      DoPaintBgTo(C, FRectArrowRight);
      DoPaintArrowTo(C,
        atriRight,
        R,
        IfThen(bOver, FColorArrowOver, FColorArrow),
        FColorBg);
      DoPaintAfter(ElemType, -1, C, FRectArrowRight);
    end;
end;


function SwapString(const S: string): string;
var
  i: integer;
begin
  SetLength(Result, Length(S));
  for i:= 1 to Length(S) do
    Result[Length(S)+1-i]:= S[i];
end;

procedure TATTabs.ApplyButtonLayout;
  //
  procedure ApplySide(var Side: TATTabButtons; const S: string);
  var
    N, i: integer;
  begin
    N:= 0;
    FillChar(Side, SizeOf(Side), 0);
    for i:= 1 to Length(S) do
      case S[i] of
        '<': begin Side[N]:= atbScrollLeft; Inc(N) end;
        '>': begin Side[N]:= atbScrollRight; Inc(N) end;
        'v': begin Side[N]:= atbDropdownMenu; Inc(N) end;
        '+': begin Side[N]:= atbPlus; Inc(N) end;
        'x': begin Side[N]:= atbClose; Inc(N) end;
        '0': begin Side[N]:= atbUser0; Inc(N) end;
        '1': begin Side[N]:= atbUser1; Inc(N) end;
        '2': begin Side[N]:= atbUser2; Inc(N) end;
        '3': begin Side[N]:= atbUser3; Inc(N) end;
        '4': begin Side[N]:= atbUser4; Inc(N) end;
      end;
  end;
  //
var
  S, SL, SR: string;
  N: integer;
begin
  S:= FOptButtonLayout;
  N:= Pos(',', S);
  if N=0 then N:= 200;
  SL:= Copy(S, 1, N-1);
  SR:= Copy(S, N+1, MaxInt);

  ApplySide(FButtonsLeft, SL);
  ApplySide(FButtonsRight, SwapString(SR));
end;

procedure TATTabs.DoClickUser(AIndex: integer);
begin
  if Assigned(FOnTabClickUserButton) then
    FOnTabClickUserButton(Self, AIndex);
end;

procedure TATTabs.DoPaintUserButtons(C: TCanvas);
var
  ElemType: TATTabElemType;
  R: TRect;
  NIndex, i: integer;
begin
  for i:= 0 to 4 do
  begin
    case i of
      0: begin NIndex:= TabIndexUser0; R:= FRectButtonUser0; end;
      1: begin NIndex:= TabIndexUser1; R:= FRectButtonUser1; end;
      2: begin NIndex:= TabIndexUser2; R:= FRectButtonUser2; end;
      3: begin NIndex:= TabIndexUser3; R:= FRectButtonUser3; end;
      4: begin NIndex:= TabIndexUser4; R:= FRectButtonUser4; end;
      else Break;
    end;

    if R.Right=0 then Continue;
    DoPaintBgTo(C, R);

    if FTabIndexOver=NIndex then
      ElemType:= aeButtonUserOver
    else
      ElemType:= aeButtonUser;

    DoPaintAfter(ElemType, i, C, R);
  end;
end;

procedure TATTabs.Loaded;
var
  i: integer;
begin
  inherited;

  Clear;
  for i:= 0 to FTabCaptions.Count-1 do
    AddTab(-1, FTabCaptions[i]);

  TabIndex:= FTabIndexLoaded;
end;

function TATTabs.GetInitialVerticalIndent: integer;
begin
  if FOptButtonLayout='' then
    Result:= FOptSpaceInitial
  else
    Result:= FOptTabHeight;
end;

procedure TATTabs.DoPaintColoredBand(C: TCanvas; PL1, PL2, PR1, PR2: TPoint; AColor: TColor);
var
  NColor: TColor;
  Pos: TATTabPosition;
begin
  NColor:= C.Brush.Color;
  C.Brush.Color:= AColor;

  case FOptPosition of
    atpTop:
      Pos:= FOptColoredBandForTop;
    atpBottom:
      Pos:= FOptColoredBandForBottom;
    atpLeft:
      Pos:= FOptColoredBandForLeft;
    atpRight:
      Pos:= FOptColoredBandForRight;
  end;

  case Pos of
    atpTop:
      C.FillRect(Rect(PL1.X+1, PL1.Y+1, PR1.X, PR1.Y+1+FOptColoredBandSize));
    atpBottom:
      C.FillRect(Rect(PL2.X+1, PL2.Y-2, PR2.X, PR2.Y-2+FOptColoredBandSize));
    atpLeft:
      C.FillRect(Rect(PL1.X+1, PL1.Y+1, PL1.X+1+FOptColoredBandSize, PL2.Y));
    atpRight:
      C.FillRect(Rect(PR1.X-FOptColoredBandSize, PR1.Y+1, PR1.X, PR2.Y));
  end;

  C.Brush.Color:= NColor;
end;


end.

