{
ATTabs component for Delphi/Lazarus
Copyright (c) Alexey Torgashin (UVviewsoft.com)
License: MPL 2.0 or LGPL
}

unit attabs;

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
  TATTabPosition = (
    atpTop,
    atpBottom,
    atpLeft,
    atpRight
    );

  TATTabTruncateCaption = (
    attcNone,
    attcDotsLeft,
    attcDotsMiddle,
    attcDotsRight
    );

type
  { TATTabData }

  TATTabData = class(TCollectionItem)
  private
    FTabCaption: TATTabString;
    FTabHint: TATTabString;
    FTabObject: TObject;
    FTabColor: TColor;
    FTabModified: boolean;
    FTabSpecial: boolean;
    FTabSpecialWidth: integer;
    FTabSpecialHeight: integer;
    FTabRect: TRect;
    FTabImageIndex: integer;
    FTabPopupMenu: TPopupMenu;
    FTabFontStyle: TFontStyles;
    FTabStartsNewLine: boolean;
    FTabHideXButton: boolean;
  public
    constructor Create(ACollection: TCollection); override;
    property TabObject: TObject read FTabObject write FTabObject;
    property TabRect: TRect read FTabRect write FTabRect;
    property TabSpecial: boolean read FTabSpecial write FTabSpecial default false;
    property TabStartsNewLine: boolean read FTabStartsNewLine write FTabStartsNewLine;
  published
    property TabCaption: TATTabString read FTabCaption write FTabCaption;
    property TabHint: TATTabString read FTabHint write FTabHint;
    property TabColor: TColor read FTabColor write FTabColor default clNone;
    property TabModified: boolean read FTabModified write FTabModified default false;
    property TabImageIndex: integer read FTabImageIndex write FTabImageIndex default -1;
    property TabFontStyle: TFontStyles read FTabFontStyle write FTabFontStyle default [];
    property TabPopupMenu: TPopupMenu read FTabPopupMenu write FTabPopupMenu;
    property TabSpecialWidth: integer read FTabSpecialWidth write FTabSpecialWidth default 0;
    property TabSpecialHeight: integer read FTabSpecialHeight write FTabSpecialHeight default 0;
    property TabHideXButton: boolean read FTabHideXButton write FTabHideXButton default false;
  end;

type
  TATTabElemType = (
    aeBackground,
    aeSpacerRect,
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
  TATTabIconPosition = (
    aipIconLefterThanText,
    aipIconRighterThanText,
    aipIconCentered,
    aipIconAboveTextCentered,
    aipIconBelowTextCentered
    );

type
  TATTabActionOnClose = (
    aocDefault,
    aocRight,
    aocRecent
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
  TATTabGetTickEvent = function (Sender: TObject; ATabObject: TObject): Int64 of object;
  TATTabGetCloseActionEvent = procedure (Sender: TObject; var AAction: TATTabActionOnClose) of object;

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
  cTabIndexNone = -1; //none tab
  cTabIndexPlus = -2;
  cTabIndexPlusBtn = -3;
  cTabIndexCloseBtn = -4;
  cTabIndexArrowMenu = -5;
  cTabIndexArrowScrollLeft = -6;
  cTabIndexArrowScrollRight = -7;
  cTabIndexUser0 = -10;
  cTabIndexUser1 = -11;
  cTabIndexUser2 = -12;
  cTabIndexUser3 = -13;
  cTabIndexUser4 = -14;

const
  _InitTabColorBg = $585858;
  _InitTabColorTabActive = $808080;
  _InitTabColorTabPassive = $786868;
  _InitTabColorTabOver = $A08080;
  _InitTabColorActiveMark = $C04040;
  _InitTabColorFont = $D0D0D0;
  _InitTabColorFontModified = $A00000;
  _InitTabColorFontActive = clNone;
  _InitTabColorFontHot = clNone;
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
  _InitOptTruncateCaption = attcDotsMiddle;
  _InitOptAnimationEnabled = false;
  _InitOptAnimationStepV = 4;
  _InitOptAnimationStepH = 25;
  _InitOptAnimationPause = 60;
  _InitOptButtonLayout = '<>,v';
  _InitOptButtonSize = 16;
  _InitOptTabHeight = 24;
  _InitOptTabWidthMinimal = 40;
  _InitOptTabWidthMaximal = 300;
  _InitOptTabWidthNormal = 130;
  _InitOptTabWidthMinimalHidesX = 55;
  _InitOptSpaceInitial = 5;
  _InitOptSpaceBeforeText = 6;
  _InitOptSpaceBetweenTabs = 0;
  _InitOptSpaceBetweenLines = 4;
  _InitOptSpaceBetweenIconCaption = 0;
  _InitOptSpacer = 4;
  _InitOptSpacer2 = 10;
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
  _InitOptActiveFontStyle = [fsUnderline];
  _InitOptActiveFontStyleUsed = false;
  _InitOptHotFontStyle = [fsUnderline];
  _InitOptHotFontStyleUsed = false;

  _InitOptShowFlat = false;
  _InitOptShowFlatSep = true;
  _InitOptPosition = atpTop;
  _InitOptFillWidth = true;
  _InitOptFillWidthLastToo = false;
  _InitOptShowNumberPrefix = '';
  _InitOptShowScrollMark = true;
  _InitOptShowDropMark = true;
  _InitOptShowArrowsNear = true;
  _InitOptShowXButtons = atbxShowAll;
  _InitOptShowPlusTab = true;
  _InitOptShowModifiedText = '*';
  _InitOptShowBorderActiveLow = false;
  _InitOptShowEntireColor = false;
  _InitOptShowAngled = false;
  _InitOptShowAngleTangent = 2.6;

  _InitOptMouseMiddleClickClose = true;
  _InitOptMouseDoubleClickClose = true;
  _InitOptMouseDoubleClickPlus = false;
  _InitOptMouseDragEnabled = {$ifdef fpc} true {$else} false {$endif};
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
    FColorFont: TColor;
    FColorFontModified: TColor;
    FColorFontActive: TColor;
    FColorFontHot: TColor;
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

    FOptAnimationEnabled: boolean;
    FOptAnimationStepV: integer;
    FOptAnimationStepH: integer;
    FOptAnimationPause: integer;

    FOptVarWidth: boolean;
    FOptMultiline: boolean;
    FOptTruncateCaption: TATTabTruncateCaption;
    FOptFillWidth: boolean;
    FOptFillWidthLastToo: boolean;
    FOptTabHeight: integer;
    FOptTabWidthMinimal: integer; //tab minimal width (used when lot of tabs)
    FOptTabWidthMaximal: integer;
    FOptTabWidthNormal: integer; //tab maximal width (used when only few tabs)
    FOptTabWidthMinimalHidesX: integer; //tab minimal width, after which "x" mark hides for inactive tabs
    FOptSpaceBetweenTabs: integer; //space between nearest tabs
    FOptSpaceBetweenLines: integer;
    FOptSpaceBetweenIconCaption: integer;
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

    FOptPosition: TATTabPosition;
    FOptIconPosition: TATTabIconPosition;
    FOptWhichActivateOnClose: TATTabActionOnClose;
    FOptCaptionAlignment: TAlignment;
    FOptShowFlat: boolean;
    FOptShowFlatSep: boolean;
    FOptShowXButtons: TATTabShowClose; //show mode for "x" buttons
    FOptShowArrowsNear: boolean;
    FOptShowPlusTab: boolean; //show "plus" tab
    FOptShowModifiedText: TATTabString;
    FOptShowBorderActiveLow: boolean; //show border line below active tab (like Firefox)
    FOptShowEntireColor: boolean;
    FOptShowNumberPrefix: TATTabString;
    FOptShowScrollMark: boolean;
    FOptShowDropMark: boolean;
    FOptShowAngled: boolean;
    FOptActiveFontStyle: TFontStyles;
    FOptActiveFontStyleUsed: boolean;
    FOptHotFontStyle: TFontStyles;
    FOptHotFontStyleUsed: boolean;

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
    FTabIndexHinted: integer;
    FTabIndexAnimated: integer;
    FTabList: TCollection;
    FTabMenu: TATTabPopupMenu;
    FCaptionList: TStringList;
    FMultilineActive: boolean;

    FRealIndentLeft: integer;
    FRealIndentRight: integer;
    FAngleTangent: single;
    FAngleSide: integer;
    FAnimationOffset: integer;

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
    FOnTabGetTick: TATTabGetTickEvent;
    FOnTabGetCloseAction: TATTabGetCloseActionEvent;

    procedure ApplyButtonLayout;
    procedure DoAnimationTabAdd(AIndex: integer);
    procedure DoAnimationTabClose(AIndex: integer);
    procedure DoClickUser(AIndex: integer);
    procedure DoHandleClick;
    procedure DoHandleRightClick;
    procedure DoPaintArrowDown(C: TCanvas);
    procedure DoPaintArrowLeft(C: TCanvas);
    procedure DoPaintArrowRight(C: TCanvas);
    procedure DoPaintButtonClose(C: TCanvas);
    procedure DoPaintButtonPlus(C: TCanvas);
    procedure DoPaintButtonsBG(C: TCanvas);
    procedure DoPaintColoredBand(C: TCanvas; PL1, PL2, PR1, PR2: TPoint; AColor: TColor);
    procedure DoPaintTo(C: TCanvas);
    procedure DoPaintX(C: TCanvas; const ARectX: TRect; AMouseOverX: boolean;
      AColorBg, AColorCloseBg, AColorCloseBorder, AColorCloseXMark: TColor);
    procedure DoTextOut(C: TCanvas; AX, AY: integer; const AClipRect: TRect; const AText: string);
    procedure DoPaintBgTo(C: TCanvas; const ARect: TRect);
    procedure DoPaintTabTo(C: TCanvas; ARect: TRect; const ACaption: TATTabString;
      AColorBg, AColorBorder, AColorBorderLow, AColorHilite, AColorCloseBg,
      AColorCloseBorder, AColorCloseXMark, AColorFont: TColor; AShowCloseBtn,
      ATabModified, ATabActive: boolean; AImageIndex: integer;
      AFontStyle: TFontStyles);
    procedure DoPaintArrowTo(C: TCanvas; ATyp: TATTabTriangle; ARect: TRect;
      AColorArr: TColor);
    procedure DoPaintUserButtons(C: TCanvas);
    procedure DoPaintXTo(C: TCanvas; const R: TRect; ATabBg, ATabCloseBg,
      ATabCloseBorder, ATabCloseXMark: TColor);
    procedure DoPaintDropMark(C: TCanvas);
    procedure DoPaintScrollMark(C: TCanvas);
    function GetIndexOfButton(AData: TATTabButtons; ABtn: TATTabButton): integer;
    function GetInitialVerticalIndent: integer;
    function GetButtonsEmpty: boolean;
    function IsScrollMarkNeeded: boolean;
    function GetMaxEdgePos: integer;
    function GetRectOfButton(AButton: TATTabButton): TRect;
    function GetRectOfButtonIndex(AIndex: integer; AtLeft: boolean): TRect;
    function GetScrollPageSize: integer;
    procedure SetOptButtonLayout(const AValue: string);
    procedure SetOptMouseDragEnabled(AValue: boolean);
    procedure SetOptVarWidth(AValue: boolean);
    procedure SetScrollPos(AValue: integer);
    procedure SetTabIndex(AIndex: integer);
    procedure GetTabXProps(AIndex: integer; const ARect: TRect; out AColorXBg,
      AColorXBorder, AColorXMark: TColor; out AMouseOverX: boolean; out ARectX: TRect);
    function IsIndexOk(AIndex: integer): boolean;
    function IsShowX(AIndex: integer): boolean;
    function IsPaintNeeded(AElemType: TATTabElemType;
      AIndex: integer; ACanvas: TCanvas; const ARect: TRect): boolean;
    function DoPaintAfter(AElemType: TATTabElemType;
      AIndex: integer; ACanvas: TCanvas; const ARect: TRect): boolean;
    procedure TabMenuClick(Sender: TObject);
    function GetTabWidth_Plus_Raw: integer;
    procedure DoUpdateTabWidths;
    procedure DoUpdateTabRects(C: TCanvas);
    procedure DoUpdateTabRectsToFillLine(AIndexFrom, AIndexTo: integer; ALastLine: boolean);
    procedure DoUpdateCanvasAntialiasMode(C: TCanvas);
    procedure DoUpdateCaptionProps(C: TCanvas; const ACaption: TATTabString;
      out ALineHeight: integer; out ATextSize: TSize);
    procedure DoTabDrop;
    procedure DoTabDropToOtherControl(ATarget: TControl; const APnt: TPoint);
    function GetTabTick(AIndex: integer): Int64;

  public
    constructor Create(AOnwer: TComponent); override;
    function CanFocus: boolean; override;
    destructor Destroy; override;
    procedure DragDrop(Source: TObject; X, Y: integer); override;

    function GetTabRectWidth(APlusBtn: boolean): integer;
    function GetTabRect(AIndex: integer; AWithScroll: boolean=true;
      AWithAnimation: boolean=true): TRect;
    function GetTabRect_Plus(AWithScroll: boolean= true): TRect;
    function GetTabRect_X(const ARect: TRect): TRect;
    function GetTabAt(X, Y: integer; out APressedX: boolean): integer;
    function GetTabData(AIndex: integer): TATTabData;
    function TabCount: integer;
    procedure AddTab(
      AIndex: integer;
      const ACaption: TATTabString;
      AObject: TObject = nil;
      AModified: boolean = false;
      AColor: TColor = clNone;
      AImageIndex: integer = -1;
      APopupMenu: TPopupMenu = nil;
      AFontStyle: TFontStyles = [];
      const AHint: TATTabString = '');
    procedure Clear;
    function DeleteTab(AIndex: integer; AAllowEvent, AWithCancelBtn: boolean;
      AAction: TATTabActionOnClose=aocDefault): boolean;
    procedure MakeVisible(AIndex: integer);
    function IsTabVisible(AIndex: integer): boolean;
    procedure ShowTabMenu;
    procedure SwitchTab(ANext: boolean; ALoopAtEdge: boolean= true);
    procedure MoveTab(AFrom, ATo: integer; AActivateThen: boolean);
    procedure DoScrollLeft;
    procedure DoScrollRight;
    procedure DoScrollAnimation(APosTo: integer);
    function GetMaxScrollPos: integer;
    property ScrollPos: integer read FScrollPos write SetScrollPos;

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
    property Tabs: TCollection read FTabList write FTabList;

    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnContextPopup;
    //these 2 lines don't compile under Delphi 7
    property OnMouseEnter;
    property OnMouseLeave;
    //
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
    property TabIndex: integer read FTabIndex write SetTabIndex default 0;

    //colors
    property ColorBg: TColor read FColorBg write FColorBg default _InitTabColorBg;
    property ColorBorderActive: TColor read FColorBorderActive write FColorBorderActive default _InitTabColorBorderActive;
    property ColorBorderPassive: TColor read FColorBorderPassive write FColorBorderPassive default _InitTabColorBorderPassive;
    property ColorTabActive: TColor read FColorTabActive write FColorTabActive default _InitTabColorTabActive;
    property ColorTabPassive: TColor read FColorTabPassive write FColorTabPassive default _InitTabColorTabPassive;
    property ColorTabOver: TColor read FColorTabOver write FColorTabOver default _InitTabColorTabOver;
    property ColorActiveMark: TColor read FColorActiveMark write FColorActiveMark default _InitTabColorActiveMark;
    property ColorFont: TColor read FColorFont write FColorFont default _InitTabColorFont;
    property ColorFontModified: TColor read FColorFontModified write FColorFontModified default _InitTabColorFontModified;
    property ColorFontActive: TColor read FColorFontActive write FColorFontActive default _InitTabColorFontActive;
    property ColorFontHot: TColor read FColorFontHot write FColorFontHot default _InitTabColorFontHot;
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
    property OptAnimationEnabled: boolean read FOptAnimationEnabled write FOptAnimationEnabled default _InitOptAnimationEnabled;
    property OptAnimationStepVert: integer read FOptAnimationStepV write FOptAnimationStepV default _InitOptAnimationStepV;
    property OptAnimationStepHorz: integer read FOptAnimationStepH write FOptAnimationStepH default _InitOptAnimationStepH;
    property OptAnimationPause: integer read FOptAnimationPause write FOptAnimationPause default _InitOptAnimationPause;
    property OptButtonLayout: string read FOptButtonLayout write SetOptButtonLayout;
    property OptButtonSize: integer read FOptButtonSize write FOptButtonSize default _InitOptButtonSize;
    property OptVarWidth: boolean read FOptVarWidth write SetOptVarWidth default false;
    property OptMultiline: boolean read FOptMultiline write FOptMultiline default false;
    property OptFillWidth: boolean read FOptFillWidth write FOptFillWidth default _InitOptFillWidth;
    property OptFillWidthLastToo: boolean read FOptFillWidthLastToo write FOptFillWidthLastToo default _InitOptFillWidthLastToo;
    property OptTruncateCaption: TATTabTruncateCaption read FOptTruncateCaption write FOptTruncateCaption default _InitOptTruncateCaption;
    property OptTabHeight: integer read FOptTabHeight write FOptTabHeight default _InitOptTabHeight;
    property OptTabWidthNormal: integer read FOptTabWidthNormal write FOptTabWidthNormal default _InitOptTabWidthNormal;
    property OptTabWidthMinimal: integer read FOptTabWidthMinimal write FOptTabWidthMinimal default _InitOptTabWidthMinimal;
    property OptTabWidthMaximal: integer read FOptTabWidthMaximal write FOptTabWidthMaximal default _InitOptTabWidthMaximal;
    property OptTabWidthMinimalHidesX: integer read FOptTabWidthMinimalHidesX write FOptTabWidthMinimalHidesX default _InitOptTabWidthMinimalHidesX;
    property OptSpaceBetweenTabs: integer read FOptSpaceBetweenTabs write FOptSpaceBetweenTabs default _InitOptSpaceBetweenTabs;
    property OptSpaceBetweenLines: integer read FOptSpaceBetweenLines write FOptSpaceBetweenLines default _InitOptSpaceBetweenLines;
    property OptSpaceBetweenIconCaption: integer read FOptSpaceBetweenIconCaption write FOptSpaceBetweenIconCaption default _InitOptSpaceBetweenIconCaption;
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

    property OptPosition: TATTabPosition read FOptPosition write FOptPosition default _InitOptPosition;
    property OptIconPosition: TATTabIconPosition read FOptIconPosition write FOptIconPosition default aipIconLefterThanText;
    property OptWhichActivateOnClose: TATTabActionOnClose read FOptWhichActivateOnClose write FOptWhichActivateOnClose default aocRight;
    property OptCaptionAlignment: TAlignment read FOptCaptionAlignment write FOptCaptionAlignment default taLeftJustify;
    property OptShowAngled: boolean read FOptShowAngled write FOptShowAngled default _InitOptShowAngled;
    property OptShowAngleTangent: single read FAngleTangent write FAngleTangent {$ifdef fpc} default _InitOptShowAngleTangent {$endif};
    property OptShowFlat: boolean read FOptShowFlat write FOptShowFlat default _InitOptShowFlat;
    property OptShowFlatSepar: boolean read FOptShowFlatSep write FOptShowFlatSep default _InitOptShowFlatSep;
    property OptShowScrollMark: boolean read FOptShowScrollMark write FOptShowScrollMark default _InitOptShowScrollMark;
    property OptShowDropMark: boolean read FOptShowDropMark write FOptShowDropMark default _InitOptShowDropMark;
    property OptShowXButtons: TATTabShowClose read FOptShowXButtons write FOptShowXButtons default _InitOptShowXButtons;
    property OptShowPlusTab: boolean read FOptShowPlusTab write FOptShowPlusTab default _InitOptShowPlusTab;
    property OptShowArrowsNear: boolean read FOptShowArrowsNear write FOptShowArrowsNear default _InitOptShowArrowsNear;
    property OptShowModifiedText: TATTabString read FOptShowModifiedText write FOptShowModifiedText;
    property OptShowBorderActiveLow: boolean read FOptShowBorderActiveLow write FOptShowBorderActiveLow default _InitOptShowBorderActiveLow;
    property OptShowEntireColor: boolean read FOptShowEntireColor write FOptShowEntireColor default _InitOptShowEntireColor;
    property OptShowNumberPrefix: TATTabString read FOptShowNumberPrefix write FOptShowNumberPrefix;
    property OptActiveFontStyle: TFontStyles read FOptActiveFontStyle write FOptActiveFontStyle default _InitOptActiveFontStyle;
    property OptActiveFontStyleUsed: boolean read FOptActiveFontStyleUsed write FOptActiveFontStyleUsed default _InitOptActiveFontStyleUsed;
    property OptHotFontStyle: TFontStyles read FOptHotFontStyle write FOptHotFontStyle default _InitOptHotFontStyle;
    property OptHotFontStyleUsed: boolean read FOptHotFontStyleUsed write FOptHotFontStyleUsed default _InitOptHotFontStyleUsed;

    property OptMouseMiddleClickClose: boolean read FOptMouseMiddleClickClose write FOptMouseMiddleClickClose default _InitOptMouseMiddleClickClose;
    property OptMouseDoubleClickClose: boolean read FOptMouseDoubleClickClose write FOptMouseDoubleClickClose default _InitOptMouseDoubleClickClose;
    property OptMouseDoubleClickPlus: boolean read FOptMouseDoubleClickPlus write FOptMouseDoubleClickPlus default _InitOptMouseDoubleClickPlus;
    property OptMouseDragEnabled: boolean read FOptMouseDragEnabled write SetOptMouseDragEnabled default _InitOptMouseDragEnabled;
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
    property OnTabGetTick: TATTabGetTickEvent read FOnTabGetTick write FOnTabGetTick;
    property OnTabGetCloseAction: TATTabGetCloseActionEvent read FOnTabGetCloseAction write FOnTabGetCloseAction;
  end;

var
  cTabsMouseMinDistanceToDrag: integer = 10; //mouse must move >=N pixels to start drag-drop
  cTabsMouseMaxDistanceToClick: integer = 4; //if mouse moves during mouse-down >=N pixels, dont click

  function _ShortenStringEx(C: TCanvas;
    const Text: string;
    Mode: TATTabTruncateCaption;
    Width: integer;
    const DotsString: string = #$2026): string;

implementation

uses
  SysUtils,
  StrUtils,
  Dialogs,
  Forms,
  Math;

const
  cSmoothScale = 5;

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

procedure DrawLine(C: TCanvas; X1, Y1, X2, Y2: integer; AColor: TColor);
begin
  if Y1=Y2 then
    if X2>X1 then Inc(X2) else Dec(X2);
  if X1=X2 then
    if Y2>Y1 then Inc(Y2) else Dec(Y2);

  C.Pen.Color:= AColor;
  C.MoveTo(X1, Y1);
  C.LineTo(X2, Y2);
end;

procedure CanvasPaintTriangleDown(C: TCanvas; AColor: TColor; ACoord: TPoint; ASize: integer);
begin
  C.Brush.Color:= AColor;
  C.Pen.Color:= AColor;
  C.Polygon([
    Point(ACoord.X - ASize*2, ACoord.Y - ASize),
    Point(ACoord.X + ASize*2, ACoord.Y - ASize),
    Point(ACoord.X, ACoord.Y + ASize)
    ]);
end;

procedure CanvasPaintTriangleRight(C: TCanvas; AColor: TColor; ACoord: TPoint; ASize: integer);
begin
  C.Brush.Color:= AColor;
  C.Pen.Color:= AColor;
  C.Polygon([
    Point(ACoord.X - ASize, ACoord.Y - ASize*2),
    Point(ACoord.X + ASize, ACoord.Y),
    Point(ACoord.X - ASize, ACoord.Y + ASize*2)
    ]);
end;

procedure CanvasPaintTriangleLeft(C: TCanvas; AColor: TColor; ACoord: TPoint; ASize: integer);
begin
  C.Brush.Color:= AColor;
  C.Pen.Color:= AColor;
  C.Polygon([
    Point(ACoord.X + ASize, ACoord.Y - ASize*2),
    Point(ACoord.X - ASize, ACoord.Y),
    Point(ACoord.X + ASize, ACoord.Y + ASize*2)
    ]);
end;

procedure DrawTriangleType(C: TCanvas; AType: TATTabTriangle; const ARect: TRect; AColor: TColor; ASize: integer);
begin
  case AType of
    atriDown:
      CanvasPaintTriangleDown(C, AColor, CenterPoint(ARect), ASize);
    atriRight:
      CanvasPaintTriangleRight(C, AColor, CenterPoint(ARect), ASize);
    atriLeft:
      CanvasPaintTriangleLeft(C, AColor, CenterPoint(ARect), ASize);
  end;
end;


procedure DrawPlusSign(C: TCanvas; const R: TRect; ASize: integer; AColor: TColor);
var
  CX, CY: integer;
begin
  CX:= (R.Left+R.Right) div 2;
  CY:= (R.Top+R.Bottom) div 2;
  DrawLine(C, CX-ASize, CY, CX+ASize, CY, AColor);
  DrawLine(C, CX, CY-ASize, CX, CY+ASize, AColor);
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


type
  TATMissedPoint = (
    ampnTopLeft,
    ampnTopRight,
    ampnBottomLeft,
    ampnBottomRight
    );

procedure DrawTriangleRectFramed(C: TCanvas;
  AX, AY, ASizeX, ASizeY, AScale: integer;
  ATriKind: TATMissedPoint;
  AColorFill, AColorLine: TColor);
var
  b: TBitmap;
  p0, p1, p2, p3: TPoint;
  line1, line2: TPoint;
  ar: array[0..2] of TPoint;
begin
  b:= TBitmap.Create;
  try
    {$ifdef fpc}
    b.SetSize(ASizeX*AScale, ASizeY*AScale);
    {$else}
    b.Width:= ASizeX*AScale;
    b.Height:= ASizeY*AScale;
    {$endif}

    p0:= Point(0, 0);
    p1:= Point(b.Width, 0);
    p2:= Point(0, b.Height);
    p3:= Point(b.Width, b.Height);

    case ATriKind of
      ampnTopLeft: begin ar[0]:= p1; ar[1]:= p2; ar[2]:= p3; line1:= p1; line2:= p2; end;
      ampnTopRight: begin ar[0]:= p0; ar[1]:= p2; ar[2]:= p3; line1:= p0; line2:= p3; end;
      ampnBottomLeft: begin ar[0]:= p0; ar[1]:= p1; ar[2]:= p3; line1:= p0; line2:= p3; end;
      ampnBottomRight: begin ar[0]:= p0; ar[1]:= p1; ar[2]:= p2; line1:= p1; line2:= p2; end;
    end;

    //b.Canvas.Brush.Color:= AColorBG;
    //b.Canvas.FillRect(0, 0, b.Width, b.Height);
    b.Canvas.CopyRect(
      Rect(0, 0, b.Width, b.Height),
      C,
      Rect(AX, AY, AX+ASizeX, AY+ASizeY)
      );

    b.Canvas.Pen.Style:= psClear;
    b.Canvas.Brush.Color:= AColorFill;
    b.Canvas.Polygon(ar);
    b.Canvas.Pen.Style:= psSolid;

    b.Canvas.Pen.Color:= AColorLine;
    b.Canvas.Pen.Width:= AScale;
    b.Canvas.MoveTo(line1.X, line1.Y);
    b.Canvas.LineTo(line2.X, line2.Y);
    b.Canvas.Pen.Width:= 1;

    {$ifdef fpc}
    C.StretchDraw(Rect(AX, AY, AX+ASizeX, AY+ASizeY), b);
    {$else}
    //Delphi: StretchDraw cannot draw smooth
    StretchBlt(
      C.Handle, AX, AY, ASizeX, ASizeY,
      b.Canvas.Handle, 0, 0, b.Width, b.Height,
      C.CopyMode);
    {$endif}
  finally
    b.Free;
  end;
end;

function _ShortenStringEx(C: TCanvas;
  const Text: string;
  Mode: TATTabTruncateCaption;
  Width: integer;
  const DotsString: string = #$2026): string;
const
  cMinLen = 3;
var
  S, STemp: UnicodeString;
  N, i: integer;
begin
  if (Mode=attcNone) or
    (C.TextWidth(Text)<=Width) then
  begin
    Result:= Text;
    exit
  end;

  S:= Text;
  STemp:= S;

  case Mode of
    attcDotsLeft:
      begin
        repeat
          Delete(STemp, 1, 1);
          S:= DotsString+STemp;
        until (Length(S)<=cMinLen) or (C.TextWidth(S)<=Width);
      end;

    attcDotsMiddle:
      begin
        for i:= 2 to $FFFF do
        begin
          N:= (Length(STemp)+1) div 2 - i div 2;
          S:= Copy(STemp, 1, N)+DotsString+Copy(STemp, N+i, MaxInt);
          if (Length(S)<=cMinLen) or (C.TextWidth(S)<=Width) then Break;
        end;
      end;

    attcDotsRight:
      begin
        repeat
          SetLength(STemp, Length(STemp)-1);
          S:= STemp+DotsString;
        until (Length(S)<=cMinLen) or (C.TextWidth(S)<=Width);
      end;
  end;

  Result:= S;
end;


{ TATTabData }

constructor TATTabData.Create(ACollection: TCollection);
begin
  inherited;
  TabColor:= clNone;
  TabImageIndex:= -1;
  TabFontStyle:= [];
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
  FColorFont:= _InitTabColorFont;
  FColorFontModified:= _InitTabColorFontModified;
  FColorFontActive:= _InitTabColorFontActive;
  FColorFontHot:= _InitTabColorFontHot;
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

  FOptAnimationEnabled:= _InitOptAnimationEnabled;
  FOptAnimationStepV:= _InitOptAnimationStepV;
  FOptAnimationStepH:= _InitOptAnimationStepH;
  FOptAnimationPause:= _InitOptAnimationPause;

  FOptButtonLayout:= _InitOptButtonLayout;
  ApplyButtonLayout;
  FOptButtonSize:= _InitOptButtonSize;
  FOptCaptionAlignment:= taLeftJustify;
  FOptIconPosition:= aipIconLefterThanText;
  FOptWhichActivateOnClose:= aocRight;
  FOptFillWidth:= _InitOptFillWidth;
  FOptFillWidthLastToo:= _InitOptFillWidthLastToo;
  FOptTruncateCaption:= _InitOptTruncateCaption;
  FOptTabHeight:= _InitOptTabHeight;
  FOptTabWidthMinimal:= _InitOptTabWidthMinimal;
  FOptTabWidthMaximal:= _InitOptTabWidthMaximal;
  FOptTabWidthNormal:= _InitOptTabWidthNormal;
  FOptTabWidthMinimalHidesX:= _InitOptTabWidthMinimalHidesX;
  FOptSpaceInitial:= _InitOptSpaceInitial;
  FOptSpaceBeforeText:= _InitOptSpaceBeforeText;
  FOptSpaceBetweenTabs:= _InitOptSpaceBetweenTabs;
  FOptSpaceBetweenLines:= _InitOptSpaceBetweenLines;
  FOptSpaceBetweenIconCaption:= _InitOptSpaceBetweenIconCaption;
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
  FAngleTangent:= _InitOptShowAngleTangent;
  FOptActiveFontStyle:= _InitOptActiveFontStyle;
  FOptActiveFontStyleUsed:= _InitOptActiveFontStyleUsed;
  FOptHotFontStyle:= _InitOptHotFontStyle;
  FOptHotFontStyleUsed:= _InitOptHotFontStyleUsed;

  FOptShowFlat:= _InitOptShowFlat;
  FOptShowFlatSep:= _InitOptShowFlatSep;
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
  FOptShowAngled:= _InitOptShowAngled;

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
  FTabIndexHinted:= -1;
  FTabIndexAnimated:= -1;
  FAnimationOffset:= 0;
  FTabList:= TCollection.Create(TATTabData);
  FTabMenu:= nil;
  FScrollPos:= 0;
  FCaptionList:= TStringList.Create;
  {$ifdef FPC}
  FCaptionList.TextLineBreakStyle:= tlbsLF;
  {$endif}
end;

function TATTabs.CanFocus: boolean;
begin
  Result:= false;
end;

procedure TATTabs.Clear;
begin
  FTabList.Clear;
  FTabIndex:= 0;
end;

destructor TATTabs.Destroy;
begin
  Clear;
  FreeAndNil(FCaptionList);
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
  AColorBg, AColorBorder, AColorBorderLow, AColorHilite, AColorCloseBg, AColorCloseBorder, AColorCloseXMark, AColorFont: TColor;
  AShowCloseBtn, ATabModified, ATabActive: boolean;
  AImageIndex: integer;
  AFontStyle: TFontStyles);
const
  cIndentSep = 2;
var
  PL1, PL2, PR1, PR2: TPoint;
  RectText: TRect;
  NIndentL, NIndentR, NIndentTop, NLineHeight, NLineWidth: integer;
  TempCaption: TATTabString;
  Extent: TSize;
  bNeedMoreSpace: boolean;
  i: integer;
begin
  //optimize for 200 tabs
  if ARect.Left>=ClientWidth then exit;
  //skip tabs scrolled lefter
  if ARect.Right<=0 then exit;

  if FOptShowFlat then
    AColorBg:= ColorBg;

  if FOptShowEntireColor and (AColorHilite<>clNone) then
    AColorBg:= AColorHilite;

  C.Pen.Color:= AColorBg;
  C.Brush.Color:= AColorBg;

  RectText:= Rect(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom);
  bNeedMoreSpace:= (RectText.Right-RectText.Left<=30) and (ACaption<>'');
  NIndentL:= IfThen(not bNeedMoreSpace, FOptSpaceBeforeText, 2);
  NIndentR:= NIndentL+IfThen(AShowCloseBtn, FOptSpaceXRight);
  C.FillRect(RectText);
  RectText:= Rect(ARect.Left+NIndentL, ARect.Top, ARect.Right-NIndentR, ARect.Bottom);

  if FOptShowFlat and FOptShowFlatSep then
  begin
    C.Pen.Color:= ColorBorderActive;
    i:= ARect.Left - FOptSpaceBetweenTabs div 2;
    C.Line(i, ARect.Top+cIndentSep, i, ARect.Bottom-cIndentSep);
  end;

  //imagelist
  if Assigned(FImages) then
    if (AImageIndex>=0) and (AImageIndex<FImages.Count) then
    begin
      NIndentTop:=
        (RectText.Top + RectText.Bottom - FImages.Height + FOptColoredBandSize) div 2;
      case FOptIconPosition of
        aipIconLefterThanText:
          begin
            FImages.Draw(C,
              RectText.Left - 2,
              NIndentTop,
              AImageIndex);
            Inc(RectText.Left, FImages.Width+FOptSpaceBetweenIconCaption);
          end;
        aipIconRighterThanText:
          begin
            FImages.Draw(C,
              RectText.Right - FImages.Width + 2,
              NIndentTop,
              AImageIndex);
            Dec(RectText.Right, FImages.Width+FOptSpaceBetweenIconCaption);
          end;
        aipIconCentered:
          begin
            FImages.Draw(C,
              (RectText.Left + RectText.Right - FImages.Width) div 2,
              NIndentTop,
              AImageIndex);
          end;
        aipIconAboveTextCentered:
          begin
            FImages.Draw(C,
              (RectText.Left + RectText.Right - FImages.Width) div 2,
              RectText.Top + FOptColoredBandSize,
              AImageIndex);
            Inc(RectText.Top, FImages.Height+FOptSpaceBetweenIconCaption);
          end;
        aipIconBelowTextCentered:
          begin
            FImages.Draw(C,
              (RectText.Left + RectText.Right - FImages.Width) div 2,
              RectText.Bottom - FImages.Height,
              AImageIndex);
            Dec(RectText.Bottom, FImages.Height+FOptSpaceBetweenIconCaption);
          end;
      end;
    end;

  PL1:= Point(ARect.Left, ARect.Top);
  PL2:= Point(ARect.Left, ARect.Bottom-1);
  PR1:= Point(ARect.Right-1, ARect.Top);
  PR2:= Point(ARect.Right-1, ARect.Bottom-1);

  //caption
  if RectText.Right-RectText.Left>=8 then
  begin
    C.Font.Assign(Self.Font);
    C.Font.Style:= AFontStyle;
    C.Font.Color:= AColorFont;

    TempCaption:= ACaption;
    if ATabModified then
      TempCaption:= FOptShowModifiedText+TempCaption;
    DoUpdateCaptionProps(C, TempCaption, NLineHeight, Extent);

    NIndentTop:= (RectText.Bottom-RectText.Top-Extent.cy) div 2 + 1;

    for i:= 0 to FCaptionList.Count-1 do
    begin
      //calculate center pos for each FCaptionList[i]
      case FOptCaptionAlignment of
        taLeftJustify:
          NIndentL:= RectText.Left;

        taCenter:
          begin
            if FCaptionList.Count<2 then
              NLineWidth:= Extent.cx
            else
              NLineWidth:= C.TextWidth(FCaptionList[i]);
            NIndentL:= Max(
              RectText.Left,
              (RectText.Left+RectText.Right-NLineWidth) div 2
              );
          end;

        taRightJustify:
          begin
            if FCaptionList.Count<2 then
              NLineWidth:= Extent.cx
            else
              NLineWidth:= C.TextWidth(FCaptionList[i]);
            NIndentL:= Max(
              RectText.Left,
              RectText.Right-NLineWidth
              );
          end;
      end;

      DoTextOut(C,
        NIndentL,
        RectText.Top+NIndentTop+i*NLineHeight,
        RectText,
        _ShortenStringEx(C, FCaptionList[i], FOptTruncateCaption, RectText.Width)
        );
    end;
  end;

  //borders
  if FOptShowFlat then
  begin
    if ATabActive then
    begin
      C.Brush.Color:= ColorActiveMark;
      case FOptPosition of
        atpTop:
          C.FillRect(Rect(PL2.X+2, ARect.Bottom-FOptActiveMarkSize, PR2.X, ARect.Bottom));
        atpBottom:
          C.FillRect(Rect(PL1.X+2, ARect.Top, PR1.X, ARect.Top+FOptActiveMarkSize));
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
        if not FOptShowAngled then
          DrawLine(C, PL1.X, PL1.Y, PL2.X, PL2.Y+1, AColorBorder);
        if not FOptShowAngled then
          DrawLine(C, PR1.X, PR1.Y, PR2.X, PR2.Y+1, AColorBorder);
        DrawLine(C, PL1.X, PL1.Y, PR1.X, PL1.Y, AColorBorder);
        if AColorBorderLow<>clNone then
          DrawLine(C, PL2.X, ARect.Bottom, PR2.X, ARect.Bottom, AColorBorderLow)
        else
          DrawLine(C, PL2.X+1, ARect.Bottom, PR2.X-1, ARect.Bottom, AColorBg);
      end;
    atpBottom:
      begin
        DrawLine(C, PL1.X, PL1.Y, PL2.X, PL2.Y+1, AColorBorder);
        DrawLine(C, PR1.X, PR1.Y, PR2.X, PR2.Y+1, AColorBorder);
        DrawLine(C, PL2.X, PL2.Y+1, PR2.X, PL2.Y+1, AColorBorder);
        if AColorBorderLow<>clNone then
          DrawLine(C, PL1.X, ARect.Top, PR1.X, ARect.Top, AColorBorderLow)
      end;
    atpLeft:
      begin
        DrawLine(C, PL1.X, PL1.Y, PR1.X, PR1.Y, AColorBorder);
        DrawLine(C, PL2.X, PL2.Y, PR2.X, PR2.Y, AColorBorder);
        DrawLine(C, PL1.X, PL1.Y, PL2.X, PL2.Y, AColorBorder);
        DrawLine(C, PR1.X+1, PR1.Y+1, PR1.X+1, PR2.Y-1, IfThen(AColorBorderLow<>clNone, AColorBorderLow, AColorBg));
      end;
    atpRight:
      begin
        DrawLine(C, PL1.X, PL1.Y, PR1.X, PR1.Y, AColorBorder);
        DrawLine(C, PL2.X, PL2.Y, PR2.X, PR2.Y, AColorBorder);
        DrawLine(C, PL1.X-1, PL1.Y+1, PL1.X-1, PL2.Y-1, IfThen(AColorBorderLow<>clNone, AColorBorderLow, AColorBg));
        DrawLine(C, PR1.X, PR1.Y, PR2.X, PR2.Y, AColorBorder);
      end;
  end;

  DoUpdateCanvasAntialiasMode(C);
  
  //angled tabs
  if FOptShowAngled and not FOptShowFlat then
    case FOptPosition of
      atpTop:
        begin
          DrawTriangleRectFramed(C,
            ARect.Left-FAngleSide+1,
            ARect.Top,
            FAngleSide,
            FOptTabHeight+IfThen(ATabActive, 1),
            cSmoothScale,
            ampnTopLeft,
            AColorBg,
            AColorBorder);
          DrawTriangleRectFramed(C,
            ARect.Right-1,
            ARect.Top,
            FAngleSide,
            FOptTabHeight+IfThen(ATabActive, 1),
            cSmoothScale,
            ampnTopRight,
            AColorBg,
            AColorBorder);
        end;
      atpBottom:
        begin
          DrawTriangleRectFramed(C,
            ARect.Left-FAngleSide+1,
            ARect.Top+IfThen(not ATabActive, 1),
            FAngleSide,
            FOptTabHeight,
            cSmoothScale,
            ampnBottomLeft,
            AColorBg,
            AColorBorder);
          DrawTriangleRectFramed(C,
            ARect.Right-1,
            ARect.Top+IfThen(not ATabActive, 1),
            FAngleSide,
            FOptTabHeight,
            cSmoothScale,
            ampnBottomRight,
            AColorBg,
            AColorBorder);
        end;
    end;

  //colored band
  if not FOptShowEntireColor then
    if AColorHilite<>clNone then
      DoPaintColoredBand(C, PL1, PL2, PR1, PR2, AColorHilite);
end;

procedure TATTabs.DoPaintX(C: TCanvas;
  const ARectX: TRect; AMouseOverX: boolean;
  AColorBg, AColorCloseBg, AColorCloseBorder, AColorCloseXMark: TColor);
var
  ElemType: TATTabElemType;
begin
  if AMouseOverX then
    ElemType:= aeTabIconXOver
  else
    ElemType:= aeTabIconX;

  if IsPaintNeeded(ElemType, -1, C, ARectX) then
  begin
    DoPaintXTo(C, ARectX, AColorBg, AColorCloseBg, AColorCloseBorder, AColorCloseXMark);
    DoPaintAfter(ElemType, -1, C, ARectX);
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
        Inc(Result, 2*FOptSpaceBeforeText);
      end;
  end;
end;


function TATTabs.GetTabRect(AIndex: integer; AWithScroll: boolean=true; AWithAnimation: boolean=true): TRect;
var
  Data: TATTabData;
begin
  Data:= GetTabData(AIndex);
  if Assigned(Data) then
  begin
    Result:= Data.TabRect;
    if AWithAnimation and (AIndex=FTabIndexAnimated) then
      case FOptPosition of
        atpTop:
          Inc(Result.Top, FAnimationOffset);
        atpBottom:
          Dec(Result.Bottom, FAnimationOffset);
        atpLeft:
          Inc(Result.Left, FAnimationOffset);
        atpRight:
          Dec(Result.Right, FAnimationOffset);
      end;
  end
  else
    Result:= Rect(0, 0, 10, 10);

  if AWithScroll then
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

procedure TATTabs.DoUpdateTabRects(C: TCanvas);
var
  TempCaption: TATTabString;
  Data: TATTabData;
  R: TRect;
  Extent: TSize;
  NWidthPlus, NIndexLineStart, NLineHeight, NWidthSaved: integer;
  i: integer;
begin
  //left/right tabs
  if FOptPosition in [atpLeft, atpRight] then
  begin
    R.Left:= IfThen(FOptPosition=atpLeft, FOptSpacer, FOptSpacer2+1);
    R.Right:= IfThen(FOptPosition=atpLeft, ClientWidth-FOptSpacer2, ClientWidth-FOptSpacer);
    R.Bottom:= GetInitialVerticalIndent;
    R.Top:= R.Bottom;

    for i:= 0 to TabCount-1 do
    begin
      Data:= GetTabData(i);
      if not Assigned(Data) then Continue;

      R.Top:= R.Bottom;
      if i>0 then
        Inc(R.Top, FOptSpaceBetweenTabs);

      if Data.TabSpecialHeight>0 then
        NLineHeight:= Data.TabSpecialHeight
      else
      if FOptVarWidth then
      begin
        DoUpdateCaptionProps(C, Data.TabCaption, NLineHeight, Extent);
        NLineHeight:= 2*FOptSpaceBeforeText + Extent.CY;
      end
      else
        NLineHeight:= FOptTabHeight;

      R.Bottom:= R.Top + NLineHeight;
      Data.TabRect:= R;
    end;

    exit;
  end;

  //top/bottom tabs
  FMultilineActive:= false;
  NWidthPlus:= 0;
  if FOptShowPlusTab then
    NWidthPlus:= GetTabRectWidth(true);
  if FOptMultiline then
    FTabWidth:= FOptTabWidthNormal;
  NWidthSaved:= FTabWidth;

  R.Left:= FRealIndentLeft;
  R.Right:= R.Left;
  R.Top:= FOptSpacer;
  R.Bottom:= R.Top+FOptTabHeight;
  NIndexLineStart:= 0;

  for i:= 0 to TabCount-1 do
  begin
    Data:= GetTabData(i);
    if not Assigned(Data) then Continue;
    Data.TabStartsNewLine:= false;

    R.Left:= R.Right;
    if i>0 then
      Inc(R.Left, FOptSpaceBetweenTabs);

    if Data.TabSpecialWidth>0 then
      FTabWidth:= Data.TabSpecialWidth
    else
    if FOptVarWidth then
    begin
      C.Font.Style:= Data.TabFontStyle;
      TempCaption:=
        Format(FOptShowNumberPrefix, [i+1]) +
        FOptShowModifiedText +
        Data.TabCaption;

      DoUpdateCaptionProps(C, TempCaption, NLineHeight, Extent);
      FTabWidth:= Extent.CX + 2*FOptSpaceBeforeText;

      if Data.TabImageIndex>=0 then
        if FOptIconPosition in [aipIconLefterThanText, aipIconRighterThanText] then
          Inc(FTabWidth, FImages.Width);

      if FOptShowXButtons<>atbxShowNone then
        if not Data.TabHideXButton then
          Inc(FTabWidth, FOptSpaceXSize);

      if FTabWidth<FOptTabWidthMinimal then
        FTabWidth:= FOptTabWidthMinimal;
      if FTabWidth>FOptTabWidthMaximal then
        FTabWidth:= FOptTabWidthMaximal;
    end;

    if FOptMultiline and (i>0) then
      if R.Left+FTabWidth+FRealIndentRight+NWidthPlus >= ClientWidth then
      begin
        Data.TabStartsNewLine:= true;
        FMultilineActive:= true;

        R.Left:= FRealIndentLeft;
        R.Top:= R.Bottom+FOptSpaceBetweenLines;
        R.Bottom:= R.Top+FOptTabHeight;

        if FOptFillWidth then
          DoUpdateTabRectsToFillLine(NIndexLineStart, i-1, false);
        NIndexLineStart:= i;
      end;

    R.Right:= R.Left + FTabWidth;
    Data.TabRect:= R;
  end;

  if FOptFillWidth and FOptFillWidthLastToo then
    DoUpdateTabRectsToFillLine(NIndexLineStart, TabCount-1, true);

  if FOptMultiline then
    Height:= R.Bottom+FOptSpacer2;

  //restore FTabWidth for other methods
  if not FOptVarWidth then
    FTabWidth:= NWidthSaved;
end;

function TATTabs.GetTabRect_Plus(AWithScroll: boolean=true): TRect;
begin
  case FOptPosition of
    atpTop,
    atpBottom:
      begin
        if TabCount>0 then
        begin
          Result:= GetTabRect(TabCount-1, AWithScroll, false);
          Result.Left:= Result.Right + FOptSpaceBetweenTabs;
          Result.Right:= Result.Left + GetTabRectWidth(true);
        end
        else
        begin
          Result.Top:= FOptSpacer;
          Result.Bottom:= Result.Top + FOptTabHeight;
          Result.Left:= FRealIndentLeft;
          Result.Right:= Result.Left + GetTabRectWidth(true);
        end;
      end;
    else
      begin
        if TabCount>0 then
        begin
          Result:= GetTabRect(TabCount-1, AWithScroll, false);
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
    ARect.Right-FOptSpaceXRight,
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

procedure TATTabs.GetTabXProps(AIndex: integer; const ARect: TRect;
  out AColorXBg, AColorXBorder, AColorXMark: TColor;
  out AMouseOverX: boolean;
  out ARectX: TRect);
begin
  if FOptShowFlat then
    AColorXBg:= FColorBg
  else
    AColorXBg:= FColorCloseBg;
  AColorXBorder:= AColorXBg;
  AColorXMark:= FColorCloseX;

  AMouseOverX:= false;
  ARectX:= GetTabRect_X(ARect);

  if _IsDrag then Exit;

  if IsShowX(AIndex) then
    if AIndex=FTabIndexOver then
    begin
      AMouseOverX:= PtInRect(ARectX, ScreenToClient(Mouse.CursorPos));
      if AMouseOverX then
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
  RRect, RBottom, RectX: TRect;
  NColorBg, NColorXBg, NColorXBorder, NColorXMark, NColorFont: TColor;
  NLineX1, NLineY1, NLineX2, NLineY2: integer;
  ElemType: TATTabElemType;
  Data: TATTabData;
  NFontStyle: TFontStyles;
  bShowX, bMouseOver, bMouseOverX: boolean;
  i: integer;
begin
  ElemType:= aeBackground;
  RRect:= ClientRect;

  FAngleSide:= Trunc(FOptTabHeight/FAngleTangent);

  FRealIndentLeft:= FOptSpaceInitial;
  FRealIndentRight:= FOptSpaceInitial;
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

  if FOptMultiline then
    FScrollPos:= 0;

  C.Font.Assign(Self.Font);
  DoUpdateTabWidths;
  DoUpdateTabRects(C);

  //paint spacer rect
  if not FOptShowFlat then
  begin
    ElemType:= aeSpacerRect;
    case FOptPosition of
      atpTop:
        begin
          if FOptMultiline then
            RBottom:= Rect(0, ClientHeight-FOptSpacer2, ClientWidth, ClientHeight)
          else
            RBottom:= Rect(0, FOptSpacer+FOptTabHeight, ClientWidth, ClientHeight);
          NLineX1:= RBottom.Left;
          NLineY1:= RBottom.Top;
          NLineX2:= RBottom.Right;
          NLineY2:= RBottom.Top;
        end;
      atpBottom:
        begin
          RBottom:= Rect(0, 0, ClientWidth, FOptSpacer);
          NLineX1:= RBottom.Left;
          NLineY1:= RBottom.Bottom;
          NLineX2:= RBottom.Right;
          NLineY2:= RBottom.Bottom;
        end;
      atpLeft:
        begin
          RBottom:= Rect(ClientWidth-FOptSpacer2, 0, ClientWidth, ClientHeight);
          NLineX1:= RBottom.Left;
          NLineY1:= RBottom.Top;
          NLineX2:= RBottom.Left;
          NLineY2:= RBottom.Bottom;
        end;
      atpRight:
        begin
          RBottom:= Rect(0, 0, FOptSpacer2, ClientHeight);
          NLineX1:= RBottom.Right;
          NLineY1:= RBottom.Top;
          NLineX2:= RBottom.Right;
          NLineY2:= RBottom.Bottom;
        end;
    end;

    if IsPaintNeeded(ElemType, -1, C, RBottom) then
    begin
      C.Brush.Color:= FColorTabActive;
      C.FillRect(RBottom);
      DrawLine(C, NLineX1, NLineY1, NLineX2, NLineY2, FColorBorderActive);
      DoPaintAfter(ElemType, -1, C, RBottom);
    end;
  end;

  //paint "plus" tab
  if FOptShowPlusTab then
  begin
    RRect:= GetTabRect_Plus;
    NColorXBg:= clNone;
    NColorXBorder:= clNone;
    NColorXMark:= clWhite;
    NColorFont:= FColorFont;
    if FTabIndexOver=cTabIndexPlus then
      ElemType:= aeTabPlusOver
    else
      ElemType:= aeTabPlus;
    if IsPaintNeeded(ElemType, -1, C, RRect) then
    begin
      DoPaintTabTo(C, RRect,
        '',
        IfThen((FTabIndexOver=cTabIndexPlus) and not _IsDrag, FColorTabOver, FColorTabPassive),
        FColorBorderPassive,
        FColorBorderActive,
        clNone,
        NColorXBg,
        NColorXBorder,
        NColorXMark,
        NColorFont,
        false,
        false,
        false,
        -1, //no icon
        []
        );
      DrawPlusSign(C, RRect, FOptArrowSize, FColorFont);
      DoPaintAfter(ElemType, -1, C, RRect);
    end;    
  end;

  //paint passive tabs
  for i:= TabCount-1 downto 0 do
    if i<>FTabIndex then
    begin
      RRect:= GetTabRect(i);
      GetTabXProps(i, RRect, NColorXBg, NColorXBorder, NColorXMark, bMouseOverX, RectX);

      bMouseOver:= i=FTabIndexOver;
      bShowX:= IsShowX(i);

      if bMouseOver and not _IsDrag then
        NColorBg:= FColorTabOver
      else
        NColorBg:= FColorTabPassive;

      if bMouseOver then
        ElemType:= aeTabPassiveOver
      else
        ElemType:= aeTabPassive;

      if IsPaintNeeded(ElemType, i, C, RRect) then
      begin
        Data:= TATTabData(FTabList.Items[i]);

        if FOptHotFontStyleUsed and bMouseOver then
          NFontStyle:= FOptHotFontStyle
        else
          NFontStyle:= Data.TabFontStyle;

        if (FColorFontHot<>clNone) and bMouseOver then
          NColorFont:= FColorFontHot
        else
        if Data.TabModified then
          NColorFont:= FColorFontModified
        else
          NColorFont:= FColorFont;

        DoPaintTabTo(C, RRect,
          Format(FOptShowNumberPrefix, [i+1]) + Data.TabCaption,
          NColorBg,
          FColorBorderPassive,
          FColorBorderActive,
          Data.TabColor,
          NColorXBg,
          NColorXBorder,
          NColorXMark,
          NColorFont,
          bShowX,
          Data.TabModified,
          false,
          Data.TabImageIndex,
          NFontStyle
          );
        DoPaintAfter(ElemType, i, C, RRect);
      end;

      if bShowX then
        DoPaintX(C, RectX,
          bMouseOverX,
          NColorBg,
          NColorXBg,
          NColorXBorder,
          NColorXMark
          );
    end;

  //paint active tab
  i:= FTabIndex;
  if IsIndexOk(i) then
  begin
    RRect:= GetTabRect(i);
    GetTabXProps(i, RRect, NColorXBg, NColorXBorder, NColorXMark, bMouseOverX, RectX);

    bMouseOver:= i=FTabIndexOver;
    bShowX:= IsShowX(i);

    if IsPaintNeeded(aeTabActive, i, C, RRect) then
    begin
      Data:= TATTabData(FTabList.Items[i]);

      NColorBg:= FColorTabActive;

      if FOptActiveFontStyleUsed then
        NFontStyle:= FOptActiveFontStyle
      else
        NFontStyle:= Data.TabFontStyle;

      if FColorFontActive<>clNone then
        NColorFont:= FColorFontActive
      else
      if Data.TabModified then
        NColorFont:= FColorFontModified
      else
        NColorFont:= FColorFont;

      DoPaintTabTo(C, RRect,
        Format(FOptShowNumberPrefix, [i+1]) + Data.TabCaption,
        NColorBg,
        FColorBorderActive,
        IfThen(FOptShowBorderActiveLow, FColorBorderActive, clNone),
        Data.TabColor,
        NColorXBg,
        NColorXBorder,
        NColorXMark,
        NColorFont,
        bShowX,
        Data.TabModified,
        true,
        Data.TabImageIndex,
        NFontStyle
        );
      DoPaintAfter(aeTabActive, i, C, RRect);
    end;

    if bShowX then
      DoPaintX(C, RectX,
        bMouseOverX,
        NColorBg,
        NColorXBg,
        NColorXBorder,
        NColorXMark
        );
  end;

  //button back
  DoPaintButtonsBG(C);
  //buttons
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

procedure TATTabs.DoTextOut(C: TCanvas; AX, AY: integer;
  const AClipRect: TRect; const AText: string);
var
  Str: WideString;
begin
  {$ifdef WIDE}
  Str:= UTF8Decode(AText);
  ExtTextOutW(C.Handle, AX, AY, ETO_CLIPPED, @AClipRect,
    PWideChar(Str), Length(Str), nil);
  {$else}
  ExtTextOut(C.Handle, AX, AY, ETO_CLIPPED, @AClipRect,
    PChar(AText), Length(AText), nil);
  {$endif}
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
begin
  if FOptMultiline then
    Result:= false
  else
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
        if not FOptVarWidth then
          Result:= FTabWidth<=FOptTabWidthMinimal
        else
          Result:= GetMaxScrollPos>0;
      end;
    else
      begin
        Result:= GetMaxScrollPos>0;
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

procedure TATTabs.SetOptButtonLayout(const AValue: string);
begin
  if FOptButtonLayout=AValue then Exit;
  FOptButtonLayout:= AValue;
  ApplyButtonLayout;
end;

procedure TATTabs.SetOptMouseDragEnabled(AValue: boolean);
begin
  if FOptMouseDragEnabled=AValue then Exit;

  {$ifdef FPC}
  FOptMouseDragEnabled:= AValue;
  {$else}
  ShowMessage('Dragging of tabs is not yet implemented under Delphi, sorry');
  FOptMouseDragEnabled:= false;
  {$endif}
end;

procedure TATTabs.SetOptVarWidth(AValue: boolean);
begin
  if FOptVarWidth=AValue then Exit;
  FOptVarWidth:= AValue;
  if not AValue then
    FScrollPos:= 0;
end;


function TATTabs.GetTabAt(X, Y: integer; out APressedX: boolean): integer;
var
  Pnt: TPoint;
  RectTab: TRect;
  i: integer;
begin
  Result:= -1;
  APressedX:= false;
  Pnt:= Point(X, Y);

  if PtInRect(FRectArrowLeft, Pnt) then
  begin
    Result:= cTabIndexArrowScrollLeft;
    Exit
  end;

  if PtInRect(FRectArrowRight, Pnt) then
  begin
    Result:= cTabIndexArrowScrollRight;
    Exit
  end;

  if PtInRect(FRectArrowDown, Pnt) then
  begin
    Result:= cTabIndexArrowMenu;
    Exit
  end;

  if PtInRect(FRectButtonPlus, Pnt) then
  begin
    Result:= cTabIndexPlusBtn;
    Exit
  end;

  if PtInRect(FRectButtonClose, Pnt) then
  begin
    Result:= cTabIndexCloseBtn;
    Exit
  end;

  if PtInRect(FRectButtonUser0, Pnt) then
  begin
    Result:= cTabIndexUser0;
    Exit
  end;

  if PtInRect(FRectButtonUser1, Pnt) then
  begin
    Result:= cTabIndexUser1;
    Exit
  end;

  if PtInRect(FRectButtonUser2, Pnt) then
  begin
    Result:= cTabIndexUser2;
    Exit
  end;

  if PtInRect(FRectButtonUser3, Pnt) then
  begin
    Result:= cTabIndexUser3;
    Exit
  end;

  if PtInRect(FRectButtonUser4, Pnt) then
  begin
    Result:= cTabIndexUser4;
    Exit
  end;

  //normal tab?
  for i:= 0 to TabCount-1 do
  begin
    RectTab:= GetTabRect(i);

    if not FOptMultiline then
      if RectTab.Left>Pnt.X then exit;

    if PtInRect(RectTab, Pnt) then
    begin
      Result:= i;
      APressedX:= PtInRect(GetTabRect_X(RectTab), Pnt);
      Exit;
    end;
  end;

  //plus tab?
  if FOptShowPlusTab then
    if PtInRect(GetTabRect_Plus, Pnt) then
    begin
      Result:= cTabIndexPlus;
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
var
  IsX: boolean;
begin
  FMouseDown:= Button in [mbLeft, mbMiddle]; //but not mbRight
  FMouseDownRightBtn:= (Button = mbRight);
  FMouseDownPnt:= Point(X, Y);
  FMouseDownButton:= Button;
  FMouseDownShift:= Shift;

  FTabIndexOver:= GetTabAt(X, Y, IsX);

  //activate tab only if not X clicked
  if not IsX then
    if TabIndex<>FTabIndexOver then
      TabIndex:= FTabIndexOver;

  Invalidate;
end;


procedure TATTabs.DoHandleClick;
var
  Action: TATTabActionOnClose;
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
      cTabIndexArrowMenu:
        begin
          EndDrag(false);
          FTabIndexOver:= -1;
          Invalidate;
          ShowTabMenu;
        end;

      cTabIndexArrowScrollLeft:
        DoScrollLeft;

      cTabIndexArrowScrollRight:
        DoScrollRight;

      cTabIndexUser0:
        DoClickUser(0);
      cTabIndexUser1:
        DoClickUser(1);
      cTabIndexUser2:
        DoClickUser(2);
      cTabIndexUser3:
        DoClickUser(3);
      cTabIndexUser4:
        DoClickUser(4);

      cTabIndexPlus,
      cTabIndexPlusBtn:
        begin
          EndDrag(false);
          FTabIndexOver:= -1;
          if Assigned(FOnTabPlusClick) then
            FOnTabPlusClick(Self);
        end;

      cTabIndexCloseBtn:
        begin
          Action:= aocDefault;
          if Assigned(FOnTabGetCloseAction) then
            FOnTabGetCloseAction(Self, Action);
          DeleteTab(FTabIndex, true, true, Action);
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
var
  IsX: boolean;
  Data: TATTabData;
begin
  inherited;
  if TabCount=0 then exit;
  FTabIndexOver:= GetTabAt(X, Y, IsX);
  FTabIndexDrop:= FTabIndexOver;

  Data:= GetTabData(FTabIndexOver);
  if Assigned(Data) and (Data.TabHint<>'') and ShowHint then
  begin
    Hint:= Data.TabHint;
    if FTabIndexOver<>FTabIndexHinted then
    begin
      FTabIndexHinted:= FTabIndexOver;
      Application.ActivateHint(Mouse.CursorPos);
    end;
  end
  else
  begin
    FTabIndexHinted:= -1;
    Hint:= '';
    Application.HideHint;
  end;

  if Assigned(Data) then
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
  APopupMenu: TPopupMenu = nil;
  AFontStyle: TFontStyles = [];
  const AHint: TATTabString = '');
var
  Data: TATTabData;
begin
  Data:= TATTabData(FTabList.Add);
  if IsIndexOk(AIndex) then
    Data.Index:= AIndex
  else
    AIndex:= TabCount-1;

  Data.TabCaption:= ACaption;
  Data.TabHint:= AHint;
  Data.TabObject:= AObject;
  Data.TabModified:= AModified;
  Data.TabColor:= AColor;
  Data.TabImageIndex:= AImageIndex;
  Data.TabPopupMenu:= APopupMenu;
  Data.TabFontStyle:= AFontStyle;

  if FOptAnimationEnabled then
    DoAnimationTabAdd(AIndex);

  Invalidate;

  if Assigned(FOnTabMove) then
    FOnTabMove(Self, -1, AIndex);
end;

function TATTabs.DeleteTab(AIndex: integer;
  AAllowEvent, AWithCancelBtn: boolean;
  AAction: TATTabActionOnClose=aocDefault): boolean;
  //
  procedure _ActivateRightTab;
  begin
    if FTabIndex>AIndex then
      SetTabIndex(FTabIndex-1)
    else
    if (FTabIndex=AIndex) and (FTabIndex>0) and (FTabIndex>=TabCount) then
      SetTabIndex(FTabIndex-1)
    else
    if FTabIndex=AIndex then
      SetTabIndex(FTabIndex);
  end;
  //
  procedure _ActivateRecentTab;
  var
    Idx, i: integer;
    Tick, TickMax: Int64;
  begin
    TickMax:= 0;
    Idx:= -1;
    for i:= 0 to TabCount-1 do
    begin
      Tick:= GetTabTick(i);
      if Tick>TickMax then
      begin
        TickMax:= Tick;
        Idx:= i;
      end;
    end;
    if Idx>=0 then
      SetTabIndex(Idx)
    else
      _ActivateRightTab;
  end;
  //
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
    if FOptAnimationEnabled then
      DoAnimationTabClose(AIndex);

    FTabList.Delete(AIndex);

    if AAction=aocDefault then
      AAction:= FOptWhichActivateOnClose;

    case AAction of
      aocRight:
        _ActivateRightTab;
      aocRecent:
        _ActivateRecentTab;
      else
        _ActivateRightTab;
    end;
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
//note: check "if AIndex=FTabIndex" must not be here, must be in outer funcs.
//Sometimes SetTabIndex(TabIndex) is needed, eg in DeleteTab().
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

    MakeVisible(AIndex);
    Invalidate;
    if Assigned(FOnTabClick) then
      FOnTabClick(Self);
  end;
end;


function TATTabs.GetTabData(AIndex: integer): TATTabData;
begin
  if IsIndexOk(AIndex) then
    Result:= TATTabData(FTabList.Items[AIndex])
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
  AColorArr: TColor);
begin
  DrawTriangleType(C, ATyp, ARect, AColorArr, FOptArrowSize div 2);
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
    mi.Caption:= TATTabData(FTabList.Items[i]).TabCaption;
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
  Value, Count: integer;
begin
  if FOptVarWidth then Exit;

  Count:= TabCount;
  if Count=0 then Exit;

  if FOptPosition in [atpLeft, atpRight] then
  begin
    FTabWidth:= ClientWidth-FOptSpacer;
    exit
  end;

  //tricky formula: calculate auto-width
  Value:= (ClientWidth
    - IfThen(FOptShowPlusTab, GetTabWidth_Plus_Raw + 2*FOptSpaceBeforeText)
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
var
  D: TATTabData;
begin
  case FOptShowXButtons of
    atbxShowNone:
      Result:= false;
    atbxShowAll:
      Result:= true;
    atbxShowActive:
      Result:= AIndex=FTabIndex;
    atbxShowMouseOver:
      Result:= AIndex=FTabIndexOver;
    else
      Result:= false;
  end;

  if Result then
  begin
    D:= GetTabData(AIndex);
    if Assigned(D) and D.TabHideXButton then
    begin
      Result:= false;
      Exit
    end;

    if not FOptVarWidth then
      if FOptPosition in [atpTop, atpBottom] then
        if FTabWidth<FOptTabWidthMinimalHidesX then
        begin
          Result:= false;
          Exit
        end;
  end;
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

  FTabList.Items[NFrom].Index:= NTo;
  SetTabIndex(NTo);

  if Assigned(FOnTabMove) then
    FOnTabMove(Self, NFrom, NTo);
end;

procedure TATTabs.MoveTab(AFrom, ATo: integer; AActivateThen: boolean);
begin
  if not IsIndexOk(AFrom) then exit;
  if not IsIndexOk(ATo) then exit;
  if AFrom=ATo then exit;

  FTabList.Items[AFrom].Index:= ATo;
  if AActivateThen then
    SetTabIndex(ATo);
end;

procedure TATTabs.DoTabDropToOtherControl(ATarget: TControl; const APnt: TPoint);
var
  ATabs: TATTabs;
  NTab, NTabTo: integer;
  Data: TATTabData;
  P: TPoint;
  IsX: boolean;
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
  NTabTo:= ATabs.GetTabAt(APnt.X, APnt.Y, IsX); //-1 is allowed

  Data:= GetTabData(NTab);
  if Data=nil then Exit;

  ATabs.AddTab(NTabTo,
    Data.TabCaption,
    Data.TabObject,
    Data.TabModified,
    Data.TabColor,
    Data.TabImageIndex,
    Data.TabPopupMenu,
    Data.TabFontStyle,
    Data.TabHint
    );

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

function TATTabs.GetTabTick(AIndex: integer): Int64;
var
  D: TATTabData;
begin
  Result:= 0;
  if Assigned(FOnTabGetTick) then
  begin
    D:= GetTabData(AIndex);
    if Assigned(D) then
      Result:= FOnTabGetTick(Self, D.TabObject);
  end;
end;

procedure TATTabs.CMMouseLeave(var Msg: TMessage);
begin
  inherited;
  FTabIndexOver:= -1;
  FTabIndexHinted:= -1;
  Invalidate;
end;

procedure TATTabs.SwitchTab(ANext: boolean; ALoopAtEdge: boolean=true);
begin
  if TabCount>1 then
    if ANext then
    begin
      if TabIndex=TabCount-1 then
      begin
        if ALoopAtEdge then
          TabIndex:= 0;
      end
      else
        TabIndex:= TabIndex+1;
    end
    else
    begin
      if TabIndex=0 then
      begin
        if ALoopAtEdge then
          TabIndex:= TabCount-1;
      end
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

function TATTabs.GetMaxEdgePos: integer;
var
  R: TRect;
begin
  Result:= 0;
  if TabCount=0 then exit;

  if FOptShowPlusTab then
    R:= GetTabRect_Plus(false)
  else
    R:= GetTabRect(TabCount-1, false);

  case FOptPosition of
    atpTop,
    atpBottom:
      Result:= R.Right;
    else
      Result:= R.Bottom;
  end;
end;

function TATTabs.GetMaxScrollPos: integer;
var
  NPos: integer;
begin
  Result:= GetMaxEdgePos;
  if Result=0 then exit;

  case FOptPosition of
    atpTop,
    atpBottom:
      Result:= Max(0, Result - ClientWidth + FRealIndentRight);
    else
      Result:= Max(0, Result - ClientHeight);
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
  if FOptMultiline then exit;

  NPos:= Max(0, FScrollPos-GetScrollPageSize);
  if NPos<>FScrollPos then
    DoScrollAnimation(NPos);
end;

procedure TATTabs.DoScrollRight;
var
  NPos: integer;
begin
  if FOptMultiline then exit;

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
  bOver:= FTabIndexOver=cTabIndexPlusBtn;
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
  bOver:= FTabIndexOver=cTabIndexCloseBtn;
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
  bOver:= FTabIndexOver=cTabIndexArrowMenu;
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
          FColorArrow)
        );
      DoPaintAfter(ElemType, -1, C, FRectArrowDown);
    end;
end;

procedure TATTabs.DoPaintArrowLeft(C: TCanvas);
var
  bOver: boolean;
  ElemType: TATTabElemType;
  R: TRect;
begin
  bOver:= FTabIndexOver=cTabIndexArrowScrollLeft;
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
        IfThen(bOver, FColorArrowOver, FColorArrow)
        );
      DoPaintAfter(ElemType, -1, C, FRectArrowLeft);
    end;
end;

procedure TATTabs.DoPaintArrowRight(C: TCanvas);
var
  bOver: boolean;
  ElemType: TATTabElemType;
  R: TRect;
begin
  bOver:= FTabIndexOver=cTabIndexArrowScrollRight;
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
        IfThen(bOver, FColorArrowOver, FColorArrow)
        );
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
      0: begin NIndex:= cTabIndexUser0; R:= FRectButtonUser0; end;
      1: begin NIndex:= cTabIndexUser1; R:= FRectButtonUser1; end;
      2: begin NIndex:= cTabIndexUser2; R:= FRectButtonUser2; end;
      3: begin NIndex:= cTabIndexUser3; R:= FRectButtonUser3; end;
      4: begin NIndex:= cTabIndexUser4; R:= FRectButtonUser4; end;
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
begin
  inherited;
  TabIndex:= FTabIndexLoaded;
end;

function TATTabs.GetButtonsEmpty: boolean;
begin
  Result:=
    (FButtonsLeft[0]=atbNone) and
    (FButtonsRight[0]=atbNone);
end;

function TATTabs.GetInitialVerticalIndent: integer;
begin
  if GetButtonsEmpty then
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
      C.FillRect(Rect(PL1.X+1+Ord(FOptShowFlat), PL1.Y+1, PR1.X, PR1.Y+1+FOptColoredBandSize));
    atpBottom:
      C.FillRect(Rect(PL2.X+1+Ord(FOptShowFlat), PL2.Y-3, PR2.X, PR2.Y-3+FOptColoredBandSize));
    atpLeft:
      C.FillRect(Rect(PL1.X+1, PL1.Y+1, PL1.X+1+FOptColoredBandSize, PL2.Y));
    atpRight:
      C.FillRect(Rect(PR1.X-FOptColoredBandSize, PR1.Y+1, PR1.X, PR2.Y));
  end;

  C.Brush.Color:= NColor;
end;

procedure TATTabs.DoPaintButtonsBG(C: TCanvas);
var
  RL, RR: TRect;
begin
  if FOptPosition in [atpLeft, atpRight] then
    if not GetButtonsEmpty then
    begin
      RL:= GetRectOfButtonIndex(0, true);
      RR:= GetRectOfButtonIndex(0, false);
      DoPaintBgTo(C, Rect(
        RL.Left, 0,
        RR.Right, FOptTabHeight));
    end;
end;

procedure TATTabs.DoUpdateCanvasAntialiasMode(C: TCanvas);
var
  p: TPoint;
begin
  {$ifdef fpc}
  C.AntialiasingMode:= amOn;
  {$else}
  GetBrushOrgEx(C.Handle, p);
  SetStretchBltMode(C.Handle, HALFTONE);
  SetBrushOrgEx(C.Handle, p.x, p.y, @p);
  {$endif}
end;

procedure TATTabs.DoUpdateTabRectsToFillLine(AIndexFrom, AIndexTo: integer; ALastLine: boolean);
var
  NDelta, NWidthOfPlus, i: integer;
  D: TATTabData;
  R: TRect;
begin
  D:= GetTabData(AIndexTo);
  if D=nil then exit;

  if ALastLine and FOptShowPlusTab then
    NWidthOfPlus:= GetTabRectWidth(true)
  else
    NWidthOfPlus:= 0;

  NDelta:=
    (ClientWidth - FRealIndentRight - D.TabRect.Right - NWidthOfPlus)
    div (AIndexTo-AIndexFrom+1);

  for i:= AIndexFrom to AIndexTo do
  begin
    D:= GetTabData(i);
    if D=nil then Continue;
    R:= D.TabRect;
    Inc(R.Left, (i-AIndexFrom)*NDelta);
    Inc(R.Right, (i+1-AIndexFrom)*NDelta);

    //width of last tab is not precise (+-2pixels). fix it.
    if i=AIndexTo then
      R.Right:= ClientWidth - FRealIndentRight - NWidthOfPlus;

    D.TabRect:= R;
  end;
end;

procedure TATTabs.DoUpdateCaptionProps(C: TCanvas; const ACaption: TATTabString;
  out ALineHeight: integer; out ATextSize: TSize);
var
  Ex: TSize;
  StrW: WideString;
  i: integer;
begin
  ALineHeight:= 0;
  ATextSize.cx:= 0;
  ATextSize.cy:= 0;
  FCaptionList.Text:=
    {$ifdef WIDE}UTF8Encode{$endif}
    (ACaption);

  for i:= 0 to FCaptionList.Count-1 do
  begin
    {$ifdef WIDE}
    StrW:= UTF8Decode(FCaptionList[i]);
    Windows.GetTextExtentPoint32W(C.Handle, PWideChar(StrW), Length(StrW), Ex);
    {$else}
    Ex:= C.TextExtent(FCaptionList[i]);
    {$endif}

    Inc(ATextSize.CY, Ex.CY);
    ALineHeight:= Max(ALineHeight, Ex.CY);
    ATextSize.CX:= Max(ATextSize.CX, Ex.CX);
  end;
end;

function TATTabs.IsTabVisible(AIndex: integer): boolean;
var
  D: TATTabData;
  R: TRect;
begin
  if not IsScrollMarkNeeded then
    begin Result:= true; exit end;

  D:= GetTabData(AIndex);
  if D=nil then
    begin Result:= false; exit end;
  R:= D.TabRect;

  case FOptPosition of
    atpTop,
    atpBottom:
      Result:=
        (R.Left-FScrollPos >= FRealIndentLeft) and
        (R.Right-FScrollPos < ClientWidth-FRealIndentRight);
    else
      Result:=
        (R.Top-FScrollPos >= FRealIndentLeft) and
        (R.Bottom-FScrollPos < ClientHeight-FRealIndentRight);
  end;
end;

procedure TATTabs.MakeVisible(AIndex: integer);
var
  D: TATTabData;
  R: TRect;
begin
  //sometimes new tab has not updated Data.TabRect
  DoUpdateTabRects(FBitmap.Canvas);

  if not IsScrollMarkNeeded then exit;

  if IsTabVisible(AIndex) then exit;

  D:= GetTabData(AIndex);
  if D=nil then exit;
  R:= D.TabRect;

  case FOptPosition of
    atpTop,
    atpBottom:
      begin
        FScrollPos:= Min(GetMaxScrollPos, Max(0,
          R.Left - ClientWidth div 2
          ));
      end
    else
      begin
        FScrollPos:= Min(GetMaxScrollPos, Max(0,
          R.Top - ClientHeight div 2
          ));
      end;
  end;

  Invalidate;
end;

procedure TATTabs.SetScrollPos(AValue: integer);
begin
  //user suggested to not limit ScrollPos
  //AValue:= Max(0, Min(GetMaxScrollPos, AValue) );
  if FScrollPos=AValue then exit;
  FScrollPos:= AValue;
  Invalidate;
end;


procedure TATTabs.DoAnimationTabClose(AIndex: integer);
var
  Data: TATTabData;
  i: integer;
begin
  Data:= GetTabData(AIndex);
  if Data=nil then exit;

  Enabled:= false;
  FTabIndexAnimated:= AIndex;

  case FOptPosition of
    atpTop,
    atpBottom:
      begin
        for i:= 0 to FOptTabHeight div FOptAnimationStepV-1 do
        begin
          FAnimationOffset:= i*FOptAnimationStepV;
          Invalidate;
          Application.ProcessMessages;
          Sleep(FOptAnimationPause);
        end;
      end;
    else
      begin
        for i:= 0 to FOptTabWidthNormal div FOptAnimationStepH-1 do
        begin
          FAnimationOffset:= i*FOptAnimationStepH;
          Invalidate;
          Application.ProcessMessages;
          Sleep(FOptAnimationPause);
        end;
      end;
  end;

  FTabIndexAnimated:= -1;
  Enabled:= true;
end;


procedure TATTabs.DoAnimationTabAdd(AIndex: integer);
var
  Data: TATTabData;
  i: integer;
begin
  Data:= GetTabData(AIndex);
  if Data=nil then exit;

  Enabled:= false;
  FTabIndexAnimated:= AIndex;

  case FOptPosition of
    atpTop,
    atpBottom:
      begin
        for i:= FOptTabHeight div FOptAnimationStepV-1 downto 0 do
        begin
          FAnimationOffset:= i*FOptAnimationStepV;
          Invalidate;
          Application.ProcessMessages;
          Sleep(FOptAnimationPause);
        end;
      end;
    else
      begin
        for i:= FOptTabWidthNormal div FOptAnimationStepH-1 downto 0 do
        begin
          FAnimationOffset:= i*FOptAnimationStepH;
          Invalidate;
          Application.ProcessMessages;
          Sleep(FOptAnimationPause);
        end;
      end;
  end;

  FTabIndexAnimated:= -1;
  Enabled:= true;
end;


end.

