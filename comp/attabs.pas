{
ATTabs component for Delphi/Lazarus
Copyright (c) Alexey Torgashin (UVviewsoft.com)
License: MPL 2.0 or LGPL
}

//{$define tabs_paint_counter}

unit attabs;

{$ifdef FPC}
  {$mode delphi}
{$else}
  {$define windows}
  {$ifdef VER150} //Delphi 7
    {$define WIDE}
  {$endif}
{$endif}

interface

uses
  {$ifdef windows}
  Windows,
  {$endif}
  Classes, Types, Graphics,
  Controls, Messages, ImgList,
  {$ifdef FPC}
  InterfaceBase,
  LCLIntf,
  LCLType,
  LCLProc,
  {$else}
  System.UITypes,
  {$endif}
  ATTabs_Picture,
  Menus;

type
  TATTabString = {$ifdef WIDE} WideString {$else} string {$endif};

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
  TATTabListCollection = class(TCollection)
  public
    AOwner: TCustomControl;
  end;

type
  { TATTabData }

  TATTabData = class(TCollectionItem)
  private
    FTabCaption: TATTabString;
    FTabHint: TATTabString;
    FTabObject: TObject;
    FTabColor: TColor;
    FTabColorActive: TColor;
    FTabColorOver: TColor;
    FTabModified: boolean;
    FTabSpecial: boolean;
    FTabSpecialWidth: integer;
    FTabSpecialHeight: integer;
    FTabRect: TRect;
    FTabImageIndex: TImageIndex;
    FTabPopupMenu: TPopupMenu;
    FTabFontStyle: TFontStyles;
    FTabStartsNewLine: boolean;
    FTabHideXButton: boolean;
    procedure UpdateTabSet;
    procedure SetTabImageIndex(const Value: TImageIndex);
    procedure SetTabCaption(const Value: TATTabString);
    procedure SetTabColor(const Value: TColor);
    procedure SetTabColorActive(const Value: TColor);
    procedure SetTabColorOver(const Value: TColor);
    procedure SetTabHideXButton(const Value: boolean);
  public
    constructor Create(ACollection: TCollection); override;
    property TabObject: TObject read FTabObject write FTabObject;
    property TabRect: TRect read FTabRect write FTabRect;
    property TabSpecial: boolean read FTabSpecial write FTabSpecial default false;
    property TabStartsNewLine: boolean read FTabStartsNewLine write FTabStartsNewLine;
  published
    property TabCaption: TATTabString read FTabCaption write SetTabCaption;
    property TabHint: TATTabString read FTabHint write FTabHint;
    property TabColor: TColor read FTabColor write SetTabColor default clNone;
    property TabColorActive: TColor read FTabColorActive write SetTabColorActive default clNone;
    property TabColorOver: TColor read FTabColorOver write SetTabColorOver default clNone;
    property TabModified: boolean read FTabModified write FTabModified default false;
    property TabImageIndex: TImageIndex read FTabImageIndex write SetTabImageIndex default -1;
    property TabFontStyle: TFontStyles read FTabFontStyle write FTabFontStyle default [];
    property TabPopupMenu: TPopupMenu read FTabPopupMenu write FTabPopupMenu;
    property TabSpecialWidth: integer read FTabSpecialWidth write FTabSpecialWidth default 0;
    property TabSpecialHeight: integer read FTabSpecialHeight write FTabSpecialHeight default 0;
    property TabHideXButton: boolean read FTabHideXButton write SetTabHideXButton default false;
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
    atbPlus,
    atbClose,
    atbScrollLeft,
    atbScrollRight,
    atbDropdownMenu,
    atbSpace,
    atbSeparator,
    atbUser0,
    atbUser1,
    atbUser2,
    atbUser3,
    atbUser4
    );

  TATTabButtons = array of record
    Id: TATTabButton;
    Size: integer;
  end;

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
  TATTabMoveEvent = procedure (Sender: TObject; AIndexFrom, AIndexTo: integer) of object;
  TATTabChangeQueryEvent = procedure (Sender: TObject; ANewTabIndex: integer;
    var ACanChange: boolean) of object;
  TATTabClickUserButton = procedure (Sender: TObject; AIndex: integer) of object;
  TATTabGetTickEvent = function (Sender: TObject; ATabObject: TObject): Int64 of object;
  TATTabGetCloseActionEvent = procedure (Sender: TObject; var AAction: TATTabActionOnClose) of object;
  TATTabDblClickEvent = procedure (Sender: TObject; AIndex: integer) of object;
  TATTabDropQueryEvent = procedure (Sender: TObject; AIndexFrom, AIndexTo: integer; var ACanDrop: boolean) of object;

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
    atbxShowMouseOver,
    atbxShowActiveAndMouseOver
    );

  TATTabTheme = record
    FileName_Left: string;
    FileName_Right: string;
    FileName_Center: string;
    FileName_LeftActive: string;
    FileName_RightActive: string;
    FileName_CenterActive: string;
    FileName_X: string;
    FileName_XActive: string;
    FileName_Plus: string;
    FileName_PlusActive: string;
    FileName_ArrowLeft: string;
    FileName_ArrowLeftActive: string;
    FileName_ArrowRight: string;
    FileName_ArrowRightActive: string;
    FileName_ArrowDown: string;
    FileName_ArrowDownActive: string;
    SpaceBetweenInPercentsOfSide: integer;
    IndentOfX: integer;
  end;

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
  _InitOptButtonSizeSpace = 10;
  _InitOptButtonSizeSeparator = 5;
  _InitOptTabHeight = 24;
  _InitOptTabWidthMinimal = 40;
  _InitOptTabWidthMaximal = 300;
  _InitOptTabWidthNormal = 130;
  _InitOptTabWidthMinimalHidesX = 55;
  _InitOptSpaceSide = 10;
  _InitOptSpaceInitial = 5;
  _InitOptSpaceBeforeText = 6;
  _InitOptSpaceBeforeTextForMinWidth = 30;
  _InitOptSpaceBetweenTabs = 0;
  _InitOptSpaceBetweenLines = 4;
  _InitOptSpaceBetweenIconCaption = 0;
  _InitOptSpaceSeparator = 2;
  _InitOptSpacer = 4;
  _InitOptSpacer2 = 10;
  _InitOptSpaceXRight = 10;
  _InitOptSpaceXInner = 3;
  _InitOptSpaceXSize = 12;
  _InitOptSpaceXIncrementRound = 1;
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
  _InitOptShowFlatMouseOver = true;
  _InitOptShowFlatSep = true;
  _InitOptPosition = atpTop;
  _InitOptFillWidth = true;
  _InitOptFillWidthLastToo = false;
  _InitOptShowNumberPrefix = '';
  _InitOptShowScrollMark = true;
  _InitOptShowDropMark = true;
  _InitOptShowArrowsNear = true;
  _InitOptShowXRounded = true;
  _InitOptShowXButtons = atbxShowAll;
  _InitOptShowPlusTab = true;
  _InitOptShowModifiedText = '*';
  _InitOptShowEntireColor = false;
  _InitOptShowActiveMarkInverted = true;
  _InitRoundedBitmapSize = 60;

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
    FMouseDragBegins: boolean;

    //colors
    FColorBg: TColor; //color of background (visible at top and between tabs)
    FColorBorderActive: TColor; //color of 1px border of active tab
    FColorBorderPassive: TColor; //color of 1px border of inactive tabs
    FColorSeparator: TColor; //vertical lines between tabs in Flat mode
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
    FOptButtonSizeSpace: integer;
    FOptButtonSizeSeparator: integer;
    FOptButtonLayout: string;

    FOptAnimationEnabled: boolean;
    FOptAnimationStepV: integer;
    FOptAnimationStepH: integer;
    FOptAnimationPause: integer;

    FOptScalePercents: integer;
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
    FOptSpaceBeforeTextForMinWidth: integer;
    FOptSpaceSeparator: integer;
    FOptSpacer: integer; //height of top empty space (colored with bg)
    FOptSpacer2: integer;
    FOptSpaceXRight: integer; //space from "x" btn to right tab edge
    FOptSpaceXInner: integer; //space from "x" square edge to "x" mark
    FOptSpaceXSize: integer; //size of "x" mark
    FOptSpaceXIncrementRound: integer;
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
    FOptActiveVisibleOnResize: boolean;

    FOptPosition: TATTabPosition;
    FOptIconPosition: TATTabIconPosition;
    FOptWhichActivateOnClose: TATTabActionOnClose;
    FOptCaptionAlignment: TAlignment;
    FOptShowFlat: boolean;
    FOptShowFlatMouseOver: boolean;
    FOptShowFlatSepar: boolean;
    FOptShowXRounded: boolean;
    FOptShowXButtons: TATTabShowClose; //show mode for "x" buttons
    FOptShowArrowsNear: boolean;
    FOptShowPlusTab: boolean; //show "plus" tab
    FOptShowModifiedText: TATTabString;
    FOptShowEntireColor: boolean;
    FOptShowNumberPrefix: TATTabString;
    FOptShowScrollMark: boolean;
    FOptShowDropMark: boolean;
    FOptShowActiveMarkInverted: boolean;
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
    FTabIndexHintedPrev: integer;
    FTabIndexAnimated: integer;
    FTabList: TATTabListCollection;
    FTabMenu: TPopupMenu;
    FCaptionList: TStringList;
    FMultilineActive: boolean;

    FRealIndentLeft: integer;
    FRealIndentRight: integer;
    FOptSpaceSide: integer;
    FAnimationOffset: integer;
    FPaintCount: integer;
    FLastOverIndex: integer;
    FLastOverX: boolean;

    FScrollPos: integer;
    FImages: TImageList;
    FBitmap: TBitmap;
    FBitmapAngle: TBitmap;
    FBitmapRound: TBitmap;

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

    FHintForX: string;
    FHintForPlus: string;
    FHintForArrowLeft: string;
    FHintForArrowRight: string;
    FHintForArrowMenu: string;
    FHintForUser0: string;
    FHintForUser1: string;
    FHintForUser2: string;
    FHintForUser3: string;
    FHintForUser4: string;

    FThemed: boolean;
    FPic_Side_L: TATTabsPicture;
    FPic_Side_L_a: TATTabsPicture;
    FPic_Side_R: TATTabsPicture;
    FPic_Side_R_a: TATTabsPicture;
    FPic_Side_C: TATTabsPicture;
    FPic_Side_C_a: TATTabsPicture;
    FPic_X: TATTabsPicture;
    FPic_X_a: TATTabsPicture;
    FPic_Plus: TATTabsPicture;
    FPic_Plus_a: TATTabsPicture;
    FPic_Arrow_L: TATTabsPicture;
    FPic_Arrow_L_a: TATTabsPicture;
    FPic_Arrow_R: TATTabsPicture;
    FPic_Arrow_R_a: TATTabsPicture;
    FPic_Arrow_D: TATTabsPicture;
    FPic_Arrow_D_a: TATTabsPicture;

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
    FOnTabDblClick: TATTabDblClickEvent;
    FOnTabDropQuery: TATTabDropQueryEvent;

    function ConvertButtonIdToTabIndex(Id: TATTabButton): integer; inline;
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
    procedure DoPaintColoredBand(C: TCanvas; const ARect: TRect; AColor: TColor;
      APos: TATTabPosition);
    procedure DoPaintPlus(C: TCanvas; const ARect: TRect);
    procedure DoPaintSeparator(C: TCanvas; const R: TRect);
    procedure DoPaintTabShape(C: TCanvas; const ATabRect: TRect;
      ATabActive: boolean; ATabIndex: integer);
    procedure DoPaintTabShape_C(C: TCanvas; ATabActive: boolean;
      ATabIndex: integer; const ARect: TRect; const PL1, PL2, PR1, PR2: TPoint);
    procedure DoPaintTabShape_L(C: TCanvas; const ARect: TRect;
      ATabActive: boolean; ATabIndex: integer);
    procedure DoPaintTabShape_R(C: TCanvas; const ARect: TRect;
      ATabActive: boolean; ATabIndex: integer);
    procedure DoPaintTo(C: TCanvas);
    procedure DoPaintX(C: TCanvas; const ARectX: TRect; ATabIndex: integer;
      ATabActive, AMouseOverX: boolean);
    procedure DoTextOut(C: TCanvas; AX, AY: integer; const AClipRect: TRect; const AText: string); inline;
    procedure DoPaintBgTo(C: TCanvas; const ARect: TRect);
    procedure DoPaintTabTo(C: TCanvas; const ARect: TRect;
      const ACaption: TATTabString; ATabIndex: integer; AColorFont: TColor;
  ATabActive, ATabMouseOver, ATabMouseOverX: boolean; AFontStyle: TFontStyles);
    procedure DoPaintArrowTo(C: TCanvas; ATyp: TATTabTriangle; ARect: TRect; AActive: boolean);
    procedure DoPaintUserButtons(C: TCanvas; const AButtons: TATTabButtons; AtLeft: boolean);
    procedure DoPaintXTo(C: TCanvas; const R: TRect; ATabIndex: integer;
      ATabActive, AMouseOverX: boolean);
    procedure DoPaintDropMark(C: TCanvas);
    procedure DoPaintScrollMark(C: TCanvas);
    function GetButtonsEdgeCoord(AtLeft: boolean): integer;
    function GetButtonsWidth(const B: TATTabButtons): integer;
    function GetPositionInverted(APos: TATTabPosition): TATTabPosition; inline;
    function GetIndexOfButton(const AButtons: TATTabButtons; ABtn: TATTabButton): integer;
    function GetInitialVerticalIndent: integer; inline;
    function GetButtonsEmpty: boolean; inline;
    function GetTabBgColor_Passive(AIndex: integer): TColor;
    function GetTabBgColor_Active(AIndex: integer): TColor;
    function GetTabFlatEffective(AIndex: integer): boolean; inline;
    procedure GetTabXColors(AIndex: integer; AMouseOverX: boolean; out AColorXBg,
      AColorXBorder, AColorXMark: TColor);
    function IsScrollMarkNeeded: boolean;
    function GetMaxEdgePos: integer;
    function GetRectOfButton(AButton: TATTabButton): TRect;
    function GetRectOfButtonIndex(AIndex: integer; AtLeft: boolean): TRect;
    function GetScrollPageSize: integer;
    procedure SetOptButtonLayout(const AValue: string);
    procedure SetOptMouseDragEnabled(AValue: boolean);
    procedure SetOptScalePercents(AValue: integer);
    procedure SetOptVarWidth(AValue: boolean);
    procedure SetScrollPos(AValue: integer);
    procedure SetTabIndex(AIndex: integer);
    procedure GetTabXProps(AIndex: integer; const ARect: TRect; out
      AMouseOverX: boolean; out ARectX: TRect);
    function IsIndexOk(AIndex: integer): boolean; inline;
    function IsShowX(AIndex: integer): boolean;
    function IsPaintNeeded(AElemType: TATTabElemType;
      AIndex: integer; ACanvas: TCanvas; const ARect: TRect): boolean;
    function DoPaintAfter(AElemType: TATTabElemType;
      AIndex: integer; ACanvas: TCanvas; const ARect: TRect): boolean;
    procedure TabMenuClick(Sender: TObject);
    function GetTabWidth_Plus_Raw: integer; inline;
    procedure UpdateTabWidths;
    procedure UpdateTabRects(C: TCanvas);
    procedure UpdateTabRectsToFillLine(AIndexFrom, AIndexTo: integer; ALastLine: boolean);
    procedure UpdateCanvasAntialiasMode(C: TCanvas); inline;
    procedure UpdateCaptionProps(C: TCanvas; const ACaption: TATTabString;
      out ALineHeight: integer; out ATextSize: TSize);
    procedure DoTabDrop;
    procedure DoTabDropToOtherControl(ATarget: TControl; const APnt: TPoint);
    function GetTabTick(AIndex: integer): Int64;
    function _IsDrag: boolean; inline;
    procedure SetOptShowPlusTab(const Value: boolean);

  public
    TabMenuExternal: TPopupMenu;

    constructor Create(AOwner: TComponent); override;
    function CanFocus: boolean; override;
    destructor Destroy; override;
    procedure DragDrop(Source: TObject; X, Y: integer); override;

    procedure ApplyButtonLayout;
    function GetTabRectWidth(APlusBtn: boolean): integer;
    function GetTabRect(AIndex: integer; AWithScroll: boolean=true;
      AWithAnimation: boolean=true): TRect;
    function GetTabRect_Plus(AWithScroll: boolean= true): TRect;
    function GetTabRect_X(const ARect: TRect): TRect;
    function GetTabAt(X, Y: integer; out APressedX: boolean): integer;
    function GetTabData(AIndex: integer): TATTabData;
    function TabCount: integer;
    function AddTab(
      AIndex: integer;
      const ACaption: TATTabString;
      AObject: TObject = nil;
      AModified: boolean = false;
      AColor: TColor = clNone;
      AImageIndex: TImageIndex = -1;
      APopupMenu: TPopupMenu = nil;
      AFontStyle: TFontStyles = [];
      const AHint: TATTabString = ''): TATTabData;
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
    procedure SetTheme(const Data: TATTabTheme);
    property IsThemed: boolean read FThemed write FThemed;
    function DoScale(AValue: integer): integer; inline;

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
    procedure DoContextPopup(MousePos: TPoint; var Handled: Boolean); override;

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
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Visible;
    //property Tabs: TCollection read FTabList write FTabList;
    property Tabs: TATTabListCollection read FTabList write FTabList;

    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnContextPopup;
    //these 2 lines don't compile under Delphi 7
    {$ifndef VER150}
    property OnMouseEnter;
    property OnMouseLeave;
    {$endif}
    //
    property OnMouseMove;
    property OnMouseDown;
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
    property ColorSeparator: TColor read FColorSeparator write FColorSeparator default _InitTabColorArrow;
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
    property OptButtonSizeSpace: integer read FOptButtonSizeSpace write FOptButtonSizeSpace default _InitOptButtonSizeSpace;
    property OptButtonSizeSeparator: integer read FOptButtonSizeSeparator write FOptButtonSizeSeparator default _InitOptButtonSizeSeparator;
    property OptScalePercents: integer read FOptScalePercents write SetOptScalePercents default 100;
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
    property OptSpaceSide: integer read FOptSpaceSide write FOptSpaceSide default _InitOptSpaceSide;
    property OptSpaceBetweenTabs: integer read FOptSpaceBetweenTabs write FOptSpaceBetweenTabs default _InitOptSpaceBetweenTabs;
    property OptSpaceBetweenLines: integer read FOptSpaceBetweenLines write FOptSpaceBetweenLines default _InitOptSpaceBetweenLines;
    property OptSpaceBetweenIconCaption: integer read FOptSpaceBetweenIconCaption write FOptSpaceBetweenIconCaption default _InitOptSpaceBetweenIconCaption;
    property OptSpaceInitial: integer read FOptSpaceInitial write FOptSpaceInitial default _InitOptSpaceInitial;
    property OptSpaceBeforeText: integer read FOptSpaceBeforeText write FOptSpaceBeforeText default _InitOptSpaceBeforeText;
    property OptSpaceBeforeTextForMinWidth: integer read FOptSpaceBeforeTextForMinWidth write FOptSpaceBeforeTextForMinWidth default _InitOptSpaceBeforeTextForMinWidth;
    property OptSpaceSeparator: integer read FOptSpaceSeparator write FOptSpaceSeparator default _InitOptSpaceSeparator;
    property OptSpacer: integer read FOptSpacer write FOptSpacer default _InitOptSpacer;
    property OptSpacer2: integer read FOptSpacer2 write FOptSpacer2 default _InitOptSpacer2;
    property OptSpaceXRight: integer read FOptSpaceXRight write FOptSpaceXRight default _InitOptSpaceXRight;
    property OptSpaceXInner: integer read FOptSpaceXInner write FOptSpaceXInner default _InitOptSpaceXInner;
    property OptSpaceXSize: integer read FOptSpaceXSize write FOptSpaceXSize default _InitOptSpaceXSize;
    property OptSpaceXIncrementRound: integer read FOptSpaceXIncrementRound write FOptSpaceXIncrementRound default _InitOptSpaceXIncrementRound;
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
    property OptActiveVisibleOnResize: boolean read FOptActiveVisibleOnResize write FOptActiveVisibleOnResize default true;

    property OptPosition: TATTabPosition read FOptPosition write FOptPosition default _InitOptPosition;
    property OptIconPosition: TATTabIconPosition read FOptIconPosition write FOptIconPosition default aipIconLefterThanText;
    property OptWhichActivateOnClose: TATTabActionOnClose read FOptWhichActivateOnClose write FOptWhichActivateOnClose default aocRight;
    property OptCaptionAlignment: TAlignment read FOptCaptionAlignment write FOptCaptionAlignment default taLeftJustify;
    property OptShowFlat: boolean read FOptShowFlat write FOptShowFlat default _InitOptShowFlat;
    property OptShowFlatMouseOver: boolean read FOptShowFlatMouseOver write FOptShowFlatMouseOver default _InitOptShowFlatMouseOver;
    property OptShowFlatSepar: boolean read FOptShowFlatSepar write FOptShowFlatSepar default _InitOptShowFlatSep;
    property OptShowScrollMark: boolean read FOptShowScrollMark write FOptShowScrollMark default _InitOptShowScrollMark;
    property OptShowDropMark: boolean read FOptShowDropMark write FOptShowDropMark default _InitOptShowDropMark;
    property OptShowXRounded: boolean read FOptShowXRounded write FOptShowXRounded default _InitOptShowXRounded;
    property OptShowXButtons: TATTabShowClose read FOptShowXButtons write FOptShowXButtons default _InitOptShowXButtons;
    property OptShowPlusTab: boolean read FOptShowPlusTab write SetOptShowPlusTab default _InitOptShowPlusTab;
    property OptShowArrowsNear: boolean read FOptShowArrowsNear write FOptShowArrowsNear default _InitOptShowArrowsNear;
    property OptShowModifiedText: TATTabString read FOptShowModifiedText write FOptShowModifiedText;
    property OptShowEntireColor: boolean read FOptShowEntireColor write FOptShowEntireColor default _InitOptShowEntireColor;
    property OptShowNumberPrefix: TATTabString read FOptShowNumberPrefix write FOptShowNumberPrefix;
    property OptShowActiveMarkInverted: boolean read FOptShowActiveMarkInverted write FOptShowActiveMarkInverted default _InitOptShowActiveMarkInverted;
    property OptActiveFontStyle: TFontStyles read FOptActiveFontStyle write FOptActiveFontStyle default _InitOptActiveFontStyle;
    property OptActiveFontStyleUsed: boolean read FOptActiveFontStyleUsed write FOptActiveFontStyleUsed default _InitOptActiveFontStyleUsed;
    property OptHotFontStyle: TFontStyles read FOptHotFontStyle write FOptHotFontStyle default _InitOptHotFontStyle;
    property OptHotFontStyleUsed: boolean read FOptHotFontStyleUsed write FOptHotFontStyleUsed default _InitOptHotFontStyleUsed;

    property OptMouseMiddleClickClose: boolean read FOptMouseMiddleClickClose write FOptMouseMiddleClickClose default _InitOptMouseMiddleClickClose;
    property OptMouseDoubleClickClose: boolean read FOptMouseDoubleClickClose write FOptMouseDoubleClickClose default _InitOptMouseDoubleClickClose;
    property OptMouseDoubleClickPlus: boolean read FOptMouseDoubleClickPlus write FOptMouseDoubleClickPlus default _InitOptMouseDoubleClickPlus;
    property OptMouseDragEnabled: boolean read FOptMouseDragEnabled write SetOptMouseDragEnabled default _InitOptMouseDragEnabled;
    property OptMouseDragOutEnabled: boolean read FOptMouseDragOutEnabled write FOptMouseDragOutEnabled default _InitOptMouseDragOutEnabled;

    property OptHintForX: string read FHintForX write FHintForX;
    property OptHintForPlus: string read FHintForPlus write FHintForPlus;
    property OptHintForArrowLeft: string read FHintForArrowLeft write FHintForArrowLeft;
    property OptHintForArrowRight: string read FHintForArrowRight write FHintForArrowRight;
    property OptHintForArrowMenu: string read FHintForArrowMenu write FHintForArrowMenu;
    property OptHintForUser0: string read FHintForUser0 write FHintForUser0;
    property OptHintForUser1: string read FHintForUser1 write FHintForUser1;
    property OptHintForUser2: string read FHintForUser2 write FHintForUser2;
    property OptHintForUser3: string read FHintForUser3 write FHintForUser3;
    property OptHintForUser4: string read FHintForUser4 write FHintForUser4;

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
    property OnTabDblClick: TATTabDblClickEvent read FOnTabDblClick write FOnTabDblClick;
    property OnTabDropQuery: TATTabDropQueryEvent read FOnTabDropQuery write FOnTabDropQuery;
  end;

var
  cTabsMouseMinDistanceToDrag: integer = 10; //mouse must move >=N pixels to start drag-drop
  cTabsMouseMaxDistanceToClick: integer = 4; //if mouse moves during mouse-down >=N pixels, dont click

  function _ShortenStringEx(C: TCanvas;
    const Text: string;
    Mode: TATTabTruncateCaption;
    Width: integer;
    DotsString: string=''): string;

implementation

uses
  SysUtils,
  StrUtils,
  Dialogs,
  Forms,
  Math;

const
  cSmoothScale = 5;

procedure AddTabButton(var Buttons: TATTabButtons; Id: TATTabButton; Size: integer);
begin
  SetLength(Buttons, Length(Buttons)+1);
  Buttons[Length(Buttons)-1].Id:= Id;
  Buttons[Length(Buttons)-1].Size:= Size;
end;

procedure TATTabData.UpdateTabSet;
begin
  if Collection is TATTabListCollection then
    if TATTabListCollection(Collection).AOwner is TATTabs then
      TATTabListCollection(Collection).AOwner.Invalidate;
end;

procedure TATTabData.SetTabImageIndex(const Value: TImageIndex);
begin
  if FTabImageIndex <> Value then
  begin
  FTabImageIndex := Value;
  UpdateTabSet;
  end;
end;

procedure TATTabData.SetTabCaption(const Value: TATTabString);
begin
  FTabCaption := Value;
  UpdateTabSet;
end;

procedure TATTabData.SetTabColor(const Value: TColor);
begin
  FTabColor := Value;
  UpdateTabSet;
end;

procedure TATTabData.SetTabColorActive(const Value: TColor);
begin
  FTabColorActive := Value;
  UpdateTabSet;
end;

procedure TATTabData.SetTabColorOver(const Value: TColor);
begin
  FTabColorOver := Value;
  UpdateTabSet;
end;

procedure TATTabData.SetTabHideXButton(const Value: boolean);
begin
  FTabHideXButton := Value;
  UpdateTabSet;
end;

procedure TATTabs.SetOptShowPlusTab(const Value: boolean);
begin
  FOptShowPlusTab := Value;
  Invalidate;
end;

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


procedure CanvasStretchDraw(C: TCanvas; const R: TRect; Bmp: TBitmap); {$ifdef fpc}inline;{$endif}
begin
  {$ifdef fpc}
  C.StretchDraw(R, Bmp);
  {$else}
  //Delphi: StretchDraw cannot draw smooth
  StretchBlt(
    C.Handle, R.Left, R.Top, R.Right-R.Left, R.Bottom-R.Top,
    Bmp.Canvas.Handle, 0, 0, Bmp.Width, Bmp.Height,
    C.CopyMode);
  {$endif}
end;

procedure BitmapSetSize(b: TBitmap; W, H: integer); {$ifdef fpc}inline;{$endif}
begin
  {$ifdef fpc}
  b.SetSize(W, H);
  {$else}
  b.Width:= W;
  b.Height:= H;
  {$endif}
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
  AColorFill, AColorLine: TColor;
  b: TBitmap);
var
  p0, p1, p2, p3: TPoint;
  line1, line2: TPoint;
  ar: array[0..2] of TPoint;
begin
    BitmapSetSize(b, ASizeX*AScale, ASizeY*AScale);

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

    CanvasStretchDraw(C, Rect(AX, AY, AX+ASizeX, AY+ASizeY), b);
end;

function _ShortenStringEx(C: TCanvas;
  const Text: string;
  Mode: TATTabTruncateCaption;
  Width: integer;
  DotsString: string=''): string;
const
  cMinLen = 3;
var
  S, STemp: WideString;
  N, i: integer;
begin
  if (Mode=attcNone) or
    (C.TextWidth(Text)<=Width) then
  begin
    Result:= Text;
    exit
  end;

  if DotsString='' then
    DotsString:= {$ifdef fpc}UTF8Encode{$endif}(#$2026);

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
  TabColorActive:= clNone;
  TabColorOver:= clNone;
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

constructor TATTabs.Create(AOwner: TComponent);
begin
  inherited;

  Caption:= '';
  ControlStyle:= ControlStyle+[csOpaque];
  DoubleBuffered:= IsDoubleBufferedNeeded;
  DragMode:= dmManual; //required Manual

  //http://delphidabbler.com/tips/76
  if (csDesigning in ComponentState) and not
    (csReading in AOwner.ComponentState) then
    {this is true if the component is dropped on the form}
      ParentColor:= false; //defaults to false only at first creation

  Width:= 400;
  Height:= 35;

  FMouseDown:= false;
  FMouseDownPnt:= Point(0, 0);
  FMouseDownDbl:= false;
  FMouseDownRightBtn:= false;

  FPaintCount:= 0;
  FLastOverIndex:= -100;
  FLastOverX:= false;

  FColorBg:= _InitTabColorBg;
  FColorSeparator:= _InitTabColorArrow;
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

  FOptScalePercents:= 100;
  FOptButtonSize:= _InitOptButtonSize;
  FOptButtonSizeSpace:= _InitOptButtonSizeSpace;
  FOptButtonSizeSeparator:= _InitOptButtonSizeSeparator;

  FOptButtonLayout:= _InitOptButtonLayout;
  ApplyButtonLayout;

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
  FOptSpaceSide:= _InitOptSpaceSide;
  FOptSpaceInitial:= _InitOptSpaceInitial;
  FOptSpaceBeforeText:= _InitOptSpaceBeforeText;
  FOptSpaceBeforeTextForMinWidth:= _InitOptSpaceBeforeTextForMinWidth;
  FOptSpaceBetweenTabs:= _InitOptSpaceBetweenTabs;
  FOptSpaceBetweenLines:= _InitOptSpaceBetweenLines;
  FOptSpaceBetweenIconCaption:= _InitOptSpaceBetweenIconCaption;
  FOptSpaceSeparator:= _InitOptSpaceSeparator;
  FOptSpacer:= _InitOptSpacer;
  FOptSpacer2:= _InitOptSpacer2;
  FOptSpaceXRight:= _InitOptSpaceXRight;
  FOptSpaceXInner:= _InitOptSpaceXInner;
  FOptSpaceXSize:= _InitOptSpaceXSize;
  FOptSpaceXIncrementRound:= _InitOptSpaceXIncrementRound;
  FOptArrowSize:= _InitOptArrowSize;
  FOptColoredBandSize:= _InitOptColoredBandSize;
  FOptColoredBandForTop:= atpTop;
  FOptColoredBandForBottom:= atpBottom;
  FOptColoredBandForLeft:= atpLeft;
  FOptColoredBandForRight:= atpRight;
  FOptActiveMarkSize:= _InitOptActiveMarkSize;
  FOptScrollMarkSizeX:= _InitOptScrollMarkSizeX;
  FOptScrollMarkSizeY:= _InitOptScrollMarkSizeY;
  FOptActiveVisibleOnResize:= true;
  FOptDropMarkSize:= _InitOptDropMarkSize;
  FOptActiveFontStyle:= _InitOptActiveFontStyle;
  FOptActiveFontStyleUsed:= _InitOptActiveFontStyleUsed;
  FOptHotFontStyle:= _InitOptHotFontStyle;
  FOptHotFontStyleUsed:= _InitOptHotFontStyleUsed;

  FOptShowFlat:= _InitOptShowFlat;
  FOptShowFlatMouseOver:= _InitOptShowFlatMouseOver;
  FOptShowFlatSepar:= _InitOptShowFlatSep;
  FOptPosition:= _InitOptPosition;
  FOptShowNumberPrefix:= _InitOptShowNumberPrefix;
  FOptShowScrollMark:= _InitOptShowScrollMark;
  FOptShowDropMark:= _InitOptShowDropMark;
  FOptShowXRounded:= _InitOptShowXRounded;
  FOptShowXButtons:= _InitOptShowXButtons;
  FOptShowPlusTab:= _InitOptShowPlusTab;
  FOptShowArrowsNear:= _InitOptShowArrowsNear;
  FOptShowModifiedText:= _InitOptShowModifiedText;
  FOptShowEntireColor:= _InitOptShowEntireColor;
  FOptShowActiveMarkInverted:= _InitOptShowActiveMarkInverted;

  FOptMouseMiddleClickClose:= _InitOptMouseMiddleClickClose;
  FOptMouseDoubleClickClose:= _InitOptMouseDoubleClickClose;
  FOptMouseDoubleClickPlus:= _InitOptMouseDoubleClickPlus;
  FOptMouseDragEnabled:= _InitOptMouseDragEnabled;
  FOptMouseDragOutEnabled:= _InitOptMouseDragOutEnabled;

  FHintForX:= 'Close tab';
  FHintForPlus:= 'Add tab';
  FHintForArrowLeft:= 'Scroll tabs left';
  FHintForArrowRight:= 'Scroll tabs right';
  FHintForArrowMenu:= 'Show tabs list';
  FHintForUser0:= '0';
  FHintForUser1:= '1';
  FHintForUser2:= '2';
  FHintForUser3:= '3';
  FHintForUser4:= '4';

  FBitmap:= TBitmap.Create;
  FBitmap.PixelFormat:= pf24bit;
  BitmapSetSize(FBitmap, 1600, 60);

  FBitmapAngle:= TBitmap.Create;
  FBitmapAngle.PixelFormat:= pf24bit;

  FBitmapRound:= TBitmap.Create;
  FBitmapRound.PixelFormat:= pf24bit;
  BitmapSetSize(FBitmapRound, _InitRoundedBitmapSize, _InitRoundedBitmapSize);

  FTabIndex:= 0;
  FTabIndexOver:= -1;
  FTabIndexHinted:= -1;
  FTabIndexAnimated:= -1;
  FAnimationOffset:= 0;
  //FTabList:= TCollection.Create(TATTabData);
  FTabList:= TATTabListCollection.Create(TATTabData);
  FTabList.AOwner:= Self;
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
  if FThemed then
  begin
    FThemed:= false;
    FreeAndNil(FPic_Side_L);
    FreeAndNil(FPic_Side_L_a);
    FreeAndNil(FPic_Side_R);
    FreeAndNil(FPic_Side_R_a);
    FreeAndNil(FPic_Side_C);
    FreeAndNil(FPic_Side_C_a);
    FreeAndNil(FPic_X);
    FreeAndNil(FPic_X_a);
    FreeAndNil(FPic_Plus);
    FreeAndNil(FPic_Plus_a);
    FreeAndNil(FPic_Arrow_L);
    FreeAndNil(FPic_Arrow_L_a);
    FreeAndNil(FPic_Arrow_R);
    FreeAndNil(FPic_Arrow_R_a);
    FreeAndNil(FPic_Arrow_D);
    FreeAndNil(FPic_Arrow_D_a);
  end;

  Clear;
  FreeAndNil(FCaptionList);
  FreeAndNil(FTabList);
  FreeAndNil(FBitmapRound);
  FreeAndNil(FBitmapAngle);
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
  begin
    DoPaintTo(Canvas);
  end;

  {$ifdef tabs_paint_counter}
  Inc(FPaintCount);
  Canvas.Font.Color:= clRed;
  Canvas.TextOut(0, 0, IntToStr(FPaintCount));;
  {$endif}
end;

procedure TATTabs.DoPaintTabTo(
  C: TCanvas; const ARect: TRect; const ACaption: TATTabString;
  ATabIndex: integer;
  AColorFont: TColor;
  ATabActive, ATabMouseOver, ATabMouseOverX: boolean;
  AFontStyle: TFontStyles);
const
  cIndentSep = 2;
var
  RectText: TRect;
  NIndentL, NIndentR, NIndentTop, NLineHeight, NLineWidth: integer;
  AImageIndex: integer;
  ATabModified: boolean;
  TempCaption: TATTabString;
  Extent: TSize;
  bNeedMoreSpace: boolean;
  NColor: TColor;
  ColorPos: TATTabPosition;
  Data: TATTabData;
  i: integer;
begin
  //optimize for 200 tabs
  if ARect.Left>=ClientWidth then exit;
  //skip tabs scrolled lefter
  if ARect.Right<=0 then exit;

  UpdateCanvasAntialiasMode(C);

  DoPaintTabShape(C,
    Rect(
      ARect.Left-DoScale(FOptSpaceSide),
      ARect.Top,
      ARect.Right+DoScale(FOptSpaceSide),
      ARect.Bottom),
    ATabActive,
    ATabIndex
    );

  Data:= GetTabData(ATabIndex);
  if Assigned(Data) then
  begin
    AImageIndex:= Data.TabImageIndex;
    ATabModified:= Data.TabModified;
  end
  else
  begin
    AImageIndex:= -1;
    ATabModified:= false;
  end;

  RectText:= Rect(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom);
  bNeedMoreSpace:= (RectText.Right-RectText.Left<=DoScale(FOptSpaceBeforeTextForMinWidth)) and (ACaption<>'');
  NIndentL:= IfThen(not bNeedMoreSpace, DoScale(FOptSpaceBeforeText), 2);
  NIndentR:= NIndentL+IfThen(IsShowX(ATabIndex), DoScale(FOptSpaceXRight));
  RectText:= Rect(ARect.Left+NIndentL, ARect.Top, ARect.Right-NIndentR, ARect.Bottom);

  if not FThemed then
  if FOptShowFlat and FOptShowFlatSepar then
  begin
    i:= ARect.Left - DoScale(FOptSpaceBetweenTabs) div 2;
    DrawLine(C, i, ARect.Top+cIndentSep, i, ARect.Bottom-cIndentSep, FColorSeparator);
  end;

  //imagelist
  if Assigned(FImages) then
    if (AImageIndex>=0) and (AImageIndex<FImages.Count) then
    begin
      NIndentTop:=
        (RectText.Top + RectText.Bottom - FImages.Height + DoScale(FOptColoredBandSize)) div 2;
      case FOptIconPosition of
        aipIconLefterThanText:
          begin
            FImages.Draw(C,
              RectText.Left - 2,
              NIndentTop,
              AImageIndex);
            Inc(RectText.Left, FImages.Width+DoScale(FOptSpaceBetweenIconCaption));
          end;
        aipIconRighterThanText:
          begin
            FImages.Draw(C,
              RectText.Right - FImages.Width + 2,
              NIndentTop,
              AImageIndex);
            Dec(RectText.Right, FImages.Width+DoScale(FOptSpaceBetweenIconCaption));
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
              RectText.Top + DoScale(FOptColoredBandSize),
              AImageIndex);
            Inc(RectText.Top, FImages.Height+DoScale(FOptSpaceBetweenIconCaption));
          end;
        aipIconBelowTextCentered:
          begin
            FImages.Draw(C,
              (RectText.Left + RectText.Right - FImages.Width) div 2,
              RectText.Bottom - FImages.Height,
              AImageIndex);
            Dec(RectText.Bottom, FImages.Height+DoScale(FOptSpaceBetweenIconCaption));
          end;
      end;
    end;

  //caption
  C.Brush.Style:= bsClear;
  if RectText.Right-RectText.Left>=8 then
  begin
    C.Font.Assign(Self.Font);
    C.Font.Style:= AFontStyle;
    C.Font.Color:= AColorFont;

    TempCaption:= ACaption;
    if ATabModified then
      TempCaption:= FOptShowModifiedText+TempCaption;
    UpdateCaptionProps(C, TempCaption, NLineHeight, Extent);

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
        _ShortenStringEx(C, FCaptionList[i], FOptTruncateCaption, RectText.Right-RectText.Left)
        );
    end;
  end;

  NColor:= clNone;
  if ATabMouseOver and not ATabActive and Assigned(Data) and (Data.TabColorOver<>clNone) then
    NColor:= Data.TabColorOver
  else
  if ATabActive and Assigned(Data) and (Data.TabColorActive<>clNone) then
    NColor:= Data.TabColorActive
  else
  if Assigned(Data) and (Data.TabColor<>clNone) then
    NColor:= Data.TabColor;

  //colored band
  if not FOptShowEntireColor then
  begin
    if NColor<>clNone then
    begin
      case FOptPosition of
        atpTop:
          ColorPos:= FOptColoredBandForTop;
        atpBottom:
          ColorPos:= FOptColoredBandForBottom;
        atpLeft:
          ColorPos:= FOptColoredBandForLeft;
        atpRight:
          ColorPos:= FOptColoredBandForRight;
        else
          raise Exception.Create('Unknown tab pos');
      end;
      DoPaintColoredBand(C, ARect, NColor, ColorPos);
    end;
  end;
end;

procedure TATTabs.DoPaintPlus(C: TCanvas; const ARect: TRect);
var
  NColorFont: TColor;
  ElemType: TATTabElemType;
  Pic: TATTabsPicture;
  bActive: boolean;
begin
  bActive:= FTabIndexOver=cTabIndexPlus;
  if bActive then
    ElemType:= aeTabPlusOver
  else
    ElemType:= aeTabPlus;

  if IsPaintNeeded(ElemType, -1, C, ARect) then
  begin
    NColorFont:= FColorFont;

    DoPaintTabTo(C, ARect,
      '',
      cTabIndexPlus,
      NColorFont,
      false,
      false,
      false,
      []
      );

    if FThemed then
    begin
      if bActive then
        Pic:= FPic_Plus_a
      else
        Pic:= FPic_Plus;
      Pic.Draw(C,
        (ARect.Left+ARect.Right-Pic.Width) div 2,
        (ARect.Top+ARect.Bottom-Pic.Height) div 2
        );
      exit;
    end
    else
      DrawPlusSign(C, ARect, DoScale(FOptArrowSize), FColorFont);

    DoPaintAfter(ElemType, -1, C, ARect);
  end;
end;


procedure TATTabs.DoPaintTabShape(C: TCanvas; const ATabRect: TRect;
  ATabActive: boolean; ATabIndex: integer);
var
  AColorBg: TColor;
  PL1, PL2, PR1, PR2: TPoint;
  R: TRect;
begin
  R.Top:= ATabRect.Top;
  R.Bottom:= ATabRect.Bottom;
  R.Left:= ATabRect.Left+DoScale(FOptSpaceSide);
  R.Right:= ATabRect.Right-DoScale(FOptSpaceSide);

  if not FThemed then
  begin
    if ATabActive then
      AColorBg:= GetTabBgColor_Active(ATabIndex)
    else
      AColorBg:= GetTabBgColor_Passive(ATabIndex);

    C.Pen.Color:= AColorBg;
    C.Brush.Color:= AColorBg;
    C.FillRect(R);
  end;

  PL1:= Point(R.Left, R.Top);
  PL2:= Point(R.Left, R.Bottom-1);
  PR1:= Point(R.Right-1, R.Top);
  PR2:= Point(R.Right-1, R.Bottom-1);

  //center shape
  DoPaintTabShape_C(C, ATabActive, ATabIndex, R, PL1, PL2, PR1, PR2);

  //left/right edges
  if FOptSpaceSide>0 then
  begin
    DoPaintTabShape_L(C, R, ATabActive, ATabIndex);
    DoPaintTabShape_R(C, R, ATabActive, ATabIndex);
  end;
end;

procedure TATTabs.DoPaintTabShape_C(C: TCanvas;
  ATabActive: boolean;
  ATabIndex: integer;
  const ARect: TRect;
  const PL1, PL2, PR1, PR2: TPoint);
var
  ColorPos: TATTabPosition;
  Pic: TATTabsPicture;
  AColorBg, AColorBorder, AColorBorderLow: TColor;
begin
  if FThemed then
  begin
    if ATabActive then
      Pic:= FPic_Side_C_a
    else
      Pic:= FPic_Side_C;
    Pic.DrawSized(C, PL1.X, PL1.Y, PR1.X-PL1.X);
    exit;
  end;

  if ATabActive then
  begin
    AColorBg:= GetTabBgColor_Active(ATabIndex);
    AColorBorder:= FColorBorderActive;
    AColorBorderLow:= clNone;
  end
  else
  begin
    AColorBg:= GetTabBgColor_Passive(ATabIndex);
    AColorBorder:= FColorBorderPassive;
    AColorBorderLow:= FColorBorderActive;
  end;

  if FOptShowFlat then
  begin
    if ATabActive then
    begin
      ColorPos:= FOptPosition;
      if FOptShowActiveMarkInverted then
        ColorPos:= GetPositionInverted(ColorPos);
      DoPaintColoredBand(C, ARect, FColorActiveMark, ColorPos);
    end;
  end
  else
  case FOptPosition of
    atpTop:
      begin
        if FOptSpaceSide=0 then
          DrawLine(C, PL1.X, PL1.Y, PL2.X, PL2.Y+1, AColorBorder);
        if FOptSpaceSide=0 then
          DrawLine(C, PR1.X, PR1.Y, PR2.X, PR2.Y+1, AColorBorder);
        DrawLine(C, PL1.X, PL1.Y, PR1.X, PL1.Y, AColorBorder);
        if AColorBorderLow<>clNone then
          DrawLine(C, PL2.X-DoScale(FOptSpaceSide), ARect.Bottom,
                      PR2.X+DoScale(FOptSpaceSide), ARect.Bottom, AColorBorderLow)
        else
          DrawLine(C, PL2.X+1, ARect.Bottom, PR2.X-1, ARect.Bottom, AColorBg);
      end;
    atpBottom:
      begin
        DrawLine(C, PL1.X, PL1.Y, PL2.X, PL2.Y+1, AColorBorder);
        DrawLine(C, PR1.X, PR1.Y, PR2.X, PR2.Y+1, AColorBorder);
        DrawLine(C, PL2.X, PL2.Y+1, PR2.X, PL2.Y+1, AColorBorder);
        if AColorBorderLow<>clNone then
          DrawLine(C, PL1.X-DoScale(FOptSpaceSide), ARect.Top,
                      PR1.X+DoScale(FOptSpaceSide), ARect.Top, AColorBorderLow)
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
end;

procedure TATTabs.DoPaintTabShape_L(C: TCanvas; const ARect: TRect;
  ATabActive: boolean; ATabIndex: integer);
var
  Pic: TATTabsPicture;
  AColorBg, AColorBorder: TColor;
begin
  if FThemed then
  begin
    if ATabActive then
      Pic:= FPic_Side_L_a
    else
      Pic:= FPic_Side_L;
    Pic.Draw(C, ARect.Left-DoScale(FOptSpaceSide), ARect.Top);
    exit;
  end;

  if ATabActive then
  begin
    AColorBg:= GetTabBgColor_Active(ATabIndex);
    AColorBorder:= FColorBorderActive
  end
  else
  begin
    AColorBg:= GetTabBgColor_Passive(ATabIndex);
    AColorBorder:= FColorBorderPassive;
  end;

  if not FOptShowFlat then
    case FOptPosition of
      atpTop:
        begin
          DrawTriangleRectFramed(C,
            ARect.Left-DoScale(FOptSpaceSide)+1,
            ARect.Top,
            DoScale(FOptSpaceSide),
            DoScale(FOptTabHeight)+IfThen(ATabActive, 1),
            cSmoothScale,
            ampnTopLeft,
            AColorBg,
            AColorBorder,
            FBitmapAngle);
        end;
      atpBottom:
        begin
          DrawTriangleRectFramed(C,
            ARect.Left-DoScale(FOptSpaceSide)+1,
            ARect.Top+IfThen(not ATabActive, 1),
            DoScale(FOptSpaceSide),
            DoScale(FOptTabHeight),
            cSmoothScale,
            ampnBottomLeft,
            AColorBg,
            AColorBorder,
            FBitmapAngle);
        end;
    end;
end;

procedure TATTabs.DoPaintTabShape_R(C: TCanvas; const ARect: TRect;
  ATabActive: boolean; ATabIndex: integer);
var
  Pic: TATTabsPicture;
  AColorBg, AColorBorder: TColor;
begin
  if FThemed then
  begin
    if ATabActive then
      Pic:= FPic_Side_R_a
    else
      Pic:= FPic_Side_R;
    Pic.Draw(C, ARect.Right-1, ARect.Top);
    exit;
  end;

  if ATabActive then
  begin
    AColorBg:= GetTabBgColor_Active(ATabIndex);
    AColorBorder:= FColorBorderActive
  end
  else
  begin
    AColorBg:= GetTabBgColor_Passive(ATabIndex);
    AColorBorder:= FColorBorderPassive;
  end;

  if not FOptShowFlat then
    case FOptPosition of
      atpTop:
        begin
          DrawTriangleRectFramed(C,
            ARect.Right-1,
            ARect.Top,
            DoScale(FOptSpaceSide),
            DoScale(FOptTabHeight)+IfThen(ATabActive, 1),
            cSmoothScale,
            ampnTopRight,
            AColorBg,
            AColorBorder,
            FBitmapAngle);
        end;
      atpBottom:
        begin
          DrawTriangleRectFramed(C,
            ARect.Right-1,
            ARect.Top+IfThen(not ATabActive, 1),
            DoScale(FOptSpaceSide),
            DoScale(FOptTabHeight),
            cSmoothScale,
            ampnBottomRight,
            AColorBg,
            AColorBorder,
            FBitmapAngle);
        end;
    end;
end;


procedure TATTabs.DoPaintX(C: TCanvas; const ARectX: TRect;
  ATabIndex: integer; ATabActive, AMouseOverX: boolean);
var
  ElemType: TATTabElemType;
begin
  if AMouseOverX then
    ElemType:= aeTabIconXOver
  else
    ElemType:= aeTabIconX;

  if IsPaintNeeded(ElemType, -1, C, ARectX) then
  begin
    DoPaintXTo(C, ARectX, ATabIndex, ATabActive, AMouseOverX);
    DoPaintAfter(ElemType, -1, C, ARectX);
  end;
end;

procedure TATTabs.DoPaintXTo(C: TCanvas; const R: TRect;
  ATabIndex: integer; ATabActive, AMouseOverX: boolean);
var
  Pic: TATTabsPicture;
  PX1, PX2, PX3, PX4, PXX1, PXX2: TPoint;
  RectRound, RectBitmap: TRect;
  NColorBg, NColorXBg, NColorXBorder, NColorXMark: TColor;
begin
  if FThemed then
  begin
    if AMouseOverX then
      Pic:= FPic_X_a
    else
      Pic:= FPic_X;
    Pic.Draw(C, R.Left, R.Top);
    exit;
  end;

  if ATabActive then
    NColorBg:= GetTabBgColor_Active(ATabIndex)
  else
    NColorBg:= GetTabBgColor_Passive(ATabIndex);
  GetTabXColors(ATabIndex, AMouseOverX, NColorXBg, NColorXBorder, NColorXMark);

  if FOptShowXRounded then
  begin
    if NColorXBg<>clNone then
    begin
      RectRound:= R;
      InflateRect(RectRound, DoScale(FOptSpaceXIncrementRound), DoScale(FOptSpaceXIncrementRound));

      RectBitmap.Left:= 0;
      RectBitmap.Top:= 0;
      RectBitmap.Right:= FBitmapRound.Width;
      RectBitmap.Bottom:= RectBitmap.Right;

      FBitmapRound.Canvas.Brush.Color:= NColorBg;
      FBitmapRound.Canvas.FillRect(RectBitmap);

      FBitmapRound.Canvas.Brush.Color:= NColorXBg;
      FBitmapRound.Canvas.Pen.Color:= NColorXBorder;
      FBitmapRound.Canvas.Ellipse(RectBitmap);

      CanvasStretchDraw(C, RectRound, FBitmapRound);
    end
    else
    begin
      C.Brush.Color:= NColorBg;
      C.FillRect(R);
    end;
  end
  else
  begin
    C.Brush.Color:= IfThen(NColorXBg<>clNone, NColorXBg, NColorBg);
    C.FillRect(R);
    C.Pen.Color:= IfThen(NColorXBorder<>clNone, NColorXBorder, NColorBg);
    C.Rectangle(R);
  end;

  //paint cross by 2 polygons, each has 6 points (3 points at line edge)
  C.Brush.Color:= NColorXMark;
  C.Pen.Color:= NColorXMark;

  PXX1:= Point(R.Left+DoScale(FOptSpaceXInner), R.Top+DoScale(FOptSpaceXInner));
  PXX2:= Point(R.Right-DoScale(FOptSpaceXInner)-1, R.Bottom-DoScale(FOptSpaceXInner)-1);
  PX1:= Point(PXX1.X+1, PXX1.Y);
  PX2:= Point(PXX1.X, PXX1.Y+1);
  PX3:= Point(PXX2.X-1, PXX2.Y);
  PX4:= Point(PXX2.X, PXX2.Y-1);
  C.Polygon([PX1, PXX1, PX2, PX3, PXX2, PX4]);

  PXX1:= Point(R.Right-DoScale(FOptSpaceXInner)-1, R.Top+DoScale(FOptSpaceXInner));
  PXX2:= Point(R.Left+DoScale(FOptSpaceXInner), R.Bottom-DoScale(FOptSpaceXInner)-1);
  PX1:= Point(PXX1.X-1, PXX1.Y);
  PX2:= Point(PXX1.X, PXX1.Y+1);
  PX3:= Point(PXX2.X+1, PXX2.Y);
  PX4:= Point(PXX2.X, PXX2.Y-1);
  C.Polygon([PX1, PXX1, PX2, PX3, PXX2, PX4]);

  C.Brush.Color:= NColorBg;
end;

function TATTabs.GetTabWidth_Plus_Raw: integer;
begin
  Result:= DoScale(FOptArrowSize)*4;
end;

function TATTabs.GetTabRectWidth(APlusBtn: boolean): integer;
begin
  case FOptPosition of
    atpLeft,
    atpRight:
      begin
        Result:= ClientWidth-DoScale(FOptSpacer);
      end;
    else
      begin
        if APlusBtn then
          Result:= GetTabWidth_Plus_Raw
        else
          Result:= DoScale(FOptTabWidthNormal);
        Inc(Result, 2*DoScale(FOptSpaceBeforeText));
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

procedure TATTabs.UpdateTabRects(C: TCanvas);
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
    R.Left:= IfThen(FOptPosition=atpLeft, DoScale(FOptSpacer), DoScale(FOptSpacer2)+1);
    R.Right:= IfThen(FOptPosition=atpLeft, ClientWidth-DoScale(FOptSpacer2), ClientWidth-DoScale(FOptSpacer));
    R.Bottom:= GetInitialVerticalIndent;
    R.Top:= R.Bottom;

    for i:= 0 to TabCount-1 do
    begin
      Data:= GetTabData(i);
      if not Assigned(Data) then Continue;

      R.Top:= R.Bottom;
      if i>0 then
        Inc(R.Top, DoScale(FOptSpaceBetweenTabs));

      if Data.TabSpecialHeight>0 then
        NLineHeight:= Data.TabSpecialHeight
      else
      if FOptVarWidth then
      begin
        UpdateCaptionProps(C, Data.TabCaption, NLineHeight, Extent);
        NLineHeight:= 2*DoScale(FOptSpaceBeforeText) + Extent.CY;
      end
      else
        NLineHeight:= DoScale(FOptTabHeight);

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
    FTabWidth:= DoScale(FOptTabWidthNormal);
  NWidthSaved:= FTabWidth;

  R.Left:= FRealIndentLeft+DoScale(FOptSpaceSide);
  R.Right:= R.Left;
  R.Top:= DoScale(FOptSpacer);
  R.Bottom:= R.Top+DoScale(FOptTabHeight);
  NIndexLineStart:= 0;

  for i:= 0 to TabCount-1 do
  begin
    Data:= GetTabData(i);
    if not Assigned(Data) then Continue;
    Data.TabStartsNewLine:= false;

    R.Left:= R.Right;
    if i>0 then
      Inc(R.Left, DoScale(FOptSpaceBetweenTabs));

    if Data.TabSpecialWidth>0 then
      FTabWidth:= Data.TabSpecialWidth
    else
    if FOptVarWidth then
    begin
      C.Font.Style:= Data.TabFontStyle;

      if FOptActiveFontStyleUsed then
        if i=FTabIndex then
          C.Font.Style:= FOptActiveFontStyle;

      TempCaption:=
        Format(FOptShowNumberPrefix, [i+1]) +
        IfThen(Data.TabModified, FOptShowModifiedText) +
        Data.TabCaption;

      UpdateCaptionProps(C, TempCaption, NLineHeight, Extent);
      FTabWidth:= Extent.CX + 2*DoScale(FOptSpaceBeforeText);

      if not Assigned(FImages) then //no imagelist
        Data.TabImageIndex := -1;

      if Data.TabImageIndex>=0 then
        if FOptIconPosition in [aipIconLefterThanText, aipIconRighterThanText] then
          Inc(FTabWidth, FImages.Width);

      if FOptShowXButtons<>atbxShowNone then
        if not Data.TabHideXButton then
          Inc(FTabWidth, DoScale(FOptSpaceXSize));

      if FTabWidth<DoScale(FOptTabWidthMinimal) then
        FTabWidth:= DoScale(FOptTabWidthMinimal);
      if FTabWidth>DoScale(FOptTabWidthMaximal) then
        FTabWidth:= DoScale(FOptTabWidthMaximal);
    end;

    if FOptMultiline and (i>0) then
      if R.Left+FTabWidth+FRealIndentRight+NWidthPlus >= ClientWidth then
      begin
        Data.TabStartsNewLine:= true;
        FMultilineActive:= true;

        R.Left:= FRealIndentLeft;
        R.Top:= R.Bottom+DoScale(FOptSpaceBetweenLines);
        R.Bottom:= R.Top+DoScale(FOptTabHeight);

        if FOptFillWidth then
          UpdateTabRectsToFillLine(NIndexLineStart, i-1, false);
        NIndexLineStart:= i;
      end;

    R.Right:= R.Left + FTabWidth;
    Data.TabRect:= R;
  end;

  if FOptFillWidth and FOptFillWidthLastToo then
    UpdateTabRectsToFillLine(NIndexLineStart, TabCount-1, true);

  if FOptMultiline then
    Height:= R.Bottom+DoScale(FOptSpacer2);

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
          Result.Left:= Result.Right + DoScale(FOptSpaceBetweenTabs);
          Result.Right:= Result.Left + GetTabRectWidth(true);
        end
        else
        begin
          Result.Top:= DoScale(FOptSpacer);
          Result.Bottom:= Result.Top + DoScale(FOptTabHeight);
          Result.Left:= FRealIndentLeft;
          Result.Right:= Result.Left + GetTabRectWidth(true);
        end;
      end;
    else
      begin
        if TabCount>0 then
        begin
          Result:= GetTabRect(TabCount-1, AWithScroll, false);
          Result.Top:= Result.Bottom + DoScale(FOptSpaceBetweenTabs);
          Result.Bottom:= Result.Top + DoScale(FOptTabHeight);
        end
        else
        begin
          Result.Left:= IfThen(FOptPosition=atpLeft, DoScale(FOptSpacer), DoScale(FOptSpacer2));
          Result.Right:= IfThen(FOptPosition=atpLeft, ClientWidth-DoScale(FOptSpacer2), ClientWidth-DoScale(FOptSpacer));
          Result.Top:= GetInitialVerticalIndent;
          Result.Bottom:= Result.Top + DoScale(FOptTabHeight);
        end;
      end;
  end;
end;

function TATTabs.GetTabRect_X(const ARect: TRect): TRect;
var
  P: TPoint;
begin
  P:= Point(
    ARect.Right-DoScale(FOptSpaceXRight),
    (ARect.Top+ARect.Bottom) div 2 + 1);
  Dec(P.X, DoScale(FOptSpaceXSize) div 2);
  Dec(P.Y, DoScale(FOptSpaceXSize) div 2);
  Result:= Rect(
    P.X,
    P.Y,
    P.X+DoScale(FOptSpaceXSize),
    P.Y+DoScale(FOptSpaceXSize));
end;

function TATTabs._IsDrag: boolean;
begin
  Result:= Dragging and FMouseDragBegins;
end;

procedure TATTabs.GetTabXColors(AIndex: integer;
  AMouseOverX: boolean;
  out AColorXBg, AColorXBorder, AColorXMark: TColor);
begin
  if GetTabFlatEffective(AIndex) then
    AColorXBg:= FColorBg
  else
    AColorXBg:= FColorCloseBg;

  AColorXBorder:= AColorXBg;
  AColorXMark:= FColorCloseX;

  if AMouseOverX then
  begin
    AColorXBg:= FColorCloseBgOver;
    AColorXBorder:= FColorCloseBorderOver;
    AColorXMark:= FColorCloseXOver;
  end;
end;

procedure TATTabs.GetTabXProps(AIndex: integer; const ARect: TRect;
  out AMouseOverX: boolean;
  out ARectX: TRect);
begin
  AMouseOverX:= false;
  ARectX:= GetTabRect_X(ARect);

  if _IsDrag then Exit;

  if IsShowX(AIndex) then
    if AIndex=FTabIndexOver then
    begin
      AMouseOverX:= PtInRect(ARectX, ScreenToClient(Mouse.CursorPos));
    end;
end;

function TATTabs.IsPaintNeeded(AElemType: TATTabElemType;
  AIndex: integer; ACanvas: TCanvas; const ARect: TRect): boolean;
begin
  Result:= ARect.Right>ARect.Left;
  if Result then
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
  if ParentColor and Assigned(Parent) then
  begin
    if C.Brush.Color <> Parent.Brush.Color then
      C.Brush.Color:= Parent.Brush.Color;
  end
  else
  begin
    if C.Brush.Color <> FColorBg then
      C.Brush.Color:= FColorBg;
  end;

  C.FillRect(ARect);
end;

procedure TATTabs.DoPaintTo(C: TCanvas);
var
  RRect, RBottom, RectX: TRect;
  NColorFont: TColor;
  NLineX1, NLineY1, NLineX2, NLineY2: integer;
  ElemType: TATTabElemType;
  Data: TATTabData;
  NFontStyle: TFontStyles;
  bMouseOver, bMouseOverX: boolean;
  i: integer;
begin
  ElemType:= aeBackground;
  RRect:= ClientRect;

  //update index here, because user can add/del tabs by keyboard
  with ScreenToClient(Mouse.CursorPos) do
    FTabIndexOver:= GetTabAt(X, Y, bMouseOverX);

  FLastOverIndex:= FTabIndexOver;
  FLastOverX:= bMouseOverX;

  FRealIndentLeft:= DoScale(FOptSpaceInitial) + GetButtonsWidth(FButtonsLeft);
  FRealIndentRight:= DoScale(FOptSpaceInitial) + GetButtonsWidth(FButtonsRight);

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
  UpdateTabWidths;
  UpdateTabRects(C);

  //paint spacer rect
  if not FOptShowFlat then
  begin
    ElemType:= aeSpacerRect;
    case FOptPosition of
      atpTop:
        begin
          if FOptMultiline then
            RBottom:= Rect(0, ClientHeight-DoScale(FOptSpacer2), ClientWidth, ClientHeight)
          else
            RBottom:= Rect(0, DoScale(FOptSpacer)+DoScale(FOptTabHeight), ClientWidth, ClientHeight);
          NLineX1:= RBottom.Left;
          NLineY1:= RBottom.Top;
          NLineX2:= RBottom.Right;
          NLineY2:= RBottom.Top;
        end;
      atpBottom:
        begin
          RBottom:= Rect(0, 0, ClientWidth, DoScale(FOptSpacer));
          NLineX1:= RBottom.Left;
          NLineY1:= RBottom.Bottom;
          NLineX2:= RBottom.Right;
          NLineY2:= RBottom.Bottom;
        end;
      atpLeft:
        begin
          RBottom:= Rect(ClientWidth-DoScale(FOptSpacer2), 0, ClientWidth, ClientHeight);
          NLineX1:= RBottom.Left;
          NLineY1:= RBottom.Top;
          NLineX2:= RBottom.Left;
          NLineY2:= RBottom.Bottom;
        end;
      atpRight:
        begin
          RBottom:= Rect(0, 0, DoScale(FOptSpacer2), ClientHeight);
          NLineX1:= RBottom.Right;
          NLineY1:= RBottom.Top;
          NLineX2:= RBottom.Right;
          NLineY2:= RBottom.Bottom;
        end;
      else
        raise Exception.Create('Unknown tab pos');
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
    DoPaintPlus(C, GetTabRect_Plus);
  end;

  //paint passive tabs
  for i:= TabCount-1 downto 0 do
    if i<>FTabIndex then
    begin
      RRect:= GetTabRect(i);
      GetTabXProps(i, RRect, bMouseOverX, RectX);

      bMouseOver:= i=FTabIndexOver;

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
          i,
          NColorFont,
          false,
          bMouseOver,
          bMouseOverX,
          NFontStyle
          );
        DoPaintAfter(ElemType, i, C, RRect);
      end;

      if IsShowX(i) then
      begin
        DoPaintX(C, RectX, i, false, bMouseOverX);
      end;
    end;

  //paint active tab
  i:= FTabIndex;
  if IsIndexOk(i) then
  begin
    RRect:= GetTabRect(i);
    GetTabXProps(i, RRect, bMouseOverX, RectX);

    bMouseOver:= i=FTabIndexOver;

    if IsPaintNeeded(aeTabActive, i, C, RRect) then
    begin
      Data:= TATTabData(FTabList.Items[i]);

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
        i,
        NColorFont,
        true,
        bMouseOver,
        bMouseOverX,
        NFontStyle
        );
      DoPaintAfter(aeTabActive, i, C, RRect);
    end;

    if IsShowX(i) then
    begin
      DoPaintX(C, RectX, i, true, bMouseOverX);
    end;
  end;

  //button back
  DoPaintButtonsBG(C);
  //buttons
  DoPaintArrowLeft(C);
  DoPaintArrowRight(C);
  DoPaintArrowDown(C);
  DoPaintButtonPlus(C);
  DoPaintButtonClose(C);
  DoPaintUserButtons(C, FButtonsLeft, true);
  DoPaintUserButtons(C, FButtonsRight, false);

  if FOptShowDropMark then
    if _IsDrag then
      if PtInControl(Self, Mouse.CursorPos) then
        DoPaintDropMark(C);

  if FOptShowScrollMark then
    DoPaintScrollMark(C);
end;

procedure TATTabs.DoTextOut(C: TCanvas; AX, AY: integer;
  const AClipRect: TRect; const AText: string);
{$ifdef WIDE}
var
  Str: WideString;
begin
  Str:= UTF8Decode(AText);
  ExtTextOutW(C.Handle, AX, AY, ETO_CLIPPED, @AClipRect,
    PWideChar(Str), Length(Str), nil);
end;
{$else}
begin
  ExtTextOut(C.Handle, AX, AY, ETO_CLIPPED, @AClipRect,
    PChar(AText), Length(AText), nil);
end;
{$endif}

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
          R.Left:= R.Left - DoScale(FOptDropMarkSize) div 2;
          R.Right:= R.Left + DoScale(FOptDropMarkSize);
        end;
      else
        begin
          R.Top:= IfThen(i<=FTabIndex, R.Top, R.Bottom);
          R.Top:= R.Top  - DoScale(FOptDropMarkSize) div 2;
          R.Bottom:= R.Top + DoScale(FOptDropMarkSize);
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
          Result:= FTabWidth<=DoScale(FOptTabWidthMinimal)
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
          R.Top:= IfThen(FOptPosition=atpBottom, DoScale(FOptTabHeight) + DoScale(FOptSpacer), 0);
          R.Bottom:= R.Top + DoScale(FOptScrollMarkSizeY);

          R.Left:= FRealIndentLeft +
            Max(0, Min(
              NSize-DoScale(FOptScrollMarkSizeX),
              Int64(FScrollPos) * (NSize-DoScale(FOptScrollMarkSizeX)) div NPos
            ));
          R.Right:= R.Left + DoScale(FOptScrollMarkSizeX);

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
              NSize - DoScale(FOptScrollMarkSizeX),
              Int64(FScrollPos) * (NSize-DoScale(FOptScrollMarkSizeX)) div NPos
              ));
          R.Bottom:= R.Top + DoScale(FOptScrollMarkSizeX);

          if FOptPosition=atpLeft then
          begin
            R.Left:= 0;
            R.Right:= R.Left + DoScale(FOptScrollMarkSizeY);
          end
          else
          begin
            R.Right:= ClientWidth;
            R.Left:= R.Right - DoScale(FOptScrollMarkSizeY);
          end;

          C.Brush.Color:= FColorScrollMark;
          C.FillRect(R);
        end;
      end;
  end;
end;

procedure TATTabs.SetOptButtonLayout(const AValue: string);
begin
  //if FOptButtonLayout=AValue then Exit;
  FOptButtonLayout:= AValue;
  ApplyButtonLayout;
  Invalidate;
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

procedure TATTabs.SetOptScalePercents(AValue: integer);
begin
  if FOptScalePercents=AValue then Exit;
  FOptScalePercents:= AValue;
  ApplyButtonLayout;
  Invalidate;
end;

procedure TATTabs.SetOptVarWidth(AValue: boolean);
begin
  if FOptVarWidth=AValue then Exit;
  FOptVarWidth:= AValue;
  if not AValue then
    FScrollPos:= 0;
  Invalidate;
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
      APressedX:= IsShowX(i) and PtInRect(GetTabRect_X(RectTab), Pnt);
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
  IsClick, IsDblClick: boolean;
begin
  inherited;
  IsClick:= FMouseDown and
    (Abs(X-FMouseDownPnt.X) < cTabsMouseMaxDistanceToClick) and
    (Abs(Y-FMouseDownPnt.Y) < cTabsMouseMaxDistanceToClick);
  IsDblClick:= IsClick and FMouseDownDbl;
  //IsRightClick:= FMouseDownRightBtn and
  //  (Abs(X-FMouseDownPnt.X) < cTabsMouseMaxDistanceToClick) and
  //  (Abs(Y-FMouseDownPnt.Y) < cTabsMouseMaxDistanceToClick);

  FMouseDown:= false;
  FMouseDownDbl:= false;
  FMouseDownRightBtn:= false;
  FMouseDragBegins:= false;
  Cursor:= crDefault;
  Screen.Cursor:= crDefault;
  
  if IsDblClick then
  begin
    if Assigned(FOnTabDblClick) and (FTabIndexOver>=0) then
      FOnTabDblClick(Self, FTabIndexOver);

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
end;

procedure TATTabs.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: integer);
var
  IsX: boolean;
begin
  inherited;
  FMouseDown:= Button in [mbLeft, mbMiddle]; //but not mbRight
  FMouseDownRightBtn:= (Button = mbRight);
  FMouseDownPnt:= Point(X, Y);
  FMouseDownButton:= Button;
  FMouseDownShift:= Shift;
  FMouseDragBegins:= false;

  FTabIndexOver:= GetTabAt(X, Y, IsX);

  //activate tab only if not X clicked
  if not IsX then
    //if TabIndex<>FTabIndexOver then //with this check, CudaText cannot focus active tab in passive tab-group
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
              exit
            end;
          end;

          {
          //normal click on tab caption - was handled on MouseDown before
          if Assigned(FOnTabClick) then
            FOnTabClick(Self);
            }
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
  TControlHack = class(TControl);

procedure TATTabs.MouseMove(Shift: TShiftState; X, Y: integer);
var
  IsX: boolean;
  Data: TATTabData;
begin
  inherited;

  if TabCount=0 then
  begin
    Invalidate; //cleans up <> v and x highlights if no tabs
    exit;
  end;

  FTabIndexOver:= GetTabAt(X, Y, IsX);
  FTabIndexDrop:= FTabIndexOver;
  Data:= nil;

  // LCL dragging with DragMode=automatic is started too early.
  // so use DragMode=manual and DragStart.
  if OptMouseDragEnabled and FMouseDown and not _IsDrag then
  begin
    BeginDrag(false, Mouse.DragThreshold);
    Exit
  end;

  if ShowHint then
  begin
    if IsX then
      FTabIndexHinted:= cTabIndexCloseBtn
    else
      FTabIndexHinted:= FTabIndexOver;

    if FTabIndexHinted<>FTabIndexHintedPrev then
    begin
      FTabIndexHintedPrev:= FTabIndexHinted;
      Hint:= '';
      case FTabIndexHinted of
        cTabIndexPlus,
        cTabIndexPlusBtn:
          Hint:= FHintForPlus;
        cTabIndexArrowScrollLeft:
          Hint:= FHintForArrowLeft;
        cTabIndexArrowScrollRight:
          Hint:= FHintForArrowRight;
        cTabIndexArrowMenu:
          Hint:= FHintForArrowMenu;
        cTabIndexCloseBtn:
          Hint:= FHintForX;
        cTabIndexUser0:
          Hint:= FHintForUser0;
        cTabIndexUser1:
          Hint:= FHintForUser1;
        cTabIndexUser2:
          Hint:= FHintForUser2;
        cTabIndexUser3:
          Hint:= FHintForUser3;
        cTabIndexUser4:
          Hint:= FHintForUser4;
        0..10000:
          begin
            Data:= GetTabData(FTabIndexOver);
            if Assigned(Data) and (Data.TabHint<>'') then
              Hint:= Data.TabHint;
          end;
      end; //case

      if Hint<>'' then
        Application.ActivateHint(Mouse.CursorPos)
      else
        Application.HideHint;
    end;
  end; //if ShowHint

  if Assigned(Data) then
    if Assigned(FOnTabOver) then
      FOnTabOver(Self, FTabIndexOver);

  //repaint only if really needed
  //use {$define tab_paint_counter} to debug it
  if (FTabIndexOver<>FLastOverIndex) or (IsX<>FLastOverX) then
  begin
    Invalidate;
  end;
end;

procedure TATTabs.Resize;
begin
  inherited;
  if Assigned(FBitmap) then
    BitmapSetSize(FBitmap, Max(FBitmap.Width, Width), Max(FBitmap.Height, Height));

  if FOptActiveVisibleOnResize then
    if FTabIndex>=0 then
      MakeVisible(FTabIndex);

  Invalidate;
end;


function TATTabs.AddTab(
  AIndex: integer;
  const ACaption: TATTabString;
  AObject: TObject = nil;
  AModified: boolean = false;
  AColor: TColor = clNone;
  AImageIndex: TImageIndex = -1;
  APopupMenu: TPopupMenu = nil;
  AFontStyle: TFontStyles = [];
  const AHint: TATTabString = ''): TATTabData;
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

  DoAnimationTabAdd(AIndex);

  Invalidate;

  if Assigned(FOnTabMove) then
    FOnTabMove(Self, -1, AIndex);

  Result:= Data;
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
  NMax: integer;
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

    //if lot of tabs were opened, and closed last tab, need to scroll all tabs righter
    NMax:= GetMaxScrollPos;
    if ScrollPos>NMax then
      ScrollPos:= NMax;

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
  AActive: boolean);
var
  Pic: TATTabsPicture;
  NColor: TColor;
begin
  if FThemed then
  begin
    if AActive then
      case ATyp of
        atriLeft: Pic:= FPic_Arrow_L_a;
        atriRight: Pic:= FPic_Arrow_R_a;
        atriDown: Pic:= FPic_Arrow_D_a;
        else exit;
      end
    else
      case ATyp of
        atriLeft: Pic:= FPic_Arrow_L;
        atriRight: Pic:= FPic_Arrow_R;
        atriDown: Pic:= FPic_Arrow_D;
        else exit;
      end;
    Pic.Draw(C,
      (ARect.Left+ARect.Right-Pic.Width) div 2,
      (ARect.Top+ARect.Bottom-Pic.Height) div 2
      );
    exit;
  end;

  if AActive and not _IsDrag then
    NColor:= FColorArrowOver
  else
    NColor:= FColorArrow;

  DrawTriangleType(C, ATyp, ARect, NColor, DoScale(FOptArrowSize) div 2);
end;


function TATTabs.GetIndexOfButton(const AButtons: TATTabButtons; ABtn: TATTabButton): integer;
var
  i: integer;
begin
  Result:= -1;
  for i:= 0 to Length(AButtons)-1 do
    if AButtons[i].Id=ABtn then
      begin Result:= i; exit; end;
end;

function TATTabs.GetButtonsEdgeCoord(AtLeft: boolean): integer;
begin
  if AtLeft then
  begin
    Result:= DoScale(FOptSpaceInitial);
    case FOptPosition of
      atpLeft:
        Inc(Result, DoScale(FOptSpacer));
      atpRight:
        Inc(Result, DoScale(FOptSpacer2));
    end;
  end
  else
  begin
    Result:= ClientWidth;
    case FOptPosition of
      atpLeft:
        Dec(Result, DoScale(FOptSpacer2));
      atpRight:
        Dec(Result, DoScale(FOptSpacer));
    end;
  end;
end;

function TATTabs.GetRectOfButtonIndex(AIndex: integer; AtLeft: boolean): TRect;
var
  NPos, i: integer;
begin
  NPos:= GetButtonsEdgeCoord(AtLeft);
  if AtLeft then
  begin
    for i:= 0 to AIndex do
    begin
      Result.Left:= NPos;
      Result.Right:= Result.Left+FButtonsLeft[i].Size;
      NPos:= Result.Right;
    end;
  end
  else
  begin
    for i:= 0 to AIndex do
    begin
      Result.Right:= NPos;
      Result.Left:= Result.Right-FButtonsRight[i].Size;
      NPos:= Result.Left;
    end;
  end;

  if FOptPosition in [atpTop, atpBottom] then
    Result.Top:= DoScale(FOptSpacer)
  else
    Result.Top:= 0;

  Result.Bottom:= Result.Top+DoScale(FOptTabHeight);

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
  mi: TMenuItem;
  P: TPoint;
  i: integer;
  bShow: boolean;
begin
  if Assigned(TabMenuExternal) then
  begin
    P:= Point(FRectArrowDown.Left, FRectArrowDown.Bottom);
    P:= ClientToScreen(P);
    TabMenuExternal.Popup(P.X, P.Y);
    exit;
  end;

  if TabCount=0 then Exit;

  bShow:= true;
  if Assigned(FOnTabMenu) then
    FOnTabMenu(Self, bShow);
  if not bShow then Exit;

  if not Assigned(FTabMenu) then
    FTabMenu:= TPopupMenu.Create(Self);
  FTabMenu.Items.Clear;

  for i:= 0 to TabCount-1 do
  begin
    mi:= TMenuItem.Create(Self);
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

procedure TATTabs.UpdateTabWidths;
var
  Value, Count: integer;
begin
  if FOptVarWidth then Exit;

  Count:= TabCount;
  if Count=0 then Exit;

  if FOptPosition in [atpLeft, atpRight] then
  begin
    FTabWidth:= ClientWidth-DoScale(FOptSpacer);
    exit
  end;

  //tricky formula: calculate auto-width
  Value:= (ClientWidth
    - IfThen(FOptShowPlusTab, GetTabWidth_Plus_Raw + 2*DoScale(FOptSpaceBeforeText))
    - FRealIndentLeft
    - FRealIndentRight) div Count
      - DoScale(FOptSpaceBetweenTabs);

  if Value<DoScale(FOptTabWidthMinimal) then
    Value:= DoScale(FOptTabWidthMinimal)
  else
  if Value>DoScale(FOptTabWidthNormal) then
    Value:= DoScale(FOptTabWidthNormal);

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
    atbxShowActiveAndMouseOver:
      Result:= (AIndex=FTabIndex) or (AIndex=FTabIndexOver);
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
        if FTabWidth<DoScale(FOptTabWidthMinimalHidesX) then
        begin
          Result:= false;
          Exit
        end;
  end;
end;

procedure TATTabs.DoTabDrop;
var
  NFrom, NTo: integer;
  ACanDrop: boolean;
begin
  NFrom:= FTabIndex;
  if not IsIndexOk(NFrom) then Exit;
  NTo:= FTabIndexDrop;
  if not IsIndexOk(NTo) then
    NTo:= TabCount-1;
  if NFrom=NTo then Exit;

  ACanDrop:= true;
  if Assigned(FOnTabDropQuery) then
    FOnTabDropQuery(Self, NFrom, NTo, ACanDrop);
  if not ACanDrop then Exit;

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
    if Assigned(TControlHack(ATarget).OnDragDrop) then
    begin
      P:= APnt;
      Data:= GetTabData(FTabIndex);
      if Data<>nil then
        TControlHack(ATarget).OnDragDrop(ATarget, Data.TabObject, P.X, P.Y);
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
  inherited;
  FMouseDownDbl:= true;
end;

procedure TATTabs.DragOver(Source: TObject; X, Y: integer; State: TDragState;
  var Accept: Boolean);
var
  IsX: Boolean;
  Limit: integer;
begin
  //this is workaround for too early painted drop-mark (vertical red line)
  if not FMouseDragBegins then
  begin
    Limit:= Mouse.DragThreshold;
    FMouseDragBegins:= (Abs(X-FMouseDownPnt.X)>=Limit) or (Abs(Y-FMouseDownPnt.Y)>=Limit);
  end;

  if Source is TATTabs then
  begin
    Accept:=
      FOptMouseDragEnabled and
      FOptMouseDragOutEnabled;

    // Delphi 7 don't call MouseMove during dragging
    {$ifndef fpc}
    if Accept then
    begin
      FTabIndexDrop:= GetTabAt(X, Y, IsX);
      Invalidate;
    end;
    {$endif}
  end    
  else
    inherited;
end;

procedure TATTabs.DragDrop(Source: TObject; X, Y: integer);
begin
  if not (Source is TATTabs) then
  begin
    inherited;
    exit;
  end;

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

function TATTabs.DoScale(AValue: integer): integer;
begin
  if FOptScalePercents=100 then
    Result:= AValue
  else
    Result:= AValue * FOptScalePercents div 100;
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
    R:= GetTabRect(TabCount-1, false, false);

  case FOptPosition of
    atpTop,
    atpBottom:
      Result:= R.Right;
    else
      Result:= R.Bottom;
  end;
end;

function TATTabs.GetMaxScrollPos: integer;
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
  if not FOptAnimationEnabled then
  begin
    FScrollPos:= APosTo;
    Invalidate;
    exit;
  end;

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

  if IsPaintNeeded(ElemType, -1, C, R) then
    begin
      NColor:= IfThen(
        bOver and not _IsDrag,
        FColorArrowOver,
        FColorArrow);

      DoPaintBgTo(C, R);
      DrawPlusSign(C, R, DoScale(FOptArrowSize), NColor);
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

  if IsPaintNeeded(ElemType, -1, C, R) then
    begin
      NColor:= IfThen(
        bOver and not _IsDrag,
        FColorArrowOver,
        FColorArrow);

      DoPaintBgTo(C, R);
      DrawCrossSign(C, R, DoScale(FOptArrowSize), NColor);
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

  if IsPaintNeeded(ElemType, -1, C, FRectArrowDown) then
    begin
      DoPaintBgTo(C, FRectArrowDown);
      DoPaintArrowTo(C, atriDown, FRectArrowDown, bOver);
      DoPaintAfter(ElemType, -1, C, FRectArrowDown);
    end;
end;

procedure TATTabs.DoPaintArrowLeft(C: TCanvas);
var
  bOver: boolean;
  ElemType: TATTabElemType;
  R: TRect;
begin
  bOver:= (TabCount > 0) and (FTabIndexOver=cTabIndexArrowScrollLeft);
  if bOver then
    ElemType:= aeArrowScrollLeftOver
  else
    ElemType:= aeArrowScrollLeft;

  if IsPaintNeeded(ElemType, -1, C, FRectArrowLeft) then
    begin
      R:= FRectArrowLeft;
      if FOptShowArrowsNear then
        R.Left:= R.Left * 2 div 3 + R.Right div 3;

      DoPaintBgTo(C, FRectArrowLeft);
      DoPaintArrowTo(C, atriLeft, R, bOver);
      DoPaintAfter(ElemType, -1, C, FRectArrowLeft);
    end;
end;

procedure TATTabs.DoPaintArrowRight(C: TCanvas);
var
  bOver: boolean;
  ElemType: TATTabElemType;
  R: TRect;
begin
  bOver:= (TabCount > 0) and (FTabIndexOver=cTabIndexArrowScrollRight);
  if bOver then
    ElemType:= aeArrowScrollRightOver
  else
    ElemType:= aeArrowScrollRight;

  if IsPaintNeeded(ElemType, -1, C, FRectArrowRight) then
    begin
      R:= FRectArrowRight;
      if FOptShowArrowsNear then
        R.Right:= R.Left div 3 + R.Right * 2 div 3;

      DoPaintBgTo(C, FRectArrowRight);
      DoPaintArrowTo(C, atriRight, R, bOver);
      DoPaintAfter(ElemType, -1, C, FRectArrowRight);
    end;
end;


function SwapString(const S: string): string;
var
  i: integer;
begin
  Result:= '';
  SetLength(Result, Length(S));
  for i:= 1 to Length(S) do
    Result[Length(S)+1-i]:= S[i];
end;

procedure TATTabs.ApplyButtonLayout;
  //
  procedure UpdateBtns(var Btns: TATTabButtons; const S: string);
  var
    i: integer;
  begin
    SetLength(Btns, 0);
    for i:= 1 to Length(S) do
      case S[i] of
        '<': AddTabButton(Btns, atbScrollLeft,   DoScale(FOptButtonSize));
        '>': AddTabButton(Btns, atbScrollRight,  DoScale(FOptButtonSize));
        'v': AddTabButton(Btns, atbDropdownMenu, DoScale(FOptButtonSize));
        '+': AddTabButton(Btns, atbPlus,         DoScale(FOptButtonSize));
        'x': AddTabButton(Btns, atbClose,        DoScale(FOptButtonSize));
        '0': AddTabButton(Btns, atbUser0,        DoScale(FOptButtonSize));
        '1': AddTabButton(Btns, atbUser1,        DoScale(FOptButtonSize));
        '2': AddTabButton(Btns, atbUser2,        DoScale(FOptButtonSize));
        '3': AddTabButton(Btns, atbUser3,        DoScale(FOptButtonSize));
        '4': AddTabButton(Btns, atbUser4,        DoScale(FOptButtonSize));
        '_': AddTabButton(Btns, atbSpace,        DoScale(FOptButtonSizeSpace));
        '|': AddTabButton(Btns, atbSeparator,    DoScale(FOptButtonSizeSeparator));
      end;
  end;
  //
var
  S, SLeft, SRight: string;
  N: integer;
begin
  S:= FOptButtonLayout;
  N:= Pos(',', S);
  if N=0 then N:= Length(S)+1;
  SLeft:= Copy(S, 1, N-1);
  SRight:= Copy(S, N+1, MaxInt);

  UpdateBtns(FButtonsLeft, SLeft);
  UpdateBtns(FButtonsRight, SwapString(SRight));
end;

procedure TATTabs.DoClickUser(AIndex: integer);
begin
  if Assigned(FOnTabClickUserButton) then
    FOnTabClickUserButton(Self, AIndex);
end;

procedure TATTabs.DoPaintSeparator(C: TCanvas; const R: TRect);
begin
  DoPaintBgTo(C, R);
  C.Pen.Color:= FColorSeparator;
  C.MoveTo(R.Left, R.Top+DoScale(FOptSpaceSeparator));
  C.LineTo(R.Left, R.Bottom-DoScale(FOptSpaceSeparator));
end;


function TATTabs.ConvertButtonIdToTabIndex(Id: TATTabButton): integer;
begin
  case Id of
    atbUser0: Result:= cTabIndexUser0;
    atbUser1: Result:= cTabIndexUser1;
    atbUser2: Result:= cTabIndexUser2;
    atbUser3: Result:= cTabIndexUser3;
    atbUser4: Result:= cTabIndexUser4;
    else
      raise Exception.Create('Unknown button id');
  end;
end;

procedure TATTabs.DoPaintUserButtons(C: TCanvas; const AButtons: TATTabButtons; AtLeft: boolean);
var
  BtnId: TATTabButton;
  ElemType: TATTabElemType;
  NIndex, i: integer;
  R: TRect;
begin
  //If we have an OptSpaceInitial > 0 then this "hides" scrolled buttons
  //in that small area before the first userbutton:
  if FOptPosition in [atpTop, atpBottom] then
    if FOptSpaceInitial>0 then
    begin
      R:= Rect(0, 0, DoScale(FOptSpaceInitial), DoScale(FOptTabHeight)+DoScale(FOptSpacer));
      DoPaintBgTo(C, R);
    end;

  for i:= 0 to Length(AButtons)-1 do
  begin
    BtnId:= AButtons[i].Id;
    R:= GetRectOfButtonIndex(i, AtLeft);

    case BtnId of
      atbUser0..atbUser4:
        begin
          NIndex:= ConvertButtonIdToTabIndex(BtnId);

          if FTabIndexOver=NIndex then
            ElemType:= aeButtonUserOver
          else
            ElemType:= aeButtonUser;

          DoPaintBgTo(C, R);
          DoPaintAfter(ElemType, Ord(BtnId)-Ord(atbUser0), C, R);
        end;
      atbSpace:
        begin
          DoPaintBgTo(C, R);
        end;
      atbSeparator:
        begin
          DoPaintSeparator(C, R);
        end
    end;
  end;
end;

procedure TATTabs.Loaded;
begin
  inherited;
  TabIndex:= FTabIndexLoaded;
end;

procedure TATTabs.DoContextPopup(MousePos: TPoint; var Handled: Boolean);
begin
  inherited;
  if not Handled then
  begin
    DoHandleRightClick;
    Handled:= true;
  end;
end;

function TATTabs.GetButtonsWidth(const B: TATTabButtons): integer;
var
  i: integer;
begin
  Result:= 0;
  for i:= 0 to Length(B)-1 do
    Inc(Result, B[i].Size);
end;

function TATTabs.GetButtonsEmpty: boolean;
begin
  Result:=
    (Length(FButtonsLeft)=0) and
    (Length(FButtonsRight)=0);
end;

function TATTabs.GetInitialVerticalIndent: integer;
begin
  if GetButtonsEmpty then
    Result:= DoScale(FOptSpaceInitial)
  else
    Result:= DoScale(FOptTabHeight);
end;

procedure TATTabs.DoPaintColoredBand(C: TCanvas; const ARect: TRect; AColor: TColor;
  APos: TATTabPosition);
var
  NColor: TColor;
  R: TRect;
begin
  case APos of
    atpTop:
      begin
        R.Left:= ARect.Left+1;
        R.Right:= ARect.Right-1;
        R.Top:= ARect.Top+1-Ord(FOptShowFlat);
        R.Bottom:= R.Top+DoScale(FOptColoredBandSize);
      end;
    atpBottom:
      begin
        R.Left:= ARect.Left+1;
        R.Right:= ARect.Right-1;
        R.Bottom:= ARect.Bottom;
        R.Top:= R.Bottom-DoScale(FOptColoredBandSize);
      end;
    atpLeft:
      begin
        R.Left:= ARect.Left+1-Ord(FOptShowFlat);
        R.Right:= R.Left+DoScale(FOptColoredBandSize);
        R.Top:= ARect.Top+1;
        R.Bottom:= ARect.Bottom-1;
      end;
    atpRight:
      begin
        R.Right:= ARect.Right-1+Ord(FOptShowFlat);
        R.Left:= R.Right-DoScale(FOptColoredBandSize);
        R.Top:= ARect.Top+1;
        R.Bottom:= ARect.Bottom-1;
      end;
  end;

  NColor:= C.Brush.Color;
  C.Brush.Color:= AColor;
  C.FillRect(R);
  C.Brush.Color:= NColor;
end;

procedure TATTabs.DoPaintButtonsBG(C: TCanvas);
var
  X1, X2: integer;
begin
  if FOptPosition in [atpLeft, atpRight] then
    if not GetButtonsEmpty then
    begin
      X1:= GetButtonsEdgeCoord(true);
      X2:= GetButtonsEdgeCoord(false);
      DoPaintBgTo(C, Rect(X1, 0, X2, DoScale(FOptTabHeight)));
    end;
end;

procedure TATTabs.UpdateCanvasAntialiasMode(C: TCanvas);
{$ifdef fpc}
begin
  C.AntialiasingMode:= amOn;
end;
{$else}
var
  p: TPoint;
begin
  GetBrushOrgEx(C.Handle, p);
  SetStretchBltMode(C.Handle, HALFTONE);
  SetBrushOrgEx(C.Handle, p.x, p.y, @p);
end;
{$endif}

procedure TATTabs.UpdateTabRectsToFillLine(AIndexFrom, AIndexTo: integer; ALastLine: boolean);
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

procedure TATTabs.UpdateCaptionProps(C: TCanvas; const ACaption: TATTabString;
  out ALineHeight: integer; out ATextSize: TSize);
var
  {$ifdef WIDE}
  StrW: WideString;
  {$endif}
  Ex: TSize;
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
  UpdateTabRects(FBitmap.Canvas);

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
  if not FOptAnimationEnabled then exit;

  Data:= GetTabData(AIndex);
  if Data=nil then exit;

  Enabled:= false;
  FTabIndexAnimated:= AIndex;

  case FOptPosition of
    atpTop,
    atpBottom:
      begin
        for i:= 0 to DoScale(FOptTabHeight) div FOptAnimationStepV-1 do
        begin
          FAnimationOffset:= i*FOptAnimationStepV;
          Invalidate;
          Application.ProcessMessages;
          Sleep(FOptAnimationPause);
        end;
      end;
    else
      begin
        for i:= 0 to DoScale(FOptTabWidthNormal) div FOptAnimationStepH-1 do
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
  if not FOptAnimationEnabled then exit;

  Data:= GetTabData(AIndex);
  if Data=nil then exit;

  Enabled:= false;
  FTabIndexAnimated:= AIndex;

  case FOptPosition of
    atpTop,
    atpBottom:
      begin
        for i:= DoScale(FOptTabHeight) div FOptAnimationStepV-1 downto 0 do
        begin
          FAnimationOffset:= i*FOptAnimationStepV;
          Invalidate;
          Application.ProcessMessages;
          Sleep(FOptAnimationPause);
        end;
      end;
    else
      begin
        for i:= DoScale(FOptTabWidthNormal) div FOptAnimationStepH-1 downto 0 do
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

function TATTabs.GetTabFlatEffective(AIndex: integer): boolean;
begin
  Result:= FOptShowFlat and not (FOptShowFlatMouseOver and (FTabIndexOver=AIndex));
end;

function TATTabs.GetTabBgColor_Passive(AIndex: integer): TColor;
var
  Data: TATTabData;
begin
  if GetTabFlatEffective(AIndex) then
    Result:= FColorBg
  else
  if (FTabIndexOver=AIndex) and not _IsDrag then
    Result:= FColorTabOver
  else
    Result:= FColorTabPassive;

  if FOptShowEntireColor then
  begin
    Data:= GetTabData(AIndex);
    if (FTabIndexOver=AIndex) and not _IsDrag and Assigned(Data) and (Data.TabColorOver<>clNone) then
      Result:= Data.TabColorOver
    else
    if Assigned(Data) and (Data.TabColor<>clNone) then
      Result:= Data.TabColor;
  end;
end;

function TATTabs.GetTabBgColor_Active(AIndex: integer): TColor;
var
  Data: TATTabData;
begin
  if GetTabFlatEffective(AIndex) then
    Result:= FColorBg
  else
    Result:= FColorTabActive;

  if FOptShowEntireColor then
  begin
    Data:= GetTabData(AIndex);
    if Assigned(Data) and (Data.TabColorActive<>clNone) then
      Result:= Data.TabColorActive
    else
    if Assigned(Data) and (Data.TabColor<>clNone) then
      Result:= Data.TabColor;
  end;
end;

function TATTabs.GetPositionInverted(APos: TATTabPosition): TATTabPosition;
begin
  case APos of
    atpTop:
      Result:= atpBottom;
    atpBottom:
      Result:= atpTop;
    atpLeft:
      Result:= atpRight;
    atpRight:
      Result:= atpLeft;
    else
      raise Exception.Create('Unknown tab pos');
  end;
end;

procedure TATTabs.SetTheme(const Data: TATTabTheme);
begin
  FThemed:= false;

  if not FileExists(Data.FileName_Left) then raise Exception.Create('File not found: '+Data.FileName_Left);
  if not FileExists(Data.FileName_Right) then raise Exception.Create('File not found: '+Data.FileName_Right);
  if not FileExists(Data.FileName_Center) then raise Exception.Create('File not found: '+Data.FileName_Center);
  if not FileExists(Data.FileName_LeftActive) then raise Exception.Create('File not found: '+Data.FileName_LeftActive);
  if not FileExists(Data.FileName_RightActive) then raise Exception.Create('File not found: '+Data.FileName_RightActive);
  if not FileExists(Data.FileName_CenterActive) then raise Exception.Create('File not found: '+Data.FileName_CenterActive);
  if not FileExists(Data.FileName_X) then raise Exception.Create('File not found: '+Data.FileName_X);
  if not FileExists(Data.FileName_XActive) then raise Exception.Create('File not found: '+Data.FileName_XActive);
  if not FileExists(Data.FileName_Plus) then raise Exception.Create('File not found: '+Data.FileName_Plus);
  if not FileExists(Data.FileName_PlusActive) then raise Exception.Create('File not found: '+Data.FileName_PlusActive);
  if not FileExists(Data.FileName_ArrowLeft) then raise Exception.Create('File not found: '+Data.FileName_ArrowLeft);
  if not FileExists(Data.FileName_ArrowLeftActive) then raise Exception.Create('File not found: '+Data.FileName_ArrowLeftActive);
  if not FileExists(Data.FileName_ArrowRight) then raise Exception.Create('File not found: '+Data.FileName_ArrowRight);
  if not FileExists(Data.FileName_ArrowRightActive) then raise Exception.Create('File not found: '+Data.FileName_ArrowRightActive);
  if not FileExists(Data.FileName_ArrowDown) then raise Exception.Create('File not found: '+Data.FileName_ArrowDown);
  if not FileExists(Data.FileName_ArrowDownActive) then raise Exception.Create('File not found: '+Data.FileName_ArrowDownActive);

  if FPic_Side_L=nil then FPic_Side_L:= TATTabsPicture.Create;
  if FPic_Side_R=nil then FPic_Side_R:= TATTabsPicture.Create;
  if FPic_Side_C=nil then FPic_Side_C:= TATTabsPicture.Create;
  if FPic_Side_L_a=nil then FPic_Side_L_a:= TATTabsPicture.Create;
  if FPic_Side_R_a=nil then FPic_Side_R_a:= TATTabsPicture.Create;
  if FPic_Side_C_a=nil then FPic_Side_C_a:= TATTabsPicture.Create;
  if FPic_X=nil then FPic_X:= TATTabsPicture.Create;
  if FPic_X_a=nil then FPic_X_a:= TATTabsPicture.Create;
  if FPic_Plus=nil then FPic_Plus:= TATTabsPicture.Create;
  if FPic_Plus_a=nil then FPic_Plus_a:= TATTabsPicture.Create;
  if FPic_Arrow_L=nil then FPic_Arrow_L:= TATTabsPicture.Create;
  if FPic_Arrow_L_a=nil then FPic_Arrow_L_a:= TATTabsPicture.Create;
  if FPic_Arrow_R=nil then FPic_Arrow_R:= TATTabsPicture.Create;
  if FPic_Arrow_R_a=nil then FPic_Arrow_R_a:= TATTabsPicture.Create;
  if FPic_Arrow_D=nil then FPic_Arrow_D:= TATTabsPicture.Create;
  if FPic_Arrow_D_a=nil then FPic_Arrow_D_a:= TATTabsPicture.Create;

  FPic_Side_L.LoadFromFile(Data.FileName_Left);
  FPic_Side_R.LoadFromFile(Data.FileName_Right);
  FPic_Side_C.LoadFromFile(Data.FileName_Center);
  FPic_Side_L_a.LoadFromFile(Data.FileName_LeftActive);
  FPic_Side_R_a.LoadFromFile(Data.FileName_RightActive);
  FPic_Side_C_a.LoadFromFile(Data.FileName_CenterActive);
  FPic_X.LoadFromFile(Data.FileName_X);
  FPic_X_a.LoadFromFile(Data.FileName_XActive);
  FPic_Plus.LoadFromFile(Data.FileName_Plus);
  FPic_Plus_a.LoadFromFile(Data.FileName_PlusActive);
  FPic_Arrow_L.LoadFromFile(Data.FileName_ArrowLeft);
  FPic_Arrow_L_a.LoadFromFile(Data.FileName_ArrowLeftActive);
  FPic_Arrow_R.LoadFromFile(Data.FileName_ArrowRight);
  FPic_Arrow_R_a.LoadFromFile(Data.FileName_ArrowRightActive);
  FPic_Arrow_D.LoadFromFile(Data.FileName_ArrowDown);
  FPic_Arrow_D_a.LoadFromFile(Data.FileName_ArrowDownActive);

  if not (
    (FPic_Side_L.Width=FPic_Side_R.Width) and
    (FPic_Side_L.Width=FPic_Side_L_a.Width) and
    (FPic_Side_L.Width=FPic_Side_R_a.Width) and
    (FPic_Side_L.Height=FPic_Side_R.Height) and
    (FPic_Side_L.Height=FPic_Side_C.Height) and
    (FPic_Side_L.Height=FPic_Side_L_a.Height) and
    (FPic_Side_L.Height=FPic_Side_R_a.Height) and
    (FPic_Side_L.Height=FPic_Side_C_a.Height) and
    (FPic_X.Width=FPic_X.Height) and
    (FPic_X.Width=FPic_X_a.Width) and
    (FPic_X.Width=FPic_X_a.Height)
    ) then
    raise Exception.Create('Incorrect picture sizes in tab-theme');

  FThemed:= true;
  FOptTabHeight:= FPic_Side_L.Height;
  FOptSpaceSide:= FPic_Side_L.Width;
  FOptShowFlat:= false;
  FOptSpaceBetweenTabs:= FOptSpaceSide * Data.SpaceBetweenInPercentsOfSide div 100;
  FOptSpaceXSize:= FPic_X.Width;
  FOptSpaceXRight:= FOptSpaceSide div 2 + Data.IndentOfX;
  FOptShowArrowsNear:= false;
  Height:= FOptTabHeight+FOptSpacer;
end;


end.

