{
ATGroups - several page-controls, each based on ATTabs
Copyright (c) Alexey Torgashin
License: MPL 2.0
}

{$ifdef FPC}
  {$mode delphi}
{$else}
  {$define windows}
  {$define SP} //Allow using SpTBXLib
{$endif}

unit ATGroups;

interface

uses
  Classes, Forms, Types, Controls, Graphics,
  ExtCtrls, Menus,
  {$ifdef SP}
  SpTbxDkPanels, SpTbxItem,
  {$endif}
  ATTabs;

type
  TMySplitter = {$ifdef SP}TSpTbxSplitter{$else}TSplitter{$endif};
  TMyPopupMenu = {$ifdef SP} TSpTbxPopupMenu {$else} TPopupMenu {$endif};

type

  { TATPages }

  TATPages = class(TPanel)
  private
    FTabs: TATTabs;
    FEnabledEmpty: boolean;
    FOnTabFocus: TNotifyEvent;
    FOnTabClose: TATTabCloseEvent;
    FOnTabAdd: TNotifyEvent;
    FOnTabEmpty: TNotifyEvent;
    FOnTabOver: TATTabOverEvent;
    FOnTabMove: TATTabMoveEvent;
    procedure SetOnTabClose(AEvent: TATTabCloseEvent);
    procedure SetOnTabAdd(AEvent: TNotifyEvent);
    procedure TabClick(Sender: TObject);
    procedure TabDrawBefore(Sender: TObject;
      AType: TATTabElemType; ATabIndex: Integer;
      C: TCanvas; const ARect: TRect; var ACanDraw: boolean);
    procedure TabEmpty(Sender: TObject);
    procedure TabOver(Sender: TObject; ATabIndex: Integer);
    procedure TabMove(Sender: TObject; NFrom, NTo: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    procedure AddTab(AControl: TControl; const ACaption: atString;
      AModified: boolean; AColor: TColor = clNone);
    property Tabs: TATTabs read FTabs;
    property EnabledEmpty: boolean read FEnabledEmpty write FEnabledEmpty;
    property OnTabFocus: TNotifyEvent read FOnTabFocus write FOnTabFocus;
    property OnTabClose: TATTabCloseEvent read FOnTabClose write SetOnTabClose;
    property OnTabAdd: TNotifyEvent read FOnTabAdd write SetOnTabAdd;
    property OnTabEmpty: TNotifyEvent read FOnTabEmpty write FOnTabEmpty;
    property OnTabOver: TATTabOverEvent read FOnTabOver write FOnTabOver;
    property OnTabMove: TATTabMoveEvent read FOnTabMove write FOnTabMove;
  end;

type
  TATTabCloseId = (
    tabCloseCurrent,
    tabCloseOthersThisPage,
    tabCloseOthersAllPages,
    tabCloseLefterThisPage,
    tabCloseRighterThisPage,
    tabCloseAllThisPage,
    tabCloseAll
    );
type
  TATTabsOptionId = (
    tabColorText,
    tabColorTextModified,
    tabColorBg,
    tabColorBgActive,
    tabColorBgPassive,
    tabColorBgPassiveOver,
    tabColorBorderActive,
    tabColorBorderPassive,
    tabColorCloseBg,
    tabColorCloseBgOver,
    tabColorCloseBorderOver,
    tabColorCloseX,
    tabColorCloseXOver,
    tabColorArrow,
    tabColorArrowOver,
    tabOptionFontSize,
    tabOptionAngle,
    tabOptionBottomTabs,
    tabOptionShowTabs,
    tabOptionShowXButtons,
    tabOptionShowPlus,
    tabOptionShowNums,
    tabOptionShowEntireColor,
    tabOptionDoubleClickClose,
    tabOptionDragDrop,
    tabOptionHeight,
    tabOptionHeightInner,
    tabOptionWidthMin,
    tabOptionWidthMax,
    tabOptionIndentTop,
    tabOptionIndentInit,
    tabOptionIndentInter,
    tabOptionIndentColor,
    tabOptionIndentXRight,
    tabOptionIndentXSize,
    tabOptionWidecharModified
    );

type
  TATGroupsMode = (
    gmNone,
    gmOne,
    gm2Horz,
    gm2Vert,
    gm3Horz,
    gm3Vert,
    gm1plus2Vert,
    gm1plus2Horz,
    gm4Horz,
    gm4Vert,
    gm4Grid,
    gm6Grid
    );

type
  TATGroupsNums = 1..6;

type

  { TATGroups }

  TATGroups = class(TPanel)
  private
    FSplit1,
    FSplit2,
    FSplit3,
    FSplit4,
    FSplit5: TMySplitter;
    FPanel1,
    FPanel2: TPanel;
    FPos1,
    FPos2,
    FPos3,
    FPos4,
    FPos5: Real;
    FPrevWidth,
    FPrevHeight: Integer;
    FSplitPopup: TMyPopupMenu;
    FMode: TATGroupsMode;
    FOnTabPopup: TNotifyEvent;
    FOnTabFocus: TNotifyEvent;
    FOnTabClose: TATTabCloseEvent;
    FOnTabAdd: TNotifyEvent;
    FOnTabOver: TATTabOverEvent;
    FOnTabMove: TATTabMoveEvent;
    FPopupPages: TATPages;
    FPopupTabIndex: Integer;
    procedure TabFocus(Sender: TObject);
    procedure TabEmpty(Sender: TObject);
    procedure TabPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
    procedure TabClose(Sender: TObject; ATabIndex: Integer;
      var ACanClose, ACanContinue: boolean);
    procedure TabAdd(Sender: TObject);
    procedure TabOver(Sender: TObject; ATabIndex: Integer);
    procedure TabMove(Sender: TObject; NFrom, NTo: Integer);
    procedure SetMode(Value: TATGroupsMode);
    function GetSplitPos: Integer;
    procedure SetSplitPos(N: Integer);
    procedure Split1Moved(Sender: TObject);
    procedure Split2Moved(Sender: TObject);
    procedure Split3Moved(Sender: TObject);
    procedure Split4Moved(Sender: TObject);
    procedure Split5Moved(Sender: TObject);
    procedure SplitClick(Sender: TObject);
    procedure InitSplitterPopup;
    procedure MoveTabsOnModeChanging(Value: TATGroupsMode);
  protected
    procedure Resize; override;
  public
    Pages1,
    Pages2,
    Pages3,
    Pages4,
    Pages5,
    Pages6,
    PagesCurrent: TATPages;
    Pages: array[TATGroupsNums] of TATPages;
    //
    property Panel1: TPanel read FPanel1;
    property Splitter1: TMySplitter read FSplit1;
    property Splitter2: TMySplitter read FSplit2;
    property Splitter3: TMySplitter read FSplit3;
    property Splitter4: TMySplitter read FSplit4;
    property Splitter5: TMySplitter read FSplit5;
    //
    constructor Create(AOwner: TComponent); override;
    //
    function PagesVisibleCount: Integer;
    function PagesSetIndex(ANum: Integer): boolean;
    procedure PagesSetNext(ANext: boolean);
    function PagesIndexOf(APages: TATPages): Integer;
    function PagesIndexOfControl(ACtl: TControl): Integer;
    function PagesNextIndex(AIndex: Integer; ANext: boolean; AEnableEmpty: boolean): Integer;
    procedure PagesAndTabIndexOfControl(AObject: TObject; var NPages, NTab: Integer);
    //
    property PopupPages: TATPages read FPopupPages write FPopupPages;
    property PopupTabIndex: Integer read FPopupTabIndex write FPopupTabIndex;
    property SplitterPopupMenu: TMyPopupMenu read FSplitPopup;
    //
    property Mode: TATGroupsMode read FMode write SetMode;
    function GetTabTotalCount: Integer;
    function GetTabDataOfTotalIndex(N: Integer): TATTabData;
    function SetPagesAndTabIndex(APageIndex, ATabIndex: Integer): boolean;
    procedure SetTabOption(Id: TATTabsOptionId; N: Integer);
    procedure SetTabFont(AFont: TFont);
    //
    function CloseTabsOther(APages: TATPages; ATabIndex: Integer;
      ADoRighter, ADoLefter: boolean): boolean;
    function CloseTabsAll(APages: TATPages): boolean;
    function CloseTabs(Id: TATTabCloseId; AForPopupMenu: boolean): boolean;
    //
    procedure MoveTab(AFromPages: TATPages; AFromIndex: Integer;
      AToPages: TATPages; AToIndex: Integer; AActivateTabAfter: boolean);
    procedure MovePopupTabToNext(ANext: boolean);
    procedure MoveCurrentTabToNext(ANext: boolean);
    procedure MoveCurrentTabToOpposite;
    //
    property SplitPos: Integer read GetSplitPos write SetSplitPos;
    procedure SplitPosIncrease;
    procedure SplitPosDecrease;
    procedure SaveSplitPos;
    procedure RestoreSplitPos;
    //
    property OnTabPopup: TNotifyEvent read FOnTabPopup write FOnTabPopup;
    property OnTabFocus: TNotifyEvent read FOnTabFocus write FOnTabFocus;
    property OnTabClose: TATTabCloseEvent read FOnTabClose write FOnTabClose;
    property OnTabAdd: TNotifyEvent read FOnTabAdd write FOnTabAdd;
    property OnTabOver: TATTabOverEvent read FOnTabOver write FOnTabOver;
    property OnTabMove: TATTabMoveEvent read FOnTabMove write FOnTabMove;
  end;

function PtInControl(Control: TControl; const ScreenPnt: TPoint): boolean;
procedure DoControlLock(Ctl: TWinControl);
procedure DoControlUnlock(Ctl: TWinControl);


implementation

uses
  {$ifdef windows}
  Windows, Messages,
  {$endif}
  SysUtils, StrUtils,
  {$ifdef SP}
  SpTbxSkins,
  {$endif}
  Math, Dialogs;

const
  cAbsMin = 4;

procedure UpdW(C: TControl; Value: Integer);
begin
  if C.Align<>alClient then
    if Value>cAbsMin then
    begin
      C.Width:= Value;
    end;
end;

procedure UpdH(C: TControl; Value: Integer);
begin
  if C.Align<>alClient then
    if Value>cAbsMin then
    begin
      C.Height:= Value;
    end;
end;

function PtInControl(Control: TControl; const ScreenPnt: TPoint): boolean;
begin
  Result:= PtInRect(Control.ClientRect, Control.ScreenToClient(ScreenPnt));
end;

procedure DoControlLock(Ctl: TWinControl);
begin
  {$ifdef windows}
  Ctl.Perform(WM_SetRedraw, 0, 0);
  {$endif}
end;

procedure DoControlUnlock(Ctl: TWinControl);
begin
  {$ifdef windows}
  Ctl.Perform(WM_SetRedraw, 1, 0);
  SetWindowPos(Ctl.Handle, 0, 0, 0, 0, 0,
    SWP_FRAMECHANGED or SWP_NOCOPYBITS or SWP_NOMOVE or SWP_NOZORDER or SWP_NOSIZE);
  {$endif}
end;


const
  cGroupsCount: array[TATGroupsMode] of Integer = (
    1,
    1,
    2,
    2,
    3,
    3,
    3,
    3,
    4,
    4,
    4,
    6
    );

{ TATPages }

constructor TATPages.Create(AOwner: TComponent);
begin
  inherited;

  Caption:= '';
  BorderStyle:= bsNone;
  BevelInner:= bvNone;
  BevelOuter:= bvNone;
  FEnabledEmpty:= true;

  FTabs:= TATTabs.Create(Self);
  FTabs.Parent:= Self;
  FTabs.Align:= alTop;
  FTabs.OnTabClick:= TabClick;
  FTabs.OnTabDrawBefore:= TabDrawBefore;
  FTabs.OnTabEmpty:= TabEmpty;
  FTabs.OnTabOver:= TabOver;
  FTabs.OnTabMove:= TabMove;
  FTabs.DragMode:= dmAutomatic; //allow DnD between groups

  FTabs.TabAngle:= 0;
  FTabs.TabHeight:= 24;
  FTabs.TabIndentTop:= 1;
  FTabs.TabIndentInter:= 0;
  FTabs.TabIndentXSize:= 14;
  FTabs.TabIndentColor:= 5;
  FTabs.TabWidthMin:= 18; 
  FTabs.Height:= FTabs.TabHeight+FTabs.TabIndentTop+1;

  FTabs.TabShowModifiedText:= #$95;
  FTabs.TabMiddleClickClose:= true;
  FTabs.TabDoubleClickPlus:= true;

  FTabs.ColorBg:= clWindow;
  FTabs.ColorCloseX:= clDkGray;
end;

procedure TATPages.AddTab(AControl: TControl;
  const ACaption: atString; AModified: boolean; AColor: TColor);
begin
  FTabs.AddTab(-1, ACaption, AControl, AModified, AColor);
  AControl.Parent:= Self;
  AControl.Align:= alClient;
  FTabs.TabIndex:= FTabs.TabCount-1;
end;

procedure TATPages.TabClick(Sender: TObject);
var
  i: Integer;
  D: TATTabData;
  Ctl: TWinControl;
begin
  DoControlLock(Self);
  try
    for i:= 0 to FTabs.TabCount-1 do
    begin
      D:= FTabs.GetTabData(i);
      if D<>nil then
      begin
        Ctl:= D.TabObject as TWinControl;
        Ctl.Visible:= i=FTabs.TabIndex;
      end;
    end;
  finally
    DoControlUnlock(Self);
  end;

  D:= FTabs.GetTabData(FTabs.TabIndex);
  if D<>nil then
  begin
    Ctl:= D.TabObject as TWinControl;
    if Ctl.Showing then
      if Assigned(FOnTabFocus) then
        FOnTabFocus(FTabs);
  end;
end;

procedure TATPages.SetOnTabClose(AEvent: TATTabCloseEvent);
begin
  FOnTabClose:= AEvent;
  FTabs.OnTabClose:= AEvent;
end;

procedure TATPages.SetOnTabAdd(AEvent: TNotifyEvent);
begin
  FOnTabAdd:= AEvent;
  FTabs.OnTabPlusClick:= AEvent;
end;

procedure TATPages.TabEmpty(Sender: TObject);
begin
  if Assigned(FOnTabEmpty) then
    FOnTabEmpty(Sender);
end;

procedure TATPages.TabOver(Sender: TObject; ATabIndex: Integer);
begin
  if Assigned(FOnTabOver) then
    FOnTabOver(Sender, ATabIndex);
end;

procedure TATPages.TabMove(Sender: TObject; NFrom, NTo: Integer);
begin
  if Assigned(FOnTabMove) then
    FOnTabMove(Sender, NFrom, NTo);
end;

{ TATGroups }

constructor TATGroups.Create(AOwner: TComponent);
const
  cMinSize = 60;
var
  i: Integer;
begin
  inherited;

  Caption:= '';
  BorderStyle:= bsNone;
  BevelInner:= bvNone;
  BevelOuter:= bvNone;

  Pages1:= TATPages.Create(Self);
  Pages2:= TATPages.Create(Self);
  Pages3:= TATPages.Create(Self);
  Pages4:= TATPages.Create(Self);
  Pages5:= TATPages.Create(Self);
  Pages6:= TATPages.Create(Self);

  Pages1.EnabledEmpty:= false;
  PagesCurrent:= Pages1;
  Pages[1]:= Pages1;
  Pages[2]:= Pages2;
  Pages[3]:= Pages3;
  Pages[4]:= Pages4;
  Pages[5]:= Pages5;
  Pages[6]:= Pages6;

  for i:= Low(Pages) to High(Pages) do
    with Pages[i] do
    begin
      Visible:= i=Low(Pages);
      Name:= 'aPages'+IntToStr(i); //debug
      Caption:= '';
      Tabs.Name:= 'aPagesTabs'+IntToStr(i); //debug
      //
      Parent:= Self;
      Align:= alLeft;
      //
      OnContextPopup:= Self.TabPopup;
      OnTabEmpty:= Self.TabEmpty;
      OnTabFocus:= Self.TabFocus;
      OnTabClose:= Self.TabClose;
      OnTabAdd:= Self.TabAdd;
      OnTabOver:= Self.TabOver;
      OnTabMove:= Self.TabMove;
    end;

  FSplit1:= TMySplitter.Create(Self);
  FSplit1.Parent:= Self;
  FSplit1.OnMoved:= Split1Moved;
  FSplit1.MinSize:= cMinSize;

  FSplit2:= TMySplitter.Create(Self);
  FSplit2.Parent:= Self;
  FSplit2.OnMoved:= Split2Moved;
  FSplit2.MinSize:= cMinSize;

  FSplit3:= TMySplitter.Create(Self);
  FSplit3.Parent:= Self;
  FSplit3.OnMoved:= Split3Moved;
  FSplit3.MinSize:= cMinSize;

  FSplit4:= TMySplitter.Create(Self);
  FSplit4.Parent:= Self;
  FSplit4.OnMoved:= Split4Moved;
  FSplit4.MinSize:= cMinSize;

  FSplit5:= TMySplitter.Create(Self);
  FSplit5.Parent:= Self;
  FSplit5.OnMoved:= Split5Moved;
  FSplit5.MinSize:= cMinSize;

  FSplit1.ResizeStyle:= rsPattern;
  FSplit2.ResizeStyle:= rsPattern;
  FSplit3.ResizeStyle:= rsPattern;
  FSplit4.ResizeStyle:= rsPattern;
  FSplit5.ResizeStyle:= rsPattern;
  
  {$ifdef fpc}
  FSplit1.AutoSnap:= false;
  FSplit2.AutoSnap:= false;
  FSplit3.AutoSnap:= false;
  FSplit4.AutoSnap:= false;
  FSplit5.AutoSnap:= false;
  {$endif}

  FPanel1:= TPanel.Create(Self);
  FPanel1.Parent:= Self;
  FPanel1.Align:= alTop;
  FPanel1.Caption:= '';
  FPanel1.BorderStyle:= bsNone;
  FPanel1.BevelInner:= bvNone;
  FPanel1.BevelOuter:= bvNone;
  FPanel1.Visible:= false;

  FPanel2:= TPanel.Create(Self);
  FPanel2.Parent:= Self;
  FPanel2.Align:= alClient;
  FPanel2.Caption:= '';
  FPanel2.BorderStyle:= bsNone;
  FPanel2.BevelInner:= bvNone;
  FPanel2.BevelOuter:= bvNone;
  FPanel2.Visible:= false;

  InitSplitterPopup;
  FPopupPages:= nil;
  FPopupTabIndex:= -1;
  FMode:= gmNone;
end;

procedure TATGroups.InitSplitterPopup;
  //
  procedure Add(N: Integer);
  var
    MI: {$ifdef SP}TSpTbxItem{$else}TMenuItem{$endif};
  begin
    MI:= {$ifdef SP}TSpTbxItem{$else}TMenuItem{$endif}.Create(Self);
    MI.Caption:= Format('%d/%d', [N, 100-N]);
    MI.Tag:= N;
    MI.OnClick:= SplitClick;
    FSplitPopup.Items.Add(MI);
  end;
  //
begin
  FSplitPopup:= TMyPopupMenu.Create(Self);
  Add(20);
  Add(30);
  Add(40);
  Add(50);
  Add(60);
  Add(70);
  Add(80);
end;

type
  TControlHack = class(TSplitter);

procedure SetSplitterPopup(ASplitter: TMySplitter; APopup: TPopupMenu);
begin
  {$ifdef SP}
  ASplitter.PopupMenu:= APopup;
  {$else}
  TControlHack(ASplitter).PopupMenu:= APopup;
  {$endif}
end;

procedure TATGroups.MoveTabsOnModeChanging(Value: TATGroupsMode);
var
  NCountBefore, NCountAfter: Integer;
  i, j: Integer;
begin
  NCountBefore:= cGroupsCount[FMode];
  NCountAfter:= cGroupsCount[Value];

  for i:= NCountAfter+1 to NCountBefore do
    for j:= 0 to Pages[i].Tabs.TabCount-1 do
      MoveTab(Pages[i], 0{first tab}, Pages[NCountAfter], -1, false);
end;

procedure TATGroups.SetMode(Value: TATGroupsMode);
var
  NSplit: Real;
  NPagesBefore, NPagesAfter: Integer;
  i: Integer;
begin
  if Value<>FMode then
  try
    DoControlLock(Self);

    //actions before changing FMode
    NPagesBefore:= PagesIndexOf(PagesCurrent);
    MoveTabsOnModeChanging(Value);

    case FMode of
      gm2Horz:
        NSplit:= Pages1.Width / ClientWidth;
      gm2Vert:
        NSplit:= Pages1.Height / ClientHeight;
      else
        NSplit:= 0.5;
    end;

    //changing FMode and actions after changing
    FMode:= Value;

    SetSplitterPopup(FSplit1, nil);
    SetSplitterPopup(FSplit3, nil);
    case FMode of
      gm2Horz, gm2Vert:
        SetSplitterPopup(FSplit1, FSplitPopup);
      gm1plus2Vert, gm1plus2Horz:
        SetSplitterPopup(FSplit3, FSplitPopup);
    end;

    for i:= Low(Pages) to High(Pages) do
      Pages[i].Visible:= i<=cGroupsCount[FMode];

    case FMode of
      gm1plus2Vert:
      begin
        FPanel1.Visible:= true;
        FPanel2.Visible:= true;
        Pages1.Parent:= FPanel1;
        Pages2.Parent:= FPanel2;
        Pages3.Parent:= FPanel2;
        Pages4.Parent:= FPanel2;
        Pages5.Parent:= FPanel2;
        Pages6.Parent:= FPanel2;
        FSplit1.Parent:= FPanel1;
        FSplit2.Parent:= FPanel2;
        FSplit3.Parent:= Self;
        FSplit4.Parent:= Self;
        FSplit5.Parent:= Self;
        //
        FPanel1.Align:= alLeft;
      end;
      gm1plus2Horz:
      begin
        FPanel1.Visible:= true;
        FPanel2.Visible:= true;
        Pages1.Parent:= FPanel1;
        Pages2.Parent:= FPanel2;
        Pages3.Parent:= FPanel2;
        Pages4.Parent:= FPanel2;
        Pages5.Parent:= FPanel2;
        Pages6.Parent:= FPanel2;
        FSplit1.Parent:= FPanel1;
        FSplit2.Parent:= FPanel2;
        FSplit3.Parent:= Self;
        FSplit4.Parent:= Self;
        FSplit5.Parent:= Self;
        //
        FPanel1.Align:= alTop;
      end;
      gm4Grid:
      begin
        FPanel1.Visible:= true;
        FPanel2.Visible:= true;
        Pages1.Parent:= FPanel1;
        Pages2.Parent:= FPanel1;
        Pages3.Parent:= FPanel2;
        Pages4.Parent:= FPanel2;
        Pages5.Parent:= FPanel2;
        Pages6.Parent:= FPanel2;
        FSplit1.Parent:= FPanel1;
        FSplit2.Parent:= FPanel2;
        FSplit3.Parent:= Self;
        FSplit4.Parent:= Self;
        FSplit5.Parent:= Self;
        //
        FPanel1.Align:= alTop;
      end;
      gm6Grid:
      begin
        FPanel1.Visible:= true;
        FPanel2.Visible:= true;
        Pages1.Parent:= FPanel1;
        Pages2.Parent:= FPanel1;
        Pages3.Parent:= FPanel1;
        Pages4.Parent:= FPanel2;
        Pages5.Parent:= FPanel2;
        Pages6.Parent:= FPanel2;
        FSplit1.Parent:= FPanel1;
        FSplit2.Parent:= FPanel1;
        FSplit3.Parent:= Self;
        FSplit4.Parent:= FPanel2;
        FSplit5.Parent:= FPanel2;
        //
        FPanel1.Align:= alTop;
      end
      else
      begin
        FPanel1.Visible:= false;
        FPanel2.Visible:= false;
        Pages1.Parent:= Self;
        Pages2.Parent:= Self;
        Pages3.Parent:= Self;
        Pages4.Parent:= Self;
        Pages5.Parent:= Self;
        Pages6.Parent:= Self;
        FSplit1.Parent:= Self;
        FSplit2.Parent:= Self;
        FSplit3.Parent:= Self;
        FSplit4.Parent:= Self;
        FSplit5.Parent:= Self;
      end;
    end;

    case FMode of
      gmOne:
        begin
          FSplit1.Visible:= false;
          FSplit2.Visible:= false;
          FSplit3.Visible:= false;
          FSplit4.Visible:= false;
          FSplit5.Visible:= false;
          Pages1.Align:= alClient;
        end;
      gm2Horz:
        begin
          FSplit1.Visible:= true;
          FSplit2.Visible:= false;
          FSplit3.Visible:= false;
          FSplit4.Visible:= false;
          FSplit5.Visible:= false;
          Pages1.Align:= alLeft;
          Pages2.Align:= alClient;
          FSplit1.Align:= alLeft;
          //size
          UpdW(Pages1, Trunc(ClientWidth * NSplit));
          //pos
          FSplit1.Left:= ClientWidth;
          Pages2.Left:= ClientWidth;
        end;
      gm2Vert:
        begin
          FSplit1.Visible:= true;
          FSplit2.Visible:= false;
          FSplit3.Visible:= false;
          FSplit4.Visible:= false;
          FSplit5.Visible:= false;
          Pages1.Align:= alTop;
          Pages2.Align:= alClient;
          FSplit1.Align:= alTop;
          //size
          UpdH(Pages1, Trunc(ClientHeight * NSplit));
          //pos
          FSplit1.Top:= ClientHeight;
          Pages2.Top:= ClientHeight;
        end;
      gm3Horz:
        begin
          FSplit1.Visible:= true;
          FSplit2.Visible:= true;
          FSplit3.Visible:= false;
          FSplit4.Visible:= false;
          FSplit5.Visible:= false;
          Pages1.Align:= alLeft;
          Pages2.Align:= alLeft;
          Pages3.Align:= alClient;
          FSplit1.Align:= alLeft;
          FSplit2.Align:= alLeft;
          //size
          UpdW(Pages1, ClientWidth div 3);
          UpdW(Pages2, ClientWidth div 3);
          //pos
          FSplit1.Left:= ClientWidth;
          Pages2.Left:= ClientWidth;
          FSplit2.Left:= ClientWidth;
          Pages3.Left:= ClientWidth;
        end;
      gm3Vert:
        begin
          FSplit1.Visible:= true;
          FSplit2.Visible:= true;
          FSplit3.Visible:= false;
          FSplit4.Visible:= false;
          FSplit5.Visible:= false;
          Pages1.Align:= alTop;
          Pages2.Align:= alTop;
          Pages3.Align:= alClient;
          FSplit1.Align:= alTop;
          FSplit2.Align:= alTop;
          //size
          UpdH(Pages1, ClientHeight div 3);
          UpdH(Pages2, ClientHeight div 3);
          //pos
          FSplit1.Top:= ClientHeight;
          Pages2.Top:= ClientHeight;
          FSplit2.Top:= ClientHeight;
          Pages3.Top:= ClientHeight;
        end;
      gm4Horz:
        begin
          FSplit1.Visible:= true;
          FSplit2.Visible:= true;
          FSplit3.Visible:= true;
          FSplit4.Visible:= false;
          FSplit5.Visible:= false;
          Pages1.Align:= alLeft;
          Pages2.Align:= alLeft;
          Pages3.Align:= alLeft;
          Pages4.Align:= alClient;
          FSplit1.Align:= alLeft;
          FSplit2.Align:= alLeft;
          FSplit3.Align:= alLeft;
          //size
          UpdW(Pages1, ClientWidth div 4);
          UpdW(Pages2, ClientWidth div 4);
          UpdW(Pages3, ClientWidth div 4);
          //pos
          FSplit1.Left:= ClientWidth;
          Pages2.Left:= ClientWidth;
          FSplit2.Left:= ClientWidth;
          Pages3.Left:= ClientWidth;
          FSplit3.Left:= ClientWidth;
          Pages4.Left:= ClientWidth;
        end;
      gm4Vert:
        begin
          FSplit1.Visible:= true;
          FSplit2.Visible:= true;
          FSplit3.Visible:= true;
          FSplit4.Visible:= false;
          FSplit5.Visible:= false;
          Pages1.Align:= alTop;
          Pages2.Align:= alTop;
          Pages3.Align:= alTop;
          Pages4.Align:= alClient;
          FSplit1.Align:= alTop;
          FSplit2.Align:= alTop;
          FSplit3.Align:= alTop;
          //size
          UpdH(Pages1, ClientHeight div 4);
          UpdH(Pages2, ClientHeight div 4);
          UpdH(Pages3, ClientHeight div 4);
          //pos
          FSplit1.Top:= ClientHeight;
          Pages2.Top:= ClientHeight;
          FSplit2.Top:= ClientHeight;
          Pages3.Top:= ClientHeight;
          FSplit3.Top:= ClientHeight;
          Pages4.Top:= ClientHeight;
        end;
      gm4Grid:
        begin
          FSplit1.Visible:= true;
          FSplit2.Visible:= true;
          FSplit3.Visible:= true;
          FSplit4.Visible:= false;
          FSplit5.Visible:= false;
          Pages1.Align:= alLeft;
          Pages2.Align:= alClient;
          Pages3.Align:= alLeft;
          Pages4.Align:= alClient;
          FSplit1.Align:= alLeft;
          FSplit2.Align:= alLeft;
          FSplit3.Align:= alTop;
          //size
          UpdW(Pages1, ClientWidth div 2);
          UpdW(Pages3, ClientWidth div 2);
          UpdH(FPanel1, ClientHeight div 2);
          //pos-a
          FSplit1.Left:= ClientWidth;
          Pages2.Left:= ClientWidth;
          //pos-b
          FSplit2.Left:= ClientWidth;
          Pages4.Left:= ClientWidth;
          //pos-c
          FSplit3.Top:= ClientHeight;
          FPanel2.Top:= ClientHeight;
        end;
      gm1plus2Vert:
        begin
          FSplit1.Visible:= false;
          FSplit2.Visible:= true;
          FSplit3.Visible:= true;
          FSplit4.Visible:= false;
          FSplit5.Visible:= false;
          Pages1.Align:= alClient; //pages1 on panel1
          Pages2.Align:= alTop; //pages2 on panel2
          Pages3.Align:= alClient;
          Pages4.Align:= alBottom;
          FSplit1.Align:= alTop;
          FSplit2.Align:= alTop;
          FSplit3.Align:= alLeft;
          //size
          UpdH(Pages2, ClientHeight div 2);
          UpdW(FPanel1, ClientWidth div 2);
          //pos-b
          FSplit2.Top:= ClientHeight;
          Pages4.Top:= ClientHeight;
          //pos-c
          FSplit3.Left:= ClientWidth;
          FPanel2.Left:= ClientWidth;
        end;
      gm1plus2Horz:
        begin
          FSplit1.Visible:= false;
          FSplit2.Visible:= true;
          FSplit3.Visible:= true;
          FSplit4.Visible:= false;
          FSplit5.Visible:= false;
          Pages1.Align:= alClient; //pages1 on panel1
          Pages2.Align:= alLeft; //pages2 on panel2
          Pages3.Align:= alClient;
          Pages4.Align:= alRight;
          FSplit1.Align:= alLeft;
          FSplit2.Align:= alLeft;
          FSplit3.Align:= alTop;
          //size
          UpdW(Pages2, ClientWidth div 2);
          UpdH(FPanel1, ClientHeight div 2);
          //pos-b
          FSplit2.Left:= ClientWidth;
          Pages4.Left:= ClientWidth;
          //pos-c
          FSplit3.Top:= ClientHeight;
          FPanel2.Top:= ClientHeight;
        end;
      gm6Grid:
        begin
          FSplit1.Visible:= true;
          FSplit2.Visible:= true;
          FSplit3.Visible:= true;
          FSplit4.Visible:= true;
          FSplit5.Visible:= true;
          Pages1.Align:= alLeft;
          Pages2.Align:= alLeft;
          Pages3.Align:= alClient;
          Pages4.Align:= alLeft;
          Pages5.Align:= alLeft;
          Pages6.Align:= alClient;
          FSplit1.Align:= alLeft;
          FSplit2.Align:= alLeft;
          FSplit3.Align:= alTop;
          FSplit4.Align:= alLeft;
          FSplit5.Align:= alLeft;
          //size
          UpdW(Pages1, ClientWidth div 3);
          UpdW(Pages2, ClientWidth div 3);
          UpdW(Pages4, ClientWidth div 3);
          UpdW(Pages5, ClientWidth div 3);
          UpdH(FPanel1, ClientHeight div 2);
          //pos-a
          FSplit1.Left:= ClientWidth;
          Pages2.Left:= ClientWidth;
          FSplit2.Left:= ClientWidth;
          Pages3.Left:= ClientWidth;
          //pos-b
          FSplit4.Left:= ClientWidth;
          Pages5.Left:= ClientWidth;
          FSplit5.Left:= ClientWidth;
          Pages6.Left:= ClientWidth;
          //pos-c
          FSplit3.Top:= ClientHeight;
          FPanel2.Top:= ClientHeight;
        end;
    end;

    SaveSplitPos;

    //focus same group, if possible
    NPagesAfter:= Min(NPagesBefore, cGroupsCount[FMode]);
    if Assigned(FOnTabFocus) then
      FOnTabFocus(Pages[NPagesAfter].Tabs);
  finally
    DoControlUnlock(Self);
  end;
end;

procedure TATGroups.Split1Moved(Sender: TObject);
begin
  if FMode=gm4Grid then
    UpdW(Pages3, Pages1.Width);
  if FMode=gm6Grid then
    UpdW(Pages4, Pages1.Width);

  SaveSplitPos;
end;

procedure TATGroups.Split2Moved(Sender: TObject);
begin
  if FMode=gm4Grid then
    UpdW(Pages1, Pages3.Width);
  if FMode=gm6Grid then
    UpdW(Pages5, Pages2.Width);

  SaveSplitPos;
end;

procedure TATGroups.Split3Moved(Sender: TObject);
begin
  SaveSplitPos;
end;

procedure TATGroups.Split4Moved(Sender: TObject);
begin
  if FMode=gm6Grid then
    UpdW(Pages1, Pages4.Width);

  SaveSplitPos;
end;

procedure TATGroups.Split5Moved(Sender: TObject);
begin
  if FMode=gm6Grid then
    UpdW(Pages2, Pages5.Width);

  SaveSplitPos;
end;

procedure TATGroups.TabPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
var
  Pnt, PntC: TPoint;
  i: Integer;
begin
  FPopupPages:= nil;
  FPopupTabIndex:= -1;

  Pnt:= (Sender as TControl).ClientToScreen(MousePos);
  for i:= Low(Pages) to High(Pages) do
    if PtInControl(Pages[i].Tabs, Pnt) then
    begin
      FPopupPages:= Pages[i];
      Break
    end;
  if FPopupPages=nil then Exit;

  PntC:= PopupPages.Tabs.ScreenToClient(Pnt);
  FPopupTabIndex:= FPopupPages.Tabs.GetTabAt(PntC.X, PntC.Y);

  if Assigned(FOnTabPopup) then
    FOnTabPopup(Self);
  Handled:= true;
end;

procedure TATPages.TabDrawBefore(Sender: TObject;
  AType: TATTabElemType; ATabIndex: Integer;
  C: TCanvas; const ARect: TRect; var ACanDraw: boolean);
begin
  {$ifndef SP}
  ACanDraw:= true;
  {$else}
  case AType of
    aeBackground:
    begin
      if SkinManager.CurrentSkinName='Default' then
      begin
        C.Brush.Color:= clBtnFace;
        C.FillRect(ARect);
      end
      else
        CurrentSkin.PaintBackground(C, ARect,
          skncDock, sknsNormal, true{BG}, false{Borders});
      ACanDraw:= false;
    end;

    aeXButton:
    begin
      //if ATabMouseOver then
      //  SpDrawXPToolbarButton(Control.Canvas, R, sknsHotTrack, cpNone);
      SpDrawGlyphPattern(C, ARect, 0{0 is X icon index},
        CurrentSkin.GetTextColor(skncToolbarItem, sknsNormal));
      ACanDraw:= false;
    end;

    aeXButtonOver:
    begin
      SpDrawXPToolbarButton(C,
        Rect(ARect.Left-1, ARect.Top-1, ARect.Right, ARect.Bottom),
        sknsHotTrack, cpNone);
      SpDrawGlyphPattern(C, ARect, 0{0 is X icon index},
        CurrentSkin.GetTextColor(skncToolbarItem, sknsNormal));
      ACanDraw:= false;
    end;
  end;
  {$endif}
end;

procedure TATGroups.SaveSplitPos;
begin
  if ClientWidth<=0 then Exit;
  if ClientHeight<=0 then Exit;

  FPos1:= 0;
  FPos2:= 0;
  FPos3:= 0;
  FPos4:= 0;
  FPos5:= 0;

  case FMode of
    gm2Horz,
    gm3Horz,
    gm4Horz:
      begin
        FPos1:= Pages1.Width / ClientWidth;
        FPos2:= Pages2.Width / ClientWidth;
        FPos3:= Pages3.Width / ClientWidth;
      end;
    gm2Vert,
    gm3Vert,
    gm4Vert:
      begin
        FPos1:= Pages1.Height / ClientHeight;
        FPos2:= Pages2.Height / ClientHeight;
        FPos3:= Pages3.Height / ClientHeight;
      end;
    gm1plus2Vert:
      begin
        FPos1:= FPanel1.Width / ClientWidth;
        FPos2:= Pages2.Height / ClientHeight;
      end;
    gm1plus2Horz:
      begin
        FPos1:= FPanel1.Height / ClientHeight;
        FPos2:= Pages2.Width / ClientWidth;
      end;
    gm4Grid:
      begin
        FPos1:= Pages1.Width / ClientWidth;
        FPos2:= Pages3.Width / ClientWidth;
        FPos3:= FPanel1.Height / ClientHeight;
      end;
    gm6Grid:
      begin
        FPos1:= Pages1.Width / ClientWidth;
        FPos2:= Pages2.Width / ClientWidth;
        FPos3:= FPanel1.Height / ClientHeight;
        FPos4:= Pages4.Width / ClientWidth;
        FPos5:= Pages5.Width / ClientWidth;
      end;
  end;
end;

procedure TATGroups.RestoreSplitPos;
begin
  if ClientWidth<=0 then Exit;
  if ClientHeight<=0 then Exit;

  case FMode of
    gm2Horz,
    gm3Horz,
    gm4Horz:
      begin
        UpdW(Pages1, Trunc(FPos1 * ClientWidth));
        UpdW(Pages2, Trunc(FPos2 * ClientWidth));
        UpdW(Pages3, Trunc(FPos3 * ClientWidth));
      end;
    gm2Vert,
    gm3Vert,
    gm4Vert:
      begin
        UpdH(Pages1, Trunc(FPos1 * ClientHeight));
        UpdH(Pages2, Trunc(FPos2 * ClientHeight));
        UpdH(Pages3, Trunc(FPos3 * ClientHeight));
      end;
    gm1plus2Vert:
      begin
        UpdW(FPanel1, Trunc(FPos1 * ClientWidth));
        UpdH(Pages2, Trunc(FPos2 * ClientHeight));
      end;
    gm1plus2Horz:
      begin
        UpdH(FPanel1, Trunc(FPos1 * ClientHeight));
        UpdW(Pages2, Trunc(FPos2 * ClientWidth));
      end;
    gm4Grid:
      begin
        UpdW(Pages1, Trunc(FPos1 * ClientWidth));
        UpdW(Pages3, Trunc(FPos2 * ClientWidth));
        UpdH(FPanel1, Trunc(FPos3 * ClientHeight));
      end;
    gm6Grid:
      begin
        UpdW(Pages1, Trunc(FPos1 * ClientWidth));
        UpdW(Pages2, Trunc(FPos2 * ClientWidth));
        UpdH(FPanel1, Trunc(FPos3 * ClientHeight));
        UpdW(Pages4, Trunc(FPos4 * ClientWidth));
        UpdW(Pages5, Trunc(FPos5 * ClientWidth));
      end;
  end;
end;

procedure TATGroups.Resize;
begin
  //Logic FPrev* needed for Lazarus!! laz calls onresize also for internal things like
  //splitter move and this causes bad things (resize group1 to width=0 in horz-view)
  if (FPrevWidth<>Width) or (FPrevHeight<>Height) then
  begin
    FPrevWidth:= Width;
    FPrevHeight:= Height;
    RestoreSplitPos;
  end;
end;


procedure TATGroups.TabEmpty(Sender: TObject);
var
  APages: TATPages;
begin
  APages:= (Sender as TATTabs).Parent as TATPages;

  //if last tab closed on Pages1, add new tab
  //if last tab closed on Pages2..Pages4, activate Pages1
  if not APages.EnabledEmpty then
  begin
    APages.OnTabAdd(Pages1.Tabs);
  end
  else
  begin
    if Pages1.Tabs.TabCount>0 then
      Pages1.Tabs.OnTabClick(nil);
  end;
end;

procedure TATGroups.SplitClick(Sender: TObject);
begin
  SetSplitPos((Sender as TComponent).Tag);
end;

function TATGroups.GetSplitPos: Integer;
begin
  case FMode of
    gm2Horz:
      begin
        Result:= Pages1.Width * 100 div ClientWidth;
      end;
    gm2Vert:
      begin
        Result:= Pages1.Height * 100 div ClientHeight;
      end;
    gm1plus2Vert:
      begin
        Result:= FPanel1.Width * 100 div ClientWidth;
      end;
    gm1plus2Horz:
      begin
        Result:= FPanel1.Height * 100 div ClientHeight;
      end;
    else
      Result:= 50;
  end;
end;

procedure TATGroups.SetSplitPos(N: Integer);
begin
  case FMode of
    gm2Horz:
      begin
        UpdW(Pages1, ClientWidth * N div 100);
        SaveSplitPos;
      end;
    gm2Vert:
      begin
        UpdH(Pages1, ClientHeight * N div 100);
        SaveSplitPos;
      end;
    gm1plus2Vert:
      begin
        UpdW(FPanel1, ClientWidth * N div 100);
        SaveSplitPos;
      end;
    gm1plus2Horz:
      begin
        UpdH(FPanel1, ClientHeight * N div 100);
        SaveSplitPos;
      end;
  end;
end;

procedure TATGroups.MoveTab(AFromPages: TATPages; AFromIndex: Integer;
  AToPages: TATPages; AToIndex: Integer; AActivateTabAfter: boolean);
var
  D: TATTabData;
begin
  D:= AFromPages.Tabs.GetTabData(AFromIndex);
  if D=nil then Exit;
  AToPages.AddTab(D.TabObject as TControl, D.TabCaption, D.TabModified, D.TabColor);
  AFromPages.Tabs.DeleteTab(AFromIndex, false, false);

  if AActivateTabAfter then
    with AToPages.Tabs do
      TabIndex:= TabCount-1;
end;


function TATGroups.PagesSetIndex(ANum: Integer): boolean;
var
  APages: TATPages;
begin
  if (ANum>=Low(Pages)) and (ANum<=High(Pages)) then
    APages:= Pages[ANum]
  else
    APages:= nil;

  Result:= (APages<>nil) and APages.Visible and (APages.Tabs.TabCount>0);
  if Result then
    APages.Tabs.OnTabClick(nil);
end;

procedure TATGroups.PagesSetNext(ANext: boolean);
var
  Num0, Num1: Integer;
begin
  Num0:= PagesIndexOf(PagesCurrent);
  if Num0<0 then Exit;
  Num1:= PagesNextIndex(Num0, ANext, false);
  if Num1<0 then Exit;
  PagesSetIndex(Num1);
end;


function TATGroups.PagesIndexOfControl(ACtl: TControl): Integer;
var
  i, j: Integer;
begin
  for i:= Low(Pages) to High(Pages) do
    with Pages[i] do
      for j:= 0 to Tabs.TabCount-1 do
        if Tabs.GetTabData(j).TabObject = ACtl then
        begin
          Result:= i;
          Exit
        end;
  Result:= -1;
end;

function TATGroups.PagesIndexOf(APages: TATPages): Integer;
var
  i: Integer;
begin
  Result:= -1;
  for i:= Low(Pages) to High(Pages) do
    if Pages[i] = APages then
    begin
      Result:= i;
      Exit
    end;
end;

function TATGroups.PagesNextIndex(AIndex: Integer; ANext: boolean;
  AEnableEmpty: boolean): Integer;
var
  N: Integer;
begin
  Result:= -1;
  N:= AIndex;

  repeat
    if ANext then Inc(N) else Dec(N);
    if N>High(Pages) then N:= Low(Pages) else
      if N<Low(Pages) then N:= High(Pages);

    if N=AIndex then Exit; //don't return same index

    if Pages[N].Visible then
      if (Pages[N].Tabs.TabCount>0) or AEnableEmpty then
      begin
        Result:= N;
        Exit
      end;
  until false;
end;


procedure TATGroups.TabFocus(Sender: TObject);
begin
  if Assigned(FOnTabFocus) then
    FOnTabFocus(Sender);
end;

procedure TATGroups.MovePopupTabToNext(ANext: boolean);
var
  N0, N1: Integer;
begin
  N0:= PagesIndexOf(PopupPages);
  if N0<0 then Exit;
  N1:= PagesNextIndex(N0, ANext, true);
  if N1<0 then Exit;
  MoveTab(PopupPages, PopupTabIndex, Pages[N1], -1, false);
end;

procedure TATGroups.MoveCurrentTabToNext(ANext: boolean);
var
  N0, N1: Integer;
begin
  N0:= PagesIndexOf(PagesCurrent);
  if N0<0 then Exit;
  N1:= PagesNextIndex(N0, ANext, true);
  if N1<0 then Exit;
  MoveTab(PagesCurrent, PagesCurrent.Tabs.TabIndex, Pages[N1], -1, true);
end;

procedure TATGroups.TabClose(Sender: TObject; ATabIndex: Integer;
  var ACanClose, ACanContinue: boolean);
begin
  //not needed
  //DoControlLock(Self);
  try
    if Assigned(FOnTabClose) then
      FOnTabClose(Sender, ATabIndex, ACanClose, ACanContinue);
  finally
    //DoControlUnlock(Self);
  end;      
end;

procedure TATGroups.TabAdd(Sender: TObject);
begin
  if Assigned(FOnTabAdd) then
    FOnTabAdd(Sender);
end;

procedure TATGroups.SetTabFont(AFont: TFont);
var
  i: Integer;
begin
  for i:= Low(Pages) to High(Pages) do
    Pages[i].Tabs.Font.Assign(AFont);
end;

procedure TATGroups.SetTabOption(Id: TATTabsOptionId; N: Integer);
var
  i: Integer;
begin
  for i:= Low(Pages) to High(Pages) do
    with Pages[i].Tabs do
      case Id of
        //
        tabColorBg:
          begin
            ColorBg:= N;
            Pages[i].Color:= N;
            FSplit1.Color:= N;
            FSplit2.Color:= N;
            FSplit3.Color:= N;
            FSplit4.Color:= N;
            FSplit5.Color:= N;
          end;
        tabColorBgActive: ColorTabActive:= N;
        tabColorBgPassive: ColorTabPassive:= N;
        tabColorBgPassiveOver: ColorTabOver:= N;
        tabColorText: Font.Color:= N;
        tabColorTextModified: ColorFontModified:= N;
        tabColorBorderActive: ColorBorderActive:= N;
        tabColorBorderPassive: ColorBorderPassive:= N;
        tabColorCloseBg: ColorCloseBg:= N;
        tabColorCloseBgOver: ColorCloseBgOver:= N;
        tabColorCloseBorderOver: ColorCloseBorderOver:= N;
        tabColorCloseX: ColorCloseX:= N;
        tabColorCloseXOver: ColorCloseXOver:= N;
        tabColorArrow: ColorArrow:= N;
        tabColorArrowOver: ColorArrowOver:= N;
        //
        tabOptionFontSize:
          begin
            Font.Size:= N;
            TabHeight:= Trunc(N * 1.8) + 8; //tested for sizes 8..38
            Height:= TabHeight+TabIndentTop+1;
          end;
        //
        tabOptionBottomTabs:
          begin
            TabBottom:= Boolean(N);
            if TabBottom then Align:= alBottom else Align:= alTop;
          end;
        tabOptionShowTabs:         Visible:= Boolean(N);
        tabOptionShowXButtons:     TabShowClose:= TATTabShowClose(N);
        tabOptionShowPlus:         TabShowPlus:= Boolean(N);
        tabOptionShowNums:         TabNumPrefix:= IfThen(Boolean(N), '%d. ', '');
        tabOptionShowEntireColor:  TabShowEntireColor:= Boolean(N);
        tabOptionDoubleClickClose: TabDoubleClickClose:= Boolean(N);
        tabOptionDragDrop:         TabDragEnabled:= Boolean(N);
        tabOptionAngle:            TabAngle:= N;
        tabOptionHeight:           Height:= N;
        tabOptionHeightInner:      TabHeight:= N;
        tabOptionWidthMin:         TabWidthMin:= N;
        tabOptionWidthMax:         TabWidthMax:= N;
        tabOptionIndentTop:        TabIndentTop:= N;
        tabOptionIndentInit:       TabIndentInit:= N;
        tabOptionIndentInter:      TabIndentInter:= N;
        tabOptionIndentColor:      TabIndentColor:= N;
        tabOptionIndentXRight:     TabIndentXRight:= N;
        tabOptionIndentXSize:      TabIndentXSize:= N;
        tabOptionWidecharModified: TabShowModifiedText:= Widechar(N);
      end;
end;

procedure TATGroups.MoveCurrentTabToOpposite;
var
  NFrom, NTo, NTabIndex: Integer;
begin
  NFrom:= PagesIndexOf(PagesCurrent);
  if NFrom<0 then Exit;
  if NFrom=1 then NTo:= 2 else NTo:= 1;

  NTabIndex:= Pages[NFrom].Tabs.TabIndex;
  if NTabIndex<0 then Exit;

  if (NTo>1) and (FMode<=gmOne) then
    SetMode(gm2Horz);

  MoveTab(Pages[NFrom], NTabIndex, Pages[NTo], -1, true);
end;

function TATGroups.GetTabTotalCount: Integer;
var
  i: Integer;
begin
  Result:= 0;
  for i:= Low(Pages) to High(Pages) do
    Inc(Result, Pages[i].Tabs.TabCount);
end;


function TATGroups.GetTabDataOfTotalIndex(N: Integer): TATTabData;
var
  i, Count: Integer;
begin
  Result:= nil;
  Count:= N;
  for i:= Low(Pages) to High(Pages) do
  begin
    if (Count>=0) and (Count<Pages[i].Tabs.TabCount) then
    begin
      Result:= Pages[i].Tabs.GetTabData(Count);
      Exit
    end;
    Dec(Count, Pages[i].Tabs.TabCount);
  end;
end;

function TATGroups.CloseTabsOther(APages: TATPages; ATabIndex: Integer;
  ADoRighter, ADoLefter: boolean): boolean;
var
  j: Integer;
begin
  Result:= false;
  with APages do
  begin
    if ADoRighter then
      for j:= Tabs.TabCount-1 downto ATabIndex+1 do
        if not Tabs.DeleteTab(j, true, true) then Exit;
    if ADoLefter then
      for j:= ATabIndex-1 downto 0 do
        if not Tabs.DeleteTab(j, true, true) then Exit;
  end;
  Result:= true;
end;

function TATGroups.CloseTabsAll(APages: TATPages): boolean;
var
  j: Integer;
begin
  Result:= false;
  with APages do
  begin
    Tabs.TabIndex:= 0; //activate 1st tab to remove TabIndex change on closing
    for j:= Tabs.TabCount-1 downto 0 do
      if not Tabs.DeleteTab(j, true, true) then Exit;
  end;
  Result:= true;
end;

function TATGroups.CloseTabs(Id: TATTabCloseId; AForPopupMenu: boolean): boolean;
var
  i: Integer;
  APagesIndex, ATabIndex: Integer;
begin
  Result:= false;

  if AForPopupMenu then
  begin
    APagesIndex:= PagesIndexOf(PopupPages);
    ATabIndex:= PopupTabIndex;
  end
  else
  begin
    APagesIndex:= PagesIndexOf(PagesCurrent);
    ATabIndex:= PagesCurrent.Tabs.TabIndex;
  end;

  case Id of
    tabCloseCurrent:
      begin
        with Pages[APagesIndex].Tabs do
          if not DeleteTab(ATabIndex, true, true) then Exit;
      end;
    tabCloseOthersThisPage:
      begin
        if not CloseTabsOther(Pages[APagesIndex], ATabIndex, true, true) then Exit;
      end;
    tabCloseLefterThisPage:
      begin
        if not CloseTabsOther(Pages[APagesIndex], ATabIndex, false, true) then Exit;
      end;
    tabCloseRighterThisPage:
      begin
        if not CloseTabsOther(Pages[APagesIndex], ATabIndex, true, false) then Exit;
      end;
    tabCloseOthersAllPages:
      begin
        for i:= High(Pages) downto Low(Pages) do
          if i=APagesIndex then
          begin
            if not CloseTabsOther(Pages[i], ATabIndex, true, true) then Exit;
          end
          else
          begin
            if not CloseTabsAll(Pages[i]) then Exit;
          end;
      end;
    tabCloseAllThisPage:
      begin
        if not CloseTabsAll(Pages[APagesIndex]) then Exit;
      end;
    tabCloseAll:
      begin
        for i:= High(Pages) downto Low(Pages) do
          if not CloseTabsAll(Pages[i]) then Exit;
      end;
  end;

  Result:= true;
end;


const
  cMinSplitter = 10;
  cDeltaSplitter = 5;

procedure TATGroups.SplitPosIncrease;
begin
  SplitPos:= Min(SplitPos + cDeltaSplitter, 100-cMinSplitter);
end;

procedure TATGroups.SplitPosDecrease;
begin
  SplitPos:= Max(SplitPos - cDeltaSplitter, cMinSplitter);
end;

function TATGroups.PagesVisibleCount: Integer;
begin
  Result:= cGroupsCount[FMode];
end;

function TATGroups.SetPagesAndTabIndex(APageIndex, ATabIndex: Integer): boolean;
begin
  Result:=
    (APageIndex>=1) and
    (APageIndex<=PagesVisibleCount) and
    (ATabIndex>=0) and
    (ATabIndex<Pages[APageIndex].Tabs.TabCount);
  if Result then
    Pages[APageIndex].Tabs.TabIndex:= ATabIndex;
end;

procedure TATGroups.TabOver(Sender: TObject; ATabIndex: Integer);
begin
  if Assigned(FOnTabOver) then
    FOnTabOver(Sender, ATabIndex);
end;

procedure TATGroups.TabMove(Sender: TObject; NFrom, NTo: Integer);
begin
  if Assigned(FOnTabMove) then
    FOnTabMove(Sender, NFrom, NTo);
end;


procedure TATGroups.PagesAndTabIndexOfControl(AObject: TObject;
  var NPages, NTab: Integer);
var
  i, j: Integer;
  D: TATTabData;
begin
  NPages:= -1;
  NTab:= -1;
  if AObject=nil then Exit;

  for i:= Low(Pages) to High(Pages) do
    with Pages[i].Tabs do
      for j:= 0 to TabCount-1 do
      begin
        D:= GetTabData(j);
        if D<>nil then
          if D.TabObject=AObject then
          begin
            NPages:= i;
            NTab:= j;
            Exit
          end;
      end;
end;

end.
