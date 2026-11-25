{
 /***************************************************************************
                                markdown.control
                                ----------------
                             Markdown drawing control

 ***************************************************************************/

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}

unit Markdown.Control;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Math, Graphics, Controls, StdCtrls, LCLIntf, LCLType, LMessages,
  Markdown.Elements, Markdown.CanvasRender, Markdown.Parser;

Type
  TMarkdownImageEvent = Markdown.CanvasRender.TMarkdownImageEvent;
  TMarkdownOpenURLEvent = procedure(Sender : TObject; const aURL : String) of object;
  TOpenURLEvent = TMarkdownOpenURLEvent deprecated 'Use TMarkdownOpenURLEvent';

  { TCustomMarkdownControl }

  TCustomMarkdownControl = class(TCustomControl)
  private
    FCalculatedHeight: integer;
    FCalculatedWidth: integer;
    FDefLineHeight: integer;
    FInPaint: Boolean;
    FIsSelecting: Boolean;
    FLinkCursorActive: Boolean;
    FSavedCursor: TCursor;
    FLastCalcClientWidth : integer;
    FLastClientWidth : integer;
    FLastHorzScrollInfo: TScrollInfo;
    FLastVertScrollInfo: TScrollInfo;
    FLayoutNeeded: Boolean;
    FMarkdown: TStrings;
    FOnOpenURL: TMarkdownOpenURLEvent;
    FRenderer: TMarkdownCanvasRenderer;
    FSBHorzShowing: ShortInt;
    FSBVertShowing: ShortInt;
    FScrollBars: TScrollStyle;
    FScrollbarsNeedUpdate: boolean;
    FScrolledLeft: integer; // horizontal scrolled pixels (hidden pixels at left)
    FScrolledTop: integer;  // vertical scrolled pixels (hidden pixels at top)
    FSelectionStart: TSelectionPoint;
    function GetColor(AIndex: Integer): TColor;
    function GetDocument: TMarkdownDocument;
    function GetInteger(AIndex: Integer): Integer;
    function GetOnGetImage: TMarkdownImageEvent;
    function GetSelectionColor: TColor;
    function GetString(AIndex: Integer): string;
    procedure LayoutChanged;
    procedure SetInteger(AIndex: Integer; AValue: Integer);
    procedure SetMarkdown(AValue: TStrings);
    procedure SetOnGetImage(AValue: TMarkdownImageEvent);
    procedure SetRenderColor(AIndex: Integer; AValue: TColor);
    procedure SetScrollBars(AValue: TScrollStyle);
    procedure SetScrolledLeft(AValue: integer);
    procedure SetScrolledTop(AValue: integer);
    procedure SetSelectionColor(AValue: TColor);
    function GetWrapCodeBlocks: Boolean;
    procedure SetWrapCodeBlocks(AValue: Boolean);
    procedure SetString(AIndex: Integer; const AValue: string);
    procedure UpdateLinkCursor(aOverLink: Boolean);
    procedure SelectTo(const aPoint: TSelectionPoint);
  protected
    procedure Click; override;
    procedure CreateWnd; override;
    procedure DestroyWnd; override;
    procedure DoMarkdownChanged(Sender : TObject); virtual;
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean; override;
    function DoMouseWheelHorz(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean; override;
    function GetMaxScrollLeft: integer;
    function GetMaxScrollTop: integer;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure Loaded; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure CalculatePreferredSize(var PreferredWidth, PreferredHeight: integer;
      WithThemeSpace: Boolean); override;
    procedure ParseMarkdown; virtual;
    procedure Resize; override;
    procedure ScrollView(DeltaX, DeltaY: Integer);
    procedure SetShowScrollBar(Which: Integer; AShow: Boolean);
    procedure UpdateScrollBars; virtual;
    procedure WMGetDlgCode(var Message: TLMNoParams); message LM_GETDLGCODE;
    procedure WMHScroll(var Msg: TLMScroll); message LM_HSCROLL;
    procedure WMVScroll(var Msg: TLMScroll); message LM_VSCROLL;
    property Document : TMarkdownDocument Read GetDocument;
    // Renderer that lays out and draws the document
    property Renderer : TMarkdownCanvasRenderer Read FRenderer;
  Public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
    procedure CalcLayout; virtual;
    procedure ClearSelection;
    function GetSelectionText: String;
    procedure CopySelectionToClipBoard;
    // Load a markdown file, set BaseDir to its directory and show it from the top.
    procedure LoadFromFile(const aFileName: string);
    // Markdown source. Every change reparses: use BeginUpdate/EndUpdate for bulk changes.
    property Markdown : TStrings Read FMarkdown Write SetMarkdown;
    property BaseFontSize: Integer index 0 read GetInteger Write SetInteger;
    property FontName: string index 0 read GetString write SetString;
    property MonoFontName: string index 1 read GetString write SetString;
    property FontColor: TColor index 0 read GetColor write SetRenderColor;
    property FontCodeColor: TColor index 1 read GetColor write SetRenderColor;
    property FontQuoteColor: TColor index 2 read GetColor write SetRenderColor;
    property HyperLinkColor: TColor index 3 read GetColor write SetRenderColor;
    property BGCodeColor: TColor index 4 read GetColor write SetRenderColor;
    // Color of the table borders and cell separators.
    property TableLinesColor: TColor index 5 read GetColor write SetRenderColor;
    property BulletChar1 : string index 2 read GetString write SetString;
    property BulletChar2 : string index 3 read GetString write SetString;
    property BulletChar3 : string index 4 read GetString write SetString;
    // Directory used to resolve relative image file names.
    property BaseDir : string index 5 read GetString write SetString;
    Property BlockQuoteIndent : Integer index 1 read GetInteger Write SetInteger;
    property ParagraphSpacing : Integer index 2 read GetInteger Write SetInteger;
    property ExtraIndent : Integer index 3 read GetInteger Write SetInteger;
    Property ImageMargin : integer index 4 read GetInteger Write SetInteger;
    // Extra space above a heading, in percent of the base line height.
    property HeadingSpacing : Integer index 5 read GetInteger Write SetInteger;
    // Extra space between list items, in percent of the base line height.
    property ListItemSpacing : Integer index 6 read GetInteger Write SetInteger;
    // Padding around the contents of a code block, in percent of the base line height.
    property CodeBlockPadding : Integer index 7 read GetInteger Write SetInteger;
    // Horizontal padding of table cells, in percent of the base line height.
    property TableCellPaddingX : Integer index 8 read GetInteger Write SetInteger;
    // Vertical padding of table cells, in percent of the base line height.
    property TableCellPaddingY : Integer index 9 read GetInteger Write SetInteger;
    property OnGetImage : TMarkdownImageEvent read GetOnGetImage Write SetOnGetImage;
    property OnOpenURL : TMarkdownOpenURLEvent Read FOnOpenURL Write FOnOpenURL;
    property ScrollBars: TScrollStyle read FScrollBars write SetScrollBars default ssAutoBoth;
    property ScrolledLeft: integer read FScrolledLeft write SetScrolledLeft;
    property ScrolledTop: integer read FScrolledTop write SetScrolledTop;
    property SelectionColor: TColor read GetSelectionColor write SetSelectionColor;
    property SelectedText: String read GetSelectionText;
    // Wrap long lines of fenced code blocks instead of scrolling horizontally.
    property WrapCodeBlocks: Boolean read GetWrapCodeBlocks write SetWrapCodeBlocks default False;
  end;

  { TMarkdownControl }

  TMarkdownControl = class(TCustomMarkdownControl)
  published
    property Align;
    property Anchors;
    property AutoSize;
    property BorderSpacing;
    property BidiMode;
    property BorderWidth;
    property BorderStyle;
    property ChildSizing;
    property ClientHeight;
    property ClientWidth;
    property Color;
    property Constraints;
    property DockSite;
    property DoubleBuffered;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property Markdown;
    property ParentBackground;
    property ParentBidiMode;
    property ParentColor;
    property ParentDoubleBuffered;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ScrollBars;
    property ShowHint;
    property TableLinesColor;
    property WrapCodeBlocks;
    property TabOrder;
    property TabStop;
    property Visible;

    property OnChangeBounds;
    property OnClick;
    property OnContextPopup;
    property OnDockDrop;
    property OnDockOver;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetSiteInfo;
    property OnGetDockCaption;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnMouseWheelHorz;
    property OnMouseWheelLeft;
    property OnMouseWheelRight;
    property OnPaint;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
  end;

implementation

uses Markdown.Processors, Clipbrd;

{ TMarkdownControl }

procedure TCustomMarkdownControl.SetMarkdown(AValue: TStrings);
begin
  if FMarkdown=AValue then Exit;
  FMarkdown.Assign(AValue);
end;

function TCustomMarkdownControl.GetOnGetImage: TMarkdownImageEvent;
begin
  Result:=FRenderer.OnGetImage;
end;

function TCustomMarkdownControl.GetColor(AIndex: Integer): TColor;
begin
  Case aIndex of
    0 : Result:=FRenderer.FontColor;
    1 : Result:=FRenderer.FontCodeColor;
    2 : Result:=FRenderer.FontQuoteColor;
    3 : Result:=FRenderer.FontHyperLinkColor;
    4 : Result:=FRenderer.BGCodeColor;
    5 : Result:=FRenderer.TableLinesColor;
  else
    Result:=clNone;
  end;
end;

function TCustomMarkdownControl.GetDocument: TMarkdownDocument;
begin
  Result:=FRenderer.Document;
end;

function TCustomMarkdownControl.GetInteger(AIndex: Integer): Integer;
begin
  Case aIndex of
    0 : Result:=FRenderer.BaseFontSize;
    1 : Result:=FRenderer.BlockQuoteIndent;
    2 : Result:=FRenderer.ParagraphSpacing;
    3 : Result:=FRenderer.ExtraIndent;
    4 : Result:=FRenderer.ImageMargin;
    5 : Result:=FRenderer.HeadingSpacing;
    6 : Result:=FRenderer.ListItemSpacing;
    7 : Result:=FRenderer.CodeBlockPadding;
    8 : Result:=FRenderer.TableCellPaddingX;
    9 : Result:=FRenderer.TableCellPaddingY;
  else
    Result:=0;
  end;
end;

function TCustomMarkdownControl.GetString(AIndex: Integer): string;
begin
  Case aIndex of
    0 : Result:=FRenderer.FontName;
    1 : Result:=FRenderer.MonoFontName;
    2 : Result:=FRenderer.BulletChar1;
    3 : Result:=FRenderer.BulletChar2;
    4 : Result:=FRenderer.BulletChar3;
    5 : Result:=FRenderer.BaseDir;
  else
    Result:='';
  end;
end;

// Recalculate the layout after a change of a layout property.
procedure TCustomMarkdownControl.LayoutChanged;

begin
  if not (csLoading in ComponentState) then
    CalcLayout;
end;


procedure TCustomMarkdownControl.SetRenderColor(AIndex: Integer; AValue: TColor);
begin
  Case aIndex of
    0 : FRenderer.FontColor:=aValue;
    1 : FRenderer.FontCodeColor:=aValue;
    2 : FRenderer.FontQuoteColor:=aValue;
    3 : FRenderer.FontHyperLinkColor:=aValue;
    4 : FRenderer.BGCodeColor:=aValue;
    5 : FRenderer.TableLinesColor:=aValue;
  end;
  Invalidate;
end;

procedure TCustomMarkdownControl.SetInteger(AIndex: Integer; AValue: Integer);
begin
  Case aIndex of
    0 : FRenderer.BaseFontSize:=aValue;
    1 : FRenderer.BlockQuoteIndent:=aValue;
    2 : FRenderer.ParagraphSpacing:=aValue;
    3 : FRenderer.ExtraIndent:=aValue;
    4 : FRenderer.ImageMargin:=aValue;
    5 : FRenderer.HeadingSpacing:=aValue;
    6 : FRenderer.ListItemSpacing:=aValue;
    7 : FRenderer.CodeBlockPadding:=aValue;
    8 : FRenderer.TableCellPaddingX:=aValue;
    9 : FRenderer.TableCellPaddingY:=aValue;
  end;
  LayoutChanged;
end;

procedure TCustomMarkdownControl.SetOnGetImage(AValue: TMarkdownImageEvent);
begin
  FRenderer.OnGetImage:=aValue;
end;

procedure TCustomMarkdownControl.SetString(AIndex: Integer; const AValue: string);
begin
  Case aIndex of
    0 : FRenderer.FontName:=aValue;
    1 : FRenderer.MonoFontName:=aValue;
    2 : FRenderer.BulletChar1:=aValue;
    3 : FRenderer.BulletChar2:=aValue;
    4 : FRenderer.BulletChar3:=aValue;
    5 : FRenderer.BaseDir:=aValue;
  end;
  LayoutChanged;
end;

procedure TCustomMarkdownControl.SetScrollBars(AValue: TScrollStyle);
begin
  if FScrollBars = AValue then Exit;
  FScrollBars := AValue;
  LayoutChanged;
end;

procedure TCustomMarkdownControl.SetScrolledLeft(AValue: integer);
var
  OldScrolledLeft: Integer;
begin
  OldScrolledLeft := FScrolledLeft;
  if AValue<0 then AValue:=0;
  if AValue=FScrolledLeft then exit;
  AValue:=Min(AValue,GetMaxScrollLeft);
  if AValue=FScrolledLeft then exit;
  FScrolledLeft:=AValue;
  ScrollView(OldScrolledLeft-FScrolledLeft, 0);
end;

procedure TCustomMarkdownControl.SetScrolledTop(AValue: integer);
var
  OldScrolledTop: Integer;
begin
  OldScrolledTop:=FScrolledTop;
  if FScrolledTop=AValue then exit;
  if AValue<0 then AValue:=0;
  AValue:=Min(AValue,GetMaxScrollTop);
  if AValue=FScrolledTop then exit;
  FScrolledTop:=AValue;
  ScrollView(0, OldScrolledTop-FScrolledTop);
end;

procedure TCustomMarkdownControl.DoMarkdownChanged(Sender: TObject);
begin
  FIsSelecting:=False;
  FSelectionStart.LayoutItemIndex:=-1;
  if not (csLoading in ComponentState) then
    ParseMarkdown;
end;

function TCustomMarkdownControl.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
  MousePos: TPoint): Boolean;

var
  lLines: Integer;

begin
  Result:=inherited DoMouseWheel(Shift, WheelDelta, MousePos);
  if Result then
    Exit;
  lLines:=Mouse.WheelScrollLines;
  if lLines<0 then
    ScrolledTop:=ScrolledTop-Sign(WheelDelta)*(ClientHeight-FDefLineHeight)
  else
    ScrolledTop:=ScrolledTop-(WheelDelta*lLines*FDefLineHeight) div 120;
  Result:=True;
end;


function TCustomMarkdownControl.DoMouseWheelHorz(Shift: TShiftState; WheelDelta: Integer;
  MousePos: TPoint): Boolean;

begin
  Result:=inherited DoMouseWheelHorz(Shift, WheelDelta, MousePos);
  if Result then
    Exit;
  ScrolledLeft:=ScrolledLeft+(WheelDelta*FDefLineHeight) div 120;
  Result:=True;
end;


function TCustomMarkdownControl.GetMaxScrollLeft: integer;
begin
  Result:=FCalculatedWidth-(FLastCalcClientWidth-2*BorderWidth);
  if Result<0 then Result:=0;
end;

function TCustomMarkdownControl.GetMaxScrollTop: integer;
begin
  Result:=FCalculatedHeight-(ClientHeight-2*BorderWidth);
  if Result<0 then Result:=0;
end;

constructor TCustomMarkdownControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMarkdown:=TStringList.Create;
  TStringList(FMarkdown).OnChange:=@DoMarkdownChanged;
  FRenderer:=TMarkdownCanvasRenderer.Create(Nil);
  FDefLineHeight:=FRenderer.BaseFontSize*4 div 3;
  FIsSelecting := False;
  FScrollBars := ssAutoBoth;
  FSelectionStart.LayoutItemIndex := -1;
  FSelectionStart.CharOffset := 0;
  FSBVertShowing:=-1;
  FSBHorzShowing:=-1;
  ControlStyle := ControlStyle + [csTripleClicks];
  TabStop := True;
  BorderStyle := bsSingle;
  BorderWidth := 0;
  AutoSize := True;
end;

procedure TCustomMarkdownControl.CalculatePreferredSize(
  var PreferredWidth, PreferredHeight: integer; WithThemeSpace: Boolean);
const
  MaxWidgetSetHeight = 30000;
begin
  PreferredWidth := 0;
  PreferredHeight := Min(FCalculatedHeight + 2*BorderWidth, MaxWidgetSetHeight);
end;

procedure TCustomMarkdownControl.ParseMarkdown;

begin
  FRenderer.ParseMarkdown(FMarkdown);
  CalcLayout;
end;

procedure TCustomMarkdownControl.CalcLayout;
var
  lReserveVertSB: Boolean;
begin
  if not HandleAllocated then
    begin
    FLastClientWidth:=ClientWidth;
    FCalculatedWidth:=10;
    FCalculatedHeight:=10;
    FLayoutNeeded:=True;
    exit;
    end;
  FLayoutNeeded:=False;
  FIsSelecting:=False;
  FSelectionStart.LayoutItemIndex:=-1;

  // Lay out at the client width that excludes the vertical scrollbar
  lReserveVertSB:=(FSBVertShowing<>Ord(True))
                  and (ScrollBars in [ssBoth, ssVertical, ssAutoBoth, ssAutoVertical]);

  FLastClientWidth:=ClientWidth;
  FLastCalcClientWidth:=FLastClientWidth;
  if lReserveVertSB then
    dec(FLastCalcClientWidth,GetSystemMetrics(SM_CXVSCROLL));
  if FLastCalcClientWidth<0 then FLastCalcClientWidth:=0;

  FRenderer.TargetDPI:=Font.PixelsPerInch;
  FRenderer.CalculateLayout(Canvas,FLastCalcClientWidth,FCalculatedWidth,FCalculatedHeight);
  FDefLineHeight:=Max(1,FRenderer.MeasureBaseLineHeight(Canvas));

  FScrollbarsNeedUpdate:=true;
  UpdateScrollBars;
  InvalidatePreferredSize;
  AdjustSize;
  if not FInPaint then
    Invalidate;
end;

procedure TCustomMarkdownControl.Click;
var
  P : TPoint;
  URL : UTF8String;
begin
  inherited Click;
  if assigned(FOnOpenURL) then
    begin
    P:=ScreenToControl(Mouse.CursorPos);
    if FRenderer.HitTestLink(P.X+ScrolledLeft,P.Y+ScrolledTop,URL) then
      FOnOpenURL(Self,URL);
    end;
end;

procedure TCustomMarkdownControl.CreateWnd;
begin
  FSBHorzShowing:=-1;
  FSBVertShowing:=-1;
  inherited CreateWnd;
end;

procedure TCustomMarkdownControl.DestroyWnd;
begin
  inherited DestroyWnd;
  if Canvas <> nil then
    TControlCanvas(Canvas).FreeHandle;
  FLastHorzScrollInfo.cbSize := 0;
  FLastVertScrollInfo.cbSize := 0;
end;

procedure TCustomMarkdownControl.CopySelectionToClipBoard;

begin
  Clipboard.AsText:=GetSelectionText;
end;

procedure TCustomMarkdownControl.LoadFromFile(const aFileName: string);

var
  lLines: TStrings;

begin
  lLines:=TStringList.Create;
  try
    lLines.LoadFromFile(aFileName);
    FRenderer.BaseDir:=ExtractFileDir(ExpandFileName(aFileName));
    FScrolledTop:=0;
    FScrolledLeft:=0;
    FMarkdown.Assign(lLines);
  finally
    lLines.Free;
  end;
end;


procedure TCustomMarkdownControl.KeyDown(var Key: Word; Shift: TShiftState);

var
  lHandled: Boolean;

begin
  lHandled:=True;
  if (Shift=[ssCtrl]) and ((Key=Ord('C')) or (Key=VK_INSERT)) then
    CopySelectionToClipBoard
  else if (Shift=[]) or ((Shift=[ssCtrl]) and (Key in [VK_HOME,VK_END])) then
    case Key of
      VK_NEXT:  ScrolledTop:=ScrolledTop+ClientHeight-FDefLineHeight;
      VK_PRIOR: ScrolledTop:=ScrolledTop-ClientHeight+FDefLineHeight;
      VK_DOWN:  ScrolledTop:=ScrolledTop+FDefLineHeight;
      VK_UP:    ScrolledTop:=ScrolledTop-FDefLineHeight;
      VK_RIGHT: ScrolledLeft:=ScrolledLeft+FDefLineHeight div 2;
      VK_LEFT:  ScrolledLeft:=ScrolledLeft-FDefLineHeight div 2;
      VK_HOME:  ScrolledTop:=0;
      VK_END:   ScrolledTop:=GetMaxScrollTop;
    else
      lHandled:=False;
    end
  else
    lHandled:=False;
  if lHandled then
    Key:=0
  else
    inherited KeyDown(Key, Shift);
end;


procedure TCustomMarkdownControl.WMGetDlgCode(var Message: TLMNoParams);

begin
  inherited;
  Message.Result:=Message.Result or DLGC_WANTARROWS;
end;


procedure TCustomMarkdownControl.Loaded;
begin
  inherited Loaded;
  ParseMarkdown;
end;

procedure TCustomMarkdownControl.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

var
  lPoint, lStart, lEnd: TSelectionPoint;
  lFound: Boolean;

begin
  inherited MouseDown(Button, Shift, X, Y);
  if CanFocus and not Focused then
    SetFocus;

  if Button = mbLeft then
  begin
    lPoint:=FRenderer.HitTestText(X+ScrolledLeft, Y+ScrolledTop);
    if lPoint.LayoutItemIndex <> -1 then
    begin
      if [ssDouble,ssTriple]*Shift<>[] then
        begin
        if ssTriple in Shift then
          lFound:=FRenderer.GetLineBounds(lPoint, lStart, lEnd)
        else
          lFound:=FRenderer.GetWordBounds(lPoint, lStart, lEnd);
        if lFound then
          begin
          FSelectionStart:=lStart;
          SelectTo(lEnd);
          end;
        FIsSelecting:=False;
        end
      else
        begin
        // Shift+click extends the selection from the existing anchor
        if (ssShift in Shift) and (FSelectionStart.LayoutItemIndex<>-1) then
          SelectTo(lPoint)
        else
          begin
          FSelectionStart:=lPoint;
          FRenderer.ClearSelection;
          end;
        FIsSelecting := True;
        end;
      Invalidate;
    end;
  end;
end;


// Select the text between the selection anchor and aPoint.
procedure TCustomMarkdownControl.SelectTo(const aPoint: TSelectionPoint);

begin
  if (FSelectionStart.LayoutItemIndex>aPoint.LayoutItemIndex)
     or ((FSelectionStart.LayoutItemIndex=aPoint.LayoutItemIndex)
         and (FSelectionStart.CharOffset>aPoint.CharOffset)) then
    FRenderer.SetSelection(aPoint,FSelectionStart)
  else
    FRenderer.SetSelection(FSelectionStart,aPoint);
end;


procedure TCustomMarkdownControl.MouseMove(Shift: TShiftState; X, Y: Integer);

var
  CurrentPoint: TSelectionPoint;
  lURL : UTF8String;

begin
  inherited MouseMove(Shift, X, Y);

  UpdateLinkCursor(not FIsSelecting and not (ssLeft in Shift)
                   and FRenderer.HitTestLink(X+ScrolledLeft, Y+ScrolledTop, lURL));
  if FIsSelecting and (ssLeft in Shift) then
  begin
    if Y<0 then
      ScrolledTop:=ScrolledTop-FDefLineHeight
    else if Y>=ClientHeight then
      ScrolledTop:=ScrolledTop+FDefLineHeight;
    CurrentPoint := FRenderer.HitTestText(X+ScrolledLeft, Y+ScrolledTop);
    if CurrentPoint.LayoutItemIndex <> -1 then
    begin
      SelectTo(CurrentPoint);
      Invalidate;
    end;
  end;
end;

// Show the hand cursor while over a link, restore the previous cursor otherwise.
procedure TCustomMarkdownControl.UpdateLinkCursor(aOverLink: Boolean);

begin
  if aOverLink=FLinkCursorActive then
    Exit;
  FLinkCursorActive:=aOverLink;
  if aOverLink then
    begin
    FSavedCursor:=Cursor;
    Cursor:=crHandPoint;
    end
  else
    Cursor:=FSavedCursor;
end;


procedure TCustomMarkdownControl.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);

  if Button = mbLeft then
  begin
    FIsSelecting := False;
  end;
end;

procedure TCustomMarkdownControl.Resize;
begin
  inherited Resize;
  if ClientWidth<>FLastClientWidth then
    CalcLayout;
end;

procedure TCustomMarkdownControl.ScrollView(DeltaX, DeltaY: Integer);
var
  ScrollArea: TRect;
  ScrollFlags: Integer;
begin
  if (DeltaX=0) and (DeltaY=0) then
    Exit;

  FScrollbarsNeedUpdate:=true;
  if not HandleAllocated then
    Exit;
  ScrollFlags := SW_INVALIDATE or SW_ERASE;
  ScrollArea := ClientRect;
  InflateRect(ScrollArea, -BorderWidth, -BorderWidth);
  ScrollWindowEx(Handle, DeltaX, DeltaY, @ScrollArea, @ScrollArea, 0, nil, ScrollFlags);

  UpdateScrollbars;
end;

procedure TCustomMarkdownControl.SetShowScrollBar(Which: Integer; AShow: Boolean);
begin
  if ((Which in [SB_Horz, SB_BOTH]) and (FSBHorzShowing<>Ord(AShow)))
  or ((Which in [SB_Vert, SB_BOTH]) and (FSBVertShowing<>Ord(AShow)))
  then
    ShowScrollBar(Handle, Which, AShow);

  if Which in [SB_Horz, SB_BOTH] then
    FSBHorzShowing:=Ord(AShow);
  if Which in [SB_Vert, SB_BOTH] then
    FSBVertShowing:=Ord(AShow);
end;

procedure TCustomMarkdownControl.UpdateScrollBars;
var
  NeedHorzSB, NeedVertSB: Boolean;
  MaxScrollLeft, MaxScrollTop: Integer;
  ScrollInfo: TScrollInfo;
begin
  if not HandleAllocated then
    exit;
  if not FScrollbarsNeedUpdate then exit;
  FScrollbarsNeedUpdate:=false;

  NeedHorzSB:=FCalculatedWidth+2*BorderWidth > FLastCalcClientWidth;
  NeedVertSB:=FCalculatedHeight+2*BorderWidth > ClientHeight;

  MaxScrollLeft := GetMaxScrollLeft;
  MaxScrollTop := GetMaxScrollTop;

  if ScrolledLeft>MaxScrollLeft then FScrolledLeft:=MaxScrollLeft;
  if ScrolledTop>MaxScrollTop then FScrolledTop:=MaxScrollTop;

  if (ScrollBars in [ssBoth, ssHorizontal, ssAutoBoth, ssAutoHorizontal]) then
    begin
    // horizontal scrollbar
    ScrollInfo:=Default(TScrollInfo);
    ScrollInfo.cbSize := SizeOf(ScrollInfo);
    ScrollInfo.fMask := SIF_ALL or SIF_DISABLENOSCROLL;
    ScrollInfo.nTrackPos := 0;
    ScrollInfo.nMin := 0;
    ScrollInfo.nPage := Max(1,FLastCalcClientWidth-2*BorderWidth);
    ScrollInfo.nMax := Max(1,MaxScrollLeft+integer(ScrollInfo.nPage));
    ScrollInfo.nPos := Max(FScrolledLeft,0);
    if not CompareMem(@ScrollInfo,@FLastHorzScrollInfo,SizeOf(TScrollInfo)) then
      begin
      if (fScrollBars in [ssAutoBoth, ssAutoHorizontal])
          and not NeedHorzSB then
        begin
        //DebugLn(['TCustomMarkdownControl.UpdateScrollbars Hide Horizontal.']);
        ScrollInfo.nPos := 0;
        SetScrollInfo(Handle, SB_HORZ, ScrollInfo, False);
        FLastHorzScrollInfo.cbSize:=0;
        SetShowScrollBar(SB_HORZ, false);
        end
      else
        begin
        //DebugLn(['TCustomMarkdownControl.UpdateScrollbars Show Horizontal: nMin=',ScrollInfo.nMin,
        //' nMax=',ScrollInfo.nMax,' nPage=',ScrollInfo.nPage,
        //' nPos=',ScrollInfo.nPos,' GetMaxScrollLeft=',MaxScrollLeft,
        //' ClientW=',ClientWidth, ' MaxRight=',FMaxRight]);
        FLastHorzScrollInfo:=ScrollInfo;
        SetShowScrollBar(SB_HORZ, true);
        SetScrollInfo(Handle, SB_HORZ, ScrollInfo, true);
        end;
      end;
    end
  else
    begin
    FLastHorzScrollInfo.cbSize:=0;
    SetShowScrollBar(SB_HORZ,false);
    end;

  if (ScrollBars in [ssBoth, ssVertical, ssAutoBoth, ssAutoVertical]) then
    begin
    // vertical scrollbar
    ScrollInfo:=Default(TScrollInfo);
    ScrollInfo.cbSize := SizeOf(ScrollInfo);
    ScrollInfo.fMask := SIF_ALL or SIF_DISABLENOSCROLL;
    ScrollInfo.nTrackPos := 0;
    ScrollInfo.nMin := 0;
    ScrollInfo.nPage := Max(1,ClientHeight-2*BorderWidth);
    ScrollInfo.nMax := Max(1,MaxScrollTop+integer(ScrollInfo.nPage));
    ScrollInfo.nPos := Max(FScrolledTop,0);
    if not CompareMem(@ScrollInfo,@FLastVertScrollInfo,SizeOf(TScrollInfo)) then
      begin
      if (fScrollBars in [ssAutoBoth, ssAutoVertical])
          and not NeedVertSB then
        begin
        //DebugLn(['TCustomMarkdownControl.UpdateScrollbars Hide Vertical.']);
        ScrollInfo.nPos := 0;
        SetScrollInfo(Handle, SB_Vert, ScrollInfo, False);
        FLastVertScrollInfo.cbSize:=0;
        SetShowScrollBar(SB_Vert, false);
        end
      else
        begin
        //DebugLn(['TCustomMarkdownControl.UpdateScrollbars Show Vertical: nMin=',ScrollInfo.nMin,
        //' nMax=',ScrollInfo.nMax,' nPage=',ScrollInfo.nPage,
        //' nPos=',ScrollInfo.nPos,' GetMaxScrollLeft=',MaxScrollLeft,
        //' ClientW=',ClientWidth, ' MaxRight=',FMaxRight]);
        FLastVertScrollInfo:=ScrollInfo;
        SetShowScrollBar(SB_Vert, true);
        SetScrollInfo(Handle, SB_Vert, ScrollInfo, true);
        end;
      end;
    end
  else
    begin
    FLastVertScrollInfo.cbSize:=0;
    SetShowScrollBar(SB_Vert,false);
    end;
end;

procedure TCustomMarkdownControl.WMHScroll(var Msg: TLMScroll);
begin
  case Msg.ScrollCode of
      // Scrolls to start / end of the text
    SB_LEFT:       ScrolledLeft := 0;
    SB_RIGHT:      ScrolledLeft := GetMaxScrollLeft;
      // Scrolls one line left / right
    SB_LINERIGHT:  ScrolledLeft := ScrolledLeft + FDefLineHeight div 2;
    SB_LINELEFT:   ScrolledLeft := ScrolledLeft - FDefLineHeight div 2;
      // Scrolls one page of lines left / right
    SB_PAGERIGHT:  ScrolledLeft := ScrolledLeft + ClientWidth
                                       - FDefLineHeight;
    SB_PAGELEFT:   ScrolledLeft := ScrolledLeft - ClientWidth
                                       + FDefLineHeight;
      // Scrolls to the current scroll bar position
    SB_THUMBPOSITION,
    SB_THUMBTRACK: ScrolledLeft := Msg.Pos;

    SB_ENDSCROLL: ;// Ends scrolling
  end;
end;

procedure TCustomMarkdownControl.WMVScroll(var Msg: TLMScroll);
begin
  case Msg.ScrollCode of
      // Scrolls to start / end of the text
    SB_TOP:        ScrolledTop := 0;
    SB_BOTTOM:     ScrolledTop := GetMaxScrollTop;
      // Scrolls one line up / down
    SB_LINEDOWN:   ScrolledTop := ScrolledTop + FDefLineHeight;
    SB_LINEUP:     ScrolledTop := ScrolledTop - FDefLineHeight;
      // Scrolls one page of lines up / down
    SB_PAGEDOWN:   ScrolledTop := ScrolledTop + ClientHeight
                                     - FDefLineHeight;
    SB_PAGEUP:     ScrolledTop := ScrolledTop - ClientHeight
                                     + FDefLineHeight;
      // Scrolls to the current scroll bar position
    SB_THUMBPOSITION,
    SB_THUMBTRACK: ScrolledTop := Msg.Pos;

    SB_ENDSCROLL: ; // Ends scrolling
  end;
end;

procedure TCustomMarkdownControl.ClearSelection;
begin
  FRenderer.ClearSelection;
  FIsSelecting := False;
  FSelectionStart.LayoutItemIndex := -1;
  Invalidate;
end;

function TCustomMarkdownControl.GetSelectionText: String;
begin
  Result := FRenderer.GetSelectedText;
end;

function TCustomMarkdownControl.GetSelectionColor: TColor;
begin
  Result := FRenderer.SelectionColor;
end;

procedure TCustomMarkdownControl.SetSelectionColor(AValue: TColor);
begin
  FRenderer.SelectionColor := AValue;
  Invalidate;
end;

function TCustomMarkdownControl.GetWrapCodeBlocks: Boolean;
begin
  Result:=FRenderer.WrapCodeBlocks;
end;

procedure TCustomMarkdownControl.SetWrapCodeBlocks(AValue: Boolean);
begin
  if FRenderer.WrapCodeBlocks=AValue then
    Exit;
  FRenderer.WrapCodeBlocks:=AValue;
  LayoutChanged;
end;

destructor TCustomMarkdownControl.Destroy;
begin
  FreeAndNil(FRenderer);
  FreeAndNil(FMarkdown);
  inherited destroy;
end;

procedure TCustomMarkdownControl.Paint;
begin
  if FLayoutNeeded then
    begin
    FInPaint:=True;
    try
      CalcLayout;
    finally
      FInPaint:=False;
    end;
    end;
  Canvas.Brush.Color:=GetColorResolvingParent;
  Canvas.Brush.Style:=bsSolid;
  Canvas.FillRect(0,0,Width,Height);
  if (FCalculatedWidth>0) and (FCalculatedHeight>0) then
    begin
    FRenderer.BGColor:=GetColorResolvingParent;
    FRenderer.DrawLayout(Canvas,-ScrolledLeft,-ScrolledTop);
    end;
end;

end.

