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

unit MarkDown.Control;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Math, Graphics, Controls, StdCtrls, LCLIntf, LCLType, LMessages,
  MarkDown.Elements, MarkDown.CanvasRender, MarkDown.Parser;

Type
  TMarkdownImageEvent = MarkDown.CanvasRender.TMarkdownImageEvent;
  TOpenURLEvent = procedure(Sender : TObject; aURL : String) of object;

  { TCustomMarkDownControl }

  TCustomMarkDownControl = class(TCustomControl)
  private
    FCalculatedHeight: integer;
    FCalculatedWidth: integer;
    FDefLineHeight: integer;
    FIsSelecting: Boolean;
    FLastCalcClientWidth : integer;
    FLastClientWidth : integer;
    FLastHorzScrollInfo: TScrollInfo;
    FLastVertScrollInfo: TScrollInfo;
    FMarkDown: TStrings;
    FOnOpenURL: TOpenURLEvent;
    FRenderer: TMarkDownCanvasRenderer;
    FSBHorzShowing: ShortInt;
    FSBVertShowing: ShortInt;
    FScrollBars: TScrollStyle;
    FScrollbarsNeedUpdate: boolean;
    FScrolledLeft: integer; // horizontal scrolled pixels (hidden pixels at left)
    FScrolledTop: integer;  // vertical scrolled pixels (hidden pixels at top)
    FSelectionStart: TSelectionPoint;
    function GetColor(AIndex: Integer): TColor;
    function GetDocument: TMarkDownDocument;
    function GetInteger(AIndex: Integer): Integer;
    function GetOnGetImage: TMarkdownImageEvent;
    function GetSelectionColor: TColor;
    function GetString(AIndex: Integer): string;
    procedure SetInteger(AIndex: Integer; AValue: Integer);
    procedure SetMarkDown(AValue: TStrings);
    procedure SetOnGetImage(AValue: TMarkdownImageEvent);
    procedure SetRenderColor(AIndex: Integer; AValue: TColor);
    procedure SetScrollBars(AValue: TScrollStyle);
    procedure SetScrolledLeft(AValue: integer);
    procedure SetScrolledTop(AValue: integer);
    procedure SetSelectionColor(AValue: TColor);
    procedure SetString(AIndex: Integer; AValue: string);
  protected
    procedure Click; override;
    procedure CreateWnd; override;
    procedure DestroyWnd; override;
    procedure DoMarkdownChanged(Sender : TObject); virtual;
    function GetMaxScrollLeft: integer;
    function GetMaxScrollTop: integer;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure Loaded; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure CalculatePreferredSize(var PreferredWidth, PreferredHeight: integer;
      WithThemeSpace: Boolean); override;
    procedure ParseMarkDown; virtual;
    procedure Resize; override;
    procedure ScrollView(DeltaX, DeltaY: Integer);
    procedure SetShowScrollBar(Which: Integer; AShow: Boolean);
    procedure UpdateScrollBars; virtual;
    procedure WMHScroll(var Msg: TLMScroll); message LM_HSCROLL;
    procedure WMVScroll(var Msg: TLMScroll); message LM_VSCROLL;
    property Document : TMarkDownDocument Read GetDocument;
  Public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
    procedure CalcLayout; virtual;
    procedure ClearSelection;
    function GetSelectionText: String;
    procedure CopySelectionToClipBoard;
    property MarkDown : TStrings Read FMarkDown Write SetMarkDown;
    property BaseFontSize: Integer index 0 read GetInteger Write SetInteger;
    property FontName: string index 0 read GetString write SetString;
    property MonoFontName: string index 1 read GetString write SetString;
    property FontColor: TColor index 0 read GetColor write SetRenderColor;
    property FontCodeColor: TColor index 1 read GetColor write SetRenderColor;
    property FontQuoteColor: TColor index 2 read GetColor write SetRenderColor;
    property HyperLinkColor: TColor index 3 read GetColor write SetRenderColor;
    property BGCodeColor: TColor index 4 read GetColor write SetRenderColor;
    property BulletChar1 : string index 2 read GetString write SetString;
    property BulletChar2 : string index 3 read GetString write SetString;
    property BulletChar3 : string index 4 read GetString write SetString;
    Property BlockQuoteIndent : Integer index 1 read GetInteger Write SetInteger;
    property ParagraphSpacing : Integer index 2 read GetInteger Write SetInteger;
    property ExtraIndent : Integer index 3 read GetInteger Write SetInteger;
    Property ImageMargin : integer index 4 read GetInteger Write SetInteger;
    property OnGetImage : TMarkdownImageEvent read GetOnGetImage Write SetOnGetImage;
    property OnOpenURL : TOpenURLEvent Read FOnOpenURL Write FOnOpenURL;
    property ScrollBars: TScrollStyle read FScrollBars write SetScrollBars default ssAutoVertical;
    property ScrolledLeft: integer read FScrolledLeft write SetScrolledLeft;
    property ScrolledTop: integer read FScrolledTop write SetScrolledTop;
    property SelectionColor: TColor read GetSelectionColor write SetSelectionColor;
    property SelectedText: String read GetSelectionText;
  end;

  { TMarkDownControl }

  TMarkDownControl = class(TCustomMarkDownControl)
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
    property MarkDown;
    property ParentBackground;
    property ParentBidiMode;
    property ParentColor;
    property ParentDoubleBuffered;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ScrollBars;
    property ShowHint;
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

uses markdown.processors,Clipbrd;

{ TMarkDownControl }

procedure TCustomMarkDownControl.SetMarkDown(AValue: TStrings);
begin
  if FMarkDown=AValue then Exit;
  FMarkDown.Assign(AValue);
end;

function TCustomMarkDownControl.GetOnGetImage: TMarkdownImageEvent;
begin
  Result:=FRenderer.OnGetImage;
end;

function TCustomMarkDownControl.GetColor(AIndex: Integer): TColor;
begin
  Case aIndex of
    0 : Result:=FRenderer.FontColor;
    1 : Result:=FRenderer.FontCodeColor;
    2 : Result:=FRenderer.FontQuoteColor;
    3 : Result:=FRenderer.FontHyperLinkColor;
    4 : Result:=FRenderer.BGCodeColor;
  end;
end;

function TCustomMarkDownControl.GetDocument: TMarkDownDocument;
begin
  Result:=FRenderer.Document;
end;

function TCustomMarkDownControl.GetInteger(AIndex: Integer): Integer;
begin
  Case aIndex of
    0 : Result:=FRenderer.BaseFontSize;
    1 : Result:=FRenderer.BlockQuoteIndent;
    2 : Result:=FRenderer.ParagraphSpacing;
    3 : Result:=FRenderer.ExtraIndent;
    4 : Result:=FRenderer.ImageMargin;
  end;
end;

function TCustomMarkDownControl.GetString(AIndex: Integer): string;
begin
  Case aIndex of
    0 : Result:=FRenderer.FontName;
    1 : Result:=FRenderer.MonoFontName;
  end;
end;

procedure TCustomMarkDownControl.SetRenderColor(AIndex: Integer; AValue: TColor);
begin
  Case aIndex of
    0 : FRenderer.FontColor:=aValue;
    1 : FRenderer.FontCodeColor:=aValue;
    2 : FRenderer.FontQuoteColor:=aValue;
    3 : FRenderer.FontHyperLinkColor:=aValue;
    4 : FRenderer.BGCodeColor:=aValue;
  end;
end;

procedure TCustomMarkDownControl.SetInteger(AIndex: Integer; AValue: Integer);
begin
  Case aIndex of
    0 : FRenderer.BaseFontSize:=aValue;
    1 : FRenderer.BlockQuoteIndent:=aValue;
    2 : FRenderer.ParagraphSpacing:=aValue;
    3 : FRenderer.ExtraIndent:=aValue;
    4 : FRenderer.ImageMargin:=aValue;
  end;
end;

procedure TCustomMarkDownControl.SetOnGetImage(AValue: TMarkdownImageEvent);
begin
  FRenderer.OnGetImage:=aValue;
end;

procedure TCustomMarkDownControl.SetString(AIndex: Integer; AValue: string);
begin
  Case aIndex of
    0 : FRenderer.FontName:=aValue;
    1 : FRenderer.MonoFontName:=aValue;
  end;
end;

procedure TCustomMarkDownControl.SetScrollBars(AValue: TScrollStyle);
begin
  if FScrollBars = AValue then Exit;
  FScrollBars := AValue;
  FScrollbarsNeedUpdate:=true;
  UpdateScrollBars;
end;

procedure TCustomMarkDownControl.SetScrolledLeft(AValue: integer);
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

procedure TCustomMarkDownControl.SetScrolledTop(AValue: integer);
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

procedure TCustomMarkDownControl.DoMarkdownChanged(Sender: TObject);
begin
  if not (csLoading in ComponentState) then
    ParseMarkDown;
end;

function TCustomMarkDownControl.GetMaxScrollLeft: integer;
begin
  Result:=FCalculatedWidth-(FLastCalcClientWidth-2*BorderWidth);
  if Result<0 then Result:=0;
end;

function TCustomMarkDownControl.GetMaxScrollTop: integer;
begin
  Result:=FCalculatedHeight-(ClientHeight-2*BorderWidth);
  if Result<0 then Result:=0;
end;

constructor TCustomMarkDownControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMarkDown:=TStringList.Create;
  TStringList(FMarkDown).OnChange:=@DoMarkdownChanged;
  FRenderer:=TMarkDownCanvasRenderer.Create(Nil);
  FDefLineHeight:=FRenderer.BaseFontSize*4 div 3;
  FIsSelecting := False;
  FScrollBars := ssAutoVertical;
  FSelectionStart.LayoutItemIndex := -1;
  FSelectionStart.CharOffset := 0;
  FSBVertShowing:=-1;
  FSBHorzShowing:=-1;
  TabStop := True;
  BorderStyle := bsSingle;
  BorderWidth := 0;
  AutoSize := True;
end;

procedure TCustomMarkDownControl.CalculatePreferredSize(
  var PreferredWidth, PreferredHeight: integer; WithThemeSpace: Boolean);
const
  MaxWidgetSetHeight = 30000;
begin
  PreferredWidth := 0;
  PreferredHeight := Min(FCalculatedHeight + 2*BorderWidth, MaxWidgetSetHeight);
end;

procedure TCustomMarkDownControl.ParseMarkDown;

begin
  FRenderer.ParseMarkdown(FMarkDown);
  CalcLayout;
end;

procedure TCustomMarkDownControl.CalcLayout;
var
  HasVertSB: Boolean;
begin
  if not HandleAllocated then
    begin
    FLastClientWidth:=ClientWidth;
    FCalculatedWidth:=10;
    FCalculatedHeight:=10;
    exit;
    end;

  // compute layout as if there is a vertical scrollbar, to get a stable layout
  HasVertSB:=FSBHorzShowing=ord(true);

  FLastClientWidth:=ClientWidth;
  FLastCalcClientWidth:=FLastClientWidth;
  if not HasVertSB then
    dec(FLastCalcClientWidth,GetSystemMetrics(SM_CXVSCROLL));
  if FLastCalcClientWidth<0 then FLastCalcClientWidth:=0;

  FRenderer.CalculateLayout(Canvas,FLastCalcClientWidth,FCalculatedWidth,FCalculatedHeight);

  FScrollbarsNeedUpdate:=true;
  UpdateScrollBars;
  InvalidatePreferredSize;
  AdjustSize;
end;

procedure TCustomMarkDownControl.Click;
var
  P : TPoint;
  URL : UTF8String;
begin
  inherited Click;
  if assigned(FOnOpenURL) then
    begin
    P:=ScreenToControl(Mouse.CursorPos);
    if FRenderer.HitTestLink(P.X,P.Y,URL) then
      FOnOpenURL(Self,URL);
    end;
end;

procedure TCustomMarkDownControl.CreateWnd;
begin
  FSBHorzShowing:=-1;
  FSBVertShowing:=-1;
  inherited CreateWnd;
end;

procedure TCustomMarkDownControl.DestroyWnd;
begin
  inherited DestroyWnd;
  if Canvas <> nil then
    TControlCanvas(Canvas).FreeHandle;
  FLastHorzScrollInfo.cbSize := 0;
  FLastVertScrollInfo.cbSize := 0;
end;

procedure TCustomMarkDownControl.CopySelectionToClipBoard;

begin
  Clipboard.AsText:=GetSelectionText;
end;

procedure TCustomMarkDownControl.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if (Shift=[ssCtrl]) and ((Key=Ord('C')) or (Key=VK_INSERT)) then
    begin
    Key:=0;
    CopySelectionToClipBoard;
    end
  else
    inherited KeyDown(Key, Shift);
end;

procedure TCustomMarkDownControl.Loaded;
begin
  inherited Loaded;
  ParseMarkDown;
end;

procedure TCustomMarkDownControl.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);

  if Button = mbLeft then
  begin
    // Start text selection
    FSelectionStart := FRenderer.HitTestText(X, Y);
    if FSelectionStart.LayoutItemIndex <> -1 then
    begin
      FIsSelecting := True;
      FRenderer.ClearSelection;
      Invalidate;
    end;
  end;
end;

procedure TCustomMarkDownControl.MouseMove(Shift: TShiftState; X, Y: Integer);

  procedure NormalizeSelection(aStart, aCurrent : TSelectionPoint; out P1, P2 : TSelectionPoint);
  begin
    if aStart.LayoutItemIndex>aCurrent.LayoutItemIndex then
      begin
      p1:=aCurrent;
      p2:=aStart;
      end
    else
      begin
      p1:=aStart;
      p2:=aCurrent;
      end
  end;

var
  CurrentPoint: TSelectionPoint;
  P1,P2 : TSelectionPoint;

begin
  inherited MouseMove(Shift, X, Y);

  if FIsSelecting and (ssLeft in Shift) then
  begin
    CurrentPoint := FRenderer.HitTestText(X, Y);
    if CurrentPoint.LayoutItemIndex <> -1 then
    begin
      NormalizeSelection(FSelectionStart, CurrentPoint, P1, P2);
      FRenderer.SetSelection(P1,P2);
      Invalidate;
    end;
  end;
end;

procedure TCustomMarkDownControl.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);

  if Button = mbLeft then
  begin
    FIsSelecting := False;
  end;
end;

procedure TCustomMarkDownControl.Resize;
begin
  inherited Resize;
  if ClientWidth<>FLastClientWidth then
    CalcLayout;
end;

procedure TCustomMarkDownControl.ScrollView(DeltaX, DeltaY: Integer);
var
  ScrollArea: TRect;
  ScrollFlags: Integer;
begin
  if (DeltaX=0) and (DeltaY=0) then
    Exit;

  FScrollbarsNeedUpdate:=true;
  ScrollFlags := SW_INVALIDATE or SW_ERASE;
  ScrollArea := ClientRect;
  InflateRect(ScrollArea, -BorderWidth, -BorderWidth);
  ScrollWindowEx(Handle, DeltaX, DeltaY, @ScrollArea, @ScrollArea, 0, nil, ScrollFlags);

  UpdateScrollbars;
end;

procedure TCustomMarkDownControl.SetShowScrollBar(Which: Integer; AShow: Boolean);
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

procedure TCustomMarkDownControl.UpdateScrollBars;
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
        //DebugLn(['TCustomMarkDownControl.UpdateScrollbars Hide Horizontal.']);
        ScrollInfo.nPos := 0;
        SetScrollInfo(Handle, SB_HORZ, ScrollInfo, False);
        FLastHorzScrollInfo.cbSize:=0;
        SetShowScrollBar(SB_HORZ, false);
        end
      else
        begin
        //DebugLn(['TCustomMarkDownControl.UpdateScrollbars Show Horizontal: nMin=',ScrollInfo.nMin,
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
        //DebugLn(['TCustomMarkDownControl.UpdateScrollbars Hide Vertical.']);
        ScrollInfo.nPos := 0;
        SetScrollInfo(Handle, SB_Vert, ScrollInfo, False);
        FLastVertScrollInfo.cbSize:=0;
        SetShowScrollBar(SB_Vert, false);
        end
      else
        begin
        //DebugLn(['TCustomMarkDownControl.UpdateScrollbars Show Vertical: nMin=',ScrollInfo.nMin,
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

procedure TCustomMarkDownControl.WMHScroll(var Msg: TLMScroll);
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

procedure TCustomMarkDownControl.WMVScroll(var Msg: TLMScroll);
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

procedure TCustomMarkDownControl.ClearSelection;
begin
  FRenderer.ClearSelection;
  FIsSelecting := False;
  Invalidate;
end;

function TCustomMarkDownControl.GetSelectionText: String;
begin
  Result := FRenderer.GetSelectedText;
end;

function TCustomMarkDownControl.GetSelectionColor: TColor;
begin
  Result := FRenderer.SelectionColor;
end;

procedure TCustomMarkDownControl.SetSelectionColor(AValue: TColor);
begin
  FRenderer.SelectionColor := AValue;
end;

destructor TCustomMarkDownControl.Destroy;
begin
  FreeAndNil(FRenderer);
  FreeAndNil(FMarkDown);
  inherited destroy;
end;

procedure TCustomMarkDownControl.Paint;
begin
  Canvas.Brush.Color:=Color;
  Canvas.Brush.Style:=bsSolid;
  Canvas.FillRect(0,0,Width,Height);
  if (FCalculatedWidth>0) and (FCalculatedHeight>0) then
    begin
    FRenderer.BGColor:=Color;
    FRenderer.DrawLayout(Canvas,-ScrolledLeft,-ScrolledTop);
    end;
end;

end.

