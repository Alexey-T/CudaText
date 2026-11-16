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
    FLastCalcClientHeight : integer;
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
    FLastClickCount: integer; // 1=single, 2=double, 3+=triple (reset on move/time)
    FLastClickTime: TDateTime;
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
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean; override;
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
    property FileDir: string index 100 read GetString write SetString;
    property FontName: string index 0 read GetString write SetString;
    property MonoFontName: string index 1 read GetString write SetString;
    property FontColor: TColor index 0 read GetColor write SetRenderColor;
    property FontCodeColor: TColor index 1 read GetColor write SetRenderColor;
    property FontQuoteColor: TColor index 2 read GetColor write SetRenderColor;
    property HyperLinkColor: TColor index 3 read GetColor write SetRenderColor;
    property BGCodeColor: TColor index 4 read GetColor write SetRenderColor;
    property TableLinesColor: TColor index 5 read GetColor write SetRenderColor;
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
    property TableLinesColor;
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
    5 : Result:=FRenderer.TableLinesColor;
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
    100 : Result:= FRenderer.FileDir;
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
    5 : FRenderer.TableLinesColor:=aValue;
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
    100 : FRenderer.FileDir:=aValue;
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
  // same as vertical: keep scrollbar in sync
  UpdateScrollBars;
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
  // scrollbar thumb must follow programmatic scroll too
  // (found by AI: scrollbar stayed at the old position when ScrolledTop
  // was set in code, e.g. after loading a shorter document)
  UpdateScrollBars;
end;

procedure TCustomMarkDownControl.DoMarkdownChanged(Sender: TObject);
begin
  if not (csLoading in ComponentState) then
  begin
    // new document: scroll to the begin and repaint
    // (fixes Lazarus work item 42722: after LoadFromFile the control
    // kept the old scroll position and was not repainted)
    ScrolledTop:= 0;
    ScrolledLeft:= 0;
    ParseMarkDown;
    Invalidate;
  end;
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
  // the canvas renderer must not paint with its default (white-ish)
  // background before the first Paint() syncs it with Color
  // (fixes Lazarus work item 42722: white areas at first paint)
  FRenderer.BGColor:= Color;
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

  // Reserve the vertical scrollbar strip when the bar is already shown
  // or WILL be needed (document taller than the client area), so the
  // first layout already uses the wrap width the control has with its
  // scrollbar visible.
  // The old code read the HORZ flag (`FSBHorzShowing`) here: the strip
  // was never reserved on the first layout of a tall document, the
  // scrollbar then appeared, WM_SIZE shrank ClientWidth, Resize
  // re-layouted at the smaller width, the content became taller and
  // the scrollbar range GREW after the first drag -- the user had to
  // drag a second time to reach the document end
  // (Lazarus work item 42737: initial scrolling cannot reach the end).
  HasVertSB:=(FSBVertShowing=ord(true)) or
             (FCalculatedHeight+2*BorderWidth > ClientHeight);

  FLastClientWidth:=ClientWidth;
  FLastCalcClientWidth:=FLastClientWidth;
  FLastCalcClientHeight:=ClientHeight;
  if not HasVertSB then
    dec(FLastCalcClientWidth,GetSystemMetrics(SM_CXVSCROLL));
  if FLastCalcClientWidth<0 then FLastCalcClientWidth:=0;

  FRenderer.CalculateLayout(Canvas,FLastCalcClientWidth,FCalculatedWidth,FCalculatedHeight);

  // Second pass: if the document turned out taller than the client
  // area and the strip was NOT reserved above, redo the layout WITH
  // the strip width, exactly as the control will be sized once
  // UpdateScrollBars shows the bar. Without this the scrollbar range
  // computed here is provisional, and the bar appearing (WM_SIZE,
  // Resize, re-layout at the smaller width, taller content) would
  // change the range after the user's first drag to the end (42737).
  if (not HasVertSB) and
     (FCalculatedHeight+2*BorderWidth > ClientHeight) then
    begin
    FLastCalcClientWidth:=FLastClientWidth-GetSystemMetrics(SM_CXVSCROLL);
    if FLastCalcClientWidth<0 then FLastCalcClientWidth:=0;
    FRenderer.CalculateLayout(Canvas,FLastCalcClientWidth,FCalculatedWidth,FCalculatedHeight);
    end;

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
    // client->document mapping, as in MouseDown: link rectangles are
    // stored in document coordinates too, so after scrolling a click
    // would otherwise open the link that was at that client position
    // before the scroll
    P:=ScreenToControl(Mouse.CursorPos);
    if FRenderer.HitTestLink(P.X+ScrolledLeft, P.Y+ScrolledTop, URL) then
      FOnOpenURL(Self,URL);
    end;
end;

procedure TCustomMarkDownControl.CreateWnd;
begin
  FSBHorzShowing:=-1;
  FSBVertShowing:=-1;
  FLastCalcClientHeight:=-1;
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
  if not (ssAlt in Shift) then
    begin
    // keyboard scrolling, mirroring WMVScroll/WMHScroll codes
    // (fixes CudaText issue 6643: PageUp/PageDown keys had no effect)
    case Key of
      VK_NEXT: // PageDown
        if ssShift in Shift then
          begin
          Key:=0;
          ScrolledLeft:=ScrolledLeft + ClientWidth - FDefLineHeight;
          end
        else
          begin
          Key:=0;
          ScrolledTop:=ScrolledTop + ClientHeight - FDefLineHeight;
          end;
      VK_PRIOR: // PageUp
        if ssShift in Shift then
          begin
          Key:=0;
          ScrolledLeft:=ScrolledLeft - ClientWidth + FDefLineHeight;
          end
        else
          begin
          Key:=0;
          ScrolledTop:=ScrolledTop - ClientHeight + FDefLineHeight;
          end;
      VK_DOWN:
        begin
        Key:=0;
        ScrolledTop:=ScrolledTop + FDefLineHeight;
        end;
      VK_UP:
        begin
        Key:=0;
        ScrolledTop:=ScrolledTop - FDefLineHeight;
        end;
      VK_RIGHT:
        begin
        Key:=0;
        ScrolledLeft:=ScrolledLeft + FDefLineHeight div 2;
        end;
      VK_LEFT:
        begin
        Key:=0;
        ScrolledLeft:=ScrolledLeft - FDefLineHeight div 2;
        end;
      VK_HOME:
        begin
        Key:=0;
        if ssCtrl in Shift then
          ScrolledTop:=0
        else
          ScrolledLeft:=0;
        end;
      VK_END:
        begin
        Key:=0;
        if ssCtrl in Shift then
          ScrolledTop:=GetMaxScrollTop
        else
          ScrolledLeft:=GetMaxScrollLeft;
        end;
    else
      inherited KeyDown(Key, Shift);
    end;
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

  // The control takes keyboard input (Ctrl+C copy etc) but Win32 does
  // not focus a user-drawn child window on click by itself: after the
  // user clicks ANY other control (e.g. a "load file..." button that
  // opens a dialog), KeyDown/Ctrl+C stopped reaching the control and
  // nothing was copied, although the selection itself worked.
  // Take focus on click like the reference editors do (SynEdit calls
  // SetFocus in MouseDown for the same reason).
  if CanFocus and not Focused then
    SetFocus;

  if Button = mbLeft then
  begin
    // Start text selection.
    // X,Y are CLIENT coordinates; layout items live in DOCUMENT
    // coordinates (Paint draws them at -ScrolledLeft,-ScrolledTop).
    // Map the point before hit-testing, otherwise after scrolling the
    // hit lands on the row that was at that client position BEFORE
    // the scroll: selecting the visible "444" selected "ggg" etc.
    FSelectionStart := FRenderer.HitTestText(X+ScrolledLeft, Y+ScrolledTop);
    if FSelectionStart.LayoutItemIndex <> -1 then
    begin
      // Shift+Click EXTENDS the selection to the clicked point; the
      // anchor stays. No selection yet: the click becomes the anchor
      // (fixes: shift+click cleared the selection and selected
      // nothing). Without Shift the plain-click path below runs as
      // before.
      if (ssShift in Shift) then
        begin
        FRenderer.ExtendSelectionTo(FSelectionStart);
        FIsSelecting:= False;
        Invalidate;
        Exit;
        end;

      FIsSelecting := True;
      FRenderer.ClearSelection;
      // click counting for double/triple selection
      // (fixes CudaText issue 6642: double click must select the word,
      // triple click the whole paragraph)
      if (Now - FLastClickTime) * 86400.0 > GetDoubleClickTime / 1000.0 then
        FLastClickCount:= 0;
      FLastClickTime:= Now;
      if FLastClickCount >= 3 then
        FLastClickCount:= 0; // 4th click restarts the sequence
      Inc(FLastClickCount);
      case FLastClickCount of
        2: // double click: select the word under the cursor
          begin
          FRenderer.SelectWordAtPoint(FSelectionStart);
          FIsSelecting:= False;
          end;
        3: // triple click: select the whole paragraph (line group)
          begin
          FRenderer.SelectParagraphAtPoint(FSelectionStart);
          FIsSelecting:= False;
          end;
      end;
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
    // same client->document mapping as in MouseDown: hit-testing
    // must use document coordinates (see comment there)
    CurrentPoint := FRenderer.HitTestText(X+ScrolledLeft, Y+ScrolledTop);
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
    CalcLayout
  else
    begin
    // height changed with the same wrap width: the content layout is
    // still valid, but the scrollbar range (nPage/nMax) was computed
    // with the old ClientHeight and must be refreshed
    // (fixes Lazarus work item 42737: vert-scrollbar Max stayed too
    // small after loading a bigger document, until the user dragged it)
    if ClientHeight<>FLastCalcClientHeight then
      begin
      FLastCalcClientHeight:=ClientHeight;
      FScrollbarsNeedUpdate:=true;
      UpdateScrollBars;
      Invalidate;
      end;
    end;
end;

procedure TCustomMarkDownControl.ScrollView(DeltaX, DeltaY: Integer);
var
  ScrollArea: TRect;
  ScrollFlags: Integer;
begin
  if (DeltaX=0) and (DeltaY=0) then
    Exit;

  // handleless (e.g. created from code before parenting): ScrollWindowEx
  // needs a real window handle, otherwise LCL raises
  // "Control has no parent window" (part of the work item 42736 fix)
  if not HandleAllocated then
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

function TCustomMarkDownControl.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean;
begin
  // scroll by whole lines, like other Lazarus controls:
  // Mouse.WheelScrollLines is the OS-configured value read by the LCL
  // (fixes Lazarus work item 42733: wheel scrolled only a few pixels)
  Result:= True;
  if WheelDelta=0 then Exit;
  if ssShift in Shift then
    ScrolledLeft:= ScrolledLeft - WheelDelta * Mouse.WheelScrollLines * FDefLineHeight div 120
  else
    ScrolledTop:= ScrolledTop - WheelDelta * Mouse.WheelScrollLines * FDefLineHeight div 120;
  UpdateScrollBars;
  Invalidate;
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

