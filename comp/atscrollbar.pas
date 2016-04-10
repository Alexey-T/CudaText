{
ATScrollBar for Delphi/Lazarus
Copyright (c) Alexey Torgashin (UVViewSoft)
License: MPL 2.0

Supports most of needed features:
- default look is flat
- arrow mark of any size
- border of any size
- corner indent (for horz+vert at same form)
- owner-draw (you can paint OS theme)
mouse:
- click and holding mouse on arrows
- click and holding mouse on page-up (area above thumb) / page-down (area below thumb)
- drag of thumb
}

unit ATScrollBar;

interface

{$ifndef FPC}
{$define windows}
{$endif}

uses
  {$ifdef windows}
  Windows, Messages,
  {$endif}
  {$ifdef FPC}
  LCLIntf,
  {$endif}
  Classes, Types, Graphics,
  Controls, ExtCtrls, Forms;

type
  TATScrollElemType = (
    aseArrowUp,
    aseArrowDown,
    aseArrowLeft,
    aseArrowRight,
    aseScrollThumbV,
    aseScrollThumbH,
    aseScrollAreaH,
    aseScrollAreaV,
    aseScrolledAreaH,
    aseScrolledAreaV,
    aseCorner
    );

type
  TATScrollDrawEvent = procedure (Sender: TObject; AType: TATScrollElemType;
    ACanvas: TCanvas; const ARect: TRect; var ACanDraw: boolean) of object;

type
  TATScrollbarTheme = record
    ColorBG: TColor;
    ColorBorder: TColor;
    ColorThumbBorder: TColor;
    ColorThumbFill: TColor;
    ColorArrowBorder: TColor;
    ColorArrowFill: TColor;
    ColorArrowSign: TColor;
    ColorScrolled: TColor;
  end;
var
  ATScrollbarTheme: TATScrollbarTheme;

type
  TATScroll = class(TPanel)
  private
    FKind: TScrollBarKind;
    FIndentBorder: Integer;
    FIndentCorner: Integer;
    FIndentArrow: Integer;
    FIndentArrLonger: Integer;
    FTimerDelay: Integer;

    FPos,
    FMin,
    FMax,
    FPage: Integer;

    //internal
    FIn: TRect; //area for scrolling
    FInUp: TRect; //area for up or left arrow
    FInDown: TRect; //area for down or right arrow
    FInThumb: TRect; //area for scroll-thumb
    FInCorner: TRect;
    FInPageUp: TRect;
    FInPageDown: TRect;

    FBitmap: TBitmap;
    FTimer: TTimer;
    FOnChange: TNotifyEvent;
    FOnOwnerDraw: TATScrollDrawEvent;

    //drag-drop
    FMouseDown: boolean;
    FMouseDragOffset: Integer;
    FMouseDownOnUp,
    FMouseDownOnDown,
    FMouseDownOnThumb,
    FMouseDownOnPageUp,
    FMouseDownOnPageDown: boolean;

    procedure DoPaintArrow(C: TCanvas; const R: TRect; Typ: TATScrollElemType);
    procedure DoPaintThumb(C: TCanvas);
    procedure DoPaintBack(C: TCanvas);
    procedure DoPaintBackScrolled(C: TCanvas);
    procedure DoPaintTo(C: TCanvas);

    procedure DoPaintStd_Corner(C: TCanvas; const R: TRect);
    procedure DoPaintStd_Back(C: TCanvas; const R: TRect);
    procedure DoPaintStd_BackScrolled(C: TCanvas; const R: TRect);
    procedure DoPaintStd_Arrow(C: TCanvas; R: TRect; Typ: TATScrollElemType);
    procedure DoPaintStd_Thumb(C: TCanvas; const R: TRect);

    function IsHorz: boolean;
    function MouseToPos(X, Y: Integer): Integer;
    procedure DoUpdateThumbRect;
    procedure DoUpdateCornerRect;
    procedure DoUpdatePosOnDrag(X, Y: Integer);
    procedure DoScrollBy(NDelta: Integer);
    function GetPxAtScroll(APos: Integer): Integer;

    procedure TimerTimer(Sender: TObject);
    procedure SetKind(Value: TScrollBarKind);
    procedure SetPos(Value: Integer);
    procedure SetMin(Value: Integer);
    procedure SetMax(Value: Integer);
    procedure SetPage(Value: Integer);
    function DoDrawEvent(AType: TATScrollElemType;
      ACanvas: TCanvas; const ARect: TRect): boolean;
  public
    constructor Create(AOnwer: TComponent); override;
    destructor Destroy; override;
    property Position: Integer read FPos write SetPos;
    property Min: Integer read FMin write SetMin;
    property Max: Integer read FMax write SetMax;
    property PageSize: Integer read FPage write SetPage;
  protected
    procedure Paint; override;
    procedure Resize; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure Click; override;
    {$ifdef windows}
    procedure WMEraseBkgnd(var Message: TMessage); message WM_ERASEBKGND;
    {$endif}
  published
    property Kind: TScrollBarKind read FKind write SetKind;
    property IndentBorder: Integer read FIndentBorder write FIndentBorder;
    property IndentCorner: Integer read FIndentCorner write FIndentCorner;
    property IndentArrow: Integer read FIndentArrow write FIndentArrow;
    property IndentArrLonger: Integer read FIndentArrLonger write FIndentArrLonger;
    property TimerDelay: Integer read FTimerDelay write FTimerDelay;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnOwnerDraw: TATScrollDrawEvent read FOnOwnerDraw write FOnOwnerDraw;
  end;

implementation

uses
  SysUtils, Math;

{ TATScroll }

constructor TATScroll.Create(AOnwer: TComponent);
begin
  inherited;

  Caption:= '';
  BorderStyle:= bsNone;
  ControlStyle:= ControlStyle+[csOpaque];
  Width:= 200;
  Height:= 20;

  FKind:= sbHorizontal;
  FIndentBorder:= 1;
  FIndentCorner:= 0;
  FIndentArrow:= 3;
  FIndentArrLonger:= 0;

  FMin:= 0;
  FMax:= 100;
  FPage:= 20;

  Color:= ATScrollbarTheme.ColorBG;

  FBitmap:= TBitmap.Create;
  FBitmap.PixelFormat:= pf24bit;
  FBitmap.Width:= 1600;
  FBitmap.Height:= 60;

  FTimerDelay:= 80;
  FTimer:= TTimer.Create(Self);
  FTimer.Enabled:= false;
  FTimer.Interval:= FTimerDelay;
  FTimer.OnTimer:= {$ifdef fpc}@{$endif} TimerTimer;

  FMouseDown:= false;
  FMouseDragOffset:= 0;
end;

destructor TATScroll.Destroy;
begin
  FTimer.Enabled:= false;
  FreeAndNil(FBitmap);
  inherited;
end;

procedure TATScroll.Paint;
begin
  if Assigned(FBitmap) then
  begin
    DoPaintTo(FBitmap.Canvas);
    Canvas.CopyRect(ClientRect, FBitmap.Canvas, ClientRect);
  end;
end;

procedure TATScroll.DoPaintTo(C: TCanvas);
var
  fSize: Integer;
begin
  FIn:= ClientRect;

  DoUpdateCornerRect;
  if not IsRectEmpty(FInCorner) then
    if DoDrawEvent(aseCorner, C, FInCorner) then
      DoPaintStd_Corner(C, FInCorner);

  C.Brush.Color:= ATScrollbarTheme.ColorBorder;
  C.FillRect(FIn);

  FIn:= Rect(
    FIn.Left+FIndentBorder,
    FIn.Top+FIndentBorder,
    FIn.Right-FIndentBorder,
    FIn.Bottom-FIndentBorder);

  if IsHorz then
  begin
    FSize:= Math.Min(FIn.Bottom-FIn.Top, (FIn.Right-FIn.Left) div 2);
    Inc(FSize, FIndentArrLonger);
    FInUp:= Rect(FIn.Left, FIn.Top, FIn.Left+FSize, FIn.Bottom);
    FInDown:= Rect(FIn.Right-FSize, FIn.Top, FIn.Right, FIn.Bottom);
    DoPaintArrow(C, FInUp, aseArrowLeft);
    DoPaintArrow(C, FInDown, aseArrowRight);
    Inc(FIn.Left, FSize);
    Dec(FIn.Right, FSize);
  end
  else
  begin
    FSize:= Math.Min(FIn.Right-FIn.Left, (FIn.Bottom-FIn.Top) div 2);
    Inc(FSize, FIndentArrLonger);
    FInUp:= Rect(FIn.Left, FIn.Top, FIn.Right, FIn.Top+FSize);
    FInDown:= Rect(FIn.Left, FIn.Bottom-FSize, FIn.Right, FIn.Bottom);
    DoPaintArrow(C, FInUp, aseArrowUp);
    DoPaintArrow(C, FInDown, aseArrowDown);
    Inc(FIn.Top, FSize);
    Dec(FIn.Bottom, FSize);
  end;

  DoPaintBack(C);
  DoUpdateThumbRect;
  DoPaintBackScrolled(C);
  DoPaintThumb(C);
end;

procedure TATScroll.DoPaintBack(C: TCanvas);
var
  Typ: TATScrollElemType;
begin
  if IsHorz then Typ:= aseScrollAreaH else Typ:= aseScrollAreaV;
  if DoDrawEvent(Typ, C, FIn) then
    DoPaintStd_Back(C, FIn);
end;

procedure TATScroll.DoPaintBackScrolled(C: TCanvas);
var
  Typ: TATScrollElemType;
begin
  if IsHorz then Typ:= aseScrolledAreaH else Typ:= aseScrolledAreaV;

  if FMouseDown and FMouseDownOnPageUp then
    if DoDrawEvent(Typ, C, FInPageUp) then
      DoPaintStd_BackScrolled(C, FInPageUp);

  if FMouseDown and FMouseDownOnPageDown then
    if DoDrawEvent(Typ, C, FInPageDown) then
      DoPaintStd_BackScrolled(C, FInPageDown);
end;


procedure TATScroll.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  FMouseDown:= Button=mbLeft;
  FMouseDownOnThumb:= PtInRect(FInThumb, Point(X, Y));
  FMouseDownOnUp:= PtInRect(FInUp, Point(X, Y));
  FMouseDownOnDown:= PtInRect(FInDown, Point(X, Y));
  FMouseDownOnPageUp:= PtInRect(FInPageUp, Point(X, Y));
  FMouseDownOnPageDown:= PtInRect(FInPageDown, Point(X, Y));

  if IsHorz then
    FMouseDragOffset:= X-FInThumb.Left
  else
    FMouseDragOffset:= Y-FInThumb.Top;

  FTimer.Enabled:= FMouseDown and
    (FMouseDownOnUp or
     FMouseDownOnDown or
     FMouseDownOnPageUp or
     FMouseDownOnPageDown);
end;

procedure TATScroll.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  FMouseDown:= false;
  FMouseDownOnThumb:= false;
  FTimer.Enabled:= false;
  Invalidate;
end;

procedure TATScroll.Resize;
begin
  inherited;
  if Assigned(FBitmap) then
  begin
    //little complicated to speed up
    if IsHorz then
    begin
      FBitmap.Width:= Math.Max(FBitmap.Width, Width);
      FBitmap.Height:= Height;
    end
    else
    begin
      FBitmap.Width:= Width;
      FBitmap.Height:= Math.Max(FBitmap.Height, Height);
    end;
  end;
end;


{$ifdef windows}
//needed to remove flickering on resize and mouse-over
procedure TATScroll.WMEraseBkgnd(var Message: TMessage);
begin
  Message.Result:= 1;
end;
{$endif}

procedure TATScroll.Click;
begin
  inherited;
end;

function TATScroll.DoDrawEvent(AType: TATScrollElemType;
  ACanvas: TCanvas; const ARect: TRect): boolean;
begin
  Result:= true;
  if Assigned(FOnOwnerDraw) then
    FOnOwnerDraw(Self, AType, ACanvas, ARect, Result);
end;

procedure TATScroll.SetKind(Value: TScrollBarKind);
begin
  if Value<>FKind then
  begin
    FKind:= Value;
    Invalidate;
  end;
end;

procedure TATScroll.DoPaintArrow(C: TCanvas; const R: TRect;
  Typ: TATScrollElemType);
begin
  if DoDrawEvent(Typ, C, R) then
    DoPaintStd_Arrow(C, R, Typ);
end;    

procedure TATScroll.DoPaintStd_Arrow(C: TCanvas; R: TRect;
  Typ: TATScrollElemType);
var
  P, P1, P2, P3: TPoint;
  cc: Integer;
begin
  C.Brush.Color:= ATScrollbarTheme.ColorArrowBorder;
  C.FillRect(R);

  InflateRect(R, -1, -1);
  C.Brush.Color:= ATScrollbarTheme.ColorArrowFill;
  C.FillRect(R);

  P:= CenterPoint(R);
  cc:= FIndentArrow;

  case Typ of
    aseArrowUp:
      begin
        P1:= Point(P.X-cc, P.Y+cc div 2);
        P2:= Point(P.X+cc, P.Y+cc div 2);
        P3:= Point(P.X, P.Y-cc+cc div 2);
      end;
    aseArrowDown:
      begin
        P1:= Point(P.X-cc, P.Y-cc div 2);
        P2:= Point(P.X+cc, P.Y-cc div 2);
        P3:= Point(P.X, P.Y+cc-cc div 2);
      end;
    aseArrowLeft:
      begin
        P1:= Point(P.X+cc div 2, P.Y-cc);
        P2:= Point(P.X+cc div 2, P.Y+cc);
        P3:= Point(P.X-cc+cc div 2, P.Y);
      end;
    aseArrowRight:
      begin
        P1:= Point(P.X-cc div 2    -1, P.Y-cc);
        P2:= Point(P.X-cc div 2    -1, P.Y+cc);
        P3:= Point(P.X+cc-cc div 2 -1, P.Y);
      end;
    else
      Exit;
 end;     

  C.Brush.Color:= ATScrollbarTheme.ColorArrowSign;
  C.Pen.Color:= ATScrollbarTheme.ColorArrowSign;
  C.Polygon([P1, P2, P3]);
end;

function TATScroll.IsHorz: boolean;
begin
  Result:= FKind=sbHorizontal;
end;

function TATScroll.GetPxAtScroll(APos: Integer): Integer;
var
  N0, NLen: Integer;
begin
  if IsHorz then
  begin
    N0:= FIn.Left;
    NLen:= FIn.Right-FIn.Left
  end
  else
  begin
    N0:= FIn.Top;
    NLen:= FIn.Bottom-FIn.Top;
  end;
  Result:= N0 + (APos-FMin) * NLen div Math.Max(1, FMax-FMin);
end;

procedure TATScroll.DoUpdateThumbRect;
const
  cMinView = 10;
var
  R: TRect;
begin
  FInThumb:= Rect(0, 0, 0, 0);
  FInPageUp:= Rect(0, 0, 0, 0);
  FInPageDown:= Rect(0, 0, 0, 0);

  if IsHorz then
  begin
    if FIn.Right-FIn.Left<cMinView then Exit;
    R.Top:= FIn.Top;
    R.Bottom:= FIn.Bottom;
    R.Left:= GetPxAtScroll(FPos);
    R.Left:= Math.Min(R.Left, FIn.Right-cMinView);
    R.Right:= GetPxAtScroll(FPos+FPage);
    R.Right:= Math.Max(R.Right, R.Left+cMinView);
    R.Right:= Math.Min(R.Right, FIn.Right);
  end
  else
  begin
    if FIn.Bottom-FIn.Top<cMinView then Exit;
    R.Left:= FIn.Left;
    R.Right:= FIn.Right;
    R.Top:= GetPxAtScroll(FPos);
    R.Top:= Math.Min(R.Top, FIn.Bottom-cMinView);
    R.Bottom:= GetPxAtScroll(FPos+FPage);
    R.Bottom:= Math.Max(R.Bottom, R.Top+cMinView);
    R.Bottom:= Math.Min(R.Bottom, FIn.Bottom);
  end;
  FInThumb:= R;

  if IsHorz then
  begin
    FInPageUp:= Rect(FIn.Left, FIn.Top, FInThumb.Left, FIn.Bottom);
    FInPageDown:= Rect(FInThumb.Right, FIn.Top, FIn.Right, FIn.Bottom);
  end
  else
  begin
    FInPageUp:= Rect(FIn.Left, FIn.Top, FIn.Right, FInThumb.Top);
    FInPageDown:= Rect(FIn.Left, FInThumb.Bottom, FIn.Right, FIn.Bottom);
  end;
end;

procedure TATScroll.DoPaintThumb(C: TCanvas);
var
  Typ: TATScrollElemType;
begin
  if IsRectEmpty(FInThumb) then Exit;
  if IsHorz then
    Typ:= aseScrollThumbH
  else
    Typ:= aseScrollThumbV;

  if DoDrawEvent(Typ, C, FInThumb) then
    DoPaintStd_Thumb(C, FInThumb);
end;

procedure TATScroll.DoPaintStd_Thumb(C: TCanvas; const R: TRect);
const
  cMinMark = 20; //minimial size of thumb, after which thumb disappears
  cMarkOf = 4; //offset from thumb edge to "|||" lines
var
  P: TPoint;
begin
  C.Brush.Color:= ATScrollbarTheme.ColorThumbFill;
  C.Pen.Color:= ATScrollbarTheme.ColorThumbBorder;
  C.Rectangle(R);

  P:= CenterPoint(R);
  if IsHorz then
  begin
    if (R.Right-R.Left)>cMinMark then
    begin
      C.MoveTo(P.X  , R.Top+cMarkOf);
      C.LineTo(P.X  , R.Bottom-cMarkOf);
      C.MoveTo(P.X-2, R.Top+cMarkOf);
      C.LineTo(P.X-2, R.Bottom-cMarkOf);
      C.MoveTo(P.X+2, R.Top+cMarkOf);
      C.LineTo(P.X+2, R.Bottom-cMarkOf);
    end;
  end
  else
  begin
    if (R.Bottom-R.Top)>cMinMark then
    begin
      C.MoveTo(R.Left+cMarkOf, P.Y);
      C.LineTo(R.Right-cMarkOf, P.Y);
      C.MoveTo(R.Left+cMarkOf, P.Y-2);
      C.LineTo(R.Right-cMarkOf, P.Y-2);
      C.MoveTo(R.Left+cMarkOf, P.Y+2);
      C.LineTo(R.Right-cMarkOf, P.Y+2);
    end;
  end;
end;


procedure TATScroll.SetMax(Value: Integer);
begin
  if FMax<>Value then
  begin
    FMax:= Value;
    FPos:= Math.Min(FPos, FMax);
    Invalidate;
  end;
end;

procedure TATScroll.SetMin(Value: Integer);
begin
  if FMin<>Value then
  begin
    FMin:= Value;
    FPos:= Math.Max(FPos, FMin);
    Invalidate;
  end;
end;

procedure TATScroll.SetPage(Value: Integer);
begin
  if FPage<>Value then
  begin
    FPage:= Value;
    Invalidate;
  end;
end;

procedure TATScroll.SetPos(Value: Integer);
begin
  Value:= Math.Min(Value, FMax);
  Value:= Math.Max(Value, FMin);
  if FPos<>Value then
  begin
    FPos:= Value;
    Invalidate;
    if Assigned(FOnChange) then
      FOnChange(Self);
  end;
end;

procedure TATScroll.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;

  if FMouseDownOnThumb then
  begin
    DoUpdatePosOnDrag(X, Y);
    Exit
  end;
end;

function TATScroll.MouseToPos(X, Y: Integer): Integer;
begin
  if IsHorz then
    Result:= FMin + (X-FIn.Left) * (FMax-FMin) div Math.Max(FIn.Right-FIn.Left, 1)
  else
    Result:= FMin + (Y-FIn.Top) * (FMax-FMin) div Math.Max(FIn.Bottom-FIn.Top, 1);
end;

procedure TATScroll.DoUpdatePosOnDrag(X, Y: Integer);
var
  N: Integer;
begin
  N:= MouseToPos(
    X-FMouseDragOffset,
    Y-FMouseDragOffset);
  N:= Math.Max(N, FMin);
  N:= Math.Min(N, FMax-FPage);
  SetPos(N);
end;

procedure TATScroll.DoScrollBy(NDelta: Integer);
var
  N: Integer;
begin
  N:= FPos;
  Inc(N, NDelta);
  if (NDelta>0) then
    N:= Math.Min(N, FMax-FPage);
  SetPos(N);
end;

procedure TATScroll.TimerTimer(Sender: TObject);
var
  P: TPoint;
begin
  P:= Mouse.CursorPos;
  P:= ScreenToClient(P);

  if FMouseDownOnDown and PtInRect(FInDown, P) then
    DoScrollBy(1)
  else
  if FMouseDownOnUp and PtInRect(FInUp, P) then
    DoScrollBy(-1)
  else
  if FMouseDownOnPageDown and PtInRect(FInPageDown, P) then
    DoScrollBy(FPage)  
  else
  if FMouseDownOnPageUp and PtInRect(FInPageUp, P) then
    DoScrollBy(-FPage);
end;

procedure TATScroll.DoPaintStd_Corner(C: TCanvas; const R: TRect);
begin
  C.Brush.Color:= ATScrollbarTheme.ColorBG;
  C.FillRect(R);
end;

procedure TATScroll.DoPaintStd_Back(C: TCanvas; const R: TRect);
begin
  C.Brush.Color:= ATScrollbarTheme.ColorBG;
  C.FillRect(R);
end;

procedure TATScroll.DoPaintStd_BackScrolled(C: TCanvas; const R: TRect);
begin
  C.Brush.Color:= ATScrollbarTheme.ColorScrolled;
  C.FillRect(R);
end;

procedure TATScroll.DoUpdateCornerRect;
begin
  FInCorner:= Rect(0, 0, 0, 0);
  if IsHorz then
  begin
    if FIndentCorner>0 then
    begin
      FInCorner:= Rect(ClientWidth-FIndentCorner, 0, ClientWidth, ClientHeight);
      Dec(FIn.Right, FIndentCorner);
    end
    else
    if FIndentCorner<0 then
    begin
      FInCorner:= Rect(0, 0, Abs(FIndentCorner), ClientHeight);
      Inc(FIn.Left, Abs(FIndentCorner));
    end;
  end
  else
  begin
    if FIndentCorner>0 then
    begin
      FInCorner:= Rect(0, ClientHeight-FIndentCorner, ClientWidth, ClientHeight);
      Dec(FIn.Bottom, FIndentCorner);
    end
    else
    if FIndentCorner<0 then
    begin
      FInCorner:= Rect(0, 0, ClientWidth, Abs(FIndentCorner));
      Inc(FIn.Top, Abs(FIndentCorner));
    end;
  end;
end;

initialization
  ATScrollbarTheme.ColorBorder:= clLtGray;
  ATScrollbarTheme.ColorThumbBorder:= $808080;
  ATScrollbarTheme.ColorThumbFill:= $c0c0c0;
  ATScrollbarTheme.ColorArrowBorder:= $808080;
  ATScrollbarTheme.ColorArrowFill:= $c0c0c0;
  ATScrollbarTheme.ColorArrowSign:= $404040;
  ATScrollbarTheme.ColorScrolled:= $c8c8c8;

end.
