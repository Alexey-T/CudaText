{
ATImageBox for Lazarus
Copyright (C) Alexey Torgashin, uvviewsoft.com
License: MPL 2.0 or LGPL
}

unit ATImageBox;

{$mode objfpc}{$H+}
{$define USE_BGRA}

interface

uses
  SysUtils, Classes, Controls, Graphics,
  StdCtrls, ExtCtrls,
  LMessages,
  LCLType,
  Forms,
  {$ifdef USE_BGRA}
  BGRABitmap,
  {$endif}
  Math;

const
  cImageboxZooms: array[1..33] of integer = (
    1, 2, 4, 7, 10, 15, 20, 25, 30,
    40, 50, 60, 70, 80, 90, 100,
    125, 150, 175, 200, 250, 300, 350, 400, 450, 500,
    600, 700, 800, 1000, 1200, 1400, 1600);

type
  { TATImageBox }

  TATImageBox = class(TScrollBox)
  private
    FFocusable: boolean;
    FImage: TImage;
    FImageFit,
    FImageFitOnlyBig,
    FImageFitWidth,
    FImageFitHeight,
    FImageCenter: boolean;
    FImageZoom: integer;
    FImageZoomMin: integer;
    FImageZoomMax: integer;
    FImageKeepPosition: boolean;
    FCursorDrag: TCursor;
    FCursorZoom: TCursor;
    FDrag: boolean;
    FDragging: boolean;
    FDraggingPoint: TPoint;
    FMouseDown: boolean;
    FModifierMouseZoom: TShiftStateEnum;
    FModifierMouseHorzScroll: TShiftStateEnum;
    FModifierArrowsToEdge: TShiftStateEnum;
    FInitScrollbarSize: integer;
    FCheckers: boolean;
    FCheckersSize: integer;
    FCheckersColor1: TColor;
    FCheckersColor2: TColor;
    FScrollSmallStep: integer;
    FScrollGapSize: integer;

    FOnScroll: TNotifyEvent;
    FOnOptionsChange: TNotifyEvent;
    FOnImageResize: TNotifyEvent;

    FOldLeft: integer;
    FOldTop: integer;
    FOldWidth: integer;
    FOldHeight: integer;
    FOldSelfW: integer;
    FOldSelfH: integer;
    FImageResized: boolean;

    procedure DoEventScroll;
    procedure DoEventOptionsChange;
    procedure DoEventImageResize;
    function GetImageHeight: integer;
    function GetImageWidth: integer;
    function GetPageSize(AClientSize: integer): integer;
    procedure MouseWheelUp(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: boolean);
    procedure MouseWheelDown(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: boolean);
    procedure SetCheckers(AValue: boolean);
    procedure SetCheckersColor1(AValue: TColor);
    procedure SetCheckersColor2(AValue: TColor);
    procedure SetCheckersSize(AValue: integer);
    procedure UpdateImagePosition(AResetPosition: boolean = False);
    procedure SetImageFit(AValue: boolean);
    procedure SetImageFitOnlyBig(AValue: boolean);
    procedure SetImageFitWidth(AValue: boolean);
    procedure SetImageFitHeight(AValue: boolean);
    procedure SetImageCenter(AValue: boolean);
    procedure SetImageZoom(AValue: integer);
    procedure ImageMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
    procedure ImageMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
    procedure ImageMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
    procedure ImagePaintBackground(ASender: TObject; ACanvas: TCanvas; ARect: TRect);
    function GetPicture: TPicture;

  public
    constructor Create(AOwner: TComponent); override;
    procedure LoadFromFile(const AFileName: string);
    procedure LoadBitmap(ABitmap: TBitmap; ATransp: boolean);
    procedure LoadPicture(APicture: TPicture);
    procedure Clear;
    procedure UpdateInfo;
    procedure IncreaseImageZoom(AIncrement: boolean);
    property Image: TImage read FImage;
    property ImageWidth: integer read GetImageWidth;
    property ImageHeight: integer read GetImageHeight;
    property ImageZoom: integer read FImageZoom write SetImageZoom;

  protected
    procedure WMHScroll(var Message: TLMHScroll); message LM_HScroll;
    procedure WMVScroll(var Message: TLMVScroll); message LM_VScroll;
    procedure WMGetDlgCode(var Message: TLMessage); message LM_GETDLGCODE;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure Resize; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: integer); override;
    procedure Loaded; override;

  published
    property Picture: TPicture read GetPicture write LoadPicture;
    property CursorDrag: TCursor read FCursorDrag write FCursorDrag default crSizeAll;
    property CursorZoom: TCursor read FCursorZoom write FCursorZoom default crSizeNS;
    property OptFocusable: boolean read FFocusable write FFocusable default True;
    property OptZoomMin: integer read FImageZoomMin write FImageZoomMin default 1;
    property OptZoomMax: integer read FImageZoomMax write FImageZoomMax default 1600;
    property OptFitToWindow: boolean read FImageFit write SetImageFit default False;
    property OptFitOnlyBig: boolean read FImageFitOnlyBig write SetImageFitOnlyBig default True;
    property OptFitWidth: boolean read FImageFitWidth write SetImageFitWidth default False;
    property OptFitHeight: boolean read FImageFitHeight write SetImageFitHeight default False;
    property OptCenter: boolean read FImageCenter write SetImageCenter default True;
    property OptKeepPosition: boolean read FImageKeepPosition write FImageKeepPosition default True;
    property OptDrag: boolean read FDrag write FDrag default True;
    property OptModifierMouseZoom: TShiftStateEnum read FModifierMouseZoom write FModifierMouseZoom default ssModifier;
    property OptModifierMouseHorzScroll: TShiftStateEnum read FModifierMouseHorzScroll write FModifierMouseHorzScroll default ssShift;
    property OptModifierArrowsToEdge: TShiftStateEnum read FModifierArrowsToEdge write FModifierArrowsToEdge default ssModifier;
    property OptCheckers: boolean read FCheckers write SetCheckers default true;
    property OptChechersSize: integer read FCheckersSize write SetCheckersSize default 8;
    property OptCheckersColor1: TColor read FCheckersColor1 write SetCheckersColor1 default clWhite;
    property OptCheckersColor2: TColor read FCheckersColor2 write SetCheckersColor2 default clLtGray;
    property OptScrollSmallStep: integer read FScrollSmallStep write FScrollSmallStep default 50;
    property OptScrollBigStepDecrement: integer read FScrollGapSize write FScrollGapSize default 20;

    property OnScroll: TNotifyEvent read FOnScroll write FOnScroll;
    property OnOptionsChange: TNotifyEvent read FOnOptionsChange write FOnOptionsChange;
    property OnImageResize: TNotifyEvent read FOnImageResize write FOnImageResize;
  end;


implementation

procedure DoPaintCheckers(C: TCanvas;
  ASizeX, ASizeY: integer;
  ACellSize: integer;
  AColor1, AColor2: TColor);
var
  i, j: integer;
begin
  c.Brush.Color:= AColor1;
  c.FillRect(0, 0, ASizeX, ASizeY);

  for i:= 0 to ASizeX div ACellSize + 1 do
    for j:= 0 to ASizeY div ACellSize + 1 do
      if odd(i) xor odd(j) then
      begin
        c.Brush.Color:= AColor2;
        c.FillRect(i*ACellSize, j*ACellSize, (i+1)*ACellSize, (j+1)*ACellSize);
      end;
end;


{ TATImageBox }

constructor TATImageBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  AutoScroll:= False;
  DoubleBuffered:= True;
  HorzScrollBar.Tracking:= True;
  VertScrollBar.Tracking:= True;

  FFocusable:= True;
  FImageFit:= False;
  FImageFitOnlyBig:= True;
  FImageFitWidth:= False;
  FImageFitHeight:= False;
  FImageCenter:= True;
  FImageZoom:= 100;
  FImageZoomMin:= 1;
  FImageZoomMax:= 1600;
  FImageKeepPosition:= True;
  FDrag:= True;
  FCursorDrag:= crSizeAll;
  FCursorZoom:= crSizeNS;
  FDragging:= False;
  FDraggingPoint:= Point(0, 0);
  FMouseDown:= False;

  FModifierMouseZoom:= ssModifier;
  FModifierMouseHorzScroll:= ssShift;
  FModifierArrowsToEdge:= ssModifier;

  FCheckers:= true;
  FCheckersSize:= 8;
  FCheckersColor1:= clWhite;
  FCheckersColor2:= clLtGray;
  FScrollSmallStep:= 50;
  FScrollGapSize:= 20;

  FImage:= TImage.Create(Self);
  with FImage do
  begin
    Parent:= Self;
    Align:= alNone;
    SetBounds(0, 0, 100, 100);
    AutoSize:= False;
    OnMouseDown:= @ImageMouseDown;
    OnMouseUp:= @ImageMouseUp;
    OnMouseMove:= @ImageMouseMove;
    OnPaintBackground:= @ImagePaintBackground;
  end;

  OnMouseWheelUp:= @MouseWheelUp;
  OnMouseWheelDown:= @MouseWheelDown;

  with TScrollBar.Create(Self) do
  try
    Kind:= sbVertical;
    FInitScrollbarSize:= Width;
  finally
    Free;
  end;
end;

procedure TATImageBox.SetCheckers(AValue: boolean);
begin
  if FCheckers<>AValue then
  begin
    FCheckers:= AValue;
    if FCheckers then
      FImage.OnPaintBackground:= @ImagePaintBackground
    else
      FImage.OnPaintBackground:= nil;
    FImage.Invalidate;
  end;
end;

procedure TATImageBox.WMHScroll(var Message: TLMHScroll);
begin
  inherited;
  DoEventScroll;
end;

procedure TATImageBox.WMVScroll(var Message: TLMVScroll);
begin
  inherited;
  DoEventScroll;
end;

procedure TATImageBox.MouseWheelUp(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: boolean);
begin
  if (Shift = []) then
  begin
    with VertScrollBar do
      Position:= Max(0, Position - FScrollSmallStep);
    DoEventScroll;
  end
  else
  if (Shift = [FModifierMouseHorzScroll]) then
  begin
    with HorzScrollBar do
      Position:= Max(0, Position - FScrollSmallStep);
    DoEventScroll;
  end
  else
  if (Shift = [FModifierMouseZoom]) or FMouseDown then
  begin
    IncreaseImageZoom(True);
    FDragging:= False;
    if FMouseDown then
      Screen.Cursor:= FCursorZoom;
  end;

  Handled:= True;
end;

procedure TATImageBox.MouseWheelDown(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: boolean);
begin
  if (Shift = []) then
  begin
    with VertScrollBar do
      Position:= Max(0, Min(Range-Page, Position + FScrollSmallStep));
    DoEventScroll;
  end
  else
  if (Shift = [FModifierMouseHorzScroll]) then
  begin
    with HorzScrollBar do
      Position:= Max(0, Min(Range-Page, Position + FScrollSmallStep));
    DoEventScroll;
  end
  else
  if (Shift = [FModifierMouseZoom]) or FMouseDown then
  begin
    IncreaseImageZoom(False);
    FDragging:= False;
    if FMouseDown then
      Screen.Cursor:= FCursorZoom;
  end;

  Handled:= True;
end;

procedure TATImageBox.SetCheckersColor1(AValue: TColor);
begin
  if FCheckersColor1= AValue then Exit;
  FCheckersColor1:= AValue;
  FImage.Invalidate;
end;

procedure TATImageBox.SetCheckersColor2(AValue: TColor);
begin
  if FCheckersColor2= AValue then Exit;
  FCheckersColor2:= AValue;
  FImage.Invalidate;
end;

procedure TATImageBox.SetCheckersSize(AValue: integer);
begin
  if FCheckersSize=AValue then Exit;
  FCheckersSize:= AValue;
  FImage.Invalidate;
end;

procedure TATImageBox.WMGetDlgCode(var Message: TLMessage);
begin
  Message.Result:= DLGC_WANTARROWS;
end;

function TATImageBox.GetPageSize(AClientSize: integer): integer;
begin
  Result:= Max(AClientSize - FScrollGapSize, AClientSize div 3 * 2);
end;

procedure TATImageBox.KeyDown(var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_LEFT:
    begin
      if Shift = [] then
      begin
        with HorzScrollBar do
          Position:= Position - FScrollSmallStep;
        DoEventScroll;
        Key:= 0;
      end
      else
      if Shift = [FModifierArrowsToEdge] then
      begin
        with HorzScrollBar do
          Position:= 0;
        DoEventScroll;
        Key:= 0;
      end;
    end;

    VK_RIGHT:
    begin
      if Shift = [] then
      begin
        with HorzScrollBar do
          Position:= Position + FScrollSmallStep;
        DoEventScroll;
        Key:= 0;
      end
      else
      if Shift = [FModifierArrowsToEdge] then
      begin
        with HorzScrollBar do
          Position:= Range;
        DoEventScroll;
        Key:= 0;
      end;
    end;

    VK_HOME:
      if Shift = [] then
      begin
        with HorzScrollBar do
          Position:= Position - GetPageSize(ClientWidth);
        DoEventScroll;
        Key:= 0;
      end;

    VK_END:
      if Shift = [] then
      begin
        with HorzScrollBar do
          Position:= Position + GetPageSize(ClientWidth);
        DoEventScroll;
        Key:= 0;
      end;

    VK_UP:
    begin
      if Shift = [] then
      begin
        with VertScrollBar do
          Position:= Position - FScrollSmallStep;
        DoEventScroll;
        Key:= 0;
      end
      else
      if Shift = [FModifierArrowsToEdge] then
      begin
        with VertScrollBar do
          Position:= 0;
        DoEventScroll;
        Key:= 0;
      end;
    end;

    VK_DOWN:
    begin
      if Shift = [] then
      begin
        with VertScrollBar do
          Position:= Position + FScrollSmallStep;
        DoEventScroll;
        Key:= 0;
      end
      else
      if Shift = [FModifierArrowsToEdge] then
      begin
        with VertScrollBar do
          Position:= Range;
        DoEventScroll;
        Key:= 0;
      end;
    end;

    VK_PRIOR:
      if Shift = [] then
      begin
        with VertScrollBar do
          Position:= Position - GetPageSize(ClientHeight);
        DoEventScroll;
        Key:= 0;
      end;

    VK_NEXT:
      if Shift = [] then
      begin
        with VertScrollBar do
          Position:= Position + GetPageSize(ClientHeight);
        DoEventScroll;
        Key:= 0;
      end;
  end;

  inherited;
end;


procedure TATImageBox.UpdateImagePosition(AResetPosition: boolean = False);
var
  bKeepPosition: boolean;
  PicWidth, PicHeight,
  CliWidth, CliHeight,
  NewWidth, NewHeight, NewLeft, NewTop,
  ScrollMaxX, ScrollMaxY: integer;
  NRatio, NImageRatio, CenterRatioX, CenterRatioY: Double;
  NScrollbarSize: integer;
begin
  bKeepPosition:= FImageKeepPosition and not AResetPosition;

  PicWidth:= ImageWidth;
  PicHeight:= ImageHeight;
  if PicWidth=0 then exit;
  if PicHeight=0 then exit;

  if FImageFit then
    NScrollbarSize:= 0
  else
    NScrollbarSize:= FInitScrollbarSize;

  VertScrollBar.Visible:= not FImageFit;
  HorzScrollBar.Visible:= not FImageFit;
  CliWidth:= Width-NScrollbarSize;
  CliHeight:= Height-NScrollbarSize;

  //Save center position, need to restore it later
  CenterRatioX:= 0;
  CenterRatioY:= 0;

  if FImage.Width > 0 then
  begin
    if FImage.Left >= 0 then
      CenterRatioX:= (CliWidth div 2 - FImage.Left) / FImage.Width
    else
      CenterRatioX:= (CliWidth div 2 + HorzScrollBar.Position) / FImage.Width;
  end;

  if FImage.Height > 0 then
  begin
    if FImage.Top >= 0 then
      CenterRatioY:= (CliHeight div 2 - FImage.Top) / FImage.Height
    else
      CenterRatioY:= (CliHeight div 2 + VertScrollBar.Position) / FImage.Height;
  end;

  if not bKeepPosition then
  begin
    HorzScrollBar.Position:= 0;
    VertScrollBar.Position:= 0;
  end;

  AutoScroll:= not FImageFit;
  FImage.AutoSize:= (not FImageFit) and (FImageZoom=100);
  FImage.Stretch:= not FImage.AutoSize;

  if FImageFit then
  begin
    NewWidth:= PicWidth;
    NewHeight:= PicHeight;

    if FImageFitOnlyBig and
      (PicWidth <= CliWidth) and (PicHeight <= CliHeight) then
    begin
      FImageZoom:= 100;
    end
    else
    begin
      if (CliWidth > 0) and (CliHeight > 0) then
      begin
        NRatio:= CliWidth / CliHeight;
        NImageRatio:= PicWidth / PicHeight;
        if ((NRatio >= NImageRatio) and (not FImageFitWidth)) or FImageFitHeight then
        begin
          //fit height
          if FImageFitOnlyBig and (CliHeight >= PicHeight) then begin end
          else
          begin
            NewHeight:= CliHeight;
            NewWidth:= Trunc(NewHeight * NImageRatio);
            FImageZoom:= CliHeight * 100 div PicHeight;
          end;
        end
        else
        begin
          //fit width
          if FImageFitOnlyBig and (CliWidth >= PicWidth) then begin end
          else
          begin
            NewWidth:= CliWidth;
            NewHeight:= Trunc(NewWidth / NImageRatio);
            FImageZoom:= CliWidth * 100 div PicWidth;
          end;
        end;
      end;
    end
  end //if FImageFit
  else
  begin
    NewWidth:= Round(PicWidth * FImageZoom / 100);
    NewHeight:= Round(PicHeight * FImageZoom / 100);
  end;

  //Update image position
  NewLeft:= 0;
  NewTop:= 0;

  if FImageCenter then
  begin
    if CliWidth > NewWidth then
      NewLeft:= (CliWidth - NewWidth) div 2;
    if CliHeight > NewHeight then
      NewTop:= (CliHeight - NewHeight) div 2;
  end;

  FImageResized:=
    (FOldWidth<>NewWidth) or
    (FOldHeight<>NewHeight);

  if FImageResized or
    (FOldLeft<>NewLeft - HorzScrollBar.Position) or
    (FOldTop<>NewTop - VertScrollBar.Position) then
  begin
    FOldLeft:= NewLeft - HorzScrollBar.Position;
    FOldTop:= NewTop - VertScrollBar.Position;
    FOldWidth:= NewWidth;
    FOldHeight:= NewHeight;
    FImage.SetBounds(
      FOldLeft,
      FOldTop,
      FOldWidth,
      FOldHeight);
  end;

  //Restore saved center position
  if bKeepPosition then
  begin
    if NewLeft = 0 then
    begin
      ScrollMaxX:= Max(NewWidth - CliWidth, 0);
      HorzScrollBar.Position:=
        Min(ScrollMaxX, Trunc(CenterRatioX * NewWidth) - CliWidth div 2);
    end
    else
      HorzScrollBar.Position:= 0;

    if NewTop = 0 then
    begin
      ScrollMaxY:= Max(NewHeight - CliHeight, 0);
      VertScrollBar.Position:=
        Min(ScrollMaxY, Trunc(CenterRatioY * NewHeight) - CliHeight div 2);
    end
    else
      VertScrollBar.Position:= 0;
  end;

  //adjust range
  if HorzScrollbar.Visible then
    HorzScrollbar.Range:= NewWidth;
  if VertScrollBar.Visible then
    VertScrollbar.Range:= NewHeight;

  if FImageResized then
  begin
    FImageResized:= false;
    DoEventImageResize;
  end;

  DoEventScroll;
end;

procedure TATImageBox.SetImageFit(AValue: boolean);
begin
  if AValue <> FImageFit then
  begin
    FImageFit:= AValue;
    if not FImageFit then
      FImageZoom:= 100;
    UpdateImagePosition(True);
  end;
end;

procedure TATImageBox.SetImageFitOnlyBig(AValue: boolean);
begin
  if AValue <> FImageFitOnlyBig then
  begin
    FImageFitOnlyBig:= AValue;
    UpdateImagePosition(True);
  end;
end;

procedure TATImageBox.SetImageCenter(AValue: boolean);
begin
  if AValue <> FImageCenter then
  begin
    FImageCenter:= AValue;
    UpdateImagePosition(True);
  end;
end;

procedure TATImageBox.UpdateInfo;
begin
  FImageZoom:= 100;
  FImage.Visible:= true;

  if Assigned(FImage.Picture) and Assigned(FImage.Picture.Graphic) then
    UpdateImagePosition(True);
end;

procedure TATImageBox.Resize;
begin
  inherited;
  if Assigned(FImage) and
    ((FOldSelfW <> Self.Width) or (FOldSelfH <> Self.Height)) then
  begin
    FOldSelfW:= Self.Width;
    FOldSelfH:= Self.Height;
    UpdateImagePosition;
  end;
end;

procedure TATImageBox.SetImageZoom(AValue: integer);
begin
  if AValue<=0 then exit;
  if AValue<FImageZoomMin then
    AValue:= FImageZoomMin;
  if AValue>FImageZoomMax then
    AValue:= FImageZoomMax;

  if AValue<>FImageZoom then
  begin
    FImageZoom:= AValue;
    FImageFit:= False;
    HorzScrollBar.Position:= 0;
    VertScrollBar.Position:= 0;
    UpdateImagePosition;
  end;
end;

procedure TATImageBox.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  inherited;

  if FFocusable then
    SetFocus;
end;

procedure TATImageBox.Loaded;
begin
  inherited;
  UpdateInfo;
end;

procedure TATImageBox.ImageMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
var
  P: TPoint;
begin
  if FFocusable then
    SetFocus;

  if (Button = mbLeft) then
  begin
    FMouseDown:= True;
    if FDrag then
    begin
      FDragging:= True;
      FDraggingPoint:= Point(X, Y);
      Screen.Cursor:= FCursorDrag;
    end;
  end;

  P.X:= X+FImage.Left;
  P.Y:= Y+FImage.Top;
  Self.MouseDown(Button, Shift, P.X, P.Y);
end;

procedure TATImageBox.ImageMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
var
  P: TPoint;
begin
  if (Button = mbLeft) then
  begin
    FMouseDown:= False;
    FDragging:= False;
    Screen.Cursor:= crDefault;
  end;

  P.X:= X+FImage.Left;
  P.Y:= Y+FImage.Top;
  Self.MouseUp(Button, Shift, P.X, P.Y);
end;

procedure TATImageBox.ImageMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
var
  bAllowX, bAllowY: boolean;
  P: TPoint;
begin
  if FDrag and FDragging then
  begin
    bAllowX:= FImage.Width>ClientWidth;
    bAllowY:= FImage.Height>ClientHeight;

    if bAllowX then
      HorzScrollBar.Position:= HorzScrollBar.Position + (FDraggingPoint.X - X);
    if bAllowY then
      VertScrollBar.Position:= VertScrollBar.Position + (FDraggingPoint.Y - Y);
    DoEventScroll;
  end;

  P.X:= X+FImage.Left;
  P.Y:= Y+FImage.Top;
  Self.MouseMove(Shift, P.X, P.Y);
end;

procedure TATImageBox.IncreaseImageZoom(AIncrement: boolean);
var
  i: integer;
begin
  if AIncrement then
  begin
    for i:= Low(cImageboxZooms) to High(cImageboxZooms) do
      if cImageboxZooms[i] > ImageZoom then
      begin
        ImageZoom:= cImageboxZooms[i];
        Break
      end;
  end
  else
  begin
    for i:= High(cImageboxZooms) downto Low(cImageboxZooms) do
      if cImageboxZooms[i] < ImageZoom then
      begin
        ImageZoom:= cImageboxZooms[i];
        Break
      end;
  end;
end;

procedure TATImageBox.DoEventScroll;
begin
  if Assigned(FOnScroll) then
    FOnScroll(Self);
end;

procedure TATImageBox.DoEventOptionsChange;
begin
  if Assigned(FOnOptionsChange) then
    FOnOptionsChange(Self);
end;

procedure TATImageBox.DoEventImageResize;
begin
  if Assigned(FOnImageResize) then
    FOnImageResize(Self);
end;

function TATImageBox.GetImageHeight: integer;
begin
  if Assigned(FImage.Picture) then
    Result:= FImage.Picture.Height
  else
    Result:= 0;
end;

function TATImageBox.GetImageWidth: integer;
begin
  if Assigned(FImage.Picture) then
    Result:= FImage.Picture.Width
  else
    Result:= 0;
end;


procedure TATImageBox.SetImageFitWidth(AValue: boolean);
begin
  if AValue <> FImageFitWidth then
  begin
    FImageFitWidth:= AValue;
    if AValue then
      FImageFitHeight:= False;
    UpdateImagePosition(True);
  end;
end;

procedure TATImageBox.SetImageFitHeight(AValue: boolean);
begin
  if AValue <> FImageFitHeight then
  begin
    FImageFitHeight:= AValue;
    if AValue then
      FImageFitWidth:= False;
    UpdateImagePosition(True);
  end;
end;

procedure TATImageBox.Clear;
begin
  FImage.Picture:= nil;
  UpdateInfo;
end;

procedure TATImageBox.LoadBitmap(ABitmap: TBitmap; ATransp: boolean);
begin
  Clear;
  FImage.Picture.Assign(ABitmap);
  FImage.Transparent:= ATransp;
  UpdateInfo;
end;

procedure TATImageBox.LoadPicture(APicture: TPicture);
begin
  Clear;
  FImage.Picture.Assign(APicture);
  UpdateInfo;
end;

procedure TATImageBox.LoadFromFile(const AFileName: string);
{$ifdef USE_BGRA}
var
  bg: TBGRABitmap;
  ext: string;
{$endif}
begin
  Clear;

  {$ifdef USE_BGRA}
  ext:= ExtractFileExt(AFileName);
  if (ext='.webp') or (ext='.psd') or (ext='.tga') or (ext='.tif') or (ext='.tiff')
    or (ext='.cur') or (ext='.svg') or (ext='.pcx') then
  begin
    bg:= TBGRABitmap.Create(AFileName);
    try
      FImage.Picture.Bitmap.Assign(bg);
    finally
      FreeAndNil(bg);
    end;
  end
  else
  {$endif}
    FImage.Picture.LoadFromFile(AFileName);

  UpdateInfo;
end;

function TATImageBox.GetPicture: TPicture;
begin
  Result:= FImage.Picture;
end;

procedure TATImageBox.ImagePaintBackground(ASender: TObject; ACanvas: TCanvas; ARect: TRect);
begin
  DoPaintCheckers(
    ACanvas,
    ARect.Right-ARect.Left,
    ARect.Bottom-ARect.Top,
    FCheckersSize,
    FCheckersColor1,
    FCheckersColor2
    );
end;

end.
