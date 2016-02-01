{
ATStatusBar component for Delphi/Lazarus
Copyright (c) Alexey Torgashin (UVViewSoft)
License: MPL 2.0
}

unit ATStatusBar;

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
  Controls, ExtCtrls;

type
  TATStatusAlign = (saLeft, saRight, saMiddle);

type
  TATStatusData = class
  public
    ItemWidth: Integer;
    ItemAlign: TATStatusAlign;
    ItemCaption: string;
  end;

type
  TATStatusClickEvent = procedure (Sender: TObject; AIndex: Integer) of object;
  TATStatusDrawEvent = procedure (Sender: TObject; AIndex: Integer;
    ACanvas: TCanvas; const ARect: TRect; var ACanDraw: boolean) of object;

type

  { TATStatus }

  TATStatus = class(TPanel)
  private
    FColorBorderTop: TColor;
    FColorBorderR: TColor;
    FColorBorderL: TColor;
    FColorBorderU: TColor;
    FColorBorderD: TColor;
    FIndentLeft: Integer;

    FList: TList;
    FBitmap: TBitmap;
    FBitmapText: TBitmap;

    FItemIndex: Integer;
    FOnPanelClick: TATStatusClickEvent;
    FOnPanelDrawBefore: TATStatusDrawEvent;
    FOnPanelDrawAfter: TATStatusDrawEvent;

    procedure DoPaintTo(C: TCanvas);
    procedure DoPaintPanelTo(C: TCanvas; ARect: TRect; AAlign: TATStatusAlign; const ACaption: string);
    function IsIndexOk(AIndex: Integer): boolean;
    function DoDrawBefore(AIndex: Integer; ACanvas: TCanvas; const ARect: TRect): boolean;
    function DoDrawAfter(AIndex: Integer; ACanvas: TCanvas; const ARect: TRect): boolean;
    function GetCaption(N: integer): string;
    procedure SetCaption(N: integer; const S: string);
  public
    constructor Create(AOnwer: TComponent); override;
    destructor Destroy; override;
    function GetPanelRect(AIndex: Integer): TRect;
    function GetPanelAt(X, Y: Integer): Integer;
    function GetPanelData(AIndex: Integer): TATStatusData;
    function PanelCount: Integer;
    procedure AddPanel(AWidth: Integer; AAlign: TATStatusAlign; const ACaption: string = '');
    procedure DeletePanel(AIndex: Integer);
    procedure DeletePanels;
    property Captions[Index: integer]: string read GetCaption write SetCaption; default;
  protected
    procedure Paint; override;
    procedure Resize; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Click; override;
    {$ifdef windows}
    procedure WMEraseBkgnd(var Message: TMessage); message WM_ERASEBKGND;
    {$endif}
  published
    property ColorBorderTop: TColor read FColorBorderTop write FColorBorderTop;
    property ColorBorderR: TColor read FColorBorderR write FColorBorderR;
    property ColorBorderL: TColor read FColorBorderL write FColorBorderL;
    property ColorBorderU: TColor read FColorBorderU write FColorBorderU;
    property ColorBorderD: TColor read FColorBorderD write FColorBorderD;
    property IndentLeft: Integer read FIndentLeft write FIndentLeft;
    property OnPanelClick: TATStatusClickEvent read FOnPanelClick write FOnPanelClick;
    property OnPanelDrawBefore: TATStatusDrawEvent read FOnPanelDrawBefore write FOnPanelDrawBefore;
    property OnPanelDrawAfter: TATStatusDrawEvent read FOnPanelDrawAfter write FOnPanelDrawAfter;
  end;

implementation

uses
  SysUtils, Forms, Math;

{ TATStatus }

function TATStatus.IsIndexOk(AIndex: Integer): boolean;
begin
  Result:= (AIndex>=0) and (AIndex<FList.Count);
end;

function TATStatus.PanelCount: Integer;
begin
  Result:= FList.Count;
end;

constructor TATStatus.Create(AOnwer: TComponent);
begin
  inherited;

  Align:= alBottom;
  Caption:= '';
  BorderStyle:= bsNone;
  ControlStyle:= ControlStyle+[csOpaque];

  Width:= 400;
  Height:= 24;

  Font.Name:= 'Tahoma';
  Font.Color:= clBlack;
  Font.Size:= 8;

  FIndentLeft:= 5;

  Color:= $E0E0E0;
  FColorBorderTop:= clGray;
  FColorBorderR:= clGray;
  FColorBorderL:= clNone;
  FColorBorderU:= clNone;
  FColorBorderD:= clNone;

  FBitmap:= TBitmap.Create;
  FBitmap.PixelFormat:= pf24bit;
  FBitmap.Width:= 1600;
  FBitmap.Height:= 60;

  FBitmapText:= TBitmap.Create;
  FBitmapText.PixelFormat:= pf24bit;
  FBitmapText.Width:= 1600;
  FBitmapText.Height:= 60;

  FList:= TList.Create;
end;

destructor TATStatus.Destroy;
var
  i: Integer;
begin
  for i:= PanelCount-1 downto 0 do
  begin
    TObject(FList[i]).Free;
    FList[i]:= nil;
  end;
  FreeAndNil(FList);

  FreeAndNil(FBitmapText);
  FreeAndNil(FBitmap);
  inherited;
end;

procedure TATStatus.Paint;
begin
  if Assigned(FBitmap) then
  begin
    DoPaintTo(FBitmap.Canvas);
    Canvas.CopyRect(ClientRect, FBitmap.Canvas, ClientRect);
  end;
end;

procedure TATStatus.DoPaintPanelTo(C: TCanvas; ARect: TRect;
  AAlign: TATStatusAlign; const ACaption: string);
var
  RText: TRect;
  NSize, NOffset, NOffsetTop: Integer;
begin
  C.Brush.Color:= Color;
  C.FillRect(ARect);

  RText:= Rect(ARect.Left+FIndentLeft, ARect.Top, ARect.Right-FIndentLeft, ARect.Bottom);
  C.FillRect(RText);

  FBitmapText.Canvas.Brush.Color:= Color;
  FBitmapText.Canvas.FillRect(Rect(0, 0, FBitmapText.Width, FBitmapText.Height));
  FBitmapText.Canvas.Font.Assign(Self.Font);

  NSize:= FBitmapText.Canvas.TextWidth(ACaption);
  case AAlign of
    saLeft:
      NOffset:= 0;
    saRight:
      NOffset:= (ARect.Right-ARect.Left) - NSize - FIndentLeft*2;
    else
      NOffset:= (ARect.Right-ARect.Left) div 2 - NSize div 2 - FIndentLeft;
  end;

  NOffsetTop:= (ClientHeight - FBitmapText.Canvas.TextHeight(ACaption)) div 2;
  FBitmapText.Canvas.TextOut(NOffset, NOffsetTop, ACaption);
  C.CopyRect(
    RText,
    FBitmapText.Canvas,
    Rect(0, 0, RText.Right-RText.Left, RText.Bottom-RText.Top));

  if FColorBorderR<>clNone then
  begin
    C.Pen.Color:= FColorBorderR;
    C.MoveTo(ARect.Right, ARect.Top);
    C.LineTo(ARect.Right, ARect.Bottom);
  end;

  if FColorBorderL<>clNone then
  begin
    C.Pen.Color:= FColorBorderL;
    C.MoveTo(ARect.Left, ARect.Top);
    C.LineTo(ARect.Left, ARect.Bottom);
  end;

  if FColorBorderU<>clNone then
  begin
    C.Pen.Color:= FColorBorderU;
    C.MoveTo(ARect.Left, ARect.Top);
    C.LineTo(ARect.Right, ARect.Top);
  end;

  if FColorBorderD<>clNone then
  begin
    C.Pen.Color:= FColorBorderD;
    C.MoveTo(ARect.Left, ARect.Bottom-1);
    C.LineTo(ARect.Right, ARect.Bottom-1);
  end;  
end;

function TATStatus.GetPanelRect(AIndex: Integer): TRect;
var
  i: Integer;
begin
  Result.Left:= 0;
  Result.Right:= -1;
  Result.Top:= 1;
  Result.Bottom:= ClientHeight;

  if IsIndexOk(AIndex) then
    for i:= 0 to PanelCount-1 do
    begin
      Result.Left:= Result.Right + 1;
      Result.Right:= Result.Left + TATStatusData(FList[i]).ItemWidth - 1;
      if AIndex=i then Exit;
    end;
end;

procedure TATStatus.DoPaintTo(C: TCanvas);
var
  i: Integer;
  ARect: TRect;
begin
  C.Brush.Color:= Color;
  C.FillRect(ClientRect);

  for i:= 0 to PanelCount-1 do
  begin
    ARect:= GetPanelRect(i);
    if DoDrawBefore(i, C, ARect) then
    begin
      DoPaintPanelTo(C, ARect,
        TATStatusData(FList[i]).ItemAlign,
        TATStatusData(FList[i]).ItemCaption
        );
      DoDrawAfter(i, C, ARect);
    end;  
  end;

  C.Pen.Color:= FColorBorderTop;
  C.MoveTo(0, 0);
  C.LineTo(ClientWidth, 0);
end;


function TATStatus.GetPanelAt(X, Y: Integer): Integer;
var
  i: Integer;
  Pnt: TPoint;
begin
  Result:= -1;
  Pnt:= Point(X, Y);

  for i:= 0 to PanelCount-1 do
    if PtInRect(GetPanelRect(i), Pnt) then
    begin
      Result:= i;
      Exit;
    end;
end;

procedure TATStatus.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited;
  FItemIndex:= GetPanelAt(X, Y);
end;

procedure TATStatus.Resize;
begin
  inherited;
  if Assigned(FBitmap) then
  begin
    FBitmap.Width:= Max(FBitmap.Width, Width);
    FBitmap.Height:= Max(FBitmap.Height, Height);
  end;
end;


procedure TATStatus.AddPanel(AWidth: Integer; AAlign: TATStatusAlign; const ACaption: string = '');
var
  Data: TATStatusData;
begin
  Data:= TATStatusData.Create;
  Data.ItemWidth:= AWidth;
  Data.ItemAlign:= AAlign;
  Data.ItemCaption:= ACaption;
  FList.Add(Data);
  Invalidate;
end;

procedure TATStatus.DeletePanel(AIndex: Integer);
begin
  if IsIndexOk(AIndex) then
  begin
    TObject(FList[AIndex]).Free;
    FList.Delete(AIndex);
    Invalidate;
  end;
end;

procedure TATStatus.DeletePanels;
begin
  while PanelCount>0 do
    DeletePanel(PanelCount-1);
end;

function TATStatus.GetPanelData(AIndex: Integer): TATStatusData;
begin
  if IsIndexOk(AIndex) then
    Result:= TATStatusData(FList[AIndex])
  else
    Result:= nil;
end;

{$ifdef windows}
//needed to remove flickering on resize and mouse-over
procedure TATStatus.WMEraseBkgnd(var Message: TMessage);
begin
  Message.Result:= 1;
end;
{$endif}

procedure TATStatus.Click;
begin
  inherited;
  if Assigned(FOnPanelClick) then
    FOnPanelClick(Self, FItemIndex);
end;

function TATStatus.DoDrawBefore(AIndex: Integer; ACanvas: TCanvas; const ARect: TRect): boolean;
begin
  Result:= true;
  if Assigned(FOnPanelDrawBefore) then
    FOnPanelDrawBefore(Self, AIndex, ACanvas, ARect, Result);
end;

function TATStatus.DoDrawAfter(AIndex: Integer; ACanvas: TCanvas; const ARect: TRect): boolean;
begin
  Result:= true;
  if Assigned(FOnPanelDrawAfter) then
    FOnPanelDrawAfter(Self, AIndex, ACanvas, ARect, Result);
end;

function TATStatus.GetCaption(N: integer): string;
begin
  Result:= GetPanelData(N).ItemCaption;
end;

procedure TATStatus.SetCaption(N: integer; const S: string);
var
  D: TATStatusData;
begin
  D:= GetPanelData(N);
  if Assigned(D) then
  begin
    D.ItemCaption:= S;
    Invalidate;
  end;
end;

end.
