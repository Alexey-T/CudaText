{
 /***************************************************************************
                              ColorPalette.pas


 ***************************************************************************/

 *****************************************************************************
 *                                                                           *
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************

  Author:  Tom Gregorovic (_tom_@centrum.cz)

  Abstract:
    Color palette grid with custom palette support.
    The OnColorPick event is fired when user picks a color.
    The LoadPalette procedure loads custom palette.

    Custom palette example:
    
    $COLS 8
    # sets count of palette grid columns

    0,0,0
    # inserts color r,g,b [optional color name]
    255,255,255 Pure white

    $NONE
    # inserts empty palette grid cell

    $BLENDWB 128,128,128 3
    # creates color gradient white -> color -> black with specified steps

}
unit ColorPalette;

{$ifdef fpc}
  {$mode objfpc}{$H+}
{$endif}

interface

uses
  Classes, SysUtils, LResources, Controls, Forms, Graphics, Math,
  LCLType;
  
type
  TPickMode = (
    pmDefault,    // Select color at mouse-down, ColorPick event at mouse-up if at same pos
    pmImmediate,  // Select color and ColorPick event at mouse-down
    pmContinuous  // Select color at mouse-down and mouse-move, ColorPick event at mouse-up
  );

  TPaletteKind = (pkStandardPalette, pkExtendedPalette, pkSystemPalette,
    pkStandardAndSystemPalette, pkExtendedAndSystemPalette,
    pkGradientPalette, pkHTMLPalette, pkWebSafePalette);

  TPaletteSelectionKind = (pskNone, pskThin, pskThinInverted, pskThick, pskThickInverted);

  TPaletteItem = (
    piColors, piColumnCount, piFlipped,
    piButtonBorder, piButtonSize, piButtonDistance,
    piSelKind, piSelColor
  );
  TPaletteItems = set of TPaletteItem;

const
  piAll = [piColors, piColumnCount, piFlipped,
    piButtonBorder, piButtonSize, piButtonDistance,
    piSelKind, piSelColor
  ];


type

  { TCustomColorPalette }

  TColorMouseEvent = procedure (Sender: TObject; AColor: TColor; Shift: TShiftState) of object;
  TColorPaletteHintEvent = procedure (Sender: TObject; AColor: TColor; var AText: String) of object;

  TCustomColorPalette = class(TGraphicControl)
  private
    FSizeToLastCol: Integer;
    FButtonHeight: Integer;
    FButtonWidth: Integer;
    FButtonBorderColor: TColor;
    FButtonDistance: Integer;
    FCols: Integer;
    FOnColorMouseMove: TColorMouseEvent;
    FOnColorPick: TColorMouseEvent;
    FOnGetHintText: TColorPaletteHintEvent;
    FRows: Integer;
    FColors: TStringList;
    FPickedIndex: Integer;
    FPickMode: TPickMode;
    FMousePt: TPoint;
    FMouseIndex: Integer;
    FPrevMouseIndex: Integer;
    FStoredShift: TShiftState;
    FShowColorHint: Boolean;
    FSelectionColor: TColor;
    FSelectionKind: TPaletteSelectionKind;
    FSavedHint: String;
    FPaletteKind: TPaletteKind;
    FGradientSteps: Byte;
    FUseSpacers: Boolean;
    FMargin: Integer;
    FFlipped: Boolean;
    function GetColorCount: Integer;
    function GetColors(AIndex: Integer): TColor;
    function GetColorNames(AIndex: Integer): String;
    function GetMouseColor: TColor;
    function GetPickedColor: TColor;
    procedure SetButtonBorderColor(const AValue: TColor);
    procedure SetButtonDistance(const AValue: Integer);
    procedure SetButtonHeight(const AValue: Integer);
    procedure SetButtonWidth(const AValue: Integer);
    procedure SetColorNames(AIndex: Integer; const AValue: String);
    procedure SetColors(AIndex: Integer; const AValue: TColor);
    procedure SetCols(AValue: Integer);
    procedure SetFlipped(AValue: Boolean);
    procedure SetGradientSteps(AValue: Byte);
    procedure SetPaletteKind(AValue: TPaletteKind);
    procedure SetPickedIndex(AValue: Integer);
    procedure SetSelectionColor(AValue: TColor);
    procedure SetSelectionKind(AValue: TPaletteSelectionKind);
    procedure SetUseSpacers(AValue: Boolean);

  protected
    procedure BlendWBColor(AColor: TColor; Steps: Integer);
    procedure ColorPick(AIndex: Integer; Shift: TShiftState); virtual;
    procedure ColorMouseMove(AColor: TColor; Shift: TShiftState); virtual;
    procedure DoAddColor(AColor: TColor; AColorName: String = ''); virtual;
    procedure DoColorPick(AColor: TColor; AShift: TShiftState); virtual;
    procedure DoDeleteColor(AIndex: Integer); virtual;
    procedure DoInsertColor(AIndex: Integer; AColor: TColor; AColorName: String = ''); virtual;
    function GetCellHeight: Integer; inline;
    function GetCellWidth: Integer; inline;
    function GetColorIndex(X,Y: Integer): Integer;
    function GetHintText(AIndex: Integer): String; virtual;
    procedure Loaded; override;
    procedure MouseDown(Button: TMouseButton; Shift:TShiftState; X, Y:Integer); override;
    procedure MouseEnter; override;
    procedure MouseLeave; override;
    procedure MouseMove(Shift:TShiftState; X, Y:Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift:TShiftState; X, Y:Integer); override;
    procedure UpdateSize; virtual;

    property ButtonBorderColor: TColor read FButtonBorderColor write SetButtonBorderColor default clBlack;
    property ButtonDistance: Integer read FButtonDistance write SetButtonDistance default 0;
    property ButtonWidth: Integer read FButtonWidth write SetButtonWidth;
    property ButtonHeight: Integer read FButtonHeight write SetButtonHeight;
    property ColumnCount: Integer read FCols write SetCols;
    property Flipped: Boolean read FFlipped write SetFlipped default false;
    property GradientSteps: Byte read FGradientSteps write SetGradientSteps default 3;
    property PaletteKind: TPaletteKind read FPaletteKind write SetPaletteKind default pkStandardPalette;
    property PickedIndex: Integer read FPickedIndex write SetPickedIndex default -1;
    property PickMode: TPickMode read FPickMode write FPickMode default pmImmediate;
    property SelectionColor: TColor read FSelectionColor write SetSelectionColor default clBlack;
    property SelectionKind: TPaletteSelectionKind read FSelectionKind write SetSelectionKind default pskNone;
    property ShowColorHint: Boolean read FShowColorHint write FShowColorHint default true;
    property UseSpacers: Boolean read FUseSpacers write SetUseSpacers default true;

    property OnColorPick: TColorMouseEvent read FOnColorPick write FOnColorPick;
    property OnColorMouseMove: TColorMouseEvent read FOnColorMouseMove write FOnColorMouseMove; deprecated 'Use OnMouseMove() and MouseColor';
    property OnGetHintText: TColorPaletteHintEvent read FOnGetHintText write FOnGetHintText;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;

    procedure AddColor(AColor: TColor; AColorName: String = '');
    procedure ClearColors;
    procedure DeleteColor(AIndex: Integer);
    procedure InsertColor(AIndex: Integer; AColor: TColor; AColorName: String = '');
    procedure LoadPalette(const FileName: String;
      AItems: TPaletteItems = [piColors, piColumnCount]);
    procedure SavePalette(const FileName: String);
  
    property Colors[Index: Integer]: TColor read GetColors write SetColors;
    property ColorNames[Index: Integer]: String read GetColorNames write SetColorNames;
    property ColorCount: Integer read GetColorCount;
    property MouseIndex: Integer read FMouseIndex;
    property MouseColor: TColor read GetMouseColor;
    property PickedColor: TColor read GetPickedColor;

    property Height stored False;
    property Width stored False;
  end;
  
  TColorPalette = class(TCustomColorPalette)
  published
    // inherited from TCustomColorPalette
    property ButtonBorderColor;
    property ButtonDistance;
    property ButtonHeight;
    property ButtonWidth;
    property ColumnCount;
    property Flipped;
    property GradientSteps;
    property PaletteKind;
    property PickedIndex;
    property PickMode;
    property SelectionColor;
    property SelectionKind;
    property ShowColorHint;
    property UseSpacers;

    property OnColorMouseMove;
    property OnColorPick;
    property OnGetHintText;

    // inherited from TCustomColorPalette's ancestors
    property Align;
    property Anchors;
    property BorderSpacing;
    property Color default clNone;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Hint;
    property ParentColor;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Visible;
    
    property OnChangeBounds;
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnResize;
  end;
  
  procedure Register;


implementation

uses
  LCLIntf, StrUtils;

const
  SELKIND_NAMES: Array[TPaletteSelectionKind] of String = (
    'NONE', 'THIN', 'THIN-INV', 'THICK', 'THICK-INV'
  );

procedure Register;
begin
  RegisterComponents('Misc', [TColorPalette]);
end;


{ TCustomColorPalette }

constructor TCustomColorPalette.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle + [csFixedWidth, csFixedHeight];
  Color := clNone;

  FColors := TStringList.Create;
  FButtonBorderColor := clBlack;
  FButtonDistance := 0;
  FMargin := 1;
  FButtonHeight := 12;
  FButtonWidth := 12;
  FPrevMouseIndex := -1;
  FMouseIndex := -1;
  FPickMode := pmImmediate;
  FShowColorHint := true;
  FGradientSteps := 3;
  FUseSpacers := true;
  FCols := 8;
  SetPaletteKind(pkStandardPalette);

  UpdateSize;
end;

destructor TCustomColorPalette.Destroy;
begin
  FColors.Free;
  inherited;
end;

procedure TCustomColorPalette.AddColor(AColor: TColor; AColorName: String = '');
begin
  DoAddColor(AColor, AColorName);
  UpdateSize;
  Invalidate;
end;

procedure TCustomColorPalette.BlendWBColor(AColor: TColor; Steps: Integer);
var
  I: Integer;
  R, G, B, NR, NG, NB: Byte;
begin
  RedGreenBlue(AColor, R, G, B);

  for I := 1 to Steps do
  begin
    NR := Round((R * I + 255 * (Steps + 1 - I)) / (Steps + 1));
    NG := Round((G * I + 255 * (Steps + 1 - I)) / (Steps + 1));
    NB := Round((B * I + 255 * (Steps + 1 - I)) / (Steps + 1));
    DoAddColor(RGBToColor(NR, NG, NB));
  end;

  DoAddColor(AColor);

  for I := Steps downto 1 do
  begin
    NR := Round(R * I / (Steps + 1));
    NG := Round(G * I / (Steps + 1));
    NB := Round(B * I / (Steps + 1));
    DoAddColor(RGBToColor(NR, NG, NB));
  end;
end;

procedure TCustomColorPalette.ClearColors;
begin
  FColors.Clear;
end;

procedure TCustomColorPalette.ColorPick(AIndex: Integer; Shift: TShiftState);
var
  c: TColor;
begin
  FPickedIndex := AIndex;
  c := GetColors(AIndex);
  DoColorPick(c, Shift);
  Invalidate;
end;

procedure TCustomColorPalette.ColorMouseMove(AColor: TColor; Shift: TShiftState);
begin
  if Assigned(FOnColorMouseMove) then
    FOnColorMouseMove(Self, AColor, Shift);
end;

procedure TCustomColorPalette.DeleteColor(AIndex: Integer);
begin
  DoDeleteColor(AIndex);
  UpdateSize;
  Invalidate;
end;

procedure TCustomColorPalette.DoAddColor(AColor: TColor; AColorName: String = '');
begin
  FColors.AddObject(AColorName, TObject(PtrInt(AColor)));
end;

procedure TCustomColorPalette.DoColorPick(AColor: TColor; AShift: TShiftState);
begin
  if Assigned(FOnColorPick) then
    FOnColorPick(Self, AColor, AShift);
end;

procedure TCustomColorPalette.DoDeleteColor(AIndex: Integer);
begin
  if (AIndex < 0) or (AIndex >= FColors.Count) then
    exit;
  FColors.Delete(AIndex);
end;

procedure TCustomColorPalette.DoInsertColor(AIndex: Integer; AColor: TColor;
  AColorName: String = '');
begin
  FColors.InsertObject(AIndex, AColorName, TObject(PtrInt(AColor)));
end;

function TCustomColorPalette.GetColorCount: Integer;
begin
  Result := FColors.Count;
end;

function TCustomColorPalette.GetColorNames(AIndex: Integer): String;
begin
  if (AIndex >= 0) and (AIndex < FColors.Count) then
  begin
    Result := FColors.Strings[AIndex];
    if Result = '' then
    begin
      Result := ColorToString(GetColors(AIndex));
      if FUseSpacers and (Result = ColorToString(clNone)) then
        Result := '';
    end;
  end else
    Result := '';
end;

function TCustomColorPalette.GetMouseColor: TColor;
begin
  Result := GetColors(FMouseIndex);
end;

function TCustomColorPalette.GetColors(AIndex: Integer): TColor;
begin
  if (AIndex >= 0) and (AIndex < FColors.Count) then
    Result := TColor(PtrUInt(FColors.Objects[AIndex]))
  else
    Result := clNone;
end;

// Distance between top edge of a cell to the top edge of the next one
function TCustomColorPalette.GetCellHeight: Integer;
begin
  Result := FButtonHeight + FButtonDistance;
end;

// Distance between left edge of a cell to the left edge of the next one
function TCustomColorPalette.GetCellWidth: Integer;
begin
  Result := FButtonWidth + FButtonDistance;
end;

function TCustomColorPalette.GetColorIndex(X,Y: Integer): Integer;
var
  W, H: Integer;
  ix, iy: Integer;
begin
  Result := -1;
  if FFlipped then
  begin
    if (Y < 0) or (Y >= FSizeToLastCol-1) then exit;
  end else
    if (X < 0) or (X >= FSizeToLastCol-1) then exit;

  W := GetCellWidth;
  H := GetCellHeight;
  dec(X, FMargin);
  dec(Y, FMargin);
  if (FButtonDistance = 0) and (FButtonBorderColor <> clNone) then
  begin
    dec(W);
    dec(H);
    if FFlipped then
      Result := Y div H + X div W * FCols else
      Result := X div W + Y div H * FCols;
  end else
  begin
    if FFlipped then
    begin
      Result := Y div H + X div W * FCols;
      // Do not consider the space between the buttons
      if (Y mod H > FButtonWidth) or (X mod W > FButtonWidth) then
        Result := -1;
    end else
    begin
      Result := X div W + Y div H * FCols;
      // Do not consider the space between the buttons
      if (X mod W > FButtonWidth) or (Y mod H > FButtonWidth) then
        Result := -1
    end;
  end;
  if (Result >= FColors.Count) or (Result < 0) then
    Result := -1;
end;

function TCustomColorPalette.GetHintText(AIndex: Integer): string;
const
  INDENT = '* ';
  MASK = '%0:s' + LineEnding + '%4:sRed: %1:d' + LineEnding + '%4:sGreen: %2:d' + LineEnding + '%4:sBlue: %3:d';
var
  C: TColor;
begin
  C := GetColors(AIndex);
  if C = clNone then
  begin
    if FUseSpacers then
      Result := ''
    else
      Result := 'None'
  end else
  begin
    Result := GetColorNames(AIndex);
    if (Result <> '') and (Result[1] = 'c') and (Result[2] = 'l') then
      Delete(Result, 1, 2);
    Result := Format(MASK, [
      Result, Red(C), Green(C), Blue(C), INDENT]
    );
  end;
  if Assigned(FOnGetHintText) then
    FOnGetHintText(Self, C, Result);
end;

function TCustomColorPalette.GetPickedColor: TColor;
begin
  Result := GetColors(FPickedIndex);
end;

procedure TCustomColorPalette.InsertColor(AIndex: Integer; AColor: TColor;
  AColorName: String = '');
begin
  DoInsertColor(AIndex, AColor, AColorName);
end;

procedure TCustomColorPalette.Loaded;
begin
  inherited;
  UpdateSize;
end;

procedure TCustomColorPalette.LoadPalette(const FileName: String;
  AItems: TPaletteItems = [piColors, piColumnCount]);
var
  F: TextFile;
  Line: String;
  ucline: String;
  C: TColor;
  clrName: String;
  p, steps: Integer;
  sk: TPaletteSelectionKind;

  procedure ParseColor(S: String; out AColor: TColor; out Steps: Integer;
    out ColorName: String);
  var
    I, counter: Integer;
    L: TStringList;
    tmp: String;
    P: PChar;
    R,G,B: Integer;
  begin
    R := 0;
    G := 0;
    B := 0;
    Steps := 0;
    ColorName := '';
    tmp := '';
    P := PChar(S);
    counter := 0;
    // Skip leading spaces
    while (P^ = ' ') do inc(P);
    while P^ <> #0 do begin
      case P^ of
        ' ': begin
               if counter = 2 then begin
                 B := StrToIntDef(tmp, B);
                 inc(counter);
                 tmp := '';
                 while P^ = ' ' do inc(P);
               end else
               if counter > 2 then
               begin
                 tmp := tmp + ' ';
                 inc(P);
               end;
             end;
        ',': begin
               case counter of
                 0: R := StrToIntDef(tmp, R);
                 1: G := StrToIntDef(tmp, G);
               end;
               inc(counter);
               tmp := '';
               inc(P);
               while P^ = ' ' do inc(P);
             end;
        else tmp := tmp + P^;
             inc(P);
      end;
    end;
    if tmp <> '' then
      case counter of
        0: R := StrToIntDef(tmp, R);
        1: G := StrToIntDef(tmp, B);
        2: B := StrToIntDef(tmp, B);
      else
        if not TryStrToInt(tmp, Steps) then ColorName := tmp;
      end;
    AColor := RGBToColor(Max(0, Min(R, 255)), Max(0, Min(G, 255)), Max(0, Min(B, 255)));
  end;

begin
  if not FileExists(FileName) then
    raise Exception.Create(Format('[TCustomColorPalette.LoadPalette] File not found: %s', [FileName]));

  AssignFile(F, FileName);
  try
    Reset(F);

    FColors.Clear;
    FCols := 1;
    while not EOF(F) do
    begin
      ReadLn(F, Line);
      Line := Trim(Line);
      if Length(Line) < 2 then Continue;
      if Line[1] = '#' then
        Continue;
      // Allow '#' as comment within line
      p := pos('#', Line);
      if p > 0 then
        Line := TrimRight(Copy(Line, 1, p-1));
      // Parse data lines
      ucLine := Uppercase(Line);
      if ucLine[1] = '$' then
      begin
        if Copy(ucLine, 2, 4) = 'NONE' then
          DoAddColor(clNone)
        else
        if (Copy(ucLine, 2, 4) = 'COLS') and (piColumnCount in AItems) then
          FCols := StrToIntDef(Copy(Line, 6, MaxInt), FCols)
        else
        if (Copy(ucLine, 2, 7) = 'BTNDIST') and (piButtonDistance in AItems) then
          FButtonDistance := StrToIntDef(Copy(Line, 9, MaxInt), FButtonDistance)
        else
        if (Copy(ucLine, 2, 8) = 'BTNWIDTH') and (piButtonSize in AItems) then
          FButtonWidth := StrToIntDef(Copy(Line, 10, MaxInt), FButtonWidth)
        else
        if (Copy(ucLine, 2, 9) = 'BTNHEIGHT') and (piButtonSize in AItems) then
          FButtonHeight := StrToIntDef(Copy(Line, 11, MaxInt), FButtonHeight)
        else
        if (Copy(ucLine, 2, 9) = 'BTNBORDER') and (piButtonBorder in AItems) then
        begin
          Delete(Line, 1, 11);
          ParseColor(Line, C, steps, clrName);
          FButtonBorderColor := C;
        end else
        if (Copy(ucLine, 2, 7) = 'FLIPPED') and (piFlipped in AItems) then
        begin
          Delete(ucLine, 1, 9);
          case ucLine of
            'TRUE' : FFlipped := true;
            'FALSE': FFlipped := false;
          end;
        end else
        if (Copy(ucLine, 2, 7) = 'SELKIND') and (piSelKind in AItems) then
        begin
          Delete(ucLine, 1, 9);
          for sk in TPaletteSelectionKind do
            if ucLine = SELKIND_NAMES[sk] then
            begin
              FSelectionKind := sk;
              break;
            end;
        end else
        if (Copy(ucLine, 2, 8) = 'SELCOLOR') and (piSelColor in AItems) then
        begin
          Delete(Line, 1, 10);
          ParseColor(Line, C, steps, clrName);
          FSelectionColor := C;
        end else
        if (Copy(ucLine, 2, 7) = 'BLENDWB') and (piColors in AItems) then
        begin
          Delete(Line, 1, 8);
          ParseColor(Line, C, steps, clrName);
          BlendWBColor(C, steps);
        end;
      end
      else
        if (Pos(',', Line) > 0) and (piColors in AItems) then
        begin
          ParseColor(Line, C, steps, clrName);
          DoAddColor(C, clrName);
        end;
    end;
  finally
    Close(F);
  end;

  UpdateSize;
  PickedIndex := -1;
end;

procedure TCustomColorPalette.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;

  FMousePt.X := X;
  FMousePt.Y := Y;

  FMouseIndex := GetColorIndex(X, Y);
  FPrevMouseIndex := FMouseIndex;

  if FMouseIndex < 0 then
    Exit;

  if (FMouseIndex < FColors.Count) then
  begin
    FStoredShift := Shift;       // store for usage by pmDefault at MouseUp
    if FPickMode <> pmDefault then
      ColorPick(FMouseIndex, Shift);
  end;
end;

procedure TCustomColorPalette.MouseEnter;
begin
  FSavedHint := Hint;
  inherited;
end;

procedure TCustomColorPalette.MouseLeave;
begin
  inherited;
  Hint := FSavedHint;
  FMouseIndex := -1;
  if Assigned(OnMouseMove) then
    OnMouseMove(self, GetKeyShiftState, FMousePt.X, FMousePt.Y);
//  ColorMouseMove(MouseColor, []);
end;

procedure TCustomColorPalette.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  C: TColor;
begin
  FMouseIndex := GetColorIndex(X, Y);
  C := GetColors(FMouseIndex);
  ColorMouseMove(C, Shift);

  if ShowHint and FShowColorHint then
  begin
    Hint := GetHintText(FMouseIndex);
    if FMouseIndex <> FPrevMouseIndex then
      Application.ActivateHint(ClientToScreen(Point(X, Y)));
  end;

  if (FMouseIndex >= 0) and (FMouseIndex < FColors.Count) and
     ([ssLeft, ssRight, ssMiddle] * Shift <> []) and
     (FMouseIndex <> FPrevMouseIndex) and
     (FUseSpacers or (C <> clNone)) and
     (FPickMode = pmContinuous)
  then
    ColorPick(FMouseIndex, Shift);

  FPrevMouseIndex := FMouseIndex;

  inherited;
end;

procedure TCustomColorPalette.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  case FPickMode of
    pmDefault:
      if (FMousePt.X = X) and (FMousePt.Y = Y) then
        ColorPick(FMouseIndex, FStoredShift);
    pmImmediate, pmContinuous:
      begin
        FMouseIndex := GetColorIndex(X, Y);
        if (FMouseIndex >= 0) and (FMouseIndex < FColors.Count) and
           (FMouseIndex <> FPrevMouseIndex) then
        begin
          ColorPick(FMouseIndex, Shift);
        end;
      end;
  end;
  FPrevMouseIndex := -1;

  inherited;
end;

procedure TCustomColorPalette.Paint;

  procedure PaintBox(x1, y1, x2, y2: Integer; c: TColor);
  begin
    if FUseSpacers and (c = clNone) then
      exit;

    // Fill interior
    Canvas.Pen.Color := FButtonBorderColor;
    Canvas.Pen.Width := 1;
    Canvas.Pen.Style := psSolid;
    if c = clNone then
    begin
      if Canvas.Pen.Color = clNone then
        Canvas.Pen.Color := FButtonBorderColor;
      Canvas.Line(x1, y1, x2, y2);
      Canvas.Line(x1, y2, x2, y1);
      Canvas.Brush.Style := bsClear;
      Canvas.Rectangle(x1, y1, x2, y2);
    end else
    begin
      Canvas.Brush.Color := c;
      if (FButtonBorderColor = clNone) then
        Canvas.FillRect(x1, y1, x2, y2) else
        Canvas.Rectangle(x1, y1, x2, y2);
    end;
  end;

var
  I, X, Y: Integer;
  Rsel: TRect;
  max: Integer;
begin
  // Paint background color
  if Color <> clNone then begin
    Canvas.Brush.Color := Color;
    Canvas.Brush.Style := bsSolid;
    Canvas.FillRect(0, 0, Width, Height);
  end;

  Canvas.Pen.Endcap := pecFlat;

  // Paint color boxes
  X := FMargin;
  Y := FMargin;
  max := FSizeToLastCol - FMargin;
//  max := IfThen(FFlipped, Height, Width) - FMargin;
  if (FButtonDistance = 0) and (FButtonBordercolor <> clNone) then
    dec(max);

  for I := 0 to pred(FColors.Count) do
  begin
    if I = FPickedIndex then    // Selected rect of box with selected color
      Rsel := Bounds(X, Y, FButtonWidth, FButtonHeight);
    PaintBox(X, Y, X + FButtonWidth, Y + FButtonHeight, GetColors(I));
    if FFlipped then
    begin
      inc(Y, GetCellHeight);
      if (FButtonDistance = 0) and (FButtonBorderColor <> clNone) then dec(Y);
      if Y >= max then
      begin
        inc(X, GetCellWidth);
        if (FButtonDistance = 0) and (FButtonBorderColor <> clNone) then dec(X);
        Y := FMargin;
      end;
    end else
    begin
      inc(X, GetCellWidth);
      if (FButtonDistance = 0) and (FButtonBorderColor <> clNone) then dec(X);
      if X >= max then
      begin
        inc(Y, GetCellHeight);
        if (FButtonDistance = 0) and (FButtonBorderColor <> clNone) then dec(Y);
        X := FMargin;
      end;
    end;
  end;

  // Paint selection
  if FSelectionKind <> pskNone then
  begin
    Canvas.Brush.Style := bsClear;
    Canvas.Pen.Style := psSolid;
    case FSelectionKind of
      pskThin, pskThinInverted :
        begin
          Canvas.Pen.Width := 1;
          if FButtonDistance > 2 then InflateRect(Rsel, 2, 2);
        end;
      pskThick, pskThickInverted:
        begin
          Canvas.Pen.Width := 3;
        end;
    end;
    case FSelectionKind of
      pskThin, pskThick:
        Canvas.Pen.Color := FSelectionColor;
      pskThinInverted, pskThickInverted:
        begin
          Canvas.Pen.Color := InvertColor(GetPickedColor);
          if (FSelectionKind = pskThinInverted) and (Canvas.Pen.Color = FButtonBorderColor) then
            Canvas.Pen.Color := FSelectionColor;
        end;
    end;
    Canvas.Rectangle(Rsel);
  end;
end;

procedure TCustomColorPalette.SavePalette(const Filename: String);
var
  i: Integer;
  L: TStringList;
  clr: TColor;
  clrName: String;
  r,g,b: Byte;
begin
  L := TStringList.Create;
  try
    L.Add('# PROPERTIES');
    L.Add(Format('$BTNBORDER %d, %d, %d', [Red(FButtonBorderColor), Green(FButtonBorderColor), Blue(FButtonBorderColor)]));
    L.ADd(Format('$BTNWIDTH %d', [FButtonWidth]));
    L.Add(Format('$BTNHEIGHT %d', [FButtonHeight]));
    L.Add(Format('$BTNDIST %d', [FButtonDistance]));
    L.Add(Format('$SELKIND %s', [SELKIND_NAMES[FSelectionKind]]));
    L.Add(Format('$SELCOLOR %d, %d, %d', [Red(FSelectionColor), Green(FSelectionColor), Blue(FSelectionColor)]));
    L.Add(Format('$FLIPPED %s', [BoolToStr(FFlipped, 'TRUE', 'FALSE')]));
    L.Add(Format('$COLS %d', [FCols]));
    L.Add('');
    L.Add('# COLORS');
    for i:=0 to FColors.Count-1 do begin
      clr := Colors[i];
      if clr = clNone then
        L.Add('$NONE')
      else begin
        RedGreenBlue(clr, r,g,b);
        clrName := ColorNames[i];
        if clrName = '' then
          L.Add(Format('%d, %d, %d',[r, g, b]))
        else
          L.Add(Format('%d, %d, %d %s', [r, g, b, clrName]));
      end;
    end;
    L.SaveToFile(FileName);
  finally
    L.Free;
  end;
end;

procedure TCustomColorPalette.SetButtonBorderColor(const AValue: TColor);
begin
  if FButtonBorderColor = AValue then exit;
  FButtonBorderColor := AValue;
  UpdateSize;
  Invalidate;
end;

procedure TCustomColorPalette.SetButtonDistance(const AValue: Integer);
begin
  if FButtonDistance = AValue then exit;
  FButtonDistance := AValue;
  UpdateSize;
  Invalidate;
end;

procedure TCustomColorPalette.SetButtonHeight(const AValue: Integer);
begin
  if FButtonHeight = AValue then Exit;
  FButtonHeight := AValue;
  if FButtonHeight < 1 then FButtonHeight := 1;
  UpdateSize;
end;

procedure TCustomColorPalette.SetButtonWidth(const AValue: Integer);
begin
  if FButtonWidth = AValue then Exit;
  FButtonWidth := AValue;
  if FButtonWidth < 1 then FButtonWidth := 1;
  UpdateSize;
end;

procedure TCustomColorPalette.SetColorNames(AIndex: Integer; const AValue: String);
begin
  FColors.Strings[AIndex] := AValue;
end;

procedure TCustomColorPalette.SetColors(AIndex: Integer; const AValue: TColor);
begin
  FColors.Objects[AIndex] := TObject(PtrInt(AValue));
  Invalidate;
end;

{ Setter for the property ColumnCount.
  WARNING: If Flipped is true then this property is reinterpreted as the number
  of ROWS! }
procedure TCustomColorPalette.SetCols(AValue: Integer);
begin
  if AValue = FCols then
    exit;
  FCols := AValue;
  UpdateSize;
  Invalidate;
end;

{ Setter for the property Flipped.
  WARNING: If Flipped is true then the property ColumnCount is reinterpreted
  as the number of ROWS! }
procedure TCustomColorPalette.SetFlipped(AValue: Boolean);
begin
  if FFlipped = AValue then exit;
  FFlipped := AValue;
  UpdateSize;
  Invalidate;
end;

procedure TCustomColorPalette.SetGradientSteps(AValue: Byte);
begin
  if FGradientSteps = AValue then
    exit;
  FGradientSteps := AValue;
  if FPaletteKind = pkGradientPalette then
  begin
    FColors.Clear;
    SetPaletteKind(FPaletteKind);
  end;
end;

procedure TCustomColorPalette.SetPaletteKind(AValue: TPaletteKind);
const
  STEPS: array[0..4] of byte = (0, 64, 128, 192, 255);

  // Number of columns for each built-in palette, for a decent layout.
  COLCOUNT: array[TPaletteKind] of Integer = (
     8,  // StandardPalette = 16 standard colors
     4,  // ExtendedPalette = 16 standard colors + 4 extra colors
     5,  // SystemPalette = 25 system colors
     8,  // StandardAndSystemPalette = 16 standard + 25 system colors = 41 colors
     5,  // ExtendedAndSystemPalette = 16 std + 4 extra + 25 system colors = 45 colors
    -1,  // Gradient palette - color count depends on PaletteStep
    10,  // HTML palette
     6   // Websafe palette
  );
var
  i, n: Integer;
  r,g,b: Integer;
begin
  if (FPaletteKind = AValue) and (FColors.Count > 0) then
    exit;

  FPaletteKind := AValue;
  FColors.Clear;

  if FPaletteKind in [pkStandardPalette, pkExtendedPalette,
    pkStandardAndSystemPalette, pkExtendedAndSystemPalette] then
  begin
    DoAddColor(clBlack);       // 16
    DoAddColor(clGray);
    DoAddColor(clMaroon);
    DoAddColor(clOlive);
    DoAddColor(clGreen);
    DoAddColor(clTeal);
    DoAddColor(clNavy);
    DoAddColor(clPurple);
    DoAddColor(clWhite);
    DoAddColor(clSilver);
    DoAddColor(clRed);
    DoAddColor(clYellow);
    DoAddColor(clLime);
    DoAddColor(clAqua);
    DoAddColor(clBlue);
    DoAddColor(clFuchsia);
  end;

  if FPaletteKind in [pkExtendedPalette, pkExtendedAndSystemPalette] then
  begin
    DoAddColor(clMoneyGreen);     // 4
    DoAddColor(clSkyBlue);
    DoAddColor(clCream);
    DoAddColor(clMedGray);
  end;

  if FPaletteKind in [pkSystemPalette, pkStandardAndSystemPalette, pkExtendedAndSystemPalette] then
  begin
    DoAddColor(clScrollBar);            // 25
    DoAddColor(clBackground);
    DoAddColor(clActiveCaption);
    DoAddColor(clInactiveCaption);
    DoAddColor(clMenu);
    DoAddColor(clWindow);
    DoAddColor(clWindowFrame);
    DoAddColor(clMenuText);
    DoAddColor(clWindowText);
    DoAddColor(clCaptionText);
    DoAddColor(clActiveBorder);
    DoAddColor(clInactiveBorder);
    DoAddColor(clAppWorkspace);
    DoAddColor(clHighlight);
    DoAddColor(clHighlightText);
    DoAddColor(clBtnFace);
    DoAddColor(clBtnShadow);
    DoAddColor(clGrayText);
    DoAddColor(clBtnText);
    DoAddColor(clInactiveCaptionText);
    DoAddColor(clBtnHighlight);
    DoAddColor(cl3DDkShadow);
    DoAddColor(cl3DLight);
    DoAddColor(clInfoText);
    DoAddColor(clInfoBk);
  end;

  if FPaletteKind = pkGradientPalette then
  begin
    if FGradientSteps < 0 then n := 0 else n := FGradientSteps;
    for i:= Low(STEPS) to High(STEPS)-1 do BlendWBColor((RGBToColor(255, STEPS[i], 0)), n);
    for i:= High(STEPS) downto Low(STEPS)+1 do BlendWBColor((RGBToColor(STEPS[i], 255, 0)), n);
    for i:= Low(STEPS) to High(STEPS)-1 do BlendWBColor((RGBToColor(0, 255, STEPS[i])), n);
    for i:= High(STEPS) downto Low(STEPS)+1 do BlendWBColor((RGBToColor(0, STEPS[i], 255)), n);
    for i:= Low(STEPS) to High(STEPS)-1 do BlendWBColor((RGBToColor(STEPS[i], 0, 255)), n);
    for i:= Low(STEPS) downto High(STEPS) do BlendWBColor((RGBToColor(0, 255, STEPS[i])), n);
    SetCols(n*2 + 1);
  end;

  if FPaletteKind = pkHTMLPalette then
  // https://en.wikipedia.org/wiki/Web_colors#X11_color_names
  begin
    // White_colors
    DoAddColor(RGBToColor(255,255,255), 'White');
    DoAddColor(RGBToColor(255,250,250), 'Snow');
    DoAddColor(RGBToColor(240,255,240), 'Honeydew');
    DoAddColor(RGBToColor(245,255,250), 'MintCream');
    DoAddColor(RGBToColor(240,255,255), 'Azure');
    DoAddColor(RGBToColor(240,248,255), 'AliceBlue');
    DoAddColor(RGBToColor(248,248,255), 'GhostWhite');
    DoAddColor(RGBToColor(245,245,245), 'WhiteSmoke');
    DoAddColor(RGBToColor(255,245,238), 'Seashell');
    DoAddColor(RGBToColor(245,245,220), 'Beige');
    DoAddColor(RGBToColor(253,245,230), 'OldLace');
    DoAddColor(RGBToColor(255,250,240), 'FloralWhite');
    DoAddColor(RGBToColor(255,255,240), 'Ivory');
    DoAddColor(RGBToColor(250,235,215), 'AntiqueWhite');
    DoAddColor(RGBToColor(250,240,230), 'Linen');
    DoAddColor(RGBToColor(255,240,245), 'LavenderBlush');
    DoAddColor(RGBToColor(255,228,225), 'MistyRose');
    // Pink_colors
    DoAddColor(RGBToColor(255,192,203), 'Pink');
    DoAddColor(RGBToColor(255,182,193), 'LightPink');
    DoAddColor(RGBToColor(255,105,180), 'HotPink');
    DoAddColor(RGBToColor(255, 20,147), 'DeepPink');
    DoAddColor(RGBToColor(219,112,147), 'PaleVioletRed');
    DoAddColor(RGBToColor(199, 21,133), 'MediumVioletRed');
    // Red_colors
    DoAddColor(RGBToColor(255,160,122), 'LightSalmon');
    DoAddColor(RGBToColor(250,128,114), 'Salmon');
    DoAddColor(RGBToColor(233,150,122), 'DarkSalmon');
    DoAddColor(RGBToColor(240,128,128), 'LightCoral');
    DoAddColor(RGBToColor(205, 92, 92), 'IndianRed');
    DoAddColor(RGBToColor(220, 20, 60), 'Crimson');
    DoAddColor(RGBToColor(178, 34, 34), 'FireBrick');
    DoAddColor(RGBToColor(139,  0,  0), 'DarkRed');
    DoAddColor(RGBToColor(255,  0,  0), 'Red');
    // Orange_colors
    DoAddColor(RGBToColor(255, 69,  0), 'OrangeRed');
    DoAddColor(RGBToColor(255, 99, 71), 'Tomato');
    DoAddColor(RGBToColor(255,127, 80), 'Coral');
    DoAddColor(RGBToColor(255,140,  0), 'DarkOrange');
    DoAddColor(RGBToColor(255,165,  0), 'Orange');
    // Yellow_colors
    DoAddColor(RGBToColor(255,255,  0), 'Yellow');
    DoAddColor(RGBToColor(255,255,224), 'LightYellow');
    DoAddColor(RGBToColor(255,250,205), 'LemonChiffon');
    DoAddColor(RGBToColor(250,250,210), 'LightGoldenrodYellow');
    DoAddColor(RGBToColor(255,239,213), 'PapayaWhip');
    DoAddColor(RGBToColor(255,228,181), 'Moccasin');
    DoAddColor(RGBToColor(255,218,185), 'PeachPuff');
    DoAddColor(RGBToColor(238,232,170), 'PaleGoldenrod');
    DoAddColor(RGBToColor(240,230,140), 'Khaki');
    DoAddColor(RGBToColor(189,183,107), 'DarkKhaki');
    DoAddColor(RGBToColor(255,215,  0), 'Gold');
    // Brown_colors
    DoAddColor(RGBToColor(255,248,220), 'Cornsilk');
    DoAddColor(RGBToColor(255,235,205), 'BlanchedAlmond');
    DoAddColor(RGBToColor(255,228,196), 'Bisque');
    DoAddColor(RGBToColor(255,222,173), 'NavajoWhite');
    DoAddColor(RGBToColor(245,222,179), 'Wheat');
    DoAddColor(RGBToColor(222,184,135), 'BurlyWood');
    DoAddColor(RGBToColor(210,180,140), 'Tan');
    DoAddColor(RGBToColor(188,143,143), 'RosyBrown');
    DoAddColor(RGBToColor(244,164, 96), 'SandyBrown');
    DoAddColor(RGBToColor(218,165, 32), 'Goldenrod');
    DoAddColor(RGBToColor(184,134, 11), 'DarkGoldenrod');
    DoAddColor(RGBToColor(205,133, 63), 'Peru');
    DoAddColor(RGBToColor(210,105, 30), 'Chocolate');
    DoAddColor(RGBToColor(139, 69, 19), 'SaddleBrown');
    DoAddColor(RGBToColor(160, 82, 45), 'Sienna');
    DoAddColor(RGBToColor(165, 42, 42), 'Brown');
    DoAddColor(RGBToColor(128,  0,  0), 'Maroon');
    // Green_colors
    DoAddColor(RGBToColor( 85,107, 47), 'DarkOliveGreen');
    DoAddColor(RGBToColor(128,128,  0), 'Olive');
    DoAddColor(RGBToColor(107,142, 35), 'OliveDrab');
    DoAddColor(RGBToColor(154,205, 50), 'YellowGreen');
    DoAddColor(RGBToColor( 50,205, 50), 'LimeGreen');
    DoAddColor(RGBToColor(  0,255,  0), 'Lime');
    DoAddColor(RGBToColor(124,252,  0), 'LawnGreen');
    DoAddColor(RGBToColor(127,255,  0), 'Chartreuse');
    DoAddColor(RGBToColor(173,255, 47), 'GreenYellow');
    DoAddColor(RGBToColor(  0,255,127), 'SpringGreen');
    DoAddColor(RGBToColor(  0,250,154), 'MediumSpringGreen');
    DoAddColor(RGBToColor(144,238,144), 'LightGreen');
    DoAddColor(RGBToColor(152,251,152), 'PaleGreen');
    DoAddColor(RGBToColor(143,188,143), 'DarkSeaGreen');
    DoAddColor(RGBToColor( 60,179,113), 'MediumSeaGreen');
    DoAddColor(RGBToColor( 46,139, 87), 'SeaGreen');
    DoAddColor(RGBToColor( 34,139, 34), 'ForestGreen');
    DoAddColor(RGBToColor(  0,128,  0), 'Green');
    DoAddColor(RGBToColor(  0,100,  0), 'DarkGreen');
    // Cyan_colors
    DoAddColor(RGBToColor(102,205,170), 'MediumAquamarine');
    DoAddColor(RGBToColor(  0,255,255), 'Aqua');
//    DoAddColor(RGBToColor(  0,255,255), 'Cyan');
    DoAddColor(RGBToColor(224,255,255), 'LightCyan');
    DoAddColor(RGBToColor(175,238,238), 'PaleTurquoise');
    DoAddColor(RGBToColor(127,255,212), 'Aquamarine');
    DoAddColor(RGBToColor( 64,224,208), 'Turquoise');
    DoAddColor(RGBToColor( 72,209,204), 'MediumTurquoise');
    DoAddColor(RGBToColor(  0,206,209), 'DarkTurquoise');
    DoAddColor(RGBToColor( 32,178,170), 'LightSeaGreen');
    DoAddColor(RGBToColor( 95,158,160), 'CadetBlue');
    DoAddColor(RGBToColor(  0,139,139), 'DarkCyan');
    DoAddColor(RGBToColor(  0,128,128), 'Teal');
    // Blue_colors
    DoAddColor(RGBToColor(176,196,222), 'LightSteelBlue');
    DoAddColor(RGBToColor(176,224,230), 'PowderBlue');
    DoAddColor(RGBToColor(173,216,230), 'LightBlue');
    DoAddColor(RGBToColor(135,206,235), 'SkyBlue');
    DoAddColor(RGBToColor(135,206,250), 'LightSkyBlue');
    DoAddColor(RGBToColor(  0,191,255), 'DeepSkyBlue');
    DoAddColor(RGBToColor( 30,144,255), 'DodgerBlue');
    DoAddColor(RGBToColor(100,149,237), 'CornflowerBlue');
    DoAddColor(RGBToColor( 70,130,180), 'SteelBlue');
    DoAddColor(RGBToColor( 65,105,225), 'RoyalBlue');
    DoAddColor(RGBToColor(  0,  0,255), 'Blue');
    DoAddColor(RGBToColor(  0,  0,205), 'MediumBlue');
    DoAddColor(RGBToColor(  0,  0,139), 'DarkBlue');
    DoAddColor(RGBToColor(  0,  0,128), 'Navy');
    DoAddColor(RGBToColor( 25, 25,112), 'MidnightBlue');
    // Purple/Violet/Magenta colors
    DoAddColor(RGBToColor(230,230,250), 'Lavender');
    DoAddColor(RGBToColor(216,191,216), 'Thistle');
    DoAddColor(RGBToColor(221,160,221), 'Plum');
    DoAddColor(RGBToColor(238,130,238), 'Violet');
    DoAddColor(RGBToColor(218,112,214), 'Orchid');
    DoAddColor(RGBToColor(255,  0,255), 'Fuchsia');
    DoAddColor(RGBToColor(255,  0,255), 'Magenta');
    DoAddColor(RGBToColor(186, 85,211), 'MediumOrchid');
    DoAddColor(RGBToColor(147,112,219), 'MediumPurple');
    DoAddColor(RGBToColor(138, 43,226), 'BlueViolet');
    DoAddColor(RGBToColor(148,  0,211), 'DarkViolet');
    DoAddColor(RGBToColor(153, 50,204), 'DarkOrchid');
    DoAddColor(RGBToColor(139,  0,139), 'DarkMagenta');
    DoAddColor(RGBToColor(128,  0,128), 'Purple');
    DoAddColor(RGBToColor( 75,  0,130), 'Indigo');
    DoAddColor(RGBToColor( 72, 61,139), 'DarkSlateBlue');
    DoAddColor(RGBToColor(102, 51,153), 'RebeccaPurple');
    DoAddColor(RGBToColor(106, 90,205), 'SlateBlue');
    DoAddColor(RGBToColor(123,104,238), 'MediumSlateBlue');
    // Gray/Black_colors
    DoAddColor(RGBToColor(220,220,220), 'Gainsboro');
    DoAddColor(RGBToColor(211,211,211), 'LightGrey');
    DoAddColor(RGBToColor(192,192,192), 'Silver');
    DoAddColor(RGBToColor(169,169,169), 'DarkGray');
    DoAddColor(RGBToColor(128,128,128), 'Gray');
    DoAddColor(RGBToColor(105,105,105), 'DimGray');
    DoAddColor(RGBToColor(119,136,153), 'LightSlateGray');
    DoAddColor(RGBToColor(112,128,144), 'SlateGray');
    DoAddColor(RGBToColor( 47, 79, 79), 'DarkSlateGray');
    DoAddColor(RGBToColor(  0,  0,  0), 'Black');
  end;

  if FPaletteKind = pkWebSafePalette then
  begin
    // https://en.wikipedia.org/wiki/Web_colors
    for g := 0 to 5 do
      for b:= 0 to 5 do
        for r:=0 to 5 do
          DoAddColor(RGBToColor(r*$33, g*$33, b*$33));
  end;

  if FPaletteKind <> pkGradientPalette then
    SetCols(COLCOUNT[FPaletteKind]);
end;

procedure TCustomColorPalette.SetPickedIndex(AValue: Integer);
begin
  if FPickedIndex = AValue then exit;
  if (AValue >= 0) and (AValue < FColors.Count) then
    FPickedIndex := AValue else
    FPickedIndex := -1;
  Invalidate;
end;

procedure TCustomColorPalette.SetSelectionColor(AValue: TColor);
begin
  if FSelectionColor = AValue then exit;
  FSelectionColor := AValue;
  Invalidate;
end;

procedure TCustomColorPalette.SetSelectionKind(AValue: TPaletteSelectionKind);
begin
  if FSelectionKind = AValue then exit;
  FSelectionKind := AValue;
  UpdateSize;
  Invalidate;
end;

procedure TCustomColorPalette.SetUseSpacers(AValue: Boolean);
begin
  if FUseSpacers = AValue then exit;
  FUseSpacers := AValue;
  Invalidate;
end;

procedure TCustomColorPalette.UpdateSize;
var
  d, dx, dy: Integer;
begin
  if (FCols = 0) or (FColors.Count = 0) then
    FRows := 0 else
    FRows := Ceil(FColors.Count / FCols);

  if FButtonDistance = 0 then
    FMargin := 1 else
    FMargin := FButtonDistance div 2 + FButtonDistance mod 2;

  dx := GetCellWidth;
  dy := GetCellHeight;
  d := FButtonDistance;
  if (FButtonDistance = 0) and (FButtonBorderColor <> clNone) then
  begin
    dec(dx);
    dec(dy);
    d := 0;
  end;

  if FFlipped then  // Rows and columns are interchanged here !!!
  begin
    FSizeToLastCol := FCols * dy - d + 2*FMargin;
    SetBounds(Left, Top, FRows * dx - d + 2*FMargin, FCols * dy - d + 2*FMargin);
  end else
  begin
    FSizeToLastCol := FCols * dx - d + 2*FMargin;
    SetBounds(Left, Top, FCols * dx - d + 2*FMargin, FRows * dy - d + 2*FMargin);
  end;
end;


initialization
{$I colorpalette.lrs}

end.

