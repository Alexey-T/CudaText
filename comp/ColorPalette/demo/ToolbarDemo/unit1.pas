unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  ExtCtrls, StdCtrls, ColorPalette;

type

  { TMainForm }

  TMainForm = class(TForm)
    ColorPalette: TColorPalette;
    CoolBar: TCoolBar;
    ImageList: TImageList;
    LblStartColor: TLabel;
    LblMouseColor: TLabel;
    LblEndColor: TLabel;
    IconInfoLabel: TLabel;
    MainPanel: TPanel;
    Shape1: TShape;
    Shape2: TShape;
    Shape3: TShape;
    ToolBar: TToolBar;
    TbChangeOrientation: TToolButton;
    procedure ColorPaletteColorMouseMove(Sender: TObject; AColor: TColor;
      Shift: TShiftState);
    procedure ColorPaletteColorPick(Sender: TObject; AColor: TColor;
      Shift: TShiftState);
    procedure ColorPaletteMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure FormCreate(Sender: TObject);
    procedure MainPanelPaint(Sender: TObject);
    procedure TbChangeOrientationClick(Sender: TObject);
  private
    { private declarations }
    FStartColor: TColor;
    FEndColor: TColor;
  public
    { public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

{ OnColorMouseMove is called when the mouse enters a different color button,
  or when the ColorPalette is left. }
procedure TMainForm.ColorPaletteColorMouseMove(Sender: TObject; AColor: TColor;
  Shift: TShiftState);
var
  clrName: String;
begin
  (*
  if ColorPalette.MouseColor = clNone then
    Shape2.Brush.Style := bsClear
  else begin
    Shape2.Brush.Style := bsSolid;
    Shape2.Brush.Color := ColorPalette.MouseColor;
  end;
  if ColorPalette.MouseIndex = -1 then
    clrName := 'clNone'
  else
    clrName := ColorPalette.ColorNames[ColorPalette.MouseIndex];
  LblMouseColor.Caption := 'Mouse color:'#13 + clrName;
  *)
end;

{ OnColorPick is called whenever a color button is clicked.
  A left-click defines the start color of the gradient of the main panel,
  a right-click defines its end color. }
procedure TMainForm.ColorPaletteColorPick(Sender: TObject; AColor: TColor;
  Shift: TShiftState);
begin
  // Select gradient start color with left mouse button
  if (Shift * [ssLeft] <> []) then
  begin
    FStartColor := AColor;
    if FStartColor = clNone then
      Shape1.Brush.Style := bsClear
    else begin
      Shape1.Brush.Style := bsSolid;
      Shape1.Brush.Color := FStartColor;
    end;
    LblStartColor.Caption := 'Gradient start color:'#13 +
      ColorPalette.ColorNames[ColorPalette.PickedIndex] +
      #13'(Left click)';
  end;

  // Select gradient end color with right mouse button
  if (Shift * [ssRight] <> []) then
  begin
    FEndColor := AColor;
    if FEndColor = clNone then
      Shape3.Brush.Style := bsClear
    else begin
      Shape3.Brush.Style := bsSolid;
      Shape3.Brush.Color := FEndColor;
    end;
    LblEndColor.Caption := 'Gradient end color:'#13 +
      ColorPalette.ColorNames[ColorPalette.PickedIndex] +
      #13'(Right click)';

    IconInfoLabel.Font.Color := InvertColor(AColor);
  end;

  AColor := RGBToColor(
    (Red(FStartColor) + Red(FEndColor)) div 2,
    (Green(FStartColor) + Green(FEndColor)) div 2,
    (Blue(FStartColor) + Blue(FEndColor)) div 2
  );
  if Red(AColor) + Green(AColor) + Blue(AColor) < 3*128 then
    AColor := clWhite else
    AColor := clBlack;

  LblStartColor.Font.Color := AColor;
  LblEndColor.Font.Color := AColor;
  LblMouseColor.Font.Color := AColor;

  MainPanel.Invalidate;
end;

procedure TMainForm.ColorPaletteMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
var
  clrName: String;
begin
  if ColorPalette.MouseColor = clNone then
  begin
    Shape2.Brush.Style := bsClear;
    Shape2.Pen.Style := psDot;
  end else
  begin
    Shape2.Brush.Style := bsSolid;
    Shape2.Brush.Color := ColorPalette.MouseColor;
    Shape2.Pen.Style := psSolid;
  end;
  if ColorPalette.MouseIndex = -1 then
    clrName := 'clNone'
  else
    clrName := ColorPalette.ColorNames[ColorPalette.MouseIndex];
  LblMouseColor.Caption := 'Mouse color:'#13 + clrName;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  Toolbar.BorderSpacing.Left := 0;
  Toolbar.AutoSize := true;
  Coolbar.AutoSize := true;

  // Paint the three color boxes
  ColorPaletteColorPick(self, ColorPalette.Colors[0], [ssLeft]);
  ColorPaletteColorPick(self, ColorPalette.Colors[ColorPalette.ColorCount-1], [ssRight]);
  ColorPaletteMouseMove(self, [], 0, 0);

  // For Laz 1.4.2 where TPanel.OnPaint is not published:
  MainPanel.OnPaint := @MainPanelPaint;
end;

{ Paints a color gradient onto the main panel of the form. The gradient is
  defined by the FStartColor and FEndColor obtained by clicks into the
  ColorPalette. }
procedure TMainForm.MainPanelPaint(Sender: TObject);
begin
  MainPanel.Canvas.GradientFill(MainPanel.ClientRect,
    FStartColor,
    FEndColor,
    gdVertical
  );
end;

{ Fires when the "Flip" button is clicked }
procedure TMainForm.TbChangeOrientationClick(Sender: TObject);
var
  i: Integer;
begin
  // Vertical orientation
  CoolBar.AutoSize := false;
  if TbChangeOrientation.Down then
  begin
    CoolBar.Vertical := true;
    CoolBar.Align := alLeft;
    ColorPalette.Flipped := not ColorPalette.Flipped;
    TbChangeOrientation.ImageIndex := 1;
  end
  else
  // Horizontal orientation
  begin
    CoolBar.Vertical := false;
    CoolBar.Align := alTop;
    ColorPalette.Flipped := not ColorPalette.Flipped;
    TbChangeOrientation.ImageIndex := 0;
  end;
  CoolBar.AutoSize := true;
end;


end.

