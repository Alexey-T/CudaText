unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Spin, ExtCtrls, Menus, ColorBox, ColorPalette;

type

  { TMainForm }

  TMainForm = class(TForm)
    Bevel1: TBevel;
    Bevel2: TBevel;
    BtnDeleteColor: TButton;
    BtnLoadPaletteAndProps: TButton;
    BtnSavePalette: TButton;
    BtnLoadRndPalette: TButton;
    BtnCreateRndPalette: TButton;
    BtnAddColor: TButton;
    BtnLoadPalette: TButton;
    BtnEditColor: TButton;
    CbBuiltinPalettes: TComboBox;
    CbSelColor: TColorBox;
    CbBkColor: TColorBox;
    CbSelKind: TComboBox;
    CbShowColorHints: TCheckBox;
    CbButtonBorderColor: TColorBox;
    CbCustomHintText: TCheckBox;
    CbUseSpacers: TCheckBox;
    CbFlipped: TCheckBox;
    ColorDialog: TColorDialog;
    ColorPalette: TColorPalette;
    CbPickMode: TComboBox;
    LblBkColor: TLabel;
    LblSelColor: TLabel;
    LblSelKind: TLabel;
    MouseColorSample: TShape;
    EdButtonDistance: TSpinEdit;
    EdButtonSize: TSpinEdit;
    EdGradientSteps: TSpinEdit;
    LblButtonDistance: TLabel;
    LblButtonSize: TLabel;
    LblMouseColorInfo: TLabel;
    LblGradientSteps: TLabel;
    LblPickMode: TLabel;
    EdColCount: TSpinEdit;
    LblColCount: TLabel;
    LblColorInfo: TLabel;
    LblButtonBorderColor: TLabel;
    LblBuiltinPalettes: TLabel;
    MnuEditPickedColor: TMenuItem;
    MnuDeletePickedColor: TMenuItem;
    OpenDialog: TOpenDialog;
    PalettePopupMenu: TPopupMenu;
    LeftPanel: TPanel;
    RightPanel: TPanel;
    SaveDialog: TSaveDialog;
    ColorSample: TShape;
    ScrollBox: TScrollBox;
    procedure BtnAddColorClick(Sender: TObject);
    procedure BtnCreateRndPaletteClick(Sender: TObject);
    procedure BtnDeleteColorClick(Sender: TObject);
    procedure BtnEditColorClick(Sender: TObject);
    procedure BtnLoadPaletteClick(Sender: TObject);
    procedure BtnLoadRndPaletteClick(Sender: TObject);
    procedure BtnSavePaletteClick(Sender: TObject);
    procedure CbBuiltinPalettesSelect(Sender: TObject);
    procedure CbBkColorSelect(Sender: TObject);
    procedure CbCustomHintTextChange(Sender: TObject);
    procedure CbPickModeSelect(Sender: TObject);
    procedure CbSelColorSelect(Sender: TObject);
    procedure CbSelKindSelect(Sender: TObject);
    procedure CbShowColorHintsChange(Sender: TObject);
    procedure CbButtonBorderColorSelect(Sender: TObject);
    procedure CbUseSpacersChange(Sender: TObject);
    procedure CbFlippedChange(Sender: TObject);
    procedure ColorPaletteColorPick(Sender: TObject; AColor: TColor;
      Shift: TShiftState);
    procedure ColorPaletteDblClick(Sender: TObject);
    procedure ColorPaletteGetHintText(Sender: TObject; AColor: TColor;
      var AText: String);
    procedure ColorPaletteMouseEnter(Sender: TObject);
    procedure ColorPaletteMouseLeave(Sender: TObject);
    procedure ColorPaletteMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure EdButtonDistanceChange(Sender: TObject);
    procedure EdButtonSizeChange(Sender: TObject);
    procedure EdColCountChange(Sender: TObject);
    procedure EdGradientStepsChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure MnuDeletePickedColorClick(Sender: TObject);
    procedure MnuEditPickedColorClick(Sender: TObject);
  private
    { private declarations }
    procedure EditCurColor;
    procedure SetColorInfo(ALabel: TLabel; ATitle: string; AIndex: Integer);
    procedure UpdateCaption;
    procedure UpdatePalette;
  public
    { public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}


{ TMainForm }

procedure TMainForm.BtnAddColorClick(Sender: TObject);
begin
  if ColorDialog.Execute then
    ColorPalette.AddColor(ColorDialog.Color);
  UpdateCaption;
end;

procedure TMainForm.BtnCreateRndPaletteClick(Sender: TObject);
const
  N = 64;
var
  i: Integer;
  R,G,B: Byte;
  L: TStringList;
begin
  L := TStringList.Create;
  try
    L.Add('$COLS 16');
    for i:=1 to N do begin
      R := Random(256);
      G := Random(256);
      B := Random(256);
      L.Add(Format('%d, %d, %d', [R, G, B]));
    end;
    L.SaveToFile('random_palette.pal');
  finally
    L.Free;
  end;
  BtnLoadRndPalette.Enabled := true;
end;

procedure TMainForm.BtnDeleteColorClick(Sender: TObject);
begin
  with ColorPalette do
  begin
    DeleteColor(PickedIndex);
    if PickedIndex = ColorCount then PickedIndex := ColorCount-1;
    ColorSample.Brush.Color := PickedColor;
    if PickedColor = clNone then
      ColorSample.Brush.Style := bsClear else
      ColorSample.Brush.Style := bsSolid;
    UpdateCaption;
    SetColorInfo(LblColorInfo, 'Current', PickedIndex);
  end;
end;

procedure TMainForm.BtnLoadPaletteClick(Sender: TObject);
begin
  with OpenDialog do
    if Execute then
    begin
      if Sender = BtnLoadPaletteAndProps then
        ColorPalette.LoadPalette(FileName, piAll) else
        ColorPalette.LoadPalette(FileName);
      EdColCount.Value := ColorPalette.ColumnCount;
      CbSelKind.ItemIndex := ord(Colorpalette.SelectionKind);
      CbSelColor.Selected := ColorPalette.SelectionColor;
      CbButtonBorderColor.Selected := ColorPalette.ButtonBorderColor;
      EdButtonSize.Value := ColorPalette.ButtonWidth;
      EdButtonDistance.Value := ColorPalette.ButtonDistance;
      CbFlipped.Checked := ColorPalette.Flipped;
      UpdateCaption;
    end;
end;

procedure TMainForm.BtnLoadRndPaletteClick(Sender: TObject);
begin
  ColorPalette.LoadPalette('random_palette.pal');
  UpdateCaption;
  EdColCount.Value := ColorPalette.ColumnCount;
end;

procedure TMainForm.BtnSavePaletteClick(Sender: TObject);
begin
  if SaveDialog.Execute then
    ColorPalette.SavePalette(SaveDialog.FileName);
end;

procedure TMainForm.BtnEditColorClick(Sender: TObject);
begin
  if BtnEditColor.caption = 'Edit' then
    EditCurColor
  else
    UpdatePalette;
end;

procedure TMainForm.CbButtonBorderColorSelect(Sender: TObject);
begin
  ColorPalette.ButtonBorderColor := CbButtonBorderColor.Selected;
end;

procedure TMainForm.CbBuiltinPalettesSelect(Sender: TObject);
begin
  ColorPalette.PaletteKind := TPaletteKind(CbBuiltinPalettes.ItemIndex);
  UpdateCaption;
  EdColCount.Value := ColorPalette.ColumnCount;
  EdGradientSteps.Enabled := ColorPalette.PaletteKind = pkGradientPalette;
  LblGradientSteps.Enabled := EdGradientSteps.Enabled;
end;

procedure TMainForm.CbBkColorSelect(Sender: TObject);
begin
  ColorPalette.Color := CbBkColor.Selected;
end;

procedure TMainForm.CbCustomHintTextChange(Sender: TObject);
begin
  if CbCustomHintText.Checked then
    ColorPalette.OnGetHintText := @ColorPaletteGetHintText
  else
    ColorPalette.OnGetHintText := nil;
end;

procedure TMainForm.CbPickModeSelect(Sender: TObject);
begin
  ColorPalette.PickMode := TPickMode(CbPickMode.ItemIndex);
end;

procedure TMainForm.CbSelColorSelect(Sender: TObject);
begin
  ColorPalette.SelectionColor := CbSelColor.Selected;
end;

procedure TMainForm.CbSelKindSelect(Sender: TObject);
begin
  ColorPalette.SelectionKind := TPaletteSelectionKind(CbSelKind.ItemIndex);
end;

procedure TMainForm.CbShowColorHintsChange(Sender: TObject);
begin
  ColorPalette.ShowColorHint := CbShowColorHints.Checked;
end;

procedure TMainForm.CbUseSpacersChange(Sender: TObject);
begin
  ColorPalette.UseSpacers := CbUseSpacers.Checked;
end;

procedure TMainForm.CbFlippedChange(Sender: TObject);
begin
  ColorPalette.Flipped := CbFlipped.Checked;
end;

procedure TMainForm.ColorPaletteColorPick(Sender: TObject; AColor: TColor;
  Shift: TShiftState);
begin
  ColorSample.Brush.Color := AColor;
  if AColor = clNone then
    ColorSample.Brush.Style := bsClear else
    ColorSample.Brush.Style := bsSolid;
  SetColorInfo(LblColorInfo, 'Picked color', ColorPalette.PickedIndex);
  BtnDeleteColor.Caption := 'Delete color #' + IntToStr(ColorPalette.PickedIndex);
  UpdateCaption;
end;

procedure TMainForm.ColorPaletteDblClick(Sender: TObject);
begin
  with ColorDialog do
  begin
    Color := ColorPalette.PickedColor;
    if Execute then
    begin
      ColorPalette.Colors[ColorPalette.PickedIndex] := Color;
      ColorSample.Brush.Color := Color;
      ColorSample.Brush.Style := bsSolid;
      SetColorInfo(LblColorInfo, 'Current', ColorPalette.PickedIndex);
      with  BtnEditColor do
      begin
        Caption := 'Edit';
        Hint := 'Edit current color';
      end;
    end;
  end;
end;

procedure TMainForm.ColorPaletteGetHintText(Sender: TObject; AColor: TColor;
  var AText: String);
begin
  AText := Format('This is HTML color #%0.2x%0.2x%0.2x', [
    Red(AColor), Green(AColor), Blue(AColor)
  ]);
end;

procedure TMainForm.ColorPaletteMouseEnter(Sender: TObject);
begin
  MouseColorSample.Show;
  LblMouseColorInfo.Show;
end;

procedure TMainForm.ColorPaletteMouseLeave(Sender: TObject);
begin
  MouseColorSample.Hide;
  LblMouseColorInfo.Hide;
end;

procedure TMainForm.ColorPaletteMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  MousecolorSample.Brush.Color := Colorpalette.MouseColor;
  if MouseColorSample.Brush.Color = clNone then
    MouseColorSample.Brush.Style := bsClear else
  MouseColorSample.Brush.Style := bsSolid;
  SetColorInfo(LblMouseColorInfo, 'Mouse color', ColorPalette.MouseIndex);
end;

procedure TMainForm.EdButtonDistanceChange(Sender: TObject);
begin
  ColorPalette.ButtonDistance := EdButtonDistance.Value;
end;

procedure TMainForm.EdButtonSizeChange(Sender: TObject);
begin
  ColorPalette.ButtonWidth := EdButtonSize.Value;
  ColorPalette.ButtonHeight := EdButtonSize.Value;
end;

procedure TMainForm.EdColCountChange(Sender: TObject);
begin
  ColorPalette.ColumnCount := EdColCount.Value;
end;

procedure TMainForm.EdGradientStepsChange(Sender: TObject);
begin
  ColorPalette.GradientSteps := EdGradientSteps.Value;
  UpdateCaption;
end;

procedure TMainForm.EditCurColor;
begin
  with ColorDialog do
  begin
    Color := ColorSample.Brush.color;
    if Execute then begin
      ColorSample.Brush.Color := Color;
      ColorSample.Brush.Style := bsSolid;
    end;
  end;
  if ColorSample.Brush.Color <> ColorPalette.PickedColor then
  begin
    BtnEditColor.caption := 'Update >';
    BtnEditColor.hint := 'Update palette';
    SetColorInfo(LblColorInfo, 'New color', ColorPalette.PickedIndex);
  end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  EdColCount.Value := ColorPalette.ColumnCount;
  EdGradientSteps.Value := ColorPalette.GradientSteps;
  CbPickMode.ItemIndex := ord(ColorPalette.PickMode);
  CbSelKind.ItemIndex := ord(ColorPalette.SelectionKind);
  CbShowColorHints.Checked := ColorPalette.ShowColorHint;
  CbButtonBorderColor.Selected := ColorPalette.ButtonBorderColor;
  CbBkColor.Selected := ColorPalette.Color;
  EdButtonDistance.Value := ColorPalette.ButtonDistance;
  EdButtonSize.Value := ColorPalette.ButtonWidth;

  ColorSample.Brush.Color := ColorPalette.PickedColor;
  SetColorInfo(LblColorInfo, 'Current', ColorPalette.PickedIndex);
  UpdateCaption;

  { ColorPalette.PickShift must contain ssRight in order to be able to select
    colors for the context menu. Use object inspector, or use this code:  }
  //ColorPalette.PickShift := [ssLeft, ssRight];

  ColorPalette.OnGetHintText := nil;   // will be activated by CbCustomHintText
end;

procedure TMainForm.MnuDeletePickedColorClick(Sender: TObject);
begin
  BtnDeleteColorClick(self);
end;

procedure TMainForm.MnuEditPickedColorClick(Sender: TObject);
begin
  BtnEditColorClick(self);
end;

procedure TMainForm.SetColorInfo(ALabel: TLabel; ATitle: string; AIndex: Integer);
var
  C: TColor;
begin
  C := ColorPalette.Colors[AIndex];
  if C = clNone then
    ALabel.Caption := Format(
      '%s: None', [ATitle]
    )
  else
    ALabel.caption := Format(
      '%s: %s' + LineEnding +
      ' red = %d' + LineEnding +
      ' green = %d' + LineEnding +
      ' blue = %d', [
      ATitle, ColorPalette.ColorNames[AIndex],
      Red(C), Green(C), Blue(C)
    ]);
end;

procedure TMainForm.UpdateCaption;
begin
  Caption := Format('ColorPalette demo - CurIndex: %d (%d colors available)',
    [ColorPalette.PickedIndex, ColorPalette.ColorCount]
  );
end;

procedure TMainForm.UpdatePalette;
begin
  ColorPalette.Colors[ColorPalette.PickedIndex] := ColorSample.Brush.Color;
  SetColorInfo(LblColorInfo, 'Current', ColorPalette.PickedIndex);
  with  BtnEditColor do
  begin
    Caption := 'Edit';
    Hint := 'Edit current color';
  end;
end;

end.

