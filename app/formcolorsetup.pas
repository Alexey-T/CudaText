(*
This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at http://mozilla.org/MPL/2.0/.

Copyright (c) Alexey Torgashin
*)
unit formcolorsetup;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ButtonPanel,
  IniFiles, ColorBox, StdCtrls, ExtCtrls,
  Types, LazUTF8, LazFileUtils,
  LCLType,
  ATSynEdit_Globals,
  ec_SyntAnal,
  ec_syntax_format,
  formlexerstyle,
  proc_msg,
  proc_globdata,
  proc_colors;

type
  TApplyThemeEvent = procedure(const AColors: TAppTheme; AThemeUI: boolean) of object;

type
  { TfmColorSetup }

  TfmColorSetup = class(TForm)
    bChange: TButton;
    bNone: TButton;
    bStyle: TButton;
    ButtonPanel1: TButtonPanel;
    ColorDialog1: TColorDialog;
    List: TColorListBox;
    ListStyles: TListBox;
    PanelSyntax: TPanel;
    PanelUi: TPanel;
    procedure bChangeClick(Sender: TObject);
    procedure bNoneClick(Sender: TObject);
    procedure bStyleClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
    procedure ListKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ListSelectionChange(Sender: TObject; User: boolean);
    procedure ListStylesDrawItem(Control: TWinControl; AIndex: Integer; ARect: TRect; State: TOwnerDrawState);
    procedure OKButtonClick(Sender: TObject);
  private
    { private declarations }
    FChanged: boolean;
    FColorBg: TColor;
    procedure UpdateList;
    procedure UpdateChanged;
    procedure Localize;
  public
    { public declarations }
    ThemeUI: boolean;
    Data: TAppTheme;
    OnApply: TApplyThemeEvent;
  end;

implementation

{$R *.lfm}

{ TfmColorSetup }

procedure TfmColorSetup.Localize;
const
  section = 'd_theme';
var
  ini: TIniFile;
  fn: string;
begin
  fn:= AppFile_Language;
  if not FileExists(fn) then exit;
  ini:= TIniFile.Create(fn);
  try
    Caption:= ini.ReadString(section, '_', Caption);
    with ButtonPanel1.OKButton do Caption:= msgButtonOk;
    with ButtonPanel1.CancelButton do Caption:= msgButtonCancel;
    with ButtonPanel1.HelpButton do Caption:= msgButtonApply;

    with bChange do Caption:= ini.ReadString(section, 'ch', Caption);
    with bNone do Caption:= ini.ReadString(section, 'non', Caption);
    with bStyle do Caption:= ini.ReadString(section, 'sty', Caption);
  finally
    FreeAndNil(ini);
  end;
end;


procedure TfmColorSetup.UpdateList;
var
  st: TecSyntaxFormat;
  iColor: TAppThemeColorId;
  iStyle: TAppThemeStyleId;
  NPrevIndex: integer;
begin
  NPrevIndex:= List.ItemIndex;
  List.Items.Clear;

  for iColor:= Low(iColor) to High(iColor) do
    List.Items.AddObject(Data.Colors[iColor].desc, TObject(PtrInt(Data.Colors[iColor].color)));

  if ListStyles.Count=0 then
    for iStyle:= Low(iStyle) to apstLastStyle do
    begin
      st:= Data.Styles[iStyle];
      ListStyles.Items.AddObject(st.DisplayName, st);
    end;

  if NPrevIndex<List.Items.Count then
    List.ItemIndex:= NPrevIndex;
  List.Invalidate;
end;

procedure TfmColorSetup.UpdateChanged;
begin
  ButtonPanel1.OKButton.Enabled:= FChanged;
  ButtonPanel1.HelpButton.Enabled:= FChanged;
end;

procedure TfmColorSetup.bChangeClick(Sender: TObject);
begin
  ColorDialog1.Color:= PtrInt(List.Items.Objects[List.ItemIndex]);
  if ColorDialog1.Execute then
  begin
    Data.Colors[TAppThemeColorId(List.ItemIndex)].color:= ColorDialog1.Color;
    UpdateList;

    FChanged:= true;
    UpdateChanged;
  end;
end;

procedure TfmColorSetup.bNoneClick(Sender: TObject);
begin
  Data.Colors[TAppThemeColorId(List.ItemIndex)].color:= clNone;
  UpdateList;

  FChanged:= true;
  UpdateChanged;
end;

procedure TfmColorSetup.bStyleClick(Sender: TObject);
var
  st: TecSyntaxFormat;
  Form: TfmLexerStyle;
begin
  if ListStyles.ItemIndex<0 then exit;
  st:= TecSyntaxFormat(ListStyles.Items.Objects[ListStyles.ItemIndex]);

  Form:= TfmLexerStyle.Create(nil);
  with Form do
  try
    edColorFont.Selected:= st.Font.Color;
    edColorBG.Selected:= st.BgColor;
    edColorBorder.Selected:= st.BorderColorBottom;
    edStyleType.ItemIndex:= Ord(st.FormatType);
    chkBold.Checked:= fsBold in st.Font.Style;
    chkItalic.Checked:= fsItalic in st.Font.Style;
    chkStrik.Checked:= fsStrikeOut in st.Font.Style;
    chkUnder.Checked:= fsUnderline in st.Font.Style;
    cbBorderL.ItemIndex:= Ord(st.BorderTypeLeft);
    cbBorderR.ItemIndex:= Ord(st.BorderTypeRight);
    cbBorderT.ItemIndex:= Ord(st.BorderTypeTop);
    cbBorderB.ItemIndex:= Ord(st.BorderTypeBottom);

    if ShowModal=mrOk then
    begin
      FChanged:= true;
      UpdateChanged;

      st.Font.Color:= edColorFont.Selected;
      st.BgColor:= edColorBG.Selected;
      st.BorderColorBottom:= edColorBorder.Selected;
      st.FormatType:= TecFormatType(edStyleType.ItemIndex);

      st.Font.Style:= [];
      if chkBold.Checked then st.Font.Style:= st.Font.Style+[fsBold];
      if chkItalic.Checked then st.Font.Style:= st.Font.Style+[fsItalic];
      if chkStrik.Checked then st.Font.Style:= st.Font.Style+[fsStrikeOut];
      if chkUnder.Checked then st.Font.Style:= st.Font.Style+[fsUnderline];

      st.BorderTypeLeft:= TecBorderLineType(cbBorderL.ItemIndex);
      st.BorderTypeRight:= TecBorderLineType(cbBorderR.ItemIndex);
      st.BorderTypeTop:= TecBorderLineType(cbBorderT.ItemIndex);
      st.BorderTypeBottom:= TecBorderLineType(cbBorderB.ItemIndex);
    end;
  finally
    Free
  end;
end;

procedure TfmColorSetup.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  ////disabled yet, to quick solve issue #3626
  //FormHistorySave(Self, '/pos/colortheme', false);
end;

procedure TfmColorSetup.FormCreate(Sender: TObject);
begin
  ////disabled yet, to quick solve issue #3626
  //FormHistoryLoad(Self, '/pos/colortheme', false);
end;

procedure TfmColorSetup.FormShow(Sender: TObject);
begin
  PanelUi.Visible:= ThemeUI;
  PanelSyntax.Visible:= not ThemeUI;
  ButtonPanel1.HelpButton.Visible:= ThemeUI;

  Localize;

  Width:= ATEditorScale(Width);
  Height:= ATEditorScale(Height);
  List.Width:= ATEditorScale(List.Width);
  ListStyles.Width:= ATEditorScale(ListStyles.Width);
  UpdateFormOnTop(Self);

  PanelUi.Align:= alClient;
  PanelSyntax.Align:= alClient;

  UpdateList;
  List.ItemIndex:= 0;
  ListStyles.ItemIndex:= 0;

  FColorBg:= Data.Colors[apclEdTextBg].color;

  if PanelUi.Visible then
    ActiveControl:= List
  else
  if PanelSyntax.Visible then
    ActiveControl:= ListStyles;

  UpdateChanged;
end;

procedure TfmColorSetup.HelpButtonClick(Sender: TObject);
begin
  if Assigned(OnApply) then
    OnApply(Data, ThemeUI);
end;

procedure TfmColorSetup.ListKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (key=Ord(' ')) and (Shift=[]) then
  begin
    bChange.Click;
    key:= 0;
    exit
  end;
end;

procedure TfmColorSetup.ListSelectionChange(Sender: TObject; User: boolean);
var
  NSel: integer;
begin
  //disable "None color" button to some elements
  NSel:= List.ItemIndex;
  bNone.Enabled:= (NSel>=0) and (TAppThemeColorId(NSel) in cAppThemeColorsWhichAllowNone);
end;

procedure TfmColorSetup.ListStylesDrawItem(Control: TWinControl; AIndex: Integer; ARect: TRect;
  State: TOwnerDrawState);
const
  cIndent = 6;
  cExample = ' Example ';
var
  C: TCanvas;
  st: TecSyntaxFormat;
  NWidth: integer;
begin
  if (AIndex<0) or (AIndex>=ListStyles.Items.Count) then exit;

  C:= (Control as TListbox).Canvas;
  st:= ListStyles.Items.Objects[AIndex] as TecSyntaxFormat;

  C.Brush.Color:= clWindow;
  C.FillRect(ARect);

  C.Font.Color:= st.Font.Color;
  C.Font.Style:= st.Font.Style;
  C.Brush.Color:= st.BgColor;
  if st.BgColor=clNone then
    C.Brush.Color:= FColorBg;

  NWidth:= C.TextWidth(cExample);
  C.TextOut(ARect.Right-NWidth, ARect.Top, cExample);

  if st.BorderColorBottom<>clNone then
  begin
    C.Pen.Color:= st.BorderColorBottom;
    C.Line(ARect.Right-NWidth, ARect.Bottom-2, ARect.Right, ARect.Bottom-2);
  end;

  if odSelected in State then
  begin
    C.Brush.Color:= clHighlight;
    C.Font.Color:= clHighlightText;
    C.FillRect(ARect.Left, ARect.Top, ARect.Right-NWidth, ARect.Bottom);
  end
  else
  begin
    C.Brush.Color:= clWindow;
    C.Font.Color:= clWindowText;
  end;

  C.Font.Style:= [];
  C.TextOut(ARect.Left+cIndent, ARect.Top, ListStyles.Items[AIndex]);
end;

procedure TfmColorSetup.OKButtonClick(Sender: TObject);
begin
  if Assigned(OnApply) then
    OnApply(Data, ThemeUI);
  ModalResult:= mrOk;
end;

end.
