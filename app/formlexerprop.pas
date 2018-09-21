(*
This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at http://mozilla.org/MPL/2.0/.

Copyright (c) Alexey Torgashin
*)
unit formlexerprop;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Forms, Controls, StdCtrls,
  Dialogs, ButtonPanel, ComCtrls, ExtCtrls, ColorBox, IniFiles,
  LazUTF8, LazFileUtils,
  ec_SyntAnal,
  ATSynEdit,
  ATSynEdit_Adapter_EControl,
  proc_msg,
  proc_globdata,
  proc_lexer_styles,
  proc_editor,
  formlexerstylemap;

type
  { TfmLexerProp }

  TfmLexerProp = class(TForm)
    ButtonPanel1: TButtonPanel;
    chkBold: TCheckBox;
    chkItalic: TCheckBox;
    chkStrik: TCheckBox;
    chkUnder: TCheckBox;
    cbBorderL: TComboBox;
    cbBorderT: TComboBox;
    cbBorderR: TComboBox;
    cbBorderB: TComboBox;
    edCmtStream2: TEdit;
    edColorFont: TColorBox;
    edColorBG: TColorBox;
    edColorBorder: TColorBox;
    edCmtLine: TEdit;
    edCmtStream1: TEdit;
    edCmtFull1: TEdit;
    edCmtFull2: TEdit;
    edStylesStrings: TEdit;
    edStylesCmt: TEdit;
    edTypes: TEdit;
    edName: TEdit;
    edSample: TATSynEdit;
    LabelInfoThemes: TLabel;
    LabelStylesStrings: TLabel;
    LabelStylesCmt: TLabel;
    LabelCmtFull: TLabel;
    LabelCmtStream: TLabel;
    LabelCmtLine: TLabel;
    LabelSample: TLabel;
    LabelBorderL: TLabel;
    LabelBorderT: TLabel;
    LabelBorderR: TLabel;
    LabelBorderB: TLabel;
    LabelColorBorder: TLabel;
    LabelLexerName: TLabel;
    LabelFileTypes: TLabel;
    edNotes: TMemo;
    LabelColorFont: TLabel;
    LabelColorBg: TLabel;
    LabelFontStyles: TLabel;
    LabelBorder: TLabel;
    ListStyles: TListBox;
    Pages: TPageControl;
    Panel1: TPanel;
    TabSheetComments: TTabSheet;
    TabSheetGeneral: TTabSheet;
    TabSheetNotes: TTabSheet;
    TabSheetStyles: TTabSheet;
    procedure cbBorderLChange(Sender: TObject);
    procedure chkBoldChange(Sender: TObject);
    procedure edCmtStream1Change(Sender: TObject);
    procedure edColorBGChange(Sender: TObject);
    procedure edNameChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ListStylesClick(Sender: TObject);
  private
    { private declarations }
    FAnalyzer: TecSyntAnalyzer;
    FFormats: TecStylesCollection;
    FLockedUpdate: boolean;
    FChangedAllowed: boolean;
    FChangedLexer: boolean;
    FChangedComments: boolean;
    procedure InitBorder(cb: TCombobox);
    procedure SaveChangedComments;
    procedure SaveChangedLexer;
    procedure SetChangedComments(AValue: boolean);
    procedure SetChangedLexer(AValue: boolean);
    procedure UpdateListboxStyles;
    procedure UpdateStlEn(fmt: TecFormatType);
    procedure UpdateStlFromListbox;
    procedure UpdateStlToListbox;
  public
    { public declarations }
    Adapter: TATAdapterEControl;
    property IsChangedLexer: boolean read FChangedLexer write SetChangedLexer;
    property IsChangedComments: boolean read FChangedComments write SetChangedComments;
  end;

var
  fmLexerProp: TfmLexerProp;

function DoShowDialogLexerProp(
  an: TecSyntAnalyzer;
  const AFontName: string;
  AFontSize: integer): boolean;

implementation

{$R *.lfm}

var
  msgBorderTypeNone: string = 'none';
  msgBorderTypeSolid: string = 'solid';
  msgBorderTypeDash: string = 'dash';
  msgBorderTypeDot: string = 'dot';
  msgBorderTypeDashDot: string = 'dash dot';
  msgBorderTypeDashDotDot: string = 'dash dot dot';
  msgBorderTypeSolid2: string = 'solid2';
  msgBorderTypeSolid3: string = 'solid3';
  msgBorderTypeWave: string = 'wave';
  msgBorderTypeDouble: string = 'double';

procedure DoLocString(var AStr: string; ini: TIniFile; const ASection, AKey: string);
begin
  AStr:= ini.ReadString(ASection, AKey, AStr);
end;

procedure DoLocalize_FormLexerProp(F: TfmLexerProp);
const
  section = 'd_lex_prop';
var
  ini: TIniFile;
  fn: string;
begin
  fn:= GetAppLangFilename;
  if not FileExists(fn) then exit;
  ini:= TIniFile.Create(fn);
  try
    with F do Caption:= ini.ReadString(section, '_', Caption);
    with F.ButtonPanel1.OKButton do Caption:= msgButtonOk;
    with F.ButtonPanel1.CancelButton do Caption:= msgButtonCancel;

    with F.TabSheetGeneral do Caption:= ini.ReadString(section, 'tab_gen', Caption);
    with F.TabSheetComments do Caption:= ini.ReadString(section, 'tab_cmt', Caption);
    with F.TabSheetStyles do Caption:= ini.ReadString(section, 'tab_st', Caption);
    with F.TabSheetNotes do Caption:= ini.ReadString(section, 'tab_not', Caption);

    with F.LabelLexerName do Caption:= ini.ReadString(section, 'gen_nam', Caption);
    with F.LabelFileTypes do Caption:= ini.ReadString(section, 'gen_typ', Caption);
    with F.LabelSample do Caption:= ini.ReadString(section, 'gen_smp', Caption);

    with F.LabelCmtLine do Caption:= ini.ReadString(section, 'cmt_line', Caption);
    with F.LabelCmtStream do Caption:= ini.ReadString(section, 'cmt_str', Caption);
    with F.LabelCmtFull do Caption:= ini.ReadString(section, 'cmt_full', Caption);

    with F.LabelStylesCmt do Caption:= ini.ReadString(section, 'styles_cmt', Caption);
    with F.LabelStylesStrings do Caption:= ini.ReadString(section, 'styles_str', Caption);

    with F.LabelColorBg do Caption:= ini.ReadString(section, 'col_bg', Caption);
    with F.LabelColorFont do Caption:= ini.ReadString(section, 'col_fon', Caption);
    with F.LabelColorBorder do Caption:= ini.ReadString(section, 'col_bor', Caption);

    with F.LabelBorder do Caption:= ini.ReadString(section, 'bor', Caption);
    with F.LabelBorderL do Caption:= ini.ReadString(section, 'bor_l', Caption);
    with F.LabelBorderR do Caption:= ini.ReadString(section, 'bor_r', Caption);
    with F.LabelBorderT do Caption:= ini.ReadString(section, 'bor_t', Caption);
    with F.LabelBorderB do Caption:= ini.ReadString(section, 'bor_b', Caption);

    with F.LabelFontStyles do Caption:= ini.ReadString(section, 'fon_st', Caption);
    with F.chkBold do Caption:= ini.ReadString(section, 'fon_b', Caption);
    with F.chkItalic do Caption:= ini.ReadString(section, 'fon_i', Caption);
    with F.chkUnder do Caption:= ini.ReadString(section, 'fon_u', Caption);
    with F.chkStrik do Caption:= ini.ReadString(section, 'fon_s', Caption);

    {
    with F.LabelStyleType do Caption:= ini.ReadString(section, 'typ_', Caption);
    with F.edStyleType do Items[0]:= ini.ReadString(section, 'typ_mi', Items[0]);
    with F.edStyleType do Items[1]:= ini.ReadString(section, 'typ_col_st', Items[1]);
    with F.edStyleType do Items[2]:= ini.ReadString(section, 'typ_col', Items[2]);
    with F.edStyleType do Items[3]:= ini.ReadString(section, 'typ_col_bg', Items[3]);
    }

    with F.LabelInfoThemes do Caption:= ini.ReadString(section, 'info_theme', Caption);

    DoLocString(msgBorderTypeNone, ini, section, 'bty_none');
    DoLocString(msgBorderTypeSolid, ini, section, 'bty_solid');
    DoLocString(msgBorderTypeDash, ini, section, 'bty_dash');
    DoLocString(msgBorderTypeDot, ini, section, 'bty_dot');
    DoLocString(msgBorderTypeDashDot, ini, section, 'bty_dashdot');
    DoLocString(msgBorderTypeDashDotDot, ini, section, 'bty_dashdotdot');
    DoLocString(msgBorderTypeSolid2, ini, section, 'bty_solid2');
    DoLocString(msgBorderTypeSolid3, ini, section, 'bty_solid3');
    DoLocString(msgBorderTypeWave, ini, section, 'bty_wave');
    DoLocString(msgBorderTypeDouble, ini, section, 'bty_double');

  finally
    FreeAndNil(ini);
  end;
end;


{ TfmLexerProp }

procedure TfmLexerProp.FormCreate(Sender: TObject);
begin
  Adapter:= TATAdapterEControl.Create(Self);
  Adapter.AddEditor(edSample);

  edSample.DoubleBuffered:= UiOps.DoubleBuffered;
  edSample.OptTabSize:= 4;

  FFormats:= TecStylesCollection.Create;

  LabelInfoThemes.Visible:= UiOps.LexerThemes;
end;

procedure TfmLexerProp.SaveChangedLexer;
var
  i: integer;
begin
  for i:= 0 to FAnalyzer.Formats.Count-1 do
    FAnalyzer.Formats.Items[i].Assign(FFormats[i]);

  DoSaveLexerStylesToFile_JsonLexerOps(
    FAnalyzer,
    GetAppLexerOpsFilename(FAnalyzer.LexerName)
    );
end;

procedure TfmLexerProp.SaveChangedComments;
begin
  with TIniFile.Create(GetAppLexerMapFilename(FAnalyzer.LexerName)) do
  try
    if edCmtStream1.Text<>'' then
    begin
      WriteString('comments', 'str1', edCmtStream1.Text);
      WriteString('comments', 'str2', edCmtStream2.Text);
    end
    else
    begin
      DeleteKey('comments', 'str1');
      DeleteKey('comments', 'str2');
    end;

    if edCmtFull1.Text<>'' then
    begin
      WriteString('comments', 'full1', edCmtFull1.Text);
      WriteString('comments', 'full2', edCmtFull2.Text);
    end
    else
    begin
      DeleteKey('comments', 'full1');
      DeleteKey('comments', 'full2');
    end;

    WriteString('comments', 'styles_cmt', edStylesCmt.Text);
    WriteString('comments', 'styles_str', edStylesStrings.Text);
  finally
    Free
  end;
end;

procedure TfmLexerProp.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if ModalResult=mrOk then
  begin
    if IsChangedLexer then SaveChangedLexer;
    if IsChangedComments then SaveChangedComments;
  end;
end;

procedure TfmLexerProp.edColorBGChange(Sender: TObject);
begin
  IsChangedLexer:= true;
  UpdateStlToListbox;
end;

procedure TfmLexerProp.chkBoldChange(Sender: TObject);
begin
  IsChangedLexer:= true;
  UpdateStlToListbox;
end;

procedure TfmLexerProp.edCmtStream1Change(Sender: TObject);
begin
  IsChangedComments:= true;
end;

procedure TfmLexerProp.cbBorderLChange(Sender: TObject);
begin
  IsChangedLexer:= true;
  UpdateStlToListbox;
end;

procedure TfmLexerProp.edNameChange(Sender: TObject);
begin
  IsChangedLexer:= true;
end;

procedure TfmLexerProp.FormDestroy(Sender: TObject);
begin
  Adapter.AddEditor(nil);
  FreeAndNil(Adapter);

  FFormats.Clear;
  FreeAndNil(FFormats);
end;

procedure TfmLexerProp.FormShow(Sender: TObject);
var
  i: integer;
begin
  UpdateFormOnTop(Self);

  Pages.PageIndex:= 0;

  InitBorder(cbBorderL);
  InitBorder(cbBorderT);
  InitBorder(cbBorderR);
  InitBorder(cbBorderB);

  FFormats.Clear;
  for i:= 0 to FAnalyzer.Formats.Count-1 do
  begin
    FFormats.Add;
    FFormats[FFormats.Count-1].Assign(FAnalyzer.Formats[i]);
  end;

  with TIniFile.Create(GetAppLexerMapFilename(FAnalyzer.LexerName)) do
  try
    edCmtStream1.Text:= ReadString('comments', 'str1', '');
    edCmtStream2.Text:= ReadString('comments', 'str2', '');
    edCmtFull1.Text:= ReadString('comments', 'full1', '');
    edCmtFull2.Text:= ReadString('comments', 'full2', '');
    edStylesCmt.Text:= ReadString('comments', 'styles_cmt', '');
    edStylesStrings.Text:= ReadString('comments', 'styles_str', '');
  finally
    Free
  end;

  UpdateListboxStyles;
  FChangedAllowed:= true;
end;

procedure TfmLexerProp.ListStylesClick(Sender: TObject);
begin
  FChangedAllowed:= false;
  UpdateStlFromListbox;
  FChangedAllowed:= true;
end;

procedure TfmLexerProp.UpdateListboxStyles;
var
  i: integer;
begin
  ListStyles.Items.Clear;
  for i:= 0 to FFormats.Count-1 do
    ListStyles.Items.Add(FFormats[i].DisplayName);

  if ListStyles.Count>0 then
    ListStyles.ItemIndex:= 0;
  UpdateStlFromListbox;
end;



procedure TfmLexerProp.UpdateStlFromListbox;
var
  n: integer;
  fmt: TecSyntaxFormat;
begin
  n:= ListStyles.ItemIndex;
  if n<0 then exit;

  FLockedUpdate:= true;
  fmt:= FFormats[n];
  UpdateStlEn(fmt.FormatType);

  //edStyleType.ItemIndex:= Ord(fmt.FormatType);
  edColorFont.Selected:= fmt.Font.Color;
  edColorBG.Selected:= fmt.BgColor;
  edColorBorder.Selected:= fmt.BorderColorBottom;

  chkBold.Checked:= fsBold in fmt.Font.Style;
  chkItalic.Checked:= fsItalic in fmt.Font.Style;
  chkUnder.Checked:= fsUnderline in fmt.Font.Style;
  chkStrik.Checked:= fsStrikeOut in fmt.Font.Style;

  cbBorderL.ItemIndex:= Ord(fmt.BorderTypeLeft);
  cbBorderT.ItemIndex:= Ord(fmt.BorderTypeTop);
  cbBorderR.ItemIndex:= Ord(fmt.BorderTypeRight);
  cbBorderB.ItemIndex:= Ord(fmt.BorderTypeBottom);

  FLockedUpdate:= false;
end;

procedure TfmLexerProp.UpdateStlEn(fmt: TecFormatType);
begin
  if UiOps.LexerThemes then
  begin
    Panel1.Enabled:= false;
    Exit;
  end;

  edColorFont.Enabled:= fmt in [ftCustomFont, ftFontAttr, ftColor];
  edColorBG.Enabled:= true;

  chkBold.Enabled:= fmt in [ftCustomFont, ftFontAttr];
  chkItalic.Enabled:= chkBold.Enabled;
  chkUnder.Enabled:= chkBold.Enabled;
  chkStrik.Enabled:= chkBold.Enabled;
end;

procedure TfmLexerProp.UpdateStlToListbox;
var
  n: integer;
  fmt: TecSyntaxFormat;
  fs: TFontStyles;
begin
  if FLockedUpdate then exit;

  n:= ListStyles.ItemIndex;
  if n<0 then exit;
  fmt:= FFormats[n];

  //fmt.FormatType:= TecFormatType(edStyleType.ItemIndex);
  fmt.Font.Color:= edColorFont.Selected;
  fmt.BgColor:= edColorBG.Selected;
  fmt.BorderColorBottom:= edColorBorder.Selected;

  fs:= [];
  if chkBold.Checked then Include(fs, fsBold);
  if chkItalic.Checked then Include(fs, fsItalic);
  if chkUnder.Checked then Include(fs, fsUnderline);
  if chkStrik.Checked then Include(fs, fsStrikeOut);
  fmt.Font.Style:= fs;

  fmt.BorderTypeLeft:= TecBorderLineType(cbBorderL.ItemIndex);
  fmt.BorderTypeTop:= TecBorderLineType(cbBorderT.ItemIndex);
  fmt.BorderTypeRight:= TecBorderLineType(cbBorderR.ItemIndex);
  fmt.BorderTypeBottom:= TecBorderLineType(cbBorderB.ItemIndex);
end;


function DoShowDialogLexerProp(an: TecSyntAnalyzer; const AFontName: string;
  AFontSize: integer): boolean;
var
  F: TfmLexerProp;
  an2: TecSyntAnalyzer;
begin
  Result:= false;
  if an=nil then exit;

  F:= TfmLexerProp.Create(nil);
  try
    DoLocalize_FormLexerProp(F);

    DoApplyLexerStylesMap(an, an2);
    EditorApplyTheme(F.edSample);

    F.FAnalyzer:= an;
    F.edName.Text:= an.LexerName;
    F.edTypes.Text:= an.Extentions;
    F.edCmtLine.Text:= an.LineComment;
    F.edNotes.Lines.AddStrings(an.Notes);

    F.edSample.Font.Name:= AFontName;
    F.edSample.Font.Size:= AFontSize;
    F.edSample.Gutter[F.edSample.GutterBandBm].Visible:= false;
    F.edSample.Gutter[F.edSample.GutterBandNum].Visible:= false;
    F.Adapter.Lexer:= an;
    if Assigned(an.SampleText) then
    begin
      F.edSample.Strings.LoadFromString(an.SampleText.Text);
      F.edSample.Update(true);
      F.edSample.DoEventChange;
    end;

    if F.ShowModal<>mrOk then exit;
    if Trim(F.edName.Text)='' then exit;
    Result:= F.IsChangedLexer;
    if Result then
    begin
      an.LexerName:= F.edName.Text;
      an.Extentions:= F.edTypes.Text;
      an.LineComment:= F.edCmtLine.Text;
      //an.Notes.Clear;
      //an.Notes.AddStrings(F.edNotes.Lines);
    end;
  finally
    F.Free;
  end;
end;

procedure TfmLexerProp.InitBorder(cb: TCombobox);
begin
  with cb.Items do
  begin
    Clear;
    Add(msgBorderTypeNone);
    Add(msgBorderTypeSolid);
    Add(msgBorderTypeDash);
    Add(msgBorderTypeDot);
    Add(msgBorderTypeDashDot);
    Add(msgBorderTypeDashDotDot);
    Add(msgBorderTypeSolid2);
    Add(msgBorderTypeSolid3);
    Add(msgBorderTypeWave);
    Add(msgBorderTypeDouble);
  end;
end;

procedure TfmLexerProp.SetChangedComments(AValue: boolean);
begin
  if not FChangedAllowed then exit;
  FChangedComments:= AValue;
end;

procedure TfmLexerProp.SetChangedLexer(AValue: boolean);
begin
  if not FChangedAllowed then exit;
  FChangedLexer:= AValue;
end;

end.

