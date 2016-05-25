unit formlexerstyle;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  ColorBox, StdCtrls, ButtonPanel, IniFiles,
  ecSyntAnal,
  proc_colors,
  proc_globdata,
  proc_msg;

type
  { TfmLexerStyle }

  TfmLexerStyle = class(TForm)
    ButtonPanel1: TButtonPanel;
    cbBorderB: TComboBox;
    cbBorderL: TComboBox;
    cbBorderR: TComboBox;
    cbBorderT: TComboBox;
    chkBold: TCheckBox;
    chkItalic: TCheckBox;
    chkStrik: TCheckBox;
    chkUnder: TCheckBox;
    edColorBG: TColorBox;
    edColorBorder: TColorBox;
    edColorFont: TColorBox;
    edStyleType: TComboBox;
    LabelBorder: TLabel;
    LabelBorderB: TLabel;
    LabelBorderL: TLabel;
    LabelBorderR: TLabel;
    LabelBorderT: TLabel;
    LabelColorBg: TLabel;
    LabelColorBorder: TLabel;
    LabelColorFont: TLabel;
    LabelFontStyles: TLabel;
    LabelStyleType: TLabel;
    Panel1: TPanel;
    procedure edStyleTypeChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    procedure InitBorder(cb: TCombobox);
    procedure UpdateStyleEn;
    { private declarations }
  public
    { public declarations }
  end;

var
  fmLexerStyle: TfmLexerStyle;

procedure DoLocalize_FormLexerStyle(F: TfmLexerStyle);


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

procedure DoLocalize_FormLexerStyle(F: TfmLexerStyle);
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
    with F do Caption:= ini.ReadString(section, '_style', Caption);
    with F.ButtonPanel1.OKButton do Caption:= msgButtonOk;
    with F.ButtonPanel1.CancelButton do Caption:= msgButtonCancel;

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

    with F.LabelStyleType do Caption:= ini.ReadString(section, 'typ_', Caption);
    with F.edStyleType do Items[0]:= ini.ReadString(section, 'typ_mi', Items[0]);
    with F.edStyleType do Items[1]:= ini.ReadString(section, 'typ_col_st', Items[1]);
    with F.edStyleType do Items[2]:= ini.ReadString(section, 'typ_col', Items[2]);
    with F.edStyleType do Items[3]:= ini.ReadString(section, 'typ_col_bg', Items[3]);

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

procedure TfmLexerStyle.FormCreate(Sender: TObject);
begin
  InitBorder(cbBorderL);
  InitBorder(cbBorderT);
  InitBorder(cbBorderR);
  InitBorder(cbBorderB);
end;

procedure TfmLexerStyle.edStyleTypeChange(Sender: TObject);
begin
  UpdateStyleEn;
end;

procedure TfmLexerStyle.FormShow(Sender: TObject);
begin
  FormCreate(nil);
  UpdateStyleEn;
end;

procedure TfmLexerStyle.InitBorder(cb: TCombobox);
var
  n: integer;
begin
  n:= cb.ItemIndex;
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
  cb.ItemIndex:= n;
end;


procedure TfmLexerStyle.UpdateStyleEn;
var
  fmt: TecFormatType;
begin
  fmt:= TecFormatType(edStyleType.ItemIndex);
  edColorFont.Enabled:= fmt in [ftCustomFont, ftFontAttr, ftColor];
  edColorBG.Enabled:= true;

  chkBold.Enabled:= fmt in [ftCustomFont, ftFontAttr];
  chkItalic.Enabled:= chkBold.Enabled;
  chkUnder.Enabled:= chkBold.Enabled;
  chkStrik.Enabled:= chkBold.Enabled;
end;

end.

