unit formlexerstyle;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  ColorBox, StdCtrls, ButtonPanel,
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
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    procedure InitBorder(cb: TCombobox);
    { private declarations }
  public
    { public declarations }
  end;

var
  fmLexerStyle: TfmLexerStyle;

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

procedure TfmLexerStyle.FormShow(Sender: TObject);
begin
end;

procedure TfmLexerStyle.FormCreate(Sender: TObject);
begin
  InitBorder(cbBorderL);
  InitBorder(cbBorderT);
  InitBorder(cbBorderR);
  InitBorder(cbBorderB);
end;

procedure TfmLexerStyle.InitBorder(cb: TCombobox);
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


end.

