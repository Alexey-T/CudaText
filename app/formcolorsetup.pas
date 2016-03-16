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
  IniFiles, ColorBox, StdCtrls,
  LazUTF8, LazFileUtils,
  ecSyntAnal,
  formlexerstyle,
  proc_msg,
  proc_globdata,
  proc_colors;

type
  TApplyThemeEvent = procedure(const AColors: TAppTheme) of object;

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
    procedure bChangeClick(Sender: TObject);
    procedure bNoneClick(Sender: TObject);
    procedure bStyleClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
    procedure ListKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    { private declarations }
    procedure Updatelist;
  public
    { public declarations }
    Data: TAppTheme;
    OnApply: TApplyThemeEvent;
  end;

procedure DoLocalize_FormColorSetup(F: TfmColorSetup);


implementation

{$R *.lfm}

procedure DoLocalize_FormColorSetup(F: TfmColorSetup);
const
  section = 'd_theme';
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
    with F.ButtonPanel1.HelpButton do Caption:= msgButtonApply;

    with F.bChange do Caption:= ini.ReadString(section, 'ch', Caption);
    with F.bNone do Caption:= ini.ReadString(section, 'non', Caption);
    with F.bStyle do Caption:= ini.ReadString(section, 'sty', Caption);
  finally
    FreeAndNil(ini);
  end;
end;


{ TfmColorSetup }

procedure TfmColorSetup.Updatelist;
var
  st: TecSyntaxFormat;
  i, n: integer;
begin
  n:= List.ItemIndex;
  List.Items.Clear;

  for i:= Low(Data.Colors) to High(Data.Colors) do
    List.Items.AddObject(Data.Colors[i].desc, TObject(ptrint(Data.Colors[i].color)));

  if ListStyles.Count=0 then
    for i:= 0 to Data.Styles.Count-1 do
    begin
      st:= TecSyntaxFormat(Data.Styles[i]);
      ListStyles.Items.AddObject(st.DisplayName, st);
    end;

  if n<List.Items.Count then
    List.ItemIndex:= n;
  List.Invalidate;
end;

procedure TfmColorSetup.bChangeClick(Sender: TObject);
begin
  ColorDialog1.Color:= ptrint(List.Items.Objects[List.itemindex]);
  if ColorDialog1.Execute then
  begin
    Data.Colors[List.Itemindex].color:= ColorDialog1.Color;
    Updatelist;
  end;
end;

procedure TfmColorSetup.bNoneClick(Sender: TObject);
begin
  Data.Colors[List.Itemindex].color:= clNone;
  Updatelist;
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

    DoLocalize_FormLexerStyle(Form);
    if ShowModal=mrOk then
    begin
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

procedure TfmColorSetup.FormShow(Sender: TObject);
begin
  Updatelist;
  List.ItemIndex:= 0;
  ListStyles.ItemIndex:= 0;
  List.SetFocus;
end;

procedure TfmColorSetup.HelpButtonClick(Sender: TObject);
begin
  if Assigned(OnApply) then
    OnApply(Data);
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

end.

