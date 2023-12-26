(*
This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at http://mozilla.org/MPL/2.0/.

Copyright (c) Alexey Torgashin
*)
unit formlexerstylemap;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ButtonPanel,
  StdCtrls, StrUtils, IniFiles,
  Types, LCLType,
  ec_SyntAnal,
  ec_syntax_format,
  proc_msg,
  proc_globdata,
  proc_customdialog,
  proc_colors;

type
  { TfmLexerStyleMap }

  TfmLexerStyleMap = class(TForm)
    btnSet: TButton;
    btnSetNone: TButton;
    btnClear: TButton;
    ButtonPanel1: TButtonPanel;
    LabelLex: TLabel;
    LabelTh: TLabel;
    ListLex: TListBox;
    ListTh: TListBox;
    procedure btnClearClick(Sender: TObject);
    procedure btnSetClick(Sender: TObject);
    procedure btnSetNoneClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ListLexSelectionChange(Sender: TObject; User: boolean);
    procedure ListThDrawItem(Control: TWinControl; AIndex: Integer;
      ARect: TRect; State: TOwnerDrawState);
  private
    { private declarations }
    LexerName: string;
    procedure DoLoad;
    procedure DoSave;
    procedure Localize;
    procedure UpdateList;
  public
    { public declarations }
    ItemsLex: TStringlist;
    ItemsTh: TStringlist;
    ItemsVal: TStringlist;
  end;

var
  fmLexerStyleMap: TfmLexerStyleMap;

function DoApplyLexerStylesMap(an: TecSyntAnalyzer; out anNotCorrect: TecSyntAnalyzer): boolean;
function DoDialogLexerStylesMap(an: TecSyntAnalyzer): boolean;
procedure DoClearLexersAskedList(an: TecSyntAnalyzer = nil);


implementation

{$R *.lfm}

const
  cSectionMap = 'map';

procedure DoClearLexersAskedList(an: TecSyntAnalyzer = nil);
// if an=nil, clear for all lexers
var
  anLoop: TecSyntAnalyzer;
  i: integer;
begin
  if an=nil then
  begin
    //fixing issue #4811 (dialog to customize syntax theme cannnot apply changed colors)
    for i:= 0 to AppManager.LexerCount-1 do
    begin
      anLoop:= AppManager.Lexers[i];
      anLoop.AppliedSyntaxTheme:= '';
      anLoop.AskedToApplyLexerMap:= false;
    end;
  end
  else
  begin
    an.AppliedSyntaxTheme:= '';
    an.AskedToApplyLexerMap:= false;
  end;
end;

procedure DoStyleAssign(s, s2: TecSyntaxFormat);
begin
  s.FormatType:= s2.FormatType;
  s.Font.Color:= s2.Font.Color;
  s.Font.Style:= s2.Font.Style;
  s.BgColor:= s2.BgColor;
  s.BorderColorLeft:= s2.BorderColorLeft;
  s.BorderColorRight:= s2.BorderColorRight;
  s.BorderColorTop:= s2.BorderColorTop;
  s.BorderColorBottom:= s2.BorderColorBottom;
  s.BorderTypeLeft:= s2.BorderTypeLeft;
  s.BorderTypeRight:= s2.BorderTypeRight;
  s.BorderTypeTop:= s2.BorderTypeTop;
  s.BorderTypeBottom:= s2.BorderTypeBottom;
end;


function DoApplyLexerStylesMap(an: TecSyntAnalyzer; out anNotCorrect: TecSyntAnalyzer): boolean;
var
  value: string;
  st: TecSyntaxFormat;
  iStyle: TAppThemeStyle;
  anSub: TecSyntAnalyzer;
  NewThemeName: string;
  i: integer;
begin
  Result:= true;
  anNotCorrect:= an;
  if an=nil then exit;
  if an.Formats.Count=0 then exit;
  if not UiOps.LexerThemes then exit;

  NewThemeName:= UiOps.ThemeSyntax;
  if NewThemeName='' then
    NewThemeName:= '-';
  if NewThemeName=an.AppliedSyntaxTheme then exit;

  if an.AskedToApplyLexerMap then exit;
  an.AskedToApplyLexerMap:= true;

  AppTheme.UpdateBoldAndItalicColors;

  //work for sublexers
  for i:= 0 to an.SubAnalyzers.Count-1 do
    if Assigned(an.SubAnalyzers[i]) then
    begin
      anSub:= an.SubAnalyzers[i].SyntAnalyzer;
      if not DoApplyLexerStylesMap(anSub, anNotCorrect) then
      begin
        anNotCorrect:= anSub;
        Result:= false;
        exit;
      end;
    end;

  for i:= 0 to an.Formats.Count-1 do
  begin
    value:= an.ThemeMappingOfStyle(an.Formats[i].DisplayName);
    if value='-' then
      Continue;
    if value='' then
    begin
      anNotCorrect:= an;
      Result:= false;
      exit;
    end;

    for iStyle:= Low(iStyle) to High(iStyle) do
    begin
      st:= GetAppStyle(iStyle);
      if st.DisplayName=value then
      begin
        DoStyleAssign(an.Formats[i], st);
        Break
      end;
    end;

    an.AppliedSyntaxTheme:= NewThemeName;
  end;
end;

function DoDialogLexerStylesMap(an: TecSyntAnalyzer): boolean;
var
  F: TfmLexerStyleMap;
  anNotCorrent: TecSyntAnalyzer;
  iStyle: TAppThemeStyle;
  st: TecSyntaxFormat;
  i: integer;
begin
  Result:= false;
  if an=nil then exit;
  if an.Formats.Count=0 then exit;

  F:= TfmLexerStyleMap.Create(nil);
  try
    F.LexerName:= an.LexerName;
    F.Caption:= F.Caption + ' - ' + F.LexerName;

    for i:= 0 to an.Formats.Count-1 do
      F.ItemsLex.Add(an.Formats[i].DisplayName);
    for i:= 0 to an.Formats.Count-1 do
      F.ItemsVal.Add('');
    for iStyle:= Low(iStyle) to High(iStyle) do
    begin
      st:= AppTheme.Styles[iStyle];
      F.ItemsTh.AddObject(st.DisplayName, st);
    end;

    F.ListLex.Items.AddStrings(F.ItemsLex);
    F.ListLex.ItemIndex:= 0;
    F.ListTh.Items.AddStrings(F.ItemsTh);
    F.ListTh.ItemIndex:= 0;

    F.DoLoad;
    F.UpdateList;

    Result:= F.ShowModal=mrOk;
    if Result then
    begin
      F.DoSave;
      DoClearLexersAskedList(an);
      DoApplyLexerStylesMap(an, anNotCorrent);
    end;
  finally
    F.Free;
  end;
end;

{ TfmLexerStyleMap }

procedure TfmLexerStyleMap.FormCreate(Sender: TObject);
begin
  Localize;
  DoForm_ScaleAuto(Self, false);

  ListTh.ItemHeight:= Canvas.TextHeight('Tj');

  ItemsLex:= TStringList.Create;
  ItemsTh:= TStringList.Create;
  ItemsVal:= TStringList.Create;
end;

procedure TfmLexerStyleMap.btnSetClick(Sender: TObject);
begin
  ItemsVal[ListLex.ItemIndex]:= ListTh.Items[ListTh.ItemIndex];
  UpdateList;
end;

procedure TfmLexerStyleMap.btnClearClick(Sender: TObject);
begin
  ItemsVal[ListLex.ItemIndex]:= '';
  UpdateList;
end;

procedure TfmLexerStyleMap.btnSetNoneClick(Sender: TObject);
begin
  ItemsVal[ListLex.ItemIndex]:= '-';
  UpdateList;
end;

procedure TfmLexerStyleMap.FormDestroy(Sender: TObject);
begin
  FreeAndNil(ItemsLex);
  FreeAndNil(ItemsTh);
  FreeAndNil(ItemsVal);
end;

procedure TfmLexerStyleMap.FormShow(Sender: TObject);
begin
  UpdateFormOnTop(Self);
  ListLexSelectionChange(Self, false);
end;

procedure TfmLexerStyleMap.ListLexSelectionChange(Sender: TObject; User: boolean);
var
  i: integer;
begin
  if ListLex.ItemIndex<0 then exit;
  i:= ListTh.Items.IndexOf(ItemsVal[ListLex.ItemIndex]);
  if i>=0 then
    ListTh.ItemIndex:= i;
end;

procedure TfmLexerStyleMap.DoSave;
var
  i: integer;
begin
  if LexerName='' then exit;
  with TIniFile.Create(AppFile_LexerMap(LexerName)) do
  try
    EraseSection(cSectionMap);
    for i:= 0 to ItemsLex.Count-1 do
      WriteString(cSectionMap, ItemsLex[i], ItemsVal[i]);
  finally
    Free
  end;
end;

procedure TfmLexerStyleMap.DoLoad;
var
  i: integer;
begin
  if LexerName='' then exit;
  with TIniFile.Create(AppFile_LexerMap(LexerName)) do
  try
    for i:= 0 to ItemsLex.Count-1 do
      ItemsVal[i]:= ReadString(cSectionMap, ItemsLex[i], '');
  finally
    Free
  end;
end;

procedure TfmLexerStyleMap.UpdateList;
const
  cArrow = '  ---  ';
var
  i: integer;
begin
  ListLex.Items.BeginUpdate;
  for i:= 0 to ItemsLex.Count-1 do
    ListLex.Items[i]:= ItemsLex[i] + cArrow + IfThen(ItemsVal[i]<>'', ItemsVal[i], '?');
  ListLex.Items.EndUpdate;
end;

procedure TfmLexerStyleMap.Localize;
const
  section = 'd_lex_map';
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
    with LabelLex do Caption:= ini.ReadString(section, 'st_lex', Caption);
    with LabelTh do Caption:= ini.ReadString(section, 'st_th', Caption);
    with btnSet do Caption:= ini.ReadString(section, 'set_sel', Caption);
    with btnSetNone do Caption:= ini.ReadString(section, 'set_non', Caption);
    with btnClear do Caption:= ini.ReadString(section, 'set_un', Caption);
  finally
    FreeAndNil(ini);
  end;
end;


procedure TfmLexerStyleMap.ListThDrawItem(Control: TWinControl; AIndex: Integer;
  ARect: TRect; State: TOwnerDrawState);
const
  cIndent = 6;
  cExample = ' Example ';
var
  C: TCanvas;
  st: TecSyntaxFormat;
  NWidth: integer;
begin
  if (AIndex<0) or (AIndex>=ListTh.Items.Count) then exit;

  C:= (Control as TListbox).Canvas;
  st:= ListTh.Items.Objects[AIndex] as TecSyntaxFormat;

  C.Brush.Color:= clWindow;
  C.FillRect(ARect);

  C.Font.Color:= st.Font.Color;
  C.Font.Style:= st.Font.Style;
  C.Brush.Color:= st.BgColor;
  if st.BgColor=clNone then
    C.Brush.Color:= AppTheme.Colors[TAppThemeColor.EdTextBg].Color;

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
  C.TextOut(ARect.Left+cIndent, ARect.Top, ListTh.Items[AIndex]);
end;


initialization
  EControlOptions.OnLexerApplyTheme:= @DoApplyLexerStylesMap;

finalization

end.

