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
  ec_SyntAnal,
  ec_syntax_format,
  proc_msg,
  proc_globdata,
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
var
  LexersAsked: TFPList = nil;

procedure DoClearLexersAskedList(an: TecSyntAnalyzer = nil);
var
  n: integer;
begin
  if not Assigned(LexersAsked) then exit;

  if an=nil then
    LexersAsked.Clear
  else
  begin
    n:= LexersAsked.IndexOf(an);
    if n>=0 then LexersAsked.Delete(n);
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
  iStyle: TAppThemeStyleId;
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

  if LexersAsked.IndexOf(an)>=0 then exit;
  LexersAsked.Add(an);

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
  iStyle: TAppThemeStyleId;
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
      F.ItemsTh.Add(AppTheme.Styles[iStyle].DisplayName);

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
  ItemsLex:= TStringlist.Create;
  ItemsTh:= TStringlist.Create;
  ItemsVal:= TStringlist.Create;
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
end;

procedure TfmLexerStyleMap.DoSave;
var
  i: integer;
begin
  if LexerName='' then exit;
  with TIniFile.Create(GetAppLexerMapFilename(LexerName)) do
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
  with TIniFile.Create(GetAppLexerMapFilename(LexerName)) do
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
  fn:= GetAppLangFilename;
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


initialization
  LexersAsked:= TFPList.Create;
  EControlOptions.OnLexerApplyTheme:= @DoApplyLexerStylesMap;

finalization
  if Assigned(LexersAsked) then
  begin
    LexersAsked.Clear;
    LexersAsked.Free;
  end;

end.

