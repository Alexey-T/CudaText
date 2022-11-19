(*
This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at http://mozilla.org/MPL/2.0/.

Copyright (c) Alexey Torgashin
*)
unit form_choose_theme;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ButtonPanel,
  ExtCtrls, FileUtil, LazFileUtils, IniFiles, Math,
  at__jsonConf,
  proc_msg,
  proc_customdialog,
  proc_globdata;

type
  TAppThemeSetter = procedure(const S: string) of object;

type
  { TfmChooseTheme }

  TfmChooseTheme = class(TForm)
    ButtonPanel1: TButtonPanel;
    chkEnableLex: TCheckBox;
    chkSync: TCheckBox;
    GroupUI: TGroupBox;
    GroupSyntax: TGroupBox;
    IdleTimer1: TIdleTimer;
    ListboxSyntax: TListBox;
    ListboxUI: TListBox;
    procedure chkEnableLexChange(Sender: TObject);
    procedure chkSyncChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure IdleTimer1Timer(Sender: TObject);
    procedure ListboxSyntaxClick(Sender: TObject);
    procedure ListboxUIClick(Sender: TObject);
  private
    FBusyClickUi: boolean;
    FBusyClickSyntax: boolean;
    procedure Localize;
    function GetEnableLexerThemes: boolean;
    function GetEnableSync: boolean;
    function SelectedThemeUI: string;
    function SelectedThemeSyntax: string;
    procedure SetEnableLexerThemes(AValue: boolean);
    procedure SetEnableSync(AValue: boolean);
  public
    ThemeUiSetter: TAppThemeSetter;
    ThemeSyntaxSetter: TAppThemeSetter;
    property EnableLexerThemes: boolean read GetEnableLexerThemes write SetEnableLexerThemes;
    property EnableSync: boolean read GetEnableSync write SetEnableSync;
  end;

var
  fmChooseTheme: TfmChooseTheme;

implementation

{$R *.lfm}

{ TfmChooseTheme }

procedure TfmChooseTheme.FormCreate(Sender: TObject);
var
  Files_ui: TStringList;
  Files_sy: TStringList;
  s: string;
begin
  Localize;
  DoForm_ScaleAuto(Self, false);

  ListboxUI.Items.Clear;
  ListboxSyntax.Items.Clear;

  Files_ui:= TStringList.Create;
  Files_sy:= TStringList.Create;
  try
    FindAllFiles(Files_ui, AppDir_DataThemes, '*'+AppExtensionThemeUi, false);
    Files_ui.Sort;

    FindAllFiles(Files_sy, AppDir_DataThemes, '*'+AppExtensionThemeSyntax, false);
    Files_sy.Sort;

    ListboxUI.Items.Add(msgThemeDefault);
    for s in Files_ui do
      ListboxUI.Items.Add(ExtractFileNameOnly(s));

    ListboxSyntax.Items.Add(msgThemeDefault);
    for s in Files_sy do
      ListboxSyntax.Items.Add(ExtractFileNameOnly(s));

  finally
    FreeAndNil(Files_ui);
    FreeAndNil(Files_sy);
  end;
end;

procedure TfmChooseTheme.FormResize(Sender: TObject);
begin
  GroupUI.Width:= Max(100, (ClientWidth - 2*6) div 2);
end;

procedure TfmChooseTheme.chkEnableLexChange(Sender: TObject);
begin
  EnableLexerThemes:= chkEnableLex.Checked;
  ListboxSyntax.Enabled:= chkEnableLex.Checked;
end;

procedure TfmChooseTheme.chkSyncChange(Sender: TObject);
begin
  EnableSync:= chkSync.Checked;
  if chkSync.Checked and chkEnableLex.Checked then
    ListboxUIClick(Self);
end;

procedure TfmChooseTheme.FormShow(Sender: TObject);
begin
  ListboxUI.ItemIndex:= Max(0, ListboxUI.Items.IndexOf(UiOps.ThemeUi));
  ListboxSyntax.ItemIndex:= Max(0, ListboxSyntax.Items.IndexOf(UiOps.ThemeSyntax));
  chkSync.Checked:= EnableSync;
  chkEnableLex.Checked:= EnableLexerThemes;
  ListboxSyntax.Enabled:= chkEnableLex.Checked;
end;

function TfmChooseTheme.SelectedThemeUI: string;
var
  N: integer;
begin
  N:= ListboxUI.ItemIndex;
  if N>0 then
    Result:= ListboxUI.Items[N]
  else
    Result:= '';
end;

function TfmChooseTheme.SelectedThemeSyntax: string;
var
  N: integer;
begin
  N:= ListboxSyntax.ItemIndex;
  if N>0 then
    Result:= ListboxSyntax.Items[N]
  else
    Result:= '';
end;

procedure TfmChooseTheme.ListboxUIClick(Sender: TObject);
var
  N: integer;
  S: string;
begin
  if FBusyClickUi then exit;
  FBusyClickUi:= true;

  if chkSync.Checked and chkEnableLex.Checked and not FBusyClickSyntax then
  begin
    S:= SelectedThemeUI;
    if S='' then
      N:= 0
    else
      N:= ListboxSyntax.Items.IndexOf(S);
    if N>=0 then
    begin
      ListboxSyntax.ItemIndex:= N;
      ListboxSyntaxClick(Self);
    end;
  end;

  FBusyClickUi:= false;
end;

procedure TfmChooseTheme.ListboxSyntaxClick(Sender: TObject);
var
  N: integer;
  S: string;
begin
  if FBusyClickSyntax then exit;
  FBusyClickSyntax:= true;

  if chkSync.Checked and not FBusyClickUi then
  begin
    S:= SelectedThemeSyntax;
    if S='' then
      N:= 0
    else
      N:= ListboxUI.Items.IndexOf(S);
    if N>=0 then
    begin
      ListboxUI.ItemIndex:= N;
      ListboxUIClick(Self);
    end;
  end;

  FBusyClickSyntax:= false;
end;

procedure TfmChooseTheme.Localize;
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

    with GroupUI do Caption:= ini.ReadString(section, 'ty_ui', Caption);
    with GroupSyntax do Caption:= ini.ReadString(section, 'ty_sy', Caption);

    with chkEnableLex do Caption:= ini.ReadString(section, 'ele', Caption);
    with chkSync do Caption:= ini.ReadString(section, 'syn', Caption);
  finally
    FreeAndNil(ini);
  end;
end;

function TfmChooseTheme.GetEnableLexerThemes: boolean;
var
  c: TJSONConfig;
begin
  c:= TJSONConfig.Create(nil);
  try
    c.FileName:= AppFile_OptionsUser;
    Result:= c.GetValue('/ui_lexer_themes', true);
  finally
    c.Free;
  end;
end;

function TfmChooseTheme.GetEnableSync: boolean;
var
  c: TJSONConfig;
begin
  c:= TJSONConfig.Create(nil);
  try
    c.FileName:= AppFile_History;
    Result:= c.GetValue('/sync_choose_themes', true);
  finally
    c.Free;
  end;
end;

procedure TfmChooseTheme.SetEnableLexerThemes(AValue: boolean);
var
  c: TJSONConfig;
begin
  c:= TJSONConfig.Create(nil);
  try
    c.Formatted:= true;
    c.FileName:= AppFile_OptionsUser;
    c.SetDeleteValue('/ui_lexer_themes', AValue, true);
  finally
    c.Free;
  end;
end;

procedure TfmChooseTheme.SetEnableSync(AValue: boolean);
var
  c: TJSONConfig;
begin
  c:= TJSONConfig.Create(nil);
  try
    c.Formatted:= true;
    c.FileName:= AppFile_History;
    c.SetDeleteValue('/sync_choose_themes', AValue, false);
  finally
    c.Free;
  end;
end;

procedure TfmChooseTheme.IdleTimer1Timer(Sender: TObject);
begin
  ThemeUiSetter(SelectedThemeUI);
  ThemeSyntaxSetter(SelectedThemeSyntax);
end;


end.

