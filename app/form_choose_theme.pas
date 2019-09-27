unit form_choose_theme;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ButtonPanel,
  FileUtil, LazFileUtils, IniFiles, Math,
  at__jsonConf,
  proc_msg,
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
    ListboxSyntax: TListBox;
    ListboxUI: TListBox;
    procedure chkEnableLexChange(Sender: TObject);
    procedure chkSyncChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ListboxSyntaxClick(Sender: TObject);
    procedure ListboxUIClick(Sender: TObject);
  private
    procedure Localize;
    function GetEnableLexerThemes: boolean;
    function GetEnableSync: boolean;
    procedure SaveThemes;
    procedure SetEnableLexerThemes(AValue: boolean);
    procedure SetEnableSync(AValue: boolean);
  public
    ThemeUi: string;
    ThemeSyntax: string;
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

  ListboxUI.Items.Clear;
  ListboxSyntax.Items.Clear;

  Files_ui:= TStringList.Create;
  Files_sy:= TStringList.Create;
  try
    FindAllFiles(Files_ui, GetAppPath(cDirDataThemes), '*'+AppExtensionThemeUi, false);
    Files_ui.Sort;

    FindAllFiles(Files_sy, GetAppPath(cDirDataThemes), '*'+AppExtensionThemeSyntax, false);
    Files_sy.Sort;

    ListboxUI.Items.Add(msgThemeDefault);
    for s in Files_ui do
      ListboxUI.Items.Add(LowerCase(ExtractFileNameOnly(s)));

    ListboxSyntax.Items.Add(msgThemeDefault);
    for s in Files_sy do
      ListboxSyntax.Items.Add(LowerCase(ExtractFileNameOnly(s)));

  finally
    FreeAndNil(Files_ui);
    FreeAndNil(Files_sy);
  end;
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

procedure TfmChooseTheme.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  if ModalResult=mrOk then
    SaveThemes;
end;

procedure TfmChooseTheme.FormShow(Sender: TObject);
begin
  ListboxUI.ItemIndex:= Max(0, ListboxUI.Items.IndexOf(ThemeUI));
  ListboxSyntax.ItemIndex:= Max(0, ListboxSyntax.Items.IndexOf(ThemeSyntax));
  chkSync.Checked:= EnableSync;
  chkEnableLex.Checked:= EnableLexerThemes;
  ListboxSyntax.Enabled:= chkEnableLex.Checked;
end;

procedure TfmChooseTheme.ListboxUIClick(Sender: TObject);
var
  N: integer;
  S: string;
begin
  N:=ListboxUi.ItemIndex;
  if N>0 then
    S:= ListboxUi.Items[N]
  else
    S:= '';
  ThemeUiSetter(S);

  if chkSync.Checked and chkEnableLex.Checked
    and Assigned(Sender) then //check Sender to not do recursion
  begin
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
end;

procedure TfmChooseTheme.ListboxSyntaxClick(Sender: TObject);
var
  N: integer;
  S: string;
begin
  N:= ListboxSyntax.ItemIndex;
  if N>0 then
    S:= ListboxSyntax.Items[N]
  else
    S:= '';
  ThemeSyntaxSetter(S);

  if chkSync.Checked then
  begin
    if S='' then
      N:= 0
    else
      N:= ListboxUI.Items.IndexOf(S);
    if N>=0 then
    begin
      ListboxUI.ItemIndex:= N;
      ListboxUIClick(nil); //nil: don't do recursion
    end;
  end;
end;

procedure TfmChooseTheme.Localize;
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
    c.FileName:= GetAppPath(cFileOptionsUser);
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
    c.FileName:= GetAppPath(cFileOptionsHistory);
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
    c.FileName:= GetAppPath(cFileOptionsUser);
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
    c.FileName:= GetAppPath(cFileOptionsHistory);
    c.SetDeleteValue('/sync_choose_themes', AValue, false);
  finally
    c.Free;
  end;
end;

procedure TfmChooseTheme.SaveThemes;
var
  c: TJSONConfig;
begin
  c:= TJSONConfig.Create(nil);
  try
    c.Formatted:= true;
    c.FileName:= GetAppPath(cFileOptionsUser);
    c.SetValue('/ui_theme', UiOps.ThemeUi);
    c.SetValue('/ui_theme_syntax', UiOps.ThemeSyntax);
  finally
    c.Free;
  end;
end;



end.

