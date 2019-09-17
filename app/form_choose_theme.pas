unit form_choose_theme;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ButtonPanel,
  FileUtil, LazFileUtils, IniFiles, Math,
  proc_msg,
  proc_globdata;

type
  TAppThemeSetter = procedure(const S: string) of object;

type
  { TfmChooseTheme }

  TfmChooseTheme = class(TForm)
    ButtonPanel1: TButtonPanel;
    chkSync: TCheckBox;
    GroupUI: TGroupBox;
    GroupSyntax: TGroupBox;
    ListboxSyntax: TListBox;
    ListboxUI: TListBox;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ListboxSyntaxClick(Sender: TObject);
    procedure ListboxUIClick(Sender: TObject);
  private
  public
    ThemeUi: string;
    ThemeSyntax: string;
    ThemeUiSetter: TAppThemeSetter;
    ThemeSyntaxSetter: TAppThemeSetter;
  end;

var
  fmChooseTheme: TfmChooseTheme;

implementation

{$R *.lfm}

procedure DoLocalize_FormChooseTheme(F: TfmChooseTheme);
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

    with F.chkSync do Caption:= ini.ReadString(section, 'syn', Caption);
    with F.GroupUI do Caption:= ini.ReadString(section, 'ty_ui', Caption);
    with F.GroupSyntax do Caption:= ini.ReadString(section, 'ty_sy', Caption);
  finally
    FreeAndNil(ini);
  end;
end;

{ TfmChooseTheme }

procedure TfmChooseTheme.FormCreate(Sender: TObject);
var
  Files_ui: TStringList;
  Files_sy: TStringList;
  s: string;
begin
  DoLocalize_FormChooseTheme(Self);

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

procedure TfmChooseTheme.FormShow(Sender: TObject);
begin
  ListboxUI.ItemIndex:= Max(0, ListboxUI.Items.IndexOf(ThemeUI));
  ListboxSyntax.ItemIndex:= Max(0, ListboxSyntax.Items.IndexOf(ThemeSyntax));
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

  if chkSync.Checked then
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
end;

end.

