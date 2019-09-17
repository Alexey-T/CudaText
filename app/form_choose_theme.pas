unit form_choose_theme;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ButtonPanel,
  FileUtil,
  LazFileUtils,
  Math,
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

{ TfmChooseTheme }

procedure TfmChooseTheme.FormCreate(Sender: TObject);
var
  Files_ui: TStringList;
  Files_sy: TStringList;
  s: string;
begin
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
    ListboxUI.ItemIndex:= Max(0, ListboxUI.Items.IndexOf(ThemeUI));

    ListboxSyntax.Items.Add(msgThemeDefault);
    for s in Files_sy do
      ListboxSyntax.Items.Add(LowerCase(ExtractFileNameOnly(s)));
    ListboxSyntax.ItemIndex:= Max(0, ListboxSyntax.Items.IndexOf(ThemeSyntax));

  finally
    FreeAndNil(Files_ui);
    FreeAndNil(Files_sy);
  end;
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

