unit formlexerstylemap;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ButtonPanel,
  StdCtrls, StrUtils, IniFiles,
  ecSyntAnal,
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
  private
    { private declarations }
    LexerName: string;
    procedure DoLoad;
    procedure DoSave;
    procedure UpdateList;
  public
    { public declarations }
    ItemsLex: TStringlist;
    ItemsTh: TStringlist;
    ItemsVal: TStringlist;
  end;

var
  fmLexerStyleMap: TfmLexerStyleMap;

function DoCheckLexerStylesMap(an: TecSyntAnalyzer): boolean;
procedure DoDialogLexerStylesMap(an: TecSyntAnalyzer);


implementation

{$R *.lfm}

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

procedure DoLocalize_FormLexerStylesMap(F: TfmLexerStyleMap);
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
    with F do Caption:= ini.ReadString(section, '_', Caption);
    with F.ButtonPanel1.OKButton do Caption:= msgButtonOk;
    with F.ButtonPanel1.CancelButton do Caption:= msgButtonCancel;
    with F.LabelLex do Caption:= ini.ReadString(section, 'st_lex', Caption);
    with F.LabelTh do Caption:= ini.ReadString(section, 'st_th', Caption);
    with F.btnSet do Caption:= ini.ReadString(section, 'set_sel', Caption);
    with F.btnSetNone do Caption:= ini.ReadString(section, 'set_non', Caption);
    with F.btnClear do Caption:= ini.ReadString(section, 'set_un', Caption);
  finally
    FreeAndNil(ini);
  end;
end;


function DoCheckLexerStylesMap(an: TecSyntAnalyzer): boolean;
var
  value: string;
  st: TecSyntaxFormat;
  i: integer;
begin
  Result:= true;
  if an=nil then exit;
  if an.Formats.Count=0 then exit;

  with TIniFile.Create(GetAppPath(cFileOptStylesMap)) do
  try
    for i:= 0 to an.Formats.Count-1 do
    begin
      value:= ReadString(an.LexerName, an.Formats[i].DisplayName, '');
      if value='' then Result:= false; //not exit
      if value='-' then Continue;

      st:= GetAppStyleFromName(value);
      if Assigned(st) then
        DoStyleAssign(an.Formats[i], st);
    end;
  finally
    Free
  end;
end;

procedure DoDialogLexerStylesMap(an: TecSyntAnalyzer);
var
  F: TfmLexerStyleMap;
  i: integer;
begin
  if an=nil then exit;
  if an.Formats.Count=0 then exit;

  F:= TfmLexerStyleMap.Create(nil);
  try
    DoLocalize_FormLexerStylesMap(F);
    F.LexerName:= an.LexerName;
    F.Caption:= F.Caption + ' - ' + F.LexerName;

    for i:= 0 to an.Formats.Count-1 do
      F.ItemsLex.Add(an.Formats[i].DisplayName);
    for i:= 0 to an.Formats.Count-1 do
      F.ItemsVal.Add('');
    for i:= 0 to Theme.Styles.Count-1 do
      F.ItemsTh.Add(TecSyntaxFormat(Theme.Styles[i]).DisplayName);

    F.ListLex.Items.AddStrings(F.ItemsLex);
    F.ListLex.ItemIndex:= 0;
    F.ListTh.Items.AddStrings(F.ItemsTh);
    F.ListTh.ItemIndex:= 0;

    F.DoLoad;
    F.UpdateList;

    if F.ShowModal=mrOk then
    begin
      F.DoSave;
      DoCheckLexerStylesMap(an); //reapply new colors
    end;
  finally
    F.Free;
  end;
end;

{ TfmLexerStyleMap }

procedure TfmLexerStyleMap.FormCreate(Sender: TObject);
begin
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

procedure TfmLexerStyleMap.DoSave;
var
  fn: string;
  i: integer;
begin
  if LexerName='' then
  begin
    ShowMessage('Lexer name not set');
    exit
  end;

  fn:= GetAppPath(cFileOptStylesMap);
  with TIniFile.Create(fn) do
  try
    EraseSection(LexerName);
    for i:= 0 to ItemsLex.Count-1 do
      WriteString(LexerName, ItemsLex[i], ItemsVal[i]);
  finally
    Free
  end;
end;

procedure TfmLexerStyleMap.DoLoad;
var
  fn: string;
  i: integer;
begin
  if LexerName='' then
  begin
    ShowMessage('Lexer name not set');
    exit
  end;

  fn:= GetAppPath(cFileOptStylesMap);
  with TIniFile.Create(fn) do
  try
    for i:= 0 to ItemsLex.Count-1 do
      ItemsVal[i]:= ReadString(LexerName, ItemsLex[i], '');
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

end.

