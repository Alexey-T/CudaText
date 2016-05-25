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

function DoApplyLexerStylesMap(an: TecSyntAnalyzer): boolean;
function DoDialogLexerStylesMap(an: TecSyntAnalyzer): boolean;
procedure DoClearLexersAskedList(an: TecSyntAnalyzer = nil);


implementation

{$R *.lfm}

const
  cSectionMap = 'map';
var
  LexersAsked: TList = nil;

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


function DoApplyLexerStylesMap(an: TecSyntAnalyzer): boolean;
var
  value: string;
  st: TecSyntaxFormat;
  i: integer;
begin
  Result:= true;
  if an=nil then exit;
  if an.Formats.Count=0 then exit;
  if not UiOps.LexerThemes then exit;

  if LexersAsked.IndexOf(an)>=0 then exit;
  LexersAsked.Add(an);

  //work for sublexers
  for i:= 0 to an.SubAnalyzers.Count-1 do
    if Assigned(an.SubAnalyzers[i]) then
      if not DoApplyLexerStylesMap(an.SubAnalyzers[i].SyntAnalyzer) then
        Result:= false; //not exit

  with TIniFile.Create(GetAppLexerMapFilename(an.LexerName)) do
  try
    for i:= 0 to an.Formats.Count-1 do
    begin
      value:= ReadString(cSectionMap, an.Formats[i].DisplayName, '');
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

function DoDialogLexerStylesMap(an: TecSyntAnalyzer): boolean;
var
  F: TfmLexerStyleMap;
  i: integer;
begin
  Result:= false;
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
    for i:= 0 to AppTheme.Styles.Count-1 do
      F.ItemsTh.Add(TecSyntaxFormat(AppTheme.Styles[i]).DisplayName);

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
      DoApplyLexerStylesMap(an);
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


initialization
  LexersAsked:= TList.Create;

finalization
  if Assigned(LexersAsked) then
  begin
    LexersAsked.Clear;
    LexersAsked.Free;
  end;

end.

