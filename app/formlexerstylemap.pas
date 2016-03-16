unit formlexerstylemap;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ButtonPanel,
  StdCtrls, strutils,
  ecSyntAnal,
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
    procedure UpdateList;
  public
    { public declarations }
    ItemsLex: TStringlist;
    ItemsTh: TStringlist;
    ItemsVal: TStringlist;
  end;

var
  fmLexerStyleMap: TfmLexerStyleMap;

procedure DoDialogLexerStylesMap(an: TecSyntAnalyzer);

implementation

{$R *.lfm}

procedure DoDialogLexerStylesMap(an: TecSyntAnalyzer);
var
  F: TfmLexerStyleMap;
  i: integer;
begin
  if an=nil then exit;
  if an.Formats.Count=0 then exit;

  F:= TfmLexerStyleMap.Create(nil);
  try
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
    F.UpdateList;

    if F.ShowModal=mrOk then
    begin
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

procedure TfmLexerStyleMap.UpdateList;
var
  n, i: integer;
begin
  n:= ListLex.ItemIndex;
  ListLex.Items.BeginUpdate;
  ListLex.Items.Clear;
  for i:= 0 to ItemsLex.Count-1 do
    ListLex.Items.Add(ItemsLex[i] + IfThen(ItemsVal[i]<>'', ' >>> ' + ItemsVal[i]));
  ListLex.Items.EndUpdate;
  ListLex.ItemIndex:= n;
end;

end.

