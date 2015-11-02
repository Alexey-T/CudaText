(*
This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at http://mozilla.org/MPL/2.0/.

Copyright (c) Alexey Torgashin
*)
unit formlexerstyles;

interface

uses
  SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, CheckLst, IniFiles,
  LclProc, LclType, ButtonPanel,
  proc_globdata;

type
  { TfmLexerStylesRestore }

  TfmLexerStylesRestore = class(TForm)
    ButtonPanel1: TButtonPanel;
    Label1: TLabel;
    GroupBox1: TGroupBox;
    List: TCheckListBox;
    bSelAll: TButton;
    bSelNone: TButton;
    bDelete: TButton;
    procedure FormShow(Sender: TObject);
    procedure bSelAllClick(Sender: TObject);
    procedure bSelNoneClick(Sender: TObject);
    procedure bDeleteClick(Sender: TObject);
    procedure ListClickCheck(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    StylesFilename: string;
  end;

implementation

{$R *.lfm}

procedure TfmLexerStylesRestore.FormShow(Sender: TObject);
var
  L: TStringList;
  Ini: TIniFile;
begin
  if StylesFilename='' then Exit;
  List.Items.Clear;

  L:= TStringList.Create;
  Ini:= TIniFile.Create(StylesFilename);
  try
    L.Sorted:= true;
    Ini.ReadSections(L);
    List.Items.AddStrings(L);
  finally
    FreeAndNil(Ini);
    FreeAndNil(L);
  end;

  bSelAllClick(Self);
  bSelAll.Enabled:= List.Items.Count>0;
  bSelNone.Enabled:= bSelAll.Enabled;
end;

procedure TfmLexerStylesRestore.bSelAllClick(Sender: TObject);
var
  i: Integer;
begin
  with List do
    for i:= 0 to Items.Count-1 do
      Checked[i]:= true;
  ListClickCheck(Self);
end;

procedure TfmLexerStylesRestore.bSelNoneClick(Sender: TObject);
var
  i: Integer;
begin
  with List do
    for i:= 0 to Items.Count-1 do
      Checked[i]:= false;
  ListClickCheck(Self);
end;

procedure TfmLexerStylesRestore.bDeleteClick(Sender: TObject);
var
  i: Integer;
begin
  if MsgBox('Remove checked styles from backup file?', MB_OKCANCEL or MB_ICONWARNING)<>ID_OK then Exit;

  with TIniFile.Create(StylesFilename) do
  try
    with List do
      for i:= 0 to Items.Count-1 do
        if Checked[i] then
          EraseSection(Items[i]);
  finally
    Free
  end;
  
  FormShow(Self);
end;

procedure TfmLexerStylesRestore.ListClickCheck(Sender: TObject);
var
  i: Integer;
  en: boolean;
begin
  en:= false;
  with List do
    for i:= 0 to Items.Count-1 do
      if Checked[i] then
        begin en:= true; Break end;

  ButtonPanel1.OKButton.Enabled:= en;
  ButtonPanel1.CancelButton.Enabled:= en;
end;

end.
