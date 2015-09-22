(*
This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at http://mozilla.org/MPL/2.0/.

Copyright (c) Alexey Torgashin
*)
unit formlexerlib;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ButtonPanel,
  StdCtrls, ComCtrls, CheckLst,
  LCLIntf, LCLType, LCLProc,
  ecSyntAnal,
  formlexerprop, proc_lexer_install_zip,
  math;

type
  { TfmLexerLib }

  TfmLexerLib = class(TForm)
    ButtonPanel1: TButtonPanel;
    List: TCheckListBox;
    OpenDlg: TOpenDialog;
    ToolBar1: TToolBar;
    bProp: TToolButton;
    bDel: TToolButton;
    bAdd: TToolButton;
    procedure bAddClick(Sender: TObject);
    procedure bDelClick(Sender: TObject);
    procedure bPropClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ListClickCheck(Sender: TObject);
  private
    { private declarations }
    procedure UpdateList;
  public
    FManager: TecSyntaxManager;
    FFontName: string;
    FFontSize: integer;
    FDirAcp: string;
    { public declarations }
  end;

var
  fmLexerLib: TfmLexerLib;

function DoShowDialogLexerLib(ALexerManager: TecSyntaxManager;
  const ADirAcp: string;
  const AFontName: string;
  AFontSize: integer): boolean;

implementation

{$R *.lfm}

function DoShowDialogLexerLib(ALexerManager: TecSyntaxManager;
  const ADirAcp: string; const AFontName: string; AFontSize: integer): boolean;
var
  F: TfmLexerLib;
begin
  F:= TfmLexerLib.Create(nil);
  try
    F.FManager:= ALexerManager;
    F.FFontName:= AFontName;
    F.FFontSize:= AFontSize;
    F.FDirAcp:= ADirAcp;
    F.ShowModal;
    Result:= F.FManager.Modified;
  finally
    F.Free;
  end;
end;

function IsLexerLinkDup(an: TecSyntAnalyzer; LinkN: integer): boolean;
var
  i: integer;
begin
  Result:= false;
  for i:= 0 to LinkN-1 do
    if an.SubAnalyzers[i].SyntAnalyzer=an.SubAnalyzers[LinkN].SyntAnalyzer then
    begin
      Result:= true;
      exit
    end;
end;

{ TfmLexerLib }

procedure TfmLexerLib.FormShow(Sender: TObject);
begin
  UpdateList;
  if List.Items.Count>0 then
    List.ItemIndex:= 0;
end;

procedure TfmLexerLib.ListClickCheck(Sender: TObject);
var
  an: TecSyntAnalyzer;
  n: integer;
begin
  n:= List.ItemIndex;
  if n<0 then exit;
  an:= List.Items.Objects[n] as TecSyntAnalyzer;

  an.Internal:= not List.Checked[n];
  FManager.Modified:= true;
end;

procedure TfmLexerLib.bPropClick(Sender: TObject);
var
  an: TecSyntAnalyzer;
  n: integer;
begin
  n:= List.ItemIndex;
  if n<0 then exit;
  an:= List.Items.Objects[n] as TecSyntAnalyzer;

  if DoShowDialogLexerProp(an, FFontName, FFontSize) then
  begin
    FManager.Modified:= true;
    UpdateList;
    List.ItemIndex:= n;
  end;
end;

procedure TfmLexerLib.bDelClick(Sender: TObject);
var
  an: TecSyntAnalyzer;
  n: integer;
begin
  n:= List.ItemIndex;
  if n<0 then exit;
  an:= List.Items.Objects[n] as TecSyntAnalyzer;

  if Application.MessageBox(
    PChar(Format('Delete lexer "%s"?', [an.LexerName])),
    PChar(Caption),
    MB_OKCANCEL or MB_ICONWARNING)=id_ok then
  begin
    an.Free;
    FManager.Modified:= true;
    UpdateList;
    List.ItemIndex:= Min(n, List.Count-1);
  end;
end;

procedure TfmLexerLib.bAddClick(Sender: TObject);
var
  msg: string;
begin
  OpenDlg.Filename:= '';
  if not OpenDlg.Execute then exit;
  if DoInstallLexerFromZip(OpenDlg.FileName, FManager, FDirAcp, msg) then
  begin
    UpdateList;
    Application.MessageBox(
      PChar('Installed:'#13+msg),
      PChar(Caption), MB_OK or MB_ICONINFORMATION);
  end;
end;

procedure TfmLexerLib.UpdateList;
var
  sl: tstringlist;
  an: TecSyntAnalyzer;
  an_sub: TecSubAnalyzerRule;
  links: string;
  i, j: integer;
begin
  List.Items.BeginUpdate;
  List.Items.Clear;

  sl:= tstringlist.create;
  try
    for i:= 0 to FManager.AnalyzerCount-1 do
    begin
      an:= FManager.Analyzers[i];
      sl.AddObject(an.LexerName, an);
    end;
    sl.sort;

    for i:= 0 to sl.count-1 do
    begin
      an:= sl.Objects[i] as TecSyntAnalyzer;

      links:= '';
      for j:= 0 to an.SubAnalyzers.Count-1 do
        if not IsLexerLinkDup(an, j) then
        begin
          if links='' then
            links:= 'links: '
          else
            links:= links+', ';
          an_sub:= an.SubAnalyzers[j];
          if an_sub<>nil then
            if an_sub.SyntAnalyzer<>nil then
              links:= links+an_sub.SyntAnalyzer.LexerName;
        end;
      if links<>'' then links:= '  ('+links+')';

      List.Items.AddObject(sl[i]+links, an);
      List.Checked[List.Count-1]:= not an.Internal;
    end;
  finally
    sl.free;
  end;
  List.Items.EndUpdate;
end;

end.

