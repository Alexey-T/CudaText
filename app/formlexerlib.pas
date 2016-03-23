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
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ButtonPanel,
  StdCtrls, ComCtrls, CheckLst, IniFiles,
  LCLIntf, LCLType, LCLProc,
  LazUTF8, LazFileUtils,
  ecSyntAnal,
  formlexerprop,
  proc_globdata,
  proc_msg,
  math;

type
  { TfmLexerLib }

  TfmLexerLib = class(TForm)
    ButtonPanel1: TButtonPanel;
    List: TCheckListBox;
    ToolBar1: TToolBar;
    bProp: TToolButton;
    bDel: TToolButton;
    procedure bDelClick(Sender: TObject);
    procedure bPropClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ListClickCheck(Sender: TObject);
  private
    { private declarations }
    procedure UpdateList;
  public
    FFontName: string;
    FFontSize: integer;
    FDirAcp: string;
    FStylesFilename: string;
    { public declarations }
  end;

var
  fmLexerLib: TfmLexerLib;

function DoShowDialogLexerLib(
  const ADirAcp: string;
  const AFontName: string;
  AFontSize: integer;
  const AStylesFilename: string): boolean;

implementation

{$R *.lfm}

procedure DoLocalize_FormLexerLib(F: TfmLexerLib);
const
  section = 'd_lex_lib';
var
  ini: TIniFile;
  fn: string;
begin
  fn:= GetAppLangFilename;
  if not FileExists(fn) then exit;
  ini:= TIniFile.Create(fn);
  try
    with F do Caption:= ini.ReadString(section, '_', Caption);
    with F.ButtonPanel1.CloseButton do Caption:= msgButtonClose;
    with F.bProp do Caption:= ini.ReadString(section, 'cfg', Caption);
    with F.bDel do Caption:= ini.ReadString(section, 'del', Caption);
  finally
    FreeAndNil(ini);
  end;
end;


function DoShowDialogLexerLib(const ADirAcp: string; const AFontName: string;
  AFontSize: integer; const AStylesFilename: string): boolean;
var
  F: TfmLexerLib;
begin
  F:= TfmLexerLib.Create(nil);
  try
    DoLocalize_FormLexerLib(F);
    F.FFontName:= AFontName;
    F.FFontSize:= AFontSize;
    F.FDirAcp:= ADirAcp;
    F.FStylesFilename:= AStylesFilename;
    F.ShowModal;
    Result:= AppManager.Modified;
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
  AppManager.Modified:= true;

  DoLexerExportFromLibToFile(an);
end;

procedure TfmLexerLib.bPropClick(Sender: TObject);
var
  an: TecSyntAnalyzer;
  n: integer;
begin
  n:= List.ItemIndex;
  if n<0 then exit;
  an:= List.Items.Objects[n] as TecSyntAnalyzer;

  if DoShowDialogLexerProp(an, FFontName, FFontSize, FStylesFilename) then
  begin
    DoLexerExportFromLibToFile(an);
    UpdateList;
    List.ItemIndex:= n;
  end;
end;

procedure TfmLexerLib.FormCreate(Sender: TObject);
begin
end;

procedure TfmLexerLib.bDelClick(Sender: TObject);
var
  an: TecSyntAnalyzer;
  n: integer;
begin
  n:= List.ItemIndex;
  if n<0 then exit;
  an:= List.Items.Objects[n] as TecSyntAnalyzer;

  if MsgBox(
    Format(msgConfirmDeleteLexer, [an.LexerName]),
    MB_OKCANCEL or MB_ICONWARNING)=ID_OK then
  begin
    DeleteFile(GetAppLexerFilename(an.LexerName));
    an.Free;
    UpdateList;
    List.ItemIndex:= Min(n, List.Count-1);
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
    for i:= 0 to AppManager.AnalyzerCount-1 do
    begin
      an:= AppManager.Analyzers[i];
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

