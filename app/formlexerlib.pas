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
  StdCtrls, ComCtrls, IniFiles,
  LCLIntf, LCLType, LCLProc, ExtCtrls,
  LazUTF8, LazFileUtils,
  ec_SyntAnal,
  formlexerprop,
  proc_globdata,
  proc_msg,
  math;

type
  { TfmLexerLib }

  TfmLexerLib = class(TForm)
    btnConfig: TButton;
    btnDelete: TButton;
    btnShowHide: TButton;
    List: TListBox;
    PanelBtn: TButtonPanel;
    PanelTop: TPanel;
    procedure btnDeleteClick(Sender: TObject);
    procedure btnConfigClick(Sender: TObject);
    procedure btnShowHideClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
    FOnDeleteLexer: TStrEvent;
    procedure UpdateList;
  public
    { public declarations }
    FFontName: string;
    FFontSize: integer;
    FDirAcp: string;
    FStylesFilename: string;
    property OnDeleteLexer: TStrEvent read FOnDeleteLexer write FOnDeleteLexer;
  end;

var
  fmLexerLib: TfmLexerLib;

function DoShowDialogLexerLib(
  const ADirAcp: string;
  const AFontName: string;
  AFontSize: integer;
  const AStylesFilename: string;
  AOnDeleteLexer: TStrEvent): boolean;

implementation

{$R *.lfm}

const
  cHiddenSuffix: string = '(hidden)';
  cLexerLinks: string = 'links:';

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
    with F.PanelBtn.CloseButton do Caption:= msgButtonClose;
    with F.btnConfig do Caption:= ini.ReadString(section, 'cfg', Caption);
    with F.btnDelete do Caption:= ini.ReadString(section, 'del', Caption);
    with F.btnShowHide do Caption:= ini.ReadString(section, 'hid', Caption);
    cHiddenSuffix:= ini.ReadString(section, 'hidmk', cHiddenSuffix);
    cLexerLinks:= ini.ReadString(section, 'lns', cLexerLinks);
  finally
    FreeAndNil(ini);
  end;
end;


function DoShowDialogLexerLib(const ADirAcp: string; const AFontName: string;
  AFontSize: integer; const AStylesFilename: string; AOnDeleteLexer: TStrEvent): boolean;
var
  F: TfmLexerLib;
begin
  F:= TfmLexerLib.Create(nil);
  try
    DoLocalize_FormLexerLib(F);
    F.OnDeleteLexer:= AOnDeleteLexer;
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
  UpdateFormOnTop(Self);
  UpdateList;
  if List.Items.Count>0 then
    List.ItemIndex:= 0;
end;


procedure TfmLexerLib.btnConfigClick(Sender: TObject);
var
  an: TecSyntAnalyzer;
  n: integer;
begin
  List.SetFocus;

  n:= List.ItemIndex;
  if n<0 then exit;
  an:= List.Items.Objects[n] as TecSyntAnalyzer;

  if DoShowDialogLexerProp(an, FFontName, FFontSize) then
  begin
    DoLexerExportFromLibToFile(an);
    UpdateList;
    List.ItemIndex:= n;
  end;
end;

procedure TfmLexerLib.btnShowHideClick(Sender: TObject);
var
  an: TecSyntAnalyzer;
  n: integer;
begin
  List.SetFocus;

  n:= List.ItemIndex;
  if n<0 then exit;
  an:= List.Items.Objects[n] as TecSyntAnalyzer;

  an.Internal:= not an.Internal;
  AppManager.Modified:= true;
  UpdateList;

  DoLexerExportFromLibToFile(an);
end;

procedure TfmLexerLib.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key=VK_DELETE) and (Shift=[]) then
  begin
    btnDelete.Click;
    Key:= 0;
    exit
  end;

  if (Key=VK_RETURN) and (Shift=[]) then
  begin
    btnConfig.Click;
    Key:= 0;
    exit
  end;

  if (Key=VK_SPACE) and (Shift=[]) then
  begin
    btnShowHide.Click;
    Key:= 0;
    exit
  end;

  if (Key=VK_ESCAPE) and (Shift=[]) then
  begin
    Close;
    Key:= 0;
    exit
  end;
end;


procedure TfmLexerLib.btnDeleteClick(Sender: TObject);
var
  an: TecSyntAnalyzer;
  NIndex: integer;
begin
  List.SetFocus;

  NIndex:= List.ItemIndex;
  if NIndex<0 then exit;
  an:= List.Items.Objects[NIndex] as TecSyntAnalyzer;

  if MsgBox(
    Format(msgConfirmDeleteLexer, [an.LexerName]),
    MB_OKCANCEL or MB_ICONWARNING)=ID_OK then
  begin
    if Assigned(FOnDeleteLexer) then
      FOnDeleteLexer(nil, an.LexerName);

    DeleteFile(GetAppLexerFilename(an.LexerName));
    DeleteFile(GetAppLexerMapFilename(an.LexerName));
    DeleteFile(GetAppLexerAcpFilename(an.LexerName));

    AppManager.DeleteLexer(an);
    UpdateList;
    List.ItemIndex:= Min(NIndex, List.Count-1);
  end;
end;

procedure TfmLexerLib.UpdateList;
var
  sl: tstringlist;
  an: TecSyntAnalyzer;
  an_sub: TecSubAnalyzerRule;
  links, suffix: string;
  PrevIndex, i, j: integer;
begin
  PrevIndex:= List.ItemIndex;
  List.Items.BeginUpdate;
  List.Items.Clear;

  sl:= tstringlist.create;
  try
    for i:= 0 to AppManager.LexerCount-1 do
    begin
      an:= AppManager.Lexers[i];
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
            links:= cLexerLinks+' '
          else
            links:= links+', ';
          an_sub:= an.SubAnalyzers[j];
          if an_sub<>nil then
            if an_sub.SyntAnalyzer<>nil then
              links:= links+an_sub.SyntAnalyzer.LexerName;
        end;
      if links<>'' then links:= '  ('+links+')';

      suffix:= '';
      if an.Internal then
        suffix:= '    '+cHiddenSuffix;

      List.Items.AddObject(sl[i] + links + suffix, an);
    end;
  finally
    sl.free;
  end;

  List.Items.EndUpdate;
  if (PrevIndex>=0) and (PrevIndex<List.Count) then
    List.ItemIndex:= PrevIndex;
end;

end.

