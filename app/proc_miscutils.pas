(*
This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at http://mozilla.org/MPL/2.0/.

Copyright (c) Alexey Torgashin
*)
unit proc_miscutils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, ComCtrls, Graphics, ImgList,
  LclIntf, LclType, LazFileUtils, StrUtils,
  ATSynEdit,
  ATSynEdit_Export_HTML,
  ATStringProc,
  ecSyntAnal;

procedure LexerEnumSublexers(An: TecSyntAnalyzer; List: TStringList);
procedure LexerEnumStyles(An: TecSyntAnalyzer; List: TStringList);
procedure LexerSetSublexers(SyntaxManager: TecSyntaxManager; An: TecSyntAnalyzer; const Links: string);

type
  TAppTreeGoto = (treeGoNext, treeGoPrev, treeGoParent, treeGoNextBro, treeGoPrevBro);
procedure DoTreeviewJump(ATree: TTreeView; AMode: TAppTreeGoto);

procedure DoEditorExportToHTML_WithParams(Ed: TATSynEdit; AParams: string);

function ConvertTwoPointsToDiffPoint(APrevPnt, ANewPnt: TPoint): TPoint;
function ConvertShiftStateToString(const Shift: TShiftState): string;
function KeyboardStateToShiftState: TShiftState; //like VCL
function UpdateImagelistWithIconFromFile(AImagelist: TCustomImagelist; const AFilename: string): boolean;


implementation

procedure LexerEnumSublexers(An: TecSyntAnalyzer; List: TStringList);
var
  i: Integer;
  AnLink: TecSyntAnalyzer;
begin
  List.Clear;
  for i:= 0 to An.SubAnalyzers.Count-1 do
  begin
    AnLink:= An.SubAnalyzers[i].SyntAnalyzer;
    if AnLink<>nil then
      List.Add(AnLink.LexerName)
    else
      List.Add('');
  end;
end;

procedure LexerEnumStyles(An: TecSyntAnalyzer; List: TStringList);
var
  i: Integer;
begin
  List.Clear;
  for i:= 0 to An.Formats.Count-1 do
    List.Add(An.Formats[i].DisplayName);
end;

procedure LexerSetSublexers(SyntaxManager: TecSyntaxManager; An: TecSyntAnalyzer; const Links: string);
var
  S, SItem: string;
  Cnt: Integer;
begin
  S:= Links;
  Cnt:= 0;
  repeat
    SItem:= SGetItem(S, '|');
    if Cnt>=An.SubAnalyzers.Count then Break;
    An.SubAnalyzers[Cnt].SyntAnalyzer:= SyntaxManager.FindAnalyzer(SItem);
    Inc(Cnt);
  until false;
end;

procedure DoTreeviewJump(ATree: TTreeView; AMode: TAppTreeGoto);
var
  tn, tn2: TTreeNode;
begin
  with ATree do
    if Selected<>nil then
    begin
      case AMode of
        treeGoNext:
          tn:= Selected.GetNext;
        treeGoPrev:
          tn:= Selected.GetPrev;
        treeGoParent:
          tn:= Selected.Parent;
        treeGoNextBro:
          begin
            tn:= Selected.GetNextSibling;
            tn2:= Selected;
            if tn=nil then
              repeat
                tn2:= tn2.Parent;
                if tn2=nil then Break;
                tn:= tn2.GetNextSibling;
                if tn<>nil then Break;
              until false;
          end;
        treeGoPrevBro:
          begin
            tn:= Selected.GetPrevSibling;
            if tn=nil then
              tn:= Selected.Parent;
          end;
        else tn:= nil;
      end;
      if tn<>nil then
      begin
        Selected:= tn;
        ATree.OnDblClick(nil);
      end;
    end;
end;


function ConvertTwoPointsToDiffPoint(APrevPnt, ANewPnt: TPoint): TPoint;
begin
  if APrevPnt.Y=ANewPnt.Y then
  begin
    Result.Y:= 0;
    Result.X:= ANewPnt.X-APrevPnt.X;
  end
  else
  begin
    Result.Y:= ANewPnt.Y-APrevPnt.Y;
    Result.X:= ANewPnt.X;
  end;
end;

procedure DoEditorExportToHTML_WithParams(Ed: TATSynEdit; AParams: string);
var
  SFileName, STitle, SFontName: string;
  NFontSize: integer;
  bWithNums: boolean;
  NColorBg, NColorNums: TColor;
begin
  SFileName:= SGetItem(AParams, ';');
  STitle:= SGetItem(AParams, ';');
  SFontName:= SGetItem(AParams, ';');
  NFontSize:= StrToIntDef(SGetItem(AParams, ';'), 10);
  bWithNums:= StrToBoolDef(SGetItem(AParams, ';'), false);
  NColorBg:= StrToIntDef(SGetItem(AParams, ';'), clWhite);
  NColorNums:= StrToIntDef(SGetItem(AParams, ';'), clGray);

  DoEditorExportToHTML(Ed, SFileName, STitle, SFontName, NFontSize, bWithNums,
    NColorBg, NColorNums);
end;


function KeyboardStateToShiftState: TShiftState;
begin
  Result := [];
  if GetKeyState(VK_SHIFT) < 0 then Include(Result, ssShift);
  if GetKeyState(VK_CONTROL) < 0 then Include(Result, ssCtrl);
  if GetKeyState(VK_MENU) < 0 then Include(Result, ssAlt);
  if GetKeyState(VK_LWIN) < 0 then Include(Result, ssMeta);
end;

function ConvertShiftStateToString(const Shift: TShiftState): string;
begin
  Result:=
    IfThen(ssShift in Shift, 's')+
    IfThen(ssCtrl in Shift, 'c')+
    IfThen(ssAlt in Shift, 'a')+
    IfThen(ssMeta in Shift, 'm');
end;


function UpdateImagelistWithIconFromFile(AImagelist: TCustomImagelist;
  const AFilename: string): boolean;
var
  bmp: TBitmap;
begin
  if AImagelist=nil then exit(false);
  if not FileExistsUtf8(AFilename) then exit(false);

  bmp:= TBitmap.Create;
  try
    try
      bmp.LoadFromFile(AFilename);
      bmp.Transparent:= true;
      AImagelist.Add(bmp, nil);
      Result:= true;
    finally
      FreeAndNil(bmp);
    end;
  except
    Result:= false;
  end;
end;

end.


