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
  Dialogs, Forms,
  LclIntf, LclType, LazFileUtils, StrUtils,
  ATSynEdit,
  ATSynEdit_Export_HTML,
  ATStringProc,
  ATButtons,
  ecSyntAnal,
  proc_py_const,
  proc_colors;

function Canvas_NumberToFontStyles(Num: integer): TFontStyles;
procedure Canvas_PaintPolygonFromSting(C: TCanvas; Str: string);
function Canvas_PaintImage(C: TCanvas; const AFilename: string; ARect: TRect; AResize: boolean): boolean;
function DoPictureLoadFromFile(const AFilename: string): TGraphic;

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
function FormatFileDateAsNiceString(const AFilename: string): string;
procedure DoForm_SetPropertyFromPair(F: TForm; const AName, AValue: string);


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
  bmp: TCustomBitmap;
begin
  Result:= false;
  if AImagelist=nil then exit;
  if not FileExistsUtf8(AFilename) then exit;

  if ExtractFileExt(AFilename)='.bmp' then
    bmp:= TBitmap.Create
  else
  if ExtractFileExt(AFilename)='.png' then
    bmp:= TPortableNetworkGraphic.Create
  else
    exit;

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
  end;
end;


function Canvas_NumberToFontStyles(Num: integer): TFontStyles;
begin
  Result:= [];
  if (Num and FONT_B)<>0 then Include(Result, fsBold);
  if (Num and FONT_I)<>0 then Include(Result, fsItalic);
  if (Num and FONT_U)<>0 then Include(Result, fsUnderline);
  if (Num and FONT_S)<>0 then Include(Result, fsStrikeOut);
end;

procedure Canvas_PaintPolygonFromSting(C: TCanvas; Str: string);
var
  S1, S2: string;
  P: TPoint;
  Pnt: array of TPoint;
begin
  SetLength(Pnt, 0);
  repeat
    S1:= SGetItem(Str, ',');
    S2:= SGetItem(Str, ',');
    if (S1='') or (S2='') then Break;
    P.X:= StrToIntDef(S1, MaxInt);
    P.Y:= StrToIntDef(S2, MaxInt);
    if (P.X=MaxInt) or (P.Y=MaxInt) then Exit;
    SetLength(Pnt, Length(Pnt)+1);
    Pnt[Length(Pnt)-1]:= P;
  until false;

  if Length(Pnt)>2 then
    C.Polygon(Pnt);
end;


function DoPictureLoadFromFile(const AFilename: string): TGraphic;
var
  ext: string;
begin
  Result:= nil;
  if not FileExistsUTF8(AFilename) then exit;
  ext:= LowerCase(ExtractFileExt(AFilename));

  if ext='.png' then
    Result:= TPortableNetworkGraphic.Create
  else
  if ext='.gif' then
    Result:= TGIFImage.Create
  else
  if ext='.bmp' then
    Result:= TBitmap.Create
  else
  if SBeginsWith(ext, '.j') then //jpg, jpeg, jpe, jfif
    Result:= TJPEGImage.Create
  else
    exit;

  try
    Result.LoadFromFile(AFilename);
    Result.Transparent:= true;
  except
    FreeAndNil(Result);
  end;
end;

function Canvas_PaintImage(C: TCanvas; const AFilename: string; ARect: TRect; AResize: boolean): boolean;
var
  Pic: TGraphic;
  Bitmap: TBitmap;
begin
  Result:= false;
  Pic:= DoPictureLoadFromFile(AFilename);
  if Pic=nil then exit;

  try
    try
      if AResize then
      begin
        Bitmap:= TBitmap.Create;
        try
          Bitmap.PixelFormat:= pf24bit;
          Bitmap.SetSize(Pic.Width, Pic.Height);
          Bitmap.Canvas.Brush.Color:= clWhite;
          Bitmap.Canvas.FillRect(0, 0, Bitmap.Width, Bitmap.Height);
          Bitmap.Canvas.Draw(0, 0, Pic);
          C.AntialiasingMode:= amOn;
          C.StretchDraw(ARect, Bitmap);
        finally
          FreeAndNil(Bitmap);
        end;
      end
      else
        C.Draw(ARect.Left, ARect.Top, Pic);
      Result:= true;
    finally
      FreeAndNil(Pic);
    end;
  except
  end;
end;


function ConvertDateTimeToNiceString(const ADate: TDateTime): string;
var
  DTime: TDateTime;
  NHour, NMinute, NSec, NMilSec: word;
begin
  //fix result: make millisec=0, make seconds even int
  DecodeTime(ADate, NHour, NMinute, NSec, NMilSec);
  NMilSec:= 0;
  NSec:= NSec div 2 * 2;
  DTime:= EncodeTime(NHour, NMinute, NSec, NMilSec);
  Result:= FormatDateTime('yyyy-mm-dd_hh-nn-ss', ComposeDateTime(ADate, DTime));
end;

function FormatFileDateAsNiceString(const AFilename: string): string;
var
  D: TDateTime;
begin
  D:= FileDateToDateTime(FileAgeUTF8(AFilename));
  Result:= ConvertDateTimeToNiceString(D);
end;

procedure DoForm_SetPropertyFromPair(F: TForm; const AName, AValue: string);
begin
  if AName='cap' then
    F.Caption:= AValue
  else
  if AName='x' then
    F.Left:= StrToIntDef(AValue, F.Left)
  else
  if AName='y' then
    F.Top:= StrToIntDef(AValue, F.Top)
  else
  if AName='w' then
    F.Width:= StrToIntDef(AValue, F.Width)
  else
  if AName='h' then
    F.Height:= StrToIntDef(AValue, F.Height)
  else
  exit;
end;


end.


