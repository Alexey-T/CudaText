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
  Classes, SysUtils, Controls, StdCtrls, ComCtrls, Graphics,
  ImgList, Dialogs, Forms, Menus,
  LclIntf, LclType, LazFileUtils, StrUtils,
  ATSynEdit,
  ATSynEdit_Adapter_EControl,
  ATSynEdit_Export_HTML,
  ATStringProc,
  ATListbox,
  ATPanelSimple,
  ATButtons,
  ATFlatToolbar,
  ATBinHex,
  ec_LexerList,
  ec_SyntAnal,
  proc_globdata,
  proc_py_const,
  proc_colors;

function Canvas_NumberToFontStyles(Num: integer): TFontStyles;
procedure Canvas_PaintPolygonFromSting(C: TCanvas; Str: string);
procedure Canvas_PaintImageInRect(C: TCanvas; APic: TGraphic; const ARect: TRect);
function DoPictureLoadFromFile(const AFilename: string): TGraphic;
procedure DoScalePanelControls(APanel: TWinControl);

procedure LexerEnumSublexers(An: TecSyntAnalyzer; List: TStringList);
procedure LexerEnumStyles(An: TecSyntAnalyzer; List: TStringList);
function LexerFilenameToComponentName(const fn: string): string;

type
  TAppTreeGoto = (
    treeGoNext,
    treeGoPrev,
    treeGoParent,
    treeGoNextBro,
    treeGoPrevBro
    );

procedure DoTreeviewJump(ATree: TTreeView; AMode: TAppTreeGoto);
procedure DoTreeviewFoldLevel(ATree: TTreeView; ALevel: integer);
procedure DoTreeviewCopy(Src, Dst: TTreeView);

procedure DoApplyThemeToTreeview(C: TTreeview; AThemed, AChangeShowRoot: boolean);
procedure DoApplyThemeToListbox(C: ATListbox.TATListbox);
procedure DoApplyThemeToToolbar(C: TATFlatToolbar);

function ConvertTwoPointsToDiffPoint(APrevPnt, ANewPnt: TPoint): TPoint;
function ConvertShiftStateToString(const Shift: TShiftState): string;
function KeyboardStateToShiftState: TShiftState; //like VCL
function UpdateImagelistWithIconFromFile(AImagelist: TCustomImagelist;
         const AFilename: string; const AIndex: integer = -1): integer;
function FormatFileDateAsNiceString(const AFilename: string): string;

function AppStrToBool(const S: string): boolean; inline;
function AppStringToAlignment(const S: string): TAlignment;
function AppAlignmentToString(const V: TAlignment): string;

function ViewerGotoFromString(V: TATBinHex; SInput: string): boolean;
procedure ViewerApplyTheme(V: TATBinHex);
function ViewerModeString(V: TATBinHex): string;

function ExtractFileName_Fixed(const FileName: string): string;
function ExtractFileDir_Fixed(const FileName: string): string;

procedure DoPaintCheckers(C: TCanvas;
  ASizeX, ASizeY: integer;
  ACellSize: integer;
  AColor1, AColor2: TColor);

procedure Menu_Copy(ASrc, ADest: TMenu);


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
  const AFilename: string; const AIndex: integer = -1): integer;
var
  bmp: TCustomBitmap;
begin
  Result:= -1;
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

      if AIndex >= 0 then
      begin
        AImagelist.Replace(AIndex, bmp, nil);
        Result:= AIndex;
      end
      else
      begin
        AImagelist.Add(bmp, nil);
        Result:= AImageList.Count-1;
      end;
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


procedure Canvas_PaintImageInRect(C: TCanvas; APic: TGraphic; const ARect: TRect);
var
  Bitmap: TBitmap;
begin
  Bitmap:= TBitmap.Create;
  try
    Bitmap.PixelFormat:= pf24bit;
    Bitmap.SetSize(APic.Width, APic.Height);
    Bitmap.Canvas.Brush.Color:= clWhite;
    Bitmap.Canvas.FillRect(0, 0, Bitmap.Width, Bitmap.Height);
    Bitmap.Canvas.Draw(0, 0, APic);
    C.AntialiasingMode:= amOn;
    C.StretchDraw(ARect, Bitmap);
  finally
    FreeAndNil(Bitmap);
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


procedure DoApplyThemeToTreeview(C: TTreeview; AThemed, AChangeShowRoot: boolean);
var
  Op: TTreeViewOptions;
begin
  if AThemed then
  begin
    C.Font.Name:= UiOps.VarFontName;
    C.Font.Size:= UiOps.VarFontSize;
    C.Font.Color:= GetAppColor('TreeFont');
    C.BackgroundColor:= GetAppColor('TreeBg');
    C.SelectionFontColor:= GetAppColor('TreeSelFont'); //lew Laz
    C.SelectionFontColorUsed:= true; //new Laz
    C.SelectionColor:= GetAppColor('TreeSelBg');
    C.TreeLineColor:= GetAppColor('TreeLines');
    C.TreeLinePenStyle:= psSolid;
    C.ExpandSignColor:= GetAppColor('TreeSign');
  end;

  C.BorderStyle:= bsNone;
  C.ExpandSignType:= tvestArrowFill;

  Op:= [
    tvoAutoItemHeight,
    tvoKeepCollapsedNodes,
    tvoShowButtons,
    tvoRowSelect,
    tvoRightClickSelect,
    tvoReadOnly
    ];

  if AChangeShowRoot or C.ShowRoot then
    Include(Op, tvoShowRoot)
  else
    Exclude(Op, tvoShowRoot);

  if UiOps.TreeShowLines then
    Include(Op, tvoShowLines)
  else
    Exclude(Op, tvoShowLines);

  if UiOps.TreeShowTooltips then
    Include(Op, tvoToolTips)
  else
    Exclude(Op, tvoToolTips);

  C.Options:= Op;

  if AThemed then
    C.ScrollBars:= ssNone
  else
    C.ScrollBars:= ssVertical;

  C.Invalidate;
end;


procedure DoApplyThemeToListbox(C: ATListbox.TATListbox);
begin
  C.Font.Name:= UiOps.VarFontName;
  C.Font.Size:= UiOps.VarFontSize;
  C.Font.Color:= GetAppColor('ListFont');
  C.Color:= GetAppColor('ListBg');
  C.ColorSelFont:= GetAppColor('ListSelFont');
  C.ColorSelBack:= GetAppColor('ListSelBg');
  C.Invalidate;
end;

procedure DoApplyThemeToToolbar(C: TATFlatToolbar);
begin
  C.Color:= GetAppColor('TabBg');
  C.Invalidate;
end;


procedure DoScalePanelControls(APanel: TWinControl);
var
  Ctl: TControl;
  i: integer;
begin
  for i:= 0 to APanel.ControlCount-1 do
  begin
    Ctl:= APanel.Controls[i];

    if (Ctl is TATButton) or (Ctl is TATPanelSimple) then
    begin
      Ctl.Width:= MulDiv(Ctl.Width, UiOps.ScreenScale, 100);
      Ctl.Height:= MulDiv(Ctl.Height, UiOps.ScreenScale, 100);
    end;

    if Ctl is TATPanelSimple then
      DoScalePanelControls(Ctl as TATPanelSimple)
  end;
end;

function AppStrToBool(const S: string): boolean; inline;
begin
  Result:= S='1';
end;


procedure DoTreeviewFoldLevel(ATree: TTreeView; ALevel: integer);
var
  Node: TTreeNode;
  i: integer;
begin
  ATree.Items.BeginUpdate;
  ATree.FullExpand;
  try
    for i:= 0 to ATree.Items.Count-1 do
    begin
      Node:= ATree.Items[i];
      if Node.Level>=ALevel-1 then
        Node.Collapse(true);
    end;
  finally
    ATree.Items.EndUpdate;
  end;
end;


function ExtractFileName_Fixed(const FileName: string): string;
var
  EndSep: Set of Char;
  I: integer;
begin
  I := Length(FileName);
  EndSep:= AllowDirectorySeparators; //dont include ":", needed for NTFS streams
  while (I > 0) and not CharInSet(FileName[I],EndSep) do
    Dec(I);
  Result := Copy(FileName, I + 1, MaxInt);
end;

function ExtractFileDir_Fixed(const FileName: string): string;
var
  EndSep: Set of Char;
  I: integer;
begin
  I := Length(FileName);
  EndSep:= AllowDirectorySeparators; //dont include ":", for ntfs streams
  while (I > 0) and not CharInSet(FileName[I],EndSep) do
    Dec(I);
  if (I > 1) and CharInSet(FileName[I],AllowDirectorySeparators) and
     not CharInSet(FileName[I - 1],EndSep) then
    Dec(I);
  Result := Copy(FileName, 1, I);
end;


procedure DoTreeviewCopy(Src, Dst: TTreeView);
var
  SrcItem, DstItem: TTreeNode;
  R, R2: TATRangeInCodeTree;
  i: integer;
begin
  Dst.BeginUpdate;
  try
    Dst.Items.Assign(Src.Items);

    if Assigned(Src.Selected) then
      Dst.Selected:= Dst.Items[Src.Selected.AbsoluteIndex];

    for i:= 0 to Src.Items.Count-1 do
    begin
      SrcItem:= Src.Items[i];
      DstItem:= Dst.Items[i];

      DstItem.Expanded:= SrcItem.Expanded;

      //copying of Item.Data, must create new range
      if SrcItem.Data<>nil then
        if TObject(SrcItem.Data) is TATRangeInCodeTree then
        begin
          R:= TATRangeInCodeTree(SrcItem.Data);
          R2:= TATRangeInCodeTree.Create;
          R2.Assign(R);
          DstItem.Data:= R2;
        end;
    end;
  finally
    Dst.EndUpdate;
  end;
end;


function ViewerGotoFromString(V: TATBinHex; SInput: string): boolean;
var
  Num: Int64;
begin
  if SEndsWith(SInput, '%') then
  begin
    Num:= StrToIntDef(Copy(SInput, 1, Length(SInput)-1), -1);
    Num:= V.FileSize * Num div 100;
  end
  else
  begin
    Num:= StrToInt64Def('$'+SInput, -1);
  end;

  Result:= Num>=0;
  if Result then
    V.PosAt(Num);
end;

procedure ViewerApplyTheme(V: TATBinHex);
var
  St: TecSyntaxFormat;
begin
  V.Font.Name:= EditorOps.OpFontName;
  V.Font.Size:= EditorOps.OpFontSize;
  V.Font.Color:= GetAppColor('EdTextFont');
  V.FontGutter.Name:= EditorOps.OpFontName;
  V.FontGutter.Size:= EditorOps.OpFontSize;
  V.FontGutter.Color:= GetAppColor('EdGutterFont');
  V.Color:= GetAppColor('EdTextBg');
  V.TextColorGutter:= GetAppColor('EdGutterBg');
  V.TextColorURL:= GetAppColor('EdLinks');

  St:= GetAppStyleFromName('SectionBG1');
  V.TextColorHexBack:= St.BgColor;

  St:= GetAppStyleFromName('Id');
  V.TextColorHex:= St.Font.Color;

  St:= GetAppStyleFromName('Id1');
  V.TextColorHex2:= St.Font.Color;

  St:= GetAppStyleFromName('Pale1');
  V.TextColorLines:= St.Font.Color;
end;


function ViewerModeString(V: TATBinHex): string;
const
  cModes: array[TATBinHexMode] of string = (
    'text',
    'binary',
    'hex',
    'unicode',
    'unicode/hex'
    );
begin
  Result:= cModes[V.Mode];
end;

function AppStringToAlignment(const S: string): TAlignment;
begin
  case S of
    'L': Result:= taLeftJustify;
    'R': Result:= taRightJustify
     else Result:= taCenter;
  end;
end;

function AppAlignmentToString(const V: TAlignment): string;
begin
  case V of
    taLeftJustify: Result:= 'L';
    taRightJustify: Result:= 'R';
    else Result:= 'C';
  end;
end;

procedure DoPaintCheckers(C: TCanvas;
  ASizeX, ASizeY: integer;
  ACellSize: integer;
  AColor1, AColor2: TColor);
var
  i, j: integer;
begin
  c.Brush.Color:= AColor1;
  c.FillRect(0, 0, ASizeX, ASizeY);

  for i:= 0 to ASizeX div ACellSize + 1 do
    for j:= 0 to ASizeY div ACellSize + 1 do
      if odd(i) xor odd(j) then
      begin
        c.Brush.Color:= AColor2;
        c.FillRect(i*ACellSize, j*ACellSize, (i+1)*ACellSize, (j+1)*ACellSize);
      end;
end;

procedure MenuItem_Copy(ASrc, ADest: TMenuItem);
var
  mi: TMenuItem;
  i: integer;
begin
  ADest.Clear;
  ADest.Action:= ASrc.Action;
  ADest.AutoCheck:= ASrc.AutoCheck;
  ADest.Caption:= ASrc.Caption;
  ADest.Checked:= ASrc.Checked;
  ADest.Default:= ASrc.Default;
  ADest.Enabled:= ASrc.Enabled;
  ADest.Bitmap:= ASrc.Bitmap;
  ADest.GroupIndex:= ASrc.GroupIndex;
  ADest.GlyphShowMode:= ASrc.GlyphShowMode;
  ADest.HelpContext:= ASrc.HelpContext;
  ADest.Hint:= ASrc.Hint;
  ADest.ImageIndex:= ASrc.ImageIndex;
  ADest.RadioItem:= ASrc.RadioItem;
  ADest.RightJustify:= ASrc.RightJustify;
  ADest.ShortCut:= ASrc.ShortCut;
  ADest.ShortCutKey2:= ASrc.ShortCutKey2;
  ADest.ShowAlwaysCheckable:= ASrc.ShowAlwaysCheckable;
  ADest.SubMenuImages:= ASrc.SubMenuImages;
  ADest.SubMenuImagesWidth:= ASrc.SubMenuImagesWidth;
  ADest.Visible:= ASrc.Visible;
  ADest.OnClick:= ASrc.OnClick;
  ADest.OnDrawItem:= ASrc.OnDrawItem;
  ADest.OnMeasureItem:= ASrc.OnMeasureItem;
  ADest.Tag:= ASrc.Tag;

  for i:= 0 to ASrc.Count-1 do
  begin
    mi:= TMenuItem.Create(ASrc.Owner);
    MenuItem_Copy(ASrc.Items[i], mi);
    ADest.Add(mi);
  end;
end;

procedure Menu_Copy(ASrc, ADest: TMenu);
begin
  MenuItem_Copy(ASrc.Items, ADest.Items);
end;


function LexerFilenameToComponentName(const fn: string): string;
var
  i: integer;
begin
  Result:= ChangeFileExt(ExtractFileName(fn), '');
  for i:= 1 to Length(Result) do
    if Pos(Result[i], ' ~!@#$%^&*()[]-+=;:.,')>0 then
      Result[i]:= '_';
end;


end.


