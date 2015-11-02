(*
This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at http://mozilla.org/MPL/2.0/.

Copyright (c) Alexey Torgashin
*)
unit proc_lexer_styles;

interface

uses
  SysUtils, Classes, Graphics, IniFiles,
  ecSyntAnal,
  ATStringProc;

procedure SaveLexerStylesToFile(f: TecSyntAnalyzer; const fn: string);
procedure LoadLexerStylesFromFile(f: TecSyntAnalyzer; const fn: string);

implementation

function FontStylesToString(const f: TFontStyles): string;
begin
  Result:= '';
  if fsBold in f then Result:= Result+'b';
  if fsItalic in f then Result:= Result+'i';
  if fsUnderline in f then Result:= Result+'u';
  if fsStrikeout in f then Result:= Result+'s';
end;

function StringToFontStyles(const s: string): TFontStyles;
var
  i: Integer;
begin
  Result:= [];
  for i:= 1 to Length(s) do
    case s[i] of
      'b': Include(Result, fsBold);
      'i': Include(Result, fsItalic);
      'u': Include(Result, fsUnderline);
      's': Include(Result, fsStrikeout);
    end;
end;


function FormatFlagsToStr(const f: TecFormatFlags): string;
begin
  Result:= '';
  if ffBold in f then Result:= Result+'b';
  if ffItalic in f then Result:= Result+'i';
  if ffUnderline in f then Result:= Result+'u';
  if ffStrikeOut in f then Result:= Result+'s';
  if ffReadOnly in f then Result:= Result+'r';
  if ffHidden in f then Result:= Result+'h';
  if ffFontName in f then Result:= Result+'N';
  if ffFontSize in f then Result:= Result+'S';
  if ffFontCharset in f then Result:= Result+'C';
  if ffVertAlign in f then Result:= Result+'v';
end;

function StrToFormatFlags(const s: string): TecFormatFlags;
var i:Integer;
begin
  Result:= [];
  for i:= 1 to Length(s) do
    case s[i] of
      'b': Include(Result, ffBold);
      'i': Include(Result, ffItalic);
      'u': Include(Result, ffUnderline);
      's': Include(Result, ffStrikeOut);
      'r': Include(Result, ffReadOnly);
      'h': Include(Result, ffHidden);
      'N': Include(Result, ffFontName);
      'S': Include(Result, ffFontSize);
      'C': Include(Result, ffFontCharset);
      'v': Include(Result, ffVertAlign);
    end;
end;

procedure SaveLexerStylesToFile(f: TecSyntAnalyzer; const fn: string);
var
  i: Integer;
  s, si: string;
begin
  s:= f.LexerName;
  if s='' then Exit;
  with TIniFile.Create(fn) do
  try
    EraseSection(s);
    WriteString(s, 'Ext', f.Extentions);
    WriteInteger(s, 'Num', f.Formats.Count);
    for i:= 0 to f.Formats.Count-1 do
     with f.Formats[i] do
     begin
      si:= IntToStr(i);
      WriteString(s, si+'_Name', '"'+DisplayName+'"');
      WriteString(s, si+'_FontName', Font.Name);
      WriteInteger(s, si+'_FontSize', Font.Size);
      WriteString(s, si+'_FontColor', ColorToString(Font.Color));
      WriteString(s, si+'_FontStyles', FontStylesToString(Font.Style));
      WriteString(s, si+'_BgColor', ColorToString(BgColor));

      WriteString(s, si+'_BorderColorBottom', ColorToString(BorderColorBottom));
      WriteString(s, si+'_BorderColorLeft', ColorToString(BorderColorLeft));
      WriteString(s, si+'_BorderColorRight', ColorToString(BorderColorRight));
      WriteString(s, si+'_BorderColorTop', ColorToString(BorderColorTop));

      WriteInteger(s, si+'_BorderTypeBottom', Integer(BorderTypeBottom));
      WriteInteger(s, si+'_BorderTypeLeft', Integer(BorderTypeLeft));
      WriteInteger(s, si+'_BorderTypeRight', Integer(BorderTypeRight));
      WriteInteger(s, si+'_BorderTypeTop', Integer(BorderTypeTop));

      WriteInteger(s, si+'_ChangeCase', Integer(ChangeCase));
      WriteString(s, si+'_FormatFlags', FormatFlagsToStr(FormatFlags));
      WriteInteger(s, si+'_FormatType', Integer(FormatType));
      WriteBool(s, si+'_Hidden', Hidden);
      WriteBool(s, si+'_MultiLineBorder', MultiLineBorder);
      WriteBool(s, si+'_ReadOnly', ReadOnly);
      WriteInteger(s, si+'_VertAlignment', Integer(VertAlignment));
    end;
  finally
    Free
  end;
end;


procedure LoadLexerStylesFromFile(f: TecSyntAnalyzer; const fn: string);
var
  i, j:Integer;
  s, si: string;
  fm: TecSyntaxFormat;
begin
  if f=nil then Exit;
  s:= f.LexerName;
  fm:= TecSyntaxFormat.Create(nil);
  with TIniFile.Create(fn) do
  try
    f.Extentions:= ReadString(s, 'Ext', f.Extentions);
    for i:= 0 to ReadInteger(s, 'Num', 0)-1 do
    begin
      si:= IntToStr(i);
      fm.DisplayName:= ReadString(s, si+'_Name', '');
      fm.Font.Name:= ReadString(s, si+'_FontName', '');
      fm.Font.Size:= ReadInteger(s, si+'_FontSize', 10);
      fm.Font.Color:= StringToColor(ReadString(s, si+'_FontColor', ''));
      fm.Font.Style:= StringToFontStyles(ReadString(s, si+'_FontStyles', ''));
      fm.BgColor:= StringToColor(ReadString(s, si+'_BgColor', ''));

      fm.BorderColorBottom:= StringToColor(ReadString(s, si+'_BorderColorBottom', ''));
      fm.BorderColorLeft:= StringToColor(ReadString(s, si+'_BorderColorLeft', ''));
      fm.BorderColorRight:= StringToColor(ReadString(s, si+'_BorderColorRight', ''));
      fm.BorderColorTop:= StringToColor(ReadString(s, si+'_BorderColorTop', ''));

      fm.BorderTypeBottom:= TecBorderLineType(ReadInteger(s, si+'_BorderTypeBottom', 0));
      fm.BorderTypeLeft:= TecBorderLineType(ReadInteger(s, si+'_BorderTypeLeft', 0));
      fm.BorderTypeRight:= TecBorderLineType(ReadInteger(s, si+'_BorderTypeRight', 0));
      fm.BorderTypeTop:= TecBorderLineType(ReadInteger(s, si+'_BorderTypeTop', 0));

      fm.ChangeCase:= TecChangeCase(ReadInteger(s, si+'_ChangeCase', 0));
      fm.FormatFlags:= StrToFormatFlags(ReadString(s, si+'_FormatFlags', ''));
      fm.FormatType:= TecFormatType(ReadInteger(s, si+'_FormatType', 0));
      fm.Hidden:= ReadBool(s, si+'_Hidden', false);
      fm.MultiLineBorder:= ReadBool(s, si+'_MultiLineBorder', false);
      fm.ReadOnly:= ReadBool(s, si+'_ReadOnly', false);
      fm.VertAlignment:= TecVertAlignment(ReadInteger(s, si+'_VertAlignment', 0));

      //Apply fm to matched style
      for j:= 0 to f.Formats.Count-1 do
        if f.Formats[j].DisplayName = fm.DisplayName then
        begin
          f.Formats[j].Assign(fm);
          Break
        end;
    end;
  finally
    Free;
    fm.Free;
  end;
end;


end.
