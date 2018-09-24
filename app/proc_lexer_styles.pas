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
  LazFileUtils,
  at__jsonconf,
  ec_SyntAnal,
  proc_globdata,
  ATStringProc,
  ATStringProc_HtmlColor;

procedure DoSaveLexerStyleToFile_JsonTheme(st: TecSyntaxFormat; cfg: TJSONConfig; skey: string);
procedure DoSaveLexerStylesToFile_JsonLexerOps(an: TecSyntAnalyzer; const Filename: string);

procedure DoLoadLexerStyleFromFile(st: TecSyntaxFormat; ini: TIniFile; const section, skey: string);
procedure DoLoadLexerStylesFromFile(an: TecSyntAnalyzer; const Filename: string);
procedure DoLoadLexerStyleFromFile_JsonTheme(st: TecSyntaxFormat; cfg: TJSONConfig; skey: string);
procedure DoLoadLexerStylesFromFile_JsonLexerOps(an: TecSyntAnalyzer; const Filename: string; NoStyles: boolean);

function FontStylesToString(const f: TFontStyles): string;
function StringToFontStyles(const s: string): TFontStyles;


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

(*
function FormatFlagsToStr(const f: TecFormatFlags): string;
begin
  Result:= '';
  if ffBold in f then Result:= Result+'b';
  if ffItalic in f then Result:= Result+'i';
  if ffUnderline in f then Result:= Result+'u';
  if ffStrikeOut in f then Result:= Result+'s';
  //if ffReadOnly in f then Result:= Result+'r';
  //if ffHidden in f then Result:= Result+'h';
  //if ffFontName in f then Result:= Result+'N';
  //if ffFontSize in f then Result:= Result+'S';
  //if ffFontCharset in f then Result:= Result+'C';
  //if ffVertAlign in f then Result:= Result+'v';
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
      //'r': Include(Result, ffReadOnly);
      //'h': Include(Result, ffHidden);
      //'N': Include(Result, ffFontName);
      //'S': Include(Result, ffFontSize);
      //'C': Include(Result, ffFontCharset);
      //'v': Include(Result, ffVertAlign);
    end;
end;
*)

procedure DoSaveLexerStyleToFile_JsonTheme(st: TecSyntaxFormat; cfg: TJSONConfig;
  skey: string);
begin
  if not SEndsWith(skey, '/') then skey:= skey+'/';

  cfg.SetValue(skey+'Type', Integer(st.FormatType));
  cfg.SetValue(skey+'Styles', FontStylesToString(st.Font.Style));
  cfg.SetValue(skey+'CFont', SColorToHtmlColor(st.Font.Color));
  cfg.SetValue(skey+'CBack', SColorToHtmlColor(st.BgColor));
  cfg.SetValue(skey+'CBorder', SColorToHtmlColor(st.BorderColorBottom));

  cfg.SetValue(skey+'Border', Format('%d,%d,%d,%d', [
    Ord(st.BorderTypeLeft),
    Ord(st.BorderTypeRight),
    Ord(st.BorderTypeTop),
    Ord(st.BorderTypeBottom) ]));
end;


procedure DoSaveLexerStylesToFile_JsonLexerOps(an: TecSyntAnalyzer; const Filename: string);
var
  conf: TJSONConfig;
  an_orig: TecSyntAnalyzer;
  st, st_orig: TecSyntaxFormat;
  fn_lexer, path, val_orig: string;
  int_orig, i: integer;
begin
  conf:= TJSONConfig.Create(nil);
  an_orig:= TecSyntAnalyzer.Create(nil);
  try
    try
      fn_lexer:= GetAppLexerFilename(an.LexerName);
      if FileExistsUTF8(fn_lexer) then
        an_orig.LoadFromFile(fn_lexer);

      conf.Formatted:= true;
      conf.FileName:= Filename;

      conf.SetDeleteValue('/files', an.Extentions, an_orig.Extentions);

      for i:= 0 to an.Formats.Count-1 do
      begin
        st:= an.Formats[i];
        path:= '/style_'+StringReplace(st.DisplayName, '/', '_', [rfReplaceAll])+'/';

        if i<an_orig.Formats.Count then
          st_orig:= an_orig.Formats[i]
        else
          st_orig:= nil;

        if Assigned(st_orig) then
          val_orig:= ColorToString(st_orig.Font.Color)
        else
          val_orig:= '?';
        conf.SetDeleteValue(path+'font_color', ColorToString(st.Font.Color), val_orig);

        if Assigned(st_orig) then
          val_orig:= FontStylesToString(st_orig.Font.Style)
        else
          val_orig:= '?';
        conf.SetDeleteValue(path+'font_style', FontStylesToString(st.Font.Style), val_orig);

        if Assigned(st_orig) then
          val_orig:= ColorToString(st_orig.BgColor)
        else
          val_orig:= 'clNone';
        conf.SetDeleteValue(path+'back', ColorToString(st.BgColor), val_orig);

        if Assigned(st_orig) then
          val_orig:= ColorToString(st_orig.BorderColorLeft)
        else
          val_orig:= 'clBlack';
        conf.SetDeleteValue(path+'brd_c_l', ColorToString(st.BorderColorLeft), val_orig);

        if Assigned(st_orig) then
          val_orig:= ColorToString(st_orig.BorderColorRight)
        else
          val_orig:= 'clBlack';
        conf.SetDeleteValue(path+'brd_c_r', ColorToString(st.BorderColorRight), val_orig);

        if Assigned(st_orig) then
          val_orig:= ColorToString(st_orig.BorderColorTop)
        else
          val_orig:= 'clBlack';
        conf.SetDeleteValue(path+'brd_c_t', ColorToString(st.BorderColorTop), val_orig);

        if Assigned(st_orig) then
          val_orig:= ColorToString(st_orig.BorderColorBottom)
        else
          val_orig:= 'clBlack';
        conf.SetDeleteValue(path+'brd_c_b', ColorToString(st.BorderColorBottom), val_orig);

        if Assigned(st_orig) then
          int_orig:= Ord(st_orig.BorderTypeLeft)
        else
          int_orig:= 0;
        conf.SetDeleteValue(path+'brd_t_l', Ord(st.BorderTypeLeft), int_orig);

        if Assigned(st_orig) then
          int_orig:= Ord(st_orig.BorderTypeRight)
        else
          int_orig:= 0;
        conf.SetDeleteValue(path+'brd_t_r', Ord(st.BorderTypeRight), int_orig);

        if Assigned(st_orig) then
          int_orig:= Ord(st_orig.BorderTypeTop)
        else
          int_orig:= 0;
        conf.SetDeleteValue(path+'brd_t_t', Ord(st.BorderTypeTop), int_orig);

        if Assigned(st_orig) then
          int_orig:= Ord(st_orig.BorderTypeBottom)
        else
          int_orig:= 0;
        conf.SetDeleteValue(path+'brd_t_b', Ord(st.BorderTypeBottom), int_orig);
      end;
    except
    end;
  finally
    FreeAndNil(an_orig);
    FreeAndNil(conf);
  end;
end;


procedure DoLoadLexerStylesFromFile_JsonLexerOps(an: TecSyntAnalyzer; const Filename: string;
  NoStyles: boolean);
var
  conf: TJSONConfig;
  st: TecSyntaxFormat;
  path, s: string;
  i: integer;
begin
  conf:= TJSONConfig.Create(nil);
  try
    try
      conf.FileName:= Filename;

      an.Extentions:= conf.GetValue('/files', an.Extentions);
      if NoStyles then exit;

      for i:= 0 to an.Formats.Count-1 do
      begin
        st:= an.Formats[i];
        path:= '/style_'+StringReplace(st.DisplayName, '/', '_', [rfReplaceAll])+'/';

        s:= conf.GetValue(path+'font_color', '?');
        if s<>'?' then
          st.Font.Color:= StringToColor(s);

        s:= conf.GetValue(path+'font_style', '?');
        if s<>'?' then
          st.Font.Style:= StringToFontStyles(s);

        s:= conf.GetValue(path+'back', '?');
        if s<>'?' then
          st.BgColor:= StringToColor(s);

        s:= conf.GetValue(path+'brd_c_l', '');
        if s<>'' then
          st.BorderColorLeft:= StringToColor(s);

        s:= conf.GetValue(path+'brd_c_r', '');
        if s<>'' then
          st.BorderColorRight:= StringToColor(s);

        s:= conf.GetValue(path+'brd_c_t', '');
        if s<>'' then
          st.BorderColorTop:= StringToColor(s);

        s:= conf.GetValue(path+'brd_c_b', '');
        if s<>'' then
          st.BorderColorBottom:= StringToColor(s);

        st.BorderTypeLeft:= TecBorderLineType(conf.GetValue(path+'brd_t_l', 0));
        st.BorderTypeRight:= TecBorderLineType(conf.GetValue(path+'brd_t_r', 0));
        st.BorderTypeTop:= TecBorderLineType(conf.GetValue(path+'brd_t_t', 0));
        st.BorderTypeBottom:= TecBorderLineType(conf.GetValue(path+'brd_t_b', 0));
      end;
    except
    end;
  finally
    FreeAndNil(conf);
  end;
end;



procedure DoLoadLexerStyleFromFile_JsonTheme(st: TecSyntaxFormat; cfg: TJSONConfig;
  skey: string);
var
  Len: integer;
  s: string;
begin
  if not SEndsWith(skey, '/') then skey:= skey+'/';

  st.FormatType:= TecFormatType(cfg.GetValue(skey+'Type', Ord(st.FormatType)));
  st.Font.Style:= StringToFontStyles(cfg.GetValue(skey+'Styles', FontStylesToString(st.Font.Style)));
  st.Font.Color:= SHtmlColorToColor(cfg.GetValue(skey+'CFont', ''), Len, st.Font.Color);
  st.BgColor:= SHtmlColorToColor(cfg.GetValue(skey+'CBack', ''), Len, st.BgColor);
  st.BorderColorBottom:= SHtmlColorToColor(cfg.GetValue(skey+'CBorder', ''), Len, st.BorderColorBottom);
  st.BorderColorLeft:= st.BorderColorBottom;
  st.BorderColorRight:= st.BorderColorBottom;
  st.BorderColorTop:= st.BorderColorBottom;

  s:= cfg.GetValue(skey+'Border', '');
  if s<>'' then
  begin
    st.BorderTypeLeft:= TecBorderLineType(StrToIntDef(SGetItem(s), 0));
    st.BorderTypeRight:= TecBorderLineType(StrToIntDef(SGetItem(s), 0));
    st.BorderTypeTop:= TecBorderLineType(StrToIntDef(SGetItem(s), 0));
    st.BorderTypeBottom:= TecBorderLineType(StrToIntDef(SGetItem(s), 0));
  end;
end;


procedure DoLoadLexerStyleFromFile(st: TecSyntaxFormat; ini: TIniFile; const section, skey: string);
begin
  st.DisplayName:= AnsiDequotedStr(ini.ReadString(section, skey+'_Name', ''), '"');
  //st.Font.Name:= ini.ReadString(section, skey+'_FontName', '');
  //st.Font.Size:= ini.ReadInteger(section, skey+'_FontSize', 10);
  st.Font.Color:= StringToColor(ini.ReadString(section, skey+'_FontColor', ''));
  st.Font.Style:= StringToFontStyles(ini.ReadString(section, skey+'_FontStyles', ''));
  st.BgColor:= StringToColor(ini.ReadString(section, skey+'_BgColor', ''));

  st.BorderColorBottom:= StringToColor(ini.ReadString(section, skey+'_BorderColorBottom', ''));
  st.BorderColorLeft:= StringToColor(ini.ReadString(section, skey+'_BorderColorLeft', ''));
  st.BorderColorRight:= StringToColor(ini.ReadString(section, skey+'_BorderColorRight', ''));
  st.BorderColorTop:= StringToColor(ini.ReadString(section, skey+'_BorderColorTop', ''));

  st.BorderTypeBottom:= TecBorderLineType(ini.ReadInteger(section, skey+'_BorderTypeBottom', 0));
  st.BorderTypeLeft:= TecBorderLineType(ini.ReadInteger(section, skey+'_BorderTypeLeft', 0));
  st.BorderTypeRight:= TecBorderLineType(ini.ReadInteger(section, skey+'_BorderTypeRight', 0));
  st.BorderTypeTop:= TecBorderLineType(ini.ReadInteger(section, skey+'_BorderTypeTop', 0));

  //st.FormatFlags:= StrToFormatFlags(ini.ReadString(section, skey+'_FormatFlags', ''));
  st.FormatType:= TecFormatType(ini.ReadInteger(section, skey+'_FormatType', 0));
end;


procedure DoLoadLexerStylesFromFile(an: TecSyntAnalyzer; const Filename: string);
var
  ini: TIniFile;
  i, j:Integer;
  section: string;
  fm: TecSyntaxFormat;
begin
  if an=nil then Exit;
  section:= an.LexerName;
  fm:= TecSyntaxFormat.Create(nil);
  ini:= TIniFile.Create(Filename);
  try
    an.Extentions:= ini.ReadString(section, 'Ext', an.Extentions);
    for i:= 0 to ini.ReadInteger(section, 'Num', 0)-1 do
    begin
      DoLoadLexerStyleFromFile(fm, ini, section, IntToStr(i));
      //Apply fm to matched style
      for j:= 0 to an.Formats.Count-1 do
        if an.Formats[j].DisplayName = fm.DisplayName then
        begin
          an.Formats[j].Assign(fm);
          Break
        end;
    end;
  finally
    ini.Free;
    fm.Free;
  end;
end;


end.
