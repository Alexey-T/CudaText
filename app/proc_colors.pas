(*
This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at http://mozilla.org/MPL/2.0/.

Copyright (c) Alexey Torgashin
*)
unit proc_colors;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Dialogs,
  LclProc, LclType,
  ATStringProc,
  ATStringProc_HtmlColor,
  ecSyntAnal,
  proc_msg,
  proc_globdata,
  proc_lexer_styles;

type
  TAppColor = record
    color: TColor;
    name, desc: string;
  end;
  TAppTheme = record
    Colors: array of TAppColor;
    Styles: TList;
  end;

var
  AppTheme: TAppTheme;

procedure DoInitTheme(var D: TAppTheme);
procedure DoLoadTheme(const fn: string; var D: TAppTheme; IsThemeUI: boolean);
procedure DoSaveTheme(const fn: string; const D: TAppTheme; IsThemeUI: boolean);
function GetAppColor(const name: string): TColor;
function GetAppStyleFromName(const SName: string): TecSyntaxFormat;


implementation

uses
  ATButtons,
  jsonConf;

procedure DoLoadTheme(const fn: string; var D: TAppTheme; IsThemeUI: boolean);
var
  c: TJsonConfig;
  //
  procedure DoVal(var Val: TColor; const id: string);
  var
    s: string;
    len: integer;
  begin
    s:= c.GetValue(id, '?');
    if s='?' then exit;
    if s='' then
      Val:= clNone
    else
      Val:= SHtmlColorToColor(s, len, Val);
  end;
  //
var
  st: TecSyntaxFormat;
  i: integer;
begin
  c:= TJsonConfig.Create(nil);
  try
    try
      c.Filename:= fn;
    except
      Showmessage(msgCannotReadConf+#13+fn);
      Exit
    end;

    if IsThemeUI then
    begin
      for i:= Low(D.Colors) to High(D.Colors) do
        DoVal(D.Colors[i].color, D.Colors[i].name);
    end
    else
    begin
      for i:= 0 to d.Styles.Count-1 do
      begin
        st:= TecSyntaxFormat(d.Styles[i]);
        DoLoadLexerStyleFromFile(st, c, 'Lex_'+st.DisplayName);
      end;
    end;
  finally
    c.Free;
  end;
end;


procedure DoInitTheme(var D: TAppTheme);
  //
  procedure Add(color: TColor; const name, desc: string);
  begin
    SetLength(D.Colors, Length(D.Colors)+1);
    D.Colors[High(D.Colors)].color:= color;
    D.Colors[High(D.Colors)].name:= name;
    D.Colors[High(D.Colors)].desc:= desc;
  end;
  //
  procedure AddStyle(const SName: string;
    NColorFont, NColorBg, NColorBorder: TColor;
    NFontStyle: TFontStyles;
    NBorderLeft, NBorderRight, NBorderUp, NBorderDown: TecBorderLineType;
    NFormatType: TecFormatType);
  var
    st: TecSyntaxFormat;
  begin
    st:= TecSyntaxFormat.Create(nil);
    st.DisplayName:= SName;
    st.Font.Color:= NColorFont;
    st.Font.Style:= NFontStyle;
    st.BgColor:= NColorBg;
    st.BorderColorLeft:= NColorBorder;
    st.BorderColorRight:= NColorBorder;
    st.BorderColorTop:= NColorBorder;
    st.BorderColorBottom:= NColorBorder;
    st.BorderTypeLeft:= NBorderLeft;
    st.BorderTypeRight:= NBorderRight;
    st.BorderTypeTop:= NBorderUp;
    st.BorderTypeBottom:= NBorderDown;
    st.FormatType:= NFormatType;

    D.Styles.Add(st);
  end;
  //
const
  nColorText = $202020;
  nColorBack = $e4e4e4;
  nColorBack2 = $c8c8c8;
  nColorGutterBack = $d8d8d8;
  nColorGutterFont = $909090;
  nColorArrow = $969696;
  nColorBorder = $c0c0c0;
  nColorListBack = $d0d0d0;
  nColorListSelBack = $c0c0c0;
begin
  SetLength(D.Colors, 0);

  if Assigned(D.Styles) then
    D.Styles.Clear
  else
    D.Styles:= TList.Create;

  //add colors
  Add(nColorText, 'EdTextFont', 'editor, font');
  Add(nColorBack, 'EdTextBg', 'editor, BG');
  Add($e0e0e0, 'EdSelFont', 'editor, selection, font');
  Add($b0a0a0, 'EdSelBg', 'editor, selection, BG');
  Add(clGray, 'EdDisableFont', 'editor, disabled state, font');
  Add(nColorGutterBack, 'EdDisableBg', 'editor, disabled state, BG');
  Add($c05050, 'EdLinks', 'editor, links');
  Add(nColorGutterBack, 'EdLockedBg', 'editor, locked state, BG');
  Add(clBlack, 'EdCaret', 'editor, caret');
  Add($6060d0, 'EdMarkers', 'editor, markers');
  Add($eaf0f0, 'EdCurLineBg', 'editor, current line BG');
  Add(clMedGray, 'EdIndentVLine', 'editor, wrapped line indent vert-lines');
  Add($a0a0b8, 'EdUnprintFont', 'editor, unprinted chars, font');
  Add($e0e0e0, 'EdUnprintBg', 'editor, unprinted chars, BG');
  Add(clMedGray, 'EdUnprintHexFont', 'editor, special hex codes, font');
  Add(clLtGray, 'EdMinimapBorder', 'editor, minimap, border');
  Add($eeeeee, 'EdMinimapSelBg', 'editor, minimap, view BG');
  Add($e0e0e0, 'EdMicromapBg', 'editor, micromap, BG');
  Add($c0c0c0, 'EdMicromapViewBg', 'editor, micromap, current view area');
  Add($c05050, 'EdMicromapOccur', 'editor, micromap, word occurrences');
  Add($6060d0, 'EdMicromapSpell', 'editor, micromap, misspelled marks');
  Add($70b0b0, 'EdStateChanged', 'editor, line states, changed');
  Add($80a080, 'EdStateAdded', 'editor, line states, added');
  Add(clMedGray, 'EdStateSaved', 'editor, line states, saved');
  Add($b0b0b0, 'EdBlockStaple', 'editor, block staples (indent guides)');
  Add(nColorArrow, 'EdComboArrow', 'editor, combobox arrow-down');
  Add(nColorBack, 'EdComboArrowBg', 'editor, combobox arrow-down BG');
  Add(nColorBorder, 'EdBorder', 'editor, combobox border');
  Add(clMedGray, 'EdBlockSepLine', 'editor, separator line');
  Add($a06060, 'EdFoldMarkLine', 'editor, folded line');
  Add($e08080, 'EdFoldMarkFont', 'editor, folded block mark, font');
  Add($e08080, 'EdFoldMarkBorder', 'editor, folded block mark, border');
  Add(nColorBack, 'EdFoldMarkBg', 'editor, folded block mark, BG');
  Add(nColorGutterFont, 'EdGutterFont', 'editor, gutter font');
  Add(nColorGutterBack, 'EdGutterBg', 'editor, gutter BG');
  Add(nColorGutterFont, 'EdGutterCaretFont', 'editor, gutter font, lines with carets');
  Add(nColorListSelBack, 'EdGutterCaretBg', 'editor, gutter BG, lines with carets');
  Add(nColorGutterFont, 'EdRulerFont', 'editor, ruler font');
  Add(nColorBack, 'EdRulerBg', 'editor, ruler BG');
  Add(nColorGutterFont, 'EdFoldLine', 'editor, gutter folding, lines');
  Add(nColorGutterBack, 'EdFoldBg', 'editor, gutter folding, BG');
  Add(nColorGutterFont, 'EdFoldPlusLine', 'editor, gutter folding, "plus" border');
  Add(nColorGutterBack, 'EdFoldPlusBg', 'editor, gutter folding, "plus" BG');
  Add(clLtGray, 'EdMarginFixed', 'editor, margin, fixed position');
  Add($b0c0c0, 'EdMarginCaret', 'editor, margins, for carets');
  Add($b0c0c0, 'EdMarginUser', 'editor, margins, user defined');
  Add(clMoneyGreen, 'EdBookmarkBg', 'editor, bookmark, line BG');
  Add(clMedGray, 'EdBookmarkIcon', 'editor, bookmark, gutter mark');
  Add($f0e0b0, 'EdMarkedRangeBg', 'editor, marked range BG');

  Add(nColorBack2, 'TabBg', 'main-toolbar, tabs BG');
  Add($808080, 'SideBg', 'side-toolbar BG');
  Add(nColorText, 'TabFont', 'tabs, font');
  Add($a00000, 'TabFontMod', 'tabs, font, modified tab');
  Add(nColorBack, 'TabActive', 'tabs, active tab BG');
  Add($e4d0d0, 'TabActiveOthers', 'tabs, active tab BG, inactive groups');
  Add(nColorBack2+$0a0a0a, 'TabPassive', 'tabs, passive tab BG');
  Add($ffffff, 'TabOver', 'tabs, mouse-over tab BG');
  Add(nColorBorder, 'TabBorderActive', 'tabs, active tab border');
  Add(nColorBorder, 'TabBorderPassive', 'tabs, passive tab border');
  Add(clNone, 'TabCloseBg', 'tabs, close button BG');
  Add($9090c0, 'TabCloseBgOver', 'tabs, close button BG, mouse-over');
  Add($9090c0, 'TabCloseBorderOver', 'tabs, close button border');
  Add(nColorArrow, 'TabCloseX', 'tabs, close x mark');
  Add(nColorBack, 'TabCloseXOver', 'tabs, close x mark, mouse-over');
  Add(nColorArrow, 'TabArrow', 'tabs, tab-list arrow-down');
  Add(nColorArrow, 'TabArrowOver', 'tabs, tab-list arrow-down, mouse-over');

  Add(nColorText, 'TreeFont', 'treeview, font');
  Add(nColorBack, 'TreeBg', 'treeview, BG');
  Add(nColorText, 'TreeSelFont', 'treeview, selected font');
  Add(nColorListSelBack, 'TreeSelBg', 'treeview, selected BG');
  Add(nColorGutterFont, 'TreeLines', 'treeview, lines');
  Add(nColorGutterFont, 'TreeSign', 'treeview, fold sign');

  Add(nColorListBack, 'ListBg', 'listbox, BG');
  Add(nColorListSelBack, 'ListSelBg', 'listbox, selected line BG');
  Add(nColorText, 'ListFont', 'listbox, font');
  Add(nColorText, 'ListSelFont', 'listbox, selected line font');
  Add($c05050, 'ListFontHotkey', 'listbox, font, hotkey');
  Add($c05050, 'ListFontHilite', 'listbox, font, search chars');

  Add($c05050, 'ListCompletePrefix', 'listbox, font, auto-complete prefix');
  Add(clGray, 'ListCompleteParams', 'listbox, font, auto-complete params');

  Add($a0a0a0, 'GaugeFill', 'search progressbar, fill');
  Add($e0e0e0, 'GaugeBg', 'search progressbar, BG');

  Add(nColorText, 'ButtonFont', 'buttons, font');
  Add($808088, 'ButtonFontDisabled', 'buttons, font, disabled state');
  Add(nColorBack, 'ButtonBgPassive', 'buttons, BG, passive');
  Add($d0b0b0, 'ButtonBgOver', 'buttons, BG, mouse-over');
  Add($b0b0b0, 'ButtonBgChecked', 'buttons, BG, checked state');
  Add($c0c0d0, 'ButtonBgDisabled', 'buttons, BG, disabled state');
  Add(nColorBorder, 'ButtonBorderPassive', 'buttons, border, passive');
  Add(nColorBorder, 'ButtonBorderOver', 'buttons, border, mouse-over');
  Add(clGray, 'ButtonBorderFocused', 'buttons, border, focused');

  Add(nColorGutterBack, 'ScrollBack', 'scrollbar, BG');
  Add(nColorBorder, 'ScrollRect', 'scrollbar, thumb border');
  Add(nColorBack, 'ScrollFill', 'scrollbar, thumb fill');
  Add(nColorArrow, 'ScrollArrow', 'scrollbar, arrow');
  Add($d0d0d0, 'ScrollScrolled', 'scrollbar, scrolling area');

  Add(nColorText, 'StatusFont', 'statusbar, font');
  Add(nColorBack2, 'StatusBg', 'statusbar, BG');
  Add(nColorBorder, 'StatusLines', 'statusbar, border');
  Add(nColorText, 'StatusAltFont', 'statusbar alternative, font');
  Add(clCream, 'StatusAltBg', 'statusbar alternative, BG');

  Add(nColorBack2, 'SplitMain', 'splitters, main');
  Add(nColorBack2, 'SplitGroups', 'splitters, groups');

  Add(clWhite, 'ExportHtmlBg', 'export to html, BG');
  Add(clMedGray, 'ExportHtmlNumbers', 'export to html, line numbers');

  //--------------
  //add styles
  AddStyle('Id', nColorText, clNone, clNone, [], blNone, blNone, blNone, blNone, ftFontAttr);
  AddStyle('Id1', clNavy, clNone, clNone, [], blNone, blNone, blNone, blNone, ftFontAttr);
  AddStyle('Id2', clPurple, clNone, clNone, [], blNone, blNone, blNone, blNone, ftFontAttr);
  AddStyle('Id3', clOlive, clNone, clNone, [], blNone, blNone, blNone, blNone, ftFontAttr);
  AddStyle('Id4', clBlue, clNone, clNone, [], blNone, blNone, blNone, blNone, ftFontAttr);
  AddStyle('IdKeyword', clBlack, clNone, clNone, [fsBold], blNone, blNone, blNone, blNone, ftFontAttr);
  AddStyle('IdVar', clGreen, clNone, clNone, [], blNone, blNone, blNone, blNone, ftFontAttr);
  AddStyle('IdBad', clBlack, clNone, clRed, [], blNone, blNone, blNone, blSolid, ftFontAttr);

  AddStyle('String', clTeal, clNone, clNone, [], blNone, blNone, blNone, blNone, ftFontAttr);
  AddStyle('String2', clOlive, clNone, clNone, [], blNone, blNone, blNone, blNone, ftFontAttr);
  AddStyle('String3', $C8C040, clNone, clNone, [], blNone, blNone, blNone, blNone, ftFontAttr);

  AddStyle('Symbol', clMaroon, clNone, clNone, [], blNone, blNone, blNone, blNone, ftFontAttr);
  AddStyle('Symbol2', $0000C0, clNone, clNone, [], blNone, blNone, blNone, blNone, ftFontAttr);
  AddStyle('SymbolBad', clMaroon, clNone, clRed, [], blNone, blNone, blNone, blSolid, ftFontAttr);

  AddStyle('Comment', clGray, clNone, clNone, [fsItalic], blNone, blNone, blNone, blNone, ftFontAttr);
  AddStyle('Comment2', $00C080, clNone, clNone, [fsItalic], blNone, blNone, blNone, blNone, ftFontAttr);
  AddStyle('CommentDoc', $A0B090, clNone, clNone, [fsItalic], blNone, blNone, blNone, blNone, ftFontAttr);

  AddStyle('Number', clNavy, clNone, clNone, [fsBold], blNone, blNone, blNone, blNone, ftFontAttr);
  AddStyle('Label', $607EB6, clNone, clNone, [], blNone, blNone, blNone, blNone, ftFontAttr);
  AddStyle('Color', $0080C0, clNone, clNone, [fsBold], blNone, blNone, blNone, blNone, ftFontAttr);

  AddStyle('IncludeBG1', clBlack, clMoneyGreen, clNone, [], blNone, blNone, blNone, blNone, ftBackGround);
  AddStyle('IncludeBG2', clBlack, clSkyBlue, clNone, [], blNone, blNone, blNone, blNone, ftBackGround);
  AddStyle('IncludeBG3', clBlack, $F0B0F0, clNone, [], blNone, blNone, blNone, blNone, ftBackGround);
  AddStyle('IncludeBG4', clBlack, $B0F0F0, clNone, [], blNone, blNone, blNone, blNone, ftBackGround);

  AddStyle('SectionBG1', clBlack, clCream, clNone, [], blNone, blNone, blNone, blNone, ftBackGround);
  AddStyle('SectionBG2', clBlack, $E0FFE0, clNone, [], blNone, blNone, blNone, blNone, ftBackGround);
  AddStyle('SectionBG3', clBlack, $F0F0E0, clNone, [], blNone, blNone, blNone, blNone, ftBackGround);
  AddStyle('SectionBG4', clBlack, $FFE0FF, clNone, [], blNone, blNone, blNone, blNone, ftBackGround);

  AddStyle('BracketBG', clBlack, clMoneyGreen, clGray, [], blSolid, blSolid, blSolid, blSolid, ftFontAttr);
  AddStyle('CurBlockBG', clBlack, $E8E8E8, clNone, [], blNone, blNone, blNone, blNone, ftBackGround);
  AddStyle('SeparLine', clBlack, $00E000, clNone, [], blNone, blNone, blNone, blNone, ftBackGround);

  AddStyle('TagBound', clGray, clNone, clNone, [], blNone, blNone, blNone, blNone, ftFontAttr);
  AddStyle('TagId', $F06060, clNone, clNone, [], blNone, blNone, blNone, blNone, ftFontAttr);
  AddStyle('TagIdBad', $F06060, clNone, clRed, [], blNone, blNone, blNone, blWavyLine, ftFontAttr);
  AddStyle('TagProp', $40A040, clNone, clNone, [], blNone, blNone, blNone, blNone, ftFontAttr);
  AddStyle('TagPropBad', $40A040, clNone, clRed, [], blNone, blNone, blNone, blWavyLine, ftFontAttr);

  AddStyle('LightBG1', clBlack, $8080FF, clNone, [], blNone, blNone, blNone, blNone, ftFontAttr);
  AddStyle('LightBG2', clBlack, clYellow, clNone, [], blNone, blNone, blNone, blNone, ftFontAttr);
  AddStyle('LightBG3', clBlack, $40F040, clNone, [], blNone, blNone, blNone, blNone, ftFontAttr);
  AddStyle('LightBG4', clBlack, $F08080, clNone, [], blNone, blNone, blNone, blNone, ftFontAttr);
  AddStyle('LightBG5', clBlack, $C0C0B0, clNone, [], blNone, blNone, blNone, blNone, ftFontAttr);

  AddStyle('Pale1', $A0E0E0, clNone, clNone, [], blNone, blNone, blNone, blNone, ftFontAttr);
  AddStyle('Pale2', $E0E0A0, clNone, clNone, [], blNone, blNone, blNone, blNone, ftFontAttr);
  AddStyle('Pale3', $E0E0E0, clNone, clNone, [], blNone, blNone, blNone, blNone, ftFontAttr);

  AddStyle('TextBold', clBlack, clNone, clNone, [fsBold], blNone, blNone, blNone, blNone, ftFontAttr);
  AddStyle('TextItalic', clBlack, clNone, clNone, [fsItalic], blNone, blNone, blNone, blNone, ftFontAttr);
  AddStyle('TextBoldItalic', clBlack, clNone, clNone, [fsBold, fsItalic], blNone, blNone, blNone, blNone, ftFontAttr);
  AddStyle('TextCross', clBlack, clNone, clNone, [fsStrikeOut], blNone, blNone, blNone, blNone, ftFontAttr);
end;

procedure DoSaveTheme(const fn: string; const D: TAppTheme; IsThemeUI: boolean);
var
  c: TJSONConfig;
  i: integer;
  st: TecSyntaxFormat;
begin
  if FileExists(fn) then
    DeleteFile(fn);
  c:= TJSONConfig.Create(nil);
  try
    try
      c.Formatted:= true;
      c.Filename:= fn;
    except
      MsgBox(msgStatusIncorrectFilename+#13+fn, MB_OK or MB_ICONERROR);
      exit;
    end;

    if IsThemeUI then
    begin
      for i:= low(d.Colors) to high(d.Colors) do
        c.SetValue(d.Colors[i].name, SColorToHtmlColor(d.Colors[i].color));
    end
    else
    begin
      for i:= 0 to d.Styles.Count-1 do
      begin
        st:= TecSyntaxFormat(d.Styles[i]);
        DoSaveLexerStyleToFile(st, c, 'Lex_'+st.DisplayName);
      end;
    end;
  finally
    c.Free;
  end;
end;

function GetAppColor(const name: string): TColor;
var
  i: integer;
begin
  for i:= Low(AppTheme.Colors) to High(AppTheme.Colors) do
    if AppTheme.Colors[i].name=name then
      begin Result:= AppTheme.Colors[i].color; exit end;
  raise Exception.Create('Incorrect color id: '+name);
end;

function GetAppStyleFromName(const SName: string): TecSyntaxFormat;
var
  st: TecSyntaxFormat;
  i: integer;
begin
  Result:= nil;
  for i:= 0 to AppTheme.Styles.Count-1 do
  begin
    st:= TecSyntaxFormat(AppTheme.Styles[i]);
    if st.DisplayName=SName then exit(st);
  end;
end;


initialization
  DoInitTheme(AppTheme);

end.

