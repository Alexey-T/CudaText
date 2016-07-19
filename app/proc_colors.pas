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
procedure DoLoadTheme(const fn: string; var D: TAppTheme);
procedure DoSaveTheme(const fn: string; const D: TAppTheme);
function GetAppColor(const name: string): TColor;
function GetAppStyleFromName(const SName: string): TecSyntaxFormat;


implementation

uses
  ATButtons,
  jsonConf;

procedure DoLoadTheme(const fn: string; var D: TAppTheme);
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

    //load colors
    for i:= Low(D.Colors) to High(D.Colors) do
      DoVal(D.Colors[i].color, D.Colors[i].name);

    //load styles
    for i:= 0 to d.Styles.Count-1 do
    begin
      st:= TecSyntaxFormat(d.Styles[i]);
      DoLoadLexerStyleFromFile(st, c, 'Lex_'+st.DisplayName);
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
  cl_back = $e4e4e4;
  cl_back2 = $d0d0d0;
  cl_back_gutter = $d8d8d8;
  cl_arrow = $a0a0a0;
  cl_frame = $c0c0c0;
begin
  SetLength(D.Colors, 0);

  if Assigned(D.Styles) then
    D.Styles.Clear
  else
    D.Styles:= TList.Create;

  //add colors
  Add(clBlack, 'EdTextFont', 'editor, font');
  Add(cl_back, 'EdTextBg', 'editor, BG');
  Add($e0e0e0, 'EdSelFont', 'editor, selection, font');
  Add($b0a0a0, 'EdSelBg', 'editor, selection, BG');
  Add(clGray, 'EdDisableFont', 'editor, disabled state, font');
  Add($c0c0c0, 'EdDisableBg', 'editor, disabled state, BG');
  Add($d08080, 'EdLinks', 'editor, links');
  Add($c0c0c0, 'EdLockedBg', 'editor, locked state, BG');
  Add(clBlack, 'EdCaret', 'editor, caret');
  Add($0000d0, 'EdMarkers', 'editor, markers');
  Add($e0f0f0, 'EdCurLineBg', 'editor, current line BG');
  Add(clMedGray, 'EdIndentVLine', 'editor, wrapped line indent vert-lines');
  Add($5050f0, 'EdUnprintFont', 'editor, unprinted chars, font');
  Add($e0e0e0, 'EdUnprintBg', 'editor, unprinted chars, BG');
  Add(clMedGray, 'EdUnprintHexFont', 'editor, special hex codes, font');
  Add(clLtGray, 'EdMinimapBorder', 'editor, minimap, border');
  Add($eeeeee, 'EdMinimapSelBg', 'editor, minimap, view BG');
  Add($e0e0e0, 'EdMicromapBg', 'editor, micromap, BG');
  Add($c0c0c0, 'EdMicromapViewBg', 'editor, micromap, view BG');
  Add($70b0b0, 'EdStateChanged', 'editor, line states, changed');
  Add($80a080, 'EdStateAdded', 'editor, line states, added');
  Add(clMedGray, 'EdStateSaved', 'editor, line states, saved');
  Add(clMedGray, 'EdBlockStaple', 'editor, block staples (indent guides)');
  Add(cl_arrow, 'EdComboArrow', 'editor, combobox arrow-down');
  Add(cl_back, 'EdComboArrowBg', 'editor, combobox arrow-down BG');
  Add(cl_frame, 'EdBorder', 'editor, combobox border');
  Add(clMedGray, 'EdBlockSepLine', 'editor, separator line');
  Add($a06060, 'EdFoldMarkLine', 'editor, folded line');
  Add($e08080, 'EdFoldMarkFont', 'editor, folded block mark, font');
  Add($e08080, 'EdFoldMarkBorder', 'editor, folded block mark, border');
  Add(clCream, 'EdFoldMarkBg', 'editor, folded block mark, BG');
  Add(clGray, 'EdGutterFont', 'editor, gutter font');
  Add(cl_back_gutter, 'EdGutterBg', 'editor, gutter BG');
  Add($808080, 'EdGutterCaretFont', 'editor, gutter font, lines with carets');
  Add($c4c4c4, 'EdGutterCaretBg', 'editor, gutter BG, lines with carets');
  Add(clGray, 'EdRulerFont', 'editor, ruler font');
  Add(cl_back, 'EdRulerBg', 'editor, ruler BG');
  Add(clGray, 'EdFoldLine', 'editor, gutter folding, lines');
  Add(cl_back_gutter, 'EdFoldBg', 'editor, gutter folding, BG');
  Add(clGray, 'EdFoldPlusLine', 'editor, gutter folding, "plus" border');
  Add(cl_back_gutter, 'EdFoldPlusBg', 'editor, gutter folding, "plus" BG');
  Add(clLtGray, 'EdMarginFixed', 'editor, margin, fixed position');
  Add(clLime, 'EdMarginCaret', 'editor, margins, for carets');
  Add(clYellow, 'EdMarginUser', 'editor, margins, user defined');
  Add(clMoneyGreen, 'EdBookmarkBg', 'editor, bookmark, line BG');
  Add(clMedGray, 'EdBookmarkIcon', 'editor, bookmark, gutter mark');
  Add($f0e0b0, 'EdMarkedRangeBg', 'editor, marked range BG');

  Add(cl_back2, 'TabBg', 'tabs, toolbar BG');
  Add(clBlack, 'TabFont', 'tabs, font');
  Add($A00000, 'TabFontMod', 'tabs, font, modified tab');
  Add(cl_back, 'TabActive', 'tabs, active tab BG');
  Add($c8c8c8, 'TabActiveOthers', 'tabs, active tab BG, inactive groups');
  Add($f4f4f4, 'TabPassive', 'tabs, passive tab BG');
  Add($ffffff, 'TabOver', 'tabs, mouse-over tab BG');
  Add(cl_frame, 'TabBorderActive', 'tabs, active tab border');
  Add(cl_frame, 'TabBorderPassive', 'tabs, passive tab border');
  Add(clNone, 'TabCloseBg', 'tabs, close button BG');
  Add($9090c0, 'TabCloseBgOver', 'tabs, close button BG, mouse-over');
  Add($9090c0, 'TabCloseBorderOver', 'tabs, close button border');
  Add(cl_arrow, 'TabCloseX', 'tabs, close x mark');
  Add(cl_back, 'TabCloseXOver', 'tabs, close x mark, mouse-over');
  Add(cl_arrow, 'TabArrow', 'tabs, tab-list arrow-down');
  Add(cl_arrow, 'TabArrowOver', 'tabs, tab-list arrow-down, mouse-over');

  Add(clBlack, 'TreeFont', 'treeview, font');
  Add(cl_back, 'TreeBg', 'treeview, BG');
  Add(clBlack, 'TreeSelFont', 'treeview, selected font');
  Add($c8c8c8, 'TreeSelBg', 'treeview, selected BG');
  Add(clMedGray, 'TreeLines', 'treeview, lines');
  Add(clMedGray, 'TreeSign', 'treeview, fold sign');

  Add(cl_back, 'ListBg', 'listbox, BG');
  Add($c8c8c8, 'ListSelBg', 'listbox, selected line BG');
  Add(clBlack, 'ListFont', 'listbox, font');
  Add(clBlack, 'ListSelFont', 'listbox, selected line font');
  Add($802020, 'ListFontHotkey', 'listbox, font, hotkey');
  Add($f04040, 'ListFontHilite', 'listbox, font, search chars');

  Add(clPurple, 'ListCompletePrefix', 'listbox, font, auto-complete prefix');
  Add(clGray, 'ListCompleteParams', 'listbox, font, auto-complete params');

  Add($a0a0a0, 'GaugeFill', 'search progressbar, fill');
  Add($e0e0e0, 'GaugeBg', 'search progressbar, BG');

  Add($303030, 'ButtonFont', 'buttons, font');
  Add($808088, 'ButtonFontDisabled', 'buttons, font, disabled state');
  Add($e0e0e0, 'ButtonBgPassive', 'buttons, BG, passive');
  Add($e0e0e0, 'ButtonBgOver', 'buttons, BG, mouse-over');
  Add($b0b0b0, 'ButtonBgChecked', 'buttons, BG, checked state');
  Add($c0c0d0, 'ButtonBgDisabled', 'buttons, BG, disabled state');
  Add($a0a0a0, 'ButtonBorderPassive', 'buttons, border, passive');
  Add($d0d0d0, 'ButtonBorderOver', 'buttons, border, mouse-over');
  Add(clNavy, 'ButtonBorderFocused', 'buttons, border, focused');

  Add(cl_back_gutter, 'ScrollBack', 'scrollbar, BG');
  Add(cl_frame, 'ScrollRect', 'scrollbar, thumb border');
  Add(cl_back, 'ScrollFill', 'scrollbar, thumb fill');
  Add(cl_arrow, 'ScrollArrow', 'scrollbar, arrow');
  Add($d0d0d0, 'ScrollScrolled', 'scrollbar, scrolling area');

  Add(clBlack, 'StatusFont', 'statusbar, font');
  Add(cl_back2, 'StatusBg', 'statusbar, BG');
  Add(cl_frame, 'StatusLines', 'statusbar, border');
  Add(clBlack, 'StatusAltFont', 'statusbar alternative, font');
  Add(clCream, 'StatusAltBg', 'statusbar alternative, BG');

  Add($e0e0e0, 'SplitMain', 'splitters, main');
  Add($e0e0e0, 'SplitGroups', 'splitters, groups');

  Add(clWhite, 'ExportHtmlBg', 'export to html, BG');
  Add(clMedGray, 'ExportHtmlNumbers', 'export to html, line numbers');

  //--------------
  //add styles
  AddStyle('Id', clBlack, clNone, clNone, [], blNone, blNone, blNone, blNone, ftFontAttr);
  AddStyle('Id1', clNavy, clNone, clNone, [], blNone, blNone, blNone, blNone, ftFontAttr);
  AddStyle('Id2', clPurple, clNone, clNone, [], blNone, blNone, blNone, blNone, ftFontAttr);
  AddStyle('Id3', clOlive, clNone, clNone, [], blNone, blNone, blNone, blNone, ftFontAttr);
  AddStyle('Id4', clBlue, clNone, clNone, [], blNone, blNone, blNone, blNone, ftFontAttr);
  AddStyle('IdKeyword', clBlack, clNone, clNone, [fsBold], blNone, blNone, blNone, blNone, ftFontAttr);
  AddStyle('IdVar', clGreen, clNone, clNone, [], blNone, blNone, blNone, blNone, ftFontAttr);
  AddStyle('IdBad', clBlack, clNone, clRed, [], blNone, blNone, blNone, blDot, ftFontAttr);

  AddStyle('String', clTeal, clNone, clNone, [], blNone, blNone, blNone, blNone, ftFontAttr);
  AddStyle('String2', clOlive, clNone, clNone, [], blNone, blNone, blNone, blNone, ftFontAttr);
  AddStyle('String3', clBlue, clNone, clNone, [], blNone, blNone, blNone, blNone, ftFontAttr);

  AddStyle('Symbol', clMaroon, clNone, clNone, [fsBold], blNone, blNone, blNone, blNone, ftFontAttr);
  AddStyle('Symbol2', $0000C0, clNone, clNone, [fsBold], blNone, blNone, blNone, blNone, ftFontAttr);
  AddStyle('SymbolBad', clMaroon, clNone, clRed, [fsBold], blNone, blNone, blNone, blDot, ftFontAttr);

  AddStyle('Comment', clGray, clNone, clNone, [fsItalic], blNone, blNone, blNone, blNone, ftFontAttr);
  AddStyle('Comment2', $00C080, clNone, clNone, [fsItalic], blNone, blNone, blNone, blNone, ftFontAttr);
  AddStyle('CommentDoc', $A0B090, clNone, clNone, [fsItalic], blNone, blNone, blNone, blNone, ftFontAttr);

  AddStyle('Number', clNavy, clNone, clNone, [fsBold], blNone, blNone, blNone, blNone, ftFontAttr);
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

  AddStyle('TagBound', clGray, clNone, clNone, [fsBold], blNone, blNone, blNone, blNone, ftFontAttr);
  AddStyle('TagId', $F06060, clNone, clNone, [fsBold], blNone, blNone, blNone, blNone, ftFontAttr);
  AddStyle('TagIdBad', $F06060, clNone, clRed, [fsBold], blNone, blNone, blNone, blWavyLine, ftFontAttr);
  AddStyle('TagProp', $40D040, clNone, clNone, [fsBold], blNone, blNone, blNone, blNone, ftFontAttr);

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

procedure DoSaveTheme(const fn: string; const D: TAppTheme);
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

    //save colors
    for i:= low(d.Colors) to high(d.Colors) do
      c.SetValue(d.Colors[i].name, SColorToHtmlColor(d.Colors[i].color));

    //save styles
    for i:= 0 to d.Styles.Count-1 do
    begin
      st:= TecSyntaxFormat(d.Styles[i]);
      DoSaveLexerStyleToFile(st, c, 'Lex_'+st.DisplayName);
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

