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
  Classes, SysUtils, Graphics,
  ATStringProc,
  ATStringProc_HtmlColor,
  ec_SyntAnal,
  ec_syntax_format,
  proc_msg,
  proc_globdata,
  proc_lexer_styles;

type
  TAppThemeColorId = (
    apclEdTextFont,
    apclEdTextBg,
    apclEdSelFont,
    apclEdSelBg,
    apclEdDisableFont,
    apclEdDisableBg,
    apclEdLinks,
    apclEdLockedBg,
    apclEdCaret,
    apclEdMarkers,
    apclEdCurLineBg,
    apclEdIndentVLine,
    apclEdUnprintFont,
    apclEdUnprintBg,
    apclEdUnprintHexFont,
    apclEdMinimapBorder,
    apclEdMinimapSelBg,
    apclEdMinimapTooltipBg,
    apclEdMinimapTooltipBorder,
    apclEdMicromapBg,
    apclEdMicromapViewBg,
    apclEdMicromapOccur,
    apclEdMicromapSpell,
    apclEdStateChanged,
    apclEdStateAdded,
    apclEdStateSaved,
    apclEdBlockStaple,
    apclEdBlockStapleActive,
    apclEdComboArrow,
    apclEdComboArrowBg,
    apclEdBorder,
    apclEdBorderFocused,
    apclEdBlockSepLine,
    apclEdFoldMarkLine,
    apclEdFoldMarkFont,
    apclEdFoldMarkBorder,
    apclEdFoldMarkBg,
    apclEdGutterFont,
    apclEdGutterBg,
    apclEdGutterCaretFont,
    apclEdGutterCaretBg,
    apclEdRulerFont,
    apclEdRulerBg,
    apclEdFoldLine,
    apclEdFoldBg,
    apclEdFoldPlusLine,
    apclEdFoldPlusBg,
    apclEdMarginFixed,
    apclEdMarginCaret,
    apclEdMarginUser,
    apclEdBookmarkBg,
    apclEdBookmarkIcon,
    apclEdMarkedRangeBg,
    apclTabBg,
    apclSideBg,
    apclSideBadgeBg,
    apclSideBadgeFont,
    apclTabFont,
    apclTabFontMod,
    apclTabActive,
    apclTabActiveOthers,
    apclTabPassive,
    apclTabOver,
    apclTabBorderActive,
    apclTabBorderPassive,
    apclTabCloseBg,
    apclTabCloseBgOver,
    apclTabCloseBorderOver,
    apclTabCloseX,
    apclTabCloseXOver,
    apclTabArrow,
    apclTabArrowOver,
    apclTabActiveMark,
    apclTabMarks,
    apclTreeFont,
    apclTreeBg,
    apclTreeSelFont,
    apclTreeSelBg,
    apclTreeSelBg2,
    apclTreeSign,
    apclListBg,
    apclListSelBg,
    apclListFont,
    apclListSelFont,
    apclListFontHotkey,
    apclListFontHilite,
    apclListCompletePrefix,
    apclListCompleteParams,
    apclButtonFont,
    apclButtonFontDisabled,
    apclButtonBgPassive,
    apclButtonBgOver,
    apclButtonBgChecked,
    apclButtonBgDisabled,
    apclButtonBorderPassive,
    apclButtonBorderOver,
    apclButtonBorderFocused,
    apclScrollBack,
    apclScrollRect,
    apclScrollFill,
    apclScrollArrow,
    apclScrollScrolled,
    apclSplitMain,
    apclSplitGroups,
    apclExportHtmlBg,
    apclExportHtmlNumbers
    );

type
  TAppThemeColor = record
    color: TColor;
    name, desc: string;
  end;

  TAppTheme = record
    Colors: array[TAppThemeColorId] of TAppThemeColor;
    Styles: TFPList;
  end;

var
  AppTheme: TAppTheme;
  AppStyleBrackets: TecSyntaxFormat = nil;
  AppStyleSymbols: TecSyntaxFormat = nil;
  AppStyleId2: TecSyntaxFormat = nil;
  AppStyleError: TecSyntaxFormat = nil;

procedure DoInitTheme(var D: TAppTheme);
procedure DoLoadTheme(const fn: string; var D: TAppTheme; IsThemeUI: boolean);
procedure DoSaveTheme(const fn: string; const D: TAppTheme; IsThemeUI: boolean);
function GetAppColor(id: TAppThemeColorId): TColor; inline;
function GetAppStyleFromName(const SName: string): TecSyntaxFormat;

implementation

uses
  ATButtons,
  at__jsonconf;

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
    if s='?' then
    begin
      MsgLogConsole(Format(msgErrorInTheme, [ExtractFileName(fn), id]));
      exit;
    end;
    if s='' then
      Val:= clNone
    else
      Val:= SHtmlColorToColor(s, len, Val);
  end;
  //
var
  st: TecSyntaxFormat;
  iColor: TAppThemeColorId;
  i: integer;
begin
  if not FileExists(fn) then
  begin
    MsgLogConsole('Theme file not found: '+fn);
    exit;
  end;

  c:= TJsonConfig.Create(nil);
  try
    try
      c.Filename:= fn;
    except
      MsgBadConfig(fn);
      Exit
    end;

    if IsThemeUI then
    begin
      for iColor:= Low(iColor) to High(iColor) do
        DoVal(D.Colors[iColor].color, D.Colors[iColor].name);
    end
    else
    begin
      for i:= 0 to d.Styles.Count-1 do
      begin
        st:= TecSyntaxFormat(d.Styles[i]);
        if not DoLoadLexerStyleFromFile_JsonTheme(st, c, 'Lex_'+st.DisplayName) then
          MsgLogConsole(Format(msgErrorInTheme,
            [ExtractFileName(fn), 'Lex_'+st.DisplayName]));
      end;
    end;
  finally
    c.Free;
  end;
end;


procedure DoInitTheme(var D: TAppTheme);
  //
  procedure SetColor(id: TAppThemeColorId; color: TColor; const name, desc: string); inline;
  begin
    D.Colors[id].color:= color;
    D.Colors[id].name:= name;
    D.Colors[id].desc:= desc;
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
  nColorBorder = $b0b0b0;
  nColorListBack = $c4b8b8;
  nColorListSelBack = $d0d0d0;
  nColorListSelBack2 = $f4f4f4;
begin
  if Assigned(D.Styles) then
    D.Styles.Clear
  else
    D.Styles:= TFPList.Create;

  //init colors
  SetColor(apclEdTextFont, nColorText, 'EdTextFont', 'editor, font');
  SetColor(apclEdTextBg, nColorBack, 'EdTextBg', 'editor, BG');
  SetColor(apclEdSelFont, $e0e0e0, 'EdSelFont', 'editor, selection, font');
  SetColor(apclEdSelBg, $b0a0a0, 'EdSelBg', 'editor, selection, BG');
  SetColor(apclEdDisableFont, clGray, 'EdDisableFont', 'editor, disabled state, font');
  SetColor(apclEdDisableBg, nColorGutterBack, 'EdDisableBg', 'editor, disabled state, BG');
  SetColor(apclEdLinks, $c05050, 'EdLinks', 'editor, links');
  SetColor(apclEdLockedBg, nColorGutterBack, 'EdLockedBg', 'editor, locked state, BG');
  SetColor(apclEdCaret, clBlack, 'EdCaret', 'editor, caret');
  SetColor(apclEdMarkers, $6060d0, 'EdMarkers', 'editor, markers');
  SetColor(apclEdCurLineBg, $eaf0f0, 'EdCurLineBg', 'editor, current line BG');
  SetColor(apclEdIndentVLine, clMedGray, 'EdIndentVLine', 'editor, wrapped line indent vert-lines');
  SetColor(apclEdUnprintFont, $a0a0b8, 'EdUnprintFont', 'editor, unprinted chars, font');
  SetColor(apclEdUnprintBg, $e0e0e0, 'EdUnprintBg', 'editor, unprinted chars, BG');
  SetColor(apclEdUnprintHexFont, clMedGray, 'EdUnprintHexFont', 'editor, special hex codes, font');
  SetColor(apclEdMinimapBorder, clLtGray, 'EdMinimapBorder', 'editor, minimap, border');
  SetColor(apclEdMinimapSelBg, $eeeeee, 'EdMinimapSelBg', 'editor, minimap, view BG');
  SetColor(apclEdMinimapTooltipBg, clMoneyGreen, 'EdMinimapTooltipBg', 'editor, minimap, tooltip BG');
  SetColor(apclEdMinimapTooltipBorder, clMedGray, 'EdMinimapTooltipBorder', 'editor, minimap, tooltip border');
  SetColor(apclEdMicromapBg, $e0e0e0, 'EdMicromapBg', 'editor, micromap, BG');
  SetColor(apclEdMicromapViewBg, $d0d0d0, 'EdMicromapViewBg', 'editor, micromap, current view area');
  SetColor(apclEdMicromapOccur, $c05050, 'EdMicromapOccur', 'editor, micromap, word occurrences');
  SetColor(apclEdMicromapSpell, $6060d0, 'EdMicromapSpell', 'editor, micromap, misspelled marks');
  SetColor(apclEdStateChanged, $70b0b0, 'EdStateChanged', 'editor, line states, changed');
  SetColor(apclEdStateAdded, $80a080, 'EdStateAdded', 'editor, line states, added');
  SetColor(apclEdStateSaved, clMedGray, 'EdStateSaved', 'editor, line states, saved');
  SetColor(apclEdBlockStaple, $b0b0b0, 'EdBlockStaple', 'editor, block staples (indent guides)');
  SetColor(apclEdBlockStapleActive, clNone, 'EdBlockStapleActive', 'editor, block staples, for caret');
  SetColor(apclEdComboArrow, nColorArrow, 'EdComboArrow', 'editor, combobox arrow-down');
  SetColor(apclEdComboArrowBg, nColorBack, 'EdComboArrowBg', 'editor, combobox arrow-down BG');
  SetColor(apclEdBorder, nColorBorder, 'EdBorder', 'editor, combobox border');
  SetColor(apclEdBorderFocused, clNavy, 'EdBorderFocused', 'editor, combobox border, focused');
  SetColor(apclEdBlockSepLine, clMedGray, 'EdBlockSepLine', 'editor, separator line');
  SetColor(apclEdFoldMarkLine, $a06060, 'EdFoldMarkLine', 'editor, folded line');
  SetColor(apclEdFoldMarkFont, $e08080, 'EdFoldMarkFont', 'editor, folded block mark, font');
  SetColor(apclEdFoldMarkBorder, $e08080, 'EdFoldMarkBorder', 'editor, folded block mark, border');
  SetColor(apclEdFoldMarkBg, nColorBack, 'EdFoldMarkBg', 'editor, folded block mark, BG');
  SetColor(apclEdGutterFont, nColorGutterFont, 'EdGutterFont', 'editor, gutter font');
  SetColor(apclEdGutterBg, nColorGutterBack, 'EdGutterBg', 'editor, gutter BG');
  SetColor(apclEdGutterCaretFont, nColorGutterFont, 'EdGutterCaretFont', 'editor, gutter font, lines with carets');
  SetColor(apclEdGutterCaretBg, nColorListSelBack, 'EdGutterCaretBg', 'editor, gutter BG, lines with carets');
  SetColor(apclEdRulerFont, nColorGutterFont, 'EdRulerFont', 'editor, ruler font');
  SetColor(apclEdRulerBg, nColorBack, 'EdRulerBg', 'editor, ruler BG');
  SetColor(apclEdFoldLine, nColorGutterFont, 'EdFoldLine', 'editor, gutter folding, lines');
  SetColor(apclEdFoldBg, nColorGutterBack, 'EdFoldBg', 'editor, gutter folding, BG');
  SetColor(apclEdFoldPlusLine, nColorGutterFont, 'EdFoldPlusLine', 'editor, gutter folding, "plus" border');
  SetColor(apclEdFoldPlusBg, nColorGutterBack, 'EdFoldPlusBg', 'editor, gutter folding, "plus" BG');
  SetColor(apclEdMarginFixed, clLtGray, 'EdMarginFixed', 'editor, margin, fixed position');
  SetColor(apclEdMarginCaret, $b0c0c0, 'EdMarginCaret', 'editor, margins, for carets');
  SetColor(apclEdMarginUser, $b0c0c0, 'EdMarginUser', 'editor, margins, user defined');
  SetColor(apclEdBookmarkBg, clMoneyGreen, 'EdBookmarkBg', 'editor, bookmark, line BG');
  SetColor(apclEdBookmarkIcon, clMedGray, 'EdBookmarkIcon', 'editor, bookmark, gutter mark');
  SetColor(apclEdMarkedRangeBg, $f0e0b0, 'EdMarkedRangeBg', 'editor, marked range BG');
  SetColor(apclTabBg, nColorBack2, 'TabBg', 'main-toolbar, tabs BG');
  SetColor(apclSideBg, $808080, 'SideBg', 'side-toolbar BG');
  SetColor(apclSideBadgeBg, clNavy, 'SideBadgeBg', 'side-toolbar, button badges BG');
  SetColor(apclSideBadgeFont, clWhite, 'SideBadgeFont', 'side-toolbar, button badges font');
  SetColor(apclTabFont, nColorText, 'TabFont', 'tabs, font');
  SetColor(apclTabFontMod, $a00000, 'TabFontMod', 'tabs, font, modified tab');
  SetColor(apclTabActive, nColorBack, 'TabActive', 'tabs, active tab BG');
  SetColor(apclTabActiveOthers, $e4d0d0, 'TabActiveOthers', 'tabs, active tab BG, inactive groups');
  SetColor(apclTabPassive, nColorBack2+$0a0a0a, 'TabPassive', 'tabs, passive tab BG');
  SetColor(apclTabOver, $ffffff, 'TabOver', 'tabs, mouse-over tab BG');
  SetColor(apclTabBorderActive, nColorBorder, 'TabBorderActive', 'tabs, active tab border');
  SetColor(apclTabBorderPassive, nColorBorder, 'TabBorderPassive', 'tabs, passive tab border');
  SetColor(apclTabCloseBg, clNone, 'TabCloseBg', 'tabs, close button BG');
  SetColor(apclTabCloseBgOver, $9090c0, 'TabCloseBgOver', 'tabs, close button BG, mouse-over');
  SetColor(apclTabCloseBorderOver, $9090c0, 'TabCloseBorderOver', 'tabs, close button border');
  SetColor(apclTabCloseX, nColorArrow, 'TabCloseX', 'tabs, close x mark');
  SetColor(apclTabCloseXOver, nColorBack, 'TabCloseXOver', 'tabs, close x mark, mouse-over');
  SetColor(apclTabArrow, nColorArrow, 'TabArrow', 'tabs, triangle arrows');
  SetColor(apclTabArrowOver, $404040, 'TabArrowOver', 'tabs, triangle arrows, mouse-over');
  SetColor(apclTabActiveMark, clMedGray, 'TabActiveMark', 'tabs, flat style, active tab mark');
  SetColor(apclTabMarks, $6060E0, 'TabMarks', 'tabs, special marks');
  SetColor(apclTreeFont, nColorText, 'TreeFont', 'treeview, font');
  SetColor(apclTreeBg, nColorBack, 'TreeBg', 'treeview, BG');
  SetColor(apclTreeSelFont, nColorText, 'TreeSelFont', 'treeview, selected font');
  SetColor(apclTreeSelBg, nColorListSelBack, 'TreeSelBg', 'treeview, selected BG');
  SetColor(apclTreeSelBg2, nColorListSelBack2, 'TreeSelBg2', 'treeview, selected BG, not focused');
  SetColor(apclTreeSign, nColorGutterFont, 'TreeSign', 'treeview, folding sign');
  SetColor(apclListBg, nColorListBack, 'ListBg', 'listbox, BG');
  SetColor(apclListSelBg, nColorListSelBack, 'ListSelBg', 'listbox, selected line BG');
  SetColor(apclListFont, nColorText, 'ListFont', 'listbox, font');
  SetColor(apclListSelFont, nColorText, 'ListSelFont', 'listbox, selected line font');
  SetColor(apclListFontHotkey, $c05050, 'ListFontHotkey', 'listbox, font, hotkey');
  SetColor(apclListFontHilite, $e00000, 'ListFontHilite', 'listbox, font, search chars');
  SetColor(apclListCompletePrefix, $c05050, 'ListCompletePrefix', 'listbox, font, auto-complete prefix');
  SetColor(apclListCompleteParams, clGray, 'ListCompleteParams', 'listbox, font, auto-complete params');
  SetColor(apclButtonFont, nColorText, 'ButtonFont', 'buttons, font');
  SetColor(apclButtonFontDisabled, $808088, 'ButtonFontDisabled', 'buttons, font, disabled state');
  SetColor(apclButtonBgPassive, nColorBack, 'ButtonBgPassive', 'buttons, BG, passive');
  SetColor(apclButtonBgOver, $d0b0b0, 'ButtonBgOver', 'buttons, BG, mouse-over');
  SetColor(apclButtonBgChecked, $b0b0b0, 'ButtonBgChecked', 'buttons, BG, checked state');
  SetColor(apclButtonBgDisabled, $c0c0d0, 'ButtonBgDisabled', 'buttons, BG, disabled state');
  SetColor(apclButtonBorderPassive, nColorBorder, 'ButtonBorderPassive', 'buttons, border, passive');
  SetColor(apclButtonBorderOver, nColorBorder, 'ButtonBorderOver', 'buttons, border, mouse-over');
  SetColor(apclButtonBorderFocused, clGray, 'ButtonBorderFocused', 'buttons, border, focused');
  SetColor(apclScrollBack, $d0d0d0, 'ScrollBack', 'scrollbar, BG');
  SetColor(apclScrollRect, nColorBorder, 'ScrollRect', 'scrollbar, thumb border');
  SetColor(apclScrollFill, nColorBack, 'ScrollFill', 'scrollbar, thumb fill');
  SetColor(apclScrollArrow, nColorArrow, 'ScrollArrow', 'scrollbar, arrows');
  SetColor(apclScrollScrolled, $c0c0a0, 'ScrollScrolled', 'scrollbar, scrolling area');
  SetColor(apclSplitMain, nColorBack2, 'SplitMain', 'splitters, main');
  SetColor(apclSplitGroups, nColorBack2, 'SplitGroups', 'splitters, groups');
  SetColor(apclExportHtmlBg, clWhite, 'ExportHtmlBg', 'export to html, BG');
  SetColor(apclExportHtmlNumbers, clMedGray, 'ExportHtmlNumbers', 'export to html, line numbers');

  //--------------
  //styles
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

  //don't use Italic for comments, coz comments often have Unicode
  AddStyle('Comment', clGray, clNone, clNone, [], blNone, blNone, blNone, blNone, ftFontAttr);
  AddStyle('Comment2', $00C080, clNone, clNone, [], blNone, blNone, blNone, blNone, ftFontAttr);
  AddStyle('CommentDoc', $809070, clNone, clNone, [], blNone, blNone, blNone, blNone, ftFontAttr);

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
  AddStyle('TagInclude', clOlive, clNone, clNone, [fsBold], blNone, blNone, blNone, blNone, ftFontAttr);

  AddStyle('LightBG1', clBlack, $8080F0, clNone, [], blNone, blNone, blNone, blNone, ftFontAttr);
  AddStyle('LightBG2', clBlack, $60F0F0, clNone, [], blNone, blNone, blNone, blNone, ftFontAttr);
  AddStyle('LightBG3', clBlack, $80F080, clNone, [], blNone, blNone, blNone, blNone, ftFontAttr);
  AddStyle('LightBG4', clBlack, $F08080, clNone, [], blNone, blNone, blNone, blNone, ftFontAttr);
  AddStyle('LightBG5', clBlack, $C0C0B0, clNone, [], blNone, blNone, blNone, blNone, ftFontAttr);

  AddStyle('Pale1', $A0E0E0, clNone, clNone, [], blNone, blNone, blNone, blNone, ftFontAttr);
  AddStyle('Pale2', $E0E0A0, clNone, clNone, [], blNone, blNone, blNone, blNone, ftFontAttr);
  AddStyle('Pale3', $E0E0E0, clNone, clNone, [], blNone, blNone, blNone, blNone, ftFontAttr);

  AddStyle('TextBold', clBlack, clNone, clNone, [fsBold], blNone, blNone, blNone, blNone, ftFontAttr);
  AddStyle('TextItalic', clBlack, clNone, clNone, [fsItalic], blNone, blNone, blNone, blNone, ftFontAttr);
  AddStyle('TextBoldItalic', clBlack, clNone, clNone, [fsBold, fsItalic], blNone, blNone, blNone, blNone, ftFontAttr);
  AddStyle('TextCross', clBlack, clNone, clNone, [fsStrikeOut], blNone, blNone, blNone, blNone, ftFontAttr);

  AppStyleBrackets:= GetAppStyleFromName('BracketBG');
  AppStyleSymbols:= GetAppStyleFromName('Symbol');
  AppStyleId2:= GetAppStyleFromName('Id2');
  AppStyleError:= GetAppStyleFromName('LightBG1');
end;

procedure DoSaveTheme(const fn: string; const D: TAppTheme; IsThemeUI: boolean);
var
  c: TJSONConfig;
  iColor: TAppThemeColorId;
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
      MsgBadConfig(fn);
      exit;
    end;

    if IsThemeUI then
    begin
      for iColor:= Low(iColor) to High(iColor) do
        c.SetValue(D.Colors[iColor].name, SColorToHtmlColor(D.Colors[iColor].color));
    end
    else
    begin
      for i:= 0 to d.Styles.Count-1 do
      begin
        st:= TecSyntaxFormat(d.Styles[i]);
        DoSaveLexerStyleToFile_JsonTheme(st, c, 'Lex_'+st.DisplayName);
      end;
    end;
  finally
    c.Free;
  end;
end;

function GetAppColor(id: TAppThemeColorId): TColor;
begin
  Result:= AppTheme.Colors[id].Color;
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

