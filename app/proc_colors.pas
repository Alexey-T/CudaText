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
  ATStringProc_HtmlColor,
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
    apclEdFoldLine2,
    apclEdFoldBg,
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
    apclTabFontActive,
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
    apclMenuFont,
    apclMenuFontHotkey,
    apclMenuFontDisabled,
    apclMenuBg,
    apclMenuSelBg,
    apclStatusFont,
    apclStatusBg,
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

const
  cAppThemeColorsWhichAllowNone: set of TAppThemeColorId =
    [
    apclEdBlockStapleActive,
    apclTabFontActive,
    apclTabCloseBg,
    apclMenuFont..apclMenuSelBg,
    apclStatusFont,
    apclStatusBg
    ];

type
  TAppThemeStyleId = (
    apstId,
    apstId1,
    apstId2,
    apstId3,
    apstId4,
    apstIdKeyword,
    apstIdVar,
    apstIdBad,
    apstString,
    apstString2,
    apstString3,
    apstSymbol,
    apstSymbol2,
    apstSymbolBad,
    apstComment,
    apstComment2,
    apstCommentDoc,
    apstNumber,
    apstLabel,
    apstColor,
    apstIncludeBG1,
    apstIncludeBG2,
    apstIncludeBG3,
    apstIncludeBG4,
    apstSectionBG1,
    apstSectionBG2,
    apstSectionBG3,
    apstSectionBG4,
    apstBracketBG,
    apstCurBlockBG,
    apstSeparLine,
    apstTagBound,
    apstTagId,
    apstTagIdBad,
    apstTagProp,
    apstTagPropBad,
    apstTagInclude,
    apstLightBG1,
    apstLightBG2,
    apstLightBG3,
    apstLightBG4,
    apstLightBG5,
    apstPale1,
    apstPale2,
    apstPale3,
    //styles below must not be saved to file, see apstLastStyle
    apstTextBold,
    apstTextItalic,
    apstTextBoldItalic,
    apstTextCross,
    apstTextCrossBold,
    apstTextCrossItalic,
    apstTextCrossBoldItalic
    );

const
  //saving theme to file considers it, to skip last items 'bold'/'italic'/etc
  apstLastStyle = Pred(apstTextBold);

type
  TAppThemeColor = record
    color: TColor;
    name, desc: string;
  end;

  TAppTheme = record
    Colors: array[TAppThemeColorId] of TAppThemeColor;
    Styles: array[TAppThemeStyleId] of TecSyntaxFormat;
  end;

var
  AppTheme: TAppTheme;
  AppHiAll_ThemeStyleId: TAppThemeStyleId = apstSeparLine;

procedure AppThemeInit_UI(var D: TAppTheme);
procedure AppThemeInit_Syntax(var D: TAppTheme);
procedure AppThemeFree(var D: TAppTheme);
procedure AppThemeLoadFromFile(const AFileName: string; var D: TAppTheme; IsThemeUI: boolean);
procedure AppThemeSaveToFile(const AFileName: string; const D: TAppTheme; IsThemeUI: boolean);

function GetAppColor(id: TAppThemeColorId): TColor;
function GetAppStyle(id: TAppThemeStyleId): TecSyntaxFormat;

implementation

uses
  ATButtons,
  at__jsonconf;

procedure AppThemeFree(var D: TAppTheme);
var
  St: TecSyntaxFormat;
  id: TAppThemeStyleId;
begin
  for id:= High(TAppThemeStyleId) downto Low(TAppThemeStyleId) do
  begin
    St:= D.Styles[id];
    if Assigned(St) then
    begin
      St.Free;
      D.Styles[id]:= nil;
    end;
  end;
end;

procedure AppThemeLoadFromFile(const AFileName: string; var D: TAppTheme; IsThemeUI: boolean);
var
  cfg: TJsonConfig;
  //
  procedure ReadColorValue(var Val: TColor; const id: string);
  var
    s: string;
    len: integer;
  begin
    s:= cfg.GetValue(id, '?');
    if s='?' then
    begin
      MsgLogConsole(Format(msgErrorInTheme, [ExtractFileName(AFileName), id]));
      exit;
    end;
    if s='' then
      Val:= clNone
    else
      Val:= TATHtmlColorParserA.ParseTokenRGB(PChar(s), len, Val);
  end;
  //
var
  st: TecSyntaxFormat;
  iColor: TAppThemeColorId;
  iStyle: TAppThemeStyleId;
begin
  if not FileExists(AFileName) then
  begin
    MsgLogConsole(Format(msgCannotFindData, [AFileName]));
    MsgStdout(Format(msgCannotFindData, [AFileName]));
    exit;
  end;

  cfg:= TJsonConfig.Create(nil);
  try
    try
      cfg.Filename:= AFileName;
    except
      on E: Exception do
      begin
        MsgBadConfig(AFileName, E.Message);
        Exit
      end;
    end;

    if IsThemeUI then
    begin
      for iColor:= Low(iColor) to High(iColor) do
        ReadColorValue(D.Colors[iColor].color, D.Colors[iColor].name);
    end
    else
    begin
      for iStyle:= Low(iStyle) to apstLastStyle do
      begin
        st:= d.Styles[iStyle];
        if not Lexer_LoadStyleFromFile_JsonTheme(st, cfg, 'Lex_'+st.DisplayName) then
          MsgLogConsole(Format(msgErrorInTheme,
            [ExtractFileName(AFileName), 'Lex_'+st.DisplayName]));
      end;
    end;
  finally
    cfg.Free;
  end;
end;

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

procedure AppThemeInit_UI(var D: TAppTheme);
  //
  procedure SetColor(AId: TAppThemeColorId; AColor: TColor; const AName, ADesc: string); inline;
  begin
    D.Colors[AId].color:= AColor;
    D.Colors[AId].name:= AName;
    D.Colors[AId].desc:= ADesc;
  end;
  //
begin
  SetColor(apclEdTextFont, nColorText, 'EdTextFont', 'editor, font');
  SetColor(apclEdTextBg, nColorBack, 'EdTextBg', 'editor, BG');
  SetColor(apclEdSelFont, $e0e0e0, 'EdSelFont', 'editor, selection, font');
  SetColor(apclEdSelBg, $b0a0a0, 'EdSelBg', 'editor, selection, BG');
  SetColor(apclEdDisableFont, nColorText, 'EdDisableFont', 'editor, disabled state, font');
  SetColor(apclEdDisableBg, $e0e0e0, 'EdDisableBg', 'editor, disabled state, BG');
  SetColor(apclEdLinks, $c05050, 'EdLinks', 'editor, links');
  SetColor(apclEdLockedBg, nColorGutterBack, 'EdLockedBg', 'editor, locked state, BG');
  SetColor(apclEdCaret, clBlack, 'EdCaret', 'editor, caret');
  SetColor(apclEdMarkers, $6060d0, 'EdMarkers', 'editor, markers');
  SetColor(apclEdCurLineBg, $e0e0d0, 'EdCurLineBg', 'editor, current line BG');
  SetColor(apclEdIndentVLine, clMedGray, 'EdIndentVLine', 'editor, wrapped line indent vert-lines');
  SetColor(apclEdUnprintFont, $a0a0b8, 'EdUnprintFont', 'editor, unprinted chars, font');
  SetColor(apclEdUnprintBg, $e0e0e0, 'EdUnprintBg', 'editor, unprinted chars, BG');
  SetColor(apclEdUnprintHexFont, clMedGray, 'EdUnprintHexFont', 'editor, special hex codes, font');
  SetColor(apclEdMinimapBorder, $b09090, 'EdMinimapBorder', 'editor, minimap, border');
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
  SetColor(apclEdBorder, nColorBorder, 'EdBorder', 'editor, border');
  SetColor(apclEdBorderFocused, clNavy, 'EdBorderFocused', 'editor, border, focused');
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
  SetColor(apclEdFoldLine2, $a04040, 'EdFoldLine2', 'editor, gutter folding, lines, current range');
  SetColor(apclEdFoldBg, nColorGutterBack, 'EdFoldBg', 'editor, gutter folding, BG');
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
  SetColor(apclTabFontActive, clNone, 'TabFontActive', 'tabs, font, active tab');
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
  SetColor(apclMenuFont, clNone, 'MenuFont', 'top menu, font');
  SetColor(apclMenuFontHotkey, clNone, 'MenuFontHotkey', 'top menu, font, hotkey');
  SetColor(apclMenuFontDisabled, clNone, 'MenuFontDisabled', 'top menu, font, disabled state');
  SetColor(apclMenuBg, clNone, 'MenuBg', 'top menu, BG');
  SetColor(apclMenuSelBg, clNone, 'MenuSelBg', 'top menu, BG, selected');
  SetColor(apclStatusFont, clNone, 'StatusFont', 'statusbar, font');
  SetColor(apclStatusBg, clNone, 'StatusBg', 'statusbar, BG');
  SetColor(apclScrollBack, $d0d0d0, 'ScrollBack', 'scrollbar, BG');
  SetColor(apclScrollRect, nColorBorder, 'ScrollRect', 'scrollbar, thumb border');
  SetColor(apclScrollFill, nColorBack, 'ScrollFill', 'scrollbar, thumb fill');
  SetColor(apclScrollArrow, nColorArrow, 'ScrollArrow', 'scrollbar, arrows');
  SetColor(apclScrollScrolled, $c0c0a0, 'ScrollScrolled', 'scrollbar, scrolling area');
  SetColor(apclSplitMain, nColorBack2, 'SplitMain', 'splitters, main');
  SetColor(apclSplitGroups, $d8d8d8, 'SplitGroups', 'splitters, groups');
  SetColor(apclExportHtmlBg, clWhite, 'ExportHtmlBg', 'export to html, BG');
  SetColor(apclExportHtmlNumbers, clMedGray, 'ExportHtmlNumbers', 'export to html, line numbers');
end;

procedure AppThemeInit_Syntax(var D: TAppTheme);
  //
  procedure SetStyle(AId: TAppThemeStyleId; const AName: string;
    AColorFont, AColorBg, AColorBorder: TColor;
    AFontStyle: TFontStyles;
    ABorderLeft, ABorderRight, ABorderUp, ABorderDown: TecBorderLineType;
    AFormatType: TecFormatType);
  var
    st: TecSyntaxFormat;
  begin
    if D.Styles[AId]=nil then
      D.Styles[AId]:= TecSyntaxFormat.Create(nil);
    st:= D.Styles[AId];

    st.DisplayName:= AName;
    st.Font.Color:= AColorFont;
    st.Font.Style:= AFontStyle;
    st.BgColor:= AColorBg;
    st.BorderColorLeft:= AColorBorder;
    st.BorderColorRight:= AColorBorder;
    st.BorderColorTop:= AColorBorder;
    st.BorderColorBottom:= AColorBorder;
    st.BorderTypeLeft:= ABorderLeft;
    st.BorderTypeRight:= ABorderRight;
    st.BorderTypeTop:= ABorderUp;
    st.BorderTypeBottom:= ABorderDown;
    st.FormatType:= AFormatType;
  end;
  //
begin
  SetStyle(apstId, 'Id', nColorText, clNone, clNone, [], blNone, blNone, blNone, blNone, ftFontAttr);
  SetStyle(apstId1, 'Id1', clNavy, clNone, clNone, [], blNone, blNone, blNone, blNone, ftFontAttr);
  SetStyle(apstId2, 'Id2', clPurple, clNone, clNone, [], blNone, blNone, blNone, blNone, ftFontAttr);
  SetStyle(apstId3, 'Id3', clOlive, clNone, clNone, [], blNone, blNone, blNone, blNone, ftFontAttr);
  SetStyle(apstId4, 'Id4', clBlue, clNone, clNone, [], blNone, blNone, blNone, blNone, ftFontAttr);
  SetStyle(apstIdKeyword, 'IdKeyword', clBlack, clNone, clNone, [fsBold], blNone, blNone, blNone, blNone, ftFontAttr);
  SetStyle(apstIdVar, 'IdVar', clGreen, clNone, clNone, [], blNone, blNone, blNone, blNone, ftFontAttr);
  SetStyle(apstIdBad, 'IdBad', clBlack, clNone, clRed, [], blNone, blNone, blNone, blSolid, ftFontAttr);

  SetStyle(apstString, 'String', clTeal, clNone, clNone, [], blNone, blNone, blNone, blNone, ftFontAttr);
  SetStyle(apstString2, 'String2', clOlive, clNone, clNone, [], blNone, blNone, blNone, blNone, ftFontAttr);
  SetStyle(apstString3, 'String3', $C8C040, clNone, clNone, [], blNone, blNone, blNone, blNone, ftFontAttr);

  SetStyle(apstSymbol, 'Symbol', clMaroon, clNone, clNone, [], blNone, blNone, blNone, blNone, ftFontAttr);
  SetStyle(apstSymbol2, 'Symbol2', $0000C0, clNone, clNone, [], blNone, blNone, blNone, blNone, ftFontAttr);
  SetStyle(apstSymbolBad, 'SymbolBad', clMaroon, clNone, clRed, [], blNone, blNone, blNone, blSolid, ftFontAttr);

  //don't use Italic for comments, coz comments often have Unicode
  SetStyle(apstComment, 'Comment', clGray, clNone, clNone, [], blNone, blNone, blNone, blNone, ftFontAttr);
  SetStyle(apstComment2, 'Comment2', $00C080, clNone, clNone, [], blNone, blNone, blNone, blNone, ftFontAttr);
  SetStyle(apstCommentDoc, 'CommentDoc', $809070, clNone, clNone, [], blNone, blNone, blNone, blNone, ftFontAttr);

  SetStyle(apstNumber, 'Number', clNavy, clNone, clNone, [fsBold], blNone, blNone, blNone, blNone, ftFontAttr);
  SetStyle(apstLabel, 'Label', $406090, clNone, clNone, [], blNone, blNone, blNone, blNone, ftFontAttr);
  SetStyle(apstColor, 'Color', $0080C0, clNone, clNone, [fsBold], blNone, blNone, blNone, blNone, ftFontAttr);

  SetStyle(apstIncludeBG1, 'IncludeBG1', clBlack, clMoneyGreen, clNone, [], blNone, blNone, blNone, blNone, ftBackGround);
  SetStyle(apstIncludeBG2, 'IncludeBG2', clBlack, $F0E0C0, clNone, [], blNone, blNone, blNone, blNone, ftBackGround);
  SetStyle(apstIncludeBG3, 'IncludeBG3', clBlack, $F0B0F0, clNone, [], blNone, blNone, blNone, blNone, ftBackGround);
  SetStyle(apstIncludeBG4, 'IncludeBG4', clBlack, $B0F0F0, clNone, [], blNone, blNone, blNone, blNone, ftBackGround);

  SetStyle(apstSectionBG1, 'SectionBG1', clBlack, clCream, clNone, [], blNone, blNone, blNone, blNone, ftBackGround);
  SetStyle(apstSectionBG2, 'SectionBG2', clBlack, $E0FFE0, clNone, [], blNone, blNone, blNone, blNone, ftBackGround);
  SetStyle(apstSectionBG3, 'SectionBG3', clBlack, $F0F0E0, clNone, [], blNone, blNone, blNone, blNone, ftBackGround);
  SetStyle(apstSectionBG4, 'SectionBG4', clBlack, $FFE0FF, clNone, [], blNone, blNone, blNone, blNone, ftBackGround);

  SetStyle(apstBracketBG, 'BracketBG', clBlack, clMoneyGreen, clGray, [], blSolid, blSolid, blSolid, blSolid, ftFontAttr);
  SetStyle(apstCurBlockBG, 'CurBlockBG', clBlack, $E8E8E8, clNone, [], blNone, blNone, blNone, blNone, ftBackGround);
  SetStyle(apstSeparLine, 'SeparLine', clBlack, $00E000, clNone, [], blNone, blNone, blNone, blNone, ftBackGround);

  SetStyle(apstTagBound, 'TagBound', clGray, clNone, clNone, [], blNone, blNone, blNone, blNone, ftFontAttr);
  SetStyle(apstTagId, 'TagId', $F06060, clNone, clNone, [], blNone, blNone, blNone, blNone, ftFontAttr);
  SetStyle(apstTagIdBad, 'TagIdBad', $F06060, clNone, clRed, [], blNone, blNone, blNone, blWavyLine, ftFontAttr);
  SetStyle(apstTagProp, 'TagProp', $40A040, clNone, clNone, [], blNone, blNone, blNone, blNone, ftFontAttr);
  SetStyle(apstTagPropBad, 'TagPropBad', $40A040, clNone, clRed, [], blNone, blNone, blNone, blWavyLine, ftFontAttr);
  SetStyle(apstTagInclude, 'TagInclude', clOlive, clNone, clNone, [fsBold], blNone, blNone, blNone, blNone, ftFontAttr);

  SetStyle(apstLightBG1, 'LightBG1', clBlack, $8080F0, clNone, [], blNone, blNone, blNone, blNone, ftFontAttr);
  SetStyle(apstLightBG2, 'LightBG2', clBlack, $60F0F0, clNone, [], blNone, blNone, blNone, blNone, ftFontAttr);
  SetStyle(apstLightBG3, 'LightBG3', clBlack, $80F080, clNone, [], blNone, blNone, blNone, blNone, ftFontAttr);
  SetStyle(apstLightBG4, 'LightBG4', clBlack, $F08080, clNone, [], blNone, blNone, blNone, blNone, ftFontAttr);
  SetStyle(apstLightBG5, 'LightBG5', clBlack, $C0C0B0, clNone, [], blNone, blNone, blNone, blNone, ftFontAttr);

  SetStyle(apstPale1, 'Pale1', $A0E0E0, clNone, clNone, [], blNone, blNone, blNone, blNone, ftFontAttr);
  SetStyle(apstPale2, 'Pale2', $E0E0A0, clNone, clNone, [], blNone, blNone, blNone, blNone, ftFontAttr);
  SetStyle(apstPale3, 'Pale3', $E0E0E0, clNone, clNone, [], blNone, blNone, blNone, blNone, ftFontAttr);

  SetStyle(apstTextBold, 'TextBold', clBlack, clNone, clNone, [fsBold], blNone, blNone, blNone, blNone, ftFontAttr);
  SetStyle(apstTextItalic, 'TextItalic', clBlack, clNone, clNone, [fsItalic], blNone, blNone, blNone, blNone, ftFontAttr);
  SetStyle(apstTextBoldItalic, 'TextBoldItalic', clBlack, clNone, clNone, [fsBold, fsItalic], blNone, blNone, blNone, blNone, ftFontAttr);
  SetStyle(apstTextCross, 'TextCross', clBlack, clNone, clNone, [fsStrikeOut], blNone, blNone, blNone, blNone, ftFontAttr);
  SetStyle(apstTextCrossBold, 'TextCrossBold', clBlack, clNone, clNone, [fsBold, fsStrikeOut], blNone, blNone, blNone, blNone, ftFontAttr);
  SetStyle(apstTextCrossItalic, 'TextCrossItalic', clBlack, clNone, clNone, [fsItalic, fsStrikeOut], blNone, blNone, blNone, blNone, ftFontAttr);
  SetStyle(apstTextCrossBoldItalic, 'TextCrossBoldItalic', clBlack, clNone, clNone, [fsBold, fsItalic, fsStrikeOut], blNone, blNone, blNone, blNone, ftFontAttr);
end;

procedure AppThemeSaveToFile(const AFileName: string; const D: TAppTheme; IsThemeUI: boolean);
var
  cfg: TJSONConfig;
  iColor: TAppThemeColorId;
  iStyle: TAppThemeStyleId;
  st: TecSyntaxFormat;
begin
  cfg:= TJSONConfig.Create(nil);
  try
    try
      cfg.Formatted:= true;
      cfg.Filename:= AFileName;
      cfg.Clear; //avoid file deletion, to support symlinks for files
    except
      on E: Exception do
      begin
        MsgBadConfig(AFileName, E.Message);
        exit;
      end;
    end;

    if IsThemeUI then
    begin
      for iColor:= Low(iColor) to High(iColor) do
        cfg.SetValue(D.Colors[iColor].name, TATHtmlColorParserA.ColorToHtmlString(D.Colors[iColor].color));
    end
    else
    begin
      for iStyle:= Low(iStyle) to apstLastStyle do
      begin
        st:= d.Styles[iStyle];
        Lexer_SaveStyleToFile_JsonTheme(st, cfg, 'Lex_'+st.DisplayName);
      end;
    end;
  finally
    cfg.Free;
  end;
end;

function GetAppColor(id: TAppThemeColorId): TColor;
begin
  if (id=apclEdSelFont) and EditorOps.OpKeepSelFontColor then
    Result:= clNone
  else
    Result:= AppTheme.Colors[id].Color;
end;

function GetAppStyle(id: TAppThemeStyleId): TecSyntaxFormat;
var
  styleId: TecSyntaxFormat;
begin
  Result:= AppTheme.Styles[id];

  if id in [apstTextBold..High(TAppThemeStyleId)] then
  begin
    styleId:= AppTheme.Styles[apstId];
    Result.Font.Color:= styleId.Font.Color;
  end;
end;

initialization

  FillChar(AppTheme, SizeOf(AppTheme), 0);
  AppThemeInit_UI(AppTheme);
  AppThemeInit_Syntax(AppTheme);

finalization

  AppThemeFree(AppTheme);

end.

