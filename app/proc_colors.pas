(*
This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at http://mozilla.org/MPL/2.0/.

Copyright (c) Alexey Torgashin
*)
unit proc_colors;

{$mode objfpc}{$H+}
{$ModeSwitch advancedrecords}
{$ScopedEnums on}

interface

uses
  Classes, SysUtils, Graphics,
  ATStringProc_HtmlColor,
  ec_syntax_format,
  proc_msg,
  proc_globdata,
  proc_lexer_styles;

type
  TAppThemeColor = (
    EdTextFont,
    EdTextBg,
    EdSelFont,
    EdSelBg,
    EdDisableFont,
    EdDisableBg,
    EdLinks,
    EdLockedBg,
    EdCaret,
    EdMarkers,
    EdCurLineBg,
    EdIndentVLine,
    EdUnprintFont,
    EdUnprintBg,
    EdUnprintHexFont,
    EdMinimapBorder,
    EdMinimapTooltipBg,
    EdMinimapTooltipBorder,
    EdMicromapBg,
    EdMicromapViewBg,
    EdMicromapOccur,
    EdMicromapSpell,
    EdStateChanged,
    EdStateAdded,
    EdStateSaved,
    EdBlockStaple,
    EdBlockStapleActive,
    EdComboArrow,
    EdComboArrowBg,
    EdBorder,
    EdBorderFocused,
    EdBlockSepLine,
    EdFoldMarkLine,
    EdFoldMarkFont,
    EdFoldMarkBorder,
    EdFoldMarkBg,
    EdGutterFont,
    EdGutterBg,
    EdGutterCaretFont,
    EdGutterCaretBg,
    EdRulerFont,
    EdRulerBg,
    EdFoldLine,
    EdFoldLine2,
    EdFoldBg,
    EdMarginFixed,
    EdMarginCaret,
    EdMarginUser,
    EdBookmarkBg,
    EdBookmarkIcon,
    EdMarkedRangeBg,
    TabBg,
    SideBg,
    SideBadgeBg,
    SideBadgeFont,
    TabFont,
    TabFontActive,
    TabFontMod,
    TabActive,
    TabActiveOthers,
    TabPassive,
    TabOver,
    TabBorderActive,
    TabBorderPassive,
    TabCloseBg,
    TabCloseBgOver,
    TabCloseBorderOver,
    TabCloseX,
    TabCloseXOver,
    TabArrow,
    TabArrowOver,
    TabActiveMark,
    TabMarks,
    TreeFont,
    TreeBg,
    TreeSelFont,
    TreeSelBg,
    TreeSelBg2,
    TreeSign,
    ListBg,
    ListSelBg,
    ListFont,
    ListSelFont,
    ListFontHotkey,
    ListFontHilite,
    ListCompletePrefix,
    ListCompleteParams,
    ButtonFont,
    ButtonFontDisabled,
    ButtonBgPassive,
    ButtonBgOver,
    ButtonBgChecked,
    ButtonBgDisabled,
    ButtonBorderPassive,
    ButtonBorderOver,
    ButtonBorderFocused,
    MenuFont,
    MenuFontHotkey,
    MenuFontDisabled,
    MenuBg,
    MenuSelBg,
    StatusFont,
    StatusBg,
    ScrollBack,
    ScrollRect,
    ScrollFill,
    ScrollArrow,
    ScrollScrolled,
    SplitMain,
    SplitGroups,
    ExportHtmlBg,
    ExportHtmlNumbers
    );

const
  cAppThemeColorsWhichAllowNone: set of TAppThemeColor =
    [
    TAppThemeColor.EdBlockStapleActive,
    TAppThemeColor.TabFontActive,
    TAppThemeColor.TabCloseBg,
    TAppThemeColor.MenuFont..TAppThemeColor.MenuSelBg,
    TAppThemeColor.StatusFont,
    TAppThemeColor.StatusBg
    ];

type
  TAppThemeStyle = (
    Id,
    Id1,
    Id2,
    Id3,
    Id4,
    IdKeyword,
    IdVar,
    IdBad,
    String_,
    String2,
    String3,
    Symbol,
    Symbol2,
    SymbolBad,
    Comment,
    Comment2,
    CommentDoc,
    Number,
    Label_,
    Color,
    IncludeBG1,
    IncludeBG2,
    IncludeBG3,
    IncludeBG4,
    SectionBG1,
    SectionBG2,
    SectionBG3,
    SectionBG4,
    BracketBG,
    CurBlockBG,
    SeparLine,
    TagBound,
    TagId,
    TagIdBad,
    TagProp,
    TagPropBad,
    TagInclude,
    LightBG1,
    LightBG2,
    LightBG3,
    LightBG4,
    LightBG5,
    Pale1,
    Pale2,
    Pale3,
    //styles below must not be saved to file, see apstLastStyle
    TextBold,
    TextItalic,
    TextBoldItalic,
    TextCross,
    TextCrossBold,
    TextCrossItalic,
    TextCrossBoldItalic
    );

const
  //saving theme to file considers it, to skip last items 'bold'/'italic'/etc
  apstLastStyle = Pred(TAppThemeStyle.TextBold);

type
  TAppThemeColorRec = record
    color: TColor;
    name, desc: string;
  end;

  TAppTheme = record
    Colors: array[TAppThemeColor] of TAppThemeColorRec;
    Styles: array[TAppThemeStyle] of TecSyntaxFormat;
    //trailing styles (bold, italic, bold+italic, etc) must be synced with 'Id' style
    procedure UpdateBoldAndItalicColors;
  end;

var
  AppTheme: TAppTheme;
  AppHiAll_ThemeStyleId: TAppThemeStyle = TAppThemeStyle.SeparLine;

procedure AppThemeInit_UI(var D: TAppTheme);
procedure AppThemeInit_Syntax(var D: TAppTheme);
procedure AppThemeFree(var D: TAppTheme);
procedure AppThemeLoadFromFile(const AFileName: string; var D: TAppTheme; IsThemeUI: boolean);
procedure AppThemeSaveToFile(const AFileName: string; const D: TAppTheme; IsThemeUI: boolean);

function GetAppColor(id: TAppThemeColor): TColor;
function GetAppStyle(id: TAppThemeStyle): TecSyntaxFormat; inline;
function FindAppColorByName(const AName: string; ADefaultColor: TColor): TColor;

implementation

uses
  ATButtons,
  at__jsonconf;

procedure AppThemeFree(var D: TAppTheme);
var
  St: TecSyntaxFormat;
  id: TAppThemeStyle;
begin
  for id:= High(TAppThemeStyle) downto Low(TAppThemeStyle) do
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
  iColor: TAppThemeColor;
  iStyle: TAppThemeStyle;
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
        st:= D.Styles[iStyle];
        if not Lexer_LoadStyleFromFile_JsonTheme(st, cfg, 'Lex_'+st.DisplayName) then
          MsgLogConsole(Format(msgErrorInTheme,
            [ExtractFileName(AFileName), 'Lex_'+st.DisplayName]));
      end;
      D.UpdateBoldAndItalicColors;
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
  nColorListBack = $b4d8a8;
  nColorListSelBack = $d0d0d0;
  nColorListSelBack2 = $f4f4f4;

procedure AppThemeInit_UI(var D: TAppTheme);
  //
  procedure SetColor(AId: TAppThemeColor; AColor: TColor; const AName, ADesc: string); inline;
  begin
    D.Colors[AId].color:= AColor;
    D.Colors[AId].name:= AName;
    D.Colors[AId].desc:= ADesc;
  end;
  //
begin
  SetColor(TAppThemeColor.EdTextFont, nColorText, 'EdTextFont', 'editor, font');
  SetColor(TAppThemeColor.EdTextBg, nColorBack, 'EdTextBg', 'editor, BG');
  SetColor(TAppThemeColor.EdSelFont, nColorText, 'EdSelFont', 'editor, selection, font');
  SetColor(TAppThemeColor.EdSelBg, $ffc0a0, 'EdSelBg', 'editor, selection, BG');
  SetColor(TAppThemeColor.EdDisableFont, nColorText, 'EdDisableFont', 'editor, disabled state, font');
  SetColor(TAppThemeColor.EdDisableBg, $e0e0e0, 'EdDisableBg', 'editor, disabled state, BG');
  SetColor(TAppThemeColor.EdLinks, $c05050, 'EdLinks', 'editor, links');
  SetColor(TAppThemeColor.EdLockedBg, nColorGutterBack, 'EdLockedBg', 'editor, locked state, BG');
  SetColor(TAppThemeColor.EdCaret, clBlack, 'EdCaret', 'editor, caret');
  SetColor(TAppThemeColor.EdMarkers, $6060d0, 'EdMarkers', 'editor, markers');
  SetColor(TAppThemeColor.EdCurLineBg, $e0e0d0, 'EdCurLineBg', 'editor, current line BG');
  SetColor(TAppThemeColor.EdIndentVLine, clMedGray, 'EdIndentVLine', 'editor, wrapped line indent vert-lines');
  SetColor(TAppThemeColor.EdUnprintFont, $a0a0b8, 'EdUnprintFont', 'editor, unprinted chars, font');
  SetColor(TAppThemeColor.EdUnprintBg, $e0e0e0, 'EdUnprintBg', 'editor, unprinted chars, BG');
  SetColor(TAppThemeColor.EdUnprintHexFont, clMedGray, 'EdUnprintHexFont', 'editor, special hex codes, font');
  SetColor(TAppThemeColor.EdMinimapBorder, $b09090, 'EdMinimapBorder', 'editor, minimap, border');
  SetColor(TAppThemeColor.EdMinimapTooltipBg, clMoneyGreen, 'EdMinimapTooltipBg', 'editor, minimap, tooltip BG');
  SetColor(TAppThemeColor.EdMinimapTooltipBorder, clMedGray, 'EdMinimapTooltipBorder', 'editor, minimap, tooltip border');
  SetColor(TAppThemeColor.EdMicromapBg, $e0e0e0, 'EdMicromapBg', 'editor, micromap, BG');
  SetColor(TAppThemeColor.EdMicromapViewBg, $d0d0d0, 'EdMicromapViewBg', 'editor, micromap, current view area');
  SetColor(TAppThemeColor.EdMicromapOccur, $c05050, 'EdMicromapOccur', 'editor, micromap, word occurrences');
  SetColor(TAppThemeColor.EdMicromapSpell, $6060d0, 'EdMicromapSpell', 'editor, micromap, misspelled marks');
  SetColor(TAppThemeColor.EdStateChanged, $70b0b0, 'EdStateChanged', 'editor, line states, changed');
  SetColor(TAppThemeColor.EdStateAdded, $80a080, 'EdStateAdded', 'editor, line states, added');
  SetColor(TAppThemeColor.EdStateSaved, clMedGray, 'EdStateSaved', 'editor, line states, saved');
  SetColor(TAppThemeColor.EdBlockStaple, $b0b0b0, 'EdBlockStaple', 'editor, block staples (indent guides)');
  SetColor(TAppThemeColor.EdBlockStapleActive, clNone, 'EdBlockStapleActive', 'editor, block staples, for caret');
  SetColor(TAppThemeColor.EdComboArrow, nColorArrow, 'EdComboArrow', 'editor, combobox arrow-down');
  SetColor(TAppThemeColor.EdComboArrowBg, nColorBack, 'EdComboArrowBg', 'editor, combobox arrow-down BG');
  SetColor(TAppThemeColor.EdBorder, nColorBorder, 'EdBorder', 'editor, border');
  SetColor(TAppThemeColor.EdBorderFocused, clNavy, 'EdBorderFocused', 'editor, border, focused');
  SetColor(TAppThemeColor.EdBlockSepLine, clMedGray, 'EdBlockSepLine', 'editor, separator line');
  SetColor(TAppThemeColor.EdFoldMarkLine, $a06060, 'EdFoldMarkLine', 'editor, horizontal folding line — — —');
  SetColor(TAppThemeColor.EdFoldMarkFont, $e08080, 'EdFoldMarkFont', 'editor, folded block mark, font');
  SetColor(TAppThemeColor.EdFoldMarkBorder, $e08080, 'EdFoldMarkBorder', 'editor, folded block mark, border');
  SetColor(TAppThemeColor.EdFoldMarkBg, nColorBack, 'EdFoldMarkBg', 'editor, folded block mark, BG');
  SetColor(TAppThemeColor.EdGutterFont, nColorGutterFont, 'EdGutterFont', 'editor, gutter font');
  SetColor(TAppThemeColor.EdGutterBg, nColorGutterBack, 'EdGutterBg', 'editor, gutter BG');
  SetColor(TAppThemeColor.EdGutterCaretFont, nColorGutterFont, 'EdGutterCaretFont', 'editor, gutter font, lines with carets');
  SetColor(TAppThemeColor.EdGutterCaretBg, $a0d0d0, 'EdGutterCaretBg', 'editor, gutter BG, lines with carets');
  SetColor(TAppThemeColor.EdRulerFont, nColorGutterFont, 'EdRulerFont', 'editor, ruler font');
  SetColor(TAppThemeColor.EdRulerBg, nColorBack, 'EdRulerBg', 'editor, ruler BG');
  SetColor(TAppThemeColor.EdFoldLine, nColorGutterFont, 'EdFoldLine', 'editor, gutter folding, lines');
  SetColor(TAppThemeColor.EdFoldLine2, $a04040, 'EdFoldLine2', 'editor, gutter folding, lines, current range');
  SetColor(TAppThemeColor.EdFoldBg, nColorGutterBack, 'EdFoldBg', 'editor, gutter folding, BG');
  SetColor(TAppThemeColor.EdMarginFixed, clLtGray, 'EdMarginFixed', 'editor, margin, fixed position');
  SetColor(TAppThemeColor.EdMarginCaret, $b0c0c0, 'EdMarginCaret', 'editor, margins, for carets');
  SetColor(TAppThemeColor.EdMarginUser, $b0c0c0, 'EdMarginUser', 'editor, margins, user defined');
  SetColor(TAppThemeColor.EdBookmarkBg, clMoneyGreen, 'EdBookmarkBg', 'editor, bookmark, line BG');
  SetColor(TAppThemeColor.EdBookmarkIcon, clMedGray, 'EdBookmarkIcon', 'editor, bookmark, gutter mark');
  SetColor(TAppThemeColor.EdMarkedRangeBg, $f0e0b0, 'EdMarkedRangeBg', 'editor, marked range BG / info panels BG');
  SetColor(TAppThemeColor.TabBg, nColorBack2, 'TabBg', 'main-toolbar, tabs BG');
  SetColor(TAppThemeColor.SideBg, $808080, 'SideBg', 'side-toolbar BG');
  SetColor(TAppThemeColor.SideBadgeBg, clNavy, 'SideBadgeBg', 'side-toolbar, button badges BG');
  SetColor(TAppThemeColor.SideBadgeFont, clWhite, 'SideBadgeFont', 'side-toolbar, button badges font');
  SetColor(TAppThemeColor.TabFont, nColorText, 'TabFont', 'tabs, font');
  SetColor(TAppThemeColor.TabFontActive, clNone, 'TabFontActive', 'tabs, font, active tab');
  SetColor(TAppThemeColor.TabFontMod, $a00000, 'TabFontMod', 'tabs, font, modified tab');
  SetColor(TAppThemeColor.TabActive, nColorBack, 'TabActive', 'tabs, active tab BG');
  SetColor(TAppThemeColor.TabActiveOthers, $e4d0d0, 'TabActiveOthers', 'tabs, active tab BG, inactive groups');
  SetColor(TAppThemeColor.TabPassive, nColorBack2+$0a0a0a, 'TabPassive', 'tabs, passive tab BG');
  SetColor(TAppThemeColor.TabOver, $ffffff, 'TabOver', 'tabs, mouse-over tab BG');
  SetColor(TAppThemeColor.TabBorderActive, nColorBorder, 'TabBorderActive', 'tabs, active tab border');
  SetColor(TAppThemeColor.TabBorderPassive, nColorBorder, 'TabBorderPassive', 'tabs, passive tab border');
  SetColor(TAppThemeColor.TabCloseBg, clNone, 'TabCloseBg', 'tabs, close button BG');
  SetColor(TAppThemeColor.TabCloseBgOver, $9090c0, 'TabCloseBgOver', 'tabs, close button BG, mouse-over');
  SetColor(TAppThemeColor.TabCloseBorderOver, $9090c0, 'TabCloseBorderOver', 'tabs, close button border');
  SetColor(TAppThemeColor.TabCloseX, nColorArrow, 'TabCloseX', 'tabs, close x mark');
  SetColor(TAppThemeColor.TabCloseXOver, nColorBack, 'TabCloseXOver', 'tabs, close x mark, mouse-over');
  SetColor(TAppThemeColor.TabArrow, nColorArrow, 'TabArrow', 'tabs, triangle arrows');
  SetColor(TAppThemeColor.TabArrowOver, $404040, 'TabArrowOver', 'tabs, triangle arrows, mouse-over');
  SetColor(TAppThemeColor.TabActiveMark, clMedGray, 'TabActiveMark', 'tabs, flat style, active tab mark');
  SetColor(TAppThemeColor.TabMarks, $6060E0, 'TabMarks', 'tabs, special marks');
  SetColor(TAppThemeColor.TreeFont, nColorText, 'TreeFont', 'treeview, font');
  SetColor(TAppThemeColor.TreeBg, nColorBack, 'TreeBg', 'treeview, BG');
  SetColor(TAppThemeColor.TreeSelFont, nColorText, 'TreeSelFont', 'treeview, selected font');
  SetColor(TAppThemeColor.TreeSelBg, nColorListSelBack, 'TreeSelBg', 'treeview, selected BG');
  SetColor(TAppThemeColor.TreeSelBg2, nColorListSelBack2, 'TreeSelBg2', 'treeview, selected BG, not focused');
  SetColor(TAppThemeColor.TreeSign, nColorGutterFont, 'TreeSign', 'treeview, folding sign');
  SetColor(TAppThemeColor.ListBg, nColorListBack, 'ListBg', 'listbox, BG');
  SetColor(TAppThemeColor.ListSelBg, nColorListSelBack, 'ListSelBg', 'listbox, selected line BG');
  SetColor(TAppThemeColor.ListFont, nColorText, 'ListFont', 'listbox, font');
  SetColor(TAppThemeColor.ListSelFont, nColorText, 'ListSelFont', 'listbox, selected line font');
  SetColor(TAppThemeColor.ListFontHotkey, $676767, 'ListFontHotkey', 'listbox, font, hotkey');
  SetColor(TAppThemeColor.ListFontHilite, $007000, 'ListFontHilite', 'listbox, font, search chars');
  SetColor(TAppThemeColor.ListCompletePrefix, $c05050, 'ListCompletePrefix', 'listbox, font, auto-complete prefix');
  SetColor(TAppThemeColor.ListCompleteParams, clGray, 'ListCompleteParams', 'listbox, font, auto-complete params');
  SetColor(TAppThemeColor.ButtonFont, nColorText, 'ButtonFont', 'buttons, font');
  SetColor(TAppThemeColor.ButtonFontDisabled, $808088, 'ButtonFontDisabled', 'buttons, font, disabled state');
  SetColor(TAppThemeColor.ButtonBgPassive, nColorBack, 'ButtonBgPassive', 'buttons, BG, passive');
  SetColor(TAppThemeColor.ButtonBgOver, $d0b0b0, 'ButtonBgOver', 'buttons, BG, mouse-over');
  SetColor(TAppThemeColor.ButtonBgChecked, $b0b0b0, 'ButtonBgChecked', 'buttons, BG, checked state');
  SetColor(TAppThemeColor.ButtonBgDisabled, $c0c0d0, 'ButtonBgDisabled', 'buttons, BG, disabled state');
  SetColor(TAppThemeColor.ButtonBorderPassive, nColorBorder, 'ButtonBorderPassive', 'buttons, border, passive');
  SetColor(TAppThemeColor.ButtonBorderOver, nColorBorder, 'ButtonBorderOver', 'buttons, border, mouse-over');
  SetColor(TAppThemeColor.ButtonBorderFocused, clGray, 'ButtonBorderFocused', 'buttons, border, focused');
  SetColor(TAppThemeColor.MenuFont, clNone, 'MenuFont', 'top menu, font');
  SetColor(TAppThemeColor.MenuFontHotkey, clNone, 'MenuFontHotkey', 'top menu, font, hotkey');
  SetColor(TAppThemeColor.MenuFontDisabled, clNone, 'MenuFontDisabled', 'top menu, font, disabled state');
  SetColor(TAppThemeColor.MenuBg, clNone, 'MenuBg', 'top menu, BG');
  SetColor(TAppThemeColor.MenuSelBg, clNone, 'MenuSelBg', 'top menu, BG, selected');
  SetColor(TAppThemeColor.StatusFont, clNone, 'StatusFont', 'statusbar, font');
  SetColor(TAppThemeColor.StatusBg, clNone, 'StatusBg', 'statusbar, BG');
  SetColor(TAppThemeColor.ScrollBack, $d0d0d0, 'ScrollBack', 'scrollbar, BG');
  SetColor(TAppThemeColor.ScrollRect, nColorBorder, 'ScrollRect', 'scrollbar, thumb border');
  SetColor(TAppThemeColor.ScrollFill, nColorBack, 'ScrollFill', 'scrollbar, thumb fill');
  SetColor(TAppThemeColor.ScrollArrow, nColorArrow, 'ScrollArrow', 'scrollbar, arrows');
  SetColor(TAppThemeColor.ScrollScrolled, $c0c0a0, 'ScrollScrolled', 'scrollbar, scrolling area');
  SetColor(TAppThemeColor.SplitMain, nColorBack2, 'SplitMain', 'splitters, main');
  SetColor(TAppThemeColor.SplitGroups, $d8d8d8, 'SplitGroups', 'splitters, groups');
  SetColor(TAppThemeColor.ExportHtmlBg, clWhite, 'ExportHtmlBg', 'export to html, BG');
  SetColor(TAppThemeColor.ExportHtmlNumbers, clMedGray, 'ExportHtmlNumbers', 'export to html, line numbers');
end;

procedure AppThemeInit_Syntax(var D: TAppTheme);
  //
  procedure SetStyle(AId: TAppThemeStyle; const AName: string;
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
  SetStyle(TAppThemeStyle.Id, 'Id', nColorText, clNone, clNone, [], blNone, blNone, blNone, blNone, ftFontAttr);
  SetStyle(TAppThemeStyle.Id1, 'Id1', clNavy, clNone, clNone, [], blNone, blNone, blNone, blNone, ftFontAttr);
  SetStyle(TAppThemeStyle.Id2, 'Id2', clPurple, clNone, clNone, [], blNone, blNone, blNone, blNone, ftFontAttr);
  SetStyle(TAppThemeStyle.Id3, 'Id3', clOlive, clNone, clNone, [], blNone, blNone, blNone, blNone, ftFontAttr);
  SetStyle(TAppThemeStyle.Id4, 'Id4', clBlue, clNone, clNone, [], blNone, blNone, blNone, blNone, ftFontAttr);
  SetStyle(TAppThemeStyle.IdKeyword, 'IdKeyword', clBlack, clNone, clNone, [fsBold], blNone, blNone, blNone, blNone, ftFontAttr);
  SetStyle(TAppThemeStyle.IdVar, 'IdVar', clGreen, clNone, clNone, [], blNone, blNone, blNone, blNone, ftFontAttr);
  SetStyle(TAppThemeStyle.IdBad, 'IdBad', clBlack, clNone, clRed, [], blNone, blNone, blNone, blSolid, ftFontAttr);

  SetStyle(TAppThemeStyle.String_, 'String', clTeal, clNone, clNone, [], blNone, blNone, blNone, blNone, ftFontAttr);
  SetStyle(TAppThemeStyle.String2, 'String2', clOlive, clNone, clNone, [], blNone, blNone, blNone, blNone, ftFontAttr);
  SetStyle(TAppThemeStyle.String3, 'String3', $C8C040, clNone, clNone, [], blNone, blNone, blNone, blNone, ftFontAttr);

  SetStyle(TAppThemeStyle.Symbol, 'Symbol', clMaroon, clNone, clNone, [], blNone, blNone, blNone, blNone, ftFontAttr);
  SetStyle(TAppThemeStyle.Symbol2, 'Symbol2', $0000C0, clNone, clNone, [], blNone, blNone, blNone, blNone, ftFontAttr);
  SetStyle(TAppThemeStyle.SymbolBad, 'SymbolBad', clMaroon, clNone, clRed, [], blNone, blNone, blNone, blSolid, ftFontAttr);

  //don't use Italic for comments, coz comments often have Unicode
  SetStyle(TAppThemeStyle.Comment, 'Comment', clGray, clNone, clNone, [], blNone, blNone, blNone, blNone, ftFontAttr);
  SetStyle(TAppThemeStyle.Comment2, 'Comment2', $00C080, clNone, clNone, [], blNone, blNone, blNone, blNone, ftFontAttr);
  SetStyle(TAppThemeStyle.CommentDoc, 'CommentDoc', $809070, clNone, clNone, [], blNone, blNone, blNone, blNone, ftFontAttr);

  SetStyle(TAppThemeStyle.Number, 'Number', clNavy, clNone, clNone, [fsBold], blNone, blNone, blNone, blNone, ftFontAttr);
  SetStyle(TAppThemeStyle.Label_, 'Label', $406090, clNone, clNone, [], blNone, blNone, blNone, blNone, ftFontAttr);
  SetStyle(TAppThemeStyle.Color, 'Color', $0080C0, clNone, clNone, [fsBold], blNone, blNone, blNone, blNone, ftFontAttr);

  SetStyle(TAppThemeStyle.IncludeBG1, 'IncludeBG1', clBlack, clMoneyGreen, clNone, [], blNone, blNone, blNone, blNone, ftBackGround);
  SetStyle(TAppThemeStyle.IncludeBG2, 'IncludeBG2', clBlack, $F0E0C0, clNone, [], blNone, blNone, blNone, blNone, ftBackGround);
  SetStyle(TAppThemeStyle.IncludeBG3, 'IncludeBG3', clBlack, $F0B0F0, clNone, [], blNone, blNone, blNone, blNone, ftBackGround);
  SetStyle(TAppThemeStyle.IncludeBG4, 'IncludeBG4', clBlack, $B0F0F0, clNone, [], blNone, blNone, blNone, blNone, ftBackGround);

  SetStyle(TAppThemeStyle.SectionBG1, 'SectionBG1', clBlack, clCream, clNone, [], blNone, blNone, blNone, blNone, ftBackGround);
  SetStyle(TAppThemeStyle.SectionBG2, 'SectionBG2', clBlack, $E0FFE0, clNone, [], blNone, blNone, blNone, blNone, ftBackGround);
  SetStyle(TAppThemeStyle.SectionBG3, 'SectionBG3', clBlack, $F0F0E0, clNone, [], blNone, blNone, blNone, blNone, ftBackGround);
  SetStyle(TAppThemeStyle.SectionBG4, 'SectionBG4', clBlack, $FFE0FF, clNone, [], blNone, blNone, blNone, blNone, ftBackGround);

  SetStyle(TAppThemeStyle.BracketBG, 'BracketBG', clBlack, clMoneyGreen, clGray, [], blSolid, blSolid, blSolid, blSolid, ftFontAttr);
  SetStyle(TAppThemeStyle.CurBlockBG, 'CurBlockBG', clBlack, $E8E8E8, clNone, [], blNone, blNone, blNone, blNone, ftBackGround);
  SetStyle(TAppThemeStyle.SeparLine, 'SeparLine', clBlack, $00E000, clNone, [], blNone, blNone, blNone, blNone, ftBackGround);

  SetStyle(TAppThemeStyle.TagBound, 'TagBound', clGray, clNone, clNone, [], blNone, blNone, blNone, blNone, ftFontAttr);
  SetStyle(TAppThemeStyle.TagId, 'TagId', $F06060, clNone, clNone, [], blNone, blNone, blNone, blNone, ftFontAttr);
  SetStyle(TAppThemeStyle.TagIdBad, 'TagIdBad', $F06060, clNone, clRed, [], blNone, blNone, blNone, blWavyLine, ftFontAttr);
  SetStyle(TAppThemeStyle.TagProp, 'TagProp', $40A040, clNone, clNone, [], blNone, blNone, blNone, blNone, ftFontAttr);
  SetStyle(TAppThemeStyle.TagPropBad, 'TagPropBad', $40A040, clNone, clRed, [], blNone, blNone, blNone, blWavyLine, ftFontAttr);
  SetStyle(TAppThemeStyle.TagInclude, 'TagInclude', clOlive, clNone, clNone, [fsBold], blNone, blNone, blNone, blNone, ftFontAttr);

  SetStyle(TAppThemeStyle.LightBG1, 'LightBG1', clBlack, $8080F0, clNone, [], blNone, blNone, blNone, blNone, ftFontAttr);
  SetStyle(TAppThemeStyle.LightBG2, 'LightBG2', clBlack, $60F0F0, clNone, [], blNone, blNone, blNone, blNone, ftFontAttr);
  SetStyle(TAppThemeStyle.LightBG3, 'LightBG3', clBlack, $80F080, clNone, [], blNone, blNone, blNone, blNone, ftFontAttr);
  SetStyle(TAppThemeStyle.LightBG4, 'LightBG4', clBlack, $F08080, clNone, [], blNone, blNone, blNone, blNone, ftFontAttr);
  SetStyle(TAppThemeStyle.LightBG5, 'LightBG5', clBlack, $C0C0B0, clNone, [], blNone, blNone, blNone, blNone, ftFontAttr);

  SetStyle(TAppThemeStyle.Pale1, 'Pale1', $A0E0E0, clNone, clNone, [], blNone, blNone, blNone, blNone, ftFontAttr);
  SetStyle(TAppThemeStyle.Pale2, 'Pale2', $E0E0A0, clNone, clNone, [], blNone, blNone, blNone, blNone, ftFontAttr);
  SetStyle(TAppThemeStyle.Pale3, 'Pale3', $E0E0E0, clNone, clNone, [], blNone, blNone, blNone, blNone, ftFontAttr);

  SetStyle(TAppThemeStyle.TextBold, 'TextBold', clBlack, clNone, clNone, [fsBold], blNone, blNone, blNone, blNone, ftFontAttr);
  SetStyle(TAppThemeStyle.TextItalic, 'TextItalic', clBlack, clNone, clNone, [fsItalic], blNone, blNone, blNone, blNone, ftFontAttr);
  SetStyle(TAppThemeStyle.TextBoldItalic, 'TextBoldItalic', clBlack, clNone, clNone, [fsBold, fsItalic], blNone, blNone, blNone, blNone, ftFontAttr);
  SetStyle(TAppThemeStyle.TextCross, 'TextCross', clBlack, clNone, clNone, [fsStrikeOut], blNone, blNone, blNone, blNone, ftFontAttr);
  SetStyle(TAppThemeStyle.TextCrossBold, 'TextCrossBold', clBlack, clNone, clNone, [fsBold, fsStrikeOut], blNone, blNone, blNone, blNone, ftFontAttr);
  SetStyle(TAppThemeStyle.TextCrossItalic, 'TextCrossItalic', clBlack, clNone, clNone, [fsItalic, fsStrikeOut], blNone, blNone, blNone, blNone, ftFontAttr);
  SetStyle(TAppThemeStyle.TextCrossBoldItalic, 'TextCrossBoldItalic', clBlack, clNone, clNone, [fsBold, fsItalic, fsStrikeOut], blNone, blNone, blNone, blNone, ftFontAttr);
end;

procedure AppThemeSaveToFile(const AFileName: string; const D: TAppTheme; IsThemeUI: boolean);
var
  cfg: TJSONConfig;
  iColor: TAppThemeColor;
  iStyle: TAppThemeStyle;
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

function GetAppColor(id: TAppThemeColor): TColor;
begin
  if (id=TAppThemeColor.EdSelFont) and EditorOps.OpKeepSelFontColor then
    Result:= clNone
  else
    Result:= AppTheme.Colors[id].Color;
end;

function GetAppStyle(id: TAppThemeStyle): TecSyntaxFormat; inline;
begin
  Result:= AppTheme.Styles[id];
end;

procedure TAppTheme.UpdateBoldAndItalicColors;
var
  ColorOfId: TColor;
  iStyle: TAppThemeStyle;
begin
  ColorOfId:= Styles[TAppThemeStyle.Id].Font.Color;
  for iStyle:= Succ(apstLastStyle) to High(iStyle) do
    Styles[iStyle].Font.Color:= ColorOfId;
end;

function FindAppColorByName(const AName: string; ADefaultColor: TColor): TColor;
var
  iColor: TAppThemeColor;
begin
  Result:= ADefaultColor;
  for iColor:= Low(iColor) to High(iColor) do
    if SameText(AppTheme.Colors[iColor].Name, AName) then
    begin
      Result:= AppTheme.Colors[iColor].Color;
      exit;
    end;
end;

initialization

  FillChar(AppTheme, SizeOf(AppTheme), 0);
  AppThemeInit_UI(AppTheme);
  AppThemeInit_Syntax(AppTheme);

finalization

  AppThemeFree(AppTheme);

end.

