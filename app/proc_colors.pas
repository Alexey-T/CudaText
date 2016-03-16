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
  Theme: TAppTheme;

procedure DoInitTheme(var D: TAppTheme);
procedure DoLoadTheme(const fn: string; var D: TAppTheme);
procedure DoSaveTheme(const fn: string; const D: TAppTheme);
function GetAppColor(const name: string): TColor;

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
    s:= c.GetValue(id, '');
    if s='' then exit;
    Val:= SHtmlColorToColor(s, len, Val);
  end;
  //
var
  i: integer;
begin
  c:= TJsonConfig.Create(nil);
  try
    try
      c.Filename:= fn;
    except
      Showmessage('Incorrect theme file:'#13+fn);
      Exit
    end;

    for i:= Low(D.Colors) to High(D.Colors) do
      DoVal(D.Colors[i].color, D.Colors[i].name);
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

    if D.Styles=nil then
      D.Styles:= TList.Create;
    D.Styles.Add(st);
  end;
  //
begin
  SetLength(D.Colors, 0);

  Add(clBlack, 'EdTextFont', 'editor, font');
  Add(clWhite, 'EdTextBg', 'editor, BG');
  Add(clHighlightText, 'EdSelFont', 'editor, selection, font');
  Add(clHighlight, 'EdSelBg', 'editor, selection, BG');
  Add(clGray, 'EdDisableFont', 'editor, disabled state, font');
  Add($f0f0f0, 'EdDisableBg', 'editor, disabled state, BG');
  Add(clBlue, 'EdLinks', 'editor, links');
  Add($e0e0e0, 'EdLockedBg', 'editor, locked state, BG');
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
  Add($00f0f0, 'EdStateChanged', 'editor, line states, changed');
  Add($20c020, 'EdStateAdded', 'editor, line states, added');
  Add(clMedGray, 'EdStateSaved', 'editor, line states, saved');
  Add(clMedGray, 'EdBlockStaple', 'editor, block staples (indent guides)');
  Add(clGray, 'EdComboArrow', 'editor, combobox arrow-down');
  Add($f0f0f0, 'EdComboArrowBg', 'editor, combobox arrow-down BG');
  Add(clMedGray, 'EdBlockSepLine', 'editor, separator line');
  Add($a06060, 'EdFoldMarkLine', 'editor, folded line');
  Add($e08080, 'EdFoldMarkFont', 'editor, folded block mark, font');
  Add($e08080, 'EdFoldMarkBorder', 'editor, folded block mark, border');
  Add(clCream, 'EdFoldMarkBg', 'editor, folded block mark, BG');
  Add(clGray, 'EdGutterFont', 'editor, gutter font');
  Add($e0e0e0, 'EdGutterBg', 'editor, gutter BG');
  Add($c8c8c8, 'EdGutterCaretBg', 'editor, gutter BG, lines with carets');
  Add(clGray, 'EdRulerFont', 'editor, ruler font');
  Add($e0e0e0, 'EdRulerBg', 'editor, ruler BG');
  Add(clGray, 'EdFoldLine', 'editor, gutter folding, lines');
  Add($c8c8c8, 'EdFoldBg', 'editor, gutter folding, BG');
  Add(clGray, 'EdFoldPlusLine', 'editor, gutter folding, "plus" border');
  Add($f4f4f4, 'EdFoldPlusBg', 'editor, gutter folding, "plus" BG');
  Add(clLtGray, 'EdMarginFixed', 'editor, margin, fixed position');
  Add(clLime, 'EdMarginCaret', 'editor, margins, for carets');
  Add(clYellow, 'EdMarginUser', 'editor, margins, user defined');
  Add(clMoneyGreen, 'EdBookmarkBg', 'editor, bookmark, line BG');
  Add(clMedGray, 'EdBookmarkIcon', 'editor, bookmark, gutter mark');
  Add($f0e0b0, 'EdMarkedRangeBg', 'editor, marked range BG');

  Add($f0f0f0, 'TabBg', 'tabs, toolbar, statusbar BG');
  Add(clBlack, 'TabFont', 'tabs, font');
  Add($A00000, 'TabFontMod', 'tabs, font, modified tab');
  Add(clWhite, 'TabActive', 'tabs, active tab BG');
  Add($f0f0f0, 'TabActiveOthers', 'tabs, active tab BG, inactive groups');
  Add($e0e0e0, 'TabPassive', 'tabs, passive tab BG');
  Add($f0f0f0, 'TabOver', 'tabs, mouse-over tab BG');
  Add($c0c0c0, 'TabBorderActive', 'tabs, active tab border');
  Add($c0c0c0, 'TabBorderPassive', 'tabs, passive tab border');
  Add(clNone, 'TabCloseBg', 'tabs, close button BG');
  Add($6060c0, 'TabCloseBgOver', 'tabs, close button BG, mouse-over');
  Add($6060c0, 'TabCloseBorderOver', 'tabs, close button border');
  Add(clGray, 'TabCloseX', 'tabs, close x mark');
  Add(clWhite, 'TabCloseXOver', 'tabs, close x mark, mouse-over');
  Add(clGray, 'TabArrow', 'tabs, tab-list arrow-down');
  Add(clLtGray, 'TabArrowOver', 'tabs, tab-list arrow-down, mouse-over');

  Add(clBlack, 'TreeFont', 'syntax tree, font');
  Add(clWhite, 'TreeBg', 'syntax tree, BG');
  Add(clBlack, 'TreeSelFont', 'syntax tree, selected font');
  Add($e0e0e0, 'TreeSelBg', 'syntax tree, selected BG');
  Add(clLtGray, 'TreeLines', 'syntax tree, lines');
  Add(clBlack, 'TreeSign', 'syntax tree, fold sign');

  Add($e0e0e0, 'ListBg', 'listbox, BG');
  Add(clLtGray, 'ListSelBg', 'listbox, selected line BG');
  Add(clBlack, 'ListFont', 'listbox, font');
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

  Add(clCream, 'StatusAltBg', 'statusbar alt, BG');
  Add($e0e0e0, 'SplitMain', 'splitters, main');
  Add($e0e0e0, 'SplitGroups', 'splitters, groups');

  Add(clWhite, 'ExportHtmlBg', 'export to html, BG');
  Add(clMedGray, 'ExportHtmlNumbers', 'export to html, line numbers');

  //--------------
  AddStyle('Id', clBlack, 0, 0, [], blNone, blNone, blNone, blNone, ftFontAttr);
  AddStyle('IdKeyword', clBlack, 0, 0, [fsBold], blNone, blNone, blNone, blNone, ftFontAttr);
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
  for i:= Low(Theme.Colors) to High(Theme.Colors) do
    if Theme.Colors[i].name=name then
      begin Result:= Theme.Colors[i].color; exit end;
  raise Exception.Create('Incorrect color id: '+name);
end;


initialization
  DoInitTheme(Theme);

end.

