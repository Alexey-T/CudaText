(*
This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at http://mozilla.org/MPL/2.0/.

Copyright (c) Alexey Torgashin
*)
unit proc_cmd;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  ATSynEdit_Keymap;

procedure InitKeymapApp(M: TATKeymap);

const
  cmd_First = 2500;
  cmd_FileNew            = 2500;
  cmd_FileOpen           = 2501;
  cmd_FileSave           = 2502;
  cmd_FileSaveAs         = 2503;
  cmd_FileSaveAll        = 2504;
  cmd_FileReopen         = 2505;
  cmd_FileExit           = 2506;
  cmd_FileClose          = 2510;
  cmd_FileCloseOtherThis = 2511;
  cmd_FileCloseOtherAll  = 2512;
  cmd_FileCloseAll       = 2513;
  cmd_FileCloseAndDelete = 2514;
  cmd_FileExportHtml     = 2515;

  cmd_OpsClearRecent     = 2520;
  cmd_OpsOpenDefault     = 2521;
  cmd_OpsOpenUser        = 2522;
  cmd_OpsOpenLexerOvr    = 2523;
  cmd_OpsOpenFileTypes   = 2524;
  cmd_OpsFontText        = 2525;
  cmd_OpsFontUi          = 2526;
  cmd_DlgSaveTabs        = 2527;
  cmd_ToggleFullScreen   = 2528;
  cmd_OpsReloadAndApply  = 2529;
  cmd_DlgLexerProp       = 2530;
  cmd_DlgLexerLib        = 2531;
  cmd_DlgColors          = 2532;
  cmd_ToggleSidePanel    = 2533;
  cmd_ToggleBottomPanel  = 2534;
  cmd_ShowPanelConsole   = 2535;
  cmd_ShowPanelOutput    = 2536;
  cmd_ShowPanelValidate  = 2537;
  cmd_ToggleFindDialog   = 2538;

  cmd_DlgGoto       = 2580;
  cmd_DlgGotoBm     = 2581;
  cmd_DlgCommands   = 2582;
  cmd_DlgTabs       = 2583;
  cmd_DlgFind       = 2584;
  cmd_DlgReplace    = 2585;

  cmd_FindNext         = 2590;
  cmd_FindPrev         = 2591;
  cmd_FindCurWordNext  = 2592;
  cmd_FindCurWordPrev  = 2593;
  cmd_FindCurSelNext   = 2594;
  cmd_FindCurSelPrev   = 2595;

  cmd_SplitTabToggle   = 2620;
  cmd_SplitTabHorzVert = 2621;
  cmd_SplitTab3070     = 2622;
  cmd_SplitTab4060     = 2623;
  cmd_SplitTab5050     = 2624;
  cmd_SplitTab6040     = 2625;
  cmd_SplitTab7030     = 2626;

  cmd_Groups1     = 2630;
  cmd_Groups2horz = 2631;
  cmd_Groups2vert = 2632;
  cmd_Groups3horz = 2633;
  cmd_Groups3vert = 2634;
  cmd_Groups3plus = 2635;
  cmd_Groups4horz = 2636;
  cmd_Groups4vert = 2637;
  cmd_Groups4grid = 2638;
  cmd_Groups6grid = 2639;

  cmd_GroupActivateNext = 2640;
  cmd_GroupActivatePrev = 2641;

  cmd_MoveTabToGroupNext = 2642;
  cmd_MoveTabToGroupPrev = 2643;

  cmd_CopyLine         = 2650;
  cmd_CopyFilenameFull = 2651;
  cmd_CopyFilenameDir  = 2652;
  cmd_CopyFilenameName = 2653;
  cmd_SortAsc          = 2654;
  cmd_SortDesc         = 2655;
  cmd_SortNocaseAsc    = 2656;
  cmd_SortNocaseDesc   = 2657;

  cmd_BookmarkToggle    = 2661;
  cmd_BookmarkInvertAll = 2662;
  cmd_BookmarkClearAll  = 2663;
  cmd_BookmarkGotoNext  = 2664;
  cmd_BookmarkGotoPrev  = 2665;

  cmd_CommentLineAdd    = 2670;
  cmd_CommentLineRemove = 2671;
  cmd_CommentLineToggle = 2672;

  cmd_LineEndWin        = 2677;
  cmd_LineEndUnix       = 2678;
  cmd_LineEndMac        = 2679;

  cmd_EncAnsi           = 2680;
  cmd_EncUtf8bom        = 2681;
  cmd_EncUtf8nobom      = 2682;
  cmd_EncUtf16le        = 2683;
  cmd_EncUtf16be        = 2684;

  cmd_AutoComplete      = 2690;
  cmd_MenuEnc           = 2691;
  cmd_MenuEnds          = 2692;
  cmd_MenuLexers        = 2693;

implementation

const
  cXControl = {$ifdef darwin} 'Meta' {$else} 'Ctrl' {$endif};

procedure InitKeymapApp(M: TATKeymap);
begin
  M.Add(cmd_FileNew, 'file: new file', [cXControl+'+N'], []);
  M.Add(cmd_FileOpen, 'file: open file', [cXControl+'+O'], []);
  M.Add(cmd_FileSave, 'file: save file', [cXControl+'+S'], []);
  M.Add(cmd_FileSaveAs, 'file: save file as', [], []);
  M.Add(cmd_FileSaveAll, 'file: save all tabs', [], []);
  M.Add(cmd_FileReopen, 'file: reopen', [], []);
  M.Add(cmd_FileClose, 'file: close tab', [cXControl+'+W'], []);
  M.Add(cmd_FileCloseAll, 'file: close all tabs', [], []);
  M.Add(cmd_FileCloseOtherThis, 'file: close other tabs (this group)', [], []);
  M.Add(cmd_FileCloseOtherAll, 'file: close other tabs (all groups)', [], []);
  M.Add(cmd_FileCloseAndDelete, 'file: close tab, delete file', [], []);
  M.Add(cmd_FileExit, 'file: quit program', [cXControl+'+Q'], []);
  M.Add(cmd_FileExportHtml, 'file: export to html', [], []);

  M.Add(cmd_OpsReloadAndApply, 'settings: reload/apply config', [], []);
  M.Add(cmd_OpsClearRecent, 'settings: clear recent files history', [], []);
  M.Add(cmd_OpsOpenDefault, 'settings: open default config', [], []);
  M.Add(cmd_OpsOpenUser, 'settings: open user config', [], []);
  M.Add(cmd_OpsOpenLexerOvr, 'settings: open lexer-override config', [], []);
  M.Add(cmd_OpsOpenFileTypes, 'settings: open file-types config', [], []);
  M.Add(cmd_OpsFontText, 'settings: select text font', [], []);
  M.Add(cmd_OpsFontUi, 'settings: select ui font', [], []);
  M.Add(cmd_ToggleFullScreen, 'ui: toggle full-screen mode', ['F11'], []);
  M.Add(cmd_ToggleSidePanel, 'ui: toggle side panel', ['F12'], []);
  M.Add(cmd_ToggleBottomPanel, 'ui: toggle bottom panel', [], []);
  M.Add(cmd_ToggleFindDialog, 'ui: toggle find/replace dialog', [], []);
  M.Add(cmd_ShowPanelConsole, 'ui: show panel: console', ['Ctrl+`'], []);
  M.Add(cmd_ShowPanelOutput, 'ui: show panel: output', [], []);
  M.Add(cmd_ShowPanelValidate, 'ui: show panel: validate', [], []);

  M.Add(cmd_DlgSaveTabs, 'dialog: save tabs', [], []);
  M.Add(cmd_DlgCommands, 'dialog: command list', ['F1'], []);
  M.Add(cmd_DlgTabs, 'dialog: go to tab', [cXControl+'+T'], []);
  M.Add(cmd_DlgGoto, 'dialog: go to line', [cXControl+'+G'], []);
  M.Add(cmd_DlgGotoBm, 'dialog: go to bookmark', [cXControl+'+B'], []);
  M.Add(cmd_DlgLexerProp, 'dialog: lexer properties', [], []);
  M.Add(cmd_DlgLexerLib, 'dialog: lexer library', [], []);
  M.Add(cmd_DlgColors, 'dialog: config color theme', [], []);

  M.Add(cmd_DlgFind, 'dialog: find', [cXControl+'+F'], []);
  M.Add(cmd_DlgReplace, 'dialog: replace', [cXControl+'+R'], []);
  M.Add(cmd_FindNext, 'find, next', ['F3'], []);
  M.Add(cmd_FindPrev, 'find, prev', ['Shift+F3'], []);
  M.Add(cmd_FindCurWordNext, 'find current word, next', [], []);
  M.Add(cmd_FindCurWordPrev, 'find current word, prev', [], []);
  M.Add(cmd_FindCurSelNext, 'find current selection, next', [], []);
  M.Add(cmd_FindCurSelPrev, 'find current selection, prev', [], []);

  M.Add(cmd_CopyLine, 'clipboard: copy current line', [], []);
  M.Add(cmd_CopyFilenameFull, 'clipboard: copy full filepath', [], []);
  M.Add(cmd_CopyFilenameDir, 'clipboard: copy filepath only', [], []);
  M.Add(cmd_CopyFilenameName, 'clipboard: copy filename only', [], []);

  M.Add(cmd_SortAsc, 'sort selection, asc', [], []);
  M.Add(cmd_SortDesc, 'sort selection, desc', [], []);

  M.Add(cmd_Groups1, 'groups: 1 group', [], []);
  M.Add(cmd_Groups2horz, 'groups: 2 groups vert', [], []);
  M.Add(cmd_Groups2vert, 'groups: 2 groups horz', [], []);
  M.Add(cmd_Groups3horz, 'groups: 3 groups vert', [], []);
  M.Add(cmd_Groups3vert, 'groups: 3 groups horz', [], []);
  M.Add(cmd_Groups3plus, 'groups: 3 groups as 1+2', [], []);
  M.Add(cmd_Groups4horz, 'groups: 4 groups vert', [], []);
  M.Add(cmd_Groups4vert, 'groups: 4 groups horz', [], []);
  M.Add(cmd_Groups4grid, 'groups: 4 groups grid', [], []);
  M.Add(cmd_Groups6grid, 'groups: 6 groups grid', [], []);

  M.Add(cmd_GroupActivateNext, 'groups: focus next group', [], []);
  M.Add(cmd_GroupActivatePrev, 'groups: focus prev group', [], []);
  M.Add(cmd_MoveTabToGroupNext, 'groups: move tab to next group', [], []);
  M.Add(cmd_MoveTabToGroupPrev, 'groups: move tab to prev group', [], []);

  M.Add(cmd_BookmarkToggle, 'bookmarks: toggle current line', [], []);
  M.Add(cmd_BookmarkGotoNext, 'bookmarks: go to next', [], []);
  M.Add(cmd_BookmarkGotoPrev, 'bookmarks: go to prev', [], []);
  M.Add(cmd_BookmarkInvertAll, 'bookmarks: inverse all lines', [], []);
  M.Add(cmd_BookmarkClearAll, 'bookmarks: clear all', [], []);

  M.Add(cmd_CommentLineAdd, 'comments: add line comment', [], []);
  M.Add(cmd_CommentLineRemove, 'comments: remove line comment', [], []);
  M.Add(cmd_CommentLineToggle, 'comments: toggle line comment', [cXControl+'+/'], []);

  M.Add(cmd_SplitTabToggle, 'split tab: toggle split', [], []);
  M.Add(cmd_SplitTabHorzVert, 'split tab: toggle horz/vert', [], []);
  M.Add(cmd_SplitTab3070, 'split tab: 30/70', [], []);
  M.Add(cmd_SplitTab4060, 'split tab: 40/60', [], []);
  M.Add(cmd_SplitTab5050, 'split tab: 50/50', [], []);
  M.Add(cmd_SplitTab6040, 'split tab: 60/40', [], []);
  M.Add(cmd_SplitTab7030, 'split tab: 70/30', [], []);

  M.Add(cmd_LineEndWin, 'change line ends: win', [], []);
  M.Add(cmd_LineEndUnix, 'change line ends: unix', [], []);
  M.Add(cmd_LineEndMac, 'change line ends: mac', [], []);

  M.Add(cmd_EncAnsi, 'change encoding: ansi', [], []);
  M.Add(cmd_EncUtf8bom, 'change encoding: utf8 bom', [], []);
  M.Add(cmd_EncUtf8nobom, 'change encoding: utf8 no bom', [], []);
  M.Add(cmd_EncUtf16le, 'change encoding: utf16 le', [], []);
  M.Add(cmd_EncUtf16be, 'change encoding: utf16 be', [], []);

  M.Add(cmd_AutoComplete, 'auto-complete menu', ['Ctrl+Space'], []);
  M.Add(cmd_MenuEnc, 'menu: encodings', [], []);
  M.Add(cmd_MenuEnds, 'menu: line ends', [], []);
  M.Add(cmd_MenuLexers, 'menu: lexers', [], []);
end;


end.

