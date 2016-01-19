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

procedure InitKeymapForApplication(M: TATKeymap);
function IsCommandForMacros(Cmd: integer): boolean;
function IsCommandNeedTimer(Cmd: integer): boolean;

const
  cmdFirstAppCommand = 2500;
  cmdFirstLexerCommand = 6000;
  cmdLastLexerCommand = 6400-1;
  cmdFirstPluginCommand = 6400;
  cmdLastPluginCommand = 6800-1;

  //for macros
  cmd_MouseClick = 2490;
  cmd_MouseSelect = 2491;
  cmd_FinderAction = 2492;

  //normal commands
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
  cmd_DialogSaveTabs     = 2527;
  cmd_ToggleFullScreen   = 2528;
  cmd_OpsReloadAndApply  = 2529;
  cmd_DialogLexerProp    = 2530;
  cmd_DialogLexerLib     = 2531;
  cmd_DialogColors       = 2532;
  cmd_ToggleSidePanel    = 2533;
  cmd_ToggleBottomPanel  = 2534;
  cmd_ShowPanelConsole   = 2535;
  cmd_ShowPanelOutput    = 2536;
  cmd_ShowPanelValidate  = 2537;
  cmd_ToggleFindDialog   = 2538;
  cmd_DialogLoadLexerStyles = 2540;
  cmd_ToggleToolbar      = 2541;
  cmd_ToggleStatusbar    = 2542;
  cmd_ResetPythonPlugins = 2543;
  cmd_DialogCharMap      = 2544;
  cmd_RunLastCommandPlugin = 2545;

  cmd_DialogGoto       = 2580;
  cmd_DialogGotoBookmark = 2581;
  cmd_DialogCommands   = 2582;
  cmd_DialogFind       = 2584;
  cmd_DialogReplace    = 2585;

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
  cmd_ConvertTabsToSpaces = 2658;
  cmd_ConvertSpacesToTabsLeading = 2659;

  cmd_BookmarkToggle    = 2661;
  cmd_BookmarkInvertAll = 2662;
  cmd_BookmarkClearAll  = 2663;
  cmd_BookmarkGotoNext  = 2664;
  cmd_BookmarkGotoPrev  = 2665;

  cmd_CommentLineAdd_AtNonspace    = 2670;
  cmd_CommentLineAdd_AtStart       = 2671;
  cmd_CommentLineRemove            = 2672;
  cmd_CommentLineToggle_AtNonspace = 2673;
  cmd_CommentLineToggle_AtStart    = 2674;
  cmd_CommentStreamToggle          = 2675;
  cmd_DuplicateLineEx              = 2676;

  cmd_LineEndWin        = 2677;
  cmd_LineEndUnix       = 2678;
  cmd_LineEndMac        = 2679;

  cmd_FoldingFoldAtCurLine   = 2680;
  cmd_FoldingUnfoldAtCurLine = 2681;

  cmd_MenuEnc           = 2691;
  cmd_MenuEnds          = 2692;
  cmd_MenuLexers        = 2693;

  cmd_AutoComplete      = 2695;
  cmd_GotoDefinition    = 2696;
  cmd_ShowFunctionHint  = 2697;

  cmd_HelpAbout     = 2700;
  cmd_HelpForum     = 2701;
  cmd_HelpWiki      = 2702;
  cmd_HelpMouse     = 2703;
  cmd_HelpChangelog = 2704;
  cmd_HelpLexers    = 2705;

  cmd_Encoding_ansi_NoReload      = 2710;
  cmd_Encoding_utf8bom_NoReload   = 2711;
  cmd_Encoding_utf8nobom_NoReload = 2712;
  cmd_Encoding_utf16le_NoReload   = 2713;
  cmd_Encoding_utf16be_NoReload   = 2714;
  cmd_Encoding_cp1250_NoReload    = 2715;
  cmd_Encoding_cp1251_NoReload    = 2716;
  cmd_Encoding_cp1252_NoReload    = 2717;
  cmd_Encoding_cp1253_NoReload    = 2718;
  cmd_Encoding_cp1254_NoReload    = 2719;
  cmd_Encoding_cp1255_NoReload    = 2720;
  cmd_Encoding_cp1256_NoReload    = 2721;
  cmd_Encoding_cp1257_NoReload    = 2722;
  cmd_Encoding_cp1258_NoReload    = 2723;
  cmd_Encoding_mac_NoReload       = 2724;
  cmd_Encoding_iso1_NoReload      = 2725;
  cmd_Encoding_iso2_NoReload      = 2726;
  cmd_Encoding_cp437_NoReload     = 2730;
  cmd_Encoding_cp850_NoReload     = 2731;
  cmd_Encoding_cp852_NoReload     = 2732;
  cmd_Encoding_cp866_NoReload     = 2733;
  cmd_Encoding_cp874_NoReload     = 2734;
  cmd_Encoding_cp932_NoReload     = 2735;
  cmd_Encoding_cp936_NoReload     = 2736;
  cmd_Encoding_cp949_NoReload     = 2737;
  cmd_Encoding_cp950_NoReload     = 2738;

  cmd_Encoding_ansi_Reload      = 2750;
  cmd_Encoding_utf8bom_Reload   = 2751;
  cmd_Encoding_utf8nobom_Reload = 2752;
  cmd_Encoding_utf16le_Reload   = 2753;
  cmd_Encoding_utf16be_Reload   = 2754;
  cmd_Encoding_cp1250_Reload    = 2755;
  cmd_Encoding_cp1251_Reload    = 2756;
  cmd_Encoding_cp1252_Reload    = 2757;
  cmd_Encoding_cp1253_Reload    = 2758;
  cmd_Encoding_cp1254_Reload    = 2759;
  cmd_Encoding_cp1255_Reload    = 2760;
  cmd_Encoding_cp1256_Reload    = 2761;
  cmd_Encoding_cp1257_Reload    = 2762;
  cmd_Encoding_cp1258_Reload    = 2763;
  cmd_Encoding_mac_Reload       = 2764;
  cmd_Encoding_iso1_Reload      = 2765;
  cmd_Encoding_iso2_Reload      = 2766;
  cmd_Encoding_cp437_Reload     = 2770;
  cmd_Encoding_cp850_Reload     = 2771;
  cmd_Encoding_cp852_Reload     = 2772;
  cmd_Encoding_cp866_Reload     = 2773;
  cmd_Encoding_cp874_Reload     = 2774;
  cmd_Encoding_cp932_Reload     = 2775;
  cmd_Encoding_cp936_Reload     = 2776;
  cmd_Encoding_cp949_Reload     = 2777;
  cmd_Encoding_cp950_Reload     = 2778;

  cmd_Markers_DropAtCaret        = 2800;
  cmd_Markers_GotoLastNoDelete   = 2801;
  cmd_Markers_GotoLastAndDelete  = 2802;
  cmd_Markers_ClearAll           = 2803;
  cmd_Markers_SwapCaretAndMarker = 2804;

  cmd_MacroStart                 = 2810;
  cmd_MacroStop                  = 2811;
  cmd_MacroCancel                = 2812;

  cmd_TreeGotoNext               = 2815;
  cmd_TreeGotoPrev               = 2816;
  cmd_TreeGotoParent             = 2817;
  cmd_TreeGotoNextBrother        = 2818;
  cmd_TreeGotoPrevBrother        = 2819;
  cmd_TreeUpdate                 = 2820;


implementation

const
  cXControl = {$ifdef darwin} 'Meta' {$else} 'Ctrl' {$endif};

procedure InitKeymapForApplication(M: TATKeymap);
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

  M.Add(cmd_ResetPythonPlugins, 'plugins: reset python plugins', [], []);
  M.Add(cmd_RunLastCommandPlugin, 'plugins: run last command plugin', [], []);

  M.Add(cmd_ToggleFullScreen, 'ui: toggle full-screen mode', ['F11'], []);
  M.Add(cmd_ToggleSidePanel, 'ui: toggle side panel', ['F12'], []);
  M.Add(cmd_ToggleBottomPanel, 'ui: toggle bottom panel', [], []);
  M.Add(cmd_ToggleFindDialog, 'ui: toggle find/replace dialog', [], []);
  M.Add(cmd_ToggleToolbar, 'ui: toggle toolbar', [], []);
  M.Add(cmd_ToggleStatusbar, 'ui: toggle statusbar', [], []);

  M.Add(cmd_ShowPanelConsole, 'ui: show panel: console', ['Ctrl+`'], []);
  M.Add(cmd_ShowPanelOutput, 'ui: show panel: output', [], []);
  M.Add(cmd_ShowPanelValidate, 'ui: show panel: validate', [], []);

  M.Add(cmd_DialogSaveTabs, 'dialog: save tabs', [], []);
  M.Add(cmd_DialogCommands, 'dialog: command list', ['F1'], []);
  M.Add(cmd_DialogGoto, 'dialog: go to line', [cXControl+'+G'], []);
  M.Add(cmd_DialogGotoBookmark, 'dialog: go to bookmark', [cXControl+'+B'], []);
  M.Add(cmd_DialogLexerProp, 'dialog: lexer properties', [], []);
  M.Add(cmd_DialogLexerLib, 'dialog: lexer library', [], []);
  M.Add(cmd_DialogLoadLexerStyles, 'dialog: restore lexer styles', [], []);
  M.Add(cmd_DialogColors, 'dialog: config color theme', [], []);
  M.Add(cmd_DialogCharMap, 'dialog: char map', [], []);

  M.Add(cmd_DialogFind, 'dialog: find', [cXControl+'+F'], []);
  M.Add(cmd_DialogReplace, 'dialog: replace', [cXControl+'+R'], []);
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

  M.Add(cmd_ConvertTabsToSpaces, 'convert tabs to spaces', [], []);
  M.Add(cmd_ConvertSpacesToTabsLeading, 'convert spaces (leading) to tabs', [], []);

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

  M.Add(cmd_CommentLineAdd_AtNonspace, 'comments: add line comment, at non-space char', [], []);
  M.Add(cmd_CommentLineAdd_AtStart, 'comments: add line comment, at line start', [], []);
  M.Add(cmd_CommentLineRemove, 'comments: remove line comment', [], []);
  M.Add(cmd_CommentLineToggle_AtNonspace, 'comments: toggle line comment, at non-space char', [cXControl+'+/'], []);
  M.Add(cmd_CommentLineToggle_AtStart, 'comments: toggle line comment, at line start', [], []);
  M.Add(cmd_CommentStreamToggle, 'comments: toggle stream comment', ['Alt+/'], []);
  M.Add(cmd_DuplicateLineEx, 'duplicate line (advanced)', [], []);

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

  M.Add(cmd_MenuEnc, 'menu: encodings', [], []);
  M.Add(cmd_MenuEnds, 'menu: line ends', [], []);
  M.Add(cmd_MenuLexers, 'menu: lexers', [], []);

  M.Add(cmd_AutoComplete, 'code: auto-completion menu', ['Ctrl+Space'], []);
  M.Add(cmd_GotoDefinition, 'code: go to definition', [], []);
  M.Add(cmd_ShowFunctionHint, 'code: show function-hint', ['Ctrl+Shift+Space'], []);

  M.Add(cmd_FoldingFoldAtCurLine, 'folding: fold range at current line', [], []);
  M.Add(cmd_FoldingUnfoldAtCurLine, 'folding: unfold range at current line', [], []);

  M.Add(cmd_Encoding_ansi_NoReload, 'change encoding, no reload: ansi', [], []);
  M.Add(cmd_Encoding_utf8bom_NoReload, 'change encoding, no reload: utf8 bom', [], []);
  M.Add(cmd_Encoding_utf8nobom_NoReload, 'change encoding, no reload: utf8 no bom', [], []);
  M.Add(cmd_Encoding_utf16le_NoReload, 'change encoding, no reload: utf16 le', [], []);
  M.Add(cmd_Encoding_utf16be_NoReload, 'change encoding, no reload: utf16 be', [], []);
  M.Add(cmd_Encoding_cp1250_NoReload, 'change encoding, no reload: cp1250', [], []);
  M.Add(cmd_Encoding_cp1251_NoReload, 'change encoding, no reload: cp1251', [], []);
  M.Add(cmd_Encoding_cp1252_NoReload, 'change encoding, no reload: cp1252', [], []);
  M.Add(cmd_Encoding_cp1253_NoReload, 'change encoding, no reload: cp1253', [], []);
  M.Add(cmd_Encoding_cp1254_NoReload, 'change encoding, no reload: cp1254', [], []);
  M.Add(cmd_Encoding_cp1255_NoReload, 'change encoding, no reload: cp1255', [], []);
  M.Add(cmd_Encoding_cp1256_NoReload, 'change encoding, no reload: cp1256', [], []);
  M.Add(cmd_Encoding_cp1257_NoReload, 'change encoding, no reload: cp1257', [], []);
  M.Add(cmd_Encoding_cp1258_NoReload, 'change encoding, no reload: cp1258', [], []);
  M.Add(cmd_Encoding_mac_NoReload, 'change encoding, no reload: mac', [], []);
  M.Add(cmd_Encoding_iso1_NoReload, 'change encoding, no reload: iso1', [], []);
  M.Add(cmd_Encoding_iso2_NoReload, 'change encoding, no reload: iso2', [], []);
  M.Add(cmd_Encoding_cp437_NoReload, 'change encoding, no reload: cp437', [], []);
  M.Add(cmd_Encoding_cp850_NoReload, 'change encoding, no reload: cp850', [], []);
  M.Add(cmd_Encoding_cp852_NoReload, 'change encoding, no reload: cp852', [], []);
  M.Add(cmd_Encoding_cp866_NoReload, 'change encoding, no reload: cp866', [], []);
  M.Add(cmd_Encoding_cp874_NoReload, 'change encoding, no reload: cp874', [], []);
  M.Add(cmd_Encoding_cp932_NoReload, 'change encoding, no reload: cp932', [], []);
  M.Add(cmd_Encoding_cp936_NoReload, 'change encoding, no reload: cp936', [], []);
  M.Add(cmd_Encoding_cp949_NoReload, 'change encoding, no reload: cp949', [], []);
  M.Add(cmd_Encoding_cp950_NoReload, 'change encoding, no reload: cp950', [], []);

  M.Add(cmd_Encoding_ansi_Reload, 'change encoding, reload: ansi', [], []);
  M.Add(cmd_Encoding_utf8bom_Reload, 'change encoding, reload: utf8 bom', [], []);
  M.Add(cmd_Encoding_utf8nobom_Reload, 'change encoding, reload: utf8 no bom', [], []);
  M.Add(cmd_Encoding_utf16le_Reload, 'change encoding, reload: utf16 le', [], []);
  M.Add(cmd_Encoding_utf16be_Reload, 'change encoding, reload: utf16 be', [], []);
  M.Add(cmd_Encoding_cp1250_Reload, 'change encoding, reload: cp1250', [], []);
  M.Add(cmd_Encoding_cp1251_Reload, 'change encoding, reload: cp1251', [], []);
  M.Add(cmd_Encoding_cp1252_Reload, 'change encoding, reload: cp1252', [], []);
  M.Add(cmd_Encoding_cp1253_Reload, 'change encoding, reload: cp1253', [], []);
  M.Add(cmd_Encoding_cp1254_Reload, 'change encoding, reload: cp1254', [], []);
  M.Add(cmd_Encoding_cp1255_Reload, 'change encoding, reload: cp1255', [], []);
  M.Add(cmd_Encoding_cp1256_Reload, 'change encoding, reload: cp1256', [], []);
  M.Add(cmd_Encoding_cp1257_Reload, 'change encoding, reload: cp1257', [], []);
  M.Add(cmd_Encoding_cp1258_Reload, 'change encoding, reload: cp1258', [], []);
  M.Add(cmd_Encoding_mac_Reload, 'change encoding, reload: mac', [], []);
  M.Add(cmd_Encoding_iso1_Reload, 'change encoding, reload: iso1', [], []);
  M.Add(cmd_Encoding_iso2_Reload, 'change encoding, reload: iso2', [], []);
  M.Add(cmd_Encoding_cp437_Reload, 'change encoding, reload: cp437', [], []);
  M.Add(cmd_Encoding_cp850_Reload, 'change encoding, reload: cp850', [], []);
  M.Add(cmd_Encoding_cp852_Reload, 'change encoding, reload: cp852', [], []);
  M.Add(cmd_Encoding_cp866_Reload, 'change encoding, reload: cp866', [], []);
  M.Add(cmd_Encoding_cp874_Reload, 'change encoding, reload: cp874', [], []);
  M.Add(cmd_Encoding_cp932_Reload, 'change encoding, reload: cp932', [], []);
  M.Add(cmd_Encoding_cp936_Reload, 'change encoding, reload: cp936', [], []);
  M.Add(cmd_Encoding_cp949_Reload, 'change encoding, reload: cp949', [], []);
  M.Add(cmd_Encoding_cp950_Reload, 'change encoding, reload: cp950', [], []);

  M.Add(cmd_Markers_DropAtCaret       , 'markers: drop marker at caret', [], []);
  M.Add(cmd_Markers_GotoLastNoDelete  , 'markers: go to last marker (don''t delete)', [], []);
  M.Add(cmd_Markers_GotoLastAndDelete , 'markers: collect last marker (delete)', [], []);
  M.Add(cmd_Markers_ClearAll          , 'markers: remove all', [], []);
  M.Add(cmd_Markers_SwapCaretAndMarker, 'markers: swap caret and last marker', [], []);

  M.Add(cmd_MacroStart, 'macros: start recording', [], []);
  M.Add(cmd_MacroStop, 'macros: stop recording', [], []);
  M.Add(cmd_MacroCancel, 'macros: cancel recording', [], []);

  M.Add(cmd_TreeGotoNext, 'tree: select next node', [], []);
  M.Add(cmd_TreeGotoPrev, 'tree: select prev node', [], []);
  M.Add(cmd_TreeGotoParent, 'tree: select parent node', [], []);
  M.Add(cmd_TreeGotoNextBrother, 'tree: select next-brother node', [], []);
  M.Add(cmd_TreeGotoPrevBrother, 'tree: select prev-brother node', [], []);
  M.Add(cmd_TreeUpdate, 'tree: update tree panel', [], []);

  M.Add(cmd_HelpAbout, 'help: about', [], []);
  M.Add(cmd_HelpForum, 'help: forum', [], []);
  M.Add(cmd_HelpWiki, 'help: wiki', [], []);
  M.Add(cmd_HelpMouse, 'help: mouse usage', [], []);
  M.Add(cmd_HelpChangelog, 'help: changelog', [], []);
  M.Add(cmd_HelpLexers, 'help: lexers', [], []);

end;


function IsCommandNeedTimer(Cmd: integer): boolean;
begin
  case Cmd of
    0..Pred(cmdFirstAppCommand):
      Result:= false;

    cmdFirstLexerCommand..cmdLastLexerCommand,
    cmdFirstPluginCommand..cmdLastPluginCommand:
      Result:= true;

    cmd_FileNew               ,
    cmd_FileOpen              ,
    cmd_FileReopen            ,
    cmd_FileExit              ,
    cmd_FileClose             ,
    cmd_FileCloseOtherThis    ,
    cmd_FileCloseOtherAll     ,
    cmd_FileCloseAll          ,
    cmd_FileCloseAndDelete    ,
    cmd_FileExportHtml        ,

    cmd_OpsClearRecent        ,
    cmd_OpsOpenDefault        ,
    cmd_OpsOpenUser           ,
    cmd_OpsOpenLexerOvr       ,
    cmd_OpsOpenFileTypes      ,
    cmd_OpsFontText           ,
    cmd_OpsFontUi             ,
    cmd_DialogSaveTabs        ,
    cmd_ToggleFullScreen      ,
    cmd_OpsReloadAndApply     ,
    cmd_DialogLexerProp       ,
    cmd_DialogLexerLib        ,
    cmd_DialogColors          ,
    cmd_ToggleSidePanel       ,
    cmd_ToggleBottomPanel     ,
    //cmd_ShowPanelConsole      ,
    //cmd_ShowPanelOutput       ,
    //cmd_ShowPanelValidate     ,
    cmd_ToggleFindDialog      ,
    cmd_DialogLoadLexerStyles,
    cmd_ToggleToolbar       ,
    cmd_ToggleStatusbar     ,
    cmd_ResetPythonPlugins  ,
    cmd_DialogCharMap       ,
    cmd_RunLastCommandPlugin ,

    cmd_DialogGotoBookmark,
    cmd_DialogCommands   ,

    cmd_SplitTabToggle    ,
    cmd_SplitTabHorzVert  ,
    cmd_SplitTab3070      ,
    cmd_SplitTab4060      ,
    cmd_SplitTab5050      ,
    cmd_SplitTab6040      ,
    cmd_SplitTab7030      ,

    cmd_Groups1      ,
    cmd_Groups2horz  ,
    cmd_Groups2vert  ,
    cmd_Groups3horz  ,
    cmd_Groups3vert  ,
    cmd_Groups3plus  ,
    cmd_Groups4horz  ,
    cmd_Groups4vert  ,
    cmd_Groups4grid  ,
    cmd_Groups6grid  ,

    cmd_GroupActivateNext ,
    cmd_GroupActivatePrev ,

    cmd_MoveTabToGroupNext ,
    cmd_MoveTabToGroupPrev ,

    cmd_MenuEnc           ,
    cmd_MenuEnds          ,
    cmd_MenuLexers        ,

    cmd_AutoComplete      ,
    cmd_GotoDefinition    ,

    cmd_HelpAbout     ,
    cmd_HelpForum     ,
    cmd_HelpWiki      ,
    cmd_HelpMouse     ,
    cmd_HelpChangelog ,
    cmd_HelpLexers    :
      Result:= true;

    else
      Result:= false;
  end;
end;

function IsCommandForMacros(Cmd: integer): boolean;
begin
  case Cmd of
    1..Pred(cmdFirstAppCommand):
      Result:= true;

    cmdFirstLexerCommand..cmdLastLexerCommand,
    cmdFirstPluginCommand..cmdLastPluginCommand,
    cmd_MacroStart,
    cmd_MacroStop,
    cmd_MacroCancel,
    cmd_DialogCommands,
    cmd_DialogColors,
    cmd_DialogCharMap,
    cmd_DialogFind,
    cmd_DialogReplace,
    cmd_DialogGoto,
    cmd_DialogGotoBookmark,
    cmd_DialogLexerLib,
    cmd_DialogLexerProp,
    cmd_DialogLoadLexerStyles,
    cmd_DialogSaveTabs,
    cmd_FileNew,
    cmd_FileOpen,
    cmd_FileSaveAs,
    cmd_FileExit,
    cmd_FileClose,
    cmd_FileCloseOtherThis,
    cmd_FileCloseOtherAll,
    cmd_FileCloseAll,
    cmd_FileCloseAndDelete,
    cmd_FileExportHtml,
    cmd_ToggleBottomPanel,
    cmd_ToggleSidePanel,
    cmd_ToggleFindDialog,
    cmd_ToggleFullScreen,
    cmd_ToggleStatusbar,
    cmd_ToggleToolbar,
    cmd_Groups1,
    cmd_Groups2horz,
    cmd_Groups2vert,
    cmd_Groups3horz,
    cmd_Groups3vert,
    cmd_Groups3plus,
    cmd_Groups4horz,
    cmd_Groups4vert,
    cmd_Groups4grid,
    cmd_Groups6grid,
    cmd_GroupActivateNext,
    cmd_GroupActivatePrev,
    cmd_MenuEnc,
    cmd_MenuEnds,
    cmd_MenuLexers,
    cmd_HelpAbout,
    cmd_HelpForum,
    cmd_HelpWiki,
    cmd_HelpMouse,
    cmd_HelpChangelog,
    cmd_HelpLexers:
      Result:= false;
    else
      Result:= true;
  end;
end;


end.

