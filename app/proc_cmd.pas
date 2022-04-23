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

procedure Keymap_AddCudatextItems(M: TATKeymap);

function IsCommandForMacros(Cmd: integer): boolean;
function IsCommandNeedTimer(Cmd: integer): boolean;
function IsCommandHandledFromFindDialog(Cmd: integer): boolean;

type
  TAppCommandCategory = (
    categ_Normal,
    categ_Plugin,
    categ_PluginSub, //commands added via API app_proc
    categ_Lexer,
    categ_OpenedFile,
    categ_RecentFile
    );

const
  cmd_PluginRun = 1;
  cmd_PluginEnd = 2;

  cmd_GotoLastEditingPos = 2006;

  cmdFirstAppCommand = 2500;
  cmdFirstLexerCommand = 6000;
  cmdLastLexerCommand = 6400-1;
  cmdFirstPluginCommand = 6400;
  cmdLastPluginCommand = 8000-1;
  cmdFirstFileCommand = 8000;
  cmdLastFileCommand = 8800-1;
  cmdFirstRecentCommand = 8800;
  cmdLastRecentCommand = 9000-1;
  cmdLastAppCommand = cmdLastRecentCommand;

  //for macros, must be before cmdFirstAppCommand
  cmd_MouseClickAtCursor = 2480;
  cmd_MouseClickAtCursorAndSelect = 2481;
  cmd_MouseClickNearCaret = 2490;
  cmd_MouseClickNearCaretAndSelect = 2491;
  cmd_FinderAction = 2492;

  //normal commands
  cmd_FileNew            = 2500;
  cmd_FileOpen           = 2501;
  cmd_FileSave           = 2502;
  cmd_FileSaveAs         = 2503;
  cmd_FileSaveAll        = 2504;
  cmd_FileReopen         = 2505;
  cmd_FileExit           = 2506;
  cmd_FileOpen_NoPlugins = 2507;
  cmd_FileNewMenu        = 2508;
  cmd_FileOpenFolder     = 2509;
  cmd_FileClose          = 2510;
  cmd_FileCloseOtherThis = 2511;
  cmd_FileCloseOtherAll  = 2512;
  cmd_FileCloseAll       = 2513;
  cmd_FileCloseAndDelete = 2514;
  cmd_FileExportHtml     = 2515;
  cmd_RepaintEditor      = 2516;
  cmd_FileReopenRecent   = 2517;

  cmd_OpsOpenDefaultAndUser = 2519;
  cmd_OpsClearRecent     = 2520;
  cmd_OpsOpenDefault     = 2521;
  cmd_OpsOpenUser        = 2522;
  cmd_OpsOpenLexerSpecific = 2523;
  cmd_OpsFontText        = 2525;
  cmd_OpsFontUi          = 2526;
  cmd_OpsFontOutput      = 2527;
  cmd_ToggleFullScreen   = 2528;
  cmd_OpsReloadAndApply  = 2529;
  cmd_DialogLexerProp    = 2530;
  cmd_DialogLexerLib     = 2531;
  cmd_ToggleDistractionFree = 2532;
  cmd_ToggleSidePanel    = 2533;
  cmd_ToggleBottomPanel  = 2534;
  cmd_ShowPanelConsole   = 2535;
  cmd_ShowPanelOutput    = 2536;
  cmd_ShowPanelValidate  = 2537;
  cmd_ToggleFindDialog   = 2538;
  cmd_ToggleOnTop        = 2539;
  cmd_ToggleSidebar      = 2540;
  cmd_ToggleToolbar      = 2541;
  cmd_ToggleStatusbar    = 2542;
  cmd_ResetPythonPlugins = 2543;
  cmd_DialogCharMap      = 2544;
  cmd_RunLastCommandPlugin= 2545;
  cmd_ShowSidePanelAsIs   = 2546;
  cmd_ShowSidePanelAndSyntaxTree = 2547;
  cmd_HideSidePanel       = 2548;
  cmd_DialogSaveTabs      = 2549;
  cmd_DialogLexerStyleMap = 2550;
  cmd_RescanPythonPluginsInfFiles = 2551;
  cmd_DialogThemeUi       = 2552;
  cmd_DialogThemeSyntax   = 2553;
  cmd_ShowMainMenuAsPopup = 2554;
  cmd_DialogLexerMenu     = 2555;
  cmd_ToggleFloatSide     = 2556;
  cmd_ToggleFloatBottom   = 2557;
  cmd_HideBottomPanel     = 2558;
  cmd_OpsFontSizeBigger   = 2559;
  cmd_OpsFontSizeSmaller  = 2560;
  cmd_ShowPanelConsole_AndFocus   = 2561;
  cmd_ShowPanelOutput_AndFocus    = 2562;
  cmd_ShowPanelValidate_AndFocus  = 2563;
  cmd_ToggleReplaceDialog         = 2564;
  cmd_ToggleSidePanelAndSyntaxTree= 2565;
  cmd_OpsFontSizeReset            = 2566;
  cmd_FindPythonLib               = 2567;
  cmd_ToggleFileNotifications     = 2568;
  cmd_ToggleFindDialog_AndFocus   = 2569;

  cmd_ChooseTranslation = 2570;
  cmd_ChooseThemeUI     = 2571;

  cmd_OpsClearSearchHistory = 2573;
  cmd_OpsClearConsoleHistory = 2574;

  cmd_ToggleUiTabs = 2575;
  cmd_ToggleFocusSplitEditors = 2576;
  cmd_FocusEditor = 2577;
  cmd_FocusNotificationPanel = 2578;
  cmd_HideNotificationPanels = 2579;

  cmd_DialogGoto       = 2580;
  cmd_DialogGotoBookmark = 2581;
  cmd_DialogCommands   = 2582;
  cmd_DialogFind       = 2584;
  cmd_DialogReplace    = 2585;
  cmd_DialogFind_Hide  = 2586;

  cmd_FindFirst        = 2589;
  cmd_FindNext         = 2590;
  cmd_FindPrev         = 2591;
  cmd_FindCurWordNext  = 2592;
  cmd_FindCurWordPrev  = 2593;
  cmd_FindCurSelNext   = 2594;
  cmd_FindCurSelPrev   = 2595;
  cmd_FindAllAndSelect = 2596;
  cmd_FindAllAndMarkers = 2597;
  cmd_FindAllAndBookmarks = 2598;

  cmd_SelectExpandToWord = 2600; //like Ctrl+D in Sublime
  cmd_OpenContainingFolder = 2601;
  cmd_OpenFileInDefaultApp = 2602;
  cmd_FileOpen_TextViewer    = 2603;
  cmd_FileOpen_HexViewer     = 2604;
  cmd_FileOpen_UnicodeViewer = 2605;
  cmd_SelectExpandToWord_Skip = 2606;
  cmd_SelectExpandToText = 2607; //like cmd_SelectExpandToWord but ignores whole-words
  cmd_SelectExpandToText_Skip = 2608;

  cmd_SwitchTab_HotkeyNext = 2610;
  cmd_SwitchTab_HotkeyPrev = 2611;
  cmd_SwitchTab_SimpleNext = 2612;
  cmd_SwitchTab_SimplePrev = 2613;
  cmd_SwitchTab_Dialog     = 2614;
  cmd_SwitchTab_Recent     = 2615;

  cmd_SplitTabHorz     = 2617;
  cmd_SplitTabVert     = 2618;
  cmd_SplitTabNo       = 2619;
  cmd_SplitTabToggle   = 2620;
  cmd_SplitTabHorzVert = 2621;
  cmd_SplitTab3070     = 2622;
  cmd_SplitTab4060     = 2623;
  cmd_SplitTab5050     = 2624;
  cmd_SplitTab6040     = 2625;
  cmd_SplitTab7030     = 2626;

  cmd_Groups6vert = 2627;
  cmd_Groups6horz = 2628;
  cmd_Groups3plushorz = 2629;
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
  cmd_MoveTabToGroup1 = 2644;
  cmd_MoveTabToGroup2 = 2645;
  cmd_MoveTabToGroupFloating1 = 2646;
  cmd_MoveTabToGroupFloating2 = 2647;
  cmd_MoveTabToGroupFloating3 = 2648;

  cmd_CopyLine         = 2650;
  cmd_CopyFilenameFull = 2651;
  cmd_CopyFilenameDir  = 2652;
  cmd_CopyFilenameName = 2653;

  cmd_TabUsesSpaces_On    = 2655;
  cmd_TabUsesSpaces_Off   = 2656;
  cmd_ToggleTabUsesSpaces = 2657;
  cmd_ConvertTabsToSpaces = 2658;
  cmd_ConvertSpacesToTabsLeading = 2659;
  cmd_ConvertTabsToSpacesLeading = 2660;

  cmd_BookmarkToggle    = 2661;
  cmd_BookmarkInvertAll = 2662;
  cmd_BookmarkClearAll  = 2663;
  cmd_BookmarkGotoNext  = 2664;
  cmd_BookmarkGotoPrev  = 2665;
  cmd_BookmarkPlaceCarets = 2667;
  cmd_BookmarkCopyMarkedLines = 2668;
  cmd_BookmarkDeleteMarkedLines = 2669;
  cmd_BookmarkPlaceBookmarksOnCarets = 2670;

  cmd_SetTabColor       = 2671;
  cmd_ResetTabColor     = 2672;
  cmd_ToggleTabPinned   = 2675;
  cmd_DuplicateLineEx   = 2676;

  cmd_LineEndWin        = 2677;
  cmd_LineEndUnix       = 2678;
  cmd_LineEndMac        = 2679;

  cmd_LineEndWin_Caret     = 2680;
  cmd_LineEndUnix_Caret    = 2681;
  cmd_LineEndMac_Caret     = 2682;
  cmd_LineEndDefault_Caret = 2683;

  cmd_FoldingEnable          = 2684;
  cmd_FoldingDisable         = 2685;
  cmd_DeleteNewColorAttrs    = 2686;

  cmd_MenuEnc           = 2691;
  cmd_MenuEnds          = 2692;
  cmd_MenuLexers        = 2693;

  cmd_AutoComplete      = 2695;
  cmd_GotoDefinition    = 2696;
  cmd_ShowFunctionHint  = 2697;

  cmd_HelpAbout     = 2700;
  cmd_HelpForum     = 2701;
  cmd_HelpWiki      = 2702;
  cmd_HelpIssues    = 2706;
  cmd_HelpCheckUpdates = 2708;

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
  cmd_Encoding_iso15_NoReload     = 2727;
  cmd_Encoding_cp437_NoReload     = 2730;
  cmd_Encoding_cp850_NoReload     = 2731;
  cmd_Encoding_cp852_NoReload     = 2732;
  cmd_Encoding_cp866_NoReload     = 2733;
  cmd_Encoding_cp874_NoReload     = 2734;
  cmd_Encoding_cp932_NoReload     = 2735;
  cmd_Encoding_cp936_NoReload     = 2736;
  cmd_Encoding_cp949_NoReload     = 2737;
  cmd_Encoding_cp950_NoReload     = 2738;
  cmd_Encoding_utf32le_NoReload   = 2739;
  cmd_Encoding_utf32be_NoReload   = 2740;

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
  cmd_Encoding_iso15_Reload     = 2767;
  cmd_Encoding_cp437_Reload     = 2770;
  cmd_Encoding_cp850_Reload     = 2771;
  cmd_Encoding_cp852_Reload     = 2772;
  cmd_Encoding_cp866_Reload     = 2773;
  cmd_Encoding_cp874_Reload     = 2774;
  cmd_Encoding_cp932_Reload     = 2775;
  cmd_Encoding_cp936_Reload     = 2776;
  cmd_Encoding_cp949_Reload     = 2777;
  cmd_Encoding_cp950_Reload     = 2778;
  cmd_Encoding_utf32le_Reload   = 2779;
  cmd_Encoding_utf32be_Reload   = 2780;

  cmd_Markers_SelectToCaret      = 2798;
  cmd_Markers_DeleteToCaret      = 2799;
  cmd_Markers_DropAtCaret        = 2800;
  cmd_Markers_GotoLastNoDelete   = 2801;
  cmd_Markers_GotoLastAndDelete  = 2802;
  cmd_Markers_ClearAll           = 2803;
  cmd_Markers_SwapCaretAndMarker = 2804;

  cmd_LinkAtCaret_Open           = 2806;
  cmd_LinkAtCaret_Copy           = 2807;
  cmd_LinkAtPopup_Open           = 2808;
  cmd_LinkAtPopup_Copy           = 2809;

  cmd_MacroStart                 = 2810;

  cmd_TreeGotoNext               = 2815;
  cmd_TreeGotoPrev               = 2816;
  cmd_TreeGotoParent             = 2817;
  cmd_TreeGotoNextBrother        = 2818;
  cmd_TreeGotoPrevBrother        = 2819;
  cmd_TreeUpdate                 = 2820;
  cmd_TreeSelectBlockForCurNode  = 2821;
  cmd_TreeGotoBlockForCurNode    = 2822;
  cmd_TreeFilterClear            = 2823;
  cmd_TreeFilterFocus            = 2824;
  cmd_TreeFocus                  = 2825;

  cmd_BracketHighlightOn         = 2840;
  cmd_BracketHighlightOff        = 2841;
  cmd_BracketHighlightToggle     = 2842;
  cmd_BracketJump                = 2845;
  cmd_BracketSelect              = 2846;
  cmd_BracketSelectInside        = 2847;

  cmd_TabSize_Set2               = 2862;
  cmd_TabSize_Set4               = 2864;
  cmd_TabSize_Set8               = 2868;

  cmd_GroupActivate1             = 2901;
  cmd_GroupActivate2             = 2902;
  cmd_GroupActivate3             = 2903;
  cmd_GroupActivate4             = 2904;
  cmd_GroupActivate5             = 2905;
  cmd_GroupActivate6             = 2906;
  cmd_GroupActivateFloat1        = 2910;
  cmd_GroupActivateFloat2        = 2911;
  cmd_GroupActivateFloat3        = 2912;

implementation

const
  cXControl = {$ifdef darwin} 'Meta' {$else} 'Ctrl' {$endif};

procedure Keymap_AddCudatextItems(M: TATKeymap);
begin
  M.Add(cmd_RepaintEditor, 'repaint editor', [], []);
  M.Add(cmd_FileNew, 'file: new file', [cXControl+'+N'], []);
  M.Add(cmd_FileNewMenu, 'file: new file, from template', [], []);
  M.Add(cmd_FileOpen, 'file: open file', [cXControl+'+O'], []);
  M.Add(cmd_FileOpen_NoPlugins, 'file: open file, ignore plugins', [], []);
  M.Add(cmd_FileOpen_TextViewer, 'file: open file, in text viewer', [], []);
  M.Add(cmd_FileOpen_HexViewer, 'file: open file, in hex viewer', [], []);
  M.Add(cmd_FileOpen_UnicodeViewer, 'file: open file, in unicode viewer', [], []);
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
  M.Add(cmd_FileReopenRecent, 'file: reopen recent file', [], []);
  M.Add(cmd_OpenContainingFolder, 'file: open folder containing the current file', [], []);
  M.Add(cmd_OpenFileInDefaultApp, 'file: open file in default application', [], []);

  M.Add(cmd_OpsReloadAndApply, 'settings: reload/apply config', [], []);
  M.Add(cmd_OpsClearRecent, 'settings: clear recent files history', [], []);
  M.Add(cmd_OpsClearSearchHistory, 'settings: clear find/replace dialog history', [], []);
  M.Add(cmd_OpsClearConsoleHistory, 'settings: clear console input history', [], []);
  M.Add(cmd_OpsOpenDefault, 'settings: open default config', [], []);
  M.Add(cmd_OpsOpenUser, 'settings: open user config', [], []);
  M.Add(cmd_OpsOpenDefaultAndUser, 'settings: open default/user configs', [], []);
  M.Add(cmd_OpsOpenLexerSpecific, 'settings: open lexer-specific config', [], []);

  M.Add(cmd_OpsFontText, 'settings: select font: text', [], []);
  M.Add(cmd_OpsFontUi, 'settings: select font: ui', [], []);
  M.Add(cmd_OpsFontOutput, 'settings: select font: output panel', [], []);

  M.Add(cmd_OpsFontSizeBigger, 'settings: font size: bigger', [], []);
  M.Add(cmd_OpsFontSizeSmaller, 'settings: font size: smaller', [], []);
  M.Add(cmd_OpsFontSizeReset, 'settings: font size: reset', [], []);

  M.Add(cmd_ResetPythonPlugins, 'plugins: reset python plugins', [], []);
  M.Add(cmd_RunLastCommandPlugin, 'plugins: run last command plugin', [], []);
  M.Add(cmd_RescanPythonPluginsInfFiles, 'plugins: rescan python plugins inf-files', [], []);
  M.Add(cmd_FindPythonLib, 'plugins: find python library in OS', [], []);

  M.Add(cmd_ToggleFullScreen, 'ui: toggle full-screen mode', [{$ifndef darwin}'F11'{$else}'Ctrl+Meta+F'{$endif}], []);
  M.Add(cmd_ToggleDistractionFree, 'ui: toggle distraction-free mode', [{$ifndef darwin}'Alt+F11'{$else}'Ctrl+Meta+G'{$endif}], []);
  M.Add(cmd_ToggleSidePanel, 'ui: toggle side panel', [{$ifndef darwin}'F12'{$endif}], []);
  M.Add(cmd_ToggleSidePanelAndSyntaxTree, 'ui: toggle side panel / code tree', [], []);
  M.Add(cmd_ToggleBottomPanel, 'ui: toggle bottom panel', [], []);
  M.Add(cmd_ToggleSidebar, 'ui: toggle sidebar', [], []);
  M.Add(cmd_ToggleToolbar, 'ui: toggle toolbar', [], []);
  M.Add(cmd_ToggleStatusbar, 'ui: toggle statusbar', [], []);
  M.Add(cmd_ToggleUiTabs, 'ui: toggle ui-tabs', [], []);
  //M.Add(cmd_ToggleMenu, 'ui: toggle menu bar', [], []);
  M.Add(cmd_ToggleOnTop, 'ui: toggle window always on top', [], []);
  M.Add(cmd_ToggleFloatSide, 'ui: toggle floating side panel', [], []);
  M.Add(cmd_ToggleFloatBottom, 'ui: toggle floating bottom panel', [], []);

  M.Add(cmd_ShowSidePanelAsIs, 'ui: show side panel', [], []);
  M.Add(cmd_ShowSidePanelAndSyntaxTree, 'ui: show side panel / code tree', [], []);
  M.Add(cmd_HideSidePanel, 'ui: hide side panel', [], []);
  M.Add(cmd_HideBottomPanel, 'ui: hide bottom panel', [], []);

  M.Add(cmd_ShowPanelConsole,          'ui: show bottom panel / console', [], []);
  M.Add(cmd_ShowPanelOutput,           'ui: show bottom panel / output', [], []);
  M.Add(cmd_ShowPanelValidate,         'ui: show bottom panel / validate', [], []);
  M.Add(cmd_ShowPanelConsole_AndFocus, 'ui: show+focus bottom panel / console', ['Ctrl+`'], []);
  M.Add(cmd_ShowPanelOutput_AndFocus,  'ui: show+focus bottom panel / output', [], []);
  M.Add(cmd_ShowPanelValidate_AndFocus, 'ui: show+focus bottom panel / validate', [], []);

  M.Add(cmd_ToggleFocusSplitEditors, 'ui: toggle focus between split 1st/2nd editors', [], []);
  M.Add(cmd_FocusEditor, 'ui: focus editor', [], []);
  M.Add(cmd_FocusNotificationPanel, 'ui: focus editor notification panel', [], []);
  M.Add(cmd_HideNotificationPanels, 'ui: hide editor notification panel(s)', [], []);
  M.Add(cmd_ToggleFileNotifications, 'ui: toggle file-change notifications', [], []);

  M.Add(cmd_SwitchTab_HotkeyNext, 'ui: switch tab, to next', ['Ctrl+Tab'], []);
  M.Add(cmd_SwitchTab_HotkeyPrev, 'ui: switch tab, to previous', ['Ctrl+Shift+Tab'], []);
  M.Add(cmd_SwitchTab_SimpleNext, 'ui: switch tab, simply to next', [], []);
  M.Add(cmd_SwitchTab_SimplePrev, 'ui: switch tab, simply to previous', [], []);
  M.Add(cmd_SwitchTab_Dialog, 'ui: switch tab, dialog', [], []);
  M.Add(cmd_SwitchTab_Recent, 'ui: switch tab, to recent', [], []);

  M.Add(cmd_ShowMainMenuAsPopup, 'ui: show main menu as popup', [], []);
  M.Add(cmd_DialogSaveTabs, 'dialog: save tabs', [], []);
  M.Add(cmd_DialogCommands, 'dialog: command palette', [cXControl+'+Shift+P'], ['F1']);
  M.Add(cmd_DialogGoto, 'dialog: go to line', [cXControl+'+G'], []);
  M.Add(cmd_DialogGotoBookmark, 'dialog: go to bookmark', [cXControl+'+B'], []);
  M.Add(cmd_DialogLexerProp, 'dialog: lexer properties', [], []);
  M.Add(cmd_DialogLexerLib, 'dialog: lexer library', [], []);
  M.Add(cmd_DialogLexerStyleMap, 'dialog: lexer styles mapping', [], []);
  M.Add(cmd_DialogThemeUi, 'dialog: configure ui-theme', [], []);
  M.Add(cmd_DialogThemeSyntax, 'dialog: configure syntax-theme', [], []);
  M.Add(cmd_DialogCharMap, 'dialog: char map', [], []);

  M.Add(cmd_DialogFind, 'dialog: find: show dialog', [cXControl+'+F'], []);
  M.Add(cmd_DialogFind_Hide, 'dialog: find: hide dialog', [], []);
  M.Add(cmd_ToggleFindDialog, 'dialog: find: toggle dialog', [], []);
  M.Add(cmd_ToggleFindDialog_AndFocus, 'dialog: find: toggle+focus dialog', [], []);
  M.Add(cmd_DialogReplace, 'dialog: replace: show dialog', [cXControl+'+R'], []);
  M.Add(cmd_ToggleReplaceDialog, 'dialog: replace: toggle dialog', [], []);

  M.Add(cmd_FindFirst, 'find, first', [], []);
  M.Add(cmd_FindNext, 'find, next', ['F3'], []);
  M.Add(cmd_FindPrev, 'find, previous', ['Shift+F3'], []);
  M.Add(cmd_FindAllAndSelect, 'find all, and select', [], []);
  M.Add(cmd_FindAllAndMarkers, 'find all, and place markers', [], []);
  M.Add(cmd_FindAllAndBookmarks, 'find all, and place bookmarks', [], []);
  M.Add(cmd_FindCurWordNext, 'find current word, next', [], []);
  M.Add(cmd_FindCurWordPrev, 'find current word, previous', [], []);
  M.Add(cmd_FindCurSelNext, 'find current selection, next', [], []);
  M.Add(cmd_FindCurSelPrev, 'find current selection, previous', [], []);
  M.Add(cmd_GotoLastEditingPos, 'go to last editing pos', [], []);

  M.Add(cmd_SelectExpandToWord, 'selection: add next occurrence of selected word', [cXControl+'+Shift+D'], []);
  M.Add(cmd_SelectExpandToText, 'selection: add next occurrence of selected text (not whole-word)', [], []);
  M.Add(cmd_SelectExpandToWord_Skip, 'selection: skip (don''t select) next occurrence of selected word', [], []);
  M.Add(cmd_SelectExpandToText_Skip, 'selection: skip (don''t select) next occurrence of selected text', [], []);

  M.Add(cmd_CopyLine, 'clipboard: copy current line', [], []);
  M.Add(cmd_CopyFilenameFull, 'clipboard: copy full filepath', [], []);
  M.Add(cmd_CopyFilenameDir, 'clipboard: copy filepath only', [], []);
  M.Add(cmd_CopyFilenameName, 'clipboard: copy filename only', [], []);

  M.Add(cmd_TabUsesSpaces_On, 'tabulation-key uses spaces: turn on', [], []);
  M.Add(cmd_TabUsesSpaces_Off, 'tabulation-key uses spaces: turn off', [], []);
  M.Add(cmd_ToggleTabUsesSpaces, 'tabulation-key uses spaces: toggle', [], []);

  M.Add(cmd_TabSize_Set2, 'tabulation size: set to 2', [], []);
  M.Add(cmd_TabSize_Set4, 'tabulation size: set to 4', [], []);
  M.Add(cmd_TabSize_Set8, 'tabulation size: set to 8', [], []);

  M.Add(cmd_ConvertTabsToSpaces, 'convert tabs (all) to spaces', [], []);
  M.Add(cmd_ConvertTabsToSpacesLeading, 'convert tabs (leading) to spaces', [], []);
  M.Add(cmd_ConvertSpacesToTabsLeading, 'convert spaces (leading) to tabs', [], []);

  M.Add(cmd_Groups1, 'groups: 1 group', [], []);
  M.Add(cmd_Groups2vert, 'groups: 2 groups vert', [], []);
  M.Add(cmd_Groups2horz, 'groups: 2 groups horz', [], []);
  M.Add(cmd_Groups3vert, 'groups: 3 groups vert', [], []);
  M.Add(cmd_Groups3horz, 'groups: 3 groups horz', [], []);
  M.Add(cmd_Groups3plus, 'groups: 1+2 groups vert', [], []);
  M.Add(cmd_Groups3plushorz, 'groups: 1+2 groups horz', [], []);
  M.Add(cmd_Groups4vert, 'groups: 4 groups vert', [], []);
  M.Add(cmd_Groups4horz, 'groups: 4 groups horz', [], []);
  M.Add(cmd_Groups4grid, 'groups: 4 groups grid', [], []);
  M.Add(cmd_Groups6vert, 'groups: 6 groups vert', [], []);
  M.Add(cmd_Groups6horz, 'groups: 6 groups horz', [], []);
  M.Add(cmd_Groups6grid, 'groups: 6 groups grid', [], []);

  M.Add(cmd_GroupActivateNext, 'groups: focus next group', [], []);
  M.Add(cmd_GroupActivatePrev, 'groups: focus previous group', [], []);

  M.Add(cmd_GroupActivate1, 'groups: focus group 1', [], []);
  M.Add(cmd_GroupActivate2, 'groups: focus group 2', [], []);
  M.Add(cmd_GroupActivate3, 'groups: focus group 3', [], []);
  M.Add(cmd_GroupActivate4, 'groups: focus group 4', [], []);
  M.Add(cmd_GroupActivate5, 'groups: focus group 5', [], []);
  M.Add(cmd_GroupActivate6, 'groups: focus group 6', [], []);

  ////not finished: floating window is focused but editor inside is not
  //M.Add(cmd_GroupActivateFloat1, 'groups: focus group floating-1', [], []);
  //M.Add(cmd_GroupActivateFloat2, 'groups: focus group floating-2', [], []);
  //M.Add(cmd_GroupActivateFloat3, 'groups: focus group floating-3', [], []);

  M.Add(cmd_MoveTabToGroupNext, 'groups: move tab to next group', [], []);
  M.Add(cmd_MoveTabToGroupPrev, 'groups: move tab to previous group', [], []);
  M.Add(cmd_MoveTabToGroup1, 'groups: move tab to group 1', [], []);
  M.Add(cmd_MoveTabToGroup2, 'groups: move tab to group 2', [], []);
  M.Add(cmd_MoveTabToGroupFloating1, 'groups: move tab to group floating-1', [], []);
  M.Add(cmd_MoveTabToGroupFloating2, 'groups: move tab to group floating-2', [], []);
  M.Add(cmd_MoveTabToGroupFloating3, 'groups: move tab to group floating-3', [], []);

  M.Add(cmd_BookmarkToggle, 'bookmarks: toggle on current line', [], []);
  M.Add(cmd_BookmarkGotoNext, 'bookmarks: go to next', [], []);
  M.Add(cmd_BookmarkGotoPrev, 'bookmarks: go to previous', [], []);
  M.Add(cmd_BookmarkInvertAll, 'bookmarks: inverse all lines', [], []);
  M.Add(cmd_BookmarkClearAll, 'bookmarks: clear all', [], []);
  M.Add(cmd_BookmarkCopyMarkedLines, 'bookmarks: copy bookmarked lines to clipboard', [], []);
  M.Add(cmd_BookmarkDeleteMarkedLines, 'bookmarks: delete bookmarked lines', [], []);
  M.Add(cmd_BookmarkPlaceCarets, 'bookmarks: place carets on bookmarks', [], []);
  M.Add(cmd_BookmarkPlaceBookmarksOnCarets, 'bookmarks: place bookmarks on carets', [], []);

  M.Add(cmd_DuplicateLineEx, 'duplicate line (advanced)', [], []);

  M.Add(cmd_SetTabColor, 'ui: set tab color...', [], []);
  M.Add(cmd_ResetTabColor, 'ui: reset tab color', [], []);
  M.Add(cmd_ToggleTabPinned, 'ui: toggle tab "pinned" state', [], []);

  M.Add(cmd_SplitTabToggle, 'split tab: toggle "splitted" state', [], []);
  M.Add(cmd_SplitTabHorzVert, 'split tab: toggle "horizontally"/"vertically"', [], []);
  M.Add(cmd_SplitTabNo, 'split tab: do not split', [], []);
  M.Add(cmd_SplitTabHorz, 'split tab: split horizontally', [], []);
  M.Add(cmd_SplitTabVert, 'split tab: split vertically', [], []);
  M.Add(cmd_SplitTab3070, 'split tab: 30/70', [], []);
  M.Add(cmd_SplitTab4060, 'split tab: 40/60', [], []);
  M.Add(cmd_SplitTab5050, 'split tab: 50/50', [], []);
  M.Add(cmd_SplitTab6040, 'split tab: 60/40', [], []);
  M.Add(cmd_SplitTab7030, 'split tab: 70/30', [], []);

  M.Add(cmd_LineEndWin, 'change line ends, for entire document: CR LF', [], []);
  M.Add(cmd_LineEndUnix, 'change line ends, for entire document: LF', [], []);
  M.Add(cmd_LineEndMac, 'change line ends, for entire document: CR', [], []);

  M.Add(cmd_LineEndWin_Caret, 'change line ends, for line(s) with caret: CR LF', [], []);
  M.Add(cmd_LineEndUnix_Caret, 'change line ends, for line(s) with caret: LF', [], []);
  M.Add(cmd_LineEndMac_Caret, 'change line ends, for line(s) with caret: CR', [], []);
  M.Add(cmd_LineEndDefault_Caret, 'change line ends, for line(s) with caret: default', [], []);

  M.Add(cmd_MenuEnc, 'menu: encodings', [], []);
  M.Add(cmd_MenuEnds, 'menu: line ends', [], []);
  M.Add(cmd_MenuLexers, 'menu: lexers (popup)', [], []);
  M.Add(cmd_DialogLexerMenu, 'menu: lexers (dialog)', [], []);

  M.Add(cmd_ChooseTranslation, 'menu: translations', [], []);
  M.Add(cmd_ChooseThemeUI, 'menu: themes', [], []);

  M.Add(cmd_AutoComplete, 'code: auto-completion menu', ['Ctrl+Space'], []);
  M.Add(cmd_GotoDefinition, 'code: go to definition', [], []);
  M.Add(cmd_ShowFunctionHint, 'code: show function-hint', ['Ctrl+Shift+Space'], []);

  M.Add(cmd_FoldingEnable, 'folding: enable folding functionality', [], []);
  M.Add(cmd_FoldingDisable, 'folding: disable folding functionality', [], []);
  M.Add(cmd_DeleteNewColorAttrs, 'clear custom color attributes (added in plugins)', [], []);

  M.Add(cmd_Encoding_utf8bom_NoReload, 'change encoding, no reload: utf8 bom', [], []);
  M.Add(cmd_Encoding_utf8nobom_NoReload, 'change encoding, no reload: utf8 no bom', [], []);
  M.Add(cmd_Encoding_utf16le_NoReload, 'change encoding, no reload: utf16 le', [], []);
  M.Add(cmd_Encoding_utf16be_NoReload, 'change encoding, no reload: utf16 be', [], []);
  M.Add(cmd_Encoding_utf32le_NoReload, 'change encoding, no reload: utf32 le', [], []);
  M.Add(cmd_Encoding_utf32be_NoReload, 'change encoding, no reload: utf32 be', [], []);
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
  M.Add(cmd_Encoding_iso15_NoReload, 'change encoding, no reload: iso15', [], []);
  M.Add(cmd_Encoding_cp437_NoReload, 'change encoding, no reload: cp437', [], []);
  M.Add(cmd_Encoding_cp850_NoReload, 'change encoding, no reload: cp850', [], []);
  M.Add(cmd_Encoding_cp852_NoReload, 'change encoding, no reload: cp852', [], []);
  M.Add(cmd_Encoding_cp866_NoReload, 'change encoding, no reload: cp866', [], []);
  M.Add(cmd_Encoding_cp874_NoReload, 'change encoding, no reload: cp874', [], []);
  M.Add(cmd_Encoding_cp932_NoReload, 'change encoding, no reload: cp932', [], []);
  M.Add(cmd_Encoding_cp936_NoReload, 'change encoding, no reload: cp936', [], []);
  M.Add(cmd_Encoding_cp949_NoReload, 'change encoding, no reload: cp949', [], []);
  M.Add(cmd_Encoding_cp950_NoReload, 'change encoding, no reload: cp950', [], []);

  M.Add(cmd_Encoding_utf8bom_Reload, 'change encoding, reload: utf8 bom', [], []);
  M.Add(cmd_Encoding_utf8nobom_Reload, 'change encoding, reload: utf8 no bom', [], []);
  M.Add(cmd_Encoding_utf16le_Reload, 'change encoding, reload: utf16 le', [], []);
  M.Add(cmd_Encoding_utf16be_Reload, 'change encoding, reload: utf16 be', [], []);
  M.Add(cmd_Encoding_utf32le_Reload, 'change encoding, reload: utf32 le', [], []);
  M.Add(cmd_Encoding_utf32be_Reload, 'change encoding, reload: utf32 be', [], []);
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
  M.Add(cmd_Encoding_iso15_Reload, 'change encoding, reload: iso15', [], []);
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
  M.Add(cmd_Markers_SelectToCaret,      'markers: select to last marker', [], []);
  M.Add(cmd_Markers_DeleteToCaret,      'markers: delete to last marker', [], []);

  M.Add(cmd_MacroStart, 'start/stop macro recording', [], []);

  M.Add(cmd_LinkAtCaret_Open, 'url: open url at 1st caret ', [], []);
  M.Add(cmd_LinkAtCaret_Copy, 'url: copy url at 1st caret', [], []);
  M.Add(cmd_LinkAtPopup_Open, 'url: open url of right-click', [], []);
  M.Add(cmd_LinkAtPopup_Copy, 'url: copy url of right-click', [], []);

  M.Add(cmd_TreeGotoNext, 'code tree: select next node', [], []);
  M.Add(cmd_TreeGotoPrev, 'code tree: select previous node', [], []);
  M.Add(cmd_TreeGotoParent, 'code tree: select parent node', [], []);
  M.Add(cmd_TreeGotoNextBrother, 'code tree: select next brother node', [], []);
  M.Add(cmd_TreeGotoPrevBrother, 'code tree: select previous brother node', [], []);
  M.Add(cmd_TreeUpdate, 'code tree: update tree panel', [], []);
  M.Add(cmd_TreeGotoBlockForCurNode, 'code tree: go to block for current node', [], []);
  M.Add(cmd_TreeSelectBlockForCurNode, 'code tree: go to block for current node, and select', [], []);
  M.Add(cmd_TreeFilterClear, 'code tree: clear filter', [], []);
  M.Add(cmd_TreeFilterFocus, 'code tree: focus filter', [], []);
  M.Add(cmd_TreeFocus, 'code tree: focus treeview', [], []);

  M.Add(cmd_BracketHighlightOn, 'brackets: pair highlight: turn on', [], []);
  M.Add(cmd_BracketHighlightOff, 'brackets: pair highlight: turn off', [], []);
  M.Add(cmd_BracketHighlightToggle, 'brackets: pair highlight: toggle on/off', [], []);
  M.Add(cmd_BracketJump, 'brackets: jump to pair', [], []);
  M.Add(cmd_BracketSelect, 'brackets: select to pair', [], []);
  M.Add(cmd_BracketSelectInside, 'brackets: select to pair, inside', [], []);

  M.Add(cmd_HelpAbout, 'help: about', [], []);
  M.Add(cmd_HelpForum, 'help: forum', [], []);
  M.Add(cmd_HelpWiki, 'help: wiki', [], []);
  M.Add(cmd_HelpIssues, 'help: issues', [], []);
end;


function IsCommandNeedTimer(Cmd: integer): boolean;
begin
  if Cmd<cmdFirstAppCommand then exit(false);
  if Cmd>cmdLastAppCommand then exit(false);

  case Cmd of
    cmdFirstLexerCommand..cmdLastLexerCommand,
    cmdFirstPluginCommand..cmdLastPluginCommand,
    cmdFirstFileCommand..cmdLastFileCommand,
    cmdFirstRecentCommand..cmdLastRecentCommand:
      Result:= true;

    cmd_FileNew,
    cmd_FileNewMenu,
    cmd_FileOpen,
    cmd_FileOpen_NoPlugins,
    cmd_FileOpen_TextViewer,
    cmd_FileOpen_HexViewer,
    cmd_FileOpen_UnicodeViewer,
    cmd_FileReopen,
    cmd_FileExit,
    cmd_FileClose,
    cmd_FileCloseOtherThis,
    cmd_FileCloseOtherAll,
    cmd_FileCloseAll,
    cmd_FileCloseAndDelete,
    cmd_FileExportHtml,
    cmd_FileReopenRecent,
    cmd_ToggleFocusSplitEditors,
    cmd_FocusEditor,
    cmd_FocusNotificationPanel,
    cmd_HideNotificationPanels,

    cmd_OpsClearRecent,
    cmd_OpsClearSearchHistory,
    cmd_OpsClearConsoleHistory,
    cmd_OpsOpenDefault,
    cmd_OpsOpenUser,
    cmd_OpsOpenDefaultAndUser,
    cmd_OpsOpenLexerSpecific,
    cmd_OpsFontText,
    cmd_OpsFontUi,
    cmd_OpsFontOutput,
    cmd_DialogSaveTabs,
    cmd_ToggleFullScreen,
    cmd_ToggleDistractionFree,
    cmd_OpsReloadAndApply,
    cmd_DialogLexerProp,
    cmd_DialogLexerLib,
    cmd_DialogLexerStyleMap,
    cmd_DialogLexerMenu,
    cmd_DialogThemeUi,
    cmd_DialogThemeSyntax,
    cmd_ToggleSidePanel,
    cmd_ToggleBottomPanel,
    //cmd_ShowPanelConsole,
    //cmd_ShowPanelOutput,
    //cmd_ShowPanelValidate,
    cmd_ToggleFindDialog,
    cmd_ToggleFindDialog_AndFocus,
    cmd_ToggleReplaceDialog,
    cmd_ToggleSidebar,
    cmd_ToggleToolbar,
    cmd_ToggleStatusbar,
    cmd_ToggleUiTabs,
    cmd_DialogCharMap,
    cmd_RunLastCommandPlugin,

    cmd_ResetPythonPlugins,
    cmd_RescanPythonPluginsInfFiles,
    cmd_FindPythonLib,

    cmd_DialogGotoBookmark,
    cmd_DialogCommands,

    cmd_SplitTabNo,
    cmd_SplitTabHorz,
    cmd_SplitTabVert,
    cmd_SplitTabToggle,
    cmd_SplitTabHorzVert,
    cmd_SplitTab3070,
    cmd_SplitTab4060,
    cmd_SplitTab5050,
    cmd_SplitTab6040,
    cmd_SplitTab7030,

    cmd_Groups1,
    cmd_Groups2horz,
    cmd_Groups2vert,
    cmd_Groups3horz,
    cmd_Groups3vert,
    cmd_Groups3plus,
    cmd_Groups3plushorz,
    cmd_Groups4horz,
    cmd_Groups4vert,
    cmd_Groups4grid,
    cmd_Groups6horz,
    cmd_Groups6vert,
    cmd_Groups6grid,

    cmd_GroupActivateNext,
    cmd_GroupActivatePrev,
    cmd_GroupActivate1,
    cmd_GroupActivate2,
    cmd_GroupActivate3,
    cmd_GroupActivate4,
    cmd_GroupActivate5,
    cmd_GroupActivate6,
    cmd_GroupActivateFloat1,
    cmd_GroupActivateFloat2,
    cmd_GroupActivateFloat3,

    cmd_MoveTabToGroupNext,
    cmd_MoveTabToGroupPrev,
    cmd_MoveTabToGroup1,
    cmd_MoveTabToGroup2,
    cmd_MoveTabToGroupFloating1,
    cmd_MoveTabToGroupFloating2,
    cmd_MoveTabToGroupFloating3,

    cmd_MenuEnc,
    cmd_MenuEnds,
    cmd_MenuLexers,

    cmd_ChooseTranslation,
    cmd_ChooseThemeUI,

    cmd_AutoComplete,
    cmd_GotoDefinition,
    cmd_ShowFunctionHint,

    cmd_HelpAbout,
    cmd_HelpForum,
    cmd_HelpWiki,
    cmd_HelpIssues:
      Result:= true;

    else
      Result:= false;
  end;
end;

function IsCommandHandledFromFindDialog(Cmd: integer): boolean;
begin
  case Cmd of
    cmd_SwitchTab_HotkeyNext,
    cmd_SwitchTab_HotkeyPrev,
    cmd_SwitchTab_SimpleNext,
    cmd_SwitchTab_SimplePrev,
    cmd_SwitchTab_Dialog,
    cmd_SwitchTab_Recent,

    cmd_ShowPanelConsole_AndFocus,
    cmd_ShowPanelOutput_AndFocus,
    cmd_ShowPanelValidate_AndFocus,
    cmd_FocusEditor,
    cmd_FocusNotificationPanel,
    cmd_TreeFilterFocus,
    cmd_TreeFocus,

    cmd_GroupActivateNext,
    cmd_GroupActivatePrev,
    cmd_GroupActivate1,
    cmd_GroupActivate2,
    cmd_GroupActivate3,
    cmd_GroupActivate4,
    cmd_GroupActivate5,
    cmd_GroupActivate6:
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
    cmdFirstFileCommand..cmdLastFileCommand,
    cmdFirstRecentCommand..cmdLastRecentCommand,
    cmd_MacroStart,
    cmd_DialogCommands,
    cmd_DialogThemeUi,
    cmd_DialogThemeSyntax,
    cmd_DialogCharMap,
    cmd_DialogFind,
    cmd_DialogReplace,
    cmd_DialogGoto,
    cmd_DialogGotoBookmark,
    cmd_DialogLexerLib,
    cmd_DialogLexerProp,
    cmd_DialogLexerStyleMap,
    cmd_DialogLexerMenu,
    cmd_DialogSaveTabs,
    cmd_FileNew,
    cmd_FileNewMenu,
    cmd_FileOpen,
    cmd_FileOpen_TextViewer,
    cmd_FileOpen_HexViewer,
    cmd_FileOpen_UnicodeViewer,
    cmd_FileOpen_NoPlugins,
    cmd_FileOpenFolder,
    cmd_FileSaveAs,
    cmd_FileExit,
    cmd_FileClose,
    cmd_FileCloseOtherThis,
    cmd_FileCloseOtherAll,
    cmd_FileCloseAll,
    cmd_FileCloseAndDelete,
    cmd_FileExportHtml,
    cmd_FileReopenRecent,
    cmd_ToggleFocusSplitEditors,
    cmd_FocusEditor,
    cmd_ToggleBottomPanel,
    cmd_ToggleSidePanel,
    cmd_ToggleFindDialog,
    cmd_ToggleFindDialog_AndFocus,
    cmd_ToggleReplaceDialog,
    cmd_ToggleFullScreen,
    cmd_ToggleDistractionFree,
    cmd_ToggleSidebar,
    cmd_ToggleStatusbar,
    cmd_ToggleToolbar,
    cmd_ToggleUiTabs,
    cmd_TabUsesSpaces_On,
    cmd_TabUsesSpaces_Off,
    cmd_ToggleTabUsesSpaces,
    cmd_ConvertTabsToSpaces,
    cmd_ConvertSpacesToTabsLeading,
    cmd_ConvertTabsToSpacesLeading,
    cmd_Groups1,
    cmd_Groups2horz,
    cmd_Groups2vert,
    cmd_Groups3horz,
    cmd_Groups3vert,
    cmd_Groups3plus,
    cmd_Groups3plushorz,
    cmd_Groups4horz,
    cmd_Groups4vert,
    cmd_Groups4grid,
    cmd_Groups6horz,
    cmd_Groups6vert,
    cmd_Groups6grid,
    cmd_GroupActivateNext,
    cmd_GroupActivatePrev,
    cmd_GroupActivate1,
    cmd_GroupActivate2,
    cmd_GroupActivate3,
    cmd_GroupActivate4,
    cmd_GroupActivate5,
    cmd_GroupActivate6,
    cmd_GroupActivateFloat1,
    cmd_GroupActivateFloat2,
    cmd_GroupActivateFloat3,
    cmd_MenuEnc,
    cmd_MenuEnds,
    cmd_MenuLexers,

    cmd_Markers_SelectToCaret,
    cmd_Markers_DeleteToCaret,
    cmd_Markers_DropAtCaret,
    cmd_Markers_GotoLastNoDelete,
    cmd_Markers_GotoLastAndDelete,
    cmd_Markers_ClearAll,
    cmd_Markers_SwapCaretAndMarker,

    cmd_LinkAtCaret_Open,
    cmd_LinkAtCaret_Copy,

    cmd_BracketHighlightOn,
    cmd_BracketHighlightOff,
    cmd_BracketHighlightToggle,
    cmd_BracketJump,
    cmd_BracketSelect,
    cmd_BracketSelectInside,

    cmd_TabSize_Set2,
    cmd_TabSize_Set4,
    cmd_TabSize_Set8,

    cmd_ResetPythonPlugins,
    cmd_RescanPythonPluginsInfFiles,
    cmd_FindPythonLib,

    cmd_HelpAbout,
    cmd_HelpCheckUpdates,
    cmd_HelpForum,
    cmd_HelpWiki,
    cmd_HelpIssues:

      Result:= false;
    else
      Result:= true;
  end;
end;


end.

