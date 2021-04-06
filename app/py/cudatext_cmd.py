#prefix cCommand: commands of editor core ATSynEdit
#prefix cmd: high-level commands of CudaText

_CmdFlag_SelKeep    = 0x10000 #cmd continues selection (new caret pos makes bigger selection)
_CmdFlag_SelReset   = 0x20000 #before command, reset selection
_CmdFlag_MovesCaret = 0x80000 #cmd moves caret and makes new undo-group

_base_KeyUp       = 100 | _CmdFlag_MovesCaret
_base_KeyDown     = 101 | _CmdFlag_MovesCaret
_base_KeyLeft     = 102 | _CmdFlag_MovesCaret
_base_KeyRight    = 103 | _CmdFlag_MovesCaret
_base_KeyHome     = 104 | _CmdFlag_MovesCaret
_base_KeyEnd      = 105 | _CmdFlag_MovesCaret
_base_KeyPageUp   = 106 | _CmdFlag_MovesCaret
_base_KeyPageDown = 107 | _CmdFlag_MovesCaret

cCommand_KeyUp           = _base_KeyUp | _CmdFlag_SelReset
cCommand_KeyDown         = _base_KeyDown | _CmdFlag_SelReset
cCommand_KeyLeft         = _base_KeyLeft #handles sel
cCommand_KeyRight        = _base_KeyRight #handles sel
cCommand_KeyHome         = _base_KeyHome | _CmdFlag_SelReset
cCommand_KeyEnd          = _base_KeyEnd | _CmdFlag_SelReset
cCommand_KeyPageUp       = _base_KeyPageUp | _CmdFlag_SelReset
cCommand_KeyPageDown     = _base_KeyPageDown | _CmdFlag_SelReset

cCommand_KeyUp_Sel       = _base_KeyUp | _CmdFlag_SelKeep
cCommand_KeyDown_Sel     = _base_KeyDown | _CmdFlag_SelKeep
cCommand_KeyLeft_Sel     = _base_KeyLeft | _CmdFlag_SelKeep
cCommand_KeyRight_Sel    = _base_KeyRight | _CmdFlag_SelKeep
cCommand_KeyHome_Sel     = _base_KeyHome | _CmdFlag_SelKeep
cCommand_KeyEnd_Sel      = _base_KeyEnd | _CmdFlag_SelKeep
cCommand_KeyPageUp_Sel   = _base_KeyPageUp | _CmdFlag_SelKeep
cCommand_KeyPageDown_Sel = _base_KeyPageDown | _CmdFlag_SelKeep

cCommand_ColSelectUp    = 110
cCommand_ColSelectDown  = 111
cCommand_ColSelectLeft  = 112
cCommand_ColSelectRight = 113
cCommand_ColSelectToLineBegin = 114
cCommand_ColSelectToLineEnd = 115
cCommand_ColSelectPageUp = 116
cCommand_ColSelectPageDown = 117

cCommand_ColSelectWithoutKey_On = 130
cCommand_ColSelectWithoutKey_Off = 131
cCommand_ColSelectWithoutKey_Toggle = 132

cCommand_RemoveFirstCaret = 135
cCommand_RemoveLastCaret = 136

cCommand_TextInsert = 150
cCommand_TextInsertTabChar = 151
cCommand_KeyBackspace = 152
cCommand_KeyDelete = 153
cCommand_KeyEnter = 154
cCommand_KeyTab = 155

cCommand_ForceFinalEndOfLine = 160

cCommand_TextDeleteSelection = 170
cCommand_TextDeleteLine = 171
cCommand_TextDuplicateLine = 172
cCommand_TextDeleteToLineBegin = 173 | _CmdFlag_SelReset
cCommand_TextDeleteToLineEnd = 174 | _CmdFlag_SelReset
cCommand_TextDeleteToTextEnd = 175 | _CmdFlag_SelReset
cCommand_TextDeleteWordNext = 176 | _CmdFlag_SelReset
cCommand_TextDeleteWordPrev = 177 | _CmdFlag_SelReset
cCommand_TextDeleteToTextBegin = 178 | _CmdFlag_SelReset
cCommand_TextDeleteWordEntire = 179 | _CmdFlag_SelReset

_base_GotoTextBegin = 200 | _CmdFlag_MovesCaret
_base_GotoTextEnd   = 201 | _CmdFlag_MovesCaret
_base_GotoWordNext  = 202 | _CmdFlag_MovesCaret
_base_GotoWordPrev  = 203 | _CmdFlag_MovesCaret
_base_GotoWordEnd   = 204 | _CmdFlag_MovesCaret
_base_GotoWordNext_Simple = 205 | _CmdFlag_MovesCaret
_base_GotoWordPrev_Simple = 206 | _CmdFlag_MovesCaret

cCommand_GotoTextBegin = _base_GotoTextBegin | _CmdFlag_SelReset
cCommand_GotoTextEnd = _base_GotoTextEnd | _CmdFlag_SelReset
cCommand_GotoWordNext = _base_GotoWordNext | _CmdFlag_SelReset
cCommand_GotoWordPrev = _base_GotoWordPrev | _CmdFlag_SelReset
cCommand_GotoWordNext_Simple = _base_GotoWordNext_Simple | _CmdFlag_SelReset
cCommand_GotoWordPrev_Simple = _base_GotoWordPrev_Simple | _CmdFlag_SelReset
cCommand_GotoWordEnd = _base_GotoWordEnd | _CmdFlag_SelReset

cCommand_GotoTextBegin_Sel = _base_GotoTextBegin | _CmdFlag_SelKeep
cCommand_GotoTextEnd_Sel = _base_GotoTextEnd | _CmdFlag_SelKeep
cCommand_GotoWordNext_Sel = _base_GotoWordNext | _CmdFlag_SelKeep
cCommand_GotoWordPrev_Sel = _base_GotoWordPrev | _CmdFlag_SelKeep
cCommand_GotoWordNext_Simple_Sel = _base_GotoWordNext_Simple | _CmdFlag_SelKeep
cCommand_GotoWordPrev_Simple_Sel = _base_GotoWordPrev_Simple | _CmdFlag_SelKeep
cCommand_GotoWordEnd_Sel = _base_GotoWordEnd | _CmdFlag_SelKeep

cCommand_GotoScreenTop = 215 | _CmdFlag_MovesCaret
cCommand_GotoScreenBottom = 216 | _CmdFlag_MovesCaret
cCommand_GotoScreenCenter = 217 | _CmdFlag_MovesCaret

_base_GotoLineAbsBegin = 210 | _CmdFlag_MovesCaret
_base_GotoLineAbsEnd   = 211 | _CmdFlag_MovesCaret

cCommand_GotoLineAbsBegin     = _base_GotoLineAbsBegin | _CmdFlag_SelReset
cCommand_GotoLineAbsBegin_Sel = _base_GotoLineAbsBegin | _CmdFlag_SelKeep
cCommand_GotoLineAbsEnd       = _base_GotoLineAbsEnd | _CmdFlag_SelReset
cCommand_GotoLineAbsEnd_Sel   = _base_GotoLineAbsEnd | _CmdFlag_SelKeep

cCommand_Undo = 235 | _CmdFlag_SelReset
cCommand_Redo = 236 | _CmdFlag_SelReset

cCommand_TextIndent = 240
cCommand_TextUnindent = 241

cCommand_ScrollPageUp = 248
cCommand_ScrollPageDown = 249
cCommand_ScrollLineUp = 250
cCommand_ScrollLineDown = 251
cCommand_ScrollToCaretTop = 252
cCommand_ScrollToCaretBottom = 253
cCommand_ScrollToCaretLeft = 254
cCommand_ScrollToCaretRight = 255
cCommand_ScrollColumnLeft = 256
cCommand_ScrollColumnRight = 257
cCommand_ScrollToBegin = 258
cCommand_ScrollToEnd = 259

cCommand_SelectAll = 260 | _CmdFlag_SelReset | _CmdFlag_MovesCaret
cCommand_SelectNone = 261 | _CmdFlag_SelReset | _CmdFlag_MovesCaret
cCommand_SelectWords = 262 | _CmdFlag_SelReset | _CmdFlag_MovesCaret
cCommand_SelectLines = 263 | _CmdFlag_SelReset | _CmdFlag_MovesCaret
cCommand_SelectInverted = 264 | _CmdFlag_MovesCaret
cCommand_SelectSplitToLines = 265 | _CmdFlag_MovesCaret
cCommand_SelectExtendByLine = 266 | _CmdFlag_MovesCaret

cCommand_MoveSelectionUp = 268
cCommand_MoveSelectionDown = 269
cCommand_TextInsertEmptyAbove = 270 | _CmdFlag_SelReset | _CmdFlag_MovesCaret
cCommand_TextInsertEmptyBelow = 271 | _CmdFlag_SelReset | _CmdFlag_MovesCaret

cCommand_ToggleOverwrite = 300
cCommand_ToggleReadOnly = 301
cCommand_ToggleWordWrap = 302
cCommand_ToggleUnprinted = 303
cCommand_ToggleUnprintedSpaces = 304
cCommand_ToggleUnprintedEnds = 305
cCommand_ToggleUnprintedEndDetails = 306
cCommand_ToggleLineNums = 307
cCommand_ToggleFolding = 308
cCommand_ToggleRuler = 309
cCommand_ToggleMinimap = 310
cCommand_ToggleMicromap = 311
cCommand_ToggleWordWrapAlt = 312
cCommand_ToggleUnprintedSpacesTrailing = 320

cCommand_Sort_Asc        = 330
cCommand_Sort_AscNoCase  = 331
cCommand_Sort_Desc       = 332
cCommand_Sort_DescNoCase = 333

cCommand_DeleteAllBlanks      = 340
cCommand_DeleteAdjacentBlanks = 341
cCommand_DeleteAdjacentDups   = 342
cCommand_DeleteAllDups        = 343
cCommand_DeleteAllDupsKeepBlanks = 344
cCommand_ReverseLines = 350
cCommand_ShuffleLines = 351

cCommand_ClipboardPaste = 1000
cCommand_ClipboardPaste_Select = 1001
cCommand_ClipboardPaste_KeepCaret = 1002
cCommand_ClipboardPaste_Column = 1003 | _CmdFlag_SelReset
cCommand_ClipboardPaste_ColumnKeepCaret = 1004 | _CmdFlag_SelReset
cCommand_ClipboardCopy = 1006
cCommand_ClipboardCopyAdd = 1007
cCommand_ClipboardCut = 1008

#these use "Primary selection" (alternative clipboard on gtk2)
cCommand_ClipboardAltPaste = 1010
cCommand_ClipboardAltPaste_Select = 1011
cCommand_ClipboardAltPaste_KeepCaret = 1012
cCommand_ClipboardAltPaste_Column = 1013 or _CmdFlag_SelReset
cCommand_ClipboardAltPaste_ColumnKeepCaret = 1014 or _CmdFlag_SelReset
#these use "Secondary selection" (alternative clipboard on gtk2)
cCommand_ClipboardAltAltPaste = 1015

cCommand_TextCaseLower = 1020
cCommand_TextCaseUpper = 1021
cCommand_TextCaseTitle = 1022
cCommand_TextCaseInvert = 1023
cCommand_TextCaseSentence = 1024

cCommand_TextTrimSpacesLeft = 1026
cCommand_TextTrimSpacesRight = 1027
cCommand_TextTrimSpacesAll = 1028

cCommand_UnfoldAll = 1029
cCommand_FoldAll = 1030
cCommand_FoldLevel1 = 1031
cCommand_FoldLevel2 = 1032
cCommand_FoldLevel3 = 1033
cCommand_FoldLevel4 = 1034
cCommand_FoldLevel5 = 1035
cCommand_FoldLevel6 = 1036
cCommand_FoldLevel7 = 1037
cCommand_FoldLevel8 = 1038
cCommand_FoldLevel9 = 1039

cCommand_FoldingFoldAtCurLine = 1040
cCommand_FoldingUnfoldAtCurLine = 1041
cCommand_FoldingToggleAtCurLine = 1042
cCommand_FoldingFoldSelection = 1043

cCommand_Cancel = 2001
cCommand_RepeatTextCommand = 2002
cCommand_ZoomIn = 2003
cCommand_ZoomOut = 2004
cCommand_GotoLastEditPos = 2006
cCommand_ZoomReset = 2007

cCommand_CaretsExtendDownLine = 2010
cCommand_CaretsExtendDownPage = 2011
cCommand_CaretsExtendDownToEnd = 2012
cCommand_CaretsExtendUpLine = 2013
cCommand_CaretsExtendUpPage = 2014
cCommand_CaretsExtendUpToTop = 2015

cmd_MouseClickAtCursor = 2480
cmd_MouseClickAtCursorAndSelect = 2481
cmd_MouseClickNearCaret = 2490
cmd_MouseClick = cmd_MouseClickNearCaret
cmd_MouseSelect = 2491
cmd_FinderAction = 2492

cmd_FileNew            = 2500
cmd_FileOpen           = 2501
cmd_FileSave           = 2502
cmd_FileSaveAs         = 2503
cmd_FileSaveAll        = 2504
cmd_FileReopen         = 2505
cmd_FileExit           = 2506
cmd_FileOpen_NoPlugins = 2507
cmd_FileNewMenu        = 2508
cmd_FileOpenFolder     = 2509
cmd_FileClose          = 2510
cmd_FileCloseOtherThis = 2511
cmd_FileCloseOtherAll  = 2512
cmd_FileCloseAll       = 2513
cmd_FileCloseAndDelete = 2514
cmd_FileExportHtml     = 2515
cmd_RepaintEditor      = 2516

cmd_OpsOpenDefaultAndUser = 2519
cmd_OpsClearRecent     = 2520
cmd_OpsOpenDefault     = 2521
cmd_OpsOpenUser        = 2522
cmd_OpsOpenLexerOvr    = 2523
cmd_OpsFontText        = 2525
cmd_OpsFontUi          = 2526
cmd_OpsFontOutput      = 2527
cmd_ToggleFullScreen   = 2528
cmd_OpsReloadAndApply  = 2529
cmd_DialogLexerProp    = 2530
cmd_DialogLexerLib     = 2531
cmd_ToggleDistractionFree = 2532
cmd_ToggleSidePanel    = 2533
cmd_ToggleBottomPanel  = 2534
cmd_ShowPanelConsole   = 2535
cmd_ShowPanelOutput    = 2536
cmd_ShowPanelValidate  = 2537
cmd_ToggleFindDialog   = 2538
cmd_ToggleOnTop        = 2539
cmd_DialogLoadLexerStyles = 2540 #deleted already
cmd_ToggleSidebar      = 2540
cmd_ToggleToolbar      = 2541
cmd_ToggleStatusbar    = 2542
cmd_ResetPythonPlugins = 2543
cmd_DialogCharMap      = 2544
cmd_RunLastCommandPlugin = 2545
cmd_ShowSidePanelAsIs = 2546
cmd_ShowSidePanelAndSyntaxTree = 2547
cmd_HideSidePanel = 2548
cmd_DialogSaveTabs = 2549
cmd_DialogLexerStyleMap = 2550
cmd_RescanPythonPluginsInfFiles = 2551
cmd_DialogThemeUi       = 2552
cmd_DialogThemeSyntax   = 2553
cmd_ShowMainMenuAsPopup = 2554
cmd_DialogLexerMenu = 2555
cmd_ToggleFloatSide = 2556
cmd_ToggleFloatBottom = 2557
cmd_HideBottomPanel = 2558
cmd_OpsFontSizeBigger = 2559
cmd_OpsFontSizeSmaller = 2560
cmd_ShowPanelConsole_AndFocus  = 2561
cmd_ShowPanelOutput_AndFocus   = 2562
cmd_ShowPanelValidate_AndFocus = 2563
cmd_ToggleSidePanelAndSyntaxTree = 2565
cmd_OpsFontSizeReset = 2566
cmd_FindPythonLib = 2567

cmd_ChooseTranslation = 2570
cmd_ChooseThemeUI     = 2571

cmd_OpsClearSearchHistory = 2573
cmd_OpsClearConsoleHistory = 2574

cmd_ToggleUiTabs = 2575
cmd_ToggleFocusSplitEditors = 2576
cmd_FocusEditor = 2577
cmd_FocusNotificationPanel = 2578
cmd_HideNotificationPanels = 2579

cmd_DialogGoto       = 2580
cmd_DialogGotoBookmark = 2581
cmd_DialogCommands   = 2582
cmd_DialogFind       = 2584
cmd_DialogReplace    = 2585
cmd_DialogFind_Hide  = 2586

cmd_FindFirst        = 2589
cmd_FindNext         = 2590
cmd_FindPrev         = 2591
cmd_FindCurWordNext  = 2592
cmd_FindCurWordPrev  = 2593
cmd_FindCurSelNext   = 2594
cmd_FindCurSelPrev   = 2595
cmd_FindAllAndSelect = 2596
cmd_FindAllAndMarkers = 2597
cmd_FindAllAndBookmarks = 2598
cmd_FindMarkAll      = cmd_FindAllAndMarkers
cmd_SelectExpandToWord = 2600

cmd_FileOpen_TextViewer    = 2603
cmd_FileOpen_HexViewer     = 2604
cmd_FileOpen_UnicodeViewer = 2605

cmd_SwitchTab_HotkeyNext = 2610
cmd_SwitchTab_HotkeyPrev = 2611
cmd_SwitchTab_SimpleNext = 2612
cmd_SwitchTab_SimplePrev = 2613
cmd_SwitchTab_Dialog     = 2614
cmd_SwitchTab_Recent     = 2615

cmd_SplitTabHorz     = 2617
cmd_SplitTabVert     = 2618
cmd_SplitTabNo       = 2619
cmd_SplitTabToggle   = 2620
cmd_SplitTabHorzVert = 2621
cmd_SplitTab3070     = 2622
cmd_SplitTab4060     = 2623
cmd_SplitTab5050     = 2624
cmd_SplitTab6040     = 2625
cmd_SplitTab7030     = 2626

cmd_Groups6vert = 2627
cmd_Groups6horz = 2628
cmd_Groups3plushorz = 2629
cmd_Groups1     = 2630
cmd_Groups2horz = 2631
cmd_Groups2vert = 2632
cmd_Groups3horz = 2633
cmd_Groups3vert = 2634
cmd_Groups3plus = 2635
cmd_Groups4horz = 2636
cmd_Groups4vert = 2637
cmd_Groups4grid = 2638
cmd_Groups6grid = 2639

cmd_GroupActivateNext = 2640
cmd_GroupActivatePrev = 2641

cmd_MoveTabToGroupNext = 2642
cmd_MoveTabToGroupPrev = 2643
cmd_MoveTabToGroup1 = 2644
cmd_MoveTabToGroup2 = 2645
cmd_MoveTabToGroupFloating1 = 2646
cmd_MoveTabToGroupFloating2 = 2647
cmd_MoveTabToGroupFloating3 = 2648

cmd_CopyLine         = 2650
cmd_CopyFilenameFull = 2651
cmd_CopyFilenameDir  = 2652
cmd_CopyFilenameName = 2653

cmd_SortAsc          = 2654 #deleted
cmd_SortDesc         = 2655 #deleted

cmd_ToggleTabUsesSpaces = 2657
cmd_ConvertTabsToSpaces = 2658
cmd_ConvertSpacesToTabsLeading = 2659
cmd_ConvertTabsToSpacesLeading = 2660

cmd_BookmarkToggle    = 2661
cmd_BookmarkInvertAll = 2662
cmd_BookmarkClearAll  = 2663
cmd_BookmarkGotoNext  = 2664
cmd_BookmarkGotoPrev  = 2665
cmd_BookmarkPlaceCarets = 2667
cmd_BookmarkCopyMarkedLines = 2668
cmd_BookmarkDeleteMarkedLines = 2669
cmd_BookmarkPlaceBookmarksOnCarets = 2670

cmd_SetTabColor       = 2671
cmd_ResetTabColor     = 2672
cmd_ToggleTabPinned   = 2675
cmd_DuplicateLineEx   = 2676

cmd_LineEndWin        = 2677
cmd_LineEndUnix       = 2678
cmd_LineEndMac        = 2679

cmd_DeleteNewColorAttrs    = 2683
cmd_FoldingEnable          = 2684
cmd_FoldingDisable         = 2685

cmd_MenuEnc           = 2691
cmd_MenuEnds          = 2692
cmd_MenuLexers        = 2693

cmd_AutoComplete      = 2695
cmd_GotoDefinition    = 2696
cmd_ShowFunctionHint  = 2697

cmd_HelpAbout     = 2700
cmd_HelpForum     = 2701
cmd_HelpWiki      = 2702
cmd_HelpMouse     = 2703
cmd_HelpChangelog = 2704
cmd_HelpLexers    = 2705
cmd_HelpIssues    = 2706
cmd_HelpHotkeys   = 2707
cmd_HelpCheckUpdates = 2708

cmd_Encoding_ansi_NoReload      = 2710
cmd_Encoding_utf8bom_NoReload   = 2711
cmd_Encoding_utf8nobom_NoReload = 2712
cmd_Encoding_utf16le_NoReload   = 2713
cmd_Encoding_utf16be_NoReload   = 2714
cmd_Encoding_cp1250_NoReload    = 2715
cmd_Encoding_cp1251_NoReload    = 2716
cmd_Encoding_cp1252_NoReload    = 2717
cmd_Encoding_cp1253_NoReload    = 2718
cmd_Encoding_cp1254_NoReload    = 2719
cmd_Encoding_cp1255_NoReload    = 2720
cmd_Encoding_cp1256_NoReload    = 2721
cmd_Encoding_cp1257_NoReload    = 2722
cmd_Encoding_cp1258_NoReload    = 2723
cmd_Encoding_mac_NoReload       = 2724
cmd_Encoding_iso1_NoReload      = 2725
cmd_Encoding_iso2_NoReload      = 2726
cmd_Encoding_iso15_NoReload     = 2727
cmd_Encoding_cp437_NoReload     = 2730
cmd_Encoding_cp850_NoReload     = 2731
cmd_Encoding_cp852_NoReload     = 2732
cmd_Encoding_cp866_NoReload     = 2733
cmd_Encoding_cp874_NoReload     = 2734
cmd_Encoding_cp932_NoReload     = 2735
cmd_Encoding_cp936_NoReload     = 2736
cmd_Encoding_cp949_NoReload     = 2737
cmd_Encoding_cp950_NoReload     = 2738
cmd_Encoding_utf32le_NoReload   = 2739
cmd_Encoding_utf32be_NoReload   = 2740

cmd_Encoding_ansi_Reload      = 2750
cmd_Encoding_utf8bom_Reload   = 2751
cmd_Encoding_utf8nobom_Reload = 2752
cmd_Encoding_utf16le_Reload   = 2753
cmd_Encoding_utf16be_Reload   = 2754
cmd_Encoding_cp1250_Reload    = 2755
cmd_Encoding_cp1251_Reload    = 2756
cmd_Encoding_cp1252_Reload    = 2757
cmd_Encoding_cp1253_Reload    = 2758
cmd_Encoding_cp1254_Reload    = 2759
cmd_Encoding_cp1255_Reload    = 2760
cmd_Encoding_cp1256_Reload    = 2761
cmd_Encoding_cp1257_Reload    = 2762
cmd_Encoding_cp1258_Reload    = 2763
cmd_Encoding_mac_Reload       = 2764
cmd_Encoding_iso1_Reload      = 2765
cmd_Encoding_iso2_Reload      = 2766
cmd_Encoding_iso15_Reload     = 2767
cmd_Encoding_cp437_Reload     = 2770
cmd_Encoding_cp850_Reload     = 2771
cmd_Encoding_cp852_Reload     = 2772
cmd_Encoding_cp866_Reload     = 2773
cmd_Encoding_cp874_Reload     = 2774
cmd_Encoding_cp932_Reload     = 2775
cmd_Encoding_cp936_Reload     = 2776
cmd_Encoding_cp949_Reload     = 2777
cmd_Encoding_cp950_Reload     = 2778
cmd_Encoding_utf32le_Reload   = 2779
cmd_Encoding_utf32be_Reload   = 2780

cmd_Markers_SelectToCaret      = 2798
cmd_Markers_DeleteToCaret      = 2799
cmd_Markers_DropAtCaret        = 2800
cmd_Markers_GotoLastNoDelete   = 2801
cmd_Markers_GotoLastAndDelete  = 2802
cmd_Markers_ClearAll           = 2803
cmd_Markers_SwapCaretAndMarker = 2804

cmd_LinkAtCaret_Open           = 2806
cmd_LinkAtCaret_Copy           = 2807
cmd_LinkAtPopup_Open           = 2808
cmd_LinkAtPopup_Copy           = 2809

cmd_MacroStart                 = 2810
cmd_MacroStop                  = 2811
cmd_MacroCancel                = 2812

cmd_TreeGotoNext               = 2815
cmd_TreeGotoPrev               = 2816
cmd_TreeGotoParent             = 2817
cmd_TreeGotoNextBrother        = 2818
cmd_TreeGotoPrevBrother        = 2819
cmd_TreeUpdate                 = 2820
cmd_TreeSelectBlockForCurNode  = 2821
cmd_TreeGotoBlockForCurNode    = 2822
cmd_TreeFilterClear            = 2823
cmd_TreeFilterFocus            = 2824
cmd_TreeFocus                  = 2825

cmd_BracketHighlightOn         = 2840
cmd_BracketHighlightOff        = 2841
cmd_BracketHighlightToggle     = 2842
cmd_BracketJump                = 2845
cmd_BracketSelect              = 2846
cmd_BracketSelectInside        = 2847

cmd_GroupActivate1             = 2901
cmd_GroupActivate2             = 2902
cmd_GroupActivate3             = 2903
cmd_GroupActivate4             = 2904
cmd_GroupActivate5             = 2905
cmd_GroupActivate6             = 2906
#cmd_GroupActivateFloat1        = 2910
#cmd_GroupActivateFloat2        = 2911
#cmd_GroupActivateFloat3        = 2912
