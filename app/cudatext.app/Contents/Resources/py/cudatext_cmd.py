#prefix cCommand: std commands of editor control
#prefix cmd: high-level commands of CudaText
#don't use flags cCmdNNN

cCmdSelKeep   = 0x10000 #cmd continues selection (new caret pos makes bigger selection)
cCmdSelReset  = 0x20000 #before command reset selection
cCmdCaret     = 0x80000 #cmd moves caret and makes new undo-group

_base_KeyUp       = 100 | cCmdCaret
_base_KeyDown     = 101 | cCmdCaret
_base_KeyLeft     = 102 | cCmdCaret
_base_KeyRight    = 103 | cCmdCaret
_base_KeyHome     = 104 | cCmdCaret
_base_KeyEnd      = 105 | cCmdCaret
_base_KeyPageUp   = 106 | cCmdCaret
_base_KeyPageDown = 107 | cCmdCaret

cCommand_KeyUp           = _base_KeyUp | cCmdSelReset
cCommand_KeyDown         = _base_KeyDown | cCmdSelReset
cCommand_KeyLeft         = _base_KeyLeft #handles sel
cCommand_KeyRight        = _base_KeyRight #handles sel
cCommand_KeyHome         = _base_KeyHome | cCmdSelReset
cCommand_KeyEnd          = _base_KeyEnd | cCmdSelReset
cCommand_KeyPageUp       = _base_KeyPageUp | cCmdSelReset
cCommand_KeyPageDown     = _base_KeyPageDown | cCmdSelReset

cCommand_KeyUp_Sel       = _base_KeyUp | cCmdSelKeep
cCommand_KeyDown_Sel     = _base_KeyDown | cCmdSelKeep
cCommand_KeyLeft_Sel     = _base_KeyLeft | cCmdSelKeep
cCommand_KeyRight_Sel    = _base_KeyRight | cCmdSelKeep
cCommand_KeyHome_Sel     = _base_KeyHome | cCmdSelKeep
cCommand_KeyEnd_Sel      = _base_KeyEnd | cCmdSelKeep
cCommand_KeyPageUp_Sel   = _base_KeyPageUp | cCmdSelKeep
cCommand_KeyPageDown_Sel = _base_KeyPageDown | cCmdSelKeep

cCommand_ColSelectUp    = 110
cCommand_ColSelectDown  = 111
cCommand_ColSelectLeft  = 112
cCommand_ColSelectRight = 113

cCommand_TextInsert = 150
cCommand_TextInsertTabChar = 151
cCommand_KeyBackspace = 152
cCommand_KeyDelete = 153
cCommand_KeyEnter = 154
cCommand_KeyTab = 155

cCommand_TextDeleteSelection = 170
cCommand_TextDeleteLine = 171 | cCmdSelReset
cCommand_TextDuplicateLine = 172 | cCmdSelReset
cCommand_TextDeleteToLineBegin = 173 | cCmdSelReset
cCommand_TextDeleteToLineEnd = 174 | cCmdSelReset
cCommand_TextDeleteToTextEnd = 175 | cCmdSelReset
cCommand_TextDeleteWordNext = 176 | cCmdSelReset
cCommand_TextDeleteWordPrev = 177 | cCmdSelReset

_base_GotoTextBegin = 200 | cCmdCaret
_base_GotoTextEnd   = 201 | cCmdCaret
_base_GotoWordNext  = 202 | cCmdCaret
_base_GotoWordPrev  = 203 | cCmdCaret

cCommand_GotoTextBegin = _base_GotoTextBegin | cCmdSelReset
cCommand_GotoTextEnd = _base_GotoTextEnd | cCmdSelReset
cCommand_GotoWordNext = _base_GotoWordNext | cCmdSelReset
cCommand_GotoWordPrev = _base_GotoWordPrev | cCmdSelReset

cCommand_GotoTextBegin_Sel = _base_GotoTextBegin | cCmdSelKeep
cCommand_GotoTextEnd_Sel = _base_GotoTextEnd | cCmdSelKeep
cCommand_GotoWordNext_Sel = _base_GotoWordNext | cCmdSelKeep
cCommand_GotoWordPrev_Sel = _base_GotoWordPrev | cCmdSelKeep

cCommand_Undo = 235 | cCmdSelReset
cCommand_Redo = 236 | cCmdSelReset

cCommand_TextIndent = 240
cCommand_TextUnindent = 241

cCommand_ScrollLineUp = 250
cCommand_ScrollLineDown = 251
cCommand_ScrollToCaretTop = 252
cCommand_ScrollToCaretBottom = 253
cCommand_ScrollToCaretLeft = 254
cCommand_ScrollToCaretRight = 255

cCommand_SelectAll = 260 | cCmdSelReset | cCmdCaret
cCommand_SelectNone = 261 | cCmdSelReset | cCmdCaret
cCommand_SelectWords = 262 | cCmdSelReset | cCmdCaret
cCommand_SelectLines = 263 | cCmdSelReset | cCmdCaret
cCommand_SelectInverted = 264 | cCmdCaret
cCommand_SelectSplitToLines = 265 | cCmdCaret
cCommand_SelectExtendByLine = 266 | cCmdCaret

cCommand_MoveSelectionUp = 268 | cCmdCaret
cCommand_MoveSelectionDown = 269 | cCmdCaret
cCommand_TextInsertEmptyAbove = 270 | cCmdSelReset | cCmdCaret
cCommand_TextInsertEmptyBelow = 271 | cCmdSelReset | cCmdCaret

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

cCommand_ClipboardPaste = 1000
cCommand_ClipboardPaste_Select = 1001
cCommand_ClipboardPaste_KeepCaret = 1002
cCommand_ClipboardPaste_Column = 1003 | cCmdSelReset
cCommand_ClipboardPaste_ColumnKeepCaret = 1004 | cCmdSelReset
cCommand_ClipboardCopy = 1006
cCommand_ClipboardCopyAdd = 1007
cCommand_ClipboardCut = 1008

cCommand_TextCaseLower = 1020
cCommand_TextCaseUpper = 1021
cCommand_TextCaseTitle = 1022
cCommand_TextCaseInvert = 1023
cCommand_TextCaseSentence = 1024

cCommand_TextTrimSpacesLeft = 1026
cCommand_TextTrimSpacesRight = 1027
cCommand_TextTrimSpacesAll = 1028

cCommand_FoldAll = 1030
cCommand_UnfoldAll = 1031
cCommand_FoldLevel2 = 1032
cCommand_FoldLevel3 = 1033
cCommand_FoldLevel4 = 1034
cCommand_FoldLevel5 = 1035
cCommand_FoldLevel6 = 1036
cCommand_FoldLevel7 = 1037
cCommand_FoldLevel8 = 1038
cCommand_FoldLevel9 = 1039

cCommand_Cancel = 2001
cCommand_RepeatTextCommand = 2002
cCommand_ZoomIn = 2003
cCommand_ZoomOut = 2004

cCommand_CaretsExtendDownLine = 2010
cCommand_CaretsExtendDownPage = 2011
cCommand_CaretsExtendDownToEnd = 2012
cCommand_CaretsExtendUpLine = 2013
cCommand_CaretsExtendUpPage = 2014
cCommand_CaretsExtendUpToTop = 2015


cmd_FileNew            = 2500
cmd_FileOpen           = 2501
cmd_FileSave           = 2502
cmd_FileSaveAs         = 2503
cmd_FileSaveAll        = 2504
cmd_FileReopen         = 2505
cmd_FileExit           = 2506
cmd_FileClose          = 2510
cmd_FileCloseOtherThis = 2511
cmd_FileCloseOtherAll  = 2512
cmd_FileCloseAll       = 2513
cmd_FileCloseAndDelete = 2514
cmd_FileExportHtml     = 2515

cmd_OpsClearRecent     = 2520
cmd_OpsOpenDefault     = 2521
cmd_OpsOpenUser        = 2522
cmd_OpsOpenLexerOvr    = 2523
cmd_OpsOpenFileTypes   = 2524
cmd_OpsFontText        = 2525
cmd_OpsFontUi          = 2526
cmd_DialogSaveTabs     = 2527
cmd_ToggleFullScreen   = 2528
cmd_OpsReloadAndApply  = 2529
cmd_DialogLexerProp    = 2530
cmd_DialogLexerLib     = 2531
cmd_DialogColors       = 2532
cmd_ToggleSidePanel    = 2533
cmd_ToggleBottomPanel  = 2534
cmd_ShowPanelConsole   = 2535
cmd_ShowPanelOutput    = 2536
cmd_ShowPanelValidate  = 2537
cmd_ToggleFindDialog   = 2538
cmd_DialogLoadLexerStyles = 2540
cmd_ToggleToolbar      = 2541
cmd_ToggleStatusbar    = 2542
cmd_ResetPythonPlugins = 2543
cmd_DialogCharMap      = 2544
cmd_RunLastCommandPlugin = 2545

cmd_DialogGoto       = 2580
cmd_DialogGotoBookmark = 2581
cmd_DialogCommands   = 2582
cmd_DialogTabs       = 2583
cmd_DialogFind       = 2584
cmd_DialogReplace    = 2585

cmd_FindNext         = 2590
cmd_FindPrev         = 2591
cmd_FindCurWordNext  = 2592
cmd_FindCurWordPrev  = 2593
cmd_FindCurSelNext   = 2594
cmd_FindCurSelPrev   = 2595

cmd_SplitTabToggle   = 2620
cmd_SplitTabHorzVert = 2621
cmd_SplitTab3070     = 2622
cmd_SplitTab4060     = 2623
cmd_SplitTab5050     = 2624
cmd_SplitTab6040     = 2625
cmd_SplitTab7030     = 2626

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

cmd_CopyLine         = 2650
cmd_CopyFilenameFull = 2651
cmd_CopyFilenameDir  = 2652
cmd_CopyFilenameName = 2653
cmd_SortAsc          = 2654
cmd_SortDesc         = 2655
cmd_SortNocaseAsc    = 2656
cmd_SortNocaseDesc   = 2657

cmd_BookmarkToggle    = 2661
cmd_BookmarkInvertAll = 2662
cmd_BookmarkClearAll  = 2663
cmd_BookmarkGotoNext  = 2664
cmd_BookmarkGotoPrev  = 2665

cmd_CommentLineAdd_AtNonspace    = 2670
cmd_CommentLineAdd_AtStart       = 2671
cmd_CommentLineRemove            = 2672
cmd_CommentLineToggle_AtNonspace = 2673
cmd_CommentLineToggle_AtStart    = 2674
cmd_CommentStreamToggle          = 2675
cmd_DuplicateLineEx              = 2676
  
cmd_LineEndWin        = 2677
cmd_LineEndUnix       = 2678
cmd_LineEndMac        = 2679

cmd_FoldingFoldAtCurLine   = 2680
cmd_FoldingUnfoldAtCurLine = 2681

cmd_MenuEnc           = 2691
cmd_MenuEnds          = 2692
cmd_MenuLexers        = 2693

cmd_AutoComplete      = 2695
cmd_GotoDefinition    = 2696

cmd_HelpAbout     = 2700
cmd_HelpForum     = 2701
cmd_HelpWiki      = 2702
cmd_HelpMouse     = 2703
cmd_HelpChangelog = 2704
cmd_HelpLexers    = 2705

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
cmd_Encoding_cp437_NoReload     = 2730
cmd_Encoding_cp850_NoReload     = 2731
cmd_Encoding_cp852_NoReload     = 2732
cmd_Encoding_cp866_NoReload     = 2733
cmd_Encoding_cp874_NoReload     = 2734
cmd_Encoding_cp932_NoReload     = 2735
cmd_Encoding_cp936_NoReload     = 2736
cmd_Encoding_cp949_NoReload     = 2737
cmd_Encoding_cp950_NoReload     = 2738

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
cmd_Encoding_cp437_Reload     = 2770
cmd_Encoding_cp850_Reload     = 2771
cmd_Encoding_cp852_Reload     = 2772
cmd_Encoding_cp866_Reload     = 2773
cmd_Encoding_cp874_Reload     = 2774
cmd_Encoding_cp932_Reload     = 2775
cmd_Encoding_cp936_Reload     = 2776
cmd_Encoding_cp949_Reload     = 2777
cmd_Encoding_cp950_Reload     = 2778

cmd_Markers_DropAtCaret        = 2800
cmd_Markers_GotoLastNoDelete   = 2801
cmd_Markers_GotoLastAndDelete  = 2802
cmd_Markers_ClearAll           = 2803
cmd_Markers_SwapCaretAndMarker = 2804
