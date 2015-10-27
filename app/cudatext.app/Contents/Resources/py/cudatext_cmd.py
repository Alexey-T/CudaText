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
cmd_DlgSaveTabs        = 2527
cmd_ToggleFullScreen   = 2528
cmd_OpsReloadAndApply  = 2529
cmd_DlgLexerProp       = 2530
cmd_DlgLexerLib        = 2531
cmd_DlgColors          = 2532
cmd_ToggleSidePanel    = 2533
cmd_ToggleBottomPanel  = 2534
cmd_ShowPanelConsole   = 2535
cmd_ShowPanelOutput    = 2536
cmd_ShowPanelValidate  = 2537
cmd_ToggleFindDialog   = 2538

cmd_DlgGoto       = 2580
cmd_DlgGotoBm     = 2581
cmd_DlgCommands   = 2582
cmd_DlgTabs       = 2583
cmd_DlgFind       = 2584
cmd_DlgReplace    = 2585

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
  
cmd_LineEndWin        = 2677
cmd_LineEndUnix       = 2678
cmd_LineEndMac        = 2679

cmd_EncAnsi           = 2680
cmd_EncUtf8bom        = 2681
cmd_EncUtf8nobom      = 2682
cmd_EncUtf16le        = 2683
cmd_EncUtf16be        = 2684

cmd_MenuEnc           = 2691
cmd_MenuEnds          = 2692
cmd_MenuLexers        = 2693

cmd_AutoComplete      = 2695
cmd_GotoDefinition    = 2696
