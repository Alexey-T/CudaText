(*
This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at http://mozilla.org/MPL/2.0/.

Copyright (c) Alexey Torgashin
*)
{$ifdef nn}begin end;{$endif}

procedure TfmMain.TimerCmdTimer(Sender: TObject);
var
  Ed: TATSynEdit;
  CmdCode: integer;
  CmdInvoke: TATEditorCommandInvoke;
begin
  TimerCmd.Enabled:= false;

  repeat
    case AppCommandGet(Ed, CmdCode, CmdInvoke) of
      acgNoCommands:
        Break;
      acgBadCommand:
        begin
          Sleep(15);
          Continue;
        end;
    end;

    HandleTimerCommand(Ed, CmdCode, CmdInvoke);
  until false;

  UpdateStatusbar;
end;


procedure TfmMain.HandleTimerCommand(Ed: TATSynEdit; CmdCode: integer; CmdInvoke: TATEditorCommandInvoke);
var
  Frame: TEditorFrame;
  EdGrp: TATGroups;
  NIndex: integer;
  NColor: TColor;
begin
  Frame:= TGroupsHelper.GetEditorFrame(Ed);

  //support floating groups
  if Assigned(Frame) then
    EdGrp:= Frame.GetTabGroups
  else
    EdGrp:= Groups;

  case CmdCode of

    cmd_FileClose:
      begin
        EdGrp.CloseTabs(tabCloseCurrent, false, true);

        Frame:= CurrentFrame;
        if Assigned(Frame) then
          Frame.SetFocus
        else
        if FrameCount>0 then
          TGroupsHelper.ForceFrameVisible(Frames[0])
        else
          raise Exception.Create('No frames left after "Close tab"');
      end;

    cmd_FileCloseAll:
      begin
        DoCloseAllTabs(false);

        Frame:= CurrentFrame;
        if Assigned(Frame) then
          Frame.SetFocus
        else
        if FrameCount>0 then
          TGroupsHelper.ForceFrameVisible(Frames[0])
        else
          raise Exception.Create('No frames left after "Close all tabs"');
      end;

    cmd_FileCloseOtherThis:
      begin
        AppClosingTabs:= true;
        EdGrp.CloseTabs(tabCloseOthersThisPage, false, false);
        AppClosingTabs:= false;
      end;

    cmd_FileCloseOtherAll:
      begin
        AppClosingTabs:= true;
        EdGrp.CloseTabs(tabCloseOthersAllPages, false, false);
        AppClosingTabs:= false;
      end;

    cmd_FileCloseAndDelete:
      begin
        DoFileCloseAndDelete(Ed);
      end;

    cmd_FileNew:
      DoFileNew;

    cmd_FileNewMenu:
      DoFileNewMenu(nil, CmdInvoke);

    cmd_FileOpen:
      DoFileOpenDialog;

    cmd_FileOpen_TextViewer:
      DoFileOpenDialog('/view-text /nozip /nopictures');

    cmd_FileOpen_HexViewer:
      DoFileOpenDialog('/view-hex /nozip /nopictures');

    cmd_FileOpen_UnicodeViewer:
      DoFileOpenDialog('/view-unicode /nozip /nopictures');

    cmd_FileOpenFolder:
      DoFolderAdd(CmdInvoke);

    cmd_FileOpen_NoPlugins:
      DoFileOpenDialog_NoPlugins;

    cmd_FileReopen:
      DoFileReopen(Frame, Ed);

    cmd_FileSave:
      DoFileSave(Frame, Ed);

    cmd_FileSaveAs:
      DoFileSaveAs(Frame, Ed);

    cmd_FileSaveAll:
      DoFileSaveAll;

    cmd_FileExit:
      Close;

    cmd_FileExportHtml:
      begin
        if Assigned(Frame) then
          DoFileExportHtml(Frame);
      end;

    cmd_FileReopenRecent:
      DoFileReopenRecent;

    cmd_OpenContainingFolder:
      begin
        if Ed.FileName<>'' then
          AppBrowseToFilenameInShell(Ed.FileName);
      end;

    cmd_OpenFileInDefaultApp:
      begin
        if Ed.FileName<>'' then
          OpenDocument(Ed.FileName);
      end;

    cmd_ToggleFocusSplitEditors:
      begin
        if Assigned(Frame) then
          Frame.DoToggleFocusSplitEditors;
      end;

    cmd_FocusEditor:
      DoFocusEditor(CurrentEditor);

    cmd_FocusNotificationPanel:
      begin
        if Assigned(Frame) then
          Frame.DoFocusNotificationPanel;
      end;

    cmd_HideNotificationPanels:
      begin
        if Assigned(Frame) then
          Frame.DoHideNotificationPanels;
      end;

    cmd_ToggleFileNotifications:
      begin
        UiOps.NotificationEnabled:= not UiOps.NotificationEnabled;
        if UiOps.NotificationEnabled then
          if AppNotifThread=nil then
          begin
            AppNotifThread:= TAppNotifThread.Create(false);
            AppNotifThread.Priority:= tpLower;
          end;
      end;

    cmd_SwitchTab_HotkeyNext:
      DoSwitchTab(true);
    cmd_SwitchTab_HotkeyPrev:
      DoSwitchTab(false);
    cmd_SwitchTab_SimpleNext:
      DoSwitchTabSimply(true);
    cmd_SwitchTab_SimplePrev:
      DoSwitchTabSimply(false);
    cmd_SwitchTab_Dialog:
      DoDialogMenuTabSwitcher;
    cmd_SwitchTab_Recent:
      DoSwitchTabToRecent;

    cmd_DialogCommands:
      DoDialogCommands;

    cmd_DialogGoto:
      DoDialogGoto;

    cmd_DialogGotoBookmark:
      DoDialogGotoBookmark;

    cmd_DialogFind:
      DoDialogFind(false);

    cmd_DialogFind_Hide:
      DoDialogFind_Hide;

    cmd_DialogReplace:
      DoDialogFind(true);

    cmd_DialogSaveTabs:
      DoDialogSaveTabs;

    cmd_DialogLexerProp:
      begin
        if Assigned(Frame) then
          DoDialogLexerProp(Frame.Lexer[Ed]);
      end;

    cmd_DialogLexerLib:
      DoDialogLexerLib;

    cmd_DialogLexerStyleMap:
      DoDialogLexerMap;

    cmd_DialogThemeUi:
      DoDialogTheme(true);

    cmd_DialogThemeSyntax:
      DoDialogTheme(false);

    cmd_DialogLexerMenu:
      DoDialogLexerMenu;

    cmd_DialogCharMap:
      DoDialogCharMap;

    cmd_ShowMainMenuAsPopup:
      DoSidebar_MainMenuClick(nil);

    cmd_ToggleFullScreen:
      begin
        DoToggleFullScreen;
        FNeedUpdateMenuChecks:= true;
      end;
    cmd_ToggleDistractionFree:
      begin
        DoToggleDistractionFree;
        FNeedUpdateMenuChecks:= true;
      end;

    cmd_ToggleSidePanel:
      begin
        DoToggleSidePanel;
        FNeedUpdateMenuChecks:= true;
      end;
    cmd_ToggleBottomPanel:
      begin
        DoToggleBottomPanel;
        FNeedUpdateMenuChecks:= true;
      end;

    cmd_ToggleFindDialog:
      DoDialogFind_Toggle(false, false);
    cmd_ToggleFindDialog_AndFocus:
      DoDialogFind_Toggle(false, true);
    cmd_ToggleReplaceDialog:
      DoDialogFind_Toggle(true, false);

    cmd_ToggleSidebar:
      begin
        DoToggleSidebar;
        FNeedUpdateMenuChecks:= true;
      end;

    cmd_ToggleToolbar:
      begin
        DoToggleToolbar;
        FNeedUpdateMenuChecks:= true;
      end;

    cmd_ToggleStatusbar:
      begin
        DoToggleStatusbar;
        FNeedUpdateMenuChecks:= true;
      end;

    cmd_ToggleUiTabs:
      DoToggleUiTabs;

    cmd_ToggleOnTop:
      begin
        DoToggleOnTop;
        FNeedUpdateMenuChecks:= true;
      end;

    cmd_ToggleFloatSide:
    begin
      DoToggleFloatSide;
      FNeedUpdateMenuChecks:= true;
    end;

    cmd_ToggleFloatBottom:
    begin
      DoToggleFloatBottom;
      FNeedUpdateMenuChecks:= true;
    end;

    cmd_OpsReloadAndApply:
      DoOps_LoadOptionsAndApplyAll;

    cmd_ShowPanelConsole:
      begin
        DoShowConsole(false);
        FNeedUpdateMenuChecks:= true;
      end;
    cmd_ShowPanelOutput:
      begin
        DoShowOutput(false);
        FNeedUpdateMenuChecks:= true;
      end;
    cmd_ShowPanelValidate:
      begin
        DoShowValidate(false);
        FNeedUpdateMenuChecks:= true;
      end;

    cmd_ShowPanelConsole_AndFocus:
      begin
        DoShowConsole(true);
        FNeedUpdateMenuChecks:= true;
      end;
    cmd_ShowPanelOutput_AndFocus:
      begin
        DoShowOutput(true);
        FNeedUpdateMenuChecks:= true;
      end;
    cmd_ShowPanelValidate_AndFocus:
      begin
        DoShowValidate(true);
        FNeedUpdateMenuChecks:= true;
      end;

    cmd_ShowSidePanelAsIs:
      begin
        AppPanels[cPaneSide].Visible:= true;
        FNeedUpdateMenuChecks:= true;
      end;

    cmd_ShowSidePanelAndSyntaxTree:
      begin
        AppPanels[cPaneSide].UpdatePanels(msgPanelTree_Init, false, true);
        FNeedUpdateMenuChecks:= true;
      end;

    cmd_ToggleSidePanelAndSyntaxTree:
      begin
        if AppPanels[cPaneSide].Visible then
          AppPanels[cPaneSide].Visible:= false
        else
          AppPanels[cPaneSide].UpdatePanels(msgPanelTree_Init, false, true);
        FNeedUpdateMenuChecks:= true;
      end;

    cmd_HideSidePanel:
      begin
        AppPanels[cPaneSide].Visible:= false;
        FNeedUpdateMenuChecks:= true;
      end;

    cmd_HideBottomPanel:
      begin
        AppPanels[cPaneOut].Visible:= false;
        FNeedUpdateMenuChecks:= true;
      end;

    cmd_FindFirst:
      DoFindFirst;
    cmd_FindNext:
      DoFindNext(true);
    cmd_FindPrev:
      DoFindNext(false);

    cmd_FindAllAndSelect:
      DoFindMarkAll(markingSelections);
    cmd_FindAllAndMarkers:
      DoFindMarkAll(markingMarkers);
    cmd_FindAllAndBookmarks:
      DoFindMarkAll(markingBookmarks);

    cmd_FindCurWordNext:
      DoFindCurrentWordOrSel(Ed, true, true);
    cmd_FindCurWordPrev:
      DoFindCurrentWordOrSel(Ed, false, true);

    cmd_FindCurSelNext:
      DoFindCurrentWordOrSel(Ed, true, false);
    cmd_FindCurSelPrev:
      DoFindCurrentWordOrSel(Ed, false, false);

    cmd_SelectExpandToWord:
      EditorExpandSelectionToWord(Ed, @FinderShowResultSimple, true, true);
    cmd_SelectExpandToText:
      EditorExpandSelectionToWord(Ed, @FinderShowResultSimple, true, false);

    cmd_SelectExpandToWord_Skip:
      EditorExpandSelectionToWord(Ed, @FinderShowResultSimple, false, true);
    cmd_SelectExpandToText_Skip:
      EditorExpandSelectionToWord(Ed, @FinderShowResultSimple, false, false);

    cmd_OpsClearRecent:
      DoOps_ClearConfigHistory([acheRecentFiles]);
    cmd_OpsClearSearchHistory:
      DoOps_ClearConfigHistory([acheSearch]);
    cmd_OpsClearConsoleHistory:
      DoOps_ClearConfigHistory([acheConsole]);

    cmd_OpsOpenDefault:
      DoOps_OpenFile_Default;
    cmd_OpsOpenUser:
      DoOps_OpenFile_User;
    cmd_OpsOpenDefaultAndUser:
      DoOps_OpenFile_DefaultAndUser;
    cmd_OpsOpenLexerSpecific:
      DoOps_OpenFile_LexerSpecific;

    cmd_OpsFontText:
      DoOps_DialogFont_Text;
    cmd_OpsFontUi:
      DoOps_DialogFont_Ui;
    cmd_OpsFontOutput:
      DoOps_DialogFont_Output;

    cmd_OpsFontSizeBigger:
      DoOps_FontSizeChange(1);
    cmd_OpsFontSizeSmaller:
      DoOps_FontSizeChange(-1);
    cmd_OpsFontSizeReset:
      DoOps_FontSizeReset;

    cmd_FindPythonLib:
      DoOps_FindPythonLib(nil);

    cmd_ResetPythonPlugins:
      DoPyResetPlugins;

    cmd_RunLastCommandPlugin:
      DoPyRunLastPlugin(CmdInvoke);

    cmd_RescanPythonPluginsInfFiles:
      DoPyRescanPlugins;

    cmd_Groups1:
      UpdateGroupsMode(gmOne);
    cmd_Groups2horz:
      UpdateGroupsMode(gm2h);
    cmd_Groups2vert:
      UpdateGroupsMode(gm2v);
    cmd_Groups3horz:
      UpdateGroupsMode(gm3h);
    cmd_Groups3vert:
      UpdateGroupsMode(gm3v);
    cmd_Groups3plus:
      UpdateGroupsMode(gm1plus2v);
    cmd_Groups3plushorz:
      UpdateGroupsMode(gm1plus2h);
    cmd_Groups4horz:
      UpdateGroupsMode(gm4h);
    cmd_Groups4vert:
      UpdateGroupsMode(gm4v);
    cmd_Groups4grid:
      UpdateGroupsMode(gm4grid);
    cmd_Groups6horz:
      UpdateGroupsMode(gm6h);
    cmd_Groups6vert:
      UpdateGroupsMode(gm6v);
    cmd_Groups6grid:
      UpdateGroupsMode(gm6grid);

    cmd_GroupActivateNext:
      Groups.PagesSetNext(true);
    cmd_GroupActivatePrev:
      Groups.PagesSetNext(false);

    cmd_GroupActivate1:
      Groups.PagesSetIndex(0);
    cmd_GroupActivate2:
      Groups.PagesSetIndex(1);
    cmd_GroupActivate3:
      Groups.PagesSetIndex(2);
    cmd_GroupActivate4:
      Groups.PagesSetIndex(3);
    cmd_GroupActivate5:
      Groups.PagesSetIndex(4);
    cmd_GroupActivate6:
      Groups.PagesSetIndex(5);

    cmd_GroupActivateFloat1:
      DoFormFocus(FFormFloatGroups1, true);
    cmd_GroupActivateFloat2:
      DoFormFocus(FFormFloatGroups2, true);
    cmd_GroupActivateFloat3:
      DoFormFocus(FFormFloatGroups3, true);

    cmd_MoveTabToGroupNext:
      Groups.MoveCurrentTabToNext(true);
    cmd_MoveTabToGroupPrev:
      Groups.MoveCurrentTabToNext(false);

    cmd_MoveTabToGroup1:
      DoMoveTabToGroup(0, true);
    cmd_MoveTabToGroup2:
      DoMoveTabToGroup(1, true);

    cmd_MoveTabToGroupFloating1:
      DoMoveTabToGroup(6, true);
    cmd_MoveTabToGroupFloating2:
      DoMoveTabToGroup(7, true);
    cmd_MoveTabToGroupFloating3:
      DoMoveTabToGroup(8, true);

    cmd_CopyLine:
      EditorCopyLine(Ed);

    cmd_CopyFilenameFull:
      begin
        if Assigned(Frame) then
          SClipboardCopy(Frame.GetFileName(Ed));
      end;
    cmd_CopyFilenameDir:
      begin
        if Assigned(Frame) then
          SClipboardCopy(ExtractFileDir(Frame.GetFileName(Ed)));
      end;
    cmd_CopyFilenameName:
      begin
        if Assigned(Frame) then
          SClipboardCopy(ExtractFileName(Frame.GetFileName(Ed)));
      end;

    cmd_DuplicateLineEx:
      begin
        DoPyCommand_Cudaxlib(Ed, 'duplicate', CmdInvoke);
      end;

    cmd_AutoComplete:
      begin
        AppAutocompleteInvoke:= 'c';
        DoAutoComplete(Ed);
      end;

    cmd_GotoDefinition:
      DoGotoDefinition(Ed);

    cmd_ShowFunctionHint:
      DoShowFuncHint(Ed);

    cmd_MenuEnc:
      begin
        InitPopupEnc;
        MenuShowAtEditorCorner(PopupEnc, CurrentEditor);
      end;
    cmd_MenuEnds:
      begin
        InitPopupEnds;
        MenuShowAtEditorCorner(PopupEnds, CurrentEditor);
      end;
    cmd_MenuLexers:
      begin
        InitPopupLex;
        MenuShowAtEditorCorner(PopupLex, CurrentEditor);
      end;

    cmd_SetTabColor:
      begin
        if Assigned(Frame) then
        begin
          NColor:= PyHelper_DialogColorPicker(Frame.TabColor);
          if NColor>=0 then
            Frame.TabColor:= NColor;
        end;
      end;
    cmd_ResetTabColor:
      begin
        if Assigned(Frame) then
          Frame.TabColor:= clNone;
      end;

    cmd_ToggleTabPinned:
      begin
        if Assigned(Frame) then
          Frame.TabPinned:= not Frame.TabPinned;
      end;

    cmd_ChooseTranslation:
      DoDialogMenuTranslations;
    cmd_ChooseThemeUI:
      DoDialogMenuThemes;

    cmd_MacroStart:
      begin
        if Assigned(Frame) then
        begin
          Frame.DoMacroStartOrStop;
          MsgStatus('');
        end;
      end;

    cmd_TreeGotoNext:
      DoTreeviewJump(CodeTree.Tree, treeGoNext);
    cmd_TreeGotoPrev:
      DoTreeviewJump(CodeTree.Tree, treeGoPrev);
    cmd_TreeGotoParent:
      DoTreeviewJump(CodeTree.Tree, treeGoParent);
    cmd_TreeGotoNextBrother:
      DoTreeviewJump(CodeTree.Tree, treeGoNextBro);
    cmd_TreeGotoPrevBrother:
      DoTreeviewJump(CodeTree.Tree, treeGoPrevBro);
    cmd_TreeUpdate:
      UpdateTree(true, false{ignore visible});
    cmd_TreeSelectBlockForCurNode:
      DoCodetree_GotoBlockForCurrentNode(true);
    cmd_TreeGotoBlockForCurNode:
      DoCodetree_GotoBlockForCurrentNode(false);
    cmd_TreeFilterClear:
      CodeTreeFilter_ResetOnClick(nil);
    cmd_TreeFilterFocus:
      DoSidebar_FocusCodetreeFilter;
    cmd_TreeFocus:
      DoSidebar_FocusCodetree;

    cmd_BracketHighlightOn:
      begin
        if Assigned(Frame) then
        begin
          Frame.BracketHilite:= true;
          Frame.BracketHiliteUserChanged:= true;
        end;
      end;
    cmd_BracketHighlightOff:
      begin
        if Assigned(Frame) then
        begin
          Frame.BracketHilite:= false;
          Frame.BracketHiliteUserChanged:= true;
        end;
      end;
    cmd_BracketHighlightToggle:
      begin
        if Assigned(Frame) then
        begin
          Frame.BracketHilite:= not Frame.BracketHilite;
          Frame.BracketHiliteUserChanged:= true;
        end;
      end;
    cmd_BracketJump:
      begin
        if Assigned(Frame) then
          Frame.BracketJump(Ed);
      end;
    cmd_BracketSelect:
      begin
        if Assigned(Frame) then
          Frame.BracketSelect(Ed);
      end;
    cmd_BracketSelectInside:
      begin
        if Assigned(Frame) then
          Frame.BracketSelectInside(Ed);
      end;

    cmdFirstLexerCommand..
    cmdLastLexerCommand:
      SetFrameLexerByIndex(Ed, CmdCode-cmdFirstLexerCommand);

    cmdFirstPluginCommand..
    cmdLastPluginCommand:
      DoPyCommand_ByPluginIndex(CmdCode-cmdFirstPluginCommand, CmdInvoke);

    cmdFirstFileCommand..
    cmdLastFileCommand:
      begin
        NIndex:= CmdCode-cmdFirstFileCommand;
        if NIndex<AppFrameList1.Count then
        begin
          Frame:= TEditorFrame(AppFrameList1[NIndex]);
          SetFrame(Frame);
          Frame.SetFocus;
        end;
      end;

    cmdFirstRecentCommand..
    cmdLastRecentCommand:
      begin
        NIndex:= CmdCode-cmdFirstRecentCommand;
        if NIndex<AppListRecents.Count then
          DoFileOpen(AppListRecents[NIndex], '');
      end;

    cmd_HelpAbout:
      DoHelpAbout;
    cmd_HelpForum:
      DoHelpForum;
    cmd_HelpWiki:
      DoHelpWiki;
    cmd_HelpIssues:
      DoHelpIssues;

    cmd_HelpCheckUpdates:
      begin
        if UiOps.AllowProgramUpdates then
          DoPyCommand('cuda_addonman', 'check_cudatext_updates', [], CmdInvoke);
      end;
  end;
end;

procedure TfmMain.FrameOnEditorCommand(Sender: TObject; ACommand: integer;
  AInvoke: TATEditorCommandInvoke; const AText: string; var AHandled: boolean);
var
  Ed: TATSynEdit;
  Frame: TEditorFrame;
  Str: string;
begin
  AHandled:= true;
  if not Groups.Enabled then
  begin
    AHandled:= false; //if True: scrolling to 1st caret after 'select all' in Find dlg won't work
    Exit;
  end;

  Ed:= Sender as TATSynEdit;
  Frame:= TGroupsHelper.GetEditorFrame(Ed);

  if Assigned(Frame) and Frame.MacroRecord then
    if not AppPython.IsRunning then
      if IsCommandForMacros(ACommand) then
      begin
        Str:= IntToStr(ACommand);
        if ACommand=cCommand_TextInsert then
          Str:= Str+','+AText;
        Frame.MacroStrings.Add(Str);
      end;

  if (ACommand=cCommand_TextInsert) and
    (AText<>'') then
  begin
    if DoPyEvent(Ed, cEventOnInsert, [AppVariant(AText)]).Val = evrFalse then
    begin
      AHandled:= true;
      exit
    end;
  end;

  //handle commands cmd_
  case ACommand of
    cCommand_ToggleWordWrap,
    cCommand_ToggleWordWrapAlt:
      begin
        UpdateStatusbar;
        AHandled:= false;
        exit
      end;

    cCommand_Cancel,
    cCommand_CancelKeepLast,
    cCommand_CancelKeepSel,
    cCommand_CancelKeepLastAndSel:
      begin
        Ed.Markers.DeleteWithTag(UiOps.FindOccur_TagValue);
        AHandled:= false;
      end;

    cmd_RepaintEditor:
      begin
        //params 2nd/3rd are to fix repainting with cuda_sync_scroll with "renderer_anti_flicker":20;
        Ed.Update(false, true, false);
        AHandled:= true;
        exit
      end;

    cmd_TabUsesSpaces_On:
      begin
        Ed.OptTabSpaces:= true;
        AHandled:= true;
        exit
      end;
    cmd_TabUsesSpaces_Off:
      begin
        Ed.OptTabSpaces:= false;
        AHandled:= true;
        exit
      end;
    cmd_ToggleTabUsesSpaces:
      begin
        Ed.OptTabSpaces:= not Ed.OptTabSpaces;
        AHandled:= true;
        exit
      end;

    cmd_ConvertTabsToSpaces:
      begin
        Ed.DoConvertTabsToSpaces;
        AHandled:= true;
        exit
      end;
    cmd_ConvertTabsToSpacesLeading:
      begin
        Ed.DoConvertIndentation(false);
        AHandled:= true;
        exit
      end;
    cmd_ConvertSpacesToTabsLeading:
      begin
        Ed.DoConvertIndentation(true);
        AHandled:= true;
        exit
      end;

    cmd_BookmarkToggle:
      begin
        if ed.Carets.Count>0 then
          ed.BookmarkToggleForLine(ed.Carets[0].PosY, 1, '', bmadOption, true, 0);
        UpdateFrameEx(Frame, false);
        AHandled:= true;
        exit
      end;
    cmd_BookmarkClearAll:
      begin
        ed.BookmarkDeleteAll;
        UpdateFrameEx(Frame, false);
        AHandled:= true;
        exit
      end;
    cmd_BookmarkInvertAll:
      begin
        ed.BookmarkInvertAll;
        UpdateFrameEx(Frame, false);
        AHandled:= true;
        exit
      end;
    cmd_BookmarkGotoNext:
      begin
        ed.BookmarkGotoNext(true, UiOps.FindIndentHorz, UiOps.FindIndentVert, true);
        AHandled:= true;
        exit
      end;
    cmd_BookmarkGotoPrev:
      begin
        ed.BookmarkGotoNext(false, UiOps.FindIndentHorz, UiOps.FindIndentVert, true);
        AHandled:= true;
        exit
      end;
    cmd_BookmarkPlaceCarets:
      begin
        Ed.BookmarkPlaceCaretsOnBookmarks;
        AHandled:= true;
        exit
      end;
    cmd_BookmarkPlaceBookmarksOnCarets:
      begin
        Ed.BookmarkPlaceBookmarksOnCarets;
        AHandled:= true;
        exit
      end;
    cmd_BookmarkCopyMarkedLines:
      begin
        Ed.BookmarkCopyMarkedLines;
        AHandled:= true;
        exit
      end;
    cmd_BookmarkDeleteMarkedLines:
      begin
        Ed.BookmarkDeleteMarkedLines;
        AHandled:= true;
        exit
      end;

    cmd_Markers_DropAtCaret:
      begin
        Ed.MarkerDrop;
        UpdateStatusbar;
        AHandled:= true;
        exit
      end;

    cmd_Markers_GotoLastAndDelete:
      begin
        Ed.MarkerGotoLast(true,
          UiOps.FindIndentHorz,
          UiOps.FindIndentVert);
        if Assigned(Frame) then
         if Ed.Markers.Count=0 then
          Frame.TabKeyCollectMarkers:= false;
        UpdateStatusbar;
        AHandled:= true;
        exit
      end;

    cmd_Markers_GotoLastNoDelete:
      begin
        Ed.MarkerGotoLast(false,
          UiOps.FindIndentHorz,
          UiOps.FindIndentVert
        );
        UpdateStatusbar;
        AHandled:= true;
        exit
      end;

    cmd_Markers_ClearAll:
      begin
        Ed.MarkerClearAll;
        if Assigned(Frame) then
          Frame.TabKeyCollectMarkers:= false;
        UpdateStatusbar;
        AHandled:= true;
        exit
      end;

    cmd_Markers_SwapCaretAndMarker:
      begin
        Ed.MarkerSwap;
        UpdateStatusbar;
        AHandled:= true;
        exit
      end;

    cmd_Markers_SelectToCaret:
      begin
        Ed.MarkerSelectToCaret;
        UpdateStatusbar;
        AHandled:= true;
        exit
      end;

    cmd_Markers_DeleteToCaret:
      begin
        Ed.MarkerDeleteToCaret;
        UpdateStatusbar;
        AHandled:= true;
        exit
      end;

    cmd_FoldingEnable:
      begin
        if Assigned(Frame) then
          Frame.EnabledFolding:= true;
        AHandled:= true;
        exit
      end;
    cmd_FoldingDisable:
      begin
        if Assigned(Frame) then
          Frame.EnabledFolding:= false;
        AHandled:= true;
        exit
      end;

    cmd_DeleteNewColorAttrs:
      begin
        EditorDeleteNewColorAttribs(Ed);
        AHandled:= true;
        exit
      end;

    cmd_LineEndWin:
      begin
        UpdateFrameLineEnds(Frame, cEndWin);
        AHandled:= true;
        exit
      end;
    cmd_LineEndUnix:
      begin
        UpdateFrameLineEnds(Frame, cEndUnix);
        AHandled:= true;
        exit
      end;
    cmd_LineEndMac:
      begin
        UpdateFrameLineEnds(Frame, cEndMac);
        AHandled:= true;
        exit
      end;

    cmd_LineEndWin_Caret:
      begin
        UpdateEditorCaretLineEnds(Frame, Ed, cEndWin);
        AHandled:= true;
        exit
      end;
    cmd_LineEndUnix_Caret:
      begin
        UpdateEditorCaretLineEnds(Frame, Ed, cEndUnix);
        AHandled:= true;
        exit
      end;
    cmd_LineEndMac_Caret:
      begin
        UpdateEditorCaretLineEnds(Frame, Ed, cEndMac);
        AHandled:= true;
        exit
      end;
    cmd_LineEndDefault_Caret:
      begin
        UpdateEditorCaretLineEnds(Frame, Ed, Ed.Strings.Endings);
        AHandled:= true;
        exit
      end;

    cmd_SplitTabToggle:
      begin
        if Assigned(Frame) then
        begin
          Frame.Splitted:= not Frame.Splitted;
          UpdateMenuChecks_FrameSplit(Frame);
        end;
        AHandled:= true;
        exit
      end;
    cmd_SplitTabHorzVert:
      begin
        if Assigned(Frame) then
        begin
          Frame.SplitHorz:= not Frame.SplitHorz;
          UpdateMenuChecks_FrameSplit(Frame);
        end;
        AHandled:= true;
        exit
      end;

    cmd_SplitTabHorz:
      begin
        if Assigned(Frame) then
        begin
          Frame.SplitHorz:= true;
          Frame.Splitted:= true;
          UpdateMenuChecks_FrameSplit(Frame);
        end;
        AHandled:= true;
        exit
      end;
    cmd_SplitTabVert:
      begin
        if Assigned(Frame) then
        begin
          Frame.SplitHorz:= false;
          Frame.Splitted:= true;
          UpdateMenuChecks_FrameSplit(Frame);
        end;
        AHandled:= true;
        exit
      end;
    cmd_SplitTabNo:
      begin
        if Assigned(Frame) then
        begin
          Frame.Splitted:= false;
          UpdateMenuChecks_FrameSplit(Frame);
        end;
        AHandled:= true;
        exit
      end;

    cmd_SplitTab3070:
      begin
        if Assigned(Frame) then
          Frame.SplitPos:= 0.7;
        AHandled:= true;
        exit
      end;
    cmd_SplitTab4060:
      begin
        if Assigned(Frame) then
          Frame.SplitPos:= 0.6;
        AHandled:= true;
        exit
      end;
    cmd_SplitTab5050:
      begin
        if Assigned(Frame) then
          Frame.SplitPos:= 0.5;
        AHandled:= true;
        exit
      end;
    cmd_SplitTab6040:
      begin
        if Assigned(Frame) then
          Frame.SplitPos:= 0.4;
        AHandled:= true;
        exit
      end;
    cmd_SplitTab7030:
      begin
        if Assigned(Frame) then
          Frame.SplitPos:= 0.3;
        AHandled:= true;
        exit
      end;

    cmd_LinkAtCaret_Open:
      begin
        Str:= EditorGetLinkAtCaret(Ed);
        if Str<>'' then
        begin
          EditorOpenLink(Str);
          MsgStatus(msgStatusOpenedBrowser);
        end;
        AHandled:= true;
        exit
      end;

    cmd_LinkAtCaret_Copy:
      begin
        Str:= EditorGetLinkAtCaret(Ed);
        if Str<>'' then
        begin
          SClipboardCopy(UTF8Encode(Str));
          MsgStatus(msgStatusCopiedLink);
        end;
        AHandled:= true;
        exit
      end;

    cmd_LinkAtPopup_Open:
      begin
        Str:= EditorGetLinkAtScreenCoord(Ed, PopupText.PopupPoint);
        if Str<>'' then
        begin
          EditorOpenLink(Str);
          MsgStatus(msgStatusOpenedBrowser);
        end;
        AHandled:= true;
        exit
      end;

    cmd_LinkAtPopup_Copy:
      begin
        Str:= EditorGetLinkAtScreenCoord(Ed, PopupText.PopupPoint);
        if Str<>'' then
        begin
          SClipboardCopy(UTF8Encode(Str));
          MsgStatus(msgStatusCopiedLink);
        end;
        AHandled:= true;
        exit
      end;

    cmd_TabSize_Set2:
      begin
        Ed.OptTabSize:= 2;
        AHandled:= true;
        exit
      end;
    cmd_TabSize_Set4:
      begin
        Ed.OptTabSize:= 4;
        AHandled:= true;
        exit
      end;
    cmd_TabSize_Set8:
      begin
        Ed.OptTabSize:= 8;
        AHandled:= true;
        exit
      end;

    cmd_Encoding_utf8bom_NoReload:   begin SetFrameEncoding(Ed, cEncNameUtf8_WithBom, false); AHandled:= true; exit end;
    cmd_Encoding_utf8nobom_NoReload: begin SetFrameEncoding(Ed, cEncNameUtf8_NoBom, false); AHandled:= true; exit end;
    cmd_Encoding_utf16le_NoReload:   begin SetFrameEncoding(Ed, cEncNameUtf16LE_WithBom, false); AHandled:= true; exit end;
    cmd_Encoding_utf16be_NoReload:   begin SetFrameEncoding(Ed, cEncNameUtf16BE_WithBom, false); AHandled:= true; exit end;
    cmd_Encoding_utf32le_NoReload:   begin SetFrameEncoding(Ed, cEncNameUtf32LE_WithBom, false); AHandled:= true; exit end;
    cmd_Encoding_utf32be_NoReload:   begin SetFrameEncoding(Ed, cEncNameUtf32BE_WithBom, false); AHandled:= true; exit end;
    cmd_Encoding_cp1250_NoReload:    begin SetFrameEncoding(Ed, cEncConvNames[eidCP1250], false); AHandled:= true; exit end;
    cmd_Encoding_cp1251_NoReload:    begin SetFrameEncoding(Ed, cEncConvNames[eidCP1251], false); AHandled:= true; exit end;
    cmd_Encoding_cp1252_NoReload:    begin SetFrameEncoding(Ed, cEncConvNames[eidCP1252], false); AHandled:= true; exit end;
    cmd_Encoding_cp1253_NoReload:    begin SetFrameEncoding(Ed, cEncConvNames[eidCP1253], false); AHandled:= true; exit end;
    cmd_Encoding_cp1254_NoReload:    begin SetFrameEncoding(Ed, cEncConvNames[eidCP1254], false); AHandled:= true; exit end;
    cmd_Encoding_cp1255_NoReload:    begin SetFrameEncoding(Ed, cEncConvNames[eidCP1255], false); AHandled:= true; exit end;
    cmd_Encoding_cp1256_NoReload:    begin SetFrameEncoding(Ed, cEncConvNames[eidCP1256], false); AHandled:= true; exit end;
    cmd_Encoding_cp1257_NoReload:    begin SetFrameEncoding(Ed, cEncConvNames[eidCP1257], false); AHandled:= true; exit end;
    cmd_Encoding_cp1258_NoReload:    begin SetFrameEncoding(Ed, cEncConvNames[eidCP1258], false); AHandled:= true; exit end;
    cmd_Encoding_mac_NoReload:       begin SetFrameEncoding(Ed, cEncConvNames[eidCPMac], false); AHandled:= true; exit end;
    cmd_Encoding_iso1_NoReload:      begin SetFrameEncoding(Ed, cEncConvNames[eidISO1], false); AHandled:= true; exit end;
    cmd_Encoding_iso2_NoReload:      begin SetFrameEncoding(Ed, cEncConvNames[eidISO2], false); AHandled:= true; exit end;
    cmd_Encoding_iso3_NoReload:      begin SetFrameEncoding(Ed, cEncConvNames[eidISO3], false); AHandled:= true; exit end;
    cmd_Encoding_iso4_NoReload:      begin SetFrameEncoding(Ed, cEncConvNames[eidISO4], false); AHandled:= true; exit end;
    cmd_Encoding_iso5_NoReload:      begin SetFrameEncoding(Ed, cEncConvNames[eidISO5], false); AHandled:= true; exit end;
    cmd_Encoding_iso7_NoReload:      begin SetFrameEncoding(Ed, cEncConvNames[eidISO7], false); AHandled:= true; exit end;
    cmd_Encoding_iso9_NoReload:      begin SetFrameEncoding(Ed, cEncConvNames[eidISO9], false); AHandled:= true; exit end;
    cmd_Encoding_iso10_NoReload:     begin SetFrameEncoding(Ed, cEncConvNames[eidISO10], false); AHandled:= true; exit end;
    cmd_Encoding_iso13_NoReload:     begin SetFrameEncoding(Ed, cEncConvNames[eidISO13], false); AHandled:= true; exit end;
    cmd_Encoding_iso14_NoReload:     begin SetFrameEncoding(Ed, cEncConvNames[eidISO14], false); AHandled:= true; exit end;
    cmd_Encoding_iso15_NoReload:     begin SetFrameEncoding(Ed, cEncConvNames[eidISO15], false); AHandled:= true; exit end;
    cmd_Encoding_iso16_NoReload:     begin SetFrameEncoding(Ed, cEncConvNames[eidISO16], false); AHandled:= true; exit end;
    cmd_Encoding_cp437_NoReload:     begin SetFrameEncoding(Ed, cEncConvNames[eidCP437], false); AHandled:= true; exit end;
    cmd_Encoding_cp850_NoReload:     begin SetFrameEncoding(Ed, cEncConvNames[eidCP850], false); AHandled:= true; exit end;
    cmd_Encoding_cp852_NoReload:     begin SetFrameEncoding(Ed, cEncConvNames[eidCP852], false); AHandled:= true; exit end;
    cmd_Encoding_cp861_NoReload:     begin SetFrameEncoding(Ed, cEncConvNames[eidCP861], false); AHandled:= true; exit end;
    cmd_Encoding_cp865_NoReload:     begin SetFrameEncoding(Ed, cEncConvNames[eidCP865], false); AHandled:= true; exit end;
    cmd_Encoding_cp866_NoReload:     begin SetFrameEncoding(Ed, cEncConvNames[eidCP866], false); AHandled:= true; exit end;
    cmd_Encoding_cp874_NoReload:     begin SetFrameEncoding(Ed, cEncConvNames[eidCP874], false); AHandled:= true; exit end;
    cmd_Encoding_cp932_NoReload:     begin SetFrameEncoding(Ed, cEncConvNames[eidCP932], false); AHandled:= true; exit end;
    cmd_Encoding_cp936_NoReload:     begin SetFrameEncoding(Ed, cEncConvNames[eidCP936], false); AHandled:= true; exit end;
    cmd_Encoding_cp949_NoReload:     begin SetFrameEncoding(Ed, cEncConvNames[eidCP949], false); AHandled:= true; exit end;
    cmd_Encoding_cp950_NoReload:     begin SetFrameEncoding(Ed, cEncConvNames[eidCP950], false); AHandled:= true; exit end;

    cmd_Encoding_utf8bom_Reload:     begin SetFrameEncoding(Ed, cEncNameUtf8_WithBom, true); AHandled:= true; exit end;
    cmd_Encoding_utf8nobom_Reload:   begin SetFrameEncoding(Ed, cEncNameUtf8_NoBom, true); AHandled:= true; exit end;
    cmd_Encoding_utf16le_Reload:     begin SetFrameEncoding(Ed, cEncNameUtf16LE_WithBom, true); AHandled:= true; exit end;
    cmd_Encoding_utf16be_Reload:     begin SetFrameEncoding(Ed, cEncNameUtf16BE_WithBom, true); AHandled:= true; exit end;
    cmd_Encoding_utf32le_Reload:     begin SetFrameEncoding(Ed, cEncNameUtf32LE_WithBom, true); AHandled:= true; exit end;
    cmd_Encoding_utf32be_Reload:     begin SetFrameEncoding(Ed, cEncNameUtf32BE_WithBom, true); AHandled:= true; exit end;
    cmd_Encoding_cp1250_Reload:      begin SetFrameEncoding(Ed, cEncConvNames[eidCP1250], true); AHandled:= true; exit end;
    cmd_Encoding_cp1251_Reload:      begin SetFrameEncoding(Ed, cEncConvNames[eidCP1251], true); AHandled:= true; exit end;
    cmd_Encoding_cp1252_Reload:      begin SetFrameEncoding(Ed, cEncConvNames[eidCP1252], true); AHandled:= true; exit end;
    cmd_Encoding_cp1253_Reload:      begin SetFrameEncoding(Ed, cEncConvNames[eidCP1253], true); AHandled:= true; exit end;
    cmd_Encoding_cp1254_Reload:      begin SetFrameEncoding(Ed, cEncConvNames[eidCP1254], true); AHandled:= true; exit end;
    cmd_Encoding_cp1255_Reload:      begin SetFrameEncoding(Ed, cEncConvNames[eidCP1255], true); AHandled:= true; exit end;
    cmd_Encoding_cp1256_Reload:      begin SetFrameEncoding(Ed, cEncConvNames[eidCP1256], true); AHandled:= true; exit end;
    cmd_Encoding_cp1257_Reload:      begin SetFrameEncoding(Ed, cEncConvNames[eidCP1257], true); AHandled:= true; exit end;
    cmd_Encoding_cp1258_Reload:      begin SetFrameEncoding(Ed, cEncConvNames[eidCP1258], true); AHandled:= true; exit end;
    cmd_Encoding_mac_Reload:         begin SetFrameEncoding(Ed, cEncConvNames[eidCPMac], true); AHandled:= true; exit end;
    cmd_Encoding_iso1_Reload:        begin SetFrameEncoding(Ed, cEncConvNames[eidISO1], true); AHandled:= true; exit end;
    cmd_Encoding_iso2_Reload:        begin SetFrameEncoding(Ed, cEncConvNames[eidISO2], true); AHandled:= true; exit end;
    cmd_Encoding_iso3_Reload:        begin SetFrameEncoding(Ed, cEncConvNames[eidISO3], true); AHandled:= true; exit end;
    cmd_Encoding_iso4_Reload:        begin SetFrameEncoding(Ed, cEncConvNames[eidISO4], true); AHandled:= true; exit end;
    cmd_Encoding_iso5_Reload:        begin SetFrameEncoding(Ed, cEncConvNames[eidISO5], true); AHandled:= true; exit end;
    cmd_Encoding_iso7_Reload:        begin SetFrameEncoding(Ed, cEncConvNames[eidISO7], true); AHandled:= true; exit end;
    cmd_Encoding_iso9_Reload:        begin SetFrameEncoding(Ed, cEncConvNames[eidISO9], true); AHandled:= true; exit end;
    cmd_Encoding_iso10_Reload:       begin SetFrameEncoding(Ed, cEncConvNames[eidISO10], true); AHandled:= true; exit end;
    cmd_Encoding_iso13_Reload:       begin SetFrameEncoding(Ed, cEncConvNames[eidISO13], true); AHandled:= true; exit end;
    cmd_Encoding_iso14_Reload:       begin SetFrameEncoding(Ed, cEncConvNames[eidISO14], true); AHandled:= true; exit end;
    cmd_Encoding_iso15_Reload:       begin SetFrameEncoding(Ed, cEncConvNames[eidISO15], true); AHandled:= true; exit end;
    cmd_Encoding_iso16_Reload:       begin SetFrameEncoding(Ed, cEncConvNames[eidISO16], true); AHandled:= true; exit end;
    cmd_Encoding_cp437_Reload:       begin SetFrameEncoding(Ed, cEncConvNames[eidCP437], true); AHandled:= true; exit end;
    cmd_Encoding_cp850_Reload:       begin SetFrameEncoding(Ed, cEncConvNames[eidCP850], true); AHandled:= true; exit end;
    cmd_Encoding_cp852_Reload:       begin SetFrameEncoding(Ed, cEncConvNames[eidCP852], true); AHandled:= true; exit end;
    cmd_Encoding_cp861_Reload:       begin SetFrameEncoding(Ed, cEncConvNames[eidCP861], true); AHandled:= true; exit end;
    cmd_Encoding_cp865_Reload:       begin SetFrameEncoding(Ed, cEncConvNames[eidCP865], true); AHandled:= true; exit end;
    cmd_Encoding_cp866_Reload:       begin SetFrameEncoding(Ed, cEncConvNames[eidCP866], true); AHandled:= true; exit end;
    cmd_Encoding_cp874_Reload:       begin SetFrameEncoding(Ed, cEncConvNames[eidCP874], true); AHandled:= true; exit end;
    cmd_Encoding_cp932_Reload:       begin SetFrameEncoding(Ed, cEncConvNames[eidCP932], true); AHandled:= true; exit end;
    cmd_Encoding_cp936_Reload:       begin SetFrameEncoding(Ed, cEncConvNames[eidCP936], true); AHandled:= true; exit end;
    cmd_Encoding_cp949_Reload:       begin SetFrameEncoding(Ed, cEncConvNames[eidCP949], true); AHandled:= true; exit end;
    cmd_Encoding_cp950_Reload:       begin SetFrameEncoding(Ed, cEncConvNames[eidCP950], true); AHandled:= true; exit end;
  end;

  //handle other cmd_ commands
  if (ACommand>=cmdFirstAppCommand) and
     (ACommand<=cmdLastAppCommand) then
  begin
    if ACommand=cmd_DialogCommands then
      UiOps.CmdPaletteFilterText_Forced:= AText;
    AppCommandPut(Ed, ACommand, AInvoke, false);
    exit;
  end;

  case ACommand of
    cCommand_KeyTab:
      begin
        if Assigned(Frame) then
          AHandled:= Frame.TabKeyCollectMarkers and (Ed.Markers.Count>0)
        else
          AHandled:= false;
        if AHandled then
        begin
          Ed.MarkerGotoLast(true,
            UiOps.FindIndentHorz,
            UiOps.FindIndentVert);
          if Ed.Markers.Count=0 then
            Frame.TabKeyCollectMarkers:= false;
        end;
      end;

    cmd_MouseClickAtCursor:
      EditorMouseClick_AtCursor(Ed, false);
    cmd_MouseClickAtCursorAndSelect:
      EditorMouseClick_AtCursor(Ed, true);
    cmd_MouseClickNearCaret:
      EditorMouseClick_NearCaret(Ed, AText, false);
    cmd_MouseClickNearCaretAndSelect:
      EditorMouseClick_NearCaret(Ed, AText, true);
    cmd_FinderAction:
      DoFindActionFromString(AText);
    cmd_GotoLastEditingPos:
      EditorGotoLastEditingPos(Ed, UiOps.FindIndentHorz, UiOps.FindIndentVert);

    else
      AHandled:= false;
  end;
end;


