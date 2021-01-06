(*
This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at http://mozilla.org/MPL/2.0/.

Copyright (c) Alexey Torgashin
*)
{$ifdef nn}begin end;{$endif}

procedure TfmMain.TimerCmdTimer(Sender: TObject);
var
  Cmd: integer;
  Frame: TEditorFrame;
  Ed: TATSynEdit;
  EdGrp: TATGroups;
  NIndex: integer;
  Params: TAppVariantArray;
begin
 TimerCmd.Enabled:= false;

 repeat
  case AppCommandGet(Ed, Cmd) of
    acgNoCommands:
      Break;
    acgBadCommand:
      Continue;
  end;

  Frame:= GetEditorFrame(Ed);

  //support floating groups
  if Assigned(Frame) then
    EdGrp:= Frame.GetTabGroups
  else
    EdGrp:= Groups;

  case Cmd of

    cmd_FileClose:
      begin
        EdGrp.CloseTabs(tabCloseCurrent, false);
        CurrentFrame.SetFocus;
      end;

    cmd_FileCloseAll:
      begin
        EdGrp.CloseTabs(tabCloseAll, false);
        CurrentFrame.SetFocus;
      end;

    cmd_FileCloseOtherThis:
      begin
        EdGrp.CloseTabs(tabCloseOthersThisPage, false);
      end;

    cmd_FileCloseOtherAll:
      begin
        EdGrp.CloseTabs(tabCloseOthersAllPages, false);
      end;

    cmd_FileCloseAndDelete:
      begin
        DoFileCloseAndDelete(Ed);
      end;

    cmd_FileNew:
      DoFileNew;

    cmd_FileNewMenu:
      DoFileNewMenu(nil);

    cmd_FileOpen:
      DoFileOpenDialog;

    cmd_FileOpen_TextViewer:
      DoFileOpenDialog('/view-text /nozip /nopictures');

    cmd_FileOpen_HexViewer:
      DoFileOpenDialog('/view-hex /nozip /nopictures');

    cmd_FileOpen_UnicodeViewer:
      DoFileOpenDialog('/view-unicode /nozip /nopictures');

    cmd_FileOpenFolder:
      DoFolderAdd;

    cmd_FileOpen_NoPlugins:
      DoFileOpenDialog_NoPlugins;

    cmd_FileReopen:
      DoFileReopen(Ed);

    cmd_FileSave:
      DoFileSave(Ed);

    cmd_FileSaveAs:
      DoFileSaveAs(Ed);

    cmd_FileSaveAll:
      DoFileSaveAll;

    cmd_FileExit:
      Close;

    cmd_FileExportHtml:
      begin
        if Assigned(Frame) then
          DoFileExportHtml(Frame);
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
      DoToggleFullScreen;
    cmd_ToggleDistractionFree:
      DoToggleDistractionFree;
    cmd_ToggleSidePanel:
      DoToggleSidePanel;
    cmd_ToggleBottomPanel:
      DoToggleBottomPanel;
    cmd_ToggleFindDialog:
      DoToggleFindDialog;
    cmd_ToggleSidebar:
      DoToggleSidebar;
    cmd_ToggleToolbar:
      DoToggleToolbar;
    cmd_ToggleStatusbar:
      DoToggleStatusbar;
    cmd_ToggleUiTabs:
      DoToggleUiTabs;
    cmd_ToggleOnTop:
      DoToggleOnTop;
    cmd_ToggleFloatSide:
      DoToggleFloatSide;
    cmd_ToggleFloatBottom:
      DoToggleFloatBottom;

    cmd_OpsReloadAndApply:
      DoOps_LoadOptionsAndApplyAll;

    cmd_ShowPanelConsole:
      DoShowConsole(false);
    cmd_ShowPanelOutput:
      DoShowOutput(false);
    cmd_ShowPanelValidate:
      DoShowValidate(false);

    cmd_ShowPanelConsole_AndFocus:
      DoShowConsole(true);
    cmd_ShowPanelOutput_AndFocus:
      DoShowOutput(true);
    cmd_ShowPanelValidate_AndFocus:
      DoShowValidate(true);

    cmd_ShowSidePanelAsIs:
      AppPanels[cPaneSide].Visible:= true;

    cmd_ShowSidePanelAndSyntaxTree:
      AppPanels[cPaneSide].UpdatePanels(msgPanelTree_Init, false, true);

    cmd_ToggleSidePanelAndSyntaxTree:
      begin
        if AppPanels[cPaneSide].Visible then
          AppPanels[cPaneSide].Visible:= false
        else
          AppPanels[cPaneSide].UpdatePanels(msgPanelTree_Init, false, true);
      end;

    cmd_HideSidePanel:
      AppPanels[cPaneSide].Visible:= false;

    cmd_HideBottomPanel:
      AppPanels[cPaneOut].Visible:= false;

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
      ShowFinderResultSimple(EditorExpandSelectionToWord(Ed));

    cmd_OpsClearRecent:
      DoOps_ClearConfigHistory(acheRecentFiles);
    cmd_OpsClearSearchHistory:
      DoOps_ClearConfigHistory(acheSearch);
    cmd_OpsClearConsoleHistory:
      DoOps_ClearConfigHistory(acheConsole);

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
      DoPyRunLastPlugin;
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
        DoPyCommand_Cudaxlib(Ed, 'duplicate');
      end;

    cmd_AutoComplete:
      DoAutoComplete(Ed);

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

    cmd_ChooseTranslation:
      DoDialogMenuTranslations;
    cmd_ChooseThemeUI:
      DoDialogMenuThemes;

    cmd_MacroStart:
      begin
        if Assigned(Frame) then
        begin
          Frame.DoMacroStart;
          MsgStatus('');
        end;
      end;
    cmd_MacroStop:
      begin
        if Assigned(Frame) then
        begin
          Frame.DoMacroStop(false);
          DoOnMacro(Frame, Frame.MacroString);
          MsgStatus('');
        end;
      end;
    cmd_MacroCancel:
      begin
        if Assigned(Frame) then
        begin
          Frame.DoMacroStop(true);
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
      UpdateTree(true, false{ignore visible}, true{ForceUpdateAll});
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

    cmd_HelpAbout:
      DoHelpAbout;
    cmd_HelpForum:
      DoHelpForum;
    cmd_HelpWiki:
      DoHelpWiki;
    cmd_HelpIssues:
      DoHelpIssues;
    cmd_HelpHotkeys:
      DoHelpHotkeys;

    cmd_HelpCheckUpdates:
      begin
        SetLength(Params, 0);
        DoPyCommand('cuda_addonman', 'check_cudatext_updates', Params);
      end;

    cmdFirstLexerCommand..
    cmdLastLexerCommand:
      SetFrameLexerByIndex(Ed, Cmd-cmdFirstLexerCommand);

    cmdFirstPluginCommand..
    cmdLastPluginCommand:
      DoPyCommand_ByPluginIndex(Cmd-cmdFirstPluginCommand);

    cmdFirstFileCommand..
    cmdLastFileCommand:
      begin
        NIndex:= Cmd-cmdFirstFileCommand;
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
        NIndex:= Cmd-cmdFirstRecentCommand;
        if NIndex<AppListRecents.Count then
          DoFileOpen(AppListRecents[NIndex], '');
      end;
  end;
 until false;

 UpdateStatus;
end;

procedure TfmMain.FrameOnEditorCommand(Sender: TObject; ACommand: integer;
  const AText: string; var AHandled: boolean);
var
  Ed: TATSynEdit;
  Frame: TEditorFrame;
  Params: TAppVariantArray;
  Str: string;
begin
  AHandled:= true;
  if not Groups.Enabled then Exit;

  Ed:= Sender as TATSynEdit;
  Frame:= GetEditorFrame(Ed);

  if Assigned(Frame) and Frame.MacroRecord then
    if not AppPython.IsRunning then
      if IsCommandForMacros(ACommand) then
      begin
        Str:= IntToStr(ACommand);
        if ACommand=cCommand_TextInsert then
          Str:= Str+','+AText;
        Frame.MacroString:= Frame.MacroString+(Str+#10);
      end;

  if (ACommand=cCommand_TextInsert) and
    (AText<>'') then
  begin
    SetLength(Params, 1);
    Params[0]:= AppVariant(AText);
    if DoPyEvent(Ed, cEventOnInsert, Params).Val = evrFalse then
      begin AHandled:= true; exit end;
  end;

  //auto-close brackets
  if (ACommand=cCommand_TextInsert) and
    (Ed.OptAutoCloseBrackets<>'') and
    (Length(AText)=1) and
    (Pos(AText[1], Ed.OptAutoCloseBrackets)>0) then
  begin
    if EditorAutoCloseBracket(Ed, AText[1]) then
    begin
      AHandled:= true;
      Ed.DoEventCarets; //to highlight pair brackets after typing bracket
      exit
    end;
  end;

  //handle commands cmd_
  case ACommand of
    cCommand_ToggleWordWrap,
    cCommand_ToggleWordWrapAlt:
      begin
        UpdateStatus;
        AHandled:= false;
        exit
      end;

    cmd_RepaintEditor:
      begin
        Ed.Update;
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
        UpdateStatus;
        AHandled:= true;
        exit
      end;

    cmd_Markers_GotoLastAndDelete:
      begin
        Ed.MarkerGotoLast(true,
          UiOps.FindIndentHorz,
          UiOps.FindIndentVert);
        if Ed.Markers.Count=0 then
          Frame.TabKeyCollectMarkers:= false;
        UpdateStatus;
        AHandled:= true;
        exit
      end;

    cmd_Markers_GotoLastNoDelete:
      begin
        Ed.MarkerGotoLast(false,
          UiOps.FindIndentHorz,
          UiOps.FindIndentVert
        );
        UpdateStatus;
        AHandled:= true;
        exit
      end;

    cmd_Markers_ClearAll:
      begin
        Ed.MarkerClearAll;
        Frame.TabKeyCollectMarkers:= false;
        UpdateStatus;
        AHandled:= true;
        exit
      end;

    cmd_Markers_SwapCaretAndMarker:
      begin
        Ed.MarkerSwap;
        UpdateStatus;
        AHandled:= true;
        exit
      end;

    cmd_Markers_SelectToCaret:
      begin
        Ed.MarkerSelectToCaret;
        UpdateStatus;
        AHandled:= true;
        exit
      end;

    cmd_Markers_DeleteToCaret:
      begin
        Ed.MarkerDeleteToCaret;
        UpdateStatus;
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

    cmd_SplitTabToggle:
      begin
        Frame.Splitted:= not Frame.Splitted;
        AHandled:= true;
        exit
      end;
    cmd_SplitTabHorzVert:
      begin
        Frame.SplitHorz:= not Frame.SplitHorz;
        AHandled:= true;
        exit
      end;
    cmd_SplitTab3070:
      begin
        Frame.SplitPos:= 0.7;
        AHandled:= true;
        exit
      end;
    cmd_SplitTab4060:
      begin
        Frame.SplitPos:= 0.6;
        AHandled:= true;
        exit
      end;
    cmd_SplitTab5050:
      begin
        Frame.SplitPos:= 0.5;
        AHandled:= true;
        exit
      end;
    cmd_SplitTab6040:
      begin
        Frame.SplitPos:= 0.4;
        AHandled:= true;
        exit
      end;
    cmd_SplitTab7030:
      begin
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
    cmd_Encoding_iso15_NoReload:     begin SetFrameEncoding(Ed, cEncConvNames[eidISO15], false); AHandled:= true; exit end;
    cmd_Encoding_cp437_NoReload:     begin SetFrameEncoding(Ed, cEncConvNames[eidCP437], false); AHandled:= true; exit end;
    cmd_Encoding_cp850_NoReload:     begin SetFrameEncoding(Ed, cEncConvNames[eidCP850], false); AHandled:= true; exit end;
    cmd_Encoding_cp852_NoReload:     begin SetFrameEncoding(Ed, cEncConvNames[eidCP852], false); AHandled:= true; exit end;
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
    cmd_Encoding_iso15_Reload:       begin SetFrameEncoding(Ed, cEncConvNames[eidISO15], true); AHandled:= true; exit end;
    cmd_Encoding_cp437_Reload:       begin SetFrameEncoding(Ed, cEncConvNames[eidCP437], true); AHandled:= true; exit end;
    cmd_Encoding_cp850_Reload:       begin SetFrameEncoding(Ed, cEncConvNames[eidCP850], true); AHandled:= true; exit end;
    cmd_Encoding_cp852_Reload:       begin SetFrameEncoding(Ed, cEncConvNames[eidCP852], true); AHandled:= true; exit end;
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
    AppCommandPut(Ed, ACommand, false);
    exit;
  end;

  case ACommand of
    cCommand_KeyTab:
      begin
        AHandled:= Frame.TabKeyCollectMarkers and (Ed.Markers.Count>0);
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


