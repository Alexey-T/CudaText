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
  P: TPoint;
  Str: atString;
begin
  TimerCmd.Enabled:= false;
  Cmd:= TimerCmd.Tag;
  case Cmd of

    cmd_FileClose:
      begin
        Groups.CloseTabs(tabCloseCurrent, false);
        CurrentFrame.SetFocus;
      end;

    cmd_FileCloseAll:
      begin
        Groups.CloseTabs(tabCloseAll, false);
        CurrentFrame.SetFocus;
      end;

    cmd_FileCloseOtherThis:
      begin
        Groups.CloseTabs(tabCloseOthersThisPage, false);
      end;

    cmd_FileCloseOtherAll:
      begin
        Groups.CloseTabs(tabCloseOthersAllPages, false);
      end;

    cmd_FileCloseAndDelete:
      begin
        DoFileCloseAndDelete;
      end;

    cmd_FileNew:             DoFileOpen('');
    cmd_FileNewMenu:         DoFileNewMenu(nil);
    cmd_FileOpen:            DoFileOpenDialog;
    cmd_FileOpenFolder:      DoPyCommand('cuda_project_man', 'new_project_open_dir', []);
    cmd_FileOpen_NoPlugins:  DoFileOpenDialog_NoPlugins;
    cmd_FileReopen:          DoFileReopen;
    cmd_FileSave:            DoFileSave;
    cmd_FileSaveAs:          DoFileSaveAs;
    cmd_FileSaveAll:         DoFileSaveAll;

    cmd_FileExit:            Close;
    cmd_FileExportHtml:      DoFileExportHtml;

    cmd_FocusEditor:           DoFocusEditor;
    cmd_SwitchActiveTabToNext: DoSwitchActiveTab(true);
    cmd_SwitchActiveTabToPrev: DoSwitchActiveTab(false);

    cmd_DialogCommands:    DoDialogCommands;
    cmd_DialogGoto:        DoDialogGoto;
    cmd_DialogGoto_Hide:   DoDialogGoto_Hide;
    cmd_DialogGotoBookmark: DoDialogGotoBookmark;
    cmd_DialogFind:        DoDialogFind(false);
    cmd_DialogFind_Hide:   DoDialogFind_Hide;
    cmd_DialogReplace:     DoDialogFind(true);
    cmd_DialogSaveTabs:    DoDialogSaveTabs;
    cmd_DialogLexerProp:   DoDialogLexerProp(CurrentFrame.Lexer);
    cmd_DialogLexerLib:    DoDialogLexerLib;
    cmd_DialogLexerStyleMap: DoDialogLexerMap;
    cmd_DialogLoadLexerStyles: DoDialogRestoreLexerStyles;
    cmd_DialogThemeUi:     DoDialogTheme(true);
    cmd_DialogThemeSyntax: DoDialogTheme(false);
    cmd_DialogCharMap:     DoDialogCharMap;

    cmd_ToggleFullScreen:  DoToggleFullScreen;
    cmd_ToggleDistractionFree: DoToggleDistractionFree;
    cmd_ToggleSidePanel:   DoToggleSidePanel;
    cmd_ToggleBottomPanel: DoToggleBottomPanel;
    cmd_ToggleFindDialog:  DoToggleFindDialog;
    cmd_ToggleToolbar:     DoToggleToolbar;
    cmd_ToggleStatusbar:   DoToggleStatusbar;
    cmd_ToggleOnTop:       DoToggleOnTop;

    cmd_OpsReloadAndApply: DoOps_LoadOptionsAndApplyAll;
    cmd_ShowPanelConsole:  DoShowConsole(true);
    cmd_ShowPanelOutput:   DoShowOutput;
    cmd_ShowPanelValidate: DoShowValidate;

    cmd_ShowSidePanelAsIs: DoShowSidePanel('', false);
    cmd_ShowSidePanelAndSyntaxTree: DoShowSidePanel('Tree', false);
    cmd_HideSidePanel: DoShowSidePanel('-', false);

    cmd_FindFirst:       DoFindFirst;
    cmd_FindNext:        DoFindNext(true);
    cmd_FindPrev:        DoFindNext(false);
    cmd_FindAllAndSelect:     DoFindMarkAll(markingSelections);
    cmd_FindAllAndMarkers:    DoFindMarkAll(markingMarkers);
    cmd_FindAllAndBookmarks:  DoFindMarkAll(markingBookmarks);
    cmd_FindCurWordNext: DoFindCurrentWordOrSel(true, true);
    cmd_FindCurWordPrev: DoFindCurrentWordOrSel(false, true);
    cmd_FindCurSelNext:  DoFindCurrentWordOrSel(true, false);
    cmd_FindCurSelPrev:  DoFindCurrentWordOrSel(false, false);

    cmd_OpsClearRecent:     DoClearRecentFileHistory;
    cmd_OpsOpenDefault:     DoOps_OpenFile_Default;
    cmd_OpsOpenUser:        DoOps_OpenFile_User;
    cmd_OpsOpenLexerOvr:    DoOps_OpenFile_LexerSpecific;
    cmd_OpsOpenFileTypes:   DoOps_OpenFile_FileTypes;

    cmd_OpsFontText:        DoOps_DialogFont_Text;
    cmd_OpsFontUi:          DoOps_DialogFont_Ui;
    cmd_OpsFontOutput:      DoOps_DialogFont_Output;

    cmd_ResetPythonPlugins: DoPyResetPlugins;
    cmd_RunLastCommandPlugin: DoPyRunLastPlugin;
    cmd_RescanPythonPluginsInfFiles: DoPyRescanPlugins;

    cmd_Groups1:     Groups.Mode:= gmOne;
    cmd_Groups2horz: Groups.Mode:= gm2Horz;
    cmd_Groups2vert: Groups.Mode:= gm2Vert;
    cmd_Groups3horz: Groups.Mode:= gm3Horz;
    cmd_Groups3vert: Groups.Mode:= gm3Vert;
    cmd_Groups3plus: Groups.Mode:= gm1plus2Vert;
    cmd_Groups3plushorz: Groups.Mode:= gm1plus2Horz;
    cmd_Groups4horz: Groups.Mode:= gm4Horz;
    cmd_Groups4vert: Groups.Mode:= gm4Vert;
    cmd_Groups4grid: Groups.Mode:= gm4Grid;
    cmd_Groups6grid: Groups.Mode:= gm6Grid;

    cmd_GroupActivateNext: Groups.PagesSetNext(true);
    cmd_GroupActivatePrev: Groups.PagesSetNext(false);

    cmd_MoveTabToGroupNext: Groups.MoveCurrentTabToNext(true);
    cmd_MoveTabToGroupPrev: Groups.MoveCurrentTabToNext(false);

    cmd_SplitTabToggle:    with CurrentFrame do Splitted:= not Splitted;
    cmd_SplitTabHorzVert:  with CurrentFrame do SplitHorz:= not SplitHorz;
    cmd_SplitTab3070:   CurrentFrame.SplitPos:= 0.7;
    cmd_SplitTab4060:   CurrentFrame.SplitPos:= 0.6;
    cmd_SplitTab5050:   CurrentFrame.SplitPos:= 0.5;
    cmd_SplitTab6040:   CurrentFrame.SplitPos:= 0.4;
    cmd_SplitTab7030:   CurrentFrame.SplitPos:= 0.3;
    cmd_CopyLine:         DoCopyLine;
    cmd_CopyFilenameFull: DoCopyFilenameFull;
    cmd_CopyFilenameDir:  DoCopyFilenameDir;
    cmd_CopyFilenameName: DoCopyFilenameName;

    cmd_ToggleTabUsesSpaces: with CurrentEditor do OptTabSpaces:= not OptTabSpaces;
    cmd_ConvertTabsToSpaces: EditorConvertTabsToSpaces(CurrentEditor);
    cmd_ConvertSpacesToTabsLeading: EditorConvertSpacesToTabsLeading(CurrentEditor);

    cmd_BookmarkToggle: begin EditorBookmarkSet(CurrentEditor, -1, 1, bmOpToggle, ''); UpdateFrame; end;
    cmd_BookmarkClearAll: begin EditorBookmarkClearAll(CurrentEditor); UpdateFrame; end;
    cmd_BookmarkInvertAll: begin EditorBookmarkInvertAll(CurrentEditor); UpdateFrame; end;
    cmd_BookmarkGotoNext: EditorBookmarkGotoNext(CurrentEditor, true);
    cmd_BookmarkGotoPrev: EditorBookmarkGotoNext(CurrentEditor, false);
    cmd_BookmarkPlaceCarets: EditorBookmarkPlaceCaretsOnBookmarks(CurrentEditor);
    cmd_BookmarkPlaceBookmarksOnCarets: EditorBookmarkPlaceBookmarksOnCarets(CurrentEditor);
    cmd_BookmarkCopyMarkedLines: EditorBookmarkCopyMarkedLines(CurrentEditor);
    cmd_BookmarkDeleteMarkedLines: EditorBookmarkDeleteMarkedLines(CurrentEditor);

    cmd_DuplicateLineEx: DoCudaLibAction('duplicate');

    cmd_LineEndWin: SetLineEnds(cEndWin);
    cmd_LineEndUnix: SetLineEnds(cEndUnix);
    cmd_LineEndMac: SetLineEnds(cEndMac);

    cmd_AutoComplete: DoAutoComplete;
    cmd_GotoDefinition: DoGotoDefinition;
    cmd_ShowFunctionHint: DoShowFuncHint;

    cmd_FoldingFoldAtCurLine: EditorFoldUnfoldRangeAtCurLine(CurrentEditor, cEditorFold);
    cmd_FoldingUnfoldAtCurLine: EditorFoldUnfoldRangeAtCurLine(CurrentEditor, cEditorUnfold);
    cmd_FoldingToggleAtCurLine: EditorFoldUnfoldRangeAtCurLine(CurrentEditor, cEditorFoldUnfold);
    cmd_FoldingEnable: CurrentFrame.EnabledFolding:= true;
    cmd_FoldingDisable: CurrentFrame.EnabledFolding:= false;
    cmd_DeleteNewColorAttrs: EditorDeleteNewColorAttribs(CurrentEditor);

    cmd_MenuEnc:
      begin
        P:= CurrentEditor.ClientToScreen(Point(0, 0));
        PopupEnc.Popup(P.X, P.Y);
      end;
    cmd_MenuEnds:
      begin
        P:= CurrentEditor.ClientToScreen(Point(0, 0));
        PopupEnds.Popup(P.X, P.Y);
      end;
    cmd_MenuLexers:
      begin
        P:= CurrentEditor.ClientToScreen(Point(0, 0));
        PopupLex.Popup(P.X, P.Y);
      end;

    cmd_Encoding_ansi_NoReload: SetFrameEncoding(CurrentFrame, cEncNameAnsi, false);
    cmd_Encoding_utf8bom_NoReload: SetFrameEncoding(CurrentFrame, cEncNameUtf8_WithBom, false);
    cmd_Encoding_utf8nobom_NoReload: SetFrameEncoding(CurrentFrame, cEncNameUtf8_NoBom, false);
    cmd_Encoding_utf16le_NoReload: SetFrameEncoding(CurrentFrame, cEncNameUtf16LE_WithBom, false);
    cmd_Encoding_utf16be_NoReload: SetFrameEncoding(CurrentFrame, cEncNameUtf16BE_WithBom, false);
    cmd_Encoding_cp1250_NoReload: SetFrameEncoding(CurrentFrame, cEncNameCP1250, false);
    cmd_Encoding_cp1251_NoReload: SetFrameEncoding(CurrentFrame, cEncNameCP1251, false);
    cmd_Encoding_cp1252_NoReload: SetFrameEncoding(CurrentFrame, cEncNameCP1252, false);
    cmd_Encoding_cp1253_NoReload: SetFrameEncoding(CurrentFrame, cEncNameCP1253, false);
    cmd_Encoding_cp1254_NoReload: SetFrameEncoding(CurrentFrame, cEncNameCP1254, false);
    cmd_Encoding_cp1255_NoReload: SetFrameEncoding(CurrentFrame, cEncNameCP1255, false);
    cmd_Encoding_cp1256_NoReload: SetFrameEncoding(CurrentFrame, cEncNameCP1256, false);
    cmd_Encoding_cp1257_NoReload: SetFrameEncoding(CurrentFrame, cEncNameCP1257, false);
    cmd_Encoding_cp1258_NoReload: SetFrameEncoding(CurrentFrame, cEncNameCP1258, false);
    cmd_Encoding_mac_NoReload: SetFrameEncoding(CurrentFrame, cEncNameMac, false);
    cmd_Encoding_iso1_NoReload: SetFrameEncoding(CurrentFrame, cEncNameIso1, false);
    cmd_Encoding_iso2_NoReload: SetFrameEncoding(CurrentFrame, cEncNameIso2, false);
    cmd_Encoding_cp437_NoReload: SetFrameEncoding(CurrentFrame, cEncNameCP437, false);
    cmd_Encoding_cp850_NoReload: SetFrameEncoding(CurrentFrame, cEncNameCP850, false);
    cmd_Encoding_cp852_NoReload: SetFrameEncoding(CurrentFrame, cEncNameCP852, false);
    cmd_Encoding_cp866_NoReload: SetFrameEncoding(CurrentFrame, cEncNameCP866, false);
    cmd_Encoding_cp874_NoReload: SetFrameEncoding(CurrentFrame, cEncNameCP874, false);
    cmd_Encoding_cp932_NoReload: SetFrameEncoding(CurrentFrame, cEncNameCP932, false);
    cmd_Encoding_cp936_NoReload: SetFrameEncoding(CurrentFrame, cEncNameCP936, false);
    cmd_Encoding_cp949_NoReload: SetFrameEncoding(CurrentFrame, cEncNameCP949, false);
    cmd_Encoding_cp950_NoReload: SetFrameEncoding(CurrentFrame, cEncNameCP950, false);

    cmd_Encoding_ansi_Reload: SetFrameEncoding(CurrentFrame, cEncNameAnsi, true);
    cmd_Encoding_utf8bom_Reload: SetFrameEncoding(CurrentFrame, cEncNameUtf8_WithBom, true);
    cmd_Encoding_utf8nobom_Reload: SetFrameEncoding(CurrentFrame, cEncNameUtf8_NoBom, true);
    cmd_Encoding_utf16le_Reload: SetFrameEncoding(CurrentFrame, cEncNameUtf16LE_WithBom, true);
    cmd_Encoding_utf16be_Reload: SetFrameEncoding(CurrentFrame, cEncNameUtf16BE_WithBom, true);
    cmd_Encoding_cp1250_Reload: SetFrameEncoding(CurrentFrame, cEncNameCP1250, true);
    cmd_Encoding_cp1251_Reload: SetFrameEncoding(CurrentFrame, cEncNameCP1251, true);
    cmd_Encoding_cp1252_Reload: SetFrameEncoding(CurrentFrame, cEncNameCP1252, true);
    cmd_Encoding_cp1253_Reload: SetFrameEncoding(CurrentFrame, cEncNameCP1253, true);
    cmd_Encoding_cp1254_Reload: SetFrameEncoding(CurrentFrame, cEncNameCP1254, true);
    cmd_Encoding_cp1255_Reload: SetFrameEncoding(CurrentFrame, cEncNameCP1255, true);
    cmd_Encoding_cp1256_Reload: SetFrameEncoding(CurrentFrame, cEncNameCP1256, true);
    cmd_Encoding_cp1257_Reload: SetFrameEncoding(CurrentFrame, cEncNameCP1257, true);
    cmd_Encoding_cp1258_Reload: SetFrameEncoding(CurrentFrame, cEncNameCP1258, true);
    cmd_Encoding_mac_Reload: SetFrameEncoding(CurrentFrame, cEncNameMac, true);
    cmd_Encoding_iso1_Reload: SetFrameEncoding(CurrentFrame, cEncNameIso1, true);
    cmd_Encoding_iso2_Reload: SetFrameEncoding(CurrentFrame, cEncNameIso2, true);
    cmd_Encoding_cp437_Reload: SetFrameEncoding(CurrentFrame, cEncNameCP437, true);
    cmd_Encoding_cp850_Reload: SetFrameEncoding(CurrentFrame, cEncNameCP850, true);
    cmd_Encoding_cp852_Reload: SetFrameEncoding(CurrentFrame, cEncNameCP852, true);
    cmd_Encoding_cp866_Reload: SetFrameEncoding(CurrentFrame, cEncNameCP866, true);
    cmd_Encoding_cp874_Reload: SetFrameEncoding(CurrentFrame, cEncNameCP874, true);
    cmd_Encoding_cp932_Reload: SetFrameEncoding(CurrentFrame, cEncNameCP932, true);
    cmd_Encoding_cp936_Reload: SetFrameEncoding(CurrentFrame, cEncNameCP936, true);
    cmd_Encoding_cp949_Reload: SetFrameEncoding(CurrentFrame, cEncNameCP949, true);
    cmd_Encoding_cp950_Reload: SetFrameEncoding(CurrentFrame, cEncNameCP950, true);

    cmd_Markers_DropAtCaret:
      begin
        EditorMarkerDrop(CurrentEditor);
        UpdateStatus;
      end;
    cmd_Markers_GotoLastAndDelete:
      begin
        EditorMarkerGotoLast(CurrentEditor, true);
        if CurrentEditor.Markers.Count=0 then
          CurrentFrame.TabKeyCollectMarkers:= false;
        UpdateStatus;
      end;
    cmd_Markers_GotoLastNoDelete:
      begin
        EditorMarkerGotoLast(CurrentEditor, false);
        UpdateStatus;
      end;
    cmd_Markers_ClearAll:
      begin
        EditorMarkerClearAll(CurrentEditor);
        CurrentFrame.TabKeyCollectMarkers:= false;
        UpdateStatus;
      end;
    cmd_Markers_SwapCaretAndMarker:
      begin
        EditorMarkerSwap(CurrentEditor);
        UpdateStatus;
      end;

    cmd_MacroStart:
      begin
        CurrentFrame.DoMacroStart;
        MsgStatus('');
      end;
    cmd_MacroStop:
      begin
        CurrentFrame.DoMacroStop(false);
        DoOnMacro(CurrentFrame.MacroString);
        MsgStatus('');
      end;
    cmd_MacroCancel:
      begin
        CurrentFrame.DoMacroStop(true);
        MsgStatus('');
      end;

    cmd_LinkAtCaret_Open:
      begin
        Str:= EditorGetLinkAtCaret(CurrentEditor);
        if Str<>'' then
        begin
          OpenURL(Str);
          MsgStatus(msgStatusOpenedBrowser);
        end;
      end;

    cmd_LinkAtCaret_Copy:
      begin
        Str:= EditorGetLinkAtCaret(CurrentEditor);
        if Str<>'' then
        begin
          SClipboardCopy(UTF8Encode(Str));
          MsgStatus(msgStatusCopiedLink);
        end;
      end;

    cmd_LinkAtPopup_Open:
      begin
        Str:= EditorGetLinkAtScreenCoord(CurrentEditor, PopupText.PopupPoint);
        if Str<>'' then
        begin
          OpenURL(Str);
          MsgStatus(msgStatusOpenedBrowser);
        end;
      end;

    cmd_LinkAtPopup_Copy:
      begin
        Str:= EditorGetLinkAtScreenCoord(CurrentEditor, PopupText.PopupPoint);
        if Str<>'' then
        begin
          SClipboardCopy(UTF8Encode(Str));
          MsgStatus(msgStatusCopiedLink);
        end;
      end;

    cmd_TreeGotoNext: DoTreeviewJump(Tree, treeGoNext);
    cmd_TreeGotoPrev: DoTreeviewJump(Tree, treeGoPrev);
    cmd_TreeGotoParent: DoTreeviewJump(Tree, treeGoParent);
    cmd_TreeGotoNextBrother: DoTreeviewJump(Tree, treeGoNextBro);
    cmd_TreeGotoPrevBrother: DoTreeviewJump(Tree, treeGoPrevBro);
    cmd_TreeUpdate: UpdateTree(true, false{ignore visible}, true{ForceUpdateAll});

    cmd_HelpAbout: DoHelpAbout;
    cmd_HelpForum: DoHelpForum;
    cmd_HelpWiki: DoHelpWiki;
    cmd_HelpMouse: DoHelpMouse;
    cmd_HelpChangelog: DoHelpChangelog;
    cmd_HelpLexers: DoHelpLexers;
    cmd_HelpIssues: DoHelpIssues;
    cmd_HelpHotkeys: DoHelpHotkeys;
    cmd_HelpCheckUpdates: DoPyCommand('cuda_addonman', 'check_cudatext_updates', []);

    cmdFirstLexerCommand..
    cmdLastLexerCommand:
      SetLexerIndex(Cmd-cmdFirstLexerCommand);

    cmdFirstPluginCommand..
    cmdLastPluginCommand:
      DoPyCommand_ByPluginIndex(Cmd-cmdFirstPluginCommand);
  end;

  UpdateStatus;
end;

procedure TfmMain.TimerStatusAltTimer(Sender: TObject);
begin
  TimerStatusAlt.Enabled:= false;
  StatusAlt.Hide;
end;

procedure TfmMain.FrameOnCommand(Sender: TObject; ACommand: integer;
  const AText: string; var AHandled: boolean);
var
  Ed: TATSynEdit;
  Frame: TEditorFrame;
  Str: string;
begin
  AHandled:= true;
  if not Groups.Enabled then Exit;

  Ed:= Sender as TATSynEdit;
  Frame:= GetEditorFrame(Ed);

  if Frame.MacroRecord then
    if not PyCommandRunning then
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
    if DoPyEvent(Ed, cEventOnInsert,
      [SStringToPythonString(AText)]) = cPyFalse then
      begin AHandled:= true; exit end;
  end;

  //auto-close brackets
  if (ACommand=cCommand_TextInsert) and
    (UiOps.AutoCloseBrackets<>'') and
    (Length(AText)=1) and
    (Pos(AText[1], UiOps.AutoCloseBrackets)>0) then
  begin
    if EditorAutoCloseBracket(Ed, AText[1]) then
      begin AHandled:= true; exit end;
  end;

  //handle commands cmd_
  if (ACommand>=cmdFirstAppCommand) and
     (ACommand<cmdLastPluginCommand) then
    if IsCommandNeedTimer(ACommand) then
    begin
      TimerCmd.Tag:= ACommand;
      TimerCmd.Enabled:= true;
      exit
    end
    else
    begin
      TimerCmd.Tag:= ACommand;
      TimerCmdTimer(nil);
      exit
    end;

  case ACommand of
    cCommand_KeyTab:
      begin
        AHandled:= Frame.TabKeyCollectMarkers and (Ed.Markers.Count>0);
        if AHandled then
        begin
          EditorMarkerGotoLast(Ed, true);
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


