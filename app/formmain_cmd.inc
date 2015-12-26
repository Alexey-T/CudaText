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
begin
  TimerCmd.Enabled:= false;
  Cmd:= TimerCmd.Tag;
  case Cmd of
    cmd_FileNew:             DoFileOpen('');
    cmd_FileOpen:            DoFileOpenDialog;
    cmd_FileReopen:          DoFileReopen;

    cmd_FileClose:           Groups.CloseTabs(tabCloseCurrent, false);
    cmd_FileCloseAll:        Groups.CloseTabs(tabCloseAll, false);
    cmd_FileCloseOtherThis:  Groups.CloseTabs(tabCloseOthersThisPage, false);
    cmd_FileCloseOtherAll:   Groups.CloseTabs(tabCloseOthersAllPages, false);
    cmd_FileCloseAndDelete:  DoFileCloseAndDelete;

    cmd_FileSave:            if CurrentFrame.Modified then
                               CurrentFrame.DoFileSave(false, SaveDlg);
    cmd_FileSaveAs:          CurrentFrame.DoFileSave(true, SaveDlg);
    cmd_FileSaveAll:         DoFileSaveAll;

    cmd_FileExit:            Close;
    cmd_FileExportHtml:      DoFileExportHtml;

    cmd_DialogCommands:    DoDialogCommands;
    cmd_DialogGoto:        DoDialogGoto;
    cmd_DialogGotoBookmark: DoDialogGotoBookmk;
    cmd_DialogFind:        DoDialogFind(false);
    cmd_DialogReplace:     DoDialogFind(true);
    cmd_DialogSaveTabs:    DoDialogSaveTabs;
    cmd_DialogLexerProp:   DoDialogLexerProp(CurrentFrame.Lexer);
    cmd_DialogLexerLib:    DoDialogLexerLib;
    cmd_DialogLoadLexerStyles: DoDialogLoadLexerStyles;
    cmd_DialogColors:      DoDialogColors;
    cmd_DialogCharMap:     DoDialogCharMap;

    cmd_ToggleFullScreen:  DoToggleFullScreen;
    cmd_ToggleSidePanel:   DoToggleSidePanel;
    cmd_ToggleBottomPanel: DoToggleBottomPanel;
    cmd_ToggleFindDialog:  DoToggleFindDialog;
    cmd_ToggleToolbar:     DoToggleToolbar;
    cmd_ToggleStatusbar:   DoToggleStatusbar;

    cmd_OpsReloadAndApply: DoOps_LoadOptionsAndApplyAll;
    cmd_ShowPanelConsole:  DoShowConsole;
    cmd_ShowPanelOutput:   DoShowOutput;
    cmd_ShowPanelValidate: DoShowValidate;

    cmd_FindNext:        DoFindNext(true);
    cmd_FindPrev:        DoFindNext(false);
    cmd_FindCurWordNext: DoFindCurrentWordOrSel(true, true);
    cmd_FindCurWordPrev: DoFindCurrentWordOrSel(false, true);
    cmd_FindCurSelNext:  DoFindCurrentWordOrSel(true, false);
    cmd_FindCurSelPrev:  DoFindCurrentWordOrSel(false, false);

    cmd_OpsClearRecent:     DoClearRecentFileHistory;
    cmd_OpsOpenDefault:     DoOps_OpenFile_Default;
    cmd_OpsOpenUser:        DoOps_OpenFile_User;
    cmd_OpsOpenLexerOvr:    DoOps_OpenFile_LexerOvr;
    cmd_OpsOpenFileTypes:   DoOps_OpenFile_FileTypes;
    cmd_OpsFontText:        DoOps_DlgFontText;
    cmd_OpsFontUi:          DoOps_DlgFontUi;

    cmd_ResetPythonPlugins: DoPyResetPlugins;
    cmd_RunLastCommandPlugin: DoPyRunLastPlugin;

    cmd_Groups1:     Groups.Mode:= gmOne;
    cmd_Groups2horz: Groups.Mode:= gm2Horz;
    cmd_Groups2vert: Groups.Mode:= gm2Vert;
    cmd_Groups3horz: Groups.Mode:= gm3Horz;
    cmd_Groups3vert: Groups.Mode:= gm3Vert;
    cmd_Groups3plus: Groups.Mode:= gm3Plus;
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

    cmd_SortAsc: DoSortSel(CurrentEditor, true, false);
    cmd_SortDesc: DoSortSel(CurrentEditor, false, false);

    cmd_ConvertTabsToSpaces: EditorConvertTabsToSpaces(CurrentEditor);
    cmd_ConvertSpacesToTabsLeading: EditorConvertSpacesToTabsLeading(CurrentEditor);

    cmd_BookmarkToggle: begin EditorBmSet(CurrentEditor, -1, 1, bmOpToggle); UpdateFrame; end;
    cmd_BookmarkClearAll: begin EditorBmClearAll(CurrentEditor); UpdateFrame; end;
    cmd_BookmarkInvertAll: begin EditorBmInvertAll(CurrentEditor); UpdateFrame; end;
    cmd_BookmarkGotoNext: EditorBmGotoNext(CurrentEditor, true);
    cmd_BookmarkGotoPrev: EditorBmGotoNext(CurrentEditor, false);

    cmd_CommentLineAdd_AtNonspace: DoCommentAction(cCommentAdd_AtNonespace_IfNone);
    cmd_CommentLineAdd_AtStart: DoCommentAction(cCommentAdd_AtStart_IfNone);
    cmd_CommentLineRemove: DoCommentAction(cCommentRemove);
    cmd_CommentLineToggle_AtNonspace: DoCudaLibAction('cmt_toggle_line_body'); //DoCommentAction(cCommentToggle_AtNonspace);
    cmd_CommentLineToggle_AtStart: DoCudaLibAction('cmt_toggle_line_1st'); //DoCommentAction(cCommentToggle_AtStart);
    cmd_CommentStreamToggle: DoCudaLibAction('cmt_toggle_stream');
    cmd_DuplicateLineEx: DoCudaLibAction('duplicate');

    cmd_LineEndWin: SetLineEnds(cEndWin);
    cmd_LineEndUnix: SetLineEnds(cEndUnix);
    cmd_LineEndMac: SetLineEnds(cEndMac);

    cmd_AutoComplete: DoAutoComplete;
    cmd_GotoDefinition: DoGotoDefinition;

    cmd_FoldingFoldAtCurLine: EditorFoldUnfoldRangeAtCurLine(CurrentEditor, true);
    cmd_FoldingUnfoldAtCurLine: EditorFoldUnfoldRangeAtCurLine(CurrentEditor, false);

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
    cmd_Encoding_utf8bom_NoReload: SetFrameEncoding(CurrentFrame, cEncNameUtf8, false);
    cmd_Encoding_utf8nobom_NoReload: SetFrameEncoding(CurrentFrame, cEncNameUtf8NoBom, false);
    cmd_Encoding_utf16le_NoReload: SetFrameEncoding(CurrentFrame, cEncNameUtf16LE, false);
    cmd_Encoding_utf16be_NoReload: SetFrameEncoding(CurrentFrame, cEncNameUtf16BE, false);
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
    cmd_Encoding_utf8bom_Reload: SetFrameEncoding(CurrentFrame, cEncNameUtf8, true);
    cmd_Encoding_utf8nobom_Reload: SetFrameEncoding(CurrentFrame, cEncNameUtf8NoBom, true);
    cmd_Encoding_utf16le_Reload: SetFrameEncoding(CurrentFrame, cEncNameUtf16LE, true);
    cmd_Encoding_utf16be_Reload: SetFrameEncoding(CurrentFrame, cEncNameUtf16BE, true);
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

    cmd_TreeGotoNext: DoTreeviewJump(Tree, treeGoNext);
    cmd_TreeGotoPrev: DoTreeviewJump(Tree, treeGoPrev);
    cmd_TreeGotoParent: DoTreeviewJump(Tree, treeGoParent);
    cmd_TreeGotoNextBrother: DoTreeviewJump(Tree, treeGoNextBro);
    cmd_TreeGotoPrevBrother: DoTreeviewJump(Tree, treeGoPrevBro);
    cmd_TreeUpdate: UpdateTree(true, false{ignore visible});

    cmd_HelpAbout: DoHelpAbout;
    cmd_HelpForum: DoHelpForum;
    cmd_HelpWiki: DoHelpWiki;
    cmd_HelpMouse: DoHelpMouse;
    cmd_HelpChangelog: DoHelpChangelog;
    cmd_HelpLexers: DoHelpLexers;


    cmdFirstLexerCommand..
    cmdLastLexerCommand:
      SetLexerIndex(Cmd-cmdFirstLexerCommand);

    cmdFirstPluginCommand..
    cmdLastPluginCommand:
      Py_RunPlugin_Index(Cmd-cmdFirstPluginCommand);
  end;

  UpdateStatus;
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

    cmd_MouseClick:
      EditorMouseClickFromString(Ed, AText, false);
    cmd_MouseSelect:
      EditorMouseClickFromString(Ed, AText, true);
    cmd_FinderAction:
      DoFindActionFromString(AText);

    else
      AHandled:= false;
  end;
end;


