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
    cmd_FileSaveAll:         DoFileSaveAll;
    cmd_FileReopen:          DoFileReopen;

    cmd_FileClose:           Groups.CloseTabs(tabCloseCurrent, false);
    cmd_FileCloseAll:        Groups.CloseTabs(tabCloseAll, false);
    cmd_FileCloseOtherThis:  Groups.CloseTabs(tabCloseOthersThisPage, false);
    cmd_FileCloseOtherAll:   Groups.CloseTabs(tabCloseOthersAllPages, false);
    cmd_FileCloseAndDelete:  DoFileCloseAndDelete;
    cmd_FileSave:            CurrentFrame.DoFileSave(false, SaveDlg);
    cmd_FileSaveAs:          CurrentFrame.DoFileSave(true, SaveDlg);
    cmd_FileExit:            Close;
    cmd_FileExportHtml:      DoFileExportHtml;

    cmd_DlgCommands:    DoDialogCommands;
    cmd_DlgGoto:        DoDialogGoto;
    cmd_DlgGotoBm:      DoDialogGotoBm;
    cmd_DlgTabs:        DoDialogTabs;
    cmd_DlgFind:        DoDialogFind(false);
    cmd_DlgReplace:     DoDialogFind(true);
    cmd_DlgSaveTabs:    DoDialogSaveTabs;
    cmd_DlgLexerProp:   DoDialogLexerProp(CurrentFrame.Lexer);
    cmd_DlgLexerLib:    DoDialogLexerLib;
    cmd_DlgColors:      DoDialogColors;
    cmd_ToggleFullScreen:  DoToggleFullScreen;
    cmd_ToggleSidePanel:   DoToggleSidePanel;
    cmd_ToggleBottomPanel: DoToggleBottomPanel;
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

    cmd_OpsClearRecent:  DoClearRecentFileHistory;
    cmd_OpsOpenDefault:  DoOps_OpenFile_Default;
    cmd_OpsOpenUser:     DoOps_OpenFile_User;
    cmd_OpsOpenLexerOvr: DoOps_OpenFile_LexerOvr;
    cmd_OpsOpenFileTypes: DoOps_OpenFile_FileTypes;
    cmd_OpsFontText:     DoOps_DlgFontText;
    cmd_OpsFontUi:       DoOps_DlgFontUi;

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
    cmd_SortNocaseAsc: DoSortSel(CurrentEditor, true, true);
    cmd_SortNocaseDesc: DoSortSel(CurrentEditor, false, true);

    cmd_BookmarkToggle: begin EditorBmSet(CurrentEditor, -1, 1, bmOpToggle); UpdateFrame; end;
    cmd_BookmarkClearAll: begin EditorBmClearAll(CurrentEditor); UpdateFrame; end;
    cmd_BookmarkInvertAll: begin EditorBmInvertAll(CurrentEditor); UpdateFrame; end;
    cmd_BookmarkGotoNext: EditorBmGotoNext(CurrentEditor, true);
    cmd_BookmarkGotoPrev: EditorBmGotoNext(CurrentEditor, false);

    cmd_CommentLineAdd: DoCommentAct(cCommentAddIfNone);
    cmd_CommentLineRemove: DoCommentAct(cCommentRemove);
    cmd_CommentLineToggle: DoCommentAct(cCommentToggle);

    cmd_LineEndWin: SetLineEnds(cEndWin);
    cmd_LineEndUnix: SetLineEnds(cEndUnix);
    cmd_LineEndMac: SetLineEnds(cEndMac);

    cmd_EncAnsi: SetEnc(cEncNameAnsi);
    cmd_EncUtf8bom: SetEnc(cEncNameUtf8);
    cmd_EncUtf8nobom: SetEnc(cEncNameUtf8NoBom);
    cmd_EncUtf16le: SetEnc(cEncNameUtf16LE);
    cmd_EncUtf16be: SetEnc(cEncNameUtf16BE);

    cmd_AutoComplete: DoAutoComplete;

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

    cmdFirstLexerCommand..
    cmdLastLexerCommand:
      SetLexerIndex(Cmd-cmdFirstLexerCommand);

    cmdFirstPluginCommand..
    cmdLastPluginCommand:
      Py_RunPlugin_Index(Cmd-cmdFirstPluginCommand);
  end;

  UpdateStatus;
end;

procedure TfmMain.FrameOnCommand(Sender: TObject; Cmd: integer; var Handled: boolean);
begin
  Handled:= true;
  if not Groups.Enabled then Exit;

  case Cmd of
    cmd_First..10000:
      begin
        TimerCmd.Tag:= Cmd;
        TimerCmd.Enabled:= true;
      end;
    else
      Handled:= false;
  end;
end;

