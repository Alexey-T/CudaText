(*
This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at http://mozilla.org/MPL/2.0/.

Copyright (c) Alexey Torgashin
*)
{$ifdef nn}begin end;{$endif}

procedure TfmMain.FrameOnChangeCaption(Sender: TObject);
var
  Gr: TATGroups;
  Pages: TATPages;
  Frame: TEditorFrame;
  NLocalGroup, NGlobalGroup, NTab: integer;
  D: TATTabData;
begin
  Frame:= Sender as TEditorFrame;
  GetFrameLocation(Frame, Gr, Pages, NLocalGroup, NGlobalGroup, NTab);
  if Gr=nil then exit;

  D:= Pages.Tabs.GetTabData(NTab);
  if D=nil then Exit;
  D.TabCaption:= Frame.TabCaption;
  D.TabCaptionAddon:= Frame.TabCaptionAddon;

  if Frame.EditorsLinked then
  begin
    D.TabTwoDocuments:= false;
    D.TabModified:= Frame.Ed1.Modified;
    D.TabModified2:= false;
  end
  else
  begin
    D.TabTwoDocuments:= true;
    D.TabModified:= Frame.Ed1.Modified;
    D.TabModified2:= Frame.Ed2.Modified;
  end;

  if NTab=Pages.Tabs.TabIndex then
  begin
    Pages.Tabs.PaintSimulated; //updates tab rects, for MakeVisible
    Pages.Tabs.MakeVisible(NTab);
  end;

  Pages.Invalidate;

  UpdateCaption;
end;

procedure TfmMain.FrameOnUpdateStatusbar(Sender: TObject; AReason: TAppStatusbarUpdateReason);
var
  N: integer;
begin
  case AReason of
    TAppStatusbarUpdateReason.Caret, //Caret: add with care! it may make editor repaint slower on mouse selection; issue #4917
    TAppStatusbarUpdateReason.ViewerScroll,
    TAppStatusbarUpdateReason.InsOvr,
    TAppStatusbarUpdateReason.PictureResize:
      begin
        //some events only need the fast update of the statusbar
        UpdateStatusbar_RealWork;
      end;
    TAppStatusbarUpdateReason.Zoom:
      begin
        N:= CurrentEditor.OptScaleFont;
        if N=0 then N:= 100;
        DoStatusbarTextByTag(Status, StatusbarTag_Zoom, IntToStr(N)+'%');
      end;
    else
      begin
        //request the call UpdateTabCaptionsFromFolders + update of the statusbar + update menu checkmarks
        FNeedUpdateStatuses:= true;
      end;
  end;
end;

procedure TfmMain.UpdateToolbarButtons_UndoAndRedo(Sender: TObject);
var
  Ed: TATSynEdit;
  St: TATStrings;
begin
  //during drag-drop, avoid changing button's Enabled to false - it will call
  //TControl.CaptureChanged which calls TDragPerformer.DragStop,
  //which causes crash in TDragPerformer.DragMove
  if DragManager.IsDragging then exit;

  Ed:= CurrentEditor;
  if Ed=nil then exit;
  St:= Ed.Strings;
  UpdateToolbarButton(ToolbarMain, cCommand_Undo, false, not St.ReadOnly and not St.UndoEmpty);
  UpdateToolbarButton(ToolbarMain, cCommand_Redo, false, not St.ReadOnly and not St.RedoEmpty);
end;


procedure TfmMain.FrameOnUpdateState(Sender: TObject);
begin
  UpdateMenuChecks(false);
end;

procedure TfmMain.DoCodetree_StopUpdate;
var
  F: TEditorFrame;
begin
  F:= CurrentFrame;
  if F=nil then exit;
  F.Adapter[F.Editor].StopTreeUpdate;
end;

procedure TfmMain.FrameOnEditorFocus(Sender: TObject);
var
  F: TEditorFrame;
  Ed: TATSynEdit;
  Tree: TTreeView;
  bTreeCached: boolean;
begin
  Ed:= Sender as TATSynEdit;
  F:= TGroupsHelper.GetEditorFrame(Ed);

  F.GetTabGroups.PagesCurrent:= F.GetTabPages;

  UpdateCaption;
  UpdateTabsActiveColor(F);
  UpdateMenuPlugins_Shortcuts;

  if FNewClickedEditor<>Ed then
  begin
    FNewClickedEditor:= Ed;

    //prevent filling tree from ParseDone in previous focused frame
    TimerTreeFill.Enabled:= false;

    bTreeCached:= false;
    if F.CachedTreeViewInited[Ed] then
    begin
      Tree:= F.CachedTreeView[Ed];
      if (Tree.Items.Count>0) then
      begin
        bTreeCached:= true;

        DoPyEvent_AppState(APPSTATE_CODETREE_BEFORE_FILL);
        DoTreeviewCopy(Tree, CodeTree.Tree);
        CodeTree.Tree.SortType:= F.CodetreeSortType;
        if CodeTree.Tree.SortType<>stNone then
          CodeTree.Tree.AlphaSort;
        DoPyEvent_AppState(APPSTATE_CODETREE_AFTER_FILL);

        UpdateTreeFilter;
        UpdateTreeSelection(Ed);
        DoCodetree_UpdateVersion(Ed);
      end;
    end;

    if not bTreeCached then
      UpdateTreeByTimer;

    UpdateToolbarButtons_UndoAndRedo(nil);
  end;
end;

function TfmMain.GetFrame(AIndex: integer): TEditorFrame;
var
  D: TATTabData;
  NCount: integer;
begin
  Result:= nil;

  //frame can be inside Groups, GroupsFloating1..GroupsFloating3
  NCount:= Groups.GetTabTotalCount;
  if AIndex<NCount then
  begin
    D:= Groups.GetTabDataOfTotalIndex(AIndex);
    if Assigned(D) then
      Result:= D.TabObject as TEditorFrame;
    exit;
  end;

  if FloatingForms then
  begin
    Dec(AIndex, NCount);
    NCount:= GroupsFloating1.GetTabTotalCount;
    if AIndex<NCount then
    begin
      D:= GroupsFloating1.GetTabDataOfTotalIndex(AIndex);
      if Assigned(D) then
        Result:= D.TabObject as TEditorFrame;
      exit;
    end;

    Dec(AIndex, NCount);
    NCount:= GroupsFloating2.GetTabTotalCount;
    if AIndex<NCount then
    begin
      D:= GroupsFloating2.GetTabDataOfTotalIndex(AIndex);
      if Assigned(D) then
        Result:= D.TabObject as TEditorFrame;
      exit;
    end;

    Dec(AIndex, NCount);
    NCount:= GroupsFloating3.GetTabTotalCount;
    if AIndex<NCount then
    begin
      D:= GroupsFloating3.GetTabDataOfTotalIndex(AIndex);
      if Assigned(D) then
        Result:= D.TabObject as TEditorFrame;
      exit;
    end;
  end;
end;

function TfmMain.FrameCount: integer;
begin
  Result:= Groups.GetTabTotalCount;
  if FloatingForms then
  begin
    Inc(Result, GroupsFloating1.GetTabTotalCount);
    Inc(Result, GroupsFloating2.GetTabTotalCount);
    Inc(Result, GroupsFloating3.GetTabTotalCount);
  end;
end;

function TfmMain.CurrentGroups: TATGroups;
  //
  function _FormAct(F: TForm): boolean; inline;
  begin
    Result:= Assigned(F)
      and (AppActiveForm=F)
      and F.Visible;
  end;
  //
begin
  Result:= Groups;

  if FloatingForms then
  begin
    if _FormAct(FFormFloating1) then
      Result:= GroupsFloating1
    else
    if _FormAct(FFormFloating2) then
      Result:= GroupsFloating2
    else
    if _FormAct(FFormFloating3) then
      Result:= GroupsFloating3
  end;
end;

function TfmMain.CurrentFrame: TEditorFrame;
begin
  Result:= CurrentFrameEx(CurrentGroups);
end;

function TfmMain.CurrentFrameEx(AGroups: TATGroups): TEditorFrame;
var
  Pages: TATPages;
  D: TATTabData;
begin
  Pages:= AGroups.PagesCurrent;
  D:= Pages.Tabs.GetTabData(Pages.Tabs.TabIndex);
  if Assigned(D) and Assigned(D.TabObject) then
    Result:= D.TabObject as TEditorFrame
  else
    Result:= nil;
end;

function TfmMain.CurrentEditor: TATSynEdit;
var
  F: TEditorFrame;
begin
  F:= CurrentFrame;
  if F<>nil then
    Result:= F.Editor
  else
    Result:= nil;
end;

procedure TfmMain.SetFrame(Frame: TEditorFrame);
var
  Gr: TATGroups;
  Pages: TATPages;
  NLocalGroup, NGlobalGroup, NTab: integer;
begin
  GetFrameLocation(Frame, Gr, Pages, NLocalGroup, NGlobalGroup, NTab);
  Gr.SetPagesAndTabIndex(NLocalGroup, NTab);
end;

procedure TfmMain.FrameOnSaveFile(Sender: TObject; const fn: string);
//var
//  Ed: TATSynEdit;
begin
  //Ed:= Sender as TATSynEdit;
  MsgStatus(
    msgStatusSavedFile+' '+
    ExtractFileName_Fixed(fn)+
    ' ('+AppCollapseHomeDirInFilename(ExtractFileDir_Fixed(fn))+')'
    );

  //if it's user.json, reread app config (using timer!)
  if SameFileName(fn, AppFile_OptionsUser) then
    CurrentEditor.DoCommand(cmd_OpsReloadAndApply, TATCommandInvoke.AppInternal);

  //untitled tab maybe became titled
  UpdateMenuChecks(false);
end;


procedure TfmMain.FrameOnInitAdapter(Sender: TObject);
begin
  with Sender as TATAdapterEControl do
  begin
    DynamicHiliteEnabled:= EditorOps.OpLexerDynamicHiliteMaxLines>0;
    DynamicHiliteMaxLines:= EditorOps.OpLexerDynamicHiliteMaxLines;

    OnParseDone:= @FrameParseDone;
    OnLexerChange:= @FrameLexerChange;
  end;
end;

procedure TfmMain.FrameParseDone(Sender: TObject);
var
  Ada: TATAdapterEControl;
  Ed: TATSynEdit;
begin
  UpdateLexerProgressbar(0, true);
  UpdateTreeByTimer;

  Ada:= Sender as TATAdapterEControl;
  Ed:= Ada.Editor;

  //restoring of folded state from history is here, after parsing done
  if not TreeHelperInPascal(nil, Ada.GetLexerName, nil) then
    if Ed.FoldingAsStringTodo<>'' then
    begin
      Ed.FoldingAsString:= Ed.FoldingAsStringTodo;
      Ed.FoldingAsStringTodo:= '';
    end;

  //do on_lexer_parsed only if parsing was long
  if Ada.LexerParsingElapsed>=UiOps.LexerParsingMinTimeForEvent then
    DoPyEvent(Ed, TAppPyEvent.OnLexerParsed, []);
end;

function TfmMain.FrameOfPopup: TEditorFrame;
var
  Pages: TATPages;
  NTab: integer;
  D: TATTabData;
begin
  Result:= nil;
  if GroupsCtx=nil then exit;
  Pages:= GroupsCtx.PopupPages; if Pages=nil then exit;
  NTab:= GroupsCtx.PopupTabIndex; if NTab<0 then exit;
  D:= Pages.Tabs.GetTabData(NTab); if D=nil then exit;
  Result:= D.TabObject as TEditorFrame;
end;

procedure TfmMain.FrameOnChangeSlow(Sender: TObject);
var
  Ed: TATSynEdit;
begin
  Ed:= Sender as TATSynEdit;

  //run tree-helper for lite lexers
  if Ed.AdapterForHilite is TATLiteLexer then
    UpdateTree(true);

  //call Find dialog's updating of 'Hi' results
  if Assigned(fmFind) and fmFind.Visible then
    if fmFind.chkHiAll.Enabled and fmFind.chkHiAll.Checked then
      fmFind.UpdateHiAll(false);
end;


