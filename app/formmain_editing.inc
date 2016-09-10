{$ifdef nn}begin end;{$endif}

procedure TfmMain.DoCommentAction(Act: TATCommentAction);
var
  F: TEditorFrame;
  Ed: TATSynEdit;
  Str: string;
  bMoveCaret: boolean;
begin
  F:= CurrentFrame;
  Ed:= CurrentEditor;

  Str:= F.CommentString;
  if Str='' then
  begin
    MsgStatus(msgStatusNoLineCmtDefined);
    Exit
  end;

  bMoveCaret:= not Ed.Carets.IsSelection and (Ed.Carets.Count=1);
  Ed.DoCommentSelectionLines(Act, Str);
  if bMoveCaret then
  begin
    with Ed.Carets[0] do
      if PosY<Ed.Strings.Count-1 then
        PosY:= PosY+1;
    Ed.Update;
  end;
end;


