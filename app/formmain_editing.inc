(*
This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at http://mozilla.org/MPL/2.0/.

Copyright (c) Alexey Torgashin
*)
{$ifdef nn}begin end;{$endif}


procedure TfmMain.DoSortSel(ed: TATSynEdit; Asc, ANocase: boolean);
var
  Res: boolean;
  Cnt: integer;
begin
  Res:= EditorSortSel(ed, Asc, ANocase, Cnt);
  if not Res then
    MsgStatus(msgStatusCancel)
  else
  begin
    MsgStatus(Format(msgStatusSorted, [Cnt]));
    UpdateFrame(true);
  end;
end;

procedure TfmMain.DoCommentAction(Act: TATCommentAction);
var
  F: TEditorFrame;
  Ed: TATSynEdit;
  Str: string;
begin
  F:= CurrentFrame;
  Ed:= CurrentEditor;
  Str:= F.CommentString;
  if Str='' then
    MsgStatus(msgStatusNoCmtStr)
  else
    Ed.DoCommentSelectionLines(Act, Str);
end;


