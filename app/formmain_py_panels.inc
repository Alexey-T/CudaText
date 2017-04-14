{$ifdef nn}begin end;{$endif}

procedure TfmMain.DoPanel_Event(AControl: TObject; const AEvent: string);
begin
  DoPyEvent(CurrentEditor, cEventOnPanel,
    [
      IntToStr(PtrInt(AControl)),
      '"'+AEvent+'"'
    ]);
end;

procedure TfmMain.DoPanel_OnContextPopup(Sender: TObject; MousePos: TPoint;
  var Handled: Boolean);
begin
  DoPanel_Event(Sender, 'on_menu');
end;

procedure TfmMain.DoPanel_OnClick(Sender: TObject);
begin
  DoPanel_Event(Sender, 'on_click');
end;

procedure TfmMain.DoPanel_OnDblClick(Sender: TObject);
begin
  DoPanel_Event(Sender, 'on_dbl_click');
end;

procedure TfmMain.DoPanel_OnSelChanged(Sender: TObject);
begin
  DoPanel_Event(Sender, 'on_sel');
end;

