{$ifdef nn}begin end;{$endif}

function TfmMain.IsFocusedBottom: boolean;
begin
  Result:=
    fmConsole.ed.Focused or
    fmConsole.memo.Focused or
    ListboxOut.Focused or
    ListboxVal.Focused;
end;

procedure TfmMain.DoBottom_OnTabClick(Sender: TObject);
var
  N: integer;
  Data: TATTabData;
  Ctl: TWinControl;
begin
  fmConsole.Hide;
  ListboxOut.Hide;
  ListboxVal.Hide;
  TreeRes.Hide;
  //hide api-added panels
  for N:= 0 to FPanelCaptions.Count-1 do
    (FPanelCaptions.Objects[N] as TAppPanelPropsClass).Data.Listbox.Hide;

  case TabsBottom.TabIndex of
    0:
      begin
        fmConsole.Show;
        fmConsole.Ed.SetFocus;
      end;
    1:
      begin
        ListboxOut.Show;
        ListboxOut.SetFocus;
      end;
    2:
      begin
        ListboxVal.Show;
        ListboxVal.SetFocus;
      end;
    3:
      begin
        TreeRes.Show;
        TreeRes.SetFocus;
      end;
    else
      begin
        Data:= TabsBottom.GetTabData(TabsBottom.TabIndex);
        if Data=nil then exit;
        N:= FPanelCaptions.IndexOf(Data.TabCaption);
        if N<0 then exit;
        Ctl:= (FPanelCaptions.Objects[N] as TAppPanelPropsClass).Data.Listbox;
        Ctl.Show;
        if Ctl.CanFocus and Ctl.CanSetFocus then
          Ctl.SetFocus;
      end;
  end;
end;

function TfmMain.DoBottom_CaptionToControlHandle(const ACaption: string): PtrInt;
begin
  Result:= 0;

  if ACaption='Search Results' then
    Exit(PtrInt(TreeRes));
end;


