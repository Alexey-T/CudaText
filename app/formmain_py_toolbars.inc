{$ifdef nnnn}begin end;{$endif}

function DoToolbar_GetToolbarFromId(const Str: string): TATFlatToolbar;
var
  N: PtrInt;
begin
  Result:= nil;

  if Str='top' then
    exit(fmMain.ToolbarMain);

  N:= StrToInt64Def(Str, 0);
  if N<>0 then
    exit(TATFlatToolbar(N));
end;


procedure DoToolbar_Clear(AToolbar: TATFlatToolbar);
var
  i: integer;
begin
  for i:= AToolbar.ButtonCount-1 downto 0 do
    AToolbar.Buttons[i].Free;
  AToolbar.UpdateControls;
end;

procedure DoToolbar_DeleteButton(AToolbar: TATFlatToolbar; AButtonIndex: integer);
begin
  if (AButtonIndex>=0) and (AButtonIndex<AToolbar.ButtonCount) then
    AToolbar.Buttons[AButtonIndex].Free;
  AToolbar.UpdateControls;
end;

function DoToolbar_GetChecked(AToolbar: TATFlatToolbar; AButtonIndex: integer): boolean;
begin
  if (AButtonIndex>=0) and (AButtonIndex<AToolbar.ButtonCount) then
    Result:= AToolbar.Buttons[AButtonIndex].Checked
  else
    Result:= false;
end;

procedure DoToolbar_SetChecked(AToolbar: TATFlatToolbar; AButtonIndex: integer; AValue: boolean);
begin
  if (AButtonIndex>=0) and (AButtonIndex<AToolbar.ButtonCount) then
    AToolbar.Buttons[AButtonIndex].Checked:= AValue;
end;


procedure DoToolbar_AddMenu(
  AToolbar: TATFlatToolbar;
  const ACaption: string;
  const ACommand: string;
  const AHint: string;
  AButtonIndex: integer;
  AImageIndex: integer);
var
  mi: TMenuItem;
  mnu: TPopupMenu;
begin
  mi:= TMenuItem.Create(fmMain);
  mi.Caption:= '('+ACommand+')';
  mnu:= TPopupMenu.Create(fmMain);
  mnu.Items.Add(mi);
  AToolbar.AddDropdown(AImageIndex, mnu, nil, ACaption, AHint, ACommand);
end;


procedure DoToolbar_AddButton(
  AToolbar: TATFlatToolbar;
  const ACaption: string;
  const ACommand: string;
  const AHint: string;
  AButtonIndex: integer;
  AImageIndex: integer);
begin
  if ACaption='-' then
  begin
    AToolbar.AddSep;
  end
  else
  if SBeginsWith(ACommand, 'toolmenu:') then
  begin
    DoToolbar_AddMenu(AToolbar, ACaption, ACommand, AHint, AButtonIndex, AImageIndex);
  end
  else
  begin
    AToolbar.AddButton(
      AImageIndex,
      @fmMain.DoToolbarClick,
      ACaption,
      AHint,
      ACommand,
      ACaption<>''
      );
  end;

  if (AButtonIndex>=0) and (AButtonIndex<AToolbar.ButtonCount) then
    AToolbar.SetControlIndex(
      AToolbar.Controls[AToolbar.ControlCount-1],
      AButtonIndex);

  AToolbar.UpdateControls;
end;


function DoToolbar_Enum(AToolbar: TATFlatToolbar): PPyObject;
var
  NLen: integer;
  Btn: TATButton;
  i: integer;
begin
  with GetPythonEngine do
  begin
    NLen:= AToolbar.ButtonCount;
    Result:= PyList_New(NLen);
    if not Assigned(Result) then
      raise EPythonError.Create(msgPythonListError);
    for i:= 0 to NLen-1 do
    begin
      Btn:= AToolbar.Buttons[i];
      PyList_SetItem(Result, i,
        Py_BuildValue('{sssssssisi}',
          'cap',
          PChar(Btn.Caption),
          'hint',
          PChar(Btn.Hint),
          'cmd',
          PChar(Btn.DataString),
          'icon',
          Btn.ImageIndex,
          'kind',
          Ord(Btn.Kind)
          ));
    end;
  end;
end;


function DoToolbar_SetButtonProps(
  AToolbar: TATFlatToolbar;
  AButtonIndex, AImageIndex: integer;
  const ACaption, AHint: string
  ): PPyObject;
var
  Btn: TATButton;
begin
  with GetPythonEngine do
    if (AButtonIndex>=0) and (AButtonIndex<AToolbar.ButtonCount) then
    begin
      Btn:= AToolbar.Buttons[AButtonIndex];
      Btn.ImageIndex:= AImageIndex;
      if ACaption<>'' then
        Btn.Caption:= ACaption;
      if AHint<>'' then
        Btn.Hint:= AHint;

      Btn.Invalidate;
      Result:= PyBool_FromLong(1);
    end
    else
      Result:= PyBool_FromLong(0);
end;

