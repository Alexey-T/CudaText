{$ifdef nnnn}begin end;{$endif}

function DoToolbar_GetToolbarFromId(const Str: string): TATButtonsToolbar;
begin
  Result:= nil;
  if Str='top' then
    exit(fmMain.Toolbar);
end;


function DoToolbar_GetImageSizes(AToolbar: TATButtonsToolbar): PPyObject;
begin
  with GetPythonEngine do
    Result:= Py_BuildValue('(ii)',
      AToolbar.Images.Width,
      AToolbar.Images.Height);
end;

procedure DoToolbar_SetImageSizes(AToolbar: TATButtonsToolbar; ASizeX, ASizeY: integer);
begin
  if AToolbar.Images=nil then
    AToolbar.Images:= TImageList.Create(fmMain);

  AToolbar.Images.Width:= ASizeX;
  AToolbar.Images.Height:= ASizeY;
  AToolbar.UpdateControls;
end;

procedure DoToolbar_SetImageIndex(AToolbar: TATButtonsToolbar; AButtonIndex, AImageIndex: integer);
begin
  if (AButtonIndex>=0) and (AButtonIndex<AToolbar.ButtonCount) then
    AToolbar.Buttons[AButtonIndex].ImageIndex:= AImageIndex;
  AToolbar.UpdateControls;
end;

function DoToolbar_AddImage(AToolbar: TATButtonsToolbar; const AFilename: string): PPyObject;
begin
  if AToolbar.Images=nil then
    AToolbar.Images:= TImageList.Create(fmMain);

  with GetPythonEngine do
    if UpdateImagelistWithIconFromFile(AToolbar.Images, AFilename) then
      Result:= PyInt_FromLong(AToolbar.Images.Count-1)
    else
      Result:= ReturnNone;
end;


procedure DoToolbar_Clear(AToolbar: TATButtonsToolbar);
var
  i: integer;
begin
  for i:= AToolbar.ButtonCount-1 downto 0 do
    AToolbar.Buttons[i].Free;
  AToolbar.UpdateControls;
end;

procedure DoToolbar_DeleteButton(AToolbar: TATButtonsToolbar; AButtonIndex: integer);
begin
  if (AButtonIndex>=0) and (AButtonIndex<AToolbar.ButtonCount) then
    AToolbar.Buttons[AButtonIndex].Free;
  AToolbar.UpdateControls;
end;


procedure DoToolbar_AddButton(
  AToolbar: TATButtonsToolbar;
  const ACaption: string;
  const ACommand: string;
  const AHint: string;
  AButtonIndex: integer;
  AImageIndex: integer);
var
  mi: TMenuItem;
  mnu: TPopupMenu;
begin
  if ACaption='-' then
  begin
    AToolbar.AddSep;
  end
  else
  if SBeginsWith(ACommand, 'toolmenu:') then
  begin
    mi:= TMenuItem.Create(fmMain);
    mi.Caption:= '('+ACommand+')';
    mnu:= TPopupMenu.Create(fmMain);
    mnu.Items.Add(mi);
    AToolbar.AddDropdown(mnu, nil, ACaption, AHint, ACommand);
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


const
  cATButtonKinds: array[TATButtonKind] of string = (
    'text',
    'icon',
    'text_icon',
    'text_icon',
    'text_arrow',
    'arrow',
    'sep',
    'sep',
    'x'
    );

function DoToolbar_Enum(AToolbar: TATButtonsToolbar): PPyObject;
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
        Py_BuildValue('{sssssssiss}',
          'cap',
          PChar(Btn.Caption),
          'hint',
          PChar(Btn.Hint),
          'cmd',
          PChar(Btn.DataString),
          'icon',
          Btn.ImageIndex,
          'kind',
          PChar(cATButtonKinds[Btn.Kind])
          ));
    end;
  end;
end;


function DoToolbar_SetButtonProps(
  AToolbar: TATButtonsToolbar;
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

