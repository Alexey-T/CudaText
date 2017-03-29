{$ifdef nnnn}begin end;{$endif}

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
  AImageIndex: integer);
var
  mi: TMenuItem;
  mnu: TPopupMenu;
begin
  if ACaption='-' then
  begin
    AToolbar.AddSep;
    exit
  end;

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

  AToolbar.UpdateControls;
end;


procedure DoToolbar_AddButton_FromString_Deprecated(
  AToolbar: TATButtonsToolbar; AStr: string);
var
  SHint, SCmd, SCaption, SImageIndex: string;
begin
  SHint:= SGetItem(AStr, ';');
  SImageIndex:= SGetItem(AStr, ';');
  SCmd:= SGetItem(AStr, ';');
  SCaption:= SGetItem(AStr, ';');

  DoToolbar_AddButton(
    AToolbar,
    SCaption,
    SCmd,
    SHint,
    StrToIntDef(SImageIndex, -1)
    );
end;


function DoToolbar_Enum_Deprecated(AToolbar: TATButtonsToolbar): PPyObject;
var
  Str: string;
  i: integer;
begin
  with GetPythonEngine do
  begin
    Str:= '';
    for i:= 0 to AToolbar.ButtonCount-1 do
      with AToolbar do
        Str:= Str+Format('%s;%s;%s'#10, [
          Buttons[i].Hint,
          Buttons[i].Caption,
          Buttons[i].DataString
          ]);
    Result:= PyString_FromString(PChar(Str));
  end;
end;

