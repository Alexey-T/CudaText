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

  fmMain.UpdateToolbar;
end;

procedure DoToolbar_SetImageIndex(AToolbar: TATButtonsToolbar; AButtonIndex, AImageIndex: integer);
begin
  if (AButtonIndex>=0) and (AButtonIndex<AToolbar.ButtonCount) then
    AToolbar.Buttons[AButtonIndex].ImageIndex:= AImageIndex;
  fmMain.UpdateToolbar;
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

