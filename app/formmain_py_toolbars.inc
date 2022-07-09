{$ifdef nnnn}begin end;{$endif}

function PyHelper_ToolbarFromId(const Str: string): TATFlatToolbar;
var
  N: PtrInt;
  Obj: TObject;
begin
  Result:= nil;

  if Str='top' then
    exit(fmMain.ToolbarMain);

  N:= StrToInt64Def(Str, 0);
  if N<>0 then
  begin
    Obj:= TObject(N);
    if Obj is TATFlatToolbar then
      exit(TATFlatToolbar(Obj))
    else
    begin
      MsgLogConsole('ERROR: toolbar_proc() gets bad handle of type '+Obj.ClassName);
      exit;
    end;
  end;
end;

type
  { TAppToolbarHelper }

  TAppToolbarHelper = class
  public
    class procedure Clear(AToolbar: TATFlatToolbar);
    class procedure DeleteButton(AToolbar: TATFlatToolbar; AButtonIndex: integer);
    class function GetChecked(AToolbar: TATFlatToolbar; AButtonIndex: integer): boolean;
    class function AddMenu(
      AToolbar: TATFlatToolbar;
      const ACaption: string;
      const ACommand: string;
      const AHint: string;
      AButtonIndex: integer;
      AImageIndex: integer): TATButton;
    class function AddButton(
      AToolbar: TATFlatToolbar;
      const ACaption: string;
      const ACommand: string;
      const AHint: string;
      AButtonIndex: integer;
      AImageIndex: integer): TATButton;
  end;

class procedure TAppToolbarHelper.Clear(AToolbar: TATFlatToolbar);
var
  i: integer;
begin
  for i:= AToolbar.ButtonCount-1 downto 0 do
    AToolbar.Buttons[i].Free;
  AToolbar.UpdateControls;
end;

class procedure TAppToolbarHelper.DeleteButton(AToolbar: TATFlatToolbar; AButtonIndex: integer);
begin
  if (AButtonIndex>=0) and (AButtonIndex<AToolbar.ButtonCount) then
    AToolbar.Buttons[AButtonIndex].Free;
  AToolbar.UpdateControls;
end;

class function TAppToolbarHelper.GetChecked(AToolbar: TATFlatToolbar; AButtonIndex: integer): boolean;
begin
  if (AButtonIndex>=0) and (AButtonIndex<AToolbar.ButtonCount) then
    Result:= AToolbar.Buttons[AButtonIndex].Checked
  else
    Result:= false;
end;


class function TAppToolbarHelper.AddMenu(
  AToolbar: TATFlatToolbar;
  const ACaption: string;
  const ACommand: string;
  const AHint: string;
  AButtonIndex: integer;
  AImageIndex: integer): TATButton;
var
  mi: TMenuItem;
  mnu: TPopupMenu;
begin
  mi:= TMenuItem.Create(fmMain);
  mi.Caption:= '('+ACommand+')';
  mnu:= TPopupMenu.Create(fmMain);
  mnu.Items.Add(mi);
  Result:= AToolbar.AddDropdown(AImageIndex, mnu, nil, ACaption, AHint, ACommand);
end;


class function TAppToolbarHelper.AddButton(
  AToolbar: TATFlatToolbar;
  const ACaption: string;
  const ACommand: string;
  const AHint: string;
  AButtonIndex: integer;
  AImageIndex: integer): TATButton;
begin
  if ACaption='-' then
  begin
    Result:= AToolbar.AddSep;
  end
  else
  if SBeginsWith(ACommand, 'toolmenu:') then
  begin
    Result:= AddMenu(AToolbar, ACaption, ACommand, AHint, AButtonIndex, AImageIndex);
  end
  else
  begin
    Result:= AToolbar.AddButton(
      AImageIndex,
      @fmMain.DoToolbarClick,
      nil,
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

  //AToolbar.UpdateControls;
  //API must call TOOLBAR_UPDATE
end;


