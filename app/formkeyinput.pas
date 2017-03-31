unit formkeyinput;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics,
  ButtonPanel, ExtCtrls, Menus, IniFiles,
  LCLProc, LCLType,
  proc_globdata,
  proc_msg;

type
  { TfmKeyInput }

  TfmKeyInput = class(TForm)
    ButtonPanel1: TButtonPanel;
    PanelPress: TPanel;
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    { private declarations }
  public
    { public declarations }
    FHotkey: string;
  end;

function DoDialogHotkeyInput(ATitle: string): string;


implementation

procedure DoLocalize_FormKeyInput(F: TfmKeyInput);
const
  section = 'd_keys';
var
  ini: TIniFile;
  fn: string;
begin
  fn:= GetAppLangFilename;
  if not FileExists(fn) then exit;
  ini:= TIniFile.Create(fn);
  try
    with F do Caption:= ini.ReadString(section, '_', Caption);
    with F.ButtonPanel1.OKButton do Caption:= msgButtonOk;
    with F.ButtonPanel1.CancelButton do Caption:= msgButtonCancel;
    with F.PanelPress do Caption:= ini.ReadString(section, 'wait', Caption);
  finally
    FreeAndNil(ini);
  end;
end;


{$R *.lfm}

function DoDialogHotkeyInput(ATitle: string): string;
var
  Form: TfmKeyInput;
begin
  Result:= '';
  Form:= TfmKeyInput.Create(nil);
  try
    DoLocalize_FormKeyInput(Form);
    if ATitle<>'' then
      Form.Caption:= ATitle;
    if Form.ShowModal=mrOk then
      Result:= Form.FHotkey;
  finally
    FreeAndNil(Form);
  end;
end;

{ TfmKeyInput }

procedure TfmKeyInput.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  //dont allow to enter system keys: Alt/Ctrl/Shift/Win
  if (Key=VK_MENU) or
     (Key=VK_LMENU) or
     (Key=VK_RMENU) or
     (Key=VK_CONTROL) or
     (Key=VK_LCONTROL) or
     (Key=VK_RCONTROL) or
     (Key=VK_SHIFT) or
     (Key=VK_LSHIFT) or
     (Key=VK_RSHIFT) or
     (Key=VK_LWIN) or
     (Key=VK_RWIN) then
  begin
    Key:= 0;
    exit
  end;

  if (Key=VK_ESCAPE) then
  begin
    Key:= 0;
    ModalResult:= mrCancel;
    exit
  end;

  FHotkey:= ShortCutToText(ShortCut(Key, Shift));
  ModalResult:= mrOk;
  Key:= 0;
end;

end.

