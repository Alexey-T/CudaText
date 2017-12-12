unit formconfirmbinary;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  IniFiles,
  proc_msg,
  proc_globdata;

type
  { TfmConfirmBinary }

  TfmConfirmBinary = class(TForm)
    btnEdit: TButton;
    btnHex: TButton;
    btnCancel: TButton;
    LabelText: TLabel;
    LabelFN: TLabel;
    procedure btnCancelClick(Sender: TObject);
    procedure btnEditClick(Sender: TObject);
    procedure btnHexClick(Sender: TObject);
  private
  public
  end;

type
  TAppConfirmBinary = (
    ConfirmBinaryCancel,
    ConfirmBinaryEdit,
    ConfirmBinaryViewHex
    );

function DoDialogConfirmBinaryFile(const AFilename: string; ATooBig: boolean): TAppConfirmBinary;

implementation

{$R *.lfm}

var
  MsgFileNotText: string = 'File is maybe not text:';
  MsgFileTooBig: string = 'File is too big to edit:';

procedure DoLocalize_FormConfirmBinary(F: TfmConfirmBinary);
const
  section = 'd_cfm_op';
var
  ini: TIniFile;
  fn: string;
begin
  fn:= GetAppLangFilename;
  if not FileExists(fn) then exit;
  ini:= TIniFile.Create(fn);
  try
    //with F do Caption:= ini.ReadString(section, '_', Caption);
    with F.btnEdit do Caption:= ini.ReadString(section, 'edit', Caption);
    with F.btnHex do Caption:= ini.ReadString(section, 'hex', Caption);
    with F.btnCancel do Caption:= msgButtonCancel;
    with F do MsgFileNotText:= ini.ReadString(section, 'ntxt', MsgFileNotText);
    with F do MsgFileTooBig:= ini.ReadString(section, 'big', MsgFileTooBig);
  finally
    FreeAndNil(ini);
  end;
end;

function DoDialogConfirmBinaryFile(const AFilename: string; ATooBig: boolean): TAppConfirmBinary;
var
  F: TfmConfirmBinary;
  S: string;
begin
  F:= TfmConfirmBinary.Create(nil);
  try
    DoLocalize_FormConfirmBinary(F);
    if ATooBig then
      S:= MsgFileTooBig
    else
      S:= MsgFileNotText;

    F.LabelText.Caption:= S;
    F.LabelFN.Caption:= ExtractFileName(AFilename);
    F.btnEdit.Enabled:= not ATooBig;

    case F.ShowModal of
      mrYes: Result:= ConfirmBinaryEdit;
      mrNo: Result:= ConfirmBinaryViewHex;
      else Result:= ConfirmBinaryCancel;
    end;
  finally
    F.Free;
  end;
end;

{ TfmConfirmBinary }

procedure TfmConfirmBinary.btnCancelClick(Sender: TObject);
begin
  ModalResult:= mrCancel;
end;

procedure TfmConfirmBinary.btnEditClick(Sender: TObject);
begin
  ModalResult:= mrYes;
end;

procedure TfmConfirmBinary.btnHexClick(Sender: TObject);
begin
  ModalResult:= mrNo;
end;

end.

