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
    btnViewBinary: TButton;
    btnCancel: TButton;
    btnViewHex: TButton;
    btnViewText: TButton;
    btnViewUnicode: TButton;
    LabelText: TLabel;
    LabelFN: TLabel;
    procedure btnCancelClick(Sender: TObject);
    procedure btnEditClick(Sender: TObject);
    procedure btnViewBinaryClick(Sender: TObject);
    procedure btnViewHexClick(Sender: TObject);
    procedure btnViewTextClick(Sender: TObject);
    procedure btnViewUnicodeClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
  public
  end;

type
  TAppConfirmBinary = (
    ConfirmBinaryCancel,
    ConfirmBinaryEditor,
    ConfirmBinaryViewText,
    ConfirmBinaryViewBinary,
    ConfirmBinaryViewHex,
    ConfirmBinaryViewUnicode
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
    with F.btnViewText do Caption:= ini.ReadString(section, 'text', Caption);
    with F.btnViewBinary do Caption:= ini.ReadString(section, 'bin', Caption);
    with F.btnViewHex do Caption:= ini.ReadString(section, 'hex', Caption);
    with F.btnViewUnicode do Caption:= ini.ReadString(section, 'uni', Caption);
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
      mrOk: Result:= ConfirmBinaryEditor;
      200: Result:= ConfirmBinaryViewText;
      201: Result:= ConfirmBinaryViewBinary;
      202: Result:= ConfirmBinaryViewHex;
      203: Result:= ConfirmBinaryViewUnicode;
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
  ModalResult:= mrOk;
end;

procedure TfmConfirmBinary.btnViewTextClick(Sender: TObject);
begin
  ModalResult:= 200;
end;

procedure TfmConfirmBinary.btnViewBinaryClick(Sender: TObject);
begin
  ModalResult:= 201;
end;

procedure TfmConfirmBinary.btnViewHexClick(Sender: TObject);
begin
  ModalResult:= 202;
end;

procedure TfmConfirmBinary.btnViewUnicodeClick(Sender: TObject);
begin
  ModalResult:= 203;
end;

procedure TfmConfirmBinary.FormShow(Sender: TObject);
begin
  ClientHeight:= btnCancel.Top+btnCancel.Height+10;
end;

end.

