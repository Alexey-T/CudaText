unit formconfirmbinary;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type
  { TfmConfirmBinary }

  TfmConfirmBinary = class(TForm)
    btnEdit: TButton;
    btnHex: TButton;
    btnCancel: TButton;
    Label1: TLabel;
    procedure btnCancelClick(Sender: TObject);
    procedure btnEditClick(Sender: TObject);
    procedure btnHexClick(Sender: TObject);
  private

  public

  end;

type
  TAppConfirmBinary = (
    binConfirmCancel,
    binConfirmEdit,
    binConfirmViewHex
    );

function DoDialogConfirmBinaryFile(const AFilename: string; ATooBig: boolean): TAppConfirmBinary;

implementation

function DoDialogConfirmBinaryFile(const AFilename: string; ATooBig: boolean): TAppConfirmBinary;
var
  F: TfmConfirmBinary;
  S: string;
begin
  F:= TfmConfirmBinary.Create(nil);
  try
    if ATooBig then
      S:= 'File is too big to edit:'
    else
      S:= 'File is not text:';
    F.Label1.Caption:= S+#10+ExtractFileName(AFilename);
    F.btnEdit.Enabled:= not ATooBig;

    case F.ShowModal of
      mrYes: Result:= binConfirmEdit;
      mrNo: Result:= binConfirmViewHex;
      else Result:= binConfirmCancel;
    end;
  finally
    F.Free;
  end;
end;

{$R *.lfm}

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

