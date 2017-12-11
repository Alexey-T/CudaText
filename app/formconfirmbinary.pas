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

function DoDialogConfirmBinaryFile(const fn: string): TAppConfirmBinary;

implementation

function DoDialogConfirmBinaryFile(const fn: string): TAppConfirmBinary;
var
  F: TfmConfirmBinary;
begin
  F:= TfmConfirmBinary.Create(nil);
  try
    F.Label1.Caption:= 'File is maybe not text:'#10+ExtractFileName(fn);
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

