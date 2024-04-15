unit form_rename_file;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ButtonPanel, StdCtrls,
  StrUtils;

type

  { TfmRenameFile }

  TfmRenameFile = class(TForm)
    ButtonPanel1: TButtonPanel;
    EditExt: TEdit;
    EditName: TEdit;
    LabelPrompt: TLabel;
    procedure EditNameChange(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormShow(Sender: TObject);
  private
    OldPath: string;
  public
    OldFileName: string;
    function NewFileName: string;

  end;

function DoDialogRenameFile(const AOldFileName: string; out ANewFileName: string): boolean;

implementation

{$R *.lfm}

function DoDialogRenameFile(const AOldFileName: string; out ANewFileName: string): boolean;
var
  Dlg: TfmRenameFile;
begin
  Result:= false;
  ANewFileName:= '';

  Dlg:= TfmRenameFile.Create(nil);
  try
    Dlg.OldFileName:= AOldFileName;
    if Dlg.ShowModal=mrOk then
    begin
      ANewFileName:= Dlg.NewFileName;
      Result:= true;
    end;
  finally
    FreeAndNil(Dlg);
  end;
end;

{ TfmRenameFile }

procedure TfmRenameFile.FormShow(Sender: TObject);
begin
  EditName.Text:= ChangeFileExt(ExtractFileName(OldFileName), '');
  EditExt.Text:= Copy(ExtractFileExt(OldFileName), 2, MaxInt);

  OldPath:= ExtractFilePath(OldFileName);

  if Assigned(EditName.OnChange) then
    EditName.OnChange(nil);
end;

function TfmRenameFile.NewFileName: string;
begin
  Result:=
    OldPath+
    EditName.Text+
    IfThen(EditExt.Text<>'', '.'+EditExt.Text);
end;

procedure TfmRenameFile.EditNameChange(Sender: TObject);
begin
  ButtonPanel1.OKButton.Enabled:=
    (EditName.Text<>'') and
    (Pos(DirectorySeparator, EditName.Text)=0) and
    (Pos(DirectorySeparator, EditExt.Text)=0) and
    not SameFileName(NewFileName, OldFileName);
end;

procedure TfmRenameFile.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if ModalResult=mrOk then
    if not RenameFile(OldFileName, NewFileName) then
    begin
      MessageDlg('CudaText', 'Cannot rename file to:'+#10+NewFileName, mtError, [mbOK], 0);
      CanClose:= false;
    end;
end;

end.

