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
  private

  public

  end;

var
  fmConfirmBinary: TfmConfirmBinary;

implementation

{$R *.lfm}

end.

