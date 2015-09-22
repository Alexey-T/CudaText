(*
This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at http://mozilla.org/MPL/2.0/.

Copyright (c) Alexey Torgashin
*)
unit formoutput;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TfmOutput }

  TfmOutput = class(TForm)
    ListBox1: TListBox;
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  fmOutput: TfmOutput;

implementation

{$R *.lfm}

{ TfmOutput }

procedure TfmOutput.FormShow(Sender: TObject);
begin
  ListBox1.SetFocus;
end;

end.

