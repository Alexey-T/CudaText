(*
This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at http://mozilla.org/MPL/2.0/.

Copyright (c) Alexey Torgashin
*)
unit formsavetabs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  CheckLst, ExtCtrls, StdCtrls;

type
  { TfmSaveTabs }

  TfmSaveTabs = class(TForm)
    btnSave: TButton;
    btnDontSave: TButton;
    btnCancel: TButton;
    List: TCheckListBox;
    Panel1: TPanel;
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  fmSaveTabs: TfmSaveTabs;

implementation

{$R *.lfm}

{ TfmSaveTabs }

procedure TfmSaveTabs.FormShow(Sender: TObject);
begin
  with List do
    if Items.Count>0 then
      ItemIndex:= 0;
end;

end.

