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
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ButtonPanel,
  CheckLst;

type
  { TfmSaveTabs }

  TfmSaveTabs = class(TForm)
    ButtonPanel1: TButtonPanel;
    List: TCheckListBox;
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
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

procedure TfmSaveTabs.FormDestroy(Sender: TObject);
begin
end;

procedure TfmSaveTabs.FormShow(Sender: TObject);
begin
  with List do
    if Items.Count>0 then
      ItemIndex:= 0;
end;

procedure TfmSaveTabs.HelpButtonClick(Sender: TObject);
begin
  ModalResult:= mrCancel;
end;

end.

