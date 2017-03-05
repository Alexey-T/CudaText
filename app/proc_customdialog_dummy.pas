(*
This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at http://mozilla.org/MPL/2.0/.

Copyright (c) Alexey Torgashin
*)
unit proc_customdialog_dummy;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, StdCtrls, Forms,
  ComCtrls, LclType;

type
  { TDummyClass }
  TDummyClass = class
  public
    Form: TForm;
    OnChangeActive: boolean;
    procedure DoOnShow(Sender: TObject);
    procedure DoOnKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure DoOnChange(Sender: TObject);
    procedure DoOnSelChange(Sender: TObject; User: boolean);
    procedure DoOnListviewChange(Sender: TObject; Item: TListItem; Change: TItemChange);
    procedure DoOnListviewSelect(Sender: TObject; Item: TListItem; Selected: Boolean);
  end;

const
  Dummy_ResultStart = 100;
  Dummy_TagActive = -1;


implementation

{ TDummyClass }

procedure TDummyClass.DoOnShow(Sender: TObject);
var
  C: TControl;
  i: integer;
begin
  OnChangeActive:= true;
  for i:= 0 to Form.ControlCount-1 do
  begin
    C:= Form.Controls[i];
    if C is TListview then
      with (C as TListview) do
        if ItemFocused<>nil then
          ItemFocused.MakeVisible(false);
  end;
end;

procedure TDummyClass.DoOnKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key=VK_ESCAPE) then
  begin
    if Assigned(Form) then
      Form.ModalResult:= mrCancel;
    Key:= 0;
    exit;
  end;
end;

procedure TDummyClass.DoOnChange(Sender: TObject);
var
  i: integer;
begin
  //workarnd for bug on MacOS
  if not OnChangeActive then exit;

  //Tag=Dummy_TagActive means that control change closes form
  if (Sender as TControl).Tag=Dummy_TagActive then
    for i:= 0 to Form.ControlCount-1 do
      if Form.Controls[i]=Sender then
      begin
        Form.ModalResult:= Dummy_ResultStart+i;
        exit
      end;
end;

procedure TDummyClass.DoOnSelChange(Sender: TObject; User: boolean);
begin
  DoOnChange(Sender);
end;

procedure TDummyClass.DoOnListviewChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
begin
  DoOnChange(Sender);
end;

procedure TDummyClass.DoOnListviewSelect(Sender: TObject; Item: TListItem;
  Selected: Boolean);
begin
  DoOnChange(Sender);
end;

end.

