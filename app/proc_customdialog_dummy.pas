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
  ComCtrls, LclType,
  ATSynEdit,
  proc_globdata;

type
  TAppPyEventCallback = function(AEd: TATSynEdit; AEvent: TAppPyEvent;
      const AParams: array of string): string of object;

var
  CustomDialog_DoPyEvent: TAppPyEventCallback = nil;

type

  { TFormDummy }

  TFormDummy = class(TForm)
  private
    FormShown: boolean;
    procedure DoOnShow(Sender: TObject);
    procedure DoOnKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  public
    constructor Create(TheOwner: TComponent); override;
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

constructor TFormDummy.Create(TheOwner: TComponent);
begin
  inherited CreateNew(TheOwner);

  BorderStyle:= bsDialog;
  KeyPreview:= true;
  Position:= poScreenCenter;
  ShowHint:= true;
  Scaled:= true;

  OnShow:= @DoOnShow;
  OnKeyDown:= @DoOnKeyDown;
end;

procedure TFormDummy.DoOnShow(Sender: TObject);
var
  C: TControl;
  i: integer;
begin
  FormShown:= true;
  for i:= 0 to ControlCount-1 do
  begin
    C:= Controls[i];
    if C is TListview then
      with (C as TListview) do
        if ItemFocused<>nil then
          ItemFocused.MakeVisible(false);
  end;
end;

procedure TFormDummy.DoOnKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key=VK_ESCAPE) then
  begin
    ModalResult:= mrCancel;
    Key:= 0;
    exit;
  end;
end;

procedure TFormDummy.DoOnChange(Sender: TObject);
var
  i: integer;
begin
  //workarnd for bug on Mac (flicker on More>> press in BackupFile dialog)
  if not FormShown then exit;

  //Tag=Dummy_TagActive means that control change closes form
  if (Sender as TControl).Tag=Dummy_TagActive then
    for i:= 0 to ControlCount-1 do
      if Controls[i]=Sender then
      begin
        ModalResult:= Dummy_ResultStart+i;

        if not (fsModal in FFormState) then
        begin
          if Assigned(CustomDialog_DoPyEvent) then
            CustomDialog_DoPyEvent(nil, cEventOnDlg,
              [
                IntToStr(PtrInt(Self)), //id_dlg
                IntToStr(i), //id_ctl
                '"on_change"' //id_event
              ]);
        end;

        exit
      end;
end;

procedure TFormDummy.DoOnSelChange(Sender: TObject; User: boolean);
begin
  DoOnChange(Sender);
end;

procedure TFormDummy.DoOnListviewChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
begin
  DoOnChange(Sender);
end;

procedure TFormDummy.DoOnListviewSelect(Sender: TObject; Item: TListItem;
  Selected: Boolean);
begin
  DoOnChange(Sender);
end;

end.

