(*
This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at http://mozilla.org/MPL/2.0/.

Copyright (c) Alexey Torgashin
*)
unit formchecklist;

interface

uses
  SysUtils, Classes, Graphics, Controls, Forms,
  StdCtrls, ComCtrls, ExtCtrls, ButtonPanel,
  ATStringProc,
  StrUtils;

type
  { TfmCheckList }

  TfmCheckList = class(TForm)
    ButtonPanel1: TButtonPanel;
    List: TListView;
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

function DoInputCheckList(const ACaption, AColumns, AItems: string;
  ASizeX, ASizeY: Integer): string;

implementation

{$R *.lfm}

function DoInputCheckList(const ACaption, AColumns, AItems: string;
  ASizeX, ASizeY: Integer): string;
const
  sepLine=#10;
  sepTab=#9;
var
  S, SItem, SSubItem: string;
  i: integer;
begin
  Result:= '';
  with TfmCheckList.Create(nil) do
  try
    Caption:= ACaption;
    Width:= ASizeX;
    Height:= ASizeY;

    List.Columns.Clear;
    List.Items.Clear;

    S:= AColumns;
    repeat
      SItem:= SGetItem(S, sepLine);
      if SItem='' then Break;
      with List.Columns.Add do
      begin
        Caption:= SGetItem(SItem, sepTab);
        i:= StrToIntDef(SGetItem(SItem, sepTab), 0);
        if i>0 then
          Width:= i
        else
          AutoSize:= true;
      end;
    until false;

    S:= AItems;
    repeat
      SItem:= SGetItem(S, sepLine);
      if SItem='' then Break;
      with List.Items.Add do
      begin
        SSubItem:= SGetItem(SItem, sepTab);
        Checked:= SBeginsWith(SSubItem, '*');
        if Checked then System.Delete(SSubItem, 1, 1);
        Caption:= SSubItem;
        repeat
          SSubItem:= SGetItem(SItem, sepTab);
          if SSubItem='' then Break;
          SubItems.Add(SSubItem);
        until false;
      end;
    until false;

    if ShowModal=mrOk then
    begin
      for i:= 0 to List.Items.Count-1 do
        Result:= Result+ IfThen(List.Items[i].Checked, '1', '0');
    end;
  finally
    Free
  end;
end;

{ TfmCheckList }

procedure TfmCheckList.FormShow(Sender: TObject);
begin
  List.SetFocus;
  if List.Items.Count>0 then
    List.Selected:= List.Items[0];
end;


end.
