(*
This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at http://mozilla.org/MPL/2.0/.

Copyright (c) Alexey Torgashin
*)
unit formcolorsetup;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ButtonPanel,
  ColorBox, StdCtrls, proc_colors;

type
  { TfmColorSetup }

  TfmColorSetup = class(TForm)
    bChange: TButton;
    bNone: TButton;
    ButtonPanel1: TButtonPanel;
    ColorDialog1: TColorDialog;
    List: TColorListBox;
    procedure bChangeClick(Sender: TObject);
    procedure bNoneClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
    Data: TAppTheme;
    procedure Updatelist;
  public
    { public declarations }
  end;

var
  fmColorSetup: TfmColorSetup;

function DoDialogConfColors(var AColors: TAppTheme): boolean;

implementation

{$R *.lfm}

function DoDialogConfColors(var AColors: TAppTheme): boolean;
begin
  with TfmColorSetup.Create(nil) do
  try
    Data:= AColors;
    Result:= ShowModal=mrOk;
    if result then AColors:= Data;
  finally
    Free
  end;
end;

{ TfmColorSetup }

procedure TfmColorSetup.Updatelist;
var
  i, n: integer;
begin
  n:= list.itemindex;

  List.Items.Clear;
  for i:= Low(Data) to High(Data) do
    List.Items.AddObject(Data[i].desc, TObject(ptrint(Data[i].color)));

  if n<list.items.count then
    list.itemindex:= n;
  list.Invalidate;
end;

procedure TfmColorSetup.bChangeClick(Sender: TObject);
begin
  ColorDialog1.Color:= ptrint(List.Items.Objects[List.itemindex]);
  if ColorDialog1.Execute then
  begin
    Data[List.Itemindex].color:= ColorDialog1.Color;
    Updatelist;
  end;
end;

procedure TfmColorSetup.bNoneClick(Sender: TObject);
begin
  Data[List.Itemindex].color:= clNone;
  Updatelist;
end;

procedure TfmColorSetup.FormShow(Sender: TObject);
begin
  Updatelist;
  List.ItemIndex:= 0;
  List.SetFocus;
end;

end.

