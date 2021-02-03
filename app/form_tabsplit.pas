(*
This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at http://mozilla.org/MPL/2.0/.

Copyright (c) Alexey Torgashin
*)
unit form_tabsplit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Spin;

type
  { TfmTabSplit }

  TfmTabSplit = class(TForm)
    btnClose: TButton;
    LabelPercent: TLabel;
    btnNoSplit: TRadioButton;
    btnHorz: TRadioButton;
    btnVert: TRadioButton;
    edPercent: TSpinEdit;
    procedure btnCloseClick(Sender: TObject);
    procedure btnHorzChange(Sender: TObject);
    procedure btnNoSplitChange(Sender: TObject);
    procedure btnVertChange(Sender: TObject);
    procedure edPercentChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
  private
    FOnChanged: TNotifyEvent;
    procedure DoChanged;
  public
    Splitted: boolean;
    SplitHorz: boolean;
    SplitPercent: integer;
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
  end;

implementation

{$R *.lfm}

{ TfmTabSplit }

procedure TfmTabSplit.btnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TfmTabSplit.btnNoSplitChange(Sender: TObject);
begin
  edPercent.Enabled:= false;
  Splitted:= false;
  DoChanged;
end;

procedure TfmTabSplit.btnHorzChange(Sender: TObject);
begin
  edPercent.Enabled:= true;
  Splitted:= true;
  SplitHorz:= true;
  DoChanged;
end;

procedure TfmTabSplit.btnVertChange(Sender: TObject);
begin
  edPercent.Enabled:= true;
  Splitted:= true;
  SplitHorz:= false;
  DoChanged;
end;

procedure TfmTabSplit.edPercentChange(Sender: TObject);
begin
  SplitPercent:= edPercent.Value;
  DoChanged;
end;

procedure TfmTabSplit.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction:= caFree;
end;

procedure TfmTabSplit.FormShow(Sender: TObject);
begin
  if Application.MainForm.FormStyle=fsStayOnTop then
    FormStyle:= fsStayOnTop;
end;

procedure TfmTabSplit.DoChanged;
begin
  if Assigned(FOnChanged) then
    FOnChanged(Self);
end;

end.

