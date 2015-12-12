unit proc_customdialog;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Controls, StdCtrls, ExtCtrls, Forms,
  Dialogs,
  ATStringProc;

procedure DoDialogCustom(const ATitle: string; ASizeX, ASizeY: integer;
  AText: string; AFocusedIndex: integer; out AButtonIndex: integer; out AStateText: string);


implementation

const
  cButtonResultStart=100;

procedure DoAddControl(AForm: TForm; AItems: string; AControlIndex: integer);
var
  SNameValue, SName, SValue, SListItem: string;
  NX1, NX2, NY1, NY2: integer;
  Ctl: TControl;
  UseAutosize: boolean;
begin
  Ctl:= nil;
  UseAutosize:= false;

  repeat
    SNameValue:= SGetItem(AItems, Chr(1));
    if SNameValue='' then break;
    SName:= SGetItem(SNameValue, '=');
    SValue:= SNameValue;
    if SName='' then continue;

    if SName='type' then
    begin
      if SValue='label' then begin Ctl:= TLabel.Create(AForm); end;
      if SValue='check' then begin Ctl:= TCheckBox.Create(AForm); end;
      if SValue='edit' then begin Ctl:= TEdit.Create(AForm); end;
      if SValue='listbox' then begin Ctl:= TListBox.Create(AForm); end;
      if SValue='combo' then
        begin
          Ctl:= TComboBox.Create(AForm);
          (Ctl as TComboBox).ReadOnly:= true;
        end;
      if SValue='button' then
        begin
          Ctl:= TButton.Create(AForm);
          UseAutosize:= true;
          (Ctl as TButton).ModalResult:= cButtonResultStart+AControlIndex;
        end;
      if Assigned(Ctl) then
        Ctl.Parent:= AForm;
      Continue;
    end;

    //first name must be "type"
    if not Assigned(Ctl) then exit;

    if SName='cap' then
    begin
      Ctl.Caption:= SValue;
      Ctl.AutoSize:= true;
      Ctl.AutoSize:= false;
      Continue;
    end;

    if SName='pos' then
    begin
      NX1:= StrToIntDef(SGetItem(SValue, ','), -1);
      NY1:= StrToIntDef(SGetItem(SValue, ','), -1);
      NX2:= StrToIntDef(SGetItem(SValue, ','), -1);
      NY2:= StrToIntDef(SGetItem(SValue, ','), -1);
      if NX1<0 then Continue;
      if NX2<0 then Continue;
      if NY1<0 then Continue;
      if NY2<0 then Continue;
      Ctl.Left:= NX1;
      Ctl.Width:= NX2-NX1;
      Ctl.Top:= NY1;
      if not UseAutosize then
        Ctl.Height:= NY2-NY1;
      Continue;
    end;

    if SName='items' then
    begin
      repeat
        SListItem:= SGetItem(SValue, #9);
        if SListItem='' then break;
        if Ctl is TListbox then
          (Ctl as TListbox).Items.Add(SListItem);
        if Ctl is TComboBox then
          (Ctl as TComboBox).Items.Add(SListItem);
      until false;
      Continue;
    end;

    if SName='val' then
    begin
      if Ctl is TCheckBox then (Ctl as TCheckBox).Checked:= (SValue<>'0');
      if Ctl is TEdit then (Ctl as TEdit).Text:= SValue;
      if Ctl is TComboBox then (Ctl as TCombobox).ItemIndex:= StrToIntDef(SValue, 0);
      if Ctl is TListBox then (Ctl as TListBox).ItemIndex:= StrToIntDef(SValue, 0);
      Continue;
    end;

  until false;
end;

procedure DoDialogCustom(const ATitle: string; ASizeX, ASizeY: integer;
  AText: string; AFocusedIndex: integer; out AButtonIndex: integer; out AStateText: string);
var
  F: TForm;
  Res, NIndex: integer;
  SItem: string;
begin
  AButtonIndex:= -1;
  AStateText:= '';

  F:= TForm.Create(nil);
  F.BorderStyle:= bsDialog;
  F.Position:= poDesktopCenter;
  F.Width:= ASizeX;
  F.Height:= ASizeY;
  F.Caption:= ATitle;

  NIndex:= 0;
  repeat
    SItem:= SGetItem(AText, #10);
    if SItem='' then break;
    DoAddControl(F, SItem, NIndex);
    Inc(NIndex);
  until false;

  if (AFocusedIndex>=0) and (AFocusedIndex<F.ControlCount) then
    F.ActiveControl:= F.Controls[AFocusedIndex] as TWinControl;

  Res:= F.ShowModal;
  if Res>=cButtonResultStart then
  begin
    AButtonIndex:= Res-cButtonResultStart;
  end;
end;

end.

