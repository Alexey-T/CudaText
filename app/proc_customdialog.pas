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

function DoGetFormResult(AForm: TForm): string;
var
  C: TControl;
  i: integer;
  Str: string;
begin
  Result:= '';
  for i:= 0 to AForm.ControlCount-1 do
  begin
    Str:= '';

    C:= AForm.Controls[i];
    if C is TEdit then Str:= (C as TEdit).Text;
    if C is TCheckBox then Str:= IntToStr(Ord((C as TCheckBox).Checked));
    if C is TComboBox then Str:= IntToStr((C as TComboBox).ItemIndex);
    if C is TListBox then Str:= IntToStr((C as TListBox).ItemIndex);
    if C is TMemo then
    begin
      Str:= (C as TMemo).Lines.Text;
      Str:= StringReplace(Str, #10, #9, [rfReplaceAll]);
    end;

    if Result<>'' then Result:= Result+#10;
    Result:= Result+Str;
  end;
end;

procedure DoAddControl(AForm: TForm; ATextItems: string; AControlIndex: integer);
var
  SNameValue, SName, SValue, SListItem: string;
  NX1, NX2, NY1, NY2: integer;
  Ctl: TControl;
  UseAutosize: boolean;
begin
  Ctl:= nil;
  UseAutosize:= false;

  repeat
    SNameValue:= SGetItem(ATextItems, Chr(1));
    if SNameValue='' then break;
    SName:= SGetItem(SNameValue, '=');
    SValue:= SNameValue;
    if SName='' then Continue;

    //-------type
    if SName='type' then
    begin
      if SValue='check' then begin Ctl:= TCheckBox.Create(AForm); end;
      if SValue='edit' then begin Ctl:= TEdit.Create(AForm); end;
      if SValue='listbox' then begin Ctl:= TListBox.Create(AForm); end;
      if SValue='memo' then begin Ctl:= TMemo.Create(AForm); end;
      if SValue='label' then
        begin
          UseAutosize:= true;
          Ctl:= TLabel.Create(AForm);
        end;
      if SValue='combo' then
        begin
          UseAutosize:= true;
          Ctl:= TComboBox.Create(AForm);
          (Ctl as TComboBox).ReadOnly:= true;
        end;
      if SValue='button' then
        begin
          UseAutosize:= true;
          Ctl:= TButton.Create(AForm);
          (Ctl as TButton).ModalResult:= cButtonResultStart+AControlIndex;
        end;

      if Assigned(Ctl) then
        Ctl.Parent:= AForm;
      Continue;
    end;

    //first name must be "type"
    if not Assigned(Ctl) then exit;

    //-------cap
    if SName='cap' then
    begin
      Ctl.Caption:= SValue;
      Ctl.AutoSize:= true;
      Ctl.AutoSize:= false;
      Continue;
    end;

    //-------pos
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

    //-------items
    if SName='items' then
    begin
      repeat
        SListItem:= SGetItem(SValue, #9);
        if SListItem='' then break;
        if Ctl is TListbox then (Ctl as TListbox).Items.Add(SListItem);
        if Ctl is TComboBox then (Ctl as TComboBox).Items.Add(SListItem);
        if Ctl is TMemo then (Ctl as TMemo).Lines.Add(SListItem);
      until false;
      Continue;
    end;

    //-------val
    if SName='val' then
    begin
      if Ctl is TCheckBox then (Ctl as TCheckBox).Checked:= (SValue<>'0');
      if Ctl is TEdit then (Ctl as TEdit).Text:= SValue;
      if Ctl is TComboBox then (Ctl as TCombobox).ItemIndex:= StrToIntDef(SValue, 0);
      if Ctl is TListBox then (Ctl as TListBox).ItemIndex:= StrToIntDef(SValue, 0);
      Continue;
    end;

    //-------more?
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
  try
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
      if F.Controls[AFocusedIndex] is TWinControl then
        F.ActiveControl:= F.Controls[AFocusedIndex] as TWinControl;

    Res:= F.ShowModal;
    if Res>=cButtonResultStart then
    begin
      AButtonIndex:= Res-cButtonResultStart;
      AStateText:= DoGetFormResult(F);
    end;
  finally
    FreeAndNil(F);
  end;
end;

end.

