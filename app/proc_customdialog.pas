unit proc_customdialog;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Controls, StdCtrls, ExtCtrls, Forms,
  CheckLst,
  ATStringProc;

procedure DoDialogCustom(const ATitle: string; ASizeX, ASizeY: integer;
  AText: string; AFocusedIndex: integer; out AButtonIndex: integer; out AStateText: string);


implementation

const
  cButtonResultStart=100;

function StrToBool(const S: string): boolean;
begin
  Result:= S<>'0';
end;

function DoGetFormResult(AForm: TForm): string;
var
  Ctl: TControl;
  Str: string;
  i, k: integer;
begin
  Result:= '';
  for i:= 0 to AForm.ControlCount-1 do
  begin
    Str:= '';
    Ctl:= AForm.Controls[i];

    if Ctl is TEdit then
      Str:= (Ctl as TEdit).Text;
    if Ctl is TCheckBox then
      Str:= IntToStr(Ord((Ctl as TCheckBox).Checked));
    if Ctl is TComboBox then
      Str:= IntToStr((Ctl as TComboBox).ItemIndex);
    if Ctl is TListBox then
      Str:= IntToStr((Ctl as TListBox).ItemIndex);

    if Ctl is TMemo then
    begin
      Str:= (Ctl as TMemo).Lines.Text;
      Str:= StringReplace(Str, #13#10, #9, [rfReplaceAll]);
      Str:= StringReplace(Str, #13, #9, [rfReplaceAll]);
      Str:= StringReplace(Str, #10, #9, [rfReplaceAll]);
    end;

    if Ctl is TRadioGroup then
      Str:= IntToStr((Ctl as TRadioGroup).ItemIndex);

    if Ctl is TCheckGroup then
    begin
      Str:= '';
      for k:= 0 to (Ctl as TCheckGroup).Items.Count-1 do
        Str:= Str+IntToStr(Ord((Ctl as TCheckGroup).Checked[k]))+',';
    end;

    if Ctl is TCheckListBox then
    begin
      Str:= '';
      for k:= 0 to (Ctl as TCheckListBox).Items.Count-1 do
        Str:= Str+IntToStr(Ord((Ctl as TCheckListBox).Checked[k]))+',';
    end;

    if Result<>'' then Result:= Result+#10;
    Result:= Result+Str;
  end;
end;


procedure DoSetCheckGroupState(C: TControl; AValue: string);
var
  SItem: string;
  NCount, N: integer;
begin
  if C is TCheckGroup then NCount:= (C as TCheckGroup).Items.Count else
   if C is TCheckListBox then NCount:= (C as TCheckListBox).Items.Count else
    exit;

  N:= 0;
  repeat
    if N>=NCount then exit;
    SItem:= SGetItem(AValue);
    if SItem='' then break;

    if C is TCheckGroup then (C as TCheckGroup).Checked[N]:= StrToBool(SItem);
    if C is TCheckListBox then (C as TCheckListBox).Checked[N]:= StrToBool(SItem);

    Inc(N);
  until false;
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
      if SValue='memo' then
        begin
          Ctl:= TMemo.Create(AForm);
          (Ctl as TMemo).WordWrap:= false;
          (Ctl as TMemo).ScrollBars:= ssBoth;
        end;
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
      if SValue='radiogroup' then
        Ctl:= TRadioGroup.Create(AForm);
      if SValue='checkgroup' then
        Ctl:= TCheckGroup.Create(AForm);
      if SValue='checklistbox' then
        Ctl:= TCheckListBox.Create(AForm);

      if Assigned(Ctl) then
        Ctl.Parent:= AForm;
      Continue;
    end;

    //first name must be "type"
    if not Assigned(Ctl) then exit;

    //-------en
    if SName='en' then
    begin
      Ctl.Enabled:= StrToBool(SValue);
      Continue;
    end;

    //-------cap
    if SName='cap' then
    begin
      Ctl.Caption:= SValue;
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
        if Ctl is TCheckGroup then (Ctl as TCheckGroup).Items.Add(SListItem);
        if Ctl is TRadioGroup then (Ctl as TRadioGroup).Items.Add(SListItem);
        if Ctl is TCheckListBox then (Ctl as TCheckListBox).Items.Add(SListItem);
      until false;
      Continue;
    end;

    //-------val
    if SName='val' then
    begin
      if Ctl is TCheckBox then (Ctl as TCheckBox).Checked:= StrToBool(SValue);
      if Ctl is TEdit then (Ctl as TEdit).Text:= SValue;
      if Ctl is TComboBox then (Ctl as TCombobox).ItemIndex:= StrToIntDef(SValue, 0);
      if Ctl is TListBox then (Ctl as TListBox).ItemIndex:= StrToIntDef(SValue, 0);
      if Ctl is TRadioGroup then (Ctl as TRadioGroup).ItemIndex:= StrToIntDef(SValue, 0);
      if Ctl is TCheckGroup then DoSetCheckGroupState(Ctl, SValue);
      if Ctl is TCheckListBox then DoSetCheckGroupState(Ctl, SValue);
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
      if F.Controls[AFocusedIndex].Enabled then
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

