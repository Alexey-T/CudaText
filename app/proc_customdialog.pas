(*
This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at http://mozilla.org/MPL/2.0/.

Copyright (c) Alexey Torgashin
*)
unit proc_customdialog;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Controls, StdCtrls, ExtCtrls, Forms,
  CheckLst, Spin, ComCtrls,
  LclProc, LclType,
  ATStringProc;

procedure DoDialogCustom(const ATitle: string; ASizeX, ASizeY: integer;
  AText: string; AFocusedIndex: integer; out AButtonIndex: integer; out AStateText: string);


implementation

const
  cButtonResultStart=100;

type
  TCustomEditHack = class(TCustomEdit);

type
  { TDummyClass }
  TDummyClass = class
  public
    Form: TForm;
    procedure DoKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  end;

function StrToBool(const S: string): boolean;
begin
  Result:= S<>'0';
end;

function IsControlAutosizeY(C: TControl): boolean;
begin
  Result:=
    (C is TLabel) or
    (C is TButton) or
    (C is TEdit) or
    (C is TComboBox) or
    (C is TCheckBox) or
    (C is TRadioButton) or
    (C is TSpinEdit);
end;

function DoGetListviewState(C: TListView): string; forward;
function DoGetControlState(C: TControl): string;
var
  i: integer;
begin
  Result:= '';

  if C is TEdit then
    Result:= (C as TEdit).Text;

  if C is TCheckBox then
    Result:= IntToStr(Ord((C as TCheckBox).Checked));

  if C is TRadioButton then
    Result:= IntToStr(Ord((C as TRadioButton).Checked));

  if C is TListBox then
    Result:= IntToStr((C as TListBox).ItemIndex);

  if C is TComboBox then
  begin
    if (C as TComboBox).ReadOnly then
      Result:= IntToStr((C as TComboBox).ItemIndex)
    else
      Result:= (C as TComboBox).Text;
  end;

  if C is TMemo then
  begin
    Result:= (C as TMemo).Lines.Text;
    Result:= StringReplace(Result, #9, #2, [rfReplaceAll]);
    Result:= StringReplace(Result, #13#10, #9, [rfReplaceAll]);
    Result:= StringReplace(Result, #13, #9, [rfReplaceAll]);
    Result:= StringReplace(Result, #10, #9, [rfReplaceAll]);
  end;

  if C is TRadioGroup then
    Result:= IntToStr((C as TRadioGroup).ItemIndex);

  if C is TCheckGroup then
    for i:= 0 to (C as TCheckGroup).Items.Count-1 do
      Result:= Result+IntToStr(Ord((C as TCheckGroup).Checked[i]))+',';

  if C is TCheckListBox then
  begin
    Result:= IntToStr((C as TCheckListBox).ItemIndex)+';';
    for i:= 0 to (C as TCheckListBox).Items.Count-1 do
      Result:= Result+IntToStr(Ord((C as TCheckListBox).Checked[i]))+',';
  end;

  if C is TSpinEdit then
    Result:= IntToStr((C as TSpinEdit).Value);

  if C is TListView then
    Result:= DoGetListviewState(C as TListView);
end;


function DoGetFormResult(AForm: TForm): string;
var
  Str: string;
  i: integer;
begin
  Result:= '';
  for i:= 0 to AForm.ControlCount-1 do
  begin
    Str:= DoGetControlState(AForm.Controls[i]);
    Result:= Result+Str+#10;
  end;
end;


procedure DoSetMemoState(C: TMemo; AValue: string);
var
  SItem: string;
begin
  C.Lines.Clear;
  repeat
    SItem:= SGetItem(AValue, #9);
    if SItem='' then break;
    C.Lines.Add(SItem);
  until false;
end;

procedure DoSetCheckgroupState(C: TCheckGroup; AValue: string);
var
  SItem: string;
  N: integer;
begin
  N:= 0;
  repeat
    if N>=C.Items.Count then exit;
    SItem:= SGetItem(AValue);
    if SItem='' then break;
    C.Checked[N]:= StrToBool(SItem);
    Inc(N);
  until false;
end;

procedure DoSetChecklistboxState(C: TCheckListBox; AValue: string);
var
  SItem: string;
  N: integer;
begin
  C.ItemIndex:= StrToIntDef(SGetItem(AValue, ';'), 0);

  N:= 0;
  repeat
    if N>=C.Items.Count then exit;
    SItem:= SGetItem(AValue);
    if SItem='' then break;
    C.Checked[N]:= StrToBool(SItem);
    Inc(N);
  until false;
end;


procedure DoSetListviewItem(C: TListView; SListItem: string);
var
  SItem: string;
  Col: TListColumn;
  i: integer;
begin
  if C.Columns.Count=0 then
  begin
    repeat
      SItem:= SGetItem(SListItem, #13);
      if SItem='' then break;
      Col:= C.Columns.Add;
      Col.Caption:= SGetItem(SItem, '=');
      if SItem<>'' then
      begin
        if SItem[1]='L' then begin Delete(SItem, 1, 1); Col.Alignment:= taLeftJustify; end;
        if SItem[1]='R' then begin Delete(SItem, 1, 1); Col.Alignment:= taRightJustify; end;
        if SItem[1]='C' then begin Delete(SItem, 1, 1); Col.Alignment:= taCenter; end;
        Col.Width:= StrToIntDef(SItem, 80);
      end;
    until false;
  end
  else
  begin
    SItem:= SGetItem(SListItem, #13);
    C.Items.Add.Caption:= SItem;
    for i:= 1 to C.ColumnCount do
    begin
      SItem:= SGetItem(SListItem, #13);
      C.Items[C.Items.Count-1].SubItems.Add(SItem);
    end;
  end;
end;


procedure DoSetListviewState(C: TListView; SValue: string);
// index;check0,check1,
var
  N: integer;
  SItem: string;
begin
  SItem:= SGetItem(SValue, ';');
  N:= StrToIntDef(SItem, 0);
  if (N>=0) and (N<C.Items.Count) then
    C.ItemFocused:= C.Items[N];

  if C.Checkboxes then
  begin
    N:= 0;
    repeat
      if N>=C.Items.Count then break;
      SItem:= SGetItem(SValue);
      if SItem='' then break;
      C.Items[N].Checked:= StrToBool(SItem);
      Inc(N);
    until false;
  end;
end;


function DoGetListviewState(C: TListView): string;
// index;check0,check1,
var
  i: integer;
begin
  if Assigned(C.ItemFocused) then
    Result:= IntToStr(C.ItemFocused.Index);

  if C.Checkboxes then
  begin
    Result:= Result+';';
    for i:= 0 to C.Items.Count-1 do
      Result:= Result+IntToStr(Ord(C.Items[i].Checked))+',';
  end;
end;


procedure DoAddControl(AForm: TForm; ATextItems: string);
var
  SNameValue, SName, SValue, SListItem: string;
  NX1, NX2, NY1, NY2: integer;
  Ctl, CtlPrev: TControl;
begin
  Ctl:= nil;

  repeat
    SNameValue:= SGetItem(ATextItems, Chr(1));
    if SNameValue='' then break;
    SName:= SGetItem(SNameValue, '=');
    SValue:= SNameValue;
    if SName='' then Continue;

    //-------type
    if SName='type' then
    begin
      if SValue='check' then Ctl:= TCheckBox.Create(AForm);
      if SValue='radio' then Ctl:= TRadioButton.Create(AForm);
      if SValue='edit' then Ctl:= TEdit.Create(AForm);
      if SValue='listbox' then Ctl:= TListBox.Create(AForm);
      if SValue='spinedit' then Ctl:= TSpinEdit.Create(AForm);
      if SValue='memo' then
        begin
          Ctl:= TMemo.Create(AForm);
          (Ctl as TMemo).WordWrap:= false;
          (Ctl as TMemo).ScrollBars:= ssBoth;
        end;
      if SValue='label' then
        begin
          Ctl:= TLabel.Create(AForm);
        end;
      if SValue='combo' then
        begin
          Ctl:= TComboBox.Create(AForm);
        end;
      if SValue='combo_ro' then
        begin
          Ctl:= TComboBox.Create(AForm);
          (Ctl as TComboBox).ReadOnly:= true;
        end;
      if SValue='button' then
        begin
          Ctl:= TButton.Create(AForm);
          (Ctl as TButton).ModalResult:= cButtonResultStart+ AForm.ControlCount;
          {$ifdef windows}
          Ctl.Height:= 23; //smaller
          {$endif}
          {$ifdef darwin}
          Ctl.Height:= 21; //smaller
          {$endif}
        end;
      if SValue='radiogroup' then
        Ctl:= TRadioGroup.Create(AForm);
      if SValue='checkgroup' then
        Ctl:= TCheckGroup.Create(AForm);
      if SValue='checklistbox' then
        Ctl:= TCheckListBox.Create(AForm);

      //disabled: label paints bad onto groupbox, Linux
      //if SValue='group' then
      //  Ctl:= TGroupBox.Create(AForm);

      if (SValue='listview') or
         (SValue='checklistview') then
      begin
        Ctl:= TListView.Create(AForm);
        (Ctl as TListView).ReadOnly:= true;
        (Ctl as TListView).ColumnClick:= false;
        (Ctl as TListView).ViewStyle:= vsReport;
        (Ctl as TListView).RowSelect:= true;
        (Ctl as TListView).Checkboxes:= (SValue='checklistview');
      end;

      //set parent
      if Assigned(Ctl) then
        Ctl.Parent:= AForm;
      Continue;
    end;

    //first name must be "type"
    if not Assigned(Ctl) then exit;

    //adjust previous label's FocusControl
    if Ctl is TWinControl then
      if AForm.ControlCount>=2 then
      begin
        CtlPrev:= AForm.Controls[AForm.ControlCount-2];
        if CtlPrev is TLabel then
          (CtlPrev as TLabel).FocusControl:= Ctl as TWinControl;
      end;

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
      if not IsControlAutosizeY(Ctl) then
        Ctl.Height:= NY2-NY1;
      Continue;
    end;

    //-------props
    if SName='props' then
    begin
      if Ctl is TButton then
      begin
        (Ctl as TButton).Default:= StrToBool(SGetItem(SValue));
      end;

      if Ctl is TSpinEdit then
      begin
        (Ctl as TSpinEdit).MinValue:= StrToIntDef(SGetItem(SValue), 0);
        (Ctl as TSpinEdit).MaxValue:= StrToIntDef(SGetItem(SValue), 100);
        (Ctl as TSpinEdit).Increment:= StrToIntDef(SGetItem(SValue), 1);
      end;

      if (Ctl is TEdit) or (Ctl is TMemo) then
      begin
        //RO
        if StrToBool(SGetItem(SValue)) then
        begin
          (Ctl as TCustomEdit).ReadOnly:= true;
          TCustomEditHack(Ctl).ParentColor:= true;
        end;
        //Monospaced
        if StrToBool(SGetItem(SValue)) then
        begin
          Ctl.Font.Name:= 'Courier New';
          {$ifdef windows}
          Ctl.Font.Size:= 9;
          {$endif}
        end;
        //Border
        if StrToBool(SGetItem(SValue)) then
          (Ctl as TCustomEdit).BorderStyle:= bsSingle
        else
          (Ctl as TCustomEdit).BorderStyle:= bsNone;
      end;
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
        if Ctl is TCheckGroup then (Ctl as TCheckGroup).Items.Add(SListItem);
        if Ctl is TRadioGroup then (Ctl as TRadioGroup).Items.Add(SListItem);
        if Ctl is TCheckListBox then (Ctl as TCheckListBox).Items.Add(SListItem);
        if Ctl is TListView then DoSetListviewItem(Ctl as TListView, SListItem);
      until false;
      Continue;
    end;

    //-------val
    if SName='val' then
    begin
      if Ctl is TCheckBox then (Ctl as TCheckBox).Checked:= StrToBool(SValue);
      if Ctl is TRadioButton then (Ctl as TRadioButton).Checked:= StrToBool(SValue);
      if Ctl is TEdit then (Ctl as TEdit).Text:= SValue;
      if Ctl is TComboBox then
      begin
        if (Ctl as TCombobox).ReadOnly then
          (Ctl as TCombobox).ItemIndex:= StrToIntDef(SValue, 0)
        else
          (Ctl as TCombobox).Text:= SValue;
      end;
      if Ctl is TListBox then (Ctl as TListBox).ItemIndex:= StrToIntDef(SValue, 0);
      if Ctl is TRadioGroup then (Ctl as TRadioGroup).ItemIndex:= StrToIntDef(SValue, 0);
      if Ctl is TCheckGroup then DoSetCheckgroupState(Ctl as TCheckGroup, SValue);
      if Ctl is TCheckListBox then DoSetChecklistboxState(Ctl as TCheckListBox, SValue);
      if Ctl is TMemo then DoSetMemoState(Ctl as TMemo, SValue);
      if Ctl is TSpinEdit then (Ctl as TSpinEdit).Value:= StrToIntDef(SValue, 0);
      if Ctl is TListView then DoSetListviewState(Ctl as TListView, SValue);

      Continue;
    end;

    //-------more?
  until false;
end;


procedure DoDialogCustom(const ATitle: string; ASizeX, ASizeY: integer;
  AText: string; AFocusedIndex: integer; out AButtonIndex: integer; out AStateText: string);
var
  F: TForm;
  Res: integer;
  SItem: string;
  Dummy: TDummyClass;
begin
  AButtonIndex:= -1;
  AStateText:= '';

  F:= TForm.Create(nil);
  Dummy:= TDummyClass.Create;
  try
    F.BorderStyle:= bsDialog;
    F.Position:= poDesktopCenter;
    F.Width:= ASizeX;
    F.Height:= ASizeY;
    F.Caption:= ATitle;

    repeat
      SItem:= SGetItem(AText, #10);
      if SItem='' then break;
      DoAddControl(F, SItem);
    until false;

    if (AFocusedIndex>=0) and (AFocusedIndex<F.ControlCount) then
      if F.Controls[AFocusedIndex].Enabled then
        if F.Controls[AFocusedIndex] is TWinControl then
          F.ActiveControl:= F.Controls[AFocusedIndex] as TWinControl;

    Dummy.Form:= F;
    F.KeyPreview:= true;
    F.OnKeyDown:= @Dummy.DoKeyDown;

    Res:= F.ShowModal;
    if Res>=cButtonResultStart then
    begin
      AButtonIndex:= Res-cButtonResultStart;
      AStateText:= DoGetFormResult(F);
    end;
  finally
    FreeAndNil(F);
    FreeAndNil(Dummy);
  end;
end;

{ TDummyClass }

procedure TDummyClass.DoKeyDown(Sender: TObject; var Key: Word;
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


end.

