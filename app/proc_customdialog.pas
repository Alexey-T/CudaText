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
  CheckLst, Spin, ComCtrls, Dialogs,
  LclProc, LclType;

procedure DoDialogCustom(const ATitle: string; ASizeX, ASizeY: integer;
  const AText: string; AFocusedIndex: integer; out AButtonIndex: integer; out AStateText: string);

function IsDialogCustomShown: boolean;
function DoControl_GetAutoHeight(const Id: string): integer;

implementation

uses
  ATLinkLabel,
  ATStringProc,
  ATColorPanel,
  proc_customdialog_dummy;

var
  FDialogShown: boolean = false;

type
  TCustomEditHack = class(TCustomEdit);

function StrToBool(const S: string): boolean;
begin
  Result:= S<>'0';
end;

function DoControl_IsAutoHeight(C: TControl): boolean;
begin
  Result:=
    (C is TLabel) or
    (C is TButton) or
    (C is TToggleBox) or
    (C is TEdit) or
    (C is TComboBox) or
    (C is TCheckBox) or
    (C is TRadioButton) or
    (C is TSpinEdit);
end;

procedure DoControl_FixButtonHeight(C: TControl);
begin
  {$ifdef windows}
  C.Height:= 23; //smaller
  {$endif}

  {$ifdef linux}
  C.Height:= 25;
  {$endif}

  {$ifdef darwin}
  C.Height:= 21; //smaller
  {$endif}
end;

function DoControl_GetState_Listview(C: TListView): string; forward;

function DoControl_GetState(C: TControl): string;
var
  i: integer;
begin
  Result:= '';

  if C is TEdit then
    Result:= (C as TEdit).Text;

  if C is TCheckBox then
    with C as TCheckBox do
      case State of
        cbChecked: Result:= '1';
        cbUnchecked: Result:= '0';
        cbGrayed: Result:= '?';
      end;

  if C is TToggleBox then
    Result:= IntToStr(Ord((C as TToggleBox).Checked));

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
    Result:= DoControl_GetState_Listview(C as TListView);

  if C is TTabControl then
    Result:= IntToStr((C as TTabControl).TabIndex);
end;


procedure DoControl_SetState_Memo(C: TMemo; AValue: string);
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

procedure DoControl_SetState_Checkgroup(C: TCheckGroup; AValue: string);
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

procedure DoControl_SetState_Checklistbox(C: TCheckListBox; AValue: string);
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


procedure DoControl_SetState_ListviewItem(C: TListView; SListItem: string);
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


procedure DoControl_SetState_Listview(C: TListView; SValue: string);
var
  N: integer;
  SItem: string;
begin
  //index
  SItem:= SGetItem(SValue, ';');
  N:= StrToIntDef(SItem, 0);
  if (N>=0) and (N<C.Items.Count) then
  begin
    C.ItemFocused:= C.Items[N];
    C.Selected:= C.ItemFocused;
  end;

  //check0,check1,..
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


function DoControl_GetState_Listview(C: TListView): string;
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


procedure DoForm_Scale(F: TForm);
begin
  //dont scale if Screen dpi is smaller (macOS: 72)
  if Screen.PixelsPerInch>F.DesignTimeDPI then
    F.AutoAdjustLayout(lapAutoAdjustForDPI,
      F.DesignTimeDPI, Screen.PixelsPerInch,
      F.Width, ScaleX(F.Width, F.DesignTimeDPI)
      , false //AScaleFonts, Laz 1.7 trunk
      );
end;


function DoForm_GetResult(AForm: TForm): string;
var
  List: TStringList;
  NActive, i: integer;
  C: TControl;
begin
  Result:= '';

  List:= TStringList.Create;
  try
    List.TextLineBreakStyle:= tlbsLF;

    NActive:= -1;
    for i:= 0 to AForm.ControlCount-1 do
    begin
      C:= AForm.Controls[i];
      if C=AForm.ActiveControl then NActive:= i;
      List.Add(DoControl_GetState(C));
    end;

    //append NActive
    List.Add('focused='+IntToStr(NActive));

    Result:= List.Text;
  finally
    FreeAndNil(List);
  end;
end;

procedure DoForm_AddControl(AForm: TForm; ATextItems: string; ADummy: TDummyClass);
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
      if SValue='check' then
      begin
        Ctl:= TCheckBox.Create(AForm);
        (Ctl as TCheckBox).OnChange:= @ADummy.DoOnChange;
      end;
      if SValue='radio' then
      begin
        Ctl:= TRadioButton.Create(AForm);
        (Ctl as TRadioButton).OnChange:= @ADummy.DoOnChange;
      end;
      if SValue='edit' then
      begin
        Ctl:= TEdit.Create(AForm);
      end;
      if SValue='listbox' then
      begin
        Ctl:= TListBox.Create(AForm);
        (Ctl as TListBox).OnSelectionChange:= @ADummy.DoOnSelChange;
      end;
      if SValue='spinedit' then
      begin
        Ctl:= TSpinEdit.Create(AForm);
      end;
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
          (Ctl as TComboBox).DropDownCount:= 20;
        end;
      if SValue='combo_ro' then
        begin
          Ctl:= TComboBox.Create(AForm);
          (Ctl as TComboBox).DropDownCount:= 20;
          (Ctl as TComboBox).ReadOnly:= true;
          (Ctl as TComboBox).OnChange:= @ADummy.DoOnChange;
        end;
      if SValue='button' then
        begin
          Ctl:= TButton.Create(AForm);
          (Ctl as TButton).ModalResult:= Dummy_ResultStart+ AForm.ControlCount;
          DoControl_FixButtonHeight(Ctl);
        end;
      if SValue='checkbutton' then
        begin
          Ctl:= TToggleBox.Create(AForm);
          (Ctl as TToggleBox).OnChange:= @ADummy.DoOnChange;
          DoControl_FixButtonHeight(Ctl);
        end;
      if SValue='radiogroup' then
      begin
        Ctl:= TRadioGroup.Create(AForm);
      end;
      if SValue='checkgroup' then
      begin
        Ctl:= TCheckGroup.Create(AForm);
      end;
      if SValue='checklistbox' then
      begin
        Ctl:= TCheckListBox.Create(AForm);
        (Ctl as TCheckListBox).OnSelectionChange:= @ADummy.DoOnSelChange;
        (Ctl as TCheckListBox).OnClickCheck:= @ADummy.DoOnChange;
      end;

      if (SValue='listview') or
         (SValue='checklistview') then
      begin
        Ctl:= TListView.Create(AForm);
        (Ctl as TListView).ReadOnly:= true;
        (Ctl as TListView).ColumnClick:= false;
        (Ctl as TListView).ViewStyle:= vsReport;
        (Ctl as TListView).RowSelect:= true;
        (Ctl as TListView).HideSelection:= false;
        (Ctl as TListView).Checkboxes:= (SValue='checklistview');
        (Ctl as TListView).OnChange:= @ADummy.DoOnListviewChange;
        (Ctl as TListView).OnSelectItem:= @ADummy.DoOnListviewSelect;
      end;

      if SValue='linklabel' then
      begin
        Ctl:= TLinkLabel.Create(AForm);
      end;

      if SValue='tabs' then
      begin
        Ctl:= TTabControl.Create(AForm);
        (Ctl as TTabControl).OnChange:= @ADummy.DoOnChange;
      end;

      if SValue='colorpanel' then
      begin
        Ctl:= TATColorPanel.Create(AForm);
        (Ctl as TPanel).BorderStyle:= bsNone;
        (Ctl as TPanel).BevelInner:= bvNone;
        (Ctl as TPanel).BevelOuter:= bvNone;
        (Ctl as TPanel).OnClick:= @ADummy.DoOnChange;
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

    //-------hint
    if SName='hint' then
    begin
      Ctl.Hint:= SValue;
      Continue;
    end;

    //-------act
    if SName='act' then
    begin
      if SValue='1' then
        Ctl.Tag:= Dummy_TagActive;
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
      if not DoControl_IsAutoHeight(Ctl) then
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

      if Ctl is TLinkLabel then
        (Ctl as TLinkLabel).Link:= SValue;

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

      if (Ctl is TListView) then
      begin
        (Ctl as TListView).GridLines:= StrToBool(SGetItem(SValue));
      end;

      if (Ctl is TTabControl) then
        if SValue='1' then
          (Ctl as TTabControl).TabPosition:= tpBottom;

      if (Ctl is TATColorPanel) then
      begin
        (Ctl as TPanel).BorderWidth:= StrToIntDef(SGetItem(SValue), 0);
        (Ctl as TPanel).Color:= StrToIntDef(SGetItem(SValue), clDefault);
        (Ctl as TPanel).Font.Color:= StrToIntDef(SGetItem(SValue), clDefault);
        (Ctl as TATColorPanel).BorderColor:= StrToIntDef(SGetItem(SValue), clBlack);
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
        if Ctl is TListView then DoControl_SetState_ListviewItem(Ctl as TListView, SListItem);
        if Ctl is TTabControl then (Ctl as TTabControl).Tabs.Add(SListItem);
      until false;
      Continue;
    end;

    //-------val
    if SName='val' then
    begin
      if Ctl is TCheckBox then
        with Ctl as TCheckBox do
        begin
          if SValue='1' then Checked:= true else
          if SValue='0' then Checked:= false else
          if SValue='?' then begin AllowGrayed:= true; State:= cbGrayed; end;
        end;

      if Ctl is TToggleBox then (Ctl as TToggleBox).Checked:= StrToBool(SValue);
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
      if Ctl is TCheckGroup then DoControl_SetState_Checkgroup(Ctl as TCheckGroup, SValue);
      if Ctl is TCheckListBox then DoControl_SetState_Checklistbox(Ctl as TCheckListBox, SValue);
      if Ctl is TMemo then DoControl_SetState_Memo(Ctl as TMemo, SValue);
      if Ctl is TSpinEdit then (Ctl as TSpinEdit).Value:= StrToIntDef(SValue, 0);
      if Ctl is TListView then DoControl_SetState_Listview(Ctl as TListView, SValue);
      if Ctl is TTabControl then (Ctl as TTabControl).TabIndex:= StrToIntDef(SValue, 0);

      Continue;
    end;

    //-------more?
  until false;
end;


procedure DoForm_FocusControl(F: TForm; AIndex: integer);
var
  C: TControl;
begin
  if (AIndex>=0) and (AIndex<F.ControlCount) then
  begin
    C:= F.Controls[AIndex];
    if C.Enabled then
      if C is TWinControl then
        F.ActiveControl:= C as TWinControl;
  end;
end;

procedure DoForm_FillContent(
  F: TForm; Dummy: TDummyClass;
  const AContent: string);
var
  List: TStringList;
  i: integer;
begin
  F.BorderStyle:= bsDialog;
  F.ShowHint:= true;

  List:= TStringList.Create;
  try
    List.StrictDelimiter:= true;
    List.Delimiter:= #10;
    List.DelimitedText:= AContent;
    for i:= 0 to List.Count-1 do
      DoForm_AddControl(F, List[i], Dummy);
  finally
    FreeAndNil(List);
  end;

  {
  //prev variant, slower
  repeat
    SItem:= SGetItem(AText, #10);
    if SItem='' then break;
    DoForm_AddControl(F, SItem, Dummy);
  until false;
  }

  Dummy.Form:= F;
  F.KeyPreview:= true;
  F.OnKeyDown:= @Dummy.DoOnKeyDown;
  F.OnShow:= @Dummy.DoOnShow;

  DoForm_Scale(F);
end;


procedure DoDialogCustom(const ATitle: string; ASizeX, ASizeY: integer;
  const AText: string; AFocusedIndex: integer; out AButtonIndex: integer; out AStateText: string);
var
  F: TForm;
  Res: integer;
  Dummy: TDummyClass;
begin
  AButtonIndex:= -1;
  AStateText:= '';

  F:= TForm.Create(nil);
  Dummy:= TDummyClass.Create;
  try
    DoForm_FillContent(F, Dummy, AText);
    DoForm_FocusControl(F, AFocusedIndex);
    F.Position:= poScreenCenter;
    F.ClientWidth:= ASizeX;
    F.ClientHeight:= ASizeY;
    F.Caption:= ATitle;

    FDialogShown:= true;
    Res:= F.ShowModal;

    if Res>=Dummy_ResultStart then
    begin
      AButtonIndex:= Res-Dummy_ResultStart;
      AStateText:= DoForm_GetResult(F);
    end;
  finally
    FreeAndNil(F);
    FreeAndNil(Dummy);
    FDialogShown:= false;
  end;
end;

function IsDialogCustomShown: boolean;
begin
  Result:= FDialogShown;
end;

function DoControl_GetAutoHeight(const Id: string): integer;
var
  C: TControl;
begin
  Result:= 0;
  if Id='button' then begin C:= TButton.Create(nil); DoControl_FixButtonHeight(C); end else
  if Id='label' then C:= TLabel.Create(nil) else
  if Id='combo' then C:= TComboBox.Create(nil) else
  if Id='combo_ro' then begin C:= TComboBox.Create(nil); TCombobox(C).ReadOnly:= true; end else
  if Id='edit' then C:= TEdit.Create(nil) else
  if Id='spinedit' then C:= TSpinEdit.Create(nil) else
  if Id='check' then C:= TCheckbox.Create(nil) else
  if Id='radio' then C:= TRadioButton.Create(nil) else
  if Id='checkbutton' then C:= TToggleBox.Create(nil) else
  exit;

  try
    C.Caption:= 'WpPJjy'; //for label autosize
    C.Parent:= Application.MainForm; //else height incorrect
    Result:= C.Height;
  finally
    FreeAndNil(C);
  end;
end;


end.

