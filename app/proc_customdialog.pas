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
  ListFilterEdit,
  ListViewFilterEdit,
  LclProc, LclType,
  proc_customdialog_dummy,
  formconsole,
  PythonEngine;

procedure DoDialogCustom(const ATitle: string;
  ASizeX, ASizeY: integer;
  const AText: string;
  const AFocusedIndex: integer;
  out AResultIndex: integer;
  out AResultText: string);

function IsDialogCustomShown: boolean;
function DoControl_GetAutoHeight(const Id: string): integer;
procedure DoControl_CreateNew(const S: string; AForm: TFormDummy; out Ctl: TControl);
function DoControl_GetPropsAsStringDict(C: TControl): PPyObject;
procedure DoControl_SetPropsFromStringDict(C: TControl; AText: string);
function DoForm_GetPropsAsStringDict(F: TFormDummy): PPyObject;
procedure DoForm_SetPropsFromStringDict(F: TFormDummy; AText: string);
procedure DoForm_FocusControl(F: TForm; AIndex: integer);


implementation

uses
  ATLinkLabel,
  ATStringProc,
  ATPanelColor;

var
  FDialogShown: boolean = false;

type
  TCustomEditHack = class(TCustomEdit);

function StrToBool(const S: string): boolean; inline;
begin
  Result:= S='1';
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
    (C is TSpinEdit) or
    (C is TListFilterEdit) or
    (C is TListViewFilterEdit);
end;

procedure DoControl_FixButtonHeight(C: TControl); inline;
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
    exit((C as TEdit).Text);

  if C is TCheckBox then
  begin
    case (C as TCheckBox).State of
      cbChecked: Result:= '1';
      cbUnchecked: Result:= '0';
      cbGrayed: Result:= '?';
    end;
    exit;
  end;

  if C is TToggleBox then
    exit(IntToStr(Ord((C as TToggleBox).Checked)));

  if C is TRadioButton then
    exit(IntToStr(Ord((C as TRadioButton).Checked)));

  if C is TListBox then
    exit(IntToStr((C as TListBox).ItemIndex));

  if C is TComboBox then
  begin
    if (C as TComboBox).ReadOnly then
      exit(IntToStr((C as TComboBox).ItemIndex))
    else
      exit((C as TComboBox).Text);
  end;

  if C is TMemo then
  begin
    Result:= (C as TMemo).Lines.Text;
    Result:= StringReplace(Result, #9, #2, [rfReplaceAll]);
    Result:= StringReplace(Result, #13#10, #9, [rfReplaceAll]);
    Result:= StringReplace(Result, #13, #9, [rfReplaceAll]);
    Result:= StringReplace(Result, #10, #9, [rfReplaceAll]);
    exit;
  end;

  if C is TRadioGroup then
    exit(IntToStr((C as TRadioGroup).ItemIndex));

  if C is TCheckGroup then
  begin
    for i:= 0 to (C as TCheckGroup).Items.Count-1 do
      Result:= Result+IntToStr(Ord((C as TCheckGroup).Checked[i]))+',';
    exit;
  end;

  if C is TCheckListBox then
  begin
    Result:= IntToStr((C as TCheckListBox).ItemIndex)+';';
    for i:= 0 to (C as TCheckListBox).Items.Count-1 do
      Result:= Result+IntToStr(Ord((C as TCheckListBox).Checked[i]))+',';
    exit;
  end;

  if C is TSpinEdit then
    exit(IntToStr((C as TSpinEdit).Value));

  if C is TListView then
    exit(DoControl_GetState_Listview(C as TListView));

  if C is TTabControl then
    exit(IntToStr((C as TTabControl).TabIndex));

  if C is TListFilterEdit then
    exit((C as TListFilterEdit).Text);

  if C is TListViewFilterEdit then
    exit((C as TListViewFilterEdit).Text);
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

procedure DoControl_SetState_CheckListbox(C: TCheckListBox; AValue: string);
var
  SItem: string;
  N: integer;
begin
  N:= StrToIntDef(SGetItem(AValue, ';'), -1);
  if (N>=0) and (N<C.Items.Count) then
    C.ItemIndex:= N;

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


procedure DoControl_SetState_Combobox(C: TCombobox; const SValue: string);
var
  N: integer;
begin
  if C.ReadOnly then
  begin
    N:= StrToIntDef(SValue, -1);
    if (N>=0) and (N<C.Items.Count) then
      C.ItemIndex:= N;
  end
  else
    C.Text:= SValue;
end;

procedure DoControl_SetState_Checkbox(C: TCheckbox; const SValue: string);
begin
  if SValue='1' then C.Checked:= true else
  if SValue='0' then C.Checked:= false else
  if SValue='?' then begin C.AllowGrayed:= true; C.State:= cbGrayed; end;
end;

procedure DoControl_SetState_Listbox(C: TListbox; const SValue: string);
var
  N: integer;
begin
  N:= StrToIntDef(SValue, -1);
  if (N>=0) and (N<C.Items.Count) then
    C.ItemIndex:= N;
end;

procedure DoControl_SetState_RadioGroup(C: TRadioGroup; const SValue: string);
var
  N: integer;
begin
  N:= StrToIntDef(SValue, -1);
  if (N>=0) and (N<C.Items.Count) then
    C.ItemIndex:= N;
end;


procedure DoControl_SetState_TabControl(C: TTabControl; const SValue: string);
var
  N: integer;
begin
  N:= StrToIntDef(SValue, -1);
  if (N>=0) and (N<C.Tabs.Count) then
    C.TabIndex:= N;
end;

procedure DoControl_SetState_Image(C: TImage; const SValue: string);
begin
  try
    C.Picture.LoadFromFile(SValue);
    C.Transparent:= true;
  except
  end;
end;

procedure DoControl_SetState_SpinEdit(C: TSpinEdit; const SValue: string);
var
  N: integer;
begin
  N:= StrToIntDef(SValue, -10000);
  if N<C.MinValue then N:= C.MinValue;
  if N>C.MaxValue then N:= C.MaxValue;
  C.Value:= N;
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


procedure DoControl_CreateNew(
  const S: string;
  AForm: TFormDummy;
  out Ctl: TControl);
var
  Props: TAppControlProps;
begin
  Ctl:= nil;

 try
  if S='check' then
  begin
    Ctl:= TCheckBox.Create(AForm);
    (Ctl as TCheckBox).OnChange:= @AForm.DoOnChange;
    exit;
  end;

  if S='edit' then
  begin
    Ctl:= TEdit.Create(AForm);
    exit;
  end;

  if S='edit_pwd' then
  begin
    Ctl:= TEdit.Create(AForm);
    TEdit(Ctl).EchoMode:= emPassword;
    exit;
  end;

  if S='spinedit' then
  begin
    Ctl:= TSpinEdit.Create(AForm);
    exit;
  end;

  if S='memo' then
  begin
    Ctl:= TMemo.Create(AForm);
    (Ctl as TMemo).WordWrap:= false;
    (Ctl as TMemo).ScrollBars:= ssBoth;
    exit;
  end;

  if S='label' then
  begin
    Ctl:= TLabel.Create(AForm);
    exit;
  end;

  if S='combo' then
  begin
    Ctl:= TComboBox.Create(AForm);
    (Ctl as TComboBox).DropDownCount:= 20;
    exit;
  end;

  if S='combo_ro' then
  begin
    Ctl:= TComboBox.Create(AForm);
    (Ctl as TComboBox).DropDownCount:= 20;
    (Ctl as TComboBox).ReadOnly:= true;
    (Ctl as TComboBox).OnChange:= @AForm.DoOnChange;
    exit;
  end;

  if S='button' then
  begin
    Ctl:= TButton.Create(AForm);
    (Ctl as TButton).OnClick:= @AForm.DoOnChange;
    DoControl_FixButtonHeight(Ctl);
    exit;
  end;

  if S='checkbutton' then
  begin
    Ctl:= TToggleBox.Create(AForm);
    (Ctl as TToggleBox).OnChange:= @AForm.DoOnChange;
    DoControl_FixButtonHeight(Ctl);
    exit;
  end;

  if S='listbox' then
  begin
    Ctl:= TListBox.Create(AForm);
    (Ctl as TListBox).OnSelectionChange:= @AForm.DoOnSelChange;
    exit;
  end;

  if S='radio' then
  begin
    Ctl:= TRadioButton.Create(AForm);
    (Ctl as TRadioButton).OnChange:= @AForm.DoOnChange;
    exit;
  end;

  if S='radiogroup' then
  begin
    Ctl:= TRadioGroup.Create(AForm);
    exit;
  end;

  if S='checkgroup' then
  begin
    Ctl:= TCheckGroup.Create(AForm);
    exit;
  end;

  if S='checklistbox' then
  begin
    Ctl:= TCheckListBox.Create(AForm);
    (Ctl as TCheckListBox).OnSelectionChange:= @AForm.DoOnSelChange;
    (Ctl as TCheckListBox).OnClickCheck:= @AForm.DoOnChange;
    exit;
  end;

  if (S='listview') or
     (S='checklistview') then
  begin
    Ctl:= TListView.Create(AForm);
    (Ctl as TListView).ReadOnly:= true;
    (Ctl as TListView).ColumnClick:= false;
    (Ctl as TListView).ViewStyle:= vsReport;
    (Ctl as TListView).RowSelect:= true;
    (Ctl as TListView).HideSelection:= false;
    (Ctl as TListView).Checkboxes:= (S='checklistview');
    (Ctl as TListView).OnChange:= @AForm.DoOnListviewChange;
    (Ctl as TListView).OnSelectItem:= @AForm.DoOnListviewSelect;
    exit;
  end;

  if S='linklabel' then
  begin
    Ctl:= TLinkLabel.Create(AForm);
    exit;
  end;

  if S='tabs' then
  begin
    Ctl:= TTabControl.Create(AForm);
    (Ctl as TTabControl).OnChange:= @AForm.DoOnChange;
    exit;
  end;

  if S='colorpanel' then
  begin
    Ctl:= TATPanelColor.Create(AForm);
    (Ctl as TATPanelColor).OnClick:= @AForm.DoOnChange;
    exit;
  end;

  if S='image' then
  begin
    Ctl:= TImage.Create(AForm);
    (Ctl as TImage).Proportional:= true;
    (Ctl as TImage).AntialiasingMode:= amOn;
    exit;
  end;

  if S='filter_listbox' then
  begin
    Ctl:= TListFilterEdit.Create(AForm);
    exit;
  end;

  if S='filter_listview' then
  begin
    Ctl:= TListViewFilterEdit.Create(AForm);
    exit;
  end;

 finally
   if Assigned(Ctl) then
   begin
     Props:= TAppControlProps.Create(S);
     if S='button' then
       Props.FActive:= true;
     Ctl.Tag:= PtrInt(Props);
   end;
 end;
end;


procedure DoControl_SetPropsFromString_Adv(C: TControl; S: string);
begin
  if C is TButton then
  begin
    (C as TButton).Default:= StrToBool(SGetItem(S));
    exit
  end;

  if C is TSpinEdit then
  begin
    (C as TSpinEdit).MinValue:= StrToIntDef(SGetItem(S), 0);
    (C as TSpinEdit).MaxValue:= StrToIntDef(SGetItem(S), 100);
    (C as TSpinEdit).Increment:= StrToIntDef(SGetItem(S), 1);
    exit
  end;

  if C is TLinkLabel then
  begin
    (C as TLinkLabel).Link:= S;
    exit
  end;

  if C is TLabel then
  begin
    if StrToBool(SGetItem(S)) then
    begin
      (C as TLabel).AutoSize:= false;
      (C as TLabel).Alignment:= taRightJustify;
    end;
    exit
  end;

  if (C is TEdit) or (C is TMemo) then
  begin
    //RO
    if StrToBool(SGetItem(S)) then
    begin
      (C as TCustomEdit).ReadOnly:= true;
      TCustomEditHack(C).ParentColor:= true;
    end;
    //Monospaced
    if StrToBool(SGetItem(S)) then
    begin
      C.Font.Name:= 'Courier New';
      {$ifdef windows}
      C.Font.Size:= 9;
      {$endif}
    end;
    //Border
    if StrToBool(SGetItem(S)) then
      (C as TCustomEdit).BorderStyle:= bsSingle
    else
      (C as TCustomEdit).BorderStyle:= bsNone;

    exit;
  end;

  if (C is TListView) then
  begin
    (C as TListView).GridLines:= StrToBool(SGetItem(S));
    exit
  end;

  if (C is TTabControl) then
  begin
    if StrToBool(S) then
      (C as TTabControl).TabPosition:= tpBottom;
    exit
  end;

  if (C is TATPanelColor) then
  begin
    (C as TATPanelColor).BorderWidth:= StrToIntDef(SGetItem(S), 0);
    (C as TATPanelColor).Color:= StrToIntDef(SGetItem(S), clDefault);
    (C as TATPanelColor).Font.Color:= StrToIntDef(SGetItem(S), clDefault);
    (C as TATPanelColor).BorderColor:= StrToIntDef(SGetItem(S), clBlack);
    exit
  end;

  if (C is TImage) then
  begin
    (C as TImage).Center:= StrToBool(SGetItem(S));
    (C as TImage).Stretch:= StrToBool(SGetItem(S));
    (C as TImage).StretchInEnabled:= StrToBool(SGetItem(S));
    (C as TImage).StretchOutEnabled:= StrToBool(SGetItem(S));
    (C as TImage).KeepOriginXWhenClipped:= StrToBool(SGetItem(S));
    (C as TImage).KeepOriginYWhenClipped:= StrToBool(SGetItem(S));
    exit
  end;

  if (C is TListViewFilterEdit) then
  begin
    (C as TListViewFilterEdit).ByAllFields:= StrToBool(SGetItem(S));
    exit
  end;
end;


procedure DoControl_SetItemsFromString(C: TControl; S: string);
var
  SItem: string;
begin
  if C is TImage then
  begin
    DoControl_SetState_Image(C as TImage, S);
    exit
  end;

  repeat
    SItem:= SGetItem(S, #9);
    if SItem='' then Break;
    if C is TListbox then (C as TListbox).Items.Add(SItem);
    if C is TComboBox then (C as TComboBox).Items.Add(SItem);
    if C is TCheckGroup then (C as TCheckGroup).Items.Add(SItem);
    if C is TRadioGroup then (C as TRadioGroup).Items.Add(SItem);
    if C is TCheckListBox then (C as TCheckListBox).Items.Add(SItem);
    if C is TListView then DoControl_SetState_ListviewItem(C as TListView, SItem);
    if C is TTabControl then (C as TTabControl).Tabs.Add(SItem);
  until false;
end;


procedure DoControl_SetPosFromString(C: TControl; S: string);
var
  NX1, NY1, NX2, NY2: integer;
begin
  NX1:= StrToIntDef(SGetItem(S, ','), -1);
  NY1:= StrToIntDef(SGetItem(S, ','), -1);
  NX2:= StrToIntDef(SGetItem(S, ','), -1);
  NY2:= StrToIntDef(SGetItem(S, ','), -1);
  if NX1<0 then exit;
  if NX2<0 then exit;
  if NY1<0 then exit;
  if NY2<0 then exit;
  C.Left:= NX1;
  C.Width:= NX2-NX1;
  C.Top:= NY1;
  if not DoControl_IsAutoHeight(C) then
    C.Height:= NY2-NY1;
end;


procedure DoControl_SetHintFromString(C: TControl; const S: string);
begin
  //QT cannot handle #13 in hint
  C.Hint:= StringReplace(S, #13, #10, [rfReplaceAll]);
end;


procedure DoControl_SetStateFromString(C: TControl; const S: string);
begin
  if C is TCheckBox then
  begin
    DoControl_SetState_Checkbox(C as TCheckbox, S);
    exit
  end;
  if C is TToggleBox then
  begin
    (C as TToggleBox).Checked:= StrToBool(S);
    exit
  end;
  if C is TRadioButton then
  begin
    (C as TRadioButton).Checked:= StrToBool(S);
    exit
  end;
  if C is TEdit then
  begin
    (C as TEdit).Text:= S;
    exit
  end;
  if C is TComboBox then
  begin
    DoControl_SetState_Combobox(C as TCombobox, S);
    exit
  end;
  if C is TListBox then
  begin
    DoControl_SetState_Listbox(C as TListbox, S);
    exit
  end;
  if C is TRadioGroup then
  begin
    DoControl_SetState_RadioGroup(C as TRadioGroup, S);
    exit
  end;
  if C is TCheckGroup then
  begin
    DoControl_SetState_CheckGroup(C as TCheckGroup, S);
    exit
  end;
  if C is TCheckListBox then
  begin
    DoControl_SetState_CheckListbox(C as TCheckListBox, S);
    exit
  end;
  if C is TMemo then
  begin
    DoControl_SetState_Memo(C as TMemo, S);
    exit
  end;
  if C is TSpinEdit then
  begin
    DoControl_SetState_SpinEdit(C as TSpinEdit, S);
    exit
  end;
  if C is TListView then
  begin
    DoControl_SetState_Listview(C as TListView, S);
    exit
  end;
  if C is TTabControl then
  begin
    DoControl_SetState_TabControl(C as TTabControl, S);
    exit
  end;
  if C is TListFilterEdit then
  begin
    (C as TListFilterEdit).Text:= S;
    exit
  end;
  if C is TListViewFilterEdit then
  begin
    (C as TListViewFilterEdit).Text:= S;
    exit
  end;
end;


procedure DoControl_SetPropFromPair(C: TControl; AName, AValue: string);
begin
  if AName='name' then
  begin
    try
      C.Name:= AValue;
    except
      fmConsole.DoLogConsoleLine(Format('API: incorrect or duplicate control name "%s"', [AValue]));
    end;
    exit;
  end;

  if AName='cap' then
  begin
    C.Caption:= AValue;
    exit;
  end;

  if AName='en' then
  begin
    C.Enabled:= StrToBool(AValue);
    exit;
  end;

  if AName='vis' then
  begin
    C.Visible:= StrToBool(AValue);
    exit;
  end;

  if AName='hint' then
  begin
    DoControl_SetHintFromString(C, AValue);
    exit;
  end;

  if AName='act' then
  begin
    TAppControlProps(C.Tag).FActive:= StrToBool(AValue);
    exit;
  end;

  if AName='pos' then
  begin
    DoControl_SetPosFromString(C, AValue);
    exit;
  end;

  if AName='x' then
  begin
    C.Left:= StrToIntDef(AValue, C.Left);
    exit;
  end;
  if AName='y' then
  begin
    C.Top:= StrToIntDef(AValue, C.Top);
    exit;
  end;
  if AName='w' then
  begin
    C.ClientWidth:= StrToIntDef(AValue, C.ClientWidth);
    exit;
  end;
  if AName='h' then
  begin
    C.ClientHeight:= StrToIntDef(AValue, C.ClientHeight);
    exit;
  end;

  if AName='props' then
  begin
    DoControl_SetPropsFromString_Adv(C, AValue);
    exit;
  end;

  if AName='items' then
  begin
    DoControl_SetItemsFromString(C, AValue);
    exit;
  end;

  if AName='val' then
  begin
    DoControl_SetStateFromString(C, AValue);
    exit;
  end;

  if AName='tag' then
  begin
    TAppControlProps(C.Tag).FTagString:= AValue;
    exit;
  end;

  if AName='callback' then
  begin
    TAppControlProps(C.Tag).FCallback:= AValue;
    exit;
  end;

  if AName='color' then
  begin
    C.Color:= StrToIntDef(AValue, C.Color);
    exit;
  end;

  if AName='font_name' then
  begin
    C.Font.Name:= AValue;
    exit;
  end;

  if AName='font_size' then
  begin
    C.Font.Size:= StrToIntDef(AValue, C.Font.Size);
    exit;
  end;

  if AName='font_color' then
  begin
    C.Font.Color:= StrToIntDef(AValue, C.Font.Color);
    exit;
  end;
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


procedure DoForm_AddControl(AForm: TFormDummy; ATextItems: string);
var
  SNameValue, SName, SValue: string;
  Ctl, CtlPrev: TControl;
begin
  Ctl:= nil;

  repeat
    SNameValue:= SGetItem(ATextItems, Chr(1));
    if SNameValue='' then break;
    SName:= SGetItem(SNameValue, '=');
    SValue:= SNameValue;
    if SName='' then Continue;

    //type
    if SName='type' then
    begin
      DoControl_CreateNew(SValue, AForm, Ctl);
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

    DoControl_SetPropFromPair(Ctl, SName, SValue);
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
  F: TFormDummy;
  const AContent: string);
var
  List: TStringList;
  i: integer;
begin
  List:= TStringList.Create;
  try
    List.StrictDelimiter:= true;
    List.Delimiter:= #10;
    List.DelimitedText:= AContent;
    for i:= 0 to List.Count-1 do
      DoForm_AddControl(F, List[i]);
  finally
    FreeAndNil(List);
  end;
end;


procedure DoDialogCustom(const ATitle: string;
  ASizeX, ASizeY: integer;
  const AText: string;
  const AFocusedIndex: integer;
  out AResultIndex: integer;
  out AResultText: string);
var
  F: TFormDummy;
begin
  AResultIndex:= -1;
  AResultText:= '';

  F:= TFormDummy.Create(nil);
  try
    F.IsDlgCustom:= true;
    F.Caption:= ATitle;
    F.ClientWidth:= ASizeX;
    F.ClientHeight:= ASizeY;

    DoForm_FillContent(F, AText);
    DoForm_FocusControl(F, AFocusedIndex);

    FDialogShown:= true;
    if F.ShowModal=mrOk then
    begin
      AResultIndex:= F.IdClicked;
      AResultText:= DoForm_GetResult(F);
    end;
  finally
    FreeAndNil(F);
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
  if (Id='edit') or (Id='filter_listview') or (Id='filter_listbox') then C:= TEdit.Create(nil) else
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


procedure DoForm_SetPropFromPair(F: TFormDummy; const AName, AValue: string);
begin
  if AName='cap' then
    F.Caption:= AValue
  else
  if AName='x' then
  begin
    F.Position:= poDesigned;
    F.Left:= StrToIntDef(AValue, F.Left)
  end
  else
  if AName='y' then
  begin
    F.Position:= poDesigned;
    F.Top:= StrToIntDef(AValue, F.Top)
  end
  else
  if AName='w' then
    F.ClientWidth:= StrToIntDef(AValue, F.ClientWidth)
  else
  if AName='h' then
    F.ClientHeight:= StrToIntDef(AValue, F.ClientHeight)
  else
  if AName='w_min' then
    F.Constraints.MinWidth:= StrToIntDef(AValue, 0)
  else
  if AName='w_max' then
    F.Constraints.MaxWidth:= StrToIntDef(AValue, 1000)
  else
  if AName='h_min' then
    F.Constraints.MinHeight:= StrToIntDef(AValue, 0)
  else
  if AName='h_max' then
    F.Constraints.MaxHeight:= StrToIntDef(AValue, 1000)
  else
  if AName='resize' then
  begin
    if StrToBool(AValue) then
      F.BorderStyle:= bsSizeable
    else
      F.BorderStyle:= bsDialog;
  end
  else
  if AName='topmost' then
  begin
    if StrToBool(AValue) then
      F.FormStyle:= fsStayOnTop
    else
      F.FormStyle:= fsNormal;
  end
  else
  if AName='callback' then
    F.Callback:= AValue
  else
  if AName='vis' then
    F.Visible:= StrToBool(AValue)
  else
  ;
end;


function DoForm_GetPropsAsStringDict(F: TFormDummy): PPyObject;
begin
  with GetPythonEngine do
  begin
    Result:= Py_BuildValue('{sssisisisisisisOsOsO}',
      'cap', PChar(F.Caption),
      PChar(string('x')), F.Left,
      PChar(string('y')), F.Top,
      PChar(string('w')), F.Width,
      PChar(string('h')), F.Height,
      'clicked', F.IdClicked,
      'focused', F.IdFocused,
      'vis', PyBool_FromLong(Ord(F.Visible)),
      'resize', PyBool_FromLong(Ord(F.BorderStyle=bsSizeable)),
      'topmost', PyBool_FromLong(Ord(F.FormStyle=fsStayOnTop))
      );
  end;
end;


procedure DoForm_SetPropsFromStringDict(F: TFormDummy; AText: string);
var
  SItem, SKey, SValue: string;
begin
  //text is '{key1:value1;key2:value2}' from to_str()
  if AText[1]='{' then
    AText:= Copy(AText, 2, Length(AText)-2);
  repeat
    SItem:= SGetItem(AText, chr(1));
    if SItem='' then Break;
    SKey:= SGetItem(SItem, ':');
    SValue:= SItem;
    DoForm_SetPropFromPair(F, SKey, SValue);
  until false;
end;


function DoControl_GetPropsAsStringDict(C: TControl): PPyObject;
begin
  with GetPythonEngine do
  begin
    Result:= Py_BuildValue('{sssssssssssssisisisisssOsOsO}',
      'name', PChar(C.Name),
      'cap', PChar(C.Caption),
      'hint', PChar(C.Hint),
      'type', PChar(TAppControlProps(C.Tag).FTypeString),
      'tag', PChar(TAppControlProps(C.Tag).FTagString),
      'callback', PChar(TAppControlProps(C.Tag).FCallback),
      PChar(string('x')), C.Left,
      PChar(string('y')), C.Top,
      PChar(string('w')), C.Width,
      PChar(string('h')), C.Height,
      'val', PChar(DoControl_GetState(C)),
      'act', PyBool_FromLong(Ord(TAppControlProps(C.Tag).FActive)),
      'en', PyBool_FromLong(Ord(C.Enabled)),
      'vis', PyBool_FromLong(Ord(C.Visible))
      );
  end;
end;


procedure DoControl_SetPropsFromStringDict(C: TControl; AText: string);
var
  SItem, SKey, SValue: string;
begin
  //text is '{key1:value1;key2:value2}' from to_str()
  if AText[1]='{' then
    AText:= Copy(AText, 2, Length(AText)-2);
  repeat
    SItem:= SGetItem(AText, chr(1));
    if SItem='' then Break;
    SKey:= SGetItem(SItem, ':');
    SValue:= SItem;
    DoControl_SetPropFromPair(C, SKey, SValue);
  until false;
end;


end.

