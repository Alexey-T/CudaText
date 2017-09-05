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
  ATListbox,
  ATButtons,
  ATButtonsToolbar,
  ATLinkLabel,
  ATStringProc,
  ATPanelColor,
  ATGauge,
  proc_customdialog_dummy,
  proc_miscutils,
  proc_globdata,
  proc_scrollbars,
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
procedure DoForm_AdjustLabelForNewControl(F: TForm; Ctl: TControl);
procedure DoForm_FocusControl(F: TForm; C: TControl);
procedure DoForm_FocusControl(F: TForm; AIndex: integer);
procedure DoForm_ScaleAuto(F: TForm);
procedure DoForm_CloseDockedForms(F: TForm);


//var
//  CustomDialog_Listbox_OnDrawItem: TATListboxDrawItemEvent = nil;

implementation

var
  FDialogShown: boolean = false;

type
  TControlHack = class(TControl);
  TCustomEditHack = class(TCustomEdit);
  TWinControlHack = class(TWinControl);

const
  cControlBorderStyles: array[boolean] of TBorderStyle = (bsNone, bsSingle);


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

function DoControl_GetState_CheckListBox(C: TCheckListBox): string;
var
  i: integer;
begin
  Result:= IntToStr(C.ItemIndex)+';';
  for i:= 0 to C.Items.Count-1 do
    Result:= Result+IntToStr(Ord(C.Checked[i]))+',';
end;

function DoControl_GetState_CheckGroup(C: TCheckGroup): string;
var
  i: integer;
begin
  Result:= '';
  for i:= 0 to C.Items.Count-1 do
    Result:= Result+IntToStr(Ord(C.Checked[i]))+',';
end;

function DoControl_GetState_Memo(C: TMemo): string;
begin
  Result:= C.Lines.Text;
  Result:= StringReplace(Result, #9, #2, [rfReplaceAll]);
  Result:= StringReplace(Result, #13#10, #9, [rfReplaceAll]);
  Result:= StringReplace(Result, #13, #9, [rfReplaceAll]);
  Result:= StringReplace(Result, #10, #9, [rfReplaceAll]);
end;

function DoControl_GetState_CheckBox(C: TCheckBox): string;
begin
  case C.State of
    cbChecked: Result:= '1';
    cbUnchecked: Result:= '0';
    cbGrayed: Result:= '?';
    else Result:= '';
  end;
end;

function DoControl_GetState(C: TControl): string;
begin
  Result:= '';

  if C is TEdit then
    exit((C as TEdit).Text);

  if C is TCheckBox then
    exit(DoControl_GetState_CheckBox(C as TCheckbox));

  if C is TToggleBox then
    exit(IntToStr(Ord((C as TToggleBox).Checked)));

  if C is TRadioButton then
    exit(IntToStr(Ord((C as TRadioButton).Checked)));

  if C is TListBox then
    exit(IntToStr((C as TListBox).ItemIndex));

  if C is TComboBox then
  begin
    if (C as TComboBox).Style=csDropDownList then
      exit(IntToStr((C as TComboBox).ItemIndex))
    else
      exit((C as TComboBox).Text);
  end;

  if C is TMemo then
    exit(DoControl_GetState_Memo(C as TMemo));

  if C is TRadioGroup then
    exit(IntToStr((C as TRadioGroup).ItemIndex));

  if C is TCheckGroup then
    exit(DoControl_GetState_CheckGroup(C as TCheckGroup));

  if C is TCheckListBox then
    exit(DoControl_GetState_CheckListBox(C as TCheckListBox));

  if C is TSpinEdit then
    exit(IntToStr((C as TSpinEdit).Value));

  if C is TListView then
    exit(DoControl_GetState_Listview(C as TListView));

  if C is TTabControl then
    exit(IntToStr((C as TTabControl).TabIndex));

  if C is TTrackBar then
    exit(IntToStr((C as TTrackBar).Position));

  if C is TProgressBar then
    exit(IntToStr((C as TProgressBar).Position));

  if C is TGauge then
    exit(IntToStr((C as TGauge).Progress));

  if C is TListFilterEdit then
    exit((C as TListFilterEdit).Text);

  if C is TListViewFilterEdit then
    exit((C as TListViewFilterEdit).Text);
end;


procedure DoControl_SetState_Memo(C: TMemo; AValue: string);
var
  SItem: string;
begin
  C.Lines.BeginUpdate;
  C.Lines.Clear;
  repeat
    SItem:= SGetItem(AValue, #9);
    if SItem='' then break;
    C.Lines.Add(SItem);
  until false;
  C.Lines.EndUpdate;
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
    C.Checked[N]:= AppStrToBool(SItem);
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
    C.Checked[N]:= AppStrToBool(SItem);
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
  if C.Style in [csDropDownList, csOwnerDrawFixed, csOwnerDrawVariable] then
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
      C.Items[N].Checked:= AppStrToBool(SItem);
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
    (Ctl as TComboBox).Style:= csDropDownList;
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

  if S='button_ex' then
  begin
    Ctl:= TATButton.Create(AForm);
    (Ctl as TATButton).OnClick:= @AForm.DoOnChange;
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
    (Ctl as TListBox).OnSelectionChange:= @AForm.DoOnListboxSelect;
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

  if S='panel' then
  begin
    Ctl:= TPanel.Create(AForm);
    (Ctl as TPanel).BevelInner:= bvNone;
    (Ctl as TPanel).BevelOuter:= bvNone;
    (Ctl as TPanel).BorderStyle:= bsNone;
    exit;
  end;

  if S='group' then
  begin
    Ctl:= TGroupBox.Create(AForm);
    exit;
  end;

  if S='checklistbox' then
  begin
    Ctl:= TCheckListBox.Create(AForm);
    (Ctl as TCheckListBox).OnSelectionChange:= @AForm.DoOnListboxSelect;
    (Ctl as TCheckListBox).OnClickCheck:= @AForm.DoOnChange;
    exit;
  end;

  if (S='listview') or
     (S='checklistview') then
  begin
    Ctl:= TListView.Create(AForm);
    TListView(Ctl).ReadOnly:= true;
    TListView(Ctl).ColumnClick:= false;
    TListView(Ctl).ViewStyle:= vsReport;
    TListView(Ctl).RowSelect:= true;
    TListView(Ctl).HideSelection:= false;
    TListView(Ctl).Checkboxes:= (S='checklistview');
    TListView(Ctl).OnChange:= @AForm.DoOnListviewChange;
    TListView(Ctl).OnSelectItem:= @AForm.DoOnListviewSelect;
    exit;
  end;

  if (S='treeview') then
  begin
    Ctl:= TTreeViewMy.Create(AForm);
    DoApplyThemeToTreeview(TTreeViewMy(Ctl), false, true);
    TTreeViewMy(Ctl).BorderStyle:= bsSingle;
    TTreeViewMy(Ctl).Images:= TImageList.Create(AForm);
    TTreeViewMy(Ctl).OnChange:= @AForm.DoOnTreeviewChange;
    TTreeViewMy(Ctl).OnSelectionChanged:= @AForm.DoOnTreeviewSelect;
    TTreeViewMy(Ctl).OnCollapsing:= @AForm.DoOnTreeviewCollapsing;
    TTreeViewMy(Ctl).OnExpanding:= @AForm.DoOnTreeviewExpanding;
    exit
  end;

  if (S='listbox_ex') then
  begin
    Ctl:= TATListbox.Create(AForm);
    TATListbox(Ctl).ItemHeight:= GetListboxItemHeight(UiOps.VarFontName, UiOps.VarFontSize);;
    TATListbox(Ctl).CanGetFocus:= true;
    TATListbox(Ctl).OnChangedSel:= @AForm.DoOnChange;
    TATListbox(Ctl).OnDrawItem:= @AForm.DoOnListboxDrawItem;
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

  if S='pages' then
  begin
    Ctl:= TPageControl.Create(AForm);
    (Ctl as TPageControl).OnChange:= @AForm.DoOnChange;
    exit;
  end;

  if S='colorpanel' then
  begin
    Ctl:= TATPanelColor.Create(AForm);
    (Ctl as TATPanelColor).OnClick:= @AForm.DoOnChange;
    exit;
  end;

  if S='bevel' then
  begin
    Ctl:= TBevel.Create(AForm);
    (Ctl as TBevel).Shape:= bsFrame;
    exit;
  end;

  if S='image' then
  begin
    Ctl:= TImage.Create(AForm);
    (Ctl as TImage).Proportional:= true;
    (Ctl as TImage).AntialiasingMode:= amOn;
    exit;
  end;

  if S='trackbar' then
  begin
    Ctl:= TTrackBar.Create(AForm);
    (Ctl as TTrackBar).Min:= -1000000;
    (Ctl as TTrackBar).Max:= 1000000;
    (Ctl as TTrackBar).OnChange:= @AForm.DoOnChange;
    exit;
  end;

  if S='splitter' then
  begin
    Ctl:= TSplitter.Create(AForm);
    (Ctl as TSplitter).Beveled:= true;
    (Ctl as TSplitter).ResizeStyle:= rsPattern;
    (Ctl as TSplitter).AutoSnap:= false;
    (Ctl as TSplitter).OnMoved:= @AForm.DoOnChange;
    exit;
  end;

  if S='progressbar' then
  begin
    Ctl:= TProgressBar.Create(AForm);
    (Ctl as TProgressBar).Min:= -1000000;
    (Ctl as TProgressBar).Max:= 1000000;
    exit;
  end;

  if S='progressbar_ex' then
  begin
    Ctl:= TGauge.Create(AForm);
    (Ctl as TGauge).MinValue:= -1000000;
    (Ctl as TGauge).MaxValue:= 1000000;
    exit;
  end;

  if S='paintbox' then
  begin
    Ctl:= TPaintBox.Create(AForm);
    exit;
  end;

  if S='toolbar' then
  begin
    Ctl:= TATButtonsToolbar.Create(AForm);
    TATButtonsToolbar(Ctl).Images:= TImageList.Create(AForm);
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
     if SBeginsWith(S, 'button') then
       Props.FActive:= true;
     Ctl.Tag:= PtrInt(Props);

     if not Assigned(Ctl.OnClick) then
       Ctl.OnClick:= @AForm.DoOnClick;
     TControlHack(Ctl).OnDblClick:= @AForm.DoOnDblClick;
     TControlHack(Ctl).OnContextPopup:= @AForm.DoOnControlMenu;
   end;
 end;
end;


procedure DoControl_SetParentFromString(C: TControl; AValue: string);
var
  P: TControl;
  Form: TFormDummy;
  NPage: integer;
  SItem1, SItem2: string;
begin
  Form:= C.Owner as TFormDummy;

  //handle "name.N"
  SItem1:= SGetItem(AValue, '.');
  SItem2:= AValue;
  AValue:= SItem1;
  NPage:= StrToIntDef(SItem2, -1);

  P:= Form.FindControlByOurName(AValue);
  if P=nil then
    C.Parent:= Form
  else
  if P is TPageControl then
  begin
    if NPage>=0 then
      C.Parent:= TPageControl(P).Pages[NPage];
  end
  else
  if P is TWinControl then
    C.Parent:= P as TWinControl;
end;


procedure DoControl_SetAnchorFromString(C: TControl; AKind: TAnchorKind; AValue: string);
var
  CTo: TControl;
  SName, SSide: string;
  NSide: TAnchorSideReference;
  Form: TFormDummy;
begin
  if AValue='' then
  begin
    C.AnchorSide[AKind].Control:= nil;
    C.Anchors:= C.Anchors-[AKind];
    exit;
  end;

  SName:= SGetItem(AValue);
  SSide:= SGetItem(AValue);

  Form:= C.Owner as TFormDummy;
  if SName='' then
    CTo:= C.Parent
  else
    CTo:= Form.FindControlByOurName(SName);

  if SSide='[' then NSide:= asrLeft else
   if SSide=']' then NSide:= asrRight else
    NSide:= asrCenter;

  C.AnchorSide[AKind].Control:= CTo;
  C.AnchorSide[AKind].Side:= NSide;
  C.Anchors:= C.Anchors+[AKind];
end;


procedure DoControl_SetPropsFromString_Adv(C: TControl; S: string);
const
  cResizeStyle: array[boolean] of TResizeStyle = (rsPattern, rsUpdate);
begin
  if C is TButton then
  begin
    (C as TButton).Default:= AppStrToBool(SGetItem(S));
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
    if AppStrToBool(SGetItem(S)) then
    begin
      (C as TLabel).AutoSize:= false;
      (C as TLabel).Alignment:= taRightJustify;
    end;
    exit
  end;

  if (C is TEdit) or (C is TMemo) then
  begin
    //RO
    if AppStrToBool(SGetItem(S)) then
    begin
      (C as TCustomEdit).ReadOnly:= true;
      TCustomEditHack(C).ParentColor:= true;
    end;
    //Monospaced
    if AppStrToBool(SGetItem(S)) then
    begin
      C.Font.Name:= 'Courier New';
      {$ifdef windows}
      C.Font.Size:= 9;
      {$endif}
    end;
    //Border
    (C as TCustomEdit).BorderStyle:= cControlBorderStyles[AppStrToBool(SGetItem(S))];

    exit;
  end;

  if (C is TListView) then
  begin
    (C as TListView).GridLines:= AppStrToBool(SGetItem(S));
    exit
  end;

  if (C is TTabControl) then
  begin
    if AppStrToBool(S) then
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

  if (C is TBevel) then
  begin
    (C as TBevel).Shape:= TBevelShape(StrToIntDef(SGetItem(S), 1{bsFrame}));
    exit;
  end;

  if (C is TImage) then
  begin
    (C as TImage).Center:= AppStrToBool(SGetItem(S));
    (C as TImage).Stretch:= AppStrToBool(SGetItem(S));
    (C as TImage).StretchInEnabled:= AppStrToBool(SGetItem(S));
    (C as TImage).StretchOutEnabled:= AppStrToBool(SGetItem(S));
    (C as TImage).KeepOriginXWhenClipped:= AppStrToBool(SGetItem(S));
    (C as TImage).KeepOriginYWhenClipped:= AppStrToBool(SGetItem(S));
    exit
  end;

  if (C is TTrackBar) then
  begin
    (C as TTrackBar).Orientation:= TTrackBarOrientation(StrToIntDef(SGetItem(S), 0));
    (C as TTrackBar).Min:= StrToIntDef(SGetItem(S), 0);
    (C as TTrackBar).Max:= StrToIntDef(SGetItem(S), 100);
    (C as TTrackBar).LineSize:= StrToIntDef(SGetItem(S), 1);
    (C as TTrackBar).PageSize:= StrToIntDef(SGetItem(S), 10);
    (C as TTrackBar).Reversed:= AppStrToBool(SGetItem(S));
    (C as TTrackBar).TickMarks:= TTickMark(StrToIntDef(SGetItem(S), 0));
    (C as TTrackBar).TickStyle:= TTickStyle(StrToIntDef(SGetItem(S), 0));
    exit;
  end;

  if (C is TProgressBar) then
  begin
    (C as TProgressBar).Orientation:= TProgressBarOrientation(StrToIntDef(SGetItem(S), 0));
    (C as TProgressBar).Min:= StrToIntDef(SGetItem(S), 0);
    (C as TProgressBar).Max:= StrToIntDef(SGetItem(S), 100);
    (C as TProgressBar).Smooth:= AppStrToBool(SGetItem(S));
    (C as TProgressBar).Step:= StrToIntDef(SGetItem(S), 1);
    (C as TProgressBar).Style:= TProgressBarStyle(StrToIntDef(SGetItem(S), 0));
    (C as TProgressBar).BarShowText:= AppStrToBool(SGetItem(S));
    exit;
  end;

  if (C is TGauge) then
  begin
    (C as TGauge).Kind:= TGaugeKind(StrToIntDef(SGetItem(S), 0));
    (C as TGauge).MinValue:= StrToIntDef(SGetItem(S), 0);
    (C as TGauge).MaxValue:= StrToIntDef(SGetItem(S), 100);
    (C as TGauge).ShowText:= AppStrToBool(SGetItem(S));
    (C as TGauge).BackColor:= StrToIntDef(SGetItem(S), clWhite);
    (C as TGauge).ForeColor:= StrToIntDef(SGetItem(S), clNavy);
    (C as TGauge).BorderColor:= StrToIntDef(SGetItem(S), clBlack);
    exit;
  end;

  if (C is TSplitter) then
  begin
    (C as TSplitter).Beveled:= AppStrToBool(SGetItem(S));
    (C as TSplitter).ResizeStyle:= cResizeStyle[AppStrToBool(SGetItem(S))];
    (C as TSplitter).AutoSnap:= AppStrToBool(SGetItem(S));
    (C as TSplitter).MinSize:= StrToIntDef(SGetItem(S), (C as TSplitter).MinSize);
    exit;
  end;

  if (C is TListViewFilterEdit) then
  begin
    (C as TListViewFilterEdit).ByAllFields:= AppStrToBool(SGetItem(S));
    exit
  end;
end;


procedure DoControl_SetItemsFromString(C: TControl; S: string);
var
  SItem: string;
  i: integer;
begin
  if C is TImage then
  begin
    DoControl_SetState_Image(C as TImage, S);
    exit
  end;

  if C is TListbox then (C as TListbox).Items.Clear;
  if C is TComboBox then (C as TComboBox).Items.Clear;
  if C is TCheckGroup then (C as TCheckGroup).Items.Clear;
  if C is TRadioGroup then (C as TRadioGroup).Items.Clear;
  if C is TCheckListBox then (C as TCheckListBox).Items.Clear;
  if C is TTabControl then (C as TTabControl).Tabs.Clear;

  if C is TListView then
  begin
    (C as TListView).Columns.Clear;
    (C as TListView).Items.Clear;
  end;

  if C is TPageControl then
  begin
    for i:= TPageControl(C).PageCount-1 downto 0 do
      TPageControl(C).Pages[i].Free;
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
    if C is TPageControl then (C as TPageControl).AddTabSheet.Caption:= SItem;
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
    (C as TToggleBox).Checked:= AppStrToBool(S);
    exit
  end;
  if C is TRadioButton then
  begin
    (C as TRadioButton).Checked:= AppStrToBool(S);
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
  if C is TTrackBar then
  begin
    (C as TTrackBar).Position:= StrToIntDef(S, 0);
    exit
  end;
  if C is TProgressBar then
  begin
    (C as TProgressBar).Position:= StrToIntDef(S, 0);
    exit
  end;
  if C is TGauge then
  begin
    (C as TGauge).Progress:= StrToIntDef(S, 0);
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
    TAppControlProps(C.Tag).FName:= AValue;
    exit;
  end;

  if AName='cap' then
  begin
    C.Caption:= AValue;
    exit;
  end;

  if AName='p' then
  begin
    DoControl_SetParentFromString(C, AValue);
    exit;
  end;

  if AName='en' then
  begin
    C.Enabled:= AppStrToBool(AValue);
    exit;
  end;

  if AName='vis' then
  begin
    C.Visible:= AppStrToBool(AValue);
    exit;
  end;

  if AName='hint' then
  begin
    DoControl_SetHintFromString(C, AValue);
    exit;
  end;

  if AName='act' then
  begin
    TAppControlProps(C.Tag).FActive:= AppStrToBool(AValue);
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
    C.Width:= StrToIntDef(AValue, C.Width);
    exit;
  end;
  if AName='h' then
  begin
    C.Height:= StrToIntDef(AValue, C.Height);
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

  if AName='border' then
  begin
    if C is TWinControl then
      TWinControlHack(C).BorderStyle:= cControlBorderStyles[AppStrToBool(AValue)];
    exit;
  end;

  if AName='on_change' then
  begin
    TAppControlProps(C.Tag).FEventOnChange:= AValue;
    exit;
  end;

  if AName='on_select' then
  begin
    TAppControlProps(C.Tag).FEventOnSelect:= AValue;
    exit;
  end;

  if AName='on_fold' then
  begin
    TAppControlProps(C.Tag).FEventOnFold:= AValue;
    exit;
  end;

  if AName='on_unfold' then
  begin
    TAppControlProps(C.Tag).FEventOnUnfold:= AValue;
    exit;
  end;

  if AName='on_menu' then
  begin
    TAppControlProps(C.Tag).FEventOnMenu:= AValue;
    exit;
  end;

  if AName='on_click' then
  begin
    TAppControlProps(C.Tag).FEventOnClick:= AValue;
    exit;
  end;

  if AName='on_click_dbl' then
  begin
    TAppControlProps(C.Tag).FEventOnClickDbl:= AValue;
    exit;
  end;

  if AName='on_draw_item' then
  begin
    TAppControlProps(C.Tag).FEventOnListboxDrawItem:= AValue;
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

  if AName='tab_stop' then
  begin
    if C is TWinControl then
      (C as TWinControl).TabStop:= AppStrToBool(AValue);
    exit;
  end;

  if AName='tab_order' then
  begin
    if C is TWinControl then
      (C as TWinControl).TabOrder:= StrToIntDef(AValue, -1);
    exit;
  end;

  if AName='align' then
  begin
    C.Align:= TAlign(StrToIntDef(AValue, 0));
    exit;
  end;

  if AName='sp_l' then
  begin
    C.BorderSpacing.Left:= StrToIntDef(AValue, 0);
    exit;
  end;

  if AName='sp_r' then
  begin
    C.BorderSpacing.Right:= StrToIntDef(AValue, 0);
    exit;
  end;

  if AName='sp_t' then
  begin
    C.BorderSpacing.Top:= StrToIntDef(AValue, 0);
    exit;
  end;

  if AName='sp_b' then
  begin
    C.BorderSpacing.Bottom:= StrToIntDef(AValue, 0);
    exit;
  end;

  if AName='sp_a' then
  begin
    C.BorderSpacing.Around:= StrToIntDef(AValue, 0);
    exit;
  end;

  if AName='a_l' then
  begin
    DoControl_SetAnchorFromString(C, akLeft, AValue);
    exit;
  end;

  if AName='a_r' then
  begin
    DoControl_SetAnchorFromString(C, akRight, AValue);
    exit;
  end;

  if AName='a_t' then
  begin
    DoControl_SetAnchorFromString(C, akTop, AValue);
    exit;
  end;

  if AName='a_b' then
  begin
    DoControl_SetAnchorFromString(C, akBottom, AValue);
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


procedure DoForm_AdjustLabelForNewControl(F: TForm; Ctl: TControl);
var
  CtlPrev: TControl;
begin
  if Ctl is TWinControl then
    if F.ControlCount>=2 then
    begin
      CtlPrev:= F.Controls[F.ControlCount-2];
      if CtlPrev is TLabel then
        (CtlPrev as TLabel).FocusControl:= Ctl as TWinControl;
    end;
end;


procedure DoForm_AddControl(AForm: TFormDummy; ATextItems: string);
var
  SNameValue, SName, SValue: string;
  Ctl: TControl;
begin
  Ctl:= nil;

  repeat
    SNameValue:= SGetItem(ATextItems, #1);
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

    if not Assigned(Ctl) then exit;
    DoForm_AdjustLabelForNewControl(AForm, Ctl);
    DoControl_SetPropFromPair(Ctl, SName, SValue);

  until false;
end;


procedure DoForm_FocusControl(F: TForm; C: TControl);
begin
  if C.Enabled and C.Visible then
    if C is TWinControl then
      F.ActiveControl:= C as TWinControl;
end;

procedure DoForm_FocusControl(F: TForm; AIndex: integer);
begin
  if (AIndex>=0) and (AIndex<F.ControlCount) then
    DoForm_FocusControl(F, F.Controls[AIndex]);
end;


procedure DoForm_ScaleAuto(F: TForm);
begin
  {$ifdef windows}
  //Linux: cannot test
  //macOS: gives bad result, tool big labels

  F.AutoAdjustLayout(lapAutoAdjustForDPI,
    96, Screen.PixelsPerInch,
    F.Width, F.Scale96ToForm(F.Width));
  {$endif}
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
    F.Scaled:= true; //safe for dlg_custom

    DoForm_FillContent(F, AText);
    DoForm_FocusControl(F, AFocusedIndex);
    DoForm_ScaleAuto(F);

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
  if Id='combo_ro' then begin C:= TComboBox.Create(nil); TCombobox(C).Style:= csDropDownList; end else
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
  if AName='tag' then
    F.TagString:= AValue
  else
  if AName='resize' then
  begin
    if AppStrToBool(AValue) then
      F.BorderStyle:= bsSizeable
    else
      F.BorderStyle:= bsDialog;
  end
  else
  if AName='topmost' then
  begin
    if AppStrToBool(AValue) then
      F.FormStyle:= fsStayOnTop
    else
      F.FormStyle:= fsNormal;
  end
  else
  if AName='on_resize' then
    F.FEventOnResize:= AValue
  else
  if AName='on_close' then
    F.FEventOnClose:= AValue
  else
  if AName='on_close_query' then
    F.FEventOnCloseQuery:= AValue
  else
  if AName='on_key_down' then
    F.FEventOnKeyDown:= AValue
  else
  if AName= 'on_key_up' then
    F.FEventOnKeyUp:= AValue
  else
  if AName='vis' then
    F.Visible:= AppStrToBool(AValue)
  else
  if AName='keypreview' then
    F.KeyPreview:= AppStrToBool(AValue)
  else
  if AName='color' then
    F.Color:= StrToIntDef(AValue, F.Color)
  else
  if AName='autosize' then
    F.AutoSize:= AppStrToBool(AValue)
  else
  if AName='border' then
  begin
    if AppStrToBool(AValue) then
      F.BorderStyle:= bsSizeable
    else
      F.BorderStyle:= bsNone;
  end
  else;
end;


function DoForm_GetPropsAsStringDict(F: TFormDummy): PPyObject;
begin
  with GetPythonEngine do
  begin
    Result:= Py_BuildValue('{sssssisisisisisisOsOsOsO}',
      'cap', PChar(F.Caption),
      'tag', PChar(F.TagString),
      PChar(string('x')), F.Left,
      PChar(string('y')), F.Top,
      PChar(string('w')), F.Width,
      PChar(string('h')), F.Height,
      'clicked', F.IdClicked,
      'focused', F.IdFocused,
      'vis', PyBool_FromLong(Ord(F.Visible)),
      'resize', PyBool_FromLong(Ord(F.BorderStyle=bsSizeable)),
      'topmost', PyBool_FromLong(Ord(F.FormStyle=fsStayOnTop)),
      'keypreview', PyBool_FromLong(Ord(F.KeyPreview))
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
    SItem:= SGetItem(AText, #1);
    if SItem='' then Break;
    SKey:= SGetItem(SItem, ':');
    SValue:= SItem;
    SValue:= StringReplace(SValue, #2, ',', [rfReplaceAll]);
    DoForm_SetPropFromPair(F, SKey, SValue);
  until false;
end;


function DoControl_GetPropsAsStringDict(C: TControl): PPyObject;
var
  bTabStop: boolean;
  nTabOrder: integer;
begin
  bTabStop:= false;
  nTabOrder:= -1;
  if C is TWinControl then
  begin
    bTabStop:= (C as TWinControl).TabStop;
    nTabOrder:= (C as TWinControl).TabOrder;
  end;

  with GetPythonEngine do
  begin
    //is it docked form?
    if C.Tag=0 then
      exit(ReturnNone);

    Result:= Py_BuildValue('{sssssssssssisisisisssOsOsOsOsisisisisisi}',
      'name', PChar(TAppControlProps(C.Tag).FName),
      'cap', PChar(C.Caption),
      'hint', PChar(C.Hint),
      'type', PChar(TAppControlProps(C.Tag).FTypeString),
      'tag', PChar(TAppControlProps(C.Tag).FTagString),
      PChar(string('x')), C.Left,
      PChar(string('y')), C.Top,
      PChar(string('w')), C.Width,
      PChar(string('h')), C.Height,
      'val', PChar(DoControl_GetState(C)),
      'act', PyBool_FromLong(Ord(TAppControlProps(C.Tag).FActive)),
      'en', PyBool_FromLong(Ord(C.Enabled)),
      'vis', PyBool_FromLong(Ord(C.Visible)),
      'tab_stop', PyBool_FromLong(Ord(bTabStop)),
      'tab_order', nTabOrder,
      'sp_l', C.BorderSpacing.Left,
      'sp_r', C.BorderSpacing.Right,
      'sp_t', C.BorderSpacing.Top,
      'sp_b', C.BorderSpacing.Bottom,
      'sp_a', C.BorderSpacing.Around
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
    SItem:= SGetItem(AText, #1);
    if SItem='' then Break;
    SKey:= SGetItem(SItem, ':');
    SValue:= SItem;
    SValue:= StringReplace(SValue, #2, ',', [rfReplaceAll]);
    DoControl_SetPropFromPair(C, SKey, SValue);
  until false;
end;


procedure DoForm_CloseDockedForms(F: TForm);
var
  C: TControl;
  i: integer;
begin
  for i:= F.ControlCount-1 downto 0 do
  begin
    C:= F.Controls[i];
    if C is TForm then
    begin
      showmessage(C.Caption); //dont run, so code not needed
      (C as TForm).Parent:= nil;
      (C as TForm).Close;
    end;
  end;
end;


end.

