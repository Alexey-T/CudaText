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
  LclIntf, LclProc, LclType,
  ATListbox,
  ATButtons,
  ATFlatToolbar,
  ATLinkLabel,
  ATStringProc,
  ATPanelColor,
  ATGauge,
  ATStatusBar,
  ATSynEdit,
  ATSynEdit_Adapter_EControl,
  proc_customdialog_dummy,
  proc_miscutils,
  proc_globdata,
  proc_editor,
  proc_scrollbars,
  proc_lexer_styles,
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
procedure DoForm_AdjustLabelForNewControl(F: TForm; Ctl: TControl);
procedure DoForm_FocusControl(F: TForm; C: TControl);
procedure DoForm_ScaleAuto(F: TForm; ASimpleResize: boolean=false);
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


function DoControl_Target(C: TControl): TControl; inline;
begin
  if C is TAppTreeContainer then
    Result:= TAppTreeContainer(C).Tree
  else
    Result:= C;
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
  C.Height:= 23;
  {$endif}

  {$ifdef linux}
  C.Height:= 25;
  {$endif}

  {$ifdef LCLCarbon}
  C.Height:= 21;
  {$endif}

  {$ifdef LCLCocoa}
  C.Height:= 32;
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
var
  L: TStringList;
begin
  L:= TStringList.Create;
  try
    L.SkipLastLineBreak:= true;
    L.Assign(C.Lines);
    Result:= L.Text;
  finally
    L.Free;
  end;

  Result:= StringReplace(Result, #9, #3, [rfReplaceAll]);
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
    exit(TEdit(C).Text);

  if C is TCheckBox then
    exit(DoControl_GetState_CheckBox(TCheckbox(C)));

  if C is TToggleBox then
    exit(IntToStr(Ord(TToggleBox(C).Checked)));

  if C is TRadioButton then
    exit(IntToStr(Ord(TRadioButton(C).Checked)));

  if C is TListBox then
    exit(IntToStr(TListBox(C).ItemIndex));

  if C is TComboBox then
  begin
    if TComboBox(C).Style=csDropDownList then
      exit(IntToStr(TComboBox(C).ItemIndex))
    else
      exit(TComboBox(C).Text);
  end;

  if C is TMemo then
    exit(DoControl_GetState_Memo(TMemo(C)));

  if C is TRadioGroup then
    exit(IntToStr(TRadioGroup(C).ItemIndex));

  if C is TCheckGroup then
    exit(DoControl_GetState_CheckGroup(TCheckGroup(C)));

  if C is TCheckListBox then
    exit(DoControl_GetState_CheckListBox(TCheckListBox(C)));

  if C is TSpinEdit then
    exit(IntToStr(TSpinEdit(C).Value));

  if C is TListView then
    exit(DoControl_GetState_Listview(TListView(C)));

  if C is TTabControl then
    exit(IntToStr(TTabControl(C).TabIndex));

  if C is TPageControl then
    exit(IntToStr(TPageControl(C).PageIndex));

  if C is TTrackBar then
    exit(IntToStr(TTrackBar(C).Position));

  if C is TProgressBar then
    exit(IntToStr(TProgressBar(C).Position));

  if C is TATGauge then
    exit(IntToStr(TATGauge(C).Progress));

  if C is TListFilterEdit then
    exit(TListFilterEdit(C).Text);

  if C is TListViewFilterEdit then
    exit(TListViewFilterEdit(C).Text);
end;


procedure DoControl_SetState_Memo(C: TMemo; AValue: string);
begin
  AValue:= StringReplace(AValue, #9, #10, [rfReplaceAll]);
  AValue:= StringReplace(AValue, #3, #9, [rfReplaceAll]);

  C.Lines.TextLineBreakStyle:= tlbsLF;
  C.Lines.Text:= AValue;
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


function DoControl_GetItems_Listbox(C: TCustomListbox): string;
var
  L: TStringList;
begin
  L:= TStringList.Create;
  try
    L.Assign(C.Items);
    L.LineBreak:= #9;
    Result:= L.Text;
  finally
    FreeAndNil(L);
  end;
end;


function DoControl_GetItems_ListView(C: TListView): string;
var
  L: TStringList;
  SItem: string;
  i, j: integer;
begin
  Result:= '';
  if C.Items.Count=0 then exit;

  L:= TStringList.Create;
  try
    for i:= 0 to C.Items.Count-1 do
    begin
      SItem:= C.Items[i].Caption;
      for j:= 0 to C.Items[i].SubItems.Count-1 do
        SItem:= SItem+#13+C.Items[i].SubItems[j];
      L.Add(SItem);
    end;

    L.LineBreak:= #9;
    Result:= L.Text;
  finally
    FreeAndNil(L);
  end;
end;


function DoControl_GetColumns_ListView(C: TListView): string;
const
  cAlign: array[TAlignment] of string = ('L', 'R', 'C');
  cBool: array[boolean] of string = ('0', '1');
var
  L: TStringList;
  i: integer;
begin
  Result:= '';
  L:= TStringList.Create;
  try
    L.LineBreak:= #9;
    L.SkipLastLineBreak:= true;
    for i:= 0 to C.ColumnCount-1 do
      L.Add(
        C.Column[i].Caption + #13 +
        IntToStr(C.Column[i].Width) + #13 +
        IntToStr(C.Column[i].MinWidth) + #13 +
        IntToStr(C.Column[i].MaxWidth) + #13 +
        cAlign[C.Column[i].Alignment] + #13 +
        cBool[C.Column[i].AutoSize] + #13 +
        cBool[C.Column[i].Visible]
        );
    Result:= L.Text;
  finally
    FreeAndNil(L);
  end;
end;

function SGetCharCount(const S: string; ch: char): integer;
var
  i: integer;
begin
  Result:= 0;
  for i:= 1 to Length(S) do
    if S[i]=ch then Inc(Result);
end;

procedure DoControl_SetColumns_ListView(C: TListView; AValue: string);
var
  Column: TListColumn;
  SCol, SItem: string;
  NCount, i: integer;
begin
  NCount:= SGetCharCount(AValue, #9)+1;
  for i:= 0 to NCount-1 do
  begin
    SCol:= SGetItem(AValue, #9);
    if i<C.Columns.Count then
      Column:= C.Columns[i]
    else
      Column:= C.Columns.Add;

    Column.Caption:= SGetItem(SCol, #13);
    Column.Width:= StrToIntDef(SGetItem(SCol, #13), 100);
    Column.MinWidth:= StrToIntDef(SGetItem(SCol, #13), 0);
    Column.MaxWidth:= StrToIntDef(SGetItem(SCol, #13), 0);

    SItem:= SGetItem(SCol, #13);
    if SItem<>'' then
      case SItem of
        'L': Column.Alignment:= taLeftJustify;
        'R': Column.Alignment:= taRightJustify;
        'C': Column.Alignment:= taCenter;
      end;

    SItem:= SGetItem(SCol, #13);
    if SItem<>'' then
      Column.AutoSize:= AppStrToBool(SItem);

    SItem:= SGetItem(SCol, #13);
    if SItem<>'' then
      Column.Visible:= AppStrToBool(SItem);
  end;
end;


function DoControl_GetItems(C: TControl): string;
begin
  Result:= '';
  if C is TCustomListbox then
    exit(DoControl_GetItems_Listbox(TCustomListbox(C)));
  if C is TListView then
    exit(DoControl_GetItems_ListView(TListView(C)));
end;


function DoControl_GetColumns(C: TControl): string;
begin
  Result:= '';

  if C is TListView then
    exit(DoControl_GetColumns_ListView(TListView(C)));

  if C is TRadioGroup then
    exit(IntToStr(TRadioGroup(C).Columns));
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

procedure DoControl_SetState_PageControl(C: TPageControl; const SValue: string);
var
  N: integer;
begin
  N:= StrToIntDef(SValue, -1);
  if (N>=0) and (N<C.PageCount) then
    C.PageIndex:= N;
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
    if Assigned(C.ItemFocused) then
      C.ItemFocused.MakeVisible(false);
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
  Adapter: TATAdapterEControl;
begin
  Ctl:= nil;

 try
  if S='check' then
  begin
    Ctl:= TCheckBox.Create(AForm);
    TCheckBox(Ctl).OnChange:= @AForm.DoOnChange;
    exit;
  end;

  if S='edit' then
  begin
    Ctl:= TEdit.Create(AForm);
    TEdit(Ctl).OnChange:= @AForm.DoOnChange;
    exit;
  end;

  if S='editor' then
  begin
    Ctl:= TATSynEdit.Create(AForm);

    TATSynEdit(Ctl).OnChange:= @AForm.DoOnEditorChange;
    TATSynEdit(Ctl).OnChangeCaretPos:= @AForm.DoOnEditorChangeCaretPos;
    TATSynEdit(Ctl).OnKeyDown:= @AForm.DoOnEditorKeyDown;
    TATSynEdit(Ctl).OnKeyUp:= @AForm.DoOnEditorKeyUp;
    TATSynEdit(Ctl).OnClickGutter:= @AForm.DoOnEditorClickGutter;
    TATSynEdit(Ctl).OnClickGap:= @AForm.DoOnEditorClickGap;
    TATSynEdit(Ctl).OnScroll:= @AForm.DoOnEditorScroll;
    TATSynEdit(Ctl).OnPaste:= @AForm.DoOnEditorPaste;

    EditorApplyTheme(TATSynEdit(Ctl));
    EditorApplyOps(TATSynEdit(Ctl), EditorOps, true, true, false);

    Adapter:= TATAdapterEControl.Create(Ctl);
    Adapter.DynamicHiliteEnabled:= EditorOps.OpLexerDynamicHiliteEnabled;
    Adapter.DynamicHiliteMaxLines:= EditorOps.OpLexerDynamicHiliteMaxLines;
    Adapter.AddEditor(TATSynEdit(Ctl));

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
    TMemo(Ctl).WordWrap:= false;
    TMemo(Ctl).ScrollBars:= ssBoth;
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
    TComboBox(Ctl).OnChange:= @AForm.DoOnChange;
    TComboBox(Ctl).DropDownCount:= 20;
    exit;
  end;

  if S='combo_ro' then
  begin
    Ctl:= TComboBox.Create(AForm);
    TComboBox(Ctl).DropDownCount:= 20;
    TComboBox(Ctl).Style:= csDropDownList;
    TComboBox(Ctl).OnChange:= @AForm.DoOnChange;
    exit;
  end;

  if S='button' then
  begin
    Ctl:= TButton.Create(AForm);
    TButton(Ctl).OnClick:= @AForm.DoOnChange;
    DoControl_FixButtonHeight(Ctl);
    exit;
  end;

  if S='button_ex' then
  begin
    Ctl:= TATButton.Create(AForm);
    TATButton(Ctl).OnClick:= @AForm.DoOnChange;
    exit;
  end;

  if S='checkbutton' then
  begin
    Ctl:= TToggleBox.Create(AForm);
    TToggleBox(Ctl).OnChange:= @AForm.DoOnChange;
    DoControl_FixButtonHeight(Ctl);
    exit;
  end;

  if S='listbox' then
  begin
    Ctl:= TListBox.Create(AForm);
    TListBox(Ctl).OnSelectionChange:= @AForm.DoOnListboxSelect;
    exit;
  end;

  if S='radio' then
  begin
    Ctl:= TRadioButton.Create(AForm);
    TRadioButton(Ctl).OnChange:= @AForm.DoOnChange;
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
    TPanel(Ctl).BevelInner:= bvNone;
    TPanel(Ctl).BevelOuter:= bvNone;
    TPanel(Ctl).BorderStyle:= bsNone;
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
    TCheckListBox(Ctl).OnSelectionChange:= @AForm.DoOnListboxSelect;
    TCheckListBox(Ctl).OnClickCheck:= @AForm.DoOnChange;
    exit;
  end;

  if (S='listview') or
     (S='checklistview') then
  begin
    Ctl:= TListView.Create(AForm);
    TListView(Ctl).ReadOnly:= true;
    TListView(Ctl).ColumnClick:= true;
    TListView(Ctl).ViewStyle:= vsReport;
    TListView(Ctl).RowSelect:= true;
    TListView(Ctl).HideSelection:= false;
    TListView(Ctl).Checkboxes:= (S='checklistview');
    TListView(Ctl).OnChange:= @AForm.DoOnListviewChange;
    TListView(Ctl).OnSelectItem:= @AForm.DoOnListviewSelect;
    TListView(Ctl).OnColumnClick:= @AForm.DoOnListviewColumnClick;
    exit;
  end;

  if (S='treeview') then
  begin
    Ctl:= TAppTreeContainer.Create(AForm);
    DoApplyThemeToTreeview(TAppTreeContainer(Ctl).Tree, false, true);
    TAppTreeContainer(Ctl).Tree.BorderStyle:= bsSingle;
    TAppTreeContainer(Ctl).Tree.Images:= TImageList.Create(Ctl);
    TAppTreeContainer(Ctl).Tree.OnChange:= @AForm.DoOnTreeviewChange;
    TAppTreeContainer(Ctl).Tree.OnSelectionChanged:= @AForm.DoOnTreeviewSelect;
    TAppTreeContainer(Ctl).Tree.OnCollapsing:= @AForm.DoOnTreeviewCollapsing;
    TAppTreeContainer(Ctl).Tree.OnExpanding:= @AForm.DoOnTreeviewExpanding;
    TAppTreeContainer(Ctl).Tree.DefaultItemHeight:= AppScale(DefaultTreeNodeHeight);
    TAppTreeContainer(Ctl).Invalidate;
    exit
  end;

  if (S='listbox_ex') then
  begin
    Ctl:= TATListbox.Create(AForm);
    TATListbox(Ctl).VirtualMode:= false;
    TATListbox(Ctl).CanGetFocus:= true;
    TATListbox(Ctl).OnChangedSel:= @AForm.DoOnChange;
    TATListbox(Ctl).OnDrawItem:= @AForm.DoOnListboxDrawItem;
    exit;
  end;

  if S='linklabel' then
  begin
    Ctl:= TATLabelLink.Create(AForm);
    exit;
  end;

  if S='tabs' then
  begin
    Ctl:= TTabControl.Create(AForm);
    TTabControl(Ctl).OnChange:= @AForm.DoOnChange;
    exit;
  end;

  if S='pages' then
  begin
    Ctl:= TPageControl.Create(AForm);
    TPageControl(Ctl).OnChange:= @AForm.DoOnChange;
    exit;
  end;

  if S='colorpanel' then
  begin
    Ctl:= TATPanelColor.Create(AForm);
    TATPanelColor(Ctl).OnClick:= @AForm.DoOnChange;
    exit;
  end;

  if S='bevel' then
  begin
    Ctl:= TBevel.Create(AForm);
    TBevel(Ctl).Shape:= bsFrame;
    exit;
  end;

  if S='image' then
  begin
    Ctl:= TImage.Create(AForm);
    TImage(Ctl).Proportional:= true;
    TImage(Ctl).AntialiasingMode:= amOn;
    TImage(Ctl).OnPaintBackground:= @AForm.DoOnImagePaintBackground;
    exit;
  end;

  if S='trackbar' then
  begin
    Ctl:= TTrackBar.Create(AForm);
    TTrackBar(Ctl).Min:= -1000000;
    TTrackBar(Ctl).Max:= 1000000;
    TTrackBar(Ctl).OnChange:= @AForm.DoOnChange;
    exit;
  end;

  if S='splitter' then
  begin
    Ctl:= TSplitter.Create(AForm);
    TSplitter(Ctl).Beveled:= true;
    TSplitter(Ctl).ResizeStyle:= rsPattern;
    TSplitter(Ctl).AutoSnap:= false;
    TSplitter(Ctl).OnMoved:= @AForm.DoOnChange;
    exit;
  end;

  if S='progressbar' then
  begin
    Ctl:= TProgressBar.Create(AForm);
    TProgressBar(Ctl).Min:= -1000000;
    TProgressBar(Ctl).Max:= 1000000;
    exit;
  end;

  if S='progressbar_ex' then
  begin
    Ctl:= TATGauge.Create(AForm);
    TATGauge(Ctl).MinValue:= -1000000;
    TATGauge(Ctl).MaxValue:= 1000000;
    exit;
  end;

  if S='paintbox' then
  begin
    Ctl:= TPaintBox.Create(AForm);
    exit;
  end;

  if S='toolbar' then
  begin
    Ctl:= TATFlatToolbar.Create(AForm);
    TATFlatToolbar(Ctl).Images:= TImageList.Create(Ctl);
    exit;
  end;

  if S='statusbar' then
  begin
    Ctl:= TATStatus.Create(AForm);
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

     //treeview is a special case, because it's inside TAppTreeContainer
     if Ctl is TAppTreeContainer then
     begin
       TAppTreeContainer(Ctl).Tree.Tag:= Ctl.Tag;
       TAppTreeContainer(Ctl).Tree.OnClick:= @AForm.DoOnClick;
       TAppTreeContainer(Ctl).Tree.OnDblClick:= @AForm.DoOnDblClick;
       TAppTreeContainer(Ctl).Tree.OnContextPopup:= @AForm.DoOnControlMenu;
       TAppTreeContainer(Ctl).Tree.OnEnter:= @AForm.DoOnControlFocusEnter;
       TAppTreeContainer(Ctl).Tree.OnExit:= @AForm.DoOnControlFocusExit;
       TAppTreeContainer(Ctl).Tree.OnMouseEnter:= @AForm.DoOnControlMouseEnter;
       TAppTreeContainer(Ctl).Tree.OnMouseLeave:= @AForm.DoOnControlMouseLeave;
       TAppTreeContainer(Ctl).Tree.OnMouseDown:= @AForm.DoOnControlMouseDown;
       TAppTreeContainer(Ctl).Tree.OnMouseUp:= @AForm.DoOnControlMouseUp;
     end
     else
     begin
       //for some controls, OnClick already set to another handler
       if not Assigned(Ctl.OnClick) then
         Ctl.OnClick:= @AForm.DoOnClick;
       TControlHack(Ctl).OnDblClick:= @AForm.DoOnDblClick;
       TControlHack(Ctl).OnContextPopup:= @AForm.DoOnControlMenu;

       TControlHack(Ctl).OnMouseEnter:= @AForm.DoOnControlMouseEnter;
       TControlHack(Ctl).OnMouseLeave:= @AForm.DoOnControlMouseLeave;
       TControlHack(Ctl).OnMouseDown:= @AForm.DoOnControlMouseDown;
       TControlHack(Ctl).OnMouseUp:= @AForm.DoOnControlMouseUp;

       if Ctl is TWinControl then
       begin
         TWinControl(Ctl).OnEnter:= @AForm.DoOnControlFocusEnter;
         TWinControl(Ctl).OnExit:= @AForm.DoOnControlFocusExit;
       end;
     end;
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
    C.Parent:= TWinControl(P);
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


procedure DoControl_SetEx(C: TControl; const S: string; AIndex: integer);
const
  cResizeStyle: array[boolean] of TResizeStyle = (rsPattern, rsUpdate);
begin
  if C is TButton then
  begin
    case AIndex of
      0: TButton(C).Default:= AppStrToBool(S);
    end;
    exit
  end;

  if C is TSpinEdit then
  begin
    case AIndex of
      0: TSpinEdit(C).MinValue:= StrToIntDef(S, 0);
      1: TSpinEdit(C).MaxValue:= StrToIntDef(S, 100);
      2: TSpinEdit(C).Increment:= StrToIntDef(S, 1);
    end;
    exit
  end;

  if C is TATLabelLink then
  begin
    case AIndex of
      0: TATLabelLink(C).Link:= S;
    end;
    exit
  end;

  if C is TLabel then
  begin
    case AIndex of
      0:
        begin
          if AppStrToBool(S) then
          begin
            TLabel(C).AutoSize:= false;
            TLabel(C).Alignment:= taRightJustify;
          end;
        end;
    end;
    exit
  end;

  if (C is TEdit) or (C is TMemo) then
  begin
    case AIndex of
      0: //Read-only
        begin
          TCustomEdit(C).ReadOnly:= AppStrToBool(S);
          TCustomEditHack(C).ParentColor:= AppStrToBool(S);
        end;
      1: //Monospaced
        begin
          if AppStrToBool(S) then
          begin
            C.Font.Name:= 'Courier New';
            {$ifdef windows}
            C.Font.Size:= 9;
            {$endif}
          end;
        end;
      2: //Border
        begin
          TCustomEdit(C).BorderStyle:= cControlBorderStyles[AppStrToBool(S)];
        end;
    end;
    exit;
  end;

  if (C is TListView) then
  begin
    case AIndex of
      0: TListView(C).GridLines:= AppStrToBool(S);
    end;
    exit
  end;

  if (C is TTabControl) then
  begin
    case AIndex of
      0:
        begin
          if AppStrToBool(S) then
            TTabControl(C).TabPosition:= tpBottom;
        end;
    end;
    exit
  end;

  if (C is TATPanelColor) then
  begin
    case AIndex of
      0: TATPanelColor(C).BorderWidth:= StrToIntDef(S, 0);
      1: TATPanelColor(C).Color:= StrToIntDef(S, clDefault);
      2: TATPanelColor(C).Font.Color:= StrToIntDef(S, clDefault);
      3: TATPanelColor(C).BorderColor:= StrToIntDef(S, clBlack);
    end;
    exit
  end;

  if (C is TBevel) then
  begin
    case AIndex of
      0: TBevel(C).Shape:= TBevelShape(StrToIntDef(S, 1{bsFrame}));
    end;
    exit;
  end;

  if (C is TImage) then
  begin
    case AIndex of
      0: TImage(C).Center:= AppStrToBool(S);
      1: TImage(C).Stretch:= AppStrToBool(S);
      2: TImage(C).StretchInEnabled:= AppStrToBool(S);
      3: TImage(C).StretchOutEnabled:= AppStrToBool(S);
      4: TImage(C).KeepOriginXWhenClipped:= AppStrToBool(S);
      5: TImage(C).KeepOriginYWhenClipped:= AppStrToBool(S);
    end;
    exit
  end;

  if (C is TTrackBar) then
  begin
    case AIndex of
      0: TTrackBar(C).Orientation:= TTrackBarOrientation(StrToIntDef(S, 0));
      1: TTrackBar(C).Min:= StrToIntDef(S, 0);
      2: TTrackBar(C).Max:= StrToIntDef(S, 100);
      3: TTrackBar(C).LineSize:= StrToIntDef(S, 1);
      4: TTrackBar(C).PageSize:= StrToIntDef(S, 10);
      5: TTrackBar(C).Reversed:= AppStrToBool(S);
      6: TTrackBar(C).TickMarks:= TTickMark(StrToIntDef(S, 0));
      7: TTrackBar(C).TickStyle:= TTickStyle(StrToIntDef(S, 0));
    end;
    exit;
  end;

  if (C is TProgressBar) then
  begin
    case AIndex of
      0: TProgressBar(C).Orientation:= TProgressBarOrientation(StrToIntDef(S, 0));
      1: TProgressBar(C).Min:= StrToIntDef(S, 0);
      2: TProgressBar(C).Max:= StrToIntDef(S, 100);
      3: TProgressBar(C).Smooth:= AppStrToBool(S);
      4: TProgressBar(C).Step:= StrToIntDef(S, 1);
      5: TProgressBar(C).Style:= TProgressBarStyle(StrToIntDef(S, 0));
      6: TProgressBar(C).BarShowText:= AppStrToBool(S);
    end;
    exit;
  end;

  if (C is TATGauge) then
  begin
    case AIndex of
      0: TATGauge(C).Kind:= TATGaugeKind(StrToIntDef(S, 0));
      1: TATGauge(C).MinValue:= StrToIntDef(S, 0);
      2: TATGauge(C).MaxValue:= StrToIntDef(S, 100);
      3: TATGauge(C).ShowText:= AppStrToBool(S);
    end;
    exit;
  end;

  if (C is TSplitter) then
  begin
    case AIndex of
      0: TSplitter(C).Beveled:= AppStrToBool(S);
      1: TSplitter(C).ResizeStyle:= cResizeStyle[AppStrToBool(S)];
      2: TSplitter(C).AutoSnap:= AppStrToBool(S);
      3: TSplitter(C).MinSize:= StrToIntDef(S, TSplitter(C).MinSize);
    end;
    exit;
  end;

  if (C is TListViewFilterEdit) then
  begin
    case AIndex of
      0: TListViewFilterEdit(C).ByAllFields:= AppStrToBool(S);
    end;
    exit
  end;

  if (C is TRadioGroup) then
  begin
    case AIndex of
      0: TRadioGroup(C).ColumnLayout:= TColumnLayout(StrToIntDef(S, 0));
    end;
  end;
end;


procedure DoControl_SetPropsFromString_Adv(C: TControl; S: string);
var
  SItem: string;
  NIndex: integer;
begin
  NIndex:= 0;
  repeat
    SItem:= SGetItem(S);
    if SItem<>'' then
      DoControl_SetEx(C, SItem, NIndex);
    Inc(NIndex);
    if S='' then Break;
  until false;
end;


procedure DoControl_SetColumnsFromString(C: TControl; S: string);
begin
  if C is TListView then
  begin
    DoControl_SetColumns_ListView(TListView(C), S);
    exit
  end;

  if C is TRadioGroup then
  begin
    TRadioGroup(C).Columns:= StrToIntDef(S, 1);
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
    DoControl_SetState_Image(TImage(C), S);
    exit
  end;

  if C is TListbox then TListbox(C).Items.Clear;
  if C is TComboBox then TComboBox(C).Items.Clear;
  if C is TCheckGroup then TCheckGroup(C).Items.Clear;
  if C is TRadioGroup then TRadioGroup(C).Items.Clear;
  if C is TCheckListBox then TCheckListBox(C).Items.Clear;
  if C is TTabControl then TTabControl(C).Tabs.Clear;

  if C is TListView then
  begin
    TListView(C).Columns.Clear;
    TListView(C).Items.Clear;
  end;

  if C is TPageControl then
  begin
    for i:= TPageControl(C).PageCount-1 downto 0 do
      TPageControl(C).Pages[i].Free;
  end;

  while S<>'' do
  begin
    SItem:= SGetItem(S, #9);
    if C is TListbox then TListbox(C).Items.Add(SItem);
    if C is TComboBox then TComboBox(C).Items.Add(SItem);
    if C is TCheckGroup then TCheckGroup(C).Items.Add(SItem);
    if C is TRadioGroup then TRadioGroup(C).Items.Add(SItem);
    if C is TCheckListBox then TCheckListBox(C).Items.Add(SItem);
    if C is TListView then DoControl_SetState_ListviewItem(TListView(C), SItem);
    if C is TTabControl then TTabControl(C).Tabs.Add(SItem);
    if C is TPageControl then TPageControl(C).AddTabSheet.Caption:= SItem;
  end;
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

procedure DoControl_SetTextHintFromString(C: TControl; const S: string);
begin
  if C is TCustomEdit then
    (C as TCustomEdit).TextHint:= S
  else
  if C is TATSynEdit then
    (C as TATSynEdit).OptTextHint:= S;
end;


procedure DoControl_SetStateFromString(C: TControl; const S: string);
begin
  if C is TCheckBox then
  begin
    DoControl_SetState_Checkbox(TCheckbox(C), S);
    exit
  end;
  if C is TToggleBox then
  begin
    TToggleBox(C).Checked:= AppStrToBool(S);
    exit
  end;
  if C is TRadioButton then
  begin
    TRadioButton(C).Checked:= AppStrToBool(S);
    exit
  end;
  if C is TEdit then
  begin
    TEdit(C).Text:= S;
    exit
  end;
  if C is TComboBox then
  begin
    DoControl_SetState_Combobox(TCombobox(C), S);
    exit
  end;
  if C is TListBox then
  begin
    DoControl_SetState_Listbox(TListbox(C), S);
    exit
  end;
  if C is TRadioGroup then
  begin
    DoControl_SetState_RadioGroup(TRadioGroup(C), S);
    exit
  end;
  if C is TCheckGroup then
  begin
    DoControl_SetState_CheckGroup(TCheckGroup(C), S);
    exit
  end;
  if C is TCheckListBox then
  begin
    DoControl_SetState_CheckListbox(TCheckListBox(C), S);
    exit
  end;
  if C is TMemo then
  begin
    DoControl_SetState_Memo(TMemo(C), S);
    exit
  end;
  if C is TSpinEdit then
  begin
    DoControl_SetState_SpinEdit(TSpinEdit(C), S);
    exit
  end;
  if C is TListView then
  begin
    DoControl_SetState_Listview(TListView(C), S);
    exit
  end;
  if C is TTabControl then
  begin
    DoControl_SetState_TabControl(TTabControl(C), S);
    exit
  end;
  if C is TPageControl then
  begin
    DoControl_SetState_PageControl(TPageControl(C), S);
    exit
  end;
  if C is TTrackBar then
  begin
    TTrackBar(C).Position:= StrToIntDef(S, 0);
    exit
  end;
  if C is TProgressBar then
  begin
    TProgressBar(C).Position:= StrToIntDef(S, 0);
    exit
  end;
  if C is TATGauge then
  begin
    TATGauge(C).Progress:= StrToIntDef(S, 0);
    exit
  end;
  if C is TListFilterEdit then
  begin
    TListFilterEdit(C).Text:= S;
    exit
  end;
  if C is TListViewFilterEdit then
  begin
    TListViewFilterEdit(C).Text:= S;
    exit
  end;
end;


procedure DoControl_SetPropFromPair(C: TControl; AName, AValue: string);
var
  Num: integer;
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

  if AName='texthint' then
  begin
    DoControl_SetTextHintFromString(C, AValue);
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

  if AName='w_min' then
  begin
    C.Constraints.MinWidth:= StrToIntDef(AValue, 0);
    exit;
  end;
  if AName='w_max' then
  begin
    C.Constraints.MaxWidth:= StrToIntDef(AValue, 4000);
    exit;
  end;
  if AName='h_min' then
  begin
    C.Constraints.MinHeight:= StrToIntDef(AValue, 0);
    exit;
  end;
  if AName='h_max' then
  begin
    C.Constraints.MaxHeight:= StrToIntDef(AValue, 4000);
    exit;
  end;

  if AName='autosize' then
  begin
    C.AutoSize:= AppStrToBool(AValue);
    exit;
  end;

  if AName='props' then
  begin
    ////MsgLogConsole('Deprecated API: dlg_proc "props" for "'+TAppControlProps(C.Tag).FTypeString+'"');
    DoControl_SetPropsFromString_Adv(C, AValue);
    exit;
  end;

  if AName='items' then
  begin
    DoControl_SetItemsFromString(C, AValue);
    exit;
  end;

  if AName='columns' then
  begin
    DoControl_SetColumnsFromString(C, AValue);
    exit;
  end;

  if AName='val' then
  begin
    DoControl_SetStateFromString(C, AValue);
    exit;
  end;

  if SBeginsWith(AName, 'ex') then
  begin
    Num:= StrToIntDef(Copy(AName, 3, MaxInt), -1);
    if Num>=0 then
      DoControl_SetEx(C, AValue, Num);
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

  if AName='on_click_header' then
  begin
    TAppControlProps(C.Tag).FEventOnClickHeader:= AValue;
    exit;
  end;

  if AName='on_focus_enter' then
  begin
    TAppControlProps(C.Tag).FEventOnFocusEnter:= AValue;
    exit;
  end;

  if AName='on_focus_exit' then
  begin
    TAppControlProps(C.Tag).FEventOnFocusExit:= AValue;
    exit;
  end;

  if AName='on_mouse_enter' then
  begin
    TAppControlProps(C.Tag).FEventOnMouseEnter:= AValue;
    exit;
  end;

  if AName='on_mouse_exit' then
  begin
    TAppControlProps(C.Tag).FEventOnMouseExit:= AValue;
    exit;
  end;

  if AName='on_mouse_down' then
  begin
    TAppControlProps(C.Tag).FEventOnMouseDown:= AValue;
    exit;
  end;

  if AName='on_mouse_up' then
  begin
    TAppControlProps(C.Tag).FEventOnMouseUp:= AValue;
    exit;
  end;

  if AName='on_draw_item' then
  begin
    TAppControlProps(C.Tag).FEventOnListboxDrawItem:= AValue;
    exit;
  end;

  if AName='on_caret' then
  begin
    TAppControlProps(C.Tag).FEventOnEditorCaret:= AValue;
    exit;
  end;

  if AName='on_scroll' then
  begin
    TAppControlProps(C.Tag).FEventOnEditorScroll:= AValue;
    exit;
  end;

  if AName='on_key_down' then
  begin
    TAppControlProps(C.Tag).FEventOnEditorKeyDown:= AValue;
    exit;
  end;

  if AName='on_key_up' then
  begin
    TAppControlProps(C.Tag).FEventOnEditorKeyUp:= AValue;
    exit;
  end;

  if AName='on_click_gutter' then
  begin
    TAppControlProps(C.Tag).FEventOnEditorClickGutter:= AValue;
    exit;
  end;

  if AName='on_click_gap' then
  begin
    TAppControlProps(C.Tag).FEventOnEditorClickGap:= AValue;
    exit;
  end;

  if AName='on_paste' then
  begin
    TAppControlProps(C.Tag).FEventOnEditorPaste:= AValue;
    exit;
  end;


  if AName='color' then
  begin
    with DoControl_Target(C) do
      Color:= StrToIntDef(AValue, Color);
    exit;
  end;

  if AName='font_name' then
  begin
    with DoControl_Target(C) do
      Font.Name:= AValue;
    exit;
  end;

  if AName='font_size' then
  begin
    with DoControl_Target(C) do
      Font.Size:= StrToIntDef(AValue, Font.Size);
    exit;
  end;

  if AName='font_color' then
  begin
    with DoControl_Target(C) do
      Font.Color:= StrToIntDef(AValue, Font.Color);
    exit;
  end;

  if AName='font_style' then
  begin
    with DoControl_Target(C) do
      Font.Style:= StringToFontStyles(AValue);
    exit;
  end;

  if AName='tab_stop' then
  begin
    if C is TWinControl then
      TWinControl(C).TabStop:= AppStrToBool(AValue);
    exit;
  end;

  if AName='tab_order' then
  begin
    if C is TWinControl then
      TWinControl(C).TabOrder:= StrToIntDef(AValue, -1);
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
  C: TComponent;
  Str: string;
begin
  Result:= '';

  List:= TStringList.Create;
  try
    List.TextLineBreakStyle:= tlbsLF;

    NActive:= -1;
    for i:= 0 to AForm.ComponentCount-1 do
    begin
      C:= AForm.Components[i];
      if C=AForm.ActiveControl then NActive:= i;
      if C is TControl then
        Str:= DoControl_GetState(TControl(C))
      else
        Str:= '';
      List.Add(Str);
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
  CtlPrev: TComponent;
begin
  if Ctl is TWinControl then
    if F.ComponentCount>=2 then
    begin
      CtlPrev:= F.Components[F.ComponentCount-2];
      if CtlPrev is TLabel then
        TLabel(CtlPrev).FocusControl:= TWinControl(Ctl);
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
      F.ActiveControl:= TWinControl(C);
end;

procedure DoForm_ScaleAuto(F: TForm; ASimpleResize: boolean=false);
var
  i: integer;
begin
  {$ifdef darwin}
  exit;
  //macOS: gives bad result, toolbar big labels
  {$endif}

  //if Screen.PixelsPerInch=96 then
  //  ASimpleResize:= false;

  if UiOps.Scale<>100 then
    if ASimpleResize then
    begin
      F.Width:= AppScale(F.Width);
      F.Height:= AppScale(F.Height);
      for i:= 0 to F.ControlCount-1 do
        with F.Controls[i] do
        begin
          Left:= AppScale(Left);
          Top:= AppScale(Top);
        end;
    end
    else
      F.AutoAdjustLayout(
        lapAutoAdjustForDPI ,
        96, AppScale(96),
        F.Width, AppScale(F.Width)
        );
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
  C: TControl;
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

    C:= F.FindControlByIndex(AFocusedIndex);
    if Assigned(C) then
      DoForm_FocusControl(F, C);
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
  if Id='button' then
  begin
    C:= TButton.Create(nil);
    DoControl_FixButtonHeight(C);
  end
  else
  if (Id='label') or (Id='linklabel') then
    C:= TLabel.Create(nil)
  else
  if Id='combo' then
    C:= TComboBox.Create(nil)
  else
  if Id='combo_ro' then
  begin
    C:= TComboBox.Create(nil);
    TComboBox(C).Style:= csDropDownList;
    TComboBox(C).Items.Add('DD');
  end
  else
  if (Id='edit') or (Id='filter_listview') or (Id='filter_listbox') then
    C:= TEdit.Create(nil)
  else
  if Id='spinedit' then
    C:= TSpinEdit.Create(nil)
  else
  if Id='check' then
    C:= TCheckbox.Create(nil)
  else
  if Id='radio' then
    C:= TRadioButton.Create(nil)
  else
  if Id='checkbutton' then
    C:= TToggleBox.Create(nil)
  else
  if Id='scrollbar' then
    exit(GetSystemMetrics(SM_CXVSCROLL))
  else
    exit;

  try
    C.Hide;
    C.Left:= -300;
    C.Caption:= 'WpPJjy'; //for label autosize
    C.Parent:= Application.MainForm; //else height incorrect
    Result:= C.Height;
  finally
    FreeAndNil(C);
  end;
end;


procedure DoForm_SetPropFromPair(F: TFormDummy; const AName, AValue: string);
var
  Num64: Int64;
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
  if AName='resize' then //deprecated!
  begin
    if AppStrToBool(AValue) then
      F.BorderStyle:= bsSizeable
    else
      F.BorderStyle:= bsDialog;
  end
  else
  if AName='border' then
    F.BorderStyle:= TFormBorderStyle(StrToIntDef(AValue, Ord(bsDialog)))
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
  if AName= 'on_act' then
    F.FEventOnActivate:= AValue
  else
  if AName= 'on_deact' then
    F.FEventOnDeactivate:= AValue
  else
  if AName='on_mouse_enter' then
    F.FEventOnMouseEnter:= AValue
  else
  if AName='on_mouse_exit' then
    F.FEventOnMouseExit:= AValue
  else
  if AName='on_show' then
    F.FEventOnShow:= AValue
  else
  if AName='on_hide' then
    F.FEventOnHide:= AValue
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
  if AName='p' then
  begin
    Num64:= StrToInt64Def(AValue, 0);
    if Num64<>0 then
      F.Parent:= TWinControl(PtrInt(Num64));
  end
  else;
end;


function DoForm_GetPropsAsStringDict(F: TFormDummy): PPyObject;
begin
  with GetPythonEngine do
  begin
    Result:= Py_BuildValue('{sssssisisisisisisOsOsOsOsO}',
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
      'keypreview', PyBool_FromLong(Ord(F.KeyPreview)),
      PChar(string('p')), PyLong_FromLongLong(PtrInt(F.Parent))
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
  bTabStop, bFocused: boolean;
  nTabOrder: integer;
  SItems, SColumns: string;
begin
  bFocused:= false;
  bTabStop:= false;
  nTabOrder:= -1;
  SItems:= DoControl_GetItems(C);
  SColumns:= DoControl_GetColumns(C);

  if C is TWinControl then
  begin
    bFocused:= TWinControl(C).Focused;
    bTabStop:= TWinControl(C).TabStop;
    nTabOrder:= TWinControl(C).TabOrder;
  end;

  with GetPythonEngine do
  begin
    //is it docked form?
    if C.Tag=0 then
      exit(ReturnNone);

    Result:= Py_BuildValue('{sssssssssssisisisisssOsOsOsOsOsisisisisisissss}',
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
      'focused', PyBool_FromLong(Ord(bFocused)),
      'tab_stop', PyBool_FromLong(Ord(bTabStop)),
      'tab_order', nTabOrder,
      'sp_l', C.BorderSpacing.Left,
      'sp_r', C.BorderSpacing.Right,
      'sp_t', C.BorderSpacing.Top,
      'sp_b', C.BorderSpacing.Bottom,
      'sp_a', C.BorderSpacing.Around,
      'items', PChar(SItems),
      'columns', PChar(SColumns)
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
  C: TComponent;
  i: integer;
begin
  for i:= F.ComponentCount-1 downto 0 do
  begin
    C:= F.Components[i];
    if C is TForm then
    begin
      //ShowMessage(C.Caption); //dont run, so code not needed
      TForm(C).Parent:= nil;
      TForm(C).Close;
    end;
  end;
end;


end.

