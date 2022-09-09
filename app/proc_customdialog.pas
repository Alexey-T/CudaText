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
  CheckLst, Spin, ComCtrls, Dialogs, Math,
  ListFilterEdit,
  ListViewFilterEdit,
  LclIntf, LclProc, LclType,
  ATListbox,
  ATButtons,
  ATFlatToolbar,
  ATLinkLabel,
  ATStringProc,
  ATStringProc_Separator,
  ATPanelColor,
  ATGauge,
  ATStatusBar,
  ATSynEdit,
  ATSynEdit_Globals,
  ATSynEdit_Edits,
  ATSynEdit_Adapter_EControl,
  proc_customdialog_dummy,
  proc_miscutils,
  proc_globdata,
  proc_py,
  proc_editor,
  proc_scrollbars,
  proc_lexer_styles,
  proc_str,
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
procedure DoControl_SetPropsFromStringDict(C: TControl; const AText: string);
procedure DoControl_InitPropsObject(Ctl: TControl; AForm: TFormDummy; const ATypeName: string);

function DoForm_GetPropsAsStringDict(F: TFormDummy): PPyObject;
procedure DoForm_SetPropsFromStringDict(F: TFormDummy; const AText: string);
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

procedure SetDictKey_NotEmpty(Obj: PPyObject; const AKey, AValue: string);
begin
  if AValue<>'' then
    AppPython.SetDictKey(Obj, AKey, AValue);
end;

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
begin
  Result:= C.Lines.Text;

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

function DoControl_GetState_ScrollBox(C: TScrollBox): string;
begin
  Result:= Format('%d,%d,%d,%d,%d,%d', [
    C.VertScrollBar.Position,
    C.HorzScrollBar.Position,
    C.VertScrollBar.Range,
    C.HorzScrollBar.Range,
    C.VertScrollBar.Page,
    C.HorzScrollBar.Page
    ]);
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

  if C is TScrollBox then
    exit(DoControl_GetState_ScrollBox(TScrollBox(C)));
end;


procedure DoControl_SetState_Memo(C: TMemo; AValue: string);
begin
  AValue:= StringReplace(AValue, #9, #10, [rfReplaceAll]);
  AValue:= StringReplace(AValue, #3, #9, [rfReplaceAll]);

  C.Lines.TextLineBreakStyle:= tlbsLF;
  C.Lines.Text:= AValue;
end;

procedure DoControl_SetState_Checkgroup(C: TCheckGroup; const AValue: string);
var
  Sep: TATStringSeparator;
  SItem: string;
  N: integer;
begin
  N:= 0;
  Sep.Init(AValue);
  repeat
    if N>=C.Items.Count then exit;
    if not Sep.GetItemStr(SItem) then Break;
    C.Checked[N]:= AppStrToBool(SItem);
    Inc(N);
  until false;
end;

procedure DoControl_SetState_CheckListbox(C: TCheckListBox; const AValue: string);
var
  Sep: TATStringSeparator;
  S, S2: string;
  N: integer;
begin
  SSplitByChar(AValue, ';', S, S2);
  N:= StrToIntDef(S, -1);
  if (N>=0) and (N<C.Items.Count) then
    C.ItemIndex:= N;

  N:= 0;
  Sep.Init(S2);
  repeat
    if N>=C.Items.Count then exit;
    if not Sep.GetItemStr(S) then Break;
    C.Checked[N]:= AppStrToBool(S);
    Inc(N);
  until false;
end;


procedure DoControl_SetState_ListviewItem(C: TListView; const AListItem: string);
var
  Sep: TATStringSeparator;
  SItem, SKey, SVal: string;
  Col: TListColumn;
  i: integer;
begin
  Sep.Init(AListItem, #13);
  if C.Columns.Count=0 then
  begin
    repeat
      if not Sep.GetItemStr(SItem) then Break;
      Col:= C.Columns.Add;
      SSplitByChar(SItem, '=', SKey, SVal);
      Col.Caption:= SKey;
      if SVal<>'' then
      begin
        if SVal[1]='L' then begin Delete(SVal, 1, 1); Col.Alignment:= taLeftJustify; end;
        if SVal[1]='R' then begin Delete(SVal, 1, 1); Col.Alignment:= taRightJustify; end;
        if SVal[1]='C' then begin Delete(SVal, 1, 1); Col.Alignment:= taCenter; end;
        Col.Width:= StrToIntDef(SVal, 80);
      end;
    until false;
  end
  else
  begin
    Sep.GetItemStr(SItem);
    C.Items.Add.Caption:= SItem;
    for i:= 1 to C.ColumnCount do
    begin
      Sep.GetItemStr(SItem);
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

function DoControl_GetIcons_ListView(C: TListView): string;
var
  L: TStringList;
  i: integer;
begin
  Result:= '';
  if C.Items.Count=0 then exit;

  L:= TStringList.Create;
  try
    for i:= 0 to C.Items.Count-1 do
      L.Add(IntToStr(C.Items[i].ImageIndex));

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

procedure DoControl_SetColumns_ListView(C: TListView; const AValue: string);
var
  SepColumns, SepProps: TATStringSeparator;
  Column: TListColumn;
  SCol, SItem: string;
  Num, NIndex: integer;
begin
  SepColumns.Init(AValue, #9);
  NIndex:= 0;
  repeat
    if not SepColumns.GetItemStr(SCol) then Break;
    if NIndex<C.Columns.Count then
      Column:= C.Columns[NIndex]
    else
      Column:= C.Columns.Add;
    Inc(NIndex);

    SepProps.Init(SCol, #13);

    SepProps.GetItemStr(SItem);
    Column.Caption:= SItem;
    SepProps.GetItemInt(Num, 100);
    Column.Width:= Num;
    SepProps.GetItemInt(Num, 0);
    Column.MinWidth:= Num;
    SepProps.GetItemInt(Num, 0);
    Column.MaxWidth:= Num;

    if SepProps.GetItemStr(SItem) then
      case SItem of
        'L': Column.Alignment:= taLeftJustify;
        'R': Column.Alignment:= taRightJustify;
        'C': Column.Alignment:= taCenter;
      end;

    if SepProps.GetItemStr(SItem) then
      Column.AutoSize:= AppStrToBool(SItem);

    if SepProps.GetItemStr(SItem) then
      Column.Visible:= AppStrToBool(SItem);
  until false;
end;


function DoControl_GetItems(C: TControl): string;
begin
  Result:= '';
  if C is TCustomListbox then
    exit(DoControl_GetItems_Listbox(TCustomListbox(C)));
  if C is TListView then
    exit(DoControl_GetItems_ListView(TListView(C)));
end;


function DoControl_GetIndexHovered(C: TControl): integer;
var
  P: TPoint;
begin
  Result:= -1;

  if C is TTabControl then
  begin
    P:= Mouse.CursorPos;
    P:= C.ScreenToClient(P);
    Result:= TTabControl(C).IndexOfTabAt(P);
    exit
  end;

  if C is TPageControl then
  begin
    P:= Mouse.CursorPos;
    P:= C.ScreenToClient(P);
    Result:= TPageControl(C).IndexOfTabAt(P);
    exit
  end;

  if C is TATFlatToolbar then
  begin
    Result:= TATFlatToolbar(C).ButtonWithMouseOver;
    exit
  end;

  if C is TATStatus then
  begin
    P:= Mouse.CursorPos;
    P:= C.ScreenToClient(P);
    Result:= TATStatus(C).GetPanelAt(P.X, P.Y);
    exit
  end;
end;

function DoControl_GetColumns(C: TControl): string;
begin
  Result:= '';

  if C is TListView then
    exit(DoControl_GetColumns_ListView(TListView(C)));

  if C is TRadioGroup then
    exit(IntToStr(TRadioGroup(C).Columns));

  {
  //for TabControl/PageControl, I get result rects as (-1,-1,-1,-1) for linux gtk2, (0,0,0,0) for win32
  if C is TTabControl then
    exit(DoControl_GetColumns_TabControl(TTabControl(C)));

  if C is TPageControl then
    exit(DoControl_GetColumns_TabControl(TPageControl(C)));
    }
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

procedure DoControl_SetState_Listview(C: TListView; const AValue: string);
var
  Sep: TATStringSeparator;
  S, S2: string;
  N: integer;
begin
  SSplitByChar(AValue, ';', S, S2);

  //index
  N:= StrToIntDef(S, 0);
  if (N>=0) and (N<C.Items.Count) then
  begin
    C.ItemFocused:= C.Items[N];
    C.Selected:= C.ItemFocused;
    if Assigned(C.ItemFocused) then
      C.ItemFocused.MakeVisible(false);
  end;

  //check0,check1,..
  Sep.Init(S2);
  if C.Checkboxes then
  begin
    N:= 0;
    repeat
      if N>=C.Items.Count then break;
      if not Sep.GetItemStr(S) then Break;
      C.Items[N].Checked:= AppStrToBool(S);
      Inc(N);
    until false;
  end;
end;

procedure DoControl_SetState_ScrollBox(C: TScrollbox; const AValue: string);
var
  S1, S2: string;
  N: integer;
begin
  SSplitByChar(AValue, ',', S1, S2);

  N:= StrToIntDef(S1, -1);
  if N>=0 then
    C.VertScrollBar.Position:= N;

  N:= StrToIntDef(S2, -1);
  if N>=0 then
    C.HorzScrollBar.Position:= N;
end;

function DoControl_GetState_Listview(C: TListView): string;
// index;check0,check1,
var
  i: integer;
begin
  Result:= '';
  if Assigned(C.ItemFocused) then
    Result:= IntToStr(C.ItemFocused.Index);

  if C.Checkboxes then
  begin
    Result:= Result+';';
    for i:= 0 to C.Items.Count-1 do
      Result:= Result+IntToStr(Ord(C.Items[i].Checked))+',';
  end;
end;

procedure DoControl_ApplyEditorProps(Ed: TATSynEdit; AForm: TFormDummy;
  AApplyUnprintedAndWrap, AApplyTabSize, AApplyCentering, AOneLiner: boolean);
begin
  Ed.Font.Name:= EditorOps.OpFontName;
  Ed.Font.Size:= EditorOps.OpFontSize;

  Ed.DoubleBuffered:= UiOps.DoubleBuffered;
  Ed.OnChange:= @AForm.DoOnEditorChange;
  Ed.OnChangeCaretPos:= @AForm.DoOnEditorChangeCaretPos;
  Ed.OnKeyDown:= @AForm.DoOnEditorKeyDown;
  Ed.OnKeyUp:= @AForm.DoOnEditorKeyUp;
  Ed.OnClickGutter:= @AForm.DoOnEditorClickGutter;
  Ed.OnClickGap:= @AForm.DoOnEditorClickGap;
  Ed.OnClickLink:= @AForm.DoOnEditorClickLink;
  Ed.OnScroll:= @AForm.DoOnEditorScroll;
  Ed.OnPaste:= @AForm.DoOnEditorPaste;
  Ed.OnCommand:= AppCustomDialog_OnEditorCommand;

  Ed.OptBorderFocusedActive:= EditorOps.OpActiveBorderInEditor;
  Ed.OptBorderWidthFocused:= ATEditorScale(EditorOps.OpActiveBorderWidth);

  Ed.OptThemed:= true;
  EditorApplyTheme(Ed);

  if not AOneLiner then
    EditorApplyOps(Ed, EditorOps, AApplyUnprintedAndWrap, AApplyTabSize, AApplyCentering, AOneLiner)
  else
  begin
    Ed.OptCaretBlinkTime:= EditorOps.OpCaretBlinkTime;
    Ed.OptCaretBlinkEnabled:= EditorOps.OpCaretBlinkEn;
  end;

  Ed.OptDimUnfocusedBack:= 0; //fix issue #4346
end;

procedure DoControl_InitPropsObject(Ctl: TControl; AForm: TFormDummy; const ATypeName: string);
var
  Props: TAppControlProps;
begin
  Props:= TAppControlProps.Create(ATypeName);
  Props.FName:= Ctl.Name;
  Ctl.Tag:= PtrInt(Props);

  //for buttons, set 'act':True by default
  if SBeginsWith(ATypeName, 'button') then
    Props.FActive:= true;
  //for editors, set 'act':True, issue #3361
  if Ctl is TATSynEdit then
    Props.FActive:= true;

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

procedure DoControl_CreateNew(
  const S: string;
  AForm: TFormDummy;
  out Ctl: TControl);
var
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
    TATSynEdit(Ctl).Keymap:= AppKeymapMain;
    TATSynEdit(Ctl).OptEnableMarkersInUndo:= false;
    DoControl_ApplyEditorProps(TATSynEdit(Ctl), AForm, true, true, false, false);

    Adapter:= TATAdapterEControl.Create(Ctl);
    Adapter.EnabledSublexerTreeNodes:= UiOps.TreeSublexers;
    Adapter.DynamicHiliteEnabled:= EditorOps.OpLexerDynamicHiliteEnabled;
    Adapter.DynamicHiliteMaxLines:= EditorOps.OpLexerDynamicHiliteMaxLines;
    Adapter.AddEditor(TATSynEdit(Ctl));

    exit;
  end;

  if S='editor_edit' then
  begin
    Ctl:= TATEdit.Create(AForm);
    TATSynEdit(Ctl).Keymap:= AppKeymapMain;
    TATSynEdit(Ctl).OptEnableMarkersInUndo:= false;
    DoControl_ApplyEditorProps(TATSynEdit(Ctl), AForm, false, false, false, true);
    exit;
  end;

  if S='editor_combo' then
  begin
    Ctl:= TATComboEdit.Create(AForm);
    TATSynEdit(Ctl).Keymap:= AppKeymapMain;
    TATSynEdit(Ctl).OptEnableMarkersInUndo:= false;
    DoControl_ApplyEditorProps(TATSynEdit(Ctl), AForm, false, false, false, true);
    exit;
  end;

  if S='edit_pwd' then
  begin
    Ctl:= TEdit.Create(AForm);
    TEdit(Ctl).EchoMode:= emPassword;
    TEdit(Ctl).OnChange:= @AForm.DoOnChange;
    exit;
  end;

  if S='spinedit' then
  begin
    Ctl:= TSpinEdit.Create(AForm);
    TSpinEdit(Ctl).OnChange:= @AForm.DoOnChange;
    exit;
  end;

  if S='memo' then
  begin
    Ctl:= TMemo.Create(AForm);
    TMemo(Ctl).WordWrap:= false;
    TMemo(Ctl).ScrollBars:= ssBoth;
    TMemo(Ctl).OnChange:= @AForm.DoOnChange;
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
    TATButton(Ctl).DoubleBuffered:= UiOps.DoubleBuffered;
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
    TRadioGroup(Ctl).OnSelectionChanged:= @AForm.DoOnChange;
    exit;
  end;

  if S='checkgroup' then
  begin
    Ctl:= TCheckGroup.Create(AForm);
    TCheckGroup(Ctl).OnItemClick:= @AForm.DoOnCheckGroupClicked;
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
    TListView(Ctl).DoubleBuffered:= UiOps.DoubleBuffered;
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
    TAppTreeContainer(Ctl).Tree.DoubleBuffered:= UiOps.DoubleBuffered;
    TAppTreeContainer(Ctl).Tree.OnChange:= @AForm.DoOnTreeviewChange;
    TAppTreeContainer(Ctl).Tree.OnSelectionChanged:= @AForm.DoOnTreeviewSelect;
    TAppTreeContainer(Ctl).Tree.OnCollapsing:= @AForm.DoOnTreeviewCollapsing;
    TAppTreeContainer(Ctl).Tree.OnExpanding:= @AForm.DoOnTreeviewExpanding;
    TAppTreeContainer(Ctl).Tree.DefaultItemHeight:= ATEditorScale(DefaultTreeNodeHeight);
    TAppTreeContainer(Ctl).Invalidate;
    exit
  end;

  if (S='listbox_ex') then
  begin
    Ctl:= TATListbox.Create(AForm);
    TATListbox(Ctl).Theme:= @AppApiFlatTheme;
    TATListbox(Ctl).DoubleBuffered:= UiOps.DoubleBuffered;
    TATListbox(Ctl).VirtualMode:= false;
    TATListbox(Ctl).CanGetFocus:= true;
    TATListbox(Ctl).OnClickXMark:= @AForm.DoOnClickX;
    TATListbox(Ctl).OnClickHeader:= @AForm.DoOnListboxClickHeader;
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
    Ctl:= TAppSplitter.Create(AForm);
    TAppSplitter(Ctl).Beveled:= true;
    TAppSplitter(Ctl).ResizeStyle:= rsPattern;
    TAppSplitter(Ctl).AutoSnap:= false;
    TAppSplitter(Ctl).OnMoved:= @AForm.DoOnChange;
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

  if S='scrollbox' then
  begin
    Ctl:= TScrollBox.Create(AForm);
    TScrollBox(Ctl).VertScrollBar.Tracking:= true;
    TScrollBox(Ctl).HorzScrollBar.Tracking:= true;
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
    TATStatus(Ctl).OnPanelClick:= @AForm.DoOnStatusbarPanelClick;
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

  //unsupported control type: create red panel to indicate plugin's error
  Ctl:= TPanel.Create(AForm);
  Ctl.Caption:= S+'?';
  Ctl.Color:= clRed;
  exit;

 finally
   if Assigned(Ctl) then
     DoControl_InitPropsObject(Ctl, AForm, S);
 end;
end;


procedure DoControl_SetParentFromString(C: TControl; const AValue: string);
var
  P: TControl;
  Form: TFormDummy;
  NPage: integer;
  SName, SVal: string;
begin
  Form:= C.Owner as TFormDummy;

  //handle "name.N"
  if Pos('.', AValue)=0 then
  begin
    SName:= AValue;
    NPage:= -1;
  end
  else
  begin
    SSplitByChar(AValue, '.', SName, SVal);
    NPage:= StrToIntDef(SVal, -1);
  end;

  P:= Form.FindControlByOurName(SName);
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


procedure DoControl_SetAnchorFromString(C: TControl; AKind: TAnchorKind; const AValue: string);
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

  SSplitByChar(AValue, ',', SName, SSide);

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

procedure DoControl_SetBorder(C: TControl; AValue: boolean);
begin
  //listbox has it's special BorderVisible
  if C is TATListbox then
    TATListbox(C).BorderVisible:= AValue
  else
  //editor has it's special OptBorderVisible
  if C is TATSynEdit then
    TATSynEdit(C).OptBorderVisible:= AValue
  else
  if C is TWinControl then
    TWinControlHack(C).BorderStyle:= cControlBorderStyles[AValue];
end;

procedure DoControl_SetEx(C: TControl; const S: string; AIndex: integer);
const
  cResizeStyle: array[boolean] of TResizeStyle = (rsPattern, rsUpdate);
var
  N: integer;
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

  if (C is TCustomTabControl) then
  begin
    case AIndex of
      0:
        begin
          //possible values: '0'..'3'
          N:= StrToIntDef(S, 0);
          N:= Max(0, Min(3, N));
          TCustomTabControl(C).TabPosition:= TTabPosition(N);
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


procedure DoControl_SetColumnsFromString(C: TControl; const S: string);
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

procedure DoControl_SetIconsFromString(C: TControl; const AValue: string);
var
  Sep: TATStringSeparator;
  ListView: TListView;
  ListItem: TListItem;
  NItem: integer;
  i: integer;
begin
  Sep.Init(AValue, #9);

  if C is TListView then
  begin
    ListView:= TListView(C);
    for i:= 0 to ListView.Items.Count-1 do
    begin
      if not Sep.GetItemInt(NItem, -1) then
        Break;
      ListItem:= ListView.Items[i];
      ListItem.ImageIndex:= NItem;
    end;
    exit
  end;
end;

procedure DoControl_SetItemsFromString(C: TControl; const AValue: string);
var
  Sep: TATStringSeparator;
  SItem: string;
  i: integer;
begin
  if C is TImage then
  begin
    DoControl_SetState_Image(TImage(C), AValue);
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

  Sep.Init(AValue, #9);
  while Sep.GetItemStr(SItem) do
  begin
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


procedure DoControl_SetPosFromString(C: TControl; const AValue: string);
var
  Sep: TATStringSeparator;
  NX1, NY1, NX2, NY2: integer;
begin
  Sep.Init(AValue);
  Sep.GetItemInt(NX1, -1);
  Sep.GetItemInt(NY1, -1);
  Sep.GetItemInt(NX2, -1);
  Sep.GetItemInt(NY2, -1);
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
  if C is TScrollBox then
  begin
    DoControl_SetState_ScrollBox(TScrollBox(C), S);
    exit
  end;
end;


procedure DoControl_SetPropFromPair(C: TControl; const AName, AValue: string);
var
  Num: integer;
  Props: TAppControlProps;
begin
  if C.Tag=0 then exit;
  Props:= TAppControlProps(C.Tag);

  if AName='name' then
  begin
    Props.FName:= AValue;
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
    Props.FActive:= AppStrToBool(AValue);
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
    MsgLogConsole('NOTE: Deleted API is used: dlg_proc "props" for "'+Props.FTypeString+'"');
    //DoControl_SetPropsFromString_Adv(C, AValue);
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

  if AName='imageindexes' then
  begin
    DoControl_SetIconsFromString(TListView(C), AValue);
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
    Props.FTagString:= AValue;
    exit;
  end;

  if AName='border' then
  begin
    DoControl_SetBorder(C, AppStrToBool(AValue));
    exit;
  end;

  if AName='on_change' then
  begin
    Props.FEventOnChange:= AValue;
    exit;
  end;

  if AName='on_select' then
  begin
    Props.FEventOnSelect:= AValue;
    exit;
  end;

  if AName='on_fold' then
  begin
    Props.FEventOnFold:= AValue;
    exit;
  end;

  if AName='on_unfold' then
  begin
    Props.FEventOnUnfold:= AValue;
    exit;
  end;

  if AName='on_menu' then
  begin
    Props.FEventOnMenu:= AValue;
    exit;
  end;

  if AName='on_click' then
  begin
    Props.FEventOnClick:= AValue;
    exit;
  end;

  if AName='on_click_x' then
  begin
    Props.FEventOnClickX:= AValue;
    exit;
  end;

  if AName='on_click_dbl' then
  begin
    Props.FEventOnClickDbl:= AValue;
    exit;
  end;

  if AName='on_click_header' then
  begin
    Props.FEventOnClickHeader:= AValue;
    exit;
  end;

  if AName='on_focus_enter' then
  begin
    Props.FEventOnFocusEnter:= AValue;
    exit;
  end;

  if AName='on_focus_exit' then
  begin
    Props.FEventOnFocusExit:= AValue;
    exit;
  end;

  if AName='on_mouse_enter' then
  begin
    Props.FEventOnMouseEnter:= AValue;
    exit;
  end;

  if AName='on_mouse_exit' then
  begin
    Props.FEventOnMouseExit:= AValue;
    exit;
  end;

  if AName='on_mouse_down' then
  begin
    Props.FEventOnMouseDown:= AValue;
    exit;
  end;

  if AName='on_mouse_up' then
  begin
    Props.FEventOnMouseUp:= AValue;
    exit;
  end;

  if AName='on_draw_item' then
  begin
    Props.FEventOnListboxDrawItem:= AValue;
    exit;
  end;

  if AName='on_caret' then
  begin
    Props.FEventOnEditorCaret:= AValue;
    exit;
  end;

  if AName='on_scroll' then
  begin
    Props.FEventOnEditorScroll:= AValue;
    exit;
  end;

  if AName='on_key_down' then
  begin
    Props.FEventOnEditorKeyDown:= AValue;
    exit;
  end;

  if AName='on_key_up' then
  begin
    Props.FEventOnEditorKeyUp:= AValue;
    exit;
  end;

  if AName='on_click_gutter' then
  begin
    Props.FEventOnEditorClickGutter:= AValue;
    exit;
  end;

  if AName='on_click_gap' then
  begin
    Props.FEventOnEditorClickGap:= AValue;
    exit;
  end;

  if AName='on_click_link' then
  begin
    Props.FEventOnEditorClickLink:= AValue;
    exit;
  end;

  if AName='on_paste' then
  begin
    Props.FEventOnEditorPaste:= AValue;
    exit;
  end;


  if AName='color' then
  begin
    if C is TAppSplitter then
      TAppSplitter(C).CustomColored:= true;
    with DoControl_Target(C) do
      Color:= StrToIntDef(AValue, Color);
    exit;
  end;

  if AName='font_name' then
  begin
    with DoControl_Target(C) do
      Font.Name:= AValue;
    if C is TATListbox then
      TATListbox(C).ThemedFont:= false;
    exit;
  end;

  if AName='font_size' then
  begin
    with DoControl_Target(C) do
      Font.Size:= StrToIntDef(AValue, Font.Size);
    if C is TATListbox then
      TATListbox(C).ThemedFont:= false;
    exit;
  end;

  if AName='font_color' then
  begin
    with DoControl_Target(C) do
      Font.Color:= StrToIntDef(AValue, Font.Color);
    if C is TATListbox then
      TATListbox(C).ThemedFont:= false;
    exit;
  end;

  if AName='font_style' then
  begin
    with DoControl_Target(C) do
      Font.Style:= Lexer_StringToFontStyles(AValue);
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


procedure DoForm_AddControl(AForm: TFormDummy; const ATextItems: string);
var
  Sep: TATStringSeparator;
  SNameValue, SName, SValue: string;
  Ctl: TControl;
begin
  Ctl:= nil;
  Sep.Init(ATextItems, #1);
  repeat
    if not Sep.GetItemStr(SNameValue) then Break;
    SSplitByChar(SNameValue, '=', SName, SValue);
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
  if F.Enabled
    //and F.Visible //don't check Visible, DLG_CTL_FOCUS is called for modal form before form is shown
    and C.Enabled
    and C.Visible then
    if C is TWinControl then
    begin
      if TWinControl(C).CanFocus then
      try
        F.ActiveControl:= DoControl_Target(C) as TWinControl;
      except
        //suppress Pascal exception in Py API
      end;
    end;
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

  if ATEditorScalePercents<>100 then
    if ASimpleResize then
    begin
      F.Width:= ATEditorScale(F.Width);
      F.Height:= ATEditorScale(F.Height);
      for i:= 0 to F.ControlCount-1 do
        with F.Controls[i] do
        begin
          Left:= ATEditorScale(Left);
          Top:= ATEditorScale(Top);
        end;
    end
    else
      F.AutoAdjustLayout(
        lapAutoAdjustForDPI ,
        96, ATEditorScale(96),
        F.Width, ATEditorScale(F.Width)
        );
end;


procedure DoForm_FillContent(F: TFormDummy; const AContent: string);
var
  Sep: TATStringSeparator;
  SItem: string;
begin
  SItem:= '';
  Sep.Init(AContent, #10);
  while Sep.GetItemStr(SItem) do
    DoForm_AddControl(F, SItem);
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

    F.FixPositionIfOutOfScreen;

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
  if AName='taskbar' then
  begin
    F.ShowInTaskbar_Pending:= TShowInTaskbar(StrToIntDef(AValue, 0));
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
  if AName='on_key_press' then
    F.FEventOnKeyPress:= AValue
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
  if AName='on_form_state' then
    F.FEventOnFormState:= AValue
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
  with AppPython.Engine do
  begin
    Result:= Py_BuildValue('{sssssisisisisisisOsOsOsOsOss}',
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
      PChar(string('p')), PyLong_FromLongLong(PtrInt(F.Parent)),
      'form_state', PChar(cAppFormStateStr[F.WindowState])
      );
  end;

  SetDictKey_NotEmpty(Result, 'on_close', F.FEventOnClose);
  SetDictKey_NotEmpty(Result, 'on_close_query', F.FEventOnCloseQuery);
  SetDictKey_NotEmpty(Result, 'on_key_down', F.FEventOnKeyDown);
  SetDictKey_NotEmpty(Result, 'on_key_up', F.FEventOnKeyUp);
  SetDictKey_NotEmpty(Result, 'on_key_press', F.FEventOnKeyPress);
  SetDictKey_NotEmpty(Result, 'on_resize', F.FEventOnResize);
  SetDictKey_NotEmpty(Result, 'on_mouse_enter', F.FEventOnMouseEnter);
  SetDictKey_NotEmpty(Result, 'on_mouse_exit', F.FEventOnMouseExit);
  SetDictKey_NotEmpty(Result, 'on_show', F.FEventOnShow);
  SetDictKey_NotEmpty(Result, 'on_hide', F.FEventOnHide);
  SetDictKey_NotEmpty(Result, 'on_form_state', F.FEventOnFormState);
end;


procedure DoForm_SetPropsFromStringDict(F: TFormDummy; const AText: string);
var
  Sep: TATStringSeparator;
  SItem, SKey, SValue: string;
begin
  //text is '{key1:value1;key2:value2}' from to_str()
  Sep.Init(SDeleteCurlyBrackets(AText), #1);
  repeat
    if not Sep.GetItemStr(SItem) then Break;
    SSplitByChar(SItem, ':', SKey, SValue);
    SValue:= StringReplace(SValue, #2, ',', [rfReplaceAll]);
    DoForm_SetPropFromPair(F, SKey, SValue);
  until false;
end;

function DoControl_GetPropsAsStringDict(C: TControl): PPyObject;
var
  bTabStop, bFocused: boolean;
  nTabOrder: integer;
  SItems, SColumns: string;
  SParent: string;
  Props: TAppControlProps;
  N: integer;
begin
  bFocused:= false;
  bTabStop:= false;
  nTabOrder:= -1;
  SItems:= DoControl_GetItems(C);
  SColumns:= DoControl_GetColumns(C);

  if C is TWinControl then
  begin
    bFocused:= TWinControl(DoControl_Target(C)).Focused;
    bTabStop:= TWinControl(C).TabStop;
    nTabOrder:= TWinControl(C).TabOrder;
  end;

  SParent:= '';
  if Assigned(C.Parent) and (C.Parent.Tag<>0) then
    SParent:= TAppControlProps(C.Parent.Tag).FName;

  with AppPython.Engine do
  begin
    //is it docked form?
    if C.Tag=0 then
      exit(ReturnNone);
    Props:= TAppControlProps(C.Tag);

    Result:= Py_BuildValue('{sssssssssssisisisisssssOsOsOsOsOsisisisisisissss}',
      'name', PChar(Props.FName),
      'cap', PChar(C.Caption),
      'hint', PChar(C.Hint),
      'type', PChar(Props.FTypeString),
      'tag', PChar(Props.FTagString),
      PChar(string('x')), C.Left,
      PChar(string('y')), C.Top,
      PChar(string('w')), C.Width,
      PChar(string('h')), C.Height,
      PChar(string('p')), PChar(SParent),
      'val', PChar(DoControl_GetState(C)),
      'act', PyBool_FromLong(Ord(Props.FActive)),
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

    if C is TListView then
    begin
      if TListView(C).SmallImages=nil then
        TListView(C).SmallImages:= TImageList.Create(C);
      if TListView(C).LargeImages=nil then
        TListView(C).LargeImages:= TImageList.Create(C);

      AppPython.SetDictKey(Result, 'imagelist_small', TListView(C).SmallImages);
      AppPython.SetDictKey(Result, 'imagelist_large', TListView(C).LargeImages);

      SColumns:= DoControl_GetIcons_ListView(TListView(C));
      AppPython.SetDictKey(Result, 'imageindexes', SColumns);
    end;

    N:= DoControl_GetIndexHovered(C);
    if N>=0 then
      AppPython.SetDictKey(Result, 'tab_hovered', N);

    SetDictKey_NotEmpty(Result, 'on_change', Props.FEventOnChange);
    SetDictKey_NotEmpty(Result, 'on_click', Props.FEventOnClick);
    SetDictKey_NotEmpty(Result, 'on_click_dbl', Props.FEventOnClickDbl);
    SetDictKey_NotEmpty(Result, 'on_focus_enter', Props.FEventOnFocusEnter);
    SetDictKey_NotEmpty(Result, 'on_focus_exit', Props.FEventOnFocusExit);
    SetDictKey_NotEmpty(Result, 'on_menu', Props.FEventOnMenu);
    SetDictKey_NotEmpty(Result, 'on_select', Props.FEventOnSelect);
    SetDictKey_NotEmpty(Result, 'on_fold', Props.FEventOnFold);
    SetDictKey_NotEmpty(Result, 'on_unfold', Props.FEventOnUnfold);
    SetDictKey_NotEmpty(Result, 'on_listbox_draw_item', Props.FEventOnListboxDrawItem);
    SetDictKey_NotEmpty(Result, 'on_mouse_enter', Props.FEventOnMouseEnter);
    SetDictKey_NotEmpty(Result, 'on_mouse_exit', Props.FEventOnMouseExit);
    SetDictKey_NotEmpty(Result, 'on_mouse_down', Props.FEventOnMouseDown);
    SetDictKey_NotEmpty(Result, 'on_mouse_up', Props.FEventOnMouseUp);
    SetDictKey_NotEmpty(Result, 'on_editor_caret', Props.FEventOnEditorCaret);
    SetDictKey_NotEmpty(Result, 'on_editor_scroll', Props.FEventOnEditorScroll);
    SetDictKey_NotEmpty(Result, 'on_editor_key_down', Props.FEventOnEditorKeyDown);
    SetDictKey_NotEmpty(Result, 'on_editor_key_up', Props.FEventOnEditorKeyUp);
    SetDictKey_NotEmpty(Result, 'on_editor_click_gutter', Props.FEventOnEditorClickGutter);
    SetDictKey_NotEmpty(Result, 'on_editor_click_gap', Props.FEventOnEditorClickGap);
    SetDictKey_NotEmpty(Result, 'on_editor_click_link', Props.FEventOnEditorClickLink);
    SetDictKey_NotEmpty(Result, 'on_editor_paste', Props.FEventOnEditorPaste);
  end;
end;


procedure DoControl_SetPropsFromStringDict(C: TControl; const AText: string);
var
  Sep: TATStringSeparator;
  SItem, SKey, SValue: string;
begin
  //text is '{key1:value1;key2:value2}' from to_str()
  Sep.Init(SDeleteCurlyBrackets(AText), #1);
  repeat
    if not Sep.GetItemStr(SItem) then Break;
    SSplitByChar(SItem, ':', SKey, SValue);
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
