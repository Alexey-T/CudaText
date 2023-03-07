(*
This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at http://mozilla.org/MPL/2.0/.

Copyright (c) Alexey Torgashin
*)
unit proc_customdialog_dummy;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Graphics,
  StdCtrls, Forms, ComCtrls, ExtCtrls, Math,
  LCLType,
  LCLIntf,
  ListFilterEdit,
  ListViewFilterEdit,
  proc_globdata,
  proc_py,
  proc_miscutils,
  proc_appvariant,
  proc_scrollbars,
  ATSynEdit,
  ATSynEdit_Gaps,
  ATStatusBar,
  ATListbox;

type
  TAppPyCommonCallback = function(
    const ACallback: string;
    const AParamVars: TAppVariantArray;
    const AParamNames: array of string): boolean;

type
  TAppCtlMouseEvent = (
    cControlEventMouseEnter,
    cControlEventMouseExit,
    cControlEventMouseDown,
    cControlEventMouseUp
    );

var
  AppCustomDialog_DoPyCallback: TAppPyCommonCallback = nil;
  AppCustomDialog_OnEditorCommand: TATSynEditCommandEvent = nil;
  AppCustomDialogs: TFPList;


type
  { TAppControlProps }

  TAppControlProps = class
  public
    FName: string;
    FTypeString: string;
    FActive: boolean;
    FTagString: string;
    FEventOnChange: string;
    FEventOnClick: string;
    FEventOnClickX: string;
    FEventOnClickDbl: string;
    FEventOnClickHeader: string;
    FEventOnFocusEnter: string;
    FEventOnFocusExit: string;
    FEventOnMenu: string;
    FEventOnSelect: string;
    FEventOnFold: string;
    FEventOnUnfold: string;
    FEventOnListboxDrawItem: string;
    FEventOnMouseEnter: string;
    FEventOnMouseExit: string;
    FEventOnMouseDown: string;
    FEventOnMouseUp: string;
    FEventOnEditorCaret: string;
    FEventOnEditorScroll: string;
    FEventOnEditorKeyDown: string;
    FEventOnEditorKeyUp: string;
    FEventOnEditorClickGutter: string;
    FEventOnEditorClickGap: string;
    FEventOnEditorClickLink: string;
    FEventOnEditorPaste: string;
    constructor Create(const ATypeString: string);
  end;


type
  { TFormDummy }

  TFormDummy = class(TForm)
  private
    IsFormShownAlready: boolean;
    procedure DoOnFormWindowStateChange(Sender: TObject);
    procedure _HandleClickEvent(Sender: TObject; ADblClick: boolean);
    procedure _HandleMouseEvent(Sender: TObject;
      const AEventKind: TAppCtlMouseEvent; const AData: TAppVariant);
  protected
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure UTF8KeyPress(var UTF8Key: TUTF8Char); override;
    procedure MouseEnter; override;
    procedure MouseLeave; override;
    procedure DoShow; override;
    procedure DoHide; override;
    procedure Activate; override;
    procedure Deactivate; override;
    procedure DoClose(var CloseAction: TCloseAction); override;
  public
    IsDlgCustom: boolean;
    IsDlgModalEmulated: boolean;
    IsDlgCounterIgnored: boolean;
    IdClicked: integer;
    ShowInTaskbar_Pending: TShowInTaskbar;
    FEventOnClose: string;
    FEventOnCloseQuery: string;
    FEventOnKeyDown: string;
    FEventOnKeyUp: string;
    FEventOnKeyPress: string;
    FEventOnResize: string;
    FEventOnActivate: string;
    FEventOnDeactivate: string;
    FEventOnMouseEnter: string;
    FEventOnMouseExit: string;
    FEventOnShow: string;
    FEventOnHide: string;
    FEventOnFormState: string;
    TagString: string;
    PrevForms: TFPList;
    PrevBorderStyle: TFormBorderStyle;
    BlockedOnChange: boolean;
    BlockedOnSelect_Listview: boolean;
    BlockedOnSelect_Treeview: boolean;
    BlockedOnFold: boolean;
    BlockedOnUnfold: boolean;
    function IdFocused: integer;
    procedure SetFocus; override;
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    function CloseQuery: boolean; override;
    procedure DoOnResize; override;
    procedure DoOnClick(Sender: TObject);
    procedure DoOnClickX(Sender: TObject);
    procedure DoOnDblClick(Sender: TObject);
    procedure DoOnChange(Sender: TObject);
    procedure DoOnCheckGroupClicked(Sender: TObject; AIndex: integer);
    procedure DoOnListboxSelect(Sender: TObject; User: boolean);
    procedure DoOnListboxDrawItem(Sender: TObject; ACanvas: TCanvas; AIndex: integer; const ARect: TRect);
    procedure DoOnListboxClickHeader(Sender: TObject; AIndex: integer);
    procedure DoOnListviewChange(Sender: TObject; Item: TListItem; Change: TItemChange);
    procedure DoOnListviewSelect(Sender: TObject; Item: TListItem; Selected: Boolean);
    procedure DoOnListviewColumnClick(Sender: TObject; Column: TListColumn);
    procedure DoOnTreeviewChange(Sender: TObject; Node: TTreeNode);
    procedure DoOnTreeviewSelect(Sender: TObject);
    procedure DoOnTreeviewExpanding(Sender: TObject; Node: TTreeNode; var AllowExpansion: Boolean);
    procedure DoOnTreeviewCollapsing(Sender: TObject; Node: TTreeNode; var AllowCollapse: Boolean);
    procedure DoOnStatusbarPanelClick(Sender: TObject; AIndex: integer);
    procedure DoOnControlSelect(Sender: TObject);
    procedure DoOnControlFocusEnter(Sender: TObject);
    procedure DoOnControlFocusExit(Sender: TObject);
    procedure DoOnControlMenu(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
    procedure DoOnControlMouseEnter(Sender: TObject);
    procedure DoOnControlMouseLeave(Sender: TObject);
    procedure DoOnControlMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure DoOnControlMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure DoOnImagePaintBackground(ASender: TObject; ACanvas: TCanvas; ARect: TRect);
    procedure DoOnEditorChange(Sender: TObject);
    procedure DoOnEditorChangeCaretPos(Sender: TObject);
    procedure DoOnEditorScroll(Sender: TObject);
    procedure DoOnEditorKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure DoOnEditorKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure DoOnEditorClickGutter(Sender: TObject; ABand, ALine: integer; var AHandled: boolean);
    procedure DoOnEditorClickGap(Sender: TObject; AGapItem: TATGapItem; APos: TPoint);
    procedure DoOnEditorClickLink(Sender: TObject; const ALink: string);
    procedure DoOnEditorPaste(Sender: TObject; var AHandled: boolean; AKeepCaret, ASelectThen: boolean);
    function DoEvent(AIdControl: integer; const ACallback: string; const AData: TAppVariant): boolean;
    procedure DoEmulatedModalShow;
    procedure DoEmulatedModalClose;
    function FindControlByIndex(AIndex: integer): TControl;
    function FindControlByOurName(const AName: string): TControl;
    function FindControlIndexByOurObject(Sender: TObject): integer;
    function FindControlIndexByOurName(const AName: string): integer;
    procedure FixPositionIfOutOfScreen;
  end;


function IsEventItemListed(const SItem, SList: string): boolean;

const
  cAppFormStateStr: array[TWindowState] of string = ('norm', 'min', 'max', 'fullscr');

implementation

function DoControl_Target(C: TControl): TControl;
begin
  if C is TAppTreeContainer then
    Result:= TAppTreeContainer(C).Tree
  else
    Result:= C;
end;

function AppVariant_KeyData(AKey: word; AShift: TShiftState): TAppVariant;
begin
  Result:= Default(TAppVariant);
  Result.Typ:= avrTuple;
  SetLength(Result.Items, 2);

  Result.Items[0].Typ:= avdInt;
  Result.Items[0].Int:= AKey;

  Result.Items[1].Typ:= avdStr;
  Result.Items[1].Str:= ConvertShiftStateToString(AShift);
end;


function AppVariant_MouseData(AButton: TMouseButton; AShift: TShiftState; AX, AY: Integer): TAppVariant;
begin
  Result:= Default(TAppVariant);
  Result.Typ:= avrDict;
  SetLength(Result.Items, 4);

  Result.Items[0].KeyName:= 'btn';
  Result.Items[0].Typ:= avdInt;
  Result.Items[0].Int:= Ord(AButton);

  Result.Items[1].KeyName:= 'state';
  Result.Items[1].Typ:= avdStr;
  Result.Items[1].Str:= ConvertShiftStateToString(AShift);

  Result.Items[2].KeyName:= 'x';
  Result.Items[2].Typ:= avdInt;
  Result.Items[2].Int:= AX;

  Result.Items[3].KeyName:= 'y';
  Result.Items[3].Typ:= avdInt;
  Result.Items[3].Int:= AY;
end;


procedure DoForm_SetupFilters(F: TFormDummy);
const
  cPrefix = 'f_';
var
  SName: string;
  C: TComponent;
  C2: TControl;
  CFilterListbox: TListFilterEdit;
  CFilterListview: TListViewFilterEdit;
  i: integer;
begin
  for i:= 0 to F.ComponentCount-1 do
  begin
    C:= F.Components[i];
    if not (C is TControl) then Continue;
    if C.Tag=0 then Continue;
    SName:= TAppControlProps(C.Tag).FName;

    if C is TListFilterEdit then
    begin
      SName:= Copy(SName, Length(cPrefix)+1, MaxInt);
      C2:= F.FindControlByOurName(SName);
      if not (C2 is TListbox) then Continue;

      CFilterListbox:= C as TListFilterEdit;
      CFilterListbox.FilteredListbox:= C2 as TListbox;

      SName:= CFilterListbox.Text;
      CFilterListbox.Text:= '';
      CFilterListbox.Text:= SName;
    end
    else
    if C is TListViewFilterEdit then
    begin
      SName:= Copy(SName, Length(cPrefix)+1, MaxInt);
      C2:= F.FindControlByOurName(SName);
      if not (C2 is TListView) then Continue;

      CFilterListview:= C as TListViewFilterEdit;
      CFilterListview.FilteredListview:= C2 as TListView;

      SName:= CFilterListview.Text;
      CFilterListview.Text:= '';
      CFilterListview.Text:= SName;
    end;
  end;
end;


{ TAppControlProps }

constructor TAppControlProps.Create(const ATypeString: string);
begin
  inherited Create;
  FName:= '';
  FActive:= false;
  FTypeString:= ATypeString;
  FTagString:= '';
  FEventOnChange:= '';
end;

{ TFormDummy }

constructor TFormDummy.Create(TheOwner: TComponent);
const
  cTaskbarModes: array[boolean] of TShowInTaskbar = (stNever, stAlways);
begin
  inherited CreateNew(TheOwner);

  BorderStyle:= bsDialog;
  BorderIcons:= [biSystemMenu];
  KeyPreview:= true;
  Position:= poMainFormCenter;
  ShowHint:= true;
  Scaled:= false;
  ShowInTaskbar_Pending:= cTaskbarModes[UiOps.PluginDialogsShowInTaskbar];

  IsDlgCustom:= false;
  IsFormShownAlready:= false;
  IdClicked:= -1;
  TagString:= '';

  OnWindowStateChange:= @DoOnFormWindowStateChange;

  PrevBorderStyle:= BorderStyle;
  PrevForms:= TFPList.Create;

  AppCustomDialogs.Add(Self);
end;

destructor TFormDummy.Destroy;
var
  Ctl: TComponent;
  i, n: integer;
begin
  for i:= ComponentCount-1 downto 0 do
  begin
    Ctl:= Components[i];
    if Ctl.Tag<>0 then
    begin
      TObject(Ctl.Tag).Free;
      Ctl.Tag:= 0;
    end;
  end;

  n:= AppCustomDialogs.IndexOf(Self);
  if n>=0 then
    AppCustomDialogs.Delete(n);

  FreeAndNil(PrevForms);
  inherited;
end;

procedure TFormDummy.DoShow;
var
  C: TComponent;
  i: integer;
begin
  inherited;

  IsFormShownAlready:= true;

  if not IsDlgCounterIgnored then
    Inc(AppApiDialogCounter);

  DoForm_SetupFilters(Self);

  for i:= 0 to ComponentCount-1 do
  begin
    C:= Components[i];
    if C is TListview then
      with (C as TListview) do
        if ItemFocused<>nil then
          ItemFocused.MakeVisible(false);
  end;

  DoEvent(-1, FEventOnShow, AppVariantNil);
end;

procedure TFormDummy.DoHide;
begin
  inherited;

  DoEvent(-1, FEventOnHide, AppVariantNil);

  if not IsDlgCounterIgnored then
    AppApiDialogCounter:= Max(0, AppApiDialogCounter-1);
end;

procedure TFormDummy.MouseEnter;
begin
  inherited;
  DoEvent(-1, FEventOnMouseEnter, AppVariantNil);
end;

procedure TFormDummy.MouseLeave;
begin
  inherited;
  DoEvent(-1, FEventOnMouseExit, AppVariantNil);
end;

procedure TFormDummy.Activate;
begin
  inherited;
  DoEvent(-1, FEventOnActivate, AppVariantNil);
end;

procedure TFormDummy.Deactivate;
begin
  inherited;
  DoEvent(-1, FEventOnDeactivate, AppVariantNil);
end;

procedure TFormDummy._HandleClickEvent(Sender: TObject; ADblClick: boolean);
var
  Callback: string;
  Props: TAppControlProps;
  IdControl: integer;
  Data: TAppVariant;
  P: TPoint;
begin
  Props:= TAppControlProps((Sender as TControl).Tag);
  if ADblClick then
    Callback:= Props.FEventOnClickDbl
  else
    Callback:= Props.FEventOnClick;
  if Callback='' then exit;

  IdControl:= FindControlIndexByOurObject(Sender);
  P:= (Sender as TControl).ScreenToClient(Mouse.CursorPos);

  Data:= Default(TAppVariant);
  Data.Typ:= avrTuple;
  SetLength(Data.Items, 2);

  Data.Items[0].Typ:= avdInt;
  Data.Items[0].Int:= P.X;

  Data.Items[1].Typ:= avdInt;
  Data.Items[1].Int:= P.Y;

  DoEvent(IdControl, Callback, Data);
end;

procedure TFormDummy.DoOnControlMouseEnter(Sender: TObject);
begin
  _HandleMouseEvent(Sender, cControlEventMouseEnter, AppVariantNil);
end;

procedure TFormDummy.DoOnControlMouseLeave(Sender: TObject);
begin
  _HandleMouseEvent(Sender, cControlEventMouseExit, AppVariantNil);
end;

procedure TFormDummy.DoOnControlMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  _HandleMouseEvent(Sender, cControlEventMouseDown,
    AppVariant_MouseData(Button, Shift, X, Y)
    );
end;

procedure TFormDummy.DoOnControlMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  _HandleMouseEvent(Sender, cControlEventMouseUp,
    AppVariant_MouseData(Button, Shift, X, Y)
    );
end;

procedure TFormDummy.DoOnImagePaintBackground(ASender: TObject;
  ACanvas: TCanvas; ARect: TRect);
var
  Img: TImage;
begin
  Img:= ASender as TImage;
  DoPaintCheckers(
    Img.Canvas,
    Img.Width,
    Img.Height,
    8,
    clWhite,
    clLtGray
    );
end;

procedure TFormDummy.DoOnClick(Sender: TObject);
begin
  _HandleClickEvent(Sender, false);
end;

procedure TFormDummy.DoOnClickX(Sender: TObject);
var
  Props: TAppControlProps;
  IdControl: integer;
begin
  Props:= TAppControlProps((Sender as TControl).Tag);
  IdControl:= FindControlIndexByOurObject(Sender);
  DoEvent(IdControl, Props.FEventOnClickX, AppVariantNil);
end;

procedure TFormDummy.DoOnDblClick(Sender: TObject);
begin
  _HandleClickEvent(Sender, true);
end;

procedure TFormDummy.KeyDown(var Key: Word; Shift: TShiftState);
{
Wiki:
    param "id_ctl": int key code.
    param "data": key-state string
}
var
  Form: TCustomForm;
  Data: TAppVariant;
begin
  inherited;
  Data:= AppVariant(ConvertShiftStateToString(Shift));

  if not DoEvent(Key, FEventOnKeyDown, Data) then
  begin
    Key:= 0;
    exit;
  end;

  if (Key=VK_ESCAPE) and (Shift=[]) then
  begin
    IdClicked:= -1;

    if Assigned(Parent) then //form is embedded e.g. to side panel
    begin
      //forward Esc press to main form
      Form:= GetParentForm(Self, true);
      if Assigned(Form) and Assigned(Form.OnKeyDown) then
        Form.OnKeyDown(nil, Key, Shift);
    end
    else
    if fsModal in FFormState then
      ModalResult:= mrCancel
    else
      Close;

    Key:= 0;
    exit;
  end;

  //support F12 (default: toggle side panel) in Project Manager with hidden MainMenu
  //MainForm.OnKeyDown must handle F1..F12 as well
  if (Key>=VK_F1) and (Key<=VK_F24) then
  begin
    Form:= Application.MainForm;
    if Assigned(Form) and Assigned(Form.OnKeyDown) then
      Form.OnKeyDown(nil, Key, Shift);
  end;
end;

procedure TFormDummy.KeyUp(var Key: Word; Shift: TShiftState);
{
Wiki:
    param "id_ctl": int key code.
    param "data": key-state string
}
var
  Data: TAppVariant;
begin
  inherited;
  Data:= AppVariant(ConvertShiftStateToString(Shift));

  if not DoEvent(Key, FEventOnKeyUp, Data) then
  begin
    Key:= 0;
    exit;
  end;
end;

procedure TFormDummy.UTF8KeyPress(var UTF8Key: TUTF8Char);
var
  Data: TAppVariant;
  SWide: UnicodeString;
begin
  inherited;
  Data:= AppVariant(ConvertShiftStateToString(GetKeyShiftState));

  SWide:= UTF8Decode(UTF8Key);
  if not DoEvent(Ord(SWide[1]), FEventOnKeyPress, Data) then
  begin
    UTF8Key:= #0;
    exit;
  end;
end;

procedure TFormDummy.DoOnResize;
begin
  //bsNone needed too: issue #3119
  if not (BorderStyle in [bsNone, bsSizeable, bsSizeToolWin]) then exit;

  if not IsFormShownAlready then exit;
  DoEvent(-1, FEventOnResize, AppVariantNil);
end;

function TFormDummy.CloseQuery: boolean;
begin
  inherited;
  Result:= DoEvent(-1, FEventOnCloseQuery, AppVariantNil);
end;

procedure TFormDummy.DoOnFormWindowStateChange(Sender: TObject);
var
  Param: TAppVariant;
begin
  //this is for https://github.com/Alexey-T/CudaText/issues/3078
  //prevent minimizing the modal-emulated form
  if IsDlgModalEmulated then
    if WindowState=wsMinimized then
    begin
      WindowState:= wsNormal;
      Show;
    end;

  Param:= AppVariant(cAppFormStateStr[WindowState]);
  DoEvent(-1, FEventOnFormState, Param);
end;

procedure TFormDummy.DoOnControlMenu(Sender: TObject; MousePos: TPoint;
  var Handled: Boolean);
var
  Props: TAppControlProps;
  IdControl: integer;
begin
  Props:= TAppControlProps((Sender as TControl).Tag);
  if Props.FEventOnMenu='' then exit;
  IdControl:= FindControlIndexByOurObject(Sender);
  Handled:= not DoEvent(IdControl, Props.FEventOnMenu,
    AppVariant_MouseData(mbRight, GetKeyShiftState, MousePos.X, MousePos.Y)
    );
end;


procedure TFormDummy.DoClose(var CloseAction: TCloseAction);
begin
  inherited;
  CloseAction:= caHide; //caFree gives crash on btn clicks on win
  if IsDlgCustom then exit;

  DoEmulatedModalClose;
  IdClicked:= -1;
  DoEvent(-1, FEventOnClose, AppVariantNil);
end;

function TFormDummy.IdFocused: integer;
var
  C: TWinControl;
begin
  if Parent=nil then
    C:= ActiveControl
  else
    //ActiveControl works bad if form is docked
    // https://github.com/Alexey-T/CudaText/issues/2618
    C:= FindOwnerControl(GetFocus); // like TWinControl.Focused does

  Result:= FindControlIndexByOurObject(C);
end;

procedure TFormDummy.SetFocus;
var
  Ctl: TControl;
  i: integer;
begin
  inherited;

  //find first "big" control and focus it
  for i:= 0 to ControlCount-1 do
  begin
    Ctl:= Controls[i];
    if (Ctl is TAppTreeContainer) or
      (Ctl is TListView) or
      (Ctl is TMemo) or
      (Ctl is TATListbox) or
      (Ctl is TATSynEdit) then
    begin
      if Ctl.Enabled and Ctl.Visible and (Ctl as TWinControl).CanFocus then
        (DoControl_Target(Ctl) as TWinControl).SetFocus;
      Break;
    end;
  end;
end;


function TFormDummy.FindControlByIndex(AIndex: integer): TControl;
var
  C: TComponent;
begin
  Result:= nil;
  if (AIndex>=0) and (AIndex<ComponentCount) then
  begin
    C:= Components[AIndex];
    if C is TControl then
      Result:= C as TControl;
  end;
end;

function TFormDummy.FindControlIndexByOurObject(Sender: TObject): integer;
var
  i: integer;
begin
  Result:= -1;
  for i:= 0 to ComponentCount-1 do
    if Components[i]=Sender then
      exit(i);
end;

function TFormDummy.FindControlByOurName(const AName: string): TControl;
var
  C: TComponent;
  i: integer;
begin
  Result:= nil;
  for i:= 0 to ComponentCount-1 do
  begin
    C:= Components[i];
    if C.Tag=0 then Continue;
    if SameText(TAppControlProps(C.Tag).FName, AName) then
      if C is TControl then
        exit(C as TControl);
  end;
end;

function TFormDummy.FindControlIndexByOurName(const AName: string): integer;
var
  C: TComponent;
  i: integer;
begin
  Result:= -1;
  for i:= 0 to ComponentCount-1 do
  begin
    C:= Components[i];
    if C.Tag=0 then Continue;
    if SameText(TAppControlProps(C.Tag).FName, AName) then
      exit(i);
  end;
end;

procedure TFormDummy.FixPositionIfOutOfScreen;
{$ifdef windows}
var
  R: TRect;
  {$endif}
begin
  {$ifdef windows}
  MoveToDefaultPosition;
  Position:= poDesigned;

  R:= Monitor.WorkareaRect;
  //move window from the bottom
  if Top+Height>R.Bottom then
    Top:= R.Bottom-Height;
  //move window from the top
  if Top<R.Top then
    Top:= R.Top;
  {$endif}
end;


procedure TFormDummy.DoOnChange(Sender: TObject);
var
  Props: TAppControlProps;
begin
  if BlockedOnChange then exit;

  //workaround for bug on Mac
  //(flickering on More>> press in "Backup File" plugin dialog)
  if not IsFormShownAlready then exit;

  Props:= TAppControlProps((Sender as TControl).Tag);
  if not Props.FActive then exit;

  IdClicked:= FindControlIndexByOurObject(Sender);

  if IsDlgCustom then
  begin
    ModalResult:= mrOk;
    exit;
  end;

  BlockedOnChange:= true;
  try
    DoEvent(IdClicked, Props.FEventOnChange, AppVariantNil);
  finally
    BlockedOnChange:= false;
  end;
end;

procedure TFormDummy.DoOnCheckGroupClicked(Sender: TObject; AIndex: integer);
begin
  DoOnChange(Sender);
end;

procedure TFormDummy.DoOnListboxSelect(Sender: TObject; User: boolean);
begin
  //here called on_change, not on_select
  //reason: for Listbox/CheckListbox, value is selected index, so sel change - on_change
  DoOnChange(Sender);
end;

procedure TFormDummy._HandleMouseEvent(Sender: TObject;
  const AEventKind: TAppCtlMouseEvent; const AData: TAppVariant);
var
  Props: TAppControlProps;
  IdControl: integer;
  SCallback: string;
begin
  Props:= TAppControlProps((Sender as TControl).Tag);
  IdControl:= FindControlIndexByOurObject(Sender);

  case AEventKind of
    cControlEventMouseEnter:
      SCallback:= Props.FEventOnMouseEnter;
    cControlEventMouseExit:
      SCallback:= Props.FEventOnMouseExit;
    cControlEventMouseDown:
      SCallback:= Props.FEventOnMouseDown;
    cControlEventMouseUp:
      SCallback:= Props.FEventOnMouseUp;
    else
      SCallback:= '';
  end;

  if SCallback<>'' then
    DoEvent(IdControl, SCallback, AData);
end;

procedure TFormDummy.DoOnListboxDrawItem(Sender: TObject; ACanvas: TCanvas;
  AIndex: integer; const ARect: TRect);
var
  Props: TAppControlProps;
  IdControl: integer;
  Data: TAppVariant;
  Callback: string;
begin
  if Sender is TATListbox then
    if not TATListbox(Sender).IsIndexValid(AIndex) then exit;

  Props:= TAppControlProps((Sender as TControl).Tag);
  Callback:= Props.FEventOnListboxDrawItem;
  if Callback='' then exit;
  IdControl:= FindControlIndexByOurObject(Sender);

  Data:= Default(TAppVariant);
  Data.Typ:= avrDict;
  SetLength(Data.Items, 3);

  Data.Items[0].KeyName:= 'canvas';
  Data.Items[0].Typ:= avdInt;
  Data.Items[0].Int:= PtrInt(ACanvas);

  Data.Items[1].KeyName:= 'index';
  Data.Items[1].Typ:= avdInt;
  Data.Items[1].Int:= AIndex;

  Data.Items[2].KeyName:= 'rect';
  Data.Items[2].Typ:= avdRect;
  Data.Items[2].Rect:= ARect;

  DoEvent(IdControl, Callback, Data);
    (*
    Format('{ "canvas": %d, "index": %d, "rect": (%d,%d,%d,%d) }', [
      PtrInt(ACanvas),
      AIndex,
      ARect.Left,
      ARect.Top,
      ARect.Right,
      ARect.Bottom
    ]));
    *)
end;

procedure TFormDummy.DoOnListviewChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
begin
  DoOnChange(Sender);
end;

procedure TFormDummy.DoOnListviewSelect(Sender: TObject; Item: TListItem;
  Selected: Boolean);
var
  Props: TAppControlProps;
  IdControl: integer;
  Data: TAppVariant;
  Callback: string;
begin
  if BlockedOnSelect_Listview then exit;
  BlockedOnSelect_Listview:= true;

  Props:= TAppControlProps((Sender as TControl).Tag);
  Callback:= Props.FEventOnSelect;
  if Callback='' then exit;
  IdControl:= FindControlIndexByOurObject(Sender);

  Data:= Default(TAppVariant);
  Data.Typ:= avrTuple;
  SetLength(Data.Items, 2);

  Data.Items[0].Typ:= avdInt;
  Data.Items[0].Int:= Item.Index;

  Data.Items[1].Typ:= avdBool;
  Data.Items[1].Bool:= Selected;

  try
    DoEvent(IdControl, Callback, Data);
  finally
    BlockedOnSelect_Listview:= false;
  end;
end;

procedure TFormDummy.DoOnListviewColumnClick(Sender: TObject; Column: TListColumn);
var
  Props: TAppControlProps;
  IdControl: integer;
  Data: TAppVariant;
begin
  Props:= TAppControlProps((Sender as TControl).Tag);
  IdControl:= FindControlIndexByOurObject(Sender);
  Data:= AppVariant(Column.Index);
  DoEvent(IdControl, Props.FEventOnClickHeader, Data);
end;

procedure TFormDummy.DoOnListboxClickHeader(Sender: TObject; AIndex: integer);
var
  Props: TAppControlProps;
  IdControl: integer;
  Data: TAppVariant;
begin
  Props:= TAppControlProps((Sender as TControl).Tag);
  IdControl:= FindControlIndexByOurObject(Sender);
  Data:= AppVariant(AIndex);
  DoEvent(IdControl, Props.FEventOnClickHeader, Data);
end;


function IsEventItemListed(const SItem, SList: string): boolean;
begin
  if SList='' then exit(false);
  if SList='*' then exit(true);
  Result:= Pos(','+SItem+',', ','+SList+',')>0;
end;


function TFormDummy.DoEvent(AIdControl: integer; const ACallback: string;
  const AData: TAppVariant): boolean;
var
  ParamVars: TAppVariantArray;
  ParamNames: array of string;
begin
  if ACallback='' then exit(true);

  ParamVars:= [
    AppVariant(PtrInt(Self)),
    AppVariant(AIdControl)
    ];
  ParamNames:= [
    'id_dlg',
    'id_ctl'
    ];

  if AData.Typ<>avrNil then
  begin
    ParamVars:= Concat(ParamVars, [AData]);
    ParamNames:= Concat(ParamNames, ['data']);
  end;

  Result:= AppCustomDialog_DoPyCallback(ACallback, ParamVars, ParamNames);
end;

procedure TFormDummy.DoEmulatedModalShow;
var
  F: TForm;
  i: integer;
begin
  for i:= 0 to Screen.FormCount-1 do
  begin
    F:= Screen.Forms[i];
    if F=Self then Continue; //skip self
    if F=Self.Parent then Continue;
    if F.Parent<>nil then Continue; //skip docked
    if F.Enabled then
    begin
      PrevForms.Add(F);
      F.Enabled:= false;
    end;
  end;

  //turn off 'minimize icon', it will be a problem for modal form on Linux
  BorderIcons:= BorderIcons-[biMinimize];

  FormStyle:= UiOps.PluginDialogsModalFormStyle;
  IsDlgModalEmulated:= true;
  Show;
end;

procedure TFormDummy.DoEmulatedModalClose;
var
  i: integer;
begin
  for i:= PrevForms.Count-1 downto 0 do
    TForm(PrevForms[i]).Enabled:= true;
  PrevForms.Clear;
  IsDlgModalEmulated:= false;
end;

procedure TFormDummy.DoOnTreeviewChange(Sender: TObject; Node: TTreeNode);
var
  Props: TAppControlProps;
  IdControl: integer;
  Data: TAppVariant;
begin
  Props:= TAppControlProps((Sender as TControl).Tag);
  IdControl:= FindControlIndexByOurObject(Sender);
  Data:= AppVariant(PtrInt(Node));
  DoEvent(IdControl, Props.FEventOnChange, Data);
end;

procedure TFormDummy.DoOnTreeviewExpanding(Sender: TObject; Node: TTreeNode;
  var AllowExpansion: Boolean);
var
  Props: TAppControlProps;
  IdControl: integer;
  Data: TAppVariant;
begin
  if BlockedOnUnfold then exit;

  Props:= TAppControlProps((Sender as TControl).Tag);
  IdControl:= FindControlIndexByOurObject(Sender);
  Data:= AppVariant(PtrInt(Node));
  DoEvent(IdControl, Props.FEventOnUnfold, Data);
end;

procedure TFormDummy.DoOnTreeviewCollapsing(Sender: TObject; Node: TTreeNode;
  var AllowCollapse: Boolean);
var
  Props: TAppControlProps;
  IdControl: integer;
  Data: TAppVariant;
begin
  if BlockedOnFold then exit;

  Props:= TAppControlProps((Sender as TControl).Tag);
  IdControl:= FindControlIndexByOurObject(Sender);
  Data:= AppVariant(PtrInt(Node));
  DoEvent(IdControl, Props.FEventOnFold, Data);
end;

procedure TFormDummy.DoOnStatusbarPanelClick(Sender: TObject; AIndex: integer);
var
  Bar: TATStatus;
  BarData: TATStatusData;
  //Props: TAppControlProps;
  IdControl: integer;
  Data: TAppVariant;
begin
  if not (Sender is TATStatus) then exit;
  Bar:= Sender as TATStatus;
  BarData:= Bar.GetPanelData(AIndex);
  if Assigned(BarData) and (BarData.Callback<>'') then
  begin
    //Props:= TAppControlProps((Sender as TControl).Tag);
    IdControl:= FindControlIndexByOurObject(Sender);
    Data:= AppVariant(BarData.Tag);
    DoEvent(IdControl, BarData.Callback, Data);
  end;
end;

procedure TFormDummy.DoOnTreeviewSelect(Sender: TObject);
var
  Props: TAppControlProps;
  IdControl: integer;
begin
  if BlockedOnSelect_Treeview then exit;
  BlockedOnSelect_Treeview:= true;
  try
    Props:= TAppControlProps((Sender as TControl).Tag);
    IdControl:= FindControlIndexByOurObject(Sender);
    DoEvent(IdControl, Props.FEventOnSelect, AppVariantNil);
  finally
    BlockedOnSelect_Treeview:= false;
  end;
end;

procedure TFormDummy.DoOnControlSelect(Sender: TObject);
var
  Props: TAppControlProps;
  IdControl: integer;
begin
  Props:= TAppControlProps((Sender as TControl).Tag);
  IdControl:= FindControlIndexByOurObject(Sender);
  DoEvent(IdControl, Props.FEventOnSelect, AppVariantNil);
end;

procedure TFormDummy.DoOnControlFocusEnter(Sender: TObject);
var
  Props: TAppControlProps;
  IdControl: integer;
begin
  Props:= TAppControlProps((Sender as TControl).Tag);
  IdControl:= FindControlIndexByOurObject(Sender);
  DoEvent(IdControl, Props.FEventOnFocusEnter, AppVariantNil);
end;

procedure TFormDummy.DoOnControlFocusExit(Sender: TObject);
var
  Props: TAppControlProps;
  IdControl: integer;
begin
  Props:= TAppControlProps((Sender as TControl).Tag);
  IdControl:= FindControlIndexByOurObject(Sender);
  DoEvent(IdControl, Props.FEventOnFocusExit, AppVariantNil);
end;


procedure TFormDummy.DoOnEditorChange(Sender: TObject);
var
  Props: TAppControlProps;
  IdControl: integer;
begin
  Props:= TAppControlProps((Sender as TControl).Tag);
  if not Props.FActive then exit; //fire on_change only if 'act':True
  IdControl:= FindControlIndexByOurObject(Sender);
  DoEvent(IdControl, Props.FEventOnChange, AppVariantNil);
end;

procedure TFormDummy.DoOnEditorChangeCaretPos(Sender: TObject);
var
  Props: TAppControlProps;
  IdControl: integer;
begin
  Props:= TAppControlProps((Sender as TControl).Tag);
  IdControl:= FindControlIndexByOurObject(Sender);
  DoEvent(IdControl, Props.FEventOnEditorCaret, AppVariantNil);
end;

procedure TFormDummy.DoOnEditorKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
{
Wiki:
Param "data" is tuple (int_key_code, str_key_state).
}
var
  Props: TAppControlProps;
  IdControl: integer;
  Data: TAppVariant;
  Callback: string;
begin
  Props:= TAppControlProps((Sender as TControl).Tag);
  Callback:= Props.FEventOnEditorKeyDown;
  if Callback='' then exit;
  IdControl:= FindControlIndexByOurObject(Sender);
  Data:= AppVariant_KeyData(Key, Shift);
  if not DoEvent(IdControl, Callback, Data) then
    Key:= 0;
end;

procedure TFormDummy.DoOnEditorKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
{
Wiki:
Param "data" is tuple (int_key_code, str_key_state).
}
var
  Props: TAppControlProps;
  IdControl: integer;
  Data: TAppVariant;
  Callback: string;
begin
  Props:= TAppControlProps((Sender as TControl).Tag);
  Callback:= Props.FEventOnEditorKeyUp;
  if Callback='' then exit;
  IdControl:= FindControlIndexByOurObject(Sender);
  Data:= AppVariant_KeyData(Key, Shift);
  if not DoEvent(IdControl, Callback, Data) then
    Key:= 0;
end;


procedure TFormDummy.DoOnEditorClickGutter(Sender: TObject; ABand, ALine: integer;
  var AHandled: boolean);
var
  Props: TAppControlProps;
  IdControl: integer;
  Data: TAppVariant;
  Callback: string;
begin
  Props:= TAppControlProps((Sender as TControl).Tag);
  Callback:= Props.FEventOnEditorClickGutter;
  if Callback='' then exit;
  IdControl:= FindControlIndexByOurObject(Sender);

  Data:= Default(TAppVariant);
  Data.Typ:= avrDict;
  SetLength(Data.Items, 3);

  Data.Items[0].KeyName:= 'state';
  Data.Items[0].Typ:= avdStr;
  Data.Items[0].Str:= ConvertShiftStateToString(KeyboardStateToShiftState);

  Data.Items[1].KeyName:= 'line';
  Data.Items[1].Typ:= avdInt;
  Data.Items[1].Int:= ALine;

  Data.Items[2].KeyName:= 'band';
  Data.Items[2].Typ:= avdInt;
  Data.Items[2].Int:= ABand;

  if not DoEvent(IdControl, Callback, Data) then
    AHandled:= true;
end;

procedure TFormDummy.DoOnEditorClickGap(Sender: TObject; AGapItem: TATGapItem; APos: TPoint);
var
  Props: TAppControlProps;
  IdControl: integer;
  Data: TAppVariant;
  Callback: string;
begin
  if not Assigned(AGapItem) then exit;

  Props:= TAppControlProps((Sender as TControl).Tag);
  Callback:= Props.FEventOnEditorClickGap;
  if Callback='' then exit;
  IdControl:= FindControlIndexByOurObject(Sender);

  Data:= Default(TAppVariant);
  Data.Typ:= avrDict;
  SetLength(Data.Items, 7);

  Data.Items[0].KeyName:= 'state';
  Data.Items[0].Typ:= avdStr;
  Data.Items[0].Str:= ConvertShiftStateToString(KeyboardStateToShiftState);

  Data.Items[1].KeyName:= 'line';
  Data.Items[1].Typ:= avdInt;
  Data.Items[1].Int:= AGapItem.LineIndex;

  Data.Items[2].KeyName:= 'tag';
  Data.Items[2].Typ:= avdInt;
  Data.Items[2].Int:= AGapItem.Tag;

  Data.Items[3].KeyName:= 'gap_w';
  Data.Items[3].Typ:= avdInt;
  Data.Items[3].Int:= AGapItem.Bitmap.Width;

  Data.Items[4].KeyName:= 'gap_h';
  Data.Items[4].Typ:= avdInt;
  Data.Items[4].Int:= AGapItem.Bitmap.Height;

  Data.Items[5].KeyName:= 'x';
  Data.Items[5].Typ:= avdInt;
  Data.Items[5].Int:= APos.X;

  Data.Items[6].KeyName:= 'y';
  Data.Items[6].Typ:= avdInt;
  Data.Items[6].Int:= APos.Y;

  DoEvent(IdControl, Callback, Data);
    (*
    Format('{ "state": "%s", "line": %d, "tag": %d, "gap_w": %d, "gap_h": %d, "x": %d, "y": %d }', [
        ConvertShiftStateToString(KeyboardStateToShiftState),
        AGapItem.LineIndex,
        AGapItem.Tag,
        AGapItem.Bitmap.Width,
        AGapItem.Bitmap.Height,
        APos.X,
        APos.Y
    ]));
    *)
end;

procedure TFormDummy.DoOnEditorClickLink(Sender: TObject; const ALink: string);
var
  Props: TAppControlProps;
  IdControl: integer;
  Data: TAppVariant;
  Callback: string;
begin
  Props:= TAppControlProps((Sender as TControl).Tag);
  Callback:= Props.FEventOnEditorClickLink;
  if Callback='' then exit;
  IdControl:= FindControlIndexByOurObject(Sender);

  Data:= Default(TAppVariant);
  Data.Typ:= avrStr;
  Data.Str:= ALink;

  DoEvent(IdControl, Callback, Data);
end;

procedure TFormDummy.DoOnEditorScroll(Sender: TObject);
var
  Props: TAppControlProps;
  IdControl: integer;
begin
  Props:= TAppControlProps((Sender as TControl).Tag);
  IdControl:= FindControlIndexByOurObject(Sender);
  DoEvent(IdControl, Props.FEventOnEditorScroll, AppVariantNil);
end;

procedure TFormDummy.DoOnEditorPaste(Sender: TObject; var AHandled: boolean; AKeepCaret, ASelectThen: boolean);
var
  Props: TAppControlProps;
  IdControl: integer;
  Data: TAppVariant;
  Callback: string;
begin
  Props:= TAppControlProps((Sender as TControl).Tag);
  Callback:= Props.FEventOnEditorPaste;
  if Callback='' then exit;
  IdControl:= FindControlIndexByOurObject(Sender);

  Data:= Default(TAppVariant);
  Data.Typ:= avrDict;
  SetLength(Data.Items, 2);

  Data.Items[0].KeyName:= 'keep_caret';
  Data.Items[0].Typ:= avdBool;
  Data.Items[0].Bool:= AKeepCaret;

  Data.Items[1].KeyName:= 'sel_then';
  Data.Items[1].Typ:= avdBool;
  Data.Items[1].Bool:= ASelectThen;

  if not DoEvent(IdControl, Callback, Data) then
    AHandled:= true;
end;


initialization
  AppCustomDialogs:= TFPList.Create;

finalization
  AppCustomDialogs.Free;

end.

