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
  StdCtrls, Forms, ComCtrls, ExtCtrls,
  LCLType,
  ListFilterEdit,
  ListViewFilterEdit,
  proc_globdata,
  proc_miscutils,
  PythonEngine,
  ATSynEdit,
  ATSynEdit_Gaps;

type
  TAppPyCommonCallback = function(
    const ACallback: string;
    const AParams: array of PPyObject;
    const AParamNames: array of string): string;

type
  TAppCtlMouseEvent = (
    cControlEventMouseEnter,
    cControlEventMouseExit,
    cControlEventMouseDown,
    cControlEventMouseUp
    );

var
  CustomDialog_DoPyCallback: TAppPyCommonCallback = nil;
  CustomDialogs: TFPList;

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
    FEventOnEditorPaste: string;
    constructor Create(const ATypeString: string);
  end;


type
  { TFormDummy }

  TFormDummy = class(TForm)
  private
    IsFormShownAlready: boolean;
    procedure DoOnFormActivate(Sender: TObject);
    procedure DoOnFormDeactivate(Sender: TObject);
    procedure DoOnFormShow(Sender: TObject);
    procedure DoOnFormHide(Sender: TObject);
    procedure DoOnFormMouseEnter(Sender: TObject);
    procedure DoOnFormMouseLeave(Sender: TObject);
    procedure DoOnFormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure DoOnFormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure DoOnFormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure DoOnFormCloseQuery(Sender: TObject; var CanClose: boolean);
    function _MouseEventDataObject(AButton: TMouseButton; AShift: TShiftState; AX, AY: Integer): PPyObject;
    procedure _HandleClickEvent(Sender: TObject; ADblClick: boolean);
    procedure _HandleMouseEvent(Sender: TObject;
      const AEventKind: TAppCtlMouseEvent; AData: PPyObject);
  public
    IsDlgCustom: boolean;
    IdClicked: integer;
    FEventOnClose: string;
    FEventOnCloseQuery: string;
    FEventOnKeyDown: string;
    FEventOnKeyUp: string;
    FEventOnResize: string;
    FEventOnActivate: string;
    FEventOnDeactivate: string;
    FEventOnMouseEnter: string;
    FEventOnMouseExit: string;
    FEventOnShow: string;
    FEventOnHide: string;
    TagString: string;
    PrevForms: TFPList;
    PrevBorderStyle: TFormBorderStyle;
    BlockedOnChange: boolean;
    BlockedOnSelect_Listview: boolean;
    BlockedOnSelect_Treeview: boolean;
    BlockedOnFold: boolean;
    BlockedOnUnfold: boolean;
    function IdFocused: integer;
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure DoOnResize; override;
    procedure DoOnClick(Sender: TObject);
    procedure DoOnClickX(Sender: TObject);
    procedure DoOnDblClick(Sender: TObject);
    procedure DoOnChange(Sender: TObject);
    procedure DoOnListboxSelect(Sender: TObject; User: boolean);
    procedure DoOnListboxDrawItem(Sender: TObject; ACanvas: TCanvas; AIndex: integer; const ARect: TRect);
    procedure DoOnListviewChange(Sender: TObject; Item: TListItem; Change: TItemChange);
    procedure DoOnListviewSelect(Sender: TObject; Item: TListItem; Selected: Boolean);
    procedure DoOnListviewColumnClick(Sender: TObject; Column: TListColumn);
    procedure DoOnTreeviewChange(Sender: TObject; Node: TTreeNode);
    procedure DoOnTreeviewSelect(Sender: TObject);
    procedure DoOnTreeviewExpanding(Sender: TObject; Node: TTreeNode; var AllowExpansion: Boolean);
    procedure DoOnTreeviewCollapsing(Sender: TObject; Node: TTreeNode; var AllowCollapse: Boolean);
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
    procedure DoOnEditorClickGutter(Sender: TObject; ABand, ALine: integer);
    procedure DoOnEditorClickGap(Sender: TObject; AGapItem: TATGapItem; APos: TPoint);
    procedure DoOnEditorPaste(Sender: TObject; var AHandled: boolean; AKeepCaret, ASelectThen: boolean);
    function DoEvent(AIdControl: integer; const ACallback: string; AData: PPyObject): string;
    procedure DoEmulatedModalShow;
    procedure DoEmulatedModalClose;
    function FindControlByIndex(AIndex: integer): TControl;
    function FindControlByOurName(const AName: string): TControl;
    function FindControlIndexByOurObject(Sender: TObject): integer;
    function FindControlIndexByOurName(const AName: string): integer;
  end;


function IsEventItemListed(const SItem, SList: string): boolean;


implementation

const
  cPyFalse = 'False';
  cPyTrue = 'True';
  cPyFalseTrue: array[boolean] of string = ('False', 'True');


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
begin
  inherited CreateNew(TheOwner);

  BorderStyle:= bsDialog;
  BorderIcons:= [biSystemMenu];
  KeyPreview:= true;
  Position:= poMainFormCenter;
  ShowHint:= true;
  Scaled:= false;
  ShowInTaskBar:= stNever;

  IsDlgCustom:= false;
  IsFormShownAlready:= false;
  IdClicked:= -1;
  TagString:= '';

  OnShow:= @DoOnFormShow;
  OnHide:= @DoOnFormHide;
  OnClose:= @DoOnFormClose;
  OnCloseQuery:= @DoOnFormCloseQuery;
  OnKeyDown:= @DoOnFormKeyDown;
  OnKeyUp:= @DoOnFormKeyUp;
  OnActivate:= @DoOnFormActivate;
  OnDeactivate:= @DoOnFormDeactivate;
  OnMouseEnter:= @DoOnFormMouseEnter;
  OnMouseLeave:= @DoOnFormMouseLeave;

  PrevBorderStyle:= BorderStyle;
  PrevForms:= TFPList.Create;

  CustomDialogs.Add(Self);
end;

destructor TFormDummy.Destroy;
var
  n: integer;
begin
  n:= CustomDialogs.IndexOf(Self);
  if n>=0 then
    CustomDialogs.Delete(n);

  FreeAndNil(PrevForms);
  inherited;
end;

procedure TFormDummy.DoOnFormShow(Sender: TObject);
var
  C: TComponent;
  i: integer;
begin
  IsFormShownAlready:= true;
  DoForm_SetupFilters(Self);

  for i:= 0 to ComponentCount-1 do
  begin
    C:= Components[i];
    if C is TListview then
      with (C as TListview) do
        if ItemFocused<>nil then
          ItemFocused.MakeVisible(false);
  end;

  DoEvent(-1, FEventOnShow, nil);
end;

procedure TFormDummy.DoOnFormHide(Sender: TObject);
begin
  DoEvent(-1, FEventOnHide, nil);
end;

procedure TFormDummy.DoOnFormMouseEnter(Sender: TObject);
begin
  DoEvent(-1, FEventOnMouseEnter, nil);
end;

procedure TFormDummy.DoOnFormMouseLeave(Sender: TObject);
begin
  DoEvent(-1, FEventOnMouseExit, nil);
end;

procedure TFormDummy.DoOnFormActivate(Sender: TObject);
begin
  DoEvent(-1, FEventOnActivate, nil);
end;

procedure TFormDummy.DoOnFormDeactivate(Sender: TObject);
begin
  DoEvent(-1, FEventOnDeactivate, nil);
end;

procedure TFormDummy._HandleClickEvent(Sender: TObject; ADblClick: boolean);
var
  Props: TAppControlProps;
  IdControl: integer;
  DataObj: PPyObject;
  P: TPoint;
begin
  Props:= TAppControlProps((Sender as TControl).Tag);
  IdControl:= FindControlIndexByOurObject(Sender);
  P:= (Sender as TControl).ScreenToClient(Mouse.CursorPos);

  with GetPythonEngine do
  begin
    DataObj:= PyTuple_New(2);
    PyTuple_SetItem(DataObj, 0, PyInt_FromLong(P.X));
    PyTuple_SetItem(DataObj, 1, PyInt_FromLong(P.Y));
  end;

  if ADblClick then
    DoEvent(IdControl, Props.FEventOnClickDbl, DataObj)
  else
    DoEvent(IdControl, Props.FEventOnClick, DataObj);
end;

procedure TFormDummy.DoOnControlMouseEnter(Sender: TObject);
begin
  _HandleMouseEvent(Sender, cControlEventMouseEnter, nil);
end;

procedure TFormDummy.DoOnControlMouseLeave(Sender: TObject);
begin
  _HandleMouseEvent(Sender, cControlEventMouseExit, nil);
end;

function TFormDummy._MouseEventDataObject(
  AButton: TMouseButton; AShift: TShiftState; AX, AY: Integer): PPyObject;
begin
  with GetPythonEngine do
  begin
    Result:= PyDict_New();
    PyDict_SetItemString(Result, 'btn', PyInt_FromLong(Ord(AButton)));
    PyDict_SetItemString(Result, 'state', PyString_FromString(PChar(ConvertShiftStateToString(AShift))));
    PyDict_SetItemString(Result, PChar(string('x')), PyInt_FromLong(AX));
    PyDict_SetItemString(Result, PChar(string('y')), PyInt_FromLong(AY));
  end;
  (*
  Result:= Format('{"btn": %d, "state": "%s", "x": %d, "y": %d}', [
    Ord(AButton),
    ConvertShiftStateToString(AShift),
    AX,
    AY
    ]);
    *)
end;

procedure TFormDummy.DoOnControlMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  _HandleMouseEvent(Sender, cControlEventMouseDown,
    _MouseEventDataObject(Button, Shift, X, Y)
    );
end;

procedure TFormDummy.DoOnControlMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  _HandleMouseEvent(Sender, cControlEventMouseUp,
    _MouseEventDataObject(Button, Shift, X, Y)
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
  DoEvent(IdControl, Props.FEventOnClickX, nil);
end;

procedure TFormDummy.DoOnDblClick(Sender: TObject);
begin
  _HandleClickEvent(Sender, true);
end;

procedure TFormDummy.DoOnFormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  Str: string;
  Form: TCustomForm;
  Data: PPyObject;
begin
  with GetPythonEngine do
    Data:= PyString_FromString(PChar(ConvertShiftStateToString(Shift)));

  Str:= DoEvent(
    Key,
    FEventOnKeyDown,
    Data
    );
  if Str=cPyFalse then
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
      if Assigned(Form) then
        Form.OnKeyDown(nil, Key, []);
    end
    else
    if fsModal in FFormState then
      ModalResult:= mrCancel
    else
      Close;

    Key:= 0;
    exit;
  end;
end;

procedure TFormDummy.DoOnFormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  Str: string;
  Data: PPyObject;
begin
  with GetPythonEngine do
    Data:= PyString_FromString(PChar(ConvertShiftStateToString(Shift)));

  Str:= DoEvent(
    Key,
    FEventOnKeyUp,
    Data
    );
  if Str=cPyFalse then
  begin
    Key:= 0;
    exit;
  end;
end;

procedure TFormDummy.DoOnResize;
begin
  if not (BorderStyle in [bsSizeable, bsSizeToolWin]) then exit;
  if not IsFormShownAlready then exit;
  DoEvent(-1, FEventOnResize, nil);
end;

procedure TFormDummy.DoOnFormCloseQuery(Sender: TObject; var CanClose: boolean);
var
  Str: string;
begin
  Str:= DoEvent(-1, FEventOnCloseQuery, nil);
  CanClose:= Str<>cPyFalse;
end;

procedure TFormDummy.DoOnControlMenu(Sender: TObject; MousePos: TPoint;
  var Handled: Boolean);
var
  Props: TAppControlProps;
  IdControl: integer;
begin
  Props:= TAppControlProps((Sender as TControl).Tag);
  IdControl:= FindControlIndexByOurObject(Sender);
  Handled:= DoEvent(IdControl, Props.FEventOnMenu,
    _MouseEventDataObject(mbRight, GetKeyShiftState, MousePos.X, MousePos.Y)
    )=cPyFalse;
end;


procedure TFormDummy.DoOnFormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction:= caHide; //caFree gives crash on btn clicks on win
  if IsDlgCustom then exit;

  DoEmulatedModalClose;
  IdClicked:= -1;
  DoEvent(-1, FEventOnClose, nil);
end;

function TFormDummy.IdFocused: integer;
begin
  Result:= FindControlIndexByOurObject(ActiveControl);
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


procedure TFormDummy.DoOnChange(Sender: TObject);
var
  Props: TAppControlProps;
begin
  if BlockedOnChange then exit;

  //workarnd for bug on Mac
  //(flickering on More>> press in BackupFile dialog)
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
    DoEvent(IdClicked, Props.FEventOnChange, nil);
  finally
    BlockedOnChange:= false;
  end;
end;

procedure TFormDummy.DoOnListboxSelect(Sender: TObject; User: boolean);
begin
  //here called on_change, not on_select
  //reason: for Listbox/CheckListbox, value is selected index, so sel change - on_change
  DoOnChange(Sender);
end;

procedure TFormDummy._HandleMouseEvent(Sender: TObject;
  const AEventKind: TAppCtlMouseEvent;
  AData: PPyObject);
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
  end;

  DoEvent(IdControl, SCallback, AData);
end;

procedure TFormDummy.DoOnListboxDrawItem(Sender: TObject; ACanvas: TCanvas;
  AIndex: integer; const ARect: TRect);
var
  Props: TAppControlProps;
  IdControl: integer;
  DataRect, Data: PPyObject;
begin
  with GetPythonEngine do
  begin
    DataRect:= PyTuple_New(4);
    PyTuple_SetItem(DataRect, 0, PyInt_FromLong(ARect.Left));
    PyTuple_SetItem(DataRect, 1, PyInt_FromLong(ARect.Top));
    PyTuple_SetItem(DataRect, 2, PyInt_FromLong(ARect.Right));
    PyTuple_SetItem(DataRect, 3, PyInt_FromLong(ARect.Bottom));

    Data:= PyDict_New();
    PyDict_SetItemString(Data, 'canvas', PyLong_FromLongLong(PtrInt(ACanvas)));
    PyDict_SetItemString(Data, 'index', PyInt_FromLong(AIndex));
    PyDict_SetItemString(Data, 'rect', DataRect);
  end;

  Props:= TAppControlProps((Sender as TControl).Tag);
  IdControl:= FindControlIndexByOurObject(Sender);
  DoEvent(IdControl, Props.FEventOnListboxDrawItem, Data);
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
  Data: PPyObject;
begin
  if BlockedOnSelect_Listview then exit;
  BlockedOnSelect_Listview:= true;

  with GetPythonEngine do
  begin
    Data:= PyTuple_New(2);
    PyTuple_SetItem(Data, 0, PyInt_FromLong(Item.Index));
    PyTuple_SetItem(Data, 1, PyBool_FromLong(Ord(Selected)));
  end;

  try
    Props:= TAppControlProps((Sender as TControl).Tag);
    IdControl:= FindControlIndexByOurObject(Sender);
    DoEvent(IdControl, Props.FEventOnSelect, Data);
      {
      Format('(%d, %s)', [Item.Index, cPyFalseTrue[Selected] ])
      );
      }
  finally
    BlockedOnSelect_Listview:= false;
  end;
end;

procedure TFormDummy.DoOnListviewColumnClick(Sender: TObject;
  Column: TListColumn);
var
  Props: TAppControlProps;
  IdControl: integer;
  Data: PPyObject;
begin
  with GetPythonEngine do
    Data:= PyInt_FromLong(Column.Index);

  Props:= TAppControlProps((Sender as TControl).Tag);
  IdControl:= FindControlIndexByOurObject(Sender);
  DoEvent(IdControl, Props.FEventOnClickHeader, Data);
end;


function IsEventItemListed(const SItem, SList: string): boolean;
begin
  if SList='' then exit(false);
  if SList='*' then exit(true);
  Result:= Pos(','+SItem+',', ','+SList+',')>0;
end;


function TFormDummy.DoEvent(AIdControl: integer; const ACallback: string; AData: PPyObject): string;
var
  Params: array of PPyObject;
  ParamNames: array of string;
begin
  if ACallback='' then exit('');

  with GetPythonEngine do
  begin
    SetLength(Params, 2);
    SetLength(ParamNames, 2);
    Params[0]:= PyLong_FromLongLong(PtrInt(Self));
    Params[1]:= PyInt_FromLong(AIdControl);
    ParamNames[0]:= 'id_dlg';
    ParamNames[1]:= 'id_ctl';

    if Assigned(AData) then
    begin
      SetLength(Params, Length(Params)+1);
      Params[Length(Params)-1]:= AData;
      SetLength(ParamNames, Length(ParamNames)+1);
      ParamNames[Length(ParamNames)-1]:= 'data';
    end;
  end;

  Result:= CustomDialog_DoPyCallback(ACallback, Params, ParamNames);
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
  FormStyle:= fsStayOnTop;
  Show;
end;

procedure TFormDummy.DoEmulatedModalClose;
var
  i: integer;
begin
  for i:= PrevForms.Count-1 downto 0 do
    TForm(PrevForms[i]).Enabled:= true;
  PrevForms.Clear;
end;

procedure TFormDummy.DoOnTreeviewChange(Sender: TObject; Node: TTreeNode);
var
  Props: TAppControlProps;
  IdControl: integer;
  Data: PPyObject;
begin
  with GetPythonEngine do
    Data:= PyLong_FromLongLong(PtrInt(Node));

  Props:= TAppControlProps((Sender as TControl).Tag);
  IdControl:= FindControlIndexByOurObject(Sender);
  DoEvent(IdControl, Props.FEventOnChange, Data);
end;

procedure TFormDummy.DoOnTreeviewExpanding(Sender: TObject; Node: TTreeNode;
  var AllowExpansion: Boolean);
var
  Props: TAppControlProps;
  IdControl: integer;
  Data: PPyObject;
begin
  if BlockedOnUnfold then exit;

  with GetPythonEngine do
    Data:= PyLong_FromLongLong(PtrInt(Node));

  Props:= TAppControlProps((Sender as TControl).Tag);
  IdControl:= FindControlIndexByOurObject(Sender);
  DoEvent(IdControl, Props.FEventOnUnfold, Data);
end;

procedure TFormDummy.DoOnTreeviewCollapsing(Sender: TObject; Node: TTreeNode;
  var AllowCollapse: Boolean);
var
  Props: TAppControlProps;
  IdControl: integer;
  Data: PPyObject;
begin
  if BlockedOnFold then exit;

  with GetPythonEngine do
    Data:= PyLong_FromLongLong(PtrInt(Node));

  Props:= TAppControlProps((Sender as TControl).Tag);
  IdControl:= FindControlIndexByOurObject(Sender);
  DoEvent(IdControl, Props.FEventOnFold, Data);
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
    DoEvent(IdControl, Props.FEventOnSelect, nil);
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
  DoEvent(IdControl, Props.FEventOnSelect, nil);
end;

procedure TFormDummy.DoOnControlFocusEnter(Sender: TObject);
var
  Props: TAppControlProps;
  IdControl: integer;
begin
  Props:= TAppControlProps((Sender as TControl).Tag);
  IdControl:= FindControlIndexByOurObject(Sender);
  DoEvent(IdControl, Props.FEventOnFocusEnter, nil);
end;

procedure TFormDummy.DoOnControlFocusExit(Sender: TObject);
var
  Props: TAppControlProps;
  IdControl: integer;
begin
  Props:= TAppControlProps((Sender as TControl).Tag);
  IdControl:= FindControlIndexByOurObject(Sender);
  DoEvent(IdControl, Props.FEventOnFocusExit, nil);
end;


procedure TFormDummy.DoOnEditorChange(Sender: TObject);
var
  Props: TAppControlProps;
  IdControl: integer;
begin
  Props:= TAppControlProps((Sender as TControl).Tag);
  IdControl:= FindControlIndexByOurObject(Sender);
  DoEvent(IdControl, Props.FEventOnChange, nil);
end;

procedure TFormDummy.DoOnEditorChangeCaretPos(Sender: TObject);
var
  Props: TAppControlProps;
  IdControl: integer;
begin
  Props:= TAppControlProps((Sender as TControl).Tag);
  IdControl:= FindControlIndexByOurObject(Sender);
  DoEvent(IdControl, Props.FEventOnEditorCaret, nil);
end;


procedure TFormDummy.DoOnEditorKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  Props: TAppControlProps;
  IdControl: integer;
  Data: PPyObject;
begin
  with GetPythonEngine do
  begin
    Data:= PyTuple_New(2);
    PyTuple_SetItem(Data, 0, PyInt_FromLong(Key));
    PyTuple_SetItem(Data, 1, PyString_FromString(PChar(ConvertShiftStateToString(Shift))));
  end;

  Props:= TAppControlProps((Sender as TControl).Tag);
  IdControl:= FindControlIndexByOurObject(Sender);
  if DoEvent(IdControl, Props.FEventOnEditorKeyDown, Data)
    {
    Format('(%d, "%s")', [Key, ConvertShiftStateToString(Shift)])
    }
    = cPyFalse then
   Key:= 0;
end;

procedure TFormDummy.DoOnEditorKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  Props: TAppControlProps;
  IdControl: integer;
  Data: PPyObject;
begin
  with GetPythonEngine do
  begin
    Data:= PyTuple_New(2);
    PyTuple_SetItem(Data, 0, PyInt_FromLong(Key));
    PyTuple_SetItem(Data, 1, PyString_FromString(PChar(ConvertShiftStateToString(Shift))));
  end;

  Props:= TAppControlProps((Sender as TControl).Tag);
  IdControl:= FindControlIndexByOurObject(Sender);
  if DoEvent(IdControl, Props.FEventOnEditorKeyUp, Data)
    {
    Format('(%d, "%s")', [Key, ConvertShiftStateToString(Shift)])
    }
    = cPyFalse then
   Key:= 0;
end;


procedure TFormDummy.DoOnEditorClickGutter(Sender: TObject; ABand, ALine: integer);
var
  Props: TAppControlProps;
  IdControl: integer;
  Data: PPyObject;
begin
  with GetPythonEngine do
  begin
    Data:= PyDict_New();
    PyDict_SetItemString(Data, 'state', PyString_FromString(PChar(ConvertShiftStateToString(KeyboardStateToShiftState))));
    PyDict_SetItemString(Data, 'line', PyInt_FromLong(ALine));
    PyDict_SetItemString(Data, 'band', PyInt_FromLong(ABand));
  end;

  Props:= TAppControlProps((Sender as TControl).Tag);
  IdControl:= FindControlIndexByOurObject(Sender);
  DoEvent(IdControl, Props.FEventOnEditorClickGutter, Data);
    (*
    Format('{ "state": "%s", "line": %d, "band": %d }', [
      ConvertShiftStateToString(KeyboardStateToShiftState),
      ALine,
      ABand
      *)
end;

procedure TFormDummy.DoOnEditorClickGap(Sender: TObject; AGapItem: TATGapItem; APos: TPoint);
var
  Props: TAppControlProps;
  IdControl: integer;
  Data: PPyObject;
begin
  if not Assigned(AGapItem) then exit;

  with GetPythonEngine do
  begin
    Data:= PyDict_New();
    PyDict_SetItemString(Data, 'state', PyString_FromString(PChar(ConvertShiftStateToString(KeyboardStateToShiftState))));
    PyDict_SetItemString(Data, 'line', PyInt_FromLong(AGapItem.LineIndex));
    PyDict_SetItemString(Data, 'tag', PyLong_FromLongLong(AGapItem.Tag));
    PyDict_SetItemString(Data, 'gap_w', PyInt_FromLong(AGapItem.Bitmap.Width));
    PyDict_SetItemString(Data, 'gap_h', PyInt_FromLong(AGapItem.Bitmap.Height));
    PyDict_SetItemString(Data, PChar(string('x')), PyInt_FromLong(APos.X));
    PyDict_SetItemString(Data, PChar(string('y')), PyInt_FromLong(APos.Y));
  end;

  Props:= TAppControlProps((Sender as TControl).Tag);
  IdControl:= FindControlIndexByOurObject(Sender);
  DoEvent(IdControl, Props.FEventOnEditorClickGap, Data);
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

procedure TFormDummy.DoOnEditorScroll(Sender: TObject);
var
  Props: TAppControlProps;
  IdControl: integer;
begin
  Props:= TAppControlProps((Sender as TControl).Tag);
  IdControl:= FindControlIndexByOurObject(Sender);
  DoEvent(IdControl, Props.FEventOnEditorScroll, nil);
end;

procedure TFormDummy.DoOnEditorPaste(Sender: TObject; var AHandled: boolean; AKeepCaret, ASelectThen: boolean);
var
  Props: TAppControlProps;
  IdControl: integer;
  Data: PPyObject;
begin
  with GetPythonEngine do
  begin
    Data:= PyDict_New();
    PyDict_SetItemString(Data, 'keep_caret', PyBool_FromLong(Ord(AKeepCaret)));
    PyDict_SetItemString(Data, 'sel_then', PyBool_FromLong(Ord(ASelectThen)));
  end;

  Props:= TAppControlProps((Sender as TControl).Tag);
  IdControl:= FindControlIndexByOurObject(Sender);
  if DoEvent(IdControl, Props.FEventOnEditorPaste, Data)
    (*
    Format('{ "keep_caret": %s, "sel_then": %s }', [
      cPyFalseTrue[AKeepCaret],
      cPyFalseTrue[ASelectThen]
    ]))
    *)
    = cPyFalse then
    AHandled:= true;
end;


initialization
  CustomDialogs:= TFPList.Create;

finalization
  CustomDialogs.Free;

end.

