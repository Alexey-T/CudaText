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
  StdCtrls, Forms, ComCtrls,
  LclType,
  ListFilterEdit,
  ListViewFilterEdit,
  proc_globdata,
  proc_miscutils,
  ATSynEdit;

type
  TAppPyCommonCallback = function(
    const ACallback: string;
    const AParams: array of string): string of object;

var
  CustomDialog_DoPyCallback: TAppPyCommonCallback = nil;

type
  { TAppControlProps }

  TAppControlProps = class
  public
    FName: string;
    FTypeString: string;
    FActive: boolean;
    FTagString: string;
    FCallback: string;
    FCallbackOnClick: string;
    FCallbackOnClickDbl: string;
    FCallbackOnMenu: string;
    FCallbackOnSelect: string;
    FCallbackOnFold: string;
    FCallbackOnUnfold: string;
    constructor Create(const ATypeString: string);
  end;


type
  { TFormDummy }

  TFormDummy = class(TForm)
  private
    IsFormShownAlready: boolean;
    procedure DoOnShow(Sender: TObject);
    procedure DoOnKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure DoOnKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure DoOnClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure DoOnCloseQuery(Sender: TObject; var CanClose: boolean);
  public
    IsDlgCustom: boolean;
    IdClicked: integer;
    CallbackOnClose: string;
    CallbackOnCloseQuery: string;
    CallbackOnKeyDown: string;
    CallbackOnKeyUp: string;
    CallbackOnResize: string;
    TagString: string;
    PrevForms: TList;
    PrevBorderStyle: TFormBorderStyle;
    BlockedOnChange: boolean;
    function IdFocused: integer;
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure DoOnResize; override;
    procedure DoOnClick(Sender: TObject);
    procedure DoOnChange(Sender: TObject);
    procedure DoOnSelChange(Sender: TObject; User: boolean);
    procedure DoOnListviewChange(Sender: TObject; Item: TListItem; Change: TItemChange);
    procedure DoOnListviewSelect(Sender: TObject; Item: TListItem; Selected: Boolean);
    procedure DoOnTreeviewChange(Sender: TObject; Node: TTreeNode);
    function DoEvent(AIdControl: integer; const ACallback, AInfo: string): string;
    procedure DoEmulatedModalShow;
    procedure DoEmulatedModalClose;
    function FindControlByOurName(const AName: string): TControl;
    function FindControlIndexByOurObject(Sender: TObject): integer;
    function FindControlIndexByOurName(const AName: string): integer;
  end;


function IsEventItemListed(const SItem, SList: string): boolean;


implementation

procedure DoForm_SetupFilters(F: TFormDummy);
const
  cPrefix = 'f_';
var
  SName: string;
  C, C2: TControl;
  CFilterListbox: TListFilterEdit;
  CFilterListview: TListViewFilterEdit;
  i: integer;
begin
  for i:= 0 to F.ControlCount-1 do
  begin
    C:= F.Controls[i];
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
  FCallback:= '';
end;

{ TFormDummy }

constructor TFormDummy.Create(TheOwner: TComponent);
begin
  inherited CreateNew(TheOwner);

  BorderStyle:= bsDialog;
  BorderIcons:= [biSystemMenu];
  KeyPreview:= true;
  Position:= poScreenCenter;
  ShowHint:= true;
  Scaled:= false;

  IsDlgCustom:= false;
  IsFormShownAlready:= false;
  IdClicked:= -1;
  TagString:= '';
  BlockedOnChange:= false;

  OnShow:= @DoOnShow;
  OnClose:= @DoOnClose;
  OnCloseQuery:= @DoOnCloseQuery;
  OnKeyDown:= @DoOnKeyDown;
  OnKeyUp:= @DoOnKeyUp;

  PrevBorderStyle:= BorderStyle;
  PrevForms:= TList.Create;
end;

destructor TFormDummy.Destroy;
begin
  FreeAndNil(PrevForms);
  inherited;
end;

procedure TFormDummy.DoOnShow(Sender: TObject);
var
  C: TControl;
  i: integer;
begin
  IsFormShownAlready:= true;
  DoForm_SetupFilters(Self);

  for i:= 0 to ControlCount-1 do
  begin
    C:= Controls[i];
    if C is TListview then
      with (C as TListview) do
        if ItemFocused<>nil then
          ItemFocused.MakeVisible(false);
  end;
end;

procedure TFormDummy.DoOnClick(Sender: TObject);
var
  Props: TAppControlProps;
  IdControl: integer;
  SInfo: string;
  P: TPoint;
begin
  Props:= TAppControlProps((Sender as TControl).Tag);
  IdControl:= FindControlIndexByOurObject(Sender);
  P:= (Sender as TControl).ScreenToClient(Mouse.CursorPos);
  SInfo:= Format('(%d,%d)', [P.X, P.Y]);

  DoEvent(IdControl, Props.FCallback, SInfo);
end;

procedure TFormDummy.DoOnKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  Str: string;
begin
  Str:= DoEvent(Key, CallbackOnKeyDown, '');
  if Str='False' then
  begin
    Key:= 0;
    exit;
  end;

  if (Key=VK_ESCAPE) then
  begin
    IdClicked:= -1;

    if fsModal in FFormState then
      ModalResult:= mrCancel
    else
      Close;

    Key:= 0;
    exit;
  end;
end;

procedure TFormDummy.DoOnKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  Str: string;
begin
  Str:= DoEvent(Key, CallbackOnKeyUp, '');
  if Str='False' then
  begin
    Key:= 0;
    exit;
  end;
end;

procedure TFormDummy.DoOnResize;
begin
  if BorderStyle<>bsSizeable then exit;
  if not IsFormShownAlready then exit;
  DoEvent(-1, CallbackOnResize, '');
end;

procedure TFormDummy.DoOnCloseQuery(Sender: TObject; var CanClose: boolean);
var
  Str: string;
begin
  Str:= DoEvent(-1, CallbackOnCloseQuery, '');
  CanClose:= Str<>'False';
end;


procedure TFormDummy.DoOnClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction:= caHide; //caFree gives crash on btn clicks on win
  if IsDlgCustom then exit;

  DoEmulatedModalClose;
  IdClicked:= -1;
  DoEvent(-1, CallbackOnClose, '');
end;

function TFormDummy.IdFocused: integer;
begin
  Result:= FindControlIndexByOurObject(ActiveControl);
end;


function TFormDummy.FindControlIndexByOurObject(Sender: TObject): integer;
var
  i: integer;
begin
  Result:= -1;
  for i:= 0 to ControlCount-1 do
    if Controls[i]=Sender then
      exit(i);
end;

function TFormDummy.FindControlByOurName(const AName: string): TControl;
var
  C: TControl;
  i: integer;
begin
  Result:= nil;
  for i:= 0 to ControlCount-1 do
  begin
    C:= Controls[i];
    if SameText(TAppControlProps(C.Tag).FName, AName) then
      exit(C);
  end;
end;

function TFormDummy.FindControlIndexByOurName(const AName: string): integer;
var
  C: TControl;
  i: integer;
begin
  Result:= -1;
  for i:= 0 to ControlCount-1 do
  begin
    C:= Controls[i];
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

  DoEvent(IdClicked, Props.FCallback, '');
end;

procedure TFormDummy.DoOnSelChange(Sender: TObject; User: boolean);
begin
  DoOnChange(Sender);
end;

procedure TFormDummy.DoOnListviewChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
begin
  DoOnChange(Sender);
end;

procedure TFormDummy.DoOnListviewSelect(Sender: TObject; Item: TListItem;
  Selected: Boolean);
begin
  DoOnChange(Sender);
end;


function IsEventItemListed(const SItem, SList: string): boolean;
begin
  if SList='' then exit(false);
  if SList='*' then exit(true);
  Result:= Pos(','+SItem+',', ','+SList+',')>0;
end;


function TFormDummy.DoEvent(AIdControl: integer; const ACallback, AInfo: string): string;
var
  Params: array of string;
begin
  if ACallback='' then exit('');

  SetLength(Params, 2);
  Params[0]:= 'id_dlg='+IntToStr(PtrInt(Self));
  Params[1]:= 'id_ctl='+IntToStr(AIdControl);

  if AInfo<>'' then
  begin
    SetLength(Params, Length(Params)+1);
    Params[Length(Params)-1]:= 'info='+AInfo;
  end;

  Result:= CustomDialog_DoPyCallback(ACallback, Params);
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
begin
  DoOnChange(Sender);
end;

end.

