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
  ATSynEdit,
  ATListbox;

type
  TAppPyEventCallback = function(
      AEd: TATSynEdit;
      AEvent: TAppPyEvent;
      const AParams: array of string): string of object;
  TAppPyCommonCallback = function(
      const ACallback: string;
      const AParams: array of string): string of object;

var
  CustomDialog_DoPyEvent: TAppPyEventCallback = nil;
  CustomDialog_DoPyCallback: TAppPyCommonCallback = nil;

type
  { TAppControlProps }

  TAppControlProps = class
  public
    FTypeString: string;
    FActive: boolean;
    FTagString: string;
    FCallback: string;
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
    Callback: string;
    PrevForms: TList;
    function IdFocused: integer;
    function IdFromName(const AName: string): integer;
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure DoOnResize; override;
    procedure DoOnChange(Sender: TObject);
    procedure DoOnSelChange(Sender: TObject; User: boolean);
    procedure DoOnListviewChange(Sender: TObject; Item: TListItem; Change: TItemChange);
    procedure DoOnListviewSelect(Sender: TObject; Item: TListItem; Selected: Boolean);
    procedure DoOnATListboxDrawItem(Sender: TObject; C: TCanvas; AIndex: integer; const ARect: TRect);
    procedure DoOnTreeviewChange(Sender: TObject; Node: TTreeNode);
    function DoEvent(AIdControl: integer; const AEvent, AInfo: string): string;
    procedure DoEmulatedModalShow;
    procedure DoEmulatedModalClose;
  end;


implementation

procedure DoForm_SetupFilters(F: TForm);
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
    if C is TListFilterEdit then
    begin
      SName:= Copy(C.Name, Length(cPrefix)+1, MaxInt);
      C2:= F.FindChildControl(SName);
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
      SName:= Copy(C.Name, Length(cPrefix)+1, MaxInt);
      C2:= F.FindChildControl(SName);
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
  Scaled:= true;

  IsDlgCustom:= false;
  IsFormShownAlready:= false;
  IdClicked:= -1;
  Callback:= '';

  OnShow:= @DoOnShow;
  OnClose:= @DoOnClose;
  OnCloseQuery:= @DoOnCloseQuery;
  OnKeyDown:= @DoOnKeyDown;
  OnKeyUp:= @DoOnKeyUp;

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

procedure TFormDummy.DoOnKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  Str: string;
begin
  Str:= DoEvent(Key, '"on_key_down"',
    '"'+ConvertShiftStateToString(KeyboardStateToShiftState)+'"' );
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
  Str:= DoEvent(Key, '"on_key_up"',
    '"'+ConvertShiftStateToString(KeyboardStateToShiftState)+'"' );
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
  DoEvent(-1, '"on_resize"', '');
end;

procedure TFormDummy.DoOnCloseQuery(Sender: TObject; var CanClose: boolean);
var
  Str: string;
begin
  Str:= DoEvent(-1, '"on_close_query"', '');
  CanClose:= Str<>'False';
end;


procedure TFormDummy.DoOnClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction:= caHide; //caFree gives crash on btn clicks on win
  if IsDlgCustom then exit;

  DoEmulatedModalClose;
  IdClicked:= -1;
  DoEvent(-1, '"on_close"', '');
end;

function TFormDummy.IdFocused: integer;
var
  i: integer;
begin
  Result:= -1;
  for i:= 0 to ControlCount-1 do
    if Controls[i]=ActiveControl then
      exit(i);
end;

function TFormDummy.IdFromName(const AName: string): integer;
var
  i: integer;
begin
  Result:= -1;
  for i:= 0 to ControlCount-1 do
    if Controls[i].Name=AName then
      exit(i);
end;


procedure TFormDummy.DoOnChange(Sender: TObject);
var
  Props: TAppControlProps;
  i: integer;
begin
  //workarnd for bug on Mac
  //(flicker on More>> press in BackupFile dialog)
  if not IsFormShownAlready then exit;

  Props:= TAppControlProps((Sender as TControl).Tag);
  if not Props.FActive then exit;

  IdClicked:= -1;
  for i:= 0 to ControlCount-1 do
    if Controls[i]=Sender then
    begin
      IdClicked:= i;
      Break;
    end;

  if IsDlgCustom then
  begin
    ModalResult:= mrOk;
    exit;
  end;

  if Props.FCallback<>'' then
    CustomDialog_DoPyCallback(Props.FCallback, [
      IntToStr(PtrInt(Self)), //id_dlg
      IntToStr(IdClicked), //id_ctl
      '"on_change"' //id_event
    ])
  else
    DoEvent(IdClicked, '"on_change"', '');
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

function TFormDummy.DoEvent(AIdControl: integer; const AEvent, AInfo: string): string;
var
  Params: array of string;
begin
  SetLength(Params, 4);
  Params[0]:= IntToStr(PtrInt(Self)); //id_dlg
  Params[1]:= IntToStr(AIdControl); //id_ctl
  Params[2]:= AEvent; //id_event
  Params[3]:= AInfo;

  if Callback<>'' then
    Result:= CustomDialog_DoPyCallback(Callback, Params);
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


procedure TFormDummy.DoOnATListboxDrawItem(Sender: TObject; C: TCanvas;
  AIndex: integer; const ARect: TRect);
const
  cIndent = 4;
var
  List: TATListbox;
  str: string;
begin
  List:= Sender as TATListbox;
  if AIndex<0 then exit;
  if AIndex=List.ItemIndex then
  begin
    c.Font.Color:= clWhite;
    c.Brush.Color:= clMedGray;
  end
  else
  begin
    c.Font.Color:= clBlack;
    c.Brush.Color:= clWhite;
  end;
  c.Pen.Color:= c.Brush.Color;
  c.FillRect(ARect);

  str:= List.Items[AIndex];
  c.TextOut(ARect.Left+cIndent, ARect.Top+1, str);
end;

procedure TFormDummy.DoOnTreeviewChange(Sender: TObject; Node: TTreeNode);
begin
  DoOnChange(Sender);
end;

end.

