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
  Classes, SysUtils, Controls, StdCtrls, Forms,
  ComCtrls, LclType,
  ListFilterEdit,
  ListViewFilterEdit,
  ATSynEdit,
  proc_globdata;

type
  TAppPyEventCallback = function(
      AEd: TATSynEdit;
      AEvent: TAppPyEvent;
      const AParams: array of string): string of object;
  TAppPyCommonCallback = procedure(
      const ACallback: string;
      const AParams: array of string) of object;

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
    FormShown: boolean;
    procedure DoOnShow(Sender: TObject);
    procedure DoOnKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure DoOnClose(Sender: TObject; var CloseAction: TCloseAction);
  public
    IsDlgCustom: boolean;
    IdClicked: integer;
    PrevForms: TList;
    function IdFocused: integer;
    function IdFromName(const AName: string): integer;
    constructor Create(TheOwner: TComponent); override;
    procedure DoOnResize; override;
    procedure DoOnChange(Sender: TObject);
    procedure DoOnSelChange(Sender: TObject; User: boolean);
    procedure DoOnListviewChange(Sender: TObject; Item: TListItem; Change: TItemChange);
    procedure DoOnListviewSelect(Sender: TObject; Item: TListItem; Selected: Boolean);
    procedure DoEvent(AIdControl: integer; const AEvent: string);
  end;


procedure DoFormEmulatedModalShow(AForm: TFormDummy);


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


procedure DoFormEmulatedModalShow(AForm: TFormDummy);
var
  F: TForm;
  i: integer;
begin
  for i:= 0 to Screen.FormCount-1 do
  begin
    F:= Screen.Forms[i];
    if F=AForm then Continue; //skip AForm
    if F.Parent<>nil then Continue; //skip docked
    if F.Enabled then
    begin
      AForm.PrevForms.Add(F);
      F.Enabled:= false;
    end;
  end;
  AForm.FormStyle:= fsStayOnTop;
  AForm.Show;
end;

procedure DoFormEmulatedModalClose(AForm: TFormDummy);
var
  i: integer;
begin
  for i:= AForm.PrevForms.Count-1 downto 0 do
    TForm(AForm.PrevForms[i]).Enabled:= true;
  AForm.PrevForms.Clear;
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
  FormShown:= false;
  IdClicked:= -1;

  OnShow:= @DoOnShow;
  OnClose:= @DoOnClose;
  OnKeyDown:= @DoOnKeyDown;

  PrevForms:= TList.Create;
end;

procedure TFormDummy.DoOnShow(Sender: TObject);
var
  C: TControl;
  i: integer;
begin
  FormShown:= true;
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

procedure TFormDummy.DoOnKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
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

procedure TFormDummy.DoOnResize;
begin
  if BorderStyle<>bsSizeable then exit;
  DoEvent(-1, '"on_resize"');
end;

procedure TFormDummy.DoOnClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction:= caHide; //caFree gives crash on btn clicks on win
  if IsDlgCustom then exit;

  DoFormEmulatedModalClose(Self);

  IdClicked:= -1;
  DoEvent(-1, '"on_close"');
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
  if not FormShown then exit;

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
      IntToStr(IdClicked) //id_ctl
    ])
  else
    DoEvent(IdClicked, '"on_change"');
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

procedure TFormDummy.DoEvent(AIdControl: integer; const AEvent: string);
begin
  CustomDialog_DoPyEvent(nil, cEventOnDlg,
    [
      IntToStr(PtrInt(Self)), //id_dlg
      IntToStr(AIdControl), //id_ctl
      AEvent //id_event
    ]);
end;

end.

