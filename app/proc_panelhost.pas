(*
This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at http://mozilla.org/MPL/2.0/.

Copyright (c) Alexey Torgashin
*)
unit proc_panelhost;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, Controls,
  ExtCtrls, Buttons, Forms,
  ATButtons,
  ATPanelSimple,
  ATFlatToolbar;

type
  TAppPanelId = (
    cPaneNone,
    cPaneSide,
    cPaneOut
    );

type
  { TAppPanelItem }

  TAppPanelItem = class
  public
    ItemCaption: string;
    ItemControl: TCustomControl;
    ItemModule: string;
    ItemMethod: string;
    ItemOnShow: TNotifyEvent;
  end;

type
  TAppPanelOnCommand = procedure(const ACallback: string) of object;
  TAppPanelOnGetTitle = function(const ACaption: string): string of object;
  TAppPanelOnContextPopup = procedure(const ACaption: string) of object;

type
  { TAppPanelHost }

  TAppPanelHost = class
  private
    FOwner: TComponent;
    FAlign: TAlign;
    FToolbarUpdateCount: integer;
    FToolbarUpdateTime: QWord;
    FOnContextPopup: TAppPanelOnContextPopup;
    function GetFloating: boolean;
    function GetPanelSize: integer;
    function GetVisible: boolean;
    procedure SetAlign(AValue: TAlign);
    procedure SetFloating(AValue: boolean);
    procedure SetPanelSize(AValue: integer);
    procedure SetVisible(AValue: boolean);
    procedure HandleButtonClick(Sender: TObject);
    procedure HandleRightClick(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
    procedure UpdateTitle;
  public
    PanelGrouper: TCustomControl;
    PanelRoot: TCustomControl;
    PanelTitle: TPanel;
    Panels: TFPList;
    Toolbar: TATFlatToolbar;
    Splitter: TSplitter;
    LastActivePanel: string;
    DefaultPanel: string;
    FormFloat: TForm;
    FormFloatBounds: TRect;
    ShowTitleForSide: boolean;
    ShowTitleForBottom: boolean;
    OnShow: TNotifyEvent;
    OnHide: TNotifyEvent;
    OnCommand: TAppPanelOnCommand;
    OnCloseFloatForm: TCloseEvent;
    OnGetTranslatedTitle: TAppPanelOnGetTitle;
    constructor Create;
    destructor Destroy; override;
    procedure Init(AOwner: TComponent; AAlign: TAlign);
    property Align: TAlign read FAlign write SetAlign;
    property Floating: boolean read GetFloating write SetFloating;
    property Visible: boolean read GetVisible write SetVisible;
    property PanelSize: integer read GetPanelSize write SetPanelSize;
    function CaptionToPanelIndex(const ACaption: string): integer;
    function CaptionToButtonIndex(const ACaption: string): integer;
    function CaptionToControlHandle(const ACaption: string): PtrInt;
    function Add(const ACaption: string; AImageIndex: integer; AHandle: PtrInt; AOnPanelShow: TNotifyEvent): boolean;
    function AddEmpty(const ACaption: string; AImageIndex: integer; const AModule, AMethod: string): boolean;
    function Delete(const ACaption: string): boolean;
    function SetProp(const ACaption: string; AImageIndex: integer; const AHint: string): boolean;
    procedure UpdateButtons;
    procedure UpdateSplitter;
    function UpdatePanels(const ACaption: string; AndFocus: boolean; ACheckExists: boolean): boolean;
    procedure UpdateToolbarControls;
    procedure InitFormFloat;
    property ToolbarUpdateCount: integer read FToolbarUpdateCount;
    property ToolbarUpdateTime: QWord read FToolbarUpdateTime;
    property OnContextPopup: TAppPanelOnContextPopup read FOnContextPopup write FOnContextPopup;
  end;

var
  AppPanels: array[TAppPanelId] of TAppPanelHost;

implementation

const
  msgTitle = 'CudaText';

{ TAppPanelItem }

{ TAppPanelHost }

constructor TAppPanelHost.Create;
begin
  inherited Create;
  Panels:= TFPList.Create;
  ShowTitleForSide:= true;
  ShowTitleForBottom:= false;
end;

destructor TAppPanelHost.Destroy;
var
  Panel: TAppPanelItem;
  i: integer;
begin
  for i:= Panels.Count-1 downto 0 do
  begin
    Panel:= TAppPanelItem(Panels.Items[i]);
    Panel.Free;
  end;

  Panels.Clear;
  FreeAndNil(Panels);
  inherited Destroy;
end;

procedure TAppPanelHost.Init(AOwner: TComponent; AAlign: TAlign);
begin
  FOwner:= AOwner;
  FAlign:= AAlign;

  PanelGrouper:= TATPanelSimple.Create(FOwner);
  PanelGrouper.Align:= Align;
  PanelGrouper.Parent:= PanelRoot;

  PanelTitle:= TPanel.Create(FOwner);
  PanelTitle.Align:= alTop;
  PanelTitle.Height:= 20;
  PanelTitle.BevelInner:= bvNone;
  PanelTitle.BevelOuter:= bvNone;
  PanelTitle.Parent:= PanelGrouper;

  Splitter:= TSplitter.Create(FOwner);
  Splitter.Align:= Align;
  Splitter.Parent:= PanelRoot;
  Splitter.MinSize:= 100;

  UpdateSplitter;
end;

function TAppPanelHost.GetVisible: boolean;
begin
  if Floating then
    Result:= FormFloat.Visible
  else
    Result:= PanelGrouper.Visible;
end;

procedure TAppPanelHost.SetAlign(AValue: TAlign);
begin
  if FAlign=AValue then Exit;
  FAlign:= AValue;
  if not Floating then
    PanelGrouper.Align:= FAlign;
  Splitter.Align:= FAlign;
  UpdateSplitter;
end;

procedure TAppPanelHost.SetFloating(AValue: boolean);
begin
  {$ifdef LCLGTK2}
  //disable floating panels on GTK2, fixing #4204
  exit;
  {$endif}

  if GetFloating=AValue then exit;

  if AValue then
  begin
    InitFormFloat;
    FormFloat.Show;

    PanelGrouper.Parent:= FormFloat;
    PanelGrouper.Align:= alClient;
    PanelGrouper.Show;
    Splitter.Hide;
  end
  else
  begin
    if Assigned(FormFloat) then
      FormFloat.Hide;

    PanelGrouper.Align:= Align;
    PanelGrouper.Parent:= PanelRoot;
    Splitter.Visible:= PanelGrouper.Visible;
    UpdateSplitter;
  end;

  UpdateTitle;
end;

procedure TAppPanelHost.SetPanelSize(AValue: integer);
begin
  if not Floating then
    case Align of
      alLeft,
      alRight:
        PanelGrouper.Width:= AValue;
      alTop,
      alBottom:
        PanelGrouper.Height:= AValue
    end;
end;

procedure TAppPanelHost.SetVisible(AValue: boolean);
var
  Panel: TAppPanelItem;
  //Btn: TATButton;
  N: integer;
begin
  if GetVisible=AValue then exit;

  PanelGrouper.Visible:= AValue;
  if Floating then
  begin
    FormFloat.Visible:= AValue;
  end
  else
  begin
    Splitter.Visible:= AValue;
    if AValue then
      UpdateSplitter;
  end;

  if AValue then
  begin
    if LastActivePanel='' then
      LastActivePanel:= DefaultPanel;

    if LastActivePanel<>'' then
    begin
      N:= CaptionToPanelIndex(LastActivePanel);
      if N>=0 then
      begin
        Panel:= TAppPanelItem(Panels[N]);
        if Assigned(Panel.ItemOnShow) then
          Panel.ItemOnShow(nil);
      end;

      {
      //this focuses LastActivePanel, bad
      N:= CaptionToButtonIndex(LastActivePanel);
      if N>=0 then
      begin
        Btn:= Toolbar.Buttons[N];
        Btn.OnClick(Btn);
      end;
      }

      UpdateTitle;
    end;

    if Assigned(OnShow) then
      OnShow(Self);
  end
  else
  begin
    if Assigned(OnHide) then
      OnHide(Self);
  end;

  UpdateButtons;
end;

function TAppPanelHost.GetFloating: boolean;
begin
  Result:= Assigned(FormFloat) and (PanelGrouper.Parent=FormFloat);
end;

function TAppPanelHost.GetPanelSize: integer;
begin
  Result:= 0;
  if Floating then exit;
  case Align of
    alLeft,
    alRight:
      Result:= PanelGrouper.Width;
    alTop,
    alBottom:
      Result:= PanelGrouper.Height;
  end;
end;

function TAppPanelHost.CaptionToPanelIndex(const ACaption: string): integer;
var
  i: integer;
begin
  Result:= -1;
  for i:= 0 to Panels.Count-1 do
    with TAppPanelItem(Panels[i]) do
      if ItemCaption=ACaption then
        exit(i);
end;

function TAppPanelHost.CaptionToButtonIndex(const ACaption: string): integer;
var
  i: integer;
begin
  Result:= -1;
  for i:= 0 to Toolbar.ButtonCount-1 do
    if SameText(Toolbar.Buttons[i].Caption, ACaption) then
      exit(i);
end;

function TAppPanelHost.CaptionToControlHandle(const ACaption: string): PtrInt;
var
  Num: integer;
begin
  Result:= 0;
  Num:= CaptionToPanelIndex(ACaption);
  if Num<0 then exit;

  with TAppPanelItem(Panels[Num]) do
    if Assigned(ItemControl) then
      Result:= PtrInt(ItemControl);
end;

function TAppPanelHost.Add(const ACaption: string; AImageIndex: integer;
  AHandle: PtrInt; AOnPanelShow: TNotifyEvent): boolean;
var
  Panel: TAppPanelItem;
  Num: integer;
  bExist: boolean;
  Obj: TObject;
  Ctl: TCustomControl;
begin
  Result:= false;
  Num:= CaptionToPanelIndex(ACaption);
  bExist:= Num>=0;

  if bExist then
    Panel:= TAppPanelItem(Panels[Num])
  else
  begin
    Panel:= TAppPanelItem.Create;
    Panel.ItemOnShow:= AOnPanelShow;
    Panels.Add(Panel);
  end;

  if AHandle<>0 then
  begin
    Obj:= TObject(AHandle);
    if not (Obj is TCustomControl) then exit;
    Ctl:= TCustomControl(Obj);

    Panel.ItemCaption:= ACaption;
    Panel.ItemControl:= Ctl;
    Panel.ItemModule:= '';
    Panel.ItemMethod:= '';

    Ctl.Parent:= PanelGrouper;
    Ctl.Align:= alClient;
    if Ctl is TCustomForm then
      TCustomForm(Ctl).BorderStyle:= bsNone;
  end;

  if not bExist then
  begin
    Toolbar.AddButton(AImageIndex, @HandleButtonClick, @HandleRightClick, ACaption, ACaption, '', false);
    UpdateToolbarControls;
  end;

  Result:= true;
end;

function TAppPanelHost.AddEmpty(const ACaption: string; AImageIndex: integer;
  const AModule, AMethod: string): boolean;
var
  Panel: TAppPanelItem;
begin
  if CaptionToPanelIndex(ACaption)>=0 then exit(false);

  Panel:= TAppPanelItem.Create;
  Panel.ItemCaption:= ACaption;
  Panel.ItemControl:= nil;
  Panel.ItemModule:= AModule;
  Panel.ItemMethod:= AMethod;
  Panels.Add(Panel);

  //save module/method to Btn.DataString
  Toolbar.AddButton(
    AImageIndex,
    @HandleButtonClick,
    @HandleRightClick,
    ACaption,
    ACaption,
    AModule+'.'+AMethod,
    false);
  UpdateToolbarControls;

  Result:= true;
end;

function TAppPanelHost.Delete(const ACaption: string): boolean;
var
  Num, i: integer;
begin
  Num:= CaptionToButtonIndex(ACaption);
  Result:= Num>=0;
  if Result then
  begin
    Toolbar.Buttons[Num].Free;
    UpdateToolbarControls;

    for i:= 0 to Panels.Count-1 do
      with TAppPanelItem(Panels[i]) do
        if ItemCaption=ACaption then
        begin
          if Assigned(ItemControl) then
            ItemControl.Hide;
          Panels.Delete(i);
          Break;
        end;
  end;
end;

function TAppPanelHost.SetProp(const ACaption: string; AImageIndex: integer;
  const AHint: string): boolean;
var
  Num: integer;
begin
  Num:= CaptionToButtonIndex(ACaption);
  Result:= Num>=0;
  if Result then
  begin
    if AImageIndex>=0 then
      Toolbar.Buttons[Num].ImageIndex:= AImageIndex;
    if AHint<>'' then
      Toolbar.Buttons[Num].Hint:= AHint;
  end;
end;

procedure TAppPanelHost.UpdateButtons;
var
  Btn: TATButton;
  bVis: boolean;
  i: integer;
begin
  bVis:= Visible;
  for i:= 0 to Toolbar.ButtonCount-1 do
  begin
    Btn:= Toolbar.Buttons[i];
    Btn.Checked:= bVis and SameText(Btn.Caption, LastActivePanel);
  end;
end;

function TAppPanelHost.UpdatePanels(const ACaption: string; AndFocus: boolean;
  ACheckExists: boolean): boolean;
var
  Ctl: TCustomControl;
  bFound: boolean;
  i: integer;
begin
  if ACheckExists then
  begin
    Result:= CaptionToButtonIndex(ACaption)>=0;
    if not Result then exit;
  end
  else
    Result:= true;

  LastActivePanel:= ACaption;
  Visible:= true;
  UpdateButtons;
  UpdateTitle;

  for i:= 0 to Panels.Count-1 do
  begin
    Ctl:= TAppPanelItem(Panels[i]).ItemControl;
    if Assigned(Ctl) then
      Ctl.Hide;
  end;

  for i:= 0 to Panels.Count-1 do
    with TAppPanelItem(Panels[i]) do
    begin
      if ItemCaption='' then Continue;
      bFound:= ItemCaption=ACaption;
      if bFound then
      begin
        Ctl:= ItemControl;
        if Assigned(Ctl) then
        begin
          Ctl.Show;
          if AndFocus then
            if PanelGrouper.Visible then
              if Ctl.Visible and Ctl.CanFocus then
              begin
                Ctl.SetFocus;
                if Assigned(Ctl.OnEnter) then
                  Ctl.OnEnter(nil);
              end;
        end
        else
        if (ItemModule<>'') and (ItemMethod<>'') then
          OnCommand(ItemModule+'.'+ItemMethod);
        Break;
      end;
    end;
end;

procedure TAppPanelHost.InitFormFloat;
begin
  if not Assigned(FormFloat) then
  begin
    FormFloat:= TForm.CreateNew(Application.MainForm);
    FormFloat.Position:= poDesigned;
    FormFloat.BoundsRect:= FormFloatBounds;
    //FormFloat.BorderStyle:= bsSizeToolWin; //such style is nice but it lacks Maximize button
    FormFloat.BorderIcons:= [biSystemMenu, biMaximize];
    FormFloat.ShowInTaskBar:= stNever;
    FormFloat.FormStyle:= fsStayOnTop; //issue #3383
    FormFloat.OnClose:= OnCloseFloatForm;
  end;
end;

procedure TAppPanelHost.HandleButtonClick(Sender: TObject);
var
  Btn: TATButton;
  SCaption: string;
  NPanel: integer;
  Panel: TAppPanelItem;
begin
  Btn:= Sender as TATButton;
  SCaption:= Btn.Caption;

  if Btn.Checked or (SCaption='') then
  begin
    Btn.Checked:= false;
    Visible:= false;
    //focus current editor when panel hides
    with Application.MainForm do
      if Assigned(OnEnter) then
        OnEnter(nil);
    exit
  end;

  //avoid plugin call if panel already inited
  NPanel:= CaptionToPanelIndex(SCaption);
  if (NPanel>=0) and (TAppPanelItem(Panels[NPanel]).ItemControl=nil) then
    OnCommand(Btn.DataString);

  if NPanel>=0 then
  begin
    Panel:= TAppPanelItem(Panels[NPanel]);
    if Assigned(Panel.ItemOnShow) then
      if Assigned(Panel.ItemControl) and not Panel.ItemControl.Visible then
        Panel.ItemOnShow(nil);
  end;

  UpdatePanels(SCaption, true, false);
end;

procedure TAppPanelHost.HandleRightClick(Sender: TObject; MousePos: TPoint;
  var Handled: Boolean);
var
  Btn: TATButton;
begin
  if Assigned(FOnContextPopup) then
  begin
    Btn:= Sender as TATButton;
    FOnContextPopup(Btn.Caption);
    Handled:= true;
  end;
end;

procedure TAppPanelHost.UpdateSplitter;
begin
  if Splitter.Visible then
    case Align of
      alLeft:
        Splitter.Left:= PanelGrouper.Width;
      alBottom:
        Splitter.Top:= PanelGrouper.Top-8;
      alRight:
        Splitter.Left:= PanelGrouper.Left-8;
      else
        begin end;
    end;
end;

procedure TAppPanelHost.UpdateTitle;
var
  S: string;
  bShow: boolean;
begin
  S:= OnGetTranslatedTitle(LastActivePanel);

  if Floating then
    bShow:= false
  else
  case Align of
    alLeft,
    alRight:
      bShow:= ShowTitleForSide;
    alTop,
    alBottom:
      bShow:= ShowTitleForBottom;
    else
      bShow:= false;
  end;

  PanelTitle.Visible:= bShow;
  PanelTitle.Caption:= S;
  if Assigned(FormFloat) then
    FormFloat.Caption:= S+' - '+msgTitle;
end;

procedure TAppPanelHost.UpdateToolbarControls;
{
var
  tick: QWord;
  }
begin
  {
  tick:= GetTickCount64;
  }

  Toolbar.UpdateControls;

  {
  tick:= GetTickCount64-tick;
  Inc(FToolbarUpdateCount);
  Inc(FToolbarUpdateTime, tick);
  }
end;


var
  id: TAppPanelId;

initialization

  for id in TAppPanelId do
    if id<>cPaneNone then
      AppPanels[id]:= TAppPanelHost.Create;

finalization

  for id in TAppPanelId do
    if id<>cPaneNone then
      FreeAndNil(AppPanels[id]);

end.
