(*
This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at http://mozilla.org/MPL/2.0/.

Copyright (c) Alexey Torgashin
*)
unit proc_scrollbars;

{$mode objfpc}{$H+}
{$ScopedEnums on}

interface

uses
  {$ifdef windows}
  Windows, Messages,
  {$endif}
  LCLIntf, //SetScrollInfo
  Classes, SysUtils, Controls, Graphics, StdCtrls, ComCtrls, Forms,
  LMessages, LCLType,
  ATScrollBar,
  proc_colors,
  proc_globdata,
  math;

type
  { TAppTreeView }
  TAppTreeContainer = class;

  TAppTreeView = class(TTreeView)
  private
    procedure UpdateBars;
  protected
    procedure DoSelectionChanged; override;
    procedure Resize; override;
    procedure Collapse(Node: TTreeNode); override;
    procedure Expand(Node: TTreeNode); override;
    procedure CMChanged(var Message: TLMessage); message CM_CHANGED;
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean; override;
    procedure DoContextPopup(MousePos: TPoint; var Handled: Boolean); override;
    procedure DoEnter; override;
    procedure DoExit; override;
    {$ifdef windows}
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    {$endif}
  public
    Container: TAppTreeContainer;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
  end;

type
  TAppTreeScrollStyle = (
    Hide,
    Show,
    Auto
    );

type

  { TAppTreeContainer }

  TAppTreeContainer = class(TCustomControl)
  private
    FThemedColors: boolean;
    FScrollbarVert: TATScrollbar;
    FScrollbarHorz: TATScrollbar;
    FScrollbarsNew: boolean;
    procedure ScrollHorzChange(Sender: TObject);
    procedure ScrollVertChange(Sender: TObject);
    procedure SetScrollbarsNew(AValue: boolean);
    procedure TreeOnDeletion(Sender: TObject; Node: TTreeNode);
    procedure UpdateScrollbars;
  public
    Tree: TAppTreeView;
    ScrollStyleVert: TAppTreeScrollStyle;
    ScrollStyleHorz: TAppTreeScrollStyle;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property ThemedColors: boolean read FThemedColors write FThemedColors;
    property ScrollbarsNew: boolean read FScrollbarsNew write SetScrollbarsNew;
    property ScrollbarVert: TATScrollbar read FScrollbarVert;
    property ScrollbarHorz: TATScrollbar read FScrollbarHorz;
    procedure SetFocus; override;
    procedure Invalidate; override;
  end;

implementation

constructor TAppTreeContainer.Create(AOwner: TComponent);
begin
  inherited;

  Tree:= TAppTreeView.Create(nil);
  Tree.Parent:= Self;
  Tree.Align:= alClient;
  Tree.Container:= Self;
  Tree.OnDeletion:= @TreeOnDeletion;

  FScrollbarVert:= TATScrollbar.Create(nil);
  FScrollbarVert.Parent:= Self;
  FScrollbarVert.Kind:= sbVertical;
  FScrollbarVert.Align:= alRight;
  FScrollbarVert.Width:= ATScrollbarTheme.InitialSize;
  FScrollbarVert.OnChange:= @ScrollVertChange;

  FScrollbarHorz:= TATScrollbar.Create(nil);
  FScrollbarHorz.Parent:= Self;
  FScrollbarHorz.Kind:= sbHorizontal;
  FScrollbarHorz.Align:= alBottom;
  FScrollbarHorz.Height:= ATScrollbarTheme.InitialSize;
  FScrollbarHorz.IndentCorner:= 100;
  FScrollbarHorz.OnChange:= @ScrollHorzChange;

  ScrollStyleVert:= TAppTreeScrollStyle.Auto;
  ScrollStyleHorz:= TAppTreeScrollStyle.Auto;

  ThemedColors:= false;
  ScrollbarsNew:= false;
  UpdateScrollbars;
end;

destructor TAppTreeContainer.Destroy;
begin
  Tree.Container:= nil;
  FreeAndNil(Tree);
  FreeAndNil(FScrollbarVert);
  FreeAndNil(FScrollbarHorz);
  inherited;
end;

procedure TAppTreeContainer.SetFocus;
begin
  if GetParentForm(Self).CanFocus then
    if Tree.CanFocus then
      Tree.SetFocus;
end;

procedure TAppTreeContainer.Invalidate;
begin
  inherited;
  Tree.Invalidate;
  FScrollbarHorz.Invalidate;
  FScrollbarVert.Invalidate;
end;

procedure TAppTreeContainer.ScrollVertChange(Sender: TObject);
begin
  Tree.ScrolledTop:= FScrollbarVert.Position;
end;

procedure TAppTreeContainer.ScrollHorzChange(Sender: TObject);
begin
  Tree.ScrolledLeft:= FScrollbarHorz.Position;
end;

procedure TAppTreeContainer.SetScrollbarsNew(AValue: boolean);
begin
  FScrollbarsNew:= AValue;
  FScrollbarVert.Visible:= FScrollbarsNew;
  FScrollbarHorz.Visible:= FScrollbarsNew;
  if FScrollbarsNew then
    Tree.ScrollBars:= ssNone
  else
    Tree.ScrollBars:= ssAutoBoth;
end;

procedure TAppTreeContainer.TreeOnDeletion(Sender: TObject; Node: TTreeNode);
begin
  if Assigned(Node.Data) then
  begin
    TObject(Node.Data).Free;
    Node.Data:= nil;
  end;
end;

procedure TAppTreeContainer.UpdateScrollbars;
var
  Info: TScrollInfo;
begin
  if Tree=nil then exit;

  if ScrollbarsNew then
  begin
    if Assigned(FScrollbarVert) then
    begin
      FScrollbarVert.Min:= 0;
      FScrollbarVert.PageSize:= Tree.Height;
      FScrollbarVert.Max:= Tree.GetMaxScrollTop+FScrollbarVert.PageSize;
      FScrollbarVert.Position:= Tree.ScrolledTop;
      FScrollbarVert.Update;

      case ScrollStyleVert of
        TAppTreeScrollStyle.Hide:
          FScrollbarVert.Hide;
        TAppTreeScrollStyle.Show:
          FScrollbarVert.Show;
        else
          FScrollbarVert.Visible:= FScrollbarVert.Max>FScrollbarVert.PageSize;
      end;
    end;

    if Assigned(FScrollbarHorz) then
    begin
      FScrollbarHorz.Min:= 0;
      FScrollbarHorz.PageSize:= Max(1, Tree.ClientWidth);
      FScrollbarHorz.Max:= Max(1, Tree.GetMaxScrollLeft+FScrollbarHorz.PageSize);
      FScrollbarHorz.Position:= Max(0, Tree.ScrolledLeft);
      if Assigned(FScrollbarVert) and FScrollbarVert.Visible then
        FScrollbarHorz.IndentCorner:= 100
      else
        FScrollbarHorz.IndentCorner:= 0;
      FScrollbarHorz.Update;

      case ScrollStyleHorz of
        TAppTreeScrollStyle.Hide:
          FScrollbarHorz.Hide;
        TAppTreeScrollStyle.Show:
          FScrollbarHorz.Show;
        else
          FScrollbarHorz.Visible:= FScrollbarHorz.Max>FScrollbarHorz.PageSize;
      end;
    end;
  end;

  if not ScrollbarsNew and Tree.HandleAllocated then
  begin
    Info:= Default(TScrollInfo);
    Info.cbSize:= SizeOf(Info);
    Info.nMin:= 0;
    Info.nMax:= Tree.GetMaxScrollTop+Tree.Height;
    Info.nPage:= Tree.Height;
    Info.nPos:= Tree.ScrolledTop;
    Info.fMask:= SIF_ALL;
    LCLIntf.SetScrollInfo(Tree.Handle, SB_VERT, Info, true);
  end;
end;

{ TAppTreeView }

procedure TAppTreeView.UpdateBars;
begin
  if Assigned(Container) then
    Container.UpdateScrollbars;
end;

procedure TAppTreeView.DoSelectionChanged;
begin
  inherited;
  if Assigned(Selected) then
    UpdateBars;
end;

procedure TAppTreeView.Resize;
begin
  inherited;
  UpdateBars;
end;

procedure TAppTreeView.CMChanged(var Message: TLMessage);
begin
  inherited;
  UpdateBars;
end;

function TAppTreeView.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
  MousePos: TPoint): Boolean;
begin
  if ssShift in Shift then
  begin
    Result:= DoMouseWheelHorz(Shift, -WheelDelta, MousePos);
  end
  else
  begin
    Result:= inherited;
  end;

  UpdateBars;
end;

procedure TAppTreeView.DoContextPopup(MousePos: TPoint; var Handled: Boolean);
begin
  if not IsEnabled then exit; //prevent popup menu if form is disabled, needed for plugins dlg_proc API on Qt5

  inherited DoContextPopup(MousePos, Handled);
end;

procedure TAppTreeView.DoEnter;
begin
  inherited;
  SelectionColor:= GetAppColor(TAppThemeColor.TreeSelBg);

  //focus at least 1st item if no selection yet
  if Selected=nil then
    if Items.Count>0 then
      Selected:= Items[0];
end;

procedure TAppTreeView.DoExit;
begin
  inherited;
  SelectionColor:= GetAppColor(TAppThemeColor.TreeSelBg2);
end;

procedure TAppTreeView.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
end;

procedure TAppTreeView.Collapse(Node: TTreeNode);
begin
  inherited;
  UpdateBars;
end;

procedure TAppTreeView.Expand(Node: TTreeNode);
begin
  inherited;
  UpdateBars;
end;

{$ifdef windows}
procedure TAppTreeView.WMEraseBkgnd(var Message: TWMEraseBkgnd);
var
  R: TRect;
begin
  //to avoid flickering with white on app startup
  if (Message.DC<>0) then
  begin
    Brush.Color:= Color;
    R.Left:= 0;
    R.Top:= 0;
    R.Width:= Width;
    R.Height:= Height;
    Windows.FillRect(Message.DC, R, Brush.Reference.Handle);
  end;

  //to remove flickering on resize and mouse-over
  Message.Result:= 1;
end;
{$endif}

end.

