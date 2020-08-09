(*
This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at http://mozilla.org/MPL/2.0/.

Copyright (c) Alexey Torgashin
*)
unit proc_scrollbars;

{$mode objfpc}{$H+}

interface

uses
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
    procedure UpdateBars; inline;
  protected
    procedure DoSelectionChanged; override;
    procedure Resize; override;
    procedure Collapse(Node: TTreeNode); override;
    procedure Expand(Node: TTreeNode); override;
    procedure CMChanged(var Message: TLMessage); message CM_CHANGED;
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean; override;
  protected
    procedure DoEnter; override;
    procedure DoExit; override;
  public
    Container: TAppTreeContainer;
  end;

type

  { TAppTreeContainer }

  TAppTreeContainer = class(TCustomControl)
  private
    FScrollbarVert: TATScrollbar;
    FScrollbarHorz: TATScrollbar;
    FThemed: boolean;
    procedure ScrollHorzChange(Sender: TObject);
    procedure ScrollVertChange(Sender: TObject);
    procedure SetThemed(AValue: boolean);
    procedure UpdateScrollbars;
  public
    Tree: TAppTreeView;
    SourceObject: TObject;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Themed: boolean read FThemed write SetThemed;
    procedure SetFocus; override;
    property ScrollbarVert: TATScrollbar read FScrollbarVert;
    property ScrollbarHorz: TATScrollbar read FScrollbarHorz;
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

  FScrollbarVert:= TATScrollbar.Create(nil);
  FScrollbarVert.Parent:= Self;
  FScrollbarVert.Kind:= sbVertical;
  FScrollbarVert.Align:= alRight;
  FScrollbarVert.Width:= UiOps.ScrollbarWidth;
  FScrollbarVert.OnChange:= @ScrollVertChange;

  FScrollbarHorz:= TATScrollbar.Create(nil);
  FScrollbarHorz.Parent:= Self;
  FScrollbarHorz.Kind:= sbHorizontal;
  FScrollbarHorz.Align:= alBottom;
  FScrollbarHorz.Height:= UiOps.ScrollbarWidth;
  FScrollbarHorz.IndentCorner:= 100;
  FScrollbarHorz.OnChange:= @ScrollHorzChange;

  SetThemed(false);
  UpdateScrollbars;
end;

destructor TAppTreeContainer.Destroy;
begin
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
  FScrollbarHorz.Update;
  FScrollbarVert.Update;
  inherited Invalidate;
end;

procedure TAppTreeContainer.ScrollVertChange(Sender: TObject);
begin
  Tree.ScrolledTop:= FScrollbarVert.Position;
end;

procedure TAppTreeContainer.ScrollHorzChange(Sender: TObject);
begin
  Tree.ScrolledLeft:= FScrollbarHorz.Position;
end;

procedure TAppTreeContainer.SetThemed(AValue: boolean);
begin
  FThemed:= AValue;
  FScrollbarVert.Visible:= FThemed;
  FScrollbarHorz.Visible:= FThemed;
  if FThemed then
    Tree.ScrollBars:= ssNone
  else
    Tree.ScrollBars:= ssAutoBoth;
end;

procedure TAppTreeContainer.UpdateScrollbars;
begin
  if not Assigned(Tree) then exit;
  if not Assigned(FScrollbarVert) then exit;
  if not Assigned(FScrollbarHorz) then exit;

  FScrollbarVert.Min:= 0;
  FScrollbarVert.PageSize:= Tree.Height;
  FScrollbarVert.Max:= Tree.GetMaxScrollTop+FScrollbarVert.PageSize;
  FScrollbarVert.Position:= Tree.ScrolledTop;

  FScrollbarHorz.Min:= 0;
  FScrollbarHorz.PageSize:= Max(1, Tree.ClientWidth);
  FScrollbarHorz.Max:= Max(1, Tree.GetMaxScrollLeft+FScrollbarHorz.PageSize);
  FScrollbarHorz.Position:= Max(0, Tree.ScrolledLeft);

  FScrollbarVert.Update;
  FScrollbarHorz.Update;
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
  Result:= inherited;
  UpdateBars;
end;

procedure TAppTreeView.DoEnter;
begin
  inherited;
  SelectionColor:= GetAppColor(apclTreeSelBg);
end;

procedure TAppTreeView.DoExit;
begin
  inherited;
  SelectionColor:= GetAppColor(apclTreeSelBg2);
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


end.

