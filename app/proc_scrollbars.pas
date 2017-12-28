unit proc_scrollbars;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Graphics, StdCtrls, ComCtrls, Forms,
  LMessages, LCLType,
  ATSynEdit_ScrollBar,
  ATListbox;

type
  { TAppTreeView }

  TAppTreeView = class(TTreeView)
  protected
    procedure DoSelectionChanged; override;
    procedure Resize; override;
    procedure Collapse(Node: TTreeNode); override;
    procedure Expand(Node: TTreeNode); override;
    procedure CMChanged(var Message: TLMessage); message CM_CHANGED;
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean; override;
  public
  end;

type

  { TAppTreeContainer }

  TAppTreeContainer = class(TCustomControl)
  private
    FScrollVert: TATScroll;
    FScrollHorz: TATScroll;
    FThemed: boolean;
    procedure ScrollHorzChange(Sender: TObject);
    procedure ScrollVertChange(Sender: TObject);
    procedure SetThemed(AValue: boolean);
    procedure UpdateScrolls;
  public
    Tree: TAppTreeView;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Themed: boolean read FThemed write SetThemed;
    procedure DoScaleScrollbar;
  end;

const
  UiOps_ScrollbarWidth: integer = 14;
  UiOps_ScrollbarBorderSize: integer = 0;


implementation

constructor TAppTreeContainer.Create(AOwner: TComponent);
begin
  inherited;

  Tree:= TAppTreeView.Create(Self);
  Tree.Parent:= Self;
  Tree.Align:= alClient;

  FScrollVert:= TATScroll.Create(Self);
  FScrollVert.Parent:= Self;
  FScrollVert.Kind:= sbVertical;
  FScrollVert.Align:= alRight;
  FScrollVert.Width:= UiOps_ScrollbarWidth;
  FScrollVert.IndentBorder:= UiOps_ScrollbarBorderSize;
  FScrollVert.OnChange:= @ScrollVertChange;

  FScrollHorz:= TATScroll.Create(Self);
  FScrollHorz.Parent:= Self;
  FScrollHorz.Kind:= sbHorizontal;
  FScrollHorz.Align:= alBottom;
  FScrollHorz.Height:= UiOps_ScrollbarWidth;
  FScrollHorz.IndentBorder:= UiOps_ScrollbarBorderSize;
  FScrollHorz.OnChange:= @ScrollHorzChange;

  SetThemed(false);
  UpdateScrolls;
end;

destructor TAppTreeContainer.Destroy;
begin
  FreeAndNil(Tree);
  FreeAndNil(FScrollVert);
  FreeAndNil(FScrollHorz);
  inherited;
end;

procedure TAppTreeContainer.DoScaleScrollbar;
begin
  FScrollVert.AutoAdjustLayout(lapDefault, 96, Screen.PixelsPerInch, 100, 100);
  FScrollHorz.AutoAdjustLayout(lapDefault, 96, Screen.PixelsPerInch, 100, 100);
end;

procedure TAppTreeContainer.ScrollVertChange(Sender: TObject);
begin
  Tree.ScrolledTop:= FScrollVert.Position;
end;

procedure TAppTreeContainer.ScrollHorzChange(Sender: TObject);
begin
  Tree.ScrolledLeft:= FScrollHorz.Position;
end;

procedure TAppTreeContainer.SetThemed(AValue: boolean);
begin
  FThemed:= AValue;
  FScrollVert.Visible:= FThemed;
  FScrollHorz.Visible:= FThemed;
  if FThemed then
    Tree.ScrollBars:= ssNone
  else
    Tree.ScrollBars:= ssAutoBoth;
end;

procedure TAppTreeContainer.UpdateScrolls;
begin
  if not Assigned(Tree) then exit;
  if not Assigned(FScrollVert) then exit;
  if not Assigned(FScrollHorz) then exit;

  FScrollVert.Min:= 0;
  FScrollVert.PageSize:= ClientHeight;
  FScrollVert.Max:= Tree.GetMaxScrollTop+FScrollVert.PageSize;
  FScrollVert.Position:= Tree.ScrolledTop;

  FScrollHorz.Min:= 0;
  FScrollHorz.PageSize:= ClientWidth;
  FScrollHorz.Max:= Tree.GetMaxScrollLeft+FScrollHorz.PageSize;
  FScrollHorz.Position:= Tree.ScrolledLeft;
  FScrollHorz.IndentCorner:= FScrollHorz.Height;
end;

procedure TAppTreeView.DoSelectionChanged;
begin
  inherited;
  (Owner as TAppTreeContainer).UpdateScrolls;
end;

procedure TAppTreeView.Resize;
begin
  inherited;
  (Owner as TAppTreeContainer).UpdateScrolls;
end;

procedure TAppTreeView.CMChanged(var Message: TLMessage);
begin
  inherited;
  (Owner as TAppTreeContainer).UpdateScrolls;
end;

function TAppTreeView.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
  MousePos: TPoint): Boolean;
begin
  Result:= inherited;
  (Owner as TAppTreeContainer).UpdateScrolls;
end;

procedure TAppTreeView.Collapse(Node: TTreeNode);
begin
  inherited;
  (Owner as TAppTreeContainer).UpdateScrolls;
end;

procedure TAppTreeView.Expand(Node: TTreeNode);
begin
  inherited;
  (Owner as TAppTreeContainer).UpdateScrolls;
end;


end.

