unit proc_scrollbars;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Graphics, StdCtrls, ComCtrls, Forms,
  LMessages, LCLType,
  ATSynEdit_ScrollBar,
  ATListbox;

type
  { TTreeViewMy }

  TTreeViewMy = class(TTreeView)
  private
    FScroll: TATScroll;
    FThemed: boolean;
    procedure ScrollChange(Sender: TObject);
    procedure SetThemed(AValue: boolean);
    procedure UpdScroll;
  protected
    procedure DoSelectionChanged; override;
    procedure Resize; override;
    procedure Collapse(Node: TTreeNode); override;
    procedure Expand(Node: TTreeNode); override;
    procedure CMChanged(var Message: TLMessage); message CM_CHANGED;
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    property Themed: boolean read FThemed write SetThemed;
    procedure DoScaleScrollbar;
  end;

const
  UiOps_ScrollbarWidth: integer = 14;
  UiOps_ScrollbarBorderSize: integer = 0;


implementation

{ TTreeViewMy }

constructor TTreeViewMy.Create(AOwner: TComponent);
begin
  inherited;

  FScroll:= TATScroll.Create(Self);
  FScroll.Parent:= Self;
  FScroll.Kind:= sbVertical;
  FScroll.Align:= alRight;
  FScroll.Width:= UiOps_ScrollbarWidth;
  FScroll.IndentBorder:= UiOps_ScrollbarBorderSize;
  FScroll.OnChange:= @ScrollChange;

  SetThemed(false);
  UpdScroll;
end;

procedure TTreeViewMy.DoScaleScrollbar;
begin
  FScroll.AutoAdjustLayout(lapDefault, 96, Screen.PixelsPerInch, 100, 100);
end;

procedure TTreeViewMy.ScrollChange(Sender: TObject);
begin
  ScrolledTop:= FScroll.Position;
end;

procedure TTreeViewMy.SetThemed(AValue: boolean);
begin
  FThemed:= AValue;
  FScroll.Visible:= FThemed;
  if FThemed then
    ScrollBars:= ssNone
  else
    ScrollBars:= ssAutoBoth;
end;

procedure TTreeViewMy.UpdScroll;
begin
  if not Assigned(FScroll) then exit;
  FScroll.Min:= 0;
  FScroll.PageSize:= ClientHeight;
  FScroll.Max:= GetMaxScrollTop+FScroll.PageSize;
  FScroll.Position:= ScrolledTop;
end;

procedure TTreeViewMy.DoSelectionChanged;
begin
  inherited;
  UpdScroll;
end;

procedure TTreeViewMy.Resize;
begin
  inherited;
  UpdScroll;
end;

procedure TTreeViewMy.CMChanged(var Message: TLMessage);
begin
  inherited;
  UpdScroll;
end;

function TTreeViewMy.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
  MousePos: TPoint): Boolean;
begin
  Result:= inherited;
  UpdScroll;
end;

procedure TTreeViewMy.Collapse(Node: TTreeNode);
begin
  inherited Collapse(Node);
  UpdScroll;
end;

procedure TTreeViewMy.Expand(Node: TTreeNode);
begin
  inherited Expand(Node);
  UpdScroll;
end;


end.

