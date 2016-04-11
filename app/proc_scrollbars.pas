unit proc_scrollbars;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Graphics, StdCtrls, ComCtrls, Forms,
  ATScrollBar, ATListbox;

type
  { TTreeViewMy }

  TTreeViewMy = class(TTreeView)
  private
    FScroll: TATScroll;
    procedure ScrollChange(Sender: TObject);
    procedure UpdScroll;
  protected
    procedure DoSelectionChanged; override;
    procedure Resize; override;
    procedure Changed; override;
    procedure Collapse(Node: TTreeNode); override;
    procedure Expand(Node: TTreeNode); override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

type
  { TATListboxMy }

  TATListboxMy = class(TATListbox)
  private
    FScroll: TATScroll;
    procedure ScrollChange(Sender: TObject);
    procedure UpdScroll;
  protected
    procedure Changed; override;
    procedure Resize; override;
  public
    constructor Create(AOwner: TComponent); override;
    function ClientWidth: integer;
  end;

const
  UiOps_ScrollbarWidth: integer = 14;
  UiOps_ScrollbarBorderSize: integer = 0;


implementation

{ TATListboxMy }

procedure TATListboxMy.ScrollChange(Sender: TObject);
begin
  ItemTop:= FScroll.Position;
end;

procedure TATListboxMy.UpdScroll;
begin
  if not Assigned(FScroll) then exit;
  FScroll.Min:= 0;
  FScroll.Max:= ItemCount;
  FScroll.PageSize:= VisibleItems;
  FScroll.Position:= ItemTop;
end;

procedure TATListboxMy.Changed;
begin
  inherited;
  UpdScroll;
end;

procedure TATListboxMy.Resize;
begin
  inherited;
  UpdScroll;
end;

constructor TATListboxMy.Create(AOwner: TComponent);
begin
  inherited;
  ShowScrollbar:= false;
  FScroll:= TATScroll.Create(Self);
  FScroll.Parent:= Self;
  FScroll.Kind:= sbVertical;
  FScroll.Align:= alRight;
  FScroll.Width:= UiOps_ScrollbarWidth;
  FScroll.IndentBorder:= UiOps_ScrollbarBorderSize;
  FScroll.OnChange:= @ScrollChange;
  UpdScroll;
end;

function TATListboxMy.ClientWidth: integer;
begin
  Result:= inherited ClientWidth - FScroll.Width;
end;

{ TTreeViewMy }

constructor TTreeViewMy.Create(AOwner: TComponent);
begin
  inherited;
  ScrollBars:= ssNone;
  FScroll:= TATScroll.Create(Self);
  FScroll.Parent:= Self;
  FScroll.Kind:= sbVertical;
  FScroll.Align:= alRight;
  FScroll.Width:= UiOps_ScrollbarWidth;
  FScroll.IndentBorder:= UiOps_ScrollbarBorderSize;
  FScroll.OnChange:= @ScrollChange;
  UpdScroll;
end;

procedure TTreeViewMy.ScrollChange(Sender: TObject);
begin
  ScrolledTop:= FScroll.Position;
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

procedure TTreeViewMy.Changed;
begin
  inherited;
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

