unit proc_scrollbars;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Graphics, StdCtrls, ComCtrls, Forms,
  ATScrollBar;

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
  public
    constructor Create(AOwner: TComponent); override;
  end;


implementation

constructor TTreeViewMy.Create(AOwner: TComponent);
begin
  inherited;
  ScrollBars:= ssNone;
  FScroll:= TATScroll.Create(Self);
  FScroll.Parent:= Self;
  FScroll.Kind:= sbVertical;
  FScroll.Align:= alRight;
  FScroll.Width:= 18;
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


end.

