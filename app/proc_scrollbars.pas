unit proc_scrollbars;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Graphics, StdCtrls, ComCtrls, Forms,
  LMessages, LCLType,
  ATScrollBar,
  ATListbox,
  proc_colors,
  proc_globdata,
  math;

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
  protected
    procedure DoEnter; override;
    procedure DoExit; override;
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
    SourceObject: TObject;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Themed: boolean read FThemed write SetThemed;
    procedure DoScaleScrollbar;
    procedure SetFocus; override;
    property ScrollVert: TATScroll read FScrollVert;
    property ScrollHorz: TATScroll read FScrollHorz;
  end;

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
  FScrollVert.WidthInitial:= UiOps_ScrollbarWidth;
  FScrollVert.IndentBorder:= UiOps_ScrollbarBorderSize;
  FScrollVert.OnChange:= @ScrollVertChange;

  FScrollHorz:= TATScroll.Create(Self);
  FScrollHorz.Parent:= Self;
  FScrollHorz.Kind:= sbHorizontal;
  FScrollHorz.Align:= alBottom;
  FScrollHorz.Height:= UiOps_ScrollbarWidth;
  FScrollHorz.WidthInitial:= UiOps_ScrollbarWidth;
  FScrollHorz.IndentBorder:= UiOps_ScrollbarBorderSize;
  FScrollHorz.IndentCorner:= UiOps_ScrollbarWidth;
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

procedure TAppTreeContainer.SetFocus;
begin
  Tree.SetFocus;
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
  FScrollVert.PageSize:= Tree.Height;
  FScrollVert.Max:= Tree.GetMaxScrollTop+FScrollVert.PageSize;
  FScrollVert.Position:= Tree.ScrolledTop;

  FScrollHorz.Min:= 0;
  FScrollHorz.PageSize:= Max(1, Tree.ClientWidth);
  FScrollHorz.Max:= Max(1, Tree.GetMaxScrollLeft+FScrollHorz.PageSize);
  FScrollHorz.Position:= Max(0, Tree.ScrolledLeft);
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

procedure TAppTreeView.DoEnter;
begin
  inherited;
  SelectionColor:= GetAppColor('TreeSelBg');
end;

procedure TAppTreeView.DoExit;
begin
  inherited;
  SelectionColor:= GetAppColor('TreeSelBg2');
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

