unit proc_linklabel;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Controls, StdCtrls,
  LclIntf;

type
  { TLinkLabel }

  TLinkLabel = class(TLabel)
  private
    FLink: string;
    procedure SetLink(AValue: string);
  public
    constructor Create(AOwner: TComponent); override;
  protected
    procedure Click; override;
    procedure MouseEnter; override;
    procedure MouseLeave; override;
  published
    property Link: string read FLink write SetLink;
  end;

implementation

const
  ColorLinkPassive = clHighlight;
  ColorLinkActive = clRed;

procedure TLinkLabel.SetLink(AValue: string);
begin
  if FLink=AValue then Exit;
  FLink:= AValue;
  Hint:= AValue;
end;

constructor TLinkLabel.Create(AOwner: TComponent);
begin
  inherited;
  Font.Color:= ColorLinkPassive;
  Cursor:= crHandPoint;
  ShowHint:= true;
end;

procedure TLinkLabel.Click;
begin
  if Link<>'' then
    OpenURL(Link);
end;

procedure TLinkLabel.MouseEnter;
begin
  Font.Color:= ColorLinkActive;
  Font.Style:= Font.Style+[fsUnderline];
end;

procedure TLinkLabel.MouseLeave;
begin
  Font.Color:= ColorLinkPassive;
  Font.Style:= Font.Style-[fsUnderline];
end;

end.

