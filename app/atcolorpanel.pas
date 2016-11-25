unit ATColorPanel;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Controls, ExtCtrls;

type
  { TATColorPanel }
  TATColorPanel = class(TPanel)
  private
    FBorderColor: TColor;
  public
    constructor Create(AOwner: TComponent); override;
  protected
    procedure Paint; override;
  published
    property BorderColor: TColor read FBorderColor write FBorderColor default clBlack;
  end;


implementation

{ TATColorPanel }

constructor TATColorPanel.Create(AOwner: TComponent);
begin
  inherited;
  BorderColor:= clBlack;
end;

procedure TATColorPanel.Paint;
var
  R: TRect;
begin
  inherited;
  if BorderWidth>0 then
  begin
    R:= ClientRect;
    Canvas.Frame3d(R, BorderColor, BorderColor, BorderWidth);
  end;
end;

end.

