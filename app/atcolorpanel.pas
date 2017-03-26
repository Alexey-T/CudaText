(*
This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at http://mozilla.org/MPL/2.0/.

Copyright (c) Alexey Torgashin
*)
unit ATColorPanel;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Controls, ExtCtrls, Types;

type
  { TATColorPanel }

  TATColorPanel = class(TCustomControl)
  private
    FBorderColor: TColor;
  public
    constructor Create(AOwner: TComponent); override;
  protected
    procedure Paint; override;
  published
    property Align;
    property Caption;
    property Color;
    property ParentColor;
    property Enabled;
    property Font;
    property Visible;
    property BorderColor: TColor read FBorderColor write FBorderColor default clBlack;
    property BorderWidth;
    property OnClick;
    property OnDblClick;
    property OnResize;
  end;


implementation

{ TATColorPanel }

constructor TATColorPanel.Create(AOwner: TComponent);
begin
  inherited;
  Caption:= '';
  BorderStyle:= bsNone;
  BorderWidth:= 0;
  BorderColor:= clBlack;
end;

procedure TATColorPanel.Paint;
var
  R: TRect;
  Pnt: TPoint;
  Size: TSize;
begin
  //inherited;

  R:= ClientRect;
  if BorderWidth>0 then
    Canvas.Frame3d(R, BorderColor, BorderColor, BorderWidth);

  if Caption<>'' then
  begin
    Canvas.Font.Assign(Self.Font);
    Size:= Canvas.TextExtent(Caption);
    Pnt.X:= (R.Right-Size.cx) div 2;
    Pnt.Y:= (R.Bottom-Size.cy) div 2;
    Canvas.TextOut(Pnt.X, Pnt.Y, Caption);
  end;
end;

end.

