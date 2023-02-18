{ Scrolling Text component

  Copyright (C)2014 Gordon Bamber minesadorada@charcodelvalle.com

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}

unit ScrollingText;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, LCLIntf, LCLTranslator;

const
  C_VERSION = '1.0.2.3';

type
  TTextSource = (stStringlist, stTextfile, stResource);

  TScrollingText = class(TGraphicControl)
  private
    FActive: boolean;
    FActiveLine: integer;   //the line over which the mouse hovers
    FBuffer: TBitmap;
    FEndLine: integer;
    FLineHeight: integer;
    FLines: TStrings;
    FNumLines: integer;
    FOffset: integer;
    FStartLine: integer;
    FStepSize: integer;
    FTimer: TTimer;
    FFont: TFont;
    FLinkFont:TFont;
    FBackColor: TColor;
    fVersionString: string;
    function ActiveLineIsURL: boolean;
    procedure DoTimer(Sender: TObject);
    procedure SetActive(const AValue: boolean);
    procedure Init;
    procedure DrawScrollingText(Sender: TObject);
    procedure SetLines(AValue: TStrings);
    procedure SetFont(AValue: TFont);
    procedure SetLinkFont(AValue: TFont);
  protected
    procedure DoOnChangeBounds; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property OnClick;
    // Can be set in design mode. Note URL links are inactive in design mode
    property Active: boolean read FActive write SetActive;
    // Inherited property
    property Align;
    // Inherited property
    property Anchors;
    // Inherited property
    property BiDiMode;
    // Inherited property
    property Constraints;
    // Inherited property
    property Enabled;
    // Inherited property
    property Borderspacing;
    // Can be set in design or runtime mode. (TextSource=stStringlist)
    property Lines: TStrings read FLines write SetLines;
    // Sets the background color of the window
    property BackColor: TColor read fBackColor write fBackColor default clWindow;
    // Sets the font properties of the scrolling text
    property Font: TFont read fFont write SetFont;
    // Sets the font properties of links in the scrolling text
    property LinkFont: TFont read fLinkFont write SetLinkFont;
    // Version number of this component
    property Version: string read fVersionString;
  end;


implementation

procedure TScrollingText.SetFont(AValue: TFont);
begin
  fFont.Assign(AValue);
end;

procedure TScrollingText.SetLinkFont(AValue: TFont);
begin
  fLinkFont.Assign(AValue);
end;

procedure TScrollingText.SetLines(AValue: TStrings);
begin
  fLines.Assign(AValue);
end;

procedure TScrollingText.SetActive(const AValue: boolean);
begin
  FActive := AValue;
  if FActive then
  begin
       Init;
       FOffset := FBuffer.Height; // Start at the bottom of the window
  end;
  FTimer.Enabled := Active;
end;

procedure TScrollingText.Init;
begin
  FBuffer.Width := Width;
  FBuffer.Height := Height;
  FLineHeight := FBuffer.Canvas.TextHeight('X');
  FNumLines := FBuffer.Height div FLineHeight;

  if FOffset = -1 then
    FOffset := FBuffer.Height;

  with FBuffer.Canvas do
  begin
    Brush.Color := fBackColor;
    Brush.Style := bsSolid;
    FillRect(0, 0, Width, Height);
  end;
end;

procedure TScrollingText.DrawScrollingText(Sender: TObject);
begin
  if Active then
    Canvas.Draw(0,0, FBuffer);
end;

procedure TScrollingText.DoTimer(Sender: TObject);
var
  w: integer;
  s: string;
  i: integer;
begin
  if not Active then
    Exit;

  Dec(FOffset, FStepSize);

  if FOffSet < 0 then
    FStartLine := -FOffset div FLineHeight
  else
    FStartLine := 0;

  FEndLine := FStartLine + FNumLines + 1;
  if FEndLine > FLines.Count - 1 then
    FEndLine := FLines.Count - 1;

  FBuffer.Canvas.FillRect(Rect(0, 0, FBuffer.Width, FBuffer.Height));

  for i := FEndLine downto FStartLine do
  begin
    s := Trim(FLines[i]);

    //reset buffer font
    FBuffer.Canvas.Font := fFont;
    FBuffer.Canvas.Font.Style := fFont.Style;
    FBuffer.Canvas.Font.Color := fFont.Color;

    //skip empty lines
    if Length(s) > 0 then
    begin
      //check for url
      if (Pos('http://', s) <> 0)
      OR (Pos('https://', s) <> 0)
      OR  (Pos('mailto:', s) <> 0) then
      begin
        FBuffer.Canvas.Font := FLinkFont;
        if i = FActiveLine then FBuffer.Canvas.Font.Style := FBuffer.Canvas.Font.Style+[fsUnderline];
      end;
      //check for bold format token
      if s[1] = '#' then
      begin
        s := copy(s, 2, Length(s) - 1);
        FBuffer.Canvas.Font.Style := [fsBold];
      end;
      w := FBuffer.Canvas.TextWidth(s);
      FBuffer.Canvas.TextOut((FBuffer.Width - w) div 2, FOffset + i * FLineHeight, s);
    end;
  end;

  //start showing the list from the start
  if FStartLine > FLines.Count - 1 then
    FOffset := FBuffer.Height;
  Invalidate;
end;

function TScrollingText.ActiveLineIsURL: boolean;
begin
  if (FActiveLine > 0) and (FActiveLine < FLines.Count) then
    Result := (Pos('http://', FLines[FActiveLine]) <> 0)
    OR (Pos('https:', FLines[FActiveLine]) <> 0)
    OR (Pos('mailto:', FLines[FActiveLine]) <> 0)
  else
    Result := False;
end;

procedure TScrollingText.DoOnChangeBounds;
begin
  inherited DoOnChangeBounds;
  Init;
end;

procedure TScrollingText.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: integer);
var
  s:string;
begin
  inherited MouseDown(Button, Shift, X, Y);
  if ActiveLineIsURL and
  ((Pos('http://', FLines[FActiveLine]) OR (Pos('https://', FLines[FActiveLine]))<> 0)) then
  begin
    s:=FLines[FActiveLine];
    if (Pos(' ',s))=0 then s:=Copy(s,Pos('http://',s),MaxInt)
    else begin
      if Pos(' ',s)<Pos('http://',s) then s:=Copy(s,Pos('http://',s),MaxInt);
      if (Pos(' ',s))=0 then s:=Copy(s,Pos('http://',s),MaxInt)
      else s:=Copy(s,Pos('http://',s),(Pos(' ',s)-Pos('http://',s)));
    end;
    if (Pos(' ',s))=0 then s:=Copy(s,Pos('https://',s),MaxInt)
    else begin
      if Pos(' ',s)<Pos('https://',s) then s:=Copy(s,Pos('https://',s),MaxInt);
      if (Pos(' ',s))=0 then s:=Copy(s,Pos('https://',s),MaxInt)
      else s:=Copy(s,Pos('https://',s),(Pos(' ',s)-Pos('https://',s)));
    end;
    OpenURL(s);
  end
  else if ActiveLineIsURL and (Pos('mailto:', FLines[FActiveLine]) <> 0) then begin
    s:=FLines[FActiveLine];
    if (Pos(' ',s))=0 then s:=Copy(s,Pos('mailto:',s),MaxInt)
    else begin
      if Pos(' ',s)<Pos('mailto:',s) then s:=Copy(s,Pos('mailto:',s),MaxInt);
      if (Pos(' ',s))=0 then s:=Copy(s,Pos('mailto:',s),MaxInt)
      else s:=Copy(s,Pos('mailto:',s),(Pos(' ',s)-Pos('mailto:',s)));
    end;
    OpenURL(s);
  end;
end;

procedure TScrollingText.MouseMove(Shift: TShiftState; X, Y: integer);
begin
  inherited MouseMove(Shift, X, Y);

  //calculate what line is clicked from the mouse position
  FActiveLine := (Y - FOffset) div FLineHeight;

  Cursor := crDefault;

  if (FActiveLine >= 0) and (FActiveLine < FLines.Count) and ActiveLineIsURL then
    Cursor := crHandPoint;
end;

constructor TScrollingText.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  ControlStyle := ControlStyle + [csOpaque];

  OnPaint := @DrawScrollingText;
  FLines := TStringList.Create;
  FTimer := TTimer.Create(nil);
  FTimer.OnTimer := @DoTimer;
  FTimer.Interval := 30;
  FBuffer := TBitmap.Create;
  FFont := TFont.Create;
  FLinkFont := TFont.Create;
  FFont.Size := 10;
  FLinkFont.Size := 10;
  fBackColor := clWindow;

  FStepSize := 1;
  FStartLine := 0;
  FOffset := -1;
  Width := 100;
  Height := 100;
  fVersionString := C_VERSION;
  SendToBack;

end;

destructor TScrollingText.Destroy;
begin
  FLines.Free;
  FTimer.Free;
  FBuffer.Free;
  FFont.Free;
  FLinkFont.Free;
  inherited Destroy;
end;

end.
