unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs;

type

  { TForm1 }

  TForm1 = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormPaint(Sender: TObject);
  private
  public

  end;

var
  Form1: TForm1;

implementation

uses
  Gtk2Int, textout_faster_gtk2, LCLIntf, LCLType, LCLProc;

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  //
end;

procedure TForm1.FormPaint(Sender: TObject);
var
  C: TCanvas;
  Delta0: array of integer;
  i: integer;
  t_orig, t_new: qword;
  R: TRect;
begin
  C:= Canvas;
  C.Brush.Color:= clYellow;
  C.Font.Color:= clRed;
  C.Font.Size:= 9;
  C.TextOut(10, 10, 'textout');

  C.Brush.Color:= clCream;
  C.Font.Color:= clGreen;
  C.Font.Size:= 12;

  t_orig:= GetTickCount64;
  for i:= 1 to 1000 do
    ExtTextOut(C.Handle, 50, 70, 0, @R, 'text_11', 7, nil);
  t_orig:= GetTickCount64-t_orig;

  Delta0:= [10, 10, 10, 10, 10, 10, 10];
  TextOutFaster(C.Handle, 10, 30, 'text_fx', @Delta0[0]);

  t_new:= GetTickCount64;
  for i:= 1 to 1000 do
    TextOutFaster(C.Handle, 10, 50, 'text_f0', nil);
  t_new:= GetTickCount64-t_new;

  C.TextOut(2, 150, 't_orig: '+Inttostr(t_orig)+'  t_new: '+Inttostr(t_new));
end;

end.

