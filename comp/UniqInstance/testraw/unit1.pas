unit Unit1; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    ButCrashApp: TButton;
    procedure ButCrashAppClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  Form1: TForm1; 

implementation

{$R *.lfm}

{$ifdef unix}
uses
  BaseUnix;
{$endif}

{$ifdef windows}
uses
  Windows;
{$endif}

{ TForm1 }

procedure TForm1.ButCrashAppClick(Sender: TObject);
begin
  {$ifdef unix}
  FpKill(FpGetpid, 9);
  {$endif}
  {$ifdef windows}
  TerminateProcess(GetCurrentProcess, 0);
  {$endif}
end;

initialization

end.

