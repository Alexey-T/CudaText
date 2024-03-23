unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  proc_str_c_syntax;

type

  { TForm1 }

  TForm1 = class(TForm)
    Edit1: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    procedure Edit1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}


{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  Edit1.OnChange(nil);
end;

procedure TForm1.Edit1Change(Sender: TObject);
var
  S: UnicodeString;
  t: qword;
const
  cnt=500;
var
  i: integer;
begin
  S:= Edit1.Text;
  t:= GetTickCount64;
  for i:= 1 to cnt do
  begin
    Label1.caption:= 'begins with keyword: '+BoolToStr(CSyntax_LineBeginsWithBlockKeyword(S), true);
    Label2.caption:= 'ends with symbol: '+CSyntax_LineEndSymbol(S);
  end;
  t:= GetTickCount64-t;
  Label3.caption:= 'time: '+FloatToStr(t/cnt);
end;

end.

