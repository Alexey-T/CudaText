unit Test;    
                                    
interface

const
  cColorCodes: array[0..1] of TColorCode = (
    (sName: 'AliceBlue'; sVal: '#00'),
    (sName: 'AntiqueWhite'; sVal: '#00'),
    );
 asm
   MOV  AX, 1234H
   MOV Number, AX
 end;
    
type                        
  TForm1 = class(TForm)    
    procedure Button1Click(Sender: TObject);
  end;                             
                   
implementation                                      
                                                                       
{Syntax highlighting}  
procedure TForm1.Button1Click(Sender: TObject);
var
 Number: integer;
 FloatNumber: double;
begin
 Number := 123;  // View integer number style
 Caption := 'This Number is ' + IntToStr(Number); // View string style
 Inc(Number, $1FA7); // View HEX style
 {Assembler style}
 asm
   MOV  AX, 1234H
   MOV Number, AX
 end;
end;

begin
  if A then
  begin
    Do_;
  end;
end.
