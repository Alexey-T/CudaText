unit registeruniqueinstance;

{$Mode ObjFpc}
{$H+}

interface

uses 
  Classes, SysUtils, LResources, LazarusPackageIntf,uniqueinstance;
  
procedure Register;

implementation

procedure RegisterUnitUniqueInstance;
begin
  RegisterComponents('System',[TUniqueInstance]);
end;  

procedure Register;

begin
  RegisterUnit('uniqueinstance',@RegisterUnitUniqueInstance);
end; 

initialization
{$i uniqueicon.lrs}
 
end.
