{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit uniqueinstance_package;

interface

uses
  UniqueInstance, UniqueInstanceRaw, registeruniqueinstance, 
  UniqueInstanceBase, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('registeruniqueinstance', @registeruniqueinstance.Register);
end;

initialization
  RegisterPackage('uniqueinstance_package', @Register);
end.
