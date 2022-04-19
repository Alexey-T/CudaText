unit AppUniqueInstanceBase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, simpleipc;

const
  ParamsSeparator = #13;

var
  FIPCServer: TSimpleIPCServer;

procedure InitializeUniqueServer(const ServerId: String);

function GetFormattedParams(const AParams: array of string): String; //Alexey

function GetServerId(const Identifier: String): String;

implementation

uses
  LazUTF8;

const
  BaseServerId = 'tuniqueinstance_';

procedure InitializeUniqueServer(const ServerId: String);
begin
  //It's the first instance. Init the server
  if FIPCServer = nil then
  begin
    FIPCServer := TSimpleIPCServer.Create(nil);
    FIPCServer.ServerID := ServerId;
    FIPCServer.Global := True;
    try
      FIPCServer.StartServer;
    except
      //Alexey: hide the server error to fix CudaText #4079
    end;
  end;
end;

function GetFormattedParams(const AParams: array of string): String; //Alexey
var
  i: Integer;
begin
  Result := '';
  for i := Low(AParams) to High(AParams) do
    Result := Result + AParams[i] + ParamsSeparator;
end;

function GetServerId(const Identifier: String): String;
begin
  if Identifier <> '' then
    Result := BaseServerId + Identifier
  else
    Result := BaseServerId + ExtractFileName(ParamStrUTF8(0));
end;

finalization
  FIPCServer.Free;

end.

