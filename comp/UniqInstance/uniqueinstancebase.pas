unit UniqueInstanceBase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, simpleipc;

const
  ParamsSeparator = '|';

var
  FIPCServer: TSimpleIPCServer;

procedure InitializeUniqueServer(const ServerId: String);

function GetFormattedParams: String;

function GetServerId(const Identifier: String): String;

implementation

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
    FIPCServer.StartServer;
  end;
end;

function GetFormattedParams: String;
var
  i: Integer;
begin
  Result := '';
  for i := 1 to ParamCount do
    Result := Result + ParamStr(i) + ParamsSeparator;
end;

function GetServerId(const Identifier: String): String;
begin
  if Identifier <> '' then
    Result := BaseServerId + Identifier
  else
    Result := BaseServerId + ExtractFileName(ParamStr(0));
end;

finalization
  FIPCServer.Free;

end.

