unit UniqueInstanceBase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, simpleipc, LazUTF8;

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
    Result := Result + ParamStrUTF8(i) + ParamsSeparator; //AT fix (utf8)
end;

function GetServerId(const Identifier: String): String;
begin
  if Identifier <> '' then
    Result := BaseServerId + Identifier
  else
    Result := BaseServerId + ExtractFileName(ParamStrUTF8(0)); //AT fix
end;

finalization
  FIPCServer.Free;

end.

