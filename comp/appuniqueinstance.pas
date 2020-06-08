unit AppUniqueInstance;

{
  UniqueInstance is a component to allow only a instance by program

  Copyright (C) 2006 Luiz Americo Pereira Camara
  pascalive@bol.com.br

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


{$mode objfpc}{$H+}
{$if not defined(Windows) or (FPC_FULLVERSION >= 30001)}
{$define PollIPCMessage}
{$endif}

interface

uses
  Forms, Classes, SysUtils, simpleipc, ExtCtrls;
  
type

  TOnOtherInstance = procedure (Sender : TObject; ParamCount: Integer; const Parameters: array of String) of object;

  { TUniqueInstance }

  TUniqueInstance = class(TComponent)
  private
    FIdentifier: String;
    FOnOtherInstance: TOnOtherInstance;
    FUpdateInterval: Cardinal;
    FEnabled: Boolean;
    FPriorInstanceRunning: Boolean;
    procedure ReceiveMessage(Sender: TObject);
    {$ifdef PollIPCMessage}
    procedure CheckMessage(Sender: TObject);
    {$endif}
  public
    constructor Create(AOwner: TComponent); override;
    property PriorInstanceRunning: Boolean read FPriorInstanceRunning;
    procedure Loaded(const AParams: array of string); reintroduce; //Alexey
  published
    property Enabled: Boolean read FEnabled write FEnabled default False;
    property Identifier: String read FIdentifier write FIdentifier;
    property UpdateInterval: Cardinal read FUpdateInterval write FUpdateInterval default 1000;
    property OnOtherInstance: TOnOtherInstance read FOnOtherInstance write FOnOtherInstance;
  end;

implementation

uses
  StrUtils, AppUniqueInstanceBase;

{ TUniqueInstance }

procedure TUniqueInstance.ReceiveMessage(Sender: TObject);
var
  ParamsArray: array of String;
  Params: String;
  Count, i: Integer;
begin
  if Assigned(FOnOtherInstance) then
  begin
    //MsgType stores ParamCount
    Count := FIPCServer.MsgType;
    SetLength(ParamsArray, Count);
    Params := FIPCServer.StringMessage;
    for i := 1 to Count do
      ParamsArray[i - 1] := ExtractWord(i, Params, [ParamsSeparator]);
    FOnOtherInstance(Self, Count, ParamsArray);
  end;
end;

{$ifdef PollIPCMessage}
procedure TUniqueInstance.CheckMessage(Sender: TObject);
begin
  FIPCServer.PeekMessage(1, True);
end;
{$endif}

procedure TUniqueInstance.Loaded(const AParams: array of string);
var
  IPCClient: TSimpleIPCClient;
  {$ifdef PollIPCMessage}
  Timer: TTimer;
  {$endif}
begin
  if not (csDesigning in ComponentState) and FEnabled then
  begin
    IPCClient := TSimpleIPCClient.Create(Self);
    IPCClient.ServerId := GetServerId(FIdentifier);
    if not Assigned(FIPCServer) and IPCClient.ServerRunning then
    begin
      //A older instance is running.
      FPriorInstanceRunning := True;
      //A instance is already running
      //Send a message and then exit
      if Assigned(FOnOtherInstance) then
      begin
        IPCClient.Active := True;
        IPCClient.SendStringMessage(ParamCount, GetFormattedParams(AParams));
      end;
      Application.ShowMainForm := False;
      Application.Terminate;
    end
    else
    begin
      if not Assigned(FIPCServer) then
        InitializeUniqueServer(IPCClient.ServerID);
      FIPCServer.OnMessage := @ReceiveMessage;
      //there's no more need for IPCClient
      IPCClient.Destroy;
      {$ifdef PollIPCMessage}
      if Assigned(FOnOtherInstance) then
      begin
        Timer := TTimer.Create(Self);
        Timer.Interval := FUpdateInterval;
        Timer.OnTimer := @CheckMessage;
      end;
      {$endif}
    end;
  end;//if
  inherited Loaded;
end;

constructor TUniqueInstance.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FUpdateInterval := 1000;
end;

end.

