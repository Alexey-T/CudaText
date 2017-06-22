(*
This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at http://mozilla.org/MPL/2.0/.

Copyright (c) Alexey Torgashin
*)
unit fix_focus_window;

{$mode objfpc}{$H+}

interface

uses
  {$ifdef windows}
  Windows,
  SimpleIPC, JsonConf, UniqueInstanceBase, Classes, proc_globdata, proc_msg,
  {$endif}
  SysUtils,
  LCLIntf;

// Create Component using SimpleIPC Server&Client to detect running instances
{$ifdef windows}
type

  { TUniqueWinInstance }
  TUniqueWinInstance = class(TComponent)
  private
    _WindowHandle: HWND;
    _Server: TSimpleIPCServer;
    _TalkWith: String;
    _ServerId: String;
    _UniqueInstanceId: String;
    procedure ReceivedMessage(Sender: TObject);
  protected
    procedure SetServerId(Id: String);
    procedure SetTargetId(Id: String);
    procedure SetWndHandle(Wnd: HWND);
    procedure SetUniqueInstanceId(Id: String);
  public
    property WindowHandle: HWND write SetWndHandle;
    property TargetId: String write SetTargetId;
    property Id: String write SetServerId;
    property UniqueInstanceId: String write SetUniqueInstanceId;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function IsAnotherInstance: boolean;
    procedure StartListening;
  end;


var
  // For handling response from our existing window on FormMain
  OneWinInstanceRunning: TUniqueWinInstance;

//Read ui_one_instance option from user config file and get result
function IsSetToOneInstance: boolean;
//Block another window instance if Single Instance is True
function IsAnotherInstanceRunning:boolean;
{$ifend}

{$ifndef windows}
procedure DoFocusWindow(h: THandle);
{$ifend}

implementation

{$ifdef windows}
type
  TSwitchFunc = procedure(h: HWND; fAltTab: BOOL); stdcall;

var
  SwitchFunc: TSwitchFunc = nil;
  hLib: HInst;
  OneWinInstance: TUniqueWinInstance;

function IsSetToOneInstance: boolean;
var
  c: TJSONConfig;
  fn: String;
begin
  Result := False;
  fn := GetAppPath(cFileOptionsUser);
  c := TJSONConfig.Create(nil);
  try
    try
      c.Filename := fn;
    except
      on E: Exception do
      begin
        MsgBox(msgStatusErrorInConfigFile+#13+fn+#13#13+E.Message, MB_OK or MB_ICONERROR);
        Exit;
      end;
    end;
    Result := c.GetValue('ui_one_instance', False);
  finally
    c.Free;
  end;
end;

{ TUniqueWinInstance }

procedure TUniqueWinInstance.ReceivedMessage(Sender: TObject);
var
  CudaWnd: String;
  Client: TSimpleIPCClient;
begin
  CudaWnd := _Server.StringMessage;

  if CudaWnd = 'GETCUDAWND' then
  begin
    Client := TSimpleIPCClient.Create(Self);
    Client.ServerId := _TalkWith;
    if Client.ServerRunning then
    begin
      Client.Active := True;
      Client.SendStringMessage(IntToStr(_WindowHandle));
    end;

    Client.Free;
  end
  else // it will receive existing window handle to switch to
  begin
    Client := TSimpleIPCClient.Create(Self);
    Client.ServerId := _UniqueInstanceId;
    if Client.ServerRunning then
    begin
      Client.Active := True;
      Client.SendStringMessage(ParamCount, GetFormattedParams);
    end;
    hLib:= LoadLibrary('user32.dll');
    try
      Pointer(SwitchFunc):= GetProcAddress(hLib, 'SwitchToThisWindow');
    finally
      FreeLibrary(hLib);
    end;
    Sleep(10);
    if Assigned(SwitchFunc) then
    begin
       SwitchFunc(StrToInt(CudaWnd), True);
    end;
    Client.Free;
  end;
end;

procedure TUniqueWinInstance.SetServerId(Id: String);
begin
  _ServerId:=Id;
end;

procedure TUniqueWinInstance.SetTargetId(Id: String);
begin
  _TalkWith:=Id;
end;

procedure TUniqueWinInstance.SetWndHandle(Wnd: HWND);
begin
  _WindowHandle:=Wnd;
end;

procedure TUniqueWinInstance.SetUniqueInstanceId(Id: String);
begin
  _UniqueInstanceId:=Id;
end;

constructor TUniqueWinInstance.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  _Server := TSimpleIPCServer.Create(Self);
end;

destructor TUniqueWinInstance.Destroy;
begin
  _Server.Free;
  inherited Destroy;
end;

function TUniqueWinInstance.IsAnotherInstance: boolean;
var
  client: TSimpleIPCClient;
begin
  Result := False;

  // start our server to listen commands from existing cudatext's window handle
  _Server.ServerID:=_ServerId;
  _Server.Global:=True;
  _Server.OnMessage:=@ReceivedMessage;
  _Server.StartServer;

  // find out if already running another cudatext's instance
  client := TSimpleIPCClient.Create(Self);
  client.ServerID:=_UniqueInstanceId;
  if client.ServerRunning then
  begin
    Result := True; // Another instances is running
    client.Free;

    // send pseudo command to ask for running cudatext's window instance handle
    client := TSimpleIPCClient.Create(Self);
    client.ServerID:=_TalkWith;
    if client.ServerRunning then
    begin
      client.Active:=True;
      client.SendStringMessage('GETCUDAWND');
    end;
  end;

  client.Free;

end;

procedure TUniqueWinInstance.StartListening;
begin
  _Server.ServerID:=_ServerId;
  _Server.Global:=True;
  _Server.OnMessage:=@ReceivedMessage;
  _Server.StartServer;
end;

function IsAnotherInstanceRunning:boolean;
begin

  Result := False;

  if IsSetToOneInstance then
  begin

    OneWinInstance := TUniqueWinInstance.Create(nil);
    OneWinInstance.SetServerId('cudatext.1');
    OneWinInstance.SetUniqueInstanceId(GetServerId('cudatext.0'));
    OneWinInstance.SetTargetId('cudatext.2');
    Result := OneWinInstance.IsAnotherInstance;

    OneWinInstance.Free;
  end;

end;

{$else}
procedure DoFocusWindow(h: THandle);
begin
  LCLIntf.SetForegroundWindow(h);
end;
{$endif}

end.
