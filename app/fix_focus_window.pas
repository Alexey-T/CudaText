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
  Classes,
  InterfaceBase,
  LCLType,
  LazUTF8,
  proc_globdata,
  proc_msg,
  at__jsonconf,
  {$endif}
  SysUtils,
  LCLIntf;

{$ifdef windows}
const
  AppUniqueUID = '{950ccfac-9878-4e1a-b50e-0dd66b92679c}';

type
  { TPageFileStream }
  TPageFileStreamStates = set of (pfsValid, pfsOpenExisting);
  TPageFileStream = class(TCustomMemoryStream)
  strict private
    FMapHandle: THandle;
    FStates: TPageFileStreamStates;
    procedure DoMap(DesiredAccess: DWORD);
  public
    constructor Create(MapSize: SizeUInt; const MapName: UnicodeString = '');
    constructor CreateForRead(const MapName: UnicodeString);
    destructor Destroy; override;
    function Write(const Buffer; Count: LongInt): LongInt; override;
    property States: TPageFileStreamStates read FStates;
  end;

  { TInstanceManager }
  TInstanceStatus = (isNotChecked, isFirst, isSecond);
  TSecondInstanceStartedEvent = procedure(const SentFromSecondInstance: TBytes) of object;
  TInstanceManage = class(TObject)
  strict private
    FDataPFS: TPageFileStream;
    FDataPFSName: UnicodeString;
    FEventHandler: PEventHandler;
    FOnSecondInstanceStarted: TSecondInstanceStartedEvent;
    FStatus: TInstanceStatus;
    FUniqueAppId: String;
    FWakeupEvent: THandle;
    FWakeupEventName: UnicodeString;
    FWndHandlePFS: TPageFileStream;
    FWndHandlePFSName: UnicodeString;
    procedure ClearEventHandler;
    procedure OtherInstanceStarted(Unused1: PtrInt; Unused2: DWORD);
    procedure SetOnSecondInstanceStarted(const AValue: TSecondInstanceStartedEvent);
  public
    constructor Create(const UniqueAppId: String);
    destructor Destroy; override;
    procedure Check;
    function ActivateFirstInstance(const DataForFirstInstance: TBytes = nil): Boolean;
    function SetFormHandleForActivate(Handle: HWND): Boolean;
    property OnSecondInstanceStarted: TSecondInstanceStartedEvent read
        FOnSecondInstanceStarted write SetOnSecondInstanceStarted;
    property Status: TInstanceStatus read FStatus;
    property UniqueAppId: string read FUniqueAppId;
  end;

var
  // For handling response from our existing window on FormMain
  FInstanceManage: TInstanceManage;

//Read ui_one_instance option from user config file and get result
function IsSetToOneInstance: boolean;
//Block another window instance if Single Instance is True
function IsAnotherInstanceRunning:boolean;
procedure debug(const text: String);
{$ifend}

{$ifndef windows}
procedure DoFocusWindow(h: THandle);
{$ifend}

implementation

{$ifdef windows}
const
  ParamsSeparator = '|';

type
  TSwitchFunc = procedure(h: HWND; fAltTab: BOOL); stdcall;

var
  SwitchFunc: TSwitchFunc = nil;

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

{ TInstanceManage }

procedure TInstanceManage.ClearEventHandler;
begin
  if Assigned(FEventHandler) then
  begin
    WidgetSet.RemoveEventHandler(FEventHandler);
    FEventHandler := nil;
  end;
end;

{$PUSH}
{$WARN 5024 OFF : Parameter "$1" not used}
procedure TInstanceManage.OtherInstanceStarted(Unused1: PtrInt; Unused2: DWORD);
var
  Bytes: TBytes;
begin
  if (FStatus = isFirst) and Assigned(FOnSecondInstanceStarted) then
  begin
    with TPageFileStream.CreateForRead(FDataPFSName) do
      try
        if pfsValid in States then
        begin
          SetLength(Bytes, ReadDWord);
          ReadBuffer(Bytes[0], Length(Bytes));
        end;
      finally
        Free;
      end;
    FOnSecondInstanceStarted(Bytes);
  end;
end;
{$POP}

procedure TInstanceManage.SetOnSecondInstanceStarted(
  const AValue: TSecondInstanceStartedEvent);
begin
  if FStatus <> isNotChecked then
  begin
    FOnSecondInstanceStarted := AValue;
    ClearEventHandler;
    if Assigned(FOnSecondInstanceStarted) then
      FEventHandler := WidgetSet.AddEventHandler(FWakeupEvent, 0,
        @OtherInstanceStarted, 0);
  end;
end;

constructor TInstanceManage.Create(const UniqueAppId: String);
begin
  inherited Create;
  FUniqueAppId := UniqueAppId;
  FDataPFSName := UTF8Decode(FUniqueAppId + '_MapData');
  FWakeupEventName := UTF8Decode(FUniqueAppId + '_Event');
  FWndHandlePFSName := UTF8Decode(FUniqueAppId + '_MapWnd');
end;

destructor TInstanceManage.Destroy;
begin
  ClearEventHandler;
  if FWakeupEvent <> 0 then
  begin
    CloseHandle(FWakeupEvent);
    FWakeupEvent := 0;
  end;
  FreeAndNil(FWndHandlePFS);
  FreeAndNil(FDataPFS);
  inherited;
end;

procedure TInstanceManage.Check;
begin
  if FStatus = isNotChecked then
  begin
    FWakeupEvent := CreateEventW(nil, False, False, PWideChar(FWakeupEventName));
    if FWakeupEvent = 0 then
        RaiseLastOSError();
    case GetLastError of
        ERROR_SUCCESS:
          FStatus := isFirst;
        ERROR_ALREADY_EXISTS:
          FStatus := isSecond;
    else
      RaiseLastOSError();
    end;
  end;
end;

function TInstanceManage.ActivateFirstInstance(
  const DataForFirstInstance: TBytes): Boolean;

  procedure PrepareData(const ShareName: UnicodeString;
    var PFS: TPageFileStream; const Data; DataSize: DWORD);
  begin
    FreeAndNil(PFS);
    PFS := TPageFileStream.Create(DataSize + SizeOf(DWORD), ShareName);
    if pfsValid in PFS.States then
    begin
      PFS.WriteDWord(DataSize);
      PFS.WriteBuffer(Data, DataSize);
    end;
  end;
var
  WndToActivate: HWND;
begin
  if Status = isNotChecked then
    Exit(False);
  WndToActivate := 0;
  with TPageFileStream.CreateForRead(FWndHandlePFSName) do
    try
      if pfsValid in States then
        ReadBuffer(WndToActivate, SizeOf(WndToActivate));
    finally
      Free;
    end;
  if Assigned(DataForFirstInstance) then
    PrepareData(FDataPFSName, FDataPFS, DataForFirstInstance[0],
        Length(DataForFirstInstance));
  if WndToActivate <> 0 then
  begin
    if Assigned(SwitchFunc) then
        SwitchFunc(WndToActivate, True);
  end;
  SetEvent(FWakeupEvent);
end;

function TInstanceManage.SetFormHandleForActivate(Handle: HWND): Boolean;
begin
  if FStatus = isNotChecked then
    Exit(False);
  FreeAndNil(FWndHandlePFS);
  FWndHandlePFS := TPageFileStream.Create(SizeOf(Handle), FWndHandlePFSName);
  Result := pfsValid in FWndHandlePFS.States;
  if Result then
    FWndHandlePFS.WriteBuffer(Handle, SizeOf(Handle));
end;

{ TPageFileStream }

procedure TPageFileStream.DoMap(DesiredAccess: DWORD);
var
  P: Pointer;
  Info: TMemoryBasicInformation;
begin
  P := MapViewOfFile(FMapHandle, DesiredAccess, 0, 0, 0);
  if Assigned(P) then
  begin
    if VirtualQuery(P, @Info, SizeOf(Info)) <> 0 then
    begin
      SetPointer(P, Info.RegionSize);
      Include(FStates, pfsValid);
    end;
  end;
end;

constructor TPageFileStream.Create(MapSize: SizeUInt;
  const MapName: UnicodeString);
begin
  inherited Create;
  FMapHandle := CreateFileMappingW(INVALID_HANDLE_VALUE, nil, PAGE_READWRITE,
    Hi(MapSize), Lo(MapSize), Pointer(MapName));
  if FMapHandle <> 0 then
  begin
    if GetLastError = ERROR_ALREADY_EXISTS then
        Include(FStates, pfsOpenExisting);
    DoMap(FILE_MAP_WRITE);
  end;
end;

constructor TPageFileStream.CreateForRead(const MapName: UnicodeString);
begin
  inherited Create;
  FMapHandle := OpenFileMappingW(FILE_MAP_READ, False, Pointer(MapName));
  if FMapHandle <> 0 then
  begin
    Include(FStates, pfsOpenExisting);
    DoMap(FILE_MAP_READ);
  end;
end;

destructor TPageFileStream.Destroy;
begin
  if Memory <> nil then
  begin
    UnmapViewOfFile(Memory);
    SetPointer(nil, 0);
  end;
  if FMapHandle <> 0 then
  begin
    CloseHandle(FMapHandle);
    FMapHandle := 0;
  end;
  inherited;
end;

function TPageFileStream.Write(const Buffer; Count: LongInt): LongInt;
var
  OldPos, NewPos: Int64;
begin
  Result := 0;
  OldPos := Position;
  if (OldPos >= 0) and (Count >= 0) then
  begin
    NewPos := OldPos + Count;
    if (NewPos > 0) and (NewPos < Size) then
    begin
      system.Move(Buffer, PByte(Memory)[OldPos], Count);
      Position := NewPos;
      Result := Count;
    end;
  end;
end;

function IsAnotherInstanceRunning:boolean;
var
  i: Integer;
  cli: String;
begin

  Result := False;

  if IsSetToOneInstance then
  begin
    FInstanceManage := TInstanceManage.Create(AppUniqueUID);
    FInstanceManage.Check;
    case FInstanceManage.Status of
      isSecond:
        begin
          cli := '';
          for i := 1 to ParamCount do
            cli := cli + ParamStrUTF8(i) + ParamsSeparator;
          FInstanceManage.ActivateFirstInstance(BytesOf(cli));
          Sleep(100);
          Result := True;
        end;
    end;
    FInstanceManage.Free;
  end;

end;

procedure debug(const text: String);
begin
  OutputDebugString(PChar('CudaText: ' + text));
end;

var
  hLib: HINST;

initialization
  hLib := LoadLibrary('user32.dll');
  Pointer(SwitchFunc) := GetProcAddress(hLib, 'SwitchToThisWindow');

finalization
  if hLib <> 0 then
     FreeLibrary(hLib);

{$else}
procedure DoFocusWindow(h: THandle);
begin
  LCLIntf.SetForegroundWindow(h);
end;
{$endif}

end.
