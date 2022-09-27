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
  LazUTF8,
  proc_globdata,
  proc_msg,
  proc_files,
  at__jsonconf,
  LazFileUtils,
  {$endif}
  SysUtils,
  LCLType,
  LCLIntf;

(*
----------------------------------------------------------------------
detailed comment by Andreas Heim about single-instance code on Windows,
https://github.com/Alexey-T/CudaText/issues/2519

-    When there is a running CudaText instance that is set to
single-instance-mode and a user wants to open a file by right-click -> Open
with CudaText, an additional instance of CudaText is started (in the following
I call it the "new instance").

-    The new instance checks the single-instance-mode setting (which is set to
TRUE in this case) and then checks for and detects the already running CudaText
instance.

-    Now the new instance performs a blocking wait until it becomes the owner
of a certain Windows mutex object. If there is no other CudaText process that
owns that mutex object rightnow, the blocking wait terminates immediately.

-    After that the new instance writes the name(s) of the file(s) to open to
shared memory (backed by the page file) and sets a Windows event object to the
signaled state. This informs the already running CudaText instance that there
is data in the shared memory for reading.

-    Now the new instance performs a blocking wait until the already running
CudaText instance sets another Windows event object to the signaled state to
inform that it has read the filename(s) from the shared memory and opened the
files.

-    The new CudaText instance now releases ownership of the mutex object and
terminates itself.

-    If there is another "new instance" (which happens when a user selects
multiple files in Windows Explorer and opens them by right-click -> Open with
CudaText) it becomes the owner of the mutex object. It terminates its blocking
wait for that and processes the last three steps from above.

If the already running CudaText instance crashes before it sets the second
Windows event object to the signaled state, the new instance waits forever.
That's the reason for the background CudaText process that is only visible in
Taskmanager. It is also the reason why it is impossible to start another
instance of CudaText, all these new instances wait for becoming the owner of
the mutex object that is still owned by the blocked CudaText process.
----------------------------------------------------------------------
*)

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
  TSecondInstanceSentDataEvent = procedure(const SentFromSecondInstance: TBytes) of object;
  TInstanceManage = class(TObject)
  strict private
    class var FInstance: TInstanceManage;
    class var FSingletonCreate: boolean;
    class constructor Create;
    class destructor Destroy;
  strict private
    FDataPFS: TPageFileStream;
    FDataPFSName: UnicodeString;
    FEventHandler: PEventHandler;
    FOnSecondInstanceSentData: TSecondInstanceSentDataEvent;
    FStatus: TInstanceStatus;
    FUniqueAppId: String;
    FSyncMutex: THandle;
    FSyncMutexName: UnicodeString;
    FDataSentEvent: THandle;
    FDataSentEventName: UnicodeString;
    FDataProcessedEvent: THandle;
    FDataProcessedEventName: UnicodeString;
    FWndHandlePFS: TPageFileStream;
    FWndHandlePFSName: UnicodeString;
    procedure ClearEventHandler;
    procedure SecondInstanceSentData(Unused1: PtrInt; Unused2: DWORD);
    procedure SetOnSecondInstanceSentData(const AValue: TSecondInstanceSentDataEvent);
  public
    constructor Create(const UniqueAppId: String);
    destructor Destroy; override;
    class function GetInstance: TInstanceManage;
    procedure Check;
    function ActivateFirstInstance(const DataForFirstInstance: TBytes = nil): Boolean;
    function SetFormHandleForActivate(Handle: HWND): Boolean;
    property OnSecondInstanceSentData: TSecondInstanceSentDataEvent read
        FOnSecondInstanceSentData write SetOnSecondInstanceSentData;
    property Status: TInstanceStatus read FStatus;
    property UniqueAppId: string read FUniqueAppId;
  end;

//Block another window instance if Single Instance is True
function IsAnotherInstanceRunning:boolean;
procedure debug(const text: String);
{$ifend}

{$ifndef windows}
procedure DoFocusWindow(h: HWND);
{$ifend}

implementation

{$ifdef windows}
const
  ParamsSeparator = '|';

type
  TSwitchFunc = procedure(h: HWND; fAltTab: BOOL); stdcall;

var
  SwitchFunc: TSwitchFunc = nil;

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
procedure TInstanceManage.SecondInstanceSentData(Unused1: PtrInt; Unused2: DWORD);
var
  Bytes: TBytes;
begin
  if (FStatus = isFirst) and Assigned(FOnSecondInstanceSentData) then
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

    // Process data read from other instance
    FOnSecondInstanceSentData(Bytes);

    // Signal other instance that data has been processed
    SetEvent(FDataProcessedEvent);
  end;
end;
{$POP}

procedure TInstanceManage.SetOnSecondInstanceSentData(const AValue: TSecondInstanceSentDataEvent);
begin
  if FStatus <> isNotChecked then
  begin
    FOnSecondInstanceSentData := AValue;
    ClearEventHandler;

    if Assigned(FOnSecondInstanceSentData) then
      FEventHandler := WidgetSet.AddEventHandler(FDataSentEvent, 0,
        @SecondInstanceSentData, 0);

    // First instance signals to be ready for receiving data.
    // This unblocks one of the subsequent instances that may wait for that.
    ReleaseMutex(FSyncMutex);
  end;
end;

constructor TInstanceManage.Create(const UniqueAppId: String);
begin
  inherited Create;

  if not FSingletonCreate then
    raise Exception.Create('Do not create instances of ' + Self.ClassName);

  FUniqueAppId := UniqueAppId;
  FDataPFSName := UTF8Decode(FUniqueAppId + '_MapData');
  FSyncMutexName := UTF8Decode(FUniqueAppId + '_SyncMutex');
  FDataSentEventName := UTF8Decode(FUniqueAppId + '_DataSentEvent');
  FDataProcessedEventName := UTF8Decode(FUniqueAppId + '_DataProcessedEvent');
  FWndHandlePFSName := UTF8Decode(FUniqueAppId + '_MapWnd');
end;

destructor TInstanceManage.Destroy;
begin
  ClearEventHandler;

  if FDataSentEvent <> 0 then
  begin
    CloseHandle(FDataSentEvent);
    FDataSentEvent := 0;
  end;

  if FDataProcessedEvent <> 0 then
  begin
    CloseHandle(FDataProcessedEvent);
    FDataProcessedEvent := 0;
  end;

  if FSyncMutex <> 0 then
  begin
    CloseHandle(FSyncMutex);
    FSyncMutex := 0;
  end;

  FreeAndNil(FWndHandlePFS);
  FreeAndNil(FDataPFS);

  inherited;
end;

class constructor TInstanceManage.Create;
begin
  FSingletonCreate := true;

  try
    FInstance := Create(AppUniqueUID);
  finally
    FSingletonCreate := false;
  end;
end;

class destructor TInstanceManage.Destroy;
begin
  FInstance.Free;
end;

class function TInstanceManage.GetInstance: TInstanceManage;
begin
  Result := FInstance;
end;

procedure TInstanceManage.Check;
begin
  if FStatus = isNotChecked then
  begin
    FSyncMutex := CreateMutexW(nil, false, PWideChar(FSyncMutexName));
    if FSyncMutex = 0 then RaiseLastOSError();

    FDataSentEvent := CreateEventW(nil, False, False, PWideChar(FDataSentEventName));
    if FDataSentEvent = 0 then RaiseLastOSError();

    FDataProcessedEvent := CreateEventW(nil, False, False, PWideChar(FDataProcessedEventName));
    if FDataProcessedEvent = 0 then RaiseLastOSError();

    case GetLastError of
      ERROR_SUCCESS:        FStatus := isFirst;
      ERROR_ALREADY_EXISTS: FStatus := isSecond;
      else                  RaiseLastOSError();
    end;

    // Wait for being allowed to send data.
    // The first instance doesn't block. Subsequent instances perform a
    // blocking wait. When the function returns the current instance is
    // owner of the mutex.
    WaitForSingleObject(FSyncMutex, INFINITE);
  end;
end;

function TInstanceManage.ActivateFirstInstance(const DataForFirstInstance: TBytes): Boolean;

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

  // Request first instance to read data
  SetEvent(FDataSentEvent);

  // Perform blocking wait until data has been processed
  WaitForSingleObject(FDataProcessedEvent, INFINITE);

  // Unblock other subsequent instance that may wait
  // for being allowed to send data
  ReleaseMutex(FSyncMutex);
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

function IsAnotherInstanceRunning: boolean;
var
  i: Integer;
  cli: String;
  workDir: String;
  param: String;
  InstanceManage: TInstanceManage;
  bAddDir: boolean;
begin
  Result := False;

  InstanceManage := TInstanceManage.GetInstance();
  InstanceManage.Check;

  case InstanceManage.Status of
    isSecond:
        begin
          cli := '';
          workDir := GetCurrentDirUTF8;

          if SameFileName(ExtractFileDir(ParamStrUTF8(0)), workDir) then
            workDir := '';

          for i := 1 to ParamCount do
          begin
            param := ParamStrUTF8(i);
            if param = '' then Continue;

            param:= AppExpandFilename(param);

            bAddDir :=
              (param[1] <> '-') and // issue #2578
              (workDir <> '') and
              not IsOsFullPath(param);

            if bAddDir then
              param := workDir + '\' + param;

            cli := cli + param + ParamsSeparator;
          end;

          InstanceManage.ActivateFirstInstance(BytesOf(cli));
          Result := True;
        end;
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
procedure DoFocusWindow(h: HWND);
begin
  LCLIntf.SetForegroundWindow(h);
end;
{$endif}

end.

