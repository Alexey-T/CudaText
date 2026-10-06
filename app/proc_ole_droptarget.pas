{
Copyright 2025 CudaText developers.
License: MPL 2.0 (same as CudaText).

Implements OLE drag&drop (IDropTarget) support for CudaText on Windows,
so that text/URLs can be dragged into the app from other applications
(browser address bar, clipboard managers, other editors, etc).
Fixes issue: https://github.com/Alexey-T/CudaText/issues/4894
Fixes issue: https://github.com/Alexey-T/CudaText/issues/6521

This is a compact, self-contained alternative to porting the whole
Melander's Drag&Drop Components library
(https://github.com/Baltimore99/delphi-drag-drop):
it uses the same Windows API (IDropTarget + RegisterDragDrop) with zero
dependencies, only FPC RTL units (Windows, ActiveX) are used.

How it works:
- TOleDropTargetManager.Attach() registers an OLE drop target
  (class TOleDropTarget, which implements IDropTarget) for the window
  handle of the given TWinControl (main form / floating form).
  OLE drop target of a top-level window automatically receives drops
  which happen over any child controls (editor, tabs, panels).
- Manager has an internal timer which periodically calls Attach() for
  all registered TWinControls. It self-heals all registrations when
  LCL creates/recreates a form's window handle later (e.g. floating
  group forms: handle is created/recreated on show/ShowInTaskBar
  changes, AFTER the initial Attach call - issue #4894 comment).
  The timer is paused while a drag is in progress: changing OLE
  registrations during an active drag can disturb the OLE drag loop.
- On drop, data is extracted from the COM IDataObject:
  * CF_HDROP - filenames, they are reported via OnDropFiles
    (same event format as LCL's OnDropFiles); drops are allowed
    anywhere on the attached window, like LCL's file drops;
  * CF_UNICODETEXT / CF_TEXT - text, it is reported via OnDropText;
    such drops are allowed only at positions accepted by the
    OnCanDropText event (e.g. only over the editor area, not over
    the ui-tabs area - issue #4894);
  * registered clipboard formats 'UniformResourceLocatorW' /
    'UniformResourceLocator' / 'text/x-moz-url' - URLs from browsers.
- Data objects of drag sources are handled defensively (issue #6521,
  drag from Firefox on win10 showed 'prohibited' cursor):
  * drop effects: copy is preferred, but link and move are accepted
    too (e.g. Firefox address-bar drags allow only 'link'); the drop
    handling always inserts the text, it never modifies the source;
  * format detection: IDataObject.QueryGetData is tried first (with
    both global memory and stream storage mediums); if it reports
    nothing, the format list is enumerated (IEnumFormatEtc); if that
    fails too, the data object is still accepted over the editor area,
    and the data is extracted on drop (QueryGetData/EnumFormatEtc of
    some sources lie or report formats too late);
  * if formats were not detected at DragEnter, detection is retried
    several times on DragOver: some sources populate their format
    list only after the drag has started;
  * text extraction: data is read from global memory first; if that
    fails, the same format is requested as IStream and read from the
    stream (some sources provide text only as a stream).
- All IDropTarget methods are protected from exceptions: an exception
  must never cross the COM boundary, it would break the whole drop.
- Debug log: every drag is written to %TEMP%\cudatext_oledrag.log
  (file is reset at app start). If drag&drop misbehaves with some
    app, this log shows exactly what was asked and answered.
- LCL on Windows initializes OLE itself (OleInitialize is called by the
  win32 widgetset), so no extra OLE initialization is done here.

Note: if the LCL recreates the window handle of an attached form
(rare case), call Attach() again to re-register the drop target.
}
unit proc_ole_droptarget;

{$mode objfpc}{$H+}
{$IFDEF MSWINDOWS}

interface

uses
  Windows, ActiveX, Classes, SysUtils, Contnrs, Controls, Forms,
  ExtCtrls;

const
  { not defined in FPC's ActiveX unit }
  DRAGDROP_E_ALREADYREGISTERED = HRESULT($80040101);

type
  { clipboard format id, alias of FPC's TCLIPFORMAT type from unit ActiveX }
  TClipFormat = ActiveX.TClipFormat;

  { Event fired when text (or URL) is dropped from an external app.
    AScreenPos is the mouse position (in screen coordinates) at drop. }
  TOleDropTextEvent = procedure(Sender: TObject;
    const AText: string; const AScreenPos: TPoint) of object;

  { Event fired to ask whether a drop of text (not files) is allowed
    at the given screen position. Fired on DragEnter/DragOver/Drop of
    IDropTarget, so the handler must be fast. If event is not assigned,
    text drops are allowed anywhere on the attached window. }
  TOleDropTextQueryEvent = function(Sender: TObject;
    const AScreenPos: TPoint): boolean of object;

  TOleDropTarget = class;

  { TOleDropTargetManager }

  TOleDropTargetManager = class(TComponent)
  private
    FTargets: TFPObjectList; // owns TOleDropTarget objects
    FTimer: TTimer;          // periodically re-registers all targets
    FWatchTimer: TTimer;     // diagnostics: watches mouse gestures (see unit comment)
    FDragCount: integer;     // >0 while an OLE drag is over one of our windows
    //state of the drag-watch diagnostic
    FWatchLMB: boolean;      // left mouse button is down
    FWatchDrag: boolean;     // button down + cursor moved beyond the drag threshold
    FWatchAnchor: TPoint;    // cursor position when the button was pressed
    FWatchOverUs: boolean;   // cursor is over one of our windows
    FWatchEverOverUs: boolean; // cursor was over our windows at least once
    FWatchOleActive: boolean;  // our IDropTarget got OLE events during the gesture
    FWatchStartTick: DWORD;  // GetTickCount when the button was pressed
    FWatchLastBeat: DWORD;   // tick of the last heartbeat log line
    FOnDropFiles: TDropFilesEvent;
    FOnDropText: TOleDropTextEvent;
    FOnCanDropText: TOleDropTextQueryEvent;
    function FindTarget(AWinControl: TWinControl): TOleDropTarget;
    function HandleRegistered(H: HWND): boolean;
    function WatchStateText: string;
    procedure OnReattachTimer(Sender: TObject);
    procedure OnWatchTimer(Sender: TObject);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Attach(AWinControl: TWinControl);
    procedure Detach(AWinControl: TWinControl);
    property OnDropFiles: TDropFilesEvent read FOnDropFiles write FOnDropFiles;
    property OnDropText: TOleDropTextEvent read FOnDropText write FOnDropText;
    property OnCanDropText: TOleDropTextQueryEvent read FOnCanDropText write FOnCanDropText;
  end;

  { TOleDropTarget - implements OLE IDropTarget for one window.
    Object is owned by TOleDropTargetManager (COM ref-count is manual,
    _Release never frees the object, so manager can safely free it). }

  TOleDropTarget = class(TObject, IUnknown, IDropTarget)
  private
    FManager: TOleDropTargetManager;
    FWinControl: TWinControl;
    FHandle: HWND;
    FRegistered: boolean;
    FRefCount: integer;
    FHasFiles: boolean;      // data object has CF_HDROP
    FHasText: boolean;       // data object has text/URL format
    FUnknownData: boolean;   // data object hides its formats: allow over editor, extract on drop
    FDataObj: IDataObject;   // data object of the current drag, for re-detection on DragOver
    FRedetectCount: integer; // how many times formats were re-detected on DragOver
    FLastOverLog: DWORD;     // last logged DragOver effect (log only changes)
    procedure SetDataFlags(const AData: IDataObject);
    function IsDropAllowedAt(const pt: TPoint): boolean;
    procedure FinishDrag;
  protected
    //IUnknown
    function QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} iid: TGuid; out obj): HRESULT; stdcall;
    function _AddRef: LongInt; stdcall;
    function _Release: LongInt; stdcall;
    //IDropTarget
    function DragEnter(const dataObj: IDataObject; grfKeyState: DWORD; pt: TPoint; var dwEffect: DWORD): HRESULT; stdcall;
    function DragOver(grfKeyState: DWORD; pt: TPoint; var dwEffect: DWORD): HRESULT; stdcall;
    function DragLeave: HRESULT; stdcall;
    function Drop(const dataObj: IDataObject; grfKeyState: DWORD; pt: TPoint; var dwEffect: DWORD): HRESULT; stdcall;
  public
    constructor Create(AManager: TOleDropTargetManager; AWinControl: TWinControl);
    destructor Destroy; override;
    procedure Attach;
    procedure Detach;
    property WinControl: TWinControl read FWinControl;
    { window handle currently registered in OLE (0 when not registered) }
    property RegisteredHandle: HWND read FHandle;
  end;

{ Global manager instance, created by InitOleDropSupport(). }
function OleDropSupport: TOleDropTargetManager;

{ Creates the global manager, must be called once from the main form. }
function InitOleDropSupport: TOleDropTargetManager;

{ Writes one line to the debug log file %TEMP%\cudatext_oledrag.log.
  Never raises, safe to call from COM callbacks and timers.
  Public: proc_ole_dragsource logs into the same file. }
procedure DbgLog(const S: string);

{ Readable name of a clipboard format id, for the log. }
function ClipboardFormatName(AFormat: TClipFormat): string;

{ HRESULT as readable text, for the log. }
function HResultText(H: HRESULT): string;

implementation

var
  ManagerIntf: TOleDropTargetManager = nil;

function OleDropSupport: TOleDropTargetManager;
begin
  Result:= ManagerIntf;
end;

function InitOleDropSupport: TOleDropTargetManager;
begin
  if ManagerIntf=nil then
    ManagerIntf:= TOleDropTargetManager.Create(nil);
  Result:= ManagerIntf;
end;

const
  //names of registered clipboard formats, which carry URLs
  cUrlFormatNameW = 'UniformResourceLocatorW';
  cUrlFormatNameA = 'UniformResourceLocator';
  cMozUrlFormatName = 'text/x-moz-url';

  //HRESULT values used in the debug log
  E_PENDING_        = HRESULT($8000000A);
  E_NOTIMPL_        = HRESULT($80004001);
  E_FAIL_           = HRESULT($80004005);
  E_UNEXPECTED_     = HRESULT($8000FFFF);
  DV_E_FORMATETC_   = HRESULT($80040064);
  DV_E_LINDEX_      = HRESULT($80040068);
  DV_E_TYMED_       = HRESULT($80040069);
  DV_E_DVASPECT_    = HRESULT($8004006A);
  DRAGDROP_S_DROP_  = HRESULT($00040100);
  DRAGDROP_S_CANCEL_= HRESULT($00040101);

type
  //structure of CF_HDROP data (parsed w/o ShellAPI's DragQueryFile)
  POleDropFiles = ^TOleDropFiles;
  TOleDropFiles = packed record
    pFiles: DWORD; //offset of file list
    pt: TPoint;    //drop point, client coords
    fNC: BOOL;
    fWide: BOOL;   //true: file list is WideChar
  end;

{ ---- debug log (issue #6521: diagnostics for drag&drop problems) ---- }

var
  LogFileInitialized: boolean = false;
  LogFileNameCache: string = '';

function GetLogFileName: string;
var
  Buf: array[0..600] of WideChar;
  W: UnicodeString;
  N: DWORD;
begin
  if LogFileNameCache='' then
  begin
    N:= Windows.GetTempPathW(600, @Buf[0]);
    if (N>0) and (N<600) then
    begin
      SetString(W, PWideChar(@Buf[0]), N);
      LogFileNameCache:= string(W)+'cudatext_oledrag.log';
    end
    else
      LogFileNameCache:= 'cudatext_oledrag.log';
  end;
  Result:= LogFileNameCache;
end;

{ Writes one line to the debug log, never raises. }
procedure DbgLog(const S: string);
var
  F: TextFile;
begin
  try
    if not LogFileInitialized then
    begin
      LogFileInitialized:= true;
      if FileExists(GetLogFileName) then
        SysUtils.DeleteFile(GetLogFileName);
    end;
    AssignFile(F, GetLogFileName);
    if FileExists(GetLogFileName) then
      Append(F)
    else
      Rewrite(F);
    try
      WriteLn(F, FormatDateTime('hh:nn:ss.zzz', Now), ' ', S);
    finally
      CloseFile(F);
    end;
  except
    //logging must never break drag&drop
  end;
end;

{ Class name of a window of this process, for the log. }
function GetWinClassName(H: HWND): string;
var
  Buf: array[0..100] of WideChar;
  W: UnicodeString;
  N: integer;
begin
  Result:= '';
  if H=0 then exit;
  FillChar(Buf, SizeOf(Buf), 0);
  N:= Windows.GetClassNameW(H, @Buf[0], 100);
  if N>0 then
  begin
    SetString(W, PWideChar(@Buf[0]), N);
    Result:= string(W);
  end;
end;

{ Windows version, via RtlGetVersion (unlike GetVersionEx it is not
  lied to by compatibility manifests). For the log. }
function WindowsVersionText: string;
type
  TRtlGetVersionFunc = function(var Info: TOSVersionInfoW): LONGBOOL; stdcall;
var
  Info: TOSVersionInfoW;
  F: TRtlGetVersionFunc;
  P: Pointer;
begin
  FillChar(Info, SizeOf(Info), 0);
  Info.dwOSVersionInfoSize:= SizeOf(Info);
  P:= GetProcAddress(GetModuleHandleW('ntdll.dll'), 'RtlGetVersion');
  if P<>nil then
  begin
    F:= TRtlGetVersionFunc(P);
    if F(Info) then
      exit('Windows '+IntToStr(Info.dwMajorVersion)+'.'+
        IntToStr(Info.dwMinorVersion)+' build '+IntToStr(Info.dwBuildNumber));
  end;
  if GetVersionExW(Info) then
    Result:= 'Windows '+IntToStr(Info.dwMajorVersion)+'.'+
      IntToStr(Info.dwMinorVersion)+' build '+IntToStr(Info.dwBuildNumber)
  else
    Result:= 'Windows (version unknown)';
end;

{ Is the process elevated (UAC)? This is important for drag&drop:
  Windows UIPI blocks OLE drag&drop from non-elevated apps (e.g.
  Firefox) into elevated apps - such drags never reach DragEnter. }
function ProcessElevationText: string;
var
  Token: THandle;
  Elev: DWORD; //TOKEN_ELEVATION.TokenIsElevated
  Len: DWORD;
begin
  Result:= 'unknown';
  if not OpenProcessToken(GetCurrentProcess, TOKEN_QUERY, Token) then
    exit('OpenProcessToken failed');
  try
    Elev:= 0;
    if GetTokenInformation(Token, TokenElevation, @Elev, SizeOf(Elev), @Len) then
    begin
      if Elev<>0 then
        Result:= 'YES (admin) - note: drags from non-elevated apps are blocked by Windows UIPI'
      else
        Result:= 'no';
    end;
  finally
    CloseHandle(Token);
  end;
end;

{ Exe file path of this process, for the log. }
function GetExeFileName: string;
var
  Buf: array[0..1000] of WideChar;
  W: UnicodeString;
  N: DWORD;
begin
  Result:= '';
  FillChar(Buf, SizeOf(Buf), 0);
  N:= GetModuleFileNameW(0, @Buf[0], 1000);
  if (N>0) and (N<1000) then
  begin
    SetString(W, PWideChar(@Buf[0]), N);
    Result:= string(W);
  end;
end;

procedure LogEnvironment;
begin
  DbgLog('env: '+WindowsVersionText);
  DbgLog('env: elevated='+ProcessElevationText+' pid='+
    IntToStr(GetCurrentProcessId)+' exe='+GetExeFileName);
end;

function HResultText(H: HRESULT): string;
begin
  case H of
    S_OK: Result:= 'S_OK';
    S_FALSE: Result:= 'S_FALSE';
    DRAGDROP_S_DROP_: Result:= 'DRAGDROP_S_DROP (dropped)';
    DRAGDROP_S_CANCEL_: Result:= 'DRAGDROP_S_CANCEL (Esc)';
    E_PENDING_: Result:= 'E_PENDING';
    E_NOTIMPL_: Result:= 'E_NOTIMPL';
    E_FAIL_: Result:= 'E_FAIL';
    E_UNEXPECTED_: Result:= 'E_UNEXPECTED';
    DV_E_FORMATETC_: Result:= 'DV_E_FORMATETC';
    DV_E_LINDEX_: Result:= 'DV_E_LINDEX';
    DV_E_TYMED_: Result:= 'DV_E_TYMED';
    DV_E_DVASPECT_: Result:= 'DV_E_DVASPECT';
  else
    Result:= IntToHex(H, 8);
  end;
end;

{ Readable name of a clipboard format id, for the log. }
function ClipboardFormatName(AFormat: TClipFormat): string;
var
  Buf: array[0..200] of AnsiChar;
  S: AnsiString;
  N: integer;
begin
  case AFormat of
    CF_TEXT: exit('CF_TEXT');
    CF_UNICODETEXT: exit('CF_UNICODETEXT');
    CF_HDROP: exit('CF_HDROP');
    CF_OEMTEXT: exit('CF_OEMTEXT');
    CF_BITMAP: exit('CF_BITMAP');
    CF_DIB: exit('CF_DIB');
  end;
  FillChar(Buf, SizeOf(Buf), 0);
  N:= Windows.GetClipboardFormatNameA(AFormat, PAnsiChar(@Buf[0]), 200);
  if N>0 then
  begin
    SetString(S, PAnsiChar(@Buf[0]), N);
    Result:= '"'+string(S)+'"';
  end
  else
    Result:= 'fmt_'+IntToStr(AFormat);
end;

{ helper routines }

function GetFormatEtc(AFormat: TClipFormat; ATymed: DWORD; out FE: TFormatEtc): boolean;
begin
  Result:= AFormat<>0;
  if not Result then exit;
  FillChar(FE, SizeOf(FE), 0);
  FE.cfFormat:= AFormat;
  FE.dwAspect:= DVASPECT_CONTENT;
  FE.lindex:= -1;
  FE.tymed:= ATymed;
end;

{ Queries the data object for a format with one storage medium. }
function QueryHasFormat(const AData: IDataObject; AFormat: TClipFormat;
  ATymed: DWORD; out HR: HRESULT): boolean;
var
  FE: TFormatEtc;
begin
  Result:= false;
  HR:= E_FAIL_;
  if AData=nil then exit;
  if not GetFormatEtc(AFormat, ATymed, FE) then exit;
  HR:= AData.QueryGetData(FE);
  Result:= HR=S_OK;
end;

{ Checks the format via QueryGetData, global memory first, then stream.
  Every answer is written to the debug log. }
function DataHasFormatLog(const AData: IDataObject; AFormat: TClipFormat): boolean;
var
  HR: HRESULT;
begin
  Result:= QueryHasFormat(AData, AFormat, TYMED_HGLOBAL, HR);
  DbgLog('  QueryGetData('+ClipboardFormatName(AFormat)+', hglobal)='+HResultText(HR));
  if not Result then
  begin
    Result:= QueryHasFormat(AData, AFormat, TYMED_ISTREAM, HR);
    DbgLog('  QueryGetData('+ClipboardFormatName(AFormat)+', istream)='+HResultText(HR));
  end;
end;

{ Checks the URL formats via QueryGetData, with logging. }
function DataHasUrlFormatsLog(const AData: IDataObject): boolean;
var
  Id: TClipFormat;
begin
  Id:= RegisterClipboardFormat(cUrlFormatNameW);
  if DataHasFormatLog(AData, Id) then exit(true);
  Id:= RegisterClipboardFormat(cMozUrlFormatName);
  if DataHasFormatLog(AData, Id) then exit(true);
  Id:= RegisterClipboardFormat(cUrlFormatNameA);
  if DataHasFormatLog(AData, Id) then exit(true);
  Result:= false;
end;

{ Checks the format by enumerating the data object's format list
  (fallback for data objects which don't answer QueryGetData
  correctly - matching is by clipboard format id, the storage
  medium advertised by the source is ignored). }
function DataObjectHasFormatId(const AData: IDataObject; AFormat: TClipFormat): boolean;
var
  Enum: IEnumFORMATETC;
  FE: TFormatEtc;
  N: ULong;
  i: integer;
begin
  Result:= false;
  if AData=nil then exit;
  if AFormat=0 then exit;
  if AData.EnumFormatEtc(DATADIR_GET, Enum)<>S_OK then exit;
  if Enum=nil then exit;
  Enum.Reset;
  for i:= 1 to 1000 do //defensive cap
  begin
    N:= 0;
    if Enum.Next(1, FE, @N)<>S_OK then exit;
    if N=0 then exit; //defensive: no item written
    if FE.cfFormat=AFormat then
      exit(true);
  end;
end;

{ Enumerates all formats of the data object and writes them to the log. }
procedure LogDataFormats(const AData: IDataObject);
var
  Enum: IEnumFORMATETC;
  FE: TFormatEtc;
  N: ULong;
  i: integer;
  S: string;
begin
  if AData=nil then exit;
  if AData.EnumFormatEtc(DATADIR_GET, Enum)<>S_OK then
  begin
    DbgLog('  EnumFormatEtc failed');
    exit;
  end;
  if Enum=nil then exit;
  Enum.Reset;
  S:= '';
  for i:= 1 to 100 do
  begin
    N:= 0;
    if Enum.Next(1, FE, @N)<>S_OK then break;
    if N=0 then break;
    if S<>'' then S:= S+', ';
    S:= S+ClipboardFormatName(FE.cfFormat);
  end;
  DbgLog('  formats: ['+S+']');
end;

{ Gets HGLOBAL of a format from IDataObject.
  Returns ownership of the storage medium to the caller:
  call ReleaseStgMediumForHGlobal() after using the data. }
function GetDataHGlobal(const AData: IDataObject; AFormat: TClipFormat;
  out AGlobal: HGLOBAL): boolean;
var
  FE: TFormatEtc;
  SM: TStgMedium;
begin
  Result:= false;
  AGlobal:= 0;
  if not GetFormatEtc(AFormat, TYMED_HGLOBAL, FE) then exit;

  FillChar(SM, SizeOf(SM), 0);
  if AData.GetData(FE, SM)<>S_OK then exit;

  if SM.tymed=TYMED_HGLOBAL then
  begin
    AGlobal:= SM.hGlobal;
    Result:= true;
  end
  else
    ReleaseStgMedium(SM);
end;

procedure ReleaseStgMediumForHGlobal(AGlobal: HGLOBAL);
var
  SM: TStgMedium;
begin
  if AGlobal=0 then exit;
  FillChar(SM, SizeOf(SM), 0);
  SM.tymed:= TYMED_HGLOBAL;
  SM.hGlobal:= AGlobal;
  ReleaseStgMedium(SM);
end;

{ Reads zero-terminated string list from CF_HDROP global memory. }
function GetDroppedFiles(const AData: IDataObject; AList: TStrings): boolean;
var
  Global: HGLOBAL;
  Ptr: PByte;
  Size: DWORD;
  Data: POleDropFiles;
  Uni: UnicodeString;
  Ans: AnsiString;
  P: PAnsiChar;
  PW: PWideChar;
begin
  Result:= false;
  AList.Clear;

  if not GetDataHGlobal(AData, CF_HDROP, Global) then exit;
  try
    Ptr:= GlobalLock(Global);
    if Ptr=nil then exit;
    try
      Size:= GlobalSize(Global);
      if Size<SizeOf(TOleDropFiles) then exit;
      Data:= POleDropFiles(Ptr);
      if Data^.pFiles>=Size then exit;

      if Data^.fWide then
      begin
        PW:= Pointer(Ptr+Data^.pFiles);
        while PW^<>#0 do
        begin
          Uni:= PW;
          AList.Add(string(Uni));
          inc(PW, Length(Uni)+1);
        end;
      end
      else
      begin
        P:= Pointer(Ptr+Data^.pFiles);
        while P^<>#0 do
        begin
          Ans:= P;
          AList.Add(string(Ans));
          inc(P, Length(Ans)+1);
        end;
      end;
      Result:= AList.Count>0;
    finally
      GlobalUnlock(Global);
    end;
  finally
    ReleaseStgMediumForHGlobal(Global);
  end;
end;

{ Extracts zero-terminated text (wide or ansi) from HGLOBAL memory. }
function TextFromGlobal(AGlobal: HGLOBAL; AWide: boolean): string;
var
  Ptr: Pointer;
  Size: DWORD;
  N: DWORD;
  Uni: UnicodeString;
  Ans: AnsiString;
begin
  Result:= '';
  Ptr:= GlobalLock(AGlobal);
  if Ptr=nil then exit;
  try
    Size:= GlobalSize(AGlobal);
    if AWide then
    begin
      N:= Size div SizeOf(WideChar);
      if N<=0 then exit;
      SetLength(Uni, N);
      Move(Ptr^, PWideChar(Uni)^, N*SizeOf(WideChar));
      if Pos(WideChar(0), Uni)>0 then
        SetLength(Uni, Pos(WideChar(0), Uni)-1);
      Result:= string(Uni);
    end
    else
    begin
      if Size=0 then exit;
      SetLength(Ans, Size);
      Move(Ptr^, PAnsiChar(Ans)^, Size);
      if Pos(#0, Ans)>0 then
        SetLength(Ans, Pos(#0, Ans)-1);
      Result:= string(Ans);
    end;
  finally
    GlobalUnlock(AGlobal);
  end;
end;

{ Extracts zero-terminated text (wide or ansi) from a raw byte buffer. }
function TextFromBuffer(ABuf: AnsiString; AWide: boolean): string;
var
  Uni: UnicodeString;
  i: integer;
begin
  Result:= '';
  if ABuf='' then exit;
  if AWide then
  begin
    SetLength(Uni, Length(ABuf) div SizeOf(WideChar));
    if Uni<>'' then
      Move(ABuf[1], PWideChar(Uni)^, Length(Uni)*SizeOf(WideChar));
    i:= Pos(WideChar(0), Uni);
    if i>0 then
      SetLength(Uni, i-1);
    Result:= string(Uni);
  end
  else
  begin
    i:= Pos(AnsiChar(0), ABuf);
    if i>0 then
      SetLength(ABuf, i-1);
    Result:= string(ABuf);
  end;
end;

{ Reads the whole IStream content into a raw byte buffer. }
function ReadStreamBytes(const AStream: IStream; out ABuf: AnsiString): boolean;
const
  cChunk = 8192;
  cMaxSize = 64*1024*1024;
var
  Chunk: array[0..cChunk-1] of Byte;
  N: DWORD;
  Total: integer;
begin
  Result:= false;
  ABuf:= '';
  if AStream=nil then exit;

  Total:= 0;
  repeat
    N:= 0;
    if AStream.Read(@Chunk[0], cChunk, @N)<>S_OK then exit;
    if N=0 then break;
    SetLength(ABuf, Total+integer(N));
    Move(Chunk[0], ABuf[Total+1], N);
    inc(Total, N);
  until Total>=cMaxSize;

  Result:= Total>0;
end;

{ Extracts the string of a text format from the data object.
  First the data is requested as global memory; if that fails, the
  same format is requested as IStream and read from the stream
  (some data objects provide their text only as a stream).
  Every answer is written to the debug log. }
function GetDataString(const AData: IDataObject; AFormat: TClipFormat;
  AWide: boolean): string;
var
  FE: TFormatEtc;
  SM: TStgMedium;
  Stream: IStream;
  Raw: AnsiString;
begin
  Result:= '';
  if (AData=nil) or (AFormat=0) then exit;

  //1) global memory
  if not GetFormatEtc(AFormat, TYMED_HGLOBAL, FE) then exit;
  FillChar(SM, SizeOf(SM), 0);
  if AData.GetData(FE, SM)=S_OK then
  begin
    try
      if (SM.tymed=TYMED_HGLOBAL) and (SM.hGlobal<>0) then
        Result:= TextFromGlobal(SM.hGlobal, AWide);
    finally
      ReleaseStgMedium(SM);
    end;
    DbgLog('  GetData('+ClipboardFormatName(AFormat)+', hglobal): '+
      IntToStr(Length(Result))+' chars');
    if Result<>'' then exit;
  end
  else
    DbgLog('  GetData('+ClipboardFormatName(AFormat)+', hglobal) failed');

  //2) stream
  if not GetFormatEtc(AFormat, TYMED_ISTREAM, FE) then exit;
  FillChar(SM, SizeOf(SM), 0);
  if AData.GetData(FE, SM)=S_OK then
  begin
    try
      if (SM.tymed=TYMED_ISTREAM) and (SM.pstm<>nil) then
      begin
        Stream:= IStream(SM.pstm);
        try
          if ReadStreamBytes(Stream, Raw) then
            Result:= TextFromBuffer(Raw, AWide);
        finally
          //release our interface reference before the storage
          //medium is released (the medium owns its own reference)
          Stream:= nil;
        end;
      end;
    finally
      ReleaseStgMedium(SM);
    end;
    DbgLog('  GetData('+ClipboardFormatName(AFormat)+', istream): '+
      IntToStr(Length(Result))+' chars');
  end
  else
    DbgLog('  GetData('+ClipboardFormatName(AFormat)+', istream) failed');
end;

{ Extracts dropped text: URL formats have priority,
  then CF_UNICODETEXT, then CF_TEXT.
  Data is requested directly, without QueryGetData first:
  some data objects report 'no' but still give the data. }
function GetDroppedText(const AData: IDataObject; out AText: string): boolean;
var
  Id: TClipFormat;
  S: string;
  i: integer;
begin
  Result:= false;
  AText:= '';

  S:= GetDataString(AData, RegisterClipboardFormat(cUrlFormatNameW), true);
  if S<>'' then
  begin
    AText:= S;
    exit(true);
  end;

  //content of 'text/x-moz-url' is "url\ntitle"
  Id:= RegisterClipboardFormat(cMozUrlFormatName);
  S:= GetDataString(AData, Id, true);
  if S<>'' then
  begin
    i:= Pos(#10, S);
    if i>0 then
      S:= Copy(S, 1, i-1);
    S:= TrimRight(S);
    if S<>'' then
    begin
      AText:= S;
      exit(true);
    end;
  end;

  S:= GetDataString(AData, RegisterClipboardFormat(cUrlFormatNameA), false);
  if S<>'' then
  begin
    AText:= S;
    exit(true);
  end;

  S:= GetDataString(AData, CF_UNICODETEXT, true);
  if S<>'' then
  begin
    AText:= S;
    exit(true);
  end;

  S:= GetDataString(AData, CF_TEXT, false);
  if S<>'' then
  begin
    AText:= S;
    exit(true);
  end;

  Result:= false;
end;

{ Chooses the drop effect to report to the drag source.
  Copy is preferred, but link and move are accepted too: some sources
  (e.g. Firefox address-bar drags) allow only the 'link' effect.
  The drop handling always inserts the text, it never modifies the
  source, so which effect is reported only matters for the cursor
  image and for the drag source's final report. }
function SelectDropEffect(const dwEffect: DWORD): DWORD;
var
  Allowed: DWORD;
begin
  Allowed:= dwEffect and (DROPEFFECT_COPY or DROPEFFECT_LINK or DROPEFFECT_MOVE);
  if (Allowed and DROPEFFECT_COPY)<>0 then
    exit(DROPEFFECT_COPY);
  if (Allowed and DROPEFFECT_LINK)<>0 then
    exit(DROPEFFECT_LINK);
  if (Allowed and DROPEFFECT_MOVE)<>0 then
    exit(DROPEFFECT_MOVE);
  //source reported no usual effect (only e.g. DROPEFFECT_SCROLL):
  //report copy like other editors do, OLE masks it itself
  Result:= DROPEFFECT_COPY;
end;

{ TOleDropTargetManager }

constructor TOleDropTargetManager.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTargets:= TFPObjectList.Create(true);
  //timer re-attaches all drop targets: this heals the registrations
  //when a form's window handle is created/recreated after Attach()
  //was called (e.g. floating group forms, which get the handle on
  //Show, and ShowInTaskBar changes recreate the handle)
  FTimer:= TTimer.Create(nil);
  FTimer.Interval:= 500;
  FTimer.OnTimer:= @OnReattachTimer;
  FTimer.Enabled:= true;
  //diagnostics: watches mouse gestures; the log line 'cursor over our
  //window' with no DragEnter lines proves that OLE does not deliver
  //the drag to us (issue #6521 debugging)
  FWatchTimer:= TTimer.Create(nil);
  FWatchTimer.Interval:= 150;
  FWatchTimer.OnTimer:= @OnWatchTimer;
  FWatchTimer.Enabled:= true;
  DbgLog('OLE drop support initialized');
  LogEnvironment;
end;

destructor TOleDropTargetManager.Destroy;
begin
  if FWatchTimer<>nil then
  begin
    FWatchTimer.Enabled:= false;
    FreeAndNil(FWatchTimer);
  end;
  if FTimer<>nil then
  begin
    FTimer.Enabled:= false;
    FreeAndNil(FTimer);
  end;
  //revoke all registrations, free all targets
  while FTargets.Count>0 do
    FTargets.Delete(0);
  FreeAndNil(FTargets);
  if ManagerIntf=Self then
    ManagerIntf:= nil;
  inherited Destroy;
end;

procedure TOleDropTargetManager.Notification(AComponent: TComponent;
  Operation: TOperation);
var
  Target: TOleDropTarget;
begin
  inherited Notification(AComponent, Operation);
  //attached TWinControl is being destroyed: forget its drop target,
  //so the re-attach timer never touches a destroyed control
  if (Operation=opRemove) and (AComponent is TWinControl) then
  begin
    Target:= FindTarget(TWinControl(AComponent));
    if Target<>nil then
      FTargets.Remove(Target);
  end;
end;

procedure TOleDropTargetManager.OnReattachTimer(Sender: TObject);
var
  i: integer;
begin
  //don't touch OLE registrations while a drag is in progress:
  //revoking/registering drop targets during an active drag can
  //disturb the OLE drag loop
  if FDragCount>0 then exit;
  //Attach() is idempotent and cheap: it exits early when a window
  //handle is not allocated yet, or when it did not change
  for i:= 0 to FTargets.Count-1 do
    TOleDropTarget(FTargets[i]).Attach;
end;

function TOleDropTargetManager.HandleRegistered(H: HWND): boolean;
var
  i: integer;
begin
  Result:= false;
  if H=0 then exit;
  for i:= 0 to FTargets.Count-1 do
    if TOleDropTarget(FTargets[i]).RegisteredHandle=H then
      exit(true);
end;

function TOleDropTargetManager.WatchStateText: string;
begin
  if FWatchOleActive or (FDragCount>0) then
    exit('OLE drag is ACTIVE');
  if (DragManager<>nil) and DragManager.IsDragging then
    exit('internal CudaText drag, no OLE events is normal here');
  Result:= 'NO OLE events (external drag does not reach CudaText!)';
end;

procedure TOleDropTargetManager.OnWatchTimer(Sender: TObject);
var
  P: TPoint;
  Down, OverUs: boolean;
  H: HWND;
  Tick: DWORD;
begin
  try
    Down:= (GetAsyncKeyState(VK_LBUTTON) and $8000)<>0;
    if not Down then
    begin
      if FWatchDrag and FWatchEverOverUs then
        DbgLog('watch: gesture finished (button up) after '+
          IntToStr(GetTickCount-FWatchStartTick)+' ms');
      FWatchLMB:= false;
      FWatchDrag:= false;
      FWatchOverUs:= false;
      FWatchEverOverUs:= false;
      FWatchOleActive:= false;
      exit;
    end;
    if not GetCursorPos(P) then exit;

    if not FWatchLMB then
    begin
      //button just pressed
      FWatchLMB:= true;
      FWatchDrag:= false;
      FWatchAnchor:= P;
      FWatchOverUs:= false;
      FWatchEverOverUs:= false;
      FWatchOleActive:= false;
      FWatchStartTick:= GetTickCount;
      FWatchLastBeat:= 0;
      exit;
    end;

    if not FWatchDrag then
    begin
      //distinguish a real drag from a simple click: some cursor
      //movement beyond the system drag threshold is required
      if (Abs(P.x-FWatchAnchor.x)>GetSystemMetrics(SM_CXDRAG)) or
         (Abs(P.y-FWatchAnchor.y)>GetSystemMetrics(SM_CYDRAG)) then
        FWatchDrag:= true
      else
        exit;
    end;

    //is the cursor over one of our registered windows (or its children)?
    OverUs:= false;
    H:= WindowFromPoint(P);
    while (H<>0) and (not OverUs) do
    begin
      OverUs:= HandleRegistered(H);
      if not OverUs then
        H:= GetParent(H);
    end;

    if OverUs and (not FWatchOverUs) then
    begin
      FWatchOverUs:= true;
      FWatchEverOverUs:= true;
      FWatchLastBeat:= GetTickCount;
      DbgLog('watch: cursor over our window: '+WatchStateText);
    end;
    if (not OverUs) and FWatchOverUs then
    begin
      FWatchOverUs:= false;
      DbgLog('watch: cursor left our window');
    end;

    //heartbeat: an external drag stays over us without any OLE events
    if FWatchOverUs and (not FWatchOleActive) and (FDragCount=0) then
    begin
      Tick:= GetTickCount;
      if (FWatchLastBeat=0) or (Tick-FWatchLastBeat>1000) then
      begin
        FWatchLastBeat:= Tick;
        DbgLog('watch: still over our window after '+
          IntToStr(Tick-FWatchStartTick)+' ms, no OLE events');
      end;
    end;
  except
    //diagnostics must never break anything
  end;
end;

function TOleDropTargetManager.FindTarget(AWinControl: TWinControl): TOleDropTarget;
var
  i: integer;
begin
  Result:= nil;
  for i:= 0 to FTargets.Count-1 do
    if TOleDropTarget(FTargets[i]).WinControl=AWinControl then
      exit(TOleDropTarget(FTargets[i]));
end;

procedure TOleDropTargetManager.Attach(AWinControl: TWinControl);
var
  Target: TOleDropTarget;
begin
  if AWinControl=nil then exit;
  Target:= FindTarget(AWinControl);
  if Target=nil then
  begin
    Target:= TOleDropTarget.Create(Self, AWinControl);
    FTargets.Add(Target);
    //get notified when the control is destroyed
    AWinControl.FreeNotification(Self);
  end;
  Target.Attach;
end;

procedure TOleDropTargetManager.Detach(AWinControl: TWinControl);
var
  Target: TOleDropTarget;
begin
  if AWinControl=nil then exit;
  Target:= FindTarget(AWinControl);
  if Target=nil then exit;
  AWinControl.RemoveFreeNotification(Self);
  FTargets.Remove(Target);
end;

{ TOleDropTarget }

constructor TOleDropTarget.Create(AManager: TOleDropTargetManager;
  AWinControl: TWinControl);
begin
  inherited Create;
  FManager:= AManager;
  FWinControl:= AWinControl;
end;

destructor TOleDropTarget.Destroy;
begin
  Detach;
  inherited;
end;

procedure TOleDropTarget.Attach;
var
  H: HRESULT;
  Intf: IDropTarget;
begin
  if FWinControl=nil then exit;
  if not FWinControl.HandleAllocated then exit;

  //already registered for the current handle?
  if FRegistered and (FHandle=FWinControl.Handle) then exit;

  //handle was recreated: revoke old registration
  if FRegistered then
    Detach;

  FHandle:= FWinControl.Handle;
  Intf:= IDropTarget(Self);
  H:= RegisterDragDrop(FHandle, Intf);
  FRegistered:= (H=S_OK) or (H=DRAGDROP_E_ALREADYREGISTERED);
  DbgLog('RegisterDragDrop hwnd='+IntToHex(Int64(FHandle), 16)+
    ' class="'+GetWinClassName(FHandle)+'"'+
    ' = '+HResultText(H));
  //drop reference added by the interface cast: it is owned by COM now,
  //but the object is freed by the manager anyway
  Intf:= nil;
end;

procedure TOleDropTarget.Detach;
begin
  if FRegistered then
  begin
    DbgLog('RevokeDragDrop hwnd='+IntToHex(Int64(FHandle), 16));
    RevokeDragDrop(FHandle);
    FRegistered:= false;
  end;
end;

function TOleDropTarget.QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} iid: TGuid; out obj): HRESULT; stdcall;
begin
  if GetInterface(iid, obj) then
    Result:= S_OK
  else
  begin
    //COM requires: on failure, out param must be set to nil
    Pointer(obj):= nil;
    Result:= E_NOINTERFACE;
  end;
end;

function TOleDropTarget._AddRef: LongInt; stdcall;
begin
  inc(FRefCount);
  Result:= FRefCount;
end;

function TOleDropTarget._Release: LongInt; stdcall;
begin
  //object lifetime is managed by TOleDropTargetManager
  Result:= FRefCount;
end;

procedure TOleDropTarget.SetDataFlags(const AData: IDataObject);
var
  IdW, IdA, IdMoz: TClipFormat;
begin
  FHasFiles:= false;
  FHasText:= false;
  FUnknownData:= false;
  if AData=nil then
  begin
    DbgLog('  data object is nil');
    exit;
  end;

  //fast way: QueryGetData (works for most apps)
  FHasFiles:= DataHasFormatLog(AData, CF_HDROP);
  FHasText:=
    (not FHasFiles) and
    (DataHasFormatLog(AData, CF_UNICODETEXT) or
     DataHasFormatLog(AData, CF_TEXT) or
     DataHasUrlFormatsLog(AData));

  //fallback: some data objects (e.g. modern Firefox builds on win10,
  //issue #6521) don't answer QueryGetData correctly, but enumerate
  //their formats fine
  if (not FHasFiles) and (not FHasText) then
  begin
    DbgLog('  query says no: checking the format list');
    LogDataFormats(AData);
    IdW:= RegisterClipboardFormat(cUrlFormatNameW);
    IdA:= RegisterClipboardFormat(cUrlFormatNameA);
    IdMoz:= RegisterClipboardFormat(cMozUrlFormatName);
    FHasFiles:= DataObjectHasFormatId(AData, CF_HDROP);
    FHasText:=
      (not FHasFiles) and
      (DataObjectHasFormatId(AData, CF_UNICODETEXT) or
       DataObjectHasFormatId(AData, CF_TEXT) or
       DataObjectHasFormatId(AData, IdW) or
       DataObjectHasFormatId(AData, IdA) or
       DataObjectHasFormatId(AData, IdMoz));
  end;

  //last resort: the data object hides its formats from both
  //QueryGetData and EnumFormatEtc - allow it over the editor area
  //anyway (if OnCanDropText accepts the position) and try the full
  //extraction on Drop; some data objects say 'no' but give the data
  if (not FHasFiles) and (not FHasText) then
  begin
    DbgLog('  formats not detected: allowed as unknown data');
    FUnknownData:= true;
  end;
end;

{ Is the current data object droppable at the given screen position?
  Files are droppable anywhere on the attached window (like LCL's
  OnDropFiles). Text/URL (and unknown data, which is handled like
  text) are droppable only at positions accepted by the manager's
  OnCanDropText event. }
function TOleDropTarget.IsDropAllowedAt(const pt: TPoint): boolean;
begin
  if FHasFiles then
    exit(true);
  if not (FHasText or FUnknownData) then
    exit(false);
  if (FManager=nil) or (not Assigned(FManager.FOnCanDropText)) then
    exit(true);
  Result:= FManager.FOnCanDropText(FWinControl, pt);
end;

{ Resets the drag state, must be called when the drag is over
  (DragLeave or Drop). }
procedure TOleDropTarget.FinishDrag;
begin
  FHasFiles:= false;
  FHasText:= false;
  FUnknownData:= false;
  FDataObj:= nil;
  if (FManager<>nil) and (FManager.FDragCount>0) then
    dec(FManager.FDragCount);
end;

function TOleDropTarget.DragEnter(const dataObj: IDataObject;
  grfKeyState: DWORD; pt: TPoint; var dwEffect: DWORD): HRESULT; stdcall;
begin
  Result:= S_OK;
  //defensive: if a previous drag was not properly finished
  //(unpaired DragEnter), reset its state first, so the drag
  //counter of the manager is never leaked
  if FDataObj<>nil then
    FinishDrag;
  //pause the re-attach timer for the whole drag: OLE registration
  //changes during an active drag can disturb the OLE drag loop
  if FManager<>nil then
  begin
    inc(FManager.FDragCount);
    //tell the drag-watch diagnostic that OLE events are delivered
    FManager.FWatchOleActive:= true;
  end;
  //keep the data object: formats may need to be re-checked on DragOver
  FDataObj:= dataObj;
  FRedetectCount:= 0;
  FLastOverLog:= $FFFFFFFF;

  DbgLog('DragEnter hwnd='+IntToHex(Int64(FHandle), 16)+
    ' pt='+IntToStr(pt.x)+','+IntToStr(pt.y)+
    ' effects_in='+IntToHex(dwEffect, 8)+
    ' keys='+IntToHex(grfKeyState, 8));
  try
    try
      SetDataFlags(dataObj);

      if IsDropAllowedAt(pt) then
      begin
        dwEffect:= SelectDropEffect(dwEffect);
        DbgLog('DragEnter: allowed, effect='+IntToHex(dwEffect, 8));
      end
      else
      begin
        dwEffect:= DROPEFFECT_NONE;
        DbgLog('DragEnter: position not allowed');
      end;
    except
      on E: Exception do
      begin
        DbgLog('DragEnter exception: '+E.Message);
        dwEffect:= DROPEFFECT_NONE;
      end;
    end;
  except
    //never let an exception cross the COM boundary
  end;
end;

function TOleDropTarget.DragOver(grfKeyState: DWORD; pt: TPoint;
  var dwEffect: DWORD): HRESULT; stdcall;
begin
  Result:= S_OK;
  try
    try
      //if formats were not detected at DragEnter, retry a few times:
      //some sources (e.g. Firefox) populate their format list late
      if FUnknownData and (FDataObj<>nil) and (FRedetectCount<3) then
      begin
        inc(FRedetectCount);
        DbgLog('DragOver: re-detecting formats, try '+IntToStr(FRedetectCount));
        SetDataFlags(FDataObj);
      end;

      if IsDropAllowedAt(pt) then
        dwEffect:= SelectDropEffect(dwEffect)
      else
        dwEffect:= DROPEFFECT_NONE;

      //DragOver fires very often: log only when the effect changes
      if dwEffect<>FLastOverLog then
      begin
        FLastOverLog:= dwEffect;
        DbgLog('DragOver: effect='+IntToHex(dwEffect, 8)+
          ' pt='+IntToStr(pt.x)+','+IntToStr(pt.y));
      end;
    except
      on E: Exception do
      begin
        DbgLog('DragOver exception: '+E.Message);
        dwEffect:= DROPEFFECT_NONE;
      end;
    end;
  except
    //never let an exception cross the COM boundary
  end;
end;

function TOleDropTarget.DragLeave: HRESULT; stdcall;
begin
  Result:= S_OK;
  try
    try
      DbgLog('DragLeave');
    except
    end;
  except
    //never let an exception cross the COM boundary
  end;
  FinishDrag;
end;

function TOleDropTarget.Drop(const dataObj: IDataObject; grfKeyState: DWORD;
  pt: TPoint; var dwEffect: DWORD): HRESULT; stdcall;
var
  Files: TStringList;
  FileArr: array of string;
  S: string;
  i: integer;
begin
  Result:= S_OK;
  dwEffect:= DROPEFFECT_NONE;

  if dataObj=nil then
  begin
    DbgLog('Drop: data object is nil');
    FinishDrag;
    exit;
  end;
  if FManager=nil then
  begin
    FinishDrag;
    exit;
  end;

  DbgLog('Drop: pt='+IntToStr(pt.x)+','+IntToStr(pt.y)+
    ' effects_in='+IntToHex(dwEffect, 8)+
    ' keys='+IntToHex(grfKeyState, 8));
  try
    try
      SetDataFlags(dataObj);

      //files have priority: dropped file(s) from Explorer open like before;
      //for unknown data objects CF_HDROP is tried here too, then text
      if FHasFiles or FUnknownData then
      begin
        Files:= TStringList.Create;
        try
          if GetDroppedFiles(dataObj, Files) then
          begin
            dwEffect:= SelectDropEffect(dwEffect);
            if Assigned(FManager.FOnDropFiles) then
            begin
              SetLength(FileArr, Files.Count);
              for i:= 0 to Files.Count-1 do
                FileArr[i]:= Files[i];
              FManager.FOnDropFiles(FWinControl, FileArr);
            end;
            DbgLog('Drop: '+IntToStr(Files.Count)+' file(s)');
            exit;
          end;
        finally
          FreeAndNil(Files);
        end;
      end;

      //text or URL: only at the position accepted by OnCanDropText
      //(e.g. only over the editor area, not over the ui-tabs)
      if IsDropAllowedAt(pt) then
        if GetDroppedText(dataObj, S) then
          if S<>'' then
          begin
            dwEffect:= SelectDropEffect(dwEffect);
            if Assigned(FManager.FOnDropText) then
              FManager.FOnDropText(FWinControl, S, pt);
            DbgLog('Drop: text, '+IntToStr(Length(S))+' chars');
          end;

      if dwEffect=DROPEFFECT_NONE then
        DbgLog('Drop: no data extracted');
    except
      on E: Exception do
      begin
        DbgLog('Drop exception: '+E.Message);
        dwEffect:= DROPEFFECT_NONE;
      end;
    end;
  finally
    FinishDrag;
  end;
end;

end.

{$ENDIF}
