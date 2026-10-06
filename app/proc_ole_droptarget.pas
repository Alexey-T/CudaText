{
Copyright 2025 CudaText developers.
License: MPL 2.0 (same as CudaText).

Implements OLE drag&drop (IDropTarget) support for CudaText on Windows,
so that text/URLs can be dragged into the app from other applications
(browser address bar, clipboard managers, other editors, etc).
Fixes issue: https://github.com/Alexey-T/CudaText/issues/4894

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
  modern Firefox builds on win10 show 'prohibited' cursor):
  * drop effects: copy is preferred, but link and move are accepted
    too (e.g. Firefox address-bar drags allow only 'link'); the drop
    handling always inserts the text, it never modifies the source;
  * format detection: IDataObject.QueryGetData is tried first; if it
    reports nothing, the format list is enumerated
    (IEnumFormatEtc); if that fails too, the data object is still
    accepted over the editor area, and the data is extracted on
    drop (QueryGetData/EnumFormatEtc of some sources lie);
  * text extraction: data is read from global memory first; if that
    fails, the same format is requested as IStream and read from
    the stream (some sources provide text only as a stream).
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
    FOnDropFiles: TDropFilesEvent;
    FOnDropText: TOleDropTextEvent;
    FOnCanDropText: TOleDropTextQueryEvent;
    function FindTarget(AWinControl: TWinControl): TOleDropTarget;
    procedure OnReattachTimer(Sender: TObject);
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
    FHasFiles: boolean; // data object has CF_HDROP
    FHasText: boolean;  // data object has text/URL format
    FUnknownData: boolean; // data object hides its formats: allow over editor, extract on drop
    procedure SetDataFlags(const AData: IDataObject);
    function IsDropAllowedAt(const pt: TPoint): boolean;
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
  end;

{ Global manager instance, created by InitOleDropSupport(). }
function OleDropSupport: TOleDropTargetManager;

{ Creates the global manager, must be called once from the main form. }
function InitOleDropSupport: TOleDropTargetManager;

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

type
  //structure of CF_HDROP data (parsed w/o ShellAPI's DragQueryFile)
  POleDropFiles = ^TOleDropFiles;
  TOleDropFiles = packed record
    pFiles: DWORD; //offset of file list
    pt: TPoint;    //drop point, client coords
    fNC: BOOL;
    fWide: BOOL;   //true: file list is WideChar
  end;

{ helper routines }

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
  Result:= DROPEFFECT_NONE;
end;

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

{ Queries the data object for a format with one storage medium.
  Some data objects implement QueryGetData for one medium only,
  so both HGLOBAL and ISTREAM are tried by DataHasFormat. }
function QueryHasFormat(const AData: IDataObject; AFormat: TClipFormat;
  ATymed: DWORD): boolean;
var
  FE: TFormatEtc;
begin
  Result:= false;
  if AData=nil then exit;
  if not GetFormatEtc(AFormat, ATymed, FE) then exit;
  Result:= AData.QueryGetData(FE)=S_OK;
end;

function DataHasFormat(const AData: IDataObject; AFormat: TClipFormat): boolean;
begin
  Result:= QueryHasFormat(AData, AFormat, TYMED_HGLOBAL);
  if not Result then
    Result:= QueryHasFormat(AData, AFormat, TYMED_ISTREAM);
end;

{ Checks the format by enumerating the data object's format list
  (fallback for data objects which don't answer QueryGetData
  correctly - matching is by clipboard format id, the storage
  medium advertised by the source is ignored). }
function DataObjectHasFormatId(const AData: IDataObject; AFormat: TClipFormat): boolean;
var
  Enum: IEnumFORMATETC;
  FE: TFormatEtc;
  N: DWord;
  i: integer;
begin
  Result:= false;
  if AData=nil then exit;
  if AFormat=0 then exit;
  if AData.EnumFormatEtc(DATADIR_GET, Enum)<>S_OK then exit;
  if Enum=nil then exit;
  Enum.Reset;
  N:= 0;
  for i:= 1 to 1000 do //defensive cap
  begin
    if Enum.Next(1, FE, @N)<>S_OK then exit;
    if N=0 then exit; //defensive: no item written
    if FE.cfFormat=AFormat then
      exit(true);
    N:= 0;
  end;
end;

function DataHasUrlFormat(const AData: IDataObject): boolean;
var
  Id: TClipFormat;
begin
  Id:= RegisterClipboardFormat(cUrlFormatNameW);
  if DataHasFormat(AData, Id) then exit(true);
  Id:= RegisterClipboardFormat(cUrlFormatNameA);
  if DataHasFormat(AData, Id) then exit(true);
  Id:= RegisterClipboardFormat(cMozUrlFormatName);
  if DataHasFormat(AData, Id) then exit(true);
  Result:= false;
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
  N: DWord;
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
  (some data objects provide their text only as a stream). }
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
    if Result<>'' then exit;
  end;

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
  end;
end;

{ Extracts dropped text: URL formats have priority,
  then CF_UNICODETEXT, then CF_TEXT. }
function GetDroppedText(const AData: IDataObject; out AText: string): boolean;
var
  Id: TClipFormat;
  S: string;
  i: integer;
begin
  Result:= false;
  AText:= '';

  //URL formats have priority; data is requested directly, without
  //QueryGetData: some data objects report 'no' but still give the data
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
end;

destructor TOleDropTargetManager.Destroy;
begin
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
  //Attach() is idempotent and cheap: it exits early when a window
  //handle is not allocated yet, or when it did not change
  for i:= 0 to FTargets.Count-1 do
    TOleDropTarget(FTargets[i]).Attach;
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
  //drop reference added by the interface cast: it is owned by COM now,
  //but the object is freed by the manager anyway
  Intf:= nil;
end;

procedure TOleDropTarget.Detach;
begin
  if FRegistered then
  begin
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
begin
  FHasFiles:= false;
  FHasText:= false;
  FUnknownData:= false;
  if AData=nil then exit;

  //fast way: QueryGetData (works for most apps)
  FHasFiles:= DataHasFormat(AData, CF_HDROP);
  FHasText:=
    (not FHasFiles) and
    (DataHasFormat(AData, CF_UNICODETEXT) or
     DataHasFormat(AData, CF_TEXT) or
     DataHasUrlFormat(AData));

  //fallback: some data objects (e.g. modern Firefox builds on win10,
  //issue #6521) don't answer QueryGetData correctly, but enumerate
  //their formats fine
  if (not FHasFiles) and (not FHasText) then
  begin
    FHasFiles:= DataObjectHasFormatId(AData, CF_HDROP);
    FHasText:=
      (not FHasFiles) and
      (DataObjectHasFormatId(AData, CF_UNICODETEXT) or
       DataObjectHasFormatId(AData, CF_TEXT) or
       DataObjectHasFormatId(AData, RegisterClipboardFormat(cUrlFormatNameW)) or
       DataObjectHasFormatId(AData, RegisterClipboardFormat(cMozUrlFormatName)) or
       DataObjectHasFormatId(AData, RegisterClipboardFormat(cUrlFormatNameA)));
  end;

  //last resort: the data object hides its formats from both
  //QueryGetData and EnumFormatEtc - allow it over the editor area
  //anyway (if OnCanDropText accepts the position) and try the full
  //extraction on Drop; some data objects say 'no' but give the data
  if (not FHasFiles) and (not FHasText) then
    FUnknownData:= true;
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

function TOleDropTarget.DragEnter(const dataObj: IDataObject;
  grfKeyState: DWORD; pt: TPoint; var dwEffect: DWORD): HRESULT; stdcall;
begin
  Result:= S_OK;
  SetDataFlags(dataObj);

  if IsDropAllowedAt(pt) then
    dwEffect:= SelectDropEffect(dwEffect)
  else
    dwEffect:= DROPEFFECT_NONE;
end;

function TOleDropTarget.DragOver(grfKeyState: DWORD; pt: TPoint;
  var dwEffect: DWORD): HRESULT; stdcall;
begin
  Result:= S_OK;
  if IsDropAllowedAt(pt) then
    dwEffect:= SelectDropEffect(dwEffect)
  else
    dwEffect:= DROPEFFECT_NONE;
end;

function TOleDropTarget.DragLeave: HRESULT; stdcall;
begin
  Result:= S_OK;
  FHasFiles:= false;
  FHasText:= false;
  FUnknownData:= false;
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

  if dataObj=nil then exit;
  if FManager=nil then exit;

  SetDataFlags(dataObj);

  try
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
        end;
  finally
    FHasFiles:= false;
    FHasText:= false;
    FUnknownData:= false;
  end;
end;

end.

{$ENDIF}
