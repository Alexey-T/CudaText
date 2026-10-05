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
- On drop, data is extracted from the COM IDataObject:
  * CF_HDROP - filenames, they are reported via OnDropFiles
    (same event format as LCL's OnDropFiles);
  * CF_UNICODETEXT / CF_TEXT - text, it is reported via OnDropText;
  * registered clipboard formats 'UniformResourceLocatorW' /
    'UniformResourceLocator' / 'text/x-moz-url' - URLs from browsers.
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
  Windows, ActiveX, Classes, SysUtils, Contnrs, Controls, Forms;

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

  TOleDropTarget = class;

  { TOleDropTargetManager }

  TOleDropTargetManager = class
  private
    FTargets: TFPObjectList; // owns TOleDropTarget objects
    FOnDropFiles: TDropFilesEvent;
    FOnDropText: TOleDropTextEvent;
    function FindTarget(AWinControl: TWinControl): TOleDropTarget;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Attach(AWinControl: TWinControl);
    procedure Detach(AWinControl: TWinControl);
    property OnDropFiles: TDropFilesEvent read FOnDropFiles write FOnDropFiles;
    property OnDropText: TOleDropTextEvent read FOnDropText write FOnDropText;
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
    FCanDrop: boolean;
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
    ManagerIntf:= TOleDropTargetManager.Create;
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

function GetFormatEtc(AFormat: TClipFormat; out FE: TFormatEtc): boolean;
begin
  Result:= AFormat<>0;
  if not Result then exit;
  FillChar(FE, SizeOf(FE), 0);
  FE.cfFormat:= AFormat;
  FE.dwAspect:= DVASPECT_CONTENT;
  FE.lindex:= -1;
  FE.tymed:= TYMED_HGLOBAL;
end;

function DataHasFormat(const AData: IDataObject; AFormat: TClipFormat): boolean;
var
  FE: TFormatEtc;
begin
  Result:= false;
  if not GetFormatEtc(AFormat, FE) then exit;
  Result:= AData.QueryGetData(FE)=S_OK;
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
  if not GetFormatEtc(AFormat, FE) then exit;

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

{ Extracts dropped text: URL formats have priority,
  then CF_UNICODETEXT, then CF_TEXT. }
function GetDroppedText(const AData: IDataObject; out AText: string): boolean;
var
  Global: HGLOBAL;
  Id: TClipFormat;
  S: string;
  i: integer;
begin
  Result:= false;
  AText:= '';

  Id:= RegisterClipboardFormat(cUrlFormatNameW);
  if DataHasFormat(AData, Id) then
    if GetDataHGlobal(AData, Id, Global) then
    begin
      try
        AText:= TextFromGlobal(Global, true);
      finally
        ReleaseStgMediumForHGlobal(Global);
      end;
      if AText<>'' then exit(true);
    end;

  Id:= RegisterClipboardFormat(cMozUrlFormatName);
  if DataHasFormat(AData, Id) then
    if GetDataHGlobal(AData, Id, Global) then
    begin
      try
        //content is "url\ntitle"
        AText:= TextFromGlobal(Global, true);
      finally
        ReleaseStgMediumForHGlobal(Global);
      end;
      i:= Pos(#10, AText);
      if i>0 then
        AText:= Copy(AText, 1, i-1);
      AText:= TrimRight(AText);
      if AText<>'' then exit(true);
    end;

  Id:= RegisterClipboardFormat(cUrlFormatNameA);
  if DataHasFormat(AData, Id) then
    if GetDataHGlobal(AData, Id, Global) then
    begin
      try
        AText:= TextFromGlobal(Global, false);
      finally
        ReleaseStgMediumForHGlobal(Global);
      end;
      if AText<>'' then exit(true);
    end;

  if DataHasFormat(AData, CF_UNICODETEXT) then
    if GetDataHGlobal(AData, CF_UNICODETEXT, Global) then
    begin
      try
        AText:= TextFromGlobal(Global, true);
      finally
        ReleaseStgMediumForHGlobal(Global);
      end;
      if AText<>'' then exit(true);
    end;

  if DataHasFormat(AData, CF_TEXT) then
    if GetDataHGlobal(AData, CF_TEXT, Global) then
    begin
      try
        AText:= TextFromGlobal(Global, false);
      finally
        ReleaseStgMediumForHGlobal(Global);
      end;
      if AText<>'' then exit(true);
    end;

  Result:= false;
end;

{ TOleDropTargetManager }

constructor TOleDropTargetManager.Create;
begin
  inherited;
  FTargets:= TFPObjectList.Create(true);
end;

destructor TOleDropTargetManager.Destroy;
begin
  //revoke all registrations, free all targets
  while FTargets.Count>0 do
    FTargets.Delete(0);
  FreeAndNil(FTargets);
  if ManagerIntf=Self then
    ManagerIntf:= nil;
  inherited;
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

function TOleDropTarget.DragEnter(const dataObj: IDataObject;
  grfKeyState: DWORD; pt: TPoint; var dwEffect: DWORD): HRESULT; stdcall;
begin
  Result:= S_OK;
  FCanDrop:= false;

  if dataObj=nil then
  begin
    dwEffect:= DROPEFFECT_NONE;
    exit;
  end;

  //check if data object has any format we accept
  FCanDrop:=
    DataHasFormat(dataObj, CF_HDROP) or
    DataHasFormat(dataObj, CF_UNICODETEXT) or
    DataHasFormat(dataObj, CF_TEXT) or
    DataHasUrlFormat(dataObj);

  if FCanDrop and ((dwEffect and DROPEFFECT_COPY)<>0) then
    dwEffect:= DROPEFFECT_COPY
  else
    dwEffect:= DROPEFFECT_NONE;
end;

function TOleDropTarget.DragOver(grfKeyState: DWORD; pt: TPoint;
  var dwEffect: DWORD): HRESULT; stdcall;
begin
  Result:= S_OK;
  if FCanDrop and ((dwEffect and DROPEFFECT_COPY)<>0) then
    dwEffect:= DROPEFFECT_COPY
  else
    dwEffect:= DROPEFFECT_NONE;
end;

function TOleDropTarget.DragLeave: HRESULT; stdcall;
begin
  Result:= S_OK;
  FCanDrop:= false;
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
  FCanDrop:= false;

  if dataObj=nil then exit;
  if FManager=nil then exit;

  //files have priority: dropped file(s) from Explorer open like before
  Files:= TStringList.Create;
  try
    if GetDroppedFiles(dataObj, Files) then
    begin
      dwEffect:= DROPEFFECT_COPY;
      if Assigned(FManager.FOnDropFiles) then
      begin
        SetLength(FileArr, Files.Count);
        for i:= 0 to Files.Count-1 do
          FileArr[i]:= Files[i];
        FManager.FOnDropFiles(FWinControl, FileArr);
      end;
      exit;
    end;

    //text or URL
    if GetDroppedText(dataObj, S) then
    begin
      if S<>'' then
      begin
        dwEffect:= DROPEFFECT_COPY;
        if Assigned(FManager.FOnDropText) then
          FManager.FOnDropText(FWinControl, S, pt);
      end;
    end;
  finally
    FreeAndNil(Files);
  end;
end;

end.

{$ENDIF}
