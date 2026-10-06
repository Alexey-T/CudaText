{
Copyright 2025 CudaText developers.
License: MPL 2.0 (same as CudaText).

Implements OLE drag&drop OUT of CudaText on Windows: selected editor text
can be dragged into other applications (issue #4894, drag-out part).

Design (important): the editor's internal LCL drag&drop is NOT touched
at all. Editor keeps its normal behavior: moving selected text inside
the editor, dragging text to another editor/tab - all as in unpatched
CudaText. This unit only watches an ALREADY STARTED internal LCL drag,
and when the mouse cursor leaves all windows of this application while
the drag is in progress:
1. it cancels the internal LCL drag, using the public LCL API CancelDrag
   (exactly the same thing which happens when user presses Esc);
2. it starts an OLE drag (DoDragDrop) with the editor's selected text
   (CF_UNICODETEXT + CF_TEXT) while the mouse button is still held;
   user finishes the gesture by dropping the text in the target app.
So: while the cursor stays inside CudaText windows, this unit does
nothing, and internal drag&drop behaves 100% as before.

Details:
- TOleDragOutMonitor polls with a 50ms TTimer, which is nearly free:
  the handler exits early when no LCL drag is active.
- "Cursor is outside the app": GetCursorPos + WindowFromPoint +
  GetWindowThreadProcessId, window under cursor must belong to
  another process.
- DoDragDrop is called with DROPEFFECT_COPY only: the dropped text is
  inserted by the target app, and the source selection is NEVER
  deleted in CudaText (this matches CudaText's internal behavior for
  dragging text to another document, which also copies by default;
  users of issue #4894 expect no deletion). Dropping back into a
  CudaText window is handled by proc_ole_droptarget (inserts text,
  COPY effect).
- Esc cancels the OLE drag at any moment (standard IDropSource
  behavior), source text stays untouched.
}
unit proc_ole_dragsource;

{$mode objfpc}{$H+}

interface

{$IFDEF MSWINDOWS}

uses
  Windows, ActiveX, Classes, SysUtils, Controls, Forms, ExtCtrls,
  ATSynEdit;

type
  { TOleTextDataObject - implements IDataObject for dragging text out
    of CudaText. Provides CF_UNICODETEXT and CF_TEXT in TYMED_HGLOBAL.
    Standard COM life-time: freed when last reference is released. }

  TOleTextDataObject = class(TObject, IUnknown, IDataObject)
  private
    FRefCount: integer;
    FTextW: UnicodeString;
    FTextA: AnsiString;
    function MakeTextHGlobal(Wide: boolean): HGLOBAL;
  protected
    //IUnknown
    function QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} iid: TGuid; out obj): HRESULT; stdcall;
    function _AddRef: LongInt; stdcall;
    function _Release: LongInt; stdcall;
    //IDataObject
    function GetData(const formatetcIn: FORMATETC; out medium: STGMEDIUM): HRESULT; stdcall;
    function GetDataHere(const pformatetc: FORMATETC; out medium: STGMEDIUM): HRESULT; stdcall;
    function QueryGetData(const pformatetc: FORMATETC): HRESULT; stdcall;
    function GetCanonicalFormatEtc(const pformatetcIn: FORMATETC; out pformatetcOut: FORMATETC): HRESULT; stdcall;
    function SetData(const pformatetc: FORMATETC; var medium: STGMEDIUM; fRelease: BOOL): HRESULT; stdcall;
    function EnumFormatEtc(dwDirection: DWORD; out enumEtc: IEnumFORMATETC): HRESULT; stdcall;
    function DAdvise(const formatetc: FORMATETC; advf: DWORD; const advSink: IAdviseSink; out dwConnection: DWORD): HRESULT; stdcall;
    function DUnadvise(dwConnection: DWORD): HRESULT; stdcall;
    function EnumDAdvise(out enumAdvise: IEnumStatData): HRESULT; stdcall;
  public
    constructor Create(const AText: UnicodeString);
  end;

  { TOleEnumFormatEtc - enumerator over 2 formats of TOleTextDataObject.
    Standard COM life-time: freed when last reference is released. }

  TOleEnumFormatEtc = class(TObject, IUnknown, IEnumFORMATETC)
  private
    FRefCount: integer;
    FIndex: integer;
    procedure GetItem(AIndex: integer; var FE: FORMATETC);
  protected
    //IUnknown
    function QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} iid: TGuid; out obj): HRESULT; stdcall;
    function _AddRef: LongInt; stdcall;
    function _Release: LongInt; stdcall;
    //IEnumFORMATETC
    function Next(celt: ULONG; out rgelt: FORMATETC; pceltFetched: PULONG): HRESULT; stdcall;
    function Skip(celt: ULONG): HRESULT; stdcall;
    function Reset: HRESULT; stdcall;
    function Clone(out ppenum: IEnumFORMATETC): HRESULT; stdcall;
  public
    constructor Create;
  end;

  { TOleDropSource - implements IDropSource for dragging text out of
    CudaText. Standard COM life-time: freed when last reference is
    released. }

  TOleDropSource = class(TObject, IUnknown, IDropSource)
  private
    FRefCount: integer;
  protected
    //IUnknown
    function QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} iid: TGuid; out obj): HRESULT; stdcall;
    function _AddRef: LongInt; stdcall;
    function _Release: LongInt; stdcall;
    //IDropSource
    function QueryContinueDrag(fEscapePressed: BOOL; grfKeyState: DWORD): HRESULT; stdcall;
    function GiveFeedback(dwEffect: DWORD): HRESULT; stdcall;
  public
    constructor Create;
  end;

  { TOleDragOutMonitor - watches active LCL drag&drop sessions of the
    app, and when the mouse cursor leaves all app windows during a drag
    of editor text, converts it to an OLE drag&drop to other apps. }

  TOleDragOutMonitor = class(TComponent)
  private
    FTimer: TTimer;
    FBusy: boolean;            //true while DoDragDrop runs
    procedure OnTimerEvent(Sender: TObject);
    function FindDraggingEditor: TATSynEdit;
    function IsCursorOutsideAppWindows: boolean;
    procedure DoOleDragOut(AEditor: TATSynEdit);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

{ Creates the global monitor instance, owned by AOwner (main form);
  called once from TfmMain.FormCreate. }
function InitOleDragOutSupport(AOwner: TComponent): TOleDragOutMonitor;

{$ENDIF}

implementation

{$IFDEF MSWINDOWS}

uses
  //only for the debug log (DbgLog, ClipboardFormatName, HResultText)
  proc_ole_droptarget;

var
  MonitorIntf: TOleDragOutMonitor = nil;

function InitOleDragOutSupport(AOwner: TComponent): TOleDragOutMonitor;
begin
  if MonitorIntf=nil then
    MonitorIntf:= TOleDragOutMonitor.Create(AOwner);
  Result:= MonitorIntf;
end;

{ Converts UnicodeString to AnsiString using Windows ANSI codepage,
  needed for CF_TEXT data ( AnsiString(...) cast would use UTF-8 ). }
function WideToAnsiCP(const S: UnicodeString): AnsiString;
var
  N: integer;
begin
  Result:= '';
  if S='' then exit;
  N:= WideCharToMultiByte(CP_ACP, 0, PWideChar(S), Length(S), nil, 0, nil, nil);
  if N<=0 then exit;
  SetLength(Result, N);
  WideCharToMultiByte(CP_ACP, 0, PWideChar(S), Length(S),
    PAnsiChar(Result), N, nil, nil);
end;

{ TOleTextDataObject }

constructor TOleTextDataObject.Create(const AText: UnicodeString);
begin
  inherited Create;
  FRefCount:= 0;
  FTextW:= AText;
  FTextA:= WideToAnsiCP(AText);
end;

function TOleTextDataObject.QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} iid: TGuid; out obj): HRESULT; stdcall;
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

function TOleTextDataObject._AddRef: LongInt; stdcall;
begin
  inc(FRefCount);
  Result:= FRefCount;
end;

function TOleTextDataObject._Release: LongInt; stdcall;
begin
  dec(FRefCount);
  Result:= FRefCount;
  if Result=0 then
    Free;
end;

function TOleTextDataObject.MakeTextHGlobal(Wide: boolean): HGLOBAL;
{ allocates HGLOBAL with zero-terminated text; caller (target app)
  frees it via ReleaseStgMedium/GlobalFree }
var
  Size: DWORD;
  Ptr: Pointer;
  N: integer;
begin
  Result:= 0;
  if Wide then
  begin
    N:= Length(FTextW);
    Size:= (N+1)*SizeOf(WideChar);
  end
  else
  begin
    N:= Length(FTextA);
    Size:= N+1;
  end;

  Result:= GlobalAlloc(GMEM_MOVEABLE or GMEM_ZEROINIT, Size);
  if Result=0 then exit;

  Ptr:= GlobalLock(Result);
  if Ptr=nil then
  begin
    GlobalFree(Result);
    Result:= 0;
    exit;
  end;
  try
    if Wide then
    begin
      if N>0 then
        Move(PWideChar(FTextW)^, Ptr^, N*SizeOf(WideChar));
      PWideChar(Ptr)[N]:= #0;
    end
    else
    begin
      if N>0 then
        Move(PAnsiChar(FTextA)^, Ptr^, N);
      PAnsiChar(Ptr)[N]:= #0;
    end;
  finally
    GlobalUnlock(Result);
  end;
end;

function TOleTextDataObject.GetData(const formatetcIn: FORMATETC;
  out medium: STGMEDIUM): HRESULT; stdcall;
var
  H: HGLOBAL;
begin
  DbgLog('drag-out: target calls GetData('+
    ClipboardFormatName(formatetcIn.cfFormat)+')');
  FillChar(medium, SizeOf(medium), 0);

  if (formatetcIn.dwAspect<>DVASPECT_CONTENT) then
    exit(DV_E_DVASPECT);
  if (formatetcIn.lindex<>-1) then
    exit(DV_E_LINDEX);
  if (formatetcIn.tymed and TYMED_HGLOBAL)=0 then
    exit(DV_E_TYMED);

  case formatetcIn.cfFormat of
    CF_UNICODETEXT:
      H:= MakeTextHGlobal(true);
    CF_TEXT:
      H:= MakeTextHGlobal(false);
    else
      exit(DV_E_FORMATETC);
  end;

  if H=0 then
    exit(E_OUTOFMEMORY);

  medium.tymed:= TYMED_HGLOBAL;
  medium.hGlobal:= H;
  medium.PUnkForRelease:= nil;
  Result:= S_OK;
end;

function TOleTextDataObject.GetDataHere(const pformatetc: FORMATETC;
  out medium: STGMEDIUM): HRESULT; stdcall;
begin
  FillChar(medium, SizeOf(medium), 0);
  Result:= E_NOTIMPL;
end;

function TOleTextDataObject.QueryGetData(const pformatetc: FORMATETC): HRESULT; stdcall;
begin
  DbgLog('drag-out: target calls QueryGetData('+
    ClipboardFormatName(pformatetc.cfFormat)+')');
  if (pformatetc.dwAspect<>DVASPECT_CONTENT) then
    exit(DV_E_DVASPECT);
  if (pformatetc.lindex<>-1) then
    exit(DV_E_LINDEX);
  if (pformatetc.tymed and TYMED_HGLOBAL)=0 then
    exit(DV_E_TYMED);

  if (pformatetc.cfFormat=CF_UNICODETEXT) or
     (pformatetc.cfFormat=CF_TEXT) then
    Result:= S_OK
  else
    Result:= DV_E_FORMATETC;
end;

function TOleTextDataObject.GetCanonicalFormatEtc(const pformatetcIn: FORMATETC;
  out pformatetcOut: FORMATETC): HRESULT; stdcall;
begin
  FillChar(pformatetcOut, SizeOf(pformatetcOut), 0);
  pformatetcOut.ptd:= nil;
  Result:= E_NOTIMPL;
end;

function TOleTextDataObject.SetData(const pformatetc: FORMATETC;
  var medium: STGMEDIUM; fRelease: BOOL): HRESULT; stdcall;
begin
  Result:= E_NOTIMPL;
end;

function TOleTextDataObject.EnumFormatEtc(dwDirection: DWORD;
  out enumEtc: IEnumFORMATETC): HRESULT; stdcall;
var
  Enum: TOleEnumFormatEtc;
begin
  DbgLog('drag-out: target calls EnumFormatEtc');
  Pointer(enumEtc):= nil;
  if dwDirection<>DATADIR_GET then
    exit(E_NOTIMPL);
  Enum:= TOleEnumFormatEtc.Create;
  enumEtc:= IEnumFORMATETC(Enum);
  Result:= S_OK;
end;

function TOleTextDataObject.DAdvise(const formatetc: FORMATETC; advf: DWORD;
  const advSink: IAdviseSink; out dwConnection: DWORD): HRESULT; stdcall;
begin
  dwConnection:= 0;
  Result:= OLE_E_ADVISENOTSUPPORTED;
end;

function TOleTextDataObject.DUnadvise(dwConnection: DWORD): HRESULT; stdcall;
begin
  Result:= OLE_E_ADVISENOTSUPPORTED;
end;

function TOleTextDataObject.EnumDAdvise(out enumAdvise: IEnumStatData): HRESULT; stdcall;
begin
  Pointer(enumAdvise):= nil;
  Result:= OLE_E_ADVISENOTSUPPORTED;
end;

{ TOleEnumFormatEtc }
{ enumerates CF_UNICODETEXT, CF_TEXT, in this order }

constructor TOleEnumFormatEtc.Create;
begin
  inherited Create;
  FRefCount:= 0;
  FIndex:= 0;
end;

procedure TOleEnumFormatEtc.GetItem(AIndex: integer; var FE: FORMATETC);
begin
  FillChar(FE, SizeOf(FE), 0);
  if AIndex=0 then
    FE.cfFormat:= CF_UNICODETEXT
  else
    FE.cfFormat:= CF_TEXT;
  FE.dwAspect:= DVASPECT_CONTENT;
  FE.lindex:= -1;
  FE.tymed:= TYMED_HGLOBAL;
end;

function TOleEnumFormatEtc.QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} iid: TGuid; out obj): HRESULT; stdcall;
begin
  if GetInterface(iid, obj) then
    Result:= S_OK
  else
  begin
    Pointer(obj):= nil;
    Result:= E_NOINTERFACE;
  end;
end;

function TOleEnumFormatEtc._AddRef: LongInt; stdcall;
begin
  inc(FRefCount);
  Result:= FRefCount;
end;

function TOleEnumFormatEtc._Release: LongInt; stdcall;
begin
  dec(FRefCount);
  Result:= FRefCount;
  if Result=0 then
    Free;
end;

function TOleEnumFormatEtc.Next(celt: ULONG; out rgelt: FORMATETC;
  pceltFetched: PULONG): HRESULT; stdcall;
begin
  if FIndex>=2 then
  begin
    FillChar(rgelt, SizeOf(rgelt), 0);
    if pceltFetched<>nil then
      pceltFetched^:= 0;
    exit(S_FALSE);
  end;

  GetItem(FIndex, rgelt);
  inc(FIndex);

  if pceltFetched<>nil then
    pceltFetched^:= 1;

  if celt>1 then
    Result:= S_FALSE //provided less than requested
  else
    Result:= S_OK;
end;

function TOleEnumFormatEtc.Skip(celt: ULONG): HRESULT; stdcall;
begin
  inc(FIndex, celt);
  if FIndex>=2 then
    Result:= S_FALSE
  else
    Result:= S_OK;
end;

function TOleEnumFormatEtc.Reset: HRESULT; stdcall;
begin
  FIndex:= 0;
  Result:= S_OK;
end;

function TOleEnumFormatEtc.Clone(out ppenum: IEnumFORMATETC): HRESULT; stdcall;
var
  Enum: TOleEnumFormatEtc;
begin
  Pointer(ppenum):= nil;
  Enum:= TOleEnumFormatEtc.Create;
  Enum.FIndex:= FIndex;
  ppenum:= IEnumFORMATETC(Enum);
  Result:= S_OK;
end;

{ TOleDropSource }

constructor TOleDropSource.Create;
begin
  inherited Create;
  FRefCount:= 0;
end;

function TOleDropSource.QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} iid: TGuid; out obj): HRESULT; stdcall;
begin
  if GetInterface(iid, obj) then
    Result:= S_OK
  else
  begin
    Pointer(obj):= nil;
    Result:= E_NOINTERFACE;
  end;
end;

function TOleDropSource._AddRef: LongInt; stdcall;
begin
  inc(FRefCount);
  Result:= FRefCount;
end;

function TOleDropSource._Release: LongInt; stdcall;
begin
  dec(FRefCount);
  Result:= FRefCount;
  if Result=0 then
    Free;
end;

function TOleDropSource.QueryContinueDrag(fEscapePressed: BOOL;
  grfKeyState: DWORD): HRESULT; stdcall;
begin
  if fEscapePressed then
    exit(DRAGDROP_S_CANCEL);

  //drag was started with left mouse button: wait for its release
  if (grfKeyState and MK_LBUTTON)=0 then
    exit(DRAGDROP_S_DROP);

  Result:= S_OK;
end;

function TOleDropSource.GiveFeedback(dwEffect: DWORD): HRESULT; stdcall;
begin
  //use standard OLE drag cursors (copy/move/no-drop arrows)
  Result:= DRAGDROP_S_USEDEFAULTCURSORS;
end;

{ TOleDragOutMonitor }

constructor TOleDragOutMonitor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTimer:= TTimer.Create(Self);
  FTimer.Interval:= 50;
  FTimer.OnTimer:= @OnTimerEvent;
  FTimer.Enabled:= true;
end;

destructor TOleDragOutMonitor.Destroy;
begin
  if FTimer<>nil then
  begin
    FTimer.Enabled:= false;
    FreeAndNil(FTimer);
  end;
  if MonitorIntf=Self then
    MonitorIntf:= nil;
  inherited Destroy;
end;

procedure TOleDragOutMonitor.DoOleDragOut(AEditor: TATSynEdit);
var
  SText: UnicodeString;
  Data: TOleTextDataObject;
  Source: TOleDropSource;
  IntfData: IDataObject;
  IntfSource: IDropSource;
  Effect: DWORD;
  H: HRESULT;
begin
  SText:= AEditor.TextSelected;
  if SText='' then exit;

  //stop polling: DoDragDrop runs a nested message loop
  FTimer.Enabled:= false;
  try
    //cancel the internal LCL drag: the same happens on Esc keypress.
    //Internal editor drag&drop is not modified in any way, it just ends
    //here, because the mouse cursor left the app windows.
    CancelDrag;

    //clean the leftover drop-marker of the cancelled drag
    AEditor.Invalidate;

    FBusy:= true;
    try
      Data:= TOleTextDataObject.Create(SText);
      Source:= TOleDropSource.Create;
      IntfData:= IDataObject(Data);
      IntfSource:= IDropSource(Source);
      try
        Effect:= DROPEFFECT_NONE;
        //only the COPY effect is allowed: source selection is never
        //deleted, like CudaText's internal drag to another document
        //(which copies by default). Even if a target app wants 'move',
        //it can only get a copy - users expect no text deletion
        //(issue #4894 testing).
        DbgLog('drag-out: DoDragDrop starts, '+IntToStr(Length(SText))+
          ' chars selected');
        H:= DoDragDrop(IntfData, IntfSource, DROPEFFECT_COPY, @Effect);
        DbgLog('drag-out: DoDragDrop ended, result='+HResultText(H)+
          ', final effect='+IntToHex(Effect, 8));
        //H=DRAGDROP_S_DROP: text is inserted by the target app,
        //source keeps the selection.
        //H=DRAGDROP_S_CANCEL: user pressed Esc, drag aborted,
        //source keeps the selection.
      finally
        IntfData:= nil;
        IntfSource:= nil;
      end;
    finally
      FBusy:= false;
    end;
  finally
    FTimer.Enabled:= true;
  end;
end;

function FindDraggingControl(AControl: TControl): TControl;
var
  i: integer;
  WC: TWinControl;
begin
  Result:= nil;
  if AControl=nil then exit;
  if AControl.Dragging then exit(AControl);
  //only TWinControl can have child controls
  if AControl is TWinControl then
  begin
    WC:= TWinControl(AControl);
    for i:= 0 to WC.ControlCount-1 do
    begin
      Result:= FindDraggingControl(WC.Controls[i]);
      if Result<>nil then exit;
    end;
  end;
end;

function TOleDragOutMonitor.FindDraggingEditor: TATSynEdit;
var
  i: integer;
  C: TControl;
begin
  Result:= nil;
  for i:= 0 to Screen.FormCount-1 do
  begin
    C:= FindDraggingControl(Screen.Forms[i]);
    if C<>nil then
    begin
      //only one drag can be active; if it's not an editor
      //(e.g. tab dragging), drag-out is not started
      if C is TATSynEdit then
        Result:= TATSynEdit(C);
      exit;
    end;
  end;
end;

function TOleDragOutMonitor.IsCursorOutsideAppWindows: boolean;
var
  P: TPoint;
  H: HWND;
  PID: DWORD;
begin
  Result:= true;
  if not GetCursorPos(P) then exit;
  H:= WindowFromPoint(P);
  if H=0 then exit; //cursor is not over any window
  GetWindowThreadProcessId(H, @PID);
  if PID=0 then exit;
  Result:= PID<>GetCurrentProcessId;
end;

procedure TOleDragOutMonitor.OnTimerEvent(Sender: TObject);
var
  Ed: TATSynEdit;
begin
  if FBusy then exit;

  //fast exit when no LCL drag&drop is running
  if (DragManager=nil) or (not DragManager.IsDragging) then exit;

  //drag must be started by an editor with selected text
  Ed:= FindDraggingEditor;
  if Ed=nil then exit;

  //cursor must leave all windows of this app
  if not IsCursorOutsideAppWindows then exit;

  //mouse button must still be pressed,
  //else the drag is being released right now
  if (GetAsyncKeyState(VK_LBUTTON) and $8000)=0 then exit;

  //editor must have selected text
  if Ed.TextSelected='' then exit;

  DoOleDragOut(Ed);
end;

{$ENDIF}

end.
