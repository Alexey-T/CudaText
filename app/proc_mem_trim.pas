unit proc_mem_trim;
{
  OS-level trim of process memory, for CudaText on Windows.

  Why it's needed: FPC heap manager on Windows gets its OS chunks with
  HeapAlloc() from the NT process heap (see FPC rtl/win/sysheap.inc).
  FPC itself already releases fully-empty chunks back (HeapFree), but
  the NT heap keeps most freed tail pages *committed*; also CPython
  (used by plugins) and other DLLs keep freed pages in their own NT
  heaps. So after closing huge editor tabs (e.g. big diff sessions),
  Task Manager shows much more memory than the app really needs.
  HeapCompact() makes the NT heap decommit those committed-but-free
  regions, and SetProcessWorkingSetSize(-1,-1) drops stale pages from
  the working set, so the drop becomes visible at once.

  On Linux/macOS this unit is a compiled no-op: FPC heap there uses
  mmap/munmap chunks (rtl/unix/sysheap.inc) and already returns empty
  chunks to the OS, nothing to trim.

  Wiring in CudaText (two call sites):
  - TfmMain.TimerAppIdleTimer (idle timer) calls MemTrim_IdleTick():
      * if a "big free" was reported recently and things got quiet,
        a full trim runs: compact all heaps + clear working set;
      * else a cheap heap-compaction backstop runs every ~60 s.
  - proc_globdata.AppUpdateWatcherFrames calls MemTrim_NotifyBigFree()
    when it finished freeing lazily-deleted editor frames (this covers
    all tab closes, incl. diff tabs closed by plugins via ed.close()).

  All entry points are called from the UI thread (idle timer), so the
  small state below needs no locking. HeapCompact/SetProcessWorkingSet-
  Size are process-wide and thread-safe themselves.
}

{$mode objfpc}{$H+}

interface

//call when some big teardown just finished freeing objects;
//cheap, only schedules the real work for the idle timer
procedure MemTrim_NotifyBigFree;

//call periodically from an idle timer; cheap when nothing to do
procedure MemTrim_IdleTick;

//do the full trim right now; True if performed (False on non-Windows)
function MemTrim_Now: boolean;

//is OS-level trim supported? (Windows only)
function MemTrim_Supported: boolean;

//how many trims were performed since startup (diagnostics)
function MemTrim_TrimCount: integer;

implementation

uses
  {$ifdef MSWINDOWS}
  Windows,
  {$endif}
  SysUtils;

{$ifdef MSWINDOWS}
const
  //idle-tick backstop: compact NT heaps every N ms (cheap, no
  //working-set churn; catches frees not tied to tab close)
  MemTrim_AutoIntervalMs = 60000;
  //quiet time required after the last "big free" report, before the
  //event trim runs (frames are freed lazily, plugins free more
  //objects from on_close, CPython gc runs some ms later)
  MemTrim_BigFreeDelayMs = 2000;
  //clear the working set on the event path: makes the RAM drop visible
  //at once, costs some soft page faults on next touch, so it is NOT
  //done on the periodic backstop path
  MemTrim_WorkingSetOnBigFree = true;
  MemTrim_WorkingSetOnAuto = false;

var
  TrimLastTick: QWord = 0;      //tick of the last performed trim
  TrimPendingTick: QWord = 0;   //0 = no trim is scheduled
  TrimCount: integer = 0;

procedure TrimCore(AClearWorkingSet: boolean);
var
  HeapHandles: array of HANDLE;
  NHeaps, NGot: DWORD;
  I: integer;
begin
  //1. decommit committed-but-free regions in ALL NT heaps of the
  //process: FPC heap (process heap), CPython/UCRT heap(s), other DLLs
  NHeaps:= GetProcessHeaps(0, nil);
  if NHeaps>0 then
  begin
    SetLength(HeapHandles, NHeaps);
    NGot:= GetProcessHeaps(NHeaps, @HeapHandles[0]);
    if NGot>NHeaps then NGot:= NHeaps;
    for I:=0 to NGot-1 do
      HeapCompact(HeapHandles[I], 0);
  end;

  //2. ask the OS to remove pages from the working set
  //(PTRUINT(-1) = "remove as many pages as possible")
  if AClearWorkingSet then
    SetProcessWorkingSetSize(GetCurrentProcess, PtrUInt(-1), PtrUInt(-1));

  Inc(TrimCount);
  TrimLastTick:= GetTickCount64;
end;
{$endif}

procedure MemTrim_NotifyBigFree;
begin
  {$ifdef MSWINDOWS}
  //extend the quiet window on every report: one trim when the app
  //really settled, not one per closed tab
  TrimPendingTick:= GetTickCount64+MemTrim_BigFreeDelayMs;
  {$endif}
end;

procedure MemTrim_IdleTick;
{$ifdef MSWINDOWS}
var
  NTick: QWord;
{$endif}
begin
  {$ifdef MSWINDOWS}
  NTick:= GetTickCount64;

  //don't auto-trim right after startup (nothing was freed yet)
  if TrimLastTick=0 then
    TrimLastTick:= NTick;

  //event path: big teardown reported and things got quiet
  if (TrimPendingTick>0) and (NTick>=TrimPendingTick) then
  begin
    TrimPendingTick:= 0;
    TrimCore(MemTrim_WorkingSetOnBigFree);
    exit;
  end;

  //backstop path: periodic cheap compaction
  if NTick-TrimLastTick>=MemTrim_AutoIntervalMs then
    TrimCore(MemTrim_WorkingSetOnAuto);
  {$endif}
end;

function MemTrim_Now: boolean;
begin
  Result:= false;
  {$ifdef MSWINDOWS}
  TrimPendingTick:= 0;
  TrimCore(true);
  Result:= true;
  {$endif}
end;

function MemTrim_Supported: boolean;
begin
  {$ifdef MSWINDOWS}
  Result:= true;
  {$else}
  Result:= false;
  {$endif}
end;

function MemTrim_TrimCount: integer;
begin
  {$ifdef MSWINDOWS}
  Result:= TrimCount;
  {$else}
  Result:= 0;
  {$endif}
end;

end.
