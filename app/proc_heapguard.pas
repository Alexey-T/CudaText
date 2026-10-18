{
  proc_heapguard: "pad-to-chunk" memory-manager wrapper for FPC 3.2.2 (x64).

  Fixes the heap-chunk pinning behind issue #6533 memory complaints
  ("RAM stays high after closing big compare tabs", rounds 4-6 evidence):

  PROBLEM (rtl/inc/heap.inc, FPC 3.2.2, verified from source + v27.3 dumps)
  -------------------------------------------------------------------------
  A var-path GetMem (request > 544-8) that does not fit any free var block
  gets a fresh OS chunk, rounded UP to 64 KiB
    (rtl/inc/heap.inc: "size := (size+varfirstoffset+$ffff) and not $ffff").
  The rounding slack (up to ~64 KiB) becomes a FREE VAR BLOCK in the owning
  thread's varlist, for as long as the big block is alive. Any later var-path
  allocation (any request > 536: a long statusbar message, a TStringList
  array, a wrap-cache buffer...) can best-fit into that tail. From that
  moment the chunk can never be given back: FPC returns a chunk to the OS
  only when it is COMPLETELY empty ("append_to_oslist": only when
  oscount>=MaxKeptOSChunks(4) or chunksize > GrowHeapSize2(1 MiB)).
  So one 456-byte block keeps 86 MiB committed forever. Cross-thread frees
  make it worse: SysFreeMem_Var parks such blocks on the owner thread's
  waitvar list and the chunk is only released later, inside the owner's next
  var-path GetMem - with the tail exposed the whole time.

  Round-6 proofs (v27.3 --diffall):
    chunk 0x35f80000 86.2 MiB busy 480 B  = diff-text string of file A; its
      tail caught a console-editor wrap buffer during the compare;
    chunk 0xc0d0000 2.9 MB busy 1.4 KB     = editor paint bitmap chunk; its
      19,304 B tail caught the 1361-char "Opened: ..." statusbar message
      (block sits at exactly chunk+$78+2,995,232);
    chunk 0xb810000 1.4 MB busy 3.7 KB     = same mechanism, unknown block.

  FIX
  -------------------------------------------------------------------------
  Round every var-path request that would get a dedicated >1 MiB chunk UP
  so that its block is EXACTLY the whole usable space of the chunk:

    x64 constants (rtl/inc/heap.inc, verified):
      var header = 24, varfirstoffset = 56, quantization = 32, round = 64 KiB
    for request s > 1,048,457 (= last size that still lands in a shared
    1 MiB chunk):
      k   = ceil((s + 150) / 65536)        (always >= 17, so chunk > 1 MiB)
      req = k*65536 - 119
      -> block total  = roundup32(req+24)  = k*65536 - 64
      -> chunk        = roundup64(total+56)= k*65536
      -> initial free = (chunk-56) and ~31 = k*65536 - 64  = the block!
    split_block() creates a leftover only for remainders >= 40, so the
    chunk is ONE block with first+last flags and ZERO free tail.

  Consequences:
    * nothing can be carved into the chunk while the block is alive;
    * ANY free path (same thread, or cross-thread via the waitvar queue)
      finds the chunk whole-empty and > 1 MiB -> append_to_oslist ->
      free_oschunk -> SysOSFree: the chunk goes back to the OS at once,
      atomically, regardless of timing, focus, or close-all storms;
    * FreeMem needs NO wrapper logic at all (no state, no races, no
      bookkeeping): the memory-manager entry stays the RTL function.
  Cost: at most ~64 KiB extra transient bytes per big allocation, and one
  integer compare per GetMem. Small allocations (< ~1 MiB) are untouched.

  This is NOT the rejected "bigalloc" layer: nothing is routed around the
  FPC heap, no VirtualAlloc, no arena, no API change - big blocks still
  live in the normal FPC heap, just sized to own their chunk exclusively.

  Escape hatch for A/B testing: set environment CUDATEXT_NO_HEAPGUARD=1
  before starting the app to disable the wrapper.
}
unit proc_heapguard;

{$mode objfpc}{$H+}

interface

function HeapGuardInstalled: boolean;
// Returns the size that will actually be requested from the RTL for ASize,
// or ASize unchanged when ASize is not padded. Exposed for tests/diagnostics.
function HeapGuardPaddedSize(ASize: ptruint): ptruint;

implementation

uses
  SysUtils;

const
  // Last request size that is still served from a SHARED (<= 1 MiB) OS chunk:
  //   shared  iff roundup32(s+55)+56 <= GrowHeapSize2 (1 MiB)
  //        iff roundup32(s+55) <= 1048520  iff  s <= 1048457
  //        (1048520 = 32*32766+8; the largest 32-multiple below it is 1048512)
  // Everything above gets a dedicated chunk and is padded by this unit.
  HG_LAST_SHARED = 1048457;

var
  // NB: two records are required - the wrapper functions call OldMM.*, so
  // OldMM must never be overwritten with the wrapped entries (recursion!)
  OldMM: TMemoryManager; // pristine RTL (or previous) manager - called by wrappers
  NewMM: TMemoryManager; // copy with GetMem/AllocMem/ReAllocMem replaced
  FInstalled: boolean = false;

function HeapGuardInstalled: boolean;
begin
  Result := FInstalled;
end;

function HeapGuardPaddedSize(ASize: ptruint): ptruint;
var
  k: ptruint;
begin
  Result := ASize;
  if (ASize <= HG_LAST_SHARED) or (ASize > high(ptruint) - $20000) then
    exit;
  k := (ASize + 150 + 65535) div 65536;
  if k < 17 then
    k := 17; // never emit a chunk that could join the <=1 MiB oslist cache
  Result := k * 65536 - 119;
end;

function HGGetMem(size: ptruint): pointer;
begin
  Result := OldMM.GetMem(HeapGuardPaddedSize(size));
  if (Result = nil) and (size > HG_LAST_SHARED) then
    Result := OldMM.GetMem(size); // OOM fallback: unpadded is better than nil
end;

function HGAllocMem(size: ptruint): pointer;
begin
  if size > HG_LAST_SHARED then
  begin
    Result := HGGetMem(size);
    if Result <> nil then
      FillChar(Result^, size, 0);
  end
  else
    Result := OldMM.AllocMem(size);
end;

function HGReAllocMem(var p: pointer; size: ptruint): pointer;
var
  OldSize, MinSize: ptruint;
  q: pointer;
begin
  if p = nil then
  begin
    p := HGGetMem(size);
    Result := p;
    exit;
  end;
  if size = 0 then
  begin
    OldMM.FreeMem(p);
    p := nil;
    Result := nil;
    exit;
  end;
  OldSize := OldMM.MemSize(p);
  if not ((size > HG_LAST_SHARED) or (OldSize > HG_LAST_SHARED)) then
  begin
    // small -> small: keep plain RTL behaviour (in-place resize allowed)
    Result := OldMM.ReAllocMem(p, size);
    exit;
  end;
  // At least one side is big: always move. Never in-place-resize a padded
  // block: a shrink would re-open a free tail inside its chunk, and a grow
  // is impossible anyway (the block already ends at its chunk boundary).
  q := HGGetMem(size);
  MinSize := OldSize;
  if size < MinSize then
    MinSize := size;
  if q <> nil then
    Move(p^, q^, MinSize);
  OldMM.FreeMem(p);
  p := q;
  Result := q;
end;

initialization
  // Must run before LCL/Interfaces init and before any thread is created:
  // include this unit early in the .lpr uses list (right after cthreads /
  // proc_inittick). SetMemoryManager is only safe pre-threads.
  if GetEnvironmentVariable('CUDATEXT_NO_HEAPGUARD') = '' then
  begin
    GetMemoryManager(OldMM);
    NewMM := OldMM; // copy FIRST: OldMM must keep the real old entries
    NewMM.GetMem := @HGGetMem;
    NewMM.AllocMem := @HGAllocMem;
    NewMM.ReAllocMem := @HGReAllocMem;
    // FreeMem / FreeMemSize / MemSize / status entries stay the RTL ones:
    // padded chunks need no extra free-time logic at all.
    SetMemoryManager(NewMM);
    FInstalled := true;
  end;
end.
