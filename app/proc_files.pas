(*
This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at http://mozilla.org/MPL/2.0/.

Copyright (c) Alexey Torgashin
*)
unit proc_files;

{$mode objfpc}{$H+}

interface

function AppCreateFile(const fn: string): boolean;
function AppCreateFileJSON(const fn: string): boolean;

procedure AppCopyDir(const d1, d2: string);

function AppIsFileContentText(const fn: string;
  BufSizeKb: integer;
  BufSizeWords: integer;
  DetectOEM: boolean): Boolean;

function AppIsFileReadonly(const fn: string): boolean;

procedure AppFileAttrPrepare(const fn: string; out attr: Longint);
procedure AppFileAttrRestore(const fn: string; attr: Longint);

function AppExpandFilename(const fn: string): string;


implementation

uses
  Classes, SysUtils,
  LazFileUtils,
  ATStrings,
  proc_globdata,
  proc_windows_link,
  CopyDir;

function AppCreateFile(const fn: string): boolean;
var
  L: TStringList;
begin
  L:= TStringList.Create;
  try
    try
      L.SaveToFile(fn);
      Result:= true;
    except
      Result:= false;
    end;
  finally
    FreeAndNil(L);
  end;
end;

function AppCreateFileJSON(const fn: string): boolean;
var
  L: TStringList;
begin
  L:= TStringList.Create;
  try
    L.Add('{');
    L.Add('');
    L.Add('}');

    try
      L.SaveToFile(fn);
      Result:= true;
    except
      Result:= false;
    end;
  finally
    FreeAndNil(L);
  end;
end;


type
  TFreqTable = array[$80 .. $FF] of Integer;

function IsAsciiControlChar(n: integer): boolean; inline;
const
  cAllowedControlChars: set of byte = [
    7, //Bell
    9,
    10,
    13,
    12, //FormFeed
    26 //EOF
    ];
begin
  Result:= (n < 32) and not (byte(n) in cAllowedControlChars);
end;

function AppIsFileContentText(const fn: string; BufSizeKb: integer;
  BufSizeWords: integer;
  DetectOEM: boolean): Boolean;
var
  Buffer: PAnsiChar;
  BufSize, BytesRead, i: DWORD;
  n: Integer;
  Table: TFreqTable;
  TableSize: Integer;
  Str: TFileStream;
  IsLE: boolean;
begin
  Result:= False;
  //IsOEM:= False;
  Str:= nil;
  Buffer:= nil;

  if BufSizeKb<=0 then Exit;
  BufSize:= BufSizeKb*1024;

  //Init freq table
  TableSize:= 0;
  FillChar(Table, SizeOf(Table), 0);

  try
    try
      Str:= TFileStream.Create(fn, fmOpenRead or fmShareDenyNone);
    except
      exit(false);
    end;

    if Str.Size<=2 then exit(true);
    if IsStreamWithUt8NoBom(Str, BufSizeKb) then exit(true);
    if IsStreamWithUtf16NoBom(Str, BufSizeWords, IsLE) then exit(true);
    Str.Position:= 0;

    GetMem(Buffer, BufSize);
    FillChar(Buffer^, BufSize, 0);

    BytesRead:= Str.Read(Buffer^, BufSize);
    if BytesRead > 0 then
      begin
        //Test UTF16 signature
        if ((Buffer[0]=#$ff) and (Buffer[1]=#$fe)) or
          ((Buffer[0]=#$fe) and (Buffer[1]=#$ff)) then
         Exit(True);

        Result:= True;
        for i:= 0 to BytesRead - 1 do
        begin
          n:= Ord(Buffer[i]);

          //If control chars present, then non-text
          if IsAsciiControlChar(n) then
          begin
            Result:= False;
            Break
          end;

          //Calculate freq table
          if DetectOEM then
            if (n >= Low(Table)) and (n <= High(Table)) then
            begin
              Inc(TableSize);
              Inc(Table[n]);
            end;
        end;
      end;

    //Analize table
    if DetectOEM then
      if Result and (TableSize > 0) then
        for i:= Low(Table) to High(Table) do
        begin
          Table[i]:= Table[i] * 100 div TableSize;
          if ((i >= $B0) and (i <= $DF)) or (i = $FF) or (i = $A9) then
            if Table[i] >= 18 then
            begin
              //IsOEM:= True;
              Break
            end;
        end;

  finally
    if Assigned(Buffer) then
      FreeMem(Buffer);
    if Assigned(Str) then
      FreeAndNil(Str);
  end;
end;

function AppIsFileReadonly(const fn: string): boolean;
begin
  {$ifdef windows}
  Result:= FileIsReadOnlyUTF8(fn);
  {$else}
  Result:= not FileIsWritable(fn);
  {$endif}
end;

{$ifdef windows}
procedure AppFileAttrPrepare(const fn: string; out attr: Longint);
const
  spec = faReadOnly or faSysFile or faHidden;
var
  temp_attr: Longint;
begin
  attr:= 0;
  if not FileExists(fn) then exit;

  temp_attr:= FileGetAttrUTF8(fn);
  if (temp_attr and spec)=0 then exit;
  attr:= temp_attr;
  FileSetAttrUTF8(fn, temp_attr and not spec);
end;
{$else}
procedure AppFileAttrPrepare(const fn: string; out attr: Longint);
begin
  attr:= 0;
end;
{$endif}

procedure AppFileAttrRestore(const fn: string; attr: Longint);
begin
  {$ifdef windows}
  if attr=0 then exit;
  FileSetAttrUTF8(fn, attr);
  {$endif}
end;

procedure AppCopyDir(const d1, d2: string);
var
  c: TCopyDir;
begin
  c:= TCopyDir.Create(d1, d2);
  try
    c.Start;
  finally
    c.Free;
  end;
end;

function AppExpandWin32RelativeRootFilename(const fn: string): string;
//this fixes issue #2862, expand '\name.txt'
begin
  Result:= fn;
  {$ifdef windows}
  if (Length(Result)>1) and (Result[1]='\') and (Result[2]<>'\') then
    Insert(ExtractFileDrive(GetCurrentDir), Result, 1);
  {$endif}
end;

function AppExpandFilename(const fn: string): string;
begin
  if fn='' then exit(fn);

  //handle cmd-line options here
  if fn[1]='-' then exit(fn);

  Result:=
    ResolveWindowsLinkTarget(
    ExpandFileName(
    {$ifdef windows}
    AppExpandWin32RelativeRootFilename(
    {$else}
    AppExpandHomeDirInFilename(
    {$endif}
    fn
    )));
end;


end.

