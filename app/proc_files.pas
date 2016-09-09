(*
This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at http://mozilla.org/MPL/2.0/.

Copyright (c) Alexey Torgashin
*)
unit proc_files;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  LazFileUtils,
  LazUTF8Classes,
  CopyDir;

procedure FCreateFile(const fn: string; AsJson: boolean = false);
procedure FFindFilesInDir(const dir, mask: string; L: TStrings);
procedure FCopyDir(const d1, d2: string);

function IsFileContentText(const fn: string; BufSizeKb: DWORD; DetectOEM: Boolean; out IsOEM: Boolean): Boolean;
function IsFileReadonly(const fn: string): boolean;

procedure FFileAttrPrepare(const fn: string; var attr: Longint);
procedure FFileAttrRestore(const fn: string; attr: Longint);

implementation

procedure FCreateFile(const fn: string; AsJson: boolean);
begin
  with TStringListUTF8.Create do
  try
    if AsJson then
    begin
      Add('{');
      Add('');
      Add('}');
    end;
    SaveToFile(fn);
  finally
    Free;
  end;
end;

type
  TFreqTable = array[$80 .. $FF] of Integer;

function IsFileContentText(const fn: string; BufSizeKb: DWORD;
  DetectOEM: Boolean; out IsOEM: Boolean): Boolean;
var
  Buffer: PAnsiChar;
  BufSize, BytesRead, i: DWORD;
  n: Integer;
  Table: TFreqTable;
  TableSize: Integer;
  Str: TFileStreamUTF8;
  SSign: string;
begin
  Result:= False;
  IsOEM:= False;
  Str:= nil;
  Buffer:= nil;

  if BufSizeKb<=0 then Exit;
  BufSize:= BufSizeKb*1024;

  //Init freq table
  TableSize:= 0;
  FillChar(Table, SizeOf(Table), 0);

  try
    GetMem(Buffer, BufSize);
    FillChar(Buffer^, BufSize, 0);

    try
      Str:= TFileStreamUTF8.Create(fn, fmOpenRead);
    except
      Result:= false;
      Exit
    end;

    Str.Position:= 0;
    if Str.Size<=2 then
      begin Result:= true; Exit end;

    BytesRead:= Str.Read(Buffer^, BufSize);
    if BytesRead > 0 then
      begin
        //Test UTF16 signature
        SetString(SSign, Buffer, 2);
        if (SSign=#$ff#$fe) or (SSign=#$fe#$ff) then Exit(True);

        Result:= True;
        for i:= 0 to BytesRead - 1 do
        begin
          n:= Ord(Buffer[i]);

          //If control chars present, then non-text
          if (n < 32) and (n <> 09) and (n <> 13) and (n <> 10) then
            begin Result:= False; Break end;

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
              begin IsOEM:= True; Break end;
        end;

  finally
    if Assigned(Buffer) then
      FreeMem(Buffer);
    if Assigned(Str) then
      FreeAndNil(Str);
  end;
end;

function IsFileReadonly(const fn: string): boolean;
begin
  {$ifdef windows}
  Result:= FileIsReadOnlyUTF8(fn);
  {$else}
  Result:= not FileIsWritable(fn);
  {$endif}
end;


procedure FFindFilesInDir(const dir, mask: string; L: TStrings);
const
  attr = faAnyFile and not faDirectory;
var
  rec: TSearchRec;
begin
  L.Clear;
  if FindFirstUTF8(dir+DirectorySeparator+mask, attr, rec)=0 then
  begin
    L.Add(dir+DirectorySeparator+rec.Name);
    while FindNextUTF8(rec)=0 do
      L.Add(dir+DirectorySeparator+rec.Name);
  end;
  FindCloseUTF8(rec);
end;


{$ifdef windows}
procedure FFileAttrPrepare(const fn: string; var attr: Longint);
const
  spec = faReadOnly or faSysFile or faHidden;
var
  temp_attr: Longint;
begin
  attr:= 0;
  if not FileExistsUTF8(fn) then exit;

  temp_attr:= FileGetAttrUTF8(fn);
  if (temp_attr and spec)=0 then exit;
  attr:= temp_attr;
  FileSetAttrUTF8(fn, temp_attr and not spec);
end;
{$else}
procedure FFileAttrPrepare(const fn: string; var attr: Longint);
begin
  attr:= 0;
end;
{$endif}

procedure FFileAttrRestore(const fn: string; attr: Longint);
begin
  {$ifdef windows}
  if attr=0 then exit;
  FileSetAttrUTF8(fn, attr);
  {$endif}
end;

procedure FCopyDir(const d1, d2: string);
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


end.

