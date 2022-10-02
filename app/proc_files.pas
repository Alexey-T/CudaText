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
  Classes;

function AppCreateFile(const fn: string): boolean;
function AppCreateFileJSON(const fn: string): boolean;

procedure AppCopyDir(const DirSrc, DirTarget: string);

function AppIsFileContentText(const fn: string;
  BufSizeKb: integer;
  BufSizeWords: integer;
  DetectOEM: boolean): Boolean;

function AppIsFileReadonly(const fn: string): boolean;

procedure AppFileAttrPrepare(const fn: string; out attr: Longint);
procedure AppFileAttrRestore(const fn: string; attr: Longint);

function AppExpandFilename(const fn: string): string;
procedure AppBrowseToFilenameInShell(const fn: string);
function AppFileExtentionCreatable(const fn: string): boolean;
procedure AppFileCheckForNullBytes(const fn: string);
procedure AppMakeBackupFiles(const AFilename, AExtension: string; ACount: integer);
procedure AppFindFilesByMask(List: TStringList; AMask: string);


implementation

uses
  SysUtils, LCLIntf,
  FileUtil, LazFileUtils, LCLType,
  ATStrings,
  proc_globdata,
  proc_msg,
  win32linkfiles;

function AppCreateFile(const fn: string): boolean;
var
  L: TStringList;
begin
  Result:= false;
  L:= TStringList.Create;
  try
    try
      L.SaveToFile(fn);
      Result:= true;
    except
      on E: EFCreateError do
        MsgBox(msgCannotSaveFile+' '+fn, MB_OK or MB_ICONERROR);
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
    8, //Backspace
    9, //Tab
    10, //LF
    12, //FormFeed
    13, //CR
    26, //EOF
    27 //Escape
    ];
begin
  Result:= (n < 32) and not (byte(n) in cAllowedControlChars);
end;

function AppIsFileContentText(const fn: string; BufSizeKb: integer;
  BufSizeWords: integer;
  DetectOEM: boolean): Boolean;
const
  cBadBytesAtEndAllowed = 2;
var
  Buffer: PAnsiChar;
  BufSize, BytesRead, i: DWORD;
  n: Integer;
  Table: TFreqTable;
  TableSize: Integer;
  Str: TFileStream;
  IsLE: boolean;
  bReadAllFile: boolean;
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

    if Str.Size<=2 then
      exit(true);
    if DetectStreamUtf8NoBom(Str, BufSizeKb)=TBufferUTF8State.u8sYes then
      exit(true);
    if DetectStreamUtf16NoBom(Str, BufSizeWords, IsLE) then
      exit(true);

    Str.Position:= 0;

    GetMem(Buffer, BufSize);
    FillChar(Buffer^, BufSize, 0);

    BytesRead:= Str.Read(Buffer^, BufSize);
    if BytesRead > 0 then
      begin
        bReadAllFile:= BytesRead=Str.Size;

        //Test UTF-16 signature
        if (Buffer[0]=#$ff) and (Buffer[1]=#$fe) then
          exit(true);
        if (Buffer[0]=#$fe) and (Buffer[1]=#$ff) then
          exit(true);
        //Test UTF-32 BE signature
        if (BytesRead>=8) and (Buffer[0]=#0) and (Buffer[1]=#0) and (Buffer[2]=#$fe) and (Buffer[3]=#$ff) then
          exit(true);

        Result:= True;
        for i:= 0 to BytesRead - 1 do
        begin
          n:= Ord(Buffer[i]);

          //If control chars present, then non-text
          if IsAsciiControlChar(n) then
            //ignore bad bytes at the end, https://github.com/Alexey-T/CudaText/issues/2959
            if not (bReadAllFile and (i>=BytesRead-cBadBytesAtEndAllowed)) then
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

    //Analyze table
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
  //Result:= not FileIsWritable(fn);
  //on Unix, always allow to edit file, we can save even systems files via "pkexec"
  Result:= false;
  {$endif}
end;

{$ifdef windows}
procedure AppFileAttrPrepare(const fn: string; out attr: Longint);
const
  spec = faReadOnly or faSysFile{%H-} or faHidden{%H-};
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

procedure AppCopyDir(const DirSrc, DirTarget: string);
begin
  CopyDirTree(DirSrc, DirTarget, [
    cffOverwriteFile,
    cffCreateDestDirectory,
    cffPreserveTime
    ]);
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


procedure AppBrowseToFilenameInShell(const fn: string);
{$ifdef windows}
var
  Params: string;
{$endif}
begin
  if fn='' then exit;
  {$ifdef windows}
  Params:= '/n,/select,'+fn;
  ExecuteProcess('explorer.exe', Params);
  {$else}
  OpenURL(ExtractFileDir(fn));
  {$endif}
end;

function AppFileExtentionCreatable(const fn: string): boolean;
begin
  case LowerCase(ExtractFileExt(fn)) of
    '.zip',
    '.rar',
    '.tar',
    '.tgz',
    '.gz',
    '.xz',
    '.png',
    '.gif',
    '.jpg',
    '.jpeg',
    '.bmp',
    '.ico',
    '.icns',
    '.cuda-proj',
    '.cuda-session',
    '.dll',
    '.exe':
      Result:= false;
    else
      Result:= true;
  end;
end;


function AppFileIsNullBytes(const fn: string): boolean;
var
  fs: TFileStream;
  Buf: LongInt;
  NRead: integer;
begin
  Result:= false;
  if not FileExists(fn) then exit;

  fs:= TFileStream.Create(fn, fmOpenRead or fmShareDenyNone);
  try
    Buf:= 0;
    NRead:= fs.Read(Buf, SizeOf(Buf));
    Result:= (NRead=SizeOf(Buf)) and (Buf=0);
  finally
    FreeAndNil(fs);
  end;
end;


procedure AppFileCheckForNullBytes(const fn: string);
begin
  if not UiOps.AllowCheckConfigsForNullBytes then exit;

  if AppFileIsNullBytes(fn) then
    if MsgBox(Format(msgErrorNullBytesInFile, [fn]),
      MB_OKCANCEL or MB_ICONERROR) = ID_OK then
      DeleteFile(fn);
end;

procedure AppMakeBackupFiles(const AFilename, AExtension: string; ACount: integer);
var
  fnTemp, fnTemp2: string;
  i: integer;
begin
  for i:= ACount downto 1 do
  begin
    fnTemp:= ChangeFileExt(AFilename, '.'+IntToStr(i)+AExtension);
    if i>1 then
      fnTemp2:= ChangeFileExt(AFilename, '.'+IntToStr(i-1)+AExtension)
    else
      fnTemp2:= AFilename;
    if i>=ACount then
      if FileExists(fnTemp) then
        DeleteFile(fnTemp);
    if FileExists(fnTemp2) then
      RenameFile(fnTemp2, fnTemp);
  end;
end;

procedure AppFindFilesByMask(List: TStringList; AMask: string);
var
  Dir: string;
begin
  Dir:= GetCurrentDirUTF8;

  //support full dir path
  if IsOsFullPath(AMask) then
  begin
    Dir:= ExtractFileDir(AMask);
    AMask:= ExtractFileName(AMask);
  end
  else
  //support relative dir path
  if Pos(DirectorySeparator, AMask)>0 then
  begin
    Dir+= DirectorySeparator+ExtractFileDir(AMask);
    AMask:= ExtractFileName(AMask);
  end;

  FindAllFiles(List, Dir, AMask, false{SubDirs});
end;


end.

