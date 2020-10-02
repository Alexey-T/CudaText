{
Copy of some code from FreePascal 3.3 trunk,
so these funcs can be used in older FPC with 3.3 fixes
}
{$mode objfpc}
{$H+}
{$ModeSwitch advancedrecords}

unit at_sysutils;

interface

{$ifdef windows}
Function FileExists (Const FileName : RawByteString; FollowLink : Boolean = True) : Boolean;
{$endif}

implementation

{$ifdef windows}
uses Windows, SysUtils;
  
type
  TSymLinkResult = (
    slrOk,
    slrNoSymLink,
    slrError
  );    

var
  FindExInfoDefaults : TFINDEX_INFO_LEVELS = FindExInfoStandard;
  FindFirstAdditionalFlags : DWord = 0;


function FileGetSymLinkTargetInt(const FileName: UnicodeString; out SymLinkRec: TUnicodeSymLinkRec; RaiseErrorOnMissing: Boolean): TSymLinkResult;
{ reparse point specific declarations from Windows headers }
const
  IO_REPARSE_TAG_MOUNT_POINT = $A0000003;
  IO_REPARSE_TAG_SYMLINK = $A000000C;
  ERROR_REPARSE_TAG_INVALID = 4393;
  FSCTL_GET_REPARSE_POINT = $900A8;
  MAXIMUM_REPARSE_DATA_BUFFER_SIZE = 16 * 1024;
  SYMLINK_FLAG_RELATIVE = 1;
  FILE_FLAG_OPEN_REPARSE_POINT = $200000;
  FILE_READ_EA = $8;
type
  TReparseDataBuffer = record
    ReparseTag: ULONG;
    ReparseDataLength: Word;
    Reserved: Word;
    SubstituteNameOffset: Word;
    SubstituteNameLength: Word;
    PrintNameOffset: Word;
    PrintNameLength: Word;
    case ULONG of
      IO_REPARSE_TAG_MOUNT_POINT: (
        PathBufferMount: array[0..4095] of WCHAR);
      IO_REPARSE_TAG_SYMLINK: (
        Flags: ULONG;
        PathBufferSym: array[0..4095] of WCHAR);
  end;

const
  CShareAny = FILE_SHARE_READ or FILE_SHARE_WRITE or FILE_SHARE_DELETE;
  COpenReparse = FILE_FLAG_OPEN_REPARSE_POINT or FILE_FLAG_BACKUP_SEMANTICS;
var
  HFile, Handle: THandle;
  PBuffer: ^TReparseDataBuffer;
  BytesReturned: DWORD;
begin
  Result := slrError;
  SymLinkRec := Default(TUnicodeSymLinkRec);

  HFile := CreateFileW(PUnicodeChar(FileName), FILE_READ_EA, CShareAny, Nil, OPEN_EXISTING, COpenReparse, 0);
  if HFile <> INVALID_HANDLE_VALUE then
    try
      GetMem(PBuffer, MAXIMUM_REPARSE_DATA_BUFFER_SIZE);
      try
        if DeviceIoControl(HFile, FSCTL_GET_REPARSE_POINT, Nil, 0,
             PBuffer, MAXIMUM_REPARSE_DATA_BUFFER_SIZE, @BytesReturned, Nil) then begin
          case PBuffer^.ReparseTag of
            IO_REPARSE_TAG_MOUNT_POINT: begin
              SymLinkRec.TargetName := WideCharLenToString(
                @PBuffer^.PathBufferMount[4 { skip start '\??\' } +
                  PBuffer^.SubstituteNameOffset div SizeOf(WCHAR)],
                PBuffer^.SubstituteNameLength div SizeOf(WCHAR) - 4);
            end;
            IO_REPARSE_TAG_SYMLINK: begin
              SymLinkRec.TargetName := WideCharLenToString(
                @PBuffer^.PathBufferSym[PBuffer^.PrintNameOffset div SizeOf(WCHAR)],
                PBuffer^.PrintNameLength div SizeOf(WCHAR));
              if (PBuffer^.Flags and SYMLINK_FLAG_RELATIVE) <> 0 then
                SymLinkRec.TargetName := ExpandFileName(ExtractFilePath(FileName) + SymLinkRec.TargetName);
            end;
          end;

          if SymLinkRec.TargetName <> '' then begin
            Handle := FindFirstFileExW(PUnicodeChar(SymLinkRec.TargetName), FindExInfoDefaults , @SymLinkRec.FindData,
                        FindExSearchNameMatch, Nil, 0);
            if Handle <> INVALID_HANDLE_VALUE then begin
              Windows.FindClose(Handle);
              SymLinkRec.Attr := SymLinkRec.FindData.dwFileAttributes;
              SymLinkRec.Size := QWord(SymLinkRec.FindData.nFileSizeHigh) shl 32 + QWord(SymLinkRec.FindData.nFileSizeLow);
            end else if RaiseErrorOnMissing then
              raise EDirectoryNotFoundException.Create(SysErrorMessage(GetLastOSError))
            else
              SymLinkRec.TargetName := '';
          end else begin
            SetLastError(ERROR_REPARSE_TAG_INVALID);
            Result := slrNoSymLink;
          end;
        end else
          SetLastError(ERROR_REPARSE_TAG_INVALID);
      finally
        FreeMem(PBuffer);
      end;
    finally
      CloseHandle(HFile);
    end;

  if SymLinkRec.TargetName <> '' then
    Result := slrOk
end;


function FileGetSymLinkTarget(const FileName: UnicodeString; out SymLinkRec: TUnicodeSymLinkRec): Boolean;
begin
  Result := FileGetSymLinkTargetInt(FileName, SymLinkRec, True) = slrOk;
end;


function FileOrDirExists(const FileOrDirName: UnicodeString; CheckDir: Boolean; FollowLink: Boolean): Boolean;
const
  CDirAttributes: array[Boolean] of DWORD = (0, FILE_ATTRIBUTE_DIRECTORY);

  function FoundByEnum: Boolean;
  var
    FindData: TWin32FindDataW;
    Handle: THandle;
  begin
    { FindFirstFileEx is faster than FindFirstFile }
    Handle := FindFirstFileExW(PUnicodeChar(FileOrDirName), FindExInfoDefaults , @FindData,
                FindExSearchNameMatch, Nil, 0);
    Result := Handle <> INVALID_HANDLE_VALUE;
    if Result then begin
      Windows.FindClose(Handle);
      Result := (FindData.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY) = CDirAttributes[CheckDir];
    end;
  end;

const
  CNotExistsErrors = [
    ERROR_FILE_NOT_FOUND,
    ERROR_PATH_NOT_FOUND,
    ERROR_INVALID_NAME, // protects from names in the form of masks like '*'
    ERROR_INVALID_DRIVE,
    ERROR_NOT_READY,
    ERROR_INVALID_PARAMETER,
    ERROR_BAD_PATHNAME,
    ERROR_BAD_NETPATH,
    ERROR_BAD_NET_NAME
  ];
var
  Attr : DWord;
  slr : TUnicodeSymLinkRec;
  res : TSymLinkResult;
begin
  Attr := GetFileAttributesW(PUnicodeChar(FileOrDirName));
  if Attr = INVALID_FILE_ATTRIBUTES then
    Result := not (GetLastError in CNotExistsErrors) and FoundByEnum
  else begin
    Result := (Attr and FILE_ATTRIBUTE_DIRECTORY) = CDirAttributes[CheckDir];
    if Result and FollowLink and ((Attr and FILE_ATTRIBUTE_REPARSE_POINT) <> 0) then begin
      res := FileGetSymLinkTargetInt(FileOrDirName, slr, False);
      case res of
        slrOk:
          Result := FileOrDirExists(slr.TargetName, CheckDir, False);
        slrNoSymLink:
          Result := True;
        else
          Result := False;
      end;
    end;
  end;
end;


Function FileExists (Const FileName : UnicodeString; FollowLink : Boolean) : Boolean;
begin
  Result := FileOrDirExists(FileName, False, FollowLink);
end;
           

Function FileExists (Const FileName : RawByteString; FollowLink : Boolean) : Boolean;
begin
  Result:=FileExists(UnicodeString(FileName), FollowLink);
end;
     
{$endif}

end.
