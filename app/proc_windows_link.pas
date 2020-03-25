(*
This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at http://mozilla.org/MPL/2.0/.

Copyright (c) Alexey Torgashin
*)
unit proc_windows_link;

interface

function ResolveWindowShortcutTarget(const AFileName: string): string;

implementation

{$ifdef windows}
uses
  Windows,
  SysUtils,
  ShlObj, ComObj, ActiveX;

function GetWindowsShortcutTarget(const AFileName: string): string;
var
  ShellLink: IShellLinkW;
  PersistFile: IPersistFile;
  AnObj: IUnknown;
  buf: array[0 .. Pred(MAX_PATH)] of WideChar;
  fd: array[0 .. 1] of WIN32_FIND_DATAW;
begin
  try
    AnObj := CreateComObject(CLSID_ShellLink);
    ShellLink := AnObj as IShellLinkW;
    PersistFile := AnObj as IPersistFile;
    FillChar(buf, Sizeof(buf), 0);

    PersistFile.Load(PWideChar(WideString(AFileName)), 0);
    ShellLink.GetPath(buf, MAX_PATH, @fd[0], SLGP_UNCPRIORITY);
    Result := WideString(buf);
  except
    Result := ''
  end;
end;

function ResolveWindowShortcutTarget(const AFileName: string): string;
begin
  Result := AFileName;
  if LowerCase(ExtractFileExt(AFileName))='.lnk' then
    Result := GetWindowsShortcutTarget(Result);
end;

{$else}
function ResolveWindowShortcutTarget(const AFileName: string): string;
begin
  Result := AFileName;
end;
{$endif}

end.
