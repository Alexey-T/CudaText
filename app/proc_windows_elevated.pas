(*
This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at http://mozilla.org/MPL/2.0/.

Copyright (c) Alexey Torgashin
*)
unit proc_windows_elevated;

{$mode ObjFPC}{$H+}

interface

function RunElevated(const AProgram, AParameters: UnicodeString): boolean;

implementation

uses
  Windows, ShellAPI, SysUtils, Classes, Forms;

function RunElevated(const AProgram, AParameters: UnicodeString): boolean;
var
  sei: TShellExecuteInfoW;
begin
  FillChar(sei, SizeOf(sei), 0);
  sei.cbSize := SizeOf(sei);
  sei.fMask := SEE_MASK_NOCLOSEPROCESS or SEE_MASK_UNICODE;
  sei.Wnd := Application.MainForm.Handle;
  sei.lpVerb := 'runas';
  sei.lpFile := PWideChar(AProgram);
  sei.lpParameters := PWideChar(AParameters);
  sei.nShow := SW_SHOW;

  Result := ShellExecuteExW(@sei);
  if Result then
  begin
    WaitForSingleObject(sei.hProcess, INFINITE);
    CloseHandle(sei.hProcess);
  end;
  {
  else
    Application.MessageBox(
      PChar(Format('ShellExecuteExW failed: %d', [GetLastError])),
      'CudaText');
  }
end;


end.

