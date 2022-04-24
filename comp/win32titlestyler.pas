(*
This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at http://mozilla.org/MPL/2.0/.

Copyright (c) Alexey Torgashin
*)
unit win32titlestyler;

{$mode objfpc}{$H+}

interface

uses
  Windows, SysUtils, Forms, Graphics;

procedure ApplyFormDarkTitle(AForm: TForm; ADarkMode: bool; AForceApply: bool);

implementation

const
  //DWMWA_USE_IMMERSIVE_DARK_MODE_BEFORE_20H1: Integer = 19;
  DWMWA_USE_IMMERSIVE_DARK_MODE: Integer = 20;
  
type
  TDwmSetWindowAttribute = function(hwnd: HWND; dwAttribute: DWORD; pvAttribute: Pointer; cbAttribute: DWORD): HRESULT; stdcall;
  // https://docs.microsoft.com/en-us/windows/win32/api/dwmapi/nf-dwmapi-dwmsetwindowattribute

var
  DwmSetWindowAttribute: TDwmSetWindowAttribute = nil;
  hLib: THandle = 0;
  LastFormHandle: THandle = 0;
  LastDarkMode: bool = false;

procedure ApplyFormDarkTitle(AForm: TForm; ADarkMode: bool; AForceApply: bool);
begin
  //require Windows Vista
  if Win32MajorVersion<6 then exit;

  if not AForceApply then
    if (LastFormHandle=AForm.Handle) and
      (LastDarkMode=ADarkMode) then exit;

  if hLib=0 then
    hLib:= LoadLibrary('dwmapi.dll');

  if hLib<>0 then
  begin
    LastFormHandle:= AForm.Handle;
    LastDarkMode:= ADarkMode;

    Pointer(DwmSetWindowAttribute):= GetProcAddress(hLib, 'DwmSetWindowAttribute');
    if Assigned(DwmSetWindowAttribute) then
      try
        DwmSetWindowAttribute(AForm.Handle, DWMWA_USE_IMMERSIVE_DARK_MODE, @ADarkMode, SizeOf(ADarkMode));

        if AForceApply then
        begin
          AForm.Width:= AForm.Width-1;
          AForm.Width:= AForm.Width+1;
        end;
      except
      end;
  end;
end;

finalization
  if hLib<>0 then
    FreeLibrary(hLib);

end.
