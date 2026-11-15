unit proc_inittick;

{$mode objfpc}{$H+}

interface

var
  AppTickInitial: QWord = 0;
  //Value of env var DESKTOP_STARTUP_ID (or XDG_ACTIVATION_TOKEN) read before
  //LCL/GTK3 init: GTK3 clears DESKTOP_STARTUP_ID env var during init, so it must
  //be stashed early. Passed to the 1st app instance via UniqueInstance IPC,
  //to fix focusing, issue #6618.
  AppStartupId: string = '';

implementation

uses
  SysUtils;

initialization
  AppTickInitial:= GetTickCount64;
  AppStartupId:= GetEnvironmentVariable('DESKTOP_STARTUP_ID');
  if AppStartupId='' then
    AppStartupId:= GetEnvironmentVariable('XDG_ACTIVATION_TOKEN');

end.
