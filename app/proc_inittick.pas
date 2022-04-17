unit proc_inittick;

{$mode objfpc}{$H+}

interface

var
  AppTickInitial: QWord = 0;

implementation

uses
  SysUtils;

initialization
  AppTickInitial:= GetTickCount64;

end.

