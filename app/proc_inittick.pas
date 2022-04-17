unit proc_inittick;

{$mode objfpc}{$H+}

interface

var
  NTickInitial: QWord = 0;

implementation

uses
  SysUtils;

initialization
  NTickInitial:= GetTickCount64;

end.

