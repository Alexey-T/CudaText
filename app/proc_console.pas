unit proc_console;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ATStringProc,gqueue;

type
  TAppConsoleQueue = specialize TQueue<UnicodeString>;
var
  AppConsoleQueue: TAppConsoleQueue;

procedure MsgLogConsole(const AText: string);

implementation

procedure MsgLogConsole(const AText: string);
var
  Sep: TATStringSeparator;
  S: UnicodeString;
begin
  if Pos(#10, AText)=0 then
    AppConsoleQueue.Push(AText)
  else
  begin
    Sep.Init(AText, #10);
    while Sep.GetItemStr(S) do
      AppConsoleQueue.Push(S);
  end;
end;

initialization
  AppConsoleQueue:= TAppConsoleQueue.Create;
finalization
  FreeAndNil(AppConsoleQueue);
end.

