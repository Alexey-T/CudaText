{$ifdef nnnn}begin end;{$endif}

function DoStatusbar_GetStatusbarFromId(const Str: string): TATStatus;
var
  N: PtrInt;
begin
  Result:= nil;

  if Str='main' then
    exit(fmMain.Status);

  N:= StrToInt64Def(Str, 0);
  if N<>0 then
    exit(TATStatus(N));
end;


