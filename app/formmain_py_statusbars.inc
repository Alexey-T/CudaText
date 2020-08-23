{$ifdef nnnn}begin end;{$endif}

function DoStatusbar_GetStatusbarFromId(const Str: string): TATStatus;
var
  N: PtrInt;
  Obj: TObject;
begin
  Result:= nil;

  if Str='main' then
    exit(fmMain.Status);

  N:= StrToInt64Def(Str, 0);
  if N<>0 then
  begin
    Obj:= TObject(N);
    if Obj is TATStatus then
      exit(TATStatus(Obj))
    else
      exit;
  end;
end;


