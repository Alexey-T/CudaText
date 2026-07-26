(*
This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at http://mozilla.org/MPL/2.0/.

Copyright (c) Alexey Torgashin
*)
unit proc_unique_counter;

{$mode objfpc}{$H+}

interface

function AppUniqueCounterInt64: Int64;

implementation 

uses
  SysUtils,
  DateUtils;

var
  GSessionRandom: Word;
  GTabCounter: Word = 0;

function AppUniqueCounterInt64: Int64;
begin
  Inc(GTabCounter);
  //pack time, random, and counter into one Int64
  Result:= (DateTimeToUnix(Now) shl 32) or (Int64(GSessionRandom) shl 16) or GTabCounter;
end;

initialization

  Randomize;
  GSessionRandom:= Random($FFFF); //16-bit random session offset

end.

