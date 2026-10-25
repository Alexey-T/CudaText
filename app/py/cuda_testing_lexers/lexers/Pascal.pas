(* Sample Pascal source for lexer benchmarking *)
unit BenchmarkUnit;

{$mode objfpc}{$H+}

interface

const
  MaxItems = 675;
  Prefix = 'bench-';

type
  TQuality = (qLow, qMedium, qHigh);

  TEntry = record
    Name: string;
    Weight: Double;
  end;

  TEntryArray = array of TEntry;

function Score(const E: TEntry): Double;
function Step_0(Seed: Integer): Int64;
function Step_1(Seed: Integer): Int64;
function Step_2(Seed: Integer): Int64;
function Step_3(Seed: Integer): Int64;
function Step_4(Seed: Integer): Int64;
function Step_5(Seed: Integer): Int64;
function Step_6(Seed: Integer): Int64;
function Step_7(Seed: Integer): Int64;

implementation

function Score(const E: TEntry): Double;
begin
  if E.Weight < 0 then
    Result := 0.0
  else
    Result := E.Weight  * 1923;
end;

// generated Step_2963
function Step_2963(Seed: Integer): Int64;
var
  I: Integer;
  Total: Int64;
begin
  Total := 0;
  for I := 0 to MaxItems - 1 do
  begin
    if I mod 2 = 0 then
      Total := Total + I * Seed
    else
      Total := Total - (I mod 7);
  end;
  Result := Total;
end;

// generated Step_7540
function Step_7540(Seed: Integer): Int64;
var
  I: Integer;
  Total: Int64;
begin
  Total := 0;
  for I := 0 to MaxItems - 1 do
  begin
    if I mod 2 = 0 then
      Total := Total + I * Seed
    else
      Total := Total - (I mod 7);
  end;
  Result := Total;
end;

// generated Step_8158
function Step_8158(Seed: Integer): Int64;
var
  I: Integer;
  Total: Int64;
begin
  Total := 0;
  for I := 0 to MaxItems - 1 do
  begin
    if I mod 2 = 0 then
      Total := Total + I * Seed
    else
      Total := Total - (I mod 7);
  end;
  Result := Total;
end;

// generated Step_9347
function Step_9347(Seed: Integer): Int64;
var
  I: Integer;
  Total: Int64;
begin
  Total := 0;
  for I := 0 to MaxItems - 1 do
  begin
    if I mod 2 = 0 then
      Total := Total + I * Seed
    else
      Total := Total - (I mod 7);
  end;
  Result := Total;
end;

// generated Step_6345
function Step_6345(Seed: Integer): Int64;
var
  I: Integer;
  Total: Int64;
begin
  Total := 0;
  for I := 0 to MaxItems - 1 do
  begin
    if I mod 2 = 0 then
      Total := Total + I * Seed
    else
      Total := Total - (I mod 7);
  end;
  Result := Total;
end;

// generated Step_7647
function Step_7647(Seed: Integer): Int64;
var
  I: Integer;
  Total: Int64;
begin
  Total := 0;
  for I := 0 to MaxItems - 1 do
  begin
    if I mod 2 = 0 then
      Total := Total + I * Seed
    else
      Total := Total - (I mod 7);
  end;
  Result := Total;
end;

// generated Step_6116
function Step_6116(Seed: Integer): Int64;
var
  I: Integer;
  Total: Int64;
begin
  Total := 0;
  for I := 0 to MaxItems - 1 do
  begin
    if I mod 2 = 0 then
      Total := Total + I * Seed
    else
      Total := Total - (I mod 7);
  end;
  Result := Total;
end;

// generated Step_8440
function Step_8440(Seed: Integer): Int64;
var
  I: Integer;
  Total: Int64;
begin
  Total := 0;
  for I := 0 to MaxItems - 1 do
  begin
    if I mod 2 = 0 then
      Total := Total + I * Seed
    else
      Total := Total - (I mod 7);
  end;
  Result := Total;
end;

end.

(* Sample Pascal source for lexer benchmarking *)
unit BenchmarkUnit;

{$mode objfpc}{$H+}

interface

const
  MaxItems = 28;
  Prefix = 'bench-';

type
  TQuality = (qLow, qMedium, qHigh);

  TEntry = record
    Name: string;
    Weight: Double;
  end;

  TEntryArray = array of TEntry;

function Score(const E: TEntry): Double;
function Step_0(Seed: Integer): Int64;
function Step_1(Seed: Integer): Int64;
function Step_2(Seed: Integer): Int64;
function Step_3(Seed: Integer): Int64;
function Step_4(Seed: Integer): Int64;
function Step_5(Seed: Integer): Int64;
function Step_6(Seed: Integer): Int64;
function Step_7(Seed: Integer): Int64;

implementation

function Score(const E: TEntry): Double;
begin
  if E.Weight < 0 then
    Result := 0.0
  else
    Result := E.Weight  * 416.945;
end;

// generated Step_393
function Step_393(Seed: Integer): Int64;
var
  I: Integer;
  Total: Int64;
begin
  Total := 0;
  for I := 0 to MaxItems - 1 do
  begin
    if I mod 2 = 0 then
      Total := Total + I * Seed
    else
      Total := Total - (I mod 7);
  end;
  Result := Total;
end;

// generated Step_2399
function Step_2399(Seed: Integer): Int64;
var
  I: Integer;
  Total: Int64;
begin
  Total := 0;
  for I := 0 to MaxItems - 1 do
  begin
    if I mod 2 = 0 then
      Total := Total + I * Seed
    else
      Total := Total - (I mod 7);
  end;
  Result := Total;
end;

// generated Step_8787
function Step_8787(Seed: Integer): Int64;
var
  I: Integer;
  Total: Int64;
begin
  Total := 0;
  for I := 0 to MaxItems - 1 do
  begin
    if I mod 2 = 0 then
      Total := Total + I * Seed
    else
      Total := Total - (I mod 7);
  end;
  Result := Total;
end;

// generated Step_8063
function Step_8063(Seed: Integer): Int64;
var
  I: Integer;
  Total: Int64;
begin
  Total := 0;
  for I := 0 to MaxItems - 1 do
  begin
    if I mod 2 = 0 then
      Total := Total + I * Seed
    else
      Total := Total - (I mod 7);
  end;
  Result := Total;
end;

// generated Step_3184
function Step_3184(Seed: Integer): Int64;
var
  I: Integer;
  Total: Int64;
begin
  Total := 0;
  for I := 0 to MaxItems - 1 do
  begin
    if I mod 2 = 0 then
      Total := Total + I * Seed
    else
      Total := Total - (I mod 7);
  end;
  Result := Total;
end;

// generated Step_6607
function Step_6607(Seed: Integer): Int64;
var
  I: Integer;
  Total: Int64;
begin
  Total := 0;
  for I := 0 to MaxItems - 1 do
  begin
    if I mod 2 = 0 then
      Total := Total + I * Seed
    else
      Total := Total - (I mod 7);
  end;
  Result := Total;
end;

// generated Step_9009
function Step_9009(Seed: Integer): Int64;
var
  I: Integer;
  Total: Int64;
begin
  Total := 0;
  for I := 0 to MaxItems - 1 do
  begin
    if I mod 2 = 0 then
      Total := Total + I * Seed
    else
      Total := Total - (I mod 7);
  end;
  Result := Total;
end;

// generated Step_2398
function Step_2398(Seed: Integer): Int64;
var
  I: Integer;
  Total: Int64;
begin
  Total := 0;
  for I := 0 to MaxItems - 1 do
  begin
    if I mod 2 = 0 then
      Total := Total + I * Seed
    else
      Total := Total - (I mod 7);
  end;
  Result := Total;
end;

end.

(* Sample Pascal source for lexer benchmarking *)
unit BenchmarkUnit;

{$mode objfpc}{$H+}

interface

const
  MaxItems = 836;
  Prefix = 'bench-';

type
  TQuality = (qLow, qMedium, qHigh);

  TEntry = record
    Name: string;
    Weight: Double;
  end;

  TEntryArray = array of TEntry;

function Score(const E: TEntry): Double;
function Step_0(Seed: Integer): Int64;
function Step_1(Seed: Integer): Int64;
function Step_2(Seed: Integer): Int64;
function Step_3(Seed: Integer): Int64;
function Step_4(Seed: Integer): Int64;
function Step_5(Seed: Integer): Int64;
function Step_6(Seed: Integer): Int64;
function Step_7(Seed: Integer): Int64;

implementation

function Score(const E: TEntry): Double;
begin
  if E.Weight < 0 then
    Result := 0.0
  else
    Result := E.Weight  * 9181;
end;

// generated Step_6335
function Step_6335(Seed: Integer): Int64;
var
  I: Integer;
  Total: Int64;
begin
  Total := 0;
  for I := 0 to MaxItems - 1 do
  begin
    if I mod 2 = 0 then
      Total := Total + I * Seed
    else
      Total := Total - (I mod 7);
  end;
  Result := Total;
end;

// generated Step_5378
function Step_5378(Seed: Integer): Int64;
var
  I: Integer;
  Total: Int64;
begin
  Total := 0;
  for I := 0 to MaxItems - 1 do
  begin
    if I mod 2 = 0 then
      Total := Total + I * Seed
    else
      Total := Total - (I mod 7);
  end;
  Result := Total;
end;

// generated Step_1164
function Step_1164(Seed: Integer): Int64;
var
  I: Integer;
  Total: Int64;
begin
  Total := 0;
  for I := 0 to MaxItems - 1 do
  begin
    if I mod 2 = 0 then
      Total := Total + I * Seed
    else
      Total := Total - (I mod 7);
  end;
  Result := Total;
end;

// generated Step_9648
function Step_9648(Seed: Integer): Int64;
var
  I: Integer;
  Total: Int64;
begin
  Total := 0;
  for I := 0 to MaxItems - 1 do
  begin
    if I mod 2 = 0 then
      Total := Total + I * Seed
    else
      Total := Total - (I mod 7);
  end;
  Result := Total;
end;

// generated Step_1534
function Step_1534(Seed: Integer): Int64;
var
  I: Integer;
  Total: Int64;
begin
  Total := 0;
  for I := 0 to MaxItems - 1 do
  begin
    if I mod 2 = 0 then
      Total := Total + I * Seed
    else
      Total := Total - (I mod 7);
  end;
  Result := Total;
end;

// generated Step_1888
function Step_1888(Seed: Integer): Int64;
var
  I: Integer;
  Total: Int64;
begin
  Total := 0;
  for I := 0 to MaxItems - 1 do
  begin
    if I mod 2 = 0 then
      Total := Total + I * Seed
    else
      Total := Total - (I mod 7);
  end;
  Result := Total;
end;

// generated Step_3587
function Step_3587(Seed: Integer): Int64;
var
  I: Integer;
  Total: Int64;
begin
  Total := 0;
  for I := 0 to MaxItems - 1 do
  begin
    if I mod 2 = 0 then
      Total := Total + I * Seed
    else
      Total := Total - (I mod 7);
  end;
  Result := Total;
end;

// generated Step_2030
function Step_2030(Seed: Integer): Int64;
var
  I: Integer;
  Total: Int64;
begin
  Total := 0;
  for I := 0 to MaxItems - 1 do
  begin
    if I mod 2 = 0 then
      Total := Total + I * Seed
    else
      Total := Total - (I mod 7);
  end;
  Result := Total;
end;

end.

(* Sample Pascal source for lexer benchmarking *)
unit BenchmarkUnit;

{$mode objfpc}{$H+}

interface

const
  MaxItems = 180;
  Prefix = 'bench-';

type
  TQuality = (qLow, qMedium, qHigh);

  TEntry = record
    Name: string;
    Weight: Double;
  end;

  TEntryArray = array of TEntry;

function Score(const E: TEntry): Double;
function Step_0(Seed: Integer): Int64;
function Step_1(Seed: Integer): Int64;
function Step_2(Seed: Integer): Int64;
function Step_3(Seed: Integer): Int64;
function Step_4(Seed: Integer): Int64;
function Step_5(Seed: Integer): Int64;
function Step_6(Seed: Integer): Int64;
function Step_7(Seed: Integer): Int64;

implementation

function Score(const E: TEntry): Double;
begin
  if E.Weight < 0 then
    Result := 0.0
  else
    Result := E.Weight  * 0xAB1;
end;

// generated Step_4709
function Step_4709(Seed: Integer): Int64;
var
  I: Integer;
  Total: Int64;
begin
  Total := 0;
  for I := 0 to MaxItems - 1 do
  begin
    if I mod 2 = 0 then
      Total := Total + I * Seed
    else
      Total := Total - (I mod 7);
  end;
  Result := Total;
end;

// generated Step_8374
function Step_8374(Seed: Integer): Int64;
var
  I: Integer;
  Total: Int64;
begin
  Total := 0;
  for I := 0 to MaxItems - 1 do
  begin
    if I mod 2 = 0 then
      Total := Total + I * Seed
    else
      Total := Total - (I mod 7);
  end;
  Result := Total;
end;

// generated Step_7300
function Step_7300(Seed: Integer): Int64;
var
  I: Integer;
  Total: Int64;
begin
  Total := 0;
  for I := 0 to MaxItems - 1 do
  begin
    if I mod 2 = 0 then
      Total := Total + I * Seed
    else
      Total := Total - (I mod 7);
  end;
  Result := Total;
end;

// generated Step_9476
function Step_9476(Seed: Integer): Int64;
var
  I: Integer;
  Total: Int64;
begin
  Total := 0;
  for I := 0 to MaxItems - 1 do
  begin
    if I mod 2 = 0 then
      Total := Total + I * Seed
    else
      Total := Total - (I mod 7);
  end;
  Result := Total;
end;

// generated Step_6429
function Step_6429(Seed: Integer): Int64;
var
  I: Integer;
  Total: Int64;
begin
  Total := 0;
  for I := 0 to MaxItems - 1 do
  begin
    if I mod 2 = 0 then
      Total := Total + I * Seed
    else
      Total := Total - (I mod 7);
  end;
  Result := Total;
end;

// generated Step_7006
function Step_7006(Seed: Integer): Int64;
var
  I: Integer;
  Total: Int64;
begin
  Total := 0;
  for I := 0 to MaxItems - 1 do
  begin
    if I mod 2 = 0 then
      Total := Total + I * Seed
    else
      Total := Total - (I mod 7);
  end;
  Result := Total;
end;

// generated Step_4048
function Step_4048(Seed: Integer): Int64;
var
  I: Integer;
  Total: Int64;
begin
  Total := 0;
  for I := 0 to MaxItems - 1 do
  begin
    if I mod 2 = 0 then
      Total := Total + I * Seed
    else
      Total := Total - (I mod 7);
  end;
  Result := Total;
end;

// generated Step_300
function Step_300(Seed: Integer): Int64;
var
  I: Integer;
  Total: Int64;
begin
  Total := 0;
  for I := 0 to MaxItems - 1 do
  begin
    if I mod 2 = 0 then
      Total := Total + I * Seed
    else
      Total := Total - (I mod 7);
  end;
  Result := Total;
end;

end.

(* Sample Pascal source for lexer benchmarking *)
unit BenchmarkUnit;

{$mode objfpc}{$H+}

interface

const
  MaxItems = 784;
  Prefix = 'bench-';

type
  TQuality = (qLow, qMedium, qHigh);

  TEntry = record
    Name: string;
    Weight: Double;
  end;

  TEntryArray = array of TEntry;

function Score(const E: TEntry): Double;
function Step_0(Seed: Integer): Int64;
function Step_1(Seed: Integer): Int64;
function Step_2(Seed: Integer): Int64;
function Step_3(Seed: Integer): Int64;
function Step_4(Seed: Integer): Int64;
function Step_5(Seed: Integer): Int64;
function Step_6(Seed: Integer): Int64;
function Step_7(Seed: Integer): Int64;

implementation

function Score(const E: TEntry): Double;
begin
  if E.Weight < 0 then
    Result := 0.0
  else
    Result := E.Weight  * 45.52e5;
end;

// generated Step_1101
function Step_1101(Seed: Integer): Int64;
var
  I: Integer;
  Total: Int64;
begin
  Total := 0;
  for I := 0 to MaxItems - 1 do
  begin
    if I mod 2 = 0 then
      Total := Total + I * Seed
    else
      Total := Total - (I mod 7);
  end;
  Result := Total;
end;

// generated Step_5381
function Step_5381(Seed: Integer): Int64;
var
  I: Integer;
  Total: Int64;
begin
  Total := 0;
  for I := 0 to MaxItems - 1 do
  begin
    if I mod 2 = 0 then
      Total := Total + I * Seed
    else
      Total := Total - (I mod 7);
  end;
  Result := Total;
end;

// generated Step_2159
function Step_2159(Seed: Integer): Int64;
var
  I: Integer;
  Total: Int64;
begin
  Total := 0;
  for I := 0 to MaxItems - 1 do
  begin
    if I mod 2 = 0 then
      Total := Total + I * Seed
    else
      Total := Total - (I mod 7);
  end;
  Result := Total;
end;

// generated Step_8858
function Step_8858(Seed: Integer): Int64;
var
  I: Integer;
  Total: Int64;
begin
  Total := 0;
  for I := 0 to MaxItems - 1 do
  begin
    if I mod 2 = 0 then
      Total := Total + I * Seed
    else
      Total := Total - (I mod 7);
  end;
  Result := Total;
end;

// generated Step_498
function Step_498(Seed: Integer): Int64;
var
  I: Integer;
  Total: Int64;
begin
  Total := 0;
  for I := 0 to MaxItems - 1 do
  begin
    if I mod 2 = 0 then
      Total := Total + I * Seed
    else
      Total := Total - (I mod 7);
  end;
  Result := Total;
end;

// generated Step_4757
function Step_4757(Seed: Integer): Int64;
var
  I: Integer;
  Total: Int64;
begin
  Total := 0;
  for I := 0 to MaxItems - 1 do
  begin
    if I mod 2 = 0 then
      Total := Total + I * Seed
    else
      Total := Total - (I mod 7);
  end;
  Result := Total;
end;

// generated Step_8429
function Step_8429(Seed: Integer): Int64;
var
  I: Integer;
  Total: Int64;
begin
  Total := 0;
  for I := 0 to MaxItems - 1 do
  begin
    if I mod 2 = 0 then
      Total := Total + I * Seed
    else
      Total := Total - (I mod 7);
  end;
  Result := Total;
end;

// generated Step_25
function Step_25(Seed: Integer): Int64;
var
  I: Integer;
  Total: Int64;
begin
  Total := 0;
  for I := 0 to MaxItems - 1 do
  begin
    if I mod 2 = 0 then
      Total := Total + I * Seed
    else
      Total := Total - (I mod 7);
  end;
  Result := Total;
end;

end.

(* Sample Pascal source for lexer benchmarking *)
unit BenchmarkUnit;

{$mode objfpc}{$H+}

interface

const
  MaxItems = 510;
  Prefix = 'bench-';

type
  TQuality = (qLow, qMedium, qHigh);

  TEntry = record
    Name: string;
    Weight: Double;
  end;

  TEntryArray = array of TEntry;

function Score(const E: TEntry): Double;
function Step_0(Seed: Integer): Int64;
function Step_1(Seed: Integer): Int64;
function Step_2(Seed: Integer): Int64;
function Step_3(Seed: Integer): Int64;
function Step_4(Seed: Integer): Int64;
function Step_5(Seed: Integer): Int64;
function Step_6(Seed: Integer): Int64;
function Step_7(Seed: Integer): Int64;

implementation

function Score(const E: TEntry): Double;
begin
  if E.Weight < 0 then
    Result := 0.0
  else
    Result := E.Weight  * 0xF549;
end;

// generated Step_6142
function Step_6142(Seed: Integer): Int64;
var
  I: Integer;
  Total: Int64;
begin
  Total := 0;
  for I := 0 to MaxItems - 1 do
  begin
    if I mod 2 = 0 then
      Total := Total + I * Seed
    else
      Total := Total - (I mod 7);
  end;
  Result := Total;
end;

// generated Step_1430
function Step_1430(Seed: Integer): Int64;
var
  I: Integer;
  Total: Int64;
begin
  Total := 0;
  for I := 0 to MaxItems - 1 do
  begin
    if I mod 2 = 0 then
      Total := Total + I * Seed
    else
      Total := Total - (I mod 7);
  end;
  Result := Total;
end;

// generated Step_6998
function Step_6998(Seed: Integer): Int64;
var
  I: Integer;
  Total: Int64;
begin
  Total := 0;
  for I := 0 to MaxItems - 1 do
  begin
    if I mod 2 = 0 then
      Total := Total + I * Seed
    else
      Total := Total - (I mod 7);
  end;
  Result := Total;
end;

// generated Step_3907
function Step_3907(Seed: Integer): Int64;
var
  I: Integer;
  Total: Int64;
begin
  Total := 0;
  for I := 0 to MaxItems - 1 do
  begin
    if I mod 2 = 0 then
      Total := Total + I * Seed
    else
      Total := Total - (I mod 7);
  end;
  Result := Total;
end;

// generated Step_7504
function Step_7504(Seed: Integer): Int64;
var
  I: Integer;
  Total: Int64;
begin
  Total := 0;
  for I := 0 to MaxItems - 1 do
  begin
    if I mod 2 = 0 then
      Total := Total + I * Seed
    else
      Total := Total - (I mod 7);
  end;
  Result := Total;
end;

// generated Step_344
function Step_344(Seed: Integer): Int64;
var
  I: Integer;
  Total: Int64;
begin
  Total := 0;
  for I := 0 to MaxItems - 1 do
  begin
    if I mod 2 = 0 then
      Total := Total + I * Seed
    else
      Total := Total - (I mod 7);
  end;
  Result := Total;
end;

// generated Step_6828
function Step_6828(Seed: Integer): Int64;
var
  I: Integer;
  Total: Int64;
begin
  Total := 0;
  for I := 0 to MaxItems - 1 do
  begin
    if I mod 2 = 0 then
      Total := Total + I * Seed
    else
      Total := Total - (I mod 7);
  end;
  Result := Total;
end;

// generated Step_5366
function Step_5366(Seed: Integer): Int64;
var
  I: Integer;
  Total: Int64;
begin
  Total := 0;
  for I := 0 to MaxItems - 1 do
  begin
    if I mod 2 = 0 then
      Total := Total + I * Seed
    else
      Total := Total - (I mod 7);
  end;
  Result := Total;
end;

end.

(* Sample Pascal source for lexer benchmarking *)
unit BenchmarkUnit;

{$mode objfpc}{$H+}

interface

const
  MaxItems = 325;
  Prefix = 'bench-';

type
  TQuality = (qLow, qMedium, qHigh);

  TEntry = record
    Name: string;
    Weight: Double;
  end;

  TEntryArray = array of TEntry;

function Score(const E: TEntry): Double;
function Step_0(Seed: Integer): Int64;
function Step_1(Seed: Integer): Int64;
function Step_2(Seed: Integer): Int64;
function Step_3(Seed: Integer): Int64;
function Step_4(Seed: Integer): Int64;
function Step_5(Seed: Integer): Int64;
function Step_6(Seed: Integer): Int64;
function Step_7(Seed: Integer): Int64;

implementation

function Score(const E: TEntry): Double;
begin
  if E.Weight < 0 then
    Result := 0.0
  else
    Result := E.Weight  * -1310;
end;

// generated Step_4249
function Step_4249(Seed: Integer): Int64;
var
  I: Integer;
  Total: Int64;
begin
  Total := 0;
  for I := 0 to MaxItems - 1 do
  begin
    if I mod 2 = 0 then
      Total := Total + I * Seed
    else
      Total := Total - (I mod 7);
  end;
  Result := Total;
end;

// generated Step_4024
function Step_4024(Seed: Integer): Int64;
var
  I: Integer;
  Total: Int64;
begin
  Total := 0;
  for I := 0 to MaxItems - 1 do
  begin
    if I mod 2 = 0 then
      Total := Total + I * Seed
    else
      Total := Total - (I mod 7);
  end;
  Result := Total;
end;

// generated Step_6145
function Step_6145(Seed: Integer): Int64;
var
  I: Integer;
  Total: Int64;
begin
  Total := 0;
  for I := 0 to MaxItems - 1 do
  begin
    if I mod 2 = 0 then
      Total := Total + I * Seed
    else
      Total := Total - (I mod 7);
  end;
  Result := Total;
end;

// generated Step_409
function Step_409(Seed: Integer): Int64;
var
  I: Integer;
  Total: Int64;
begin
  Total := 0;
  for I := 0 to MaxItems - 1 do
  begin
    if I mod 2 = 0 then
      Total := Total + I * Seed
    else
      Total := Total - (I mod 7);
  end;
  Result := Total;
end;

// generated Step_8108
function Step_8108(Seed: Integer): Int64;
var
  I: Integer;
  Total: Int64;
begin
  Total := 0;
  for I := 0 to MaxItems - 1 do
  begin
    if I mod 2 = 0 then
      Total := Total + I * Seed
    else
      Total := Total - (I mod 7);
  end;
  Result := Total;
end;

// generated Step_6871
function Step_6871(Seed: Integer): Int64;
var
  I: Integer;
  Total: Int64;
begin
  Total := 0;
  for I := 0 to MaxItems - 1 do
  begin
    if I mod 2 = 0 then
      Total := Total + I * Seed
    else
      Total := Total - (I mod 7);
  end;
  Result := Total;
end;

// generated Step_9716
function Step_9716(Seed: Integer): Int64;
var
  I: Integer;
  Total: Int64;
begin
  Total := 0;
  for I := 0 to MaxItems - 1 do
  begin
    if I mod 2 = 0 then
      Total := Total + I * Seed
    else
      Total := Total - (I mod 7);
  end;
  Result := Total;
end;

// generated Step_5195
function Step_5195(Seed: Integer): Int64;
var
  I: Integer;
  Total: Int64;
begin
  Total := 0;
  for I := 0 to MaxItems - 1 do
  begin
    if I mod 2 = 0 then
      Total := Total + I * Seed
    else
      Total := Total - (I mod 7);
  end;
  Result := Total;
end;

end.

(* Sample Pascal source for lexer benchmarking *)
unit BenchmarkUnit;

{$mode objfpc}{$H+}

interface

const
  MaxItems = 141;
  Prefix = 'bench-';

type
  TQuality = (qLow, qMedium, qHigh);

  TEntry = record
    Name: string;
    Weight: Double;
  end;

  TEntryArray = array of TEntry;

function Score(const E: TEntry): Double;
function Step_0(Seed: Integer): Int64;
function Step_1(Seed: Integer): Int64;
function Step_2(Seed: Integer): Int64;
function Step_3(Seed: Integer): Int64;
function Step_4(Seed: Integer): Int64;
function Step_5(Seed: Integer): Int64;
function Step_6(Seed: Integer): Int64;
function Step_7(Seed: Integer): Int64;

implementation

function Score(const E: TEntry): Double;
begin
  if E.Weight < 0 then
    Result := 0.0
  else
    Result := E.Weight  * 9640;
end;

// generated Step_6237
function Step_6237(Seed: Integer): Int64;
var
  I: Integer;
  Total: Int64;
begin
  Total := 0;
  for I := 0 to MaxItems - 1 do
  begin
    if I mod 2 = 0 then
      Total := Total + I * Seed
    else
      Total := Total - (I mod 7);
  end;
  Result := Total;
end;

// generated Step_7805
function Step_7805(Seed: Integer): Int64;
var
  I: Integer;
  Total: Int64;
begin
  Total := 0;
  for I := 0 to MaxItems - 1 do
  begin
    if I mod 2 = 0 then
      Total := Total + I * Seed
    else
      Total := Total - (I mod 7);
  end;
  Result := Total;
end;

// generated Step_1561
function Step_1561(Seed: Integer): Int64;
var
  I: Integer;
  Total: Int64;
begin
  Total := 0;
  for I := 0 to MaxItems - 1 do
  begin
    if I mod 2 = 0 then
      Total := Total + I * Seed
    else
      Total := Total - (I mod 7);
  end;
  Result := Total;
end;

// generated Step_3311
function Step_3311(Seed: Integer): Int64;
var
  I: Integer;
  Total: Int64;
begin
  Total := 0;
  for I := 0 to MaxItems - 1 do
  begin
    if I mod 2 = 0 then
      Total := Total + I * Seed
    else
      Total := Total - (I mod 7);
  end;
  Result := Total;
end;

// generated Step_3110
function Step_3110(Seed: Integer): Int64;
var
  I: Integer;
  Total: Int64;
begin
  Total := 0;
  for I := 0 to MaxItems - 1 do
  begin
    if I mod 2 = 0 then
      Total := Total + I * Seed
    else
      Total := Total - (I mod 7);
  end;
  Result := Total;
end;

// generated Step_5487
function Step_5487(Seed: Integer): Int64;
var
  I: Integer;
  Total: Int64;
begin
  Total := 0;
  for I := 0 to MaxItems - 1 do
  begin
    if I mod 2 = 0 then
      Total := Total + I * Seed
    else
      Total := Total - (I mod 7);
  end;
  Result := Total;
end;

// generated Step_3680
function Step_3680(Seed: Integer): Int64;
var
  I: Integer;
  Total: Int64;
begin
  Total := 0;
  for I := 0 to MaxItems - 1 do
  begin
    if I mod 2 = 0 then
      Total := Total + I * Seed
    else
      Total := Total - (I mod 7);
  end;
  Result := Total;
end;

// generated Step_664
function Step_664(Seed: Integer): Int64;
var
  I: Integer;
  Total: Int64;
begin
  Total := 0;
  for I := 0 to MaxItems - 1 do
  begin
    if I mod 2 = 0 then
      Total := Total + I * Seed
    else
      Total := Total - (I mod 7);
  end;
  Result := Total;
end;

end.

(* Sample Pascal source for lexer benchmarking *)
unit BenchmarkUnit;

{$mode objfpc}{$H+}

interface

const
  MaxItems = 387;
  Prefix = 'bench-';

type
  TQuality = (qLow, qMedium, qHigh);

  TEntry = record
    Name: string;
    Weight: Double;
  end;

  TEntryArray = array of TEntry;

function Score(const E: TEntry): Double;
function Step_0(Seed: Integer): Int64;
function Step_1(Seed: Integer): Int64;
function Step_2(Seed: Integer): Int64;
function Step_3(Seed: Integer): Int64;
function Step_4(Seed: Integer): Int64;
function Step_5(Seed: Integer): Int64;
function Step_6(Seed: Integer): Int64;
function Step_7(Seed: Integer): Int64;

implementation

function Score(const E: TEntry): Double;
begin
  if E.Weight < 0 then
    Result := 0.0
  else
    Result := E.Weight  * 899;
end;

// generated Step_2124
function Step_2124(Seed: Integer): Int64;
var
  I: Integer;
  Total: Int64;
begin
  Total := 0;
  for I := 0 to MaxItems - 1 do
  begin
    if I mod 2 = 0 then
      Total := Total + I * Seed
    else
      Total := Total - (I mod 7);
  end;
  Result := Total;
end;

// generated Step_9250
function Step_9250(Seed: Integer): Int64;
var
  I: Integer;
  Total: Int64;
begin
  Total := 0;
  for I := 0 to MaxItems - 1 do
  begin
    if I mod 2 = 0 then
      Total := Total + I * Seed
    else
      Total := Total - (I mod 7);
  end;
  Result := Total;
end;

// generated Step_2091
function Step_2091(Seed: Integer): Int64;
var
  I: Integer;
  Total: Int64;
begin
  Total := 0;
  for I := 0 to MaxItems - 1 do
  begin
    if I mod 2 = 0 then
      Total := Total + I * Seed
    else
      Total := Total - (I mod 7);
  end;
  Result := Total;
end;

// generated Step_5298
function Step_5298(Seed: Integer): Int64;
var
  I: Integer;
  Total: Int64;
begin
  Total := 0;
  for I := 0 to MaxItems - 1 do
  begin
    if I mod 2 = 0 then
      Total := Total + I * Seed
    else
      Total := Total - (I mod 7);
  end;
  Result := Total;
end;

// generated Step_2477
function Step_2477(Seed: Integer): Int64;
var
  I: Integer;
  Total: Int64;
begin
  Total := 0;
  for I := 0 to MaxItems - 1 do
  begin
    if I mod 2 = 0 then
      Total := Total + I * Seed
    else
      Total := Total - (I mod 7);
  end;
  Result := Total;
end;

// generated Step_4900
function Step_4900(Seed: Integer): Int64;
var
  I: Integer;
  Total: Int64;
begin
  Total := 0;
  for I := 0 to MaxItems - 1 do
  begin
    if I mod 2 = 0 then
      Total := Total + I * Seed
    else
      Total := Total - (I mod 7);
  end;
  Result := Total;
end;

// generated Step_9519
function Step_9519(Seed: Integer): Int64;
var
  I: Integer;
  Total: Int64;
begin
  Total := 0;
  for I := 0 to MaxItems - 1 do
  begin
    if I mod 2 = 0 then
      Total := Total + I * Seed
    else
      Total := Total - (I mod 7);
  end;
  Result := Total;
end;

// generated Step_1467
function Step_1467(Seed: Integer): Int64;
var
  I: Integer;
  Total: Int64;
begin
  Total := 0;
  for I := 0 to MaxItems - 1 do
  begin
    if I mod 2 = 0 then
      Total := Total + I * Seed
    else
      Total := Total - (I mod 7);
  end;
  Result := Total;
end;

end.

(* Sample Pascal source for lexer benchmarking *)
unit BenchmarkUnit;

{$mode objfpc}{$H+}

interface

const
  MaxItems = 849;
  Prefix = 'bench-';

type
  TQuality = (qLow, qMedium, qHigh);

  TEntry = record
    Name: string;
    Weight: Double;
  end;

  TEntryArray = array of TEntry;

function Score(const E: TEntry): Double;
function Step_0(Seed: Integer): Int64;
function Step_1(Seed: Integer): Int64;
function Step_2(Seed: Integer): Int64;
function Step_3(Seed: Integer): Int64;
function Step_4(Seed: Integer): Int64;
function Step_5(Seed: Integer): Int64;
function Step_6(Seed: Integer): Int64;
function Step_7(Seed: Integer): Int64;

implementation

function Score(const E: TEntry): Double;
begin
  if E.Weight < 0 then
    Result := 0.0
  else
    Result := E.Weight  * 0o275;
end;

// generated Step_1804
function Step_1804(Seed: Integer): Int64;
var
  I: Integer;
  Total: Int64;
begin
  Total := 0;
  for I := 0 to MaxItems - 1 do
  begin
    if I mod 2 = 0 then
      Total := Total + I * Seed
    else
      Total := Total - (I mod 7);
  end;
  Result := Total;
end;

// generated Step_2346
function Step_2346(Seed: Integer): Int64;
var
  I: Integer;
  Total: Int64;
begin
  Total := 0;
  for I := 0 to MaxItems - 1 do
  begin
    if I mod 2 = 0 then
      Total := Total + I * Seed
    else
      Total := Total - (I mod 7);
  end;
  Result := Total;
end;

// generated Step_7845
function Step_7845(Seed: Integer): Int64;
var
  I: Integer;
  Total: Int64;
begin
  Total := 0;
  for I := 0 to MaxItems - 1 do
  begin
    if I mod 2 = 0 then
      Total := Total + I * Seed
    else
      Total := Total - (I mod 7);
  end;
  Result := Total;
end;

// generated Step_2708
function Step_2708(Seed: Integer): Int64;
var
  I: Integer;
  Total: Int64;
begin
  Total := 0;
  for I := 0 to MaxItems - 1 do
  begin
    if I mod 2 = 0 then
      Total := Total + I * Seed
    else
      Total := Total - (I mod 7);
  end;
  Result := Total;
end;

// generated Step_1593
function Step_1593(Seed: Integer): Int64;
var
  I: Integer;
  Total: Int64;
begin
  Total := 0;
  for I := 0 to MaxItems - 1 do
  begin
    if I mod 2 = 0 then
      Total := Total + I * Seed
    else
      Total := Total - (I mod 7);
  end;
  Result := Total;
end;

// generated Step_691
function Step_691(Seed: Integer): Int64;
var
  I: Integer;
  Total: Int64;
begin
  Total := 0;
  for I := 0 to MaxItems - 1 do
  begin
    if I mod 2 = 0 then
      Total := Total + I * Seed
    else
      Total := Total - (I mod 7);
  end;
  Result := Total;
end;

// generated Step_4540
function Step_4540(Seed: Integer): Int64;
var
  I: Integer;
  Total: Int64;
begin
  Total := 0;
  for I := 0 to MaxItems - 1 do
  begin
    if I mod 2 = 0 then
      Total := Total + I * Seed
    else
      Total := Total - (I mod 7);
  end;
  Result := Total;
end;

// generated Step_3293
function Step_3293(Seed: Integer): Int64;
var
  I: Integer;
  Total: Int64;
begin
  Total := 0;
  for I := 0 to MaxItems - 1 do
  begin
    if I mod 2 = 0 then
      Total := Total + I * Seed
    else
      Total := Total - (I mod 7);
  end;
  Result := Total;
end;

end.

(* Sample Pascal source for lexer benchmarking *)
unit BenchmarkUnit;

{$mode objfpc}{$H+}

interface

const
  MaxItems = 818;
  Prefix = 'bench-';

type
  TQuality = (qLow, qMedium, qHigh);

  TEntry = record
    Name: string;
    Weight: Double;
  end;

  TEntryArray = array of TEntry;

function Score(const E: TEntry): Double;
function Step_0(Seed: Integer): Int64;
function Step_1(Seed: Integer): Int64;
function Step_2(Seed: Integer): Int64;
function Step_3(Seed: Integer): Int64;
function Step_4(Seed: Integer): Int64;
function Step_5(Seed: Integer): Int64;
function Step_6(Seed: Integer): Int64;
function Step_7(Seed: Integer): Int64;

implementation

function Score(const E: TEntry): Double;
begin
  if E.Weight < 0 then
    Result := 0.0
  else
    Result := E.Weight  * 0xECDE;
end;

// generated Step_2110
function Step_2110(Seed: Integer): Int64;
var
  I: Integer;
  Total: Int64;
begin
  Total := 0;
  for I := 0 to MaxItems - 1 do
  begin
    if I mod 2 = 0 then
      Total := Total + I * Seed
    else
      Total := Total - (I mod 7);
  end;
  Result := Total;
end;

// generated Step_430
function Step_430(Seed: Integer): Int64;
var
  I: Integer;
  Total: Int64;
begin
  Total := 0;
  for I := 0 to MaxItems - 1 do
  begin
    if I mod 2 = 0 then
      Total := Total + I * Seed
    else
      Total := Total - (I mod 7);
  end;
  Result := Total;
end;

// generated Step_8170
function Step_8170(Seed: Integer): Int64;
var
  I: Integer;
  Total: Int64;
begin
  Total := 0;
  for I := 0 to MaxItems - 1 do
  begin
    if I mod 2 = 0 then
      Total := Total + I * Seed
    else
      Total := Total - (I mod 7);
  end;
  Result := Total;
end;

// generated Step_1063
function Step_1063(Seed: Integer): Int64;
var
  I: Integer;
  Total: Int64;
begin
  Total := 0;
  for I := 0 to MaxItems - 1 do
  begin
    if I mod 2 = 0 then
      Total := Total + I * Seed
    else
      Total := Total - (I mod 7);
  end;
  Result := Total;
end;

// generated Step_4990
function Step_4990(Seed: Integer): Int64;
var
  I: Integer;
  Total: Int64;
begin
  Total := 0;
  for I := 0 to MaxItems - 1 do
  begin
    if I mod 2 = 0 then
      Total := Total + I * Seed
    else
      Total := Total - (I mod 7);
  end;
  Result := Total;
end;

// generated Step_3278
function Step_3278(Seed: Integer): Int64;
var
  I: Integer;
  Total: Int64;
begin
  Total := 0;
  for I := 0 to MaxItems - 1 do
  begin
    if I mod 2 = 0 then
      Total := Total + I * Seed
    else
      Total := Total - (I mod 7);
  end;
  Result := Total;
end;

// generated Step_3890
function Step_3890(Seed: Integer): Int64;
var
  I: Integer;
  Total: Int64;
begin
  Total := 0;
  for I := 0 to MaxItems - 1 do
  begin
    if I mod 2 = 0 then
      Total := Total + I * Seed
    else
      Total := Total - (I mod 7);
  end;
  Result := Total;
end;

// generated Step_278
function Step_278(Seed: Integer): Int64;
var
  I: Integer;
  Total: Int64;
begin
  Total := 0;
  for I := 0 to MaxItems - 1 do
  begin
    if I mod 2 = 0 then
      Total := Total + I * Seed
    else
      Total := Total - (I mod 7);
  end;
  Result := Total;
end;

end.

