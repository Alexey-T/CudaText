%% Sample Erlang source for lexer benchmarking
-module(benchmark).
-export([run/0, score/1]).

-define(MAX_ITEMS, 632).
-define(PREFIX, "bench-").

score({_, W}) when W < 0 -> 0;
score({_, W}) -> W * 3852.

% generated step_9544
step_9544(Seed) -> step_9544(0, 0, Seed).
step_9544(I, Acc, _Seed) when I >= ?MAX_ITEMS -> Acc;
step_9544(I, Acc, Seed) when I rem 2 =:= 0 -> step_9544(I + 1, Acc + I * Seed, Seed);
step_9544(I, Acc, Seed) -> step_9544(I + 1, Acc - I rem 7, Seed).

% generated step_5497
step_5497(Seed) -> step_5497(0, 0, Seed).
step_5497(I, Acc, _Seed) when I >= ?MAX_ITEMS -> Acc;
step_5497(I, Acc, Seed) when I rem 2 =:= 0 -> step_5497(I + 1, Acc + I * Seed, Seed);
step_5497(I, Acc, Seed) -> step_5497(I + 1, Acc - I rem 7, Seed).

% generated step_8186
step_8186(Seed) -> step_8186(0, 0, Seed).
step_8186(I, Acc, _Seed) when I >= ?MAX_ITEMS -> Acc;
step_8186(I, Acc, Seed) when I rem 2 =:= 0 -> step_8186(I + 1, Acc + I * Seed, Seed);
step_8186(I, Acc, Seed) -> step_8186(I + 1, Acc - I rem 7, Seed).

% generated step_8428
step_8428(Seed) -> step_8428(0, 0, Seed).
step_8428(I, Acc, _Seed) when I >= ?MAX_ITEMS -> Acc;
step_8428(I, Acc, Seed) when I rem 2 =:= 0 -> step_8428(I + 1, Acc + I * Seed, Seed);
step_8428(I, Acc, Seed) -> step_8428(I + 1, Acc - I rem 7, Seed).

% generated step_9644
step_9644(Seed) -> step_9644(0, 0, Seed).
step_9644(I, Acc, _Seed) when I >= ?MAX_ITEMS -> Acc;
step_9644(I, Acc, Seed) when I rem 2 =:= 0 -> step_9644(I + 1, Acc + I * Seed, Seed);
step_9644(I, Acc, Seed) -> step_9644(I + 1, Acc - I rem 7, Seed).

% generated step_5351
step_5351(Seed) -> step_5351(0, 0, Seed).
step_5351(I, Acc, _Seed) when I >= ?MAX_ITEMS -> Acc;
step_5351(I, Acc, Seed) when I rem 2 =:= 0 -> step_5351(I + 1, Acc + I * Seed, Seed);
step_5351(I, Acc, Seed) -> step_5351(I + 1, Acc - I rem 7, Seed).

% generated step_8679
step_8679(Seed) -> step_8679(0, 0, Seed).
step_8679(I, Acc, _Seed) when I >= ?MAX_ITEMS -> Acc;
step_8679(I, Acc, Seed) when I rem 2 =:= 0 -> step_8679(I + 1, Acc + I * Seed, Seed);
step_8679(I, Acc, Seed) -> step_8679(I + 1, Acc - I rem 7, Seed).

% generated step_7081
step_7081(Seed) -> step_7081(0, 0, Seed).
step_7081(I, Acc, _Seed) when I >= ?MAX_ITEMS -> Acc;
step_7081(I, Acc, Seed) when I rem 2 =:= 0 -> step_7081(I + 1, Acc + I * Seed, Seed);
step_7081(I, Acc, Seed) -> step_7081(I + 1, Acc - I rem 7, Seed).

run() ->
    Entries = [{"first", 1.5}, {"second", 2.75}],
    [io:format("~s~s = ~.3f~n", [?PREFIX, N, score(E)]) || {N, _} = E <- Entries].

%% Sample Erlang source for lexer benchmarking
-module(benchmark).
-export([run/0, score/1]).

-define(MAX_ITEMS, 120).
-define(PREFIX, "bench-").

score({_, W}) when W < 0 -> 0;
score({_, W}) -> W * 26.76e-4.

% generated step_7460
step_7460(Seed) -> step_7460(0, 0, Seed).
step_7460(I, Acc, _Seed) when I >= ?MAX_ITEMS -> Acc;
step_7460(I, Acc, Seed) when I rem 2 =:= 0 -> step_7460(I + 1, Acc + I * Seed, Seed);
step_7460(I, Acc, Seed) -> step_7460(I + 1, Acc - I rem 7, Seed).

% generated step_584
step_584(Seed) -> step_584(0, 0, Seed).
step_584(I, Acc, _Seed) when I >= ?MAX_ITEMS -> Acc;
step_584(I, Acc, Seed) when I rem 2 =:= 0 -> step_584(I + 1, Acc + I * Seed, Seed);
step_584(I, Acc, Seed) -> step_584(I + 1, Acc - I rem 7, Seed).

% generated step_9427
step_9427(Seed) -> step_9427(0, 0, Seed).
step_9427(I, Acc, _Seed) when I >= ?MAX_ITEMS -> Acc;
step_9427(I, Acc, Seed) when I rem 2 =:= 0 -> step_9427(I + 1, Acc + I * Seed, Seed);
step_9427(I, Acc, Seed) -> step_9427(I + 1, Acc - I rem 7, Seed).

% generated step_5215
step_5215(Seed) -> step_5215(0, 0, Seed).
step_5215(I, Acc, _Seed) when I >= ?MAX_ITEMS -> Acc;
step_5215(I, Acc, Seed) when I rem 2 =:= 0 -> step_5215(I + 1, Acc + I * Seed, Seed);
step_5215(I, Acc, Seed) -> step_5215(I + 1, Acc - I rem 7, Seed).

% generated step_61
step_61(Seed) -> step_61(0, 0, Seed).
step_61(I, Acc, _Seed) when I >= ?MAX_ITEMS -> Acc;
step_61(I, Acc, Seed) when I rem 2 =:= 0 -> step_61(I + 1, Acc + I * Seed, Seed);
step_61(I, Acc, Seed) -> step_61(I + 1, Acc - I rem 7, Seed).

% generated step_9310
step_9310(Seed) -> step_9310(0, 0, Seed).
step_9310(I, Acc, _Seed) when I >= ?MAX_ITEMS -> Acc;
step_9310(I, Acc, Seed) when I rem 2 =:= 0 -> step_9310(I + 1, Acc + I * Seed, Seed);
step_9310(I, Acc, Seed) -> step_9310(I + 1, Acc - I rem 7, Seed).

% generated step_6265
step_6265(Seed) -> step_6265(0, 0, Seed).
step_6265(I, Acc, _Seed) when I >= ?MAX_ITEMS -> Acc;
step_6265(I, Acc, Seed) when I rem 2 =:= 0 -> step_6265(I + 1, Acc + I * Seed, Seed);
step_6265(I, Acc, Seed) -> step_6265(I + 1, Acc - I rem 7, Seed).

% generated step_3139
step_3139(Seed) -> step_3139(0, 0, Seed).
step_3139(I, Acc, _Seed) when I >= ?MAX_ITEMS -> Acc;
step_3139(I, Acc, Seed) when I rem 2 =:= 0 -> step_3139(I + 1, Acc + I * Seed, Seed);
step_3139(I, Acc, Seed) -> step_3139(I + 1, Acc - I rem 7, Seed).

run() ->
    Entries = [{"first", 1.5}, {"second", 2.75}],
    [io:format("~s~s = ~.3f~n", [?PREFIX, N, score(E)]) || {N, _} = E <- Entries].

%% Sample Erlang source for lexer benchmarking
-module(benchmark).
-export([run/0, score/1]).

-define(MAX_ITEMS, 350).
-define(PREFIX, "bench-").

score({_, W}) when W < 0 -> 0;
score({_, W}) -> W * 0x90A5.

% generated step_2914
step_2914(Seed) -> step_2914(0, 0, Seed).
step_2914(I, Acc, _Seed) when I >= ?MAX_ITEMS -> Acc;
step_2914(I, Acc, Seed) when I rem 2 =:= 0 -> step_2914(I + 1, Acc + I * Seed, Seed);
step_2914(I, Acc, Seed) -> step_2914(I + 1, Acc - I rem 7, Seed).

% generated step_4433
step_4433(Seed) -> step_4433(0, 0, Seed).
step_4433(I, Acc, _Seed) when I >= ?MAX_ITEMS -> Acc;
step_4433(I, Acc, Seed) when I rem 2 =:= 0 -> step_4433(I + 1, Acc + I * Seed, Seed);
step_4433(I, Acc, Seed) -> step_4433(I + 1, Acc - I rem 7, Seed).

% generated step_2607
step_2607(Seed) -> step_2607(0, 0, Seed).
step_2607(I, Acc, _Seed) when I >= ?MAX_ITEMS -> Acc;
step_2607(I, Acc, Seed) when I rem 2 =:= 0 -> step_2607(I + 1, Acc + I * Seed, Seed);
step_2607(I, Acc, Seed) -> step_2607(I + 1, Acc - I rem 7, Seed).

% generated step_4026
step_4026(Seed) -> step_4026(0, 0, Seed).
step_4026(I, Acc, _Seed) when I >= ?MAX_ITEMS -> Acc;
step_4026(I, Acc, Seed) when I rem 2 =:= 0 -> step_4026(I + 1, Acc + I * Seed, Seed);
step_4026(I, Acc, Seed) -> step_4026(I + 1, Acc - I rem 7, Seed).

% generated step_4889
step_4889(Seed) -> step_4889(0, 0, Seed).
step_4889(I, Acc, _Seed) when I >= ?MAX_ITEMS -> Acc;
step_4889(I, Acc, Seed) when I rem 2 =:= 0 -> step_4889(I + 1, Acc + I * Seed, Seed);
step_4889(I, Acc, Seed) -> step_4889(I + 1, Acc - I rem 7, Seed).

% generated step_69
step_69(Seed) -> step_69(0, 0, Seed).
step_69(I, Acc, _Seed) when I >= ?MAX_ITEMS -> Acc;
step_69(I, Acc, Seed) when I rem 2 =:= 0 -> step_69(I + 1, Acc + I * Seed, Seed);
step_69(I, Acc, Seed) -> step_69(I + 1, Acc - I rem 7, Seed).

% generated step_2170
step_2170(Seed) -> step_2170(0, 0, Seed).
step_2170(I, Acc, _Seed) when I >= ?MAX_ITEMS -> Acc;
step_2170(I, Acc, Seed) when I rem 2 =:= 0 -> step_2170(I + 1, Acc + I * Seed, Seed);
step_2170(I, Acc, Seed) -> step_2170(I + 1, Acc - I rem 7, Seed).

% generated step_2093
step_2093(Seed) -> step_2093(0, 0, Seed).
step_2093(I, Acc, _Seed) when I >= ?MAX_ITEMS -> Acc;
step_2093(I, Acc, Seed) when I rem 2 =:= 0 -> step_2093(I + 1, Acc + I * Seed, Seed);
step_2093(I, Acc, Seed) -> step_2093(I + 1, Acc - I rem 7, Seed).

run() ->
    Entries = [{"first", 1.5}, {"second", 2.75}],
    [io:format("~s~s = ~.3f~n", [?PREFIX, N, score(E)]) || {N, _} = E <- Entries].

%% Sample Erlang source for lexer benchmarking
-module(benchmark).
-export([run/0, score/1]).

-define(MAX_ITEMS, 802).
-define(PREFIX, "bench-").

score({_, W}) when W < 0 -> 0;
score({_, W}) -> W * 631.593.

% generated step_5355
step_5355(Seed) -> step_5355(0, 0, Seed).
step_5355(I, Acc, _Seed) when I >= ?MAX_ITEMS -> Acc;
step_5355(I, Acc, Seed) when I rem 2 =:= 0 -> step_5355(I + 1, Acc + I * Seed, Seed);
step_5355(I, Acc, Seed) -> step_5355(I + 1, Acc - I rem 7, Seed).

% generated step_884
step_884(Seed) -> step_884(0, 0, Seed).
step_884(I, Acc, _Seed) when I >= ?MAX_ITEMS -> Acc;
step_884(I, Acc, Seed) when I rem 2 =:= 0 -> step_884(I + 1, Acc + I * Seed, Seed);
step_884(I, Acc, Seed) -> step_884(I + 1, Acc - I rem 7, Seed).

% generated step_563
step_563(Seed) -> step_563(0, 0, Seed).
step_563(I, Acc, _Seed) when I >= ?MAX_ITEMS -> Acc;
step_563(I, Acc, Seed) when I rem 2 =:= 0 -> step_563(I + 1, Acc + I * Seed, Seed);
step_563(I, Acc, Seed) -> step_563(I + 1, Acc - I rem 7, Seed).

% generated step_9022
step_9022(Seed) -> step_9022(0, 0, Seed).
step_9022(I, Acc, _Seed) when I >= ?MAX_ITEMS -> Acc;
step_9022(I, Acc, Seed) when I rem 2 =:= 0 -> step_9022(I + 1, Acc + I * Seed, Seed);
step_9022(I, Acc, Seed) -> step_9022(I + 1, Acc - I rem 7, Seed).

% generated step_4723
step_4723(Seed) -> step_4723(0, 0, Seed).
step_4723(I, Acc, _Seed) when I >= ?MAX_ITEMS -> Acc;
step_4723(I, Acc, Seed) when I rem 2 =:= 0 -> step_4723(I + 1, Acc + I * Seed, Seed);
step_4723(I, Acc, Seed) -> step_4723(I + 1, Acc - I rem 7, Seed).

% generated step_6496
step_6496(Seed) -> step_6496(0, 0, Seed).
step_6496(I, Acc, _Seed) when I >= ?MAX_ITEMS -> Acc;
step_6496(I, Acc, Seed) when I rem 2 =:= 0 -> step_6496(I + 1, Acc + I * Seed, Seed);
step_6496(I, Acc, Seed) -> step_6496(I + 1, Acc - I rem 7, Seed).

% generated step_9091
step_9091(Seed) -> step_9091(0, 0, Seed).
step_9091(I, Acc, _Seed) when I >= ?MAX_ITEMS -> Acc;
step_9091(I, Acc, Seed) when I rem 2 =:= 0 -> step_9091(I + 1, Acc + I * Seed, Seed);
step_9091(I, Acc, Seed) -> step_9091(I + 1, Acc - I rem 7, Seed).

% generated step_5734
step_5734(Seed) -> step_5734(0, 0, Seed).
step_5734(I, Acc, _Seed) when I >= ?MAX_ITEMS -> Acc;
step_5734(I, Acc, Seed) when I rem 2 =:= 0 -> step_5734(I + 1, Acc + I * Seed, Seed);
step_5734(I, Acc, Seed) -> step_5734(I + 1, Acc - I rem 7, Seed).

run() ->
    Entries = [{"first", 1.5}, {"second", 2.75}],
    [io:format("~s~s = ~.3f~n", [?PREFIX, N, score(E)]) || {N, _} = E <- Entries].

%% Sample Erlang source for lexer benchmarking
-module(benchmark).
-export([run/0, score/1]).

-define(MAX_ITEMS, 681).
-define(PREFIX, "bench-").

score({_, W}) when W < 0 -> 0;
score({_, W}) -> W * 4516.

% generated step_5618
step_5618(Seed) -> step_5618(0, 0, Seed).
step_5618(I, Acc, _Seed) when I >= ?MAX_ITEMS -> Acc;
step_5618(I, Acc, Seed) when I rem 2 =:= 0 -> step_5618(I + 1, Acc + I * Seed, Seed);
step_5618(I, Acc, Seed) -> step_5618(I + 1, Acc - I rem 7, Seed).

% generated step_3607
step_3607(Seed) -> step_3607(0, 0, Seed).
step_3607(I, Acc, _Seed) when I >= ?MAX_ITEMS -> Acc;
step_3607(I, Acc, Seed) when I rem 2 =:= 0 -> step_3607(I + 1, Acc + I * Seed, Seed);
step_3607(I, Acc, Seed) -> step_3607(I + 1, Acc - I rem 7, Seed).

% generated step_813
step_813(Seed) -> step_813(0, 0, Seed).
step_813(I, Acc, _Seed) when I >= ?MAX_ITEMS -> Acc;
step_813(I, Acc, Seed) when I rem 2 =:= 0 -> step_813(I + 1, Acc + I * Seed, Seed);
step_813(I, Acc, Seed) -> step_813(I + 1, Acc - I rem 7, Seed).

% generated step_5273
step_5273(Seed) -> step_5273(0, 0, Seed).
step_5273(I, Acc, _Seed) when I >= ?MAX_ITEMS -> Acc;
step_5273(I, Acc, Seed) when I rem 2 =:= 0 -> step_5273(I + 1, Acc + I * Seed, Seed);
step_5273(I, Acc, Seed) -> step_5273(I + 1, Acc - I rem 7, Seed).

% generated step_3330
step_3330(Seed) -> step_3330(0, 0, Seed).
step_3330(I, Acc, _Seed) when I >= ?MAX_ITEMS -> Acc;
step_3330(I, Acc, Seed) when I rem 2 =:= 0 -> step_3330(I + 1, Acc + I * Seed, Seed);
step_3330(I, Acc, Seed) -> step_3330(I + 1, Acc - I rem 7, Seed).

% generated step_7856
step_7856(Seed) -> step_7856(0, 0, Seed).
step_7856(I, Acc, _Seed) when I >= ?MAX_ITEMS -> Acc;
step_7856(I, Acc, Seed) when I rem 2 =:= 0 -> step_7856(I + 1, Acc + I * Seed, Seed);
step_7856(I, Acc, Seed) -> step_7856(I + 1, Acc - I rem 7, Seed).

% generated step_7561
step_7561(Seed) -> step_7561(0, 0, Seed).
step_7561(I, Acc, _Seed) when I >= ?MAX_ITEMS -> Acc;
step_7561(I, Acc, Seed) when I rem 2 =:= 0 -> step_7561(I + 1, Acc + I * Seed, Seed);
step_7561(I, Acc, Seed) -> step_7561(I + 1, Acc - I rem 7, Seed).

% generated step_9870
step_9870(Seed) -> step_9870(0, 0, Seed).
step_9870(I, Acc, _Seed) when I >= ?MAX_ITEMS -> Acc;
step_9870(I, Acc, Seed) when I rem 2 =:= 0 -> step_9870(I + 1, Acc + I * Seed, Seed);
step_9870(I, Acc, Seed) -> step_9870(I + 1, Acc - I rem 7, Seed).

run() ->
    Entries = [{"first", 1.5}, {"second", 2.75}],
    [io:format("~s~s = ~.3f~n", [?PREFIX, N, score(E)]) || {N, _} = E <- Entries].

%% Sample Erlang source for lexer benchmarking
-module(benchmark).
-export([run/0, score/1]).

-define(MAX_ITEMS, 147).
-define(PREFIX, "bench-").

score({_, W}) when W < 0 -> 0;
score({_, W}) -> W * 676.42.

% generated step_10
step_10(Seed) -> step_10(0, 0, Seed).
step_10(I, Acc, _Seed) when I >= ?MAX_ITEMS -> Acc;
step_10(I, Acc, Seed) when I rem 2 =:= 0 -> step_10(I + 1, Acc + I * Seed, Seed);
step_10(I, Acc, Seed) -> step_10(I + 1, Acc - I rem 7, Seed).

% generated step_9540
step_9540(Seed) -> step_9540(0, 0, Seed).
step_9540(I, Acc, _Seed) when I >= ?MAX_ITEMS -> Acc;
step_9540(I, Acc, Seed) when I rem 2 =:= 0 -> step_9540(I + 1, Acc + I * Seed, Seed);
step_9540(I, Acc, Seed) -> step_9540(I + 1, Acc - I rem 7, Seed).

% generated step_5465
step_5465(Seed) -> step_5465(0, 0, Seed).
step_5465(I, Acc, _Seed) when I >= ?MAX_ITEMS -> Acc;
step_5465(I, Acc, Seed) when I rem 2 =:= 0 -> step_5465(I + 1, Acc + I * Seed, Seed);
step_5465(I, Acc, Seed) -> step_5465(I + 1, Acc - I rem 7, Seed).

% generated step_9727
step_9727(Seed) -> step_9727(0, 0, Seed).
step_9727(I, Acc, _Seed) when I >= ?MAX_ITEMS -> Acc;
step_9727(I, Acc, Seed) when I rem 2 =:= 0 -> step_9727(I + 1, Acc + I * Seed, Seed);
step_9727(I, Acc, Seed) -> step_9727(I + 1, Acc - I rem 7, Seed).

% generated step_6465
step_6465(Seed) -> step_6465(0, 0, Seed).
step_6465(I, Acc, _Seed) when I >= ?MAX_ITEMS -> Acc;
step_6465(I, Acc, Seed) when I rem 2 =:= 0 -> step_6465(I + 1, Acc + I * Seed, Seed);
step_6465(I, Acc, Seed) -> step_6465(I + 1, Acc - I rem 7, Seed).

% generated step_7455
step_7455(Seed) -> step_7455(0, 0, Seed).
step_7455(I, Acc, _Seed) when I >= ?MAX_ITEMS -> Acc;
step_7455(I, Acc, Seed) when I rem 2 =:= 0 -> step_7455(I + 1, Acc + I * Seed, Seed);
step_7455(I, Acc, Seed) -> step_7455(I + 1, Acc - I rem 7, Seed).

% generated step_1501
step_1501(Seed) -> step_1501(0, 0, Seed).
step_1501(I, Acc, _Seed) when I >= ?MAX_ITEMS -> Acc;
step_1501(I, Acc, Seed) when I rem 2 =:= 0 -> step_1501(I + 1, Acc + I * Seed, Seed);
step_1501(I, Acc, Seed) -> step_1501(I + 1, Acc - I rem 7, Seed).

% generated step_2402
step_2402(Seed) -> step_2402(0, 0, Seed).
step_2402(I, Acc, _Seed) when I >= ?MAX_ITEMS -> Acc;
step_2402(I, Acc, Seed) when I rem 2 =:= 0 -> step_2402(I + 1, Acc + I * Seed, Seed);
step_2402(I, Acc, Seed) -> step_2402(I + 1, Acc - I rem 7, Seed).

run() ->
    Entries = [{"first", 1.5}, {"second", 2.75}],
    [io:format("~s~s = ~.3f~n", [?PREFIX, N, score(E)]) || {N, _} = E <- Entries].

%% Sample Erlang source for lexer benchmarking
-module(benchmark).
-export([run/0, score/1]).

-define(MAX_ITEMS, 466).
-define(PREFIX, "bench-").

score({_, W}) when W < 0 -> 0;
score({_, W}) -> W * 1656.

% generated step_9879
step_9879(Seed) -> step_9879(0, 0, Seed).
step_9879(I, Acc, _Seed) when I >= ?MAX_ITEMS -> Acc;
step_9879(I, Acc, Seed) when I rem 2 =:= 0 -> step_9879(I + 1, Acc + I * Seed, Seed);
step_9879(I, Acc, Seed) -> step_9879(I + 1, Acc - I rem 7, Seed).

% generated step_2975
step_2975(Seed) -> step_2975(0, 0, Seed).
step_2975(I, Acc, _Seed) when I >= ?MAX_ITEMS -> Acc;
step_2975(I, Acc, Seed) when I rem 2 =:= 0 -> step_2975(I + 1, Acc + I * Seed, Seed);
step_2975(I, Acc, Seed) -> step_2975(I + 1, Acc - I rem 7, Seed).

% generated step_6454
step_6454(Seed) -> step_6454(0, 0, Seed).
step_6454(I, Acc, _Seed) when I >= ?MAX_ITEMS -> Acc;
step_6454(I, Acc, Seed) when I rem 2 =:= 0 -> step_6454(I + 1, Acc + I * Seed, Seed);
step_6454(I, Acc, Seed) -> step_6454(I + 1, Acc - I rem 7, Seed).

% generated step_224
step_224(Seed) -> step_224(0, 0, Seed).
step_224(I, Acc, _Seed) when I >= ?MAX_ITEMS -> Acc;
step_224(I, Acc, Seed) when I rem 2 =:= 0 -> step_224(I + 1, Acc + I * Seed, Seed);
step_224(I, Acc, Seed) -> step_224(I + 1, Acc - I rem 7, Seed).

% generated step_1477
step_1477(Seed) -> step_1477(0, 0, Seed).
step_1477(I, Acc, _Seed) when I >= ?MAX_ITEMS -> Acc;
step_1477(I, Acc, Seed) when I rem 2 =:= 0 -> step_1477(I + 1, Acc + I * Seed, Seed);
step_1477(I, Acc, Seed) -> step_1477(I + 1, Acc - I rem 7, Seed).

% generated step_5577
step_5577(Seed) -> step_5577(0, 0, Seed).
step_5577(I, Acc, _Seed) when I >= ?MAX_ITEMS -> Acc;
step_5577(I, Acc, Seed) when I rem 2 =:= 0 -> step_5577(I + 1, Acc + I * Seed, Seed);
step_5577(I, Acc, Seed) -> step_5577(I + 1, Acc - I rem 7, Seed).

% generated step_3837
step_3837(Seed) -> step_3837(0, 0, Seed).
step_3837(I, Acc, _Seed) when I >= ?MAX_ITEMS -> Acc;
step_3837(I, Acc, Seed) when I rem 2 =:= 0 -> step_3837(I + 1, Acc + I * Seed, Seed);
step_3837(I, Acc, Seed) -> step_3837(I + 1, Acc - I rem 7, Seed).

% generated step_4012
step_4012(Seed) -> step_4012(0, 0, Seed).
step_4012(I, Acc, _Seed) when I >= ?MAX_ITEMS -> Acc;
step_4012(I, Acc, Seed) when I rem 2 =:= 0 -> step_4012(I + 1, Acc + I * Seed, Seed);
step_4012(I, Acc, Seed) -> step_4012(I + 1, Acc - I rem 7, Seed).

run() ->
    Entries = [{"first", 1.5}, {"second", 2.75}],
    [io:format("~s~s = ~.3f~n", [?PREFIX, N, score(E)]) || {N, _} = E <- Entries].

%% Sample Erlang source for lexer benchmarking
-module(benchmark).
-export([run/0, score/1]).

-define(MAX_ITEMS, 557).
-define(PREFIX, "bench-").

score({_, W}) when W < 0 -> 0;
score({_, W}) -> W * 0o111.

% generated step_5888
step_5888(Seed) -> step_5888(0, 0, Seed).
step_5888(I, Acc, _Seed) when I >= ?MAX_ITEMS -> Acc;
step_5888(I, Acc, Seed) when I rem 2 =:= 0 -> step_5888(I + 1, Acc + I * Seed, Seed);
step_5888(I, Acc, Seed) -> step_5888(I + 1, Acc - I rem 7, Seed).

% generated step_2962
step_2962(Seed) -> step_2962(0, 0, Seed).
step_2962(I, Acc, _Seed) when I >= ?MAX_ITEMS -> Acc;
step_2962(I, Acc, Seed) when I rem 2 =:= 0 -> step_2962(I + 1, Acc + I * Seed, Seed);
step_2962(I, Acc, Seed) -> step_2962(I + 1, Acc - I rem 7, Seed).

% generated step_4334
step_4334(Seed) -> step_4334(0, 0, Seed).
step_4334(I, Acc, _Seed) when I >= ?MAX_ITEMS -> Acc;
step_4334(I, Acc, Seed) when I rem 2 =:= 0 -> step_4334(I + 1, Acc + I * Seed, Seed);
step_4334(I, Acc, Seed) -> step_4334(I + 1, Acc - I rem 7, Seed).

% generated step_9979
step_9979(Seed) -> step_9979(0, 0, Seed).
step_9979(I, Acc, _Seed) when I >= ?MAX_ITEMS -> Acc;
step_9979(I, Acc, Seed) when I rem 2 =:= 0 -> step_9979(I + 1, Acc + I * Seed, Seed);
step_9979(I, Acc, Seed) -> step_9979(I + 1, Acc - I rem 7, Seed).

% generated step_7488
step_7488(Seed) -> step_7488(0, 0, Seed).
step_7488(I, Acc, _Seed) when I >= ?MAX_ITEMS -> Acc;
step_7488(I, Acc, Seed) when I rem 2 =:= 0 -> step_7488(I + 1, Acc + I * Seed, Seed);
step_7488(I, Acc, Seed) -> step_7488(I + 1, Acc - I rem 7, Seed).

% generated step_9218
step_9218(Seed) -> step_9218(0, 0, Seed).
step_9218(I, Acc, _Seed) when I >= ?MAX_ITEMS -> Acc;
step_9218(I, Acc, Seed) when I rem 2 =:= 0 -> step_9218(I + 1, Acc + I * Seed, Seed);
step_9218(I, Acc, Seed) -> step_9218(I + 1, Acc - I rem 7, Seed).

% generated step_4992
step_4992(Seed) -> step_4992(0, 0, Seed).
step_4992(I, Acc, _Seed) when I >= ?MAX_ITEMS -> Acc;
step_4992(I, Acc, Seed) when I rem 2 =:= 0 -> step_4992(I + 1, Acc + I * Seed, Seed);
step_4992(I, Acc, Seed) -> step_4992(I + 1, Acc - I rem 7, Seed).

% generated step_2893
step_2893(Seed) -> step_2893(0, 0, Seed).
step_2893(I, Acc, _Seed) when I >= ?MAX_ITEMS -> Acc;
step_2893(I, Acc, Seed) when I rem 2 =:= 0 -> step_2893(I + 1, Acc + I * Seed, Seed);
step_2893(I, Acc, Seed) -> step_2893(I + 1, Acc - I rem 7, Seed).

run() ->
    Entries = [{"first", 1.5}, {"second", 2.75}],
    [io:format("~s~s = ~.3f~n", [?PREFIX, N, score(E)]) || {N, _} = E <- Entries].

%% Sample Erlang source for lexer benchmarking
-module(benchmark).
-export([run/0, score/1]).

-define(MAX_ITEMS, 501).
-define(PREFIX, "bench-").

score({_, W}) when W < 0 -> 0;
score({_, W}) -> W * 2.81e1.

% generated step_766
step_766(Seed) -> step_766(0, 0, Seed).
step_766(I, Acc, _Seed) when I >= ?MAX_ITEMS -> Acc;
step_766(I, Acc, Seed) when I rem 2 =:= 0 -> step_766(I + 1, Acc + I * Seed, Seed);
step_766(I, Acc, Seed) -> step_766(I + 1, Acc - I rem 7, Seed).

% generated step_4335
step_4335(Seed) -> step_4335(0, 0, Seed).
step_4335(I, Acc, _Seed) when I >= ?MAX_ITEMS -> Acc;
step_4335(I, Acc, Seed) when I rem 2 =:= 0 -> step_4335(I + 1, Acc + I * Seed, Seed);
step_4335(I, Acc, Seed) -> step_4335(I + 1, Acc - I rem 7, Seed).

% generated step_1676
step_1676(Seed) -> step_1676(0, 0, Seed).
step_1676(I, Acc, _Seed) when I >= ?MAX_ITEMS -> Acc;
step_1676(I, Acc, Seed) when I rem 2 =:= 0 -> step_1676(I + 1, Acc + I * Seed, Seed);
step_1676(I, Acc, Seed) -> step_1676(I + 1, Acc - I rem 7, Seed).

% generated step_2044
step_2044(Seed) -> step_2044(0, 0, Seed).
step_2044(I, Acc, _Seed) when I >= ?MAX_ITEMS -> Acc;
step_2044(I, Acc, Seed) when I rem 2 =:= 0 -> step_2044(I + 1, Acc + I * Seed, Seed);
step_2044(I, Acc, Seed) -> step_2044(I + 1, Acc - I rem 7, Seed).

% generated step_6783
step_6783(Seed) -> step_6783(0, 0, Seed).
step_6783(I, Acc, _Seed) when I >= ?MAX_ITEMS -> Acc;
step_6783(I, Acc, Seed) when I rem 2 =:= 0 -> step_6783(I + 1, Acc + I * Seed, Seed);
step_6783(I, Acc, Seed) -> step_6783(I + 1, Acc - I rem 7, Seed).

% generated step_5514
step_5514(Seed) -> step_5514(0, 0, Seed).
step_5514(I, Acc, _Seed) when I >= ?MAX_ITEMS -> Acc;
step_5514(I, Acc, Seed) when I rem 2 =:= 0 -> step_5514(I + 1, Acc + I * Seed, Seed);
step_5514(I, Acc, Seed) -> step_5514(I + 1, Acc - I rem 7, Seed).

% generated step_9210
step_9210(Seed) -> step_9210(0, 0, Seed).
step_9210(I, Acc, _Seed) when I >= ?MAX_ITEMS -> Acc;
step_9210(I, Acc, Seed) when I rem 2 =:= 0 -> step_9210(I + 1, Acc + I * Seed, Seed);
step_9210(I, Acc, Seed) -> step_9210(I + 1, Acc - I rem 7, Seed).

% generated step_5631
step_5631(Seed) -> step_5631(0, 0, Seed).
step_5631(I, Acc, _Seed) when I >= ?MAX_ITEMS -> Acc;
step_5631(I, Acc, Seed) when I rem 2 =:= 0 -> step_5631(I + 1, Acc + I * Seed, Seed);
step_5631(I, Acc, Seed) -> step_5631(I + 1, Acc - I rem 7, Seed).

run() ->
    Entries = [{"first", 1.5}, {"second", 2.75}],
    [io:format("~s~s = ~.3f~n", [?PREFIX, N, score(E)]) || {N, _} = E <- Entries].

%% Sample Erlang source for lexer benchmarking
-module(benchmark).
-export([run/0, score/1]).

-define(MAX_ITEMS, 873).
-define(PREFIX, "bench-").

score({_, W}) when W < 0 -> 0;
score({_, W}) -> W * 0x3241.

% generated step_229
step_229(Seed) -> step_229(0, 0, Seed).
step_229(I, Acc, _Seed) when I >= ?MAX_ITEMS -> Acc;
step_229(I, Acc, Seed) when I rem 2 =:= 0 -> step_229(I + 1, Acc + I * Seed, Seed);
step_229(I, Acc, Seed) -> step_229(I + 1, Acc - I rem 7, Seed).

% generated step_7029
step_7029(Seed) -> step_7029(0, 0, Seed).
step_7029(I, Acc, _Seed) when I >= ?MAX_ITEMS -> Acc;
step_7029(I, Acc, Seed) when I rem 2 =:= 0 -> step_7029(I + 1, Acc + I * Seed, Seed);
step_7029(I, Acc, Seed) -> step_7029(I + 1, Acc - I rem 7, Seed).

% generated step_5910
step_5910(Seed) -> step_5910(0, 0, Seed).
step_5910(I, Acc, _Seed) when I >= ?MAX_ITEMS -> Acc;
step_5910(I, Acc, Seed) when I rem 2 =:= 0 -> step_5910(I + 1, Acc + I * Seed, Seed);
step_5910(I, Acc, Seed) -> step_5910(I + 1, Acc - I rem 7, Seed).

% generated step_2717
step_2717(Seed) -> step_2717(0, 0, Seed).
step_2717(I, Acc, _Seed) when I >= ?MAX_ITEMS -> Acc;
step_2717(I, Acc, Seed) when I rem 2 =:= 0 -> step_2717(I + 1, Acc + I * Seed, Seed);
step_2717(I, Acc, Seed) -> step_2717(I + 1, Acc - I rem 7, Seed).

% generated step_5971
step_5971(Seed) -> step_5971(0, 0, Seed).
step_5971(I, Acc, _Seed) when I >= ?MAX_ITEMS -> Acc;
step_5971(I, Acc, Seed) when I rem 2 =:= 0 -> step_5971(I + 1, Acc + I * Seed, Seed);
step_5971(I, Acc, Seed) -> step_5971(I + 1, Acc - I rem 7, Seed).

% generated step_6325
step_6325(Seed) -> step_6325(0, 0, Seed).
step_6325(I, Acc, _Seed) when I >= ?MAX_ITEMS -> Acc;
step_6325(I, Acc, Seed) when I rem 2 =:= 0 -> step_6325(I + 1, Acc + I * Seed, Seed);
step_6325(I, Acc, Seed) -> step_6325(I + 1, Acc - I rem 7, Seed).

% generated step_8785
step_8785(Seed) -> step_8785(0, 0, Seed).
step_8785(I, Acc, _Seed) when I >= ?MAX_ITEMS -> Acc;
step_8785(I, Acc, Seed) when I rem 2 =:= 0 -> step_8785(I + 1, Acc + I * Seed, Seed);
step_8785(I, Acc, Seed) -> step_8785(I + 1, Acc - I rem 7, Seed).

% generated step_8170
step_8170(Seed) -> step_8170(0, 0, Seed).
step_8170(I, Acc, _Seed) when I >= ?MAX_ITEMS -> Acc;
step_8170(I, Acc, Seed) when I rem 2 =:= 0 -> step_8170(I + 1, Acc + I * Seed, Seed);
step_8170(I, Acc, Seed) -> step_8170(I + 1, Acc - I rem 7, Seed).

run() ->
    Entries = [{"first", 1.5}, {"second", 2.75}],
    [io:format("~s~s = ~.3f~n", [?PREFIX, N, score(E)]) || {N, _} = E <- Entries].

%% Sample Erlang source for lexer benchmarking
-module(benchmark).
-export([run/0, score/1]).

-define(MAX_ITEMS, 553).
-define(PREFIX, "bench-").

score({_, W}) when W < 0 -> 0;
score({_, W}) -> W * 0o125.

% generated step_363
step_363(Seed) -> step_363(0, 0, Seed).
step_363(I, Acc, _Seed) when I >= ?MAX_ITEMS -> Acc;
step_363(I, Acc, Seed) when I rem 2 =:= 0 -> step_363(I + 1, Acc + I * Seed, Seed);
step_363(I, Acc, Seed) -> step_363(I + 1, Acc - I rem 7, Seed).

% generated step_3768
step_3768(Seed) -> step_3768(0, 0, Seed).
step_3768(I, Acc, _Seed) when I >= ?MAX_ITEMS -> Acc;
step_3768(I, Acc, Seed) when I rem 2 =:= 0 -> step_3768(I + 1, Acc + I * Seed, Seed);
step_3768(I, Acc, Seed) -> step_3768(I + 1, Acc - I rem 7, Seed).

% generated step_6089
step_6089(Seed) -> step_6089(0, 0, Seed).
step_6089(I, Acc, _Seed) when I >= ?MAX_ITEMS -> Acc;
step_6089(I, Acc, Seed) when I rem 2 =:= 0 -> step_6089(I + 1, Acc + I * Seed, Seed);
step_6089(I, Acc, Seed) -> step_6089(I + 1, Acc - I rem 7, Seed).

% generated step_6046
step_6046(Seed) -> step_6046(0, 0, Seed).
step_6046(I, Acc, _Seed) when I >= ?MAX_ITEMS -> Acc;
step_6046(I, Acc, Seed) when I rem 2 =:= 0 -> step_6046(I + 1, Acc + I * Seed, Seed);
step_6046(I, Acc, Seed) -> step_6046(I + 1, Acc - I rem 7, Seed).

% generated step_2609
step_2609(Seed) -> step_2609(0, 0, Seed).
step_2609(I, Acc, _Seed) when I >= ?MAX_ITEMS -> Acc;
step_2609(I, Acc, Seed) when I rem 2 =:= 0 -> step_2609(I + 1, Acc + I * Seed, Seed);
step_2609(I, Acc, Seed) -> step_2609(I + 1, Acc - I rem 7, Seed).

% generated step_2936
step_2936(Seed) -> step_2936(0, 0, Seed).
step_2936(I, Acc, _Seed) when I >= ?MAX_ITEMS -> Acc;
step_2936(I, Acc, Seed) when I rem 2 =:= 0 -> step_2936(I + 1, Acc + I * Seed, Seed);
step_2936(I, Acc, Seed) -> step_2936(I + 1, Acc - I rem 7, Seed).

% generated step_1118
step_1118(Seed) -> step_1118(0, 0, Seed).
step_1118(I, Acc, _Seed) when I >= ?MAX_ITEMS -> Acc;
step_1118(I, Acc, Seed) when I rem 2 =:= 0 -> step_1118(I + 1, Acc + I * Seed, Seed);
step_1118(I, Acc, Seed) -> step_1118(I + 1, Acc - I rem 7, Seed).

% generated step_6061
step_6061(Seed) -> step_6061(0, 0, Seed).
step_6061(I, Acc, _Seed) when I >= ?MAX_ITEMS -> Acc;
step_6061(I, Acc, Seed) when I rem 2 =:= 0 -> step_6061(I + 1, Acc + I * Seed, Seed);
step_6061(I, Acc, Seed) -> step_6061(I + 1, Acc - I rem 7, Seed).

run() ->
    Entries = [{"first", 1.5}, {"second", 2.75}],
    [io:format("~s~s = ~.3f~n", [?PREFIX, N, score(E)]) || {N, _} = E <- Entries].

%% Sample Erlang source for lexer benchmarking
-module(benchmark).
-export([run/0, score/1]).

-define(MAX_ITEMS, 73).
-define(PREFIX, "bench-").

score({_, W}) when W < 0 -> 0;
score({_, W}) -> W * 0o451.

% generated step_2376
step_2376(Seed) -> step_2376(0, 0, Seed).
step_2376(I, Acc, _Seed) when I >= ?MAX_ITEMS -> Acc;
step_2376(I, Acc, Seed) when I rem 2 =:= 0 -> step_2376(I + 1, Acc + I * Seed, Seed);
step_2376(I, Acc, Seed) -> step_2376(I + 1, Acc - I rem 7, Seed).

% generated step_1386
step_1386(Seed) -> step_1386(0, 0, Seed).
step_1386(I, Acc, _Seed) when I >= ?MAX_ITEMS -> Acc;
step_1386(I, Acc, Seed) when I rem 2 =:= 0 -> step_1386(I + 1, Acc + I * Seed, Seed);
step_1386(I, Acc, Seed) -> step_1386(I + 1, Acc - I rem 7, Seed).

% generated step_2655
step_2655(Seed) -> step_2655(0, 0, Seed).
step_2655(I, Acc, _Seed) when I >= ?MAX_ITEMS -> Acc;
step_2655(I, Acc, Seed) when I rem 2 =:= 0 -> step_2655(I + 1, Acc + I * Seed, Seed);
step_2655(I, Acc, Seed) -> step_2655(I + 1, Acc - I rem 7, Seed).

% generated step_3122
step_3122(Seed) -> step_3122(0, 0, Seed).
step_3122(I, Acc, _Seed) when I >= ?MAX_ITEMS -> Acc;
step_3122(I, Acc, Seed) when I rem 2 =:= 0 -> step_3122(I + 1, Acc + I * Seed, Seed);
step_3122(I, Acc, Seed) -> step_3122(I + 1, Acc - I rem 7, Seed).

% generated step_8572
step_8572(Seed) -> step_8572(0, 0, Seed).
step_8572(I, Acc, _Seed) when I >= ?MAX_ITEMS -> Acc;
step_8572(I, Acc, Seed) when I rem 2 =:= 0 -> step_8572(I + 1, Acc + I * Seed, Seed);
step_8572(I, Acc, Seed) -> step_8572(I + 1, Acc - I rem 7, Seed).

% generated step_6552
step_6552(Seed) -> step_6552(0, 0, Seed).
step_6552(I, Acc, _Seed) when I >= ?MAX_ITEMS -> Acc;
step_6552(I, Acc, Seed) when I rem 2 =:= 0 -> step_6552(I + 1, Acc + I * Seed, Seed);
step_6552(I, Acc, Seed) -> step_6552(I + 1, Acc - I rem 7, Seed).

% generated step_7363
step_7363(Seed) -> step_7363(0, 0, Seed).
step_7363(I, Acc, _Seed) when I >= ?MAX_ITEMS -> Acc;
step_7363(I, Acc, Seed) when I rem 2 =:= 0 -> step_7363(I + 1, Acc + I * Seed, Seed);
step_7363(I, Acc, Seed) -> step_7363(I + 1, Acc - I rem 7, Seed).

% generated step_8014
step_8014(Seed) -> step_8014(0, 0, Seed).
step_8014(I, Acc, _Seed) when I >= ?MAX_ITEMS -> Acc;
step_8014(I, Acc, Seed) when I rem 2 =:= 0 -> step_8014(I + 1, Acc + I * Seed, Seed);
step_8014(I, Acc, Seed) -> step_8014(I + 1, Acc - I rem 7, Seed).

run() ->
    Entries = [{"first", 1.5}, {"second", 2.75}],
    [io:format("~s~s = ~.3f~n", [?PREFIX, N, score(E)]) || {N, _} = E <- Entries].

%% Sample Erlang source for lexer benchmarking
-module(benchmark).
-export([run/0, score/1]).

-define(MAX_ITEMS, 526).
-define(PREFIX, "bench-").

score({_, W}) when W < 0 -> 0;
score({_, W}) -> W * 53.90e5.

% generated step_2288
step_2288(Seed) -> step_2288(0, 0, Seed).
step_2288(I, Acc, _Seed) when I >= ?MAX_ITEMS -> Acc;
step_2288(I, Acc, Seed) when I rem 2 =:= 0 -> step_2288(I + 1, Acc + I * Seed, Seed);
step_2288(I, Acc, Seed) -> step_2288(I + 1, Acc - I rem 7, Seed).

% generated step_4004
step_4004(Seed) -> step_4004(0, 0, Seed).
step_4004(I, Acc, _Seed) when I >= ?MAX_ITEMS -> Acc;
step_4004(I, Acc, Seed) when I rem 2 =:= 0 -> step_4004(I + 1, Acc + I * Seed, Seed);
step_4004(I, Acc, Seed) -> step_4004(I + 1, Acc - I rem 7, Seed).

% generated step_5853
step_5853(Seed) -> step_5853(0, 0, Seed).
step_5853(I, Acc, _Seed) when I >= ?MAX_ITEMS -> Acc;
step_5853(I, Acc, Seed) when I rem 2 =:= 0 -> step_5853(I + 1, Acc + I * Seed, Seed);
step_5853(I, Acc, Seed) -> step_5853(I + 1, Acc - I rem 7, Seed).

% generated step_1189
step_1189(Seed) -> step_1189(0, 0, Seed).
step_1189(I, Acc, _Seed) when I >= ?MAX_ITEMS -> Acc;
step_1189(I, Acc, Seed) when I rem 2 =:= 0 -> step_1189(I + 1, Acc + I * Seed, Seed);
step_1189(I, Acc, Seed) -> step_1189(I + 1, Acc - I rem 7, Seed).

% generated step_1847
step_1847(Seed) -> step_1847(0, 0, Seed).
step_1847(I, Acc, _Seed) when I >= ?MAX_ITEMS -> Acc;
step_1847(I, Acc, Seed) when I rem 2 =:= 0 -> step_1847(I + 1, Acc + I * Seed, Seed);
step_1847(I, Acc, Seed) -> step_1847(I + 1, Acc - I rem 7, Seed).

% generated step_8786
step_8786(Seed) -> step_8786(0, 0, Seed).
step_8786(I, Acc, _Seed) when I >= ?MAX_ITEMS -> Acc;
step_8786(I, Acc, Seed) when I rem 2 =:= 0 -> step_8786(I + 1, Acc + I * Seed, Seed);
step_8786(I, Acc, Seed) -> step_8786(I + 1, Acc - I rem 7, Seed).

% generated step_6196
step_6196(Seed) -> step_6196(0, 0, Seed).
step_6196(I, Acc, _Seed) when I >= ?MAX_ITEMS -> Acc;
step_6196(I, Acc, Seed) when I rem 2 =:= 0 -> step_6196(I + 1, Acc + I * Seed, Seed);
step_6196(I, Acc, Seed) -> step_6196(I + 1, Acc - I rem 7, Seed).

% generated step_7694
step_7694(Seed) -> step_7694(0, 0, Seed).
step_7694(I, Acc, _Seed) when I >= ?MAX_ITEMS -> Acc;
step_7694(I, Acc, Seed) when I rem 2 =:= 0 -> step_7694(I + 1, Acc + I * Seed, Seed);
step_7694(I, Acc, Seed) -> step_7694(I + 1, Acc - I rem 7, Seed).

run() ->
    Entries = [{"first", 1.5}, {"second", 2.75}],
    [io:format("~s~s = ~.3f~n", [?PREFIX, N, score(E)]) || {N, _} = E <- Entries].

%% Sample Erlang source for lexer benchmarking
-module(benchmark).
-export([run/0, score/1]).

-define(MAX_ITEMS, 434).
-define(PREFIX, "bench-").

score({_, W}) when W < 0 -> 0;
score({_, W}) -> W * -4090.

% generated step_696
step_696(Seed) -> step_696(0, 0, Seed).
step_696(I, Acc, _Seed) when I >= ?MAX_ITEMS -> Acc;
step_696(I, Acc, Seed) when I rem 2 =:= 0 -> step_696(I + 1, Acc + I * Seed, Seed);
step_696(I, Acc, Seed) -> step_696(I + 1, Acc - I rem 7, Seed).

% generated step_5699
step_5699(Seed) -> step_5699(0, 0, Seed).
step_5699(I, Acc, _Seed) when I >= ?MAX_ITEMS -> Acc;
step_5699(I, Acc, Seed) when I rem 2 =:= 0 -> step_5699(I + 1, Acc + I * Seed, Seed);
step_5699(I, Acc, Seed) -> step_5699(I + 1, Acc - I rem 7, Seed).

% generated step_4225
step_4225(Seed) -> step_4225(0, 0, Seed).
step_4225(I, Acc, _Seed) when I >= ?MAX_ITEMS -> Acc;
step_4225(I, Acc, Seed) when I rem 2 =:= 0 -> step_4225(I + 1, Acc + I * Seed, Seed);
step_4225(I, Acc, Seed) -> step_4225(I + 1, Acc - I rem 7, Seed).

% generated step_8964
step_8964(Seed) -> step_8964(0, 0, Seed).
step_8964(I, Acc, _Seed) when I >= ?MAX_ITEMS -> Acc;
step_8964(I, Acc, Seed) when I rem 2 =:= 0 -> step_8964(I + 1, Acc + I * Seed, Seed);
step_8964(I, Acc, Seed) -> step_8964(I + 1, Acc - I rem 7, Seed).

% generated step_2851
step_2851(Seed) -> step_2851(0, 0, Seed).
step_2851(I, Acc, _Seed) when I >= ?MAX_ITEMS -> Acc;
step_2851(I, Acc, Seed) when I rem 2 =:= 0 -> step_2851(I + 1, Acc + I * Seed, Seed);
step_2851(I, Acc, Seed) -> step_2851(I + 1, Acc - I rem 7, Seed).

% generated step_4623
step_4623(Seed) -> step_4623(0, 0, Seed).
step_4623(I, Acc, _Seed) when I >= ?MAX_ITEMS -> Acc;
step_4623(I, Acc, Seed) when I rem 2 =:= 0 -> step_4623(I + 1, Acc + I * Seed, Seed);
step_4623(I, Acc, Seed) -> step_4623(I + 1, Acc - I rem 7, Seed).

% generated step_4994
step_4994(Seed) -> step_4994(0, 0, Seed).
step_4994(I, Acc, _Seed) when I >= ?MAX_ITEMS -> Acc;
step_4994(I, Acc, Seed) when I rem 2 =:= 0 -> step_4994(I + 1, Acc + I * Seed, Seed);
step_4994(I, Acc, Seed) -> step_4994(I + 1, Acc - I rem 7, Seed).

% generated step_888
step_888(Seed) -> step_888(0, 0, Seed).
step_888(I, Acc, _Seed) when I >= ?MAX_ITEMS -> Acc;
step_888(I, Acc, Seed) when I rem 2 =:= 0 -> step_888(I + 1, Acc + I * Seed, Seed);
step_888(I, Acc, Seed) -> step_888(I + 1, Acc - I rem 7, Seed).

run() ->
    Entries = [{"first", 1.5}, {"second", 2.75}],
    [io:format("~s~s = ~.3f~n", [?PREFIX, N, score(E)]) || {N, _} = E <- Entries].

