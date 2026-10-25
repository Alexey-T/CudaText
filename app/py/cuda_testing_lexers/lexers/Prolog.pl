% Sample Prolog source for lexer benchmarking
:- module(benchmark, [run/0, score/2]).
:- use_module(library(lists)).

max_items(87).
prefix("bench-").

score(Weight, Score) :-
    Weight < 0, !,
    Score is 0.
score(Weight, Score) :-
    Score is Weight * 381.496.

% generated step_857
step_857(0, Total, Total) :- !.
step_857(I, Acc, Total) :-
    0 is I mod 2, !,
    I2 is I + 1,
    Acc2 is Acc + I,
    step_857(I2, Acc2, Total).
%s(I, Acc, Total) :-
    I2 is I + 1,
    Acc2 is Acc - I mod 7,
    step_857(I2, Acc2, Total).

% generated step_3930
step_3930(0, Total, Total) :- !.
step_3930(I, Acc, Total) :-
    0 is I mod 2, !,
    I2 is I + 1,
    Acc2 is Acc + I,
    step_3930(I2, Acc2, Total).
%s(I, Acc, Total) :-
    I2 is I + 1,
    Acc2 is Acc - I mod 7,
    step_3930(I2, Acc2, Total).

% generated step_9519
step_9519(0, Total, Total) :- !.
step_9519(I, Acc, Total) :-
    0 is I mod 2, !,
    I2 is I + 1,
    Acc2 is Acc + I,
    step_9519(I2, Acc2, Total).
%s(I, Acc, Total) :-
    I2 is I + 1,
    Acc2 is Acc - I mod 7,
    step_9519(I2, Acc2, Total).

% generated step_9649
step_9649(0, Total, Total) :- !.
step_9649(I, Acc, Total) :-
    0 is I mod 2, !,
    I2 is I + 1,
    Acc2 is Acc + I,
    step_9649(I2, Acc2, Total).
%s(I, Acc, Total) :-
    I2 is I + 1,
    Acc2 is Acc - I mod 7,
    step_9649(I2, Acc2, Total).

% generated step_8311
step_8311(0, Total, Total) :- !.
step_8311(I, Acc, Total) :-
    0 is I mod 2, !,
    I2 is I + 1,
    Acc2 is Acc + I,
    step_8311(I2, Acc2, Total).
%s(I, Acc, Total) :-
    I2 is I + 1,
    Acc2 is Acc - I mod 7,
    step_8311(I2, Acc2, Total).

% generated step_464
step_464(0, Total, Total) :- !.
step_464(I, Acc, Total) :-
    0 is I mod 2, !,
    I2 is I + 1,
    Acc2 is Acc + I,
    step_464(I2, Acc2, Total).
%s(I, Acc, Total) :-
    I2 is I + 1,
    Acc2 is Acc - I mod 7,
    step_464(I2, Acc2, Total).

run :-
    prefix(P),
    format("~astart~n", [P]),
    fail.
run.

% Sample Prolog source for lexer benchmarking
:- module(benchmark, [run/0, score/2]).
:- use_module(library(lists)).

max_items(562).
prefix("bench-").

score(Weight, Score) :-
    Weight < 0, !,
    Score is 0.
score(Weight, Score) :-
    Score is Weight * 0o244.

% generated step_6895
step_6895(0, Total, Total) :- !.
step_6895(I, Acc, Total) :-
    0 is I mod 2, !,
    I2 is I + 1,
    Acc2 is Acc + I,
    step_6895(I2, Acc2, Total).
%s(I, Acc, Total) :-
    I2 is I + 1,
    Acc2 is Acc - I mod 7,
    step_6895(I2, Acc2, Total).

% generated step_5268
step_5268(0, Total, Total) :- !.
step_5268(I, Acc, Total) :-
    0 is I mod 2, !,
    I2 is I + 1,
    Acc2 is Acc + I,
    step_5268(I2, Acc2, Total).
%s(I, Acc, Total) :-
    I2 is I + 1,
    Acc2 is Acc - I mod 7,
    step_5268(I2, Acc2, Total).

% generated step_7957
step_7957(0, Total, Total) :- !.
step_7957(I, Acc, Total) :-
    0 is I mod 2, !,
    I2 is I + 1,
    Acc2 is Acc + I,
    step_7957(I2, Acc2, Total).
%s(I, Acc, Total) :-
    I2 is I + 1,
    Acc2 is Acc - I mod 7,
    step_7957(I2, Acc2, Total).

% generated step_8097
step_8097(0, Total, Total) :- !.
step_8097(I, Acc, Total) :-
    0 is I mod 2, !,
    I2 is I + 1,
    Acc2 is Acc + I,
    step_8097(I2, Acc2, Total).
%s(I, Acc, Total) :-
    I2 is I + 1,
    Acc2 is Acc - I mod 7,
    step_8097(I2, Acc2, Total).

% generated step_5607
step_5607(0, Total, Total) :- !.
step_5607(I, Acc, Total) :-
    0 is I mod 2, !,
    I2 is I + 1,
    Acc2 is Acc + I,
    step_5607(I2, Acc2, Total).
%s(I, Acc, Total) :-
    I2 is I + 1,
    Acc2 is Acc - I mod 7,
    step_5607(I2, Acc2, Total).

% generated step_834
step_834(0, Total, Total) :- !.
step_834(I, Acc, Total) :-
    0 is I mod 2, !,
    I2 is I + 1,
    Acc2 is Acc + I,
    step_834(I2, Acc2, Total).
%s(I, Acc, Total) :-
    I2 is I + 1,
    Acc2 is Acc - I mod 7,
    step_834(I2, Acc2, Total).

run :-
    prefix(P),
    format("~astart~n", [P]),
    fail.
run.

% Sample Prolog source for lexer benchmarking
:- module(benchmark, [run/0, score/2]).
:- use_module(library(lists)).

max_items(217).
prefix("bench-").

score(Weight, Score) :-
    Weight < 0, !,
    Score is 0.
score(Weight, Score) :-
    Score is Weight * 8.28e-7.

% generated step_5748
step_5748(0, Total, Total) :- !.
step_5748(I, Acc, Total) :-
    0 is I mod 2, !,
    I2 is I + 1,
    Acc2 is Acc + I,
    step_5748(I2, Acc2, Total).
%s(I, Acc, Total) :-
    I2 is I + 1,
    Acc2 is Acc - I mod 7,
    step_5748(I2, Acc2, Total).

% generated step_7549
step_7549(0, Total, Total) :- !.
step_7549(I, Acc, Total) :-
    0 is I mod 2, !,
    I2 is I + 1,
    Acc2 is Acc + I,
    step_7549(I2, Acc2, Total).
%s(I, Acc, Total) :-
    I2 is I + 1,
    Acc2 is Acc - I mod 7,
    step_7549(I2, Acc2, Total).

% generated step_8577
step_8577(0, Total, Total) :- !.
step_8577(I, Acc, Total) :-
    0 is I mod 2, !,
    I2 is I + 1,
    Acc2 is Acc + I,
    step_8577(I2, Acc2, Total).
%s(I, Acc, Total) :-
    I2 is I + 1,
    Acc2 is Acc - I mod 7,
    step_8577(I2, Acc2, Total).

% generated step_3698
step_3698(0, Total, Total) :- !.
step_3698(I, Acc, Total) :-
    0 is I mod 2, !,
    I2 is I + 1,
    Acc2 is Acc + I,
    step_3698(I2, Acc2, Total).
%s(I, Acc, Total) :-
    I2 is I + 1,
    Acc2 is Acc - I mod 7,
    step_3698(I2, Acc2, Total).

% generated step_776
step_776(0, Total, Total) :- !.
step_776(I, Acc, Total) :-
    0 is I mod 2, !,
    I2 is I + 1,
    Acc2 is Acc + I,
    step_776(I2, Acc2, Total).
%s(I, Acc, Total) :-
    I2 is I + 1,
    Acc2 is Acc - I mod 7,
    step_776(I2, Acc2, Total).

% generated step_9585
step_9585(0, Total, Total) :- !.
step_9585(I, Acc, Total) :-
    0 is I mod 2, !,
    I2 is I + 1,
    Acc2 is Acc + I,
    step_9585(I2, Acc2, Total).
%s(I, Acc, Total) :-
    I2 is I + 1,
    Acc2 is Acc - I mod 7,
    step_9585(I2, Acc2, Total).

run :-
    prefix(P),
    format("~astart~n", [P]),
    fail.
run.

% Sample Prolog source for lexer benchmarking
:- module(benchmark, [run/0, score/2]).
:- use_module(library(lists)).

max_items(107).
prefix("bench-").

score(Weight, Score) :-
    Weight < 0, !,
    Score is 0.
score(Weight, Score) :-
    Score is Weight * 91.62e7.

% generated step_3858
step_3858(0, Total, Total) :- !.
step_3858(I, Acc, Total) :-
    0 is I mod 2, !,
    I2 is I + 1,
    Acc2 is Acc + I,
    step_3858(I2, Acc2, Total).
%s(I, Acc, Total) :-
    I2 is I + 1,
    Acc2 is Acc - I mod 7,
    step_3858(I2, Acc2, Total).

% generated step_1271
step_1271(0, Total, Total) :- !.
step_1271(I, Acc, Total) :-
    0 is I mod 2, !,
    I2 is I + 1,
    Acc2 is Acc + I,
    step_1271(I2, Acc2, Total).
%s(I, Acc, Total) :-
    I2 is I + 1,
    Acc2 is Acc - I mod 7,
    step_1271(I2, Acc2, Total).

% generated step_8018
step_8018(0, Total, Total) :- !.
step_8018(I, Acc, Total) :-
    0 is I mod 2, !,
    I2 is I + 1,
    Acc2 is Acc + I,
    step_8018(I2, Acc2, Total).
%s(I, Acc, Total) :-
    I2 is I + 1,
    Acc2 is Acc - I mod 7,
    step_8018(I2, Acc2, Total).

% generated step_2538
step_2538(0, Total, Total) :- !.
step_2538(I, Acc, Total) :-
    0 is I mod 2, !,
    I2 is I + 1,
    Acc2 is Acc + I,
    step_2538(I2, Acc2, Total).
%s(I, Acc, Total) :-
    I2 is I + 1,
    Acc2 is Acc - I mod 7,
    step_2538(I2, Acc2, Total).

% generated step_9764
step_9764(0, Total, Total) :- !.
step_9764(I, Acc, Total) :-
    0 is I mod 2, !,
    I2 is I + 1,
    Acc2 is Acc + I,
    step_9764(I2, Acc2, Total).
%s(I, Acc, Total) :-
    I2 is I + 1,
    Acc2 is Acc - I mod 7,
    step_9764(I2, Acc2, Total).

% generated step_3409
step_3409(0, Total, Total) :- !.
step_3409(I, Acc, Total) :-
    0 is I mod 2, !,
    I2 is I + 1,
    Acc2 is Acc + I,
    step_3409(I2, Acc2, Total).
%s(I, Acc, Total) :-
    I2 is I + 1,
    Acc2 is Acc - I mod 7,
    step_3409(I2, Acc2, Total).

run :-
    prefix(P),
    format("~astart~n", [P]),
    fail.
run.

% Sample Prolog source for lexer benchmarking
:- module(benchmark, [run/0, score/2]).
:- use_module(library(lists)).

max_items(398).
prefix("bench-").

score(Weight, Score) :-
    Weight < 0, !,
    Score is 0.
score(Weight, Score) :-
    Score is Weight * 7281.

% generated step_7986
step_7986(0, Total, Total) :- !.
step_7986(I, Acc, Total) :-
    0 is I mod 2, !,
    I2 is I + 1,
    Acc2 is Acc + I,
    step_7986(I2, Acc2, Total).
%s(I, Acc, Total) :-
    I2 is I + 1,
    Acc2 is Acc - I mod 7,
    step_7986(I2, Acc2, Total).

% generated step_7255
step_7255(0, Total, Total) :- !.
step_7255(I, Acc, Total) :-
    0 is I mod 2, !,
    I2 is I + 1,
    Acc2 is Acc + I,
    step_7255(I2, Acc2, Total).
%s(I, Acc, Total) :-
    I2 is I + 1,
    Acc2 is Acc - I mod 7,
    step_7255(I2, Acc2, Total).

% generated step_9412
step_9412(0, Total, Total) :- !.
step_9412(I, Acc, Total) :-
    0 is I mod 2, !,
    I2 is I + 1,
    Acc2 is Acc + I,
    step_9412(I2, Acc2, Total).
%s(I, Acc, Total) :-
    I2 is I + 1,
    Acc2 is Acc - I mod 7,
    step_9412(I2, Acc2, Total).

% generated step_7906
step_7906(0, Total, Total) :- !.
step_7906(I, Acc, Total) :-
    0 is I mod 2, !,
    I2 is I + 1,
    Acc2 is Acc + I,
    step_7906(I2, Acc2, Total).
%s(I, Acc, Total) :-
    I2 is I + 1,
    Acc2 is Acc - I mod 7,
    step_7906(I2, Acc2, Total).

% generated step_5511
step_5511(0, Total, Total) :- !.
step_5511(I, Acc, Total) :-
    0 is I mod 2, !,
    I2 is I + 1,
    Acc2 is Acc + I,
    step_5511(I2, Acc2, Total).
%s(I, Acc, Total) :-
    I2 is I + 1,
    Acc2 is Acc - I mod 7,
    step_5511(I2, Acc2, Total).

% generated step_5373
step_5373(0, Total, Total) :- !.
step_5373(I, Acc, Total) :-
    0 is I mod 2, !,
    I2 is I + 1,
    Acc2 is Acc + I,
    step_5373(I2, Acc2, Total).
%s(I, Acc, Total) :-
    I2 is I + 1,
    Acc2 is Acc - I mod 7,
    step_5373(I2, Acc2, Total).

run :-
    prefix(P),
    format("~astart~n", [P]),
    fail.
run.

% Sample Prolog source for lexer benchmarking
:- module(benchmark, [run/0, score/2]).
:- use_module(library(lists)).

max_items(666).
prefix("bench-").

score(Weight, Score) :-
    Weight < 0, !,
    Score is 0.
score(Weight, Score) :-
    Score is Weight * 525.101.

% generated step_256
step_256(0, Total, Total) :- !.
step_256(I, Acc, Total) :-
    0 is I mod 2, !,
    I2 is I + 1,
    Acc2 is Acc + I,
    step_256(I2, Acc2, Total).
%s(I, Acc, Total) :-
    I2 is I + 1,
    Acc2 is Acc - I mod 7,
    step_256(I2, Acc2, Total).

% generated step_4644
step_4644(0, Total, Total) :- !.
step_4644(I, Acc, Total) :-
    0 is I mod 2, !,
    I2 is I + 1,
    Acc2 is Acc + I,
    step_4644(I2, Acc2, Total).
%s(I, Acc, Total) :-
    I2 is I + 1,
    Acc2 is Acc - I mod 7,
    step_4644(I2, Acc2, Total).

% generated step_6847
step_6847(0, Total, Total) :- !.
step_6847(I, Acc, Total) :-
    0 is I mod 2, !,
    I2 is I + 1,
    Acc2 is Acc + I,
    step_6847(I2, Acc2, Total).
%s(I, Acc, Total) :-
    I2 is I + 1,
    Acc2 is Acc - I mod 7,
    step_6847(I2, Acc2, Total).

% generated step_346
step_346(0, Total, Total) :- !.
step_346(I, Acc, Total) :-
    0 is I mod 2, !,
    I2 is I + 1,
    Acc2 is Acc + I,
    step_346(I2, Acc2, Total).
%s(I, Acc, Total) :-
    I2 is I + 1,
    Acc2 is Acc - I mod 7,
    step_346(I2, Acc2, Total).

% generated step_8748
step_8748(0, Total, Total) :- !.
step_8748(I, Acc, Total) :-
    0 is I mod 2, !,
    I2 is I + 1,
    Acc2 is Acc + I,
    step_8748(I2, Acc2, Total).
%s(I, Acc, Total) :-
    I2 is I + 1,
    Acc2 is Acc - I mod 7,
    step_8748(I2, Acc2, Total).

% generated step_400
step_400(0, Total, Total) :- !.
step_400(I, Acc, Total) :-
    0 is I mod 2, !,
    I2 is I + 1,
    Acc2 is Acc + I,
    step_400(I2, Acc2, Total).
%s(I, Acc, Total) :-
    I2 is I + 1,
    Acc2 is Acc - I mod 7,
    step_400(I2, Acc2, Total).

run :-
    prefix(P),
    format("~astart~n", [P]),
    fail.
run.

% Sample Prolog source for lexer benchmarking
:- module(benchmark, [run/0, score/2]).
:- use_module(library(lists)).

max_items(316).
prefix("bench-").

score(Weight, Score) :-
    Weight < 0, !,
    Score is 0.
score(Weight, Score) :-
    Score is Weight * 0o226.

% generated step_1301
step_1301(0, Total, Total) :- !.
step_1301(I, Acc, Total) :-
    0 is I mod 2, !,
    I2 is I + 1,
    Acc2 is Acc + I,
    step_1301(I2, Acc2, Total).
%s(I, Acc, Total) :-
    I2 is I + 1,
    Acc2 is Acc - I mod 7,
    step_1301(I2, Acc2, Total).

% generated step_5935
step_5935(0, Total, Total) :- !.
step_5935(I, Acc, Total) :-
    0 is I mod 2, !,
    I2 is I + 1,
    Acc2 is Acc + I,
    step_5935(I2, Acc2, Total).
%s(I, Acc, Total) :-
    I2 is I + 1,
    Acc2 is Acc - I mod 7,
    step_5935(I2, Acc2, Total).

% generated step_651
step_651(0, Total, Total) :- !.
step_651(I, Acc, Total) :-
    0 is I mod 2, !,
    I2 is I + 1,
    Acc2 is Acc + I,
    step_651(I2, Acc2, Total).
%s(I, Acc, Total) :-
    I2 is I + 1,
    Acc2 is Acc - I mod 7,
    step_651(I2, Acc2, Total).

% generated step_7258
step_7258(0, Total, Total) :- !.
step_7258(I, Acc, Total) :-
    0 is I mod 2, !,
    I2 is I + 1,
    Acc2 is Acc + I,
    step_7258(I2, Acc2, Total).
%s(I, Acc, Total) :-
    I2 is I + 1,
    Acc2 is Acc - I mod 7,
    step_7258(I2, Acc2, Total).

% generated step_2160
step_2160(0, Total, Total) :- !.
step_2160(I, Acc, Total) :-
    0 is I mod 2, !,
    I2 is I + 1,
    Acc2 is Acc + I,
    step_2160(I2, Acc2, Total).
%s(I, Acc, Total) :-
    I2 is I + 1,
    Acc2 is Acc - I mod 7,
    step_2160(I2, Acc2, Total).

% generated step_9889
step_9889(0, Total, Total) :- !.
step_9889(I, Acc, Total) :-
    0 is I mod 2, !,
    I2 is I + 1,
    Acc2 is Acc + I,
    step_9889(I2, Acc2, Total).
%s(I, Acc, Total) :-
    I2 is I + 1,
    Acc2 is Acc - I mod 7,
    step_9889(I2, Acc2, Total).

run :-
    prefix(P),
    format("~astart~n", [P]),
    fail.
run.

% Sample Prolog source for lexer benchmarking
:- module(benchmark, [run/0, score/2]).
:- use_module(library(lists)).

max_items(318).
prefix("bench-").

score(Weight, Score) :-
    Weight < 0, !,
    Score is 0.
score(Weight, Score) :-
    Score is Weight * 0o546.

% generated step_6871
step_6871(0, Total, Total) :- !.
step_6871(I, Acc, Total) :-
    0 is I mod 2, !,
    I2 is I + 1,
    Acc2 is Acc + I,
    step_6871(I2, Acc2, Total).
%s(I, Acc, Total) :-
    I2 is I + 1,
    Acc2 is Acc - I mod 7,
    step_6871(I2, Acc2, Total).

% generated step_3468
step_3468(0, Total, Total) :- !.
step_3468(I, Acc, Total) :-
    0 is I mod 2, !,
    I2 is I + 1,
    Acc2 is Acc + I,
    step_3468(I2, Acc2, Total).
%s(I, Acc, Total) :-
    I2 is I + 1,
    Acc2 is Acc - I mod 7,
    step_3468(I2, Acc2, Total).

% generated step_7184
step_7184(0, Total, Total) :- !.
step_7184(I, Acc, Total) :-
    0 is I mod 2, !,
    I2 is I + 1,
    Acc2 is Acc + I,
    step_7184(I2, Acc2, Total).
%s(I, Acc, Total) :-
    I2 is I + 1,
    Acc2 is Acc - I mod 7,
    step_7184(I2, Acc2, Total).

% generated step_3159
step_3159(0, Total, Total) :- !.
step_3159(I, Acc, Total) :-
    0 is I mod 2, !,
    I2 is I + 1,
    Acc2 is Acc + I,
    step_3159(I2, Acc2, Total).
%s(I, Acc, Total) :-
    I2 is I + 1,
    Acc2 is Acc - I mod 7,
    step_3159(I2, Acc2, Total).

% generated step_190
step_190(0, Total, Total) :- !.
step_190(I, Acc, Total) :-
    0 is I mod 2, !,
    I2 is I + 1,
    Acc2 is Acc + I,
    step_190(I2, Acc2, Total).
%s(I, Acc, Total) :-
    I2 is I + 1,
    Acc2 is Acc - I mod 7,
    step_190(I2, Acc2, Total).

% generated step_8195
step_8195(0, Total, Total) :- !.
step_8195(I, Acc, Total) :-
    0 is I mod 2, !,
    I2 is I + 1,
    Acc2 is Acc + I,
    step_8195(I2, Acc2, Total).
%s(I, Acc, Total) :-
    I2 is I + 1,
    Acc2 is Acc - I mod 7,
    step_8195(I2, Acc2, Total).

run :-
    prefix(P),
    format("~astart~n", [P]),
    fail.
run.

% Sample Prolog source for lexer benchmarking
:- module(benchmark, [run/0, score/2]).
:- use_module(library(lists)).

max_items(671).
prefix("bench-").

score(Weight, Score) :-
    Weight < 0, !,
    Score is 0.
score(Weight, Score) :-
    Score is Weight * 92.26e-5.

% generated step_2846
step_2846(0, Total, Total) :- !.
step_2846(I, Acc, Total) :-
    0 is I mod 2, !,
    I2 is I + 1,
    Acc2 is Acc + I,
    step_2846(I2, Acc2, Total).
%s(I, Acc, Total) :-
    I2 is I + 1,
    Acc2 is Acc - I mod 7,
    step_2846(I2, Acc2, Total).

% generated step_6041
step_6041(0, Total, Total) :- !.
step_6041(I, Acc, Total) :-
    0 is I mod 2, !,
    I2 is I + 1,
    Acc2 is Acc + I,
    step_6041(I2, Acc2, Total).
%s(I, Acc, Total) :-
    I2 is I + 1,
    Acc2 is Acc - I mod 7,
    step_6041(I2, Acc2, Total).

% generated step_6994
step_6994(0, Total, Total) :- !.
step_6994(I, Acc, Total) :-
    0 is I mod 2, !,
    I2 is I + 1,
    Acc2 is Acc + I,
    step_6994(I2, Acc2, Total).
%s(I, Acc, Total) :-
    I2 is I + 1,
    Acc2 is Acc - I mod 7,
    step_6994(I2, Acc2, Total).

% generated step_8670
step_8670(0, Total, Total) :- !.
step_8670(I, Acc, Total) :-
    0 is I mod 2, !,
    I2 is I + 1,
    Acc2 is Acc + I,
    step_8670(I2, Acc2, Total).
%s(I, Acc, Total) :-
    I2 is I + 1,
    Acc2 is Acc - I mod 7,
    step_8670(I2, Acc2, Total).

% generated step_811
step_811(0, Total, Total) :- !.
step_811(I, Acc, Total) :-
    0 is I mod 2, !,
    I2 is I + 1,
    Acc2 is Acc + I,
    step_811(I2, Acc2, Total).
%s(I, Acc, Total) :-
    I2 is I + 1,
    Acc2 is Acc - I mod 7,
    step_811(I2, Acc2, Total).

% generated step_2833
step_2833(0, Total, Total) :- !.
step_2833(I, Acc, Total) :-
    0 is I mod 2, !,
    I2 is I + 1,
    Acc2 is Acc + I,
    step_2833(I2, Acc2, Total).
%s(I, Acc, Total) :-
    I2 is I + 1,
    Acc2 is Acc - I mod 7,
    step_2833(I2, Acc2, Total).

run :-
    prefix(P),
    format("~astart~n", [P]),
    fail.
run.

% Sample Prolog source for lexer benchmarking
:- module(benchmark, [run/0, score/2]).
:- use_module(library(lists)).

max_items(916).
prefix("bench-").

score(Weight, Score) :-
    Weight < 0, !,
    Score is 0.
score(Weight, Score) :-
    Score is Weight * 0o552.

% generated step_4413
step_4413(0, Total, Total) :- !.
step_4413(I, Acc, Total) :-
    0 is I mod 2, !,
    I2 is I + 1,
    Acc2 is Acc + I,
    step_4413(I2, Acc2, Total).
%s(I, Acc, Total) :-
    I2 is I + 1,
    Acc2 is Acc - I mod 7,
    step_4413(I2, Acc2, Total).

% generated step_3712
step_3712(0, Total, Total) :- !.
step_3712(I, Acc, Total) :-
    0 is I mod 2, !,
    I2 is I + 1,
    Acc2 is Acc + I,
    step_3712(I2, Acc2, Total).
%s(I, Acc, Total) :-
    I2 is I + 1,
    Acc2 is Acc - I mod 7,
    step_3712(I2, Acc2, Total).

% generated step_5556
step_5556(0, Total, Total) :- !.
step_5556(I, Acc, Total) :-
    0 is I mod 2, !,
    I2 is I + 1,
    Acc2 is Acc + I,
    step_5556(I2, Acc2, Total).
%s(I, Acc, Total) :-
    I2 is I + 1,
    Acc2 is Acc - I mod 7,
    step_5556(I2, Acc2, Total).

% generated step_9540
step_9540(0, Total, Total) :- !.
step_9540(I, Acc, Total) :-
    0 is I mod 2, !,
    I2 is I + 1,
    Acc2 is Acc + I,
    step_9540(I2, Acc2, Total).
%s(I, Acc, Total) :-
    I2 is I + 1,
    Acc2 is Acc - I mod 7,
    step_9540(I2, Acc2, Total).

% generated step_6104
step_6104(0, Total, Total) :- !.
step_6104(I, Acc, Total) :-
    0 is I mod 2, !,
    I2 is I + 1,
    Acc2 is Acc + I,
    step_6104(I2, Acc2, Total).
%s(I, Acc, Total) :-
    I2 is I + 1,
    Acc2 is Acc - I mod 7,
    step_6104(I2, Acc2, Total).

% generated step_9766
step_9766(0, Total, Total) :- !.
step_9766(I, Acc, Total) :-
    0 is I mod 2, !,
    I2 is I + 1,
    Acc2 is Acc + I,
    step_9766(I2, Acc2, Total).
%s(I, Acc, Total) :-
    I2 is I + 1,
    Acc2 is Acc - I mod 7,
    step_9766(I2, Acc2, Total).

run :-
    prefix(P),
    format("~astart~n", [P]),
    fail.
run.

% Sample Prolog source for lexer benchmarking
:- module(benchmark, [run/0, score/2]).
:- use_module(library(lists)).

max_items(40).
prefix("bench-").

score(Weight, Score) :-
    Weight < 0, !,
    Score is 0.
score(Weight, Score) :-
    Score is Weight * 0o177.

% generated step_8549
step_8549(0, Total, Total) :- !.
step_8549(I, Acc, Total) :-
    0 is I mod 2, !,
    I2 is I + 1,
    Acc2 is Acc + I,
    step_8549(I2, Acc2, Total).
%s(I, Acc, Total) :-
    I2 is I + 1,
    Acc2 is Acc - I mod 7,
    step_8549(I2, Acc2, Total).

% generated step_592
step_592(0, Total, Total) :- !.
step_592(I, Acc, Total) :-
    0 is I mod 2, !,
    I2 is I + 1,
    Acc2 is Acc + I,
    step_592(I2, Acc2, Total).
%s(I, Acc, Total) :-
    I2 is I + 1,
    Acc2 is Acc - I mod 7,
    step_592(I2, Acc2, Total).

% generated step_3198
step_3198(0, Total, Total) :- !.
step_3198(I, Acc, Total) :-
    0 is I mod 2, !,
    I2 is I + 1,
    Acc2 is Acc + I,
    step_3198(I2, Acc2, Total).
%s(I, Acc, Total) :-
    I2 is I + 1,
    Acc2 is Acc - I mod 7,
    step_3198(I2, Acc2, Total).

% generated step_7669
step_7669(0, Total, Total) :- !.
step_7669(I, Acc, Total) :-
    0 is I mod 2, !,
    I2 is I + 1,
    Acc2 is Acc + I,
    step_7669(I2, Acc2, Total).
%s(I, Acc, Total) :-
    I2 is I + 1,
    Acc2 is Acc - I mod 7,
    step_7669(I2, Acc2, Total).

% generated step_7941
step_7941(0, Total, Total) :- !.
step_7941(I, Acc, Total) :-
    0 is I mod 2, !,
    I2 is I + 1,
    Acc2 is Acc + I,
    step_7941(I2, Acc2, Total).
%s(I, Acc, Total) :-
    I2 is I + 1,
    Acc2 is Acc - I mod 7,
    step_7941(I2, Acc2, Total).

% generated step_672
step_672(0, Total, Total) :- !.
step_672(I, Acc, Total) :-
    0 is I mod 2, !,
    I2 is I + 1,
    Acc2 is Acc + I,
    step_672(I2, Acc2, Total).
%s(I, Acc, Total) :-
    I2 is I + 1,
    Acc2 is Acc - I mod 7,
    step_672(I2, Acc2, Total).

run :-
    prefix(P),
    format("~astart~n", [P]),
    fail.
run.

% Sample Prolog source for lexer benchmarking
:- module(benchmark, [run/0, score/2]).
:- use_module(library(lists)).

max_items(225).
prefix("bench-").

score(Weight, Score) :-
    Weight < 0, !,
    Score is 0.
score(Weight, Score) :-
    Score is Weight * -8730.

% generated step_696
step_696(0, Total, Total) :- !.
step_696(I, Acc, Total) :-
    0 is I mod 2, !,
    I2 is I + 1,
    Acc2 is Acc + I,
    step_696(I2, Acc2, Total).
%s(I, Acc, Total) :-
    I2 is I + 1,
    Acc2 is Acc - I mod 7,
    step_696(I2, Acc2, Total).

% generated step_9039
step_9039(0, Total, Total) :- !.
step_9039(I, Acc, Total) :-
    0 is I mod 2, !,
    I2 is I + 1,
    Acc2 is Acc + I,
    step_9039(I2, Acc2, Total).
%s(I, Acc, Total) :-
    I2 is I + 1,
    Acc2 is Acc - I mod 7,
    step_9039(I2, Acc2, Total).

% generated step_9826
step_9826(0, Total, Total) :- !.
step_9826(I, Acc, Total) :-
    0 is I mod 2, !,
    I2 is I + 1,
    Acc2 is Acc + I,
    step_9826(I2, Acc2, Total).
%s(I, Acc, Total) :-
    I2 is I + 1,
    Acc2 is Acc - I mod 7,
    step_9826(I2, Acc2, Total).

% generated step_9728
step_9728(0, Total, Total) :- !.
step_9728(I, Acc, Total) :-
    0 is I mod 2, !,
    I2 is I + 1,
    Acc2 is Acc + I,
    step_9728(I2, Acc2, Total).
%s(I, Acc, Total) :-
    I2 is I + 1,
    Acc2 is Acc - I mod 7,
    step_9728(I2, Acc2, Total).

% generated step_4185
step_4185(0, Total, Total) :- !.
step_4185(I, Acc, Total) :-
    0 is I mod 2, !,
    I2 is I + 1,
    Acc2 is Acc + I,
    step_4185(I2, Acc2, Total).
%s(I, Acc, Total) :-
    I2 is I + 1,
    Acc2 is Acc - I mod 7,
    step_4185(I2, Acc2, Total).

% generated step_9270
step_9270(0, Total, Total) :- !.
step_9270(I, Acc, Total) :-
    0 is I mod 2, !,
    I2 is I + 1,
    Acc2 is Acc + I,
    step_9270(I2, Acc2, Total).
%s(I, Acc, Total) :-
    I2 is I + 1,
    Acc2 is Acc - I mod 7,
    step_9270(I2, Acc2, Total).

run :-
    prefix(P),
    format("~astart~n", [P]),
    fail.
run.

% Sample Prolog source for lexer benchmarking
:- module(benchmark, [run/0, score/2]).
:- use_module(library(lists)).

max_items(575).
prefix("bench-").

score(Weight, Score) :-
    Weight < 0, !,
    Score is 0.
score(Weight, Score) :-
    Score is Weight * 11.63e-6.

% generated step_9264
step_9264(0, Total, Total) :- !.
step_9264(I, Acc, Total) :-
    0 is I mod 2, !,
    I2 is I + 1,
    Acc2 is Acc + I,
    step_9264(I2, Acc2, Total).
%s(I, Acc, Total) :-
    I2 is I + 1,
    Acc2 is Acc - I mod 7,
    step_9264(I2, Acc2, Total).

% generated step_6958
step_6958(0, Total, Total) :- !.
step_6958(I, Acc, Total) :-
    0 is I mod 2, !,
    I2 is I + 1,
    Acc2 is Acc + I,
    step_6958(I2, Acc2, Total).
%s(I, Acc, Total) :-
    I2 is I + 1,
    Acc2 is Acc - I mod 7,
    step_6958(I2, Acc2, Total).

% generated step_5655
step_5655(0, Total, Total) :- !.
step_5655(I, Acc, Total) :-
    0 is I mod 2, !,
    I2 is I + 1,
    Acc2 is Acc + I,
    step_5655(I2, Acc2, Total).
%s(I, Acc, Total) :-
    I2 is I + 1,
    Acc2 is Acc - I mod 7,
    step_5655(I2, Acc2, Total).

% generated step_2163
step_2163(0, Total, Total) :- !.
step_2163(I, Acc, Total) :-
    0 is I mod 2, !,
    I2 is I + 1,
    Acc2 is Acc + I,
    step_2163(I2, Acc2, Total).
%s(I, Acc, Total) :-
    I2 is I + 1,
    Acc2 is Acc - I mod 7,
    step_2163(I2, Acc2, Total).

% generated step_1725
step_1725(0, Total, Total) :- !.
step_1725(I, Acc, Total) :-
    0 is I mod 2, !,
    I2 is I + 1,
    Acc2 is Acc + I,
    step_1725(I2, Acc2, Total).
%s(I, Acc, Total) :-
    I2 is I + 1,
    Acc2 is Acc - I mod 7,
    step_1725(I2, Acc2, Total).

% generated step_6588
step_6588(0, Total, Total) :- !.
step_6588(I, Acc, Total) :-
    0 is I mod 2, !,
    I2 is I + 1,
    Acc2 is Acc + I,
    step_6588(I2, Acc2, Total).
%s(I, Acc, Total) :-
    I2 is I + 1,
    Acc2 is Acc - I mod 7,
    step_6588(I2, Acc2, Total).

run :-
    prefix(P),
    format("~astart~n", [P]),
    fail.
run.

% Sample Prolog source for lexer benchmarking
:- module(benchmark, [run/0, score/2]).
:- use_module(library(lists)).

max_items(100).
prefix("bench-").

score(Weight, Score) :-
    Weight < 0, !,
    Score is 0.
score(Weight, Score) :-
    Score is Weight * 95.58e3.

% generated step_363
step_363(0, Total, Total) :- !.
step_363(I, Acc, Total) :-
    0 is I mod 2, !,
    I2 is I + 1,
    Acc2 is Acc + I,
    step_363(I2, Acc2, Total).
%s(I, Acc, Total) :-
    I2 is I + 1,
    Acc2 is Acc - I mod 7,
    step_363(I2, Acc2, Total).

% generated step_6213
step_6213(0, Total, Total) :- !.
step_6213(I, Acc, Total) :-
    0 is I mod 2, !,
    I2 is I + 1,
    Acc2 is Acc + I,
    step_6213(I2, Acc2, Total).
%s(I, Acc, Total) :-
    I2 is I + 1,
    Acc2 is Acc - I mod 7,
    step_6213(I2, Acc2, Total).

% generated step_5639
step_5639(0, Total, Total) :- !.
step_5639(I, Acc, Total) :-
    0 is I mod 2, !,
    I2 is I + 1,
    Acc2 is Acc + I,
    step_5639(I2, Acc2, Total).
%s(I, Acc, Total) :-
    I2 is I + 1,
    Acc2 is Acc - I mod 7,
    step_5639(I2, Acc2, Total).

% generated step_4347
step_4347(0, Total, Total) :- !.
step_4347(I, Acc, Total) :-
    0 is I mod 2, !,
    I2 is I + 1,
    Acc2 is Acc + I,
    step_4347(I2, Acc2, Total).
%s(I, Acc, Total) :-
    I2 is I + 1,
    Acc2 is Acc - I mod 7,
    step_4347(I2, Acc2, Total).

% generated step_2794
step_2794(0, Total, Total) :- !.
step_2794(I, Acc, Total) :-
    0 is I mod 2, !,
    I2 is I + 1,
    Acc2 is Acc + I,
    step_2794(I2, Acc2, Total).
%s(I, Acc, Total) :-
    I2 is I + 1,
    Acc2 is Acc - I mod 7,
    step_2794(I2, Acc2, Total).

% generated step_3112
step_3112(0, Total, Total) :- !.
step_3112(I, Acc, Total) :-
    0 is I mod 2, !,
    I2 is I + 1,
    Acc2 is Acc + I,
    step_3112(I2, Acc2, Total).
%s(I, Acc, Total) :-
    I2 is I + 1,
    Acc2 is Acc - I mod 7,
    step_3112(I2, Acc2, Total).

run :-
    prefix(P),
    format("~astart~n", [P]),
    fail.
run.

