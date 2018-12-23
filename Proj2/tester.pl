:- consult('Projeto2.pl').

options([leftmost, step, up]).
options([leftmost, step, down]).
options([leftmost, enum, up]).
options([leftmost, enum, down]).
options([leftmost, bisect, up]).
options([leftmost, bisect, down]).
options([leftmost, middle, up]).
options([leftmost, middle, down]).
options([leftmost, median, up]).
options([leftmost, median, down]).

options([max, step, up]).
options([max, step, down]).
options([max, enum, up]).
options([max, enum, down]).
options([max, bisect, up]).
options([max, bisect, down]).
options([max, middle, up]).
options([max, middle, down]).
options([max, median, up]).
options([max, median, down]).

options([min, step, up]).
options([min, step, down]).
options([min, enum, up]).
options([min, enum, down]).
options([min, bisect, up]).
options([min, bisect, down]).
options([min, middle, up]).
options([min, middle, down]).
options([min, median, up]).
options([min, median, down]).

options([ff, step, up]).
options([ff, step, down]).
options([ff, enum, up]).
options([ff, enum, down]).
options([ff, bisect, up]).
options([ff, bisect, down]).
options([ff, middle, up]).
options([ff, middle, down]).
options([ff, median, up]).
options([ff, median, down]).

options([ffc, step, up]).
options([ffc, step, down]).
options([ffc, enum, up]).
options([ffc, enum, down]).
options([ffc, bisect, up]).
options([ffc, bisect, down]).
options([ffc, middle, up]).
options([ffc, middle, down]).
options([ffc, median, up]).
options([ffc, median, down]).

options([anti_first_fail, step, up]).
options([anti_first_fail, step, down]).
options([anti_first_fail, enum, up]).
options([anti_first_fail, enum, down]).
options([anti_first_fail, bisect, up]).
options([anti_first_fail, bisect, down]).
options([anti_first_fail, middle, up]).
options([anti_first_fail, middle, down]).
options([anti_first_fail, median, up]).
options([anti_first_fail, median, down]).

options([occurrence, step, up]).
options([occurrence, step, down]).
options([occurrence, enum, up]).
options([occurrence, enum, down]).
options([occurrence, bisect, up]).
options([occurrence, bisect, down]).
options([occurrence, middle, up]).
options([occurrence, middle, down]).
options([occurrence, median, up]).
options([occurrence, median, down]).

options([max_regret, step, up]).
options([max_regret, step, down]).
options([max_regret, enum, up]).
options([max_regret, enum, down]).
options([max_regret, bisect, up]).
options([max_regret, bisect, down]).
options([max_regret, middle, up]).
options([max_regret, middle, down]).
options([max_regret, median, up]).
options([max_regret, median, down]).

labeling_tests:-
    findall(Opt, options(Opt), Opts),
    test_opts(Opts).


test_opts([]).
test_opts([[H1, H2, H3]|Tail]):-
    test([H1, H2, H3], T, Flag),

    write(user_error, [H1, H2, H3]),
    write(user_error, '\n'),
    write(H1),
    write(','),
    write(H2),
    write(','),
    write(H3),
    write(','),
    write(T),
    write(','),
    % trace,
    write(Flag), nl,
    test_opts(Tail).


labeling_tests(Tested):-
    % trace,
    options([O1, O2, O3]),
    \+member([O1, O2, O3], Tested),
    % notrace,
    test([O1, O2, O3], T, Flag),

    write(user_error, [O1, O2, O3]),
    write(user_error, '\n'),
    write(O1),
    write(','),
    write(O2),
    write(','),
    write(O3),
    write(','),
    write(T),
    write(','),
    % trace,
    write(Flag), nl,
    append(Tested, [[O1, O2, O3]], NewTested),
    labeling_tests(NewTested).

    
test_basic(O, T, Flag):-
    main(6, 5, 2, _, O, T, Flag).
test_medium(O, T, Flag):-
    main(9, 8, 2, _, O, T, Flag).
test_big(O, T, Flag):-
    main(41, 8, 7, _, O, T, Flag).

complexity_test_nCells(FirstNumber, Multiplier, EndAmountOfCells, EndAmountOfCells):-
    
    run_profiling_mode(FirstNumber, EndAmountOfCells, Multiplier, _, [ff, bisect, up], T, _),
    format('~d,~d\n', [EndAmountOfCells, T]).

complexity_test_nCells(FirstNumber, Multiplier, StartAmountOfCells, EndAmountOfCells):-
    run_profiling_mode(FirstNumber, StartAmountOfCells, Multiplier, _, [ff, bisect, up], T, _),
    format('~d,~d\n', [StartAmountOfCells, T]),
    NextAmount is StartAmountOfCells + 1,
    complexity_test_nCells(FirstNumber, Multiplier, NextAmount, EndAmountOfCells).
complexity_test_nCells(FirstNumber, Multiplier, StartAmountOfCells, EndAmountOfCells):-
    NextAmount is StartAmountOfCells + 1,
    complexity_test_nCells(FirstNumber, Multiplier, NextAmount, EndAmountOfCells).

complexity_test_multiplier(FirstNumber, EndMultiplier, EndMultiplier, AmountOfCells):-
    
    run_profiling_mode(FirstNumber, AmountOfCells, EndMultiplier, _, [ff, bisect, up], T, _),
    % format(user_error, '~d,~d\n', [EndMultiplier, T]).
    format('~d,~d\n', [EndMultiplier, T]).

complexity_test_multiplier(FirstNumber, FirstMultiplier, EndMultiplier, AmountOfCells):-
    run_profiling_mode(FirstNumber, AmountOfCells, FirstMultiplier, _, [ff, bisect, up], T, _),
    % format(user_error, '~d,~d\n', [FirstMultiplier, T]),
    format('~d,~d\n', [FirstMultiplier, T]),
    NextAmount is FirstMultiplier + 1,
    complexity_test_multiplier(FirstNumber, NextAmount, EndMultiplier, AmountOfCells).
complexity_test_multiplier(FirstNumber, FirstMultiplier, EndMultiplier, AmountOfCells):-
    NextAmount is FirstMultiplier + 1,
    complexity_test_multiplier(FirstNumber, NextAmount, EndMultiplier, AmountOfCells).

complexity_test_start(EndNumber, EndNumber, Multiplier, AmountOfCells):-
    
    run_profiling_mode(EndNumber, AmountOfCells, Multiplier, _, [ff, bisect, up], T, _),
    format('~d,~d\n', [EndNumber, T]).

complexity_test_start(FirstNumber, EndNumber, Multiplier, AmountOfCells):-
    run_profiling_mode(FirstNumber, AmountOfCells, Multiplier, _, [ff, bisect, up], T, _),
    format('~d,~d\n', [FirstNumber, T]),
    NextAmount is FirstNumber + 1,
    complexity_test_start(NextAmount, EndNumber, Multiplier, AmountOfCells).
complexity_test_start(FirstNumber, EndNumber, Multiplier, AmountOfCells):-
    NextAmount is FirstNumber + 1,
    complexity_test_start(NextAmount, EndNumber, Multiplier, AmountOfCells).

