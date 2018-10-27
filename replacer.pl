:- use_module(library(lists)).

update_matrix_at(Matrix, Result, X, Y, NewVal):-
    length(Matrix, MatrixLength),
    Y < MatrixLength,
    Y >= 0,
    !,
    nth0(Y, Matrix, Line),
    replace_at(Line, NewLine, X, NewVal),
    replace_at(Matrix, Result, Y, NewLine).

update_matrix_at(Matrix, Result, X, Y, NewVal):-
    nl, write('Trying to replace value with Y coordinate out of bounds!'), nl, fail.

replace_at(SourceList, DestList, N, NewVal):-
    length(SourceList, Length),
    Length > N,
    N >= 0,
    !,
    replace_at_aux(SourceList, DestList, 0, N, NewVal).

replace_at(SourceList, DestList, N, NewVal):-
    nl, write('Trying to replace an index out of bounds!'), nl, fail.


replace_at_aux([H|T], LResult, C, C, NewVal):-
    LResult = [NewVal | T].

replace_at_aux([H|T], LResult, C, N, NewVal):-
    C1 is C + 1,
    replace_at_aux(T, L1, C1, N, NewVal),
    LResult = [H | L1].