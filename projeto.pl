display_game(Board, Player):-
    nl,
    length(Board, Length),
    gen_column_labels(Length, Labels, 65),
    display_separated_line(Labels, ''),
    display_separator,
    display_matrix(Board, 0).

start_gameplay(L):-
    L = [
        [1,0,0,0,0,0,0,0,0,0,0],
        [0,0,0,0,0,0,0,0,0,0,0],
        [0,0,0,0,0,0,0,0,0,0,0],
        [0,0,0,0,0,0,0,0,0,0,0],
        [0,0,0,0,0,0,0,0,0,0,0],
        [0,0,0,0,0,0,0,0,0,0,0],
        [0,0,0,0,0,0,0,0,0,0,0],
        [0,0,0,0,0,0,0,0,0,0,0],
        [0,0,0,0,0,0,0,0,0,0,0],
        [0,0,0,0,0,0,0,0,0,0,0],
        [0,0,0,0,0,0,0,0,0,0,2]
    ].

mid_gameplay(L):-
    L = [
        [1,1,3,0,0,0,0,0,0,0,0],
        [1,3,0,0,0,0,0,0,0,0,0],
        [0,0,3,3,0,0,0,0,0,0,0],
        [0,0,0,0,2,0,0,0,0,0,0],
        [0,0,0,0,2,2,0,0,0,0,0],
        [0,0,0,3,3,2,0,0,0,0,0],
        [0,2,2,0,0,0,2,2,2,2,2],
        [0,0,0,0,0,0,0,0,0,0,2],
        [0,0,0,0,0,0,0,0,0,0,2],
        [0,0,0,0,0,0,0,0,0,0,2],
        [0,0,0,0,0,0,0,0,0,0,2]
    ].

final_gameplay(L):-
    L = [
        [1,1,3,0,0,0,0,0,0,0,0],
        [1,3,0,0,0,0,0,0,0,0,0],
        [0,0,3,3,0,3,0,0,0,0,0],
        [0,0,1,0,3,3,3,0,0,0,0],
        [0,0,0,0,3,3,3,0,0,0,0],
        [0,0,1,3,3,3,1,1,0,0,0],
        [0,3,3,1,0,0,3,3,3,3,3],
        [0,0,0,0,0,0,1,1,0,3,3],
        [0,0,0,0,0,0,0,0,0,3,3],
        [0,0,0,0,0,0,0,0,0,1,3],
        [0,0,0,0,0,0,0,0,0,0,3]
    ].

display_matrix([], _).
display_matrix([H | T], LineNumber):-
    display_separated_line(H, LineNumber),
    display_separator,
    LineNumber1 is LineNumber+1,
    display_matrix(T, LineNumber1).

display_separated_line([], LineNumber):- write('|  '), write(LineNumber), nl.
display_separated_line([H | T], LineNumber):-
    print_cell(H),
    display_separated_line(T, LineNumber).

display_line([]):- nl.
display_line([H | T]):-
    traducao(H, X),
    put_code(X),
    display_line(T).

print_cell(C):-
    write('|'),
    traducao(C, X),
    write(' '),
    put_code(X),
    write('  ').

display_separator:-
    gen_line(56, SeperatorLineList, 111),
    display_line(SeperatorLineList).
    

gen_column_labels(0, [], _).
gen_column_labels(Size, List, LabelNr):-
    Size1 is Size - 1,
    NextLabelNr is LabelNr + 1,
    gen_column_labels(Size1, List1, NextLabelNr),
    List = [LabelNr | List1].



gen_line(0, [], _).
gen_line(Size, List, Value):-
    Size1 is Size - 1,
    gen_line(Size1, List1, Value),
    List = [Value | List1].


traducao(0, 32).
traducao(1, 9634).
traducao(2, 9711).
traducao(3, 9635).
traducao(4, 9673).
traducao(111, 9472).
traducao(Val, Translated):-Translated is Val.