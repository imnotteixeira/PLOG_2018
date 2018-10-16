run_game:-
    write('Welcome to Tic Tac Toe in Prolog'),
    nl,
    start_gameplay.

start_gameplay:-
    L = [
        [0,0,0,0,0,0,0,0,0,0,2],
        [0,0,0,0,0,0,0,0,0,0,0],
        [0,0,0,0,0,0,0,0,0,0,0],
        [0,0,0,0,0,0,0,0,0,0,0],
        [0,0,0,0,0,0,0,0,0,0,0],
        [0,0,0,0,0,0,0,0,0,0,0],
        [0,0,0,0,0,0,0,0,0,0,0],
        [0,0,0,0,0,0,0,0,0,0,0],
        [0,0,0,0,0,0,0,0,0,0,0],
        [0,0,0,0,0,0,0,0,0,0,0],
        [1,0,0,0,0,0,0,0,0,0,0]
    ],
    display_separator,
    display_matrix(L),

display_matrix([]).
display_matrix([H | T]):-
    display_separated_line(H),
    display_separator,
    display_matrix(T).

display_separated_line([]):- write('|'), nl.
display_separated_line([H | T]):-
    print_cell(H),
    display_separated_line(T).

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
    display_line([111,111,111,111,111,111,111,111,111,111,111,111,
        111,111,111,111,111,111,111,111,111,111,111,
        111,111,111,111,111,111,111,111,111,111,111,
        111,111,111,111,111,111,111,111,111,111,111,
        111,111,111,111,111,111,111,111,111,111,111]).
    
traducao(0, 32).
traducao(1, 9634).
traducao(2, 9711).
traducao(3, 9635).
traducao(4, 9673).
traducao(111, 9472).