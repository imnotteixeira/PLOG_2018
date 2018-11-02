:- consult('replacer.pl').

%displays a game for the given board
display_game(Board, Player):-
    nl,
    length(Board, Length),
    gen_column_labels(Length, Labels, 65),
    write(' '), display_separated_line(Labels, '', ' '),
    display_separator,
    display_matrix(Board, 0),
    write('Player '),
    PlayerNumber is Player + 1,
    write(PlayerNumber),
    write('\'s turn.'), nl,
    !,
    readCoordinatesAndUpdateMatrix(Board, NewBoard, Player),
    !,
    NextPlayer is ((Player + 1) mod 2),
    display_game(NewBoard, NextPlayer).

readCoordinatesAndUpdateMatrix(Matrix, NewMatrix, Player):-
    write('Insere as coordenadas da tua proxima jogada <A-K 0-10 .>:'),
    get_code(X1),
    read(Y),
    get_code(_),
    number(Y),
    !,
    X is X1 - "A",
    write('O X e '),
    write(X),
    write(' e o Y e '),
    write(Y),
    update_matrix_at(Matrix, NewMatrix, X, Y, 3).

readCoordinatesAndUpdateMatrix(_,_,_):- write('Read error'),nl, fail.

%abstractions for game states demo
display_start_game:-
    start_gameplay(Board),
    display_game(Board, 0).
display_mid_game:-
    mid_gameplay(Board),
    display_game(Board, 1).
display_final_game:-
    final_gameplay(Board),
    display_game(Board, 0).
    
%generates start board
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
%generates mid-game board
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

%generates end board
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

%displays a matrix representing a board
display_matrix([], _).
display_matrix([H | T], LineNumber):-
    display_separated_line(H, LineNumber, '|'),
    display_separator,
    LineNumber1 is LineNumber+1,
    display_matrix(T, LineNumber1).

%displays a line with vertical separators
display_separated_line([], LineNumber, SeparatorStr):- write(SeparatorStr), write('  '), write(LineNumber), nl.
display_separated_line([H | T], LineNumber, SeparatorStr):-
    print_cell(H, SeparatorStr),
    display_separated_line(T, LineNumber, SeparatorStr).

%displays a line without separators
display_line([]):- nl.
display_line([H | T]):-
    traducao(H, X),
    put_code(X),
    display_line(T).

%prints a translated cell
print_cell(C, SeparatorStr):-
    write(SeparatorStr),
    traducao(C, X),
    write(' '),
    put_code(X),
    write('  ').

%prints an horizontal separator
display_separator:-
    gen_line(56, SeperatorLineList, -1),
    display_line(SeperatorLineList).
    
%generates the column labels
gen_column_labels(0, [], _).
gen_column_labels(Size, List, LabelNr):-
    Size1 is Size - 1,
    NextLabelNr is LabelNr + 1,
    gen_column_labels(Size1, List1, NextLabelNr),
    List = [LabelNr | List1].

%generates a line of n given chars
gen_line(0, [], _).
gen_line(Size, List, Value):-
    Size1 is Size - 1,
    gen_line(Size1, List1, Value),
    List = [Value | List1].

%define translations
traducao(0, 32). %space
traducao(1, 9634). %square
traducao(2, 9711). %ball
traducao(3, 9635). %filled square
traducao(4, 9673). %filled ball
traducao(-1, 9472). %horizontal line
traducao(Val, Translated):-Translated is Val.