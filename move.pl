:- use_module(library(random)).
:- use_module(library(system)).

move(Player-X-Y, Board, NewBoard):-
    valid_move(Board, Player, X-Y), !,
    play(Board, Player-X-Y, NewBoard).

move(Player-_X-_Y, Board, NewBoard):-
    write('That is not a valid move'), nl, 
    read_move(NewX, NewY),
    move(Player-NewX-NewY, Board, NewBoard).

play(Board, Player-X-Y, NewBoard):-
    getPlayerNewElem(Player, X, Y, Board, NewElem), 
    update_matrix_at(Board, NewBoard, X, Y, NewElem).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%% RANDOM AI %%%%%%%%%%%%%%%%%

random_move(Board, Player, X, Y):-
    %%%%%% TODO PUT THIS IN MAIN %%%%
    now(T),
    setrand(T),
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    valid_moves(Board, Player, ValidMoves), !,
    length(ValidMoves, NMoves),
    random(0, NMoves, Move),
    nth0(Move, ValidMoves, X-Y).
    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%% SMART AI %%%%%%%%%%%%%%%%%

ai_move(Board, Player, X, Y):-
    % get_best_move(Board, Player, X, Y).
    valid_moves_ordered_by_value(Board, Player, ValidMoves), !, 
    nth0(0, ValidMoves, Value-X-Y).
    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

simulate_move(Board, Player, X-Y, Value):-
    write('Simulating | '),
    write(X),
    write(', '),
    write(Y),
    write(' |'), nl,
    play(Board, Player-X-Y, NewBoard),
    value(NewBoard, Player, Value),
    write('Simulated Value: '),
    write(Value), nl.


valued_valid_move(Board, Player, X-Y, Value):-
    valid_move(Board, Player, X-Y), !,
    simulate_move(Board, Player, X-Y, Value), !.

valid_moves_ordered_by_value(Board, Player, ListOfMoves):-
    setof(Value-X-Y, valued_valid_move(Board, Player, X-Y, Value), ListOfMoves).