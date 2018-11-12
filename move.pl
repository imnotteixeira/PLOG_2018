:- use_module(library(random)).

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
    valid_moves(Board, Player, ValidMoves),
    length(ValidMoves, NMoves),
    random(0, NMoves, Move),
    nth0(Move, ValidMoves, X-Y).
    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%% SMART AI %%%%%%%%%%%%%%%%%

ai_move(Board, Player, X, Y):-
    write('be4 vmoves'),
    valid_moves_ordered_by_value(Board, Player, ValidMoves),
    write('after vmoves'),
    nth0(0, ValidMoves, Value-X-Y).
    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

simulate_move(Board, Player, X-Y, Value):-
    play(Board, Player-X-Y, NewBoard),
    value(NewBoard, Player, Value).

valid_moves_ordered_by_value(Board, Player, ListOfMoves):-
    setof(Value-X-Y, (valid_move(Board, Player, X-Y), !, simulate_move(Board, Player, X-Y, Value)), ListOfMoves).


