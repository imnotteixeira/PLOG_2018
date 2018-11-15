:- use_module(library(random)).
:- use_module(library(system)).
:- use_module(library(between)).

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
    valid_moves(Board, Player, ValidMoves), !,
    length(ValidMoves, NMoves),
    random(0, NMoves, Move),
    nth0(Move, ValidMoves, X-Y).
    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%% SMART AI %%%%%%%%%%%%%%%%%

ai_move(Board, Player, X, Y):-
    valid_moves_ordered_by_value(Board, Player, ValidMoves), !,
    select_best_move(ValidMoves, X, Y, []).
    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

simulate_move(Board, Player, X-Y, Value):-
    play(Board, Player-X-Y, NewBoard),
    value(NewBoard, Player, Value).

valued_valid_move(Board, Player, X-Y, Value):-
    valid_move(Board, Player, X-Y), !,
    simulate_move(Board, Player, X-Y, Value), !.

valid_moves_ordered_by_value(Board, Player, ListOfMoves):-
    setof(Value-X-Y, valued_valid_move(Board, Player, X-Y, Value), ListOfMoves).


select_best_move([], X, Y, BestMoves):-
    length(BestMoves, NMoves),
    UpperLimit is NMoves -1,
    between(0, UpperLimit, Value),
    nth0(Value, BestMoves, X-Y).    

select_best_move([CurrVal-CurrX-CurrY | [ NextVal-NextX-NextY | Tail] ], X, Y, BestMoves):-
    CurrVal > NextVal, !,
    select_best_move([], X, Y, [ X-Y | BestMoves]).

select_best_move([CurrVal-CurrX-CurrY | [ NextVal-NextX-NextY | Tail] ], X, Y, BestMoves):-
    X is CurrX, 
    Y is CurrY,
    select_best_move([ NextVal-NextX-NextY | Tail], X, Y, [ X-Y | BestMoves]).



