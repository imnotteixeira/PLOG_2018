:- use_module(library(random)).
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
%%%%%%%%% SMART AI %%%%%%%%%%%%%%%%%%

ai_move(Board, Player, X, Y):-
    valid_moves_valued(Board, Player, ValidMoves), !,
    get_best_moves(ValidMoves, BestMoves),
    length(BestMoves, NMoves),
    random(0, NMoves, MoveIndex),
    nth0(MoveIndex, BestMoves, X-Y).






get_best_moves(Moves, BestMoves):-
    nth0(0, Moves, Val-X-Y),
    get_max_move_value(Moves, MaxVal, Val),
    find_moves_by_value(Moves, MaxVal, BestMoves).

get_max_move_value([], CurrMax, CurrMax).

get_max_move_value([Val-X-Y | Tail], Max, CurrMax):-
    Val > CurrMax, !,
    get_max_move_value(Tail, Max, Val).

get_max_move_value([Val-X-Y | Tail], Max, CurrMax):-
    get_max_move_value(Tail, Max, CurrMax).

find_moves_by_value([], _Val, []).

find_moves_by_value([Val-X-Y | Tail], TargetVal, [ X-Y | Moves]):-
    Val =:= TargetVal, !,
    find_moves_by_value(Tail, TargetVal, Moves).

find_moves_by_value([Val-X-Y | Tail], TargetVal, Moves):-
    find_moves_by_value(Tail, TargetVal, Moves).
    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%% SMART AI %%%%%%%%%%%%%%%%%%

hard_ai_move(Board, Player, X, Y):-
    valid_moves(Board, Player, FirstLevelValidMoves), !,
    get_valid_oponent_moves(Board, Player, FirstLevelValidMoves, SecondLevelValidMoves), !,
    
    get_best_source_moves(SecondLevelValidMoves, WorstMoves),
    length(WorstMoves, NMoves),
    random(0, NMoves, MoveIndex),

    write('UpperLimit: '), 
    write(UpperLimit), nl,
    write('MoveIndex: '), 
    write(MoveIndex), nl,
    nth0(MoveIndex, WorstMoves, X-Y).    
    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

simulate_move(Board, Player, X-Y, Value):-
    play(Board, Player-X-Y, NewBoard),
    value(NewBoard, Player, Value), !.

valued_valid_move(Board, Player, X-Y, Value):-
    valid_move(Board, Player, X-Y),
    simulate_move(Board, Player, X-Y, Value).

valid_moves_valued(Board, Player, ListOfMoves):-
    findall(Value-X-Y, valued_valid_move(Board, Player, X-Y, Value), ListOfMoves).

valid_moves_ordered_by_value(Board, Player, ListOfMoves):-
    setof(Value-X-Y, valued_valid_move(Board, Player, X-Y, Value), ListOfMoves).

get_valid_oponent_moves(_, _, [], []).
 
get_valid_oponent_moves(Board, Player, [SrcX-SrcY | FirstLevelTail], SecondLevelValidMoves):-
    enemy(Player, Enemy), % get enemy player
    play(Board, Player-SrcX-SrcY, CurrPlayBoard), % simulate a move
    valid_moves_valued(CurrPlayBoard, Enemy, EnemyMoves), % get valid oponent moves for the above simulated move
    length(EnemyMoves, 0), !, 
    SecondLevelValidMoves = [SrcX-SrcY-1000-99-99].% populate the result.

get_valid_oponent_moves(Board, Player, [SrcX-SrcY | FirstLevelTail], SecondLevelValidMoves):-
    enemy(Player, Enemy), % get enemy player
    play(Board, Player-SrcX-SrcY, CurrPlayBoard), % simulate a move
    valid_moves_valued(CurrPlayBoard, Enemy, EnemyMoves), % get valid oponent moves for the above simulated move
    attach_src_move_to_counter_moves(SrcX, SrcY, EnemyMoves, EnemyMovesWithSource),
    get_valid_oponent_moves(Board, Player, FirstLevelTail, NewSecondLevelMoves), % get remaining oponent moves (recusrsion)
    append(EnemyMovesWithSource, NewSecondLevelMoves, SecondLevelValidMoves).% populate the result

attach_src_move_to_counter_moves(_SrcX, _SrcY, [], []).
attach_src_move_to_counter_moves(SrcX, SrcY, [Val-X-Y | Tail], [SrcX-SrcY-Val-X-Y | EnemyMovesWithSource ]):-
    attach_src_move_to_counter_moves(SrcX, SrcY, Tail, EnemyMovesWithSource).


get_min_counter_move_value([], CurrMin, CurrMin).

get_min_counter_move_value([SrcX-SrcY-Val-X-Y | Tail], Max, CurrMin):-
    Val < CurrMin, !,
    get_min_counter_move_value(Tail, Max, Val).

get_min_counter_move_value([SrcX-SrcY-Val-X-Y | Tail], Max, CurrMin):-
    get_min_counter_move_value(Tail, Max, CurrMin).




get_best_source_moves(Moves, BestMoves):-
    nth0(0, Moves, SrcX-SrcY-Val-X-Y),
    get_min_counter_move_value(Moves, MinVal, Val),
    find_source_moves_by_counter_value(Moves, MinVal, BestMoves).

find_source_moves_by_counter_value([], _Val, []).

find_source_moves_by_counter_value([SrcX-SrcY-Val-X-Y | Tail], TargetVal, [ SrcX-SrcY | Moves]):-
    Val =:= TargetVal, !,
    find_source_moves_by_counter_value(Tail, TargetVal, Moves).

find_source_moves_by_counter_value([SrcX-SrcY-Val-X-Y | Tail], TargetVal, Moves):-
    find_source_moves_by_counter_value(Tail, TargetVal, Moves).

