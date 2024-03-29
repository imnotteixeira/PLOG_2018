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
    valid_moves(Board, Player, ValidMoves), !, % get possible moves for player
    length(ValidMoves, NMoves), % get number of possible moves to generate a random one
    random(0, NMoves, Move), % generate a random index for choosing the selected move
    nth0(Move, ValidMoves, X-Y). % get the chosen move from the random index
    

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


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%% SMART AI %%%%%%%%%%%%%%%%%%

ai_move(Board, Player, X, Y):-
    valid_moves_valued(Board, Player, ValidMoves), !, % get possible moves for player
    get_best_moves(ValidMoves, BestMoves), % get the moves that generate a move favorable board
    length(BestMoves, NMoves), % get number of selected moves to generate a random one
    random(0, NMoves, MoveIndex), % generate a random index for choosing the selected move
    nth0(MoveIndex, BestMoves, X-Y). % get the chosen move from the random index

get_best_moves(Moves, BestMoves):-
    nth0(0, Moves, Val-_X-_Y), % get the first value for max (next line)
    get_max_move_value(Moves, MaxVal, Val), % get best value of board from possible moves
    find_moves_by_value(Moves, MaxVal, BestMoves). % get all moves that result in the previously calculated best value


get_max_move_value([], CurrMax, CurrMax).

get_max_move_value([Val-_X-_Y | Tail], Max, CurrMax):-
    Val > CurrMax, !,
    get_max_move_value(Tail, Max, Val).

get_max_move_value([Val-_X-_Y | Tail], Max, CurrMax):-
    get_max_move_value(Tail, Max, CurrMax).


get_min_move_value([], CurrMin, CurrMin).

get_min_move_value([Val-_X-_Y | Tail], Min, CurrMin):-
    Val < CurrMin, !,
    get_min_move_value(Tail, Min, Val).

get_min_move_value([Val-_X-_Y | Tail], Min, CurrMin):-
    get_min_move_value(Tail, Min, CurrMin).


find_moves_by_value([], _Val, []).

find_moves_by_value([Val-X-Y | Tail], TargetVal, [ X-Y | Moves]):-
    Val =:= TargetVal, !,
    find_moves_by_value(Tail, TargetVal, Moves).

find_moves_by_value([Val-_X-_Y | Tail], TargetVal, Moves):-
    find_moves_by_value(Tail, TargetVal, Moves).
    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%% SMARTER AI %%%%%%%%%%%%%%%%

hard_ai_move(Board, Player, X, Y):-
    valid_moves(Board, Player, FirstLevelValidMoves), !, % get possible moves for player
    get_oponent_max_values_for_valid_moves(Board, Player, FirstLevelValidMoves, SecondLevelValidMoves), !, % get max enemy counter-move values for each possible player move
    nth0(0, SecondLevelValidMoves, Val-_-_), % get the first value for min (next line)
    get_min_move_value(SecondLevelValidMoves, MinVal, Val), % get min value of the max values of counter-moves
    find_moves_by_value(SecondLevelValidMoves, MinVal, SelectedMoves), % get all moves that generate counter-moves whose max is equal to the min calculated in last line 
    length(SelectedMoves, NMoves), % get number of selected moves to generate a random one
    random(0, NMoves, MoveIndex), % generate a random index for choosing the selected move
    nth0(MoveIndex, SelectedMoves, X-Y). % get the chosen move from the random index
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_oponent_max_values_for_valid_moves(_, _, [], []).
 
get_oponent_max_values_for_valid_moves(Board, Player, [SrcX-SrcY | FirstLevelTail], [MaxVal-SrcX-SrcY | NewSecondLevelMoves]):-
    enemy(Player, Enemy), % get enemy player
    play(Board, Player-SrcX-SrcY, CurrPlayBoard), % simulate a move
    valid_moves_valued(CurrPlayBoard, Enemy, EnemyMoves), % get valid oponent moves for the above simulated move
    \+ length(EnemyMoves, 0), !, % check if oponent has any moves
    nth0(0, EnemyMoves, Val-_-_), % get the first value for max (next line)
    get_max_move_value(EnemyMoves, MaxVal, Val), % get the max value of a possible enemy move after the simulated move
    get_oponent_max_values_for_valid_moves(Board, Player, FirstLevelTail, NewSecondLevelMoves). % recursive call to populate list
   
get_oponent_max_values_for_valid_moves(Board, Player, [SrcX-SrcY | FirstLevelTail], [1000-SrcX-SrcY | NewSecondLevelMoves]):- % make sure oponent has a high value if it has no moves
    get_oponent_max_values_for_valid_moves(Board, Player, FirstLevelTail, NewSecondLevelMoves). % recursive call to populate list

