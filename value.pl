% 1º zombie streaks
% 2º proximidade a enemies (possibilidade de obter zombies)
value(Board, Player, Value):-
    win_condition(Board, Player, WinVal, 0),
    zombie_value(Board, Player, ZVal, 0),
    proximity_value(Board, Player, ProximityVal, 0, 0, Board),
    Value is (WinVal ** 3) + (ZVal ** 2) + ProximityVal.

win_condition([], _Player, 1000, 1).
win_condition([], _Player, 1000, _).
win_condition([Row | Tail], Player, WinVal, ActiveEnemyCount):-
    row_win_condition(Row, Player, RowActiveEnemyCount, 0), !,
    NextCount is ActiveEnemyCount + RowActiveEnemyCount,
    win_condition(Tail, Player, WinVal, NextCount).

row_win_condition([], _Player, Counter, Counter).
row_win_condition([Cell | Tail], Player, Val, Counter):-
    enemy(Player, Enemy),
    non_zombie(Enemy, Cell), !, 
    NextCounter = Counter + 1,
    row_win_condition(Tail, Player, Val, NextCounter).

row_win_condition([Cell | Tail], Player, Val, Counter):-
    row_win_condition(Tail, Player, Val, Counter).





zombie_value([], _Player, Value, Value).

zombie_value([Row | Tail], Player, Value, Counter):-
    zombie_row_value(Row, Player, RowVal, 0, 0),
    NewCounter is Counter + (RowVal ** 2),
    zombie_value(Tail, Player, Value, NewCounter).



zombie_row_value([], _Player, RowVal, RowVal, _Curr_Seq).

zombie_row_value([Elem | Tail], Player, RowVal, Max_Seq, Curr_Seq):-
    zombie(Player, Elem),
    New_Curr_Seq is Curr_Seq + 1,
    New_Curr_Seq > Max_Seq,
    zombie_row_value(Tail, Player, RowVal, New_Curr_Seq, New_Curr_Seq).

zombie_row_value([Elem | Tail], Player, RowVal, Max_Seq, Curr_Seq):-
    zombie(Player, Elem),
    New_Curr_Seq is Curr_Seq + 1,
    zombie_row_value(Tail, Player, RowVal, Max_Seq, New_Curr_Seq).

zombie_row_value([_ | Tail], Player, RowVal, Max_Seq, _Curr_Seq):-
    zombie_row_value(Tail, Player, RowVal, Max_Seq, 0).


proximity_value([], _Player, Value, Value, _Y, _Board).

proximity_value([Row | Tail], Player, Value, Counter, Y, Board):-
    proximity_row_value(Row, Player, RowVal, 0, 0, Y, Board),
    NewCounter is Counter + RowVal,
    NewY is Y + 1,
    proximity_value(Tail, Player, Value, NewCounter, NewY, Board).


proximity_row_value([], _Player, RowVal, RowVal, _CurrX, _CurrY, _Board).

proximity_row_value([_Elem | Tail], Player, RowVal, CurrVal, CurrX, CurrY, Board):-
    has_enemy_adjacent(Player, CurrX, CurrY, Board),
    New_Curr_Val is CurrVal + 1,
    NewX is CurrX + 1,
    proximity_row_value(Tail, Player, RowVal, New_Curr_Val, NewX, CurrY, Board).

proximity_row_value([_ | Tail], Player, RowVal, CurrVal, CurrX, CurrY, Board):-
    NewX is CurrX + 1,
    proximity_row_value(Tail, Player, RowVal, CurrVal, NewX, CurrY, Board).


has_enemy_adjacent(Player, X, Y, Matrix):-
    RightX is X + 1,
    getElementInCoords(Matrix, RightX, Y, Elem),
    enemy_non_zombie(Player, Elem).

has_enemy_adjacent(Player, X, Y, Matrix):-
    LeftX is X - 1,
    getElementInCoords(Matrix, LeftX, Y, Elem),
    enemy_non_zombie(Player, Elem).

has_enemy_adjacent(Player, X, Y, Matrix):-
    BottomY is Y + 1,
    getElementInCoords(Matrix, X, BottomY, Elem),
    enemy_non_zombie(Player, Elem).

has_enemy_adjacent(Player, X, Y, Matrix):-
    TopY is Y - 1,
    getElementInCoords(Matrix, X, TopY, Elem),
    enemy_non_zombie(Player, Elem).

has_enemy_adjacent(Player, X, Y, Matrix):-
    TopY is Y - 1,
    RightX is X + 1,
    getElementInCoords(Matrix, RightX, TopY, Elem),
    enemy_non_zombie(Player, Elem).

has_enemy_adjacent(Player, X, Y, Matrix):-
    TopY is Y - 1,
    LeftX is X - 1,
    getElementInCoords(Matrix, LeftX, TopY, Elem),
    enemy_non_zombie(Player, Elem).

has_enemy_adjacent(Player, X, Y, Matrix):-
    BottomY is Y + 1,
    RightX is X + 1,
    getElementInCoords(Matrix, RightX, BottomY, Elem),
    enemy_non_zombie(Player, Elem).

has_enemy_adjacent(Player, X, Y, Matrix):-
    BottomY is Y + 1,
    LeftX is X - 1,
    getElementInCoords(Matrix, LeftX, BottomY, Elem),
    enemy_non_zombie(Player, Elem).