move(Player-X-Y, Board, NewBoard):-
    valid_move(Board, Player-X-Y),
    play(Board, Player-X-Y, NewBoard).

play(Board, Player-X-Y, NewBoard):-
    getPlayerNewElem(Player, X, Y, Board, NewElem), 
    update_matrix_at(Board, NewBoard, X, Y, NewElem).