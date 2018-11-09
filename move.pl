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