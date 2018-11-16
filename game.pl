game(NewBoard, Player1-Type1, Player2-Type2, 0):-
    game(NewBoard, Player2-Type2, Player1-Type1, 5).

game(Board, Player1-Type1, Player2-Type2, PlayCount):-
    
    game_over(Board, Winner), !,
    display_game(Board, Player1),
    write('Game Over! Winner is Player '),
    write(Winner), nl.

game(Board, Player1-Type1, Player2-Type2, PlayCount):-
    display_game(Board, Player1),
    write(PlayCount),
    write(' moves left.'), nl,
    next_move(Player1, Type1, Board, NewBoard),
    NextPlayCount is PlayCount - 1,
    game(NewBoard, Player1-Type1, Player2-Type2, NextPlayCount).

%%%%%%%%%%% READ MOVE %%%%%%%%%%%%%%%%

read_move(X, Y):-
    write('Insert the next play coordinates <A-K><0-10>. :'),
    get_code(X1),
    read(Y),
    get_code(_),
    integer(Y),
    X is X1 - "A".

read_move(X,Y):-
    write('That is not a valid move!'), nl,
    read_move(X,Y).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

human(0).
random_ai(1).
beginner_ai(2).

%%%%%%%%%%% GET NEXT MOVE %%%%%%%%%%%%%%%%%

next_move(Player, Type, Board, NewBoard):-
    human(Type), !,
    read_move(X, Y),
    move(Player-X-Y, Board, NewBoard).

next_move(Player, Type, Board, NewBoard):-
    choose_move(Board, Player-Type, X-Y),
    play(Board, Player-X-Y, NewBoard).


choose_move(Board, Player-Type, X-Y):-
    random_ai(Type), !,
    random_move(Board, Player, X, Y).

choose_move(Board, Player-Type, X-Y):-
    beginner_ai(Type), !,
    ai_move(Board, Player, X, Y).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%