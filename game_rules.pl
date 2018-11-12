:- use_module(library(lists)).

zombie(0, 3).
zombie(1, 4).
non_zombie(0, 1).
non_zombie(1, 2).
enemy_zombie(0, 4).
enemy_zombie(1, 3).

valid_moves(Board, Player, ListOfMoves):-
    setof(X-Y, valid_move(Board, Player, X-Y), ListOfMoves).

valid_move(Board, Player, X-Y):-
    available_cell(X, Y, Board),
    has_adjacent(Player, X, Y, Board).

valid_move(Board, Player, X-Y):-
    enemy_non_zombie(Player, X, Y, Board),
    has_adjacent(Player, X, Y, Board).

available_cell(X, Y, Board):-
    nth0(Y, Board, YElem),
    nth0(X, YElem, 0).

enemy_non_zombie(Player, EnemyNonZombie):-
    enemy(Player, Enemy),
    non_zombie(Enemy, EnemyNonZombie).


enemy_non_zombie(Player, X, Y, Board):-
    enemy_non_zombie(Player, EnemyNonZombie),
    nth0(Y, Board, YElem),
    nth0(X, YElem, EnemyNonZombie).

game_over(Board, 0) :-
    \+ valid_moves(Board, 1, EnemyMoves).

game_over(Board, 1) :-
    \+ valid_moves(Board, 0, EnemyMoves).

enemy(Player, Enemy):-
    Enemy is (Player + 1) mod 2.

isPlayersElementInCoords(Player, X, Y, Matrix):-
    getElementInCoords(Matrix, X, Y, Elem),
    isPlayersElement(Player, Elem).

getPlayerNewElem(Player, X, Y, Matrix, NewElem):- % player is player 1 and target space is empty
    0 =:= Player mod 2, 
    getElementInCoords(Matrix, X, Y, Elem),
    0 =:= Elem,
    NewElem is 1.

getPlayerNewElem(Player, _X, _Y, _Matrix, NewElem):- % player is player 1 and target space is enemy
    0 =:= Player mod 2, 
    NewElem is 3.

getPlayerNewElem(Player, X, Y, Matrix, NewElem):- % player is player 2 and target space is empty
    1 =:= Player mod 2, 
    getElementInCoords(Matrix, X, Y, Elem),
    0 =:= Elem,
    NewElem is 2.

getPlayerNewElem(_Player, _X, _Y, _Matrix, NewElem):- % player is player 2 and target space is enemy
    NewElem is 4.

isPlayersElement(Player, Elem):-
    Elem =\= 0,
    Player =:= (Elem + 1) mod 2.

getElementInCoords(Matrix, X, Y, Elem):-
    length(Matrix, BoardSize),
    X > -1,
    Y > -1,
    X < BoardSize,
    Y < BoardSize,
    nth0(Y, Matrix, YElem),
    nth0(X, YElem, Elem).

djisplai_gami(Board, Player):-
    nl,
    length(Board, Length),
    gen_column_labels(Length, Labels, 65),
    write(' '), display_column_labels(Labels, '', ' '),
    display_separator,
    display_matrix(Board, 0),
    write('Player '),
    write(Player),
    write('\'s turn.'), nl.


game(Board, Player1-Type1, Player2-Type2):-
    
    game_over(Board, Winner), !,
    djisplai_gami(Board, Player1),
    write('Game Over! Winner is Player '),
    write(Winner), nl.

game(Board, Player1-Type1, Player2-Type2):-
    djisplai_gami(Board, Player1),
    next_move(Player1, Type1, Board, NewBoard),
    game(NewBoard, Player2-Type2, Player1-Type1).

%%%%%%%%%%% READ MOVE %%%%%%%%%%%%%%%%

read_move(X, Y):-
    write('Insert the next play coordinates <A-K><0-10>. :'),
    get_code(X1),
    read(Y),
    get_code(_),
    integer(Y),
    X is X1 - "A".

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
    random_ai(Type), !,
    random_move(X, Y),
    play(Board, Player-X-Y, NewBoard).

next_move(Player, Type, Board, NewBoard):-
    beginner_ai(Type),
    ai_move(X, Y),
    play(Board, Player-X-Y, NewBoard).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% %%%%%%%%%%% CHECK AGAINST ADJACENT ELEMENTS %%%%%%%%%%%% %

has_adjacent(Player, X, Y, Matrix):-
    RightX is X + 1,
    isPlayersElementInCoords(Player, RightX, Y, Matrix).

has_adjacent(Player, X, Y, Matrix):-
    LeftX is X - 1,
    isPlayersElementInCoords(Player, LeftX, Y, Matrix).

has_adjacent(Player, X, Y, Matrix):-
    BottomY is Y + 1,
    isPlayersElementInCoords(Player, X, BottomY, Matrix).

has_adjacent(Player, X, Y, Matrix):-
    TopY is Y - 1,
    isPlayersElementInCoords(Player, X, TopY, Matrix).

has_adjacent(Player, X, Y, Matrix):-
    TopY is Y - 1,
    RightX is X + 1,
    isPlayersElementInCoords(Player, RightX, TopY, Matrix).

has_adjacent(Player, X, Y, Matrix):-
    TopY is Y - 1,
    LeftX is X - 1,
    isPlayersElementInCoords(Player, LeftX, TopY, Matrix).

has_adjacent(Player, X, Y, Matrix):-
    BottomY is Y + 1,
    RightX is X + 1,
    isPlayersElementInCoords(Player, RightX, BottomY, Matrix).

has_adjacent(Player, X, Y, Matrix):-
    BottomY is Y + 1,
    LeftX is X - 1,
    isPlayersElementInCoords(Player, LeftX, BottomY, Matrix).