:- use_module(library(lists)).

valid_moves(Board, Player, ListOfMoves):-
    setof(X-Y, valid_move(Board, Player-X-Y), ListOfMoves).

valid_move(Board, Player-X-Y):-
    available_cell(X, Y, Board),
    has_adjacent(Player, X, Y, Board).

valid_move(Board, Player-X-Y):-
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
    \+ valid_moves(Board, 1, _EnemyMoves).

game_over(Board, 1) :-
    \+ valid_moves(Board, 0, _EnemyMoves).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        %% LEGACY CODE  %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

validatePlay(Player, X, Y, Matrix):-
    \+ isPlayersElementInCoords(Player, X, Y, Matrix),
    \+ isZombieInCoords(X, Y, Matrix),
    has_adjacent(Player, X, Y, Matrix).

isZombieInCoords(X, Y, Matrix):-
    getElementInCoords(Matrix, X, Y, Elem),
    Elem > 2.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

non_zombie(0, 1).
zombie(0, 3).
non_zombie(1, 2).
zombie(1, 4).

enemy(Player, Enemy):-
    Enemy is (Player + 1) mod 2.


enemy_zombie(0, 4).
enemy_zombie(1, 3).

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
    nth0(Y, Matrix, YElem),
    nth0(X, YElem, Elem).

% %%%%%%%%%%% CHECK ADJACENT ELEMENTS %%%%%%%%%%%% %

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