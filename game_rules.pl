:- use_module(library(lists)).

:- dynamic verifiedCell/2.

enemy(0, 1).
enemy(1, 0).
zombie(0, 3).
zombie(1, 4).
non_zombie(0, 1).
non_zombie(1, 2).
enemy_zombie(0, 4).
enemy_zombie(1, 3).

game_over(Board, 0) :-
    \+ valid_moves(Board, 1, _), !.

game_over(Board, 1) :-
    \+ valid_moves(Board, 0, _), !.

valid_moves(Board, Player, ListOfMoves):-
    setof(X-Y, valid_move(Board, Player, X-Y), ListOfMoves).

valid_move(Board, Player, X-Y):-
    available_cell(X, Y, Board),
    abolish(verifiedCell/2),
    asserta( ( verifiedCell(_,_):- fail ) ),
    has_active_chain(Board, Player, X, Y).

valid_move(Board, Player, X-Y):-
    enemy_non_zombie(Player, X, Y, Board),
    abolish(verifiedCell/2),
    asserta( ( verifiedCell(_,_):- fail ) ),
    has_active_chain(Board, Player, X, Y).

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
    Y < BoardSize, !,
    nth0(Y, Matrix, YElem),
    nth0(X, YElem, Elem).

has_active_chain(Board, Player, X, Y):-
    \+ verifiedCell(X, Y), !,
    asserta( ( verifiedCell(X, Y) ) ), % mark cell as "seen"
    has_active_chain_aux(Board, Player, X, Y).



has_active_chain_aux(Board, Player, X, Y):-
    RightX is X + 1,
    non_zombie(Player, ActiveVirus),
    getElementInCoords(Board, RightX, Y, Elem),
    Elem =:= ActiveVirus, !.

has_active_chain_aux(Board, Player, X, Y):-
    LeftX is X - 1,
    non_zombie(Player, ActiveVirus),
    getElementInCoords(Board, LeftX, Y, Elem),
    Elem =:= ActiveVirus, !.

has_active_chain_aux(Board, Player, X, Y):-
    BottomY is Y + 1,
    non_zombie(Player, ActiveVirus),
    getElementInCoords(Board, X, BottomY, Elem),
    Elem =:= ActiveVirus, !.

has_active_chain_aux(Board, Player, X, Y):-
    TopY is Y - 1,
    non_zombie(Player, ActiveVirus),
    getElementInCoords(Board, X, TopY, Elem),
    Elem =:= ActiveVirus, !.

has_active_chain_aux(Board, Player, X, Y):-
    TopY is Y - 1,
    RightX is X + 1,
    non_zombie(Player, ActiveVirus),
    getElementInCoords(Board, RightX, TopY, Elem),
    Elem =:= ActiveVirus, !.

has_active_chain_aux(Board, Player, X, Y):-
    BottomY is Y + 1,
    LeftX is X - 1,
    non_zombie(Player, ActiveVirus),
    getElementInCoords(Board, LeftX, BottomY, Elem),
    Elem =:= ActiveVirus, !.

has_active_chain_aux(Board, Player, X, Y):-
    TopY is Y - 1,
    LeftX is X - 1,
    non_zombie(Player, ActiveVirus),
    getElementInCoords(Board, LeftX, TopY, Elem),
    Elem =:= ActiveVirus, !.

has_active_chain_aux(Board, Player, X, Y):-
    BottomY is Y + 1,
    RightX is X + 1,
    non_zombie(Player, ActiveVirus),
    getElementInCoords(Board, RightX, BottomY, Elem),
    Elem =:= ActiveVirus, !.





has_active_chain_aux(Board, Player, X, Y):-
    RightX is X + 1,
    zombie(Player, Zombie),
    getElementInCoords(Board, RightX, Y, Elem),
    Elem =:= Zombie,
    has_active_chain(Board, Player, RightX, Y), !.

has_active_chain_aux(Board, Player, X, Y):-
    LeftX is X - 1,
    zombie(Player, Zombie),
    getElementInCoords(Board, LeftX, Y, Elem),
    Elem =:= Zombie,
    has_active_chain(Board, Player, LeftX, Y), !.

has_active_chain_aux(Board, Player, X, Y):-
    BottomY is Y + 1,
    zombie(Player, Zombie),
    getElementInCoords(Board, X, BottomY, Elem),
    Elem =:= Zombie,
    has_active_chain(Board, Player, X, BottomY), !.

has_active_chain_aux(Board, Player, X, Y):-
    TopY is Y - 1,
    zombie(Player, Zombie),
    getElementInCoords(Board, X, TopY, Elem),
    Elem =:= Zombie,
    has_active_chain(Board, Player, X, TopY), !.

has_active_chain_aux(Board, Player, X, Y):-
    TopY is Y - 1,
    RightX is X + 1,
    zombie(Player, Zombie),
    getElementInCoords(Board, RightX, TopY, Elem),
    Elem =:= Zombie,
    has_active_chain(Board, Player, RightX, TopY), !.

has_active_chain_aux(Board, Player, X, Y):-
    TopY is Y - 1,
    LeftX is X - 1,
    zombie(Player, Zombie),
    getElementInCoords(Board, LeftX, TopY, Elem),
    Elem =:= Zombie,
    has_active_chain(Board, Player, LeftX, TopY), !.

has_active_chain_aux(Board, Player, X, Y):-
    BottomY is Y + 1,
    RightX is X + 1,
    zombie(Player, Zombie),
    getElementInCoords(Board, RightX, BottomY, Elem),
    Elem =:= Zombie,
    has_active_chain(Board, Player, RightX, BottomY), !.

has_active_chain_aux(Board, Player, X, Y):-
    BottomY is Y + 1,
    LeftX is X - 1,
    zombie(Player, Zombie),
    getElementInCoords(Board, LeftX, BottomY, Elem),
    Elem =:= Zombie,
    has_active_chain(Board, Player, LeftX, BottomY), !.
