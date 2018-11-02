validatePlay(Player, X, Y, Matrix):-
    \+ isPlayersElementInCoords(Player, X, Y, Matrix),
    \+ isZombieInCoords(X, Y, Matrix),
    checkAdjacentsHasPlayersElem(Player, X, Y, Matrix).

isZombieInCoords(X, Y, Matrix):-
    getElementInCoords(Matrix, X, Y, Elem),
    Elem > 2.

isPlayersElementInCoords(Player, X, Y, Matrix):-
    getElementInCoords(Matrix, X, Y, Elem),
    isPlayersElement(Player, Elem).

getPlayerNewElem(Player, X, Y, Matrix, NewElem):- % player is player 1 and target space is empty
    0 =:= Player mod 2, 
    getElementInCoords(Matrix, X, Y, Elem),
    0 =:= Elem,
    NewElem is 1.

getPlayerNewElem(Player, X, Y, Matrix, NewElem):- % player is player 1 and target space is enemy
    0 =:= Player mod 2, 
    NewElem is 3.

getPlayerNewElem(Player, X, Y, Matrix, NewElem):- % player is player 2 and target space is empty
    1 =:= Player mod 2, 
    getElementInCoords(Matrix, X, Y, Elem),
    0 =:= Elem,
    NewElem is 2.

getPlayerNewElem(Player, X, Y, Matrix, NewElem):- % player is player 2 and target space is enemy
    NewElem is 4.

isPlayersElement(Player, Elem):-
    Elem =\= 0,
    Player =:= (Elem + 1) mod 2.

getElementInCoords(Matrix, X, Y, Elem):-
    nth0(Y, Matrix, YElem),
    nth0(X, YElem, Elem).



% %%%%%%%%%%% CHECK ADJACENT ELEMENTS %%%%%%%%%%%% %

checkAdjacentsHasPlayersElem(Player, X, Y, Matrix):-
    RightX is X + 1,
    isPlayersElementInCoords(Player, RightX, Y, Matrix).

checkAdjacentsHasPlayersElem(Player, X, Y, Matrix):-
    LeftX is X - 1,
    isPlayersElementInCoords(Player, LeftX, Y, Matrix).

checkAdjacentsHasPlayersElem(Player, X, Y, Matrix):-
    BottomY is Y + 1,
    isPlayersElementInCoords(Player, X, BottomY, Matrix).

checkAdjacentsHasPlayersElem(Player, X, Y, Matrix):-
    TopY is Y - 1,
    isPlayersElementInCoords(Player, X, TopY, Matrix).

checkAdjacentsHasPlayersElem(Player, X, Y, Matrix):-
    TopY is Y - 1,
    RightX is X + 1,
    isPlayersElementInCoords(Player, RightX, TopY, Matrix).

checkAdjacentsHasPlayersElem(Player, X, Y, Matrix):-
    TopY is Y - 1,
    LeftX is X - 1,
    isPlayersElementInCoords(Player, LeftX, TopY, Matrix).

checkAdjacentsHasPlayersElem(Player, X, Y, Matrix):-
    BottomY is Y + 1,
    RightX is X + 1,
    isPlayersElementInCoords(Player, RightX, BottomY, Matrix).

checkAdjacentsHasPlayersElem(Player, X, Y, Matrix):-
    BottomY is Y + 1,
    LeftX is X - 1,
    isPlayersElementInCoords(Player, LeftX, BottomY, Matrix).