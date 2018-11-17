:- use_module(library(system)).
:- consult('replacer.pl').
:- consult('display.pl').
:- consult('game_rules.pl').
:- consult('game.pl').
:- consult('move.pl').
:- consult('value.pl').

%abstractions for game states demo
display_start_game:-
    start_gameplay(Board),
    display_game(Board, 0).
display_mid_game:-
    mid_gameplay(Board),
    display_game(Board, 1).
display_final_game:-
    final_gameplay(Board),
    display_game(Board, 0).
    
%generates start board
start_gameplay(L):-
    L = [
        [1,0,0,0,0,0,0,0,0,0,0],
        [0,0,0,0,0,0,0,0,0,0,0],
        [0,0,0,0,0,0,0,0,0,0,0],
        [0,0,0,0,0,0,0,0,0,0,0],
        [0,0,0,0,0,0,0,0,0,0,0],
        [0,0,0,0,0,0,0,0,0,0,0],
        [0,0,0,0,0,0,0,0,0,0,0],
        [0,0,0,0,0,0,0,0,0,0,0],
        [0,0,0,0,0,0,0,0,0,0,0],
        [0,0,0,0,0,0,0,0,0,0,0],
        [0,0,0,0,0,0,0,0,0,0,2]
    ].

%generates mid-game board
mid_gameplay(L):-
    L = [
        [1,1,3,0,0,0,0,0,0,0,0],
        [1,3,0,0,0,0,0,0,0,0,0],
        [0,0,3,3,0,0,0,0,0,0,0],
        [0,0,0,0,2,0,0,0,0,0,0],
        [0,0,0,0,2,2,0,0,0,0,0],
        [0,0,0,3,3,2,0,0,0,0,0],
        [0,2,2,0,0,0,2,2,2,2,2],
        [0,0,0,0,0,0,0,0,0,0,2],
        [0,0,0,0,0,0,0,0,0,0,2],
        [0,0,0,0,0,0,0,0,0,0,2],
        [0,0,0,0,0,0,0,0,0,0,2]
    ].

%generates end board
final_gameplay(L):-
    L = [
        [1,1,3,0,0,0,0,0,0,0,0],
        [1,3,0,0,0,0,0,0,0,0,0],
        [0,0,3,3,0,3,0,0,0,0,0],
        [0,0,1,0,3,3,3,0,0,0,0],
        [0,0,0,0,3,3,3,0,0,0,0],
        [0,0,1,3,3,3,1,1,0,0,0],
        [0,3,3,1,0,0,3,3,3,3,3],
        [0,0,0,0,0,0,1,1,0,3,3],
        [0,0,0,0,0,0,0,0,0,3,3],
        [0,0,0,0,0,0,0,0,0,1,3],
        [0,0,0,0,0,0,0,0,0,0,3]
    ].

test(L):-
    L = [
        [1,1,3,0,0,0,0,0,0,0,0],
        [1,3,0,0,0,0,0,0,0,0,0],
        [0,0,3,3,0,3,0,0,0,0,0],
        [0,0,1,0,3,3,3,0,0,0,0],
        [0,0,0,0,3,3,3,0,0,0,0],
        [0,0,4,2,2,3,1,1,0,0,0],
        [0,3,3,1,0,0,3,3,3,3,3],
        [0,0,0,0,0,0,1,1,0,3,3],
        [0,0,0,0,0,0,0,0,0,3,3],
        [0,0,0,0,0,0,0,0,0,1,3],
        [0,0,0,0,0,0,0,0,0,0,3]
    ].

% Entry Point
main:-
    now(T),
    setrand(T),
    write(' -+*+- VIRUS WARS -+*+- '), nl, nl,
    display_player_options,
    catch(read(Player0Type),_,fail),
    display_player_options,
    catch(read(Player1Type),_,fail),
    start_gameplay(Board),
    game(Board, 0-Player0Type, 1-Player1Type, 5).

main:-
    write('Invalid Choice. Try again.'), nl.

display_player_options:-
    write('Select Player Mode <0-3>:'), nl,
    write('0 - Human'),nl,
    write('1 - Computer (Lv.1)'),nl,
    write('2 - Computer (Lv.2)'),nl,
    write('3 - Computer (Lv.3)'),nl.