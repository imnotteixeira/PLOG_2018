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

% Entry Point
main:-
    now(T),
    setrand(T),
    write(' -+*+- VIRUS WARS -+*+- '), nl, nl,
    write('Select Game Mode <0-3>:'), nl,
    write('1 - Human vs Human'),nl,
    write('2 - Human vs Computer (Lv.1)'),nl,
    write('3 - Human vs Computer (Lv.2)'),nl,
    write('4 - Human vs Computer (Lv.3)'),nl,
    write('5 - Computer vs Computer (Lv. 1)'),nl,
    write('6 - Computer vs Computer (Lv. 2)'),nl,
    write('7 - Computer vs Computer (Lv. 3)'),nl,
    write('0 - Quit'),nl,
    read(Selection),
    parse_game_mode(Selection).

% Game menu options selectors
parse_game_mode(0).
parse_game_mode(1):-
    start_gameplay(Board),
    game(Board, 0-0, 1-0, 5).
parse_game_mode(2):-
    start_gameplay(Board),
    game(Board, 0-0, 1-1, 5).
parse_game_mode(3):-
    start_gameplay(Board),
    game(Board, 0-0, 1-2, 5).
parse_game_mode(4):-
    start_gameplay(Board),
    game(Board, 0-0, 1-3, 5).
parse_game_mode(5):-
    start_gameplay(Board),
    game(Board, 0-1, 1-1, 5).
parse_game_mode(6):-
    start_gameplay(Board),
    game(Board, 0-2, 1-2, 5).
parse_game_mode(7):-
    start_gameplay(Board),
    game(Board, 0-3, 1-3, 5).

parse_game_mode(_):-
    write('Invalid Choice. Choose between <0-4>.'), nl,
    read(Selection),
    parse_game_mode(Selection), !.

