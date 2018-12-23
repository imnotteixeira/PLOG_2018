:- consult('PLOG_TP2_Power_Strike_5.pl').

generate(FirstNumber, AmountOfNumbers, Multiplier):-
    now(T),
    setrand(T),
    random(1, 10, FirstNumber),
    random(2, 7, AmountOfNumbers),
    random(2, 10, Multiplier),
    run(FirstNumber, AmountOfNumbers, Multiplier, _).

generate(A,B,C):- generate(A,B,C).