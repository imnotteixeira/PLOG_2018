:-use_module(library(clpfd)).
:-use_module(library(random)).
:-use_module(library(system)).

getPuzzleInputFromSize(1, 6, 5, 2).
getPuzzleInputFromSize(2, 9, 8, 2).
getPuzzleInputFromSize(3, 41, 8, 7).

generate(FirstNumber, AmountOfNumbers, Multiplier):-
    now(T),
    setrand(T),
    random(1, 10, FirstNumber),
    random(2, 7, AmountOfNumbers),
    random(2, 10, Multiplier),
    run(FirstNumber, AmountOfNumbers, Multiplier, _).

generate(A,B,C):- generate(A,B,C).


start:-
    nl,
    write('Welcome to power strike!'), nl, nl,
    getInput(FirstNumber, AmountOfNumbers, Multiplier), nl,
    write('Calculating solution for problem with:'), nl,
    write('Initial value: '), write(FirstNumber), nl,
    write('Amount of values: '), write(AmountOfNumbers), nl,
    write('Multiplier: '), write(Multiplier), nl, nl,
    run(FirstNumber, AmountOfNumbers, Multiplier, ResultList),
    write('The result is: '), write(ResultList), nl.

getInput(FirstNumber, AmountOfNumbers, Multiplier):-
    write('Please insert the puzzle size (1-3): '), nl, nl,
    write('1 - Small'), nl,
    write('2 - Medium'), nl,
    write('3 - Big'), nl,
    catch(read(PuzzleSize),_,fail),
    getPuzzleInputFromSize(PuzzleSize, FirstNumber, AmountOfNumbers, Multiplier).

getInput(FirstNumber, AmountOfNumbers, Multiplier):-
    write('That is not a valid input. Please try again'), nl,
    getInput(FirstNumber, AmountOfNumbers, Multiplier).

    

run(FirstNumber, AmountOfNumbers, Multiplier, ResultList):-
    run_profiling_mode(FirstNumber, AmountOfNumbers, Multiplier, ResultList, [ff, bisect, up], _, _).


run_profiling_mode(FirstNumber, AmountOfNumbers, Multiplier, ResultList, O, Time, Flag):-
    getMaxNumber(FirstNumber, AmountOfNumbers, Multiplier, Max),
    length(ResultList, AmountOfNumbers),
    domain(ResultList, 1, Max),
    getDigitsInNumber(Max, NbrOfDigits, 0),

    CoeffsSize is NbrOfDigits + 1,
    length(Coeffs, CoeffsSize),
    element(1, Coeffs, 1),
    generateCoeffs(Coeffs),

    element(1, ResultList, FirstNumber),!,
    generateNumbers(ResultList, Multiplier, Coeffs, 1, Powers, []),
    append(Powers, ResultList, Vars),

    statistics(runtime, [Start, _]),
    append(O, [time_out(5000, Flag)], Options),
    labeling(Options, Vars),

    statistics(runtime, [End, _]),
    Time is End - Start.
    % write('Time: '),
    % write(Time).

getMaxNumber(Max,1,_,Max).
getMaxNumber(FirstNumber, AmountOfNumbers, Multiplier, Max):-
    NextNumber is FirstNumber * Multiplier,
    NextAmount is AmountOfNumbers - 1,
    getMaxNumber(NextNumber, NextAmount, Multiplier, Max).

generateNumbers(List, Multiplier, Coeffs, Counter, Powers, PowersTemp):-
    length(List, Counter),
    element(Counter, List, LastElement),
    element(1, List, FirstElement),
    generateRestrictedNumberRemoveDigit(LastElement, Coeffs, LastElementWithRemovedDigit, GeneratedPower),
    (FirstElement #= LastElement * Multiplier #/\ GeneratedPower #= 1) #\/ (LastElement #>= 10 #/\ FirstElement #= LastElementWithRemovedDigit),
    append(PowersTemp, [GeneratedPower], Powers).

generateNumbers(List, Multiplier, Coeffs, Counter, Powers, PowersTemp):-
    element(Counter, List, CurrElement),
    NextCounter is Counter + 1,
    element(NextCounter, List, NextElement),
    generateRestrictedNumberRemoveDigit(CurrElement, Coeffs, CurrElementWithRemovedDigit, GeneratedPower),
    (NextElement #= CurrElement * Multiplier #/\ GeneratedPower #= 1) #\/ (CurrElement #>= 10 #/\ NextElement #= CurrElementWithRemovedDigit),
    append(PowersTemp, [GeneratedPower], NewPowers),
    generateNumbers(List, Multiplier, Coeffs, NextCounter, Powers, NewPowers).

generateRestrictedNumberRemoveDigit(Number, Coeffs, NextNumber, Power):-
    NextNumber #\= Number,
    element(_, Coeffs, PowerIminus1),
    PowerI #= PowerIminus1 * 10,
    LeftSide #= Number // PowerI,
    RightSide #= Number mod PowerIminus1,
    NextNumber #= (LeftSide * PowerIminus1 + RightSide),
    Power = PowerIminus1.

getDigitsInNumber(0, Nbr, Nbr).
getDigitsInNumber(Nbr, NbrOfDigits, Temp):-
    NextNbr #= Nbr div 10,
    NextTemp #= Temp + 1,
    getDigitsInNumber(NextNbr, NbrOfDigits, NextTemp).


generateCoeffs([_]).
generateCoeffs([CurrCoeff, NextCoeff | Tail]):-
    NextCoeff #= CurrCoeff * 10,
    generateCoeffs([NextCoeff | Tail]). 