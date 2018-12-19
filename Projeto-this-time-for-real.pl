:-use_module(library(clpfd)).
:-use_module(library(between)).


wubalubadubdub(LOL):-
    findall(RIP, main(41, 8, 7, RIP), LOL),
    nl, write('Helllloo').

generate(FirstNumber, AmountOfNumbers, Multiplier):-
    between(2, 10, FirstNumber),
    between(2,10, AmountOfNumbers),
    between(2,10, Multiplier),
    main(FirstNumber, AmountOfNumbers, Multiplier, ResultList).

main(FirstNumber, AmountOfNumbers, Multiplier, ResultList):-
    getMaxNumber(FirstNumber, AmountOfNumbers, Multiplier, Max),
    length(ResultList, AmountOfNumbers),
    domain(ResultList, 1, Max),
    getDigitsInNumber(Max, NbrOfDigits, 0),

    CoeffsSize is NbrOfDigits + 1,
    length(Coeffs, CoeffsSize),
    element(1, Coeffs, 1),
    generateCoeffs(Coeffs),

    element(1, ResultList, FirstNumber),!,
    generateNumbers(ResultList, Multiplier, Coeffs, NbrOfDigits, 1, Powers, []),
    append(Powers, ResultList, Vars),

    statistics(runtime, [Start, _]),
    labeling([max_regret, bisect], Vars),
    statistics(runtime, [End, _]),
    Time is End - Start,
    write('Time: '),
    write(Time).

getMaxNumber(Max,1,_,Max).
getMaxNumber(FirstNumber, AmountOfNumbers, Multiplier, Max):-
    NextNumber is FirstNumber * Multiplier,
    NextAmount is AmountOfNumbers - 1,
    getMaxNumber(NextNumber, NextAmount, Multiplier, Max).

generateNumbers(List, Multiplier, Coeffs, NbrOfDigits, Counter, Powers, PowersTemp):-
    length(List, Counter),
    element(Counter, List, LastElement),
    element(1, List, FirstElement),
    generateRestrictedNumberRemoveDigit(LastElement, Coeffs, NbrOfDigits, LastElementWithRemovedDigit, GeneratedPower),
    (FirstElement #= LastElement * Multiplier #/\ GeneratedPower #= 1) #\/ (LastElement #>= 10 #/\ FirstElement #= LastElementWithRemovedDigit),
    append(PowersTemp, [GeneratedPower], Powers).

generateNumbers(List, Multiplier, Coeffs, NbrOfDigits, Counter, Powers, PowersTemp):-
    element(Counter, List, CurrElement),
    NextCounter is Counter + 1,
    element(NextCounter, List, NextElement),
    generateRestrictedNumberRemoveDigit(CurrElement, Coeffs, NbrOfDigits, CurrElementWithRemovedDigit, GeneratedPower),
    (NextElement #= CurrElement * Multiplier #/\ GeneratedPower #= 1) #\/ (CurrElement #>= 10 #/\ NextElement #= CurrElementWithRemovedDigit),
    append(PowersTemp, [GeneratedPower], NewPowers),
    generateNumbers(List, Multiplier, Coeffs, NbrOfDigits, NextCounter, Powers, NewPowers).

generateRestrictedNumberRemoveDigit(Number, Coeffs, NbrOfDigits, NextNumber, Power):-
    NextNumber #\= Number,
    element(Iminus1, Coeffs, PowerIminus1),
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