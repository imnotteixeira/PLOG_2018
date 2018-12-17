:-use_module(library(clpfd)).

main(FirstNumber, AmountOfNumbers, Multiplier):-
    getMaxNumber(FirstNumber, AmountOfNumbers, Multiplier, Max),
    length(ResultList, AmountOfNumbers),
    domain(ResultList, 1, Max),
    getDigitsInNumber(Max, NbrOfDigits, 0),
    getCoeffs(NbrOfDigits, Coeffs, 1),
    element(1, ResultList, FirstNumber),
    generateNumbers(ResultList, Multiplier, Coeffs, NbrOfDigits, 1),
    write('.'),
    labeling([], ResultList),
    write(ResultList).

getMaxNumber(Max,1,_,Max).
getMaxNumber(FirstNumber, AmountOfNumbers, Multiplier, Max):-
    NextNumber is FirstNumber * Multiplier,
    NextAmount is AmountOfNumbers - 1,
    getMaxNumber(NextNumber, NextAmount, Multiplier, Max).

generateNumbers(List, Multiplier, Coeffs, NbrOfDigits, Counter):-
    length(List, Counter),
    element(Counter, List, LastElement),
    element(1, List, FirstElement),
    (FirstElement #= LastElement * Multiplier) #\/ (LastElement #>= 10 #/\ FirstElement #= LastElementWithRemovedDigit),
    generateRestrictedNumberRemoveDigit(LastElement, Coeffs, NbrOfDigits, LastElementWithRemovedDigit).

generateNumbers(List, Multiplier, Coeffs, NbrOfDigits, Counter):-
    element(Counter, List, CurrElement),
    NextCounter is Counter + 1,
    element(NextCounter, List, NextElement),
    (NextElement #= CurrElement * Multiplier) #\/ (CurrElement #>= 10 #/\ NextElement #= CurrElementWithRemovedDigit),
    generateRestrictedNumberRemoveDigit(CurrElement, Coeffs, NbrOfDigits, CurrElementWithRemovedDigit),
    generateNumbers(List, Multiplier, Coeffs, NbrOfDigits, NextCounter).

generateRestrictedNumberRemoveDigit(Number, Coeffs, NbrOfDigits, NextNumber):-
    trace,
    Number #> NextNumber,
    I #>= 2, I #=< NbrOfDigits,
    Iminus1 #= I - 1,
    element(I, Coeffs, PowerI),
    element(Iminus1, Coeffs, PowerIminus1),
    LeftSide #= Number div PowerI,
    RightSide #= Number div PowerIminus1,
    NextNumber #= LeftSide * PowerIminus1 + RightSide.

getDigitsInNumber(0, Nbr, Nbr).
getDigitsInNumber(Nbr, NbrOfDigits, Temp):-
    NextNbr #= Nbr div 10,
    NextTemp #= Temp + 1,
    getDigitsInNumber(NextNbr, NbrOfDigits, NextTemp).

getCoeffs(0, [], _).
getCoeffs(NbrOfDigits, [H | T], CurrCoef):-
    H is CurrCoef,
    NextCoef is CurrCoef * 10,
    NextNbrOfDigits is NbrOfDigits - 1,
    getCoeffs(NextNbrOfDigits, T, NextCoef).