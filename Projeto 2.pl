:-use_module(library(clpfd)).
:-use_module(library(lists)).

main(FirstNumber, AmountOfNumbers, Multiplier):-
    getMaxNumber(FirstNumber, AmountOfNumbers, Multiplier, Max),
    getDigitsInNumber(Max, NbrOfDigits, 0),
    getCoeffs(NbrOfDigits, Coeffs),
    generateNumberListFromNumber(FirstNumber, NbrOfDigits, Coeffs, FirstNumberList),
    setRestrictions(FirstNumberList, AmountOfNumbers, Multiplier, NbrOfDigits, Coeffs).

getMaxNumber(Max,0,_,Max).
getMaxNumber(FirstNumber, AmountOfNumbers, Multiplier, Max):-
    NextNumber is FirstNumber * Multiplier,
    NextAmount is AmountOfNumbers - 1,
    getMaxNumber(NextNumber, NextAmount, Multiplier, Max).


getDigitsInNumber(0, Nbr, Nbr).

getDigitsInNumber(Nbr, NbrOfDigits, Temp):-
    NextNbr is Nbr div 10,
    NextTemp is Temp + 1,
    getDigitsInNumber(NextNbr, NbrOfDigits, NextTemp).

getCoeffs(NbrOfDigits, Coeffs):-
    getCoeffsAux(NbrOfDigits, InvertedCoeffs, 1),
    reverse(InvertedCoeffs, Coeffs).

getCoeffsAux(0, [], _).
getCoeffsAux(NbrOfDigits, [H | T], CurrCoef):-
    H is CurrCoef,
    NextCoef is CurrCoef * 10,
    NextNbrOfDigits is NbrOfDigits - 1,
    getCoeffsAux(NextNbrOfDigits, T, NextCoef).


% SET RESTRICTIONS

setRestrictions(FirstNumberList, AmountOfNumbers, Multiplier, NbrOfDigits, Coeffs):-
    length(NumberLists, AmountOfNumbers),
    generateNumbers(FirstNumberList, Multiplier, NbrOfDigits, Coeffs, NumberLists, 1),
    labeling([], NumberLists),
    write(NumberLists).




generateNumbers(LastNumberList, _, _, Coeffs, NumberLists, Counter):-
    length(NumberLists, Counter),
    scalar_product(Coeffs, LastNumberList, #=, LastNum),
    element(Counter, NumberLists, LastNum).

generateNumbers(PreviousNumberList, Multiplier, NbrOfDigits, Coeffs, NumberLists, Counter):-
    scalar_product(Coeffs, PreviousNumberList, #=, PrevNum),
    element(Counter, NumberLists, PrevNum),
    generateNumber(PreviousNumberList, Multiplier, NbrOfDigits, Coeffs, CurrentNumberList),
    NextCounter #= Counter + 1,
    generateNumbers(CurrentNumberList, Multiplier, NbrOfDigits, Coeffs, NumberLists, NextCounter).

generateNumber(PrevNumberList, Multiplier, NbrOfDigits, Coeffs, NextNumberList):-
    scalar_product(Coeffs, PrevNumberList, #=, PrevNumber),
    generateRestrictedNumberRemoveDigit(PrevNumberList, NbrOfDigits, Coeffs, NextNumberDigitRemovedList),
    scalar_product(Coeffs, NextNumberDigitRemovedList, #=, NextNumberDigitRemoved),
    NextNumber #\= PrevNumber,
    (NextNumber #= NextNumberDigitRemoved #\/ NextNumber #= PrevNumber * Multiplier),
    generateNumberListFromNumber(NextNumber, NbrOfDigits, Coeffs, NextNumberList).
    



generateRestrictedNumberRemoveDigit(PrevNumList, NbrOfDigits, Coeffs, NextNumList):-
    length(NextNumList, NbrOfDigits),
    domain(NextNumList, 0, 9),
    IndexToRemove in 1..NbrOfDigits,
    restrictNumberListRemoveDigit(PrevNumList, IndexToRemove, NextNumList, 2).

restrictNumberListRemoveDigit(_, _, NextNumberList, CurrIndex):-
    ExceededIndex #= CurrIndex - 1,
    length(NextNumberList, ExceededIndex),
    element(1, NextNumberList, 0).

restrictNumberListRemoveDigit(PrevNumList, IndexToRemove, NextNumList, CurrIndex):-
    %In case CurrIndex > IndexToRemove
    NextIndex #= CurrIndex + 1,
    CurrIndex #> IndexToRemove,
    element(CurrIndex, PrevNumList, CurrValue),
    element(CurrIndex, NextNumList, CurrValue),
    restrictNumberListRemoveDigit(PrevNumList,IndexToRemove,NextNumList,NextIndex).

restrictNumberListRemoveDigit(PrevNumList, IndexToRemove, NextNumList, CurrIndex):-
    %In case CurrIndex <= IndexToRemove
    NextIndex #= CurrIndex + 1,
    CurrIndex #=< IndexToRemove,
    element(NextIndex, PrevNumList, CurrValue),
    element(CurrIndex, NextNumList, CurrValue),
    restrictNumberListRemoveDigit(PrevNumList,IndexToRemove,NextNumList,NextIndex).

    




generateNumberListFromNumber(NextNumber, NbrOfDigits, Coeffs, NextNumberList):-
    length(NextNumberList, NbrOfDigits),
    domain(NextNumberList, 0, 9),
    generateNumberListFromNumberAux(NextNumber, Coeffs, NextNumberList, 1).

generateNumberListFromNumberAux(Remainder, _, NumberList, CurrIndex):-
    length(NumberList, CurrIndex),
    element(CurrIndex, NumberList, Remainder).

generateNumberListFromNumberAux(Number, Coeffs, NumberList, CurrIndex):-
    element(CurrIndex, Coeffs, CurrCoef),
    Division #= Number div CurrCoef,
    Remainder #= Number mod CurrCoef,
    element(CurrIndex, NumberList, Division),
    NextIndex #= CurrIndex + 1,
    generateNumberListFromNumberAux(Remainder, Coeffs, NumberList, NextIndex).