:-use_module(library(clpfd)).

main(FirstNumber, AmountOfNumbers, Multiplier, ResultList):-
    getMaxNumber(FirstNumber, AmountOfNumbers, Multiplier, Max),
    length(ResultList, AmountOfNumbers),
    domain(ResultList, 1, Max),
    getDigitsInNumber(Max, NbrOfDigits, 0),
    getCoeffs(NbrOfDigits, Coeffs, [], 1),
    
    element(1, ResultList, FirstNumber),
    % trace,
    generateNumbers(ResultList, Multiplier, Coeffs, NbrOfDigits, 1, Powers, []),
    append(Powers, ResultList, Vars),

    write('.'),
    labeling([], Vars).

getMaxNumber(Max,1,_,Max).
getMaxNumber(FirstNumber, AmountOfNumbers, Multiplier, Max):-
    NextNumber is FirstNumber * Multiplier,
    NextAmount is AmountOfNumbers - 1,
    getMaxNumber(NextNumber, NextAmount, Multiplier, Max).

generateNumbers(List, Multiplier, Coeffs, NbrOfDigits, Counter, Powers, PowersTemp):-
    write('-----Counter----'),
    write(Counter), nl,
    length(List, Counter),
    write('generateNumbers - base case'), nl,
    element(Counter, List, LastElement),
    element(1, List, FirstElement),
    generateRestrictedNumberRemoveDigit(LastElement, Coeffs, NbrOfDigits, LastElementWithRemovedDigit, GeneratedPowers),
    (FirstElement #= LastElement * Multiplier) #\/ (LastElement #>= 10 #/\ FirstElement #= LastElementWithRemovedDigit),
    append(PowersTemp, GeneratedPowers, Powers).

generateNumbers(List, Multiplier, Coeffs, NbrOfDigits, Counter, Powers, PowersTemp):-
    element(Counter, List, CurrElement),
    write('generateNumbers - normal'), nl,
    NextCounter is Counter + 1,
    element(NextCounter, List, NextElement),
    generateRestrictedNumberRemoveDigit(CurrElement, Coeffs, NbrOfDigits, CurrElementWithRemovedDigit, GeneratedPowers),
    (NextElement #= CurrElement * Multiplier) #\/ (CurrElement #>= 10 #/\ NextElement #= CurrElementWithRemovedDigit),
    append(PowersTemp, GeneratedPowers, NewPowers),
    generateNumbers(List, Multiplier, Coeffs, NbrOfDigits, NextCounter, Powers, NewPowers).

generateRestrictedNumberRemoveDigit(Number, Coeffs, NbrOfDigits, NextNumber, PowersList):-
    % Number #> NextNumber,
    NextNumber #> 0,
    Iminus1 #= I - 1,
    element(I, Coeffs, PowerI),
    element(Iminus1, Coeffs, PowerIminus1),
    write('uno'),nl,
    LeftSide #= Number // PowerI,
    write('dos'),nl,
    RightSide #= Number mod PowerIminus1,
    write('tres'),nl,
    NextNumber #= (LeftSide * PowerIminus1 + RightSide),
    write('cuatro'),nl,
    PowersList = [PowerI, PowerIminus1].

getDigitsInNumber(0, Nbr, Nbr).
getDigitsInNumber(Nbr, NbrOfDigits, Temp):-
    NextNbr #= Nbr div 10,
    NextTemp #= Temp + 1,
    getDigitsInNumber(NextNbr, NbrOfDigits, NextTemp).

getCoeffs(0, [0|Temp], Temp, _).
getCoeffs(NbrOfDigits, CoeffsList, Temp, CurrCoef):-
    append(Temp, [CurrCoef], NewTemp),
    NextCoef is CurrCoef * 10,
    NextNbrOfDigits is NbrOfDigits - 1,
    getCoeffs(NextNbrOfDigits, CoeffsList, NewTemp, NextCoef).