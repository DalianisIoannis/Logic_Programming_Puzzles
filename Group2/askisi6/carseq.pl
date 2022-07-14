:- set_flag(print_depth, 1000).
:- lib(ic).
:- lib(ic_global).
:- [carseq_data4].

carseq(S) :-
    classes(Cl),
    options(Op),
    length(Cl, NumOfDifConfigs),
    sumListElements(Cl, TotalCars),
    length(S, TotalCars),
    S #:: 1..NumOfDifConfigs,
    constrainOptions(S, Cl, Op),
    constrainCarsPerConf(S, Cl, NumOfDifConfigs),
    search(S, 0, input_order, indomain, complete, []).

%how many times a car of config 1..NumOfDifConfigs should occur in S
constrainCarsPerConf(_, [], _).
constrainCarsPerConf(S, [CarsOfConfig|Rest], NumOfDifConfigs) :-
    length(Rest, LengthOfRest),
    CurrentConfig is NumOfDifConfigs - LengthOfRest,
    occurrences(CurrentConfig, S, CarsOfConfig),
    constrainCarsPerConf(S, Rest, NumOfDifConfigs).

constrainOptions(_, _, []).
constrainOptions( S, Cl, [M/K/ConfsWithOption|Rest] ) :-
    carNumFromAces(ConfsWithOption, 1, ConfsWithOptionToNum),
    carTotFromConfs(Cl, ConfsWithOptionToNum, AddCars),
    sequence_total(AddCars, AddCars, 0, K, M, S, ConfsWithOptionToNum),
    constrainOptions(S, Cl, Rest).

%total cars of specific configs that have to be produced
carTotFromConfs(_, [], 0).
carTotFromConfs(Cl, [CarConf|Rest], Ret) :-
    element(CarConf, Cl, X),
    carTotFromConfs(Cl, Rest, RetNext),
    Ret is RetNext + X.

%return indexes where value is 1, means configs that have a specific option
carNumFromAces([], _, []).
carNumFromAces( [Option|OptionListRest], Counter, RetList ) :-
    CounterNew is Counter + 1,
    carNumFromAces(OptionListRest, CounterNew, RetListNext),
    (Option = 1 ->
        append([Counter], RetListNext, RetList);
        append([], RetListNext, RetList) ).

sumListElements([El], El).
sumListElements([El|Rest], Sum) :-
    sumListElements(Rest, SumNext),
    Sum is SumNext + El.