:- set_flag(print_depth, 1000).
:- lib(ic).
:- lib(branch_and_bound).
:- [create_graph].

maxclq(N, D, Clique, Size) :-
    create_graph(N, D, G),
    createComplementaryGraph(N, G, CompGraph),
    !,
    length(C, N),
    C #:: [0,-1],
    constrain(CompGraph, C),
    Cost #= sum(C),
    bb_min(
        search(C, 0, input_order, indomain, complete, []),
        Cost, bb_options{strategy:continue}
    ),
    returnCor(C, 1, Clique),
    length(Clique, Size).

constrain([], _).
constrain([E1-E2|Rest], C) :-
    getElement(E1, C, ZorOne1),
    getElement(E2, C, ZorOne2),
    ZorOne1 + ZorOne2 #>= -1,
    constrain(Rest, C).

returnCor([], _, []).
returnCor([El|Rest], Counter, ListRet) :-
    CounterNew is Counter + 1,
    returnCor(Rest, CounterNew, ListRetRest),
    (El = -1 ->  append([Counter], ListRetRest, ListRet);
                append([], ListRetRest, ListRet)).

getElement(1, [CurrVl|_], CurrVl) :- !.
getElement(ElPlace, [_|Rest], Val) :-
    ElPlaceNew is ElPlace - 1,
    getElement(ElPlaceNew, Rest, Val), !.

createComplementaryGraph(N, InitialGraph, FinalGraph) :-
    retAllIJs(1, N, InitialGraph, FinalGraph).

retAllIJs(End, End, _, []).
retAllIJs(Start, End, InitialGraph, RetList) :-
    Start < End,
    StartPlus is Start + 1,
    retAllIJs(StartPlus, End, InitialGraph, RetListNext),
    makePairsIJ(Start, StartPlus, End, InitialGraph, RetListTmp),
    append(RetListTmp, RetListNext, RetList).

makePairsIJ(Start, StartPlus, End, InitialGraph, [Start-StartPlus|Rest]) :-
    StartPlus =< End,
    not member(Start-StartPlus, InitialGraph),
    StartPlusNew is StartPlus + 1,
    makePairsIJ(Start, StartPlusNew, End, InitialGraph, Rest).

makePairsIJ(Start, StartPlus, End, InitialGraph, Rest) :-
    StartPlus =< End,
    member(Start-StartPlus, InitialGraph),
    StartPlusNew is StartPlus + 1,
    makePairsIJ(Start, StartPlusNew, End, InitialGraph, Rest).

makePairsIJ(_, StartPlus, End, _, []) :-
    StartPlus > End.