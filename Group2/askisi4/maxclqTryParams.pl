:- set_flag(print_depth, 1000).
:- lib(ic).
:- lib(branch_and_bound).
:- [create_graph].

maxclqPARAMS(N, D, Method, ChooseVar, Strat,Clique, Size) :-
    create_graph(N, D, G),
    createComplementaryGraph(N, G, CompGraph),
    length(C, N),
    C #:: [0,-1],
    constrain(CompGraph, C),
    Cost #= sum(C),
    bb_min(
        search(C, 0, Method, ChooseVar, complete, []),
        Cost, bb_options{strategy:Strat}
    ),
    returnCor(C, 1, Clique),
    length(Clique, Size).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

go_all(Method, ChooseVar, Strat) :-
   cputime(T1),
   %seed(1010101), maxclqPARAMS(165, 30, Method, ChooseVar, Strat, Clique, Size),
   %seed(654321), maxclqPARAMS(220, 20, Method, ChooseVar, Strat, Clique, Size),
   seed(1111), maxclqPARAMS(300, 15, Method, ChooseVar, Strat, Clique, Size),
   %seed(1821), maxclqPARAMS(500, 10, Method, ChooseVar, Strat, Clique, Size),
   %seed(1), maxclqPARAMS(800, 2, Method, ChooseVar, Strat, Clique, Size),
   cputime(T2), T is T2-T1,
   write('Clique is '), write(Clique), write(' with size '), write(Size), writeln('.'),
   write('Time: '), write(T), writeln(' secs.').

go_all :-
   %member(Method, [input_order, max_regret, first_fail, most_constrained, smallest, largest, occurrence]),
   member(Method, [input_order]),
   %member(ChooseVar, [indomain, indomain_median, indomain_min, indomain_middle, indomain_split]),
   member(ChooseVar, [indomain]),
   %member(Strat, [restart, continue]),
   member(Strat, [continue]),
   nl, write('--------------------------------'), nl,
   write('Method: '), write(Method),
   write('  ChooseVar: '), write(ChooseVar),
   write('  Strat: '), write(Strat), nl,
   write('--------------------------------'), nl,
   go_all(Method, ChooseVar, Strat),
   fail.