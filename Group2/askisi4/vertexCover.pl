:- set_flag(print_depth, 1000).
:- lib(ic).
:- lib(branch_and_bound).
:- [create_graph].

vertexcover(N, D, ListRet) :-
    create_graph(N, D, G),
    writeln(G),
    length(C, N),
    C #:: [0,1],
    constrain(G, C),
    Cost #= sum(C),
    bb_min(
        search(C, 0, first_fail, indomain, complete, []),
        Cost,
        bb_options{strategy:restart}
    ),
    returnCor(C, 1, ListRet).

returnCor([], _, []).
returnCor([El|Rest], Counter, ListRet) :-
    CounterNew is Counter + 1,
    returnCor(Rest, CounterNew, ListRetRest),
    (El = 1 ->  append([Counter], ListRetRest, ListRet);
                append([], ListRetRest, ListRet)
    ).

constrain([], _) :- !.
constrain([E1-E2|Rest], C) :-
    getElement(E1, C, ZorOne1),
    getElement(E2, C, ZorOne2),
    ZorOne1 + ZorOne2 #> 0,
    constrain(Rest, C).

getElement(1, [CurrVl|_], CurrVl) :- !.
getElement(ElPlace, [_|Rest], Val) :-
    ElPlaceNew is ElPlace - 1,
    getElement(ElPlaceNew, Rest, Val), !.