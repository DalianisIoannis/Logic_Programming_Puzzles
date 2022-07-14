:- set_flag(print_depth, 1000).
:- lib(ic).
:- [genrand].

liars(Declair, Liars) :-
    length(Declair, N),
    length(Liars, N),
    Liars #:: [0,1],
    SumOfLiars #= sum(Liars),
    constrain(Declair, Liars, SumOfLiars),
    search(Liars, 0, input_order, indomain, complete, []).

constrain( [], [], _ ).
constrain( [PersonDeclair|RestDec] , [PersonID|RestIDs] , SumOfLiars ) :-
    PersonID #= (PersonDeclair #> SumOfLiars),
    constrain(RestDec, RestIDs, SumOfLiars).