:- set_flag(print_depth, 1000).
:- lib(ic).
:- lib(ic_global).

%tents(RowTents, ColumnTents, Trees, Tents) :-
tents(RowTents, ColumnTents, Trees, Positions) :-
    
    writeln(RowTents),
    length(RowTents, N),
    writeln(N),
    
    writeln(ColumnTents),
    length(ColumnTents, M),
    writeln(M),
    
    writeln(Trees),
    length(Trees, K),
    writeln(K),
    
    TotalPositions is N * M,
    length(Positions, TotalPositions),
    Positions #::[0,1],
    writeln(Positions),
    
    constrains(Positions, RowTents, ColumnTents, Trees),
    
    write('\n'),

    printListPerCols(Positions, 0, M).


constrains(Positions, RowTents, ColumnTents, Trees) :-
    length(RowTents, N),
    length(ColumnTents, M),
    excludeTreePositions(Positions, N, M, Trees).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Δεν μπορεί να υπάρχει τέντα σε θέση που υπάρχει δέντρο
%positions with tree get 0
excludeTreePositions(_, _, _, []).
excludeTreePositions(Positions, N, M, [I-J|RestTrees]) :-
    getElement(Positions, N, M, I, J, El),
    El #= 0,
    excludeTreePositions(Positions, N, M, RestTrees).

%return element corresponding to I-J
getElement(Positions, N, M, I, J, El) :-
    ElsToBypass is (I-1) * M,
    retListBypass(Positions, 0, ElsToBypass, PositionsBP),
    PositionsBP \= [],
    element(J, PositionsBP, El).

%return list elements without ElsToBypass first elements
retListBypass([El|Rest], Counter, ElsToBypass, PositionsBP) :-
    CounterNew is Counter + 1,
    CounterNew =< ElsToBypass,
    retListBypass(Rest, CounterNew, ElsToBypass, PositionsBP),
    !.
retListBypass(PositionsBP, _, _, PositionsBP).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

printList([]).
printList([El|Rest]) :-
    writeln(El),
    printList(Rest).

printListPerCols([], _, _).
printListPerCols([El|Rest], Counter, Columns) :-
    CounterNew is Counter + 1,
    CounterNew < Columns,
    write('\t\t'), write(El),
    printListPerCols(Rest, CounterNew, Columns), !.
printListPerCols([El|Rest], Counter, Columns) :-
    write('\t\t'), writeln(El),
    printListPerCols(Rest, 0, Columns).