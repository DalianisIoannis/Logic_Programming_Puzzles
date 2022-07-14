:- set_flag(print_depth, 1000).
:- lib(ic).
:- lib(ic_global).
:- lib(branch_and_bound).

tents(RowTents, ColumnTents, Trees, Tents) :-
    
    length(RowTents, N),
    length(ColumnTents, M),
    
    initPositions(0, N, M, Positions),
    Positions#::[0,1],
    constrains(Positions, RowTents, ColumnTents, Trees),

    returnListWithAllPos(Positions, ListPos),
    Cost #= sum(ListPos),

    bb_min(
        search(Positions, 0, input_order, indomain, complete, []),
        Cost, bb_options{strategy:continue, solutions:all}
    ),

    %printTreesAndTents(Positions, 0, Trees, RowTents, ColumnTents),
    returnCorList(Positions, 0, Tents).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

returnCorList([], _, []).
returnCorList([Row|Rest], I, Tents) :-
    Inew is I + 1,
    returnCorList(Rest, Inew, TentsNext),
    retPlacesFromRow(Row, Inew, 0, RowPairs),
    append(RowPairs, TentsNext, Tents).

retPlacesFromRow([], _, _, []).
retPlacesFromRow([El|Rest], I, J, Pairs) :-
    Jnew is J + 1,
    retPlacesFromRow(Rest, I, Jnew, NextPairs),
    ( El = 1 -> append([I-Jnew], NextPairs, Pairs);
                append([], NextPairs, Pairs) ).

returnListWithAllPos([], []).
returnListWithAllPos([Row|Rest], RetList) :-
    returnListWithAllPos(Rest, ListNext),
    append(Row, ListNext, RetList).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

constrains(Positions, RowTents, ColumnTents, Trees) :-
    length(RowTents, N),
    length(ColumnTents, M),
    excludeTreePositions(Positions, Trees),
    tentAroundTrees(Positions, N, M, Trees),
    noTentsNextToEachOther(Positions, 0, N, M, Positions),
    rowAndColLims(Positions, RowTents, ColumnTents).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%Για κάποιες, όχι απαραίτητα όλες, από τις γραμμές και τις στήλες του αγρού δίνονται μέγιστοι αριθμοί τεντών
%που μπορεί υπάρχουν στη γραμμή ή τη στήλη, αντίστοιχα.
rowAndColLims(Positions, RowTents, ColumnTents) :-
    rowMaxTents(Positions, RowTents),
    length(ColumnTents, M),
    colMaxTents(Positions, 0, M, ColumnTents).

colMaxTents(_, J, M, _) :-
    Jnew is J + 1,
    Jnew > M.
colMaxTents(Positions, J, M, ColumnTents) :-
    Jnew is J + 1,
    Jnew =< M,
    getJColumnToList(Positions, Jnew, ColList),
    element(Jnew, ColumnTents, MaxCol),
    maxTreesInList(ColList, MaxCol),
    colMaxTents(Positions, Jnew, M, ColumnTents).

getJColumnToList([], _, []).
getJColumnToList([Row|Rest], J, ColList) :-
    getJColumnToList(Rest, J, ColListNext),
    element(J, Row, El),
    append([El], ColListNext, ColList).

rowMaxTents([], []).
rowMaxTents([Row|Rest], [RowMax|RestRows]) :-
    maxTreesInList(Row, RowMax),
    rowMaxTents(Rest, RestRows).

maxTreesInList(List, Max) :-
    Max = 0,
    sum(List) #= 0.
maxTreesInList(_, Max) :-
    Max < 0.
maxTreesInList(List, Max) :-
    Max > 0,
    sum(List) #=< Max.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%Δύο τέντες δεν πρέπει βρίσκονται σε γειτονικές θέσεις, ούτε οριζόντια, ούτε κάθετα, ούτε διαγώνια.
noTentsNextToEachOther([], _, _, _, _).
noTentsNextToEachOther([RowList|Rest], I, N, M, Positions) :-
    Inew is I + 1,
    iterateRowForAdjacents(RowList, Inew, 0, N, M, Positions),
    noTentsNextToEachOther(Rest, Inew, N, M, Positions).

iterateRowForAdjacents([], _, _, _, _, _).
iterateRowForAdjacents([El|Rest], I, J, N, M, Positions) :-
    Jnew is J + 1, (Jnew > M -> Jcurrent is 1 ; Jcurrent is Jnew),
    Jnext is Jcurrent + 1,
    Jbefore is Jcurrent - 1,
    Ibelow  is I + 1,
    Ibelow =< N,
    returnIthRow(Positions, 0, I, RowI),
    returnIthRow(Positions, 0, Ibelow, RowIbelow),
    checkBelowLeft(El, RowIbelow, Jbefore),
    checkRight(El, RowI, Jnext, M),
    checkBelowRight(El, RowIbelow, Jnext, M),
    checkBelow(El, RowIbelow, Jcurrent),
    iterateRowForAdjacents(Rest, I, Jcurrent, N, M, Positions).

iterateRowForAdjacents([El|Rest], I, J, N, M, Positions) :-
    Jnew is J + 1, (Jnew > M -> Jcurrent is 1 ; Jcurrent is Jnew),
    Ibelow  is I + 1,
    Ibelow > N,
    Jnext is Jcurrent + 1,
    returnIthRow(Positions, 0, I, RowI),
    checkRight(El, RowI, Jnext, M),
    iterateRowForAdjacents(Rest, I, Jcurrent, N, M, Positions).

checkBelowLeft(_, _, Jbefore) :-
    Jbefore =< 0.
checkBelowLeft(El, RowIbelow, Jbefore) :-
    Jbefore > 0,
    element(Jbefore, RowIbelow, El2),
    El + El2 #=< 1.

checkBelow(El, RowIbelow, J) :-
    element(J, RowIbelow, El2),
    El + El2 #=< 1.

checkBelowRight(_, _, Jnext, M) :-
    Jnext > M.
checkBelowRight(El, RowIbelow, Jnext, M) :-
    Jnext =< M,
    element(Jnext, RowIbelow, El2),
    El + El2 #=< 1.

checkRight(_, _, Jnext, M) :-
    Jnext > M.
checkRight(El, RowI, Jnext, M) :-
    Jnext =< M,
    element(Jnext, RowI, El2),
    El + El2 #=< 1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%Σε τουλάχιστον μία από τις γειτονικές θέσεις κάθε δέντρου, οριζόντια, κάθετα ή διαγώνια, να υπάρχει τέντα.
tentAroundTrees(_, _, _, []).
tentAroundTrees(Positions, N, M, [I-J|RestTrees]) :-
    
    length(ListOfElsAround, 0),
    returnIthRow(Positions, 0, I, RowI),
    Iabove is I - 1,
    Ibelow is I + 1,
    Jbefore is J - 1,
    Jnext is J + 1,

    (   %apo panw grammh yparxei
        Iabove > 0 ->   (
                            returnIthRow(Positions, 0, Iabove, RowIabove),

                            %panw aristera
                            (Jbefore > 0 -> (   element(Jbefore, RowIabove, ElImin1Jmin1),
                                                append([ElImin1Jmin1], ListOfElsAround, ListOfElsAround1)
                                            ) ; append([], ListOfElsAround, ListOfElsAround1)
                            ),
                            
                            %akrivws apo panw
                            element(J, RowIabove, ElImin1J),
                            append([ElImin1J], ListOfElsAround1, ListOfElsAround2),
                            
                            %panw dexia
                            (Jnext =< M -> (   element(Jnext, RowIabove, ElImin1Jplus1),
                                                append([ElImin1Jplus1], ListOfElsAround2, ListOfElsAround3)
                                            ) ; append([], ListOfElsAround2, ListOfElsAround3)
                            )
                        %apo panw grammh den yparxei
                        ) ; append([], ListOfElsAround, ListOfElsAround3)
    ),
    (   %idia grammh to prin
        Jbefore > 0 ->  (   element(Jbefore, RowI, ElIJmin1),
                            append([ElIJmin1], ListOfElsAround3, ListOfElsAround4)
                        ) ; append([], ListOfElsAround3, ListOfElsAround4)
    ),
    (   %idia grammh to meta
        Jnext =< M -> ( element(Jnext, RowI, ElIJplus1),
                        append([ElIJplus1], ListOfElsAround4, ListOfElsAround5)
                    ) ; append([], ListOfElsAround4, ListOfElsAround5)
    ),
    (   %apo katw grammh yparxei
        Ibelow =< N ->   (
                            returnIthRow(Positions, 0, Ibelow, RowIbelow),
                            %katw aristera
                            (Jbefore > 0 -> (   element(Jbefore, RowIbelow, ElIplus1Jmin1),
                                                append([ElIplus1Jmin1], ListOfElsAround5, ListOfElsAround6)
                                            ) ; append([], ListOfElsAround5, ListOfElsAround6)
                            ),
                            %akrivws apo katw
                            element(J, RowIbelow, ElIplus1J),
                            append([ElIplus1J], ListOfElsAround6, ListOfElsAround7),
                            %katw dexia
                            (Jnext =< M -> (   element(Jnext, RowIbelow, ElIplus1Jplus1),
                                                append([ElIplus1Jplus1], ListOfElsAround7, ListOfElsAround8)
                                            ) ; append([], ListOfElsAround7, ListOfElsAround8)
                            )
                        %apo katw grammh den yparxei
                        ) ; append([], ListOfElsAround5, ListOfElsAround8)
    ),
    sum(ListOfElsAround8) #>= 1,
    tentAroundTrees(Positions, N, M, RestTrees).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%Δεν μπορεί υπάρχει τέντα σε θέση που υπάρχει δέντρο
excludeTreePositions(_, []).
excludeTreePositions(Positions, [I-J|RestTrees]) :-
    returnIthRow(Positions, 0, I, Row),
    element(J, Row, El),
    El #= 0,
    excludeTreePositions(Positions, RestTrees).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

returnIthRow([_|Rest], _, RowSearch, RowRet) :-
    RowSearchNew is RowSearch - 1,
    RowSearchNew > 0,
    returnIthRow(Rest, _, RowSearchNew, RowRet).

returnIthRow([Row|_], _, RowSearch, Row) :-
    RowSearchNew is RowSearch - 1,
    RowSearchNew = 0.

%create list of rows as lists
initPositions(RowCounter, N, _, []) :-
    RowCounterNew is RowCounter + 1,
    RowCounterNew > N.
initPositions(RowCounter, N, M, [RowList|Rest]) :-
    RowCounterNew is RowCounter + 1,
    RowCounterNew =< N,
    initPositions(RowCounterNew, N, M, Rest),
    length(RowList, M).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

printField([]).
printField([Row|Rest]) :-
    printFieldRow(Row),
    write('\n'),
    printField(Rest).
printFieldRow([]).
printFieldRow([El|Rest]) :-
    write('\t\t'), write(El),
    printFieldRow(Rest).

printTreesAndTents([], _, _, _, _) :-
    write('############################################'),
    write('############################################\n\n').
printTreesAndTents([Row|Rest], I, Trees, RowTents, ColumnTents) :-
    Inew is I + 1,
    printRowTreesAndTents(Row, Inew, 0, Trees),
    write('\n\n'),
    printTreesAndTents(Rest, Inew, Trees, RowTents, ColumnTents).
printRowTreesAndTents([], _, _, _).
printRowTreesAndTents([El|Rest], I, J, Trees) :-
    Jnew is J + 1, write('\t\t'),
    ( El = 1 -> ( write('A') );
                ( member(I-Jnew, Trees) ->  ( write('Y') ) ; ( write('-') ) )
    ), printRowTreesAndTents(Rest, I, Jnew, Trees).