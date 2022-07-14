

makeCounterList(Count, Limit, []) :-
	Count >= Limit, !.
makeCounterList(Count, Limit, List) :-
	Count < Limit,
	CountNew is Count + 1,
	makeCounterList(CountNew, Limit, ListNew),
	append([CountNew], ListNew, List).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

codegen(X, X, []).
codegen(ListStart, ListGoal, Moves) :-
	
	depth_first(ListStart, ListGoal, [ListStart], Moves).

depth_first(List, List, _, []) :- !.
depth_first(ListCur, ListGoal, TriedStates, [Move|RestMoves]) :-
	makeChange(ListCur, ListNext, Move),
	not member(ListNext, TriedStates),
	append(TriedStates, [ListNext], TriedStatesNew),
	depth_first(ListNext, ListGoal, TriedStatesNew, RestMoves),
	!.


makeChange(ListCur, ListRet, Move) :-
	
	%loop me mia metrhsh
	length(ListCur, LengthList),
	makeCounterList(0, LengthList, Nums),
	member(CountNum, Nums),

	move(ListCur, ListRet, CountNum),
	append([], move(CountNum), Move).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

move([X,_|List1], List, 1) :-
	!,
	append([X,X], List1, List).

move([X,_], List, 1) :-
	!,
	append([], [X,X], List).

move([X|List1], List, Place) :-
	length([X|List1], LengthList),
	Place = LengthList,
	getLastItem(Last, List1),
	!,
	append([Last], List1, List).

move([_], List, _) :-
	append([], [], List).

move([X,Z|List1], List, Place) :-
	Place \= 1,
	Place > 0,
	Place2 is Place - 1,
	move([Z|List1], NewList, Place2),
	append([X], NewList, List).

getLastItem(X, [X]).
getLastItem(X, [_|Tail]) :-
	getLastItem(X, Tail).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

swap(ListCur, FirstPlace, SecondPlace, ListRest) :-
	getNthItem(First, ListCur, FirstPlace),
	getNthItem(Last, ListCur, SecondPlace),
	
	CounterBef is FirstPlace - 1,
	appendUntil(ListCur, CounterBef, ListRest1),
	deleteFirstN(ListCur, FirstPlace, DeletedFirst),
	append(ListRest1, [Last], ListRest3),
	
	CounterDeleteFirstN is SecondPlace - FirstPlace,
	CounterBef2 is CounterDeleteFirstN - 1,
	appendUntil(DeletedFirst, CounterBef2, ListRest2),
	deleteFirstN(DeletedFirst, CounterDeleteFirstN, DeletedFirst2),
	
	append(ListRest3, ListRest2, ListRest4),
	append(ListRest4, [First], ListRest5),
	append(ListRest5, DeletedFirst2, ListRest).


% appends untill counter with counter
appendUntil(_, 0, []) :- !.
appendUntil([X|Tail], Counter, ListRet) :-
	Counter > 0,
	Counter2 is Counter - 1,
	appendUntil(Tail, Counter2, ListRec),
	!,
	append([X], ListRec, ListRet).

getNthItem(X, [X|_], 1) :- !.
getNthItem(X, [_|Tail], Counter) :-
	Counter > 1,
	Counter2 is Counter - 1,
	getNthItem(X, Tail, Counter2).
	
deleteFirstN(List, 0, List) :- !.
deleteFirstN([_|List], Counter, L1) :-
	Counter > 0,
	Counter2 is Counter - 1,
	deleteFirstN(List, Counter2, L2),
	append([], L2, L1).