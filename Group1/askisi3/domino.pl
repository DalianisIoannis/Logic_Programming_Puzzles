
dominos([
	(0,0),(0,1),(0,2),(0,3),(0,4),(0,5),(0,6),
	(1,1),(1,2),(1,3),(1,4),(1,5),(1,6),
	(2,2),(2,3),(2,4),(2,5),(2,6),
	(3,3),(3,4),(3,5),(3,6),
	(4,4),(4,5),(4,6),
	(5,5),(5,6),
	(6,6)
]).
frame([
	[3,1,2,6,6,1,2,2],
	[3,4,1,5,3,0,3,6],
	[5,6,6,1,2,4,5,0],
	[5,6,4,1,3,3,0,0],
	[6,1,0,6,3,2,4,0],
	[4,1,5,2,4,3,5,5],
	[4,1,0,2,4,5,2,0]
]).

/*
dominos([
		(0,0),(0,1),(0,2),(0,3),(0,4),(0,5),(0,6),(0,7),(0,8),(0,9),(0,a),
		(1,1),(1,2),(1,3),(1,4),(1,5),(1,6),(1,7),(1,8),(1,9),(1,a),
		(2,2),(2,3),(2,4),(2,5),(2,6),(2,7),(2,8),(2,9),(2,a),
		(3,3),(3,4),(3,5),(3,6),(3,7),(3,8),(3,9),(3,a),
		(4,4),(4,5),(4,6),(4,7),(4,8),(4,9),(4,a),
		(5,5),(5,6),(5,7),(5,8),(5,9),(5,a),
		(6,6),(6,7),(6,8),(6,9),(6,a),
		(7,7),(7,8),(7,9),(7,a),
		(8,8),(8,9),(8,a),
		(9,9),(9,a),
		(a,a)
]).
frame([
	[6,5,0,5,5,3,3,1,1,4,6],
	[1,2,2,a,a,5,7,0,1,0,7],
	[5,8,6,0,8,0,9,7,7,4,2],
	[4,0,9,0,7,7,9,9,8,8,0],
	[1,a,3,8,8,5,a,8,0,0,3],
	[9,2,3,5,7,6,9,1,6,3,9],
	[2,2,2,5,8,6,0,4,6,a,a],
	[9,4,2,1,7,9,5,4,a,4,a],
	[9,a,4,9,5,5,6,6,0,a,2],
	[1,a,1,2,1,1,8,2,2,7,8],
	[7,7,3,3,4,3,6,6,4,3,1],
	[5,9,6,3,3,a,7,4,4,8,8]
]).
*/
/*
dominos([(a,b),(b,c),(c,d),(d,e),(e,f),(f,g),(g,h),(h,i),(i,j),(j,k),(k,l),(l,m),
         (a,c),(b,d),(c,e),(d,f),(e,g),(f,h),(g,i),(h,j),(i,k),(j,l),(k,m),(l,n),
         (a,d),(b,e),(c,f),(d,g),(e,h),(f,i),(g,j),(h,k),(i,l),(j,m),(k,n),(l,o),
         (a,e),(b,f),(c,g),(d,h),(e,i),(f,j),(g,k),(h,l),(i,m),(j,n),(k,o),(l,p),
         (a,f),(b,g),(c,h),(d,i),(e,j),(f,k),(g,l),(h,m),(i,n),(j,o),(k,p),(l,q),
         (a,g),(b,h),(c,i),(d,j),(e,k),(f,l),(g,m),(h,n),(i,o),(j,p),(k,q),(l,r),
         (a,h),(b,i),(c,j),(d,k),(e,l),(f,m),(g,n),(h,o),(i,p),(j,q),(k,r),(l,s),
         (a,i),(b,j),(c,k),(d,l),(e,m),(f,n),(g,o),(h,p),(i,q),(j,r),(k,s),(l,t),
         (a,j),(b,k),(c,l),(d,m),(e,n),(f,o),(g,p),(h,q),(i,r),(j,s),(k,t),(l,u),
         (a,k),(b,l),(c,m),(d,n),(e,o),(f,p),(g,q),(h,r),(i,s),(j,t),(k,u),(l,v),
         (a,l),(b,m),(c,n),(d,o),(e,p),(f,q),(g,r),(h,s),(i,t),(j,u),(k,v),(l,w),
         (a,m),(b,n),(c,o),(d,p),(e,q),(f,r),(g,s),(h,t),(i,u),(j,v),(k,w),(l,x)]).

frame([
	   [d,g,i,r,d,f,g,l,n,f,i,s,f,k,w,l],
       [k,e,a,j,k,e,s,k,j,k,b,i,r,c,j,o],
       [l,q,j,p,n,h,k,l,s,j,r,t,f,v,k,k],
       [x,k,a,d,f,m,m,o,c,g,d,h,j,i,c,u],
       [g,q,i,b,m,a,f,e,i,b,l,a,e,i,f,g],
       [n,a,o,i,p,g,r,l,r,h,a,o,g,l,p,i],
       [d,c,d,g,e,f,n,h,b,t,j,e,d,h,c,i],
       [i,m,g,b,q,i,b,f,c,l,l,b,u,i,h,t],
       [j,h,c,g,f,a,s,l,f,l,e,c,d,j,i,j],
       [p,s,n,d,a,p,c,l,b,e,k,j,u,t,h,g],
       [c,f,g,g,b,h,n,e,j,h,m,i,j,f,h,c],
       [f,l,w,h,e,o,h,j,k,j,v,d,b,b,n,k],
       [h,r,g,n,m,d,a,d,h,l,k,b,h,m,a,i],
       [o,j,l,e,k,g,d,m,e,h,k,r,j,j,l,k],
       [e,c,o,h,a,n,f,k,d,q,k,k,a,j,f,p],
       [v,l,i,q,p,p,k,o,m,e,d,l,g,m,k,s],
       [e,n,i,g,e,q,l,m,i,o,g,m,i,c,l,k],
       [q,n,j,q,l,h,f,o,b,j,p,c,l,l,u,t]
	]).
*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*for counting results
findall(Sol, put_dominos(Sol), Sols), length(Sols, N).*/
%put_dominos(ReturnDominos) :-
put_dominos :-
	dominos(X),
	listOfdomsPlaces(X, Dominos),
	sortDominos(Dominos, DominosSorted),
	reCMakeListDominoPlacePut(DominosSorted, ReturnDominos),
	printRes(ReturnDominos).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

reCMakeListDominoPlacePut([], []) :- !.
reCMakeListDominoPlacePut( Dominos, ReturnDominos ) :-
	/*recursively returns Dom*/
	dominoWithMinPlaces(Dominos, Dom),
	/*not to return the same dominos all over again*/
	!,
	returnPlacesDominoFits(Dom, Place),
	/*delete domino from dominos*/
	(   member(Dom, Dominos) ->
		delete(Dom, Dominos, RestDominos);
		append([], Dominos, RestDominos) ),
	/*remove place from all other dominos*/
	returnDominosWIthoutPlace(Place, RestDominos, DominosNew),
	/*make pair of domino with which place fits*/
	onlyDominoID(Dom, DomID),
	append( DomID, [Place], DomPlace ),
	sortDominos(DominosNew, DominosNewSorted),
	reCMakeListDominoPlacePut(DominosNewSorted, RetDominosRec),
	append([DomPlace], RetDominosRec, ReturnDominos).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

onlyDominoID( [_, Dom, [_|_]], [Dom] ).

/*return all other dominos without particular place*/
returnDominosWIthoutPlace(_, [], []).
returnDominosWIthoutPlace([Coord1, Coord2], [ [_, [Dom1,Dom2],ListOfPlaces ] | RestOfList ], ListReturned) :-
	removeAllPlaces(Coord1, Coord2, ListOfPlaces, ListOfPlacesNew, NumberPlacesNew),
	/*den prepei na gyrisei kapoio me mhden*/
	NumberPlacesNew > 0,
	returnDominosWIthoutPlace([Coord1, Coord2], RestOfList, ListRetRec),
	append([NumberPlacesNew, [Dom1,Dom2]], [ListOfPlacesNew], CurrentDom),
	append([CurrentDom], ListRetRec, ListReturned).

/*delete all occurences coordinates from list and change sum*/
removeAllPlaces(_, _, [], [], 0).
removeAllPlaces(Coord1, Coord2, [[Coord3, Coord4]|RestPlaces], RetPlaces, LastNum) :-
	removeAllPlaces(Coord1, Coord2, RestPlaces, RetPlaces2, LastNum2),
	(
		(Coord1 = Coord3;
		Coord1 = Coord4;
		Coord2 = Coord3;
		Coord2 = Coord4) ->
		append([], RetPlaces2, RetPlaces),
		LastNum is LastNum2;
		append([[Coord3, Coord4]], RetPlaces2, RetPlaces),
		LastNum is LastNum2 + 1
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*recursively return all places of a domino*/
returnPlacesDominoFits([], []).
returnPlacesDominoFits( [_, [_,_],ListOfPlaces], Place) :-
	returnPlaceFromList(ListOfPlaces, Place).

returnPlaceFromList( [[[X1,Y1],[X2,Y2]]|_], [[X1,Y1],[X2,Y2]] ).
returnPlaceFromList( [_|RestPlaces], NextPlace ) :-
	returnPlaceFromList(RestPlaces, NextPlace).

/*returns with ascending order dominos with places*/
dominoWithMinPlaces([], []) :- !.
dominoWithMinPlaces([[NumPlaces, [Dom1,Dom2],ListOfPlaces] | _ ], [NumPlaces, [Dom1,Dom2],ListOfPlaces]) :- !.
dominoWithMinPlaces([_|RestOfList], NextDomino) :-
	dominoWithMinPlaces(RestOfList, NextDomino).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*print result*/
printRes(Dominos) :-
	frame([List1|ListRest]),
	Frame = [List1|ListRest],
	length(Frame, Grammes),
	length(List1, Sthles),
	makeCounterList(0, Grammes, GrammesList),
	makeCounterList(0, Sthles, SthlesList),
	printLineWithElements(Dominos, GrammesList, SthlesList).

printLineWithElements(_, [], _).
printLineWithElements(Dominos, [Elem|RestLines], SthlesList) :-
    printElementsPerCol(Dominos, Elem, SthlesList, BetweenLine),
    nl,
    writeBetweenLine(BetweenLine),
    nl,
    printLineWithElements(Dominos, RestLines, SthlesList).

writeBetweenLine([]).
writeBetweenLine([El|Rest]) :-
    write(El),
    writeBetweenLine(Rest).

printElementsPerCol(_, _, [], []).
printElementsPerCol(Doms, Line, [FirstElem|Rest], BetweenLine) :-
	printLine(Doms, Line, FirstElem, NextLine),
	printElementsPerCol(Doms, Line, Rest, ReturnList2),
    append(NextLine, ReturnList2, BetweenLine).

/*searches for the element of particular place*/
printLine([[[Dom1,_], [[Coord1, Coord2],[Coord3,Coord4]]] | _ ], Line, Column, Ret) :-
	Line = Coord1,
	Column = Coord2,
	(   Line = Coord3 ->(
							(CompareCol is Column+1, Coord4 = CompareCol) -> (
                                                                                write(Dom1), write('-'),
                                                                                append([], [' ', ' '], Ret)
                                                                            );
							(CompareCol is Column-1, Coord4 = CompareCol) -> (
                                                                                write(Dom1), write(' '),
                                                                                append([], [' ', ' '], Ret)
                                                                            )
						);
                        (
                            write(Dom1), write(' '),
                            CompareLine is Line + 1,
                            (
                                Coord3 = CompareLine -> ( Ret = ['|', ' '] );
                                                        ( Ret = [' ', ' '] )
                            )
                        )
	),
    !.
printLine([[[_,Dom2], [[Coord1, Coord2],[Coord3,Coord4]]] | _ ], Line, Column, Ret) :-
	Line = Coord3,
	Column = Coord4,
	(   Line = Coord1 ->(
							(CompareCol is Column+1, Coord2 = CompareCol) -> (
                                                                                write(Dom2), write('-'),
                                                                                append([], [' ', ' '], Ret)
                                                                            );
							(CompareCol is Column-1, Coord2 = CompareCol) -> (
                                                                                write(Dom2), write(' '),
                                                                                append([], [' ', ' '], Ret)
                                                                            )
						);
                        (
                            write(Dom2), write(' '),
                            CompareLine is Line + 1,
                            (
                                Coord1 = CompareLine -> ( Ret = ['|', ' '] );
                                                        ( Ret = [' ', ' '] )
                            )
                        )
	),
	!.
printLine([[[_,_], [[_, _],[_,_]]] | RestDominos ], Line, Column, Ret) :-
	printLine(RestDominos, Line, Column, Ret),
	!.
printLine([], _, _, []).

/* takes a number N and makes a list of numbers [1...N]*/
makeCounterList(Count, Limit, []) :-
	Count >= Limit, !.
makeCounterList(Count, Limit, List) :-
	Count < Limit,
	CountNew is Count + 1,
	makeCounterList(CountNew, Limit, ListNew),
	append([CountNew], ListNew, List).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*sort base on num of possible places ascending
https://kti.mff.cuni.cz/~bartak/prolog/sorting.html#quick */
sortDominos(Dominos, DominosSorted) :-
	b_sort_Dominos(Dominos, [], DominosSorted).
b_sort_Dominos([], Acc, Acc).
b_sort_Dominos( [[NumPlaces, [Dom1,Dom2],ListOfPlaces] | RestOfList ], Acc,  DominosSorted) :-
	bubble_Dominos( [NumPlaces, [Dom1,Dom2],ListOfPlaces], RestOfList, NT, Max ),
	b_sort_Dominos( NT, [Max|Acc], DominosSorted ).
bubble_Dominos(X, [], [], X).
bubble_Dominos( 
		[NumPlaces, [Dom1,Dom2],ListOfPlaces],
		[[NumPlaces2,[Dom3,Dom4],ListOfPlaces1]|T],
		[[NumPlaces2, [Dom3,Dom4],ListOfPlaces1]|NT],
		Max ) :-
	NumPlaces > NumPlaces2,
	bubble_Dominos( [NumPlaces, [Dom1,Dom2],ListOfPlaces], T, NT, Max ).
bubble_Dominos(
		[NumPlaces, [Dom1,Dom2],ListOfPlaces],
		[[NumPlaces2, [Dom3,Dom4],ListOfPlaces1]|T],
		[[NumPlaces, [Dom1,Dom2],ListOfPlaces]|NT],
		Max ) :-
	NumPlaces =< NumPlaces2,
	bubble_Dominos( [NumPlaces2, [Dom3,Dom4],ListOfPlaces1], T, NT, Max).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*returns list of pairs [NumPlaces, Domino, PlacesOfDomino]*/
listOfdomsPlaces([], []).
listOfdomsPlaces( [Domino|Dominos], Retdoms ) :-
	findAllPlaces(Domino, Retdom),
	listOfdomsPlaces(Dominos, Retdoms2),
	append(Retdoms2, [Retdom], Retdoms).

/*find for every domino list of all possible places*/
findAllPlaces( (DomFirst,DomSec), [N, [DomFirst,DomSec],List] ) :-
	findall([X,Y], tryPlaceMatch([DomFirst,DomSec], X, Y), List2),
	extractSymmetrics(List2, List),
	length(List, N).

extractSymmetrics([], []).
extractSymmetrics( [[X,Y]|RestList], RetList) :-
	extractSymmetrics(RestList, RetList2),
	( not member( [Y, X], RetList2 ) ->
		append( [[X,Y]], RetList2, RetList );
		append( [], RetList2, RetList )
	).

/*try for each domino all potential places it can go on frame
returns for a domino a correct double coordinate*/
tryPlaceMatch([DomFirst,DomSec], RetCordDomFirst, RetCordDomSec) :-
	frame(Fr),
	findCoordDomFirst(Fr, DomFirst, 0, RetCordDomFirst),
	findCoordDomSec(Fr, DomSec, 0, RetCordDomFirst, RetCordDomSec).

/*finds coordinates for DomFirst*/
findCoordDomFirst([Line|_], DomFirst, CounterLines, [RetLine, RetCordX]) :-
	CounterLinesNew is CounterLines + 1,
	testEqDomFirst(Line, DomFirst, 0, RetCordX),
	RetLine is CounterLinesNew.
findCoordDomFirst([_|RestLines], DomFirst, CounterLines, [RetLine, RetCordX]) :-
	CounterLinesNew is CounterLines + 1,
	findCoordDomFirst(RestLines, DomFirst, CounterLinesNew, [RetLine, RetCordX]).

/* finds X coordinate in Line*/
testEqDomFirst([El|_], DomFirst, Counter, RetCordX) :-
	NewCounter is Counter + 1,
	El = DomFirst,
	RetCordX is NewCounter.
testEqDomFirst([_|RestLine], DomFirst, Counter, RetCordX) :-
	NewCounter is Counter + 1,
	testEqDomFirst(RestLine, DomFirst, NewCounter, RetCordX).

/* search the whole same line or last and next line*/
findCoordDomSec([Line|_], DomSec, CounterLines, [YDomFirst,XDomFirst], [YDomSec,XDomSec]) :-
	CounterLinesNew is CounterLines + 1,
	CounterLinesNew=YDomFirst,
	testEqDomSecSameLine(Line, DomSec, 0, XDomFirst, XDomSec),
	YDomSec is CounterLinesNew.
findCoordDomSec([Line|_], DomSec, CounterLines, [YDomFirst,XDomFirst], [YDomSec,XDomSec]) :-
	CounterLinesNew is CounterLines + 1,
	CompareLineBefore is YDomFirst-1,
	CompareLineAfter is YDomFirst+1,
	( CounterLinesNew=CompareLineBefore; CounterLinesNew=CompareLineAfter ),
	testEqDomSecOtherLine(Line, DomSec, 0, XDomFirst, XDomSec),
	YDomSec is CounterLinesNew.
findCoordDomSec([_|RestLines], DomSec, CounterLines, [YDomFirst,XDomFirst], [YDomSec,XDomSec]) :-
	CounterLinesNew is CounterLines + 1,
	findCoordDomSec(RestLines, DomSec, CounterLinesNew, [YDomFirst,XDomFirst], [YDomSec,XDomSec]).

testEqDomSecSameLine([El|_], DomSec, Counter, XDomFirst, XDomSec) :-
	NewCounter is Counter + 1,
	El = DomSec,
	CompareElBefore is XDomFirst - 1,
	CompareElAfter is XDomFirst + 1,
	(NewCounter = CompareElBefore; NewCounter = CompareElAfter),
	XDomSec is NewCounter.
testEqDomSecSameLine([_|RestLine], DomSec, Counter, XDomFirst, XDomSec) :-
	NewCounter is Counter + 1,
	testEqDomSecSameLine(RestLine, DomSec, NewCounter, XDomFirst, XDomSec).
testEqDomSecOtherLine([El|_], DomSec, Counter, XDomFirst, XDomSec) :-
	NewCounter is Counter + 1,
	El = DomSec,
	NewCounter = XDomFirst,
	!,
	XDomSec is NewCounter.
testEqDomSecOtherLine([_|RestLine], DomSec, Counter, XDomFirst, XDomSec) :-
	NewCounter is Counter + 1,
	testEqDomSecOtherLine(RestLine, DomSec, NewCounter, XDomFirst, XDomSec).