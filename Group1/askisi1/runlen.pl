
make_cont(_, 0, []) :-
	!.
make_cont(Stoixeio, Menoun, [Stoixeio|Lista1]) :-
	!,
	Menoun>0,
	Menoun1 is Menoun - 1,
	make_cont(Stoixeio, Menoun1, Lista1).	

decode_r1([], []) :-
	!.
decode_r1( [(Stoixeio,Plithos)|Tail], Result) :-
	!,	
	make_cont(Stoixeio, Plithos, FirstPart),
	decode_r1( Tail, SecondPart ),
	append(FirstPart, SecondPart, Result).
decode_r1( [X|Tail], L ) :-
	decode_r1( Tail, L1 ),
	append( [X], L1, L ).

/*
decode_r1([(a,3),(b,2),c,(d,4),e], L).
decode_r1([(f(5,a),7)], L).
decode_r1([g(X),(h(Y),3),k(Z),(m(W),4),n(U)], L).
*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

deleteFirstN(OldList, N, NewList) :-
    length(First, N),
    append(First, NewList, OldList).

checkRest(_, [], 1) :- !.
checkRest(Teleutaio, [X|_], N) :-
	X \= Teleutaio,
	checkRest(X, [], N).
checkRest(Teleutaio, [X|Lista], N) :-
	X = Teleutaio,
	checkRest(X, Lista, N1),
	N is N1 + 1.

encode_r1([], []) :-
!.
encode_r1([X|Tail], Result) :- 
%!,
	checkRest(X, Tail, N),
	N1 is N -1,
	deleteFirstN(Tail, N1, NewTail),
	encode_r1(NewTail, SecondPart),
	N>1,
	append([(X, N)], SecondPart, Result).
	
encode_r1([X|Tail], Result) :- 
!,
	checkRest(X, Tail, N),
	N1 is N -1,
	deleteFirstN(Tail, N1, NewTail),
	encode_r1(NewTail, SecondPart),
	N = 1,
	append([X], SecondPart, Result).

/*
encode_r1([a,a,a,b,b,c,d,d,d,d,e], L).
encode_r1([f(5,a),f(5,a),f(5,a),f(5,a),f(5,a),f(5,a),f(5,a)], L).
encode_r1([g(X),h(Y),h(Y),h(Y),k(Z),m(W),m(W),m(W),m(W),n(U)], L).
encode_r1([p(3),p(X),q(X),q(Y),q(4)], L).
*/

/*
Απάντηση Ερώτησης
Δίνοντας ?-encode_r1([p(3),p(X),q(X),q(Y),q(4)], L), όταν καλείται η checkRest(p(3), [p(X),q(X),q(...),...], _997), 
αρχικά η p(X) \= p(3) αποτυγχάνει καθώς και στα δύο μέρη είναι η ίδια δομή p. Συνεπώς γίνεται η ενοποίηση
p(X) = p(3) η οποία είναι επιτυχής, οπότε η μεταβλητή X θα πάρει την τιμή 3. Για αυτό μερικά βήματα μετά θα κληθεί
η checkRest(p(3), [q(3),q(Y), q(...)], _2414). Το q(3) στην αρχή της λίστας πριν ήταν q(X) αλλά πλέον το X είναι 3.
Αντίστοιχα αργότερα επιτυγχάνει η ενοποίηση q(Y) = q(3) και το Y γίνεται 3. Συνεπώς μέχρι τώρα έχουν μετρηθεί 2 φορές
το p(3) και 2 φορές το q(3). Όσον αφορά το τελευταίο στοιχείο της λίστας (q(4)), η q(4) \= q(3) θα είναι
αληθής, καθώς πλέον η δομή q έχει και στα δύο μέρη αριθμό και όχι μεταβλητή. Άρα, θα μετρηθεί και μία φορά το στοιχείο
q(4) ως διαφορετικό από τα προηγούμενα. Έτσι το αποτέλεσμά μας είναι L = [(p(3), 2), (q(3), 2), q(4)].
*/