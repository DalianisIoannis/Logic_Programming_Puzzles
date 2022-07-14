:- [maxclq].


run_all :-
   member(Querry, 
        [
            [1, 1, 100],
            [1, 2, 100],
            [1, 3, 100],
            [1, 4, 60],
            [1, 5, 50],
            [1, 5, 70],
            [1, 5, 90],
            [1, 5, 100],
            [1, 8, 80],
            [2022, 15, 60],
            [100, 32, 50],
            [1234, 80, 50],
            [8231, 120, 40],
            [1010101, 165, 30],
            [6543, 220, 20],
            [1111, 300, 15],
            [1821, 500, 10],
            [1, 800, 2]
        ]
    ),
    runREC([Querry]),
    fail.

runREC( [[SEED, N, D]] ) :-
    cputime(T1),
    seed(SEED), maxclq(N, D, Clique, Size),
    cputime(T2), T is T2-T1,
    nl, write('--------------------------------'),
    nl, write('--------------------------------'), nl,
    write('Querry: '), nl,
    write('\tseed('), write(SEED), write('), maxclq('), write(N), write(', '), write(D), write(', Clique, Size).'),
    nl,
    write('Clique: '), write(Clique), nl,
    write('Size: '), write(Size), nl,
    write('Time: '), write(T), writeln(' secs.'),
    write('--------------------------------'), nl,
    write('--------------------------------'), nl.
