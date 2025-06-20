:- module(teste, [teste/0]).
:- use_module(kojun_solver).

teste :-
    writeln('== CHAMANDO run_kojun =='),
    Problem = [
        [0, 0, 4, 0, 2, 0],
        [0, 0, 3, 0, 0, 0],
        [1, 4, 0, 4, 0, 0],
        [0, 5, 0, 0, 0, 2],
        [0, 0, 0, 0, 3, 0],
        [6, 2, 0, 2, 0, 5]
    ],
    Regions = [
        [1, 4, 4, 4, 7,11],
        [1, 5, 4, 7, 7, 7],
        [1, 1, 6, 7,10,10],
        [2, 3, 6, 8, 8,10],
        [2, 3, 3, 9, 9,10],
        [3, 3, 3, 9, 9, 9]
    ],
    run_kojun(Problem, Regions).
