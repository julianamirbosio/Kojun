:- module(teste, [teste/0]).
:- use_module(kojun_solver).

% Problema 6x6 com valores iniciais e regiões

problem([
  [3, 2, 4, 1, 2, 1],
  [2, 1, 3, 5, 1, 3],
  [1, 4, 2, 4, 3, 4],
  [2, 5, 1, 2, 1, 2],
  [1, 4, 3, 4, 3, 1],
  [6, 2, 1, 2, 1, 5]
]).

regions([
  [1, 4, 4, 4, 7,11],
  [1, 5, 4, 7, 7, 7],
  [1, 1, 6, 7,10,10],
  [2, 3, 6, 8, 8,10],
  [2, 3, 3, 9, 9,10],
  [3, 3, 3, 9, 9, 9]
]).

teste :-
    writeln('== CHAMANDO run_kojun =='),
    problem(P),
    regions(R),
    run_kojun(P, R).
