:- module(kojun_solver, [run_kojun/2]).
:- use_module(library(clpfd)).

% Predicado principal
run_kojun(ProblemWithZeros, Regions) :-
    same_length(ProblemWithZeros, Regions),
    maplist(same_length, ProblemWithZeros, Regions),
    compute_region_sizes(Regions, RegionSizes),
    writeln('Tamanhos das regiões: '), writeln(RegionSizes),
    transform_grid(ProblemWithZeros, Regions, RegionSizes, Grid),
    writeln('Grid após transformação: '), maplist(writeln, Grid),
    constrain_regions(Grid, Regions), writeln('constrain_regions OK'),
    constrain_vertical_neighbors(Regions, Grid), writeln('constrain_vertical_neighbors OK'),
    constrain_adjacent_regions(Grid), writeln('constrain_adjacent_regions OK'),
    append(Grid, Vars),
    labeling([], Vars), writeln('labeling OK'),
    print_grid(Grid).

% Pré-processa os tamanhos das regiões
compute_region_sizes(Regions, RegionSizes) :-
    flatten(Regions, Flat),
    sort(Flat, Unique),
    findall(Id-N,
        (member(Id, Unique), count_occurrences(Id, Flat, N)),
        RegionSizes).

count_occurrences(Id, List, Count) :-
    include(=(Id), List, Filtered),
    length(Filtered, Count).

% Transforma o grid substituindo zeros por variáveis com domínios
transform_grid([], [], _, []).
transform_grid([Row|RestP], [RRow|RestR], RegionSizes, [GRow|RestG]) :-
    transform_row(Row, RRow, RegionSizes, GRow),
    transform_grid(RestP, RestR, RegionSizes, RestG).

transform_row([], [], _, []).
transform_row([Cell|Cells], [RegionId|RegionIds], RegionSizes, [Var|Vars]) :-
    transform_cell(Cell, RegionId, RegionSizes, Var),
    transform_row(Cells, RegionIds, RegionSizes, Vars).

transform_cell(0, RegionId, RegionSizes, Var) :-
    member(RegionId-N, RegionSizes),
    Var in 1..N.
transform_cell(X, _, _, X) :- X \= 0.

% Restrição: valores distintos por região
constrain_regions(Grid, Regions) :-
    findall(Id, (member(Row, Regions), member(Id, Row)), Ids),
    sort(Ids, UniqueIds),
    forall(member(Id, UniqueIds), (
        region_cells(Grid, Regions, Id, Cells),
        all_different(Cells)
    )).

% Restrição: ordem vertical e diferença sempre
constrain_vertical_neighbors(Regions, Grid) :-
    length(Grid, Rows),
    Rows1 is Rows - 1,
    forall(between(0, Rows1, I), (
        nth0(I, Grid, Row1),
        nth0(I, Regions, Reg1),
        I2 is I + 1,
        nth0(I2, Grid, Row2),
        nth0(I2, Regions, Reg2),
        length(Row1, NCols),
        Cols1 is NCols - 1,
        forall(between(0, Cols1, J), (
            nth0(J, Row1, V1),
            nth0(J, Row2, V2),
            nth0(J, Reg1, R1),
            nth0(J, Reg2, R2),
            V1 #\= V2,                     % <-- sempre diferentes
            (R1 = R2 -> V1 #> V2 ; true)   % <-- se mesma região, aplicar ordem
        ))
    )).

% FUNÇÃO PROBLEMÁTICA!!!!!
% Restrição: células adjacentes (cima, baixo, esquerda, direita) diferentes
constrain_adjacent_regions(Grid) :-
    length(Grid, Rows),
    nth0(0, Grid, FirstRow),
    length(FirstRow, Cols),
    forall(between(0, Rows-1, I), (
        forall(between(0, Cols-1, J), (
            cell(Grid, I, J, V),
            I1 is I+1, I2 is I-1, J1 is J+1, J2 is J-1,
            (I1 < Rows -> cell(Grid, I1, J, D), V #\= D ; true),
            (I2 >= 0    -> cell(Grid, I2, J, U), V #\= U ; true),
            (J1 < Cols -> cell(Grid, I, J1, R), V #\= R ; true),
            (J2 >= 0    -> cell(Grid, I, J2, L), V #\= L ; true)
        ))
    )).

% Utilitários
cell(Matrix, I, J, Val) :- nth0(I, Matrix, Row), nth0(J, Row, Val).
region_id(Regions, I, J, Id) :- cell(Regions, I, J, Id).

region_cells(Grid, Regions, Id, Cells) :-
    findall(Val,
        (nth0(I, Grid, GRow),
         nth0(I, Regions, RRow),
         nth0(J, GRow, Val),
         nth0(J, RRow, Id)),
        Cells).

print_grid([]).
print_grid([Row|Rest]) :-
    print_row(Row), nl,
    print_grid(Rest).

print_row([]).
print_row([X|Xs]) :-
    ( var(X) -> write('_') ; write(X) ),
    write(' '),
    print_row(Xs).
