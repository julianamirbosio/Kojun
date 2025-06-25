:- module(kojun_solver, [
    solve_kojun/3,
    next_coord/5,
    replace_nth0/4,
    update_grid/5,
    region_size/3,
    valid/5,
    same_adjacent/4,
    in_same_region/5,
    valid_vertical/5,
    get_cell/4,
    neighbor/4
]).

% Biblioteca de domínios finitos
:- use_module(library(clpfd)).

% Coord: coordenada (Linha, Coluna), ambas inteiros.
% Representação: coord(Linha, Coluna)
%
% Grid: lista de linhas, cada linha é lista de inteiros (0 = vazio).
% Representação: Grid = [ [C11,C12,…], [C21,C22,…], … ]
%
% RegionMap: mesma forma que Grid, mas em cada posição armazena o ID da região.
% Representação: RegionMap = [ [R11,R12,…], [R21,R22,…], … ]


% solve_kojun(+Grid, +Regions, -Solution)
%   Grid:      lista de listas de inteiros (0 = célula vazia)
%   Regions:   mesma forma que Grid, mas cada célula carrega o ID da região
%   Solution:  instância de Grid totalmente preenchida, ou o predicado falha
solve_kojun(Grid, Regions, Solution) :-
    % começa o backtracking na coordenada (0,0)
    backtrack(Grid, Regions, 0, 0, Solution).


% backtrack(+Grid, +Regions, +Row, +Col, -Solution) 
%   percorre célula a célula, preenche vazias e unifica Solution no fim
% Primeira Cláusula: caso base
backtrack(Grid, _, Row, _, Grid) :-
    % caso base: ultrapassou a última linha → Grid já está completo
    % testa se Row é igual a length(Grid)
    length(Grid, Row).

% Segunda Cláusula: caso recursivo
backtrack(Grid, Regions, Row, Col, Solution) :-
    % se a célula já estiver preenchida (!=0), apenas avança
    % nth0(Index, List, Element)
    nth0(Row, Grid, RowList),                       % extrai a linha Row de Grid
    nth0(Col, RowList, Val),                        % extrai o valor Val na coluna Col
    Val \= 0,                                       % garante que não é célula vazia
    next_coord(Grid, Row, Col, NextRow, NextCol),   % calcula próxima (Row,Col)
    backtrack(Grid, Regions, NextRow, NextCol, Solution).

% Terceira Cláusula: caso recursivo
backtrack(Grid, Regions, Row, Col, Solution) :-
    % célula vazia: gera candidatos, testa e recua
    nth0(Row, Regions, RegRow),                     % pega a linha Row de Regions
    nth0(Col, RegRow, RegionID),                    % pega o ID da região 
    region_size(Regions, RegionID, Max),            % conta tamanho da região
    between(1, Max, Cand),                          % gera cada Cand em 1..Max
    valid(Grid, Regions, Row, Col, Cand),           % checa as 3 regras do kojun
    update_grid(Grid, Row, Col, Cand, NewGrid),     % coloca Cand em (Row,Col), retorna NewGrid
    next_coord(Grid, Row, Col, NextRow, NextCol),
    backtrack(NewGrid, Regions, NextRow, NextCol, Solution).

% next_coord(+Grid, +Row, +Col, -NextRow, -NextCol)
next_coord(Grid, Row, Col, NextRow, NextCol) :-
    % Esta cláusula trata o caso “há ainda colunas à direita” na mesma linha.
    nth0(0, Grid, FirstRow),    % pega a primeira linha de Grid
    length(FirstRow, W),        % calcula o número de colunas W
    Col < W-1,                  % verifica se Col < W-1 (ainda há colunas à direita)
    NextRow = Row,              % mantém a mesma linha
    NextCol is Col + 1.         % avança para a próxima coluna
next_coord(Grid, Row, Col, NextRow, 0) :-
    % Esta cláusula trata o caso “chegou ao final da linha, avança para a próxima linha”.
    nth0(0, Grid, FirstRow),
    length(FirstRow, W),        
    Col >= W-1,                 % verifica se Col >= W-1 (já está na última coluna)
    NextRow is Row + 1.         % avança para a próxima linha
    % NextCol = 0.

% update_grid(+Grid, +Row, +Col, +Val, -NewGrid)
update_grid(Grid, Row, Col, Val, NewGrid) :-
    nth0(Row, Grid, OldRow),                    % pega a linha Row de Grid
    replace_nth0(OldRow, Col, Val, NewRow),     % cria NewRow igual a OldRow, mas com Val na posição Col
    replace_nth0(Grid, Row, NewRow, NewGrid).   % substitui a linha Row de Grid por NewRow, resultando em NewGrid.

% replace_nth0(+List, +Index, +Elem, -NewList)
replace_nth0(List, I, Elem, NewList) :-
    same_length(List, NewList),               % garante que NewList terá o mesmo comprimento que List
    append(Prefix, [_|Suffix], List),         % divide List em Prefix + [Elem] + Suffix
    length(Prefix, I),                        % Prefix terá I elementos   
    append(Prefix, [Elem|Suffix], NewList).   % concatena Prefix + [Elem] + Suffix → NewList, substituindo o elemento antigo por Elem

% region_size(+Regions, +RegID, -Count)
region_size(Regions, RegID, Count) :-
    flatten(Regions, Flat),         % transforma a lista de listas Regions numa lista única Flat
    include(=(RegID), Flat, L),     % filtra Flat para obter apenas os elementos iguais a RegID
    length(L, Count).               % conta quantos elementos RegID existem em L, unificando Count com o resultado

% valid(+Grid, +Regions, +R, +C, +Val)
% Verifica não repetir em adjacentes, dentro da região e regra vertical.
valid(Grid, Regions, R, C, Val) :-
    \+ same_adjacent(Grid, R, C, Val),
    \+ in_same_region(Grid, Regions, R, C, Val),
    valid_vertical(Grid, Regions, R, C, Val).

% same_adjacent(+Grid, +R, +C, +Val)
% verdadeiro se algum vizinho ortogonal tem mesmo valor.
same_adjacent(Grid, R, C, Val) :-
    neighbor(R1,C1,R,C),            % gera não-deterministicamente cada par de coordenadas (R1,C1) que esteja acima, abaixo, à esquerda ou à direita de (R,C)
    get_cell(Grid, R1, C1, Val).    % verifica se, no Grid, a célula (R1,C1) de fato existe e tem valor igual a Val. Se algum vizinho satisfaz isso, a cláusula fecha com sucesso

% neighbor(-R1, -C1, +R, +C)
% verdadeiro se (R1,C1) é vizinho ortogonal de (R,C).
neighbor(R1,C1, R,C) :- R1 is R-1, C1 = C.  % vizinho acima
neighbor(R1,C1, R,C) :- R1 is R+1, C1 = C.  % vizinho abaixo
neighbor(R1,C1, R,C) :- R1 = R, C1 is C-1.  % vizinho à esquerda
neighbor(R1,C1, R,C) :- R1 = R, C1 is C+1.  % vizinho à direita

% in_same_region(+Grid, +Regions, +R, +C, +Val)
% verdadeiro se Val já ocorre na região de (R,C).
in_same_region(Grid, Regions, R, C, Val) :-
    % obtém o ID da região desta célula
    nth0(R, Regions, RegRow), nth0(C, RegRow, RegID),
    % encontra todas as células na região RegID
    findall(V, (
        nth0(I, Regions, RR), nth0(J, RR, RegID),
        get_cell(Grid, I, J, V), V \= 0
    ), Vs), member(Val, Vs).        % % verifica se Val está entre os valores Vs encontrados na região

% valid_vertical(+Grid, +Regions, +R, +C, +Val)
% verifica regra vertical entre regiões.
valid_vertical(Grid, Regions, R, C, Val) :-
    nth0(R, Regions, RRow), nth0(C, RRow, RegID),
    length(Regions, N), Last is N-1,
    % Regra para célula ACIMA (↑)
    ( R > 0 ->
        R1 is R-1,
        nth0(R1, Regions, RR1), nth0(C, RR1, AboveReg),
        get_cell(Grid, R1, C, V1),
        % mesma região → valor acima deve ser MAIOR
        ( (AboveReg =:= RegID, V1 \= 0) -> V1 > Val ; true)
    ; true ),
    % Regra para célula ABAIXO (↓)
    ( R < Last ->
        R2 is R+1,
        nth0(R2, Regions, RR2), nth0(C, RR2, BelowReg),
        get_cell(Grid, R2, C, V2),
        % mesma região → valor abaixo deve ser MENOR
        ( (BelowReg =:= RegID, V2 \= 0) -> V2 < Val ; true)
    ; true ).

% get_cell(+Grid, +R, +C, -Val)
% unifica Val se dentro dos limites, caso contrário falha.
get_cell(Grid, R, C, Val) :-
    nth0(R, Grid, Row), nth0(C, Row, Val).
