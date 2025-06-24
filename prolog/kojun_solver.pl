:- module(kojun_solver, [run_kojun/2]).
:- use_module(library(clpfd)).

run_kojun(Problem, Regions) :-
    writeln('== Iniciando solução do Kojun =='),
    construir_tabuleiro(Problem, Regions, Tab),
    aplicar_regras(Tab, Regions),
    coletar_variaveis(Tab, Vars),
    labeling([], Vars),
    exibir_tabuleiro(Tab).

% --- CONSTRUÇÃO DO TABULEIRO ---
construir_tabuleiro([], [], []).
construir_tabuleiro([PL|PT], [RL|RT], [Linha|Rest]) :-
    construir_linha(PL, RL, Linha),
    construir_tabuleiro(PT, RT, Rest).

construir_linha([], [], []).
construir_linha([P|TP], [R|TR], [[R,V]|TL]) :-
    ( integer(P), P =\= 0 -> V = P ; V in 1..100 ),
    construir_linha(TP, TR, TL).

% --- EXTRAÇÃO DE VARIÁVEIS SEM LISTAS ANINHADAS ---
coletar_variaveis(Tab, Vars) :-
    findall(V, (
        member(Linha, Tab),
        member([_,V], Linha)
    ), Vars).

% --- APLICA TODAS AS REGRAS ---
aplicar_regras(Tab, Regions) :-
    definir_dominios(Tab, Regions),
    restricao_vizinhos(Tab),
    transpose(Tab, Colunas),
    restricao_vizinhos(Colunas),
    restricao_decrescente(Colunas),
    garantir_unicidade_por_regiao(Tab).

% --- DOMÍNIOS COM BASE NAS REGIÕES ---
definir_dominios([], _).
definir_dominios([Linha|Resto], Regions) :-
    definir_dominios_linha(Linha, Regions),
    definir_dominios(Resto, Regions).

definir_dominios_linha([], _).
definir_dominios_linha([[Reg,V]|T], Regions) :-
    tamanho_regiao(Reg, Regions, Limite),
    V in 1..Limite,
    definir_dominios_linha(T, Regions).

tamanho_regiao(Id, Regs, Tam) :-
    flatten(Regs, Flat),
    include(=(Id), Flat, Lista),
    length(Lista, Tam).

% --- VIZINHOS DIRETOS DIFERENTES ---
restricao_vizinhos([]).
restricao_vizinhos([Linha|Outros]) :-
    vizinhos_distintos(Linha),
    restricao_vizinhos(Outros).

vizinhos_distintos([]).
vizinhos_distintos([_]).
vizinhos_distintos([[_,A],[_,B]|T]) :-
    A #\= B,
    vizinhos_distintos([[_,B]|T]).

% --- REGRA DECRESCENTE VERTICAL ---
restricao_decrescente([]).
restricao_decrescente([Coluna|Resto]) :-
    decrescentes_coluna(Coluna),
    restricao_decrescente(Resto).

decrescentes_coluna([]).
decrescentes_coluna([_]).
decrescentes_coluna([[R1,V1],[R2,V2]|Tail]) :-
    (R1 =:= R2 -> V1 #> V2 ; true),
    decrescentes_coluna([[R2,V2]|Tail]).

% --- UNICIDADE POR REGIÃO SEM LISTAS ANINHADAS ---
garantir_unicidade_por_regiao(Tab) :-
    total_regioes(Tab, Max),
    aplicar_em_regioes(1, Max, Tab).

aplicar_em_regioes(Id, Max, _) :-
    Id > Max, !.
aplicar_em_regioes(Id, Max, Grid) :-
    extrair_regiao(Id, Grid, Valores),
    all_distinct(Valores),
    Prox is Id + 1,
    aplicar_em_regioes(Prox, Max, Grid).

extrair_regiao(_, [], []).
extrair_regiao(Id, [Linha|Resto], Saida) :-
    filtrar_regiao(Id, Linha, Parciais),
    extrair_regiao(Id, Resto, Outras),
    append(Parciais, Outras, Saida).

filtrar_regiao(_, [], []).
filtrar_regiao(R, [[R,V]|T], [V|S]) :- filtrar_regiao(R, T, S).
filtrar_regiao(R, [[Out,_]|T], S) :- R =\= Out, filtrar_regiao(R, T, S).

% --- DESCOBRE O NÚMERO DE REGIÕES A PARTIR DO TABULEIRO ---
total_regioes(Tab, Max) :-
    findall(R, (member(L, Tab), member([R,_], L)), Todos),
    sort(Todos, Unicos),
    max_list(Unicos, Max).

% --- EXIBIÇÃO DO RESULTADO ---
exibir_tabuleiro([]).
exibir_tabuleiro([Linha|Outros]) :-
    maplist(arg(2), Linha, Valores),
    writeln(Valores),
    exibir_tabuleiro(Outros).
