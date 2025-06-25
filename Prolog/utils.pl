% utils.pl
:- module(utils, [read_puzzle_from_file/3, print_grid/1, parse_line/2]).

% Usamos read_file_to_string/3 para ler arquivo e split_string/4 para quebrar linhas
:- use_module(library(readutil)).

%% read_puzzle_from_file(+FilePath, -Grid, -Regions)
% Lê um puzzle no formato:
%   n
%   <grid: n linhas de números separados por espaço>
%   <regions: n linhas de números separados por espaço>
% Retorna Grid e Regions como listas de listas de inteiros.
read_puzzle_from_file(File, Grid, Regions) :-
    read_file_to_string(File, Str, []),
    split_string(Str, "\n", "\s\t\n\r", Lines0),
    exclude(==(""), Lines0, Lines),            % remove linhas vazias
    Lines = [DimLine|Rest],
    split_string(DimLine, " ", "", [NStr]),
    number_string(N, NStr),                     % tamanho do puzzle
    length(GridLines, N), append(GridLines, Rest1, Rest),
    length(RegionLines, N), append(RegionLines, _, Rest1),
    maplist(parse_line, GridLines, Grid),       % converte cada linha em lista de ints
    maplist(parse_line, RegionLines, Regions).

%% parse_line(+LineString, -Nums)
% Converte string de números separados por espaço em lista de inteiros.
parse_line(Line, Nums) :-
    split_string(Line, " ", "", Strs),
    maplist(number_string, Nums, Strs).

%% print_grid(+Grid)
% Imprime cada linha do Grid com números separados por espaço.
print_grid(Grid) :-
    maplist(print_line, Grid).

% print_line(+Row)
print_line(Row) :-
    maplist(number_string, Row, Strs),
    atomic_list_concat(Strs, " ", Line),
    writeln(Line).
