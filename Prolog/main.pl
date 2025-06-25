/* 
    Universidade Federal de Santa Catarina
    Trabalho 1 de Paradigmas de Programação
    Grupo:
        - Heloísa Jonck Hammes (23200361)
        - Juliana Miranda Bosio (23201966)

    Uso:
        Compile  e execute os arquivos com o comando:
        swipl -q -s Prolog/main.pl -- inputs/input.txt
*/

% Ponto de entrada do programa Kojun em Prolog
:- use_module(utils).
:- use_module(kojun_solver).

% Ao iniciar, executa main/0
:- initialization(main).

main :-
    % Obtém lista de argumentos da linha de comando
    current_prolog_flag(argv, Argv),
    ( Argv = [File|_] ->
        % Lê puzzle e regiões do arquivo
        read_puzzle_from_file(File, Grid, Regions),
        % Tenta resolver
        ( solve_kojun(Grid, Regions, Solution) ->
            writeln(''),
            writeln('Solução:'),
            print_grid(Solution)
        ;
            writeln('Nenhuma solução encontrada.')
        )
    ;
        % Caso sem argumentos
        writeln('Uso: swipl -q -s main.pl -- <arquivo_de_entrada>')
    ),
    halt.
