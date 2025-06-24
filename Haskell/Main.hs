{-  
    Universidade Federal de Santa Catarina
    Trabalho 1 de Paradigmas de Programação
    Grupo:
        - Heloísa Jonck Hammes (23200361)
        - Juliana Miranda Bosio (23201966)

    Uso:
        Compile os arquivos com o comando:
        ghc Main.hs Kojun.hs Utils.hs -o kojun

        Execute o programa com o comando:
        ./kojun <arquivo_de_entrada>
        ./kojun problems/input.txt
-}


module Main where

-- Importa os módulos necessários
import System.Environment (getArgs)
import Kojun (solveKojun)
import Utils (readPuzzleFromFile, printGrid)

main :: IO ()
main = do
    -- Lê os argumentos passados pelo terminal
    args <- getArgs

    -- Analisa os argumentos recebidos
    case args of
        -- Se houver exatamente 1 argumento (o nome do arquivo)
        [filename] -> do
            -- Lê o grid e as regiões a partir do arquivo de entrada
            (problem, regions) <- readPuzzleFromFile filename

            -- Tenta resolver o puzzle com backtracking
            case solveKojun problem regions of
                -- Se houver solução, imprime
                Just solution -> do
                    putStrLn "\nSolução:"
                    printGrid solution
                -- Caso contrário, informa que não há solução
                Nothing -> putStrLn "Nenhuma solução encontrada."

        -- Se os argumentos estiverem errados, mostra mensagem de uso
        _ -> putStrLn "Uso: ./Main <arquivo_de_entrada>"
