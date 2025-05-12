-- Módulo Utils.hs: Lê o arquivo de entrada e imprime o grid
module Utils (readPuzzleFromFile, printGrid) where

import System.IO

-- Define o tipo Grid como uma matriz de inteiros
type Grid = [[Int]]

-- | Lê o arquivo de entrada e retorna uma tupla (grid, regions)
-- Espera-se que o arquivo siga o formato:
--   <tamanho: n>
--   <grid>
--   <regions>
readPuzzleFromFile :: FilePath -> IO (Grid, Grid)
readPuzzleFromFile filename = do
    -- Lê todo o conteúdo do arquivo como string
    contents <- readFile filename

    -- Divide o conteúdo em linhas, remove linhas vazias
    let ls = lines contents
        nonEmptyLines = filter (not . null) ls

    -- Separa a primeira linha (dimensões) das demais
        (dimLine : rest) = nonEmptyLines
        [n] = map read (words dimLine)

    -- A seguir vêm 'n' linhas do grid e depois 'n' linhas de regiões
        -- A função take pega os n primeiros elementos de 'rest
        gridLines = take n rest
        -- A função drop remove os n primeiros elementos de 'rest
        -- e a função take pega os n primeiros elementos do restantes
        regionLines = take n (drop n rest)

        grid = map (map read . words) gridLines
        regions = map (map read . words) regionLines

    return (grid, regions)

-- | Imprime um grid na tela, separando os valores com espaço
printGrid :: Grid -> IO ()
printGrid = mapM_ (putStrLn . unwords . map show)

