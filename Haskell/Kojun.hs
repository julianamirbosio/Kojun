-- Módulo Kojun.hs: Implementa a lógica de resolução do puzzle Kojun
module Kojun (solveKojun, Grid, RegionMap) where

type Coord = (Int, Int)
type Grid = [[Int]]
type RegionMap = [[Int]]

-- Função principal: resolve o puzzle Kojun
solveKojun :: Grid -> RegionMap -> Maybe Grid
solveKojun grid regions = backtrack grid regions (0, 0)


backtrack :: Grid -> RegionMap -> Coord -> Maybe Grid
backtrack grid regions (r, c)
  -- caso base: se já percorreu todas as linhas, retorna o grid
  | r == length grid = Just grid
  -- se a célula grid[r][c] já estiver preenchida, continua para a próxima
  | grid !! r !! c /= 0 = backtrack grid regions (nextCoord grid (r, c))
  | otherwise =
      -- celula vazia, pegamos o id da regiao
      let regionId = regions !! r !! c
          -- calcula o tamanho da região
          regionSize = length [ () | i <- [0..length grid - 1], j <- [0..length grid - 1], regions !! i !! j == regionId ]
          -- gera os candidatos possíveis (quantidade de números na região)
          candidates = [1..regionSize]
          -- para cada candidato, atualiza o grid e tenta resolver, colocando cada v candidato na posiçao
          -- e gerando possibilidades de grids
          nextGrids = [ updateGrid grid (r, c) v | v <- candidates, valid grid regions (r, c) v ]
      -- passo recursivo
      in trySolutions nextGrids regions (nextCoord grid (r, c))


-- Tenta soluções possíveis
trySolutions :: [Grid] -> RegionMap -> Coord -> Maybe Grid
-- caso base: a lista de grids esta vazia, retorna Nothing
trySolutions [] _ _ = Nothing
-- passo recursivo: testa o primeiro grid da lista
trySolutions (g:gs) regions coord =
  -- chama backtrak recursivamente para o grid atual, a mesma regiao e a proxima coordenada
  case backtrack g regions coord of
    -- se o backtrack com esse grid deu certo, retornamos, interrompendo a busca,
    -- e não testamos as demais tentativas(gs), pois uma ja funcionou!
    Just solution -> Just solution
    -- se não, continuamos testando os demais grids
    Nothing -> trySolutions gs regions coord


-- Calcula a próxima coordenada no grid
nextCoord :: Grid -> Coord -> Coord
nextCoord grid (r, c)
  -- se já percorreu todas as colunas, vai para a próxima linha
  | c < length (head grid) - 1 = (r, c + 1)
  | otherwise                  = (r + 1, 0)


-- Atualiza uma célula do grid com um novo valor
updateGrid :: Grid -> Coord -> Int -> Grid
updateGrid grid (r, c) val =
  -- pega todas as linhas acima da linha r (exclusiva)
  take r grid ++
  -- pega a linha r, substitui o valor na coluna c e concatena com o restante da linha
  [take c (grid !! r) ++ [val] ++ drop (c + 1) (grid !! r)] ++
  -- pega todas as linhas abaixo da linha r
  drop (r + 1) grid
  -- Na concatenação, retorna o grid com o novo valor na posição (r, c)


-- Verifica se o valor é válido na posição dada
valid :: Grid -> RegionMap -> Coord -> Int -> Bool
valid grid regions coord@(r, c) val =
  -- testa as 3 regras do puzzle Kojun
  notSameAsAdjacent && notInRegion && validVertical grid regions coord val
  where
    notSameAsAdjacent =
      -- verifica se o valor não é igual aos vizinhos
      getCell grid (r - 1, c) /= Just val &&
      getCell grid (r + 1, c) /= Just val &&
      getCell grid (r, c - 1) /= Just val &&
      getCell grid (r, c + 1) /= Just val

    regionId = regions !! r !! c
    -- gera uma lista com todas as coordenadas da regiao atual de (r, c)
    regionCells = [ (i, j) | i <- [0..length grid - 1], j <- [0..length grid - 1], regions !! i !! j == regionId ]
    -- filtra os valores já preenchidas na região
    regionVals = [ grid !! i !! j | (i, j) <- regionCells, grid !! i !! j /= 0 ]
    -- verifica se o valor não está presente na região
    notInRegion = val `notElem` regionVals


-- Validação da ordem vertical dentro da mesma região
validVertical :: Grid -> RegionMap -> Coord -> Int -> Bool
validVertical grid regions (r, c) val =
  let upRegion    = getRegion regions (r - 1, c)
      downRegion  = getRegion regions (r + 1, c)
  -- primeiro analisamos a celula acima
  in case getCell grid (r - 1, c) of
      -- se a celula contem um valor u, então, u não pode ser igual a val
      -- e se a região acima for diferente da região atual, u deve ser maior que val
      Just u -> u /= val && (upRegion /= Just regionId || u > val)
      -- se a celula acima não existe (fora dos limites ou fora da região), não há restricao
      Nothing -> True
    -- continua com a validaçao, testando a regra da celula abaixo no mesmo in
    &&
    case getCell grid (r + 1, c) of
      -- se a celula contem um valor d, então, d não pode ser igual a val
      -- e se a região abaixo for diferente da região atual, d deve ser menor que val
      Just d -> d /= val && (downRegion /= Just regionId || val > d)
      -- novamente, se a celula acima não existe (fora dos limites ou fora da região), não há restricao
      Nothing -> True
  where
    -- area de variavel local, calcula o ID da regiao
    regionId = regions !! r !! c


-- Acesso seguro ao valor do grid
getCell :: Grid -> Coord -> Maybe Int
getCell grid (r, c)
  | r < 0 || r >= length grid = Nothing
  | c < 0 || c >= length (head grid) = Nothing
  | otherwise = Just ((grid !! r) !! c)

-- Acesso seguro à região
getRegion :: RegionMap -> Coord -> Maybe Int
getRegion regions (r, c)
  | r < 0 || r >= length regions = Nothing
  | c < 0 || c >= length (head regions) = Nothing
  | otherwise = Just ((regions !! r) !! c)