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
  | r == length grid = Just grid
  | grid !! r !! c /= 0 = backtrack grid regions (nextCoord grid (r, c))
  | otherwise =
      let regionId = regions !! r !! c
          regionSize = length [ () | i <- [0..length grid - 1], j <- [0..length grid - 1], regions !! i !! j == regionId ]
          candidates = [1..regionSize]
          nextGrids = [ updateGrid grid (r, c) v | v <- candidates, valid grid regions (r, c) v ]
      in trySolutions nextGrids regions (nextCoord grid (r, c))

-- Tenta soluções possíveis
trySolutions :: [Grid] -> RegionMap -> Coord -> Maybe Grid
trySolutions [] _ _ = Nothing
trySolutions (g:gs) regions coord =
  case backtrack g regions coord of
    Just solution -> Just solution
    Nothing -> trySolutions gs regions coord

-- Calcula a próxima coordenada no grid
nextCoord :: Grid -> Coord -> Coord
nextCoord grid (r, c)
  | c < length (head grid) - 1 = (r, c + 1)
  | otherwise                  = (r + 1, 0)

-- Atualiza uma célula do grid com um novo valor
updateGrid :: Grid -> Coord -> Int -> Grid
updateGrid grid (r, c) val =
  take r grid ++
  [take c (grid !! r) ++ [val] ++ drop (c + 1) (grid !! r)] ++
  drop (r + 1) grid

-- Verifica se o valor é válido na posição dada
valid :: Grid -> RegionMap -> Coord -> Int -> Bool
valid grid regions coord@(r, c) val =
  notSameAsAdjacent && notInRegion && validVertical grid regions coord val
  where
    notSameAsAdjacent =
      getCell grid (r - 1, c) /= Just val &&
      getCell grid (r + 1, c) /= Just val &&
      getCell grid (r, c - 1) /= Just val &&
      getCell grid (r, c + 1) /= Just val

    regionId = regions !! r !! c
    regionCells = [ (i, j) | i <- [0..length grid - 1], j <- [0..length grid - 1], regions !! i !! j == regionId ]
    regionVals = [ grid !! i !! j | (i, j) <- regionCells, grid !! i !! j /= 0 ]
    notInRegion = val `notElem` regionVals

-- Validação da ordem vertical dentro da mesma região
validVertical :: Grid -> RegionMap -> Coord -> Int -> Bool
validVertical grid regions (r, c) val =
  let upRegion    = getRegion regions (r - 1, c)
      downRegion  = getRegion regions (r + 1, c)
  in case getCell grid (r - 1, c) of
       Just u -> u /= val && (upRegion /= Just regionId || u > val)
       Nothing -> True
     &&
     case getCell grid (r + 1, c) of
       Just d -> d /= val && (downRegion /= Just regionId || val > d)
       Nothing -> True
  where
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