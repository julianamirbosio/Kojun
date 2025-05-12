type Coord = (Int, Int)
type Grid = [[Int]]
type RegionMap = [[Int]]

main :: IO ()
main = case solveKojun problem regions of
  Just solution -> mapM_ print solution
  Nothing -> putStrLn "Nenhuma solução encontrada."

solveKojun :: Grid -> RegionMap -> Maybe Grid
solveKojun grid regions = backtrack grid regions (0, 0)

backtrack :: Grid -> RegionMap -> Coord -> Maybe Grid
backtrack grid regions (r, c)
  | r == length grid = Just grid
  | grid !! r !! c /= 0 = backtrack grid regions (nextCoord (r, c))
  | otherwise =
      let regionId = regions !! r !! c
          regionSize = length [ () | i <- [0..length grid - 1], j <- [0..length grid - 1], regions !! i !! j == regionId ]
          candidates = [1..regionSize]
          nextGrids = [ updateGrid grid (r, c) v | v <- candidates, valid grid regions (r, c) v ]
      in trySolutions nextGrids regions (nextCoord (r, c))

trySolutions :: [Grid] -> RegionMap -> Coord -> Maybe Grid
trySolutions [] _ _ = Nothing
trySolutions (g:gs) regions coord =
  case backtrack g regions coord of
    Just solution -> Just solution
    Nothing -> trySolutions gs regions coord

nextCoord :: Coord -> Coord
nextCoord (r, c)
  | c < 9     = (r, c + 1)
  | otherwise = (r + 1, 0)

updateGrid :: Grid -> Coord -> Int -> Grid
updateGrid grid (r, c) val =
  take r grid ++
  [take c (grid !! r) ++ [val] ++ drop (c + 1) (grid !! r)] ++
  drop (r + 1) grid

-- Validação de valor
valid :: Grid -> RegionMap -> Coord -> Int -> Bool
valid grid regions coord@(r, c) val =
  notSameAsAdjacent && notInRegion && validVertical grid regions coord val
  where
    -- Agora verifica todos os 4 vizinhos ortogonais
    notSameAsAdjacent =
      getCell grid (r - 1, c) /= Just val &&
      getCell grid (r + 1, c) /= Just val &&
      getCell grid (r, c - 1) /= Just val &&
      getCell grid (r, c + 1) /= Just val

    regionId = regions !! r !! c
    regionCells = [ (i, j) | i <- [0..length grid - 1], j <- [0..length grid - 1], regions !! i !! j == regionId ]
    regionVals = [ grid !! i !! j | (i, j) <- regionCells, grid !! i !! j /= 0 ]
    notInRegion = val `notElem` regionVals

-- Verificação vertical: ordem em regiões verticais
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

-- Acessos seguros
getCell :: Grid -> Coord -> Maybe Int
getCell grid (r, c)
  | r < 0 || r >= length grid = Nothing
  | c < 0 || c >= length (head grid) = Nothing
  | otherwise = Just ((grid !! r) !! c)

getRegion :: RegionMap -> Coord -> Maybe Int
getRegion regions (r, c)
  | r < 0 || r >= length regions = Nothing
  | c < 0 || c >= length (head regions) = Nothing
  | otherwise = Just ((regions !! r) !! c)

-- Entrada do problema
problem :: Grid
problem =
  [ [5,0,2,0,2,0,3,1,3,1]
  , [0,4,0,1,0,5,0,5,0,4]
  , [7,5,1,7,0,0,3,1,3,0]
  , [0,4,0,0,0,0,0,0,0,3]
  , [2,0,3,4,0,2,0,0,4,0]
  , [5,0,2,0,6,0,0,0,0,0]
  , [0,1,3,0,1,0,0,4,0,3]
  , [6,7,0,3,0,1,4,0,0,1]
  , [4,0,3,0,4,0,0,0,0,3]
  , [0,1,0,2,0,6,2,0,2,1]
  ]

regions :: RegionMap
regions =
  [ [1,5,5,5,9,9,9,9,20,20]
  , [1,1,1,5,6,6,13,13,20,13]
  , [2,2,1,6,6,12,14,13,13,13]
  , [2,2,6,6,10,12,14,14,14,18]
  , [2,2,2,6,10,10,15,18,18,18]
  , [3,3,7,7,7,10,16,16,21,21]
  , [3,3,3,7,7,11,17,19,21,21]
  , [4,4,3,7,11,11,17,19,22,22]
  , [4,4,8,8,8,8,17,19,19,23]
  , [4,4,4,8,8,8,17,17,23,23]
  ]
