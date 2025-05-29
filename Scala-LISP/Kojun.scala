object KojunSolver {
  type Coord = (Int, Int)
  type Grid = Vector[Vector[Int]]
  type RegionMap = Vector[Vector[Int]]

  def solveKojun(grid: Grid, regions: RegionMap): Option[Grid] = {
    backtrack(grid, regions, (0, 0))
  }

  def backtrack(grid: Grid, regions: RegionMap, coord: Coord): Option[Grid] = {
    val (r, c) = coord
    if (r == grid.length) Some(grid)
    else if (grid(r)(c) != 0) backtrack(grid, regions, nextCoord(grid, coord))
    else {
      val regionId = regions(r)(c)
      val regionSize = (for {
        i <- grid.indices
        j <- grid(i).indices
        if regions(i)(j) == regionId
      } yield ()).length

      val candidates = (1 to regionSize).toList
      val nextGrids = candidates.collect {
        case v if valid(grid, regions, coord, v) => updateGrid(grid, coord, v)
      }

      trySolutions(nextGrids, regions, nextCoord(grid, coord))
    }
  }

  def trySolutions(grids: List[Grid], regions: RegionMap, coord: Coord): Option[Grid] = {
    grids match {
      case Nil => None
      case g :: gs =>
        backtrack(g, regions, coord) match {
          case Some(solution) => Some(solution)
          case None => trySolutions(gs, regions, coord)
        }
    }
  }

  def nextCoord(grid: Grid, coord: Coord): Coord = {
    val (r, c) = coord
    if (c < grid.head.length - 1) (r, c + 1)
    else (r + 1, 0)
  }

  def updateGrid(grid: Grid, coord: Coord, value: Int): Grid = {
    val (r, c) = coord
    grid.updated(r, grid(r).updated(c, value))
  }

  def valid(grid: Grid, regions: RegionMap, coord: Coord, value: Int): Boolean = {
    val (r, c) = coord
    val regionId = regions(r)(c)

    val notSameAsAdjacent = List((-1,0), (1,0), (0,-1), (0,1)).forall { case (dr, dc) =>
      getCell(grid, (r + dr, c + dc)).forall(_ != value)
    }

    val regionCells = for {
      i <- grid.indices
      j <- grid(i).indices
      if regions(i)(j) == regionId
    } yield (i, j)

    val regionVals = regionCells.collect {
      case (i, j) if grid(i)(j) != 0 => grid(i)(j)
    }

    val notInRegion = !regionVals.contains(value)

    notSameAsAdjacent && notInRegion && validVertical(grid, regions, coord, value)
  }

  def validVertical(grid: Grid, regions: RegionMap, coord: Coord, value: Int): Boolean = {
    val (r, c) = coord
    val regionId = regions(r)(c)

    val upValid = getCell(grid, (r - 1, c)) match {
      case Some(u) => u != value && (getRegion(regions, (r - 1, c)).forall(_ != regionId) || u > value)
      case None => true
    }

    val downValid = getCell(grid, (r + 1, c)) match {
      case Some(d) => d != value && (getRegion(regions, (r + 1, c)).forall(_ != regionId) || value > d)
      case None => true
    }

    upValid && downValid
  }

  def getCell(grid: Grid, coord: Coord): Option[Int] = {
    val (r, c) = coord
    if (r >= 0 && r < grid.length && c >= 0 && c < grid(r).length) Some(grid(r)(c))
    else None
  }

  def getRegion(regions: RegionMap, coord: Coord): Option[Int] = {
    val (r, c) = coord
    if (r >= 0 && r < regions.length && c >= 0 && c < regions(r).length) Some(regions(r)(c))
    else None
  }
} 
