import scala.collection.mutable

object KojunSolver {
    type Coord = (Int, Int)
    type Grid = Array[Array[Int]]
    type Dict = mutable.Map[Int, mutable.ListBuffer[Coord]]

    def solveKojun(board: Grid, regions: Grid): Option[Grid] = {
        val rows: Int = board.length
        val cols: Int = board(0).length
        val regionCells: Dict = mutable.Map[Int, mutable.ListBuffer[Coord]]()

        for (r <- 0 until rows; c <- 0 until cols) {
            val region: Int = regions(r)(c)
            if (!regionCells.contains(region)) {
                regionCells(region) = mutable.ListBuffer[Coord]()
            }
            regionCells(region).append((r, c))
        }

        def backtrack(): Option[Grid] = {
            val emptyCell: Option[Coord] = findEmptyCell(board)
            // Nota: findEmptyCell retorna um  Option[(Int, Int)], é uma forma do Scala lidar
            // com valores 'maybe', portanto perceba que precisamos chamar "funcoes"
            // da variavel como isDefined e get
            if (emptyCell.isEmpty) return Some(board)

            val (r, c) = emptyCell.get
            val region: Int = regions(r)(c)
            val regionSize: Int = regionCells(region).size

            for (value <- 1 to regionSize) {
                if (isValid(board, regions, r, c, value, regionCells)) {
                    board(r)(c) = value
                    val result = backtrack()
                    if (result.isDefined) return result
                    board(r)(c) = 0
                }
            }
            None
        }

        backtrack()
    }

    def isValid(board: Grid, regions: Grid, row: Int, col: Int, value: Int, 
                regionCells: Dict): Boolean = {
        val region: Int = regions(row)(col)

        // Regra 1: único na região
        for ((r, c) <- regionCells(region)) {
            if (board(r)(c) == value) return false
        }

        // Regra 2: adjacentes diferentes
        val deltas = List((-1,0), (1,0), (0,-1), (0,1))
        for ((dr, dc) <- deltas) {
            val nr = row + dr
            val nc = col + dc
            if (0 <= nr && nr < board.length && 0 <= nc && nc < board(0).length) {
                if (board(nr)(nc) == value) return false
            }
        }

        // Regra 3: ordem vertical
        // Célula acima
        if (row > 0 && regions(row)(col) == regions(row-1)(col)) {
            val upVal: Int = board(row - 1)(col)
            if (upVal != 0 && upVal <= value) return false
        }

        // Célula abaixo
        if (row < board.length - 1 && regions(row)(col) == regions(row + 1)(col)) {
            val downVal: Int = board(row + 1)(col)
            if (downVal != 0 && downVal >= value) return false
        }

        return true 
    }

    def findEmptyCell(board: Grid): Option[Coord] = {
        for (r <- board.indices; c <- board(r).indices) {
            if (board(r)(c) == 0) {
                return Some((r, c))
            }
        }
        None
    }
}
