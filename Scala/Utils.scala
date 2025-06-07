import scala.io.Source

object Utils {
    type Grid = Array[Array[Int]]

    // Reads puzzle from file and returns (grid, regions)
    def readPuzzleFromFile(filename: String): (Grid, Grid) = {
        val lines = Source.fromFile(filename).getLines().map(_.trim).filter(_.nonEmpty).toArray

        val size = lines(0).toInt

        val grid = Array.tabulate(size) { i =>
            lines(i + 1).split("\\s+").map(_.toInt)
        }

        val regions = Array.tabulate(size) { i =>
            lines(i + 1 + size).split("\\s+").map(_.toInt)
        }

        (grid, regions)
    }

    // Prints the grid
    def printGrid(grid: Grid): Unit = {
        for (row <- grid) {
            println(row.mkString(" "))
        }
    }
}


