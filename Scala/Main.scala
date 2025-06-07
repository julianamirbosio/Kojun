import Utils._
import KojunSolver._

object Main {
    def main(args: Array[String]): Unit = {
        if (args.length != 1) {
            println("Usage: scala Main input1.txt")
            System.exit(1)
        }

        val (grid, regions) = readPuzzleFromFile(args(0))
        val solvedOpt = solveKojun(grid, regions)

        println("\nSolution:")
        solvedOpt match {
            case Some(solvedGrid) => printGrid(solvedGrid)
            case None => println("No solution found.")
        }
    }
}
