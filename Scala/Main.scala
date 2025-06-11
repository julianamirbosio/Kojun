/* 
 * scalac Utils.scala KojunSolver.scala Main.scala  
 * scala Main ../inputs/input1.txt
 */

/* 
 * Para compilar o projeto:
 *   scalac Utils.scala KojunSolver.scala Main.scala
 * Para rodar o programa:
 *   scala Main ../inputs/input1.txt
 */

// Importa tudo do objeto Utils (funções utilitárias como readPuzzleFromFile e printGrid)
import Utils._
// Importa tudo do objeto KojunSolver (função solveKojun)
import KojunSolver._

object Main {

    // Método main
    def main(args: Array[String]): Unit = {

        // Verifica se o número de argumentos passados é exatamente 1 (deve ser o nome do arquivo de entrada)
        if (args.length != 1) {
            println("Usage: scala Main input1.txt") // Imprime mensagem de uso correto
            System.exit(1)                         // Sai do programa com código de erro 1
        }

        // Lê o puzzle a partir do arquivo cujo nome foi passado como argumento
        // Desempacota o resultado em duas variáveis: grid e regions
        val (grid, regions) = readPuzzleFromFile(args(0))

        // Chama o resolvedor de Kojun para tentar resolver o puzzle
        // O resultado é um Option[Grid]: Some(grid resolvido) ou None se não houver solução
        val solvedOpt = solveKojun(grid, regions)

        // Imprime a solução no console
        println("\nSolution:")
        solvedOpt match {
            // Caso tenha encontrado uma solução, imprime o grid resolvido
            case Some(solvedGrid) => printGrid(solvedGrid)
            // Caso contrário, informa que não foi possível encontrar uma solução
            case None => println("No solution found.")
        }
    }
}

