// Importa a funcionalidade para ler arquivos
import scala.io.Source

object Utils {

    // Define um alias de tipo: Grid é uma matriz (array de arrays) de inteiros
    type Grid = Array[Array[Int]]

    // Função que lê um puzzle a partir de um arquivo e retorna um par (grid, regions)
    def readPuzzleFromFile(filename: String): (Grid, Grid) = {

        // Lê todas as linhas do arquivo, remove espaços em branco laterais e ignora linhas vazias
        val lines = Source.fromFile(filename).getLines().map(_.trim).filter(_.nonEmpty).toArray

        // A primeira linha do arquivo contém o tamanho da grade
        val size = lines(0).toInt

        // Cria a matriz grid: para cada linha i do grid, transforma a string em um array de inteiros
        val grid = Array.tabulate(size) { i =>
            // Divide a linha em substrings separadas por espaços e converte cada uma em inteiro
            lines(i + 1).split("\\s+").map(_.toInt)
        }

        // Cria a matriz regions: para cada linha i das regiões, transforma a string em um array de inteiros
        val regions = Array.tabulate(size) { i =>
            // As linhas das regiões começam após o grid (por isso o +1+size)
            lines(i + 1 + size).split("\\s+").map(_.toInt)
        }

        // Retorna um par (grid, regions) como resultado da função
        (grid, regions)
    }

    // Função que imprime uma matriz grid no console
    def printGrid(grid: Grid): Unit = {
        // Itera sobre cada linha da grade
        for (row <- grid) {
            // Concatena os valores da linha com espaços e imprime no console
            println(row.mkString(" "))
        }
    }
}
