// Importa a coleção mutable do Scala (para usar Map e ListBuffer mutáveis)
import scala.collection.mutable

object KojunSolver {

    // Define alias de tipo Coord = par (linha, coluna)
    type Coord = (Int, Int)
    // Define alias Grid = matriz de inteiros
    type Grid = Array[Array[Int]]
    // Define alias Dict = Map de Int (id da região) para uma lista mutável de coordenadas
    type Dict = mutable.Map[Int, mutable.ListBuffer[Coord]]

    // Função principal que resolve o puzzle Kojun
    def solveKojun(board: Grid, regions: Grid): Option[Grid] = {

        // Número de linhas e colunas da grade
        val rows: Int = board.length
        val cols: Int = board(0).length

        // Inicializa um dicionário para mapear cada região às suas células (coordenadas)
        val regionCells: Dict = mutable.Map[Int, mutable.ListBuffer[Coord]]()

        // Preenche regionCells com as coordenadas de cada célula da grade, agrupadas por região
        for (r <- 0 until rows; c <- 0 until cols) {
            val region: Int = regions(r)(c)
            if (!regionCells.contains(region)) {
                regionCells(region) = mutable.ListBuffer[Coord]()
            }
            regionCells(region).append((r, c))
        }

        // Função interna que implementa a busca por backtracking
        def backtrack(): Option[Grid] = {

            // Busca a próxima célula vazia (com valor 0)
            val emptyCell: Option[Coord] = findEmptyCell(board)

            // Caso base: se não há mais células vazias, o puzzle está resolvido
            if (emptyCell.isEmpty) return Some(board)

            // Desempacota a coordenada da célula vazia
            val (r, c) = emptyCell.get
            // Obtém o id da região dessa célula
            val region: Int = regions(r)(c)
            // Obtém o tamanho da região (quantas células ela possui)
            val regionSize: Int = regionCells(region).size

            // Tenta colocar valores de 1 até regionSize na célula atual
            for (value <- 1 to regionSize) {

                // Verifica se é válido colocar esse valor na célula
                if (isValid(board, regions, r, c, value, regionCells)) {

                    // Atribui o valor à célula
                    board(r)(c) = value

                    // Chama recursivamente o backtracking
                    val result = backtrack()

                    // Se encontrou uma solução, propaga a solução para cima
                    if (result.isDefined) return result

                    // Se não deu certo, desfaz a tentativa (backtrack)
                    board(r)(c) = 0
                }
            }

            // Se nenhum valor foi válido, retorna None (impasse)
            None
        }

        // Inicia a busca
        backtrack()
    }

    // Função que verifica se um valor é válido em uma posição específica
    def isValid(board: Grid, regions: Grid, row: Int, col: Int, value: Int, 
                regionCells: Dict): Boolean = {

        val region: Int = regions(row)(col)

        // Regra 1: o valor deve ser único na sua região
        for ((r, c) <- regionCells(region)) {
            if (board(r)(c) == value) return false
        }

        // Regra 2: o valor deve ser diferente dos valores em células adjacentes (acima, abaixo, esquerda, direita)
        val deltas = List((-1,0), (1,0), (0,-1), (0,1))
        for ((dr, dc) <- deltas) {
            val nr = row + dr
            val nc = col + dc
            // Verifica se a célula vizinha está dentro dos limites da grade
            if (0 <= nr && nr < board.length && 0 <= nc && nc < board(0).length) {
                if (board(nr)(nc) == value) return false
            }
        }

        // Regra 3: respeitar a ordem vertical na região (números decrescentes de cima para baixo)
        // Se a célula de cima for da mesma região, ela deve ser maior
        if (row > 0 && regions(row)(col) == regions(row-1)(col)) {
            val upVal: Int = board(row - 1)(col)
            if (upVal != 0 && upVal <= value) return false
        }

        // Se a célula de baixo for da mesma região, ela deve ser menor
        if (row < board.length - 1 && regions(row)(col) == regions(row + 1)(col)) {
            val downVal: Int = board(row + 1)(col)
            if (downVal != 0 && downVal >= value) return false
        }

        // Se passou em todas as verificações, o valor é válido
        return true 
    }

    // Função que localiza a próxima célula vazia na grade
    def findEmptyCell(board: Grid): Option[Coord] = {
        // Percorre cada célula da grade
        for (r <- board.indices; c <- board(r).indices) {
            if (board(r)(c) == 0) {
                // Retorna a primeira célula vazia encontrada
                return Some((r, c))
            }
        }
        // Se não houver mais células vazias, retorna None
        None
    }
}
