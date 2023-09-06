type Board = Array[Array[Boolean]]

def drawBoard(board: Board): Unit = {
  print("\u001b[2J")
  for (row <- board) {
    row.map(if _ then {
      '#'
    } else {
      '.'
    }).foreach(print)
    println()
  }
  println()
}

def isValidCell(board: Board, row: Int, col: Int): Boolean = {
  val nRows = board.length
  val nCols = board(0).length

  row >= 0 && row < nRows && col >= 0 && col < nCols
}

def countAliveNeighbours(board: Board, row: Int, col: Int): Int = {
  var neighbours = 0

  for (i <- row - 1 until row + 2) {
    for (j <- col - 1 until col + 2) {
      if ((i != row || j != col) && isValidCell(board, i, j) && board(i)(j)) {
        neighbours += 1
      }
    }
  }

  neighbours
}

def cycle(board: Board): Board = {
  val nRows = board.length
  val nCols = board(0).length
  val newBoard: Board = Array.ofDim(nRows, nCols)

  for (row <- 0 until nRows) {
    for (col <- 0 until nCols) {
      val aliveNeighbours = countAliveNeighbours(board, row, col)

      newBoard(row)(col) = if (board(row)(col)) {
        aliveNeighbours == 2 || aliveNeighbours == 3
      } else {
        aliveNeighbours == 3
      }
    }
  }

  newBoard
}

@main
def main(): Unit = {
  val boardWidth = 30
  val boardHeight = 10
  var board: Board = Array.ofDim(boardHeight, boardWidth)

  board(2)(1) = true
  board(3)(2) = true
  board(3)(3) = true
  board(2)(3) = true
  board(1)(3) = true

  while (true) {
    board = cycle(board)
    drawBoard(board)
    Thread.sleep(100)
  }
}