
def solve_kojun(board, regions):
    rows, cols = len(board), len(board[0])
    region_cells = {}

    # Pré-processamento: mapeia cada região para suas células
    for r in range(rows):
        for c in range(cols):
            region = regions[r][c]
            if region not in region_cells:
                region_cells[region] = []
            region_cells[region].append((r, c))

    def backtrack():
        find = find_empty(board)
        if not find:
            return True

        row, col = find
        region = regions[row][col]
        max_val = len(region_cells[region])  # Máximo valor permitido na região

        for val in range(1, max_val + 1):
            if valid(board, regions, (row, col), val, region_cells):
                board[row][col] = val
                if backtrack():
                    return True
                board[row][col] = 0  # desfaz (backtrack)

        return False

    if backtrack():
        return board
    else:
        print("No solution exists")

def valid(board, regions, pos, val, region_cells):
    row, col = pos
    region = regions[row][col]

    # Regra 1: único na região
    for (rr, cc) in region_cells[region]:
        if board[rr][cc] == val:
            return False

    # Regra 2: adjacentes diferentes
    for dr, dc in [(-1,0), (1,0), (0,-1), (0,1)]:
        nr, nc = row + dr, col + dc
        if 0 <= nr < len(board) and 0 <= nc < len(board[0]) and board[nr][nc] == val:
            return False

    # Regra 3: ordem vertical
    # Célula acima
    if row > 0 and regions[row][col] == regions[row - 1][col]:
        up_val = board[row - 1][col]
        if up_val != 0 and val != 0 and up_val <= val:
            return False
    # Célula abaixo
    if row < len(board) - 1 and regions[row][col] == regions[row + 1][col]:
        down_val = board[row + 1][col]
        if down_val != 0 and val != 0 and val <= down_val:
            return False

    return True


def find_empty(board):
    for i in range(len(board)):
        for j in range(len(board[i])):
            if board[i][j] == 0:
                return (i, j)  # linha, coluna
    return None