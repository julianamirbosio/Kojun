def solve_kojun(grid, regions):
    rows, cols = len(grid), len(grid[0])
    region_cells = {}

    # Pré-processamento: mapeia cada região para suas células
    for r in range(rows):
        for c in range(cols):
            region = regions[r][c]
            if region not in region_cells:
                region_cells[region] = []
            region_cells[region].append((r, c))

    def is_valid(r, c, val):
        region = regions[r][c]

        # Regra 1: único na região
        for (rr, cc) in region_cells[region]:
            if grid[rr][cc] == val:
                return False

        # Regra 2: adjacentes diferentes
        for dr, dc in [(-1,0), (1,0), (0,-1), (0,1)]:
            nr, nc = r + dr, c + dc
            if 0 <= nr < rows and 0 <= nc < cols and grid[nr][nc] == val:
                return False

        # Regra 3: ordem vertical
        
        # Célula acima
        if r > 0 and regions[r][c] == regions[r - 1][c]:
            up_val = grid[r - 1][c]
            if up_val != 0 and not (up_val > val):
                return False

        # Célula abaixo
        if r < rows - 1 and regions[r][c] == regions[r + 1][c]:
            down_val = grid[r + 1][c]
            if down_val != 0 and not (val > down_val):
                return False

        return True

    def backtrack():
        for r in range(rows):
            for c in range(cols):
                if grid[r][c] == 0:
                    region = regions[r][c]
                    size = len(region_cells[region])
                    for val in range(1, size + 1):
                        if is_valid(r, c, val):
                            # print(f"Tentando {val} em ({r},{c})")
                            grid[r][c] = val
                            if backtrack():
                                return True
                            grid[r][c] = 0
                    return False
        return True

    backtrack()
    return grid