# Lê o arquivo e printa
def read_puzzle_from_file(filename):
    with open(filename) as f:
        lines = [line.strip() for line in f if line.strip()]

    dims = list(map(int, lines[0].split()))
    rows, cols = dims
    grid = [list(map(int, lines[i + 1].split())) for i in range(rows)]
    regions = [list(map(int, lines[i + 1 + rows].split())) for i in range(rows)]

    return grid, regions


def print_grid(grid):
    for row in grid:
        print(" ".join(str(val) for val in row))