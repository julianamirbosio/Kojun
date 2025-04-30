from kojun import solve_kojun
from utils import read_puzzle_from_file, print_grid
import sys

if __name__ == "__main__":
    if len(sys.argv) != 2:
        print("Uso: python solver.py input1.txt")
        sys.exit(1)

    grid, regions = read_puzzle_from_file(sys.argv[1])
    solved = solve_kojun(grid, regions)
    print("\nSolução:")
    print_grid(solved)