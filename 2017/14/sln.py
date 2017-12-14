from __future__ import print_function

import sys


def dfs(grid, visited, state):
    row, col = state
    if state in visited:
        return
    visited.add(state)
    adjacent = [(row + 1, col), (row - 1, col), (row, col + 1), (row, col - 1)]
    for adj in adjacent:
        adj_row, adj_col = adj
        if 0 <= adj_row < len(grid) and 0 <= adj_col < len(grid[adj_row]) and grid[adj_row][adj_col] == '1':
            dfs(grid, visited, adj)


def main(inp):
    sys.path.append('..')
    import knot_hash

    grid = [''.join([bin(elem)[2:].rjust(8, '0') for elem in knot_hash.hash('{}-{}'.format(inp, idx))]) for idx in xrange(128)]
    easy_answer = 0
    for row in grid:
        easy_answer += row.count('1')
    print(easy_answer)

    visited = set()
    region_count = 0
    for row in xrange(len(grid)):
        for col in xrange(len(grid[row])):
            state = row, col
            if state not in visited and grid[row][col] == '1':
                dfs(grid, visited, state)
                region_count += 1
    print(region_count)


if __name__ == '__main__':
    main(sys.stdin.readline().strip())
