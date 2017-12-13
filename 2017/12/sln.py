from __future__ import print_function

import collections


def dfs(visited, graph, start):
    visited.add(start)
    for adj in graph[start]:
        if adj not in visited:
            dfs(visited, graph, adj)


def main(inp):
    graph = collections.defaultdict(list)
    for line in inp:
        src, dst = line.rstrip().split(' <-> ')
        for d in dst.split(', '):
            graph[src].append(d)
            graph[d].append(src)
    all_nodes = set(graph)
    visited = set()
    dfs(visited, graph, '0')
    print(len(visited))
    group_count = 1
    while len(visited) != len(all_nodes):
        dfs(visited, graph, list(all_nodes - visited)[0])
        group_count += 1
    print(group_count)


if __name__ == '__main__':
    with open('input.txt') as inp:
        main(inp)
