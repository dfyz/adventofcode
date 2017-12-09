from __future__ import print_function


def traverse(tree, root):
    result = tree[root][0]
    children = tree[root][1]
    if children:
        child_weights = [traverse(tree, ch) for ch in children]
        if len(set(child_weights)) != 1:
            n = len(child_weights)
            for idx in xrange(n):
                prev_weight = child_weights[(idx - 1 + n) % n]
                cur_weight = child_weights[idx]
                next_weight = child_weights[(idx + 1) % n]
                if cur_weight != prev_weight and cur_weight != next_weight:
                    delta = prev_weight - cur_weight
                    bad_child = children[idx]
                    corrected_weight = tree[bad_child][0] + delta
                    child_weights[idx] += delta
                    print(corrected_weight)
        result += sum(child_weights)
    return result


def main(inp):
    tree = {}
    have_parent = set()
    nodes = set()
    for line in inp:
        tokens = line.rstrip().split(' -> ')
        name, weight = tokens[0].split()
        weight = int(weight[1:-1])
        children = tokens[1].split(', ') if len(tokens) > 1 else []
        have_parent.update(children)
        nodes.update(children)
        nodes.add(name)
        tree[name] = (weight, children)
    root_node = list(nodes - have_parent)[0]
    print(root_node)
    traverse(tree, root_node)


if __name__ == '__main__':
    with open('input.txt') as f:
        main(f)