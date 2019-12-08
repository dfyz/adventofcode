from pathlib import Path


WIDTH = 25
HEIGHT = 6


def parse_image(data):
    layer_size = WIDTH * HEIGHT
    return [data[start:start + layer_size] for start in range(0, len(data), layer_size)]


def solve_easy(data):
    min_layer = min(data, key=lambda x: x.count('0'))
    return min_layer.count('1') * min_layer.count('2')


def solve_hard(data):
    def combine_pixels(p1, p2):
        return p1 if p1 != '2' else p2

    def combine_layers(l1, l2):
        return [combine_pixels(p1, p2) for p1, p2 in zip(l1, l2)]

    final_layer = data[0]
    for layer in data[1:]:
        final_layer = combine_layers(final_layer, layer)

    return '\n'.join(''.join(final_layer[start:start + WIDTH]) for start in range(0, len(final_layer), WIDTH)).replace('0', ' ')


def main():
    data = parse_image(Path('08.txt').read_text().strip())
    print(solve_easy(data))
    print(solve_hard(data))


if __name__ == '__main__':
    main()