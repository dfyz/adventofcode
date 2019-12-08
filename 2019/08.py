from pathlib import Path


WIDTH = 25
HEIGHT = 6


def parse_image(data):
    layer_size = WIDTH * HEIGHT
    return [data[start:start + layer_size] for start in range(0, len(data), layer_size)]


def solve_easy(data):
    min_layer = min(data, key=lambda x: x.count('0'))
    return min_layer.count('1') * min_layer.count('2')


def main():
    data = parse_image(Path('08.txt').read_text().strip())
    print(solve_easy(data))


if __name__ == '__main__':
    main()