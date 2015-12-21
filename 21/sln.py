import collections
from itertools import combinations, product
import sys

player_hp = 100

def read_boss_stat():
	return int(sys.stdin.readline().rstrip().split(': ')[-1])

enemy_hp, enemy_damage, enemy_armor = [read_boss_stat() for _ in xrange(3)]

Item = collections.namedtuple('Item', ['cost', 'damage', 'armor'])

on_stock = '''
Weapons:    Cost  Damage  Armor
Dagger        8     4       0
Shortsword   10     5       0
Warhammer    25     6       0
Longsword    40     7       0
Greataxe     74     8       0

Armor:      Cost  Damage  Armor
Leather      13     0       1
Chainmail    31     0       2
Splintmail   53     0       3
Bandedmail   75     0       4
Platemail   102     0       5

Rings:      Cost  Damage  Armor
Damage +1    25     1       0
Damage +2    50     2       0
Damage +3   100     3       0
Defense +1   20     0       1
Defense +2   40     0       2
Defense +3   80     0       3
'''

store = collections.defaultdict(list)
current_category = None
for row in on_stock.split('\n'):
	if not row:
		continue
	tokens = row.split()
	if tokens[0].endswith(':'):
		current_category = tokens[0][:-1]
	else:
		nums = map(int, tokens[-3:])
		store[current_category].append(Item(*nums))

def real_damage(my_damage, his_armor):
	return max(1, my_damage - his_armor)

def time_to_live(hp, real_enemy_damage):
	return (hp / real_enemy_damage) + int(hp % real_enemy_damage != 0)

def player_wins(player_damage, player_armor):
	player_ttl = time_to_live(player_hp, real_damage(enemy_damage, player_armor))
	enemy_ttl = time_to_live(enemy_hp, real_damage(player_damage, enemy_armor))
	return player_ttl >= enemy_ttl

def enumerate_mega_items():
	select = lambda item_type, count: combinations(store[item_type], count)
	for w, a, r in product([1], [0, 1], [0, 1, 2]):
		for ww, aa, rr in product(select('Weapons', w), select('Armor', a), select('Rings', r)):
			all_items = [item for items in (ww, aa, rr) for item in items]
			sum_items = lambda i1, i2: Item(i1.cost + i2.cost, i1.damage + i2.damage, i1.armor + i2.armor)
			mega_item = reduce(sum_items, all_items)
			yield mega_item

print min([mi for mi in enumerate_mega_items() if player_wins(mi.damage, mi.armor)], key=lambda mi: mi.cost)
print max([mi for mi in enumerate_mega_items() if not player_wins(mi.damage, mi.armor)], key=lambda mi: mi.cost)