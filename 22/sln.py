import sys

def read_stat():
	return int(sys.stdin.readline().rstrip().split()[-1])

initial_enemy_health = read_stat()
enemy_damage = read_stat()

initial_health = int(sys.argv[1])
initial_mana = int(sys.argv[2])

cache = {}

MAGIC_MISSILE_COST = 53
DRAIN_COST = 73
SHIELD_COST = 113
POISON_COST = 173
RECHARGE_COST = 229

def solve(*args):
	state = args
	if state not in cache:
		cache[state] = solve_non_cached(*args)
	return cache[state]

def update_answer(current, delta, *args):
	new_answer = solve(*args)
	if new_answer is None:
		return current
	new_answer += delta
	if current is None:
		return new_answer
	return min(current, new_answer)

def solve_non_cached(is_hard, my_turn, my_health, enemy_health, mana, shield, poison, recharge):
	if is_hard and my_turn:
		my_health -= 1
	if my_health <= 0:
		return None

	armor = 0
	if shield:
		shield -= 1
		if shield:
			armor = 7
	if poison:
		enemy_health -= 3
		poison -= 1
	if recharge:
		mana += 101
		recharge -= 1

	if my_health <= 0:
		return None
	if enemy_health <= 0:
		return 0

	if not my_turn:
		return solve(is_hard, True, my_health - max(1, enemy_damage - armor), enemy_health, mana, shield, poison, recharge)

	answer = None
	if mana >= MAGIC_MISSILE_COST:
		answer = update_answer(answer, MAGIC_MISSILE_COST, is_hard, False, my_health, enemy_health - 4, mana - MAGIC_MISSILE_COST, shield, poison, recharge)
	if mana >= DRAIN_COST:
		answer = update_answer(answer, DRAIN_COST, is_hard, False, my_health + 2, enemy_health - 2, mana - DRAIN_COST, shield, poison, recharge)
	if mana >= SHIELD_COST and not shield:
		answer = update_answer(answer, SHIELD_COST, is_hard, False, my_health, enemy_health, mana - SHIELD_COST, 6, poison, recharge)
	if mana >= POISON_COST and not poison:
		answer = update_answer(answer, POISON_COST, is_hard, False, my_health, enemy_health, mana - POISON_COST, shield, 6, recharge)
	if mana >= RECHARGE_COST and not recharge:
		answer = update_answer(answer, RECHARGE_COST, is_hard, False, my_health, enemy_health, mana - RECHARGE_COST, shield, poison, 5)
	return answer

print solve(False, True, initial_health, initial_enemy_health, initial_mana, 0, 0, 0)
print solve(True, True, initial_health, initial_enemy_health, initial_mana, 0, 0, 0)