import sys
import re

reindeers = []
pattern = '''can fly (\d+) km/s for (\d+).*rest for (\d+)'''
for line in sys.stdin:
	speed, fly_time, rest_time = map(int, re.search(pattern, line.rstrip()).groups())
	reindeers.append((speed, fly_time, rest_time))

total_time = 2503

def get_dist(speed, fly_time, rest_time, time):
	result = 0

	period = fly_time + rest_time
	full_period_count = time / period
	remaining_time = time - period * full_period_count
	remaining_fly_time = min(remaining_time, fly_time)
	dist_in_period = fly_time * speed

	return full_period_count * dist_in_period + remaining_fly_time * speed

print max(get_dist(*r, time=total_time) for r in reindeers)

n = len(reindeers)
points = [0] * n
for tm in xrange(1, total_time + 1):
	dist_by_reindeer = [get_dist(*r, time=tm) for r in reindeers]
	winning_dist = max(dist_by_reindeer)
	for i, dist in enumerate(dist_by_reindeer):
		if winning_dist == dist:
			points[i] += 1

print max(points)